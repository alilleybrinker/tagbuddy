//! Evaluating a query by walking items, without building an index.

use crate::label::Label;
use crate::parse::Parser;
use crate::query::Resolved;
use crate::query::ResolvedMatch;
use crate::query::ResolvedValue;
use crate::storage::Key;
use crate::storage::Storage;
use crate::tag::Tag;
use crate::tag::TagParts;
use crate::tag::Tagged;
use crate::TagManager;
use std::hash::BuildHasher;
use std::hash::Hash;

/// A set of items to run a [`Query`] against, one pass at a time.
///
/// Produced by [`TagManager::select`]. Holds no index and allocates nothing; each query
/// costs one walk of the items.
///
/// [`Query`]: crate::query::Query
/// [`TagManager::select`]: crate::TagManager::select
pub struct Scan<'m, 'brand, L, K, T, P, H, Items>
where
    L: Label,
    K: Key + Hash,
    T: Tag<'brand, Label = L, Key = K>,
    P: Parser<'brand, Tag = T> + Send + Sync,
    H: BuildHasher + Clone,
{
    pub(crate) manager: &'m TagManager<'brand, L, K, T, P, H>,
    pub(crate) items: Items,
}

impl<'m, 'brand, 'items, L, K, T, P, H, I, Items> Scan<'m, 'brand, L, K, T, P, H, Items>
where
    L: Label,
    K: Key + Hash,
    T: Tag<'brand, Label = L, Key = K> + 'items,
    P: Parser<'brand, Tag = T> + Send + Sync,
    H: BuildHasher + Clone,
    I: Tagged<'brand, T> + 'items,
    Items: IntoIterator<Item = &'items I>,
{
    /// Keep the items satisfying `query`.
    ///
    /// The returned iterator borrows the items it was given, so results are references
    /// into the caller's own collection rather than copies.
    pub fn matching(
        self,
        query: &crate::query::Query,
    ) -> impl Iterator<Item = &'items I> + use<'items, 'brand, L, K, T, P, H, I, Items> {
        let storage = self.manager.storage();
        let resolved = Resolved::new(query, storage);
        let storage = storage.share_as::<L>();

        self.items
            .into_iter()
            .filter(move |item| matches_item(*item, &resolved, &storage))
    }
}

/// Test one item against a resolved query.
pub(crate) fn matches_item<'brand, L, K, T, H, I>(
    item: &I,
    query: &Resolved<K>,
    storage: &Storage<'brand, L, K, H>,
) -> bool
where
    L: Label,
    K: Key + Hash,
    T: Tag<'brand, Label = L, Key = K>,
    H: BuildHasher + Clone,
    I: Tagged<'brand, T>,
{
    match query {
        Resolved::Anything => true,
        Resolved::Contains(m) => item
            .get_tags()
            .any(|tag| matches_tag(&tag.parts(), m, storage)),
        Resolved::All(qs) => qs.iter().all(|q| matches_item(item, q, storage)),
        Resolved::Any(qs) => qs.iter().any(|q| matches_item(item, q, storage)),
        Resolved::Not(q) => !matches_item(item, q, storage),
    }
}

/// Test one tag against a resolved match.
///
/// This is the single definition of what a [`Match`] means. The indexed path narrows
/// candidates using the index and then calls back here to confirm, so the two strategies
/// can't drift apart on semantics.
///
/// [`Match`]: crate::query::Match
pub(crate) fn matches_tag<'brand, L, K, H>(
    parts: &TagParts<'_, K>,
    m: &ResolvedMatch<K>,
    storage: &Storage<'brand, L, K, H>,
) -> bool
where
    L: Label,
    K: Key + Hash,
    H: BuildHasher + Clone,
{
    match m {
        ResolvedMatch::Never => false,

        ResolvedMatch::Exact(want) => matches!(parts, TagParts::Plain(k) if k == want),

        ResolvedMatch::HasKey(want) => {
            matches!(parts, TagParts::KeyValue { key, .. } if key == want)
        }

        ResolvedMatch::KeyValue { key: want, value } => match parts {
            TagParts::KeyValue { key, value: got } if key == want => {
                matches_value(*got, value, storage)
            }
            _ => false,
        },

        ResolvedMatch::Path(want) => {
            matches!(parts, TagParts::Multipart(got) if *got == want.as_slice())
        }

        ResolvedMatch::Prefix(want) => {
            matches!(parts, TagParts::Multipart(got) if got.starts_with(want))
        }

        ResolvedMatch::All(ms) => ms.iter().all(|m| matches_tag(parts, m, storage)),
        ResolvedMatch::Any(ms) => ms.iter().any(|m| matches_tag(parts, m, storage)),
    }
}

/// Test one key-value tag's value against a resolved value constraint.
fn matches_value<'brand, L, K, H>(
    got: K,
    value: &ResolvedValue<K>,
    storage: &Storage<'brand, L, K, H>,
) -> bool
where
    L: Label,
    K: Key + Hash,
    H: BuildHasher + Clone,
{
    match value {
        ResolvedValue::Never => false,

        // The interesting case: key identity, no string comparison at all.
        ResolvedValue::Is(want) => got == *want,
        ResolvedValue::OneOf(wants) => wants.contains(&got),

        // These two need the text, so they pay a resolve. It's cheap and lock-free, but
        // it's also why they can't be answered from an index.
        #[cfg(feature = "regex")]
        ResolvedValue::Matches(r) => r.is_match(storage.resolve(got)),
        ResolvedValue::Passes(p) => p.test(storage.resolve(got)),
    }
}
