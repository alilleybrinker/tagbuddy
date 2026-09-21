//! Evaluating a query by walking items, without building an index.

use crate::query::Query;
use crate::query::Resolved;
use crate::query::ResolvedMatch;
use crate::query::ResolvedValue;
use crate::tag::TagParts;
use crate::tag::Tagged;
use crate::ManagerParts;

/// A set of items to run a [`Query`] against, one pass at a time.
///
/// Produced by [`TagManager::select`]. Holds no index and allocates nothing; each query
/// costs one walk of the items.
///
/// [`TagManager::select`]: crate::TagManager::select
pub struct Scan<'m, M, Items> {
    manager: &'m M,
    items: Items,
}

impl<'m, M, Items> Scan<'m, M, Items> {
    pub(crate) fn new(manager: &'m M, items: Items) -> Self {
        Scan { manager, items }
    }
}

impl<'m, 'items, M, I, Items> Scan<'m, M, Items>
where
    M: ManagerParts,
    I: Tagged<M::Tag> + 'items,
    Items: IntoIterator<Item = &'items I>,
{
    /// Keep the items satisfying `query`.
    ///
    /// The returned iterator borrows the items it was given, so results are references
    /// into the caller's own collection rather than copies.
    pub fn matching(
        self,
        query: &Query,
    ) -> impl Iterator<Item = &'items I> + use<'m, 'items, M, I, Items> {
        let manager = self.manager;
        let resolved = Resolved::new(query, manager);

        self.items
            .into_iter()
            .filter(move |item| matches_item(*item, &resolved, manager))
    }
}

/// Test one item against a resolved query.
pub(crate) fn matches_item<M, I>(item: &I, query: &Resolved<M::Key>, manager: &M) -> bool
where
    M: ManagerParts,
    I: Tagged<M::Tag>,
{
    match query {
        Resolved::Anything => true,
        Resolved::Contains(m) => item
            .get_tags()
            .any(|tag| matches_tag(&M::parts_of(tag), m, manager)),
        Resolved::All(qs) => qs.iter().all(|q| matches_item(item, q, manager)),
        Resolved::Any(qs) => qs.iter().any(|q| matches_item(item, q, manager)),
        Resolved::Not(q) => !matches_item(item, q, manager),
    }
}

/// Test one tag against a resolved match.
///
/// This is the single definition of what a [`Match`] means. The indexed path narrows
/// candidates using the index and then calls back here to confirm, so the two strategies
/// can't drift apart on semantics.
///
/// [`Match`]: crate::query::Match
pub(crate) fn matches_tag<M>(
    parts: &TagParts<'_, M::Key>,
    m: &ResolvedMatch<M::Key>,
    manager: &M,
) -> bool
where
    M: ManagerParts,
{
    match m {
        ResolvedMatch::Never => false,

        ResolvedMatch::Exact(want) => matches!(parts, TagParts::Plain(k) if k == want),

        ResolvedMatch::HasKey(want) => {
            matches!(parts, TagParts::KeyValue { key, .. } if key == want)
        }

        ResolvedMatch::KeyValue { key: want, value } => match parts {
            TagParts::KeyValue { key, value: got } if key == want => {
                matches_value(*got, value, manager)
            }
            _ => false,
        },

        ResolvedMatch::Path(want) => {
            matches!(parts, TagParts::Multipart(got) if *got == want.as_slice())
        }

        ResolvedMatch::Prefix(want) => {
            matches!(parts, TagParts::Multipart(got) if got.starts_with(want))
        }

        ResolvedMatch::All(ms) => ms.iter().all(|m| matches_tag(parts, m, manager)),
        ResolvedMatch::Any(ms) => ms.iter().any(|m| matches_tag(parts, m, manager)),
    }
}

/// Test one key-value tag's value against a resolved value constraint.
fn matches_value<M>(got: M::Key, value: &ResolvedValue<M::Key>, manager: &M) -> bool
where
    M: ManagerParts,
{
    match value {
        ResolvedValue::Never => false,

        // The interesting case: key identity, no string comparison at all.
        ResolvedValue::Is(want) => got == *want,
        ResolvedValue::OneOf(wants) => wants.contains(&got),

        // These two need the text, so they pay a lookup. It's cheap and lock-free, but
        // it's also why they can't be answered from an index.
        #[cfg(feature = "regex")]
        ResolvedValue::Matches(r) => r.is_match(manager.text(got)),
        ResolvedValue::Passes(p) => p.test(manager.text(got)),
    }
}
