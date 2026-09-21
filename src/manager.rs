//! Produce and resolve tags.

use crate::error::ParseError;
#[cfg(doc)]
use crate::label::Label;
use crate::parse::*;
use crate::query::Index;
use crate::query::Scan;
#[cfg(doc)]
use crate::storage::Interner;
use crate::storage::Key;
use crate::storage::Storage;
use crate::tag::KeyValueSep;
#[cfg(doc)]
use crate::tag::KeyValueTag;
#[cfg(doc)]
use crate::tag::MultipartTag;
use crate::tag::PathSep;
#[cfg(doc)]
use crate::tag::PlainTag;
use crate::tag::Tag;
use crate::tag::TagKind;
use crate::tag::TagParts;
use crate::tag::Tagged;
use std::collections::hash_map::RandomState;
use std::convert::identity;
use std::hash::BuildHasher;
use std::hash::Hash;
use typed_builder::TypedBuilder;

/// The [`Tag`] type a parser produces.
pub type TagOf<'brand, P> = <P as Parser<'brand>>::Tag;

/// The [`Label`] of the tags a parser produces.
pub type LabelOf<'brand, P> = <TagOf<'brand, P> as Tag<'brand>>::Label;

/// The [`Key`] of the tags a parser produces.
pub type KeyOf<'brand, P> = <TagOf<'brand, P> as Tag<'brand>>::Key;

mod sealed {
    /// Prevents [`ManagerParts`] being implemented outside this crate.
    ///
    /// [`ManagerParts`]: super::ManagerParts
    pub trait Sealed {}
}

/// What the query engine needs from a [`TagManager`], without naming its brand.
///
/// A [`TagManager`] has a lifetime and five type parameters, and anything generic over one
/// has to repeat all six. [`Index`] would otherwise be
/// `Index<'m, 'brand, 'items, L, K, T, P, H, I>` — nine parameters to write out in any
/// struct field or return type, even though inference handles them at the call site.
///
/// This collapses them. The associated types are projections of the manager's parameters,
/// and the three methods are the only things the query engine actually does with a manager:
/// look a query string up, get a key's text back, and see inside a tag.
///
/// # Why there's no `'brand` here
///
/// The trait deliberately has no lifetime parameter, and [`ManagerParts::Tag`] is just the
/// manager's tag type — which already carries its own brand. So `<M as ManagerParts>::Tag`
/// for a manager branded `'b` projects to, say, `PlainTag<'b>`, without the trait ever
/// naming `'b`. That's what lets [`Index`] drop from nine parameters to four.
///
/// It's also why the operations are methods rather than a `fn storage(&self) -> &Storage<..>`
/// accessor: naming [`Storage`] would mean naming the brand, putting the lifetime straight
/// back into every signature.
///
/// # Sealed
///
/// Implemented only for [`TagManager`]. There's no meaningful way to be "the parts of a
/// manager" without being one, and sealing keeps this free to change.
///
/// [`Index`]: crate::query::Index
pub trait ManagerParts: sealed::Sealed {
    /// The [`Key`] the manager's [`Storage`] hands out.
    type Key: Key + Hash;

    /// The [`Tag`] type the manager's parser produces.
    type Tag;

    /// Find the key an already-interned string was given, if it has one.
    ///
    /// This is [`Storage::get`], not `get_or_intern`: a query must not add its own search
    /// terms to an append-only interner.
    fn lookup(&self, raw: &str) -> Option<Self::Key>;

    /// Get the text behind a key.
    ///
    /// Only needed for value constraints that can't be answered by key identity — a regex
    /// or a predicate.
    fn text(&self, key: Self::Key) -> &str;

    /// Get a structured view of a tag's keys.
    ///
    /// An associated function rather than a method on [`Tag`] directly, because calling
    /// [`Tag::parts`] would require the bound `Self::Tag: Tag<'brand>`, and `'brand` is
    /// exactly what this trait exists not to name.
    fn parts_of(tag: &Self::Tag) -> TagParts<'_, Self::Key>;
}

/// Constructs [`Tag`]s according to the configured parser and interner.
///
/// A single [`TagManager`] is responsible for parsing and resolving tags that
/// match the rules of a single configured parser, with storage handled by an
/// underlying [`Interner`]. The [`Interner`] may be shared with other
/// [`TagManager`]s, via [`Storage::share_as`].
///
/// # Parameters
///
/// Just the parser and the hasher. Everything else follows from the parser: it determines
/// the [`Tag`] type it produces, which determines that tag's [`Label`] and [`Key`]. So a
/// manager over plain tags labelled `Tags` is
///
/// ```text
/// TagManager<'brand, Plain<Tags>>
/// ```
///
/// rather than restating the label, key and tag type the parser already implies. Those
/// projections are available as [`TagOf`], [`LabelOf`] and [`KeyOf`] when you need to name
/// one, and [`ManagerParts`] bundles them for code generic over a manager.
///
/// Resolving through a [`TagManager`] can't fail. Its [`Storage`] carries a
/// `'brand`, so a [`Tag`] can only be resolved through the storage that interned
/// it, and an [`Interner`] is append-only, so a key it handed out stays valid.
///
/// The manner in which tag data is stored depends on the `T` parameter.
/// [`PlainTag`] stores the full string data in the interner. [`KeyValueTag`]
/// stores the key and value data separately, on the expectation that keys
/// especially will be repeated, and thus a lot of space saving is achieved by
/// deduplicating them through separate interning. [`MultipartTag`] stores
/// each part separately, again on the expectation that individual parts will
/// be frequently repeated across tags, resulting in space savings from interning.
#[derive(TypedBuilder)]
pub struct TagManager<'brand, P = Plain, H = RandomState>
where
    P: Parser<'brand> + Send + Sync,
    H: BuildHasher + Clone,
{
    /// Defines how key-value tags are parsed, if key-value tags are permitted.
    pub(crate) parser: P,

    /// The separator used for separating key and values in key-value tags.
    #[builder(default)]
    pub(crate) key_value_separator: KeyValueSep,

    /// The separator used for separating parts in multipart tags.
    #[builder(default)]
    pub(crate) path_separator: PathSep,

    /// Interns and stores string data for tags, to reduce memory usage.
    pub(crate) storage: Storage<'brand, LabelOf<'brand, P>, KeyOf<'brand, P>, H>,
}

impl<'brand, P, H> sealed::Sealed for TagManager<'brand, P, H>
where
    P: Parser<'brand> + Send + Sync,
    H: BuildHasher + Clone,
{
}

impl<'brand, P, H> ManagerParts for TagManager<'brand, P, H>
where
    P: Parser<'brand> + Send + Sync,
    H: BuildHasher + Clone,
{
    type Key = KeyOf<'brand, P>;

    // The tag type carries `'brand` inside itself, so this projects a branded tag type
    // without the trait having a brand of its own.
    type Tag = TagOf<'brand, P>;

    fn lookup(&self, raw: &str) -> Option<Self::Key> {
        self.storage.get(raw)
    }

    fn text(&self, key: Self::Key) -> &str {
        self.storage.resolve(key)
    }

    fn parts_of(tag: &Self::Tag) -> TagParts<'_, Self::Key> {
        tag.parts()
    }
}

impl<'brand, P, H> TagManager<'brand, P, H>
where
    P: Parser<'brand> + Send + Sync,
    H: BuildHasher + Clone,
{
    /// Attempt to parse a structured tag from the provided "raw" tag.
    ///
    /// This may fail if the tag is empty, or if it violates the configured [`Parser`]'s rules.
    pub fn parse_tag(&self, raw: &str) -> Result<P::Tag, ParseError> {
        self.parser.parse(
            &self.storage,
            self.key_value_separator,
            self.path_separator,
            raw,
        )
    }

    /// Parse tags into a collection of your choosing.
    pub fn parse_tags_into<'raw, C>(&self, src: impl IntoIterator<Item = &'raw str>) -> C
    where
        C: FromIterator<Result<P::Tag, ParseError>>,
    {
        self.parse_tags_into_with(src, identity)
    }

    /// Parse tags into a collection of your choosing, pairing each with its [`TagKind`].
    pub fn parse_tags_into_with_kind<'raw, C>(&self, src: impl IntoIterator<Item = &'raw str>) -> C
    where
        C: FromIterator<Result<(P::Tag, TagKind), ParseError>>,
    {
        self.parse_tags_into_with(src, |t| {
            let kind = t.kind();
            (t, kind)
        })
    }

    /// Parse tags into a collection of your choosing, mapping each one as it's produced.
    pub fn parse_tags_into_with<'raw, O, C>(
        &self,
        src: impl IntoIterator<Item = &'raw str>,
        f: impl FnOnce(P::Tag) -> O + Copy,
    ) -> C
    where
        C: FromIterator<Result<O, ParseError>>,
    {
        src.into_iter()
            .map(|raw| {
                self.parser
                    .parse(
                        &self.storage,
                        self.key_value_separator,
                        self.path_separator,
                        raw,
                    )
                    .map(f)
            })
            .collect()
    }

    /// Get a string representation of a [`Tag`].
    ///
    /// This can't fail: the tag's `'brand` means it was interned by this [`TagManager`]'s
    /// [`Storage`], and an [`Interner`] never drops a key it handed out.
    pub fn resolve_tag(&self, tag: &P::Tag) -> String {
        tag.resolve(&self.storage, self.key_value_separator, self.path_separator)
    }

    /// Get the string representation of a set of [`Tag`]s.
    pub fn resolve_tags_into<'tag, C>(&self, src: impl IntoIterator<Item = &'tag P::Tag>) -> C
    where
        P::Tag: 'tag,
        C: FromIterator<String>,
    {
        self.resolve_tags_into_with(src, identity)
    }

    /// Get the string representation of a set of [`Tag`]s, mapping each one as it's resolved.
    pub fn resolve_tags_into_with<'tag, O, C>(
        &self,
        src: impl IntoIterator<Item = &'tag P::Tag>,
        f: impl FnOnce(String) -> O + Copy,
    ) -> C
    where
        P::Tag: 'tag,
        C: FromIterator<O>,
    {
        src.into_iter()
            .map(|tag| f(tag.resolve(&self.storage, self.key_value_separator, self.path_separator)))
            .collect()
    }

    /// Select items to run queries against by scanning them.
    ///
    /// Takes anything iterable, borrows rather than collects, and costs one pass per
    /// query. Use [`TagManager::index`] instead when the same items are queried more than
    /// once.
    ///
    /// ```
    /// # use tagbuddy::brand::make_guard;
    /// # use tagbuddy::parse::Plain;
    /// # use tagbuddy::query::{contains, Match};
    /// # use tagbuddy::storage::DefaultStorage;
    /// # use tagbuddy::tag::{PlainTag, Tagged};
    /// # use tagbuddy::TagManager;
    /// # use std::slice::Iter;
    /// # struct Post<'b>(Vec<PlainTag<'b>>);
    /// # impl<'b> Tagged<PlainTag<'b>> for Post<'b> {
    /// #     type TagIter<'i> = Iter<'i, PlainTag<'b>> where Self: 'i;
    /// #     fn has_tags(&self) -> bool { !self.0.is_empty() }
    /// #     fn get_tags(&self) -> Self::TagIter<'_> { self.0.iter() }
    /// # }
    /// make_guard!(guard);
    /// let manager = TagManager::builder()
    ///     .parser(Plain::new())
    ///     .storage(DefaultStorage::fresh(guard))
    ///     .build();
    ///
    /// let posts = vec![
    ///     Post(vec![manager.parse_tag("rust").unwrap()]),
    ///     Post(vec![manager.parse_tag("go").unwrap()]),
    /// ];
    ///
    /// let found = manager
    ///     .select(&posts)
    ///     .matching(&contains(Match::Exact("rust".to_owned())))
    ///     .count();
    ///
    /// assert_eq!(found, 1);
    /// ```
    pub fn select<Items>(&self, items: Items) -> Scan<'_, Self, Items> {
        Scan::new(self, items)
    }

    /// Build a reusable inverted index over `items`.
    ///
    /// Requires a slice, because the index maps tags to item *positions*. Building costs
    /// about what one [`TagManager::select`] pass costs, so this pays off from the second
    /// query onwards.
    ///
    /// The index borrows `items`, so it can't be left holding stale positions: the
    /// collection can't change while the index is alive.
    pub fn index<'m, 'items, I>(&'m self, items: &'items [I]) -> Index<'m, 'items, Self, I>
    where
        I: Tagged<TagOf<'brand, P>>,
    {
        Index::build(self, items)
    }

    /// Get the inner [`Storage`] of the [`TagManager`].
    pub fn storage(&self) -> &Storage<'brand, LabelOf<'brand, P>, KeyOf<'brand, P>, H> {
        &self.storage
    }

    /// Get the [`Parser`] applied by the [`TagManager`].
    pub fn parser(&self) -> &P {
        &self.parser
    }

    /// Get the key-value separator (default `":"`) used by the [`TagManager`] for [`KeyValueTag`]s.
    pub fn key_value_separator(&self) -> KeyValueSep {
        self.key_value_separator
    }

    /// Get the path separator (default `"/"`) used by the [`TagManager`] for [`MultipartTag`]s.
    pub fn path_separator(&self) -> PathSep {
        self.path_separator
    }
}
