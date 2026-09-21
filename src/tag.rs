//! Different kinds of [`Tag`]s that can be parsed.

use crate::label::DefaultLabel;
use crate::label::Label;
use crate::storage::Id;
use crate::storage::Interner;
use crate::storage::Key;
use crate::storage::Spur;
use crate::storage::Storage;
#[cfg(doc)]
use crate::TagManager;
#[cfg(feature = "either")]
use either::Either;
use itertools::Itertools as _;
use std::fmt::Display;
use std::fmt::Formatter;
use std::fmt::Result as FmtResult;
use std::hash::BuildHasher;
use std::hash::Hash;
use std::marker::PhantomData;

/// A structured view of the [`Key`]s inside a [`Tag`].
///
/// [`Tag::resolve`] can turn a tag back into a string, but a query engine needs to see a
/// tag's *keys* rather than its text: matching by key identity is a integer comparison,
/// where matching by text would mean resolving every tag on every comparison. This is how
/// a [`Tag`] exposes that, uniformly across tag kinds, so that code which works over tags
/// generically — queries especially — doesn't have to know which concrete type it holds.
///
/// A [`Tag`] implementation outside this crate that doesn't fit any of these shapes should
/// return [`TagParts::Opaque`], which matching treats as "matches nothing structurally".
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum TagParts<'tag, K> {
    /// The single key of a [`PlainTag`].
    Plain(K),

    /// The separately-interned key and value of a [`KeyValueTag`].
    KeyValue {
        /// The key half.
        key: K,
        /// The value half.
        value: K,
    },

    /// The parts of a [`MultipartTag`], in order.
    Multipart(&'tag [K]),

    /// A tag whose internals this crate can't see into.
    Opaque,
}

impl<K> TagParts<'_, K> {
    /// Get the [`TagKind`] these parts correspond to.
    pub fn kind(&self) -> TagKind {
        match self {
            TagParts::Plain(_) => TagKind::Plain,
            TagParts::KeyValue { .. } => TagKind::KeyValue,
            TagParts::Multipart(_) => TagKind::Multipart,
            TagParts::Opaque => TagKind::Other,
        }
    }
}

/// A trait defining a [`Tag`] which contains interned data.
///
/// The _only_ defining operation of a [`Tag`] is that it can be
/// converted back into a [`String`] using the [`Storage`] that
/// created it and the correct separator configured by the [`TagManager`]
/// that built it.
///
/// [`Tag`]s have an underlying [`Key`] used to define their storage.
/// Internally, [`Tag`]s are just a set of [`Key`]s used to make
/// storage and identity comparison cheap while enabling reconstruction
/// of the original [`String`].
pub trait Tag<'brand> {
    /// The label of the [`TagManager`] used to produce the [`Tag`].
    type Label: Label;

    /// The [`Key`] used by the [`Storage`] as a handle to the stored string data.
    type Key: Key + Hash;

    /// Get the [`TagKind`] of the current tag.
    ///
    /// Defaulted from [`Tag::parts`], which already knows the shape. Override it only if
    /// a tag can report a kind its parts don't imply.
    fn kind(&self) -> TagKind {
        self.parts().kind()
    }

    /// Get a structured view of the [`Key`]s this tag holds.
    ///
    /// See [`TagParts`] for why this exists alongside [`Tag::resolve`].
    fn parts(&self) -> TagParts<'_, Self::Key>;

    /// Resolve a [`Tag`] back into a [`String`].
    ///
    /// The `'brand` on the [`Storage`] must match the one this [`Tag`] carries, so this
    /// can only ever be called with the [`Storage`] that interned it. That, plus the
    /// [`Interner`] being append-only, is why this can't fail.
    fn resolve<H>(
        &self,
        storage: &Storage<'brand, Self::Label, Self::Key, H>,
        key_value_separator: KeyValueSep,
        path_separator: PathSep,
    ) -> String
    where
        H: BuildHasher + Clone;
}

#[cfg(feature = "either")]
// Auto-impl for `Either` wrapping two `Tag`s.
impl<'brand, L, K, T1, T2> Tag<'brand> for Either<T1, T2>
where
    L: Label,
    K: Key + Hash,
    T1: Tag<'brand, Label = L, Key = K>,
    T2: Tag<'brand, Label = L, Key = K>,
{
    type Label = L;
    type Key = K;

    fn resolve<H>(
        &self,
        storage: &Storage<'brand, Self::Label, Self::Key, H>,
        key_value_separator: KeyValueSep,
        path_separator: PathSep,
    ) -> String
    where
        H: BuildHasher + Clone,
    {
        match self {
            Either::Left(t) => t.resolve(storage, key_value_separator, path_separator),
            Either::Right(t) => t.resolve(storage, key_value_separator, path_separator),
        }
    }

    fn parts(&self) -> TagParts<'_, Self::Key> {
        match self {
            Either::Left(t) => t.parts(),
            Either::Right(t) => t.parts(),
        }
    }
}

//---------------------------------------------------------------------------

/// A [`Tag`] without internal structure.
///
/// [`PlainTag`] interns the full contents of a tag together.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct PlainTag<'brand, L = DefaultLabel, K = Spur>(K, PhantomData<L>, Id<'brand>)
where
    L: Label,
    K: Key + Hash;

impl<'brand, L: Label, K: Key + Hash> PlainTag<'brand, L, K> {
    /// Construct a new [`PlainTag`].
    pub(crate) fn new<H>(storage: &Storage<'brand, L, K, H>, raw: &str) -> Self
    where
        H: BuildHasher + Clone,
    {
        PlainTag(storage.get_or_intern(raw), PhantomData, storage.brand())
    }

    /// Resolve the whole tag into a [`String`].
    pub fn resolve<H>(&self, storage: &Storage<'brand, L, K, H>) -> String
    where
        H: BuildHasher + Clone,
    {
        self.resolve_str(storage).to_owned()
    }

    /// Resolve the whole tag into a string slice.
    ///
    /// Note that the returned string slice is a view into the underlying interner
    /// data, which means you're holding a borrow on the interner as long as the slice
    /// is held. If you want to let go of the borrow, copy the slice into a new owned
    /// string.
    pub fn resolve_str<'s, H>(&self, storage: &'s Storage<'brand, L, K, H>) -> &'s str
    where
        H: BuildHasher + Clone,
    {
        storage.resolve(self.0)
    }
}

impl<'brand, L: Label, K: Key + Hash> Tag<'brand> for PlainTag<'brand, L, K> {
    type Label = L;
    type Key = K;

    fn resolve<H>(
        &self,
        storage: &Storage<'brand, Self::Label, Self::Key, H>,
        _key_value_separator: KeyValueSep,
        _path_separator: PathSep,
    ) -> String
    where
        H: BuildHasher + Clone,
    {
        self.resolve(storage)
    }

    fn parts(&self) -> TagParts<'_, Self::Key> {
        TagParts::Plain(self.0)
    }
}

//---------------------------------------------------------------------------

/// A [`Tag`] composed of a key and a value.
///
/// [`KeyValueTag`] interns the key and value separately, on the expectation
/// that keys especially will be frequently repeated across tags.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct KeyValueTag<'brand, L = DefaultLabel, K = Spur>(K, K, PhantomData<L>, Id<'brand>)
where
    L: Label,
    K: Key + Hash;

impl<'brand, L: Label, K: Key + Hash> KeyValueTag<'brand, L, K> {
    /// Construct a new [`KeyValueTag`].
    pub(crate) fn new<H>(storage: &Storage<'brand, L, K, H>, key: &str, value: &str) -> Self
    where
        H: BuildHasher + Clone,
    {
        KeyValueTag(
            storage.get_or_intern(key),
            storage.get_or_intern(value),
            PhantomData,
            storage.brand(),
        )
    }

    /// Resolve the whole tag into a [`String`].
    pub fn resolve<H>(
        &self,
        storage: &Storage<'brand, L, K, H>,
        key_value_separator: KeyValueSep,
        _path_separator: PathSep,
    ) -> String
    where
        H: BuildHasher + Clone,
    {
        let (key, value) = self.resolve_key_value(storage);
        format!("{key}{key_value_separator}{value}")
    }

    /// Resolve the key and value parts of the tag separately.
    ///
    /// Note that the returned string slices are views into the underlying interner
    /// data, which means you're holding a borrow on the interner as long as the slices
    /// are held. If you want to let go of the borrow, copy the slices into new owned
    /// strings.
    pub fn resolve_key_value<'s, H>(
        &self,
        storage: &'s Storage<'brand, L, K, H>,
    ) -> (&'s str, &'s str)
    where
        H: BuildHasher + Clone,
    {
        (storage.resolve(self.0), storage.resolve(self.1))
    }
}

impl<'brand, L: Label, K: Key + Hash> Tag<'brand> for KeyValueTag<'brand, L, K> {
    type Label = L;
    type Key = K;

    fn resolve<H>(
        &self,
        storage: &Storage<'brand, Self::Label, Self::Key, H>,
        key_value_separator: KeyValueSep,
        path_separator: PathSep,
    ) -> String
    where
        H: BuildHasher + Clone,
    {
        self.resolve(storage, key_value_separator, path_separator)
    }

    fn parts(&self) -> TagParts<'_, Self::Key> {
        TagParts::KeyValue {
            key: self.0,
            value: self.1,
        }
    }
}

//---------------------------------------------------------------------------

/// A [`Tag`] composed of arbitrary parts.
///
/// [`MultipartTag`] interns each part of the tag separately, on the
/// expectation that individual parts will be frequently repeated.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MultipartTag<'brand, L = DefaultLabel, K = Spur>(Vec<K>, PhantomData<L>, Id<'brand>)
where
    L: Label,
    K: Key + Hash;

impl<'brand, L: Label, K: Key + Hash> MultipartTag<'brand, L, K> {
    /// Construct a new [`MultipartTag`].
    pub(crate) fn new<'part, I, H>(storage: &Storage<'brand, L, K, H>, parts: I) -> Self
    where
        I: Iterator<Item = &'part str>,
        H: BuildHasher + Clone,
    {
        MultipartTag(
            parts.map(|part| storage.get_or_intern(part)).collect(),
            PhantomData,
            storage.brand(),
        )
    }

    /// Resolve the whole tag into a [`String`].
    pub fn resolve<H>(
        &self,
        storage: &Storage<'brand, L, K, H>,
        _key_value_separator: KeyValueSep,
        path_separator: PathSep,
    ) -> String
    where
        H: BuildHasher + Clone,
    {
        self.parts(storage).join(path_separator.0)
    }

    /// Resolve each part of the tag into a collection of your choosing.
    ///
    /// Note that the returned string slices are views into the underlying interner
    /// data, which means you're holding a borrow on the interner as long as the slices
    /// are held. If you want to let go of the borrow, copy the slices into new owned
    /// strings.
    pub fn resolve_parts<'s, H, C>(&'s self, storage: &'s Storage<'brand, L, K, H>) -> C
    where
        H: BuildHasher + Clone,
        C: FromIterator<&'s str>,
    {
        self.parts(storage).collect()
    }

    /// Iterate over the resolved parts of the tag.
    pub fn parts<'s, H>(
        &'s self,
        storage: &'s Storage<'brand, L, K, H>,
    ) -> impl Iterator<Item = &'s str> + 's
    where
        H: BuildHasher + Clone,
    {
        // Hold the interner rather than the storage: `Storage` names `'brand`, so
        // capturing it would make the returned iterator capture `'brand` too, which
        // the `+ 's` bound doesn't name. `Interner` doesn't mention the brand.
        let interner: &'s Interner<K, H> = storage.handle();

        self.0
            .iter()
            .copied()
            .map(move |part| interner.resolve(&part))
    }
}

impl<'brand, L: Label, K: Key + Hash> Tag<'brand> for MultipartTag<'brand, L, K> {
    type Label = L;
    type Key = K;

    fn resolve<H>(
        &self,
        storage: &Storage<'brand, Self::Label, Self::Key, H>,
        key_value_separator: KeyValueSep,
        path_separator: PathSep,
    ) -> String
    where
        H: BuildHasher + Clone,
    {
        self.resolve(storage, key_value_separator, path_separator)
    }

    fn parts(&self) -> TagParts<'_, Self::Key> {
        TagParts::Multipart(&self.0)
    }
}

/// The separator between keys and values in a key-value tag.
///
/// The default separator is `":"`.
#[derive(Debug, Copy, Clone)]
pub struct KeyValueSep(pub &'static str);

impl Display for KeyValueSep {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        write!(f, "{}", self.0)
    }
}

impl Default for KeyValueSep {
    fn default() -> Self {
        KeyValueSep(":")
    }
}

/// The separator between path segments in a multipart tag.
///
/// The default separator is `"/"`.
#[derive(Debug, Copy, Clone)]
pub struct PathSep(pub &'static str);

impl Display for PathSep {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        write!(f, "{}", self.0)
    }
}

impl Default for PathSep {
    fn default() -> Self {
        PathSep("/")
    }
}

/// The kind of tag being worked with.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum TagKind {
    /// A [`PlainTag`].
    Plain,

    /// A [`KeyValueTag`].
    KeyValue,

    /// A [`MultipartTag`].
    Multipart,

    /// A type of [`Tag`] not otherwise known.
    ///
    /// No [`Tag`] defined by this crate reports this kind; it's here for
    /// downstream [`Tag`] implementations which aren't plain, key-value,
    /// or multipart.
    Other,
}

/// A trait to implement on types that _carry_ [`Tag`]s.
///
/// This trait is generic over the tag type, to permit implementing
/// it for multiple types of tags.
/// Note there's deliberately no bound on `T` here, and no `'brand` parameter. A tag type
/// already carries its own brand, so a `Tagged` impl never has to name one:
///
/// ```ignore
/// impl<'brand> Tagged<PlainTag<'brand, Tags>> for BlogPost<'brand> { .. }
/// ```
///
/// Code that needs `T` to actually be a [`Tag`] says so itself. That keeps the lifetime
/// out of every impl and every bound that mentions this trait, which matters because
/// [`Index`] and [`Scan`] are generic over a manager whose brand they can't name.
///
/// [`Index`]: crate::query::Index
/// [`Scan`]: crate::query::Scan
pub trait Tagged<T> {
    /// The type of iterator used to provide the [`Tag`]s.
    ///
    /// The lifetime bounds indicate that the tagged type and the
    /// tags it produces need to outlive the references to those tags
    /// returned by the tag iterator.
    type TagIter<'item>: Iterator<Item = &'item T>
    where
        Self: 'item,
        T: 'item;

    /// Get if the tagged type has tags.
    ///
    /// This is included in the API because there's not a good way to get
    /// the number of elements out of an iterator without using `count()`,
    /// which consumes the iterator.
    ///
    /// `size_hint` unfortunately is `None` for the upper bound by default,
    /// so it is frequently not useful.
    fn has_tags(&self) -> bool;

    /// Get the tags of the tagged type.
    fn get_tags(&self) -> Self::TagIter<'_>;
}
