//! Different kinds of [`Tag`]s that can be parsed.

use crate::error::ResolveError;
use crate::label::DefaultLabel;
use crate::label::Label;
#[cfg(doc)]
use crate::storage::Storage;
use crate::storage::StorageLock;
#[cfg(doc)]
use crate::TagManager;
#[cfg(feature = "either")]
use either::Either;
use itertools::intersperse_with;
use std::fmt::Display;
use std::fmt::Formatter;
use std::fmt::Result as FmtResult;
use std::hash::BuildHasher;
use std::marker::PhantomData;
use string_interner::backend::Backend as InternerBackend;
use string_interner::DefaultSymbol;
#[cfg(doc)]
use string_interner::StringInterner;
use string_interner::Symbol;

/// A trait defining a [`Tag`] which contains interned data.
///
/// The _only_ defining operation of a [`Tag`] is that it can be
/// converted back into a [`String`] using the [`Storage`] that
/// created it and the correct separator configured by the [`TagManager`]
/// that built it.
///
/// [`Tag`]s have an underlying [`Symbol`] used to define their storage.
/// Internally, [`Tag`]s are just a set of [`Symbol`]s used to make
/// storage and identity comparison cheap while enabling reconstruction
/// of the original [`String`].
pub trait Tag {
    /// The label of the [`TagManager`] used to produce the [`Tag`].
    type Label: Label;

    /// The [`Symbol`] used by the [`Storage`] as a handle to the stored string data.
    type Symbol: Symbol;

    /// Get the [`TagKind`] of the current tag.
    fn kind(&self) -> TagKind;

    /// Try to resolve a [`Tag`] back into a [`String`].
    fn resolve<B, H>(
        &self,
        storage: &StorageLock<'_, Self::Label, B, H>,
        key_value_separator: KeyValueSep,
        path_separator: PathSep,
    ) -> Result<String, ResolveError>
    where
        B: InternerBackend<Symbol = Self::Symbol>,
        H: BuildHasher;
}

#[cfg(feature = "either")]
// Auto-impl for `Either` wrapping two `Tag`s.
impl<L, S, T1, T2> Tag for Either<T1, T2>
where
    L: Label,
    S: Symbol,
    T1: Tag<Label = L, Symbol = S>,
    T2: Tag<Label = L, Symbol = S>,
{
    type Label = L;
    type Symbol = S;

    fn resolve<B, H>(
        &self,
        storage: &StorageLock<'_, Self::Label, B, H>,
        key_value_separator: KeyValueSep,
        path_separator: PathSep,
    ) -> Result<String, ResolveError>
    where
        B: InternerBackend<Symbol = Self::Symbol>,
        H: BuildHasher,
    {
        match self {
            Either::Left(t) => t.resolve(storage, key_value_separator, path_separator),
            Either::Right(t) => t.resolve(storage, key_value_separator, path_separator),
        }
    }

    fn kind(&self) -> TagKind {
        match self {
            Either::Left(t) => t.kind(),
            Either::Right(t) => t.kind(),
        }
    }
}

//---------------------------------------------------------------------------

/// A [`Tag`] without internal structure.
///
/// [`PlainTag`] interns the full contents of a tag together.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct PlainTag<L = DefaultLabel, S = DefaultSymbol>(S, PhantomData<L>)
where
    L: Label,
    S: Symbol;

impl<L: Label, S: Symbol> PlainTag<L, S> {
    /// Construct a new [`PlainTag`].
    pub(crate) fn new<B, H>(storage: &mut StorageLock<'_, L, B, H>, raw: &str) -> Self
    where
        B: InternerBackend<Symbol = S>,
        H: BuildHasher,
    {
        PlainTag(storage.get_or_intern(raw), PhantomData)
    }

    /// Resolve the whole tag into a [`String`].
    pub fn resolve<B, H>(&self, storage: &StorageLock<'_, L, B, H>) -> Result<String, ResolveError>
    where
        B: InternerBackend<Symbol = S>,
        H: BuildHasher,
    {
        self.resolve_str(storage).map(ToString::to_string)
    }

    /// Resolve the whole tag into a string slice.
    ///
    /// Note that the returned string slice is a view into the underlying interner
    /// data, which means you're holding a borrow on the interner as long as the slice
    /// is held. If you want to let go of the borrow, copy the slice into a new owned
    /// string.
    pub fn resolve_str<'intern, B, H>(
        &self,
        storage: &'intern StorageLock<'_, L, B, H>,
    ) -> Result<&'intern str, ResolveError>
    where
        B: InternerBackend<Symbol = S>,
        H: BuildHasher,
    {
        storage.resolve(self.0).ok_or(ResolveError::TagNotFound)
    }
}

impl<L: Label, S: Symbol> Tag for PlainTag<L, S> {
    type Label = L;
    type Symbol = S;

    fn resolve<B, H>(
        &self,
        storage: &StorageLock<'_, Self::Label, B, H>,
        _key_value_separator: KeyValueSep,
        _path_separator: PathSep,
    ) -> Result<String, ResolveError>
    where
        B: InternerBackend<Symbol = Self::Symbol>,
        H: BuildHasher,
    {
        self.resolve(storage)
    }

    fn kind(&self) -> TagKind {
        TagKind::Plain
    }
}

//---------------------------------------------------------------------------

/// A [`Tag`] composed of a key and a value.
///
/// [`KeyValueTag`] interns the key and value separately, on the expectation
/// that keys especially will be frequently repeated across tags.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct KeyValueTag<L = DefaultLabel, S = DefaultSymbol>(S, S, PhantomData<L>)
where
    L: Label,
    S: Symbol;

impl<L: Label, S: Symbol> KeyValueTag<L, S> {
    /// Construct a new [`KeyValueTag`].
    pub(crate) fn new<B, H>(storage: &mut StorageLock<'_, L, B, H>, key: &str, value: &str) -> Self
    where
        B: InternerBackend<Symbol = S>,
        H: BuildHasher,
    {
        KeyValueTag(
            storage.get_or_intern(key),
            storage.get_or_intern(value),
            PhantomData,
        )
    }

    /// Resolve the whole tag into a [`String`].
    pub fn resolve<B, H>(
        &self,
        storage: &StorageLock<'_, L, B, H>,
        key_value_separator: KeyValueSep,
        _path_separator: PathSep,
    ) -> Result<String, ResolveError>
    where
        B: InternerBackend<Symbol = S>,
        H: BuildHasher,
    {
        self.resolve_key_value(storage)
            .map(|(key, value)| format!("{key}{key_value_separator}{value}"))
    }

    /// Resolve the key and value parts of the tag separately.
    ///
    /// Note that the returned string slices are views into the underlying interner
    /// data, which means you're holding a borrow on the interner as long as the slices
    /// are held. If you want to let go of the borrow, copy the slices into new owned
    /// strings.
    pub fn resolve_key_value<'intern, B, H>(
        &self,
        storage: &'intern StorageLock<'_, L, B, H>,
    ) -> Result<(&'intern str, &'intern str), ResolveError>
    where
        B: InternerBackend<Symbol = S>,
        H: BuildHasher,
    {
        let (key, value) = self.try_resolve_key_value(storage);
        Ok((key?, value?))
    }

    /// Try to resolve the key and value parts of the tag separately.
    ///
    /// This lets you resolve partial tags, if for some reason part of the tag
    /// resolves and the other doesn't.
    ///
    /// Note that the returned string slices are views into the underlying interner
    /// data, which means you're holding a borrow on the interner as long as the slices
    /// are held. If you want to let go of the borrow, copy the slices into new owned
    /// strings.
    pub fn try_resolve_key_value<'intern, B, H>(
        &self,
        storage: &'intern StorageLock<'_, L, B, H>,
    ) -> (
        Result<&'intern str, ResolveError>,
        Result<&'intern str, ResolveError>,
    )
    where
        B: InternerBackend<Symbol = S>,
        H: BuildHasher,
    {
        (
            storage.resolve(self.0).ok_or(ResolveError::KeyNotFound),
            storage.resolve(self.1).ok_or(ResolveError::ValueNotFound),
        )
    }
}

impl<L: Label, S: Symbol> Tag for KeyValueTag<L, S> {
    type Label = L;
    type Symbol = S;

    fn resolve<B, H>(
        &self,
        storage: &StorageLock<'_, Self::Label, B, H>,
        key_value_separator: KeyValueSep,
        path_separator: PathSep,
    ) -> Result<String, ResolveError>
    where
        B: InternerBackend<Symbol = Self::Symbol>,
        H: BuildHasher,
    {
        self.resolve(storage, key_value_separator, path_separator)
    }

    fn kind(&self) -> TagKind {
        TagKind::KeyValue
    }
}

//---------------------------------------------------------------------------

/// A [`Tag`] composed of arbitrary parts.
///
/// [`MultipartTag`] interns each part of the tag separately, on the
/// expectation that individual parts will be frequently repeated.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MultipartTag<L = DefaultLabel, S = DefaultSymbol>(Vec<S>, PhantomData<L>)
where
    L: Label,
    S: Symbol;

impl<L: Label, S: Symbol> MultipartTag<L, S> {
    /// Construct a new [`MultipartTag`].
    pub(crate) fn new<'part, I, B, H>(storage: &mut StorageLock<'_, L, B, H>, parts: I) -> Self
    where
        I: Iterator<Item = &'part str>,
        B: InternerBackend<Symbol = S>,
        H: BuildHasher,
    {
        MultipartTag(
            parts.map(|part| storage.get_or_intern(part)).collect(),
            PhantomData,
        )
    }

    /// Resolve the whole tag into a [`String`].
    pub fn resolve<B, H>(
        &self,
        storage: &StorageLock<'_, L, B, H>,
        _key_value_separator: KeyValueSep,
        path_separator: PathSep,
    ) -> Result<String, ResolveError>
    where
        B: InternerBackend<Symbol = S>,
        H: BuildHasher,
    {
        intersperse_with(self.try_resolve_parts(storage), || Ok(path_separator.0)).try_fold(
            String::new(),
            |mut acc, res| {
                res.map(|next| {
                    acc.push_str(next);
                    acc
                })
            },
        )
    }

    /// Resolve each part of the tag.
    ///
    /// Note that the returned string slices are views into the underlying interner
    /// data, which means you're holding a borrow on the interner as long as the slices
    /// are held. If you want to let go of the borrow, copy the slices into new owned
    /// strings.
    pub fn resolve_parts<'intern, B, H, C>(
        &self,
        storage: &'intern StorageLock<'_, L, B, H>,
    ) -> Result<C, ResolveError>
    where
        B: InternerBackend<Symbol = S>,
        H: BuildHasher,
        C: FromIterator<&'intern str>,
    {
        self.try_resolve_parts(storage).collect()
    }

    /// Try to resolve each part of the tag.
    ///
    /// This lets you partially resolve the tag, if for some reason individual
    /// parts don't resolve.
    ///
    /// Note that the returned string slices are views into the underlying interner
    /// data, which means you're holding a borrow on the interner as long as the slices
    /// are held. If you want to let go of the borrow, copy the slices into new owned
    /// strings.
    pub fn try_resolve_parts<'s, 'intern: 's, B, H>(
        &'s self,
        storage: &'intern StorageLock<'_, L, B, H>,
    ) -> impl Iterator<Item = Result<&'intern str, ResolveError>> + 's
    where
        B: InternerBackend<Symbol = S>,
        H: BuildHasher,
    {
        self.0
            .iter()
            .copied()
            .map(|part| storage.resolve(part).ok_or(ResolveError::PartNotFound))
    }
}

impl<L: Label, S: Symbol> Tag for MultipartTag<L, S> {
    type Label = L;
    type Symbol = S;

    fn resolve<B, H>(
        &self,
        storage: &StorageLock<'_, Self::Label, B, H>,
        key_value_separator: KeyValueSep,
        path_separator: PathSep,
    ) -> Result<String, ResolveError>
    where
        B: InternerBackend<Symbol = Self::Symbol>,
        H: BuildHasher,
    {
        self.resolve(storage, key_value_separator, path_separator)
    }

    fn kind(&self) -> TagKind {
        TagKind::Multipart
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
    Other,
}

/// A trait to implement on types that _carry_ [`Tag`]s.
///
/// This trait is generic over the tag type, to permit implementing
/// it for multiple types of tags.
pub trait Tagged<T: Tag> {
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
