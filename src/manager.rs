//! Produce and resolve tags.

use crate::error::ResolveError;
use crate::label::DefaultLabel;
use crate::label::Label;
use crate::parse::*;
use crate::storage::Storage;
use crate::tag::KeyValueSep;
#[cfg(doc)]
use crate::tag::KeyValueTag;
#[cfg(doc)]
use crate::tag::MultipartTag;
use crate::tag::PathSep;
use crate::tag::PlainTag;
use crate::tag::Tag;
use crate::{error::ParseError, tag::TagKind};
#[cfg(doc)]
use std::sync::Mutex;
use std::{convert::identity, hash::BuildHasher};
use string_interner::backend::Backend as InternerBackend;
use string_interner::DefaultBackend;
use string_interner::DefaultHashBuilder;
use string_interner::DefaultSymbol;
#[cfg(doc)]
use string_interner::StringInterner;
use string_interner::Symbol;
use typed_builder::TypedBuilder;

/// Constructs [`Tag`]s according to the configured parser and interner.
///
/// A single [`TagManager`] is responsible for parsing and resolving tags that
/// match the rules of a single configured parser, with storage handled by
/// an underlying [`StringInterner`]. The [`StringInterner`] may be shared
/// with other [`TagManager`]s.
///
/// [`TagManager`] is designed to be generic over:
///
/// - The parser used to produce tags.
/// - The interner used to store tag data.
///
/// The trait bounds on [`TagManager`] ensure that the parser and interner
/// agree on the [`Symbol`] used as handles for the stored string data.
/// This is required because the parser produces [`Tag`]s which store
/// [`Symbol`]s so they can later be resolved into [`String`]s to recover
/// the full originally-input tag data.
///
/// The manner in which tag data is stored depends on the `T` parameter.
/// [`PlainTag`] stores the full string data in the interner. [`KeyValueTag`]
/// stores the key and value data separately, on the expectation that keys
/// especially will be repeated, and thus a lot of space saving is achieved by
/// deduplicating them through separate interning. [`MultipartTag`] stores
/// each part separately, again on the expectation that individual parts will
/// be frequently repeated across tags, resulting in space savings from interning.
#[derive(TypedBuilder)]
pub struct TagManager<
    L = DefaultLabel,
    S = DefaultSymbol,
    T = PlainTag<L, S>,
    P = Plain<L, S>,
    B = DefaultBackend<S>,
    H = DefaultHashBuilder,
> where
    L: Label,
    S: Symbol,
    T: Tag<Label = L, Symbol = S>,
    P: Parser<Tag = T> + Send + Sync,
    B: InternerBackend<Symbol = S>,
    H: BuildHasher,
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
    pub(crate) storage: Storage<L, B, H>,
}

// These `Send` and `Sync` impls are safe _because_:
//
// 1. `key_value_separator` and `path_separator` are just read-only string slices, so they are
//    trivially `Send` and `Sync`.
// 2. `parser` is constrained to be `Send` and `Sync`, either trivially-so, or by being wrapped
//    in an `Arc<Mutex<_>>` (in which case it takes advantage of an auto-impl for `Parser`
//    that tries to lock the parser before parsing can proceed).
// 3. `storage` is _always_ wrapped in an `Arc<Mutex<_>>`, so it is always `Send` and `Sync`.
//
// Given the above, `TagManager` is _always_ safe to send and sync, and can implement these traits.

unsafe impl<L, S, T, P, B, H> Send for TagManager<L, S, T, P, B, H>
where
    L: Label,
    S: Symbol,
    T: Tag<Label = L, Symbol = S>,
    P: Parser<Tag = T> + Send + Sync,
    B: InternerBackend<Symbol = S>,
    H: BuildHasher,
{
}

unsafe impl<L, S, T, P, B, H> Sync for TagManager<L, S, T, P, B, H>
where
    L: Label,
    S: Symbol,
    T: Tag<Label = L, Symbol = S>,
    P: Parser<Tag = T> + Send + Sync,
    B: InternerBackend<Symbol = S>,
    H: BuildHasher,
{
}

impl<
        L: Label,
        S: Symbol,
        T: Tag<Label = L, Symbol = S>,
        P: Parser<Tag = T> + Send + Sync,
        B: InternerBackend<Symbol = S>,
        H: BuildHasher,
    > TagManager<L, S, T, P, B, H>
{
    /// Attempt to parse a structured tag from the provided "raw" tag.
    ///
    /// This may fail if the tag is empty, or if it violates the configured [`Parser`]'s rules.
    pub fn parse_tag(&self, raw: &str) -> Result<P::Tag, ParseError> {
        self.parser.parse(
            &mut self.storage.lock()?,
            self.key_value_separator,
            self.path_separator,
            raw,
        )
    }

    /// Parse tags into a collection of your choosing.
    ///
    /// Note this can perform strictly better than `parse_tag`, because it takes the lock on the
    /// storage before starting to parse _any_ tags, and holds it for the duration.
    pub fn parse_tags_into<'raw, C>(&self, src: impl IntoIterator<Item = &'raw str>) -> C
    where
        C: FromIterator<Result<P::Tag, ParseError>>,
    {
        self.parse_tags_into_with(src, identity)
    }

    /// Parse tags into a collection of your choosing.
    ///
    /// Note this can perform strictly better than `parse_tag`, because it takes the lock on the
    /// storage before starting to parse _any_ tags, and holds it for the duration.
    pub fn parse_tags_into_with_kind<'raw, C>(&self, src: impl IntoIterator<Item = &'raw str>) -> C
    where
        C: FromIterator<Result<(P::Tag, TagKind), ParseError>>,
    {
        self.parse_tags_into_with(src, |t| {
            let kind = t.kind();
            (t, kind)
        })
    }

    /// Parse tags into a collection of your choosing.
    ///
    /// Note this can perform strictly better than `parse_tag`, because it takes the lock on the
    /// storage before starting to parse _any_ tags, and holds it for the duration.
    pub fn parse_tags_into_with<'raw, O, C>(
        &self,
        src: impl IntoIterator<Item = &'raw str>,
        f: impl FnOnce(P::Tag) -> O + Copy,
    ) -> C
    where
        C: FromIterator<Result<O, ParseError>>,
    {
        src.into_iter()
            .map(move |raw| {
                self.parser
                    .parse(
                        &mut self.storage.lock()?,
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
    /// Note that this may fail to resolve a tag if the tag wasn't interned
    /// in the current [`TagManager`]. It may alternatively resolve an incorrect tag.
    pub fn resolve_tag(&self, tag: &P::Tag) -> Result<String, ResolveError> {
        tag.resolve(
            &self.storage.lock()?,
            self.key_value_separator,
            self.path_separator,
        )
    }

    /// Get the string representation of a set of [`Tag`]s.
    ///
    /// Note this can perform strictly better than `resolve_tag` because it takes the storage lock
    /// before beginning iteration, and holds it for the duration.
    pub fn resolve_tags_into<'tag, C>(&self, src: impl IntoIterator<Item = &'tag P::Tag>) -> C
    where
        P::Tag: 'tag,
        C: FromIterator<Result<String, ResolveError>>,
    {
        self.resolve_tags_into_with(src, identity)
    }

    /// Get the string representation of a set of [`Tag`]s.
    ///
    /// Note this can perform strictly better than `resolve_tag` because it takes the storage lock
    /// before beginning iteration, and holds it for the duration.
    pub fn resolve_tags_into_with<'tag, O, C>(
        &self,
        src: impl IntoIterator<Item = &'tag P::Tag>,
        f: impl FnOnce(String) -> O + Copy,
    ) -> C
    where
        P::Tag: 'tag,
        C: FromIterator<Result<O, ResolveError>>,
    {
        src.into_iter()
            .map(move |tag| {
                tag.resolve(
                    &self.storage.lock()?,
                    self.key_value_separator,
                    self.path_separator,
                )
                .map(f)
            })
            .collect()
    }

    /// Get the inner [`Storage`] of the [`TagManager`].
    pub fn storage(&self) -> &Storage<L, B, H> {
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
