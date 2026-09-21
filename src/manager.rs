//! Produce and resolve tags.

use crate::error::ParseError;
use crate::label::DefaultLabel;
use crate::label::Label;
use crate::parse::*;
#[cfg(doc)]
use crate::storage::Interner;
use crate::storage::Key;
use crate::storage::Spur;
use crate::storage::Storage;
use crate::tag::KeyValueSep;
#[cfg(doc)]
use crate::tag::KeyValueTag;
#[cfg(doc)]
use crate::tag::MultipartTag;
use crate::tag::PathSep;
use crate::tag::PlainTag;
use crate::tag::Tag;
use crate::tag::TagKind;
use std::collections::hash_map::RandomState;
use std::convert::identity;
use std::hash::BuildHasher;
use std::hash::Hash;
use typed_builder::TypedBuilder;

/// Constructs [`Tag`]s according to the configured parser and interner.
///
/// A single [`TagManager`] is responsible for parsing and resolving tags that
/// match the rules of a single configured parser, with storage handled by an
/// underlying [`Interner`]. The [`Interner`] may be shared with other
/// [`TagManager`]s, via [`Storage::share_as`].
///
/// [`TagManager`] is designed to be generic over:
///
/// - The parser used to produce tags.
/// - The key type and hasher used to store tag data.
///
/// The trait bounds on [`TagManager`] ensure that the parser and storage agree
/// on the [`Key`] used as a handle for the stored string data. This is required
/// because the parser produces [`Tag`]s which store [`Key`]s so they can later
/// be resolved back into [`String`]s to recover the full originally-input tag
/// data.
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
pub struct TagManager<
    'brand,
    L = DefaultLabel,
    K = Spur,
    T = PlainTag<'brand, L, K>,
    P = Plain<L, K>,
    H = RandomState,
> where
    L: Label,
    K: Key + Hash,
    T: Tag<'brand, Label = L, Key = K>,
    P: Parser<'brand, Tag = T> + Send + Sync,
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
    pub(crate) storage: Storage<'brand, L, K, H>,
}

impl<
        'brand,
        L: Label,
        K: Key + Hash,
        T: Tag<'brand, Label = L, Key = K>,
        P: Parser<'brand, Tag = T> + Send + Sync,
        H: BuildHasher + Clone,
    > TagManager<'brand, L, K, T, P, H>
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

    /// Get the inner [`Storage`] of the [`TagManager`].
    pub fn storage(&self) -> &Storage<'brand, L, K, H> {
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
