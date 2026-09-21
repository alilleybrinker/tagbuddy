//! Different tag parsers and their strategies.

mod adapters;

use crate::error::ParseError;
use crate::label::DefaultLabel;
use crate::label::Label;
pub use crate::parse::adapters::*;
#[cfg(doc)]
use crate::storage::Storage;
use crate::storage::StorageLock;
use crate::tag::*;
#[cfg(doc)]
use crate::TagManager;
use std::hash::BuildHasher;
use std::marker::PhantomData;
use std::ops::Not as _;
use std::sync::Arc;
use std::sync::Mutex;
use string_interner::backend::Backend as InternerBackend;
use string_interner::DefaultSymbol;
use string_interner::Symbol;

/// Types that provide a strategy for parsing tags.
///
/// `Parser`s are required to be [`Send`] and [`Sync`] as we want [`TagManager`]
/// to be [`Send`] and [`Sync`]. For basic parsers that don't maintain
/// any internal state, this is trivial, but more complex parsers may
/// need to establish internal synchronization of their state in the case
/// that they are performing concurrent parses.
pub trait Parser<'brand> {
    /// The type of [`Tag`] produced by the [`Parser`].
    type Tag: Tag<'brand>;

    /// Parse a given string to produce a new [`Tag`].
    fn parse<B, H>(
        &self,
        storage: &mut StorageLock<'_, 'brand, <Self::Tag as Tag<'brand>>::Label, B, H>,
        key_value_separator: KeyValueSep,
        path_separator: PathSep,
        raw: &str,
    ) -> Result<Self::Tag, ParseError>
    where
        B: InternerBackend<Symbol = <Self::Tag as Tag<'brand>>::Symbol>,
        H: BuildHasher;
}

// Implement Parser for any Parser wrapped in `Arc<Mutex<_>>`, to enable
// passing externally-synchronized parsers in addition to trivially-synchronized ones,
// in cases where the parsers maintain internal state.
impl<'brand, P> Parser<'brand> for Arc<Mutex<P>>
where
    P: Parser<'brand>,
{
    type Tag = P::Tag;

    fn parse<B, H>(
        &self,
        storage: &mut StorageLock<'_, 'brand, <Self::Tag as Tag<'brand>>::Label, B, H>,
        key_value_separator: KeyValueSep,
        path_separator: PathSep,
        raw: &str,
    ) -> Result<Self::Tag, ParseError>
    where
        B: InternerBackend<Symbol = <Self::Tag as Tag<'brand>>::Symbol>,
        H: BuildHasher,
    {
        let internal_parser = self.lock().map_err(|_| ParseError::CouldNotLock)?;
        internal_parser.parse(storage, key_value_separator, path_separator, raw)
    }
}

/// The policy to use for splitting on separators in a [`KeyValue`].
#[derive(Debug, PartialEq, Eq, Copy, Clone, Hash)]
pub enum KvPolicy {
    /// Don't allow ambiguous separators. Only one separator is permitted.
    NoAmbiguousSep,

    /// Split keys and values on the first occurence of the separator.
    SplitOnFirstSep,

    /// Split keys and values on the last occurence of the separator.
    SplitOnLastSep,
}

/// The policy to use for permitting "single-part" [`MultipartTag`]s.
#[derive(Debug, PartialEq, Eq, Copy, Clone, Hash)]
pub enum MultipartPolicy {
    /// Permit single-part tags.
    PermitOnePart,

    /// Do not permit single-part tags.
    RequireMultipart,
}

/// Helper macro to construct tag parsers.
///
/// This macro:
///
/// 1. Defines each parser as either an empty struct or tuple struct with only public fields.
/// 2. Implements a `parse` inherent method, which calls `check_empty` and then whatever closure
///    is provided by the macro to implement the actual parsing behavior.
/// 3. Implements the `Parser` trait, with `Parser::parse` just delegating to the `parse`
///    inherent method.
///
/// The syntax of each parser-defining pattern is:
///
/// ```text
/// <doc_comment>
/// <struct_name>(<field_types>)? => <tag_type> {
///     <parser_closure>
/// }
/// ```
macro_rules! parsers {
    (
        $(
            $( #[$($attrss:meta)*] )*
            $struct:ident { $($field_name:ident: $field_ty:tt),* } => $tag:ident {
                $parser:expr
            }
        )*
    ) => {
        $(
            parsers! {
                @single
                $( #[$($attrss)*] )*
                $struct { $($field_name: $field_ty),* } => $tag {
                    $parser
                }
            }
        )*
    };

    (
        @single
        $(#[$($attrss:meta)*] )*
        $struct:ident { $($field_name:ident: $field_ty:tt),* } => $tag:ident {
            $parser:expr
        }
    ) => {
        $( #[$($attrss)*] )*
        #[derive(Debug, PartialEq, Eq, Copy, Clone, Hash)]
        pub struct $struct<L: Label = DefaultLabel, S: Symbol = DefaultSymbol> {
            _label: PhantomData<L>,
            _symbol: PhantomData<S>,
            $( $field_name: $field_ty ),*
        }

        impl<L, S> $struct<L, S> where L: Label, S: Symbol {
            /// Construct a new parser.
            #[allow(clippy::new_without_default)]
            pub fn new($( $field_name: $field_ty ),*) -> Self {
                Self {
                    _label: PhantomData,
                    _symbol: PhantomData,
                    $($field_name),*
                }
            }

            /// Parse a token with the given `interner` and `separator`.
            ///
            /// The produced tag carries the `'brand` of the storage it was interned into.
            #[allow(clippy::redundant_closure_call)]
            pub fn parse<'brand, B, H>(
                &self,
                storage: &mut StorageLock<'_, 'brand, L, B, H>,
                key_value_separator: KeyValueSep,
                path_separator: PathSep,
                raw: &str
            ) -> Result<$tag<'brand, L, S>, ParseError>
            where
                S: Symbol,
                B: InternerBackend<Symbol = S>,
                H: BuildHasher
            {
                check_empty(raw)?;
                ($parser)(self, storage, key_value_separator, path_separator, raw)
            }
        }

        impl<'brand, L: Label, S: Symbol> Parser<'brand> for $struct<L, S> {
            type Tag = $tag<'brand, L, S>;

            fn parse<B, H>(
                &self,
                storage: &mut StorageLock<'_, 'brand, <Self::Tag as Tag<'brand>>::Label, B, H>,
                key_value_separator: KeyValueSep,
                path_separator: PathSep,
                raw: &str
            ) -> Result<Self::Tag, ParseError>
            where
                B: InternerBackend<Symbol = <Self::Tag as Tag<'brand>>::Symbol>,
                H: BuildHasher
            {
                self.parse(storage, key_value_separator, path_separator, raw)
            }
        }
    };
}

/// Validate that the raw tag isn't empty, error out if it is.
fn check_empty(raw: &str) -> Result<(), ParseError> {
    raw.is_empty()
        .not()
        .then_some(())
        .ok_or(ParseError::EmptyTag)
}

/// Validate that neither side of a key-value tag is empty, error out if either is.
///
/// `check_empty` only rejects a wholly-empty raw tag, so a tag like `"key:"` or
/// `":value"` reaches the key-value parser with one side empty.
fn check_key_value(key: &str, value: &str) -> Result<(), ParseError> {
    if key.is_empty() {
        return Err(ParseError::MissingKey);
    }

    if value.is_empty() {
        return Err(ParseError::MissingValue);
    }

    Ok(())
}

/// Validate that no part of a multipart tag is empty, error out if any is.
///
/// Like `check_key_value`, this catches what `check_empty` can't: a tag such as
/// `"a//b"`, `"/a"`, or `"a/"` isn't empty itself, but has an empty part.
fn check_parts<'part>(parts: impl Iterator<Item = &'part str>) -> Result<(), ParseError> {
    for part in parts {
        if part.is_empty() {
            return Err(ParseError::EmptyPart);
        }
    }

    Ok(())
}

parsers! {
    /// No internal structure; the whole tag is interned as-is.
    Plain {} => PlainTag {
        |_this, interner, _key_value_separator, _path_separator, raw| Ok(PlainTag::new(interner, raw))
    }

    /// Key-value parser, `':'` default separator.
    KeyValue { policy: KvPolicy } => KeyValueTag {
        |this: &KeyValue<L, S>, interner, key_value_separator: KeyValueSep, _path_separator, raw: &str| {
            let (key, value) = match this.policy {
                KvPolicy::NoAmbiguousSep => {
                    let (key, value) = raw
                        .split_once(key_value_separator.0)
                        .ok_or(ParseError::MissingValue)?;

                    if value.contains(key_value_separator.0) {
                        return Err(ParseError::AmbiguousKeyValueTag);
                    }

                    (key, value)
                }
                KvPolicy::SplitOnFirstSep => {
                    raw.split_once(key_value_separator.0).ok_or(ParseError::MissingValue)?
                }
                KvPolicy::SplitOnLastSep => {
                    raw.rsplit_once(key_value_separator.0).ok_or(ParseError::MissingValue)?
                }
            };

            check_key_value(key, value)?;

            Ok(KeyValueTag::new(interner, key, value))
        }
    }

    /// Multipart parser, splits parts on separator, `'/'` default separator.
    Multipart { policy: MultipartPolicy } => MultipartTag {
        |this: &Multipart<L, S>, interner, _key_value_separator, path_separator: PathSep, raw: &str| {
            let parts = raw.split(path_separator.0);

            if this.policy == MultipartPolicy::RequireMultipart && parts.clone().count() < 2 {
                return Err(ParseError::SinglePartMultipart);
            }

            check_parts(parts.clone())?;

            Ok(MultipartTag::new(interner, parts))
        }
    }
}

/* # NOTE
 *
 * `Plain`, `KeyValue`, and `Multipart` deliberately have no hand-written `Send`
 * and `Sync` impls. Their only fields are `PhantomData` and read-only
 * configuration, so the compiler derives both for any label and symbol that are
 * themselves `Send`/`Sync` — which every label built by `generate_label` is,
 * being a unit struct. An `unsafe impl` here would only serve to paper over a
 * label that isn't.
 */
