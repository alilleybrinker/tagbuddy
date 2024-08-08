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
pub trait Parser {
    /// The type of [`Tag`] produced by the [`Parser`].
    type Tag: Tag;

    /// Parse a given string to produce a new [`Tag`].
    fn parse<B, H>(
        &self,
        storage: &mut StorageLock<'_, <Self::Tag as Tag>::Label, B, H>,
        key_value_separator: KeyValueSep,
        path_separator: PathSep,
        raw: &str,
    ) -> Result<Self::Tag, ParseError>
    where
        B: InternerBackend<Symbol = <Self::Tag as Tag>::Symbol>,
        H: BuildHasher;
}

// Implement Parser for any Parser wrapped in `Arc<Mutex<_>>`, to enable
// passing externally-synchronized parsers in addition to trivially-synchronized ones,
// in cases where the parsers maintain internal state.
impl<P> Parser for Arc<Mutex<P>>
where
    P: Parser,
{
    type Tag = P::Tag;

    fn parse<B, H>(
        &self,
        storage: &mut StorageLock<'_, <Self::Tag as Tag>::Label, B, H>,
        key_value_separator: KeyValueSep,
        path_separator: PathSep,
        raw: &str,
    ) -> Result<Self::Tag, ParseError>
    where
        B: InternerBackend<Symbol = <Self::Tag as Tag>::Symbol>,
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
            #[allow(clippy::redundant_closure_call)]
            pub fn parse<B, H>(
                &self,
                storage: &mut StorageLock<'_, L, B, H>,
                key_value_separator: KeyValueSep,
                path_separator: PathSep,
                raw: &str
            ) -> Result<$tag<L, S>, ParseError>
            where
                S: Symbol,
                B: InternerBackend<Symbol = S>,
                H: BuildHasher
            {
                check_empty(raw)?;
                ($parser)(self, storage, key_value_separator, path_separator, raw)
            }
        }

        impl<L: Label, S: Symbol> Parser for $struct<L, S> {
            type Tag = $tag<L, S>;

            fn parse<B, H>(
                &self,
                storage: &mut StorageLock<'_, <Self::Tag as Tag>::Label, B, H>,
                key_value_separator: KeyValueSep,
                path_separator: PathSep,
                raw: &str
            ) -> Result<Self::Tag, ParseError>
            where
                B: InternerBackend<Symbol = <Self::Tag as Tag>::Symbol>,
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

parsers! {
    /// No internal structure, `':'` default separator.
    Plain {} => PlainTag {
        |_this, interner, _key_value_separator, _path_separator, raw| Ok(PlainTag::new(interner, raw))
    }

    /// Key-value parser, `':'` default separator.
    KeyValue { policy: KvPolicy } => KeyValueTag {
        |this: &KeyValue<L, S>, interner, key_value_separator: KeyValueSep, _path_separator, raw: &str| {
            match this.policy {
                KvPolicy::NoAmbiguousSep => {
                    let mut parts_iter = raw.split(key_value_separator.0);
                    let key = parts_iter.next().ok_or(ParseError::MissingKey)?;
                    let value = parts_iter.next().ok_or(ParseError::MissingValue)?;
                    match parts_iter.next() {
                        Some(_) => Err(ParseError::AmbiguousKeyValueTag),
                        None => Ok(KeyValueTag::new(interner, key, value))
                    }
                }
                KvPolicy::SplitOnFirstSep => {
                    match raw.split_once(key_value_separator.0) {
                        None => Err(ParseError::MissingValue),
                        Some((key, value)) => Ok(KeyValueTag::new(interner, key, value)),
                    }
                }
                KvPolicy::SplitOnLastSep => {
                    match raw.rsplit_once(key_value_separator.0) {
                        None => Err(ParseError::MissingValue),
                        Some((key, value)) => Ok(KeyValueTag::new(interner, key, value)),
                    }
                }
            }
        }
    }

    /// Multipart parser, splits parts on separator, `':'` default separator.
    Multipart { policy: MultipartPolicy } => MultipartTag {
        |this: &Multipart<L, S>, interner, _key_value_separator, path_separator: PathSep, raw: &str| {
            match this.policy {
                MultipartPolicy::PermitOnePart => Ok(MultipartTag::new(interner, raw.split(path_separator.0))),
                MultipartPolicy::RequireMultipart => {
                    let parts = raw.split(path_separator.0);

                    if parts.clone().count() < 2 {
                        return Err(ParseError::SinglePartMultipart);
                    }

                    Ok(MultipartTag::new(interner, parts))
                },
            }
        }
    }
}

/* # SAFETY
 *
 * There's no data to sync for any of these; the only fields involved
 * are read-only once the type is created (they just set configuration).
 * Since there's nothing to sync, there's no worry about deriving this.
 */

unsafe impl<L: Label, S: Symbol> Send for Plain<L, S> {}
unsafe impl<L: Label, S: Symbol> Sync for Plain<L, S> {}

unsafe impl<L: Label, S: Symbol> Send for KeyValue<L, S> {}
unsafe impl<L: Label, S: Symbol> Sync for KeyValue<L, S> {}

unsafe impl<L: Label, S: Symbol> Send for Multipart<L, S> {}
unsafe impl<L: Label, S: Symbol> Sync for Multipart<L, S> {}
