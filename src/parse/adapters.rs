//! Types which augment or modify the behavior of an underlying parser.

use crate::error::ParseError;
#[cfg(feature = "either")]
use crate::label::Label;
use crate::parse::Parser;
use crate::storage::StorageLock;
use crate::tag::KeyValueSep;
use crate::tag::PathSep;
use crate::tag::Tag;
#[cfg(feature = "convert_case")]
pub use convert_case::Case;
#[cfg(feature = "convert_case")]
use convert_case::Casing as _;
#[cfg(feature = "either")]
use either::Either;
#[cfg(feature = "regex")]
use regex::Regex;
#[cfg(feature = "regex")]
use regex::Replacer;
use std::hash::BuildHasher;
use string_interner::backend::Backend as InternerBackend;
#[cfg(feature = "either")]
use string_interner::Symbol;

// Helper macro to generate parser adapters.
macro_rules! adapters {
    (
        $(
            $( #[$($attrss:meta)*] )*
            $struct:ident $(< $($type_var:ident: $type_bound:ident),* >)? $(($($field_ty:ty),*))? => { $adapter:expr }
        )*
    ) => {
        $(
            adapters! {
                @single
                $( #[$($attrss)*] )*
                $struct $(< $($type_var: $type_bound),* >)* $(($($field_ty),*))* => { $adapter }
            }
        )*
    };

    (
        @single
        $(#[$($attrss:meta)*] )*
        $struct:ident $(< $($type_var:ident: $type_bound:ident),* >)? $(($($field_ty:ty),* ))? => { $adapter:expr }
    ) => {
        $( #[$($attrss)*] )*
        #[derive(Debug, Clone)]
        // No `Parser` bound on the struct itself: `Parser` is branded now, and an
        // adapter is brand-agnostic -- it's the impls below that tie it to a brand.
        pub struct $struct<$($($type_var: $type_bound),*,)* P>($($(pub $field_ty),*,)* pub P);

        impl<'brand, $($($type_var: $type_bound),*,)* P: Parser<'brand>> $struct<$($($type_var),*,)* P> {
            /// Parse a token with the given `interner` and `separator`.
            #[allow(clippy::redundant_closure_call)]
            fn parse<B, H>(
                &self,
                storage: &mut StorageLock<'_, 'brand, <P::Tag as Tag<'brand>>::Label, B, H>,
                key_value_separator: KeyValueSep,
                path_separator: PathSep,
                raw: &str,
            ) -> Result<P::Tag, ParseError>
            where
                B: InternerBackend<Symbol = <P::Tag as Tag<'brand>>::Symbol>,
                H: BuildHasher
            {
                ($adapter)(self, storage, key_value_separator, path_separator, raw)
            }

        }

        impl<'brand, $($($type_var: $type_bound),*,)* P: Parser<'brand>> Parser<'brand> for $struct<$($($type_var),*,)* P> {
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
                H: BuildHasher
            {
                self.parse(storage, key_value_separator, path_separator, raw)
            }
        }
    };
}

adapters! {
    /// Trim whitespace from one or both sides of the tag.
    Trim(TrimBounds) => {
        |this: &Trim<P>, interner, kv_sep, path_sep, raw: &str| {
            let Trim::<P>(bounds, sub_parser) = this;
            let raw = match bounds {
                TrimBounds::Both => raw.trim(),
                TrimBounds::Start => raw.trim_start(),
                TrimBounds::End => raw.trim_end(),
            };
            sub_parser.parse(interner, kv_sep, path_sep, raw)
        }
    }

    /// Filter out tags longer than a maximum number of characters.
    MaxChar(usize) => {
        |this: &MaxChar<P>, interner, kv_sep, path_sep, raw: &str| {
            let MaxChar::<P>(limit, sub_parser) = this;

            if raw.chars().count() > *limit {
                return Err(ParseError::TagTooManyChars);
            }

            sub_parser.parse(interner, kv_sep, path_sep, raw)
        }
    }

    /// Filter out tags longer than a maximum number of bytes.
    MaxBytes(usize) => {
        |this: &MaxBytes<P>, interner, kv_sep, path_sep, raw: &str| {
            let MaxBytes::<P>(limit, sub_parser) = this;

            if raw.len() > *limit {
                return Err(ParseError::TagTooManyBytes);
            }

            sub_parser.parse(interner, kv_sep, path_sep, raw)
        }
    }
}

#[cfg(feature = "convert_case")]
adapters! {
    /// Change the case of the tag.
    ChangeCase(Case) => {
        |this: &ChangeCase<P>, interner, kv_sep, path_sep, raw: &str| {
            let ChangeCase::<P>(case, sub_parser) = this;
            let raw = raw.to_case(*case);
            sub_parser.parse(interner, kv_sep, path_sep, &raw)
        }
    }
}

#[cfg(feature = "regex")]
adapters! {
    /// Filter tags by matching against a regex.
    Match(Regex) => {
        |this: &Match<P>, interner, kv_sep, path_sep, raw: &str| {
            let Match::<P>(regex, sub_parser) = this;

            let raw = regex
                .is_match(raw)
                .then_some(raw)
                .ok_or(ParseError::TagDidntMatchRegex)?;

            sub_parser.parse(interner, kv_sep, path_sep, raw)
        }
    }

    /// Replace the content of a tag according to a regex.
    Replace<R: CloneableReplacer>(Regex, R, ReplaceCount) => {
        |this: &Replace<R, P>, interner, kv_sep, path_sep, raw: &str| {
            let Replace::<R, P>(regex, replacer, count, sub_parser) = this;

            let raw = match count {
                ReplaceCount::First => regex.replace(raw, replacer.clone()),
                ReplaceCount::N(count) => regex.replacen(raw, *count, replacer.clone()),
                ReplaceCount::All => regex.replace_all(raw, replacer.clone()),
            };

            sub_parser.parse(interner, kv_sep, path_sep, &raw)
        }
    }
}

/// A `regex::Replacer` that can be [`Clone`]d.
///
/// This is automatically implemented for any type that implements both
/// `regex::Replacer` and [`Clone`].
#[cfg(feature = "regex")]
pub trait CloneableReplacer: Replacer + Clone {}

#[cfg(feature = "regex")]
impl<T: Replacer + Clone> CloneableReplacer for T {}

// The `Or` adapter is implemented by hand, because making the adapter-generating
// macro learn how to handle all of these bounds and everything isn't worth it.

/// Apply one parser, and if it fails, apply the other one.
///
/// Note that the tokens produced by the two parsers have to support the same underlying
/// symbol type, as they're both being backed by the same interner for storage. That
/// constraint now lives on the [`Parser`] impl rather than on [`Or`] itself, so the two
/// tag types are inferred from the parsers instead of being spelled out.
#[cfg(feature = "either")]
#[derive(Debug)]
pub struct Or<P1, P2>(pub P1, pub P2);

#[cfg(feature = "either")]
impl<P1, P2> Or<P1, P2> {
    /// Parse a token with the given `interner` and `separator`.
    fn parse<'brand, L, S, B, H>(
        &self,
        storage: &mut StorageLock<'_, 'brand, L, B, H>,
        key_value_separator: KeyValueSep,
        path_separator: PathSep,
        raw: &str,
    ) -> Result<Either<P1::Tag, P2::Tag>, ParseError>
    where
        L: Label,
        S: Symbol,
        P1: Parser<'brand>,
        P2: Parser<'brand>,
        P1::Tag: Tag<'brand, Label = L, Symbol = S>,
        P2::Tag: Tag<'brand, Label = L, Symbol = S>,
        B: InternerBackend<Symbol = S>,
        H: BuildHasher,
    {
        self.0
            .parse(storage, key_value_separator, path_separator, raw)
            .map(Either::Left)
            .or_else(|err1| {
                self.1
                    .parse(storage, key_value_separator, path_separator, raw)
                    .map(Either::Right)
                    .map_err(|err2| ParseError::FailedOr(Box::new(err1), Box::new(err2)))
            })
    }
}

#[cfg(feature = "either")]
impl<'brand, L, S, P1, P2> Parser<'brand> for Or<P1, P2>
where
    L: Label,
    S: Symbol,
    P1: Parser<'brand>,
    P2: Parser<'brand>,
    P1::Tag: Tag<'brand, Label = L, Symbol = S>,
    P2::Tag: Tag<'brand, Label = L, Symbol = S>,
{
    type Tag = Either<P1::Tag, P2::Tag>;

    fn parse<B, H>(
        &self,
        storage: &mut StorageLock<'_, 'brand, L, B, H>,
        key_value_separator: KeyValueSep,
        path_separator: PathSep,
        raw: &str,
    ) -> Result<Self::Tag, ParseError>
    where
        B: InternerBackend<Symbol = S>,
        H: BuildHasher,
    {
        self.parse(storage, key_value_separator, path_separator, raw)
    }
}

/// Sets which side(s) of the raw tag should be trimmed of whitespace.
#[derive(Debug, PartialEq, Eq, Copy, Clone, Hash)]
pub enum TrimBounds {
    /// Both sides should be trimmed.
    Both,

    /// Just the starting side should be trimmed.
    Start,

    /// Just the ending side should be trimmed.
    End,
}

#[cfg_attr(
    feature = "regex",
    doc = "Sets how many replacements should be done when using the [`Replace`] adapter."
)]
#[cfg_attr(
    not(feature = "regex"),
    doc = "Sets how many replacements should be done when using the `Replace` adapter."
)]
#[derive(Debug, PartialEq, Eq, Copy, Clone, Hash)]
pub enum ReplaceCount {
    /// Replace just the first instance of the regex match.
    First,

    /// Replace the first `N` instances of the regex match.
    N(usize),

    /// Replace all instances of the regex match.
    All,
}
