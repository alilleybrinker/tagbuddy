//! Errors for producing and consuming tags.

#[cfg(all(doc, feature = "either"))]
use crate::parse::Or;
#[cfg(doc)]
use crate::tag::MultipartTag;
#[cfg(doc)]
use crate::tag::Tag;
use std::error::Error as StdError;
use std::fmt::Display;
use std::fmt::Formatter;
use std::fmt::Result as FmtResult;

/// Error arising during parsing of new [`Tag`]s.
#[derive(Debug)]
#[non_exhaustive]
pub enum ParseError {
    /// Can't create an empty tag.
    EmptyTag,

    /// Key-value tag is missing a key.
    MissingKey,

    /// Key-value tag is missing a value.
    MissingValue,

    /// Key-value tag is ambiguous; key-value tags must have one separator.
    AmbiguousKeyValueTag,

    /// Tag didn't match a regular expression.
    TagDidntMatchRegex,

    /// Tag is more characters long than allowed.
    TagTooManyChars,

    /// Tag is more bytes long than allowed.
    TagTooManyBytes,

    /// Could not lock the parser prior to parsing.
    CouldNotLock,

    /// Tried to parse a single-part [`MultipartTag`].
    SinglePartMultipart,

    /// Tried to parse a [`MultipartTag`] with an empty part.
    ///
    /// A tag like `"a//b"`, `"/a"`, or `"a/"` has an empty part between or
    /// beside its separators.
    EmptyPart,

    #[cfg_attr(feature = "either", doc = "Failed an [`Or`] match.")]
    #[cfg_attr(not(feature = "either"), doc = "Failed an `Or` match.")]
    FailedOr(Box<ParseError>, Box<ParseError>),
}

impl Display for ParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        match self {
            ParseError::EmptyTag => write!(f, "can't create an empty tag"),
            ParseError::MissingKey => write!(f, "missing key in a key-value tag"),
            ParseError::MissingValue => write!(f, "missing value in a key-value tag"),
            ParseError::AmbiguousKeyValueTag => {
                write!(f, "ambiguous key-tag value; should have just one separator")
            }
            ParseError::TagDidntMatchRegex => write!(f, "tag didn't match the regular expression"),
            ParseError::TagTooManyChars => write!(f, "tag is too many characters long"),
            ParseError::TagTooManyBytes => write!(f, "tag is too many bytes long"),
            ParseError::CouldNotLock => write!(f, "could not lock parser"),
            ParseError::SinglePartMultipart => {
                write!(f, "can't accept a single-part multipart tag")
            }
            ParseError::EmptyPart => write!(f, "empty part in a multipart tag"),
            ParseError::FailedOr(e1, e2) => {
                write!(f, "failed two parsers with errors '{e1}' and '{e2}'")
            }
        }
    }
}

impl StdError for ParseError {}
