//! Errors for producing and consuming tags.

#[cfg(doc)]
use crate::parse::Or;
#[cfg(doc)]
use crate::storage::Storage;
#[cfg(doc)]
use crate::tag::MultipartTag;
#[cfg(doc)]
use crate::tag::Tag;
#[cfg(doc)]
use crate::TagManager;
use std::error::Error as StdError;
use std::fmt::Display;
use std::fmt::Formatter;
use std::fmt::Result as FmtResult;
#[cfg(doc)]
use std::sync::Mutex;

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

    /// Failed an [`Or`] match.
    FailedOr(Box<ParseError>, Box<ParseError>),

    /// An underlying storage error arose.
    StorageError(StorageError),
}

impl From<StorageError> for ParseError {
    fn from(e: StorageError) -> Self {
        ParseError::StorageError(e)
    }
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
            ParseError::FailedOr(e1, e2) => {
                write!(f, "failed two parsers with errors '{e1}' and '{e2}'")
            }
            ParseError::StorageError(e) => write!(f, "{e}"),
        }
    }
}

impl StdError for ParseError {
    fn source(&self) -> Option<&(dyn StdError + 'static)> {
        match self {
            ParseError::StorageError(e) => Some(e),
            _ => None,
        }
    }
}

/// Errors arising when resolving [`Tag`]s.
#[derive(Debug)]
#[non_exhaustive]
pub enum ResolveError {
    /// Tag wasn't found in the [`TagManager`].
    TagNotFound,

    /// Key wasn't found in the [`TagManager`].
    KeyNotFound,

    /// Value wasn't found in the [`TagManager`].
    ValueNotFound,

    /// Part wasn't found in the [`TagManager`].
    PartNotFound,

    /// An underlying [`Storage`] error occurred.
    StorageError(StorageError),
}

impl From<StorageError> for ResolveError {
    fn from(e: StorageError) -> Self {
        ResolveError::StorageError(e)
    }
}

impl Display for ResolveError {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        match self {
            ResolveError::TagNotFound => write!(f, "tag wasn't found in tag manager"),
            ResolveError::KeyNotFound => write!(f, "key wasn't found in tag manager"),
            ResolveError::ValueNotFound => write!(f, "value wasn't found in tag manager"),
            ResolveError::PartNotFound => write!(f, "part wasn't found in tag manager"),
            ResolveError::StorageError(e) => write!(f, "{e}"),
        }
    }
}

impl StdError for ResolveError {
    fn source(&self) -> Option<&(dyn StdError + 'static)> {
        match self {
            ResolveError::StorageError(e) => Some(e),
            _ => None,
        }
    }
}

/// Errors arising when interacting with [`Storage`]s.
#[derive(Debug)]
#[non_exhaustive]
pub enum StorageError {
    /// Failed to lock the storage, likely because the [`Mutex`] is poisoned.
    CouldNotLock,
}

impl Display for StorageError {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        match self {
            StorageError::CouldNotLock => write!(f, "could not lock storage"),
        }
    }
}

impl StdError for StorageError {}
