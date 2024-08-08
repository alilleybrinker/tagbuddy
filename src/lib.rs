//! A Rust crate for creating and managing tags and their relationships.
//!
//! "Tags" are string data which may or may not contain some structure, like
//! key-value pairs or multipart segments, and which are attached as metadata
//! to annotate data for organization.
//!
//! This crate defines a set of mechanisms for generically parsing, storing,
//! comparing, and querying sets of tags according to configured policies.

pub mod error;
pub mod label;
mod manager;
pub mod parse;
pub mod query;
pub mod storage;
pub mod tag;
#[cfg(test)]
mod test;

pub use crate::manager::TagManager;

pub mod builder {
    //! Contains a builder type for the [`TagManager`].

    #[cfg(doc)]
    use crate::TagManager;

    #[doc(inline)]
    pub use crate::manager::TagManagerBuilder;
}
