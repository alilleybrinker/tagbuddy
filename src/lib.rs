//! A Rust crate for creating and managing tags and their relationships.
//!
//! "Tags" are string data which may or may not contain some structure, like
//! key-value pairs or multipart segments, and which are attached as metadata
//! to annotate data for organization.
//!
//! This crate defines a set of mechanisms for generically parsing, storing,
//! comparing, and querying sets of tags according to configured policies.

/// Compiles the code blocks in `README.md` as doctests, so the README's examples
/// can't drift from the API. `cfg(doctest)` keeps it out of real builds and out of
/// the rendered docs.
#[cfg(doctest)]
#[doc = include_str!("../README.md")]
pub struct ReadmeDoctests;

pub mod brand {
    //! Compile-time identity for a single [`Storage`].
    //!
    //! A "brand" is an invariant lifetime unique to one [`Storage`], which every [`Tag`]
    //! interned into that storage carries. Because the lifetime is invariant, no two
    //! brands can ever unify, so the compiler rejects any attempt to resolve a tag
    //! through a storage that didn't intern it.
    //!
    //! Brands are minted with [`make_guard`], which produces a [`Guard`] that a
    //! [`Storage`] constructor consumes:
    //!
    //! ```
    //! # use tagbuddy::brand::make_guard;
    //! # use tagbuddy::storage::DefaultStorage;
    //! make_guard!(guard);
    //! let storage = DefaultStorage::fresh(guard);
    //! ```
    //!
    //! [`Storage`]: crate::storage::Storage
    //! [`Tag`]: crate::tag::Tag

    #[doc(inline)]
    pub use generativity::make_guard;
    pub use generativity::Guard;
    pub use generativity::Id;
}

pub mod error;
pub mod label;
mod manager;
pub mod parse;
pub mod query;
pub mod storage;
pub mod tag;
#[cfg(test)]
mod test;

pub use crate::manager::KeyOf;
pub use crate::manager::LabelOf;
pub use crate::manager::ManagerParts;
pub use crate::manager::TagManager;
pub use crate::manager::TagOf;

pub mod builder {
    //! Contains a builder type for the [`TagManager`].

    #[cfg(doc)]
    use crate::TagManager;

    #[doc(inline)]
    pub use crate::manager::TagManagerBuilder;
}
