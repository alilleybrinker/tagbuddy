//! Enables compile-time separation of tags from different managers.

#[cfg(doc)]
use crate::generate_label;
#[cfg(doc)]
use crate::tag::Tag;
#[cfg(doc)]
use crate::TagManager;

/// A type for compile-time separation of [`Tag`]s from different [`TagManager`]s.
///
/// This enables the compile-time guarantee that a [`Tag`] generated by one
/// [`TagManager`] is never resolved through another [`TagManager`] (which might
/// succeed but produce incorrect or nonsensical results at runtime if permitted).
///
/// # Safety
///
/// Note that the usage of [`Label`] types in this crate never actually instantiates
/// any value of that type, and in general we recommend using a zero-sized type
/// (this is what's done by the [`generate_label`] macro).
///
/// There aren't actual safety concerns around its use, and no choice to implement
/// or not implement this marker trait can actually break safety guarantees of the
/// crate, but it's marked `unsafe` to hint toward the special guarantees around
/// the marker trait and encourage its construction through [`generate_label`].
pub unsafe trait Label: Copy {}

/// Generate a new type implementing [`Label`].
///
/// # Example
///
/// ```
/// # use tagbuddy::generate_label;
///
/// generate_label! {
///     /// Tags used for blog posts in this blog implementation.
///     pub BlogPostTags {}
/// }
/// ```
#[macro_export]
macro_rules! generate_label {
    ( $( $( #[$($attrss:meta)*] )* $visibility:vis $struct_name:ident {} )* ) => {
        $(
            $crate::generate_label! { @single $( #[$($attrss)*] )* $visibility $struct_name {} }
        )*
    };

    ( @single $( #[$($attrss:meta)*] )* $visibility:vis $struct_name:ident {} ) => {
        $( #[$($attrss)*] )*
        #[derive(Debug, Copy, Clone)]
        $visibility struct $struct_name;
        unsafe impl $crate::label::Label for $struct_name {}
    };
}

generate_label! {
    /// The default label applied when no other label is provided.
    pub DefaultLabel {}
}
