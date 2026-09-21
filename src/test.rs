//! Tests for the crate's APIs.

use crate::brand::make_guard;
use crate::brand::Guard;
use crate::error::ParseError;
use crate::label::DefaultLabel;
use crate::label::Label;
use crate::parse::*;
use crate::storage::DefaultStorage;
use crate::tag::KeyValueSep;
use crate::tag::KeyValueTag;
use crate::tag::MultipartTag;
use crate::tag::PathSep;
use crate::tag::Tag;
use crate::tag::TagKind;
use crate::TagManager;
use anyhow::anyhow as err;
use anyhow::Result;
use string_interner::DefaultSymbol;
use string_interner::Symbol;

// Helper function to test that a tag that's parsed and then resolved
// back into a string results in the same string that was originally
// put into the manager.
fn test_roundtrip<'brand, L, S, T, P>(
    manager: &TagManager<'brand, L, S, T, P>,
    input: &str,
) -> Result<()>
where
    L: Label,
    S: Symbol,
    T: Tag<'brand, Label = L, Symbol = S>,
    P: Parser<'brand, Tag = T> + Send + Sync,
{
    let tag = manager.parse_tag(input)?;
    let output = manager.resolve_tag(&tag)?;
    assert_eq!(input, output);
    Ok(())
}

#[test]
fn roundtrip_plain_tag() -> Result<()> {
    make_guard!(guard);
    let manager = TagManager::builder()
        .parser(Plain::new())
        .storage(DefaultStorage::fresh(guard))
        .build();

    test_roundtrip(&manager, "hello")
}

#[test]
#[cfg(feature = "convert_case")]
fn transform_tag() -> Result<()> {
    let parser = Trim(
        TrimBounds::Both,
        ChangeCase(Case::Snake, KeyValue::new(KvPolicy::NoAmbiguousSep)),
    );

    make_guard!(guard);
    let manager = TagManager::builder()
        .parser(parser)
        .storage(DefaultStorage::fresh(guard))
        .key_value_separator(KeyValueSep("/"))
        .build();

    let tag = manager.parse_tag(" \t     HELLO_WORLD/GOODBYE_WORLD    ")?;
    let interner = manager.storage().lock()?;
    let (key, value) = tag.resolve_key_value(&interner)?;

    assert_eq!(key, "hello_world");
    assert_eq!(value, "goodbye_world");

    Ok(())
}

#[test]
fn roundtrip_key_value_tag_unambiguous() -> Result<()> {
    make_guard!(guard);
    let manager = TagManager::builder()
        .parser(KeyValue::new(KvPolicy::NoAmbiguousSep))
        .storage(DefaultStorage::fresh(guard))
        .build();

    test_roundtrip(&manager, "hello:world")
}

#[test]
fn key_part_key_value_tag_unambiguous() -> Result<()> {
    make_guard!(guard);
    let manager = TagManager::builder()
        .parser(KeyValue::new(KvPolicy::NoAmbiguousSep))
        .storage(DefaultStorage::fresh(guard))
        .build();

    let input = "hello:world";
    let tag = manager.parse_tag(input)?;
    let lock = manager.storage().lock()?;
    let (key, value) = tag.resolve_key_value(&lock)?;
    assert_eq!(key, "hello");
    assert_eq!(value, "world");

    Ok(())
}

#[test]
fn roundtrip_key_value_tag_split_first() -> Result<()> {
    make_guard!(guard);
    let manager = TagManager::builder()
        .parser(KeyValue::new(KvPolicy::SplitOnFirstSep))
        .storage(DefaultStorage::fresh(guard))
        .build();

    test_roundtrip(&manager, "hello:world")
}

#[test]
fn key_part_key_value_tag_split_first() -> Result<()> {
    make_guard!(guard);
    let manager = TagManager::builder()
        .parser(KeyValue::new(KvPolicy::SplitOnFirstSep))
        .storage(DefaultStorage::fresh(guard))
        .build();

    let input = "hello:world:today";
    let tag = manager.parse_tag(input)?;
    let lock = manager.storage().lock()?;
    let (key, value) = tag.resolve_key_value(&lock)?;
    assert_eq!(key, "hello");
    assert_eq!(value, "world:today");
    Ok(())
}

#[test]
fn roundtrip_key_value_tag_split_last() -> Result<()> {
    make_guard!(guard);
    let manager = TagManager::builder()
        .parser(KeyValue::new(KvPolicy::SplitOnLastSep))
        .storage(DefaultStorage::fresh(guard))
        .build();

    test_roundtrip(&manager, "hello:world")
}

#[test]
fn key_part_key_value_tag_split_last() -> Result<()> {
    make_guard!(guard);
    let manager = TagManager::builder()
        .parser(KeyValue::new(KvPolicy::SplitOnLastSep))
        .storage(DefaultStorage::fresh(guard))
        .build();

    let input = "hello:world:today";
    let tag = manager.parse_tag(input)?;
    let lock = manager.storage().lock()?;
    let (key, value) = tag.resolve_key_value(&lock)?;
    assert_eq!(key, "hello:world");
    assert_eq!(value, "today");
    Ok(())
}

#[test]
fn roundtrip_multipart_tag() -> Result<()> {
    make_guard!(guard);
    let manager = TagManager::builder()
        .parser(Multipart::new(MultipartPolicy::RequireMultipart))
        .storage(DefaultStorage::fresh(guard))
        .path_separator(PathSep(":"))
        .build();

    test_roundtrip(&manager, "hello:world:today:its:me")
}

#[test]
#[cfg(all(feature = "convert_case", feature = "either"))]
fn complex_parser() -> Result<()> {
    make_guard!(guard);
    let manager = TagManager::builder()
        .parser(Trim(
            TrimBounds::Both,
            ChangeCase(
                Case::Snake,
                Or(
                    Multipart::new(MultipartPolicy::RequireMultipart),
                    Or(KeyValue::new(KvPolicy::NoAmbiguousSep), Plain::new()),
                ),
            ),
        ))
        .storage(DefaultStorage::fresh(guard))
        .build();

    let tags: Vec<_> =
        manager.parse_tags_into_with_kind(["lotr/legolas/friends", "score:5", "rustlang"]);

    let mut iter = tags.into_iter();
    let (t1, t2, t3) = (
        iter.next().ok_or(err!("nothing"))??,
        iter.next().ok_or(err!("nothing"))??,
        iter.next().ok_or(err!("nothing"))??,
    );

    assert_eq!(t1.1, TagKind::Multipart);
    assert_eq!(t2.1, TagKind::KeyValue);
    assert_eq!(t3.1, TagKind::Plain);

    Ok(())
}

// Helper to build a key-value manager with the default separator and the given policy.
fn key_value_manager<'brand>(
    guard: Guard<'brand>,
    policy: KvPolicy,
) -> TagManager<'brand, DefaultLabel, DefaultSymbol, KeyValueTag<'brand>, KeyValue> {
    TagManager::builder()
        .parser(KeyValue::new(policy))
        .storage(DefaultStorage::fresh(guard))
        .build()
}

#[test]
fn key_value_tag_rejects_empty_key() {
    for policy in [
        KvPolicy::NoAmbiguousSep,
        KvPolicy::SplitOnFirstSep,
        KvPolicy::SplitOnLastSep,
    ] {
        make_guard!(guard);
        let manager = key_value_manager(guard, policy);
        assert!(
            matches!(manager.parse_tag(":world"), Err(ParseError::MissingKey)),
            "{policy:?} should reject an empty key"
        );
    }
}

#[test]
fn key_value_tag_rejects_empty_value() {
    for policy in [
        KvPolicy::NoAmbiguousSep,
        KvPolicy::SplitOnFirstSep,
        KvPolicy::SplitOnLastSep,
    ] {
        make_guard!(guard);
        let manager = key_value_manager(guard, policy);
        assert!(
            matches!(manager.parse_tag("hello:"), Err(ParseError::MissingValue)),
            "{policy:?} should reject an empty value"
        );
    }
}

#[test]
fn key_value_tag_rejects_separator_only() {
    for policy in [
        KvPolicy::NoAmbiguousSep,
        KvPolicy::SplitOnFirstSep,
        KvPolicy::SplitOnLastSep,
    ] {
        make_guard!(guard);
        let manager = key_value_manager(guard, policy);
        assert!(
            matches!(manager.parse_tag(":"), Err(ParseError::MissingKey)),
            "{policy:?} should reject a bare separator"
        );
    }
}

#[test]
fn key_value_tag_missing_separator_is_missing_value() {
    for policy in [
        KvPolicy::NoAmbiguousSep,
        KvPolicy::SplitOnFirstSep,
        KvPolicy::SplitOnLastSep,
    ] {
        make_guard!(guard);
        let manager = key_value_manager(guard, policy);
        assert!(
            matches!(manager.parse_tag("hello"), Err(ParseError::MissingValue)),
            "{policy:?} should report a missing value when there's no separator"
        );
    }
}

#[test]
fn key_value_tag_still_rejects_ambiguous_separators() {
    make_guard!(guard);
    let manager = key_value_manager(guard, KvPolicy::NoAmbiguousSep);
    assert!(matches!(
        manager.parse_tag("hello:world:today"),
        Err(ParseError::AmbiguousKeyValueTag)
    ));
}

#[test]
fn batch_parse_and_resolve_roundtrip() -> Result<()> {
    make_guard!(guard);
    let manager = TagManager::builder()
        .parser(Plain::new())
        .storage(DefaultStorage::fresh(guard))
        .build();

    let inputs = ["hello", "world", "today"];

    // Both of these take the storage lock exactly once, for the whole batch.
    let tags: Vec<_> = manager.parse_tags_into::<Result<Vec<_>, _>>(inputs)?;
    let resolved: Vec<String> = manager.resolve_tags_into::<Result<Vec<_>, _>>(&tags)?;

    assert_eq!(resolved, inputs);

    Ok(())
}

#[test]
fn batch_parse_reports_per_tag_errors() {
    make_guard!(guard);
    let manager = key_value_manager(guard, KvPolicy::NoAmbiguousSep);

    let results: Vec<_> = manager.parse_tags_into::<Vec<_>>(["good:tag", "bad", "also:good"]);

    assert!(results[0].is_ok());
    assert!(matches!(results[1], Err(ParseError::MissingValue)));
    assert!(results[2].is_ok());
}

// Helper to build a multipart manager with the default separator and the given policy.
fn multipart_manager<'brand>(
    guard: Guard<'brand>,
    policy: MultipartPolicy,
) -> TagManager<'brand, DefaultLabel, DefaultSymbol, MultipartTag<'brand>, Multipart> {
    TagManager::builder()
        .parser(Multipart::new(policy))
        .storage(DefaultStorage::fresh(guard))
        .build()
}

#[test]
fn multipart_tag_rejects_empty_parts() {
    // Interior, leading, trailing, and a bare separator, under both policies.
    let inputs = ["a//b", "/a", "a/", "/", "a//", "//a"];

    for policy in [
        MultipartPolicy::PermitOnePart,
        MultipartPolicy::RequireMultipart,
    ] {
        make_guard!(guard);
        let manager = multipart_manager(guard, policy);

        for input in inputs {
            assert!(
                matches!(manager.parse_tag(input), Err(ParseError::EmptyPart)),
                "{policy:?} should reject {input:?}"
            );
        }
    }
}

#[test]
fn multipart_tag_accepts_non_empty_parts() -> Result<()> {
    make_guard!(guard);
    let manager = multipart_manager(guard, MultipartPolicy::RequireMultipart);

    test_roundtrip(&manager, "a/b")?;
    test_roundtrip(&manager, "lotr/legolas/friends")?;

    Ok(())
}

#[test]
fn multipart_tag_single_part_still_depends_on_policy() -> Result<()> {
    // A single part is not an *empty* part, so the policy still decides.
    make_guard!(permissive);
    test_roundtrip(
        &multipart_manager(permissive, MultipartPolicy::PermitOnePart),
        "solo",
    )?;

    make_guard!(strict);
    assert!(matches!(
        multipart_manager(strict, MultipartPolicy::RequireMultipart).parse_tag("solo"),
        Err(ParseError::SinglePartMultipart)
    ));

    Ok(())
}
