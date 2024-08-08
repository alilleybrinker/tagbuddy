//! Tests for the crate's APIs.

use crate::label::Label;
use crate::parse::*;
use crate::storage::Storage;
use crate::tag::KeyValueSep;
use crate::tag::PathSep;
use crate::tag::Tag;
use crate::tag::TagKind;
use crate::TagManager;
use anyhow::anyhow as err;
use anyhow::Result;
use string_interner::Symbol;

// Helper function to test that a tag that's parsed and then resolved
// back into a string results in the same string that was originally
// put into the manager.
fn test_roundtrip<L, S, T, P>(manager: &TagManager<L, S, T, P>, input: &str) -> Result<()>
where
    L: Label,
    S: Symbol,
    T: Tag<Label = L, Symbol = S>,
    P: Parser<Tag = T> + Send + Sync,
{
    let tag = manager.parse_tag(input)?;
    let output = manager.resolve_tag(&tag)?;
    assert_eq!(input, output);
    Ok(())
}

#[test]
fn roundtrip_plain_tag() -> Result<()> {
    let manager = TagManager::builder()
        .parser(Plain::new())
        .storage(Storage::default())
        .build();

    test_roundtrip(&manager, "hello")
}

#[test]
fn transform_tag() -> Result<()> {
    let parser = Trim(
        TrimBounds::Both,
        ChangeCase(Case::Snake, KeyValue::new(KvPolicy::NoAmbiguousSep)),
    );

    let manager = TagManager::builder()
        .parser(parser)
        .storage(Storage::default())
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
    let manager = TagManager::builder()
        .parser(KeyValue::new(KvPolicy::NoAmbiguousSep))
        .storage(Storage::default())
        .build();

    test_roundtrip(&manager, "hello:world")
}

#[test]
fn key_part_key_value_tag_unambiguous() -> Result<()> {
    let manager = TagManager::builder()
        .parser(KeyValue::new(KvPolicy::NoAmbiguousSep))
        .storage(Storage::default())
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
    let manager = TagManager::builder()
        .parser(KeyValue::new(KvPolicy::SplitOnFirstSep))
        .storage(Storage::default())
        .build();

    test_roundtrip(&manager, "hello:world")
}

#[test]
fn key_part_key_value_tag_split_first() -> Result<()> {
    let manager = TagManager::builder()
        .parser(KeyValue::new(KvPolicy::SplitOnFirstSep))
        .storage(Storage::default())
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
    let manager = TagManager::builder()
        .parser(KeyValue::new(KvPolicy::SplitOnLastSep))
        .storage(Storage::default())
        .build();

    test_roundtrip(&manager, "hello:world")
}

#[test]
fn key_part_key_value_tag_split_last() -> Result<()> {
    let manager = TagManager::builder()
        .parser(KeyValue::new(KvPolicy::SplitOnLastSep))
        .storage(Storage::default())
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
    let manager = TagManager::builder()
        .parser(Multipart::new(MultipartPolicy::RequireMultipart))
        .storage(Storage::default())
        .path_separator(PathSep(":"))
        .build();

    test_roundtrip(&manager, "hello:world:today:its:me")
}

#[test]
fn complex_parser() -> Result<()> {
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
        .storage(Storage::default())
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
