//! Tests for the crate's APIs.

use crate::brand::make_guard;
use crate::brand::Guard;
use crate::error::ParseError;
use crate::label::DefaultLabel;
use crate::parse::*;
use crate::storage::Capacity;
use crate::storage::DefaultStorage;
use crate::storage::Interner;
use crate::storage::Key;
use crate::storage::Spur;
use crate::storage::Storage;
#[cfg(feature = "convert_case")]
use crate::tag::KeyValueSep;
use crate::tag::PathSep;
#[cfg(all(feature = "convert_case", feature = "either"))]
use crate::tag::TagKind;
use crate::TagManager;
#[cfg(all(feature = "convert_case", feature = "either"))]
use anyhow::anyhow as err;
use anyhow::Result;
use std::collections::hash_map::DefaultHasher;
use std::hash::BuildHasher;
use std::hash::BuildHasherDefault;
use std::sync::Arc;

// Helper function to test that a tag that's parsed and then resolved
// back into a string results in the same string that was originally
// put into the manager.
fn test_roundtrip<'brand, P, H>(manager: &TagManager<'brand, P, H>, input: &str) -> Result<()>
where
    P: Parser<'brand> + Send + Sync,
    H: BuildHasher + Clone,
{
    let tag = manager.parse_tag(input)?;
    let output = manager.resolve_tag(&tag);
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
    let (key, value) = tag.resolve_key_value(manager.storage());

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
    let (key, value) = tag.resolve_key_value(manager.storage());
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
    let (key, value) = tag.resolve_key_value(manager.storage());
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
    let (key, value) = tag.resolve_key_value(manager.storage());
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
) -> TagManager<'brand, KeyValue> {
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

    let tags: Vec<_> = manager.parse_tags_into::<Result<Vec<_>, _>>(inputs)?;
    let resolved: Vec<String> = manager.resolve_tags_into::<Vec<_>>(&tags);

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
) -> TagManager<'brand, Multipart> {
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

#[test]
fn share_as_keeps_the_brand() -> Result<()> {
    make_guard!(guard);

    let manager = TagManager::builder()
        .parser(Plain::new())
        .storage(DefaultStorage::fresh(guard))
        .build();

    // A second manager over the same interner, sharing the brand.
    let shared = TagManager::builder()
        .parser(Plain::new())
        .storage(manager.storage().share_as::<DefaultLabel>())
        .build();

    let tag = manager.parse_tag("hello")?;

    // Same brand, so the tag resolves through either one.
    assert_eq!(manager.resolve_tag(&tag), "hello");
    assert_eq!(shared.resolve_tag(&tag), "hello");

    Ok(())
}

#[test]
fn try_share_as_adopts_the_brand_for_the_same_interner() -> Result<()> {
    make_guard!(guard);

    let manager = TagManager::builder()
        .parser(Plain::new())
        .storage(DefaultStorage::fresh(guard))
        .build();

    let tag = manager.parse_tag("hello")?;

    // A bare handle to the very same interner, separated from its storage.
    let handle = Arc::clone(manager.storage().handle());

    let adopted = manager
        .storage()
        .try_share_as::<DefaultLabel>(&handle)
        .expect("the handle is this storage's own interner");

    let adopted_manager = TagManager::builder()
        .parser(Plain::new())
        .storage(adopted)
        .build();

    // The brand came along, so the original tag still resolves.
    assert_eq!(adopted_manager.resolve_tag(&tag), "hello");

    Ok(())
}

#[test]
fn try_share_as_refuses_a_different_interner() {
    make_guard!(one);
    make_guard!(two);

    let first = DefaultStorage::fresh(one);
    let second = DefaultStorage::fresh(two);

    // A handle to a genuinely different interner must not inherit `first`'s brand.
    let foreign = Arc::clone(second.handle());
    assert!(first.try_share_as::<DefaultLabel>(&foreign).is_none());

    // ... while its own handle is accepted.
    let own = Arc::clone(first.handle());
    assert!(first.try_share_as::<DefaultLabel>(&own).is_some());
}

#[test]
fn tags_resolve_concurrently_without_a_lock() {
    make_guard!(guard);

    let manager = TagManager::builder()
        .parser(Plain::new())
        .storage(DefaultStorage::fresh(guard))
        .build();

    // Storage holds its interner behind an `Arc` with no `Mutex`, so many threads can
    // intern and resolve through `&TagManager` at once. Under the old `Mutex<_>` this
    // serialized; the point here is that it compiles at all -- `&TagManager` being
    // usable from several threads is what the lockless interner buys.
    std::thread::scope(|scope| {
        for _ in 0..8 {
            let manager = &manager;

            scope.spawn(move || {
                for i in 0..256 {
                    // Overlapping vocabularies, so threads race on the same strings.
                    let raw = format!("tag-{}", i % 16);
                    let tag = manager.parse_tag(&raw).expect("plain tags always parse");
                    assert_eq!(manager.resolve_tag(&tag), raw);
                }
            });
        }
    });

    // Every thread interned the same 16 strings, and interning dedupes.
    assert_eq!(manager.storage().len(), 16);
}

//---------------------------------------------------------------------------
// Storage construction and sharing

#[test]
fn deep_clone_copies_every_string() -> Result<()> {
    make_guard!(original_guard);
    let original = DefaultStorage::fresh(original_guard);

    let inputs = ["hello", "world", "today", "its", "me"];
    for input in inputs {
        original.get_or_intern(input);
    }

    make_guard!(copy_guard);
    let copy: DefaultStorage = original.deep_clone(copy_guard);

    assert_eq!(copy.len(), original.len());

    // Every string made it across, and resolves through the copy's own keys. This
    // deliberately doesn't assert that a string kept the *key* it had in the original:
    // `deep_clone` makes no such promise, and the brand stops stale keys reaching here
    // anyway.
    for input in inputs {
        let key = copy
            .get(input)
            .expect("every string should have been copied");
        assert_eq!(copy.resolve(key), input);
    }

    Ok(())
}

#[test]
fn deep_clone_is_independent_of_its_original() -> Result<()> {
    make_guard!(original_guard);
    let original = DefaultStorage::fresh(original_guard);
    original.get_or_intern("shared");

    make_guard!(copy_guard);
    let copy: DefaultStorage = original.deep_clone(copy_guard);

    // Interning into one doesn't reach the other: they're separate interners, which is
    // the whole reason the copy needs a guard of its own.
    original.get_or_intern("only-in-original");
    copy.get_or_intern("only-in-copy");

    assert!(original.get("only-in-copy").is_none());
    assert!(copy.get("only-in-original").is_none());

    Ok(())
}

#[test]
fn shared_keeps_earlier_tags_valid_when_another_holder_interns() -> Result<()> {
    // A handle held outside this crate, wrapped with a brand of its own.
    let foreign: Arc<Interner> = Arc::new(Interner::new());

    make_guard!(guard);
    let storage: DefaultStorage = Storage::shared(guard, &foreign);

    let manager = TagManager::builder()
        .parser(Plain::new())
        .storage(storage.share_as::<DefaultLabel>())
        .build();

    let tag = manager.parse_tag("hello")?;

    // The other holder interns more, directly through its own handle. An append-only
    // interner has no way to remove or reindex what's already there, so this can only
    // grow it -- which is exactly why sharing a handle stays safe.
    for i in 0..64 {
        foreign.get_or_intern(format!("foreign-{i}"));
    }

    assert_eq!(manager.resolve_tag(&tag), "hello");

    Ok(())
}

#[test]
fn unique_takes_ownership_of_a_prepopulated_interner() -> Result<()> {
    let interner = Interner::new();
    interner.get_or_intern("already-here");

    make_guard!(guard);
    let storage: DefaultStorage = Storage::unique(guard, interner);

    // Strings interned before the storage existed are still resolvable, they just
    // aren't tags: nothing branded points at them yet.
    let key = storage
        .get("already-here")
        .expect("interned before wrapping");
    assert_eq!(storage.resolve(key), "already-here");

    let manager = TagManager::builder()
        .parser(Plain::new())
        .storage(storage.share_as::<DefaultLabel>())
        .build();

    // Interning the same string through a tag dedupes onto the existing key.
    let tag = manager.parse_tag("already-here")?;
    assert_eq!(manager.resolve_tag(&tag), "already-here");
    assert_eq!(manager.storage().len(), 1);

    Ok(())
}

#[test]
fn fresh_with_capacity_still_grows_past_it() -> Result<()> {
    make_guard!(guard);
    let storage = DefaultStorage::fresh_with_capacity(guard, Capacity::for_strings(2));

    for i in 0..16 {
        storage.get_or_intern(&format!("tag-{i}"));
    }

    assert_eq!(storage.len(), 16);

    Ok(())
}

#[test]
fn try_resolve_refuses_a_key_from_nowhere() -> Result<()> {
    make_guard!(guard);
    let storage = DefaultStorage::fresh(guard);

    let key = storage.get_or_intern("hello");
    assert_eq!(storage.try_resolve(key), Some("hello"));

    // A key built by hand, rather than handed out by this interner. No `Tag` can carry
    // one past the brand check, so this is the only way to reach the `None` arm.
    let forged = Spur::try_from_usize(9_999).expect("in range for a Spur");
    assert_eq!(storage.try_resolve(forged), None);

    Ok(())
}

//---------------------------------------------------------------------------
// The hasher generic

/// A `BuildHasher` that isn't `RandomState`, to exercise the `H` parameter.
///
/// Every other test in this file runs on the default hasher, so without this the `H`
/// generic threading through `Storage`, `Tag`, `Parser`, and `TagManager` would only
/// ever be instantiated at one type.
type TestHasher = BuildHasherDefault<DefaultHasher>;

#[test]
fn a_non_default_hasher_roundtrips() -> Result<()> {
    make_guard!(guard);

    let storage: Storage<'_, DefaultLabel, Spur, TestHasher> =
        Storage::fresh_with_hasher(guard, TestHasher::default());

    let manager = TagManager::builder()
        .parser(Plain::new())
        .storage(storage)
        .build();

    test_roundtrip(&manager, "hello")
}

#[test]
fn a_non_default_hasher_works_with_capacity() -> Result<()> {
    make_guard!(guard);

    let storage: Storage<'_, DefaultLabel, Spur, TestHasher> =
        Storage::fresh_with_capacity_and_hasher(
            guard,
            Capacity::for_strings(4),
            TestHasher::default(),
        );

    let manager = TagManager::builder()
        .parser(KeyValue::new(KvPolicy::NoAmbiguousSep))
        .storage(storage)
        .build();

    test_roundtrip(&manager, "hello:world")
}

#[test]
fn a_non_default_hasher_survives_sharing_and_cloning() -> Result<()> {
    make_guard!(guard);

    let storage: Storage<'_, DefaultLabel, Spur, TestHasher> =
        Storage::fresh_with_hasher(guard, TestHasher::default());
    storage.get_or_intern("hello");

    // `share_as` and `deep_clone` both have to carry `H` through.
    let shared = storage.share_as::<DefaultLabel>();
    assert_eq!(shared.len(), 1);

    make_guard!(copy_guard);
    let copy: Storage<'_, DefaultLabel, Spur, TestHasher> = storage.deep_clone(copy_guard);
    assert!(copy.get("hello").is_some());

    Ok(())
}
