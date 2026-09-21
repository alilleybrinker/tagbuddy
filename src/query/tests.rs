//! Tests for the query system.
//!
//! The important one is [`scan_and_index_agree`]: the two execution strategies are
//! separate code, and only one of them is the definition of what a query means. Every
//! other test here pins down a specific behaviour; that one checks the strategies haven't
//! drifted apart across a spread of query shapes.

use crate::brand::make_guard;
use crate::parse::KeyValue;
use crate::parse::KvPolicy;
use crate::parse::Multipart;
use crate::parse::MultipartPolicy;
use crate::parse::Plain;
use crate::query::all;
use crate::query::any;
use crate::query::contains;
use crate::query::exact;
use crate::query::has_key;
use crate::query::key_value;
use crate::query::not;
use crate::query::parses_to;
use crate::query::path;
use crate::query::prefix;
use crate::query::Match;
use crate::query::Query;
use crate::query::Value;
use crate::storage::DefaultStorage;
use crate::storage::Spur;
use crate::tag::PlainTag;
use crate::tag::Tagged;
use crate::TagManager;
use std::slice::Iter;

/// An item carrying plain tags.
struct Item<'brand> {
    name: &'static str,
    tags: Vec<PlainTag<'brand>>,
}

impl<'brand> Tagged<PlainTag<'brand>> for Item<'brand> {
    type TagIter<'iter>
        = Iter<'iter, PlainTag<'brand>>
    where
        Self: 'iter;

    fn has_tags(&self) -> bool {
        !self.tags.is_empty()
    }

    fn get_tags(&self) -> Self::TagIter<'_> {
        self.tags.iter()
    }
}

/// Names of the matching items, scanned.
fn scanned<'b>(
    manager: &TagManager<'b, crate::label::DefaultLabel, Spur, PlainTag<'b>, Plain>,
    items: &[Item<'b>],
    query: &Query,
) -> Vec<&'static str> {
    manager
        .select(items)
        .matching(query)
        .map(|item| item.name)
        .collect()
}

/// Names of the matching items, from an index.
fn indexed<'b>(
    manager: &TagManager<'b, crate::label::DefaultLabel, Spur, PlainTag<'b>, Plain>,
    items: &[Item<'b>],
    query: &Query,
) -> Vec<&'static str> {
    manager
        .index(items)
        .matching(query)
        .map(|item| item.name)
        .collect()
}

#[test]
fn plain_queries_find_the_right_items() {
    make_guard!(guard);
    let manager = TagManager::builder()
        .parser(Plain::new())
        .storage(DefaultStorage::fresh(guard))
        .build();

    let tag = |s: &str| manager.parse_tag(s).expect("plain tags parse");

    let items = vec![
        Item {
            name: "a",
            tags: vec![tag("rust"), tag("systems")],
        },
        Item {
            name: "b",
            tags: vec![tag("rust"), tag("web")],
        },
        Item {
            name: "c",
            tags: vec![tag("go"), tag("web")],
        },
        Item {
            name: "d",
            tags: vec![],
        },
    ];

    // One tag.
    assert_eq!(scanned(&manager, &items, &exact("rust")), ["a", "b"]);

    // This tag and that tag.
    let both = all([exact("rust"), exact("web")]);
    assert_eq!(scanned(&manager, &items, &both), ["b"]);

    // This tag but not that one -- the shape from the design notes.
    let but_not = all([exact("rust"), not(exact("web"))]);
    assert_eq!(scanned(&manager, &items, &but_not), ["a"]);

    // Either tag.
    let either = any([exact("go"), exact("systems")]);
    assert_eq!(scanned(&manager, &items, &either), ["a", "c"]);

    // `Anything` includes the untagged item; `Not` of a tag does too.
    assert_eq!(
        scanned(&manager, &items, &Query::Anything),
        ["a", "b", "c", "d"]
    );
    assert_eq!(scanned(&manager, &items, &not(exact("rust"))), ["c", "d"]);
}

#[test]
fn an_uninterned_term_matches_nothing_and_is_not_interned() {
    make_guard!(guard);
    let manager = TagManager::builder()
        .parser(Plain::new())
        .storage(DefaultStorage::fresh(guard))
        .build();

    let items = vec![Item {
        name: "a",
        tags: vec![manager.parse_tag("rust").expect("parses")],
    }];

    let before = manager.storage().len();

    assert!(scanned(&manager, &items, &exact("never-seen")).is_empty());

    // The query must not have added its own search term. The interner is append-only, so
    // a query that interned would permanently grow the storage it searched.
    assert_eq!(manager.storage().len(), before);
    assert!(manager.storage().get("never-seen").is_none());

    // And it inverts correctly rather than short-circuiting the whole query.
    assert_eq!(scanned(&manager, &items, &not(exact("never-seen"))), ["a"]);
}

#[test]
fn key_value_queries_use_keys_and_values() {
    make_guard!(guard);
    let manager = TagManager::builder()
        .parser(KeyValue::new(KvPolicy::NoAmbiguousSep))
        .storage(DefaultStorage::fresh(guard))
        .build();

    let tag = |s: &str| manager.parse_tag(s).expect("key-value tags parse");

    struct KvItem<'brand> {
        name: &'static str,
        tags: Vec<crate::tag::KeyValueTag<'brand>>,
    }

    impl<'brand> Tagged<crate::tag::KeyValueTag<'brand>> for KvItem<'brand> {
        type TagIter<'iter>
            = Iter<'iter, crate::tag::KeyValueTag<'brand>>
        where
            Self: 'iter;

        fn has_tags(&self) -> bool {
            !self.tags.is_empty()
        }

        fn get_tags(&self) -> Self::TagIter<'_> {
            self.tags.iter()
        }
    }

    let items = vec![
        KvItem {
            name: "a",
            tags: vec![tag("score:5"), tag("lang:rust")],
        },
        KvItem {
            name: "b",
            tags: vec![tag("score:2")],
        },
        KvItem {
            name: "c",
            tags: vec![tag("lang:go")],
        },
    ];

    let names = |q: &Query| -> Vec<&'static str> {
        manager.select(&items).matching(q).map(|i| i.name).collect()
    };

    // Key presence.
    assert_eq!(names(&has_key("score")), ["a", "b"]);

    // Exact value.
    let score_is_5 = key_value("score", Value::is("5"));
    assert_eq!(names(&score_is_5), ["a"]);

    // Set of values.
    let lang_either = key_value("lang", Value::one_of(["rust", "go"]));
    assert_eq!(names(&lang_either), ["a", "c"]);

    // Parsed value with a constraint on the parsed type -- the case a closed set of
    // variants can't express.
    let high_score = key_value("score", parses_to(|n: u32| n > 3));
    assert_eq!(names(&high_score), ["a"]);

    // Index and scan agree on all of the above, including the opaque predicate.
    for query in [&score_is_5, &lang_either, &high_score] {
        let from_index: Vec<_> = manager
            .index(&items)
            .matching(query)
            .map(|i| i.name)
            .collect();
        assert_eq!(from_index, names(query), "index disagreed with scan");
    }
}

#[test]
fn multipart_queries_match_paths_and_prefixes() {
    make_guard!(guard);
    let manager = TagManager::builder()
        .parser(Multipart::new(MultipartPolicy::RequireMultipart))
        .storage(DefaultStorage::fresh(guard))
        .build();

    let tag = |s: &str| manager.parse_tag(s).expect("multipart tags parse");

    struct PathItem<'brand> {
        name: &'static str,
        tags: Vec<crate::tag::MultipartTag<'brand>>,
    }

    impl<'brand> Tagged<crate::tag::MultipartTag<'brand>> for PathItem<'brand> {
        type TagIter<'iter>
            = Iter<'iter, crate::tag::MultipartTag<'brand>>
        where
            Self: 'iter;

        fn has_tags(&self) -> bool {
            !self.tags.is_empty()
        }

        fn get_tags(&self) -> Self::TagIter<'_> {
            self.tags.iter()
        }
    }

    let items = vec![
        PathItem {
            name: "a",
            tags: vec![tag("lotr/legolas/friends")],
        },
        PathItem {
            name: "b",
            tags: vec![tag("lotr/gimli")],
        },
        PathItem {
            name: "c",
            tags: vec![tag("dune/paul")],
        },
    ];

    let names = |q: &Query| -> Vec<&'static str> {
        manager.select(&items).matching(q).map(|i| i.name).collect()
    };

    // Prefix matches everything beneath it.
    assert_eq!(names(&prefix(["lotr"])), ["a", "b"]);

    // A full path matches only the tag that is exactly it.
    assert_eq!(names(&path(["lotr", "gimli"])), ["b"]);

    // A prefix that is also a full path doesn't match the shorter tag's parent.
    assert_eq!(names(&path(["lotr"])), [] as [&str; 0]);

    // An empty prefix matches every multipart tag.
    assert_eq!(names(&prefix(Vec::<&str>::new())), ["a", "b", "c"]);

    for query in [
        prefix(["lotr"]),
        path(["lotr", "gimli"]),
        prefix(Vec::<&str>::new()),
    ] {
        let from_index: Vec<_> = manager
            .index(&items)
            .matching(&query)
            .map(|i| i.name)
            .collect();
        assert_eq!(from_index, names(&query), "index disagreed with scan");
    }
}

#[test]
fn match_all_needs_one_tag_satisfying_everything() {
    make_guard!(guard);
    let manager = TagManager::builder()
        .parser(Plain::new())
        .storage(DefaultStorage::fresh(guard))
        .build();

    let tag = |s: &str| manager.parse_tag(s).expect("parses");

    let items = vec![Item {
        name: "a",
        tags: vec![tag("rust"), tag("web")],
    }];

    // Two exact matches on one tag is unsatisfiable: no single tag is both. This is the
    // distinction the two-level algebra exists to make -- at the `Query` level the same
    // pair is the ordinary "has both tags".
    let one_tag_both = contains(Match::All(vec![
        Match::Exact("rust".to_owned()),
        Match::Exact("web".to_owned()),
    ]));
    assert_eq!(scanned(&manager, &items, &one_tag_both), [] as [&str; 0]);
    assert_eq!(indexed(&manager, &items, &one_tag_both), [] as [&str; 0]);

    let two_tags = all([exact("rust"), exact("web")]);
    assert_eq!(scanned(&manager, &items, &two_tags), ["a"]);
    assert_eq!(indexed(&manager, &items, &two_tags), ["a"]);

    // An empty `Match::All` is satisfied by any tag, so it selects items that have one.
    let has_any_tag = contains(Match::All(vec![]));
    assert_eq!(scanned(&manager, &items, &has_any_tag), ["a"]);
    assert_eq!(indexed(&manager, &items, &has_any_tag), ["a"]);
}

#[test]
fn empty_combinators_follow_the_usual_conventions() {
    make_guard!(guard);
    let manager = TagManager::builder()
        .parser(Plain::new())
        .storage(DefaultStorage::fresh(guard))
        .build();

    let items = vec![
        Item {
            name: "a",
            tags: vec![manager.parse_tag("rust").expect("parses")],
        },
        Item {
            name: "b",
            tags: vec![],
        },
    ];

    // Empty conjunction is true, empty disjunction is false.
    assert_eq!(scanned(&manager, &items, &all([])), ["a", "b"]);
    assert_eq!(indexed(&manager, &items, &all([])), ["a", "b"]);
    assert_eq!(scanned(&manager, &items, &any([])), [] as [&str; 0]);
    assert_eq!(indexed(&manager, &items, &any([])), [] as [&str; 0]);
}

#[test]
fn an_index_answers_many_queries() {
    make_guard!(guard);
    let manager = TagManager::builder()
        .parser(Plain::new())
        .storage(DefaultStorage::fresh(guard))
        .build();

    let tag = |s: &str| manager.parse_tag(s).expect("parses");

    let items = vec![
        Item {
            name: "a",
            tags: vec![tag("rust")],
        },
        Item {
            name: "b",
            tags: vec![tag("go")],
        },
    ];

    // The point of the index: build once, query repeatedly.
    let index = manager.index(&items);

    assert_eq!(
        index
            .matching(&exact("rust"))
            .map(|i| i.name)
            .collect::<Vec<_>>(),
        ["a"]
    );
    assert_eq!(
        index
            .matching(&exact("go"))
            .map(|i| i.name)
            .collect::<Vec<_>>(),
        ["b"]
    );
    assert_eq!(index.items().len(), 2);
}

#[test]
fn scan_and_index_agree() {
    make_guard!(guard);
    let manager = TagManager::builder()
        .parser(Plain::new())
        .storage(DefaultStorage::fresh(guard))
        .build();

    let tag = |s: &str| manager.parse_tag(s).expect("parses");

    // A spread of overlapping tag sets, including an untagged item, so that `Not` and the
    // empty-conjunction cases have something to be wrong about.
    let vocabulary = ["rust", "go", "web", "systems", "cli"];
    let mut items = Vec::new();

    for i in 0..32u32 {
        let tags = vocabulary
            .iter()
            .enumerate()
            .filter(|(bit, _)| i & (1 << bit) != 0)
            .map(|(_, word)| tag(word))
            .collect();

        items.push(Item { name: "item", tags });
    }

    // Built from the vocabulary plus one term that was never interned, so resolution's
    // `Never` path is exercised too.
    let leaves: Vec<Query> = vocabulary
        .iter()
        .chain(["never-interned"].iter())
        .map(|w| exact(*w))
        .collect();

    let mut queries = vec![Query::Anything, all([]), any([])];

    for a in &leaves {
        queries.push(a.clone());
        queries.push(not(a.clone()));

        for b in &leaves {
            queries.push(all([a.clone(), b.clone()]));
            queries.push(any([a.clone(), b.clone()]));
            queries.push(all([a.clone(), not(b.clone())]));
            queries.push(not(any([a.clone(), b.clone()])));
            queries.push(contains(Match::any_of([
                Match::exact(leaf_text(a)),
                Match::exact(leaf_text(b)),
            ])));
            queries.push(contains(Match::all_of([
                Match::exact(leaf_text(a)),
                Match::exact(leaf_text(b)),
            ])));
        }
    }

    // Positions rather than names, since every item is called the same thing.
    let positions = |query: &Query, from_index: bool| -> Vec<usize> {
        let matched: Vec<*const Item<'_>> = if from_index {
            manager
                .index(&items)
                .matching(query)
                .map(|i| i as *const _)
                .collect()
        } else {
            manager
                .select(&items)
                .matching(query)
                .map(|i| i as *const _)
                .collect()
        };

        matched
            .into_iter()
            .map(|ptr| {
                items
                    .iter()
                    .position(|i| std::ptr::eq(i, ptr))
                    .expect("result came from the input")
            })
            .collect()
    };

    for query in &queries {
        assert_eq!(
            positions(query, false),
            positions(query, true),
            "scan and index disagreed on {query:?}"
        );
    }
}

/// Pull the string back out of a single-`Exact` query, for building nested matches.
fn leaf_text(q: &Query) -> String {
    match q {
        Query::Contains(Match::Exact(s)) => s.clone(),
        _ => unreachable!("only called on exact leaves"),
    }
}

#[test]
#[cfg(feature = "either")]
fn exact_only_matches_plain_tags() {
    use crate::parse::Or;
    use crate::tag::KeyValueTag;
    use crate::tag::MultipartTag;
    use either::Either;

    make_guard!(guard);
    let manager = TagManager::builder()
        .parser(Or(
            Multipart::new(MultipartPolicy::RequireMultipart),
            Or(KeyValue::new(KvPolicy::NoAmbiguousSep), Plain::new()),
        ))
        .storage(DefaultStorage::fresh(guard))
        .build();

    type MixedTag<'brand> =
        Either<MultipartTag<'brand>, Either<KeyValueTag<'brand>, PlainTag<'brand>>>;

    struct Mixed<'brand> {
        name: &'static str,
        tags: Vec<MixedTag<'brand>>,
    }

    impl<'brand> Tagged<MixedTag<'brand>> for Mixed<'brand> {
        type TagIter<'iter>
            = Iter<'iter, MixedTag<'brand>>
        where
            Self: 'iter;

        fn has_tags(&self) -> bool {
            !self.tags.is_empty()
        }

        fn get_tags(&self) -> Self::TagIter<'_> {
            self.tags.iter()
        }
    }

    let tag = |s: &str| {
        manager
            .parse_tag(s)
            .expect("parses under some branch of the Or")
    };

    // All three tags intern the very same key for "score": the plain tag's whole content,
    // the key-value tag's key half, and the multipart tag's first part. So if matching
    // discriminated on keys alone rather than on tag shape, these would be confusable.
    let items = vec![
        Mixed {
            name: "plain",
            tags: vec![tag("score")],
        },
        Mixed {
            name: "key-value",
            tags: vec![tag("score:5")],
        },
        Mixed {
            name: "multipart",
            tags: vec![tag("score/high")],
        },
    ];

    let names = |q: &Query| -> Vec<&'static str> {
        manager.select(&items).matching(q).map(|i| i.name).collect()
    };

    // Each matcher picks out its own tag shape, and only that one.
    assert_eq!(
        names(&contains(Match::Exact("score".to_owned()))),
        ["plain"]
    );
    assert_eq!(
        names(&contains(Match::HasKey("score".to_owned()))),
        ["key-value"]
    );
    assert_eq!(
        names(&contains(Match::Prefix(vec!["score".to_owned()]))),
        ["multipart"]
    );

    // `Exact` is about plain tags, so it never matches a key-value or multipart tag, even
    // when the tag's *text* is exactly the string asked for.
    assert_eq!(manager.resolve_tag(&items[1].tags[0]), "score:5");
    assert_eq!(manager.resolve_tag(&items[2].tags[0]), "score/high");
    assert_eq!(
        names(&contains(Match::Exact("score:5".to_owned()))),
        [] as [&str; 0]
    );
    assert_eq!(
        names(&contains(Match::Exact("score/high".to_owned()))),
        [] as [&str; 0]
    );

    for query in [
        contains(Match::Exact("score".to_owned())),
        contains(Match::Exact("score:5".to_owned())),
        contains(Match::HasKey("score".to_owned())),
    ] {
        let from_index: Vec<_> = manager
            .index(&items)
            .matching(&query)
            .map(|i| i.name)
            .collect();
        assert_eq!(from_index, names(&query), "index disagreed with scan");
    }
}

#[test]
fn operators_build_the_same_queries_as_the_combinators() {
    make_guard!(guard);
    let manager = TagManager::builder()
        .parser(Plain::new())
        .storage(DefaultStorage::fresh(guard))
        .build();

    let tag = |s: &str| manager.parse_tag(s).expect("parses");

    let items = vec![
        Item {
            name: "a",
            tags: vec![tag("rust"), tag("systems")],
        },
        Item {
            name: "b",
            tags: vec![tag("rust"), tag("web")],
        },
        Item {
            name: "c",
            tags: vec![tag("go")],
        },
    ];

    // `rust` but not `web`, written both ways.
    let with_ops = exact("rust") & !exact("web");
    let with_fns = all([exact("rust"), not(exact("web"))]);
    assert_eq!(scanned(&manager, &items, &with_ops), ["a"]);
    assert_eq!(
        scanned(&manager, &items, &with_ops),
        scanned(&manager, &items, &with_fns)
    );

    let either = exact("go") | exact("systems");
    assert_eq!(scanned(&manager, &items, &either), ["a", "c"]);

    // Chains flatten rather than nesting, so the tree stays shallow.
    let three = exact("a") & exact("b") & exact("c");
    assert!(matches!(three, Query::All(ref qs) if qs.len() == 3));

    let three_or = exact("a") | exact("b") | exact("c");
    assert!(matches!(three_or, Query::Any(ref qs) if qs.len() == 3));

    // Mixing still nests where it has to: `&` inside `|` stays a distinct node.
    let mixed = exact("rust") & (exact("web") | exact("systems"));
    assert_eq!(scanned(&manager, &items, &mixed), ["a", "b"]);
    assert_eq!(indexed(&manager, &items, &mixed), ["a", "b"]);
}
