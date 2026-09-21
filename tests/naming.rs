//! Naming `Index` and `Scan` in positions where inference can't help.
//!
//! Call sites infer everything, so the parameter count only bites in a struct field or a
//! return type. That's what this pins: `Index` takes two lifetimes and two types, and if
//! it ever grows back toward naming the manager's parameters individually, this stops
//! compiling.

use std::slice::Iter;
use tagbuddy::brand::make_guard;
use tagbuddy::label::DefaultLabel;
use tagbuddy::parse::Plain;
use tagbuddy::query::{contains, Index, Match, Scan};
use tagbuddy::storage::{DefaultStorage, Spur};
use tagbuddy::tag::{PlainTag, Tagged};
use tagbuddy::TagManager;

struct Post<'brand> {
    title: &'static str,
    tags: Vec<PlainTag<'brand>>,
}

// No `'brand` on the trait: the tag type carries it.
impl<'brand> Tagged<PlainTag<'brand>> for Post<'brand> {
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

type Manager<'brand> = TagManager<'brand, DefaultLabel, Spur, PlainTag<'brand>, Plain>;

/// Holding an index in a struct: the manager type stands in for its five parameters.
struct Search<'m, 'items, 'brand> {
    index: Index<'m, 'items, Manager<'brand>, Post<'brand>>,
}

/// Returning one from a function.
fn build<'m, 'items, 'brand>(
    manager: &'m Manager<'brand>,
    posts: &'items [Post<'brand>],
) -> Index<'m, 'items, Manager<'brand>, Post<'brand>> {
    manager.index(posts)
}

/// And the scanning half, which needs one lifetime and two types.
fn select<'m, 'items, 'brand>(
    manager: &'m Manager<'brand>,
    posts: &'items [Post<'brand>],
) -> Scan<'m, Manager<'brand>, &'items [Post<'brand>]> {
    manager.select(posts)
}

#[test]
fn index_and_scan_are_nameable() {
    make_guard!(guard);
    let manager: Manager = TagManager::builder()
        .parser(Plain::new())
        .storage(DefaultStorage::fresh(guard))
        .build();

    let tag = |s: &str| manager.parse_tag(s).expect("parses");

    let posts = vec![
        Post {
            title: "one",
            tags: vec![tag("rust")],
        },
        Post {
            title: "two",
            tags: vec![tag("go")],
        },
    ];

    let rust = contains(Match::Exact("rust".to_owned()));

    let search = Search {
        index: build(&manager, &posts),
    };
    let found: Vec<_> = search.index.matching(&rust).map(|p| p.title).collect();
    assert_eq!(found, ["one"]);

    let scanned: Vec<_> = select(&manager, &posts)
        .matching(&rust)
        .map(|p| p.title)
        .collect();
    assert_eq!(scanned, ["one"]);
}
