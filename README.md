# `tagbuddy`

Annotate your data with tags — plain words, `key:value` pairs, or
`path/like/hierarchies` — and query it by them, in memory, without a database.

```rust
use tagbuddy::brand::make_guard;
use tagbuddy::parse::{KeyValue, KvPolicy};
use tagbuddy::TagManager;

make_guard!(guard);
let manager = TagManager::new(guard, KeyValue::new(KvPolicy::NoAmbiguousSep));

let tag = manager.parse_tag("score:5").unwrap();

assert_eq!(manager.resolve_tag(&tag), "score:5");
```

## Why

- **Tags are nearly free to keep around.** Each distinct string is stored once,
  however many records carry it, and a tag itself is a handful of integers.
- **Using a tag with the wrong store is a compile error.** Not a wrong answer at
  runtime — the program doesn't build.
- **Reading a tag back never fails.** No `Result`, because there is no failure
  case to handle.
- **Query collections you already have.** Boolean queries over your own `Vec`,
  with no schema, no migration, and no index server.
- **Read from as many threads as you like.** Nothing sits between a thread and
  a tag.
- **Say how input is cleaned up once,** where you declare the parser, instead of
  at every site that makes a tag.

The rest of this file shows each of those.

## Tags are nearly free to keep around

A tag is an index into a string table, not a string. So it's `Copy`, comparing
two is comparing integers, and putting one on every record costs almost nothing:

```rust
use std::mem::size_of;
use tagbuddy::tag::{KeyValueTag, PlainTag};

assert_eq!(size_of::<PlainTag>(), 4);
assert_eq!(size_of::<KeyValueTag>(), 8);

// And there's a spare niche, so wrapping one costs nothing either.
assert_eq!(size_of::<Option<PlainTag>>(), 4);
```

The text is stored once no matter how many records use it, which is the point
when a thousand posts are all tagged `rust`:

```rust
use tagbuddy::brand::make_guard;
use tagbuddy::parse::Plain;
use tagbuddy::TagManager;

make_guard!(guard);
let manager = TagManager::new(guard, Plain::new());

let tags: Vec<_> = (0..1_000).map(|_| manager.parse_tag("rust").unwrap()).collect();

assert_eq!(tags.len(), 1_000);
assert_eq!(manager.storage().len(), 1); // one copy of "rust"
```

## Using a tag with the wrong store is a compile error

Tags are indices, so a tag from one store means something else entirely in
another — it would quietly resolve to a *different string* rather than
complain. That bug can't be written here:

```rust,compile_fail
use tagbuddy::brand::make_guard;
use tagbuddy::parse::Plain;
use tagbuddy::TagManager;

make_guard!(g1);
make_guard!(g2);

let posts = TagManager::new(g1, Plain::new());
let users = TagManager::new(g2, Plain::new());

let tag = posts.parse_tag("rust").unwrap();

users.resolve_tag(&tag); // does not compile
```

Two managers can deliberately share one store when you want them to — see
`Storage::share_as` — and then their tags are interchangeable by construction.

## Reading a tag back never fails

`resolve_tag` hands you a `String`. There's no `ResolveError` in the crate,
because a tag that exists can always be read:

```rust
# use tagbuddy::brand::make_guard;
# use tagbuddy::parse::Plain;
# use tagbuddy::TagManager;
make_guard!(guard);
let manager = TagManager::new(guard, Plain::new());

let tag = manager.parse_tag("rust").unwrap();

let text: String = manager.resolve_tag(&tag); // no `?`, no `unwrap`
assert_eq!(text, "rust");
```

Parsing still fails, of course — that's where bad input gets rejected.

## Query collections you already have

Point it at your own `Vec` and ask questions. Nothing is copied; you get
references back into the collection you passed in:

```rust
use tagbuddy::brand::make_guard;
use tagbuddy::parse::Plain;
use tagbuddy::query::exact;
use tagbuddy::tag::PlainTag;
use tagbuddy::{tagged, TagManager};

struct Post<'b> { title: &'static str, tags: Vec<PlainTag<'b>> }

// One line to say where this type keeps its tags.
tagged!(Post<'b> => PlainTag<'b> { tags });

make_guard!(guard);
let manager = TagManager::new(guard, Plain::new());
let tag = |s: &str| manager.parse_tag(s).unwrap();

let posts = vec![
    Post { title: "one", tags: vec![tag("rust"), tag("web")] },
    Post { title: "two", tags: vec![tag("rust")] },
];

// Tagged `rust` but not `web`.
let found: Vec<_> = manager
    .select(&posts)
    .matching(&(exact("rust") & !exact("web")))
    .map(|post| post.title)
    .collect();

assert_eq!(found, ["two"]);
```

Queries combine with `&`, `|` and `!`, or with `all`, `any` and `not` when
you're building them up programmatically.

Ask the same collection many questions and `index` pays for itself from the
second query onwards — same queries, same answers, an inverted index instead of
a scan:

```rust
# use tagbuddy::brand::make_guard;
# use tagbuddy::parse::Plain;
# use tagbuddy::query::exact;
# use tagbuddy::tag::PlainTag;
# use tagbuddy::{tagged, TagManager};
# struct Post<'b> { title: &'static str, tags: Vec<PlainTag<'b>> }
# tagged!(Post<'b> => PlainTag<'b> { tags });
# make_guard!(guard);
# let manager = TagManager::new(guard, Plain::new());
# let tag = |s: &str| manager.parse_tag(s).unwrap();
# let posts = vec![
#     Post { title: "one", tags: vec![tag("rust"), tag("web")] },
#     Post { title: "two", tags: vec![tag("rust")] },
# ];
let index = manager.index(&posts);

assert_eq!(index.matching(&exact("rust")).count(), 2);
assert_eq!(index.matching(&exact("web")).count(), 1);
```

### What you can ask for

| | |
|---|---|
| `exact("rust")` | a plain tag |
| `has_key("score")` | a `key:value` tag with this key |
| `key_value("score", Value::is("5"))` | …and this exact value |
| `key_value("score", parses_to(\|n: u32\| n > 3))` | …and a value that parses and passes |
| `prefix(["lotr"])` | a `path/tag` starting here |
| `path(["lotr", "gimli"])` | a `path/tag` that is exactly this |

Values can also be one of a set, or match a regex. `Match::all_of` and
`Match::any_of` combine conditions that must hold of *one* tag, as against `&`
and `|`, which combine conditions on the *item*.

## Read from as many threads as you like

There's no lock between a thread and a tag, so a shared `&TagManager` works from
as many threads as you have:

```rust
use tagbuddy::brand::make_guard;
use tagbuddy::parse::Plain;
use tagbuddy::TagManager;

make_guard!(guard);
let manager = TagManager::new(guard, Plain::new());

std::thread::scope(|scope| {
    for _ in 0..8 {
        let manager = &manager;
        scope.spawn(move || {
            for i in 0..100 {
                let raw = format!("tag-{}", i % 10);
                let tag = manager.parse_tag(&raw).unwrap();
                assert_eq!(manager.resolve_tag(&tag), raw);
            }
        });
    }
});

assert_eq!(manager.storage().len(), 10);
```

## Say how input is cleaned up once

Parsers describe the shape of a tag — `Plain`, `KeyValue`, `Multipart` — and
adapters wrap them to normalize or reject input before it's ever stored. Declare
it once, and every tag that manager makes has been through it:

```rust
use tagbuddy::brand::make_guard;
use tagbuddy::parse::{MaxChar, Plain, Trim, TrimBounds};
use tagbuddy::TagManager;

make_guard!(guard);
let manager = TagManager::new(
    guard,
    Trim(TrimBounds::Both, MaxChar(20, Plain::new())),
);

// Whitespace is trimmed before interning, so this is the same tag as "rust".
let tag = manager.parse_tag("   rust   ").unwrap();
assert_eq!(manager.resolve_tag(&tag), "rust");

// And over-long input is rejected rather than stored.
assert!(manager.parse_tag(&"x".repeat(50)).is_err());
```

`ChangeCase` normalizes case, `Match` and `Replace` apply regexes, and `Or`
falls back from one parser to another — each behind the feature of the same
name.

## Tradeoffs

- **Memory is never reclaimed.** Storage only ever grows. That suits tag
  vocabularies, which repeat and plateau, but a store fed unbounded distinct
  strings will grow without limit. Compacting means copying into a fresh store
  with `Storage::deep_clone`, which gives you a genuinely separate one — so old
  tags can't follow the data across, and the compiler says so.
- **Each match targets one shape of tag.** `exact("score:5")` will not match a
  `key:value` tag reading `score:5`; `key_value` will. Matching compares indices
  rather than text, and a `key:value` tag has no single index standing for its
  whole text.
- **It's experimental.** The API is still moving.

## How it works

Skip this unless you're curious. Tags are keys into a [`lasso::ThreadedRodeo`],
which interns and resolves through `&self` — hence no lock. It's append-only by
construction, with no `clear` and no `&mut` API at all, so a key stays valid for
as long as the store lives; that, plus the brand below, is why reading back
can't fail.

Each store carries an invariant lifetime minted by `make_guard!`, and every tag
copies the brand of the store that made it. Invariant lifetimes never unify, so
crossing them can't typecheck. A separate, weaker `Label` marker distinguishes
tag vocabularies that deliberately share one store.

[`lasso::ThreadedRodeo`]: https://docs.rs/lasso/latest/lasso/struct.ThreadedRodeo.html

## Minimum supported Rust version

1.85, required by `generativity`, which uses edition 2024. Nothing in this
crate's own code needs anything that recent.

The MSRV is checked in CI. Treat a bump as a breaking change.

## License

`tagbuddy` is Apache-2.0 licensed.
