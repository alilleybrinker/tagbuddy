# `tagbuddy`

`tagbuddy` is an experimental library for annotating data with interned
lightweight tags, supporting a flexible mechanism for parsing, storing, and
querying data based on those tags.

## Example

```rust
use tagbuddy::brand::make_guard;
use tagbuddy::parse::KeyValue;
use tagbuddy::parse::KvPolicy;
use tagbuddy::TagManager;

// `make_guard!` mints the brand for a storage. See below for what it buys.
make_guard!(guard);

let manager = TagManager::new(guard, KeyValue::new(KvPolicy::NoAmbiguousSep));

let tag = manager.parse_tag("score:5").unwrap();

// Resolving returns a `String`, not a `Result`.
assert_eq!(manager.resolve_tag(&tag), "score:5");
```

A tag is a handful of interned keys, so it's cheap to copy, compare, and store.
Parsers compose: `Plain`, `KeyValue`, and `Multipart` describe the shape of a
tag, and adapters like `Trim`, `MaxChar`, `ChangeCase`, `Match`, and `Or` wrap
them to normalize or reject input before it's interned.

## Querying

A query has two levels, because "items with this tag and that tag" and "one tag
which is both of these things" are different questions. `Query` is a predicate
over an item; `Match` is a predicate over a single tag.

```rust
use tagbuddy::brand::make_guard;
use tagbuddy::parse::Plain;
use tagbuddy::query::exact;
use tagbuddy::tag::PlainTag;
use tagbuddy::{tagged, TagManager};

struct Post<'b> { title: &'static str, tags: Vec<PlainTag<'b>> }

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

Matching is by key identity, not string comparison. Query strings are looked up
with `Storage::get`, which does *not* intern — so a query can't permanently add
its own search terms to the append-only storage it is searching, and a term that
was never interned short-circuits to "matches nothing".

Besides `exact` for plain tags there are `has_key` and `key_value` for key-value
tags, `path` and `prefix` for multipart tags, and `Match::all_of`/`Match::any_of`
to combine matches within a single tag. Value constraints cover an exact value, a
set of them, a regex, and an arbitrary predicate — including `parses_to`, for
"parses into this type and then satisfies this":

```rust,ignore
key_value("score", parses_to(|n: u32| n > 3)) | has_key("featured")
```

Queries combine with `&`, `|` and `!`, or with the `all`, `any` and `not`
functions when building them programmatically.

Each match targets one shape of tag, so `Match::Exact` never matches a key-value
or multipart tag even when that tag's text is exactly the string given:
`Exact("score:5")` doesn't match the key-value tag reading `score:5`, and
`KeyValue` does. That's a consequence of matching on keys rather than text — a
plain tag is a single key, while a key-value or multipart tag has no one key
standing for its whole text.

`select` scans, taking any iterable and allocating nothing. `index` builds an
inverted index over a slice and answers from it, which is worth it from the
second query onwards. The two are required to agree, which the test suite checks
by generating queries and comparing both paths.

## A tag can't be resolved through the wrong storage

Tags are keys into an interner, and a key from one interner means something
different in another — resolving it elsewhere yields a different string rather
than an error. `tagbuddy` makes that a compile error.

Each storage carries an invariant `'brand` lifetime, minted from a guard, and
every tag copies the brand of the storage that interned it. Because the
lifetime is invariant, no two brands can ever be unified:

```rust,compile_fail
use tagbuddy::brand::make_guard;
use tagbuddy::parse::Plain;
use tagbuddy::storage::DefaultStorage;
use tagbuddy::TagManager;

make_guard!(g1);
make_guard!(g2);

let m1 = TagManager::builder()
    .parser(Plain::new())
    .storage(DefaultStorage::fresh(g1))
    .build();

let m2 = TagManager::builder()
    .parser(Plain::new())
    .storage(DefaultStorage::fresh(g2))
    .build();

let tag = m1.parse_tag("hello").unwrap();

// Does not compile: `tag` carries m1's brand.
m2.resolve_tag(&tag);
```

Two managers *can* share one interner — that's what `Storage::share_as` is
for. It keeps the brand and changes only the label, a separate and weaker
marker that keeps distinct tag vocabularies from being mixed up with each
other while they share string storage.

## Resolving can't fail

There is no `ResolveError`, and resolving returns `String` rather than
`Result`. Two properties combine to rule out the failure: the brand says a key
came from *this* interner, and the interner is append-only, so a key it handed
out stays valid for as long as it lives.

Storage is `lasso::ThreadedRodeo`, which interns and resolves through `&self`.
So there is no lock in the way either: many threads can parse and resolve
through a shared `&TagManager` at once.

The tradeoff is that memory is never reclaimed. Compaction means copying into
a fresh interner, which `Storage::deep_clone` does — and since that really is
a different interner, it takes a guard of its own, so the type system stops
stale tags from following the data across.

## Minimum supported Rust version

1.85, required by `generativity`, which uses edition 2024. Nothing in this
crate's own code needs anything that recent.

The MSRV is checked in CI. Treat a bump as a breaking change.

## License

`tagbuddy` is Apache-2.0 licensed.
