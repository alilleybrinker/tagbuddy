//! Querying collections of [`Tagged`] items by their tags.
//!
//! A query has two levels, because "items with this tag and that tag" and "a tag which is
//! both of these things" are different questions. [`Query`] is a predicate over an *item*;
//! [`Match`] is a predicate over a single *tag*. So "has `a` and `b` but not `c`" is three
//! [`Query::Contains`] nodes combined at the item level, while "has key `score` whose value
//! parses to a number over 3" is one [`Query::Contains`] wrapping a single [`Match`].
//!
//! # Matching is by key identity
//!
//! Tags are interned keys, and so are the strings in a query: before evaluating, every
//! string is looked up with [`Storage::get`], which does *not* intern. That matters twice
//! over. Comparisons become integer equality rather than string equality, and — since the
//! interner is append-only — a query can't permanently add its own search terms to the
//! storage it's searching. A string that was never interned can't be on any tag, so it
//! matches nothing, and the surrounding query short-circuits.
//!
//! # Matches are per tag shape
//!
//! Because matching is by identity, each constructor targets one shape of tag:
//! [`Match::Exact`] is about [`PlainTag`]s, [`Match::HasKey`] and [`Match::KeyValue`] about
//! [`KeyValueTag`]s, and [`Match::Path`] and [`Match::Prefix`] about [`MultipartTag`]s. A
//! manager whose parser produces a mix (via the `Or` adapter) can be queried with all of
//! them, and each still only matches its own shape.
//!
//! The consequence worth knowing is that **[`Match::Exact`] never matches a key-value or
//! multipart tag, even when that tag's text is exactly the string you asked for.**
//! `Match::Exact("score:5")` does not match the key-value tag that resolves to `"score:5"`;
//! [`Match::KeyValue`] does. `Match::Exact("a/b")` does not match the multipart tag that
//! resolves to `"a/b"`; [`Match::Path`] does.
//!
//! That falls out of matching on keys rather than text. A plain tag is one key, so asking
//! whether it equals a string is one integer comparison. A key-value or multipart tag has
//! no single key standing for its whole text — the text only exists once the parts are
//! resolved and joined with the manager's separators — so an `Exact` over them would have
//! to resolve and compare strings, which is the cost the whole design avoids. Naming the
//! shape you mean is also more precise: with a shared interner, `"score"` can be a plain
//! tag, the key half of `score:5`, and the first part of `score/high` all at once, and
//! those are the same key. [`Match::Exact`], [`Match::HasKey`] and [`Match::Prefix`] tell
//! them apart by shape.
//!
//! # Two ways to run one
//!
//! [`TagManager::select`] scans: it walks the items, evaluating the query against each. It
//! takes any iterator, allocates nothing, and costs one pass per query.
//!
//! [`TagManager::index`] builds an inverted index over a slice of items first, and answers
//! from it. Building costs about what one scan costs, so it only pays off when the same
//! items are queried more than once — but then each query is close to the size of its own
//! answer rather than the size of the collection.
//!
//! Both are required to agree, which `query::tests` checks by generating queries and
//! asserting the two paths return the same items.
//!
//! [`PlainTag`]: crate::tag::PlainTag
//! [`KeyValueTag`]: crate::tag::KeyValueTag
//! [`MultipartTag`]: crate::tag::MultipartTag
//! [`Storage::get`]: crate::storage::Storage::get
//! [`TagManager::select`]: crate::TagManager::select
//! [`TagManager::index`]: crate::TagManager::index

mod index;
mod resolve;
mod scan;
#[cfg(test)]
mod tests;

pub use crate::query::index::Index;
pub(crate) use crate::query::resolve::Resolved;
pub(crate) use crate::query::resolve::ResolvedMatch;
pub(crate) use crate::query::resolve::ResolvedValue;
pub use crate::query::scan::Scan;

#[cfg(doc)]
use crate::tag::Tagged;
use std::fmt::Debug;
use std::fmt::Formatter;
use std::fmt::Result as FmtResult;
use std::ops::BitAnd;
use std::ops::BitOr;
use std::ops::Not as NotOp;
use std::sync::Arc;

/// A predicate over a tagged *item*.
///
/// Build these with the free functions in this module — [`contains`], [`all`], [`any`],
/// [`not`] — or construct the variants directly.
#[derive(Debug, Clone)]
#[non_exhaustive]
pub enum Query {
    /// Matches every item, including items with no tags.
    Anything,

    /// Matches an item carrying at least one tag satisfying the [`Match`].
    Contains(Match),

    /// Matches an item satisfying every one of these.
    ///
    /// An empty `All` matches everything, as the empty conjunction.
    All(Vec<Query>),

    /// Matches an item satisfying at least one of these.
    ///
    /// An empty `Any` matches nothing, as the empty disjunction.
    Any(Vec<Query>),

    /// Matches an item that does *not* satisfy the inner query.
    Not(Box<Query>),
}

/// A predicate over a single *tag*.
#[derive(Debug, Clone)]
#[non_exhaustive]
pub enum Match {
    /// A [`PlainTag`] whose whole content is this string.
    ///
    /// Only plain tags. This does *not* match a [`KeyValueTag`] or [`MultipartTag`] whose
    /// text happens to equal the string: use [`Match::KeyValue`] or [`Match::Path`] for
    /// those. See the module documentation for why.
    ///
    /// [`PlainTag`]: crate::tag::PlainTag
    /// [`KeyValueTag`]: crate::tag::KeyValueTag
    /// [`MultipartTag`]: crate::tag::MultipartTag
    Exact(String),

    /// A [`KeyValueTag`] with this key, whatever its value.
    ///
    /// [`KeyValueTag`]: crate::tag::KeyValueTag
    HasKey(String),

    /// A [`KeyValueTag`] with this key, whose value satisfies the [`Value`].
    ///
    /// [`KeyValueTag`]: crate::tag::KeyValueTag
    KeyValue {
        /// The key half to match.
        key: String,
        /// The constraint on the value half.
        value: Value,
    },

    /// A [`MultipartTag`] whose parts are exactly these.
    ///
    /// This is the multipart equivalent of [`Match::Exact`], which doesn't match multipart
    /// tags. Note the parts are given separately rather than as one separator-joined
    /// string, because each part is interned on its own.
    ///
    /// [`MultipartTag`]: crate::tag::MultipartTag
    Path(Vec<String>),

    /// A [`MultipartTag`] whose leading parts are these.
    ///
    /// An empty prefix matches every multipart tag.
    ///
    /// [`MultipartTag`]: crate::tag::MultipartTag
    Prefix(Vec<String>),

    /// One tag satisfying every one of these.
    ///
    /// Note this is about a *single* tag satisfying all of them, which is why it's here
    /// and not at the [`Query`] level. `Match::All` of two [`Match::Exact`]es is
    /// unsatisfiable; [`Query::All`] of two [`Query::Contains`]es is the usual "has both
    /// of these tags".
    All(Vec<Match>),

    /// One tag satisfying at least one of these.
    Any(Vec<Match>),
}

impl Match {
    /// A [`PlainTag`] whose whole content is this string.
    ///
    /// [`PlainTag`]: crate::tag::PlainTag
    pub fn exact(s: impl Into<String>) -> Match {
        Match::Exact(s.into())
    }

    /// A [`KeyValueTag`] with this key, whatever its value.
    ///
    /// [`KeyValueTag`]: crate::tag::KeyValueTag
    pub fn has_key(key: impl Into<String>) -> Match {
        Match::HasKey(key.into())
    }

    /// A [`KeyValueTag`] with this key, whose value satisfies `value`.
    ///
    /// [`KeyValueTag`]: crate::tag::KeyValueTag
    pub fn key_value(key: impl Into<String>, value: Value) -> Match {
        Match::KeyValue {
            key: key.into(),
            value,
        }
    }

    /// A [`MultipartTag`] whose parts are exactly these.
    ///
    /// [`MultipartTag`]: crate::tag::MultipartTag
    pub fn path(parts: impl IntoIterator<Item = impl Into<String>>) -> Match {
        Match::Path(parts.into_iter().map(Into::into).collect())
    }

    /// A [`MultipartTag`] whose leading parts are these.
    ///
    /// [`MultipartTag`]: crate::tag::MultipartTag
    pub fn prefix(parts: impl IntoIterator<Item = impl Into<String>>) -> Match {
        Match::Prefix(parts.into_iter().map(Into::into).collect())
    }

    /// One tag satisfying every one of these.
    pub fn all_of(matches: impl IntoIterator<Item = Match>) -> Match {
        Match::All(matches.into_iter().collect())
    }

    /// One tag satisfying at least one of these.
    pub fn any_of(matches: impl IntoIterator<Item = Match>) -> Match {
        Match::Any(matches.into_iter().collect())
    }
}

impl Value {
    /// The value is exactly this string.
    pub fn is(value: impl Into<String>) -> Value {
        Value::Is(value.into())
    }

    /// The value is one of these strings.
    pub fn one_of(values: impl IntoIterator<Item = impl Into<String>>) -> Value {
        Value::OneOf(values.into_iter().map(Into::into).collect())
    }
}

/// A constraint on the value half of a key-value tag.
#[derive(Clone)]
#[non_exhaustive]
pub enum Value {
    /// The value is exactly this string.
    Is(String),

    /// The value is one of these strings.
    OneOf(Vec<String>),

    /// The value matches this regular expression.
    #[cfg(feature = "regex")]
    Matches(regex::Regex),

    /// The value satisfies an arbitrary predicate.
    ///
    /// This is the escape hatch for constraints the other variants can't express — most
    /// usefully "parses into some type, and then satisfies something", which no closed set
    /// of variants can cover for arbitrary types. See [`parses_to`].
    ///
    /// The cost is that the engine can't see into it: [`Value::Is`] and [`Value::OneOf`]
    /// are answered straight from the index, while a predicate has to be run against each
    /// candidate the rest of the query narrows down to.
    Passes(Predicate),
}

/// An arbitrary predicate on a tag value's string.
///
/// Held in an [`Arc`] so that a [`Query`] stays [`Clone`], which matters when one query is
/// run against several [`Index`]es.
#[derive(Clone)]
pub struct Predicate(Arc<dyn Fn(&str) -> bool + Send + Sync>);

impl Predicate {
    /// Wrap a closure as a [`Predicate`].
    pub fn new(f: impl Fn(&str) -> bool + Send + Sync + 'static) -> Self {
        Predicate(Arc::new(f))
    }

    /// Test a value's string against the predicate.
    pub(crate) fn test(&self, value: &str) -> bool {
        (self.0)(value)
    }
}

impl Debug for Predicate {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        // There's nothing faithful to print for a closure, and pretending otherwise would
        // make two different predicates look identical in a debug dump.
        f.write_str("Predicate(<closure>)")
    }
}

impl Debug for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        match self {
            Value::Is(s) => f.debug_tuple("Is").field(s).finish(),
            Value::OneOf(s) => f.debug_tuple("OneOf").field(s).finish(),
            #[cfg(feature = "regex")]
            Value::Matches(r) => f.debug_tuple("Matches").field(&r.as_str()).finish(),
            Value::Passes(p) => f.debug_tuple("Passes").field(p).finish(),
        }
    }
}

//---------------------------------------------------------------------------
// Constructors

/// An item carrying at least one tag satisfying `m`.
pub fn contains(m: Match) -> Query {
    Query::Contains(m)
}

/// An item carrying a [`PlainTag`] whose content is this string.
///
/// Shorthand for `contains(Match::exact(s))`.
///
/// [`PlainTag`]: crate::tag::PlainTag
pub fn exact(s: impl Into<String>) -> Query {
    contains(Match::exact(s))
}

/// An item carrying a [`KeyValueTag`] with this key.
///
/// Shorthand for `contains(Match::has_key(key))`.
///
/// [`KeyValueTag`]: crate::tag::KeyValueTag
pub fn has_key(key: impl Into<String>) -> Query {
    contains(Match::has_key(key))
}

/// An item carrying a [`KeyValueTag`] with this key and a matching value.
///
/// Shorthand for `contains(Match::key_value(key, value))`.
///
/// [`KeyValueTag`]: crate::tag::KeyValueTag
pub fn key_value(key: impl Into<String>, value: Value) -> Query {
    contains(Match::key_value(key, value))
}

/// An item carrying a [`MultipartTag`] with exactly these parts.
///
/// Shorthand for `contains(Match::path(parts))`.
///
/// [`MultipartTag`]: crate::tag::MultipartTag
pub fn path(parts: impl IntoIterator<Item = impl Into<String>>) -> Query {
    contains(Match::path(parts))
}

/// An item carrying a [`MultipartTag`] starting with these parts.
///
/// Shorthand for `contains(Match::prefix(parts))`.
///
/// [`MultipartTag`]: crate::tag::MultipartTag
pub fn prefix(parts: impl IntoIterator<Item = impl Into<String>>) -> Query {
    contains(Match::prefix(parts))
}

/// An item satisfying every one of `queries`.
pub fn all(queries: impl IntoIterator<Item = Query>) -> Query {
    Query::All(queries.into_iter().collect())
}

/// An item satisfying at least one of `queries`.
pub fn any(queries: impl IntoIterator<Item = Query>) -> Query {
    Query::Any(queries.into_iter().collect())
}

/// An item not satisfying `query`.
pub fn not(query: Query) -> Query {
    Query::Not(Box::new(query))
}

/// A value which parses into `T` and then satisfies `f`.
///
/// This is the case [`Value`]'s closed variants can't reach: the type is the caller's, so
/// no fixed set of variants can cover it.
///
/// ```
/// # use tagbuddy::query::{parses_to, Match, Value};
/// // score:4 and score:5 match; score:2 and score:many don't.
/// let m = Match::KeyValue {
///     key: "score".to_owned(),
///     value: parses_to(|n: u32| n > 3),
/// };
/// # let _ = m;
/// ```
pub fn parses_to<T>(f: impl Fn(T) -> bool + Send + Sync + 'static) -> Value
where
    T: std::str::FromStr,
{
    Value::Passes(Predicate::new(move |raw| {
        raw.parse::<T>().map(&f).unwrap_or(false)
    }))
}

//---------------------------------------------------------------------------
// Operators
//
// `&`, `|` and `!` for queries written by hand. The combinator functions stay for
// building queries programmatically, where a `Vec` is what you already have.
//
// Conjunctions and disjunctions flatten as they're built, so `a & b & c` is one `All` of
// three rather than nested pairs. That keeps the tree shallow for the evaluator and makes
// the `Debug` output readable.

impl BitAnd for Query {
    type Output = Query;

    fn bitand(self, rhs: Query) -> Query {
        match (self, rhs) {
            (Query::All(mut left), Query::All(right)) => {
                left.extend(right);
                Query::All(left)
            }
            (Query::All(mut left), rhs) => {
                left.push(rhs);
                Query::All(left)
            }
            (lhs, Query::All(mut right)) => {
                right.insert(0, lhs);
                Query::All(right)
            }
            (lhs, rhs) => Query::All(vec![lhs, rhs]),
        }
    }
}

impl BitOr for Query {
    type Output = Query;

    fn bitor(self, rhs: Query) -> Query {
        match (self, rhs) {
            (Query::Any(mut left), Query::Any(right)) => {
                left.extend(right);
                Query::Any(left)
            }
            (Query::Any(mut left), rhs) => {
                left.push(rhs);
                Query::Any(left)
            }
            (lhs, Query::Any(mut right)) => {
                right.insert(0, lhs);
                Query::Any(right)
            }
            (lhs, rhs) => Query::Any(vec![lhs, rhs]),
        }
    }
}

impl NotOp for Query {
    type Output = Query;

    fn not(self) -> Query {
        Query::Not(Box::new(self))
    }
}
