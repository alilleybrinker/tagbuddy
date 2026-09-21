//! Turning a string-based [`Query`] into a key-based one, against a given [`Storage`].

use crate::label::Label;
use crate::query::Match;
use crate::query::Predicate;
use crate::query::Query;
use crate::query::Value;
use crate::storage::Key;
use crate::storage::Storage;
use std::hash::BuildHasher;
use std::hash::Hash;

/// A [`Query`] with every string replaced by the key it interns to.
#[derive(Debug, Clone)]
pub(crate) enum Resolved<K> {
    Anything,
    Contains(ResolvedMatch<K>),
    All(Vec<Resolved<K>>),
    Any(Vec<Resolved<K>>),
    Not(Box<Resolved<K>>),
}

/// A [`Match`] with every string replaced by the key it interns to.
#[derive(Debug, Clone)]
pub(crate) enum ResolvedMatch<K> {
    /// Nothing can satisfy this: one of its strings was never interned, so no tag holds
    /// it. Kept as a variant rather than collapsing the whole query, because `Any` can
    /// still be satisfied by a sibling and `Not` inverts it.
    Never,
    Exact(K),
    HasKey(K),
    KeyValue {
        key: K,
        value: ResolvedValue<K>,
    },
    Path(Vec<K>),
    Prefix(Vec<K>),
    All(Vec<ResolvedMatch<K>>),
    Any(Vec<ResolvedMatch<K>>),
}

/// A [`Value`] constraint with strings replaced by keys where possible.
#[derive(Debug, Clone)]
pub(crate) enum ResolvedValue<K> {
    /// As [`ResolvedMatch::Never`].
    Never,
    Is(K),
    OneOf(Vec<K>),
    #[cfg(feature = "regex")]
    Matches(regex::Regex),
    Passes(Predicate),
}

impl<K: Key + Hash> Resolved<K> {
    /// Resolve a [`Query`] against a [`Storage`].
    ///
    /// Uses [`Storage::get`] rather than `get_or_intern`: a query must not add its own
    /// search terms to an append-only interner, and a term that isn't there already can't
    /// be on any tag anyway.
    pub(crate) fn new<L, H>(query: &Query, storage: &Storage<'_, L, K, H>) -> Self
    where
        L: Label,
        H: BuildHasher + Clone,
    {
        match query {
            Query::Anything => Resolved::Anything,
            Query::Contains(m) => Resolved::Contains(ResolvedMatch::new(m, storage)),
            Query::All(qs) => Resolved::All(qs.iter().map(|q| Self::new(q, storage)).collect()),
            Query::Any(qs) => Resolved::Any(qs.iter().map(|q| Self::new(q, storage)).collect()),
            Query::Not(q) => Resolved::Not(Box::new(Self::new(q, storage))),
        }
    }
}

impl<K: Key + Hash> ResolvedMatch<K> {
    fn new<L, H>(m: &Match, storage: &Storage<'_, L, K, H>) -> Self
    where
        L: Label,
        H: BuildHasher + Clone,
    {
        match m {
            Match::Exact(s) => match storage.get(s) {
                Some(k) => ResolvedMatch::Exact(k),
                None => ResolvedMatch::Never,
            },
            Match::HasKey(s) => match storage.get(s) {
                Some(k) => ResolvedMatch::HasKey(k),
                None => ResolvedMatch::Never,
            },
            Match::KeyValue { key, value } => match storage.get(key) {
                Some(key) => ResolvedMatch::KeyValue {
                    key,
                    value: ResolvedValue::new(value, storage),
                },
                None => ResolvedMatch::Never,
            },
            Match::Path(parts) => match resolve_all(parts, storage) {
                Some(parts) => ResolvedMatch::Path(parts),
                None => ResolvedMatch::Never,
            },
            Match::Prefix(parts) => match resolve_all(parts, storage) {
                Some(parts) => ResolvedMatch::Prefix(parts),
                None => ResolvedMatch::Never,
            },
            Match::All(ms) => {
                let resolved: Vec<_> = ms.iter().map(|m| Self::new(m, storage)).collect();

                // One unsatisfiable conjunct makes the whole conjunction unsatisfiable.
                if resolved.iter().any(|m| matches!(m, ResolvedMatch::Never)) {
                    ResolvedMatch::Never
                } else {
                    ResolvedMatch::All(resolved)
                }
            }
            Match::Any(ms) => ResolvedMatch::Any(
                ms.iter()
                    .map(|m| Self::new(m, storage))
                    // An unsatisfiable disjunct simply never contributes.
                    .filter(|m| !matches!(m, ResolvedMatch::Never))
                    .collect(),
            ),
        }
    }
}

impl<K: Key + Hash> ResolvedValue<K> {
    fn new<L, H>(value: &Value, storage: &Storage<'_, L, K, H>) -> Self
    where
        L: Label,
        H: BuildHasher + Clone,
    {
        match value {
            Value::Is(s) => match storage.get(s) {
                Some(k) => ResolvedValue::Is(k),
                None => ResolvedValue::Never,
            },
            Value::OneOf(ss) => {
                // Unlike `Path`, a missing member here just drops out: the others can
                // still match.
                let keys: Vec<_> = ss.iter().filter_map(|s| storage.get(s)).collect();

                if keys.is_empty() {
                    ResolvedValue::Never
                } else {
                    ResolvedValue::OneOf(keys)
                }
            }
            #[cfg(feature = "regex")]
            Value::Matches(r) => ResolvedValue::Matches(r.clone()),
            Value::Passes(p) => ResolvedValue::Passes(p.clone()),
        }
    }
}

/// Resolve every string, or `None` if any one of them was never interned.
fn resolve_all<L, K, H>(parts: &[String], storage: &Storage<'_, L, K, H>) -> Option<Vec<K>>
where
    L: Label,
    K: Key + Hash,
    H: BuildHasher + Clone,
{
    parts.iter().map(|part| storage.get(part)).collect()
}
