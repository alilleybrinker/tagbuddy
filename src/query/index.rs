//! Evaluating a query from an inverted index built over a slice of items.

use crate::label::Label;
use crate::parse::Parser;
use crate::query::scan::matches_tag;
use crate::query::Query;
use crate::query::Resolved;
use crate::query::ResolvedMatch;
use crate::query::ResolvedValue;
use crate::storage::Key;
use crate::tag::Tag;
use crate::tag::TagParts;
use crate::tag::Tagged;
use crate::TagManager;
use std::collections::HashMap;
use std::hash::BuildHasher;
use std::hash::Hash;

/// Where an item sits in the indexed slice.
type Pos = u32;

/// An inverted index over a slice of [`Tagged`] items.
///
/// Produced by [`TagManager::index`]. Building costs about what one [`Scan`] costs, so an
/// index is worth it when the same items are queried repeatedly.
///
/// The index borrows the items it was built from, so it can't go stale: the borrow checker
/// won't let the collection change while the index is alive.
///
/// [`Scan`]: crate::query::Scan
/// [`TagManager::index`]: crate::TagManager::index
pub struct Index<'m, 'brand, 'items, L, K, T, P, H, I>
where
    L: Label,
    K: Key + Hash,
    T: Tag<'brand, Label = L, Key = K>,
    P: Parser<'brand, Tag = T> + Send + Sync,
    H: BuildHasher + Clone,
{
    manager: &'m TagManager<'brand, L, K, T, P, H>,
    items: &'items [I],

    /// Plain tag key to the items carrying it.
    plain: HashMap<K, Vec<Pos>>,

    /// Key-value tag's key to the items carrying a tag with that key.
    kv_key: HashMap<K, Vec<Pos>>,

    /// Key-value tag's exact (key, value) to the items carrying it.
    kv_pair: HashMap<(K, K), Vec<Pos>>,

    /// Multipart tags, as a trie over their parts.
    paths: Trie<K>,

    /// Every position, for `Not`.
    all: Vec<Pos>,

    /// Positions of items carrying at least one tag, for the empty conjunction.
    with_tags: Vec<Pos>,
}

/// A trie over multipart tag parts.
struct Trie<K> {
    children: HashMap<K, Trie<K>>,

    /// Items with a tag ending exactly at this node.
    here: Vec<Pos>,

    /// Items with a tag passing through this node, for prefix matching.
    through: Vec<Pos>,
}

impl<K: Key + Hash> Default for Trie<K> {
    fn default() -> Self {
        Trie {
            children: HashMap::new(),
            here: Vec::new(),
            through: Vec::new(),
        }
    }
}

impl<K: Key + Hash> Trie<K> {
    fn insert(&mut self, parts: &[K], pos: Pos) {
        self.through.push(pos);

        match parts.split_first() {
            None => self.here.push(pos),
            Some((head, rest)) => self.children.entry(*head).or_default().insert(rest, pos),
        }
    }

    fn get(&self, parts: &[K]) -> Option<&Trie<K>> {
        match parts.split_first() {
            None => Some(self),
            Some((head, rest)) => self.children.get(head)?.get(rest),
        }
    }

    fn tidy(&mut self) {
        tidy(&mut self.here);
        tidy(&mut self.through);

        for child in self.children.values_mut() {
            child.tidy();
        }
    }
}

/// A set of candidate positions, and whether it's the exact answer.
///
/// Some matches can only be narrowed by the index, not decided by it: a [`Match::All`]
/// because separate tags could satisfy separate conjuncts, and a value predicate because
/// the index can't see inside a closure or a regex. Those come back inexact and get
/// checked against the same `matches_tag` the scan uses, so the two paths can't disagree.
///
/// [`Match::All`]: crate::query::Match::All
struct Candidates {
    positions: Vec<Pos>,
    exact: bool,
}

impl Candidates {
    fn exact(positions: Vec<Pos>) -> Self {
        Candidates {
            positions,
            exact: true,
        }
    }

    fn narrowed(positions: Vec<Pos>) -> Self {
        Candidates {
            positions,
            exact: false,
        }
    }
}

impl<'m, 'brand, 'items, L, K, T, P, H, I> Index<'m, 'brand, 'items, L, K, T, P, H, I>
where
    L: Label,
    K: Key + Hash,
    T: Tag<'brand, Label = L, Key = K>,
    P: Parser<'brand, Tag = T> + Send + Sync,
    H: BuildHasher + Clone,
    I: Tagged<'brand, T>,
{
    /// Build an index over `items`.
    pub(crate) fn build(
        manager: &'m TagManager<'brand, L, K, T, P, H>,
        items: &'items [I],
    ) -> Self {
        let mut index = Index {
            manager,
            items,
            plain: HashMap::new(),
            kv_key: HashMap::new(),
            kv_pair: HashMap::new(),
            paths: Trie::default(),
            all: (0..items.len() as Pos).collect(),
            with_tags: Vec::new(),
        };

        for (pos, item) in items.iter().enumerate() {
            let pos = pos as Pos;

            if item.has_tags() {
                index.with_tags.push(pos);
            }

            for tag in item.get_tags() {
                match tag.parts() {
                    TagParts::Plain(k) => index.plain.entry(k).or_default().push(pos),
                    TagParts::KeyValue { key, value } => {
                        index.kv_key.entry(key).or_default().push(pos);
                        index.kv_pair.entry((key, value)).or_default().push(pos);
                    }
                    TagParts::Multipart(parts) => index.paths.insert(parts, pos),
                    TagParts::Opaque => {}
                }
            }
        }

        // Every posting list has to be sorted and deduplicated for the set operations to
        // work, and because one item can hold several tags landing in the same list.
        for positions in index.plain.values_mut() {
            tidy(positions);
        }
        for positions in index.kv_key.values_mut() {
            tidy(positions);
        }
        for positions in index.kv_pair.values_mut() {
            tidy(positions);
        }
        index.paths.tidy();
        tidy(&mut index.with_tags);

        index
    }

    /// The items this index was built over.
    pub fn items(&self) -> &'items [I] {
        self.items
    }

    /// Keep the items satisfying `query`.
    pub fn matching(
        &self,
        query: &Query,
    ) -> impl Iterator<Item = &'items I> + use<'items, L, K, T, P, H, I> {
        let resolved = Resolved::new(query, self.manager.storage());
        let positions = self.positions(&resolved);
        let items = self.items;

        positions.into_iter().map(move |pos| &items[pos as usize])
    }

    /// Positions of the items satisfying a resolved query.
    fn positions(&self, query: &Resolved<K>) -> Vec<Pos> {
        match query {
            Resolved::Anything => self.all.clone(),

            Resolved::Contains(m) => {
                let candidates = self.candidates(m);

                if candidates.exact {
                    candidates.positions
                } else {
                    self.confirm(candidates.positions, m)
                }
            }

            // An empty `All` is the empty conjunction, so it matches everything, which
            // `intersect_all` returns for an empty input.
            Resolved::All(qs) => intersect_all(qs.iter().map(|q| self.positions(q)), &self.all),

            Resolved::Any(qs) => union_all(qs.iter().map(|q| self.positions(q))),

            Resolved::Not(q) => difference(&self.all, &self.positions(q)),
        }
    }

    /// Candidate positions for a resolved match.
    fn candidates(&self, m: &ResolvedMatch<K>) -> Candidates {
        match m {
            ResolvedMatch::Never => Candidates::exact(Vec::new()),

            ResolvedMatch::Exact(k) => Candidates::exact(self.lookup(&self.plain, k)),

            ResolvedMatch::HasKey(k) => Candidates::exact(self.lookup(&self.kv_key, k)),

            ResolvedMatch::KeyValue { key, value } => match value {
                ResolvedValue::Never => Candidates::exact(Vec::new()),

                ResolvedValue::Is(v) => Candidates::exact(self.lookup(&self.kv_pair, &(*key, *v))),

                ResolvedValue::OneOf(vs) => Candidates::exact(union_all(
                    vs.iter().map(|v| self.lookup(&self.kv_pair, &(*key, *v))),
                )),

                // Opaque to the index: narrow to tags with the right key, then check.
                #[cfg(feature = "regex")]
                ResolvedValue::Matches(_) => Candidates::narrowed(self.lookup(&self.kv_key, key)),
                ResolvedValue::Passes(_) => Candidates::narrowed(self.lookup(&self.kv_key, key)),
            },

            ResolvedMatch::Path(parts) => Candidates::exact(
                self.paths
                    .get(parts)
                    .map(|node| node.here.clone())
                    .unwrap_or_default(),
            ),

            ResolvedMatch::Prefix(parts) => Candidates::exact(
                self.paths
                    .get(parts)
                    .map(|node| node.through.clone())
                    .unwrap_or_default(),
            ),

            ResolvedMatch::Any(ms) => {
                let parts: Vec<_> = ms.iter().map(|m| self.candidates(m)).collect();
                let exact = parts.iter().all(|c| c.exact);
                let positions = union_all(parts.into_iter().map(|c| c.positions));

                Candidates { positions, exact }
            }

            // Never exact, even when every conjunct is: the intersection says each
            // conjunct is satisfied by *some* tag on the item, where the match requires
            // one tag satisfying all of them. So this is a superset, and gets checked.
            //
            // The empty conjunction is satisfied by any tag at all, so its candidates are
            // the items that have one.
            ResolvedMatch::All(ms) => Candidates::narrowed(intersect_all(
                ms.iter().map(|m| self.candidates(m).positions),
                &self.with_tags,
            )),
        }
    }

    /// Check candidate positions against the authoritative per-tag matcher.
    fn confirm(&self, candidates: Vec<Pos>, m: &ResolvedMatch<K>) -> Vec<Pos> {
        let storage = self.manager.storage();

        candidates
            .into_iter()
            .filter(|pos| {
                self.items[*pos as usize]
                    .get_tags()
                    .any(|tag| matches_tag(&tag.parts(), m, storage))
            })
            .collect()
    }

    fn lookup<Q: Hash + Eq>(&self, map: &HashMap<Q, Vec<Pos>>, key: &Q) -> Vec<Pos> {
        map.get(key).cloned().unwrap_or_default()
    }
}

/// Sort and deduplicate a posting list.
fn tidy(positions: &mut Vec<Pos>) {
    positions.sort_unstable();
    positions.dedup();
}

/// Intersect sorted position lists, returning `empty_case` when there are none.
fn intersect_all(mut lists: impl Iterator<Item = Vec<Pos>>, empty_case: &[Pos]) -> Vec<Pos> {
    match lists.next() {
        None => empty_case.to_vec(),
        Some(first) => lists.fold(first, |acc, next| intersect(&acc, &next)),
    }
}

/// Union sorted position lists.
fn union_all(lists: impl Iterator<Item = Vec<Pos>>) -> Vec<Pos> {
    let mut merged: Vec<Pos> = lists.flatten().collect();
    tidy(&mut merged);
    merged
}

/// Intersect two sorted, deduplicated lists.
fn intersect(left: &[Pos], right: &[Pos]) -> Vec<Pos> {
    let (mut l, mut r) = (0, 0);
    let mut out = Vec::new();

    while l < left.len() && r < right.len() {
        match left[l].cmp(&right[r]) {
            std::cmp::Ordering::Less => l += 1,
            std::cmp::Ordering::Greater => r += 1,
            std::cmp::Ordering::Equal => {
                out.push(left[l]);
                l += 1;
                r += 1;
            }
        }
    }

    out
}

/// Everything in `left` that isn't in `right`, both sorted and deduplicated.
fn difference(left: &[Pos], right: &[Pos]) -> Vec<Pos> {
    let (mut l, mut r) = (0, 0);
    let mut out = Vec::new();

    while l < left.len() {
        if r >= right.len() {
            out.extend_from_slice(&left[l..]);
            break;
        }

        match left[l].cmp(&right[r]) {
            std::cmp::Ordering::Less => {
                out.push(left[l]);
                l += 1;
            }
            std::cmp::Ordering::Greater => r += 1,
            std::cmp::Ordering::Equal => {
                l += 1;
                r += 1;
            }
        }
    }

    out
}
