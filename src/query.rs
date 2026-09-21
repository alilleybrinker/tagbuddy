#![allow(dead_code)]

use crate::storage::Key;
use crate::{label::Label, parse::Parser, tag::Tag, TagManager};
use std::hash::Hash;
use std::{collections::BTreeMap, hash::BuildHasher, marker::PhantomData};

struct QueryBuilder<'m, 'brand, L, K, T, P, H>
where
    L: Label,
    K: Key + Hash,
    T: Tag<'brand, Label = L, Key = K>,
    P: Parser<'brand, Tag = T> + Send + Sync,
    H: BuildHasher + Clone,
{
    manager: &'m TagManager<'brand, L, K, T, P, H>,
    indices: QueryIndices<K>,
}

struct QueryIndices<K>
where
    K: Key + Hash,
{
    plain: PlainIndex<K>,
    key_value: KeyValueIndex<K>,
    multipart: MultipartIndex<K>,
}

struct PlainIndex<K>(Vec<K>)
where
    K: Key + Hash;

struct KeyValueIndex<K>(BTreeMap<K, Vec<K>>)
where
    K: Key + Hash;

struct MultipartIndex<K>(Vec<Trie<K>>)
where
    K: Key + Hash;

struct Trie<K>(PhantomData<K>)
where
    K: Key + Hash;

/*
The basic design of the query system is:

manager
    .select_from(&container_of_queryable_things)
    .where(Contains(And("this-tag", Or("that_tag", "someothertag"))))
    .run()


This isn't the exact API, because it needs to have a way to resolve
the query tags such that identity-based matching can happen.

When it's doing the "select_from" construction, it needs to go through the
queryable-things and construct indices of their tags.


Individual queries probably need to be relative to a single tag manager,
to be able to match up the parser and storage.

But then those queries return iterators over tagged items, and the
intersection of the returned items from multiple queries is the answer
to all the queries.


struct QueryEngine {
    indices: QueryIndices,
}

struct QueryIndices {
    plain_index: PlainIndex,
    key_value_index: KeyValueIndex,
    multipart_index: MultipartIndex,
}

/*
 Queries might include:
 - Find all items with this tag and that tag but not that tag
 - Find all items with tags starting with this path
 - Find all items with this key for key-value
 - Find all items with this key and a value matching some constraint
    - Specific value
    - Set of values
    - Values match regex
    - Values are parseable into a particular type
        - Values parseable into a type meet some constraint on that type

 */

// This will just be a sorted vector.
struct PlainIndex {

}

// This one will be a hash map, with keys being the keys of all KV tags, and values being sorted vectors of values.
struct KeyValueIndex {

}

// This one will be a forest, a set of trees with roots being all the first segments of multipart paths.
struct MultipartIndex {

}
*/
