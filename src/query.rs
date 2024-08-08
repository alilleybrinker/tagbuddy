use std::{hash::BuildHasher, collections::{BTreeMap, HashSet}, marker::PhantomData};
use string_interner::backend::Backend as InternerBackend;
use string_interner::Symbol;
use crate::{TagManager, label::Label, tag::Tag, parse::Parser};


struct QueryBuilder<
    'm,
    L,
    S,
    T,
    P,
    B,
    H,
>
where
    L: Label,
    S: Symbol,
    T: Tag<Label = L, Symbol = S>,
    P: Parser<Tag = T> + Send + Sync,
    B: InternerBackend<Symbol = S>,
    H: BuildHasher,
{
    manager: &'m TagManager<L, S, T, P, B, H>,
    indices: QueryIndices<S>,
}

struct QueryIndices<S> where S: Symbol {
    plain: PlainIndex<S>,
    key_value: KeyValueIndex<S>,
    multipart: MultipartIndex<S>,
}

struct PlainIndex<S>(Vec<S>) where S: Symbol;

struct KeyValueIndex<S>(BTreeMap<S, Vec<S>>) where S: Symbol;

struct MultipartIndex<S>(Vec<Trie<S>>) where S: Symbol;

struct Trie<S>(PhantomData<S>) where S: Symbol;


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
