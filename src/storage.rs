//! Types defining how tag data is stored.

use crate::label::DefaultLabel;
use crate::label::Label;
#[cfg(doc)]
use crate::tag::Tag;
#[cfg(doc)]
use crate::TagManager;
pub use generativity::Guard;
pub use generativity::Id;
pub use lasso::Capacity;
pub use lasso::Key;
pub use lasso::Spur;
pub use lasso::ThreadedRodeo;
use std::collections::hash_map::RandomState;
use std::fmt::Debug;
use std::fmt::Formatter;
use std::fmt::Result as FmtResult;
use std::hash::BuildHasher;
use std::hash::Hash;
use std::marker::PhantomData;
use std::sync::Arc;

/// The interner backing a [`Storage`].
///
/// This is [`lasso::ThreadedRodeo`], which is *append-only by construction*: it exposes no
/// `&mut self` method at all, and no `clear`, `remove`, or `truncate`. Since a [`Storage`]
/// only ever holds it behind an [`Arc`], there is no way — from inside this crate or out
/// of it — to remove or reindex a string once interned. That's what makes a key valid for
/// as long as the interner lives, and in turn what makes resolving infallible.
///
/// It also interns and resolves through `&self`, so tag storage needs no lock.
pub type Interner<K = Spur, H = RandomState> = ThreadedRodeo<K, H>;

/// A [`Storage`] using the default label, key, and hasher.
///
/// [`Storage`] can't implement [`Default`], because that would hand out a [`Storage`]
/// whose `'brand` the caller picks rather than one minted from a [`Guard`]. This alias
/// covers the same convenience case — "I don't care about labels" — while still going
/// through a [`Guard`]:
///
/// ```
/// # use tagbuddy::brand::make_guard;
/// # use tagbuddy::storage::DefaultStorage;
/// make_guard!(guard);
/// let storage = DefaultStorage::fresh(guard);
/// ```
pub type DefaultStorage<'brand> = Storage<'brand, DefaultLabel, Spur, RandomState>;

/// Stores the actual tag data.
///
/// A [`Storage`] wraps an [`Interner`] in an [`Arc`] and carries the two markers that keep
/// tags from being resolved through the wrong storage.
///
/// # Brands
///
/// The `'brand` lifetime is an _invariant_ lifetime, unique to this [`Storage`], minted
/// from a [`Guard`] which [`make_guard`] produces. Because it's invariant, no two
/// [`Storage`]s made from different [`Guard`]s can ever have their `'brand`s unified, and
/// because every [`Tag`] copies the `'brand` of the [`Storage`] that interned it, the
/// compiler rejects any attempt to resolve a [`Tag`] through a [`Storage`] that didn't
/// produce it.
///
/// Resolving a tag through the storage that interned it works:
///
/// ```
/// # use tagbuddy::storage::DefaultStorage;
/// # use tagbuddy::brand::make_guard;
/// # use tagbuddy::parse::Plain;
/// # use tagbuddy::TagManager;
/// make_guard!(g1);
/// make_guard!(g2);
///
/// let m1 = TagManager::builder().parser(Plain::new()).storage(DefaultStorage::fresh(g1)).build();
/// let m2 = TagManager::builder().parser(Plain::new()).storage(DefaultStorage::fresh(g2)).build();
///
/// let tag = m1.parse_tag("hello").unwrap();
/// assert_eq!(m1.resolve_tag(&tag), "hello");
/// # let _ = &m2;
/// ```
///
/// Resolving it through a different storage does not compile. Note that this example is
/// identical to the one above apart from which manager resolves the tag, which is what
/// makes it a real test: `compile_fail` on its own only proves that _something_ failed
/// (rustdoc does not verify the error code even when one is given), but since the
/// version above compiles, the brand is the only difference left to fail on.
///
/// ```compile_fail
/// # use tagbuddy::storage::DefaultStorage;
/// # use tagbuddy::brand::make_guard;
/// # use tagbuddy::parse::Plain;
/// # use tagbuddy::TagManager;
/// make_guard!(g1);
/// make_guard!(g2);
///
/// let m1 = TagManager::builder().parser(Plain::new()).storage(DefaultStorage::fresh(g1)).build();
/// let m2 = TagManager::builder().parser(Plain::new()).storage(DefaultStorage::fresh(g2)).build();
///
/// let tag = m1.parse_tag("hello").unwrap();
/// assert_eq!(m2.resolve_tag(&tag), "hello"); // ERROR: `tag` carries m1's brand
/// ```
///
/// This is why there's no way to build a [`Storage`] without either consuming a [`Guard`]
/// or deriving it from an existing [`Storage`] (via [`Storage::share_as`], which keeps the
/// same interner _and_ the same brand). A constructor that let the caller pick the
/// `'brand` freely — a `Default` or `From` impl, say — would let a brand be attached to an
/// interner that never minted it, which is exactly the confusion the brand exists to prevent.
///
/// # Why resolving can't fail
///
/// Two properties together: the brand says a key came from _this_ interner, and the
/// [`Interner`] being append-only says the key is still there. An [`Interner`] only ever
/// grows, so a key handed out is good for the life of the interner, whoever else holds a
/// handle to it. [`Storage::resolve`] therefore returns `&str` rather than an `Option`,
/// and every `resolve` in this crate is infallible.
///
/// # Labels
///
/// The `L` parameter is a separate, and weaker, marker: it distinguishes _vocabularies_ of
/// tags that share one interner. [`Storage::share_as`] re-labels a [`Storage`] while
/// keeping its brand, so two [`TagManager`]s can share interned string data while still
/// keeping their tag types distinct.
///
/// [`make_guard`]: crate::brand::make_guard
pub struct Storage<'brand, L = DefaultLabel, K = Spur, H = RandomState>(
    Arc<Interner<K, H>>,
    PhantomData<L>,
    Id<'brand>,
)
where
    L: Label,
    K: Key + Hash,
    H: BuildHasher + Clone;

impl<'brand, L, K> Storage<'brand, L, K, RandomState>
where
    L: Label,
    K: Key + Hash,
{
    /// Make a [`Storage`] with a freshly-created [`Interner`].
    pub fn fresh(guard: Guard<'brand>) -> Self {
        Storage::unique(guard, Interner::new())
    }

    /// Make a [`Storage`] with a freshly-created [`Interner`] with the specified capacity.
    pub fn fresh_with_capacity(guard: Guard<'brand>, capacity: Capacity) -> Self {
        Storage::unique(guard, Interner::with_capacity(capacity))
    }
}

impl<'brand, L, K, H> Storage<'brand, L, K, H>
where
    L: Label,
    K: Key + Hash,
    H: BuildHasher + Clone,
{
    /// Make a [`Storage`] with a freshly-created [`Interner`] with the specified hash builder.
    pub fn fresh_with_hasher(guard: Guard<'brand>, hash_builder: H) -> Self {
        Storage::unique(guard, Interner::with_hasher(hash_builder))
    }

    /// Make a [`Storage`] with a freshly-created [`Interner`] with the specified capacity
    /// and hash builder.
    pub fn fresh_with_capacity_and_hasher(
        guard: Guard<'brand>,
        capacity: Capacity,
        hash_builder: H,
    ) -> Self {
        Storage::unique(
            guard,
            Interner::with_capacity_and_hasher(capacity, hash_builder),
        )
    }

    /// Take ownership of a singular interner to produce a [`Storage`].
    pub fn unique(guard: Guard<'brand>, interner: Interner<K, H>) -> Self {
        Storage(Arc::new(interner), PhantomData, guard.into())
    }

    /// Wrap an interner handle that didn't come from an existing [`Storage`], minting a
    /// new brand for it.
    ///
    /// A bare handle carries no brand, so there's no brand here to preserve and a fresh
    /// one is the only correct answer: whoever owned the interner before wasn't handing
    /// out [`Tag`]s, because tags only come from a branded [`Storage`]. If the handle _is_
    /// one of this crate's, prefer [`Storage::share_as`], or [`Storage::try_share_as`]
    /// when all you have is the raw handle.
    ///
    /// Sharing a handle with code outside this crate stays safe: an [`Interner`] has no
    /// API for removing or reindexing a string, so the worst another holder can do is
    /// intern more strings, which leaves every existing key valid.
    pub fn shared(guard: Guard<'brand>, interner: &Arc<Interner<K, H>>) -> Self {
        Storage(Arc::clone(interner), PhantomData, guard.into())
    }

    /// Make a [`Storage`] sharing this one's interner, under a different label.
    ///
    /// The result keeps the same `'brand`, because it's backed by the same interner and so
    /// its keys resolve identically. Only the label changes, which is what makes it
    /// possible for two [`TagManager`]s to share string data while keeping their tag
    /// vocabularies distinct.
    pub fn share_as<L2>(&self) -> Storage<'brand, L2, K, H>
    where
        L2: Label,
    {
        Storage(Arc::clone(&self.0), PhantomData, self.2)
    }

    /// Share a raw interner handle under this [`Storage`]'s brand, if it really is this
    /// [`Storage`]'s interner.
    ///
    /// A handle can get separated from the [`Storage`] it belongs to — stored in a struct
    /// field, handed to other code, and so on. Passing such a handle to [`Storage::shared`]
    /// would mint a _new_ brand for an interner that already has one, and the resulting
    /// [`Storage`] couldn't resolve any of the tags the original had already produced, even
    /// though the keys are the very same ones.
    ///
    /// This is the way out of that: [`Arc::ptr_eq`] settles whether the handle is the same
    /// allocation, and if it is, adopting the brand is sound, because "same allocation" _is_
    /// what the brand stands for.
    ///
    /// Returns `None` when the handle is a different interner, where adopting the brand
    /// would be exactly the confusion brands exist to prevent.
    pub fn try_share_as<L2>(
        &self,
        interner: &Arc<Interner<K, H>>,
    ) -> Option<Storage<'brand, L2, K, H>>
    where
        L2: Label,
    {
        Arc::ptr_eq(&self.0, interner).then(|| Storage(Arc::clone(interner), PhantomData, self.2))
    }

    /// Get the brand of this [`Storage`].
    ///
    /// Tags copy this when they're interned, which is what ties them to this storage.
    pub(crate) fn brand(&self) -> Id<'brand> {
        self.2
    }

    /// Get a handle to the underlying [`Interner`].
    ///
    /// The handle is _unbranded_: on its own it no longer says which brand it belongs to.
    /// Nothing unsound follows from holding one — [`Storage::shared`] mints a fresh brand
    /// for it, and [`Storage::try_share_as`] checks before reusing an existing one.
    pub fn handle(&self) -> &Arc<Interner<K, H>> {
        &self.0
    }

    /// Intern a string, returning its key.
    pub fn get_or_intern(&self, raw: &str) -> K {
        self.0.get_or_intern(raw)
    }

    /// Get the key an already-interned string was given, if it has been interned.
    pub fn get(&self, raw: &str) -> Option<K> {
        self.0.get(raw)
    }

    /// Resolve a key back into its string.
    ///
    /// This can't fail for a key that came from this [`Storage`], which the `'brand` on
    /// every [`Tag`] guarantees: the [`Interner`] is append-only, so a key it handed out
    /// stays valid for as long as it lives. See [`Storage::try_resolve`] for the case of a
    /// key that didn't come from here.
    ///
    /// # Panics
    ///
    /// Panics if the key didn't come from this interner. Reaching that requires building a
    /// key by hand through [`Key::try_from_usize`], since no [`Tag`] can carry a foreign
    /// key past the brand check.
    pub fn resolve(&self, key: K) -> &str {
        self.0.resolve(&key)
    }

    /// Resolve a key back into its string, returning `None` if it didn't come from here.
    pub fn try_resolve(&self, key: K) -> Option<&str> {
        self.0.try_resolve(&key)
    }

    /// Get the number of strings interned.
    pub fn len(&self) -> usize {
        self.0.len()
    }

    /// Get whether nothing has been interned yet.
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

impl<L, K, H> Storage<'_, L, K, H>
where
    L: Label,
    K: Key + Hash,
    H: BuildHasher + Clone + Default,
{
    /// Make a [`Storage`] by copying every string in this one into a fresh [`Interner`].
    ///
    /// Because the copy is a genuinely separate interner, it needs a [`Guard`] of its own,
    /// and the result carries that new brand: tags made against the original don't resolve
    /// through the copy. That's not just bookkeeping — the copy assigns its own keys, and
    /// makes no attempt to give a string the key it had in the original, so the old keys
    /// would be meaningless here even if the type system allowed them through.
    ///
    /// A tag still resolves through the storage it came from, copy or no copy:
    ///
    /// ```
    /// # use tagbuddy::brand::make_guard;
    /// # use tagbuddy::label::DefaultLabel;
    /// # use tagbuddy::parse::Plain;
    /// # use tagbuddy::storage::DefaultStorage;
    /// # use tagbuddy::TagManager;
    /// make_guard!(guard);
    /// let storage = DefaultStorage::fresh(guard);
    /// let manager = TagManager::builder()
    ///     .parser(Plain::new())
    ///     .storage(storage.share_as::<DefaultLabel>())
    ///     .build();
    /// let tag = manager.parse_tag("hello").unwrap();
    ///
    /// make_guard!(copy_guard);
    /// let copy: DefaultStorage = storage.deep_clone(copy_guard);
    /// let copied = TagManager::builder()
    ///     .parser(Plain::new())
    ///     .storage(copy.share_as::<DefaultLabel>())
    ///     .build();
    ///
    /// assert_eq!(manager.resolve_tag(&tag), "hello");
    /// # let _ = &copied;
    /// ```
    ///
    /// Resolving it through the copy does not compile. As with the pair on [`Storage`]
    /// itself, the value of this `compile_fail` block rests on the example above
    /// compiling: the two differ only in which manager resolves the tag, so the copy's
    /// new brand is the only thing left for it to fail on.
    ///
    /// ```compile_fail
    /// # use tagbuddy::brand::make_guard;
    /// # use tagbuddy::label::DefaultLabel;
    /// # use tagbuddy::parse::Plain;
    /// # use tagbuddy::storage::DefaultStorage;
    /// # use tagbuddy::TagManager;
    /// make_guard!(guard);
    /// let storage = DefaultStorage::fresh(guard);
    /// let manager = TagManager::builder()
    ///     .parser(Plain::new())
    ///     .storage(storage.share_as::<DefaultLabel>())
    ///     .build();
    /// let tag = manager.parse_tag("hello").unwrap();
    ///
    /// make_guard!(copy_guard);
    /// let copy: DefaultStorage = storage.deep_clone(copy_guard);
    /// let copied = TagManager::builder()
    ///     .parser(Plain::new())
    ///     .storage(copy.share_as::<DefaultLabel>())
    ///     .build();
    ///
    /// assert_eq!(copied.resolve_tag(&tag), "hello"); // ERROR: `tag` carries the original's brand
    /// ```
    pub fn deep_clone<'new, L2>(&self, guard: Guard<'new>) -> Storage<'new, L2, K, H>
    where
        L2: Label,
    {
        let fresh = Interner::with_hasher(H::default());

        for string in self.0.strings() {
            fresh.get_or_intern(string);
        }

        Storage::unique(guard, fresh)
    }
}

impl<L, K, H> Debug for Storage<'_, L, K, H>
where
    L: Label,
    K: Key + Hash + Debug,
    H: BuildHasher + Clone,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        f.debug_struct("Storage").field("0", &self.0).finish()
    }
}
