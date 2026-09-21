//! Types defining how tag data is stored.

use crate::error::StorageError;
use crate::label::DefaultLabel;
use crate::label::Label;
#[cfg(doc)]
use crate::tag::Tag;
#[cfg(doc)]
use crate::TagManager;
pub use generativity::Guard;
pub use generativity::Id;
use std::fmt::Debug;
use std::hash::BuildHasher;
use std::marker::PhantomData;
use std::ops::Deref;
use std::ops::DerefMut;
use std::sync::Arc;
use std::sync::Mutex;
use std::sync::MutexGuard;
pub use string_interner::backend::Backend as InternerBackend;
pub use string_interner::DefaultBackend;
pub use string_interner::DefaultHashBuilder;
pub use string_interner::DefaultSymbol;
pub use string_interner::StringInterner;
use string_interner::Symbol;

/// Default interner, using the default backend, symbols, and hashing.
pub type DefaultInterner = StringInterner<DefaultBackend<DefaultSymbol>, DefaultHashBuilder>;

/// A [`Storage`] using the default label, backend, symbol, and hasher.
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
pub type DefaultStorage<'brand> =
    Storage<'brand, DefaultLabel, DefaultBackend<DefaultSymbol>, DefaultHashBuilder>;

/// Stores the actual tag data.
///
/// A [`Storage`] is, essentially, a wrapper around a [`StringInterner`] that handles three
/// things: 1) Ensuring the interner is always wrapped in an `Arc<Mutex<_>>`, 2) providing
/// a convenient `lock` method and associated `StorageLock` type to make the API for
/// _using_ the interner more ergonomic, and 3) carrying the two markers that keep tags
/// from being resolved through the wrong storage.
///
/// # Brands
///
/// The `'brand` lifetime is an _invariant_ lifetime, unique to this [`Storage`], minted
/// from a [`Guard`] which [`make_guard`] produces. Because it's invariant, no two
/// [`Storage`]s made from different [`Guard`]s can ever have their `'brand`s unified, and
/// because every [`Tag`] copies the `'brand` of the [`Storage`] that interned it, the
/// compiler rejects any attempt to resolve a [`Tag`] through a [`Storage`] that didn't
/// produce it:
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
/// assert_eq!(m1.resolve_tag(&tag).unwrap(), "hello");
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
/// assert_eq!(m2.resolve_tag(&tag).unwrap(), "hello"); // ERROR: `tag` carries m1's brand
/// ```
///
/// This is why there's no way to build a [`Storage`] without either consuming a [`Guard`]
/// or deriving it from an existing [`Storage`] (via [`Storage::share_as`], which keeps
/// the same interner _and_ the same brand). A constructor that let the caller pick the
/// `'brand` freely — a `Default` or `From` impl, say — would let a brand be attached to an
/// interner that never minted it, which is exactly the confusion the brand exists to prevent.
///
/// # What the brand does not cover
///
/// A brand pins the _identity_ of the interner, not its _contents_. Interning only ever
/// adds to an interner, so symbols stay valid as it grows, but anything holding a
/// [`StorageLock`] can reach `&mut StringInterner` and replace its contents wholesale,
/// which invalidates every symbol handed out before. That's out of reach of the brand:
/// it's the same interner afterwards, just emptied. Don't do that.
///
/// [`Storage`] also derefs to its `Arc`, which is an _unbranded_ handle. Nothing unsound
/// follows from holding one — [`Storage::shared`] mints a fresh brand for it, and
/// [`Storage::try_share_as`] checks before reusing an existing one — but note that the
/// handle on its own no longer says which brand it belongs to.
///
/// # Labels
///
/// The `L` parameter is a separate, and weaker, marker: it distinguishes _vocabularies_ of
/// tags that share one interner. [`Storage::share_as`] re-labels a [`Storage`] while
/// keeping its brand, so two [`TagManager`]s can share interned string data while still
/// keeping their tag types distinct.
///
/// [`make_guard`]: crate::brand::make_guard
pub struct Storage<
    'brand,
    L = DefaultLabel,
    B = DefaultBackend<DefaultSymbol>,
    H = DefaultHashBuilder,
>(Arc<Mutex<StringInterner<B, H>>>, PhantomData<L>, Id<'brand>)
where
    L: Label,
    B: InternerBackend,
    <B as InternerBackend>::Symbol: Symbol,
    H: BuildHasher;

impl<'brand, L, B, H> Storage<'brand, L, B, H>
where
    L: Label,
    B: InternerBackend,
    <B as InternerBackend>::Symbol: Symbol,
    H: BuildHasher + Default,
{
    /// Make a [`Storage`] with a freshly-created [`StringInterner`].
    pub fn fresh(guard: Guard<'brand>) -> Self {
        Storage::unique(guard, StringInterner::<B, H>::new())
    }

    /// Make a [`Storage`] with a freshly-created [`StringInterner`] with the specified capacity.
    pub fn fresh_with_capacity(guard: Guard<'brand>, cap: usize) -> Self {
        Storage::unique(guard, StringInterner::<B, H>::with_capacity(cap))
    }
}

impl<'brand, L, B, H> Storage<'brand, L, B, H>
where
    L: Label,
    B: InternerBackend,
    <B as InternerBackend>::Symbol: Symbol,
    H: BuildHasher,
{
    /// Make a [`Storage`] with a freshly-created [`StringInterner`] with the specified hash builder.
    pub fn fresh_with_hasher(guard: Guard<'brand>, hash_builder: H) -> Self {
        Storage::unique(guard, StringInterner::<B, H>::with_hasher(hash_builder))
    }

    /// Make a [`Storage`] with a freshly-created [`StringInterner`] with the specified capacity and hash builder.
    pub fn fresh_with_capacity_and_hasher(
        guard: Guard<'brand>,
        cap: usize,
        hash_builder: H,
    ) -> Self {
        Storage::unique(
            guard,
            StringInterner::<B, H>::with_capacity_and_hasher(cap, hash_builder),
        )
    }

    /// Take ownership of a singular interner to produce a [`Storage`].
    pub fn unique(guard: Guard<'brand>, interner: StringInterner<B, H>) -> Self {
        Storage(Arc::new(Mutex::new(interner)), PhantomData, guard.into())
    }

    /// Wrap an interner handle that didn't come from an existing [`Storage`], minting a
    /// new brand for it.
    ///
    /// A bare `Arc` carries no brand, so there's no brand here to preserve and a fresh one
    /// is the only correct answer: whoever owned the interner before wasn't handing out
    /// [`Tag`]s, because tags only come from a branded [`Storage`]. If the handle _is_ one
    /// of this crate's, prefer [`Storage::share_as`], or [`Storage::try_share_as`] when all
    /// you have is the raw handle.
    pub fn shared(guard: Guard<'brand>, interner: &Arc<Mutex<StringInterner<B, H>>>) -> Self {
        Storage(Arc::clone(interner), PhantomData, guard.into())
    }

    /// Make a [`Storage`] sharing this one's interner, under a different label.
    ///
    /// The result keeps the same `'brand`, because it's backed by the same interner and so
    /// its symbols resolve identically. Only the label changes, which is what makes it
    /// possible for two [`TagManager`]s to share string data while keeping their tag
    /// vocabularies distinct.
    pub fn share_as<L2>(&self) -> Storage<'brand, L2, B, H>
    where
        L2: Label,
    {
        Storage(Arc::clone(&self.0), PhantomData, self.2)
    }

    /// Share a raw interner handle under this [`Storage`]'s brand, if it really is this
    /// [`Storage`]'s interner.
    ///
    /// [`Storage`] derefs to its `Arc`, so a handle can get separated from the [`Storage`]
    /// it belongs to — stored in a struct field, handed to other code, and so on. Passing
    /// such a handle to [`Storage::shared`] would mint a _new_ brand for an interner that
    /// already has one, and the resulting [`Storage`] couldn't resolve any of the tags the
    /// original had already produced, even though the symbols are the very same ones.
    ///
    /// This is the way out of that: [`Arc::ptr_eq`] settles whether the handle is the same
    /// allocation, and if it is, adopting the brand is sound, because "same allocation"
    /// _is_ what the brand stands for.
    ///
    /// Returns `None` when the handle is a different interner, where adopting the brand
    /// would be exactly the confusion brands exist to prevent.
    pub fn try_share_as<L2>(
        &self,
        interner: &Arc<Mutex<StringInterner<B, H>>>,
    ) -> Option<Storage<'brand, L2, B, H>>
    where
        L2: Label,
    {
        Arc::ptr_eq(&self.0, interner).then(|| Storage(Arc::clone(interner), PhantomData, self.2))
    }

    /// Lock the [`Storage`]'s underlying [`StringInterner`].
    pub fn lock(&self) -> Result<StorageLock<'_, 'brand, L, B, H>, StorageError> {
        self.0
            .lock()
            .map(|guard| StorageLock(guard, PhantomData, self.2))
            .map_err(|_| StorageError::CouldNotLock)
    }
}

impl<L, B, H> Storage<'_, L, B, H>
where
    L: Label,
    B: InternerBackend + Clone,
    <B as InternerBackend>::Symbol: Symbol,
    H: BuildHasher + Clone,
{
    /// Make a [`Storage`] by completely copying all data stored in the provided [`Storage`]
    /// into a fresh interner.
    ///
    /// Because the copy is a genuinely separate interner, it needs a [`Guard`] of its own,
    /// and the result carries that new brand: tags made against the original don't resolve
    /// through the copy.
    pub fn deep_clone<'new, L2>(
        &self,
        guard: Guard<'new>,
    ) -> Result<Storage<'new, L2, B, H>, StorageError>
    where
        L2: Label,
    {
        Ok(Storage::unique(guard, self.lock()?.clone()))
    }
}

impl<L, B, H> Debug for Storage<'_, L, B, H>
where
    L: Label,
    B: InternerBackend + Debug,
    <B as InternerBackend>::Symbol: Symbol + Debug,
    H: BuildHasher,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Storage").field("0", &self.0).finish()
    }
}

impl<L, B, H> Deref for Storage<'_, L, B, H>
where
    L: Label,
    B: InternerBackend,
    <B as InternerBackend>::Symbol: Symbol,
    H: BuildHasher,
{
    type Target = Arc<Mutex<StringInterner<B, H>>>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

/// A lock on the underlying [`StringInterner`] in a [`Storage`].
///
/// The lock carries its [`Storage`]'s `'brand`, which is how a [`Tag`] picks up the brand
/// of the storage that interned it, and how resolving checks that the two agree.
pub struct StorageLock<'lock, 'brand, L, B, H>(
    MutexGuard<'lock, StringInterner<B, H>>,
    PhantomData<L>,
    Id<'brand>,
)
where
    L: Label,
    B: InternerBackend,
    <B as InternerBackend>::Symbol: Symbol,
    H: BuildHasher;

impl<'brand, L, B, H> StorageLock<'_, 'brand, L, B, H>
where
    L: Label,
    B: InternerBackend,
    <B as InternerBackend>::Symbol: Symbol,
    H: BuildHasher,
{
    /// Get the brand of the [`Storage`] this lock came from.
    ///
    /// Tags copy this when they're interned, which is what ties them to this storage.
    pub(crate) fn brand(&self) -> Id<'brand> {
        self.2
    }
}

impl<'lock, L, B, H> Deref for StorageLock<'lock, '_, L, B, H>
where
    L: Label,
    B: InternerBackend,
    <B as InternerBackend>::Symbol: Symbol,
    H: BuildHasher,
{
    type Target = MutexGuard<'lock, StringInterner<B, H>>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<L, B, H> DerefMut for StorageLock<'_, '_, L, B, H>
where
    L: Label,
    B: InternerBackend,
    <B as InternerBackend>::Symbol: Symbol,
    H: BuildHasher,
{
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}
