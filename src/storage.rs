//! Types defining how tag data is stored.

use crate::error::StorageError;
use crate::label::DefaultLabel;
use crate::label::Label;
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

/// Stores the actual tag data.
///
/// A [`Storage`] is, essentially, a wrapper around a [`StringInterner`] that handles three
/// things: 1) Ensuring the interner is always wrapped in an `Arc<Mutex<_>>`, 2) providing
/// a convenient `lock` method and associated `StorageLockGuard` type to make the API for
/// _using_ the interner more ergonomic, and 3) keying the storage on the "label" type associated
/// with it, while enabling the underlying interners to be shared even if the labels are
/// different.
pub struct Storage<L = DefaultLabel, B = DefaultBackend<DefaultSymbol>, H = DefaultHashBuilder>(
    Arc<Mutex<StringInterner<B, H>>>,
    PhantomData<L>,
)
where
    L: Label,
    B: InternerBackend,
    <B as InternerBackend>::Symbol: Symbol,
    H: BuildHasher;

impl<L, B, H> Storage<L, B, H>
where
    L: Label,
    B: InternerBackend,
    <B as InternerBackend>::Symbol: Symbol,
    H: BuildHasher + Default,
{
    /// Make a [`Storage`] with a freshly-created [`StringInterner`].
    pub fn fresh() -> Self {
        Storage::unique(StringInterner::<B, H>::new())
    }

    /// Make a [`Storage`] with a freshly-created [`StringInterner`] with the specified capacity.
    pub fn fresh_with_capacity(cap: usize) -> Self {
        Storage::unique(StringInterner::<B, H>::with_capacity(cap))
    }
}

impl<L, B, H> Storage<L, B, H>
where
    L: Label,
    B: InternerBackend,
    <B as InternerBackend>::Symbol: Symbol,
    H: BuildHasher,
{
    /// Make a [`Storage`] with a freshly-created [`StringInterner`] with the specified hash builder.
    pub fn fresh_with_hasher(hash_builder: H) -> Self {
        Storage::unique(StringInterner::<B, H>::with_hasher(hash_builder))
    }

    /// Make a [`Storage`] with a freshly-created [`StringInterner`] with the specified capacity and hash builder.
    pub fn fresh_with_capacity_and_hasher(cap: usize, hash_builder: H) -> Self {
        Storage::unique(StringInterner::<B, H>::with_capacity_and_hasher(
            cap,
            hash_builder,
        ))
    }

    /// Take ownership of a singular interner to produce a [`Storage`].
    pub fn unique(interner: StringInterner<B, H>) -> Self {
        Storage(Arc::new(Mutex::new(interner)), PhantomData)
    }

    /// Produce a [`Storage`] which may share its underlying interner.
    pub fn shared(interner: &Arc<Mutex<StringInterner<B, H>>>) -> Self {
        Storage(Arc::clone(interner), PhantomData)
    }

    /// Make a [`Storage`] by copying and sharing the underlying interner from the provided [`Storage`].
    pub fn shallow_clone<L2>(&self) -> Storage<L2, B, H>
    where
        L2: Label,
    {
        Storage::shared(self)
    }

    /// Lock the [`Storage`]'s underlying [`StringInterner`].
    pub fn lock(&self) -> Result<StorageLock<'_, L, B, H>, StorageError> {
        self.0
            .lock()
            .map(|guard| StorageLock(guard, PhantomData))
            .map_err(|_| StorageError::CouldNotLock)
    }
}

impl<L, B, H> Storage<L, B, H>
where
    L: Label,
    B: InternerBackend + Clone,
    <B as InternerBackend>::Symbol: Symbol,
    H: BuildHasher + Clone,
{
    /// Make a [`Storage`] by completely copying all data stored in the provided [`Storage`] into a fresh interner.
    pub fn deep_clone<L2>(&self) -> Result<Storage<L2, B, H>, StorageError>
    where
        L2: Label,
    {
        Ok(Storage::unique(self.lock()?.clone()))
    }
}

impl<L, B, H> Debug for Storage<L, B, H>
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

impl Default for Storage<DefaultLabel, DefaultBackend<DefaultSymbol>, DefaultHashBuilder> {
    fn default() -> Self {
        Storage::unique(DefaultInterner::new())
    }
}

impl<L, B, H> Deref for Storage<L, B, H>
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

impl<L, B, H> DerefMut for Storage<L, B, H>
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

impl<L, B, H> From<StringInterner<B, H>> for Storage<L, B, H>
where
    L: Label,
    B: InternerBackend,
    <B as InternerBackend>::Symbol: Symbol,
    H: BuildHasher,
{
    fn from(interner: StringInterner<B, H>) -> Self {
        Storage::unique(interner)
    }
}

impl<L, B, H> From<&Arc<Mutex<StringInterner<B, H>>>> for Storage<L, B, H>
where
    L: Label,
    B: InternerBackend,
    <B as InternerBackend>::Symbol: Symbol,
    H: BuildHasher,
{
    fn from(interner: &Arc<Mutex<StringInterner<B, H>>>) -> Self {
        Storage::shared(interner)
    }
}

/// A lock on the underlying [`StringInterner`] in a [`Storage`].
pub struct StorageLock<'lock, L, B, H>(MutexGuard<'lock, StringInterner<B, H>>, PhantomData<L>)
where
    L: Label,
    B: InternerBackend,
    <B as InternerBackend>::Symbol: Symbol,
    H: BuildHasher;

impl<'lock, L, B, H> Deref for StorageLock<'lock, L, B, H>
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

impl<'lock, L, B, H> DerefMut for StorageLock<'lock, L, B, H>
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
