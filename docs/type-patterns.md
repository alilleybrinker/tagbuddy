# Type System Patterns in `tagbuddy`

`tagbuddy` leans on Rust's type system harder than most small libraries do. If you've read
the source and wondered why a crate that stores interned strings needs a lifetime
parameter on nearly everything, or why there's a trait called `ManagerParts` that appears
to exist purely so that nobody has to write type parameters, this is the document that
answers those questions.

Most of the techniques here exist to do one of two things: move a class of bug from
runtime to compile time, or stop the API from demanding information it could work out for
itself. In this article I'll walk through twelve of them, explain what problem each one
solves, and — where it applies — describe the version that was wrong first. Several of
these patterns are here because the obvious design turned out to be broken, and the broken
version is usually more instructive than the finished one.

You don't need to have used `tagbuddy` to follow along. The patterns are general. The
crate is just where I happened to hit them.

## Pattern 1: Generative Lifetimes, or Making One Value Distinguishable From Another

Let's start with the problem the whole crate is built around.

A tag in `tagbuddy` is an index into a string table. That's what makes tags cheap — a
`PlainTag` is four bytes, and a thousand posts tagged `rust` share a single copy of the
string. But an index is only meaningful relative to the table it came from. Index 7 in one
table and index 7 in another are different strings, and there's nothing about the number
itself that says which table it belongs to.

So what happens if you resolve a tag against the wrong store? Not an error. You get back a
*different string*, silently, and your program carries on with it. This is the worst
category of bug: no panic, no `Result`, just a wrong answer that looks exactly like a
right one.

Ordinary types can't help here, because two `Storage` values have the same type. Their
tags have the same type too. As far as the compiler is concerned, they're interchangeable.

The fix is to give each `Storage` value a type that no other `Storage` value shares, using
a *generative* (or *branded*) lifetime.

```rust,ignore
pub struct Storage<'brand, L = DefaultLabel, K = Spur, H = RandomState>(
    Arc<Interner<K, H>>,
    PhantomData<L>,
    Id<'brand>,
);
```

*Code 1 — The `Storage` type. `Id<'brand>` is the part that makes each value distinct.*

`Id<'brand>` comes from the [`generativity`] crate, and underneath it's a zero-sized
marker:

```rust,ignore
// What `generativity` uses.
PhantomData<fn(&'id ()) -> &'id ()>

// What you might reach for instead.
PhantomData<Cell<&'id ()>>
```

*Code 2 — Two ways to make a lifetime invariant. Only one of them is `Sync`.*

Two properties of that first marker matter, and it's worth being precise about why.

The first is **invariance**. A lifetime in Rust is usually *covariant*, meaning the
compiler is free to shorten it whenever that makes a program typecheck. That's exactly
what you don't want here: if the compiler can shorten one brand to match another, the two
brands unify, and the whole mechanism silently does nothing at all. A function pointer
type `fn(T) -> T` is invariant in `T`, because `T` appears in both argument and return
position, so `'brand` can never be coerced to anything else. Two brands never unify. Ever.

The second is that the marker is **still `Send` and `Sync`**. A function pointer is both.
The other common invariance marker, `PhantomData<Cell<&'id ()>>`, is *not* `Sync` — and
since a field's auto trait implementations propagate to the struct containing it, choosing
that one would have made every `Storage` thread-hostile. `tagbuddy` advertises lock-free
concurrent reads, so the wrong marker would have quietly broken one of the crate's
headline features while appearing to solve a completely unrelated problem. If you take one
thing from this section, take that: when you add a marker type, check which auto traits it
carries.

Where does the unique lifetime come from? The `make_guard!` macro produces a
`Guard<'brand>` whose lifetime cannot be named or reused, and a `Storage` constructor
consumes one. Every tag then copies the brand of the storage that interned it, so crossing
them fails to compile.

> **Info 1 — How `make_guard!` actually works.**
>
> The macro creates a value whose drop timing bounds the lifetime in a way the caller
> can't replicate or name. It's a genuinely clever trick, and it's thoroughly explained in
> the [`generativity`] documentation, so I won't reproduce it here. For our purposes the
> guard is a token: you get a fresh one each time, and you can't forge one.

What does this cost? A lifetime parameter on `Storage`, `Tag`, `Parser`, `TagManager`, and
everything generic over any of them. Branded values can't outlive their guard's scope, and
they can't be stored in the same struct as data they borrow. That's a real price, and it's
why I'd hesitate to recommend this pattern casually. It was worth paying here because the
crate's entire premise is cheap interned handles, and handles are precisely the thing
that's dangerous to mix up.

[`generativity`]: https://docs.rs/generativity

## Pattern 2: A Phantom Parameter Is Only as Strong as Its Weakest Constructor

This is the part that's easy to get wrong, and I want to spend some time on it, because
`tagbuddy` got it wrong three separate times.

The brand from Pattern 1 only means something if a `'brand` can't be attached to a store
that didn't mint it. Say that again slowly, because it has a consequence: **every way of
obtaining a `Storage` is part of the guarantee.** Not just the functions named `new`.
Every path.

Each constructor must either consume a `Guard<'brand>` or derive from an existing storage
over the same interner.

*Table 1 — How each constructor obtains its brand.*

| Constructor | Brand |
|---|---|
| `fresh`, `unique`, `shared`, `fresh_with_*` | consumes a `Guard` |
| `share_as`, `try_share_as` | inherits it, same interner |
| `deep_clone` | consumes a *new* `Guard`, because it's a different interner |

Now consider what a `Default` impl would have meant.

```rust,ignore
impl<'brand, L, K, H> Default for Storage<'brand, L, K, H> { /* ... */ }
```

*Code 3 — An impl that quietly destroys the guarantee.*

That signature hands out a `Storage<'brand, ..>` for whatever `'brand` the caller cares to
name — including the brand of a store the caller already holds tags for. No guard
required. The same was true of `From<StringInterner>`. Both impls existed in the crate,
and both had to go. I want to stress how innocuous `Default` looks when you're scanning a
file for soundness problems. It looks like boilerplate.

Two more leaks turned up later, in code that had nothing obviously to do with brands at
all.

**`DerefMut` to the inner `Arc`.** With it, you could write `*storage = other_arc`,
swapping the interner out while the brand stayed exactly where it was. I confirmed this
with a test: a tag interned as `"hello"` resolved, afterwards, to `"decoy"`. The fix was
to remove `DerefMut` and have `handle()` return `&Arc` rather than `&mut Arc`, which also
puts `Arc::get_mut` out of reach.

**`Storage::shared` taking a bare handle.** This one failed in the opposite direction. It
minted a *new* brand for an interner that already had one, so tags from the original
couldn't be used with the result. Not unsound — just uselessly strict, which is its own
kind of bug. `try_share_as` fixes it by checking `Arc::ptr_eq` and adopting the existing
brand when the handle really is the same allocation.

The lesson I'd carry to another crate is this: when a phantom parameter encodes an
invariant, audit the constructors *and the trait impls* as a single surface. For this
purpose `Deref`, `DerefMut`, `Default`, `From`, and `Clone` are all constructors, and the
derive macros will write several of them for you without asking.

## Pattern 3: One Marker per Job

`tagbuddy` has two markers that look similar and do genuinely different things.

The first is `'brand`, which is **storage identity**. It's enforced by the compiler, and
it prevents resolving a tag against the wrong interner.

The second is `Label`, which is **vocabulary separation**. It's a convention rather than a
guarantee, and it keeps "post tags" and "ratings" distinct at the type level even when
they deliberately share a single interner via `share_as`.

These were originally one thing. `Label` existed alone, and its documentation claimed it
gave "the compile-time guarantee that a `Tag` generated by one `TagManager` is never
resolved through another."

It did not. Two `Storage::fresh()` calls produce independent interners, and nothing stops
you giving both the same label. I wrote a small program to check, and a tag interned as
`"hello"` in the first manager resolved, through the second, as `"world"`.

What interests me here is *why* the bug survived as long as it did. `Label` genuinely
delivered the vocabulary half of what it promised — two labelled managers really do keep
their tags apart in the type system. Because half of the claim was true, and visibly
working, the other half looked true too. Conflating two jobs into one marker meant that
evidence for one was mistaken for evidence for both.

Splitting them made the gap obvious, and it let each job get the mechanism that suits it:
a lifetime for the thing that must be enforced, a plain type parameter for the thing
that's a naming convention.

## Pattern 4: Projecting Parameters Instead of Restating Them

Here's what `TagManager` used to look like at a use site.

```rust,ignore
TagManager<'brand, Tags, Spur, PlainTag<'brand, Tags>, Plain<Tags>>
```

*Code 4 — Six parameters, and `Tags` appears three times.*

Count the repetition. `Tags` shows up three times, and each of those is a chance for the
caller to write something inconsistent.

But look at what's actually free here. Given the parser, everything else follows: the tag
type is `P::Tag`, and the label and key are that tag's own associated types. Only the
parser and the hasher were ever real choices.

```rust,ignore
TagManager<'brand, Plain<Tags>>
```

*Code 5 — The same type, after the determined parameters were removed.*

Internally, the field types use projections — `LabelOf<'brand, P>`, `KeyOf<'brand, P>` —
and those aliases are public, so a caller who needs to name one still can.

The general rule: **if `B` is determined by `A`, don't take both.** The caller can't
supply a `B` that disagrees with `A`, because the bounds reject it. So the second
parameter buys no expressiveness at all. All it does is give the caller something to type
and something to get wrong.

## Pattern 5: Bundling Parameters Behind Associated Types

Pattern 4 handles the case where one parameter determines another. This is the harder
case: what do you do when a type needs to name *another type that has a lot of
parameters*?

`Index` needs a manager. Naming a manager meant naming everything the manager takes.

```rust,ignore
Index<'m, 'brand, 'items, L, K, T, P, H, I>
```

*Code 6 — Nine parameters, most of which a caller never chose.*

At a call site this didn't hurt, because inference filled it all in. It hurt in struct
fields and return types — which is exactly where you want to put a long-lived index, since
the entire point of building one is to reuse it across many queries.

The fix is to define a trait that bundles the parts.

```rust,ignore
pub trait ManagerParts: sealed::Sealed {
    type Key: Key + Hash;
    type Tag;

    fn lookup(&self, raw: &str) -> Option<Self::Key>;
    fn text(&self, key: Self::Key) -> &str;
    fn parts_of(tag: &Self::Tag) -> TagParts<'_, Self::Key>;
}
```

*Code 7 — `ManagerParts`, which stands in for a whole manager.*

```rust,ignore
Index<'m, 'items, M, I>
```

*Code 8 — Nine parameters down to four.*

Two details make this work, and they're the genuinely interesting part of the pattern,
because a naive version of the trait reintroduces everything it was meant to remove.

**The trait has no `'brand` parameter.** This surprised me when I first got it to compile.
`type Tag` is simply the manager's tag type, and *that type already carries its own
brand*. So `<Mgr<'b> as ManagerParts>::Tag` projects to `PlainTag<'b>` without the trait
ever mentioning `'b`. An associated type can carry a lifetime that arrives through `Self`
without the trait itself declaring that lifetime. If the trait had to declare `'brand`,
every bound mentioning `ManagerParts` would have to name it, and the bundle would have
bought nothing.

**It exposes operations, not the storage.** The obvious way to write this trait is with an
accessor.

```rust,ignore
fn storage(&self) -> &Storage<'brand, L, K, H>;
```

*Code 9 — The accessor that would have forced `'brand` back into the trait.*

That signature names the brand. Naming the brand means the trait needs a lifetime
parameter, which lands you right back where you started. Exposing the three operations the
query engine actually performs keeps the lifetime out of the picture entirely. `parts_of`
is an associated function rather than a method for the same reason — calling `Tag::parts`
would require the bound `Self::Tag: Tag<'brand>`, and there's no `'brand` available to
write.

The trait is sealed with a private supertrait. Nothing outside the crate could sensibly be
"the parts of a manager," and sealing keeps those three methods free to change.

## Pattern 6: Dropping a Lifetime That Only States a Bound

`Tagged` used to be declared like this.

```rust,ignore
pub trait Tagged<'brand, T: Tag<'brand>> { /* ... */ }
```

*Code 10 — A trait whose lifetime parameter exists only so the bound can be written.*

The `'brand` here isn't doing any work of its own. It's there so that `T: Tag<'brand>` is
a sentence. But a lifetime on a trait declaration isn't free — it shows up in every impl,
and in every bound anywhere that mentions the trait.

In this case it also blocked Pattern 5 outright. Once a manager's parameters are bundled
behind `ManagerParts`, there is no `'brand` in scope to write `I: Tagged<'brand, M::Tag>`
with. The two patterns were in direct conflict, and the bound-only lifetime is the one
that had to give.

Dropping the bound gives `Tagged<T>`. Impls read `impl<'brand> Tagged<PlainTag<'brand,
Tags>> for BlogPost<'brand>`, and code that actually needs `T` to be a real `Tag` says so
at its own use site. The integration test still implements the trait twice for one type,
once per tag vocabulary, so the disambiguation the tag type provides is entirely
unaffected.

The rule worth remembering: **a parameter that exists only to state a bound is usually
better stated at the use site.** Bounds on trait declarations are rarely load-bearing.
Mostly they move work from one place to every other place.

## Pattern 7: Making a Property Structural Rather Than Enforced

The brand tells you a key came from *this* interner. It doesn't tell you the key is still
valid — that requires the interner to never remove anything.

You could write that down as a rule and hope reviewers catch violations. The better option
is to pick a type that can't do otherwise. [`lasso::ThreadedRodeo`] has **no `&mut self`
method at all**, and no `clear`, `remove`, or `truncate`. `Storage` holds it behind an
`Arc` with no `DerefMut` (Pattern 2), and `handle()` returns `&Arc`, which puts
`Arc::get_mut` out of reach. Append-only isn't a rule anyone has to follow here. It's a
shape with no other option available.

I want to flag how I got the analysis wrong the first time, because the correction is the
useful part. I initially justified this change as "the new interner has no removal
methods." That was true but beside the point: the *previous* interner had no removal
methods either. The actual invalidation route was always wholesale replacement through a
`&mut` — assignment, `mem::swap`, `mem::take`. The fix was refusing to hand out `&mut` at
all, not shopping for a type with a smaller method surface.

So when you're auditing whether something can be invalidated, look at what the *container*
permits, not only at what the contained type's own methods offer.

[`lasso::ThreadedRodeo`]: https://docs.rs/lasso/latest/lasso/struct.ThreadedRodeo.html

## Pattern 8: Two Invariants Can Delete an Error Type

Put Patterns 1 and 7 together and something nice happens.

The brand says the key came from this interner. Append-only says the key is still there.
Between them, resolving a tag cannot fail. Not "rarely fails," not "fails only if you've
done something strange" — there is no failure case left to represent.

So `resolve_tag` returns `String`, with no `Result` around it, and the crate's
`ResolveError` was deleted outright. Every one of its variants — `TagNotFound`,
`KeyNotFound`, `ValueNotFound`, `PartNotFound` — had become unreachable.

Going lock-free removed the rest of the error surface. `ThreadedRodeo` interns and
resolves through `&self`, so there's no `Mutex`, so there's no lock poisoning, so
`StorageError` and `ParseError::StorageError` went too. With them went the `StorageLock`
type and a chunk of machinery in the batch methods that existed only so a caller could
take the lock once rather than once per tag.

The pattern to take away: **an error variant is often the residue of a missing
invariant.** It's worth asking, of any `Result`-returning function you write, whether the
failure is intrinsic to the operation or merely unproven. Sometimes the answer is that
you're one invariant away from a function that can't fail.

## Pattern 9: Inference Is Part of the Signature

Here's a constructor call that looks obviously fine and did not compile.

```rust,ignore
let manager = TagManager::new(guard, Plain::new());
```

*Code 11 — Correct in every respect except the one that matters.*

`new` creates the storage itself, which means the storage's label is determined by the
parser — and the parser's label is determined by nothing at all. `Plain::new()` is generic
over its label, so there was no information anywhere in the expression from which to pick
one. The function type checked perfectly well. It was simply unusable without a turbofish
naming a type the caller had no reason to know existed.

The fix was to constrain `new` to the default label and key. That makes it inferable, and
it happens to draw the line exactly where the design already draws it: labels exist so
that two managers can share one interner, and setting that up requires the builder and an
explicit `Storage` anyway.

There's a pleasing detail in how this got caught. The crate's blog integration test failed
to compile on `new`, because the blog has a labelled manager — precisely the case that
should be going through the builder. The test was right and the constructor was wrong. The
test now demonstrates both paths.

The rule: **a parameter that nothing constrains is a parameter the caller must annotate.**
When you add a convenience constructor, check what it removes from the inference context,
not just what it removes from the call.

## Pattern 10: `unsafe` Is Not a Documentation Mechanism

`Label` used to be an `unsafe trait`, and its own documentation was refreshingly candid
about why.

> There aren't actual safety concerns around its use […] but it's marked `unsafe` to hint
> toward the special guarantees around the marker trait.

The obligation being gestured at — one label per interner — is now the brand's job, and
the compiler enforces it rather than asking implementors to be careful. So the `unsafe`
came off.

An `unsafe trait` in a public API tells a reader that there is a memory-safety obligation
somewhere, and that they need to go find it and satisfy it. Spending that signal on
emphasis makes the real ones harder to take seriously. It's a bit like a lint you've
configured to fire on style issues: after a while nobody reads the output.

There's a related problem in the opposite direction, and it was considerably worse. The
crate carried hand-written `unsafe impl Send` and `unsafe impl Sync` for `TagManager` that
were *actually unsound* — unconditional in the backend and hasher parameters. A manager
holding an `Rc`-bearing hasher claimed to be `Send`, and I verified that it did. Removing
both impls left auto-derivation, which gets the answer right on its own: the default
manager is still `Send + Sync`, and the `Rc` case now correctly fails to compile.

Hand-written `unsafe impl`s of auto traits deserve suspicion in proportion to how generic
the type is. The compiler's version already accounts for every field. A hand-written one
accounts for whatever the author was thinking about that day.

## Pattern 11: Verifying a Type-Level Guarantee

A guarantee that lives in the type system needs a test that lives there too. This turns
out to be harder than it looks, and I nearly shipped a set of tests that proved nothing.

**`compile_fail` doctests do not prove what you think they prove.** A `compile_fail` block
asserts that some code failed to compile. It does not assert *why*. And rustdoc does not
verify the error code even when you supply one — I checked this directly, by annotating a
block with `E0999`, which is not a real Rust error code at all. The test passed.

> **Info 2 — Why this matters more than it sounds like it does.**
>
> A `compile_fail` test can be passing because of a typo in an import, or an inference
> ambiguity, or a renamed method — while the property it claims to check has quietly
> stopped holding. The test goes green either way, forever, and it looks like coverage.

Two things make the ones in this crate trustworthy.

**Pair each failing example with a compiling twin.** Every `compile_fail` block in
`tagbuddy` sits next to an example that is identical except for the single thing under
test, which is usually which manager resolves the tag. Because the twin compiles, the one
difference between them is the only available explanation for the failure.

**Check the error code once, by hand.** Each block was extracted into a real test file and
compiled, to confirm that it fails with `E0716` — the brand — rather than something
incidental. That's a manual step, done once, not something CI repeats. But it's the step
that converts "this doesn't compile" into "this doesn't compile *for the reason I claim*."

For runtime properties there's an analogous discipline: **make the test fail on purpose.**
The query engine has two execution strategies, a scan and an index, which must agree. A
test generates around three hundred queries and compares both paths. That test is only
worth having if it's capable of failing, so I introduced the specific bug it exists to
catch — treating `Match::All` candidates as exact when they're really a superset — and
confirmed that the test caught it. A test that has never failed is a hypothesis, not a
test.

## Pattern 12: Small Things

Three smaller points that didn't need sections of their own.

### Niche Optimization Is Free API Quality

Keys are `NonZeroU32`, so `PlainTag` is four bytes and `Option<PlainTag>` is *also* four
bytes. The compiler uses the impossible zero value to represent `None`.

```rust,ignore
assert_eq!(size_of::<PlainTag>(), 4);
assert_eq!(size_of::<Option<PlainTag>>(), 4);
```

*Code 12 — The niche in action.*

This is worth knowing when you choose a key representation, because the alternative is
invisible. Picking `u32` over `NonZeroU32` would silently cost a word in every `Option`
anyone ever wraps a tag in, and nothing would tell you.

### RPIT Lifetime Capture Has Two Fixes, and the Boring One Is Usually Better

Returning `impl Iterator<Item = &'s str> + 's` from a method on a branded type fails with
`E0700`, because the closure captures `'brand` and the bound doesn't name it. Precise
capturing — `+ use<..>` — fixes it, but it raises the MSRV and requires you to list every
type parameter you want captured.

The alternative is to capture less. Dereference to the interner first, whose type has no
brand in it, and the closure never sees the lifetime at all. Reach for `use<>` when the
capture is genuinely needed, not to paper over capturing more than you meant to.

### Sealed Traits

`ManagerParts` is sealed with a private supertrait. It's public because users write it in
bounds, but nothing outside the crate could implement it meaningfully, and sealing leaves
its three methods free to change without a breaking release.

## What Did All of This Cost?

I've spent this whole article on what these patterns buy, so it's only fair to be equally
specific about the bill.

- Every branded type carries a lifetime parameter, and branded values can't escape their
  guard's scope. This is the big one, and it shapes how callers structure their programs.
- The crate is pinned to Rust 1.85, entirely because `generativity` uses edition 2024.
  Nothing in `tagbuddy`'s own code needs anything close to that recent.
- Memory is never reclaimed, which is what append-only means. Compaction means copying
  into a fresh store, and the brand then guarantees that old tags can't follow the data
  across — which is correct, and also inconvenient.
- `Index` borrows the items it indexes, so it can't be stored alongside them, and it has
  to be rebuilt rather than updated.

I think the trade was worth it for this crate, whose entire premise is cheap interned
handles — handles being exactly the thing that's dangerous to mix up. I don't think it
would obviously be worth it for a crate whose values carry their own meaning.

## Conclusion

If you take a handful of things away from all of this, I'd suggest these.

- A phantom parameter is only as strong as its weakest constructor, and `Default`, `From`,
  and `DerefMut` are all constructors.
- Give each invariant its own marker. Conflating two jobs hides the one you haven't
  actually done.
- If one parameter determines another, take only the first. If a type has many parameters,
  bundle them behind a trait — and check whether that trait really needs the lifetime.
- An error variant is often the residue of a missing invariant.
- A type that can't do the wrong thing beats a rule that says not to.
- Whether your API can be inferred is part of its signature, and only a call site will
  tell you.
- A test that has never failed is a hypothesis. Break it on purpose once, and find out.

None of these are unique to `tagbuddy`, and none of them require a crate as lifetime-heavy
as this one to be useful. Most of them are just the habit of asking, each time you write
down a type parameter or a `Result`, whether you actually needed it.
