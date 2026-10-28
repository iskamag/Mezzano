# Instances

Instances are a flexible, efficient, and compact representation of user-defined objects.

All of `STRUCTURE-OBJECT`, `STANDARD-OBJECT`, and `FUNCALLABLE-STANDARD-OBJECT` are built on top of instances.

## Instances

An instance is a regular object pointer value with value tag `+TAG-OBJECT+`. They can be allocated in any area, depending on the layout.

An instance consists of a header word immediately followed by the instance slots.

The header word consists of the gc/spare bits at `(BYTE 2 0)`, the tag bits at `(BYTE 6 2)` contains either `+OBJECT-TAG-INSTANCE+` or `+OBJECT-TAG-FUNCALLABLE-INSTANCE+`, depending on if it's funcallable or not. The remaining bits contain a complete object pointer to the layout object for that instance. Since pointers on all targets are at most 56 bits wide, everything fits neatly.

Instance slots immediate follow the header and are described by that instance's `specific layout`, which is either specified directly by the layout in the header or indirectly if the instance has been made obsolete.

The `specific layout` is the `LAYOUT` object that exactly describes that instance, not a newer or older layout for the same class.

The header always refers to a `LAYOUT`. Superseding an instance installs a private copy of its layout with `NEW-INSTANCE` pointing to the replacement.

## Layouts

The `LAYOUT` object describes the layout of an instance.

It's a `sealed` structure-object allocated in the `wired` area with the following slots:

* `CLASS` - The class of the instance. What `CLASS-OF` would return.
* `OBSOLETE` - `NIL` if the layout is current and has not been made obsolete. Otherwise it points to a newer layout for the same class, which may itself also be obsolete.
* `HEAP-SIZE` - The size, in words, of the object, not including any trailing padding.
* `HEAP-LAYOUT` - Used by the GC to know which words of the object contain Lisp values and which contain unboxed data. Either `NIL` meaning no values, `T` meaning all words contain values to be scanned, or a `SIMPLE-BIT-VECTOR` the size of the object with a `1` bit indicating that word contains a Lisp value.
* `AREA` - Allocation area in which the object should be allocated.
* `INSTANCE-SLOTS` - A `SIMPLE-VECTOR` containing two adjacent entries per slot in the instance. The first entry is the name of the slot, the second entry is the `location` of the slot in the instance.
* `NEW-INSTANCE` - `NIL` for current storage, or the logical replacement for a superseded instance. Once published in an instance header, a non-NIL replacement link never changes.
* `COMPATIBLE` - A wired `SIMPLE-VECTOR` used for direct structure slot access.
  A structure layout records itself followed by its ancestor layouts, in
  order; a normal standard-object layout records only itself. A copied layout
  for a superseded instance shares the original vector.

Compatibility vectors describe storage, not just class membership. A direct
access must reject a superseded instance or any obsolete layout along the path
to the target layout. Structure `TYPEP` checks fall back to the class parent
chain for those stale paths; a complete, current vector without the target is
a fast negative result. When a structure's inheritance changes without a
storage change, it and its descendants keep their layouts and receive new
compatibility vectors.

When inherited slots change, structure definitions retain enough information
to rebuild their descendants' effective slots and layouts. The old layouts are
marked obsolete, and their slot metadata is kept until old instances have been
collected. Migration copies fixed-vector elements as well as scalar slots.
Constructors carry a version token so saved or inlined constructors can use a
slow path after their class changes; current constructors keep a fast check.
A sealed structure that includes an unsealed parent can still acquire a new
layout when that parent changes, so its compiled accesses must check for an
obsolete layout.

Layouts are allocated in the `wired` area since they are used by the GC. Allowing them to be movable would complicate the GC.

The first two words after the header in a funcallable instances are used to implement function calling. This and the different tag bits are the only differences between normal instances and funcallable instances. These two words can conceptually be thought of as hidden slots in the object, and code creating new layouts must be aware of them when computing the layout.

There's a one to many relationship between layout objects and instances, with many instances potentially sharing the same layout.

## Locations

A `location` is a type-carrying way to refer to a specific slot within a specific layout of an instance. It consists of a representation for the slot and the offset of the slot within the instance.

Since locations are quite internal to the system, they are represented as the two fields (representation and offset) packed into an integer.

The representation allows locations to be generic over the actual in-memory representation of a slot. Slots can be Lisp values, or they can be some kind of unboxed integer or float value.

`SLOT-DEFINITION-LOCATION`, `STANDARD-INSTANCE-ACCESS`, and `FUNCALLABLE-STANDARD-INSTANCE-ACCESS` are built on locations.

## Superseding instances

`SUPERSEDE-INSTANCE` allocates a private copy of the old storage's layout,
sets its `NEW-INSTANCE` link, and publishes it by comparing and swapping the
instance header. Shared class layouts are never modified by supersession.
This supports both class redefinition and `CHANGE-CLASS`.

Published replacement links are immutable. Repeated supersession follows the
chain to its current end and publishes another link there. A competing
publication makes the header comparison fail and the operation retries.
Readers follow the chain; GC shortens ordinary references to its current end.
This publication protocol does not make concurrent class redefinition or
migration hooks atomic: users of a class must still be quiescent during its
redefinition.

Funcallable replacements inherit the current function before publication.
After publication, the old callable storage forwards calls to its replacement.
The function getter and setter follow replacement links. The collector does
not copy callable state between instances, so collection order cannot restore
an older function.

## Slot access

Slot access separates logical identity from physical storage. Ordinary object
references identify the instance and may be snapped by GC. The internal
`+TAG-INSTANCE-STORAGE+` reference identifies the exact allocation whose layout
was used to select a slot offset. `%INSTANCE-STORAGE` creates this reference by
changing the pointer tag; it allocates nothing and adds no indirection.

The storage reference must be acquired **before** reading the layout or
selecting the offset. `%STORAGE-LAYOUT` and the `%STORAGE-REF-*` operations read
that allocation. A live storage reference keeps its contents available across
allocation, calls, and collection, including an intervening supersession.
It must not escape into ordinary Lisp object operations or user hooks.

`FETCH-UP-TO-DATE-INSTANCE-SLOTS-AND-LAYOUT` follows replacement links and
migrates obsolete layouts, returning physical storage and its layout.
`SLOT-LOCATION-USING-LAYOUT` resolves a slot name within that layout.
`STANDARD-INSTANCE-ACCESS` uses the storage and encoded location together.
The original logical object is retained for errors and user hooks such as
`SLOT-UNBOUND`.

Accessor cache entries couple the layout, slot location, and slot name.
A cache hit validates that layout against the acquired storage before using
its offset. Writers retain the storage through slot type checks. A stale
entry falls back to lookup by its own slot name. These paths are ordinary
inline Lisp compiled using the storage primitives. A discriminator remembers
its last matching weak cache cell, so repeated calls on the same class can
skip the hash probe. It rereads the cell's value on each call. Replacement,
removal, and clearing invalidate detached cells before publishing the new
table state; growing a table preserves its live cells. This avoids retaining
classes strongly or allowing saved discriminators to use detached entries.

For a constant slot name, compiled `SLOT-VALUE` and its setter allocate an
ordinary closure at load time for that call site. Each closure has its own
last-cell cache, avoiding interference between independent callers and the
funcallable-instance trampoline. Method lookup remains shared through the
anonymous generic function for the slot name. On a miss, the closure consults
that generic function's current table; if the table cannot supply an accessor
entry, it calls the generic function. Thus method changes invalidate existing
call sites through the same weak cells, including changes to dispatch mode.
The generated call needs no function-designator coercion: the load-time
factory always returns a closure. Calls allocate no additional cache objects.

Reading the current table of an exact standard generic function uses a
bootstrap slot offset guarded by the captured physical storage's layout.
Class precedence-list reads use the same protocol for standard and built-in
class metaobjects. Obsolete, superseded, and custom layouts retain their
ordinary lookup/MOP paths. This avoids repeated metadata slot-name searches
without caching a detached dispatch table across calls.

Compiled structure access follows the same rule for mutable layouts. Sealed
structures whose ancestry also has permanent layouts keep direct accesses;
those allocations cannot be superseded through class redefinition. When an
argument's type already proves that permanent layout, the compiler omits a
redundant layout test while retaining the argument evaluation and assertions.

## GC snapping

The collector distinguishes logical references from physical storage references.
It snaps logical references to the current replacement, allowing superseded
storage and removed slots to be reclaimed. Physical references instead retain
and relocate the original storage, preserving previously selected offsets.

Snapping an identity does not overwrite the old header with a forwarding
pointer to the replacement: another root may still name the old storage.
Copying the old storage does install a forwarding pointer to its physical
copy. A logical reference encountered afterwards follows that copy's layout
replacement link; a physical reference keeps the copy. Thus the result does
not depend on which kind of root the collector visits first. Once no physical
references remain, retaining the logical instance does not retain its old
slots.

## Structures

Since structure objects are also implemented using instances they get class redefinition effectively "for free", unlike most other Common Lisp implementations.

When a structure's included parent changes, its direct-subclass links, its
descendants' class precedence lists, and their layout compatibility vectors are
updated. Structure direct slots exclude slots inherited from the parent.
Cached generic-function and initarg information for the affected
classes is invalidated. Compiled type checks fall back to the current class
hierarchy for obsolete layouts, while compiled slot accesses use the checked
access path. Cyclic inheritance and incompatible descendant slot definitions
are rejected before the class graph changes. Other inherited-slot changes
rebuild descendant metadata, layouts, and affected constructor functions.
As with standard-class redefinition, these updates are not atomic with
respect to concurrent use of the affected classes. Callers must arrange for
other threads using those classes to be quiescent during redefinition.

## Tricky nonsense for unnecessary optimization

The two object tags (`+OBJECT-TAG-INSTANCE+` and `+OBJECT-TAG-FUNCALLABLE-INSTANCE+`) have been carefully chosen to be different by exactly one bit, so that it's cheap to test if an object is one of either kind. This only really matters for `CLASS-OF` and is an optimization that could be omitted.

A special quasi-immediate object type exists, the instance-header, which is a value that has identical layout to the header word of instance. This gives it a value tag of `+TAG-INSTANCE-HEADER+`. Due to the way that object headers and values are represented, this partially overlaps the value tag with the object tag in the header. The value tag and object tags are carefully chosen to allow this. `+TAG-INSTANCE-HEADER+` is also carefully chosen so that an object's header word can be read and trivially converted to be bitwise-identical to an instance header object if that object is an instance.

This allows for fast type checks when looking for a specific class of instance. Fast being 3 instructions.

## Glossary

`header word` - The very first word of an instance (or heap object in general), containing the object tag bits and reference to the direct layout object.

`direct layout` - The layout directly referenced by an instance's header word, including a private superseded layout.

`specific layout` - The `LAYOUT` object that exactly describes a specific instance, not a newer or older layout for the same class.

`sealed` - Of a class, no subclasses can be defined and the class cannot be redefined.

`area` - Kind of memory where an object can be allocated. May be pageable or non-pageable, and copying or non-copying.

`wired` - Stored in an allocation area that is non-pageable and not moved by the garbage collector.

`location` - Offset and representation of a slot within an instance.

`representation` - The in-memory storage format of a value. For example, a full tagged lisp object, an untagged single float, an untagged 64-bit unsigned integer, etc.

`value tag` - Tag bits stored in the low bits of a value.

`value` - Tagged pointer pointing to an object, cons, or directly containing an immediate value such as a fixnum.

`object tag` - Tag bits that exist in the header word of an object.

`object` - Any Lisp object or array that is stored on the heap, except for conses. Consists of a header word followed by the contents of the object. Conses have a special representation with no header word for efficiency.
