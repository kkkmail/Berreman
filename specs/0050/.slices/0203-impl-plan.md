# Step 0203 — impl-plan (IMPLEMENT: the copy's pure halves, renumbering and the reference closure)

## Approach

1. `Experiments.fs`: add `static member renumberedFor (taken : ExperimentId list) (incoming : Experiment list) :
   Experiment list` on `Experiment`. Fold the incoming list in order, carrying the ids given so far. An experiment
   keeps its id unless `taken` or an earlier given id holds it. Otherwise it takes the `next` of the highest id among
   `taken`, every incoming id and every given id. Only `id` changes; the order is kept.
2. `KitLibrary.fs`: move the depth-first walk out of `KitLibraryContext.snapshotReferences` into a free
   `referenceClosure resolve references`, declared before `KitLibraryContext`. It resolves each reference, skips a
   version already visited, and otherwise visits the envelope and pushes its `content.references` ahead of the rest.
   It answers the visited envelopes in visit order; the first error stops it.
   `snapshotReferences` keeps its own resolution (the kit's snapshot of a version, else the library's held or latest
   envelope, else the kit's highest), calls `referenceClosure` with it and pins and writes the versions the kit
   lacks, in visit order, as before.
3. Tests:
   - `ExperimentProxyTests`: `copied experiments keep their ids unless the kit holds them, and a clash takes the
     next free id`.
   - `KitLibraryTests`: `the closure of references walks each entry's references depth first, each version once`,
     over a resolver of `BuiltinCatalogue.catalogueData ()`'s envelopes.

## Risk: one closure over the whole list breaks an existing fact

`a reference the library and the kit cannot resolve records nothing` (its partial half) records an experiment whose
last top-level reference is unheld. It expects the earlier references' closures to be snapshotted and pinned before
the refusal. One closure over the whole list resolves everything before any write, so an unheld last reference would
write nothing.

So `snapshotReferences` calls `referenceClosure` once per top-level reference, in order. It writes each closure's
lacking versions before resolving the next reference, and skips the versions it has already written. The walk
itself exists only in `referenceClosure`. The visit order and the writes are those of the old single walk: a later
reference's subtree that meets a visited version finds that version's whole closure already held or written. I will
record this in the impl-log.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/Experiments.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/KitLibrary.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/ExperimentProxyTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitLibraryTests.fs`
