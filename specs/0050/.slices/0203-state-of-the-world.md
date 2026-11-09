# Step 0203 — state of the world (IMPLEMENT: the copy's pure halves, renumbering and the reference closure)

## Where we are

Spec 0050, section S6 (send and receive kits; copy experiments between kits).

- Steps 196–202 built the send side: the send rule, the archive seam and its zip writer, the picker, the job host
  and Send kit… in the kit bar.
- This step lands the two pure halves of Copy experiments from another kit…:
  - `Experiment.renumberedFor` decides the id each copied experiment is stored under. An experiment's file is `E<n>`
    (step 146), so a name clash is an id clash, and the next free id is S4.1's suffix rule.
  - `KitLibrary.referenceClosure` decides which library versions the copy needs: every reference and its own
    references, depth first, each version once.
- The copy's IO and its UI belong to later S6 steps.

## What's working

- Add `Experiment.renumberedFor`. A copied experiment keeps a free id, and a clash takes the next id after the highest
  taken, incoming or already given; only `id` changes.
- Move the depth-first walk of `snapshotReferences` into `KitLibrary.referenceClosure`, which answers the visited
  envelopes in visit order or the first error.
- `snapshotReferences` now calls `referenceClosure` once per top-level reference, then pins and writes the versions
  the kit lacks as before.
- Add facts for both: `copied experiments keep their ids unless the kit holds them, and a clash takes the next free
  id` and `the closure of references walks each entry's references depth first, each version once` (constructor
  unit tests 1259 → 1261).

## Tests

These are development runs, recorded in `0203-impl-log.md`. The gate engine gives the verdict (Invariant 6).

- **build:** exit 0, 0 warnings, 0 errors.
- **constructor-unit-tests:** 1261 passed, 0 failed (1259 → 1261, +2).
  - Both new facts are `Passed` in the TRX.
  - Every other `KitLibraryTests` fact passes as written.
- **elevation** (from the arc origin): `ELEV-GATE: OK`, 0 new findings in 12 projects.
- **The fourteen `invariant-s22-*` greps:** each finds nothing.

```yaml
gates:
  constructor_unit_tests: 1261
```

## Architecture

- **The reference walk exists once, in `referenceClosure`.**
  - It is a pure function of a `resolve` seam and a reference list.
  - `snapshotReferences` supplies the kit-first resolution and does the pinning and writing.
  - A copy between kits can supply the source kit's resolution to the same walk.
- **`snapshotReferences` closes each top-level reference separately.** Each closure is resolved before its writes, and
  versions already held or written are skipped.
  - This keeps the recorded behaviour that an unheld LATER reference leaves the earlier references snapshotted.
  - One closure over the whole list would have broken `a reference the library and the kit cannot resolve records
    nothing`.
- **`renumberedFor` is a static member of `Experiment`**, beside `libraryReferences`. It compares `ExperimentId`s
  structurally and reads no `.value`.

## Deferred

Nothing in this step's scope.

- The copy's IO (reading the other kit, writing the renumbered experiments, their setups and data files) and its UI
  belong to later S6 steps, by the spec's plan.
- Template names (S8.3, S9.1) carry the renumbering rule over to names in their own steps.

## Gotchas

- **Do not replace the per-reference `referenceClosure` call in `snapshotReferences` with one call over the whole
  list.** Recording relies on earlier references' snapshots surviving a later refusal.
- **An unheld reference deep in one top-level reference's closure now stops before that reference's own writes.** The
  old interleaved walk had written the envelopes above it. Earlier top-level references' writes are still kept.
- **`renumberedFor` counts every incoming id when it picks the next free id**, so a renumbered experiment never takes
  an id a later incoming experiment keeps (E1, E1, E2 → E1, E3, E2).

## Changelog

- 2026-11-09 — step 0203: `Experiment.renumberedFor`, `KitLibrary.referenceClosure` (the one depth-first walk, now
  called by `snapshotReferences`), and a fact for each.
