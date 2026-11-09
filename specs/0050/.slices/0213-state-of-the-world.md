# Step 0213 — state of the world (IMPLEMENT: an item's identity and the model it instantiates)

## Where we are

Spec 0050, section S6 (send and receive kits; copy experiments between kits), is landing the shared item
contracts. They must exist before S7, so that copying an experiment can map its items (001 §H, §S6; S7.1;
001 §I).

- Steps 205–212 made the item model a library kind with its own store, built-ins and entry file.
- This step adds the pure Domain identity of one physical piece to the `Kit` module: its error union, its id,
  label, notes and revision, and the exact item-model version it instantiates, with its optional typed physics
  binding.
- Step 214 adds parameter states after `ItemModelInstance`, step 215 adds the `Item` record, and step 216 adds
  `AddItem`.

## What's working

- Add `Kit.ItemError` with seven reason-carrying cases, each rendered by `describe`. `InvalidItemModelBindingErr`
  and `UnknownKitItemErr` are included, for AddItem and the S6 copy mapping.
- Add `ItemId`, `ItemLabel`, `ItemNotes` and `ItemRevision`:
  - `ItemLabel.tryCreate` refuses a blank label, and `fileStem` follows the shared `FileStem.ofText` rule;
  - `ItemRevision` counts up from `first` (1) through `next`.
- Add `ItemModelInstance` (`PresetInstance` | `SampleInstance` | `ItemModelVersionInstance`). Every case carries
  its exact `ItemModelVersionId`:
  - `entryReferenceOpt` is always that version's `entryReference`;
  - `bindingReferenceOpt` names the preset (latest GUID) or the sample (exact version) by step 137's rules, and
    is `None` for a standalone model.
- Add `KitItemTests` with four facts. Constructor unit tests: 1288 → 1292.

## Tests

These are development runs, recorded in `0213-impl-log.md`. The gate engine gives the verdict (Invariant 6).

- **build:** exit 0, 0 warnings, 0 errors.
- **constructor-unit-tests:** 1292 passed, 0 failed, 0 skipped (+4), in 65 s against a 180 s estimate. Each of
  the four declared identities passes:
  - `an item label keeps its text, refuses a blank one and takes its file stem from the shared name rule`;
  - `an item revision starts at one and counts up`;
  - `an item's model is referenced as the library snapshots it`;
  - `every item error renders its condition and its payload`.
- **A mutation check confirms that the binding fact bites.** With the preset binding forced to `None`, it fails.
  The source was then restored.
- **elevation** (from the arc origin `f395597`, ELEV004 excluded): `ELEV-GATE: OK - 0 new finding(s) in 12
  project(s)`, 190 s against a 600 s deadline.
- **The fourteen `invariant-s22-*` gates:** each holds. The untracked new test file was also grepped directly.
- **Attempt 02** repaired the test oracle's two cross-type re-wraps and changed no product file. It re-ran build
  (0 warnings), `KitItemTests` (4/4), the whole constructor suite (1292 passed, 1 min 05 s) and elevation from
  the arc origin (OK, 0 new findings in 12 projects, 197 s).
- The UI suites are not in this step's roster. No UI file or shared fixture changed.

```yaml
gates:
  constructor_unit_tests: 1292
```

## Architecture

- **Item identity is pure Domain data in `Kit`, appended at the end of the module in the brief's order.**
  `Kit.fs` now opens `Placement`, `Library` and `ItemModels`, in compile order.
- **A model reference and a physics binding are separate answers.**
  - The kit always snapshots the item's exact item-model version.
  - Separately, a source / ideal-polarizer / sample item names its physics dependency.
  - No preset or sample stands in for the model.
- **A preset id that is not a GUID yields no binding reference, but never erases the model reference.** Step 216's
  `addTo` turns it into `InvalidItemModelBindingErr`.

## Deferred

Nothing in this step's scope. By the spec's plan:

- parameter states and `InvalidParameterStateErr`'s producer are step 214;
- the `Item` record and `libraryReferences` are step 215;
- `AddItem` and `InvalidItemModelBindingErr`'s producer are step 216;
- the copy mapping that produces `UnknownKitItemErr` is step 221.

None of these is deferred by this step.

## Gotchas

- **The sample binding builds its `LibraryVersionRef` in `Kit.fs`.** `VersionRef.entryReference` (`Lifecycle.fs`)
  compiles later and lies outside this step's edits, so it cannot be called. The construction reuses
  `SampleId.toLibraryEntryId`, as `LibraryEntryFiles.fs` and `KitLibrary.fs` do.
- **`InvalidItemModelBindingErr`'s text is not given by the brief.** It renders as
  `the item model binding is not valid: <reason>`.
- **`FileName` comes from `Softellect.Sys.Primitives`.** A test naming it opens that namespace.
- **`git grep` invariants skip untracked files.** Check a new file directly until it is committed.
- **A test oracle reaches another type's id through the source type's named member** (`x.toLibraryEntryId`),
  never through `LibraryEntryId x.value`. AGENTS.md's re-wrap ban covers fixtures too, and the ELEV gate does not
  scan tests, so nothing but review catches it.

## Changelog

- 2026-11-09 — step 0213: added `Kit.ItemError`, `ItemId`, `ItemLabel`, `ItemNotes`, `ItemRevision` and
  `ItemModelInstance` (exact model version plus a separate preset/sample binding reference); added `KitItemTests`
  (4 facts); constructor unit tests 1288 → 1292.
- 2026-11-09 — step 0213 attempt 02: `KitItemTests` now builds the expected model and sample entry ids through
  `ItemModelId.toLibraryEntryId` / `SampleId.toLibraryEntryId` instead of re-wrapping `.value`; assertions and
  fact names unchanged; constructor unit tests still 1292.
