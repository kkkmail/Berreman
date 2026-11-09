# Step 0219 — state of the world (IMPLEMENT: `KitLibraryContext.recordItem`)

## Where we are

Spec 0050, section S6 (send and receive kits; copy experiments between kits), is landing the shared item
contracts. They must exist before S7, so that copying an experiment can map its items (001 §H, §S6; S7.1).

- Steps 213–216 built the item and `AddItem`.
- Step 217 gave the item its canonical `<label>.item-json` file.
- Step 218 read and wrote a kit's items through `KitProxy`.
- This step records an item. `recordItem` snapshots and pins the item model's exact version and its bound physics,
  each with its closure, and then writes the item's file.
- The Items panel (S7.2) and Publish calibration (S11.5) record items through this member.
- Step 222 adds the overlaid item-model store that reads those snapshots in a session.

## What's working

- Add `KitLibraryContext.recordItem`. It calls `snapshotReferences item.libraryReferences`, then
  `kit.proxy.writeItem`, so a written item never names a version the kit lacks.
- A failed write keeps the snapshots already taken, and a read-only kit records nothing.
- Add four `KitLibraryTests` facts covering standalone, stack-sample, source-preset and ideal-polarizer items:
  - snapshot and pin order, measured at the write;
  - each item's closure;
  - a read-only reopening against an empty library;
  - a missing model refused as `UnheldEntryErr`, with no preset fallback;
  - the read-only refusal.
- Share one `refusingContentWrites` test helper across the experiment, scene and item recording facts.
- Constructor unit tests: 1308 → 1312.

## Tests

These are development runs, recorded in `0219-impl-log.md`. The gate engine gives the verdict (Invariant 6).

- **build:** exit 0, 0 warnings, 0 errors on the final source.
- **constructor-unit-tests:** 1312 passed, 0 failed, 0 skipped (+4), in 66 s against a 180 s estimate. All four
  declared identities pass:
  - `recording an item snapshots its model's library version and pins it, then writes its file`;
  - `recording a sample item snapshots the sample with its materials and their categories`;
  - `recording a preset item snapshots the preset's latest version`;
  - `a read-only kit records no item`.
- **The facts bite.** Seven code mutations of `recordItem` were each caught, and the source was then restored byte for
  byte:
  - write before the snapshot;
  - snapshot the model only;
  - snapshot the binding only;
  - no snapshot;
  - no write;
  - write despite an unheld model;
  - swallow the refusal.
- **elevation** (from the arc origin `f395597`, ELEV004 excluded): `ELEV-GATE: OK - 0 new finding(s) in 12
  project(s)`, 197 s against a 600 s deadline. Storage's suppressed count is 150, unchanged from step 0218.
- **The fourteen `invariant-s22-*` gates:** each holds.
- The UI suites are not in this step's roster. No UI file or shared fixture changed.

```yaml
gates:
  constructor_unit_tests: 1312
```

## Architecture

- **Recording is one shape for every kind of kit content.** `recordExperiment`, `recordScene` and `recordItem` each
  call `snapshotReferences` over the content's `libraryReferences` and then the typed `KitProxy` writer.
  - The read-only refusal and the closure walk live once, in `snapshotReferences` (steps 139, 142).
  - For an item, the references are the model's exact version and then its binding (step 215): a preset at its
    latest, or a sample at its exact version.
- **A missing model stops the record before its binding is touched.** The model reference comes first in
  `Item.libraryReferences`, so a library that holds the preset but not the model answers `UnheldEntryErr` naming
  the model. Nothing is snapshotted, pinned or written. The preset never stands in for the model.
- **The model's snapshot is self-contained.** An `.item-model-json` envelope has no references, so a standalone
  item's closure is its model alone. That snapshot decodes back to the shipped model (`toItemModel`, step 210) with
  no library present.

## Deferred

Nothing in this step's scope. By the spec's plan:

- the session's overlaid item-model store (`ItemModelProxy.overlaidWith`) is step 222;
- the Items panel that calls `recordItem` is S7.2;
- Publish calibration is S11.5.

The brief allows no other member, so none was added.

## Gotchas

- **`observeContentWrites` asserts cumulatively** (pins and operations since the fixture was made). A second item with
  new snapshots on the same fixture needs a new observed context over the cumulative expectation.
- **`readItems` sorts ordinally,** so `LP1` comes before `Laser`.
- **The reopen check decodes the model snapshot directly.** Step 222's overlaid store will be the session's resolver
  over the same snapshot.
- **`KitLibraryTests` inlines `tryToLibraryEntryId |> Option.defaultWith …` six times** in pre-existing facts. The new
  private `presetEntryId` names it for the item facts only.

## Changelog

- 2026-11-09 — step 0219: added `KitLibraryContext.recordItem` (snapshot and pin the model and its physics closure,
  then write the item file); added four `KitLibraryTests` facts and the shared `refusingContentWrites` helper;
  constructor unit tests 1308 → 1312.
