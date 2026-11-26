# Step 0209 — state of the world (ADD_CONTRACT STORE_BigBoy_0006 ItemModelProxy)

## Where we are

Spec 0050, section S6 (send and receive kits; copy experiments between kits), is closing on its S7 prerequisite
(001 §H, §I, §S6): the shared item contracts land before S7, so that copying an experiment can map its items.

- Steps 205–208 gave `ItemModels.fs` the parameter vocabulary and the item-model forms.
- This step declares the item-model library store's seam: the entry, its identity, its errors, its proxy record with
  the in-memory double, and a contract suite that any store runs. `STORE_BigBoy_0006` is now `declared`.
- Step 210 makes the item model a library kind and ships the eleven built-ins under `ItemModelIds`. Step 211
  implements the contract over SQLite and runs the same suite.

## What's working

- Declare the item-model store seam in `ItemModels.fs`: `ItemModelId`, `ItemModelVersionId`, `ItemModelEntry`,
  `ItemModelVersion` and `ItemModelError`.
- Add `ItemModelProxy` (list the latest by name then id, resolve an exact version, save a new item model) and its
  in-memory double `ItemModelProxy.createInMemory`.
- Freeze the eleven built-in item-model identities in `module ItemModelIds`.
- Add the `ItemModelProxyContract` suite and `ItemModelProxyTests` (4 facts; constructor unit tests 1275 → 1279).

## Tests

These are development runs, recorded in `0209-impl-log.md`. The gate engine gives the verdict (Invariant 6).

- **build:** exit 0, 0 warnings, 0 errors.
- **constructor-unit-tests:** 1279 passed, 0 failed, 0 skipped (1275 → 1279, +4). All four new identities are
  `Passed` in the TRX under `OpticalConstructor.Tests.ItemModelProxyTests`.
- **elevation** (from the arc origin): `ELEV-GATE: OK`, 0 new findings in 12 of 12 projects (ELEV004 excluded).
- **The fourteen `invariant-s22-*` greps:** each finds nothing.

```yaml
gates:
  constructor_unit_tests: 1279
```

## Architecture

- **The store follows the functional-proxy convention** of `SourceProxy` and `CatalogueProxy`:
  - a `[<ReferenceEquality>]` record of `Result`-returning functions;
  - an in-memory double whose map is mutated only inside its closure.
- **One version per item model, never edited.** `saveItemModel` refuses a held id with `DuplicateItemModelErr` and
  changes nothing. A later version arrives only through the import seam (step 95). `resolveItemModelVersion` answers
  exactly the pinned version or `None`.
- **The listing order is by name, then by id.** It is the same key `SourceProxy.createInDatabase` sorts by, so step
  211's SQLite store can run the same contract.
- **`ItemModelEntry` is shaped like `PolarizerPreset`.** Its protection derives from its provenance at the file seam
  (step 210).

## Deferred

Nothing in this step's scope.

- By the spec's plan, not deferred by this step:
  - step 210: the library kind, its schema and import, and the shipped built-ins;
  - step 211: the SQLite create (`IMPLEMENT_CONTRACT STORE_BigBoy_0006`) and its `ItemModelProxySqliteTests`.

## Gotchas

- **`.contracts-json` says `declaringStep: 216` for `STORE_BigBoy_0006`, but this is step 209.** The registry belongs
  to the arc-runner and is not among this step's edits, so it was left alone. Its lifecycle (`declared`) is already
  right.
- **The test file is a namespace, not the brief's module.** The class `ItemModelProxyTests` cannot share its name
  with a module, and the census names the class. Every other contract suite here has the same shape.
- **`ItemModelVersionId` and `ItemModelVersion` both have a `version` field.** Give a bare record expression its type
  by annotation.
- **The in-memory double never answers `InvalidItemModelErr` or `ItemModelStoreUnavailableErr`.** It holds whatever
  it is given, built-ins included.

## Changelog

- 2026-11-09 — step 0209: declared `STORE_BigBoy_0006` `ItemModelProxy`. Added its identities, entry, errors and
  in-memory double to `ItemModels.fs`, the eleven built-in `ItemModelIds`, and the `ItemModelProxyContract` /
  `ItemModelProxyTests` suite (4 new facts).
