# Step 0220 — state of the world (WIRE: `AppContext.itemModels`)

## Where we are

Spec 0050, section S6 (send and receive kits; copy experiments between kits), lands the shared item contracts before
S7, so that copying an experiment can map its items (001 §H, §S6; S7.1).

- Steps 210–211 shipped the Built-in item models and the database item-model store.
- Steps 213–219 built the item, its kit file, its kit IO and `KitLibraryContext.recordItem`.
- This step wires the store into the app scope. `AppContext.compose` composes `ItemModelProxy.createInDatabase` over
  the app's library database, beside the other database stores, so `create` and `recreateLibrary` both carry it.
- Two later consumers resolve item models through `AppContext.itemModels`:
  - S6's item-copy resolution;
  - S7's Items panel ("Add item from library model…", 001 §O).
- No window reads it in this section.

## What's working

- Add `AppContext.itemModels`, the item-model store over the app's library database (spec 0050 S7.1). `compose` sets
  it to `ItemModelProxy.createInDatabase database`.
- The app scope serves the Built-in item models that the start-up import brought into its database:
  - it lists them ordered by name;
  - it resolves each one at its exact version.
- Add the `AppContextTests` fact `the app scope serves the Built-in item models from its library database`. A mutant
  wired to the in-memory store fails it.
- ui-tests over `AppContextTests` go from 15 to 16. ui-smoke is unchanged at 237.

## Tests

These are development runs, recorded in `0220-impl-log.md`. The gate engine gives the verdict (Invariant 6).

- **build:** exit 0, 0 warnings, 0 errors on the final source. The first build showed only the two existing vendored
  MathNet `SYSLIB0051` warnings.
- **ui-tests** (selection `OpticalConstructor.Ui.Tests.AppContextTests`):
  - 16 passed, 0 failed, 0 skipped (15 + 1);
  - the declared identity `the app scope serves the Built-in item models from its library database` is `Passed` in
    the TRX.
- **The fact bites:** wiring `ItemModelProxy.createInMemory ()` instead fails exactly that fact. The source was
  restored byte for byte and rebuilt.
- **ui-smoke:** 237 passed, 0 failed, 0 skipped, so the count is unchanged. 191 s against the 600 s
  `attempt_timeout_seconds` (manual 015).
- **elevation** (from the arc origin `f395597`, ELEV004 excluded):
  - `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, 199 s against a 600 s deadline;
  - suppressed counts are unchanged from step 0219 (Ui 479, total 1309).
- **The fourteen `invariant-s22-*` gates:** each holds.
- Not in this step's roster: constructor-unit-tests and unit-tests. No Domain, Storage or constructor test changed.

```yaml
gates:
  constructor_unit_tests: 1312
  ui_smoke_tests: 237
```

## Architecture

- **One library database, one set of stores.** The item-model store joins materials, samples, sources, categories,
  presets, backups and catalogue imports in `AppContext.compose`. All of them are built over the same
  `LibraryDatabase`.
  - What the start-up import writes, the item-model store reads, in every window of the scope.
  - A separate app scope composes its own store over its own database.
- **The field is composition only.** `ItemModelProxy.createInDatabase` (step 211) is the existing boundary-marked
  Storage extension.
  - Ui adds no IO and no behaviour.
  - The field is the record's elevated proxy type, so no primitive enters the surface.

## Deferred

Nothing in this step's scope. By the spec's plan:

- item-copy resolution through `AppContext.itemModels` is S6's experiment-copy work;
- the Items panel that lists and adds models is S7.2;
- the session's overlaid item-model store is step 222.

## Gotchas

- **The database store sorts by `(name, id)`.** The fact's expectation sorts by name. That is equivalent while the
  Built-in item model names are unique, which `ItemModelProxySqliteTests` already relies on.
- **`TestAppScope.freshAppContext` settles the start-up import.** A bare `AppContext.create` lists no item models until
  its pending Built-in import is settled, as for every other store.
- **`compose` is the only `AppContext` record literal.** Tests and windows that need a variant copy it with
  `{ … with … }`, so adding a field touches no other construction site.

## Changelog

- 2026-11-09 — step 0220: wired `AppContext.itemModels` (`ItemModelProxy.createInDatabase` over the app's library
  database) into `compose`; added the `AppContextTests` fact proving the app scope lists and resolves the Built-in item
  models; ui-tests over `AppContextTests` 15 → 16, ui-smoke unchanged at 237.
