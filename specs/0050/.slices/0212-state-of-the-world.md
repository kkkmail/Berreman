# Step 0212 — state of the world (IMPLEMENT: item-model entry file and import facts; the exporter goes)

## Where we are

Spec 0050, section S6 (send and receive kits; copy experiments between kits), closes its S7 prerequisite: the
shared item contracts land before S7, so that copying an experiment can map its items (001 §H, §S6).

- Step 210 made the item model the seventh library kind and shipped the eleven built-ins through a one-time
  exporter.
- Step 211 implemented the SQLite `ItemModelProxy`.
- This step proves the item-model entry file over every form, and the import of item models over both stores.
  It also deletes the exporter, as step 36 deleted step 35's. No product source file changed.

## What's working

- Delete the throw-away `Catalogue/export-item-models.fsx`; `Berreman/OpticalConstructor/Catalogue/` now holds
  only `Builtin/`.
- Add `an item-model entry file round-trips every model form`: every `ItemModelForm` and `OpticalForm` case
  round-trips under the built-in and user provenances.
- In that fact, check each detector form's noise axes:
  - the int kind tags on disk;
  - the three defaults kept;
  - a tag of 0, an unrecognised tag or a case-name string refused as `MalformedJsonErr`.
- In that fact, refuse the real LP's file read as a polarizer with `SchemaValidationErr`.
- Give `ImportStoreSet` an `itemModels : ItemModelProxy` (in-memory, and SQLite via `createInDatabase`).
- Make both import classes prove that `listItemModels` holds one version per item-model row and that
  `resolveItemModelVersion` answers each row's decoded version.
- Constructor unit tests: 1287 → 1288.

## Tests

These are development runs, recorded in `0212-impl-log.md`. The gate engine gives the verdict (Invariant 6).

- **build:** exit 0, 0 warnings, 0 errors.
- **constructor-unit-tests:** 1288 passed, 0 failed, 0 skipped (+1), 65 s against a 180 s estimate. In a
  filtered TRX, each named identity is `Passed`:
  - `EntryFileJsonTests.an item-model entry file round-trips every model form`;
  - `importFrom fills empty stores with the whole Built-in catalogue`, under `CatalogueImportInMemoryTests` and
    `CatalogueImportSqliteTests`;
  - `every imported entry resolves by its id to its decoded value`, under both classes.
- **A mutation check confirms that the new import assertions bite.** With an empty item-model store, both
  in-memory facts fail. The file was then restored.
- **elevation** (from the arc origin `f395597`, ELEV004 excluded): `ELEV-GATE: OK - 0 new finding(s) in 12
  project(s)`, 194 s against a 600 s deadline.
- **The fourteen `invariant-s22-*` gates:** each holds.
- The UI suites are not in this step's roster. No UI file or shared fixture changed.

```yaml
gates:
  constructor_unit_tests: 1288
```

## Architecture

- **No architectural change.** The step is test-side plus one deletion.
- **The detector noise axes in the new fact are read from the shipped Mueller polarimeter, never transcribed.**
  The zero variant is the same descriptors with `ParameterValue 0.0`. No test binds a `NoiseParam`.
- **Reflection guards keep the fact's coverage honest.** They compare the forms used against every union case of
  `ItemModelForm` and `OpticalForm`, so a new case fails this fact until it is covered.

## Deferred

Nothing in this step's scope. By the spec's plan, step 339 adds the parity fact between the noise budget and the
catalogue defaults; that is not deferred by this step.

## Gotchas

- **The brief's form list leaves out the three binding forms.**
  - They are covered inside the same fact, so "every model form" holds literally.
  - No new test identity was added.
- **`assertEmptyStores` is unchanged.** It is shared by facts outside this step's effects. The item-model
  assertions live in the two facts the step names.
- **F# rejects an index chain ending in a method call** (`x["a"][0].AsArray()`) with FS0597. Write it as
  `x.["a"].[0].AsArray()`.
- **The item-model schema leaves the payload form free.** A corrupted noise `kind` tag therefore reaches the
  reader's `ParameterKind.tryOfWire` check (`MalformedJsonErr`) before the hash check.

## Changelog

- 2026-11-09 — step 0212: deleted `export-item-models.fsx`; added the item-model round-trip fact (every form, both
  provenances, detector kind tags and refusals, wrong-kind schema refusal); gave the import contract an item-model
  store and extended its two facts over both stores (constructor 1287 → 1288).
