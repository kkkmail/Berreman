# Step 0210 — state of the world (IMPLEMENT: the item model becomes a library kind)

## Where we are

Spec 0050, section S6 (send and receive kits; copy experiments between kits), is landing its S7 prerequisite: the
shared item contracts arrive before S7, so that copying an experiment can map its items (001 §H, §I, §S6).

- Steps 205–209 built the item-model vocabulary, the forms and the `ItemModelProxy` store seam.
- This step makes the item model the seventh library kind, end to end: its file, its schema, its import, and the
  eleven built-in item models shipped in `Catalogue/Builtin/`.
- Step 211 implements `ItemModelProxy` over SQLite; step 212 deletes this step's throw-away exporter.

## What's working

- Add the item-model library kind: `ItemModelEntryKind` (tag 7, `.item-model-json`, schema v1), `ItemModelPayload`,
  `ofItemModel` / `toItemModel`, `ItemModelId.toLibraryEntryId` and `ItemModelVersionId.entryReference`.
- Validate item-model files with `library-item-model.schema.json`, and store a detector's `ParameterKind` as its
  int tag (`tryOfWire` on read, `MalformedJsonErr` for an unknown tag).
- Push imported item models through `ItemModelProxy.saveItemModel`, in `InMemoryCatalogueStores.itemModels`, after
  the detectors; the shipped-item-models fact checks the in-memory import lists and resolves them.
- Ship the eleven built-in item models in `Catalogue/Builtin/` through the one-time `export-item-models.fsx`, with
  the detector noise budget taken from `NoiseParam.decentLab`.
- Add two facts (the shipped item models; the item-model builder and decoder) and extend the integrity, wire-tag
  and import-tape facts (constructor unit tests 1279 → 1281).

## Tests

These are development runs, recorded in `0210-impl-log.md`. The gate engine gives the verdict (Invariant 6).

- **build:** exit 0, 0 warnings, 0 errors.
- **constructor-unit-tests:** 1281 passed, 0 failed, 0 skipped (+2). All five named identities are `Passed` in the
  TRX.
- **ui-smoke:** 237 passed, unchanged from the checkpoint.
- **ui-tests** (the selected `AppContextTests` and `ImportDialogTests`): 27 passed, 0 failed. Neither class was
  changed. The whole UI suite, outside the selection, passed 655 as an extra regression check.
- **elevation** (from the arc origin, ELEV004 excluded): `ELEV-GATE: OK`, 0 new findings in 12 of 12 projects.
- **The fourteen `invariant-s22-*` gates:** each holds.
- **Attempt 2** extended one fact only (the in-memory import's item models), then re-ran build (0 warnings),
  constructor-unit-tests (1281 passed, unchanged) and elevation (`ELEV-GATE: OK`). The UI suites' inputs did not
  change.

**Relaunch after the ui-smoke deadline recovery** (manual 015; operator commits `90ab50b` and `965cdae`). No product,
test, schema or catalogue file changed, and the exporter was not rerun. Measured on 2026-11-09 over the committed
tree, against the deadlines in the `0210.gates` snapshot:

- **build:** exit 0, 0 errors, 72 s. Its two warnings are vendored MathNet's SYSLIB0051 (as in manual 005); the
  incremental rebuild reported 0 warnings.
- **constructor-unit-tests:** 1281 passed, 0 failed, 0 skipped, 63.5 s (deadline 180 s). The five named identities
  are each `Passed`.
- **ui-smoke:** 237 passed, 0 failed, 0 skipped, **193.8 s**. That is over the former 180 s deadline (the cause of
  the 0210 escalation) and inside the approved **600 s** `attempt_timeout_seconds`.
- **ui-tests** (`AppContextTests`, `ImportDialogTests`): 27 passed, 0 failed, 35.6 s (deadline 360 s).
- **elevation:** `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, 189 s (deadline 600 s).
- **The fourteen `invariant-s22-*` gates:** each holds.
- These counts are the expected checkpoint 209 (constructor 1281, smoke 237). The supervisor's normal review and
  19-gate cycle are still to run.

```yaml
gates:
  constructor_unit_tests: 1281
  ui_smoke_tests: 237
```

The ui-tests count (27) is only the two selected classes, so it is not a suite baseline.

## Architecture

- **One entry hash, over the file's own content.**
  - `EntryFileJson` builds an entry's file node from the canonical stack (`nodeOf`, `CanonicalJson.options`) and
    turns each detector noise axis's `ParameterKind` into its int `value`.
  - `sealed`, `fileText` and `parseEntry`'s check all hash that one node.
  - Every other kind's node is unchanged, so the 61 existing files are byte-identical and keep their hashes.
- **An item model is a library entry, but not a library preset or a sample.**
  - The kit overlay, the preset database view and facet rows all skip it.
  - It references no other entry: its components are inline forms.
  - The in-memory import seam is the only seam that decodes it into a store, for now. The database seam stores its
    envelope, as it does for every kind; step 211 adds the SQLite `ItemModelProxy`.
- **The built-ins are data.** The eleven files were sealed once through the product writer under
  `BuiltInProvenance CatalogueIds.builtInCatalogue` at version 1. No code seed exists, and the catalogue keeps
  version 1 (no release exists).

## Deferred

Nothing in this step's scope.

- By the spec's plan, not deferred by this step:
  - step 211: the SQLite `ItemModelProxy` (`IMPLEMENT_CONTRACT STORE_BigBoy_0006`);
  - step 212: deleting `export-item-models.fsx`;
  - step 339: the parity fact between the promoted noise budget and these catalogue defaults.

## Gotchas

- **Five files outside the listed effects changed, each forced by this step's own requirements.** Details are in
  the impl log, Decisions 3.
  - Exhaustive kind matches (FS0025): `KitLibrary.fs`, `PresetDatabaseStore.fs`, `CatalogueImportInMemoryTests.fs`.
  - `library-catalogue.schema.json`: its row `kind` maximum went from 6 to 7.
  - `EntryFileJsonTests.fs`: two six-kind fixtures. The invalid row is now tag 8, and the "built-ins form one
    catalogue" fact includes the item models.
- **For a detector item model, `contentHashOf content` is not its entry hash.** Use `content.sealed`, `fileText` or
  `parseEntry`.
- **A script that reads library files under `dotnet fsi` must set `APP_CONTEXT_BASE_DIRECTORY`** to a folder holding
  `schema/`.
- **`export-item-models.fsx` is not idempotent.** A second run is refused with `DuplicateEntryVersionErr`.
- **ui-smoke takes about 190–194 s.** That passes only under the 600 s deadline manual 015 approved; the runtime
  estimate (180 s) is no longer its deadline.

## Changelog

- 2026-11-09 — step 0210: made the item model the seventh library kind (`.item-model-json`, schema, int
  `ParameterKind` tags, import through `saveItemModel`). Shipped the eleven built-in item models via the one-time
  exporter, and extended the catalogue, wire-tag and import facts (+2 facts).
- 2026-11-09 — step 0210, attempt 2: the shipped-item-models fact now imports the catalogue into the in-memory seam
  and checks `listItemModels` holds exactly the shipped item models and `resolveItemModelVersion` answers one at
  version 1 (no new test identity; 1281).
- 2026-11-09 — step 0210, relaunch after the ui-smoke deadline recovery (manual 015): no source change. Rebuilt and
  re-measured: constructor 1281, smoke 237 in 193.8 s under the 600 s deadline, selected UI 27, elevation OK,
  invariants hold.
