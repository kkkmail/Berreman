# Step 0211 — state of the world (IMPLEMENT_CONTRACT STORE_BigBoy_0006: the SQLite item-model store)

## Where we are

Spec 0050, section S6 (send and receive kits; copy experiments between kits), is landing its S7 prerequisite: the
shared item contracts arrive before S7, so that copying an experiment can map its items (001 §H, §I, §S6).

- Step 209 declared the `ItemModelProxy` seam, its in-memory double and its contract suite.
- Step 210 made the item model the seventh library kind and shipped the eleven built-ins.
- This step implements the seam over the library database: `ItemModelProxy.createInDatabase` (001 §C 'The library
  proxies keep their shapes and gain a SQLite create').
- Step 212 deletes step 210's throw-away exporter.

## What's working

- Add `ItemModelProxy.createInDatabase`, the SQLite item-model store over the library database's entry-version rows
  (`OpticalConstructor.Storage/ItemModelDatabaseStore.fs`).
- List every item model's latest version, built-in or user and whatever its lifecycle, by name and then id.
  Resolve exactly one version, and answer `None` for a row of another kind.
- Save user item models only. A held item-model id is `DuplicateItemModelErr`, an id held by another kind or a
  protected built-in is `InvalidItemModelErr`, and a database failure is `ItemModelStoreUnavailableErr`, never a
  throw.
- Add `ItemModelProxySqliteTests`: the three contract facts over an empty database per store, plus three
  Built-in-database facts (constructor unit tests 1281 → 1287).
- Mark `STORE_BigBoy_0006` implemented in `.contracts-json`.

## Tests

These are development runs, recorded in `0211-impl-log.md`. The gate engine gives the verdict (Invariant 6).

- **build:** exit 0, 0 errors, 70.7 s. The 2 warnings are vendored MathNet `SYSLIB0051` (manual 005).
- **constructor-unit-tests:** 1287 passed, 0 failed, 0 skipped (+6), 66 s, against a 180 s estimate. In the TRX,
  each of the six slice identities under `ItemModelProxySqliteTests` is `Passed`:
  - an item model is saved at its version and resolved by exactly that version;
  - the store lists the latest version of every item model, by name;
  - saving an item model the store already holds is DuplicateItemModelErr and keeps the held version;
  - the database item-model store lists the Built-in item models and resolves each by its version;
  - the database item-model store holds user item models only and refuses an id it cannot hold;
  - a database failure below the item-model store is ItemModelStoreUnavailableErr, never a throw.
- **elevation** (from the arc origin `f395597`, ELEV004 excluded): `ELEV-GATE: OK - 0 new finding(s) in 12
  project(s)`, 197 s, against a 600 s deadline.
- **The fourteen `invariant-s22-*` gates:** each holds.
- The UI suites are not in this step's roster. No UI file or shared fixture changed.

```yaml
gates:
  constructor_unit_tests: 1287
```

## Architecture

- **The item-model store is a thin view over the shared entry-version rows**, as the sample, source, category and
  preset stores are (steps 76 and 79).
  - Each field is one `LibraryDatabase.run`, and each write has one `SaveChanges ()`.
  - The envelope, hash and facet rows come from the existing `stageNewEntry` path. Nothing new was added to the
    database layer.
- **Built-ins and user item models are read alike, but only user item models are written here.**
  - `saveItemModel` seals under `UserProvenance`, so a `ProtectedBuiltIn` entry is refused by the builder.
  - A built-in arrives only through the import seam (step 95), which already stores every kind's envelope.
- **The held-id check comes before the builder.** Any entry under a held item-model id is `DuplicateItemModelErr`,
  whatever its protection.

## Deferred

Nothing in this step's scope. By the spec's plan, step 212 deletes `export-item-models.fsx`, and step 339 adds the
noise-budget parity fact.

## Gotchas

- **`.contracts-json` records `declaringStep` 216 for `STORE_BigBoy_0006`**, although slice 0209 declared it. I kept
  that metadata and set only `lifecycle` and `implementStep` (211).
- **The section invariants use `git grep`, which skips untracked files.** The new store file was checked directly
  against the same patterns.
- **An item-model id collision is reported in two ways.**
  - An id held by a non-item-model entry is `InvalidItemModelErr` naming that kind's extension (for example
    `.material-json`).
  - An id held by an item model is `DuplicateItemModelErr`.
- **`resolveItemModelVersion` answers `None` for a row of another kind**, not a decode error. Decoding such a row
  would be `EntryKindMismatchErr`, which would surface as `ItemModelStoreUnavailableErr`.

## Changelog

- 2026-11-09 — step 0211: added `ItemModelProxy.createInDatabase`, the SQLite item-model store (list, resolve, save
  user item models only, typed failures), and `ItemModelProxySqliteTests` (3 contract and 3 own facts; constructor
  1281 → 1287). Marked `STORE_BigBoy_0006` implemented.
