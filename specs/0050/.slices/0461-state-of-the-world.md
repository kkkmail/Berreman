# Step 0461 — state of the world

## Where we are

Spec 0050, section S13.2 ("PTB OCDB, the Henke compound calculator and glass catalogues"). This step gives an entry
imported from a user's glass-catalogue file a provenance of its own, `FileImportProvenance`. It records the file's
leaf name and the SHA-256 of its bytes, so the glass import (step 463) has a case to build. The case is user-managed,
carries no attribution, is stored as provenance kind 7 with no catalogue, and round-trips through an entry file under
`library-common.schema.json`.

## What's working

- Added `FileImportProvenance of EntryFileImportInfo` (the file's leaf name and its bytes' SHA-256) as
  `EntryProvenance`'s seventh case.
- Made a file-imported entry user-managed, in the user category, with no licence attribution.
- Stored the new provenance as kind 7 with no catalogue and no pulled-source row.
- Added the `fileImportProvenance` shape to `library-common.schema.json`'s provenance `oneOf`; schemas stay at version 1.
- Widened the three provenance facts to cover a file import; the suite count is unchanged at 1828.

## Tests

These are local development runs; the supervisor re-runs the roster.

- **build:** green, 83 s against a 600 s deadline. No `FS` warning. The only warnings are the exempt `NU1902` and the
  vendored MathNet `SYSLIB0051`, which predates this step.
- **constructor-unit-tests:** green, 1828 of 1828, 104 s against a 180 s deadline. That is the 0460 checkpoint count,
  unchanged.
- **elevation:** `ELEV-GATE: OK`, 0 new findings in 13 projects, 227 s against a 600 s deadline.
- **invariant-s44-\* (21):** all hold.

```yaml
gates:
  constructor_unit_tests: 1828
```

## Architecture

- `EntryFileImportInfo` is declared with `and` beside `EntryForkInfo` and `EntryFitInfo`, the shape step 405 set.
  Its fields are elevated: `FileName` from STL and `ContentHash`, whose `ofBytes` is the app's one SHA-256 body.
- Every complete match on `EntryProvenance` answers a file import as it answers `UserProvenance`. That covers
  `protection`, `categoryOrigin`, `attributionOpt`, `catalogueIdOpt`, `pulledSourceRows`, `pulledRecordsOf` and the
  pulled-source row check. Only `kindValue` distinguishes it, with 7.
- The schema shape mirrors what the canonical options write: `{"Case":"FileImportProvenance","Fields":[{"file":…,"fileHash":…}]}`,
  with `fileHash` on the shared `contentHash` def.

## Deferred

Nothing. Building the case from an `.agf` import is step 463's brief, not a deferral.

## Gotchas

- FS0025 reported two complete matches outside `effects.edits`: `CatalogueImport.fs` and `CatalogueImportStore.fs`.
  The how_to's clause authorizes them, and each grew one alternative.
- A wildcard match (`LauncherTests.fs`) needs no change.
- A future provenance case touches the same seven matches, plus the schema def and the `oneOf` entry.

## Changelog

- 2026-11-17 — step 0461: `FileImportProvenance` / `EntryFileImportInfo` added (user-managed, unattributed, kind 7,
  schema shape); the three provenance facts widened; gates green locally.
