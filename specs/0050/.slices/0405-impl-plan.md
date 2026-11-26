# Step 0405 — impl plan

A promoted library entry needs a provenance that names where it was fitted (spec 0050 S11.5; 001 §I, §L).
None of the five `EntryProvenance` cases fits, so the union gains a sixth, `FitProvenance of EntryFitInfo`,
with `EntryFitInfo = { kit : KitId; studyStem : FileStem; fitRunStem : FileStem }`.

## Files

1. `OpticalConstructor.Domain/LibraryEntryFiles.fs`
   - `EntryProvenance` gains `FitProvenance of EntryFitInfo`; `EntryFitInfo` joins the recursive group with `and`.
   - `protection` → `UserManaged`, `categoryOrigin` → `UserCategory`, `attributionOpt` → `None` (a fit carries no licence).
   - `FileStem` is declared below `EntryProvenance` today (it is self-contained: only `FileExtension` / `FileName`
     from Softellect.Sys). It moves, unchanged, to just above `EntryProvenance` so the record can name it.
     This is a move inside the one file the step edits, not a change of shape.
2. `OpticalConstructor.Storage/LibraryDatabaseStore.fs` — `kindValue` 6, `catalogueIdOpt` `None`.
3. `OpticalConstructor.Storage/schema/library-common.schema.json` — a `fitProvenance` definition (Case/Fields,
   one object: `kit` guid, `studyStem` / `fitRunStem` strings with `minLength: 1`), added to `provenance.oneOf`.
   Schema version stays 1.
4. Tests (change_expectation only, no new identities):
   - `LibraryEntryFilesTests.a built-in provenance is protected and every other provenance is user-managed` —
     a `FitProvenance` of a test-local `KitId` with `FileStem "QZ-LR"` / `FileStem "QZ-LR@20261101T120000Z"`
     is `UserManaged` and `UserCategory`.
   - `LibraryEntryFilesTests.a pulled entry and its forks carry its licence's attribution, and every other provenance none` —
     the fit provenance (and a fork of it, by the existing loop) answer `None`.
   - `EntryFileJsonTests.every provenance round-trips through an entry file` — a material under that
     `FitProvenance` round-trips and decodes to the material.

## Risks

- The serializer's shape for the record: the canonical stack writes single-case unions unwrapped (KitId is a bare
  GUID in `KitImportProvenance`), so `FileStem` is a bare string; the round-trip test validates the schema on read.
- Any other complete match on `EntryProvenance` (FS0025, an error under `--warnaserror+:25`): grep shows only the
  domain members and the store's two members; the build confirms.
