# Step 0405 — impl log

## Progress

- [x] Domain: `EntryProvenance` gains `FitProvenance of EntryFitInfo`. `EntryFitInfo` joins the recursive group
      with `and`, as `{ kit : KitId; studyStem : FileStem; fitRunStem : FileStem }`.
- [x] Domain: `protection` answers a fit `UserManaged`, `categoryOrigin` answers it `UserCategory`, and
      `attributionOpt` answers it `None`.
- [x] Storage: `kindValue` answers 6 for a fit and `catalogueIdOpt` answers `None`.
- [x] Schema: `library-common.schema.json` gains `$defs/fitProvenance`, added to `provenance.oneOf`.
      Schema version stays 1.
- [x] Tests: the three widened facts (change_expectation only, no new identities).
- [x] Development runs: build, the whole constructor suite, elevation and the twenty s40 invariants.
      A mutation check showed the widened facts turn red.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/LibraryEntryFiles.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/LibraryDatabaseStore.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/schema/library-common.schema.json`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/LibraryEntryFilesTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/EntryFileJsonTests.fs`

These are exactly the step's authorized `edits`. No other file changed.

## Decisions

- **D1 — `FileStem` moved up in its own file.** `FileStem` was declared about 90 lines BELOW `EntryProvenance`
  in `LibraryEntryFiles.fs`. A record in the `EntryProvenance` recursive group cannot name a later type.
  - The type moved, unchanged and with its doc comment, to just above `EntryProvenance`.
  - It is self-contained: it uses only `FileExtension` / `FileName` from `Softellect.Sys.Primitives`, already in
    scope at the top of the module.
  - `EntryName.fileStem` (the extension right after it) stays where it was. It still follows `FileStem`.
  - The brief's "nothing is moved" is about `StudyName` / `FitRunName`, which stay where they are. This is a move
    inside the one file the step edits, with no change of shape or name.
- **D2 — the schema's stems are `minLength: 1` strings**, as the brief says ("non-empty strings").
  - The canonical serializer unwraps single-case unions. `KitImportProvenance`'s `KitId` is already a bare GUID
    in `Fields`, so `FileStem` is written as a bare string.
  - The round-trip fact reads the file back through `SchemaValidation.validateEntry`. Its pass confirms that the
    schema matches what the serializer writes.
- **D3 — one doc on `catalogueIdOpt`.** The member had no doc before. Its new doc states the contract: only a
  built-in names a catalogue, never a fitted entry (S11.5).
- **D4 — no other complete match needed a case.** A grep over product and test code found no other match on
  `EntryProvenance` that lists its cases. `CatalogueImport.fs:158` matches a tuple with a wildcard. The build
  (with `--warnaserror+:25`) reported no FS0025.
- **Test fixtures.**
  - `LibraryEntryFilesTests` has one private module-level `fittedInKit : EntryFitInfo`, declared before the
    first fact that uses it:
    - kit `8c4e2a6f-3b1d-4f7e-a9c5-2d6b8e0f4a13`;
    - `FileStem "QZ-LR"`;
    - `FileStem "QZ-LR@20261101T120000Z"`.
  - The protection fact lists it as a sixth provenance, so the lists widen to six.
  - The attribution fact adds it to `unlicensed`. That loop also checks a fork of each unlicensed provenance, so a
    fork of a fit answers `None` too.
  - `EntryFileJsonTests` round-trips a `fittedSilica` (silica under a fresh material id) under a `FitProvenance`
    of its own test-local `KitId` and the same two stems. `assertMaterialRoundTrips` checks the envelope equality,
    the decoded material and the provenance read back.

## Testing state

Development runs on 2026-11-14. These are not gate verdicts; the supervisor runs the gates.

- **build** (`dotnet build Berreman.slnx -c Release -nologo -v:m`):
  - exit 0, 0 errors;
  - only the exempt `NU1902` advisories;
  - the first build also shows vendored MathNet's `SYSLIB0051`, which is in the arc's logs since step 0001;
  - 81 s first, 25 s final, of 600 s;
  - logs `0405-local-build.log` and `0405-local-build-final.log`.
- **constructor-unit-tests** (whole suite, the gate's command):
  - 1726/1726 passed, the count unchanged from step 0404's 1726;
  - 156 s first; 159 s final (after the mutation restore and a forced Tests recompile), of 180 s;
  - log `0405-local-constructor-unit-tests.log`.
  - The TRX shows the three widened facts `Passed`:
    - `LibraryEntryFilesTests.a built-in provenance is protected and every other provenance is user-managed`;
    - `LibraryEntryFilesTests.a pulled entry and its forks carry its licence's attribution, and every other provenance none`;
    - `EntryFileJsonTests.every provenance round-trips through an entry file`.
- **elevation** (`Elevation/Run-ElevationGate.ps1 -ExcludeRules ELEV004`, no base, so all 13 projects):
  - `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)`, known findings 0;
  - 224 s of 600 s;
  - log `0405-local-elevation.log`.
- **invariant-s40-\*** (20): each ran its own gate command and printed `INVARIANT HOLDS ... found nothing`, in
  under 1 s each. Log `0405-local-invariants.log`.
- **Mutation check** (in `0405-mutation/`):
  - Three mutants ran in one build:
    - `FitProvenance _ -> ProtectedBuiltIn` in `protection`;
    - `FitProvenance _ -> Some (LicenceNameOnly …)` in `attributionOpt`;
    - `fitProvenance` dropped from the schema's `oneOf`.
  - The two classes ran against them: exactly the three widened facts failed and the other 37 passed. The
    round trip failed with `SchemaValidationErr`.
  - Both sources were restored from the `.orig` copies. `sha256sum -c` against the pre-mutation hashes said OK.
  - The Tests project was force-recompiled (a mtime touch) before the final build and suite, so no
    cross-assembly-inlined mutant code survives.

No run exceeded its effective deadline. The constructor suite used 159 s of its 180 s, as at step 0404 (154 s).

## Artifacts

All under `specs/0050/.artifacts/`:

- `0405-local-build.log` and `0405-local-build-final.log`;
- `0405-local-constructor-unit-tests.log`;
- `0405-local-elevation.log`;
- `0405-local-invariants.log`;
- `0405-mutation/`: the `.orig` backups, `orig.sha256` and `mutant-run.log`.

## Gotchas

- `FileStem` now sits above `EntryProvenance` (D1). A later step that needs another file-name type inside the
  provenance group must also be declared above it.
- The entry schemas stay at version 1 (.spec-md §7: no entry file is released). A stored entry with
  `FitProvenance` is valid only under the widened `library-common.schema.json`.
- No test asserts `kindValue = 6` / `catalogueIdOpt = None` for a fit. The step authorizes no
  `LibraryDatabaseTests` identity. The promotion that stores such an entry is step 407's.
