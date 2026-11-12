# 0331 impl-log — RETIRE_CONTRACT STORE_XDUO_0009 MuellerDataProxy

## Progress

- 2026-11-12 — Read the task, both system prompts, the project prompt, the slice and its `.gates` snapshot. No
  operator note and no operator rulings section apply to step 0331.
- Listed every hit of the four acceptance greps before editing: they were all in the six files in
  `effects.edits` and nowhere else under `Berreman/`. The archive (`data.zip`) has had no reference since step 330.
- The F# recipe tool for removals is not in this repository. As `retire_contract_worker.system-md` allows, I did the
  retirement by hand: Python string splices on exact anchors, writing LF.
- Domain: deleted `MuellerDataSet`, `MuellerArchivePath`, `MuellerDataProxy` and their docs, and the
  `ArchiveUnreadableErr` / `DataSetMissingErr` cases. `MuellerDataError` keeps `MalformedRowErr` / `EmptyFileErr`
  with a one-line doc. The seam's 28-line opening comment is now two lines, worded as the brief gives.
- Storage `MuellerDataStore.fs`: deleted the `type MuellerDataSet with` extension (`archiveEntryName`),
  `committedArchive`, `tryLoadFromArchive`, `createArchiveBacked`, `open System.IO` and `open System.IO.Compression`.
  Dropped the trailing comment on the `MuellerReconstruction` open, and cut the module doc to two lines.
  `CaptureCsvRow` and `DataFileText.parseMuellerCsv` are byte-identical. `open System` stays because the parser uses
  `Int32`, `Double`, `DateTimeOffset` and `String`.
- Storage `LabFileImport.fs`: the `PhoneRoiCsvProfile` arm of `ChosenImportProfile.mapText` lost its two archive
  arms and the inline comment about them. The `///` doc on `mapText` never mentioned them, so it is unchanged.
- `OpticalConstructor.Storage.fsproj`: deleted the four-line comment above `MuellerDataStore.fs`'s `<Compile>` and
  added nothing.
- Tests: in `MuellerReconstructionTests.fs`, deleted the five seam facts, `cannedRows`, `makeMockData`,
  `seededDataMock` and their comment block, plus `open System.IO` and `open System.IO.Compression`. Before dropping
  the opens I confirmed by grep that no other `System.IO` type is used. In `MuellerInverseTests.fs`, the
  ReferenceEquality comment now names `ExperimentDataProxy / MuellerSolverProxy` (comment only).

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/MuellerReconstruction.fs` (+3 / −80)
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/MuellerDataStore.fs` (+3 / −97)
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/LabFileImport.fs` (+1 / −4)
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/OpticalConstructor.Storage.fsproj` (−4)
- `Berreman/BerremanTests/MuellerReconstructionTests.fs` (−110)
- `Berreman/BerremanTests/MuellerInverseTests.fs` (comment, 1 line)

All six are in the step's `effects.edits`. Nothing outside them changed. `specs/0050/.manifest.state.json` was
already dirty before this round and belongs to the supervisor.

## Testing state

- **Gates not run.** `retire_contract_worker.system-md` (Invariant 6) says this role acts and runs no checks; the
  arc-runner's gate engine runs build, elevation, constructor-unit-tests, unit-tests and the 16 s32 invariants after
  the worker exits.
- **Acceptance greps.** I ran the four greps once while scoping, and once after the edits to confirm the deletion
  was complete. That is a check of my own edit, not a gate verdict.
- **Test effects.** `BerremanTests.MuellerReconstructionTests` declared 30 fact members at HEAD and declares 25 now.
  The five deleted members are exactly the five `delete` effects; every other fact is unchanged. No
  constructor-unit-tests source changed, so that suite's count is unchanged by construction.
- **Compile safety, by inspection.**
  - No symbol this step deleted is referenced anywhere under `Berreman/`.
  - `MuellerDataError`'s only remaining match, in `LabFileImport.fs`, is total over its two cases.
  - Every remaining identifier from the dropped `System.IO` / `System.IO.Compression` opens was inside deleted code.
- **Line endings.** Every edited file is LF, and `git diff --numstat` equals `--ignore-cr-at-eol`.

## Artifacts

None. A pure deletion produced no logs or captures.

## Gotchas

- **KEEP_NAME.** The step declares none, and no live member shares a word with the greps.
- **What still mentions the archive.** `the optics-mueller kit holds every file of the former data archive byte for
  byte` and its `archiveHashes` local remain. They are step 330's kit facts, which the brief keeps, and neither the
  name nor the local matches any grep.
- **Test count.** The 0330 TRX counted 30 for the selection. A raw `grep -c '[<Fact>]'` gives 31 at HEAD because a
  comment on line 57 contains the text `[<Fact>]`. The real count of fact members is 30 at HEAD and 25 now.
