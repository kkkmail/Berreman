# Step 0331 — state of the world

## Where we are

Step 0331 is the `RETIRE_CONTRACT STORE_XDUO_0009` step of spec 0050 section S9.4, where the optics-mueller lab files
move into the kit folder. Step 0330 moved the lab files out of `data.zip` into `data/optics-mueller/` and re-pointed
`MuellerReconstructionTests` at the kit through the Phone ROI CSV import profile. After that, nothing read the
archive-keyed load seam except the facts that tested it.

This step deletes the seam:
- the `MuellerDataSet` identity it was keyed by;
- the `MuellerArchivePath`;
- the two archive error cases;
- the archive store in Storage;
- the five facts that tested them.

That makes 001 §T's "From section 9" greps true. `parseMuellerCsv` stays, as the body of the `Phone ROI CSV`
profile.

## What's working

- Retired `MuellerDataSet`, `MuellerArchivePath` and `MuellerDataProxy` from `MuellerReconstruction`.
  `MuellerDataError` now has only `MalformedRowErr` and `EmptyFileErr`.
- Deleted the archive store from `MuellerDataStore`: `archiveEntryName`, `committedArchive`, `tryLoadFromArchive`,
  `createArchiveBacked`, and the `System.IO` / `System.IO.Compression` opens. `parseMuellerCsv` is unchanged.
- Reduced the Phone ROI CSV arm of `ChosenImportProfile.mapText` to the parser's two typed errors.
- Deleted the five archive-seam facts and their mocks from `MuellerReconstructionTests` (30 → 25 facts).
- The four §T greps (`MuellerDataProxy|…|tryLoadDataSet`, the `MuellerDataSet` case names, `Data/MuellerMatrix`,
  `DataFilePath`) find nothing under `Berreman/`.

## Tests

**No gate was run in this round.** `retire_contract_worker.system-md` (Invariant 6) says this role acts and runs no
checks. The arc-runner's gate engine runs this step's whole roster after the worker exits:
- build;
- elevation;
- constructor-unit-tests (all discovered);
- unit-tests over `BerremanTests.MuellerReconstructionTests`;
- the sixteen `invariant-s32-*` gates.

**Duration and deadlines.** I measured no development-run duration, so nothing can be reported as over its
deadline.

**Expected outcome, by inspection (not a gate result):**
- **unit-tests.** The selected class declares 25 fact members, down from 30 at step 0330. The five removed are
  exactly the step's five `delete` test effects; every other fact is unchanged.
- **constructor-unit-tests.** No source the suite compiles changed in a way that removes or adds a test, so its count
  is unchanged.
- **build.** No retired symbol is referenced anywhere under `Berreman/`. The only remaining match on
  `MuellerDataError` is total over its two cases, and the dropped `System.IO` opens served only deleted code.
- **elevation.** The step is deletions plus shorter comments, and it adds no declaration.
- **LF.** `git diff --numstat` equals `--ignore-cr-at-eol` for every edited file.

```yaml
gates:
  berreman_unit_tests: 25
```

## Architecture

- **Phone ROI CSV parsing.** It reaches the app only through the import profile. `ChosenImportProfile.mapText`
  (`PhoneRoiCsvProfile`) calls `DataFileText.parseMuellerCsv` and maps its two typed errors onto
  `MalformedDataFileErr` / `EmptyDataFileErr`.
- **No seam in its place.** Measured Mueller data is a kit file read through a profile (001 §R, §S9), so there is no
  load seam keyed by identity or by path, and no replacement, wrapper or alias.
- **`MuellerDataError` is now a parse error only.** Its cases are `MalformedRowErr` and `EmptyFileErr`, as its new
  one-line doc says.

## Deferred

Nothing. Every item in the brief (1)–(5), every contract effect, and every test effect landed in this step.

## Gotchas

- **Fact count.** `grep -c '[<Fact>]'` over `MuellerReconstructionTests.fs` over-counts by one, because a comment
  contains the attribute text. Count the `member _.` facts: 25 now.
- **"Archive" still appears in tests, on purpose.** `the optics-mueller kit holds every file of the former data
  archive byte for byte` and its `archiveHashes` local are step 330's kit facts and stay.
- **No removal tool.** The F# recipe-driven removal tool is not present, so I did the retirement by hand, as the
  retire-contract family prompt permits.

## Changelog

- 2026-11-12 — Step 0331 retired the archive-keyed Mueller load seam.
  - Domain: `MuellerDataSet`, `MuellerArchivePath`, `MuellerDataProxy` and the two archive error cases.
  - Storage: `archiveEntryName`, `committedArchive`, `tryLoadFromArchive` and `createArchiveBacked`.
  - Tests: the five facts that tested the seam (`MuellerReconstructionTests`, 30 → 25).
