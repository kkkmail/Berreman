# Step 0330 — state of the world

## Where we are

Spec 0050 section S9.4 moves the optics-mueller lab files into the kit folder (001 §Q AT-1, §S9; `.spec-md` §3.20,
§3.21).

- Step 329 moved the reference pipeline into `data/optics-mueller/data/reference/`.
- This step moves the measured data out of `Berreman/Data/MuellerMatrix/data.zip` into the kit byte for byte.
  `MuellerReconstructionTests` now reads it there, read-only, through the Phone ROI CSV import profile.
- After this step nothing reads `data.zip`, which is deleted with `Berreman/Data/`.
- Step 331 deletes the archive-keyed seam (`MuellerDataSet`, `MuellerDataProxy`, `tryLoadFromArchive`,
  `createArchiveBacked`) and the five facts that still test it.

## What's working

- Moved the 15 lab CSVs of `data.zip` into `data/optics-mueller/data/raw/` byte for byte, and its three beam shapes
  into `data/reference/shapes/`. Deleted the archive and `Berreman/Data/`.
- Kept the lab files exactly as recorded with a `data/optics-mueller/data/raw/** -text !eol` rule in `.gitattributes`,
  noted in AGENTS.md's line-endings gotcha.
- Added the `CommittedKit` locator (`OpticsMuellerKit.tryFindFolder`), which finds the kit from the repository root.
- `MuellerReconstructionTests` reads the kit read-only through the Phone ROI CSV profile; its numbers are unchanged.
- Renamed the row-count fact for the kit, and added a fact pinning all 18 files to their former archive SHA-256.
- Updated the kit README to describe the data where it now lives.

## Tests

These are development runs of each gate's own command from `0330.gates`, over the final tree. The arc-runner's gate
engine runs the gates themselves.

| Gate | Result | Time | Deadline |
|---|---|---|---|
| build | exit 0, 0 errors | 77 s | 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`; known findings 0; ELEV004 excluded | 205 s | 600 s |
| unit-tests (`BerremanTests.MuellerReconstructionTests`) | Passed!, 30 of 30, 0 failed, 0 skipped | 3 s | 7200 s |
| invariant-s32-\* (16) | each `INVARIANT HOLDS`, exit 0 | under 1 s each | 300 s |

- **build.** Only the two known `SYSLIB0051` warnings from the vendored MathNet `Exceptions.cs`, as in steps 320–329.
  No file this step touched produced a warning.
- **unit-tests.** The selection counted 29 at step 0329 and counts 30 now: the rename keeps one, and
  `the optics-mueller kit holds every file of the former data archive byte for byte` adds one.
  - The TRX lists both new names, and the old name is absent. It is in `.artifacts/0330-dev-trx/unit-tests.trx`.
  - Stage-1 and §7.2 pass at their unchanged `stage1Tol` / `dataMatrixTol`.
  - A second run, after deleting the stale `Data/` folder from the build output, also passed 30 of 30. Nothing reads
    an archive.
- **Read-only.** No `.oc/` appeared in the kit folder after the runs.
- **Byte for byte.** Every written file's SHA-256 matched its archive entry before the archive was deleted.
  `git hash-object` with filters equals `--no-filters` for all 18, so staging keeps the bytes. The log is in
  `.artifacts/0330-data-move.log`.
- **Elevation's suppressed count** (ELEV004, which the command excludes and R7 measures without gating) is unchanged at
  1453. The run had no comparison base, so it covered all 12 projects.
- **LF.** Every edited text file has no CR, and `git diff --numstat` equals `--ignore-cr-at-eol`.
- No run exceeded its effective deadline.

```yaml
gates:
  berreman_unit_tests: 30
```

## Architecture

- **The lab files are kit content, read through the product's own path.** The test goes `KitProxy.create` →
  `readOnlyView` → `readDataFile` → `ChosenImportProfile.mapText (BuiltInProfileChoice PhoneRoiCsvProfile)`. That is
  the same route an attached experiment's data takes, so the test exercises the import profile, not a test-only
  reader. There is one copy of the data.
- **`CommittedKit` is test support in `BerremanTests`** (`CommittedKits.fs`, compiled before
  `MuellerReconstructionTests.fs`).
  - One case, `OpticsMuellerKit`, and one member, `tryFindFolder : KitFolder option`.
  - It walks up from `AppContext.BaseDirectory` to the folder holding `Berreman/Berreman.slnx`, writes nothing and
    reads no environment variable.
  - S10.2 (step 344) moves it to `BerremanTestSupport`; S12.2 adds AT-2's kit case.
- **`-text !eol` on `data/optics-mueller/data/raw/**`** keeps the recorded CRLF and BOM through every clone, so a kit's
  recorded SHA-256 (001 §E) stays true. It has the form of the `*.nb` rule. The shape JSONs are LF-only and stay under
  the LF policy, which leaves their bytes unchanged.

## Deferred

Nothing. Every requirement of the slice landed. The slice assigns these to later steps:

- the archive-keyed seam and its five remaining facts, `MuellerDataStore.fs`'s `archiveEntryName` doc and
  `committedArchive`, in step 331;
- moving `CommittedKit` to `BerremanTestSupport`, in step 344.

## Gotchas

- **The move was done with Python and `rm`, not `git mv`.** The worker protocol forbids git mutation. Until the
  supervisor stages the change, git shows `data.zip` as deleted and `data/optics-mueller/data/raw/` and
  `data/reference/shapes/` as untracked. The `.gitattributes` rule is in the working tree, so staging stores the bytes
  as they are.
- **Never run an EOL "fix" over `data/optics-mueller/data/raw/`.** The CRLF and the BOMs are the recorded bytes, and
  the byte-for-byte fact fails on any conversion.
- **The README still says every CSV carries a BOM.** `cpl_lr45_standard_ellipse_subset.csv` does not. The slice keeps
  every line it does not name, so that line is unchanged.
- **`MuellerDataStore.fs` still names `Berreman/Data/MuellerMatrix/`** (its `archiveEntryName` doc and
  `committedArchive`) until step 331 deletes them. No test calls `createArchiveBacked` any more.
- **`kitPath`** is a test-local helper over `KitRelativePath.tryCreate`. The seven `*File` values and the hash fact's
  paths are born through it.
- `.manifest.state.json` was already modified when the round started. It is the supervisor's file and was not touched.

## Changelog

- 2026-11-12 — Step 0330: moved the optics-mueller lab files out of `data.zip` into the kit, byte for byte.
  - Deleted `Berreman/Data/`.
  - Added the `CommittedKit` locator.
  - `MuellerReconstructionTests` reads the kit through the Phone ROI CSV profile. It has one fact renamed and one
    added (30 facts).
