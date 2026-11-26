# Step 0329 — state of the world

## Where we are

Spec 0050 section S9.4 moves the optics-mueller lab files into the kit folder (001 §Q AT-1, §S9; `.spec-md` §3.20).

- This step moves the reference half of `Berreman/Data/MuellerMatrix/` into `data/optics-mueller/data/reference/`:
  - the four Python scripts;
  - the two reference summaries;
  - the README.
- `data.zip`, the measured data, stays where it is. Step 330 moves it and deletes the folder.
- Step 331 changes `archiveEntryName`'s doc, which still points at the old README.
- No code and no behaviour changed.

## What's working

- Moved the reference pipeline's four scripts, two summaries and README into the optics-mueller kit's
  `data/optics-mueller/data/reference/`. The scripts and summaries keep their bytes.
- `Berreman/Data/MuellerMatrix/` now holds only `data.zip`.
- Updated the moved README where the move made it untrue:
  - it is the kit's reference folder;
  - it names the measured data as `Berreman/Data/MuellerMatrix/data.zip`;
  - `Further reading` cites 0050 `.manual/001` §Q AT-1 and §S9.
- `MuellerReconstructionTests`' provenance comments name the summaries and the README at their kit path. No code line
  changed.

## Tests

These are development runs of each gate's own command from `0329.gates`, over the final tree. The arc-runner's gate
engine runs the gates themselves.

| Gate | Result | Time | Deadline |
|---|---|---|---|
| build | exit 0, 0 errors | 75 s | 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`; known findings 0; ELEV004 excluded | 201 s | 600 s |
| unit-tests (`BerremanTests.MuellerReconstructionTests`) | Passed!, 29 of 29, 0 failed, 0 skipped | 3 s | 7200 s |
| invariant-s32-\* (16) | each `INVARIANT HOLDS`, exit 0 | under 1 s each | 300 s |

- **build.** The full recompile printed the two known `SYSLIB0051` warnings from the vendored MathNet
  `Exceptions.cs`, as in steps 320–328. No file this step touched produced a warning.
- **unit-tests.** The selected class has 29 facts at HEAD and now, and all 29 ran and passed. The count is unchanged.
  No code line changed, so the numbers are unchanged too. The TRX is in `.artifacts/0329-dev-trx/unit-tests.trx`.
- **Byte for byte.** The SHA-256 of each of the six scripts and summaries is the same before and after the move. None
  of them holds a carriage return.
- **LF.** The README and the test file are LF only. For the test file, `git diff --numstat` equals
  `--ignore-cr-at-eol` (21 / 19).
- **Ignore rules.** `git check-ignore` matches none of the new paths.
- **Elevation's suppressed count** (ELEV004, which the command excludes and R7 measures without gating) is unchanged at
  1453. The run had no comparison base, so it covered all 12 projects.
- No run exceeded its effective deadline.
- **The `berreman_unit_tests` baseline.** The selection counts 29.

```yaml
gates:
  berreman_unit_tests: 29
```

## Architecture

- **A data move with no code change.** The kit folder `data/optics-mueller/` now exists and holds only
  `data/reference/`.
  - No `.kit-json` or other kit file was written. Section S12.1 builds the kit around these files.
  - Step 330 adds `data/raw/` from `data.zip`.
- **The scripts and summaries remain provenance, not inputs.** The tests still transcribe their literals and read no
  file for their expectations. Nothing builds, references or executes the moved files.

## Deferred

Nothing. Every requirement of the slice landed. The slice itself assigns these texts to later steps:

- the `data.zip` mentions in `MuellerReconstructionTests.fs` (the class doc's bullet and `loadDataSet`'s doc), which
  move with the data in step 330;
- `archiveEntryName`'s doc in `MuellerDataStore.fs`, in step 331.

## Gotchas

- **The move was done with `mv`, not `git mv`.** The worker protocol forbids git-mutation commands.
  - The working tree is identical either way.
  - Until the supervisor stages the change, the new files show as untracked (`?? data/`) and the old ones as deleted.
  - Git records the six byte-identical files as renames, and the README as a rename with edits.
- **`MuellerDataStore.fs:150` still says `Berreman/Data/MuellerMatrix/README.md`** until step 331 rewrites that doc.
  The README now lives in `data/optics-mueller/data/reference/`.
- **The README's title still reads "measured data and reference pipeline".** The README still inventories the
  measured data, which lives in `Berreman/Data/MuellerMatrix/data.zip` until step 330.
- `.manifest.state.json` was already modified when the round started. It is the supervisor's file and was not touched.

## Changelog

- 2026-11-12 — Step 0329: moved the optics-mueller reference pipeline into the kit.
  - The four scripts, two summaries and README moved to `data/optics-mueller/data/reference/`.
  - The README and `MuellerReconstructionTests`' provenance comments name the new path.
  - `data.zip` stays for step 330.
