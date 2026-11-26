# Step 0440 — implementation log

## Progress

- Read the protocol, the project prompt, rulings R0001–R0005 (`.manual/030`, `031`, `033`, `035`, `036`) and the
  slice. None of the rulings touches this step's test-only edit; all are preserved.
- Wrote the impl-plan.
- Added `runFolderOf (run)`. Both builds (`buildOpticsMuellerKitWith`, `buildAbsorbingKit`) now call it instead of
  computing `KitAcceptanceTests/<run>` inline; the round trip is its third user.
- Extracted step 430's inline round trip into `sendAndReopen (context) (model) (run) : KitFolder * TableAndElementRotationModel`.
  - It works on its own copy of the model and keeps every send and extraction assertion of step 430.
  - The start screen's clock, kit id and author come from the window's own `model.kit` (`clock`, `kitFile.id`,
    `kitFile.author`). AT-1's are the same values it named literally before.
  - The extraction target is `received/<kit file stem>`, which is `received/optics-mueller` for AT-1 as before.
  - The run folder is created first, because `KitArchiveProxy` does not create the zip's parent folder.
  - A failure after the open closes the reopened kit and rethrows.
- Extracted AT-1's inline `residualAtStart` into `startResidualOf (model) (study)`, which requires `Ok` and a
  non-empty vector.
- Step 430's fact now calls `sendAndReopen built.context built.model run` and keeps its assertions: the same files,
  and the residual element for element. Its cleanup order is unchanged: reopened kit, built kit, run folder.
- Added the fact `the absorbing-biaxial-noisy kit travels through Send kit and Open kit from zip unchanged, its
  study's residual at the start point included`.
  - It runs over `absorbingBuild.Force ()` under the run `AT-2-send`.
  - It asserts `assertSameKitFiles built.folder target`, `assertNoisyObservationFiles target` (the five observation
    files, named explicitly) and the start residual of `AbsorbingTriclinic`, `Ok` for both and equal entry for entry.
  - A leftover `AT-2-send` is removed first, and the folder is removed in `finally`.
  - The shared build is never closed or written.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/KitAcceptanceTests.fs`, the step's only authorized edit.

## Testing state

These are development runs, evidence only, each using its gate's own command and selection.

- **build:** exit 0, 87 s against 600 s. The only warnings are the exempt NU1902 (12) and the vendored MathNet's
  pre-existing SYSLIB0051 (4).
- **elevation** (ELEV004 excluded, all 13 projects): `ELEV-GATE: OK - 0 new finding(s)`, 315 s against 600 s.
- **kit-acceptance** over `OpticalConstructor.Ui.Tests.KitAcceptanceTests`: `Passed!`, 1517 s against 3600 s.
  - Total 15: 14 succeeded, 1 skipped (the Explicit five-seed ensemble, TRX `NotExecuted`), 0 failed.
  - The count rises from 13 to 14.
  - The new AT-2 fact passed in 6.4 s, and step 430's refactored AT-1 fact in 11.8 s.
  - AT-2's lazy build, its fit through the shell included, was charged to the grid fact (14 min 11 s). The
    read-only refit took 9 min 47 s.
- **ui-tests** (`StudyViewTests`, `DataStepTests`): `Passed!`, 18 of 18, 35 s against 360 s.
- **ui-smoke** (all discovered): `Passed!`, 264 of 264, 263 s against 600 s.
- **The twenty-one s42 invariants:** each decoded `git grep` finds nothing.
- **Nothing written into `data/`:** `git status --short data/` is empty after the class run, and `AT-2-send` was
  removed by the fact.
- No development run exceeded its gate's effective deadline.

## Artifacts

- `specs/0050/.artifacts/0440-dev-build.log`
- `specs/0050/.artifacts/0440-dev-elevation.log`
- `specs/0050/.artifacts/0440-dev-kit-acceptance.log` and `0440-dev-kit-acceptance.trx`
- `specs/0050/.artifacts/0440-dev-ui-tests.log`
- `specs/0050/.artifacts/0440-dev-ui-smoke.log`
- `specs/0050/.artifacts/0440-dev-invariants-b64.txt`

## Gotchas

- **No product code changed.** `SendKit` only reads the kit folder: `KitArchiveProxy.create` streams the zip into
  `<zip>.tmp` beside the zip. `OpenKitZip` hands the opened kit to `showKit` directly, and
  `KitLibraryContext.resolveSnapshots` decides without importing. So the shared AT-2 build and its context are left
  as they were, whatever order xUnit runs the class's facts in.
