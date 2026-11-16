# Step 0435 — implementation log

## Progress

- Read the task, both system prompts, the project prompt, rulings R0001–R0003 (`.manual/030`, `031`, `033`), the slice
  and its gate snapshot, and step 0434's impl-log and state of the world.
- Read the APIs the step drives:
  - `Studies` (`StudyMsg`, `Study.openMaterial`, `withMaterialParameterState`, `scopeOf`,
    `CrystalParameters.parameterAxes`);
  - `StudyForward` (`prefitWith`, `dataWith`, `residualWith`, `parameterLayoutWith`, `assignmentFor`,
    `predictedReadingOf`);
  - `MuellerFitting.throughputResidualFor`, `ParameterScaling`, `ofScaled`;
  - `AbsorbingBiaxialSample` (`perturbedStart`, `searchBox`, `absorbingScalingAround`);
  - the window's `StudyCommand` handler and `studyForwardContext` (`SolverParameters.defaultValue`, three reflections,
    which is the same as `BiaxialSample.solverParameters`);
  - `StudyEngine.studySearchBox` (50, the same as `AbsorbingBiaxialSample.searchBox`).
- Wrote the build continuation, the shared composition, the harness helpers, the assertions and the fact, all in
  `KitAcceptanceTests.fs`.
- First development build failed: `AbsorbingTriclinicParameters` needs its `CrystalPhysics.` qualifier, as the file's
  other uses have. Fixed, rebuilt clean.
- Development kit-acceptance run: 10 of 10.
- Probed the residual identity with a temporary failing statistic in the fact, then restored the file byte for byte
  (see Testing state).
- Added a layout assertion (one vector entry per component, owned by the material), so "across the five plates" is
  checked directly. Rebuilt; re-ran kit-acceptance, ui-tests and ui-smoke.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/KitAcceptanceTests.fs`, the step's only authorized edit
  (+202 / −23, LF only, no BOM).

## Decisions

- **How-to (1), the build.** `buildAbsorbingKit` goes on after `attachNoisyObservations` with `buildAbsorbingStudy`,
  through the window's own `StudyCommand` messages, checking `kitStatus` = None after each:
  1. `CreateStudy (StudyName "AbsorbingTriclinic", <the five experiment names in kit order>)`;
  2. `OpenMaterial (…, absorbingMaterialVersion)`, which is `MaterialVersionId.firstOf absorbingMaterial.id`;
  3. one `SetOpenedMaterialParameter` per generated axis of the opened copy's `crystal.parameterAxes`, read from the
     window's study after step 2.

  `perturbedStartUnknownsOf crystal` zips the generated axes with `AbsorbingTriclinicParameters.axes` by position:
  - `s = a.parameterValueOf (AbsorbingTriclinicCrystal perturbedStart)`;
  - `h = searchBox.halfWidth × r.read (absorbingScalingAround perturbedStart).scale`;
  - `unknownBetween s (s − h) (s + h)`, the file's existing helper, with `priorOpt = None`.

  No scope, link, session gain or fit is set.
- **One body for the build and the fact.** The fact compares the stored components to `perturbedStartUnknownsOf
  material.crystal` ("as set") rather than computing a second copy of the box (AGENTS.md "Reuse, never copy"). It
  checks the axes independently through `assertAbsorbingAxes`.
- **How-to (2), the shared composition.** Step 430's inline `residualAtStart` became two private functions, placed
  before the first fact:
  - `studyDataOf`: `prefitWith forward []` then `dataWith`;
  - `studyResidualAtStart`: `residualWith` over `parameterLayoutWith`'s layout at `startVector`.

  Both find the study through a one-line `studyIn`. The signatures keep `Result<_, StudyForwardError>`, which has no
  case for a study the kit lacks, so `studyIn` fails the fact with the same message the Send fact used. The Send fact
  now calls `studyResidualAtStart` and keeps its `failwith` wrapper and every assertion.
- **How-to (3), `byPlate`.** It collects the observations of each plate, in `absorbingPlateCuts` order, filtering by
  `configuration.cut`, so each plate's observations stay in their own order.
- **How-to (4), the fact** `AT-2's study opens the material across the five plates, loads the harness's observations
  exactly, and its residual at perturbedStart is the harness's`:
  - `assertAbsorbingStudy`, over a second `KitProxy.create`:
    - exactly one study, `AbsorbingTriclinic`, of the five experiments in kit order;
    - one opened material: version 1, the entry's name, crystal `AbsorbingTriclinicCrystal absorbing`;
    - fifteen `UnknownParameter` components equal to the build's set;
    - no scope, no link, no session gain;
    - the stored study equal to the window's;
    - `scopeOf` `MaterialScope` for every component;
    - the layout: fifteen entries, each owned by `MaterialOwner` of the version.
  - `assertAbsorbingAxes`, on the stored copy's crystal:
    - fifteen record axes;
    - generated names, and field cases rendered by `fieldCaseOf`, in record order;
    - at each position, the generated read of `AbsorbingTriclinicCrystal perturbedStart` equals the record read of
      `perturbedStart` exactly.
  - `assertAbsorbingStudyData`:
    - `studyDataOf`'s runs are the study's experiments in order;
    - every row is an `ObservationRow`, failing with the run and row number otherwise;
    - the observations equal `byPlate (noisyObservationsOf attachedSeed)` in number and order, each configuration and
      `measured.element i j` exactly.
  - `assertAbsorbingStartResidual`:
    - 704 entries, equal to 16 × the observation count;
    - each entry within 1e-12 · max(1, |h_k|) of `MuellerFitting.throughputResidualFor absorbingForward observations
      (absorbingScalingAround perturbedStart)` at the zero scaled vector;
    - every mismatch is listed with its index and both values (`:R`).
- **Shared harness pieces.** Two pieces are shared rather than duplicated:
  - `absorbingForward` is the forward step 434's `noisyObservationsOf` built inline, now named once, so the draw and
    the harness residual use one forward.
  - `assertSameObservation` (configuration plus 16 elements) is split out of `assertSameObservations`. The step-434
    fact's messages are unchanged (`observation {n}: key`, `…: configuration`, `…: m{i}{j}`).
- **`attachedSeed`** is used for the expected observations: it is the named `List.head noisySeeds` the how-to cites.
- **R0001–R0003.** No slot is added, so the element-id provider mints only the reflected detector's id (R0003), and its
  assertion still holds. No fit runs, so neither the study engine's dispatch (R0001) nor the analyzer law (R0002) is
  reached.
- **`open OpticalConstructor.Optimization`** was added so the call reads `MuellerFitting.throughputResidualFor`, as the
  how-to writes it and as `StudyForwardTests` does.

## Testing state

These are development runs, evidence only and not gate verdicts; the supervisor runs the gates.
- `dotnet build OpticalConstructor.Ui.Tests.fsproj -c Release`:
  - the first attempt had 8 errors (the missing `CrystalPhysics.` qualifier);
  - after the fix, 0 errors, and the only warnings are the exempt NU1902 advisory;
  - logs: `.artifacts/0435-attempt-01-dev-build.log`, `.artifacts/0435-attempt-01-dev-build-final.log`.
- kit-acceptance over `OpticalConstructor.Ui.Tests.KitAcceptanceTests`, the gate's command shape:
  - `Passed!`, total 10, succeeded 10 (9 before, +1), in 88 s against the 3600 s deadline, run before and after the
    layout assertion;
  - the TRX names the new test `OpticalConstructor.Ui.Tests.KitAcceptanceTests.AT-2's study opens the material
    across the five plates, loads the harness's observations exactly, and its residual at perturbedStart is the
    harness's`, the census id exactly;
  - logs: `.artifacts/0435-attempt-01-dev-kit-acceptance.log`, `…-final.log`; TRX:
    `.artifacts/0435-attempt-01-dev-results/kit-acceptance-dev.trx`, `kit-acceptance-dev-final.trx`.
- Residual probe: a temporary `Assert.Fail` reported the comparison's statistics, and the file was then restored from
  its saved copy (grep finds no `PROBE`).
  - All 704 entries are bit-identical to the harness's (max |a − h| = 0).
  - |h| ranges from 1.03e-5 to 1.39, so no entry is trivially zero.
  - Log: `.artifacts/0435-probe-residual.log`. (An earlier probe attempt changed nothing, because its text replacement
    did not match. That run was the unmodified fact and passed, 1 of 1.)
- ui-tests command selecting `StudyViewTests` and `DataStepTests`: `Passed!`, 18 of 18, in 29 s against 360 s. Log:
  `.artifacts/0435-attempt-01-dev-ui-tests.log`.
- ui-smoke command (`Category=ui-smoke`, every discovered test): `Passed!`, 264 of 264, in 237 s against 600 s. Log:
  `.artifacts/0435-attempt-01-dev-ui-smoke.log`.
- Elevation was not run. The only edited file is in a test project, which is outside the ELEV scope (AGENTS.md).
- The twenty-one s42 invariants: each decoded `git grep` finds nothing.
- Line endings: `git diff --numstat` equals `--ignore-cr-at-eol` (202 / 23); the file has no CR and no BOM.

No deadline was exceeded, and no requirement is blocked.

## Artifacts

- `.artifacts/0435-attempt-01-dev-build.log`, `.artifacts/0435-attempt-01-dev-build-final.log`.
- `.artifacts/0435-attempt-01-dev-kit-acceptance.log`, `.artifacts/0435-attempt-01-dev-kit-acceptance-final.log`.
- `.artifacts/0435-attempt-01-dev-ui-tests.log`, `.artifacts/0435-attempt-01-dev-ui-smoke.log`.
- `.artifacts/0435-attempt-01-dev-results/`: their TRX reports.
- `.artifacts/0435-probe-residual.log`: the residual-identity statistics.

## Gotchas

- The study's residual equals the harness's bit for bit, not merely within 1e-12. Writing `s` back through the
  generated axis reproduces `perturbedStart` exactly, and `ofScaled` at zero adds `0.0 × scale` to the centre. If a
  later change makes the comparison loosen to the tolerance, look first at those two writes.
- `StudyForwardError` has no "study missing" case, so `studyIn` fails the fact rather than answering an error. Its
  message is the one step 430's fact used.
- `recordStudy` snapshots the study's material reference. It was already the plate's snapshot, so step 432's exact
  library assertion still holds, re-run over the shared Lazy build.
- `CrystalPhysics` is not opened in this file: its types are written qualified (`CrystalPhysics.CrystalParameters`,
  `CrystalPhysics.AbsorbingTriclinicParameters.axes`). Opening it would bring its case names into a file that already
  opens many modules.
