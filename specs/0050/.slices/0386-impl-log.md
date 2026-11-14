# Step 0386 — impl log

## Progress

- Read the task, both system prompts, the project prompt, the slice, step 0385's SoW, and the code this step builds on:
  `FitRuns.fs`, `StudyForward.fs`, `Studies.fs`, `Setups.fs`, `ItemModels.fs`, `MuellerReconstruction.fs`,
  `StagedMuellerPath.fs`, `FitRunTests.fs` and `StudyForwardTests.fs`. Also read the briefs of steps 0387–0391, which
  consume `engineWith`.
- Wrote `StudyEngine.fs`: `StudyFitError`, `CalibrationPolarizer` with `familyWith`,
  `ItemModelChoice.calibrationPolarizerOpt` and `StagedStudy.engineWith`. Added one `<Compile>` line after
  `StagedMuellerPath.fs`. The Optimization project built clean.
- Added the five facts and their test-local fixtures to `StudyEngineTests.fs`. The first solution build failed because
  `Thickness` was missing (`open Berreman.Media`). After adding the open, the build succeeded with 0 errors; the only
  warnings were the exempt NU1902 advisories.
- `StudyEngineTests`: 7/7 pass (2 existing + 5 new).
- The full constructor suite passes 1672/1672 (it was 1667, so +5).
- The twenty `invariant-s38-*` greps are all OK, also run with `--untracked`.
- Elevation and the selected `BerremanTests` classes were run locally; their results are recorded under Testing state.
- Changed `muellerCoefficients` to reuse the `SixteenElementMueller` form's own axes instead of rebuilding the sixteen
  names. After that, rebuilt and re-ran the constructor suite (1672/1672) and elevation (OK).

### Attempt 2 (code-judge cycle 1 route-back)

- The judge found a misclassification. `oneUnknownMatrix` applied `List.distinct` to the held item ids, so one free
  black box held in two slots of a path (a double pass, M·F·M, quadratic in its coefficients) was counted once and
  answered `LinearStudyEngine`. Plan:
  - first, add a double-pass bench and assertion inside the existing black-box fact, and confirm it fails (red);
  - then drop the `List.distinct`, so unknown-bearing occurrences are counted per slot.
- Red: added the bench `qzTwice` ("LP-QZ-QZ-LP": the free QZ in both object slots behind the published LP1 and LP2,
  read by the known-gain Phone) and run E12. E12's capture rows are made of the truth's square, `qzTruth * qzTruth`.
  - The assertion sits inside the existing fact `fully or partly unknown black-box matrices behind fixed instruments
    are linear, while a free gain or an unresolved series is nonlinear`. No new test identity.
  - It asserts `NonlinearStudyEngine` under `ObjectStage` and `JointStage` through `engineOf`, which first asserts
    that every prefit is `Ok` and that the layout is non-empty.
  - Against the old code the fact failed at `StudyEngineTests.fs:698`, the new assertion: `Expected:
    NonlinearStudyEngine`, `Actual: LinearStudyEngine`. Every earlier assertion in the fact had passed by then.
- Fix: `oneUnknownMatrix` now filters the held slots themselves
  (`held |> List.filter (fun (_, item, _) -> bearsUnknown item.id)`). It has one comment line citing the double pass.
  - Rebuilt: 0 errors. The only warnings are the exempt NU1902 advisories.
  - `StudyEngineTests` 7/7, the constructor suite 1672/1672, elevation OK, the twenty invariants OK.
  - The selected `BerremanTests` classes were re-run on this build.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Optimization/StudyEngine.fs` (new).
- `Berreman/OpticalConstructor/OpticalConstructor.Optimization/OpticalConstructor.Optimization.fsproj` (one
  `<Compile Include="StudyEngine.fs" />` line after `StagedMuellerPath.fs`, no comment).
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/StudyEngineTests.fs`: new opens, the S11.3 engine-choice
  fixtures and five facts. The two existing facts are unchanged.
  - Attempt 2 added the `qzTwice` bench, adding it to `benches`, and the E12 double-pass assertion inside the
    black-box fact. Every earlier assertion is kept.

## Decisions

- **Inputs.** `engineWith` reads:
  - the staged items (`this.items`), placed into a record copy of the context's kit;
  - the layout `this.study.parameterLayoutWith` over that context;
  - each fitted run's `slotsIn` in `SetupSlot.inLightOrder`, with every held item's `context.choiceOf`;
  - the study's links;
  - the prefits.

  A fitted run whose setup, item or model does not resolve matches neither shape, so it is nonlinear.
- **Fitted parameters.** These are the layout entries' parameters, plus every link follower whose leader is an entry,
  because the follower takes a fitted value (step 364). An item is *unknown-bearing* when one of its parameters is
  fitted.
- **Calibration shape (a)**, read literally. Every fitted run is read through capture rows by an intensity detector (the
  same first-detector-form rule as `boundWith`). Every fitted run also holds:
  - no object item other than an `EmptySlotModel` item;
  - exactly one prep item and one analyzer item, both with a `calibrationPolarizerOpt`.

  LP-LP, LP-CPL and CPL-LP each occur at least once. Every entry is item-owned and is one of its run family's stage-1
  constants (`CalibrationRunFamily.stageOneConstants`). A CPL-CPL run is allowed and contributes no constant. No link
  follows a fitted parameter (see Gotchas).
- **Black-box shape (b).** Every entry is an item-owned `m<r><c>` of an item whose model is
  `OpticalElementModel (BlackBoxMuellerModel SixteenElementMueller)`, and so is every fitted link follower. Every
  fitted run is read through capture rows by an intensity detector and holds **at most one** unknown-bearing item.
  - Every other element on the path is fixed, of any kind: ideal CP of either hand, constant-Mueller, compound,
    surrounding objects. `calibrationPolarizerOpt` is not consulted.
  - A free gain fails because its entry is session-owned and not a coefficient.
  - A series of two free matrices fails, and so does a link that puts a fitted coefficient on a second matrix of the
    same path, because two unknown-bearing items share one path.
  - Unknown-bearing matrices are counted per slot, not per distinct item (attempt 2). One free item held in two slots
    is a double pass (M·F·M): its residual is quadratic in the item's coefficients, so the answer is nonlinear.
  - Rank is not read.
- **No rank, score, threshold or option.** The choice is structural.
- **Test fixtures.**
  - Items are made by `ItemDraft.addTo` over shipped step-210 models: the ideal-polarizer, source-preset and
    stack-sample models for the bound choices; the circular polarizer, real LP, retarder, black-box, intensity
    detector and polarimeter models.
  - Instruments are published through `publishRevision`, as `FitRunTests` does.
  - Prefits are made as `StudyForwardTests` makes them, from a test-local `ExperimentDataProxy` that answers each
    run's file by its `CapturePoints` rows (`ObservationPoints` for the polarimeter study).
  - `engineOf` asserts that every prefit check is `Ok` and that the staged layout is non-empty before it calls
    `engineWith`, so no answer comes from an earlier refusal.
  - Observations are generated with the reference projection, 1000 · (`designRow s a` · `M.vecColumnMajor`) + 5,
    from the same truths whose elements the fixed coefficients hold: QZ = `retarderMueller 30° 40°`,
    LR = `retarderMueller 10° 25°`, and QZ2 = `retarderMueller 10° 40°`, which shares QZ's m00 and m33.
  - The links are checked against those truths: QZ's m21 = m12, QZ2's m00 and m33 = QZ's.

## Testing state

Attempt 2, run locally on 2026-11-13 on the final code (after the per-slot fix).

- Build: exit 0, 0 errors, 43 s (deadline 600 s). The only warnings are the exempt NU1902 advisories.
- `constructor-unit-tests`: 1672/1672, 74 s (deadline 180 s). The count is 1667 → 1672, so +5. The TRX shows all
  seven `StudyEngineTests` identities passing, the five new ones under their exact slice names. The double-pass
  assertion adds no identity.
- Elevation: `ELEV-GATE: OK - 0 new finding(s) in 13 project(s), 214s` (deadline 600 s). This was a standalone run
  with no `-Base`, so every project was analyzed.
- `unit-tests`, selected `BerremanTests.MuellerReconstructionTests` and `BerremanTests.MuellerInverseTests`: 45/45,
  686 s (11 min 26 s; deadline 7200 s).
- The twenty `invariant-s38-*` must-find-nothing greps: all OK, also run with `--untracked`.
- Red before the fix: `StudyEngineTests` ran 6/7 on the old `oneUnknownMatrix`. The black-box fact failed at its new
  E12 assertion (`StudyEngineTests.fs:698`) with `Expected: NonlinearStudyEngine` / `Actual: LinearStudyEngine`.

Every run finished within its effective deadline. No requirement of the slice is left open.

## Artifacts

- `specs/0050/.artifacts/0386-build-red-local.log` and `0386-studyengine-tests-red-local.log`: attempt 2's build and
  class run with the double-pass assertion but before the fix (the red).
- `specs/0050/.artifacts/0386-build-local.log`: the solution build (attempt 2, final code).
- `specs/0050/.artifacts/0386-studyengine-tests-local.log`: attempt 1's `StudyEngineTests` class run.
- `specs/0050/.artifacts/0386-constructor-unit-tests-local.log`: the full constructor suite, with its TRX under the
  test project's `TestResults/`.
- `specs/0050/.artifacts/0386-elevation-local.log`: the elevation gate script.
- `specs/0050/.artifacts/0386-unit-tests-local.log`: `MuellerReconstructionTests` and `MuellerInverseTests`.

## Gotchas

- **Count unknown-bearing matrices per slot, never per distinct item.** Attempt 1 deduplicated the held item ids, so a
  double pass (one free black box in two slots) read as one matrix and was classified linear. Neither `Setup.create`
  nor `SetupSlot.fittingItems` keeps an item out of a second slot.
- **At most one, not exactly one, unknown-bearing item per fitted run (black-box shape).** A fitted run that bears no
  unknown is a constant row, which is trivially affine. This matters for a joint fit: it fits the calibration runs too
  (`studyUse = ResidualRunUse`), and step 391 drives step 390's fixture, whose AIR runs anchor its session gains,
  through `runFit` under `JointStage` and expects `LinearStudyEngine`.
- **The calibration shape also requires that no link follows a fitted parameter.** This is my one addition to brief
  (a), which states its conditions on layout entries only.
  - Why: stage 1's reference path fits each family's constants independently (step 389 writes each constant to its
    entry). A follower would silently take its leader's constant, not one fitted under the link.
  - Effect: such a study goes to the nonlinear engine, which honours the link through `assignmentFor`.
  - The black-box shape keeps its links, as the brief requires.
- **`muellerCoefficients` reuses the form's own axes**: `(BlackBoxMuellerModel SixteenElementMueller).parameterAxes`.
  It does not rebuild the sixteen names.
- **The built-in circular polarizer's offsets are exact doubles.** `BuiltInCatalogueIntegrityTests` asserts the shipped
  form against `Angle.zero` and `Angle.degree 45.0<deg>`, so the circular match compares them exactly.
- **`grep -c $'\r'` in this shell counts lines that contain an `r`.** Use `tr -cd '\r' | wc -c`. Every file written this
  step is LF (`git ls-files --eol` shows `w/lf`).
