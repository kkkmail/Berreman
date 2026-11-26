# Step 0385 — impl plan

## Approach

The stage is expressed through the study's own levers, so `parameterLayoutWith`, the pre-fit, the data and the residual
(steps 372–379) serve each stage unchanged.

In `FitRuns.fs`, after step 383's members, add:

1. `FitRunError` with three cases, each `of reason : string`: `InstrumentNotCalibratedErr`, `NoStageRunErr` and
   `NoStageUnknownErr`. Its `describe` renders the three texts the slice gives.
2. `StagedStudy`: the record `{ study; items; stage; calibrationUse; fittedRuns }`.
3. `type Study with member this.stageWith (context : StudyContext) (stage : FitStage) (calibrationUse : CalibrationUse)
   : Result<StagedStudy, FitRunError>`:
   - **Residual runs.** These are `this.fittedRunsIn context.experiments` (step 372): the enabled runs whose `studyUse`
     is `ResidualRunUse`, in study order.
   - **Fitted runs.** `JointStage` takes every residual run. `CalibrationStage` takes the `CalibrationRun` ones.
     `ObjectStage` takes the `ScienceRun` and `DriftCheckRun` ones. When nothing is fitted the answer is `NoStageRunErr`,
     naming the study and `stage.text`.
   - **Instruments.** These are the items the enabled Calibration runs use, found through `StudyContext.itemsUsedBy`
     over the study narrowed to those runs. That is the step 261 / 362 lookup, reused rather than copied.
   - **Runs read.** The fitted runs, plus the AIR runs of `gainModelOf` for every session (`sessionsIn`) that holds a
     fitted run, plus the Dark runs. Dark runs are never residual runs, so they always stay enabled. Every other
     residual run gets `withExperimentEnablement … DisabledInStudy`.
   - **`ObjectStage` check.** An instrument's `UnknownParameter` whose `scopeOf` is `ItemScope` answers
     `InstrumentNotCalibratedErr`, naming the item's label and the parameter. It reads the items as given.
   - **`CalibrationStage` holding.** Every `UnknownParameter` of a non-instrument item, and every opened material's
     unknown component (`withMaterialParameterState`), becomes `KnownParameter` of its start.
   - **`CalibrationAsPrior`.** Every `CalibratedParameter` of an item the fitted runs use becomes
     `UnknownParameter { start = v; bounds = v ± 50σ; prior = v ± σ }`.
   - **`NoStageUnknownErr`.** Answered, naming the study and `stage.text`, when
     `staged.unknownsWith ({ context with items = staged items }.itemsUsedBy staged)` is empty.

The check order is runs, then the instrument check, then unknowns. A study with no run to fit is refused for that reason
before anything about its items.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/FitRuns.fs`: open `StudyForward`, then add the three
  declarations.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/FitRunTests.fs`: the test-local LP1 / CPL1 / QZ / Phone items
  and setups A and Q, the runs A1, A2, E1 and K1, a `StudyContext`, and five facts.

## Risks

- `withExperimentEnablement` and `withMaterialParameterState` return `Result<_, StudyError>`, and `FitRunError` takes no
  further case. Each call is handed a run or a component the study itself holds, so it never refuses. The fold keeps
  the study on that branch.
- Item states are mapped in place (`{ item with parameters = … }`), not through `Item.withParameterState`, because an
  `ItemError` has no `FitRunError` case. A known value at a validated start, and a calibration freed within ±50σ with
  σ > 0, both keep `Parameter.validate`'s rules.
- The `SessionGainModel` → AIR-run match is a four-line copy of the one in `withSessionGainModel`. The slice forbids any
  other member, and `Studies.fs` is outside its edits, so it cannot be extracted here.
