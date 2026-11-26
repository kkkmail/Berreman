# Step 0471 — impl-plan

## Approach

Add the Measurement Planner's four inputs from the kit and one of its studies, in
`OpticalConstructor.Domain/MeasurementPlanner.fs`, after step 470's lab profile:

1. `PlannerError` gains `UnopenedPlannerMaterialErr` and `NonCrystalPlannerMaterialErr`, with their `describe` arms.
2. `PlannerPlate` — a free-standing plate of the material: item, label, sample version, thickness, substrate kind,
   emission and the principal axis its substrate layer's orientation puts on the normal
   (`CrystalOrientation.normalAxisOpt`, step 466).
3. `PlannerPrior` — the study's opened copy and its crystal at the current values.
4. `PlannerInputs` — lab, plates, prior, targets.
5. `type Study with member this.plannerInputsWith context choiceOf material`. It reuses the study's own readers:
   the opened copy (`this.materials`), `OpenedMaterial.currentPhysics.crystalOpt`, `LabProfile.ofItems`,
   `Study.unknownsWith (context.itemsUsedBy this)` and `Thickness.parameterValueOpt`. No new store or cache.

Order of refusals: unopened version, then non-crystal copy, then the lab profile's own error.

## Tests

`PlannerInputsTests` gains three facts. The new fixtures are:

- a test-local uniaxial crystal entry and version, built from `CrystalMaterialFixtures.uniaxialParameters`;
- the samples QZ-Z (PrimaryAxes, ThickPlate, EmitBoth) and QZ-X (EulerRotation ZmYpXp theta 90°, EmitTransmittedOnly),
  each 0.5 mm;
- a film sample and an infinite-substrate sample;
- stack-sample drafts over the shipped `stackSampleItemModelId` entry;
- a study made by `Study.create`, then `openMaterial`, freeing n_e and g33 within ±5 %;
- a `StudyContext` that holds no experiment.

The tabulated material reuses `TabulatedStudyFixtures` (step 460's own fixture module, public, compiled earlier), so
nothing is copied.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/MeasurementPlanner.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/PlannerInputsTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/PlannerReportTests.fs`: forced. Its exhaustive `PlannerError`
  match becomes FS0025, an error under `--warnaserror+:25`, once the two cases exist. The fix is one or-pattern arm, as
  in step 470. No assertion changes.

## Risks

- Record-label inference: `PlannerPlate` shares `item` / `label` with three other planner records, so every
  construction is annotated.
- New opens in `MeasurementPlanner.fs` (`Studies`, `Placement`, `MaterialLibrary`). Our names are unique (ELEV014–016),
  so these opens cannot shadow anything.
