# Step 0247 — impl plan (IMPLEMENT: the bound element arm of the evaluator)

## Approach

1. `OpticalConstructor.Domain/ItemEvaluation.fs` opens `Berreman.Fields` (for `IncidenceAngle`, `WaveLength`,
   `MuellerMatrix`) ahead of the constructor modules, so their record labels keep precedence. It gains:
   - `ItemEvaluationError.UnmodelledBranchErr of reason : string`, rendered `the element has no such branch: <reason>`;
     the error type's doc names the new condition;
   - `ElementOrientation = { rotation : Angle; incidence : IncidenceAngle; branch : Propagation.Branch }` with
     `static member transmittingAt (rotation : Angle) : ElementOrientation` (normal incidence, `BranchTransmitted`);
   - `BoundOpticalElement = BoundPolarizerPreset of PolarizerBehavior | BoundEmptySlot` with
     `member this.muellerAt (orientation : ElementOrientation) (w : WaveLength) : Result<MuellerMatrix, ItemEvaluationError>`:
     one match over (element, branch) with no wildcard — a preset on the transmitted branch is
     `Propagation.behaviorMueller b orientation.rotation`, the empty slot is `Propagation.identityMueller`, and either on
     the reflected branch is `UnmodelledBranchErr` naming the element's kind. No other member, cache or option.
2. `OpticalConstructor.Tests/ItemEvaluationTests.fs`:
   - new `a polarizer preset is its behaviour's Mueller matrix at the element's rotation`: the three `ComputedIdeal`
     kinds and the 20°/65° two-LP `ConstantMueller` at 0°, 30°, 45°, 90°, 135°, 330°, 600 nm, compared exactly element
     by element through `MuellerMatrix.element`;
   - new `an empty slot is the identity, and an element that only transmits has no reflected branch`;
   - widened `every evaluation error renders its condition and its payload` (distinct renderings 3 → 4).

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/ItemEvaluation.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/ItemEvaluationTests.fs`

## Risks

- Opening `Berreman.Fields` in `ItemEvaluation.fs`: its record labels (`IncidentLightInfo.ellipticity`, …) could capture
  a record construction. `SourceEmission.ofValues` annotates its record, and the open goes before the constructor
  modules' opens.
- The new labels `rotation` / `incidence` / `branch` win inference in `ItemEvaluationTests` (the only file opening
  `ItemEvaluation`); its records are annotated or use other labels.
- `w` is unread by the two achromatic arms; F#'s unused-parameter warning (FS1182) is off in this repo, and the name is
  part of the declared contract.
- ELEV: unique names (`ElementOrientation`, `BoundOpticalElement`, `BoundPolarizerPreset`, `BoundEmptySlot`,
  `UnmodelledBranchErr`, `muellerAt`, `transmittingAt` absent from the tree and STL), no `%A`, no re-wrap.
