# Step 0472 — impl-plan

## Approach

The kit report (spec 0050 S14.1; 001 §M "this kit can never recover X" / "you need an off-axis cut") is two members
on step 471's `PlannerInputs`, both reusing what steps 469 and 470 already built:

1. `member this.looks : PlannerLook list`: for each plate in item order, each reach of `this.lab.reaches` (step 470,
   fullest first), each incidence of `[ IncidenceAngle.normal; IncidenceAngle.maxValue ]` and each observable the
   plate's `Emission` allows (`EmitBoth` → transmitted then reflected, `EmitTransmittedOnly` → transmitted,
   `EmitReflectedOnly` → reflected), the look `{ normalAxisOpt = plate.normalAxisOpt; incidence; observable; reach }`.
   The kit holds no angle limit (no item model publishes one, step 208), so the table's range applies: normal
   incidence and the steepest the engine takes bound every tilt layer 1 can tell.
2. `member this.structuralReport : Result<RecoverabilityReport, PlannerError>`:
   `this.prior.crystal.structuralReport this.targets this.looks` (step 469).

No other member is added. The numerical layer, the plan and its cache belong to S14.2.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/MeasurementPlanner.fs`: the two intrinsic members on the
  `PlannerInputs` record.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/PlannerInputsTests.fs`: a study freeing all four components
  (n_o, n_e, g11, g33), and three new facts:
  - `a kit's looks are each plate under each reach, at normal and steepest incidence, on each branch its emission allows`
  - `a kit of z-cut plates needs an off-axis cut, and one without a full Mueller reach never recovers the gyration`
  - `a kit with no plate of the material can never recover any target`

## Expected outcomes (traced through step 469's `sightOf` / `structuralReport`)

- QZ-Z + Polarimeter: the four looks all sit on `ThirdCrystalAxis`. The 89° looks see n_e and g33 and the normal look
  sees n_o and g11 through the oblique look, so every target is reachable. Because every look shares one normal, there
  is one finding: `OffAxisCutFinding { targets = [ n_e; g33 ]; capAngle = snellCap; cuts = [ XCut ] }`.
- With QZ-X added there are two normals, so no off-axis finding and no findings at all.
- QZ-Z + QZ-X + Phone + LP1: one polarizer gives `IntensityOnlyReach`. The oblique looks see n_o and n_e. The
  gyration's closest sight is `ReachMissesInLook`, so g11 and g33 are `FullMuellerNeededReason` with
  `[ FullMuellerRemedy ]`.
- GL only: no plate means no look, so every target is `NoLookReason`. Each target's remedies are its seeing cuts as
  `AddPlateRemedy`, then `FullMuellerRemedy`. There is no off-axis finding.

## Risks

- The test project builds with `--warnaserror+:25`, so every match must be exhaustive.
- Elevation runs over Domain and its dependents. The members are intrinsic and annotated, with no new free function.
- Constructor suite: the count must rise by exactly 3 over checkpoint 1861.
