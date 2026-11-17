# Step 0469 — impl plan

The structural recoverability report (spec 0050 S14.1), the last piece of the planner's layer 1. Step 468 gave
`CrystalParameters.targetSightOf` (reachable, or the closest look's reason) and `snellCap`; this step turns a
target list and a look list into a `RecoverabilityReport`: the reachable targets, one finding per unrecoverable
target with its remedies, and one off-axis-cut finding when every look's plate puts the same axis on its normal.

## Approach

In `MeasurementPlanner.fs`, after step 468's declarations:

1. A private `Angle.plannerDegreesText` (degrees, one decimal, `CultureInfo.InvariantCulture`): the one renderer
   every planner sentence reads, so the format is written once.
2. `SampleCut.plateText`, `PlannerRemedy` (+ `text`), `TargetFinding`, `OffAxisCutNeed`, `PlannerFinding`
   (+ `text`), `RecoverabilityReport` (+ `unrecoverable`, `offAxisCutOpt`), `PlannerError` (+ `describe`), each with
   the brief's exact strings.
3. `CrystalParameters.structuralReport targets looks`: resolve every target name through `this.components`, the
   first unknown answering `UnknownPlannerTargetErr` (reusing `ItemEvaluation.sequenced`, the shared ordered Result
   collector); each target then goes by `targetSightOf` into `reachable` or a finding whose remedies follow the
   brief's table over the candidate plates (`[ZCut; XCut]` uniaxial, all three otherwise) seen at normal incidence by
   a full-Mueller transmitted look; finally the off-axis-cut finding when the looks share one `Some a` normal.

Tests: four facts in `PlannerReportTests` named exactly as the step's effects, plus a `crystalComponentNamed`
helper that the existing `componentNamed` now calls (existing facts unchanged).

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/MeasurementPlanner.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/PlannerReportTests.fs`

## Risks

- Record-label inference (`targets`, `reason`) may pick another record in scope: annotate every construction.
- ELEV: no free functions; every helper is a member or a local; the case and type names were grepped unique.
- `private` on an optional extension member must compile; if not, fall back to an inline local renderer.
