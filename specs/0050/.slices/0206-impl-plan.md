# Step 0206 — impl-plan (IMPLEMENT: the detector noise axes in `ItemModels`)

## Approach

Spec 0050 S6 closes on its S7 prerequisite (001 §H, §I, §S6): the shared item contracts land before S7 so that
copying an experiment can map its items. This step defines the data a detector model publishes for its noise
model, without moving the bench noise implementation (that is step 339, 001 §Q / §S10).

In `OpticalConstructor.Domain/ItemModels.fs`, after `ParameterName` and `DialSign` (step 205):

- `ParameterValue of double` with `.value` — one parameter's number in its kind's unit.
- `ParameterKind`, seven cases in this order with wire tags 1–7: `AngleParameter`, `PhaseParameter`,
  `FractionParameter`, `ScaleParameter`, `MuellerElementParameter`, `ThicknessParameter`, `ReadingParameter`;
  `.value : int`, `static member tryOfWire : int -> ParameterKind option` (0 and unknown tags → `None`), and a
  separate `.text` render member (ELEV017: the tag is never the text).
- `ItemParameterAxis = { name : ParameterName; kind : ParameterKind; defaultValue : ParameterValue }` with
  `static member detectorNoiseAxes rotation element throughput : ItemParameterAxis list` returning
  `rotationUncertainty` (angle), `elementUncertainty` (fraction), `throughputUncertainty` (fraction) with the
  supplied values, in that order.

Domain declares and references none of the bench-noise types; no constants are copied from the test harness.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/ItemModels.fs` — the three types.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/ItemModelsTests.fs` — the new fact
  `the detector noise axes keep their declared quantities` (names, kinds, order, exact defaults; every tag
  round-trips; 0 and 8 read as `None`).

Both files are already compiled by their projects (step 205), so no project file changes.

## Risks

- Name uniqueness (ELEV014 / ELEV016): the three type names and seven case names are checked against the
  Berreman tree and STL — none exists today.
- The test must not claim to test a bench model: it is a data projection over test-local values.
