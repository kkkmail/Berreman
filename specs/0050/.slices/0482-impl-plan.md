# Step 0482 — impl plan

## Approach

Add the Measurement Planner's candidate experiments (spec 0050 S14.2, obligation S142-KIT-CANDIDATES) to
`OpticalConstructor.Domain/MeasurementPlanner.fs`, right after step 477's `NoiseVariance` / `CandidateRows`:

1. `PlanCandidate = { source : PlannerSource; plate : PlannerPlate; cut : SampleCut; observable : Observable }` with
   the planner's constant ranges `incidenceRange` (0…60°, 4 points) and `azimuthRange` (0…67.5°, 4 points), its
   `capture : MeasurementMode` (T → `CaptureTransmitted`, R → `CaptureReflected`) and its `caption`
   (`<label> (<cut.plateText>), transmitted|reflected, <λ nm F1> nm`, invariant culture, λ through the sole
   `Units.wavelengthToUnit` seam).
2. `PlanCoverage = CandidateInKit of ExperimentName | CandidateNotInKit` (derived, no wire tag), and
   `PlanCandidate.coverageIn setups experiments`: the first experiment whose setup resolves (`slotsIn`), whose Object 1
   slot (`SetupSlot.inLightOrder`) holds the plate's item, and whose scan is exactly one block, read on `this.capture`
   or `CaptureBoth`, of a `VaryR2` axis then a `VaryR1` axis on that slot over the two ranges, with no
   `sourceWaveLengthOpt`; labels and role are not compared. Written with pattern matches, no `bool` in a signature.
3. A `type PlannerInputs with` block (same file and module, so intrinsic members) adding `polarimeterOpt` (first
   `RecordsMuellerMatrix` detector), `unplacedPlates` (plates whose `normalAxisOpt` is `None`) and `candidates` (empty
   without a polarimeter; else source × plate × observable, each plate's cut the first of `[ZCut; XCut; YCut]` whose
   `normalAxisOpt` equals the plate's, observables in `looks`' emission order).

`MeasurementPlanner.fs` gains `open OpticalConstructor.Domain.Experiments` and `open OpticalConstructor.Domain.Setups`
(both compile before it).

## Tests

`PlannerInputsTests.fs` gains a third plate QZ-T (EulerRotation, ZmYpXp, theta 30°, no principal axis on the normal)
and the two facts the step names:

- `a kit's candidates are each placed plate on each branch it emits at each source's line, read by its polarimeter`
- `a candidate is in the kit once an experiment over its plate sweeps its angles on its branch` — over a `Bench` setup
  built as `SetupTests` / `PrefitCheckTests` build theirs (src / sample / det slots, items placed by `withItem`).

The constructor-unit-tests count rises by exactly 2.

## Risks

- Name uniqueness (ELEV014/015/016/019): `PlanCandidate`, `PlanCoverage`, `CandidateInKit`, `CandidateNotInKit` are new
  (grepped); the new members sit on our own types.
- Opening `Experiments` / `Setups` in `MeasurementPlanner.fs` could shadow a name; the build decides.
- Exact `SweptValue` equality: 60·k/3 and 67.5·k/3 are exact in IEEE doubles, so the lists compare exactly.
