# Step 0483 — impl plan

## Approach

Obligation S142-INSTRUMENT: the instrument-matrix condition number for choosing the polarizer and compensator states.

In `MeasurementPlanner.fs`:

1. `PlannerError` gains `InstrumentEvaluationErr of reason : string`, described as
   `an optic of the kit cannot be evaluated: <reason>`.
2. After step 482's `type PlannerInputs with` block (`polarimeterOpt` / `unplacedPlates` / `candidates`):
   - `InstrumentChoice = { polarizer; compensator; angles : Angle list; condition : ConditionNumber }`, with the
     planner's constant `compensatorAngleGrid` (0°, 15°, …, 165°) as a static member;
   - `InstrumentFinding = ConditionedInstrumentFinding of InstrumentChoice | CompensatorMissingFinding of PlannerOptic`
     with `text` under the invariant culture, angles through the module's `Angle.plannerDegreesText`;
   - `type PlannerInputs with member this.instrumentFindingsWith items choiceOf materials`.
3. The evaluation reuses the one item evaluator: `ItemModelChoice.boundWith materials (ParameterValues.ofParameters
   item.parameters)`, then `BoundOpticalElement.muellerAt (ElementOrientation.transmittingAt θ) λ` at the first source's
   line. The generator state is `s(θ) = M_C(θ) · M_P(0) · Propagation.unpolarizedStokes`. The analyzer row is row 0 of
   `M_P(0) · M_C(θ)`. Each instrument row is `MuellerReconstruction.designRow s a`. Conditions come from
   `ResidualJacobian.conditionNumber` (`MuellerInverse.fs`).
4. The angles are chosen from the 495 four-subsets of the grid, enumerated in lexicographic order. The least generator
   condition wins, and on a tie `List.minBy` keeps the earliest set. The analyzer turns to the same four angles.
   The instrument matrix is the 16 × 16 stack of `designRow`s, each generator state with each analyzer row.

In `PlannerInputsTests.fs`, three facts over step 470's items (Laser, LP1, RET, CPL1):

- the paired finding, with the test computing every one of the 495 generator conditions through the same evaluator;
- the missing-compensator finding, with a compound optic left unpaired, and no source giving no finding;
- the refusal of an unresolved optic, naming RET.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/MeasurementPlanner.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/PlannerInputsTests.fs`
- Consequential, one line: `Berreman/OpticalConstructor/OpticalConstructor.Tests/PlannerReportTests.fs`. Its exhaustive
  `PlannerError` or-pattern needs the new case, because FS0025 is an error under `--warnaserror+:25`. This is the
  precedent of steps 470, 471 and 477.

## Risks

- **Exhaustive matches on `PlannerError` in tests.** FS0025 is an error. Two or-patterns must gain the case, each on
  its existing line (a wrapped or-pattern inside `Error (…)` fails with FS0583).
- **ELEV rules.**
  - No new primitive in a public signature.
  - `ConditionNumber`'s float is read by destructuring in `text`.
  - The constant grid uses a qualified `deg` measure, so no new `open` can shadow a name.
- **Floating-point ties between mirror-image angle sets.** The test compares against conditions computed the same way
  (same rows in the same order), so the chosen set's condition is bit-identical to the test's.
