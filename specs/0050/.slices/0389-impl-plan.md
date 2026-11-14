# Step 0389 — impl plan

## Approach

Stage 1 of a staged study fit, by the reference AIR calibration (spec 0050 S11.3; 001 §L). All production code goes in
`OpticalConstructor.Optimization/StudyEngine.fs`, after `fitNonlinear`. All tests go in
`OpticalConstructor.Tests/StudyEngineTests.fs`.

1. `type Angle with member this.parameterValue` and `type Retardance with member this.parameterValue`. Each is the
   radians as a `ParameterValue`. They use BenchNoise's destructuring form (`let (Angle radians) = this in ParameterValue
   radians`), so the tree stays free of ELEV005 round trips.
2. `StudyFitError` gains `StagedPathFitErr of error : StagedMuellerError`. It renders as `the staged Mueller path refused
   the fit: <describe>`.
3. Run families are resolved by moving code, not copying it. `engineWith`'s local `heldIn` and `calibrationRunOf`
   become one private module function, `calibrationFamilyOf`, declared before `engineWith`. `engineWith` and
   `calibrateLinear` both call it. `engineWith` keeps its `heldIn` closure for the black-box branch. That closure now
   calls a shared private `heldItemsIn`, which is the moved body.
4. `CalibrationRunFamily.stageOneConstants` becomes a list of private `StageOneConstant` records. Each record holds the
   parameter and its value read from a `StageOneCalibrationData`. The parameter-to-constant table is then written once:
   `engineWith` reads the parameters and `calibrateLinear` reads the values.
5. `StagedStudy.calibrateLinear context prefits solver`:
   - the layout over the staged items;
   - each fitted run's family;
   - the capture rows of every Dark prefit, in prefit order, and of the LpLp, LpCpl and CplLp fitted runs, each in study
     order (the prefit's `Ok (CapturePoints rows)`, already `keptBy` its exclusions in the pre-fit check);
   - `calibrateFromAirRows solver`, its error mapped to `StagedPathFitErr`;
   - each layout entry takes the first constant naming its parameter;
   - the answer has one member: start = the start vector, `MemberConverged (values, None)`, fitted = values, and no
     object matrices.
6. Tests:
   - the two new facts over a test-local `StageOneCalibrationData` truth;
   - a new LP2 draft (real linear, `DialNegative`) and its two AIR benches;
   - stage-one runs scanning 0°–160° by 20°;
   - the rows built through `airIdentityPred`;
   - the widened describe fact.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Optimization/StudyEngine.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/StudyEngineTests.fs`

## Risks

- **Refactor parity.** `engineWith`'s behaviour must not change. The existing engine facts guard it.
- **Prefit rows.** The pre-fit check has already applied `keptBy`, so applying it a second time would renumber the
  points and exclude the wrong ones. The rows are therefore taken as the check gives them.
- **Prefit failures.** A failed pre-fit check on a run stage one reads is refused as
  `StudyForwardFitErr (PrefitFailedErr …)`, as `dataWith` refuses it. A Dark or AIR run without capture rows is refused
  as `StudyForwardFitErr (ReadingMismatchErr …)`. Neither is dropped silently, and no new case is added for them.
- **ELEV rules** on the new private record (labels unique, no primitives) and on the extension members (ELEV019).
