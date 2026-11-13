# Step 0356 — impl plan

Move the hand-written per-family AIR gain rules out of `MuellerReconstruction` into the
gain models of a new Domain module `Studies` (spec 0050 S11.1, 001 §L). This is a MOVE:
`familyGain`, `GainCalibration`, `RepeatPairGain`, `SplitGain`, `CplCplGain` and
`InterpolatedGain` are deleted in the same step that creates their new home.

## Files

- `OpticalConstructor.Domain/Studies.fs` (new, compiled directly after `ImportProfiles.fs`):
  `StudyError` (three `…Err of reason` cases with `describe`), `AirRepeatPair`,
  `SessionGainModel` (free / anchored / interpolated, case form, no int tag), `AirAnchorGain`,
  `GainInterpolation` (`InterpolatedGain` moved and renamed), `SessionGainRule` with `gainAt`
  (the `Day1Interp` arm moved verbatim), and the `SessionGainModel.ruleWith` extension
  (`meanPair` moved; more than two anchors → sum over count).
- `OpticalConstructor.Domain/OpticalConstructor.Domain.fsproj`: one `<Compile Include="Studies.fs" />`.
- `OpticalConstructor.Domain/MuellerReconstruction.fs`: delete the six retired symbols; point
  the Stage-3 comment and `correctSignal`'s doc at `SessionGainRule.gainAt`.
- `OpticalConstructor.Optimization/StagedMuellerPath.fs`: the nine AIR run names become
  `ExperimentName`s; `airBlock` matches rows by `experiment.nameText.toExperimentName`; each
  block becomes an `AirAnchorGain`; six private gain models resolve through `ruleWith` (a refusal
  is the new `SessionGainErr of error : StudyError`); `prepareRow` divides by the row's session
  rule (CPL-LP pre/post split at the bullshit check); `StagedMuellerData.airGains` replaces
  `gains` / `cplCplDay1Gain`.
- `BerremanTests/MuellerReconstructionTests.fs`: rename the Stage-3 fact and drop its
  `familyGain` block; the §7.2 fact reads each gain from `staged.airGains` by run name.
- `OpticalConstructor.Tests/StudySessionTests.fs` (new, appended last in the fsproj): the four facts.

## Risks

- Bit-identity of the §7.2 numbers: every rule keeps the exact arithmetic (`0.5 * (a + b)`, the
  verbatim interpolation, a single gain unchanged) and the anchor order (`-2` before `-2R`).
- ELEV: no bare local rewrapped at a call site (gain models born as module-level values), unique
  names (checked: none of the new names exists), no `%A` render of an elevated value.
- The existing `StagedMuellerPathTests` fact expects the empty-AIR-block message to name the run;
  `airBlock` keeps that wording.
