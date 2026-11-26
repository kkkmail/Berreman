# Step 0356 — state of the world

## Where we are

Spec 0050 S11.1 (studies: unknowns, scopes, links, sessions and opened materials), its first step on sessions. Step
0356 makes a session's gain model data. The hand-written per-family AIR gain rules (`MuellerReconstruction.familyGain`
and its `GainCalibration` bundle) are moved into the new Domain module `Studies` as `SessionGainModel` /
`SessionGainRule`, their first implementation (001 §L, .spec-md §6 "Gain rules"). The staged Mueller path now
divides each science row by its session's rule. Later S11.1 steps add the study's unknowns, scopes, links and opened
materials to `Studies.fs`, along with more `StudyError` cases.

## What's working

- Added `OpticalConstructor.Domain.Studies` with `StudyError`, `AirRepeatPair`, `SessionGainModel` (free / anchored /
  interpolated), `AirAnchorGain`, `GainInterpolation`, `SessionGainRule.gainAt` and `SessionGainModel.ruleWith`.
- Moved, not copied: deleted `familyGain`, `GainCalibration`, `RepeatPairGain`, `SplitGain`, `CplCplGain` and
  `InterpolatedGain`. Each of their rules now exists once, in the gain model.
- The staged Mueller path names its nine AIR runs as `ExperimentName`s and resolves six session gain models through
  `ruleWith`. It answers `airGains` (the nine anchors, in reference order) and refuses with `SessionGainErr`.
- Added `StudySessionTests` (4 facts). Renamed the Stage-3 BerremanTests fact. The §7.2 fact now reads its gains
  from `airGains` by run.

## Tests

These are development runs of each gate's own command from `0356.gates`, over the final tree. The gate engine
runs the gates themselves.

| Gate | Result | Time | Deadline |
|---|---|---|---|
| build | exit 0, `Build succeeded`, 0 errors; warnings only NU1902 (exempt) and the vendored MathNet C# `SYSLIB0051` (only when that project recompiles, as in steps 0001–0003); the final incremental build shows NU1902 only | 78 s full, 28 s final | 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)`; known findings 0; suppressed 1510 (ELEV004 excluded) | 220 s | 600 s |
| constructor-unit-tests | Passed!, 1572 of 1572, 0 skipped; checkpoint 354 recorded 1568, so +4 (all four `StudySessionTests` facts Passed in the TRX) | 70 s | 180 s |
| unit-tests (`BerremanTests.MuellerReconstructionTests`) | Passed!, 25 of 25, 0 skipped. That is the class's count before this step (25 at steps 0349–0352). The renamed fact and the §7.2 end-to-end fact Passed | 3 s | 7200 s |
| invariant-s36-\* (20) | each command, run from the snapshot, reports `INVARIANT HOLDS` | under 1 s each | 300 s |

- No run went past its effective deadline.
- The elevation run had no comparison base, so it analysed all 13 projects. That is wider than the gate's arc-origin
  scope.
- The §7.2 fact's matrices, product check and nine AIR gains still sit inside `MuellerReference.dataMatrixTol`, with
  the band and numbers unchanged. Every row divides by the gain the same operations compute.

```yaml
gates:
  constructor_unit_tests: 1572
  berreman_unit_tests: 25
```

## Architecture

- **`Studies.fs` compiles directly after `ImportProfiles.fs`**, as the step places it. Since step 300 four Domain
  files follow `ImportProfiles.fs` (`ExperimentDataProxy`, `ExperimentDataLoad`, `DataSplits`, `ExperimentMatrices`).
  None of the later steps that edit `Studies.fs` (357–471) names anything from them.
- **`SessionGainModel` carries no int tag.** It has payload cases and is stored in the serializer's case form, as
  `SlotTurn` is. `ruleWith` is a type extension declared after `AirAnchorGain` and `SessionGainRule`.
- **Rules keep the reference arithmetic bit for bit:**
  - one run's gain is used as is;
  - two runs give `AirGain (0.5 * (a.value + b.value))` (the old `meanPair`, in model order);
  - more runs give sum over count;
  - the interpolation is the old `Day1Interp` arm, moved verbatim into `SessionGainRule.gainAt`.
- **The CPL-LP split stays in the staged path, as data.** CPL-LP is two one-run sessions (pre-check `-2`, post-check
  `-2R`). A row captured strictly before the earliest bullshit check takes the pre-check rule, exactly as `SplitGain`
  did.
- `StagedMuellerError` gains `SessionGainErr of error : StudyError`, which no committed data reaches.
  `StagedMuellerData` replaces `gains` / `cplCplDay1Gain` with `airGains : AirAnchorGain list`.

## Deferred

Nothing. Every item of the step's brief landed this round.

## Gotchas

- `InterpolatedGain` is a prefix of the mandated `InterpolatedGainRule`, so a "must find nothing" grep for the
  retired record needs a word boundary. The private gain-model values were named to avoid the substring `SplitGain`
  (`cplLpPreCheckGainModel` / `cplLpPostCheckGainModel`).
- `ruleWith` does not reuse `ItemEvaluation.sequenced`, the ordered Result collector of ruling 016: that collector is
  typed to `ItemEvaluationError`, and `ItemEvaluation.fs` is outside this step's edits. `ruleWith` looks each run up
  once, takes the first `Error` in model order (`List.tryPick`), and otherwise the found gains.
- An AIR block's rows are matched by `r.experiment.nameText.toExperimentName = Ok run`. A blank label can never
  match a run, because a blank name is refused.
- The empty-AIR-block message still names the run, which `StagedMuellerPathTests` asserts.

## Changelog

- 2026-11-13 — Step 0356 (spec 0050 S11.1): sessions' gain models become data in the new `Studies` module.
  `familyGain` and its five gain types moved into `SessionGainModel.ruleWith` / `SessionGainRule.gainAt` and were
  deleted. The staged Mueller path divides by each session's rule and answers `airGains`. Four constructor facts were
  added (1572). One BerremanTests fact was renamed, with the class count unchanged at 25. Development runs: build,
  elevation, constructor-unit-tests, the selected unit-tests class and the 20 s36 invariants.
