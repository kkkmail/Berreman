# Step 0351 — impl log

## Progress

- [x] Read the protocol, project prompt, slice, AGENTS.md, `StagedMuellerPath.fs`, `MuellerReconstruction.fs` (gain
      and reconstruction types), the section 7.2 fact, `MuellerReference.fs` and `StagedMuellerPathTests.fs`.
- [x] `StagedMuellerPath.fs`: `ObjectReconstructionErr`, `StagedMuellerData`, `reconstructStaged`.
- [x] `MuellerReference.fs`: the nine AIR gains and the `dataMatrixTol` doc line.
- [x] `MuellerReconstructionTests.fs`: the section 7.2 fact calls the staged path.
- [x] `StagedMuellerPathTests.fs`: the two new facts.
- [x] Development runs of build, elevation, constructor-unit-tests, unit-tests (selected class) and the 20 s35
      invariants.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Optimization/StagedMuellerPath.fs`:
  - **`StagedMuellerError`** gains `ObjectReconstructionErr of kind : MatrixKind * error : SolverError`.
  - **`StagedMuellerData`** is new, after `calibrateStageOne`: `stageOne`, `gains`, `cplCplDay1Gain`, `qz`, `lr`,
    `qzLr`. Its doc is two lines.
  - **Private helpers**, the moved Stage 2/3 bodies:
    - `AirBlockRows`, a private non-empty block (`firstRow`, `laterRows`). Its members are `rows`, and `meanTime`,
      which is the fact's `meanTime` with no `[]` arm.
    - `airBlock`: the fact's `airRowsOf`. It answers `EmptyCaptureSetErr` naming the label when no row matches.
    - `fitAirGain`: the fact's body, over `block.rows`.
    - `repeatPairGain` / `splitAirGain` / `interpolatedAirGain`: the fact's `calibration` / `day1Cal` record
      builders, one per gain rule.
    - `airGainRules`: the nine blocks in the brief's order. It answers `GainCalibration * InterpolatedGain`, where
      `cplCpl` is `Day2Single`.
    - `prepareRow`: the fact's projection in step 350's form, with `darkMean` as its first parameter.
  - **`reconstructStaged solver excluded captures`** runs, in order:
    1. `calibrateStageOne`, whose error passes through;
    2. the split time, the earliest bullshit-check `capturedAt`; with no row it answers `EmptyCaptureSetErr` naming
       the bullshit checks;
    3. `airGainRules`;
    4. the five `families`, the fact's list;
    5. a local `reconstructObject`, which concatenates the families' `buildDesign` parts in order, calls
       `reconstruct`, and maps the error to `ObjectReconstructionErr (kind, e)`. It runs for `Qz`, `Lr` and
       `QzLrProduct`, in that order.
- `Berreman/BerremanTestSupport/MuellerReference.fs`:
  - Nine `AirGain` values after `cascadeLargestDeltaPositions`, each with a one-line doc naming its `gain_summary`
    key: `lpLpGainA`, `lpLpGainB`, `lpCplGainA`, `lpCplGainB`, `cplLpGainPre`, `cplLpGainPost`,
    `cplCplDay1GainStart`, `cplCplDay1GainEnd` and `cplCplDay2Gain`. The values were checked against
    `reference_step2_linear_summary.json`.
  - `dataMatrixTol`'s doc gains one line: it also bands the AIR gains, worst 2.9e-11 (`lp_cpl.gain_air2`) when spec
    0050 was authored, 2026-11-04 (~34×, §9).
- `Berreman/BerremanTests/MuellerReconstructionTests.fs`, the section 7.2 fact:
  - It binds `staged` from
    `reconstructStaged (createMathNetSvd ()) MuellerReference.excludedPoints (opticsMuellerCaptures ())`. An
    `Error` fails the fact with `%A`.
  - Deleted: `solver`, `captures`, the five row aliases, the stage-1 call, `sourceLp` / `sourceCpl` /
    `analyzerLp` / `analyzerCpl`, `darkMean`, `splitTime`, `airRowsOf`, `fitAirGain`, `meanTime`, `day1Start`,
    `day1End`, `calibration`, `day1Cal`, `prepareRow`, `families`, `reconstructKind`, and `mQz` / `mLr` / `mComb`.
  - The rank, matrix and cascade assertions read `staged.qz` / `staged.lr` / `staged.qzLr`. The cascade is
    `cascadeProduct staged.lr.matrix staged.qz.matrix`.
  - New: one gain assertion over all nine deviations. `describeGainDeviation` is its formatting, and `day2Gain`
    fails the fact on a `Day1Interp`.
  - The leading comment is two lines (spec 0050 S10.3). The §8.4 / §8.5 paragraphs keep their text. The moved
    code's comments went with it.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/StagedMuellerPathTests.fs`:
  - The module doc now covers the staged path, not only stage one.
  - New fixtures: `bullshitCheckRow`, `airRows label`, and `firstBlocksOnly`.
    - `airRows label` is nine readings at 0°…160°, `avgTotal` 2 + cos 2α, at a fixed literal time.
    - `firstBlocksOnly` holds the LP-LP, LP-CPL and CPL-LP first blocks, the darkness row and the bullshit-check
      row, and empty day files.
  - Two new facts:
    - `the staged path answers a typed error naming an AIR block that holds no capture row`: over
      `firstBlocksOnly`, it expects `EmptyCaptureSetErr` holding `LP-(AIR#0)-LP-2R`.
    - `the staged path answers a typed error naming the object whose solve fails`: over all nine blocks and no
      science row, it expects `ObjectReconstructionErr (Qz, SingularDesignErr _)` from the real
      `createMathNetSvd ()`.

## Decisions

- **A private non-empty block type (`AirBlockRows`).**
  - The fact's `meanTime` had a `[]` arm that threw.
  - `airBlock` answers a block only when one row matches, so `meanTime` is total with no unreachable arm.
  - `rows` rebuilds the same list in the same order (`firstRow :: laterRows`), so `fitAirGain` and `meanTime` do
    the same float operations on the same rows.
- **One helper per gain rule** (`repeatPairGain`, `splitAirGain`, `interpolatedAirGain`) plus `airGainRules`.
  - This keeps the `Result.bind` chain at five levels; nine flat blocks would be nine levels.
  - There is no `result { }` builder in the tree, in STL's Sys either. The only ordered collector
    (`ItemEvaluation.sequenced`) is typed to `ItemEvaluationError`.
  - The blocks are still read in the brief's order: LP-LP 2 / 2R, LP-CPL 2 / 2R, CPL-LP 2 / 2R, CPL-CPL day-1
    start / end, then day 2. So the first empty one is the one named.
- **The nine labels are written where they are used**, as `MuellerExperimentLabel "…"` literals in `airGainRules`.
  - The doc says they are the reference procedure's own until S11.1 turns them into session data.
  - ELEV001 fires only on `X y.value` re-wraps, never on a literal.
- **Design concatenation uses `Array.concat`** in place of the fact's `List.collect List.ofArray |> List.toArray`.
  Both give the same rows in the same order, and neither does arithmetic.
- **The gain assertion sits before the matrix assertion**, in pipeline order (stage 2, then 3). It reports all nine
  deviations in one message, as the matrix assertion does.
- **The synthetic darkness row is the existing `darknessRow` (869.5).** With the AIR rows at 2 + cos 2α, the
  dark-subtracted signals are negative, but every stage-1 solve has rank 2 and every gain is finite. Reusing the row
  avoids a second darkness fixture. The brief asks only that stage 1 solves.
- **`excluded` is `[]` in the two constructor facts.** No science row exists, so the excluded points are
  irrelevant there.

## Testing state

Development runs of each gate's own command from `0351.gates`, over the final tree. The arc-runner's gate engine runs
the gates themselves.

| Gate | Result | Time | Deadline |
|---|---|---|---|
| build | exit 0, no `error`. Full build: warnings were only NU1902 (exempt) and the vendored MathNet SYSLIB0051. Incremental rebuild after a final doc-comment edit: NU1902 only | 77 s full, 7 s incremental | 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)`; known findings 0; suppressed 1506 (unchanged, ELEV004 excluded) | 217 s, then 224 s on the final tree | 600 s |
| constructor-unit-tests | Passed!, 1557 of 1557, 0 failed, 0 skipped (1555 + the 2 new facts, both Passed in the TRX) | 69 s | 180 s |
| unit-tests (`BerremanTests.MuellerReconstructionTests`) | Passed!, 25 of 25, 0 failed, 0 skipped (count unchanged; the 7.2 fact Passed in the TRX) | 3 s | 7200 s |
| invariant-s35-\* (20) | each decoded `git grep` found nothing (exit 1, empty), both as written and with `--untracked` | under 1 s each | 300 s |

- No run went past its effective deadline.
- Elevation ran twice: once before the final doc-comment edit to `airGainRules`, and again on the final tree. Both
  were OK with the same counts.
- **The numbers are bit-identical by construction.** The path applies the fact's own float operations, in the same
  order and on the same rows:
  - `fitAirGain`'s two `List.map`s and `scalarGain`;
  - `meanTime`'s tick average;
  - `prepareRow`;
  - `familyGain` over the same bundles;
  - `buildDesign` over the same five families, in order;
  - `reconstruct` through the same solver.
- The 7.2 fact passes its unchanged matrix and cascade bands, and also the new gain band (`dataMatrixTol`).
- LF only: `git diff --numstat` equals `--ignore-cr-at-eol`, and no edited file contains a CR.

## Artifacts

- `.artifacts/0351-build.log` (the final, incremental build)
- `.artifacts/0351-elevation.log`
- `.artifacts/0351-unit-tests.log`
- `.artifacts/0351-constructor-unit-tests.log`
- `.artifacts/0351-invariants.log`
