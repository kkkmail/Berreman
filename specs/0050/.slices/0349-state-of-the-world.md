# Step 0349 — state of the world

## Where we are

This step is part of spec 0050 S10.3 (001 §Q AT-1, §S10): the staged Mueller pipeline and its reference values are
promoted.
- Step 0348 moved the reference pipeline's expectations into `BerremanTestSupport.MuellerReference`.
- This step promotes Stage 1, which `MuellerReconstructionTests` composed by hand. It becomes the first half of the
  study's staged Mueller path, `OpticalConstructor.Optimization.StagedMuellerPath`, and both data-backed facts now call
  it.
- This is a MOVE. `reduceAirTraces`, `lpFrameCoeffs` and `unwrapFit` no longer exist in the test.
- Steps 0351 and 0352 add the later stages and their own error cases.

## What's working

- Added `StagedMuellerPath` in Optimization: `StagedMuellerCaptureData`, `StagedMuellerError` and
  `StageOneCalibrationData` with its `lpAnalyzer` / `cplAnalyzer` / `cplSource` stage-2 models.
- Added `calibrateStageOne`, the stage-1 AIR calibration as a pure function of the capture rows and a
  `MuellerSolverProxy`. An empty darkness set and a failed AIR fit answer typed errors instead of throwing.
- Moved the AIR trace reduction and the LP-frame fit out of `MuellerReconstructionTests` into the module (private).
  The test's `failwith` helpers are deleted.
- Re-pointed the Stage-1 and section 7.2 facts to `calibrateStageOne` over `opticsMuellerCaptures ()`. The numbers are
  bit-identical, and the class is still 25 of 25.
- Added `StagedMuellerPathTests` with two typed-error facts; constructor-unit-tests went from 1553 to 1555.

## Tests

These are development runs of each gate's own command from `0349.gates`, over the final tree. The arc-runner's gate
engine runs the gates themselves.

| Gate | Result | Time | Deadline |
|---|---|---|---|
| build | exit 0, no `error` in stdout; only NU1902 (exempt advisory) and the vendored MathNet SYSLIB0051 warnings | 76 s full, 11 s final incremental | 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)`; known findings 0; ELEV004 excluded | 226 s | 600 s |
| constructor-unit-tests | Passed!, 1555 of 1555, 0 failed, 0 skipped (checkpoint 342: 1553, so +2) | 71 s | 180 s |
| unit-tests (`BerremanTests.MuellerReconstructionTests`) | Passed!, 25 of 25, 0 failed, 0 skipped | 3 s | 7200 s |
| invariant-s35-\* (20) | each grep found nothing, both as written and with `--untracked` | under 1 s each | 300 s |

- No run went past its effective deadline. Constructor-unit-tests took 71 s whole, longer than the snapshot's 13 s
  measured actual but inside its 180 s deadline.
- The TRX shows the two new facts and both changed facts `Passed`.
- Bit-identity: `.artifacts/0349-bit-identity-check.fsx` replays the deleted helpers verbatim against
  `calibrateStageOne` over the same rows (`.artifacts/0349-bit-identity-check.log`). All eight values match by
  `DoubleToInt64Bits`: the six constants, the CPL analyzer's negated retardance and the LP analyzer's zero dial.
- All touched files are LF-only: `git diff --numstat` equals `--ignore-cr-at-eol`, and the new files hold no CR.

```yaml
gates:
  constructor_unit_tests: 1555
  berreman_unit_tests: 25
```

## Architecture

- `StagedMuellerPath.fs` compiles directly after `FitUncertainty.fs`. It opens `OpticalConstructor.Domain` (for
  `Library.PolarizerKind`), `ItemModels` (`DialSign`) and `MuellerReconstruction`. No project reference changed:
  BerremanTests already referenced Optimization, and OpticalConstructor.Tests already referenced it too.
- The path is pure over the parsed capture rows. The kit read (`loadDataSet`) stays in the test as its IO.
- `calibrateStageOne` chains the three solves with `Result.bind`, in the reference order: LP-LP, then LP-CPL, then
  CPL-LP phased on the LP zero. `Result.mapError` tags a `SolverError` with its `Family`, so the first failure stops
  the path.
- `reduceAirTraces` takes its dark mean as `DarkMean` and reads `.value` only where it subtracts. The LP-frame helper
  (`lpFrameSourceModel`) binds each coefficient as a `CosineCoefficient` where it reads it, and answers
  `Result<SourceCplModel, SolverError>` instead of the old `CosineCoefficient * CosineCoefficient`.

## Deferred

- Nothing. Every item of the brief is in place.

## Gotchas

- **The section 7.2 fact keeps its five row names** (`lpLpRows` … `cplCplDay2Rows`), now bound from `captures`. That
  keeps its Stage 3 text byte-for-byte unchanged, as the brief asks.
- **The Stage-2 sign note moved with the models.** "Analyzer retardance negative, only the relative sign is
  constrained (`matrix_glue.py:211`)" now lives on `cplAnalyzer`'s doc and is no longer in the fact.
- **An empty AIR set is a solver error, not a throw.** `reduceAirTraces` over no rows yields an empty trace, because
  its `List.averageBy` calls run per group. The real solver refuses the empty design as `SingularDesignErr`, which is
  why "one darkness row and no other row" answers `AirCalibrationFitErr (LpLp, SingularDesignErr _)`.
- **Suppressed findings rose from 1502 to 1506, all ELEV004.** They are the four `.value` reads in the moved
  `reduceAirTraces` body and the dark mean: `experiment`, `avgTotal` twice, and `darkMean` where it subtracts, as the
  brief prescribes. ELEV004 is measured but excluded from the gate (spec 0048 `.manual/002` R7). No enforced rule found
  anything.
- **The helper's name is this step's choice.** The brief describes the LP-frame helper's shape but not its name;
  `lpFrameSourceModel` says what it answers.

## Changelog

- 2026-11-12 — Step 0349: promoted Stage 1 of the reference pipeline into
  `OpticalConstructor.Optimization.StagedMuellerPath` (`calibrateStageOne`, typed `StagedMuellerError`) and re-pointed
  `MuellerReconstructionTests` to it (001 §Q AT-1, S10.3). Added `StagedMuellerPathTests` (+2). Build, elevation,
  constructor-unit-tests (1555), unit-tests (`MuellerReconstructionTests`, 25) and all 20 s35 invariants are green.
