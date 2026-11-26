# Step 0388 — state of the world

## Where we are

Spec 0050, section S11.3 (engines, staged and joint fits, append-only fit runs). Step 387 added the nonlinear engine.
The linear engine is the staged Mueller path driven by a study (001 §L; the S11.3 brief's "S10.3's staged path"). This
step makes the pieces of that path a study needs callable. Each piece is MOVED out of a private helper of
`StagedMuellerPath` (or an inline expression), and the reference path calls it back:

- stage one over given AIR rows;
- an AIR anchor's gain;
- a capture row's projection;
- the design assembly and its concatenation;
- the stage-one analyzers;
- the error renders.

Steps 389 and 390 build the linear calibration and the object reconstruction on these pieces.

## What's working

- Exposed stage one over caller-selected AIR rows (`StageOneAirRows`, `calibrateFromAirRows`); `calibrateStageOne`
  selects each family file's `(AIR#0)` rows and calls it.
- Moved the AIR gain fit and its mean time into the public `airAnchorGainOf`, and the science-row projection into the
  public `projectCaptureRow`. `reconstructStaged` calls both and keeps no private copy.
- Added `LeastSquaresSystem.ofProjections` / `concat` (called by `buildDesign` and `reconstructStaged`), and
  `AnalyzerModel.linearAt` / `circularAt` (called by `StageOneCalibrationData.lpAnalyzer` / `cplAnalyzer`).
- Rendered every staged path error through `StagedMuellerError.describe`, over the new `Family.text`,
  `MatrixKind.text` and `SolverError.describe`.
- Added five `StagedMuellerPathTests` facts. The reference path's numbers are bit-identical, measured against a
  build of HEAD.

## Tests

Run locally on 2026-11-13 on the final code. Every run finished within its effective deadline.

- `build`: exit 0, 0 errors, 43 s (deadline 600 s). The only warnings are the exempt NU1902 advisories (12 lines).
- `constructor-unit-tests`, whole suite: 1682/1682, 104 s (deadline 180 s).
  - The count is 1677 → 1682, +5, as the acceptance requires.
  - The TRX shows each of the five new ids passing under its exact slice name.
- `unit-tests`, selected `BerremanTests.MuellerReconstructionTests`: 25/25, 3 s (deadline 7200 s). No BerremanTests
  file is edited.
- `elevation`: `ELEV-GATE: OK - 0 new finding(s) in 13 project(s), 222s` (deadline 600 s). This was a standalone run with
  no `-Base`, so all 13 projects were analyzed. Domain and Optimization have 0 findings. No source changed after it.
- The twenty `invariant-s38-*` must-find-nothing greps: all twenty find nothing (each `git grep` exits 1, with no hit).
- Bit-identity measured, beyond the gates. The staged path ran over the seven lab files' rows, frozen once to exact
  IEEE bits, against a build of HEAD and against this build. The 85 output lines are byte-identical:
  - the stage-one constants and both analyzers;
  - the nine AIR gains and their times;
  - M_QZ, M_LR and M_{QZ+LR}, each with its rank and RMSE;
  - the product check.
- Mutation check of the first new fact: with the `(AIR#0)` filter removed from `calibrateStageOne`, that fact fails and
  the other nine of the class pass. The source was then restored byte for byte.
- Every file this step wrote is LF (0 CR bytes); `git diff --numstat` equals `--ignore-cr-at-eol`.

```yaml
gates:
  constructor_unit_tests: 1682
```

## Architecture

- **Moves, not copies.** Each new public piece is the former private body (or inline expression) moved verbatim, with
  the same operands in the same order. The private `AirBlockRows`, `airBlock`, `fitAirGain` and `prepareRow` are
  deleted.
- **The run selection stays with the reference path.** The `toExperimentName = Ok run` filter and the `(AIR#0)` label
  filter stay in the private `airAnchor` and in `calibrateStageOne`. The public pieces take rows the caller already
  selected, so a study can hand them the rows of its own experiments.
- **`ofProjections` is an intrinsic type extension** declared right after `designRow`, which it calls. `designRow` comes
  after the record, so the member cannot sit in the record itself. `concat` reads only the record and is declared in it.
- `reconstructStaged` keeps one local adapter, `projectionOf`. It turns a reading into φ and a session rule into a gain,
  then calls `projectCaptureRow`.

## Deferred

Nothing. Every requirement of the slice landed this round.

## Gotchas

- **Record labels collide across modules.** `Studies` and `ItemModels` declare records with the labels `experiment`,
  `capturedAt`, `description` and `kind`, all of which `MuellerRawRow` / `AnalyzerModel` also use. The staged-path test
  file therefore reaches `ItemModels.DialSign`, `Library.PolarizerKind`, `Experiments.ExperimentName` and
  `Studies.MissingAnchorGainErr` qualified rather than opening those modules.
- **SYSLIB0051 from vendored MathNet.** It appears only when `MathNetNumerics/Numerics` actually recompiles (the step
  0001–0006 logs carry it too); an up-to-date build prints none. The build gate's `error` regex is case-sensitive and
  this warning text has no `error` in it.
- **Escaping on this shell.** `sed` and Python heredocs here unescaped `\\` in Windows paths. Build such paths with
  forward slashes or `chr(92)`, and count CR bytes with Python, not `grep -c $'\r'`.

## Changelog

- 2026-11-13 — Step 0388: made the staged Mueller path's pieces callable by moving them out of private helpers:
  - in `StagedMuellerPath`, `StageOneAirRows`, `calibrateFromAirRows`, `airAnchorGainOf`, `projectCaptureRow` and
    `StagedMuellerError.describe`;
  - in `MuellerReconstruction`, `LeastSquaresSystem.ofProjections` / `concat`, `AnalyzerModel.linearAt` /
    `circularAt`, `Family.text`, `MatrixKind.text` and `SolverError.describe`;
  - five `StagedMuellerPathTests` facts; the reference path's numbers are bit-identical.
