# Step 0401 — impl log

## Progress

- [x] `AirRunRows`, `AirFamilyRuns` (+ `stageOneRows`) and `StagedStudy.airRunsWith`. `calibrateLinear`'s selection moved
  into `airRunsWith`. `calibrateLinear` is now `calibrateFromAirRows solver` of `(airRunsWith …).stageOneRows`, plus
  its unchanged constants mapping.
- [x] `StagedStudy.calibrationProblemsWith`, after `objectProblemsWith`.
- [x] The three authorized facts in `StudyEngineTests`. 29/29 of the class pass, step 389's two `calibrateLinear` facts
  among them, unchanged.
- [x] Mutation check: three mutations, each failing the facts that cover it (see Testing state). The production file
  was restored and rebuilt.
- [x] Development runs: build, the twenty s39 invariants, unit-tests (selected class), constructor-unit-tests (whole),
  elevation.
- [x] State of the world.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Optimization/StudyEngine.fs` (+210 / −40)
  - `let private calibrationFamiliesIn staged context`: the `(run, CalibrationRunFamily)` selection `calibrateLinear`
    held inline (`fittedHeldIn` then `calibrationRunOf`), moved here. `airRunsWith`, `calibrateLinear` and
    `calibrationProblemsWith` all read it.
  - `type AirRunRows = { run : ExperimentName; rows : MuellerRawRow list }`.
  - `type AirFamilyRuns = { darkRows; lpLpRuns; lpCplRuns; cplLpRuns }`, with
    `member this.stageOneRows : StageOneAirRows`, the Dark rows and each family's rows run after run.
  - `StagedStudy.airRunsWith context prefits : Result<AirFamilyRuns, StudyFitError>`. It holds `calibrateLinear`'s
    former `isDark` / `ofFamily` selection unchanged, so the Dark rows come in prefit order and each family's runs in
    study order. Each run's rows are its prefit's `captureRows`, the check's `CapturePoints` kept by `keptBy`.
  - `calibrateLinear`: same signature and doc. Its body is now `airRunsWith`, then `calibrateFromAirRows`, then the
    unchanged constants mapping.
  - `let private runResidualFrom run residuals : RunResidual`: the RMSE-of-a-residual body that
    `nonlinearProblemsWith`'s `runResidualOf` held, moved so both engines' run residuals share it.
    `nonlinearProblemsWith` now calls it.
  - `StagedStudy.calibrationProblemsWith context prefits solver : Result<EngineProblems, StudyFitError>`.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/StudyEngineTests.fs` (+184 / −5)
  - `jittered` now takes its amount (`jittered 0.01 runs` at step 400's two call sites, unchanged in effect). The
    calibration facts can then move the AIR rows by 2 · (−1)ⁱ without a second copy of its body.
  - A new S11.4 section after the object-stage problems: the private helpers `stageOneRuns`, `movedStageOneRuns`,
    `calibrationProblemsOver`, `stageOneOf` and `rootMeanSquareOf`, and the three authorized facts.

## Decisions

- **Placement deviation (F# declaration order), as in step 400.** The slice puts (1) and (2) after
  `objectProblemsWith`. But `calibrateLinear`, which comes much earlier in the file, must call `airRunsWith`, and a type
  extension only sees members declared before it. So `AirRunRows`, `AirFamilyRuns` and `airRunsWith` sit directly
  before `calibrateLinear`, right after `ExperimentPrefit.captureRows`, which they read. `calibrationProblemsWith`
  follows `objectProblemsWith`, as the slice says.
- **`calibrateLinear` answers what it answered before.** The rows `stageOneRows` hands `calibrateFromAirRows` are the
  same rows in the same order: the Dark runs' rows concatenated in prefit order, then each family's runs' rows in
  prefit (study) order. Errors come in the same order too: Dark, LP-LP, LP-CPL, CPL-LP, each the first refused prefit
  through `ItemEvaluation.sequenced`. The constants mapping is untouched. Step 389's two facts pass unchanged, with
  exact equality on every constant.
- **Coordinates are the families' stage-1 constants by position.** Coordinate k of family F's fringe is the k-th entry
  of its runs' `stageOneConstants` (step 389's list), so:
  - LP-LP is [`lpZero`] plus `FringeVisibilityCoordinate`;
  - LP-CPL is [`cplZero`; `cplAnalyzerRetardance`];
  - CPL-LP is [θ − 45°; δ].

  This is the order the slice names. The residuals read s₀ and s₁ in that order.
  - The `EntryCoordinate` holds every layout entry whose parameter is any of F's runs' k-th constant parameters, in
    layout order. Two LP-LP runs behind one LP2 give that one entry (asserted).
  - The start is that constant's `constantIn` of stage one's answer, the same function `calibrateLinear` writes the
    entry from, so the two are equal bitwise (asserted with `Assert.Equal<float>`).
- **Bounds.** A coordinate's lower bound is the largest of its entries' lower bounds, and its upper bound the smallest
  of their upper bounds: within every one of its entries' bounds. A coordinate with no entry, the visibility included,
  is unbounded (±∞), as step 400's series elements are.
- **A family without a run is refused, never guessed.** Its constants' starts come from its runs. If a fitted family
  held none, `constantsOf` answers `StagedPathFitErr (EmptyCaptureSetErr …)`. With the real solver
  `calibrateFromAirRows` refuses first, because an empty trace is a `SingularDesignErr`, so this is the totality guard
  for a stub solver.
- **The LP-LP visibility start** is `freeCosineFit solver` of the LP-LP trace, refused as
  `StagedPathFitErr (AirCalibrationFitErr (LpLp, e))`, as the slice says. It is the same fit stage one made, so its zero
  is `lpZero`.
- **Run residuals** come in family order LP-LP, LP-CPL, CPL-LP, each family's runs in study order. Each is its own
  trace (`reduceAirTraces c.darkMean run.rows`) under its family's residual at the problem's start. `reduceAirTraces`
  normalises per experiment label, so a family's residual at its start is its runs' residuals run after run (asserted
  over two LP-LP runs).
- **Elevation.** Every elevated value is born in a `let` (`Angle s.[0]`, `FringeVisibility …`, `Retardance s.[1]`),
  never wrapped at a call. The new private helpers take two parameters, never one of our types alone (ELEV013P).
  "No other member is added" holds: the only new members are `stageOneRows`, `airRunsWith` and
  `calibrationProblemsWith`. The two private `let` helpers are not members.

## Testing state

Development runs on 2026-11-14. These are not gate verdicts; the supervisor re-runs every gate.

- build — `dotnet build Berreman.slnx -c Release -nologo -v:m`: exit 0, 0 errors, 6 warnings, all the exempt `NU1902`.
  Log: `0401-dev-build.log`.
- invariants — each of the twenty s39 commands answered `INVARIANT HOLDS … found nothing`. Log:
  `0401-dev-invariants.log`.
- unit-tests, selected `BerremanTests.MuellerReconstructionTests`: 25/25 passed in 4 s. Log: `0401-dev-unit-tests.log`.
- `StudyEngineTests` class: 29/29 passed. Log: `0401-dev-studyengine-tests.log`.
- Mutation check (`0401-dev-mutations.log`), each mutation applied alone, rebuilt, run against the class, then undone:
  - A, LP-CPL residual without `cos`: fails only `the calibration fit's problems are the three AIR fringes …`.
  - B, a run residual over its family's trace: fails only `each AIR run of a calibration fit has its own trace's
    residual`.
  - C, no entry gathered into a coordinate: fails all three new facts.

  A's undo first failed, because its mutated line also matches the LP-LP line. B and C were therefore first run with
  A still applied. A was restored by hand at its line (verified), and B and C were re-run alone ("clean" in the log).
  The results above are the clean ones. The solution was then rebuilt from the restored source.
- constructor-unit-tests (whole, with its TRX): `Test run summary: Passed!`, 1716/1716 succeeded, 0 skipped, in
  138 s against a 180 s deadline. That is 1713 + 3. Log: `0401-dev-constructor-unit-tests.log`.
- elevation (`Run-ElevationGate.ps1 -ExcludeRules ELEV004`, no base, so all 13 projects):
  `ELEV-GATE: OK - 0 new finding(s) in 13 project(s), 215s`, with 0 known findings. Measured 216 s against a 600 s
  deadline. Log: `0401-dev-elevation.log`.
- Durations against deadlines:
  - build: 45 s for the full build after the edits, then 9 s for the incremental rebuild after the mutation check
    (600 s deadline);
  - unit-tests: 4 s (7200 s).

  No run exceeded its deadline.
- Line endings: the two source files and the three slice files hold no carriage return (`tr -cd '\r' | wc -c` is 0
  for each). `git diff --numstat` equals `--ignore-cr-at-eol` (+210 / −40 and +184 / −5).
- `specs/0050/.manifest.state.json` was already modified before this round. It is the supervisor's file and was not
  touched.

## Artifacts

All under `specs/0050/.artifacts/`:

- `0401-dev-build.log`
- `0401-dev-invariants.log`
- `0401-dev-unit-tests.log`
- `0401-dev-studyengine-tests.log`
- `0401-dev-mutations.log`
- `0401-dev-constructor-unit-tests.log`
- `0401-dev-elevation.log`

## Gotchas

- **A start can lie outside its own bounds.** Stage one reports a zero within [0°, 180°), so CPL2's −2° truth is 178°
  (3.106 rad), while CPL2's `dialZero` entry is bounded to ±0.5 rad. The slice prescribes both the start (the value
  `calibrateLinear` writes) and the bounds (the entries'), and nothing in this step searches. A later profile
  (`profileAt`) of that coordinate would hold no point but the solution, since every x̂ ± jσ is out of bounds.
- **Record-label capture.** `AirRunRows` brings new `run` and `rows` labels into scope ahead of `ObjectRunSystem` and
  `RunResidual`. Every construction site is annotated. The build shows no inference change.
- At step 389's exact rows every fringe residual is about 1e-16, so the RMSE comparisons there are trivially within
  1e-12. The first fact therefore repeats every check over the moved rows, where the RMSEs are about 1e-3.
- An item that is both a CPL-LP prep and an LP-CPL analyzer would put its `c2.retardance` entry in two coordinates.
  `calibrateLinear` gives it the first family's constant. No fixture or bench does this.
