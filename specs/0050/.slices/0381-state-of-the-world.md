# Step 0381 — state of the world

## Where we are

This is spec 0050 section S11.3: engines, staged and joint fits, and append-only fit runs.

- **Step 0380** gave the ALGLIB Levenberg–Marquardt solve a watch (`IterationWatch.levenbergMarquardtSolver`): it can
  report iterations and be cancelled.
- **The study engine solves from its parameters' own starts within their own bounds.** The S10.1 fit wrapper
  `MuellerFitting.fitWithResidual` always starts at zero inside a symmetric box.
- **This step moves the one bounded LM request into `ScaledSearch.solveWith`,** which both call. The study-engine steps
  that follow (0387, 0397) build a `ScaledSearch` from a study's own start and bounds, and solve it under a watch.

## What's working

- Add `MuellerFitting.ScaledSearch` (start, lower and upper bounds) with `solveWith`, the one bounded Levenberg–Marquardt
  request (400 iterations, step tolerance 1e-12), solved under an `IterationWatch`
- Add `SearchBox.centredSearch`: the zero start within ± the half-width in every coordinate of a `ParameterScaling`
- Rewire `fitWithResidual` to `(box.centredSearch scaling).solveWith UnwatchedIterations`; its fits are bit-identical
  to before
- Add `StudyEngineTests`, with two facts: a scaled search's start and box, and a search box's centred search
  (constructor suite 1650 → 1652)

## Tests

```yaml
gates:
  constructor_unit_tests: 1652
  berreman_unit_tests: 12
```

| Gate | Result | Time (deadline) |
|---|---|---|
| build (`dotnet build Berreman.slnx -c Release -nologo -v:m`, from `Berreman/`) | exit 0, `0 Error(s)`, no lowercase `error`. The only warnings are the exempt NU190x advisories and vendored MathNet's `SYSLIB0051` | 78 s full; 29 s for the final incremental build (600 s) |
| constructor-unit-tests (the gate's own command, whole suite) | `Test run summary: Passed!`, 1652 succeeded (1650 at checkpoint 380, +2), 0 failed, 0 skipped. In the TRX, both new `StudyEngineTests` facts are `Passed` | 75 s (180 s) |
| unit-tests (selected `BerremanTests.AbsorbingBiaxialInverseTests` + `BerremanTests.FeasibleBiaxialInverseTests`) | `Test run summary: Passed!`, 12 of 12 (7 + 5), 0 failed, 0 skipped. In the TRX, all twelve are `Passed` | 299 s (7200 s) |
| elevation (`ASG_ARC_BASE_COMMIT` = arc base `f395597c`, ELEV004 excluded) | `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)`, exit 0. Suppressed findings are 1539, and 34 in `OpticalConstructor.Optimization`, both as at step 0380. The first run failed with ELEV010 on the slice's `(dimension : int)`; see Gotchas | 232 s (600 s) |
| the twenty s38 invariants | each prints `INVARIANT HOLDS` and exits 0 | seconds each (300 s) |

These are development runs; the supervisor's gate engine is the authority. Nothing is deferred, and every duration is
within its deadline. The final constructor-suite and elevation runs are over the final source. The BerremanTests run
preceded only a doc-comment rewrap in `MuellerFitting.fs` and test-file formatting.

**"Numbers unchanged".** The two BerremanTests classes assert tolerances and print no fitted numbers. A development
probe therefore ran the new `fitWithResidual` against HEAD `ea089620`'s request, transcribed, through
`createAlglibLevenbergMarquardt ()`:

- the cases: five four-coordinate fits (linear interior; linear pinned at +50; Rosenbrock pairs; the same in a 0.5 box;
  exp–sin in a 3 box);
- solutions, final residuals and iteration counts agree bit for bit (`specs/0050/.artifacts/0381-bit-identity.log`,
  probe `0381-bit-identity-probe.txt`).

## Architecture

- **`ScaledSearch`** (`MuellerFitting.fs`, after `SearchBox`) is `{ start; lowerBounds; upperBounds }`, all
  `ScaledVector`. It says where a scaled solve starts and the box it keeps to.
  - **`solveWith (watch) (residual)`** is the only place that builds the bounded `NonlinearRequest`. It solves it
    through `watch.levenbergMarquardtSolver` (step 380). The study engine passes a `WatchedIterations`;
    `fitWithResidual` passes `UnwatchedIterations`.
- **`SearchBox.centredSearch (scaling : ParameterScaling<'Parameters>)`** is an intrinsic augmentation
  (`type SearchBox with`), because `ScaledSearch` is declared after `SearchBox`. It reads the dimension from the
  scaling itself. That is the value `fitWithResidual` already holds, so no bare count crosses a signature (ELEV010).
- **`fitWithResidual`'s request is field-for-field the old one** (zero start, ∓half-width, 400, 1e-12), through the
  same proxy (`createAlglibLevenbergMarquardt ()` = `UnwatchedIterations.levenbergMarquardtSolver`, step 380).
  `createAlglibLevenbergMarquardt` remains for the BerremanTests suites that call it directly.

## Deferred

Nothing.

## Gotchas

- **`centredSearch` takes the `ParameterScaling`, not the slice's `(dimension : int)`.**
  - The slice's `int` signature failed elevation with ELEV010: a member may speak only its own type's primitive
    (`SearchBox`'s is `double`), and the return type `ScaledSearch` is elevated.
  - Per AGENTS.md, the analyzers win over a document (spec 0048 `.manual/002` R6).
  - The member's name, contract symbol and both test identities are unchanged. The centred-search fact builds its
    three coordinates as `scalingAround (List.take 3 UniaxialParameters.axes)` over the suite's shared crystal fixture.
    No later slice calls `centredSearch`.
- **`ScaledSearch` shares `lowerBounds` / `upperBounds` with `NonlinearRequest`.** Annotate a record expression that
  lists only those labels (`let request : NonlinearRequest = …`), or inference may pick the later-declared
  `ScaledSearch`.
- **`OptimizationInterface` stays qualified in `MuellerFitting.fs`.** Opening it would bring `OptimizationRequest`'s
  `residual` / `initial` labels beside `NonlinearRequest`'s.
- **The scaled-search fact records every residual evaluation.** It asserts that the first is the given start and that
  all of them stay in the box (ALGLIB's `minlmsetbc` contract). The two answers alone would not catch a solve that
  ignored `start`.

## Changelog

- 2026-11-13 — Step 0381: the scaled search.
  - `ScaledSearch` with `solveWith` holds the one bounded LM request, and `SearchBox.centredSearch` is the S10.1 box.
  - `fitWithResidual` calls them, bit-identical to before.
  - Two new `StudyEngineTests` facts (constructor suite 1650 → 1652).
