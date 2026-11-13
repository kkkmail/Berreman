# Step 0380 — state of the world

## Where we are

This is spec 0050 section S11.3: engines, staged and joint fits, and append-only fit runs.

- **Step 0379** closed S11.2. `Study.residualWith` is the weighted residual a study fit minimises.
- **A long fit runs on `JobRunner`, with per-iteration progress and Cancel** (001 §N; .spec-md §10.2, §10.3).
- **The Levenberg–Marquardt solve runs whole inside one ALGLIB call.** This step therefore gives the adapter a watch:
  a solve can report each iteration and honour a stop request. The study engine can then drive that solve from a job.
- **Nothing outside `AlglibAdapter.fs` learns of ALGLIB.**

## What's working

- Add `OptimizerIterationObserver` and `IterationWatch` (`UnwatchedIterations` | `WatchedIterations`) to the
  optimization interface
- Watch the ALGLIB Levenberg–Marquardt solve: number each report, tell the observer, and request termination on a
  cancelled token; ALGLIB's code 8 now reads `SearchCancelled`
- Add `OptimizationRequest.optimizeWatched`; `optimize ()` is the unwatched run, making exactly the earlier ALGLIB calls
  (bit-identical on five probe solves)
- Add `IterationWatch.levenbergMarquardtSolver`; `createAlglibLevenbergMarquardt ()` is its unwatched case
- Add two `OptimizationInterfaceTests` facts: the watched solve's reports and answers, and its cancellation (constructor
  suite 1648 → 1650)

## Tests

```yaml
gates:
  constructor_unit_tests: 1650
  berreman_unit_tests: 5
```

| Gate | Result | Time (deadline) |
|---|---|---|
| build (`dotnet build Berreman.slnx -c Release -nologo -v:m`, from `Berreman/`) | exit 0, `0 Error(s)`, no lowercase `error`. The warnings are only the exempt NU190x advisories and vendored MathNet's `SYSLIB0051`, from a file this step does not touch | 77 s full; 4 s for the final incremental build (600 s) |
| constructor-unit-tests (the gate's own command, whole suite) | `Test run summary: Passed!`, 1650 succeeded (1648 at checkpoint 379, +2), 0 failed, 0 skipped. In the TRX, both new facts are `Passed` | 92 s (180 s) |
| unit-tests (selected `BerremanTests.FeasibleBiaxialInverseTests`) | `Test run summary: Passed!`, 5 of 5, 0 failed, 0 skipped. In the TRX, all five facts are `Passed` | 216 s (7200 s) |
| elevation (from arc base `f395597c`, ELEV004 excluded) | `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)`, exit 0. The arc's `Berreman.slnx` change widens the run to all 13 projects. `OpticalConstructor.Optimization`'s suppressed count is unchanged at 34 | 296 s (600 s) |
| the twenty s38 invariants | each prints `INVARIANT HOLDS` and exits 0 | seconds each (300 s) |

These are development runs. The supervisor's gate engine is the authority. Nothing is deferred, and every duration is
within its deadline.

**"Numbers unchanged".** `FeasibleBiaxialInverseTests` asserts tolerances; it prints no fitted numbers. A development
probe therefore compared five unwatched LM solves against HEAD's adapter, loaded from `git show`:

- the cases: linear, analytic Jacobian, Rosenbrock unbounded, bounded, and capped at 5 iterations;
- solutions, final residuals, iteration counts and termination reasons agree bit for bit
  (`specs/0050/.artifacts/0380-bit-identity.log`).

## Architecture

**The watch.** `IterationWatch` (`OptimizationInterface.fs`) says whether a solve is watched. It is a union, not a
flag. `WatchedIterations` carries an `OptimizerIterationObserver`, which has the shape of step 197's
`KitArchiveObserver`:

- `reportIteration : IterationCount -> unit`;
- `cancellation : CancellationToken`.

**The adapter.** `AlglibAdapter.runLm (watch)` picks the report callback once:

- **Unwatched:** `null`, and no `minlmsetxrep`. These are exactly the earlier calls.
- **Watched:** `minlmsetxrep(state, true)` and an `ndimensional_rep` that does three things:
  - counts its calls k = 1, 2, …;
  - calls `observer.reportIteration (IterationCount k)`;
  - calls `minlmrequesttermination` when the token is cancelled.

  ALGLIB then stops with code 8, which `CompletionCode.terminationReason` reads as `SearchCancelled`, an
  `OptimizationFailed` outcome.

**The entry points.** `optimizeWatched (watch)` is the old `optimize` body. Only its LM arm passes the watch on; the
other methods run unwatched, because the study fit needs only the LM solve. `optimize ()` is
`optimizeWatched UnwatchedIterations`.

**The proxy.** `IterationWatch.levenbergMarquardtSolver` (`MuellerInverseSolver.fs`) is a member, because a free
function of one of our types belongs on it (ELEV013). A cancelled solve answers `DidNotConvergeErr`, like any failed
run; the caller tells a cancel by its own token. `createAlglibLevenbergMarquardt ()` is
`UnwatchedIterations.levenbergMarquardtSolver`, so every existing caller is unchanged.

## Deferred

Nothing.

## Gotchas

- **ALGLIB reports more often than it iterates.** `minlm` with `xrep` reports its start point, each iteration, and
  its final point (AC-G1: 5 reports for 3 iterations; Rosenbrock: 21 for 19). The watch numbers report calls, as the
  slice specifies. A progress display must not read the last `IterationCount` as the result's `iterations`.
- **A cancel at the first report stops before any iteration.** The result is `SearchCancelled`, 0 iterations, and the
  start point as its solution.
- **The ALGLIB state and the counter are not `let mutable` inside the report closure.** A closure cannot capture a
  mutable binding (FS0407). The report reads the state through an immutable alias (`lmState`), and the counter is a
  `ref` cell local to `runLm`.
- **Only Levenberg–Marquardt is watched.** Code 8 maps to `SearchCancelled` for every method, but nothing requests
  termination outside the watched LM solve, so no existing result changes.
- **`OptimizationInterfaceTests` fixture refactor:** the first AC-G1 fact's request moved, unchanged, into
  `acG1Request`, which the new watched fact reuses. No existing assertion changed.

## Changelog

- 2026-11-13 — Step 0380: the watched Levenberg–Marquardt solve.
  - Adds `OptimizerIterationObserver`, `IterationWatch`, `OptimizationRequest.optimizeWatched` and
    `IterationWatch.levenbergMarquardtSolver`, and maps ALGLIB code 8 to `SearchCancelled`.
  - Unwatched solves are bit-identical.
  - Two new facts (constructor suite 1648 → 1650).
