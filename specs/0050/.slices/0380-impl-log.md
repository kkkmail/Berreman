# Step 0380 — impl log

## Progress

- [x] Production: `OptimizerIterationObserver`, `IterationWatch` (OptimizationInterface.fs) (2026-11-13)
- [x] Production: code 8 → `SearchCancelled`, watched `runLm`, `optimizeWatched` (AlglibAdapter.fs) (2026-11-13)
- [x] Production: `IterationWatch.levenbergMarquardtSolver` (MuellerInverseSolver.fs) (2026-11-13)
- [x] Tests: two facts in `OptimizationInterfaceTests` (2026-11-13)
- [x] Development runs: build, constructor-unit-tests, elevation, the twenty s38 invariants, a bit-identity probe
  against HEAD's adapter, and unit-tests over `FeasibleBiaxialInverseTests` (2026-11-13)

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Optimization/OptimizationInterface.fs`
  - opens `System.Threading`;
  - adds `[<ReferenceEquality>] OptimizerIterationObserver = { reportIteration : IterationCount -> unit; cancellation :
    CancellationToken }` and `IterationWatch = UnwatchedIterations | WatchedIterations of OptimizerIterationObserver`,
    after `OptimizationRequest`;
  - `SearchCancelled`'s doc gains the watched-solve stop, and the module doc's G.10 progress line is replaced, both as
    the slice words them.
- `Berreman/OpticalConstructor/OpticalConstructor.Optimization/AlglibAdapter.fs`
  - `CompletionCode.terminationReason` gains `| CompletionCode 8 -> SearchCancelled` before the `c > 0` arm;
  - `runLm (watch : IterationWatch)`: the report callback is `null` when unwatched; when watched,
    `minlmsetxrep(state, true)` and an `ndimensional_rep` that numbers its calls, reports each, and calls
    `minlmrequesttermination` when the token is cancelled;
  - `optimizeWatched (watch : IterationWatch)` holds the old `optimize` body, its LM arm calling `runLm watch`;
  - `optimize ()` is `this.optimizeWatched UnwatchedIterations`.
- `Berreman/OpticalConstructor/OpticalConstructor.Optimization/MuellerInverseSolver.fs`
  - `type IterationWatch with member this.levenbergMarquardtSolver : NonlinearSolverProxy` holds the old
    `createAlglibLevenbergMarquardt` body, calling `optimizationRequest.optimizeWatched this`;
  - `createAlglibLevenbergMarquardt ()` is `UnwatchedIterations.levenbergMarquardtSolver`, its doc kept.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OptimizationInterfaceTests.fs`
  - opens `System.Threading` and `OpticalConstructor.Optimization.MuellerInverseSolver`;
  - the first AC-G1 fact's request moves, unchanged, into the shared fixture `acG1Request`, which the new fact reuses.
    That fact's assertions are unchanged;
  - adds the fixtures `rosenbrock`, `rosenbrockRequest` and `cancellingAtFirstReport`;
  - adds the two facts.

## Decisions

- **The report callback is chosen once, before the Jacobian match.** Unwatched, it is
  `(null : alglib.ndimensional_rep)`, and no `minlmsetxrep` is called. Both `minlmoptimize` calls then receive exactly
  what they received before.
- **The ALGLIB state is read through an immutable local `lmState`.** `state` is `let mutable` because ALGLIB's
  `minlmcreatev` fills it by reference, and a closure cannot capture a mutable binding (FS0407). The report captures
  `lmState`, the same object.
- **The counter is a `ref` cell local to `runLm`.** The slice calls for "a local mutable inside `runLm`", but a
  `let mutable` cannot be captured by the report closure (FS0407). A `ref` cell is the local mutable a closure can
  hold, and it never leaves `runLm`.
- **The cancellation fact's observer cancels on `IterationCount 1` only.** The slice says it cancels "at its first
  report". It records every report, so "at most two reports" is asserted on the recorded count.
- **The proxy half of the cancellation fact also asserts one or two reports.** This shows the member passes the watch
  through to the adapter. It is not only that some failure surfaced as `DidNotConvergeErr`.
- **The first fact also asserts `reports.Count >= iterations`.** ALGLIB reports its start point and each iteration, so
  every iteration is reported. This makes the fact's name ("reports each iteration") an assertion, not only the 1, 2, …
  numbering.
- **The first fact compares the two solves within 1e-12**, with xUnit's `Assert.Equal(double, double, tolerance)`. This
  is the idiom the constructor suite already uses (`DataSplitTests.assertAngle`, `DataSplitModelTests`).
  `BerremanTests/MatrixComparison.fs` is not referenced by `OpticalConstructor.Tests`.

## Testing state

Development runs on the final source. The gate engine is the authority.

- **build** (`dotnet build Berreman.slnx -c Release -nologo -v:m`, from `Berreman/`)
  - Result: exit 0, `0 Error(s)`, no lowercase `error` in the log.
  - Warnings: 8, all outside our code. They are the exempt NU190x advisories and vendored MathNet's `SYSLIB0051`
    (`MathNetNumerics/Numerics/Exceptions.cs`), which this step does not touch and step 0379 recorded too.
  - Time: 77 s (deadline 600 s).
  - The test file then gained one assertion. `OpticalConstructor.Tests.fsproj` was rebuilt alone, with 0 errors and 0
    warnings.
  - The final whole-solution build, on the final source after the inverse class finished: exit 0, `0 Error(s)`, no
    lowercase `error`. Its 6 warnings are again only the NU190x advisories and `SYSLIB0051`. Time: 4 s, incremental.
- **constructor-unit-tests** (the gate's own command, whole suite, on the final test source)
  - Result: `Test run summary: Passed!`, 1650 succeeded, 0 failed, 0 skipped. That is 1648 at checkpoint 379, +2.
  - In the TRX, both new facts are `Passed`.
  - Time: 92 s (deadline 180 s).
- **elevation** (`ASG_ARC_BASE_COMMIT` = arc base `f395597c`, ELEV004 excluded)
  - Result: `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)`, exit 0.
  - Scope: all 13 projects, because the arc's `Berreman.slnx` change widens the run.
  - `OpticalConstructor.Optimization`'s suppressed count is 34, as it was at step 0379.
  - Time: 296 s (deadline 600 s).
- **The twenty s38 invariants:** each prints `INVARIANT HOLDS` and exits 0.
- **Bit-identity probe** (development only; the script is kept as `0380-bit-identity-probe.txt`)
  - Method: HEAD (`2adf0c8b`)'s `OptimizationInterface.fs` and `AlglibAdapter.fs`, taken by `git show` and renamed
    into a scratch namespace, against the new compiled adapter.
  - Five unwatched LM solves: AC-G1 linear, an analytic Jacobian, Rosenbrock unbounded, Rosenbrock bounded, and
    Rosenbrock capped at 5 iterations.
  - Result: the solution and final residuals agree bit for bit (`DoubleToInt64Bits`), as do the iteration count and the
    termination reason. `ALL BIT-IDENTICAL: True`.
- **ALGLIB's report behaviour** (development probe, in the same `.txt`)
  - AC-G1: 5 reports for 3 iterations. Rosenbrock: 21 reports for 19 iterations.
  - Cancelling at the first report stops both after 1 report, with 0 iterations and `SearchCancelled`.
- **unit-tests** (the gate's own command, selecting `BerremanTests.FeasibleBiaxialInverseTests`)
  - Result: `Test run summary: Passed!`, 5 of 5, 0 failed, 0 skipped.
  - In the TRX, all five facts are `Passed`, including the nine-constant recovery that runs
    `createAlglibLevenbergMarquardt` (now `UnwatchedIterations.levenbergMarquardtSolver`).
  - The class asserts tolerances rather than printing its fitted numbers. "Every number unchanged" therefore rests on
    the bit-identity probe above: the unwatched LM solve is bit for bit HEAD's.
  - Time: 216 s (deadline 7200 s). It ran concurrently with elevation and the constructor suite.

## Artifacts

- `specs/0050/.artifacts/0380-build.log`
- `specs/0050/.artifacts/0380-constructor-unit-tests.log`, plus the TRX at
  `Berreman/OpticalConstructor/OpticalConstructor.Tests/TestResults/constructor-unit-tests.trx`
- `specs/0050/.artifacts/0380-elevation.log`
- `specs/0050/.artifacts/0380-invariants.log`
- `specs/0050/.artifacts/0380-bit-identity.log` and `specs/0050/.artifacts/0380-bit-identity-probe.txt`. The scratch
  folder that held HEAD's adapter copy was deleted, so no second ALGLIB-naming `.fs` file lingers.
- `specs/0050/.artifacts/0380-unit-tests.log`, plus the TRX at `Berreman/BerremanTests/TestResults/unit-tests.trx`

## Gotchas

- **ALGLIB's report calls outnumber its iterations.** `minlm` with `xrep` reports its start point first, then each
  iteration, and its final point. A watched solve's last `IterationCount` therefore runs ahead of the result's
  `iterations` by up to two. The slice numbers report calls, so this is by design. A progress display must not read
  the reported count as ALGLIB's `iterationscount`.
- **A cancel at the first report stops before any iteration.** The result is `SearchCancelled`, 0 iterations, and the
  start point as its solution.
- **No other method is watched.** `QuasiNewtonLbfgs`, `BoundedLinearConstrained` and `NonlinearConstrained` ignore
  the watch. Code 8 maps to `SearchCancelled` for them too, but nothing requests termination there, so no existing
  result changes.
- **`specs/0050/.manifest.state.json`** was already modified by the supervisor before this round. It was not touched
  here.
- **No operator note or operator ruling** applies to step 0380.
