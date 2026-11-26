# Step 0387 — state of the world

## Where we are

Spec 0050, section S11.3 (engines, staged and joint fits, append-only fit runs). Step 386 made the engine choice
automatic (`StagedStudy.engineWith`). This step adds the nonlinear engine in `OpticalConstructor.Optimization/StudyEngine.fs`.
It minimises the study residual (step 379), plus one row per prior, by the bounded ALGLIB Levenberg–Marquardt solve over
the generated axes, multi-start. It reuses the scaled search (step 381), the watched solve (step 380) and the deviate
provider (step 339). Later S11.3 steps build on it in the same file:

- 389 and 390 add the linear calibration and object reconstruction, both answering `StudyFitSolution`;
- 391 dispatches `runFit` to `fitNonlinear` when `engineWith` finds no linear shape;
- 397 moves the member-watch rule to `IterationWatch.after`, and 399 reads the fit's problem back through
  `scaledProblemWith`.

## What's working

- Added `StudyFitSolution` (engine, layout, members, fitted vector, object solves) and `studySearchBox = SearchBox 50.0`.
- Added `ScaledStudyProblem` with `physicalOf`, and `StagedStudy.scaledProblemWith`:
  - the study residual scaled about each entry's start, with unit span / 100 within the entry's own bounds;
  - one row (x − centre) / sigma per prior;
  - total, with a full-width 1.0e3 penalty;
  - refuses a fitted run whose session's gain comes from AIR runs.
- Added `StagedStudy.fitNonlinear`:
  - starts from the parameters' starts, then from deviate-placed starts;
  - one watch counts every iteration across members;
  - cancellation is read before and after each member;
  - keeps the converged member of least χ², the earliest on a tie.
- `StudyFitError` gains `SessionGainNeedsLinearEngineErr`, `NoConvergedMemberErr` and `FitCancelledErr`, each rendered
  with its reason.
- Added five `StudyEngineTests` facts over a test-local index probe's polarimeter study, and widened the error-render
  fact.

## Tests

Run locally on 2026-11-13 on the final code. Every run finished within its effective deadline:

- `build`: pass, 0 errors (deadline 600 s). The only warnings are the exempt NU1902 advisories.
- `constructor-unit-tests`: pass, 1677/1677, 101 s (deadline 180 s).
  - The count is 1672 → 1677, so +5, matching the acceptance.
  - All five new identities pass under their exact slice names (checked in the TRX), and so does the widened `every
    study-fit error renders its condition and its payload`.
  - `StudyEngineTests` now sets the suite's wall time: its multi-start fact takes 86 s under the suite's load (47 s
    alone).
- `elevation`: pass, `ELEV-GATE: OK - 0 new finding(s) in 13 project(s), 207s` (deadline 600 s). This was a standalone
  run with no `-Base`, so every project was analyzed.
- `unit-tests`: pass, 20/20 for the selected `BerremanTests.MuellerInverseTests`, 684 s (deadline 7200 s).
- The twenty `invariant-s38-*` must-find-nothing greps: all find nothing.

```yaml
gates:
  constructor_unit_tests: 1677
```

## Architecture

- **`scaledProblemWith` is the one place the nonlinear problem is made**; step 399 reads it back as it is.
  - It works over the context with `kit.items` replaced by the staged items, on step 372's layout.
  - The centre is the layout's `startVector`.
  - The data's runs are kept to `fittedRuns`.
  - The residual is `residualWith` at `physicalOf`, then the prior rows in entry order.
  - Order of refusals: the gain check (structural), then the residual at the start (`StudyForwardFitErr`). The start
    residual's length fixes the penalty width.
- **The record is built frame first.** It is made with an empty residual, then copied with the total one, so the
  residual reads its vector through the record's own `physicalOf` and the centre + unit · scaled formula exists once.
- **Members.**
  - Each member is `{ search with start = … }.solveWith` its own watch over `problem.residual`.
  - Under `WatchedIterations o`, a member's watch tells `o` the earlier members' told total plus its own count, and
    keeps the last count told.
  - A further member's start is drawn lazily, after the cancellation check before it: `lower' + (d + 1) / 2 ·
    (upper' − lower')`, entry after entry.
  - A cancelled fit drops the running member and names how many ran.
- **Cancellation is read from the watch's token**, or `CancellationToken.None` when unwatched. No helper maps the
  two-case `IterationWatch` to a `bool`.
- **No retry, cache, threshold or option is added.**

## Deferred

Nothing.

## Gotchas

- **Step 397's `IterationWatch.after` must be fed the counts each member's watch was told**, not
  `NonlinearSolution.iterations`.
  - ALGLIB also reports its start point, so a member's told count runs one ahead of its iterations.
  - Using `NonlinearSolution.iterations` would repeat a number at each member boundary and break 397's "every count it
    reports is unchanged".
- **The multi-start fact is the suite's slowest (about 47 s).** The brief's deviates start member 2 at n_o's lower and
  n_e's upper bound, many fringes away. Members 2 and 3 settle in local minima on n_e's upper bound (χ² 9.6 and 1.4)
  after roughly 145 iterations between them, each about 0.3 s. One study residual costs 134 ms, 85 ms of it the four raw
  spec-0044 plate solves, so the cost is mostly physics. Member 1 recovers the truth exactly (χ² 1.5e-24). Steps 391 and
  399 add more fits of this fixture to `StudyEngineTests`, which already sets the constructor suite's wall time (about
  100 s of 180 s).
- **The index probe's context must hold its material.** The plate's stack sample resolves through the material store
  when the run is bound, so `plateContextOf` saves the entry, as `StudyForwardTests.crystalBenchContext` does.
- **Two boxes of fifty.**
  - `FitRuns.fs`'s `boxSigmas = 50.0` (step 385, calibration freed as a prior) and `studySearchBox` (here) agree by
    value.
  - Domain cannot reference Optimization, and `FitRuns.fs` is outside this step's edits, so they were not merged.
- **Counting CRs in this shell.** `grep -c $'\r'` counts lines that contain an `r`; use `tr -cd '\r' | wc -c`.

## Changelog

- 2026-11-13 — Step 0387: added the nonlinear study engine:
  - `StudyFitSolution`, `studySearchBox`, `ScaledStudyProblem`, `StagedStudy.scaledProblemWith` and
    `StagedStudy.fitNonlinear`;
  - three new `StudyFitError` cases;
  - five `StudyEngineTests` facts, and three new cases in the error-render fact.
