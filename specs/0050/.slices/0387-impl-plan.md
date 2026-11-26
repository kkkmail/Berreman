# Step 0387 — impl plan

## Approach

The nonlinear study engine, in `OpticalConstructor.Optimization/StudyEngine.fs`, after `engineWith` (step 386).

- `StudyFitError` gains three cases, each `of reason : string`:
  - `SessionGainNeedsLinearEngineErr`
  - `NoConvergedMemberErr`
  - `FitCancelledErr`
- `StudyFitSolution`: what every engine answers.
- `studySearchBox = SearchBox 50.0`.
- `[<ReferenceEquality>] ScaledStudyProblem` with `physicalOf`.
- `StagedStudy.scaledProblemWith`:
  - It works over the context with `kit.items` replaced by the staged items.
  - The layout is step 372's.
  - Each entry's centre is its start, and its unit is span / (2 · 50).
  - The search starts at zero within each entry's own scaled bounds.
  - The data's runs are kept to `fittedRuns`.
  - A fitted run whose session's gain is anchored or interpolated is refused first.
  - Then the residual at the start is taken, and its error answers `StudyForwardFitErr`.
  - The closure is the study residual at `physicalOf`, then one row per prior entry. A refusal gives a full-width 1.0e3
    penalty.
- `StagedStudy.fitNonlinear`:
  - The members run in order. The first starts at zero. Each further member's start is drawn lazily from the deviates,
    entry after entry, after the cancellation check before it.
  - Each member is `ScaledSearch.solveWith` under a watch that counts on from the iterations the earlier members
    reported.
  - Cancellation is read before and after each member.
  - The member of least χ² wins, the earliest on a tie.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Optimization/StudyEngine.fs`: production code.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/StudyEngineTests.fs`:
  - the five new facts;
  - the widened error-render fact;
  - a test-local index crystal (n_o 1.544, n_e 1.553, no gyration), its 100 µm plate, item, bench, observations and data.

## Risks

- The fit must reach the truth within 1e-9 from a start 3e-4 off. The data are exact at the truth (the residual there is
  zero), so LM should converge tightly. This is checked by running the facts.
- Members started at the bounds may wander for up to 400 iterations. Each residual is four thin-plate Berreman solves,
  which is cheap.
- ELEV:
  - no two-case-union-to-bool helpers (the cancellation is read through a `CancellationToken`);
  - no re-wraps;
  - unique case names (checked: none collide).
- The iteration count across members is the count each member's watch was told. That is what keeps the progress
  strictly 1, 2, …, N. Step 397 moves this rule to `IterationWatch.after`.
