# Step 0387 — impl log

## Progress

- Read the task, both system prompts, the project prompt, the slice and its gates, and step 0386's log and SoW.
- Read the code this step builds on:
  - `StudyEngine.fs`, `FitRuns.fs`, `StudyForward.fs` (layout, assignment, data, residual) and `Studies.fs` (gain
    models, start count);
  - `MuellerFitting.fs` (`SearchBox`, `ScaledSearch.solveWith`, `residualFor`) and `MuellerInverseSolver.fs`;
  - `AlglibAdapter.fs` (watched LM), `OptimizationInterface.fs` and `BenchNoise.fs` (`UniformDeviateProvider`);
  - `StudyEngineTests.fs` and `StudyForwardTests.fs` (the polarimeter study fixture).
- Read the later slices that consume this step (0389, 0391, 0397, 0399).
- Checked STL's analyzer sources (read-only). ELEV010 and ELEV012 check only module-level functions and members, never
  a `let` inside a body.
- Production code in `StudyEngine.fs`:
  - the three new `StudyFitError` cases and their renders;
  - `StudyFitSolution`, `studySearchBox`, `ScaledStudyProblem` with `physicalOf`;
  - `StagedStudy.scaledProblemWith` and `StagedStudy.fitNonlinear`.

  The Optimization project built clean.
- Tests in `StudyEngineTests.fs`:
  - Fixtures: a test-local index probe (n_o 1.544, n_e 1.553, g11 = g33 = 0), its 100 µm plate, item and bench, and
    the four truth observations. The plate run P1, the study builder, the staging and data helper, and a stub deviate
    provider.
  - The five new facts and the widened error-render fact.
- First solution build: missing opens (`Berreman.MaterialProperties` for `RefractionIndex` / `RhoValue`;
  `OpticalConstructor.Optimization.BenchNoise`). Added both. The rebuild had 0 errors and only exempt NU1902 warnings.
- `StudyEngineTests`: 12/12 (7 existing + 5 new). The multi-start fact takes 47 s and the recovery fact 5.7 s; every
  other fact takes about 1 s or less.
  - A temporary diagnostic, reverted, showed 153 iterations reported over the three members, about 0.3 s each.
  - Member 1 recovers 1.544 / 1.553 exactly (χ² 1.5e-24).
  - The bound-started members 2 and 3 converge to local minima pinned at n_e's upper bound (χ² 9.6 and 1.4).
  - The cost is the step-379 forward's: four 100 µm Berreman plate solves per residual.
- Full constructor suite: 1677/1677 in 104 s (deadline 180 s). The count is 1672 → 1677, so +5.
- The twenty `invariant-s38-*` greps all find nothing.
- Elevation: `ELEV-GATE: OK - 0 new finding(s) in 13 project(s), 207s`.
- Deleted one restating comment in `fitNonlinear`. Then rebuilt and re-ran the full constructor suite: 1677/1677 in
  101 s. The TRX shows all twelve `StudyEngineTests` identities passing.
- A temporary timing fact (reverted, file restored byte for byte) split one residual's cost; see Gotchas. Rebuilt the
  solution on the final tree.
- Ran the selected `BerremanTests.MuellerInverseTests` (see Testing state).

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Optimization/StudyEngine.fs`:
  - new opens: `System.Threading`, `MuellerInverse`, `OptimizationInterface`, `MuellerFitting`, `BenchNoise`;
  - the three `StudyFitError` cases;
  - `StudyFitSolution`, `studySearchBox`, `ScaledStudyProblem`, `scaledProblemWith` and `fitNonlinear`, after
    `engineWith`.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/StudyEngineTests.fs`:
  - new opens: `System.Threading`, `Berreman.MaterialProperties`, `BenchNoise`;
  - the index-probe fixtures, with `indexPlateDraft` added to `drafts` and `indexBench` added to `benches`;
  - the five new facts;
  - three new cases in `every study-fit error renders its condition and its payload`.

  Every existing assertion is kept.

## Decisions

- **The gain refusal comes before the residual at the start.** Both live in `scaledProblemWith`, as brief (4) places
  them, and step 399 takes `scaledProblemWith`'s errors as they are.
  - The gain check is structural, so a study whose session's gain comes from AIR runs is refused whatever its data.
  - "The residual at the start is taken first" is read as: before the total closure is built. Its length fixes the
    penalty's width.
- **The problem is built frame first.** `physicalOf` is a member of `ScaledStudyProblem`, and the residual has to read
  its vector through it.
  - So `scaledProblemWith` first makes the record with an empty residual.
  - It then returns `{ frame with residual = total }`.
  - This avoids a second copy of the centre + unit · scaled formula. The empty residual never escapes.
- **The deviates are drawn lazily.** A further member's start is drawn after the cancellation check before it, entry
  after entry. A fit cancelled during member 1 therefore asks the provider for nothing, as the cancel fact requires.
- **The watch counts on from the counts the earlier members were told.** Under `WatchedIterations o`, a member's
  watch tells `o` the earlier members' told total plus its own count, and remembers the last count it told (a `ref`
  cell at the observer callback, its one effect).
  - ALGLIB also reports its start point, so a member's told count runs one ahead of its `NonlinearSolution.iterations`.
    Counting told counts keeps the progress strictly 1, 2, …, N over the fit. A failed member's iterations count too.
  - `UnwatchedIterations` stays unwatched, and nothing is counted.
- **Cancellation is read through a `CancellationToken`.** The token is the watch's, or `CancellationToken.None` when
  unwatched. No helper maps the two-case `IterationWatch` to a `bool` (ELEV012).
- **The reasons.**
  - `FitCancelledErr`:
    - before member k: `<k−1> of <n> member(s) ran`;
    - after member k: `<k> of <n> member(s) ran, the last one's result dropped`.
  - `NoConvergedMemberErr`: `member <k>: <failure>`, joined by `; `.
  - `SessionGainNeedsLinearEngineErr`: `'<session>' anchors its gain to <runs>` or `'<session>' interpolates its gain
    from <opening> to <closing>`.
- **Best member.** A strict-less fold over the converged members in order, so the earliest wins a tie. `List.minBy`'s
  tie rule is not documented.
- **The penalty is 1.0e3, written here with a comment naming `MuellerFitting.residualFor`.** This step's authorized
  edits do not include `MuellerFitting.fs`, so the constant cannot be extracted there.
- **Test fixture.**
  - The index probe is a new test-local version (`38700000-…-0201`), because the existing probe crystal gyrates (g11
    3e-5, g33 −1e-5). Its plate is 100 µm.
  - P1 is attached through the `Mueller-matrix observations` profile, with the four truth observations as
    `ObservationPoints`.
  - The context's material store also holds the probe, as `StudyForwardTests.crystalBenchContext` does. The stack
    sample has to resolve when the run is bound.
  - Extra assertions inside the authorized facts:
    - the fitted vector equals the converged member's values;
    - χ² falls below 1e-8;
    - centre, unit (1e-3) and the ±50 box;
    - a free-gain baseline whose problem is made;
    - a fit cancelled before it starts (`0 of 3`);
    - the watched reports run 1 … N over three members.

## Testing state

Run locally on 2026-11-13 on the final code. Every run finished within its effective deadline.

- Build: exit 0, 0 errors (deadline 600 s). The only warnings are the exempt NU1902 advisories, 12 lines (6 distinct).
- `constructor-unit-tests`: 1677/1677, 101 s (deadline 180 s).
  - The count is 1672 → 1677, so +5, as the acceptance requires.
  - The TRX shows the five new identities passing under their exact slice names, and the widened error-render fact
    passing.
  - `StudyEngineTests` has 12 facts (7 existing + 5 new). Its multi-start fact takes 86 s under the suite's load, so
    this class sets the suite's wall time.
- Elevation: `ELEV-GATE: OK - 0 new finding(s) in 13 project(s), 207s` (deadline 600 s). This was a standalone run with
  no `-Base`, so every project was analyzed; Optimization had 0 findings. The only edit after it was the deletion of
  one comment line.
- `unit-tests`, selected `BerremanTests.MuellerInverseTests`: 20/20, 684 s (deadline 7200 s).
- The twenty `invariant-s38-*` must-find-nothing greps: all find nothing.
- Every file this step wrote is LF: no CR in any of them, and `git diff --numstat` equals `--ignore-cr-at-eol`.

No requirement of the slice is left open.

## Artifacts

- `specs/0050/.artifacts/0387-build-local.log`: the solution build.
- `specs/0050/.artifacts/0387-studyengine-tests-local.log`: the `StudyEngineTests` class run.
- `specs/0050/.artifacts/0387-constructor-unit-tests-local.log`: the full constructor suite, with its TRX under the
  test project's `TestResults/`.
- `specs/0050/.artifacts/0387-elevation-local.log`: the elevation gate script.
- `specs/0050/.artifacts/0387-unit-tests-local.log`: `BerremanTests.MuellerInverseTests`.

## Gotchas

- **The multi-start fact is slow: 47 s alone, 86 s under the full suite's parallel load.** The brief's deviates start
  member 2 at n_o's lower and n_e's upper bound, many fringes from the truth.
  - 153 iterations are reported over the three members. Most belong to members 2 and 3, which settle in local minima
    on n_e's upper bound.
  - Each iteration is about 0.3 s.
  - A temporary timing fact, reverted, measured one study residual at 134 ms. The four raw spec-0044 forward solves
    inside it take 85 ms, so the Berreman physics itself is about two-thirds of the cost.
  - `StudyEngineTests` now sets the constructor suite's wall time: about 100 s of the 180 s deadline. Steps 391 and 399
    add more fits of this fixture to the same class.
- **Step 397's `IterationWatch.after` must be fed the counts each member was told**, not `NonlinearSolution.iterations`.
  Otherwise the progress repeats a number at each member boundary, and "every count it reports is unchanged" breaks.
- **`FitRuns.fs` keeps its own `boxSigmas = 50.0`**, step 385's calibration-as-prior box. Domain cannot see
  `studySearchBox`, which lives in Optimization. Both are fifty; nothing in this step's scope merges them.
- **Counting CRs in this shell.** `grep -c $'\r'` counts lines that contain an `r`; use `tr -cd '\r' | wc -c`.
