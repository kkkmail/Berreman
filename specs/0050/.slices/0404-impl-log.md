# Step 0404 — impl log

## Progress

- [x] `FitRun.results : FitRunResults` after `objectMatrices` in `FitRuns.fs`. `currentSchemaVersion` stays 1.
- [x] `results` in `fitrun.schema.json`: sixteen required root fields, and `results` an object of exactly six arrays.
- [x] `runFit` stores `staged.resultsWith stagedContext prefits data this.linearSolver solution watch`, after the
  solution and before the clock read. Its error is answered as it stands.
- [x] Repaired `objectProblemsWith`, which the widened fit-run fact needs (see Decisions, deviation D1).
- [x] Tests: the fixture's one-of-each results, the round-trip and refusal facts, the widened fit-run fact, and the new
  `a fit run stores its results, profiles included`.
- [x] Development gate runs (see Testing state).

## Files modified

- `OpticalConstructor.Domain/FitRuns.fs`: the `results` field, and the `FitRun` doc cites S11.4.
- `OpticalConstructor.Storage/schema/fitrun.schema.json`: `results` required under version 1, its six arrays required
  and nothing else allowed; the description names the results.
- `OpticalConstructor.Optimization/StudyEngine.fs`:
  - `runFit` gathers and stores the results, and its doc names them.
  - The object stage's solves MOVED out of `reconstructLinear` into a private `objectSolvesWith`, which also records
    each solve's system, solution and columns (private `MadeObjectSolve`).
  - `reconstructLinear` is that member plus its unchanged assembly of the solution.
  - `objectProblemsWith` reads its problems off the same solves.
- `OpticalConstructor.Tests/KitContentFileJsonTests.fs`:
  - `FitRunFileFixtures.fitRunOf` gains the slice's one-of-each results, and the fixture module opens `MuellerInverse`.
  - The round-trip fact expects the `results` key and checks its six arrays.
  - The refusal fact also refuses a run without `results`, and a `results` without `profiles`.
- `OpticalConstructor.Tests/StudyEngineTests.fs`:
  - The fit-run fact widened, along with its helpers `linearFitRunOf` and `assertLoneFitRun`.
  - The new fact, at the file's end under its own S11.4 heading, with `open OpticalConstructor.Storage.KitContentFiles`.
- `KitFitRunFileTests.fs` and `StudyFitJobTests.fs`: no edit needed. They spell out no `FitRun`, and now write and read
  the fixture's richer results through the kit unchanged.

## Decisions

- **The fixture holds the rich results.** `fitRunOf` is the one test record expression that spells out a `FitRun`,
  and it is the round-trip fact's fit run, so it holds the slice's one-of-each results (point 4's "unless stated here").
  - Every fit-run file fact (`KitFitRunFileTests`, `StudyFitJobTests`) now writes and reads them through the kit.
  - Its problems: `StudyEntriesProblem` over two `EntryCoordinate`s, with a condition and a correlation;
    `ObjectSolveProblem [QZ; CPL1]` over two `ElementCoordinate`s, with neither; and `AirFringeProblem [A1; A2]` over an
    `EntryCoordinate` and `FringeVisibilityCoordinate`, with both.
  - Its profile holds all three reasons and both outcomes. Every number is finite, and there is no −0.
- **runFit passes `stagedContext`.** This follows the slice. Each member under `resultsWith` re-stages its context with
  `this.items`, so it equals the engines' `this.forward`.
- **D1 — `objectProblemsWith` reads the solves `reconstructLinear` made (a deviation from "Nothing else in `runFit`
  changes", recorded here with its reason).**
  - **Why it was needed.** `reconstructLinear` solves a lone object that has known, calibrated or linked coefficients on
    its free columns (A·P·x = b − A·k). Step 400's `objectProblemsWith` re-solved every group by `reconstruct` over all
    sixteen elements.
  - **What broke.** Once `runFit` gathered results, the widened fit-run fact's m00-alone object behind a fixed CP was
    refused with `ObjectSolveFitErr … rank-deficient: rank 3`.
    - That fact's existing assertions need a fit run there. So do the constant-Mueller first column, the 3 × 3 block
      and the linked pair.
    - For complete designs, the old member also re-solved an unconstrained matrix, not the one the engine fitted.
  - **The repair is a move, not a copy.** The lone and series solves are moved unchanged into a private
    `objectSolvesWith`, which both `reconstructLinear` and `objectProblemsWith` call. Each solve's problem is then its own
    system: a reconstructed one over its sixteen elements, as step 400 states, or a free-columns one over its layout
    entries within their bounds.
  - **What does not change.** Step 400's all-free and series cases are the same problems in the same order, and
    `reconstructLinear` answers the same numbers. Every step 390/400/402/403 fact passes unchanged.
  - **The scope.** It is inside this step's authorized edits (`StudyEngine.fs`). It is needed by its acceptance
    ("every fit run `runFit` answers MUST hold the `FitRunResults` `resultsWith` gives its fit"). And it restores step
    400's own headline, "the object stage's results are read off each linear solve it made".
- **The widened fit-run fact.**
  - **Solve counts.** Every successful linear fit now records two solves through the recording SVD: the engine's, then
    the results' re-solve (step 400's re-solve, "No cache"). Its six `Assert.Equal(1, …Count)` became 2. The
    rank-deficient case keeps 1: the engine refuses, so no results are gathered.
  - **`linearFitRunOf`** also checks that the second half of the recorded systems equals the first, so the results
    re-solve exactly what the engine solved. It also checks one σ per entry.
  - **`assertLoneFitRun`** also checks that the run's one problem is `ObjectSolveProblem [item]`, of rank `free.Length`,
    over exactly the run's entries.
  - **The linked pair's** results hold one problem, `ObjectSolveProblem [QZ; QZ2]`, of rank 28, over its 28 entries in
    layout order.
  - **The plate fit's results** are compared with what the same solution's `engineProblemsWith` gives, which is what
    `resultsWith` reads them from: σ, problem qualities and run residuals. Its profiles' entries are compared with
    `suspectsWith`'s.
    - Re-running `resultsWith` itself would repeat the profile pass, about 13 s.
    - The new fact compares whole `resultsWith` equality, profiles included, on step 403's cheaper confounded study.
- **The new fact** runs step 403's confounded Malus study through `runFit`. Its run's results equal
  `fit.resultsUnder UnwatchedIterations`, with the gain's and the scale's profiles in layout order, seven points each.
  The run also round-trips through `fileText` / `parseFitRun` and rewrites the same text, so real results serialize
  under the schema.

## Testing state

Development runs on 2026-11-14. These are not gate verdicts; the supervisor re-runs every gate.

- **build**: exit 0, 0 errors. The only warnings are the exempt `NU1902` advisories, plus the vendored MathNet's
  already-recorded `SYSLIB0051` when it recompiles. First build 82 s, then about 33–43 s, against 600 s.
- **Targeted classes**:
  - `KitContentFileJsonTests`, `KitFitRunFileTests` and `StudyFitJobTests`: 29/29.
  - The first `StudyEngineTests` run had one failure, the solve count 1 → 2, then the rank-deficient refusal, which
    led to D1. After D1, the fit-run fact passed (33 s).
- **constructor-unit-tests**, whole suite, by the gate's command: 1726/1726, from 1725 + 1.
  - 154 s, and 153 s on the final run after the mutation restore, against 180 s.
  - `StudyEngineTests` is the critical path at 152 s serial, from 133.5 s at step 403.
  - The fit-run fact went from 12 s to 30.6 s: `runFit` now profiles the plate fit's suspect n_o and n_e.
- **elevation**: `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)`, with no base, so all 13. 221 s against 600 s.
- **invariant-s39-*** (20): each printed `INVARIANT HOLDS … found nothing`.
- **unit-tests** (selected `BerremanTests.MuellerReconstructionTests`, `BerremanTests.MuellerInverseTests`): 45/45
  passed. 682 s against 7200 s.
- **Mutation check** (`0404-scripts/mutation-run.sh`): each mutant was built and run against the four fit-run facts.
  - `empty-results` (`runFit` stores six empty lists): the new fact and the fit-run fact failed.
  - `results-optional` (the schema stops requiring `results`): the refusal fact failed.
  - `profiles-optional` (the schema stops requiring `results.profiles`): the refusal fact failed.
  - The pre-D1 `objectProblemsWith` was already seen failing the fit-run fact (`0404-dev-fit-run-fact-2.log`).
  - The sources were restored, checked byte-identical (`cmp`), rebuilt (`0404-dev-build-final.log`: 0 errors, only
    `NU1902`), and the whole suite re-run.

Nothing blocks commit.

## Artifacts

- `specs/0050/.artifacts/0404-dev-build-{1..4}.log`: the builds.
- `0404-dev-file-tests-1.log`, `0404-dev-study-engine-1.log` and `0404-dev-fit-run-fact-{2,3}.log`: the targeted runs.
  `0404-trx/` holds the first `StudyEngineTests` TRX.
- `0404-dev-constructor-unit-tests-1.log` and `0404-dev-constructor-unit-tests-final.log`: the whole constructor suite.
  Their TRX files are `OpticalConstructor.Tests/TestResults/constructor-unit-tests-0404-dev{,-final}.trx`.
- `0404-dev-build-final.log`: the final build.
- `0404-mutation/`: the per-mutant build and test logs, and `summary.log`.
- `0404-dev-elevation.log`, `0404-dev-invariants.log` and `0404-dev-unit-tests.log`: the other development runs.
- `0404-scripts/`: the edit scripts, and `StudyEngine.before-move.fs`, the pre-D1 source.

## Gotchas

- `objectProblemsWith` re-solves through the solver it is given, so any recording SVD sees each object solve twice per
  `runFit`. A test counting solves through `runFit` counts both.
- The plate study (step 387) has suspect entries under the initial thresholds. So `runFit` over it now profiles them:
  about 13 s per call of re-fits through the Berreman forward model.
- `KitFitRunFileTests` and `StudyFitJobTests` needed no edit, though both are authorized. The richer fixture reaches
  them unchanged.
