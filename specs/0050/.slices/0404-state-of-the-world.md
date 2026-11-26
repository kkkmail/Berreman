# Step 0404 — state of the world

## Where we are

Spec 0050 S11.4 (fit results) closes its stored-results step. Step 403 gathered a fit's results through
`StagedStudy.resultsWith`. Now every fit run `runFit` answers stores them.

The fit run holds:

- each entry's σ and each solve's quality;
- each run's residual, each black box's realizability and each series' product check;
- each suspect entry's χ² profile.

They are written and read in the run's `.fitrun-json`, under schema version 1. Showing the results in the UI is S11.5's.

## What's working

- Added `results : FitRunResults` to `FitRun`. The fit-run schema now requires it as an object of six arrays, still at
  version 1.
- `runFit` gathers `resultsWith`'s results after the engine's solution and before the clock read, and stores them. A
  cancelled profile pass answers `FitCancelledErr`, so nothing is written.
- `objectProblemsWith` now reads its problems off the solves `reconstructLinear` actually made. Both share one moved
  private `objectSolvesWith`, so partly known, calibrated or linked black boxes get results too.
- Added `a fit run stores its results, profiles included`, and widened the fit-run round-trip, refusal and record facts.
  Constructor tests rose from 1725 to 1726.

## Tests

Development runs on 2026-11-14. These are not gate verdicts; the supervisor re-runs every gate. Every run finished
within its effective deadline.

| Gate | Development outcome | Duration / deadline |
|---|---|---|
| build | exit 0, 0 errors, only the exempt `NU1902` advisories (plus vendored MathNet's recorded `SYSLIB0051` on recompile) | 82 s first, 8 s final / 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)` (no base: all 13) | 221 s / 600 s |
| constructor-unit-tests | 1726/1726 passed, whole suite (1725 + 1) | 154 s, 153 s final / 180 s |
| unit-tests (selected `MuellerReconstructionTests`, `MuellerInverseTests`) | 45/45 passed | 682 s / 7200 s |
| invariant-s39-* (20) | each ran its own command: `INVARIANT HOLDS ... found nothing` | < 1 s each / 300 s |

The new fact and the three widened facts all passed:

- `a fit run stores its results, profiles included` (new)
- `a fit run file round-trips through its canonical text`
- `a fit run file of another schema version, a schema-invalid one or a malformed one is refused`
- `a fit run records its name, stamp, stage, engine, fitted runs, the data it read and the vector it used and produced`

A mutation check built three mutants, and each turned a new or widened fact red:

- `runFit` storing empty results;
- the schema not requiring `results`;
- the schema not requiring `results.profiles`.

The sources were restored byte-identical before the final build and suite run (see the impl-log).

**Runtime note.** `constructor-unit-tests` went from 137 s to 154 s, of its 180 s. Its critical path is
`StudyEngineTests`, now 152 s serial. The fit-run fact went from 12 s to 30.6 s, because `runFit` now profiles step
387's plate fit, whose n_o and n_e are suspect under the initial thresholds.

```yaml
gates:
  constructor_unit_tests: 1726
  berreman_unit_tests: 45
```

`berreman_unit_tests` is the two selected classes' own count, as at step 403.

## Architecture

- **The fit run holds its results.** `FitRun.results` comes after `objectMatrices`. `currentSchemaVersion` stays 1:
  no fit-run file exists before S12 writes the committed kits, so no migration and no converter.
  - The schema binds only the results' outer shape (six required arrays, nothing else).
  - The inner shapes are the serializer's case form (step 396), so a bad inner shape is `MalformedJsonErr`.
- **One results call in `runFit`.**
  - It runs `staged.resultsWith stagedContext prefits data this.linearSolver solution watch` between the solution and
    the clock.
  - Its error is answered as it stands, so a cancel in the profile pass is the fit's `FitCancelledErr`, and the job
    writes nothing (step 393).
- **One object-stage solve path (deviation D1, in the impl-log).** The lone and series solves of `reconstructLinear`
  MOVED, unchanged, into a private `objectSolvesWith`. It returns the groups and each solve made (private
  `MadeObjectSolve`: its items, the system it solved, its solution, its columns and its group solves).
  - `reconstructLinear` assembles the fitted vector from it exactly as before.
  - `objectProblemsWith` builds one problem per solve made. A reconstructed solve's problem is over its sixteen elements,
    as step 400 states. A free-columns solve's problem is over the layout entries it fits, within their bounds. Linked
    lone objects share one problem.
  - Run residuals and product checks are at the engine's own matrices.

## Deferred

Nothing. Every requirement of the slice landed this round.

## Gotchas

- **D1 was needed for the acceptance.** Before it, `objectProblemsWith` re-solved every lone object over all sixteen
  elements. So `runFit` refused the fit-run fact's m00-alone black box behind a fixed CP (`rank-deficient: rank 3`)
  once it gathered results. For complete designs, it also re-solved an unconstrained matrix, not the one the engine
  fitted.
- **Each linear object solve is solved twice per `runFit`:** by the engine, and again by `objectProblemsWith` for the
  results. That is step 400's design ("No cache"). A recording SVD therefore sees two equal halves, and the fit-run fact
  now asserts exactly that.
- **The plate profile pass is slow.** Step 387's plate study profiles n_o and n_e inside `runFit`, about 13 s per call.
  The fit-run fact compares the plate run's σ, problems, residuals and profiled entries with the same solution's
  `engineProblemsWith` and `suspectsWith`, rather than repeating the profile pass. The new fact compares whole
  `resultsWith` equality on step 403's cheaper confounded study.
- **The fixture holds the rich results.** `FitRunFileFixtures.fitRunOf` now carries the one-of-each results, so
  `KitFitRunFileTests` and `StudyFitJobTests` write and read them through the kit. Neither file needed an edit.

## Changelog

- 2026-11-14 — Step 0404: `FitRun` stores its `FitRunResults` (schema v1 requires `results`), and `runFit` gathers them
  by `resultsWith`. `objectProblemsWith` reads the solves `reconstructLinear` made through a shared private
  `objectSolvesWith`. Added `a fit run stores its results, profiles included`, and widened three fit-run facts.
