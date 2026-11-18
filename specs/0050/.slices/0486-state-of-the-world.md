# Step 0486 — state of the world

## Where we are

Spec 0050, section S14.2: the Planner's numerical layer, the plan, the cache and the Plan view. Earlier steps built
the pieces this step joins:
- the inputs and structural report (471–472);
- the candidates' information and greedy ranking (478–479);
- the candidates' coverage by kit experiments (482);
- the instrument findings (483);
- the `.oc/planner/` cache (484–485).

This step adds the Planner's run over a kit, `PlannerInputs.planWith`. It evaluates each candidate through the
caller's `rowsOf`, with per-candidate progress and Cancel, and puts the targets' priors and the candidates a kit
experiment already holds in the baseline. It then ranks the open candidates greedily with a bound per target, and
reports the structural and instrument findings. Step 0487 opens it from the kit, wrapping `candidateRowsWith` in
`KitContext.cachedPlannerRows`.

## What's working

- Added `PlannerRuns.planWith`: per-candidate progress and Cancel, priors plus in-kit runs as the baseline, open
  candidates ranked with bounds
- Added `candidateRowsWith` (polarimeter budget resolved first), `scalingWith`, `forwardWith`, `priorRows` and
  `PlanCandidate.configurations`
- `PlannerError` gains `PlanCancelledErr` and `PlannerForwardErr`
- Five `PlanRunTests` facts cover progress/Cancel, in-kit baseline, priors, a z-cut plan over the real forward
  (edited detector noise included) and the cache

## Tests

Local development runs over the final tree, each with the gate's own command. The supervisor's gate engine remains
the authority.

- `build`: green, 0 errors. The only warnings are the exempt `NU1902` advisories and, on a MathNet recompile, the
  pre-existing vendored `SYSLIB0051`.
- `elevation`: `ELEV-GATE: OK - 0 new finding(s) in 13 project(s), 232s` (deadline 600 s); known findings 0. Domain,
  Optimization and Storage each have 0 findings.
- `constructor-unit-tests` (`all_discovered`, the whole suite): `Passed!`, 1894 of 1894, 129 s (deadline 180 s).
  That is checkpoint 1889 plus exactly the five census identities, whose TRX names match the census ids.
  - `PlanRunTests` alone: 5 of 5.
  - With the touched `PlannerInputsTests`, `PlannerReportTests` and `PlannerCacheTests`: 36 of 36.
- `kit-acceptance` (selected `KitAcceptanceTests`, trait `kit-acceptance`): `Passed!`, 14 succeeded and 1 skipped
  (AT-2's Explicit ensemble; skip policy `allow`), 1418 s (deadline 3600 s). AT-2's grid-generator fact, which holds
  the fit, took 789 s, inside R0005's 1800 s.
- `unit-tests` (selected `MuellerInverseTests`, `BiaxialInverseTests`): `Passed!`, 31 of 31, 946 s (deadline 7200 s).
- The 21 `invariant-s46-*` greps: each finds nothing, also with `--untracked` to cover the two new files.
- No run exceeded its effective deadline.

```yaml
gates:
  constructor_unit_tests: 1894
  berreman_unit_tests: 31
  kit_acceptance_tests: 14
```

## Architecture

- **One run, the caller's rows.** `planWith` takes `rowsOf : PlanCandidate -> Result<CandidateRows, PlannerError>`, so
  the cache is the caller's: step 0487 wraps `candidateRowsWith` in `KitContext.cachedPlannerRows` where the kit is
  open. The run adds no retry, cache or option. Its order:
  1. the structural report, the instrument findings and the scaling;
  2. each candidate's rows, the token read before each and the observer told `(evaluated, total)` after each;
  3. the coverage split and the baseline;
  4. the ranking.

  The first error stops it. A cancel names how many of how many were evaluated.
- **Baseline = what the kit already knows.** It is one row list:
  - first, each target's Gaussian prior as a unit-noise row `searchUnit / σ` at its coordinate (the scaled
    derivative of the study engine's `(x − centre) / σ` row);
  - then the rows of every candidate a kit experiment already holds (`coverageIn`, planned or measured).

  `rankWith` ranks only the open candidates over it, so the plan proposes the *next* experiments. The baseline and
  every ranked step carry their `boundsOver` the scaling.
- **The study engine's scaled frame.** `scalingWith` centres the targets' crystal axes at the prior crystal and
  scales each to its unknown's `bounds.searchUnit` (the study engine's ±50-unit box, step 476). So the Planner's
  information and its bounds are in the same units a fit of those runs reports. `forwardWith` is the study engine's
  observation forward (`createBerremanForward` over `opticalProperties`).
- **The detector's own noise.** `candidateRowsWith` resolves the polarimeter's `noise.benchNoise` at the
  Optimization boundary before `candidateRowsOver`. A refused budget is `UnresolvedPlannerItemErr` naming the
  detector and the axis. `BenchNoise` stays out of Domain.
- **Errors.** `PlannerError` gains `PlanCancelledErr` (the run was stopped) and `PlannerForwardErr` (the forward
  refused a candidate). `MuellerFitError` is rendered for the latter by a private extension member in `PlannerRuns`.

## Deferred

Nothing. Every requirement of the step landed in this round.

## Gotchas

- **Outside `effects.edits`, consequential test edits** (precedent: steps 470, 471, 477, 483, 485):
  - The exhaustive `PlannerError` or-patterns in `PlannerInputsTests` and `PlannerReportTests` gain the two new
    cases on their existing line; the `--warnaserror+:25` build requires it.
  - `private` is dropped from the bindings `PlanRunTests` reuses, so no fixture is copied. In `PlannerInputsTests`:
    `crystalEntry`, `qzZDraft`, `qzX`, `g11`, `benchHolding`, `sampleAxisOf`, `runOver`. In `PlannerCacheTests`:
    `newKitFolder`, `openOver`.

  No assertion or test identity changed.
- `MuellerFitError` has no render member. `PlannerRuns.plannerReason` is a private extension, because
  `MuellerFitting.fs` is outside the step's edits.
- `PlannerInformation.over` takes the first row's noise as the reference. With a prior that is the prior's unit
  noise, and the ranking does not depend on the choice.
- `ExperimentDataStore` is `[<RequireQualifiedAccess>]`: open `OpticalConstructor.Storage` and qualify it.
- `git grep` skips untracked files; the invariants were also run with `--untracked` to see the two new files.

## Changelog

- 2026-11-18 — step 0486: the Planner's run over a kit (`PlannerRuns`: progress/Cancel, baseline of priors and
  in-kit candidates, greedy ranking with bounds, structural and instrument findings); `PlanCancelledErr` /
  `PlannerForwardErr`; five `PlanRunTests` facts (1889 → 1894).
