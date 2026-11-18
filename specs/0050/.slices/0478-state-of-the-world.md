# Step 0478 — state of the world

## Where we are

Spec 0050 S14.2, the Measurement Planner's numerical layer. Step 477 gave `PlannerInformation.fs` the Fisher information
of stacked candidate rows (rank, log determinant, Cramér–Rao trace and bounds). This step adds the planner's greedy
D-optimal / target-restricted A-optimal ranking of candidate experiments over a baseline (001 §M). The Plan view
(step 491) consumes it later. How many ranked candidates join the kit stays the user's choice.

## What's working

- Added `PlanCriterion`: D-optimal (most information) or A-optimal (smallest target variances), window state with no
  wire tag.
- Added `PlanOption<'Candidate>`, a candidate and its rows, and `RankedOption<'Candidate>`, an option with the
  information held up to it.
- Added `PlannerInformation.rankWith`, greedy ranking. It ranks by rank first, then the larger log determinant (D) or
  the smaller target trace at full rank (A). A tie keeps the given order, and the first refusal answers whole.
- Covered the ranking with three `PlannerInformationTests` facts:
  - a confounded combination made identifiable;
  - D against A (determinants 24 / 16, traces 1/2 / 1 + 1/24);
  - ties and a noiseless refusal.

## Tests

These are development runs; the arc-runner's gate engine is the authority. No run exceeded its effective deadline.

| Gate | Result | Duration | Deadline |
|---|---|---|---|
| build | exit 0, final incremental build; only the exempt `NU1902` warnings | 84 s full, 11 s final | 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)`; 0 known, 1770 suppressed | 226 s | 600 s |
| constructor-unit-tests | `Passed!` 1873 of 1873 (whole suite, `all_discovered`) | 112 s | 180 s |
| kit-acceptance | `Passed!` 14 succeeded, 1 skipped (`KitAcceptanceTests`) | 1420 s | 3600 s |
| unit-tests | `Passed!` 31 of 31 (`MuellerInverseTests`, `BiaxialInverseTests`) | 1103 s | 7200 s |
| invariant-s46-* (21) | each decoded `git grep` finds nothing | — | 300 s each |

- The constructor suite rose by 3 over 0477's 1870. The three new identities are `Passed` in the TRX.
- The kit-acceptance skip is AT-2's Explicit five-seed ensemble; the gate's skip policy is `allow`.
- AT-2's grid-generator fact, which includes the fit, took 791 s, inside R0005's 1800 s bound.
- The full build also carried 4 `SYSLIB0051` warnings from the untouched vendored `MathNetNumerics/Numerics`.
  They are pre-existing and absent from the final incremental build.
- A comment-only doc edit followed the elevation run. The class was re-run on the final build (7 of 7).

## Architecture

- `rankWith` is an intrinsic extension of `PlannerInformation`, declared after `PlanOption` and `RankedOption`, which
  name the type. It is pure, holds no cache and has no stopping rule. Each round re-takes `PlannerInformation.over` of
  the baseline, the options ranked and the challenger, in that order.
- The comparison is a closure local to `rankWith`. A `bool` beside `PlannerInformation` on a member would be ELEV010.
- The ranked option leaves the pool by position, so `'Candidate` needs no equality.

## Deferred

Nothing. Every requirement of the slice landed in this round.

## Gotchas

- `PlanOption.rows : CandidateRows` shares its label with `PlannerInformation.rows : ResidualJacobian`. Annotate the
  value before reading `.rows`.
- Ties are compared exactly, with no tolerance (the slice adds no threshold). Two options tie only when their joined
  informations are bit-equal, as with identical rows.
- With no options, `rankWith` answers `Ok []` without taking `over`, so the baseline is not validated then.
- The vendored MathNet's 4 `SYSLIB0051` build warnings predate the arc (in every build log from 0001 on). They are not
  this step's.

## Changelog

- 2026-11-17: step 0478. Added `PlanCriterion`, `PlanOption`, `RankedOption` and `PlannerInformation.rankWith`, with
  three `PlannerInformationTests` facts (constructor suite 1870 → 1873).

```yaml
gates:
  constructor_unit_tests: 1873
  berreman_unit_tests: 31
  kit_acceptance_tests: 14
```
