# Step 0481 — state of the world

## Where we are

Spec 0050 S14.2, the Measurement Planner's numerical layer. Steps 477 to 479 built `PlannerInformation`:
- the Fisher information of candidate experiments;
- their rank, confounded combinations, condition and Cramér–Rao bounds;
- the candidate rows at a nominal point.

Step 480 moved the noisy-experiment runners into `BerremanTestSupport`. This step lands the planner's layer-2
acceptance in `BerremanTests.MeasurementPlannerTests`. Over spec 0044's own designs, the planner must reproduce the
facts `MuellerInverseTests` already measures (001 §M): full rank over C1–C4, C1 blind to n_e and g33, and Cramér–Rao
bounds equal to a noisy fit's own covariance prediction, for quartz and for AT-2. No new inverse physics is asserted
(001 §W).

## What's working

- Added the planner fact over the quartz design. The full C1–C4 set is rank 4 with no confounded combination, a
  finite condition and a finite positive bound on each of n_o, n_e, g11 and g33.
- In the same fact, C1 alone is rank 2, and its two confounded combinations name exactly n_e and g33.
- Added the quartz noisy-fit fact. At the fit's own Jacobian and reduced χ², each of the planner's bounds over
  |truth| equals `predictedRelativeErrors` within 1e-12 relative.
- Added the same fact for AT-2's fifteen-constant absorbing design (a1 @ a2 @ a3, first noisy seed).
- `MeasurementPlannerTests` grew from 4 to 7 facts, all passing.

## Tests

These are development runs. The arc-runner's gate engine is the authority. No run exceeded its effective deadline.

| Gate | Result | Duration | Deadline |
|---|---|---|---|
| build | exit 0, no `error`. The only warnings are the exempt `NU1902`s and the vendored MathNet's pre-existing `SYSLIB0051`s. | 86 s | 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)`; 0 known, 1770 suppressed (unchanged) | 227 s | 600 s |
| unit-tests | `Passed!` 7 of 7 (`BerremanTests.MeasurementPlannerTests`, the step's selected class) | 216 s | 7200 s |
| invariant-s46-* (21) | each decoded `git grep` finds nothing | — | 300 s each |

- **The three new identities pass under their exact census names:**
  - `the planner finds the full C1-C4 set full rank with every quartz constant bounded, and C1 alone blind to n_e and
    g33` (4.6 s);
  - `the planner's bounds for the noisy quartz design are what one noisy fit's own covariance predicts` (36.7 s);
  - `the planner's bounds for AT-2's noisy design are what one noisy fit's own covariance predicts` (172.7 s).
- **Count.** The four step-475 facts pass unchanged, and the class count rose by 3 (4 → 7).
- **Scope.** unit-tests ran with the gate's own selector only, never the whole suite.

```yaml
gates:
  berreman_unit_tests: 7
```

## Architecture

- **No production change.** The facts call only existing product code:
  - `PlannerInformation.over` / `candidateRowsOver` / `boundsOver`;
  - `MuellerFitting.residualJacobianAt`;
  - `FitUncertainty.predictedRelativeErrors`;
  - the `QuartzSample` / `AbsorbingBiaxialSample` runners.
- **Why the bound equals the prediction exactly, not approximately.**
  - The planner's single candidate is the fit's Jacobian where it stopped, taken by the same
    `FitQuality.residualJacobian` that `reportFrom` uses, with the report's reduced χ² as its noise.
  - The row factor √(noise / noise) is exactly 1.
  - `scaledSigmasOpt` is √diag(`covarianceOf` rows noise), the report's own standard-error arithmetic (step 477).
  - So the Cramér–Rao bound at the fit's residual level *is* the covariance account the fit gives, in the same
    relative units.
- **One generic class-local helper, `assertBoundsArePredicted truth first`, serves the quartz and the AT-2 facts.**
  - It first asserts that the bound names equal the prediction names in axis order.
  - It then compares each constant.
  - A failure names the constant, the planner's bound and the prediction.
- **Each design is named by its module** (`QuartzSample.c1`, `AbsorbingBiaxialSample.recoverAbsorbingFromNoisy`), as
  step 475 does.

## Deferred

Nothing. Every requirement of the slice landed in this round.

## Gotchas

- **C1's numerical rank is 2, though layer 1 (step 475) calls n_o unrecoverable from C1.**
  - The planner reads the throughput residual. In it, n_o moves m₀₀ through the surface reflectance, so it clears the
    1e-6 rank floor beside g11.
  - n_e and g33 have identically zero columns.
  - Layer 1 is the structural verdict and layer 2 the numerical one, and both facts hold.
- **The null space of C1 is span{n_e, g33}, and the SVD may return any orthonormal basis of it.**
  - The fact therefore asserts the union of the names the two `CombinationText`s carry, read by token in coordinate
    order, rather than each text verbatim.
- **Runtime.** The class now carries one transparent fit (~37 s) and one absorbing fit (~173 s) and runs in about
  3.6 min.
- **Untouched files.** R0001–R0005 bind files this step does not touch; their behavior is preserved by construction.

## Changelog

- 2026-11-18: step 0481. Three Planner layer-2 acceptance facts joined `MeasurementPlannerTests`:
  - full C1–C4 rank with every quartz constant bounded, and C1 alone rank 2, blind to n_e and g33;
  - the planner's Cramér–Rao bounds at a noisy fit's own residual level equal that fit's `predictedRelativeErrors`,
    for quartz and for AT-2.

  7 of 7 pass.
