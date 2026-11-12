# Step 0335 — state of the world

## Where we are

Spec 0050 section S10.1 moves the bench noise model and the fit harness into Optimization. Steps 332–334 moved the
crystal tensor construction onto the product parameter records. This step gives the fit machinery its product home,
`OpticalConstructor.Optimization/MuellerFitting.fs`, and moves the first two names into it: `configurationOf` and
`observeWith`. Steps 337, 339, 341 and 342 each add one `MuellerFitError` case as they move the rest of 001 §Q's
promoted list.

## What's working

- Added `OpticalConstructor.Optimization.MuellerFitting`, compiled directly after `MuellerInverseSolver.fs`, with the
  typed `MuellerFitError` (`ObservationForwardErr of ForwardModelError`).
- Moved `configurationOf` to the product; incidence and azimuth are now measured `float<deg>`.
- Moved `observeWith` to the product as a `Result`: the first forward failure is `ObservationForwardErr` and nothing
  after it is evaluated.
- `InverseFitHarness` deletes both functions and adds `expectOk`, the test-side unwrap every later move uses.
- Every inverse suite builds configurations and observations through the product. Numbers and counts are unchanged.

## Tests

These are development runs of each gate's own command from `0335.gates`, over the final tree. The arc-runner's gate
engine runs the gates themselves.

| Gate | Result | Time | Deadline |
|---|---|---|---|
| build | exit 0, 0 errors | 82 s | 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`; known findings 0; ELEV004 excluded | 296 s | 600 s |
| constructor-unit-tests (all) | Passed!, 1552 of 1552, 0 failed, 0 skipped | 89 s | 180 s |
| unit-tests (`BerremanTests.FeasibleBiaxialInverseTests`) | Passed!, 5 of 5, 0 failed, 0 skipped | 225 s | 7200 s |
| invariant-s33-\* (19) | each `INVARIANT HOLDS`, exit 0 | under 1 s each | 300 s |

- **build.** The only warnings are the two known `SYSLIB0051` warnings from the vendored MathNet `Exceptions.cs`. No
  file this step touched produced a warning.
- **Counts.**
  - constructor-unit-tests counts 1552, equal to checkpoint 333.
  - `FeasibleBiaxialInverseTests` declares 5 facts at HEAD and in the working tree, and all 5 ran: the five named in
    the step's effects.
  - No test was added, removed or renamed, and no expected value or tolerance changed.
- **Elevation** had no comparison base, so it covered all 12 projects. `OpticalConstructor.Optimization`, which holds
  the new module, reports 0 findings and 0 suppressed. The tree-wide suppressed total (ELEV004) is 1475, the same as
  step 0334: the move brings no `.value` read into an analyzed project.
- **One declaration each.** `grep -rn` over `Berreman/` finds `configurationOf`, `observeWith` and `MuellerFitError`
  only in `MuellerFitting.fs`, and `expectOk` over `MuellerFitError` only in `InverseFitHarness.fs`.
- **LF.** No edited or new file has a CR, and `git diff --numstat` equals `--ignore-cr-at-eol`.
- No run exceeded its effective deadline. The constructor tests ran concurrently with elevation and unit-tests, at 89 s
  against 180 s.

```yaml
gates:
  berreman_unit_tests: 5
  constructor_unit_tests: 1552
```

## Architecture

- **The product home of the fit machinery is `MuellerFitting`** in `OpticalConstructor.Optimization`, after
  `MuellerInverseSolver`. It is the module the study engine and the Planner will call (001 §Q, §S10). The test harness
  opens it and keeps only test assertions.
- **Errors are values.** The harness's `failwith` on a ground-truth forward failure is now
  `Error (ObservationForwardErr e)`. The tests unwrap it with `InverseFitHarness.expectOk`, which fails the test with
  the typed error.
- **`observeWith` is the `forwardModels` fold**: `Ok []`, then `Result.bind` and cons, then `List.rev`. The fold carries
  `ForwardModelError` and maps it once at the end, which gives the same value as wrapping at the first failure.
- **Measured degrees at the boundary.** `configurationOf` takes `float<deg>`, the form `Angle.degree` and
  `SampleAzimuth.degree` already take. The tests keep their readable `double` tables and multiply by `1.0<deg>` at the
  one call in each local `configuration` helper.
- **Bit-identical by construction.** `x * 1.0<deg>` is the multiplication the old harness did itself, so `Angle.degree`
  receives the same double. The forward calls run in configuration order, as the old `List.map` ran them.

## Deferred

Nothing. Every requirement of the slice landed. The slice assigns these to later steps:

- one `MuellerFitError` case each at steps 337, 339, 341 and 342, with the rest of 001 §Q's promoted list.

## Gotchas

- **`git grep` misses the new `MuellerFitting.fs` until it is staged.** Use `grep -rn` to count declarations before the
  commit.
- **`OpticalConstructor.Tests/KitArchiveProxyTests.fs` has its own module-level `expectOk<'Value>`** over
  `KitArchiveError`. Both are in test projects, which the ELEV analyzers do not scan, and neither project references
  the other. The step names the harness's `expectOk`, so neither was merged or renamed.
- **`IncidenceAngle.create` folds into [0, 90) deg.** A negative incidence passed to `configurationOf` does not tilt
  the plate the other way. The noise model's `disturb` avoids this by using the `IncidenceAngle + Angle` operator.
- `.manifest.state.json` was already modified when the round started. It is the supervisor's file and was not touched.

## Changelog

- 2026-11-12 — Step 0335: added `OpticalConstructor.Optimization.MuellerFitting` with `MuellerFitError`,
  `configurationOf` (`float<deg>`) and `observeWith` (a `Result`).
  - Deleted both functions from `InverseFitHarness` and added `expectOk`.
  - Every inverse suite now goes through the product. Numbers and counts are unchanged.
