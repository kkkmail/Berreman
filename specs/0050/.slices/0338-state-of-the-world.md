# Step 0338 — state of the world

## Where we are

Spec 0050 section S10.1 moves the bench noise model and the fit harness into Optimization.

- Steps 335–337 gave `OpticalConstructor.Optimization/MuellerFitting.fs` its observation generator, residual
  closures, Jacobian adapter, scaling, search box, fit record and the three fit runners.
- This step moves the relative error and the per-parameter recovery errors (obligation S101-RELATIVE-ERROR-MOVE).
  After `fitAbsorbingObservations` the module now declares `QuantityValue`, `RelativeError` and `recoveryErrors`.
  `InverseFitHarness.fs` declares neither `RelativeError` nor `recoveryErrors`.
- `RelativeError.between` now takes two `QuantityValue`s instead of two bare doubles.
- Steps 341 and 342 move the ensemble statistics and the fit's own uncertainty report, which read every recovered
  value through `QuantityValue`.

## What's working

- Moved `RelativeError` and `recoveryErrors` from `InverseFitHarness` into
  `OpticalConstructor.Optimization.MuellerFitting`, with exactly one declaration of each.
- Added `QuantityValue`, a recovered quantity's value in its own units. `RelativeError.between` now takes two of them,
  where it took two bare doubles.
- Renamed `RelativeError`'s union case to `RelativeErr`, because ELEV020 requires every case of a `*Error` union to end
  in `Err`. The type name, `value` and `between` are unchanged.
- The harness's `statisticsFor` and `MuellerInverseTests.relativeError` wrap their doubles in `QuantityValue`. The
  numbers are bit-identical.
- Counts are unchanged: `FeasibleBiaxialInverseTests` ran 5 of 5 and the constructor suite 1552 of 1552.

## Tests

These are development runs of each gate's own command; the arc-runner's gate engine runs the gates themselves.
Logs are under `specs/0050/.artifacts/0338-dev-*.log`.

| Gate | Outcome | Measured | Deadline |
|---|---|---|---|
| build | exit 0, 0 warnings, 0 errors | 41 s | 600 s |
| elevation (arc base `f395597c…`, 12 of 12 projects) | `ELEV-GATE: OK - 0 new finding(s)` | 204 s | 600 s |
| constructor-unit-tests (whole) | Passed!, 1552 of 1552, 0 failed, 0 skipped | 70 s | 180 s |
| unit-tests (`BerremanTests.FeasibleBiaxialInverseTests`) | Passed!, 5 of 5, 0 failed, 0 skipped | 175 s | 7200 s |
| 19 × invariant-s33-* | every `git grep` found nothing | < 1 s each | 300 s |

- No run exceeded its deadline.
- The first elevation run reported one ELEV020 finding on the moved case. It was fixed by the case rename, and the
  re-run is clean.
- The change-expectation fact `the feasible measurement set recovers all NINE constants from a perturbed start`
  passed. It now asserts on the product's `recoveryErrors`.
- The selected class's count of 5 equals step 0335's record.

```yaml
gates:
  berreman_unit_tests: 5
  constructor_unit_tests: 1552
```

## Architecture

- **`QuantityValue` is the recovered-value seam.** It is a single-case DU over a double in the quantity's own
  physical units: an index, a gyration or ε″ component, a birefringence. `RelativeError.between` is the one place a
  relative error is formed, over two of them. It closes the ELEV010 shape of two bare doubles beside one of our types.
- **`RelativeError`'s case is `RelativeErr`.** ELEV020 is name-based, and a product union named `*Error` must have
  `Err`- or `Exn`-suffixed cases. The analyzers win over the step text (AGENTS.md; spec 0048 `.manual/002` R6). Code
  constructs the value with `|> RelativeErr` and reads it through `value`.
- `recoveryErrors` qualifies `ItemModels.ParameterName` rather than opening `ItemModels` into the fitting module.

## Deferred

Nothing. Every requirement of the step landed in this round.

## Gotchas

- **Later steps' text builds the value with `|> RelativeError`.** Step 341's `standardErrorOfMean` does, and step 436
  calls `RelativeError.between` on bare doubles. They must write `|> RelativeErr` and wrap in `QuantityValue`. The
  type name and `between` are as the spec states.
- `BerremanTests.fsproj`'s comment (near line 38) still lists `RelativeError` and `SearchBox` as harness types. The
  project file is outside this step's effects.
- `InverseFitHarness.fs`'s measurement-error banner says "Everything above this line …". Only `open`s are above it
  now. This step did not name that comment, so it is unchanged.
- `MuellerFitting.fs` does not open `ItemModels`, so a moved signature naming `ParameterName` qualifies it.

## Changelog

- 2026-11-12 — Step 0338: moved `RelativeError` and `recoveryErrors` from `InverseFitHarness` into
  `OpticalConstructor.Optimization.MuellerFitting`, adding `QuantityValue` for `between`'s two arguments.
  - The case is renamed `RelativeErr` (ELEV020).
  - The harness's `statisticsFor` and `MuellerInverseTests.relativeError` wrap in `QuantityValue`.
  - Numbers and counts are unchanged.
