# Step 0396 — state of the world

## Where we are

Spec 0050, section S11.4: fit results (uncertainty, correlation, realizability, product check, profile likelihood).

- Step 394 added the Jacobian's rank, and step 395 the suspect thresholds.
- This step declares the shapes a fit run records as its results.
- Steps 397-403 fill them, and step 404 stores them in the fit run.

## What's working

- Added the S11.4 result shapes to `FitRuns.fs`, between `ObjectMatrixFit` and `FitRun`:
  - entries, coordinates and subjects (`FitEntryRef`, `FitCoordinate`, `FitProblemSubject`);
  - per-solve quality, σ and run residuals (`FitProblemQuality`, `FitEntrySigma`, `RunResidual`);
  - the product check (`SeriesProductCheck`) and the profiles (`SuspectReason`, `ProfileOutcome`, `ProfilePoint`,
    `ParameterProfile`).
- Added `JacobianColumnNorms` and `CorrelationMatrix`, two NumericCarriers read whole from `ResidualJacobian`.
- Added `ObjectRealizability.realizability`: `RealizableMueller` when every Cloude eigenvalue is at least −1e-12 of
  their sum, else `UnrealizableMueller`.
- Added `FitRunResults`, which gathers a fit's sigmas, problems, run residuals, realizability, product checks and
  profiles.
- Added the `FitResultsTests` fact: the identity and a retarder are realizable, and diag(1, 1.5, 0, 0) is not;
  −1e-14 is round-off and −1e-6 is not.

## Tests

Run locally on 2026-11-14. Every run finished within its effective deadline.

- `build`: exit 0, 0 errors. The first build took 77 s and the final one 19 s (deadline 600 s).
  - The warnings are only the exempt NU1902 advisories and the vendored MathNet `SYSLIB0051` pair.
- `constructor-unit-tests`, whole suite: 1700/1700, 115 s (deadline 180 s).
  - The count is 1699 → 1700, +1, as the acceptance requires.
  - The TRX shows the new fact `Passed`.
- `elevation`: `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)`, 217 s (deadline 600 s).
- The twenty `invariant-s39-*` greps all find nothing.
- Mutation checks: moving the floor to −1e-5, or to 0, fails only the new fact. The file was restored byte for byte
  before the final build and test run.

```yaml
gates:
  constructor_unit_tests: 1700
```

## Architecture

- **Coordinates are separate from entries.** A solve's coordinates are not always fitted entries:
  - a series' combined-matrix element and the AIR fringe visibility have no entry;
  - a calibration constant can be the value of several entries.

  So `FitProblemQuality` names its coordinates through `FitCoordinate`, and a value's σ is recorded per entry in
  `FitEntrySigma`.
- **Realizability is derived, not stored.** `MuellerRealizability` carries no tag. The stored data is the four
  eigenvalues, and `realizability` judges them with a round-off floor relative to the coherency trace.
- **Payload unions are stored in the serializer's case form, as `FitMemberOutcome` is.** These are `FitCoordinate`,
  `FitProblemSubject`, `SuspectReason` and `ProfileOutcome`, and none carries a wire tag.

## Deferred

Nothing. Every requirement of the step landed this round. Filling the shapes (397-403) and storing them (404) are
separate steps of the spec.

## Gotchas

- **Shared record labels.** These labels now also name fields of earlier records:
  - `parameter` / `owner` (`FitRunEntry`, `FitRunParameter`, `StudyUnknown`);
  - `items` / `rank` / `rmse` (`ObjectMatrixFit`);
  - `entry` / `outcome` (`FitRunMember`).

  Annotate record expressions that build the new shapes, as the new fact does with `: ObjectRealizability`.
- **`realizability` unwraps `CoherencyEigenvalue.value` in place.** These are two ELEV004 hits, measured but not gated.
  No member was added to `MuellerInverse.fs`, which is outside the step's edits.
- **A NaN eigenvalue or sum reads as unrealizable.** The `>=` test fails on NaN.

## Changelog

- 2026-11-14 — Step 0396: the S11.4 fit-result shapes in `FitRuns.fs`.
  - Added `FitEntryRef` … `FitRunResults` and `ObjectRealizability.realizability`.
  - Added one `FitResultsTests` fact.
