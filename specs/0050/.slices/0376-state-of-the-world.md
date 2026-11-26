# Step 0376 — state of the world

## Where we are

This is spec 0050 section S11.2, the forward model of an experiment and the study residual.

- **Steps 374 and 375** predicted every measured row (`BoundExperiment.predictedReadingOf`).
- **This step** forms one row's residual entries from that prediction and the row's measured reading, per kind. A
  Mueller row reuses the spec 0044 residual semantics: the 15 normalized element differences and the log-throughput
  ratio. That log-throughput term moved from Optimization into Domain so the study can use it.
- **The later S11.2 steps** assemble these row residuals into the study residual.

## What's working

- Move the log-throughput term into Domain as `MuellerInverse.throughputResidual`, refused as `normalized` refuses
- `MuellerFitting.throughputResidualFor` calls it and its local `logThroughput` is deleted, numbers bit-identical
- Add `StudyForward.RowResidual` and `RecordedReading.residualAgainst`:
  - a signal's difference;
  - Ψ's difference and Δ's taken on the circle;
  - a Mueller matrix's 15 normalized differences with its log-throughput ratio.
- Add `RowResidualErr` to `StudyForwardError`. A mismatched pair of kinds answers `ReadingMismatchErr`, naming both
- Add four row-residual facts and widen the error-render fact (constructor suite 1636 → 1640)

## Tests

```yaml
gates:
  constructor_unit_tests: 1640
  berreman_unit_tests: 7
```

| Gate | Result | Time (deadline) |
|---|---|---|
| build (`dotnet build Berreman.slnx -c Release -nologo -v:m`) | exit 0, `0 Error(s)`, no lowercase `error` in the log. The 12 warning lines are all NU1902 advisories, which are exempt | 45 s (600 s) |
| constructor-unit-tests (the gate's own command, whole suite) | `Test run summary: Passed!`, 1640 succeeded (1636 at checkpoint 375, +4), 0 failed, 0 skipped. In the TRX, the four new facts and the widened `every forward error renders its condition and its payload` are `Passed` under exactly the authorized ids | 74 s (180 s) |
| unit-tests (`BerremanTests.AbsorbingBiaxialInverseTests`) | `Test run summary: Passed!`, 7 of 7, 0 failed, 0 skipped. The count is unchanged from step 336 | 189 s (7200 s) |
| elevation (from arc base `f395597c`) | `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)`, exit 0 | 219 s (600 s) |
| the twenty s37 invariants | each `section_invariants --find-nothing` exits 0 (`INVARIANT HOLDS`) | a few seconds each (300 s) |

Nothing is deferred. Every duration is within its deadline.

## Architecture

- `throughputResidual measured model` is the one log-throughput term. It sits beside `observationResidual` in
  `MuellerInverse.fs`, because Domain cannot reference Optimization (.spec-md §4: a reuse is a move).
  - It checks `measured.normalized`, then `model.normalized`, so its refusal is `normalized`'s own, the measured
    matrix first.
  - It then answers `ResidualVector [| ln m₀₀(model) − ln m₀₀(measured) |]`.
- `throughputResidualFor` concatenates each observation's `throughputResidual` entry, or `[| 1.0e3 |]` on an `Error`.
  - It only gets there after `residualVector` succeeded, so the error branch is the totality guard it always was.
  - The evaluated expression is unchanged, so the numbers are bit-identical.
- `residualAgainst` returns `RowResidual { elements; throughputOpt }`, `this` being the prediction:
  - **signal:** `[predicted − measured]`, with no throughput;
  - **Ψ / Δ:** `[Ψ_p − Ψ_m; IEEERemainder (Δ_p − Δ_m, 2π)]` in radians, with no throughput;
  - **Mueller:** `observationResidual measured predicted`, with throughput `Some (throughputResidual measured predicted)`.
    An `InversionError` from either answers `RowResidualErr`.
  - **kinds that differ:** `ReadingMismatchErr "the prediction is <kind>, and the measured reading <kind>"`.
- `RowResidualErr` renders `the residual cannot be formed: <cause>`. The cause is each `InversionError` case's payload,
  rendered inline, because `InversionError` has no render member and the step adds none.

## Deferred

Nothing.

## Gotchas

- `throughputResidual` reuses `normalized` for its refusal instead of copying the m₀₀ test a third time. The cost is
  one extra 4×4 normalization per observation per residual evaluation, which is negligible beside the Berreman solve.
  The result does not use it.
- The step allows no new member. So the reading kinds in `ReadingMismatchErr` are named by a local `kindOf` closure
  inside `residualAgainst`, not by a render member on `RecordedReading`.
- The mismatch arms are written per first kind, not as a wildcard. A new `RecordedReading` case then shows up as an
  incomplete match.
- `MuellerMatrix` has no scalar product. The facts scale matrices through `RealMatrix4x4.create` (the test-local
  `scaledBy`).

## Changelog

- 2026-11-13 — Step 0376: the log-throughput term moved into Domain as `MuellerInverse.throughputResidual`. The fit's
  `throughputResidualFor` calls it, with bit-identical numbers. Added `StudyForward.RowResidual`,
  `RecordedReading.residualAgainst` (signal, Ψ and the wrapped Δ, Mueller with the log-throughput ratio) and
  `RowResidualErr`. Four new facts and the widened error-render fact (constructor suite 1636 → 1640).
  `AbsorbingBiaxialInverseTests` is unchanged at 7.
