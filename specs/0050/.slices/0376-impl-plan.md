# Step 0376 — impl plan

Spec 0050 S11.2: the residual entries of one measured row, per kind. A prediction (`RecordedReading`, step 375) is
compared with its row's measured reading. A Mueller matrix reuses the spec 0044 residual semantics: the 15 normalized
element differences of `MuellerInverse.observationResidual`, plus the log-throughput term. That term lives today in
`MuellerFitting.throughputResidualFor` (Optimization), which Domain cannot reference, so it moves into Domain.

## Approach

1. `OpticalConstructor.Domain/MuellerInverse.fs`, after `residualVector`: `throughputResidual measured model`, the
   one entry `ln m₀₀(model) − ln m₀₀(measured)`. It refuses as `normalized` refuses, the measured matrix first, then
   the model. The refusal reuses `normalized` itself, so the non-positive / non-finite test is not copied a third time.
2. `OpticalConstructor.Optimization/MuellerFitting.fs`: `throughputResidualFor` takes each observation's term from
   `throughputResidual o.measured model`. An `Error` gives `1.0e3`, where the deleted local `logThroughput` gave
   `None`. The same `log` and the same subtraction run, so the numbers stay bit-identical.
3. `OpticalConstructor.Domain/StudyForward.fs`:
   - the `RowResidualErr of error : InversionError` case on `StudyForwardError`, rendered as
     `the residual cannot be formed: <the error>`, with each `InversionError` case's payload rendered inline (it has
     no render member, and the step adds none);
   - after `predictedReadingOf`, the record `RowResidual = { elements; throughputOpt }`;
   - `RecordedReading.residualAgainst`: signal − signal; Ψ difference and Δ difference wrapped with
     `IEEERemainder (·, 2π)`; Mueller → `observationResidual` + `throughputResidual`; mismatched kinds →
     `ReadingMismatchErr` naming both kinds.
4. `OpticalConstructor.Tests/StudyForwardTests.fs`: four new facts under exactly the authorized ids, and the
   error-render fact widened by `RowResidualErr`.

## Risks

- ELEV: the new free function has two `MuellerMatrix` parameters and no primitive. Both new type names and the case
  name are unique in the repo and in STL. The kind names are rendered by a local closure, not a member, because the
  step says "no other member is added".
- Bit-identity: `throughputResidualFor` only reaches the throughput term after `residualVector` succeeded. So both
  m₀₀ values are already positive and finite, and `throughputResidual` returns exactly `log model − log measured`.
- `unit-tests` selects `BerremanTests.AbsorbingBiaxialInverseTests` (slow inverse class). Run it once after the build.
