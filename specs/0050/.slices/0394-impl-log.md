# Step 0394 — impl log

## Progress

- [x] Read the protocol, the project prompt, the slice and its gates. There are no operator rulings and no operator
  note this step.
- [x] `ResidualJacobian.rank : MatrixRank`, declared directly after `conditionNumber`.
  - It uses the same `RealMatrix.create` + `Svd(false)` seam.
  - It counts the singular values above `1.0e-6 ×` the largest.
  - It is `MatrixRank 0` for an empty Jacobian, or when the largest singular value is not above zero.
  - A three-line doc cites S11.4.
- [x] `LeastSquaresSystem.residualsAt (solution : RealVector) : RealVector`, declared directly before
  `residualRmseAt`.
  - `residualRmseAt` is now `(this.residualsAt solution).norm / sqrt (float this.design.Length) |> ResidualRmse`.
- [x] New `OpticalConstructor.Tests/FitResultsTests.fs` (`module FitResultsTests`, namespace
  `OpticalConstructor.Tests`). It holds the brief's two facts. Its compile line is last in the list, with no comment.
- [x] First build green; the two new facts pass (2/2).
- [x] Mutation checks, both applied at once:
  - M1: the floor `1.0e-6` lowered to `1.0e-12`. The rank fact fails: `Expected: MatrixRank 1`, `Actual:
    MatrixRank 2`.
  - M2: `A·x − b` turned into `A·x + b`. The residuals fact fails: `Expected: -0.1`, `Actual: 0.1`.
  - The source was restored byte for byte from saved copies (`cmp`), and the copies were deleted.
  - The rebuild recompiled the Domain dll (02:21:44), after the restore (02:21:27).
- [x] Gate runs on the restored tree: build, constructor-unit-tests (whole), unit-tests (selected), elevation, and the
  twenty s39 invariants. All green (Testing state).

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/MuellerInverse.fs`: `ResidualJacobian.rank`.
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/MuellerReconstruction.fs`: `LeastSquaresSystem.residualsAt`;
  `residualRmseAt` reads it.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/FitResultsTests.fs` (new).
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj`: one `<Compile>` line.

No other declaration of either Domain file changed.

## Decisions

- **"Empty" also covers a Jacobian with rows but no columns.** The guard is
  `jacobian.Length = 0 || jacobian.[0].Length = 0`.
  - The brief says `MatrixRank 0` for an empty Jacobian. A fit with no free parameter has an `m × 0` Jacobian,
    whose rank is 0.
  - `RealMatrix.create` cannot build a matrix from zero-length rows, so this guard keeps the member total.
  - When there are rows and columns, `svd.S` has `min(m, n) ≥ 1` entries, so `List.max` is safe.
- **`MatrixRank` is built by a pipe at its origin** (`… |> List.length |> MatrixRank`), the way `conditionNumber`
  builds its value. This avoids a call-site re-wrap of a bare local (ELEV001).
- **The RMSE is unchanged by construction.** The old body was `let residual = <A·x − b> in residual.norm / sqrt m`.
  The new one names that same expression `residualsAt` and takes `.norm / sqrt m` of it: the same MathNet operations
  in the same order. So the RMSE that `createMathNetSvd` and the product check report is the same doubles. The
  selected `MuellerReconstructionTests` and `FeasibleBiaxialInverseTests` pass unchanged.
- **The doc numbers.** The doc says why the floor is relative and where it sits: two orders above a
  forward-difference Jacobian's noise at ALGLIB's step (~1e-8 of a column). The fact's cases sit well clear of it:
  - Near-confounded columns, agreeing to a part in 1e9: the second singular value is about 3e-10 of the first,
    far below the floor.
  - `[1; 0], [0; 1e-4]`: the ratio is 1e-4, far above the floor.
- **Tolerances in the tests:** `Assert.Equal(expected, actual, 1e-15)`, the constructor suite's precedent (for
  example `FitRunTests`, `DataSplitTests`). `BerremanTests/MatrixComparison.fs` is not referenced by this project.

## Testing state

Local gate runs on the final, restored tree (2026-11-14). Every run finished within its effective deadline:

- `build` (`dotnet build Berreman.slnx -c Release -nologo -v:m`): exit 0, 0 errors, 18 s (deadline 600 s).
  - This final build was incremental. The first build took 81 s and recompiled MathNet, which brought the vendored
    `SYSLIB0051` pair.
  - The final log's warnings are 12 × NU1902 only.
  - Log: `0394-build.log`.
- `constructor-unit-tests`, whole suite: `Test run summary: Passed!`, 1698/1698, 114 s (deadline 180 s).
  - The count is 1696 → 1698, +2.
  - The TRX shows both new ids `Passed`.
  - Log: `0394-constructor-unit-tests.log`.
- `unit-tests` (`FeasibleBiaxialInverseTests`, `MuellerReconstructionTests`): `Test run summary: Passed!`, 30/30
  (5 + 25), 2 min 52 s (deadline 7200 s). Log: `0394-unit-tests.log`.
- `elevation`: `ELEV-GATE: OK - 0 new finding(s) in 13 project(s), 229s` (deadline 600 s).
  - 1579 findings suppressed and 0 known, the same as step 393.
  - It ran alongside the unit-tests run.
  - Log: `0394-elevation.log`.
- The twenty `invariant-s39-*` greps, decoded from the snapshot: all twenty find nothing. Log: `0394-invariants.log`.
- LF: every file written has 0 CR bytes, and `git diff --numstat` equals `--ignore-cr-at-eol`.

## Artifacts

All under `specs/0050/.artifacts/`:

- `0394-build.log`
- `0394-constructor-unit-tests.log`
- `0394-unit-tests.log`
- `0394-elevation.log`
- `0394-invariants.log`
