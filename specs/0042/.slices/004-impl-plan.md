# Impl-plan — spec 0042, slice 004 (IMPLEMENT_CONTRACT SVC_XDUO_0002 MuellerSolverProxy)

## Family / anchor

`IMPLEMENT_CONTRACT` (Development family). I build the REAL construction behind
the `MuellerSolverProxy` seam that slice 003 (`ADD_CONTRACT`) declared: a working
`createMathNetSvd ()` backed by the vendored MathNet.Numerics SVD least-squares,
replacing nothing at the type level (the seam is unchanged) and adding the real
proxy factory plus a real (non-mock) acceptance test. Per **Invariant 6 — act
only** I run **no** gates; the arc-runner's deterministic gate engine runs
`build` / `unit-tests` / `constructor-unit-tests` after I exit, and it also runs
the contract-implementation analyzer that verifies the member is genuinely
implemented. I flip the contract's lifecycle to `implemented` in the registry.

## Approach

Two edits, both in existing, compiling files:

1. `Berreman/OpticalConstructor/OpticalConstructor.Domain/MuellerReconstruction.fs`
   — append `createMathNetSvd () : MuellerSolverProxy` after the declared
   `MuellerSolverProxy` type. Its `solveLinearLeastSquares design rhs`:
   - validates the shape purely up front (empty design → `SingularDesign`;
     ragged rows → `SingularDesign`; zero columns → `SingularDesign`; an `rhs`
     length that does not match the row count → `SingularDesign`; any non-finite
     entry → `SingularDesign`) so a degenerate design never reaches the solver;
   - builds the design `A` and rhs `b` through the EXISTING
     `Berreman.MathNetNumericsMath` `RealMatrix.create` / `RealVector.create`
     wrappers (MathNet reached transitively exactly as `Propagation` opens it,
     Propagation.fs:4), unwraps to the backing `Matrix<double>` / `Vector<double>`
     only for `.Svd(true)` / `.Solve(b)` (members the DU does not surface);
   - reports the numerical `rank` (`Svd.Rank` = the count of non-negligible
     singular values). If `rank < columnCount` → `Error (RankDeficient rank)` —
     a rank-deficient design is not uniquely determined;
   - otherwise solves, computes `rmse = ‖A·x − b‖ / √m` through the `RealVector`
     operators + `.norm`, and returns `Ok { solution; rank; rmse }`;
   - runs the `.Svd()` / `.Solve()` block under `try/with` so any MathNet
     exception is caught AT this boundary and mapped to `SingularDesign` — never
     a throw across the proxy (CLAUDE.md boundary rule).
   - A doc block records the ALGLIB alternative (`rmatrixsolvels` behind the same
     proxy, living only in `OpticalConstructor.Optimization/AlglibAdapter.fs:20`)
     as an option explicitly NOT required for §7.2.

2. `Berreman/BerremanTests/MuellerReconstructionTests.fs` — add `[<Fact>]`s that
   instantiate the REAL `createMathNetSvd ()` (not the mock):
   - **full-rank known solve** (the acceptance): a small over-determined,
     full-column-rank, consistent system `A·x = b` with a known `x`; assert the
     returned solution matches `x` within the shared `allowedDiff` tolerance,
     `rank = columnCount`, `rmse ≈ 0`, and `solution.Length = columnCount`;
   - **typed-error mapping** (strengthening, not required by the acceptance):
     an empty design → `Error (SingularDesign _)`; a rank-deficient design
     (dependent columns) → `Error (RankDeficient _)` with `rank < columnCount` —
     both returned as values, never thrown.

## Files to modify

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/MuellerReconstruction.fs`
  (append `createMathNetSvd`)
- `Berreman/BerremanTests/MuellerReconstructionTests.fs` (add real-proxy facts)
- `specs/0042/.contracts-json` (flip `SVC_XDUO_0002` → `implemented`,
  `implementStep = 4`; byte-format matches the arc-runner's `atomic_write_json`)

## Risks

- **MathNet reachability / no new open.** The Domain must NOT open
  `MathNet.Numerics` directly (§ layout); I reach the SVD by unwrapping the
  `RealMatrix` / `RealVector` DUs to their backing `Matrix<double>` /
  `Vector<double>` and calling `.Svd(true)` / `.Solve(b)` — the type is already
  known, so no namespace open is needed (verified against
  `MathNetNumerics/.../Double/Factorization/DenseSvd.cs`: `Solve` handles the
  rectangular over-determined case; `Svd.Rank` counts non-negligible singular
  values).
- **Zero-warnings rule.** No FS0960 (I add only members, no class-level `let`s to
  the test type); the single-case DU `let (RealMatrix a) = …` is a complete
  pattern (no FS0025), the same idiom the module already uses; no unused bindings.
- **Float comparison** reuses the shared `allowedDiff` (`MatrixComparison.fs`) —
  no hand-rolled epsilon.
- **Invariant 6.** I do not run `dotnet build` / `dotnet test` (those ARE this
  slice's gates); the code is written correct-by-construction against the
  vendored MathNet API.

## Gates (roster from `004.gates`; run by the engine, not me)

`build`, `unit-tests` (`berreman_unit_tests`), `constructor-unit-tests`
(`constructor_unit_tests`).
