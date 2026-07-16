# Impl-plan — spec 0042, slice 003 (ADD_CONTRACT SVC_XDUO_0002 MuellerSolverProxy)

## Family / anchor

`ADD_CONTRACT` (Contract family). I **declare a surface** (proxy + result +
error types), ship an inline **mock**, and a **test** that pins the surface
through its exact signature. I do **not** build the real solver — that is the
later `IMPLEMENT_CONTRACT SVC_XDUO_0002` (slice 004, `createMathNetSvd ()`).
Per **Invariant 6** I run no gates; the arc-runner's gate engine does that after
I exit.

## Approach

Two files, both already present and compiling:

1. `Berreman/OpticalConstructor/OpticalConstructor.Domain/MuellerReconstruction.fs`
   — append to the `MuellerReconstruction` module (after `frobeniusDiff`):
   - `type SolverError = SingularDesign of reason : string | RankDeficient of rank : int`
     — typed failure of the solve, never a throw across the boundary.
   - `type LeastSquaresSolution = { solution : float[]; rank : int; rmse : float }`
     — the elevated result; the numerical `rank` is a first-class observability
     check (a full-rank solve has `rank = column count`).
   - `[<ReferenceEquality>] type MuellerSolverProxy =
        { solveLinearLeastSquares : float[][] -> float[] -> Result<LeastSquaresSolution, SolverError> }`
     — the functional-proxy seam, following the `ExperimentDataProxy`
     convention (`ExperimentDataProxy.fs:43`): function-valued field ⇒
     `[<ReferenceEquality>]` so a host context (Optimization/Elmish) that holds
     one keeps its required equality.

2. `Berreman/BerremanTests/MuellerReconstructionTests.fs` — add, to the existing
   `MuellerReconstructionTests()` class type:
   - `let` bindings (ahead of the members — FS0960): a canned
     `LeastSquaresSolution` and `makeMockSolver ()` returning an inline
     `MuellerSolverProxy` stub. The stub returns the canned solution for a
     full-height (≥16-row) design, `RankDeficient` for an under-determined one,
     and `SingularDesign` for an empty design — exercising the exact signature
     and both error cases with no real linear algebra.
   - a `[<Fact>]` that binds `solveLinearLeastSquares` to an explicitly-typed
     local (pinning the exact signature), then asserts the Ok canned path and
     both typed `SolverError` cases (mirroring `ExperimentDataProxyTests.fs:38`).
   - a second `[<Fact>]` pinning the `[<ReferenceEquality>]` behaviour
     (mirrors `ExperimentDataProxyTests` third fact).

## Files to modify

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/MuellerReconstruction.fs` (append types)
- `Berreman/BerremanTests/MuellerReconstructionTests.fs` (add mock + facts)

## Risks

- **FS0960** (class-type binding order): the new `let`s must precede all
  members. This was the slice-002 attempt-01 failure — I place them with the
  existing top-of-type `let`s.
- **Float comparison**: reuse the shared `allowedDiff` tolerance
  (`MatrixComparison.fs`) for `rmse` / solution element checks; no hand-rolled
  epsilon.
- **Unused lambda param**: the mock ignores `rhs`; name it `_rhs` to avoid any
  unused-binding warning (zero-warnings rule).
- **No collision**: grep confirms `SolverError` / `LeastSquaresSolution` /
  `MuellerSolverProxy` / `solveLinearLeastSquares` do not exist in any `.fs`
  today. The contract registry already lists `SVC_XDUO_0002` (active).

## Gates (roster from `003.gates`; run by the engine, not me)

`build`, `unit-tests` (`berreman_unit_tests`), `constructor-unit-tests`
(`constructor_unit_tests`).
