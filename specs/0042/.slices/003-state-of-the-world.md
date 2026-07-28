# State of the world — spec 0042, slice 003 (ADD_CONTRACT SVC_XDUO_0002 MuellerSolverProxy)

## Where we are

Slice 003 is the third slice of arc 0042 and the FIRST of the `MuellerSolverProxy`
contract pair: an `ADD_CONTRACT` step that *declares* the least-squares SOLVE seam
the Mueller reconstruction will fit through, ships an inline mock, and pins the
surface with a test — without any real linear algebra. The seam sits directly
behind the pure column-major core from slice 002 (`kron4` builds one design row;
`muellerOfVecColumnMajor` un-vecs a returned solution). The contract is left at
`declared` lifecycle; the real MathNet-SVD-backed `createMathNetSvd ()` lands in
the paired `IMPLEMENT_CONTRACT SVC_XDUO_0002` (slice 004).

## What's working

- Declare the `MuellerSolverProxy` SOLVE seam in `MuellerReconstruction` (Domain),
  following the `[<ReferenceEquality>]` `ExperimentDataProxy` convention.
- Add the typed `SolverError` channel (`SingularDesign of reason` /
  `RankDeficient of rank`) — never a throw across the proxy boundary.
- Add the elevated `LeastSquaresSolution` result (`solution` / `rank` / `rmse`),
  making the numerical rank a first-class observability check.
- Add an inline mock `MuellerSolverProxy` plus two BerremanTests facts: one drives
  `solveLinearLeastSquares` through its exact signature (canned solution + both
  typed `SolverError` cases), one pins the proxy's reference equality.

## Tests

Gate roster for this slice (from `003.gates`): `build`, `unit-tests`
(`berreman_unit_tests`), `constructor-unit-tests` (`constructor_unit_tests`).

Per the `ADD_CONTRACT` worker's **Invariant 6 (the worker acts; it runs no
checks)**, this worker did NOT execute the gates — the arc-runner's deterministic
gate engine runs them after exit. New coverage in
`BerremanTests/MuellerReconstructionTests.fs`:

- **exact-signature solve** — a stub `MuellerSolverProxy` whose
  `solveLinearLeastSquares` is bound to an explicitly-typed local
  (`float[][] -> float[] -> Result<LeastSquaresSolution, SolverError>`, so the
  compiler rejects any drift), asserted across a full-height design (canned
  `Ok` solution: 16 elements, `rank = 16`, `rmse = 0`), an under-determined
  design (`Error (RankDeficient 2)`), and an empty design
  (`Error (SingularDesign _)` with a non-empty reason) — every branch returns a
  value, never a throw;
- **reference equality** — the `[<ReferenceEquality>]` proxy compares by identity
  (`p = p` holds; `p = makeMockSolver ()` does not), mirroring
  `ExperimentDataProxyTests`.

Float readouts (`rmse`, solution elements) are compared with the shared
`allowedDiff` tolerance (`MatrixComparison.fs`), not a hand-rolled epsilon.

```yaml
gates:
  berreman_unit_tests:  0
  constructor_unit_tests: 0
```

(The gate engine records the real captured counts post-exit; the baseline for a
`count_at_least` gate is the prior green step's checkpoint, not a value the worker
authors — 0 is the safe floor here. The two new facts only add to the count.)

## Architecture

- **The SOLVE seam is a functional proxy, `declared` only.** `MuellerSolverProxy`
  is a `[<ReferenceEquality>]` record of one camelCase `Result`-returning function
  — the exact `ExperimentDataProxy` shape (`ExperimentDataProxy.fs:43`). Logic
  that holds it stays referentially transparent; a test substitutes an in-memory
  stub of the same shape. No SVD/ALGLIB/wiring in this slice.
- **`[<ReferenceEquality>]` is load-bearing.** The record's only field is
  function-valued, so it has no structural equality; the attribute makes it
  compare by identity, keeping a host context (Optimization / Elmish) that holds
  one comparable. `LeastSquaresSolution` (all data fields) keeps its default
  structural equality — no attribute needed there.
- **Errors are typed values, results are elevated.** `SolverError` gives the
  singular / rank-deficient failures a named channel carrying diagnostic payload
  (`reason` / `rank`); `LeastSquaresSolution` elevates the raw `float[]` answer
  into a named record that also surfaces the numerical `rank` and residual `rmse`
  — a caller trusts the fit by comparing `rank` against the column count.
- **The seam bolts onto the slice-002 core.** A design row is a stacked
  `kron4 s a`; a returned solution un-vecs to a `MuellerMatrix` via
  `muellerOfVecColumnMajor`. No new 4×4 algebra was introduced.

## Deferred

- The real solver — `createMathNetSvd ()` over the vendored MathNet.Numerics SVD
  least-squares, reporting numerical rank + intensity RMSE and mapping a
  singular/rank-deficient design to a `SolverError` — is the paired
  `IMPLEMENT_CONTRACT SVC_XDUO_0002` (slice 004), with a real (non-mock) fact
  solving a small known full-rank system.
- The ALGLIB alternative (`rmatrixsolvels` behind the same proxy, living only in
  `OpticalConstructor.Optimization/AlglibAdapter.fs`) is a documented option for
  slice 004, explicitly NOT required.
- Assembling the full design matrix from many measurements and driving it through
  this proxy to reconstruct `M` remains a later slice.

## Gotchas

- **System-prompt path drift.** The task file's "System prompt" line points at
  `C:\GitHub\AI-Strategy-Generator\add_contract_worker.system-md`, which does not
  exist; the real file is
  `.../src/ai_strategy_generator/multistep/add_contract_worker.system-md` (a thin
  `ADD_CONTRACT` delta over `.../multistep/arc-runner.system-md`). Read those; do
  not block on the literal task-file path.
- **Class-type binding order (FS0960).** The new `let`s (`cannedSolution`,
  `makeMockSolver`) sit with the existing top-of-type `let`s, ahead of every
  `[<Fact>]` member — the exact trap that failed slice-002 attempt-01. Any future
  fixture added to this class must go at the top, never inline among the members.
- **Mock semantics are a recorded choice.** The slice fixes the mock only loosely
  ("a stub returning a canned `LeastSquaresSolution` ... plus a typed
  `SolverError`"). I keyed the stub on the design HEIGHT: `[||]` →
  `SingularDesign`; `< 16` rows → `RankDeficient rows`; else `Ok cannedSolution`.
  16 is the Mueller-vec unknown count, so "fewer rows than unknowns ⇒
  rank-deficient" is physically sensible and lets one fact exercise the success
  path and both error cases through the exact signature.
- **Unused mock parameter.** The stub ignores its `rhs`; it is named `_rhs` so the
  zero-warnings rule is not tripped by an unused binding.
- **Line endings.** Both edited files are pure LF (verified with
  `od -An -tx1 | grep -c '^0d'` → 0). The `.manifest.state.json` CRLF warning in
  `git diff` is on the arc-runner's own state file, untouched by this worker.

## Changelog

- 2026-07-15 — slice 003 (ADD_CONTRACT SVC_XDUO_0002) attempt 01: declare the
  least-squares SOLVE seam in `MuellerReconstruction` (`SolverError`,
  `LeastSquaresSolution`, `[<ReferenceEquality>] MuellerSolverProxy`); add an
  inline mock and two BerremanTests facts (exact-signature Ok + both typed
  `SolverError` cases; proxy reference equality). Contract left at `declared`;
  the real `createMathNetSvd ()` is slice 004.
