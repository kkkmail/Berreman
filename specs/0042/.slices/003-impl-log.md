# Impl-log — spec 0042, slice 003 (ADD_CONTRACT SVC_XDUO_0002 MuellerSolverProxy)

## Progress

- [x] Read system prompt (`add_contract_worker.system-md` + shared base), project
      prompt, slice spec, and the reference conventions
      (`ExperimentDataProxy.fs`, `ExperimentDataProxyTests.fs`) and the current
      `MuellerReconstruction.fs` / `MuellerReconstructionTests.fs`.
- [x] Confirm no symbol collision (grep) and that `SVC_XDUO_0002` is registered.
- [x] Wrote impl-plan.
- [x] Declare the seam in `MuellerReconstruction.fs` (`SolverError`,
      `LeastSquaresSolution`, `[<ReferenceEquality>] MuellerSolverProxy`).
- [x] Add mock + facts to `BerremanTests/MuellerReconstructionTests.fs`.
- [x] Verify LF line endings.
- [x] Wrote state-of-the-world.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/MuellerReconstruction.fs`
  — appended the DECLARED least-squares SOLVE seam: `SolverError`,
  `LeastSquaresSolution`, `[<ReferenceEquality>] MuellerSolverProxy`.
- `Berreman/BerremanTests/MuellerReconstructionTests.fs` — added the inline mock
  (`cannedSolution`, `makeMockSolver`) and two `[<Fact>]`s (exact-signature Ok +
  both typed `SolverError` cases; reference-equality of the proxy).

## Testing state

Per **Invariant 6 (the worker acts; it runs no checks)** — the `ADD_CONTRACT`
delta and shared base — this worker did NOT execute the `build` / `unit-tests` /
`constructor-unit-tests` gates. Gate execution belongs to the arc-runner's
deterministic gate engine, which runs them after I exit. The new mock-driven
facts satisfy the acceptance: a stub `MuellerSolverProxy` is built and
`solveLinearLeastSquares` is called through its exact signature, returning both a
canned `LeastSquaresSolution` and typed `SolverError`s.

`commit_ready: true` — every requirement in the slice spec is addressed this
round (seam declared at `declared` lifecycle, mock + test present); nothing
deferred to a "round 2".

## Artifacts

None — a contract-declaration slice produces no captured logs/traces. (The
per-arc artifacts folder is `specs/0042/.artifacts/`.)

## Gotchas

- **System-prompt path drift.** The task file's "System prompt" line points at
  `C:\GitHub\AI-Strategy-Generator\add_contract_worker.system-md`, which does not
  exist; the real file is
  `.../src/ai_strategy_generator/multistep/add_contract_worker.system-md` (a thin
  `ADD_CONTRACT` delta over `.../multistep/arc-runner.system-md`). Read those; do
  not block on the literal task-file path.
- **Class-type binding order (FS0960).** The new `let`s (`cannedSolution`,
  `makeMockSolver`) sit with the existing top-of-type `let`s, ahead of every
  `[<Fact>]` member — the exact trap that failed slice-002 attempt-01.
- **`[<ReferenceEquality>]` is load-bearing.** `MuellerSolverProxy` has a
  function-valued field, so it has no structural equality; the attribute makes it
  compare by identity (required to live in an Optimization/Elmish context), per
  the `ExperimentDataProxy` convention.
- **Design-height mock rule (recorded choice).** The slice fixes the mock's
  return semantics only loosely ("a stub returning a canned
  `LeastSquaresSolution` ... plus a typed `SolverError`"). I keyed the stub on the
  design height: `[||]` → `SingularDesign`; `< 16` rows → `RankDeficient rows`;
  otherwise `Ok cannedSolution`. 16 is the Mueller-vec unknown count, so
  "fewer rows than unknowns ⇒ rank-deficient" is physically sensible and lets one
  fact exercise the success path and BOTH error cases through the exact signature.
