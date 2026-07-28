# State of the world — spec 0042, slice 004 (IMPLEMENT_CONTRACT SVC_XDUO_0002 MuellerSolverProxy)

## Where we are

Slice 004 is the fourth slice of arc 0042 and the SECOND of the `MuellerSolverProxy`
contract pair: the `IMPLEMENT_CONTRACT` step that supplies the REAL construction
behind the least-squares SOLVE seam slice 003 *declared*. It builds
`createMathNetSvd ()` — the working proxy backed by the vendored MathNet.Numerics
SVD least-squares — directly on the pure column-major core from slice 002 (`kron4`
builds one design row; `muellerOfVecColumnMajor` un-vecs a returned solution). No
seam type changed; the declared surface is now genuinely implemented, and the
contract's lifecycle flips `declared → implemented` (`implementStep = 4`).

## What's working

- Implement the real `createMathNetSvd () : MuellerSolverProxy` in
  `MuellerReconstruction` (Domain) over the vendored MathNet.Numerics SVD.
- Solve the over-determined design `A·x ≈ b` via `matrix.Svd(true).Solve(b)`,
  reporting the numerical `rank` and the intensity residual `rmse = ‖A·x − b‖ / √m`.
- Map a singular / rank-deficient design to a typed `SolverError` under `try/with`
  — `RankDeficient rank` below full column rank, `SingularDesign reason` for a
  degenerate shape or any MathNet exception — never a throw across the proxy.
- Add a real (non-mock) BerremanTests fact solving a known over-determined
  full-rank system, plus a typed-error-mapping fact (empty / rank-deficient).
- Flip `SVC_XDUO_0002` to `implemented` in the arc's `.contracts-json` registry.

## Tests

Gate roster for this slice (from `004.gates`): `build`, `unit-tests`
(`berreman_unit_tests`), `constructor-unit-tests` (`constructor_unit_tests`).

Per the `IMPLEMENT_CONTRACT` worker's **Invariant 6 (the worker acts; it runs no
checks)**, this worker did NOT execute the gates, nor the arc-runner's
contract-implementation analyzer — the deterministic gate engine runs them after
exit. New coverage in `BerremanTests/MuellerReconstructionTests.fs`, both driving
the REAL `createMathNetSvd ()` (not the mock):

- **full-rank known solve** (the acceptance) — a 4×2, full-column-rank, consistent
  system `A·x = b` with a known `x = [3; -2]`; asserts the returned solution
  matches `x` within the shared `allowedDiff`, `solution.Length = 2`,
  `rank = 2` (equal to the column count), and `rmse ≈ 0`;
- **typed-error mapping** — an empty design returns `Error (SingularDesign _)`
  with a non-empty reason; a dependent-column design (`c₂ = 2·c₁`) returns
  `Error (RankDeficient rank)` with `rank < 2` — every branch returns a value,
  never a throw.

Float readouts (`rmse`, solution elements) are compared with the shared
`allowedDiff` tolerance (`MatrixComparison.fs`), not a hand-rolled epsilon.

```yaml
gates:
  berreman_unit_tests: 0
  constructor_unit_tests: 0
```

(The gate engine records the real captured counts post-exit; the baseline for a
`count_at_least` gate is the prior green step's checkpoint, not a value the worker
authors — 0 is the safe floor here. The two new facts only add to the count.)

## Architecture

- **The declared seam is now genuinely implemented — no type changed.**
  `createMathNetSvd ()` returns the same `[<ReferenceEquality>] MuellerSolverProxy`
  the slice-003 `ADD_CONTRACT` declared; every consumer that holds the proxy is
  untouched. The mock stays for the pure signature/error-shape tests; the real
  factory is the production backend.
- **MathNet is reached transitively, no new `open` in the Domain.** The design and
  rhs are built through the EXISTING `Berreman.MathNetNumericsMath`
  `RealMatrix.create` / `RealVector.create` wrappers (exactly as `Propagation.fs:4`
  opens the module), then the single-case DUs are unwrapped to their backing
  `Matrix<double>` / `Vector<double>` ONLY for the `.Svd(true)` / `.Solve(b)`
  members the DU does not surface. No `MathNet.Numerics` type name is written in
  the Domain, honouring the project's layering.
- **The residual runs through the DU algebra.** `rmse` is
  `(am * RealVector x - bv).norm / sqrt (float rows)` using the `RealMatrix` ×
  `RealVector` and `RealVector` − `RealVector` operators plus `RealVector.norm` —
  no raw MathNet arithmetic method calls.
- **Rank vs column count is the trust check.** `Svd.Rank` (the count of
  non-negligible singular values) is compared against the column count; a design
  below full column rank is `RankDeficient` — the reconstruction is not uniquely
  determined. An under-determined design lands here naturally (`rank ≤ rows <
  columns`), so "fewer measurements than unknowns" is a typed error, not a throw.
- **The proxy boundary is total.** The `.Svd()` / `.Solve()` block is wrapped in
  `try/with`; any MathNet exception is mapped to `SingularDesign reason` — the
  CLAUDE.md rule that native/library exceptions are caught AT the boundary and
  mapped to a typed error, never handled above the seam.

## Deferred

- The ALGLIB alternative (`rmatrixsolvels` behind the same proxy, living only in
  `OpticalConstructor.Optimization/AlglibAdapter.fs:20`) is a documented option,
  explicitly NOT required for §7.2 — the vendored MathNet SVD is the sole required
  backend. No ALGLIB reference is added to the Domain.
- Assembling the full design matrix from many `(input Stokes, analyzer row)`
  measurements and driving it through this proxy to reconstruct a `MuellerMatrix`
  (then reading the `frobeniusDiff` residual against a reference) remains a later
  slice — this slice delivers only the solve backend.

## Gotchas

- **System-prompt path drift.** The task file's "System prompt" line points at
  `C:\GitHub\AI-Strategy-Generator\implement_contract_worker.system-md`, which does
  not exist; the real file is
  `.../src/ai_strategy_generator/multistep/implement_contract_worker.system-md`
  (a thin `IMPLEMENT_CONTRACT` delta over `.../multistep/arc-runner.system-md`).
  Read those; do not block on the literal task-file path. (Same drift the 003
  worker recorded for `add_contract_worker.system-md`.)
- **MathNet without a new `open`.** Do NOT add `open MathNet.Numerics` to the
  Domain; unwrap the `RealMatrix` / `RealVector` DUs and call `.Svd()` / `.Solve()`
  on the already-typed backing values. The vendored `Double/Factorization/DenseSvd.cs`
  `Solve` handles the rectangular over-determined case, and `Svd.Rank` counts
  non-negligible singular values — verified against the vendored source.
- **Registry edit is the worker's job here (recorded choice).** The
  `IMPLEMENT_CONTRACT` delta instructs the worker to flip the lifecycle to
  `implemented`. The arc-runner runtime *reads* but never *writes* `.contracts-json`
  (only the spec-writer's `write_lifecycle` and workers do), so the worker owns the
  flip. `SVC_XDUO_0002` is set to `lifecycle = "implemented"`, `implementStep = 4`,
  preserving the runner's exact `atomic_write_json` byte format (`indent=2`, CRLF,
  trailing newline) to avoid churn. The forward-only-closure /
  registry-consistency checks only require the id be *present*, but the explicit
  delta obligation is honoured.
- **`.contracts-json` is CRLF on purpose.** It is an arc-runner-managed state file
  (like `.manifest.state.json`), not a source file — so it stays CRLF to match the
  runner's serializer, exempt from the LF `.gitattributes` rule that governs code.
  Both edited `.fs` files are pure LF (verified with `od -An -tx1 | grep '0d'` → 0).
- **No FS0960 in the test.** The two new facts are members only — no class-level
  `let` binding was added, so the top-of-type binding-order trap that hit slice-002
  attempt-01 does not recur. The `let (RealMatrix a) = …` in the Domain is a
  complete single-case-DU pattern (no FS0025), the same idiom the module uses.

## Changelog

- 2026-07-15 — slice 004 (IMPLEMENT_CONTRACT SVC_XDUO_0002) attempt 01: implement
  the real `createMathNetSvd () : MuellerSolverProxy` over the vendored
  MathNet.Numerics SVD least-squares (numerical rank + intensity RMSE; singular /
  rank-deficient → typed `SolverError` under `try/with`, never a throw); add a
  real (non-mock) acceptance fact solving a known over-determined full-rank system
  plus a typed-error-mapping fact; flip `SVC_XDUO_0002` → `implemented` in
  `.contracts-json`. ALGLIB (`rmatrixsolvels`) documented as NOT required for §7.2.
