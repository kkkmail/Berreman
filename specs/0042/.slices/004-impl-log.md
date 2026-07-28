# Impl-log — spec 0042, slice 004 (IMPLEMENT_CONTRACT SVC_XDUO_0002 MuellerSolverProxy)

## Progress

- [x] Read system prompt (`implement_contract_worker.system-md` + shared base
      `arc-runner.system-md`), project prompt (`arc-runner.user-md`), the slice
      spec (`004.slice-md`), and the reference conventions: the declared seam in
      `MuellerReconstruction.fs`, the mock in `MuellerReconstructionTests.fs`,
      how `Propagation.fs:4` reaches MathNet, the `MathNetNumericsMath`
      `RealMatrix` / `RealVector` wrappers, `AlglibAdapter.fs:20`, the shared
      `allowedDiff` tolerance, and the vendored MathNet SVD API
      (`Double/Factorization/DenseSvd.cs` — rectangular `Solve`, `Svd.Rank`).
- [x] Wrote impl-plan.
- [x] Implement `createMathNetSvd () : MuellerSolverProxy` in
      `MuellerReconstruction.fs` (real MathNet-SVD least-squares).
- [x] Add real-proxy acceptance fact(s) to `MuellerReconstructionTests.fs`.
- [x] Flip `SVC_XDUO_0002` lifecycle to `implemented` in `.contracts-json`.
- [x] Verify LF line endings on the two source files.
- [x] Wrote state-of-the-world.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/MuellerReconstruction.fs`
  — appended `createMathNetSvd () : MuellerSolverProxy`: the REAL least-squares
  solve behind the declared seam, backed by the vendored MathNet.Numerics SVD
  (`matrix.Svd(true).Solve(b)`), reporting numerical `rank` + intensity `rmse`
  and mapping a singular / rank-deficient design to a typed `SolverError` under
  `try/with` (never a throw). The ALGLIB alternative is documented as NOT
  required for §7.2.
- `Berreman/BerremanTests/MuellerReconstructionTests.fs` — added two `[<Fact>]`s
  driving the REAL `createMathNetSvd ()` (not the mock): a known over-determined
  full-rank solve (solution matches within `allowedDiff`, `rank = columnCount`,
  `rmse ≈ 0`), and the typed-error mapping (empty → `SingularDesign`;
  dependent-column → `RankDeficient`).
- `specs/0042/.contracts-json` — `SVC_XDUO_0002` lifecycle `declared` →
  `implemented`, `implementStep` `null` → `4`.

## Testing state

Per **Invariant 6 (the worker acts; it runs no checks)** — the
`IMPLEMENT_CONTRACT` delta and the shared base — this worker did NOT execute the
`build` / `unit-tests` / `constructor-unit-tests` gates, nor the arc-runner's
contract-implementation analyzer. Gate execution belongs to the arc-runner's
deterministic gate engine, which runs them after I exit. The code is written
correct-by-construction against the vendored MathNet SVD API.

The new acceptance fact satisfies the slice: it instantiates the REAL
`createMathNetSvd ()` proxy, solves a small known over-determined full-rank
system, and asserts the returned solution matches the known answer within the
shared tolerance with `rank` equal to the column count. A second fact pins the
typed-`SolverError` mapping (empty / rank-deficient designs return values, never
throw).

`commit_ready: true` — every requirement in the slice spec is addressed this
round (real `createMathNetSvd ()` implemented; real acceptance fact present;
contract flipped to `implemented`); nothing deferred to a "round 2".

## Artifacts

None — a numerical-implementation slice produces no captured logs / traces. (The
per-arc artifacts folder is `specs/0042/.artifacts/`.)

## Gotchas

- **System-prompt path drift.** The task file's "System prompt" line points at
  `C:\GitHub\AI-Strategy-Generator\implement_contract_worker.system-md`, which
  does not exist; the real file is
  `.../src/ai_strategy_generator/multistep/implement_contract_worker.system-md`
  (a thin `IMPLEMENT_CONTRACT` delta over `.../multistep/arc-runner.system-md`).
  Read those; do not block on the literal task-file path. (Same drift the 003
  worker recorded for `add_contract_worker.system-md`.)
- **MathNet reached without a new `open`.** The Domain must not open
  `MathNet.Numerics` directly. I build the design/rhs through the EXISTING
  `RealMatrix.create` / `RealVector.create` wrappers (MathNet reached
  transitively via `Berreman.MathNetNumericsMath`, exactly as `Propagation.fs:4`
  does), then unwrap the single-case DUs to their backing `Matrix<double>` /
  `Vector<double>` ONLY to call `.Svd(true)` / `.Solve(b)` (the two members the
  DU does not surface). The types are already known from the destructure, so no
  namespace open is needed and no MathNet type name is written in the Domain.
- **Residual through the DU operators.** `rmse = ‖A·x − b‖ / √m` is computed as
  `(am * RealVector x - bv).norm / sqrt (float rows)` using the `RealMatrix` ×
  `RealVector` and `RealVector` − `RealVector` operators plus `RealVector.norm`
  — no raw MathNet arithmetic method calls.
- **Rank vs column count is the observability check.** `Svd.Rank` counts
  non-negligible singular values; `rank < columnCount` ⇒ `RankDeficient rank`
  (the reconstruction is not uniquely determined). An under-determined design
  (fewer rows than unknowns) naturally lands here since `rank ≤ rows < columns`.
- **Boundary maps every throw.** The `.Svd()` / `.Solve()` block runs under
  `try/with`; any MathNet exception becomes `SingularDesign reason` — the typed
  channel — so the proxy never throws across its boundary (CLAUDE.md).
- **Registry edit (recorded choice).** The `IMPLEMENT_CONTRACT` delta instructs
  the worker to "update the contract's lifecycle in the registry to
  `implemented`". The arc-runner runtime reads but never writes `.contracts-json`
  (only the spec-writer's `write_lifecycle` and workers do), so the worker is the
  one that flips it. I set `lifecycle = "implemented"` and `implementStep = 4`
  for `SVC_XDUO_0002`, preserving the arc-runner's exact `atomic_write_json`
  byte format (`indent=2`, CRLF, trailing newline) so no spurious churn appears.
  The `constructor_unit_tests` / forward-only-closure checks only require the id
  be *present* in the registry, but the explicit delta obligation is honored.
- **Line endings.** Both edited `.fs` files are pure LF (verified with
  `od -An -tx1 | grep '^0d'`). `.contracts-json` is intentionally CRLF — it is an
  arc-runner-managed state file (like `.manifest.state.json`), matched to the
  runner's own serializer, not a source file under the LF `.gitattributes` rule.

## Changelog

- 2026-07-15 — slice 004 (IMPLEMENT_CONTRACT SVC_XDUO_0002) attempt 01: implement
  the real `createMathNetSvd () : MuellerSolverProxy` over the vendored
  MathNet.Numerics SVD least-squares (numerical rank + intensity RMSE; singular /
  rank-deficient → typed `SolverError` under `try/with`, never a throw); add a
  real (non-mock) acceptance fact solving a known over-determined full-rank
  system plus a typed-error-mapping fact; flip `SVC_XDUO_0002` → `implemented`
  in `.contracts-json`. ALGLIB (`rmatrixsolvels`) documented as NOT required.
