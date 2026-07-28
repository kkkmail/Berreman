# Impl-plan — spec 0042, slice 008 (IMPLEMENT — Stage-1 AIR calibration)

## Goal

Port Stage 1 of the reference `matrix_step1_air_fit.py` into
`OpticalConstructor.Domain.MuellerReconstruction` as three tiny pure functions,
plus two always-run/skippable facts in `BerremanTests`.

## Approach

Add to `MuellerReconstruction.fs` (a slice-008 section after `designRow`):

- `type CosineFit = { zeroDeg : float; visibility : float; rmse : float }` — the
  2-column cosine-fit readout (shape fixed by the slice; bare floats are the
  slice's explicit contract for the fit scalars).
- `type SourceCplModel = { thetaRel : Angle; retardance : Retardance }` — the
  re-derived CPL source constants (both elevated).
- `freeCosineFit (solver : MuellerSolverProxy) (anglesDeg : float[]) (norm : float[])
  : Result<CosineFit, SolverError>` — 2-column least squares `norm − 1 ≈ p·cos2α +
  q·sin2α` **through the proxy**; `visibility = √(p²+q²)`,
  `zeroDeg = wrap180(½·atan2(q,p)°)`, `rmse` = the proxy's residual RMSE (which
  equals numpy's `sqrt(mean(resid²))` because `b = norm−1` and `A·x = fitted−1`).
- `retardanceFromVisibility (visibility : float) : Retardance` = `arccos(|vis|)`
  (clamped to [0,1]), radians into `Retardance`.
- `sourceCplFromLpFrame (qLp : float) (uLp : float) : SourceCplModel` — the
  closed-form `derive_source_cpl_from_lp_frame` (`2θ = atan2(1−qLp, uLp)`,
  `θ = wrap180(½·2θ)`, `cosδ = 1 − (1−qLp)/sin²2θ`, `δ = arccos(clamp cosδ)`).
- private `wrapDeg180` = positive `deg % 180` (numpy `angle % 180.0` semantics).

Tests in `MuellerReconstructionTests.fs`:

- Always-run `[<Fact>]`: a synthetic cosine (`norm = 1 + v·cos2(a−z)`) is
  recovered by `freeCosineFit` (zeroDeg, visibility, ~0 rmse) within 1e-6.
- Data-dependent skippable fact: locate a sibling `optics-mueller` checkout;
  reduce the AIR#0 traces (dark-subtract + per-experiment normalize, ported from
  `prepare_normalized_traces`), then check z_LP≈155.44, z_CPL≈97.00, δ_an≈83.67,
  θ_src≈39.31, δ_src≈82.55 each within ~1°. Skip via `Assert.Skip` when absent.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/MuellerReconstruction.fs` (append).
- `Berreman/BerremanTests/MuellerReconstructionTests.fs` (append let-helpers + 2 facts).

## Risks / decisions

- **`[<SkippableFact>]` vs xunit.v3.** The slice names `[<SkippableFact>]`, but the
  referenced `Xunit.SkippableFact 1.5.61` depends on `xunit.extensibility.execution
  2.4.1` (xunit **v2**); this project's runner is **xunit.v3**, which does not
  discover v2 `FactAttribute`-derived test cases. Use xunit.v3's native dynamic
  skip (`Assert.Skip`) on a `[<Fact>]` for identical skip-when-absent semantics.
  Recorded in Gotchas.
- **`freeCosineFit` signature.** The slice names `(anglesDeg) (norm) : CosineFit`
  but also "through the MuellerSolverProxy"; the proxy is `Result`-returning and
  CLAUDE.md forbids throwing across a public boundary, so the faithful shape is
  `MuellerSolverProxy -> float[] -> float[] -> Result<CosineFit, SolverError>`.
- **Ground truth confirmed** by running the reference pipeline: z_LP 155.4401,
  z_CPL 96.9961, δ_an 83.6680, θ_src 39.3110, δ_src 82.5534 — 1° band is ample.
