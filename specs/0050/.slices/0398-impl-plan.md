# Step 0398 — impl plan

## Approach

Stage one fits each AIR trace with two cosine coefficients: `freeCosineFit` for p and q, and the LP-frame fit for
q_LP and u_LP, which `sourceCplFromLpFrame` reads back. This step writes the same two models in the instruments' own
constants, so that a later step can hand them to `FitQuality` for σ, correlation and profile. It changes three things
in `OpticalConstructor.Optimization/StagedMuellerPath.fs` and nothing else:

1. `reduceAirTraces` loses `private`. Its body is unchanged, and its doc is rewritten to cite spec 0050 S11.4.
2. After it comes `type AirTrace with member this.fringeResidualAt (zero : Angle) (visibility : FringeVisibility) :
   ResidualVector`. Entry i is V · cos 2(αᵢ − z) − (normᵢ − 1), with 2(αᵢ − z) formed in degrees the way
   `lpFrameSourceModel` forms it.
3. Then `type SourceCplModel with member this.lpFrameResidualOn (trace : AirTrace) (lpZero : Angle) : ResidualVector`.
   Its coefficients are 1 − q = (1 − cos δ) · sin² 2θ and u = (1 − cos δ) · sin 2θ · cos 2θ. Entry i is
   q · cos 2(αᵢ − z) + u · sin 2(αᵢ − z) − (normᵢ − 1). The doc names the physical branch, 0° < θ < 90°.

`ResidualVector` lives in `OpticalConstructor.Domain.MuellerInverse`, which this file then opens, as `FitQuality.fs`
does.

## Tests (`StagedMuellerPathTests`, three new facts with the exact authorized IDs)

- Test-local AIR traces at 0°, 20°, …, 160°, built by one helper.
- `an AIR trace's fringe residual at its cosine fit's zero and visibility is that fit's residual`:
  - The trace is 0.54 cos 2α − 0.62 sin 2α + 0.01 (−1)ⁱ, solved by the real SVD.
  - The test rebuilds the fit's design, with coefficients V cos 2z and V sin 2z, and computes its residuals through
    `LeastSquaresSystem.residualsAt`.
  - Each entry must match `fringeResidualAt` within 1e-12, and the RMS must match `rmse` within 1e-12.
- `an LP-frame residual vanishes on the trace of the source model sourceCplFromLpFrame reads`: the trace is 0.16 /
  0.17 phased at 155°. Every entry must be zero within 1e-12, and the model's θ must lie on the branch.
- `the AIR trace of two experiments' rows is each experiment's trace in turn`:
  - Two labelled experiments, each reading every dial twice and out of order.
  - `reduceAirTraces` of their concatenation must equal the two single traces appended, exactly.

## Risks

- Opening `MuellerInverse` could re-target record-label inference in the existing record expressions. The build would
  fail rather than drift silently. If it does, qualify `MuellerInverse.ResidualVector` instead.
- ELEV: there are no new primitives in signatures, and the members sit on our own types. All three names are unique
  across the repository (checked by grep).
- `MuellerReconstructionTests` must stay bit-identical. Nothing it calls changes.
