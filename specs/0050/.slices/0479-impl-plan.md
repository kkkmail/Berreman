# Step 0479 — impl plan

## Approach

Spec 0050 S14.2: a candidate's planner rows from its measurement configurations, through the study engine's forward
and throughput residual, with its noise from the detector's bench budget.

1. `BenchNoise.fs`, after `NoiseParam`:
   - `type ParameterValue with` `angleUncertainty` / `elementUncertainty` / `throughputUncertainty`, the named
     source-type conversions to the three promoted uncertainty quantities.
   - `type ParameterValues with member this.benchNoise : Result<NoiseParam, ItemEvaluationError>`. It reuses
     `ParameterValues.checkedFor` over the budget's own noise axes (`NoiseParam.parameterAxes`). That check covers
     missing, non-finite, and element/throughput outside [0, 1]. A negative rotation is then refused as
     `ParameterOutOfRangeErr`. Each value is converted through the three members, and unrelated entries are ignored.
2. `PlannerInformation.fs`, after step 478's declarations:
   - `type NoiseParam with member this.noiseVarianceOver forward nominal configurations`:
     - Take `observeWith` at the nominal point, an error passing through.
     - The angle sensitivity is the offset-frame two-column Jacobian at (0, 0) of a total closure. The closure observes
       through a forward turned by (Δi, Δa) (`misalignedBy`), filed against the nominal configurations. It then answers
       `throughputResidualFor` at `ScaledVector [||]` over `scalingAround [] nominal nominal`. A forward error gives the
       full-width 1.0e3 penalty.
     - Per-entry variance: u_e²/3 below 15·m, else u_t²/3, plus u_r²/3·(a_i0² + a_i1²). The answer is the mean.
   - `candidateRowsOver forward scaling nominal noise configurations`:
     - `jacobian` is `(toScaled scaling nominal).offsetJacobianOf (throughputResidualFor forward <observeWith …> scaling)`.
     - `noise` is `noise.noiseVarianceOver …`.
3. Two new facts in `PlannerInformationTests`, over test-local `ForwardModelProxy<UniaxialParameters>` forwards and two
   `configurationOf` configurations (X-cut, 1 mm, 10° / 20°, azimuth 0°, transmitted, 632.8 nm).

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Optimization/BenchNoise.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Optimization/PlannerInformation.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/PlannerInformationTests.fs`

## Risks

- **Finite-difference precision.** The offset frame steps 1.49e-6 scaled units, which is 1.49e-9 of an index near 1.5.
  The index's ulp (2.2e-16) bounds the derivative to 7.45e-8 relative, and 5.96e-8 is measured. The slice's
  "within 1e-9 relative" cannot hold there. The rows fact therefore uses 1e-7 relative for those entries, a recorded
  deviation. The variance fact's angle sensitivity resolves to ~2e-11 and keeps 1e-9.
- **The penalty constant.** It is not exported by `MuellerFitting.fs`, which is outside this step's edits. It is held as
  a private `[<Literal>]` citing `residualFor`.
- **Elevation.** The new members are on types (exempt from the primitive-signature rules). `candidateRowsOver` carries
  no primitive. No `to<Target>` names are used, so ELEV006 does not apply.
