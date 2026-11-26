# Step 0481 — impl-plan

## Approach

Three facts join `BerremanTests.MeasurementPlannerTests` (`Berreman/BerremanTests/MeasurementPlannerTests.fs`, the
only authorized edit). They check the Planner's numerical layer (`PlannerInformation`, steps 477–479) against the
facts spec 0044 already measures over its own designs (001 §M; no new inverse physics, 001 §W).

1. **Full rank over C1–C4; C1 blind to n_e and g33.**
   - Over the names of `UniaxialParameters.axes`, take `PlannerInformation.over` of one candidate:
     `candidateRowsOver QuartzSample.forward (QuartzSample.quartzScalingAround QuartzSample.quartz) QuartzSample.quartz
     NoiseParam.decentLab design |> expectOk`.
   - Over `QuartzSample.fullConfigurations`, assert:
     - rank 4 and no confounded combination;
     - `conditionOpt` is `Some` of a finite number;
     - `scaledSigmasOpt` is `Some` of four finite positive entries;
     - `boundsOver` the scaling answers one finite positive bound per constant, in axis order.
   - Over `QuartzSample.c1`, assert rank 2. The names carried by the texts of the two confounded directions must be
     exactly n_e and g33.
2. **Quartz noisy fit.**
   - `first = QuartzSample.recoverQuartzFromNoisy NoiseParam.decentLab (List.head BiaxialSample.ensembleSeeds) |>
     expectOk`, `quality = first.fit.report` and `predicted = predictedRelativeErrors QuartzSample.quartz first.fit
     quality`.
   - The planner reads one candidate `{ jacobian = residualJacobianAt first.fit.residual first.fit.solution.solution;
     noise = quality.reducedChiSquared.noiseVariance }` over the fit's axis names.
   - Each bound from `boundsOver first.fit.scaling`, divided by the magnitude of its axis's value in the truth, must
     equal its `predicted` entry within 1e-12 relative.
3. **AT-2 noisy fit.** The same as fact 2, over `AbsorbingBiaxialSample.recoverAbsorbingFromNoisy NoiseParam.decentLab
   (List.head AbsorbingBiaxialSample.noisySeeds)` against `AbsorbingBiaxialSample.absorbing`.

Facts 2 and 3 share one generic class-local helper. Each failure names the constant, the planner's bound and the
prediction. Each design is named by its module, as step 475 does.

## Files

- `Berreman/BerremanTests/MeasurementPlannerTests.fs`: opens, helpers, three facts, and the class doc widened to S14.2.

No production file changes. The facts call existing product code only.

## Risks

- **C1's rank.** The facts expect rank 2 under the rank floor (1e-6 of the largest singular value).
  - With the throughput residual, n_o enters through m00 as well as the weak multiple-reflection channel.
  - n_e and g33 columns are identically zero, so their null directions span exactly {n_e, g33}.
  - The 0.1×-largest text filter drops round-off entries.
- **Exact agreement.** The planner rebuilds the fit's own covariance:
  - the Jacobian at the same point by the same `FitQuality.residualJacobian`;
  - the noise is the reduced χ², and the row factor is √1 = 1.

  Agreement should be bitwise. 1e-12 relative is the spec's tolerance.
- **Runtime.** One transparent noisy fit (~45 s) and one absorbing fit (~190 s) are added to the class.
