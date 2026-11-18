# Step 0481 — impl-log

## Progress

- Read the task, both system prompts, the project prompt, operator rulings R0001–R0005 and the slice.
  - The rulings bind steps 0427, 0428 and 0436 and ask later steps to preserve their behavior.
  - This step edits one test file and none of their files.
- Read the product surface the facts call:
  - `PlannerInformation`: `over`, `rank`, `confounded`, `conditionOpt`, `scaledSigmasOpt`, `boundsOver`,
    `SingularDirection.text`, `ReducedChiSquared.noiseVariance` and `candidateRowsOver`;
  - `FitQuality.reportFrom` / `covarianceOf`;
  - `MuellerFitting.residualJacobianAt` and `MuellerFitData.report`;
  - `FitUncertainty.predictedRelativeErrors`;
  - `QuartzSample` and `AbsorbingBiaxialSample` (steps 473, 480);
  - `BiaxialSample.ensembleSeeds`.
- Before the edit, confirmed that the planner and the fit build the same covariance.
  - `reportFrom` takes `FitQuality.residualJacobian` of `fun (DesignVector v) -> residual (ScaledVector v)` at the
    solution. `residualJacobianAt` takes the identical closure at the identical point.
  - The planner's single candidate scales its rows by √(noise / noise) = 1.0, which is exact.
  - `scaledSigmasOpt` is `sqrt` of the diagonal of `covarianceOf rows (ReducedChiSquared noise)`, with noise equal to
    the report's reduced χ². That is the report's `standardErrors` arithmetic.
  - `boundsOver` multiplies by |scale|, and the fact divides by |truth|. `predictedRelativeErrors` computes
    `se * |scale| / |truth|` in the same order.
  - The agreement is therefore expected to be bitwise. The fact holds it to the slice's 1e-12 relative.
- Edited `Berreman/BerremanTests/MeasurementPlannerTests.fs` (the step's one authorized edit):
  - **Opens added where the compiler needs them:**
    - `OpticalConstructor.Domain.MuellerReconstruction` (`MatrixRank`);
    - `OpticalConstructor.Optimization` (`FitQuality.FitReport`);
    - `.OptimizationInterface` (`DesignVector`);
    - `.MuellerFitting` (`residualJacobianAt`, `RelativeError`);
    - `.FitUncertainty` (`predictedRelativeErrors`);
    - `.BenchNoise` (`NoiseParam`);
    - `.RecoveryEnsembles` (`NoisyExperiment`);
    - `.PlannerInformation`;
    - `BerremanTests.InverseFitHarness` (`expectOk`).
  - **Class doc widened** to S14.2: the layer-2 information reproduces the designs' rank and covariance facts.
  - **Three class-local helpers:**
    - `informationOver axes candidate`: `PlannerInformation.over` of the axes' names and one candidate; a refusal
      fails the fact with `PlannerError.describe`.
    - `namesCarriedBy coordinates combination`: the coordinates whose name is a token of the combination's text, in
      coordinate order.
    - `assertBoundsArePredicted truth first`: the planner's bounds at the noisy fit's own Jacobian and
      reduced χ², checked against `predictedRelativeErrors`. Facts 2 and 3 share it, generic over `'Parameters`.
      - It reads `quality = first.fit.report` once.
      - The candidate is `{ jacobian = residualJacobianAt first.fit.residual first.fit.solution.solution; noise =
        quality.reducedChiSquared.noiseVariance }`.
      - It asserts the bound names equal the prediction names in order.
      - Each bound's σ / |axis.read truth| must equal its prediction within 1e-12 relative. A failure names the
        constant, the planner's bound and the prediction.
  - **The three facts, in the slice's words:**
    1. `the planner finds the full C1-C4 set full rank with every quartz constant bounded, and C1 alone blind to n_e
       and g33`.
       - Candidate: `candidateRowsOver QuartzSample.forward (QuartzSample.quartzScalingAround QuartzSample.quartz)
         QuartzSample.quartz NoiseParam.decentLab design |> expectOk`, over `UniaxialParameters.axes`.
       - Full set: rank 4, no confounded combination, a finite `conditionOpt`, four finite positive scaled sigmas, and
         `boundsOver` answering n_o, n_e, g11 and g33 each with a finite positive σ.
       - C1: rank 2 and two confounded texts, whose carried names are exactly `[ n_e; g33 ]`.
    2. `the planner's bounds for the noisy quartz design are what one noisy fit's own covariance predicts`:
       `QuartzSample.recoverQuartzFromNoisy NoiseParam.decentLab (List.head BiaxialSample.ensembleSeeds) |> expectOk
       |> assertBoundsArePredicted QuartzSample.quartz`.
    3. `the planner's bounds for AT-2's noisy design are what one noisy fit's own covariance predicts`:
       `AbsorbingBiaxialSample.recoverAbsorbingFromNoisy NoiseParam.decentLab (List.head
       AbsorbingBiaxialSample.noisySeeds) |> expectOk |> assertBoundsArePredicted AbsorbingBiaxialSample.absorbing`.
  - Each design is named by its module, as step 475 does.
- Build: green in 86 s, exit 0. No case-sensitive `error` in the log. The only warnings are the exempt `NU1902`
  (12) and the vendored MathNet's pre-existing `SYSLIB0051` (4).
- LF check: `git diff --numstat` and `--ignore-cr-at-eol` agree (125 / 2), and the file holds 0 CR bytes.
- The 21 section invariants were decoded from `.slices/0481.gates` and run. Each `git grep` finds nothing.

## Files modified

- `Berreman/BerremanTests/MeasurementPlannerTests.fs`: opens, the class doc, three helpers and three facts.

## Testing state

These are development runs. The arc-runner's gate engine is the authority. No run exceeded its effective deadline.

- **build**: exit 0 in 86 s (deadline 600 s).
- **unit-tests**: 7 of 7 `Passed!` in 3 min 36 s (deadline 7200 s). The gate's own selector was used,
  `--filter-method "BerremanTests.MeasurementPlannerTests.*" --filter-method "BerremanTests.MeasurementPlannerTests"`,
  so only the step's selected class ran.
  - The three new identities pass under their exact census names:
    - full C1–C4 rank / C1 blind: 4.6 s;
    - quartz noisy fit: 36.7 s;
    - AT-2 noisy fit: 172.7 s.
  - The four step-475 facts pass unchanged.
  - The count rose from 4 to 7, which is +3 as the acceptance asks.
- **elevation**: `ELEV-GATE: OK - 0 new finding(s) in 13 project(s), 227s`. 0 known findings, 1770 suppressed
  (unchanged). It ran with no comparison base, so all 13 projects were analyzed (deadline 600 s).
- **invariant-s46-\*** (21): each decoded `git grep` finds nothing.

## Artifacts

- `specs/0050/.artifacts/0481-dev-build.log`
- `specs/0050/.artifacts/0481-dev-unit-tests.log`
- `specs/0050/.artifacts/0481-dev-unit-tests.trx`
- `specs/0050/.artifacts/0481-dev-elevation.log`

## Gotchas

- **C1's numerical rank is 2, not the 1 that layer 1's structural verdict might suggest.**
  - Layer 1 (step 475) calls n_o unrecoverable from C1 (`NoRetardanceReason`).
  - The planner's rows are the throughput residual's (15 normalized entries plus ln m₀₀). There n_o enters m₀₀ through
    the surface reflectance as well as through the weak multiple-reflection channel `MuellerInverseTests` measures.
    It therefore clears the 1e-6 rank floor beside g11.
  - The two layers answer different questions, and both facts hold.
- **The confounded texts are read by token.**
  - The SVD may return any orthonormal basis of the null space span{n_e, g33}: two pure directions, or two mixed ones
    such as `+0.80 n_e +0.60 g33`.
  - So the fact asserts the union of the names the two texts carry, not each text verbatim.
  - `CombinationText`'s 0.1×-largest filter drops the round-off entries of n_o and g11.
- **Opening `OpticalConstructor.Optimization.MuellerFitting` shadowed nothing the existing facts use.** The build
  is clean, and the four step-475 facts pass unchanged.
