# Step 0480 — impl-plan

## Approach

Step 481's acceptance facts run one noisy fit of each existing design, so this step moves the noisy-experiment runners
out of the two test classes into `BerremanTestSupport`. Every change is a move: afterwards each moved name is declared
exactly once, and every number is unchanged.

1. `Berreman/BerremanTestSupport/QuartzSample.fs`, after `fullConfigurations`:
   - `solverParameters`, `forward`, `quartzScalingAround` (renamed from the class's `scalingAround`), `blindStart`,
     `wideBox`;
   - `noisyQuartzObservations`: the class's `noisyObserve` without its `expectOk`;
   - `recoverQuartzFromNoisy`: `noisyQuartzObservations` → `fitObservations forward wideBox … (quartzScalingAround
     blindStart)` → `{ seed; fit }`, answering `Result`.
   - Opens added only where the compiler needs them: `Berreman.Solvers` (`SolverParameters`), `BenchNoise`,
     `RecoveryEnsembles`.
2. `Berreman/BerremanTestSupport/AbsorbingBiaxialSample.fs`, after `absorbingBandBreaches`:
   - `absorbingForward`, the class's `forward` over `BiaxialSample.solverParameters`;
   - `recoverAbsorbingFromNoisy`, the class's `recoverFromNoisy` answering `Result`.
3. `MuellerInverseTests.fs` deletes the seven names and reads them from `QuartzSample`, which is already opened last:
   - `scalingAround` uses become `quartzScalingAround`;
   - `noisyObserve` uses become `noisyQuartzObservations … |> expectOk`;
   - `recoverFromNoisy` uses become `recoverQuartzFromNoisy … |> expectOk`.
4. `AbsorbingBiaxialNoisyTests.fs` deletes `forward` and `recoverFromNoisy`, and maps `noisySeeds` through
   `recoverAbsorbingFromNoisy NoiseParam.decentLab >> expectOk`.

Each moved doc is cut to its contract in one to three lines and cites spec 0050 S14.2.

## Bit-identical numbers

- The class's `recoverFromNoisy` rebuilt `{ recovered = ofScaled scaling solution.solution; solution; scaling; residual }`
  from the `MuellerFitData` that `fitObservations` returned. `fitWithResidual` builds exactly that record, so passing
  the record through is the same value.
- The absorbing runner already passed the record through.
- The solver parameters, the scaling, the box, the start, the seed and the configurations are all unchanged.

## Risks

- **Shadowing.** `QuartzSample` is opened last in `MuellerInverseTests`, so the moved `forward` / `solverParameters`
  win over any earlier open exactly as the class lets did. `scalingAround` would now resolve to
  `MuellerFitting.scalingAround`, so every use is renamed (the compiler catches a miss: the signatures differ).
- **Name collisions.** `QuartzSample.solverParameters` is a namesake of `BiaxialSample.solverParameters`. No file opens
  both modules. `BerremanTestSupport` is outside the ELEV `inScopePatterns`, the same position step 0473 recorded.
- **Not touched.** `KitAcceptanceTests`' private `absorbingForward` reads the modules qualified, so nothing shadows it.

## Files

- `Berreman/BerremanTestSupport/QuartzSample.fs`
- `Berreman/BerremanTestSupport/AbsorbingBiaxialSample.fs`
- `Berreman/BerremanTests/MuellerInverseTests.fs`
- `Berreman/BerremanTests/AbsorbingBiaxialNoisyTests.fs`
