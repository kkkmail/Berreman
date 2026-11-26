# Step 0339 — impl plan

## Approach

A MOVE of the bench noise model's transparent path (obligation S101-NOISE-MOVE) from
`Berreman/BerremanTests/InverseFitHarness.fs` into a new product module
`OpticalConstructor.Optimization.BenchNoise` (`BenchNoise.fs`, compiled directly after `MuellerFitting.fs`).

1. `BenchNoise.fs`, in this order: `AngleUncertainty`, `ElementUncertainty`, `ThroughputUncertainty`, `NoiseParam`
   (bodies and constants unchanged, plus each budget's `parameterValue` and `NoiseParam.parameterAxes` through
   `ItemParameterAxis.detectorNoiseAxes`), then `NoiseSeed`, the new `UniformDeviate`, `UniformDeviateProvider`
   (`nextDeviate : unit -> UniformDeviate`), `disturb`, `misread` (a `Result`) and `noisyObserveWith` (a `Result`,
   which stops at the first error). The draw order stays as it is.
2. `MuellerFitError` gains `MuellerNormalizationErr of InversionError`.
3. The harness deletes the banner, the four data types, `NoiseSeed`, `UniformDeviateProvider`, `disturb`, `misread`
   and `noisyObserveWith`, and opens `BenchNoise`. `misreadAbsolute` reads `(draws.nextDeviate ()).value`.
4. Callers:
   - `MuellerInverseTests` changes `firstTen`, the direct `misread`, `noisyObserve` and three comments.
   - `BiaxialInverseTests` changes `recoverFromNoisy` and two comments.
   - `AbsorbingBiaxialNoisyTests` adds an open.
5. A new constructor fact in `ItemModelsTests`: `the promoted bench budgets equal the detector catalogue defaults`.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Optimization/OpticalConstructor.Optimization.fsproj`
- `Berreman/OpticalConstructor/OpticalConstructor.Optimization/BenchNoise.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Optimization/MuellerFitting.fs`
- `Berreman/BerremanTests/InverseFitHarness.fs`
- `Berreman/BerremanTests/MuellerInverseTests.fs`
- `Berreman/BerremanTests/BiaxialInverseTests.fs`
- `Berreman/BerremanTests/AbsorbingBiaxialNoisyTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/ItemModelsTests.fs`

## Risks

- **Bit-identity of the seeded stream.** The draw order must stay two draws for `disturb`, then fifteen for `misread`,
  per configuration. A `Result` fold that evaluates configurations in order keeps it.
- **ELEV on product code that used to be test-only.** The likely rules are ELEV001, ELEV005, ELEV008 and ELEV016.
  Values are destructured by pattern rather than re-wrapped from `.value`.
- **Record-label inference.** Opening `BenchNoise` in the test files brings in the labels `rotation`, `element` and
  `throughput`. The compiler will flag any shadowing.
