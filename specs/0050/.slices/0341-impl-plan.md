# Step 0341 — impl plan

## Approach

This step moves the ensemble statistics from `BerremanTests/InverseFitHarness.fs` into a new product module,
`OpticalConstructor.Optimization.RecoveryEnsembles`. It is a move under S101-ENSEMBLE-MOVE: the new home is created
and the old declarations are deleted in the same step.

1. **New file `RecoveryEnsembles.fs`.** It holds `module RecoveryEnsembles` in `namespace OpticalConstructor.Optimization`.
   It compiles directly after `BenchNoise.fs`, added with one `<Compile>` line and no comment. It declares:
   - `RecoveredQuantity<'Parameters>`: `name : ParameterName` and `valueIn : 'Parameters -> QuantityValue`, with
     `ofAxis` and `derived`.
   - `RecoveryStatistics<'Parameters>`: `mean` and `absoluteScatter` become `QuantityValue`. `describe` renders the
     same text from their values.
   - `NoisyExperiment<'Parameters>`.
   - `EnsembleSize`.
   - `statisticsFor`: the same arithmetic on the same doubles in the same order. `mean` and `absoluteScatter` are
     born as `QuantityValue`s.
   - `standardErrorOfMean`, taking an `EnsembleSize`.
   - `rowFor`: answers a `Result`, with `UnknownQuantityErr` for an unknown name.
2. **`MuellerFitError` gains `UnknownQuantityErr of ParameterName`.** No product code matches on the union, so the
   new case cannot introduce an FS0025 incomplete-match error.
3. **`InverseFitHarness.fs`** deletes the three types and the three functions.
4. **The test callers** change only as the step names:
   - `BiaxialSample`, `MuellerInverseTests` and `AbsorbingBiaxialNoisyTests` build quantities with
     `ParameterName` and `QuantityValue`.
   - `rowFor ... |> expectOk` in place of the old call.
   - `.absoluteScatter.value` wherever an absolute scatter is compared as a number.
   - `EnsembleSize (List.length ...)` as the ensemble size.
   - Each file adds `open OpticalConstructor.Optimization.RecoveryEnsembles`, plus `ItemModels` and `MuellerFitting`
     where `BiaxialSample` needs them.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Optimization/OpticalConstructor.Optimization.fsproj`
- `Berreman/OpticalConstructor/OpticalConstructor.Optimization/RecoveryEnsembles.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Optimization/MuellerFitting.fs`
- `Berreman/BerremanTests/InverseFitHarness.fs`
- `Berreman/BerremanTests/BiaxialSample.fs`
- `Berreman/BerremanTests/MuellerInverseTests.fs`
- `Berreman/BerremanTests/BiaxialInverseTests.fs`
- `Berreman/BerremanTests/AbsorbingBiaxialNoisyTests.fs`

## Risks

- **Bit-identical numbers.** `statisticsFor` must keep the same operations on the same doubles:
  - `List.average` over the unwrapped values;
  - the same `(v - mean) ** 2.0` sum divided by `count - 1`;
  - `sqrt variance` computed once;
  - `RelativeError.between` and `RelativeErr`, as before.
- **ELEV on the new product module.** Every record field is elevated, and no call-site rewrap of a bare local is
  allowed: `mean` and `absoluteScatter` are born as `QuantityValue` where they are computed. The names are unique; a
  grep over Berreman and STL found no collision.
- **The step text says `|> RelativeError`.** The union's case is `RelativeErr`, so the code uses the case
  constructor. This is recorded in the impl-log.
