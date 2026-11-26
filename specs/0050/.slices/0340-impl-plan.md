# Step 0340 — impl plan

## Approach

A MOVE of the bench noise model's ABSORBING path (obligation S101-ABSOLUTE-NOISE-MOVE) from
`Berreman/BerremanTests/InverseFitHarness.fs` into `OpticalConstructor.Optimization.BenchNoise` (`BenchNoise.fs`),
where step 339 already put the transparent path.

1. `BenchNoise.fs`:
   - after `misread`: `misreadAbsolute`, answering `Result<MuellerMatrix, MuellerFitError>`. The throughput draw
     comes first, then the fifteen element draws in `misread`'s order; a normalization `Error e` becomes
     `Error (MuellerNormalizationErr e)`.
   - after `noisyObserveWith`: `noisyObserveAbsoluteWith`, the same ordered `Result` fold as `noisyObserveWith`
     with `misreadAbsolute`, answering `Result<MuellerObservation list, MuellerFitError>`.
   - Each doc is one to three lines; `misreadAbsolute` keeps the line that the throughput draw comes first so a
     seed reproduces the experiment (specs/0044/.manual/019).
2. `InverseFitHarness.fs` deletes both declarations.
3. `AbsorbingBiaxialNoisyTests.recoverFromNoisy` pipes
   `noisyObserveAbsoluteWith forward absorbing noise seed fullConfigurations |> expectOk`.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Optimization/BenchNoise.fs`
- `Berreman/BerremanTests/InverseFitHarness.fs`
- `Berreman/BerremanTests/AbsorbingBiaxialNoisyTests.fs`

## Risks

- **Bit-identity of the seeded stream.** Per configuration: two draws in `disturb`, one throughput draw, then fifteen
  element draws row by row. The fold visits the configurations in order, so the stream is unchanged.
- **ELEV on the new product functions.** Both are free functions over several parameters, like `misread` and
  `noisyObserveWith`, which are clean; the bodies destructure nothing new.
- **Runtime.** `AbsorbingBiaxialNoisyTests` takes about 946 s, inside the 7200 s deadline.
