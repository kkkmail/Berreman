# Step 0343 — impl plan

Obligation S101-DIRECTION-MOVE. The last two harness functions, `sumWeights` and `directionalSensitivity`, move
from `BerremanTests/InverseFitHarness.fs` into `OpticalConstructor.Optimization.FitUncertainty`. After the step the
harness holds only test assertions and formatting (001 §Q AT-2). It is a MOVE: each moved name is declared exactly
once, in the product.

## Approach

1. **`FitUncertainty.fs`, after `differenceWeights`:**
   - `sumWeights (scaling : ParameterScaling<'Parameters>) (names : ParameterName list) : Result<ParameterWeights, MuellerFitError>`,
     built without mutation (`Array.init`): 1.0 at the first axis of each named parameter, 0.0 elsewhere. The first
     name not among `scaling.axes` is `Error (UnknownAxisErr name)`.
   - `type DirectionalSensitivity = | DirectionalSensitivity of double` with `value`, and a one-line doc.
   - `directionalSensitivity (jacobian : ResidualJacobian) (direction : ParameterWeights) : DirectionalSensitivity`,
     with today's normalization and row sums over `direction.value`.
   - Each doc is one to three lines and keeps the column-versus-combination hazard.
2. **`InverseFitHarness.fs`** deletes both functions and the stale "the noise model" separator. It keeps five
   bindings (`expectOk`, `describeErrors`, `describeColumnNorms`, `assertMuellerEqual`, `ensembleSeeds`), with their
   bodies unchanged and each doc one to three lines. The module doc becomes the brief's one to three lines, and every
   `open` nothing reads any more is deleted.
3. **`AbsorbingBiaxialInverseTests.fs`:**
   - `isotropicAbsorption : … -> ParameterWeights` becomes
     `sumWeights scaling [ ParameterName "e11"; ParameterName "e22"; ParameterName "e33" ] |> expectOk`.
   - `anisotropicAbsorption : … -> ParameterWeights` builds its array as today and wraps it.
   - In the selected fact, the three sensitivities are compared and reported through `.value`. Bands and messages are
     unchanged.
   - The file opens `FitUncertainty` (and `ItemModels`, for `ParameterName`), as the compiler asks.
4. **`BiaxialInverseTests.fs`:** the class doc's sharing sentence names the four product modules instead of
   `InverseFitHarness`.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Optimization/FitUncertainty.fs`
- `Berreman/BerremanTests/InverseFitHarness.fs`
- `Berreman/BerremanTests/AbsorbingBiaxialInverseTests.fs`
- `Berreman/BerremanTests/BiaxialInverseTests.fs`

## Risks

- **Bit-identical numbers.** The weights hold the same 1.0 / 0.0 entries, and the sensitivity runs the same
  normalization and the same row sums in the same order.
- **ELEV.** The moved code is now inside the analyzer's scope.
  - `DirectionalSensitivity` (type and case) and `directionalSensitivity` must be unique names.
  - `sumWeights` has no primitive in its signature.
- **Test count.** No test is added or removed.
