# 0338 — impl plan

## Approach

A move of the relative error and the per-parameter recovery errors from `BerremanTests/InverseFitHarness.fs` into
`OpticalConstructor.Optimization.MuellerFitting` (spec 0050 S10.1, obligation S101-RELATIVE-ERROR-MOVE).

1. `MuellerFitting.fs`, after `fitAbsorbingObservations`:
   - `type QuantityValue = | QuantityValue of double` with `value` (one-line doc);
   - `RelativeError` with `value` and `static member between (actual : QuantityValue) (expected : QuantityValue)`,
     the same `abs (actual − expected) / abs expected` over `.value` of each;
   - `recoveryErrors`, signature unchanged, each axis's error `RelativeError.between (QuantityValue (a.read
     recovered)) (QuantityValue (a.read truth))`.
   Docs cut to one to three lines.
2. `InverseFitHarness.fs`: delete `RelativeError` and `recoveryErrors`. `statisticsFor`'s `bias` and `worst` call
   `RelativeError.between` over `QuantityValue` of the same doubles. Everything else (`describeErrors`,
   `RecoveryStatistics`, `standardErrorOfMean`, `predictedRelativeErrors`) resolves to the product type through the
   file's existing `open OpticalConstructor.Optimization.MuellerFitting`.
3. `MuellerInverseTests.relativeError` becomes `RelativeError.between (QuantityValue actual) (QuantityValue expected)`.
4. The other consumers (`BiaxialInverseTests`, `FeasibleBiaxialInverseTests`, `AbsorbingBiaxialInverseTests`,
   `AbsorbingBiaxialNoisyTests`) already open `MuellerFitting`; no edit unless the compiler asks.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Optimization/MuellerFitting.fs`
- `Berreman/BerremanTests/InverseFitHarness.fs`
- `Berreman/BerremanTests/MuellerInverseTests.fs`

## Risks

- ELEV duplicate-name rules: `QuantityValue` / `RelativeError` must be unique among `Softellect.*` names (checked: no
  namesake in Berreman product code or STL).
- Bit-identity: the division runs on the same doubles, so the numbers are unchanged.
- LF line endings on every touched file.
