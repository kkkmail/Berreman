# Step 0005 — CLOSE_RED — impl plan

Closes RED_0002 (`equal Tauc-Lorentz and Gaussian-oscillator models lower to equal axis data`).
The Tauc-Lorentz and Gaussian-oscillator models stop lowering to a fresh `EpsAxisEvaluated`
closure and lower to axis DATA: two core law records, which compare and hash structurally.
The kernels MOVE from `DispersionModels.baseIndex` into the core, so one implementation exists
(.spec-md §3.1).

## Approach

1. **Reference literals first.** Before anything moves, run the start tree's
   `DispersionModel.evaluate` (built Domain DLL, loaded by an fsi script under
   `.artifacts/`) for the three coefficient sets at 300, 450, 600, 900 and 2000 nm. Record
   each real and imaginary part as G17.
2. **Core (`Berreman/Berreman/Dispersion.fs`).**
   - `AbscissaKind.abscissaAt (w : WaveLength) : float`. This is the same arithmetic as
     `Units.fromMeters` over the scale and numerator that `ofUnit` derives.
   - `TaucLorentzLaw` and `GaussianOscillatorLaw` records, marked `NumericCarrier`. Each
     `complexIndex` is its `baseIndex` arm moved verbatim (self-identifier `c`, so the body
     text is unchanged) over `x = c.abscissa.abscissaAt w`.
   - `EpsAxisDispersion` gains `TaucLorentzAxis` and `GaussianOscillatorAxis`, compared and
     hashed structurally (hash tags 3 and 4).
3. **Domain (`DispersionModels.fs`).**
   - Move the `AbscissaKind.ofUnit` extension above the coefficient records, so the
     coefficient records can carry intrinsic `toTaucLorentzLaw` / `toGaussianOscillatorLaw`
     members (a cross-type reuse towards an earlier type is an intrinsic member).
   - `baseIndex`'s two arms answer the law's index. `toEpsAxis` lowers to the law cases.
   - `parameters`' `SumOfTerms` arm answers `[]` for the law cases.
   - Update the doc comments named in the slice, plus the module header and
     `EpsAxisDispersion`'s doc comment, which say the same thing.
4. **`LibraryFacets.facetKey`** maps both law cases to `"Evaluated (transcendental)"`.
   **`MaterialComplexity.isEditable`** treats both as "no term data".
5. **Facts.**
   - Three new `EpsWithDispValueTests` facts with exact `Assert.Equal` against the
     recorded literals.
   - Two `DispersionModelsTests` facts change only their match patterns and comments.

## Files

- `Berreman/Berreman/Dispersion.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/DispersionModels.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/LibraryFacets.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/MaterialComplexityEditor.fs`
- `Berreman/BerremanTests/EpsWithDispValueTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/DispersionModelsTests.fs`

## Risks

- **Bit-identity of the abscissa.** For every unit, `fromMeters` is `m / s` (with an exact
  `/ 1.0`), or `k / ((m / 1e-9) / 1.0)`. `ofUnit` derives `s` and `k` by exact
  multiplications by 1.0, so `abscissaAt` produces the same bits. The exact
  `Assert.Equal` facts pin this.
- **Record-label inference.** The new core records share field labels (`epsInf`,
  `amplitude`, …) with the Domain coefficient records. Every construction site is
  type-annotated or constructor-directed, and the build confirms it.
- **Exhaustive matches.** Every exhaustive match on `EpsAxisDispersion` must take the two
  new cases (FS0025 is an error in core). Sites found: `complexIndex`, `Equals`,
  `GetHashCode`, `parameters`, `facetKey`, `isEditable`.
