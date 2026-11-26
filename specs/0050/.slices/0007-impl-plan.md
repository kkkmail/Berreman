# Step 0007 — CLOSE_RED — impl plan

Closes RED_0003 (`equal Forouhi-Bloomer and Brendel-Bormann models lower to equal axis data`,
red since step 6). The two remaining transcendental models become core law DATA, exactly as step 5
did for Tauc-Lorentz and the Gaussian oscillator, so no model lowers to `EpsAxisEvaluated` any more
(.spec-md §3.1). The case itself stays: step 9 deletes it once Silicon and Langasite (step 8) no
longer produce it.

## Approach

1. **Record the references first.** Run the start tree's `DispersionModel.evaluate` (the step-6
   build, production code unchanged since step 5) over the AC-B6 `forouhiBloomerASi` and
   `brendelBormannGold` sets at 300, 450, 600, 900 and 2000 nm, printed as G17. Script and output
   go into `.artifacts/`.
2. **Core (`Berreman/Berreman/Dispersion.fs`).**
   - `open MathNetNumericsMath` right after `open System.Numerics` (as `Fields.fs` does), so the
     moved bodies keep `Complex.create` verbatim; every later open still shadows it.
   - `ForouhiBloomerLaw` (`nInf`, `a`, `b`, `c`, `bandGap`, `abscissa`), `NumericCarrier`, whose
     `complexIndex` is the `baseIndex` arm moved verbatim over `abscissaAt`.
   - Weideman's Faddeeva series (`faddeevaTermCount`, `FaddeevaSeries`, `faddeevaSeries`,
     `faddeeva`) moved verbatim, private, just above `BrendelBormannLaw`.
   - `BrendelBormannLaw` (`plasmaFrequency`, `intrabandStrength`, `intrabandDamping`, `strength`,
     `resonance`, `damping`, `broadening` as `float list`, `abscissa`), `NumericCarrier`, whose
     `complexIndex` is the `baseIndex` arm moved verbatim.
   - `EpsAxisDispersion` gains `ForouhiBloomerAxis` / `BrendelBormannAxis` with `complexIndex`,
     structural `Equals` and `GetHashCode` (tags 5 and 6) arms; its doc comment follows.
3. **Domain (`DispersionModels.fs`).** Intrinsic `toForouhiBloomerLaw` / `toBrendelBormannLaw`
   (isothermal, `AbscissaKind.ofUnit this.wavelengthUnit`); `baseIndex`'s two arms answer the law;
   the Faddeeva block is deleted; `toEpsAxis` lowers both to their cases, so the
   `EpsAxisEvaluated this.evaluate` line goes; `parameters`' `SumOfTerms` arm answers `[]` for both
   cases. Doc comments that said these two "stay closure-evaluated" are rewritten.
4. **`LibraryFacets.facetKey`** → `"Evaluated (transcendental)"` for both cases;
   **`MaterialComplexity.isEditable`** treats both as non-term data (view-only).
5. **Facts.** Two exact-literal law facts in `EpsWithDispValueTests` (reusing step 5's
   `electronVolt` and `verifyLawReferences`); the three `EpsAxisEvaluated _` matches follow the law
   cases, with `Assert.False(c.isEditable)` added to the MaterialComplexity fact.

## Files

- `Berreman/Berreman/Dispersion.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/DispersionModels.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/LibraryFacets.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/MaterialComplexityEditor.fs`
- `Berreman/BerremanTests/EpsWithDispValueTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/DispersionModelsTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/MaterialComplexityTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/MaterialEditorWindowTests.fs`

## Risks

- **Bit identity.** The abscissa must stay bit-identical to `Units.fromMeters` (step 5's
  `abscissaAt` already guarantees this), and the kernel arithmetic must be moved unchanged; the
  exact literals and the AC-B6 `Assert.Equal` on `evaluate` vs `complexIndex` pin it.
- **Record-label inference.** The law records share labels with the Domain coefficient records
  (`a`, `b`, `c`, `bandGap`, `strength`, …) and with `TaucLorentzLaw` (`resonance`,
  `broadening`). Every construction site is annotated or constructor-directed.
- **ELEV.** The moved private Faddeeva helpers were ELEV-clean in the Domain; the core is under
  the same rules. Run the elevation check locally.
