# Step 0009 — IMPLEMENT — impl plan

## Goal

Delete `EpsAxisEvaluated` and `EpsAxisDispersion`'s custom equality, so every axis is data and the
union takes F#'s structural equality (001 §B; `.spec-md` §0.1, first row; obligation
`S11-NO-CLOSURE`). Steps 2, 5, 7 and 8 already moved every producer to a data case, so only the
union case, its arms, the doc comments that name it and two test fixtures remain.

## Approach

1. `Berreman/Berreman/Dispersion.fs`
   - Drop `[<CustomEquality; NoComparison>]`, the `EpsAxisEvaluated` case, its `complexIndex`
     arm and the `Equals` / `GetHashCode` overrides.
   - Rewrite the doc comment above `EpsAxisDispersion` so it lists its data cases: term data
     (`RealNK`, `ComplexEps`), the tabulated table, the four oscillator laws and the published
     laws. Equality is structural now, so the custom-equality paragraph goes.
2. Domain dead arms:
   - `DispersionModels.fs`: `parameters`' `SumOfTerms` arm.
   - `LibraryFacets.fs`: `facetKey`.
   - `MaterialComplexityEditor.fs`: `isEditable`. Its predicate now reads "an axis is editable
     exactly when it is `RealNK` or `ComplexEps` term data".
   - Rewrite every doc comment in these three files that names the evaluated case or closure.
3. Test fixtures: `OutOfBandDiagnosticTests.fs` and `OutOfBandBadgeTests.fs` build a constant
   `RealNK` axis with n = 1.5 and k = 0.
4. `EpsWithDispValueTests.fs`: add the fact `Equal axis data of every case is equal and hashes
   equal, and a changed value breaks equality`. It covers `RealNK`, `ComplexEps`, `TabulatedNK`,
   the four law cases and `PublishedLawAxis`. For each case it builds two equal values separately
   and builds a variant with one coefficient, sample or law changed.
5. Afterwards `git grep EpsAxisEvaluated -- Berreman/` must find nothing.

## Files

- `Berreman/Berreman/Dispersion.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/DispersionModels.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/LibraryFacets.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/MaterialComplexityEditor.fs`
- `Berreman/BerremanTests/EpsWithDispValueTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OutOfBandDiagnosticTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/OutOfBandBadgeTests.fs`

## Risks

- **Comparison.** Dropping `NoComparison` lets F# infer comparison. It infers none, because the
  laws and formulas hold `System.Numerics.Complex`, which is not `IComparable`. That inference
  raises no error, so no call site changes.
- **Hashing.** Structural hashing of a record that holds arrays (`DispersionTerm.coefficients`,
  `TabulatedNkTable` columns) is structural in F#, so two separately built equal tables hash
  equal. The new fact checks this.
- **Unused `open System.Numerics` / `MathNetNumericsMath`.** These may become unused in the two
  fixtures. F# does not warn on an unused open by default, but any open the new axis no longer
  needs is removed anyway.
