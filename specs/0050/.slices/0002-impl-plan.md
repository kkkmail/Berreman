# Step 0002 — CLOSE_RED — impl plan

Close RED_0001 (step 1's two `MaterialImportTests` shape facts) by making the tabulated n,k
table an axis case of the core and building it in the importer (001 §B, .spec-md §3.1). The
interpolation kernel MOVES from `MaterialImport.buildTabulatedClosure` into the core, so one
implementation exists; no number moves.

## Approach

1. `Berreman/Berreman/Dispersion.fs`, before `EpsAxisDispersion`:
   - `TabulatedInterpolation` (one case `LinearInWaveLengthClamped`, `value : int` = 1,
     `tryOfWire : int -> TabulatedInterpolation option`).
   - `TabulatedTableError` (`EmptyTableErr` / `SampleCountMismatchErr` / `UnsortedSamplesErr`,
     each `of reason : string`).
   - `TabulatedNkTable` (`[<NumericCarrier>]` record: `wavelengths : float<meter> array`, `n`,
     `k`, `interpolation`), with `tryCreate` (refuses empty, unequal columns, a decreasing
     wavelength; never sorts), `sampleRange` (first..last as `WaveLength.nm`) and
     `complexIndex` (the closure's arithmetic, verbatim, answering the index).
   - `EpsAxisDispersion` gains `TabulatedNK of TabulatedNkTable`; `complexIndex` calls the table;
     the custom `Equals` / `GetHashCode` compare / hash it structurally; the axis doc comment
     names the case (and moves to sit directly on `EpsAxisDispersion`, where it belongs).
2. Every match on `EpsAxisDispersion`: `DispersionModel.parameters` (`[]`),
   `EpsAxisDispersion.facetKey` (`DiscreteKey "Tabulated n/k"`), `MaterialComplexity.isEditable`
   (not editable term data, like the evaluated case). Test-side matches all end in a catch-all.
3. `MaterialImport.fs`: `entryFromTabulated` sorts with the same `Array.sortBy`, builds the table
   through `tryCreate`, answers `Result<MaterialEntry, ImportError>` (`TabulatedTableError` →
   `NoDataErr reason`), complexity = one isotropic dispersive segment over `sampleRange`,
   `properties = complexity.toProperties`. The two call sites drop `Ok`. `buildTabulatedClosure`
   deleted; module doc, `entryFromTabulated` doc and `NkTableRow`'s carrier reason rewritten.
   `NkTableRow.lambdaMeters` becomes `float<meter>` so the λ column feeds the table without a
   strip-then-restore of the unit.
4. Facts: five in `EpsWithDispValueTests`, one in `LibraryFacetsTests`, one in
   `MaterialImportTests`.

## Files

- `Berreman/Berreman/Dispersion.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/DispersionModels.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/LibraryFacets.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/MaterialComplexityEditor.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/MaterialImport.fs`
- `Berreman/BerremanTests/EpsWithDispValueTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/LibraryFacetsTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/MaterialImportTests.fs`

## Risks

- A new DU case makes any exhaustive match incomplete (FS0025, zero-warnings rule): grepped every
  `RealNK` / `ComplexEps` / `EpsAxisEvaluated` match; only the three Domain sites are exhaustive,
  test matches end in `| other ->`.
- Structural equality of a record holding arrays: F# compares and hashes array fields
  element-wise, so `t1 = t2` / `hash t` are structural.
- ELEV: new names are unique across `C:\GitHub` sources; primitives live only in the carrier record
  and in members.
- Out-of-band diagnostic now sees a tabulated import's sample range — intended per the slice.
