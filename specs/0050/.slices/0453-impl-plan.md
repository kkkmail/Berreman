# Step 0453 — impl plan (IMPLEMENT, S132-TABLE-SIGMA)

## Approach

Spec 0050 S13.2: a tabulated n,k table carries optional per-sample one-sigma uncertainties of n and k as data,
serialised with the table.

1. `Berreman/Berreman/Dispersion.fs`, beside `TabulatedNkTable`:
   - `TabulatedNkUncertainty`, a `NumericCarrier` record of `sigmaN : float array` and `sigmaK : float array`;
   - `TabulatedTableError.InvalidUncertaintyErr of reason : string`;
   - `TabulatedNkTable.uncertaintyOpt : TabulatedNkUncertainty option`, the last field, which `tryCreate` sets to
     `None`;
   - `member this.withUncertainty (sigmaN : float array) (sigmaK : float array)`. Columns whose lengths are not the
     table's answer `SampleCountMismatchErr`. The first NaN, infinite or negative value answers
     `InvalidUncertaintyErr`, naming its column and index. The columns are kept as given.
   - `complexIndex` is untouched.
2. Every complete match on `TabulatedTableError` gains the new case, mapped as the other three are:
   - `MaterialImport.fs` maps it to `NoDataErr`;
   - the `tableOf` helper in `EpsWithDispValueTests.fs` maps it to `failwith`;
   - the fact in `LibraryFacetsTests.fs` maps it to `Assert.Fail`, because FS0025 is an error in every project.
3. Tests:
   - `A tabulated table takes per-sample uncertainties of its own length and refuses any other` in
     `BerremanTests.EpsWithDispValueTests`;
   - `a tabulated material keeps its per-sample uncertainties through an entry file` in
     `OpticalConstructor.Tests.EntryFileJsonTests`.

## Files

- `Berreman/Berreman/Dispersion.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/MaterialImport.fs`
- `Berreman/BerremanTests/EpsWithDispValueTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/EntryFileJsonTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/LibraryFacetsTests.fs`. This file is not in the effects'
  edits list, but it is required by how_to (2), and `OpticalConstructor.Tests` is in `touches`.

## Risks

- **Serialization.** FSharp.SystemTextJson with `WithUnwrapOption` writes `None` as `null` and an array as a JSON
  array. The entry schema does not bind the physics inside a payload (step 30), so no schema change is expected.
  No committed file holds a tabulated table, so no committed file changes.
- **Elevation.**
  - The new record holds bare `float array` fields, so it carries the `NumericCarrier` mark.
  - `withUncertainty` is a member, so its primitive signature is exempt.
  - The new error case ends in `Err`.
