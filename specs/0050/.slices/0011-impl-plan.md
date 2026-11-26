# Step 0011 — IMPLEMENT — impl plan

## Approach

Every place that builds a `MaterialEntry` from engine properties states its physics as
`MaterialComplexity` data first, while `properties` is still a field (.spec-md §3.2). The
table side of the script comparisons then reads `complexity.toProperties`, and the script side
keeps the script's own properties, so the comparisons show that no number moves before step 13.

1. `OpticalConstructor.Ui.Tests/FsxAcceptanceTests.fs`:
   - `material` takes a `MaterialComplexity` and stores `properties = complexity.toProperties`
     with `complexity = Some complexity`.
   - Every caller passes its material as data:
     - Glass: `EpsWithoutDispValue (IsotropicTransparent (RefractionIndex 1.78052))`.
     - Glass_Dispersive: one isotropic dispersive segment over 300–700 nm, with the Domain
       Sellmeier lowering `toEpsAxis`.
     - Langasite: one uniaxial dispersive segment over 250–600 nm with the two published index
       laws, plus `active = Some (PublishedLawRho LangasiteGyrationLaw)`.
     - Silicon: one isotropic dispersive segment over 250–600 nm with `SiliconIndexLaw`.
     - The glasses and the vacuum spacer: `IsotropicTransparent` of the named `RefractionIndex`
       constants.
     - EUV: each layer is a `ComplexRefractionIndex`. The table stores `IsotropicAbsorbing`, and
       the script builds `Eps.fromComplexRefractionIndex >> OpticalProperties.fromEpsion`.
       `euvComplex` is deleted.
   - The script side is unchanged: `constantN`, `dispersiveGlass`, `langasiteFilmOnSilicon`,
     `OpticalProperties.*` and the EUV scripts' arithmetic all stay.
2. `OpticalConstructor.Tests/DispersionModelsTests.fs`: the minted-id fact gets a constant
   `IsotropicTransparent (RefractionIndex 1.5)` complexity with `properties = complexity.toProperties`.
   The lookup compares `getEps` at 400, 600 and 800 nm through `epsClose 1e-12`, not by reference.
3. `OpticalConstructor.Storage/Report.fs`: `vacuumProperties` becomes
   `vacuumPlaceholderComplexity : MaterialComplexity`, and `dtoToEntry` sets both `properties`
   and `complexity` from it. The "view-only until §D.9" comment is deleted. The value is public,
   so the new fact compares with the one declaration instead of a re-transcribed copy.
4. `OpticalConstructor.Tests/ExportImportTests.fs`: a new fact,
   `a metadata-only library file re-imports each entry as vacuum data`. It exports the described
   built-ins to a file under `AppContext.BaseDirectory`, re-imports the file, and checks that
   every entry's `complexity` is `Some vacuumPlaceholderComplexity`.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Storage/Report.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/DispersionModelsTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/ExportImportTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/FsxAcceptanceTests.fs`

## Risks

- **Numerical drift on Glass_Dispersive.** The lowering is ε = (1 + Σb) + Σ b·c/(x² − c), as a
  complex value with a complex sqrt. The script computes 1 + Σ b·x²/(x² − c) with a real sqrt.
  The two differ at the 1e-15 level, far inside the 1e-6 tolerance.
- **Out-of-band segments.** The 300–700 nm and 250–600 nm segments cover their sweeps exactly.
  A grid point that falls outside by rounding takes the topmost segment, which is the same law,
  so the numbers are unchanged.
- **Downstream reads of `complexity = None` on imported entries.** No product code reads
  imported entries' complexity before step 13, and none was found by grep.
