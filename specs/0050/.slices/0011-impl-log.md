# Step 0011 — IMPLEMENT — impl log

## Progress

- [x] Report.fs: `vacuumPlaceholderComplexity`, and `dtoToEntry` carries it
- [x] ExportImportTests: the metadata-only re-import fact
- [x] DispersionModelsTests: the minted-id fact over a constant complexity
- [x] FsxAcceptanceTests: every fixture states its physics as data
- [x] Build, elevation and tests (local observations)

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Storage/Report.fs`:
  - `vacuumProperties` is replaced by the public `vacuumPlaceholderComplexity : MaterialComplexity`.
    Its value is `IsotropicAbsorbing (ComplexRefractionIndex (1 + 0i))`, with no magnetic or
    active aspect.
  - `dtoToEntry` sets `properties = vacuumPlaceholderComplexity.toProperties` and
    `complexity = Some vacuumPlaceholderComplexity`.
  - The "view-only until the §D.9 mapping lands" comment is deleted.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/ExportImportTests.fs`: new fact
  `a metadata-only library file re-imports each entry as vacuum data`.
  - It exports the described built-ins, filtered as the 0035 round-trip filters them, to
    `AppContext.BaseDirectory`, and re-imports the file.
  - It asserts that the count is equal and that every `complexity` is
    `Some vacuumPlaceholderComplexity`.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/DispersionModelsTests.fs`: the minted-id
  fact now uses a constant `IsotropicTransparent (RefractionIndex 1.5)` complexity with
  `properties = complexity.toProperties`.
  - The lookup is checked through `epsClose 1e-12` against `complexity.eps.toEpsWithDisp.getEps`
    at 400, 600 and 800 nm, not by reference.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/FsxAcceptanceTests.fs`:
  - `material` takes a `MaterialComplexity`.
  - New private helpers: `constantComplexity`, `dispersiveGlassComplexity` (the Sellmeier
    `toEpsAxis` lowering, 300–700 nm), `langasiteOnSiliconSweep` (250–600 nm),
    `langasiteComplexity` (the published laws plus `PublishedLawRho LangasiteGyrationLaw`) and
    `siliconComplexity`.
  - The glasses and the vacuum spacer pass `IsotropicTransparent` of the named
    `RefractionIndex` constants.
  - `euvStack`, `euvAgainstIncidence` and `euvAgainstWaveLength` take `ComplexRefractionIndex`.
    The table stores `IsotropicAbsorbing`, and the script builds
    `Eps.fromComplexRefractionIndex >> OpticalProperties.fromEpsion`.
  - `euvComplex` is deleted, and the new `euvLayerIndex` builds n = 1 − δ + iβ.
  - Opens `OpticalConstructor.Domain.Units` and `OpticalConstructor.Domain.DispersionModels`.

## Decisions

- **`vacuumPlaceholderComplexity` is public, not private.** The new fact compares with that one
  declaration rather than a re-transcribed literal (AGENTS.md "Reuse, never copy"). The name is
  unique in the tree.
- **`euvLayerIndex` replaces the deleted `euvComplex`.** It builds only the
  `ComplexRefractionIndex` that the facts pass, and the script's construction of the layer
  properties moves into `euvStack`, as the slice says. The alternative was to inline
  `ComplexRefractionIndex (Complex.create (1.0 - δ) β)` eight times.
- **The minted-id fact compares with the complexity's own eps (`complexity.eps.toEpsWithDisp`).**
  It does not go through `toProperties`, so the fact still says something after step 13 derives
  `properties`.
- **One 250–600 nm interval is shared by Langasite and Silicon.** It is the script's sweep, and
  the slice gives both materials that band.

## Testing state

These are local observations. The arc-runner's gate engine is the authority.

- build: exit 0, 0 errors, and only the two vendored MathNet SYSLIB0051 warnings that predate
  this step.
- elevation: `ELEV-GATE: OK - 0 new finding(s) in 5 project(s), 126s`.
- constructor-unit-tests (whole): Passed!, 737 succeeded and 0 failed. The two touched facts were
  also run by name: 2/2.
- ui-tests (FsxAcceptanceTests): Passed!, 14 succeeded and 0 failed.
- ui-smoke: Passed!, 208 succeeded and 0 failed.
- The four s02 invariants each find nothing (git grep exit 1).

## Artifacts

- `specs/0050/.artifacts/0011-local-build.log`
- `specs/0050/.artifacts/0011-local-elevation.log`
- `specs/0050/.artifacts/0011-local-constructor-unit-tests.log`
- `specs/0050/.artifacts/0011-local-ui-tests.log`
- `specs/0050/.artifacts/0011-local-ui-smoke.log`

## Gotchas

- **The older ExportImportTests facts still write to `Path.GetTempPath()`.** They are outside this
  step's effects. The new fact writes under the test run's output directory, as the slice asks.
- **The slice's line numbers matched the start tree,** for example `FsxAcceptanceTests.fs:85-88`,
  `:273`, `:357`, `:386-387`, `:410-411`, `:442-443`, `:477-509` and `Report.fs:46-56`, `:93-111`.
- **The Sellmeier lowering differs from the script's closure at about 1e-15.** The lowering is
  ε = (1 + Σb) + Σ b·c/(x² − c) with a complex sqrt; the script uses 1 + Σ b·x²/(x² − c) with a
  real sqrt. Both Glass_Dispersive facts pass at 1e-6.
