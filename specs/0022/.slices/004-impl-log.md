# 004 — Impl-log: Materials, dispersion models & spectral preview

## Progress

- [x] DispersionModels.fs — coefficient records, `DispersionModel` DU, `ThermoOptic` + `[<Measure>] K`, `evaluate`/`evaluateAt`/`toOpticalProperties`, anisotropic three-index path.
- [x] MaterialLibrary.fs — `MaterialEntry`/`MaterialCategory`/`MaterialLibrary`/`MaterialError`, `byCategory`/`byNameContains`, `resolveMaterial`, built-in presets.
- [x] MaterialImport.fs — `importRefractiveIndexInfo`/`importCsv`/`exportCsv`, `ImportError`, meter reduction, linear interpolation.
- [x] MaterialPreview.fs (Ui) + schema `materialEntry` `$def` (+ `dispersionModel`/`thermoOptic`).
- [x] Tests + fixtures + fsproj wiring (16 new tests).
- [x] Gates — build / unit-tests / constructor-unit-tests all PASS.

## Files modified

New:
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/DispersionModels.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/MaterialLibrary.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/MaterialImport.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/MaterialPreview.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/DispersionModelsTests.fs` (9 tests)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/MaterialImportTests.fs` (4 tests)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/MaterialPreviewTests.fs` (3 tests)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/fixtures/Si.yml`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/fixtures/sample-nk.csv`

Edited:
- `OpticalConstructor.Storage/OpticalConstructor.Storage.fsproj` — register `MaterialImport.fs` (after `Sidecar.fs`, before the `Storage.fs` anchor).
- `OpticalConstructor.Ui/OpticalConstructor.Ui.fsproj` — register `MaterialPreview.fs`.
- `OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj` — register the three new test files + the two fixtures as copied content.
- `OpticalConstructor.Storage/schema/optical-constructor-project.schema.json` — fill the `materialEntry` `$def` + add `dispersionModel`/`thermoOptic` `$defs`.
- (The Domain `DispersionModels.fs`/`MaterialLibrary.fs` were already registered in compile order by the slice-001 scaffold; no Domain fsproj edit needed.)

## Decisions

- `evaluate : DispersionModel -> WaveLength -> ComplexRefractionIndex` is the isothermal
  R-2 closure; `evaluateAt : float<K> -> DispersionModel -> WaveLength -> ComplexRefractionIndex`
  supplies the operating temperature for the §D.12 thermo-optic correction. R-2 and R-8
  are reconciled this way: `evaluate` = `evaluateAt referenceTemperature` (Δn = 0), and
  the operating `T` is a parameter only of `evaluateAt`, never stored.
- Oscillator models (Lorentz/Drude/Tauc-Lorentz/Gaussian) use photon energy E (eV) as
  their abscissa by setting `wavelengthUnit = ElectronVolt`; the same `Units.fromMeters`
  call normalises both wavelength-form and energy-form models.
- Import functions take raw file CONTENT (symmetric with `exportCsv` returning text), so
  IO/exceptions stay out of the parser and the error channel is pure `Result`.
- Tauc-Lorentz/Gaussian implement `ε₂` with `ε₁ = εInf` (no Kramers–Kronig `ε₁` integral)
  as the minimum implementation (§0 binding rule 6).

## Testing state

`commit_ready: true` — every R-1..R-8 requirement is addressed in this round; nothing is
deferred to a "round 2". All three project gates pass locally:

- `build` (`dotnet build Berreman.slnx -c Release`) — exit 0, "Build succeeded.", 0 errors, no lowercase `error`.
- `unit-tests` (`dotnet test --no-build -c Release` in `BerremanTests`) — Passed 70 / Skipped 5 / Total 75; baseline 70 held.
- `constructor-unit-tests` (`dotnet test OpticalConstructor.Tests`) — Passed 43 / Total 43; up from slice-003 baseline 27.

## Artifacts

- `.artifacts/004-build.log`
- `.artifacts/004-unit-tests.log`
- `.artifacts/004-constructor-unit-tests.log`

## Attempt 02 (retry — route-back fixes)

The code-judge routed back (gates were green; substance met R-1..R-8) for two narrow,
cheap fixes. This round lands ONLY those two plus the SoW hand-off note.

- [x] **Schema duplicate-`description` key.** `OpticalConstructor.Storage/schema/optical-constructor-project.schema.json`
  `dispersionModel` `$def` carried two `"description"` members — line 86 (the
  meaningful "Tagged analytic dispersion model…" text) and line 96 (the
  additional-properties note). JSON last-wins dropped the authored model description
  on load. Fix: folded the line-96 note into the line-86 description as one sentence
  and removed the duplicate key. Verified by `json.load` — `dispersionModel` now has a
  single `description` key whose text is the model description.
- [x] **Reuse-critic F1 — one `isotropicProperties` helper.** Added public
  `isotropicProperties (epsWithDisp : EpsWithDisp) : OpticalPropertiesWithDisp` to
  `DispersionModels.fs` (Domain, the lower layer — Domain cannot depend on Storage).
  Rewrote `toOpticalProperties` (both arms) and `toAnisotropicOpticalProperties` to
  pipe through it, removing the three inline `Mu.vacuum.dispersive`/`Rho.vacuum.dispersive`
  literals. Deleted the private copy in `MaterialImport.fs` and reused the Domain
  helper via `open OpticalConstructor.Domain.DispersionModels`. The vacuum-μ/ρ
  convention now lives at one site.
- **Deferred (NOT touched this round, per the retry hint).** The schema↔serializer
  tag-shape mismatch and the closure-vs-`DispersionModel` round-trip gap belong to the
  materials-wiring slice (Part A/I); nothing serializes through this `$def` yet
  (the aggregate has no `materials` field). Carried forward as an explicit hand-off
  risk in the SoW `Deferred`/`Gotchas`.

Gates re-run after the fixes (all PASS): build exit 0 / 0 Error(s);
`unit-tests` Passed 70 / Skipped 5 / Total 75 (baseline 70 held);
`constructor-unit-tests` Passed 43 / Total 43 (baseline 43 held). Logs refreshed
under `.artifacts/004-build.log`, `004-unit-tests.log`, `004-constructor-unit-tests.log`.

## Gotchas

- **AC-D4 is self-contradictory**: Silicon's engine closure is a bespoke polynomial, not a
  Sellmeier form, so a `Sellmeier` model cannot match `Silicon()` to 1e-9. Implemented
  Sellmeier faithfully; covered the intent by (a) reproducing a hand-built Sellmeier closure
  and (b) reproducing the silicon preset bit-for-bit via the library entry. Surfaced per the
  skepticism rule; operator may re-scope AC-D4 if a literal match is required (not achievable).
- **`Analytics.Variables.getWaveLengthValue` mis-scales a Nm range** (re-wraps the meter
  magnitude as `Nm`, ≈1e-9 too small). `exportCsv` and `axisTicks` sample λ in meters
  directly instead. Engine quirk surfaced here, not introduced by this slice.
- `MaterialPreview.show*` call `Chart.show` (browser) and are not exercised by tests; tests
  cover only the pure range/label/tick boundary.
