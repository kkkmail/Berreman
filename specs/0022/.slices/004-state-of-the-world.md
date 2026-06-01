# 004 — State of the world: Materials, dispersion models & spectral preview (Part D §D.5–D.12)

## Where we are

Slice 004 fills out Part D's materials half on top of the slice-002 units spine and
the slice-003 storage core. It lands the dispersion-model coefficient catalogue and
its evaluation to the engine's `OpticalPropertiesWithDisp` closure shape
(`DispersionModels.fs`), the searchable/categorised materials library with the by-id
`resolveMaterial` resolution seam slices 005/014 delegate to (`MaterialLibrary.fs`),
refractiveindex.info YAML / CSV import and CSV export (`MaterialImport.fs`), and the
spectral-preview routing into the engine's existing dispersion plots with a
nm/µm/Å/eV/cm⁻¹ axis toggle (`MaterialPreview.fs`). The schema `materialEntry` `$def`
dispersion sub-object is filled. No engine type is forked: evaluation reuses
`Eps.fromComplexRefractionIndex`, the `Units.fs` conversion seam, the `getEps` path,
and `Analytics/Charting.fs` end to end.

## What's working

- Add the dispersion-model coefficient catalogue (Sellmeier, Cauchy, Lorentz, Drude, Tauc-Lorentz, Gaussian, ConstantNK) as data with an explicit per-record `wavelengthUnit`, the wrapping `DispersionModel` DU, and `evaluate`/`toOpticalProperties` reusing the engine closure shape.
- Add the anisotropic three-index path mapping uniaxial `(n_o, n_e)` to `Eps.fromComplexRefractionIndex (n_o, n_e, n_o)` with optional per-axis dispersion.
- Add the optional thermo-optic `dn/dT` parameter and net-new `[<Measure>] K`: `None` is byte-identical to the isothermal index, `Some` applies `dndT·(T−referenceTemperature)` at a caller-supplied operating `T` that is never persisted.
- Add the searchable `MaterialLibrary` with `byCategory`/`byNameContains`, built-in `Silicon`/`Langasite`/`Standard` presets wrapped as entries, and the by-id `resolveMaterial` seam returning `Result<OpticalProperties, MaterialError>`.
- Add refractiveindex.info µm YAML + nm CSV import (linear-in-λ interpolation, λ reduced to meters via `Units`) and `exportCsv` sampling n,k over a `Range<WaveLength>`.
- Add the `MaterialPreview` routing into `plotN11`/`plotXi11`/… with a spectral-unit toggle that relabels/rescales the axis only, leaving the canonical-meter range untouched; fill the schema `materialEntry` `$def`; add 16 unit tests (constructor count 27 → 43).
- Fold the duplicate `"description"` key in the schema `dispersionModel` `$def` into one key so JSON last-wins no longer drops the authored model description on load.
- Lift one `isotropicProperties` helper into `DispersionModels.fs` (Domain) and reuse it from `toOpticalProperties`/`toAnisotropicOpticalProperties` and `MaterialImport.fs`, retiring the three inline `Mu.vacuum.dispersive`/`Rho.vacuum.dispersive` literals.

## Tests

- `build` — PASS (exit 0, "Build succeeded.", 0 Error(s); no lowercase `error`). Log `.artifacts/004-build.log`.
- `unit-tests` — PASS (exit 0, Passed 70 / Skipped 5 / Total 75; `BerremanTests` untouched). Baseline `berreman_unit_tests = 70` held. Log `.artifacts/004-unit-tests.log`.
- `constructor-unit-tests` — PASS (exit 0, Passed 43 / Total 43; up from slice-003 baseline of 27). Log `.artifacts/004-constructor-unit-tests.log`.

Coverage: AC-D3 (`MaterialImportTests` — µm YAML / nm CSV import reduces λ to meters,
`getEps` reproduces tabulated n,k, linear interpolation, exportCsv round-trip); AC-D4
(`DispersionModelsTests` — `evaluate` reproduces the analytic Sellmeier closure shape,
and the silicon library entry reproduces the engine preset bit-for-bit); AC-D5
(uniaxial maps to the engine three-index constructor); AC-D8 (`None` byte-identical,
`Some` first-order correction at the operating `T`, only `dndT`/`referenceTemperature`
persist through JSON); the §B.6/§J.4 `resolveMaterial` seam (known id → tensor, unknown
id → `Error (UnknownMaterialId _)`); AC-D7 (`MaterialPreviewTests` — the spectral-axis
unit leaves the canonical `Range<WaveLength>` unchanged). None deferred.

## Architecture

- **Unified abscissa via the `Units` seam.** `evaluate` computes the model abscissa as
  `Units.fromMeters wavelengthUnit w.value`, so wavelength-form models (Sellmeier/Cauchy
  in µm) and energy-form oscillator models (Lorentz/Drude/Tauc-Lorentz/Gaussian in eV)
  share one normalisation path with no literal conversion factors (§D.11).
- **Thermo-optic with no overhead.** `thermoOptic = None` returns the base closure
  directly (no `+0.0`), so the isothermal result is byte-identical; `Some` wraps it with
  the real-index correction. The operating temperature `T` is a parameter of `evaluateAt`,
  never a record field, so it cannot be persisted — only `dndT`/`referenceTemperature` are.
- **`resolveMaterial` reuses `getProperties`.** Resolution evaluates an entry's
  `OpticalPropertiesWithDisp` at the supplied wavelength through the engine
  `getProperties` wrapper (which calls `getEps`/`getMu`/`getRho`), yielding the concrete
  `OpticalProperties` without rebuilding any tensor — the single seam slices 005/014 call.
- **Import string = file content.** `importRefractiveIndexInfo`/`importCsv` take the raw
  text and `exportCsv` returns text, keeping filesystem IO and its exceptions out of the
  parser; the error channel stays pure `Result<_, ImportError>`.
- **Schema `$def` is documentation, not a serialized path.** The aggregate still carries
  no `materials` field, so the filled `materialEntry`/`dispersionModel`/`thermoOptic`
  `$defs` describe the hand/LLM-editable shape; they do not change the slice-003 envelope
  or the serializer.

## Deferred

- Crystal-axis ORIENTATION (rotation) — reuses `Layer.rotate`, owned by the stack editor (Part B / slice 005).
- The shareable material-library FILE format and material import/export over the storage seam — Part I §I.8 (slice 013); this slice's library is in-memory and additive.
- Full temperature-dependent Sellmeier tables, composition-interpolation tables, thermal-expansion models — out of scope; only first-order `dn/dT` lands here (§D.12).
- The full Kramers–Kronig `ε₁` for Tauc-Lorentz/Gaussian oscillators — `ε₁ = εInf` (minimum implementation); a later slice may add the dispersive real part if a directive calls for it.
- **HAND-OFF RISK — schema↔serializer tag-shape mismatch (deferred to the materials-wiring slice, Part A/I).** The `dispersionModel` `$def` documents an internally `kind`-tagged object, but the shared `ProjectJson.options` serializer emits the adjacent-tag `Case`/`Fields` FsPickler/STJ shape. The two disagree. This is NOT a slice-004 defect to fix: the aggregate carries no `materials` field yet, so nothing serializes through this `$def` in this slice. The slice that wires `materials` into the aggregate MUST reconcile the serializer's tag convention with the `$def` (or vice-versa) before any `DispersionModel` round-trips through JSON. (Per the code-judge retry hint, explicitly out of scope here.)
- **HAND-OFF RISK — closure-vs-`DispersionModel` round-trip gap (deferred to the same materials-wiring slice).** `MaterialEntry` stores an opaque `OpticalPropertiesWithDisp` closure, not the originating `DispersionModel` coefficient record, so coefficients cannot round-trip back out of an entry to the schema. The materials-wiring slice MUST decide whether `MaterialEntry` carries the `DispersionModel` (so coefficients persist via the `$def`) or whether the `$def` is documentation-only; until then a loaded entry cannot reproduce its source coefficients. Out of scope here per the retry hint.

## Architecture decisions

(see **Architecture** above.)

## Gotchas

- **AC-D4 is self-contradictory as written.** Silicon's engine closure
  (`OpticalProperties/Dispersive.fs:78-80`) is a bespoke polynomial in λ, NOT a Sellmeier
  form, so a `Sellmeier` model "whose coefficients match Silicon's closure inputs" cannot
  agree with `Silicon()` to 1e-9. Per the system-prompt skepticism rule the slice implements
  Sellmeier faithfully and covers AC-D4's intent two ways: (a) `evaluate` reproduces a
  hand-built Sellmeier closure of the engine's shape, and (b) the silicon library entry
  reproduces `Silicon().opticalProperties` bit-for-bit by reusing the preset (no
  re-derivation). The operator may re-scope AC-D4 if a literal Sellmeier⇄Silicon match is
  required — it is not achievable.
- **`Analytics.Variables.getWaveLengthValue` mis-scales for a Nm range.** It re-wraps the
  canonical meter magnitude returned by `RangedVariable.value` as a `Nm` value (so a 500 nm
  point becomes `Nm 5e-7<nm>` ≈ 5e-16 m). `exportCsv` and `MaterialPreview.axisTicks`
  therefore sample λ in meters directly (`startValue.value … nmToMeter`) rather than calling
  `getWaveLengthValue`. This is an engine quirk surfaced here, not introduced by this slice.
- **`thermoOptic` lives on each coefficient record.** R-8 says it is carried "alongside the
  model's coefficient record"; it is a field on every record (not a separate wrapper), which
  is why `thermoOpticOf` pattern-matches each case. This keeps the persisted shape a flat
  dispersion sub-object as the `materialEntry` `$def` expects.
- **`MaterialPreview.show*` call `Chart.show`** (open a browser window) — they are for
  interactive use and are NOT exercised by tests; tests cover only the pure
  `spectralRange`/`plotInput`/`axisLabel`/`axisTicks` boundary.

## Changelog

- 2026-05-31 (slice 004): Fill Part D's materials half — the dispersion-model coefficient
  catalogue + `evaluate`/`toOpticalProperties` and anisotropic three-index path
  (`DispersionModels.fs`, net-new `[<Measure>] K` + optional `ThermoOptic`), the searchable
  `MaterialLibrary` with built-in `Silicon`/`Langasite`/`Standard` presets and the by-id
  `resolveMaterial` seam (`MaterialLibrary.fs`), refractiveindex.info YAML/CSV import + CSV
  export (`MaterialImport.fs`), and the spectral-preview routing into `Analytics/Charting.fs`
  with a nm/µm/Å/eV/cm⁻¹ toggle (`MaterialPreview.fs`). Fill the schema `materialEntry`
  `$def` (+ `dispersionModel`/`thermoOptic` `$defs`). Add 16 unit tests; build green at
  Release/x64; all three gates pass (constructor-unit-tests 43, berreman_unit_tests 70 held).
- 2026-05-31 (slice 004, attempt 02 — route-back fixes): Fold the duplicate `"description"`
  key in the schema `dispersionModel` `$def` so the authored model description survives a
  load; lift one `isotropicProperties` helper into `DispersionModels.fs` (Domain), reused
  from `toOpticalProperties`/`toAnisotropicOpticalProperties` and `MaterialImport.fs`
  (reuse-critic F1). Carry the schema↔serializer tag-shape mismatch and the
  closure-vs-`DispersionModel` round-trip gap forward as explicit hand-off risks for the
  materials-wiring slice (Part A/I). All three gates re-run green (build 0 errors,
  unit-tests 70 held, constructor-unit-tests 43 held).

```yaml
gates:
  berreman_unit_tests:    70
  constructor_unit_tests: 43
```
