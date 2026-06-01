# 004 — Impl-plan: Materials, dispersion models & spectral preview (Part D §D.5–D.12)

## Approach

Fill the four placeholder modules slice 001 scaffolded (`DispersionModels.fs`,
`MaterialLibrary.fs` in Domain; `MaterialImport.fs` in Storage; `MaterialPreview.fs`
in Ui) plus the schema `materialEntry` `$def`, and add the test files. Everything
reuses the engine seams (no forks): `Eps.fromComplexRefractionIndex`, the
`OpticalPropertiesWithDisp` closure shape that `Silicon`/`Langasite` build, the
`Units.fs` conversion seam (slice 002), and the `Analytics/Charting.fs` dispersion
plots. The placeholders are already registered in fsproj compile order
(`… DispersionModels.fs → MaterialLibrary.fs`), so no compile-order surgery is needed
in Domain.

## Files

1. **`DispersionModels.fs`** (Domain) — net-new `[<Measure>] K`; `ThermoOptic`
   record; one coefficient record per analytic model (Sellmeier, Cauchy, Lorentz,
   Drude, TaucLorentz, GaussianOscillator, ConstantNK) each carrying an explicit
   `wavelengthUnit : UnitOfMeasure` and `thermoOptic : ThermoOptic option`; a
   wrapping `DispersionModel` DU; `evaluate`/`evaluateAt` returning the
   `WaveLength -> ComplexRefractionIndex` closure (reading `WaveLength.value`,
   normalising via `Units.fromMeters`, no literal factors); `toOpticalProperties`
   composing into `EpsWithDisp`/`EpsWithoutDisp`; the anisotropic three-index path
   through `Eps.fromComplexRefractionIndex (n1,n2,n3)`.

2. **`MaterialLibrary.fs`** (Domain) — `MaterialCategory` DU, `MaterialEntry`
   record (id/name/category/description/`OpticalPropertiesWithDisp`), `MaterialLibrary`
   record, `byCategory`/`byNameContains` (linear `List.filter`), the by-id
   `resolveMaterial : MaterialLibrary -> string -> WaveLength -> Result<OpticalProperties, MaterialError>`
   seam (slices 005/014 delegate here), and built-in entries wrapping
   `siliconOpticalProperties`/`langasiteOpticalProperties` and the `Standard` glass
   presets (no re-derivation).

3. **`MaterialImport.fs`** (Storage) — `importRefractiveIndexInfo`/`importCsv`
   (raw text → `Result<MaterialEntry, ImportError>`; λ reduced to meters via
   `Units.toMeters` per the file's declared unit; linear-in-λ interpolation),
   `exportCsv` (samples n,k over a `Range<WaveLength>` via `getEps`). FSharp.Data
   `CsvFile` parses the numeric tables; a small line scanner reads the
   refractiveindex.info YAML keys.

4. **`MaterialPreview.fs`** (Ui) — `spectralRange` (canonical `Range<WaveLength>`
   from user endpoints via `Units.toWaveLength`), `axisLabel`/`axisTicks` (display
   relabel/rescale via `Units.wavelengthToUnit`), `plotInput` (the unit-independent
   `(properties, range)` handed to the engine plots — AC-D7), and thin wrappers
   onto `plotN11`/`plotXi11`/… (no re-implementation of plotting).

5. **`optical-constructor-project.schema.json`** — fill the `materialEntry` `$def`
   dispersion sub-object (id/name/category/description + a tagged `dispersion`
   object enumerating the coefficient records + optional `thermoOptic`), without
   re-shaping the slice-003 envelope.

6. **Tests** — `DispersionModelsTests.fs`, `MaterialImportTests.fs`,
   `MaterialPreviewTests.fs` + fixtures `Si.yml`, `sample-nk.csv`; register in the
   Tests fsproj.

## Risks / decisions

- **AC-D4 contradiction.** Silicon's engine closure (`Dispersive.fs:78-80`) is a
  bespoke polynomial in λ, NOT a Sellmeier form, so a `Sellmeier` model "whose
  coefficients match Silicon's closure inputs" cannot agree with `Silicon()` to
  1e-9. Per the system-prompt skepticism rule I implement Sellmeier faithfully and
  cover the intent two ways: (a) `evaluate` reproduces a hand-built Sellmeier
  closure of the same shape; (b) the silicon library entry reproduces
  `Silicon().opticalProperties` bit-for-bit (reuse, not re-derivation). Recorded in
  Gotchas.
- **Thermo-optic byte-identity.** `thermoOptic = None` returns the base closure
  directly (no `+0.0`); `Some` wraps with `dndT·(T−ref)` applied at the operating
  `T` supplied at the evaluation boundary. `T` is never a record field, so it cannot
  be persisted; only `dndT`/`referenceTemperature` are stored.
- **Import string = content.** `importRefractiveIndexInfo`/`importCsv` take the raw
  file text (symmetric with `exportCsv` returning text), keeping IO/exceptions out
  of the parser and the error channel pure `Result<_, ImportError>`.

## Attempt 02 (retry) — narrow fixes only

The code-judge routed back (verdict route-back-to-worker) for two cheap fixes; this
round is ONLY those two plus a SoW hand-off note. No new requirements, no re-scope.

1. **Schema duplicate-`description` key.** The `dispersionModel` `$def` carried two
   `"description"` members (one after `"type":"object"`, one after the `properties`
   block); JSON last-wins silently dropped the authored model description on load.
   Fix: fold both texts into the single first `"description"` and remove the
   duplicate, so the model description survives a load.

2. **Reuse-critic F1 — one `isotropicProperties` helper.** Lift a single
   `isotropicProperties (epsWithDisp : EpsWithDisp) -> OpticalPropertiesWithDisp`
   into `DispersionModels.fs` (Domain — the lower layer; Domain cannot depend on
   Storage). Call it from `toOpticalProperties` (both arms) and
   `toAnisotropicOpticalProperties`, replacing the three inline
   `Mu.vacuum.dispersive`/`Rho.vacuum.dispersive` literals; delete the private copy
   in `MaterialImport.fs` and reuse the Domain helper via
   `open OpticalConstructor.Domain.DispersionModels`. The vacuum-μ/ρ convention now
   lives at one site.

**NOT in scope (deferred — carried forward in the SoW hand-off):** the
schema↔serializer tag-shape mismatch (`ProjectJson.options` emits adjacent-tag
`Case`/`Fields`, not the `kind`-tagged shape the `$def` documents) and the
closure-vs-`DispersionModel` round-trip gap (`MaterialEntry` keeps an opaque
`OpticalPropertiesWithDisp`, not the originating `DispersionModel`). Both belong to
the slice that wires a `materials` field into the aggregate (Part A/I); nothing
serializes through this `$def` yet.
