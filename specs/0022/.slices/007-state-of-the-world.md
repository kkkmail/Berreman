# 007 — State of the world: Light sources & illumination (Part E)

## Where we are

Slice 007 lands Part E: the source/illumination layer of the constructor. It sits
on the slice-002 domain foundation and the engine's canonical `IncidentLightInfo`,
adding the net-new orchestration record `SourceSpec` (wrapping exactly one
`IncidentLightInfo`), the [Standard] expand-on-the-boundary machinery (spectral
profiles, cone fans, coherent/incoherent/unpolarized flags, multi-source weighted
averaging, Gaussian-beam angular spectrum), the [Core] source editor and the
bidirectional Poincaré/ellipse picker (both Avalonia-free seams per the §0/P3
mandate and the slice-005/006 precedent), and extends the `OpticalConstructorProject`
aggregate with `sources : SourceSpec list`. Every [Standard] feature fans a base
`IncidentLightInfo` into weighted `IncidentLightInfo` values and averages on the
solver's `StokesVector`/intensity OUTPUTS, never on `EmField` amplitudes. Part F
(slice 008) consumes the `RangedVariable` values produced here through
`calculate`/`calculate3D`; slice 016 delivers the OpenTK viewport the E.4 picker
consumes.

## What's working

- Add the source domain in `SourceSpec.fs`: the immutable `SourceSpec` record (one wrapped `IncidentLightInfo` + orchestration-only fields), the `SourceAxis`/`SpectralProfile`/`StandardIlluminant`/`ConeAcceptance`/`Coherence`/`GaussianBeamSpec` DUs, and the pure `toIncidentLight`/`SpectralProfile.sample`/`ConeAcceptance.sample`/`GaussianBeamSpec.angularSpectrum`/`expand` functions over the canonical `IncidentLightInfo`.
- Add `SourceCombination.fs`: the weighted incoherent `StokesVector` reducer (`StokesVector.(+)`/`Zero`), parameterised by a `toStokes` solve closure so it never sums `EmField` values across sources.
- Add the Avalonia-free [Core] source editor (`Sources/SourceEditorView.fs`): `SourceMsg`/`update` rebuilding `IncidentLightInfo` via the engine constructors, wavelength entry through the sole `Units.toWaveLength` seam, Fixed/Ranged axis helpers reusing the `StandardLightVariables` presets, and the one-click Source palette entry.
- Add the Avalonia-free polarization picker (`Sources/PolarizationPicker.fs`): s/p/45/RCP/LCP/unpolarized presets, the bidirectional azimuth/ellipticity write-back, the live `EmField.stokesVector` readout, and the Poincaré-marker/ellipse coordinates the reserved OpenTK viewport consumes.
- Add `SpectralImport.fs` (Storage): FSharp.Data CSV → `(WaveLength * float)` list in canonical meters for `ImportedSpectrum`, never an FsPickler `.binz`.
- Extend `OpticalConstructorProject` with `sources : SourceSpec list`; add `SourceProjectionTests.fs` + `SourceExpansionTests.fs`. constructor-unit-tests rise 77 → 106; build green at Release/x64; `BerremanTests` held at 70.

## Tests

- `build` — PASS (`Build succeeded.`, `0 Error(s)`; no lowercase `error` token). Log `.artifacts/007-build.log`.
- `unit-tests` — PASS (Passed 70 / Skipped 5 / Total 75; `BerremanTests` untouched). Baseline `berreman_unit_tests = 70` held. Log `.artifacts/007-unit-tests.log`.
- `constructor-unit-tests` — PASS (Passed 107 / Failed 0 / Total 107; up from slice-006 baseline 77). Log `.artifacts/007-constructor-unit-tests.log`.

Coverage: AC-E1 (`toIncidentLight` returns the entered λ/angle/pol/ellipticity, λ in
meters; normal incidence via `IncidentLightInfo.create`); AC-E2 (Å/mm/eV/cm⁻¹ display
units land on `WaveLength.Nm` via the §A.10 `Units` conversions, no non-SI unit
stored; nm/µm native); AC-E3 (Ranged axis yields the matching
`WaveLengthRange`/`IncidenceAngleRange` handed unchanged, Fixed yields none); AC-E4
(RCP/LCP set `Ellipticity.create ±1`, unpolarized sets `Coherence.Unpolarized` not a
sentinel, live Stokes via `EmField.stokesVector`, Poincaré marker on the unit
sphere); AC-E5 (`SpectralProfile.sample` Flat/Blackbody/LaserLine in canonical
meters; imported spectrum read from CSV via FSharp.Data, not `.binz`); AC-E6 (cone
fans N symmetric sub-angles AVERAGED — each sub-angle weight 1/N, per-sample weights
sum to 1, not summed; single-sample/collimated `cone = None` collapse to the base
angle at unit weight); AC-E7 (Unpolarized expands to the incoherent s/p average; Incoherent
routes through the reused substrate `Multiple` branch); AC-E8 (multi-source weighted
incoherent `StokesVector` sum via `(+)`/`Zero`, empty list → `Zero`, no field-level
summation); AC-E9 (Gaussian angular spectrum via `createGaussian` + `FourierTransform.fft`,
normalised, peak near-axis, wider waist → narrower spread); AC-E10 (one-click Source
palette entry, no confirmation gate). None deferred.

## Architecture

- **Expand-on-the-boundary, average on output.** `SourceSpec.expand` composes the
  §E.6 cone fan, the §E.9 Gaussian angular fan, the §E.7 unpolarized s/p split and
  the §E.2/§E.8 per-source intensity weight into a `(IncidentLightInfo * float) list`;
  `SourceCombination` reduces the solved `StokesVector` outputs with
  `StokesVector.(+)`/`Zero`. No `EmField` is ever summed across samples/sources.
- **All three sub-fans normalise (AC-E6).** Because `SourceCombination.combine` is a
  non-normalising weighted sum, each sub-fan must carry weights that AVERAGE: the cone
  fan weights each of its N angles by 1/N, the Gaussian spectrum sums to 1, and the
  unpolarized split is 0.5/0.5. A collimated source (N=1) reduces to unit weight;
  per-source weights then sum to the source `intensity`. (The attempt-01 cone branch
  emitted weight 1.0 per angle and so SUMMED an N-sample cone to N×; the attempt-02 fix
  applied the 1/N factor only in the no-Gaussian branch. Attempt-03 hoists the 1/N
  per-cone-angle factor so it ALSO applies when a Gaussian beam is set: the `Some beam`
  branch previously laid a unit-sum Gaussian fan over each of the N cone angles without
  the 1/N factor, so a `cone={samples=N}` + `gaussianBeam=Some` source summed to N
  instead of 1. Now both branches normalise; the AC-E9 cone+Gaussian test asserts the
  per-source weights total the source `intensity`, not N.)
- **`SourceSpec` wraps exactly one `IncidentLightInfo`.** The base monochromatic
  state lives in `light` (its five fields are never duplicated); orchestration-only
  fields (`displayUnit`, `intensity`, `spectralProfile`, `cone`, `gaussianBeam`,
  `coherence`) have no engine equivalent. `intensity` is a unit-scalar per-source
  weight, not threaded into `IncidentLightInfo`.
- **No forked engine primitive.** Wavelength conversion routes through the sole
  `Units.toWaveLength` seam; the Gaussian angular spectrum reuses `createGaussian`
  (`FourierTransform.fs:74`) + `FourierTransform.fft` (`FourierTransform.fs:137`);
  the incident field/Stokes readout reuses `EmField.create` + `EmField.stokesVector`;
  the incoherent path reuses the existing substrate-driven `Multiple` solve.
- **UI seams are Avalonia-free.** Following `StackEditor.fs`/`ConstructionPage.fs`,
  the two `Sources/*` modules are pure `SourceSpec` projection/update seams carrying
  no Avalonia type; the FuncUI `view` body is deferred to a later UI-wiring slice.
- **`SourceCombination` is solver-decoupled.** It takes a
  `toStokes : IncidentLightInfo -> StokesVector` closure, so Part F supplies the
  actual solve-and-read-Stokes wiring without this layer depending on the solver.
- **Compile order.** `SourceSpec.fs`/`SourceCombination.fs` now compile after the
  units/beam-tree spine but BEFORE `Project.fs`, because the aggregate references
  `SourceSpec`.

## Deferred

- The sweep execution / charting / `calculate`/`calculate3D` wiring and the
  range-based spectral sampling consumed by it — Part F (slice 008). `expand` carries
  no wavelength range, so it folds in only the range-free `LaserLine` of §E.5; the
  range-based Flat/Blackbody/Illuminant sampling is `SpectralProfile.sample`, consumed
  by the sweep layer.
- CIE / photometric conversion of spectra — Part F §F.8 (slice 008).
- The 3D Poincaré OpenTK viewport delivery — slice 016; E.4 only produces the marker
  coordinates the reserved viewport consumes.
- The full CIE D65 daylight S0/S1/S2 series — D65 is approximated by a 6504 K
  Planckian (Illuminant A is exactly 2855.54 K). Out of [Standard] minimum scope.
- Tightening the `sourceSpec` JSON-Schema `$def` (it stays permissive, slice-003
  storage core) and any `.binz` sidecar for expanded source lists.

## Gotchas

- **`Analytics.getWaveLengthValue` mis-scales `.value`.** It wraps the meter
  magnitude from `RangedVariable.value` back through `WaveLength.create` (which
  multiplies by `1.0<nm>`), so the resulting `WaveLength.value` is off by `nmToMeter`.
  `SpectralProfile.sample` therefore samples the `WaveLengthRange` directly in meters
  and re-wraps as `WaveLength.Nm (m / nmToMeter)` (the `MaterialImport.exportCsv`
  precedent). Do NOT route canonical-meter wavelengths through `getWaveLengthValue`.
- **`EmField.stokesVector` returns `StokesVector option`.** It is computed on the
  complex basis via `thread` (`FieldFunctions.fs:55`); a well-formed incident field
  always has one, so `liveStokes`/the test closure default to `StokesVector.Zero`.
- **`ConeAcceptance.sample` does NOT clamp.** It adds `Angle` offsets via
  `IncidenceAngle.(+)` (`Fields.fs:33`), so a fan around normal yields signed angles
  (e.g. -10°..+10°); it does not route through `IncidenceAngle.create` (which would
  wrap/clamp into [0, π/2)). This is intentional — the fan must straddle the axis.
- **`GaussianBeamSpec.angularSpectrum` returns angle MAGNITUDES.** Because
  `IncidenceAngle.create` clamps to [0, π/2), symmetric ±bins map to the same
  magnitude; the divergence half-angle bounds the fan and weights normalise to 1.
  The angular spread scales as 1/waist (correct physics) even though the far
  high-angle bins carry negligible Gaussian weight.
- **`[<Measure>] K` is net-new.** The engine `Constants.fs` has no Kelvin; the
  measure lives in `SourceSpec.fs` and traces to §E.5's `Blackbody of double<K>`.
- **Adding `sources` to the aggregate touches every literal.** F# records need all
  fields, so the four `OpticalConstructorProject` literals (3 test files) gained
  `sources = []`; `ConstructionPage` uses a `with`-copy and was unaffected.

## Changelog

- 2026-06-01 (slice 007): Add the Part E source/illumination domain — `SourceSpec.fs`
  (`SourceSpec` + the `SourceAxis`/`SpectralProfile`/`ConeAcceptance`/`Coherence`/
  `GaussianBeamSpec` DUs and the pure `toIncidentLight`/`sample`/`expand`/
  `angularSpectrum` functions), `SourceCombination.fs` (weighted incoherent
  `StokesVector` reducer), the Avalonia-free [Core] source editor and bidirectional
  Poincaré/ellipse picker (`Sources/`), `SpectralImport.fs` (FSharp.Data spectrum
  CSV), and the aggregate's `sources` field — all over reused engine seams
  (`Units.toWaveLength`, `createGaussian`+`FourierTransform.fft`, `EmField.create`+
  `stokesVector`, the substrate `Multiple` path) and unit-tested. Add
  `SourceProjectionTests.fs` (AC-E1..E4, E10) and `SourceExpansionTests.fs`
  (AC-E5..E9). All three gates green: build 0 errors, unit-tests 70 held,
  constructor-unit-tests 77 → 106.
- 2026-06-01 (slice 007, retry 02): Fix the §E.6 cone-fan weighting in
  `SourceSpec.expand` — the no-Gaussian branch now averages (weight 1/N per
  sub-angle) instead of summing (weight 1.0), so an N-sample cone no longer produces
  N× the Stokes/intensity of a collimated source (was skewing the AC-E8 multi-source
  sum by the same factor). Add the missing per-sample weight assertions to the AC-E6
  cone test (1/7 each, sum 1). Guard `SpectralProfile.sample` against
  `numberOfPoints = 0` with `max 1`. All three gates stay green; counts unchanged
  (build 0 errors, unit-tests 70, constructor-unit-tests 106).
- 2026-06-01 (slice 007, retry 03): Fix the §E.6/§E.9 cone+Gaussian normalisation in
  `SourceSpec.expand` — the `Some beam` branch laid a unit-sum Gaussian fan over each
  of the N cone angles without the per-cone-angle 1/N factor the no-Gaussian branch
  applied, so a source with BOTH `cone={samples=N}` and `gaussianBeam=Some` summed to
  weight N instead of 1. Hoist the `coneWeight = 1/N` factor and multiply each
  Gaussian-fan weight by it, so the combined source totals the source `intensity` (1),
  matching the cone-only and unpolarized 0.5/0.5 normalisations. Add one AC-E9 test
  asserting a cone+Gaussian source's expanded per-source weights sum to `intensity`,
  not N. Edited only `SourceSpec.fs` and `SourceExpansionTests.fs`. All three gates
  stay green (build 0 errors, unit-tests 70, constructor-unit-tests 106 → 107).

```yaml
gates:
  berreman_unit_tests:    70
  constructor_unit_tests: 107
```
