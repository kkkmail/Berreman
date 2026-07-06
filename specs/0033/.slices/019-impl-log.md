# Impl log — spec 0033, slice 019 (IMPLEMENT)

## Progress

- [x] Read task file, worker system prompt (per-anchor IMPLEMENT delta + shared
  base), project prompt, slice spec 019, slice 018 state-of-the-world.
- [x] Scouted all touch points: MaterialPreview.fs helpers, callers
  (MaterialsView.fs:245, MaterialPreviewTests.fs), Domain/Units.fs,
  Analytics/Variables.fs (`Range`, `calculateOpticalProp` grid),
  Controls/ExperimentChart.fs (018 dual-axis spine), TestWindows fsproj,
  Ui.Tests fsproj (already references TestWindows), MaterialLibrary built-ins
  (Silicon dispersive / Vacuum non-dispersive).
- [x] Impl plan written.
- [x] SpectralAxis.fs move (Domain) + fsproj.
- [x] MaterialPreview / MaterialsView / MaterialPreviewTests re-point.
- [x] TestWindows: Analytics reference + NkDispersionChart.fs.
- [x] Ui.Tests: NkDispersionChartTests.fs (+4 model tests).
- [x] Advisory local gate runs (first ui-tests run caught the flat-silicon
  failure → builder re-routed through the engine `getEps` path, see Gotchas;
  all five gates green after).
- [x] State-of-the-world.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/SpectralAxis.fs` — NEW:
  `axisLabel` / `spectralRange` / `axisTicks` moved verbatim from
  `Ui/MaterialPreview.fs` (module `OpticalConstructor.Domain.SpectralAxis`);
  `axisTicks` doc gains the grid-identity note (same `s + (e−s)·i/n` meters grid
  as `RangedVariable.value`, so hosts may zip ticks with engine series).
- `OpticalConstructor.Domain.fsproj` — compile item after `Units.fs`.
- `OpticalConstructor.Ui/MaterialPreview.fs` — the three helpers REMOVED (no
  re-export shim); header doc points at the new home; `plotInput`/`show*` intact.
- `OpticalConstructor.Ui/MaterialsView.fs` — `MaterialPreview.spectralRange` →
  `SpectralAxis.spectralRange` (namespace already open); comment re-pointed.
- `OpticalConstructor.Tests/MaterialPreviewTests.fs` — one added
  `open OpticalConstructor.Domain.SpectralAxis` (moved symbols; tests unchanged).
- `OpticalConstructor.TestWindows.fsproj` — Analytics ProjectReference
  (`..\..\Analytics\Analytics.fsproj`) + `NkDispersionChart.fs` compile item.
- `OpticalConstructor.TestWindows/NkDispersionChart.fs` — NEW: `nkDispersionChart`
  (`OpticalPropertiesWithDisp -> UnitOfMeasure -> Range<WaveLength> ->
  ExperimentChart`; n = Re[√ε₁₁] / k = Im[√ε₁₁] via the engine
  `epsWithDisp.getEps` path sampled on the `SpectralAxis.axisTicks` meter grid
  — NOT `calculateN11Re`/`calculateXi11Im`, see Gotchas — x from
  `SpectralAxis.axisTicks` in the display unit, `xLabel` from
  `SpectralAxis.axisLabel`) + `nkDispersionStyle` (defaultState, k flipped
  RIGHT via `setSeriesAxisSide`) + named `nSeriesIndex`/`kSeriesIndex`.
- `OpticalConstructor.Ui.Tests/NkDispersionChartTests.fs` — NEW (+4 pure
  `ui-tests` cases): the acceptance shape (2 series n/k, shared x grid spanning
  400…800 nm over 41 points, n→LeftAxis / k→RightAxis via the style seed),
  silicon curve, vacuum flat line (n=1, k=0) through the same builder, and the
  AC-D7 display-unit rescale (identical y data in eV vs nm, descending eV axis).
- `OpticalConstructor.Ui.Tests.fsproj` — compile item for the new test file.

## Decisions

- The moved module is `OpticalConstructor.Domain.SpectralAxis` (own file after
  `Units.fs`), matching the Domain namespace+module file pattern.
- `nkDispersionChart` returns the shared `ExperimentChart` (the slice's stated
  codomain); the left/right assignment — which lives in `ChartStyle`'s
  `SeriesStyle.axisSide`, not in the chart data (018 architecture: "the side is
  style, not data") — ships as the paired pure seed `nkDispersionStyle`
  (`defaultState` then `setSeriesAxisSide kSeriesIndex RightAxis`), exactly the
  host pattern 018's state-of-the-world prescribed for this step.

## Testing state

All five roster gates pass in local ADVISORY runs (the arc-runner gate engine
re-runs them authoritatively after exit):

- `build` — exit 0, 0 errors, 94 warnings (the pre-existing MSB3277/FS1125
  noise, same count as slice 018; none from the edited files).
- `unit-tests` — 119 passed, 5 skipped (pre-existing), 0 failed (= baseline).
- `constructor-unit-tests` — 373 passed, 0 failed (= baseline; the
  MaterialPreviewTests open re-point kept every existing case green).
- `ui-smoke` — 59 passed, 0 failed (= baseline).
- `ui-tests` — 263 passed, 0 failed (259 baseline + 4 new NkDispersionChart
  model tests).

Mid-round: the FIRST ui-tests run failed exactly one test — "a dispersive
entry yields a curve" with `silicon n span across the range = 0` — which
exposed the engine seam defect described in Gotchas; after re-routing the
builder through `getEps`, all suites green.

## Artifacts

- `specs/0033/.artifacts/019-build.log`
- `specs/0033/.artifacts/019-unit-tests.log`
- `specs/0033/.artifacts/019-constructor-unit-tests.log`
- `specs/0033/.artifacts/019-ui-smoke.log`
- `specs/0033/.artifacts/019-ui-tests.log` (final green run; the transient
  one-failure run it replaced is described above)

## Gotchas

- **§7 skepticism rule — the slice's named data builders flatten dispersive
  entries.** The spec routed the n/k series through
  `Analytics.Variables.calculateN11Re` / `calculateXi11Im`, but those sample
  wavelengths via `Analytics.getWaveLengthValue` (`Variables.fs:125`), which
  re-wraps the meter magnitude as a nm scalar (`WaveLength.create` interprets
  the raw float in the case's native unit, `Fields.fs:289-292`) — so the
  dispersion is evaluated at λ×10⁻⁹, where silicon's λ² terms vanish below
  double precision and every grid point yields the SAME value (empirical: the
  first advisory run's span was exactly 0). That contradicts the step's own
  acceptance ("dispersive entries yield curves"), and two prior slices already
  documented the same defect and its workaround:
  `SourceSpec.SpectralProfile.sample` (SourceSpec.fs:120-125) and
  `MaterialImport.exportCsv` (MaterialImport.fs:179-181). The builder follows
  that precedent — it walks the canonical meter grid (via
  `SpectralAxis.axisTicks Nanometer`, the same `s + (e−s)·i/n` formula) and
  reads n,k through the engine `epsWithDisp.getEps` seam (exactly `exportCsv`'s
  extraction) — re-deriving NO dispersion formula. Recorded in the module's
  header NOTE as well.
- The same engine defect still sits under the Ui's
  `SeriesData.plotN11Series`…`plotRho33Series` (§H.11) and MaterialsView's
  `dispersionPreview` (both call `calculate*` builders) — pre-existing,
  outside this slice's `touches`, left for a dedicated fix (see Deferred in
  the state-of-the-world). Fixing `getWaveLengthValue` itself would also
  change the SOLVER's wavelength-sweep sampling (`getLight` /
  `getWaveLength`), far beyond this slice's scope.
- `OpticalConstructor.Tests` is not in the slice's `touches`, but
  `MaterialPreviewTests.fs` referenced the moved helpers unqualified; the
  REAL-MOVE therefore required a one-line open re-point there (slice-017
  precedent: callers re-point via opens). No test bodies changed.
- The task file's system-prompt path
  `C:\GitHub\AI-Strategy-Generator\implement_worker.system-md` does not exist
  (same drift as slices 015–018); the real file is
  `AI-Strategy-Generator\src\ai_strategy_generator\multistep\implement_worker.system-md`.
- `.manifest.state.json` (modified) and `.claude/` (untracked) are the
  arc-runner's / harness's own files — left alone, as in slices 001–018.
