# 012 — Charts & visualization (Part H) — impl-log

## Progress

- [x] Read system prompt, project prompt, slice spec, parent-spec constraints.
- [x] Reflected ScottPlot 5.1.58 + Plotly.NET 5.1.0 public APIs; confirmed every
      call site against the real assemblies / the engine's `Charting.fs` precedent.
- [x] Wrote impl-plan.
- [x] `ChartSettings.fs` — record + projections.
- [x] `SeriesData.fs` — 1D/surface/dispersion adapters.
- [x] `Plot1DView.fs` — 1D/overlay/field-depth/dispersion.
- [x] `Plot3DView.fs` — Plotly surface/contour.
- [x] `Readout.fs` — pure readout tools.
- [x] `PolarizationPlots.fs` — ellipse + Poincaré.
- [x] `CieView.fs` — CIE horseshoe + point.
- [x] Four test files.
- [x] fsproj registrations + Analytics ProjectReference.
- [x] Gates: build, unit-tests, constructor-unit-tests.

## Files modified

New (Ui): `Charts/ChartSettings.fs`, `Charts/SeriesData.fs`, `Charts/Plot1DView.fs`,
`Charts/Plot3DView.fs`, `Charts/Readout.fs`, `Charts/PolarizationPlots.fs`,
`Charts/CieView.fs`.
New (Tests): `SeriesDataTests.fs`, `ReadoutTests.fs`, `ChartSettingsTests.fs`,
`PolarizationPlotTests.fs`.
Edited: `OpticalConstructor.Ui.fsproj`, `OpticalConstructor.Tests.fsproj`.

## Decisions

- **Analytics ProjectReference added to `OpticalConstructor.Ui`.** §H.2/§H.5/§H.11
  require `Analytics.Variables.calculate`/`calculate3D`, `Analytics.Charting.mapFun`
  and the dispersion series builders, and §F.6/§F.8 live in `Analytics`
  (`FieldProfile`/`Colorimetry`). The Ui project did not yet reference Analytics;
  added the ProjectReference. No new C# project, no clone reference (constraint 1/5).
- **SeriesData 1D x-values are the engine's plot-axis values** (`x.plotValue i`,
  nm for wavelength), reproducing exactly what `Charting.plot`'s `getFuncData`
  chooses — required by AC-H1's equivalence assertion.
- **Readout is pure over the `(float*float)[]` array** (Euclidean/x-distance scan),
  not ScottPlot's `GetNearest` (which needs a live render's `RenderDetails`). §H.7
  mandates pure functions over the series; this keeps AC-H6 headless.
- **Log scale**: ScottPlot via `LogMinorTickGenerator` on the axis (no data
  transform, §H.6); Plotly via `StyleParam.AxisType.Log`.

## Testing state

- `build`: PASS — see SoW.
- `unit-tests` (BerremanTests): PASS, unchanged (84) — not touched here.
- `constructor-unit-tests`: PASS — four new test files added over baseline 124.

## Artifacts

Gate captures under `specs/0022/.artifacts/`.

## Attempt 02 — code-judge route-back (two correctness fixes)

The cycle-1 code-judge routed back (gates all green, reuse genuine) for two
confirmed, localized correctness defects in `Charts/ChartSettings.fs`. The reuse
critic's only findings — F1 (`plot3D` surface recipe copy, explicitly sanctioned by
§H.5) and F2 (cosmetic dispersion label strings) — were out of scope to change and
left as-is. Both fixes are surgical; no other behaviour changed.

- [x] **Defect 1 — `xMin/xMax` unit contract.** Kept storage canonical SI
      (R-6/AC-H5). Added `xUnit`/`yUnit : Units.UnitOfMeasure option` (the axis
      *display* unit) to `ChartSettings` (default `None`, so every existing
      `{ defaultValue with … }` construction is unaffected). Added a private
      `toDisplay` helper that routes a stored SI bound through the SOLE §A.10 seam
      `Units.fromMeters`; applied it to `xMin/xMax/yMin/yMax` in BOTH
      `applyToScottPlot` and `applyToPlotly` so the override shares the trace
      x-values' (plot-axis) scale. Fixed the contradictory record doc comment
      (was "stored in the same canonical representation as the plotted series").
- [x] **Defect 2 — `applyToPlotly` title/axis mix-up.** `s.title` was passed as the
      first positional arg of `Chart.withXAxisStyle` (the X-axis *label*) with the
      Y label blanked. Now the title is set via `Chart.withTitle s.title`, and
      `withXAxisStyle`/`withYAxisStyle` carry only range + `AxisType` (no title
      text), leaving the chart builder's axis label intact — matching the engine
      convention (`Charting.fs:32,76`) and the ScottPlot `plot.Title` path.
- [x] **Test.** Added `AC-H5 applyToScottPlot converts an SI xMin/xMax override to
      the axis display unit` in `ChartSettingsTests.fs`: drives a 400–700 nm window
      stored in SI meters with `xUnit = Some Nanometer` through `applyToScottPlot`
      and asserts `plot.Axes.GetLimits()` Left/Right = 400/700, plus that storage
      stays canonical SI. AC-H5 previously only exercised `Units.fromMeters` in
      isolation.

### Attempt-02 files modified

- `OpticalConstructor.Ui/Charts/ChartSettings.fs` — opens (`Berreman.Constants`,
  `OpticalConstructor.Domain`); `xUnit`/`yUnit` fields + `defaultValue`; doc-comment
  fix; `toDisplay` helper; range conversion in both projections; `applyToPlotly`
  title/axis fix.
- `OpticalConstructor.Tests/ChartSettingsTests.fs` — one new override-through-
  projection test.

### Attempt-02 gate results

- `build` (whole `Berreman.slnx`, Release): PASS — 0 errors (pre-existing warnings
  only: MSB3277 WindowsBase/WebView2 unification, FS1125 in `SeriesDataTests`).
- `unit-tests` (BerremanTests): PASS — unchanged (84), core solver untouched.
- `constructor-unit-tests` (`OpticalConstructor.Tests`): PASS — 139 passed, 0 failed
  (was 138; +1 for the new test). Baseline 124 → +15.

## Attempt 03 — resolver issue-hint (inverse-unit axis-bound ordering)

The cycle-2 escalation was a route-back-budget artifact (2/2) on an otherwise
done-green slice. The resolver issued an `issue-hint` naming a single verified
defect, agreed by the code-judge and the architecture-critic and confirmed in
source: `toDisplay` (`ChartSettings.fs`) converts each stored canonical-SI axis
bound independently, and both projections pushed the converted pair in `(min,
max)` *source* order. For the monotone length units (nm/µm/Å/mm) `xMin < xMax`
survives, but the §A.10 maps for the **inverse** units `ElectronVolt` and
`Wavenumber` (`evNmProduct / nm`, `1.0e7 / nm`) are *decreasing*, so an ascending
stored-SI window converts to a *descending* display pair and reached the renderer
as `left > right` (a reversed/empty axis). AC-H5 names nm→eV specifically; the
attempt-02 test only covered the monotone nm path, so the inverse-unit path was
added-but-unexercised.

- [x] **Required fix — order-normalize the converted pair.** Added a tiny private
      `normalizePair : float option -> float option -> (float option * float
      option)` helper (`Some a, Some b -> Some (min a b), Some (max a b)`;
      `None`/single-`Some` pass through unchanged so auto-scale is preserved).
      Applied it to the converted `(xMin,xMax)`/`(yMin,yMax)` pairs in BOTH
      projections before they reach the renderer: `applyToScottPlot`'s
      `plot.Axes.SetLimits` and `applyToPlotly`'s `xMinMax`/`yMinMax` tuples.
- [x] **Required test.** Added `AC-H5 applyToScottPlot keeps inverse-unit (nm to
      eV) limits ascending` in `ChartSettingsTests.fs`: drives the 400–700 nm SI
      window with `xUnit = Some Units.ElectronVolt` through `applyToScottPlot` and
      asserts `limits.Left < limits.Right`, plus that the ends equal the converted
      eV values (700 nm → Left, 400 nm → Right) and storage stays canonical SI.
      This exercises the previously dead inverse-unit path.
- [x] **Optional reuse F2 (taken — low-risk).** Folded the duplicated choose-on-
      `Some` projection shared by `series1D` and `seriesComparison` into one
      private `projectPoints` helper in `SeriesData.fs`. Pure refactor, identical
      behavior; the AC-H1/AC-H2 series tests still pass unchanged.

Out of scope, left for the wiring slice as the hint directs: reuse F1 (`plot3D`
recipe copy, §H.5-sanctioned), F3 (cosmetic dispersion labels), the dead second
visibility pass, the deferred Plotly legend/grid threading, and the hardcoded
`"|E|^2"` field-depth label.

### Attempt-03 files modified

- `OpticalConstructor.Ui/Charts/ChartSettings.fs` — `normalizePair` helper; applied
  in both projections before `SetLimits` / the Plotly `MinMax` tuple.
- `OpticalConstructor.Tests/ChartSettingsTests.fs` — one new nm→eV inverse-unit
  ascending-limits test.
- `OpticalConstructor.Ui/Charts/SeriesData.fs` — `projectPoints` helper; `series1D`
  and `seriesComparison` now share it (reuse F2).

### Attempt-03 gate results

- `build` (whole `Berreman.slnx`, Release): PASS — 0 errors (pre-existing warnings
  only: NU1902 log4net, MSB3277 WindowsBase/WebView2 unification, FS1125 in
  `SeriesDataTests`).
- `unit-tests` (BerremanTests): PASS — 84 passed, 5 skipped, 0 failed (unchanged;
  core solver untouched).
- `constructor-unit-tests` (`OpticalConstructor.Tests`): PASS — 140 passed, 0 failed
  (was 139; +1 for the new inverse-unit test). Baseline 139 → 140.

## Gotchas

See state-of-the-world `Gotchas` (the unit-contract resolution is recorded there).
