# 012 — Charts & visualization (Part H) — impl-plan

## Approach

Add the chart presentation layer of `OpticalConstructor.Ui` as seven new
`Charts/*.fs` modules driven by one immutable `ChartSettings` record and one
renderer-neutral `SeriesData` adapter over the engine sweep seam. No view
re-runs the solver; every renderer setting goes through the two
`applyToScottPlot`/`applyToPlotly` projections. ScottPlot 5 (native 2D) renders
the 1D/overlay/field-depth/dispersion traces and the polarization ellipse;
Plotly.NET 5.1 (hosted in WebView2 at runtime) renders the surface/contour view
and the Poincaré sphere.

Build order (matches the §H dependency chain):
`ChartSettings → SeriesData → Plot1DView / Plot3DView / Readout /
PolarizationPlots / CieView`.

## Files to add

- `Charts/ChartSettings.fs` — `ChartSettings`/`AxisScale`/`LineStyle`/`TraceSettings`;
  pure `applyToScottPlot`/`applyToPlotly` and the pure mapping helpers
  (`visibleTraces`, `scottPlotLinePattern`, `scottPlotColor`, `plotlyAxisType`).
- `Charts/SeriesData.fs` — `Series1D`/`Surface` adapters over `Variables.calculate`,
  `calculate3D`, `Charting.mapFun`, and the `plotDispersion`/`plotNxx`/`plotRhoxx`
  bodies (re-expressed to renderer-neutral `(float*float)[]`).
- `Charts/Plot1DView.fs` — ScottPlot 1D trace assembly, overlay (`plotComparison`
  layout), field-depth (§F.6) and dispersion (§H.11) traces.
- `Charts/Plot3DView.fs` — Plotly.NET surface/contour reusing `plot3D`'s axis-swap
  + `Contours.initXyz(Show = true)`.
- `Charts/Readout.fs` — pure nearest-point / peak / min / FWHM / two-cursor-delta
  over the `Series1D` array.
- `Charts/PolarizationPlots.fs` — ScottPlot polarization ellipse (engine
  `ellipticityR`/`azimuthR`) + Plotly Poincaré sphere (normalized `stokesR`).
- `Charts/CieView.fs` — CIE 1931 horseshoe (static locus table) + supplied
  `{ x; y; rgb }` point.
- Four test files (`SeriesDataTests`, `ReadoutTests`, `ChartSettingsTests`,
  `PolarizationPlotTests`).

## Files to edit

- `OpticalConstructor.Ui.fsproj` — register the seven `Charts/*.fs` in build order.
  Needs an `Analytics` ProjectReference (for `Variables`/`Charting`/`FieldProfile`/
  `Colorimetry`) — currently absent.
- `OpticalConstructor.Tests.fsproj` — register the four new test files.

## Risks / decisions

- **API surface (highest risk).** ScottPlot 5.1.58 and Plotly.NET 5.1.0 named
  APIs verified by reflection up front: `Plot.Add.Scatter(double[],double[])`,
  `Plot.Axes.SetLimits(Nullable…×4)`, `Plot.ShowLegend/HideLegend`, `Legend.IsVisible`,
  `Grid.IsVisible`, `Plot.Title/XLabel/YLabel`, `Scatter.LegendText/IsVisible/Color/
  LineWidth/LinePattern`, `Color.FromHex`, `LinePattern.{Solid,Dashed,Dotted}`,
  `TickGenerators.LogMinorTickGenerator()`, `IAxis.TickGenerator` (settable). Plotly
  `Chart.{Line,combine,Surface,withXAxisStyle,withYAxisStyle,withZAxisStyle,
  withDescription}` are proven by the engine's own `Analytics/Charting.fs` (same
  5.1.0 pin); `Chart.Point3D`/`StyleParam.AxisType.Log` reused from the same
  `Chart3D`/`StyleParam` types.
- **`calculate` x-values are engine plot units (nm for wavelength), not SI.** The
  SeriesData 1D adapter reproduces exactly what `Charting.plot` emits
  (`x.plotValue i`), so AC-H1 equivalence holds. `ChartSettings` axis-range
  overrides are stored canonical-SI and converted for display only through the
  §A.10 `Units` seam (AC-H5).
- **ScottPlot 5 log axis.** Applied as `LogMinorTickGenerator` on the axis (no data
  transform, per §H.6); Plotly log is the genuine `AxisType.Log`.
- **Readout is pure over arrays**, not ScottPlot `GetNearest` (which needs a live
  `RenderDetails`), satisfying §H.7's "pure functions over the series" mandate and
  keeping AC-H6 headlessly testable.
- **No clone, no OpenTK in charts** (Plot3DView is Plotly only), no `Chart.show`.

## Attempt 02 — code-judge route-back (two correctness fixes)

Attempt 01 was green on all gates; the code-judge routed back for two confirmed,
localized defects in `Charts/ChartSettings.fs`. The reuse critic's only findings
(F1 `plot3D` recipe copy — spec-sanctioned by §H.5; F2 cosmetic dispersion labels)
are explicitly *not* to be changed. This attempt is a surgical fix of exactly the
two defects; no other behaviour changes.

**Defect 1 — `xMin/xMax` unit contract.** The doc comment (`ChartSettings.fs:39-41`)
said ranges match the plotted series (plot-axis nm/deg), while R-6/AC-H5 and the
test store canonical SI meters, and `applyToScottPlot` pushed the stored value into
`SetLimits` with no §A.10 conversion — so an SI override (~5e-7) lands ~9 orders off
an nm-scaled trace, and the range-through-projection path was never exercised. Fix:
keep storage canonical SI; add `xUnit`/`yUnit : Units.UnitOfMeasure option` (the
axis *display* unit, defaulting `None`); add a private `toDisplay` helper that
converts a stored SI bound to the display unit via the §A.10 seam
`Units.fromMeters`; apply it to `xMin/xMax/yMin/yMax` in *both* projections so the
override shares the trace x-values' scale; fix the doc comment.

**Defect 2 — `applyToPlotly` title/axis mix-up.** `s.title` was passed as the
first positional arg of `Chart.withXAxisStyle` (the X-axis *label*) and the Y-axis
label blanked with `""`. Fix: set the chart title with `Chart.withTitle s.title`,
and call `withXAxisStyle`/`withYAxisStyle` without a title text so the real axis
label set by the chart builder survives (Plotly.NET 5.1 `TitleText` is optional) —
matching `Charting.fs:32,76` and the ScottPlot `plot.Title` path.

**Test.** AC-H5 today only exercises `Units.fromMeters` in isolation. Add one test
driving an `xMin/xMax` override (SI meters, `xUnit = Some Nanometer`) through
`applyToScottPlot` and asserting `plot.Axes.GetLimits()` Left/Right equal the nm
display values, while `ChartSettings` storage stays canonical SI.

**Files:** `Charts/ChartSettings.fs`, `Tests/ChartSettingsTests.fs` only.

## Attempt 03 — resolver issue-hint (inverse-unit axis-bound ordering)

Attempt 02 added the §A.10 `toDisplay` conversion in both projections but left one
defect, verified by the code-judge + architecture-critic and confirmed in source:
`toDisplay` (`ChartSettings.fs:118-121`) converts each stored canonical-SI bound
**independently**, and both projections push the converted pair in `(min, max)`
*source* order — `applyToScottPlot` via `SetLimits` (135-139), `applyToPlotly` via
the `xMinMax`/`yMinMax` tuples (170-177). For the monotone length units
(nm/µm/Å/mm) `xMin < xMax` survives, but for the **inverse** unit maps
`ElectronVolt` and `Wavenumber` (`evNmProduct / nm`) an ascending SI window
converts to a *descending* pair and reaches the renderer as `left > right` (a
reversed/empty axis). AC-H5 names nm→eV specifically; attempt-02's test only
covers the monotone nm path, so the inverse-unit path is added-but-unexercised.

**Required fix.** After converting each bound through `toDisplay` in BOTH
projections, normalize the converted pair (`min`/`max`) before handing it to
`SetLimits` / the Plotly `MinMax` tuple. Expressed once as a tiny private
`normalizePair` helper used by both; `None`/single-`Some` cases pass through
unchanged (auto-scale semantics preserved).

**Required test.** Add an AC-H5 case driving a nm→eV `xMin`/`xMax` window
(`xUnit = Some Units.ElectronVolt`) through `applyToScottPlot` and asserting
`limits.Left < limits.Right`, exercising the inverse-unit path; storage stays SI.

**Optional (low-risk) reuse F2.** Fold the duplicated choose-on-`Some` projection
shared by `series1D` and `seriesComparison` into one private `projectPoints`
helper in `SeriesData.fs` — pure refactor, identical behavior; abandon if it
risks the gates.

**Out of scope (wiring slice):** reuse F1 (`plot3D` recipe copy, §H.5-sanctioned),
F3 (cosmetic dispersion labels), dead second visibility pass, deferred Plotly
legend/grid threading, hardcoded `"|E|^2"` label.

**Files:** `Charts/ChartSettings.fs`, `Tests/ChartSettingsTests.fs` (required);
`Charts/SeriesData.fs` (optional F2).
