# 012 — Charts & visualization (Part H) — state of the world

## Where we are

Slice 012 delivers Part H — the chart presentation layer of `OpticalConstructor.Ui`,
on top of the slice-001 buildable FuncUI interpretation, the Part A sweeps seam
(`Analytics.Variables`/`Charting`), and Part F (slice 008) §F.6 field-depth /§F.8
color. It adds seven `Charts/*.fs` modules driven by one immutable `ChartSettings`
record (the §A.7 chart-settings slot) and one renderer-neutral `SeriesData` adapter:
ScottPlot 5 for the interactive 1D / overlay / field-depth / dispersion traces and
the polarization ellipse, Plotly.NET 5.1 (WebView2-hosted at runtime) for the
surface/contour view and the Poincaré sphere, and a CIE 1931 view. Part H is
complete; later slices (Part I §I.7 export, Part J wiring) build on the
`ChartSettings`/`SeriesData`/view seam.

## What's working

- Add `Charts/ChartSettings.fs` (§H.1): the immutable `ChartSettings` record
  (axis ranges, per-axis `AxisScale`, title, legend/grid flags, per-trace
  `TraceSettings` over the engine `OpticalFunction` DU) plus the SOLE
  `applyToScottPlot`/`applyToPlotly` projections and the pure mapping helpers.
- Add `Charts/SeriesData.fs` (§H.2): renderer-neutral 1D/comparison/surface/
  field-depth/dispersion adapters reusing `calculate`/`calculate3D`/`mapFun` and the
  engine dispersion builders — no solver re-implementation, no `Chart.show`.
- Add `Charts/Plot1DView.fs` (§H.3/H.4/H.9/H.11): ScottPlot 1D traces, overlay via
  `plotComparison`'s layout, field-depth and dispersion traces; pure of
  `ChartSettings` + series, ScottPlot's built-in zoom/pan, no browser tab.
- Add `Charts/Plot3DView.fs` (§H.5): Plotly surface/contour reusing `plot3D`'s
  axis-swap and `Contours.initXyz(Show = true)`; Plotly only, no OpenTK.
- Add `Charts/Readout.fs` (§H.7): pure nearest-point / peak / min / FWHM /
  two-cursor-delta over the series array; markers as plain MVU-model values.
- Add `Charts/PolarizationPlots.fs` (§H.8): ScottPlot ellipse from
  `ellipticityR`/`azimuthR` + Plotly Poincaré sphere from normalized `stokesR`.
- Add `Charts/CieView.fs` (§H.10): CIE 1931 horseshoe (static locus table) + the
  supplied `{ x; y; rgb }` point; no color computed here.
- Add four test files (`SeriesDataTests`, `ReadoutTests`, `ChartSettingsTests`,
  `PolarizationPlotTests`) and an `OpticalConstructor.Ui`→`Analytics`
  ProjectReference; register the seven modules and four tests in their `.fsproj`s.
- Attempt 02 — pin the `ChartSettings.xMin/xMax` unit contract: storage stays
  canonical SI, add `xUnit`/`yUnit : Units.UnitOfMeasure option` (axis display
  unit) and convert each bound through the §A.10 `Units.fromMeters` seam inside
  BOTH projections so the range override shares the trace x-values' scale.
- Attempt 02 — fix `applyToPlotly`: set the chart title via `Chart.withTitle` and
  stop passing it as the X-axis label, leaving the real axis label intact
  (matching `Charting.fs:32,76` and the ScottPlot `plot.Title` path).
- Attempt 03 — order-normalize the converted axis-bound pair in BOTH projections
  (`normalizePair` helper) before it reaches the renderer, so the inverse units
  `ElectronVolt`/`Wavenumber` no longer push `left > right`; cover the nm→eV
  inverse-unit path with a new AC-H5 ascending-limits test. Fold the duplicated
  choose-on-`Some` projection in `SeriesData` into one `projectPoints` helper.
- All three gates green (build 0 errors; BerremanTests 84; constructor 140).

## Tests

- `build` gate: PASS — `dotnet build Berreman.slnx -c Release`, 0 errors (19 pre-existing
  warnings — log4net advisory, WindowsBase/WebView2 unification, MathNet SYSLIB0051).
- `unit-tests` gate (`BerremanTests`): PASS — 84 passed, 5 skipped, 0 failed
  (baseline 84, unchanged — this slice does not touch the core solver).
- `constructor-unit-tests` gate (`OpticalConstructor.Tests`): PASS — 140 passed,
  0 failed (baseline 139 → 140): SeriesDataTests ×3 (AC-H1/AC-H3), ReadoutTests ×5
  (AC-H6), ChartSettingsTests ×6 (AC-H4/AC-H5, incl. the attempt-02
  override-through-`applyToScottPlot` axis-limits test and the attempt-03 nm→eV
  inverse-unit ascending-limits test), PolarizationPlotTests ×2 (AC-H7).

```yaml
gates:
  berreman_unit_tests:    84
  constructor_unit_tests: 140
```

## Architecture

- **One immutable `ChartSettings`, two pure projections.** `applyToScottPlot` and
  `applyToPlotly` are the ONLY place axis/legend/grid/trace-visibility reach a
  renderer (§H.1). Views build only the visible traces (`visibleTraces`); the
  ScottPlot projection additionally flips `Scatter.IsVisible` by legend name. No
  parallel settings container; no schema versioning / migration / per-chart mutable
  config (out of scope, §H.1/§H.6).
- **`SeriesData` reuses the engine data extraction, never the solver.** 1D series
  reproduce `Charting.plot`'s exact choose-on-`Some` projection over
  `Variables.calculate`; the surface adapter applies `Charting.mapFun` to a
  calculate3D run it is handed (`surfaceFromData`) so Part H consumes whatever array
  it is given; dispersion series reuse `Variables.calculateN11Re`…`calculateRho33Im`
  (the bodies behind `plotN11`…`plotRho33`) which already end in `(float*float)[]`,
  dropping only the terminal `Chart.show`.
- **`OpticalConstructor.Ui` now references `Analytics`.** §H.2/H.5/H.11 need
  `Variables`/`Charting`, and §F.6/§F.8 (`FieldProfile`/`Colorimetry`) live in
  `Analytics`. Added the ProjectReference (no new C# project, no FuncUI-clone
  reference — constraints 1/5 intact).
- **Renderer split honored.** ScottPlot for fast native interactive 2D; Plotly.NET
  (WebView2 at runtime) for the surface/contour and the Poincaré sphere. The custom
  OpenTK viewport is reserved by Part A and is NOT used in charts (§H.5).
- **Units stay canonical SI; the projection converts the range override at the
  boundary.** Axis ranges are stored canonical SI in `ChartSettings` (R-6/AC-H5);
  the trace x-values are plot-axis display units (nm/degrees). `xUnit`/`yUnit`
  carry the axis display unit, and `applyToScottPlot`/`applyToPlotly` route each
  stored bound through the SOLE §A.10 `Units.fromMeters` seam (private `toDisplay`
  helper) before pushing it, so the override and the traces share one scale. A
  `None` unit (angle / dimensionless axis) passes the bound through unchanged.
  `ChartSettings` stores no non-SI value and Part H re-implements no conversion.
- **`applyToPlotly` mirrors the ScottPlot `title`/axis split.** The chart title is
  set via `Chart.withTitle s.title`; `withXAxisStyle`/`withYAxisStyle` carry only
  range + `AxisType`, leaving the axis label set by the chart builder/view intact
  (Plotly.NET 5.1 `TitleText` is optional) — the same convention the engine uses
  at `Charting.fs:32,76` and the ScottPlot `plot.Title` path uses.

## Deferred

- **Plotly legend / grid toggling in `applyToPlotly`.** Plotly.NET 5.1's `Chart`
  exposes no lowercase `withLegend(bool)`; the projection applies axis ranges + the
  log/linear `AxisType` and leaves Plotly's default legend. ScottPlot (the gated,
  interactive 2D path) carries the full legend/grid/visibility projection. Wiring a
  Plotly `Legend` object is a cosmetic follow-up, out of the minimum (§0 #6).
- **Live WebView2 hosting + FuncUI control wiring.** This slice produces the
  `ScottPlot.Plot`/`GenericChart` values and the pure projections; mounting them in
  Avalonia/WebView2 controls and binding to a chart-page MVU model is the later
  UI-wiring slice (the page model holds the `ChartSettings` and `Readout.Markers`).
- **Chart export (CSV/PNG/SVG)** is Part I §I.7 (slice 013) — Part H produces the
  series/handles, not the export files.

## Gotchas

- **`calculate3D` parallelises over rows (`PSeq`) and is not row-order-stable across
  two independent runs.** The surface adapter is therefore split into
  `surfaceFromData` (over a given calculate3D array) and a `surface` convenience; the
  AC-H3 test shares ONE calculate3D run between `mapFun` and the adapter, exactly the
  "fixed calculate3D run" the slice's test plan calls for. Do not assert equality
  across two separate calculate3D invocations.
- **1D series x-values are the engine's plot-axis units, not SI; the range override
  is SI and gets converted in the projection.** `Variables.calculate` emits
  `x.plotValue i` (nm for a wavelength sweep, degrees for an angle sweep), so
  `SeriesData` carries those values to match `Charting.plot` (AC-H1). The
  `ChartSettings` axis-range overrides are canonical SI; the projection converts
  them to `xUnit`/`yUnit` via `Units.fromMeters` so they land on the trace scale.
  The two conventions are intentionally distinct but reconciled at the projection
  boundary — set `xUnit` to the unit the trace x-values are displayed in (e.g.
  `Nanometer` for a wavelength sweep) whenever you set `xMin`/`xMax`, or the
  override will be off-scale. (Resolved in attempt 02; the field doc comment at the
  `ChartSettings` record previously contradicted this.)
- **ScottPlot 5 has no first-class log axis; Log10 is a log MINOR-tick generator.**
  `applyToScottPlot` sets `NumericAutomatic.MinorTickGenerator = LogMinorTickGenerator`
  on the axis (assignable to `ITickGenerator`; `LogMinorTickGenerator` alone is only
  an `IMinorTickGenerator` and cannot be assigned directly). No data transform is
  applied (§H.6). Plotly's `Log` axis is genuine.
- **`Readout` is pure over the array, not ScottPlot's `GetNearest`.** `GetNearest`
  needs a live render's `RenderDetails`; §H.7 mandates pure functions over the
  series, so nearest-point is a Euclidean scan — keeping AC-H6 headless.
- **Inverse axis units (eV, cm⁻¹) flip bound order; the projections renormalize.**
  `Units.fromMeters` is monotone increasing for the length units (nm/µm/Å/mm) but
  *decreasing* for `ElectronVolt` (`evNmProduct / nm`) and `Wavenumber`
  (`1.0e7 / nm`). So an ascending stored-SI `(xMin, xMax)` window converts to a
  *descending* display pair, and a renderer handed `left > right` shows a reversed
  or empty axis. Both projections now order-normalize the converted pair via the
  `normalizePair` helper (`Some (min a b), Some (max a b)`) before pushing it to
  `plot.Axes.SetLimits` / the Plotly `MinMax` tuple; `None`/single-`Some` pass
  through so auto-scale is preserved. If you add a new inverse-direction display
  unit, this normalization already covers it — do NOT re-sort in the views.
- **`Plotly.NET.GenericChart` is the type name** for the `applyToPlotly` signature
  (not `GenericChart.GenericChart`, which is the module).
- The `Analytics.Charting`/`Variables` lowercase `Chart.*` combinators (`Line`,
  `combine`, `Surface`, `withXAxisStyle`, …) are the F#-merged Plotly.NET `Chart`
  surface; they compile under `open Plotly.NET` exactly as the engine's `Charting.fs`
  uses them (same 5.1.0 pin) even though plain CLR reflection of `Plotly.NET.Chart`
  does not list them.

## Changelog

- 2026-06-01 — Slice 012 (Part H §H.1–H.11): added the seven `Charts/*.fs` modules
  (`ChartSettings`, `SeriesData`, `Plot1DView`, `Plot3DView`, `Readout`,
  `PolarizationPlots`, `CieView`) to `OpticalConstructor.Ui`, four test files to
  `OpticalConstructor.Tests`, and an `OpticalConstructor.Ui`→`Analytics`
  ProjectReference; registered all in their `.fsproj`s. Implemented the immutable
  `ChartSettings` slot + the sole `applyToScottPlot`/`applyToPlotly` projections, the
  renderer-neutral `SeriesData` adapter over the engine sweeps/charting/dispersion
  data builders, the ScottPlot 1D/overlay/field-depth/dispersion view, the
  Plotly-in-WebView2 surface view, the pure readout tools, the polarization ellipse +
  Poincaré sphere, and the CIE 1931 view — no solver re-run, no `Chart.show`, no
  FuncUI-clone or OpenTK in charts. All three gates green (build 0 errors;
  BerremanTests 84 passed; constructor-unit-tests 138 passed, baseline 124 → +14).
- 2026-06-01 — Slice 012 attempt 02 (code-judge route-back): pinned the
  `ChartSettings.xMin/xMax` unit contract (storage canonical SI; new
  `xUnit`/`yUnit` axis-display-unit fields; `applyToScottPlot`/`applyToPlotly`
  convert each bound through the §A.10 `Units.fromMeters` seam; fixed the
  contradictory record doc comment) and fixed `applyToPlotly` to set the title via
  `Chart.withTitle` instead of overloading the X-axis label. Added one
  `ChartSettingsTests` case driving an `xMin/xMax` override through
  `applyToScottPlot` and asserting the resulting `plot.Axes` limits. All three
  gates green (build 0 errors; BerremanTests 84; constructor-unit-tests 139 passed,
  baseline 124 → +15).
- 2026-06-01 — Slice 012 attempt 03 (resolver issue-hint): order-normalize the
  converted axis-bound pair in BOTH `applyToScottPlot` and `applyToPlotly` (new
  private `normalizePair` helper) before it reaches `plot.Axes.SetLimits` / the
  Plotly `MinMax` tuple, so the inverse units `ElectronVolt`/`Wavenumber` no longer
  reach the renderer as `left > right`. Added an AC-H5 nm→eV inverse-unit test
  asserting ascending `Left < Right`, exercising the previously dead inverse-unit
  path. Folded the duplicated choose-on-`Some` projection in `SeriesData` into one
  `projectPoints` helper (reuse F2). All three gates green (build 0 errors;
  BerremanTests 84; constructor-unit-tests 140 passed, baseline 139 → 140).
