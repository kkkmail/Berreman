# 002 impl-plan — Renderer-host adapters (ChartHosts.fs) + chart panel

## Approach

Deliver the shared renderer-host seam (`ChartHosts.fs`) every later panel
(U3/U4/U6/U8) reuses, plus the `chart` panel view (`ChartView.fs`) and its wiring
into the slice-001 `Shell.fs` MVU surface.

### ChartHosts.fs (R-1, the reusable seam)

Three small adapter views, public FuncUI DSL only, each degrading to a graceful
"unavailable" placeholder (§U1.8) so the headless `ui-smoke` gate renders a frame
without throwing, and each keeping its live control in the host layer (§0.5):

- `scottPlotHost : ScottPlot.Plot -> IView` — creates `ScottPlot.Avalonia.AvaPlot`
  (an `Avalonia.Controls.Control`), assigns `.Plot`, calls `.Refresh()`, hosts it via
  `ContentControl.create`. Construction guarded by try/catch → placeholder.
- `plotlyHost : Plotly.NET.GenericChart -> IView` — the WebView2 adapter. The
  `Microsoft.Web.WebView2` control assemblies are net462/-windows only and contribute
  **no** compile-time reference under this project's `net10.0` TFM (verified in
  `project.assets.json`: `compile=[]`), and the headless platform has no Evergreen
  runtime, so there is no Avalonia-hostable WebView2 to bind today. The adapter
  therefore degrades to the §U1.8 placeholder. The `toEmbeddedHTML → NavigateToString`
  wiring is documented at its call site as the deferred Windows-desktop bridge.
- `canvasHost : IView list -> IView` — a pure-Avalonia `Canvas` host for 2-D geometry
  (U6/U8). Canvas needs no native runtime, so it never degrades.

### ChartView.fs (R-2/R-3/R-4, the chart panel)

A FuncUI `Component` taking the chart sub-state (`ChartSettings` + `Readout.Markers`)
and a `ChartMsg -> unit` dispatch — NOT `RootModel` (avoids a cycle with `Shell.fs`,
mirroring `ConstructionView.stackPanel`'s sub-model signature). Defines its own
`ChartMsg` + pure `update` over `(ChartSettings * Markers)`; `Shell.update` delegates.

- A view-local chart-kind selector (`ctx.useState`, §0.5 view-layer state) chooses
  which 1-D chart renders into the `AvaPlot` host: `Plot1DView.renderSweep` /
  `renderComparison` / `renderFieldDepth` / `renderDispersion` (R-2 — all four
  reachable). The chart is rebuilt from `model.chart` each render so the applied
  settings reach the plot through `applyToScottPlot` (R-3 / AC-U5.2) and `Refresh()`
  fires on model change (AC-U5.1). The view re-applies NO axis styling itself.
- A 3-D surface (`Plot3DView.render`) routed through `applyToPlotly model.chart`
  (R-3) into the WebView2 host (R-4) — degrades to placeholder.
- Chart-settings controls (legend/grid toggles, title) dispatch `ChartMsg` editing
  `ChartSettings` (R-3). A "mark cursor" control dispatches `ChartMsg` editing
  `Readout.Markers` via `Readout.peak` (R-3 / AC-U5.2).
- Sweep inputs are representative samples built from the reused Analytics seams
  (`StandardSystems` / `StandardLightVariables` / `IncidentLightInfo.create`), held
  at module scope. The real model-driven sweep (the `OpticalSystem -> FixedInfo`
  source lift + the effectful sweep `Cmd`) is U4/U7 work and stays deferred.

### Shell.fs (edit)

Add `RootMsg.Chart of ChartView.ChartMsg`; `update` delegates to `ChartView.update`
over `(model.chart, model.markers)`; `panelContent "chart"` renders `ChartView.chartPanel`.

### Tests / fsproj

- Register `ChartHosts.fs`, `ChartView.fs` in `OpticalConstructor.Ui.fsproj` (after
  the Charts block; ChartView before Shell).
- Add `ChartPanelTests.fs` (`Category=ui-tests`): mount the chart panel over a known
  state, assert controls mount + a simulated click dispatches a `ChartMsg`, and assert
  `update` cursor placement updates `Markers` / a settings edit reaches `ChartSettings`.
- The existing `ui-smoke` test already renders every visible panel (incl. `chart`),
  so it covers the headless one-frame render of the new adapters.

## Files

- New: `ChartHosts.fs`, `ChartView.fs`, `OpticalConstructor.Ui.Tests/ChartPanelTests.fs`.
- Edited: `Shell.fs`, `OpticalConstructor.Ui.fsproj`, `OpticalConstructor.Ui.Tests.fsproj`.

## Risks

- **Headless AvaPlot render.** `AvaPlot` wraps a Skia draw op; if it throws during the
  headless render pass (outside the construction try/catch) the `ui-smoke` gate fails.
  Mitigation: verify empirically by running `ui-smoke`; if it throws, gate the live
  control behind a headless probe and fall back to the placeholder.
- **WebView2 unbindable on net10.0.** Accepted: adapter degrades to placeholder per
  §U1.8; full hosting deferred to a Windows-desktop WebView bridge.
- **Frozen modules.** No edit to any `Charts/` builder or other frozen module (§0.1);
  they are only called.
