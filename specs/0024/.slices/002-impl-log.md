# 002 impl-log — Renderer-host adapters (ChartHosts.fs) + chart panel

## Progress

- [x] R-1 `ChartHosts.fs` — AvaPlot / WebView2(Plotly) / Canvas adapters + placeholder
- [x] R-2 chart panel renders the four `Plot1DView` 1-D charts in the AvaPlot host
- [x] R-3 chart-settings + cursor controls dispatch `Chart`; settings via `applyTo*`
- [x] R-4 3-D surface (`Plot3DView.render`) hosted in the WebView2 adapter
- [x] Shell `RootMsg.Chart` case + dispatch + `chart` panel wired to `ChartView`
- [x] fsproj registration (Ui + Tests)
- [x] `ChartPanelTests.fs` (`ui-tests`)
- [x] Gates green (build, unit-tests, constructor-unit-tests, ui-smoke, ui-tests)

## Files modified

New:
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/ChartHosts.fs` — the shared renderer-host
  seam: `scottPlotHost` (AvaPlot), `webView2Host` (Plotly→WebView2, lazy), `canvasHost`
  (Canvas), and the `unavailable` placeholder. §U1.8 graceful degradation; §0.5 control
  kept host-local; hosted via `ContentControl.create` (R-1).
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/ChartView.fs` — chart panel: the four
  `Plot1DView.*` charts in the AvaPlot host, the 3-D `Plot3DView.render` in the WebView2
  host (through `applyToPlotly`), a settings/cursor toolbar, and the panel-local
  `ChartMsg` + pure `update` over `(ChartSettings * Readout.Markers)`.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/ChartPanelTests.fs` — `ui-tests`:
  mount + simulated legend/cursor clicks dispatch the expected `ChartMsg`; `update` routes
  a settings edit into `ChartSettings` and a cursor placement into `Readout.Markers`.

Edited:
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/Shell.fs` — added `RootMsg.Chart of
  ChartView.ChartMsg`; `update` delegates to `ChartView.update` over `(model.chart,
  model.markers)`; threaded `dispatch` through `pageBody`/`constructionBody`/`panelView`/
  `panelContent` and wired the `chart` panel id to `ChartView.chartPanel`.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/OpticalConstructor.Ui.fsproj` — registered
  `ChartHosts.fs`, `ChartView.fs` (before `ConstructionView.fs`/`Shell.fs`).
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/OpticalConstructor.Ui.Tests.fsproj` —
  registered `ChartPanelTests.fs`.

No frozen module edited (§0.1): the `Charts/` builders, `Workspace`, `ConstructionPage`,
etc. are only called.

## Decisions

- **WebView2 degrades to placeholder on this TFM (§U1.8).** The `Microsoft.Web.WebView2`
  package contributes NO compile-time reference under the Ui project's `net10.0` TFM (its
  control assemblies are net462/Windows-desktop only — verified: its `compile` asset set is
  empty for `net10.0` in `project.assets.json`), and the headless platform has no Evergreen
  runtime. There is therefore no Avalonia-hostable WebView2 to bind today, so `webView2Host`
  returns the §U1.8 "unavailable" placeholder (the path AC-U3.1 explicitly allows). The
  `toEmbeddedHTML → NavigateToString` mechanism (R-1) is documented at its call site as the
  deferred Windows-desktop `NativeControlHost` WebView bridge. The chart is passed lazily so
  the placeholder path never pays for the upstream 3-D sweep. Recorded per the "don't ask"
  rule; an operator may move the app to a Windows-desktop TFM to enable functional hosting.
- **`AvaPlot.Plot` swap via `Reset(Plot)`.** `AvaPlot.Plot` has no public setter; the
  pre-built `ScottPlot.Plot` is injected with `Reset(plot)` (the ScottPlot 5 `IPlotControl`
  plot-replacement seam), then `Refresh()` (AC-U5.1).
- **Representative sweep inputs.** The chart view renders real curves through the
  `Plot1DView.*` seams using representative inputs built from the reused Analytics seams
  (`StandardSystems.transparentGlassFilm` + `IncidentLightInfo.create` +
  `StandardLightVariables`), held at module scope so the sweep solve runs once. The
  model-driven sweep — the `OpticalSystem -> FixedInfo` source lift (U4) and the effectful
  sweep `Cmd` / JobRunner (U7) — is out of this slice's scope and stays deferred. The
  field-depth profile and dispersion series are small literal render inputs (no physics
  re-derived) until their panels are wired.
- **All four 1-D charts rendered.** R-2 lists `renderSweep`/`renderComparison`/
  `renderFieldDepth`/`renderDispersion`; the panel renders all four, each in its own
  fixed-height `AvaPlot`-host row (scrollable), so every render function is exercised.
- **Settings reach the plot only through the projections (R-3).** Each 1-D chart is rebuilt
  from the current `ChartSettings` per render via the `Plot1DView.*` seams (which call
  `applyToScottPlot`); the 3-D chart goes through `applyToPlotly`. The view re-applies no
  axis/legend styling itself.
- **No `ctx.useState` / chart-kind selector.** The panel renders all four kinds rather than a
  stateful selector, keeping `ChartView` a plain sub-state→view function (no `RootModel`
  dependency, mirroring `ConstructionView.stackPanel`) and avoiding view-local state churn.

## Testing state

`commit_ready: true`. Every R-1..R-4 requirement plus the Shell wiring is addressed this
round (nothing deferred to a "round 2"; the deferred items are later parts' scope, not this
slice's requirements). Local gate run (per-gate captures under `.artifacts/`):

- build: succeeded, 0 errors (21 pre-existing MSB3277 `WindowsBase`/WebView2 version-conflict
  warnings only — not errors; inherited, not introduced).
- unit-tests (BerremanTests): 84 passed / 5 skipped. Baseline `berreman_unit_tests = 84`.
- constructor-unit-tests: 204 passed; stays Avalonia-free.
- ui-smoke: 1 passed — the real `App` lifetime renders every panel headlessly; the new
  AvaPlot host renders a frame without throwing (ScottPlot's draw op no-ops on the headless
  null-Skia lease), and the WebView2 host shows its placeholder.
- ui-tests: 3 passed — the slice-001 stack-panel test plus the two new chart-panel tests
  (simulated legend/cursor clicks dispatch `ChartMsg`; `update` routes settings → `ChartSettings`
  and cursor → `Readout.Markers`).

## Artifacts

- `C:\GitHub\Berreman\specs\0024\.artifacts\002-ui-smoke.log` — ui-smoke gate capture.

## Gotchas

- **`AvaPlot.Plot` is get-only; use `Reset(Plot)`** to swap in a pre-built plot. A direct
  `ava.Plot <- plot` fails to compile (FS0491, non-public setter).
- **WebView2 is unbindable on `net10.0`.** Confirmed via `project.assets.json` (`compile=[]`
  for the package) — the `lib_manual` Windows-desktop assemblies are only surfaced as runtime
  references (the source of the MSB3277 `WindowsBase` warnings), not compile references. So no
  WebView2 type can be referenced from the Ui project on this TFM; the adapter degrades.
- **Headless AvaPlot render is safe.** The risk that `AvaPlot` throws during the headless
  render pass (outside the construction try/catch) did not materialise: ScottPlot 5's Avalonia
  draw op guards on a null Skia lease and no-ops under `Avalonia.Headless`
  (`UseHeadlessDrawing=true`). No headless probe was needed; verified by a green `ui-smoke`.
- **`Refresh()` via recreate-per-render.** `scottPlotHost` builds a fresh `AvaPlot` and calls
  `Refresh()` each render rather than retaining a `createWithOutlet` handle; this satisfies
  "Refresh() on model change" simply. Handle retention is a later efficiency option.
