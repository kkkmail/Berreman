# 002 state-of-the-world — Renderer-host adapters (ChartHosts.fs) + chart panel

```yaml
gates:
  berreman_unit_tests: 84
  constructor_unit_tests: 204
  ui_smoke: 1
  ui_tests: 3
```

# Where we are

Slice 002 carries Part U5 of the spec-0024 UI-wiring arc and is sequenced second
(right after the U1 foundation) because its `ChartHosts.fs` is the shared
renderer-host seam U3/U4/U6/U8 MUST reuse — the seam has to land before its
consumers. It delivers that seam plus the `chart` panel itself (the primary results
surface), wired into the slice-001 root MVU `Shell`.

# What's working

- Add `ChartHosts.fs`: the shared AvaPlot / WebView2(Plotly) / Canvas renderer-host
  adapters, each hosting via `ContentControl.create`, keeping its control host-local
  (§0.5), and degrading to an "unavailable" placeholder when its runtime is absent (§U1.8).
- Add `ChartView.fs`: the chart panel renders the four `Plot1DView` 1-D charts in the
  AvaPlot host and the 3-D `Plot3DView.render` surface in the WebView2 host, with a
  chart-settings + on-plot-cursor toolbar.
- Route applied settings to the plots only through `applyToScottPlot`/`applyToPlotly`;
  cursor placements edit `Readout.Markers` held in the model.
- Add the `Chart` `RootMsg` case to `Shell.fs` and delegate it to the chart panel's
  pure `ChartView.update`; thread `dispatch` to the `chart` panel.
- Add the `ui-tests` chart-panel headless view test (mount + simulated edits dispatch
  `ChartMsg`; `update` routes settings/cursor) and keep every gate green.

# Tests

All gates in the slice roster pass locally:

- `build` — solution builds, 0 errors (only pre-existing MSB3277 WindowsBase/WebView2
  version-conflict warnings, inherited not introduced).
- `unit-tests` (BerremanTests) — 84 passed / 5 skipped; baseline `berreman_unit_tests = 84`.
- `constructor-unit-tests` — 204 passed; stays Avalonia-free (headless dep is isolated to
  the UI test project).
- `ui-smoke` — 1 passed: every panel/page (incl. the new `chart` panel with its AvaPlot
  host) renders one frame headlessly without throwing; the WebView2 host shows its
  placeholder.
- `ui-tests` — 3 passed: the slice-001 stack-panel test plus two new chart-panel tests
  (simulated legend/cursor clicks dispatch `ChartMsg`; `ChartView.update` routes a settings
  edit into `ChartSettings` and a cursor placement into `Readout.Markers`, AC-U5.1/AC-U5.2).

# Architecture

- `ChartHosts.fs` is the single hosting seam: three adapters returning `IView`, each with a
  uniform `unavailable` placeholder fallback. Native control instances live in the host
  layer (a local in the adapter), never in the root model (§0.5). U3/U4/U6/U8 reuse these
  and MUST NOT embed their own hosting.
- `ChartView.fs` follows the slice-001 sub-view precedent (`ConstructionView.stackPanel`): it
  takes the chart sub-state (`ChartSettings` + `Readout.Markers`) and a `ChartMsg -> unit`
  dispatch, not `RootModel`, so it composes under `Shell` without a module cycle. It owns the
  panel-local `ChartMsg` + pure `update`, which `Shell.update` delegates to (mirroring how
  `Shell` delegates to `ConstructionPage.update`).
- The 1-D charts are rebuilt from the current `ChartSettings` on every render via the
  `Plot1DView.*` seams (which call `applyToScottPlot`); the 3-D chart is routed through
  `applyToPlotly`. No axis/legend styling is re-applied in the view (R-3 / Non-requirements).
- The WebView2 chart is passed to the host lazily, so the placeholder path never forces the
  upstream `calculate3D` 3-D sweep.

# Deferred

- Functional WebView2 hosting (`toEmbeddedHTML → NavigateToString`): no Avalonia-hostable
  WebView2 binding exists on the Ui project's `net10.0` TFM (the package's control assemblies
  are net462/Windows-desktop only and contribute no compile reference), so the adapter
  degrades to the §U1.8 placeholder. Functional hosting lands behind a Windows-desktop TFM +
  `NativeControlHost` WebView bridge.
- The model-driven sweep: the `OpticalSystem -> FixedInfo` source lift (U4 / sources) and the
  effectful sweep `Cmd` + JobRunner wiring (U7). This slice renders representative sample
  sweeps through the real seams to prove the rendering path now.
- A live on-plot mouse-cursor pick (the toolbar's "Mark cursor at peak" stands in headlessly);
  ScottPlot pointer-event wiring is a runtime-interaction follow-up.
- Sweep/series caching and a `createWithOutlet` Refresh-handle retention (current path rebuilds
  the AvaPlot per render).

# Gotchas

- **`AvaPlot.Plot` is get-only.** Swap a pre-built plot with `Reset(Plot)`, not `ava.Plot <-`
  (the latter fails to compile, FS0491 non-public setter), then `Refresh()`.
- **WebView2 unbindable on `net10.0`.** Verified via `project.assets.json` (`compile=[]` for
  the package); the Windows-desktop `lib_manual` assemblies surface only as runtime references
  (the MSB3277 `WindowsBase` warnings). No WebView2 type can be referenced from the Ui project
  on this TFM — hence the placeholder.
- **Headless AvaPlot render is safe.** ScottPlot 5's Avalonia draw op guards on a null Skia
  lease and no-ops under `Avalonia.Headless` (`UseHeadlessDrawing=true`), so the chart panel
  renders a frame without throwing — no headless probe was needed (the construction try/catch
  in `scottPlotHost` covers ctor failures).
- **`ui_tests` baseline rose 1 → 3** (the two new chart-panel tests); the next slice's
  baseline should track this.

# Architecture decisions

(See `Architecture` above; no further decisions this round.)

# Changelog

- 2026-06-01 — Slice 002: added the shared renderer-host seam `ChartHosts.fs` (AvaPlot /
  WebView2 / Canvas adapters with §U1.8 graceful degradation and §0.5 host-local controls)
  and the `chart` panel `ChartView.fs` (the four `Plot1DView` 1-D charts in the AvaPlot host,
  the 3-D `Plot3DView` surface in the WebView2 host, settings routed through
  `applyToScottPlot`/`applyToPlotly`, on-plot cursors editing `Readout.Markers`); added the
  `Chart` `RootMsg` case + dispatch in `Shell.fs` and a `ui-tests` chart-panel view test.
  WebView2 functional hosting deferred (no Avalonia-hostable binding on `net10.0`; degrades to
  the §U1.8 placeholder). All slice gates green (build 0 errors; berreman 84, constructor 204,
  ui-smoke 1, ui-tests 3).
