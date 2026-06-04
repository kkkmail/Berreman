# 004 impl-plan — Materials panel (Part U3)

## Approach

Render and wire the `materials` panel over the **existing** data + plot builders;
introduce no new compute, no new search index, and edit no frozen module (§0.1).
This mirrors the slice-002/003 view precedent: a new sibling `*View.fs` module that
takes its sub-state plus a dispatch (never `RootModel`, to avoid a module cycle), with
the root `Shell` threading the wrapped dispatch.

Three requirements:

- **R-1 Browse + filter** — list `MaterialLibrary` entries, filtered through the
  existing `MaterialLibrary.byCategory` / `byNameContains` seams. Intersect the two
  filters in plain code (no new index, Non-requirement). Filter state (search text,
  category, selected id) is a pure/serializable sub-model (`MaterialsView.Filter`) held
  in `RootModel`, driven by a new `MaterialsMsg` + pure `update` — exactly the
  `ChartView` shape.
- **R-2 Dispersion preview in WebView2 (Plotly)** — build the chart via
  `MaterialPreview.showN11` (returns `Plotly.NET.GenericChart`) and host it through
  `ChartHosts.webView2Host` (lazy, never an `AvaPlot`). Headlessly it degrades to the
  §U1.8 "unavailable" placeholder (AC-U3.1).
- **R-3 Drag-drop onto a layer row** — material entries are real Avalonia drag sources
  (`DragDrop.DoDragDrop` carrying the stable material id); the selected node's layer
  rows are drop targets (`DragDrop.allowDrop` + `onDrop`). The drop handler routes the
  single seam `StackEditor.layerMaterialDrop` and dispatches the resulting
  `Construction (EditStack …)`. The view resolves nothing itself
  (`layerMaterialDrop`→`resolveMaterial` is the only resolution seam). The drop logic is
  exposed as `MaterialsView.materialDrop` so the headless test drives it (AC-U3.2).

## Active wavelength

`resolveMaterial` / the preview need a `WaveLength`. The model carries no single active
wavelength yet (the source-lift is a later part). Use the seed source's 550 nm light,
`Templates.defaultLight.waveLength` — the one canonical light the project is seeded from
— and record the deferral in Gotchas.

## Files

- **New** `OpticalConstructor.Ui/MaterialsView.fs` — the panel view + `Filter`/`MaterialsMsg`/`update` + `materialDrop` seam.
- **New** `OpticalConstructor.Ui.Tests/MaterialsPanelTests.fs` — headless list/filter render, WebView2-routing (no AvaPlot, AC-U3.1), drop→EditStack thickness-unchanged (AC-U3.2), plus a pure `update`/filter test.
- **Edit** `OpticalConstructor.Ui/OpticalConstructor.Ui.fsproj` — register `MaterialsView.fs` (after `ChartView.fs`, before `Shell.fs`).
- **Edit** `OpticalConstructor.Ui/Shell.fs` — add `materialsFilter` to `RootModel`, a `Materials` `RootMsg` case, its `update` delegation, and route the `materials` panel id to `MaterialsView.materialsPanel`.
- **Edit** `OpticalConstructor.Ui.Tests/OpticalConstructor.Ui.Tests.fsproj` — register the new test file.

## Risks

- Low–medium. Data + builders exist. Main worry: the WebView2-hosted Plotly preview
  degrading gracefully headless (it does — `ChartHosts.tryHostPlotly` returns `None` on
  this TFM and never forces the lazy chart) and routing through `ChartHosts` not an
  `AvaPlot`. Real drag-drop gestures cannot be raised headlessly, so the drop logic is
  tested via the `materialDrop` seam (the `onDrop` handler is a thin wrapper over it),
  mirroring how `ChartPanelTests` tests `ChartView.update` directly.
