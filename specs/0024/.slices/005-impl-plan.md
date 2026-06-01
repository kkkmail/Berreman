# 005 impl-plan — Sources panel (Part U4)

## Approach

Mirror the slice-002/003/004 view precedent: a new sibling `SourceView.fs`
that takes its sub-state (the source `SourceSpec` being edited) plus a dispatch
— never `RootModel` — so it composes under `Shell` without a module cycle. All
reducers, presets, and live projections already exist and are frozen (§0.1); the
view only renders and wires them.

The source editor reducer is `SourceEditorView.update : SourceMsg -> SourceSpec
-> SourceSpec` (R-1). The polarization presets are a *separate* seam,
`PolarizationPicker.applyPreset : PolarizationPreset -> SourceSpec -> SourceSpec`
(R-2). Because `applyPreset` mutates `coherence` (Unpolarized) — which no
`SourceMsg` case can express — the presets cannot be decomposed into `SourceMsg`.
So `SourceView` defines a thin local message union wrapping both, dispatched
through the single `Source` `RootMsg` case (matching the materials precedent's
`MaterialsMsg`):

    type SourceViewMsg =
        | Edit of SourceEditorView.SourceMsg
        | ApplyPreset of PolarizationPicker.PolarizationPreset
    let update msg s =
        match msg with
        | Edit m -> SourceEditorView.update m s        // R-1
        | ApplyPreset p -> PolarizationPicker.applyPreset p s   // R-2

The live readout reads `PolarizationPicker.liveStokes`, `poincareMarker`, and
`ellipseParameters` (R-2). The polarization ellipse
(`PolarizationPlots.renderEllipse`, a `ScottPlot.Plot`) is hosted in the slice-002
`ChartHosts.scottPlotHost` (AvaPlot); the Poincaré sphere
(`PolarizationPlots.poincareSphere`, a `Plotly.NET.GenericChart`, built lazily
from `liveStokes`) is hosted in `ChartHosts.webView2Host` (WebView2). Ellipse →
AvaPlot, Poincaré → WebView2 (Non-requirements forbid the swap).

## Files to modify

New:
- `OpticalConstructor.Ui/SourceView.fs` — the sources panel view + `SourceViewMsg`/`update`.
- `OpticalConstructor.Ui.Tests/SourcePanelTests.fs` — headless view tests (AC-U4.1 / AC-U4.2).

Edited:
- `OpticalConstructor.Ui/Shell.fs` — add `source : SourceSpec` to `RootModel` (seeded
  from `SourceEditorView.defaultSource`), add `Source of SourceView.SourceViewMsg`
  `RootMsg` case routed through `SourceView.update`, wire the `sources` panel id to
  `SourceView.sourcePanel`.
- `OpticalConstructor.Ui/OpticalConstructor.Ui.fsproj` — register `SourceView.fs`
  (before `ConstructionView.fs`/`Shell.fs`).
- `OpticalConstructor.Ui.Tests/OpticalConstructor.Ui.Tests.fsproj` — register the test file.

## Risks

- Low–medium. Hosting two different renderer kinds (ScottPlot ellipse vs Plotly
  Poincaré) through the two `ChartHosts` adapters and degrading both gracefully
  under headless. ScottPlot AvaPlot already renders headlessly via `ChartView`
  (slice 002); WebView2 degrades to the §U1.8 placeholder on this TFM (slice 004).
- The preset seam can't round-trip through `SourceMsg` (coherence), so the local
  `SourceViewMsg` wrapper is required — recorded in Gotchas.
