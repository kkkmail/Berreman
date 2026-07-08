# Step 016 — impl-plan

## Goal

Replace the primitive hand-rolled FuncUI canvas (`NkDispersionChart.inlineCanvas`)
that both the Material editor's live n/k preview and the Materials-bay View panel
draw through, with an **embeddable form of the shared dual-axis ScottPlot chart**
(the same control the pop-out `ChartWindow` uses), so a dispersion curve here IS the
experiment chart. Delete `inlineCanvas`. Every model kind — including the four
transcendental (step 15) — draws n and k, and the embedded control renders under the
headless `ui-smoke` frame without throwing.

## Approach

1. **Share the ChartWindow rebuild path (`OpticalConstructor.Controls`).** Extract the
   static cartesian dual-axis rendering out of `ChartWindow`'s `do`-block closures into
   a new `ChartPlot` module (in `ChartWindow.fs`, after `ChartRender`): `scottYAxis`,
   `placementAlignment`, `dataBoundsVisible`, `rightAxisInUse`, `applyAxisFonts`,
   `applyLegendAndFonts`, `applyAxisFormat`, `applyAxisLimits`, `applySeriesStyle`,
   `buildCartesianScatters`, and a one-shot `renderCartesian`. `ChartWindow`'s closures
   become thin delegators to `ChartPlot` (behaviour identical — the polar / crosshair /
   interactive parts stay). This keeps the dual-axis ScottPlot mapping in ONE place.

2. **Embeddable AvaPlot host (`OpticalConstructor.Controls/EmbeddedChart.fs`, NEW).**
   `EmbeddedChart.create (autoId) (chart) (style) : IView` — build a `ScottPlot.Avalonia.AvaPlot`,
   render its `Plot` through `ChartPlot.renderCartesian`, and host it in a
   `ContentControl` carrying the automation id (the `ChartHosts.scottPlotHost` pattern:
   a `try/with` that degrades to a bordered "renderer unavailable" placeholder so the
   headless gate never throws). Confirmed headless-safe by the existing
   `ChartWindow constructs and really renders` ui-smoke test (it forces Skia via
   `Plot.GetImage`).

3. **Embed it at both host sites (`OpticalConstructor.TestWindows`).**
   - `MaterialEditorView.previewCanvas` → `EmbeddedChart.create UiIds.previewChart chart (NkDispersionChart.nkDispersionStyle chart)`.
   - `TableAndElementRotationView.materialViewPanel` → `EmbeddedChart.create WorkbenchIds.materialNkChart chart (NkDispersionChart.nkDispersionStyle chart)`.
   The chart is still built by the unchanged `NkDispersionChart.nkDispersionChart` and
   the axis assignment by the unchanged `nkDispersionStyle`.

4. **Delete `NkDispersionChart.inlineCanvas`** (and its private canvas helpers + now-unused
   Avalonia opens); update the module doc.

5. **Headless render tests (`OpticalConstructor.Ui.Tests/EmbeddedChartTests.fs`, NEW,
   `ui-smoke`).** The Material editor preview and the Materials View panel render one
   frame carrying the embedded AvaPlot, for a dispersive (incl. transcendental) and a
   non-dispersive entry; plus a chart-level proof that the embedded chart draws n (left)
   and k (right) and rasterizes for every model kind including the four transcendental.

## Files

- `…Controls/ChartWindow.fs` — add `ChartPlot`; `ChartWindow` closures delegate to it.
- `…Controls/EmbeddedChart.fs` — NEW embeddable host; add to `OpticalConstructor.Controls.fsproj`.
- `…TestWindows/NkDispersionChart.fs` — delete `inlineCanvas` + canvas helpers; trim opens; doc.
- `…TestWindows/MaterialEditorView.fs` — preview embeds the shared chart.
- `…TestWindows/TableAndElementRotationView.fs` — View panel embeds the shared chart.
- `…Ui.Tests/EmbeddedChartTests.fs` — NEW render tests; add to `OpticalConstructor.Ui.Tests.fsproj`.

## Risks

- **Headless native ScottPlot render.** De-risked: `ChartWindow` (an AvaPlot host) is
  already opened + rasterized headlessly in the ui-smoke baseline; the host also
  keeps the `scottPlotHost` graceful-degrade `try/with`.
- **ChartWindow refactor regressing the interactive window.** Mitigated by keeping the
  closures (delegating, behaviour-identical) so every internal call site is unchanged;
  the existing ChartWindow ui-smoke tests (polar toggle, axis edit, series flip,
  GetImage render) still exercise the exact same code.
- **Existing tests keying on the two ids.** `MainWorkbenchTests` asserts
  `materialNkChart` present and `MaterialEditor` tests `Show()` the preview — the host
  carries the same id, so they stay green; the preview now renders an AvaPlot on Show().
