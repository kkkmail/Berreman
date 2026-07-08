# Step 016 — impl-log

## Progress

- [x] `ChartPlot` module extracted in `…Controls/ChartWindow.fs`; `ChartWindow` delegates.
- [x] `…Controls/EmbeddedChart.fs` NEW embeddable AvaPlot host + fsproj compile item.
- [x] `…TestWindows/NkDispersionChart.fs` — `inlineCanvas` + canvas helpers deleted; opens trimmed; doc.
- [x] `…TestWindows/MaterialEditorView.fs` — preview embeds the shared chart.
- [x] `…TestWindows/TableAndElementRotationView.fs` — View panel embeds the shared chart.
- [x] `…Ui.Tests/EmbeddedChartTests.fs` NEW render tests (6, ui-smoke) + fsproj compile item.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Controls/ChartWindow.fs` — NEW
  `[<RequireQualifiedAccess>] module ChartPlot` (after `ChartRender`): the shared cartesian
  dual-axis rendering (`scottYAxis`, `placementAlignment`, `dataBoundsVisible`, `rightAxisInUse`,
  `applyAxisFonts`, `applyLegendAndFonts`, `applyAxisFormat`, `applyAxisLimits`, `applySeriesStyle`,
  `buildCartesianScatters`, one-shot `renderCartesian`). `ChartWindow`'s `applyLegendAndFonts` /
  `applyAxisFormat` / `applyAxisLimits` / `applySeriesStyle` closures now delegate to `ChartPlot`
  (behaviour identical — they still read the mutable `style`/`scatters`/`polar`), and the
  `rebuildPlot` cartesian branch uses `ChartPlot.buildCartesianScatters`. The five closures the new
  module absorbed (`scottYAxis`/`placementAlignment`/`dataBoundsVisible`/`rightAxisInUse`/`applyAxisFonts`)
  are removed from the `do`-block.
- `Berreman/OpticalConstructor/OpticalConstructor.Controls/EmbeddedChart.fs` — NEW. `EmbeddedChart.create
  (autoId) (chart) (style) : IView` builds a `ScottPlot.Avalonia.AvaPlot`, renders its `Plot` through
  `ChartPlot.renderCartesian`, and hosts it in a `ContentControl` carrying the automation id, inside a
  `try`/`with` that degrades to a bordered placeholder (the `ChartHosts.scottPlotHost` seam).
- `Berreman/OpticalConstructor/OpticalConstructor.Controls/OpticalConstructor.Controls.fsproj` —
  `EmbeddedChart.fs` compile item after `ChartWindow.fs`.
- `Berreman/OpticalConstructor/OpticalConstructor.TestWindows/NkDispersionChart.fs` — deleted the
  primitive `inlineCanvas` + its private canvas geometry helpers (`canvasWidth`/`marginLeft`/… /`color`/
  `brush`/`automationId`); dropped the now-unused Avalonia opens (`Avalonia*`, `Avalonia.FuncUI.*`); the
  pure chart-model builders `nkDispersionChart` / `nkDispersionStyle` are UNCHANGED; module doc updated.
- `Berreman/OpticalConstructor/OpticalConstructor.TestWindows/MaterialEditorView.fs` — `previewCanvas`
  now embeds `OpticalConstructor.Controls.EmbeddedChart.create UiIds.previewChart chart (nkDispersionStyle chart)`.
- `Berreman/OpticalConstructor/OpticalConstructor.TestWindows/TableAndElementRotationView.fs` — the
  Materials View panel embeds `EmbeddedChart.create WorkbenchIds.materialNkChart chart (nkDispersionStyle chart)`.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/EmbeddedChartTests.fs` — NEW. Six ui-smoke
  render proofs (see below) + fsproj compile item.

## Retry (attempt 02) — build gate fix

The attempt-01 build gate failed with THREE `error`-level diagnostics, all confined to the NEW test
file `OpticalConstructor.Ui.Tests/EmbeddedChartTests.fs` (the production code — `ChartPlot`,
`EmbeddedChart.fs`, the `inlineCanvas` deletion, both consumers — compiled clean):

1. `FS0893` (warning-as-error): `open Elmish` resolved to `Avalonia.FuncUI.Elmish` through a partial
   path, because the file also carried a bare `open Avalonia.FuncUI` (which nests an `Elmish` module).
2. `FS0039`: `window.MouseDown(...)` — the type `Window` has no `MouseDown`.
3. `FS0039`: `window.MouseUp(...)` — the type `Window` has no `MouseUp`.

**Fix** (test-file only; no production change this round): matched the opens of the proven-green
`MainWorkbenchTests.fs`, which uses the exact same `Component` + `Program.mkSimple/withHost/run`
combination:
- Added `open Avalonia.Headless` — the `MouseDown`/`MouseUp` headless extension methods live there
  (the `MaterialsControlsTests.fs` / `MainWorkbenchTests.fs` `clickOn` precedent), fixing (2) and (3).
- Dropped the bare `open Avalonia.FuncUI` (the only thing it supplied here was `Component`), fixing the
  `open Elmish` partial-path clash (1); the single `Component` use in `mount` is now fully qualified
  `Avalonia.FuncUI.Component`. `Avalonia.FuncUI.Types.IView` was already fully qualified.

## Testing state

Gate execution is the arc-runner's deterministic gate engine's job after this worker exits (IMPLEMENT
Invariant 6 — the worker acts). For this build-retry round I ran the failing gate and the affected
gate locally to confirm the fix landed (diagnostic verification, not gate authority):

- `build` — `dotnet build Berreman.slnx -c Release`: **Build succeeded, 0 Error(s)**. No `warning FS`
  from our code. Remaining warnings are the pre-existing, out-of-scope third-party advisories
  (`NU1701`/`NU1902`/`NU1904`, exempt) and the solution-wide `MSB3277` WindowsBase/WebView2 reference
  conflict in `.App`/`.Ui.Tests` (pre-existing, unrelated to this slice's scope).
- `ui-smoke` — `--filter Category=ui-smoke`: **104 passed, 0 failed** (the 6 new `EmbeddedChartTests`
  render proofs pass; no regression in the `MaterialEditorWindow` / `MainWorkbench` / `ChartWindow`
  tests that exercise the shared `ChartPlot` path and the two host sites).

Six NEW `ui-smoke` render proofs in `EmbeddedChartTests.fs` (each `HeadlessSession.run`, forcing
real Skia rasterization via `Plot.GetImage` — the `ChartWindow constructs and really renders`
precedent):
1. embedded chart renders one frame, n on the LEFT axis / k on the RIGHT (non-dispersive vacuum);
2. embedded chart renders for a dispersive built-in (silicon);
3. embedded chart draws n and k and rasterizes for EACH of the four transcendental model kinds
   (TaucLorentz / GaussianOscillator / ForouhiBloomer / BrendelBormann);
4. the real Material editor preview embeds the AvaPlot and renders — a non-dispersive default AND a
   transcendental (ForouhiBloomer picked through the segment model picker);
5. the real Materials View panel embeds the AvaPlot and renders — a non-dispersive entry (glass 1.52);
6. the real Materials View panel embeds the AvaPlot and renders — a dispersive entry (silicon).

## Artifacts

None required (no captured logs / screenshots this round).

## Gotchas

- **The build-gate failure was opens-only, in the test file.** `Component` needs `open Avalonia.FuncUI`,
  but that bare open nests an `Elmish` module, so a co-resident `open Elmish` (needed for
  `Program.mkSimple`) resolves through a partial path → `FS0893` (warnaserror). The green precedent
  (`MainWorkbenchTests.fs`) never opens `Avalonia.FuncUI` bare; it fully-qualifies. Mirrored that:
  qualified `Avalonia.FuncUI.Component`, dropped the bare open. Separately, the headless
  `MouseDown`/`MouseUp` extension methods require `open Avalonia.Headless` (they hang off the headless
  window extensions), which the file was missing.
- **Headless native ScottPlot render is proven safe here.** The concern that an embedded AvaPlot
  could throw under the `ui-smoke` frame is answered by the EXISTING baseline test
  `ChartWindow constructs and really renders a live experiment chart` (ExperimentControlsTests.fs),
  which opens an AvaPlot-hosting window headlessly and forces Skia rasterization via
  `ava.Plot.GetImage(...)` — so AvaPlot renders on this platform. The embedded host additionally keeps
  the `ChartHosts.scottPlotHost` `try`/`with` graceful-degrade for defence in depth.
- **Blast radius beyond `touches`.** `touches` is `[Controls, TestWindows, Ui.Tests]`. The `build` gate
  compiles the whole `.slnx`; the two consumer edits are in TestWindows (in `touches`). No other project
  referenced `inlineCanvas`, so its deletion has no wider fallout (verified by grep).
- **Existing tests stay green by construction.** `MainWorkbenchTests` asserts the View panel carries
  `materialNkChart` — the embedded `ContentControl` host carries that same id, so it stays present (and
  now renders an AvaPlot on Show). The `MaterialEditorWindow` ui-smoke tests `Show()` the preview but do
  not assert it is a `Canvas`, so the swap to an embedded AvaPlot leaves them green.
- **ChartWindow refactor is behaviour-preserving.** The four surviving closures delegate to `ChartPlot`
  and still read the mutable `style`/`scatters`/`polar` at call time (F# mutable capture, as the original
  closures already did); the polar / crosshair / interactive layers are untouched, so the ChartWindow
  ui-smoke tests (polar toggle, axis edit, series-axis flip, GetImage render) exercise the same path.
