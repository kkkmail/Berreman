# Impl plan — spec 0033, slice 017 (IMPLEMENT, AC-D1)

## Goal

REAL-MOVE the renderer-neutral chart model (`ExperimentChart.fs`: `ExperimentChart`,
`toCsv`, `ChartFont`, `ChartStyle` with `ChartElement` / `AxisStyle` / `SeriesStyle` /
`ChartStyleState` / `dataBounds` / `defaultState`) and the ScottPlot seam
(`ChartWindow.fs`: `ChartWindowIds`, `ChartWindow` with `rebuildPlot`) from
`OpticalConstructor.TestWindows` into `OpticalConstructor.Controls`, changing their
namespace to `OpticalConstructor.Controls`. No file-linking (it mints distinct type
identities per assembly); consumers reach the one copy through the existing project
references. `OpticalConstructor.Controls.fsproj` gains the `ScottPlot.Avalonia` 5.1.59
package (the same version both hosts already use). Re-point every TestWindows usage and
reconcile `OpticalConstructor.Ui/Charts/ChartSettings.fs` onto the one shared model —
no third settings type. Both existing charts stay green; no chart type is left declared
in TestWindows.

## Approach

1. **Move by byte-copy + targeted edit** (preserves LF and history-diff clarity):
   copy the two files into `OpticalConstructor.Controls/`, edit only the namespace
   line (and, in `ChartWindow.fs`, the `open …ExperimentChart` line), delete the
   originals.
2. **Project files.** Controls.fsproj: two `Compile` entries (ExperimentChart before
   ChartWindow — the window consumes the model) + `ScottPlot.Avalonia` 5.1.59; update
   the header comment (the library is no longer "Avalonia + FuncUI NuGet only").
   TestWindows.fsproj: drop the two `Compile` entries and the now-orphaned direct
   `ScottPlot.Avalonia` reference (the seam moved; ScottPlot flows transitively
   through the Controls project reference, single-sourced at 5.1.59).
3. **Re-point TestWindows.** `TableAndElementRotationView.fs` is the only TestWindows
   consumer and already has `open OpticalConstructor.Controls` — after the move its
   `ExperimentChart.*` / `ChartWindow` references resolve from Controls unchanged.
4. **Re-point Ui.Tests.** `ChartFontTests.fs` and `ChartStyleTests.fs` open
   `OpticalConstructor.TestWindows[.ChartFont/.ExperimentChart/.ChartStyle]` — re-point
   those opens to `OpticalConstructor.Controls…`. `ExperimentControlsTests.fs` already
   opens `OpticalConstructor.Controls`, so it re-points for free.
5. **Reconcile Ui (`ChartSettings.fs`).** The shared model is now reachable from Ui
   (Ui references Controls; it could never reference TestWindows). ChartSettings stays
   the Ui-side §H.1 settings record — its public surface is pinned by
   `OpticalConstructor.Tests/ChartSettingsTests.fs`, and that project is not in this
   slice's `touches` — but the literal renderer-mapping duplicate is unified: hoist the
   pop-out window's private hex→`ScottPlot.Color` mapping into a small public
   `ChartRender` module beside the moved window and delegate Ui's `scottPlotColor` to
   it, so the solution has ONE hex-parse seam. No new settings container anywhere
   (the "no third settings type" prohibition).

## Files to modify

- add `Berreman/OpticalConstructor/OpticalConstructor.Controls/ExperimentChart.fs` (moved)
- add `Berreman/OpticalConstructor/OpticalConstructor.Controls/ChartWindow.fs` (moved, + `ChartRender`)
- delete `Berreman/OpticalConstructor/OpticalConstructor.TestWindows/ExperimentChart.fs`
- delete `Berreman/OpticalConstructor/OpticalConstructor.TestWindows/ChartWindow.fs`
- `…/OpticalConstructor.Controls/OpticalConstructor.Controls.fsproj`
- `…/OpticalConstructor.TestWindows/OpticalConstructor.TestWindows.fsproj`
- `…/OpticalConstructor.Ui/Charts/ChartSettings.fs`
- `…/OpticalConstructor.Ui.Tests/ChartFontTests.fs`
- `…/OpticalConstructor.Ui.Tests/ChartStyleTests.fs`

## Risks

- **Name resolution drift in TestWindows** — `ExperimentChart` used to resolve through
  the enclosing namespace; after the move it must resolve through
  `open OpticalConstructor.Controls`. Only `TableAndElementRotationView.fs` consumes it
  and already carries that open. Watched by the build.
- **Pinned surface in OpticalConstructor.Tests** — `ChartSettingsTests.fs` pins the
  `ChartSettings` / `TraceSettings` field names, `Linear`/`Log10`, `SolidLine`,
  `defaultValue`, and the projection functions; the reconcile must not alter any of
  those. The deep merge of `ChartSettings` onto the `ChartStyle` spine is deliberately
  NOT this slice (steps 018/019 keep evolving the spine; Tests is not in `touches`).
- **Package flow** — removing TestWindows' direct ScottPlot ref relies on transitive
  compile/runtime assets through the Controls project reference (default NuGet
  behaviour). Verified by the build + ui test suites.
- **CRLF churn** — new files created by byte-copy; verify `git diff --numstat` shows
  no EOL-only churn.

## Gates (advisory local runs; the arc-runner gate engine is authoritative)

`build`, `unit-tests` (119 baseline), `constructor-unit-tests` (373 baseline),
`ui-smoke` (58 baseline), `ui-tests` (255 baseline). A pure move adds no tests; all
counts must hold at baseline.
