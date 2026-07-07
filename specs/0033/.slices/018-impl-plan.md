# Impl plan — spec 0033, slice 018 (IMPLEMENT: second Y axis in the shared chart model)

## Approach

Step 017 real-moved the renderer-neutral chart model (`ExperimentChart.fs`:
`ExperimentChart` / `ChartFont` / `ChartStyle`) and the ScottPlot pop-out seam
(`ChartWindow.fs`) into `OpticalConstructor.Controls`. This slice threads a
SECOND (right) Y axis through that spine, model-first:

1. **Model (`ExperimentChart.fs`, module `ChartStyle`)**
   - `AxisSide` — the new two-case DU (`LeftAxis | RightAxis`) with a `.label`
     and `allSides` (display order for the series panel's picker).
   - `ChartElement` — `YAxis` becomes `YAxis of AxisSide`, so the element
     picker distinguishes the left and right Y axes.
   - `ChartAxis` — the tri-state mutator target (`AxisX | AxisY of AxisSide`)
     replacing the old boolean `isX` flag on every axis mutator
     (`setAxisAuto/Min/Max/Format`, `bumpAxisDecimals`), plus an
     `axisStyleOf` accessor.
   - `SeriesStyle` gains `axisSide : AxisSide` (default `LeftAxis`), with a
     `setSeriesAxisSide` mutator; `seriesStyleOf`'s fallback follows.
   - `ChartStyleState` — `yAxis` splits into `yAxisLeft` + `yAxisRight`.
   - `dataBounds` — takes `(ChartSeries * AxisSide) list` and returns a new
     `ChartBounds` record `{ x; yLeft; yRight }` of padded ranges: x from ALL
     series, each Y side ONLY from its own series (unit-box fallback per
     side). `defaultState` seeds `xAxis` / `yAxisLeft` / `yAxisRight` from it
     (all series default to the left axis).
   - `elements` offers five fixed parts (`Header; XAxis; YAxis LeftAxis;
     YAxis RightAxis; Legend`) + one per series; `elementLabel` labels the
     Y axes "(left)" / "(right)". `toCsv` untouched.

2. **Window (`ChartWindow.fs`)**
   - `rebuildPlot` (cartesian branch) assigns each scatter's `Axes.YAxis` to
     `plot.Axes.Left` / `plot.Axes.Right` from its style's `axisSide`;
     `applySeriesStyle` re-asserts the assignment (non-polar) so flipping a
     side takes effect without a rebuild.
   - `applyAxisLimits` drives `plot.Axes.Right` via
     `SetLimitsY(lo, hi, IYAxis)` (per-axis overload, confirmed present in
     ScottPlot 5.1.59). Decision: only when the right axis is IN USE (a
     right-assigned series exists) or the user set it manual — an unused
     ScottPlot axis stays range-unset and renders no ticks, so
     unconditionally setting 0–1 would grow phantom right-edge tick labels on
     every existing single-axis chart.
   - `applyAxisFonts` / `applyAxisFormat` / `setCartesianAxesVisible` also
     drive `plot.Axes.Right` (RightAxis : AxisBase, so the existing
     pattern-matched appliers extend directly).
   - `axisPanel` reshapes onto `ChartAxis`; the series panel gains an
     "Axis: Left/Right" ComboBox (`ChartWindowIds.seriesAxis`). Polar toggle
     logic untouched (still angular-only).

3. **Tests (`OpticalConstructor.Ui.Tests`)**
   - `ChartStyleTests`: update existing dataBounds/defaultState/mutator/
     elements tests to the new shapes; NEW tests for independent per-side
     bounds, default-left + `setSeriesAxisSide`, tri-state mutator
     independence, and a `toCsv` output pin (unchanged by the model growth).
   - `ExperimentControlsTests`: add `seriesAxis` to the ids-distinct list;
     NEW headless ui-smoke test proving a flipped series' scatter lands on
     `plot.Axes.Right` while the other stays on `plot.Axes.Left`.

## Files to modify

- `Berreman/OpticalConstructor/OpticalConstructor.Controls/ExperimentChart.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Controls/ChartWindow.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/ChartStyleTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/ExperimentControlsTests.fs`

## Risks

- **Compile fan-out of the `yAxis` split / mutator re-signature** — bounded:
  the only `ChartStyle` consumers are the two Controls files + the two test
  files (verified by grep); TestWindows/Ui need zero edits.
- **Unused right axis changing existing charts' look** — mitigated by the
  in-use/manual guard in `applyAxisLimits` (recorded as a Gotcha).
- **Headless test brittleness** — the new ui-smoke test reuses the existing
  ChartWindow test helpers (control lookup by id, RunJobs) and asserts by
  reference equality on the axis objects, not pixels.
- **EOL** — both Controls chart files are LF since 017; verify no CRLF churn
  before finishing.
