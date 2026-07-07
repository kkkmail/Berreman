# Impl log — spec 0033, slice 018 (second Y axis in the shared chart model)

## Progress

- [x] Read task file, system prompt (implement_worker + arc-runner base), project prompt, slice spec.
- [x] Surveyed the moved chart model/window; confirmed the only `ChartStyle` consumers are
      `ExperimentChart.fs`, `ChartWindow.fs`, `ChartStyleTests.fs`, `ChartFontTests.fs` (ChartFont only) —
      TestWindows and Ui need ZERO edits, matching this slice's `touches`.
- [x] Verified ScottPlot 5.1.59 API by reflection: `Axes.Right : IYAxis`,
      `SetLimitsY(double, double, IYAxis)`, `Scatter.Axes.YAxis` settable,
      `RightAxis : YAxisBase : AxisBase`.
- [x] Wrote impl-plan.
- [x] Model changes (`ExperimentChart.fs` / `ChartStyle`): `AxisSide` (+`label`/`allSides`),
      `ChartElement.YAxis of AxisSide`, tri-state `ChartAxis` mutators + `axisStyleOf`,
      `SeriesStyle.axisSide` + `setSeriesAxisSide`, `ChartBounds` + per-side `dataBounds`,
      `yAxisLeft`/`yAxisRight` split in `ChartStyleState`, `defaultState` seeding all three axes,
      five fixed picker elements with left/right labels. `toCsv`/`ChartSeries` untouched.
- [x] Window changes (`ChartWindow.fs`): `scottYAxis` side mapping, sided `dataBoundsVisible`,
      `rightAxisInUse`, right axis added to `applyAxisFonts`/`applyAxisFormat`/`setCartesianAxesVisible`,
      per-axis `applyAxisLimits` (right guarded on in-use-or-manual), scatter side assignment in
      `rebuildPlot` + re-assertion in `applySeriesStyle` (non-polar), `axisPanel` reshaped onto
      `ChartAxis`, series panel "Axis: Left/Right" picker (`ChartWindowIds.seriesAxis`).
- [x] Tests: `ChartStyleTests` reshaped + 4 new (independent per-side bounds; tri-state mutator
      independence; default-left + `setSeriesAxisSide`; `toCsv` output pin). `ExperimentControlsTests`:
      `seriesAxis` added to the ids-distinct list + new headless ui-smoke proof that flipping the second
      series moves its scatter to `plot.Axes.Right` while the first stays on `plot.Axes.Left`.
- [x] Local advisory build + all four suites green; logs in `.artifacts/018-*.log`.
- [x] EOL check clean (`git diff --numstat` = `--ignore-cr-at-eol` on all four edited files).
- [x] State-of-the-world written; structured exit.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Controls/ExperimentChart.fs` — the model growth (all in
  module `ChartStyle`; `ExperimentChart`/`ChartFont` untouched).
- `Berreman/OpticalConstructor/OpticalConstructor.Controls/ChartWindow.fs` — the ScottPlot seam growth +
  the series panel's axis picker + new id.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/ChartStyleTests.fs` — reshaped + 4 new tests.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/ExperimentControlsTests.fs` — ids list + 1 new
  headless ui-smoke test.

## Decisions

- `AxisSide = LeftAxis | RightAxis` is the two-case side DU; the tri-state mutator
  target composes it: `ChartAxis = AxisX | AxisY of AxisSide`. `ChartElement.YAxis`
  becomes `YAxis of AxisSide` (keeps `XAxis` stable, so the element picker's X index
  is unchanged for the existing axis-editing smoke test).
- Side assignment lives ONLY in `SeriesStyle` (per the slice text); `ChartSeries` /
  `ExperimentChart` / `toCsv` are untouched, so no TestWindows edits are forced
  (TestWindows is outside this slice's `touches`). A chart host that wants a series
  born on the right axis (step 019's n/k chart) seeds it via `setSeriesAxisSide` on
  `defaultState`'s output.
- `dataBounds` returns a `ChartBounds` record (`x` / `yLeft` / `yRight` padded ranges,
  shared `paddedRange` helper) instead of growing the 4-tuple to 6; input is
  `(ChartSeries * AxisSide) list` so the pure model never reads mutable style.
- The element picker always offers BOTH Y axes (editing an unused right axis is
  harmless and keeps `elements` a pure function of the series count).
- Flipping a series' side applies through `applyStyle` (the `applySeriesStyle`
  re-assertion), not a full `rebuildPlot` — same pattern as every other series edit.

## Testing state

All four suites green locally (ADVISORY ONLY — the arc-runner's gate engine is the
sole gate authority and re-runs them after exit):

- build: 0 errors (94 warnings — the pre-existing MSB3277/FS1125 noise, none from the
  four edited files).
- BerremanTests: 119 passed / 5 skipped / 0 failed (= 017 baseline; core untouched).
- OpticalConstructor.Tests: 373 passed / 0 failed (= 017 baseline; project untouched).
- ui-smoke: 59 passed / 0 failed (58 + the new right-axis scatter proof).
- ui-tests: 259 passed / 0 failed (255 + 4 new ChartStyle model tests).

## Artifacts

- `specs/0033/.artifacts/018-build.log`
- `specs/0033/.artifacts/018-unit-tests.log`
- `specs/0033/.artifacts/018-constructor-unit-tests.log`
- `specs/0033/.artifacts/018-ui-smoke.log`
- `specs/0033/.artifacts/018-ui-tests.log`

## Gotchas

- The task file's system-prompt path `C:\GitHub\AI-Strategy-Generator\implement_worker.system-md`
  does not exist (same drift as slices 015–017); the real file is
  `AI-Strategy-Generator\src\ai_strategy_generator\multistep\implement_worker.system-md`.
- An UNUSED ScottPlot 5 axis keeps an unset range and renders no ticks; setting
  explicit limits on it makes ticks appear. Unconditionally pushing the right-axis
  limits would therefore paint phantom 0–1 tick labels on the right edge of every
  existing single-axis chart. `applyAxisLimits` guards the right axis on
  "in use (a visible right-assigned series) or manual (auto off)"; the
  format/font/visibility appliers drive `plot.Axes.Right` unconditionally
  (harmless while the axis has no ticks).
- Once a right axis HAS been driven and its last right series is then hidden, its
  limits (and ticks) persist rather than resetting — accepted as a minor cosmetic
  edge; resetting a ScottPlot axis to "unset" has no clean public API in 5.1.59.
- In polar mode scatters hold polar-projected cartesian coordinates on the hidden
  default axes; side re-assertion in `applySeriesStyle` is guarded with `not polar`
  so a right-assigned series still projects correctly on the polar grid.
- `.manifest.state.json` (modified, CRLF-warned) and `.claude/` (untracked) are the
  arc-runner's / harness's own files — left alone, as in slices 001–017.
