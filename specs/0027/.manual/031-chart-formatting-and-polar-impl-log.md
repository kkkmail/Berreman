# 0027-031 — Chart view: data-fit axes, selectable elements + format panel, polar toggle (impl log)

Implements `030-experiment-redesign.txt` — an Excel-style overhaul of the pop-out chart window
(`ChartWindow`, the "chart view screen"). Three deliverables:

1. **The x-axis initializes to the data.** A 400…800 nm sweep opened showing 0…1000 (and zoomed to wild
   negatives) because the axis was never fit to the data — and the invisible crosshair/marker/text at the
   origin pulled auto-scale toward 0. Now the view is set to the **data bounds** on load.
2. **Selectable chart elements + a contextual "Format" panel.** The header, X axis, Y axis, legend, and each
   chart line are selectable (an element picker, like Excel's Chart-Elements dropdown in the Format pane),
   and the panel edits what applies to the selection.
3. **A Polar ⇄ XY toggle** for angular experiments (a 360° rotation OR a 0…89° incidence sweep) — a visual
   representation only, one button.

**Result:** full solution builds clean (x64, `--warnaserror+:25`). `OpticalConstructor.Ui.Tests` **298**
(was 268, +30). No gate regressed. (Domain untouched this task.)

## Research

ScottPlot 5.1's API was mapped by reflecting the shipped assembly (the same method used for the 028 font
work) plus reviewing Excel's "Format <element>" task pane as the UX model. Key ScottPlot 5 surfaces used:
`Plot.Axes.SetLimitsX/Y` + `AutoScale` (data-fit / auto), `AxisBase.TickGenerator` →
`NumericAutomatic.LabelFormatter` (`Func<double,string>`, for number format / digits),
`Legend.IsVisible/Alignment/FontSize` (placement = the 9-way `Alignment`), per-`Scatter`
`IsVisible/LineWidth/Color/MarkerShape/MarkerSize` (thickness / colour / dots), and the polar path
`Plot.Add.PolarAxis(radius)` + `PolarAxis.GetCoordinates(radius, degrees)` + `Plot.Add.ScatterLine(...)`.

## Part 1 — the pure style model (`ExperimentChart.fs`, module `ChartStyle`)

Domain-neutral, renderer-free (no ScottPlot types) so all the logic is unit-tested without a native window;
`ChartWindow` maps it onto ScottPlot at the IO seam.

- **`ChartElement`** = `Header | XAxis | YAxis | Legend | Series of int` — the selectable parts.
- **`NumberFormat`** = `GeneralFormat | FixedFormat of int | ScientificFormat of int` with `formatValue`
  (the pure tick-label formatter), `withDecimals` (clamped 0…10). Excel's number-format-and-digits.
- **`AxisStyle`** = `{ auto; min; max; format }`; **`LegendPlacement`** (9 cases) + `LegendStyle`;
  **`SeriesStyle`** = `{ visible; thickness; colorHex; showMarkers }` (thickness clamped 0.5…10).
- **`ChartStyleState`** ties them together with the selected element and the shared `ChartFont.ChartFontState`
  (font sizes are reused from 028, not re-modelled).
- **`dataBounds`** — the padded (xMin,xMax,yMin,yMax) of the data (unit-box fallback); **`defaultState`**
  initializes both axes to `auto` with min/max = the data bounds (so the load view and "Auto" both fit the
  data), one default-styled series per curve, legend upper-right.
- Pure updaters: `selectElement`, `setAxis{Auto,Min,Max,Format}` (setting a bound turns auto off),
  `bumpAxisDecimals`, `setLegend{Visible,Placement}`, `setSeries{Visible,Color,Markers}`,
  `bumpSeriesThickness`, `bumpFont`.
- **Polar:** `polarXY (angleDeg, radius)` (pure `(r cosθ, r sinθ)`, the counterpart of ScottPlot's
  `GetCoordinates`, for tests) and `polarRadius` (largest visible |y|, for the outer circle).

`ExperimentChart` gains **`angular : bool`** — true for a rotation (R1) or incidence (R2) sweep, false for a
wavelength sweep. It only gates the polar toggle; it does not change the data. The host `experimentResult`
sets it (R1 / R2 → true, λ → false).

## Part 2 — the chart window (`ChartWindow.fs`)

Rebuilt around the style model:

- **Data-fit on load** — the plottables are (re)built and then `applyStyle` sets the axis limits from the
  data bounds (never `AutoScale`, so the origin crosshair/marker can't drag the range to 0…1000).
- **Element picker + Format panel** (right dock) — a `ComboBox` of `ChartStyle.elements` drives a panel that
  rebuilds per selection:
  - **Header** → font −/＋ (+ readout).
  - **X / Y axis** → Auto (fit data) checkbox, Min / Max fields (editing either turns Auto off),
    number-format dropdown (General / Fixed / Scientific), digits −/＋, and a font stepper (axis label +
    tick labels).
  - **Legend** → Show-legend checkbox, 9-way placement dropdown, font stepper.
  - **Line: <series>** → Show-line checkbox, thickness −/＋, colour dropdown, Show-dots checkbox.
  All wired to the pure updaters, then `applyStyle` pushes onto ScottPlot.
- **Polar ⇄ XY toggle** — shown only when `chart.angular`. Flipping it `rebuildPlot`s: XY = the scatters as
  before; **polar** = `Add.PolarAxis(polarRadius)` for the circular grid, each series' `(angle, value)`
  points projected via `PolarAxis.GetCoordinates` and drawn as a `ScatterLine`, the whole thing auto-scaled.
  A 360° rotation traces a closed loop; a 0…89° incidence sweep traces a quarter-arc.
- Retained: the snapping crosshair (cartesian only), major/minor gridline toggles, PNG + CSV export, the
  description box.

## Tests (+30)

- **`ChartStyleTests`** (new, pure): data-fit bounds (asserting the x-range brackets 400…800, nowhere near
  0/1000 — the reported bug); `defaultState` auto + fitted min/max; `formatValue` General/Fixed/Scientific
  and `withDecimals`/`bumpAxisDecimals` clamping; setting a bound turns Auto off; legend visibility/placement;
  per-series thickness (clamped) / colour / markers / visibility independence; the element list + labels;
  `selectElement`; `bumpFont`; and the polar `polarXY` (0°→(r,0), 90°→(0,r), 180°→(−r,·)) + `polarRadius`
  (largest visible |y|, shrinks when the max series is hidden, positive fallback).
- **`ExperimentControlsTests`** (updated): the ChartWindow id set (element picker + polar toggle, distinct);
  a construction smoke test that the element picker, properties panel and polar toggle are present for an
  angular chart; and a **drive** smoke test that toggling polar (exercising the `PolarAxis` +
  `GetCoordinates` rebuild) and selecting the X axis + turning Auto off (the `SetLimitsX` path) run without
  throwing headlessly.

## Files

- Changed: `OpticalConstructor.TestWindows/{ExperimentChart,ChartWindow,TableAndElementRotationView}.fs`;
  `OpticalConstructor.Ui.Tests/{ExperimentControlsTests.fs, OpticalConstructor.Ui.Tests.fsproj}`.
- New: `OpticalConstructor.Ui.Tests/ChartStyleTests.fs`.

## Notes / deferred

- **Selection is via the element dropdown** (Excel's Format-pane Chart-Elements dropdown), which is robust
  and testable. Clicking a part directly on the plot to select it is a possible future add — ScottPlot 5 has
  no ready hit-test for the title/legend/axis regions, so it was not attempted here.
- The polar view keeps the rectangular frame/labels behind the polar grid (they read as harmless guides); a
  frameless polar look is a small future polish.
- The number-format formatter for `General` uses a clean `0.######` (trims trailing zeros); fine for the
  wavelength / angle / intensity ranges in play.

## Follow-up fix — switching polar → XY blanked the axes

**Symptom (reported):** toggling back from polar to XY broke the XY view.

**Cause.** `Plot.Add.PolarAxis` HIDES the rectangular axes (`Axes.Bottom/Left.IsVisible = false`) so the
polar grid reads cleanly — a nice side effect for the polar look, but the cartesian rebuild never turned
them back on, so the XY view rendered the curve with no axes / ticks / frame. Confirmed headlessly by
reading `Axes.Bottom/Left.IsVisible` after a polar round-trip (`false`, while the limits / labels / tick
generator were all intact).

**Fix.** The cartesian branch of `applyStyle` now calls `setCartesianAxesVisible true` before re-applying
the format + limits, so returning to XY always restores the axes. A regression test drives the polar
round-trip and asserts both axes are visible again and the data-fit limits are restored.

## Follow-up fix — double-clicking the inline chart didn't open the pop-out window

**Symptom (reported):** double-clicking the experiment (inline chart) stopped opening the chart window.

**Cause.** The only trigger was a hand-rolled `PointerPressed` + `e.ClickCount >= 2` check on the chart
`Border`. Construction + real ScottPlot rasterization of the pop-out window were verified to succeed
headlessly (a new test renders a live chart via `Plot.GetImage`), so the window itself was fine — the
double-click *gesture* just wasn't firing reliably.

**Fix (first pass, in `ExperimentControls.fs`):** an explicit **"Open chart window ↗" button** above the
inline draft chart, and the manual click-count check replaced with Avalonia's built-in **`DoubleTapped`**.

**Fix (second pass — the design the user asked for): a per-experiment "View" action.** Each collected
experiment row now has a **"View ↗" button** next to Remove that opens the chart window **for that
experiment**, and a **double-click on the row** invokes the same action. This makes the "open the chart"
behaviour a first-class, headless-testable action rather than relying on an easy-to-miss gesture.
- `experimentResult` was refactored to `chartForParams` (element + variable + capture + range → chart) so a
  specific collected experiment can be charted (`chartForExperiment`), not just the live draft.
- New `ViewExperiment of string` message + `viewExperimentHook` (the same forward-referenced side-effect
  seam as `openChartWindowHook`), wired through `ExperimentControls.Handlers.viewExperiment`.
- A `ChartWindow.ConstructedCount` test seam proves the **full** path fires: a test dispatches
  `OpenExperimentChartWindow` and asserts a `ChartWindow` is actually constructed (ruling out a dead hook —
  it fires, so the original break was purely the gesture), and another clicks the row's **View** button and
  asserts a `ChartWindow` opens end-to-end. Plus the earlier button/render tests.
