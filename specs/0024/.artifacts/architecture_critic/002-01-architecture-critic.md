# Architecture critique -- 002.slice-md cycle 1

## Summary

Clean, well-layered work with one architectural concern worth the judge's
attention. The shared renderer-host seam (`ChartHosts.fs`) and the chart panel
(`ChartView.fs`) land in the right place, compile before `Shell.fs`, and keep
native controls host-local per §0.5. The one finding that matters: the seam
offers only *create-and-host*, not *retain-and-refresh*, so AC-U5.1's "`Refresh()`
on subsequent model changes" is implemented as full native-control rebuild per
render — a shape that bakes into the seam four future slices (U3/U4/U6/U8) are
required to reuse.

## Layering

No violations. `ChartView` → `ChartHosts` and `Shell` → `ChartView` both point
inward; the `.fsproj` registers `ChartHosts.fs`, `ChartView.fs` before the U1
`Shell.fs` block (`OpticalConstructor.Ui.fsproj:64-65`), so the new seam is
visible to its consumer with no module cycle. `ChartView.chartPanel` takes the
chart sub-state (`ChartSettings * Readout.Markers`) and a `ChartMsg` dispatch
rather than `RootModel`, mirroring `ConstructionView.stackPanel` — this is the
correct composition seam and keeps the cycle out.

## Separation of concerns

`ChartView.fs:73-102` embeds a block of representative sample data **plus a real
Berreman solve** (`series1DMany`, `transparentGlassFilm`) at module scope inside
a *view* module. Rendering is a view responsibility; sourcing sweep data is not.
The slice legitimately defers the model-driven sweep (U4/U7), but the chosen
placement means the data-source seam now lives in the view's module-init, which a
later slice must surgically excise rather than simply swap. The cleaner shape
would have been a tiny `ChartSamples`-style sibling (or accepting series as a
parameter — see Evolvability), keeping `ChartView` a pure function of state. Minor,
and the SoW flags it as deferred, so this is a placement note, not a defect.

## Spec fit

R-1/R-2/R-3 are met on the letter: the four `Plot1DView.*` charts route through
the `AvaPlot` host (`ChartView.fs:183-186`), settings reach the plot only via
`applyToScottPlot` inside `Plot1DView.render` (`Plot1DView.fs:50`) — the view
re-applies no styling itself — and cursor placement edits `Readout.Markers`
(`ChartView.fs:61-62`). The `Chart` `RootMsg` case is wired and delegates to the
pure `ChartView.update` (`Shell.fs:145-147`).

Two soft spots the judge should weigh as documented deferrals, not gaps:
- **R-4 / WebView2 is structurally wired but unconditionally a placeholder.**
  `tryHostPlotly` always returns `None` (`ChartHosts.fs:91-95`), so the 3-D
  surface is "hosted" only in the sense of being routed to an adapter that always
  degrades; `surface3D`'s lazy chart is never forced. The no-`net10.0`-binding
  rationale is plausible and the SoW backs it with an `assets.json` check, and
  §U1.8 explicitly blesses the placeholder — so this is within spec, but AC-U5.1's
  3-D path is effectively unverifiable until the deferred Windows-desktop bridge.
- **On-plot cursor is text-only.** Markers update the model, but no visual marker
  is drawn on the `AvaPlot`; the toolbar shows a `cursor: (x, y)` readout. R-3's
  MUST ("edit `Readout.Markers` ... and dispatch `Chart`") is satisfied; the
  visual pick is deferred. Acceptable, noted.

## Evolvability

This is the head with the most leverage, because `ChartHosts.fs` is the seam
U3/U4/U6/U8 are *required* to reuse — its shape is load-bearing for the rest of
the arc.

- **The seam has no retain-and-refresh path.** `scottPlotHost` takes a finished
  `ScottPlot.Plot` and wraps a freshly-constructed `AvaPlot` each call
  (`ChartHosts.fs:60-69`). R-1 itself names `View.createWithOutlet` "to retain a
  handle for `Refresh()`," but the seam only exposes create-and-host. A future
  high-frequency consumer (the U7 live sweep progress) that wants to `Refresh()`
  an existing control instead of rebuilding it cannot express that through this
  seam as written — it will be forced into the same rebuild-per-render path. If
  the outlet/refresh variant is going to be needed (and the JobRunner-driven
  sweep strongly implies it), adding it to the seam *now*, while there is exactly
  one consumer, is far cheaper than retrofitting it across four panels later.
- **`chartPanel` accepts no series.** Its signature is
  `(settings) (markers) (dispatch)` (`ChartView.fs:177`) and it manufactures its
  own data. When U4/U7 produce real sweep results on the model, this signature
  must widen and `Shell.panelContent` (`Shell.fs:194`) must thread the data
  through — a forced cross-tree change. Threading a `series` parameter now (even
  fed from the sample data this slice) would let the later wiring be a call-site
  swap instead of a signature change rippling into the root.

## Risks

- **§U1.8 graceful degradation is narrower than it looks.** `scottPlotHost`'s
  `try/catch` (`ChartHosts.fs:61-69`) wraps only `AvaPlot` *construction*. The
  plot argument `sweepPlot settings` is evaluated eagerly *before* the call, and
  that is where the real solve runs (`renderSweep` → `series1DMany`). A throw in
  plot-building / the solve therefore escapes the placeholder path and crashes the
  render rather than degrading. Today's fixed sample data is known-good so
  `ui-smoke` passes, but once real model data flows a degenerate stack could throw
  and the panel will not degrade — contrary to the "renders a frame without
  throwing" guarantee the seam advertises. The SoW concedes the catch "covers ctor
  failures"; the build path it does not cover is the one real consumers will hit.
- **Native-control churn discards interaction state.** Because a new `AvaPlot` is
  built on every render and FuncUI swaps the `ContentControl` child, ScottPlot's
  built-in pan/zoom state is thrown away on each dispatch. For the primary results
  surface that is a real UX regression waiting in the consumers; it is the same
  root cause as the missing outlet path above. The SoW lists this as deferred.

## Bottom line

Gates are green and the slice meets its acceptance criteria on the letter; this
is shippable. My read is **ship, with the seam concern logged for the judge** —
the rebuild-per-render / no-outlet shape and the eager-build-outside-the-catch
risk are both cheapest to address now, while `ChartHosts.fs` has a single consumer,
rather than after U3/U4/U6/U8 have copied the pattern. None rises to a re-spawn
blocker on its own; the strongest case for a hold would be adding the
`createWithOutlet`/`Refresh` variant and widening the catch before the seam
calcifies. The verdict is the judge's.
