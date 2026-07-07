# Code judge -- 018.slice-md cycle 1

## Inputs read

- Slice spec: C:\GitHub\Berreman\specs\0033\.slices\018.slice-md
- State-of-the-world: C:\GitHub\Berreman\specs\0033\.slices\018-state-of-the-world.md
- Impl-log: C:\GitHub\Berreman\specs\0033\.slices\018-impl-log.md
- Gate results: build pass / unit-tests pass / constructor-unit-tests pass / ui-smoke pass / ui-tests pass
- Critic critiques: (none supplied this cycle)

## Rationale

All five gates in the roster pass, and no critic critique was supplied this cycle, so the
decision rests on whether the diff meets the slice-spec contract and whether the new public
surface is exercised by tests. I read the working-tree diff directly (`git diff HEAD`) to
verify the SoW and impl-log against the code; the three line up exactly.

Every element of the slice's "How to implement" is present in the diff. `ChartElement.YAxis`
now carries the two-case `AxisSide` (LeftAxis | RightAxis) so the element picker distinguishes
the left and right Y axes; `ChartStyleState` splits into `yAxisLeft` / `yAxisRight` with the
tri-state `ChartAxis = AxisX | AxisY of AxisSide` mutator surface replacing the boolean `isX`
on all six mutators plus a shared `axisStyleOf` lookup; `SeriesStyle` gains `axisSide :
AxisSide` with `setSeriesAxisSide`; and `dataBounds` returns a `ChartBounds` record with a
shared padded x-range and independent per-side y-ranges computed from only each side's series,
which `defaultState` uses to seed all three axes (series default left; the right axis seeds the
unit fallback). In `ChartWindow`, `rebuildPlot` assigns each scatter's `Axes.YAxis` through the
single `scottYAxis` mapping onto `plot.Axes.Left` / `plot.Axes.Right`, `applySeriesStyle`
re-asserts the side on style application (guarded `not polar`, correctly, since polar scatters
hold projected coordinates on the hidden default axes), and `applyAxisFonts`,
`applyAxisFormat`, `setCartesianAxesVisible`, and `applyAxisLimits` all drive
`plot.Axes.Right`. `toCsv` and the polar-toggle behaviour are untouched, satisfying the
acceptance clause that CSV output be unchanged — and the worker went further and pinned the CSV
output with a test so the invariance is asserted, not just asserted-by-silence.

The acceptance criterion — one left-axis plus one right-axis series producing independent
per-axis Y bounds and side-correct scatter assignment — is covered by tests in the diff. The
new model test `dataBounds computes INDEPENDENT y bounds per axis side` builds exactly the
n/k-shaped acceptance chart (left 1.5…2.5, right 0.01…0.09) and asserts neither side stretches
the other; the reshaped `defaultState` test asserts the right axis seeds independently of left
data; the tri-state mutator test proves X / Y-left / Y-right edits land on their axis only; and
a new headless ui-smoke test drives the series panel's new `ChartWindowSeriesAxis` picker and
asserts by reference equality that the flipped scatter lands on `plot.Axes.Right` while the
other stays on `plot.Axes.Left`. Suite counts grew consistently with the claims (ui-tests
255 → 259, ui-smoke 58 → 59), so the `count_at_least` gates did not merely hold — they advanced.

One judgment call deserves note but is not a blocker: `applyAxisLimits` drives the right axis
only when a visible series uses it or the user has pinned a manual range, rather than
unconditionally. The slice text says the limits applier "also drive[s] plot.Axes.Right"; the
guard is a defensible reading because an unconditionally-pushed limit on ScottPlot 5.1.59's
unset right axis would paint phantom 0–1 tick labels on every existing single-axis chart. The
worker recorded this reasoning and its known cosmetic edge (a once-driven right axis keeps its
ticks after its last series hides) in the impl-log Gotchas, exactly as the project prompt's
"pick a sensible default and note it" rule requires.

Hygiene is clean: only the four claimed files changed (plus `.manifest.state.json`, the
arc-runner's own state, correctly left alone), the edits stay inside the slice's `touches`
(`OpticalConstructor.Controls`, `OpticalConstructor.Ui.Tests`), and `git diff --numstat`
matches `--ignore-cr-at-eol` on all four files, so no CRLF churn was introduced. The
Gotchas' system-prompt path drift note matches slices 015–017 and is a supervisor-side
concern, not a worker defect. Verdict: done-green.

## Verdict

done-green

```json
{"verdict": "done-green", "rationale": "All five gates pass and no critic critiques were filed. The diff verifiably implements every slice requirement: two-case AxisSide on SeriesStyle, ChartElement.YAxis of AxisSide, tri-state ChartAxis mutators with per-side yAxisLeft/yAxisRight in ChartStyleState, per-side ChartBounds from dataBounds seeding defaultState, and ChartWindow mapping sides onto ScottPlot 5.1.59's native plot.Axes.Right in rebuildPlot, applySeriesStyle, applyAxisLimits, the format/font appliers, and setCartesianAxesVisible, plus a series-panel axis picker. The acceptance shape (one left- and one right-axis series with independent per-axis Y bounds and side-correct scatter assignment) is exercised by 4 new model tests (ui-tests 255->259) and a headless right-axis assignment proof (ui-smoke 58->59); toCsv is untouched and its output is pinned by a new test. SoW and impl-log match the diff exactly; edits stay inside the slice touches; no CRLF churn. The in-use-or-manual guard on right-axis limits is a defensible, documented interpretation that prevents phantom ticks on existing single-axis charts.", "retry_hint": ""}
```
