# Code judge -- 017.slice-md cycle 1

## Inputs read

- Slice spec: C:\GitHub\Berreman\specs\0033\.slices\017.slice-md
- State-of-the-world: C:\GitHub\Berreman\specs\0033\.slices\017-state-of-the-world.md
- Impl-log: C:\GitHub\Berreman\specs\0033\.slices\017-impl-log.md
- Gate results: build pass / unit-tests pass / constructor-unit-tests pass / ui-smoke pass / ui-tests pass
- Critic critiques: (none this cycle)

## Rationale

All five gates pass at the slice-016 baselines (119 / 373 / 58 / 255), and no
critic critique was produced this cycle, so the decision turns on whether the
diff actually satisfies the slice contract and whether any new public surface
escaped test coverage. I inspected the working-tree diff directly to confirm
both.

The slice's acceptance line has three clauses, and all three verify against
the diff. (1) *Chart model and window types compile in
`OpticalConstructor.Controls`*: `ExperimentChart.fs` and `ChartWindow.fs` now
live in Controls under `namespace OpticalConstructor.Controls`, with the pure
model registered before the ScottPlot seam in the fsproj Compile order, and
the `ScottPlot.Avalonia` 5.1.59 PackageReference added to Controls — the same
version both hosts already used, exactly as the spec demanded. This is a real
move (delete + re-add under a new namespace, project-reference resolution),
not file-linking, so one type identity is shared. (2) *Both hosts
re-pointed*: `ChartFontTests.fs` / `ChartStyleTests.fs` opens changed from
`OpticalConstructor.TestWindows.*` to `OpticalConstructor.Controls.*`;
`TableAndElementRotationView.fs` and `ExperimentControlsTests.fs` needed zero
edits because their existing `open OpticalConstructor.Controls` resolves the
moved types — the green build gate confirms this resolution. (3) *No chart
type left declared in TestWindows*: a solution-side grep shows only usages in
`TableAndElementRotationView.fs` (resolving through Controls) and fsproj
comments; both original files are deleted.

The `ChartSettings.fs` reconciliation is the one judgment call, and the
worker's bounded interpretation is defensible. The spec says "reconcile ...
onto the one shared model (re-point Ui, do not grow a third settings type)".
The diff hoists the pop-out window's private hex→`ScottPlot.Color` mapping
into a public shared `ChartRender.colorOf`, makes Ui's
`scottPlotColor` delegate to it (eliminating the literal duplication), and
documents the one-shared-model relationship — while leaving the
`ChartSettings` record intact because its surface is pinned by
`OpticalConstructor.Tests/ChartSettingsTests.fs`, a project outside this
slice's `touches`. No third settings type was introduced. The deeper merge of
the axis/legend/series concepts belongs with steps 018/019, which restructure
`ChartStyleState`/`SeriesStyle` anyway; the impl-log records this decision
explicitly, which is exactly the project prompt's required behavior for an
ambiguity.

On test coverage of new public surface: the only genuinely new public symbol
is `ChartRender.colorOf`. It is a pure hoist of an already-exercised private
function — the delegating body is byte-identical (`ScottPlot.Color.FromHex`)
— and it is transitively exercised by existing tests in the diff's blast
radius: the `ExperimentControlsTests` "ChartWindow constructs and really
renders a live experiment chart" test drives `rebuildPlot`, which calls
`ChartRender.colorOf` at all three of its call sites, and `ChartSettingsTests`
passes unchanged against the delegating module. A pure move/refactor of
already-tested code does not trigger the new-surface test obligation, and the
existing suites hold exactly at baseline with zero failures.

The SoW and impl-log line up with the diff file-for-file (nine product files
touched; `git status` matches the impl-log's list exactly). The Gotchas are
housekeeping, not hidden problems: the CRLF-in-legacy-blobs issue was
detected and LF-normalized before commit, and the stale system-prompt path in
the worker task file is a recurring supervisor-side template drift (slices
015/016 hit the same), not a slice defect. The noted
`ExperimentControls.ChartSeries` / `ExperimentChart.ChartSeries` structural
twin is pre-existing, correctly identified as out of this slice's stated
scope, and queued for the Part D follow-ups.

## Verdict

done-green

```json
{"verdict": "done-green", "rationale": "All five gates pass at the 016 baselines with no critic findings. Verified against the diff: the chart model (ExperimentChart.fs) and ScottPlot seam (ChartWindow.fs) are REAL-MOVED into OpticalConstructor.Controls under the Controls namespace with model-before-seam compile order and the ScottPlot.Avalonia 5.1.59 reference relocated; no chart type remains declared in TestWindows; both hosts re-pointed (ChartFontTests/ChartStyleTests opens edited, TableAndElementRotationView/ExperimentControlsTests resolve via existing opens); Ui's ChartSettings.scottPlotColor delegates to the new shared ChartRender.colorOf with no third settings type. The only new public symbol, ChartRender.colorOf, is a pure hoist of an already-exercised private function, transitively covered by the existing ChartWindow render tests and the unchanged ChartSettingsTests. SoW and impl-log match the diff file-for-file; the bounded ChartSettings reconciliation (record surface pinned by out-of-touches ChartSettingsTests, deeper merge deferred to 018/019) is a defensible recorded interpretation.", "retry_hint": ""}
```
