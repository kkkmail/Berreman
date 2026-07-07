# Impl log — spec 0033, slice 017 (IMPLEMENT, AC-D1)

REAL-MOVE of the renderer-neutral chart model (`ExperimentChart.fs`) and the ScottPlot
seam (`ChartWindow.fs`) from `OpticalConstructor.TestWindows` into
`OpticalConstructor.Controls`; re-point both hosts; reconcile Ui's `ChartSettings.fs`
onto the one shared model.

## Progress

- [x] Read protocol, project prompt, slice spec; surveyed every consumer of
      `ExperimentChart` / `ChartFont` / `ChartStyle` / `ChartWindow` / `ChartWindowIds`
      and of Ui's `ChartSettings` solution-wide.
- [x] Moved the two files by byte-copy + targeted edit (namespace →
      `OpticalConstructor.Controls`; `open …Controls.ExperimentChart` in the window),
      deleted the TestWindows originals. LF-normalized the two new files (the legacy
      blobs carried CRLF; `.gitattributes` pins `*.fs` to `eol=lf`).
- [x] `OpticalConstructor.Controls.fsproj`: `ExperimentChart.fs` + `ChartWindow.fs`
      Compile entries (model before seam) and the `ScottPlot.Avalonia` 5.1.59
      PackageReference (the same version both hosts already used); header comment
      updated (the library is now Avalonia + FuncUI + ScottPlot).
- [x] `OpticalConstructor.TestWindows.fsproj`: dropped the two Compile entries and the
      now-orphaned direct `ScottPlot.Avalonia` reference (no TestWindows source uses
      ScottPlot anymore; its compile/runtime assets flow transitively through the
      existing Controls project reference at the same 5.1.59).
- [x] Re-pointed Ui.Tests opens: `ChartFontTests.fs` and `ChartStyleTests.fs` now open
      `OpticalConstructor.Controls[.ChartFont/.ExperimentChart/.ChartStyle]`.
      `ExperimentControlsTests.fs` and `TableAndElementRotationView.fs` already carried
      `open OpticalConstructor.Controls`, so they re-pointed with zero edits.
- [x] Reconciled Ui: hoisted the pop-out window's private hex→`ScottPlot.Color` mapping
      into the public `ChartRender` module beside the moved window; Ui's
      `ChartSettings.scottPlotColor` now delegates to it (one hex-parse seam
      solution-wide); ChartSettings' module doc records that the Controls-hosted
      `ExperimentChart`/`ChartStyle` spine is the ONE shared model and ChartSettings
      stays the Ui-side §H.1 adapter — no third settings type introduced anywhere.
- [x] Advisory gate runs — all five green at baseline (see Testing state).

## Files modified

- add `Berreman/OpticalConstructor/OpticalConstructor.Controls/ExperimentChart.fs`
  (moved; only the namespace line changed)
- add `Berreman/OpticalConstructor/OpticalConstructor.Controls/ChartWindow.fs`
  (moved; namespace + model open changed; local `colorOf` hoisted into the new public
  `ChartRender.colorOf`, window uses it at its three call sites)
- delete `…/OpticalConstructor.TestWindows/ExperimentChart.fs`
- delete `…/OpticalConstructor.TestWindows/ChartWindow.fs`
- `…/OpticalConstructor.Controls/OpticalConstructor.Controls.fsproj`
- `…/OpticalConstructor.TestWindows/OpticalConstructor.TestWindows.fsproj`
- `…/OpticalConstructor.Ui/Charts/ChartSettings.fs`
- `…/OpticalConstructor.Ui.Tests/ChartFontTests.fs`
- `…/OpticalConstructor.Ui.Tests/ChartStyleTests.fs`

## Decisions

- **Move mechanics: byte-copy + edit, not rewrite** — preserves the file content
  byte-for-byte except the namespace seams, so the move reads as a move.
- **TestWindows loses its direct ScottPlot ref.** After the move no TestWindows source
  mentions ScottPlot (only comments describing the pop-out window it opens); keeping a
  direct PackageReference with a stale "chart-window host" comment would be exactly the
  residue a REAL-MOVE should clean. The assets flow transitively through the Controls
  reference; the whole solution still resolves the single 5.1.59.
- **Reconciliation depth is bounded by the pinned Tests surface.**
  `OpticalConstructor.Tests/ChartSettingsTests.fs` pins the `ChartSettings` /
  `TraceSettings` field names, `Linear`/`Log10`, `SolidLine`, `defaultValue` and the
  projection functions — and `OpticalConstructor.Tests` is NOT in this slice's
  `touches`. So the reconcile keeps the §H.1 record intact, delegates the literal
  renderer-mapping duplicate (hex→Color) to the shared `ChartRender`, and records the
  one-shared-model relationship in the module doc. The deeper merge of ChartSettings'
  axis/legend/series concepts onto the `ChartStyle` spine belongs with steps 018/019,
  which reshape that spine (second Y axis) and its consumers anyway.
- **`ChartRender` is deliberately tiny** (one function): it hosts only what is
  duplicated TODAY. Step 018's right-axis work will push more ScottPlot mappings
  through this seam if it needs them shared.

## Testing state

All five gates in `017.gates` pass in local (advisory) runs; the arc-runner gate
engine re-runs them authoritatively after exit. A pure move adds no tests; every
`count_at_least` suite holds exactly at its slice-016 baseline.

- `build` — solution builds Release/x64, exit 0, 0 errors; the 92–94 warnings are the
  pre-existing MSB3277 (WindowsBase, App/Ui.Tests) and FS1125 (SeriesDataTests) noise —
  none from the moved files.
- `unit-tests` (BerremanTests) — 119 passed, 5 skipped (pre-existing), 0 failed
  (= 119 baseline; no core file touched).
- `constructor-unit-tests` — 373 passed, 0 failed (= baseline; `ChartSettingsTests`
  passes UNCHANGED against the reconciled module).
- `ui-smoke` — 58 passed, 0 failed (= baseline).
- `ui-tests` — 255 passed, 0 failed (= baseline; `ChartFontTests` / `ChartStyleTests`
  re-pointed; the `ExperimentControlsTests` ChartWindow construction/render/polar tests
  all pass against the moved types).

## Artifacts

- `specs/0033/.artifacts/017-build.log`
- `specs/0033/.artifacts/017-unit-tests.log`
- `specs/0033/.artifacts/017-constructor-unit-tests.log`
- `specs/0033/.artifacts/017-ui-smoke.log`
- `specs/0033/.artifacts/017-ui-tests.log`

## Gotchas

- The task file's system-prompt path
  `C:\GitHub\AI-Strategy-Generator\implement_worker.system-md` does not exist (same
  drift as slices 015/016); the real file is
  `AI-Strategy-Generator\src\ai_strategy_generator\multistep\implement_worker.system-md`.
- **The committed originals carried CRLF blobs** (predating the `.gitattributes`
  policy); a byte-copy therefore produced CRLF copies. The two new Controls files were
  LF-normalized before the final build, so they commit clean. No EOL churn anywhere
  else (`git diff --numstat` = `--ignore-cr-at-eol`).
- `ExperimentControls.ChartSeries` (Controls) is a pre-existing structural twin of
  `ExperimentChart.ChartSeries` — it existed because Controls could not reference the
  TestWindows-hosted model. The move colocates them in one assembly; unifying them
  changes the ExperimentControls host contract and is NOT in this slice's stated scope
  (the slice reconciles only `ChartSettings.fs`) — left for the Part D follow-ups.
- `.manifest.state.json` (modified, with its own CRLF warning) and `.claude/`
  (untracked) are the arc-runner's / harness's own files — left alone, as in slices
  001–016.
