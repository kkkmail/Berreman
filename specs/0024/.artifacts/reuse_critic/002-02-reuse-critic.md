# Reuse critique -- 002.slice-md cycle 1

## Coverage

- Helper roots walked: `Berreman/OpticalConstructor/OpticalConstructor.Ui/` and
  `OpticalConstructor.Ui.Tests/` (the view/test surface this slice touches), plus the
  cited frozen `Charts/` seams and slice-001 `Shell.fs` / `ConstructionView.fs`.
- Files inspected: 9/200 (focused walk on the in-scope view + test modules).
- Extensions: declared bounds were `.py,.md,.json`, which do not match this pure-F#
  project (stub task template); walked the relevant `.fs` modules directly instead and
  record the mismatch here for the judge.

## Findings

### F1: Headless mount/show/flush boilerplate re-implemented instead of a shared helper

- **Worker added:** the `Window(); window.Content <- Component(fun _ctx -> ...);
  window.Show(); Dispatcher.UIThread.RunJobs()` mount-show-flush block in
  `ChartPanelTests.fs:33-38`.
- **Existing helper:** `SmokeTests.renderInWindow`
  (`OpticalConstructor.Ui.Tests/SmokeTests.fs:30-37`) already encapsulates exactly this
  sequence (build a `Window`, wrap the view in a `Component`, `Show()`, `RunJobs()`),
  and slice 001's `PanelViewTests.fs:22-27` reimplemented the same four lines a second
  time.
- **Why it matters:** the same headless-mount incantation now lives in three places
  (`SmokeTests`, `PanelViewTests`, `ChartPanelTests`). The existing helper is `private`
  and compiled before the view tests, so it is literally unreachable from them — and it
  closes the window / asserts `IsVisible` internally, so a view test that needs to keep
  the window alive to query `GetVisualDescendants` cannot call it as written. That is
  the near-miss: a single shared, non-private mount helper that *returns* the window
  would serve all three call sites; instead the convention forked at slice 001 and this
  slice perpetuates the fork. Every future panel test (U3/U4/U6/U7/U8) will copy it
  again.
- **Suggested action:** extract a shared `mountHeadless (view: IView) : Window` (or
  promote `renderInWindow` out of `SmokeTests` into the `TestApp.fs`/`HeadlessSession`
  scaffold next to `HeadlessSession.run`) and have all three tests reuse it. Advisory —
  this is test-only scaffolding and slice 001 set the precedent the worker followed.

### F2: Visual-tree "descendants of type T" query duplicated three times

- **Worker added:** two copies of the descendant-filter pattern in `ChartPanelTests.fs`
  — `buttonByContent` at `ChartPanelTests.fs:23-26`
  (`GetVisualDescendants() |> Seq.choose (fun v -> match v with | :? Button as b ->
  Some b | _ -> None)`) and the inline button collection at `ChartPanelTests.fs:42-44`,
  which repeats the identical `Seq.choose`-by-`:? Button` shape.
- **Existing helper:** the same query shape is the established precedent at
  `PanelViewTests.fs:29-34` (the `:? TextBlock` variant). There is no extracted
  `descendantsOfType<'T>` helper in the test project, so the pattern is open-coded at
  each site.
- **Why it matters:** within this one new file the worker writes the
  `Seq.choose`-by-control-type pattern twice — `buttonByContent` could itself be built
  on the same collection the inline block produces. Across the project it now appears
  three times against two control types. This is pattern divergence: a one-line generic
  `let descendantsOfType<'T when 'T :> Visual> (w: Window) = w.GetVisualDescendants() |>
  Seq.choose (fun v -> match box v with :? 'T as t -> Some t | _ -> None)` would let
  `buttonByContent`, the inline button list, and `PanelViewTests`'s TextBlock list all
  funnel through one query.
- **Suggested action:** fold the inline collection at `ChartPanelTests.fs:42-44` into a
  call to `buttonByContent`'s underlying collection, and (ideally, with F1) lift a
  shared `descendantsOfType` into the test scaffold. Advisory; low risk.

### F3: `ChartHosts.unavailable` vs `Shell.placeholder` — overlapping placeholder, but not reusable here

- **Worker added:** `ChartHosts.unavailable` (`ChartHosts.fs:29-42`), a bordered
  "<what> — renderer unavailable" note.
- **Existing helper:** `Shell.placeholder` (`Shell.fs:189-190`) renders a
  "<title> — coming soon" `TextBlock`.
- **Why it matters / why this is NOT a finding to action:** the two are semantically
  distinct (a panel that isn't built yet vs a renderer runtime that can't host), have
  different shapes (centered bordered note with min-height vs bare TextBlock), and —
  decisively — `Shell.fs` compiles *after* `ChartHosts.fs` in
  `OpticalConstructor.Ui.fsproj`, so `Shell.placeholder` is unreachable from
  `ChartHosts` by construction. There is no existing helper the worker could have
  reused. Recorded only to show it was considered and dismissed; no action suggested.

## Bottom line

The slice's *production* reuse is strong: `ChartView` calls the frozen `Plot1DView.*`,
`ChartSettings.applyToScottPlot`/`applyToPlotly`, `Readout.peak`/`Markers`,
`StandardSystems`/`StandardLightVariables`/`SeriesData` seams rather than reinventing
them, reuses the existing `RootModel.chart`/`markers` fields, and follows the
`ConstructionView.stackPanel` sub-view precedent — no production-code duplication found.
The only substantive findings (F1, F2) are duplicated headless-test scaffolding that
already forked at slice 001 and which this slice merely follows; both are advisory,
test-only, and arguably below the bar to force a re-spawn. I read them as cleanup the
judge could fold into a later test-scaffold consolidation rather than a blocker for 002.
