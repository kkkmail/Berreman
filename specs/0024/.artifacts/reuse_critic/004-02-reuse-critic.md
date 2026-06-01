# Reuse critique -- 004.slice-md cycle 1

## Coverage

- Helper roots walked: `Berreman/OpticalConstructor/OpticalConstructor.Ui/` and
  `.../OpticalConstructor.Ui.Tests/` (the only roots the diff touches).
- Files inspected: 8/200 — `MaterialsView.fs` (new), `MaterialsPanelTests.fs` (new),
  `ChartView.fs`, `ChartHosts.fs`, `ConstructionView.fs`, `ConstructionPage.fs` (grep),
  `ChartPanelTests.fs`, `ConstructionEditTests.fs`, `PanelViewTests.fs` (+ `Shell.fs` diff).
- Extensions: the task pinned `.py,.md,.json`, but this is a pure-F# project and the
  worker's additions are all `.fs`; I walked the `.fs` view/test siblings the diff
  actually duplicates against, since `.py/.json` carry no helpers here.

## Findings

### F1: `selectedNode` total-node resolver copied verbatim from `ConstructionView`

- **Worker added:** `MaterialsView.selectedNode`
  (`Berreman/OpticalConstructor/OpticalConstructor.Ui/MaterialsView.fs:134-137`).
- **Existing helper:** `ConstructionView.selectedNode`
  (`Berreman/OpticalConstructor/OpticalConstructor.Ui/ConstructionView.fs:33-36`).
  The two bodies are byte-for-byte identical, down to the doc-comment ("falling back
  to the tree root when the selected path no longer resolves … so the view is total"):
  both are `match ConstructionPage.tryGetNode model.selected model.project.beamTree.root
  with Some node -> node | None -> model.project.beamTree.root`.
- **Why it matters:** This is direct duplication of a non-trivial invariant — "the
  selected-path lookup must fall back to the root so the view is total." If that
  fallback policy ever changes (e.g. fall back to the nearest surviving ancestor instead
  of the root after a deletion), it now has to be changed in two view modules that will
  silently drift. The underlying lookup `ConstructionPage.tryGetNode` is already the
  public frozen seam (`ConstructionPage.fs:28`); only the total-fallback wrapper was
  re-implemented.
- **Suggested action:** Reuse one resolver. `ConstructionView.fs` is a new-from-this-arc
  view module (not a frozen §0.1 logic module), so surfacing its `selectedNode` as
  public and calling it from `MaterialsView` is permitted and keeps the fallback policy
  single-sourced; alternatively lift the 4-line wrapper next to `tryGetNode`. (Advisory
  — the judge decides; if kept duplicated, document why the materials drop-target view
  needs its own copy.)

### F2: Titled fixed-height chart-host row re-built instead of reusing `ChartView.chartRow`

- **Worker added:** the inline preview row in `MaterialsView.dispersionPreview`
  (`MaterialsView.fs:246-254`) — a vertical `StackPanel` with a bold-title `TextBlock`
  followed by `Border.create [ Border.height 220.0; Border.child (ChartHosts.webView2Host chart) ]`.
- **Existing helper:** `ChartView.chartRow`
  (`Berreman/OpticalConstructor/OpticalConstructor.Ui/ChartView.fs:163-172`), which is
  exactly "one titled, fixed-height (`Border.height 220.0`) chart row" wrapping a host
  `IView` under a bold title — the same shape, the same magic `220.0`, the same
  `Border.child host` structure. `ChartView` itself uses it for every hosted chart,
  including the sibling `ChartHosts.webView2Host` 3-D row (`ChartView.fs:187`).
- **Why it matters:** Near-miss duplication of the renderer-host row convention. The
  `220.0` host height is now hard-coded in two places; a change to the standard chart-row
  chrome (height, title weight, spacing) won't propagate. `chartRow` is `private` to
  `ChartView`, so the worker could not call it as-is — but that is an argument for
  promoting the one helper, not for cloning its body.
- **Suggested action:** Promote `ChartView.chartRow` (or extract a shared
  `titledChartRow` next to the `ChartHosts` adapters it pairs with) and have both panels
  call it. If a shared helper is judged out of scope for this slice, leave as-is and note
  the intentional duplication of the `220.0` row chrome.

### F3: "Layer N — <thickness>" row label built a second way

- **Worker added:** `MaterialsView.dropRow`'s label
  (`MaterialsView.fs:269`): `sprintf "Layer %d — %s  (drop material here)" index
  (StackEditor.displayThickness u layer.thickness)`.
- **Existing helper:** `ConstructionView.layerRow`'s label
  (`ConstructionView.fs:90`): `sprintf "Layer %d — %s" index
  (StackEditor.displayThickness u layer.thickness)`.
- **Why it matters:** Both views label a layer row by the same `"Layer %d — %s"` /
  `StackEditor.displayThickness` recipe; the worker's variant appends a drop hint. This
  is minor pattern divergence rather than functional duplication — the
  `StackEditor.displayThickness` seam *is* reused (good), only the surrounding label
  literal is forked. If the layer-row label format ever changes (unit suffix, index
  base), the two strings drift.
- **Suggested action:** Low priority. Optionally factor a `layerLabel index u layer`
  helper the two views share; otherwise leave as-is — the meaningful seam
  (`displayThickness`) is already reused, so this is acceptable to keep.

### F4: Headless test scaffolding (`mount` / `buttonContents` / `textBlocks`) re-localized

- **Worker added:** private `mount`, `buttonContents`, `textBlocks`
  (`MaterialsPanelTests.fs:28-47`).
- **Existing helper:** the same visual-tree-walk + mount recipe appears as private
  `buttonByContent` in `ChartPanelTests.fs:23-26` and `ConstructionEditTests.fs:31-34`,
  and the TextBlock-collection is inlined in `PanelViewTests.fs:28-34`; the
  `Window()` → `Component(...)` → `Show()` → `Dispatcher.UIThread.RunJobs()` mount
  sequence recurs in all three (`ChartPanelTests.fs:33-38`, `ConstructionEditTests.fs:45-47`,
  `PanelViewTests.fs:22-26`).
- **Why it matters:** This is now the fourth copy of the "walk `GetVisualDescendants`
  for buttons/textblocks" + "mount a Component and pump the dispatcher" scaffolding.
  However, the *established repo convention* is precisely that each test module privatizes
  its own visual-tree query helpers — there is **no shared test-helper module** to point
  at (the existing copies are all `private`, hence unreachable). So the worker *conformed*
  to the prevailing pattern rather than diverging from it; per the rubric this is
  borderline (no reusable existing helper to cite).
- **Suggested action:** Advisory / low. If the project wants these consolidated, a shared
  `OpticalConstructor.Ui.Tests` helper module (sibling to `HeadlessSession`, which the
  worker *did* correctly reuse) would retire all four copies. Absent that decision, the
  worker followed precedent and this is acceptable to leave.

## Bottom line

The diff reuses the substantive seams well — filtering goes only through
`MaterialLibrary.byCategory`/`byNameContains`, the drop routes the single
`StackEditor.layerMaterialDrop` resolution seam, the preview hosts through the slice-002
`ChartHosts.webView2Host`, and `HeadlessSession`/`displayThickness`/`Templates.defaultLight`
are all reused rather than reinvented. The one finding I would actually act on is **F1**:
a verbatim copy of `ConstructionView.selectedNode`, which duplicates a real "view-total"
invariant across two modules and is cheaply de-duplicated since `ConstructionView` is a
new-arc (non-frozen) module. F2 is a worthwhile but optional consolidation; F3/F4 are
low-priority notes where the worker largely followed existing precedent. None of these
are correctness defects — this reads as a clean-enough diff that does not by itself warrant
a re-spawn, but F1 is a tidy single-edit win if the judge wants it folded in.
