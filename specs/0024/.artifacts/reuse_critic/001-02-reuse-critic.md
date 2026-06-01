# Reuse critique -- 001.slice-md cycle 3

## Coverage

- Helper roots walked: `C:\GitHub\Berreman`, focused on the changed surface and its
  neighbours: the new `Shell.fs`/`ConstructionView.fs` and the `OpticalConstructor.Ui.Tests`
  project (re-read this cycle because attempt 03 reworked `SmokeTests.fs` + `TestApp.fs`);
  `OpticalConstructor.Ui` (`AppShell.fs`, `ConstructionPage.fs`, `UserEnvironment.fs`,
  `StackEditor.fs`); `OpticalConstructor.Storage` (`RecentFiles.fs`); the existing
  `OpticalConstructor.Tests` suite (to confirm no reusable headless scaffold predates this).
- Files inspected: ~16/200 (no cap trip).
- Extensions: `.fs`, `.fsproj`, `.slnx`, `.md`.

## Findings

The cycle-2 verdict still holds for the core surface: the diff reuses the existing pure
seams well — `Shell.update`/`updateShell` route theme/panel edits through
`AppShell.toggleTheme` / `setPanelVisible` / `dockPanel`; `Shell.view` reuses
`AppShell.visiblePanels` and the now-public `AppShell.toDock`; `ConstructionView.selectedNode`
resolves the selection through `ConstructionPage.tryGetNode`; the read path reuses
`StackEditor.layerRowLabels`; and the seed is `Templates.bandpassFilter ()` (the cycle-1
fix, still in place). What is new *this* cycle is the attempt-03 rework of the headless
test files, which is where the one fresh finding sits.

### F1: PanelViewTests re-implements the SmokeTests headless render scaffold (new this cycle)

- **Worker added:** the inline `Window()` + `Component(fun _ctx -> ...)` + `window.Show()` +
  `Dispatcher.UIThread.RunJobs()` + `window.Close()` sequence in
  `PanelViewTests.fs:22-26,39`.
- **Existing helper:** `SmokeTests.renderInWindow`, at
  `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/SmokeTests.fs:30-36`, which already
  encapsulates exactly that "host a view in a headless `Window`, show it, flush the
  dispatcher, assert visible, close" sequence. It is a sibling file in the same new test
  project, so this is reachable shared scaffolding, not a cross-boundary reach.
- **Why it matters:** the headless-render idiom is now written twice. The slice hand-off
  promises every later slice "adds its own per-panel view test", so this five-line
  incantation is on track to be copied across U2-U8; any future change to how a headless
  frame is driven (a `RunJobs` timeout, a try/finally close, a scaling option) would then
  have to be chased across N copies. `renderInWindow` is `private` to `SmokeTests` and
  returns `unit`, so `PanelViewTests` can neither call it nor keep the `Window` handle it
  needs for its `GetVisualDescendants` query — which is exactly why it forked rather than
  reused.
- **Suggested action:** extract a single shared headless-render helper alongside the
  existing `HeadlessSession` module in `TestApp.fs` (the natural home — it already owns the
  session driver both test files reuse), shaped to return the live `Window` for the
  descendant-query tests (e.g. `mountForInspection : IView -> Window`) with a thin
  `renderOnce : IView -> unit` wrapper for the smoke test, and have both call sites use it.
  This reuses scaffolding the diff already committed to rather than designing a net-new
  abstraction.

### F2: `Shell.defaultWorkingFolder` path-convention divergence (carried over from cycle 2 — already adjudicated)

- **Worker added:** `Shell.defaultWorkingFolder` (`Shell.fs:99-102`), `MyDocuments \ "OpticalConstructor"`.
- **Existing helper:** the per-user app-folder convention in `UserEnvironment.settingsPath`
  (`UserEnvironment.fs:279-284`) and `RecentFiles.prefsPath` (`RecentFiles.fs:28-33`), both
  `ApplicationData \ "Softellect" \ "Berreman" \ "OpticalConstructor"`.
- **Why it matters:** raised in the cycle-2 critique and left as-is (intentional: a user's
  project-document folder vs. the app's private settings store is a genuinely different
  purpose, none of the existing helpers is a drop-in, and U1 writes nothing here). Restated
  only so the judge sees nothing changed it this cycle; no new action.
- **Suggested action:** leave as-is and keep the documenting comment, per the cycle-2 read.

For the record, not findings: `Shell.panelTitle` (`Shell.fs:173-180`) has no existing id→title
mapping to reuse (the `PanelState` record carries only the id), so it is net-new; and
`Shell.panelView` (`Shell.fs:192-202`) resembling the `AppShell.panelView` the same diff
*removed* is a removed-helper move, explicitly out of this critic's scope.

## Bottom line

The substantive seams remain cleanly reused and the cycle-1/cycle-2 items are settled. The
one fresh observation (F1) is intra-project test scaffolding duplicated between the two new
test files — a cheap extraction that pays off before the per-panel view-test pattern
proliferates across U2-U8, but bounded and non-blocking on its own. My read: not
re-spawn-worthy; F1 is a worthwhile follow-up cleanup and F2 is already-adjudicated noise —
the judge decides routing.
