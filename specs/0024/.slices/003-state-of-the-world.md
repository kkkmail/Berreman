# 003 state-of-the-world — Stack editing + async node-solve (Part U2)

```yaml
gates:
  berreman_unit_tests: 84
  constructor_unit_tests: 204
  ui_smoke: 1
  ui_tests: 5
```

# Where we are

Slice 003 carries Part U2 of the spec-0024 UI-wiring arc. U1 (slice 001) stood up the
root MVU loop and a read-only `stack` panel; U5 (slice 002) added the renderer-host
seam and chart panel. This slice makes the `stack` panel editable and wires the
construction page's only compute-bearing interactive effect: a stack edit marks the
affected sub-tree busy and re-solves it OFF the UI thread, then refreshes dependent
views from `NodeSolved` with no manual reload. It also surfaces the delete-confirmation
gate and single-level undo. The async node-solve `Cmd` pattern (busy → off-thread solve
→ marshaled `NodeSolved`) established here is the §0.4 marshaling discipline U7's fit
`Cmd` reuses.

# What's working

- Make the `stack` panel editable in `ConstructionView.fs`: render per-layer and
  stack-level controls that dispatch every `StackEditor.StackMsg` as
  `Construction (EditStack (path, stackMsg))` through the frozen `ConstructionPage.update`.
- Attach the async node-solve `Cmd` in `Shell.update`: run `ConstructionPage.solveSubtree`
  off the UI thread and marshal `Construction (NodeSolved map)` back via
  `Dispatcher.UIThread.Post` (§0.4), driving the per-node busy indicator from `isNodeBusy`.
- Surface the delete-confirmation gate (`confirmationPrompt`) with same-row Confirm/Cancel
  positive/negative CTAs and single-level undo gated by `canUndo`.
- Add two `ui-tests` construction-edit view tests (AC-U2.1 busy→solve→refresh and
  AC-U2.2 delete-confirm + undo); keep every slice gate green.

# Tests

All gates in the slice roster pass locally (run in order; first failure short-circuits):

- `build` — solution builds, 0 errors (only pre-existing MSB3277 WindowsBase/WebView2
  version-conflict + FS1125 warnings, inherited not introduced).
- `unit-tests` (BerremanTests) — 84 passed / 5 skipped; baseline `berreman_unit_tests = 84`.
- `constructor-unit-tests` — 204 passed; the construction page / stack editor stay
  Avalonia-free (the headless dependency lives only in the UI test project).
- `ui-smoke` — 1 passed: every panel/page (incl. the now-editable `stack` panel) renders
  one frame headlessly without throwing.
- `ui-tests` — 5 passed: the 3 prior tests (stack read path + 2 chart) plus 2 new ones —
  a simulated "Add layer" dispatches `Construction (EditStack …)`, the edited node shows
  busy via `isNodeBusy`, the async solve `Cmd` marshals `NodeSolved` back so busy clears
  and per-node results refresh (AC-U2.1); and the delete-confirm gate renders same-row
  Confirm/Cancel + Undo, Confirm dispatches `ConfirmDeleteNode`, and the frozen `update`
  removes the sub-tree leaving undo available (AC-U2.2).

# Architecture

- `ConstructionView.stackPanel` follows the slice-002 `ChartView.chartPanel` precedent:
  it takes the `ConstructionPage.Model` it renders plus a `ConstructionPage.Msg -> unit`
  dispatch (NOT `RootModel`), so it composes under `Shell` without a module cycle. `Shell`
  passes `RootMsg.Construction >> dispatch`. The view re-implements NO stack mutation —
  every edit routes the frozen `ConstructionPage.update` / `StackEditor.applyStackMsg`
  (§0.1 / Non-requirements).
- The async node-solve `Cmd` lives entirely in `Shell.update` (the host layer): a
  busy-marking `Construction` transition (`EditStack`/`AttachChild`/`ConfirmDeleteNode`)
  attaches `nodeSolveCmd`, which runs `solveSubtree` on `Task.Run` and marshals
  `NodeSolved` back via `Dispatcher.UIThread.Post`. The root model holds no
  `CancellationTokenSource` or solve handle (§0.5); it stays pure/serializable.
- Per-node busy granularity is driven by the `busy` set the frozen `ConstructionPage.update`
  already populates on `EditStack` (via `editNode`); `isNodeBusy path model` reads it.
- The delete-confirmation gate and undo are rendered from the existing Avalonia-free
  presentation helpers (`confirmationPrompt`, `canUndo`), keeping the page's UX
  commitments unit-testable and the view a thin binding.

# Deferred

- `AttachChild`/`ConfirmDeleteNode` do not mark `busy` in the frozen
  `ConstructionPage.update`, so those transitions re-solve (for refresh) without a busy
  flash; richer busy semantics for attach/delete would require editing the frozen module
  (out of scope, §0.1).
- The model-driven sweep refresh into the chart/results panels from `NodeSolved` results
  (U4 sources lift + U7 sweep `Cmd`) stays deferred; this slice proves the per-node
  solve/refresh cycle for the construction page.
- Richer stack-edit affordances (drag-reorder, inline thickness/material editors, medium
  pickers) — this slice wires one control per `StackMsg` to prove the edit-dispatch path;
  full editor chrome is later UI polish.
- The clone-swap decision (§0.2 / spec 0022 §A.9 / AC-A8) — untouched.

# Gotchas

- **§0.4 marshaling is mandatory.** The solve runs on `Task.Run` (background); the
  completion dispatch MUST go through `Avalonia.Threading.Dispatcher.UIThread.Post`. A
  direct cross-thread `dispatch` is a defect. The AC-U2.1 test drives the real `Cmd` and
  pumps `Dispatcher.UIThread.RunJobs()` until the marshaled `NodeSolved` lands.
- **The delete gate only opens for a non-root node with descendants.** `RequestDeleteNode`
  routes a childless node straight to deletion; only a sub-tree opens `confirmationPrompt`.
  The AC-U2.2 test builds a two-deep tree via `AttachChild` so the middle node has a child.
- **`ui_tests` baseline rose 3 → 5** (the two new construction-edit tests); the next
  slice's baseline should track this.
- `stackPanel`'s signature changed (added `dispatch`); the slice-001 read-only view test
  call site now passes `ignore`.

# Changelog

- 2026-06-01 — Slice 003: made the `stack` panel editable (`ConstructionView.fs` — per-layer
  + stack-level controls dispatching every `StackEditor.StackMsg` through the frozen
  `ConstructionPage.update`, a per-node busy indicator via `isNodeBusy`, the
  delete-confirmation gate via `confirmationPrompt` with same-row Confirm/Cancel CTAs, and
  single-level undo via `canUndo`); attached the §0.4-marshaled async node-solve `Cmd` in
  `Shell.update` (off-thread `solveSubtree` → `Dispatcher.UIThread.Post` → `NodeSolved`);
  threaded `RootMsg.Construction >> dispatch` into `stackPanel`; added 2 `ui-tests`
  construction-edit view tests (AC-U2.1 / AC-U2.2). All slice gates green (build 0 errors;
  berreman 84, constructor 204, ui-smoke 1, ui-tests 5).
