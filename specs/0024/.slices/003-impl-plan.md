# 003 impl-plan — Stack editing + async node-solve (Part U2)

## Approach

U1 (slice 001) renders the `stack` panel read-only; U5 (slice 002) added the
chart panel + renderer-host seam. This slice makes the stack panel editable and
wires the construction page's only compute-bearing interactive effect: a stack
edit marks the affected sub-tree busy and re-solves it OFF the UI thread, then
refreshes dependent views from `NodeSolved` with no manual reload. It also
surfaces the delete-confirmation gate and single-level undo.

Every stack mutation already lives in the frozen, Avalonia-free
`StackEditor`/`ConstructionPage` modules (§0.1). This slice only adds **view +
host-loop wiring**, re-implementing no mutation:

1. **`ConstructionView.fs` (edit) — R-1 / R-3.** Give `stackPanel` a
   `dispatch : ConstructionPage.Msg -> unit` and render edit controls that
   dispatch every `StackEditor.StackMsg` as `EditStack (path, stackMsg)`
   (add/delete/duplicate/reorder/group/rotate a layer, set incident/exit medium,
   set substrate, set layer material). Surface the per-node busy indicator via
   `ConstructionPage.isNodeBusy`, the delete-confirmation gate via
   `ConstructionPage.confirmationPrompt` with same-row positive/negative-CTA
   Confirm/Cancel (commitment 5) dispatching `ConfirmDeleteNode`/`CancelDeleteNode`,
   the `RequestDeleteNode` entry, and single-level undo gated by
   `ConstructionPage.canUndo` dispatching `Undo`.

2. **`Shell.fs` (edit) — R-2.** In the root `update`, for the busy-marking
   `Construction` transitions (`EditStack` / `AttachChild` / `ConfirmDeleteNode`)
   attach an async `Cmd` that runs `ConstructionPage.solveSubtree path root` off
   the UI thread (`Task.Run`) and marshals `Construction (NodeSolved map)` back
   onto the UI thread via `Avalonia.Threading.Dispatcher.UIThread.Post` (§0.4).
   Pass `(RootMsg.Construction >> dispatch)` into `stackPanel` (mirrors the
   slice-002 chart-panel wiring). No `CancellationTokenSource` or renderer handle
   enters the root model (§0.5).

3. **`OpticalConstructor.Ui.Tests` (new tests).** Add a `ui-tests` headless view
   test: a simulated stack edit dispatches `Construction (EditStack …)`; the
   `Shell.update` transition marks the node busy (`isNodeBusy`) and attaches a
   Cmd that, run, marshals a `NodeSolved` back so the busy clears and results
   refresh (AC-U2.1); and the delete-confirm gate renders same-row Confirm/Cancel
   + Undo and Confirm removes the sub-tree with undo available (AC-U2.2).

## Files

- Edit: `OpticalConstructor.Ui/ConstructionView.fs`
- Edit: `OpticalConstructor.Ui/Shell.fs`
- Edit: `OpticalConstructor.Ui.Tests/PanelViewTests.fs` (update the slice-001 call
  site for the new `stackPanel` signature)
- New: `OpticalConstructor.Ui.Tests/ConstructionEditTests.fs`

## Risks

- **Off-UI-thread re-solve + marshaling (§0.4).** The solve runs on `Task.Run`;
  the completion dispatch MUST go through `Dispatcher.UIThread.Post`. A direct
  cross-thread dispatch is a defect. The headless test drives the real Cmd and
  pumps `Dispatcher.UIThread.RunJobs()` to prove the marshaled `NodeSolved` lands.
- **Frozen modules (§0.1).** `ConstructionPage.fs`/`StackEditor.fs` are called,
  never edited; only the busy set they already populate (on `EditStack`) drives
  `isNodeBusy`.
