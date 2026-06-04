# 003 impl-log — Stack editing + async node-solve (Part U2)

## Progress

- [x] Read system prompt, project prompt, slice spec, prior SoW (001/002).
- [x] Surveyed frozen seams: `ConstructionPage` (update/solveSubtree/isNodeBusy/
      confirmationPrompt/canUndo), `StackEditor` (StackMsg/applyStackMsg), `BeamTree`.
- [x] Wrote impl-plan + impl-log skeleton.
- [x] R-1/R-3: editable `stackPanel` in `ConstructionView.fs`.
- [x] R-2: async node-solve `Cmd` in `Shell.fs` (§0.4-marshaled).
- [x] Updated slice-001 view test call site for the new signature.
- [x] Added `ConstructionEditTests.fs` (AC-U2.1 / AC-U2.2).
- [x] Ran all slice gates green.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Ui/ConstructionView.fs` (edit) —
  edit-dispatch path + busy/confirm/undo UI.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/Shell.fs` (edit) — async
  node-solve `Cmd` attached in the root `update`, marshaled per §0.4; thread the
  `Construction` dispatch into `stackPanel`.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/PanelViewTests.fs` (edit)
  — call `stackPanel` with the new `dispatch` argument.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/ConstructionEditTests.fs`
  (new) — AC-U2.1 busy→solve→NodeSolved and AC-U2.2 delete-confirm + undo tests.

## Decisions

- The frozen `ConstructionPage.update` marks `busy` only on `EditStack` (via
  `editNode`); `AttachChild`/`ConfirmDeleteNode` change the tree without setting
  `busy`. Per §0.1 these modules are not edited, so `isNodeBusy` reflects that
  existing behaviour. The root `update` still attaches the solve `Cmd` for all
  three enumerated transitions (R-2) so dependent views refresh from `NodeSolved`;
  for `ConfirmDeleteNode` it re-solves the *parent* of the removed node (the
  removed path no longer resolves).
- `stackPanel` gained a `dispatch : ConstructionPage.Msg -> unit` parameter
  (mirrors the slice-002 `ChartView.chartPanel` shape); `Shell` passes
  `RootMsg.Construction >> dispatch`. The slice-001 read-only view test call site
  was updated to pass `ignore`.

## Testing state

- `build`: green (0 errors).
- `unit-tests` (BerremanTests): 84 passed / 5 skipped (baseline `berreman_unit_tests = 84`).
- `constructor-unit-tests`: 204 passed.
- `ui-smoke`: 1 passed.
- `ui-tests`: 5 passed (3 prior + 2 new construction-edit tests).

## Artifacts

- No captured artifacts needed; all gates ran clean to stdout.

## Gotchas

- §0.4: the solve runs on `Task.Run`; the completion dispatch is marshaled with
  `Avalonia.Threading.Dispatcher.UIThread.Post`. A direct cross-thread dispatch
  would be a defect. The AC-U2.1 test drives the real `Cmd` and pumps
  `Dispatcher.UIThread.RunJobs()` until the marshaled `NodeSolved` lands.
- §0.5: the `Cmd` is host-layer only; the root model gains no non-serializable handle.
- The delete-confirmation gate only opens for a non-root node with descendants;
  the AC-U2.2 test builds a two-deep tree via `AttachChild` so the middle node has
  a child and routes through `confirmationPrompt`.
