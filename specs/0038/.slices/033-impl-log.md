# Step 033 — IMPLEMENT — impl-log

## Progress

- [x] MaterialEditorView.fs — dirty detection + exit confirm (model/update/view)
- [x] MaterialEditorWindow.fs — OnClosing chrome gate
- [x] SampleEditorView.fs — dirty detection + exit confirm (model/update/view)
- [x] SampleEditorWindow.fs — OnClosing chrome gate
- [x] MaterialEditorWindowTests.fs — update existing + add new proofs
- [x] SampleEditorWindowTests.fs — update existing + add new proofs

## Files modified

- `OpticalConstructor.Ui/MaterialEditorView.fs` — `UiIds.exitConfirm/discardButton/
  keepEditingButton`; `ExitPrompt` DU; `MaterialEditSnapshot`; Model `initialEdit` +
  `exit`; Msg `DiscardConfirmed` + `KeepEditing`; `currentEdit`/`isDirty`; `init`
  captures the snapshot; `update` gates `CancelClicked` and adds the two arms; view
  `exitConfirmRow`/`actionsRow` route the bottom action area.
- `OpticalConstructor.Ui/MaterialEditorWindow.fs` — `mkSimple`→`mkProgram`;
  `latestModel`/`confirmedClose`; guarded `requestClose`; `OnClosing` override.
- `OpticalConstructor.Ui/SampleEditorView.fs` — the mirror of the material changes.
- `OpticalConstructor.Ui/SampleEditorWindow.fs` — `latestModel`/`confirmedClose`;
  guarded `requestClose`; the update-lambda mirrors the model; `OnClosing` override.

## Testing state

Complete. Both editors carry the dirty/confirm behaviour with pure + headless proofs.
Test files touched:
- `MaterialEditorWindowTests.fs` — updated the dirty-Cancel portion of
  "Save adds a NEW material … and Cancel writes nothing"; added 2 pure model tests
  and 3 headless (`Category=ui-smoke`) proofs.
- `SampleEditorWindowTests.fs` — updated the dirty-Cancel portion of
  "Save persists … and Cancel writes nothing"; retargeted the "Cancel discards …"
  headless test to the confirm→Discard flow; added 2 pure model tests and 3 headless
  proofs.

Net additions (for the count_at_least gates): `ui_smoke` +6 → 170; `ui_tests` +4 → 436;
`berreman_unit_tests` / `constructor_unit_tests` unchanged.

Gates are NOT run in-session (Invariant 6 — the arc-runner gate engine runs them after
exit).

## Artifacts

None.

## Gotchas

- **The gate keys on `not e.IsProgrammatic`, NOT a confirmed-close flag.** An earlier
  draft gated every non-confirmed `Close()`; that would leak windows, because existing
  headless tests click ladder toggles (which now dirty the editor) and then call
  `window.Close()` for teardown, and `HeadlessSession` is a single assembly-wide
  session with no per-test window disposal. Gating only the genuine chrome close
  (non-programmatic) leaves every existing programmatic `Close()` behaving as before.
- The OS title-bar X is unreachable through `Avalonia.Headless`, so `OnClosing`
  delegates to a public `ChromeCloseIntercepted()` seam the headless proof drives
  directly (the `not e.IsProgrammatic` / `e.Cancel <- true` glue is the only untested
  line).
- Avalonia is 12.1.0; `Window.OnClosing(WindowClosingEventArgs)` raises the `Closing`
  event, and the override calls `base.OnClosing` ONLY on the allowed path (so FuncUI's
  host teardown fires on real closes, not on an intercepted one). Verified against the
  package XML doc.
- `MaterialEditorWindow` moved `mkSimple`→`mkProgram` to mirror the latest model into a
  window field the chrome gate reads (Sample already threaded its dispatch this way).
