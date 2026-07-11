# Step 033 — IMPLEMENT — impl-plan

## Goal

Add unsaved-edit confirmation to BOTH the Material editor and the Sample editor.
A dirty editor must not close silently through Cancel OR the window chrome (X);
a pristine editor closes immediately. Verified headless.

## Approach

**Dirtiness (no flags).** Capture the structurally-comparable slice of the model
at `init` — the identity facets plus the ladder/stack edit state, excluding the
function-valued context, the reference-compared preview preset, and pure-UI state
(selection, hidden series, collapsed groups, transient entry texts). `dirty =
initialEdit <> currentEdit m` (all components are plain records/DUs with structural
equality; the domain proxies that carry `[<ReferenceEquality>]` are excluded).

- Material `currentEdit m = (name, description, category, editor)`
  (`MaterialComplexityEditState`).
- Sample `currentEdit m = (name, description, substrate, editor.structure)`
  (`SampleStructure`).

**Exit interception.** A named `ExitPrompt = Editing | ConfirmingDiscard` on each
model. Two exits both route through the pure update:

- `CancelClicked` (the Cancel button): dirty → `{ m with exit = ConfirmingDiscard }`;
  pristine → `requestClose ()` (close silently, as today).
- The window chrome via `OnClosing` on each `HostWindow`: a window-level
  `confirmedClose` flag guards it. `requestClose` sets `confirmedClose <- true`
  before `this.Close()`, so every app-initiated close (Save success, Discard,
  pristine Cancel) is allowed through. A chrome X (or any external `Close()` while
  `confirmedClose = false`) on a dirty model cancels the OS close and dispatches
  `CancelClicked`, which shows the same confirm surface — equally gated. The window
  observes the latest model through a mutable field updated each Elmish step
  (parallel to the existing `mutable dispatch` capture), so `OnClosing` can read
  `isDirty` without a re-entrant `Close()`.

**Confirm surface.** While `exit = ConfirmingDiscard`, the bottom action area
renders `Discard changes` (negative styling, `cancelBackground`) / `Keep editing`
(positive styling, `saveBackground`) in one row. `DiscardConfirmed` →
`requestClose ()` (close without saving). `KeepEditing` → `{ m with exit = Editing }`
(back to the editor).

## Files to modify

- `OpticalConstructor.Ui/MaterialEditorView.fs` — UiIds, `ExitPrompt`, Model fields
  (`exit`, `initialEdit`), Msg (`DiscardConfirmed`/`KeepEditing`), `currentEdit`/
  `isDirty`, `init`, `update` (Cancel + two new arms), view confirm surface.
- `OpticalConstructor.Ui/MaterialEditorWindow.fs` — `mkSimple`→`mkProgram`,
  `latestModel`/`confirmedClose`, guarded `requestClose`, `OnClosing` override.
- `OpticalConstructor.Ui/SampleEditorView.fs` — the mirror of the material changes.
- `OpticalConstructor.Ui/SampleEditorWindow.fs` — `latestModel`/`confirmedClose`,
  guarded `requestClose`, `OnClosing` override.
- `OpticalConstructor.Ui.Tests/MaterialEditorWindowTests.fs` — update the existing
  dirty-Cancel assertion; add pure + headless confirm/gating proofs.
- `OpticalConstructor.Ui.Tests/SampleEditorWindowTests.fs` — same.

## Risks

- Existing tests assert a dirty Cancel closes silently — those must be updated to
  the new confirm semantics (the acceptance intentionally changes Cancel's meaning).
- `OnClosing` must not call `base.OnClosing` when it cancels (FuncUI HostWindow may
  tear the Elmish loop down on a real close) — call base only on the allowed path.
- Headless `window.Close()` must honour `e.Cancel`; assert the confirm surface plus
  `IsVisible` rather than relying solely on close timing.
