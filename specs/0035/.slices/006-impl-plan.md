# Impl-plan — slice 006 (ADD_COMPONENT — UICOMP_XDUO_0006 CategoryEditorWindow)

## Goal

Create the **Category editor window** in `OpticalConstructor.TestWindows`, beside
`MaterialEditorWindow` / `SampleEditorWindow`, over the step-4 `CategoryEditor` edit model
and the step-5 `CategoryControls` component. Ship it with headless tests in
`OpticalConstructor.Ui.Tests` that drive the real window by its UiIds over a stub
`CategoryProxy`.

Declaring project: `OpticalConstructor.TestWindows`.
Touches: `OpticalConstructor.TestWindows`, `OpticalConstructor.Ui.Tests` — nothing else
(the step-5 `CategoryControls` is used AS-IS; it is not in `touches`).

## Approach

Mirror the `SampleEditorView` / `SampleEditorWindow` pair:

- **`CategoryEditorView.fs`** — the pure Elmish MVU model + FuncUI projection.
  - `UiIds.window = "CategoryEditorWindow"` — the ONE new literal; every other id reuses the
    step-5 `CategoryControls.UiIds` constants (the how-to's "reuse the step-5 constants plus
    CategoryEditorWindow").
  - `CategoryEditorContext` — the functional-proxy Context seam: `{ categories : CategoryProxy;
    requestClose : unit -> unit }` (tests substitute a recording stub).
  - `Model` — the step-4 `CategoryEditState` (working rows + the opened-over snapshot), the
    `pendingRemove : CategoryId option` confirm-gate flag, and the inline `blockMessage`.
  - `Msg` — `BeginAdd | SetName | SaveRow | RemoveRow | CancelRow` (the per-row inline verbs)
    plus `CommitAll | CancelWindow` (the window-level Save/Cancel row).
  - `update` — routes the inline verbs through the step-4 `applyCategoryMsg`; per-row **Save**
    projects that row's add/rename intent and dispatches `CategoryProxy.addCategory` /
    `updateCategory`; per-row **Remove** is confirm-gated (arm → confirm) and dispatches
    `removeCategory`, surfacing the store's typed `CategoryStillReferenced` /
    `BuiltInNotRemovable` into `blockMessage`; **Cancel** discards the row's in-progress edit;
    a successful mutation `reload`s from `listCategories` so the snapshot tracks the store.
    The window-level **Save** flushes any staged edits through the step-4 `commit` diff, then
    closes; **Cancel** closes discarding.
  - `view` — projects `Model` → `CategoryControls.State`, injects a `CategoryControls.Handlers`
    that dispatches the `Msg`s, and pins a window-level Save/Cancel row (distinct green/red
    styling, base `CategorySaveButton` / `CategoryCancelButton` ids) below the control.
- **`CategoryEditorWindow.fs`** — the `HostWindow` composition root (Name + AutomationId =
  `CategoryEditorWindow`), mounting the MVU loop; `requestClose = this.Close`.
- Register both in `OpticalConstructor.TestWindows.fsproj` after the Material editor pair.

- **`CategoryEditorWindowTests.fs`** in `OpticalConstructor.Ui.Tests`:
  - pure contract tests (the stable window id; init seeds from the proxy);
  - pure model/`update` tests (BeginAdd row shape, Add→Save persists, blank-name refusal,
    rename incl. a built-in, referenced-remove block + store unchanged, built-in-remove block,
    Cancel discards, window Save flush/close);
  - headless `[<Trait("Category","ui-smoke")>]` proofs driving the REAL window by UiIds over a
    hand-rolled stub `CategoryProxy`: **Add** grows the list, **Rename** updates a name,
    **Remove** of a referenced category surfaces the block and leaves the list unchanged.
  - Register in `OpticalConstructor.Ui.Tests.fsproj` after `CategoryControlsTests.fs`.

## Risks / ambiguities (resolved in the impl-log Gotchas)

1. **Per-row immediate vs batch commit.** The step-5 `CategoryControls` exposes per-row
   Save/Cancel/Remove handlers, and the how-to says Remove is "confirm-gated inline" and Add
   "grows the list" — so the window commits **per row** immediately; the window-level Save/Cancel
   row additionally flushes via the step-4 `commit` and closes.
2. **Confirm gate with a fixed control.** `CategoryControls` (fixed, not in `touches`) has no
   confirm button, so Remove is armed-then-confirmed on the SAME Remove button, the block slot
   doubling as the confirm prompt.
3. **Built-in Remove is hidden by step-5.** `CategoryControls` OMITS Remove for a built-in row,
   so `BuiltInNotRemovable` cannot be reached through the UI; it is covered by a pure model test
   that drives the remove path with a built-in id.
4. **Unknown minted id after Add.** `BeginAddCategory` mints the id, so the headless Add test
   locates the new row by its empty name box and extracts the guid suffix to target Save.
