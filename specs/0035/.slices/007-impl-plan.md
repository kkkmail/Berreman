# Step 007 — IMPLEMENT — impl-plan

## Goal

Make the generic Controls ribbon (`OpticalConstructor.Controls/Ribbon.fs`)
host **only the ACTIVE bay's content in one stable, keyed slot**, instead of
rendering every bay's pane with `IsVisible` toggles. Key the slot by the active
bay name so FuncUI patches a pane only against its own previous self and never
recycles a styled, named control across two DIFFERENT bays (the "Cannot set
Name : styled element already styled" hazard the current all-panes-present
layout documents and works around).

## Approach

`Ribbon.view` (Ribbon.fs:90) currently builds `panes` — one `Border` per bay,
each `Border.name (UiIds.pane b.name)` + `Border.isVisible (b.name = activeName)`
— and stacks them all. Replace that list with a **single** content slot that
holds only `activeBay`'s content:

- One `Border` named `UiIds.pane activeName`, padded as before, whose child is
  the active bay's `content`.
- Key that Border with `View.withKey activeName` (Avalonia.FuncUI.DSL). FuncUI's
  diff (`VirtualDom.Differ.diff`) recreates a view when its `ViewKey` changes
  (`KeyDidChange = true` → the patcher `create`s a fresh control at that list
  index instead of `patch`ing the old one). So switching bays disposes the old
  pane and builds a fresh one — no styled-control recycling.
- The tab strip and `activeBay` selection are unchanged. `UiIds.pane` now names
  the single active slot (its doc comment updated accordingly).

The empty-`bays` edge case keeps a keyed placeholder slot so the ribbon always
renders something.

## Mechanism note (deviation from the slice's parenthetical)

The slice says "SubPatchOptions.OnChangeOf the active name". `SubPatchOptions`
is a **subscription**-patch control (it governs event-handler re-subscription,
e.g. the tab's `onPointerPressed`), and does not apply to content/child attrs.
The view-level equivalent the slice intends — "key the slot by the active bay
name so FuncUI patches a pane only against its own previous self" — is
`View.withKey` (FuncUI's `IView.ViewKey`), confirmed against the FuncUI
2.0-preview1 assembly: `diff` returns `KeyDidChange=true` when `last.ViewKey <>
next.ViewKey`, and `shouldPatch` is false whenever `KeyDidChange`, forcing a
fresh `create`. Recorded in the impl-log Gotchas.

## Files to modify

- `Berreman/OpticalConstructor/OpticalConstructor.Controls/Ribbon.fs` — the
  `view` function body + the `view`/`UiIds.pane` doc comments.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/RibbonPaneTests.fs`
  (new) — headless test: selecting each bay in turn realizes exactly that bay's
  content and no other pane, with no recycling error.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/OpticalConstructor.Ui.Tests.fsproj`
  — add the new test file to the compile list.

## Risks

- The FuncUI keying API had to be confirmed by decompiling the assembly (no
  prior use in-repo). Mitigated: verified `View.withKey : string -> IView<'v> ->
  IView<'v>` lives in `Avalonia.FuncUI.DSL` (already opened by Ribbon.fs) and the
  diff honours `ViewKey`.
- Existing headless tests that switch bays on the live Elmish loop
  (`WireUiCompositionTests` sweeps every bay via tab clicks; `MainWorkbenchTests`;
  the `TableAndElementRotationTests` recycling regression) must keep passing —
  they operate on the ACTIVE bay only after each switch, which the single keyed
  slot serves, and they render frames without throwing. No test references a
  pane id (`UiIds.pane`/`RibbonPane_`) directly, so renaming the slot to the
  single-active shape breaks nothing.
