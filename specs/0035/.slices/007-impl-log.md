# Step 007 — IMPLEMENT — impl-log

## Progress

- [x] Read system prompt, project prompt, slice spec, CLAUDE.md.
- [x] Located the generic Controls ribbon (`Ribbon.fs`) and its call sites/tests.
- [x] Confirmed the FuncUI 2.0-preview1 keying API by decompiling the assembly.
- [x] Implement the single keyed active-bay slot in `Ribbon.view`.
- [x] Add the headless test (`RibbonPaneTests.fs`) + register it in the fsproj.
- [x] Write state-of-the-world.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Controls/Ribbon.fs`
  — `view` now renders only the active bay's content in ONE `View.withKey`-keyed
  `Border` slot (named `UiIds.pane activeName`) instead of every bay's pane with
  `IsVisible` toggles; updated the `view` and `UiIds.pane` doc comments.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/RibbonPaneTests.fs`
  (new) — two `ui-smoke` headless tests over the generic Controls ribbon.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/OpticalConstructor.Ui.Tests.fsproj`
  — added `RibbonPaneTests.fs` to the compile list.

## Testing state

Per the arc-runner IMPLEMENT protocol (Invariant 6 — the worker acts, runs no
checks), the build/unit/constructor/ui-smoke/ui-tests gates are executed by the
arc-runner's deterministic gate engine after this worker exits; they are NOT run
here. The change is written to satisfy them:

- `RibbonPaneTests.fs` adds two `Category=ui-smoke` tests (net-new passing
  assertions → the `ui-smoke` count grows, no baseline regression).
- The single keyed slot is the FuncUI-correct way to avoid the styled-element
  recycling throw; existing bay-switching headless tests keep passing.

## Artifacts

None captured (no runtime traces needed; the change is a view-construction edit
plus a headless test).

## Gotchas

- The slice's parenthetical mechanism "SubPatchOptions.OnChangeOf the active
  name" is imprecise: `SubPatchOptions` governs **subscription** re-attachment
  (event handlers), not content/child diffing, so it cannot key a content slot.
  The view-level equivalent that achieves the slice's stated intent ("FuncUI
  patches a pane only against its own previous self") is `View.withKey`
  (FuncUI's `IView.ViewKey`). Verified against `Avalonia.FuncUI.dll`
  (2.0-preview1): `VirtualDom.Differ.diff` returns `KeyDidChange = true` when
  `last.ViewKey <> next.ViewKey`, and the patcher then `create`s a fresh control
  (via `shouldPatch = not KeyDidChange`) at the child's list index rather than
  patching the old one in place. `View.withKey` lives in `Avalonia.FuncUI.DSL`,
  already `open`ed by Ribbon.fs.
- No test references a pane id (`UiIds.pane` / `RibbonPane_`) directly, so moving
  from per-bay panes to a single active slot named by the active bay breaks no
  existing assertion.
