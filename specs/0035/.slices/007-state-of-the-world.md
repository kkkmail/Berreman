# State of the world — slice 007 (IMPLEMENT — single keyed ribbon content slot)

## Where we are

Slice 007 is a focused IMPLEMENT step in arc 0035. It reshapes the generic
`OpticalConstructor.Controls.Ribbon` — the MS-Word-style bay ribbon that hosts
every "large control" (rotation / move / render / materials / library / …) — so
it realizes **only the ACTIVE bay's content, in ONE keyed slot**, instead of
rendering every bay's pane and hiding the non-selected ones with `IsVisible`
toggles. The tab strip and the `activeBay` selection are unchanged; the change is
purely in how `Ribbon.view` builds the content region. It touches
`OpticalConstructor.Controls` (the ribbon) and `OpticalConstructor.Ui.Tests` (a
new headless proof), matching the slice's `touches`.

## What's working

- Render only the selected bay's content in one `View.withKey`-keyed slot,
  replacing the all-panes-present-with-IsVisible layout.
- Key the slot by the active bay name so a bay switch recreates the pane instead
  of recycling a styled, named control across two different bays.
- Add a headless UI test: selecting each bay in turn realizes exactly that bay's
  content and no other pane, with no styled-element recycling error.
- Keep the tab strip and `activeBay` selection unchanged; `UiIds.pane` now names
  the single active slot.

## Tests

Net-new headless coverage in `OpticalConstructor.Ui.Tests/RibbonPaneTests.fs`
(two `Category=ui-smoke` tests over the generic Controls ribbon, driven on a live
FuncUI/Elmish loop):

- `the ribbon realizes only the initially-selected bay's content in one keyed
  slot` — on first render, exactly the initial bay's pane (`UiIds.pane`) and its
  content marker are realized; no other bay's pane or content is in the tree;
  every bay's tab is present.
- `selecting each bay in turn shows exactly that bay's content and no other pane,
  with no recycling error` — walks all four synthetic bays and back to the first
  by clicking tabs (each click a real Elmish dispatch → FuncUI patch); after each
  switch the window is still visible and only the selected bay's pane + content is
  realized. The synthetic bays use SAME-typed, DIFFERENTLY-named content — the
  exact shape that would throw "Cannot set Name : styled element already styled"
  if the single slot were not keyed.

A now-stale comment in `TableAndElementRotationTests` (the existing
`reveals ONLY the selected bay's controls` proof) was corrected to describe the
single-keyed-slot behaviour; its assertions are unchanged and still hold.

Per the arc-runner IMPLEMENT protocol (Invariant 6 — the worker acts and runs no
checks), the `build` / `unit-tests` / `constructor-unit-tests` / `ui-smoke` /
`ui-tests` gates are executed by the arc-runner's deterministic gate engine after
this worker exits; they were NOT run here. The counts below are the EXPECTED
post-round baselines: this slice adds two passing `ui-smoke` tests (88 → 90) and
changes no `unit-tests` / `constructor-unit-tests` / `ui-tests` file, so no
`count_at_least` baseline can regress.

## Architecture

- **The single slot is keyed, not merely hidden.** FuncUI diffs a view by its
  `IView.ViewKey` first: when the key changes the diff reports `KeyDidChange` and
  the patcher `create`s a fresh control at that child index instead of patching
  the previous one in place (verified against `Avalonia.FuncUI.dll`
  2.0-preview1: `VirtualDom.Differ.diff` → `KeyDidChange = last.ViewKey <>
  next.ViewKey`; `shouldPatch = not KeyDidChange`). Keying the pane by the active
  bay name is therefore exactly "a pane is only ever patched against its own
  previous self" — the invariant the previous all-panes workaround secured, now
  secured with one realized pane instead of N.
- **`UiIds.pane` names the one active slot.** The id function is unchanged
  (`"RibbonPane_" + name`), but there is now a single pane in the tree, named for
  the bay currently shown; no test referenced a pane id, so the semantics shift
  is invisible to existing automation.
- **The tab strip is untouched.** `activeBay`, the tab views, and the tab's
  `SubPatchOptions.OnChangeOf active` re-subscription are unchanged; only the
  content region was rebuilt.

## Deferred

- Nothing from this slice is deferred. The single keyed slot fully replaces the
  all-panes layout; there is no remaining "round 2".
- The empty-`bays` edge case renders a keyed empty placeholder slot (kept so the
  ribbon always has a content node); no product path hits it today.

## Gotchas

- **The slice's "SubPatchOptions.OnChangeOf the active name" is imprecise.**
  `SubPatchOptions` governs SUBSCRIPTION re-attachment (event handlers, e.g. the
  tab's `onPointerPressed`); it does not apply to content/child attrs and cannot
  key a content slot. The view-level equivalent that achieves the slice's stated
  intent ("FuncUI patches a pane only against its own previous self") is
  `View.withKey` (FuncUI's `IView.ViewKey`). Implemented with `View.withKey`;
  confirmed against the FuncUI assembly's diff/patcher as above.
- **`View` is ambiguous under Ribbon.fs's opens.** `Ribbon.fs` opens both
  `Avalonia.FuncUI.DSL` (the `View` MODULE, with `withKey`) and
  `Avalonia.FuncUI.Types` (the `View<'t>` TYPE). The `withKey` call is therefore
  written fully qualified (`Avalonia.FuncUI.DSL.View.withKey`) so the module
  wins unambiguously.
- **No test asserts a pane id, so the slot rename is safe.** `UiIds.pane` /
  `RibbonPane_` is referenced only inside `Ribbon.fs`; every ribbon test keys off
  `UiIds.tab` or the bay's own control ids. Moving from per-bay panes to one
  active slot breaks no existing assertion, and the existing live bay-switching
  proofs (`WireUiCompositionTests` sweeps every bay by tab click;
  `MainWorkbenchTests`; the `TableAndElementRotationTests` recycling regression)
  operate on the active bay after each switch, which the keyed slot serves.
- **MSB3277 is pre-existing and out of scope.** The full-solution build emits the
  `WindowsBase` version-conflict `MSB3277` from `OpticalConstructor.Ui.fsproj`, a
  project this slice does not touch; the edits here change no project references
  (one `<Compile>` add), so they introduce no new MSB3277.

## Changelog

- 2026-07-07 — Reshaped `OpticalConstructor.Controls.Ribbon.view` to host only the
  active bay's content in one `View.withKey`-keyed `Border` slot (named
  `UiIds.pane activeName`) instead of every bay's pane toggled by `IsVisible`;
  updated the `view` / `UiIds.pane` doc comments to the new single-slot rationale.
  Added `OpticalConstructor.Ui.Tests/RibbonPaneTests.fs` (two `ui-smoke` headless
  proofs over a live Elmish loop: only the selected bay's pane + content is
  realized on first render and after selecting each bay in turn, with no
  styled-element recycling error), registered in `OpticalConstructor.Ui.Tests.fsproj`
  after `CategoryEditorWindowTests.fs`. Corrected a stale rationale comment in
  `TableAndElementRotationTests` to describe the single-keyed-slot behaviour.

```yaml
gates:
  berreman_unit_tests: 119
  constructor_unit_tests: 440
  ui_smoke_tests: 90
  ui_tests: 324
```
