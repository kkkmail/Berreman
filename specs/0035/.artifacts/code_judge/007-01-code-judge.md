# Code judge — 007.slice-md cycle 1

## Inputs read

- Slice spec: `C:\GitHub\Berreman\specs\0035\.slices\007.slice-md`
- State-of-the-world: `C:\GitHub\Berreman\specs\0035\.slices\007-state-of-the-world.md`
- Impl-log: `C:\GitHub\Berreman\specs\0035\.slices\007-impl-log.md`
- Gate results: build=pass, unit-tests=pass, constructor-unit-tests=pass, ui-smoke=pass, ui-tests=pass
- Critic critiques: (none — no critic critiques listed for this cycle)

## Rationale

The slice asks for one concrete reshape: make `Ribbon.view` host only the ACTIVE
bay's content in ONE stable, keyed content slot instead of rendering every bay's
pane with `IsVisible` toggles, keying that slot by the active bay name so FuncUI
only ever patches a pane against its own previous self and never recycles a
styled, named control across two different bays. `git diff HEAD` on
`OpticalConstructor.Controls/Ribbon.fs` shows exactly that: the old
`state.bays |> List.map (… Border.isVisible (b.name = activeName) …)` list is
gone, replaced by a single `activePane` built from `activeBay state`, wrapped in
`Avalonia.FuncUI.DSL.View.withKey b.name`, and the content `StackPanel` now takes
`[ activePane ]`. The tab strip, `activeBay` (Ribbon.fs:80-83), and the tab's
`SubPatchOptions.OnChangeOf` re-subscription are untouched, and `UiIds.pane` now
names the one active slot — matching the slice's stated invariants.

The slice's parenthetical mechanism ("`SubPatchOptions.OnChangeOf the active
name`") is imprecise: `SubPatchOptions` governs subscription (event-handler)
re-attachment, not content/child diffing, so it cannot key a content slot. The
worker correctly identified this, implemented the view-level equivalent
(`View.withKey` → `IView.ViewKey`), and recorded the reasoning in both the
impl-log and SoW `Gotchas`. That is the interpretation most consistent with the
stated intent ("a pane is only ever patched against its own previous self") and
with the FuncUI diff semantics the worker verified against the assembly. This is
a sound, well-documented interpretation choice, not a deviation from the slice.

Test coverage for the new public surface is present in the diff. `Ribbon.view`'s
changed behavior is exercised by the net-new `RibbonPaneTests.fs` (registered in
the fsproj after `CategoryEditorWindowTests.fs`), whose two `ui-smoke` tests use
four SAME-typed, DIFFERENTLY-named bays — the exact shape that would throw
"Cannot set Name : styled element already styled" if the single slot were not
keyed. They assert on the live FuncUI/Elmish patch path that (a) on first render
only the initially-selected bay's pane + marker is realized and (b) walking every
bay and back by real tab clicks keeps the window visible and realizes exactly the
selected bay's pane + content, with every tab still present. The stale comment in
`TableAndElementRotationTests` was corrected to the single-keyed-slot rationale
with assertions unchanged; the passing `ui-tests`/`ui-smoke` gates confirm the
existing effective-visibility proof still holds under the new "absent, not
hidden" tree.

Every gate is green, the SoW and impl-log line up faithfully with the diff (files
touched, mechanism, expected count bump 88→90 ui-smoke), no critic raised a
finding, and the pre-existing `MSB3277` `WindowsBase` conflict is correctly noted
as out of scope (this slice adds one `<Compile>` and changes no project
references). All `done-green` criteria are satisfied.

## Verdict

done-green

```json
{"verdict": "done-green", "rationale": "Ribbon.view now hosts only the active bay's content in one View.withKey-keyed Border slot (named UiIds.pane activeName), replacing the every-pane-with-IsVisible layout, exactly as the slice requires; the tab strip and activeBay are unchanged. The slice's 'SubPatchOptions.OnChangeOf' mechanism was correctly recognized as content-slot-inapplicable and implemented via View.withKey (IView.ViewKey), with the choice documented in the impl-log/SoW Gotchas. The new public behavior is exercised by two net-new ui-smoke tests in RibbonPaneTests.fs over the live FuncUI patch path using same-typed differently-named bays (the recycling-hazard shape). All five gates pass, SoW and impl-log match the diff, and no critic raised a finding.", "retry_hint": ""}
```
