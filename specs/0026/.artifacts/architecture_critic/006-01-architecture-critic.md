# Architecture critique -- 006.slice-md cycle 3

## Summary

The cycle-2 finding is closed cleanly: the four element-edit commands
(`OpenElementDialog`/`ElementContextMenu`/`ResetRotation`/`DeleteElement`) now
render DISABLED on the contextual Element tab via the single
`ConstructorView.commandsWithoutFrontDoorSurface` list, and the prior three-way
duplication is collapsed to that one source. Layering, the single-source
ribbon↔menu projection, and the acyclic compile order all remain clean. What
remains are smaller, lower-severity items — the chief one being that the
"surfaceless command" invariant the slice has now re-fixed three times is still
guarded by hand rather than mechanically.

## Separation of concerns

`modalOverlay` (Ribbon.fs:370) is still `private` to `Ribbon`, and the SoW
Deferred section still commits slice 007 to wiring the context-menu /
element-dialog / pending-confirm overlays onto the front door — all of which want
exactly this dimmed-backdrop-plus-Close chrome. Re-raising the cycle-2 note
because it is unaddressed and the next slice walks straight into it: as written,
slice 007 must either reach into a `Ribbon` private or grow a parallel modal. The
established home for shared control chrome is `Controls.fs` (where
`disabledButton` correctly landed); promoting `modalOverlay` there, or at least
making it public, would let slice 007's overlays reuse the one shell. Suggestion,
not a blocker.

## Consistency

The Esc-dismissal inconsistency from cycle 2 persists. The Close-button path uses
the dedicated `CloseValueIdModal` / `CloseHelp` messages, which preserve the
selection and the contextual Element tab (ConstructorView.fs:612-614). But the Esc
path still routes through `CancelOrDeselect` (ConstructorView.fs:420-429), which
clears `selection`, `pending`, and BOTH overlay flags. So closing the value-id
modal with the Close button keeps the active element, while closing it with Esc
*also* deselects underneath it and collapses the Element tab — the AC-F2 test at
RibbonTests.fs:236 exercises exactly this Esc path and asserts only that the modal
closes, so the side-deselect is untested. The retry added `CloseHelp` precisely to
stop help-close from deselecting; the Esc route reintroduces that same surprise for
both overlays. Low severity, but it is the same papercut the slice already decided
it cared about.

A new, smaller wrinkle on the Build tab: `PlaceFromRibbon` is in
`commandsWithoutFrontDoorSurface`, so it renders as a DISABLED "Place" button
(Ribbon.fs:222-227) sitting immediately beside the `catalogueView`, whose enabled
per-role buttons are the *actual* place affordance (they dispatch `RibbonDrop`,
ConstructorView.fs:601-602). So Build shows a dead "Place" control next to the
working catalogue. This is the same redundancy the cycle-1 retry removed for
value-id, where it deleted the parallel `elementActions` because the real exposure
was the `bindValueButton`. By that precedent, `PlaceFromRibbon`'s real surface is
the catalogue, and rendering it additionally as a disabled command button reads as
a broken twin. Consider suppressing the bare command button for `PlaceFromRibbon`
(its surface is the catalogue), mirroring how the Element tab exposes value-id
through `bindValueButton` rather than a command button.

## Spec fit

Coverage is strong and exercised by real rendered-path assertions (RibbonTests
AC-D1..F2, plus the ui-smoke front-door mount). One genuinely thin area against a
named requirement: D.4.1 ("the layout MUST flex rather than truncate" under longer
RU labels). The ribbon rows are non-wrapping horizontal `StackPanel`s
(`expandedContent` Ribbon.fs:290-295, `tabHeaders` :321-332, `collapsedContent`
:312-317). A horizontal StackPanel sizes each child to its content (so individual
labels are not truncated — that half of D.4 holds), but it does not wrap: a long
Russian row overflows the parent bounds and pushes later controls (including the
collapse toggle) off-screen rather than flexing. The `AC-D4` test only asserts the
strings resolve and differ EN vs RU; nothing exercises overflow. If D.4's "flex"
is meant structurally, a `WrapPanel` for the command rows would guarantee it
headlessly; today it rests on by-eye layout. Recording as a spec-fit nuance, not a
gate failure.

The Program.fs "constructor as default landing" edit landing as a comment-only
change (the real seed is `Shell.initFrom`, Shell.fs:206-210) remains the right
single-sourcing call — satisfied, not under-delivered.

## Evolvability

The headline for this cycle. The slice has now re-fixed the same defect class —
"a generated ribbon button with no visible effect must not be live" — three times
(cycle 1: nine gesture-only; round 3: plus four element-edit). The fix is correct,
but the invariant is still maintained by hand: `commandsWithoutFrontDoorSurface`
(ConstructorView.fs:454-459) and `applyCommand`'s inert-return arms (:431-432,
plus `ToggleGroup` at :410) are two separate lists that must agree for the
gesture-only half. The guard test (RibbonTests.fs:261-272) only asserts that every
command IN the list is non-invokable and every command NOT in it is invokable —
which is tautological with `isParameterlessInvokable = not (List.contains …)`. It
does NOT verify the list actually captures every no-op command. So a *new*
gesture-only command added to `applyCommand`'s inert arm but forgotten in the list
would render enabled-and-silent, and the suite would stay green (the new command,
being "not in list," is asserted invokable — which is wrong, but passes). That is
exactly the recurrence vector. A small mechanical guard closes it: for every
registry command, if `applyCommand cmd model` returns a model equal to `model`
modulo `lastCommand`, assert `cmd ∈ commandsWithoutFrontDoorSurface`. The
element-edit half can't be caught mechanically (it is a rendering-surface fact, not
a model-equality fact), so the explicit `frontDoorlessElementCommands` guard is the
right tool there — but the gesture-only half is purely mechanical and currently
untested.

The `tabOf` wildcard auto-surfacing onto Build (not the intended tab) remains the
one expected single-site edit for slice 007; correctly flagged in the SoW
Gotchas. The independent constructor/legacy project snapshots and the front
door's missing Open/New wiring also remain deferred — within scope, but they still
corner slice 007 into adding a sync seam or lifecycle wiring before the front door
is usable for real work.

## Risks

`lastCommand` is now set inconsistently and read by nobody this slice: every
`applyCommand` path sets `lastCommand = Some cmd`, but `OpenValueIdModal` sets it
to `None` (ConstructorView.fs:610), and neither `Ribbon.fs` nor `LocalHelp.fs`
reads it (verified). It is harmless today but is a latent trap: when slice 007
wires the ribbon to read `lastCommand`, the `OpenValueIdModal` null and the
modal/help messages that bypass `applyCommand` will make it lie. Worth a one-line
note or a deliberate decision now. The dangling-`pending` risk from cycle 2 is
resolved — Reset/Delete render disabled, so the unresolvable armed `pending` can
no longer be set from the front door. EOL is clean (LF preserved, no CR churn) and
the compile order holds.

## Bottom line

The route-back is genuinely closed and the slice is otherwise well-built and
faithful to Parts D/F, with real rendered-path tests behind every AC. Nothing
remaining is a layering or single-source violation, and nothing is gate-blocking.
The residue is: a hand-maintained surfaceless invariant that has already recurred
twice and lacks a mechanical guard (the one I would most want addressed), a
still-open Esc-vs-Close deselect inconsistency, a redundant disabled "Place"
button beside the catalogue, and D.4 bilingual flex resting on non-wrapping
StackPanels. My read: ship-leaning. None of these justify another full route-back
on their own; the strongest single ask — the mechanical no-op guard test — is
cheap and would stop the next cycle from re-finding the same defect a fourth time.
The judge may reasonably accept all four as documented follow-ups for slice 007.
