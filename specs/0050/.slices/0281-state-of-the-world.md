# Step 0281 — state of the world

## Where we are

Spec 0050 section S8.4 covers the slot bar, quick swap and the direct problem. Step 274 (S8.3) added the grid generator:
`SetupGrid` over `SlotOptions` axes, with `SetupGrid.create` unticking the combinations the table cannot make. This step
adds the grid panel's editable options as pure Domain, so the panel that edits them holds no rule of its own (001 §J,
AGENTS.md "State is separable from the UI"). Nothing is drawn or written yet.

## What's working

- Add `GridSlotDraft`, one slot's options: the items tried in it and the R1 angles each is tried at.
- Add `SetupGridDraft` with toggles for items, angles and unticked combinations. A slot draft left with no item and no
  angle is dropped, so an undone edit leaves the draft as it was.
- Add `SetupGridDraft.gridOver`. It gives one axis per slot holding an item, in light order, and tries each item at each
  angle, or at the slot's R1 when no angle is listed. It goes through `SetupGrid.create` and keeps the user's unticks
  while they are listed.
- Add three `SetupGridTests` facts for the draft's grid, the idempotent toggles, and the unticks kept beside the
  impossible combinations.

## Tests

These are development runs on the final tree, using the snapshot's own commands. The arc-runner's gate engine runs the
gates themselves. No run went over its effective deadline.

- **build:** exit 0, with 0 errors and 0 warnings, in 53 s (deadline 600 s). An earlier full build that recompiled
  vendored MathNet showed only its two pre-existing `SYSLIB0051` warnings.
- **elevation:** `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)` in 197 s (deadline 600 s), with known findings 0.
- **constructor-unit-tests:** pass, 1469 of 1469, in 68 s (deadline 180 s). That is 1466 before plus 3:
  - `a grid draft tries each ticked item of a slot at each of its angles, its slots in light order`;
  - `ticking an item or adding an angle twice changes nothing more, removing undoes it, and a slot with no item is no axis`;
  - `a draft's unticked combinations stay unticked beside the impossible ones while they are listed`.

  All three show `Passed` in the TRX.
- **invariant-s28-\* (all sixteen):** each grep finds nothing, also with `--untracked`
  (`specs/0050/.artifacts/0281-invariants.log`).
- **Mutation check:** I ordered the axes by draft order instead of light order and dropped the "still listed" filter.
  The first and third new facts went red, and the code was then restored.

```yaml
gates:
  constructor_unit_tests: 1469
```

## Architecture

- Both types sit in `SetupGrids.fs` after `SetupGrid`, and they are pure records with members.
  - `SetupGridDraft.slots` keeps the slots in the order they were first edited. Light order is applied only by
    `gridOver`, through `SetupSlot.inLightOrder` (step 257), so a slot's draft keeps its place while the table is moved.
- A private module function, `withSlotDraft`, is the one place-or-append-then-drop rule the three slot edits share. It
  is not a member, because the slice adds no member beyond those it names.
- `gridOver` reuses step 274 twice:
  - `SetupGrid.create` for the impossible combinations;
  - `SetupGrid.untick`, folded over the draft's still-listed unticks, for "listed once, in the draft's order".
- Choices are built from stored `ItemId`s only. `gridOver` resolves models through the caller's `choiceOf`, as
  `SetupGrid.create` does, so nothing is manufactured or read from a preset.

## Deferred

Nothing. Every requirement of the slice landed this round.

## Gotchas

- `slot`, `slots` and `unticked` are field names of several records here (`SlotOptions`, `SlotPick`, `RoledSlot`,
  `GridSlotDraft`, `Setup`, `SetupGrid`, `SetupGridDraft`). Annotate every record expression.
- The draft keeps an untick after its combination is no longer listed. Re-adding the item leaves that combination
  unticked again. "An untick lasts while its combination is listed" refers to the grid, not to the draft.
- The fixture's slots all start at R1 = 0°. A test that checks "at the slot's R1" should turn the slot first, as the
  first new fact turns `lpB` to 30°, or the default 0° hides a wrong angle.

## Changelog

- 2026-11-11: Step 0281 adds `GridSlotDraft` and `SetupGridDraft`, the grid panel's editable options, with `gridOver`
  producing the light-ordered `SetupGrid`. constructor-unit-tests went from 1466 to 1469.
