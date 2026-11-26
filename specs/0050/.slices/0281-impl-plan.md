# Step 0281 — impl plan

## Approach

Step 274 added the grid generator (`SetupGrids.fs`): `SlotChoice`, `SlotOptions`, `SlotPick`, `GridCombination` and
`SetupGrid`, with `SetupGrid.create` unticking the combinations the table cannot make. This step adds the grid panel's
editable options as pure Domain, so the S8.4 panel that edits them holds no rule of its own.

1. `OpticalConstructor.Domain/SetupGrids.fs`, after `SetupGrid`:
   - `GridSlotDraft = { slot; items : ItemId list; angles : Angle list }`;
   - a private module function that puts an edited slot draft in its place among the drafts (or appends it, first
     edited first) and drops a draft left with no item and no angle. The three slot edits share it, and it is not a
     member, so the type gains only the members the slice names;
   - `SetupGridDraft = { slots : GridSlotDraft list; unticked : GridCombination list }` with `empty`, `slotDraft`,
     `withItemToggled`, `withGridAngle`, `withoutGridAngle`, `withCombinationToggled` and `gridOver`.
   - `gridOver` walks `SetupSlot.inLightOrder table`, makes one `SlotOptions` axis per slot whose draft holds an item
     (each item, in order, at each angle, in order, or at the slot's `placement.r1` when the draft has no angle), calls
     `SetupGrid.create`, then folds the draft's unticks that the grid lists through `SetupGrid.untick`, which already
     keeps a combination listed once.
2. `OpticalConstructor.Tests/SetupGridTests.fs`: three facts over the existing fixture, with the names the slice gives.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/SetupGrids.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/SetupGridTests.fs`

## Risks

- Record-label inference: `slot` is a field of `SlotOptions`, `SlotPick`, `RoledSlot` and now `GridSlotDraft`, and
  `unticked` / `slots` are shared too, so every record expression is annotated.
- ELEV: the new types carry only elevated fields; the members are exempt from the primitive rules. The private helper
  takes two of our types, so ELEV013 does not apply.
- constructor-unit-tests count rises from 1466 to 1469.
