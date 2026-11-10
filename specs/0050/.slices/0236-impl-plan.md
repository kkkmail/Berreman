# Step 0236 — impl plan

## Approach

This step covers the item edits the Items panel sends besides AddItem and SetParameterState: a label, notes and a
photo (spec 0050 S7.2; 001 §I, §E). Everything is pure domain code in `OpticalConstructor.Domain/Kit.fs`.

1. `ItemError.UnknownKitItemErr` already exists (step 213; `ItemCopyRow.itemsToAdd` consumes it). It is reused
   unchanged, with no edit.
2. Three intrinsic members are added on `Item`, beside `withParameterState`:
   - `withNotes (notes : ItemNotes) : Item`: a record update of `notes`.
   - `withPhoto (photoOpt : KitRelativePath option) : Item`: a record update of `photoOpt`. `None` removes only the
     reference, never a file.
   - `relabelledAmong (label : ItemLabel) (items : Item list) : Result<Item, ItemError>`: refuses with
     `DuplicateItemLabelErr` when an item of `items` with another id holds the label's file stem, ignoring case.
     Otherwise it answers `{ this with label = label }`.

   None of the three touches `parameters`, `revision` or `history`.
3. `[<ReferenceEquality>] ItemIdProvider = { mintItemId : unit -> ItemId }` is declared right after `ItemId`. Its
   `system` instance belongs to step 239.

**Reuse, never copy.** `ItemDraft.addTo` (step 216) holds a private `sameFile` check with exactly the label rule
`relabelledAmong` needs. The new member takes over that check, with the same payload text naming both labels and the
shared file name. `addTo` calls it as `this.toItem.relabelledAmong this.label items` after its id check, so the rule
has one body.

- **Why the behaviour is unchanged.** When `addTo` reaches the label check, no held item has the draft's id, so
  ignoring "the item's own entry" filters nothing.
- **Why the order holds.** The id check still precedes the label check, which still precedes the binding rules.
- **Tests.** The existing `addTo` test asserts only `Contains` on the two labels and the file name, so it holds.

## Tests (`KitItemTests`)

The tests are written first. The red is the missing members.

- `an item's notes and photo are edited in place, keeping its parameters and revision` runs over `heldCpl1`, an
  `ItemDraft` over `LibraryModelChoice circularPolarizerModel` and `DialMounted DialPositive`:
  - `withNotes`, then `withPhoto (Some data/photos/CPL1.jpg)`, then `withPhoto None`;
  - each answer is pinned by whole-item equality, so the parameters, revision 1 and the empty history are kept.
- `relabelling an item keeps its id and refuses a label whose file name another item holds` runs over
  `[ heldCpl1; heldLp1 ]`:
  - `CPL 1` and `cpl1` are each accepted, keeping the id;
  - `lp1` is refused with `DuplicateItemLabelErr`, naming `'lp1'`, `'LP1'` and `LP1.item-json`.
- `every item error renders its condition and its payload` already lists `UnknownKitItemErr` (step 213). The
  `change_expectation` is already met, so the case list stays as it is.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/Kit.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitItemTests.fs`

## Risks

- **ELEV.** The new members sit on our own type, and the provider record has the same shape as `ClockProvider`. No
  name collides: nothing in Berreman or STL names `ItemIdProvider`, `mintItemId`, `withNotes`, `withPhoto` or
  `relabelledAmong`.
- **Test count.** constructor-unit-tests should go from 1358 to 1360.
