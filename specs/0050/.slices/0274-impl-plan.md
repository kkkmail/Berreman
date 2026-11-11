# Step 0274 — impl plan

## Approach

The grid generator lands as pure Domain in a new `OpticalConstructor.Domain/SetupGrids.fs`
(`module SetupGrids`, compiled directly after `NamingTemplates.fs`), over S8.2's setup slots and
S8.3's naming template:

- `SlotChoice` (item or nothing, turned to `r1`), `SlotOptions` (one axis: a slot and its options),
  `SlotPick` / `GridCombination` (one pick per axis) with `slotsOver`, which turns each picked slot,
  fills or empties it through `SetupSlot.withItem` / `withoutItem` exactly as `AssignSlot` resolves an
  item, refuses a slot the table lacks and one item in two picked slots, and records every slot with
  `recordedWith`.
- `SetupGrid` (axes + unticked) with `combinations` (product, first axis slowest), `ticked`,
  `untick` / `tick`, `create` (impossible combinations start unticked) and `generate` (one setup per
  ticked combination: a held or earlier-made setup with equal current slots, else `Setup.create` of
  the template's name made free, else `SetupName.firstFree`; the first failure refuses the whole grid).
- `Kit.fs`: `SetupError` gains `SlotItemTwiceErr of reason : string`, rendered
  `the item is in two slots: <reason>`.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/Kit.fs` — new `SetupError` case.
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/SetupGrids.fs` — new.
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/OpticalConstructor.Domain.fsproj` — one compile line.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/SetupGridTests.fs` — new, four facts over a
  test-local copy of step 271's fixture.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj` — appended compile line.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitTests.fs` — the setup-error render fact covers the new case.

## Risks

- Two records share the field `slot` (`SlotOptions`, `SlotPick`): every construction and lambda is
  annotated so inference never picks the wrong one.
- ELEV: no free functions, no bool-from-union helpers, no wide tuples; all new surface is members.
- The `AssignSlot` resolution lives on the Ui model (`resolveSlotItem`), outside this step's effects;
  the generator resolves through the caller's `choiceOf` with the same refusal texts.
