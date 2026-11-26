# Step 0258 — impl plan

## Approach

This step adds the pure halves of AssignSlot and SwapSlot (spec 0050 S8.2; 001 §P, §J, §I).

1. `Kit.fs`:
   - `ParameterState.currentValue` takes over the rule from `ParameterValues.ofParameters`: a known value, an
     unknown's start, a calibrated value.
   - `type Item with member this.dialZeroAngle` follows `Item`. It is the `dialZero` parameter's
     `currentValue.angle`, and `Angle.zero` for a fixed item.
   - `open Berreman.Geometry` for `Angle`.
2. `ItemModels.fs`: `ParameterValue.angle` becomes an intrinsic member, with its body and doc unchanged.
3. `ItemEvaluation.fs`:
   - the `angle` extension is deleted;
   - `ofParameters` maps through `state.currentValue`.
4. `Setups.fs`, after step 257's members:
   - `type ItemModelChoice with` adds `libraryEntryOpt` and `slotKinds`. `slotKinds` is an exhaustive match over
     `OpticalForm` and `ItemModelForm`, with no wildcard.
   - `type SetupSlot with` adds `withItem`, `withoutItem`, `drawnWith` and `recordedWith`.
   - New union `SetupSwap = SwitchToHeldSetup | CreateSwappedSetup`.
   - `type Setup with static member swapTarget`.
5. Tests:
   - four facts in `SetupTests`;
   - one fact in `KitItemTests`;
   - all five named exactly as the census names them.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/Kit.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/ItemModels.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/ItemEvaluation.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/Setups.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/SetupTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitItemTests.fs`

## Risks

- **The error must name the slot's kind, but Domain has no way to render `CatalogueKind`.** The display name
  `kindName` is in the Ui project, and the slice adds no other member.
  - I use a local phrase inside `withItem`, such as "a linear polarizer slot", which matches `KitTests`' "LP1 in a
    sample slot" style.
  - The choice is recorded under Gotchas.
- **A binding form chosen as `LibraryModelChoice` has no binding to name a kind.** This applies to the
  source-preset, ideal-polarizer and stack-sample item models.
  - `ItemDraft.addTo` already refuses such a choice.
  - So `slotKinds` answers `[]` for it: it fits no slot. Recorded under Gotchas.
- **`List.forall2` throws on lists of different lengths.** `swapTarget` compares the projected
  (kind, content) lists by structural equality instead.
- **Opening `Berreman.Geometry` in `Kit.fs` could clash with a name there.** I checked: Geometry declares `Index`,
  `Angle`, vectors, matrices and `Rotation`, and none of them is a name in `Kit.fs`.
