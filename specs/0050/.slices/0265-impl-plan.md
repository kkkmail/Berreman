# Step 0265 — implementation plan

## Approach

The run law reads the setup's slots: a slot holding a kit item is evaluated through the item's own model and
parameters (spec 0050 S8.2; 001 §N "One item-model evaluator").

`OpticalConstructor.Ui/TableAndElementRotationView.fs`:

1. **One slot-item resolution.** Move `slotAssigned`'s "kit item, then `tryItemModelChoice`" lookup into a private
   member `model.resolveSlotItem (itemId) : Result<Item * ItemModelChoice, SetupError>` on the model, next to
   `tryItemModelChoice`. `slotAssigned` (AssignSlot / SwapSlot) and `chartForRun` both call it, so the two
   `UnknownSlotItemErr` texts live once ("Reuse, never copy").
2. **`chartForRun`.**
   - Before any point is evaluated, resolve each item held by a slot on a captured detector's path (the source slot
     excepted: its item emits, step 266) into a map by slot id. An unresolved item shows the
     `The run cannot be evaluated` chart with `UnknownSlotItemErr`'s `describe`.
   - `builderFor` takes that map. A slot holding an item is bound once (`choice.boundWith model.materials
     (ParameterValues.ofParameters item.parameters)`) and evaluated by `muellerAt` per point at
     `{ rotation = drawn R1 or swept x under VaryR1 when varied; incidence = drawn incidence or swept x under VaryR2
     when varied; branch = the path step's branch }` and the point's wavelength. It is an `ObjectStep` in a `Sample`
     slot and an `ElementStep` otherwise. The sweep shape is shared with step 252's sample arm (one local builder over
     a `BoundOpticalElement`). An `EmptySlotContent` slot keeps step 252's arms.
   - `noPolarizerOnPath` counts a polarizer slot holding an item; `variedIsBound` passes a varied slot holding an
     item.
   - One local `cannotEvaluate` builds the failure chart for both the slot-item error and the evaluator errors
     (`SampleMaterialErr` keeps its own title).
3. **`model.objectIds`** answers the `Sample` slots holding an item, as well as those bound to a library sample, in
   table order.

`OpticalConstructor.Ui.Tests/ExperimentControlsTests.fs`: the three named facts, over `initMain ()` with items added
by `AddItem`, placed by `AssignSlot`, edited by `SetParameterState`, and (the dial fact) recorded by `CreateSetup` and
drawn by `ActivateSetup`. Each fixture's model version comes from `BuiltinCatalogue.itemModel` over the shipped ids.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Ui/TableAndElementRotationView.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/ExperimentControlsTests.fs`

## Risks

- The source is the first step of every path. Building an item step for it would make every source item
  (step 266) fail with `NotAnOpticalElementErr`, so the source slot is excluded.
- A slot holding a preset item stays bound to the preset (`withItem`), so the item arm must take precedence over
  `boundOf`.
- Charts with no item must be byte-identical: the item arm is reached only through the resolved map, which is empty
  when no slot on a captured path holds an item.
