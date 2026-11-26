# Step 0368 — impl-plan

## Approach

This step moves the second half of the table's run rules into Domain, as 001 §N "One item-model evaluator" and
.spec-md §4 "A reuse is a move" require. That half is how a slot binds for a run and what the source emits. The
table then calls the Domain members, and every chart keeps its numbers and its texts.

1. `OpticalConstructor.Domain/ItemEvaluation.fs`:
   - add opens for `Placement` (for `CatalogueKind`) and `Setups` (for `SetupSlot`);
   - after `ItemModelChoice.boundWith`, add:
     - `[<ReferenceEquality>] SlotRunParam`, with the slice's five fields;
     - `SlotRunParam.resolveSlotItem itemId`, the item-and-model resolution the UI's `model.resolveSlotItem` does
       today. It is moved, not copied, so `AssignSlot` / `SwapSlot` and the run share one rule;
     - `SlotRunError` (`SlotItemErr` | `SlotEvaluationErr`), whose `describe` returns the inner error's text unchanged;
     - `type SetupSlot with` holding `runElementWith`, `runStepOf` and the static `runEmissionOf`;
   - generalise `sequenced` over its error type, so the table's run sequences `SlotRunError` results through the one
     ordered collector (manual 016) and does not need a second local fold.
2. `OpticalConstructor.Ui/TableAndElementRotationView.fs`:
   - add `member internal model.slotRunParam`, the one param the run and `AssignSlot` read;
   - delete `model.resolveSlotItem`. `AssignSlot` calls `model.slotRunParam.resolveSlotItem`;
   - `model.runEmission` becomes `SetupSlot.runEmissionOf model.elements model.slotRunParam`, its error's `describe`
     as the caption;
   - `chartForRun`:
     - delete the pre-resolved `slotItems` map and the three inline binding arms of `builderFor`;
     - each path step binds through `slot.runElementWith` and makes each matrix its step through `slot.runStepOf`;
     - the failure charts match `SlotRunError`: `SlotEvaluationErr (SampleMaterialErr e)` gives "Sample materials
       failed to resolve" with `e.runFailureCaption`, and every other error gives "The run cannot be evaluated" with
       its `describe`.
3. `OpticalConstructor.Tests/ItemEvaluationTests.fs`: add the three named facts. They use slots built as
   `SetupTests` builds them, items made through `ItemDraft.addTo`, models from `BuiltinCatalogue.itemModel`
   (step 210), `BuiltinCatalogue.library ()` and `BuiltinCatalogue.materialStore (fun _ -> [])`.

## Risks

- **Error order changes in an edge case.** The old run resolved every captured-path item before binding any of them.
  Now resolution and binding go per path step. The text differs only when one path has both an unresolvable item
  and an earlier binding error. The step prescribes per-step binding, so this is accepted.
- **Polarizer presets now go through `sweptOrFixed`.** They now pass the drawn incidence instead of normal incidence.
  `BoundPolarizerPreset.muellerAt` ignores incidence, so the numbers stay identical.
- **Lens, mirror and detector slots holding an item are now `Ok None`.** No item model fits a lens or a mirror slot
  (`slotKinds`), and a detector is never on the path to itself, so no chart changes.
- **Opening `Placement` in `ItemEvaluation.fs` brings the `Sample` union case into pattern scope.** `Sample` is used
  there only as a type, so nothing is shadowed.
