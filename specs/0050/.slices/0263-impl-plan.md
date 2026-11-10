# Step 0263 — implementation plan

## Approach

`OpticalConstructor.Ui/TableAndElementRotationView.fs`:

1. On the model extension holding `itemModelCaption`, add `model.tryItemModelChoice (item : Item) : ItemModelChoice option`.
   It first resolves the item's stored `ItemModelVersionId` through `model.itemModels.resolveItemModelVersion` and
   checks its form's binding kind against the instance (a standalone form for `ItemModelVersionInstance`, a source or
   polarizer preset binding for `PresetInstance`, a stack-sample binding for `SampleInstance`); only then does it
   resolve the preset (`model.library.tryGetEntry`, a `SourceItem` under the source binding, a `PolarizerItem` under
   the polarizer binding) or the sample version (`model.samples.resolveVersion`) and build the step-216 choice with that
   resolved model version. Anything unresolved, mismatched or erroring is `None`.
2. `itemModelCaption` reads that one resolution; the private `ItemModelLookup` (its three store reads) is deleted.
   Its texts stay exactly as before (`<name> v<n>`, `<preset> (<name> v<n>)`, `<sample> v<n> (<name> v<n>)`,
   `(model not in the library)`) because `ItemsPanelModelTests` / `ItemsPanelTests` facts must pass unchanged.
3. `TableAndElementRotationMsg.AssignSlot of Library.ElementId * ItemId option`, one-line doc. Its arm writes nothing
   (no `guardKitWrite`): unknown slot → `UnknownSetupSlotErr`; `None` → `slot.withoutItem`; `Some id` → the
   `kitItems` item (else `UnknownSlotItemErr` naming the id), its `tryItemModelChoice` (else `UnknownSlotItemErr`
   naming the label and that its model is not in the library), then `slot.withItem item choice`. A refusal sets
   `kitStatus` to `Not assigned — <describe>` only; success replaces the slot in `elements` and clears `kitStatus`.

`OpticalConstructor.Tests/SetupModelTests.fs`: the three facts the step names, with kit items added through `AddItem`
(LP1 polarizer preset fixed, QZ glass plate v1, Laser HeNe, LPd polarizer preset on a negative dial, `dialZero` 10°
through `SetParameterState`). Fixtures take model versions from `BuiltinCatalogue.itemModel` (step 210's shipped
entries). The refusal fact also covers an item whose preset resolves but whose model the store lacks; the first fact
also reopens the kit read-only against an empty library, where the items' models and presets answer only from the
kit's snapshots, and assigns successfully there.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Ui/TableAndElementRotationView.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/SetupModelTests.fs`

## Risks

- The brief says `itemModelCaption` "becomes the resolved choice's `caption` (step 241)", but `ItemModelChoice.caption`
  (the add form's offer caption) carries the model GUID, so it would change the texts the brief also says stay
  unchanged and break the selected `ItemsPanelTests`. Resolution: keep the texts, built from the resolved choice.
- `KitWindowModelTests.folderBytes` is private to a file outside this step's edits; the "no kit file changes"
  check needs its own small helper here.
