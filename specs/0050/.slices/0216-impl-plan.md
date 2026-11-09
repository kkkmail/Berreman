# Step 0216 — impl plan (IMPLEMENT: AddItem as `ItemDraft`)

## Approach

Append two types to the end of `OpticalConstructor.Domain/Kit.fs`, after `Item` (step 215). Each doc comment is one
to three lines citing spec 0050 S7.1.

1. **`ItemModelChoice`**: a model as the library resolved it.
   - Cases:
     - `SourcePresetChoice of model : ItemModelVersion * source : SourcePreset`;
     - `PolarizerPresetChoice of model : ItemModelVersion * polarizer : PolarizerPreset`;
     - `StackSampleChoice of model : ItemModelVersion * version : SampleVersionId * sample : Sample`;
     - `LibraryModelChoice of ItemModelVersion`.
   - `instance : ItemModelInstance` carries `model.versionId` in every case:
     - `PresetInstance (versionId, preset.id)`;
     - `SampleInstance (versionId, version)`;
     - `ItemModelVersionInstance versionId`.
   - `parameterAxes : ItemParameterAxis list`:
     - A binding choice answers `model.entry.model.tryBoundParameterAxes` over its typed binding
       (`SourcePresetBound` / `PolarizerPresetBound` / `StackSampleBound`). That is step 208's derivation, and it
       calls step 207's preset and sample `parameterAxes`. It answers empty when the form takes no such binding, a
       draft that `addTo` refuses.
     - A library choice answers `entry.model.parameterAxes`.
2. **`ItemDraft = { id; label; model : ItemModelChoice; dial : ItemDial }`.**
   - `toItem` is the draft's id, label, `model.instance` and dial.
     - Its parameters are `Parameter.knownAtDefault` over `model.parameterAxes @ dial.parameterAxes`.
     - `photoOpt` is `None`, `notes` is `ItemNotes ""`, `revision` is `ItemRevision.first` and `history` is `[]`.
   - `addTo (items : Item list) : Result<Item, ItemError>` checks three things.
     - **Id:** `DuplicateItemIdErr` when a held item has the id.
     - **Label:** `DuplicateItemLabelErr`, naming both labels and the shared file, when a held label's `fileStem`
       equals the draft's under `OrdinalIgnoreCase`.
     - **Binding:** `InvalidItemModelBindingErr`, naming the model and the binding, in four cases:
       - a binding choice whose model's form takes another binding or none (mismatched or absent);
       - a preset whose id is not a canonical GUID (`tryToLibraryEntryId`);
       - a sample version that names another sample than the sample it carries;
       - a `LibraryModelChoice` whose form requires a binding.

     Otherwise it answers `Ok this.toItem`. It writes nothing.

## Tests

`KitItemTests` gets four facts with the declared identities. Every source, polarizer and sample draft takes its model
from `BuiltinCatalogue.itemModel` (step 210).

- CPL1 over the circular-polarizer model with `DialMounted DialPositive` has these five parameters, each at its
  default:
  - `c1.extinctionRatio`;
  - `c1.axisOffset`;
  - `c2.retardance`;
  - `c2.fastAxisOffset`;
  - `dialZero`.

  Its model is `ItemModelVersionInstance`. It is at revision 1, with no history, no photo and empty notes.
- Each bound item has its own parameters:
  - the HeNe source has four light-source axes;
  - the LP preset has `dialZero` alone;
  - the glass plate v1 has `substrate.thickness` at 1e-3 and a `SampleInstance`.

  Every item asserts its model version. The mismatched, absent-binding, non-GUID, wrong-sample-version and unbound
  library-choice drafts each answer `InvalidItemModelBindingErr`, and the kit is unchanged.
- LP1 and LP2 over the real-LP model have equal parameters and different ids and labels. LP2 is added beside LP1.
- Over a kit holding LP1, three drafts are refused:
  - `lp1` answers `DuplicateItemLabelErr`;
  - `LP:1` beside `LP_1` answers `DuplicateItemLabelErr`;
  - LP1's id answers `DuplicateItemIdErr`.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/Kit.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitItemTests.fs`

## Risks

- **Record-label inference.** `ItemDraft` shares the labels `id`, `label`, `model` and `dial` with `Item`. Any
  unannotated record expression or lambda field access that relies on the latest record may now infer `ItemDraft`.
  `toItem` annotates its result, and the build will show any other site.
- **ELEV.** The new public members speak only our types. Local helper functions keep their strings, as
  `Parameter.validate` already does.
