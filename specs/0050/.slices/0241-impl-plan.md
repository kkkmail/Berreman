# Step 0241 — impl plan

Obligation `S72-ITEMS-PANEL-VIEW`: the Items panel view over the state steps 239–240 built.

## Approach

1. **`OpticalConstructor.Controls/UiIds.fs`** — a new `// --- The Items panel (ItemsPanelView). ---` group after
   the kit-bar group: the 25 fixed ids, each with a one-line doc.
2. **`OpticalConstructor.Ui/ConstructorUiIdOfDomain.fs`** — the five derived ids on `ConstructorUiId`
   (`kitItemRow`, `addItemOffer`, `newItemDialChoice`, `itemParameterRow`, `itemRevisionRow`), the captions of
   `ItemModelChoice`, `ItemDial` and `ItemRevisionRecord`, and `ParameterGridCells` with `Parameter.gridCells`.
   - `addItemOffer` always carries the item model's GUID and version. A preset adds its entry id; a sample adds its
     GUID and version. The offer caption carries the model's name, version and GUID, after the binding's name (and
     the sample's version) and id. This follows the slice's MUST over its earlier how-to shapes.
3. **`OpticalConstructor.Ui/TableAndElementRotationView.fs`**:
   - `pickDataFile` becomes public and takes a `ControlCaption` title, set on the picker options after
     `buildDataFilePickerOptions`, so `DataFilePickerFolderTests` still sees the attach title;
   - `attachDataFileButton` passes `Attach measured-data file`;
   - `itemModelCaption` goes on the kit extension beside `kitWrites`. It resolves the item's stored model version
     through `itemModels`, a preset through `library.tryGetEntry` and a sample through `samples.resolveVersion`. A
     private typed lookup carries the missing-model condition, `(model not in the library)`.
4. **New `OpticalConstructor.Ui/ItemsPanelView.fs`** (`module OpticalConstructor.Ui.ItemsPanelView`), compiled after
   `KitStartView.fs`. Its parts, in the slice's order:
   - the heading;
   - the Add verb;
   - the add form: grouped offers, the label, the dial, and Add / Cancel;
   - the item list;
   - the selected item: label, model, dial, notes, photo, the parameter grid with its editor, and the revisions.
   Every write control is withheld while `kitWrites` is `ControlWithheld`.
5. **New `OpticalConstructor.Ui.Tests/ItemsPanelTests.fs`** (`module ItemsPanelTests`), last in the compile list:
   - the six declared facts, five without a trait and one `ui-smoke`;
   - each mounts `itemsPanelView` in a `HostWindow` at 1000 × 700 over `initMain ()` driven by its messages.

## Risks

- **Record-label inference.** `ParameterGridCells` has the fields `value` and `sigma`, which
  `CalibratedParameterData` also has. Any later construction that relied on inference could change type; the build
  will show it.
- **Missing analyzer coverage.** ELEV rules apply to private signatures too (`P` codes). Every new helper keeps
  primitives inside local closures.
- **Hidden offers.** The headless clicks need `BringIntoView` for offers inside the bounded offer scroller.
