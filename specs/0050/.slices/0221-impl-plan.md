# Step 0221 — implementation plan (IMPLEMENT, S72-COPY-MAPPING)

## Approach

1. **Domain, `Kit.fs`** (after the item declarations of steps 213–217):
   - `type ItemLabel with member this.nextFreeAmong`: the label, then `<label> (2)`, `(3)`, … until no taken label
     shares its file stem, ignoring case (the suffix form of `KitDataFolder.copyTargetFor`).
   - `ItemCopyChoice = MapOntoKitItem of ItemId | AddCopiedItem`; no wire tag, never stored.
   - `type Item with member this.defaultCopyChoice`: the held item of the same id, else the first held item of equal
     `model` (the `ItemModelInstance`, step 216's payload) and equal label, else `AddCopiedItem`.
   - `ItemCopyRow = { sourceItem; choice }` with intrinsic statics `copyRowsFor` (one row per used item, a chosen row's
     choice kept by source id, otherwise the default) and `itemsToAdd` (every row checked first: a mapping onto an id
     the kit lacks is `UnknownKitItemErr`, an add of a held id `DuplicateItemIdErr`; then the added items relabelled
     `nextFreeAmong` the held labels and the labels already given).
2. **Domain, `Experiments.fs`**: `ElementDescriptor.itemIdOpt : ItemId option`, the explicitly assigned physical
   piece. `Experiment.itemsUsedBy experiments items` resolves every assigned id, answers the referenced items once in
   the supplied order, and names the id, element and experiment of a dangling reference (`UnknownKitItemErr`). No
   inference from a binding.
3. **Storage**: `experiment.schema.json` gives `setup` an item schema that requires `itemIdOpt` and accepts `null`
   or the shared canonical GUID. Non-object items still reach binding, so the existing `MalformedJsonErr` facts
   hold. The canonical option form (value or `null`) is the shared serializer's. `KitContentFiles.fs` keeps the one
   parser; only its doc names the refusal (no fallback reader).
4. **Ui, `TableAndElementRotationView.fs`**: `experimentSetup` keeps the edited experiment's `itemIdOpt` for the
   same `elementId` when the recaptured binding equals the stored one; otherwise `None`.
   `SolverHandoffView.fs` and `ExperimentDataLoad.fs` compile unchanged: they construct no descriptor.
5. **Tests**:
   - the four `KitItemTests` facts;
   - the two `ExperimentProxyTests` facts;
   - the `KitContentFileJsonTests` fact;
   - the `InverseConstructorTests` fact.
   Existing descriptor fixtures gain `itemIdOpt = None`, deliberately unassigned. E1 in
   `ExperimentCollectionRoundTripTests` carries real ItemIds on its two same-preset polarizers.

## Files

- `OpticalConstructor.Domain/Kit.fs`, `OpticalConstructor.Domain/Experiments.fs`
- `OpticalConstructor.Storage/schema/experiment.schema.json`, `OpticalConstructor.Storage/KitContentFiles.fs`
- `OpticalConstructor.Ui/TableAndElementRotationView.fs`
- Tests:
  - `KitItemTests.fs`, `ExperimentProxyTests.fs`, `ExperimentCollectionRoundTripTests.fs`;
  - `KitContentFileJsonTests.fs`, `InverseConstructorTests.fs`.
- **Out of the edit list:** `OpticalConstructor.Tests/KitLibraryTests.fs`. Its `recordingExperiment` builds a
  descriptor record literal, so the change_shape contract on `ElementDescriptor` forces it to gain
  `itemIdOpt = None`. Step 0210 set the precedent for such compile-forced edits.

## Risks

- The schema must not turn the existing malformed-descriptor facts into schema failures. `required` / `properties`
  apply only to objects, so a string item still binds and fails as `MalformedJsonErr`.
- ELEV:
  - new DU case names must be unique;
  - no two-case-union-to-bool lambda (`List.choose` instead of a filter on the choice);
  - no wide tuples.
- In the UI fact, the scene is not stored in the kit, so the reopened model takes the first session's elements. Their
  ids are minted by `freshId`.
