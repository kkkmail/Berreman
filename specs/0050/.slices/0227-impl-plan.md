# Step 0227 — impl plan

## Approach

Copy experiments from another kit… brings the items the copied experiments use (spec 0050 S7.2; 001 §H). Each item
is mapped onto a target-kit item or added, and every copied descriptor that names a physical piece is rewritten onto
the chosen target piece.

1. **`KitLibrary.fs`.** `KitLibraryError` gains `KitLibraryItemErr of ItemError`, rendered
   `the items could not be copied: <e.describe>`.
2. **`KitContentFiles.fs`.**
   - `ExperimentCopyReport` gains `items : Item list` after `experiments`.
   - `KitContext.copyExperimentsFrom source ids itemRows` reads everything before it writes, in this order:
     1. the kept experiments (step 224 (b));
     2. the source's items and the target's items;
     3. `Experiment.itemsUsedBy kept sourceItems`;
     4. `ItemCopyRow.copyRowsFor`, then `ItemCopyRow.itemsToAdd`; a refusal is `KitLibraryItemErr`;
     5. the descriptors are rewritten:
        - a descriptor with `Some sourceId` takes `Some <its target id>`, the chosen held item's or the added item's
          own;
        - its binding is derived from that item's `ItemModelInstance`: a preset id, a sample version, or `Unbound`
          for a standalone model;
        - a descriptor with `None` is untouched;
     6. the rewritten experiments must resolve through `Experiment.itemsUsedBy` over held plus added items;
     7. each chosen held item's `libraryReferences` closure must resolve through the TARGET's snapshots; a missing
        one is `UnheldEntryErr`;
     8. the source closure walks the unassigned descriptors' bindings, then the added items' `libraryReferences`,
        over the source's snapshots, keeping the versions the target lacks;
     9. the lab files are the kept experiments' data paths, then the added items' photo paths, each distinct path
        read once.
   - The writes are, in order: the snapshots, the lab files (step 224's folder and suffix rule), the added items
     through `writeItem` with `photoOpt` moved to the copied photo, then the experiments.
   - `KitLibraryContext.recordExperiment` first reads the kit's items and resolves `Experiment.itemsUsedBy
     [experiment]`; a missing id is `KitLibraryItemErr` before any snapshot or write. It then snapshots the
     unassigned bindings, the used items' references and the remaining (assigned) bindings, then writes the file.
3. **`TableAndElementRotationView.fs`.** The `CopyChosenExperiments` arm passes `[]` and, on `Ok`, re-reads the
   kit's experiments and items into `withKitContent`.
4. **Tests.**
   - `ExperimentCopyTests`:
     - the source records Laser600, ML, QZ and CPL1 through `recordItem` before its experiments;
     - E1 names Laser600 and ML, and E2 names QZ;
     - the existing facts pass `[]`, and their expected closure, paths and report gain the items;
     - the seven new facts.
   - `KitLibraryTests`: the render fact covers `KitLibraryItemErr`, and one new recordExperiment fact.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/KitLibrary.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/KitContentFiles.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/TableAndElementRotationView.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/ExperimentCopyTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitLibraryTests.fs`

## Risks

- **Existing facts' expectations.** The brief makes E1 and E2 name items, so the existing facts' expected closure,
  paths and report must include the items. Their assertions stay; only their expected values widen.
- **ELEV.** The new private helpers must stay ELEV-clean: no bare primitive, no single-parameter free function of
  our type, and no tuple wider than three.
- **recordExperiment's snapshot order.** For an experiment without items it must stay exactly as before (existing
  KitLibraryTests pin the order).
