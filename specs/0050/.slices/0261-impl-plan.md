# Step 0261 — impl plan

## Approach

An experiment stops carrying its own captured chain (`ElementDescriptor` list) and references one revision of a
kit setup (`SetupReference`). The record field's type changes every consumer at once, so the step goes bottom-up:

1. **Domain.** `Experiments.fs` loses `ElementBinding`, `PlacementSummary`, `ElementDescriptor` and the members
   reading them; `VariedElement` gains `label`; `Experiment.setup : SetupReference`; `commit` takes the reference.
   `Lifecycle.VersionRef` goes. `Kit.SetupError` gains `SetupItemReferenceErr of ItemError`. `Setups.fs` gains
   `Setup.pinFor`, `Experiment.slotsIn` and `Experiment.itemsUsedBy` (moved, over slots). `ExperimentDataLoad`
   reads the detector from slots. `ExperimentCollectionSnapshot` gains `setups`. `KitLibraryError` gains
   `KitLibrarySetupErr`.
2. **Storage.** `experiment.schema.json`'s `setup` becomes `{ name, revision }`. `recordExperiment` resolves the
   referenced revision through `readSetups` and snapshots nothing. `copyExperimentsFrom` brings the referenced
   setups, maps their slots' items onto the kit's, skips an identical held setup and renames a clashing one.
3. **UI.** `ExpCommit` records the table as the active setup (revised by `pinFor`, or created as the first free
   name) before committing over its reference. Attach validation and the solver hand-off resolve slots.
   `CopyChosenExperiments` re-reads setups. `KitCopySource` keeps the source's setups (read on open) so
   `ToggleCopiedExperiment` can call the new `itemsUsedBy`.
4. **AGENTS.md** gotcha; the two `.fsproj` comments naming the retired types are reworded so the acceptance grep
   finds nothing.
5. **Tests** per the census: renames, deletes, new facts, fixtures over `SetupReference` / `SetupSlot`.

## Files

Domain: Experiments.fs, Lifecycle.fs, Kit.fs, Setups.fs, ExperimentDataLoad.fs, ExperimentCollectionStore.fs,
KitLibrary.fs. Storage: schema/experiment.schema.json, KitContentFiles.fs. UI: TableAndElementRotationView.fs,
SolverHandoffView.fs. Tests: the census files. AGENTS.md. Domain and Tests `.fsproj` comments.

## Risks

- `KitCopySource` has no setups; adding the field is outside the brief's wording but needed by
  `ToggleCopiedExperiment`'s new `itemsUsedBy` signature.
- Many fixtures construct experiments; each must now record a setup first where it writes through
  `recordExperiment` (which refuses an unheld revision).
- The S6 identity tests must keep their assertions over `SlotContent.ItemInSlot`.
