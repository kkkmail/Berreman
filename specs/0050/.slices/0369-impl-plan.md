# Step 0369 — impl plan

Move `SolverHandoffView.validateExperiment`'s three checks into Domain as the study's pre-fit checks (spec 0050 S11.2;
spec 0038 Part L).

## Approach

1. New `OpticalConstructor.Domain/PrefitChecks.fs` (`module PrefitChecks`, compiled after `ExperimentMatrices.fs`):
   - `SlotSpecification` = `SpecifiedSlot` | `UnspecifiedSlot`, and `SetupSlot.specification` (an item held, or the
     placement bound by `valueId`).
   - `PrefitError` (`UnspecifiedSlotsErr` / `MissingDataErr` of a reason, `UnreadableDataErr` / `InconsistentDataErr` of
     an `ExperimentDataError`) with `describe`.
   - `Experiment.prefitCheckWith library dataProxy setups : Result<MappedPoints, PrefitError>`: `slotsIn`, then every
     non-`Sample` slot in light order specified, then `dataOpt`, then `ExperimentDataLoad.loadAndValidate`, the reasons
     the view gives today, so the screen's captions stay unchanged.
2. `SolverHandoffView.fs`: `validateExperiment` maps `prefitCheckWith` onto `ExperimentHandoffStatus`; the private
   `HandoffSlotState` / `handoffState` (today's name for the slice's "`unspecifiedElements`" rule) is deleted;
   `handoffSetupCaption` tags each slot through `specification` (a `Sample` slot that is unspecified still reads "the
   unknown (sample)").
3. `OpticalConstructor.Tests/PrefitCheckTests.fs` (`module PrefitCheckTests`, appended last in the fsproj) with the five
   facts the slice names, over `BuiltinCatalogue.library ()`, `Setup.create`, SetupTests-style slots and a test-local
   path-keyed `ExperimentDataProxy`.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/PrefitChecks.fs` (new), `OpticalConstructor.Domain.fsproj`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/SolverHandoffView.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/PrefitCheckTests.fs` (new), `OpticalConstructor.Tests.fsproj`
- `SolverHandoffWindowTests.fs` is listed as an authorized edit, but runs unchanged.

## Risks

- Captions must stay byte-identical: the reasons are moved verbatim.
- ELEV012 (two-case union to `bool`): the unspecified-slot filter is a `List.choose` answering the role's text, never
  a `bool` over `SlotSpecification`.
- ELEV013P: the `ExperimentDataError` reason is read by or-patterns inside `PrefitError.describe`, not by a free helper.
