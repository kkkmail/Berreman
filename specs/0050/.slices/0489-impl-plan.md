# Step 0489 — impl plan

## Approach

Add to kit records a plan candidate through the grid generator's own recording (step 276), as a `PlannedRun`.

1. In `TableAndElementRotationView.fs`, `GenerateSetupGrid`'s arm body becomes the private `withGridRecorded`. It takes
   the grid, the collection to commit from and the role the appended experiments take (`Experiment.withRole`, before
   each is recorded).
   - On success it answers what was written, the grid's setups and the appended experiments.
   - On failure it answers the model carrying step 276's text (`Not generated — …` / `Not saved to the kit — …`).
   - `GenerateSetupGrid grid` calls it with the window's collection and `ScienceRun`, and keeps its success text.
   - A small private `unheldSetups` names "the setups the kit lacks" once, for both the writes and the text's count.
2. `AddPlannedExperiment of MeasurementPlanner.PlanCandidate` (one-line doc, S14.2). Under `guardKitWrite`, in order:
   - Object 1 of `SetupSlot.inLightOrder model.elements`; none gives `Not added — the table has no sample slot.`
   - `coverageIn model.kitSetups model.experimentCollection.experiments`; `CandidateInKit name` gives
     `Not added — <name> already plans it.`
   - `candidate.capture` not among `availableCaptures model slot` gives
     `Not added — no detector of the table captures the <transmitted|reflected> light of the sample.`
   - Otherwise the draft is built from `ExperimentDraft.empty` by the draft's own functions (R2 incidence axis, then
     the R1 azimuth axis, then the capture). It is recorded over `SetupGrid.create [ Object 1 ← plate at R1 0 ]` as
     `PlannedRun`, the window's own draft put back, with `Planned <name> over <setup reference>.`
3. Tests: three facts appended to `PlanWindowModelTests` over its in-memory kit, the Polarimeter assigned to `det`.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Ui/TableAndElementRotationView.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/PlanWindowModelTests.fs`

## Risks

- `SetupGrid.create` unticks a combination the table cannot make, and `generate` then answers `Ok []`. Without a
  check the status would read `Planned  over .`. The arm asks the combination's `slotsOver` first and says
  `Not generated — <why>`, step 276's text for a grid that cannot be made.
- `GenerateSetupGrid`'s observable behaviour must not move: same draft afterwards (the committed one), same texts and
  same writes, so `SetupModelTests` holds unchanged.
- Record label clashes (`slot`, `r1`, `itemOpt`): annotate the grid literals' types.
