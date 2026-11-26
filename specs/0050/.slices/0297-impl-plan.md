# Step 0297 — impl plan

## Approach

Section S9.1's acceptance, proved through the kit window over a real kit folder, plus the AGENTS.md gotcha. No product
code changes.

1. **The ui-smoke fact** in `KitWindowTests` (after step 284's acceptance fact):
   `a two-variable experiment recorded through the bay keeps its scan, role, session and notes in its kit file`.
   - Kit: `TestAppScope.freshKitFolder owner` (the class's test directory, as step 284's fact), opened read-write
     through `openedAt` (`openKitAt`), loaded through `kitWindowModel … ForwardConstructor KitWindowShell.inert`.
   - Table: `AddElementBoundTo (Sample, Some glassPlate1mmId.toElementValueId)`. The seeded `src` and `det` stay; the
     sample lands on the main beam between them. Then `ShowFlowStep ExperimentsFlowStep` opens the Experiments bay.
   - Mount through `withKitWindow` (`kitWindowView` in an Elmish host, as step 284's fact).
   - Presses, by id, at the centre (`pressBox`): the sample's candidate, `experimentVariable VaryR2.uiKey`,
     `experimentAddScanAxisButton`, the candidate, `experimentVariable VaryR1.uiKey`, `experimentAddButton`, then
     `experimentRoleChoice CalibrationRun.uiKey`.
   - Typing: focus `experimentSessionField`, `KeyTextInput "Day 1"`, Enter; same for `experimentNotesField` with
     `LR re-seated` (the step-296 Enter commit).
   - In the window: the row `ExperimentBadge_1` reads `no data`.
   - After closing the kit: the folder holds exactly `E1.experiment-json`; the folder reopened through `openKitAt`
     gives one experiment through `readExperiments` with one block of the two axes (R2 then R1, both the sample),
     `CalibrationRun`, `Some (SessionName "Day 1")` and notes `LR re-seated`.
2. **AGENTS.md**: one bullet after "Names come from the kit's naming template", at most six lines, the slice's text.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/KitWindowTests.fs`
- `AGENTS.md`

## Risks

- The session / notes TextBoxes commit only on Enter; the Elmish host must re-render the keyed bookkeeping block after
  `ExpCommit` before the role press. Pressing by id after `RunJobs` covers it.
- Pressing a candidate must re-select it after Add variable (the draft clears its element); the slice orders the
  presses so.
- The template names no setup without a Prep 1 item, so the setup is `Setup 1` and the experiment `E1`.
