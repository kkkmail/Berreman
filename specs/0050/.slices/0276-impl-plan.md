# Step 0276 — impl plan

## Approach

`GenerateSetupGrid of SetupGrids.SetupGrid` joins `TableAndElementRotationMsg` after the setup messages, with a
one-line doc citing spec 0050 S8.3. Its arm in `updateTableAndElementRotation` sits behind `guardKitWrite`, the
read-only guard from step 185. Inside the guard it calls
`grid.generate model.elements model.kitItems model.tryItemModelChoice model.kit.kitFile.namingTemplate model.kitSetups`
(steps 274 and 263):

- `Error e` sets `kitStatus` to `Not generated — <e.describe>` and writes nothing.
- `Ok setups` folds over the setups in order. Each one not in the running `kitSetups` (compared by equality) is
  written through `KitLibraryContext.recordSetup`. It joins `kitSetups` through `withSetupRecorded`, which keeps
  ordinal name order, and `activeSetupOpt` is put back as it was.
- If the draft has a capture (`model.draftCapture`, read as `ExpCommit` reads it), the collection takes it
  (`chooseMeasurement`), and `Experiments.commitOverSetups` runs over the setups' references. Each experiment it
  appends is written through `recordExperiment`. The model's collection holds the experiments written so far, under
  the committed collection's `nextId` and draft.
- The first write that fails stops the run. `kitSetups` and the collection keep what was written, and `kitStatus` is
  `Not saved to the kit — <e.describe>`.
- On success `kitStatus` is `Generated <n> setups (<m> new) and <k> experiments.`
- The table, `activeSetupOpt` and the draft's chosen fields are left alone.

`AGENTS.md` gets one gotcha bullet of at most six lines, right after "The table is the active setup". It covers the
naming template's placeholders, `setupNameAmong`, `NamingTemplate.standard` and `SetupGrids.fs`.

The four tests go in `SetupModelTests`. They run over step 272's table (`LP1`, the sample, `LPd`). The private
`templateCommitted` helper is split so that its uncommitted table (`templateTable`) can be reused; `templateCommitted`
keeps its behaviour. The grid comes from `SetupGrid.create` over the sample's options (`QZ` 0°, `QZ` 45°, `QZ2` 0°),
with (`QZ2`, 0°) unticked.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Ui/TableAndElementRotationView.fs`: the message case and its arm.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/SetupModelTests.fs`: the four facts and the fixture split.
- `AGENTS.md`: the naming-template gotcha.

## Risks

- Partial progress has to survive the first failed write. To make that possible, the fold carries the model, with an
  error case that holds the model as written so far.
- Grid-generated setups must leave `activeSetupOpt` alone, but `withSetupRecorded` sets it. Each fold step restores it.
- ELEV: no new primitive in any signature, at most three tuple positions with no type repeated, interpolated strings
  only.
