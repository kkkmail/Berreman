# Step 0278 — impl plan

## Approach

`RecordExperiment` is a new semantic message in `TableAndElementRotationView.fs`. It appends a new experiment of the
draft over the active setup on every send, recording the table first, as `ExpCommit` records it (spec 0050 S8.4;
001 §J, §O, §P).

`ExpCommit` has two halves today, written inline. They become private functions of the update, so that no second copy
of either is written:

1. **The table recorded as the active setup.** `ExpCommit` must know the setup's reference before it writes anything,
   because it decides from the commit whether to write at all. So the half splits in two:
   - a pure member, `model.tableSetup`. It answers the table as a setup: the active setup written over through
     `tableWrittenOver` (`revisedWith` over `pinFor`), or else a new setup under the template's name made free;
   - a private `withTableRecorded setup model`. It calls `recordSetup` unless the setup is the unchanged active one,
     then `withSetupRecorded` (`kitSetups`, `activeSetupOpt`).
2. **The experiments a commit appended, recorded.** This is a private `withExperimentsRecorded committed experiments
   model`. It records each experiment in order through `recordExperiment`. Once an experiment is written, the
   collection takes `committed`'s draft and that experiment, replacing it by id or appending it. The first failure
   stops the run and keeps the model as written so far.
   - It folds through step 276's local `writtenInOrder`, which moves out of `GenerateSetupGrid` to module level, so
     the setup fold and the experiment fold share one collector.
   - A small private `appendedOver held committed` is the experiments a commit appended. `GenerateSetupGrid` and
     `RecordExperiment` both use it.

`ExpCommit` keeps its exact order and outcomes: compute the setup, commit, leave the model alone when nothing changed,
then record the setup and the edited experiment. On any failure it answers the original model with
`Not saved to the kit — …`. `GenerateSetupGrid` records its fresh setups through `writtenInOrder`, then its appended
experiments through `withExperimentsRecorded`, with every text unchanged.

`RecordExperiment` goes through these steps:

1. `guardKitWrite` refuses it on a read-only kit.
2. A draft with no element or no variable is told `Not recorded — choose the element …`, and nothing is written.
3. A draft with no capture is told `Not recorded — no detector captures this table.`, and nothing is written.
4. Otherwise the table is recorded through `tableSetup` and `withTableRecorded`. On an error the model is kept and
   told `Not saved …`.
5. Then `chooseMeasurement capture` and `commitOverSetups [ setup.reference ]` run, and the appended experiment is
   recorded through `withExperimentsRecorded`. On an error the collection is kept and the same text is told.
6. On success the status is `Recorded <id> over <reference.describe>.`

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Ui/TableAndElementRotationView.fs`: the message, the member, the
  three private functions, and the `ExpCommit`, `GenerateSetupGrid` and `RecordExperiment` arms.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/SetupModelTests.fs`: four new facts.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitWindowModelTests.fs`: the read-only fact also sends
  `RecordExperiment`.

## Risks

- `ExpCommit`'s outcome must not change. This includes the case where the setup write succeeds and the experiment
  write fails: the answer is still the original model.
- `GenerateSetupGrid`'s collection after a partial failure must stay as it was. The upsert-by-id is the same as the old
  append for new ids.
- ELEV: the new private signatures carry no primitive. The lifted generic fold has only our types and `'Written`.
