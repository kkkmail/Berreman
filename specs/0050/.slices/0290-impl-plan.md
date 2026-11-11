# Step 0290 — impl plan

Experiments get a name: the kit's template names a new experiment over its setup, or `E<n>` when the template cannot.
The name is the stem every experiment is written, read, deleted and copied under (spec 0050 S9.1; 001 §A, §J).

## Approach

1. **Domain, `Experiments.fs`.**
   - `Experiment` gains `name : ExperimentName` after `id`, and `ExperimentId.fileStem` is deleted.
   - `ExperimentTarget = { setup : SetupReference; name : ExperimentName }` is added.
   - `commit` takes a target: an appended experiment takes its name and setup, and an edited one takes only the
     setup. `commitOverSetups` folds `commit` over the targets.
   - `Experiment.renamedFor` sits beside `renumberedFor`. Each incoming experiment takes
     `name.nextFreeAmong (taken @ given so far)`, which is itself when the name is free ignoring case.
2. **Domain, `NamingTemplates.fs`.** `NamingTemplate.experimentTargets setups items taken firstId`: per setup, the target
   of its reference, named by `experimentNameAmong` with the fallback `id.defaultName`. `id` advances by `.next`, and
   the names given so far join `taken`.
3. **Storage.**
   - `KitContentFiles.fs`: `writeExperiment` and `deleteExperiment (name)` use `name.fileStem`.
   - `readExperiments` refuses a file whose experiment's `name.fileStem` is not its own stem.
   - `copyExperimentsFrom` applies `renamedFor` after `renumberedFor`, over the held names. Its missing-id error reads
     the `defaultName` stem.
   - The schema requires `name`, a string holding a non-white-space character, after `id`.
4. **Ui, `TableAndElementRotationView.fs`.**
   - `ExpCommit`, `RecordExperiment` and `GenerateSetupGrid` build targets through
     `namingTemplate.experimentTargets`, over their setups, `kitItems`, the collection's names and `nextId`.
   - `ExpRemove` deletes by the experiment's name.
   - `RecordExperiment`'s status names the experiments.
5. **Fixtures.** Every `Experiment` literal sets `name = <id>.defaultName`. The step-288 precedent re-points fixtures
   outside the list by the same rule.
6. **Tests.**
   - Two renamed facts and three new facts: ExperimentProxyTests, NamingTemplateTests and ExperimentCopyTests.
   - Three changed SetupModelTests expectations.

## Files

The step's authorized edits. Beyond them, the build forces these edits:

- `OpticalConstructor.Domain/Setups.fs`: one message used `experiment.id.fileStem`, the retired member.
- `KitOpeningTests.fs`, `KitStartTests.fs`, `KitStartWindowTests.fs` and `LauncherTests.fs`: fixture literals, edited
  by rule (5), as in step 288.

## Risks

- **Record-label inference.** `ExperimentTarget`'s `setup` / `name` labels overlap `Experiment`'s and
  `SetupReference`'s. Every construction is annotated.
- **ELEV rules on new public members.** They take only elevated types and lists of them.
- **UI facts that find `E<n>.experiment-json`.** They must keep passing. Their tables hold no item the template needs,
  so the fallback applies.
