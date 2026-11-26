# Step 0290 — impl log

## Progress

- [x] Domain: `Experiment.name`, `ExperimentTarget`, `commit` / `commitOverSetups`, `renamedFor`, retire `ExperimentId.fileStem`
- [x] Domain: `NamingTemplate.experimentTargets`
- [x] Storage: write / delete / read / copy by name; schema
- [x] Ui: the three callers, `ExpRemove`, status text
- [x] Fixtures: `name = <id>.defaultName`
- [x] Tests: renames, new facts, changed expectations
- [x] Development runs (final tree)

## Files modified

### Product

- `OpticalConstructor.Domain/Experiments.fs`
  - `Experiment` gains `name : ExperimentName` after `id`. `ExperimentId.fileStem` is deleted.
  - `Experiment.renamedFor taken incoming` sits beside `renumberedFor`. In order, each incoming experiment takes
    `name.nextFreeAmong (taken @ names given so far)`. `FileStem.firstFreeText` compares stems ignoring case, so a
    free name is kept unchanged, and only `name` changes.
  - `ExperimentTarget = { setup : SetupReference; name : ExperimentName }` is declared just before `commit`.
  - `commit (target : ExperimentTarget)`: an append takes `target.name` and `target.setup`; an edit takes
    `target.setup` and keeps its name.
  - `commitOverSetups (targets : ExperimentTarget list)` folds `commit` over the targets.
- `OpticalConstructor.Domain/NamingTemplates.fs`: `NamingTemplate.experimentTargets setups items taken firstId`, after
  `experimentNameAmong`.
  - Each setup's target is its `reference`, named by
    `experimentNameAmong setup.slots items None (taken @ given) id.defaultName`.
  - `id` starts at `firstId` and advances by `.next`.
- `OpticalConstructor.Domain/Setups.fs` (outside the listed edits, see Gotchas): the `itemsUsedBy` refusal names the
  experiment by `experiment.name.value`, in place of the retired `experiment.id.fileStem.value`.
- `OpticalConstructor.Storage/KitContentFiles.fs`
  - `writeExperiment` writes under `experiment.name.fileStem`, and `deleteExperiment (name : ExperimentName)` deletes
    `name.fileStem`.
  - `readExperiments` refuses a file whose experiment's `name.fileStem` is not its own stem, ignoring case. The message
    is `the file '<file>' holds experiment '<name>' instead of '<stem>'`.
  - `copyExperimentsFrom`: the local `renumber` pipes `renumberedFor` into `renamedFor`, both over what
    `this.proxy.readExperiments ()` holds.
  - A missing id is `MissingKitFileErr` of `ExperimentKitFile.fileNameOf missing.defaultName.fileStem`, so its text is
    unchanged (`E9.experiment-json`).
- `OpticalConstructor.Storage/schema/experiment.schema.json`: `experiment` requires `name` after `id`, a string matching
  `\S`.
- `OpticalConstructor.Ui/TableAndElementRotationView.fs`
  - New private `experimentTargetsOver setups model` is
    `model.kit.kitFile.namingTemplate.experimentTargets setups model.kitItems (names of model.experimentCollection.experiments) model.experimentCollection.nextId`.
    The three callers share it, so the argument list lives once.
  - `ExpCommit`: the target over `[ setup ]`, then `commit`. An edit passes that target and keeps its name.
  - `RecordExperiment`: `commitOverSetups (experimentTargetsOver [ setup ] tabled)`. Its status is
    `Recorded <names> over <reference.describe>.`.
  - `GenerateSetupGrid`: `commitOverSetups (experimentTargetsOver setups model)`.
  - `ExpRemove id`: deletes the file of the collection's experiment of that id by its name. An id the collection lacks
    deletes nothing and removes as before.
  - Step 278's `withExperimentsRecorded` is unchanged.

### Tests

- `KitContentFileJsonTests`:
  - `an experiment is stored under its id` is renamed `an experiment is stored under its name`. It checks the
    `CPL-(QZ#45)-CPL` file name and the `E3` stem.
  - The schema fact also refuses a missing `name` and `"  "`.
- `ExperimentCollectionRoundTripTests`: `… or holds another id …` is renamed `… or holds another name …`. Its second file
  is E1 under the stem `E6`, as before.
- `ExperimentProxyTests`:
  - New fact: `copied experiments keep their names unless the kit holds them, and a clash takes the next free suffix`.
  - The commit facts go through a private `commitOver`, a target named `c.nextId.defaultName`, with their assertions
    unchanged.
  - `commitOverSetups` takes targets named E2 and E3.
- `NamingTemplateTests`: new fact
  `the experiments of new setups are named by the template over each setup's slots, each free among the kit's and the earlier ones`.
- `ExperimentCopyTests`: new fact `a copied experiment whose name the target holds takes the next free name`.
- `SetupModelTests`: the three listed expectations changed, plus one more (see Gotchas).
- Fixtures: each `Experiment` literal, and each copy that changes `id`, sets `name = <its id>.defaultName`:
  - ExperimentCollectionRoundTrip, ExperimentCopy, ExperimentDataProxy, ExperimentProxy and KitContentFileJson;
  - KitLibrary, KitWindowModel, OutOfBandDiagnostic and Setup;
  - KitOpening and KitStart;
  - KitStartWindow, KitWindow, Launcher, ScrollExtent and SolverHandoffWindow.
- `ExperimentCollectionRoundTripTests`, `KitWindowTests` and `InverseConstructorTests`: their `id.fileStem` and
  `deleteExperiment id` uses are re-pointed to `name`.

## Decisions

- **`experimentTargetsOver` is one private helper in the Ui file.** The slice names the same four arguments for all
  three callers, and AGENTS.md says "reuse, never copy".
- **`ExpCommit` matches `[ target ]`.** `experimentTargets` over one setup answers one target. Any other shape leaves
  the collection unchanged, so the arm answers `model` and writes nothing.
- **The fallback name is not made free**, exactly as the slice writes it: `experimentNameAmong … id.defaultName` returns
  `unnamed` as given. `E<n>` names come from ids, and a copy only moves an id up (`renumberedFor`), so an `E<k>` name
  never exceeds its own id. The next id is therefore never named by a held `E<k>`.
- **`renamedFor` is `nextFreeAmong (taken @ given)`.** `FileStem.firstFreeText` returns the text itself when no stem
  clashes ignoring case. That is the slice's "keeps its name when …, otherwise …" in one call, the same rule
  `placeSetups` uses for setups.

## Testing state

Every run below is on the final tree, run one after another, with the snapshot's commands and selectors.

| Gate | Result | Time | Deadline |
|---|---|---|---|
| build | exit 0, 0 warnings, 0 errors | ~70 s | 600 s |
| constructor-unit-tests | Passed!, 1487 of 1487 | 73 s | 180 s |
| ui-smoke | Passed!, 249 of 249 | 222 s | 600 s |
| ui-tests (9 selected classes, the gate's exact command) | Passed!, 119 of 119 | 129 s | 360 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, known 0, ELEV004 excluded | 199 s | 600 s |

- **constructor-unit-tests:** 1487 is step 0289's 1484 plus the three new facts. The TRX shows each new, renamed and
  changed fact `Passed`.
- **ui-smoke:** 249, unchanged.
- **ui-tests:** this step adds or removes no UI fact, so the count over the selection is unchanged.
  - Per class: KitWindow 16, ExperimentControls 36, WorkbenchInfoAndCapture 8, InverseConstructor 14,
    SolverHandoffWindow 9, SlotBar 4, SetupGridPanel 4, ScrollExtent 14 and FsxAcceptance 14.
- **invariant-s29-\* (16):** every decoded `git grep`, run in Git Bash, finds nothing (`0290-invariants.log`).
- **Elevation:** the standalone driver had no comparison base, so it analyzed all 12 product projects. That is a
  superset of the gate's arc-origin scope.
- **Mutation (red):**
  - `copyExperimentsFrom` lost its `renamedFor` call, and the build was rerun.
  - ExperimentCopyTests then failed exactly 2 of 18: the new name-clash fact and the id-clash fact, whose copy now also
    clashes by name.
  - The file was restored (SHA-256 OK), rebuilt with 0 warnings, and re-ran 18 of 18.
- **Other new facts:** the ExperimentProxyTests and NamingTemplateTests facts were red at the first build. They could
  not compile before `renamedFor` / `experimentTargets` existed.
- **Line endings:** every changed file is LF in the index and the working tree, and `git diff --numstat` equals the
  `--ignore-cr-at-eol` diff (323 / 86).
- No run exceeded its effective deadline.

## Artifacts

- `0290-build-dev-1.log`: the first build. Product green; the test projects red on the new field, as expected.
- `0290-build-dev-2.log`, `0290-build-dev-3.log`: green, 0 warnings, 0 errors.
- `0290-constructor-unit-tests-dev-1.log`: 1486 / 1487. The panel-grid fact found template names; see Gotchas.
- `0290-constructor-unit-tests-dev-2.log`: 1487 / 1487.
- `0290-invariant-commands.txt` and `0290-invariants.log`: the 16 decoded s29 greps, each finding nothing.
- Development runs:
  - `0290-ui-smoke-dev-1.log`: 249 of 249, 215 s.
  - `0290-elevation-dev-1.log`: OK, 208 s.
- Final runs, on the final tree:
  - `0290-build-final.log`
  - `0290-constructor-unit-tests-final.log`
  - `0290-ui-smoke-final.log`
  - `0290-ui-tests-final.log`
  - `0290-elevation-final.log`
- Mutation evidence:
  - `0290-mutation-hashes.txt`
  - `0290-mutation-build.log` and `0290-mutation-run.log`
  - `0290-build-restored.log` and `0290-restored-run.log`

## Gotchas

- **Files outside the listed edits:**
  - **`OpticalConstructor.Domain/Setups.fs`:** one interpolation used `experiment.id.fileStem`, which this step
    retires. It now reads `experiment.name.value`. The fact that checks it (`Assert.Contains("E24", reason)`) passes
    unchanged.
  - **`KitOpeningTests.fs`, `KitStartTests.fs`, `KitStartWindowTests.fs` and `LauncherTests.fs`:** each builds an
    `Experiment` literal, which no longer compiled. Each gains `name = (ExperimentId 1).defaultName` by the slice's
    rule (5), as step 288 re-pointed the same four files under its own §6.
- **One more SetupModelTests expectation than the census lists:**
  - The fact is `the panel's grid generates its ticked setups through GenerateSetupGrid and stays open` (step 276).
  - It builds its table through `templateTable`, the table of the listed GenerateSetupGrid fact, so the template names
    its three experiments.
  - The slice's claim that "every other fact passes as written" does not hold for it. Its file assertion now expects
    `<name>.experiment-json` for the three names it already lists. Its other assertions are unchanged.
  - The fact's identity is unchanged. The file is in the step's edits, and the behaviour is the one the slice mandates.
- **`a copied experiment whose id the kit holds takes the next free id` (ExperimentCopyTests):**
  - The target holds `E2`, so the copy of source E2 is now also renamed `E2 (2)`, by the slice's own rule.
  - Its expected value `copiedE4` carries that name. Its assertions are unchanged.
- **Copy-and-update fixtures that change `id` must change `name` too.** Otherwise two experiments share a file:
  - KitWindowModelTests `writeE2` and `darkLine`;
  - KitWindowTests `e2`, `outsideExperiment` and `sourceExperiment`;
  - ScrollExtentTests' 40 experiments and SolverHandoffWindowTests' `withId`.
- **`ExperimentProxyTests.numbered`** (the `renumberedFor` fact) deliberately keeps the name. `renumberedFor` changes
  only `id`, and that fact checks exactly that.
- **Two Windows shell traps:**
  - A Python `bash -c` on this machine resolves to another bash: every invariant grep "found" text. The greps were run
    in Git Bash instead.
  - A Python heredoc collapsed `\\` in a JSON pattern to `\`. The schema's `"\\S"` was fixed by hand.
