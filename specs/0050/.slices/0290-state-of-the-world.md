# Step 0290 — state of the world

## Where we are

Spec 0050 section S9.1 covers Experiments v2: scans, roles, sessions and attachments.
- Step 286 gave experiments `ExperimentName` and `NamingTemplate.experimentNameAmong`.
- Steps 288 and 289 landed the scan and the dial-reading variable.

This step names every experiment and stores it as `<name>.experiment-json` (001 §A, 001 §J):
- the kit's template names a new experiment over its setup, made free;
- with no template name, the experiment is `E<n>` of its id;
- a copied experiment whose name the target holds takes the next free name.

## What's working

- `Experiment` gains `name : ExperimentName`, the stem every experiment is written, read, deleted and copied under.
  `ExperimentId.fileStem` is retired.
- `ExperimentTarget`, `commit` / `commitOverSetups` and `renamedFor`:
  - `ExperimentTarget { setup; name }` drives `commit` / `commitOverSetups`. An append takes the target's name, and
    an edit keeps its own.
  - `Experiment.renamedFor` gives a copied experiment the next free name on a clash.
- `NamingTemplate.experimentTargets` names a run of new experiments over their setups. Each name is free of the kit's
  names and of those given before it, else `E<n>` of the id it will take.
- Storage writes, deletes and reads experiments by name; the copy renumbers and then renames. The schema requires a
  non-blank `name`.
- The constructor names what it appends:
  - `ExpCommit`, `RecordExperiment` and `GenerateSetupGrid` name each new experiment, through one private helper,
    `experimentTargetsOver`.
  - `ExpRemove` deletes the experiment's file by its name.
  - `RecordExperiment` reports `Recorded <name> over <setup>.`.
- Tests:
  - Three new facts: ExperimentProxyTests, NamingTemplateTests and ExperimentCopyTests.
  - Two renamed facts: KitContentFileJsonTests and ExperimentCollectionRoundTripTests.
  - Four SetupModelTests facts expect the template's experiment names.

## Tests

These are development runs on the final tree, using the snapshot's commands and selectors, run one after another.
The arc-runner's gate engine runs the gates themselves. No run went over its effective deadline.

| Gate | Result | Time | Deadline |
|---|---|---|---|
| build | exit 0, 0 warnings, 0 errors | ~70 s | 600 s |
| constructor-unit-tests | Passed!, 1487 of 1487, 0 failed, 0 skipped | 73 s | 180 s |
| ui-smoke | Passed!, 249 of 249, 0 failed, 0 skipped | 222 s | 600 s |
| ui-tests (9 selected classes, the gate's exact command) | Passed!, 119 of 119, 0 failed, 0 skipped | 129 s | 360 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, known findings 0, ELEV004 excluded | 199 s | 600 s |

- **constructor-unit-tests:** 1487 is step 0289's 1484 plus the three new facts, as the acceptance requires.
  - The TRX shows `Passed` for the new facts.
  - The same holds for the renamed `an experiment is stored under its name` and
    `an experiment file that does not read, or holds another name, refuses the read, naming the file`, and for the four
    changed SetupModelTests facts.
- **ui-smoke:** 249, unchanged.
- **ui-tests:** no UI fact was added or removed, so the count over the selection is unchanged.
  - Per class: KitWindow 16, ExperimentControls 36, WorkbenchInfoAndCapture 8, InverseConstructor 14,
    SolverHandoffWindow 9, SlotBar 4, SetupGridPanel 4, ScrollExtent 14 and FsxAcceptance 14.
- **invariant-s29-\*** (16): every decoded `git grep` finds nothing.
- **Mutation check (red):**
  - Without the copy's `renamedFor` call, ExperimentCopyTests fails exactly the two name-clash facts (2 of 18).
  - The file was restored byte for byte (SHA-256 checked), rebuilt, and re-ran 18 of 18.

```yaml
gates:
  constructor_unit_tests: 1487
  ui_smoke_tests: 249
  ui_tests: 119
```

## Architecture

- **The name is identity on disk; the id stays identity in the collection.**
  - `readExperiments` still sorts by id, `ExperimentCollection.ofLoaded` still advances `nextId` past the ids, and the
    copy still chooses by id.
  - Only the file stem moved from `id` to `name`.
  - `MissingKitFileErr` for an id the source lacks still names `E<n>.experiment-json`, the stem of its `defaultName`.
- **Naming happens where an experiment is appended, never on an edit.**
  - `commit` takes a target, but an edited experiment keeps its name, so editing never moves a file.
  - `experimentTargets` counts ids from the collection's `nextId`, so the `E<n>` fallback is the id the experiment
    receives.
- **One rule for clashes.**
  - `renamedFor` and `experimentTargets` both use `ExperimentName.nextFreeAmong` over the held names plus those given
    so far.
  - `nextFreeAmong` rests on `FileStem.firstFreeText`, the suffix sequence that items and setups already share.
  - The copy's experiments first follow a renamed setup (`placeSetups`), then are renumbered, then renamed.
- **The Ui computes the three callers' targets once**, in the private `experimentTargetsOver`, over the kit's template,
  `kitItems`, the collection's names and its `nextId`.

## Deferred

Nothing. Every requirement of the step landed in this round.

## Gotchas

- **Files edited outside the step's list, because the build required them:**
  - **`OpticalConstructor.Domain/Setups.fs`:** one interpolation used the retired `experiment.id.fileStem`. It now
    reads `experiment.name.value`, and its message still names `E24` in the fact that checks it.
  - **KitOpeningTests, KitStartTests, KitStartWindowTests and LauncherTests:** they build `Experiment` literals, now
    re-pointed by the slice's rule (5). Step 288 re-pointed the same four files the same way.
- **One more SetupModelTests expectation than the census lists:**
  - The fact is `the panel's grid generates its ticked setups through GenerateSetupGrid and stays open`.
  - It shares `templateTable` with the listed GenerateSetupGrid fact, so the template names its experiments. The slice
    says every other fact "passes as written"; this one cannot.
  - Its file assertion now expects `<name>.experiment-json` for the three names it already lists. Its identity and
    other assertions are unchanged.
- **The expected value of one ExperimentCopyTests fact changed:**
  - In `a copied experiment whose id the kit holds takes the next free id`, the target holds `E2`.
  - The copied E2 is therefore also named `E2 (2)`. Its expected value `copiedE4` carries that name, and its assertions
    are unchanged.
- **A copy-and-update fixture that changes `id` must change `name` too.** Otherwise two experiments write one file.
  KitWindowModelTests, KitWindowTests, ScrollExtentTests and SolverHandoffWindowTests had such fixtures.
- **The copy panel's row text still reads `E<id>`.** It is `KitWindowView.fs`, outside this step's scope, and was left
  unchanged.
- **On this machine, Python's `bash -c` reaches a different bash**, which made every invariant grep look "found". Run
  the decoded greps in Git Bash.

## Changelog

- 2026-11-11 — Step 0290: experiments are named.
  - Domain: `Experiment.name`, `ExperimentTarget`, `Experiment.renamedFor` and `NamingTemplate.experimentTargets`;
    `ExperimentId.fileStem` is retired.
  - Storage: experiments are stored, read, deleted and copied by name, and the schema requires `name`.
  - Ui: the three commit callers name by the template, and `ExpRemove` deletes by name.
  - Counts: constructor tests 1487 (+3), smoke 249, selected ui-tests 119.
