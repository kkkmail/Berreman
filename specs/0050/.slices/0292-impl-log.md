# Step 0292 — impl log

## Progress

- [x] Ui: the `ExpAddScanAxis` and `ExpAddScanBlock` messages and their arms
- [x] Tests: `ExperimentModelTests` (2 facts) and its fsproj entry
- [x] Mutation check (red), with the file restored by SHA-256
- [x] Development runs (final tree)

## Files modified

### Product

- `OpticalConstructor.Ui/TableAndElementRotationView.fs`
  - `TableAndElementRotationMsg` gains `ExpAddScanAxis` and `ExpAddScanBlock`, after `ExpSetRangePoints`. Each has a
    one-line doc citing S9.1.
  - `ExpAddScanAxis` is `{ model with experimentCollection = model.experimentCollection.addScanAxis }`.
    - `addScanAxis` is step 288's member on `ExperimentCollection`, so the spec's `Experiments.addScanAxis`. It is a
      member, not a free function, because of ELEV013.
    - With no edited axis, the member answers its collection, so the model stays equal.
  - `ExpAddScanBlock` matches `draft.axisOpt` and `model.draftCapture`.
    - With both, it is `(Experiments.chooseMeasurement capture collection).addScanBlock`. That is the capture the
      detectors allow, coerced as `ExpCommit` coerces it.
    - Otherwise the model is unchanged.
  - Neither arm is behind `guardKitWrite`, because neither writes. One comment line before them says so, citing S9.1.
  - `ExpCommit`, `RecordExperiment` and `GenerateSetupGrid` are untouched.
    - Step 288's `commit` / `commitOverSetups` already build from `draft.scanOpt`, so they commit the whole scan.
    - Their texts are unchanged.

### Tests

- New `OpticalConstructor.Tests/ExperimentModelTests.fs`: `module ExperimentModelTests` in
  `namespace OpticalConstructor.Tests`.
  - **Fixture `withScanKit`.**
    - The kit is a fresh GUID folder under `<test output>/ExperimentModelTests/`, opened read-write, as
      `SetupModelTests.withSetupKit` (step 260) opens its kit.
    - The scene is `initMain ()` loaded with that kit. It holds the seeded source and detector.
    - Step 263's way of adding and binding: `AddItem` of a `QZ` item (glass plate 1 mm, `SampleIds.glassPlate1mmId`
      v1, stack-sample item model), then `AddElement LinearPolarizer`, `AddElement Sample` and
      `AssignSlot (sample, Some QZ)`.
    - Both kits are closed and the folder is deleted at the end.
  - **`Add variable and Add block build the draft's scan, and a commit records it as one experiment`:**
    1. Both the sample and the polarizer have `availableCaptures = [T]`. Only the end detector captures, through the
       sample's T.
    2. The sample is chosen, `VaryR2`, 20…60° over 3 points, all through `ExpChooseElement`, `ExpChooseVariable` and
       `ExpSetRangeMin` / `Max` / `Points`. The edited axis is checked field by field.
    3. `ExpAddScanAxis`: `outerAxes = [R2]`, there is no edited axis, and there are no closed blocks.
    4. The sample is chosen again with `VaryR1` over 0…90° in 2 points. Then come `ExpChooseMeasurement CaptureBoth` and
       `ExpAddScanBlock`.
       - `closedBlocks = [{ [R2; R1], T }]`, `outerAxes` is empty and there is no edited axis.
       - Building the scan wrote nothing: the folder's bytes are unchanged and the collection has no experiment.
    5. The polarizer is chosen with R1 over 0…67.5° in 4 points, then `ExpCommit`.
       - `kitStatus` is `None`.
       - The kit reads back one experiment, and the folder holds exactly its `<name>.experiment-json`.
       - Its scan equals `{ blocks = [ {[R2; R1], T}; {[R1 on the polarizer], T} ]; sourceWaveLengthOpt = None }`.
       - The collection is `[ stored ]` and `draft.editingId = Some stored.id`.
    - Each axis label is non-blank, and the two elements' labels differ.
  - **`Add variable without a chosen element and variable changes nothing and writes nothing`:**
    - The sample is chosen with R2, so the draft has an edited axis, and then comes `ExpNew`.
    - The draft is `ExperimentDraft.empty`.
    - `ExpAddScanAxis` and `ExpAddScanBlock` each leave the model equal (`Assert.Equal<TableAndElementRotationModel>`).
    - The folder's bytes are unchanged and it holds no experiment file.
- `OpticalConstructor.Tests.fsproj`: `<Compile Include="ExperimentModelTests.fs" />` is appended last in the compile
  list, after `SetupGridTests.fs`, with no comment.

## Decisions

- **`ExpAddScanBlock` matches the edited axis as well as the capture.**
  - The slice says "with no capture it changes nothing".
  - A draft whose element varies nothing has a `draftCapture` but no axis. Coercing its capture there would change the
    model while Add block adds nothing.
  - So both must be present. `addScanBlock` itself already ignores a draft with no axis.
- **The first fact asks for Both before `ExpAddScanBlock`.** The slice's sequence does not list this message.
  - On this table the detectors allow T alone, and `ExpChooseElement` already defaults to T.
  - Without the extra request, the coercion `ExpAddScanBlock` must apply would go untested.
  - With it, the closed block's T proves the block is "read on the capture the detectors allow". Mutation A below shows
    the fact fails if the coercion is dropped.
- **The axis labels are read from the draft, not hard-coded.**
  - `experimentElementLabel` is private, and the `<kind> #<n>` index depends on the scene order.
  - The fact asserts each label is non-blank and that the sample's and polarizer's labels differ.

## Testing state

These are development runs with the gates' own commands and this step's selectors, run one after another
(`0292-dev-runs.sh`). The arc-runner's gate engine runs the gates themselves.

| Gate | Result | Time | Deadline |
|---|---|---|---|
| build | exit 0, 0 warnings, 0 errors | 16 s, then 22 s for the final rebuild (incremental) | 600 s |
| constructor-unit-tests | Passed!, 1492 of 1492 | 69 s, then 71 s on the final tree | 180 s |
| ui-tests (3 selected classes, the gate's exact command) | Passed!, 58 of 58 | 91 s | 360 s |
| ui-smoke | Passed!, 249 of 249 | 210 s | 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, known findings 0, ELEV004 excluded | 195 s | 600 s |
| invariant-s29-\* (16) | every decoded `git grep` finds nothing | < 1 s each | 300 s |

- **constructor-unit-tests: 1492** is step 0291's 1490 plus the two new facts, as the acceptance requires.
  - The TRX shows both `ExperimentModelTests` facts `Passed`.
  - After the chain, two label assertions were added to the first fact. The rebuild and re-run on that final tree gave
    1492 of 1492 (`0292-constructor-unit-tests-dev-2.log`).
  - Only the test file changed after the chain. The Ui product file, the Ui.Tests project and the elevation scope did
    not, so the ui-tests, ui-smoke and elevation runs stand for the final tree.
- **ui-tests: 58** over the three selected classes: ExperimentControls 36, FsxAcceptance 14 and
  WorkbenchInfoAndCapture 8. Each class has the count step 0291 measured, so the count is unchanged as required.
- **ui-smoke: 249**, unchanged.
- **Elevation:** the standalone driver had no comparison base, so it analyzed all 12 product projects. That is a
  superset of the gate's arc-origin scope.
- No run exceeded its effective deadline.
- **Mutation check (red)**, run on the new class over the step's product file:
  - **A:** `ExpAddScanBlock` without `chooseMeasurement`, and its no-axis branch setting a `kitStatus`. Both facts
    failed (2 of 2). The first fact expected the closed block's `capture = CaptureTransmitted` and got `CaptureBoth`.
    The second found the model changed.
  - **B:** `ExpAddScanAxis -> model`. The first fact failed (1 of 2).
  - The view file was restored from its copy, checked by SHA-256 (`OK`) and rebuilt with 0 warnings.
- **Line endings:** `git diff --numstat` equals the `--ignore-cr-at-eol` diff (16 / 0), and no changed or new file
  holds a CR.

## Artifacts

- `0292-build-dev-1.log`: the first build. It had two compile errors in the new test file: a missing
  `open OpticalConstructor.Storage.KitContentFiles` for `readExperiments`, and an overload needing an annotation. Both
  were fixed.
- `0292-build-dev-2.log`: green, 0 warnings, 0 errors.
- `0292-new-class-dev-1.log`: the new class, 2 of 2.
- Mutation evidence:
  - `0292-mutation-hashes.txt`
  - `0292-mutation-a-build.log` and `0292-mutation-a-run.log`
  - `0292-mutation-b-build.log` and `0292-mutation-b-run.log`
- `0292-dev-runs.sh` and `0292-dev-runs-times.txt`: the sequential run chain and its times.
  - `0292-build-dev-3.log`
  - `0292-constructor-unit-tests-dev-1.log`
  - `0292-ui-tests-dev-1.log`
  - `0292-ui-smoke-dev-1.log`
  - `0292-elevation-dev-1.log`
- `0292-build-dev-4.log` and `0292-constructor-unit-tests-dev-2.log`: the final tree.
  - The first attempt at this rebuild failed on one test-only error, `Experiments.ElementLabel.blank`. Qualified that
    way, it resolves to the union case.
  - The assertion now compares with `ExperimentDraft.empty.elementLabel`, and the rebuild is green.
- `0292-ui-tests-selector.txt`
- `0292-invariant-commands.txt` and `0292-invariants.log`

## Gotchas

- **`readExperiments` is a Storage extension on `KitProxy`.** A test must
  `open OpticalConstructor.Storage.KitContentFiles` for it, as `SetupModelTests` does.
- **`git grep` cannot see a new untracked file.** The s29 invariants were also grepped directly over
  `ExperimentModelTests.fs` by plain `grep`, and found nothing.
- **A first, non-incremental build also recompiles the vendored MathNet.** That prints its `SYSLIB0051` warnings
  (`MathNetNumerics/Numerics/Exceptions.cs`), which the vendored code already had before this step. The incremental
  builds report 0 warnings. Nothing in this step touches MathNet.
