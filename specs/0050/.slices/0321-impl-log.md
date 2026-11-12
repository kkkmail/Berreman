# Step 0321 — impl log

## Progress

- [x] Read the task, both system prompts, the project prompt, the slice, and step 320's SoW and impl-log. The task
  carries no operator note and no ruling for this step.
- [x] `ImportWizardView.fs`: `ImportWizardTarget`, the `target` field, and the heading and button over the target.
- [x] `TableAndElementRotationView.fs`: `messages` by target, `OpenImportWizard`'s target, and `OpenSplitWizard`.
- [x] Tests: `ImportWizardTests`, `KitWindowModelTests`, `ScrollExtentTests`.
- [x] Development runs of the gates: build, elevation, constructor-unit-tests, ui-smoke, ui-tests and the sixteen s31
  invariants.
- [x] Mutation checks (four, all caught); the final tree rebuilt and re-tested.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Ui/ImportWizardView.fs`
  - New `ImportWizardTarget = AttachWizardTarget of Experiments.ExperimentId | SplitWizardTarget`, declared before
    `ImportWizardRequest`, with a type doc and a one-line doc on each case citing spec 0050 S9.2 / S9.3.
  - `ImportWizardRequest.experimentId` and `ImportWizardApplied.experimentId` are replaced by `target : ImportWizardTarget`.
  - `updateImportWizard`'s `applied` hands on `request.target` unchanged. Nothing else in the model or the update
    changed.
  - `importWizardView` reads the target in two places only:
    - `heading`: `Import <leaf>` or `Split <leaf> by its key column`;
    - `attach`: the caption of `importWizardAttachButton`, `Attach` or `Split`.
  - The module doc names both targets.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/TableAndElementRotationView.fs`
  - `ImportWizardApplied.messages` answers `AttachDataFile (id, file, profile)` for `AttachWizardTarget id` and
    `SplitByColumn (file, profile)` for `SplitWizardTarget`, after `SaveImportProfile p` when `savedOpt = Some p`.
  - `OpenImportWizard` builds its request with `target = AttachWizardTarget experimentId`.
  - New `TableAndElementRotationMsg.OpenSplitWizard of FileName`, with a one-line doc. Its arm is behind
    `guardKitWrite`; it opens `model.kitShell.openImportWizard` over
    `{ target = SplitWizardTarget; file; readPickedFile; kitProfiles = the kit's; defaultProfile = ImportProfileName.phoneRoiCsv }`
    and answers the model unchanged.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/ImportWizardTests.fs`
  - The class's request is built by a new private `observeTargeted target read`. `observeWizard read` is
    `observeTargeted attachToE1 read`, where `attachToE1 = AttachWizardTarget (ExperimentId 1)`, so every existing fact
    runs over the attach request.
  - `Attach with a picked profile hands it to the kit window once and closes` and `Attach with a new mapping hands the
    kit window the profile to save and attaches by it` expect `target = AttachWizardTarget (ExperimentId 1)`. Their other
    assertions are kept.
  - New `a split wizard says it splits and hands the kit window the file and the profile to split by` (see Decisions).
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitWindowModelTests.fs`
  - `opening the import wizard hands it the picked file, the kit's profiles and the detector's default` asserts
    `request.target = AttachWizardTarget (ExperimentId 1)`.
  - `the wizard's Attach saves its new mapping first and attaches the file by it` builds its applied value with that
    target. Its assertions are kept.
  - New `opening the split wizard hands it the picked file with Phone ROI CSV chosen, and its Apply splits by the chosen
    profile` (see Decisions).
  - `a kit opened read-only refuses every write message and writes nothing` also sends `OpenSplitWizard dataFile`, under
    the same assertions. Its `Assert.Empty(requests)` proves that no request was recorded.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/ScrollExtentTests.fs`
  - `wideLabFileRequest` (used by `the import wizard leaves everything reachable`) takes
    `target = AttachWizardTarget (Experiments.ExperimentId 1)`. Its assertions are kept.

## Decisions

- **The split heading's second line.** The slice names the heading's first line, `Split <leaf> by its key column`.
  - The attach heading's second line is `Nothing is attached until you press Attach.`, and the existing view fact pins
    it exactly.
  - Under a split there is no Attach button, so that line would be false. The split heading's second line names its
    own button: `Nothing is split until you press Split.`
  - The view reads the target only in the heading and the button, as the slice says.
- **The new UI fact presses the controls.** It mounts `importWizardView` over the split request as the class mounts it
  (`withMountedWizard`), then checks:
  - the heading text, exactly;
  - the button's text, `[ "Split" ]`;
  - a press on the `Phone ROI CSV` profile box, then on the button, sends `[ PickImportProfile phoneRoiCsv; ApplyImportWizard ]`;
  - the context records one `AppliedCall { SplitWizardTarget; labFile; Phone ROI CSV; None }`, then one `CloseCall`.

  It ends by mounting the class's attach wizard and checking that its button still reads `Attach`, so the caption is
  shown to follow the target.
- **The new model fact also passes the applied messages through the update.** It works over `withWizardKit`'s kit:
  - Lab scan is saved first, so "the kit's profiles" is a non-empty list.
  - `OpenSplitWizard` of a test-local `lab-scan.csv` records one request:
    - `SplitWizardTarget`;
    - the file;
    - `[ Lab scan ]`;
    - `Phone ROI CSV`.
  - The model is unchanged.
  - The applied value `{ SplitWizardTarget; the file; Lab scan; Some KitTests.labScanProfile }` gives
    `[ SaveImportProfile it; SplitByColumn (file, Lab scan) ]`. "Step 310's test-local Lab scan" is
    `KitTests.labScanProfile`, which step 310's facts save.
  - Those messages are then passed through the update over the kit before Lab scan was saved:
    - the kit then holds Lab scan;
    - the split is held over the file and that profile;
    - the experiments are unchanged.

  This shows that a new profile is in the kit before the file is split by it. In the reverse order, `SplitByColumn`
  answers `the import profile 'Lab scan' is not available`.
- **Order of the new message.** `OpenSplitWizard` is declared directly after `SaveImportProfile` and before
  `SplitByColumn`, the message its wizard ends in. Its handler sits directly after `OpenImportWizard`'s.

## Testing state

Development runs of each gate's own command over the final tree (the gate engine runs the gates themselves):

| Gate | Result | Time | Deadline |
|---|---|---|---|
| build | exit 0, 0 warnings, 0 errors; no lowercase `error` in stdout | 75 s first, 17 s final | 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`; known findings 0 | 220 s | 600 s |
| constructor-unit-tests | Passed!, 1549 of 1549 (1548 + 1) | 72 s, 70 s final | 180 s |
| ui-smoke | Passed!, 253 of 253 | 215 s, 220 s final | 600 s |
| ui-tests (`ImportWizardTests`, `KitWindowTests`, `ScrollExtentTests`) | Passed!, 42 of 42 (41 + 1) | 29 s, 27 s final | 360 s |
| invariant-s31-\* (16) | each `INVARIANT HOLDS` | under 1 s each | 300 s |

- Every named fact of the slice's test effects is `Passed` in its TRX.
- **Mutations** (`.artifacts/0321-dev-mutation.log`). Each ran over a rebuilt tree, and each source was restored
  byte-identical:
  - M1, `OpenSplitWizard` without `guardKitWrite`: the read-only fact fails.
  - M2, the button reads `Split` for either target: the new UI fact fails.
  - M3, `SplitByColumn` ignores the chosen profile: the new model fact fails.
  - M4, the split heading reads as an import: the new UI fact fails.

  The log's first M1 entry (`build exit 1`) is my first patch, which left the guard's lambda unapplied and did not
  compile. M1 was re-run with the lambda applied, and that run is the entry that counts.
- No run exceeded its effective deadline.
- Every touched file is LF only.

## Artifacts

- `.artifacts/0321-dev-build.log`: the first build, which recompiled the vendored MathNet (two `SYSLIB0051` warnings
  from `MathNetNumerics/Numerics/Exceptions.cs`, untouched). `.artifacts/0321-dev-build-2.log`: the rebuild, with 0
  warnings and 0 errors.
- `.artifacts/0321-dev-constructor-unit-tests.log` and `.artifacts/0321-dev-ui-tests.log`, with their TRX files under
  `.artifacts/0321-dev-trx/`.
- `.artifacts/0321-dev-elevation.log`, `.artifacts/0321-dev-invariants.log`, `.artifacts/0321-dev-ui-smoke.log`.
- `.artifacts/0321-dev-mutation.log` and its script, `.artifacts/0321-mutation-tools/mutate2.py`.
- The final re-runs: `.artifacts/0321-dev-build-final.log`, `.artifacts/0321-dev-constructor-unit-tests-final.log`,
  `.artifacts/0321-dev-ui-tests-final.log` and `.artifacts/0321-dev-ui-smoke-final.log`.
