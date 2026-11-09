# Step 0185 — IMPLEMENT — impl-log

## Operator recovery — not commit-ready

The approved scene-save repair is complete under manual 014: only the
`access = ReadOnlyKitAccess` fixture line was removed. Every assertion and
identity remains. The five worker product/test files are unchanged from operator
entry. The project prompt points to the amendment; all 494 steps validate and
the receipt is refreshed with all 988 compiled artifacts byte-identical.

Measured gates: build passed with zero warnings/errors; elevation passed over
all 12 product projects from the immutable arc origin; constructor-unit-tests
passed all 1,197 cases, exact census, zero failures/skips. Smoke ran all 227
accepted cases: 226 passed, one failed, none skipped. The engine short-circuited,
so the selected UI gate and fourteen invariants did not run.

The additional failure is
`LauncherTests.acceptance: a kit open elsewhere offers Open read-only and Cancel in one row, and Open read-only opens it without writing`,
at `LauncherTests.fs:959`: `Missing control KitStatus`. The test clicks Save
scene in a genuinely read-only window and expects the old storage-error prefix.
Step 0185 now disables that control, so no write message or status is produced.
This is a second omitted test adaptation; production must keep Save disabled.

The proposed narrow repair asserts Save remains visible but disabled, clicking
it produces no status, and all kit bytes/locks remain unchanged. It preserves
the test identity and all other assertions. The old storage-error assertions
remain in SceneSaveLoadTests; direct model refusal is covered by the new
KitWindowModelTests fact. Replacing the launcher's two old status assertions
exceeds the approved assertion-preservation constraint, so operator agreement
was requested. `LauncherTests.fs` remains unchanged. The concrete patch is
`.artifacts/0185-operator-repair/launcher-read-only-proposal.patch`.

Evidence: `.artifacts/0185-operator-repair/verification-summary.json`,
`gate-results.json`, per-gate logs and archived constructor/smoke TRX reports.
The original worker handoff is archived alongside them. No ASG/STL change,
commit, restart, checkpoint creation or budget/floor edit was performed.

After agreement, extend manual 014 to the named launcher fact, apply the narrow
repair and complete verification. Keep waiting for the operator's commit. A
later explicitly requested relaunch still requires normal
`--executability-profile 1`, without `--accept-current-slice`, and measured
checkpoint 184 before 0186. The arc remains stopped.

## Historical worker report — before operator recovery

The following is the original worker report; the current status is above.

## Blocking conflict and missing authority (commit_ready: false)

**Every requirement of step 0185 is implemented inside its authorized effects. The step still cannot pass its own
`ui-tests` gate**, because a fact in a selected class asserts the old behaviour of a read-only `SceneSave`, and the
fact's file is not in the step's effects.

- **Conflicting test.** Identity
  `OpticalConstructor.Ui.Tests.SceneSaveLoadTests.a saved scene is a setup file in the kit and loads back onto the table`.
  - Owner gate: `ui-tests`. The class `OpticalConstructor.Ui.Tests.SceneSaveLoadTests` is in this step's
    `ui-tests` selection.
  - Authored at step 0153 (S4.3).
  - File `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/SceneSaveLoadTests.fs`, lines 119–137.
- **What the fact does.**
  - Its `protectedModel` sets the kit to `proxy = ordered.kit.proxy.readOnlyView` **and**
    `access = ReadOnlyKitAccess` (line 126), then sends `SceneSave`.
  - It asserts `Assert.StartsWith("Not saved to the kit — ", status.value)` and `Assert.Contains("read-only", …)`.
- **Conflicting obligation.** S53-READ-ONLY, step 0185 How-to (1), requires `SceneSave` under `ReadOnlyKitAccess`
  to answer `kitStatus = Some (ControlCaption "The kit is open read-only: nothing is saved.")` and to call no
  proxy. The acceptance pins that exact text through
  `KitWindowModelTests.a kit opened read-only refuses every write message and writes nothing`. That status does not
  start with `Not saved to the kit — `, so the fact fails once the guard lands. This follows from the code and is
  certain for that assertion. It was not measured: Invariant 6, no test run.
- **Why no in-scope implementation fixes it.** The step requires one status for all four write messages under
  read-only access, and the fact requires a different prefix for the same input. The step's census declares
  `change_expectation` only for `InverseConstructorTests.a kit write that fails keeps the experiment and shows why`.
  That fact has the identical fixture shape, and the step repairs it by keeping `access` read-write.
  `SceneSaveLoadTests.fs` is in neither `effects.edits` nor `effects.tests`.
- **Missing authority.** The step needs a `change_expectation` effect, or an operator ruling, for that fact in
  `SceneSaveLoadTests.fs` under S53-READ-ONLY at step 0185.
- **Proposed repair, the same treatment the step prescribes for `InverseConstructorTests`.** Delete line 126
  (`access = ReadOnlyKitAccess`) from the fact's `protectedModel`, so its access stays `ReadWriteKitAccess`. The
  save then reaches the `readOnlyView` proxy and fails there, and every assertion of the fact is kept.
  - By the code, the refusal is `Not saved to the kit — the kit refused the library snapshot: the kit is open
    read-only: writeKindFile`. The polarizer's snapshot is already held from the earlier save, so `writeScene` hits
    the proxy's `writeKindFile`. Had the snapshot been missing, `writeSnapshot` refuses with the same prefix and the
    same `read-only` text.
  - The fact's other checks also hold: `savedScenes`, `captureScene` and `sceneName` are unchanged, and the
    kit's scene names are unchanged.
  - No identity is added, renamed or removed, and the counts are unchanged.
- **Not done, because not authorized.** `SceneSaveLoadTests.fs` was not edited. No census, baseline, oracle, gate
  or supervisor file was edited.

## Progress

- [x] Read the protocol (the base and the `IMPLEMENT` delta), the project prompt, the slice, its gate snapshot,
  `.manual/013` and step 0184's log and state of the world.
- [x] Model: `kitWrites` and the write guard (`guardKitWrite`). The four arms were wrapped mechanically: each body
  was re-indented by four spaces inside `guardKitWrite model (fun () -> …)`, with its text unchanged. The script is
  `.artifacts/0185-worker/wrap_arms.py`.
- [x] Controls: `removalAvailability` and the row Remove availability.
- [x] View: the Experiments bay state, Attach data file, and Save scene (`availableWorkbenchButton`).
- [x] Tests: two `KitWindowModelTests` facts, one `KitWindowTests` fact, and the `InverseConstructorTests`
  fixture.
- [x] Traced every selected and all-discovered test that sends a write message. Found the `SceneSaveLoadTests`
  conflict above.
- [x] State of the world.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Ui/TableAndElementRotationView.fs`
  - **`member model.kitWrites : ControlAvailability`** sits on the extension that holds `kitCaption` and
    `kitWindowTitle`. `ReadWriteKitAccess` answers `ControlOffered` and `ReadOnlyKitAccess` answers
    `ControlWithheld`. Its one-line doc cites spec 0050 S5.3.
  - **`let private guardKitWrite (model) (write : unit -> TableAndElementRotationModel)`** sits between that
    extension and `updateTableAndElementRotation`. Under `ControlWithheld` it answers
    `{ model with kitStatus = Some (ControlCaption "The kit is open read-only: nothing is saved.") }` and calls no
    proxy. Under `ControlOffered` it runs `write ()`.
  - **The four write arms call the guard:** `ExpCommit`, `ExpRemove id`, `AttachDataFileTo (experimentId, file)`
    and `SceneSave` each read `guardKitWrite model (fun () -> <the step 152/153 body, unchanged>)`.
    - The guard comes before `experimentData.attachDataFile`, so a read-only attach copies nothing into the kit.
    - Every other arm is unchanged, including `SceneSetName`, `SceneLoad`, `ExpNew`, `ExpEdit` and the draft's.
  - **`experimentState`**:
    - `addAvailability` is `ControlWithheld` under `kitWrites = ControlWithheld`. Under `ControlOffered` it follows
      today's rule (`canCommit` and a capture).
    - `removalAvailability = model.kitWrites`.
  - **`availableWorkbenchButton (availability) (id) (label) (onClick)`** is the former `workbenchButton` body. It adds
    `Border.isEnabled availability.isEnabled` and `Border.opacity` (0.4 when withheld). It keeps the existing doc
    and adds a sentence citing S5.3.
    - `workbenchButton id label onClick` is now `availableWorkbenchButton ControlOffered id label onClick`.
    - The scene row's `Save scene` is `availableWorkbenchButton model.kitWrites …`.
  - **`attachDataFileButton`** takes `(availability : ControlAvailability)` first. It sets `isEnabled` and the 0.4
    opacity.
    - Its press handler opens the picker only under `ControlOffered`.
    - Its `SubPatchOptions.OnChangeOf` keys on `box (experiment.id.uiKey.value, availability)`, so a change of
      access re-subscribes the handler.
    - `dataFileRow` passes `model.kitWrites`.
- `Berreman/OpticalConstructor/OpticalConstructor.Controls/ExperimentControls.fs`
  - `ExperimentControlsState.removalAvailability : ControlAvailability` goes after `addAvailability`. Its doc is
    "Whether a collected experiment's Remove is offered (spec 0050 S5.3)." It is `ControlOffered` in
    `emptyExperimentControls`.
  - `rowActionButton` takes `(availability : ControlAvailability)` after its background. It sets
    `Border.isEnabled` and the 0.4 opacity as `actionButton` does, and its sub-patch key includes the availability.
  - `collectionRow (removal : ControlAvailability)`: View passes `ControlOffered` and Remove passes `removal`.
    `collectionBlock` passes `state.removalAvailability`.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitWindowModelTests.fs`
  - New opens: `OpticalConstructor.Controls`, `OpticalConstructor.Domain` and
    `OpticalConstructor.Storage.KitContentFiles` (for `writeExperiment`).
  - Fixtures: `storedExperiment`, which is E1 (`ExperimentId 1`, the same shape as `KitOpeningTests`'s
    experiment), and `folderBytes`, which maps every file under the kit folder to its bytes.
  - **`a kit opened read-only refuses every write message and writes nothing`**:
    1. E1 is written through `writeExperiment` into a read-write kit, which is then closed.
    2. The kit is reopened read-only and loaded into `initMain ()` by `withLoadedScene`.
    3. A linear polarizer is added and chosen. The fact asserts `canCommit` and that the collection holds E1.
    4. `measured.csv` is written into the kit folder, then the folder's bytes are snapshotted.
    5. Four messages are each sent from the same ready model: `ExpCommit`, `ExpRemove E1`,
       `AttachDataFileTo (E1, measured.csv)`, and `SceneSetName "Bench"` followed by `SceneSave`.
    6. Each result must keep `experimentCollection` and `savedScenes` and carry the read-only `kitStatus`. The
       folder's bytes must be unchanged.
  - **`a kit's access decides whether its writes are offered`**: `kitWrites` is `ControlOffered` for the kit opened
    read-write and `ControlWithheld` after it is closed and opened read-only.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/KitWindowTests.fs`
  - New opens for Automation, VisualTree, the FuncUI host and Elmish, Controls, Domain, `Domain.Kit`,
    `KitContentFiles`, `OpticalConstructor.Tests`, `OpticalConstructor.Ui` and the view.
  - Fixtures:
    - `storedExperiment`, which is E1;
    - `writeControlIds`: `experimentAddButton`, `experimentRemoveButton E1.uiKey`, `attachDataFile E1` and
      `saveSceneButton`;
    - `openedAt`;
    - `controlById`, which finds a control by its AutomationId;
    - `assertWriteControls`.
  - **`a read-only window withholds every write control`** (no trait):
    1. E1 is written into a fresh `TestAppScope` kit, which is closed.
    2. The kit is opened read-only. `initInverseWithKit` over it is loaded with `loadKit`.
    3. The sample is chosen, with `VaryR2`, and the Experiments bay is selected. The fact asserts E1 and
       `canCommit`.
    4. `mainView` is mounted as `ScrollExtentTests.constructorWindow` mounts it: `HostWindow` plus
       `Program.mkSimple`/`withHost`/`run`. The window is shown, and every write control must be disabled.
    5. The window and the kit are closed. Steps 2–5 are repeated read-write, and every control must be enabled.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/InverseConstructorTests.fs`
  - `a kit write that fails keeps the experiment and shows why`: both `readOnlyView` fixtures drop
    `access = ReadOnlyKitAccess`, so the access stays `ReadWriteKitAccess`.
  - The expected first refusal changes from `KitReadOnlyErr "snapshotReferences"` to
    `KitReadOnlyErr "writeKindFile"` (see Decisions). Every assertion is kept.

## Decisions

- **`InverseConstructorTests`: three fixture lines change, not one.** The fact builds two read-only views, and both
  set `access`, so both lines are removed.
  - The expected `refusal` value is a fixture too. It named `snapshotReferences`, which was the short-circuit of
    `KitLibraryContext.snapshotReferences` under `ReadOnlyKitAccess` (`KitLibrary.fs:119`). With the access left
    read-write, the write reaches the proxy, as the step intends.
  - The inverse scene's three elements are unbound (`ElementPlacement.create` sets `valueId = None`), so the
    experiment has no library references. `snapshotReferences` therefore answers `Ok []`, and `writeExperiment`
    reaches `readOnlyView.writeKindFile`, which is `KitReadOnlyErr "writeKindFile"`.
  - `Assert.Contains(refusal.describe, …)` is unchanged; only the expected reason it names changed. The later
    refusals (edit, attach, remove) still contain `read-only` (`writeKindFile` and `deleteKindFile`).
- **The UI fact seeds through `initInverseWithKit`**, the kit-taking half of `initInverse`, with the same seed. This
  matches `SceneSaveLoadTests.sceneInKit` and leaves no scratch kit unclosed. The draft is the sample with `VaryR2`,
  as `InverseConstructorTests.withIntensityExperiment` commits it in the inverse scene, so Add is genuinely offered
  under read-write.
- **The model fact finds the polarizer by kind** (as `SceneSaveLoadTests` does) rather than `List.item 2`. Each write
  message is sent from the same ready model, so each refusal is independent.
- **The data file is written after the read-only open and before the snapshot.** The open never sees a stray file,
  and the snapshot covers it.
- **`folderBytes` is a private fixture in `KitWindowModelTests`.** `KitOpeningTests.folderBytes` is private, and that
  file is outside this step's effects. `ExperimentCollectionRoundTripTests` likewise holds its own. This version keys
  by full path and covers the kit folder.
- **`addAvailability` nests the kit check first**, so today's rule stays verbatim under `ControlOffered`.
- **No `workbenchButton` doc.** A doc saying "always offered" would only restate the call; the one-liner speaks for
  itself.
- **ELEV, checked by reading the rules and the analyzer source (`NoBoxIntoObjSink.fs`); no analyzer was run.**
  - New names are private or members (`kitWrites` is new on its receiver: ELEV015/019).
  - The record field is elevated (ELEV008).
  - `box` wraps tuples, never a single-case union (ELEV007).
  - Tuples have at most three elements with distinct types (ELEV011).
  - Nothing maps to `bool` (ELEV012). `.value` appears only at Avalonia and text sinks (ELEV004, excluded).

## Testing state

**`commit_ready: false`.** The `SceneSaveLoadTests` fact above conflicts with S53-READ-ONLY, and the authority to
repair it is missing. Everything else the step asks for is implemented.

Under Invariant 6 this `IMPLEMENT` worker ran no gate: no build, no test, no analyzer and no invariant grep. These
checks were made without running a gate:

- All five edited files are LF (0 CR bytes), `git diff --check` is clean, and `--ignore-cr-at-eol` gives the same
  numstat.
- No added line contains any s21 invariant word.
- Every API used was checked against its declaration:
  - `openKitAt`, `ClockProvider.system`, `writeExperiment`, `FileName.combine`, `initInverseWithKit` and
    `loadKit`;
  - `ConstructorUiId.experimentRemoveButton (ConstructorUiKey)`, `ConstructorUiId.attachDataFile (ExperimentId)`
    and `ExperimentId.uiKey` (`ConstructorUiIdOfDomain`, auto-opened);
  - `BayNames.experiments`, `ExperimentControls.VaryR2`, `Experiments.ExperimentCollection.canCommit`, and the
    `TestAppScope` and `SceneTestFixture` helpers.
- `ExperimentControlsState` is built in full only in `emptyExperimentControls` and `experimentState`, and both set
  the new field.
- Every test that sends `ExpCommit`, `ExpRemove`, `AttachDataFileTo` or `SceneSave` was traced:
  - `ElementMenuTests`, `ExperimentControlsTests`, `MainSceneMsgTests`, `ScrollExtentTests` and
    `SolverHandoffWindowTests` use read-write kits, and the guard passes them through unchanged.
  - `InverseConstructorTests` is fixed within the step's effects.
  - `SceneSaveLoadTests` is the blocking conflict.

Test identities created, all declared by the step:

- `OpticalConstructor.Tests.KitWindowModelTests.a kit opened read-only refuses every write message and writes nothing`
- `OpticalConstructor.Tests.KitWindowModelTests.a kit's access decides whether its writes are offered`
- `OpticalConstructor.Ui.Tests.KitWindowTests.a read-only window withholds every write control`

The changed expectation, as declared, is
`OpticalConstructor.Ui.Tests.InverseConstructorTests.a kit write that fails keeps the experiment and shows why`.
None is removed or renamed.

Expected counts:

- constructor-unit-tests: 1195, from checkpoint 183 (step 0184), to 1197 (+2).
- ui-smoke: unchanged at 227.
- ui-tests over the selected classes: +1. That gate's pass needs the `SceneSaveLoadTests` repair authorized above.

## Artifacts

- `.artifacts/0185-worker/wrap_arms.py`: the mechanical re-indent that wrapped the four write arms in the guard.
  It changes indentation and adds the wrapper line and closing parenthesis only.

## Gotchas

- `KitLibraryContext.snapshotReferences` short-circuits on `ReadOnlyKitAccess` with
  `KitReadOnlyErr "snapshotReferences"` before touching the proxy. A test that wants a write to reach a refusing
  proxy must keep the access read-write and swap only the proxy (`readOnlyView`). Its refusal reason is then the
  proxy's: `writeKindFile`, `writeSnapshot` or `deleteKindFile`.
- Since this step, `ReadOnlyKitAccess` on the model alone makes the four write messages answer
  `The kit is open read-only: nothing is saved.` before any proxy call. A fixture that sets `access = ReadOnlyKitAccess`
  and expects a `Not saved to the kit — …` status is now wrong, which is why `SceneSaveLoadTests` conflicts.
- A Python heredoc writing F# needs care with `\n` escapes. The first write of the model fact turned
  `"x,intensity\n…"` into literal newlines inside the F# string, and it was repaired.
- A disabled Avalonia `Border` receives no pointer presses, because input hit-testing skips non-enabled elements.
  The model guard is still the enforcement, and the disabled control is the affordance.
