# Step 0185 — IMPLEMENT — impl-log

## Operator recovery — commit_ready: true

The operator approved the secondary review's four recovery steps on 2026-11-08.
Manual 014 supplies the omitted fixture authority under S53-READ-ONLY. The
project prompt directs every role to it at 0185, 0187, 0188 and 0260.
After the first verification exposed a second obsolete expectation, the
operator also authorized the additional test repair. The earlier repair is
already committed as `6607a493b05cfe41b86947ad44905a35d383d228`.

Removed only `access = ReadOnlyKitAccess` from the existing scene-save fact's
`protectedModel` in `SceneSaveLoadTests.fs`. It retains the refusing proxy and
every assertion, test identity and case. Its inherited read-write access lets
the save reach the storage refusal. The new read-only model and control facts
independently cover the earlier guard. The five worker product/test files were
preserved byte-for-byte from operator entry.

Updated the existing launcher read-only acceptance fact to check that Save is
visible but disabled and that clicking it leaves `KitStatus` absent. Its former
storage-error expectation depended on a click that the new disabled control
correctly ignores. Every other launcher assertion, including file/lock checks,
remains. The scene-save storage-refusal assertions and direct model refusal
tests retain their separate coverage; no identity, case or gate was removed.

The escalation was an omitted later fixture migration, not evidence that the
accepted storage-refusal assertion was wrong. The worker correctly preserved
the unauthorized test. The primary and secondary reports remain unchanged;
the original worker handoff is archived under the recovery artifacts.

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
  conflict, now resolved by manual 014.
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

## Measured verification

All 19 unchanged gates passed through ASG's gate evaluator. The Release build
reported zero owned-code warnings; two existing SYSLIB0051 warnings in vendored MathNet and zero errors. Elevation actually analyzed all 12 product
projects from immutable origin `f395597c071910aaa215518626563f6c784bfb62`, with
zero findings under the existing ELEV004 exclusion. The first run's smoke
failure is archived under `initial-verification/`. After the authorized test
repair, the entire roster passed without a failure waiver.

| Test gate | Cases | Method identities | Failed | Skipped | Accepted census |
|---|---:|---:|---:|---:|---|
| constructor-unit-tests | 1197 | 1172 | 0 | 0 | exact match |
| ui-smoke | 227 | 227 | 0 | 0 | exact match |
| ui-tests | 47 | 47 | 0 | 0 | exact match |

The complete constructor suite rose from 1,195 to 1197 cases
through its two declared additions. Smoke remains 227. The selected
UI census includes all four declared classes and the one new controls fact;
it is not a whole-suite UI baseline. The scene-save, inverse-refusal and new
read-only facts and the repaired launcher fact all passed. Neither repair changes
an identity or requires an operator census row.

| Frozen gate | Result | Seconds |
|---|---|---:|
| build | PASS | 70.1 |
| elevation | PASS | 173.6 |
| constructor-unit-tests | PASS | 58.3 |
| ui-smoke | PASS | 158.2 |
| ui-tests | PASS | 21.6 |
| invariant-s21-elev-list-empty | PASS | 0.2 |
| invariant-s21-no-vstest | PASS | 0.2 |
| invariant-s21-no-kit-kit-json | PASS | 0.2 |
| invariant-s21-no-eps-axis-evaluated | PASS | 0.2 |
| invariant-s21-complexity-mandatory | PASS | 0.2 |
| invariant-s21-no-seeded-string-preset-ids | PASS | 0.2 |
| invariant-s21-no-seeding-seam | PASS | 0.2 |
| invariant-s21-t1-no-code-seeds | PASS | 0.2 |
| invariant-s21-no-seed-modules | PASS | 0.2 |
| invariant-s21-no-test-db | PASS | 0.2 |
| invariant-s21-no-versions-in-use | PASS | 0.2 |
| invariant-s21-no-scene-collection-seams | PASS | 0.2 |
| invariant-s21-t4-no-ocproj | PASS | 0.2 |
| invariant-s21-no-recent-json | PASS | 0.2 |

All 494 steps validate. The profile-1 receipt was refreshed for the prompt
amendment; all 988 compiled artifacts are byte-identical. Published contracts,
operator census, checkpoint/case records, supervisor state, budgets, package
versions and version lock remain unchanged. Checkpoint 184 is still absent.
No ASG or STL source change, commit or relaunch was performed.

Evidence: `.artifacts/0185-operator-repair/verification-summary.json`,
`gate-results.json`, `test-inventory.json`, `elevation-coverage.json`,
`contract-validation.json`, the archived TRX reports and per-gate logs.
`final-scope-check.json` records the final protected-file and Git checks.

## Handoff

Wait for the operator's commit and explicit relaunch request. Manual 014 overrides
the generic committed-green acceptance rule for this recovery: resume normally
with an operator reply and `--executability-profile 1`, without
`--accept-current-slice`, and reattach the monitor. The supervisor must review
the repaired slice, rerun its frozen gates and publish measured checkpoint 184
and case evidence before 0186. Do not write that checkpoint by hand, lower the
inherited floors or reset budgets.

## Gotchas

- A proxy-refusal fixture must retain read-write model access. Read-only access
  now stops at the model guard before `snapshotReferences` or a proxy call.
- Keep both behaviors covered through 0187, 0188 and the 0260 setup-test rename.
- A disabled control is the affordance; the model guard enforces refusal even
  when a write message arrives directly.
- Valid live `.specrc` changes and all `.manual/` descendants remain allowed
  under ASG 0090 manual 033; the primary report's contrary advice is obsolete.
