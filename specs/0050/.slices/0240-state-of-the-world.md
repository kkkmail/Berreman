# Step 0240 — state of the world

## Where we are

Spec 0050, section S7.2 ("The Items panel, the flow strip and item mapping on copy"), step 0240 (IMPLEMENT),
obligation `S72-ITEM-EDIT-MESSAGES`.

- Step 0239 gave the Items panel its state and `AddItem`.
- This step adds the edits of a held item, as semantic messages of the constructor window (001 §P
  `SetParameterState`; 001 §O's parameter grid):
  - the parameter editor;
  - the label, notes and photo edits.

  Each edit writes through the kit.
- No view reads the state yet:
  - the Items panel view is 0241's;
  - the strip view is 0242's;
  - the App's item-model wiring is 0243's.

## What's working

- Added the parameter editor's draft (`ParameterEditDraft`). It opens on a parameter's state and reads back the
  known or unknown state typed into it, refusing a text that is not a number by naming its field.
- Added `SetParameterState`, `RelabelKitItem`, `SetKitItemNotes`, `SetKitItemPhoto` and `RemoveKitItemPhoto`:
  - each changes one item through its Domain member and records it in the kit;
  - a refusal is told;
  - a kit opened read-only takes no edit.
- Added the editor messages `EditItemParameter`, `SetParameterEditChoice`, `SetParameterEditText` and
  `CancelParameterEdit`, which write nothing.
- Added six `ItemsPanelModelTests` facts (constructor-unit-tests 1371 → 1377).

## Tests

No gate verdict is claimed here (Invariant 6). After exit, the arc-runner runs the frozen roster:

- build;
- elevation;
- constructor-unit-tests, over everything discovered;
- ui-smoke, over everything discovered;
- ui-tests, selected to `OpticalConstructor.Ui.Tests.KitWindowTests`;
- the fourteen s24 invariants.

The worker's development runs over the final tree are in the impl-log's `Testing state`, with logs under
`specs/0050/.artifacts/0240-*`:

- **build:** exit 0, `0 Warning(s)`, `0 Error(s)`.
- **constructor-unit-tests:** `Passed!`, 1377 succeeded, 0 failed, 0 skipped, in 66 s (estimate 180 s).
  - That is checkpoint 238's 1371 plus the 6 declared facts, all `Passed` in the TRX under their declared ids:
    - `SetParameterState sets an item's parameter and records it`;
    - `a parameter state that breaks its rules is told and nothing is recorded`;
    - `the parameter editor starts from the parameter's state and reads the state it sets`;
    - `relabelling an item renames its kit file, and a label another item holds is told`;
    - `notes and a photo are recorded with the item, and removing the photo keeps its file`;
    - `a kit opened read-only edits no item and writes nothing`.
- **ui-smoke:** `Passed!`, 242 succeeded, 0 failed, 0 skipped, in 212 s (attempt timeout 600 s).
- **ui-tests (`KitWindowTests`):** `Passed!`, 13 succeeded, 0 failed, 0 skipped, in 19 s.
- **elevation** from the arc origin `f395597`: exit 0, `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, in
  196 s. Known findings: 0.
- **The fourteen s24 invariant greps:** none finds anything.
- **Mutation check:** ten mutations of the new code were each caught by a declared fact. The source was then
  restored byte for byte and rebuilt. The mutations were:
  - the guard;
  - the recording;
  - the relabel path;
  - closing the editor;
  - the prior's centre;
  - `SelectKitItem`'s reset;
  - removing the photo;
  - a calibration's value;
  - replacing `kitItems`;
  - the held items in a relabel.

No run came near a deadline. No test is removed or renamed, and no assertion is removed or weakened.

```yaml
gates:
  constructor_unit_tests: 1377
  ui_smoke_tests: 242
  ui_tests: 13
```

## Architecture

- **The five item writes share one path**, `recordKitItemEdit`. It runs in this order:
  1. step 185's guard;
  2. the held-id lookup;
  3. the Domain edit, telling a refusal through the private `KitItemEditError`;
  4. the recording: `recordRelabelledItem` when the label changed, `recordItem` otherwise;
  5. on success, the held items are replaced in label order and the status is cleared.
- **A photo is attached as a lab file.** The attach runs inside the edit, after the guard and the lookup, through
  `experimentData.attachDataFile`: a file outside the kit is copied into `data/`.
- **The label-order sort now lives in one member**, `kitItemsWith`, shared with step 239's `AddItem`.
- **The editor is window state**, as the add form is: `parameterEditOpt` is never stored.
  - `SetParameterState` closes it once the state is recorded.
  - A refused state leaves it open.
  - Pressing an item row closes it.
- **`tryState` checks no rule.** `withParameterState` applies `Parameter.validate` (step 214), so a state has one
  rule set.

## Deferred

Nothing in this step's scope is deferred. The views and the App wiring are steps 0241–0243's own slices.

## Gotchas

- **The slice's "six write messages" are five new ones.** The read-only fact also sends step 239's `AddItem`, the
  Items panel's sixth write. See the impl-log.
- **The scratch kit's folder is private to `KitContext.createInMemory`.** A fact that lists a kit's `paths` builds
  the scene's kit over an `InMemoryKitFolder` it holds (`withKitScene`).
- **An in-memory kit stores a copied file as UTF-8 text.** A fact that compares a copy's hash with its source
  bytes must use text-safe bytes.
- **A photo copied in stays if the later item write fails**, as `AttachDataFileTo` keeps its attachment. The app
  never deletes a lab file (001 §E).

## Changelog

- 2026-11-09 — Step 0240:
  - added `ParameterStateChoice`, `ParameterEditField`, `ParameterEditDraft` and the model's `parameterEditOpt`;
  - added the nine edit and editor messages;
  - added six `ItemsPanelModelTests` facts (constructor-unit-tests 1371 → 1377).
