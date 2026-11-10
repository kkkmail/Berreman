# Step 0240 — impl log (IMPLEMENT: the item edit messages)

## Progress

- [x] Read the protocol (arc-runner + IMPLEMENT worker), the project prompt, the slice and its `.gates` snapshot.
      The operator note is empty.
- [x] `TableAndElementRotationView.fs`:
  - `ParameterStateChoice`, `ParameterEditField` and `ParameterEditDraft` before the model;
  - the model's `parameterEditOpt`;
  - the nine messages and their arms;
  - the private helper `recordKitItemEdit` and its private `KitItemEditError`.
- [x] `ItemsPanelModelTests.fs`: the six declared facts, plus their fixture helpers.
- [x] The `ItemsPanelModelTests` class: 11 of 11 passed, step 239's 5 plus the 6 new.
- [x] A ten-mutation check of the new code: all ten caught, and the source restored byte for byte.
- [x] Two readability edits after the check, then a full rebuild:
  - one doc line shortened and the long `TryParse` lines wrapped;
  - the outer `sigmaOpt` binding renamed `readSigma`, so the lambda no longer shadows it.
- [x] Development runs over the final tree (results under Testing state):
  - build;
  - the whole constructor-unit-tests suite;
  - ui-smoke;
  - ui-tests (`KitWindowTests`);
  - elevation from the arc origin;
  - the fourteen s24 greps.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Ui/TableAndElementRotationView.fs`:
  - **Before the model:**
    - `ParameterStateChoice`: `KnownStateChoice` | `UnknownStateChoice`.
    - `ParameterEditField`: `ValueEditField` | `LowerEditField` | `UpperEditField` | `PriorSigmaEditField`. Its
      `member this.text` gives the field's name as a refusal states it: `value`, `lower`, `upper` or
      `prior sigma`.
    - The `ParameterEditDraft` record, with `static member ofParameter`, `member this.withFieldText` and
      `member this.tryState`.
  - **The model** gains `parameterEditOpt : ParameterEditDraft option` after `addItemFormOpt`. `initWith` sets it to
    `None`.
  - **Messages**, after `AddItem`:
    - `SetParameterState of ItemId * ItemModels.ParameterName * ParameterState`;
    - `EditItemParameter of ItemModels.ParameterName`;
    - `SetParameterEditChoice of ParameterStateChoice`;
    - `SetParameterEditText of ParameterEditField * ControlCaption`;
    - `CancelParameterEdit`;
    - `RelabelKitItem of ItemId * ItemLabel`;
    - `SetKitItemNotes of ItemId * ItemNotes`;
    - `SetKitItemPhoto of ItemId * FileName`;
    - `RemoveKitItemPhoto of ItemId`.
  - **In the extension block after `guardKitWrite`:** `member internal model.kitItemsWith (item : Item) : Item list`.
    It returns the held items with the item in its id's place, or added, in ordinal label order.
  - **After that block:**
    - `type private KitItemEditError`: `ItemEditRefusedErr of ItemError` | `PhotoAttachRefusedErr of KitError`. Its
      `caption` gives the two refusal texts.
    - `let private recordKitItemEdit`.
  - **The arms:**
    - nine new arms after `AddItem`;
    - `SelectKitItem` of a held id now also sets `parameterEditOpt = None`;
    - `AddItem`'s inline label-order sort is replaced by `model.kitItemsWith item`. The behaviour is the same:
      `addTo` already refused a held id.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/ItemsPanelModelTests.fs`:
  - New opens: `System.Globalization`, `System.IO`, `System.Text`, `Softellect.Sys.Primitives`,
    `OpticalConstructor.Storage` and `OpticalConstructor.Storage.KitFolderStore`.
  - Fixture values and helpers after step 239's facts: `retardance`, `lp1Draft`, `strangerId`,
    `retardanceUnknown`, `photoBytes`, `itemValue`, `withKitScene`, `withPhotoFile`, `openDraft`, `itemFilesOf` and
    `statusOf`.
  - The six declared facts.

No other file changed: no project file, Domain or Storage file, and no view.

## Decisions

- **One write path for the five item edits.** `recordKitItemEdit id edit onRecorded model` runs in this order:
  1. It runs through `guardKitWrite`, step 185's guard, first.
  2. An id `kitItems` does not hold returns the model unchanged.
  3. It runs `edit held`, the Domain member. A refusal sets `kitStatus` to its caption and writes nothing more.
  4. It records the edited item through `{ kit = model.kit; imports = model.catalogueImports }`.
  5. On `Ok ()` it sets `kitItems = model.kitItemsWith edited` and `kitStatus = None`, then applies `onRecorded`.
     On `Error e` it sets `Not saved to the kit — <e.describe>` and keeps `kitItems`.

  Each arm then passes only its Domain member:

  | Message | Domain member |
  |---|---|
  | `SetParameterState` | `withParameterState name state` |
  | `RelabelKitItem` | `relabelledAmong label model.kitItems` |
  | `SetKitItemNotes` | `withNotes` |
  | `RemoveKitItemPhoto` | `withPhoto None` |
  | `SetKitItemPhoto` | `model.experimentData.attachDataFile file`, then `withPhoto (Some attachment.path)` |

  The attach runs inside `edit`, so it happens after the guard and after the id lookup.
- **A relabel is a changed label.** The helper records through `recordRelabelledItem held edited` when
  `edited.label <> held.label`, and through `recordItem edited` otherwise.
  - Only `RelabelKitItem` can change a label, so this is the slice's "for a relabel" without a mode parameter.
  - A relabel to the identical label records through `recordItem`. That gives the same end state as
    `recordRelabelledItem`, whose same-stem branch deletes the file and rewrites it.
  - A change of case alone is a changed label, so it takes step 237's same-stem path.
- **The refusal texts live in one place.** The private `KitItemEditError` carries the typed cause. Its `caption`
  writes `The item was not changed — <e.describe>` and `The photo was not attached — <e.describe>` exactly once.
  This follows the AGENTS rule that errors are values: the cases end in `Err`, and both names are unique among ours.
- **Closing the editor.** `SetParameterState` closes the editor only once its item is recorded, through
  `onRecorded`. A refused state leaves the editor open with its texts, so the user can correct them. The four other
  edits pass `fun recorded -> recorded`.
- **The label-order sort is shared.** `kitItemsWith` replaces step 239's inline sort in `AddItem` instead of copying
  it, so one comparison now serves both arms. The comparison is ordinal, as `readItems` uses.
- **`EditItemParameter name`** opens `ParameterEditDraft.ofParameter item.id parameter` for the selected held item's
  parameter of that name. Without a selected item, or without that parameter, it changes nothing.
- **The editor messages.**
  - `SetParameterEditChoice` and `SetParameterEditText` change only an open draft; without one they change nothing.
  - `CancelParameterEdit` sets `parameterEditOpt = None`.
  - None of the three is guarded, because none writes.
- **`SelectKitItem`** clears the editor only when it selects a held id. An unheld id still returns the model
  unchanged, which step 239's fact 3 asserts.
- **`tryState`:**
  - It reads with `Double.TryParse(text, NumberStyles.Float, CultureInfo.InvariantCulture)`.
  - A blank sigma, empty or whitespace only, means no prior.
  - A text that does not read as a number is `InvalidParameterStateErr "<name>: the <field> '<text>' is not a
    number"`. The fields are read in the order value, lower, upper, prior sigma.
  - It applies no rule: `withParameterState` applies `Parameter.validate`, as the slice requires.
- **`ofParameter`** writes numbers with `ToString("R", CultureInfo.InvariantCulture)`, so an unknown's draft reads
  back as the same state. The facts check this both ways.
- **"Each of the six write messages".** The slice's own list has five item writes. The read-only fact sends those
  five plus step 239's `AddItem` (of LP1): the Items panel's six writes. Recorded under Gotchas.
- **The test fixture.** `withKitScene` reuses `withScene` instead of copying it.
  - It swaps in a `KitContext` over an `InMemoryKitFolder` the fact holds. The kit file, access and clock are the
    scratch kit's, and the proxy is `KitProxy.createInMemory folder`, initialised with the same kit file.
  - It sets `experimentData` to `ExperimentDataStore.createFileBacked` over that proxy, as `initWith` does.
  - The facts can then read the kit's `paths ()`, which the scratch kit, hidden inside `KitContext.createInMemory`,
    does not expose.
  - The read-only fact builds step 239's read-only model over that folder, with
    `kitItems = readOnlyProxy.readItems ()`, and adds `experimentData` over the read-only proxy, as a read-only
    opening has.
- **The photo file.** `withPhotoFile` writes `CPL1.jpg` under
  `Path.Combine(AppContext.BaseDirectory, "ItemsPanelModelTests", <new GUID>)` and deletes that folder in a
  `finally`.
  - Its bytes are ASCII text (`CPL1 on its mount`). The in-memory kit stores a copied file as UTF-8 text, so
    ASCII keeps the copy's content hash comparable with the picked bytes.
- **Assertions added beyond the slice's minimum, within the six declared ids.** No other identity was added.
  - **Fact 1:**
    - the other parameters keep their defaults;
    - the history stays empty;
    - the editor closes;
    - nothing else in the model changes.
  - **Fact 2:**
    - the status starts with `The item was not changed — ` and names `c2.retardance`;
    - the editor stays open;
    - an unknown parameter name is told;
    - an unheld id changes nothing.
  - **Fact 3:**
    - without a selection, a parameter or a draft, nothing changes;
    - typing writes nothing;
    - a non-number sigma names `prior sigma`;
    - `ofParameter` of the typed unknown equals the typed draft and reads back as the state;
    - `SelectKitItem` closes the editor.
  - **Fact 4:**
    - the model is unchanged apart from `kitItems`;
    - a refused relabel leaves the item files alone;
    - a change of case alone renames the one file;
    - an unheld id changes nothing.
  - **Fact 5:**
    - the copied photo's hash equals the picked bytes';
    - the picked file stays where it was;
    - removing the photo leaves the paths unchanged;
    - a missing photo file is told (`The photo was not attached — `) and writes nothing.
  - **Fact 6:**
    - each refused message changes nothing but the status;
    - the kit's items are unchanged at the end.

## Testing state

No gate verdict is claimed here (Invariant 6). These are development runs; the logs are in `specs/0050/.artifacts/`.

- **build** (`dotnet build Berreman.slnx -c Release -nologo -v:m`):
  - Final run: exit 0, `0 Warning(s)`, `0 Error(s)`, in 35 s. Ui, Tests and Ui.Tests were rebuilt. Log:
    `0240-build-final.log`.
  - An earlier Ui-only build (`0240-build-ui-1.log`) showed the vendored MathNet SYSLIB0051 pair, as step 0239
    recorded (manual 013). Neither solution build has them (`0240-build.log` is the first).
- **ItemsPanelModelTests class**, before the mutation check: `Passed!`, 11 of 11. Log:
  `0240-items-panel-class.log`.
- **Mutation check** (`0240-mutations.py`, result `0240-mutations.log`): ten mutations of the new code, each
  rebuilt and run over `ItemsPanelModelTests`. All ten were caught by a declared fact, and the source was restored
  byte for byte (`restored: True`).

  | Mutation | Caught by |
  |---|---|
  | M01: no guard | the read-only fact |
  | M02: no record | facts 1 and 5 |
  | M03: a relabel recorded as a plain write | the relabel fact |
  | M04: the editor stays open | fact 1 |
  | M05: the prior centred on the lower bound | the editor fact |
  | M06: `SelectKitItem` keeps the editor | the editor fact |
  | M07: the photo is not removed | the photo fact |
  | M08: a calibration's value is dropped | the editor fact |
  | M09: `kitItems` not replaced | facts 1, 4 and 5 |
  | M10: the relabel ignores the held items | the relabel fact |
- **constructor-unit-tests**: the gate's command over every test discovered.
  - Result: `Passed!`, total 1377, succeeded 1377, failed 0, skipped 0, in 66 s (estimate 180 s).
  - That is checkpoint 238's 1371 plus the 6 declared facts. The TRX shows all six `Passed` under exactly the
    declared ids.
  - Log: `0240-constructor-unit-tests.log`.
- **ui-smoke**: the gate's command.
  - Result: `Passed!`, 242 succeeded, 0 failed, 0 skipped, in 212 s (attempt timeout 600 s).
  - 242 equals the baseline. Log: `0240-ui-smoke.log`.
- **ui-tests**: the gate's command with the step's selector, `OpticalConstructor.Ui.Tests.KitWindowTests`.
  - Result: `Passed!`, 13 succeeded, 0 failed, 0 skipped, in 19 s. Log: `0240-ui-tests.log`.
- **elevation** (`ASG_ARC_BASE_COMMIT=f395597c…`, from `.arc-base-json`; ELEV004 excluded):
  - Result: exit 0, `ELEV-GATE: OK - 0 new finding(s) in 12 project(s), 196s`. Known findings: 0.
  - Log: `0240-elevation.log`.
- **The fourteen s24 invariant greps:** each finds nothing. Both touched source files are tracked, so `git grep`
  sees them.
- **Line endings:** `git diff --numstat` equals `--ignore-cr-at-eol` (+340/−0 tests, +240/−4 view), and no touched
  file holds a CR.
- **Test output:** each photo folder is deleted by its fact. Only the empty `ItemsPanelModelTests` parent stays, in
  the test project's `bin` output.

No run came near a deadline. No test is removed or renamed, and no assertion is removed or weakened.

## Gotchas

- **"The six write messages" is a miscount in the slice.**
  - The slice lists nine new messages and calls four of them non-writes, which leaves five.
  - The acceptance names the same five: `SetParameterState`, `RelabelKitItem`, `SetKitItemNotes`, `SetKitItemPhoto`
    and `RemoveKitItemPhoto`.
  - The read-only fact sends those five plus step 239's `AddItem`, the other write of the Items panel. That keeps
    "six" literal and adds no identity.
- **The scratch kit's folder is unreachable.** `KitContext.createInMemory` keeps its `InMemoryKitFolder` private.
  A fact that lists a kit's `paths` must build its kit over a folder it holds, as `withKitScene` does.
- **The attach is a write that a later failure does not undo.** If `recordItem` fails after `SetKitItemPhoto` has
  copied the photo into `data/`, the copy stays. This matches `AttachDataFileTo`, which keeps a successful
  attachment. A lab file is never deleted by the app (001 §E).
- **`id` is shadowed in an arm that binds `id`.** The new arms bind `itemId` and pass `fun recorded -> recorded`,
  never `id`.

## Artifacts

- Build: `0240-build-ui-1.log`, `0240-build.log`, `0240-build-final.log`.
- Tests: `0240-items-panel-class.log`, `0240-constructor-unit-tests.log`, `0240-ui-smoke.log`, `0240-ui-tests.log`.
- Elevation: `0240-elevation.log`.
- The mutation check: `0240-mutations.py` and its result, `0240-mutations.log`.
