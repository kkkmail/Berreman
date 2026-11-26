# Step 0239 — impl log (IMPLEMENT: the Items panel's state and AddItem)

## Progress

- [x] Read the protocol (arc-runner + IMPLEMENT worker), the project prompt, the slice and its `.gates` snapshot.
      The operator note is empty.
- [x] `ItemIdProvider.system` in `KitOpening.fs`.
- [x] `AddItemForm`, the four model fields, `DefaultStoreSet.itemModels`, the seven messages and their arms in
      `TableAndElementRotationView.fs`.
- [x] `ItemsPanelModelTests.fs` with the five declared facts, appended to the test project.
- [x] A ten-mutation check. It first found one surviving mutant (M02, polarizer de-duplication). Fact 1 was extended
      to catch it, and the whole check was re-run: all ten caught.
- [x] Development runs over the final tree, after a full rebuild from the restored source (results under Testing
      state):
  - build;
  - the whole constructor-unit-tests suite;
  - ui-smoke;
  - ui-tests over `KitWindowTests`;
  - elevation from the arc origin;
  - the fourteen s24 invariant greps.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Ui/KitOpening.fs` (+9): `type ItemIdProvider with static member
  system : ItemIdProvider`, beside `ClockProvider.system`. It mints `ItemId (Guid.NewGuid ())` on every call.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/TableAndElementRotationView.fs` (+167):
  - `type AddItemForm` before the model, with fields `itemId`, `offers`, `chosenOpt`, `labelText` and `dial`, and
    `member this.itemDraftOpt : ItemDraft option`.
  - Model fields after `flowStep`: `itemModels : ItemModels.ItemModelProxy`, `itemIds : ItemIdProvider`,
    `selectedItemOpt : ItemId option` and `addItemFormOpt : AddItemForm option`. `initWith` sets them to
    `ItemModels.ItemModelProxy.createInMemory ()`, `ItemIdProvider.system`, `None` and `None`.
  - `DefaultStores.DefaultStoreSet.itemModels`, which is `stores.itemModels` of the in-memory catalogue stores the
    Built-in catalogue is imported into. `initTableAndElementRotation` and `initMain` set it by record update,
    beside `sources` and `catalogueImports`.
  - Messages after `ShowFlowStep`: `SelectKitItem of ItemId`, `OpenAddItemForm`,
    `ChooseNewItemModel of ItemModelChoice`, `SetNewItemLabel of ControlCaption`, `SetNewItemDial of ItemDial`,
    `CancelAddItemForm` and `AddItem of ItemDraft`.
  - `member internal model.addItemOffersOver (itemModels : ItemModels.ItemModelVersion list) : ItemModelChoice list`,
    in the extension block after `guardKitWrite` (beside `closeCopySource`).
  - The seven arms in `updateTableAndElementRotation`, after the `ShowFlowStep` arms.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/ItemsPanelModelTests.fs` (new): `module ItemsPanelModelTests`
  in `namespace OpticalConstructor.Tests`, with the five declared facts.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj` (+1): one
  `<Compile Include="ItemsPanelModelTests.fs" />`, last in the compile list, with no comment.

No other file changed. Every touched file is LF: `git diff --numstat` equals `--ignore-cr-at-eol`, and no file holds
a CR.

## Decisions

- **The binding models come from the one listing.** `sourceModel`, `polarizerModel` and `sampleModel` are the first
  versions in `model.itemModels.listItemModels ()` whose form takes that binding (`bindingKindOpt = Some
  SourcePresetBinding`, `PolarizerPresetBinding` or `StackSampleBinding`). Those forms are exactly
  `SourcePresetItemModel`, `IdealPolarizerItemModel` and `StackSampleItemModel`.
  - "First" is in the store's own order, name then id.
  - No model is manufactured, and no preset or sample stands in for one.
  - A binding model the listing lacks offers nothing of its kind.
- **The standalone offers** are the listed versions with `bindingKindOpt = None`, as `LibraryModelChoice`s, in the
  store's order. With the Built-in stores these are the eight shipped standalone models, ordered by name.
- **Offer order**, as the slice gives it:
  1. the standalone models;
  2. the source presets of `entriesForKind LightSource`;
  3. the polarizer presets of `entriesForKind LinearPolarizer`, then `CircularPolarizer`, de-duplicated by entry id;
  4. the samples of `entriesForKind Sample` as `StackSampleChoice (sampleModel, SampleVersionId.firstOf s.id, s)`.

  Each listing that fails offers nothing of its own kind, as `allowedEntryIds` treats one. The two polarizer listings
  are independent.
- **A failed item-model listing.** It sets `kitStatus` to `The item models could not be listed — <e.describe>`, and
  the form still opens under a minted id.
  - Its offers are the ones the failed listing still allows. Every binding model comes from that same listing, so
    no preset or sample can be offered either, and the form opens with no offers.
  - This is how I read the slice's "the other offers stay" together with its "MUST be … from that same list" and
    "never manufacture a model". Recorded under Gotchas.
- **The offer builder is a private-to-the-assembly model member** (`member internal`), following this file's
  `member internal model.…` precedent. It is not a free function of one of our types, so ELEV013 does not apply.
  Being a member, it is also exempt from the primitive-signature rules.
- **`AddItem`** follows the slice step by step:
  1. It runs through `guardKitWrite`, step 185's private read-only guard, first.
  2. It calls `draft.addTo model.kitItems`. An `Error e` sets `kitStatus` to `The item was not added — <e.describe>`
     and nothing else.
  3. It calls `{ kit = model.kit; imports = model.catalogueImports }.recordItem item`. On `Ok ()` it sets:
     - `kitItems` to the item added to the held items, in label order (ordinal, the comparison `readItems` uses);
     - `selectedItemOpt = Some item.id`;
     - `addItemFormOpt = None`;
     - `kitStatus = None`.

     On `Error e` it keeps the items and sets `Not saved to the kit — <e.describe>` (step 152's text).
- **The ordering line is inline.** `List.sortWith (fun first second -> String.CompareOrdinal(first.label.value,
  second.label.value))` matches `readItems` and the file's own `SceneSave` arm. A shared named comparer would live in
  Domain or Storage, outside this step's authorized edits.
- **`SelectKitItem id`** selects only an id `kitItems` holds; any other id returns the model unchanged.
- **The form messages.** `ChooseNewItemModel`, `SetNewItemLabel` and `SetNewItemDial` set the open form's field and
  return the model unchanged without a form. `CancelAddItemForm` sets `addItemFormOpt = None`. None of them writes,
  and none is guarded, because none is a write.
- **No App wiring.** `AppContext.itemModels` is threaded into the real windows by step 0243 (`WIRE_UI`, touches
  `OpticalConstructor.App`). This step's authorized edits do not include `Program.fs`. Until then, a real window's
  model holds `initWith`'s empty in-memory store.
- **Test-local id provider.** `fixedItemIds` answers the fixed GUIDs `23900000-…-0001`, then `…-0002`, in turn. A
  further mint fails the fact.
- **Within the five declared identities.** No other identity was added. Assertions added beyond the slice's minimum:
  - **Fact 1:**
    - the whole model is unchanged apart from the form;
    - every Built-in source, polarizer and sample is offered (sets compared with `BuiltinCatalogue`), so the
      expected list is not built over empty listings;
    - each sample offer pins version 1 of its own sample;
    - a second opening mints the second id;
    - a compound polarizer listed by both kinds is offered once. This catches mutation M02, which the Built-in
      catalogue alone cannot, because it has no polarizer serving both kinds.
  - **Fact 2:**
    - the form messages change nothing without a form;
    - an unchosen form has no draft;
    - Cancel changes nothing but the form;
    - `listSnapshots ()` stays empty.
  - **Fact 3:**
    - `listSnapshots ()` is exactly the circular polarizer's version 1 envelope;
    - a second item sorts by label (ordinal) and becomes the selected one;
    - `SelectKitItem` selects a held id and ignores an unheld one.
  - **Fact 4:**
    - the status starts with `The item was not added — `;
    - nothing but `kitStatus` changes.
  - **Fact 5:**
    - nothing but `kitStatus` changes;
    - the underlying kit holds no item and no snapshot.

## Testing state

No gate verdict is claimed here (Invariant 6). These are development runs over the final tree. The final tree was
rebuilt in full after the mutation check restored the source. The logs are in `specs/0050/.artifacts/`.

- **build** (`dotnet build Berreman.slnx -c Release -nologo -v:m`):
  - The first run failed in the new test file alone, with FS0039 (`VersionNumber` needs
    `open OpticalConstructor.Domain.MaterialLibrary`). It was fixed. That first run's two SYSLIB0051 warnings came
    from the vendored MathNet `Exceptions.cs` (manual 013).
  - The final run, from the restored source: exit 0, `0 Warning(s)`, `0 Error(s)`, in 35 s. Log:
    `0239-build-final.log`; `0239-build.log` holds the run after the fix.
- **constructor-unit-tests**: the gate's command over every test discovered.
  - Result: `Passed!`, succeeded 1371, failed 0, skipped 0, in 70 s (estimate 180 s).
  - That is checkpoint 238's 1366 plus the 5 declared facts. The TRX shows all five ids `Passed`, under exactly the
    declared names.
  - Log: `0239-constructor-unit-tests.log`.
- **ui-smoke**: the gate's command.
  - Result: `Passed!`, succeeded 242, failed 0, skipped 0, in 3 min 29 s (attempt timeout 600 s).
  - 242 equals the baseline. Log: `0239-ui-smoke.log`.
- **ui-tests**: the gate's command with the step's selector, `OpticalConstructor.Ui.Tests.KitWindowTests`.
  - Result: `Passed!`, succeeded 13, failed 0, skipped 0, in 20 s. Log: `0239-ui-tests.log`.
- **elevation** (`ASG_ARC_BASE_COMMIT=f395597c…`, from `.arc-base-json`; ELEV004 excluded):
  - Result: exit 0, `ELEV-GATE: OK - 0 new finding(s) in 12 project(s), 206s`. Known findings: 0.
  - Log: `0239-elevation.log`.
- **The fourteen s24 invariant greps:** none finds anything.
  - `git grep` skips untracked files, so the new test file was also grepped directly against every pattern. It
    matches none.
  - Log: `0239-invariants.log`.
- **The facts bite.** `0239-mutations/run-mutations.py` applied each mutation to `TableAndElementRotationView.fs`,
  built the test project and ran `ItemsPanelModelTests`:

  | Mutation | Fact(s) that went red |
  |---|---|
  | M01: polarizer offers before source offers | fact 1 |
  | M02: no de-duplication of the two polarizer listings | fact 1 (survived the first run; caught after fact 1's compound-polarizer extension) |
  | M03: `AddItem` skips the read-only guard | fact 5 |
  | M04: `AddItem` does not call `recordItem` | facts 3, 4 |
  | M05: a blank label still yields a draft | fact 2 |
  | M06: `SelectKitItem` selects any id | fact 3 |
  | M07: the form's id is a fresh GUID, not the provider's | facts 1, 2, 3 |
  | M08: `AddItem` bypasses `ItemDraft.addTo` | fact 4 |
  | M09: Cancel keeps the form | fact 2 |
  | M10: `initMain` does not thread the Built-in item models | facts 1, 2 |

  The script then restored the source, and `filecmp` reports it identical to the saved original. The solution was
  then rebuilt and every suite re-run, as recorded above. Summary: `0239-mutations/summary.txt`.

No run came near a deadline. No test was removed or renamed, and no assertion was weakened.

## Artifacts

- `specs/0050/.artifacts/0239-build.log`, `0239-build-final.log`
- `specs/0050/.artifacts/0239-constructor-unit-tests.log`
- `specs/0050/.artifacts/0239-ui-smoke.log`
- `specs/0050/.artifacts/0239-ui-tests.log`
- `specs/0050/.artifacts/0239-elevation.log`
- `specs/0050/.artifacts/0239-invariants.log`
- `specs/0050/.artifacts/0239-mutations/`:
  - the driver;
  - the saved original;
  - the per-mutation build and test logs;
  - `driver-output.txt` and `summary.txt`.

## Gotchas

- **A failed item-model listing opens an empty form.**
  - The slice says that an `Error e` of the item-model store "sets `kitStatus` … and the other offers stay". It also
    says that the binding models MUST come from that same list and must never be manufactured.
  - With the listing failed, there is no source, polarizer or stack model, so no preset or sample can be offered.
    The form opens under a minted id with the status and no offers.
  - "The other offers stay" is honoured in the sense that nothing extra is dropped. Each library listing still
    fails only its own kind.
- **"The eight `BuiltinCatalogue.itemModels ()`".** The Built-in catalogue ships eleven item models: eight standalone
  forms and three binding forms. The eight are the standalone ones, the only ones a `LibraryModelChoice` can carry,
  since `ItemDraft.addTo` refuses a binding form there. Fact 1 filters `bindingKindOpt = None` and asserts that there
  are eight.
- **No Built-in polarizer serves both kinds.** The "one per entry id" rule is therefore invisible over the Built-in
  stores alone (mutation M02 first survived). Fact 1 now opens the form over a library that lists one compound
  (`LpCpCategory`) polarizer under both kinds, and checks that it is offered once.
- **The real windows offer no item models until step 0243.** `initWith` holds an empty in-memory item-model store,
  and `ConstructorScene.withAppContext` (`OpticalConstructor.App/Program.fs`) does not yet thread
  `AppContext.itemModels`. That is step 0243's `WIRE_UI`, outside this step's authorized edits.
- **The test provider is stateful.** `fixedItemIds` answers its ids in turn, so every `OpenAddItemForm` takes the
  next one. A fact that opens the form twice must fill the form it opened (`fillForm`), not open a new one. The
  first draft of fact 2 did the latter and saw the second id.
