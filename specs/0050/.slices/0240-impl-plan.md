# Step 0240 — impl plan (IMPLEMENT: the item edit messages)

Obligation `S72-ITEM-EDIT-MESSAGES`. The operator note is empty.

## Approach

The work is all in `OpticalConstructor.Ui/TableAndElementRotationView.fs`, plus six new facts in
`OpticalConstructor.Tests/ItemsPanelModelTests.fs`. Both files are authorized edits.

1. **Before the model.** Add:
   - `ParameterStateChoice` (`KnownStateChoice` | `UnknownStateChoice`);
   - `ParameterEditField` (`ValueEditField` | `LowerEditField` | `UpperEditField` | `PriorSigmaEditField`), with a
     `.text` render member giving the field's name: `value`, `lower`, `upper` or `prior sigma`;
   - the `ParameterEditDraft` record, with `ofParameter`, `withFieldText` and `tryState`.

   `tryState` reads the texts with `Double.TryParse(text, NumberStyles.Float, CultureInfo.InvariantCulture)`. It does
   not validate: `Parameter.validate` stays the one rule set, and `withParameterState` applies it.
2. **The model.** Add `parameterEditOpt` after `addItemFormOpt`. `initWith` sets it to `None`, and so does a
   successful `SelectKitItem`.
3. **The messages.** Add nine cases after `AddItem`, each with a one-line doc.
4. **The arms.**
   - The editor messages change only `parameterEditOpt`.
   - The five item writes go through one private helper, `recordKitItemEdit`:
     1. step 185's guard;
     2. the held-id lookup;
     3. the Domain edit;
     4. the recording. A changed label is recorded through `recordRelabelledItem`, any other edit through
        `recordItem`.
     5. On success, the held items are replaced in label order and `kitStatus` is cleared.

     Each refusal is told by a private `KitItemEditError` (an item rule, or the photo attach). Its `caption` holds
     the slice's two texts, so each text is written once.
   - `SetParameterState` closes the editor once its item is recorded, through the helper's `onRecorded`
     continuation.
   - Step 239's inline label-order sort in `AddItem` moves into a model member, `kitItemsWith`, which both arms
     share. That way the sort is not copied.

## Tests

Add six facts to `ItemsPanelModelTests`, under exactly the declared ids. They run over the Built-in Main scene with
CPL1 added through `AddItem`.

A new fixture helper, `withKitScene`, reuses `withScene` but swaps in a kit over an `InMemoryKitFolder` the fact
holds, so the facts can read the kit's `paths`. That kit has the same kit file, access and clock, and its
`experimentData` is file-backed over that kit.

The photo file is written under `AppContext.BaseDirectory/ItemsPanelModelTests/<guid>/CPL1.jpg`, and the fact
deletes it at the end.

## Risks

- **ELEV.** Each new type and case name is unique, and the helper's signature carries no primitive. The analyzers
  run over the Ui project and its dependents.
- **"The six write messages".** Only five of the new messages write: `SetParameterState`, `RelabelKitItem`,
  `SetKitItemNotes`, `SetKitItemPhoto` and `RemoveKitItemPhoto`. The read-only fact also sends step 239's `AddItem`
  as the sixth Items-panel write. I record this reading in the impl-log.
- **`ui-smoke` runtime** (about 3.5 min against a 600 s timeout). I add no view, so the risk is low.
