# Step 0319 — impl-plan

## Approach

Step 0319 adds the window's split messages. They identify the groups and confirm nothing: no message attaches or creates
anything, because applying a split is `MatchByTemplate`'s job in step 320 (spec 0050 S9.3; 001 §K, §P `SplitByColumn`).

`OpticalConstructor.Ui/TableAndElementRotationView.fs`:

1. **`DataSplitState`**, declared before the model. It holds `file`, `profile`, `key`, `split` and `creating`.
2. **`dataSplitOpt : DataSplitState option`**, a new model field that `initWith` sets to `None`. It sits beside the Data
   step's fields.
3. **Three new messages**, placed beside `OpenImportWizard`:
   - `SplitByColumn of FileName * ImportProfileName`;
   - `ToggleSplitOffer of DataRowKey`;
   - `CancelDataSplit`.
4. **The `SplitByColumn` handler**, inside `guardKitWrite`, which is step 185's read-only guard.
   - It resolves the profile with `ChosenImportProfile.tryOfName` over the kit file's profiles. If the kit lacks it, the
     status is `not available`.
   - It reads the profile's `splitKeyOpt`. If there is no key column, the status is `maps no key column`.
   - It runs one `Result` chain: `readPickedFile`, then `content.kitFileText`, then `mapText`, then `splitGroups`. A
     `KitError` is rendered with `describe` and an `ExperimentDataError` with its Ui `caption`.
   - It builds `DataSplit.ofGroups` over the slice's `<param>`.
   - It writes the status `Split <leaf> by '<key>': m matched, o offered, u unmatched.`
   - Every refusal changes only `kitStatus`.
5. **`ToggleSplitOffer`**. It changes `creating` only when the key's group is a `SetupOfferForGroup`.
6. **`CancelDataSplit`**. It sets `dataSplitOpt` to `None` and nothing else.

## Tests

- **New `OpticalConstructor.Tests/DataSplitModelTests.fs`.** It is appended to the fsproj's compile list.
  - The fixture is a folder kit under `<BaseDirectory>/DataSplitModelTests/<guid>/kit`, opened read-write.
  - The Phone ROI file is written to `<guid>/phone-roi.csv`, outside the kit.
  - The items and table are test-local copies of `SetupModelTests`' (whose fixture is private, and that file is outside
    this step's edits). The table is LP1, QZ at R1 45° and LPd before the seeded detector, with E1 committed through
    `ExpChooseElement` + `ExpCommit`.
  - Three facts, as the slice names them.
- **`KitWindowModelTests`.** The read-only fact also sends `SplitByColumn`. The loop also asserts that `dataSplitOpt` is
  unchanged.

## Risks

- **Record label clashes.** `key`, `file`, `profile`, `points` and `setup` are labels of other records too, so every
  construction is annotated.
- **ELEV rules.** The new type name and the three DU case names must be unique (ELEV014 and ELEV016). There must be no
  local function over `string` or `bool`, and no call-site re-wrap.
- **Slice text.** The slice says "five groups", but its five rows hold four distinct keys. The listed groups and the
  status (1 + 2 + 1) are binding, so four groups are asserted.
