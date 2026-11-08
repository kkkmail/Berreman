# Step 0163 — implementation plan

The step carries step 162's `KitHistoryLimit` in the `WorkbenchSettings` record and reads it from
`appsettings.json` through a settings door, beside the other three counts (001 §F, §G; S5.1).

## Approach

1. **Domain** (`OpticalConstructor.Domain/WorkbenchSettings.fs`).
   - The record gains `kitHistoryLimit : KitHistoryLimit` as its last field.
   - `defaults` gains `kitHistoryLimit = KitHistoryLimit.defaultValue`.
   - The record doc lists the limit's 100 among the built-in values written once here.
2. **App** (`OpticalConstructor.App/AppConfig.fs`).
   - `ConfigKey.kitHistoryLimit = "KitHistoryLimit"` follows `thicknessBucketCap`, with a one-line doc.
   - The three copies of the count-door body move into ONE private extension member:
     `member private this.tryPositiveCount` on `AppSettings.ConfigValue`, declared in
     `WorkbenchSettingsDoors`. `QuickPickThreshold`, `TreeAutoBuildThreshold` and
     `ThicknessBucketCap` call it, and their answers and texts stay the same.
   - `type KitHistoryLimit with` gets `tryCreate (v : ConfigValue)` through the same member, plus
     `configValue`.
   - `loadWorkbenchSettings` reads `kitHistoryLimit` through `readSetting`, so a missing or refused
     key is logged and written back as 100. It is saved with the other keys, once.
3. **Census.** Only `defaults` and the two test records build a `WorkbenchSettings` literal. These
   files read the record and need no edit:
   - `AppContext.fs`;
   - the Ui.Tests files the slice lists.

   The `{ … quickPickThreshold = … }` literals in `TableAndElementRotationView.fs` and the
   `LibraryWindowContext` fixtures belong to other records.
4. **Tests.**
   - In `WorkbenchSettingsTests`, the two record-literal facts carry the new field. The assembled
     record uses 250 and asserts it.
   - New fact: `the default settings carry a kit history limit of 100`.
   - In `AppConfigTests`, new fact:
     `the count doors read a positive whole number and refuse anything else, the kit history limit's included`.
     It covers the four doors over `"250"`, `"0"` and `"ten"`.

## Risks

- A private member on an optional extension is private to the enclosing module. Other members in
  that module can call it from their own `type … with` blocks. `KitFolderStore.readKitText` is a
  precedent.
- ELEV: the shared member is declared on `ConfigValue`, and members may speak primitives. It is
  private, so it adds no public mixed signature. The name `tryPositiveCount` is unique, and STL has
  no member of that name on `ConfigValue`.
- Invariant 6: this worker runs no gate. The edits are checked by reading them closely.

## Attempt 02 (elevation retry)

The gate's one finding was ELEV010P on `tryPositiveCount`: `int` and `string` sat beside
`WorkbenchSettingsError` in a private signature. ELEV010 checks private members too. The body
discards the intrinsic's refusal, so its type becomes the generic `'Refusal`, and the signature
then names none of our types. This is a one-line change in `AppConfig.fs`. The doors, the texts,
the tests and the acceptance criteria are unchanged.
