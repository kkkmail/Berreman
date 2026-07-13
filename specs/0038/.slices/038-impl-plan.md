# Step 038 — IMPLEMENT — impl-plan

## Slice

Wire the inert last-folder setting through the measured-data file picker. Re-type
`EnvironmentSettings.lastFolders` from `string list` to `Map<string, string>` keyed by picker
purpose (one key today: the measured-data picker). The picker STARTS at the persisted folder via
`FilePickerOpenOptions.SuggestedStartLocation` (resolved through the window's `StorageProvider`) and
UPDATES the stored folder ONLY on a confirmed selection, persisting through `UserEnvironment.save`;
cancel changes nothing. The option-building logic is a pure function, unit-tested without a real picker.

`touches: [OpticalConstructor.Ui, OpticalConstructor.Ui.Tests]`; `depends_on: [37]`.

## Approach

1. **`UserEnvironment.fs`** — re-type `lastFolders : string list` → `Map<string, string>`; `defaults`
   `lastFolders = []` → `Map.empty`. No migration: an old array-shaped file already fails schema
   validation and the total `load` falls back to `defaults` (empty map). Add pure helpers:
   - `[<Literal>] measuredDataFolderKey = "measured-data"` (the only purpose key today).
   - `lastFolder purpose settings : string option` — the persisted folder (the option-building input).
   - `rememberFolder purpose selectedFile settings : EnvironmentSettings` — record the file's
     directory under the purpose key (the confirmed-selection transition).
   - `type DataFilePick = FilePicked of string | PickCancelled` + `applyPick purpose pick settings` —
     model the pick outcome as data (the "IO as data" discipline) so confirmed-vs-cancel folds purely.

2. **`optical-constructor-environment.schema.json`** — `lastFolders` from
   `{ "type": "array", "items": { "type": "string" } }` to
   `{ "type": "object", "additionalProperties": { "type": "string" } }` (a `Map<string,string>` is a
   JSON object under FSharp.SystemTextJson; verified empirically). Still `required`.

3. **`TableAndElementRotationView.fs`** —
   - Add pure `buildDataFilePickerOptions (suggested : IStorageFolder option) : FilePickerOpenOptions`
     (title / single-select / `SuggestedStartLocation` from the resolved folder).
   - Rewrite `pickDataFile` (IO edge) to load the environment, resolve the persisted folder to an
     `IStorageFolder` via `StorageProvider.TryGetFolderFromPathAsync`, build options through the pure
     function, and — on a confirmed selection — fold through `applyPick` and `save` (only when the
     folder actually changed, so cancel writes nothing). Still `try/with`-wrapped for a provider-less
     headless host.

4. **`EnvironmentRoundTripTests.fs`** (in `OpticalConstructor.Tests`, `constructor-unit-tests` gate) —
   forced collateral of the re-type: update the four `lastFolders` list literals / `Assert.Equal<string
   list>` sites to `Map<string,string>`. No fact added/removed (count unchanged).

5. **`DataFilePickerFolderTests.fs`** (new, in `OpticalConstructor.Ui.Tests`, `ui-tests` gate) — pure
   tests: fresh env has no persisted folder; a confirmed selection records the file's folder; a cancel
   leaves it untouched; the next open reads exactly the persisted folder; a re-pick replaces it; and
   `buildDataFilePickerOptions None` builds the base options (title, single-select, no start location).
   Register the file in the Ui.Tests fsproj.

## Files to modify

- `Berreman/OpticalConstructor/OpticalConstructor.Ui/UserEnvironment.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/optical-constructor-environment.schema.json`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/TableAndElementRotationView.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/EnvironmentRoundTripTests.fs` (forced by re-type)
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/DataFilePickerFolderTests.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/OpticalConstructor.Ui.Tests.fsproj` (register)

## Risks / decisions

- **`touches` lists only Ui + Ui.Tests, but the re-type forces `EnvironmentRoundTripTests.fs`
  (OpticalConstructor.Tests) to compile** — unavoidable collateral of changing a shared public field
  type. Minimal in-place edits only; recorded in Gotchas.
- **The Some-branch of `buildDataFilePickerOptions` (setting `SuggestedStartLocation` from a real
  `IStorageFolder`) is left to the untested IO edge** — the headless host has no `StorageProvider`, so
  constructing a real `IStorageFolder` in a pure test is impractical; the tested pure surface proves the
  persisted folder is what the next open reads (`lastFolder`) and the base options shape
  (`buildDataFilePickerOptions None`). Faithful to "unit-tested purely".
- **System-prompt path drift** (carried from steps 035–037): the task file points at
  `C:\GitHub\AI-Strategy-Generator\implement_worker.system-md` (absent); the real prompts are under
  `.../src/ai_strategy_generator/multistep/`. Read there.
