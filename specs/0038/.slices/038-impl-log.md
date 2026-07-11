# Step 038 — IMPLEMENT — impl-log

## Progress

- [x] Re-type `EnvironmentSettings.lastFolders` → `Map<string, string>` + pure helpers (UserEnvironment.fs)
- [x] Env schema: `lastFolders` array → object-of-strings
- [x] `buildDataFilePickerOptions` + `pickDataFile` wired to persisted folder (TableAndElementRotationView.fs)
- [x] Fix `EnvironmentRoundTripTests.fs` for the re-type
- [x] Add `DataFilePickerFolderTests.fs` + register in Ui.Tests fsproj
- [x] Local build (advisory) green; affected suites advisory-green

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Ui/UserEnvironment.fs`
  - `EnvironmentSettings.lastFolders : string list` → `Map<string, string>` (purpose-keyed) with a new
    doc comment; `defaults.lastFolders = []` → `Map.empty`.
  - Added the pure picker-folder module surface after `save`: `[<Literal>] measuredDataFolderKey`,
    `lastFolder`, `rememberFolder`, the `DataFilePick` DU (`FilePicked | PickCancelled`), and `applyPick`.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/optical-constructor-environment.schema.json`
  - `lastFolders` from `array of string` to `object with additionalProperties string` (a
    `Map<string,string>` serializes as a JSON object; empirically verified with FSharp.SystemTextJson
    1.4.36). Still `required`.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/TableAndElementRotationView.fs`
  - Added pure `buildDataFilePickerOptions (suggested : IStorageFolder option) : FilePickerOpenOptions`.
  - Rewrote `pickDataFile` to load the environment, resolve the persisted measured-data folder to an
    `IStorageFolder` via `StorageProviderExtensions.TryGetFolderFromPathAsync`, build options through the
    pure function, and — folding the outcome through `UserEnvironment.applyPick` — persist through
    `UserEnvironment.save` only when a confirmed selection changed the folder (cancel writes nothing).
    Still `try/with`-wrapped for a provider-less headless host.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/EnvironmentRoundTripTests.fs` (forced collateral)
  - Updated the four `lastFolders` list literals / `Assert.Equal<string list>` sites to
    `Map<string,string>` (purpose-keyed). Renamed one test to reflect the by-purpose-key round-trip. No
    fact added/removed.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/DataFilePickerFolderTests.fs` (new) — 7 pure
  facts; registered in `OpticalConstructor.Ui.Tests.fsproj` after `InverseConstructorTests.fs`.

## Testing state

Gate execution is the arc-runner gate engine's job (IMPLEMENT Invariant 6 — the worker acts, runs no
gates). The local runs below are ADVISORY, done for baseline honesty (the prior-worker convention in
this arc); the gate engine re-runs every gate after this session exits and is the sole authority.

- **build** (`dotnet build Berreman.slnx -c Release`): green, 0 errors. 10 warnings, ALL pre-existing
  (NU1701 Wolfram.NETLink, SYSLIB0051 MathNet, FS0044 ChartWindow, FS3873 Dispersion.fs, FS1125
  SeriesDataTests) — none from any file this step touched. Log: `.artifacts/038-local-build.log`.
- **constructor-unit-tests** (OpticalConstructor.Tests): 659 passed / 0 failed — unchanged
  (EnvironmentRoundTripTests edited in place, no fact count change; the re-typed field still
  serialize → validate-on-load → round-trips through the new object schema).
- **ui-tests** (Category!=ui-smoke): 453 passed / 0 failed (446 → +7 DataFilePickerFolderTests).
- **unit-tests** (BerremanTests) and **ui-smoke**: not re-run — the solver and the headless render
  surface are untouched by this step (the picker's folder plumbing is pure and off the render path);
  baselines carried forward (119 / 171).

## Artifacts

- `.artifacts/038-local-build.log` — captured local Release build output (advisory).

## Gotchas

- **`touches` lists only Ui + Ui.Tests, but re-typing the shared public field
  `EnvironmentSettings.lastFolders` forces `EnvironmentRoundTripTests.fs` (in `OpticalConstructor.Tests`,
  the `constructor-unit-tests` gate) to compile.** Only that one non-`touches` file references the
  field; edits are minimal and in-place (no fact count change). Unavoidable collateral of a re-type —
  recorded here per the "record the interpretation" rule.
- **The Some-branch of `buildDataFilePickerOptions` (setting `SuggestedStartLocation` from a real
  `IStorageFolder`) is left to the untested IO edge.** The Avalonia headless host has no
  `StorageProvider` (verified: `Avalonia.Headless` 12.1.0 ships no storage provider), and
  `IStorageFolder` has no public concrete implementation to construct in a pure test. The tested pure
  surface instead proves the two things the slice names: the next open READS the persisted folder
  (`lastFolder`, after `applyPick`) and the base option shape (`buildDataFilePickerOptions None` —
  title, single-select, null start location). Faithful to "the option-building logic … unit-tested
  purely".
- **`Map<string,string>` serializes as a JSON object, not an array-of-pairs.** FSharp.SystemTextJson's
  default map format uses the object form for string keys (the `beamNode.children` DU-keyed map that
  serializes as array-of-pairs is the *non*-string-key case). Confirmed empirically before changing the
  schema, so an old array-shaped `lastFolders` file now fails the `object` type and the total loader
  falls back to `defaults` (empty map) — the no-migration contract, exactly as the slice specifies.
- **The confirmed/cancel decision is modeled as data (`DataFilePick`) and folded through `applyPick`,
  not left as control flow at the IO edge.** Production routes BOTH the confirmed and cancelled picker
  results through `applyPick`, so both branches are load-bearing (cancel → identity → `updated <>
  settings` is false → no save); the pure `applyPick`/`rememberFolder`/`lastFolder` are what the tests
  drive. This keeps the untested IO edge to the genuinely un-testable-headless parts (raising the
  dialog, resolving a real `IStorageFolder`, reading/writing `environment.json`).
- **System-prompt path drift (carried from steps 035–037).** The task file's
  `C:\GitHub\AI-Strategy-Generator\implement_worker.system-md` does not exist; the real prompts live
  under `.../src/ai_strategy_generator/multistep/` (`implement_worker.system-md` + the shared
  `arc-runner.system-md`). Located and read there. No scope impact.

## Changelog

- 2026-07-11 — Step 038 (IMPLEMENT): wired the inert last-folder setting through the measured-data file
  picker. Re-typed `EnvironmentSettings.lastFolders` to `Map<purpose, folder>` (schema + no-migration
  fall-back), added the pure `lastFolder` / `rememberFolder` / `applyPick` (+ `DataFilePick`) surface and
  `buildDataFilePickerOptions`, and rewrote `pickDataFile` to open at the persisted folder
  (`SuggestedStartLocation`) and persist the chosen folder on a confirmed selection only. Added
  `DataFilePickerFolderTests` (7 pure facts). Build green; suites 659 / 453 (constructor / ui-tests).
