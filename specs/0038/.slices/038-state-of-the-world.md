# Step 038 — State of the world

## Where we are

Spec 0038 Part L (the inverse-problem FLOW, wiring only — no solver). Step 038 (`IMPLEMENT`,
`depends_on: [37]`, `touches: [OpticalConstructor.Ui, OpticalConstructor.Ui.Tests]`) wires the
previously **inert** last-folder setting through the measured-data file picker built in step 037. It
re-types `EnvironmentSettings.lastFolders` from `string list` to a purpose-keyed
`Map<string, string>`, makes the collection builder's "Attach data file…" picker START at the
persisted folder and RECORD the chosen folder on a confirmed selection, and extracts the
option-building + folder logic as pure, unit-tested functions. Builds directly on step 037's
`pickDataFile` seam and the step-002 `UserEnvironment` persistence spine.

## What's working

- Re-type `EnvironmentSettings.lastFolders` from `string list` to `Map<string, string>` keyed by picker
  purpose (one key today, `measuredDataFolderKey`); `defaults` seeds an empty map. No migration — an old
  array-shaped file fails schema validation and the total `load` falls back to defaults.
- Update the env schema: `lastFolders` becomes a JSON object of string values (a `Map<string,string>`).
- Add pure helpers on `UserEnvironment`: `lastFolder` (the option-building input), `rememberFolder` (the
  confirmed-selection transition), and `applyPick` over a `DataFilePick` DU (confirmed vs cancel folds
  purely).
- Add pure `buildDataFilePickerOptions` and rewrite `pickDataFile` to open at the persisted folder via
  `SuggestedStartLocation` and persist the chosen folder through `UserEnvironment.save` on a confirmed
  selection only — cancel changes nothing.
- Add `DataFilePickerFolderTests` (7 pure facts) covering the next-open start folder, confirmed update,
  cancel no-op, re-pick replacement, and the base picker-option shape.

## Tests

Gate execution is the arc-runner gate engine's job (IMPLEMENT Invariant 6 — the worker acts, runs no
gates). The runs below are ADVISORY, done for baseline honesty; the gate engine re-runs every gate
after this session exits and is the sole authority. Roster: `build`, `unit-tests`,
`constructor-unit-tests`, `ui-smoke`, `ui-tests`.

- **build**: green (0 errors). 10 warnings, ALL pre-existing (NU1701 / SYSLIB0051 / FS0044 / FS3873 /
  FS1125) — none from any file this step touched.
- **unit-tests** (BerremanTests): unchanged at 119 — the solver is untouched (not re-run).
- **constructor-unit-tests** (OpticalConstructor.Tests): 659 passed / 0 failed — unchanged. The
  re-typed field still serialize → validate-on-load → round-trips (the `EnvironmentRoundTripTests` were
  edited in place, no fact count change).
- **ui-smoke**: unchanged at 171 — no ui-smoke test added; the picker's folder plumbing is pure and off
  the headless render path (not re-run).
- **ui-tests** (Category!=ui-smoke): 453 passed / 0 failed (446 → +7 DataFilePickerFolderTests).

```yaml
gates:
  berreman_unit_tests:    119
  constructor_unit_tests: 659
  ui_smoke_tests:         171
  ui_tests:               453
```

## Architecture

- **`lastFolders` is a `Map<purpose, folder>`, not a `string list`.** Keying by picker purpose lets each
  picker resume from where it left off and lets a NEW picker add a key rather than a parallel field. One
  key ships today (`measuredDataFolderKey = "measured-data"`), centralized as a `[<Literal>]` so the id
  is never scattered.
- **No migration, by contract.** A `Map<string,string>` serializes as a JSON object; the schema now
  types `lastFolders` as `object`/`additionalProperties: string`. An old array-shaped file fails the
  object type, and the total `load` falls back to `defaults` (empty map) — the §J.6-item-3 no-migration
  fall-back, reused verbatim (no bespoke migration code).
- **The folder logic is pure and the picker is the thin IO edge.** `lastFolder` / `rememberFolder` /
  `applyPick` (over the `DataFilePick` DU) hold the option-building input and the confirmed-vs-cancel
  persistence decision as pure functions; `buildDataFilePickerOptions` builds the Avalonia
  `FilePickerOpenOptions` from a resolved `IStorageFolder option`. `pickDataFile` only does the genuinely
  IO-bound work: load `environment.json`, resolve the persisted path to an `IStorageFolder` through the
  window's `StorageProvider`, raise the dialog, and `save` — all `try/with`-wrapped for a provider-less
  headless host. This keeps the tested surface pure (no real picker) while the IO edge stays a no-op
  under test, exactly the step-037 seam discipline.
- **Confirmed vs cancel is modeled as data.** The picker result becomes a `DataFilePick`
  (`FilePicked | PickCancelled`); production folds BOTH cases through `applyPick`, and persists only when
  the folder actually changed (`updated <> settings`), so cancel writes nothing and a re-pick of the same
  folder writes nothing. Both branches are load-bearing, not test-only.

## Deferred

- **Persisting a folder for OTHER pickers** (project open/save, export, …) — the map is keyed by purpose
  for exactly this, but only the measured-data key is wired now; other pickers keep their current
  behaviour until they are given a key.
- **Re-resolving `SuggestedStartLocation` when the persisted folder no longer exists** — the provider's
  `TryGetFolderFromPathAsync` already returns null (→ `None` → host default) for a missing folder, so no
  extra handling is needed; no stored-folder pruning is attempted.
- Everything step 037 already deferred remains deferred: the `Inverse` launcher button + whole-surface
  composition acceptance (steps 45 / 47), the `SolverHandoffWindow`, and re-validating a loaded
  collection's attached files on load.

## Gotchas

- **`touches` excludes `OpticalConstructor.Tests`, but the re-type forces `EnvironmentRoundTripTests.fs`
  to compile.** That is the only non-`touches` file referencing `lastFolders`; edits are minimal,
  in-place, and change no fact count. Unavoidable collateral of re-typing a shared public field —
  recorded per the "record the interpretation" rule.
- **The `Some`-branch of `buildDataFilePickerOptions` is proven only at the IO edge, not by a pure
  test.** Avalonia headless ships no `StorageProvider` and `IStorageFolder` has no public concrete
  implementation, so a pure test can't hand it a real folder. The tested pure surface instead proves the
  slice's two named claims — the next open READS the persisted folder (`lastFolder`) and the base option
  shape (`buildDataFilePickerOptions None`). The one-line `SuggestedStartLocation <- folder` assignment
  is left to the untestable-headless edge.
- **`Map<string,string>` → JSON object was verified before the schema change** (FSharp.SystemTextJson
  1.4.36 default map format uses the object form for string keys; the DU-keyed `beamNode.children` map
  that serializes as array-of-pairs is the non-string-key case). This is what makes the no-migration
  fall-back correct: an old array-shaped file no longer matches the `object` type.
- **System-prompt path drift (carried from steps 035–037).** The task file's
  `C:\GitHub\AI-Strategy-Generator\implement_worker.system-md` is absent; the real prompts live under
  `.../src/ai_strategy_generator/multistep/`. Located and read there. No scope impact.

## Changelog

- 2026-07-11 — Step 038 (IMPLEMENT): wired the inert last-folder setting through the measured-data file
  picker. Re-typed `EnvironmentSettings.lastFolders` to a purpose-keyed `Map<string, string>` (schema +
  no-migration fall-back), added the pure `lastFolder` / `rememberFolder` / `applyPick` (+ `DataFilePick`)
  surface and `buildDataFilePickerOptions`, and rewrote `pickDataFile` to open at the persisted folder
  (`SuggestedStartLocation`) and persist the chosen folder on a confirmed selection only. Added
  `DataFilePickerFolderTests` (7 pure facts). Build green; suites 659 / 453 (constructor / ui-tests),
  119 / 171 carried forward.
