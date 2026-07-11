# Step 005 — state of the world

## Where we are

Step 005 opens spec 0038 Part C (composition & window policy) by giving the product
app its configuration seam: `appsettings.json` now exists as a build-copied content
item of `OpticalConstructor.App`, read/written ONLY through
`Softellect.Sys.AppSettings.AppSettingsProvider` (10.1.301.54 — the version Storage
already references), created ONCE at the composition root. The key inventory lives in
one `[<RequireQualifiedAccess>] ConfigKey` module (the four workbench keys plus the
step-040 DB provider / connectionStrings names), and typed values flow inward as the
new pure `WorkbenchSettings` Domain record of elevated types — no view or Domain
module reads the provider ambiently, and user preferences remain in
`environment.json` (the two stores never merge). Later Part C/E/F steps (window
policy, quick-pick, faceted tree, bucketing) consume the record's fields; step 040
consumes the DB `ConfigKey` entries.

## What's working

- Add the appsettings.json seam: AppSettingsProvider created once at the
  composition root (AppConfig.loadWorkbenchSettings), SetOnMissing + trySave
  writing the four defaults back on first run (proven in the headless run).
- Add the ConfigKey literal inventory: SelectWindowsModal / QuickPickThreshold /
  TreeAutoBuildThreshold / ThicknessBucketCap plus the step-040 DatabaseProvider
  and OpticalConstructorMssql / OpticalConstructorSqlite entries.
- Add the elevated Domain record WorkbenchSettings: SelectWindowModality (never
  a naked bool), three single-case-DU thresholds with .value + tryCreate
  rejecting non-positive values, and a defaults member as the single source of
  the built-in values (modeless / 5 / 100 / 8).
- Pin the defaults and every tryCreate rejection in 22 new
  OpticalConstructor.Tests cases with no file IO; suites 479 / 119 / 113 / 348.

## Tests

- Gates are executed by the arc-runner's deterministic gate engine after this worker
  exits (Invariant 6 — the worker acts, it runs no checks). The roster for this step
  is `build`, `unit-tests`, `constructor-unit-tests`, `ui-smoke`, `ui-tests`.
- Diagnostic verification (not gate authority): `dotnet build Berreman.slnx -c
  Release` succeeded with 0 errors, **no MSB3277** (swept — the Softellect.Sys
  dependency set joins the App graph cleanly), and zero warnings from any touched/new
  file — the only warnings are the step-001-catalogued pre-existing set in untouched
  files (FS1125 SeriesDataTests, FS3873 Dispersion, FS0044 ChartWindow, SYSLIB0051
  vendored MathNet, NU1701 Wolfram.NETLink). Suites: OpticalConstructor.Tests
  **479/479** (checkpoint 457, +22), BerremanTests **119 passed / 5 pre-existing
  skips**, ui-smoke **113/113**, ui-tests **348/348**.
- Acceptance write-back proof: after the ui-smoke run (the headless product
  `App.Initialize`), the Ui.Tests output copy of appsettings.json carries all four
  defaults written back; the shipped skeleton stays untouched until the app runs.
- Nothing deferred.

## Architecture

- **One provider, one read site, typed inward flow**: the ONLY
  `AppSettingsProvider` call site is `AppConfig.loadWorkbenchSettings` in the App
  composition root; everything downstream sees the elevated `WorkbenchSettings`
  record. The loader is total — a missing/unreadable file logs the typed
  `FileError` and falls back to `WorkbenchSettings.defaults`, so startup (and the
  headless ui-smoke session) never blocks on configuration.
- **Defaults live in Domain, once**: each elevated type carries its
  `defaultValue`; the App passes `.defaultValue.value` as the provider default, so
  modeless / 5 / 100 / 8 appear exactly once, in `WorkbenchSettings.fs`.
- **Two config stores, disjoint purposes**: appsettings.json = app configuration
  (this step); environment.json = per-user preferences (theme, layout — §J.6).
  They never merge; nothing in this step touches `UserEnvironment`.
- **The step-040 contract is names-only here**: `ConfigKey.databaseProvider`
  ("MSSQL" | "SQLite") and the two connectionStrings entries exist as literals;
  the App neither reads nor seeds DB values — step 040's test project ships its
  own appsettings.json content item and reads via `tryGetConnectionString`.

## Deferred

- Consumers of `WorkbenchSettings` fields arrive with later steps: the
  window-policy seam / `SelectWindowModality` (Part C), `QuickPickThreshold`
  (§9.5), `TreeAutoBuildThreshold` (§7.6), `ThicknessBucketCap` (§7.5). Until
  then `Startup.workbenchSettings` is bound but unconsumed by design.
- Step 040 consumes the DB `ConfigKey` entries (names recorded in this round's
  impl-log Gotchas).
- The pre-existing warnings in untouched files remain for spec 0038 Part N's
  sweep (carried from steps 002–004).

## Gotchas

- **The local Softellect clone is ahead of the published package** — the
  10.1.301.54 API surface was verified by enumerating `AppSettingsProvider`'s
  method table from the NuGet-cache DLL metadata, not by trusting
  `C:\GitHub\Softellect`. All called members exist in the package.
- **SetOnMissing alone does not persist**: it mutates the in-memory `JObject`;
  the explicit `trySave ()` in the loader is what makes the first run
  self-document. Do not remove it when refactoring the loader.
- **`ConfigKey` collides with Softellect's `ConfigKey` DU by design**: the App
  file opens `Softellect.Sys` (never `Softellect.Sys.AppSettings`), so the
  unqualified name is the literals module and the DU is reached as
  `AppSettings.ConfigKey <literal>` (qualified module access resolves to the
  union case — the `MaterialLibrary.tryMaterialId` collision precedent).
- **appsettings.json resolves beside the assembly** (`getAssemblyLocation`),
  and the content item flows transitively into referencing test projects —
  which is why the headless smoke run finds and write-backs its OWN output
  copy. That bin-folder mutation is expected and harmless (PreserveNewest never
  copies the older source over it).
- **The wire bool serializes as "False"/"True"** (string-typed store);
  `Boolean.Parse` round-trips it and accepts hand-edited lowercase forms.
- Step 002–004 carried-over gotchas remain valid (baselines come from
  `.checkpoints-json`, not the SoW YAML; the `App` type-name ambiguity guard
  between the two composition roots).

## Changelog

- 2026-07-10 — Step 005 (IMPLEMENT, attempt 1): introduced the appsettings.json
  seam — Softellect.Sys 10.1.301.54 AppSettingsProvider created once at the App
  composition root, the ConfigKey literal inventory (four workbench keys + the
  step-040 DatabaseProvider / OpticalConstructorMssql / OpticalConstructorSqlite
  entries), the build-copied appsettings.json skeleton with the four defaults
  written back on first run, and the elevated WorkbenchSettings Domain record
  (SelectWindowModality, QuickPickThreshold 5, TreeAutoBuildThreshold 100,
  ThicknessBucketCap 8) with tryCreate validation; 22 new no-IO unit tests.
  Build clean (no MSB3277); suites 479 / 119 / 113 / 348.

```yaml
gates:
  berreman_unit_tests: 119
  constructor_unit_tests: 479
  ui_smoke_tests: 113
  ui_tests: 348
```
