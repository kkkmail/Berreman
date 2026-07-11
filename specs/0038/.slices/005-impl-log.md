# Step 005 — impl-log (attempt 1)

## Progress

- [x] Read task file, worker system prompt (`implement_worker.system-md` + shared
      base), project prompt, slice spec; surveyed prior-round outputs (004),
      the Softellect.Sys 10.1.301.54 API (packaged DLL metadata + local clone),
      App/Domain/Tests fsprojs, Program.fs, Domain elevation precedents.
- [x] Domain: `WorkbenchSettings.fs` (elevated settings record + tryCreate).
- [x] App: `AppConfig.fs` (ConfigKey inventory + the one provider read seam),
      `appsettings.json` content item, fsproj wiring, Startup binding.
- [x] Tests: `WorkbenchSettingsTests.fs` (defaults + rejection, no disk; 22 cases).
- [x] Diagnostic build + all four suites (non-gate verification) — all green,
      constructor tests 479 (+22) over the step-004 checkpoint; write-back proof
      observed in the Ui.Tests output copy; LF check clean.
- [x] State-of-the-world + exit summary.

## Files modified

New:

- `OpticalConstructor.Domain/WorkbenchSettings.fs` — `module WorkbenchSettings`:
  `WorkbenchSettingsError` (three payload-carrying non-positive rejections),
  `SelectWindowModality = ModalSelectWindows | ModelessSelectWindows` (never a
  naked bool; `.value` = wire bool true-is-modal, total `create`, default
  Modeless), `QuickPickThreshold` (5) / `TreeAutoBuildThreshold` (100) /
  `ThicknessBucketCap` (8) as single-case DUs with `.value` + `defaultValue` +
  `tryCreate` (non-positive → typed Error), and the `WorkbenchSettings` record
  (all fields elevated) with `static member defaults` — the single home of the
  built-in values.
- `OpticalConstructor.App/AppConfig.fs` — the `[<RequireQualifiedAccess>]
  ConfigKey` literal inventory (camelCase literal names per the repo's UiIds
  precedent; values are the wire key names): `SelectWindowsModal`,
  `QuickPickThreshold`, `TreeAutoBuildThreshold`, `ThicknessBucketCap`, plus the
  step-040 keys `DatabaseProvider` (appSettings; "MSSQL" | "SQLite") and the
  connectionStrings entries `OpticalConstructorMssql` /
  `OpticalConstructorSqlite`. Then `module AppConfig` with
  `loadWorkbenchSettings : unit -> WorkbenchSettings` — the ONE
  `AppSettingsProvider.tryCreate ()` call site: reads the four keys via
  `getBoolOrDefault`/`getIntOrDefault` (SetOnMissing writes missing defaults
  into the JObject), `trySave ()` persists the write-backs, values elevate
  per-field (invalid stored value → logged + per-field Domain default); a
  missing/unreadable file logs the typed `FileError` and returns
  `WorkbenchSettings.defaults` (total — startup never blocks).
- `OpticalConstructor.App/appsettings.json` — the shipped skeleton
  (`appSettings` / `connectionStrings` empty sections); first run
  self-documents by write-back.
- `OpticalConstructor.Tests/WorkbenchSettingsTests.fs` — 22 cases: defaults
  record (modeless / 5 / 100 / 8), defaults ≡ per-type-default construction,
  modality wire round-trip, and per-threshold accept (positive `.value`
  round-trip) / reject (0, −1, −100 → the typed error carrying the offending
  value) theories, plus an assembled-record proof. Pure construction — no
  provider, no disk.

Edited:

- `OpticalConstructor.App/OpticalConstructor.App.fsproj` — `Softellect.Sys
  10.1.301.54` package reference (version-matched to Storage's existing one);
  `AppConfig.fs` compiled before Program.fs; `appsettings.json` as
  `Content` / `PreserveNewest`.
- `OpticalConstructor.App/Program.fs` — `Startup.workbenchSettings =
  AppConfig.loadWorkbenchSettings ()` (created once with the Startup module's
  bindings, forced when `App.Initialize` reads `Startup.settings`; consumers
  arrive with the Part C window-policy seam).
- `OpticalConstructor.Domain/OpticalConstructor.Domain.fsproj` —
  `WorkbenchSettings.fs` compile entry after `Units.fs` (depends on nothing).
- `OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj` —
  `WorkbenchSettingsTests.fs` compile entry at the end.

## Testing state

Gate execution belongs to the arc-runner's gate engine after this worker exits
(Invariant 6 — the worker acts, it runs no checks). Diagnostic verification only,
not gate authority:

- `dotnet build Berreman.slnx -c Release` — **Build succeeded, 0 errors**;
  **no MSB3277 anywhere in the log** (swept explicitly — the heavy
  Softellect.Sys dependency set joins the App graph cleanly); zero warnings
  from any touched/new file. The 10 warning lines are exactly the
  step-001-catalogued pre-existing set in untouched files (FS1125
  SeriesDataTests ×4, FS3873 Dispersion.fs, FS0044 ChartWindow.fs, SYSLIB0051
  vendored MathNet ×2, NU1701 Wolfram.NETLink ×2).
- `OpticalConstructor.Tests`: **479/479 passed** (checkpoint 457; +22
  WorkbenchSettings cases).
- `BerremanTests` (`--no-build`): **119 passed, 5 skipped** (the pre-existing
  skips; checkpoint 119; untouched).
- `OpticalConstructor.Ui.Tests --filter Category=ui-smoke`: **113/113 passed**
  (checkpoint 113) — the headless product-App smoke path now RUNS the loader
  (Startup forces it) and stays green.
- `OpticalConstructor.Ui.Tests --filter Category!=ui-smoke`: **348/348 passed**
  (checkpoint 348).
- **Acceptance write-back proof observed**: after the ui-smoke run (which
  drives the product `App.Initialize` headlessly), the `appsettings.json`
  beside the Ui.Tests assembly contains all four defaults written back
  (`SelectWindowsModal=False, QuickPickThreshold=5, TreeAutoBuildThreshold=100,
  ThicknessBucketCap=8`), while the shipped App-output copy remains the
  untouched skeleton (the app itself was not run).
- Line endings: `git diff --numstat` equals `--ignore-cr-at-eol --numstat` for
  every touched product file; all four new files scanned — 0 CR bytes, no BOM.
  (The one CRLF warning is the arc-runner's own `.manifest.state.json`, already
  modified before this round.)

## Artifacts

- `specs/0038/.artifacts/005-diag-build.log` — diagnostic Release build log
  (MSB3277 / warning sweep ran against this capture).
- `specs/0038/.artifacts/005-diag-constructor-tests.log` — constructor suite
  run (479 passed).
- `specs/0038/.artifacts/005-diag-unit-tests.log` — BerremanTests run (119
  passed, 5 pre-existing skips).
- `specs/0038/.artifacts/005-diag-ui-smoke.log` — ui-smoke suite run (113
  passed).
- `specs/0038/.artifacts/005-diag-ui-tests.log` — ui-tests suite run (348
  passed).

## Gotchas

- No operator note in flight (the project prompt's Operator note section is empty).
- **The packaged 10.1.301.54 API was verified against the DLL, not just the
  clone**: `C:\GitHub\Softellect` is ahead of the published package, so
  `AppSettingsProvider`'s method table was enumerated from the NuGet-cache
  DLL's metadata before coding — all called members (`tryCreate` ×4 overloads,
  `getBoolOrDefault`, `getIntOrDefault`, `trySave`, `tryGetConnectionString`)
  exist in the package.
- **SetOnMissing alone does NOT persist** — it mutates the in-memory `JObject`
  only; the loader must call `trySave ()` for the first run to self-document.
  Missed, the acceptance silently fails while everything still builds.
- **Step-040 key names are this round's recorded choice** (the spec names the
  concept, not the strings): `DatabaseProvider` (appSettings; "MSSQL" |
  "SQLite") and connectionStrings entries `OpticalConstructorMssql` /
  `OpticalConstructorSqlite`. Step 040 should consume these `ConfigKey`
  literals and ship its own appsettings.json content item for its test
  project — the App's file stays free of DB values the app never reads.
- **Only the four workbench defaults write back on first run** (per the
  slice); the DB keys are inventory constants, not app reads — the App's
  provider never touches `DatabaseProvider`/connectionStrings.
- **`ConfigKey` name collision avoided by scoping, not renaming**: the file
  opens `Softellect.Sys` (NOT `Softellect.Sys.AppSettings`), so the
  unqualified `ConfigKey` can only mean the new literals module, and the
  provider's DU is constructed as `AppSettings.ConfigKey <literal>` (qualified
  module access resolves to the union case — the same collision
  `MaterialLibrary.tryMaterialId` documents).
- **`appsettings.json` resolves beside `Softellect.Sys.dll`**
  (`getAssemblyLocation` uses `GetExecutingAssembly`), i.e. the consuming
  app's output folder — which is why the content item flows transitively into
  Ui.Tests' output and the headless smoke run finds (and write-backs) its own
  copy. If the file is ever absent (fresh output layout), the loader's
  fallback returns `WorkbenchSettings.defaults` — total either way, no prompt,
  no sleep.
- **The bool wire form serializes as the string "False"/"True"**
  (`JValue($"{value}")` → .NET bool formatting); `Boolean.Parse` accepts it
  round-trip. Hand-editors writing lowercase `false` also parse fine.
