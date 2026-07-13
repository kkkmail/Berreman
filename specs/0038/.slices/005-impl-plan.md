# Step 005 — impl-plan (attempt 1)

## What the slice wants

Introduce `appsettings.json` for the product app, read/written ONLY through
`Softellect.Sys.AppSettings.AppSettingsProvider` (10.1.301.54 — the version
`OpticalConstructor.Storage` already references), created ONCE at the
composition root via `AppSettingsProvider.tryCreate ()`. One
`[<RequireQualifiedAccess>] ConfigKey` module of `[<Literal>]` key constants
lives beside the provider read in `OpticalConstructor.App`; typed values flow
inward as a new pure Domain record (`WorkbenchSettings`) of elevated types
(`SelectWindowModality` two-case DU — never a naked bool; the three thresholds
as single-case DUs with `.value` + `tryCreate` rejecting non-positive values).
`appsettings.json` is a build-copied content item of `OpticalConstructor.App`;
user preferences stay in `environment.json` (never merged). Unit tests
construct the settings record directly — defaults + `tryCreate` rejection —
with no file IO.

## API facts verified up front

- The packaged `Softellect.Sys 10.1.301.54` DLL (NuGet cache) carries exactly
  the members the local `C:\GitHub\Softellect` clone shows at
  `Sys/AppSettings.fs`: `tryCreate` (4 overloads; the `()` one opens
  `appsettings.json` resolved BESIDE the assembly via
  `FileName.tryGetFullFileName`), `getBoolOrDefault` / `getIntOrDefault`
  (SetOnMissing = true writes a missing/unparsable key's default into the
  in-memory `JObject`), `trySave` / `save` (persisting the write-backs needs an
  explicit save — SetOnMissing alone only mutates the in-memory object),
  `tryGetConnectionString` (the step-040 read seam). Verified by enumerating
  `AppSettingsProvider`'s method table from the packaged DLL's metadata.
- `tryCreate ()` fails (typed `FileError`) when the file is missing/unreadable
  — the loader must be total and fall back to `WorkbenchSettings.defaults`.

## Files to modify

1. **NEW `OpticalConstructor.Domain/WorkbenchSettings.fs`** (+ fsproj compile
   entry after `Units.fs` — depends on nothing): `WorkbenchSettingsError`
   (payload-carrying rejection cases), `SelectWindowModality =
   ModalSelectWindows | ModelessSelectWindows` (`.value` = wire bool,
   `create : bool -> _`, default Modeless), `QuickPickThreshold` (5),
   `TreeAutoBuildThreshold` (100), `ThicknessBucketCap` (8) as single-case DUs
   with `.value` / `defaultValue` / `tryCreate` (non-positive → typed Error),
   and the `WorkbenchSettings` record with `static member defaults`.
2. **NEW `OpticalConstructor.App/AppConfig.fs`** (compiled before Program.fs):
   the `[<RequireQualifiedAccess>] ConfigKey` literal inventory
   (SelectWindowsModal / QuickPickThreshold / TreeAutoBuildThreshold /
   ThicknessBucketCap, plus the step-040 keys: `DatabaseProvider` and the two
   connectionStrings entries `OpticalConstructorMssql` /
   `OpticalConstructorSqlite` — names are this step's recorded choice, the
   spec names none), and `AppConfig.loadWorkbenchSettings : unit ->
   WorkbenchSettings` — the ONE provider creation + read + `trySave` +
   elevation seam (invalid stored value → per-field default, logged).
3. **NEW `OpticalConstructor.App/appsettings.json`**: minimal skeleton
   (`appSettings` / `connectionStrings` empty sections) — first run
   self-documents by writing the four defaults back.
4. **`OpticalConstructor.App.fsproj`**: `Softellect.Sys 10.1.301.54` package
   ref, `AppConfig.fs` compile entry, `appsettings.json` as
   `Content/PreserveNewest`.
5. **`OpticalConstructor.App/Program.fs`**: `Startup.workbenchSettings =
   AppConfig.loadWorkbenchSettings ()` — created once with the module's
   startup bindings (forced when `App.Initialize` reads `Startup.settings`).
6. **NEW `OpticalConstructor.Tests/WorkbenchSettingsTests.fs`** (+ fsproj
   entry): defaults record (Modeless / 5 / 100 / 8), modality wire round-trip,
   per-type accept (positive, `.value` round-trip) and reject (0/negative →
   the typed error carrying the offending value) theories — pure construction,
   no disk.

## Risks

- **MSB3277 / new warnings from the heavy Softellect.Sys dependency set** in
  the App graph: the same package already coexists with Avalonia in
  `OpticalConstructor.Tests` (via Storage), so no conflict is expected; the
  diagnostic build log will be swept for MSB3277 and touched-project warnings.
- **ui-smoke**: the product `App.Initialize` now (transitively) runs the
  loader inside the headless session. The loader is total — if the content
  item does not flow into the Ui.Tests output the fallback path returns
  defaults without touching a window; no fixed sleeps, no prompts.
- **Name collision**: the new module `ConfigKey` (App) vs Softellect's
  `ConfigKey` DU — avoided by NOT opening `Softellect.Sys.AppSettings`
  (qualify as `AppSettings.ConfigKey`); our unqualified `ConfigKey.x` can only
  mean the literal module.
