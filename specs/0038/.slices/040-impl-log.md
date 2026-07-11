# Step 040 — Impl log (ADD_COMPONENT STORE_XDUO_0007 TestDbContext)

## Progress

- [x] Read system/project/slice prompts + prior SoW (039).
- [x] Create the C# EFC project `OpticalConstructor.Database`.
- [x] Author `Test` entity + `TestDbContext` + initial SQLite migration.
- [x] Add the project to `Berreman.slnx`.
- [x] Author the F# migration test + fixture appsettings + test-project wiring.
- [x] Build + run all gates locally (advisory — Invariant 6).
- [x] Write the state-of-the-world.

## Files modified / added

**Added — C# EFC project `OpticalConstructor.Database` (net10.0, x64):**
- `OpticalConstructor.Database/OpticalConstructor.Database.csproj` — references ONLY
  `Microsoft.EntityFrameworkCore.Sqlite` 10.0.9 (cached; SqlServer is neither cached
  nor needed). `NoWarn NU1903` (third-party SQLitePCLRaw advisory, exempt, matches App/Tests).
- `OpticalConstructor.Database/Test.cs` — the ONE entity: `int Id` key + `string Name`.
- `OpticalConstructor.Database/TestDbContext.cs` — `DbContext` with `DbSet<Test> Tests`,
  options-ctor only, table pinned to `Test` via `OnModelCreating`.
- `OpticalConstructor.Database/Migrations/20260711000000_Initial.cs` — the initial
  migration, SQLite-shaped (`INTEGER`/`TEXT`, `Sqlite:Autoincrement`).
- `OpticalConstructor.Database/Migrations/20260711000000_Initial.Designer.cs` — the
  `[DbContext]`/`[Migration]`-attributed Designer partial (runtime migration discovery).
- `OpticalConstructor.Database/Migrations/TestDbContextModelSnapshot.cs` — the model snapshot.

**Added — F# test + fixture:**
- `OpticalConstructor.Tests/TestDbMigrationTests.fs` — the EFC-wiring proof (the ONLY
  consumer of the Database project): a test-side `DatabaseProvider = Mssql | Sqlite` DU
  with `tryCreate`; reads the provider + connection string from the fixture via
  `AppSettingsProvider` and the step-5 `OpticalConstructor.App.ConfigKey` literals; runs
  `Database.Migrate()` against SQLite on a temp file under the test output; asserts the
  `Test` table exists (sqlite_master) and round-trips a row.
- `OpticalConstructor.Tests/fixtures/database-appsettings.json` — `DatabaseProvider=SQLite`
  + both connection-string entries (MSSQL present but unused).

**Edited:**
- `Berreman/Berreman.slnx` — added the Database project with the x64 platform mapping.
- `OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj` — added the compile item,
  the fixture content, `Microsoft.EntityFrameworkCore.Sqlite` 10.0.9, and project refs to
  `OpticalConstructor.App` (step-5 ConfigKey) and `OpticalConstructor.Database`.

## Testing state

All five gates run locally (advisory; the arc-runner's gate engine is the authority):

- **build** (`dotnet build Berreman.slnx -c Release`): 0 errors. 10 warnings, ALL
  pre-existing (NU1701 Wolfram, FS0044 ChartWindow, SYSLIB0051 MathNet, FS3873 Dispersion,
  FS1125 SeriesDataTests) — none from this round's code. The Database project's only
  advisory (NU1903, SQLitePCLRaw) is suppressed to match App/Tests.
- **unit-tests** (BerremanTests): 119 passed / 5 skipped — baseline 119, solver untouched.
- **constructor-unit-tests** (OpticalConstructor.Tests): 666 passed (659 → +7: 4 DU-parse
  theory cases + the unknown-provider rejection + the provider-selection fact + the
  migration round-trip fact). The migration physically created and round-tripped the
  `Test` table against a real SQLite file.
- **ui-smoke**: 173 passed — baseline 173 (Ui/Ui.Tests untouched).
- **ui-tests**: 461 passed — baseline 461 (Ui/Ui.Tests untouched).

`commit_ready: true`.

## Artifacts

No captured logs/traces needed beyond the gate stdout above; nothing routed to
`.artifacts/`. The SQLite temp DB is created under the test output and deleted in the
test's `finally` (best-effort cleanup).

## Gotchas

- **Anchor vs task shape.** The step is `ADD_COMPONENT` (a Contract-family anchor whose
  usual character is a UI component + headless semantic-tree test), but the slice's
  how-to is a C# EF Core migrations project + an F# migration test. Implemented the
  slice's explicit how-to/acceptance; there is no UI surface or automation-id tree here.
  Recorded per the "pick the interpretation most consistent with the spec" rule.
- **SQLite-shaped migration, hand-authored.** `dotnet ef` is NOT installed on this host,
  so the three EF files are hand-authored (as EF would emit them for SQLite — the STL
  migrations are checked-in source too). They are SQLite-shaped (`INTEGER`/`TEXT`,
  `Sqlite:Autoincrement`) rather than SqlServer-shaped on purpose: a SqlServer migration
  (`nvarchar(max)`, `SqlServer:Identity`) does NOT apply to SQLite (`nvarchar(max)` is a
  SQLite parse error). Since the acceptance is "the initial migration creates the Test
  table against SQLite", targeting SQLite is the correct and robust choice.
- **No SqlServer dependency.** The `DatabaseProvider` DU carries an `Mssql` case for the
  enumerated choice, but the test exercises only SQLite and the Database project
  references only the SQLite provider. This honors "no MSSQL server is required anywhere"
  and keeps the restore offline-clean (SqlServer EF packages are not cached on this host).
- **Provider/connection come from appsettings, never hardcoded.** The test reads
  `DatabaseProvider` (appSettings) and the SQLite connection string
  (`tryGetConnectionString`) through the step-5 `OpticalConstructor.App.ConfigKey`
  literals — hence the new project reference from Tests → App. Only the SQLite
  `DataSource` is relocated (via `SqliteConnectionStringBuilder`) to a unique temp file
  under the test output; the base connection string itself comes from appsettings.
- **appsettings.json collision avoided.** Referencing App copies App's own (empty)
  `appsettings.json` into the test output root. The fixture is therefore read from a
  `fixtures/` subfolder via an EXPLICIT absolute path (`AppSettingsProvider.tryCreate
  (FileName …)`), so the two never collide and the test reads its own fixture.
- **`use`-in-parens avoided.** Each context/connection lives in a small helper function
  so the `IDisposable` is released deterministically at function return (no ambiguous
  disposal scoping), and the temp DB file is deletable after `SqliteConnection.ClearAllPools()`.
- **No product project references the Database project** — only `OpticalConstructor.Tests`
  does (verified: the App/Ui/Domain/Storage/Optimization graph is unchanged except for the
  Tests → App edge, which is test-only).
- **System-prompt path drift (carried from steps 035–039).** The task file's
  `C:\GitHub\AI-Strategy-Generator\add_component_worker.system-md` is stale; the real
  per-family delta lives under `.../src/ai_strategy_generator/multistep/`. Located and read
  (with its `arc-runner.system-md` base). No scope impact.
