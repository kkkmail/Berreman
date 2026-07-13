# Step 040 — Impl plan (ADD_COMPONENT STORE_XDUO_0007 TestDbContext)

## Goal

Create a nearly-empty C# EF Core project `OpticalConstructor.Database` that proves
EFC wiring (create + migrate) works, exercised ONLY from an `OpticalConstructor.Tests`
F# test running against SQLite on a temp file. No product project references it.
Provider choice + connection string come from appsettings.json (the step-5 `ConfigKey`
inventory), never a hardcoded string.

## Approach

New C# project `Berreman/OpticalConstructor/OpticalConstructor.Database` (net10.0, x64;
added to `Berreman.slnx`), modeled on the STL migrations shape
(`Softellect/Apps/DistrProc/Migrations/Common`):

- `Test.cs` — ONE entity: `int Id` key + `string Name`.
- `TestDbContext.cs` — `DbContext` with `DbSet<Test> Tests`, mapped to table `Test`,
  options-ctor only (the runtime path the test drives).
- `Migrations/…_Initial.cs` + `.Designer.cs` + `TestDbContextModelSnapshot.cs` —
  the initial migration, **hand-authored SQLite-shaped** (`INTEGER`/`TEXT`,
  `Sqlite:Autoincrement`). `dotnet ef` is NOT installed on this host, and the STL
  migrations are checked-in source anyway, so hand-authoring the three EF files is
  the deterministic path. Discovery is by the `[DbContext(typeof(TestDbContext))]` +
  `[Migration(id)]` attributes on the Designer partial.
- References ONLY `Microsoft.EntityFrameworkCore.Sqlite` (10.0.9, cached). SqlServer is
  neither cached nor needed — the test exercises SQLite only.

F# test in `OpticalConstructor.Tests`:

- `TestDbMigrationTests.fs` — reads a fixture `fixtures/database-appsettings.json`
  (explicit absolute path via `AppSettingsProvider.tryCreate (FileName …)` so it never
  collides with App's own `appsettings.json` in the output root), parses the
  `DatabaseProvider` appSettings value into a test-side `DatabaseProvider = Mssql | Sqlite`
  DU (with `tryCreate`), reads the SQLite connection string via
  `provider.tryGetConnectionString (ConfigKey ConfigKey.sqliteConnectionString)`,
  relocates its `DataSource` to a unique temp file under `AppContext.BaseDirectory` via
  `SqliteConnectionStringBuilder`, runs `TestDbContext(...).Database.Migrate()`, and
  asserts the `Test` table exists (sqlite_master) and round-trips a row.
- Adds project references to `OpticalConstructor.App` (for the step-5 `ConfigKey`
  literals) and `OpticalConstructor.Database`, plus an explicit
  `Microsoft.EntityFrameworkCore.Sqlite` package ref (for `UseSqlite` /
  `SqliteConnectionStringBuilder` / native SQLite in the test output).

## Files

- ADD `OpticalConstructor.Database/OpticalConstructor.Database.csproj`
- ADD `OpticalConstructor.Database/Test.cs`
- ADD `OpticalConstructor.Database/TestDbContext.cs`
- ADD `OpticalConstructor.Database/Migrations/20260711000000_Initial.cs`
- ADD `OpticalConstructor.Database/Migrations/20260711000000_Initial.Designer.cs`
- ADD `OpticalConstructor.Database/Migrations/TestDbContextModelSnapshot.cs`
- ADD `OpticalConstructor.Tests/TestDbMigrationTests.fs`
- ADD `OpticalConstructor.Tests/fixtures/database-appsettings.json`
- EDIT `OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj` (compile + content + refs)
- EDIT `Berreman/Berreman.slnx` (add the project)

## Risks

- A SqlServer-shaped migration would NOT apply to SQLite (`nvarchar(max)` is a SQLite
  parse error). Mitigated by hand-authoring a SQLite-shaped migration.
- Referencing App pulls Avalonia into the test closure (already there via the existing
  Ui reference) — additive, no cycle.
- appsettings.json output-root collision — avoided by reading the fixture from a
  `fixtures/` subfolder via an explicit absolute path.
