# Step 040 — State of the world

## Where we are

Spec 0038 Part M (the persistence/EFC-wiring proof). Step 040 (`ADD_COMPONENT`,
`STORE_XDUO_0007 TestDbContext`, `declaring_project: OpticalConstructor.Database`,
`touches: [OpticalConstructor.Database, OpticalConstructor.Tests]`, `depends_on: [5]`)
introduces a nearly-empty C# EF Core project whose ONLY purpose is proving EFC create +
migrate wiring works end-to-end, exercised from a single `OpticalConstructor.Tests` F#
test against SQLite on a temp file. It builds on step 5's `ConfigKey` inventory + the
`AppSettingsProvider` seam (the fixture appsettings supplies the provider choice and
connection strings). No product project references the new Database project.

## What's working

- Add the C# EF Core project `OpticalConstructor.Database` (net10.0, x64; in `Berreman.slnx`):
  one `Test` entity (int `Id` key + `Name`), `TestDbContext`, and an initial SQLite-shaped
  migration — modeled on the STL migrations shape, trimmed to the single table.
- Reference ONLY the SQLite EF Core provider (10.0.9): no MSSQL server or SqlServer
  package is required anywhere.
- Prove EFC wiring in `OpticalConstructor.Tests`: read the provider choice + connection
  string from a fixture appsettings.json via `AppSettingsProvider` and the step-5
  `ConfigKey` literals (never a hardcoded key/string), parse the provider into a
  `DatabaseProvider` DU, and run `Database.Migrate()` against SQLite on a temp file.
- Assert the initial migration created the `Test` table (sqlite_master) and round-trips a
  row; keep the Database project out of every product dependency graph (Tests-only).

## Tests

Gate execution is the arc-runner gate engine's job (Invariant 6 — the worker acts, runs
no gates). Local runs below are advisory, done for baseline honesty. Roster: `build`,
`unit-tests`, `constructor-unit-tests`, `ui-smoke`, `ui-tests`.

- **build**: green (0 errors); no new warnings from touched projects (only the
  pre-existing NU1701 / SYSLIB0051 / FS0044 / FS3873 / FS1125 remain; the Database
  project's NU1903 SQLitePCLRaw advisory is suppressed to match App/Tests).
- **unit-tests** (BerremanTests): 119 passed / 5 pre-existing skips — unaffected (solver untouched).
- **constructor-unit-tests**: 666 passed (659 → +7: 4 DU-parse theory cases, the
  unknown-provider rejection, the provider-selection fact, and the migration round-trip fact).
- **ui-smoke**: 173 passed — unaffected (Ui/Ui.Tests untouched).
- **ui-tests**: 461 passed — unaffected (Ui/Ui.Tests untouched).

```yaml
gates:
  berreman_unit_tests:    119
  constructor_unit_tests: 666
  ui_smoke_tests:         173
  ui_tests:               461
```

## Architecture

- **The Database project is a pure EFC-wiring proof, isolated from the product.** It is a
  single-entity C# EF Core project (`Test` + `TestDbContext` + one migration) whose only
  consumer is the F# test. No product project references it, so the app's dependency graph
  is unchanged. Modeled on the STL `Migrations/Common` shape but trimmed: no shared base,
  no service-name abstraction, no SqlServer.
- **Provider + connection string are injected from appsettings, never hardcoded.** The
  context is provider-agnostic (options-ctor only); the test chooses the provider by
  reading the step-5 `DatabaseProvider` key and the SQLite connection string via
  `AppSettingsProvider.tryGetConnectionString`, both keyed by the step-5
  `OpticalConstructor.App.ConfigKey` literals. The raw provider string is elevated to a
  `DatabaseProvider = Mssql | Sqlite` DU (with `tryCreate`) before use — no naked string
  past the parse boundary.
- **The migration is SQLite-shaped on purpose.** A SqlServer migration (`nvarchar(max)`,
  `SqlServer:Identity`) does not apply to SQLite; the acceptance requires the migration to
  create the `Test` table against SQLite, so the migration uses `INTEGER`/`TEXT` +
  `Sqlite:Autoincrement`. Runtime migration discovery is by the `[DbContext]`/`[Migration]`
  attributes on the Designer partial (no `dotnet ef` tool required at build or run).
- **Temp-file isolation for the SQLite DB.** Each run relocates the appsettings SQLite
  `DataSource` to a fresh `AppContext.BaseDirectory` temp file (via
  `SqliteConnectionStringBuilder`), so the test is order-independent and self-cleaning.

## Deferred

- **Any real product persistence** (materials/samples/scenes/experiment collections into a
  database) — this step proves EFC wiring only; the current stores remain the in-memory /
  JSON seams from earlier slices. Wiring a product store onto EFC is a future step.
- **An MSSQL path.** The `Mssql` DU case and the `OpticalConstructorMssql` connection-string
  entry exist for the enumerated choice, but no SqlServer provider is referenced and no
  server is exercised — SQLite is the only proven provider.
- **`dotnet ef` design-time regeneration** (a design-time factory + the EF tools) — the
  migration is hand-authored; adding a factory is only needed if/when the schema grows.

## Gotchas

- **Anchor vs task shape.** `ADD_COMPONENT` usually means a UI component + headless
  semantic-tree test, but this slice's how-to is a C# EFC project + F# migration test.
  Implemented the slice's explicit how-to/acceptance; there is no UI surface here (per the
  "pick the interpretation most consistent with the spec" rule). Full rationale in the
  impl-log `Gotchas`.
- **Tests → App reference is new and test-only.** The step-5 `ConfigKey` literals live in
  `OpticalConstructor.App`; the proof reads them, so `OpticalConstructor.Tests` now
  references App. This is a test-only edge — no product project references the Database
  project, and App/Ui/Domain/Storage/Optimization are otherwise unchanged.
- **appsettings.json output-root collision avoided.** Referencing App copies its (empty)
  `appsettings.json` to the test output root; the fixture is read from a `fixtures/`
  subfolder via an explicit absolute path so the two never collide.
- **Offline-clean restore.** SQLite EF Core 10.0.9 is in the NuGet cache; SqlServer EF is
  not — another reason the proof targets SQLite only.
- **System-prompt path drift (carried from steps 035–039).** The task file's
  `add_component_worker.system-md` path is stale; the real per-family delta is under
  `.../src/ai_strategy_generator/multistep/`. Located and read. No scope impact.

## Changelog

- 2026-07-11 — Step 040 (ADD_COMPONENT STORE_XDUO_0007): added the C# EF Core project
  `OpticalConstructor.Database` (net10.0/x64; in `Berreman.slnx`) — a `Test` entity,
  `TestDbContext`, and an initial SQLite-shaped migration, referencing only the SQLite
  provider. Added the `OpticalConstructor.Tests` EFC-wiring proof (`TestDbMigrationTests`):
  a `DatabaseProvider` DU parsed from appsettings via the step-5 `ConfigKey` +
  `AppSettingsProvider.tryGetConnectionString`, running `Database.Migrate()` against SQLite
  on a temp file and asserting the `Test` table exists + round-trips a row. No product
  project references the Database project. Build green; suites 119 / 666 / 173 / 461.
