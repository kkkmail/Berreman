namespace OpticalConstructor.Tests

open System
open System.IO
open Xunit
open Microsoft.Data.Sqlite
open Microsoft.EntityFrameworkCore
open Softellect.Sys
open Softellect.Sys.Primitives
open OpticalConstructor.Database

/// Spec 0038 Part M (step 040, ADD_COMPONENT STORE_XDUO_0007) — the EFC-wiring proof.
/// This is the ONLY consumer of `OpticalConstructor.Database`: no product project
/// references it. The test reads the provider choice + connection string from a fixture
/// appsettings.json through `Softellect.Sys.AppSettings.AppSettingsProvider` using the
/// step-5 `OpticalConstructor.App.ConfigKey` literals (never a hardcoded key/string),
/// parses the provider into an elevated DU, runs the initial migration against SQLite on
/// a fresh temp file under the test output, and asserts the `Test` table was created and
/// round-trips a row. No MSSQL server is required anywhere.
module TestDbMigrationTests =

    /// The provider choice as an elevated DU — never a naked string past this parse
    /// boundary. The test parses the appsettings `DatabaseProvider` value into this and
    /// drives the SQLite branch; `Mssql` is a declared case the test does not exercise.
    type DatabaseProvider =
        | Mssql
        | Sqlite

        member this.value =
            match this with
            | Mssql -> "MSSQL"
            | Sqlite -> "SQLite"

        static member tryCreate (s : string) : Result<DatabaseProvider, string> =
            match s.Trim().ToUpperInvariant() with
            | "MSSQL" -> Ok Mssql
            | "SQLITE" -> Ok Sqlite
            | other -> Error $"Unknown database provider: '{other}'."

    /// The fixture appsettings.json is read by an EXPLICIT absolute path from the test
    /// output's fixtures/ folder, so it never collides with the App's own appsettings.json
    /// that also lands in the output root.
    let private settingsFileName () : FileName =
        Path.Combine(AppContext.BaseDirectory, "fixtures", "database-appsettings.json") |> FileName

    let private openProvider () : AppSettings.AppSettingsProvider =
        match AppSettings.AppSettingsProvider.tryCreate (settingsFileName ()) with
        | Ok p -> p
        | Error e -> failwith $"Could not open the fixture appsettings.json: %A{e}."

    /// The single read seam for the provider choice, going through the step-5 ConfigKey.
    let private readProviderChoice (provider : AppSettings.AppSettingsProvider) : DatabaseProvider =
        let raw = provider.getStringOrDefault (AppSettings.ConfigKey OpticalConstructor.App.ConfigKey.databaseProvider) ""
        match DatabaseProvider.tryCreate raw with
        | Ok p -> p
        | Error e -> failwith $"Bad DatabaseProvider in appsettings: %A{e}."

    /// The connection string comes from appsettings via `tryGetConnectionString` and the
    /// step-5 ConfigKey; only its DataSource is relocated to a fresh temp file under the
    /// test output (the base string itself is never hardcoded here).
    let private sqliteConnectionString (provider : AppSettings.AppSettingsProvider) (dbPath : string) : string =
        let baseConn =
            match provider.tryGetConnectionString (AppSettings.ConfigKey OpticalConstructor.App.ConfigKey.sqliteConnectionString) with
            | Ok (Some s) -> s
            | Ok None -> failwith "SQLite connection string missing from appsettings."
            | Error e -> failwith $"Could not read the SQLite connection string: %A{e}."

        let builder = SqliteConnectionStringBuilder(baseConn)
        builder.DataSource <- dbPath
        builder.ConnectionString

    let private makeOptions (connectionString : string) : DbContextOptions<TestDbContext> =
        DbContextOptionsBuilder<TestDbContext>().UseSqlite(connectionString).Options

    // Each helper owns its context/connection so the resource is disposed at function
    // return — no ambiguous `use`-in-parens scoping.

    let private migrate (options : DbContextOptions<TestDbContext>) : unit =
        use context = new TestDbContext(options)
        context.Database.Migrate()

    let private insertRow (options : DbContextOptions<TestDbContext>) (name : string) : unit =
        use context = new TestDbContext(options)
        context.Tests.Add(Test(Name = name)) |> ignore
        context.SaveChanges() |> ignore

    let private readSingle (options : DbContextOptions<TestDbContext>) : Test =
        use context = new TestDbContext(options)
        context.Tests |> Seq.exactlyOne

    let private queryTableName (connectionString : string) : string =
        use conn = new SqliteConnection(connectionString)
        conn.Open()
        use cmd = conn.CreateCommand()
        cmd.CommandText <- "SELECT name FROM sqlite_master WHERE type = 'table' AND name = 'Test';"
        cmd.ExecuteScalar() |> string

    [<Theory>]
    [<InlineData("SQLite", "SQLite")>]
    [<InlineData("sqlite", "SQLite")>]
    [<InlineData("MSSQL", "MSSQL")>]
    [<InlineData(" mssql ", "MSSQL")>]
    let ``DatabaseProvider.tryCreate parses the enumerated choice`` (raw : string) (expected : string) =
        match DatabaseProvider.tryCreate raw with
        | Ok p -> Assert.Equal(expected, p.value)
        | Error e -> Assert.Fail($"expected Ok, got %A{e}")

    [<Fact>]
    let ``DatabaseProvider.tryCreate rejects an unknown provider`` () =
        match DatabaseProvider.tryCreate "Postgres" with
        | Ok p -> Assert.Fail($"expected rejection, got Ok %A{p}")
        | Error _ -> ()

    [<Fact>]
    let ``appsettings selects the SQLite provider through the step-5 ConfigKey`` () =
        let provider = openProvider ()
        Assert.Equal(Sqlite, readProviderChoice provider)

    [<Fact>]
    let ``the initial migration creates and round-trips the Test table against SQLite`` () =
        let provider = openProvider ()

        // Provider selection comes from appsettings, parsed to the DU.
        Assert.Equal(Sqlite, readProviderChoice provider)

        // A fresh temp file under the test output; the connection string is derived from
        // the appsettings SQLite entry with only its DataSource relocated.
        let dbPath = Path.Combine(AppContext.BaseDirectory, $"testdb-{Guid.NewGuid():N}.db")
        if File.Exists dbPath then File.Delete dbPath
        let connectionString = sqliteConnectionString provider dbPath
        let options = makeOptions connectionString

        try
            // Create + migrate: the initial migration builds the Test table.
            migrate options

            // The Test table exists ...
            Assert.Equal("Test", queryTableName connectionString)

            // ... and round-trips a row (proving it is a real, writable table).
            insertRow options "efc-wiring"
            let stored = readSingle options
            Assert.Equal("efc-wiring", stored.Name)
            Assert.True(stored.Id > 0)
        finally
            // Release the SQLite handle before deleting the temp file; cleanup is
            // best-effort (a locked temp file must never fail the assertions above).
            SqliteConnection.ClearAllPools()
            try
                if File.Exists dbPath then File.Delete dbPath
            with _ -> ()
