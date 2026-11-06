namespace OpticalConstructor.Tests

open System
open System.IO
open Microsoft.Data.Sqlite
open Microsoft.EntityFrameworkCore
open OpticalConstructor.Database
open OpticalConstructor.Storage.Errors
open OpticalConstructor.Storage.LibraryDatabaseStore
open Softellect.Sys.Primitives
open Xunit

/// Pins the library's SQLite model, lifetime and schema rejection contract (spec 0050 S2.1).
module LibraryDatabaseTests =

    let private readNames (command : SqliteCommand) : Set<string> =
        use reader = command.ExecuteReader()
        seq
            {
                while reader.Read() do
                    yield reader.GetString(0)
            }
        |> Set.ofSeq

    let private expectDatabaseOk (result : Result<'Outcome, StorageError>) : 'Outcome =
        match result with
        | Ok outcome -> outcome
        | Error error -> failwith error.describe

    type private LibraryDatabaseFixture () =
        let folder = FolderName (Path.Combine(AppContext.BaseDirectory, "LibraryDatabaseTests", Guid.NewGuid().ToString("N")))
        let file = FileName (Path.Combine(folder.value, "library.db"))
        do Directory.CreateDirectory(folder.value) |> ignore

        member _.databaseFile : FileName = file

        member _.connectionString : ConnectionString =
            let builder = SqliteConnectionStringBuilder()
            builder.DataSource <- file.value
            builder.Pooling <- false
            ConnectionString builder.ConnectionString

        interface IDisposable with
            member _.Dispose () : unit = Directory.Delete(folder.value, true)

    let private catalogueImportFixture () : LibraryCatalogueImportRow =
        LibraryCatalogueImportRow(
            CatalogueId = Guid.Parse("b730da46-98d5-4a69-88c3-a9d0bb38b983"),
            Folder = "test-catalogue",
            ManifestHash = "0123456789abcdef",
            ImportedAtUtc = DateTime(2026, 11, 6, 12, 0, 0, DateTimeKind.Utc),
            EntryCount = 7)

    let private assertCatalogueImport (expected : LibraryCatalogueImportRow) (actual : LibraryCatalogueImportRow) : unit =
        Assert.Equal<Guid>(expected.CatalogueId, actual.CatalogueId)
        Assert.Equal<string>(expected.Folder, actual.Folder)
        Assert.Equal<string>(expected.ManifestHash, actual.ManifestHash)
        Assert.Equal<DateTime>(expected.ImportedAtUtc, actual.ImportedAtUtc)
        Assert.Equal<int>(expected.EntryCount, actual.EntryCount)

    let private assertLibraryStamp (database : LibraryDatabase) : unit =
        database.run (fun context ->
            let stamp = Assert.Single<LibrarySchemaStampRow>(context.SchemaStamps)
            Assert.Equal(1, stamp.Id)
            Assert.Equal(1, stamp.SchemaVersion))
        |> expectDatabaseOk

    let private assertSchemaMismatch (foundOpt : LibrarySchemaVersion option) (connectionString : ConnectionString) : unit =
        match LibraryDatabase.openOrCreate connectionString with
        | Error (LibrarySchemaMismatchErr info as error) ->
            Assert.Equal<LibrarySchemaVersion option>(foundOpt, info.foundOpt)
            Assert.Equal<LibrarySchemaVersion>(LibrarySchemaVersion.current, info.expected)
            Assert.Contains($"expected schema version {LibrarySchemaVersion.current.value}", error.describe)

            match foundOpt with
            | Some found -> Assert.Contains($"schema version {found.value}", error.describe)
            | None -> Assert.Contains("no stamp", error.describe)
        | Error error -> Assert.Fail($"expected a schema mismatch, got {error.describe}")
        | Ok database ->
            use unexpected = database
            Assert.Fail("an incompatible database was accepted")

    [<Fact>]
    let ``the library model creates exactly its seven tables and their columns`` () : unit =
        use connection = new SqliteConnection("Data Source=:memory:")
        connection.Open()

        let options = DbContextOptionsBuilder<LibraryDbContext>().UseSqlite(connection).Options
        use context = new LibraryDbContext(options)

        Assert.True(context.Database.EnsureCreated())

        let expectedColumns : Map<string, Set<string>> =
            [
                "EntryVersion",
                set [ "EntryId"; "Version"; "Kind"; "Name"; "Lifecycle"; "Protection"
                      "PinState"; "ProvenanceKind"; "CatalogueId"; "PayloadText"; "ContentHash" ]
                "LatestVersion", set [ "EntryId"; "Version" ]
                "EntryFacet", set [ "EntryId"; "Version"; "FacetKey"; "FacetValue" ]
                "SampleMaterial", set [ "SampleId"; "SampleVersion"; "MaterialId"; "MaterialVersion" ]
                "CatalogueImport", set [ "CatalogueId"; "Folder"; "ManifestHash"; "ImportedAtUtc"; "EntryCount" ]
                "PulledSource", set [ "EntryId"; "Version"; "Source"; "RecordPath"; "SourceVersion"; "Licence"; "Citation" ]
                "SchemaStamp", set [ "Id"; "SchemaVersion" ]
            ]
            |> Map.ofList

        use tablesCommand = connection.CreateCommand()
        tablesCommand.CommandText <-
            "SELECT name FROM sqlite_master WHERE type = 'table' AND name NOT GLOB 'sqlite_*';"

        let expectedTables = expectedColumns |> Map.keys |> Set.ofSeq
        Assert.Equal<Set<string>>(expectedTables, readNames tablesCommand)

        for KeyValue(tableName, columns) in expectedColumns do
            use columnsCommand = connection.CreateCommand()
            columnsCommand.CommandText <- "SELECT name FROM pragma_table_info($table);"
            columnsCommand.Parameters.AddWithValue("$table", tableName) |> ignore
            Assert.Equal<Set<string>>(columns, readNames columnsCommand)

        Assert.False(context.Database.EnsureCreated())

    [<Fact>]
    let ``an in-memory library database is created, stamped and lives as long as its handle`` () : unit =
        let connectionString = ConnectionString "Data Source=:memory:"
        use database = LibraryDatabase.openOrCreate connectionString |> expectDatabaseOk
        Assert.Equal(1, LibrarySchemaVersion.current.value)
        assertLibraryStamp database
        let expected = catalogueImportFixture ()

        database.run (fun context ->
            context.CatalogueImports.Add(expected) |> ignore
            context.SaveChanges() |> ignore)
        |> expectDatabaseOk

        database.run (fun context -> Assert.Single<LibraryCatalogueImportRow>(context.CatalogueImports))
        |> expectDatabaseOk
        |> assertCatalogueImport expected

        (database :> IDisposable).Dispose()

        match database.run (fun _ -> Assert.Fail("work ran after the database was disposed")) with
        | Error (LibraryDatabaseExn error) -> Assert.IsType<ObjectDisposedException>(error) |> ignore
        | Error error -> Assert.Fail(error.describe)
        | Ok () -> Assert.Fail("a disposed database accepted work")

        use fresh = LibraryDatabase.openOrCreate connectionString |> expectDatabaseOk
        assertLibraryStamp fresh
        fresh.run (fun context -> Assert.Empty(context.CatalogueImports)) |> expectDatabaseOk

        let namedMemoryString = ConnectionString "Data Source=library-memory;Mode=Memory"
        use namedMemory = LibraryDatabase.openOrCreate namedMemoryString |> expectDatabaseOk
        assertLibraryStamp namedMemory
        namedMemory.run (fun context ->
            use command = context.Database.GetDbConnection().CreateCommand()
            command.CommandText <- "PRAGMA journal_mode;"
            Assert.Equal("memory", command.ExecuteScalar() :?> string))
        |> expectDatabaseOk

    [<Fact>]
    let ``a library database file runs in WAL mode and keeps its stamp and rows across opens`` () : unit =
        use fixture = new LibraryDatabaseFixture()
        let expected = catalogueImportFixture ()

        let writeFile () : unit =
            let builder = SqliteConnectionStringBuilder(fixture.connectionString.value)
            builder.Pooling <- true
            let pooledString = ConnectionString builder.ConnectionString
            use database = LibraryDatabase.openOrCreate pooledString |> expectDatabaseOk
            assertLibraryStamp database
            database.run (fun context ->
                use command = context.Database.GetDbConnection().CreateCommand()
                command.CommandText <- "PRAGMA journal_mode;"
                Assert.Equal("wal", command.ExecuteScalar() :?> string)
                let builder = SqliteConnectionStringBuilder(context.Database.GetConnectionString())
                Assert.False(builder.Pooling)
                context.CatalogueImports.Add(expected) |> ignore
                context.SaveChanges() |> ignore)
            |> expectDatabaseOk

        writeFile ()

        do
            use exclusive = new FileStream(fixture.databaseFile.value, FileMode.Open, FileAccess.ReadWrite, FileShare.None)
            Assert.True(exclusive.Length > 0L)

        use reopened = LibraryDatabase.openOrCreate fixture.connectionString |> expectDatabaseOk
        assertLibraryStamp reopened
        reopened.run (fun context -> Assert.Single<LibraryCatalogueImportRow>(context.CatalogueImports))
        |> expectDatabaseOk
        |> assertCatalogueImport expected

    [<Fact>]
    let ``a stamp other than the model's is LibrarySchemaMismatchErr and the file is left as it was`` () : unit =
        use incompatible = new LibraryDatabaseFixture()

        do
            use database = LibraryDatabase.openOrCreate incompatible.connectionString |> expectDatabaseOk
            database.run (fun context ->
                context.Database.ExecuteSqlRaw("UPDATE SchemaStamp SET SchemaVersion = 2 WHERE Id = 1;") |> ignore)
            |> expectDatabaseOk

        let incompatibleVersion = LibrarySchemaVersion 2
        let incompatibleBytes = File.ReadAllBytes(incompatible.databaseFile.value)
        assertSchemaMismatch (Some incompatibleVersion) incompatible.connectionString
        assertSchemaMismatch (Some incompatibleVersion) incompatible.connectionString
        Assert.Equal<byte[]>(incompatibleBytes, File.ReadAllBytes(incompatible.databaseFile.value))

        do
            use connection = new SqliteConnection(incompatible.connectionString.value)
            connection.Open()
            use command = connection.CreateCommand()
            command.CommandText <- "SELECT SchemaVersion FROM SchemaStamp WHERE Id = 1;"
            Assert.Equal(2L, command.ExecuteScalar() :?> int64)

        use unrelated = new LibraryDatabaseFixture()

        do
            use connection = new SqliteConnection(unrelated.connectionString.value)
            connection.Open()
            use command = connection.CreateCommand()
            command.CommandText <- "CREATE TABLE TestOnly (Value TEXT NOT NULL); INSERT INTO TestOnly VALUES ('preserved');"
            command.ExecuteNonQuery() |> ignore

        let unrelatedBytes = File.ReadAllBytes(unrelated.databaseFile.value)
        assertSchemaMismatch None unrelated.connectionString
        assertSchemaMismatch None unrelated.connectionString
        Assert.Equal<byte[]>(unrelatedBytes, File.ReadAllBytes(unrelated.databaseFile.value))

        do
            use connection = new SqliteConnection(unrelated.connectionString.value)
            connection.Open()
            use command = connection.CreateCommand()
            command.CommandText <- "SELECT name FROM sqlite_master WHERE type = 'table' AND name NOT GLOB 'sqlite_*';"
            Assert.Equal<Set<string>>(set [ "TestOnly" ], readNames command)
            command.CommandText <- "SELECT Value FROM TestOnly;"
            Assert.Equal("preserved", command.ExecuteScalar() :?> string)

        use unstamped = new LibraryDatabaseFixture()

        do
            use database = LibraryDatabase.openOrCreate unstamped.connectionString |> expectDatabaseOk
            database.run (fun context -> context.Database.ExecuteSqlRaw("DELETE FROM SchemaStamp;") |> ignore)
            |> expectDatabaseOk

        let unstampedBytes = File.ReadAllBytes(unstamped.databaseFile.value)
        assertSchemaMismatch None unstamped.connectionString
        Assert.Equal<byte[]>(unstampedBytes, File.ReadAllBytes(unstamped.databaseFile.value))

        do
            use connection = new SqliteConnection(unstamped.connectionString.value)
            connection.Open()
            use command = connection.CreateCommand()
            command.CommandText <- "SELECT COUNT(*) FROM SchemaStamp;"
            Assert.Equal(0L, command.ExecuteScalar() :?> int64)

    [<Fact>]
    let ``an EF Core failure inside run is LibraryDatabaseExn, never a throw`` () : unit =
        let connectionString = ConnectionString "Data Source=:memory:;Foreign Keys=False"
        use database = LibraryDatabase.openOrCreate connectionString |> expectDatabaseOk

        let result =
            database.run (fun context ->
                let orphan =
                    LibraryEntryFacetRow(
                        EntryId = Guid.Parse("34a9cd96-852a-4c68-920e-5b0f550032c9"),
                        Version = 1,
                        FacetKey = "category",
                        FacetValue = "missing-entry")
                context.EntryFacets.Add(orphan) |> ignore
                context.SaveChanges() |> ignore)

        match result with
        | Error (LibraryDatabaseExn error as storageError) ->
            Assert.IsType<DbUpdateException>(error) |> ignore
            Assert.Equal(error.Message, storageError.describe)
        | Error error -> Assert.Fail(error.describe)
        | Ok () -> Assert.Fail("an orphan facet was saved despite its foreign key")

        database.run (fun context -> Assert.Empty(context.EntryFacets)) |> expectDatabaseOk
        assertLibraryStamp database

        let callbackFailure = InvalidOperationException("callback failure")

        match database.run<unit> (fun _ -> raise callbackFailure) with
        | Error (LibraryDatabaseExn error) -> Assert.Same(callbackFailure, error)
        | Error error -> Assert.Fail(error.describe)
        | Ok () -> Assert.Fail("a callback exception was not captured")

        let malformedString = ConnectionString "not a connection string"

        match LibraryDatabase.openOrCreate malformedString with
        | Error (LibraryDatabaseExn _) -> ()
        | Error error -> Assert.Fail(error.describe)
        | Ok unexpected ->
            use unexpected = unexpected
            Assert.Fail("an invalid connection string was accepted")
