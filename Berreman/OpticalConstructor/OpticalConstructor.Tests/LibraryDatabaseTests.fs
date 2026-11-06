namespace OpticalConstructor.Tests

open System
open System.IO
open System.Text
open System.Text.Json.Nodes
open Microsoft.Data.Sqlite
open Microsoft.EntityFrameworkCore
open OpticalConstructor.Database
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.Lifecycle
open OpticalConstructor.Domain.MaterialLibrary
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

    [<Fact>]
    let ``every kind's entry round-trips through its database row byte for byte`` () : unit =
        let connectionString = ConnectionString "Data Source=:memory:"
        use database = LibraryDatabase.openOrCreate connectionString |> expectDatabaseOk

        let versions : StoredEntryVersion list =
            (BuiltinCatalogue.catalogueData ()).envelopes
            |> List.mapi (fun index envelope ->
                let state : EntryVersionState =
                    if index % 2 = 0 then
                        {
                            lifecycle = ActiveEntry
                            pin = VersionUnpinned
                        }
                    else
                        {
                            lifecycle = InactiveEntry
                            pin = VersionPinned
                        }

                {
                    envelope = envelope
                    state = state
                })

        database.run (fun context ->
            for version in versions do
                let row = version.versionRow |> expectDatabaseOk
                context.EntryVersions.Add(row) |> ignore
            context.SaveChanges() |> ignore)
        |> expectDatabaseOk

        let rows =
            database.run (fun context -> context.EntryVersions.AsNoTracking() |> Seq.toList)
            |> expectDatabaseOk

        let expectedVersions =
            versions
            |> List.map (fun version -> (version.envelope.content.id, version.envelope.content.version), version)
            |> Map.ofList

        Assert.Equal(versions.Length, rows.Length)

        let restored =
            rows
            |> List.map (fun row ->
                let version = row.toStoredVersion |> expectDatabaseOk
                let content = version.envelope.content
                let expected = expectedVersions |> Map.find (content.id, content.version)
                Assert.Equal<StoredEntryVersion>(expected, version)
                let file = FileName (Path.Combine(AppContext.BaseDirectory, "Catalogue", "Builtin", content.fileName.value))
                Assert.Equal<byte[]>(File.ReadAllBytes(file.value), Encoding.UTF8.GetBytes(row.PayloadText))
                version)

        let kinds = restored |> List.map (fun version -> version.envelope.content.schema.kind) |> Set.ofList
        Assert.Equal<Set<LibraryEntryKind>>(Set.ofList LibraryEntryKind.all, kinds)

    [<Fact>]
    let ``a row its payload does not support is refused`` () : unit =
        let envelope =
            (BuiltinCatalogue.catalogueData ()).envelopes
            |> List.find (fun entry -> entry.content.id = MaterialIds.silicon.toLibraryEntryId)

        let stored : StoredEntryVersion =
            {
                envelope = envelope
                state =
                    {
                        lifecycle = ActiveEntry
                        pin = VersionUnpinned
                    }
            }

        let freshRow () : LibraryEntryVersionRow = stored.versionRow |> expectDatabaseOk

        let assertInvalidColumn (column : string) (row : LibraryEntryVersionRow) : unit =
            match row.toStoredVersion with
            | Error (LibraryRowInvalidErr reason as error) ->
                Assert.Contains(string row.EntryId, reason)
                Assert.Contains($"v{row.Version}", reason)
                Assert.Contains($"column {column}", reason)
                Assert.Equal(reason, error.describe)
            | Error error -> Assert.Fail($"expected an invalid {column} column, got {error.describe}")
            | Ok _ -> Assert.Fail($"an invalid {column} column was accepted")

        let unsupportedColumns : (string * (LibraryEntryVersionRow -> unit)) list =
            [
                "EntryId", (fun row -> row.EntryId <- Guid.Empty)
                "Version", (fun row -> row.Version <- row.Version + 1)
                "Name", (fun row -> row.Name <- "Renamed silicon")
                "Protection", (fun row -> row.Protection <- 2)
                "ProvenanceKind", (fun row -> row.ProvenanceKind <- 2)
                "CatalogueId", (fun row -> row.CatalogueId <- Nullable<Guid>())
                "PayloadText", (fun row -> row.PayloadText <- row.PayloadText + " ")
                "ContentHash", (fun row -> row.ContentHash <- String.replicate 64 "0")
            ]

        for column, change in unsupportedColumns do
            let row = freshRow ()
            change row
            assertInvalidColumn column row

        let unknownTags : (string * (LibraryEntryVersionRow -> unit)) list =
            [
                "Kind", (fun row -> row.Kind <- 0)
                "Lifecycle", (fun row -> row.Lifecycle <- 0)
                "PinState", (fun row -> row.PinState <- 0)
            ]

        for column, change in unknownTags do
            let row = freshRow ()
            change row
            assertInvalidColumn column row
            row.PayloadText <- "not JSON"
            assertInvalidColumn column row

            match row.toStoredVersion with
            | Error (LibraryRowInvalidErr reason) -> Assert.Contains("tag 0", reason)
            | Error error -> Assert.Fail(error.describe)
            | Ok _ -> Assert.Fail("an unknown tag was accepted")

        let twoMismatches = freshRow ()
        twoMismatches.Name <- "Renamed silicon"
        twoMismatches.ContentHash <- String.replicate 64 "0"
        assertInvalidColumn "Name" twoMismatches

        let tampered = freshRow ()
        let entry = JsonNode.Parse(tampered.PayloadText)
        entry["name"] <- JsonValue.Create "Tampered silicon"
        tampered.PayloadText <- entry.ToJsonString()

        match tampered.toStoredVersion with
        | Error (ContentHashMismatchErr info) ->
            Assert.Equal<LibraryEntryId>(envelope.content.id, info.entryId)
            Assert.Equal<ContentHash>(envelope.contentHash, info.recorded)
            Assert.NotEqual<ContentHash>(info.recorded, info.computed)
        | Error error -> Assert.Fail($"expected a payload hash mismatch, got {error.describe}")
        | Ok _ -> Assert.Fail("a tampered payload was accepted")

        let incompatible = freshRow ()
        let entry = JsonNode.Parse(incompatible.PayloadText)
        entry["schema"]["version"] <- JsonValue.Create 2
        incompatible.PayloadText <- entry.ToJsonString()

        match incompatible.toStoredVersion with
        | Error (UnsupportedSchemaVersionErr info) ->
            Assert.Equal<FileExtension>(MaterialEntryKind.extension, info.fileExtension)
            Assert.Equal<FileSchemaVersion>(FileSchemaVersion 2, info.found)
            Assert.Equal<FileSchemaVersion>(MaterialEntryKind.currentSchemaVersion, info.expected)
        | Error error -> Assert.Fail($"expected an unsupported schema version, got {error.describe}")
        | Ok _ -> Assert.Fail("an incompatible payload schema was accepted")
