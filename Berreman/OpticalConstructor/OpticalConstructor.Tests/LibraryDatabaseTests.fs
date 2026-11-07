namespace OpticalConstructor.Tests

open System
open System.IO
open System.Text
open System.Text.Json.Nodes
open Microsoft.Data.Sqlite
open Microsoft.EntityFrameworkCore
open Softellect.Sys.Primitives
open OpticalConstructor.Database
open OpticalConstructor.Domain
open OpticalConstructor.Domain.CatalogueFiles
open OpticalConstructor.Domain.CatalogueImport
open OpticalConstructor.Domain.Facets
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.LibraryFacets
open OpticalConstructor.Domain.Lifecycle
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Storage.CatalogueFolderStore
open OpticalConstructor.Storage.CatalogueImportStore
open OpticalConstructor.Storage.CategoryDatabaseStore
open OpticalConstructor.Storage.Errors
open OpticalConstructor.Storage.EntryFileJson
open OpticalConstructor.Storage.LibraryBackupStore
open OpticalConstructor.Storage.LibraryDatabaseStore
open OpticalConstructor.Storage.MaterialDatabaseStore
open OpticalConstructor.Storage.PresetDatabaseStore
open OpticalConstructor.Storage.SampleDatabaseStore
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

    let private builtinEnvelope (entryId : LibraryEntryId) : LibraryEntryEnvelope =
        (BuiltinCatalogue.catalogueData ()).envelopes
        |> List.find (fun envelope -> envelope.content.id = entryId)

    let private expectStoredVersion (result : Result<StoredEntryVersion option, StorageError>) : StoredEntryVersion =
        match expectDatabaseOk result with
        | Some stored -> stored
        | None -> failwith "the expected library version is missing"

    let private activeStoredVersion (envelope : LibraryEntryEnvelope) : StoredEntryVersion =
        {
            envelope = envelope
            state =
                {
                    lifecycle = ActiveEntry
                    pin = VersionUnpinned
                }
        }

    let private renamedSiliconVersion () : StoredEntryVersion =
        let original = builtinEnvelope MaterialIds.silicon.toLibraryEntryId
        let content =
            {
                original.content with
                    version = original.content.version.next
                    name = EntryName "Silicon version two"
            }
        content.``sealed`` |> expectDatabaseOk |> activeStoredVersion

    let private facetRowIdentity (row : LibraryEntryFacetRow) : (LibraryEntryId * VersionNumber) * AttributeKey * DiscreteKey =
        let entryId = LibraryEntryId row.EntryId
        let version = VersionNumber row.Version
        let key = AttributeKey row.FacetKey
        let value = DiscreteKey row.FacetValue
        (entryId, version), key, value

    let private sampleMaterialRowIdentity (row : LibrarySampleMaterialRow) : (LibraryEntryId * VersionNumber) * MaterialVersionId =
        let sampleId = LibraryEntryId row.SampleId
        let sampleVersion = VersionNumber row.SampleVersion
        let materialVersion : MaterialVersionId =
            {
                materialId = MaterialId row.MaterialId
                version = VersionNumber row.MaterialVersion
            }
        (sampleId, sampleVersion), materialVersion

    let private envelopeFacetPairs (envelope : LibraryEntryEnvelope) : Set<AttributeKey * DiscreteKey> =
        let rows = envelope.facetRows |> expectDatabaseOk
        let pairs =
            rows
            |> List.map (fun row ->
                let entryVersion, key, value = facetRowIdentity row
                Assert.Equal<LibraryEntryId * VersionNumber>((envelope.content.id, envelope.content.version), entryVersion)
                key, value)
            |> Set.ofList
        Assert.Equal(rows.Length, pairs.Count)
        pairs

    let private envelopeMaterialVersions (envelope : LibraryEntryEnvelope) : Set<MaterialVersionId> =
        let rows = envelope.sampleMaterialRows |> expectDatabaseOk
        let versions =
            rows
            |> List.map (fun row ->
                let sampleVersion, materialVersion = sampleMaterialRowIdentity row
                Assert.Equal<LibraryEntryId * VersionNumber>((envelope.content.id, envelope.content.version), sampleVersion)
                materialVersion)
            |> Set.ofList
        Assert.Equal(rows.Length, versions.Count)
        versions

    let private assertRowDecodeRefusal (decoded : Result<'Entry, LibraryEntryError>) (rows : Result<'Rows, StorageError>) : unit =
        match decoded, rows with
        | Error decodeError, Error (LibraryRowInvalidErr reason) -> Assert.Equal(decodeError.describe, reason)
        | _, Error error -> Assert.Fail($"expected the decoder's row error, got {error.describe}")
        | _, Ok _ -> Assert.Fail("invalid entry content produced database rows")

    type private LibraryDatabaseFixture () =
        let folder = FolderName (Path.Combine(AppContext.BaseDirectory, "LibraryDatabaseTests", Guid.NewGuid().ToString("N")))
        let file = FileName (Path.Combine(folder.value, "library.db"))
        do Directory.CreateDirectory(folder.value) |> ignore

        member _.databaseFile : FileName = file

        member _.backupsFolder : FolderName = FolderName (Path.Combine(folder.value, "Backups"))

        member _.fileNamed (name : string) : FileName =
            FileName (Path.Combine(folder.value, name))

        member _.connectionStringFor (databaseFile : FileName) : ConnectionString =
            let builder = SqliteConnectionStringBuilder()
            builder.DataSource <- databaseFile.value
            builder.Pooling <- false
            ConnectionString builder.ConnectionString

        member this.connectionString : ConnectionString = this.connectionStringFor file

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
    let ``openOrCreate says whether it created the library database or opened an existing one`` () : unit =
        let memoryString = ConnectionString "Data Source=:memory:"
        use memory = LibraryDatabase.openOrCreate memoryString |> expectDatabaseOk
        Assert.Equal<LibraryDatabaseOrigin>(CreatedLibraryDatabase, memory.origin)
        assertLibraryStamp memory

        use fixture = new LibraryDatabaseFixture()

        do
            use created = LibraryDatabase.openOrCreate fixture.connectionString |> expectDatabaseOk
            Assert.Equal<LibraryDatabaseOrigin>(CreatedLibraryDatabase, created.origin)
            assertLibraryStamp created

        use opened = LibraryDatabase.openOrCreate fixture.connectionString |> expectDatabaseOk
        Assert.Equal<LibraryDatabaseOrigin>(OpenedLibraryDatabase, opened.origin)
        assertLibraryStamp opened

    [<Fact>]
    let ``openOrCreate creates the missing folder of a new library database file`` () : unit =
        use fixture = new LibraryDatabaseFixture()
        let nestedFile = fixture.fileNamed (Path.Combine("missing", "nested", "library.db"))
        let nestedString = fixture.connectionStringFor nestedFile
        Assert.False(Directory.Exists(Path.GetDirectoryName(nestedFile.value)))

        use database = LibraryDatabase.openOrCreate nestedString |> expectDatabaseOk
        Assert.True(File.Exists(nestedFile.value))
        Assert.Equal<LibraryDatabaseOrigin>(CreatedLibraryDatabase, database.origin)
        assertLibraryStamp database

        let obstruction = fixture.fileNamed "blocked"
        File.WriteAllText(obstruction.value, "preserved")
        let blockedFile = fixture.fileNamed (Path.Combine("blocked", "nested", "library.db"))
        let blockedString = fixture.connectionStringFor blockedFile

        match LibraryDatabase.openOrCreate blockedString with
        | Error (LibraryDatabaseExn _) -> ()
        | Error error -> Assert.Fail(error.describe)
        | Ok unexpected ->
            use unexpected = unexpected
            Assert.Fail("a file was accepted as the database's parent directory")

        Assert.Equal("preserved", File.ReadAllText(obstruction.value))

    [<Fact>]
    let ``recreate copies the old library file into the backups folder, then creates a new stamped library`` () : unit =
        use fixture = new LibraryDatabaseFixture()
        let backups = fixture.backupsFolder
        let time = LibraryBackupTime (DateTime(2026, 11, 6, 14, 30, 0, DateTimeKind.Utc))
        let now () : LibraryBackupTime = time
        let original = builtinEnvelope MaterialIds.silicon.toLibraryEntryId

        do
            use database = LibraryDatabase.openOrCreate fixture.connectionString |> expectDatabaseOk
            database.run (fun context ->
                context.stageNewEntry original.content |> expectDatabaseOk
                context.SaveChanges() |> ignore)
            |> expectDatabaseOk

        do
            use connection = new SqliteConnection(fixture.connectionString.value)
            connection.Open()
            use command = connection.CreateCommand()
            command.CommandText <- "UPDATE SchemaStamp SET SchemaVersion = 2 WHERE Id = 1;"
            Assert.Equal(1, command.ExecuteNonQuery())

        let incompatibleVersion = LibrarySchemaVersion 2
        assertSchemaMismatch (Some incompatibleVersion) fixture.connectionString
        let originalBytes = File.ReadAllBytes(fixture.databaseFile.value)
        let sidecars =
            [ "-wal"; "-shm"; "-journal" ]
            |> List.map (fun suffix -> FileName $"{fixture.databaseFile.value}{suffix}")
        for sidecar in sidecars do
            File.WriteAllText(sidecar.value, "old library sidecar")

        do
            use locked = new FileStream(fixture.databaseFile.value, FileMode.Open, FileAccess.Read, FileShare.None)
            match LibraryDatabase.recreate fixture.connectionString backups now with
            | Error (FileIoExn _) -> ()
            | Error error -> Assert.Fail(error.describe)
            | Ok unexpected ->
                use unexpected = unexpected
                Assert.Fail("recreation succeeded while the source could not be copied")

        Assert.Equal<byte[]>(originalBytes, File.ReadAllBytes(fixture.databaseFile.value))
        Assert.Empty(Directory.GetFiles(backups.value))
        for sidecar in sidecars do
            Assert.Equal("old library sidecar", File.ReadAllText(sidecar.value))

        do
            use recreated = LibraryDatabase.recreate fixture.connectionString backups now |> expectDatabaseOk
            Assert.Equal<LibraryDatabaseOrigin>(CreatedLibraryDatabase, recreated.origin)
            assertLibraryStamp recreated
            recreated.run (fun context ->
                Assert.Empty(context.EntryVersions)
                Assert.Empty(context.LatestVersions)
                Assert.Empty(context.CatalogueImports))
            |> expectDatabaseOk

        for sidecar in sidecars do
            Assert.False(File.Exists(sidecar.value), $"old sidecar remains: {sidecar.value}")

        let backup = FileName (Path.Combine(backups.value, "library-20261106-143000.db"))
        Assert.Equal<string>(backup.value, Assert.Single<string>(Directory.GetFiles(backups.value)))
        Assert.Equal<byte[]>(originalBytes, File.ReadAllBytes(backup.value))

        do
            let backupString = fixture.connectionStringFor backup
            use connection = new SqliteConnection(backupString.value)
            connection.Open()
            use command = connection.CreateCommand()
            command.CommandText <- "SELECT SchemaVersion FROM SchemaStamp WHERE Id = 1;"
            Assert.Equal(2L, unbox<int64> (command.ExecuteScalar()))
            command.CommandText <- "SELECT COUNT(*) FROM EntryVersion;"
            Assert.Equal(1L, unbox<int64> (command.ExecuteScalar()))

        do
            use recreatedAgain = LibraryDatabase.recreate fixture.connectionString backups now |> expectDatabaseOk
            Assert.Equal<LibraryDatabaseOrigin>(CreatedLibraryDatabase, recreatedAgain.origin)
            assertLibraryStamp recreatedAgain

        let numberedBackup = FileName (Path.Combine(backups.value, "library-20261106-143000-2.db"))
        Assert.Equal<Set<string>>(set [ backup.value; numberedBackup.value ], Directory.GetFiles(backups.value) |> Set.ofArray)
        Assert.Equal<byte[]>(originalBytes, File.ReadAllBytes(backup.value))

        use missing = new LibraryDatabaseFixture()
        use created =
            LibraryDatabase.recreate missing.connectionString missing.backupsFolder (fun () -> failwith "a missing library needs no backup time")
            |> expectDatabaseOk
        Assert.Equal<LibraryDatabaseOrigin>(CreatedLibraryDatabase, created.origin)
        assertLibraryStamp created
        Assert.False(Directory.Exists(missing.backupsFolder.value))

        use blocked = new LibraryDatabaseFixture()
        let obstruction = blocked.fileNamed "blocked"
        File.WriteAllText(obstruction.value, "preserved")
        let blockedFile = blocked.fileNamed (Path.Combine("blocked", "nested", "library.db"))
        let blockedString = blocked.connectionStringFor blockedFile

        match LibraryDatabase.recreate blockedString blocked.backupsFolder now with
        | Error (FileIoExn _) -> ()
        | Error error -> Assert.Fail($"expected FileIoExn for the blocked parent, got {error.describe}")
        | Ok unexpected ->
            use unexpected = unexpected
            Assert.Fail("recreation accepted a file as the database's parent directory")

        Assert.Equal("preserved", File.ReadAllText(obstruction.value))
        Assert.False(File.Exists(blockedFile.value))
        Assert.False(Directory.Exists(blocked.backupsFolder.value))

    [<Fact>]
    let ``recreate of an in-memory library creates a new one and copies nothing`` () : unit =
        use fixture = new LibraryDatabaseFixture()
        let memoryString = ConnectionString "Data Source=:memory:"
        let namedMemoryString = ConnectionString "Data Source=library-recreate-memory;Mode=Memory"

        for connectionString in [ memoryString; namedMemoryString ] do
            use database =
                LibraryDatabase.recreate connectionString fixture.backupsFolder (fun () -> failwith "an in-memory library needs no backup time")
                |> expectDatabaseOk
            Assert.Equal<LibraryDatabaseOrigin>(CreatedLibraryDatabase, database.origin)
            assertLibraryStamp database
            database.run (fun context -> Assert.Empty(context.EntryVersions)) |> expectDatabaseOk
            Assert.False(Directory.Exists(fixture.backupsFolder.value))

    [<Fact>]
    let ``vacuumInto writes a stamped copy of the library and refuses a target that exists`` () : unit =
        use fixture = new LibraryDatabaseFixture()
        use database = LibraryDatabase.openOrCreate fixture.connectionString |> expectDatabaseOk
        let expected = catalogueImportFixture ()
        database.run (fun context ->
            context.CatalogueImports.Add(expected) |> ignore
            context.SaveChanges() |> ignore)
        |> expectDatabaseOk

        let backup = fixture.fileNamed "library's backup.db"
        let backupString = fixture.connectionStringFor backup
        database.vacuumInto backup |> expectDatabaseOk

        let assertBackup () : unit =
            use copied = LibraryDatabase.openOrCreate backupString |> expectDatabaseOk
            Assert.Equal<LibraryDatabaseOrigin>(OpenedLibraryDatabase, copied.origin)
            assertLibraryStamp copied
            copied.run (fun context -> Assert.Single<LibraryCatalogueImportRow>(context.CatalogueImports))
            |> expectDatabaseOk
            |> assertCatalogueImport expected

        assertBackup ()
        let originalBytes = File.ReadAllBytes(backup.value)

        match database.vacuumInto backup with
        | Error (LibraryDatabaseExn _) -> ()
        | Error error -> Assert.Fail(error.describe)
        | Ok () -> Assert.Fail("VACUUM INTO overwrote an existing backup")

        Assert.Equal<byte[]>(originalBytes, File.ReadAllBytes(backup.value))
        assertBackup ()

        let emptyTarget = fixture.fileNamed "empty-backup.db"
        File.WriteAllText(emptyTarget.value, String.Empty)

        match database.vacuumInto emptyTarget with
        | Error (LibraryDatabaseExn _) -> ()
        | Error error -> Assert.Fail(error.describe)
        | Ok () -> Assert.Fail("VACUUM INTO overwrote an existing empty file")

        Assert.True(File.Exists(emptyTarget.value))
        Assert.Empty(File.ReadAllBytes(emptyTarget.value))

        let missingFolderTarget = fixture.fileNamed (Path.Combine("not-created", "backup.db"))
        match database.vacuumInto missingFolderTarget with
        | Error (LibraryDatabaseExn _) -> ()
        | Error error -> Assert.Fail(error.describe)
        | Ok () -> Assert.Fail("VACUUM INTO created its target directory")

        Assert.False(Directory.Exists(Path.GetDirectoryName(missingFolderTarget.value)))

    [<Fact>]
    let ``restoreFrom backs up the live library, then replaces it with a backup's under the same handle, and refuses another schema`` () : unit =
        use fixture = new LibraryDatabaseFixture()
        use database = LibraryDatabase.openOrCreate fixture.connectionString |> expectDatabaseOk
        let first = catalogueImportFixture ()
        database.run (fun context ->
            context.CatalogueImports.Add(first) |> ignore
            context.SaveChanges() |> ignore)
        |> expectDatabaseOk

        let backup = fixture.fileNamed "backup.db"
        database.vacuumInto backup |> expectDatabaseOk
        let second = catalogueImportFixture ()
        second.CatalogueId <- Guid.Parse("decc1067-3d8f-4323-a608-bcce72a33982")
        second.Folder <- "second-catalogue"
        database.run (fun context ->
            context.CatalogueImports.Add(second) |> ignore
            context.SaveChanges() |> ignore)
        |> expectDatabaseOk

        let liveConnection =
            database.run (fun context -> context.Database.GetDbConnection()) |> expectDatabaseOk
        let beforeRestore = fixture.fileNamed "before-restore.db"
        database.restoreFrom backup beforeRestore |> expectDatabaseOk

        let assertLiveRestored () : unit =
            database.run (fun context ->
                Assert.Same(liveConnection, context.Database.GetDbConnection())
                Assert.Single<LibraryCatalogueImportRow>(context.CatalogueImports))
            |> expectDatabaseOk
            |> assertCatalogueImport first
            assertLibraryStamp database

        assertLiveRestored ()
        let beforeRestoreString = fixture.connectionStringFor beforeRestore

        do
            use previous = LibraryDatabase.openOrCreate beforeRestoreString |> expectDatabaseOk
            Assert.Equal<LibraryDatabaseOrigin>(OpenedLibraryDatabase, previous.origin)
            assertLibraryStamp previous
            previous.run (fun context ->
                let rows = context.CatalogueImports |> Seq.toList
                Assert.Equal(2, rows.Length)
                rows |> List.find (fun row -> row.CatalogueId = first.CatalogueId) |> assertCatalogueImport first
                rows |> List.find (fun row -> row.CatalogueId = second.CatalogueId) |> assertCatalogueImport second)
            |> expectDatabaseOk

        let incompatible = fixture.fileNamed "incompatible.db"
        database.vacuumInto incompatible |> expectDatabaseOk
        let incompatibleString = fixture.connectionStringFor incompatible

        do
            use connection = new SqliteConnection(incompatibleString.value)
            connection.Open()
            use command = connection.CreateCommand()
            command.CommandText <- "UPDATE SchemaStamp SET SchemaVersion = 2 WHERE Id = 1;"
            command.ExecuteNonQuery() |> ignore

        let assertRestoreMismatch (source : FileName) (target : FileName) (foundOpt : LibrarySchemaVersion option) : unit =
            let sourceBytes = File.ReadAllBytes(source.value)
            match database.restoreFrom source target with
            | Error (LibrarySchemaMismatchErr info) ->
                Assert.Equal<LibrarySchemaVersion option>(foundOpt, info.foundOpt)
                Assert.Equal<LibrarySchemaVersion>(LibrarySchemaVersion.current, info.expected)
            | Error error -> Assert.Fail(error.describe)
            | Ok () -> Assert.Fail("restore accepted an incompatible schema")

            Assert.False(File.Exists(target.value))
            Assert.Equal<byte[]>(sourceBytes, File.ReadAllBytes(source.value))
            assertLiveRestored ()

        let rejectedTarget = fixture.fileNamed "rejected-before-restore.db"
        let incompatibleVersion = LibrarySchemaVersion 2
        assertRestoreMismatch incompatible rejectedTarget (Some incompatibleVersion)

        let unstamped = fixture.fileNamed "unstamped.db"
        database.vacuumInto unstamped |> expectDatabaseOk
        let unstampedString = fixture.connectionStringFor unstamped

        do
            use connection = new SqliteConnection(unstampedString.value)
            connection.Open()
            use command = connection.CreateCommand()
            command.CommandText <- "DELETE FROM SchemaStamp;"
            command.ExecuteNonQuery() |> ignore

        assertRestoreMismatch unstamped rejectedTarget None

        let unrelated = fixture.fileNamed "unrelated.db"
        let unrelatedString = fixture.connectionStringFor unrelated

        do
            use connection = new SqliteConnection(unrelatedString.value)
            connection.Open()
            use command = connection.CreateCommand()
            command.CommandText <- "CREATE TABLE Unrelated (Value TEXT NOT NULL);"
            command.ExecuteNonQuery() |> ignore

        assertRestoreMismatch unrelated rejectedTarget None

        let missing = fixture.fileNamed "missing.db"
        match database.restoreFrom missing rejectedTarget with
        | Error (LibraryDatabaseExn _) -> ()
        | Error error -> Assert.Fail(error.describe)
        | Ok () -> Assert.Fail("restore accepted a missing backup")

        Assert.False(File.Exists(missing.value))
        Assert.False(File.Exists(rejectedTarget.value))
        assertLiveRestored ()

        let emptyBeforeRestore = fixture.fileNamed "empty-before-restore.db"
        File.WriteAllText(emptyBeforeRestore.value, String.Empty)

        match database.restoreFrom beforeRestore emptyBeforeRestore with
        | Error (LibraryDatabaseExn _) -> ()
        | Error error -> Assert.Fail(error.describe)
        | Ok () -> Assert.Fail("restore accepted an existing empty safety-backup file")

        Assert.True(File.Exists(emptyBeforeRestore.value))
        Assert.Empty(File.ReadAllBytes(emptyBeforeRestore.value))
        assertLiveRestored ()

        let previousBytes = File.ReadAllBytes(beforeRestore.value)
        match database.restoreFrom beforeRestore beforeRestore with
        | Error (LibraryDatabaseExn _) -> ()
        | Error error -> Assert.Fail(error.describe)
        | Ok () -> Assert.Fail("restore replaced the library after its safety backup failed")

        Assert.Equal<byte[]>(previousBytes, File.ReadAllBytes(beforeRestore.value))
        assertLiveRestored ()

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
    let ``a material's facet rows hold its own discrete facets and key its category by id`` () : unit =
        let silicon = builtinEnvelope MaterialIds.silicon.toLibraryEntryId
        let siliconPairs = envelopeFacetPairs silicon
        let expectedSiliconPairs =
            [
                materialCategoryKey, DiscreteKey (string CategoryIds.semiconductor.value)
                materialAnisotropyKey, DiscreteKey "Isotropic"
                materialDispersionKey, DiscreteKey "Dispersive"
                materialTransparencyKey, DiscreteKey "Absorbing"
            ]

        for pair in expectedSiliconPairs do
            Assert.Contains<AttributeKey * DiscreteKey>(pair, siliconPairs)

        let siliconKeys = siliconPairs |> Set.map fst
        for key in [ materialGyrationClassKey; materialHandednessKey; materialMagneticKey ] do
            Assert.DoesNotContain<AttributeKey>(key, siliconKeys)

        let langasitePairs = builtinEnvelope MaterialIds.langasite.toLibraryEntryId |> envelopeFacetPairs
        let expectedLangasitePairs =
            [
                materialAnisotropyKey, DiscreteKey "Uniaxial"
                materialGyrationClassKey, DiscreteKey "Uniaxial 3 / 4 / 6 (g₁₁, g₃₃)"
                materialHandednessKey, DiscreteKey "Right-handed"
            ]

        for pair in expectedLangasitePairs do
            Assert.Contains<AttributeKey * DiscreteKey>(pair, langasitePairs)

        let category = builtinEnvelope CategoryIds.glass.toLibraryEntryId
        Assert.Empty(envelopeFacetPairs category)

        let invalid =
            {
                silicon with
                    content =
                        {
                            silicon.content with categoryOpt = None
                        }
            }
        assertRowDecodeRefusal invalid.content.toMaterialEntry invalid.facetRows

    [<Fact>]
    let ``an entry's facet rows are its kind and its own discrete facets`` () : unit =
        let assertEntryFacets (envelope : LibraryEntryEnvelope) (expected : (AttributeKey * DiscreteKey) list) : unit =
            Assert.Equal<Set<AttributeKey * DiscreteKey>>(Set.ofList expected, envelopeFacetPairs envelope)

        let sample = builtinEnvelope SampleIds.multilayerQwId.toLibraryEntryId
        assertEntryFacets sample
            [ entryKindFacetKey, DiscreteKey "Sample"; sampleHasThinFilmsKey, DiscreteKey "Has thin films" ]

        let presetEnvelopes =
            [ PresetIds.polarizerLp; PresetIds.source600; PresetIds.detectorIntensity ]
            |> List.map (fun presetId ->
                match presetId.tryToLibraryEntryId with
                | Some entryId -> builtinEnvelope entryId
                | None -> failwith $"preset '{presetId.value}' has no GUID identity")

        for envelope in presetEnvelopes do
            let expected =
                match envelope.content.schema.kind with
                | PolarizerEntryKind ->
                    [ entryKindFacetKey, DiscreteKey "Polarizer"; polarizerCategoryFacetKey, DiscreteKey "Linear" ]
                | SourceEntryKind -> [ entryKindFacetKey, DiscreteKey "Source" ]
                | DetectorEntryKind -> [ entryKindFacetKey, DiscreteKey "Detector" ]
                | _ -> failwith "the fixture is not a preset"
            assertEntryFacets envelope expected

            let invalid =
                {
                    envelope with
                        content =
                            {
                                envelope.content with payload = sample.content.payload
                            }
                }
            match envelope.content.schema.kind with
            | PolarizerEntryKind -> assertRowDecodeRefusal invalid.content.toPolarizerPreset invalid.facetRows
            | SourceEntryKind -> assertRowDecodeRefusal invalid.content.toSourcePreset invalid.facetRows
            | DetectorEntryKind -> assertRowDecodeRefusal invalid.content.toDetectorPreset invalid.facetRows
            | _ -> failwith "the fixture is not a preset"

        let invalidSample =
            {
                sample with
                    content =
                        {
                            sample.content with descriptionOpt = None
                        }
            }
        assertRowDecodeRefusal invalidSample.content.toSample invalidSample.facetRows

    [<Fact>]
    let ``a sample's material rows are every material version its structure references`` () : unit =
        let langasiteSilicon = builtinEnvelope SampleIds.langasiteSiliconId.toLibraryEntryId
        let expectedLangasiteSilicon =
            [ MaterialIds.langasite; MaterialIds.silicon ] |> List.map MaterialVersionId.firstOf |> Set.ofList
        Assert.Equal<Set<MaterialVersionId>>(expectedLangasiteSilicon, envelopeMaterialVersions langasiteSilicon)

        let multilayer = builtinEnvelope SampleIds.multilayerQwId.toLibraryEntryId
        let expectedMultilayer =
            [ MaterialIds.glass152; MaterialIds.vacuumMaterial ] |> List.map MaterialVersionId.firstOf |> Set.ofList
        Assert.Equal<Set<MaterialVersionId>>(expectedMultilayer, envelopeMaterialVersions multilayer)

        for envelope in (BuiltinCatalogue.catalogueData ()).envelopes do
            match envelope.content.schema.kind with
            | SampleEntryKind -> ()
            | _ -> Assert.Empty(envelopeMaterialVersions envelope)

        let nextMaterialVersion : MaterialVersionId =
            {
                materialId = MaterialIds.glass152
                version = VersionNumber.first.next
            }
        let nextPayload =
            match multilayer.content.payload with
            | SamplePayload payload ->
                let structure =
                    {
                        payload.structure with lower = Some nextMaterialVersion
                    }
                let nextPayloadData =
                    {
                        payload with structure = structure
                    }
                SamplePayload nextPayloadData
            | _ -> failwith "the multilayer fixture is not a sample"
        let nextMultilayer =
            {
                multilayer with
                    content =
                        {
                            multilayer.content with
                                version = VersionNumber.first.next
                                payload = nextPayload
                        }
            }
        Assert.Equal<Set<MaterialVersionId>>(
            Set.add nextMaterialVersion expectedMultilayer,
            envelopeMaterialVersions nextMultilayer)

        let invalid =
            {
                multilayer with
                    content =
                        {
                            multilayer.content with descriptionOpt = None
                        }
            }
        assertRowDecodeRefusal invalid.content.toSample invalid.sampleMaterialRows

    [<Fact>]
    let ``every built-in's facet and material rows are stored beside its version row`` () : unit =
        let connectionString = ConnectionString "Data Source=:memory:"
        use database = LibraryDatabase.openOrCreate connectionString |> expectDatabaseOk
        let envelopes = (BuiltinCatalogue.catalogueData ()).envelopes
        let versionsAndRows =
            envelopes
            |> List.map (fun envelope ->
                let stored : StoredEntryVersion =
                    {
                        envelope = envelope
                        state =
                            {
                                lifecycle = ActiveEntry
                                pin = VersionUnpinned
                            }
                    }
                stored.versionRow |> expectDatabaseOk,
                envelope.facetRows |> expectDatabaseOk,
                envelope.sampleMaterialRows |> expectDatabaseOk)

        let expectedFacets =
            versionsAndRows |> List.collect (fun (_, facets, _) -> facets) |> List.map facetRowIdentity |> Set.ofList
        let expectedMaterials =
            versionsAndRows |> List.collect (fun (_, _, materials) -> materials) |> List.map sampleMaterialRowIdentity |> Set.ofList
        Assert.NotEmpty(expectedFacets)
        Assert.NotEmpty(expectedMaterials)

        database.run (fun context ->
            for version, facets, materials in versionsAndRows do
                context.EntryVersions.Add(version) |> ignore
                context.EntryFacets.AddRange(facets)
                context.SampleMaterials.AddRange(materials)
            context.SaveChanges() |> ignore)
        |> expectDatabaseOk

        let assertStoredRows (expectedFacetRows : Set<(LibraryEntryId * VersionNumber) * AttributeKey * DiscreteKey>)
                             (expectedMaterialRows : Set<(LibraryEntryId * VersionNumber) * MaterialVersionId>)
                             (context : LibraryDbContext) : unit =
            let facets = context.EntryFacets.AsNoTracking() |> Seq.toList
            let materials = context.SampleMaterials.AsNoTracking() |> Seq.toList
            Assert.Equal(expectedFacetRows.Count, facets.Length)
            Assert.Equal(expectedMaterialRows.Count, materials.Length)
            Assert.Equal<Set<_>>(expectedFacetRows, facets |> List.map facetRowIdentity |> Set.ofList)
            Assert.Equal<Set<_>>(expectedMaterialRows, materials |> List.map sampleMaterialRowIdentity |> Set.ofList)

        database.run (fun context ->
            Assert.Equal(envelopes.Length, context.EntryVersions |> Seq.length)
            assertStoredRows expectedFacets expectedMaterials context)
        |> expectDatabaseOk

        let multilayer = builtinEnvelope SampleIds.multilayerQwId.toLibraryEntryId
        let removedVersion = multilayer.content.id, multilayer.content.version
        Assert.NotEmpty(expectedFacets |> Set.filter (fun (version, _, _) -> version = removedVersion))
        Assert.NotEmpty(expectedMaterials |> Set.filter (fun (version, _) -> version = removedVersion))

        database.run (fun context ->
            let version =
                context.EntryVersions
                |> Seq.find (fun row -> row.EntryId = multilayer.content.id.value && row.Version = multilayer.content.version.value)
            context.EntryVersions.Remove(version) |> ignore
            context.SaveChanges() |> ignore)
        |> expectDatabaseOk

        database.run (fun context ->
            Assert.Equal(envelopes.Length - 1, context.EntryVersions |> Seq.length)
            let remainingFacets = expectedFacets |> Set.filter (fun (version, _, _) -> version <> removedVersion)
            let remainingMaterials = expectedMaterials |> Set.filter (fun (version, _) -> version <> removedVersion)
            assertStoredRows remainingFacets remainingMaterials context)
        |> expectDatabaseOk

    [<Fact>]
    let ``a row its payload does not support is refused`` () : unit =
        let envelope = builtinEnvelope MaterialIds.silicon.toLibraryEntryId

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

    [<Fact>]
    let ``importFrom fills an empty library database through the database stores`` () : unit =
        let connectionString = ConnectionString "Data Source=:memory:"
        use database = LibraryDatabase.openOrCreate connectionString |> expectDatabaseOk
        let categories = CategoryProxy.createInDatabase database
        let materials = MaterialProxy.createInDatabase database
        let samples = SampleProxy.createInDatabase database
        let presets = LibraryEntryStore.createInDatabase database
        let proxy = CatalogueImportProxy.ofStores categories materials samples presets
        let importer = CatalogueImportContext.create proxy
        let catalogue = BuiltinCatalogue.catalogueData ()
        let rows = importer.importFrom (CatalogueProxy.createBuiltIn ()) |> CatalogueImportTestData.expectImportOk
        Assert.Equal(catalogue.envelopes.Length, rows.Length)

        database.run (fun context ->
            Assert.Empty(context.EntryVersions)
            Assert.Empty(context.LatestVersions))
        |> expectDatabaseOk

        rows |> List.map _.byDefault |> importer.apply |> CatalogueImportTestData.expectImportOk |> ignore

        for expected in BuiltinCatalogue.categories () do
            let actual = categories.listCategories () |> Result.map (List.tryFind (fun category -> category.id = expected.id))
            Assert.True((Ok (Some expected) = actual), "A category id must resolve to its decoded value.")
        for expected in BuiltinCatalogue.materials () do
            Assert.True((Ok (Some expected) = materials.tryGetMaterial expected.id), "A material id must resolve to its decoded value.")
        for expected in BuiltinCatalogue.samples () do
            Assert.True((Ok (Some expected) = samples.tryGetSample expected.id), "A sample id must resolve to its decoded value.")
        for expected in BuiltinCatalogue.presets () do
            Assert.True((Ok (Some expected) = presets.proxy.tryGetEntry expected.entryId), "A preset id must resolve to its decoded value.")

        database.run (fun context ->
            Assert.Equal(catalogue.envelopes.Length, context.EntryVersions |> Seq.length)
            Assert.Equal(catalogue.envelopes.Length, context.LatestVersions |> Seq.length)
            for kind in LibraryEntryKind.all do
                let expected =
                    catalogue.envelopes
                    |> List.filter (fun envelope -> envelope.content.schema.kind = kind)
                    |> List.map (fun envelope -> envelope.content.id, VersionNumber.first)
                    |> List.sort
                let actual =
                    context.latestStoredOfKind kind
                    |> expectDatabaseOk
                    |> List.map (fun stored -> stored.envelope.content.id, stored.envelope.content.version)
                    |> List.sort
                Assert.Equal<(LibraryEntryId * VersionNumber) list>(expected, actual))
        |> expectDatabaseOk

        let categoriesBefore = categories.listCategories ()
        let materialsBefore = materials.listMaterials IncludeInactive
        let samplesBefore = samples.listSamples IncludeInactive
        let readVersions () : StoredEntryVersion list =
            database.run (fun context ->
                context.EntryVersions |> Seq.map (fun row -> row.toStoredVersion |> expectDatabaseOk)
                |> Seq.sortBy (fun stored -> stored.envelope.content.id, stored.envelope.content.version)
                |> Seq.toList)
            |> expectDatabaseOk
        let versionsBefore = readVersions ()
        let reused = importer.importFrom (CatalogueProxy.createBuiltIn ()) |> CatalogueImportTestData.expectImportOk
        Assert.Equal(catalogue.envelopes.Length, reused.Length)
        for row in reused do
            match row.status with
            | SameContentHash _ -> Assert.Equal(ReuseHeldEntry, row.defaultAction)
            | _ -> Assert.Fail("Every held catalogue entry must be reused.")
        reused |> List.map _.byDefault |> importer.apply |> CatalogueImportTestData.expectImportOk |> ignore
        Assert.Equal(categoriesBefore, categories.listCategories ())
        Assert.Equal(materialsBefore, materials.listMaterials IncludeInactive)
        Assert.Equal(samplesBefore, samples.listSamples IncludeInactive)
        let versionsAfter = readVersions ()
        Assert.Equal<StoredEntryVersion list>(versionsBefore, versionsAfter)

    [<Fact>]
    let ``the fixture library database holds every Built-in entry as its version 1 and latest`` () : unit =
        use database = BuiltinCatalogue.freshLibraryDatabase ()
        let envelopes = (BuiltinCatalogue.catalogueData ()).envelopes
        let missingId = LibraryEntryId (Guid.Parse "863ac71b-76f7-4192-9647-e9dbebd563e4")

        database.run (fun context ->
            Assert.Equal(envelopes.Length, context.EntryVersions |> Seq.length)
            Assert.Equal(envelopes.Length, context.LatestVersions |> Seq.length)
            for kind in LibraryEntryKind.all do
                let expected = envelopes |> List.filter (fun envelope -> envelope.content.schema.kind = kind)
                let actual = context.latestStoredOfKind kind |> expectDatabaseOk
                Assert.Equal(expected.Length, actual.Length)
                Assert.Equal<Set<LibraryEntryId>>(
                    expected |> List.map (fun envelope -> envelope.content.id) |> Set.ofList,
                    actual |> List.map (fun stored -> stored.envelope.content.id) |> Set.ofList)
                for stored in actual do
                    Assert.Equal<VersionNumber>(VersionNumber.first, stored.envelope.content.version)
                    let envelope = expected |> List.find (fun envelope -> envelope.content.id = stored.envelope.content.id)
                    Assert.Equal<StoredEntryVersion>(activeStoredVersion envelope, stored)
                    Assert.Equal<StoredEntryVersion>(stored, context.tryLatestStored envelope.content.id |> expectStoredVersion)
            Assert.Equal<StoredEntryVersion option>(None, context.tryLatestStored missingId |> expectDatabaseOk)
            Assert.Equal<StoredEntryVersion option>(None, context.tryStoredVersion missingId VersionNumber.first |> expectDatabaseOk)
            Assert.Empty(context.storedVersionsOf missingId |> expectDatabaseOk))
        |> expectDatabaseOk

        let siliconId = MaterialIds.silicon.toLibraryEntryId
        database.run (fun context ->
            let row = context.EntryVersions |> Seq.find (fun row -> row.EntryId = siliconId.value)
            row.Name <- "Unsupported row name"
            context.SaveChanges() |> ignore)
        |> expectDatabaseOk

        let assertReadRefused (result : Result<'Outcome, StorageError>) : unit =
            match result with
            | Error (LibraryRowInvalidErr reason) -> Assert.Contains("column Name", reason)
            | Error error -> Assert.Fail(error.describe)
            | Ok _ -> Assert.Fail("a read accepted an entry row that disagrees with its payload")

        database.run (fun context ->
            context.tryStoredVersion siliconId VersionNumber.first |> assertReadRefused
            context.tryLatestStored siliconId |> assertReadRefused
            context.storedVersionsOf siliconId |> assertReadRefused
            context.latestStoredOfKind MaterialEntryKind |> assertReadRefused)
        |> expectDatabaseOk

    [<Fact>]
    let ``staging a version overwrites its row and its relational rows, and the latest names the staged version`` () : unit =
        use database = BuiltinCatalogue.freshLibraryDatabase ()
        let multilayer = builtinEnvelope SampleIds.multilayerQwId.toLibraryEntryId
        let replacement = builtinEnvelope SampleIds.langasiteSiliconId.toLibraryEntryId
        let content =
            match multilayer.content.payload, replacement.content.payload with
            | SamplePayload original, SamplePayload replacement ->
                let payload =
                    {
                        original with structure = replacement.structure
                    }
                {
                    multilayer.content with payload = SamplePayload payload
                }
            | _ -> failwith "the sample fixtures must have sample payloads"
        let staged =
            {
                envelope = content.``sealed`` |> expectDatabaseOk
                state =
                    {
                        lifecycle = InactiveEntry
                        pin = VersionPinned
                    }
            }
        let silicon = builtinEnvelope MaterialIds.silicon.toLibraryEntryId
        let next = renamedSiliconVersion ()

        database.run (fun context ->
            context.stageVersion staged |> expectDatabaseOk
            context.stageVersion (activeStoredVersion multilayer) |> expectDatabaseOk
            context.stageVersion staged |> expectDatabaseOk
            context.stageVersion next |> expectDatabaseOk
            context.stageLatest silicon.content.id next.envelope.content.version)
        |> expectDatabaseOk

        database.run (fun context ->
            Assert.Equal<StoredEntryVersion>(activeStoredVersion multilayer, context.tryStoredVersion content.id content.version |> expectStoredVersion)
            Assert.Equal<StoredEntryVersion>(activeStoredVersion silicon, context.tryLatestStored silicon.content.id |> expectStoredVersion)
            Assert.Equal<StoredEntryVersion option>(None, context.tryStoredVersion silicon.content.id next.envelope.content.version |> expectDatabaseOk))
        |> expectDatabaseOk

        database.run (fun context ->
            context.stageVersion staged |> expectDatabaseOk
            context.stageVersion (activeStoredVersion multilayer) |> expectDatabaseOk
            context.stageVersion staged |> expectDatabaseOk
            context.stageVersion next |> expectDatabaseOk
            context.SaveChanges() |> ignore)
        |> expectDatabaseOk

        database.run (fun context ->
            Assert.Equal<StoredEntryVersion>(staged, context.tryStoredVersion content.id content.version |> expectStoredVersion)
            let actualMaterials =
                context.SampleMaterials
                |> Seq.filter (fun row -> row.SampleId = content.id.value && row.SampleVersion = content.version.value)
                |> Seq.map sampleMaterialRowIdentity
                |> Set.ofSeq
            let expectedMaterials =
                [ MaterialIds.langasite; MaterialIds.silicon ]
                |> List.map (fun id -> (content.id, content.version), MaterialVersionId.firstOf id)
                |> Set.ofList
            Assert.Equal<Set<_>>(expectedMaterials, actualMaterials)
            let actualFacets =
                context.EntryFacets
                |> Seq.filter (fun row -> row.EntryId = content.id.value && row.Version = content.version.value)
                |> Seq.map facetRowIdentity
                |> Set.ofSeq
            let expectedFacets =
                staged.envelope.facetRows |> expectDatabaseOk |> List.map facetRowIdentity |> Set.ofList
            Assert.Equal<Set<_>>(expectedFacets, actualFacets)
            Assert.Equal<StoredEntryVersion>(activeStoredVersion silicon, context.tryLatestStored silicon.content.id |> expectStoredVersion)
            context.stageLatest silicon.content.id next.envelope.content.version
            context.SaveChanges() |> ignore)
        |> expectDatabaseOk

        database.run (fun context ->
            Assert.Equal<StoredEntryVersion>(next, context.tryLatestStored silicon.content.id |> expectStoredVersion)
            Assert.Equal<StoredEntryVersion>(activeStoredVersion silicon, context.tryStoredVersion silicon.content.id VersionNumber.first |> expectStoredVersion)
            let versions = context.storedVersionsOf silicon.content.id |> expectDatabaseOk
            Assert.Equal<StoredEntryVersion list>([ activeStoredVersion silicon; next ], versions))
        |> expectDatabaseOk

    [<Fact>]
    let ``stageEdit keeps, mutates or mints exactly as its decision says`` () : unit =
        use database = BuiltinCatalogue.freshLibraryDatabase ()
        let silicon = builtinEnvelope MaterialIds.silicon.toLibraryEntryId
        let latest =
            {
                envelope = silicon
                state =
                    {
                        lifecycle = InactiveEntry
                        pin = VersionPinned
                    }
            }
        database.run (fun context ->
            context.stageVersion latest |> expectDatabaseOk
            context.SaveChanges() |> ignore)
        |> expectDatabaseOk

        let edited =
            {
                silicon.content with
                    name = EntryName "Silicon metadata edit"
                    version = VersionNumber 19
            }
        database.run (fun context ->
            context.stageEdit latest KeepCurrent edited |> expectDatabaseOk
            Assert.False(context.ChangeTracker.HasChanges())
            Assert.Equal(0, context.SaveChanges()))
        |> expectDatabaseOk

        database.run (fun context ->
            Assert.Equal<StoredEntryVersion>(latest, context.tryLatestStored silicon.content.id |> expectStoredVersion)
            context.stageEdit latest MutateInPlace edited |> expectDatabaseOk
            context.SaveChanges() |> ignore)
        |> expectDatabaseOk

        let mutated =
            database.run (fun context -> context.tryLatestStored silicon.content.id)
            |> expectDatabaseOk
            |> expectStoredVersion
        let expectedContent =
            {
                edited with version = silicon.content.version
            }
        Assert.Equal<LibraryEntryEnvelope>(expectedContent.``sealed`` |> expectDatabaseOk, mutated.envelope)
        Assert.Equal<EntryVersionState>(latest.state, mutated.state)
        Assert.Equal<EntryProvenance>(silicon.content.provenance, mutated.envelope.content.provenance)

        let mintedContent =
            {
                edited with name = EntryName "Silicon minted edit"
            }
        let newId = LibraryEntryId (Guid.Parse "f1b1c516-77be-4d21-8d95-34d94569e1ce")
        let newContent =
            {
                silicon.content with
                    id = newId
                    name = EntryName "Test-local material"
                    provenance = UserProvenance
            }
        database.run (fun context ->
            context.stageEdit mutated MintNextVersion mintedContent |> expectDatabaseOk
            context.stageNewEntry newContent |> expectDatabaseOk
            context.SaveChanges() |> ignore)
        |> expectDatabaseOk

        database.run (fun context ->
            let minted = context.tryLatestStored silicon.content.id |> expectStoredVersion
            let expectedMinted =
                {
                    mintedContent with version = silicon.content.version.next
                }
            Assert.Equal<StoredEntryVersion>(expectedMinted.``sealed`` |> expectDatabaseOk |> activeStoredVersion, minted)
            Assert.Equal<StoredEntryVersion>(mutated, context.tryStoredVersion silicon.content.id VersionNumber.first |> expectStoredVersion)
            Assert.Equal<StoredEntryVersion list>([ mutated; minted ], context.storedVersionsOf silicon.content.id |> expectDatabaseOk)
            let created = context.tryLatestStored newId |> expectStoredVersion
            Assert.Equal<VersionNumber>(VersionNumber.first, created.envelope.content.version)
            Assert.Equal<StoredEntryVersion>(newContent.``sealed`` |> expectDatabaseOk |> activeStoredVersion, created))
        |> expectDatabaseOk

    [<Fact>]
    let ``staging a removal deletes every version of the entry and its relational rows`` () : unit =
        use database = BuiltinCatalogue.freshLibraryDatabase ()
        let siliconId = MaterialIds.silicon.toLibraryEntryId
        let next = renamedSiliconVersion ()
        database.run (fun context ->
            context.stageVersion next |> expectDatabaseOk
            context.stageLatest siliconId next.envelope.content.version
            context.PulledSources.Add(
                LibraryPulledSourceRow(
                    EntryId = siliconId.value,
                    Version = next.envelope.content.version.value,
                    Source = "fixture",
                    RecordPath = "silicon",
                    SourceVersion = "1",
                    Licence = "test",
                    Citation = "test record")) |> ignore
            context.SaveChanges() |> ignore)
        |> expectDatabaseOk

        database.run (fun context -> context.stageRemoval siliconId) |> expectDatabaseOk
        database.run (fun context ->
            Assert.Equal<StoredEntryVersion>(next, context.tryLatestStored siliconId |> expectStoredVersion)
            Assert.NotEmpty(context.EntryFacets |> Seq.filter (fun row -> row.EntryId = siliconId.value))
            Assert.Single<LibraryPulledSourceRow>(context.PulledSources) |> ignore
            context.stageRemoval siliconId
            context.SaveChanges() |> ignore)
        |> expectDatabaseOk

        database.run (fun context ->
            Assert.Equal<StoredEntryVersion option>(None, context.tryLatestStored siliconId |> expectDatabaseOk)
            Assert.Empty(context.storedVersionsOf siliconId |> expectDatabaseOk)
            Assert.Empty(context.EntryVersions |> Seq.filter (fun row -> row.EntryId = siliconId.value))
            Assert.Empty(context.LatestVersions |> Seq.filter (fun row -> row.EntryId = siliconId.value))
            Assert.Empty(context.EntryFacets |> Seq.filter (fun row -> row.EntryId = siliconId.value))
            Assert.Empty(context.PulledSources |> Seq.filter (fun row -> row.EntryId = siliconId.value))
            for envelope in (BuiltinCatalogue.catalogueData ()).envelopes do
                if envelope.content.id <> siliconId then
                    Assert.Equal<StoredEntryVersion>(activeStoredVersion envelope, context.tryLatestStored envelope.content.id |> expectStoredVersion)
            context.stageRemoval siliconId
            Assert.Equal(0, context.SaveChanges()))
        |> expectDatabaseOk

        let sampleId = SampleIds.multilayerQwId.toLibraryEntryId
        database.run (fun context ->
            Assert.NotEmpty(context.SampleMaterials |> Seq.filter (fun row -> row.SampleId = sampleId.value))
            context.stageRemoval sampleId
            context.SaveChanges() |> ignore)
        |> expectDatabaseOk

        database.run (fun context ->
            Assert.Empty(context.SampleMaterials |> Seq.filter (fun row -> row.SampleId = sampleId.value))
            Assert.Empty(context.EntryFacets |> Seq.filter (fun row -> row.EntryId = sampleId.value))
            Assert.Equal<StoredEntryVersion option>(None, context.tryLatestStored sampleId |> expectDatabaseOk))
        |> expectDatabaseOk
