namespace OpticalConstructor.Tests

open System
open System.IO
open Microsoft.Data.Sqlite
open Softellect.Sys.Primitives
open Xunit
open OpticalConstructor.Domain
open OpticalConstructor.Domain.CatalogueFiles
open OpticalConstructor.Domain.CatalogueImport
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.LibraryBackup
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Storage.CatalogueFolderStore
open OpticalConstructor.Storage.CatalogueImportStore
open OpticalConstructor.Storage.CategoryDatabaseStore
open OpticalConstructor.Storage.Errors
open OpticalConstructor.Storage.LibraryBackupStore
open OpticalConstructor.Storage.LibraryDatabaseStore
open OpticalConstructor.Storage.MaterialDatabaseStore
open OpticalConstructor.Storage.PresetDatabaseStore
open OpticalConstructor.Storage.SampleDatabaseStore

/// Backup stores list all backups and preserve a safety backup before restoring (spec 0050 001 §C; .spec-md §10.5).
[<AbstractClass>]
type LibraryBackupProxyContract () =

    /// Returns a fresh store holding no backup.
    abstract freshBackups : unit -> LibraryBackupProxy

    static member private requireBackupName (result : Result<LibraryBackupName, LibraryBackupError>) : LibraryBackupName =
        match result with
        | Ok name -> name
        | Error error -> failwith error.describe

    [<Fact>]
    member this.``a new backup store lists no backup`` () : unit =
        let store = this.freshBackups ()
        Assert.Equal<Result<LibraryBackupName list, LibraryBackupError>>(Ok [], store.listBackups ())

    [<Fact>]
    member this.``each backup is listed, newest first`` () : unit =
        let store = this.freshBackups ()
        let first = store.backUpLibrary () |> LibraryBackupProxyContract.requireBackupName
        let second = store.backUpLibrary () |> LibraryBackupProxyContract.requireBackupName

        Assert.NotEqual<LibraryBackupName>(first, second)
        Assert.Equal<Result<LibraryBackupName list, LibraryBackupError>>(Ok [ second; first ], store.listBackups ())

    [<Fact>]
    member this.``restoring a listed backup first backs up the current library and answers that backup`` () : unit =
        let store = this.freshBackups ()
        let first = store.backUpLibrary () |> LibraryBackupProxyContract.requireBackupName
        let before = store.restoreLibrary first |> LibraryBackupProxyContract.requireBackupName

        Assert.NotEqual<LibraryBackupName>(first, before)
        Assert.Equal<Result<LibraryBackupName list, LibraryBackupError>>(Ok [ before; first ], store.listBackups ())

    [<Fact>]
    member this.``restoring an unknown backup is UnknownLibraryBackupErr and takes no backup`` () : unit =
        let store = this.freshBackups ()
        let first = store.backUpLibrary () |> LibraryBackupProxyContract.requireBackupName
        let unknown = LibraryBackupName "no-such-backup"

        match store.restoreLibrary unknown with
        | Error (UnknownLibraryBackupErr reason as error) ->
            let description = error.describe
            Assert.Contains(unknown.value, reason)
            Assert.Contains("library backup is not listed", description)
            Assert.Contains(reason, description)
            Assert.Contains(unknown.value, description)
        | Error error -> Assert.Fail($"Expected UnknownLibraryBackupErr, got {error.describe}")
        | Ok _ -> Assert.Fail("Restoring an unknown backup must be refused.")

        Assert.Equal<Result<LibraryBackupName list, LibraryBackupError>>(Ok [ first ], store.listBackups ())

type LibraryBackupProxyTests () =
    inherit LibraryBackupProxyContract ()

    override _.freshBackups () : LibraryBackupProxy = LibraryBackupProxy.createInMemory ()

type LibraryBackupProxySqliteTests () =
    inherit LibraryBackupProxyContract ()

    let folder = FolderName (Path.Combine(AppContext.BaseDirectory, "LibraryBackupProxyTests", Guid.NewGuid().ToString("N")))
    let backups = FolderName (Path.Combine(folder.value, "Backups"))
    let databases = ResizeArray<LibraryDatabase>()
    let takenAt = LibraryBackupTime (DateTime(2026, 11, 6, 12, 34, 56, DateTimeKind.Utc))

    static member private requireBackup (result : Result<LibraryBackupName, LibraryBackupError>) : LibraryBackupName =
        result |> Result.defaultWith (fun error -> failwith error.describe)

    member private _.openDatabase (file : FileName) : LibraryDatabase =
        let builder = SqliteConnectionStringBuilder()
        builder.DataSource <- file.value
        builder.Pooling <- false
        let connectionString = ConnectionString builder.ConnectionString
        let database =
            LibraryDatabase.openOrCreate connectionString
            |> Result.defaultWith (fun error -> failwith error.describe)
        databases.Add database
        database

    member private this.createBackups () : LibraryDatabase * LibraryBackupProxy =
        let file = FileName (Path.Combine(folder.value, "library.db"))
        let database = this.openDatabase file
        database, LibraryBackupProxy.createInDatabase database backups (fun () -> takenAt)

    member private _.backupFile (name : LibraryBackupName) : FileName =
        FileName (Path.Combine(backups.value, name.value))

    member private this.openBackup (name : LibraryBackupName) : SampleProxy =
        let database = this.openDatabase (this.backupFile name)
        Assert.Equal<LibraryDatabaseOrigin>(OpenedLibraryDatabase, database.origin)
        SampleProxy.createInDatabase database

    static member private importLibrary (database : LibraryDatabase) : SampleProxy =
        let samples = SampleProxy.createInDatabase database
        let importer =
            CatalogueImportProxy.createInDatabase database
        let context = CatalogueImportContext.create importer
        let rows = context.importFrom (CatalogueProxy.createBuiltIn ()) |> Result.defaultWith (fun error -> failwith error.describe)
        Assert.Equal<Result<Sample list, SampleError>>(Ok [], samples.listSamples IncludeInactive)
        rows |> List.map _.byDefault |> context.apply |> Result.defaultWith (fun error -> failwith error.describe) |> ignore
        samples

    static member private newUserSample () : Sample =
        {
            BuiltinCatalogue.sample SampleIds.glassPlate1mmId with
                id = newSampleId ()
                name = EntryName "User plate after backup"
        }

    static member private assertBuiltInSamples (samples : SampleProxy) : unit =
        for sample in BuiltinCatalogue.samples () do
            Assert.Equal<Result<Sample option, SampleError>>(Ok (Some sample), samples.tryGetSample sample.id)

    override this.freshBackups () : LibraryBackupProxy = this.createBackups () |> snd

    [<Fact>]
    member this.``a backup is a library database holding the library as it was when taken`` () : unit =
        let database, store = this.createBackups ()
        let samples = LibraryBackupProxySqliteTests.importLibrary database
        let backup = store.backUpLibrary () |> LibraryBackupProxySqliteTests.requireBackup
        let userSample = LibraryBackupProxySqliteTests.newUserSample ()
        Assert.Equal<Result<unit, SampleError>>(Ok (), samples.saveSample userSample)
        Assert.Equal<Result<Sample option, SampleError>>(Ok (Some userSample), samples.tryGetSample userSample.id)

        let savedSamples = this.openBackup backup
        LibraryBackupProxySqliteTests.assertBuiltInSamples savedSamples
        Assert.Equal<Result<Sample option, SampleError>>(Ok None, savedSamples.tryGetSample userSample.id)

    [<Fact>]
    member this.``restoring through the backup store brings the backup's library back to every store over the same database`` () : unit =
        let database, store = this.createBackups ()
        let samples = LibraryBackupProxySqliteTests.importLibrary database
        let otherSamples = SampleProxy.createInDatabase database
        let library = LibraryProxy.createInDatabase database
        let backup = store.backUpLibrary () |> LibraryBackupProxySqliteTests.requireBackup
        let userSample = LibraryBackupProxySqliteTests.newUserSample ()
        Assert.Equal<Result<unit, SampleError>>(Ok (), samples.saveSample userSample)
        Assert.Equal<Result<Sample option, SampleError>>(Ok (Some userSample), otherSamples.tryGetSample userSample.id)
        Assert.Equal<Result<LibraryEntry option, LibraryError>>(Ok (Some (SampleItem userSample)), library.tryGetEntry userSample.id.toElementValueId)

        let before = store.restoreLibrary backup |> LibraryBackupProxySqliteTests.requireBackup

        Assert.NotEqual<LibraryBackupName>(backup, before)
        Assert.Equal<Result<LibraryBackupName list, LibraryBackupError>>(Ok [ before; backup ], store.listBackups ())
        for liveSamples in [ samples; otherSamples ] do
            LibraryBackupProxySqliteTests.assertBuiltInSamples liveSamples
            Assert.Equal<Result<Sample option, SampleError>>(Ok None, liveSamples.tryGetSample userSample.id)
        Assert.Equal<Result<LibraryEntry option, LibraryError>>(Ok None, library.tryGetEntry userSample.id.toElementValueId)
        for sample in BuiltinCatalogue.samples () do
            Assert.Equal<Result<LibraryEntry option, LibraryError>>(Ok (Some (SampleItem sample)), library.tryGetEntry sample.id.toElementValueId)

        let previousSamples = this.openBackup before
        LibraryBackupProxySqliteTests.assertBuiltInSamples previousSamples
        Assert.Equal<Result<Sample option, SampleError>>(Ok (Some userSample), previousSamples.tryGetSample userSample.id)

    [<Fact>]
    member this.``a backup of another schema is refused with LibraryRestoreFailedErr and changes nothing`` () : unit =
        let database, store = this.createBackups ()
        let samples = LibraryBackupProxySqliteTests.importLibrary database
        let backup = store.backUpLibrary () |> LibraryBackupProxySqliteTests.requireBackup
        let userSample = LibraryBackupProxySqliteTests.newUserSample ()
        Assert.Equal<Result<unit, SampleError>>(Ok (), samples.saveSample userSample)
        let backupFile = this.backupFile backup
        let foreignVersion = LibrarySchemaVersion 2

        do
            let builder = SqliteConnectionStringBuilder()
            builder.DataSource <- backupFile.value
            builder.Pooling <- false
            use connection = new SqliteConnection(builder.ConnectionString)
            connection.Open()
            use command = connection.CreateCommand()
            command.CommandText <- "UPDATE SchemaStamp SET SchemaVersion = $version WHERE Id = 1;"
            command.Parameters.AddWithValue("$version", foreignVersion.value) |> ignore
            Assert.Equal(1, command.ExecuteNonQuery())

        let originalBytes = File.ReadAllBytes(backupFile.value)
        match store.restoreLibrary backup with
        | Error (LibraryRestoreFailedErr reason) ->
            Assert.Contains($"schema version {foreignVersion.value}", reason)
            Assert.Contains($"expected schema version {LibrarySchemaVersion.current.value}", reason)
        | Error error -> Assert.Fail($"Expected LibraryRestoreFailedErr, got {error.describe}")
        | Ok _ -> Assert.Fail("Restoring a backup of another schema must be refused.")

        Assert.Equal<Result<LibraryBackupName list, LibraryBackupError>>(Ok [ backup ], store.listBackups ())
        Assert.Single(Directory.GetFiles(backups.value)) |> ignore
        Assert.Equal<byte[]>(originalBytes, File.ReadAllBytes(backupFile.value))
        LibraryBackupProxySqliteTests.assertBuiltInSamples samples
        Assert.Equal<Result<Sample option, SampleError>>(Ok (Some userSample), samples.tryGetSample userSample.id)

    [<Fact>]
    member this.``backups taken in the same second get numbered names and only backup files are listed`` () : unit =
        let database, store = this.createBackups ()
        let first = store.backUpLibrary () |> LibraryBackupProxySqliteTests.requireBackup
        let second = store.backUpLibrary () |> LibraryBackupProxySqliteTests.requireBackup
        let third = store.backUpLibrary () |> LibraryBackupProxySqliteTests.requireBackup
        let expectedFirst = LibraryBackupName "library-20261106-123456.db"
        let expectedSecond = LibraryBackupName "library-20261106-123456-2.db"
        let expectedThird = LibraryBackupName "library-20261106-123456-3.db"
        Assert.Equal<LibraryBackupName>(expectedFirst, first)
        Assert.Equal<LibraryBackupName>(expectedSecond, second)
        Assert.Equal<LibraryBackupName>(expectedThird, third)

        for name in [ "notes.txt"; "library-20261106-123456-1.db"; "library-20261106-123456-02.db"; "library-20261306-123456.db"; "library-20261106-123456.db-wal" ] do
            let file = FileName (Path.Combine(backups.value, name))
            File.WriteAllText(file.value, "not a backup")
        let directory = FolderName (Path.Combine(backups.value, "library-20261106-123459.db"))
        Directory.CreateDirectory(directory.value) |> ignore
        Assert.Equal<Result<LibraryBackupName list, LibraryBackupError>>(Ok [ third; second; first ], store.listBackups ())

        let additional = [ for _ in 4 .. 10 -> store.backUpLibrary () |> LibraryBackupProxySqliteTests.requireBackup ]
        let tenth = LibraryBackupName "library-20261106-123456-10.db"
        Assert.Equal<LibraryBackupName>(tenth, List.last additional)
        let olderTime = LibraryBackupTime (DateTime(2026, 11, 5, 12, 34, 56, DateTimeKind.Utc))
        let olderStore = LibraryBackupProxy.createInDatabase database backups (fun () -> olderTime)
        let older = olderStore.backUpLibrary () |> LibraryBackupProxySqliteTests.requireBackup
        let newerTime = LibraryBackupTime (DateTime(2026, 11, 7, 12, 34, 56, DateTimeKind.Utc))
        let newerStore = LibraryBackupProxy.createInDatabase database backups (fun () -> newerTime)
        let newer = newerStore.backUpLibrary () |> LibraryBackupProxySqliteTests.requireBackup
        let expected = newer :: (List.rev additional @ [ third; second; first; older ])
        Assert.Equal<Result<LibraryBackupName list, LibraryBackupError>>(Ok expected, store.listBackups ())

    [<Fact>]
    member this.``a database failure below the backup store is a typed backup error, never a throw`` () : unit =
        let database, store = this.createBackups ()
        let backup = store.backUpLibrary () |> LibraryBackupProxySqliteTests.requireBackup
        (database :> IDisposable).Dispose()

        match store.backUpLibrary () with
        | Error (LibraryBackupFailedErr reason) -> Assert.False(String.IsNullOrWhiteSpace reason)
        | Error error -> Assert.Fail($"Expected LibraryBackupFailedErr, got {error.describe}")
        | Ok _ -> Assert.Fail("Backing up a disposed database must fail.")

        match store.restoreLibrary backup with
        | Error (LibraryRestoreFailedErr reason) -> Assert.False(String.IsNullOrWhiteSpace reason)
        | Error error -> Assert.Fail($"Expected LibraryRestoreFailedErr, got {error.describe}")
        | Ok _ -> Assert.Fail("Restoring a disposed database must fail.")

        Assert.Equal<Result<LibraryBackupName list, LibraryBackupError>>(Ok [ backup ], store.listBackups ())

    interface IDisposable with
        member _.Dispose () : unit =
            for database in databases do
                (database :> IDisposable).Dispose()
            if Directory.Exists(folder.value) then Directory.Delete(folder.value, true)
