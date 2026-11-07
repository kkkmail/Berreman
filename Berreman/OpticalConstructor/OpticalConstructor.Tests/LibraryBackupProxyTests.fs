namespace OpticalConstructor.Tests

open Xunit
open OpticalConstructor.Domain.LibraryBackup

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
