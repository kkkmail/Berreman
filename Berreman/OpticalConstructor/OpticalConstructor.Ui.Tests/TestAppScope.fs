namespace OpticalConstructor.Ui.Tests

open System.IO
open Softellect.Sys.Primitives
open OpticalConstructor.Domain.CatalogueImport
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.KitFolders
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.WorkbenchSettings
open OpticalConstructor.Ui.KitOpening
open OpticalConstructor.Ui

module TestAppScope =

    /// Isolate each owner's locations under the test output (spec 0050 .spec-md §3.9).
    let testLocations (owner : FolderName) : DataLocations =
        let root = FolderName (Path.Combine(System.AppContext.BaseDirectory, "TestAppScope", owner.value))
        let install = FolderName System.AppContext.BaseDirectory
        let backups = FolderName "Backups"
        let preferences = FolderName "Preferences"
        let kits = FolderName "Kits"
        let exports = FolderName "Exports"
        let catalogues = FolderName "Catalogues"

        {
            libraryConnection = ConnectionString "Data Source=:memory:"
            backupsFolder = root.combine backups
            preferencesFolder = root.combine preferences
            kitsFolder = root.combine kits
            exportsFolder = root.combine exports
            cataloguesFolder = root.combine catalogues
            shippedCatalogueFolder = install.combine DataLocations.shippedCatalogueDefault
        }

    /// Isolate writable locations and a file library beneath a fresh owner folder (spec 0050 S2.4).
    let fileLocations (owner : FolderName) : DataLocations =
        let instance = FolderName (System.Guid.NewGuid().ToString("N"))
        let locations = testLocations (owner.combine instance)
        let root = Path.GetDirectoryName locations.backupsFolder.value
        let databaseFile = FileName (Path.Combine(root, "library.db"))

        {
            locations with
                libraryConnection = ConnectionString $"Data Source={databaseFile.value}"
        }

    /// Explicitly confirms every pending catalogue's complete default choice list in populated fixtures (spec 0050 S3.1).
    let settlePendingImports (context : AppContext) : unit =
        for pending in context.pendingImports do
            let choices = pending.decisions |> List.map _.byDefault
            context.catalogueImports.settle pending choices
            |> Result.defaultWith (fun error -> failwith error.describe) |> ignore

    /// Compose a fresh database app scope and explicitly confirm every startup choice (spec 0050 S2.4; S3.1).
    let freshAppContext (owner : FolderName) : AppContext =
        let context =
            AppContext.create (testLocations owner) WorkbenchSettings.defaults
            |> Result.defaultWith (fun error -> failwith error.describe)
        settlePendingImports context
        context

    /// Returns an uncreated GUID-named kit folder under the owner's test output (spec 0050 .spec-md §3.10).
    let freshKitFolder (owner : FolderName) : KitFolder =
        let instance = FolderName (System.Guid.NewGuid().ToString("N"))
        let folder = (testLocations owner).kitsFolder.combine instance
        KitFolder folder

    /// Answers the folder picker immediately with the supplied test kit (spec 0050 S4.3).
    let kitPickerOf (folder : KitFolder) : KitFolderPicker =
        {
            pickKitFolder = fun () -> async { return Some folder }
        }

    /// Opens a fresh read-write test kit, failing the calling fact with its error (spec 0050 .spec-md §3.10).
    let openedKitIn (owner : FolderName) (imports : CatalogueImportContext) : OpenedKit =
        let author = KitAuthor "test"
        KitOpening.openKitAt (freshKitFolder owner) ReadWriteKitAccess ClockProvider.system author imports
        |> Result.defaultWith (fun error -> failwith error.describe)
