namespace OpticalConstructor.Ui.Tests

open System.IO
open Softellect.Sys.Primitives
open OpticalConstructor.Domain.WorkbenchSettings
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

    /// Compose a fresh app scope over the owner's test locations (spec 0050 .spec-md §3.9).
    let freshAppContext (owner : FolderName) : AppContext =
        AppContext.create (testLocations owner) WorkbenchSettings.defaults
