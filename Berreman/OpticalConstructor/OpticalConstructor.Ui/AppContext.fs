namespace OpticalConstructor.Ui

open OpticalConstructor.Domain
open OpticalConstructor.Domain.CatalogueFiles
open OpticalConstructor.Domain.CatalogueImport
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.LibraryBackup
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.WorkbenchSettings
open OpticalConstructor.Storage.CatalogueFolderStore
open OpticalConstructor.Storage.CatalogueImportStore
open OpticalConstructor.Storage.CategoryDatabaseStore
open OpticalConstructor.Storage.Errors
open OpticalConstructor.Storage.LibraryBackupStore
open OpticalConstructor.Storage.LibraryDatabaseStore
open OpticalConstructor.Storage.MaterialDatabaseStore
open OpticalConstructor.Storage.PresetDatabaseStore
open OpticalConstructor.Storage.SampleDatabaseStore
open OpticalConstructor.Storage.SourceDatabaseStore
open Softellect.Sys.Logging

/// Shares the configured library database stores and session proxies across product windows.
/// Context identity is by reference; database identity comes from its configured connection.
[<ReferenceEquality>]
type AppContext =
    {
        /// The live database library combines presets, samples and user sources for selection and binding.
        library : Library.LibraryProxy
        /// The read-only experiment-template seam behind the Experiments bay.
        experiments : Experiments.ExperimentProxy
        /// The materials database store refuses removal while samples reference the material.
        materials : MaterialLibrary.MaterialProxy
        /// The samples database store.
        samples : Library.SampleProxy
        /// The user light-source database store behind the Library window's source editor.
        sources : Library.SourceProxy
        /// The category database store refuses removal while materials reference the category.
        categories : MaterialLibrary.CategoryProxy
        /// The library's backup store, Backups beside the database (spec 0050 001 §G).
        libraryBackups : LibraryBackupProxy
        /// The database import seam through which the Library window registers catalogue folders (spec 0050 S3.1).
        catalogueImports : CatalogueImportContext
        /// Startup catalogues whose complete row lists await confirmation (spec 0050 §3.7).
        pendingImports : PendingCatalogueImport list
        /// Spec 0038 Part L (037): the measured-data LOAD seam (STORE_XDUO_0006) —
        /// the real file-backed adapter (step 036, the one file-read seam §0.3c
        /// permits) behind the inverse flow's per-experiment data-file attach.
        experimentData : ExperimentData.ExperimentDataProxy
        /// Spec 0038 Part I/L (037): the experiment-collection persistence seam
        /// (STORE_XDUO_0005, step 029 in-memory `createInMemory`) — named collections
        /// of experiments save / list / load through it.
        experimentCollections : ExperimentCollectionStore.ExperimentCollectionProxy
        /// Spec 0038 (027, STORE_XDUO_0004): the scene persistence seam — the in-memory
        /// `Scene.SceneProxy.createInMemory` store (`Scene.fs`), built ONCE at the app scope
        /// beside the five stores and the two experiment seams above, so "every proxy is built
        /// exactly once at the root" holds (step 047 acceptance). The scene save/load surface
        /// that consumes it is a later cycle — the seam is composed here now exactly as
        /// `experimentCollections` was before its builder wired in; a future disk-backed `create`
        /// swaps in with no change to any consumer.
        scenes : Scene.SceneProxy
        /// Write each exported run as CSV in the configured exports folder (spec 0046; spec 0050 §3.9).
        /// No file is written until an export is requested.
        experimentExport : ExperimentExport.ExperimentExportProxy
        /// The step-005 typed appsettings.json values (`AppConfig.loadWorkbenchSettings`
        /// elevates them once at the composition root); later Part C/E/F steps read the
        /// window-policy / quick-pick / tree / bucketing fields from HERE, never from
        /// the provider.
        settings : WorkbenchSettings
    }

    static member private compose (locations : DataLocations) (settings : WorkbenchSettings) (database : LibraryDatabase) : AppContext =
        let materials = MaterialProxy.createInDatabase database
        let samples = SampleProxy.createInDatabase database
        let sources = SourceProxy.createInDatabase database
        let categories = CategoryProxy.createInDatabase database
        let catalogueImports = CatalogueImportContext.create (CatalogueImportProxy.createInDatabase database)
        let pendingImports =
            match catalogueImports.importOnStart (CatalogueProxy.builtInFolderUnder locations.shippedCatalogueFolder) with
            | Error e ->
                Logger.logError $"AppContext.compose: could not import catalogues on start: {e.describe}"
                []
            | Ok report ->
                for folder in report.folders do
                    match folder.outcome with
                    | Error e -> Logger.logError $"AppContext.compose: could not import catalogue folder '{folder.folder.value}': {e.describe}"
                    | Ok _ -> ()
                report.pending

        {
            library = LibraryProxy.createInDatabase database
            experiments = Experiments.createInMemoryExperiments ()
            materials = materials
            samples = samples
            sources = sources
            categories = categories
            libraryBackups = LibraryBackupProxy.createInDatabase database locations.backupsFolder (fun () -> LibraryBackupTime System.DateTime.Now)
            catalogueImports = catalogueImports
            pendingImports = pendingImports
            experimentData = OpticalConstructor.Storage.ExperimentDataStore.createFileBacked ()
            experimentCollections = ExperimentCollectionStore.ExperimentCollectionProxy.createInMemory ()
            scenes = Scene.SceneProxy.createInMemory ()
            experimentExport =
                OpticalConstructor.Storage.ExperimentExportFile.createCsvFolder
                    locations.exportsFolder
                    (fun () -> OpticalConstructor.Storage.ExperimentExportFile.ExportTime System.DateTime.Now)
            settings = settings
        }

    /// Every start imports the shipped catalogue and registered folders by manifest hash, pending confirmation (spec 0050 S2.4; S3.1).
    static member create (locations : DataLocations) (settings : WorkbenchSettings) : Result<AppContext, StorageError> =
        LibraryDatabase.openOrCreate locations.libraryConnection
        |> Result.map (AppContext.compose locations settings)

    /// Backs up and recreates; every start imports shipped and registered catalogues by manifest hash, pending confirmation (spec 0050 001 §C; S3.1).
    static member recreateLibrary (locations : DataLocations) (settings : WorkbenchSettings) : Result<AppContext, StorageError> =
        LibraryDatabase.recreate locations.libraryConnection locations.backupsFolder (fun () -> LibraryBackupTime System.DateTime.Now)
        |> Result.map (AppContext.compose locations settings)
