namespace OpticalConstructor.Ui

open OpticalConstructor.Domain
open OpticalConstructor.Domain.CatalogueFiles
open OpticalConstructor.Domain.CatalogueImport
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.WorkbenchSettings
open OpticalConstructor.Storage.CatalogueFolderStore
open OpticalConstructor.Storage.CategoryDatabaseStore
open OpticalConstructor.Storage.Errors
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

    /// Opens or creates the configured library database and imports the Built-in catalogue into a new one.
    /// Answers open failures unchanged; import failures are logged, retaining prior imports (001 §C; spec 0050 S2.4).
    static member create (locations : DataLocations) (settings : WorkbenchSettings) : Result<AppContext, StorageError> =
        LibraryDatabase.openOrCreate locations.libraryConnection
        |> Result.map (fun database ->
            let materials = MaterialProxy.createInDatabase database
            let samples = SampleProxy.createInDatabase database
            let sources = SourceProxy.createInDatabase database
            let categories = CategoryProxy.createInDatabase database
            match database.origin with
            | CreatedLibraryDatabase ->
                let importer = CatalogueImportProxy.ofStores categories materials samples (LibraryEntryStore.createInDatabase database)
                match importer.importFrom (CatalogueProxy.createBuiltInUnder locations.shippedCatalogueFolder) with
                | Ok () -> ()
                | Error e -> Logger.logError $"AppContext.create: could not import the Built-in catalogue: {e.describe}"
            | OpenedLibraryDatabase -> ()

            {
                library = LibraryProxy.createInDatabase database
                experiments = Experiments.createInMemoryExperiments ()
                materials = materials
                samples = samples
                sources = sources
                categories = categories
                experimentData = OpticalConstructor.Storage.ExperimentDataStore.createFileBacked ()
                experimentCollections = ExperimentCollectionStore.ExperimentCollectionProxy.createInMemory ()
                scenes = Scene.SceneProxy.createInMemory ()
                experimentExport =
                    OpticalConstructor.Storage.ExperimentExportFile.createCsvFolder
                        locations.exportsFolder
                        (fun () -> OpticalConstructor.Storage.ExperimentExportFile.ExportTime System.DateTime.Now)
                settings = settings
            })
