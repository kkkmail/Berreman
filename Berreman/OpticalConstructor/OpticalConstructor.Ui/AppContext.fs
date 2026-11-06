/// Spec 0038 Part C (step 006): the ONE app-scope composition value. The five domain
/// proxies used to be created INSIDE `MainConstructorWindow`'s constructor
/// (`…App/Program.fs`), so a second window could never share the stores; now they are
/// built once at startup — before any window opens — bundled with the step-005
/// `WorkbenchSettings`, and injected into the launcher, the Main workbench window,
/// and every window opened later, so launcher-opened and constructor-opened windows
/// observe the SAME in-memory stores. The parameterless `DefaultStores` composition
/// (`TableAndElementRotationView.fs`) stays the test-scene default; this record is
/// the product composition.
namespace OpticalConstructor.Ui

open OpticalConstructor.Domain
open OpticalConstructor.Domain.CatalogueFiles
open OpticalConstructor.Domain.CatalogueImport
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.WorkbenchSettings
open OpticalConstructor.Storage.CatalogueFolderStore
open Softellect.Sys.Logging

/// The app-scope composition context (the `*Context` convention: the IO-boundary
/// proxies plus the app-configuration data, constructed once at the composition
/// root). The proxy fields are `[<ReferenceEquality>]` function records, and ONE
/// context IS the app scope, so the context itself compares by reference — two
/// structurally-similar contexts are two different scopes with independent stores.
[<ReferenceEquality>]
type AppContext =
    {
        /// The Select bay's live library (spec 0027; spec 0046; spec 0050 §3.4) composes
        /// imported presets with the samples and user-sources stores, so
        /// an entry created in the Library window binds and resolves on the table.
        library : Library.LibraryProxy
        /// The read-only experiment-template seam behind the Experiments bay.
        experiments : Experiments.ExperimentProxy
        /// The materials WRITE store (STORE_XDUO_0001) — its remove-block consults
        /// the LIVE `samples` store below through `samplesReferencing`.
        materials : MaterialLibrary.MaterialProxy
        /// The samples WRITE store (STORE_XDUO_0002).
        samples : Library.SampleProxy
        /// Spec 0046: the USER light-source WRITE store behind the Library window's source editor.
        sources : Library.SourceProxy
        /// The category WRITE store (STORE_XDUO_0003) — its remove-block consults
        /// the LIVE `materials` store above through `materialsReferencingCategory`.
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
        /// Spec 0046: the experiment-data export seam — the CSV adapter writing each exported run into the
        /// user's `Documents/Optical Constructor/Exports` folder (nothing touches the disk until an export).
        experimentExport : ExperimentExport.ExperimentExportProxy
        /// The step-005 typed appsettings.json values (`AppConfig.loadWorkbenchSettings`
        /// elevates them once at the composition root); later Part C/E/F steps read the
        /// window-policy / quick-pick / tree / bucketing fields from HERE, never from
        /// the provider.
        settings : WorkbenchSettings
    }

    /// Build one app scope over empty in-memory stores filled from the Built-in catalogue
    /// (spec 0038 §0.2; spec 0050 §3.4). Import failures are logged; entries already imported remain available.
    static member create (settings : WorkbenchSettings) : AppContext =
        let stores = CatalogueImportProxy.createInMemory ()
        match stores.proxy.importFrom (CatalogueProxy.createBuiltIn ()) with
        | Ok () -> ()
        | Error e -> Logger.logError $"AppContext.create: could not import the Built-in catalogue: {e.describe}"
        let sources = Library.SourceProxy.createInMemory ()
        {
            library = Library.liveLibrary stores.libraryEntries stores.samples sources
            experiments = Experiments.createInMemoryExperiments ()
            materials = stores.materials
            samples = stores.samples
            sources = sources
            categories = stores.categories
            // Spec 0038 Part L (037): the measured-data load seam is the real file-backed adapter
            // (step 036 — read a picked file's text and hand it to the step-34 parsers, the one
            // file-read seam §0.3c permits); the experiment-collection store is the step-029
            // in-memory `createInMemory`. Both thread into `initMainWith` / `initInverse`.
            experimentData = OpticalConstructor.Storage.ExperimentDataStore.createFileBacked ()
            experimentCollections = ExperimentCollectionStore.ExperimentCollectionProxy.createInMemory ()
            // Spec 0038 (027): the scene store — the in-memory `SceneProxy.createInMemory` (a fresh
            // session starts with no saved scenes, §0.2), the last of the STORE_XDUO_000x proxies to
            // be composed at the root (step 047 "every proxy built once at the root").
            scenes = Scene.SceneProxy.createInMemory ()
            experimentExport =
                let documents = System.Environment.GetFolderPath System.Environment.SpecialFolder.MyDocuments
                OpticalConstructor.Storage.ExperimentExportFile.createCsvFolder
                    (Softellect.Sys.Primitives.FolderName (System.IO.Path.Combine(documents, "Optical Constructor", "Exports")))
                    (fun () -> OpticalConstructor.Storage.ExperimentExportFile.ExportTime System.DateTime.Now)
            settings = settings
        }
