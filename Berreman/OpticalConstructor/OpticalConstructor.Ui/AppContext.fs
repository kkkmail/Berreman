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
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.Lifecycle           // VersionsInUse.empty (the injected in-use seam)
open OpticalConstructor.Domain.MaterialStore       // MaterialProxy.createInMemory (versioned, spec 0038 step 021)
open OpticalConstructor.Domain.SampleStore         // SampleProxy.createInMemory (versioned, spec 0038 step 022)
open OpticalConstructor.Domain.WorkbenchSettings

/// The app-scope composition context (the `*Context` convention: the IO-boundary
/// proxies plus the app-configuration data, constructed once at the composition
/// root). The proxy fields are `[<ReferenceEquality>]` function records, and ONE
/// context IS the app scope, so the context itself compares by reference — two
/// structurally-similar contexts are two different scopes with independent stores.
[<ReferenceEquality>]
type AppContext =
    {
        /// The read-only catalogue seam behind the Selector bay (spec 0027).
        library : Library.LibraryProxy
        /// The read-only experiment-template seam behind the Experiments bay.
        experiments : Experiments.ExperimentProxy
        /// The materials WRITE store (STORE_XDUO_0001) — its remove-block consults
        /// the LIVE `samples` store below through `samplesReferencing`.
        materials : MaterialLibrary.MaterialProxy
        /// The samples WRITE store (STORE_XDUO_0002).
        samples : Library.SampleProxy
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
        /// The step-005 typed appsettings.json values (`AppConfig.loadWorkbenchSettings`
        /// elevates them once at the composition root); later Part C/E/F steps read the
        /// window-policy / quick-pick / tree / bucketing fields from HERE, never from
        /// the provider.
        settings : WorkbenchSettings
    }

    /// Build the ONE app scope over the in-memory stores (§0.2: no library state is
    /// persisted — every app start re-seeds). The composition ORDER is the
    /// load-bearing part (the `DefaultStores` / former in-window precedent): the
    /// samples store first, then the materials store whose remove-block consults the
    /// LIVE samples through `samplesReferencing`, then the category store whose
    /// remove-block consults the LIVE materials through
    /// `materialsReferencingCategory`. Real, disk-backed proxies would later be built
    /// here instead (in `OpticalConstructor.Storage`), leaving every consumer
    /// unchanged.
    static member create (settings : WorkbenchSettings) : AppContext =
        // Both stores are versioned (spec 0038 steps 021/022) and take the step-20 `VersionsInUse`
        // seam. Step 25 implements the real builder — `Experiments.versionsInUseSeam` over the
        // `boundVersions` of a live experiment collection (fully unit-verified: injected into a store,
        // a bound sample version blocks removal and mints on physics change). Wiring it HERE needs a
        // mutable experiment source shared with the once-built stores, but the live experiment
        // collection lives in the immutable Elmish model (`TableAndElementRotationView`), not a store
        // this composition root can read; that shared source lands with the `ExperimentCollectionProxy`
        // (spec 0038 Part I, a later step). Until then `VersionsInUse.empty` is truthful — no experiment
        // is persisted, so no version is in use across app scope yet (§0.2).
        let samples = SampleProxy.createInMemory VersionsInUse.empty
        let materials = MaterialLibrary.MaterialProxy.createInMemory (samplesReferencing samples) VersionsInUse.empty
        let categories = MaterialLibrary.CategoryProxy.createInMemory (MaterialLibrary.materialsReferencingCategory materials)
        {
            library = Library.createInMemory ()
            experiments = Experiments.createInMemory ()
            materials = materials
            samples = samples
            categories = categories
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
            settings = settings
        }
