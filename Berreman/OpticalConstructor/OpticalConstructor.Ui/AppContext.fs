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
        // seam; nothing is persisted yet (§0.2), so `VersionsInUse.empty` is truthful for now —
        // step 25 injects the real one computed over the live experiment descriptors.
        let samples = SampleProxy.createInMemory VersionsInUse.empty
        let materials = MaterialLibrary.MaterialProxy.createInMemory (samplesReferencing samples) VersionsInUse.empty
        let categories = MaterialLibrary.CategoryProxy.createInMemory (MaterialLibrary.materialsReferencingCategory materials)
        {
            library = Library.createInMemory ()
            experiments = Experiments.createInMemory ()
            materials = materials
            samples = samples
            categories = categories
            settings = settings
        }
