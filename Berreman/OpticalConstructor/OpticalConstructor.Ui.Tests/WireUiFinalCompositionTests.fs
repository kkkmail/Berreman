/// Composition acceptance over one app scope (spec 0038 Part N; spec 0050 S4.3).
/// Covers the library and kit composition and headless rendering through the root window factory.
/// Each opened window closes before the next probe to release its app-global registry key.
namespace OpticalConstructor.Ui.Tests

open OpticalConstructor.Tests

open Avalonia.Controls
open Avalonia.Threading
open Xunit
open Softellect.Sys.Primitives
open OpticalConstructor.Domain
open OpticalConstructor.Domain.WindowMode
open OpticalConstructor.Domain.WorkbenchSettings
open OpticalConstructor.Controls
open OpticalConstructor.Ui
open OpticalConstructor.Ui.WindowLauncher
open OpticalConstructor.App

module WireUiFinalCompositionTests =

    let private owner = FolderName "WireUiFinalCompositionTests"

    /// The SolverHandoff summary snapshot the root factory hands the window — an EMPTY collection, so
    /// the terminal screen renders its summary + solver-comes-later message with no per-experiment
    /// row and NO measured-data load against the real file-backed proxy (the rich per-file validation
    /// is proven by `SolverHandoffWindowTests`). The point here is that the window COMPOSES over the
    /// root-wired `library` + `experimentData` proxies and renders one frame without throwing.
    let private handoffSnapshot : ExperimentCollectionStore.ExperimentCollectionSnapshot =
        { name = ExperimentCollectionStore.CollectionName "wire-ui-sweep"; experiments = [] }

    /// The ONE composition-root window factory: every `WindowKey` maps to its window built over the
    /// app-scope proxies — the "construct the WindowLauncher with every window factory" seam. The two
    /// editor keys carry the entity id minted at open (an Add opens a `NewMaterial` / `NewBlankSample`
    /// editor under that id). Exhaustive over `WindowKey`.
    let private rootWindowFactory (ctx : AppContext) (key : WindowKey) : Result<Window, WindowLauncherError> =
        match key with
        | MaterialsWindowKey ->
            MaterialsWindow(ctx.materials, ctx.categories, ctx.catalogueImports) :> Window |> Ok
        | LibraryWindowKey ->
            LibraryWindow(ctx.library, ctx.samples, ctx.sources, ctx.materials, ctx.categories, ctx.libraryBackups, ctx.catalogueImports) :> Window |> Ok
        | SolverHandoffWindowKey ->
            let kit = KitOpening.scratchKit () |> SceneTestFixture.kitSuccess
            let data = OpticalConstructor.Storage.ExperimentDataStore.createFileBacked kit.proxy
            SolverHandoffWindow(ctx.library, data, handoffSnapshot) :> Window |> Ok
        | CategoryEditorKey ->
            CategoryEditorWindow(ctx.categories) :> Window |> Ok
        | MaterialEditorKey mintedId ->
            MaterialEditorWindow(ctx.materials, MaterialEditorView.NewMaterial mintedId, categories = ctx.categories) :> Window |> Ok
        | SampleEditorKey mintedId ->
            SampleEditorWindow(ctx.materials, ctx.samples, ctx.categories, ctx.catalogueImports, SampleEditorView.NewBlankSample mintedId) :> Window |> Ok
        | SourceEditorKey mintedId ->
            LightSourceEditorWindow(ctx.sources, LightSourceEditorView.NewSource mintedId) :> Window |> Ok

    /// Show a window, pump one frame, assert it rendered, then close and pump the close (the `Closed`
    /// hooks) — the "renders one frame without throwing" probe. Flexible over the concrete window type.
    let private showAssertClose (window : #Window) : unit =
        window.Show()
        Dispatcher.UIThread.RunJobs()
        Assert.True(window.IsVisible, $"%s{window.GetType().Name} must render one frame headless without throwing")
        window.Close()
        Dispatcher.UIThread.RunJobs()

    /// Open `key` through the REAL root launcher (Browse-mode over the shared registry, the app's
    /// modality switch), assert it CREATED and shows a live window, then close it — the registry keys
    /// are app-global, so the open must not leak.
    let private openThroughLauncher (ctx : AppContext) (key : WindowKey) : unit =
        let launcher = WindowLauncher.create (rootWindowFactory ctx) ctx.settings.selectWindowModality BrowseOpen
        match launcher.openOrActivate key with
        | Ok (CreatedWindow window) ->
            Assert.True(window.IsVisible, $"the root factory must build and SHOW a window for %A{key}")
            window.Close()
            Dispatcher.UIThread.RunJobs()
        | other -> Assert.Fail($"opening %A{key} through the root launcher must CREATE a window, got %A{other}")

    /// A Select-session context over the given kind — the two outcome callbacks are inert here (the
    /// sweep renders the window; the return routing is proven by the faceted-window suites).
    let private selectContext (kind : Placement.CatalogueKind) : SelectionContext<'entry> =
        {
            kindConstraint = KindConstraint kind
            currentEntryIdOpt = None
            target = TableElementTarget (Library.ElementId "wire-sweep")
            onSelected = ignore
            onCancelled = ignore
        }

    // ======================== the headless wiring assertion (gate `ui-smoke`) ========================

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the launcher and both constructor states render over ONE root-composed app scope`` () =
        HeadlessSession.run (fun () ->
            let ctx = TestAppScope.freshAppContext owner
            // The three windows the composition root hands the context to (Program.fs): the launcher is
            // the startup window, Main / Inverse are its forward / inverse constructor scenes.
            let settled = { ctx with pendingImports = [] }
            showAssertClose (LauncherWindow(TestAppScope.testLocations owner, WorkbenchSettings.defaults, Ok settled))
            showAssertClose ((MainConstructorWindow.tryCreate ctx |> SceneTestFixture.kitSuccess))
            showAssertClose ((InverseConstructorWindow.tryCreate ctx |> SceneTestFixture.kitSuccess)))

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the root window-launcher factory opens and renders Materials, Library, the three editors, and the SolverHandoff`` () =
        HeadlessSession.run (fun () ->
            let ctx = TestAppScope.freshAppContext owner
            // ONE factory covering every WindowKey, driven through the REAL launcher over the shared
            // host-layer WindowRegistry — the composition-root "every window factory" seam. Each open
            // creates + shows the window and is closed before the next, so no registry key leaks.
            [ MaterialsWindowKey
              LibraryWindowKey
              MaterialEditorKey (MaterialLibrary.newMaterialId ())
              SampleEditorKey (Library.newSampleId ())
              SourceEditorKey (Library.newSourceId ())
              CategoryEditorKey
              SolverHandoffWindowKey ]
            |> List.iter (openThroughLauncher ctx))

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``both faceted windows render in Select state over the app scope`` () =
        HeadlessSession.run (fun () ->
            let ctx = TestAppScope.freshAppContext owner
            // The SAME Materials / Library windows the launcher opens Browse, opened in Select state
            // (the step-016 mode) over the app scope — the second of the "both modes" the acceptance
            // enumerates. The modeless-default modality means an ordinary Show (no modal block).
            let materialSelect : SelectionContext<MaterialLibrary.MaterialEntry> = selectContext Placement.Sample
            showAssertClose (MaterialsWindow(ctx.materials, ctx.categories, ctx.catalogueImports, mode = Select materialSelect))
            let librarySelect : SelectionContext<Library.LibraryEntry> = selectContext Placement.Sample
            showAssertClose (LibraryWindow(ctx.library, ctx.samples, ctx.sources, ctx.materials, ctx.categories, ctx.libraryBackups, ctx.catalogueImports, mode = Select librarySelect)))
