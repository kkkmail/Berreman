/// Spec 0038 Part N (step 047, WIRE_UI / AC-W1) — the CLOSING composition acceptance. Prior steps
/// built every component; this step finalizes the composition root (`OpticalConstructor.App`) and
/// drives the WHOLE root-wired surface headless. The composition root builds the app scope
/// (`AppContext.create`) ONCE — the five stores, the file-backed `ExperimentDataProxy` (36), the
/// in-memory `ExperimentCollectionProxy` (29) and `SceneProxy` (27) — and hands it to the launcher
/// window and both constructor states; the window-launcher seam (`WindowLauncher.create` over the ONE
/// host-layer `WindowRegistry`) opens every window through ONE factory (Materials, Library, the three
/// editors, SolverHandoff) under the app's modality switch.
///
/// Two layers. A `ui-tests` pin that the finalized app scope carries every store proxy built once —
/// including the step-027 scene seam this step adds to the scope. Then the `ui-smoke` wiring assertion
/// (the WIRE_UI family's mandate): render one frame each for the launcher, the Main constructor, the
/// Inverse constructor, the Materials and Library windows in Browse and Select, the three editor
/// windows, and the SolverHandoff window — all over ONE app scope composed exactly like
/// `Startup.context` — without throwing. Every real-launcher open CLOSES the window it opens, so no key
/// leaks across the app-global `WindowRegistry` (the `WindowLauncherTests` discipline).
namespace OpticalConstructor.Ui.Tests

open Avalonia.Controls
open Avalonia.Threading
open Xunit
open OpticalConstructor.Domain
open OpticalConstructor.Domain.WindowMode
open OpticalConstructor.Domain.WorkbenchSettings
open OpticalConstructor.Controls
open OpticalConstructor.Ui
open OpticalConstructor.Ui.WindowLauncher
open OpticalConstructor.App

module WireUiFinalCompositionTests =

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
            MaterialsWindow(ctx.materials, ctx.categories) :> Window |> Ok
        | LibraryWindowKey ->
            LibraryWindow(ctx.library, ctx.samples, ctx.materials, ctx.categories) :> Window |> Ok
        | SolverHandoffWindowKey ->
            SolverHandoffWindow(ctx.library, ctx.experimentData, handoffSnapshot) :> Window |> Ok
        | CategoryEditorKey ->
            CategoryEditorWindow(ctx.categories) :> Window |> Ok
        | MaterialEditorKey mintedId ->
            MaterialEditorWindow(ctx.materials, MaterialEditorView.NewMaterial mintedId, categories = ctx.categories) :> Window |> Ok
        | SampleEditorKey mintedId ->
            SampleEditorWindow(ctx.materials, ctx.samples, ctx.categories, SampleEditorView.NewBlankSample mintedId) :> Window |> Ok

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
            target = TableElementTarget (Library.elementId "wire-sweep")
            onSelected = ignore
            onCancelled = ignore
        }

    // ============================ pure composition pin (gate `ui-tests`) ============================

    [<Fact>]
    let ``the finalized app scope carries every store proxy — the step-027 scene seam is composed at the root`` () =
        let ctx = AppContext.create WorkbenchSettings.defaults
        // The scene proxy (STORE_XDUO_0004) is now built once at the app scope: a fresh scope has no
        // saved scenes, and the seam answers (a live in-memory store, not a throwing stub).
        match ctx.scenes.listScenes () with
        | Ok [] -> ()
        | Ok other -> Assert.Fail($"a fresh app scope must carry an empty scene store, got %A{other}")
        | Error e -> Assert.Fail($"the root-composed scene proxy must answer, got %A{e}")
        // …beside the two experiment seams and the settings the root also composes.
        match ctx.experimentCollections.listCollections () with
        | Ok _ -> ()
        | Error e -> Assert.Fail($"the root-composed experiment-collection proxy must answer, got %A{e}")
        Assert.Equal(WorkbenchSettings.defaults, ctx.settings)

    // ======================== the headless wiring assertion (gate `ui-smoke`) ========================

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the launcher and both constructor states render over ONE root-composed app scope`` () =
        HeadlessSession.run (fun () ->
            let ctx = AppContext.create WorkbenchSettings.defaults
            // The three windows the composition root hands the context to (Program.fs): the launcher is
            // the startup window, Main / Inverse are its forward / inverse constructor scenes.
            showAssertClose (LauncherWindow(ctx))
            showAssertClose (MainConstructorWindow(ctx))
            showAssertClose (InverseConstructorWindow(ctx)))

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the root window-launcher factory opens and renders Materials, Library, the three editors, and the SolverHandoff`` () =
        HeadlessSession.run (fun () ->
            let ctx = AppContext.create WorkbenchSettings.defaults
            // ONE factory covering every WindowKey, driven through the REAL launcher over the shared
            // host-layer WindowRegistry — the composition-root "every window factory" seam. Each open
            // creates + shows the window and is closed before the next, so no registry key leaks.
            [ MaterialsWindowKey
              LibraryWindowKey
              MaterialEditorKey (MaterialLibrary.newMaterialId ())
              SampleEditorKey (Library.newSampleId ())
              CategoryEditorKey
              SolverHandoffWindowKey ]
            |> List.iter (openThroughLauncher ctx))

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``both faceted windows render in Select state over the app scope`` () =
        HeadlessSession.run (fun () ->
            let ctx = AppContext.create WorkbenchSettings.defaults
            // The SAME Materials / Library windows the launcher opens Browse, opened in Select state
            // (the step-016 mode) over the app scope — the second of the "both modes" the acceptance
            // enumerates. The modeless-default modality means an ordinary Show (no modal block).
            let materialSelect : SelectionContext<MaterialLibrary.MaterialEntry> = selectContext Placement.Sample
            showAssertClose (MaterialsWindow(ctx.materials, ctx.categories, mode = Select materialSelect))
            let librarySelect : SelectionContext<Library.LibraryEntry> = selectContext Placement.Sample
            showAssertClose (LibraryWindow(ctx.library, ctx.samples, ctx.materials, ctx.categories, mode = Select librarySelect)))
