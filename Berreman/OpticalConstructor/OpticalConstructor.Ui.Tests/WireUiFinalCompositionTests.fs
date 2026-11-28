/// Composition acceptance over one app scope (spec 0038 Part N; spec 0050 S4.3).
/// Covers the library and kit composition and headless rendering through the root window factory.
/// Each opened window closes before the next probe to release its app-global registry key.
/// Spec 0050 Close: the composed app goes from the Launcher through the start screen to a kit under the test output in
/// each mode, and each committed kit opens read-only from the start screen and writes nothing, every page shown.
namespace OpticalConstructor.Ui.Tests

open OpticalConstructor.Tests

open System.IO
open Avalonia
open Avalonia.Automation
open Avalonia.Controls
open Avalonia.Headless
open Avalonia.Input
open Avalonia.Interactivity
open Avalonia.Threading
open Avalonia.VisualTree
open Xunit
open Softellect.Sys.Primitives
open BerremanTestSupport
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.KitFolders
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.WindowMode
open OpticalConstructor.Domain.WorkbenchSettings
open OpticalConstructor.Storage.LibraryDatabaseStore
open OpticalConstructor.Storage.LibraryBackupStore
open OpticalConstructor.Controls
open OpticalConstructor.Ui
open OpticalConstructor.Ui.WindowLauncher
open OpticalConstructor.App
open Avalonia.FuncUI.Hosts
open Avalonia.FuncUI.Elmish
open global.Elmish
open OpticalConstructor.Ui.Tests.UiDriver.ById

module WireUiFinalCompositionTests =

    module Scene = OpticalConstructor.Ui.TableAndElementRotationView

    let private owner = FolderName "WireUiFinalCompositionTests"

    /// The ONE composition-root window factory: every `WindowKey` maps to its window built over the
    /// app-scope proxies — the "construct the WindowLauncher with every window factory" seam. The two
    /// editor keys carry the entity id minted at open (an Add opens a `NewMaterial` / `NewBlankSample`
    /// editor under that id). Exhaustive over `WindowKey`; a kit's constructor window is built by the kit hand-off, never
    /// by this factory (spec 0050 S5.3).
    let private rootWindowFactory (ctx : AppContext) (key : WindowKey) : Result<Window, WindowLauncherError> =
        match key with
        | MaterialsWindowKey ->
            MaterialsWindow(ctx.materials, ctx.categories, ctx.catalogueImports) :> Window |> Ok
        | LibraryWindowKey ->
            LibraryWindow(ctx.library, ctx.samples, ctx.sources, ctx.materials, ctx.categories, ctx.libraryBackups, ctx.catalogueImports) :> Window |> Ok
        | CategoryEditorKey ->
            CategoryEditorWindow(ctx.categories) :> Window |> Ok
        | DataLocationsWindowKey ->
            DataLocationsWindow(TestAppScope.testLocations owner, ExplorerProxy.createInMemory ignore) :> Window |> Ok
        | MaterialSourcesWindowKey ->
            MaterialSourcesWindow(ctx.catalogueImports, ignore) :> Window |> Ok
        | MaterialEditorKey mintedId ->
            MaterialEditorWindow(ctx.materials, MaterialEditorView.NewMaterial mintedId, categories = ctx.categories) :> Window |> Ok
        | SampleEditorKey mintedId ->
            SampleEditorWindow(ctx.materials, ctx.samples, ctx.categories, ctx.catalogueImports, SampleEditorView.NewBlankSample mintedId) :> Window |> Ok
        | SourceEditorKey mintedId ->
            LightSourceEditorWindow(ctx.sources, LightSourceEditorView.NewSource mintedId) :> Window |> Ok
        | KitWindowKey _ -> Error (WindowFactoryFailedErr (key, "a kit window opens over its opened kit"))

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
            // The windows the composition root hands the context to (Program.fs): the launcher is the startup
            // window, its Main / Inverse open the start screen of their mode, and a kit opens the forward /
            // inverse constructor scene (spec 0050 S5.2).
            let settled = { ctx with pendingImports = [] }
            showAssertClose (LauncherWindow(TestAppScope.testLocations owner, WorkbenchSettings.defaults, Ok settled,
                fun _ -> TestAppScope.kitPickerOf (TestAppScope.freshKitFolder owner)))
            for mode in [ TableAndElementRotationView.ForwardConstructor; TableAndElementRotationView.InverseConstructor ] do
                showAssertClose (KitStartWindow(ctx, mode, (fun _ -> TestAppScope.kitPickerOf (TestAppScope.freshKitFolder owner)), (fun _ -> KitArchive.KitZipPicker.none), (fun _ _ -> ())))
            showAssertClose (MainConstructorWindow(ctx, TestAppScope.openedKitIn owner ctx.catalogueImports))
            showAssertClose (InverseConstructorWindow(ctx, TestAppScope.openedKitIn owner ctx.catalogueImports)))

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the root window-launcher factory opens and renders Materials, Library, the three editors and Data locations`` () =
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
              DataLocationsWindowKey ]
            |> List.iter (openThroughLauncher ctx))

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the Materials window renders in Select state over the app scope`` () =
        HeadlessSession.run (fun () ->
            let ctx = TestAppScope.freshAppContext owner
            // The SAME Materials window the launcher opens Browse, opened in Select state (the step-016 mode, the sample
            // editor's material pick) over the app scope. The modeless-default modality means an ordinary Show.
            let materialSelect : SelectionContext<MaterialLibrary.MaterialEntry> = selectContext Placement.Sample
            showAssertClose (MaterialsWindow(ctx.materials, ctx.categories, ctx.catalogueImports, mode = Select materialSelect)))

    // ======================== spec 0050 Close: the composed app, end to end (gate `ui-smoke`) ========================

    /// A control matches `id` by its `Name` or its `AutomationId`: the table's canvas and a ribbon pane carry a `Name`,
    /// the panels and boxes an `AutomationId` (the `LauncherTests.matchesId` precedent).
    let private matchesUiId (id : ConstructorUiId) (control : Control) : bool =
        control.Name = id.value || AutomationProperties.GetAutomationId control = id.value

    let private tryControlOf (window : Window) (id : ConstructorUiId) : Control option =
        window.GetVisualDescendants()
        |> Seq.tryPick (function :? Control as control when matchesUiId id control -> Some control | _ -> None)

    /// Presses the box carrying `id` at its centre, as `LibraryWindowTests.clickOn` does; a press that closes the window
    /// gets no release.
    let private pressBoxIn (window : Window) (id : ConstructorUiId) : unit =
        let box =
            window.GetVisualDescendants()
            |> Seq.tryPick (function :? Control as border when matchesUiId id border -> Some border | _ -> None)
            |> Option.defaultWith (fun () -> failwith $"{id.value} must be a box in the window")
        Assert.True(box.IsEffectivelyVisible, $"{id.value} must be visible")
        box.BringIntoView() |> ignore
        Dispatcher.UIThread.RunJobs()
        match box.TranslatePoint(Point(box.Bounds.Width / 2.0, box.Bounds.Height / 2.0), window) |> Option.ofNullable with
        | Some centre ->
            window.MouseDown(centre, MouseButton.Left, RawInputModifiers.None)
            Dispatcher.UIThread.RunJobs()
            if window.IsVisible then
                window.MouseUp(centre, MouseButton.Left, RawInputModifiers.None)
                Dispatcher.UIThread.RunJobs()
        | None -> Assert.Fail $"{id.value} has no position in its window"

    /// Every window `act` opens, observed through `Window.WindowOpenedEvent` as `LauncherTests.clickAndObserve` observes them.
    let private windowsOpenedBy (act : unit -> unit) : Window list =
        let opened = ResizeArray<Window>()
        use _subscription =
            Window.WindowOpenedEvent.Raised
            |> Observable.subscribe (fun (struct (sender, _)) ->
                match sender with
                | :? Window as window -> opened.Add window
                | _ -> ())
        act ()
        Dispatcher.UIThread.RunJobs()
        List.ofSeq opened

    /// Raises `Click` on the launcher's button `id` and returns every window it opens.
    let private clickLauncherButton (launcher : LauncherWindow) (id : ConstructorUiId) : Window list =
        windowsOpenedBy (fun () ->
            let button = Assert.IsType<Button>(findControl launcher id)
            button.RaiseEvent(RoutedEventArgs(Button.ClickEvent)))

    /// The surface each page of the page row shows (spec 0050 S7.2, S9.3, S11.5, S14.2; spec 0051 `.manual/007` 2.1).
    let private flowSurfaces : (Scene.ConstructorFlowStep * ConstructorUiId) list =
        [
            Scene.ItemsFlowStep, ConstructorUiId.itemsPanel
            Scene.SetupsFlowStep, ConstructorUiId.setupsPagePanel
            Scene.ExperimentsFlowStep, ConstructorUiId.experimentsPagePanel
            Scene.DataFlowStep, ConstructorUiId.dataStepPanel
            Scene.UnknownsFlowStep, ConstructorUiId.studyUnknownsPanel
            Scene.PlanFlowStep, ConstructorUiId.planPanel
            Scene.FitFlowStep, ConstructorUiId.studyFitPanel
            Scene.ResultsFlowStep, ConstructorUiId.studyResultsPanel
            Scene.KitFlowStep, ConstructorUiId.kitPagePanel
        ]

    /// The window's page row holds one button per page of `mode`, in its order, and none of another page; pressing each,
    /// in order, shows its page (spec 0050 S7.2, S9.3, S11.5, S14.2; spec 0051 `.manual/007` 2.1).
    let private pressEveryFlowStep (window : Window) (mode : Scene.ConstructorMode) : unit =
        Assert.Equal<Scene.ConstructorFlowStep list>(Scene.ConstructorFlowStep.all, flowSurfaces |> List.map fst)
        let steps = Scene.ConstructorFlowStep.stepsOf mode
        let stepIds = Scene.ConstructorFlowStep.all |> List.map (fun step -> step.flowUiId.value)
        let strip = findControl window ConstructorUiId.pageRow
        let boxIds =
            strip.GetVisualDescendants()
            |> Seq.choose (function
                | :? Control as border when List.contains (AutomationProperties.GetAutomationId border) stepIds ->
                    Some (AutomationProperties.GetAutomationId border)
                | _ -> None)
            |> List.ofSeq
        Assert.Equal<string list>(steps |> List.map (fun step -> step.flowUiId.value), boxIds)
        for step in Scene.ConstructorFlowStep.all do
            match List.contains step steps, tryControlOf window step.flowUiId with
            | true, Some _
            | false, None -> ()
            | true, None -> Assert.Fail $"the page row must hold {step.flowUiId.value}"
            | false, Some _ -> Assert.Fail $"the page row must not hold {step.flowUiId.value}"
        for step, surface in flowSurfaces |> List.filter (fun (step, _) -> List.contains step steps) do
            pressBoxIn window step.flowUiId
            let shown = findControl window surface
            Assert.True(shown.IsEffectivelyVisible, $"{step.caption.value} must show {surface.value}")

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``acceptance: the composed app goes from the Launcher through the start screen to a kit under the test output and shows every page of its mode`` () : unit =
        HeadlessSession.run (fun () ->
            let locations = TestAppScope.fileLocations owner
            // Composed as `Program.fs:Startup` composes the app.
            let ctx =
                match AppContext.create locations WorkbenchSettings.defaults with
                | Ok ctx -> ctx
                | Error error -> failwith $"AppContext.create must open the library under the test output: {error.describe}"
            let libraryFile =
                LibraryDatabase.tryFileOf locations.libraryConnection
                |> Option.defaultWith (fun () -> failwith $"{locations.libraryConnection.value} must name a library file")
            Assert.True(File.Exists libraryFile.value, $"the library database {libraryFile.value} must exist")
            let kit = TestAppScope.freshKitFolder owner
            let kitFile = Path.Combine(kit.value.value, ".optics-kit")
            let lockFile = Path.Combine(kit.value.value, ".oc", "lock")
            let launcher = LauncherWindow(locations, WorkbenchSettings.defaults, Ok ctx, fun _ -> TestAppScope.kitPickerOf kit)
            let opened = ResizeArray<Window>()
            use _subscription =
                Window.WindowOpenedEvent.Raised
                |> Observable.subscribe (fun (struct (sender, _)) ->
                    match sender with
                    | :? Window as window when not (obj.ReferenceEquals(window, launcher)) -> opened.Add window
                    | _ -> ())
            try
                // Spec 0051 `.manual/003` 5.3 (D6): a new library takes the shipped catalogue on start, without a dialog.
                launcher.Show()
                Dispatcher.UIThread.RunJobs()
                Assert.Empty opened
                Assert.Equal<Result<MaterialEntry option, MaterialError>>(
                    Ok (Some (BuiltinCatalogue.material MaterialIds.silicon)), ctx.materials.tryGetMaterial MaterialIds.silicon)

                let inverseStart = Assert.IsType<KitStartWindow>(Assert.Single(clickLauncherButton launcher ConstructorUiId.openInverseButton))
                Assert.Equal("Open a kit — Inverse", inverseStart.Title)
                let inverse =
                    Assert.IsType<InverseConstructorWindow>(Assert.Single(windowsOpenedBy (fun () -> pressBoxIn inverseStart ConstructorUiId.newKitButton)))
                Assert.False(inverseStart.IsVisible, "the start screen must close once its kit opens")
                Assert.True(File.Exists kitFile, "New kit… must write the kit's .optics-kit")
                Assert.True(File.Exists lockFile, "the open kit must hold its lock")
                match (ctx.kitHistory.readHistory ()).entries with
                | first :: _ -> Assert.Equal<KitFolder>(kit, first.folder)
                | [] -> Assert.Fail "New kit… must put the kit first in the history"
                Assert.EndsWith(" — Optical Constructor — Inverse", inverse.Title)
                pressEveryFlowStep inverse Scene.InverseConstructor
                pressBoxIn inverse ConstructorUiId.kitCloseButton
                Assert.False(inverse.IsVisible, "Close kit must close the Inverse window")
                Assert.False(File.Exists lockFile, "closing the Inverse window must release its kit")

                let mainStart = Assert.IsType<KitStartWindow>(Assert.Single(clickLauncherButton launcher ConstructorUiId.openMainButton))
                Assert.Equal("Open a kit — Main", mainStart.Title)
                let main =
                    Assert.IsType<MainConstructorWindow>(Assert.Single(windowsOpenedBy (fun () -> pressBoxIn mainStart ConstructorUiId.openKitButton)))
                Assert.False(mainStart.IsVisible, "the start screen must close once its kit opens")
                Assert.True(File.Exists lockFile, "the reopened kit must hold its lock")
                Assert.EndsWith($" — {kit.value.value} — Optical Constructor — Main", main.Title)
                pressEveryFlowStep main Scene.ForwardConstructor
                pressBoxIn main ConstructorUiId.kitCloseButton
                Assert.False(main.IsVisible, "Close kit must close the Main window")
                Assert.False(File.Exists lockFile, "closing the Main window must release its kit")

                let dataLocations =
                    Assert.IsType<DataLocationsWindow>(Assert.Single(clickLauncherButton launcher ConstructorUiId.launcherDataLocationsLink))
                let libraryPath = Assert.IsAssignableFrom<TextBlock>(findControl dataLocations (ConstructorUiId.dataLocationPathText LibraryDatabaseLocation))
                Assert.Equal(libraryFile.value, libraryPath.Text)
                let kitsPath = Assert.IsAssignableFrom<TextBlock>(findControl dataLocations (ConstructorUiId.dataLocationPathText KitsLocation))
                Assert.Equal(locations.kitsFolder.value, kitsPath.Text)
            finally
                // Every window the fact opened closes, so no window registry key outlives it.
                for window in opened |> Seq.rev |> List.ofSeq do window.Close()
                launcher.Close()
                Dispatcher.UIThread.RunJobs())

    /// Opens `kit` read-only from the start screen against an empty library, mounts its Inverse window as `KitWindowTests`
    /// mounts its screens and presses every page, then closes the window and the kit as `KitAcceptanceTests`
    /// does and checks nothing was written under `data/` (spec 0050 S12.2, Close).
    let private showCommittedReadOnly (kit : CommittedKit) : unit =
        HeadlessSession.run (fun () ->
            let session = TestAppScope.openCommittedReadOnly owner kit
            try
                let model = { session.model with kitShell = Scene.KitWindowShell.inert }
                let window = HostWindow()
                Program.mkSimple (fun () -> model) Scene.updateTableAndElementRotation KitWindowView.kitWindowView
                |> Program.withHost window
                |> Program.run
                window.Width <- 1000.0
                window.Height <- 700.0
                try
                    window.Show()
                    Dispatcher.UIThread.RunJobs()
                    // Spec 0051 `.manual/007` 2.2: the kit's caption is the Kit page's.
                    pressBoxIn window ConstructorUiId.flowKitStep
                    Assert.Contains(" (read-only)", UiDriver.textOfControl (findControl window ConstructorUiId.kitCaptionText))
                    pressEveryFlowStep window Scene.InverseConstructor
                finally
                    window.Close()
                    Dispatcher.UIThread.RunJobs()
            finally
                try session.opened.library.kit.close () |> TestAppScope.readKitResult KitFile.fileName
                finally TestAppScope.assertCommittedUnchanged kit session.recorded)

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``acceptance: the committed optics-mueller kit opens read-only from the start screen, shows every page and writes nothing`` () : unit =
        showCommittedReadOnly OpticsMuellerKit

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``acceptance: the committed absorbing-biaxial-noisy kit opens read-only from the start screen, shows every page and writes nothing`` () : unit =
        showCommittedReadOnly AbsorbingBiaxialNoisyKit

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``acceptance: the committed multilayer-thin-film kit opens read-only from the start screen, shows every page and writes nothing`` () : unit =
        showCommittedReadOnly MultilayerThinFilmKit

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``acceptance: the committed langasite-on-silicon kit opens read-only from the start screen, shows every page and writes nothing`` () : unit =
        showCommittedReadOnly LangasiteOnSiliconKit
