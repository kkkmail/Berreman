namespace OpticalConstructor.Ui.Tests

open OpticalConstructor.Tests

open Avalonia
open Avalonia.Controls
open Avalonia.Headless
open Avalonia.Threading
open Avalonia.VisualTree
open Xunit
open Softellect.Sys.Primitives
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.Library
open OpticalConstructor.Controls
open OpticalConstructor.Ui
open OpticalConstructor.Ui.TableAndElementRotationView
open OpticalConstructor.Ui.Tests.UiDriver

/// WIRE_UI acceptance drives real Main, Materials, Library and editor windows over `AppContext.create`,
/// which fills empty stores from the Built-in catalogue (spec 0033 (026); 0035 (019); 0038 (006, 013, 015); 0050 §3.4).
/// Real `EditorLaunchers.defaults` launches are observed through `Window.WindowOpenedEvent`.
module WireUiCompositionTests =

    let private owner = FolderName "WireUiCompositionTests"

    module MW = OpticalConstructor.Ui.MaterialsWindowView
    module LW = OpticalConstructor.Ui.LibraryWindowView

    /// Set the text of the TextBox carrying `id` (fires the property-change subscription the
    /// control's `onTextChanged` binds).
    let private setText (window : Window) (id : string) (text : string) : unit =
        match tryFindControl window id with
        | Some (:? TextBox as tb) ->
            tb.Text <- text
            Dispatcher.UIThread.RunJobs()
        | Some c -> Assert.Fail($"%s{id} is a %s{c.GetType().Name}, not a TextBox")
        | None -> Assert.Fail($"%s{id} was not found")

    /// Commit `text` through the REAL faceted filter box (the Materials window's box commits on
    /// Enter/LostFocus only — never per keystroke — so `setText` alone dispatches nothing).
    let private commitFilter (window : Window) (text : string) : unit =
        match tryFindControl window ConstructorUiId.facetFilterBox.value with
        | Some (:? TextBox as tb) ->
            tb.Focus() |> ignore
            Dispatcher.UIThread.RunJobs()
            tb.Text <- text
            Dispatcher.UIThread.RunJobs()
            window.KeyPressQwerty(Avalonia.Input.PhysicalKey.Enter, Avalonia.Input.RawInputModifiers.None)
            Dispatcher.UIThread.RunJobs()
            window.KeyReleaseQwerty(Avalonia.Input.PhysicalKey.Enter, Avalonia.Input.RawInputModifiers.None)
            Dispatcher.UIThread.RunJobs()
        | Some c -> Assert.Fail($"the filter box is a %s{c.GetType().Name}, not a TextBox")
        | None -> Assert.Fail("the filter box was not found")

    /// Mounts the real window over an isolated app scope and kit (spec 0038 step 006; spec 0050 S4.3).
    let private mountRoot () : Window * AppContext =
        let context = TestAppScope.freshAppContext owner
        let opened = TestAppScope.openedKitIn owner context.catalogueImports
        let window = OpticalConstructor.App.MainConstructorWindow(context, opened)
        window.Show()
        Dispatcher.UIThread.RunJobs()
        window :> Window, context

    /// Observe the windows the REAL `EditorLaunchers.defaults` open (unowned `.Show()`) through
    /// the public global `Window.WindowOpenedEvent` — the seam the desktop lifetime itself uses.
    let private trackOpened () : ResizeArray<Window> * System.IDisposable =
        let opened = ResizeArray<Window>()
        let sub =
            Window.WindowOpenedEvent.Raised
            |> Observable.subscribe (fun (struct (sender, _args)) ->
                match sender with
                | :? Window as w -> opened.Add w
                | _ -> ())
        opened, sub

    /// Open the REAL single-instance Materials window through the Launcher's seam over the root-wired stores (spec
    /// 0038 step 013; spec 0051 `.manual/007` 2.3: the Launcher alone opens it). The caller MUST close it — a leaked
    /// `MaterialsWindowKey` registration would make a later open activate a stale window over the wrong stores.
    let private openMaterialsWindow (context : AppContext) : Window =
        let opened, sub = trackOpened ()
        use _sub = sub
        EditorLaunchers.defaults.openMaterialsWindow context.materials context.categories context.catalogueImports
        Dispatcher.UIThread.RunJobs()
        Assert.Equal(1, opened.Count)
        Assert.True(UiDriver.hasId ConstructorUiId.materialsWindow.value opened.[0], "the seam must open the Materials window")
        Assert.True(opened.[0].IsVisible, "the Materials window must be shown")
        opened.[0]

    /// Open the REAL single-instance Library window through the Launcher's seam over the root-wired stores (spec 0038
    /// step 015). The caller MUST close it (the `openMaterialsWindow` discipline, keyed under `LibraryWindowKey`).
    let private openLibraryWindow (context : AppContext) : Window =
        let opened, sub = trackOpened ()
        use _sub = sub
        EditorLaunchers.defaults.openLibraryWindow context.library context.samples context.sources context.materials context.categories context.libraryBackups context.catalogueImports
        Dispatcher.UIThread.RunJobs()
        Assert.Equal(1, opened.Count)
        Assert.True(UiDriver.hasId ConstructorUiId.libraryWindow.value opened.[0], "the seam must open the Library window")
        Assert.True(opened.[0].IsVisible, "the Library window must be shown")
        opened.[0]

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``composition acceptance: the REAL Main window renders every ribbon bay, and the Materials / Library windows open over the root-wired stores`` () =
        HeadlessSession.run (fun () ->
            let window, appContext = mountRoot ()
            Assert.True(window.IsVisible, "the Main window must open headless")
            // One frame per bay, on the real window (the whole ribbon surface — a bay whose
            // content throws fails the sweep here). Spec 0038 steps 013/015: neither workbench
            // tab is in the roster any more.
            Assert.DoesNotContain(Ribbon.BayName "Materials", BayNames.allBayNames)
            Assert.DoesNotContain(Ribbon.BayName "Library", BayNames.allBayNames)
            // Spec 0051 `.manual/007` 5.2: the ribbon is a setup's Create or Edit; a fresh kit has no setup, so New setup.
            clickOn window ConstructorUiId.newSetupButton.value
            for bay in BayNames.allBayNames do
                clickOn window (ConstructorUiId.ribbonTab (ConstructorUiKey bay.value)).value
                Assert.True(window.IsVisible, $"the %s{bay.value} bay must render one frame without throwing")
            // Materials: the Launcher's seam opens the REAL single-instance window, whose tree
            // lists BuiltinCatalogue.materials () through the root-wired live MaterialProxy.
            let materialsWindow = openMaterialsWindow appContext
            // The tree opens collapsed (spec 0040 step 002) — expand entries to reach the leaves.
            clickOn materialsWindow (ConstructorUiId.facetTreeChevron (ConstructorUiKey "entries")).value
            Assert.True(isPresent materialsWindow ((ConstructorUiId.materialsEntryNode MaterialIds.glass152).value),
                        "the Materials window must list the seeded glass152 entry from the root-wired store")
            Assert.True(isPresent materialsWindow ((ConstructorUiId.materialsEntryNode MaterialIds.silicon).value),
                        "the Materials window must list the seeded silicon entry from the root-wired store")
            materialsWindow.Close()
            Dispatcher.UIThread.RunJobs()
            // Library: the REAL single-instance window's by-kind tree lists BuiltinCatalogue.samples ()
            // from the live store and BuiltinCatalogue.presets () through root-wired proxies.
            let libraryWindow = openLibraryWindow appContext
            // The tree opens collapsed (spec 0040 step 002) — expand entries to reach the leaves.
            clickOn libraryWindow (ConstructorUiId.facetTreeChevron (ConstructorUiKey "entries")).value
            Assert.True(isPresent libraryWindow ((ConstructorUiId.libraryEntryNode SampleIds.glassFilm600Id.toElementValueId).value),
                        "the Library window must list the seeded glassFilm600 sample from the root-wired store")
            Assert.True(isPresent libraryWindow ((ConstructorUiId.libraryEntryNode PresetIds.source600).value),
                        "the Library window must list the seeded source preset from the root-wired proxy")
            libraryWindow.Close()
            Dispatcher.UIThread.RunJobs()
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``composition acceptance: Add and Edit on the REAL Main window open both real editor windows through the default launchers, and the root couples materials to the live samples`` () =
        HeadlessSession.run (fun () ->
            let window, appContext = mountRoot ()
            // Materials (spec 0038 step 013): the Launcher's seam → the REAL Materials window over the root-wired stores.
            let materialsWindow = openMaterialsWindow appContext
            // Observe the editors the REAL launchers open: subscribed only after both host
            // windows are shown, so exactly the editors arrive.
            let opened, sub = trackOpened ()
            use _sub = sub
            // Materials window → Add: the REAL step-023 Material editor mounts headless.
            clickOn materialsWindow ConstructorUiId.materialsAddButton.value
            Dispatcher.UIThread.RunJobs()
            Assert.Equal(1, opened.Count)
            Assert.True(opened.[0].IsVisible, "the Material editor window must be shown")
            Assert.True(UiDriver.hasId ConstructorUiId.materialEditorWindow.value opened.[0], "the opened window must be the Material editor")
            opened.[0].Close()
            // Library window (spec 0038 step 015) → narrow, select the leaf, Edit: the REAL
            // step-022 Sample editor mounts, seeded with the picked sample.
            EditorLaunchers.defaults.openLibraryWindow
                appContext.library appContext.samples appContext.sources appContext.materials appContext.categories appContext.libraryBackups appContext.catalogueImports
            Dispatcher.UIThread.RunJobs()
            Assert.Equal(2, opened.Count)
            let libraryWindow = opened.[1]
            Assert.True(UiDriver.hasId ConstructorUiId.libraryWindow.value libraryWindow, "the seam must open the Library window")
            commitFilter libraryWindow "n=1.75"
            clickOn libraryWindow (ConstructorUiId.facetTreeChevron (ConstructorUiKey "entries")).value
            clickOn libraryWindow ((ConstructorUiId.libraryEntryNode SampleIds.glassFilm600Id.toElementValueId).value)
            clickOn libraryWindow ConstructorUiId.libraryEditButton.value
            Dispatcher.UIThread.RunJobs()
            Assert.Equal(3, opened.Count)
            Assert.True(opened.[2].IsVisible, "the Sample editor window must be shown")
            Assert.True(UiDriver.hasId ConstructorUiId.sampleEditorWindow.value opened.[2], "the opened window must be the Sample editor")
            Assert.Contains("Glass thin film", opened.[2].Title)
            opened.[2].Close()
            libraryWindow.Close()
            Dispatcher.UIThread.RunJobs()
            // The root coupling itself: removing a REFERENCED material through the real
            // Materials window surfaces the typed refusal NAMING the referencing sample — the
            // materials store's remove-block consults the LIVE samples store
            // (`samplesReferencing`) composed at the root, not a detached lookup.
            commitFilter materialsWindow "1.52"
            clickOn materialsWindow (ConstructorUiId.facetTreeChevron (ConstructorUiKey "entries")).value
            clickOn materialsWindow ((ConstructorUiId.materialsEntryNode MaterialIds.glass152).value)
            clickOn materialsWindow ConstructorUiId.materialsRemoveButton.value
            clickOn materialsWindow ConstructorUiId.materialsRemoveConfirmButton.value
            let message = textOf materialsWindow ConstructorUiId.materialsWindowMessage.value
            Assert.Contains("still referenced", message)
            Assert.Contains("Glass plate", message)
            Assert.True(isPresent materialsWindow ((ConstructorUiId.materialsEntryNode MaterialIds.glass152).value),
                        "the refused remove must leave the entry listed")
            materialsWindow.Close()
            Dispatcher.UIThread.RunJobs()
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``composition acceptance: Categories… on the REAL Materials window opens the real Category editor over the root-wired CategoryProxy`` () =
        HeadlessSession.run (fun () ->
            let window, appContext = mountRoot ()
            // Strip button → the REAL Materials window (spec 0038 step 013), then its
            // Categories… verb: the REAL step-006 Category editor mounts headless over the
            // root-composed CategoryProxy — the SAME proxy the composition root wired through
            // `initMainWith` (over `materialsReferencingCategory`) and handed to the window, so
            // this proves the CategoryProxy is wired at the root and that the third editor
            // window opens without throwing.
            let materialsWindow = openMaterialsWindow appContext
            let opened, sub = trackOpened ()
            use _sub = sub
            clickOn materialsWindow ConstructorUiId.materialsCategoriesButton.value
            Dispatcher.UIThread.RunJobs()
            Assert.Equal(1, opened.Count)
            Assert.True(opened.[0].IsVisible, "the Category editor window must be shown")
            Assert.True(UiDriver.hasId ConstructorUiId.categoryEditorWindow.value opened.[0], "the opened window must be the Category editor")
            opened.[0].Close()
            materialsWindow.Close()
            Dispatcher.UIThread.RunJobs()
            window.Close())
