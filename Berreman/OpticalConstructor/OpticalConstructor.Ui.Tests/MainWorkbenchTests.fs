namespace OpticalConstructor.Ui.Tests

open Avalonia
open Avalonia.Controls
open Avalonia.Headless
open Avalonia.Threading
open Avalonia.VisualTree
open Avalonia.FuncUI.Hosts
open Avalonia.FuncUI.Elmish
open Elmish
open Xunit
open OpticalConstructor.Domain
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.Lifecycle
open OpticalConstructor.Domain.MaterialStore
open OpticalConstructor.Controls
open OpticalConstructor.Ui
open OpticalConstructor.Ui.TableAndElementRotationView

/// Spec 0033 (024) / 0038 (013/015) — the Main-screen workbench composition around the ribbon.
/// The MATERIALS bay these tests used to cover became the single-instance Materials WINDOW
/// (spec 0038 step 013, `MaterialsWindowTests`) and the LIBRARY (samples) workbench bay the
/// single-instance Library WINDOW (spec 0038 step 015, `LibraryWindowTests`); what remains here
/// is the workbench side: the bay roster without either workbench bay, the two strip buttons'
/// launcher seam, the material-editor create picker over the live catalogue, and the
/// Details-bay band-state projection.
module MainWorkbenchTests =

    /// A control matches `id` by its `Name` OR its `AutomationProperties.AutomationId` (the
    /// workbench rows / verb buttons live in variable-membership lists, so they carry an
    /// AutomationId — the MaterialsControls precedent).
    let private matchesId (id : string) (c : Control) : bool =
        c.Name = id || Avalonia.Automation.AutomationProperties.GetAutomationId(c) = id

    let private tryFindControl (window : Window) (id : string) : Control option =
        window.GetVisualDescendants()
        |> Seq.tryPick (function :? Control as c when matchesId id c -> Some c | _ -> None)

    /// Present anywhere in the visual tree (visible or not) — the "is it listed" probe.
    let private isPresent (window : Window) (id : string) : bool =
        match tryFindControl window id with
        | Some _ -> true
        | None -> false

    /// Click the centre of the clickable Border carrying `id` (by Name or AutomationId).
    let private clickOn (window : Window) (id : string) : unit =
        let found =
            window.GetVisualDescendants()
            |> Seq.tryPick (function :? Border as b when matchesId id b && b.IsEffectivelyVisible -> Some b | _ -> None)
        match found with
        | None -> Assert.Fail($"%s{id} was not found (or not visible)")
        | Some b ->
            let c = b.TranslatePoint(Point(b.Bounds.Width / 2.0, b.Bounds.Height / 2.0), window)
            if c.HasValue then
                window.MouseDown(c.Value, Avalonia.Input.MouseButton.Left, Avalonia.Input.RawInputModifiers.None)
                Dispatcher.UIThread.RunJobs()
                if window.IsVisible then
                    window.MouseUp(c.Value, Avalonia.Input.MouseButton.Left, Avalonia.Input.RawInputModifiers.None)
                    Dispatcher.UIThread.RunJobs()
            else Assert.Fail($"%s{id} has no on-screen position")

    /// Fresh, isolated in-memory stores per test — the SAME composition the App performs: the
    /// samples store first, then the materials store whose remove-block consults the LIVE
    /// samples through `samplesReferencing`.
    let private freshStores () : MaterialProxy * SampleProxy =
        let samples = SampleProxy.createInMemory ()
        let materials = MaterialProxy.createInMemory (samplesReferencing samples) VersionsInUse.empty
        materials, samples

    /// The Main-scene model over the given stores (mock Library/Experiments proxies as in App).
    /// The category store (spec 0035 step 009) is composed here from the materials store — its
    /// `materialsReferencingCategory` lookup — exactly as the App root does.
    let private mainWith (materials : MaterialProxy) (samples : SampleProxy) : Model =
        let categories = CategoryProxy.createInMemory (materialsReferencingCategory materials)
        initMainWith (Library.createInMemory ()) (Experiments.createInMemory ()) materials samples categories

    let private freshMain () : Model =
        let materials, samples = freshStores ()
        mainWith materials samples

    /// Mount the REAL Main-screen MVU loop over `model0` in a headless `HostWindow`, so a click
    /// dispatches through `update` and the view re-renders in the same pass (the
    /// `MainConstructorWindow` shape, with injectable stores / launchers).
    let private mountMain (model0 : Model) : HostWindow =
        let window = HostWindow(Width = 980.0, Height = 1050.0)
        Program.mkSimple (fun () -> model0) update mainView
        |> Program.withHost window
        |> Program.run
        window.Show()
        Dispatcher.UIThread.RunJobs()
        window

    /// A recording launcher pair (the functional-proxy seam — tests observe which window a
    /// strip button requested without opening one): the Materials-window request (spec 0038
    /// step 013) and the Library-window request (step 015) each record their tag.
    let private recordingLaunchers () : ResizeArray<string> * EditorLaunchers =
        let calls = ResizeArray<string>()
        let launchers : EditorLaunchers =
            {
                openMaterialsWindow = fun _ _ -> calls.Add "materials-window"
                openLibraryWindow = fun _ _ _ _ -> calls.Add "library-window"
                // Spec 0038 (017): the Choose… Select-state open — recorded, never a real
                // window; the returned handle records the staleness close like the real one.
                openLibrarySelectWindow =
                    fun _ _ _ _ _ _ _ ->
                        calls.Add "library-select-window"
                        Some (fun () -> calls.Add "library-select-close")
            }
        calls, launchers

    // ============================ pure: bay roster ============================

    [<Fact>]
    let ``the ribbon offers NO workbench bay any more — Materials and Library are the strip-button windows`` () =
        // Spec 0038 steps 013/015: the Materials bay left the ribbon at step 013 and the
        // Library (samples workbench) bay leaves with step 015 — both workbenches are
        // single-instance WINDOWS now, and no full-surface bay remains.
        Assert.DoesNotContain("Materials", BayNames.all)
        Assert.DoesNotContain("Library", BayNames.all)
        Assert.Contains(BayNames.selector, BayNames.all)
        Assert.Equal(Some BayNames.details, List.tryLast BayNames.all)
        let m = freshMain ()
        let bays = mainBays m ignore
        Assert.Equal<string list>(BayNames.all, bays |> List.map (fun b -> b.name))
        for bay in bays do
            Assert.Equal(Ribbon.InRibbonPane, bay.mode)

    // ============================ pure: the create picker over the live catalogue ====

    /// Rename a built-in category through the proxy (built-ins ARE renamable — no origin guard on
    /// update), failing the test on a typed rejection.
    let private renameGlass (categories : CategoryProxy) (newName : string) : unit =
        match categories.updateCategory { id = CategoryIds.glass; name = newName; visibility = SelectableOnCreate; origin = BuiltInCategory } with
        | Ok () -> ()
        | Error e -> Assert.Fail($"rename failed: %A{e}")

    [<Fact>]
    let ``the material-editor create picker lists the live selectable catalogue, excludes HiddenOnCreate, and re-labels on a proxy rename`` () =
        let materials, _ = freshStores ()
        let categories = CategoryProxy.createInMemory (materialsReferencingCategory materials)
        let ctx : MaterialEditorView.MaterialEditorContext =
            { materials = materials; categories = categories; requestClose = ignore }
        let em = MaterialEditorView.init ctx (MaterialEditorView.NewMaterial (newMaterialId ()))
        let before = MaterialEditorView.selectableCategories em
        // The four SelectableOnCreate built-ins are offered; Vacuum (HiddenOnCreate) is excluded.
        Assert.Contains(before, fun (c : MaterialCategory) -> c.id = CategoryIds.glass)
        Assert.DoesNotContain(before, fun (c : MaterialCategory) -> c.id = CategoryIds.vacuum)
        // A rename through the shared proxy re-labels the picker on its next read.
        renameGlass categories "Glazing"
        let after = MaterialEditorView.selectableCategories em
        Assert.Contains(after, fun (c : MaterialCategory) -> c.name = "Glazing")
        Assert.DoesNotContain(after, fun (c : MaterialCategory) -> c.name = "Glass")

    // ============================ pure: the strip-button launcher seam ============================

    [<Fact>]
    let ``the Materials and Library strip buttons reach their launchers as pure launches`` () =
        let calls, launchers = recordingLaunchers ()
        let m = { freshMain () with launchers = launchers }
        // The Materials-window request (spec 0038 step 013): a pure launch over the model's stores.
        let afterMaterials = update OpenMaterialsWindow m
        Assert.Equal<string list>([ "materials-window" ], List.ofSeq calls)
        Assert.Equal<Model>(m, afterMaterials)
        // The Library-window request (spec 0038 step 015): likewise pure — the retired samples
        // workbench bay's verbs live in the window's own model now.
        calls.Clear()
        let afterLibrary = update OpenLibraryWindow m
        Assert.Equal<string list>([ "library-window" ], List.ofSeq calls)
        Assert.Equal<Model>(m, afterLibrary)

    // ============================ pure: the Details-bay band projection ============================

    [<Fact>]
    let ``sampleBandsState collapses a period group to x-N bands (the Details-bay shape)`` () =
        let st = sampleBandsState SeedSamples.multilayerQw
        Assert.Contains("Quarter-wave", st.title)
        // One band per unit-cell layer (kept collapsed, ×20) plus the closing single layer.
        Assert.Equal(3, List.length st.bands)
        Assert.Contains("×20", st.bands.[0].label)
        Assert.Contains("×20", st.bands.[1].label)
        Assert.DoesNotContain("×", st.bands.[2].label)

    // ============================ headless proofs (ui-smoke) ============================

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``headless acceptance: both workbench tabs are gone, both strip buttons ride the ribbon row, and the table canvas persists`` () =
        HeadlessSession.run (fun () ->
            let materials, samples = freshStores ()
            let window = mountMain (mainWith materials samples)
            // Spec 0038 steps 013/015: NO Materials and NO Library ribbon tab any more — the
            // tab-strip row carries the two right-aligned window buttons instead.
            Assert.False(isPresent window (Ribbon.UiIds.tab "Materials"),
                         "the Materials bay tab must be gone from the ribbon")
            Assert.False(isPresent window (Ribbon.UiIds.tab "Library"),
                         "the Library bay tab must be gone from the ribbon")
            Assert.True(isPresent window WorkbenchIds.openMaterialsButton,
                        "the ribbon strip row must carry the right-aligned Materials… button")
            Assert.True(isPresent window WorkbenchIds.openLibraryButton,
                        "the ribbon strip row must carry the right-aligned Library… button")
            // With no full-surface bay left, every bay keeps the shared table canvas below the
            // strip — switching to the LAST bay (Details) and back never drops it.
            Assert.True(isPresent window UiIds.canvas, "the default bay keeps the table canvas below the ribbon strip")
            clickOn window (Ribbon.UiIds.tab BayNames.details)
            Assert.True(isPresent window UiIds.canvas, "an in-pane bay keeps the table canvas below the strip")
            clickOn window (Ribbon.UiIds.tab BayNames.rotation)
            Assert.True(isPresent window UiIds.canvas, "returning to a table bay keeps the table canvas")
            window.Close())
