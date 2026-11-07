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
open OpticalConstructor.Domain.CatalogueImport
open OpticalConstructor.Domain.MaterialStore
open OpticalConstructor.Controls
open OpticalConstructor.Tests
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
            // Spec 0046: scroll the box into view first (the facet rows now WRAP, so the tree can sit below
            // the visible area of its scroller, where a click at the box's centre lands nowhere).
            b.BringIntoView() |> ignore
            Dispatcher.UIThread.RunJobs()
            let c = b.TranslatePoint(Point(b.Bounds.Width / 2.0, b.Bounds.Height / 2.0), window)
            if c.HasValue then
                window.MouseDown(c.Value, Avalonia.Input.MouseButton.Left, Avalonia.Input.RawInputModifiers.None)
                Dispatcher.UIThread.RunJobs()
                if window.IsVisible then
                    window.MouseUp(c.Value, Avalonia.Input.MouseButton.Left, Avalonia.Input.RawInputModifiers.None)
                    Dispatcher.UIThread.RunJobs()
            else Assert.Fail($"%s{id} has no on-screen position")

    /// Fresh, isolated stores imported from the Built-in catalogue as in the App (spec 0050 §3.4–3.5).
    let private freshStores () : InMemoryCatalogueStores =
        BuiltinCatalogue.freshStores ()

    /// Composes Main from the Built-in catalogue and live stores as in the App (spec 0050 §3.4–3.5).
    let private mainWith (stores : InMemoryCatalogueStores) : TableAndElementRotationModel =
        initMainWith
            (liveLibrary stores.libraryEntries stores.samples (SourceProxy.createInMemory ()))
            (Experiments.createInMemoryExperiments ())
            stores.materials
            stores.samples
            stores.categories

    let private freshMain () : TableAndElementRotationModel =
        freshStores () |> mainWith

    /// Mount the REAL Main-screen MVU loop over `model0` in a headless `HostWindow`, so a click
    /// dispatches through `update` and the view re-renders in the same pass (the
    /// `MainConstructorWindow` shape, with injectable stores / launchers).
    let private mountMain (model0 : TableAndElementRotationModel) : HostWindow =
        let window = HostWindow(Width = 980.0, Height = 1050.0)
        Program.mkSimple (fun () -> model0) updateTableAndElementRotation mainView
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
                openLibraryWindow = fun _ _ _ _ _ _ -> calls.Add "library-window"
                // Spec 0038 (017): the Choose… Select-state open — recorded, never a real
                // window; the returned handle records the staleness close like the real one.
                openLibrarySelectWindow =
                    fun _ _ _ _ _ _ _ _ _ ->
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
        Assert.DoesNotContain(Ribbon.BayName "Materials", BayNames.allBayNames)
        Assert.DoesNotContain(Ribbon.BayName "Library", BayNames.allBayNames)
        Assert.Contains(BayNames.selector, BayNames.allBayNames)
        Assert.Equal(Some BayNames.details, List.tryLast BayNames.allBayNames)
        let m = freshMain ()
        let bays = mainBays m ignore
        Assert.Equal<Ribbon.BayName list>(BayNames.allBayNames, bays |> List.map (fun b -> b.name))
        for bay in bays do
            Assert.Equal(Ribbon.InRibbonPane, bay.mode)

    // ============================ pure: the create picker over the live catalogue ====

    /// Rename a built-in category through the proxy (built-ins ARE renamable — no origin guard on
    /// update), failing the test on a typed rejection.
    let private renameGlass (categories : CategoryProxy) (newName : string) : unit =
        match categories.updateCategory { id = CategoryIds.glass; name = CategoryName newName; visibility = SelectableOnCreate; origin = BuiltInCategory } with
        | Ok () -> ()
        | Error e -> Assert.Fail($"rename failed: %A{e}")

    [<Fact>]
    let ``the material-editor create picker lists the live selectable catalogue, excludes HiddenOnCreate, and re-labels on a proxy rename`` () =
        let stores = freshStores ()
        let categories = stores.categories
        let ctx : MaterialEditorView.MaterialEditorContext =
            {
                materials = stores.materials
                categories = categories
                requestClose = ignore
            }
        let em = MaterialEditorView.initMaterialEditor ctx (MaterialEditorView.NewMaterial (newMaterialId ()))
        let before = em.selectableCategories
        // The four SelectableOnCreate built-ins are offered; Vacuum (HiddenOnCreate) is excluded.
        Assert.Contains(before, fun (c : MaterialCategory) -> c.id = CategoryIds.glass)
        Assert.DoesNotContain(before, fun (c : MaterialCategory) -> c.id = CategoryIds.vacuumCategory)
        // A rename through the shared proxy re-labels the picker on its next read.
        renameGlass categories "Glazing"
        let after = em.selectableCategories
        Assert.Contains(after, fun (c : MaterialCategory) -> c.name.value = "Glazing")
        Assert.DoesNotContain(after, fun (c : MaterialCategory) -> c.name.value = "Glass")

    // ============================ pure: the strip-button launcher seam ============================

    [<Fact>]
    let ``the Materials and Library strip buttons reach their launchers as pure launches`` () =
        let calls, launchers = recordingLaunchers ()
        let m = { freshMain () with launchers = launchers }
        // The Materials-window request (spec 0038 step 013): a pure launch over the model's stores.
        let afterMaterials = updateTableAndElementRotation OpenMaterialsWindow m
        Assert.Equal<string list>([ "materials-window" ], List.ofSeq calls)
        Assert.Equal<TableAndElementRotationModel>(m, afterMaterials)
        // The Library-window request (spec 0038 step 015): likewise pure — the retired samples
        // workbench bay's verbs live in the window's own model now.
        calls.Clear()
        let afterLibrary = updateTableAndElementRotation OpenLibraryWindow m
        Assert.Equal<string list>([ "library-window" ], List.ofSeq calls)
        Assert.Equal<TableAndElementRotationModel>(m, afterLibrary)

    // ============================ pure: the Details-bay band projection ============================

    [<Fact>]
    let ``sampleBandsState collapses a period group to x-N bands (the Details-bay shape)`` () =
        let st = (BuiltinCatalogue.sample SampleIds.multilayerQwId).bandsState (BuiltinCatalogue.freshStores ()).materials
        Assert.Contains("Quarter-wave", st.title.value)
        // One band per unit-cell layer (kept collapsed, ×20) plus the closing single layer.
        Assert.Equal(3, List.length st.bands)
        Assert.Contains("×20", st.bands.[0].label.value)
        Assert.Contains("×20", st.bands.[1].label.value)
        Assert.DoesNotContain("×", st.bands.[2].label.value)

    [<Fact>]
    let ``the band view names each band's material through the materials store it is given`` () : unit =
        let sample = BuiltinCatalogue.sample SampleIds.multilayerQwId
        let bands = (sample.bandsState (freshStores ()).materials).bands
        Assert.Equal(3, List.length bands)
        Assert.StartsWith("Transparent glass (n = 1.52)", bands.[0].label.value)
        Assert.StartsWith("Vacuum", bands.[1].label.value)
        Assert.StartsWith("Transparent glass (n = 1.52)", bands.[2].label.value)

        let emptyMaterials = MaterialProxy.createInMemory (fun _ -> [])
        let unnamedBands = (sample.bandsState emptyMaterials).bands
        Assert.Equal(3, List.length unnamedBands)
        Assert.StartsWith(string MaterialIds.glass152.value, unnamedBands.[0].label.value)
        Assert.StartsWith(string MaterialIds.vacuumMaterial.value, unnamedBands.[1].label.value)
        Assert.StartsWith(string MaterialIds.glass152.value, unnamedBands.[2].label.value)

    // ============================ headless proofs (ui-smoke) ============================

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``headless acceptance: both workbench tabs are gone, both strip buttons ride the ribbon row, and the table canvas persists`` () =
        HeadlessSession.run (fun () ->
            let stores = freshStores ()
            let window = mountMain (mainWith stores)
            // Spec 0038 steps 013/015: NO Materials and NO Library ribbon tab any more — the
            // tab-strip row carries the two right-aligned window buttons instead.
            Assert.False(isPresent window (ConstructorUiId.ribbonTab (ConstructorUiKey "Materials")).value,
                         "the Materials bay tab must be gone from the ribbon")
            Assert.False(isPresent window (ConstructorUiId.ribbonTab (ConstructorUiKey "Library")).value,
                         "the Library bay tab must be gone from the ribbon")
            Assert.True(isPresent window ConstructorUiId.openMaterialsWindowButton.value,
                        "the ribbon strip row must carry the right-aligned Materials… button")
            Assert.True(isPresent window ConstructorUiId.openLibraryWindowButton.value,
                        "the ribbon strip row must carry the right-aligned Library… button")
            // With no full-surface bay left, every bay keeps the shared table canvas below the
            // strip — switching to the LAST bay (Details) and back never drops it.
            Assert.True(isPresent window ConstructorUiId.tableElementCanvas.value, "the default bay keeps the table canvas below the ribbon strip")
            clickOn window (ConstructorUiId.ribbonTab (ConstructorUiKey BayNames.details.value)).value
            Assert.True(isPresent window ConstructorUiId.tableElementCanvas.value, "an in-pane bay keeps the table canvas below the strip")
            clickOn window (ConstructorUiId.ribbonTab (ConstructorUiKey BayNames.rotation.value)).value
            Assert.True(isPresent window ConstructorUiId.tableElementCanvas.value, "returning to a table bay keeps the table canvas")
            window.Close())
