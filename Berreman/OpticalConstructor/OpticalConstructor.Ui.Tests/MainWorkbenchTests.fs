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
open Softellect.Sys.Primitives
open OpticalConstructor.Domain
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.CatalogueImport
open OpticalConstructor.Domain.MaterialStore
open OpticalConstructor.Controls
open OpticalConstructor.Storage.CatalogueImportStore
open OpticalConstructor.Tests
open OpticalConstructor.Ui
open OpticalConstructor.Ui.TableAndElementRotationView
open OpticalConstructor.Ui.Tests.UiDriver

/// Spec 0033 (024) / 0038 (013/015) — the Main-screen workbench composition around the ribbon.
/// The MATERIALS bay these tests used to cover became the single-instance Materials WINDOW
/// (spec 0038 step 013, `MaterialsWindowTests`) and the LIBRARY (samples) workbench bay the
/// single-instance Library WINDOW (spec 0038 step 015, `LibraryWindowTests`); what remains here
/// is the workbench side: the bay roster without either workbench bay, the two strip buttons'
/// launcher seam, the material-editor create picker over the live catalogue, and the
/// Details-bay band-state projection.
module MainWorkbenchTests =

    /// Fresh, isolated stores imported from the Built-in catalogue as in the App (spec 0050 §3.4–3.5).
    let private freshStores () : InMemoryCatalogueStores =
        BuiltinCatalogue.freshStores ()

    /// Composes Main over a picked kit and the supplied live stores (spec 0050 §3.4–3.5, S4.3).
    let private mainWith (stores : InMemoryCatalogueStores) : TableAndElementRotationModel =
        let owner = FolderName "MainWorkbenchTests"
        let imports = CatalogueImportContext.create stores.proxy
        let opened = TestAppScope.openedKitIn owner imports
        let sources = SourceProxy.createInMemory ()
        let seed =
            initMainWithKit
                (liveLibrary stores.libraryEntries stores.samples sources)
                (Experiments.createInMemoryExperiments ())
                stores.materials stores.samples stores.categories opened.library.kit
        let model =
            {
                seed with
                    sources = sources
                    catalogueImports = imports
            }
        model.loadKit opened

    let private freshMain () : TableAndElementRotationModel =
        freshStores () |> mainWith

    /// Mount the REAL Main-screen MVU loop over `model0` in a headless `HostWindow`, so a click
    /// dispatches through `update` and the view re-renders in the same pass (the
    /// `MainConstructorWindow` shape, with injectable stores / launchers).
    let private mountMain (model0 : TableAndElementRotationModel) : HostWindow =
        let window = HostWindow(Width = 980.0, Height = 1050.0)
        window.Closed.Add(fun _ -> model0.kit.close () |> SceneTestFixture.kitSuccess)
        Program.mkSimple (fun () -> model0) updateTableAndElementRotation UiDriver.editableTableView
        |> Program.withHost window
        |> Program.run
        window.Show()
        Dispatcher.UIThread.RunJobs()
        window

    // ============================ pure: bay roster ============================

    [<Fact>]
    let ``the ribbon offers NO workbench bay any more — Materials and Library are the Launcher's windows`` () =
        // Spec 0038 steps 013/015: the Materials bay left the ribbon at step 013 and the
        // Library (samples workbench) bay leaves with step 015 — both workbenches are
        // single-instance WINDOWS now, and no full-surface bay remains.
        Assert.DoesNotContain(Ribbon.BayName "Materials", BayNames.allBayNames)
        Assert.DoesNotContain(Ribbon.BayName "Library", BayNames.allBayNames)
        Assert.DoesNotContain(Ribbon.BayName "Select", BayNames.allBayNames)
        Assert.Equal(Some BayNames.details, List.tryLast BayNames.allBayNames)
        let m = freshMain ()
        try
            let bays = mainBays m ignore
            Assert.Equal<Ribbon.BayName list>(BayNames.allBayNames, bays |> List.map (fun b -> b.name))
            for bay in bays do
                Assert.Equal(Ribbon.InRibbonPane, bay.mode)
        finally m.kit.close () |> SceneTestFixture.kitSuccess

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
    let ``headless acceptance: both workbench tabs are gone, the strip buttons too, and the table canvas persists`` () =
        HeadlessSession.run (fun () ->
            let stores = freshStores ()
            let window = mountMain (mainWith stores)
            try
                Assert.False(isPresent window (ConstructorUiId.ribbonTab (ConstructorUiKey "Materials")).value,
                             "the Materials bay tab must be gone from the ribbon")
                Assert.False(isPresent window (ConstructorUiId.ribbonTab (ConstructorUiKey "Library")).value,
                             "the Library bay tab must be gone from the ribbon")
                // Spec 0051 `.manual/007` 2.3: the Launcher alone opens the Materials and Library windows.
                Assert.False(isPresent window "OpenMaterialsWindowButton", "the ribbon row carries no Materials… button")
                Assert.False(isPresent window "OpenLibraryWindowButton", "the ribbon row carries no Library… button")
                Assert.True(isPresent window ConstructorUiId.tableElementCanvas.value, "the default bay keeps the table canvas below the ribbon strip")
                clickOn window (ConstructorUiId.ribbonTab (ConstructorUiKey BayNames.details.value)).value
                Assert.True(isPresent window ConstructorUiId.tableElementCanvas.value, "an in-pane bay keeps the table canvas below the strip")
                clickOn window (ConstructorUiId.ribbonTab (ConstructorUiKey BayNames.rotation.value)).value
                Assert.True(isPresent window ConstructorUiId.tableElementCanvas.value, "returning to a table bay keeps the table canvas")
            finally window.Close())
