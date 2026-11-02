namespace OpticalConstructor.Ui.Tests

open Avalonia
open Avalonia.Controls
open Avalonia.Headless
open Avalonia.Threading
open Avalonia.VisualTree
open Avalonia.FuncUI
open Xunit
open OpticalConstructor.Controls

/// Spec 0033 (015) — the MaterialsControls component (UICOMP_XDUO_0001): the materials-workbench
/// list surface. Covers the pure control contract (empty state, the stable intent-named UiIds, the
/// `selectedRow` resolution) and the headless structure proof: mount `view` over a known `State`,
/// find every UiIds control, and observe that simulated clicks dispatch the MATCHING stub handler
/// (the functional-proxy seam — the control never touches a proxy). Wiring into a parent view is a
/// later step; these tests mount the component alone.
module MaterialsControlsTests =

    /// A control matches `id` by its `Name` OR its `AutomationProperties.AutomationId` (the verb /
    /// row / facet-option boxes carry an AutomationId — a mutable attached property — so they survive
    /// membership changes such as the Edit verb's removal; see the control).
    let private matchesId (id : string) (c : Control) : bool =
        c.Name = id || Avalonia.Automation.AutomationProperties.GetAutomationId(c) = id

    /// A known flattened row set: two editable materials and one view-only (no edit model — the
    /// spec-0033 step-013 `complexity = None` shape the host flattens to `ViewOnly`).
    let private knownRows : MaterialsControls.MaterialsControlsRow list =
        [
            { materialId = "bk7"; label = "BK7 glass"; editability = MaterialsControls.Editable }
            { materialId = "gold"; label = "Gold"; editability = MaterialsControls.Editable }
            { materialId = "silicon"; label = "Silicon (tabulated)"; editability = MaterialsControls.ViewOnly }
        ]

    /// The known State the structure tests mount: both facets populated, an EDITABLE row selected
    /// (so every verb — including Edit — is present).
    let private knownState : MaterialsControls.MaterialsControlsState =
        {
            searchText = ""
            categoryOptions =
                [
                    { code = "all"; label = "All categories" }
                    { code = "glass"; label = "Glass" }
                    { code = "metal"; label = "Metal" }
                ]
            selectedCategory = "all"
            dispersionOptions =
                [
                    { code = "any"; label = "Any" }
                    { code = "dispersive"; label = "Dispersive" }
                    { code = "non-dispersive"; label = "Non-dispersive" }
                ]
            selectedDispersion = "any"
            rows = knownRows
            selectedId = Some "bk7"
        }

    /// Recording stub handlers (the test substitutes the functional-proxy seam): every dispatch
    /// appends a tag, so a click's MATCHING handler is observable.
    let private recorder () : ResizeArray<string> * MaterialsControls.MaterialsControlsHandlers =
        let calls = ResizeArray<string>()
        let handlers : MaterialsControls.MaterialsControlsHandlers =
            {
                setSearchText = fun t -> calls.Add("search:" + t)
                selectCategory = fun c -> calls.Add("category:" + c)
                selectDispersion = fun d -> calls.Add("dispersion:" + d)
                selectMaterial = fun id -> calls.Add("select:" + id)
                addMaterial = fun () -> calls.Add("add")
                editMaterial = fun () -> calls.Add("edit")
                removeMaterial = fun () -> calls.Add("remove")
                viewMaterial = fun () -> calls.Add("view")
            }
        calls, handlers

    // ============================ pure control contract ============================

    [<Fact>]
    let ``the empty Materials state has no rows, no selection, an empty search and no facet options`` () =
        let s = MaterialsControls.emptyMaterialsControls
        Assert.Equal("", s.searchText)
        Assert.Empty(s.rows)
        Assert.Empty(s.categoryOptions)
        Assert.Empty(s.dispersionOptions)
        Assert.Equal("", s.selectedCategory)
        Assert.Equal("", s.selectedDispersion)
        Assert.Equal(None, s.selectedId)

    [<Fact>]
    let ``the Materials UiIds are the stable intent-named ids`` () =
        Assert.Equal("MaterialSearchBox", ConstructorUiId.materialSearchBox.value)
        Assert.Equal("MaterialCategoryFilter", ConstructorUiId.materialCategoryFilter.value)
        Assert.Equal("MaterialDispersionFilter", ConstructorUiId.materialDispersionFilter.value)
        Assert.Equal("MaterialsList", ConstructorUiId.materialsList.value)
        Assert.Equal("AddMaterialButton", ConstructorUiId.addMaterialButton.value)
        Assert.Equal("EditMaterialButton", ConstructorUiId.editMaterialButton.value)
        Assert.Equal("RemoveMaterialButton", ConstructorUiId.removeMaterialButton.value)
        Assert.Equal("ViewMaterialButton", ConstructorUiId.viewMaterialButton.value)
        // The derived per-row / per-facet-option ids are prefixed so they cannot collide.
        Assert.Equal("MaterialRow_abc", (ConstructorUiId.materialRow (ConstructorUiKey "abc")).value)
        Assert.Equal("MaterialCategoryOption_glass", (ConstructorUiId.materialCategoryOption (ConstructorUiKey "glass")).value)
        Assert.Equal("MaterialDispersionOption_any", (ConstructorUiId.materialDispersionOption (ConstructorUiKey "any")).value)

    [<Fact>]
    let ``selectedRow resolves the selected id to its listed row, and None otherwise`` () =
        match MaterialsControls.selectedRow knownState with
        | Some r -> Assert.Equal("bk7", r.materialId)
        | None -> Assert.Fail("expected the selected row")
        Assert.Equal(None, MaterialsControls.selectedRow { knownState with selectedId = None })
        // A selection the host's filter no longer lists resolves to no row (verbs then disable).
        Assert.Equal(None, MaterialsControls.selectedRow { knownState with selectedId = Some "not-listed" })

    // ============================ headless structure proof ============================

    /// Every fixed UiIds id the slice mandates (the derived row / facet-option ids are asserted
    /// through the clicks below).
    let private allFixedIds : string list =
        [
            ConstructorUiId.materialSearchBox.value
            ConstructorUiId.materialCategoryFilter.value
            ConstructorUiId.materialDispersionFilter.value
            ConstructorUiId.materialsList.value
            ConstructorUiId.addMaterialButton.value
            ConstructorUiId.editMaterialButton.value
            ConstructorUiId.removeMaterialButton.value
            ConstructorUiId.viewMaterialButton.value
        ]

    let private isPresent (window : Window) (id : string) : bool =
        window.GetVisualDescendants()
        |> Seq.exists (function :? Control as c -> matchesId id c | _ -> false)

    /// Click the centre of the control carrying `id` (by Name or AutomationId).
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
                window.MouseUp(c.Value, Avalonia.Input.MouseButton.Left, Avalonia.Input.RawInputModifiers.None)
                Dispatcher.UIThread.RunJobs()
            else Assert.Fail($"%s{id} has no on-screen position")

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the Materials view mounts every UiIds control and each click dispatches its matching handler`` () =
        HeadlessSession.run (fun () ->
            let calls, handlers = recorder ()
            let window = Window(Width = 640.0, Height = 560.0)
            window.Content <- Component(fun _ -> MaterialsControls.materialsControlsView knownState handlers)
            window.Show()
            Dispatcher.UIThread.RunJobs()
            // Every slice-mandated control exists over the known State.
            for id in allFixedIds do
                Assert.True(isPresent window id, $"%s{id} is missing from the mounted view")
            let count (tag : string) : int = calls |> Seq.filter ((=) tag) |> Seq.length
            // The acceptance click: Add invokes the ADD handler — and no other verb handler.
            clickOn window ConstructorUiId.addMaterialButton.value
            Assert.Equal(1, count "add")
            Assert.Equal(0, count "edit")
            Assert.Equal(0, count "remove")
            Assert.Equal(0, count "view")
            // Each remaining verb dispatches ITS handler (the selected row is editable, so Edit is live).
            clickOn window ConstructorUiId.editMaterialButton.value
            Assert.Equal(1, count "edit")
            clickOn window ConstructorUiId.removeMaterialButton.value
            Assert.Equal(1, count "remove")
            clickOn window ConstructorUiId.viewMaterialButton.value
            Assert.Equal(1, count "view")
            // A row click selects THAT material; a facet-option click selects THAT facet code.
            clickOn window (ConstructorUiId.materialRow (ConstructorUiKey "gold")).value
            Assert.Contains("select:gold", calls)
            clickOn window (ConstructorUiId.materialCategoryOption (ConstructorUiKey "metal")).value
            Assert.Contains("category:metal", calls)
            clickOn window (ConstructorUiId.materialDispersionOption (ConstructorUiKey "dispersive")).value
            Assert.Contains("dispersion:dispersive", calls)
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``a view-only selection carries no Edit affordance — the button is removed, not greyed`` () =
        HeadlessSession.run (fun () ->
            let _, handlers = recorder ()
            let state = { knownState with selectedId = Some "silicon" }   // the view-only row
            let window = Window(Width = 640.0, Height = 560.0)
            window.Content <- Component(fun _ -> MaterialsControls.materialsControlsView state handlers)
            window.Show()
            Dispatcher.UIThread.RunJobs()
            // REMOVED means absent from the tree entirely — a greyed/hidden button would still be found.
            Assert.False(
                isPresent window ConstructorUiId.editMaterialButton.value,
                "the Edit verb must be REMOVED (not greyed) for a view-only selection")
            // The other verbs remain for the view-only entry.
            Assert.True(isPresent window ConstructorUiId.addMaterialButton.value)
            Assert.True(isPresent window ConstructorUiId.removeMaterialButton.value)
            Assert.True(isPresent window ConstructorUiId.viewMaterialButton.value)
            window.Close())
