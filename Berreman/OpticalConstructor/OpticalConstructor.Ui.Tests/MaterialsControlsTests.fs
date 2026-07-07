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
    let private knownRows : MaterialsControls.Row list =
        [
            { materialId = "bk7"; label = "BK7 glass"; editability = MaterialsControls.Editable }
            { materialId = "gold"; label = "Gold"; editability = MaterialsControls.Editable }
            { materialId = "silicon"; label = "Silicon (tabulated)"; editability = MaterialsControls.ViewOnly }
        ]

    /// The known State the structure tests mount: both facets populated, an EDITABLE row selected
    /// (so every verb — including Edit — is present).
    let private knownState : MaterialsControls.State =
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
    let private recorder () : ResizeArray<string> * MaterialsControls.Handlers =
        let calls = ResizeArray<string>()
        let handlers : MaterialsControls.Handlers =
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
        let s = MaterialsControls.empty
        Assert.Equal("", s.searchText)
        Assert.Empty(s.rows)
        Assert.Empty(s.categoryOptions)
        Assert.Empty(s.dispersionOptions)
        Assert.Equal("", s.selectedCategory)
        Assert.Equal("", s.selectedDispersion)
        Assert.Equal(None, s.selectedId)

    [<Fact>]
    let ``the Materials UiIds are the stable intent-named ids`` () =
        Assert.Equal("MaterialSearchBox", MaterialsControls.UiIds.searchBox)
        Assert.Equal("MaterialCategoryFilter", MaterialsControls.UiIds.categoryFilter)
        Assert.Equal("MaterialDispersionFilter", MaterialsControls.UiIds.dispersionFilter)
        Assert.Equal("MaterialsList", MaterialsControls.UiIds.list)
        Assert.Equal("AddMaterialButton", MaterialsControls.UiIds.addButton)
        Assert.Equal("EditMaterialButton", MaterialsControls.UiIds.editButton)
        Assert.Equal("RemoveMaterialButton", MaterialsControls.UiIds.removeButton)
        Assert.Equal("ViewMaterialButton", MaterialsControls.UiIds.viewButton)
        // The derived per-row / per-facet-option ids are prefixed so they cannot collide.
        Assert.Equal("MaterialRow_abc", MaterialsControls.UiIds.row "abc")
        Assert.Equal("MaterialCategoryOption_glass", MaterialsControls.UiIds.categoryOption "glass")
        Assert.Equal("MaterialDispersionOption_any", MaterialsControls.UiIds.dispersionOption "any")

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
            MaterialsControls.UiIds.searchBox
            MaterialsControls.UiIds.categoryFilter
            MaterialsControls.UiIds.dispersionFilter
            MaterialsControls.UiIds.list
            MaterialsControls.UiIds.addButton
            MaterialsControls.UiIds.editButton
            MaterialsControls.UiIds.removeButton
            MaterialsControls.UiIds.viewButton
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
            window.Content <- Component(fun _ -> MaterialsControls.view knownState handlers)
            window.Show()
            Dispatcher.UIThread.RunJobs()
            // Every slice-mandated control exists over the known State.
            for id in allFixedIds do
                Assert.True(isPresent window id, $"%s{id} is missing from the mounted view")
            let count (tag : string) : int = calls |> Seq.filter ((=) tag) |> Seq.length
            // The acceptance click: Add invokes the ADD handler — and no other verb handler.
            clickOn window MaterialsControls.UiIds.addButton
            Assert.Equal(1, count "add")
            Assert.Equal(0, count "edit")
            Assert.Equal(0, count "remove")
            Assert.Equal(0, count "view")
            // Each remaining verb dispatches ITS handler (the selected row is editable, so Edit is live).
            clickOn window MaterialsControls.UiIds.editButton
            Assert.Equal(1, count "edit")
            clickOn window MaterialsControls.UiIds.removeButton
            Assert.Equal(1, count "remove")
            clickOn window MaterialsControls.UiIds.viewButton
            Assert.Equal(1, count "view")
            // A row click selects THAT material; a facet-option click selects THAT facet code.
            clickOn window (MaterialsControls.UiIds.row "gold")
            Assert.Contains("select:gold", calls)
            clickOn window (MaterialsControls.UiIds.categoryOption "metal")
            Assert.Contains("category:metal", calls)
            clickOn window (MaterialsControls.UiIds.dispersionOption "dispersive")
            Assert.Contains("dispersion:dispersive", calls)
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``a view-only selection carries no Edit affordance — the button is removed, not greyed`` () =
        HeadlessSession.run (fun () ->
            let _, handlers = recorder ()
            let state = { knownState with selectedId = Some "silicon" }   // the view-only row
            let window = Window(Width = 640.0, Height = 560.0)
            window.Content <- Component(fun _ -> MaterialsControls.view state handlers)
            window.Show()
            Dispatcher.UIThread.RunJobs()
            // REMOVED means absent from the tree entirely — a greyed/hidden button would still be found.
            Assert.False(
                isPresent window MaterialsControls.UiIds.editButton,
                "the Edit verb must be REMOVED (not greyed) for a view-only selection")
            // The other verbs remain for the view-only entry.
            Assert.True(isPresent window MaterialsControls.UiIds.addButton)
            Assert.True(isPresent window MaterialsControls.UiIds.removeButton)
            Assert.True(isPresent window MaterialsControls.UiIds.viewButton)
            window.Close())
