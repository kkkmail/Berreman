namespace OpticalConstructor.Ui.Tests

open Avalonia
open Avalonia.Controls
open Avalonia.Headless
open Avalonia.Threading
open Avalonia.VisualTree
open Avalonia.FuncUI
open Xunit
open OpticalConstructor.Controls

/// Spec 0033 (016) — the SampleLibraryControls component (UICOMP_XDUO_0002): the samples-workbench
/// list surface. Covers the pure control contract (empty state, the stable intent-named UiIds, the
/// `selectedRow` resolution) and the headless structure proof: mount `view` over a known `State`,
/// find every UiIds control, and observe that simulated clicks dispatch the MATCHING stub handler
/// (the functional-proxy seam — the control never touches a proxy). Wiring into a parent view is a
/// later step; these tests mount the component alone (the step-015 MaterialsControls precedent).
module SampleLibraryControlsTests =

    /// A control matches `id` by its `Name` OR its `AutomationProperties.AutomationId` (the verb /
    /// row / facet-option boxes carry an AutomationId — a mutable attached property — so they
    /// survive membership changes when the host's filter rewrites the rows; see the control).
    let private matchesId (id : string) (c : Control) : bool =
        c.Name = id || Avalonia.Automation.AutomationProperties.GetAutomationId(c) = id

    /// A known flattened row set the host's search produced: a plate, a multilayer thin film, and
    /// a wedge (one per domain `SubstrateKind`, as the host would flatten them).
    let private knownRows : SampleLibraryControls.Row list =
        [
            { sampleId = "glass-plate"; label = "Glass plate" }
            { sampleId = "multilayer-qw"; label = "Quarter-wave multilayer" }
            { sampleId = "quartz-wedge"; label = "Quartz wedge" }
        ]

    /// The known State the structure tests mount: the substrate facet populated (the host includes
    /// its own "all" option) and a row selected, so every row-targeted verb is live.
    let private knownState : SampleLibraryControls.State =
        {
            searchText = ""
            substrateOptions =
                [
                    { code = "all"; label = "All substrates" }
                    { code = "thin-film"; label = "Thin film" }
                    { code = "plate"; label = "Plate" }
                    { code = "wedge"; label = "Wedge" }
                ]
            selectedSubstrate = "all"
            rows = knownRows
            selectedId = Some "glass-plate"
        }

    /// Recording stub handlers (the test substitutes the functional-proxy seam): every dispatch
    /// appends a tag, so a click's MATCHING handler is observable.
    let private recorder () : ResizeArray<string> * SampleLibraryControls.Handlers =
        let calls = ResizeArray<string>()
        let handlers : SampleLibraryControls.Handlers =
            {
                setSearchText = fun t -> calls.Add("search:" + t)
                selectSubstrate = fun s -> calls.Add("substrate:" + s)
                selectSample = fun id -> calls.Add("select:" + id)
                addSample = fun () -> calls.Add("add")
                editSample = fun () -> calls.Add("edit")
                removeSample = fun () -> calls.Add("remove")
                viewSample = fun () -> calls.Add("view")
                makeMultilayer = fun () -> calls.Add("make-multilayer")
            }
        calls, handlers

    // ============================ pure control contract ============================

    [<Fact>]
    let ``the empty Samples state has no rows, no selection, an empty search and no facet options`` () =
        let s = SampleLibraryControls.empty
        Assert.Equal("", s.searchText)
        Assert.Empty(s.rows)
        Assert.Empty(s.substrateOptions)
        Assert.Equal("", s.selectedSubstrate)
        Assert.Equal(None, s.selectedId)

    [<Fact>]
    let ``the Samples UiIds are the stable intent-named ids`` () =
        Assert.Equal("SampleSearchBox", UiIds.SampleLibrary.searchBox)
        Assert.Equal("SampleSubstrateFilter", UiIds.SampleLibrary.substrateFilter)
        Assert.Equal("SamplesList", UiIds.SampleLibrary.list)
        Assert.Equal("AddSampleButton", UiIds.SampleLibrary.addButton)
        Assert.Equal("EditSampleButton", UiIds.SampleLibrary.editButton)
        Assert.Equal("RemoveSampleButton", UiIds.SampleLibrary.removeButton)
        Assert.Equal("ViewSampleButton", UiIds.SampleLibrary.viewButton)
        Assert.Equal("MakeMultilayerButton", UiIds.SampleLibrary.makeMultilayerButton)
        // The derived per-row / per-facet-option ids are prefixed so they cannot collide.
        Assert.Equal("SampleRow_abc", UiIds.SampleLibrary.row "abc")
        Assert.Equal("SampleSubstrateOption_plate", UiIds.SampleLibrary.substrateOption "plate")

    [<Fact>]
    let ``selectedRow resolves the selected id to its listed row, and None otherwise`` () =
        match SampleLibraryControls.selectedRow knownState with
        | Some r -> Assert.Equal("glass-plate", r.sampleId)
        | None -> Assert.Fail("expected the selected row")
        Assert.Equal(None, SampleLibraryControls.selectedRow { knownState with selectedId = None })
        // A selection the host's filter no longer lists resolves to no row (verbs then disable).
        Assert.Equal(None, SampleLibraryControls.selectedRow { knownState with selectedId = Some "not-listed" })

    // ============================ headless structure proof ============================

    /// Every fixed UiIds id the slice mandates (the derived row / facet-option ids are asserted
    /// through the clicks below).
    let private allFixedIds : string list =
        [
            UiIds.SampleLibrary.searchBox
            UiIds.SampleLibrary.substrateFilter
            UiIds.SampleLibrary.list
            UiIds.SampleLibrary.addButton
            UiIds.SampleLibrary.editButton
            UiIds.SampleLibrary.removeButton
            UiIds.SampleLibrary.viewButton
            UiIds.SampleLibrary.makeMultilayerButton
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
    let ``the Samples view mounts every UiIds control and each click dispatches its matching handler`` () =
        HeadlessSession.run (fun () ->
            let calls, handlers = recorder ()
            let window = Window(Width = 640.0, Height = 560.0)
            window.Content <- Component(fun _ -> SampleLibraryControls.view knownState handlers)
            window.Show()
            Dispatcher.UIThread.RunJobs()
            // Every slice-mandated control exists over the known State.
            for id in allFixedIds do
                Assert.True(isPresent window id, $"%s{id} is missing from the mounted view")
            let count (tag : string) : int = calls |> Seq.filter ((=) tag) |> Seq.length
            // The acceptance click: Add invokes the ADD handler — and no other verb handler.
            clickOn window UiIds.SampleLibrary.addButton
            Assert.Equal(1, count "add")
            Assert.Equal(0, count "edit")
            Assert.Equal(0, count "remove")
            Assert.Equal(0, count "view")
            Assert.Equal(0, count "make-multilayer")
            // Each remaining verb dispatches ITS handler (a row is selected, so all are live).
            clickOn window UiIds.SampleLibrary.editButton
            Assert.Equal(1, count "edit")
            clickOn window UiIds.SampleLibrary.removeButton
            Assert.Equal(1, count "remove")
            clickOn window UiIds.SampleLibrary.viewButton
            Assert.Equal(1, count "view")
            clickOn window UiIds.SampleLibrary.makeMultilayerButton
            Assert.Equal(1, count "make-multilayer")
            // A row click selects THAT sample; a facet-option click selects THAT substrate code.
            clickOn window (UiIds.SampleLibrary.row "multilayer-qw")
            Assert.Contains("select:multilayer-qw", calls)
            clickOn window (UiIds.SampleLibrary.substrateOption "plate")
            Assert.Contains("substrate:plate", calls)
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``with no selection the row-targeted verbs are inert while Add and Make-multilayer stay live`` () =
        HeadlessSession.run (fun () ->
            let calls, handlers = recorder ()
            let state = { knownState with selectedId = None }
            let window = Window(Width = 640.0, Height = 560.0)
            window.Content <- Component(fun _ -> SampleLibraryControls.view state handlers)
            window.Show()
            Dispatcher.UIThread.RunJobs()
            let count (tag : string) : int = calls |> Seq.filter ((=) tag) |> Seq.length
            // The row-targeted verbs are present but DISABLED — a click dispatches nothing.
            clickOn window UiIds.SampleLibrary.editButton
            clickOn window UiIds.SampleLibrary.removeButton
            clickOn window UiIds.SampleLibrary.viewButton
            Assert.Equal(0, count "edit")
            Assert.Equal(0, count "remove")
            Assert.Equal(0, count "view")
            // The two creation entry points need no selection.
            clickOn window UiIds.SampleLibrary.addButton
            Assert.Equal(1, count "add")
            clickOn window UiIds.SampleLibrary.makeMultilayerButton
            Assert.Equal(1, count "make-multilayer")
            window.Close())
