namespace OpticalConstructor.Ui.Tests

open Avalonia
open Avalonia.Controls
open Avalonia.Headless
open Avalonia.Input
open Avalonia.Threading
open Avalonia.VisualTree
open Avalonia.FuncUI
open Xunit
open OpticalConstructor.Controls

/// Spec 0038 Part E (012) — the FacetedTreeControls component (UICOMP_XDUO_0008): the domain-free
/// faceted, filterable, reconfigurable tree. Covers the pure control contract (empty state, the
/// stable intent-named UiIds + derived per-item ids) and the headless structure proofs: mount
/// `view` over a known `State`, locate every declared automation id, observe that a chip click
/// dispatches removeConstraint / an offered-value click dispatches applyConstraint / the gated
/// mode's Show/Search click dispatches requestBuild, that gated mode renders ZERO tree rows, and
/// that the filter box NEVER commits per keystroke (Enter/LostFocus only). Wiring into the
/// Materials / Library windows is a later step; these tests mount the component alone over stub
/// handlers (the functional-proxy seam — the control never touches a proxy or the facet engine).
module FacetedTreeControlsTests =

    /// A control matches `id` by its `Name` OR its `AutomationProperties.AutomationId` (everything
    /// in this control carries an AutomationId — a mutable attached property — so chips, offers,
    /// and rows survive the host's membership changes; see the control).
    let private matchesId (id : string) (c : Control) : bool =
        c.Name = id || Avalonia.Automation.AutomationProperties.GetAutomationId(c) = id

    // ============================ the known State ============================

    /// A known projected tree: a heading (no count) over two branches — "Samples (2)" expanded
    /// with two leaves, "Polarizers (1)" COLLAPSED with one child that must NOT render (but the
    /// collapsed row still carries its count).
    let private knownTree : FacetedTreeControls.TreeNode list =
        [
            {
                code = "kind"
                label = "Kind"
                countOpt = None
                expansion = FacetedTreeControls.ExpandedNode
                children =
                    [
                        {
                            code = "kind/sample"
                            label = "Samples"
                            countOpt = Some 2
                            expansion = FacetedTreeControls.ExpandedNode
                            children =
                                [
                                    {
                                        code = "kind/sample/quartz"
                                        label = "Quartz plate"
                                        countOpt = None
                                        expansion = FacetedTreeControls.ExpandedNode
                                        children = []
                                    }
                                    {
                                        code = "kind/sample/mica"
                                        label = "Mica wedge"
                                        countOpt = None
                                        expansion = FacetedTreeControls.ExpandedNode
                                        children = []
                                    }
                                ]
                        }
                        {
                            code = "kind/polarizer"
                            label = "Polarizers"
                            countOpt = Some 1
                            expansion = FacetedTreeControls.CollapsedNode
                            children =
                                [
                                    {
                                        code = "kind/polarizer/lp"
                                        label = "LP sheet"
                                        countOpt = None
                                        expansion = FacetedTreeControls.ExpandedNode
                                        children = []
                                    }
                                ]
                        }
                    ]
            }
        ]

    /// The known State the structure tests mount: two applied chips with after-counts, a discrete
    /// and a numeric offer group (the numeric one offering the manual min–max box), two named
    /// representations with one active, a committed filter draft, a live result count, and the
    /// tree materialized.
    let private knownState : FacetedTreeControls.State =
        {
            tree = knownTree
            breadcrumbs =
                [
                    { code = "anisotropy"; label = "Anisotropy: Uniaxial"; afterCount = 12 }
                    { code = "text-filter"; label = "Text: quartz"; afterCount = 3 }
                ]
            offers =
                [
                    {
                        code = "category"
                        title = "Category"
                        values =
                            [
                                { code = "glass"; label = "Glass"; previewCount = 2 }
                                { code = "metal"; label = "Metal"; previewCount = 1 }
                            ]
                        manualRange = FacetedTreeControls.NoManualRange
                    }
                    {
                        code = "thickness"
                        title = "Film thickness"
                        values = [ { code = "b10-20"; label = "10-20 nm"; previewCount = 2 } ]
                        manualRange = FacetedTreeControls.ManualRangeOffered
                    }
                ]
            representations =
                [
                    { code = "by-kind"; label = "By kind" }
                    { code = "by-material"; label = "By material" }
                ]
            activeRepresentation = "by-kind"
            filterDraft = "quartz"
            resultCount = 3
            materialization = FacetedTreeControls.TreeMaterialized
        }

    /// Recording stub handlers (the test substitutes the functional-proxy seam): every dispatch
    /// appends a tag, so a click's / commit's MATCHING handler is observable.
    let private recorder () : ResizeArray<string> * FacetedTreeControls.Handlers =
        let calls = ResizeArray<string>()
        let handlers : FacetedTreeControls.Handlers =
            {
                applyConstraint = fun group value -> calls.Add($"apply:%s{group}=%s{value}")
                removeConstraint = fun chip -> calls.Add("remove:" + chip)
                commitTextFilter = fun text -> calls.Add("filter:" + text)
                chooseRepresentation = fun code -> calls.Add("repr:" + code)
                requestBuild = fun () -> calls.Add("build")
                selectNode = fun code -> calls.Add("node:" + code)
                applyManualRange = fun group text -> calls.Add($"range:%s{group}=%s{text}")
            }
        calls, handlers

    // ============================ pure control contract ============================

    [<Fact>]
    let ``the empty FacetedTree state has nothing applied, nothing offered, no tree, and is materialized`` () =
        let s = FacetedTreeControls.empty
        Assert.Empty(s.tree)
        Assert.Empty(s.breadcrumbs)
        Assert.Empty(s.offers)
        Assert.Empty(s.representations)
        Assert.Equal("", s.activeRepresentation)
        Assert.Equal("", s.filterDraft)
        Assert.Equal(0, s.resultCount)
        Assert.Equal(FacetedTreeControls.TreeMaterialized, s.materialization)

    [<Fact>]
    let ``the FacetedTree UiIds are the stable intent-named ids`` () =
        Assert.Equal("FacetFilterBox", UiIds.FacetedTree.filterBox)
        Assert.Equal("FacetRepresentationPicker", UiIds.FacetedTree.representationPicker)
        Assert.Equal("FacetBreadcrumbStrip", UiIds.FacetedTree.breadcrumbStrip)
        Assert.Equal("FacetResultCount", UiIds.FacetedTree.resultCount)
        Assert.Equal("FacetOffersPanel", UiIds.FacetedTree.offersPanel)
        Assert.Equal("FacetShowTreeButton", UiIds.FacetedTree.showTreeButton)
        Assert.Equal("FacetTree", UiIds.FacetedTree.tree)
        Assert.Equal("FacetScroll", UiIds.FacetedTree.scrollViewer)
        // The derived per-item ids are prefixed so they cannot collide; the offered-value id
        // carries the GROUP code too (the same discrete key can appear under two facets).
        Assert.Equal("FacetRepresentationOption_by-kind", UiIds.FacetedTree.representationOption "by-kind")
        Assert.Equal("FacetBreadcrumbChip_anisotropy", UiIds.FacetedTree.breadcrumbChip "anisotropy")
        Assert.Equal("FacetOfferGroup_category", UiIds.FacetedTree.offerGroup "category")
        Assert.Equal("FacetOfferedValue_category_glass", UiIds.FacetedTree.offeredValue "category" "glass")
        Assert.Equal("FacetManualRangeBox_thickness", UiIds.FacetedTree.manualRangeBox "thickness")
        Assert.Equal("FacetTreeNode_kind/sample", UiIds.FacetedTree.treeNode "kind/sample")

    // ============================ headless structure proofs ============================

    let private isPresent (window : Window) (id : string) : bool =
        window.GetVisualDescendants()
        |> Seq.exists (function :? Control as c -> matchesId id c | _ -> false)

    /// The number of generated tree node rows (any control whose AutomationId carries the
    /// FacetTreeNode_ prefix) — gated mode must render ZERO.
    let private treeRowCount (window : Window) : int =
        window.GetVisualDescendants()
        |> Seq.filter (fun v ->
            match v with
            | :? Control as c ->
                let autoId = Avalonia.Automation.AutomationProperties.GetAutomationId(c)
                not (isNull autoId) && autoId.StartsWith("FacetTreeNode_")
            | _ -> false)
        |> Seq.length

    /// The display text under the control carrying `id` (the control itself when it is a
    /// TextBlock, its TextBlock child otherwise).
    let private textOf (window : Window) (id : string) : string =
        let found =
            window.GetVisualDescendants()
            |> Seq.tryPick (function :? Control as c when matchesId id c -> Some c | _ -> None)
        match found with
        | None -> ""
        | Some (:? TextBlock as t) -> (if isNull t.Text then "" else t.Text)
        | Some c ->
            c.GetVisualDescendants()
            |> Seq.tryPick (function :? TextBlock as t when not (isNull t.Text) -> Some t.Text | _ -> None)
            |> Option.defaultValue ""

    /// The TextBox carrying `id` (fails the test when it is missing).
    let private textBoxOf (window : Window) (id : string) : TextBox =
        let found =
            window.GetVisualDescendants()
            |> Seq.tryPick (function :? TextBox as tb when matchesId id tb -> Some tb | _ -> None)
        match found with
        | Some tb -> tb
        | None -> failwith $"%s{id} TextBox was not found"

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

    let private mount (state : FacetedTreeControls.State) (handlers : FacetedTreeControls.Handlers) : Window =
        let window = Window(Width = 760.0, Height = 700.0)
        window.Content <- Component(fun _ -> FacetedTreeControls.view state handlers)
        window.Show()
        Dispatcher.UIThread.RunJobs()
        window

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the FacetedTree view mounts every declared automation id and each click dispatches its matching handler`` () =
        HeadlessSession.run (fun () ->
            let calls, handlers = recorder ()
            let window = mount knownState handlers
            // Every fixed id exists over the known State (the Show/Search button does NOT — the
            // tree is materialized, so the button is its gated-mode replacement).
            let fixedIds =
                [
                    UiIds.FacetedTree.scrollViewer
                    UiIds.FacetedTree.filterBox
                    UiIds.FacetedTree.representationPicker
                    UiIds.FacetedTree.breadcrumbStrip
                    UiIds.FacetedTree.resultCount
                    UiIds.FacetedTree.offersPanel
                    UiIds.FacetedTree.tree
                ]
            for id in fixedIds do
                Assert.True(isPresent window id, $"%s{id} is missing from the mounted view")
            Assert.False(
                isPresent window UiIds.FacetedTree.showTreeButton,
                "the Show/Search button must be ABSENT while the tree is materialized")
            // Every derived id over the known State: representation options, chips, offer groups,
            // offered values, the numeric group's manual range box (and ONLY the numeric group's),
            // and the rendered tree rows.
            for r in knownState.representations do
                Assert.True(isPresent window (UiIds.FacetedTree.representationOption r.code), $"representation %s{r.code} is missing")
            for chip in knownState.breadcrumbs do
                Assert.True(isPresent window (UiIds.FacetedTree.breadcrumbChip chip.code), $"chip %s{chip.code} is missing")
            for group in knownState.offers do
                Assert.True(isPresent window (UiIds.FacetedTree.offerGroup group.code), $"offer group %s{group.code} is missing")
                for v in group.values do
                    Assert.True(isPresent window (UiIds.FacetedTree.offeredValue group.code v.code), $"offered value %s{group.code}/%s{v.code} is missing")
            Assert.True(isPresent window (UiIds.FacetedTree.manualRangeBox "thickness"), "the numeric group's manual range box is missing")
            Assert.False(isPresent window (UiIds.FacetedTree.manualRangeBox "category"), "a discrete group must offer NO manual range box")
            for code in [ "kind"; "kind/sample"; "kind/sample/quartz"; "kind/sample/mica"; "kind/polarizer" ] do
                Assert.True(isPresent window (UiIds.FacetedTree.treeNode code), $"tree row %s{code} is missing")
            // A collapsed branch renders NO children — but still carries its count in its row text;
            // an expanded branch reads `label (count)`; a heading without a count reads its bare label.
            Assert.False(
                isPresent window (UiIds.FacetedTree.treeNode "kind/polarizer/lp"),
                "a collapsed branch's child must not render")
            Assert.Equal("Samples (2)", textOf window (UiIds.FacetedTree.treeNode "kind/sample"))
            Assert.Equal("Polarizers (1)", textOf window (UiIds.FacetedTree.treeNode "kind/polarizer"))
            Assert.Equal("Kind", textOf window (UiIds.FacetedTree.treeNode "kind"))
            // The chips read `label (afterCount) ×`; the live result count and the count-previews show.
            Assert.Equal("Anisotropy: Uniaxial (12) ×", textOf window (UiIds.FacetedTree.breadcrumbChip "anisotropy"))
            Assert.Equal("3 results", textOf window UiIds.FacetedTree.resultCount)
            Assert.Equal("Glass (2)", textOf window (UiIds.FacetedTree.offeredValue "category" "glass"))
            // The filter box echoes the host's committed draft.
            Assert.Equal("quartz", (textBoxOf window UiIds.FacetedTree.filterBox).Text)
            // The acceptance clicks: a chip click dispatches removeConstraint with ITS code; an
            // offered-value click dispatches applyConstraint with its group AND value codes — and
            // nothing else fires.
            clickOn window (UiIds.FacetedTree.breadcrumbChip "anisotropy")
            Assert.Equal<string>([ "remove:anisotropy" ], calls)
            clickOn window (UiIds.FacetedTree.offeredValue "category" "glass")
            Assert.Equal<string>([ "remove:anisotropy"; "apply:category=glass" ], calls)
            // A representation click chooses THAT representation; a tree row click selects THAT node.
            clickOn window (UiIds.FacetedTree.representationOption "by-material")
            Assert.Contains("repr:by-material", calls)
            clickOn window (UiIds.FacetedTree.treeNode "kind/sample")
            Assert.Contains("node:kind/sample", calls)
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``gated materialization shows the Show-Search button INSTEAD of tree rows and its click dispatches requestBuild`` () =
        HeadlessSession.run (fun () ->
            let calls, handlers = recorder ()
            // The state still CARRIES the tree — gating alone must keep every row unrendered.
            let window = mount { knownState with materialization = FacetedTreeControls.TreeGated } handlers
            Assert.True(isPresent window UiIds.FacetedTree.showTreeButton, "the Show/Search button is missing in gated mode")
            Assert.False(isPresent window UiIds.FacetedTree.tree, "the tree container must be ABSENT in gated mode")
            Assert.Equal(0, treeRowCount window)
            // The rest of the surface stays live while gated (the count keeps updating).
            Assert.Equal("3 results", textOf window UiIds.FacetedTree.resultCount)
            Assert.True(isPresent window UiIds.FacetedTree.breadcrumbStrip)
            Assert.True(isPresent window UiIds.FacetedTree.offersPanel)
            clickOn window UiIds.FacetedTree.showTreeButton
            Assert.Equal<string>([ "build" ], calls)
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the filter box never commits per keystroke — Enter commits once, blur commits the current text`` () =
        HeadlessSession.run (fun () ->
            let calls, handlers = recorder ()
            let window = mount { knownState with filterDraft = "" } handlers
            let commits () = calls |> Seq.filter (fun c -> c.StartsWith("filter:")) |> List.ofSeq
            let filterBox = textBoxOf window UiIds.FacetedTree.filterBox
            filterBox.Focus() |> ignore
            Dispatcher.UIThread.RunJobs()
            // Typing changes the box text but dispatches NOTHING (no per-keystroke commit).
            window.KeyTextInput("quartz")
            Dispatcher.UIThread.RunJobs()
            Assert.Equal("quartz", filterBox.Text)
            Assert.Empty(commits ())
            // Enter commits exactly once, with the typed text.
            window.KeyPressQwerty(PhysicalKey.Enter, RawInputModifiers.None)
            Dispatcher.UIThread.RunJobs()
            window.KeyReleaseQwerty(PhysicalKey.Enter, RawInputModifiers.None)
            Dispatcher.UIThread.RunJobs()
            Assert.Equal<string>([ "filter:quartz" ], commits ())
            // More typing after the commit still dispatches nothing...
            window.KeyTextInput("!")
            Dispatcher.UIThread.RunJobs()
            Assert.Equal<string>([ "filter:quartz" ], commits ())
            // ...until focus leaves the box — blur commits the box's CURRENT text.
            (textBoxOf window (UiIds.FacetedTree.manualRangeBox "thickness")).Focus() |> ignore
            Dispatcher.UIThread.RunJobs()
            Assert.Equal<string>([ "filter:quartz"; "filter:quartz!" ], commits ())
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the manual range box commits its raw text on Enter only — never per keystroke`` () =
        HeadlessSession.run (fun () ->
            let calls, handlers = recorder ()
            let window = mount knownState handlers
            let ranges () = calls |> Seq.filter (fun c -> c.StartsWith("range:")) |> List.ofSeq
            let rangeBox = textBoxOf window (UiIds.FacetedTree.manualRangeBox "thickness")
            rangeBox.Focus() |> ignore
            Dispatcher.UIThread.RunJobs()
            window.KeyTextInput("10-20")
            Dispatcher.UIThread.RunJobs()
            Assert.Equal("10-20", rangeBox.Text)
            Assert.Empty(ranges ())
            // Enter hands the RAW text to the host with the group's code (the host parses it).
            window.KeyPressQwerty(PhysicalKey.Enter, RawInputModifiers.None)
            Dispatcher.UIThread.RunJobs()
            Assert.Equal<string>([ "range:thickness=10-20" ], ranges ())
            window.Close())
