namespace OpticalConstructor.Ui.Tests

open Avalonia
open Avalonia.Controls
open Avalonia.Headless
open Avalonia.Input
open Avalonia.Threading
open Avalonia.VisualTree
open Avalonia.FuncUI.Hosts
open Avalonia.FuncUI.Elmish
open Elmish
open Xunit
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Facets
open OpticalConstructor.Domain.LibraryFacets
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.Lifecycle
open OpticalConstructor.Domain.MaterialStore
open OpticalConstructor.Domain.SampleStore
open OpticalConstructor.Domain.Placement
open OpticalConstructor.Domain.SampleStackEditor
open OpticalConstructor.Domain.WindowMode
open OpticalConstructor.Domain.WorkbenchSettings
open OpticalConstructor.Controls
open OpticalConstructor.Ui

/// Spec 0038 Part E (step 013) — the Materials window (UICOMP_XDUO_0009): the single-instance
/// window instantiating the step-012 `FacetedTreeControls` over the step-011 material facets,
/// beside the view panel and the Add / Edit / Remove / Categories… verbs rewired from the
/// retired Materials bay. Two layers, the repo precedent: pure tests for the MVU model, the
/// engine projection and the disarm discipline; headless proofs driving the REAL window (and
/// the REAL workbench strip button) by automation ids — the slice acceptance: the strip button
/// opens ONE window (a second click ACTIVATES it), applying facet constraints narrows the
/// corpus, the verbs operate over the shared app-scoped stores, and a category rename re-labels
/// the category facet. Step 016 adds the Select-state suite: the Select/Close pair, the
/// structural (non-removable) kind constraint banner, the targeted `onSelected` dispatch into
/// the sample editor's layer, close-on-table-selection-change, and the vanished-row no-op.
module MaterialsWindowTests =

    module MW = OpticalConstructor.Ui.MaterialsWindowView
    module Scene = OpticalConstructor.Ui.TableAndElementRotationView

    // ============================ shared helpers ============================

    /// A control matches `id` by its `Name` OR its `AutomationProperties.AutomationId` (the
    /// window's rows / offers / verbs live in variable-membership lists, so they carry an
    /// AutomationId — the FacetedTreeControls discipline).
    let private matchesId (id : string) (c : Control) : bool =
        c.Name = id || Avalonia.Automation.AutomationProperties.GetAutomationId(c) = id

    let private tryFindControl (window : Window) (id : string) : Control option =
        window.GetVisualDescendants()
        |> Seq.tryPick (function :? Control as c when matchesId id c -> Some c | _ -> None)

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
                window.MouseDown(c.Value, MouseButton.Left, RawInputModifiers.None)
                Dispatcher.UIThread.RunJobs()
                if window.IsVisible then
                    window.MouseUp(c.Value, MouseButton.Left, RawInputModifiers.None)
                    Dispatcher.UIThread.RunJobs()
            else Assert.Fail($"%s{id} has no on-screen position")

    /// The display text under the control carrying `id` (the control itself when it is a
    /// TextBlock, its first TextBlock descendant otherwise).
    let private textOf (window : Window) (id : string) : string =
        match tryFindControl window id with
        | None -> ""
        | Some (:? TextBlock as t) -> (if isNull t.Text then "" else t.Text)
        | Some c ->
            c.GetVisualDescendants()
            |> Seq.tryPick (function :? TextBlock as t when not (isNull t.Text) -> Some t.Text | _ -> None)
            |> Option.defaultValue ""

    /// Set the text of the TextBox carrying `id` (the editor windows subscribe text changes;
    /// the faceted filter box does NOT — it is driven through `commitFilter` below).
    let private setText (window : Window) (id : string) (text : string) : unit =
        match tryFindControl window id with
        | Some (:? TextBox as tb) ->
            tb.Text <- text
            Dispatcher.UIThread.RunJobs()
        | Some c -> Assert.Fail($"%s{id} is a %s{c.GetType().Name}, not a TextBox")
        | None -> Assert.Fail($"%s{id} was not found")

    /// Commit `text` through the REAL faceted filter box: set the box text (no dispatch — the
    /// control has no text-change subscription) and press Enter, the control's commit gesture.
    let private commitFilter (window : Window) (text : string) : unit =
        match tryFindControl window FacetedTreeControls.UiIds.filterBox with
        | Some (:? TextBox as tb) ->
            tb.Focus() |> ignore
            Dispatcher.UIThread.RunJobs()
            tb.Text <- text
            Dispatcher.UIThread.RunJobs()
            window.KeyPressQwerty(PhysicalKey.Enter, RawInputModifiers.None)
            Dispatcher.UIThread.RunJobs()
            window.KeyReleaseQwerty(PhysicalKey.Enter, RawInputModifiers.None)
            Dispatcher.UIThread.RunJobs()
        | Some c -> Assert.Fail($"the filter box is a %s{c.GetType().Name}, not a TextBox")
        | None -> Assert.Fail("the filter box was not found")

    /// The label text INSIDE the clickable Border carrying `id` (a picker option box is a
    /// Border with a single TextBlock child).
    let private labelInside (window : Window) (id : string) : string =
        match window.GetVisualDescendants() |> Seq.tryPick (function :? Border as b when matchesId id b -> Some b | _ -> None) with
        | Some b ->
            match b.GetVisualDescendants() |> Seq.tryPick (function :? TextBlock as t -> Some t.Text | _ -> None) with
            | Some text -> text
            | None -> failwith $"%s{id} carries no text label"
        | None -> failwith $"%s{id} was not found in the visual tree"

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

    /// Fresh, isolated in-memory stores per test — the SAME composition the App performs
    /// (samples first, then materials whose remove-block consults the LIVE samples, then
    /// categories whose remove-block consults the LIVE materials).
    let private freshStores () : MaterialProxy * SampleProxy * CategoryProxy =
        let samples = SampleProxy.createInMemory VersionsInUse.empty
        let materials = MaterialProxy.createInMemory (samplesReferencing samples) VersionsInUse.empty
        let categories = CategoryProxy.createInMemory (materialsReferencingCategory materials)
        materials, samples, categories

    /// A recording stub context (the functional-proxy seam): the launchers append tags, so a
    /// verb's request is observable without opening a window.
    let private stubContext (materials : MaterialProxy) (categories : CategoryProxy) : ResizeArray<string> * MW.MaterialsWindowContext =
        let calls = ResizeArray<string>()
        let context : MW.MaterialsWindowContext =
            {
                materials = materials
                categories = categories
                treeAutoBuildThreshold = TreeAutoBuildThreshold.defaultValue
                openMaterialEditor =
                    fun intent ->
                        calls.Add(
                            match intent with
                            | MaterialEditorView.NewMaterial mintedId -> $"material-add:{mintedId.value}"
                            | MaterialEditorView.EditMaterial entry -> "material-edit:" + entry.name)
                openCategoryEditor = fun () -> calls.Add "categories-open"
                requestClose = fun () -> calls.Add "close-requested"
            }
        calls, context

    let private freshModel () : ResizeArray<string> * MW.Model =
        let materials, _, categories = freshStores ()
        let calls, context = stubContext materials categories
        calls, MW.init context Browse

    /// A recording Select-session context (spec 0038 step 016): `onSelected` tags the chosen
    /// material id, `onCancelled` tags the cancel — the window guarantees exactly one fires.
    let private selectContext (kind : CatalogueKind) (target : SelectionTarget) : ResizeArray<string> * SelectionContext<MaterialEntry> =
        let events = ResizeArray<string>()
        let context : SelectionContext<MaterialEntry> =
            {
                kindConstraint = KindConstraint kind
                target = target
                onSelected = fun entry -> events.Add ("selected:" + string entry.id.value)
                onCancelled = fun () -> events.Add "cancelled"
            }
        events, context

    /// Rename a built-in category through the proxy (built-ins ARE renamable — no origin guard
    /// on update), failing the test on a typed rejection.
    let private renameGlass (categories : CategoryProxy) (newName : string) : unit =
        match categories.updateCategory { id = CategoryIds.glass; name = newName; visibility = SelectableOnCreate; origin = BuiltInCategory } with
        | Ok () -> ()
        | Error e -> Assert.Fail($"rename failed: %A{e}")

    let private filteredIds (m : MW.Model) : MaterialId list =
        MW.filteredEntries m |> List.map (fun e -> e.id)

    // ============================ pure: ids contract ============================

    [<Fact>]
    let ``the MaterialsWindow UiIds are the stable intent-named ids`` () =
        Assert.Equal("MaterialsWindow", MW.UiIds.window)
        Assert.Equal("MaterialsFacetTreeHost", MW.UiIds.treeHost)
        Assert.Equal("MaterialsViewPanel", MW.UiIds.viewPanel)
        Assert.Equal("MaterialsViewPanelNkChart", MW.UiIds.viewPanelChart)
        Assert.Equal("MaterialsAddButton", MW.UiIds.addButton)
        Assert.Equal("MaterialsEditButton", MW.UiIds.editButton)
        Assert.Equal("MaterialsRemoveButton", MW.UiIds.removeButton)
        Assert.Equal("MaterialsCategoriesButton", MW.UiIds.categoriesButton)
        Assert.Equal("MaterialsRemoveConfirmButton", MW.UiIds.removeConfirmButton)
        Assert.Equal("MaterialsRemoveCancelButton", MW.UiIds.removeCancelButton)
        Assert.Equal("MaterialsWindowMessage", MW.UiIds.message)
        // The entry-leaf id derives from the tree-node code family, prefixed so it cannot collide.
        Assert.Equal(
            "FacetTreeNode_entry:" + string MaterialIds.glass152.value,
            MW.UiIds.entryNode MaterialIds.glass152)
        // The constructor-side entry point: the ribbon strip's right-aligned button.
        Assert.Equal("OpenMaterialsWindowButton", Scene.WorkbenchIds.openMaterialsButton)

    // ============================ pure: projection ============================

    [<Fact>]
    let ``the initial projection lists every stored material as a selectable entry leaf with the live count`` () =
        let _, m = freshModel ()
        let state = MW.facetedState m
        Assert.Equal(12, state.resultCount)
        Assert.Equal(FacetedTreeControls.TreeMaterialized, state.materialization)
        Assert.Empty(state.breadcrumbs)
        Assert.Equal("", state.filterDraft)
        // The entries group leads the tree; its leaves are the whole corpus, entry-coded.
        let entries = List.head state.tree
        Assert.Equal("entries", entries.code)
        Assert.Equal(Some 12, entries.countOpt)
        Assert.Equal(12, List.length entries.children)
        for leaf in entries.children do
            Assert.StartsWith("entry:", leaf.code)
        // The two named representations are offered, category-first active by default.
        Assert.Equal<string list>(
            [ "by-category"; "by-physics" ],
            state.representations |> List.map (fun r -> r.code))
        Assert.Equal("by-category", state.activeRepresentation)
        // Every material facet is discrete — no manual min–max box is ever offered.
        Assert.NotEmpty(state.offers)
        for group in state.offers do
            Assert.Equal(FacetedTreeControls.NoManualRange, group.manualRange)
        Assert.Contains(state.offers, fun (g : FacetedTreeControls.OfferGroup) -> g.code = materialCategoryKey.value)

    [<Fact>]
    let ``the committed text filter narrows the corpus and echoes as the box draft`` () =
        let _, m = freshModel ()
        let narrowed = MW.update (MW.CommitTextFilter "glass") m
        Assert.Equal(4, List.length (MW.filteredEntries narrowed))
        Assert.Contains(MaterialIds.glass152, filteredIds narrowed)
        Assert.DoesNotContain(MaterialIds.silicon, filteredIds narrowed)
        Assert.Equal("glass", (MW.facetedState narrowed).filterDraft)
        let restored = MW.update (MW.CommitTextFilter "") narrowed
        Assert.Equal(12, List.length (MW.filteredEntries restored))

    [<Fact>]
    let ``applying a facet constraint narrows the corpus and takes a removable breadcrumb chip with its after-count`` () =
        let _, m = freshModel ()
        let constrained = MW.update (MW.ApplyFacetValue (materialCategoryKey, DiscreteKey "Crystal")) m
        Assert.Equal(4, List.length (MW.filteredEntries constrained))
        Assert.Contains(MaterialIds.langasite, filteredIds constrained)
        let chips = (MW.facetedState constrained).breadcrumbs
        Assert.Equal(1, List.length chips)
        Assert.Equal(materialCategoryKey.value, (List.head chips).code)
        Assert.Equal("Category: Crystal", (List.head chips).label)
        Assert.Equal(4, (List.head chips).afterCount)
        // Facet constraints AND the text filter compose: no glass is a crystal.
        let composed = MW.update (MW.CommitTextFilter "glass") constrained
        Assert.Empty(MW.filteredEntries composed)
        Assert.Equal(0, (MW.facetedState composed).resultCount)
        // Removing the chip restores (the text filter stays applied).
        let removed = MW.update (MW.RemoveFacet materialCategoryKey) composed
        Assert.Equal(4, List.length (MW.filteredEntries removed))
        Assert.Empty((MW.facetedState removed).breadcrumbs)

    [<Fact>]
    let ``re-applying a constrained facet replaces its selection — one chip per facet`` () =
        let _, m = freshModel ()
        let replaced =
            m
            |> MW.update (MW.ApplyFacetValue (materialCategoryKey, DiscreteKey "Crystal"))
            |> MW.update (MW.ApplyFacetValue (materialCategoryKey, DiscreteKey "Glass"))
        let chips = (MW.facetedState replaced).breadcrumbs
        Assert.Equal(1, List.length chips)
        Assert.Equal("Category: Glass", (List.head chips).label)
        for entry in MW.filteredEntries replaced do
            Assert.Equal(CategoryIds.glass, entry.category)

    [<Fact>]
    let ``a category renamed through the proxy re-labels the category facet in the next projection`` () =
        let materials, _, categories = freshStores ()
        let _, context = stubContext materials categories
        let m = MW.init context Browse
        let valueKeysOf (state : FacetedTreeControls.State) : string list =
            state.offers
            |> List.find (fun g -> g.code = materialCategoryKey.value)
            |> fun g -> g.values |> List.map (fun v -> v.code)
        Assert.Contains("Glass", valueKeysOf (MW.facetedState m))
        // Rename through the SAME proxy the window re-queries: the very next projection
        // re-labels (the discrete value key doubles as the branch label — step 009).
        renameGlass categories "Glazing"
        let after = valueKeysOf (MW.facetedState m)
        Assert.Contains("Glazing", after)
        Assert.DoesNotContain("Glass", after)
        Assert.Equal(12, (MW.facetedState m).resultCount)

    [<Fact>]
    let ``facets vanish from the offers when constrained or inapplicable to the whole filtered population`` () =
        let _, m = freshModel ()
        let offerCodes (model : MW.Model) : string list =
            (MW.facetedState model).offers |> List.map (fun g -> g.code)
        // A constrained facet leaves the offers (its chip is the removal surface).
        let constrained = MW.update (MW.ApplyFacetValue (materialCategoryKey, DiscreteKey "Crystal")) m
        Assert.DoesNotContain(materialCategoryKey.value, offerCodes constrained)
        // A facet inapplicable to every filtered item vanishes entirely: constant materials
        // carry no dispersive-eps segments, so the dispersion-model facet is gone — while
        // transparency (constant materials with value trees) stays offered.
        let constant = MW.update (MW.ApplyFacetValue (materialDispersionKey, DiscreteKey "Constant")) m
        Assert.DoesNotContain(materialDispersionModelKey.value, offerCodes constant)
        Assert.Contains(materialTransparencyKey.value, offerCodes constant)

    // ============================ pure: verbs + confirm gate ============================

    [<Fact>]
    let ``Remove is confirm-gated: request arms with the id, cancel disarms, nothing is removed; no selection is inert`` () =
        let _, m = freshModel ()
        // No selection → the request is inert.
        Assert.Equal(MW.NoPendingRemove, (MW.update MW.RequestRemoveSelected m).removeGate)
        let armed =
            m
            |> MW.update (MW.SelectEntry MaterialIds.glass200)
            |> MW.update MW.RequestRemoveSelected
        Assert.Equal(MW.PendingRemove MaterialIds.glass200, armed.removeGate)
        let cancelled = MW.update MW.CancelRemove armed
        Assert.Equal(MW.NoPendingRemove, cancelled.removeGate)
        Assert.Equal(12, List.length (MW.filteredEntries cancelled))

    [<Fact>]
    let ``removing a REFERENCED material surfaces MaterialStillReferenced and leaves the store unchanged`` () =
        let materials, _, categories = freshStores ()
        let _, context = stubContext materials categories
        let refused =
            MW.init context Browse
            |> MW.update (MW.SelectEntry MaterialIds.glass152)
            |> MW.update MW.RequestRemoveSelected
            |> MW.update MW.ConfirmRemove
        match refused.lastError with
        | Some (MaterialStillReferenced reason) ->
            Assert.Contains("still referenced", reason)
            // The block NAMES the referencing samples (never a cascade).
            Assert.Contains("Glass plate (n=1.52, 1 mm)", reason)
        | other -> Assert.Fail($"expected MaterialStillReferenced, got %A{other}")
        match materials.listMaterials ActiveOnly with
        | Ok entries -> Assert.Equal(12, List.length entries)
        | Error e -> Assert.Fail($"listMaterials failed: %A{e}")
        Assert.Contains(MaterialIds.glass152, filteredIds refused)
        Assert.Equal(Some MaterialIds.glass152, refused.selectedId)

    [<Fact>]
    let ``removing an UNREFERENCED material drops the entry from the projection in the same pass`` () =
        let materials, _, categories = freshStores ()
        let _, context = stubContext materials categories
        let removed =
            MW.init context Browse
            |> MW.update (MW.SelectEntry MaterialIds.glass200)
            |> MW.update MW.RequestRemoveSelected
            |> MW.update MW.ConfirmRemove
        match removed.lastError with
        | None -> ()
        | Some e -> Assert.Fail($"expected no error, got %A{e}")
        Assert.DoesNotContain(MaterialIds.glass200, filteredIds removed)
        Assert.Equal<MaterialId option>(None, removed.selectedId)
        match materials.listMaterials ActiveOnly with
        | Ok entries -> Assert.Equal(11, List.length entries)
        | Error e -> Assert.Fail($"listMaterials failed: %A{e}")

    [<Fact>]
    let ``a query or selection change disarms a pending remove`` () =
        let armed () =
            let _, m = freshModel ()
            m |> MW.update (MW.SelectEntry MaterialIds.glass152) |> MW.update MW.RequestRemoveSelected
        Assert.Equal(MW.NoPendingRemove, (MW.update (MW.CommitTextFilter "si") (armed ())).removeGate)
        Assert.Equal(MW.NoPendingRemove, (MW.update (MW.ApplyFacetValue (materialDispersionKey, DiscreteKey "Dispersive")) (armed ())).removeGate)
        Assert.Equal(MW.NoPendingRemove, (MW.update (MW.RemoveFacet materialDispersionKey) (armed ())).removeGate)
        // A re-selection re-targets the verbs AND disarms.
        let retargeted = MW.update (MW.SelectEntry MaterialIds.silicon) (armed ())
        Assert.Equal(Some MaterialIds.silicon, retargeted.selectedId)
        Assert.Equal(MW.NoPendingRemove, retargeted.removeGate)

    [<Fact>]
    let ``cancel leaves the inline refusal visible and the next query edit clears it`` () =
        let _, m = freshModel ()
        let refused =
            m
            |> MW.update (MW.SelectEntry MaterialIds.glass152)
            |> MW.update MW.RequestRemoveSelected
            |> MW.update MW.ConfirmRemove
        match refused.lastError with
        | Some (MaterialStillReferenced _) -> ()
        | other -> Assert.Fail($"expected MaterialStillReferenced, got %A{other}")
        let cancelled = MW.update MW.CancelRemove refused
        Assert.Equal(MW.NoPendingRemove, cancelled.removeGate)
        match cancelled.lastError with
        | Some _ -> ()
        | None -> Assert.Fail "the refusal must stay visible after a mere cancel"
        Assert.Equal(None, (MW.update (MW.CommitTextFilter "si") cancelled).lastError)

    [<Fact>]
    let ``Add mints a fresh upfront id per dispatch and reaches the launcher`` () =
        // Spec 0038 step 008: the id-mint stays off the save path — the Add verb mints the
        // entity's Guid AT the window-open dispatch, so every Add opens its own
        // registry-keyed editor (the recorded call carries the minted id).
        let calls, m = freshModel ()
        MW.update MW.AddMaterial m |> ignore
        MW.update MW.AddMaterial m |> ignore
        match calls |> Seq.filter (fun c -> c.StartsWith "material-add:") |> List.ofSeq with
        | [ a; b ] -> Assert.NotEqual<string>(a, b)
        | other -> Assert.Fail($"expected two material Adds, got %A{other}")

    [<Fact>]
    let ``Edit reaches the launcher only for an EDITABLE selection and Categories is a pure launch`` () =
        let calls, m = freshModel ()
        // Edit on an editable selection carries the resolved entry.
        m |> MW.update (MW.SelectEntry MaterialIds.glass152) |> MW.update MW.EditSelected |> ignore
        Assert.Equal<string list>([ "material-edit:Transparent glass (n = 1.52)" ], List.ofSeq calls)
        calls.Clear()
        // A view-only engine preset (complexity = None) offers no Edit — the dispatch is inert.
        m |> MW.update (MW.SelectEntry MaterialIds.silicon) |> MW.update MW.EditSelected |> ignore
        // No selection is inert too.
        MW.update MW.EditSelected m |> ignore
        Assert.Empty(calls)
        // Categories… is a pure launch: the model is unchanged and the launcher records it.
        let after = MW.update MW.OpenCategories m
        Assert.Equal<MW.Model>(m, after)
        Assert.Equal<string list>([ "categories-open" ], List.ofSeq calls)

    [<Fact>]
    let ``the view panel target follows the selection and the Edit affordance follows the step-013 complexity`` () =
        let _, m = freshModel ()
        Assert.Equal<MaterialEntry option>(None, MW.selectedEntry m)
        let glass = MW.update (MW.SelectEntry MaterialIds.glass152) m
        match MW.selectedEntry glass with
        | Some entry -> Assert.Equal("Transparent glass (n = 1.52)", entry.name)
        | None -> Assert.Fail "the selected glass entry must resolve"
        match MW.editableSelection glass with
        | Some _ -> ()
        | None -> Assert.Fail "glass152 carries the edit model and must offer Edit"
        // Silicon is a coded engine preset: selectable (the panel shows it) but NOT editable.
        let silicon = MW.update (MW.SelectEntry MaterialIds.silicon) m
        match MW.selectedEntry silicon, MW.editableSelection silicon with
        | Some _, None -> ()
        | other -> Assert.Fail($"expected a selectable but non-editable preset, got %A{other}")

    [<Fact>]
    let ``choosing a representation reshapes the tree facet order but never the constraints or the corpus`` () =
        let _, m = freshModel ()
        let constrained = MW.update (MW.ApplyFacetValue (materialCategoryKey, DiscreteKey "Crystal")) m
        let firstFacetCode (model : MW.Model) : string =
            ((MW.facetedState model).tree |> List.item 1).code
        Assert.Equal("facet:" + materialCategoryKey.value, firstFacetCode constrained)
        let reshaped = MW.update (MW.ChooseRepresentation "by-physics") constrained
        Assert.Equal("by-physics", (MW.facetedState reshaped).activeRepresentation)
        Assert.Equal("facet:" + materialAnisotropyKey.value, firstFacetCode reshaped)
        // Search order ≠ representation order: the applied chip and the result set are untouched.
        Assert.Equal(1, List.length (MW.facetedState reshaped).breadcrumbs)
        Assert.Equal(4, (MW.facetedState reshaped).resultCount)
        // An unknown code is inert.
        Assert.Equal<MW.Model>(reshaped, MW.update (MW.ChooseRepresentation "no-such") reshaped)

    [<Fact>]
    let ``a result count above the threshold gates the tree and Show-Search materializes it`` () =
        let materials, _, categories = freshStores ()
        let _, context = stubContext materials categories
        let m = MW.init { context with treeAutoBuildThreshold = TreeAutoBuildThreshold 1 } Browse
        let gated = MW.facetedState m
        Assert.Equal(FacetedTreeControls.TreeGated, gated.materialization)
        // A gated pass projects NO tree at all — the whole point is skipping the heavy render —
        // while the live count and the offers stay up.
        Assert.Empty(gated.tree)
        Assert.Equal(12, gated.resultCount)
        Assert.NotEmpty(gated.offers)
        let shown = MW.facetedState (MW.update MW.RequestTreeBuild m)
        Assert.Equal(FacetedTreeControls.TreeMaterialized, shown.materialization)
        Assert.NotEmpty(shown.tree)

    [<Fact>]
    let ``only an entry node code selects — branches and headings are grouping display`` () =
        Assert.Equal(Some MaterialIds.glass152, MW.entryIdOfNodeCode (MW.entryNodeCode MaterialIds.glass152))
        Assert.Equal<MaterialId option>(None, MW.entryIdOfNodeCode "branch:material-category:Crystal")
        Assert.Equal<MaterialId option>(None, MW.entryIdOfNodeCode "entries")
        Assert.Equal<MaterialId option>(None, MW.entryIdOfNodeCode "entry:not-a-guid")
        let dispatched = ResizeArray<MW.Msg>()
        let handlers = MW.facetedHandlers dispatched.Add
        handlers.selectNode (MW.entryNodeCode MaterialIds.glass152)
        handlers.selectNode "branch:material-category:Crystal"
        handlers.selectNode "entries"
        Assert.Equal<MW.Msg list>([ MW.SelectEntry MaterialIds.glass152 ], List.ofSeq dispatched)
        // The offer click lifts its tokens back to elevated engine values at the boundary.
        handlers.applyConstraint materialCategoryKey.value "Crystal"
        Assert.Equal(MW.ApplyFacetValue (materialCategoryKey, DiscreteKey "Crystal"), dispatched.[1])

    // ============================ headless acceptance (ui-smoke) ============================

    /// Mount the REAL Main workbench MVU loop headless (the MainConstructorWindow shape with
    /// injectable stores), for the strip-button acceptance.
    let private mountMain (materials : MaterialProxy) (samples : SampleProxy) (categories : CategoryProxy) : HostWindow =
        let model0 = Scene.initMainWith (Library.createInMemory ()) (Experiments.createInMemory ()) materials samples categories
        let window = HostWindow(Width = 980.0, Height = 1050.0)
        Program.mkSimple (fun () -> model0) Scene.update Scene.mainView
        |> Program.withHost window
        |> Program.run
        window.Show()
        Dispatcher.UIThread.RunJobs()
        window

    let private mountMaterialsWindow (materials : MaterialProxy) (categories : CategoryProxy) : MaterialsWindow =
        let window = MaterialsWindow(materials, categories)
        window.Show()
        Dispatcher.UIThread.RunJobs()
        window

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``acceptance: the strip button opens ONE Materials window and a second click ACTIVATES it`` () =
        HeadlessSession.run (fun () ->
            let materials, samples, categories = freshStores ()
            let window = mountMain materials samples categories
            Assert.True(isPresent window Scene.WorkbenchIds.openMaterialsButton,
                        "the ribbon strip row must carry the right-aligned Materials… button")
            // Observe the windows the REAL defaults open (the WindowOpenedEvent seam the desktop
            // lifetime itself uses) — subscribed only after the Main window is shown.
            let opened = ResizeArray<Window>()
            use _sub =
                Window.WindowOpenedEvent.Raised
                |> Observable.subscribe (fun (struct (sender, _args)) ->
                    match sender with
                    | :? Window as w -> opened.Add w
                    | _ -> ())
            clickOn window Scene.WorkbenchIds.openMaterialsButton
            Dispatcher.UIThread.RunJobs()
            Assert.Equal(1, opened.Count)
            let materialsWindow = opened.[0]
            Assert.True(matchesId MW.UiIds.window materialsWindow, "the opened window must be the Materials window")
            Assert.True(materialsWindow.IsVisible)
            // The single-instance acceptance: a second click ACTIVATES the live window — the
            // shared registry under MaterialsWindowKey creates nothing new.
            clickOn window Scene.WorkbenchIds.openMaterialsButton
            Dispatcher.UIThread.RunJobs()
            Assert.Equal(1, opened.Count)
            Assert.True(materialsWindow.IsVisible, "the activated window must still be the live one")
            // Close → the registry forgets the key, so the NEXT open creates afresh.
            materialsWindow.Close()
            Dispatcher.UIThread.RunJobs()
            clickOn window Scene.WorkbenchIds.openMaterialsButton
            Dispatcher.UIThread.RunJobs()
            Assert.Equal(2, opened.Count)
            opened.[1].Close()
            Dispatcher.UIThread.RunJobs()
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``acceptance: applying a facet constraint narrows the corpus in the same render pass and the chip restores it`` () =
        HeadlessSession.run (fun () ->
            let materials, _, categories = freshStores ()
            let window = mountMaterialsWindow materials categories
            Assert.True(isPresent window (MW.UiIds.entryNode MaterialIds.glass152))
            Assert.True(isPresent window (MW.UiIds.entryNode MaterialIds.silicon))
            Assert.Equal("12 results", textOf window FacetedTreeControls.UiIds.resultCount)
            // Click the Dispersion facet's "Dispersive" offer: the corpus narrows to the two
            // wavelength-dependent presets IN THE SAME RENDER PASS.
            clickOn window (FacetedTreeControls.UiIds.offeredValue materialDispersionKey.value "Dispersive")
            Assert.True(isPresent window (MW.UiIds.entryNode MaterialIds.silicon),
                        "the matching entry must stay listed")
            Assert.False(isPresent window (MW.UiIds.entryNode MaterialIds.glass152),
                         "the non-matching entry must leave the tree in the same render pass")
            Assert.Equal("2 results", textOf window FacetedTreeControls.UiIds.resultCount)
            // The removable chip carries the after-count; clicking it restores the corpus.
            Assert.True(isPresent window (FacetedTreeControls.UiIds.breadcrumbChip materialDispersionKey.value))
            clickOn window (FacetedTreeControls.UiIds.breadcrumbChip materialDispersionKey.value)
            Assert.True(isPresent window (MW.UiIds.entryNode MaterialIds.glass152))
            Assert.Equal("12 results", textOf window FacetedTreeControls.UiIds.resultCount)
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``acceptance: the verbs operate over the shared app-scoped stores — Add persists through the real editor, a referenced remove is refused inline`` () =
        HeadlessSession.run (fun () ->
            // The SAME store composition the app scope performs; the window and the editors it
            // opens share these stores.
            let materials, _, categories = freshStores ()
            let window = mountMaterialsWindow materials categories
            let opened = ResizeArray<Window>()
            use _sub =
                Window.WindowOpenedEvent.Raised
                |> Observable.subscribe (fun (struct (sender, _args)) ->
                    match sender with
                    | :? Window as w -> opened.Add w
                    | _ -> ())
            // Add → the REAL Material editor over the SHARED stores (through the real launcher
            // under the Add-minted MaterialEditorKey).
            clickOn window MW.UiIds.addButton
            Dispatcher.UIThread.RunJobs()
            Assert.Equal(1, opened.Count)
            let editor = opened.[0]
            Assert.True(matchesId MaterialEditorView.UiIds.window editor, "the opened window must be the Material editor")
            setText editor MaterialEditorView.UiIds.nameBox "Faceted window material"
            clickOn editor MaterialEditorView.UiIds.saveButton
            Dispatcher.UIThread.RunJobs()
            Assert.False(editor.IsVisible, "Save must close the editor")
            // The save landed in the SHARED store…
            match materials.listMaterials ActiveOnly with
            | Ok entries -> Assert.Contains(entries, fun (e : MaterialEntry) -> e.name = "Faceted window material")
            | Error e -> Assert.Fail($"listMaterials failed: %A{e}")
            // …and the window's next dispatch-driven render re-queries it: committing the
            // matching filter narrows the tree to the just-saved entry.
            commitFilter window "Faceted window"
            Assert.Equal("1 results", textOf window FacetedTreeControls.UiIds.resultCount)
            // The referenced remove: narrow to the seeded glass, select its leaf, Remove →
            // Confirm → the typed refusal NAMES the referencing sample and the store keeps it.
            commitFilter window "1.52"
            clickOn window (MW.UiIds.entryNode MaterialIds.glass152)
            clickOn window MW.UiIds.removeButton
            clickOn window MW.UiIds.removeConfirmButton
            let message = textOf window MW.UiIds.message
            Assert.Contains("still referenced", message)
            Assert.Contains("Glass plate", message)
            Assert.True(isPresent window (MW.UiIds.entryNode MaterialIds.glass152),
                        "the refused remove must leave the entry listed")
            match materials.listMaterials ActiveOnly with
            | Ok entries -> Assert.Equal(13, List.length entries)
            | Error e -> Assert.Fail($"listMaterials failed: %A{e}")
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``acceptance: a category renamed through the shared proxy re-labels the category facet and the create picker`` () =
        HeadlessSession.run (fun () ->
            let materials, _, categories = freshStores ()
            let window = mountMaterialsWindow materials categories
            // Before: the category facet offers "Glass" (the value key IS the label).
            Assert.True(isPresent window (FacetedTreeControls.UiIds.offeredValue materialCategoryKey.value "Glass"))
            // Rename through the shared proxy, then dispatch a MODEL-CHANGING re-render (a text
            // commit — an unchanged model is structurally equal and the Elmish host skips it).
            renameGlass categories "Glazing"
            commitFilter window "glass"
            Assert.True(isPresent window (FacetedTreeControls.UiIds.offeredValue materialCategoryKey.value "Glazing"),
                        "the category facet must re-label to the renamed catalogue name")
            Assert.False(isPresent window (FacetedTreeControls.UiIds.offeredValue materialCategoryKey.value "Glass"),
                         "the stale label must be gone")
            Assert.StartsWith("Glazing (", textOf window (FacetedTreeControls.UiIds.offeredValue materialCategoryKey.value "Glazing"))
            // The create picker re-labels too: the editor over the SAME proxy shows "Glazing"
            // for the same stable Guid id.
            let editor = MaterialEditorWindow(materials, MaterialEditorView.NewMaterial (newMaterialId ()), categories = categories)
            editor.Show()
            Dispatcher.UIThread.RunJobs()
            Assert.Equal("Glazing", labelInside editor (MaterialEditorView.UiIds.categoryOption (string CategoryIds.glass.value)))
            editor.Close()
            Dispatcher.UIThread.RunJobs()
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``headless: Edit opens the Material editor on the selected entry and Categories opens the Category editor`` () =
        HeadlessSession.run (fun () ->
            let materials, _, categories = freshStores ()
            let window = mountMaterialsWindow materials categories
            commitFilter window "1.52"
            clickOn window (MW.UiIds.entryNode MaterialIds.glass152)
            let opened = ResizeArray<Window>()
            use _sub =
                Window.WindowOpenedEvent.Raised
                |> Observable.subscribe (fun (struct (sender, _args)) ->
                    match sender with
                    | :? Window as w -> opened.Add w
                    | _ -> ())
            clickOn window MW.UiIds.editButton
            Dispatcher.UIThread.RunJobs()
            Assert.Equal(1, opened.Count)
            Assert.True(matchesId MaterialEditorView.UiIds.window opened.[0], "the opened window must be the Material editor")
            Assert.Contains("Transparent glass", opened.[0].Title)
            opened.[0].Close()
            Dispatcher.UIThread.RunJobs()
            clickOn window MW.UiIds.categoriesButton
            Dispatcher.UIThread.RunJobs()
            Assert.Equal(2, opened.Count)
            Assert.True(matchesId CategoryEditorView.UiIds.window opened.[1], "the opened window must be the Category editor")
            opened.[1].Close()
            Dispatcher.UIThread.RunJobs()
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``headless: selecting an entry leaf shows the view panel with metadata and the embedded n-k chart`` () =
        HeadlessSession.run (fun () ->
            let materials, _, categories = freshStores ()
            let window = mountMaterialsWindow materials categories
            Assert.False(isPresent window MW.UiIds.viewPanel, "no selection → no view panel")
            commitFilter window "1.52"
            clickOn window (MW.UiIds.entryNode MaterialIds.glass152)
            Assert.True(isPresent window MW.UiIds.viewPanel, "the view panel must render for the selected entry")
            Assert.True(isPresent window MW.UiIds.viewPanelChart, "the view panel must embed the shared n/k chart host")
            Assert.Contains("Transparent glass", textOf window MW.UiIds.viewPanel)
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``headless: a gated tree shows Show-Search with ZERO rows and the click materializes the entry leaves`` () =
        HeadlessSession.run (fun () ->
            let materials, _, categories = freshStores ()
            let _, context = stubContext materials categories
            // Mount the REAL MVU loop over a tiny threshold (the window minus its launcher
            // composition — the FacetedTreeControlsTests mounting precedent).
            let window = HostWindow(Width = 900.0, Height = 760.0)
            Program.mkSimple
                (fun () -> MW.init { context with treeAutoBuildThreshold = TreeAutoBuildThreshold 1 } Browse)
                MW.update
                MW.view
            |> Program.withHost window
            |> Program.run
            window.Show()
            Dispatcher.UIThread.RunJobs()
            Assert.True(isPresent window FacetedTreeControls.UiIds.showTreeButton, "the Show/Search button must gate the tree")
            Assert.Equal(0, treeRowCount window)
            Assert.Equal("12 results", textOf window FacetedTreeControls.UiIds.resultCount)
            clickOn window FacetedTreeControls.UiIds.showTreeButton
            Assert.True(treeRowCount window > 0, "the explicit build must materialize the tree")
            Assert.True(isPresent window (MW.UiIds.entryNode MaterialIds.glass152))
            window.Close())

    // ============================ step 016 — Select mode (pure) ============================

    [<Fact>]
    let ``the step-016 UiIds are the stable intent-named ids`` () =
        Assert.Equal("MaterialsSelectButton", MW.UiIds.selectButton)
        Assert.Equal("MaterialsSelectCloseButton", MW.UiIds.selectCloseButton)
        Assert.Equal("MaterialsSelectConstraint", MW.UiIds.selectConstraint)

    /// A fresh SELECT-state model over recording stubs (targeting one sample-layer slot —
    /// the step-019 picking shape).
    let private freshSelectModel () : ResizeArray<string> * ResizeArray<string> * MW.Model =
        let materials, _, categories = freshStores ()
        let calls, context = stubContext materials categories
        let events, selectCtx = selectContext Sample (SampleLayerTarget (AtSingleLayer 0))
        events, calls, MW.init context (Select selectCtx)

    [<Fact>]
    let ``Select state keeps the WHOLE material corpus — the kind constraint is structural, no breadcrumb chip`` () =
        // Every material serves a sample-layer pick (materials carry no CatalogueKind), so the
        // pre-applied constraint narrows nothing here — it shows as the fixed banner only —
        // and it takes NO chip (nothing to remove).
        let _, _, m = freshSelectModel ()
        Assert.Equal(12, List.length (MW.filteredEntries m))
        let state = MW.facetedState m
        Assert.Equal(12, state.resultCount)
        Assert.Empty(state.breadcrumbs)

    [<Fact>]
    let ``ConfirmSelect returns the HIGHLIGHTED material through onSelected and closes — no highlight is inert`` () =
        let events, calls, m = freshSelectModel ()
        Assert.Equal<MW.Model>(m, MW.update MW.ConfirmSelect m)
        Assert.Empty(events)
        let resolved =
            m
            |> MW.update (MW.SelectEntry MaterialIds.glass152)
            |> MW.update MW.ConfirmSelect
        Assert.Equal<string list>([ $"selected:{MaterialIds.glass152.value}" ], List.ofSeq events)
        Assert.Contains("close-requested", calls)
        Assert.Equal<LibraryWindowMode<MaterialEntry>>(Browse, resolved.mode)
        // A dismissal after the resolve is a no-op — onSelected and onCancelled never both fire.
        MW.update MW.SelectDismissed resolved |> ignore
        Assert.Equal<string list>([ $"selected:{MaterialIds.glass152.value}" ], List.ofSeq events)
        // Browse-mode Confirm/Cancel are inert (the pair does not exist there).
        let _, browse = freshModel ()
        Assert.Equal<MW.Model>(browse, MW.update MW.ConfirmSelect browse)
        Assert.Equal<MW.Model>(browse, MW.update MW.CancelSelect browse)

    [<Fact>]
    let ``CancelSelect cancels once and a re-target supersedes the first session`` () =
        let events, calls, m = freshSelectModel ()
        let cancelled = MW.update MW.CancelSelect m
        Assert.Equal<string list>([ "cancelled" ], List.ofSeq events)
        Assert.Contains("close-requested", calls)
        MW.update MW.SelectDismissed cancelled |> ignore
        Assert.Equal<string list>([ "cancelled" ], List.ofSeq events)
        // Re-target: the superseded session cancels, the highlight clears, the new session
        // resolves through the NEW context (a second Choose closes the first, logically).
        let events1, _, m1 = freshSelectModel ()
        let highlighted = MW.update (MW.SelectEntry MaterialIds.glass152) m1
        let events2, retargetCtx = selectContext Sample (SampleLayerTarget (AtSingleLayer 1))
        let retargeted = MW.update (MW.RetargetSelect retargetCtx) highlighted
        Assert.Equal<string list>([ "cancelled" ], List.ofSeq events1)
        Assert.Empty(events2)
        Assert.Equal<MaterialId option>(None, retargeted.selectedId)
        retargeted
        |> MW.update (MW.SelectEntry MaterialIds.glass200)
        |> MW.update MW.ConfirmSelect
        |> ignore
        Assert.Equal<string list>([ $"selected:{MaterialIds.glass200.value}" ], List.ofSeq events2)

    // ============================ step 016 — Select mode (headless) ============================

    let private mountSelectMaterialsWindow (materials : MaterialProxy) (categories : CategoryProxy) (selectCtx : SelectionContext<MaterialEntry>) : MaterialsWindow =
        let window = MaterialsWindow(materials, categories, mode = Select selectCtx)
        window.Show()
        Dispatcher.UIThread.RunJobs()
        window

    /// Mount the REAL Sample-editor MVU loop headless WITH a captured dispatch, so a Materials
    /// Select window's onSelected can dispatch the TARGETED `BindMaterialToLayer` into the live
    /// editor loop (the step-019 picking shape, driven end-to-end here).
    let private mountEditorWithDispatch (materials : MaterialProxy) (samples : SampleProxy) (intent : SampleEditorView.SampleEditorIntent) : HostWindow * (SampleEditorView.Msg -> unit) =
        let materialList =
            match materials.listMaterials ActiveOnly with
            | Ok entries -> entries
            | Error e -> failwith $"listMaterials failed: %A{e}"
        let context : SampleEditorView.SampleEditorContext =
            {
                materials = materials
                samples = samples
                openMaterialsSelect = fun _ _ -> ()
                requestClose = fun () -> ()
            }
        let window = HostWindow(Width = 1100.0, Height = 760.0)
        let mutable dispatchRef : SampleEditorView.Msg -> unit = ignore
        Program.mkProgram
            (fun () -> SampleEditorView.init context materialList intent, Cmd.ofEffect (fun d -> dispatchRef <- d))
            (fun msg m -> SampleEditorView.update msg m, Cmd.none)
            SampleEditorView.view
        |> Program.withHost window
        |> Program.run
        window.Show()
        Dispatcher.UIThread.RunJobs()
        window, (fun msg -> dispatchRef msg)

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``acceptance (016): the Materials Select state shows the Select-Close pair and the non-removable constraint — the ordinary window otherwise`` () =
        HeadlessSession.run (fun () ->
            let materials, _, categories = freshStores ()
            let _, selectCtx = selectContext Sample (SampleLayerTarget (AtSingleLayer 0))
            let window = mountSelectMaterialsWindow materials categories selectCtx
            Assert.True(isPresent window MW.UiIds.selectButton, "the Select button must render")
            Assert.True(isPresent window MW.UiIds.selectCloseButton, "the Close button must render")
            Assert.Equal("Select", textOf window MW.UiIds.selectButton)
            Assert.Equal("Close", textOf window MW.UiIds.selectCloseButton)
            let banner = textOf window MW.UiIds.selectConstraint
            Assert.Contains("Sample", banner)
            Assert.Contains("fixed", banner)
            // No chip — the constraint is structural, and the whole material corpus stays.
            Assert.Equal("12 results", textOf window FacetedTreeControls.UiIds.resultCount)
            let chipCount =
                window.GetVisualDescendants()
                |> Seq.filter (fun v ->
                    match v with
                    | :? Control as c ->
                        let autoId = Avalonia.Automation.AutomationProperties.GetAutomationId(c)
                        not (isNull autoId) && autoId.StartsWith("FacetBreadcrumbChip_")
                    | _ -> false)
                |> Seq.length
            Assert.Equal(0, chipCount)
            // Everything else IS the ordinary window: Add / Categories… (add-on-the-fly) stay.
            Assert.True(isPresent window MW.UiIds.addButton, "Add must survive Select state")
            Assert.True(isPresent window MW.UiIds.categoriesButton, "Categories… must survive Select state")
            window.Close()
            Dispatcher.UIThread.RunJobs())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``acceptance (016): Select returns the material through the TARGETED dispatch into the sample editor's layer and closes`` () =
        HeadlessSession.run (fun () ->
            let materials, samples, categories = freshStores ()
            // The editor over a seeded one-layer sample — the Select session targets ITS row 0.
            let editor, dispatch = mountEditorWithDispatch materials samples (SampleEditorView.EditSample SeedSamples.glassFilm600)
            let cancels = ResizeArray<string>()
            let selectCtx : SelectionContext<MaterialEntry> =
                {
                    kindConstraint = KindConstraint Sample
                    target = SampleLayerTarget (AtSingleLayer 0)
                    onSelected = fun entry -> dispatch (SampleEditorView.BindMaterialToLayer (AtSingleLayer 0, entry.id))
                    onCancelled = fun () -> cancels.Add "cancelled"
                }
            let selectWindow = mountSelectMaterialsWindow materials categories selectCtx
            commitFilter selectWindow "1.52"
            clickOn selectWindow (MW.UiIds.entryNode MaterialIds.glass152)
            clickOn selectWindow MW.UiIds.selectButton
            Dispatcher.UIThread.RunJobs()
            Assert.False(selectWindow.IsVisible, "Select must close the window after onSelected")
            Assert.Empty(cancels)
            // The TARGETED return re-materialed row 0 in the same pass.
            Assert.Contains("Transparent glass (n = 1.52)", textOf editor (SampleEditorView.UiIds.layerRow 0))
            Assert.Equal("", textOf editor SampleEditorView.UiIds.statusText)
            editor.Close()
            Dispatcher.UIThread.RunJobs())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``acceptance (016): a changed table selection cancels and CLOSES the open Select-state Materials window too`` () =
        HeadlessSession.run (fun () ->
            let materials, _, categories = freshStores ()
            let events, selectCtx = selectContext Sample (SampleLayerTarget (AtSingleLayer 0))
            let selectWindow = mountSelectMaterialsWindow materials categories selectCtx
            // The workbench staleness machinery is window-agnostic: the session handle closes
            // whichever Select-state window it points at.
            let sceneSamples = SampleProxy.createInMemory VersionsInUse.empty
            let sceneMaterials = MaterialProxy.createInMemory (samplesReferencing sceneSamples) VersionsInUse.empty
            let sceneCategories = CategoryProxy.createInMemory (materialsReferencingCategory sceneMaterials)
            let model0 =
                Scene.initMainWith (Library.createInMemory ()) (Experiments.createInMemory ()) sceneMaterials sceneSamples sceneCategories
            let session : Scene.SelectSession =
                {
                    target = elementId "det"
                    cancelAndClose = fun () -> selectWindow.Close()
                }
            let mainWindow = HostWindow(Width = 980.0, Height = 1050.0)
            Program.mkSimple
                (fun () -> { model0 with selection = Scene.ElementSelected 1; activeSelect = Some session })
                Scene.update
                Scene.mainView
            |> Program.withHost mainWindow
            |> Program.run
            mainWindow.Show()
            Dispatcher.UIThread.RunJobs()
            Assert.True(selectWindow.IsVisible)
            // A REAL canvas click on the empty table changes the selection: the staleness rule
            // closes the Materials Select window and its session cancels — exactly once.
            match tryFindControl mainWindow Scene.UiIds.canvas with
            | Some canvas ->
                let p = canvas.TranslatePoint(Point(Scene.center.sx, Scene.center.sy), mainWindow)
                Assert.True(p.HasValue, "the canvas must have an on-screen position")
                mainWindow.MouseDown(p.Value, MouseButton.Left, RawInputModifiers.None)
                Dispatcher.UIThread.RunJobs()
                mainWindow.MouseUp(p.Value, MouseButton.Left, RawInputModifiers.None)
                Dispatcher.UIThread.RunJobs()
            | None -> Assert.Fail "the table canvas was not found"
            Assert.False(selectWindow.IsVisible, "the changed table selection must close the Materials Select window")
            Assert.Equal<string list>([ "cancelled" ], List.ofSeq events)
            mainWindow.Close()
            Dispatcher.UIThread.RunJobs())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``acceptance (016): a vanished target layer makes the Select return a NO-OP plus the editor status line — never a throw`` () =
        HeadlessSession.run (fun () ->
            let materials, samples, categories = freshStores ()
            // A BLANK new sample: it has NO row 0, so the session's target does not exist —
            // the deleted-row race the targeted return must survive (step 019 wires the verb).
            let editor, dispatch = mountEditorWithDispatch materials samples (SampleEditorView.NewBlankSample (newSampleId ()))
            let selectCtx : SelectionContext<MaterialEntry> =
                {
                    kindConstraint = KindConstraint Sample
                    target = SampleLayerTarget (AtSingleLayer 0)
                    onSelected = fun entry -> dispatch (SampleEditorView.BindMaterialToLayer (AtSingleLayer 0, entry.id))
                    onCancelled = fun () -> ()
                }
            let selectWindow = mountSelectMaterialsWindow materials categories selectCtx
            commitFilter selectWindow "1.52"
            clickOn selectWindow (MW.UiIds.entryNode MaterialIds.glass152)
            clickOn selectWindow MW.UiIds.selectButton
            Dispatcher.UIThread.RunJobs()
            Assert.False(selectWindow.IsVisible, "the Select window still closes after its return")
            // The editor no-opped and reports the vanished row on its status line.
            Assert.Contains("no longer in the stack", textOf editor SampleEditorView.UiIds.statusText)
            Assert.False(isPresent editor (SampleEditorView.UiIds.layerRow 0), "no layer row may appear from a vanished-target return")
            editor.Close()
            Dispatcher.UIThread.RunJobs())
