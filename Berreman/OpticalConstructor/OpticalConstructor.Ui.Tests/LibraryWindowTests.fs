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
open OpticalConstructor.Domain.WorkbenchSettings
open OpticalConstructor.Controls
open OpticalConstructor.Ui

/// Spec 0038 Part F (step 015) — the Library window (UICOMP_XDUO_0010): the single-instance
/// window instantiating the step-012 `FacetedTreeControls` over the step-011 library facet
/// catalogue (seeded default representation "By kind"), beside the view panel and the
/// Add sample / Make multilayer / Edit / Remove verbs rewired from the retired Library
/// (samples workbench) bay. Two layers, the repo precedent: pure tests for the MVU model, the
/// engine projection (incl. the FIRST numeric-facet host — film-thickness buckets + the manual
/// min–max range) and the protection/confirm discipline; headless proofs driving the REAL
/// window (and the REAL workbench strip button) by automation ids — the slice acceptance: the
/// strip button opens ONE window (a second click ACTIVATES it), the by-kind tree lists every
/// entry kind with counts, the sample verbs operate over the shared app-scoped stores, and
/// removing a `ProtectedBuiltIn` entry is refused with a typed reason and changes nothing.
module LibraryWindowTests =

    module LW = OpticalConstructor.Ui.LibraryWindowView
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

    /// Fresh, isolated in-memory stores per test — the SAME composition the App performs (the
    /// samples store first, then materials whose remove-block consults the LIVE samples).
    let private freshStores () : LibraryProxy * SampleProxy * MaterialProxy =
        let samples = SampleProxy.createInMemory ()
        let materials = MaterialProxy.createInMemory (samplesReferencing samples)
        Library.createInMemory (), samples, materials

    /// A recording stub context (the functional-proxy seam): the launcher appends tags, so a
    /// verb's request is observable without opening a window.
    let private stubContext (library : LibraryProxy) (samples : SampleProxy) (materials : MaterialProxy) : ResizeArray<string> * LW.LibraryWindowContext =
        let calls = ResizeArray<string>()
        let context : LW.LibraryWindowContext =
            {
                library = library
                samples = samples
                materials = materials
                treeAutoBuildThreshold = TreeAutoBuildThreshold.defaultValue
                thicknessBucketCap = ThicknessBucketCap.defaultValue
                openSampleEditor =
                    fun intent ->
                        calls.Add(
                            match intent with
                            | SampleEditorView.NewBlankSample mintedId -> $"sample-add:{mintedId.value}"
                            | SampleEditorView.NewSeededMultilayer mintedId -> $"sample-multilayer:{mintedId.value}"
                            | SampleEditorView.EditSample s -> "sample-edit:" + s.name)
            }
        calls, context

    let private freshModel () : ResizeArray<string> * LW.Model =
        let library, samples, materials = freshStores ()
        let calls, context = stubContext library samples materials
        calls, LW.init context

    /// A fresh model PLUS its live stores (for tests that mutate behind the window's back).
    let private freshModelWithStores () : SampleProxy * LW.Model =
        let library, samples, materials = freshStores ()
        let _, context = stubContext library samples materials
        samples, LW.init context

    let private filteredEntryIds (m : LW.Model) : string list =
        LW.filteredEntries m |> List.map (fun e -> e.entryId)

    let private glassFilm600EntryId : string = string SeedSamples.glassFilm600.id.value

    /// The kind-facet constraint helper (a single discrete key).
    let private kindConstraint (kind : string) : LW.Msg =
        LW.ApplyFacetConstraint (entryKindFacetKey, DiscreteSelection (Set.singleton (DiscreteKey kind)))

    // ============================ pure: ids contract ============================

    [<Fact>]
    let ``the LibraryWindow UiIds are the stable intent-named ids`` () =
        Assert.Equal("LibraryWindow", LW.UiIds.window)
        Assert.Equal("LibraryFacetTreeHost", LW.UiIds.treeHost)
        Assert.Equal("LibraryViewPanel", LW.UiIds.viewPanel)
        Assert.Equal("LibraryAddSampleButton", LW.UiIds.addSampleButton)
        Assert.Equal("LibraryMakeMultilayerButton", LW.UiIds.makeMultilayerButton)
        Assert.Equal("LibraryEditButton", LW.UiIds.editButton)
        Assert.Equal("LibraryRemoveButton", LW.UiIds.removeButton)
        Assert.Equal("LibraryRemoveConfirmButton", LW.UiIds.removeConfirmButton)
        Assert.Equal("LibraryRemoveCancelButton", LW.UiIds.removeCancelButton)
        Assert.Equal("LibraryWindowMessage", LW.UiIds.message)
        // The entry-leaf id derives from the tree-node code family, prefixed so it cannot collide.
        Assert.Equal("FacetTreeNode_entry:src-600", LW.UiIds.entryNode "src-600")
        // The constructor-side entry point: the ribbon strip's right-aligned button.
        Assert.Equal("OpenLibraryWindowButton", Scene.WorkbenchIds.openLibraryButton)

    // ============================ pure: projection ============================

    [<Fact>]
    let ``the initial projection lists the WHOLE corpus — every sample and every preset — as selectable entry leaves`` () =
        let _, m = freshModel ()
        let state = LW.facetedState m
        // 11 seeded samples + 2 detectors + 3 polarizers + 1 source.
        Assert.Equal(17, state.resultCount)
        Assert.Equal(FacetedTreeControls.TreeMaterialized, state.materialization)
        Assert.Empty(state.breadcrumbs)
        Assert.Equal("", state.filterDraft)
        let entries = List.head state.tree
        Assert.Equal("entries", entries.code)
        Assert.Equal(Some 17, entries.countOpt)
        Assert.Equal(17, List.length entries.children)
        for leaf in entries.children do
            Assert.StartsWith("entry:", leaf.code)
        // Samples AND presets both lead as leaves.
        Assert.Contains(entries.children, fun (n : FacetedTreeControls.TreeNode) -> n.code = LW.entryNodeCode glassFilm600EntryId)
        Assert.Contains(entries.children, fun (n : FacetedTreeControls.TreeNode) -> n.code = LW.entryNodeCode "src-600")
        // The ONE seeded representation is "By kind", active by default.
        Assert.Equal<string list>([ "by-kind" ], state.representations |> List.map (fun r -> r.code))
        Assert.Equal("by-kind", state.activeRepresentation)

    [<Fact>]
    let ``acceptance (pure): the by-kind tree lists every entry kind with counts`` () =
        let _, m = freshModel ()
        let state = LW.facetedState m
        let kindNode =
            state.tree |> List.find (fun n -> n.code = "facet:" + entryKindFacetKey.value)
        let branches =
            kindNode.children |> List.map (fun b -> b.label, b.countOpt)
        // Every kind, with its count, in the engine's structural branch order.
        Assert.Equal<(string * int option) list>(
            [ "Detector", Some 2; "Polarizer", Some 3; "Sample", Some 11; "Source", Some 1 ],
            branches)
        // The kind facet is offered FIRST (the by-kind representation's head) with count-previews.
        let kindOffer = state.offers |> List.head
        Assert.Equal(entryKindFacetKey.value, kindOffer.code)
        Assert.Equal<(string * int) list>(
            [ "Detector", 2; "Polarizer", 3; "Sample", 11; "Source", 1 ],
            kindOffer.values |> List.map (fun v -> v.code, v.previewCount))

    [<Fact>]
    let ``the committed text filter narrows the corpus over display names and echoes as the box draft`` () =
        let _, m = freshModel ()
        let narrowed = LW.update (LW.CommitTextFilter "glass") m
        Assert.Equal(6, List.length (LW.filteredEntries narrowed))
        Assert.Contains(glassFilm600EntryId, filteredEntryIds narrowed)
        Assert.DoesNotContain("src-600", filteredEntryIds narrowed)
        Assert.Equal("glass", (LW.facetedState narrowed).filterDraft)
        let restored = LW.update (LW.CommitTextFilter "") narrowed
        Assert.Equal(17, List.length (LW.filteredEntries restored))

    [<Fact>]
    let ``applying the kind facet narrows the corpus and takes a removable breadcrumb chip with its after-count`` () =
        let _, m = freshModel ()
        let constrained = LW.update (kindConstraint "Polarizer") m
        Assert.Equal(3, List.length (LW.filteredEntries constrained))
        Assert.Contains("pol-lp", filteredEntryIds constrained)
        let chips = (LW.facetedState constrained).breadcrumbs
        Assert.Equal(1, List.length chips)
        Assert.Equal(entryKindFacetKey.value, (List.head chips).code)
        Assert.Equal("Kind: Polarizer", (List.head chips).label)
        Assert.Equal(3, (List.head chips).afterCount)
        let removed = LW.update (LW.RemoveFacet entryKindFacetKey) constrained
        Assert.Equal(17, List.length (LW.filteredEntries removed))
        Assert.Empty((LW.facetedState removed).breadcrumbs)

    [<Fact>]
    let ``the polarizer-category facet rides polarizer populations only and the sample facets ride the sample subtree`` () =
        let _, m = freshModel ()
        let offerCodes (model : LW.Model) : string list =
            (LW.facetedState model).offers |> List.map (fun g -> g.code)
        // Over the whole corpus both facet families are offered (polarizers and samples exist).
        Assert.Contains(polarizerCategoryFacetKey.value, offerCodes m)
        Assert.Contains(sampleFilmMaterialKey.value, offerCodes m)
        // Over polarizers only: the polarizer facet stays, every sample facet vanishes.
        let polarizers = LW.update (kindConstraint "Polarizer") m
        Assert.Contains(polarizerCategoryFacetKey.value, offerCodes polarizers)
        Assert.DoesNotContain(sampleFilmMaterialKey.value, offerCodes polarizers)
        Assert.DoesNotContain(sampleFilmThicknessKey.value, offerCodes polarizers)
        // Over a polarizer-free population the polarizer facet vanishes entirely.
        let sources = LW.update (kindConstraint "Source") m
        Assert.DoesNotContain(polarizerCategoryFacetKey.value, offerCodes sources)
        Assert.DoesNotContain(sampleFilmMaterialKey.value, offerCodes sources)
        // The polarizer facet offers the seeded categories with previews (1 LP, 2 CP).
        let polarizerOffer =
            (LW.facetedState polarizers).offers |> List.find (fun g -> g.code = polarizerCategoryFacetKey.value)
        Assert.Equal<(string * int) list>(
            [ "Circular", 2; "Linear", 1 ],
            polarizerOffer.values |> List.map (fun v -> v.code, v.previewCount))

    [<Fact>]
    let ``a lifted sample facet narrows to the samples carrying the constituent — any film layer matches`` () =
        let _, m = freshModel ()
        let constrained =
            LW.update (LW.ApplyFacetConstraint (sampleFilmMaterialKey, DiscreteSelection (Set.singleton (DiscreteKey "Transparent glass (n = 1.52)")))) m
        // glassFilm200 (single film) and multilayerQw (period cell + closing layer) carry
        // glass152 films; every other entry — incl. the plates whose SUBSTRATE is glass152 and
        // every preset — leaves the result set.
        Assert.Equal<string list>(
            ([ SeedSamples.glassFilm200; SeedSamples.multilayerQw ] |> List.map (fun s -> string s.id.value) |> List.sort),
            filteredEntryIds constrained |> List.sort)

    // ============================ pure: the numeric facet (buckets + manual range) ==========

    [<Fact>]
    let ``the film-thickness facet offers step-010 buckets with the manual range box and a bucket click reproduces its count`` () =
        let _, m = freshModel ()
        let state = LW.facetedState m
        let thicknessOffer = state.offers |> List.find (fun g -> g.code = sampleFilmThicknessKey.value)
        Assert.Equal(FacetedTreeControls.ManualRangeOffered, thicknessOffer.manualRange)
        Assert.NotEmpty(thicknessOffer.values)
        // Every OTHER offer group is discrete: no manual box anywhere else.
        for group in state.offers do
            if group.code <> sampleFilmThicknessKey.value then
                Assert.Equal(FacetedTreeControls.NoManualRange, group.manualRange)
        // Applying a bucket through the control's token seam reproduces exactly its preview
        // count (the step-010 pin, now through the window's own handler boundary).
        let bucket = List.head thicknessOffer.values
        let mutable current = m
        let handlers = LW.facetedHandlers (fun msg -> current <- LW.update msg current)
        handlers.applyConstraint sampleFilmThicknessKey.value bucket.code
        Assert.Equal(bucket.previewCount, List.length (LW.filteredEntries current))
        // The chip is removable and carries the bucket-shaped range label.
        let chips = (LW.facetedState current).breadcrumbs
        Assert.Equal(1, List.length chips)
        Assert.StartsWith("Film thickness:", (List.head chips).label)

    [<Fact>]
    let ``the tree's film-thickness branches are buckets, never raw magnitudes`` () =
        let _, m = freshModel ()
        let state = LW.facetedState m
        let thicknessNode =
            state.tree |> List.find (fun n -> n.code = "facet:" + sampleFilmThicknessKey.value)
        Assert.NotEmpty(thicknessNode.children)
        for branch in thicknessNode.children do
            // A bucket branch carries a range code and a unit-labelled range text with a count.
            Assert.StartsWith("branch:" + sampleFilmThicknessKey.value + ":", branch.code)
            match branch.countOpt with
            | Some count -> Assert.True(count > 0, "a bucket branch carries a non-zero count")
            | None -> Assert.Fail($"bucket branch %s{branch.code} must carry its count")
            Assert.Contains("m", branch.label)

    [<Fact>]
    let ``a manual min–max entry applies as an ordinary constraint chip and an unparseable commit is a no-op`` () =
        let _, m = freshModel ()
        let mutable current = m
        let handlers = LW.facetedHandlers (fun msg -> current <- LW.update msg current)
        // "160-260" nm: only the 200 nm glass film's layer lands inside the half-open range.
        handlers.applyManualRange sampleFilmThicknessKey.value "160-260"
        Assert.Equal<string list>(
            [ string SeedSamples.glassFilm200.id.value ],
            filteredEntryIds current)
        Assert.Equal(1, List.length (LW.facetedState current).breadcrumbs)
        // An unparseable commit dispatches nothing — the constraint (and the model) survive.
        let before = current
        handlers.applyManualRange sampleFilmThicknessKey.value "not-a-range"
        handlers.applyManualRange sampleFilmThicknessKey.value ""
        Assert.Equal<LW.Model>(before, current)
        // A single value parses as the degenerate exact-value range (never asserted against a
        // store magnitude — thicknesses round-trip through meters a few ulp off, step 011).
        Assert.Equal<NumericRange option>(Some { lower = 42.0; upper = 42.0 }, LW.parseManualRange "42")

    [<Fact>]
    let ``the numeric range codes round-trip and the range text reuses the step-010 unit switch`` () =
        let range : NumericRange = { lower = 200.0; upper = 500.0 }
        Assert.Equal<NumericRange option>(Some range, LW.numericRangeOfCode (LW.numericRangeCode range))
        Assert.Equal<NumericRange option>(None, LW.numericRangeOfCode "garbage")
        Assert.Equal("200-500 nm", LW.numericRangeText range)
        Assert.Equal("150 nm", LW.numericRangeText { lower = 150.0; upper = 150.0 })
        Assert.Equal("500 nm-1 µm", LW.numericRangeText { lower = 500.0; upper = 1000.0 })

    // ============================ pure: live corpus over the shared stores ============================

    [<Fact>]
    let ``the projection re-queries the LIVE samples store — a store write shows in the next pass`` () =
        let samples, m = freshModelWithStores ()
        Assert.Equal(17, (LW.facetedState m).resultCount)
        // Remove a sample directly through the SHARED store (another window's write): the very
        // next projection of the UNCHANGED model no longer lists it.
        match samples.removeSample SeedSamples.glassFilm200.id with
        | Ok () -> ()
        | Error e -> Assert.Fail($"removing the seeded sample must succeed, got %A{e}")
        Assert.Equal(16, (LW.facetedState m).resultCount)
        Assert.DoesNotContain(string SeedSamples.glassFilm200.id.value, filteredEntryIds m)

    // ============================ pure: verbs + confirm gate ============================

    [<Fact>]
    let ``Add sample and Make multilayer mint fresh upfront ids per dispatch and reach the launcher`` () =
        // Spec 0038 step 008: the id-mint stays off the save path — each verb mints the
        // entity's Guid AT the window-open dispatch, so every Add opens its own registry-keyed
        // editor (the recorded call carries the minted id).
        let calls, m = freshModel ()
        LW.update LW.AddSample m |> ignore
        LW.update LW.AddSample m |> ignore
        match calls |> Seq.filter (fun c -> c.StartsWith "sample-add:") |> List.ofSeq with
        | [ a; b ] -> Assert.NotEqual<string>(a, b)
        | other -> Assert.Fail($"expected two sample Adds, got %A{other}")
        calls.Clear()
        // Make-multilayer is the DISTINCT seeded-period creation path (spec 0035 step 014).
        LW.update LW.MakeMultilayer m |> ignore
        match List.ofSeq calls with
        | [ one ] -> Assert.StartsWith("sample-multilayer:", one)
        | other -> Assert.Fail($"expected one Make-multilayer, got %A{other}")

    [<Fact>]
    let ``Edit reaches the launcher only for a SAMPLE selection — presets and no selection are inert`` () =
        let calls, m = freshModel ()
        m |> LW.update (LW.SelectEntry glassFilm600EntryId) |> LW.update LW.EditSelected |> ignore
        Assert.Equal<string list>([ "sample-edit:Glass thin film (n=1.75, 600 nm)" ], List.ofSeq calls)
        calls.Clear()
        // A preset has no editor — the verb is absent and the dispatch inert.
        m |> LW.update (LW.SelectEntry "pol-lp") |> LW.update LW.EditSelected |> ignore
        LW.update LW.EditSelected m |> ignore
        Assert.Empty(calls)

    [<Fact>]
    let ``acceptance (pure): Remove on a ProtectedBuiltIn entry surfaces the typed refusal and changes nothing`` () =
        let samples, m = freshModelWithStores ()
        let refused =
            m
            |> LW.update (LW.SelectEntry "src-600")
            |> LW.update LW.RequestRemoveSelected
        // The typed reason NAMES the entry; the confirm gate never arms.
        match refused.lastError with
        | Some (LW.ProtectedEntryRefused reason) ->
            Assert.Contains("Monochromatic 600 nm", reason)
            Assert.Contains("protected built-in", reason)
        | other -> Assert.Fail($"expected ProtectedEntryRefused, got %A{other}")
        Assert.Equal(LW.NoPendingRemove, refused.removeGate)
        // Nothing changed: the corpus, the stores, the selection.
        Assert.Equal(17, (LW.facetedState refused).resultCount)
        Assert.Contains("src-600", filteredEntryIds refused)
        match samples.listSamples () with
        | Ok all -> Assert.Equal(11, List.length all)
        | Error e -> Assert.Fail($"listSamples failed: %A{e}")
        // A confirm after the refusal is inert too — there is no armed gate to fire.
        Assert.Equal<LW.Model>(refused, LW.update LW.ConfirmRemove refused)

    [<Fact>]
    let ``Remove on a sample is confirm-gated: request arms with the id, cancel disarms, nothing is removed; no selection is inert`` () =
        let _, m = freshModel ()
        Assert.Equal<LW.Model>(m, LW.update LW.RequestRemoveSelected m)
        let armed =
            m
            |> LW.update (LW.SelectEntry glassFilm600EntryId)
            |> LW.update LW.RequestRemoveSelected
        Assert.Equal(LW.PendingRemove SeedSamples.glassFilm600.id, armed.removeGate)
        let cancelled = LW.update LW.CancelRemove armed
        Assert.Equal(LW.NoPendingRemove, cancelled.removeGate)
        Assert.Equal(17, List.length (LW.filteredEntries cancelled))

    [<Fact>]
    let ``a confirmed sample remove drops the entry from the projection and the shared store in the same pass`` () =
        let samples, m = freshModelWithStores ()
        let removed =
            m
            |> LW.update (LW.SelectEntry glassFilm600EntryId)
            |> LW.update LW.RequestRemoveSelected
            |> LW.update LW.ConfirmRemove
        match removed.lastError with
        | None -> ()
        | Some e -> Assert.Fail($"expected no error, got %A{e}")
        Assert.DoesNotContain(glassFilm600EntryId, filteredEntryIds removed)
        Assert.Equal<string option>(None, removed.selectedEntryId)
        match samples.listSamples () with
        | Ok all -> Assert.Equal(10, List.length all)
        | Error e -> Assert.Fail($"listSamples failed: %A{e}")

    [<Fact>]
    let ``a confirm whose sample vanished behind the window's back surfaces the typed store refusal and changes nothing`` () =
        let samples, m = freshModelWithStores ()
        let armed =
            m
            |> LW.update (LW.SelectEntry glassFilm600EntryId)
            |> LW.update LW.RequestRemoveSelected
        // The sample disappears through the SHARED store (another window's remove)…
        match samples.removeSample SeedSamples.glassFilm600.id with
        | Ok () -> ()
        | Error e -> Assert.Fail($"the out-of-band remove must succeed, got %A{e}")
        // …so the armed confirm meets the typed UnknownSampleId block, surfaced inline.
        let refused = LW.update LW.ConfirmRemove armed
        match refused.lastError with
        | Some (LW.SampleRemoveRefused (UnknownSampleId reason)) -> Assert.Contains("unknown sample id", reason)
        | other -> Assert.Fail($"expected SampleRemoveRefused UnknownSampleId, got %A{other}")
        Assert.Equal(LW.NoPendingRemove, refused.removeGate)
        Assert.Equal(16, (LW.facetedState refused).resultCount)

    [<Fact>]
    let ``a query or selection change disarms a pending remove and clears the refusal`` () =
        let armed () =
            let _, m = freshModel ()
            m |> LW.update (LW.SelectEntry glassFilm600EntryId) |> LW.update LW.RequestRemoveSelected
        Assert.Equal(LW.NoPendingRemove, (LW.update (LW.CommitTextFilter "film") (armed ())).removeGate)
        Assert.Equal(LW.NoPendingRemove, (LW.update (kindConstraint "Sample") (armed ())).removeGate)
        Assert.Equal(LW.NoPendingRemove, (LW.update (LW.RemoveFacet entryKindFacetKey) (armed ())).removeGate)
        let retargeted = LW.update (LW.SelectEntry "src-600") (armed ())
        Assert.Equal(Some "src-600", retargeted.selectedEntryId)
        Assert.Equal(LW.NoPendingRemove, retargeted.removeGate)
        // The typed protected refusal clears on the next query edit (not on a mere cancel).
        let refused =
            let _, m = freshModel ()
            m |> LW.update (LW.SelectEntry "src-600") |> LW.update LW.RequestRemoveSelected
        match refused.lastError with
        | Some (LW.ProtectedEntryRefused _) -> ()
        | other -> Assert.Fail($"expected ProtectedEntryRefused, got %A{other}")
        Assert.Equal(None, (LW.update (LW.CommitTextFilter "si") refused).lastError)

    [<Fact>]
    let ``only an entry node code selects — branches and headings are grouping display`` () =
        Assert.Equal(Some "src-600", LW.entryIdOfNodeCode (LW.entryNodeCode "src-600"))
        Assert.Equal<string option>(None, LW.entryIdOfNodeCode "branch:entry-kind:Sample")
        Assert.Equal<string option>(None, LW.entryIdOfNodeCode "entries")
        let dispatched = ResizeArray<LW.Msg>()
        let handlers = LW.facetedHandlers dispatched.Add
        handlers.selectNode (LW.entryNodeCode "src-600")
        handlers.selectNode "branch:entry-kind:Sample"
        handlers.selectNode "entries"
        Assert.Equal<LW.Msg list>([ LW.SelectEntry "src-600" ], List.ofSeq dispatched)
        // A discrete offer click lifts its tokens back to elevated engine values at the boundary.
        handlers.applyConstraint entryKindFacetKey.value "Polarizer"
        Assert.Equal(kindConstraint "Polarizer", dispatched.[1])

    [<Fact>]
    let ``the view panel target follows the selection and resolves through the live corpus`` () =
        let samples, m = freshModelWithStores ()
        Assert.Equal<LibraryEntry option>(None, LW.selectedEntry m)
        // A preset selection: shown, protected, NOT editable.
        let source = LW.update (LW.SelectEntry "src-600") m
        match LW.selectedEntry source with
        | Some entry ->
            Assert.Equal("Monochromatic 600 nm", entry.displayName)
            Assert.Equal(EntryProtection.ProtectedBuiltIn, entry.protection)
        | None -> Assert.Fail "the selected source entry must resolve"
        Assert.Equal<Sample option>(None, LW.editableSample source)
        // A sample selection: shown AND editable.
        let sample = LW.update (LW.SelectEntry glassFilm600EntryId) m
        match LW.editableSample sample with
        | Some s -> Assert.Equal(SeedSamples.glassFilm600.id, s.id)
        | None -> Assert.Fail "the selected sample must offer Edit"
        // A vanished sample's selection resolves to nothing (its panel vanishes with its row).
        match samples.removeSample SeedSamples.glassFilm600.id with
        | Ok () -> ()
        | Error e -> Assert.Fail($"remove failed: %A{e}")
        Assert.Equal<LibraryEntry option>(None, LW.selectedEntry sample)

    [<Fact>]
    let ``a result count above the threshold gates the tree and Show-Search materializes it`` () =
        let library, samples, materials = freshStores ()
        let _, context = stubContext library samples materials
        let m = LW.init { context with treeAutoBuildThreshold = TreeAutoBuildThreshold 1 }
        let gated = LW.facetedState m
        Assert.Equal(FacetedTreeControls.TreeGated, gated.materialization)
        Assert.Empty(gated.tree)
        Assert.Equal(17, gated.resultCount)
        Assert.NotEmpty(gated.offers)
        let shown = LW.facetedState (LW.update LW.RequestTreeBuild m)
        Assert.Equal(FacetedTreeControls.TreeMaterialized, shown.materialization)
        Assert.NotEmpty(shown.tree)

    // ============================ headless acceptance (ui-smoke) ============================

    /// Mount the REAL Main workbench MVU loop headless (the MainConstructorWindow shape with
    /// injectable stores), for the strip-button acceptance.
    let private mountMain (library : LibraryProxy) (samples : SampleProxy) (materials : MaterialProxy) : HostWindow =
        let categories = CategoryProxy.createInMemory (materialsReferencingCategory materials)
        let model0 = Scene.initMainWith library (Experiments.createInMemory ()) materials samples categories
        let window = HostWindow(Width = 980.0, Height = 1050.0)
        Program.mkSimple (fun () -> model0) Scene.update Scene.mainView
        |> Program.withHost window
        |> Program.run
        window.Show()
        Dispatcher.UIThread.RunJobs()
        window

    let private mountLibraryWindow (library : LibraryProxy) (samples : SampleProxy) (materials : MaterialProxy) : LibraryWindow =
        let window = LibraryWindow(library, samples, materials)
        window.Show()
        Dispatcher.UIThread.RunJobs()
        window

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``acceptance: the strip button opens ONE Library window and a second click ACTIVATES it`` () =
        HeadlessSession.run (fun () ->
            let library, samples, materials = freshStores ()
            let window = mountMain library samples materials
            Assert.True(isPresent window Scene.WorkbenchIds.openLibraryButton,
                        "the ribbon strip row must carry the right-aligned Library… button")
            // Observe the windows the REAL defaults open (the WindowOpenedEvent seam the desktop
            // lifetime itself uses) — subscribed only after the Main window is shown.
            let opened = ResizeArray<Window>()
            use _sub =
                Window.WindowOpenedEvent.Raised
                |> Observable.subscribe (fun (struct (sender, _args)) ->
                    match sender with
                    | :? Window as w -> opened.Add w
                    | _ -> ())
            clickOn window Scene.WorkbenchIds.openLibraryButton
            Dispatcher.UIThread.RunJobs()
            Assert.Equal(1, opened.Count)
            let libraryWindow = opened.[0]
            Assert.True(matchesId LW.UiIds.window libraryWindow, "the opened window must be the Library window")
            Assert.True(libraryWindow.IsVisible)
            // The single-instance acceptance: a second click ACTIVATES the live window — the
            // shared registry under LibraryWindowKey creates nothing new.
            clickOn window Scene.WorkbenchIds.openLibraryButton
            Dispatcher.UIThread.RunJobs()
            Assert.Equal(1, opened.Count)
            Assert.True(libraryWindow.IsVisible, "the activated window must still be the live one")
            // Close → the registry forgets the key, so the NEXT open creates afresh.
            libraryWindow.Close()
            Dispatcher.UIThread.RunJobs()
            clickOn window Scene.WorkbenchIds.openLibraryButton
            Dispatcher.UIThread.RunJobs()
            Assert.Equal(2, opened.Count)
            opened.[1].Close()
            Dispatcher.UIThread.RunJobs()
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``acceptance: the by-kind faceted tree renders every entry kind with counts over the whole corpus`` () =
        HeadlessSession.run (fun () ->
            let library, samples, materials = freshStores ()
            let window = mountLibraryWindow library samples materials
            Assert.Equal("17 results", textOf window FacetedTreeControls.UiIds.resultCount)
            // Sample and preset leaves both render.
            Assert.True(isPresent window (LW.UiIds.entryNode glassFilm600EntryId))
            Assert.True(isPresent window (LW.UiIds.entryNode "src-600"))
            // The kind facet's branches: every entry kind, each with its count.
            for kind, count in [ "Sample", 11; "Source", 1; "Detector", 2; "Polarizer", 3 ] do
                let branchId = FacetedTreeControls.UiIds.treeNode ("branch:" + entryKindFacetKey.value + ":" + kind)
                Assert.True(isPresent window branchId, $"the by-kind tree must list the %s{kind} branch")
                Assert.Equal($"%s{kind} (%d{count})", textOf window branchId)
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``acceptance: Add sample persists through the real editor into the shared store and the window lists it`` () =
        HeadlessSession.run (fun () ->
            let library, samples, materials = freshStores ()
            let window = mountLibraryWindow library samples materials
            let opened = ResizeArray<Window>()
            use _sub =
                Window.WindowOpenedEvent.Raised
                |> Observable.subscribe (fun (struct (sender, _args)) ->
                    match sender with
                    | :? Window as w -> opened.Add w
                    | _ -> ())
            // Add sample → the REAL Sample editor over the SHARED stores (through the real
            // launcher under the Add-minted SampleEditorKey).
            clickOn window LW.UiIds.addSampleButton
            Dispatcher.UIThread.RunJobs()
            Assert.Equal(1, opened.Count)
            let editor = opened.[0]
            Assert.True(matchesId SampleEditorView.UiIds.window editor, "the opened window must be the Sample editor")
            // Name it and give it one layer (a valid stack), then Save — NewUnsaved → addSample.
            setText editor SampleEditorView.UiIds.nameBox "Library window sample"
            clickOn editor (SampleEditorView.UiIds.materialOption (string MaterialIds.glass152.value))
            clickOn editor SampleEditorView.UiIds.addLayerButton
            clickOn editor SampleEditorView.UiIds.saveButton
            Dispatcher.UIThread.RunJobs()
            Assert.False(editor.IsVisible, "Save must close the editor")
            // The save landed in the SHARED store…
            match samples.listSamples () with
            | Ok all -> Assert.Contains(all, fun (s : Sample) -> s.name = "Library window sample")
            | Error e -> Assert.Fail($"listSamples failed: %A{e}")
            // …and the window's next dispatch-driven render re-queries it: committing the
            // matching filter narrows the tree to the just-saved entry.
            commitFilter window "Library window sample"
            Assert.Equal("1 results", textOf window FacetedTreeControls.UiIds.resultCount)
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``acceptance: Remove on a protected entry surfaces the typed refusal and changes nothing`` () =
        HeadlessSession.run (fun () ->
            let library, samples, materials = freshStores ()
            let window = mountLibraryWindow library samples materials
            // Narrow to the protected source and select its leaf.
            commitFilter window "Monochromatic"
            clickOn window (LW.UiIds.entryNode "src-600")
            clickOn window LW.UiIds.removeButton
            // The typed refusal renders inline; the confirm gate never armed.
            let message = textOf window LW.UiIds.message
            Assert.Contains("Monochromatic 600 nm", message)
            Assert.Contains("protected built-in", message)
            Assert.False(isPresent window LW.UiIds.removeConfirmButton,
                         "the confirm gate must never arm for a protected entry")
            // Nothing changed: the entry is still listed and the samples store untouched.
            Assert.True(isPresent window (LW.UiIds.entryNode "src-600"))
            Assert.Equal("1 results", textOf window FacetedTreeControls.UiIds.resultCount)
            match samples.listSamples () with
            | Ok all -> Assert.Equal(11, List.length all)
            | Error e -> Assert.Fail($"listSamples failed: %A{e}")
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``headless: removing a sample through the confirm gate drops its row in the same render pass`` () =
        HeadlessSession.run (fun () ->
            let library, samples, materials = freshStores ()
            let window = mountLibraryWindow library samples materials
            commitFilter window "n=1.75"
            clickOn window (LW.UiIds.entryNode glassFilm600EntryId)
            clickOn window LW.UiIds.removeButton
            clickOn window LW.UiIds.removeConfirmButton
            Assert.False(isPresent window (LW.UiIds.entryNode glassFilm600EntryId),
                         "the removed sample's row must leave the tree in the same render pass")
            match samples.listSamples () with
            | Ok all -> Assert.Equal(10, List.length all)
            | Error e -> Assert.Fail($"listSamples failed: %A{e}")
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``headless: Edit opens the real Sample editor on the selected sample and Make multilayer opens the seeded editor`` () =
        HeadlessSession.run (fun () ->
            let library, samples, materials = freshStores ()
            let window = mountLibraryWindow library samples materials
            commitFilter window "n=1.75"
            clickOn window (LW.UiIds.entryNode glassFilm600EntryId)
            let opened = ResizeArray<Window>()
            use _sub =
                Window.WindowOpenedEvent.Raised
                |> Observable.subscribe (fun (struct (sender, _args)) ->
                    match sender with
                    | :? Window as w -> opened.Add w
                    | _ -> ())
            clickOn window LW.UiIds.editButton
            Dispatcher.UIThread.RunJobs()
            Assert.Equal(1, opened.Count)
            Assert.True(matchesId SampleEditorView.UiIds.window opened.[0], "the opened window must be the Sample editor")
            Assert.Contains("Glass thin film", opened.[0].Title)
            opened.[0].Close()
            Dispatcher.UIThread.RunJobs()
            // Make multilayer: a NEW editor seeded with the foldable 2-layer period — one
            // super-row plus its two cell rows (spec 0035 step 014), and Save persists it.
            clickOn window LW.UiIds.makeMultilayerButton
            Dispatcher.UIThread.RunJobs()
            Assert.Equal(2, opened.Count)
            let editor = opened.[1]
            Assert.True(isPresent editor (SampleEditorView.UiIds.groupRow 0), "the seeded period super-row must render")
            Assert.True(isPresent editor (SampleEditorView.UiIds.cellLayerRow 0 0), "seeded cell layer 0 must render")
            Assert.True(isPresent editor (SampleEditorView.UiIds.cellLayerRow 0 1), "seeded cell layer 1 must render")
            setText editor SampleEditorView.UiIds.nameBox "Window multilayer"
            clickOn editor SampleEditorView.UiIds.saveButton
            Assert.False(editor.IsVisible)
            match samples.listSamples () with
            | Ok all -> Assert.Contains(all, fun (s : Sample) -> s.name = "Window multilayer")
            | Error e -> Assert.Fail($"listSamples failed: %A{e}")
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``headless: selecting an entry leaf shows the view panel with the kind, protection and full description`` () =
        HeadlessSession.run (fun () ->
            let library, samples, materials = freshStores ()
            let window = mountLibraryWindow library samples materials
            Assert.False(isPresent window LW.UiIds.viewPanel, "no selection → no view panel")
            commitFilter window "Monochromatic"
            clickOn window (LW.UiIds.entryNode "src-600")
            Assert.True(isPresent window LW.UiIds.viewPanel, "the view panel must render for the selected entry")
            let panelText = textOf window LW.UiIds.viewPanel
            Assert.Contains("Monochromatic 600 nm", panelText)
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``headless: a gated tree shows Show-Search with ZERO rows and the click materializes the entry leaves`` () =
        HeadlessSession.run (fun () ->
            let library, samples, materials = freshStores ()
            let _, context = stubContext library samples materials
            // Mount the REAL MVU loop over a tiny threshold (the window minus its launcher
            // composition — the FacetedTreeControlsTests mounting precedent).
            let window = HostWindow(Width = 900.0, Height = 760.0)
            Program.mkSimple
                (fun () -> LW.init { context with treeAutoBuildThreshold = TreeAutoBuildThreshold 1 })
                LW.update
                LW.view
            |> Program.withHost window
            |> Program.run
            window.Show()
            Dispatcher.UIThread.RunJobs()
            Assert.True(isPresent window FacetedTreeControls.UiIds.showTreeButton, "the Show/Search button must gate the tree")
            let treeRowCount () =
                window.GetVisualDescendants()
                |> Seq.filter (fun v ->
                    match v with
                    | :? Control as c ->
                        let autoId = Avalonia.Automation.AutomationProperties.GetAutomationId(c)
                        not (isNull autoId) && autoId.StartsWith("FacetTreeNode_")
                    | _ -> false)
                |> Seq.length
            Assert.Equal(0, treeRowCount ())
            Assert.Equal("17 results", textOf window FacetedTreeControls.UiIds.resultCount)
            clickOn window FacetedTreeControls.UiIds.showTreeButton
            Assert.True(treeRowCount () > 0, "the explicit build must materialize the tree")
            Assert.True(isPresent window (LW.UiIds.entryNode "src-600"))
            window.Close())
