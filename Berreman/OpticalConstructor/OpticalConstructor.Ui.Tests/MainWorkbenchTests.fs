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
open OpticalConstructor.Controls
open OpticalConstructor.TestWindows
open OpticalConstructor.TestWindows.TableAndElementRotationView

/// Spec 0033 (024) — the Main-screen MATERIALS and LIBRARY workbench bays: the step-015/016
/// list surfaces (`MaterialsControls` / `SampleLibraryControls`) wired over the step-005/006
/// write seams (`MaterialProxy` / `SampleProxy`) in `TableAndElementRotationView`. Two layers,
/// the repo precedent: pure tests for the host projections / the `Mat…`/`Smp…` update arms, and
/// headless proofs that DRIVE THE REAL ELMISH LOOP BY UiIds — the slice acceptance: search
/// filters the rows, Add/Edit open the step-022/023 editor windows, removing a REFERENCED
/// material surfaces the `MaterialStillReferenced` message and leaves the store unchanged, and
/// removing an unreferenced entry updates the list in the same render pass.
module MainWorkbenchTests =

    /// A control matches `id` by its `Name` OR its `AutomationProperties.AutomationId` (the
    /// workbench rows / facet options / verb buttons live in variable-membership lists, so they
    /// carry an AutomationId — the MaterialsControls precedent).
    let private matchesId (id : string) (c : Control) : bool =
        c.Name = id || Avalonia.Automation.AutomationProperties.GetAutomationId(c) = id

    let private tryFindControl (window : Window) (id : string) : Control option =
        window.GetVisualDescendants()
        |> Seq.tryPick (function :? Control as c when matchesId id c -> Some c | _ -> None)

    /// Present anywhere in the visual tree (visible or not) — the "is it listed" probe; a row
    /// the host's re-query dropped is REMOVED from the tree, not merely hidden.
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

    /// Set the text of the TextBox carrying `id` (fires the property-change subscription the
    /// control's `onTextChanged` binds — still driving the control found by its UiId).
    let private setText (window : Window) (id : string) (text : string) : unit =
        match tryFindControl window id with
        | Some (:? TextBox as tb) ->
            tb.Text <- text
            Dispatcher.UIThread.RunJobs()
        | Some c -> Assert.Fail($"%s{id} is a %s{c.GetType().Name}, not a TextBox")
        | None -> Assert.Fail($"%s{id} was not found")

    /// The text of the TextBlock carrying `id`.
    let private textOf (window : Window) (id : string) : string =
        match tryFindControl window id with
        | Some (:? TextBlock as tb) -> tb.Text
        | Some c -> failwith $"%s{id} is a %s{c.GetType().Name}, not a TextBlock"
        | None -> failwith $"%s{id} was not found in the visual tree"

    /// Fresh, isolated in-memory stores per test — the SAME composition the App performs: the
    /// samples store first, then the materials store whose remove-block consults the LIVE
    /// samples through `samplesReferencing`.
    let private freshStores () : MaterialProxy * SampleProxy =
        let samples = SampleProxy.createInMemory ()
        let materials = MaterialProxy.createInMemory (samplesReferencing samples)
        materials, samples

    /// The Main-scene model over the given stores (mock Library/Experiments proxies as in App).
    let private mainWith (materials : MaterialProxy) (samples : SampleProxy) : Model =
        initMainWith (Library.createInMemory ()) (Experiments.createInMemory ()) materials samples

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

    /// A recording launcher pair (the functional-proxy seam — tests observe which editor a
    /// verb requested without opening a window).
    let private recordingLaunchers () : ResizeArray<string> * EditorLaunchers =
        let calls = ResizeArray<string>()
        let launchers : EditorLaunchers =
            {
                openMaterialEditor =
                    fun _ existing ->
                        calls.Add(match existing with Some e -> "material-edit:" + e.name | None -> "material-add")
                openSampleEditor =
                    fun _ _ existing ->
                        calls.Add(match existing with Some s -> "sample-edit:" + s.name | None -> "sample-add")
            }
        calls, launchers

    let private rowIds (state : MaterialsControls.State) : string list =
        state.rows |> List.map (fun r -> r.materialId)

    let private sampleRowIds (state : SampleLibraryControls.State) : string list =
        state.rows |> List.map (fun r -> r.sampleId)

    // ============================ pure: bay roster ============================

    [<Fact>]
    let ``the workbench bay names are Materials and Library and both are offered by the ribbon`` () =
        Assert.Equal("Materials", BayNames.materials)
        Assert.Equal("Library", BayNames.library)
        Assert.Contains(BayNames.materials, BayNames.all)
        Assert.Contains(BayNames.library, BayNames.all)
        // The samples workbench reuses the label the step-014 Selector rename freed — the
        // Selector bay itself is unchanged and distinct.
        Assert.Contains(BayNames.selector, BayNames.all)
        let m = freshMain ()
        let bays = mainBays m ignore
        Assert.Equal<string list>(BayNames.all, bays |> List.map (fun b -> b.name))

    [<Fact>]
    let ``the workbench facet code maps round-trip`` () =
        Assert.Equal(Some CategoryIds.glass, materialCategoryOfCode (materialCategoryCode (Some CategoryIds.glass)))
        Assert.Equal(Some CategoryIds.crystal, materialCategoryOfCode (materialCategoryCode (Some CategoryIds.crystal)))
        Assert.Equal<CategoryId option>(None, materialCategoryOfCode "all")
        Assert.Equal(OnlyDispersive, dispersionFilterOfCode (dispersionFilterCode OnlyDispersive))
        Assert.Equal(AnyDispersion, dispersionFilterOfCode "all")
        Assert.Equal(Some Plate, substrateFacetOfCode (substrateFacetCode (Some Plate)))
        Assert.Equal<SubstrateKind option>(None, substrateFacetOfCode "all")

    // ============================ pure: projections ============================

    [<Fact>]
    let ``materialsState lists every stored material and editability follows the step-013 complexity`` () =
        let m = freshMain ()
        let st = materialsState m
        Assert.Equal(12, List.length st.rows)
        Assert.Equal("", st.searchText)
        Assert.Equal("all", st.selectedCategory)
        Assert.Equal("all", st.selectedDispersion)
        let editabilityOf (id : MaterialId) : MaterialsControls.MaterialEditability =
            (st.rows |> List.find (fun r -> r.materialId = string id.value)).editability
        Assert.Equal(MaterialsControls.Editable, editabilityOf MaterialIds.glass152)
        Assert.Equal(MaterialsControls.ViewOnly, editabilityOf MaterialIds.silicon)
        Assert.Equal(MaterialsControls.ViewOnly, editabilityOf MaterialIds.vacuum)
        Assert.Equal(MaterialsControls.ViewOnly, editabilityOf MaterialIds.langasite)

    [<Fact>]
    let ``the materials search text and facet selectors drive searchMaterials`` () =
        let m = freshMain ()
        // Name-fragment search: the four transparent glasses.
        let byText = update (MatSetSearchText "glass") m
        Assert.Equal(4, List.length (materialsState byText).rows)
        Assert.Contains(string MaterialIds.glass152.value, rowIds (materialsState byText))
        Assert.DoesNotContain(string MaterialIds.silicon.value, rowIds (materialsState byText))
        // The category facet: the four crystals.
        let byCategory = update (MatSelectCategory (Some CategoryIds.crystal)) m
        Assert.Equal(4, List.length (materialsState byCategory).rows)
        Assert.Contains(string MaterialIds.langasite.value, rowIds (materialsState byCategory))
        // The dispersion facet: only the wavelength-dependent presets.
        let byDispersion = update (MatSelectDispersion OnlyDispersive) m
        Assert.Equal(2, List.length (materialsState byDispersion).rows)
        Assert.Contains(string MaterialIds.silicon.value, rowIds (materialsState byDispersion))
        // Facets compose: no glass is a crystal.
        let composed = m |> update (MatSetSearchText "glass") |> update (MatSelectCategory (Some CategoryIds.crystal))
        Assert.Empty((materialsState composed).rows)

    [<Fact>]
    let ``samplesState lists the seeded samples and the search text + substrate facet drive searchSamples`` () =
        let m = freshMain ()
        Assert.Equal(11, List.length (samplesState m).rows)
        let byText = update (SmpSetSearchText "glass") m
        Assert.Equal(6, List.length (samplesState byText).rows)
        Assert.Contains(string SeedSamples.multilayerQw.id.value, sampleRowIds (samplesState byText))
        let byFacet = update (SmpSelectSubstrate (Some Plate)) m
        Assert.Equal(4, List.length (samplesState byFacet).rows)
        Assert.Contains(string SeedSamples.glassPlate1mm.id.value, sampleRowIds (samplesState byFacet))
        Assert.DoesNotContain(string SeedSamples.glassFilm600.id.value, sampleRowIds (samplesState byFacet))

    // ============================ pure: confirm-gated remove ============================

    [<Fact>]
    let ``Remove is confirm-gated: request arms the inline confirm, Cancel backs out, no selection is inert`` () =
        let m = freshMain ()
        // No selection → the request is inert.
        Assert.Equal(NoRemoveConfirm, (update MatRequestRemove m).materialRemoveConfirm)
        let armed = m |> update (MatSelectRow MaterialIds.glass200) |> update MatRequestRemove
        Assert.Equal(ConfirmingRemove MaterialIds.glass200, armed.materialRemoveConfirm)
        let cancelled = update MatCancelRemove armed
        Assert.Equal(NoRemoveConfirm, cancelled.materialRemoveConfirm)
        // Nothing was removed by arming/cancelling.
        Assert.Equal(12, List.length (materialsState cancelled).rows)

    [<Fact>]
    let ``acceptance (pure): removing a REFERENCED material surfaces MaterialStillReferenced and leaves the store unchanged`` () =
        let materials, samples = freshStores ()
        let m = mainWith materials samples
        let refused =
            m
            |> update (MatSelectRow MaterialIds.glass152)
            |> update MatRequestRemove
            |> update MatConfirmRemove
        match refused.materialsError with
        | Some (MaterialStillReferenced reason) ->
            Assert.Contains("still referenced", reason)
            // The block NAMES the referencing samples (never a cascade).
            Assert.Contains("Glass plate (n=1.52, 1 mm)", reason)
        | other -> Assert.Fail($"expected MaterialStillReferenced, got %A{other}")
        // The store is unchanged and the projection still lists the entry.
        match materials.listMaterials () with
        | Ok entries ->
            Assert.Equal(12, List.length entries)
            Assert.Contains(MaterialIds.glass152, entries |> List.map (fun e -> e.id))
        | Error e -> Assert.Fail($"listMaterials failed: %A{e}")
        Assert.Contains(string MaterialIds.glass152.value, rowIds (materialsState refused))

    [<Fact>]
    let ``acceptance (pure): removing an UNREFERENCED material drops the row from the projection in the same pass`` () =
        let materials, samples = freshStores ()
        let m = mainWith materials samples
        let removed =
            m
            |> update (MatSelectRow MaterialIds.glass200)
            |> update MatRequestRemove
            |> update MatConfirmRemove
        match removed.materialsError with
        | None -> ()
        | Some e -> Assert.Fail($"expected no error, got %A{e}")
        Assert.DoesNotContain(string MaterialIds.glass200.value, rowIds (materialsState removed))
        Assert.Equal<MaterialId option>(None, removed.selectedMaterial)
        match materials.listMaterials () with
        | Ok entries -> Assert.Equal(11, List.length entries)
        | Error e -> Assert.Fail($"listMaterials failed: %A{e}")

    [<Fact>]
    let ``acceptance (pure): removing a sample drops its row from the samples projection in the same pass`` () =
        let materials, samples = freshStores ()
        let m = mainWith materials samples
        let removed =
            m
            |> update (SmpSelectRow SeedSamples.glassFilm600.id)
            |> update SmpRequestRemove
            |> update SmpConfirmRemove
        match removed.samplesError with
        | None -> ()
        | Some e -> Assert.Fail($"expected no error, got %A{e}")
        Assert.DoesNotContain(string SeedSamples.glassFilm600.id.value, sampleRowIds (samplesState removed))
        match samples.listSamples () with
        | Ok all -> Assert.Equal(10, List.length all)
        | Error e -> Assert.Fail($"listSamples failed: %A{e}")

    // ============================ pure: Add / Edit / View verbs ============================

    [<Fact>]
    let ``Add, Edit and Make-multilayer reach the editor launchers with the right target`` () =
        let calls, launchers = recordingLaunchers ()
        let m = { freshMain () with launchers = launchers }
        update MatAdd m |> ignore
        Assert.Contains("material-add", calls)
        m |> update (MatSelectRow MaterialIds.glass152) |> update MatEdit |> ignore
        Assert.Contains("material-edit:Transparent glass (n = 1.52)", calls)
        update SmpAdd m |> ignore
        Assert.Contains("sample-add", calls)
        m |> update (SmpSelectRow SeedSamples.multilayerQw.id) |> update SmpEdit |> ignore
        Assert.Contains("sample-edit:Quarter-wave glass/vacuum multilayer (41 layers)", calls)
        // Make-multilayer is the second creation entry point: it opens the sample editor on a
        // NEW sample (the stack editor's fold vocabulary is the multilayer flow).
        calls.Clear()
        update SmpMakeMultilayer m |> ignore
        Assert.Equal<string list>([ "sample-add" ], List.ofSeq calls)
        // Edit without a selection reaches no launcher.
        calls.Clear()
        update MatEdit m |> ignore
        update SmpEdit m |> ignore
        Assert.Empty(calls)

    [<Fact>]
    let ``View toggles the read-only panel target for the selected entry`` () =
        let m = freshMain () |> update (MatSelectRow MaterialIds.glass152)
        let shown = update MatView m
        Assert.Equal(Some MaterialIds.glass152, shown.viewedMaterial)
        Assert.Equal<MaterialId option>(None, (update MatView shown).viewedMaterial)
        let s = freshMain () |> update (SmpSelectRow SeedSamples.multilayerQw.id)
        let shownSample = update SmpView s
        Assert.Equal(Some SeedSamples.multilayerQw.id, shownSample.viewedSample)
        Assert.Equal<SampleId option>(None, (update SmpView shownSample).viewedSample)

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
    let ``headless: the Materials bay search filters the listed rows by UiIds`` () =
        HeadlessSession.run (fun () ->
            let materials, samples = freshStores ()
            let window = mountMain (mainWith materials samples)
            clickOn window (Ribbon.UiIds.tab BayNames.materials)
            // Every stored material is listed before the search narrows it.
            Assert.True(isPresent window (MaterialsControls.UiIds.row (string MaterialIds.glass152.value)))
            Assert.True(isPresent window (MaterialsControls.UiIds.row (string MaterialIds.silicon.value)))
            setText window MaterialsControls.UiIds.searchBox "glass"
            Assert.True(isPresent window (MaterialsControls.UiIds.row (string MaterialIds.glass152.value)),
                        "the matching row must stay listed")
            Assert.False(isPresent window (MaterialsControls.UiIds.row (string MaterialIds.silicon.value)),
                         "the non-matching row must leave the tree in the same render pass")
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``headless acceptance: Add opens the Material editor and Edit opens the Sample editor`` () =
        HeadlessSession.run (fun () ->
            let materials, samples = freshStores ()
            // Recording launchers that still open the REAL step-022/023 editor windows, so the
            // proof is end-to-end: verb click by UiIds → the real editor window is shown.
            let opened = ResizeArray<Window>()
            let launchers : EditorLaunchers =
                {
                    openMaterialEditor =
                        fun m existing ->
                            let w = MaterialEditorWindow(m, existing)
                            opened.Add w
                            w.Show()
                    openSampleEditor =
                        fun m s existing ->
                            let w = SampleEditorWindow(m, s, existing)
                            opened.Add w
                            w.Show()
                }
            let window = mountMain { mainWith materials samples with launchers = launchers }
            clickOn window (Ribbon.UiIds.tab BayNames.materials)
            clickOn window MaterialsControls.UiIds.addButton
            Dispatcher.UIThread.RunJobs()
            Assert.Equal(1, opened.Count)
            Assert.True(opened.[0].IsVisible, "the Material editor window must be shown")
            Assert.Equal(MaterialEditorView.UiIds.window, Avalonia.Automation.AutomationProperties.GetAutomationId(opened.[0]))
            opened.[0].Close()
            // The Library bay: narrow the search, select the sample, Edit — the step-022 editor
            // opens seeded with that sample.
            clickOn window (Ribbon.UiIds.tab BayNames.library)
            setText window SampleLibraryControls.UiIds.searchBox "n=1.75"
            clickOn window (SampleLibraryControls.UiIds.row (string SeedSamples.glassFilm600.id.value))
            clickOn window SampleLibraryControls.UiIds.editButton
            Dispatcher.UIThread.RunJobs()
            Assert.Equal(2, opened.Count)
            Assert.True(opened.[1].IsVisible, "the Sample editor window must be shown")
            Assert.Contains("Glass thin film", opened.[1].Title)
            opened.[1].Close()
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``headless acceptance: removing a REFERENCED material surfaces the inline message and leaves the store unchanged`` () =
        HeadlessSession.run (fun () ->
            let materials, samples = freshStores ()
            let window = mountMain (mainWith materials samples)
            clickOn window (Ribbon.UiIds.tab BayNames.materials)
            // Narrow the list so the target row sits inside the scroll viewport, then drive the
            // confirm-gated remove by UiIds.
            setText window MaterialsControls.UiIds.searchBox "1.52"
            clickOn window (MaterialsControls.UiIds.row (string MaterialIds.glass152.value))
            clickOn window MaterialsControls.UiIds.removeButton
            clickOn window WorkbenchIds.removeMaterialConfirm
            let message = textOf window WorkbenchIds.materialsMessage
            Assert.Contains("still referenced", message)
            Assert.Contains("Glass plate", message)
            // The store is unchanged and the row is still listed.
            match materials.listMaterials () with
            | Ok entries -> Assert.Equal(12, List.length entries)
            | Error e -> Assert.Fail($"listMaterials failed: %A{e}")
            Assert.True(isPresent window (MaterialsControls.UiIds.row (string MaterialIds.glass152.value)))
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``headless acceptance: removing unreferenced entries updates both lists in the same render pass`` () =
        HeadlessSession.run (fun () ->
            let materials, samples = freshStores ()
            let window = mountMain (mainWith materials samples)
            // Materials bay: glass200 is referenced by no seeded sample.
            clickOn window (Ribbon.UiIds.tab BayNames.materials)
            setText window MaterialsControls.UiIds.searchBox "2.00"
            clickOn window (MaterialsControls.UiIds.row (string MaterialIds.glass200.value))
            clickOn window MaterialsControls.UiIds.removeButton
            clickOn window WorkbenchIds.removeMaterialConfirm
            Assert.False(isPresent window (MaterialsControls.UiIds.row (string MaterialIds.glass200.value)),
                         "the removed material's row must leave the tree in the same render pass")
            match materials.listMaterials () with
            | Ok entries -> Assert.Equal(11, List.length entries)
            | Error e -> Assert.Fail($"listMaterials failed: %A{e}")
            // Library bay: a sample remove always succeeds and drops the row.
            clickOn window (Ribbon.UiIds.tab BayNames.library)
            setText window SampleLibraryControls.UiIds.searchBox "n=1.75"
            clickOn window (SampleLibraryControls.UiIds.row (string SeedSamples.glassFilm600.id.value))
            clickOn window SampleLibraryControls.UiIds.removeButton
            clickOn window WorkbenchIds.removeSampleConfirm
            Assert.False(isPresent window (SampleLibraryControls.UiIds.row (string SeedSamples.glassFilm600.id.value)),
                         "the removed sample's row must leave the tree in the same render pass")
            match samples.listSamples () with
            | Ok all -> Assert.Equal(10, List.length all)
            | Error e -> Assert.Fail($"listSamples failed: %A{e}")
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``headless: the View panels render the n-k chart for a material and the band view for a sample`` () =
        HeadlessSession.run (fun () ->
            let materials, samples = freshStores ()
            let window = mountMain (mainWith materials samples)
            // Materials View: read-only metadata plus the step-19 dual-axis n/k chart canvas.
            clickOn window (Ribbon.UiIds.tab BayNames.materials)
            setText window MaterialsControls.UiIds.searchBox "1.52"
            clickOn window (MaterialsControls.UiIds.row (string MaterialIds.glass152.value))
            clickOn window MaterialsControls.UiIds.viewButton
            Assert.True(isPresent window WorkbenchIds.materialViewPanel, "the material View panel must render")
            Assert.True(isPresent window WorkbenchIds.materialNkChart, "the n/k chart canvas must render")
            // Library View: the LayerBandsControls band view over the sample's stack (the
            // Details-bay rendering), scoped to the panel so the Details bay's own instance
            // cannot satisfy the assertion.
            clickOn window (Ribbon.UiIds.tab BayNames.library)
            setText window SampleLibraryControls.UiIds.searchBox "Quarter-wave"
            clickOn window (SampleLibraryControls.UiIds.row (string SeedSamples.multilayerQw.id.value))
            clickOn window SampleLibraryControls.UiIds.viewButton
            match tryFindControl window WorkbenchIds.sampleViewPanel with
            | None -> Assert.Fail("the sample View panel must render")
            | Some panel ->
                let bandTexts =
                    panel.GetVisualDescendants()
                    |> Seq.choose (function :? TextBlock as t -> Some t.Text | _ -> None)
                    |> List.ofSeq
                Assert.True(
                    panel.GetVisualDescendants()
                    |> Seq.exists (function :? Control as c when c.Name = LayerBandsControls.UiIds.band 0 -> true | _ -> false),
                    "the band view must draw its first band inside the panel")
                Assert.Contains(bandTexts, fun t -> not (isNull t) && t.Contains "×20")
            window.Close())
