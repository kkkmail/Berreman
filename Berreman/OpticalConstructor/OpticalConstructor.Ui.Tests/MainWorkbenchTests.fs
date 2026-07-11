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
open OpticalConstructor.Ui
open OpticalConstructor.Ui.TableAndElementRotationView

/// Spec 0033 (024) / 0038 (013) — the Main-screen LIBRARY (samples) workbench bay: the step-016
/// list surface (`SampleLibraryControls`) wired over the step-005 write seam (`SampleProxy`) in
/// `TableAndElementRotationView`. Two layers, the repo precedent: pure tests for the host
/// projections / the `Smp…` update arms and the launcher seam, and headless proofs that DRIVE
/// THE REAL ELMISH LOOP BY UiIds. The MATERIALS bay these tests used to cover beside it is now
/// the single-instance Materials WINDOW (spec 0038 step 013) — its coverage lives in
/// `MaterialsWindowTests`; what remains here is the workbench side: the bay roster without a
/// Materials bay, the strip button's launcher seam, and the samples workbench itself.
module MainWorkbenchTests =

    /// A control matches `id` by its `Name` OR its `AutomationProperties.AutomationId` (the
    /// workbench rows / facet options / verb buttons live in variable-membership lists, so they
    /// carry an AutomationId — the SampleLibraryControls precedent).
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

    /// Fresh, isolated in-memory stores per test — the SAME composition the App performs: the
    /// samples store first, then the materials store whose remove-block consults the LIVE
    /// samples through `samplesReferencing`.
    let private freshStores () : MaterialProxy * SampleProxy =
        let samples = SampleProxy.createInMemory ()
        let materials = MaterialProxy.createInMemory (samplesReferencing samples)
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
    /// verb requested without opening one). The NEW sample intents record their upfront-minted
    /// id (spec 0038 step 008 — the verb mints at the window-open dispatch); the strip button's
    /// Materials-window request records its tag (spec 0038 step 013).
    let private recordingLaunchers () : ResizeArray<string> * EditorLaunchers =
        let calls = ResizeArray<string>()
        let launchers : EditorLaunchers =
            {
                openSampleEditor =
                    fun _ _ intent ->
                        calls.Add(
                            match intent with
                            | SampleEditorView.NewBlankSample mintedId -> $"sample-add:{mintedId.value}"
                            | SampleEditorView.NewSeededMultilayer _ -> "sample-multilayer"
                            | SampleEditorView.EditSample s -> "sample-edit:" + s.name)
                openMaterialsWindow = fun _ _ -> calls.Add "materials-window"
            }
        calls, launchers

    let private sampleRowIds (state : SampleLibraryControls.State) : string list =
        state.rows |> List.map (fun r -> r.sampleId)

    // ============================ pure: bay roster ============================

    [<Fact>]
    let ``the ribbon offers no Materials bay any more — Library is the one full-surface workbench bay`` () =
        // Spec 0038 step 013: the Materials bay left the ribbon (the Materials WINDOW carries
        // the workbench now); the Library (samples) bay stays, LAST, beside the Selector.
        Assert.Equal("Library", BayNames.library)
        Assert.Contains(BayNames.library, BayNames.all)
        Assert.Contains(BayNames.selector, BayNames.all)
        Assert.DoesNotContain("Materials", BayNames.all)
        Assert.Equal(Some BayNames.library, List.tryLast BayNames.all)
        let m = freshMain ()
        let bays = mainBays m ignore
        Assert.Equal<string list>(BayNames.all, bays |> List.map (fun b -> b.name))

    [<Fact>]
    let ``the samples substrate facet code map round-trips`` () =
        Assert.Equal(Some Plate, substrateFacetOfCode (substrateFacetCode (Some Plate)))
        Assert.Equal(Some Wedge, substrateFacetOfCode (substrateFacetCode (Some Wedge)))
        Assert.Equal(Some ThinFilm, substrateFacetOfCode (substrateFacetCode (Some ThinFilm)))
        Assert.Equal<SubstrateKind option>(None, substrateFacetOfCode "all")

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

    // ============================ pure: projections ============================

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

    // ============================ pure: the launcher seam ============================

    [<Fact>]
    let ``Add, Edit, Make-multilayer and the strip button reach the launchers with the right target`` () =
        let calls, launchers = recordingLaunchers ()
        let m = { freshMain () with launchers = launchers }
        update SmpAdd m |> ignore
        Assert.Contains(calls, fun (c : string) -> c.StartsWith "sample-add:")
        m |> update (SmpSelectRow SeedSamples.multilayerQw.id) |> update SmpEdit |> ignore
        Assert.Contains("sample-edit:Quarter-wave glass/vacuum multilayer (41 layers)", calls)
        // Make-multilayer is the second creation entry point, and its DISTINCT launcher path
        // (spec 0035 step 014) opens the editor on a NEW sample SEEDED with a foldable period —
        // not the blank Add. It records "sample-multilayer", proving the paths diverged.
        calls.Clear()
        update SmpMakeMultilayer m |> ignore
        Assert.Equal<string list>([ "sample-multilayer" ], List.ofSeq calls)
        // The strip button's request (spec 0038 step 013): a pure launch over the model's stores.
        calls.Clear()
        let after = update OpenMaterialsWindow m
        Assert.Equal<string list>([ "materials-window" ], List.ofSeq calls)
        Assert.Equal<Model>(m, after)
        // Edit without a selection reaches no launcher.
        calls.Clear()
        update SmpEdit m |> ignore
        Assert.Empty(calls)

    [<Fact>]
    let ``two sample Adds mint two DISTINCT upfront ids at the window-open dispatch`` () =
        // Spec 0038 step 008: the id-mint left the save path — the Add verb mints the entity's
        // Guid AT WINDOW OPEN and hands it to the launcher inside the intent, so every Add
        // opens its own registry-keyed editor (the recorded call carries the minted id). The
        // material-side twin of this pin lives in MaterialsWindowTests (step 013).
        let calls, launchers = recordingLaunchers ()
        let m = { freshMain () with launchers = launchers }
        update SmpAdd m |> ignore
        update SmpAdd m |> ignore
        match calls |> Seq.filter (fun c -> c.StartsWith "sample-add:") |> List.ofSeq with
        | [ a; b ] -> Assert.NotEqual<string>(a, b)
        | other -> Assert.Fail($"expected two sample Adds, got %A{other}")

    [<Fact>]
    let ``View toggles the read-only panel target for the selected sample`` () =
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
    let ``headless acceptance: Edit on the Library bay opens the Sample editor seeded with the picked sample`` () =
        HeadlessSession.run (fun () ->
            let materials, samples = freshStores ()
            // A recording launcher that still opens the REAL step-022 editor window, so the
            // proof is end-to-end: verb click by UiIds → the real editor window is shown.
            let opened = ResizeArray<Window>()
            let launchers : EditorLaunchers =
                { EditorLaunchers.defaults with
                    openSampleEditor =
                        fun m s intent ->
                            let w = SampleEditorWindow(m, s, intent)
                            opened.Add w
                            w.Show() }
            let window = mountMain { mainWith materials samples with launchers = launchers }
            clickOn window (Ribbon.UiIds.tab BayNames.library)
            setText window SampleLibraryControls.UiIds.searchBox "n=1.75"
            clickOn window (SampleLibraryControls.UiIds.row (string SeedSamples.glassFilm600.id.value))
            clickOn window SampleLibraryControls.UiIds.editButton
            Dispatcher.UIThread.RunJobs()
            Assert.Equal(1, opened.Count)
            Assert.True(opened.[0].IsVisible, "the Sample editor window must be shown")
            Assert.Contains("Glass thin film", opened.[0].Title)
            opened.[0].Close()
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``headless acceptance: Make-multilayer opens a NEW editor seeded with a foldable 2-layer period and Save persists it`` () =
        HeadlessSession.run (fun () ->
            let materials, samples = freshStores ()
            let seededCount =
                match samples.listSamples () with
                | Ok all -> List.length all
                | Error e -> failwith $"seed listing failed: %A{e}"
            // A recording launcher that still opens the REAL step-022 editor, so the proof is
            // end-to-end: the Library bay's Make-multilayer verb click by UiId → the real editor,
            // seeded. The other launchers stay the real defaults (untriggered here).
            let opened = ResizeArray<Window>()
            let launchers : EditorLaunchers =
                { EditorLaunchers.defaults with
                    openSampleEditor =
                        fun m s intent ->
                            let w = SampleEditorWindow(m, s, intent)
                            opened.Add w
                            w.Show() }
            let window = mountMain { mainWith materials samples with launchers = launchers }
            clickOn window (Ribbon.UiIds.tab BayNames.library)
            clickOn window SampleLibraryControls.UiIds.makeMultilayerButton
            Dispatcher.UIThread.RunJobs()
            Assert.Equal(1, opened.Count)
            let editor = opened.[0]
            Assert.True(editor.IsVisible, "the Sample editor window must be shown")
            // Seeded, NOT blank: the foldable 2-layer period renders as one super-row + two cell rows.
            Assert.True(isPresent editor (SampleEditorView.UiIds.groupRow 0), "the seeded period super-row must render")
            Assert.True(isPresent editor (SampleEditorView.UiIds.cellLayerRow 0 0), "seeded cell layer 0 must render")
            Assert.True(isPresent editor (SampleEditorView.UiIds.cellLayerRow 0 1), "seeded cell layer 1 must render")
            // Name it and Save — a NEW sample persists through SampleProxy.addSample.
            setText editor SampleEditorView.UiIds.nameBox "Bay multilayer"
            clickOn editor SampleEditorView.UiIds.saveButton
            Assert.False(editor.IsVisible)
            match samples.listSamples () with
            | Ok all ->
                Assert.Equal(seededCount + 1, List.length all)
                Assert.Contains(all, fun (s : Sample) -> s.name = "Bay multilayer")
            | Error e -> Assert.Fail($"listSamples failed: %A{e}")
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``headless acceptance: removing a sample updates the Library list in the same render pass`` () =
        HeadlessSession.run (fun () ->
            let materials, samples = freshStores ()
            let window = mountMain (mainWith materials samples)
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
    let ``headless: the sample View panel renders the band view inside the panel`` () =
        HeadlessSession.run (fun () ->
            let materials, samples = freshStores ()
            let window = mountMain (mainWith materials samples)
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

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``headless acceptance: the Materials tab is gone, the strip button is on the ribbon row, and the Library bay stays full-surface`` () =
        HeadlessSession.run (fun () ->
            let materials, samples = freshStores ()
            let window = mountMain (mainWith materials samples)
            // Spec 0038 step 013: NO Materials ribbon tab any more — the tab-strip row carries
            // the right-aligned Materials… button instead (the constructor-side entry point).
            Assert.False(isPresent window (Ribbon.UiIds.tab "Materials"),
                         "the Materials bay tab must be gone from the ribbon")
            Assert.True(isPresent window WorkbenchIds.openMaterialsButton,
                        "the ribbon strip row must carry the right-aligned Materials… button")
            // The default bay (Rotation) is a table bay: the shared table canvas is realized below the strip.
            Assert.True(isPresent window UiIds.canvas, "a table bay keeps its table canvas below the ribbon strip")
            // The Library (samples) workbench is FULL-SURFACE: its list fills the area below the
            // strip and the table canvas is GONE (the full-surface bay replaces the canvas and
            // wires no table gestures).
            clickOn window (Ribbon.UiIds.tab BayNames.library)
            Assert.True(isPresent window SampleLibraryControls.UiIds.searchBox, "the Library workbench fills the surface below the strip")
            Assert.True(isPresent window (SampleLibraryControls.UiIds.row (string SeedSamples.glassFilm600.id.value)),
                        "the Library list is realized in the full-surface area")
            Assert.False(isPresent window UiIds.canvas, "a full-surface Library bay shows no table canvas")
            // Returning to a table bay restores the canvas (and its gestures) below the strip.
            clickOn window (Ribbon.UiIds.tab BayNames.rotation)
            Assert.True(isPresent window UiIds.canvas, "returning to a table bay restores the table canvas")
            window.Close())
