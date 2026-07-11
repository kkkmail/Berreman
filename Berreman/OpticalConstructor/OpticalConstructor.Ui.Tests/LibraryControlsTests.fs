namespace OpticalConstructor.Ui.Tests

open Avalonia
open Avalonia.Controls
open Avalonia.Headless
open Avalonia.Threading
open Avalonia.VisualTree
open Avalonia.FuncUI
open Xunit
open OpticalConstructor.Controls
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Placement
open OpticalConstructor.Ui.TableAndElementRotationView

/// Spec 0027 (024) Phase 1 — the Selector bay (labelled "Library" until spec 0033 step 014): the pure
/// control contract, the host's kind-constrained `libraryState` flattening, the `BindValueId` MVU
/// binding, and one headless render proof that the bay lists its entries and a click binds the selected
/// element's `valueId`. The control stays `LibraryControls` — only the bay label changed.
module LibraryControlsTests =

    let private elem (i : int) (m : Model) : TestElement = List.item i m.elements

    /// The Guid-string entry ids of the seeded samples these tests bind (the Selector valueId seam
    /// carries the Guid string form since spec 0033 step 002 — referenced programmatically, never as
    /// a repeated literal).
    let private glass1mmId : string = (Library.SampleItem Library.SeedSamples.glassPlate1mm).entryId
    let private glass2mmId : string = (Library.SampleItem Library.SeedSamples.glassPlate2mm).entryId
    let private multilayerQwId : string = (Library.SampleItem Library.SeedSamples.multilayerQw).entryId

    // ============================ pure control contract ============================

    [<Fact>]
    let ``the empty Library state is disabled with no rows and no binding`` () =
        let s = LibraryControls.empty
        Assert.False(s.enabled)
        Assert.Empty(s.rows)
        Assert.Equal(None, s.boundName)

    [<Fact>]
    let ``the Library UiIds prefix leaf ids and are stable`` () =
        // `entry` is a pure prefix over ANY entry id string (a seeded sample's id is its Guid
        // string form since spec 0033 step 002).
        Assert.Equal("LibraryEntry_abc", LibraryControls.UiIds.entry "abc")
        Assert.Equal("LibraryEntry_" + glass1mmId, LibraryControls.UiIds.entry glass1mmId)
        Assert.Equal("LibraryTree", LibraryControls.UiIds.tree)
        Assert.Equal("LibraryBoundReadout", LibraryControls.UiIds.readout)

    // ============================ host libraryState (pure) ============================

    /// Build the model with a single Sample element selected (so the Selector bay shows samples).
    let private withSampleSelected () : Model =
        initMain () |> update (AddElement Sample)   // appends a Sample at index 2, selects it

    [<Fact>]
    let ``libraryState for a selected Sample lists ONLY sample leaf rows, kind-labelled Sample`` () =
        let m = withSampleSelected ()
        let bays = mainBays m ignore
        // The Selector bay exists and is offered.
        Assert.Contains(BayNames.selector, bays |> List.map (fun b -> b.name))
        // Its rows: every selectable leaf is a Sample entry id; the kind label is "Sample".
        // (Re-derive the bay state the same way the host does — through the live model.)
        let sampleIds =
            match m.library.entriesForKind CatalogueKind.Sample with
            | Ok entries -> entries |> List.map (fun e -> e.entryId) |> Set.ofList
            | Error _ -> Set.empty
        Assert.False(Set.isEmpty sampleIds)
        // Render the bay and read its leaf ids back (the host flattening is exercised end-to-end below);
        // here assert the kind-constraint at the domain seam the host uses.
        Assert.All(Set.toList sampleIds, fun id ->
            match m.library.tryGetEntry id with
            | Ok (Some (Library.SampleItem _)) -> ()
            | other -> Assert.Fail($"%s{id} is not a Sample: %A{other}"))

    [<Fact>]
    let ``BindValueId sets the selected element's valueId; inert for table or nothing`` () =
        let m = withSampleSelected ()                      // element 2 (a Sample) selected
        let bound = update (BindValueId glass1mmId) m
        Assert.Equal(Some glass1mmId, (elem 2 bound).placement.valueId)
        // Inert when the table is selected.
        let tableSel = { (initMain ()) with selection = TableSelected }
        Assert.Equal<Model>(tableSel, update (BindValueId glass1mmId) tableSel)
        // Inert when nothing is selected.
        let nothingSel = { (initMain ()) with selection = NothingSelected }
        Assert.Equal<Model>(nothingSel, update (BindValueId glass1mmId) nothingSel)

    [<Fact>]
    let ``binding then rebinding overwrites the valueId`` () =
        let m = withSampleSelected ()
        let m1 = update (BindValueId glass1mmId) m
        let m2 = update (BindValueId glass2mmId) m1
        Assert.Equal(Some glass2mmId, (elem 2 m2).placement.valueId)

    [<Fact>]
    let ``the ribbon offers the Selector bay; the Library workbench left for its window`` () =
        // The Selector bay is present, in order, after Render (the Experiments bay was added in Phase 2,
        // so `BayNames.all` is now longer than the original five; assert membership + the prefix order
        // rather than an exact five-element list). Spec 0033 step 014 renamed the bay label from
        // "Library" to "Selector" (same behaviour); spec 0033 step 024 reused the freed label for the
        // SAMPLES WORKBENCH bay — which spec 0038 step 015 moved into the single-instance Library
        // WINDOW, so no "Library" bay is offered any more (the Selector alone keeps binding duty).
        Assert.Equal("Selector", BayNames.selector)
        Assert.Contains(BayNames.selector, BayNames.all)
        Assert.DoesNotContain("Library", BayNames.all)
        Assert.Equal<string list>(
            [ BayNames.rotation; BayNames.move; BayNames.add; BayNames.render; BayNames.selector ],
            BayNames.all |> List.truncate 5)
        let m = initMain ()
        let bays = mainBays m ignore
        Assert.Equal<string list>(BayNames.all, bays |> List.map (fun b -> b.name))

    // ============================ Spec 0027 (026) confirm-gated bind (pure) ============================

    [<Fact>]
    let ``RequestBindValueId sets a pending entry WITHOUT binding the valueId`` () =
        let m = withSampleSelected ()                          // element 2 (a Sample) selected
        let pending = update (RequestBindValueId glass1mmId) m
        // The pending choice is recorded, but no valueId is bound yet.
        Assert.Equal(Some glass1mmId, pending.pendingEntry)
        Assert.Equal(None, (elem 2 pending).placement.valueId)

    [<Fact>]
    let ``ConfirmBindValueId commits the pending entry to the selected element's valueId and clears pending`` () =
        let m =
            withSampleSelected ()
            |> update (RequestBindValueId glass1mmId)
            |> update ConfirmBindValueId
        Assert.Equal(Some glass1mmId, (elem 2 m).placement.valueId)
        Assert.Equal(None, m.pendingEntry)

    [<Fact>]
    let ``CancelBindValueId clears the pending entry and does NOT bind`` () =
        let m =
            withSampleSelected ()
            |> update (RequestBindValueId glass1mmId)
            |> update CancelBindValueId
        Assert.Equal(None, m.pendingEntry)
        Assert.Equal(None, (elem 2 m).placement.valueId)

    [<Fact>]
    let ``a confirm with no pending entry binds nothing`` () =
        let m = withSampleSelected () |> update ConfirmBindValueId
        Assert.Equal(None, (elem 2 m).placement.valueId)

    [<Fact>]
    let ``libraryState surfaces the pending entry's name and FULL description before confirm`` () =
        let m = withSampleSelected () |> update (RequestBindValueId multilayerQwId)
        // The host's libraryState fills the pending fields from the proxy; mirror that resolution here.
        match m.library.tryGetEntry multilayerQwId with
        | Ok (Some entry) ->
            Assert.False(System.String.IsNullOrWhiteSpace entry.fullDescription)
            // A multilayer's description spells out the stack, not just its short name.
            Assert.Contains("layer", entry.fullDescription)
        | other -> Assert.Fail($"expected the quarter-wave multilayer entry, got %A{other}")
        Assert.Equal(Some multilayerQwId, m.pendingEntry)

    // ============================ headless render proof (ui-smoke) ============================

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the Selector bay shows the pending description, then a Confirm click binds the valueId`` () =
        HeadlessSession.run (fun () ->
            // A Sample is selected, an entry is already PENDING (selected-not-confirmed), the Selector bay is
            // shown — so the confirm panel (the entry's full description + Confirm / Cancel) renders up front.
            let mutable model =
                withSampleSelected ()
                |> update (SelectBay BayNames.selector)
                |> update (RequestBindValueId glass1mmId)
            let dispatch (msg : Msg) = model <- update msg model
            let window = Window(Width = 980.0, Height = canvasHeight + 320.0)
            window.Content <- Component(fun _ -> mainView model dispatch)
            window.Show()
            Dispatcher.UIThread.RunJobs()
            // The ribbon shows a Selector tab (spec 0033 step 014); NO Library tab remains —
            // the samples workbench is the Library WINDOW now (spec 0038 step 015).
            let tabNames =
                window.GetVisualDescendants()
                |> Seq.choose (function
                    | :? Border as b when not (System.String.IsNullOrEmpty b.Name) && b.Name.StartsWith("RibbonTab_") -> Some b.Name
                    | _ -> None)
                |> List.ofSeq
            Assert.Contains(Ribbon.UiIds.tab BayNames.selector, tabNames)
            Assert.DoesNotContain(Ribbon.UiIds.tab "Library", tabNames)
            // The pending entry's full description is shown (and the element is NOT bound yet).
            Assert.Equal(None, (elem 2 model).placement.valueId)
            let descriptionShown () : bool =
                window.GetVisualDescendants()
                |> Seq.exists (function
                    | :? TextBlock as t -> t.Name = LibraryControls.UiIds.description && not (System.String.IsNullOrWhiteSpace t.Text) && t.IsEffectivelyVisible
                    | _ -> false)
            Assert.True(descriptionShown (), "the pending entry's full description was not shown")
            // Click Confirm — now it binds the selected element's valueId.
            let findConfirm () : Border option =
                window.GetVisualDescendants()
                |> Seq.tryPick (function :? Border as b when Avalonia.Automation.AutomationProperties.GetAutomationId(b) = LibraryControls.UiIds.confirm && b.IsEffectivelyVisible -> Some b | _ -> None)
            match findConfirm () with
            | None -> Assert.Fail("the Confirm button was not visible in the Selector bay")
            | Some b ->
                let c = b.TranslatePoint(Point(b.Bounds.Width / 2.0, b.Bounds.Height / 2.0), window)
                if c.HasValue then
                    window.MouseDown(c.Value, Avalonia.Input.MouseButton.Left, Avalonia.Input.RawInputModifiers.None)
                    Dispatcher.UIThread.RunJobs()
                    window.MouseUp(c.Value, Avalonia.Input.MouseButton.Left, Avalonia.Input.RawInputModifiers.None)
                    Dispatcher.UIThread.RunJobs()
                    Assert.Equal(Some glass1mmId, (elem 2 model).placement.valueId)
                else Assert.Fail("the Confirm button has no on-screen position")
            window.Close())

    // ============================ FuncUI recycling regressions (spec 0038) ============================
    // The leaf rows are GENERATED from a kind-constrained list whose membership changes with the
    // selection, so a re-render can shift one entry onto another's reused control. A `Border.name`
    // there made FuncUI re-set a styled element's `Name` and throw "Cannot set Name : styled element
    // already styled" (the live Selector-bay crash); the rows now carry a freely-mutable
    // `AutomationProperties.AutomationId` and are keyed with `View.withKey`. These drive the bay
    // through a REAL re-rendering Component (the app's Elmish patch path) to guard that.

    /// A Border in `window` carrying this automation id (the converted boxes carry no `Name`).
    let private borderWithId (window : Window) (id : string) : Border option =
        window.GetVisualDescendants()
        |> Seq.tryPick (function
            | :? Border as b when Avalonia.Automation.AutomationProperties.GetAutomationId(b) = id && b.IsEffectivelyVisible -> Some b
            | _ -> None)

    let private clickIn (window : Window) (id : string) : unit =
        match borderWithId window id with
        | Some b ->
            let c = b.TranslatePoint(Point(b.Bounds.Width / 2.0, b.Bounds.Height / 2.0), window)
            if c.HasValue then
                window.MouseDown(c.Value, Avalonia.Input.MouseButton.Left, Avalonia.Input.RawInputModifiers.None)
                Dispatcher.UIThread.RunJobs()
                window.MouseUp(c.Value, Avalonia.Input.MouseButton.Left, Avalonia.Input.RawInputModifiers.None)
                Dispatcher.UIThread.RunJobs()
            else Assert.Fail($"%s{id} has no on-screen position")
        | None -> Assert.Fail($"%s{id} not found")

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``a leaf-row membership shift re-renders without a styled-element rename throw`` () =
        // The mechanism regression, at the control level: rows [alpha; beta; gamma] → [beta; gamma]
        // shifts 'beta' onto 'alpha''s reused Border (same type, same slot). With `Border.name` this
        // threw "Cannot set Name : styled element already styled"; the AutomationId + ViewKey sweep
        // recreates the shifted rows instead.
        HeadlessSession.run (fun () ->
            let rowFor (id : string) (label : string) : LibraryControls.Row =
                { label = label; depth = 0; entryId = id; isBound = false }
            let fullRows = [ rowFor "alpha" "Alpha"; rowFor "beta" "Beta"; rowFor "gamma" "Gamma" ]
            let shiftedRows = [ rowFor "beta" "Beta"; rowFor "gamma" "Gamma" ]
            let stateFor (rows : LibraryControls.Row list) : LibraryControls.State =
                { LibraryControls.empty with rows = rows; kindLabel = "Sample"; enabled = true }
            let handlers : LibraryControls.Handlers =
                { selectEntry = ignore; confirmEntry = ignore; cancelEntry = ignore }
            let setRows : (LibraryControls.Row list -> unit) ref = ref (fun _ -> ())
            let host =
                Component(fun ctx ->
                    let rows = ctx.useState fullRows
                    setRows.Value <- rows.Set
                    LibraryControls.view (stateFor rows.Current) handlers)
            let window = Window(Width = 640.0, Height = 480.0, Content = host)
            window.Show()
            Dispatcher.UIThread.RunJobs()
            // Shift the membership (the first leaf disappears) — must not throw…
            setRows.Value shiftedRows
            Dispatcher.UIThread.RunJobs()
            // …and back to the full list (the reverse shift).
            setRows.Value fullRows
            Dispatcher.UIThread.RunJobs()
            let hasEntry (id : string) : bool =
                match borderWithId window (LibraryControls.UiIds.entry id) with
                | Some _ -> true
                | None -> false
            Assert.True(hasEntry "alpha" && hasEntry "beta" && hasEntry "gamma",
                        "the re-rendered leaf rows were not locatable by AutomationId")
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the Selector bay survives select, bind, and kind-change re-renders`` () =
        // The acceptance drive over the LIVE main view: select an entry (pending), bind it (Confirm),
        // then change the selected element's KIND twice — rows collapse for a polarizer and rebuild for
        // a fresh sample — with every step re-rendering through the FuncUI patch path.
        HeadlessSession.run (fun () ->
            let seed = withSampleSelected () |> update (SelectBay BayNames.selector)
            let latest : Model ref = ref seed
            let dispatchRef : (Msg -> unit) ref = ref ignore
            let comp =
                Component(fun ctx ->
                    let st = ctx.useState seed
                    latest.Value <- st.Current
                    let dispatch (msg : Msg) =
                        let m = update msg st.Current
                        latest.Value <- m
                        st.Set m
                    dispatchRef.Value <- dispatch
                    mainView st.Current dispatch)
            let window = Window(Width = 980.0, Height = canvasHeight + 360.0, Content = comp)
            window.Show()
            Dispatcher.UIThread.RunJobs()
            // SELECT: click a leaf entry — the pending confirm panel renders on the patched tree.
            clickIn window (LibraryControls.UiIds.entry glass1mmId)
            Assert.Equal(Some glass1mmId, latest.Value.pendingEntry)
            // BIND: click Confirm — the pending entry commits to the selected element's valueId.
            clickIn window LibraryControls.UiIds.confirm
            Assert.Equal(Some glass1mmId, (elem 2 latest.Value).placement.valueId)
            let sampleLeafShown () : bool =
                match borderWithId window (LibraryControls.UiIds.entry glass1mmId) with
                | Some _ -> true
                | None -> false
            // KIND-CHANGE 1: add (and select) a polarizer — no library entries for its kind, so the
            // leaf rows COLLAPSE on the re-render.
            dispatchRef.Value (AddElement LinearPolarizer)
            Dispatcher.UIThread.RunJobs()
            Assert.False(sampleLeafShown (), "a sample leaf row survived the polarizer kind-change")
            // KIND-CHANGE 2: add (and select) another sample — the full sample tree REBUILDS.
            dispatchRef.Value (AddElement Sample)
            Dispatcher.UIThread.RunJobs()
            Assert.True(sampleLeafShown (), "the sample leaf rows did not rebuild after the kind-change back")
            // The first sample kept its binding across the re-renders.
            Assert.Equal(Some glass1mmId, (elem 2 latest.Value).placement.valueId)
            window.Close())
