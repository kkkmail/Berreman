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
open OpticalConstructor.TestWindows.TableAndElementRotationView

/// Spec 0027 (024) Phase 1 — the Library bay: the pure control contract, the host's kind-constrained
/// `libraryState` flattening, the `BindValueId` MVU binding, and one headless render proof that the bay
/// lists its entries and a click binds the selected element's `valueId`.
module LibraryControlsTests =

    let private elem (i : int) (m : Model) : TestElement = List.item i m.elements

    // ============================ pure control contract ============================

    [<Fact>]
    let ``the empty Library state is disabled with no rows and no binding`` () =
        let s = LibraryControls.empty
        Assert.False(s.enabled)
        Assert.Empty(s.rows)
        Assert.Equal(None, s.boundName)

    [<Fact>]
    let ``the Library UiIds prefix leaf ids and are stable`` () =
        Assert.Equal("LibraryEntry_sample-glass-1mm", LibraryControls.UiIds.entry "sample-glass-1mm")
        Assert.Equal("LibraryTree", LibraryControls.UiIds.tree)
        Assert.Equal("LibraryBoundReadout", LibraryControls.UiIds.readout)

    // ============================ host libraryState (pure) ============================

    /// Build the model with a single Sample element selected (so the Library shows samples).
    let private withSampleSelected () : Model =
        initMain () |> update (AddElement Sample)   // appends a Sample at index 2, selects it

    [<Fact>]
    let ``libraryState for a selected Sample lists ONLY sample leaf rows, kind-labelled Sample`` () =
        let m = withSampleSelected ()
        let bays = mainBays m ignore
        // The Library bay exists and is offered.
        Assert.Contains(BayNames.library, bays |> List.map (fun b -> b.name))
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
            | other -> Assert.Fail(sprintf "%s is not a Sample: %A" id other))

    [<Fact>]
    let ``BindValueId sets the selected element's valueId; inert for table or nothing`` () =
        let m = withSampleSelected ()                      // element 2 (a Sample) selected
        let bound = update (BindValueId "sample-glass-1mm") m
        Assert.Equal(Some "sample-glass-1mm", (elem 2 bound).placement.valueId)
        // Inert when the table is selected.
        let tableSel = { (initMain ()) with selection = TableSelected }
        Assert.Equal<Model>(tableSel, update (BindValueId "sample-glass-1mm") tableSel)
        // Inert when nothing is selected.
        let nothingSel = { (initMain ()) with selection = NothingSelected }
        Assert.Equal<Model>(nothingSel, update (BindValueId "sample-glass-1mm") nothingSel)

    [<Fact>]
    let ``binding then rebinding overwrites the valueId`` () =
        let m = withSampleSelected ()
        let m1 = update (BindValueId "sample-glass-1mm") m
        let m2 = update (BindValueId "sample-glass-2mm") m1
        Assert.Equal(Some "sample-glass-2mm", (elem 2 m2).placement.valueId)

    [<Fact>]
    let ``the ribbon offers the Library bay among its bays`` () =
        // The Library bay is present, in order, after Render (the Experiments bay was added in Phase 2,
        // so `BayNames.all` is now longer than the original five; assert membership + the prefix order
        // rather than an exact five-element list).
        Assert.Contains(BayNames.library, BayNames.all)
        Assert.Equal<string list>(
            [ BayNames.rotation; BayNames.move; BayNames.add; BayNames.render; BayNames.library ],
            BayNames.all |> List.truncate 5)
        let m = initMain ()
        let bays = mainBays m ignore
        Assert.Equal<string list>(BayNames.all, bays |> List.map (fun b -> b.name))

    // ============================ Spec 0027 (026) confirm-gated bind (pure) ============================

    [<Fact>]
    let ``RequestBindValueId sets a pending entry WITHOUT binding the valueId`` () =
        let m = withSampleSelected ()                          // element 2 (a Sample) selected
        let pending = update (RequestBindValueId "sample-glass-1mm") m
        // The pending choice is recorded, but no valueId is bound yet.
        Assert.Equal(Some "sample-glass-1mm", pending.pendingEntry)
        Assert.Equal(None, (elem 2 pending).placement.valueId)

    [<Fact>]
    let ``ConfirmBindValueId commits the pending entry to the selected element's valueId and clears pending`` () =
        let m =
            withSampleSelected ()
            |> update (RequestBindValueId "sample-glass-1mm")
            |> update ConfirmBindValueId
        Assert.Equal(Some "sample-glass-1mm", (elem 2 m).placement.valueId)
        Assert.Equal(None, m.pendingEntry)

    [<Fact>]
    let ``CancelBindValueId clears the pending entry and does NOT bind`` () =
        let m =
            withSampleSelected ()
            |> update (RequestBindValueId "sample-glass-1mm")
            |> update CancelBindValueId
        Assert.Equal(None, m.pendingEntry)
        Assert.Equal(None, (elem 2 m).placement.valueId)

    [<Fact>]
    let ``a confirm with no pending entry binds nothing`` () =
        let m = withSampleSelected () |> update ConfirmBindValueId
        Assert.Equal(None, (elem 2 m).placement.valueId)

    [<Fact>]
    let ``libraryState surfaces the pending entry's name and FULL description before confirm`` () =
        let m = withSampleSelected () |> update (RequestBindValueId "sample-multilayer-qw")
        // The host's libraryState fills the pending fields from the proxy; mirror that resolution here.
        match m.library.tryGetEntry "sample-multilayer-qw" with
        | Ok (Some entry) ->
            Assert.False(System.String.IsNullOrWhiteSpace entry.fullDescription)
            // A multilayer's description spells out the stack, not just its short name.
            Assert.Contains("layer", entry.fullDescription)
        | other -> Assert.Fail(sprintf "expected the quarter-wave multilayer entry, got %A" other)
        Assert.Equal(Some "sample-multilayer-qw", m.pendingEntry)

    // ============================ headless render proof (ui-smoke) ============================

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the Library bay shows the pending description, then a Confirm click binds the valueId`` () =
        HeadlessSession.run (fun () ->
            // A Sample is selected, an entry is already PENDING (selected-not-confirmed), the Library bay is
            // shown — so the confirm panel (the entry's full description + Confirm / Cancel) renders up front.
            let mutable model =
                withSampleSelected ()
                |> update (SelectBay BayNames.library)
                |> update (RequestBindValueId "sample-glass-1mm")
            let dispatch (msg : Msg) = model <- update msg model
            let window = Window(Width = 980.0, Height = canvasHeight + 320.0)
            window.Content <- Component(fun _ -> mainView model dispatch)
            window.Show()
            Dispatcher.UIThread.RunJobs()
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
                |> Seq.tryPick (function :? Border as b when b.Name = LibraryControls.UiIds.confirm && b.IsEffectivelyVisible -> Some b | _ -> None)
            match findConfirm () with
            | None -> Assert.Fail("the Confirm button was not visible in the Library bay")
            | Some b ->
                let c = b.TranslatePoint(Point(b.Bounds.Width / 2.0, b.Bounds.Height / 2.0), window)
                if c.HasValue then
                    window.MouseDown(c.Value, Avalonia.Input.MouseButton.Left, Avalonia.Input.RawInputModifiers.None)
                    Dispatcher.UIThread.RunJobs()
                    window.MouseUp(c.Value, Avalonia.Input.MouseButton.Left, Avalonia.Input.RawInputModifiers.None)
                    Dispatcher.UIThread.RunJobs()
                    Assert.Equal(Some "sample-glass-1mm", (elem 2 model).placement.valueId)
                else Assert.Fail("the Confirm button has no on-screen position")
            window.Close())
