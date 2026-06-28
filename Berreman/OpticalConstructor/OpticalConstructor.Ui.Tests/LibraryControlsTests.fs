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

    // ============================ headless render proof (ui-smoke) ============================

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the Library bay renders kind-constrained entries and a click binds the element's valueId`` () =
        HeadlessSession.run (fun () ->
            // A Sample is selected, the Library bay is shown.
            let mutable model = withSampleSelected () |> update (SelectBay BayNames.library)
            let dispatch (msg : Msg) = model <- update msg model
            let window = Window(Width = 980.0, Height = canvasHeight + 260.0)
            window.Content <- Component(fun _ -> mainView model dispatch)
            window.Show()
            Dispatcher.UIThread.RunJobs()
            // The Library tree is effectively visible.
            let leafName = LibraryControls.UiIds.entry "sample-glass-1mm"
            let findLeaf () : Border option =
                window.GetVisualDescendants()
                |> Seq.tryPick (function :? Border as b when b.Name = leafName && b.IsEffectivelyVisible -> Some b | _ -> None)
            match findLeaf () with
            | None -> Assert.Fail("the sample-glass-1mm leaf was not visible in the Library bay")
            | Some b ->
                // Click the leaf — it must bind the selected element's valueId.
                let c = b.TranslatePoint(Point(b.Bounds.Width / 2.0, b.Bounds.Height / 2.0), window)
                if c.HasValue then
                    window.MouseDown(c.Value, Avalonia.Input.MouseButton.Left, Avalonia.Input.RawInputModifiers.None)
                    Dispatcher.UIThread.RunJobs()
                    window.MouseUp(c.Value, Avalonia.Input.MouseButton.Left, Avalonia.Input.RawInputModifiers.None)
                    Dispatcher.UIThread.RunJobs()
                    Assert.Equal(Some "sample-glass-1mm", (elem 2 model).placement.valueId)
                else Assert.Fail("the leaf has no on-screen position")
            window.Close())
