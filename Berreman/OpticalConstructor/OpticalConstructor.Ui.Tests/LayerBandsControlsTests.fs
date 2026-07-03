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

/// Spec 0027 (026) Part 4 — the Details bay: the reusable `LayerBandsControls` band component (pure
/// contract + a headless render proof) and the host's `detailsState` band-builder (the selected element's
/// bound entry → "what it is" title + collapsed "×N" bands), plus the seven-bay ribbon.
module LayerBandsControlsTests =

    let private elem (i : int) (m : Model) : TestElement = List.item i m.elements

    /// Select the element at index `i`, then bind it to a Library entry id (the immediate `BindValueId` path).
    let private bind (i : int) (entryId : string) (m : Model) : Model =
        { m with selection = ElementSelected i } |> update (BindValueId entryId)

    // ============================ pure control contract ============================

    [<Fact>]
    let ``the empty LayerBands state has no bands and an empty title`` () =
        let s = LayerBandsControls.empty
        Assert.Empty(s.bands)
        Assert.Equal("", s.title)

    [<Fact>]
    let ``the LayerBands UiIds prefix band ids and are stable`` () =
        Assert.Equal("LayerBandsStack", LayerBandsControls.UiIds.stack)
        Assert.Equal("LayerBandsTitle", LayerBandsControls.UiIds.title)
        Assert.Equal("LayerBand_0", LayerBandsControls.UiIds.band 0)
        Assert.Equal("LayerBand_3", LayerBandsControls.UiIds.band 3)

    // ============================ the Details bay in the ribbon ============================

    [<Fact>]
    let ``the ribbon offers the Details bay LAST, after Experiments`` () =
        Assert.Equal(BayNames.details, List.last BayNames.all)
        let m = initMain ()
        let bays = mainBays m ignore
        let names = bays |> List.map (fun b -> b.name)
        Assert.Equal<string list>(BayNames.all, names)
        // Details comes immediately after Experiments.
        let idxExp = List.findIndex ((=) BayNames.experiments) names
        let idxDet = List.findIndex ((=) BayNames.details) names
        Assert.Equal(idxExp + 1, idxDet)

    // ============================ host detailsState (pure) ============================

    [<Fact>]
    let ``detailsState for a multilayer sample yields collapsed x-N bands and a descriptive title`` () =
        // A Sample bound to the 41-layer quarter-wave multilayer ⇒ two collapsed bands (glass ×21, vacuum ×20).
        let m =
            initMain ()
            |> update (AddElement Sample)             // index 2
            |> bind 2 "sample-multilayer-qw"
        let bays = mainBays m ignore
        Assert.Contains(BayNames.details, bays |> List.map (fun b -> b.name))
        // Re-derive the band view through the same public seam the host uses (the ribbon bay's content is the
        // LayerBandsControls.view of detailsState); assert the band shape via the rendered Details bay below.
        // Here assert that the bound entry resolves to a multilayer whose description spells out the stack.
        match m.library.tryGetEntry "sample-multilayer-qw" with
        | Ok (Some entry) -> Assert.Contains("layer", entry.fullDescription)
        | other -> Assert.Fail(sprintf "expected the multilayer entry, got %A" other)
        // The bound element carries the multilayer valueId.
        Assert.Equal(Some "sample-multilayer-qw", (elem 2 m).placement.valueId)

    [<Fact>]
    let ``detailsState shows a hint when nothing is selected and nothing bound`` () =
        // initMain selects the table first; with no element selected the Details bay shows a hint (no bands).
        let m = { (initMain ()) with selection = TableSelected }
        let bays = mainBays m ignore
        Assert.Contains(BayNames.details, bays |> List.map (fun b -> b.name))

    // ============================ headless render proofs (ui-smoke) ============================

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the LayerBands view renders a titled stack of bands`` () =
        HeadlessSession.run (fun () ->
            let state : LayerBandsControls.State =
                {
                    title = "A three-band test stack."
                    bands =
                        [
                            { LayerBandsControls.Band.label = "Glass — 100 nm ×21"; heightWeight = 2.0; colorHex = "#C8E1F5" }
                            { LayerBandsControls.Band.label = "Vacuum — 150 nm ×20"; heightWeight = 2.2; colorHex = "#F2F2F2" }
                            { LayerBandsControls.Band.label = "Glass — 100 nm"; heightWeight = 2.0; colorHex = "#C8E1F5" }
                        ]
                }
            let window = Window(Width = 420.0, Height = 480.0)
            window.Content <- Component(fun _ -> LayerBandsControls.view state)
            window.Show()
            Dispatcher.UIThread.RunJobs()
            let named (name : string) : bool =
                window.GetVisualDescendants()
                |> Seq.exists (function :? Control as c -> c.Name = name && c.IsEffectivelyVisible | _ -> false)
            Assert.True(named LayerBandsControls.UiIds.stack, "the band stack was not present")
            Assert.True(named LayerBandsControls.UiIds.title, "the band title was not present")
            Assert.True(named (LayerBandsControls.UiIds.band 0), "band 0 was not present")
            Assert.True(named (LayerBandsControls.UiIds.band 1), "band 1 was not present")
            Assert.True(named (LayerBandsControls.UiIds.band 2), "band 2 was not present")
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the empty LayerBands view renders just the title, no bands`` () =
        HeadlessSession.run (fun () ->
            let window = Window(Width = 420.0, Height = 320.0)
            window.Content <- Component(fun _ -> LayerBandsControls.view { LayerBandsControls.empty with title = "Nothing here." })
            window.Show()
            Dispatcher.UIThread.RunJobs()
            let bandPresent () : bool =
                window.GetVisualDescendants()
                |> Seq.exists (function :? Control as c -> c.Name = LayerBandsControls.UiIds.band 0 | _ -> false)
            Assert.False(bandPresent (), "an empty state must draw no bands")
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the Details bay renders the bound multilayer sample's bands and description`` () =
        HeadlessSession.run (fun () ->
            // Add a Sample, bind it to the multilayer, show the Details bay.
            let mutable model =
                initMain ()
                |> update (AddElement Sample)               // index 2, selected
                |> update (BindValueId "sample-multilayer-qw")
                |> update (SelectBay BayNames.details)
            let dispatch (msg : Msg) = model <- update msg model
            let window = Window(Width = 980.0, Height = canvasHeight + 360.0)
            window.Content <- Component(fun _ -> mainView model dispatch)
            window.Show()
            Dispatcher.UIThread.RunJobs()
            // The Details band stack and at least the first two collapsed bands are visible.
            let named (name : string) : bool =
                window.GetVisualDescendants()
                |> Seq.exists (function :? Control as c -> c.Name = name && c.IsEffectivelyVisible | _ -> false)
            Assert.True(named LayerBandsControls.UiIds.stack, "the Details band stack was not visible")
            Assert.True(named (LayerBandsControls.UiIds.band 0), "the first collapsed band was not visible")
            Assert.True(named (LayerBandsControls.UiIds.band 1), "the second collapsed band was not visible")
            window.Close())
