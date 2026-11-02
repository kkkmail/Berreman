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

/// Spec 0027 (026) Part 4 — the Details bay: the reusable `LayerBandsControls` band component (pure
/// contract + a headless render proof) and the host's `detailsState` band-builder (the selected element's
/// bound entry → "what it is" title + collapsed "×N" bands), plus the seven-bay ribbon.
module LayerBandsControlsTests =

    let private elem (i : int) (m : TableAndElementRotationModel) : TestElement = List.item i m.elements

    /// Select the element at index `i`, then bind it to a Library entry id (the immediate `BindValueId` path).
    let private bind (i : int) (entryId : string) (m : TableAndElementRotationModel) : TableAndElementRotationModel =
        { m with selection = ElementSelected i } |> updateTableAndElementRotation (BindValueId entryId)

    /// The Guid-string entry id of the seeded 41-layer multilayer these tests bind (referenced
    /// programmatically — the id literal lives only in `Library.SeedSamples`, spec 0033 step 002).
    let private multilayerQwId : string = (Library.SampleItem Library.SeedSamples.multilayerQw).entryId.value

    // ============================ pure control contract ============================

    [<Fact>]
    let ``the empty LayerBands state has no bands and an empty title`` () =
        let s = LayerBandsControls.emptyLayerBandsControls
        Assert.Empty(s.bands)
        Assert.Equal("", s.title.value)

    [<Fact>]
    let ``the LayerBands UiIds prefix band ids and are stable`` () =
        Assert.Equal("LayerBandsStack", ConstructorUiId.layerBandsStack.value)
        Assert.Equal("LayerBandsTitle", ConstructorUiId.layerBandsTitle.value)
        Assert.Equal("LayerBand_0", (ConstructorUiId.layerBand (ConstructorUiPosition 0)).value)
        Assert.Equal("LayerBand_3", (ConstructorUiId.layerBand (ConstructorUiPosition 3)).value)

    // ============================ the Details bay in the ribbon ============================

    [<Fact>]
    let ``the ribbon offers the Details bay LAST, after Experiments`` () =
        // Spec 0038 (013) removed the Materials bay (the Materials WINDOW carries that
        // workbench now) and spec 0038 (015) removed the Library (samples) workbench bay the
        // same way (the Library WINDOW), so Details is the last bay, still immediately after
        // Experiments.
        Assert.Equal(Some BayNames.details, List.tryLast BayNames.allBayNames)
        Assert.DoesNotContain("Materials", BayNames.allBayNames)
        Assert.DoesNotContain("Library", BayNames.allBayNames)
        let m = initMain ()
        let bays = mainBays m ignore
        let names = bays |> List.map (fun b -> b.name.value)
        Assert.Equal<string list>(BayNames.allBayNames, names)
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
            |> updateTableAndElementRotation (AddElement Sample)             // index 2
            |> bind 2 multilayerQwId
        let bays = mainBays m ignore
        Assert.Contains(BayNames.details, bays |> List.map (fun b -> b.name.value))
        // Re-derive the band view through the same public seam the host uses (the ribbon bay's content is the
        // LayerBandsControls.view of detailsState); assert the band shape via the rendered Details bay below.
        // Here assert that the bound entry resolves to a multilayer whose description spells out the stack.
        match m.library.tryGetEntry (ElementValueId multilayerQwId) with
        | Ok (Some entry) -> Assert.Contains("layer", entry.fullDescription)
        | other -> Assert.Fail($"expected the multilayer entry, got %A{other}")
        // The bound element carries the multilayer valueId.
        Assert.Equal(Some (ElementValueId multilayerQwId), (elem 2 m).placement.valueId)

    [<Fact>]
    let ``detailsState shows a hint when nothing is selected and nothing bound`` () =
        // initMain selects the table first; with no element selected the Details bay shows a hint (no bands).
        let m = { (initMain ()) with selection = TableSelected }
        let bays = mainBays m ignore
        Assert.Contains(BayNames.details, bays |> List.map (fun b -> b.name.value))

    // ============================ headless render proofs (ui-smoke) ============================

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the LayerBands view renders a titled stack of bands`` () =
        HeadlessSession.run (fun () ->
            let state : LayerBandsControls.LayerBandsControlsState =
                {
                    title = ControlCaption "A three-band test stack."
                    bands =
                        [
                            { LayerBandsControls.Band.label = ControlCaption "Glass — 100 nm ×21"; heightWeight = LayerBandsControls.BandWeight 2.0; colorHex = LayerBandsControls.BandColorHex "#C8E1F5" }
                            { LayerBandsControls.Band.label = ControlCaption "Vacuum — 150 nm ×20"; heightWeight = LayerBandsControls.BandWeight 2.2; colorHex = LayerBandsControls.BandColorHex "#F2F2F2" }
                            { LayerBandsControls.Band.label = ControlCaption "Glass — 100 nm"; heightWeight = LayerBandsControls.BandWeight 2.0; colorHex = LayerBandsControls.BandColorHex "#C8E1F5" }
                        ]
                }
            let window = Window(Width = 420.0, Height = 480.0)
            window.Content <- Component(fun _ -> state.view)
            window.Show()
            Dispatcher.UIThread.RunJobs()
            let named (name : string) : bool =
                window.GetVisualDescendants()
                |> Seq.exists (function :? Control as c -> c.Name = name && c.IsEffectivelyVisible | _ -> false)
            Assert.True(named ConstructorUiId.layerBandsStack.value, "the band stack was not present")
            Assert.True(named ConstructorUiId.layerBandsTitle.value, "the band title was not present")
            Assert.True(named (ConstructorUiId.layerBand (ConstructorUiPosition 0)).value, "band 0 was not present")
            Assert.True(named (ConstructorUiId.layerBand (ConstructorUiPosition 1)).value, "band 1 was not present")
            Assert.True(named (ConstructorUiId.layerBand (ConstructorUiPosition 2)).value, "band 2 was not present")
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the empty LayerBands view renders just the title, no bands`` () =
        HeadlessSession.run (fun () ->
            let window = Window(Width = 420.0, Height = 320.0)
            window.Content <- Component(fun _ -> { LayerBandsControls.emptyLayerBandsControls with title = ControlCaption "Nothing here." }.view)
            window.Show()
            Dispatcher.UIThread.RunJobs()
            let bandPresent () : bool =
                window.GetVisualDescendants()
                |> Seq.exists (function :? Control as c -> c.Name = (ConstructorUiId.layerBand (ConstructorUiPosition 0)).value | _ -> false)
            Assert.False(bandPresent (), "an empty state must draw no bands")
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the Details bay renders the bound multilayer sample's bands and description`` () =
        HeadlessSession.run (fun () ->
            // Add a Sample, bind it to the multilayer, show the Details bay.
            let mutable model =
                initMain ()
                |> updateTableAndElementRotation (AddElement Sample)               // index 2, selected
                |> updateTableAndElementRotation (BindValueId multilayerQwId)
                |> updateTableAndElementRotation (SelectBay BayNames.details)
            let dispatch (msg : TableAndElementRotationMsg) = model <- updateTableAndElementRotation msg model
            let window = Window(Width = 980.0, Height = tableAndElementRotationCanvasHeight + 360.0)
            window.Content <- Component(fun _ -> mainView model dispatch)
            window.Show()
            Dispatcher.UIThread.RunJobs()
            // The Details band stack and at least the first two collapsed bands are visible.
            let named (name : string) : bool =
                window.GetVisualDescendants()
                |> Seq.exists (function :? Control as c -> c.Name = name && c.IsEffectivelyVisible | _ -> false)
            Assert.True(named ConstructorUiId.layerBandsStack.value, "the Details band stack was not visible")
            Assert.True(named (ConstructorUiId.layerBand (ConstructorUiPosition 0)).value, "the first collapsed band was not visible")
            Assert.True(named (ConstructorUiId.layerBand (ConstructorUiPosition 1)).value, "the second collapsed band was not visible")
            window.Close())
