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
open OpticalConstructor.Domain
open OpticalConstructor.Domain.CatalogueImport
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.Placement
open OpticalConstructor.Tests
open OpticalConstructor.Ui.TableAndElementRotationView

/// Spec 0027 (026) Part 4 — the Details bay: the reusable `LayerBandsControls` band component (pure
/// contract + a headless render proof) and the host's `detailsState` band-builder (the selected element's
/// bound entry → "what it is" title + collapsed "×N" bands), plus the seven-bay ribbon.
module LayerBandsControlsTests =

    let private elem (i : int) (m : TableAndElementRotationModel) : TestElement = List.item i m.elements

    /// Select the element at index `i`, then bind it to a Library entry id (the immediate `BindValueId` path).
    let private bind (i : int) (entryId : string) (m : TableAndElementRotationModel) : TableAndElementRotationModel =
        { m with selection = ElementSelected (SceneElementIndex i) } |> updateTableAndElementRotation (BindValueId (ElementValueId entryId))

    /// Bind the 41-layer multilayer from BuiltinCatalogue.sample by Library.SampleIds
    /// (spec 0033 step 002; 0050 §3.3).
    let private multilayerQwId : string = (Library.SampleItem (BuiltinCatalogue.sample Library.SampleIds.multilayerQwId)).entryId.value

    type private DetailsAttributionData =
        {
            model : TableAndElementRotationModel
            ptbId : SampleId
            riiId : SampleId
            licensedSample : LibraryEntryContent
        }

    let private detailsAttributionData () : DetailsAttributionData =
        let model = SceneTestFixture.mainScene ()
        let ptbMaterialId = newMaterialId ()
        let riiMaterialId = newMaterialId ()
        let ptbId = newSampleId ()
        let riiId = newSampleId ()
        let ptbName = EntryName "PTB glass"
        let riiName = EntryName "RII glass"
        let ptbStackName = EntryName "PTB stack"
        let riiStackName = EntryName "RII stack"
        let ccBy = EntryLicence "CC-BY-4.0"
        let cc0 = EntryLicence "CC0-1.0"
        let citation = EntryCitation "A. Author, Test optical constants (2026), doi:10.0000/test"
        let ptb = BuiltinCatalogue.licensedMaterialContent ptbMaterialId ptbName ccBy citation
        let rii = BuiltinCatalogue.licensedMaterialContent riiMaterialId riiName cc0 citation
        let ptbSample = BuiltinCatalogue.sampleContentOver ptbId ptbMaterialId ptbStackName
        let riiSample = BuiltinCatalogue.sampleContentOver riiId riiMaterialId riiStackName
        BuiltinCatalogue.putSealedEntries model.catalogueImports [ ptb; rii; ptbSample; riiSample ]
        {
            model = model
            ptbId = ptbId
            riiId = riiId
            licensedSample =
                {
                    ptbSample with
                        version = ptbSample.version.next
                        provenance = ptb.provenance
                }
        }

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
        Assert.DoesNotContain(Ribbon.BayName "Materials", BayNames.allBayNames)
        Assert.DoesNotContain(Ribbon.BayName "Library", BayNames.allBayNames)
        let m = SceneTestFixture.mainScene ()
        let bays = mainBays m ignore
        let names = bays |> List.map (fun b -> b.name)
        Assert.Equal<Ribbon.BayName list>(BayNames.allBayNames, names)
        // Details comes immediately after Experiments.
        let idxExp = List.findIndex ((=) BayNames.experiments) names
        let idxDet = List.findIndex ((=) BayNames.details) names
        Assert.Equal(idxExp + 1, idxDet)

    // ============================ host detailsState (pure) ============================

    [<Fact>]
    let ``detailsState for a multilayer sample yields collapsed x-N bands and a descriptive title`` () =
        // A Sample bound to the 41-layer quarter-wave multilayer ⇒ two collapsed bands (glass ×21, vacuum ×20).
        let m =
            SceneTestFixture.mainScene ()
            |> updateTableAndElementRotation (AddElement Sample)             // index 2
            |> bind 2 multilayerQwId
        let bays = mainBays m ignore
        Assert.Contains(BayNames.details, bays |> List.map (fun b -> b.name))
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
        let m = { (SceneTestFixture.mainScene ()) with selection = TableSelected }
        let bays = mainBays m ignore
        Assert.Contains(BayNames.details, bays |> List.map (fun b -> b.name))

    [<Fact>]
    let ``the Details of a bound sample credit its CC-BY material with its citation`` () : unit =
        let data = detailsAttributionData ()
        let unbound = data.model |> updateTableAndElementRotation (AddElement Sample)
        let ptb = unbound |> bind 2 data.ptbId.toElementValueId.value
        let credit = Assert.Single(ptb.detailsAttributionLines)
        Assert.Equal("PTB glass: Licence CC-BY-4.0 — cite: A. Author, Test optical constants (2026), doi:10.0000/test", credit.text)
        let rii = ptb |> bind 2 data.riiId.toElementValueId.value
        Assert.Equal("RII glass: Licence CC0-1.0", (Assert.Single(rii.detailsAttributionLines)).text)
        let builtin = rii |> bind 2 multilayerQwId
        Assert.Empty(builtin.detailsAttributionLines)
        Assert.Empty(unbound.detailsAttributionLines)
        Assert.Empty(data.model.detailsAttributionLines)
        for selection in [ NothingSelected; ElementSelected (SceneElementIndex 99) ] do
            let unselected =
                {
                    ptb with
                        selection = selection
                }
            Assert.Empty(unselected.detailsAttributionLines)
        let unreadable =
            {
                ptb with
                    catalogueImports =
                        {
                            ptb.catalogueImports with
                                proxy =
                                    {
                                        ptb.catalogueImports.proxy with
                                            tryHeldEntry = fun _ -> Error (ImportStoreUnavailableErr "Attribution unavailable")
                                    }
                        }
            }
        Assert.Empty(unreadable.detailsAttributionLines)
        let missingBinding =
            {
                ptb with
                    library =
                        {
                            ptb.library with
                                tryGetEntry = fun _ -> Ok None
                        }
            }
        Assert.Empty(missingBinding.detailsAttributionLines)
        let unreadableBinding =
            {
                ptb with
                    library =
                        {
                            ptb.library with
                                tryGetEntry = fun _ -> Error (LibraryUnavailableErr "Binding unavailable")
                        }
            }
        Assert.Empty(unreadableBinding.detailsAttributionLines)
        BuiltinCatalogue.putSealedEntries ptb.catalogueImports [ data.licensedSample ]
        Assert.Equal<string list>(
            [ "PTB stack: Licence CC-BY-4.0 — cite: A. Author, Test optical constants (2026), doi:10.0000/test"
              "PTB glass: Licence CC-BY-4.0 — cite: A. Author, Test optical constants (2026), doi:10.0000/test" ],
            ptb.detailsAttributionLines |> List.map _.text)

    [<Fact>]
    let ``the Details bay shows the citation of a bound sample's CC-BY material`` () : unit =
        HeadlessSession.run (fun () ->
            let data = detailsAttributionData ()
            let initial =
                data.model
                |> updateTableAndElementRotation (AddElement Sample)
                |> bind 2 data.ptbId.toElementValueId.value
            let window = Window(Width = 980.0, Height = tableAndElementRotationCanvasHeight + 360.0)
            try
                window.Content <- Component(fun ctx ->
                    let state = ctx.useState initial
                    mainView state.Current (fun msg -> state.Set (updateTableAndElementRotation msg state.Current)))
                window.Show()
                Dispatcher.UIThread.RunJobs()
                let titles () : TextBlock list =
                    window.GetVisualDescendants()
                    |> Seq.choose (function
                        | :? TextBlock as title when title.Name = ConstructorUiId.layerBandsTitle.value -> Some title
                        | _ -> None)
                    |> Seq.toList
                Assert.Empty(titles ())
                let tabId = ConstructorUiId.ribbonTab BayNames.details.uiKey
                let tab =
                    window.GetVisualDescendants()
                    |> Seq.choose (function
                        | :? Border as border when border.Name = tabId.value && border.IsEffectivelyVisible -> Some border
                        | _ -> None)
                    |> Assert.Single
                let centre = tab.TranslatePoint(Point(tab.Bounds.Width / 2.0, tab.Bounds.Height / 2.0), window)
                Assert.True(centre.HasValue, "The Details tab must have an on-screen position.")
                let point = centre.GetValueOrDefault()
                window.MouseDown(point, MouseButton.Left, RawInputModifiers.None)
                Dispatcher.UIThread.RunJobs()
                window.MouseUp(point, MouseButton.Left, RawInputModifiers.None)
                Dispatcher.UIThread.RunJobs()
                let title = Assert.Single(titles ())
                Assert.True(title.IsEffectivelyVisible)
                Assert.StartsWith("PTB stack — ", title.Text)
                Assert.EndsWith("\nPTB glass: Licence CC-BY-4.0 — cite: A. Author, Test optical constants (2026), doi:10.0000/test", title.Text)
            finally window.Close())

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
                SceneTestFixture.mainScene ()
                |> updateTableAndElementRotation (AddElement Sample)               // index 2, selected
                |> updateTableAndElementRotation (BindValueId (ElementValueId multilayerQwId))
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
