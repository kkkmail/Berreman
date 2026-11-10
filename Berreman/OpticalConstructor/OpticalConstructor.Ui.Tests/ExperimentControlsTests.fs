namespace OpticalConstructor.Ui.Tests

open Avalonia
open Avalonia.Controls
open Avalonia.Controls.Shapes
open Avalonia.Headless
open Avalonia.Threading
open Avalonia.VisualTree
open Avalonia.FuncUI
open Xunit
open Berreman.Constants
open Berreman.Geometry
open Berreman.Fields
open OpticalConstructor.Controls
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Placement
open OpticalConstructor.Domain.ItemModels
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.ItemEvaluation
open OpticalConstructor.Tests
open OpticalConstructor.Ui
open OpticalConstructor.Ui.TableAndElementRotationView

/// Spec 0027 (028) — the redesigned Experiments bay: the multi-step, editable experiment builder (choose
/// element → element-constrained variable → T/R/both capture → range → Add) and the persistent collection
/// (edit / remove). Covers the pure control contract, the host's `experimentState` / `experimentResult`
/// branches, the MVU bindings, and a few headless render proofs.
module ExperimentControlsTests =

    /// A control matches `id` by its `Name` OR its `AutomationProperties.AutomationId` (candidate / collection
    /// items carry an AutomationId — a mutable attached property — so they survive reordering; see the bay).
    let private matchesId (id : string) (c : Control) : bool =
        c.Name = id || Avalonia.Automation.AutomationProperties.GetAutomationId(c) = id

    let private elem (i : int) (m : TableAndElementRotationModel) : Setups.SetupSlot = List.item i m.elements

    /// Select the element at index `i`, then bind it to a Library entry id (the `BindValueId` MVU path).
    let private bind (i : int) (entryId : ElementValueId) (m : TableAndElementRotationModel) : TableAndElementRotationModel =
        { m with selection = ElementSelected (SceneElementIndex i) } |> updateTableAndElementRotation (BindValueId entryId)

    /// Spec 0046: the Main scene with its source bound to a TUNABLE source (xenon lamp + monochromator,
    /// 190–1000 nm) — only a tunable source's wavelength can be varied.
    let private withTunableSource () : TableAndElementRotationModel = bind 0 Library.PresetIds.xenonMonochromator (SceneTestFixture.mainScene ())

    /// Bind the 200 nm glass film from BuiltinCatalogue.sample by Library.SampleIds (spec 0050 §3.3).
    let private glassFilm200Id : ElementValueId = (Library.SampleItem (BuiltinCatalogue.sample Library.SampleIds.glassFilm200Id)).entryId

    /// BuiltinCatalogue.sample supplies ThinFilm and Plate samples supporting BOTH branches (spec 0046),
    /// and the EUV MIRROR (`EmitReflectedOnly`): the geometries bounding placed emission (spec 0040 D.3).
    let private glassFilm600Id : ElementValueId = (Library.SampleItem (BuiltinCatalogue.sample Library.SampleIds.glassFilm600Id)).entryId
    let private glassPlate1mmId : ElementValueId = (Library.SampleItem (BuiltinCatalogue.sample Library.SampleIds.glassPlate1mmId)).entryId
    let private euvMirrorId : ElementValueId = (Library.SampleItem (BuiltinCatalogue.sample Library.SampleIds.euvMoSiId)).entryId

    /// The live id of the element at index `i`.
    let private idOf (i : int) (m : TableAndElementRotationModel) : string = (elem i m).id.value

    // ============================ pure control contract ============================

    [<Fact>]
    let ``the empty Experiments state is disabled with no candidates and no choice`` () =
        let s = ExperimentControls.emptyExperimentControls
        Assert.Equal(ControlWithheld, s.availability)
        Assert.Empty(s.candidates)
        Assert.Equal(None, s.chosenId)
        Assert.Equal(ControlWithheld, s.addAvailability)
        Assert.Empty(s.collection)

    [<Fact>]
    let ``the Experiments UiIds prefix ids and are stable`` () =
        Assert.Equal("ExperimentCandidate_src", (ConstructorUiId.experimentCandidate (ConstructorUiKey "src")).value)
        Assert.Equal("ExperimentVariable_r1", (ConstructorUiId.experimentVariable (ConstructorUiKey "r1")).value)
        Assert.Equal("ExperimentMeasurement_both", (ConstructorUiId.experimentMeasurement (ConstructorUiKey "both")).value)
        Assert.Equal("ExperimentReadout", ConstructorUiId.experimentReadout.value)
        Assert.Equal("ExperimentAddButton", ConstructorUiId.experimentAddButton.value)
        Assert.Equal("ExperimentEdit_3", (ConstructorUiId.experimentEditButton (ConstructorUiKey "3")).value)
        Assert.Equal("ExperimentRemove_3", (ConstructorUiId.experimentRemoveButton (ConstructorUiKey "3")).value)

    [<Fact>]
    let ``the variable / measurement codes and labels are the mirror mapping`` () =
        Assert.Equal("wavelength", ExperimentControls.VaryWaveLength.uiKey.value)
        Assert.Equal("r1", ExperimentControls.VaryR1.uiKey.value)
        Assert.Equal("r2", ExperimentControls.VaryR2.uiKey.value)
        Assert.Equal("t", ExperimentControls.CaptureT.uiKey.value)
        Assert.Equal("r", ExperimentControls.CaptureR.uiKey.value)
        Assert.Equal("both", ExperimentControls.CaptureBoth.uiKey.value)

    [<Fact>]
    let ``the ribbon offers every bay including Experiments and Details`` () =
        // Spec 0038 (013): the Materials bay left the ribbon (the Materials WINDOW carries that
        // workbench now); spec 0038 (015): the Library (samples) workbench bay left the same
        // way (the Library WINDOW), so seven in-pane bays remain, Details last. Spec 0046: Add first.
        Assert.Equal<Ribbon.BayName list>(
            [ BayNames.add; BayNames.rotation; BayNames.move; BayNames.render; BayNames.selector
              BayNames.experiments; BayNames.details ],
            BayNames.allBayNames)
        let m = SceneTestFixture.mainScene ()
        let bays = mainBays m ignore
        Assert.Equal<Ribbon.BayName list>(BayNames.allBayNames, bays |> List.map (fun b -> b.name))

    // ============================ host MVU: choose / vary / capture / range ============================

    [<Fact>]
    let ``choosing the source constrains the variable to wavelength (data-driven)`` () =
        let m = withTunableSource () |> updateTableAndElementRotation (ExpChooseElement (Library.ElementId "src"))
        Assert.Equal(Some (Library.ElementId "src"), m.experimentCollection.draft.elementId)
        Assert.Equal(Some Experiments.VaryWaveLength, m.experimentCollection.draft.variable)
        // Spec 0046: the sweep spans the tunable source's limits (190–1000 nm) and stays inside them.
        Assert.True(abs (m.experimentCollection.draft.range.min.value - 190.0) < 1e-9)
        Assert.True(abs (m.experimentCollection.draft.range.max.value - 1000.0) < 1e-9)
        Assert.True(abs ((updateTableAndElementRotation (ExpSetRangeMin (ExperimentControls.ExperimentRangeEnd 50.0)) m).experimentCollection.draft.range.min.value - 190.0) < 1e-9)

    [<Fact>]
    let ``a fixed-line or unbound source varies nothing — its wavelength cannot be changed`` () =
        for m in [ SceneTestFixture.mainScene (); bind 0 Library.PresetIds.heNe633 (SceneTestFixture.mainScene ()) ] do
            let chosen = m |> updateTableAndElementRotation (ExpChooseElement (Library.ElementId "src"))
            Assert.Equal(None, chosen.experimentCollection.draft.variable)
            Assert.Empty((chosen.experimentState).variableChoices)

    [<Fact>]
    let ``choosing a sample allows R1 and R2 and can then switch variable`` () =
        let m = SceneTestFixture.mainScene () |> updateTableAndElementRotation (AddElement Sample)
        let sid = idOf 2 m
        let m1 = m |> updateTableAndElementRotation (ExpChooseElement (Library.ElementId sid))
        // The sample bay state offers exactly the sample's allowed variables (R1, R2).
        let st = m1.experimentState
        Assert.Equal<ExperimentControls.VariableChoice list>([ ExperimentControls.VaryR1; ExperimentControls.VaryR2 ], st.variableChoices)
        Assert.Equal(Some Experiments.VaryR1, m1.experimentCollection.draft.variable)
        let m2 = m1 |> updateTableAndElementRotation (ExpChooseVariable ExperimentControls.VaryR2)
        Assert.Equal(Some Experiments.VaryR2, m2.experimentCollection.draft.variable)

    [<Fact>]
    let ``choosing a mirror defaults the capture to reflected (not hard-coded)`` () =
        let m = SceneTestFixture.mainScene () |> updateTableAndElementRotation (AddElement FlatMirror)
        let mid = idOf 2 m
        let m1 = m |> updateTableAndElementRotation (ExpChooseElement (Library.ElementId mid))
        Assert.Equal(Experiments.CaptureReflected, m1.experimentCollection.draft.measurement)

    [<Fact>]
    let ``acceptance (012): a placed sample's emission and experiment default derive from its supportedEmission`` () =
        // Spec 0040 Part D.3: a placed sample is bounded by the SAMPLE it is bound to, not the generic
        // defaultEmission. Spec 0046: a MIRROR reflects only ⇒ the beam continues reflected and the seeded
        // detector captures R; a ThinFilm reflects AND transmits (EmitBoth).
        let mirror = SceneTestFixture.mainScene () |> updateTableAndElementRotation (AddElement Sample) |> bind 2 euvMirrorId
        Assert.Equal<Placement.Emission>(Placement.EmitReflectedOnly, (elem 2 mirror).placement.emission)
        let mirrorChosen = mirror |> updateTableAndElementRotation (ExpChooseElement (Library.ElementId (idOf 2 mirror)))
        Assert.Equal(Experiments.CaptureReflected, mirrorChosen.experimentCollection.draft.measurement)
        let film = SceneTestFixture.mainScene () |> updateTableAndElementRotation (AddElement Sample) |> bind 2 glassFilm600Id
        Assert.Equal<Placement.Emission>(Placement.EmitBoth, (elem 2 film).placement.emission)

        // Spec 0046: a Plate EMITS both, but what an experiment captures follows the DETECTORS — the
        // seeded detector sits on the plate's transmitted beam only ⇒ CaptureTransmitted.
        let plate = SceneTestFixture.mainScene () |> updateTableAndElementRotation (AddElement Sample) |> bind 2 glassPlate1mmId
        Assert.Equal<Placement.Emission>(Placement.EmitBoth, (elem 2 plate).placement.emission)
        let plateChosen = plate |> updateTableAndElementRotation (ExpChooseElement (Library.ElementId (idOf 2 plate)))
        Assert.Equal(Experiments.CaptureTransmitted, plateChosen.experimentCollection.draft.measurement)

    [<Fact>]
    let ``the measurement and range round-trip through the model`` () =
        let m =
            SceneTestFixture.mainScene ()
            |> updateTableAndElementRotation (ExpChooseElement (Library.ElementId "src"))
            |> updateTableAndElementRotation (ExpChooseMeasurement ExperimentControls.CaptureBoth)
            |> updateTableAndElementRotation (ExpSetRangeMin (ExperimentControls.ExperimentRangeEnd 250.0))
            |> updateTableAndElementRotation (ExpSetRangeMax (ExperimentControls.ExperimentRangeEnd 650.0))
            |> updateTableAndElementRotation (ExpSetRangePoints (ExperimentControls.ExperimentPointCount 51))
        let d = m.experimentCollection.draft
        Assert.Equal(Experiments.CaptureBoth, d.measurement)
        Assert.Equal(250.0, d.range.min.value)
        Assert.Equal(650.0, d.range.max.value)
        Assert.Equal(51, d.range.points.value)

    // ============================ host MVU: Add / edit / remove collection ============================

    [<Fact>]
    let ``Add appends to the collection and re-Add does not duplicate`` () =
        let m = withTunableSource () |> updateTableAndElementRotation (ExpChooseElement (Library.ElementId "src")) |> updateTableAndElementRotation ExpCommit
        Assert.Equal(1, List.length m.experimentCollection.experiments)
        let m2 = m |> updateTableAndElementRotation ExpCommit
        Assert.Equal(1, List.length m2.experimentCollection.experiments)

    [<Fact>]
    let ``New then Add appends a distinct second experiment`` () =
        let m =
            withTunableSource ()
            |> updateTableAndElementRotation (ExpChooseElement (Library.ElementId "src")) |> updateTableAndElementRotation ExpCommit
            |> updateTableAndElementRotation ExpNew
            |> updateTableAndElementRotation (ExpChooseElement (Library.ElementId "det"))           // detector has nothing to vary → cannot commit
        // The detector exposes nothing to vary, so a second Add is inert.
        let m2 = m |> updateTableAndElementRotation ExpCommit
        Assert.Equal(1, List.length m2.experimentCollection.experiments)
        // Choose a varyable element instead and Add — now there are two.
        let m3 = m |> updateTableAndElementRotation (AddElement Sample)
        let sid = idOf 2 m3
        let m4 = m3 |> updateTableAndElementRotation (ExpChooseElement (Library.ElementId sid)) |> updateTableAndElementRotation ExpCommit
        Assert.Equal(2, List.length m4.experimentCollection.experiments)

    [<Fact>]
    let ``Edit loads an experiment and Remove deletes it`` () =
        let m = withTunableSource () |> updateTableAndElementRotation (ExpChooseElement (Library.ElementId "src")) |> updateTableAndElementRotation ExpCommit
        let exp = List.head m.experimentCollection.experiments
        let mEdit = m |> updateTableAndElementRotation (ExpEdit exp.id)
        Assert.Equal(Some exp.id, mEdit.experimentCollection.draft.editingId)
        let mRemove = m |> updateTableAndElementRotation (ExpRemove exp.id)
        Assert.Empty(mRemove.experimentCollection.experiments)

    [<Fact>]
    let ``the bay state exposes the collection rows and the editing highlight`` () =
        let m = withTunableSource () |> updateTableAndElementRotation (ExpChooseElement (Library.ElementId "src")) |> updateTableAndElementRotation ExpCommit
        let st = m.experimentState
        Assert.Single(st.collection) |> ignore
        let row = List.head st.collection
        Assert.Contains("Light source", row.description.value)
        Assert.Equal(ExperimentControls.LoadedInEditor, row.focus)
        Assert.Equal(ExperimentControls.EditorUpdatesExisting, st.editorMode)

    // ============================ host branches.experimentResult ============================

    /// initMain + a LinearPolarizer at index 2, chosen to vary (default VaryR1).
    let private withChosenPolarizer () : TableAndElementRotationModel =
        let m = SceneTestFixture.mainScene () |> updateTableAndElementRotation (AddElement LinearPolarizer)
        m |> updateTableAndElementRotation (ExpChooseElement (Library.ElementId (idOf 2 m)))

    /// initMain + a bound Sample at index 2, chosen, on the given variable, capture T.
    let private withChosenSample (v : ExperimentControls.VariableChoice) : TableAndElementRotationModel =
        let m = SceneTestFixture.mainScene () |> updateTableAndElementRotation (AddElement Sample) |> bind 2 glassFilm200Id
        m
        |> updateTableAndElementRotation (ExpChooseElement (Library.ElementId (idOf 2 m)))
        |> updateTableAndElementRotation (ExpChooseVariable v)
        |> updateTableAndElementRotation (ExpChooseMeasurement ExperimentControls.CaptureT)

    [<Fact>]
    let ``with no element chosen the chart is empty`` () =
        Assert.Empty(((SceneTestFixture.mainScene ()).experimentResult).series)

    [<Fact>]
    let ``varying a polarizer R1 with an intensity detector yields an intensity series`` () =
        let chart = (withChosenPolarizer ()).experimentResult
        Assert.Single(chart.series) |> ignore
        Assert.NotEmpty((List.head chart.series).points)
        Assert.Equal("Rotation R1 (°)", chart.xLabel.value)
        Assert.Equal("Intensity (S₀)", chart.yLabel.value)

    [<Fact>]
    let ``a VaryR2 intensity experiment yields one incidence series with x running to 89`` () =
        let chart = (withChosenSample ExperimentControls.VaryR2).experimentResult
        Assert.Single(chart.series) |> ignore
        let xs = (List.head chart.series).points |> List.map (fun p -> p.x)
        Assert.NotEmpty(xs)
        Assert.Equal("Incidence angle R2 (°)", chart.xLabel.value)
        Assert.True(abs (89.0 - List.last xs) < 1e-6, $"last x = %g{List.last xs}, expected 89")
        Assert.True((xs = List.sort xs), "incidence x-values must be sorted ascending")

    [<Fact>]
    let ``a VaryWaveLength intensity experiment spans the chosen wavelength range`` () =
        let m =
            withChosenSample ExperimentControls.VaryWaveLength
            |> updateTableAndElementRotation (ExpSetRangeMin (ExperimentControls.ExperimentRangeEnd 300.0))
            |> updateTableAndElementRotation (ExpSetRangeMax (ExperimentControls.ExperimentRangeEnd 700.0))
        let chart = m.experimentResult
        Assert.Single(chart.series) |> ignore
        let xs = (List.head chart.series).points |> List.map (fun p -> p.x)
        Assert.True(abs (300.0 - List.head xs) < 1e-6)
        Assert.True(abs (700.0 - List.last xs) < 1e-6)
        Assert.Equal("Wavelength (nm)", chart.xLabel.value)

    [<Fact>]
    let ``capturing BOTH branches yields two series (T and R)`` () =
        // A bound Plate sample with a detector on its transmitted beam (the seeded one) AND one on its
        // reflected ray (spec 0046: Both is offered only then), VaryR2, capture Both ⇒ a transmitted AND
        // a reflected intensity series.
        let m =
            SceneTestFixture.mainScene () |> updateTableAndElementRotation (AddElement Sample) |> bind 2 glassPlate1mmId
            |> updateTableAndElementRotation (AddElement Detector)
            |> (fun m -> m |> updateTableAndElementRotation (PlaceOnBeam ((elem 3 m).id, BeamLayout.OnReflectedBeamOf (elem 2 m).id)))
            |> (fun m -> m |> updateTableAndElementRotation (ExpChooseElement (Library.ElementId (idOf 2 m))))
            |> updateTableAndElementRotation (ExpChooseVariable ExperimentControls.VaryR2)
            |> updateTableAndElementRotation (ExpChooseMeasurement ExperimentControls.CaptureBoth)
        let chart = m.experimentResult
        Assert.Equal(2, List.length chart.series)
        Assert.Equal<string list>([ "Intensity (T)"; "Intensity (R)" ], chart.series |> List.map (fun s -> s.name.value))

    [<Fact>]
    let ``an ellipsometer VaryR2 experiment yields two series (Psi and Delta)`` () =
        let m =
            SceneTestFixture.mainScene () |> updateTableAndElementRotation (AddElement Sample) |> bind 2 glassFilm200Id |> bind 1 Library.PresetIds.detectorEllipsometer
            |> (fun m -> m |> updateTableAndElementRotation (ExpChooseElement (Library.ElementId (idOf 2 m))))
            |> updateTableAndElementRotation (ExpChooseVariable ExperimentControls.VaryR2)
            |> updateTableAndElementRotation (ExpChooseMeasurement ExperimentControls.CaptureT)
        let chart = m.experimentResult
        Assert.Equal<string list>([ "Ψ"; "Δ" ], chart.series |> List.map (fun s -> s.name.value))

    [<Fact>]
    let ``a VaryR2 experiment with no bound sample yields an empty chart`` () =
        // A polarizer varied on R2 is impossible (polarizers only vary R1); use a sample element but do NOT
        // bind it — an R2 vary then has no sample to re-solve.
        let m = SceneTestFixture.mainScene () |> updateTableAndElementRotation (AddElement Sample)
        let m1 =
            m
            |> (fun m -> m |> updateTableAndElementRotation (ExpChooseElement (Library.ElementId (idOf 2 m))))
            |> updateTableAndElementRotation (ExpChooseVariable ExperimentControls.VaryR2)
        Assert.Empty((m1.experimentResult).series)

    [<Fact>]
    let ``two bound samples in series are both solved on the detector's path`` () =
        let m =
            SceneTestFixture.mainScene ()
            |> updateTableAndElementRotation (AddElement Sample) |> bind 2 glassPlate1mmId
            |> updateTableAndElementRotation (AddElement Sample) |> bind 3 glassFilm200Id
            |> (fun m -> m |> updateTableAndElementRotation (ExpChooseElement (Library.ElementId (idOf 3 m))))
            |> updateTableAndElementRotation (ExpChooseVariable ExperimentControls.VaryR2)
            |> updateTableAndElementRotation (ExpChooseMeasurement ExperimentControls.CaptureT)
            |> updateTableAndElementRotation (ExpSetRangeMin (ExperimentControls.ExperimentRangeEnd 0.0))
            |> updateTableAndElementRotation (ExpSetRangeMax (ExperimentControls.ExperimentRangeEnd 60.0))
            |> updateTableAndElementRotation (ExpSetRangePoints (ExperimentControls.ExperimentPointCount 7))
        let plateId, filmId = (elem 2 m).id, (elem 3 m).id
        // Both samples sit untilted on the main beam in front of the seeded intensity detector (index 1).
        let samplesInLightOrder =
            (List.item 1 m.sceneLayout.elements).path
            |> List.map (fun step -> step.element)
            |> List.filter (fun id -> id = plateId || id = filmId)
        Assert.Equal(2, List.length samplesInLightOrder)
        Assert.Contains(plateId, samplesInLightOrder)
        Assert.Contains(filmId, samplesInLightOrder)
        let resolved (id : Library.SampleId) : Propagation.ResolvedSample =
            match Propagation.resolveSampleMaterials m.materials (BuiltinCatalogue.sample id) with
            | Ok sample -> sample
            | Error error -> failwith $"{id.value}: {(ItemEvaluation.SampleMaterialErr error).describe}"
        let plate, film = resolved Library.SampleIds.glassPlate1mmId, resolved Library.SampleIds.glassFilm200Id
        let at600 = WaveLength.nm 600.0<nm>
        let chart = m.experimentResult
        let points = (Assert.Single chart.series).points
        Assert.Equal<float list>([ 0.0; 10.0; 20.0; 30.0; 40.0; 50.0; 60.0 ], points |> List.map (fun p -> p.x))
        for p in points do
            let transmitted (id : Library.ElementId) : MuellerMatrix =
                if id = plateId then Propagation.sampleMuellerSpun Propagation.BranchTransmitted plate at600 IncidenceAngle.normal Angle.zero
                else Propagation.sampleMuellerSpun Propagation.BranchTransmitted film at600 (IncidenceAngle.create (Angle.degree (p.x * 1.0<deg>))) Angle.zero
            let expected = samplesInLightOrder |> List.fold (fun sv id -> transmitted id * sv) Propagation.unpolarizedStokes
            Assert.True(abs (expected.s0 - p.y) <= 1.0e-12, $"x = {p.x:R}: expected {expected.s0:R}, got {p.y:R}")

    // ============ spec 0050 S8.2 — the run reads the slots: a slot's item through its own model ============

    /// Runs over a fresh Main scene in its scratch kit (`initMain`), closing the kit at the end (spec 0050 .manual/010).
    let private withMainScene (run : TableAndElementRotationModel -> unit) : unit =
        let scene = initMain () |> SceneTestFixture.kitSuccess
        try run scene
        finally scene.kit.close () |> SceneTestFixture.kitSuccess

    /// The model after the message, which must leave the kit status clear.
    let private applied (msg : TableAndElementRotationMsg) (model : TableAndElementRotationModel) : TableAndElementRotationModel =
        let next = model |> updateTableAndElementRotation msg
        Assert.Equal<ControlCaption option>(None, next.kitStatus)
        next

    /// A known angle parameter of that many degrees, in radians, an angle parameter's unit (spec 0050 S8.1).
    let private knownDegrees (degrees : float<deg>) : ParameterState =
        KnownParameter (ParameterValue (Angle.degree degrees).value)

    /// The draft varying the slot on `variable` over `lo` … `hi` in that many points, capturing the transmitted branch.
    let private sweeping
        (id : Library.ElementId)
        (variable : ExperimentControls.VariableChoice)
        (lo : ExperimentControls.ExperimentRangeEnd)
        (hi : ExperimentControls.ExperimentRangeEnd)
        (points : ExperimentControls.ExperimentPointCount)
        (model : TableAndElementRotationModel)
        : TableAndElementRotationModel =
        model
        |> updateTableAndElementRotation (ExpChooseElement id)
        |> updateTableAndElementRotation (ExpChooseVariable variable)
        |> updateTableAndElementRotation (ExpChooseMeasurement ExperimentControls.CaptureT)
        |> updateTableAndElementRotation (ExpSetRangeMin lo)
        |> updateTableAndElementRotation (ExpSetRangeMax hi)
        |> updateTableAndElementRotation (ExpSetRangePoints points)

    /// The table with two linear polarizers added, each pre-bound to the linear preset, the second selected.
    let private withTwoPolarizers (model : TableAndElementRotationModel) : TableAndElementRotationModel =
        model
        |> updateTableAndElementRotation (AddElement LinearPolarizer)
        |> updateTableAndElementRotation (AddElement LinearPolarizer)

    /// The real linear polarizer on its shipped item model (step 210).
    let private realPolarizerModel : ItemModelVersion = BuiltinCatalogue.itemModel ItemModelIds.realLinearPolarizerModelId

    let private realPolarizerDraft : ItemDraft =
        {
            id = ItemId (System.Guid.Parse "26500000-0000-4000-8000-000000000001")
            label = ItemLabel "RLP"
            model = LibraryModelChoice realPolarizerModel
            dial = FixedMounting
        }

    /// The 1 mm glass plate at its first version on the shipped stack-sample item model (step 210).
    let private plateDraft : ItemDraft =
        {
            id = ItemId (System.Guid.Parse "26500000-0000-4000-8000-000000000002")
            label = ItemLabel "QZ"
            model =
                StackSampleChoice (
                    BuiltinCatalogue.itemModel ItemModelIds.stackSampleItemModelId,
                    Library.SampleVersionId.firstOf Library.SampleIds.glassPlate1mmId,
                    BuiltinCatalogue.sample Library.SampleIds.glassPlate1mmId
                )
            dial = FixedMounting
        }

    [<Fact>]
    let ``a slot's item is run through its own parameters: a real polarizer item's leakage passes crossed light`` () =
        withMainScene (fun scene ->
            let placed =
                scene
                |> applied (AddItem realPolarizerDraft)
                |> applied (SetParameterState (realPolarizerDraft.id, ParameterName.extinctionRatio, KnownParameter (ParameterValue 0.01)))
                |> withTwoPolarizers
            let first, second = elem 2 placed, elem 3 placed
            Assert.Equal<ElementValueId option list>(
                [ Some Library.PresetIds.polarizerLp; Some Library.PresetIds.polarizerLp ],
                [ first.placement.valueId; second.placement.valueId ])
            Assert.Equal(0.0, first.placement.r1.value)
            // The first polarizer stands in front of the second on the seeded detector's (index 1) path.
            Assert.Equal<Library.ElementId list>(
                [ first.id; second.id ],
                (List.item 1 placed.sceneLayout.elements).path |> List.map (fun step -> step.element) |> List.filter (fun id -> id = first.id || id = second.id))
            let held = placed |> applied (AssignSlot (second.id, Some realPolarizerDraft.id))
            let chartOf (table : TableAndElementRotationModel) : ExperimentChart.ExperimentChart =
                let swept =
                    table
                    |> sweeping second.id ExperimentControls.VaryR1 (ExperimentControls.ExperimentRangeEnd 0.0) (ExperimentControls.ExperimentRangeEnd 180.0) (ExperimentControls.ExperimentPointCount 5)
                swept.experimentResult
            let sweptBy (table : TableAndElementRotationModel) : ExperimentControls.ChartPoint list = (Assert.Single (chartOf table).series).points
            let points = sweptBy held
            Assert.Equal<float list>([ 0.0; 45.0; 90.0; 135.0; 180.0 ], points |> List.map (fun p -> p.x))

            // Each point is the item's own form at its values, turned to the point's rotation, after the first polarizer at 0°.
            let item = held.kitItems |> List.find (fun kitItem -> kitItem.id = realPolarizerDraft.id)
            let form =
                match realPolarizerModel.entry.model with
                | OpticalElementModel form -> form
                | _ -> failwith "The real linear polarizer model must be an optical element."
            let bound =
                ItemEvaluation.BoundOpticalElement.ofOpticalForm (ParameterValues.ofParameters item.parameters) form
                |> Result.defaultWith (fun error -> failwith error.describe)
            let at600 = WaveLength.nm 600.0<nm>
            for p in points do
                let matrix =
                    bound.muellerAt (ItemEvaluation.ElementOrientation.transmittingAt (Angle.degree (p.x * 1.0<deg>))) at600
                    |> Result.defaultWith (fun error -> failwith error.describe)
                let expected = (matrix * Propagation.inputStokes Library.IdealLinear Angle.zero).s0
                Assert.True(abs (expected - p.y) <= 1.0e-12, $"x = {p.x:R}: expected {expected:R}, got {p.y:R}")

            // Crossed at 90°, the item's leakage passes light, where the preset in its place passes none.
            let crossed = (List.item 2 points).y
            Assert.True(crossed > 1.0e-4, $"the real polarizer item crossed must pass its leakage, read {crossed:R}")
            let presetCrossed = (List.item 2 (sweptBy (held |> applied (AssignSlot (second.id, None))))).y
            Assert.True(abs presetCrossed < 1.0e-12, $"the preset crossed must pass nothing, read {presetCrossed:R}")

            // A slot's item the kit no longer holds is named on the failure chart, never run as the preset it sits on.
            let lost = chartOf { held with kitItems = [] }
            Assert.Empty(lost.series)
            Assert.Equal("The run cannot be evaluated", lost.title.value)
            Assert.Equal((UnknownSlotItemErr $"the kit holds no item {realPolarizerDraft.id.value}").describe, lost.description.value))

    [<Fact>]
    let ``a sample item is solved at its own thickness, as the item evaluator solves it`` () =
        withMainScene (fun scene ->
            let thickness = ParameterValue 2.0e-3
            let placed =
                scene
                |> applied (AddItem plateDraft)
                |> applied (SetParameterState (plateDraft.id, ParameterName.substrateThickness, KnownParameter thickness))
                |> updateTableAndElementRotation (AddElement Sample)
            let sample = elem 2 placed
            Assert.Equal<ElementValueId option>(None, sample.placement.valueId)
            let held = placed |> applied (AssignSlot (sample.id, Some plateDraft.id))
            Assert.Equal<Experiments.MeasurementMode list>([ Experiments.CaptureTransmitted ], availableCaptures held sample.id)
            let swept =
                held
                |> sweeping sample.id ExperimentControls.VaryR2 (ExperimentControls.ExperimentRangeEnd 0.0) (ExperimentControls.ExperimentRangeEnd 60.0) (ExperimentControls.ExperimentPointCount 7)
            let points = (Assert.Single swept.experimentResult.series).points
            Assert.Equal<float list>([ 0.0; 10.0; 20.0; 30.0; 40.0; 50.0; 60.0 ], points |> List.map (fun p -> p.x))

            // Each point is the item evaluator's solve of the plate at the item's values, at the point's incidence, spin 0.
            let item = held.kitItems |> List.find (fun kitItem -> kitItem.id = plateDraft.id)
            let values = ParameterValues.ofParameters item.parameters
            Assert.Equal<ParameterValue>(thickness, values.valueOf ParameterName.substrateThickness |> Result.defaultWith (fun error -> failwith error.describe))
            let plate = BuiltinCatalogue.sample Library.SampleIds.glassPlate1mmId
            let transmittedS0 (bound : ItemEvaluation.BoundOpticalElement) (incidenceDegrees : float) : float =
                let orientation : ItemEvaluation.ElementOrientation =
                    {
                        ItemEvaluation.ElementOrientation.rotation = Angle.zero
                        incidence = IncidenceAngle.create (Angle.degree (incidenceDegrees * 1.0<deg>))
                        branch = Propagation.BranchTransmitted
                    }
                let matrix = bound.muellerAt orientation (WaveLength.nm 600.0<nm>) |> Result.defaultWith (fun error -> failwith error.describe)
                (matrix * Propagation.unpolarizedStokes).s0
            let boundAt (atValues : ItemEvaluation.ParameterValues) : ItemEvaluation.BoundOpticalElement =
                ItemEvaluation.BoundOpticalElement.ofStackSample held.materials atValues plate
                |> Result.defaultWith (fun error -> failwith error.describe)
            let itemPlate = boundAt values
            for p in points do
                let expected = transmittedS0 itemPlate p.x
                Assert.True(abs (expected - p.y) <= 1.0e-12, $"x = {p.x:R}: expected {expected:R}, got {p.y:R}")

            // The plate's own 1 mm reads otherwise: the run solved the item's thickness, not the sample's default.
            let defaultPlate = boundAt (ParameterValues.defaultsOf plate.parameterAxes)
            Assert.True(
                points |> List.exists (fun p -> abs (transmittedS0 defaultPlate p.x - p.y) > 1.0e-12),
                "the 2 mm item must read otherwise than the 1 mm sample at some incidence"))

    [<Fact>]
    let ``a dial-mounted item is run at the angle its reading draws`` () =
        let polarizer =
            match BuiltinCatalogue.preset Library.PresetIds.polarizerLp with
            | Library.PolarizerItem value -> value
            | Library.SampleItem _
            | Library.SourceItem _
            | Library.DetectorItem _ -> failwith "The linear polarizer preset must hold a polarizer."
        let dialDraft : ItemDraft =
            {
                id = ItemId (System.Guid.Parse "26500000-0000-4000-8000-000000000003")
                label = ItemLabel "LPd"
                model = PolarizerPresetChoice (BuiltinCatalogue.itemModel ItemModelIds.idealPolarizerItemModelId, polarizer)
                dial = DialMounted DialNegative
            }
        let chartSweepingFirst (table : TableAndElementRotationModel) : ExperimentChart.ExperimentChart =
            let swept =
                table
                |> sweeping (elem 2 table).id ExperimentControls.VaryR1 (ExperimentControls.ExperimentRangeEnd 0.0) (ExperimentControls.ExperimentRangeEnd 90.0) (ExperimentControls.ExperimentPointCount 4)
            swept.experimentResult
        withMainScene (fun scene ->
            // The second polarizer, which the add selected, turned to R1 30° and given LPd, whose zero is 10°: the reading
            // is 10° − 30° = −20°.
            let placed =
                scene
                |> applied (AddItem dialDraft)
                |> applied (SetParameterState (dialDraft.id, ParameterName.dialZero, knownDegrees 10.0<deg>))
                |> withTwoPolarizers
                |> updateTableAndElementRotation (RotSetAxis (RotationControls.R1, BarDegrees 30.0))
            let second = elem 3 placed
            Assert.Equal((Angle.degree 30.0<deg>).value, second.placement.r1.value, 1e-12)
            let recorded =
                placed
                |> applied (AssignSlot (second.id, Some dialDraft.id))
                |> updateTableAndElementRotation (SetSetupName (ControlCaption "Dial"))
                |> updateTableAndElementRotation CreateSetup
            Assert.Equal<ControlCaption option>(Some (ControlCaption "Created setup 'Dial'."), recorded.kitStatus)
            match (recorded.kitSetups |> List.find (fun setup -> setup.name = SetupName "Dial")).slots |> List.find (fun slot -> slot.id = second.id) with
            | { Setups.SetupSlot.content = Setups.ItemInSlot (heldId, Setups.DialTurn reading) } ->
                Assert.Equal<ItemId>(dialDraft.id, heldId)
                Assert.Equal((Angle.degree (-20.0<deg>)).value, reading.value, 1e-12)
            | other -> Assert.Fail $"the second polarizer must hold LPd on its dial, held %A{other.content}"

            // Re-zeroed to 20°, the activated setup draws the reading −20° at −(−20° − 20°) = 40°.
            let drawn =
                recorded
                |> applied (SetParameterState (dialDraft.id, ParameterName.dialZero, knownDegrees 20.0<deg>))
                |> updateTableAndElementRotation (ActivateSetup (SetupName "Dial"))
            Assert.Equal<ControlCaption option>(Some (ControlCaption "Loaded setup 'Dial'."), drawn.kitStatus)
            Assert.Equal((Angle.degree 40.0<deg>).value, (drawn.elements |> List.find (fun slot -> slot.id = second.id)).placement.r1.value, 1e-12)
            let itemChart = chartSweepingFirst drawn

            withMainScene (fun bare ->
                let reference = bare |> withTwoPolarizers |> updateTableAndElementRotation (RotSetAxis (RotationControls.R1, BarDegrees 40.0))
                Assert.Equal<ElementValueId option>(Some Library.PresetIds.polarizerLp, (elem 3 reference).placement.valueId)
                Assert.Equal<Setups.SlotContent>(Setups.EmptySlotContent, (elem 3 reference).content)
                let referenceChart = chartSweepingFirst reference
                Assert.Equal(referenceChart.title, itemChart.title)
                let itemPoints = (Assert.Single itemChart.series).points
                let referencePoints = (Assert.Single referenceChart.series).points
                Assert.Equal<float list>([ 0.0; 30.0; 60.0; 90.0 ], itemPoints |> List.map (fun p -> p.x))
                Assert.Equal<float list>(referencePoints |> List.map (fun p -> p.x), itemPoints |> List.map (fun p -> p.x))
                for itemPoint, referencePoint in List.zip itemPoints referencePoints do
                    Assert.True(
                        abs (itemPoint.y - referencePoint.y) <= 1.0e-12,
                        $"x = {itemPoint.x:R}: the dial item read {itemPoint.y:R}, the bare preset at 40° {referencePoint.y:R}")))

    // ============ spec 0050 S8.2 — the source slot's item is the run's source ============

    /// The HeNe preset on the shipped source item model (step 210).
    let private laserDraft : ItemDraft =
        {
            id = ItemId (System.Guid.Parse "26600000-0000-4000-8000-000000000001")
            label = ItemLabel "Laser"
            model = SourcePresetChoice (BuiltinCatalogue.itemModel ItemModelIds.sourcePresetItemModelId, BuiltinCatalogue.source Library.PresetIds.heNe633)
            dial = FixedMounting
        }

    /// The source slot holding `Laser`, then one linear polarizer bound to its preset before the seeded intensity
    /// detector, swept over R1 0 … 180° in 7 points.
    let private laserThroughPolarizer (scene : TableAndElementRotationModel) : TableAndElementRotationModel =
        let source = elem 0 scene
        Assert.Equal(LightSource, source.placement.catalogueKind)
        let held =
            scene
            |> applied (AddItem laserDraft)
            |> applied (AssignSlot (source.id, Some laserDraft.id))
            |> updateTableAndElementRotation (AddElement LinearPolarizer)
        let polarizer = elem 2 held
        Assert.Equal<ElementValueId option>(Some Library.PresetIds.heNe633, (elem 0 held).placement.valueId)
        Assert.Equal<ElementValueId option>(Some Library.PresetIds.polarizerLp, polarizer.placement.valueId)
        Assert.Equal<Library.ElementId list>(
            [ source.id; polarizer.id ],
            (List.item 1 held.sceneLayout.elements).path |> List.map (fun step -> step.element) |> List.filter (fun id -> id = source.id || id = polarizer.id))
        held
        |> sweeping polarizer.id ExperimentControls.VaryR1 (ExperimentControls.ExperimentRangeEnd 0.0) (ExperimentControls.ExperimentRangeEnd 180.0) (ExperimentControls.ExperimentPointCount 7)

    [<Fact>]
    let ``a source item emits its own polarization through the run`` () =
        withMainScene (fun scene ->
            let swept = laserThroughPolarizer scene
            let pointsOf (table : TableAndElementRotationModel) : ExperimentControls.ChartPoint list = (Assert.Single table.experimentResult.series).points

            // At its defaults the item emits the default source's unpolarized light, which the polarizer, the intensity
            // reference (r = 2), reads as 1.
            let defaultEmission = ExperimentRun.SourceEmission.ofValues (ParameterValues.defaultsOf Library.SourcePreset.lightSourceAxes)
            Assert.Equal<Result<ExperimentRun.SourceEmission, ControlCaption>>(defaultEmission |> Result.mapError (fun error -> ControlCaption error.describe), swept.runEmission)
            let atDefaults = pointsOf swept
            Assert.Equal<float list>([ 0.0; 30.0; 60.0; 90.0; 120.0; 150.0; 180.0 ], atDefaults |> List.map (fun p -> p.x))
            for p in atDefaults do
                Assert.True(abs (p.y - 1.0) <= 1.0e-12, $"x = {p.x:R}: the item at its defaults read {p.y:R}, expected 1")
            let unheld = pointsOf (swept |> applied (AssignSlot ((elem 0 swept).id, None)))
            for p, q in List.zip atDefaults unheld do
                Assert.True(abs (p.y - q.y) <= 1.0e-12, $"x = {p.x:R}: the item at its defaults read {p.y:R}, the default source {q.y:R}")

            // Fully polarized at 30°, it is the item's own `emissionWith` light, and the polarizer reads 1 + cos 2(x − 30°).
            let polarized =
                swept
                |> applied (SetParameterState (laserDraft.id, ParameterName.degreeOfPolarization, KnownParameter (ParameterValue 1.0)))
                |> applied (SetParameterState (laserDraft.id, ParameterName.azimuth, knownDegrees 30.0<deg>))
            let item = polarized.kitItems |> List.find (fun kitItem -> kitItem.id = laserDraft.id)
            let ownEmission = laserDraft.model.emissionWith (ParameterValues.ofParameters item.parameters)
            Assert.Equal<Result<ExperimentRun.SourceEmission, ControlCaption>>(ownEmission |> Result.mapError (fun error -> ControlCaption error.describe), polarized.runEmission)
            for p in pointsOf polarized do
                let expected = 1.0 + cos (2.0 * (Angle.degree ((p.x - 30.0) * 1.0<deg>)).value)
                Assert.True(abs (expected - p.y) <= 1.0e-12, $"x = {p.x:R}: expected {expected:R}, got {p.y:R}"))

    [<Fact>]
    let ``a source item whose parameters the run cannot evaluate shows that the run cannot be evaluated`` () =
        withMainScene (fun scene ->
            let held =
                laserThroughPolarizer scene
                |> applied (SetParameterState (laserDraft.id, ParameterName.degreeOfPolarization, KnownParameter (ParameterValue 1.5)))
            let chart = held.experimentResult
            Assert.Empty(chart.series)
            Assert.Equal("The run cannot be evaluated", chart.title.value)
            Assert.Contains(ParameterName.degreeOfPolarization.value, chart.description.value)
            Assert.Equal((ParameterOutOfRangeErr "degreeOfPolarization = 1.5 lies outside [0, 1]").describe, chart.description.value)
            Assert.Equal<Result<ExperimentRun.SourceEmission, ControlCaption>>(Error chart.description, held.runEmission)

            // A source item the kit no longer holds is named on the same chart, never run as the preset it binds.
            let lost = ({ held with kitItems = [] }).experimentResult
            Assert.Empty(lost.series)
            Assert.Equal("The run cannot be evaluated", lost.title.value)
            Assert.Equal((UnknownSlotItemErr $"the kit holds no item {laserDraft.id.value}").describe, lost.description.value))

    // ============ step 018 — the silent ideal-analyzer fallback is RETIRED ============

    [<Fact>]
    let ``runAnalyzerKind prefers the VARIED element's bound polarizer over the scene's`` () =
        // Two pre-bound polarizers: the linear one (idx 2) and — LAST in the scene — the CPR
        // button's right-circular one (idx 3). The varied element's own bound entry wins; the
        // scene's last bound polarizer only serves a non-polarizer varied element.
        let m =
            SceneTestFixture.mainScene ()
            |> updateTableAndElementRotation (AddElement LinearPolarizer)                                                       // pre-bound PresetIds.polarizerLp
            |> updateTableAndElementRotation (AddElementBoundTo (CircularPolarizer, Some Library.PresetIds.polarizerCpRight))   // pre-bound PresetIds.polarizerCpRight
        Assert.Equal(ResolvedAnalyzer Library.IdealLinear, runAnalyzerKind m (Some (elem 2 m).id))
        Assert.Equal(ResolvedAnalyzer Library.IdealCircularRight, runAnalyzerKind m (Some (elem 3 m).id))

    [<Fact>]
    let ``runAnalyzerKind reads the scene's bound polarizer for a non-polarizer varied element — never an assumed linear`` () =
        // A circular-only scene: the resolution is the BOUND entry's kind (circular). The retired
        // fallback would have silently answered IdealLinear here.
        let m =
            SceneTestFixture.mainScene ()
            |> updateTableAndElementRotation (AddElement CircularPolarizer)   // pre-bound PresetIds.polarizerCpLeft
            |> updateTableAndElementRotation (AddElement Sample)              // unbound (the inverse hook)
        Assert.Equal(ResolvedAnalyzer Library.IdealCircularLeft, runAnalyzerKind m (Some (elem 3 m).id))

    [<Fact>]
    let ``runAnalyzerKind reports the typed NoAnalyzerPresent status when the scene holds no bound polarizer`` () =
        // initMain = source + detector only; adding an (unbound) sample binds no polarizer entry.
        let m = SceneTestFixture.mainScene () |> updateTableAndElementRotation (AddElement Sample)
        Assert.Equal(NoAnalyzerPresent, runAnalyzerKind m (Some (elem 2 m).id))
        Assert.Equal(NoAnalyzerPresent, runAnalyzerKind m None)

    [<Fact>]
    let ``a rotate-R1 intensity run with NO analyzer reports the typed status chart — no silent Malus curve`` () =
        // The retired fallback would have synthesized an IdealLinear Malus curve here; the
        // experiment surface now reports the typed 'no analyzer present' status instead.
        let m = SceneTestFixture.mainScene () |> updateTableAndElementRotation (AddElement Sample)
        let m1 = m |> updateTableAndElementRotation (ExpChooseElement (Library.ElementId (idOf 2 m)))     // a sample defaults to VaryR1
        let chart = m1.experimentResult
        Assert.Empty(chart.series)
        Assert.Equal(noAnalyzerTitle, chart.title.value)
        Assert.Equal(noAnalyzerStatus, chart.description.value)
        // The Experiments bay surfaces the same status through its state.
        Assert.Equal(noAnalyzerStatus, (m1.experimentState).description.value)

    [<Fact>]
    let ``a rotate-R1 intensity run with a pre-bound polarizer still yields the Malus curve`` () =
        // The step-018 add path pre-binds the polarizer, so the analyzer resolves from the BOUND
        // entry — the surface that used to lean on the silent fallback keeps its curve.
        let m = SceneTestFixture.mainScene () |> updateTableAndElementRotation (AddElement LinearPolarizer)
        Assert.Equal(Some Library.PresetIds.polarizerLp, (elem 2 m).placement.valueId)
        let chart = (m |> updateTableAndElementRotation (ExpChooseElement (Library.ElementId (idOf 2 m)))).experimentResult
        Assert.Single(chart.series) |> ignore
        Assert.Equal("Rotating analyzer (Malus)", chart.title.value)

    // ============================ ExperimentChart CSV (unchanged data model) ============================

    /// A named chart series over (x, y) points.
    let private curve (name : string) (points : (float * float) list) : ExperimentControls.ChartSeries =
        {
            ExperimentControls.ChartSeries.name = ControlCaption name
            points = points |> List.map (fun (x, y) -> { ExperimentControls.ChartPoint.x = x; y = y })
        }

    [<Fact>]
    let ``ExperimentChart toCsv writes a header and one row per x for two series`` () =
        let chart : ExperimentChart.ExperimentChart =
            { ExperimentChart.emptyExperimentChart with
                series =
                    [
                        curve "Ψ" [ 0.0, 10.0; 1.0, 11.0 ]
                        curve "Δ" [ 0.0, 20.0; 1.0, 21.0 ]
                    ] }
        let lines = chart.toCsv.Split('\n')
        Assert.Equal("x,Ψ,Δ", lines.[0])
        Assert.Equal(3, lines.Length)
        Assert.Equal("0,10,20", lines.[1])
        Assert.Equal("1,11,21", lines.[2])

    [<Fact>]
    let ``ExperimentChart toCsv of the empty chart is just the header`` () =
        Assert.Equal("x", ExperimentChart.emptyExperimentChart.toCsv)

    // ============================ ChartWindow ids ============================

    [<Fact>]
    let ``ChartWindow ids are stable and distinct incl the element picker + polar toggle`` () =
        Assert.Equal("ChartWindowElement", ConstructorUiId.chartWindowElement.value)
        Assert.Equal("ChartWindowPolar", ConstructorUiId.chartWindowPolar.value)
        let ids =
            [ ConstructorUiId.chartWindowPlot.value; ConstructorUiId.chartWindowElement.value; ConstructorUiId.chartWindowProperties.value
              ConstructorUiId.chartWindowFontMinus.value; ConstructorUiId.chartWindowFontPlus.value; ConstructorUiId.chartWindowFontSize.value
              ConstructorUiId.chartWindowAxisAuto.value; ConstructorUiId.chartWindowAxisMin.value; ConstructorUiId.chartWindowAxisMax.value; ConstructorUiId.chartWindowAxisFormat.value
              ConstructorUiId.chartWindowAxisDecimals.value; ConstructorUiId.chartWindowLegendVisible.value; ConstructorUiId.chartWindowLegendPlacement.value
              ConstructorUiId.chartWindowSeriesVisible.value; ConstructorUiId.chartWindowSeriesThick.value; ConstructorUiId.chartWindowSeriesColor.value
              ConstructorUiId.chartWindowSeriesMarkers.value; ConstructorUiId.chartWindowSeriesAxis.value; ConstructorUiId.chartWindowPolar.value; ConstructorUiId.chartWindowMajorGrid.value
              ConstructorUiId.chartWindowMinorGrid.value; ConstructorUiId.chartWindowExportPng.value; ConstructorUiId.chartWindowExportCsv.value; ConstructorUiId.chartWindowDescription.value ]
        Assert.Equal(List.length ids, ids |> List.distinct |> List.length)

    /// A small sample chart with two series, axis labels, a title, and a description.
    let private sampleChart : ExperimentChart.ExperimentChart =
        {
            ExperimentChart.ExperimentChart.series =
                [
                    curve "Ψ" [ 0.0, 10.0; 1.0, 12.0; 2.0, 9.0 ]
                    curve "Δ" [ 0.0, 20.0; 1.0, 18.0; 2.0, 22.0 ]
                ]
            xLabel = ControlCaption "Incidence angle R2 (°)"
            yLabel = ControlCaption "Ψ, Δ (°)"
            title = ControlCaption "Ellipsometric Ψ/Δ vs incidence"
            description = ControlCaption "A two-series ellipsometer sweep used by the pop-out chart-window smoke test."
            xAxisKind = ExperimentChart.AngularXAxis
        }

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the ChartWindow opens with the element picker, properties panel and polar toggle`` () =
        HeadlessSession.run (fun () ->
            let window = ChartWindow(sampleChart)     // an angular x-axis ⇒ the polar toggle is offered
            try window.Show() with _ -> ()
            try Dispatcher.UIThread.RunJobs() with _ -> ()
            let hasNamed (name : string) : bool =
                window.GetVisualDescendants()
                |> Seq.exists (function :? Control as c -> c.Name = name | _ -> false)
            Assert.True(hasNamed ConstructorUiId.chartWindowPlot.value, "the ScottPlot host control was not present")
            Assert.True(hasNamed ConstructorUiId.chartWindowElement.value, "the element picker was not present")
            Assert.True(hasNamed ConstructorUiId.chartWindowProperties.value, "the properties panel was not present")
            Assert.True(hasNamed ConstructorUiId.chartWindowPolar.value, "the polar toggle was not present for an angular chart")
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the ChartWindow polar toggle and axis editing run without throwing`` () =
        HeadlessSession.run (fun () ->
            let window = ChartWindow(sampleChart)
            try window.Show() with _ -> ()
            try Dispatcher.UIThread.RunJobs() with _ -> ()
            let ctrl (name : string) : Control option =
                window.GetVisualDescendants() |> Seq.tryPick (function :? Control as c when c.Name = name -> Some c | _ -> None)
            // Toggle polar ON (exercises the PolarAxis + GetCoordinates rebuild) and back to XY.
            match ctrl ConstructorUiId.chartWindowPolar.value with
            | Some c ->
                let b = c :?> Button
                b.RaiseEvent(Avalonia.Interactivity.RoutedEventArgs(Button.ClickEvent))
                (try Dispatcher.UIThread.RunJobs() with _ -> ())
                b.RaiseEvent(Avalonia.Interactivity.RoutedEventArgs(Button.ClickEvent))
                (try Dispatcher.UIThread.RunJobs() with _ -> ())
            | None -> Assert.Fail("no polar toggle")
            // Pick the X axis and turn Auto off (exercises the axis panel + SetLimitsX path).
            match ctrl ConstructorUiId.chartWindowElement.value with
            | Some c -> (c :?> ComboBox).SelectedIndex <- 1
            | None -> Assert.Fail("no element selector")
            (try Dispatcher.UIThread.RunJobs() with _ -> ())
            match ctrl ConstructorUiId.chartWindowAxisAuto.value with
            | Some c -> (c :?> CheckBox).IsChecked <- System.Nullable false; (try Dispatcher.UIThread.RunJobs() with _ -> ())
            | None -> Assert.Fail("the X-axis panel (Auto checkbox) was not shown after selecting X axis")
            window.Close())

    // ============================ headless render proofs (ui-smoke) ============================

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the Experiments bay shows the intensity polyline for a varied polarizer`` () =
        HeadlessSession.run (fun () ->
            let baseModel = SceneTestFixture.mainScene () |> updateTableAndElementRotation (AddElement LinearPolarizer)
            let pid = idOf 2 baseModel
            let mutable model =
                baseModel
                |> updateTableAndElementRotation (ExpChooseElement (Library.ElementId pid))
                |> updateTableAndElementRotation (SelectBay BayNames.experiments)
            let dispatch (msg : TableAndElementRotationMsg) = model <- updateTableAndElementRotation msg model
            let window = Window(Width = 980.0, Height = tableAndElementRotationCanvasHeight + 360.0)
            window.Content <- Component(fun _ -> mainView model dispatch)
            window.Show()
            Dispatcher.UIThread.RunJobs()
            let chartVisible () : bool =
                window.GetVisualDescendants()
                |> Seq.exists (function :? Polyline as p when p.Name = ConstructorUiId.experimentChart.value && p.IsEffectivelyVisible -> true | _ -> false)
            Assert.True(chartVisible (), "the intensity polyline was not visible")
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the Experiments bay lists candidates and a click picks the element`` () =
        HeadlessSession.run (fun () ->
            let mutable model = SceneTestFixture.mainScene () |> updateTableAndElementRotation (SelectBay BayNames.experiments)
            let dispatch (msg : TableAndElementRotationMsg) = model <- updateTableAndElementRotation msg model
            let window = Window(Width = 980.0, Height = tableAndElementRotationCanvasHeight + 300.0)
            window.Content <- Component(fun _ -> mainView model dispatch)
            window.Show()
            Dispatcher.UIThread.RunJobs()
            let readoutVisible () : bool =
                window.GetVisualDescendants()
                |> Seq.exists (function :? TextBlock as t when t.Name = ConstructorUiId.experimentReadout.value && t.IsEffectivelyVisible -> true | _ -> false)
            Assert.True(readoutVisible (), "the Experiment readout was not visible")
            let candName = (ConstructorUiId.experimentCandidate (ConstructorUiKey "src")).value
            let findCand () : Border option =
                window.GetVisualDescendants()
                |> Seq.tryPick (function :? Border as b when matchesId candName b && b.IsEffectivelyVisible -> Some b | _ -> None)
            match findCand () with
            | None -> Assert.Fail("the src candidate was not visible in the Experiments bay")
            | Some b ->
                let c = b.TranslatePoint(Point(b.Bounds.Width / 2.0, b.Bounds.Height / 2.0), window)
                if c.HasValue then
                    window.MouseDown(c.Value, Avalonia.Input.MouseButton.Left, Avalonia.Input.RawInputModifiers.None)
                    Dispatcher.UIThread.RunJobs()
                    window.MouseUp(c.Value, Avalonia.Input.MouseButton.Left, Avalonia.Input.RawInputModifiers.None)
                    Dispatcher.UIThread.RunJobs()
                    Assert.Equal(Some "src", model.experimentCollection.draft.elementId |> Option.map (fun id -> id.value))
                else Assert.Fail("the candidate has no on-screen position")
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``clicking Add in the Experiments bay adds the experiment to the collection`` () =
        HeadlessSession.run (fun () ->
            let baseModel = SceneTestFixture.mainScene () |> updateTableAndElementRotation (AddElement LinearPolarizer)
            let pid = idOf 2 baseModel
            let mutable model =
                baseModel
                |> updateTableAndElementRotation (ExpChooseElement (Library.ElementId pid))
                |> updateTableAndElementRotation (SelectBay BayNames.experiments)
            let dispatch (msg : TableAndElementRotationMsg) = model <- updateTableAndElementRotation msg model
            let window = Window(Width = 980.0, Height = tableAndElementRotationCanvasHeight + 360.0)
            window.Content <- Component(fun _ -> mainView model dispatch)
            window.Show()
            Dispatcher.UIThread.RunJobs()
            let findAdd () : Border option =
                window.GetVisualDescendants()
                |> Seq.tryPick (function :? Border as b when matchesId ConstructorUiId.experimentAddButton.value b && b.IsEffectivelyVisible -> Some b | _ -> None)
            match findAdd () with
            | None -> Assert.Fail("the Add button was not visible")
            | Some b ->
                let c = b.TranslatePoint(Point(b.Bounds.Width / 2.0, b.Bounds.Height / 2.0), window)
                if c.HasValue then
                    window.MouseDown(c.Value, Avalonia.Input.MouseButton.Left, Avalonia.Input.RawInputModifiers.None)
                    Dispatcher.UIThread.RunJobs()
                    window.MouseUp(c.Value, Avalonia.Input.MouseButton.Left, Avalonia.Input.RawInputModifiers.None)
                    Dispatcher.UIThread.RunJobs()
                    Assert.Equal(1, List.length model.experimentCollection.experiments)
                else Assert.Fail("the Add button has no on-screen position")
            window.Close())

    let private liveComponent (seed : TableAndElementRotationModel) : Window =
        let comp =
            Component(fun ctx ->
                let st = ctx.useState seed
                mainView st.Current (fun msg -> st.Set(updateTableAndElementRotation msg st.Current)))
        Window(Width = 1000.0, Height = 980.0, Content = comp)

    let private clickIn (window : Window) (name : string) : unit =
        match window.GetVisualDescendants() |> Seq.tryPick (function :? Border as b when matchesId name b -> Some b | _ -> None) with
        | Some b ->
            match b.TranslatePoint(Point(b.Bounds.Width / 2.0, b.Bounds.Height / 2.0), window) with
            | v when v.HasValue ->
                window.MouseDown(v.Value, Avalonia.Input.MouseButton.Left, Avalonia.Input.RawInputModifiers.None)
                Dispatcher.UIThread.RunJobs()
                window.MouseUp(v.Value, Avalonia.Input.MouseButton.Left, Avalonia.Input.RawInputModifiers.None)
                Dispatcher.UIThread.RunJobs()
            | _ -> Assert.Fail($"%s{name} off-screen")
        | None -> Assert.Fail($"%s{name} not found")

    // ============================ FuncUI recycling regressions (spec 028) ============================
    // The real app re-renders `mainView` on EVERY message (Elmish), so FuncUI diffs the tree. A styled
    // Avalonia control cannot change its `Name`, so a named control recycled onto a different item's slot
    // throws "Cannot set Name : … already styled" (the reason the Ribbon keeps all panes present, and why
    // the bay's variable selector toggles visibility and its candidate / collection items carry a mutable
    // AutomationId). These drive the bay through a REAL re-rendering Component to guard those paths.

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``removing a collected experiment re-renders without a name-collision crash`` () =
        HeadlessSession.run (fun () ->
            let baseM = SceneTestFixture.mainScene () |> updateTableAndElementRotation (AddElement LinearPolarizer)
            let pid = idOf 2 baseM
            let seed =
                baseM
                |> updateTableAndElementRotation (ExpChooseElement (Library.ElementId pid)) |> updateTableAndElementRotation ExpCommit      // exp #1
                |> updateTableAndElementRotation ExpNew
                |> updateTableAndElementRotation (ExpChooseElement (Library.ElementId "src")) |> updateTableAndElementRotation ExpCommit     // exp #2
                |> updateTableAndElementRotation (SelectBay BayNames.experiments)
            let window = liveComponent seed
            window.Show()
            Dispatcher.UIThread.RunJobs()
            clickIn window (ConstructorUiId.experimentRemoveButton (ConstructorUiKey "1")).value   // remove the FIRST (list shifts)
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``removing a scene element re-renders the Experiments bay without a crash`` () =
        HeadlessSession.run (fun () ->
            // Elements: src(0), det(1), Sample(2), Polarizer(3). Select the sample and remove it (a MIDDLE
            // element) while the Experiments bay's candidate list is present.
            let seed =
                SceneTestFixture.mainScene ()
                |> updateTableAndElementRotation (AddElement Sample)
                |> updateTableAndElementRotation (AddElement LinearPolarizer)
                |> (fun m -> { m with selection = ElementSelected (SceneElementIndex 2) })
                |> updateTableAndElementRotation (SelectBay BayNames.experiments)
            let window = liveComponent seed
            window.Show()
            Dispatcher.UIThread.RunJobs()
            // The Remove control lives in the Add bay: switch to it and click Remove selected. The
            // Experiments pane (kept present by the ribbon) re-renders its candidate list on the change.
            clickIn window (ConstructorUiId.ribbonTab (ConstructorUiKey BayNames.add.value)).value
            clickIn window ConstructorUiId.paletteRemoveSelectedButton.value
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``switching the varied element's kind re-renders without a name-collision crash`` () =
        // Switching the chosen element between kinds changes the VARIABLE selector — a source's [wavelength]
        // box vs a polarizer's [r1] box at the same slot. Before the fix (fixed boxes toggled by visibility)
        // this recycled a named box into a differently-named one and threw.
        HeadlessSession.run (fun () ->
            let seed =
                SceneTestFixture.mainScene () |> updateTableAndElementRotation (AddElement LinearPolarizer)     // idx 2
                |> updateTableAndElementRotation (ExpChooseElement (Library.ElementId "src"))                     // source ⇒ variable boxes = [wavelength]
                |> updateTableAndElementRotation (SelectBay BayNames.experiments)
            let pid = idOf 2 seed
            let window = liveComponent seed
            window.Show()
            Dispatcher.UIThread.RunJobs()
            clickIn window (ConstructorUiId.experimentCandidate (ConstructorUiKey pid)).value     // → polarizer ([r1])
            clickIn window (ConstructorUiId.experimentCandidate (ConstructorUiKey "src")).value   // → source ([wavelength])
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``toggling the T-R-both capture (series count) re-renders without a crash`` () =
        HeadlessSession.run (fun () ->
            let seed =
                SceneTestFixture.mainScene () |> updateTableAndElementRotation (AddElement Sample)
                |> (fun m -> { m with selection = ElementSelected (SceneElementIndex 2) })
                |> updateTableAndElementRotation (BindValueId glassFilm200Id)
                |> (fun m -> m |> updateTableAndElementRotation (ExpChooseElement (Library.ElementId (idOf 2 m))))
                |> updateTableAndElementRotation (ExpChooseVariable ExperimentControls.VaryR2)
                |> updateTableAndElementRotation (ExpChooseMeasurement ExperimentControls.CaptureBoth)   // two series (T + R)
                |> updateTableAndElementRotation (SelectBay BayNames.experiments)
            let window = liveComponent seed
            window.Show()
            Dispatcher.UIThread.RunJobs()
            clickIn window (ConstructorUiId.experimentMeasurement (ConstructorUiKey "t")).value      // 2 series → 1
            clickIn window (ConstructorUiId.experimentMeasurement (ConstructorUiKey "both")).value   // 1 → 2
            clickIn window (ConstructorUiId.experimentMeasurement (ConstructorUiKey "r")).value      // 2 → 1
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``selecting a previously added experiment loads it for editing`` () =
        // The reported bug: clicking a previously added experiment did nothing (an Avalonia error swallowed
        // the click's re-render). Drive it through a live re-render and assert the edit actually takes.
        HeadlessSession.run (fun () ->
            let baseModel = SceneTestFixture.mainScene () |> updateTableAndElementRotation (AddElement LinearPolarizer)
            let pid = idOf 2 baseModel
            let seed =
                baseModel
                |> updateTableAndElementRotation (ExpChooseElement (Library.ElementId pid)) |> updateTableAndElementRotation ExpCommit          // exp #1 (polarizer)
                |> updateTableAndElementRotation ExpNew
                |> updateTableAndElementRotation (ExpChooseElement (Library.ElementId "src")) |> updateTableAndElementRotation ExpCommit         // exp #2 (source) → editing #2
                |> updateTableAndElementRotation (SelectBay BayNames.experiments)
            let latest = ref seed
            let comp =
                Component(fun ctx ->
                    let st = ctx.useState seed
                    mainView st.Current (fun msg ->
                        let m' = updateTableAndElementRotation msg st.Current
                        latest.Value <- m'
                        st.Set m'))
            let window = Window(Width = 1000.0, Height = 980.0, Content = comp)
            window.Show()
            Dispatcher.UIThread.RunJobs()
            clickIn window (ConstructorUiId.experimentEditButton (ConstructorUiKey "1")).value          // select the FIRST experiment
            Assert.Equal(Some 1, latest.Value.experimentCollection.draft.editingId |> Option.map (fun i -> i.value))
            // The editor loaded experiment #1's element (the polarizer), not the source it was on.
            Assert.Equal(Some pid, latest.Value.experimentCollection.draft.elementId |> Option.map (fun i -> i.value))
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``returning from polar to XY restores the cartesian axes and data-fit limits`` () =
        // Regression (spec 030): `Add.PolarAxis` hides the rectangular axes for a clean polar grid; switching
        // back must restore them (and the data-fit limits) or the XY view renders with no axes / ticks.
        HeadlessSession.run (fun () ->
            let window = ChartWindow(sampleChart)
            try window.Show() with _ -> ()
            try Dispatcher.UIThread.RunJobs() with _ -> ()
            let ctrl (name : string) : Control option =
                window.GetVisualDescendants() |> Seq.tryPick (function :? Control as c when c.Name = name -> Some c | _ -> None)
            let ava = (ctrl ConstructorUiId.chartWindowPlot.value |> Option.get) :?> ScottPlot.Avalonia.AvaPlot
            let lim0 = ava.Plot.Axes.GetLimits()
            let btn = (ctrl ConstructorUiId.chartWindowPolar.value |> Option.get) :?> Button
            let click () = btn.RaiseEvent(Avalonia.Interactivity.RoutedEventArgs(Button.ClickEvent)); (try Dispatcher.UIThread.RunJobs() with _ -> ())
            click ()   // → polar (hides the cartesian axes)
            click ()   // → XY (must restore them)
            let axisVisible (a : obj) : bool = match a with :? ScottPlot.AxisPanels.AxisBase as ax -> ax.IsVisible | _ -> false
            Assert.True(axisVisible ava.Plot.Axes.Bottom, "the X axis was not restored after leaving polar")
            Assert.True(axisVisible ava.Plot.Axes.Left, "the Y axis was not restored after leaving polar")
            let lim1 = ava.Plot.Axes.GetLimits()
            Assert.True(abs (lim0.Left - lim1.Left) < 1e-6 && abs (lim0.Right - lim1.Right) < 1e-6, "the XY x-limits were not restored")
            Assert.True(abs (lim0.Bottom - lim1.Bottom) < 1e-6 && abs (lim0.Top - lim1.Top) < 1e-6, "the XY y-limits were not restored")
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``flipping a series to the right axis moves its scatter to plot.Axes.Right`` () =
        // Spec 0033 (018) acceptance: rebuildPlot/applySeriesStyle assign each scatter's Axes.YAxis to the
        // native left or right axis from its SeriesStyle.axisSide. This drives the series panel's axis
        // picker for the SECOND series and asserts the scatters land on their sides (reference equality on
        // the axis objects — no pixels).
        HeadlessSession.run (fun () ->
            let window = ChartWindow(sampleChart)
            try window.Show() with _ -> ()
            try Dispatcher.UIThread.RunJobs() with _ -> ()
            let ctrl (name : string) : Control option =
                window.GetVisualDescendants() |> Seq.tryPick (function :? Control as c when c.Name = name -> Some c | _ -> None)
            let ava = (ctrl ConstructorUiId.chartWindowPlot.value |> Option.get) :?> ScottPlot.Avalonia.AvaPlot
            let scatters () : ScottPlot.Plottables.Scatter list =
                ava.Plot.GetPlottables()
                |> Seq.choose (function :? ScottPlot.Plottables.Scatter as s -> Some s | _ -> None)
                |> List.ofSeq
            Assert.True(
                scatters () |> List.forall (fun s -> obj.ReferenceEquals(s.Axes.YAxis, ava.Plot.Axes.Left)),
                "every series should START on the left axis")
            // The element picker: Header, X axis, Y left, Y right, Legend, series 0, series 1 → index 6.
            (ctrl ConstructorUiId.chartWindowElement.value |> Option.get :?> ComboBox).SelectedIndex <- 6
            (try Dispatcher.UIThread.RunJobs() with _ -> ())
            match ctrl ConstructorUiId.chartWindowSeriesAxis.value with
            | Some c -> (c :?> ComboBox).SelectedIndex <- 1     // Left → Right
            | None -> Assert.Fail("the series panel offered no axis-side picker")
            (try Dispatcher.UIThread.RunJobs() with _ -> ())
            match scatters () with
            | [ first; second ] ->
                Assert.True(obj.ReferenceEquals(second.Axes.YAxis, ava.Plot.Axes.Right), "the flipped series did not move to the right axis")
                Assert.True(obj.ReferenceEquals(first.Axes.YAxis, ava.Plot.Axes.Left), "the other series must stay on the left axis")
            | other -> Assert.Fail($"expected exactly two scatters, got %d{List.length other}")
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``ChartWindow constructs and really renders a live experiment chart`` () =
        // The double-click hook does `ChartWindow(model.experimentResult).Show()`. This drives that exact
        // path AND forces ScottPlot's real Skia rasterization (headless Avalonia never rasterizes), so a
        // construction- or render-time throw would surface here rather than silently failing in the app.
        HeadlessSession.run (fun () ->
            let m = SceneTestFixture.mainScene () |> updateTableAndElementRotation (AddElement LinearPolarizer)
            let pid = idOf 2 m
            let chart = (m |> updateTableAndElementRotation (ExpChooseElement (Library.ElementId pid))).experimentResult
            Assert.False(List.isEmpty chart.series, "precondition: the R1 intensity chart has a series")
            let window = ChartWindow(chart)
            try window.Show() with _ -> ()
            try Dispatcher.UIThread.RunJobs() with _ -> ()
            let ava = window.GetVisualDescendants() |> Seq.pick (function :? ScottPlot.Avalonia.AvaPlot as a -> Some a | _ -> None)
            let img = ava.Plot.GetImage(700, 500)
            Assert.True(img.GetImageBytes().Length > 0, "the chart rendered no image bytes")
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``dispatching OpenExperimentChartWindow actually constructs a ChartWindow`` () =
        // Proves the full host path — update → openChartWindowHook → ChartWindow — actually fires (the hook
        // is a mutable assigned by a module-level statement; this guards against it staying the no-op stub).
        HeadlessSession.run (fun () ->
            let m = SceneTestFixture.mainScene () |> updateTableAndElementRotation (AddElement LinearPolarizer)
            let pid = idOf 2 m
            let m1 = m |> updateTableAndElementRotation (ExpChooseElement (Library.ElementId pid))      // draft ⇒ a non-empty R1 intensity chart
            Assert.False(List.isEmpty (m1.experimentResult).series, "precondition: the draft chart has a series")
            let before = ChartWindow.ConstructedCount
            updateTableAndElementRotation OpenExperimentChartWindow m1 |> ignore
            try Dispatcher.UIThread.RunJobs() with _ -> ()
            Assert.True(ChartWindow.ConstructedCount > before, "OpenExperimentChartWindow did not open a ChartWindow (hook not firing)"))

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the per-experiment View button opens a ChartWindow`` () =
        // The user's requested design: a "View" action on each collected experiment (next to Remove) opens
        // the chart window for THAT experiment; a row double-click calls the same action. This drives the
        // full path button → dispatch → update → viewExperimentHook → ChartWindow.
        HeadlessSession.run (fun () ->
            let baseM = SceneTestFixture.mainScene () |> updateTableAndElementRotation (AddElement LinearPolarizer)
            let pid = idOf 2 baseM
            let seed =
                baseM
                |> updateTableAndElementRotation (ExpChooseElement (Library.ElementId pid)) |> updateTableAndElementRotation ExpCommit          // add the experiment
                |> updateTableAndElementRotation (SelectBay BayNames.experiments)
            let expIdStr = string (List.head seed.experimentCollection.experiments).id.value
            let mutable model = seed
            let dispatch (msg : TableAndElementRotationMsg) = model <- updateTableAndElementRotation msg model
            let window = Window(Width = 1000.0, Height = 980.0)
            window.Content <- Component(fun _ -> mainView model dispatch)
            window.Show()
            Dispatcher.UIThread.RunJobs()
            let before = ChartWindow.ConstructedCount
            clickIn window (ConstructorUiId.experimentViewButton (ConstructorUiKey expIdStr)).value
            try Dispatcher.UIThread.RunJobs() with _ -> ()
            Assert.True(ChartWindow.ConstructedCount > before, "the View button did not open a ChartWindow")
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``there is no separate Open chart window button; a collected experiment's View opens its chart`` () =
        // Spec 0046 (operator): "Open chart window" duplicated a collected experiment's "View" — removed. The
        // draft's inline chart still opens its pop-out on a double-click (`openChartWindow`).
        HeadlessSession.run (fun () ->
            let handlers : ExperimentControls.ExperimentControlsHandlers =
                {
                    chooseElement = ignore; chooseVariable = ignore; chooseMeasurement = ignore
                    setRangeMin = ignore; setRangeMax = ignore; setRangePoints = ignore; setSourceWaveLength = ignore
                    addOrUpdate = ignore; newExperiment = ignore; editExperiment = ignore; removeExperiment = ignore
                    viewExperiment = ignore; openChartWindow = ignore
                }
            let state =
                { ExperimentControls.emptyExperimentControls with
                    availability = ControlOffered
                    series =
                        [
                            {
                                ExperimentControls.ChartSeries.name = ControlCaption "I"
                                points = [ { ExperimentControls.ChartPoint.x = 0.0; y = 1.0 }; { x = 1.0; y = 0.5 } ]
                            }
                        ]
                    xLabel = ControlCaption "x"; yLabel = ControlCaption "y" }
            let window = Window(Width = 420.0, Height = 560.0)
            window.Content <- Component(fun _ -> ExperimentControls.experimentControlsView state handlers)
            window.Show()
            Dispatcher.UIThread.RunJobs()
            let texts = window.GetVisualDescendants() |> Seq.choose (function :? TextBlock as t -> Option.ofObj t.Text | _ -> None) |> List.ofSeq
            Assert.DoesNotContain(texts, fun t -> t.StartsWith "Open chart")
            window.Close())
