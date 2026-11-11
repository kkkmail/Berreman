namespace OpticalConstructor.Tests

open System
open Xunit
open Berreman.Constants
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Placement
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.Experiments
open OpticalConstructor.Domain.ItemModels
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.Setups

/// Experiment capture over a setup revision, collection editing and the items a setup revision's slots hold
/// (spec 0027; spec 0038 Part I; spec 0050 S7.2, S8.2).
module ExperimentProxyTests =

    let private proxy = Experiments.createInMemoryExperiments ()
    let private el (s : string) : ElementId = Library.ElementId s

    // Test-local setup revisions an experiment references (spec 0050 S8.2).
    let private benchRevision1 : SetupReference = { name = SetupName "Bench"; revision = SetupRevision 1 }
    let private benchRevision2 : SetupReference = { name = SetupName "Bench"; revision = SetupRevision 2 }
    let private darkRevision1 : SetupReference = { name = SetupName "Dark"; revision = SetupRevision 1 }

    // ============================ variable capability (attached to the kind, as DATA) ============================

    [<Fact>]
    let ``variablesFor is data-driven per kind (source=λ, polarizer=R1, sample=R1+R2, others none)`` () =
        Assert.Equal<VariableParameter list>([ VaryWaveLength ], VariableParameter.allowedFor LightSource)
        Assert.Equal<VariableParameter list>([ VaryR1 ], VariableParameter.allowedFor LinearPolarizer)
        Assert.Equal<VariableParameter list>([ VaryR1 ], VariableParameter.allowedFor CircularPolarizer)
        Assert.Equal<VariableParameter list>([ VaryR1; VaryR2 ], VariableParameter.allowedFor Sample)
        Assert.Empty(VariableParameter.allowedFor Lens)
        Assert.Empty(VariableParameter.allowedFor FlatMirror)
        Assert.Empty(VariableParameter.allowedFor Detector)

    [<Fact>]
    let ``each variable carries a code, a label, and a display unit`` () =
        Assert.Equal("wavelength", VaryWaveLength.code)
        Assert.Equal("r1", VaryR1.code)
        Assert.Equal("r2", VaryR2.code)
        Assert.Equal("nm", VaryWaveLength.unitLabel)
        Assert.Equal("°", VaryR1.unitLabel)
        Assert.False(System.String.IsNullOrEmpty VaryR2.label)

    [<Fact>]
    let ``the default range depends on the variable`` () =
        Assert.Equal({ min = Propagation.SweptValue 0.0; max = Propagation.SweptValue 360.0; points = Propagation.CurvePointCount 73 }, VariableRange.forVariable VaryR1)
        Assert.Equal({ min = Propagation.SweptValue 0.0; max = Propagation.SweptValue 89.0; points = Propagation.CurvePointCount 91 }, VariableRange.forVariable VaryR2)
        Assert.Equal({ min = Propagation.SweptValue 200.0; max = Propagation.SweptValue 800.0; points = Propagation.CurvePointCount 91 }, VariableRange.forVariable VaryWaveLength)

    // ============================ measurement (T / R / both), defaulted from emission ============================

    [<Fact>]
    let ``MeasurementMode captures the right branches`` () =
        Assert.True(CaptureTransmitted.capturesTransmitted)
        Assert.False(CaptureTransmitted.capturesReflected)
        Assert.True(CaptureReflected.capturesReflected)
        Assert.False(CaptureReflected.capturesTransmitted)
        Assert.True(CaptureBoth.capturesTransmitted)
        Assert.True(CaptureBoth.capturesReflected)

    [<Fact>]
    let ``ofEmission defaults a mirror to R and everything else to both`` () =
        Assert.Equal(CaptureReflected, MeasurementMode.ofEmission EmitReflectedOnly)
        Assert.Equal(CaptureTransmitted, MeasurementMode.ofEmission EmitTransmittedOnly)
        Assert.Equal(CaptureBoth, MeasurementMode.ofEmission EmitBoth)
        // A freshly-placed mirror emits reflected only, so a varied mirror defaults to R (not hard-coded).
        Assert.Equal(CaptureReflected, MeasurementMode.ofEmission (FlatMirror.defaultEmission))
        Assert.Equal(CaptureBoth, MeasurementMode.ofEmission (Sample.defaultEmission))

    // ============================ spec 0050 S9.1: a run's role, session and notes ============================

    [<Fact>]
    let ``an experiment role round-trips its wire tag and renders its text`` () : unit =
        let roles = [ ScienceRun; CalibrationRun; DarkRun; DriftCheckRun; DiagnosticRun; IgnoredRun; PlannedRun ]
        Assert.Equal<int list>([ 1 .. 7 ], roles |> List.map (fun role -> role.value))
        Assert.Equal<ExperimentRole option list>(roles |> List.map Some, roles |> List.map (fun role -> ExperimentRole.tryOfWire role.value))
        Assert.Equal<ExperimentRole option>(None, ExperimentRole.tryOfWire 0)
        Assert.Equal<ExperimentRole option>(None, ExperimentRole.tryOfWire 8)
        Assert.Equal<string list>([ "Science"; "Calibration"; "Dark"; "Drift check"; "Diagnostic"; "Ignored"; "Planned" ], roles |> List.map (fun role -> role.text))
        Assert.Equal<ExperimentRole list>(roles, ExperimentRole.all)

    [<Fact>]
    let ``a session name keeps its text and refuses a blank one, and notes are free text`` () : unit =
        Assert.Equal<SessionName option>(Some (SessionName "Day 1 (morning)"), SessionName.tryCreate "Day 1 (morning)")
        Assert.Equal<SessionName option>(None, SessionName.tryCreate "")
        Assert.Equal<SessionName option>(None, SessionName.tryCreate "   ")
        Assert.Equal<string>("", ExperimentNotes.none.value)

    // ============================ the editable collection (choose / commit / edit / remove) ============================

    /// A collection with the source picked (λ variable, both capture allowed) — the common editing start.
    let private withSource () : ExperimentCollection =
        ExperimentCollection.empty
        |> chooseElement (el "src") (ElementLabel "Light source #1") (VariableParameter.allowedFor LightSource) CaptureTransmitted

    [<Fact>]
    let ``chooseElement defaults the variable to the first allowed and the range to its default`` () =
        let c = withSource ()
        Assert.Equal(Some (el "src"), c.draft.elementId)
        Assert.Equal(Some VaryWaveLength, c.draft.variable)
        Assert.Equal(VariableRange.forVariable VaryWaveLength, c.draft.range)
        Assert.True(c.canCommit)

    [<Fact>]
    let ``an element with nothing to vary leaves the variable unset and uncommittable`` () =
        let c = ExperimentCollection.empty |> chooseElement (el "lens") (ElementLabel "Lens #1") (VariableParameter.allowedFor Lens) CaptureTransmitted
        Assert.Equal(None, c.draft.variable)
        Assert.False(c.canCommit)

    [<Fact>]
    let ``chooseVariable resets the range to the new variable's default`` () =
        let c =
            ExperimentCollection.empty
            |> chooseElement (el "s") (ElementLabel "Sample #1") (VariableParameter.allowedFor Sample) CaptureBoth
            |> chooseVariable VaryR2
        Assert.Equal(Some VaryR2, c.draft.variable)
        Assert.Equal(VariableRange.forVariable VaryR2, c.draft.range)

    [<Fact>]
    let ``commit APPENDS a new experiment and then keeps editing it (no duplicate on re-commit)`` () =
        let c0 = withSource ()
        let c1 = commit benchRevision1 c0
        Assert.Equal(1, List.length c1.experiments)
        let added = List.head c1.experiments
        Assert.Equal(Some (el "src"), added.varied |> Option.map (fun v -> v.elementId))
        Assert.Equal(Some VaryWaveLength, added.varied |> Option.map (fun v -> v.variable))
        // The draft now edits the added experiment, so committing again UPDATES rather than appends.
        let c2 = commit benchRevision1 c1
        Assert.Equal(1, List.length c2.experiments)

    [<Fact>]
    let ``editing the draft after add updates the stored experiment in place`` () =
        let c1 = withSource () |> commit benchRevision1
        let id = (List.head c1.experiments).id
        let c2 = c1 |> chooseMeasurement CaptureReflected |> setRangeMin (Propagation.SweptValue 300.0) |> setRangeMax (Propagation.SweptValue 500.0) |> commit benchRevision1
        Assert.Equal(1, List.length c2.experiments)
        let updated = c2.experiments |> List.find (fun e -> e.id = id)
        Assert.Equal(CaptureReflected, updated.measurement)
        Assert.Equal(300.0, updated.range.min.value)
        Assert.Equal(500.0, updated.range.max.value)

    [<Fact>]
    let ``newDraft after add lets a second commit APPEND a distinct experiment`` () =
        let c1 = withSource () |> commit benchRevision1
        let c2 =
            c1
            |> (fun c -> c.newDraft)
            |> chooseElement (el "pol") (ElementLabel "Linear polarizer #2") (VariableParameter.allowedFor LinearPolarizer) CaptureBoth
            |> commit benchRevision2
        Assert.Equal(2, List.length c2.experiments)
        let ids = c2.experiments |> List.map (fun e -> e.id.value)
        Assert.Equal(List.length ids, ids |> List.distinct |> List.length)   // distinct ids

    [<Fact>]
    let ``edit loads an experiment into the draft for re-editing`` () =
        let c1 =
            withSource () |> commit benchRevision1
            |> (fun c -> c.newDraft)
            |> chooseElement (el "pol") (ElementLabel "Linear polarizer #2") (VariableParameter.allowedFor LinearPolarizer) CaptureTransmitted
            |> commit benchRevision2
        let target = c1.experiments |> List.find (fun e -> (e.varied |> Option.map (fun v -> v.elementId)) = Some (el "pol"))
        let c2 = edit target.id c1
        Assert.Equal(Some target.id, c2.draft.editingId)
        Assert.Equal(Some (el "pol"), c2.draft.elementId)
        Assert.Equal(Some VaryR1, c2.draft.variable)

    [<Fact>]
    let ``remove deletes the experiment and clears the draft when it was being edited`` () =
        let c1 = withSource () |> commit benchRevision1
        let id = (List.head c1.experiments).id
        let c2 = remove id c1
        Assert.Empty(c2.experiments)
        // It was the one being edited, so the draft resets (editingId cleared).
        Assert.Equal(None, c2.draft.editingId)
        Assert.Equal(None, c2.draft.elementId)

    [<Fact>]
    let ``an experiment description mentions the element, the variable, and the capture`` () =
        let c1 = withSource () |> chooseMeasurement CaptureBoth |> commit benchRevision1
        let exp = List.head c1.experiments
        Assert.Contains("Light source", exp.description)
        Assert.Contains("Wavelength", exp.description)
        Assert.Contains("Both", exp.description)

    [<Fact>]
    let ``setRangePoints keeps at least two points`` () =
        let c = withSource () |> setRangePoints (Propagation.CurvePointCount 1)
        Assert.True(c.draft.range.points.value >= 2)

    // ============================ E1 / E2 / E3 over their setup revisions ============================

    /// E1 = source + polarizer + sample + rotating polarizer + detector on `Bench` revision 1, the rotating polarizer varied.
    let private e1 : Experiment =
        {
            id = ExperimentId 1
            setup = benchRevision1
            varied = Some { elementId = el "lp2"; label = ElementLabel "Linear polarizer #4"; variable = VaryR1 }
            measurement = CaptureTransmitted
            range = VariableRange.forVariable VaryR1
            dataFileOpt = None
            sourceWaveLengthOpt = None
        }

    /// E2 = the same WITHOUT a sample, on `Bench` revision 2 (captures that the polarizers are not ideal).
    let private e2 : Experiment =
        { e1 with
            id = ExperimentId 2
            setup = benchRevision2
            varied = Some { elementId = el "lp2"; label = ElementLabel "Linear polarizer #3"; variable = VaryR1 } }

    /// E3 = an empty dark line (nothing varied) — how much ambient light reaches the detector with no source.
    let private e3 : Experiment =
        { id = ExperimentId 3; setup = darkRevision1; varied = None; measurement = CaptureTransmitted; range = VariableRange.forVariable VaryR1; dataFileOpt = None; sourceWaveLengthOpt = None }

    [<Fact>]
    let ``E1 references its setup revision and names its varied polarizer with its label`` () =
        Assert.Equal<SetupReference>(benchRevision1, e1.setup)
        let expectedVaried : VariedElement = { elementId = el "lp2"; label = ElementLabel "Linear polarizer #4"; variable = VaryR1 }
        Assert.Equal<VariedElement option>(Some expectedVaried, e1.varied)
        Assert.Equal(ElementLabel "Linear polarizer #4", e1.variedLabel)

    [<Fact>]
    let ``E3 is an empty dark line — nothing varied, still describable`` () =
        Assert.Equal<SetupReference>(darkRevision1, e3.setup)
        Assert.Equal(None, e3.varied)
        Assert.Equal(ElementLabel.blank, e3.variedLabel)
        Assert.Contains("Dark line", e3.description)
        Assert.Contains("'Dark' revision 1", e3.description)

    [<Fact>]
    let ``E1 and E2 describe their varied element, variable and capture`` () =
        Assert.Contains("Linear polarizer #4", e1.description)
        Assert.Contains("Rotation R1", e1.description)
        Assert.Contains("Transmitted", e1.description)
        Assert.Contains("Linear polarizer #3", e2.description)

    [<Fact>]
    let ``attachDataFile binds one data attachment per experiment`` () =
        let attachment : DataAttachment =
            {
                path = KitRelativePath "data/run1.csv"
                contentHash = ContentHash "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"
            }
        Assert.Equal("data/run1.csv", attachment.path.value)
        let c = withSource () |> commit benchRevision1
        let id = (List.head c.experiments).id
        Assert.Equal(None, (List.head c.experiments).dataFileOpt)
        let c2 = attachDataFile id attachment c
        let exp = c2.experiments |> List.find (fun e -> e.id = id)
        Assert.Equal(Some attachment, exp.dataFileOpt)

    /// One copy into a kit holding `taken`, and the experiments it must answer (spec 0050 S6).
    type private RenumberingCase =
        {
            taken : ExperimentId list
            incoming : Experiment list
            expected : Experiment list
        }

    let private numbered (experiment : Experiment) (id : ExperimentId) : Experiment =
        {
            experiment with id = id
        }

    [<Fact>]
    let ``copied experiments keep their ids unless the kit holds them, and a clash takes the next free id`` () : unit =
        let unnumbered (experiments : Experiment list) : Experiment list =
            experiments |> List.map (fun experiment -> numbered experiment (ExperimentId 0))
        let cases : RenumberingCase list =
            [
                {
                    taken = [ ExperimentId 1; ExperimentId 2 ]
                    incoming = [ numbered e1 (ExperimentId 2); numbered e2 (ExperimentId 3) ]
                    expected = [ numbered e1 (ExperimentId 4); numbered e2 (ExperimentId 3) ]
                }
                {
                    taken = []
                    incoming = [ numbered e1 (ExperimentId 2); numbered e2 (ExperimentId 3) ]
                    expected = [ numbered e1 (ExperimentId 2); numbered e2 (ExperimentId 3) ]
                }
                {
                    taken = [ ExperimentId 5 ]
                    incoming = [ numbered e1 (ExperimentId 1); numbered e2 (ExperimentId 5); numbered e3 (ExperimentId 2) ]
                    expected = [ numbered e1 (ExperimentId 1); numbered e2 (ExperimentId 6); numbered e3 (ExperimentId 2) ]
                }
                // An id given to an earlier incoming experiment is taken, and no later incoming id is handed out.
                {
                    taken = []
                    incoming = [ numbered e1 (ExperimentId 1); numbered e2 (ExperimentId 1); numbered e3 (ExperimentId 2) ]
                    expected = [ numbered e1 (ExperimentId 1); numbered e2 (ExperimentId 3); numbered e3 (ExperimentId 2) ]
                }
            ]
        for renumbering in cases do
            let renumbered = Experiment.renumberedFor renumbering.taken renumbering.incoming
            Assert.Equal<Experiment list>(renumbering.expected, renumbered)
            Assert.Equal<Experiment list>(unnumbered renumbering.incoming, unnumbered renumbered)
        Assert.Empty(Experiment.renumberedFor [ ExperimentId 1 ] [])

    // ============================ spec 0050 S7.2, S8.2: the physical items an experiment uses ============================

    /// The draft's item, built from the shipped item model's resolved version, failing when `addTo` refuses it.
    let private itemOf (draft : ItemDraft) : Item =
        match draft.addTo [] with
        | Ok item -> item
        | Error error -> failwith $"'{draft.label.value}' was refused: {error.describe}"

    let private glassPlateVersion : SampleVersionId = SampleVersionId.firstOf SampleIds.glassPlate1mmId

    /// The Built-in polarizer preset of that id, failing when the catalogue holds another kind under it.
    let private polarizerPresetOf (id : ElementValueId) : PolarizerPreset =
        match BuiltinCatalogue.preset id with
        | PolarizerItem polarizer -> polarizer
        | SampleItem _ | SourceItem _ | DetectorItem _ -> failwith $"The Built-in preset '{id.value}' is not a polarizer."

    let private laserItem : Item =
        let draft : ItemDraft =
            {
                id = ItemId (Guid.Parse "5d3b8a51-0c47-4b8e-9f2a-6e1d7c90b4a3")
                label = ItemLabel "Laser"
                model = SourcePresetChoice (BuiltinCatalogue.itemModel ItemModelIds.sourcePresetItemModelId, BuiltinCatalogue.source PresetIds.heNe633)
                dial = FixedMounting
            }

        itemOf draft

    /// A glass plate on the stack-sample item model at the plate's first version: QZ-A and QZ-B share both.
    let private plateDraft (id : ItemId) (label : ItemLabel) : ItemDraft =
        {
            id = id
            label = label
            model = StackSampleChoice (BuiltinCatalogue.itemModel ItemModelIds.stackSampleItemModelId, glassPlateVersion, BuiltinCatalogue.sample SampleIds.glassPlate1mmId)
            dial = FixedMounting
        }

    let private qzAItem : Item = itemOf (plateDraft (ItemId (Guid.Parse "a0c6f3d2-71b4-4e0f-8d5a-3b9e2c1f6a70")) (ItemLabel "QZ-A"))

    /// QZ-B is another piece of QZ-A's model and sample version, with its own measured parameters.
    let private qzBItem : Item =
        let drafted = itemOf (plateDraft (ItemId (Guid.Parse "b7e2d9c4-58a1-4f63-a0b8-9c4d1e2f3a85")) (ItemLabel "QZ-B"))
        {
            drafted with
                parameters = drafted.parameters |> List.map (fun parameter -> { parameter with state = KnownParameter (ParameterValue 1.05e-3) })
        }

    let private cplItem : Item =
        let draft : ItemDraft =
            {
                id = ItemId (Guid.Parse "c3f81e6a-9d24-4b57-b0e1-7a2c5d8f4b96")
                label = ItemLabel "CPL"
                model = LibraryModelChoice (BuiltinCatalogue.itemModel ItemModelIds.circularPolarizerModelId)
                dial = DialMounted DialPositive
            }

        itemOf draft

    /// A main-beam slot of that kind at `x` metres along the table, bound to `valueIdOpt`, holding `content`.
    let private slotAt
        (id : string)
        (kind : CatalogueKind)
        (x : float<meter>)
        (valueIdOpt : ElementValueId option)
        (content : SlotContent)
        : SetupSlot =
        {
            id = el id
            placement =
                {
                    ElementPlacement.create kind { x = x; y = 0.0<meter> } with valueId = valueIdOpt
                }
            zoom = BeamLayout.ElementZoom 1.0
            beam = BeamLayout.OnMainBeam
            content = content
        }

    let private holdingItem (item : Item) : SlotContent = ItemInSlot (item.id, PlacementTurn)

    /// A setup at its first revision holding these slots.
    let private setupOf (name : string) (slots : SetupSlot list) : Setup =
        {
            name = SetupName name
            revision = SetupRevision.first
            slots = slots
            history = []
        }

    [<Fact>]
    let ``the items an experiment uses are those its setup revision's slots hold, each once`` () : unit =
        let items = [ laserItem; qzAItem; qzBItem; cplItem ]
        Assert.Equal<ItemModelInstance>(qzAItem.model, qzBItem.model)
        Assert.Equal<ItemModelInstance>(SampleInstance ((BuiltinCatalogue.itemModel ItemModelIds.stackSampleItemModelId).versionId, glassPlateVersion), qzBItem.model)
        Assert.NotEqual<ItemId>(qzAItem.id, qzBItem.id)
        Assert.NotEqual<Parameter list>(qzAItem.parameters, qzBItem.parameters)

        // `Bench` revision 2 holds Laser at the source and QZ-B at the sample; its revision 1 bound the same entries
        // with no piece in any slot.
        let boundSlots (laserContent : SlotContent) (plateContent : SlotContent) : SetupSlot list =
            [
                slotAt "src" LightSource -1.0<meter> (Some PresetIds.heNe633) laserContent
                slotAt "qz" Sample -0.5<meter> (Some SampleIds.glassPlate1mmId.toElementValueId) plateContent
                slotAt "lp" LinearPolarizer -0.25<meter> (Some PresetIds.polarizerLp) EmptySlotContent
                slotAt "det" Detector 0.5<meter> (Some PresetIds.detectorIntensity) EmptySlotContent
            ]
        let bench : Setup =
            {
                name = SetupName "Bench"
                revision = SetupRevision 2
                slots = boundSlots (holdingItem laserItem) (holdingItem qzBItem)
                history = [ { revision = SetupRevision 1; slots = boundSlots EmptySlotContent EmptySlotContent } ]
            }
        let twice =
            setupOf "Twice" [ slotAt "qz" Sample -0.5<meter> None (holdingItem qzBItem); slotAt "qz-again" Sample 0.0<meter> None (holdingItem qzBItem) ]
        let circular = setupOf "Circular" [ slotAt "cpl" CircularPolarizer -0.5<meter> None (holdingItem cplItem) ]
        let dark = setupOf "Dark" []
        let setups = [ bench; twice; circular; dark ]

        let over (setup : SetupReference) (id : int) : Experiment =
            {
                e3 with
                    id = ExperimentId id
                    setup = setup
            }

        let onBench = over bench.reference 21

        // QZ-A shares QZ-B's model, sample version and binding, and is still not used.
        Assert.Equal<Result<Item list, SetupError>>(Ok [ laserItem; qzBItem ], Experiment.itemsUsedBy [ onBench ] setups items)

        let again = over twice.reference 22
        Assert.Equal<Result<Item list, SetupError>>(Ok [ qzBItem ], Experiment.itemsUsedBy [ again ] setups items)
        // Two experiments over the bench name Laser once.
        Assert.Equal<Result<Item list, SetupError>>(Ok [ laserItem; qzBItem ], Experiment.itemsUsedBy [ onBench; over bench.reference 25 ] setups items)
        Assert.Equal<Result<Item list, SetupError>>(Ok [ laserItem; qzBItem ], Experiment.itemsUsedBy [ again; onBench ] setups items)
        // The items come in the order they are supplied, not the order the slots hold them.
        Assert.Equal<Result<Item list, SetupError>>(
            Ok [ qzBItem; laserItem ], Experiment.itemsUsedBy [ onBench ] setups [ cplItem; qzBItem; qzAItem; laserItem ])

        // E1's revision binds the plate and the presets the items bind, yet holds no piece, so it uses none.
        Assert.Equal<Result<Item list, SetupError>>(Ok [], Experiment.itemsUsedBy [ e1; e3 ] setups items)
        Assert.Equal<Result<Item list, SetupError>>(Ok [], Experiment.itemsUsedBy [] setups items)

        // A standalone item model binds no preset or sample and is used by its id alone.
        Assert.Equal<EntryReference option>(None, cplItem.model.bindingReferenceOpt)
        let onCircular = over circular.reference 23
        Assert.Equal<Result<Item list, SetupError>>(Ok [ cplItem ], Experiment.itemsUsedBy [ onCircular ] setups items)
        Assert.Equal<Result<Item list, SetupError>>(Ok [ laserItem; qzBItem; cplItem ], Experiment.itemsUsedBy [ onCircular; onBench ] setups items)

        // A setup or revision the list lacks, or a piece the items lack, is its typed error, never an empty list.
        match Experiment.itemsUsedBy [ onBench; over { name = SetupName "Gone"; revision = SetupRevision 1 } 26 ] setups items with
        | Error (UnknownSetupErr reason) -> Assert.Contains("'Gone' revision 1", reason)
        | other -> Assert.Fail($"expected UnknownSetupErr, got %A{other}")
        match Experiment.itemsUsedBy [ over benchRevision1 27; over { bench.reference with revision = SetupRevision 3 } 28 ] setups items with
        | Error (UnknownSetupRevisionErr reason) -> Assert.Contains("'Bench' revision 3", reason)
        | other -> Assert.Fail($"expected UnknownSetupRevisionErr, got %A{other}")
        match Experiment.itemsUsedBy [ onBench ] setups [ qzBItem ] with
        | Error (SetupItemReferenceErr (UnknownKitItemErr reason)) -> Assert.Contains(laserItem.id.value.ToString(), reason)
        | other -> Assert.Fail($"expected the missing Laser, got %A{other}")

    [<Fact>]
    let ``a missing physical item is refused instead of guessed from a shared preset`` () : unit =
        let heldPolarizer =
            let draft : ItemDraft =
                {
                    id = ItemId (Guid.Parse "d94a0b17-2e6c-4f85-a3d9-1b7e8c2f5a04")
                    label = ItemLabel "LP1"
                    model = PolarizerPresetChoice (BuiltinCatalogue.itemModel ItemModelIds.idealPolarizerItemModelId, polarizerPresetOf PresetIds.polarizerLp)
                    dial = DialMounted DialNegative
                }

            itemOf draft

        let missing = ItemId (Guid.Parse "e15c7a93-4b08-4d2f-8e6a-5c9b3d1f7e28")
        let polarizerBinding = Some PresetIds.polarizerLp
        Assert.Equal<ItemModelInstance>(PresetInstance ((BuiltinCatalogue.itemModel ItemModelIds.idealPolarizerItemModelId).versionId, PresetIds.polarizerLp), heldPolarizer.model)

        // Both polarizer slots bind the one preset; only the held piece's id tells the second apart.
        let bench =
            setupOf
                "Shared preset"
                [
                    slotAt "src" LightSource -1.0<meter> (Some PresetIds.heNe633) (holdingItem laserItem)
                    slotAt "lp" LinearPolarizer -0.5<meter> polarizerBinding (ItemInSlot (missing, PlacementTurn))
                    slotAt "lp-held" LinearPolarizer -0.25<meter> polarizerBinding (holdingItem heldPolarizer)
                ]
        let dangling =
            {
                e3 with
                    id = ExperimentId 24
                    setup = bench.reference
            }

        match Experiment.itemsUsedBy [ dangling ] [ bench ] [ laserItem; heldPolarizer; qzAItem ] with
        | Error (SetupItemReferenceErr (UnknownKitItemErr reason)) ->
            Assert.Contains(missing.value.ToString(), reason)
            Assert.Contains("'Prep 1' (lp)", reason)
            Assert.Contains("E24", reason)
            Assert.DoesNotContain(heldPolarizer.id.value.ToString(), reason)
        | other -> Assert.Fail($"expected UnknownKitItemErr for the missing piece, got %A{other}")

    // ============================ committing over the active setup revision ============================

    [<Fact>]
    let ``committing references the setup revision and the varied slot with its label`` () =
        // The bay picks the rotating polarizer to vary and commits over the table's setup revision.
        let c =
            ExperimentCollection.empty
            |> chooseElement (el "lp2") (ElementLabel "Linear polarizer #4") (VariableParameter.allowedFor LinearPolarizer) CaptureTransmitted
            |> commit benchRevision2
        Assert.Equal(1, List.length c.experiments)
        let exp = List.head c.experiments
        Assert.Equal<SetupReference>(benchRevision2, exp.setup)
        let expectedVaried : VariedElement = { elementId = el "lp2"; label = ElementLabel "Linear polarizer #4"; variable = VaryR1 }
        Assert.Equal<VariedElement option>(Some expectedVaried, exp.varied)
        Assert.Equal(ElementLabel "Linear polarizer #4", exp.variedLabel)

    [<Fact>]
    let ``committing the draft over setups appends one experiment over each, and an incomplete draft appends none`` () : unit =
        // Test-local setup revisions of a grid (spec 0050 S8.3).
        let a1 : SetupReference = { name = SetupName "A"; revision = SetupRevision 1 }
        let b2 : SetupReference = { name = SetupName "B"; revision = SetupRevision 2 }
        let attachment : DataAttachment =
            {
                path = KitRelativePath "data/e1.csv"
                contentHash = ContentHash "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"
            }
        // E1 holds a data file, so a new experiment holding none shows the draft's edited experiment lends it nothing.
        let held : Experiment = { e1 with dataFileOpt = Some attachment }
        let editing =
            ExperimentCollection.ofLoaded [ held ]
            |> edit held.id
            |> setRangeMax (Propagation.SweptValue 90.0)
        Assert.Equal(Some held.id, editing.draft.editingId)
        let range : VariableRange =
            {
                min = Propagation.SweptValue 0.0
                max = Propagation.SweptValue 90.0
                points = Propagation.CurvePointCount 73
            }
        let over (setup : SetupReference) (id : ExperimentId) : Experiment =
            {
                id = id
                setup = setup
                varied = Some { elementId = el "lp2"; label = ElementLabel "Linear polarizer #4"; variable = VaryR1 }
                measurement = CaptureTransmitted
                range = range
                dataFileOpt = None
                sourceWaveLengthOpt = None
            }
        let committed = commitOverSetups [ a1; b2 ] editing
        Assert.Equal<Experiment list>([ held; over a1 (ExperimentId 2); over b2 (ExperimentId 3) ], committed.experiments)
        Assert.Equal<ExperimentDraft>({ editing.draft with editingId = None }, committed.draft)
        Assert.Equal(ExperimentId 4, committed.nextId)
        // The draft edits nothing, so a later commit appends.
        Assert.Equal(4, List.length (commit benchRevision1 committed).experiments)
        let incomplete = { editing with draft = ExperimentDraft.empty }
        Assert.Equal<ExperimentCollection>(incomplete, commitOverSetups [ a1; b2 ] incomplete)

    // ============================ the mock proxy seam ============================

    [<Fact>]
    let ``the in-memory experiments proxy lists no experiment`` () : unit =
        Assert.Equal<Result<Experiment list, ExperimentError>>(Ok [], proxy.listExperiments ())

    [<Fact>]
    let ``the in-memory experiments proxy resolves no experiment id`` () : unit =
        let firstId = ExperimentId 1
        let unknownId = ExperimentId 9999
        Assert.Equal<Result<Experiment option, ExperimentError>>(Ok None, proxy.tryGetExperiment firstId)
        Assert.Equal<Result<Experiment option, ExperimentError>>(Ok None, proxy.tryGetExperiment unknownId)

    [<Fact>]
    let ``a STUB proxy of the same shape drives the same listing logic`` () =
        let only : Experiment =
            {
                id = ExperimentId 7
                setup = benchRevision1
                varied = Some { elementId = el "stub-el"; label = ElementLabel "Stub"; variable = VaryR1 }
                measurement = CaptureTransmitted
                range = VariableRange.forVariable VaryR1
                dataFileOpt = None
                sourceWaveLengthOpt = None
            }
        let stub : ExperimentProxy =
            {
                listExperiments = fun () -> Ok [ only ]
                tryGetExperiment = fun i -> Ok (if i = ExperimentId 7 then Some only else None)
            }
        match stub.listExperiments () with
        | Ok [ s ] -> Assert.Equal(7, s.id.value)
        | other -> Assert.Fail($"%A{other}")
        match stub.tryGetExperiment (ExperimentId 7) with
        | Ok (Some s) -> Assert.Equal(Some (el "stub-el"), s.varied |> Option.map (fun v -> v.elementId))
        | other -> Assert.Fail($"%A{other}")
        match stub.tryGetExperiment (ExperimentId 0) with
        | Ok None -> ()
        | other -> Assert.Fail($"%A{other}")
