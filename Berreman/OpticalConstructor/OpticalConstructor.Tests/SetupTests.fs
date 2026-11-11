namespace OpticalConstructor.Tests

open System
open Xunit
open Berreman.Constants
open Berreman.Geometry
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Experiments
open OpticalConstructor.Domain.Placement
open OpticalConstructor.Domain.ItemModels
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.Lifecycle
open OpticalConstructor.Domain.Setups

/// A setup as Domain data: the slots the table draws, the revisions experiments reference and the one version rule
/// that mints them (spec 0050 S8.2; 001 §J).
module SetupTests =

    let private emptySlot (id : Library.ElementId) (kind : CatalogueKind) (point : TablePoint) : SetupSlot =
        {
            id = id
            placement = ElementPlacement.create kind point
            zoom = BeamLayout.ElementZoom 5.0
            beam = BeamLayout.OnMainBeam
            content = EmptySlotContent
        }

    let private src : SetupSlot = emptySlot (Library.ElementId "src") LightSource RayModel.defaultSourcePoint
    let private lp : SetupSlot = emptySlot (Library.ElementId "lp") LinearPolarizer { x = -0.3<meter>; y = 0.0<meter> }
    let private det : SetupSlot = emptySlot (Library.ElementId "det") Detector RayModel.defaultDetectorPoint

    let private itemA : ItemId = ItemId (Guid.Parse "3f0d5e7a-6c1b-4a8e-9d2f-1b7c4e5a6d01")
    let private itemB : ItemId = ItemId (Guid.Parse "8a2c9b14-5e3d-4f6a-b7c8-2d9e0f1a3b02")
    let private itemC : ItemId = ItemId (Guid.Parse "c5e7f912-0a4b-4d3c-8e1f-6a2b7c9d4e03")

    let private holding (item : ItemId) (turn : SlotTurn) (slot : SetupSlot) : SetupSlot =
        { slot with content = ItemInSlot (item, turn) }

    /// The slot turned to that R1 through the lock-respecting setter; `lp` turns freely.
    let private turnedTo (r1 : Angle) (slot : SetupSlot) : SetupSlot =
        { slot with placement = withR1 r1 slot.placement }

    let private historyRecord (revision : SetupRevision) (slots : SetupSlot list) : SetupRevisionRecord =
        {
            revision = revision
            slots = slots
        }

    let private createdOver (name : SetupName) (held : Setup list) : Setup =
        match Setup.create name [ src; lp; det ] held with
        | Ok setup -> setup
        | Error error -> failwith $"'{name.value}' was refused: {error.describe}"

    [<Fact>]
    let ``a slot lays out as the element it is, on its own beam, and names the item it holds`` () : unit =
        let expected : BeamLayout.LayoutElement =
            {
                id = lp.id
                placement = lp.placement
                host = lp.beam
                zoom = lp.zoom
            }

        Assert.Equal<BeamLayout.LayoutElement>(expected, lp.layoutElement)

        // A slot on a side beam is laid out on that beam, not the main one.
        let reflected = { lp with beam = BeamLayout.OnReflectedBeamOf src.id }
        Assert.Equal<BeamLayout.BeamHost>(BeamLayout.OnReflectedBeamOf src.id, reflected.layoutElement.host)

        Assert.Equal<ItemId option>(Some itemA, (ItemInSlot (itemA, PlacementTurn)).itemOpt)
        Assert.Equal<ItemId option>(Some itemA, (ItemInSlot (itemA, DialTurn (Angle.degree 30.0<deg>))).itemOpt)
        Assert.Equal<ItemId option>(None, EmptySlotContent.itemOpt)

    [<Fact>]
    let ``a setup answers its slots at its own revision and at each earlier one, and refuses a revision it never had`` () : unit =
        let r1Slots = [ src; det ]
        let r2Slots = [ src; lp; det ]
        let r3Slots = [ src; turnedTo (Angle.degree 30.0<deg>) lp; det ]

        let bench : Setup =
            {
                name = SetupName "Bench"
                revision = SetupRevision 3
                slots = r3Slots
                history = [ historyRecord (SetupRevision 1) r1Slots; historyRecord (SetupRevision 2) r2Slots ]
            }

        let expectedReference : SetupReference = { name = SetupName "Bench"; revision = SetupRevision 3 }
        Assert.Equal<SetupReference>(expectedReference, bench.reference)

        for revision, expected in [ SetupRevision 1, r1Slots; SetupRevision 2, r2Slots; SetupRevision 3, r3Slots ] do
            match bench.slotsAt revision with
            | Ok slots -> Assert.Equal<SetupSlot list>(expected, slots)
            | Error error -> Assert.Fail($"revision {revision.value} was refused: {error.describe}")

        match bench.slotsAt (SetupRevision 4) with
        | Error (UnknownSetupRevisionErr reason) -> Assert.Contains("'Bench' revision 4", reason)
        | Error error -> Assert.Fail($"expected an unknown revision: {error.describe}")
        | Ok slots -> Assert.Fail($"answered {slots.Length} slots at a revision 'Bench' never had")

    [<Fact>]
    let ``a setup lists each item its revisions hold once, in order`` () : unit =
        let setup : Setup =
            {
                name = SetupName "Bench"
                revision = SetupRevision 2
                slots = [ src; holding itemA PlacementTurn lp; holding itemB (DialTurn (Angle.degree 12.0<deg>)) det ]
                history = [ historyRecord SetupRevision.first [ src; holding itemB PlacementTurn lp; holding itemC PlacementTurn det ] ]
            }

        Assert.Equal<ItemId list>([ itemA; itemB; itemC ], setup.heldItems)

    [<Fact>]
    let ``a setup is created at its first revision, and a name whose file the kit holds is refused`` () : unit =
        match Setup.create (SetupName "Bench") [ src; lp; det ] [] with
        | Ok created ->
            Assert.Equal<SetupName>(SetupName "Bench", created.name)
            Assert.Equal<SetupRevision>(SetupRevision.first, created.revision)
            Assert.Equal<SetupSlot list>([ src; lp; det ], created.slots)
            Assert.Empty(created.history)
        | Error error -> Assert.Fail($"expected a new setup: {error.describe}")

        // On Windows `bench.setup-json` is the file `Bench.setup-json`, and `Bench: A` is stored as `Bench_ A.setup-json`.
        let sameFile : (SetupName * SetupName) list =
            [
                SetupName "Bench", SetupName "bench"
                SetupName "Bench: A", SetupName "Bench_ A"
            ]

        for name, heldName in sameFile do
            match Setup.create name [ src; det ] [ createdOver heldName [] ] with
            | Error (DuplicateSetupNameErr reason) ->
                Assert.Contains($"'{name.value}'", reason)
                Assert.Contains($"'{heldName.value}'", reason)
                Assert.Contains((SetupKitFile.fileNameOf heldName.fileStem).value, reason)
            | Error error -> Assert.Fail($"expected a duplicate setup name: {error.describe}")
            | Ok created -> Assert.Fail($"created '{created.name.value}' beside '{heldName.value}'")

        // A name stored under another file is created beside them.
        let held = [ createdOver (SetupName "bench") []; createdOver (SetupName "Bench_ A") [] ]

        match Setup.create (SetupName "Bench 2") [ src; det ] held with
        | Ok created -> Assert.Equal<SetupRevision>(SetupRevision.first, created.revision)
        | Error error -> Assert.Fail($"expected 'Bench 2' beside 'bench' and 'Bench_ A': {error.describe}")

    [<Fact>]
    let ``a setup's slots change in place until an experiment pins its revision, which then mints the next`` () : unit =
        let original = createdOver (SetupName "Bench") []
        let firstSlots = original.slots
        let turnedSlots = [ src; turnedTo (Angle.degree 30.0<deg>) lp; det ]
        let laterSlots = [ src; turnedTo (Angle.degree 45.0<deg>) lp; det ]
        Assert.NotEqual<SetupSlot list>(firstSlots, turnedSlots)

        for pin in [ VersionUnpinned; VersionPinned ] do
            Assert.Equal<Setup>(original, original.revisedWith firstSlots pin)

        let edited = original.revisedWith turnedSlots VersionUnpinned
        Assert.Equal<SetupRevision>(SetupRevision.first, edited.revision)
        Assert.Empty(edited.history)
        Assert.Equal<SetupSlot list>(turnedSlots, edited.slots)

        let minted = original.revisedWith turnedSlots VersionPinned
        Assert.Equal<SetupRevision>(SetupRevision 2, minted.revision)
        Assert.Equal<SetupSlot list>(turnedSlots, minted.slots)
        Assert.Equal<SetupRevisionRecord list>([ historyRecord SetupRevision.first firstSlots ], minted.history)

        let mintedAgain = minted.revisedWith laterSlots VersionPinned
        Assert.Equal<SetupRevision>(SetupRevision 3, mintedAgain.revision)
        Assert.Equal<SetupSlot list>(laterSlots, mintedAgain.slots)

        Assert.Equal<SetupRevisionRecord list>(
            [ historyRecord SetupRevision.first firstSlots; historyRecord (SetupRevision 2) turnedSlots ],
            mintedAgain.history
        )

    [<Fact>]
    let ``an experiment pins the revision it references, and its slots resolve through the kit's setups`` () : unit =
        let firstSlots = [ src; holding itemA PlacementTurn lp; det ]
        let currentSlots = [ src; holding itemB PlacementTurn lp; det ]
        let bench : Setup =
            {
                name = SetupName "Bench"
                revision = SetupRevision 2
                slots = currentSlots
                history = [ historyRecord SetupRevision.first firstSlots ]
            }
        let plate = createdOver (SetupName "Plate") [ bench ]
        let setups = [ bench; plate ]

        let over (setup : SetupReference) (id : int) : Experiments.Experiment =
            {
                id = Experiments.ExperimentId id
                name = (Experiments.ExperimentId id).defaultName
                setup = setup
                scan = Experiments.ExperimentScan.darkLine Experiments.CaptureTransmitted
                role = Experiments.ScienceRun
                sessionOpt = None
                notes = ExperimentNotes.none
                dataOpt = None
            }

        let onFirst = over { name = SetupName "Bench"; revision = SetupRevision.first } 1
        let onCurrent = over bench.reference 2
        Assert.Equal<Result<SetupSlot list, SetupError>>(Ok firstSlots, onFirst.slotsIn setups)
        Assert.Equal<Result<SetupSlot list, SetupError>>(Ok currentSlots, onCurrent.slotsIn setups)
        Assert.Equal<Result<SetupSlot list, SetupError>>(Ok plate.slots, (over plate.reference 3).slotsIn setups)

        match (over { name = SetupName "Gone"; revision = SetupRevision.first } 4).slotsIn setups with
        | Error (UnknownSetupErr reason) -> Assert.Contains("'Gone' revision 1", reason)
        | other -> Assert.Fail($"expected UnknownSetupErr, got %A{other}")

        match (over { bench.reference with revision = SetupRevision 3 } 5).slotsIn setups with
        | Error (UnknownSetupRevisionErr reason) -> Assert.Contains("'Bench' revision 3", reason)
        | other -> Assert.Fail($"expected UnknownSetupRevisionErr, got %A{other}")

        // Only an experiment over the current revision pins it; one over an earlier revision leaves it free to change.
        Assert.Equal<PinState>(VersionPinned, bench.pinFor [ onCurrent ])
        Assert.Equal<PinState>(VersionPinned, bench.pinFor [ onFirst; onCurrent ])
        Assert.Equal<PinState>(VersionUnpinned, bench.pinFor [ onFirst ])
        Assert.Equal<PinState>(VersionUnpinned, bench.pinFor [])
        Assert.Equal<PinState>(VersionUnpinned, plate.pinFor [ onCurrent ])

        // A held id the kit's items lack is the nested item error, naming the id, the slot and the revision.
        match Experiments.Experiment.itemsUsedBy [ onCurrent ] setups [] with
        | Error (SetupItemReferenceErr (UnknownKitItemErr reason) as error) ->
            Assert.Contains(itemB.value.ToString(), reason)
            Assert.Contains("(lp)", reason)
            Assert.Contains("'Bench' revision 2", reason)
            Assert.Equal<string>($"a slot's item could not be resolved: the kit has no item of that id: {reason}", error.describe)
        | other -> Assert.Fail($"expected the missing item, got %A{other}")

    let private on (beam : BeamLayout.BeamHost) (slot : SetupSlot) : SetupSlot = { slot with beam = beam }

    let private roled (role : SlotRole) (slot : SetupSlot) : RoledSlot = { role = role; slot = slot }

    let private lpA : SetupSlot = emptySlot (Library.ElementId "lpA") LinearPolarizer { x = -0.3<meter>; y = 0.0<meter> }
    let private lpB : SetupSlot = emptySlot (Library.ElementId "lpB") LinearPolarizer { x = 0.3<meter>; y = 0.0<meter> }

    let private sampleA : SetupSlot =
        let slot = emptySlot (Library.ElementId "sampleA") Sample { x = -0.1<meter>; y = 0.0<meter> }
        { slot with placement = { slot.placement with emission = EmitBoth } }

    let private sampleB : SetupSlot = emptySlot (Library.ElementId "sampleB") Sample { x = 0.1<meter>; y = 0.0<meter> }

    /// Both samples in series on the main beam, a polarizer before and after them, listed out of light order.
    let private inSeries : SetupSlot list = [ src; det; lpA; sampleB; sampleA; lpB ]

    [<Fact>]
    let ``a setup's slots take their roles in light order: the source, the preps, the objects in series, the analyzers, the detectors`` () : unit =
        let expected =
            [
                roled SourceSlotRole src
                roled (PrepSlotRole (SlotPosition 1)) lpA
                roled (ObjectSlotRole (SlotPosition 1)) sampleA
                roled (ObjectSlotRole (SlotPosition 2)) sampleB
                roled (AnalyzerSlotRole (SlotPosition 1)) lpB
                roled (DetectorSlotRole (SlotPosition 1)) det
            ]

        Assert.Equal<RoledSlot list>(expected, SetupSlot.inLightOrder inSeries)

        // A setup reads the same roles off its current slots.
        match Setup.create (SetupName "Bench") inSeries [] with
        | Ok setup -> Assert.Equal<RoledSlot list>(expected, setup.lightOrder)
        | Error error -> Assert.Fail($"expected a new setup: {error.describe}")

    [<Fact>]
    let ``an element behind an object, on its reflected ray included, is an analyzer, and without an object every optical element is a prep`` () : unit =
        let reflected = BeamLayout.OnReflectedBeamOf sampleA.id
        let polR = emptySlot (Library.ElementId "polR") LinearPolarizer { x = 0.2<meter>; y = 0.0<meter> } |> on reflected
        let detR = emptySlot (Library.ElementId "detR") Detector { x = 0.5<meter>; y = 0.0<meter> } |> on reflected

        // polR's path (src, lpA, sampleA) is three steps, lpB's four; detR's is four, det's five.
        let expected =
            [
                roled SourceSlotRole src
                roled (PrepSlotRole (SlotPosition 1)) lpA
                roled (ObjectSlotRole (SlotPosition 1)) sampleA
                roled (ObjectSlotRole (SlotPosition 2)) sampleB
                roled (AnalyzerSlotRole (SlotPosition 1)) polR
                roled (AnalyzerSlotRole (SlotPosition 2)) lpB
                roled (DetectorSlotRole (SlotPosition 1)) detR
                roled (DetectorSlotRole (SlotPosition 2)) det
            ]

        Assert.Equal<RoledSlot list>(expected, SetupSlot.inLightOrder (inSeries @ [ polR; detR ]))

        let lp1 = emptySlot (Library.ElementId "lp1") LinearPolarizer { x = -0.3<meter>; y = 0.0<meter> }
        let lp2 = emptySlot (Library.ElementId "lp2") LinearPolarizer { x = 0.2<meter>; y = 0.0<meter> }

        let expectedWithoutObject =
            [
                roled SourceSlotRole src
                roled (PrepSlotRole (SlotPosition 1)) lp1
                roled (PrepSlotRole (SlotPosition 2)) lp2
                roled (DetectorSlotRole (SlotPosition 1)) det
            ]

        Assert.Equal<RoledSlot list>(expectedWithoutObject, SetupSlot.inLightOrder [ src; lp2; det; lp1 ])

    [<Fact>]
    let ``a role renders its chip name`` () : unit =
        let chips : (SlotRole * string) list =
            [
                SourceSlotRole, "Source"
                PrepSlotRole (SlotPosition 2), "Prep 2"
                ObjectSlotRole (SlotPosition 1), "Obj 1"
                AnalyzerSlotRole (SlotPosition 3), "Analyzer 3"
                DetectorSlotRole (SlotPosition 1), "Detector 1"
            ]

        for role, chip in chips do
            Assert.Equal(chip, role.text)

    // ============================ spec 0050 S8.2: assigning and swapping a slot's item ============================

    let private sourceItemModel : ItemModelVersion = BuiltinCatalogue.itemModel ItemModelIds.sourcePresetItemModelId
    let private polarizerItemModel : ItemModelVersion = BuiltinCatalogue.itemModel ItemModelIds.idealPolarizerItemModelId
    let private stackItemModel : ItemModelVersion = BuiltinCatalogue.itemModel ItemModelIds.stackSampleItemModelId
    let private realLinearPolarizerModel : ItemModelVersion = BuiltinCatalogue.itemModel ItemModelIds.realLinearPolarizerModelId

    let private heNe : Library.SourcePreset = BuiltinCatalogue.source Library.PresetIds.heNe633
    let private glassPlate : Library.Sample = BuiltinCatalogue.sample Library.SampleIds.glassPlate1mmId

    let private linearPolarizer : Library.PolarizerPreset =
        match BuiltinCatalogue.preset Library.PresetIds.polarizerLp with
        | Library.PolarizerItem polarizer -> polarizer
        | Library.SampleItem _ | Library.SourceItem _ | Library.DetectorItem _ -> failwith "The Built-in LP preset is not a polarizer."

    let private heNeChoice : ItemModelChoice = SourcePresetChoice (sourceItemModel, heNe)
    let private linearPolarizerChoice : ItemModelChoice = PolarizerPresetChoice (polarizerItemModel, linearPolarizer)

    let private glassPlateChoice : ItemModelChoice =
        StackSampleChoice (stackItemModel, Library.SampleVersionId.firstOf Library.SampleIds.glassPlate1mmId, glassPlate)

    let private realLinearPolarizerChoice : ItemModelChoice = LibraryModelChoice realLinearPolarizerModel

    /// The draft's item, failing with the refusal's text when `addTo` refuses it.
    let private itemOf (id : ItemId) (label : ItemLabel) (model : ItemModelChoice) (dial : ItemDial) : Item =
        let draft : ItemDraft =
            {
                id = id
                label = label
                model = model
                dial = dial
            }

        match draft.addTo [] with
        | Ok item -> item
        | Error error -> failwith $"'{label.value}' was refused: {error.describe}"

    let private withDialZero (zero : Angle) (item : Item) : Item =
        match item.withParameterState ParameterName.dialZero (KnownParameter (ParameterValue zero.value)) with
        | Ok zeroed -> zeroed
        | Error error -> failwith $"'{item.label.value}' refused a dial zero: {error.describe}"

    let private fixedLp : Item =
        itemOf (ItemId (Guid.Parse "0b1c2d3e-4f5a-4b6c-8d7e-9f0a1b2c3d41")) (ItemLabel "LP1") linearPolarizerChoice FixedMounting

    /// LP2 sits on a dial that reads backwards, its zero at 10°.
    let private dialLp : Item =
        itemOf (ItemId (Guid.Parse "1c2d3e4f-5a6b-4c7d-9e8f-0a1b2c3d4e52")) (ItemLabel "LP2") linearPolarizerChoice (DialMounted DialNegative)
        |> withDialZero (Angle.degree 10.0<deg>)

    let private plate : Item =
        itemOf (ItemId (Guid.Parse "2d3e4f5a-6b7c-4d8e-af90-1b2c3d4e5f63")) (ItemLabel "QZ") glassPlateChoice FixedMounting

    let private realLp : Item =
        itemOf (ItemId (Guid.Parse "3e4f5a6b-7c8d-4e9f-b0a1-2c3d4e5f6a74")) (ItemLabel "LP3") realLinearPolarizerChoice FixedMounting

    let private laser : Item =
        itemOf (ItemId (Guid.Parse "4f5a6b7c-8d9e-4fa0-81b2-3d4e5f6a7b85")) (ItemLabel "Laser") heNeChoice FixedMounting

    /// The polarizer slot, unbound, at R1 30°.
    let private lp30 : SetupSlot = turnedTo (Angle.degree 30.0<deg>) lp

    let private assigned (item : Item) (model : ItemModelChoice) (slot : SetupSlot) : SetupSlot =
        match slot.withItem item model with
        | Ok held -> held
        | Error error -> failwith $"'{item.label.value}' did not fit '{slot.id.value}': {error.describe}"

    let private assertAngle (expected : Angle) (actual : Angle) : unit = Assert.Equal(expected.value, actual.value, 1e-12)

    /// The raw reading a slot records, failing when it holds no dial reading.
    let private readingOf (slot : SetupSlot) : Angle =
        match slot.content with
        | ItemInSlot (_, DialTurn reading) -> reading
        | ItemInSlot (_, PlacementTurn)
        | EmptySlotContent -> failwith $"'{slot.id.value}' records no dial reading"

    [<Fact>]
    let ``an item fits the slots its model may fill`` () : unit =
        let bound : (ItemModelChoice * CatalogueKind list * Library.LibraryEntry) list =
            [
                heNeChoice, [ LightSource ], Library.SourceItem heNe
                linearPolarizerChoice, [ LinearPolarizer ], Library.PolarizerItem linearPolarizer
                glassPlateChoice, [ Sample ], Library.SampleItem glassPlate
            ]

        for choice, kinds, entry in bound do
            Assert.Equal<CatalogueKind list>(kinds, choice.slotKinds)
            Assert.Equal<Library.LibraryEntry option>(Some entry, choice.libraryEntryOpt)

        let anyOptic = [ LinearPolarizer; CircularPolarizer; Sample ]

        let standalone : (ItemModelId * CatalogueKind list) list =
            [
                ItemModelIds.realLinearPolarizerModelId, [ LinearPolarizer ]
                ItemModelIds.retarderPlateModelId, anyOptic
                ItemModelIds.circularPolarizerModelId, [ LinearPolarizer; CircularPolarizer ]
                ItemModelIds.blackBoxMuellerModelId, [ Sample ]
                ItemModelIds.emptySlotModelId, anyOptic
                ItemModelIds.intensityDetectorModelId, [ Detector ]
                ItemModelIds.muellerPolarimeterModelId, [ Detector ]
                ItemModelIds.ellipsometerModelId, [ Detector ]
            ]

        for id, kinds in standalone do
            let choice = LibraryModelChoice (BuiltinCatalogue.itemModel id)
            Assert.Equal<CatalogueKind list>(kinds, choice.slotKinds)
            Assert.Equal<Library.LibraryEntry option>(None, choice.libraryEntryOpt)

    [<Fact>]
    let ``an item put in a slot binds the slot to its library entry, and a dial-mounted item keeps the drawn angle as its reading`` () : unit =
        // A fixed item turns with the placement, and the slot is bound to the LP preset as the table binds it.
        let expectedFixed : SetupSlot =
            {
                lp30 with
                    placement = { lp30.placement with valueId = Some Library.PresetIds.polarizerLp }
                    content = ItemInSlot (fixedLp.id, PlacementTurn)
            }

        Assert.Equal<Result<SetupSlot, SetupError>>(Ok expectedFixed, lp30.withItem fixedLp linearPolarizerChoice)

        // On a backwards dial zeroed at 10°, R1 30° reads 10° − 30° = −20°, and drawing that reading gives 30° back.
        let dialSlot = assigned dialLp linearPolarizerChoice lp30
        Assert.Equal<SetupSlot>({ expectedFixed with content = ItemInSlot (dialLp.id, DialTurn (readingOf dialSlot)) }, dialSlot)
        assertAngle (Angle.degree (-20.0<deg>)) (readingOf dialSlot)
        assertAngle (Angle.degree 30.0<deg>) (dialSlot.drawnWith [ dialLp ]).placement.r1

        // A bound sample takes the sample's supported emission, whatever the slot emitted before.
        let sampleSlot = emptySlot (Library.ElementId "sample") Sample { x = 0.0<meter>; y = 0.0<meter> }
        let reflecting = { sampleSlot with placement = { sampleSlot.placement with emission = EmitReflectedOnly } }
        let plateSlot = assigned plate glassPlateChoice reflecting
        Assert.Equal<ElementValueId option>(Some Library.SampleIds.glassPlate1mmId.toElementValueId, plateSlot.placement.valueId)
        Assert.Equal<Emission>(glassPlate.supportedEmission, plateSlot.placement.emission)
        Assert.NotEqual<Emission>(reflecting.placement.emission, plateSlot.placement.emission)
        Assert.Equal<SlotContent>(ItemInSlot (plate.id, PlacementTurn), plateSlot.content)

        // An item model binds no entry, so the slot keeps whatever it was bound to, or nothing.
        let boundLp30 = { lp30 with placement = { lp30.placement with valueId = Some Library.PresetIds.polarizerLp } }

        for slot in [ lp30; boundLp30 ] do
            let realSlot = assigned realLp realLinearPolarizerChoice slot
            Assert.Equal<SetupSlot>({ slot with content = ItemInSlot (realLp.id, PlacementTurn) }, realSlot)

        match lp30.withItem laser heNeChoice with
        | Error (IncompatibleSlotItemErr reason) ->
            Assert.Contains("'Laser'", reason)
            Assert.Contains("linear polarizer", reason)
        | Error error -> Assert.Fail($"expected an incompatible slot item: {error.describe}")
        | Ok slot -> Assert.Fail($"the light source was put in the polarizer slot '{slot.id.value}'")

        let emptied = dialSlot.withoutItem
        Assert.Equal<SlotContent>(EmptySlotContent, emptied.content)
        Assert.Equal<ElementPlacement>(dialSlot.placement, emptied.placement)
        Assert.Equal<SetupSlot>({ dialSlot with content = EmptySlotContent }, emptied)

    [<Fact>]
    let ``a dial reading is drawn through the item's current zero, and the table's angle is recorded back as a reading`` () : unit =
        let dialSlot = assigned dialLp linearPolarizerChoice lp30
        let rezeroed = dialLp |> withDialZero (Angle.degree 20.0<deg>)

        // The −20° reading through the zero now at 20° is −1 · (−20° − 20°) = 40°; the reading itself is kept.
        let drawn = dialSlot.drawnWith [ fixedLp; rezeroed ]
        assertAngle (Angle.degree 40.0<deg>) drawn.placement.r1
        assertAngle (Angle.degree (-20.0<deg>)) (readingOf drawn)

        // The table turns the slot to 50°, which the backwards dial zeroed at 20° reads as 20° − 50° = −30°.
        let recorded = (turnedTo (Angle.degree 50.0<deg>) drawn).recordedWith [ rezeroed ]
        assertAngle (Angle.degree (-30.0<deg>)) (readingOf recorded)
        assertAngle (Angle.degree 50.0<deg>) recorded.placement.r1

        // A placement-turned slot, and a reading whose item is not given, are left as they are.
        let fixedSlot = assigned fixedLp linearPolarizerChoice lp30

        for slot, items in [ fixedSlot, [ fixedLp; rezeroed ]; dialSlot, [ fixedLp ] ] do
            Assert.Equal<SetupSlot>(slot, slot.drawnWith items)
            let turned = turnedTo (Angle.degree 50.0<deg>) slot
            Assert.Equal<SetupSlot>(turned, turned.recordedWith items)

    [<Fact>]
    let ``a swap switches to the held setup holding that content, and otherwise makes a new setup`` () : unit =
        let dialSlot = assigned dialLp linearPolarizerChoice lp30
        let setupA : Setup = createdOver (SetupName "A") []
        let slotsB = [ src; dialSlot; det ]

        let setupB : Setup =
            match Setup.create (SetupName "B") slotsB [ setupA ] with
            | Ok setup -> setup
            | Error error -> failwith $"'B' was refused: {error.describe}"

        let held = [ setupA; setupB ]
        let fresh = SetupName "Setup 3"
        Assert.Equal<SetupSwap>(SwitchToHeldSetup setupB, Setup.swapTarget slotsB held fresh)
        Assert.Equal<SetupSwap>(SwitchToHeldSetup setupA, Setup.swapTarget setupA.slots held fresh)

        let created (slots : SetupSlot list) : Setup =
            {
                name = fresh
                revision = SetupRevision.first
                slots = slots
                history = []
            }

        // No held setup holds LP1 in the polarizer slot.
        let withFixed = [ src; assigned fixedLp linearPolarizerChoice lp30; det ]
        Assert.Equal<SetupSwap>(CreateSwappedSetup (created withFixed), Setup.swapTarget withFixed held fresh)

        // B's slots with the dial reading changed hold other content, so B is not matched.
        let otherReading = [ src; { dialSlot with content = ItemInSlot (dialLp.id, DialTurn (Angle.degree (-25.0<deg>))) }; det ]
        Assert.Equal<SetupSwap>(CreateSwappedSetup (created otherReading), Setup.swapTarget otherReading held fresh)

    // ================================ spec 0050 S8.4: the slot bar's chips ================================

    let private circularPolarizerChoice : ItemModelChoice =
        LibraryModelChoice (BuiltinCatalogue.itemModel ItemModelIds.circularPolarizerModelId)

    let private fixedLp2 : Item =
        itemOf (ItemId (Guid.Parse "5a6b7c8d-9eaf-4b0c-91d2-3e4f5a6b7c96")) (ItemLabel "LP2") linearPolarizerChoice FixedMounting

    let private cpl1 : Item =
        itemOf (ItemId (Guid.Parse "6b7c8d9e-af0b-4c1d-a2e3-4f5a6b7c8da7")) (ItemLabel "CPL1") circularPolarizerChoice FixedMounting

    /// The glass plate on a dial that reads backwards, its zero at 10°.
    let private plateOnDial : Item =
        itemOf (ItemId (Guid.Parse "7c8d9eaf-0b1c-4d2e-b3f4-5a6b7c8d9eb8")) (ItemLabel "QZd") glassPlateChoice (DialMounted DialNegative)
        |> withDialZero (Angle.degree 10.0<deg>)

    /// Made over the LP preset, so it would fit a polarizer slot, but `choiceOf` resolves no model for it.
    let private ghost : Item =
        itemOf (ItemId (Guid.Parse "8d9eaf0b-1c2d-4e3f-84a5-6b7c8d9eafc9")) (ItemLabel "Ghost") linearPolarizerChoice FixedMounting

    /// Each item's model as the library resolves it; none for `ghost`.
    let private choiceOf (item : Item) : ItemModelChoice option =
        let resolved : (ItemId * ItemModelChoice) list =
            [
                laser.id, heNeChoice
                fixedLp.id, linearPolarizerChoice
                fixedLp2.id, linearPolarizerChoice
                cpl1.id, circularPolarizerChoice
                plate.id, glassPlateChoice
                plateOnDial.id, glassPlateChoice
            ]

        resolved |> List.tryFind (fun (id, _) -> id = item.id) |> Option.map snd

    [<Fact>]
    let ``a slot lists the kit's items that fit it, in their order`` () : unit =
        let kitItems = [ laser; fixedLp; plate; cpl1; ghost; fixedLp2 ]

        Assert.Equal<Item list>([ fixedLp; cpl1; fixedLp2 ], lpA.fittingItems kitItems choiceOf)
        Assert.Equal<Item list>([ plate ], sampleA.fittingItems kitItems choiceOf)
        Assert.Equal<Item list>([ laser ], src.fittingItems kitItems choiceOf)
        Assert.Empty(det.fittingItems kitItems choiceOf)

    [<Fact>]
    let ``a typed letter picks the next fitting item whose label starts with it, wrapping, and never the slot's own`` () : unit =
        let fitting = [ fixedLp; cpl1; fixedLp2 ]

        let typed (text : string) : PickLetter =
            match PickLetter.tryCreate text with
            | Some letter -> letter
            | None -> failwith $"'{text}' types no letter"

        let holdingLp1 = assigned fixedLp linearPolarizerChoice lpA
        let holdingLp2 = assigned fixedLp2 linearPolarizerChoice lpA
        let holdingCpl1 = assigned cpl1 circularPolarizerChoice lpA

        let picks : (SetupSlot * string * Item option) list =
            [
                holdingLp1, "l", Some fixedLp2
                holdingLp1, "c", Some cpl1
                // The last LP wraps round to the first.
                holdingLp2, "L", Some fixedLp
                lpA, "l", Some fixedLp
                // CPL1 is the only label starting with C, and a letter never picks the slot's own item.
                holdingCpl1, "c", None
                holdingLp1, "x", None
                lpA, "x", None
            ]

        for slot, text, expected in picks do
            Assert.Equal<Item option>(expected, slot.pickedByLetter (typed text) fitting)

        for text in [ "q"; "qz" ] do
            Assert.Equal<PickLetter option>(Some (PickLetter 'Q'), PickLetter.tryCreate text)

        for text in [ ""; " "; "-" ] do
            Assert.Equal<PickLetter option>(None, PickLetter.tryCreate text)

    [<Fact>]
    let ``an object's angle is its dial reading, or its R1`` () : unit =
        // On a backwards dial zeroed at 10°, R1 30° reads 10° − 30° = −20° (step 258); the drawn R1 stays 30°.
        let onDial = assigned plateOnDial glassPlateChoice (turnedTo (Angle.degree 30.0<deg>) sampleA)
        assertAngle (Angle.degree (-20.0<deg>)) onDial.turnAngle
        assertAngle (Angle.degree 30.0<deg>) onDial.placement.r1

        let fixedPlate = assigned plate glassPlateChoice (turnedTo (Angle.degree 45.0<deg>) sampleA)
        assertAngle (Angle.degree 45.0<deg>) fixedPlate.turnAngle

        assertAngle (Angle.degree 15.0<deg>) (turnedTo (Angle.degree 15.0<deg>) sampleA).turnAngle

    // ================================ spec 0050 S9.1: a dial-mounted item's scan ================================

    [<Fact>]
    let ``a dial-mounted item's slot scans its dial reading where a fixed one scans R1`` () : unit =
        // LPd sits on a dial that reads backwards, its zero known at 10°.
        let dialLpd =
            itemOf (ItemId (Guid.Parse "9eaf0b1c-2d3e-4f4a-95b6-7c8d9eaf0bda")) (ItemLabel "LPd") linearPolarizerChoice (DialMounted DialNegative)
            |> withDialZero (Angle.degree 10.0<deg>)

        let kitItems = [ laser; fixedLp; plate; cpl1; fixedLp2; plateOnDial; dialLpd ]

        let scans : (SetupSlot * VariableParameter list) list =
            [
                assigned fixedLp linearPolarizerChoice lpA, [ VaryR1 ]
                assigned dialLpd linearPolarizerChoice lpA, [ VaryDialReading ]
                assigned plateOnDial glassPlateChoice sampleA, [ VaryDialReading; VaryR2 ]
                // A fixed item, an empty slot and a source scan what their kind allows.
                assigned plate glassPlateChoice sampleA, [ VaryR1; VaryR2 ]
                lpA, [ VaryR1 ]
                src, [ VaryWaveLength ]
            ]

        for slot, expected in scans do
            Assert.Equal<VariableParameter list>(expected, slot.scanVariablesWith kitItems)

        // An item the list does not hold reads no dial.
        Assert.Equal<VariableParameter list>([ VaryR1 ], (assigned dialLpd linearPolarizerChoice lpA).scanVariablesWith [ fixedLp ])

        let readings = [ for k in 0 .. 18 -> Propagation.SweptValue (20.0 * float k) ]
        Assert.Equal<Propagation.SweptValue list>(readings, (VariableRange.forVariable VaryDialReading).sweptValues)
