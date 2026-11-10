namespace OpticalConstructor.Tests

open System
open Xunit
open Berreman.Constants
open Berreman.Geometry
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Placement
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
