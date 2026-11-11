namespace OpticalConstructor.Tests

open System
open Xunit
open Berreman.Constants
open Berreman.Geometry
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Placement
open OpticalConstructor.Domain.ItemModels
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.Lifecycle
open OpticalConstructor.Domain.Setups
open OpticalConstructor.Domain.NamingTemplates
open OpticalConstructor.Domain.SetupGrids

/// Spec 0050 S8.3: the grid generator, over a test-local copy of step 271's bench.
module SetupGridTests =

    let private emptySlot (id : Library.ElementId) (kind : CatalogueKind) (point : TablePoint) : SetupSlot =
        {
            id = id
            placement = ElementPlacement.create kind point
            zoom = BeamLayout.ElementZoom 5.0
            beam = BeamLayout.OnMainBeam
            content = EmptySlotContent
        }

    let private src : SetupSlot = emptySlot (Library.ElementId "src") LightSource RayModel.defaultSourcePoint
    let private lpA : SetupSlot = emptySlot (Library.ElementId "lpA") LinearPolarizer { x = -0.3<meter>; y = 0.0<meter> }
    let private sampleA : SetupSlot = emptySlot (Library.ElementId "sampleA") Sample { x = -0.1<meter>; y = 0.0<meter> }
    let private lpB : SetupSlot = emptySlot (Library.ElementId "lpB") LinearPolarizer { x = 0.3<meter>; y = 0.0<meter> }
    let private det : SetupSlot = emptySlot (Library.ElementId "det") Detector RayModel.defaultDetectorPoint

    let private heNeChoice : ItemModelChoice =
        SourcePresetChoice (BuiltinCatalogue.itemModel ItemModelIds.sourcePresetItemModelId, BuiltinCatalogue.source Library.PresetIds.heNe633)

    let private circularPolarizerChoice : ItemModelChoice =
        LibraryModelChoice (BuiltinCatalogue.itemModel ItemModelIds.circularPolarizerModelId)

    let private blackBoxChoice : ItemModelChoice = LibraryModelChoice (BuiltinCatalogue.itemModel ItemModelIds.blackBoxMuellerModelId)

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

    let private laser : Item =
        itemOf (ItemId (Guid.Parse "5a6b7c8d-9eaf-4b01-92c3-4e5f6a7b8c91")) (ItemLabel "Laser") heNeChoice FixedMounting

    let private cpl1 : Item =
        itemOf (ItemId (Guid.Parse "6b7c8d9e-af01-4c12-a3d4-5f6a7b8c9da2")) (ItemLabel "CPL1") circularPolarizerChoice FixedMounting

    let private cpl2 : Item =
        itemOf (ItemId (Guid.Parse "7c8d9eaf-0112-4d23-b4e5-6a7b8c9daeb3")) (ItemLabel "CPL2") circularPolarizerChoice FixedMounting

    let private qz : Item = itemOf (ItemId (Guid.Parse "8d9eaf01-1223-4e34-85f6-7b8c9daebfc4")) (ItemLabel "QZ") blackBoxChoice FixedMounting
    let private lr : Item = itemOf (ItemId (Guid.Parse "9eaf0112-2334-4f45-9607-8c9daebfc0d5")) (ItemLabel "LR") blackBoxChoice FixedMounting

    /// Made over a shipped model, but `choiceOf` finds its model in no library.
    let private ghost : Item =
        itemOf (ItemId (Guid.Parse "b0122334-4556-4167-b829-aebfc0d1e2f7")) (ItemLabel "Ghost") blackBoxChoice FixedMounting

    let private kitItems : Item list = [ laser; cpl1; cpl2; qz; lr; ghost ]

    /// Each item's model as the library resolves it; none for `ghost`.
    let private choiceOf (item : Item) : ItemModelChoice option =
        let resolved : (ItemId * ItemModelChoice) list =
            [
                laser.id, heNeChoice
                cpl1.id, circularPolarizerChoice
                cpl2.id, circularPolarizerChoice
                qz.id, blackBoxChoice
                lr.id, blackBoxChoice
            ]

        resolved |> List.tryFind (fun (id, _) -> id = item.id) |> Option.map snd

    /// The table: the Laser at the source and every other slot empty.
    let private table : SetupSlot list =
        match src.withItem laser heNeChoice with
        | Ok lit -> [ lit; lpA; sampleA; lpB; det ]
        | Error error -> failwith $"the Laser did not fit the source: {error.describe}"

    let private lab : NamingTemplate =
        match NamingTemplate.tryCreate "{source:CPL1=CPL,LP1=LP}-({obj1}#{phi1}-{obj2}#{phi2})-{analyzer:CPL2=CPL,LP2=LP}-{note}" with
        | Ok template -> template
        | Error error -> failwith $"the lab template was refused: {error.describe}"

    let private zero : Angle = Angle.degree 0.0<deg>

    let private holdingAt (item : Item) (r1 : Angle) : SlotChoice = { itemOpt = Some item.id; r1 = r1 }

    let private axisOf (slot : SetupSlot) (choices : SlotChoice list) : SlotOptions = { slot = slot.id; choices = choices }

    let private combinationOf (picks : (SetupSlot * SlotChoice) list) : GridCombination =
        {
            picks =
                picks
                |> List.map (fun (slot, choice) ->
                    let pick : SlotPick = { slot = slot.id; choice = choice }
                    pick)
        }

    let private namesOf (setups : Setup list) : SetupName list = setups |> List.map (fun setup -> setup.name)

    [<Fact>]
    let ``a grid lists the product of its slots' options, the first slot slowest, and leaves out what is unticked`` () : unit =
        let cpl1At0 = holdingAt cpl1 zero
        let nothingAt0 : SlotChoice = { itemOpt = None; r1 = zero }
        let qzAt0 = holdingAt qz zero
        let qzAt45 = holdingAt qz (Angle.degree 45.0<deg>)
        let lrAt90 = holdingAt lr (Angle.degree 90.0<deg>)

        let grid : SetupGrid =
            {
                axes = [ axisOf lpA [ cpl1At0; nothingAt0 ]; axisOf sampleA [ qzAt0; qzAt45; lrAt90 ] ]
                unticked = []
            }

        let expected : GridCombination list =
            [
                combinationOf [ lpA, cpl1At0; sampleA, qzAt0 ]
                combinationOf [ lpA, cpl1At0; sampleA, qzAt45 ]
                combinationOf [ lpA, cpl1At0; sampleA, lrAt90 ]
                combinationOf [ lpA, nothingAt0; sampleA, qzAt0 ]
                combinationOf [ lpA, nothingAt0; sampleA, qzAt45 ]
                combinationOf [ lpA, nothingAt0; sampleA, lrAt90 ]
            ]

        Assert.Equal<GridCombination list>(expected, grid.combinations)
        Assert.Equal<GridCombination list>(expected, grid.ticked)

        let unwanted = combinationOf [ lpA, cpl1At0; sampleA, qzAt45 ]
        let without = grid.untick unwanted
        Assert.Equal<GridCombination list>(expected |> List.filter (fun combination -> combination <> unwanted), without.ticked)
        Assert.Equal<GridCombination list>(expected, without.combinations)
        Assert.Equal<GridCombination list>([ unwanted ], without.unticked)
        Assert.Equal<SetupGrid>(without, without.untick unwanted)

        let again = without.tick unwanted
        Assert.Equal<GridCombination list>(expected, again.ticked)
        Assert.Empty(again.unticked)

        let noAxis : SetupGrid = { axes = []; unticked = [] }
        Assert.Equal<GridCombination list>([ { picks = [] } ], noAxis.combinations)

    [<Fact>]
    let ``a grid starts with the combinations it cannot make unticked`` () : unit =
        let strangerId = ItemId (Guid.Parse "c1233445-5667-4278-893a-bfc0d1e2f308")
        let stranger : SlotChoice = { itemOpt = Some strangerId; r1 = zero }
        let qzAt0 = holdingAt qz zero
        let lpAChoices = [ holdingAt cpl1 zero; holdingAt laser zero; holdingAt ghost zero; stranger ]
        let grid = SetupGrid.create [ axisOf lpA lpAChoices; axisOf sampleA [ qzAt0 ] ] table kitItems choiceOf
        let made = combinationOf [ lpA, holdingAt cpl1 zero; sampleA, qzAt0 ]
        let laserOne = combinationOf [ lpA, holdingAt laser zero; sampleA, qzAt0 ]
        let ghostOne = combinationOf [ lpA, holdingAt ghost zero; sampleA, qzAt0 ]
        let strangerOne = combinationOf [ lpA, stranger; sampleA, qzAt0 ]

        Assert.Equal<GridCombination list>([ laserOne; ghostOne; strangerOne ], grid.unticked)
        Assert.Equal<GridCombination list>([ made ], grid.ticked)

        // Each is left out for the reason AssignSlot would refuse it.
        match laserOne.slotsOver table kitItems choiceOf with
        | Error (IncompatibleSlotItemErr reason) -> Assert.Contains("'Laser'", reason)
        | Error other -> Assert.Fail($"expected IncompatibleSlotItemErr, got {other.describe}")
        | Ok slots -> Assert.Fail($"the Laser was put in a polarizer slot among {slots.Length} slots")

        match ghostOne.slotsOver table kitItems choiceOf with
        | Error (UnknownSlotItemErr reason) -> Assert.Contains("'Ghost'", reason)
        | Error other -> Assert.Fail($"expected UnknownSlotItemErr, got {other.describe}")
        | Ok slots -> Assert.Fail($"Ghost was put in a slot among {slots.Length} slots")

        match strangerOne.slotsOver table kitItems choiceOf with
        | Error (UnknownSlotItemErr reason) -> Assert.Contains(strangerId.value.ToString(), reason)
        | Error other -> Assert.Fail($"expected UnknownSlotItemErr, got {other.describe}")
        | Ok slots -> Assert.Fail($"an item no kit holds was put in a slot among {slots.Length} slots")

        let cpl1At0 = holdingAt cpl1 zero
        let twice = SetupGrid.create [ axisOf lpA [ cpl1At0 ]; axisOf lpB [ cpl1At0; holdingAt cpl2 zero ] ] table kitItems choiceOf
        Assert.Equal<GridCombination list>([ combinationOf [ lpA, cpl1At0; lpB, cpl1At0 ] ], twice.unticked)
        Assert.Equal<GridCombination list>([ combinationOf [ lpA, cpl1At0; lpB, holdingAt cpl2 zero ] ], twice.ticked)

    [<Fact>]
    let ``a grid's setups take the template's names, and a combination a held setup holds is that setup`` () : unit =
        let qzAt45 = holdingAt qz (Angle.degree 45.0<deg>)

        let grid : SetupGrid =
            {
                axes = [ axisOf lpA [ holdingAt cpl1 zero ]; axisOf sampleA [ holdingAt qz zero; qzAt45 ]; axisOf lpB [ holdingAt cpl2 zero ] ]
                unticked = []
            }

        let generated (held : Setup list) (over : SetupGrid) : Setup list =
            match over.generate table kitItems choiceOf lab held with
            | Ok setups -> setups
            | Error error -> failwith $"the grid was refused: {error.describe}"

        let createdAs (name : SetupName) (slots : SetupSlot list) : Setup =
            match Setup.create name slots [] with
            | Ok setup -> setup
            | Error error -> failwith $"'{name.value}' was refused: {error.describe}"

        let fresh = generated [] grid
        Assert.Equal<SetupName list>([ SetupName "CPL-(QZ#0)-CPL"; SetupName "CPL-(QZ#45)-CPL" ], namesOf fresh)

        for setup in fresh do
            Assert.Equal<SetupRevision>(SetupRevision.first, setup.revision)
            Assert.Empty(setup.history)

        // Each setup holds its combination's slots: the table's, picked, turned and recorded.
        let slotsEach = grid.ticked |> List.map (fun combination -> combination.slotsOver table kitItems choiceOf)
        Assert.Equal<Result<SetupSlot list, SetupError> list>(fresh |> List.map (fun setup -> Ok setup.slots), slotsEach)

        let first = List.head fresh
        let second = List.item 1 fresh
        let turnedSample = second.slots |> List.find (fun slot -> slot.id = sampleA.id)
        Assert.Equal<Angle>(Angle.degree 45.0<deg>, turnedSample.placement.r1)
        Assert.Equal<SlotContent>(ItemInSlot (qz.id, PlacementTurn), turnedSample.content)

        // A held setup whose current slots are a combination's is that setup; the others are made as before.
        Assert.Equal<Setup list>([ first; second ], generated [ first ] grid)

        let bench = (createdAs (SetupName "Bench") table).revisedWith first.slots VersionPinned
        Assert.Equal<SetupRevision>(SetupRevision.first.next, bench.revision)
        Assert.Equal<Setup list>([ bench; second ], generated [ bench ] grid)

        // A held setup of other slots keeps its name; the new one takes the template's made free.
        let namesake = createdAs (SetupName "CPL-(QZ#45)-CPL") table
        Assert.Equal<SetupName list>([ SetupName "CPL-(QZ#0)-CPL"; SetupName "CPL-(QZ#45)-CPL (2)" ], namesOf (generated [ namesake ] grid))

        // With lpA left empty, `lab` names no setup, so each takes the first free `Setup N`.
        let unnamed = { grid with axes = grid.axes |> List.filter (fun options -> options.slot <> lpA.id) }
        Assert.Equal<SetupName list>([ SetupName "Setup 1"; SetupName "Setup 2" ], namesOf (generated [] unnamed))

    [<Fact>]
    let ``a ticked combination the grid cannot make refuses the whole grid`` () : unit =
        let refusalOf (grid : SetupGrid) : SetupError =
            match grid.generate table kitItems choiceOf lab [] with
            | Error error -> error
            | Ok setups -> failwith $"the grid made {setups.Length} setups"

        let qzAt0 = holdingAt qz zero

        let withLaser : SetupGrid =
            {
                axes = [ axisOf lpA [ holdingAt cpl1 zero; holdingAt laser zero ]; axisOf sampleA [ qzAt0 ] ]
                unticked = []
            }

        match refusalOf withLaser with
        | IncompatibleSlotItemErr reason -> Assert.Contains("'Laser'", reason)
        | other -> Assert.Fail($"expected IncompatibleSlotItemErr, got {other.describe}")

        // Unticked, the Laser's combination leaves the one the grid can make; lpB is empty, so `lab` cannot name it.
        let laserLeftOut = withLaser.untick (combinationOf [ lpA, holdingAt laser zero; sampleA, qzAt0 ])

        match laserLeftOut.generate table kitItems choiceOf lab [] with
        | Ok setups -> Assert.Equal<SetupName list>([ SetupName "Setup 1" ], namesOf setups)
        | Error error -> Assert.Fail($"the grid was refused: {error.describe}")

        let cpl1Twice : SetupGrid =
            {
                axes = [ axisOf lpA [ holdingAt cpl1 zero ]; axisOf lpB [ holdingAt cpl1 zero ] ]
                unticked = []
            }

        match refusalOf cpl1Twice with
        | SlotItemTwiceErr reason -> Assert.Contains("'CPL1'", reason)
        | other -> Assert.Fail($"expected SlotItemTwiceErr, got {other.describe}")

        let sampleB = emptySlot (Library.ElementId "sampleB") Sample { x = 0.1<meter>; y = 0.0<meter> }

        let offTable : SetupGrid =
            {
                axes = [ axisOf lpA [ holdingAt cpl1 zero ]; axisOf sampleB [ qzAt0 ] ]
                unticked = []
            }

        match refusalOf offTable with
        | UnknownSetupSlotErr reason -> Assert.Contains("'sampleB'", reason)
        | other -> Assert.Fail($"expected UnknownSetupSlotErr, got {other.describe}")
