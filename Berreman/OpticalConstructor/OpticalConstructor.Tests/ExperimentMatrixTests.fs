namespace OpticalConstructor.Tests

open System
open Xunit
open Berreman.Constants
open Berreman.Geometry
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Placement
open OpticalConstructor.Domain.ItemModels
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.Setups
open OpticalConstructor.Domain.Experiments
open OpticalConstructor.Domain.ExperimentMatrices

/// The experiment matrix: each experiment in the row of its Prep items and its objects at their angles and the column
/// of its analyzers, over a copy of step 271's bench (spec 0050 S9.3; 001 §O).
module ExperimentMatrixTests =

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
    let private sampleB : SetupSlot = emptySlot (Library.ElementId "sampleB") Sample { x = 0.1<meter>; y = 0.0<meter> }
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

    /// QZd sits on a dial that reads backwards, its zero at 10°.
    let private qzd : Item =
        let item =
            itemOf (ItemId (Guid.Parse "af011223-3445-4056-a718-9daebfc0d1e6")) (ItemLabel "QZd") blackBoxChoice (DialMounted DialNegative)

        match item.withParameterState ParameterName.dialZero (KnownParameter (ParameterValue (Angle.degree 10.0<deg>).value)) with
        | Ok zeroed -> zeroed
        | Error error -> failwith $"'QZd' refused a dial zero: {error.describe}"

    let private kitItems : Item list = [ laser; cpl1; cpl2; qz; lr; qzd ]

    /// The slot holding the item, put there by `withItem` as the table assigns it.
    let private holding (item : Item) (model : ItemModelChoice) (slot : SetupSlot) : SetupSlot =
        match slot.withItem item model with
        | Ok held -> held
        | Error error -> failwith $"'{item.label.value}' did not fit '{slot.id.value}': {error.describe}"

    /// The slot turned to that R1, then holding the item.
    let private turnedHolding (r1 : Angle) (item : Item) (model : ItemModelChoice) (slot : SetupSlot) : SetupSlot =
        holding item model { slot with placement = withR1 r1 slot.placement }

    let private deg45 : Angle = Angle.degree 45.0<deg>
    let private qz45 : SetupSlot = turnedHolding deg45 qz blackBoxChoice sampleA
    let private cpl2Behind : SetupSlot = holding cpl2 circularPolarizerChoice lpB

    /// The bench with this analyzer slot and these object slots, out of light order: the Laser at the source, CPL1
    /// before the objects and an empty detector, none of which but CPL1 enters the matrix.
    let private benchWith (analyzer : SetupSlot) (objA : SetupSlot) (objB : SetupSlot) : SetupSlot list =
        [
            holding laser heNeChoice src
            det
            analyzer
            objB
            objA
            holding cpl1 circularPolarizerChoice lpA
        ]

    let private setupOf (name : SetupName) (slots : SetupSlot list) : Setup =
        match Setup.create name slots [] with
        | Ok setup -> setup
        | Error error -> failwith $"'{name.value}' was refused: {error.describe}"

    let private experimentOver (id : ExperimentId) (setup : SetupReference) : Experiment =
        {
            id = id
            name = id.defaultName
            setup = setup
            scan = ExperimentScan.darkLine CaptureTransmitted
            role = ScienceRun
            sessionOpt = None
            notes = ExperimentNotes.none
            dataOpt = None
            corrections = []
        }

    let private cellOf (matrix : ExperimentMatrix) (row : MatrixRowKey) (column : MatrixColumnKey) : MatrixCell =
        match matrix.tryFindCell row column with
        | Some cell -> cell
        | None -> failwith $"no cell at %A{row} and %A{column} in %A{matrix}"

    [<Fact>]
    let ``the matrix puts each experiment in the row of its sources and objects at their angles and the column of its analyzers`` () : unit =
        let a = setupOf (SetupName "A") (benchWith cpl2Behind qz45 sampleB)
        let qz90 = turnedHolding (Angle.degree 90.0<deg>) qz blackBoxChoice sampleA
        let b = setupOf (SetupName "B") (benchWith cpl2Behind qz90 sampleB)
        let c = setupOf (SetupName "C") (benchWith lpB qz45 sampleB)

        let e1 = ExperimentId 1
        let e2 = ExperimentId 2
        let e3 = ExperimentId 3
        let e4 = ExperimentId 4

        let experiments =
            [ experimentOver e1 a.reference; experimentOver e2 b.reference; experimentOver e3 a.reference; experimentOver e4 c.reference ]

        let matrix = ExperimentMatrix.ofExperiments experiments [ a; b; c ] kitItems

        let qzAt45 : MatrixRowKey = { sources = [ cpl1.label ]; objects = [ { item = qz.label; angle = deg45 } ] }
        let qzAt90 : MatrixRowKey = { sources = [ cpl1.label ]; objects = [ { item = qz.label; angle = Angle.degree 90.0<deg> } ] }
        let throughCpl2 : MatrixColumnKey = { analyzers = [ cpl2.label ] }
        let noAnalyzer : MatrixColumnKey = { analyzers = [] }

        Assert.Equal<MatrixRowKey list>([ qzAt45; qzAt90 ], matrix.rows)
        Assert.Equal<MatrixColumnKey list>([ throughCpl2; noAnalyzer ], matrix.columns)

        let expectedCells : MatrixCell list =
            [
                { row = qzAt45; column = throughCpl2; experiments = [ e1; e3 ] }
                { row = qzAt90; column = throughCpl2; experiments = [ e2 ] }
                { row = qzAt45; column = noAnalyzer; experiments = [ e4 ] }
            ]

        Assert.Equal<MatrixCell list>(expectedCells, matrix.cells)
        Assert.Equal<ExperimentId list>([], matrix.unplaced)

        // Each cell is found at its row and column; where nothing was measured there is no cell.
        for expected in expectedCells do
            Assert.Equal<MatrixCell option>(Some expected, matrix.tryFindCell expected.row expected.column)

        Assert.Equal<MatrixCell option>(None, matrix.tryFindCell qzAt90 noAnalyzer)

        // An item the items lack adds nothing: without CPL2 in the kit, E1 sits in the column of no analyzer.
        let lackingCpl2 = kitItems |> List.filter (fun item -> item.id <> cpl2.id)
        let withoutCpl2 = ExperimentMatrix.ofExperiments [ experimentOver e1 a.reference ] [ a ] lackingCpl2
        Assert.Equal<ExperimentId list>([ e1 ], (cellOf withoutCpl2 qzAt45 noAnalyzer).experiments)

    [<Fact>]
    let ``two objects in series make one row, a dial-mounted object sits at its reading, and an experiment whose setup the kit lacks is unplaced`` () : unit =
        let lr45 = turnedHolding deg45 lr blackBoxChoice sampleB
        let qzd30 = turnedHolding (Angle.degree 30.0<deg>) qzd blackBoxChoice sampleA
        let series = setupOf (SetupName "Series") (benchWith cpl2Behind qz45 lr45)
        let dial = setupOf (SetupName "Dial") (benchWith cpl2Behind qzd30 sampleB)
        let gone : SetupReference = { name = SetupName "Gone"; revision = SetupRevision.first }

        let e1 = ExperimentId 1
        let e2 = ExperimentId 2
        let e3 = ExperimentId 3

        let matrix =
            ExperimentMatrix.ofExperiments
                [ experimentOver e1 series.reference; experimentOver e2 gone; experimentOver e3 dial.reference ]
                [ series; dial ]
                kitItems

        let throughCpl2 : MatrixColumnKey = { analyzers = [ cpl2.label ] }
        let qzThenLr : MatrixRowKey =
            {
                sources = [ cpl1.label ]
                objects = [ { item = qz.label; angle = deg45 }; { item = lr.label; angle = deg45 } ]
            }

        // QZ and LR in series are one row, in light order; QZd's row and the column follow.
        match matrix.rows with
        | [ seriesRow; dialRow ] ->
            Assert.Equal<MatrixRowKey>(qzThenLr, seriesRow)
            Assert.Equal<ItemLabel list>([ cpl1.label ], dialRow.sources)

            // R1 30° on QZd's backwards dial zeroed at 10° reads 10° − 30° = −20°.
            match dialRow.objects with
            | [ dialObject ] ->
                Assert.Equal<ItemLabel>(qzd.label, dialObject.item)
                Assert.Equal((Angle.degree (-20.0<deg>)).value, dialObject.angle.value, 1e-12)
            | other -> Assert.Fail($"QZd's row: expected one object, got %A{other}")

            Assert.Equal<ExperimentId list>([ e1 ], (cellOf matrix seriesRow throughCpl2).experiments)
            Assert.Equal<ExperimentId list>([ e3 ], (cellOf matrix dialRow throughCpl2).experiments)
        | other -> Assert.Fail($"expected two rows, got %A{other}")

        Assert.Equal<MatrixColumnKey list>([ throughCpl2 ], matrix.columns)

        // The experiment over `Gone` revision 1 is unplaced, and in no cell.
        Assert.Equal<ExperimentId list>([ e2 ], matrix.unplaced)
        Assert.Equal<ExperimentId list>([ e1; e3 ], matrix.cells |> List.collect (fun cell -> cell.experiments))
