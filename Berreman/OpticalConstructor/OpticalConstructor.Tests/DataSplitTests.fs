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
open OpticalConstructor.Domain.NamingTemplates
open OpticalConstructor.Domain.MeasuredData
open OpticalConstructor.Domain.MuellerReconstruction
open OpticalConstructor.Domain.ImportProfiles
open OpticalConstructor.Domain.DataSplits

/// One lab file split into groups by its key column, the key column each import profile splits by, and each group
/// matched to an experiment by the naming template (spec 0050 S9.3; 001 §K).
module DataSplitTests =

    let private groupOf (key : DataRowKey) (points : MeasuredPointCount) : KeyedGroup = { key = key; points = points }

    let private assertSplit (expected : KeyedGroup list) (points : MappedPoints) : unit =
        let actual = points.splitGroups

        if actual <> Ok expected then
            Assert.Fail($"expected the groups %A{expected}, got %A{actual}")

    [<Fact>]
    let ``a file splits into one group per key in first-appearance order, each counting its points`` () : unit =
        let a = DataRowKey "A"
        let b = DataRowKey "B"
        let c = DataRowKey "C"
        let zCut = DataRowKey "Z-cut"
        let xCut = DataRowKey "X-cut"
        let k1 = DataRowKey "K1"
        let aLabel = MuellerExperimentLabel "A"
        let bLabel = MuellerExperimentLabel "B"

        let captures =
            CapturePoints (ImportProfileTests.capturesLabelled [ bLabel; aLabel; bLabel; MuellerExperimentLabel "C"; aLabel; bLabel ])

        let observations =
            ObservationPoints [ ImportProfileTests.observationOf zCut; ImportProfileTests.observationOf zCut; ImportProfileTests.observationOf xCut ]

        let labRows =
            ColumnPoints
                [
                    ImportProfileTests.labScanRow (Some k1) (Units.UnitMagnitude 0.0)
                    ImportProfileTests.labScanRow None (Units.UnitMagnitude 10.0)
                    ImportProfileTests.labScanRow (Some k1) (Units.UnitMagnitude 20.0)
                ]

        assertSplit [ groupOf b (MeasuredPointCount 3); groupOf a (MeasuredPointCount 2); groupOf c (MeasuredPointCount 1) ] captures
        assertSplit [ groupOf zCut (MeasuredPointCount 2); groupOf xCut (MeasuredPointCount 1) ] observations
        assertSplit [ groupOf k1 (MeasuredPointCount 2) ] labRows
        assertSplit [] (CapturePoints [])

        // A group's points are exactly those the selection of its key keeps.
        for points in [ captures; observations; labRows ] do
            match points.splitGroups with
            | Ok groups ->
                for group in groups do
                    match points.selectedBy (KeyedDataRows group.key) with
                    | Ok selected -> Assert.Equal(group.points, selected.pointCount)
                    | Error error -> Assert.Fail($"the selection of '{group.key.value}' was refused: %A{error}")
            | Error error -> Assert.Fail($"the file was not split: %A{error}")

    [<Fact>]
    let ``a file without a key column does not split, and a profile names the key column it splits by`` () : unit =
        match (SeriesPoints (IntensityData ImportProfileTests.twoPointIntensity)).splitGroups with
        | Error (MalformedDataFileErr reason) -> Assert.Contains("no key column to split by", reason)
        | other -> Assert.Fail($"an X,Y series: expected MalformedDataFileErr, got %A{other}")

        let unkeyed =
            ColumnPoints
                [
                    ImportProfileTests.labScanRow None (Units.UnitMagnitude 0.0)
                    ImportProfileTests.labScanRow None (Units.UnitMagnitude 20.0)
                ]

        match unkeyed.splitGroups with
        | Error (MalformedDataFileErr reason) -> Assert.Contains("maps no key column", reason)
        | other -> Assert.Fail($"`Lab scan` rows without a key: expected MalformedDataFileErr, got %A{other}")

        let labScan = KitTests.labScanProfile

        let keyless =
            {
                labScan with
                    name = ImportProfileName "Lab scan, no key"
                    roles = { labScan.roles with keyOpt = None }
            }

        let splitKeyOf (name : ImportProfileName) : SplitKeyColumn option =
            match ChosenImportProfile.tryOfName [ labScan; keyless ] name with
            | Some profile -> profile.splitKeyOpt
            | None ->
                Assert.Fail($"no import profile is named '{name.value}'")
                None

        Assert.Equal<SplitKeyColumn option>(Some (SplitKeyColumn "experiment"), splitKeyOf ImportProfileName.phoneRoiCsv)
        Assert.Equal<SplitKeyColumn option>(Some (SplitKeyColumn "sample"), splitKeyOf ImportProfileName.muellerObservations)
        Assert.Equal<SplitKeyColumn option>(Some (SplitKeyColumn "column 4"), splitKeyOf labScan.name)
        Assert.Equal<SplitKeyColumn option>(None, splitKeyOf ImportProfileName.intensitySeries)
        Assert.Equal<SplitKeyColumn option>(None, splitKeyOf ImportProfileName.ellipsometricSeries)
        Assert.Equal<SplitKeyColumn option>(None, splitKeyOf keyless.name)

    // ============== spec 0050 S9.3: each group matched by the naming template, over a copy of step 271's bench ==============

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

    /// Each item's model as the library resolves it.
    let private choiceOf (item : Item) : ItemModelChoice option =
        let resolved : (ItemId * ItemModelChoice) list =
            [
                laser.id, heNeChoice
                cpl1.id, circularPolarizerChoice
                cpl2.id, circularPolarizerChoice
                qz.id, blackBoxChoice
                lr.id, blackBoxChoice
                qzd.id, blackBoxChoice
            ]

        resolved |> List.tryFind (fun (id, _) -> id = item.id) |> Option.map snd

    /// The table: the Laser at the source and every other slot empty, out of light order.
    let private table : SetupSlot list =
        match src.withItem laser heNeChoice with
        | Ok lit -> [ lit; det; lpB; sampleB; sampleA; lpA ]
        | Error error -> failwith $"the Laser did not fit the source: {error.describe}"

    let private lab : NamingTemplate =
        match NamingTemplate.tryCreate "{source:CPL1=CPL,LP1=LP}-({obj1}#{phi1}-{obj2}#{phi2})-{analyzer:CPL2=CPL,LP2=LP}-{note}" with
        | Ok template -> template
        | Error error -> failwith $"the lab template was refused: {error.describe}"

    let private experimentOf (id : ExperimentId) (name : ExperimentName) : Experiment =
        {
            id = id
            name = name
            setup = { name = SetupName "Bench"; revision = SetupRevision.first }
            scan = ExperimentScan.darkLine CaptureTransmitted
            role = ScienceRun
            sessionOpt = None
            notes = ExperimentNotes.none
            dataOpt = None
            corrections = []
        }

    let private setupOf (name : SetupName) (slots : SetupSlot list) : Setup =
        match Setup.create name slots [] with
        | Ok setup -> setup
        | Error error -> failwith $"'{name.value}' was refused: {error.describe}"

    let private paramOver (experiments : Experiment list) (setups : Setup list) (slots : SetupSlot list) : DataSplitParam =
        {
            template = lab
            experiments = experiments
            setups = setups
            table = slots
            items = kitItems
            choiceOf = choiceOf
        }

    let private onePoint : MeasuredPointCount = MeasuredPointCount 1

    let private splitOf (param : DataSplitParam) (keys : DataRowKey list) : DataSplit =
        DataSplit.ofGroups param (keys |> List.map (fun key -> groupOf key onePoint))

    let private matchingsOf (param : DataSplitParam) (keys : DataRowKey list) : SplitGroupMatch list =
        (splitOf param keys).groups |> List.map (fun group -> group.matching)

    let private slotIn (setup : Setup) (slot : SetupSlot) : SetupSlot =
        match setup.slots |> List.tryFind (fun held -> held.id = slot.id) with
        | Some held -> held
        | None -> failwith $"'{setup.name.value}' has no slot '{slot.id.value}'"

    let private assertAngle (expected : Angle) (actual : Angle) : unit = Assert.Equal(expected.value, actual.value, 1e-12)

    let private qz45Lr45 : ExperimentName = ExperimentName "CPL-(QZ#45-LR#45)-CPL"
    let private e2 : ExperimentName = ExperimentName "E2"

    [<Fact>]
    let ``a group matches the experiment its key names, as written or as the template writes it back`` () : unit =
        let param = paramOver [ experimentOf (ExperimentId 1) qz45Lr45; experimentOf (ExperimentId 2) e2 ] [] table

        let matches (expected : ExperimentName) (key : DataRowKey) : unit =
            let matched : SplitGroup =
                {
                    key = key
                    points = onePoint
                    matching = ExperimentForGroup expected
                }

            Assert.Equal<SplitGroup list>([ matched ], (splitOf param [ key ]).groups)

        // By its name's file stem, ignoring case; by the name the template writes for it (`#45.0` as `#45`).
        matches qz45Lr45 (DataRowKey "cpl-(qz#45-lr#45)-cpl")
        matches qz45Lr45 (DataRowKey "CPL-(QZ#45.0-LR#45)-CPL")
        matches e2 (DataRowKey "e2")

    [<Fact>]
    let ``an unmatched group the template reads is offered its setup and experiment over the table's slots`` () : unit =
        let first = DataRowKey "CPL-(QZ#90)-CPL-2"
        let later = DataRowKey "CPL-(QZ#90)-CPL-BR"
        let rewritten = DataRowKey "CPL-(QZ#90.0)-CPL-2"

        match matchingsOf (paramOver [] [] table) [ first; later; rewritten ] with
        | [ SetupOfferForGroup offer; SetupOfferForGroup laterOffer; SetupOfferForGroup rewrittenOffer ] ->
            let setup = offer.setup
            Assert.Equal<SetupName>(SetupName "CPL-(QZ#90)-CPL", setup.name)
            Assert.Equal<SetupRevision>(SetupRevision.first, setup.revision)
            Assert.Equal<ItemId option>(Some cpl1.id, (slotIn setup lpA).content.itemOpt)
            Assert.Equal<ItemId option>(Some qz.id, (slotIn setup sampleA).content.itemOpt)
            Assert.Equal<Angle>(Angle.degree 90.0<deg>, (slotIn setup sampleA).placement.r1)
            Assert.Equal<ItemId option>(None, (slotIn setup sampleB).content.itemOpt)
            Assert.Equal<ItemId option>(Some cpl2.id, (slotIn setup lpB).content.itemOpt)

            // The light and the detector, which `lab` has no placeholder for, stay as the table holds them.
            Assert.Equal<ItemId option>(Some laser.id, (slotIn setup src).content.itemOpt)
            Assert.Equal<ItemId option>(None, (slotIn setup det).content.itemOpt)
            Assert.Equal<ExperimentName>(ExperimentName "CPL-(QZ#90)-CPL-2", offer.experiment)

            // A later group of the same setup is offered it, and an experiment free of those offered before it.
            Assert.Equal<Setup>(setup, laterOffer.setup)
            Assert.Equal<ExperimentName>(ExperimentName "CPL-(QZ#90)-CPL-BR", laterOffer.experiment)
            Assert.Equal<Setup>(setup, rewrittenOffer.setup)
            Assert.Equal<ExperimentName>(ExperimentName "CPL-(QZ#90)-CPL-2 (2)", rewrittenOffer.experiment)

            // With that setup held, under another name, the offer is the held setup itself.
            let held = setupOf (SetupName "QZ at 90") setup.slots

            match matchingsOf (paramOver [] [ held ] table) [ first ] with
            | [ SetupOfferForGroup heldOffer ] ->
                Assert.Equal<Setup>(held, heldOffer.setup)
                Assert.Equal<ExperimentName>(ExperimentName "CPL-(QZ#90)-CPL-2", heldOffer.experiment)
            | other -> Assert.Fail($"with the setup held: expected one offer, got %A{other}")
        | other -> Assert.Fail($"expected three offers, got %A{other}")

        // QZd's name holds its dial reading −20°, which its backwards dial zeroed at 10° turns to R1 30°.
        match matchingsOf (paramOver [] [] table) [ DataRowKey "CPL-(QZd#-20)-CPL" ] with
        | [ SetupOfferForGroup dialOffer ] ->
            let dialSlot = slotIn dialOffer.setup sampleA
            assertAngle (Angle.degree 30.0<deg>) dialSlot.placement.r1

            match dialSlot.content with
            | ItemInSlot (item, DialTurn reading) ->
                Assert.Equal<ItemId>(qzd.id, item)
                assertAngle (Angle.degree (-20.0<deg>)) reading
            | other -> Assert.Fail($"expected QZd on its dial, got %A{other}")

            Assert.Equal<SetupName>(SetupName "CPL-(QZd#-20)-CPL", dialOffer.setup.name)
            Assert.Equal<ExperimentName>(ExperimentName "CPL-(QZd#-20)-CPL", dialOffer.experiment)
        | other -> Assert.Fail($"QZd: expected one offer, got %A{other}")

    [<Fact>]
    let ``an unmatched group the template cannot place is listed with its reason, and two groups never share an experiment`` () : unit =
        let kitParam = paramOver [ experimentOf (ExperimentId 1) qz45Lr45; experimentOf (ExperimentId 2) e2 ] [] table

        match matchingsOf kitParam [ DataRowKey "Darkness"; DataRowKey "CPL-(XX#0)-CPL" ] with
        | [ GroupWithoutMatch (SplitKeyNameErr (NameMismatchErr darkness)); GroupWithoutMatch (SplitKeyNameErr (UnknownNameItemErr unknown)) ] ->
            Assert.Contains("'Darkness'", darkness)
            Assert.Contains("'XX'", unknown)
        | other -> Assert.Fail($"expected two SplitKeyNameErr groups, got %A{other}")

        let withoutSampleB = table |> List.filter (fun slot -> slot.id <> sampleB.id)

        match matchingsOf (paramOver [] [] withoutSampleB) [ DataRowKey "CPL-(QZ#0-LR#0)-CPL" ] with
        | [ GroupWithoutMatch (SplitKeySetupErr (UnknownSetupSlotErr reason)) ] ->
            Assert.Contains("'CPL-(QZ#0-LR#0)-CPL'", reason)
            Assert.Contains("'Obj 2'", reason)
        | other -> Assert.Fail($"without sampleB: expected SplitKeySetupErr, got %A{other}")

        let held = DataRowKey "CPL-(QZ#45-LR#45)-CPL"
        let again = DataRowKey "CPL-(QZ#45.0-LR#45)-CPL"

        match matchingsOf kitParam [ held; again ] with
        | [ ExperimentForGroup matched; GroupWithoutMatch (SplitKeyTwiceErr reason) ] ->
            Assert.Equal<ExperimentName>(qz45Lr45, matched)
            Assert.Contains($"'{held.value}'", reason)
            Assert.Contains($"'{again.value}'", reason)
            Assert.Contains($"'{qz45Lr45.value}'", reason)
        | other -> Assert.Fail($"expected one match and a SplitKeyTwiceErr, got %A{other}")

        // Every group is listed in file order, and each filter keeps that order.
        let keys =
            [ held; DataRowKey "Darkness"; DataRowKey "CPL-(QZ#90)-CPL-2"; again; DataRowKey "e2"; DataRowKey "CPL-(QZ#0)-CPL" ]

        let split = splitOf kitParam keys
        let keysOf (groups : SplitGroup list) : DataRowKey list = groups |> List.map (fun group -> group.key)
        Assert.Equal<DataRowKey list>(keys, keysOf split.groups)
        Assert.Equal<DataRowKey list>([ keys[0]; keys[4] ], keysOf split.matched)
        Assert.Equal<DataRowKey list>([ keys[2]; keys[5] ], keysOf split.offered)
        Assert.Equal<DataRowKey list>([ keys[1]; keys[3] ], keysOf split.unmatched)

    [<Fact>]
    let ``every split error renders its condition and its payload`` () : unit =
        let renders (expected : string) (error : DataSplitError) : string =
            Assert.Equal(expected, error.describe)
            error.describe

        let texts =
            [
                SplitKeyTextErr (InvalidExperimentNameErr "the blank key")
                |> renders "the key is no experiment name: the experiment name is not valid: the blank key"
                SplitKeyNameErr (NameMismatchErr "'Darkness' against the lab template")
                |> renders "the key does not read through the naming template: the name does not fit the naming template: 'Darkness' against the lab template"
                SplitKeySetupErr (UnknownSetupSlotErr "'Obj 2' of the bench")
                |> renders "the table cannot hold the key's setup: the table has no such slot: 'Obj 2' of the bench"
                SplitKeyTwiceErr "'K1' and 'k1' both match 'K1'"
                |> renders "two groups match one experiment: 'K1' and 'k1' both match 'K1'"
            ]

        Assert.Equal(texts.Length, (List.distinct texts).Length)
