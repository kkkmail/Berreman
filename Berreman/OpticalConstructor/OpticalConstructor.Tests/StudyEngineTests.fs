namespace OpticalConstructor.Tests

open System
open System.Threading
open Berreman.Constants
open Berreman.Fields
open Berreman.Geometry
open Berreman.Media
open Berreman.MaterialProperties
open Berreman.Solvers
open OpticalConstructor.Domain
open OpticalConstructor.Domain.CrystalPhysics
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.ItemModels
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.Placement
open OpticalConstructor.Domain.MeasuredData
open OpticalConstructor.Domain.MuellerReconstruction
open OpticalConstructor.Domain.MuellerInverse
open OpticalConstructor.Domain.Experiments
open OpticalConstructor.Domain.Setups
open OpticalConstructor.Domain.ImportProfiles
open OpticalConstructor.Domain.Studies
open OpticalConstructor.Domain.ExperimentData
open OpticalConstructor.Domain.StudyForward
open OpticalConstructor.Domain.FitRuns
open OpticalConstructor.Optimization.OptimizationInterface
open OpticalConstructor.Optimization.MuellerFitting
open OpticalConstructor.Optimization.BenchNoise
open OpticalConstructor.Optimization.StagedMuellerPath
open OpticalConstructor.Optimization.StudyEngine
open Xunit

/// The study engine's facts: the scaled search, the engine's automatic choice by the shape of a staged study, and the
/// nonlinear engine (spec 0050 S11.3).
module StudyEngineTests =

    /// r(v) = [v0 − 5], whose unbounded minimum is v0 = 5. Every point it is evaluated at is recorded, in order.
    let private offsetByFive (evaluated : ResizeArray<float[]>) (ScaledVector v) : ResidualVector =
        evaluated.Add(Array.copy v)
        ResidualVector [| v.[0] - 5.0 |]

    let private solvedFrom (search : ScaledSearch) : float[] * ResizeArray<float[]> =
        let evaluated = ResizeArray<float[]>()
        match search.solveWith UnwatchedIterations (offsetByFive evaluated) with
        | Ok solution -> solution.solution.value, evaluated
        | Error e -> failwith $"expected Ok, got %A{e}"

    [<Fact>]
    let ``a scaled search starts where it is told and keeps to its own box`` () =
        let narrow : ScaledSearch =
            { start = ScaledVector [| 0.5 |]; lowerBounds = ScaledVector [| -1.0 |]; upperBounds = ScaledVector [| 2.0 |] }
        let wide : ScaledSearch =
            { narrow with lowerBounds = ScaledVector [| -10.0 |]; upperBounds = ScaledVector [| 10.0 |] }

        let pinned, narrowEvaluated = solvedFrom narrow
        Assert.Equal(1, pinned.Length)
        Assert.Equal(2.0, pinned.[0], 1e-9)

        let free, wideEvaluated = solvedFrom wide
        Assert.Equal(1, free.Length)
        Assert.Equal(5.0, free.[0], 1e-9)

        // ALGLIB evaluates the start first, and its bounded LM (`minlmsetbc`) evaluates only inside the box or on it.
        let runs : (ScaledSearch * ResizeArray<float[]>) list = [ narrow, narrowEvaluated; wide, wideEvaluated ]
        for search, evaluated in runs do
            Assert.NotEmpty evaluated
            Assert.Equal<float[]>(search.start.value, evaluated.[0])
            for point in evaluated do
                Assert.InRange(point.[0], search.lowerBounds.value.[0], search.upperBounds.value.[0])

    [<Fact>]
    let ``a search box's centred search starts at zero within plus or minus its half-width`` () =
        let parameters = CrystalMaterialFixtures.uniaxialParameters
        let threeAxes = scalingAround (List.take 3 UniaxialParameters.axes) parameters parameters
        let search = (SearchBox 50.0).centredSearch threeAxes
        Assert.Equal<float[]>([| 0.0; 0.0; 0.0 |], search.start.value)
        Assert.Equal<float[]>([| -50.0; -50.0; -50.0 |], search.lowerBounds.value)
        Assert.Equal<float[]>([| 50.0; 50.0; 50.0 |], search.upperBounds.value)

    // ============================ spec 0050 S11.3: the engine's automatic choice ============================

    let private polarizerPreset (id : ElementValueId) : PolarizerPreset =
        match BuiltinCatalogue.preset id with
        | PolarizerItem polarizer -> polarizer
        | SampleItem _
        | SourceItem _
        | DetectorItem _ -> failwith "the Built-in preset is not a polarizer"

    let private idealPolarizerModel : ItemModelVersion = BuiltinCatalogue.itemModel ItemModelIds.idealPolarizerItemModelId
    let private idealLpChoice : ItemModelChoice = PolarizerPresetChoice (idealPolarizerModel, polarizerPreset PresetIds.polarizerLp)
    let private idealCpLeftChoice : ItemModelChoice = PolarizerPresetChoice (idealPolarizerModel, polarizerPreset PresetIds.polarizerCpLeft)
    let private idealCpRightChoice : ItemModelChoice = PolarizerPresetChoice (idealPolarizerModel, polarizerPreset PresetIds.polarizerCpRight)
    let private realLinearChoice : ItemModelChoice = LibraryModelChoice (BuiltinCatalogue.itemModel ItemModelIds.realLinearPolarizerModelId)
    let private circularChoice : ItemModelChoice = LibraryModelChoice (BuiltinCatalogue.itemModel ItemModelIds.circularPolarizerModelId)
    let private retarderChoice : ItemModelChoice = LibraryModelChoice (BuiltinCatalogue.itemModel ItemModelIds.retarderPlateModelId)
    let private blackBoxChoice : ItemModelChoice = LibraryModelChoice (BuiltinCatalogue.itemModel ItemModelIds.blackBoxMuellerModelId)
    let private intensityChoice : ItemModelChoice = LibraryModelChoice (BuiltinCatalogue.itemModel ItemModelIds.intensityDetectorModelId)
    let private polarimeterChoice : ItemModelChoice = LibraryModelChoice (BuiltinCatalogue.itemModel ItemModelIds.muellerPolarimeterModelId)

    let private laserChoice : ItemModelChoice =
        SourcePresetChoice (BuiltinCatalogue.itemModel ItemModelIds.sourcePresetItemModelId, BuiltinCatalogue.source PresetIds.heNe633)

    /// A test-local constant-Mueller preset bound through the shipped ideal-polarizer model: a reference LP, then a
    /// quarter-wave retarder at 45°.
    let private constantMuellerChoice : ItemModelChoice =
        let preset : PolarizerPreset =
            {
                id = ElementValueId "38600000-0000-4000-8000-000000000100"
                name = EntryName "Constant-Mueller circular polarizer"
                behavior =
                    ConstantMueller
                        [
                            ({ matrix = Propagation.analyzerMueller IdealLinear Angle.zero; offset = Angle.zero } : MuellerComponent)
                            ({ matrix = retarderMueller Angle.zero (Retardance.degree 90.0<deg>); offset = Angle.degree 45.0<deg> } : MuellerComponent)
                        ]
                category = CustomMueller
                protection = UserManaged
            }

        PolarizerPresetChoice (idealPolarizerModel, preset)

    /// The test-local uniaxial crystal version the polarimeter study opens, and its 1 mm plate.
    let private crystalVersion : MaterialVersionId =
        MaterialVersionId.firstOf (MaterialId (Guid.Parse "38600000-0000-4000-8000-000000000201"))

    let private crystalEntry : MaterialEntry =
        {
            id = crystalVersion.materialId
            name = EntryName "Probe uniaxial crystal"
            category = CategoryIds.crystal
            description = None
            physics = CrystalParameterPhysics (UniaxialCrystal CrystalMaterialFixtures.uniaxialParameters)
        }

    let private crystalPlate : Sample =
        {
            id = SampleId (Guid.Parse "38600000-0000-4000-8000-000000000202")
            name = EntryName "Probe crystal plate"
            structure =
                {
                    films = []
                    substrate = Some { materialId = crystalVersion; thickness = Thickness.mm 1.0<mm>; orientation = PrimaryAxes }
                    lower = None
                }
            substrate = ThickPlate
            description = EntryDescription "A 1 mm plate of the probe uniaxial crystal"
            supportedEmission = ThickPlate.defaultSupportedEmission
        }

    let private crystalPlateChoice : ItemModelChoice =
        StackSampleChoice (BuiltinCatalogue.itemModel ItemModelIds.stackSampleItemModelId, SampleVersionId.firstOf crystalPlate.id, crystalPlate)

    /// The index probe the nonlinear fit recovers, step 379's observed crystal: n_o 1.544, n_e 1.553 and no gyration.
    let private indexTruth : UniaxialParameters =
        {
            ordinaryIndex = RefractionIndex 1.544
            extraordinaryIndex = RefractionIndex 1.553
            g11 = RhoValue 0.0
            g33 = RhoValue 0.0
        }

    /// The test-local uniaxial version of the index probe, and its 100 µm plate.
    let private indexVersion : MaterialVersionId =
        MaterialVersionId.firstOf (MaterialId (Guid.Parse "38700000-0000-4000-8000-000000000201"))

    let private indexEntry : MaterialEntry =
        {
            id = indexVersion.materialId
            name = EntryName "Index probe crystal"
            category = CategoryIds.crystal
            description = None
            physics = CrystalParameterPhysics (UniaxialCrystal indexTruth)
        }

    let private indexPlate : Sample =
        {
            id = SampleId (Guid.Parse "38700000-0000-4000-8000-000000000202")
            name = EntryName "Index probe plate"
            structure =
                {
                    films = []
                    substrate = Some { materialId = indexVersion; thickness = Thickness.mkm 100.0<mkm>; orientation = PrimaryAxes }
                    lower = None
                }
            substrate = ThickPlate
            description = EntryDescription "A 100 µm plate of the index probe crystal"
            supportedEmission = ThickPlate.defaultSupportedEmission
        }

    let private indexPlateChoice : ItemModelChoice =
        StackSampleChoice (BuiltinCatalogue.itemModel ItemModelIds.stackSampleItemModelId, SampleVersionId.firstOf indexPlate.id, indexPlate)

    let private draftOf (number : int) (label : string) (model : ItemModelChoice) (dial : ItemDial) : ItemDraft =
        {
            id = ItemId (Guid.Parse $"38600000-0000-4000-8000-%012d{number}")
            label = ItemLabel label
            model = model
            dial = dial
        }

    // Every model is a shipped entry of step 210.
    let private laserDraft : ItemDraft = draftOf 1 "HeNe" laserChoice FixedMounting
    let private lp1Draft : ItemDraft = draftOf 2 "LP1" idealLpChoice FixedMounting
    let private lp2Draft : ItemDraft = draftOf 3 "LP2" idealLpChoice (DialMounted DialPositive)
    let private cpl1Draft : ItemDraft = draftOf 4 "CPL1" circularChoice FixedMounting
    let private cpl2Draft : ItemDraft = draftOf 5 "CPL2" circularChoice (DialMounted DialPositive)
    let private phoneDraft : ItemDraft = draftOf 6 "Phone" intensityChoice FixedMounting
    let private qzDraft : ItemDraft = draftOf 7 "QZ" blackBoxChoice FixedMounting
    let private lrDraft : ItemDraft = draftOf 8 "LR" blackBoxChoice FixedMounting
    let private qz2Draft : ItemDraft = draftOf 9 "QZ2" blackBoxChoice FixedMounting
    let private cpLeftDraft : ItemDraft = draftOf 10 "CP left" idealCpLeftChoice FixedMounting
    let private cpRightDraft : ItemDraft = draftOf 11 "CP right" idealCpRightChoice FixedMounting
    let private constantDraft : ItemDraft = draftOf 12 "CMP" constantMuellerChoice FixedMounting
    let private plateDraft : ItemDraft = draftOf 13 "WP" retarderChoice FixedMounting
    let private polarimeterDraft : ItemDraft = draftOf 14 "Polarimeter" polarimeterChoice FixedMounting
    let private crystalPlateDraft : ItemDraft = draftOf 15 "Crystal" crystalPlateChoice FixedMounting
    let private indexPlateDraft : ItemDraft = draftOf 16 "Index plate" indexPlateChoice FixedMounting

    /// LP2 as the reference's stage one reads it: the real linear polarizer on a negative dial.
    let private realLp2Draft : ItemDraft = draftOf 17 "LP2" realLinearChoice (DialMounted DialNegative)

    let private drafts : ItemDraft list =
        [
            laserDraft; lp1Draft; lp2Draft; cpl1Draft; cpl2Draft; phoneDraft; qzDraft; lrDraft; qz2Draft
            cpLeftDraft; cpRightDraft; constantDraft; plateDraft; polarimeterDraft; crystalPlateDraft; indexPlateDraft
            realLp2Draft
        ]

    /// Each item's model as its draft chose it.
    let private choiceOf (item : Item) : ItemModelChoice option =
        drafts |> List.tryFind (fun draft -> draft.id = item.id) |> Option.map (fun draft -> draft.model)

    /// The draft's new item, failing with the refusal's text.
    let private itemOf (draft : ItemDraft) : Item =
        match draft.addTo [] with
        | Ok item -> item
        | Error error -> failwith $"'{draft.label.value}' was refused: {error.describe}"

    /// The item with one parameter in that state, failing with the refusal's text.
    let private stating (name : ParameterName) (state : ParameterState) (item : Item) : Item =
        match item.withParameterState name state with
        | Ok stated -> stated
        | Error error -> failwith $"'{item.label.value}' refused {name.value}: {error.describe}"

    let private unknownWithin (start : float) (lower : float) (upper : float) : ParameterState =
        UnknownParameter
            {
                start = ParameterValue start
                bounds = { lower = ParameterValue lower; upper = ParameterValue upper }
                priorOpt = None
            }

    let private calibratedAt (value : float) : ParameterState =
        CalibratedParameter
            {
                value = ParameterValue value
                sigma = ParameterSigma 0.001
                fitRun = FitRunName "AIR@20261101T120000Z"
            }

    /// The item at its next revision, each parameter published in its state, failing with the refusal's text.
    let private revisedWith (published : Parameter list) (item : Item) : Item =
        match item.publishRevision published with
        | Ok revised -> revised
        | Error error -> failwith $"'{item.label.value}' refused its calibration: {error.describe}"

    /// The item at its next revision, each named parameter published as a calibration of that value.
    let private publishing (values : (ParameterName * float) list) (item : Item) : Item =
        item |> revisedWith (values |> List.map (fun (name, value) -> ({ name = name; state = calibratedAt value } : Parameter)))

    let private retardance : ParameterName = ParameterName "c2.retardance"
    let private fastAxisOffset : ParameterName = ParameterName "c2.fastAxisOffset"
    let private quarterWave : float = Math.PI / 2.0

    let private coefficient (row : int) (column : int) : ParameterName =
        ParameterName.muellerElement (ParameterNameIndex row) (ParameterNameIndex column)

    let private coefficientPlaces : (int * int) list = [ for row in 0 .. 3 do for column in 0 .. 3 -> row, column ]

    let private laser : Item = itemOf laserDraft
    let private lp1 : Item = itemOf lp1Draft

    /// The Phone at gain 1000 and dark offset 5, both known.
    let private phone : Item =
        itemOf phoneDraft
        |> stating ParameterName.gain (KnownParameter (ParameterValue 1000.0))
        |> stating ParameterName.darkOffset (KnownParameter (ParameterValue 5.0))

    /// The Phone with its gain free.
    let private freeGainPhone : Item = phone |> stating ParameterName.gain (unknownWithin 1000.0 100.0 10000.0)

    // The instruments with their stage-1 constants unknown, then as stage 1 publishes them: LP2's and CPL2's dial zeros at
    // 0, CPL2's and CPL1's retardances a quarter wave and CPL1's fast-axis offset 0, the truth the AIR rows are made of.
    let private lp2Unknown : Item = itemOf lp2Draft |> stating ParameterName.dialZero (unknownWithin 0.0 (-0.5) 0.5)

    let private cpl2Unknown : Item =
        itemOf cpl2Draft
        |> stating ParameterName.dialZero (unknownWithin 0.0 (-0.5) 0.5)
        |> stating retardance (unknownWithin quarterWave 1.0 2.0)

    let private cpl1Unknown : Item =
        itemOf cpl1Draft
        |> stating fastAxisOffset (unknownWithin 0.0 (-0.5) 0.5)
        |> stating retardance (unknownWithin quarterWave 1.0 2.0)

    let private realLp2Unknown : Item = itemOf realLp2Draft |> stating ParameterName.dialZero (unknownWithin 0.0 (-0.5) 0.5)

    let private lp2 : Item = itemOf lp2Draft |> publishing [ ParameterName.dialZero, 0.0 ]
    let private cpl1 : Item = itemOf cpl1Draft |> publishing [ fastAxisOffset, 0.0; retardance, quarterWave ]

    let private cpLeft : Item = itemOf cpLeftDraft
    let private cpRight : Item = itemOf cpRightDraft
    let private constantPrep : Item = itemOf constantDraft

    /// A quarter-wave plate at its own axis, every parameter known at its default.
    let private plate : Item = itemOf plateDraft

    let private polarimeter : Item = itemOf polarimeterDraft
    let private crystalItem : Item = itemOf crystalPlateDraft
    let private indexItem : Item = itemOf indexPlateDraft

    // The synthetic truths the observations are made of (step 390's QZ and LR), QZ2 sharing QZ's retardance, and the plate.
    let private qzTruth : MuellerMatrix = retarderMueller (Angle.degree 30.0<deg>) (Retardance.degree 40.0<deg>)
    let private lrTruth : MuellerMatrix = retarderMueller (Angle.degree 10.0<deg>) (Retardance.degree 25.0<deg>)
    let private qz2Truth : MuellerMatrix = retarderMueller (Angle.degree 10.0<deg>) (Retardance.degree 40.0<deg>)
    let private plateTruth : MuellerMatrix = retarderMueller Angle.zero (Retardance.degree 90.0<deg>)

    let private knownIn (truth : MuellerMatrix) (row : int) (column : int) : ParameterState =
        KnownParameter (ParameterValue (truth.element row column))

    /// The black box with every coefficient unknown from the identity within [−2, 2], then each listed one in its state.
    let private blackBoxWith (draft : ItemDraft) (states : (ParameterName * ParameterState) list) : Item =
        let free =
            coefficientPlaces
            |> List.fold
                (fun item (row, column) -> item |> stating (coefficient row column) (unknownWithin (if row = column then 1.0 else 0.0) (-2.0) 2.0))
                (itemOf draft)

        states |> List.fold (fun item (name, state) -> stating name state item) free

    /// The black box with every coefficient known at the truth's element.
    let private knownBlackBox (draft : ItemDraft) (truth : MuellerMatrix) : Item =
        blackBoxWith draft (coefficientPlaces |> List.map (fun (row, column) -> coefficient row column, knownIn truth row column))

    let private qzAllFree : Item = blackBoxWith qzDraft []
    let private qzM33Known : Item = blackBoxWith qzDraft [ coefficient 3 3, knownIn qzTruth 3 3 ]

    let private qzM23KnownM32Calibrated : Item =
        blackBoxWith qzDraft [ coefficient 2 3, knownIn qzTruth 2 3; coefficient 3 2, calibratedAt (qzTruth.element 3 2) ]

    let private qzM00Free : Item =
        coefficientPlaces
        |> List.filter (fun place -> place <> (0, 0))
        |> List.map (fun (row, column) -> coefficient row column, knownIn qzTruth row column)
        |> blackBoxWith qzDraft

    let private lrFree : Item = blackBoxWith lrDraft []
    let private lrKnown : Item = knownBlackBox lrDraft lrTruth
    let private qz2Free : Item = blackBoxWith qz2Draft []
    let private qz2Known : Item = knownBlackBox qz2Draft qz2Truth

    let private emptySlot (id : string) (kind : CatalogueKind) (point : TablePoint) : SetupSlot =
        {
            id = Library.ElementId id
            placement = ElementPlacement.create kind point
            zoom = BeamLayout.ElementZoom 5.0
            beam = BeamLayout.OnMainBeam
            content = EmptySlotContent
        }

    /// The slot holding the item as `AssignSlot` places it, failing with the refusal's text.
    let private holding (item : Item) (slot : SetupSlot) : SetupSlot =
        match choiceOf item with
        | None -> failwith $"'{item.label.value}' has no draft"
        | Some model ->
            match slot.withItem item model with
            | Ok held -> held
            | Error error -> failwith error.describe

    let private alongBeam (x : float<meter>) : TablePoint = { x = x; y = 0.0<meter> }

    /// A polarizer slot at x of a kind the item fits, holding it.
    let private opticSlot (id : string) (x : float<meter>) (item : Item) : SetupSlot =
        let kinds = choiceOf item |> Option.map (fun model -> model.slotKinds) |> Option.defaultValue []
        let kind = if List.contains LinearPolarizer kinds then LinearPolarizer else CircularPolarizer
        emptySlot id kind (alongBeam x) |> holding item

    let private setupOf (name : string) (slots : SetupSlot list) : Setup =
        match Setup.create (SetupName name) slots [] with
        | Ok setup -> setup
        | Error error -> failwith $"'{name}' was refused: {error.describe}"

    /// The k-th object slot, from 0, in light order between the prep and the analyzer.
    let private objectSlotAt (k : int) : SetupSlot = emptySlot $"object{k + 1}" Sample (alongBeam (-0.2<meter> + float k * 0.1<meter>))

    /// The HeNe, the prep, these object slots, the analyzer and the detector.
    let private benchWith (name : string) (prep : Item) (objectSlots : SetupSlot list) (analyzer : Item) (detector : Item) : Setup =
        setupOf
            name
            ([ emptySlot "src" LightSource RayModel.defaultSourcePoint |> holding laser; opticSlot "prep" (-0.4<meter>) prep ]
             @ objectSlots
             @ [ opticSlot "analyzer" 0.4<meter> analyzer; emptySlot "det" Detector RayModel.defaultDetectorPoint |> holding detector ])

    /// The HeNe, the prep, the object slots in light order (an empty one holds nothing), the analyzer and the detector.
    let private benchOf (name : string) (prep : Item) (objects : Item option list) (analyzer : Item) (detector : Item) : Setup =
        let objectSlots =
            objects
            |> List.mapi (fun k objectOpt ->
                match objectOpt with
                | Some item -> holding item (objectSlotAt k)
                | None -> objectSlotAt k)

        benchWith name prep objectSlots analyzer detector

    // The AIR benches of the three calibration families, and the Science benches.
    let private lpLpAir : Setup = benchOf "LP-AIR-LP" lp1 [ None ] lp2 phone
    let private lpCplAir : Setup = benchOf "LP-AIR-CPL" lp1 [ None ] cpl2Unknown phone
    let private cplLpAir : Setup = benchOf "CPL-AIR-LP" cpl1 [ None ] lp2 phone
    let private qzBehindCpl : Setup = benchOf "CPL-QZ-LP" cpl1 [ Some qzAllFree ] lp2 phone
    let private qzBehindLp : Setup = benchOf "LP-QZ-LP" lp1 [ Some qzAllFree ] lp2 phone
    let private qzBehindCpLeft : Setup = benchOf "CP left-QZ-LP" cpLeft [ Some qzAllFree ] lp2 phone
    let private qzBehindCpRight : Setup = benchOf "CP right-QZ-LP" cpRight [ Some qzAllFree ] lp2 phone
    let private qzBehindConstant : Setup = benchOf "CMP-QZ-LP" constantPrep [ Some qzAllFree ] lp2 phone
    let private qzSurrounded : Setup = benchOf "LP-WP-QZ-LR-LP" lp1 [ Some plate; Some qzAllFree; Some lrKnown ] lp2 phone
    let private qz2BehindCpl : Setup = benchOf "CPL-QZ2-LP" cpl1 [ Some qz2Free ] lp2 phone
    let private qz2BehindLp : Setup = benchOf "LP-QZ2-LP" lp1 [ Some qz2Free ] lp2 phone
    let private seriesBehindCpl : Setup = benchOf "CPL-QZ-LR-LP" cpl1 [ Some qzAllFree; Some lrFree ] lp2 phone
    let private seriesBehindLp : Setup = benchOf "LP-QZ-LR-LP" lp1 [ Some qzAllFree; Some lrFree ] lp2 phone
    let private qzThenQz2 : Setup = benchOf "LP-QZ-QZ2-LP" lp1 [ Some qzAllFree; Some qz2Known ] lp2 phone
    let private qzTwice : Setup = benchOf "LP-QZ-QZ-LP" lp1 [ Some qzAllFree; Some qzAllFree ] lp2 phone

    // Stage one's AIR benches behind the real LP2; LP1 -> CPL2 is `lpCplAir`.
    let private lp1AirRealLp2 : Setup = benchOf "LP1-AIR-LP2" lp1 [ None ] realLp2Unknown phone
    let private cpl1AirRealLp2 : Setup = benchOf "CPL1-AIR-LP2" cpl1Unknown [ None ] realLp2Unknown phone

    /// The HeNe, the crystal plate as the lone object, and the polarimeter.
    let private crystalBench : Setup =
        setupOf
            "Crystal plate"
            [
                emptySlot "src" LightSource RayModel.defaultSourcePoint |> holding laser
                emptySlot "object1" Sample (alongBeam 0.0<meter>) |> holding crystalItem
                emptySlot "det" Detector RayModel.defaultDetectorPoint |> holding polarimeter
            ]

    /// The HeNe, the index probe's plate as the lone object, and the polarimeter.
    let private indexBench : Setup =
        setupOf
            "Index plate"
            [
                emptySlot "src" LightSource RayModel.defaultSourcePoint |> holding laser
                emptySlot "object1" Sample (alongBeam 0.0<meter>) |> holding indexItem
                emptySlot "det" Detector RayModel.defaultDetectorPoint |> holding polarimeter
            ]

    let private benches : Setup list =
        [
            lpLpAir; lpCplAir; cplLpAir; qzBehindCpl; qzBehindLp; qzBehindCpLeft; qzBehindCpRight; qzBehindConstant; qzSurrounded
            qz2BehindCpl; qz2BehindLp; seriesBehindCpl; seriesBehindLp; qzThenQz2; qzTwice; crystalBench; indexBench
            lp1AirRealLp2; cpl1AirRealLp2
        ]

    // The reference projection the rows are made of: a source's state, the analyzer's row at each dial reading.
    let private lpAnalyzer : AnalyzerModel =
        {
            kind = IdealLinear
            zeroDial = Angle.zero
            dialSign = DialPositive
            thetaRelOpt = None
            retardanceOpt = None
        }

    let private cplAnalyzer : AnalyzerModel =
        {
            lpAnalyzer with
                kind = IdealCircularLeft
                thetaRelOpt = Some (Angle.degree 45.0<deg>)
                retardanceOpt = Some (Retardance.degree (-90.0<deg>))
        }

    let private circularSource : SourceModel = CplSource (Angle.degree 45.0<deg>, Retardance.degree 90.0<deg>)
    let private rightCircularSource : SourceModel = CplSource (Angle.degree 45.0<deg>, Retardance.degree (-90.0<deg>))

    let private dialReadings : Angle list = [ 0.0 .. 20.0 .. 160.0 ] |> List.map (fun reading -> Angle.degree (reading * 1.0<deg>))

    /// The Phone's capture rows over the analyzer's dial: 1000 · the prediction at each reading + 5.
    let private phoneRowsOf (label : string) (predictedAt : Angle -> float) : MuellerRawRow list =
        dialReadings
        |> List.mapi (fun k reading ->
            ({
                experiment = MuellerExperimentLabel label
                captureIndex = CaptureIndex (k + 1)
                capturedAt = DateTimeOffset(2026, 11, 13, 10, 0, 0, TimeSpan.Zero).AddSeconds(float k)
                description = reading
                avgTotal = MeasuredIntensity (1000.0 * predictedAt reading + 5.0)
            } : MuellerRawRow))

    /// The Phone's capture rows over the analyzer's dial: 1000 · (designRow s a · M.vecColumnMajor) + 5, the source's state
    /// through `matrix` onto the analyzer's row at each reading.
    let private capturesOf (label : string) (source : SourceModel) (analyzer : AnalyzerModel) (matrix : MuellerMatrix) : MuellerRawRow list =
        let m = matrix.vecColumnMajor

        phoneRowsOf label (fun reading ->
            let (DesignRow row) = designRow source.baseStokes (analyzerBaseRow analyzer reading)
            [ 0 .. 15 ] |> List.sumBy (fun i -> row.[i] * m.[i]))

    /// A run and the points its attached file reads.
    type private AttachedRun =
        {
            attachedRun : Experiment
            attachedPoints : MappedPoints
        }

    let private analyzerTurn : ScanAxis =
        {
            elementId = Library.ElementId "analyzer"
            label = ElementLabel "Analyzer"
            variable = VaryDialReading
            range = VariableRange.forVariable VaryDialReading
        }

    /// A run of that role over the setup turning the analyzer's dial, its file read through the profile as the points.
    let private attachedOver (setup : Setup) (name : string) (role : ExperimentRole) (profile : ImportProfileName) (points : MappedPoints) : AttachedRun =
        let data : MappedAttachment =
            {
                attachment = { path = KitRelativePath $"data/{name}.csv"; contentHash = LibraryEntryFiles.ContentHash "0386" }
                profile = profile
                rows = AllDataRows
                exclusions = []
            }

        let run : Experiment =
            {
                id = ExperimentId 1
                name = ExperimentName name
                setup = setup.reference
                scan = ExperimentScan.oneAxis analyzerTurn CaptureTransmitted None
                role = role
                sessionOpt = None
                notes = ExperimentNotes.none
                dataOpt = Some data
                corrections = []
            }

        { attachedRun = run; attachedPoints = points }

    /// A run over the setup whose Phone ROI CSV holds the rows.
    let private capturedOver (setup : Setup) (name : string) (role : ExperimentRole) (rows : MuellerRawRow list) : AttachedRun =
        attachedOver setup name role ImportProfileName.phoneRoiCsv (CapturePoints rows)

    /// K1, the Dark run on a dark line over the setup: three captures of the dark level 5.
    let private darkOver (setup : Setup) : AttachedRun =
        let darkRows : MuellerRawRow list =
            [ 1 .. 3 ]
            |> List.map (fun k ->
                {
                    experiment = MuellerExperimentLabel "K1"
                    captureIndex = CaptureIndex k
                    capturedAt = DateTimeOffset(2026, 11, 13, 9, 0, k, TimeSpan.Zero)
                    description = Angle.zero
                    avgTotal = MeasuredIntensity 5.0
                })

        let captured = capturedOver setup "K1" DarkRun darkRows
        { captured with attachedRun = { captured.attachedRun with scan = ExperimentScan.darkLine CaptureTransmitted } }

    /// A data seam answering each run's attached file by its points, unchanged since attach; nothing is picked or attached.
    let private dataOver (runs : AttachedRun list) : ExperimentDataProxy =
        let pointsAt (path : KitRelativePath) : MappedPoints option =
            runs
            |> List.tryPick (fun attached ->
                match attached.attachedRun.dataOpt with
                | Some data when data.attachment.path = path -> Some attached.attachedPoints
                | Some _
                | None -> None)

        {
            readPickedFile = fun file -> Error (KitIoErr $"the test reads no picked file '{file.value}'")
            attachDataFile = fun file -> Error (KitIoErr $"the test attaches no file '{file.value}'")
            attachmentStatus = fun _ -> Ok AttachedDataUnchanged
            readMapped =
                fun data ->
                    match pointsAt data.attachment.path with
                    | Some points -> Ok points
                    | None -> Error (MalformedDataFileErr $"the test holds no file '{data.attachment.path.value}'")
        }

    let private studyOf (result : Result<Study, StudyError>) : Study =
        match result with
        | Ok study -> study
        | Error error -> failwith $"expected a study, got {error.describe}"

    /// The study of the runs, in order.
    let private studyOver (name : string) (runs : AttachedRun list) : Study =
        let experiments = runs |> List.map (fun attached -> attached.attachedRun)
        studyOf (Study.create (StudyName name) (experiments |> List.map (fun run -> run.name)) [] experiments)

    /// The forward context over a kit of these items, these setups and the runs holding the study, the Built-in library
    /// and materials, and the runs' files.
    let private contextWithin (setups : Setup list) (items : Item list) (runs : AttachedRun list) (study : Study) : StudyForwardContext =
        let kit : StudyContext =
            {
                studies = [ study ]
                experiments = runs |> List.map (fun attached -> attached.attachedRun)
                setups = setups
                items = items
                resolveMaterial = fun version -> Error (MaterialUnavailableErr $"material {version.materialId.value} is not the test's")
            }

        {
            kit = kit
            library = BuiltinCatalogue.library ()
            materials = BuiltinCatalogue.materialStore (fun _ -> [])
            experimentData = dataOver runs
            choiceOf = choiceOf
            solverParameters = SolverParameters.defaultValue
        }

    /// The forward context over a kit of these items, every bench and the runs holding the study.
    let private contextOver (items : Item list) (runs : AttachedRun list) (study : Study) : StudyForwardContext =
        contextWithin benches items runs study

    /// The study staged as asked, its calibrations used so, and its pre-fit over the staged items.
    let private prefitStaged
        (context : StudyForwardContext)
        (study : Study)
        (stage : FitStage)
        (calibrationUse : CalibrationUse)
        : StagedStudy * ExperimentPrefit list =
        let staged =
            match study.stageWith context.kit stage calibrationUse with
            | Ok staged -> staged
            | Error error -> failwith $"'{study.name.value}' was not staged for {stage.text}: {error.describe}"

        let stagedContext = { context with kit = { context.kit with items = staged.items } }

        match staged.study.prefitWith stagedContext [] with
        | Ok prefits -> staged, prefits
        | Error error -> failwith $"'{study.name.value}' has no pre-fit: {error.describe}"

    /// The engine `engineWith` chooses for the study staged with its calibrations held, once every run its pre-fit reads is
    /// ready and its layout holds an entry, so no answer comes from an earlier refusal.
    let private engineOf (context : StudyForwardContext) (study : Study) (stage : FitStage) : StudyEngineKind =
        let staged, prefits = prefitStaged context study stage CalibrationHeldFixed
        let stagedContext = { context with kit = { context.kit with items = staged.items } }

        Assert.NotEmpty(prefits)

        for prefit in prefits do
            match prefit.check with
            | Ok _ -> ()
            | Error error -> Assert.Fail($"{prefit.experiment.name.value} is not ready: {error.describe}")

        Assert.NotEmpty((staged.study.parameterLayoutWith stagedContext).entries)
        staged.engineWith context prefits

    /// The engine for a study of the runs over a kit of the items, staged as asked.
    let private engineOver (items : Item list) (runs : AttachedRun list) (stage : FitStage) : StudyEngineKind =
        let study = studyOver "Engine" runs
        engineOf (contextOver items runs study) study stage

    let private objectStages : FitStage list = [ ObjectStage; JointStage ]

    [<Fact>]
    let ``a polarizer item is classified linear or circular by its model`` () : unit =
        Assert.Equal<CalibrationPolarizer option>(Some LinearCalibrationPolarizer, idealLpChoice.calibrationPolarizerOpt)
        Assert.Equal<CalibrationPolarizer option>(Some LinearCalibrationPolarizer, realLinearChoice.calibrationPolarizerOpt)
        Assert.Equal<CalibrationPolarizer option>(Some CircularCalibrationPolarizer, circularChoice.calibrationPolarizerOpt)

        // A retarder, a black box, either ideal CP, a constant-Mueller compound, a source and a detector are no reference
        // family's polarizer.
        for choice in [ retarderChoice; blackBoxChoice; idealCpLeftChoice; idealCpRightChoice; constantMuellerChoice; laserChoice; intensityChoice ] do
            Assert.Equal<CalibrationPolarizer option>(None, choice.calibrationPolarizerOpt)

        Assert.Equal(LpLp, LinearCalibrationPolarizer.familyWith LinearCalibrationPolarizer)
        Assert.Equal(LpCpl, LinearCalibrationPolarizer.familyWith CircularCalibrationPolarizer)
        Assert.Equal(CplLp, CircularCalibrationPolarizer.familyWith LinearCalibrationPolarizer)
        Assert.Equal(CplCpl, CircularCalibrationPolarizer.familyWith CircularCalibrationPolarizer)

    [<Fact>]
    let ``the calibration stage of LP and CPL AIR runs over capture rows is linear`` () : unit =
        let identity = Propagation.identityMueller
        let a1 = capturedOver lpLpAir "A1" CalibrationRun (capturesOf "LP-(AIR#0)-LP" LpSource lpAnalyzer identity)
        let a2 = capturedOver lpCplAir "A2" CalibrationRun (capturesOf "LP-(AIR#0)-CPL" LpSource cplAnalyzer identity)
        let a3 = capturedOver cplLpAir "A3" CalibrationRun (capturesOf "CPL-(AIR#0)-LP" circularSource lpAnalyzer identity)
        let k1 = darkOver lpLpAir
        let instruments = [ laser; lp1; lp2Unknown; cpl2Unknown; cpl1Unknown; phone ]

        // LP2's dial zero, CPL2's dial zero and retardance, and CPL1's fast-axis offset and retardance: stage 1's constants.
        Assert.Equal(LinearStudyEngine, engineOver instruments [ a1; a2; a3; k1 ] CalibrationStage)

        // Without the CPL→LP run, no run gives CPL-LP's constants.
        Assert.Equal(NonlinearStudyEngine, engineOver instruments [ a1; a2; k1 ] CalibrationStage)

        // A free gain scales every row.
        Assert.Equal(
            NonlinearStudyEngine,
            engineOver [ laser; lp1; lp2Unknown; cpl2Unknown; cpl1Unknown; freeGainPhone ] [ a1; a2; a3; k1 ] CalibrationStage)

    [<Fact>]
    let ``fully or partly unknown black-box matrices behind fixed instruments are linear, while a free gain or an unresolved series is nonlinear`` () : unit =
        let k1 = darkOver qzBehindLp

        // QZ behind the published CPL1 or LP1, analyzed by the published LP2, its rows made of QZ's truth.
        let e1 = capturedOver qzBehindCpl "E1" ScienceRun (capturesOf "CPL-(QZ#0)-LP" circularSource lpAnalyzer qzTruth)
        let e2 = capturedOver qzBehindLp "E2" ScienceRun (capturesOf "LP-(QZ#0)-LP" LpSource lpAnalyzer qzTruth)
        let instrumentsWith (objects : Item list) : Item list = [ laser; lp1; lp2; cpl1; phone ] @ objects

        // Every coefficient free, m33 known, m23 known with m32 calibrated, and m00 alone free, each fixed one at QZ's truth.
        for qz in [ qzAllFree; qzM33Known; qzM23KnownM32Calibrated; qzM00Free ] do
            for stage in objectStages do
                Assert.Equal(LinearStudyEngine, engineOver (instrumentsWith [ qz ]) [ e1; e2; k1 ] stage)

        // m00 alone free behind an ideal CP of either handedness, no reference family's polarizer, and a fixed LP2.
        let e3 = capturedOver qzBehindCpLeft "E3" ScienceRun (capturesOf "CP-(QZ#0)-LP" circularSource lpAnalyzer qzTruth)
        let e4 = capturedOver qzBehindCpRight "E4" ScienceRun (capturesOf "CP-(QZ#0)-LP" rightCircularSource lpAnalyzer qzTruth)

        for run in [ e3; e4 ] do
            for stage in objectStages do
                Assert.Equal(LinearStudyEngine, engineOver (instrumentsWith [ cpLeft; cpRight; qzM00Free ]) [ run; k1 ] stage)

        // Behind a constant-Mueller preset, or between a fixed plate and a known LR.
        let e5 = capturedOver qzBehindConstant "E5" ScienceRun (capturesOf "CMP-(QZ#0)-LP" circularSource lpAnalyzer qzTruth)
        let e6 = capturedOver qzSurrounded "E6" ScienceRun (capturesOf "LP-(WP-QZ-LR)-LP" LpSource lpAnalyzer (lrTruth * qzTruth * plateTruth))

        for stage in objectStages do
            Assert.Equal(LinearStudyEngine, engineOver (instrumentsWith [ constantPrep; qzAllFree ]) [ e5; k1 ] stage)
            Assert.Equal(LinearStudyEngine, engineOver (instrumentsWith [ plate; qzAllFree; lrKnown ]) [ e6; k1 ] stage)

        // Equality links: QZ's m21 follows its m12, QZ2's m00 follows QZ's free m00 across separate runs, and QZ2's m33
        // QZ's known m33; QZ2's truth shares QZ's m00 and m33, and QZ's truth has m21 = m12.
        Assert.Equal(qzTruth.element 1 2, qzTruth.element 2 1, 1e-12)
        Assert.Equal(qzTruth.element 0 0, qz2Truth.element 0 0, 1e-12)
        Assert.Equal(qzTruth.element 3 3, qz2Truth.element 3 3, 1e-12)

        let e7 = capturedOver qz2BehindCpl "E7" ScienceRun (capturesOf "CPL-(QZ2#0)-LP" circularSource lpAnalyzer qz2Truth)
        let e8 = capturedOver qz2BehindLp "E8" ScienceRun (capturesOf "LP-(QZ2#0)-LP" LpSource lpAnalyzer qz2Truth)
        let linkedRuns = [ e1; e2; e7; e8; k1 ]
        let linkedItems = instrumentsWith [ qzM33Known; qz2Free ]

        let linked =
            let study = studyOver "Linked" linkedRuns
            let used = (contextOver linkedItems linkedRuns study).kit.itemsUsedBy study

            let links : ParameterLink list =
                [
                    { follower = ItemParameterRef (qzM33Known.id, coefficient 2 1); leader = ItemParameterRef (qzM33Known.id, coefficient 1 2) }
                    { follower = ItemParameterRef (qz2Free.id, coefficient 0 0); leader = ItemParameterRef (qzM33Known.id, coefficient 0 0) }
                    { follower = ItemParameterRef (qz2Free.id, coefficient 3 3); leader = ItemParameterRef (qzM33Known.id, coefficient 3 3) }
                ]

            links |> List.fold (fun (held : Study) link -> studyOf (held.withLink link used)) study

        for stage in objectStages do
            Assert.Equal(LinearStudyEngine, engineOf (contextOver linkedItems linkedRuns linked) linked stage)

        // A free gain multiplies every free coefficient.
        for stage in objectStages do
            Assert.Equal(NonlinearStudyEngine, engineOver [ laser; lp1; lp2; cpl1; freeGainPhone; qzAllFree ] [ e1; e2; k1 ] stage)

        // QZ only ever behind a free LR: the rows hold products of two unknown matrices.
        let series = lrTruth * qzTruth
        let e9 = capturedOver seriesBehindCpl "E9" ScienceRun (capturesOf "CPL-(QZ#0-LR#0)-LP" circularSource lpAnalyzer series)
        let e10 = capturedOver seriesBehindLp "E10" ScienceRun (capturesOf "LP-(QZ#0-LR#0)-LP" LpSource lpAnalyzer series)

        for stage in objectStages do
            Assert.Equal(NonlinearStudyEngine, engineOver (instrumentsWith [ qzAllFree; lrFree ]) [ e9; e10; k1 ] stage)

        // A link coupling two matrices on one path: QZ2, behind QZ, has its m00 follow QZ's free m00.
        let e11 = capturedOver qzThenQz2 "E11" ScienceRun (capturesOf "LP-(QZ#0-QZ2#0)-LP" LpSource lpAnalyzer (qz2Truth * qzTruth))
        let coupledRuns = [ e11; k1 ]
        let coupledItems = instrumentsWith [ qzAllFree; qz2Known ]

        let coupled =
            let study = studyOver "Coupled" coupledRuns
            let link : ParameterLink = { follower = ItemParameterRef (qz2Known.id, coefficient 0 0); leader = ItemParameterRef (qzAllFree.id, coefficient 0 0) }
            studyOf (study.withLink link ((contextOver coupledItems coupledRuns study).kit.itemsUsedBy study))

        for stage in objectStages do
            Assert.Equal(NonlinearStudyEngine, engineOf (contextOver coupledItems coupledRuns coupled) coupled stage)

        // A double pass: the free QZ held in two slots of one path, its rows made of the truth's square.
        let e12 = capturedOver qzTwice "E12" ScienceRun (capturesOf "LP-(QZ#0-QZ#0)-LP" LpSource lpAnalyzer (qzTruth * qzTruth))

        for stage in objectStages do
            Assert.Equal(NonlinearStudyEngine, engineOver (instrumentsWith [ qzAllFree ]) [ e12; k1 ] stage)

    [<Fact>]
    let ``a study fitting an opened material's components is nonlinear`` () : unit =
        let configuration : MeasurementConfiguration =
            configurationOf XCut (Thickness.mm 1.0<mm>) 20.0<deg> 30.0<deg> TransmittedMueller (WaveLength.nm 600.0<nm>)

        let observation : KeyedObservation =
            { key = DataRowKey "M1"; observation = { configuration = configuration; measured = Propagation.identityMueller } }

        let m1 = attachedOver crystalBench "M1" ScienceRun ImportProfileName.muellerObservations (ObservationPoints [ observation ])

        // M1 alone, the crystal opened with each of its four components unknown from its own value (step 379's study).
        let study =
            let opened = studyOf ((studyOver "Plate" [ m1 ]).openMaterial crystalVersion crystalEntry)

            UniaxialParameters.axes
            |> List.fold
                (fun (held : Study) (axis : ParameterAxis<UniaxialParameters>) ->
                    let value = axis.read CrystalMaterialFixtures.uniaxialParameters
                    studyOf (held.withMaterialParameterState crystalVersion axis.name (unknownWithin value (value - 0.01) (value + 0.01))))
                opened

        Assert.Equal(NonlinearStudyEngine, engineOf (contextOver [ laser; crystalItem; polarimeter ] [ m1 ] study) study JointStage)

    // ============================ spec 0050 S11.3: the nonlinear engine ============================

    /// The truth's four observations of the 100 µm X-cut plate at azimuth 30° and 600 nm, through the spec 0044 forward:
    /// transmitted at incidence 0° and 30°, reflected at 30° and 50°.
    let private indexObservations : KeyedObservation list =
        let forward = createBerremanForward (fun (p : UniaxialParameters) -> p.opticalProperties) SolverParameters.defaultValue

        let plateAt (incidence : float<deg>) (observable : Observable) : MeasurementConfiguration =
            configurationOf XCut (Thickness.mkm 100.0<mkm>) incidence 30.0<deg> observable (WaveLength.nm 600.0<nm>)

        let configurations =
            [ plateAt 0.0<deg> TransmittedMueller; plateAt 30.0<deg> TransmittedMueller; plateAt 30.0<deg> ReflectedMueller; plateAt 50.0<deg> ReflectedMueller ]

        match observeWith forward indexTruth configurations with
        | Ok observations ->
            observations |> List.mapi (fun k observation -> ({ key = DataRowKey $"P{k + 1}"; observation = observation } : KeyedObservation))
        | Error error -> failwith $"the truth's observations were refused: %A{error}"

    /// P1, the plate run whose Mueller-observations file holds the four observations.
    let private plateRun : AttachedRun =
        attachedOver indexBench "P1" ScienceRun ImportProfileName.muellerObservations (ObservationPoints indexObservations)

    let private ordinaryIndex : ParameterName = ParameterName "n_o"
    let private extraordinaryIndex : ParameterName = ParameterName "n_e"

    // The starts, 1.0002 and 1.0001 times the truth (a retardance error well inside half a fringe), each searched within
    // ± 0.05: a scaled unit of 1e-3, the harness's.
    let private ordinaryStart : float = 1.0002 * indexTruth.ordinaryIndex.value
    let private extraordinaryStart : float = 1.0001 * indexTruth.extraordinaryIndex.value
    let private indexHalfWidth : float = 0.05

    /// P1's study: the index probe opened, n_o and n_e unknown from their starts, n_o with the prior when one is given.
    let private plateStudyWith (ordinaryPriorOpt : ParameterPrior option) : Study =
        let freed (start : float) (priorOpt : ParameterPrior option) : ParameterState =
            UnknownParameter
                {
                    start = ParameterValue start
                    bounds = { lower = ParameterValue (start - indexHalfWidth); upper = ParameterValue (start + indexHalfWidth) }
                    priorOpt = priorOpt
                }

        let opened = studyOf ((studyOver "Index plate" [ plateRun ]).openMaterial indexVersion indexEntry)

        opened.withMaterialParameterState indexVersion ordinaryIndex (freed ordinaryStart ordinaryPriorOpt)
        |> Result.bind (fun study -> study.withMaterialParameterState indexVersion extraordinaryIndex (freed extraordinaryStart None))
        |> studyOf

    /// The study fitting that many starts.
    let private startingFrom (starts : int) (study : Study) : Study =
        match FitStartCount.tryCreate starts with
        | Ok count -> study.withFitStarts count
        | Error error -> failwith error.describe

    /// The context over P1's kit, its material store also holding the index probe P1's plate pins.
    let private plateContextOf (study : Study) : StudyForwardContext =
        let context = contextOver [ laser; indexItem; polarimeter ] [ plateRun ] study

        match context.materials.saveMaterial indexEntry with
        | Ok () -> context
        | Error error -> failwith $"the index probe was refused: %A{error}"

    /// The study staged jointly with its calibrations held, the context over its staged items, and its data made from its
    /// own pre-fit, as `StudyForwardTests` makes them.
    let private stagedOver (context : StudyForwardContext) (study : Study) : StagedStudy * StudyForwardContext * StudyData =
        let staged =
            match study.stageWith context.kit JointStage CalibrationHeldFixed with
            | Ok staged -> staged
            | Error error -> failwith $"'{study.name.value}' was not staged: {error.describe}"

        let stagedContext = { context with kit = { context.kit with items = staged.items } }

        match staged.study.prefitWith stagedContext [] |> Result.bind (staged.study.dataWith stagedContext) with
        | Ok data -> staged, stagedContext, data
        | Error error -> failwith $"'{study.name.value}' has no data: {error.describe}"

    /// A provider answering these deviates in turn, recording each draw; a draw past them fails the fact.
    let private deviatesOf (values : float list) (drawn : ResizeArray<UniformDeviate>) : UniformDeviateProvider =
        {
            nextDeviate =
                fun () ->
                    match List.tryItem drawn.Count values with
                    | Some value ->
                        let deviate = UniformDeviate value
                        drawn.Add deviate
                        deviate
                    | None -> failwith $"only {values.Length} deviate(s) were expected"
        }

    let private solutionOf (result : Result<StudyFitSolution, StudyFitError>) : StudyFitSolution =
        match result with
        | Ok solution -> solution
        | Error error -> failwith $"expected a fit, got {error.describe}"

    let private problemOf (result : Result<ScaledStudyProblem, StudyFitError>) : ScaledStudyProblem =
        match result with
        | Ok problem -> problem
        | Error error -> failwith $"expected the scaled problem, got {error.describe}"

    let private assertValuesWithin (tolerance : float) (expected : float list) (actual : ParameterValue list) : unit =
        Assert.Equal(expected.Length, actual.Length)
        List.iter2 (fun (e : float) (a : ParameterValue) -> Assert.Equal(e, a.value, tolerance)) expected actual

    [<Fact>]
    let ``a polarimeter study recovers an opened uniaxial material's two indices from a perturbed start`` () : unit =
        let study = plateStudyWith None
        let context = plateContextOf study
        let staged, _, data = stagedOver context study
        let drawn = ResizeArray<UniformDeviate>()
        let solution = solutionOf (staged.fitNonlinear context data (deviatesOf [] drawn) UnwatchedIterations)

        // n_o's then n_e's entry, each the opened version's; one start, which draws no deviate.
        Assert.Equal(NonlinearStudyEngine, solution.engine)
        Assert.Empty(solution.objectMatrices)
        Assert.Empty(drawn)

        Assert.Equal<StudyParameterRef list>(
            [ MaterialParameterRef (indexVersion, ordinaryIndex); MaterialParameterRef (indexVersion, extraordinaryIndex) ],
            solution.layout.entries |> List.map (fun entry -> entry.unknown.parameter))

        match solution.members with
        | [ { start = start; outcome = MemberConverged (fitted, Some chiSquared) } ] ->
            assertValuesWithin 1e-12 [ ordinaryStart; extraordinaryStart ] start
            assertValuesWithin 0.0 (List.ofArray solution.fitted.value) fitted
            // The observations are exact at the truth, so the fit drives the residual from about 0.5 to nothing.
            Assert.True(chiSquared.value < 1e-8, $"the converged χ² is {chiSquared.value:R}")
        | members -> Assert.Fail($"expected one converged member, got %A{members}")

        Assert.Equal(indexTruth.ordinaryIndex.value, solution.fitted.value.[0], 1e-9)
        Assert.Equal(indexTruth.extraordinaryIndex.value, solution.fitted.value.[1], 1e-9)

    [<Fact>]
    let ``the first member starts at the parameters' starts and each further member where the deviates place it within the bounds`` () : unit =
        let study = plateStudyWith None |> startingFrom 3
        let context = plateContextOf study
        let staged, _, data = stagedOver context study
        let drawn = ResizeArray<UniformDeviate>()
        let reports = ResizeArray<IterationCount>()
        let watch = WatchedIterations { reportIteration = reports.Add; cancellation = CancellationToken.None }
        let solution = solutionOf (staged.fitNonlinear context data (deviatesOf [ -1.0; 1.0; 0.0; 0.5 ] drawn) watch)

        // Two deviates per further member, drawn entry after entry: n_o's then n_e's.
        Assert.Equal<float list>([ -1.0; 1.0; 0.0; 0.5 ], drawn |> Seq.map (fun deviate -> deviate.value) |> List.ofSeq)

        match solution.members |> List.map (fun fitMember -> fitMember.start) with
        | [ first; second; third ] ->
            assertValuesWithin 1e-12 [ ordinaryStart; extraordinaryStart ] first
            assertValuesWithin 1e-12 [ ordinaryStart - indexHalfWidth; extraordinaryStart + indexHalfWidth ] second
            assertValuesWithin 1e-12 [ ordinaryStart; extraordinaryStart + 0.025 ] third
        | starts -> Assert.Fail($"expected three members, got {starts.Length}")

        // The fit is the converged member of least χ², the earliest on a tie.
        let converged =
            solution.members
            |> List.choose (fun fitMember ->
                match fitMember.outcome with
                | MemberConverged (fitted, Some chiSquared) -> Some (fitted, chiSquared.value)
                | MemberConverged (_, None)
                | MemberFailed _ -> None)

        Assert.NotEmpty(converged)
        let least = converged |> List.map snd |> List.min
        let best = converged |> List.find (fun (_, chiSquared) -> chiSquared = least) |> fst
        assertValuesWithin 0.0 (List.ofArray solution.fitted.value) best

        // One watch counts every member's iterations on from the earlier ones': 1, 2, … over the whole fit.
        Assert.True(reports.Count >= 3, $"{reports.Count} iteration(s) were reported over three members")
        Assert.Equal<IterationCount list>([ for k in 1 .. reports.Count -> IterationCount k ], List.ofSeq reports)

    [<Fact>]
    let ``a prior adds one residual row, its scaled offset from its centre over its sigma`` () : unit =
        let prior : ParameterPrior = { centre = ParameterValue 1.545; sigma = ParameterSigma 0.001 }
        let origin = ScaledVector [| 0.0; 0.0 |]

        // The study residual at the parameters' starts, read directly.
        let dataEntriesOf (stagedContext : StudyForwardContext) (staged : StagedStudy) (data : StudyData) (problem : ScaledStudyProblem) : float list =
            match staged.study.residualWith stagedContext problem.layout data problem.layout.startVector with
            | Ok residual -> List.ofArray residual.value
            | Error error -> failwith $"the study residual was refused: {error.describe}"

        let withPrior = plateStudyWith (Some prior)
        let context = plateContextOf withPrior
        let staged, stagedContext, data = stagedOver context withPrior
        let problem = problemOf (staged.scaledProblemWith context data)

        // Each entry scaled about its start by a unit of its span over 100, its box ± 50 about the zero start.
        Assert.Equal<float[]>([| ordinaryStart; extraordinaryStart |], problem.centre.value)
        Array.iter (fun (unit : float) -> Assert.Equal(1e-3, unit, 1e-15)) problem.unit.value
        Assert.Equal<float[]>([| 0.0; 0.0 |], problem.search.start.value)
        Array.iter (fun (bound : float) -> Assert.Equal(-50.0, bound, 1e-9)) problem.search.lowerBounds.value
        Array.iter (fun (bound : float) -> Assert.Equal(50.0, bound, 1e-9)) problem.search.upperBounds.value

        // Sixteen entries per observation, then n_o's prior row.
        let dataEntries = dataEntriesOf stagedContext staged data problem
        let residual = List.ofArray (problem.residual origin).value
        Assert.Equal(16 * indexObservations.Length, dataEntries.Length)
        Assert.Equal(dataEntries.Length + 1, residual.Length)
        List.iter2 (fun (e : float) (a : float) -> Assert.Equal(e, a, 1e-12)) dataEntries (List.take dataEntries.Length residual)
        Assert.Equal((ordinaryStart - 1.545) / 0.001, List.last residual, 1e-12)

        // Without the prior, no extra row.
        let plain = plateStudyWith None
        let plainContext = plateContextOf plain
        let plainStaged, plainStagedContext, plainData = stagedOver plainContext plain
        let plainProblem = problemOf (plainStaged.scaledProblemWith plainContext plainData)
        let plainEntries = dataEntriesOf plainStagedContext plainStaged plainData plainProblem
        let plainResidual = List.ofArray (plainProblem.residual origin).value
        Assert.Equal(plainEntries.Length, plainResidual.Length)
        List.iter2 (fun (e : float) (a : float) -> Assert.Equal(e, a, 1e-12)) plainEntries plainResidual

    [<Fact>]
    let ``a session whose gain is anchored or interpolated is refused by the nonlinear engine`` () : unit =
        let identity = Propagation.identityMueller
        let a1 = capturedOver lpLpAir "A1" CalibrationRun (capturesOf "LP-(AIR#0)-LP" LpSource lpAnalyzer identity)
        let e2 = capturedOver qzBehindLp "E2" ScienceRun (capturesOf "LP-(QZ#0)-LP" LpSource lpAnalyzer qzTruth)
        let a4 = capturedOver lpLpAir "A4" CalibrationRun (capturesOf "LP-(AIR#1)-LP" LpSource lpAnalyzer identity)
        let k1 = darkOver lpLpAir
        let runs = [ a1; e2; a4; k1 ]
        let kitRuns = runs |> List.map (fun attached -> attached.attachedRun)
        let items = [ laser; lp1; lp2; phone; qzM00Free ]
        let session = SessionName "Session 1"

        // One session holds every run; with its gain free, the problem is made.
        let free = studyOver "Gain" runs
        Assert.Equal<SessionName list>([ session ], free.sessionsIn kitRuns |> List.map (fun held -> held.name))
        let freeContext = contextOver items runs free
        let freeStaged, _, freeData = stagedOver freeContext free
        Assert.NotEmpty((problemOf (freeStaged.scaledProblemWith freeContext freeData)).layout.entries)

        let models : SessionGainModel list =
            [ AnchoredSessionGain [ a1.attachedRun.name ]; InterpolatedSessionGain { opening = a1.attachedRun.name; closing = a4.attachedRun.name } ]

        for model in models do
            let study = studyOf (free.withSessionGainModel session model kitRuns)
            let context = contextOver items runs study
            let staged, _, data = stagedOver context study
            let drawn = ResizeArray<UniformDeviate>()

            match staged.scaledProblemWith context data with
            | Error (SessionGainNeedsLinearEngineErr reason) -> Assert.Contains("'Session 1'", reason)
            | Error error -> Assert.Fail($"expected SessionGainNeedsLinearEngineErr, got {error.describe}")
            | Ok _ -> Assert.Fail("a session whose gain comes from AIR runs was fitted nonlinearly")

            match staged.fitNonlinear context data (deviatesOf [] drawn) UnwatchedIterations with
            | Error (SessionGainNeedsLinearEngineErr reason) -> Assert.Contains("'Session 1'", reason)
            | Error error -> Assert.Fail($"expected SessionGainNeedsLinearEngineErr, got {error.describe}")
            | Ok _ -> Assert.Fail("a session whose gain comes from AIR runs was fitted nonlinearly")

    [<Fact>]
    let ``a cancelled fit stops before its next member and answers FitCancelledErr`` () : unit =
        let study = plateStudyWith None |> startingFrom 3
        let context = plateContextOf study
        let staged, _, data = stagedOver context study

        // Cancelled at the first member's first report: its result is dropped and no further member starts.
        use source = new CancellationTokenSource()
        let reports = ResizeArray<IterationCount>()

        let cancelling : OptimizerIterationObserver =
            {
                reportIteration =
                    fun count ->
                        reports.Add count
                        source.Cancel()
                cancellation = source.Token
            }

        let drawn = ResizeArray<UniformDeviate>()

        match staged.fitNonlinear context data (deviatesOf [ 0.0; 0.0; 0.0; 0.0 ] drawn) (WatchedIterations cancelling) with
        | Error (FitCancelledErr reason) -> Assert.Contains("1 of 3", reason)
        | Error error -> Assert.Fail($"expected FitCancelledErr, got {error.describe}")
        | Ok solution -> Assert.Fail($"expected FitCancelledErr, got {solution.members.Length} member(s)")

        Assert.Empty(drawn)
        Assert.InRange(reports.Count, 1, 2)

        // Cancelled before the fit: no member runs.
        use cancelled = new CancellationTokenSource()
        cancelled.Cancel()
        let silent = ResizeArray<IterationCount>()
        let beforeAny : OptimizerIterationObserver = { reportIteration = silent.Add; cancellation = cancelled.Token }

        match staged.fitNonlinear context data (deviatesOf [] drawn) (WatchedIterations beforeAny) with
        | Error (FitCancelledErr reason) -> Assert.Contains("0 of 3", reason)
        | Error error -> Assert.Fail($"expected FitCancelledErr, got {error.describe}")
        | Ok solution -> Assert.Fail($"expected FitCancelledErr, got {solution.members.Length} member(s)")

        Assert.Empty(silent)
        Assert.Empty(drawn)

    // ============================ spec 0050 S11.3: the linear engine's stage 1 ============================

    /// The stage-1 truth the AIR rows are made of: dark 5, LP2's zero 3°, CPL2's zero −2° and retardance 88°, and CPL1's
    /// source retarder at 46° and 91°.
    let private stageOneTruth : StageOneCalibrationData =
        {
            darkMean = DarkMean 5.0
            lpZero = Angle.degree 3.0<deg>
            cplZero = Angle.degree (-2.0<deg>)
            cplAnalyzerRetardance = Retardance.degree 88.0<deg>
            cplSourceModel = ({ thetaRel = Angle.degree 46.0<deg>; retardance = Retardance.degree 91.0<deg> } : SourceCplModel)
        }

    /// An AIR run's rows: 1000 · `airIdentityPred` of the source and the analyzer's row at each reading, plus the dark 5.
    let private airCapturesOf (label : string) (source : SourceModel) (analyzer : AnalyzerModel) : MuellerRawRow list =
        phoneRowsOf label (fun reading -> (airIdentityPred source.baseStokes (analyzerBaseRow analyzer reading)).value)

    let private stageOneLpLpRows : MuellerRawRow list = airCapturesOf "LP-(AIR#0)-LP" LpSource stageOneTruth.lpAnalyzer
    let private stageOneLpCplRows : MuellerRawRow list = airCapturesOf "LP-(AIR#0)-CPL" LpSource stageOneTruth.cplAnalyzer
    let private stageOneCplLpRows : MuellerRawRow list = airCapturesOf "CPL-(AIR#0)-LP" stageOneTruth.cplSource stageOneTruth.lpAnalyzer

    /// The scan turning the analyzer's dial 0° to 160° by 20°, the readings the rows are taken at.
    let private dialSweep : ExperimentScan =
        let range : VariableRange =
            {
                min = Propagation.SweptValue 0.0
                max = Propagation.SweptValue 160.0
                points = Propagation.CurvePointCount 9
            }

        ExperimentScan.oneAxis { analyzerTurn with range = range } CaptureTransmitted None

    /// A Calibration run over the setup turning the analyzer's dial 0° to 160° by 20°, its Phone ROI CSV the rows.
    let private stageOneRunOver (setup : Setup) (name : string) (rows : MuellerRawRow list) : AttachedRun =
        let captured = capturedOver setup name CalibrationRun rows
        { captured with attachedRun = { captured.attachedRun with scan = dialSweep } }

    let private stageOneLpLp : AttachedRun = stageOneRunOver lp1AirRealLp2 "S1" stageOneLpLpRows
    let private stageOneLpCpl : AttachedRun = stageOneRunOver lpCplAir "S2" stageOneLpCplRows
    let private stageOneCplLp : AttachedRun = stageOneRunOver cpl1AirRealLp2 "S3" stageOneCplLpRows
    let private stageOneDark : AttachedRun = darkOver lp1AirRealLp2
    let private stageOneInstruments : Item list = [ laser; lp1; realLp2Unknown; cpl2Unknown; cpl1Unknown; phone ]

    /// The AIR runs' rows as the reference stage one reads them: the Dark run's, then each family's.
    let private stageOneAirRows : StageOneAirRows =
        let darknessRows =
            match stageOneDark.attachedPoints with
            | CapturePoints rows -> rows
            | SeriesPoints _
            | ObservationPoints _
            | ColumnPoints _ -> failwith "the Dark run holds no capture rows"

        {
            darknessRows = darknessRows
            lpLpAirRows = stageOneLpLpRows
            lpCplAirRows = stageOneLpCplRows
            cplLpAirRows = stageOneCplLpRows
        }

    /// The five instrument parameters stage one fits, each at its constant in stage one's answer; CPL1's retarder
    /// offset is its θ less 45°.
    let private stageOneValuesOf (stageOne : StageOneCalibrationData) : (Item * ParameterName * float) list =
        [
            realLp2Unknown, ParameterName.dialZero, stageOne.lpZero.value
            cpl2Unknown, ParameterName.dialZero, stageOne.cplZero.value
            cpl2Unknown, retardance, stageOne.cplAnalyzerRetardance.value
            cpl1Unknown, fastAxisOffset, (stageOne.cplSourceModel.thetaRel - Angle.degree 45.0<deg>).value
            cpl1Unknown, retardance, stageOne.cplSourceModel.retardance.value
        ]

    /// The calibration stage of a study of the runs over the stage-one instruments, its pre-fit, and the context it was
    /// staged in.
    let private stageOneOver (runs : AttachedRun list) : StagedStudy * ExperimentPrefit list * StudyForwardContext =
        let study = studyOver "Stage one" runs
        let context = contextOver stageOneInstruments runs study
        let staged, prefits = prefitStaged context study CalibrationStage CalibrationHeldFixed
        staged, prefits, context

    [<Fact>]
    let ``the calibration stage recovers the instruments' zeros, retardances and offset through the reference stage one`` () : unit =
        let staged, prefits, context = stageOneOver [ stageOneLpLp; stageOneLpCpl; stageOneCplLp; stageOneDark ]
        let solver = createMathNetSvd ()
        Assert.Equal(LinearStudyEngine, staged.engineWith context prefits)

        let expected =
            match calibrateFromAirRows solver stageOneAirRows with
            | Ok stageOne -> stageOne
            | Error error -> failwith $"stage one refused the rows: {error.describe}"

        let solution = solutionOf (staged.calibrateLinear context prefits solver)
        Assert.Equal(LinearStudyEngine, solution.engine)
        Assert.Empty(solution.objectMatrices)

        let fittedOf (item : Item) (name : ParameterName) : float =
            match solution.layout.entries |> List.tryFindIndex (fun entry -> entry.unknown.parameter = ItemParameterRef (item.id, name)) with
            | Some k -> solution.fitted.value.[k]
            | None -> failwith $"no entry fits {item.label.value}'s {name.value}"

        // The truth's zeros, 3° and −2°, as stage one reports an axis, within [0°, 180°): −2° is the same axis as 178°.
        Assert.Equal(stageOneTruth.lpZero.value, fittedOf realLp2Unknown ParameterName.dialZero, 1e-9)
        Assert.Equal((stageOneTruth.cplZero + Angle.pi).value, fittedOf cpl2Unknown ParameterName.dialZero, 1e-9)

        // Both retardances are the truth's. CPL1's offset is checked against stage one alone: through LP2's negative dial
        // the reference reads the source's 46° as 44°.
        Assert.Equal(stageOneTruth.cplAnalyzerRetardance.value, fittedOf cpl2Unknown retardance, 1e-9)
        Assert.Equal(stageOneTruth.cplSourceModel.retardance.value, fittedOf cpl1Unknown retardance, 1e-9)

        // Each of the five values is stage one's own constant, exactly.
        let constants = stageOneValuesOf expected

        Assert.Equal(constants.Length, solution.layout.entries.Length)

        for item, name, value in constants do
            Assert.Equal<float>(value, fittedOf item name)

        // One member, from the layout's starts to the fitted values, with no common χ².
        match solution.members with
        | [ { start = start; outcome = MemberConverged (fitted, None) } ] ->
            Assert.Equal<ParameterValue list>(solution.layout.entries |> List.map (fun entry -> entry.unknown.unknown.start), start)
            Assert.Equal<float list>(List.ofArray solution.fitted.value, fitted |> List.map (fun value -> value.value))
        | members -> Assert.Fail($"expected one converged member without a χ², got %A{members}")

    [<Fact>]
    let ``a calibration stage without a dark run is refused with the staged path's error`` () : unit =
        let staged, prefits, context = stageOneOver [ stageOneLpLp; stageOneLpCpl; stageOneCplLp ]
        Assert.Equal(LinearStudyEngine, staged.engineWith context prefits)

        match staged.calibrateLinear context prefits (createMathNetSvd ()) with
        | Error (StagedPathFitErr (EmptyCaptureSetErr reason)) -> Assert.Contains("darkness", reason)
        | Error error -> Assert.Fail($"expected the staged path's EmptyCaptureSetErr, got {error.describe}")
        | Ok _ -> Assert.Fail("a calibration stage without a dark run was fitted")

    // ============================ spec 0050 S11.3: the linear engine's object stage ============================

    // Step 389's instruments, each stage-1 constant published as a calibration at the truth's: LP2's zero 3°, CPL2's zero
    // −2° and retardance 88°, and CPL1's retarder at 46° (its offset 1° past the 45° it sits at) and 91°.
    let private calibratedLp2 : Item = itemOf realLp2Draft |> publishing [ ParameterName.dialZero, stageOneTruth.lpZero.value ]

    let private calibratedCpl2 : Item =
        itemOf cpl2Draft
        |> publishing [ ParameterName.dialZero, stageOneTruth.cplZero.value; retardance, stageOneTruth.cplAnalyzerRetardance.value ]

    let private calibratedCpl1 : Item =
        itemOf cpl1Draft
        |> publishing
            [
                fastAxisOffset, (stageOneTruth.cplSourceModel.thetaRel - Angle.degree 45.0<deg>).value
                retardance, stageOneTruth.cplSourceModel.retardance.value
            ]

    /// The instruments every object-stage kit holds, the quarter-wave plate among them.
    let private objectInstruments : Item list = [ laser; lp1; calibratedLp2; calibratedCpl1; calibratedCpl2; phone; plate ]

    let private phis : float<deg> list = [ 0.0<deg>; 30.0<deg>; 60.0<deg>; 90.0<deg> ]

    /// The bench of these objects in light order, each at its R1, read by the Phone.
    let private turnedBench (name : string) (prep : Item) (objects : (Item * Angle) list) (analyzer : Item) : Setup =
        let objectSlots =
            objects
            |> List.mapi (fun k (item, turn) ->
                let slot = objectSlotAt k
                holding item { slot with placement = { slot.placement with r1 = turn } })

        benchWith name prep objectSlots analyzer phone

    /// The four reference families about the segment, each of its objects at φ: LP1 or CPL1 preparing, and LP2, or the
    /// quarter-wave plate then CPL2, analyzing. The shipped CPL is a polarizer then a retarder in light order, so as an
    /// analyzer it reads no S₃; the plate ahead of it does, as the reference's circular analyzer (a retarder, then an LP).
    let private familyBenches (label : string) (segment : Item list) (phi : float<deg>) : Setup list =
        let turned = segment |> List.map (fun item -> item, Angle.degree phi)
        let quarterWave = [ plate, Angle.zero ]

        [
            turnedBench $"{label} LP-LP {phi}" lp1 turned calibratedLp2
            turnedBench $"{label} LP-CPL {phi}" lp1 (turned @ quarterWave) calibratedCpl2
            turnedBench $"{label} CPL-LP {phi}" calibratedCpl1 turned calibratedLp2
            turnedBench $"{label} CPL-CPL {phi}" calibratedCpl1 (turned @ quarterWave) calibratedCpl2
        ]

    /// LP1 to LP2 through no object: the AIR bench the sessions' gains are anchored to.
    let private airBench : Setup = benchOf "AIR LP-LP" lp1 [ None ] calibratedLp2 phone

    /// A run of that role over the setup, in that hand-set session or none, turning the analyzer's dial 0° to 160° by 20°.
    let private sweptRunOver (setup : Setup) (name : string) (role : ExperimentRole) (sessionOpt : SessionName option) : Experiment =
        { (capturedOver setup name role []).attachedRun with scan = dialSweep; sessionOpt = sessionOpt }

    let private dialSettingAt (reading : Angle) : ScanSetting =
        { elementId = Library.ElementId "analyzer"; variable = VaryDialReading; value = Propagation.SweptValue reading.degrees }

    /// A black box's values: the unit matrix at vec place k (4·column + row), or the identity.
    let private probeValues (placeOpt : int option) : ItemEvaluation.ParameterValues =
        coefficientPlaces
        |> List.map (fun (row, column) ->
            let value =
                match placeOpt with
                | Some place -> if 4 * column + row = place then 1.0 else 0.0
                | None -> if row = column then 1.0 else 0.0

            coefficient row column, ParameterValue value)
        |> Map.ofList
        |> ItemEvaluation.ParameterValues

    /// The ordinary run law's S₀ per unit gain at each dial reading: the Phone's reading over 1000 at no dark, every item
    /// at its own values but the probed ones.
    let private runLawSignalsOf (context : StudyForwardContext) (run : Experiment) (probed : (ItemId * ItemEvaluation.ParameterValues) list) : float list =
        let study = studyOf (Study.create (StudyName "Run law") [ run.name ] [] [ run ])
        let assignment : ExperimentAssignment = { itemValues = Map.ofList probed; crystals = Map.empty }

        match run.boundWith context study assignment (Some (DarkMean 0.0)) with
        | Error error -> failwith $"{run.name.value} was not bound: {error.describe}"
        | Ok bound ->
            dialReadings
            |> List.map (fun reading ->
                match bound.readingAt [ dialSettingAt reading ] None with
                | Ok (RecordedSignal signal) -> signal.value / 1000.0
                | Ok other -> failwith $"{run.name.value} read {other.kindText}"
                | Error error -> failwith $"{run.name.value} was not read: {error.describe}")

    /// The run law's design row at each dial reading: entry 4·column + row is S₀ per unit gain with the segment's last item
    /// at that place's unit matrix and each other at the identity, so a series reads its combined matrix.
    let private runLawDesignsOf (context : StudyForwardContext) (run : Experiment) (segment : ItemId list) : float[] list =
        let byPlace : float list list =
            [
                for place in 0 .. 15 ->
                    segment
                    |> List.mapi (fun k item -> item, probeValues (if k = segment.Length - 1 then Some place else None))
                    |> runLawSignalsOf context run
            ]

        dialReadings |> List.mapi (fun r _ -> byPlace |> List.map (List.item r) |> Array.ofList)

    /// A run to observe: the experiment, its unknown segment's items in light order (none for an AIR run) and the matrix
    /// they make, and each reading's capture time and the Phone's gain at a time.
    type private PlannedRun =
        {
            plannedRun : Experiment
            segment : ItemId list
            segmentTruth : MuellerMatrix
            timeOf : int -> DateTimeOffset
            gainAt : DateTimeOffset -> float
        }

    /// An observed run: the run with its rows, and per row the run law's design and the corrected signal it is made of.
    type private ObservedRun =
        {
            observed : AttachedRun
            designs : float[] list
            targets : float list
        }

    /// Each planned run's Phone ROI CSV over a kit of the items within these setups: g · (design · M.vecColumnMajor) + 5
    /// at each reading's time, an AIR run's g · S₀ + 5, the dark 5.
    let private observedOver (setups : Setup list) (items : Item list) (planned : PlannedRun list) : ObservedRun list =
        let blank = planned |> List.map (fun plan -> ({ attachedRun = plan.plannedRun; attachedPoints = CapturePoints [] } : AttachedRun))
        let context = contextWithin setups items blank (studyOver "Run law" blank)

        planned
        |> List.map (fun plan ->
            let designs, targets =
                match plan.segment with
                | [] -> [], runLawSignalsOf context plan.plannedRun []
                | segment ->
                    let designs = runLawDesignsOf context plan.plannedRun segment
                    let m = plan.segmentTruth.vecColumnMajor
                    designs, designs |> List.map (fun design -> design |> Array.mapi (fun place entry -> entry * m.[place]) |> Array.sum)

            let rows =
                targets
                |> List.mapi (fun k target ->
                    let at = plan.timeOf k

                    ({
                        experiment = MuellerExperimentLabel plan.plannedRun.name.value
                        captureIndex = CaptureIndex (k + 1)
                        capturedAt = at
                        description = List.item k dialReadings
                        avgTotal = MeasuredIntensity (plan.gainAt at * target + 5.0)
                    } : MuellerRawRow))

            { observed = { attachedRun = plan.plannedRun; attachedPoints = CapturePoints rows }; designs = designs; targets = targets })

    let private afternoon : DateTimeOffset = DateTimeOffset(2026, 11, 13, 14, 0, 0, TimeSpan.Zero)

    /// A run of a free session: its readings a second apart from 14:00, the Phone at its gain 1000.
    let private freelyPlanned (run : Experiment) (segment : Item list) (truth : MuellerMatrix) : PlannedRun =
        {
            plannedRun = run
            segment = segment |> List.map (fun item -> item.id)
            segmentTruth = truth
            timeOf = fun k -> afternoon.AddSeconds(float k)
            gainAt = fun _ -> 1000.0
        }

    /// The free-session Science runs of the segment's four families at each φ, named `<prefix><k>`.
    let private freeFamilyRuns (prefix : string) (segment : Item list) (truth : MuellerMatrix) : Setup list * PlannedRun list =
        let benches = phis |> List.collect (familyBenches prefix segment)
        let runs = benches |> List.mapi (fun k bench -> freelyPlanned (sweptRunOver bench $"{prefix}{k + 1}" ScienceRun None) segment truth)
        benches, runs

    /// The object stage over a kit of the items within these setups: the staged study, the unstaged context, its pre-fit,
    /// each run of it ready, and its data.
    let private objectStageOf
        (setups : Setup list)
        (items : Item list)
        (runs : AttachedRun list)
        (study : Study)
        : StagedStudy * StudyForwardContext * ExperimentPrefit list * StudyData =
        let context = contextWithin setups items runs study
        let staged, prefits = prefitStaged context study ObjectStage CalibrationHeldFixed
        let stagedContext = { context with kit = { context.kit with items = staged.items } }

        for prefit in prefits do
            match prefit.check with
            | Ok _ -> ()
            | Error error -> Assert.Fail($"{prefit.experiment.name.value} is not ready: {error.describe}")

        match staged.study.dataWith stagedContext prefits with
        | Ok data -> staged, context, prefits, data
        | Error error -> failwith $"'{study.name.value}' has no data: {error.describe}"

    /// The real MathNet SVD, recording each system it is handed, in order.
    let private recordingSvd (recorded : ResizeArray<LeastSquaresSystem>) : MuellerSolverProxy =
        let real = createMathNetSvd ()

        {
            solveLinearLeastSquares =
                fun system ->
                    recorded.Add system
                    real.solveLinearLeastSquares system
        }

    /// Where a coefficient of a solved object comes from: the system column it is fitted in, or its fixed value.
    type private CoefficientSource =
        | FittedInColumn of column : int
        | FixedAtValue of value : float

    /// A black box's sources: its free places, in row-major order, fitted in the columns from `first` on, every other
    /// place fixed at the truth's element.
    let private sourcesOf (truth : MuellerMatrix) (free : (int * int) list) (first : int) : int * int -> CoefficientSource =
        fun (row, column) ->
            match free |> List.tryFindIndex (fun place -> place = (row, column)) with
            | Some k -> FittedInColumn (first + k)
            | None -> FixedAtValue (truth.element row column)

    /// A full reconstruction's sources: every place fitted in its own vec column, 4·column + row.
    let private vecSources : int * int -> CoefficientSource = fun (row, column) -> FittedInColumn (4 * column + row)

    /// The system a solve must be handed over the observed runs, in order: each column of a row the sum of the run law's
    /// design entries of the places fitted in it, its target less the fixed places' part.
    let private assertSystemOver (columnCount : int) (runs : (ObservedRun * (int * int -> CoefficientSource)) list) (system : LeastSquaresSystem) : unit =
        let expected =
            runs
            |> List.collect (fun (run, sourceOf) ->
                List.map2
                    (fun (design : float[]) (target : float) ->
                        let sources = coefficientPlaces |> List.map (fun (row, column) -> design.[4 * column + row], sourceOf (row, column))

                        let columns =
                            List.init columnCount (fun j ->
                                sources
                                |> List.sumBy (fun (entry, source) ->
                                    match source with
                                    | FittedInColumn k when k = j -> entry
                                    | FittedInColumn _
                                    | FixedAtValue _ -> 0.0))

                        let fixedPart =
                            sources
                            |> List.sumBy (fun (entry, source) ->
                                match source with
                                | FixedAtValue value -> entry * value
                                | FittedInColumn _ -> 0.0)

                        columns, target - fixedPart)
                    run.designs
                    run.targets)

        Assert.Equal(expected.Length, system.design.Length)

        expected
        |> List.iteri (fun r (columns, _) ->
            Assert.Equal(columnCount, system.design.[r].value.Length)
            assertValuesWithin 1e-12 columns (system.design.[r].value |> List.ofArray |> List.map ParameterValue))

        assertValuesWithin 1e-12 (expected |> List.map snd) (system.rhs |> List.ofArray |> List.map ParameterValue)

    let private placeOf (row : int) (column : int) : int = 4 * row + column

    /// The object solve of these items, failing the fact when there is none.
    let private objectFitOf (items : Item list) (solution : StudyFitSolution) : ObjectMatrixFit =
        match solution.objectMatrices |> List.tryFind (fun fit -> fit.items = (items |> List.map (fun item -> item.id))) with
        | Some fit -> fit
        | None ->
            let labels = items |> List.map (fun item -> item.label.value) |> String.concat ", "
            failwith $"no object solve of {labels}"

    /// The fitted values of the item's coefficients at these places, failing the fact when the layout fits one not.
    let private fittedCoefficientsOf (solution : StudyFitSolution) (item : Item) (places : (int * int) list) : ParameterValue list =
        places
        |> List.map (fun (row, column) ->
            match solution.layout.entries |> List.tryFindIndex (fun entry -> entry.unknown.parameter = ItemParameterRef (item.id, coefficient row column)) with
            | Some k -> solution.fitted.valueAt (StudyVectorIndex k)
            | None -> failwith $"no entry fits {item.label.value}'s m{row}{column}")

    /// The truth's elements at these places.
    let private elementsAt (truth : MuellerMatrix) (places : (int * int) list) : float list =
        places |> List.map (fun (row, column) -> truth.element row column)

    /// A lone black box's solve: one object solve of it of rank `free.Length`, every element within 1e-9 of the truth and
    /// each fixed one exactly the truth's; its free places in row-major order its only entries, each within 1e-9.
    let private assertLoneSolve (item : Item) (truth : MuellerMatrix) (free : (int * int) list) (solution : StudyFitSolution) : unit =
        Assert.Equal(LinearStudyEngine, solution.engine)

        Assert.Equal<StudyParameterRef list>(
            free |> List.map (fun (row, column) -> ItemParameterRef (item.id, coefficient row column)),
            solution.layout.entries |> List.map (fun entry -> entry.unknown.parameter))

        assertValuesWithin 1e-9 (elementsAt truth free) (fittedCoefficientsOf solution item free)

        Assert.Equal(1, solution.objectMatrices.Length)
        let fit = objectFitOf [ item ] solution
        Assert.Equal(MatrixRank free.Length, fit.rank)
        assertValuesWithin 1e-9 (truth.rowMajorValues |> List.map (fun value -> value.value)) fit.elements

        for row, column in coefficientPlaces |> List.filter (fun place -> not (List.contains place free)) do
            Assert.Equal<float>(truth.element row column, (List.item (placeOf row column) fit.elements).value)

        // One member, from the layout's starts to the fitted values, with the χ² of the solves.
        match solution.members with
        | [ { start = start; outcome = MemberConverged (fitted, Some chiSquared) } ] ->
            Assert.Equal<ParameterValue list>(solution.layout.entries |> List.map (fun entry -> entry.unknown.unknown.start), start)
            Assert.Equal<float list>(List.ofArray solution.fitted.value, fitted |> List.map (fun value -> value.value))
            Assert.InRange(chiSquared.value, 0.0, 1e-18)
        | members -> Assert.Fail($"expected one converged member with a χ², got %A{members}")

    /// QZ with every coefficient unknown except these places, each held at M_QZ's element in its state.
    let private qzHolding (held : (int * int * ParameterState) list) : Item =
        blackBoxWith qzDraft (held |> List.map (fun (row, column, state) -> coefficient row column, state))

    /// QZ's places other than these, in row-major order.
    let private placesBut (held : (int * int) list) : (int * int) list =
        coefficientPlaces |> List.filter (fun place -> not (List.contains place held))

    let private day1 : SessionName = SessionName "Day 1"
    let private day2 : SessionName = SessionName "Day 2"
    let private tenOClock : DateTimeOffset = DateTimeOffset(2026, 11, 13, 10, 0, 0, TimeSpan.Zero)

    /// Day 2's gain, from 1000 at 10:00 to 1200 at 12:00.
    let private day2GainAt (at : DateTimeOffset) : float = 1000.0 + 200.0 * (at - tenOClock).TotalSeconds / 7200.0

    /// QZ's object stage over two sessions: Day 1 anchored to the LP-LP AIR run A1 of gain 1000; Day 2 interpolated between
    /// the AIR runs A2 of gain 1000 at 10:00 and A3 of 1200 at 12:00, its Science runs read at the gain then; and the Dark
    /// run K1 of 5. The setups, the runs and the study, and QZ's sixteen observed Science runs.
    let private sessionFixture () : Setup list * AttachedRun list * Study * ObservedRun list =
        let benchesAt = phis |> List.map (fun phi -> phi, familyBenches "QZ" [ qzAllFree ] phi)
        let setups = airBench :: (benchesAt |> List.collect snd)

        let science : PlannedRun list =
            benchesAt
            |> List.collect (fun (phi, benches) -> benches |> List.map (fun bench -> phi, bench))
            |> List.mapi (fun k (phi, bench) ->
                let onDay1 = phi < 60.0<deg>
                let run = sweptRunOver bench $"E{k + 1}" ScienceRun (Some (if onDay1 then day1 else day2))

                {
                    plannedRun = run
                    segment = [ qzAllFree.id ]
                    segmentTruth = qzTruth
                    timeOf =
                        if onDay1 then fun reading -> DateTimeOffset(2026, 11, 13, 8, 10, 0, TimeSpan.Zero).AddMinutes(float (5 * k)).AddSeconds(float (10 * reading))
                        else fun reading -> tenOClock.AddMinutes(float (10 + 12 * (k - 8))).AddSeconds(float (30 * reading))
                    gainAt = if onDay1 then (fun _ -> 1000.0) else day2GainAt
                })

        let air (name : string) (session : SessionName) (at : DateTimeOffset) (gain : float) : PlannedRun =
            {
                plannedRun = sweptRunOver airBench name CalibrationRun (Some session)
                segment = []
                segmentTruth = Propagation.identityMueller
                timeOf = fun reading -> at.AddSeconds(float (reading - 4))
                gainAt = fun _ -> gain
            }

        let a1 = air "A1" day1 (DateTimeOffset(2026, 11, 13, 8, 0, 0, TimeSpan.Zero)) 1000.0
        let a2 = air "A2" day2 tenOClock 1000.0
        let a3 = air "A3" day2 (tenOClock.AddHours 2.0) 1200.0
        let observed = observedOver setups (objectInstruments @ [ qzAllFree ]) ([ a1; a2; a3 ] @ science)
        let observedOf (plan : PlannedRun) : AttachedRun = (observed |> List.find (fun run -> run.observed.attachedRun.name = plan.plannedRun.name)).observed
        let scienceObserved = observed |> List.filter (fun run -> science |> List.exists (fun plan -> plan.plannedRun.name = run.observed.attachedRun.name))
        let day1Runs = scienceObserved |> List.take 8 |> List.map (fun run -> run.observed)
        let day2Runs = scienceObserved |> List.skip 8 |> List.map (fun run -> run.observed)
        let runs = [ observedOf a1 ] @ day1Runs @ [ darkOver airBench; observedOf a2 ] @ day2Runs @ [ observedOf a3 ]
        let kitRuns = runs |> List.map (fun run -> run.attachedRun)

        let study =
            (studyOver "QZ" runs).withSessionGainModel day1 (AnchoredSessionGain [ a1.plannedRun.name ]) kitRuns
            |> Result.bind (fun study ->
                study.withSessionGainModel day2 (InterpolatedSessionGain { opening = a2.plannedRun.name; closing = a3.plannedRun.name }) kitRuns)
            |> studyOf

        setups, runs, study, scienceObserved

    /// The observed runs of a free-session study of the runs and the Dark run K1, its setups and its study.
    let private freeStudyOf (name : string) (setups : Setup list) (observed : ObservedRun list) : Setup list * AttachedRun list * Study =
        let runs = (observed |> List.map (fun run -> run.observed)) @ [ darkOver airBench ]
        airBench :: setups, runs, studyOver name runs

    /// The variant's solve over the kit of the instruments and these objects, through the recording SVD, the engine
    /// first asserted linear.
    let private linearSolveOf
        (setups : Setup list)
        (objects : Item list)
        (runs : AttachedRun list)
        (study : Study)
        (recorded : ResizeArray<LeastSquaresSystem>)
        : StudyFitSolution =
        let staged, context, prefits, data = objectStageOf setups (objectInstruments @ objects) runs study
        Assert.Equal(LinearStudyEngine, staged.engineWith context prefits)
        solutionOf (staged.reconstructLinear context prefits data (recordingSvd recorded))

    [<Fact>]
    let ``the object stage reconstructs a black-box matrix behind calibrated instruments, each row divided by its session's gain at its time`` () : unit =
        let setups, runs, study, scienceRuns = sessionFixture ()

        // Every coefficient free: one `reconstruct` of the sixteen, its rows the run law's designs and corrected signals.
        let allFree = ResizeArray<LeastSquaresSystem>()
        let fullSolution = linearSolveOf setups [ qzAllFree ] runs study allFree
        assertLoneSolve qzAllFree qzTruth coefficientPlaces fullSolution
        Assert.Equal(1, allFree.Count)
        assertSystemOver 16 (scienceRuns |> List.map (fun run -> run, vecSources)) allFree.[0]

        // Over the same observations, m33 known (15 free), m23 known with m32 calibrated (14 free: the two are of opposite
        // sign, so a transposed column shows), and m00 alone unknown (1 free): each solved on its own columns, A_U·m_U =
        // b − A_K·m_K, never sixteen columns with the fixed ones overwritten after.
        Assert.NotEqual(qzTruth.element 2 3, qzTruth.element 3 2)

        let m00Free = placesBut [ 0, 0 ] |> List.map (fun (row, column) -> row, column, knownIn qzTruth row column)

        let partials : (Item * (int * int) list) list =
            [
                qzHolding [ 3, 3, knownIn qzTruth 3 3 ], placesBut [ 3, 3 ]
                qzHolding [ 2, 3, knownIn qzTruth 2 3; 3, 2, calibratedAt (qzTruth.element 3 2) ], placesBut [ 2, 3; 3, 2 ]
                qzHolding m00Free, [ 0, 0 ]
            ]

        for qz, free in partials do
            let recorded = ResizeArray<LeastSquaresSystem>()
            let solution = linearSolveOf setups [ qz ] runs study recorded
            assertLoneSolve qz qzTruth free solution
            Assert.Equal(1, recorded.Count)
            assertSystemOver free.Length (scienceRuns |> List.map (fun run -> run, sourcesOf qzTruth free 0)) recorded.[0]

        // (a) m00 alone free behind a fixed ideal CP of either handedness, analyzed by the calibrated linear LP2.
        let qzM00 = qzHolding m00Free

        for prep, prefix in [ cpLeft, "L"; cpRight, "R" ] do
            let bench = turnedBench $"CP {prefix}-QZ-LP" prep [ qzM00, Angle.zero ] calibratedLp2
            let planned = freelyPlanned (sweptRunOver bench $"{prefix}1" ScienceRun None) [ qzM00 ] qzTruth
            let observed = observedOver [ bench ] (objectInstruments @ [ prep; qzM00 ]) [ planned ]
            let kitSetups, kitRuns, kitStudy = freeStudyOf $"CP {prefix}" [ bench ] observed
            let recorded = ResizeArray<LeastSquaresSystem>()
            let solution = linearSolveOf kitSetups [ prep; qzM00 ] kitRuns kitStudy recorded
            assertLoneSolve qzM00 qzTruth [ 0, 0 ] solution
            Assert.Equal(1, recorded.Count)
            assertSystemOver 1 (observed |> List.map (fun run -> run, sourcesOf qzTruth [ 0, 0 ] 0)) recorded.[0]

        // (b) behind the fixed constant-Mueller preparation (the compound CPL1 prepares above): its circular state shows
        // QZ's first column through LP2.
        let firstColumn = [ 0, 0; 1, 0; 2, 0 ]
        let qzFirstColumn = qzHolding (placesBut firstColumn |> List.map (fun (row, column) -> row, column, knownIn qzTruth row column))

        let constantBenches = [ 0.0<deg>; 30.0<deg> ] |> List.map (fun phi -> turnedBench $"CMP-QZ-LP {phi}" constantPrep [ qzFirstColumn, Angle.degree phi ] calibratedLp2)
        let constantPlanned = constantBenches |> List.mapi (fun k bench -> freelyPlanned (sweptRunOver bench $"C{k + 1}" ScienceRun None) [ qzFirstColumn ] qzTruth)
        let constantObserved = observedOver constantBenches (objectInstruments @ [ constantPrep; qzFirstColumn ]) constantPlanned
        let constantSetups, constantRuns, constantStudy = freeStudyOf "CMP" constantBenches constantObserved
        let constantRecorded = ResizeArray<LeastSquaresSystem>()
        let constantSolution = linearSolveOf constantSetups [ constantPrep; qzFirstColumn ] constantRuns constantStudy constantRecorded
        assertLoneSolve qzFirstColumn qzTruth firstColumn constantSolution
        Assert.Equal(1, constantRecorded.Count)
        assertSystemOver 3 (constantObserved |> List.map (fun run -> run, sourcesOf qzTruth firstColumn 0)) constantRecorded.[0]

        // (c) between fixed objects: the quarter-wave plate at 30° before it, the known LR after it, its upper 3 × 3 free.
        let block = [ for row in 0 .. 2 do for column in 0 .. 2 -> row, column ]
        let qzBlock = qzHolding (placesBut block |> List.map (fun (row, column) -> row, column, knownIn qzTruth row column))

        let betweenBenches =
            phis
            |> List.map (fun phi ->
                turnedBench $"LP-WP-QZ-LR-LP {phi}" lp1 [ plate, Angle.degree 30.0<deg>; qzBlock, Angle.degree phi; lrKnown, Angle.zero ] calibratedLp2)

        let betweenPlanned = betweenBenches |> List.mapi (fun k bench -> freelyPlanned (sweptRunOver bench $"S{k + 1}" ScienceRun None) [ qzBlock ] qzTruth)
        let betweenObserved = observedOver betweenBenches (objectInstruments @ [ lrKnown; qzBlock ]) betweenPlanned
        let betweenSetups, betweenRuns, betweenStudy = freeStudyOf "Between" betweenBenches betweenObserved
        let betweenRecorded = ResizeArray<LeastSquaresSystem>()
        let betweenSolution = linearSolveOf betweenSetups [ lrKnown; qzBlock ] betweenRuns betweenStudy betweenRecorded
        assertLoneSolve qzBlock qzTruth block betweenSolution
        Assert.Equal(1, betweenRecorded.Count)
        assertSystemOver 9 (betweenObserved |> List.map (fun run -> run, sourcesOf qzTruth block 0)) betweenRecorded.[0]

        // (d) equality links: QZ's m21 follows its m12, QZ2's m00 follows QZ's free m00 across separately observed runs,
        // and QZ2's m33 QZ's known m33, a fixed leader; the truths satisfy them. One system stacks both objects' rows on
        // the shared columns, so solving each object alone cannot pass.
        let qzLinked = qzHolding [ 3, 3, knownIn qzTruth 3 3 ]
        let qzBenches, qzPlanned = freeFamilyRuns "Q" [ qzLinked ] qzTruth
        let qz2Benches, qz2Planned = freeFamilyRuns "W" [ qz2Free ] qz2Truth
        let linkedItems = [ qzLinked; qz2Free ]
        let linkedObserved = observedOver (qzBenches @ qz2Benches) (objectInstruments @ linkedItems) (qzPlanned @ qz2Planned)
        let linkedSetups, linkedRuns, unlinked = freeStudyOf "Linked" (qzBenches @ qz2Benches) linkedObserved

        let linked =
            let used = (contextWithin linkedSetups (objectInstruments @ linkedItems) linkedRuns unlinked).kit.itemsUsedBy unlinked

            let links : ParameterLink list =
                [
                    { follower = ItemParameterRef (qzLinked.id, coefficient 2 1); leader = ItemParameterRef (qzLinked.id, coefficient 1 2) }
                    { follower = ItemParameterRef (qz2Free.id, coefficient 0 0); leader = ItemParameterRef (qzLinked.id, coefficient 0 0) }
                    { follower = ItemParameterRef (qz2Free.id, coefficient 3 3); leader = ItemParameterRef (qzLinked.id, coefficient 3 3) }
                ]

            links |> List.fold (fun (held : Study) link -> studyOf (held.withLink link used)) unlinked

        let linkedRecorded = ResizeArray<LeastSquaresSystem>()
        let linkedSolution = linearSolveOf linkedSetups linkedItems linkedRuns linked linkedRecorded

        // QZ's fourteen entries (m21 follows, m33 is known) in columns 0 to 13, then QZ2's fourteen (m00 and m33 follow).
        let qzFree = placesBut [ 2, 1; 3, 3 ]
        let qz2Own = placesBut [ 0, 0; 3, 3 ]

        let qzSources (place : int * int) : CoefficientSource =
            match place with
            | 2, 1 -> sourcesOf qzTruth qzFree 0 (1, 2)
            | _ -> sourcesOf qzTruth qzFree 0 place

        let qz2Sources (place : int * int) : CoefficientSource =
            match place with
            | 0, 0 -> sourcesOf qzTruth qzFree 0 (0, 0)
            | 3, 3 -> FixedAtValue (qzTruth.element 3 3)
            | _ -> sourcesOf qz2Truth qz2Own qzFree.Length place

        Assert.Equal(LinearStudyEngine, linkedSolution.engine)
        Assert.Equal(1, linkedRecorded.Count)

        let qzRuns, qz2Runs = linkedObserved |> List.splitAt qzPlanned.Length
        assertSystemOver 28 ((qzRuns |> List.map (fun run -> run, qzSources)) @ (qz2Runs |> List.map (fun run -> run, qz2Sources))) linkedRecorded.[0]

        Assert.Equal<StudyParameterRef list>(
            (qzFree |> List.map (fun (row, column) -> ItemParameterRef (qzLinked.id, coefficient row column)))
            @ (qz2Own |> List.map (fun (row, column) -> ItemParameterRef (qz2Free.id, coefficient row column))),
            linkedSolution.layout.entries |> List.map (fun entry -> entry.unknown.parameter))

        assertValuesWithin 1e-9 (elementsAt qzTruth qzFree) (fittedCoefficientsOf linkedSolution qzLinked qzFree)
        assertValuesWithin 1e-9 (elementsAt qz2Truth qz2Own) (fittedCoefficientsOf linkedSolution qz2Free qz2Own)

        let qzFit = objectFitOf [ qzLinked ] linkedSolution
        let qz2Fit = objectFitOf [ qz2Free ] linkedSolution
        let elementOf (fit : ObjectMatrixFit) (row : int) (column : int) : float = (List.item (placeOf row column) fit.elements).value

        for fit, truth in [ qzFit, qzTruth; qz2Fit, qz2Truth ] do
            Assert.Equal(MatrixRank 28, fit.rank)
            assertValuesWithin 1e-9 (truth.rowMajorValues |> List.map (fun value -> value.value)) fit.elements

        // The known leader is kept exactly, and each follower is its leader's value exactly.
        Assert.Equal<float>(qzTruth.element 3 3, elementOf qzFit 3 3)
        Assert.Equal<float>(qzTruth.element 3 3, elementOf qz2Fit 3 3)
        Assert.Equal<float>(elementOf qzFit 1 2, elementOf qzFit 2 1)
        Assert.Equal<float>(elementOf qzFit 0 0, elementOf qz2Fit 0 0)

    [<Fact>]
    let ``a series of two black-box objects is reconstructed as one combined matrix`` () : unit =
        let qzBenches, qzPlanned = freeFamilyRuns "Q" [ qzAllFree ] qzTruth
        let lrBenches, lrPlanned = freeFamilyRuns "L" [ lrFree ] lrTruth
        let combined = cascadeProduct lrTruth qzTruth
        let seriesBenches, seriesPlanned = freeFamilyRuns "QL" [ qzAllFree; lrFree ] combined
        let benches = qzBenches @ lrBenches @ seriesBenches
        let observed = observedOver benches (objectInstruments @ [ qzAllFree; lrFree ]) (qzPlanned @ lrPlanned @ seriesPlanned)
        let setups, runs, study = freeStudyOf "QZ-LR" benches observed
        let staged, context, prefits, data = objectStageOf setups (objectInstruments @ [ qzAllFree; lrFree ]) runs study
        let recorded = ResizeArray<LeastSquaresSystem>()
        let solution = solutionOf (staged.reconstructLinear context prefits data (recordingSvd recorded))

        // QZ's and LR's own runs fit their coefficients; the series' rows give only its combined matrix.
        Assert.Equal(LinearStudyEngine, solution.engine)

        assertValuesWithin 1e-9 (elementsAt qzTruth coefficientPlaces) (fittedCoefficientsOf solution qzAllFree coefficientPlaces)
        assertValuesWithin 1e-9 (elementsAt lrTruth coefficientPlaces) (fittedCoefficientsOf solution lrFree coefficientPlaces)

        // One object solve per group in first-run order, the series' M_comb against S11.4's cascade M_LR · M_QZ.
        Assert.Equal<ItemId list list>(
            [ [ qzAllFree.id ]; [ lrFree.id ]; [ qzAllFree.id; lrFree.id ] ],
            solution.objectMatrices |> List.map (fun fit -> fit.items))

        for fit, truth in List.zip solution.objectMatrices [ qzTruth; lrTruth; combined ] do
            Assert.Equal(MatrixRank 16, fit.rank)
            assertValuesWithin 1e-9 (truth.rowMajorValues |> List.map (fun value -> value.value)) fit.elements

        // Each group is one sixteen-column `reconstruct`, the series' over its own rows' combined designs.
        Assert.Equal(3, recorded.Count)
        let seriesObserved = observed |> List.skip (qzPlanned.Length + lrPlanned.Length)
        assertSystemOver 16 (seriesObserved |> List.map (fun run -> run, vecSources)) recorded.[2]

    [<Fact>]
    let ``an object stage whose design is rank-deficient is refused naming its objects`` () : unit =
        // QZ's runs of the LP-LP family at φ 0° alone: a linear state into a linear analyzer, three directions of sixteen.
        let bench = turnedBench "QZ LP-LP 0" lp1 [ qzAllFree, Angle.zero ] calibratedLp2
        let planned = freelyPlanned (sweptRunOver bench "E1" ScienceRun None) [ qzAllFree ] qzTruth
        let observed = observedOver [ bench ] (objectInstruments @ [ qzAllFree ]) [ planned ]
        let setups, runs, study = freeStudyOf "QZ alone" [ bench ] observed

        // The full matrix, then the partial variant with only m33 fixed, judged against its 15 free columns.
        for qz, columns in [ qzAllFree, 16; qzHolding [ 3, 3, knownIn qzTruth 3 3 ], 15 ] do
            let recorded = ResizeArray<LeastSquaresSystem>()
            let staged, context, prefits, data = objectStageOf setups (objectInstruments @ [ qz ]) runs study
            Assert.Equal(LinearStudyEngine, staged.engineWith context prefits)

            match staged.reconstructLinear context prefits data (recordingSvd recorded) with
            | Error (ObjectSolveFitErr (items, RankDeficientErr rank)) ->
                Assert.Equal<ItemId list>([ qz.id ], items)
                Assert.InRange(rank.value, 1, columns - 1)
            | Error error -> Assert.Fail($"expected ObjectSolveFitErr naming QZ, got {error.describe}")
            | Ok _ -> Assert.Fail("a rank-deficient object stage was fitted")

            // The solver was reached, with the variant's own columns.
            Assert.Equal(1, recorded.Count)
            Assert.All(recorded.[0].design, fun row -> Assert.Equal(columns, row.value.Length))

    // ============================ spec 0050 S11.3: one fit, one fit run ============================

    /// 2026-11-04 09:30:00 UTC, the instant the test clock answers.
    let private fitInstant : ClockTime = ClockTime (DateTime(2026, 11, 4, 9, 30, 0, DateTimeKind.Utc))

    /// The fit's inputs over the context: a clock answering the fit instant and recording each read, the linear proxy,
    /// deviates failing the fact on any draw, and no earlier fit run.
    let private fitParamOver
        (context : StudyForwardContext)
        (reads : ResizeArray<ClockTime>)
        (solver : MuellerSolverProxy)
        (drawn : ResizeArray<UniformDeviate>)
        : StudyFitParam =
        let clock : ClockProvider =
            {
                now =
                    fun () ->
                        reads.Add fitInstant
                        fitInstant
            }

        {
            forward = context
            clock = clock
            linearSolver = solver
            deviates = deviatesOf [] drawn
            fitRuns = []
        }

    /// The study's fit at the stage with its calibrations held.
    let private heldRequest (study : Study) (stage : FitStage) : StudyFitRequest =
        { study = study.name; stage = stage; calibrationUse = CalibrationHeldFixed }

    let private fitRunOf (result : Result<FitRun, StudyFitError>) : FitRun =
        match result with
        | Ok run -> run
        | Error error -> failwith $"expected a fit run, got {error.describe}"

    /// Each item's parameters as a fit run records them, item by item, each with its state.
    let private snapshotOf (items : Item list) : FitRunParameter list =
        items
        |> List.collect (fun item ->
            item.parameters |> List.map (fun parameter -> ({ parameter = ItemParameterRef (item.id, parameter.name); state = parameter.state } : FitRunParameter)))

    /// The fitted values of the item's coefficients at these places, failing the fact when the run fits one not.
    let private fittedInRun (run : FitRun) (item : Item) (places : (int * int) list) : ParameterValue list =
        places
        |> List.map (fun (row, column) ->
            match run.entries |> List.tryFind (fun entry -> entry.parameter = ItemParameterRef (item.id, coefficient row column)) with
            | Some entry -> entry.fitted
            | None -> failwith $"no entry fits {item.label.value}'s m{row}{column}")

    /// The study's linear fit run at the stage, its calibrations held, over a kit of the items within these setups,
    /// through the recording SVD: read once by the clock, no iteration told to its watch and no deviate drawn, so a
    /// nonlinear solve or an earlier refusal fails the fact. It fits the stage's runs, records every run's attachment
    /// in study order and every parameter of the items the fitted runs use, `used` in the kit's order.
    let private linearFitRunOf
        (setups : Setup list)
        (items : Item list)
        (used : Item list)
        (runs : AttachedRun list)
        (study : Study)
        (stage : FitStage)
        (recorded : ResizeArray<LeastSquaresSystem>)
        : FitRun =
        let reads = ResizeArray<ClockTime>()
        let drawn = ResizeArray<UniformDeviate>()
        let reports = ResizeArray<IterationCount>()
        let watch = WatchedIterations { reportIteration = reports.Add; cancellation = CancellationToken.None }
        let fit = fitParamOver (contextWithin setups items runs study) reads (recordingSvd recorded) drawn
        let run = fitRunOf (fit.runFit (heldRequest study stage) watch)

        Assert.Empty(reports)
        Assert.Empty(drawn)
        Assert.Equal(1, reads.Count)
        Assert.NotEmpty(recorded)
        Assert.Equal(LinearStudyEngine, run.engine)
        Assert.Equal(study.name.fitRunNameAt fitInstant, run.name)
        Assert.Equal(fitInstant, run.stamp)
        Assert.Equal(JointFit, run.mode)
        Assert.Equal(stage, run.stage)
        Assert.Equal(CalibrationHeldFixed, run.calibrationUse)
        Assert.Equal(FitRunNotes.none, run.notes)

        let fittedRuns =
            runs
            |> List.map (fun attached -> attached.attachedRun)
            |> List.filter (fun experiment ->
                match stage, experiment.role with
                | ObjectStage, ScienceRun
                | JointStage, (ScienceRun | CalibrationRun) -> true
                | _ -> false)
            |> List.map (fun experiment -> experiment.name)

        Assert.Equal<ExperimentName list>(fittedRuns, run.runs)

        Assert.Equal<DataAttachment list>(
            runs |> List.choose (fun attached -> attached.attachedRun.dataOpt |> Option.map (fun data -> data.attachment)),
            run.dataRead)

        Assert.Equal<FitRunParameter list>(snapshotOf used, run.parameters)
        run

    /// A lone black box's fit run: its free places in row-major order its only entries, each from the identity's start to
    /// within 1e-9 of the truth; one complete object solve of it of rank `free.Length`, exact at each fixed place; and one
    /// converged member from the entries' starts to their fitted values.
    let private assertLoneFitRun (item : Item) (truth : MuellerMatrix) (free : (int * int) list) (run : FitRun) : unit =
        Assert.Equal<StudyParameterRef list>(
            free |> List.map (fun (row, column) -> ItemParameterRef (item.id, coefficient row column)),
            run.entries |> List.map (fun entry -> entry.parameter))

        assertValuesWithin 0.0 (free |> List.map (fun (row, column) -> if row = column then 1.0 else 0.0)) (run.entries |> List.map (fun entry -> entry.start))
        assertValuesWithin 1e-9 (elementsAt truth free) (run.entries |> List.map (fun entry -> entry.fitted))

        match run.objectMatrices with
        | [ fit ] ->
            Assert.Equal<ItemId list>([ item.id ], fit.items)
            Assert.Equal(MatrixRank free.Length, fit.rank)
            assertValuesWithin 1e-9 (truth.rowMajorValues |> List.map (fun value -> value.value)) fit.elements

            for row, column in placesBut free do
                Assert.Equal<float>(truth.element row column, (List.item (placeOf row column) fit.elements).value)
        | fits -> Assert.Fail($"expected one object solve, got {fits.Length}")

        match run.members with
        | [ { start = start; outcome = MemberConverged (fitted, Some _) } ] ->
            Assert.Equal<ParameterValue list>(run.entries |> List.map (fun entry -> entry.start), start)
            Assert.Equal<ParameterValue list>(run.entries |> List.map (fun entry -> entry.fitted), fitted)
        | members -> Assert.Fail($"expected one converged member with a χ², got %A{members}")

    [<Fact>]
    let ``a fit run records its name, stamp, stage, engine, fitted runs, the data it read and the vector it used and produced`` () : unit =
        // Step 387's polarimeter study jointly, its calibrations held and unwatched, against `fitNonlinear` itself.
        let study = plateStudyWith None
        let context = plateContextOf study
        let staged, _, data = stagedOver context study
        let expected = solutionOf (staged.fitNonlinear context data (deviatesOf [] (ResizeArray<UniformDeviate>())) UnwatchedIterations)
        let reads = ResizeArray<ClockTime>()
        let drawn = ResizeArray<UniformDeviate>()
        let recorded = ResizeArray<LeastSquaresSystem>()
        let fit = fitParamOver context reads (recordingSvd recorded) drawn
        let run = fitRunOf (fit.runFit { study = study.name; stage = JointStage; calibrationUse = CalibrationHeldFixed } UnwatchedIterations)

        Assert.Equal(FitRun.currentSchemaVersion, run.schemaVersion)
        Assert.Equal(FitRunName $"{study.name.fileStem.value}@20261104T093000Z", run.name)
        Assert.Equal(study.name, run.study)
        Assert.Equal(fitInstant, run.stamp)
        Assert.Equal(1, reads.Count)
        Assert.Equal(JointFit, run.mode)
        Assert.Equal(JointStage, run.stage)
        Assert.Equal(CalibrationHeldFixed, run.calibrationUse)
        Assert.Equal(NonlinearStudyEngine, run.engine)
        Assert.Empty(recorded)
        Assert.Empty(drawn)
        Assert.Equal<ExperimentName list>([ plateRun.attachedRun.name ], run.runs)

        let plateAttachment : DataAttachment = { path = KitRelativePath "data/P1.csv"; contentHash = LibraryEntryFiles.ContentHash "0386" }
        Assert.Equal<DataAttachment list>([ plateAttachment ], run.dataRead)

        // The items' parameters, then the opened material's four components with their states: n_o and n_e unknown.
        let components =
            match study.materials with
            | [ material ] ->
                material.parameters
                |> List.map (fun parameter -> ({ parameter = MaterialParameterRef (indexVersion, parameter.name); state = parameter.state } : FitRunParameter))
            | materials -> failwith $"expected one opened material, got {materials.Length}"

        Assert.Equal(4, components.Length)
        Assert.Equal<FitRunParameter list>(snapshotOf [ laser; indexItem; polarimeter ] @ components, run.parameters)

        // n_o's and n_e's entries from their starts to `fitNonlinear`'s values, and its one member.
        Assert.Equal<StudyParameterRef list>(
            [ MaterialParameterRef (indexVersion, ordinaryIndex); MaterialParameterRef (indexVersion, extraordinaryIndex) ],
            run.entries |> List.map (fun entry -> entry.parameter))

        assertValuesWithin 1e-12 [ ordinaryStart; extraordinaryStart ] (run.entries |> List.map (fun entry -> entry.start))

        let expectedEntries =
            expected.layout.entries
            |> List.mapi (fun k entry ->
                ({
                    parameter = entry.unknown.parameter
                    owner = entry.owner
                    start = entry.unknown.unknown.start
                    fitted = expected.fitted.valueAt (StudyVectorIndex k)
                } : FitRunEntry))

        Assert.Equal<FitRunEntry list>(expectedEntries, run.entries)
        Assert.Equal(1, run.members.Length)
        Assert.Equal<FitRunMember list>(expected.members, run.members)
        Assert.Empty(run.objectMatrices)
        Assert.Equal(FitRunNotes.none, run.notes)

        // Step 390's fixtures through automatic dispatch, at stage 2 and jointly: each a real SVD solve answering a linear
        // fit run. QZ every coefficient free, m33 known (15 free), m23 known with m32 calibrated, and m00 alone free.
        let setups, runs, sessionStudy, _ = sessionFixture ()
        let m33Known = knownIn qzTruth 3 3
        let qzM33 = qzHolding [ 3, 3, m33Known ]
        let m00Free = placesBut [ 0, 0 ] |> List.map (fun (row, column) -> row, column, knownIn qzTruth row column)

        let sessionVariants : (Item * (int * int) list) list =
            [
                qzAllFree, coefficientPlaces
                qzHolding [ 2, 3, knownIn qzTruth 2 3; 3, 2, calibratedAt (qzTruth.element 3 2) ], placesBut [ 2, 3; 3, 2 ]
                qzHolding m00Free, [ 0, 0 ]
            ]

        for stage in objectStages do
            for qz, free in sessionVariants do
                let solves = ResizeArray<LeastSquaresSystem>()
                let fitted = linearFitRunOf setups (objectInstruments @ [ qz ]) (objectInstruments @ [ qz ]) runs sessionStudy stage solves
                Assert.Equal(1, solves.Count)
                assertLoneFitRun qz qzTruth free fitted

            // Only m33 fixed: fifteen entries, m33 kept exactly with its known state among the parameters, never an entry.
            let m33Solves = ResizeArray<LeastSquaresSystem>()
            let m33Run = linearFitRunOf setups (objectInstruments @ [ qzM33 ]) (objectInstruments @ [ qzM33 ]) runs sessionStudy stage m33Solves
            let m33 = ItemParameterRef (qzM33.id, coefficient 3 3)
            Assert.Equal(1, m33Solves.Count)
            assertLoneFitRun qzM33 qzTruth (placesBut [ 3, 3 ]) m33Run
            Assert.Equal(15, m33Run.entries.Length)
            Assert.DoesNotContain(m33, m33Run.entries |> List.map (fun entry -> entry.parameter))
            Assert.Contains(({ parameter = m33; state = m33Known } : FitRunParameter), m33Run.parameters)
            Assert.Equal(MatrixRank 15, (List.exactlyOne m33Run.objectMatrices).rank)
            Assert.Equal<float>(qzTruth.element 3 3, (List.item (placeOf 3 3) (List.exactlyOne m33Run.objectMatrices).elements).value)

        // (a) m00 alone free behind a fixed ideal CP of either handedness: rank 1, one entry.
        let qzM00 = qzHolding m00Free

        for prep, prefix in [ cpLeft, "L"; cpRight, "R" ] do
            let bench = turnedBench $"CP {prefix}-QZ-LP" prep [ qzM00, Angle.zero ] calibratedLp2
            let planned = freelyPlanned (sweptRunOver bench $"{prefix}1" ScienceRun None) [ qzM00 ] qzTruth
            let observed = observedOver [ bench ] (objectInstruments @ [ prep; qzM00 ]) [ planned ]
            let kitSetups, kitRuns, kitStudy = freeStudyOf $"CP {prefix}" [ bench ] observed

            for stage in objectStages do
                let solves = ResizeArray<LeastSquaresSystem>()
                let used = [ laser; calibratedLp2; phone; prep; qzM00 ]
                let fitted = linearFitRunOf kitSetups (objectInstruments @ [ prep; qzM00 ]) used kitRuns kitStudy stage solves
                Assert.Equal(1, solves.Count)
                Assert.Equal(1, fitted.entries.Length)
                assertLoneFitRun qzM00 qzTruth [ 0, 0 ] fitted

            // The Science run's file changed since attach: it is recorded at the hash the fit read, its current one, and the
            // unchanged Dark run's at its own.
            let changedHash = LibraryEntryFiles.ContentHash "0391"

            let attachments =
                kitRuns |> List.choose (fun attached -> attached.attachedRun.dataOpt |> Option.map (fun data -> data.attachment))

            let changed, unchanged =
                match attachments with
                | [ science; dark ] -> science, dark
                | held -> failwith $"expected the Science and the Dark run's files, got {held.Length}"

            let kitContext = contextWithin kitSetups (objectInstruments @ [ prep; qzM00 ]) kitRuns kitStudy

            let changedData : ExperimentDataProxy =
                {
                    kitContext.experimentData with
                        attachmentStatus = fun attachment -> Ok (if attachment.path = changed.path then AttachedDataChanged changedHash else AttachedDataUnchanged)
                }

            let changedFit = fitParamOver { kitContext with experimentData = changedData } (ResizeArray<ClockTime>()) (createMathNetSvd ()) (ResizeArray<UniformDeviate>())
            let changedRun = fitRunOf (changedFit.runFit (heldRequest kitStudy ObjectStage) UnwatchedIterations)
            Assert.NotEqual(changed.contentHash, changedHash)
            Assert.Equal<DataAttachment list>([ { changed with contentHash = changedHash }; unchanged ], changedRun.dataRead)

        // (b) QZ's first column behind the fixed constant-Mueller preparation.
        let firstColumn = [ 0, 0; 1, 0; 2, 0 ]
        let qzFirstColumn = qzHolding (placesBut firstColumn |> List.map (fun (row, column) -> row, column, knownIn qzTruth row column))
        let constantBenches = [ 0.0<deg>; 30.0<deg> ] |> List.map (fun phi -> turnedBench $"CMP-QZ-LP {phi}" constantPrep [ qzFirstColumn, Angle.degree phi ] calibratedLp2)
        let constantPlanned = constantBenches |> List.mapi (fun k bench -> freelyPlanned (sweptRunOver bench $"C{k + 1}" ScienceRun None) [ qzFirstColumn ] qzTruth)
        let constantObserved = observedOver constantBenches (objectInstruments @ [ constantPrep; qzFirstColumn ]) constantPlanned
        let constantSetups, constantRuns, constantStudy = freeStudyOf "CMP" constantBenches constantObserved

        // (c) QZ's upper 3 × 3 between the fixed plate at 30° and the known LR.
        let block = [ for row in 0 .. 2 do for column in 0 .. 2 -> row, column ]
        let qzBlock = qzHolding (placesBut block |> List.map (fun (row, column) -> row, column, knownIn qzTruth row column))

        let betweenBenches =
            phis
            |> List.map (fun phi ->
                turnedBench $"LP-WP-QZ-LR-LP {phi}" lp1 [ plate, Angle.degree 30.0<deg>; qzBlock, Angle.degree phi; lrKnown, Angle.zero ] calibratedLp2)

        let betweenPlanned = betweenBenches |> List.mapi (fun k bench -> freelyPlanned (sweptRunOver bench $"S{k + 1}" ScienceRun None) [ qzBlock ] qzTruth)
        let betweenObserved = observedOver betweenBenches (objectInstruments @ [ lrKnown; qzBlock ]) betweenPlanned
        let betweenSetups, betweenRuns, betweenStudy = freeStudyOf "Between" betweenBenches betweenObserved

        for stage in objectStages do
            let constantSolves = ResizeArray<LeastSquaresSystem>()
            let constantUsed = [ laser; calibratedLp2; phone; constantPrep; qzFirstColumn ]
            let constantItems = objectInstruments @ [ constantPrep; qzFirstColumn ]
            let constantRun = linearFitRunOf constantSetups constantItems constantUsed constantRuns constantStudy stage constantSolves
            Assert.Equal(1, constantSolves.Count)
            assertLoneFitRun qzFirstColumn qzTruth firstColumn constantRun

            let betweenSolves = ResizeArray<LeastSquaresSystem>()
            let betweenUsed = [ laser; lp1; calibratedLp2; phone; plate; lrKnown; qzBlock ]
            let betweenItems = objectInstruments @ [ lrKnown; qzBlock ]
            let betweenRun = linearFitRunOf betweenSetups betweenItems betweenUsed betweenRuns betweenStudy stage betweenSolves
            Assert.Equal(1, betweenSolves.Count)
            assertLoneFitRun qzBlock qzTruth block betweenRun

        // (d) Equality links: QZ's m21 follows its m12, QZ2's m00 follows QZ's free m00, and QZ2's m33 QZ's known m33.
        let qzLinked = qzHolding [ 3, 3, m33Known ]
        let qzBenches, qzPlanned = freeFamilyRuns "Q" [ qzLinked ] qzTruth
        let qz2Benches, qz2Planned = freeFamilyRuns "W" [ qz2Free ] qz2Truth
        let linkedItems = objectInstruments @ [ qzLinked; qz2Free ]
        let linkedObserved = observedOver (qzBenches @ qz2Benches) linkedItems (qzPlanned @ qz2Planned)
        let linkedSetups, linkedRuns, unlinked = freeStudyOf "Linked" (qzBenches @ qz2Benches) linkedObserved

        let linked =
            let used = (contextWithin linkedSetups linkedItems linkedRuns unlinked).kit.itemsUsedBy unlinked

            let links : ParameterLink list =
                [
                    { follower = ItemParameterRef (qzLinked.id, coefficient 2 1); leader = ItemParameterRef (qzLinked.id, coefficient 1 2) }
                    { follower = ItemParameterRef (qz2Free.id, coefficient 0 0); leader = ItemParameterRef (qzLinked.id, coefficient 0 0) }
                    { follower = ItemParameterRef (qz2Free.id, coefficient 3 3); leader = ItemParameterRef (qzLinked.id, coefficient 3 3) }
                ]

            links |> List.fold (fun (held : Study) link -> studyOf (held.withLink link used)) unlinked

        let qzFree = placesBut [ 2, 1; 3, 3 ]
        let qz2Own = placesBut [ 0, 0; 3, 3 ]

        for stage in objectStages do
            let solves = ResizeArray<LeastSquaresSystem>()

            // Every parameter of both objects stays in the record with its own state, the followers' included.
            let linkedRun = linearFitRunOf linkedSetups linkedItems linkedItems linkedRuns linked stage solves
            Assert.Equal(1, solves.Count)

            // One entry per independent coefficient: QZ's fourteen, then QZ2's fourteen; no follower is an entry.
            Assert.Equal<StudyParameterRef list>(
                (qzFree |> List.map (fun (row, column) -> ItemParameterRef (qzLinked.id, coefficient row column)))
                @ (qz2Own |> List.map (fun (row, column) -> ItemParameterRef (qz2Free.id, coefficient row column))),
                linkedRun.entries |> List.map (fun entry -> entry.parameter))

            assertValuesWithin 1e-9 (elementsAt qzTruth qzFree) (fittedInRun linkedRun qzLinked qzFree)
            assertValuesWithin 1e-9 (elementsAt qz2Truth qz2Own) (fittedInRun linkedRun qz2Free qz2Own)

            // Two complete matrices of the reduced rank, each follower its leader's value exactly.
            match linkedRun.objectMatrices with
            | [ qzFit; qz2Fit ] ->
                let elementOf (fit : ObjectMatrixFit) (row : int) (column : int) : float = (List.item (placeOf row column) fit.elements).value

                for fit, truth, item in [ qzFit, qzTruth, qzLinked; qz2Fit, qz2Truth, qz2Free ] do
                    Assert.Equal<ItemId list>([ item.id ], fit.items)
                    Assert.Equal(MatrixRank 28, fit.rank)
                    assertValuesWithin 1e-9 (truth.rowMajorValues |> List.map (fun value -> value.value)) fit.elements

                Assert.Equal<float>(elementOf qzFit 1 2, elementOf qzFit 2 1)
                Assert.Equal<float>((List.exactlyOne (fittedInRun linkedRun qzLinked [ 0, 0 ])).value, elementOf qz2Fit 0 0)
                Assert.Equal<float>(qzTruth.element 3 3, elementOf qzFit 3 3)
                Assert.Equal<float>(qzTruth.element 3 3, elementOf qz2Fit 3 3)
            | fits -> Assert.Fail($"expected QZ's and QZ2's object solves, got {fits.Length}")

        // A rank-deficient design reaches the solver and answers its refusal: no fit run, so the clock is never read.
        let deficientBench = turnedBench "QZ LP-LP 0" lp1 [ qzM33, Angle.zero ] calibratedLp2
        let deficientPlanned = freelyPlanned (sweptRunOver deficientBench "E1" ScienceRun None) [ qzM33 ] qzTruth
        let deficientObserved = observedOver [ deficientBench ] (objectInstruments @ [ qzM33 ]) [ deficientPlanned ]
        let deficientSetups, deficientRuns, deficientStudy = freeStudyOf "QZ alone" [ deficientBench ] deficientObserved

        for stage in objectStages do
            let solves = ResizeArray<LeastSquaresSystem>()
            let deficientReads = ResizeArray<ClockTime>()
            let deficientContext = contextWithin deficientSetups (objectInstruments @ [ qzM33 ]) deficientRuns deficientStudy
            let deficientFit = fitParamOver deficientContext deficientReads (recordingSvd solves) (ResizeArray<UniformDeviate>())

            match deficientFit.runFit (heldRequest deficientStudy stage) UnwatchedIterations with
            | Error (ObjectSolveFitErr (items, RankDeficientErr rank)) ->
                Assert.Equal<ItemId list>([ qzM33.id ], items)
                Assert.InRange(rank.value, 1, 14)
            | Error error -> Assert.Fail($"expected ObjectSolveFitErr naming QZ, got {error.describe}")
            | Ok deficientRun -> Assert.Fail($"a rank-deficient design was recorded as {deficientRun.name.value}")

            Assert.Equal(1, solves.Count)
            Assert.Empty(deficientReads)

    [<Fact>]
    let ``a fit naming no study of the kit, or refused by its stage, records nothing`` () : unit =
        // Step 385's study: A1 and A2 over CPL1 -> AIR -> LP1, E1 over CPL1 -> QZ -> LP1, and K1; CPL1's retardance
        // unknown from 1.5 within [1, 2] and QZ's m00 from 1 within [0, 2].
        let cpl1Retardance = itemOf cpl1Draft |> stating retardance (unknownWithin 1.5 1.0 2.0)
        let qzM00 = itemOf qzDraft |> stating (coefficient 0 0) (unknownWithin 1.0 0.0 2.0)
        let setupA = benchOf "A" cpl1Retardance [ None ] lp1 phone
        let setupQ = benchOf "Q" cpl1Retardance [ Some qzM00 ] lp1 phone

        let runs =
            [
                capturedOver setupA "A1" CalibrationRun []
                capturedOver setupA "A2" CalibrationRun []
                capturedOver setupQ "E1" ScienceRun []
                darkOver setupA
            ]

        let study = studyOver "S" runs
        let context = contextWithin [ setupA; setupQ ] [ laser; lp1; cpl1Retardance; qzM00; phone ] runs study
        let reads = ResizeArray<ClockTime>()
        let drawn = ResizeArray<UniformDeviate>()
        let solves = ResizeArray<LeastSquaresSystem>()
        let fit = fitParamOver context reads (recordingSvd solves) drawn

        match fit.runFit { study = StudyName "LR"; stage = JointStage; calibrationUse = CalibrationHeldFixed } UnwatchedIterations with
        | Error (UnknownFitStudyErr reason) -> Assert.Contains("'LR'", reason)
        | Error error -> Assert.Fail($"expected UnknownFitStudyErr, got {error.describe}")
        | Ok run -> Assert.Fail($"a fit of a study the kit lacks was recorded as {run.name.value}")

        match fit.runFit (heldRequest study ObjectStage) UnwatchedIterations with
        | Error (FitStageErr (InstrumentNotCalibratedErr reason)) -> Assert.Contains("'CPL1' c2.retardance", reason)
        | Error error -> Assert.Fail($"expected FitStageErr (InstrumentNotCalibratedErr _), got {error.describe}")
        | Ok run -> Assert.Fail($"a stage refusing an uncalibrated instrument was recorded as {run.name.value}")

        // Neither request read the clock, solved or drew a start.
        Assert.Empty(reads)
        Assert.Empty(solves)
        Assert.Empty(drawn)

    [<Fact>]
    let ``every study-fit error renders its condition and its payload`` () : unit =
        let forwardError = MissingStudyRunErr "E9, which the study 'QZ' names"
        let stageError = NoStageRunErr "'QZ' has no enabled run that Stage 2 (objects) fits"
        let gainReason = "'Session 1' anchors its gain to A1"
        let failuresReason = "member 1: the solver did not converge: BackendFailed"
        let cancelReason = "1 of 3 member(s) ran, the last one's result dropped"
        let stagedError = EmptyCaptureSetErr "the darkness checks hold no capture row, so stage one has no dark mean"
        let gainModelError = MissingAnchorGainErr "'A9' is not among the 2 AIR gains given"
        let solverError = RankDeficientErr (MatrixRank 3)
        let objects = [ qzAllFree.id; lrFree.id ]
        let studyReason = "'LR', which the fit names, is not among the kit's 1 studies"

        let cases : (StudyFitError * string) list =
            [
                StudyForwardFitErr forwardError, $"the study's model or data refused the fit: {forwardError.describe}"
                FitStageErr stageError, $"the stage refused the fit: {stageError.describe}"
                SessionGainNeedsLinearEngineErr gainReason, $"the nonlinear engine fits a session's gain as its detector's parameter: {gainReason}"
                NoConvergedMemberErr failuresReason, $"no start of the fit converged: {failuresReason}"
                FitCancelledErr cancelReason, $"the fit was cancelled: {cancelReason}"
                StagedPathFitErr stagedError, $"the staged Mueller path refused the fit: {stagedError.describe}"
                SessionGainFitErr gainModelError, $"a session gain model was refused: {gainModelError.describe}"
                ObjectSolveFitErr (objects, solverError),
                $"the objects could not be reconstructed: {qzAllFree.id.value}, {lrFree.id.value} — {solverError.describe}"
                UnknownFitStudyErr studyReason, $"the kit has no such study: {studyReason}"
            ]

        for error, expected in cases do
            Assert.Equal<string>(expected, error.describe)

        Assert.Contains("E9, which the study 'QZ' names", (StudyForwardFitErr forwardError).describe)
        Assert.Contains("'QZ' has no enabled run that Stage 2 (objects) fits", (FitStageErr stageError).describe)
        Assert.Contains(gainReason, (SessionGainNeedsLinearEngineErr gainReason).describe)
        Assert.Contains(failuresReason, (NoConvergedMemberErr failuresReason).describe)
        Assert.Contains(cancelReason, (FitCancelledErr cancelReason).describe)
        Assert.Contains("the darkness checks hold no capture row", (StagedPathFitErr stagedError).describe)
        Assert.Contains("'A9' is not among the 2 AIR gains given", (SessionGainFitErr gainModelError).describe)
        Assert.Contains($"{lrFree.id.value}", (ObjectSolveFitErr (objects, solverError)).describe)
        Assert.Contains("rank 3", (ObjectSolveFitErr (objects, solverError)).describe)
        Assert.Contains("'LR', which the fit names", (UnknownFitStudyErr studyReason).describe)
        Assert.Equal(cases.Length, cases |> List.map (fun (error, _) -> error.describe) |> List.distinct |> List.length)

    // ============================ spec 0050 S11.3: the staged hand-off ============================

    /// The sigma each stage-1 value is published with; S11.5's `PublishCalibration` takes it from S11.4's results.
    let private handOffSigma : ParameterSigma = ParameterSigma 1.0e-6

    /// Steps 389 and 390's runs in one study over a kit with the five instrument parameters unknown: the AIR Calibration
    /// runs S1 to S3, the Dark run K1 and QZ's sixteen free-session Science runs. QZ's rows are the run law's at the
    /// values the reference stage one reads from the AIR rows, so only those values give M_QZ back. The context, the
    /// study and those values.
    let private handOffFixture () : StudyForwardContext * Study * (Item * ParameterName * float) list =
        let stageOne =
            match calibrateFromAirRows (createMathNetSvd ()) stageOneAirRows with
            | Ok read -> read
            | Error error -> failwith $"stage one refused the rows: {error.describe}"

        let values = stageOneValuesOf stageOne

        // Known, not calibrated, so the rows never share the read of a calibration that stage 2 is checked on.
        let readAt (item : Item) : Item =
            values
            |> List.filter (fun (owner, _, _) -> owner.id = item.id)
            |> List.fold (fun held (_, name, value) -> stating name (KnownParameter (ParameterValue value)) held) item

        let instrumentsRead = values |> List.map (fun (item, _, _) -> item) |> List.distinctBy (fun item -> item.id) |> List.map readAt
        let qzBenches, qzPlanned = freeFamilyRuns "Q" [ qzAllFree ] qzTruth
        let observed = observedOver qzBenches ([ laser; lp1; phone; plate; qzAllFree ] @ instrumentsRead) qzPlanned
        let runs = [ stageOneLpLp; stageOneLpCpl; stageOneCplLp; stageOneDark ] @ (observed |> List.map (fun run -> run.observed))
        let study = studyOver "Hand-off" runs
        let setups = [ lp1AirRealLp2; lpCplAir; cpl1AirRealLp2 ] @ qzBenches
        contextWithin setups (stageOneInstruments @ [ plate; qzAllFree ]) runs study, study, values

    /// Stage 1 of the study through `runFit` over the real SVD, its calibrations held: a linear run.
    let private stageOneRunOf (context : StudyForwardContext) (study : Study) : FitRun =
        let fit = fitParamOver context (ResizeArray<ClockTime>()) (createMathNetSvd ()) (ResizeArray<UniformDeviate>())
        let run = fitRunOf (fit.runFit (heldRequest study CalibrationStage) UnwatchedIterations)
        Assert.Equal(LinearStudyEngine, run.engine)
        run

    /// The stage-1 run's fitted value of the item's parameter, failing the fact when it fits none.
    let private stageOneValueOf (stageOne : FitRun) (item : Item) (name : ParameterName) : ParameterValue =
        match stageOne.entries |> List.tryFind (fun entry -> entry.parameter = ItemParameterRef (item.id, name)) with
        | Some entry -> entry.fitted
        | None -> failwith $"stage 1 fits no {item.label.value}'s {name.value}"

    /// The context whose items the stage-1 run fits are each at their next revision (`Item.publishRevision`, step 235),
    /// every entry of theirs a calibration of its fitted value from that run.
    let private publishedBy (stageOne : FitRun) (context : StudyForwardContext) : StudyForwardContext =
        let calibrationsOf (item : Item) : Parameter list =
            stageOne.entries
            |> List.choose (fun entry ->
                match entry.parameter with
                | ItemParameterRef (owner, name) when owner = item.id ->
                    let calibration : CalibratedParameterData = { value = entry.fitted; sigma = handOffSigma; fitRun = stageOne.name }
                    Some ({ name = name; state = CalibratedParameter calibration } : Parameter)
                | ItemParameterRef _
                | MaterialParameterRef _ -> None)

        let items =
            context.kit.items
            |> List.map (fun item ->
                match calibrationsOf item with
                | [] -> item
                | published -> revisedWith published item)

        { context with kit = { context.kit with items = items } }

    [<Fact>]
    let ``publishing stage one fixes the instruments' values for stage two`` () : unit =
        let context, study, values = handOffFixture ()
        let reads = ResizeArray<ClockTime>()
        let unpublished = fitParamOver context reads (createMathNetSvd ()) (ResizeArray<UniformDeviate>())

        // Stage 2 first: the instruments' parameters are unknown until stage 1 publishes them.
        match unpublished.runFit (heldRequest study ObjectStage) UnwatchedIterations with
        | Error (FitStageErr (InstrumentNotCalibratedErr _)) -> Assert.Empty(reads)
        | Error error -> Assert.Fail($"expected FitStageErr (InstrumentNotCalibratedErr _), got {error.describe}")
        | Ok run -> Assert.Fail($"a stage 2 over uncalibrated instruments was recorded as {run.name.value}")

        // Stage 1 fits the AIR runs, and its five entries are the instruments' values, exactly.
        let s1 = stageOneRunOf context study
        Assert.Equal(CalibrationStage, s1.stage)
        Assert.Equal<ExperimentName list>([ stageOneLpLp; stageOneLpCpl; stageOneCplLp ] |> List.map (fun run -> run.attachedRun.name), s1.runs)
        Assert.Equal(values.Length, s1.entries.Length)

        for item, name, value in values do
            Assert.Equal<float>(value, (stageOneValueOf s1 item name).value)

        // Each instrument's s1 values published and recorded into the kit, which now holds s1 among its fit runs.
        let published = publishedBy s1 context
        let stageTwo = { fitParamOver published (ResizeArray<ClockTime>()) (createMathNetSvd ()) (ResizeArray<UniformDeviate>()) with fitRuns = [ s1 ] }
        let s2 = fitRunOf (stageTwo.runFit (heldRequest study ObjectStage) UnwatchedIterations)
        Assert.Equal(LinearStudyEngine, s2.engine)
        Assert.Equal(ObjectStage, s2.stage)
        Assert.Equal(CalibrationHeldFixed, s2.calibrationUse)

        Assert.Equal<ExperimentName list>(
            context.kit.experiments |> List.filter (fun run -> run.role = ScienceRun) |> List.map (fun run -> run.name),
            s2.runs)

        // s2 reads each instrument parameter as exactly the calibration s1 published.
        for item, name, _ in values do
            let calibration : CalibratedParameterData = { value = stageOneValueOf s1 item name; sigma = handOffSigma; fitRun = s1.name }
            Assert.Contains(({ parameter = ItemParameterRef (item.id, name); state = CalibratedParameter calibration } : FitRunParameter), s2.parameters)

        // QZ's sixteen are its only entries, each within 1e-9 of M_QZ, in one complete object solve.
        assertLoneFitRun qzAllFree qzTruth coefficientPlaces s2

    [<Fact>]
    let ``as priors, stage two frees the published calibration with its priors for the nonlinear engine`` () : unit =
        let context, study, values = handOffFixture ()
        let s1 = stageOneRunOf context study
        let published = publishedBy s1 context
        let staged, prefits = prefitStaged published study ObjectStage CalibrationAsPrior

        // Every run stage 2 reads is ready, so the engine is chosen by the freed calibrations alone.
        Assert.NotEmpty(prefits)

        for prefit in prefits do
            match prefit.check with
            | Ok _ -> ()
            | Error error -> Assert.Fail($"{prefit.experiment.name.value} is not ready: {error.describe}")

        Assert.Equal(NonlinearStudyEngine, staged.engineWith published prefits)

        let layout = staged.study.parameterLayoutWith { published with kit = { published.kit with items = staged.items } }
        Assert.Equal(coefficientPlaces.Length + values.Length, layout.entries.Length)

        // QZ's sixteen, without a prior.
        let qzEntries = layout.entries |> List.filter (fun entry -> entry.owner = ItemOwner qzAllFree.id)

        Assert.Equal<StudyParameterRef list>(
            coefficientPlaces |> List.map (fun (row, column) -> ItemParameterRef (qzAllFree.id, coefficient row column)),
            qzEntries |> List.map (fun entry -> entry.unknown.parameter))

        Assert.All(qzEntries, fun entry -> Assert.Equal<ParameterPrior option>(None, entry.unknown.unknown.priorOpt))

        // Beside them, each published calibration freed within fifty sigma: from its s1 value, its prior that value.
        for item, name, _ in values do
            let value = stageOneValueOf s1 item name

            match layout.entries |> List.tryFind (fun entry -> entry.unknown.parameter = ItemParameterRef (item.id, name)) with
            | Some entry ->
                let unknown = entry.unknown.unknown
                Assert.Equal<ParameterValue>(value, unknown.start)
                assertValuesWithin 1e-12 [ value.value - 5.0e-5; value.value + 5.0e-5 ] [ unknown.bounds.lower; unknown.bounds.upper ]
                Assert.Equal<ParameterPrior option>(Some { centre = value; sigma = handOffSigma }, unknown.priorOpt)
            | None -> Assert.Fail($"the layout holds no entry of {item.label.value}'s {name.value}")
