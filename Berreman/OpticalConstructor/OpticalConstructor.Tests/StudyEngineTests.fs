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

    let private drafts : ItemDraft list =
        [
            laserDraft; lp1Draft; lp2Draft; cpl1Draft; cpl2Draft; phoneDraft; qzDraft; lrDraft; qz2Draft
            cpLeftDraft; cpRightDraft; constantDraft; plateDraft; polarimeterDraft; crystalPlateDraft; indexPlateDraft
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

    /// The item at its next revision, each named parameter published as a calibration of that value.
    let private publishing (values : (ParameterName * float) list) (item : Item) : Item =
        let published : Parameter list = values |> List.map (fun (name, value) -> ({ name = name; state = calibratedAt value } : Parameter))

        match item.publishRevision published with
        | Ok revised -> revised
        | Error error -> failwith $"'{item.label.value}' refused its calibration: {error.describe}"

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

    /// The HeNe, the prep, the object slots in light order (an empty one holds nothing), the analyzer and the detector.
    let private benchOf (name : string) (prep : Item) (objects : Item option list) (analyzer : Item) (detector : Item) : Setup =
        let objectSlots =
            objects
            |> List.mapi (fun k objectOpt ->
                let slot = emptySlot $"object{k + 1}" Sample (alongBeam (-0.2<meter> + float k * 0.1<meter>))

                match objectOpt with
                | Some item -> holding item slot
                | None -> slot)

        setupOf
            name
            ([ emptySlot "src" LightSource RayModel.defaultSourcePoint |> holding laser; opticSlot "prep" (-0.4<meter>) prep ]
             @ objectSlots
             @ [ opticSlot "analyzer" 0.4<meter> analyzer; emptySlot "det" Detector RayModel.defaultDetectorPoint |> holding detector ])

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

    /// The Phone's capture rows over the analyzer's dial: 1000 · (designRow s a · M.vecColumnMajor) + 5, the source's state
    /// through `matrix` onto the analyzer's row at each reading.
    let private capturesOf (label : string) (source : SourceModel) (analyzer : AnalyzerModel) (matrix : MuellerMatrix) : MuellerRawRow list =
        let m = matrix.vecColumnMajor

        dialReadings
        |> List.mapi (fun k reading ->
            let (DesignRow row) = designRow source.baseStokes (analyzerBaseRow analyzer reading)
            let predicted = [ 0 .. 15 ] |> List.sumBy (fun i -> row.[i] * m.[i])

            ({
                experiment = MuellerExperimentLabel label
                captureIndex = CaptureIndex (k + 1)
                capturedAt = DateTimeOffset(2026, 11, 13, 10, 0, 0, TimeSpan.Zero).AddSeconds(float k)
                description = reading
                avgTotal = MeasuredIntensity (1000.0 * predicted + 5.0)
            } : MuellerRawRow))

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

    /// The forward context over a kit of these items, every bench and the runs holding the study, the Built-in library and
    /// materials, and the runs' files.
    let private contextOver (items : Item list) (runs : AttachedRun list) (study : Study) : StudyForwardContext =
        let kit : StudyContext =
            {
                studies = [ study ]
                experiments = runs |> List.map (fun attached -> attached.attachedRun)
                setups = benches
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

    /// The engine `engineWith` chooses for the study staged with its calibrations held, once every run its pre-fit reads is
    /// ready and its layout holds an entry, so no answer comes from an earlier refusal.
    let private engineOf (context : StudyForwardContext) (study : Study) (stage : FitStage) : StudyEngineKind =
        let staged =
            match study.stageWith context.kit stage CalibrationHeldFixed with
            | Ok staged -> staged
            | Error error -> failwith $"'{study.name.value}' was not staged for {stage.text}: {error.describe}"

        let stagedContext = { context with kit = { context.kit with items = staged.items } }

        let prefits =
            match staged.study.prefitWith stagedContext [] with
            | Ok prefits -> prefits
            | Error error -> failwith $"'{study.name.value}' has no pre-fit: {error.describe}"

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

    [<Fact>]
    let ``every study-fit error renders its condition and its payload`` () : unit =
        let forwardError = MissingStudyRunErr "E9, which the study 'QZ' names"
        let stageError = NoStageRunErr "'QZ' has no enabled run that Stage 2 (objects) fits"
        let gainReason = "'Session 1' anchors its gain to A1"
        let failuresReason = "member 1: the solver did not converge: BackendFailed"
        let cancelReason = "1 of 3 member(s) ran, the last one's result dropped"

        let cases : (StudyFitError * string) list =
            [
                StudyForwardFitErr forwardError, $"the study's model or data refused the fit: {forwardError.describe}"
                FitStageErr stageError, $"the stage refused the fit: {stageError.describe}"
                SessionGainNeedsLinearEngineErr gainReason, $"the nonlinear engine fits a session's gain as its detector's parameter: {gainReason}"
                NoConvergedMemberErr failuresReason, $"no start of the fit converged: {failuresReason}"
                FitCancelledErr cancelReason, $"the fit was cancelled: {cancelReason}"
            ]

        for error, expected in cases do
            Assert.Equal<string>(expected, error.describe)

        Assert.Contains("E9, which the study 'QZ' names", (StudyForwardFitErr forwardError).describe)
        Assert.Contains("'QZ' has no enabled run that Stage 2 (objects) fits", (FitStageErr stageError).describe)
        Assert.Contains(gainReason, (SessionGainNeedsLinearEngineErr gainReason).describe)
        Assert.Contains(failuresReason, (NoConvergedMemberErr failuresReason).describe)
        Assert.Contains(cancelReason, (FitCancelledErr cancelReason).describe)
        Assert.Equal(cases.Length, cases |> List.map (fun (error, _) -> error.describe) |> List.distinct |> List.length)
