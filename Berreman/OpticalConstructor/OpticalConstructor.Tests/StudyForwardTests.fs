namespace OpticalConstructor.Tests

open System
open Xunit
open Berreman.Constants
open Berreman.Fields
open Berreman.Geometry
open Berreman.Media
open Berreman.Solvers
open Analytics.AnalysisFunctions
open OpticalConstructor.Domain
open OpticalConstructor.Domain.CrystalPhysics
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.ItemModels
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.Placement
open OpticalConstructor.Domain.Propagation
open OpticalConstructor.Domain.MeasuredData
open OpticalConstructor.Domain.MuellerReconstruction
open OpticalConstructor.Domain.MuellerInverse
open OpticalConstructor.Domain.Experiments
open OpticalConstructor.Domain.Setups
open OpticalConstructor.Domain.ImportProfiles
open OpticalConstructor.Domain.Studies
open OpticalConstructor.Domain.ExperimentData
open OpticalConstructor.Domain.StudyForward
open CrystalMaterialFixtures

/// Spec 0050 S11.2 — what a detector records, each mapped point placed on the scan its experiment was taken over, and
/// the study's parameter vector laid out by its unknowns' owners.
module StudyForwardTests =

    let private polarizer : Library.ElementId = Library.ElementId "polarizer"
    let private sample : Library.ElementId = Library.ElementId "sample"
    let private dialPlate : Library.ElementId = Library.ElementId "dial-plate"

    /// The polarizer's R1 over 0…90° in 4 points.
    let private polarizerTurn : ScanAxis =
        {
            elementId = polarizer
            label = ElementLabel "Linear polarizer #1"
            variable = VaryR1
            range =
                {
                    min = SweptValue 0.0
                    max = SweptValue 90.0
                    points = CurvePointCount 4
                }
        }

    /// The sample's incidence over 20…60° in 3 points.
    let private sampleIncidence : ScanAxis =
        {
            elementId = sample
            label = ElementLabel "Sample #1"
            variable = VaryR2
            range =
                {
                    min = SweptValue 20.0
                    max = SweptValue 60.0
                    points = CurvePointCount 3
                }
        }

    /// A dial-mounted plate's raw reading over its default turn.
    let private dialTurn : ScanAxis =
        {
            elementId = dialPlate
            label = ElementLabel "Sample #2"
            variable = VaryDialReading
            range = VariableRange.forVariable VaryDialReading
        }

    let private polarizerScan : ExperimentScan = ExperimentScan.oneAxis polarizerTurn CaptureTransmitted None
    let private dialScan : ExperimentScan = ExperimentScan.oneAxis dialTurn CaptureTransmitted None

    /// The polarizer's R1 outer, the sample's R2 inner, in one block.
    let private twoAxisScan : ExperimentScan =
        {
            blocks = [ { axes = [ polarizerTurn; sampleIncidence ]; capture = CaptureTransmitted } ]
            sourceWaveLengthOpt = None
        }

    let private darkLine : ExperimentScan = ExperimentScan.darkLine CaptureTransmitted

    let private settingOn (axis : ScanAxis) (value : SweptValue) : ScanSetting =
        {
            elementId = axis.elementId
            variable = axis.variable
            value = value
        }

    let private intensityAt (x : MeasuredAbscissa) (intensity : MeasuredIntensity) : IntensityPoint = { x = x; intensity = intensity }

    let private captureAt (description : Angle) (avgTotal : MeasuredIntensity) : MuellerRawRow =
        {
            experiment = MuellerExperimentLabel "LP-(AIR#0)-LP"
            captureIndex = CaptureIndex 1
            capturedAt = DateTimeOffset(2026, 11, 13, 12, 0, 0, TimeSpan.Zero)
            description = description
            avgTotal = avgTotal
        }

    let private degreeCell (magnitude : Units.UnitMagnitude) : ScanCell =
        let column : ScanColumn =
            {
                column = DataColumnNumber 1
                valueUnit = DegreeColumnUnit
                reading = PhysicalScanValue
            }

        { column = column; magnitude = magnitude }

    let private columnRow (cells : ScanCell list) (reading : MappedReading) : MappedRow =
        {
            keyOpt = None
            scan = cells
            reading = reading
            timestampOpt = None
            captureIndexOpt = None
            observableOpt = None
        }

    /// Compares two placements, printing both in full on a mismatch.
    let private assertRows (expected : Result<MeasuredRow list, StudyForwardError>) (actual : Result<MeasuredRow list, StudyForwardError>) : unit =
        if expected <> actual then
            Assert.Fail($"expected %A{expected}, got %A{actual}")

    [<Fact>]
    let ``each kind of mapped point is placed on the scan it was taken over`` () : unit =
        // An intensity series under the polarizer's R1: each x is the R1 it was taken at, at no wavelength of its own.
        let series = SeriesPoints (IntensityData { points = [ intensityAt (MeasuredAbscissa 0.0) (MeasuredIntensity 1.0); intensityAt (MeasuredAbscissa 30.0) (MeasuredIntensity 0.75) ] })

        assertRows
            (Ok
                [
                    SettingsRow ([ settingOn polarizerTurn (SweptValue 0.0) ], None, RecordedSignal (MeasuredIntensity 1.0))
                    SettingsRow ([ settingOn polarizerTurn (SweptValue 30.0) ], None, RecordedSignal (MeasuredIntensity 0.75))
                ])
            (series.measuredRowsFor polarizerScan)

        // A Phone ROI capture under a dial reading: its description is the reading, its averaged total the signal.
        let capture = CapturePoints [ captureAt (Angle.degree 20.0<deg>) (MeasuredIntensity 0.5) ]

        assertRows
            (Ok [ SettingsRow ([ settingOn dialTurn (SweptValue 20.0) ], None, RecordedSignal (MeasuredIntensity 0.5)) ])
            (capture.measuredRowsFor dialScan)

        // Mapped rows under two quantities: cell i sets the i-th; a Mueller reading is recorded whole, a signal without its σ.
        let mueller : MuellerMatrix = identityMueller
        let columns =
            ColumnPoints
                [
                    columnRow [ degreeCell (Units.UnitMagnitude 10.0); degreeCell (Units.UnitMagnitude 45.0) ] (MuellerReading mueller)
                    columnRow [ degreeCell (Units.UnitMagnitude 0.0); degreeCell (Units.UnitMagnitude 20.0) ] (SignalReading (MeasuredIntensity 0.25, Some (MeasuredSigma 0.01)))
                ]

        assertRows
            (Ok
                [
                    SettingsRow ([ settingOn polarizerTurn (SweptValue 10.0); settingOn sampleIncidence (SweptValue 45.0) ], None, RecordedMueller mueller)
                    SettingsRow ([ settingOn polarizerTurn (SweptValue 0.0); settingOn sampleIncidence (SweptValue 20.0) ], None, RecordedSignal (MeasuredIntensity 0.25))
                ])
            (columns.measuredRowsFor twoAxisScan)

        // An ellipsometric point sets the sample's R2 to its angle of incidence, read in degrees as `validatedFor` reads
        // it, at its own wavelength; one without an angle of incidence sets nothing.
        let incidence = Angle.degree 60.0<deg>
        let psiDelta : PsiDelta = { psi = Angle.degree 30.0<deg>; delta = Angle.degree 120.0<deg> }
        let otherPsiDelta : PsiDelta = { psi = Angle.degree 40.0<deg>; delta = Angle.degree 90.0<deg> }

        let ellipsometric =
            SeriesPoints (
                EllipsometricData
                    {
                        points =
                            [
                                { waveLength = WaveLength.nm 500.0<nm>; psi = psiDelta.psi; delta = psiDelta.delta; aoiOpt = Some incidence }
                                { waveLength = WaveLength.nm 600.0<nm>; psi = otherPsiDelta.psi; delta = otherPsiDelta.delta; aoiOpt = None }
                            ]
                    })

        assertRows
            (Ok
                [
                    SettingsRow ([ settingOn sampleIncidence (SweptValue incidence.degrees) ], Some (WaveLength.nm 500.0<nm>), RecordedPsiDelta psiDelta)
                    SettingsRow ([], Some (WaveLength.nm 600.0<nm>), RecordedPsiDelta otherPsiDelta)
                ])
            (ellipsometric.measuredRowsFor twoAxisScan)

        Assert.True(abs (incidence.degrees - 60.0) < 1e-12, $"the angle of incidence reads {incidence.degrees}°")

        // Observations carry their own configurations: one row each, in order, whatever the scan.
        let qz = ImportProfileTests.observationOf (DataRowKey "QZ")

        let lr =
            {
                key = DataRowKey "LR"
                observation =
                    {
                        qz.observation with
                            configuration = { qz.observation.configuration with waveLength = WaveLength.nm 633.0<nm> }
                    }
            }

        assertRows
            (Ok [ ObservationRow qz.observation; ObservationRow lr.observation ])
            ((ObservationPoints [ qz; lr ]).measuredRowsFor twoAxisScan)

        // A dark line varies nothing: a series point, a capture row and a column row of no cell have no setting.
        assertRows
            (Ok [ SettingsRow ([], None, RecordedSignal (MeasuredIntensity 0.9)) ])
            ((SeriesPoints (IntensityData { points = [ intensityAt (MeasuredAbscissa 0.0) (MeasuredIntensity 0.9) ] })).measuredRowsFor darkLine)

        assertRows
            (Ok [ SettingsRow ([], None, RecordedSignal (MeasuredIntensity 0.5)) ])
            (capture.measuredRowsFor darkLine)

        assertRows
            (Ok [ SettingsRow ([], None, RecordedSignal (MeasuredIntensity 0.3)) ])
            ((ColumnPoints [ columnRow [] (SignalReading (MeasuredIntensity 0.3, None)) ]).measuredRowsFor darkLine)

    [<Fact>]
    let ``a point the scan cannot place is refused`` () : unit =
        // Under two quantities an X,Y point and a capture row hold one scan value too few.
        assertRows
            (Error (UnpairedPointsErr "point 1 holds one scan value, and the scan varies 2"))
            ((SeriesPoints (IntensityData { points = [ intensityAt (MeasuredAbscissa 30.0) (MeasuredIntensity 0.75) ] })).measuredRowsFor twoAxisScan)

        assertRows
            (Error (UnpairedPointsErr "point 1 holds one scan value, and the scan varies 2"))
            ((CapturePoints [ captureAt (Angle.degree 20.0<deg>) (MeasuredIntensity 0.5) ]).measuredRowsFor twoAxisScan)

        // A column row whose cells do not number the quantities is refused by its own number, the rows before it placed.
        let columns =
            ColumnPoints
                [
                    columnRow [ degreeCell (Units.UnitMagnitude 10.0); degreeCell (Units.UnitMagnitude 45.0) ] (SignalReading (MeasuredIntensity 0.5, None))
                    columnRow [ degreeCell (Units.UnitMagnitude 10.0) ] (SignalReading (MeasuredIntensity 0.5, None))
                ]

        assertRows
            (Error (UnpairedPointsErr "point 2 holds 1 scan value(s), and the scan varies 2"))
            (columns.measuredRowsFor twoAxisScan)

    [<Fact>]
    let ``every forward error renders its condition and its payload`` () : unit =
        let reason = "point 3 holds 1 scan value(s), and the scan varies 2"
        let cases : (StudyForwardError * string) list = [ UnpairedPointsErr reason, $"the data cannot be placed on the scan: {reason}" ]

        for error, expected in cases do
            Assert.Equal<string>(expected, error.describe)

        Assert.Equal(cases.Length, cases |> List.map (fun (error, _) -> error.describe) |> List.distinct |> List.length)

    let private e1 : ExperimentName = ExperimentName "E1"
    let private e2 : ExperimentName = ExperimentName "E2"
    let private e3 : ExperimentName = ExperimentName "E3"
    let private d1 : ExperimentName = ExperimentName "D1"
    let private session1 : SessionName = SessionName "Session 1"
    let private session2 : SessionName = SessionName "Session 2"

    let private retardance : ParameterName = ParameterName "c2.retardance"
    let private ordinaryIndex : ParameterName = ParameterName "n_o"

    /// CPL1's retardance: an unknown from π / 2 within [1, 2].
    let private retardanceData : UnknownParameterData =
        {
            start = ParameterValue (Math.PI / 2.0)
            bounds = { lower = ParameterValue 1.0; upper = ParameterValue 2.0 }
            priorOpt = None
        }

    /// The Phone's gain: an unknown from 1 within [0.1, 10].
    let private gainData : UnknownParameterData =
        {
            start = ParameterValue 1.0
            bounds = { lower = ParameterValue 0.1; upper = ParameterValue 10.0 }
            priorOpt = None
        }

    /// The crystal's ordinary index: an unknown from 1.54 within [1.5, 1.6].
    let private ordinaryIndexData : UnknownParameterData =
        {
            start = ParameterValue 1.54
            bounds = { lower = ParameterValue 1.5; upper = ParameterValue 1.6 }
            priorOpt = None
        }

    /// The test-local uniaxial crystal version QZ's plate is made of.
    let private crystalVersion : MaterialVersionId =
        MaterialVersionId.firstOf (MaterialId (Guid.Parse "37200000-0000-4000-8000-000000000001"))

    let private crystalEntry : MaterialEntry =
        {
            id = crystalVersion.materialId
            name = EntryName "Probe uniaxial crystal"
            category = CategoryIds.crystal
            description = None
            physics = CrystalParameterPhysics (UniaxialCrystal uniaxialParameters)
        }

    let private qzPlate : Sample =
        {
            id = SampleId (Guid.Parse "37200000-0000-4000-8000-000000000002")
            name = EntryName "QZ plate"
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

    // Every model is a shipped entry of step 210.
    let private cpl1Draft : ItemDraft =
        {
            id = ItemId (Guid.Parse "37200000-0000-4000-8000-000000000011")
            label = ItemLabel "CPL1"
            model = LibraryModelChoice (BuiltinCatalogue.itemModel ItemModelIds.circularPolarizerModelId)
            dial = FixedMounting
        }

    let private phoneDraft : ItemDraft =
        {
            id = ItemId (Guid.Parse "37200000-0000-4000-8000-000000000012")
            label = ItemLabel "Phone"
            model = LibraryModelChoice (BuiltinCatalogue.itemModel ItemModelIds.intensityDetectorModelId)
            dial = FixedMounting
        }

    let private qzDraft : ItemDraft =
        {
            id = ItemId (Guid.Parse "37200000-0000-4000-8000-000000000013")
            label = ItemLabel "QZ"
            model = StackSampleChoice (BuiltinCatalogue.itemModel ItemModelIds.stackSampleItemModelId, SampleVersionId.firstOf qzPlate.id, qzPlate)
            dial = FixedMounting
        }

    /// The draft's new item with one parameter freed, failing with the refusal's text.
    let private freeing (name : ParameterName) (unknown : UnknownParameterData) (draft : ItemDraft) : Item =
        match draft.toItem.withParameterState name (UnknownParameter unknown) with
        | Ok item -> item
        | Error error -> failwith $"'{draft.label.value}' refused {name.value}: {error.describe}"

    let private cpl1 : Item = freeing retardance retardanceData cpl1Draft
    let private phone : Item = freeing ParameterName.gain gainData phoneDraft
    let private qz : Item = qzDraft.toItem

    /// Each item's model as its draft chose it.
    let private choiceOf (item : Item) : ItemModelChoice option =
        [ cpl1Draft; phoneDraft; qzDraft ] |> List.tryFind (fun draft -> draft.id = item.id) |> Option.map (fun draft -> draft.model)

    let private emptySlot (id : Library.ElementId) (kind : CatalogueKind) (point : TablePoint) : SetupSlot =
        {
            id = id
            placement = ElementPlacement.create kind point
            zoom = BeamLayout.ElementZoom 5.0
            beam = BeamLayout.OnMainBeam
            content = EmptySlotContent
        }

    let private holding (item : Item) (slot : SetupSlot) : SetupSlot = { slot with content = ItemInSlot (item.id, PlacementTurn) }

    let private sourceSlot : SetupSlot = emptySlot (Library.ElementId "src") LightSource RayModel.defaultSourcePoint
    let private cplSlot : SetupSlot = emptySlot (Library.ElementId "cpl") CircularPolarizer { x = -0.3<meter>; y = 0.0<meter> }
    let private qzSlot : SetupSlot = emptySlot (Library.ElementId "qz") Sample { x = 0.1<meter>; y = 0.0<meter> }
    let private detectorSlot : SetupSlot = emptySlot (Library.ElementId "det") Detector RayModel.defaultDetectorPoint

    let private setupOf (name : SetupName) (slots : SetupSlot list) : Setup =
        match Setup.create name slots [] with
        | Ok setup -> setup
        | Error error -> failwith $"'{name.value}' was refused: {error.describe}"

    /// CPL1, QZ and the Phone in their slots.
    let private bench : Setup = setupOf (SetupName "Bench") [ sourceSlot; holding cpl1 cplSlot; holding qz qzSlot; holding phone detectorSlot ]

    /// E1 over the bench: a Science run of `Session 1` turning CPL1.
    let private e1Run : Experiment =
        {
            id = ExperimentId 1
            name = e1
            setup = bench.reference
            scan =
                ExperimentScan.oneAxis
                    {
                        elementId = cplSlot.id
                        label = ElementLabel "CPL1"
                        variable = VaryR1
                        range = VariableRange.forVariable VaryR1
                    }
                    CaptureTransmitted
                    None
            role = ScienceRun
            sessionOpt = Some session1
            notes = ExperimentNotes.none
            dataOpt = None
            corrections = []
        }

    let private e2Run : Experiment = { e1Run with id = ExperimentId 2; name = e2 }
    let private d1Run : Experiment = { e1Run with id = ExperimentId 3; name = d1; role = DiagnosticRun; sessionOpt = None }
    let private e3Run : Experiment = { e1Run with id = ExperimentId 4; name = e3; sessionOpt = None }

    /// E1, E2 (Science, `Session 1`), D1 (Diagnostic) and E3 (Science), the kit's experiments in its order.
    let private kitRuns : Experiment list = [ e1Run; e2Run; d1Run; e3Run ]

    let private studyOf (result : Result<Study, StudyError>) : Study =
        match result with
        | Ok study -> study
        | Error error -> failwith $"expected a study, got {error.describe}"

    /// QZ over the kit's four runs with the crystal opened and its `n_o` freed; nothing scoped or linked.
    let private layoutStudy : Study =
        let created = studyOf (Study.create (StudyName "QZ") [ e1; e2; d1; e3 ] [] kitRuns)
        let opened = studyOf (created.openMaterial crystalVersion crystalEntry)
        studyOf (opened.withMaterialParameterState crystalVersion ordinaryIndex (UnknownParameter ordinaryIndexData))

    /// A data seam the layout never reads.
    let private noData : ExperimentDataProxy =
        {
            readPickedFile = fun file -> Error (KitIoErr $"the test reads no picked file '{file.value}'")
            attachDataFile = fun file -> Error (KitIoErr $"the test attaches no file '{file.value}'")
            attachmentStatus = fun attachment -> Error (KitIoErr $"the test reads no status of '{attachment.path.value}'")
            readMapped = fun data -> Error (MalformedDataFileErr $"the test holds no file '{data.attachment.path.value}'")
        }

    /// The study's forward context over a kit of these runs and setups, the three items, the Built-in library and
    /// materials, and the default solver parameters.
    let private forwardFor (study : Study) (runs : Experiment list) (setups : Setup list) : StudyForwardContext =
        let kit : StudyContext =
            {
                studies = [ study ]
                experiments = runs
                setups = setups
                items = [ cpl1; qz; phone ]
                resolveMaterial =
                    fun version ->
                        if version = crystalVersion then Ok crystalEntry
                        else Error (MaterialUnavailableErr $"material {version.materialId.value} is not the test's crystal")
            }

        {
            kit = kit
            library = BuiltinCatalogue.library ()
            materials = BuiltinCatalogue.materialStore (fun _ -> [])
            experimentData = noData
            choiceOf = choiceOf
            solverParameters = SolverParameters.defaultValue
        }

    let private entryOf (unknown : StudyUnknown) (owner : UnknownOwner) : StudyVectorEntry = { unknown = unknown; owner = owner }

    /// The owners the layout holds a value of the parameter for, in its order.
    let private ownersIn (layout : StudyParameterLayout) (parameter : StudyParameterRef) : UnknownOwner list =
        layout.entries |> List.filter (fun entry -> entry.unknown.parameter = parameter) |> List.map (fun entry -> entry.owner)

    let private cpl1Retardance : StudyUnknown = { parameter = ItemParameterRef (cpl1.id, retardance); scope = ItemScope; unknown = retardanceData }
    let private phoneGain : StudyUnknown = { parameter = ItemParameterRef (phone.id, ParameterName.gain); scope = SessionBlockScope; unknown = gainData }
    let private crystalOrdinary : StudyUnknown = { parameter = MaterialParameterRef (crystalVersion, ordinaryIndex); scope = MaterialScope; unknown = ordinaryIndexData }

    /// The study with CPL1's retardance shared per experiment.
    let private perExperimentStudy : Study =
        let used = (forwardFor layoutStudy kitRuns [ bench ]).kit.itemsUsedBy layoutStudy
        studyOf (layoutStudy.withParameterScope { parameter = cpl1Retardance.parameter; scope = ExperimentScope } used)

    [<Fact>]
    let ``the vector holds one value per owner of each unknown, in the unknowns' order, and starts at each unknown's start`` () : unit =
        let layout = layoutStudy.parameterLayoutWith (forwardFor layoutStudy kitRuns [ bench ])

        // CPL1's retardance is per item, the Phone's gain per session holding a fitted run, n_o per material version.
        Assert.Equal<StudyVectorEntry list>(
            [
                entryOf cpl1Retardance (ItemOwner cpl1.id)
                entryOf phoneGain (SessionOwner session1)
                entryOf phoneGain (SessionOwner session2)
                entryOf crystalOrdinary (MaterialOwner crystalVersion)
            ],
            layout.entries)

        // D1 splits the runs into sessions and joins none.
        Assert.Equal<StudySession list>(
            [ ({ name = session1; experiments = [ e1; e2 ] } : StudySession); ({ name = session2; experiments = [ e3 ] } : StudySession) ],
            layout.sessions)

        Assert.Equal<float[]>([| Math.PI / 2.0; 1.0; 1.0; 1.54 |], layout.startVector.value)
        Assert.Equal(ParameterValue 1.54, layout.startVector.valueAt (StudyVectorIndex 3))
        Assert.Equal(layout.startVector, StudyParameterVector.ofParameterValues (layout.entries |> List.map (fun entry -> entry.unknown.unknown.start)))

        // Per experiment, the retardance has one value per fitted run, in study order, ahead of the unknowns after it.
        let perExperiment = perExperimentStudy.parameterLayoutWith (forwardFor perExperimentStudy kitRuns [ bench ])
        let experimentRetardance = { cpl1Retardance with scope = ExperimentScope }

        Assert.Equal<StudyVectorEntry list>(
            [
                entryOf experimentRetardance (ExperimentOwner e1)
                entryOf experimentRetardance (ExperimentOwner e2)
                entryOf experimentRetardance (ExperimentOwner e3)
                entryOf phoneGain (SessionOwner session1)
                entryOf phoneGain (SessionOwner session2)
                entryOf crystalOrdinary (MaterialOwner crystalVersion)
            ],
            perExperiment.entries)

        Assert.Equal<float[]>([| Math.PI / 2.0; Math.PI / 2.0; Math.PI / 2.0; 1.0; 1.0; 1.54 |], perExperiment.startVector.value)

    [<Fact>]
    let ``a disabled run, a run that is not fitted and a run whose setup does not use an unknown own none of its values`` () : unit =
        // Science, Calibration and Drift-check runs give residual rows, a Dark run its session's dark level; the rest are not read.
        Assert.Equal<StudyRunUse list>(
            [ ResidualRunUse; ResidualRunUse; DarkLevelRunUse; ResidualRunUse; UnreadRunUse; UnreadRunUse; UnreadRunUse ],
            ExperimentRole.all |> List.map (fun role -> role.studyUse))

        let retardanceRef = cpl1Retardance.parameter
        let namesOf (runs : Experiment list) : ExperimentName list = runs |> List.map (fun run -> run.name)

        // The Diagnostic D1 is not fitted.
        Assert.Equal<ExperimentName list>([ e1; e2; e3 ], namesOf (perExperimentStudy.fittedRunsIn kitRuns))

        // E2 disabled: the experiment-scoped retardance has the owners E1 and E3.
        let withoutE2 = studyOf (perExperimentStudy.withExperimentEnablement e2 DisabledInStudy)
        Assert.Equal<ExperimentName list>([ e1; e3 ], namesOf (withoutE2.fittedRunsIn kitRuns))

        Assert.Equal<UnknownOwner list>(
            [ ExperimentOwner e1; ExperimentOwner e3 ],
            ownersIn (withoutE2.parameterLayoutWith (forwardFor withoutE2 kitRuns [ bench ])) retardanceRef)

        // E1 made a Dark run owns none of them.
        let darkE1 = [ { e1Run with role = DarkRun }; e2Run; d1Run; e3Run ]
        Assert.Equal<ExperimentName list>([ e2; e3 ], namesOf (perExperimentStudy.fittedRunsIn darkE1))

        Assert.Equal<UnknownOwner list>(
            [ ExperimentOwner e2; ExperimentOwner e3 ],
            ownersIn (perExperimentStudy.parameterLayoutWith (forwardFor perExperimentStudy darkE1 [ bench ])) retardanceRef)

        // E3 over a setup holding no QZ is no user of n_o, which keeps its one material owner through E1 and E2.
        let noQzBench = setupOf (SetupName "Bench without QZ") [ sourceSlot; holding cpl1 cplSlot; qzSlot; holding phone detectorSlot ]
        let noQzRuns = [ e1Run; e2Run; d1Run; { e3Run with setup = noQzBench.reference } ]
        let noQz = perExperimentStudy.parameterLayoutWith (forwardFor perExperimentStudy noQzRuns [ bench; noQzBench ])

        Assert.Equal<UnknownOwner list>([ MaterialOwner crystalVersion ], ownersIn noQz crystalOrdinary.parameter)
        Assert.Equal<UnknownOwner list>([ ExperimentOwner e1; ExperimentOwner e2; ExperimentOwner e3 ], ownersIn noQz retardanceRef)

        // Over a setup holding no CPL1, E3 owns no value of the retardance; over one the kit lacks it uses nothing, so the
        // Phone's gain loses `Session 2`, which E3 alone forms.
        let noCplBench = setupOf (SetupName "Bench without CPL1") [ sourceSlot; cplSlot; holding qz qzSlot; holding phone detectorSlot ]
        let noCplRuns = [ e1Run; e2Run; d1Run; { e3Run with setup = noCplBench.reference } ]
        let noCpl = perExperimentStudy.parameterLayoutWith (forwardFor perExperimentStudy noCplRuns [ bench; noCplBench ])

        Assert.Equal<UnknownOwner list>([ ExperimentOwner e1; ExperimentOwner e2 ], ownersIn noCpl retardanceRef)
        Assert.Equal<UnknownOwner list>([ SessionOwner session1; SessionOwner session2 ], ownersIn noCpl phoneGain.parameter)

        let goneRuns = [ e1Run; e2Run; d1Run; { e3Run with setup = { name = SetupName "Gone"; revision = SetupRevision.first } } ]
        let gone = perExperimentStudy.parameterLayoutWith (forwardFor perExperimentStudy goneRuns [ bench ])

        Assert.Equal<UnknownOwner list>([ ExperimentOwner e1; ExperimentOwner e2 ], ownersIn gone retardanceRef)
        Assert.Equal<UnknownOwner list>([ SessionOwner session1 ], ownersIn gone phoneGain.parameter)
        Assert.Equal<UnknownOwner list>([ MaterialOwner crystalVersion ], ownersIn gone crystalOrdinary.parameter)
