namespace OpticalConstructor.Tests

open System
open Xunit
open Berreman.Constants
open Berreman.Fields
open Berreman.Geometry
open Berreman.Media
open Berreman.MaterialProperties
open Berreman.Solvers
open Analytics.AnalysisFunctions
open OpticalConstructor.Domain
open OpticalConstructor.Domain.CrystalPhysics
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.ItemModels
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.ExperimentRun
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
open OpticalConstructor.Domain.ItemEvaluation
open OpticalConstructor.Domain.PrefitChecks
open OpticalConstructor.Domain.StudyForward
open CrystalMaterialFixtures

/// Spec 0050 S11.2 — what a detector records, each mapped point placed on the scan its experiment was taken over, the
/// study's parameter vector laid out by its unknowns' owners, the numbers one experiment is evaluated with, and the
/// reading its detector item predicts at a scan point.
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
        let lengthReason = "it holds 3 value(s), and the layout has 4 entry(ies)"
        let setupError = UnknownSetupErr "'Missing' revision 1"
        let slotError = SlotItemErr (UnknownSlotItemErr "the kit holds no item LP3")
        let detectorReason = "'Bench' revision 1, which E1 references"
        let plateReason = "'Crystal' revision 1, which F1 references"
        let mismatchReason = "F1 is read by an intensity detector, which records no Mueller matrix"
        let rotationReason = "the polarization block has trace 5, which no rotation can produce"
        let counts : ObservationCounts = { measuredCount = ObservationCount 3; modelCount = ObservationCount 2 }
        let missingReason = "E9, which the study 'QZ' names"
        let kitError = KitIoErr "the test cannot hash 'data/E1.csv'"
        let prefitError = MissingDataErr "no measured-data file is attached to this experiment"
        let darkReason = "K1 records Ψ and Δ, not a signal"

        let cases : (StudyForwardError * string) list =
            [
                UnpairedPointsErr reason, $"the data cannot be placed on the scan: {reason}"
                VectorLengthErr lengthReason, $"the parameter vector does not fit the study: {lengthReason}"
                ForwardSetupErr setupError, $"the experiment's setup does not resolve: {setupError.describe}"
                ForwardSlotErr slotError, $"a slot cannot be evaluated: {slotError.describe}"
                StudyDetectorErr detectorReason, $"the setup has no detector item: {detectorReason}"
                ObservationPlateErr plateReason, $"the observation has no single plate of one opened crystal: {plateReason}"
                ReadingMismatchErr mismatchReason, $"the reading is not what the detector records: {mismatchReason}"
                RowResidualErr (NonPositiveIntensityErr (-0.25)), $"the residual cannot be formed: m₀₀ is {-0.25}, not a positive finite throughput"
                RowResidualErr (DegenerateRotationErr rotationReason), $"the residual cannot be formed: {rotationReason}"
                RowResidualErr (MismatchedObservationsErr counts), "the residual cannot be formed: 3 measured and 2 model observation(s) do not pair"
                MissingStudyRunErr missingReason, $"the kit has no such experiment: {missingReason}"
                StudyRunKitErr kitError, $"the kit cannot report the data's status: {kitError.describe}"
                PrefitFailedErr (ExperimentName "E2", prefitError), $"the experiment is not ready for a fit: E2: {prefitError.describe}"
                DarkReadingErr darkReason, $"a dark run does not record a signal: {darkReason}"
            ]

        // A wrapped error keeps its own payload in the text.
        Assert.Contains("'Missing' revision 1", (ForwardSetupErr setupError).describe)
        Assert.Contains("the kit holds no item LP3", (ForwardSlotErr slotError).describe)
        Assert.Contains("the test cannot hash 'data/E1.csv'", (StudyRunKitErr kitError).describe)
        Assert.Contains("no measured-data file is attached to this experiment", (PrefitFailedErr (ExperimentName "E2", prefitError)).describe)

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

    /// A second circular polarizer, outside step 372's bench.
    let private cpl2Draft : ItemDraft =
        {
            cpl1Draft with
                id = ItemId (Guid.Parse "37300000-0000-4000-8000-000000000011")
                label = ItemLabel "CPL2"
        }

    /// The draft's new item with one parameter in that state, failing with the refusal's text.
    let private stating (name : ParameterName) (state : ParameterState) (draft : ItemDraft) : Item =
        match draft.toItem.withParameterState name state with
        | Ok item -> item
        | Error error -> failwith $"'{draft.label.value}' refused {name.value}: {error.describe}"

    let private cpl1 : Item = stating retardance (UnknownParameter retardanceData) cpl1Draft
    let private phone : Item = stating ParameterName.gain (UnknownParameter gainData) phoneDraft
    let private qz : Item = qzDraft.toItem

    /// CPL2 with its retardance known at 1.2.
    let private cpl2 : Item = stating retardance (KnownParameter (ParameterValue 1.2)) cpl2Draft

    /// Each item's model as its draft chose it.
    let private choiceOf (item : Item) : ItemModelChoice option =
        [ cpl1Draft; phoneDraft; qzDraft; cpl2Draft ] |> List.tryFind (fun draft -> draft.id = item.id) |> Option.map (fun draft -> draft.model)

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

    /// The study's forward context over a kit of these items, runs and setups, the Built-in library and materials, and
    /// the default solver parameters.
    let private forwardOver (items : Item list) (study : Study) (runs : Experiment list) (setups : Setup list) : StudyForwardContext =
        let kit : StudyContext =
            {
                studies = [ study ]
                experiments = runs
                setups = setups
                items = items
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

    /// The forward context over CPL1, QZ and the Phone.
    let private forwardFor (study : Study) (runs : Experiment list) (setups : Setup list) : StudyForwardContext =
        forwardOver [ cpl1; qz; phone ] study runs setups

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

    /// The QZ study's layout over the bench: CPL1's retardance, the Phone's gain in `Session 1` and `Session 2`, n_o.
    let private qzLayout : StudyParameterLayout = layoutStudy.parameterLayoutWith (forwardFor layoutStudy kitRuns [ bench ])

    /// A vector of that layout: the retardance 0.7, the gain 2 in `Session 1` and 3 in `Session 2`, n_o 1.6.
    let private qzVector : StudyParameterVector = StudyParameterVector [| 0.7; 2.0; 3.0; 1.6 |]

    /// The probe crystal with its n_o at the vector's 1.6 and its other three components the copy's.
    let private crystalAtVector : CrystalParameters = UniaxialCrystal { uniaxialParameters with ordinaryIndex = RefractionIndex 1.6 }

    let private assignmentOf (result : Result<ExperimentAssignment, StudyForwardError>) : ExperimentAssignment =
        match result with
        | Ok assignment -> assignment
        | Error error -> failwith $"expected an assignment, got {error.describe}"

    /// The study's working copy of that version, failing when the study has not opened it.
    let private openedOf (study : Study) (version : MaterialVersionId) : OpenedMaterial =
        match study.materials |> List.tryFind (fun material -> material.version = version) with
        | Some material -> material
        | None -> failwith $"'{study.name.value}' opens no copy of material {version.materialId.value}"

    let private ownValuesOf (item : Item) : ParameterValues = ParameterValues.ofParameters item.parameters

    /// A second version of the probe crystal, opened beside it with every component known.
    let private twinVersion : MaterialVersionId =
        MaterialVersionId.firstOf (MaterialId (Guid.Parse "37300000-0000-4000-8000-000000000001"))

    let private twinEntry : MaterialEntry = { crystalEntry with id = twinVersion.materialId; name = EntryName "Probe uniaxial twin" }

    [<Fact>]
    let ``an experiment's items take the vector's value for the owner that holds it, and every other parameter its own`` () : unit =
        let items = [ cpl1; qz; phone ]
        let atE1 = assignmentOf (qzLayout.assignmentFor layoutStudy items e1 qzVector)
        let atE3 = assignmentOf (qzLayout.assignmentFor layoutStudy items e3 qzVector)

        // CPL1's retardance has one value for every run; the Phone's gain is its session's, its dark offset its own 0.
        Assert.Equal<ParameterValues>((ownValuesOf cpl1).withValue retardance (ParameterValue 0.7), atE1.valuesOf cpl1)
        Assert.Equal<ParameterValues>((ownValuesOf phone).withValue ParameterName.gain (ParameterValue 2.0), atE1.valuesOf phone)
        Assert.Equal<Result<ParameterValue, ItemEvaluationError>>(Ok (ParameterValue 0.0), (atE1.valuesOf phone).valueOf ParameterName.darkOffset)
        Assert.Equal<ParameterValues>(ownValuesOf qz, atE1.valuesOf qz)

        Assert.Equal<ParameterValues>((ownValuesOf cpl1).withValue retardance (ParameterValue 0.7), atE3.valuesOf cpl1)
        Assert.Equal<ParameterValues>((ownValuesOf phone).withValue ParameterName.gain (ParameterValue 3.0), atE3.valuesOf phone)
        Assert.Equal<ParameterValues>(ownValuesOf qz, atE3.valuesOf qz)

        // D1 is in no session, so the Phone keeps its own gain, the unknown's start.
        let atD1 = assignmentOf (qzLayout.assignmentFor layoutStudy items d1 qzVector)
        Assert.Equal<ParameterValues>(ownValuesOf phone, atD1.valuesOf phone)
        Assert.Equal<Result<ParameterValue, ItemEvaluationError>>(Ok (ParameterValue 1.0), (atD1.valuesOf phone).valueOf ParameterName.gain)

        // An item the assignment was not given takes its own values.
        let withoutQz = assignmentOf (qzLayout.assignmentFor layoutStudy [ cpl1; phone ] e1 qzVector)
        Assert.Equal<ParameterValues>(ownValuesOf qz, withoutQz.valuesOf qz)

        // Per experiment, each run takes its own value of the retardance.
        let perExperiment = perExperimentStudy.parameterLayoutWith (forwardFor perExperimentStudy kitRuns [ bench ])
        let perExperimentVector = StudyParameterVector [| 0.5; 0.6; 0.7; 2.0; 3.0; 1.6 |]
        let atE2 = assignmentOf (perExperiment.assignmentFor perExperimentStudy items e2 perExperimentVector)
        let atE3PerExperiment = assignmentOf (perExperiment.assignmentFor perExperimentStudy items e3 perExperimentVector)

        Assert.Equal<ParameterValues>((ownValuesOf cpl1).withValue retardance (ParameterValue 0.6), atE2.valuesOf cpl1)
        Assert.Equal<ParameterValues>((ownValuesOf phone).withValue ParameterName.gain (ParameterValue 2.0), atE2.valuesOf phone)
        Assert.Equal<ParameterValues>((ownValuesOf cpl1).withValue retardance (ParameterValue 0.7), atE3PerExperiment.valuesOf cpl1)
        Assert.Equal<ParameterValues>((ownValuesOf phone).withValue ParameterName.gain (ParameterValue 3.0), atE3PerExperiment.valuesOf phone)

    [<Fact>]
    let ``an opened material's crystal takes its components from the vector, and a link's follower takes its leader's value`` () : unit =
        let atE1 = assignmentOf (qzLayout.assignmentFor layoutStudy [ cpl1; qz; phone ] e1 qzVector)
        let copy = openedOf layoutStudy crystalVersion

        // The copy's n_o is the vector's; its n_e, g11 and g33 are the copy's own.
        Assert.Equal<CrystalParameters>(crystalAtVector, atE1.crystalOf copy)
        Assert.Equal<Map<MaterialVersionId, CrystalParameters>>(Map.ofList [ crystalVersion, crystalAtVector ], atE1.crystals)

        // A copy the assignment does not hold is its current crystal: n_o at its unknown's start.
        let empty : ExperimentAssignment = { itemValues = Map.empty; crystals = Map.empty }
        Assert.Equal<CrystalParameters>(UniaxialCrystal { uniaxialParameters with ordinaryIndex = RefractionIndex 1.54 }, empty.crystalOf copy)

        // CPL2 (retardance known at 1.2) on the bench and the twin version opened; then CPL2 follows CPL1 and the
        // twin's n_o the crystal's.
        let cpl2Slot = emptySlot (Library.ElementId "cpl2") CircularPolarizer { x = -0.1<meter>; y = 0.0<meter> }

        let cpl2Bench =
            setupOf (SetupName "Bench with CPL2") [ sourceSlot; holding cpl1 cplSlot; holding cpl2 cpl2Slot; holding qz qzSlot; holding phone detectorSlot ]

        let cpl2Runs = kitRuns |> List.map (fun run -> { run with setup = cpl2Bench.reference })
        let cpl2Items = [ cpl1; qz; phone; cpl2 ]
        let twinStudy = studyOf (layoutStudy.openMaterial twinVersion twinEntry)
        let unlinkedContext = forwardOver cpl2Items twinStudy cpl2Runs [ cpl2Bench ]
        let used = unlinkedContext.kit.itemsUsedBy twinStudy

        let cplLink : ParameterLink = { follower = ItemParameterRef (cpl2.id, retardance); leader = ItemParameterRef (cpl1.id, retardance) }

        let twinLink : ParameterLink =
            {
                follower = MaterialParameterRef (twinVersion, ordinaryIndex)
                leader = MaterialParameterRef (crystalVersion, ordinaryIndex)
            }

        let linkedStudy = studyOf (twinStudy.withLink cplLink used |> Result.bind (fun linked -> linked.withLink twinLink used))
        let unlinkedLayout = twinStudy.parameterLayoutWith unlinkedContext
        let linkedLayout = linkedStudy.parameterLayoutWith (forwardOver cpl2Items linkedStudy cpl2Runs [ cpl2Bench ])

        // Neither follower is an unknown, so the vector is the QZ study's.
        Assert.Equal<StudyVectorEntry list>(qzLayout.entries, unlinkedLayout.entries)
        Assert.Equal<StudyVectorEntry list>(qzLayout.entries, linkedLayout.entries)

        let before = assignmentOf (unlinkedLayout.assignmentFor twinStudy cpl2Items e1 qzVector)
        let after = assignmentOf (linkedLayout.assignmentFor linkedStudy cpl2Items e1 qzVector)
        let twin = openedOf linkedStudy twinVersion

        Assert.Equal<Result<ParameterValue, ItemEvaluationError>>(Ok (ParameterValue 1.2), (before.valuesOf cpl2).valueOf retardance)
        Assert.Equal<ParameterValues>((ownValuesOf cpl2).withValue retardance (ParameterValue 0.7), after.valuesOf cpl2)
        Assert.Equal<CrystalParameters>(UniaxialCrystal uniaxialParameters, before.crystalOf twin)
        Assert.Equal<CrystalParameters>(crystalAtVector, after.crystalOf twin)

        // The leaders keep their values.
        Assert.Equal<ParameterValues>(before.valuesOf cpl1, after.valuesOf cpl1)
        Assert.Equal<CrystalParameters>(crystalAtVector, after.crystalOf copy)

    [<Fact>]
    let ``a stack sample made of an opened material resolves to the study's working copy`` () : unit =
        let store = BuiltinCatalogue.materialStore (fun _ -> [])
        Assert.Equal<Result<unit, MaterialError>>(Ok (), store.saveMaterial crystalEntry)

        let atE1 = assignmentOf (qzLayout.assignmentFor layoutStudy [ cpl1; qz; phone ] e1 qzVector)
        let over = atE1.materialProxyOver store
        let e1Crystal = atE1.crystalOf (openedOf layoutStudy crystalVersion)

        // QZ's plate pins the opened version, which resolves to the store's entry over E1's crystal.
        let plateVersion =
            match qzPlate.structure.substrate with
            | Some layer -> layer.materialId
            | None -> failwith $"'{qzPlate.name.value}' has no substrate"

        Assert.Equal(crystalVersion, plateVersion)
        Assert.Equal<CrystalParameters>(crystalAtVector, e1Crystal)

        Assert.Equal<Result<MaterialEntry option, MaterialError>>(
            Ok (Some { crystalEntry with physics = CrystalParameterPhysics e1Crystal }),
            over.resolveVersion plateVersion)

        // Another version, and every other read, is the store's own answer.
        let glass = MaterialVersionId.firstOf MaterialIds.glass152

        match store.resolveVersion glass with
        | Ok (Some _) -> ()
        | Ok None
        | Error _ -> Assert.Fail($"the Built-in store does not resolve {glass.materialId.value}")

        Assert.Equal<Result<MaterialEntry option, MaterialError>>(store.resolveVersion glass, over.resolveVersion glass)
        Assert.Equal<Result<MaterialEntry option, MaterialError>>(Ok (Some crystalEntry), over.tryGetMaterial crystalVersion.materialId)

    [<Fact>]
    let ``a vector of another length is refused`` () : unit =
        let items = [ cpl1; qz; phone ]

        Assert.Equal<Result<ExperimentAssignment, StudyForwardError>>(
            Error (VectorLengthErr "it holds 3 value(s), and the layout has 4 entry(ies)"),
            qzLayout.assignmentFor layoutStudy items e1 (StudyParameterVector [| 0.7; 2.0; 3.0 |]))

        Assert.Equal<Result<ExperimentAssignment, StudyForwardError>>(
            Error (VectorLengthErr "it holds 5 value(s), and the layout has 4 entry(ies)"),
            qzLayout.assignmentFor layoutStudy items e1 (StudyParameterVector [| 0.7; 2.0; 3.0; 1.6; 1.6 |]))

    // The forward model's bench: a HeNe source item, an ideal linear polarizer (the prep), a real linear polarizer (the
    // analyzer), glass samples, QZ's crystal plate and a detector item of each form; every model is a shipped entry of
    // step 210.
    let private benchDraftOf (id : string) (label : string) (model : ItemModelChoice) (dial : ItemDial) : ItemDraft =
        {
            id = ItemId (Guid.Parse id)
            label = ItemLabel label
            model = model
            dial = dial
        }

    let private idealLinearPolarizer : PolarizerPreset =
        match BuiltinCatalogue.preset PresetIds.polarizerLp with
        | PolarizerItem polarizer -> polarizer
        | SampleItem _
        | SourceItem _
        | DetectorItem _ -> failwith "the Built-in linear polarizer preset is a polarizer"

    let private sampleChoiceOf (id : SampleId) : ItemModelChoice =
        StackSampleChoice (BuiltinCatalogue.itemModel ItemModelIds.stackSampleItemModelId, SampleVersionId.firstOf id, BuiltinCatalogue.sample id)

    let private laserDraft : ItemDraft =
        benchDraftOf
            "37400000-0000-4000-8000-000000000001"
            "HeNe"
            (SourcePresetChoice (BuiltinCatalogue.itemModel ItemModelIds.sourcePresetItemModelId, BuiltinCatalogue.source PresetIds.heNe633))
            FixedMounting

    let private prepDraft : ItemDraft =
        benchDraftOf
            "37400000-0000-4000-8000-000000000002"
            "LP1"
            (PolarizerPresetChoice (BuiltinCatalogue.itemModel ItemModelIds.idealPolarizerItemModelId, idealLinearPolarizer))
            FixedMounting

    let private analyzerDraft : ItemDraft =
        benchDraftOf
            "37400000-0000-4000-8000-000000000003"
            "LP2"
            (LibraryModelChoice (BuiltinCatalogue.itemModel ItemModelIds.realLinearPolarizerModelId))
            FixedMounting

    /// The real linear polarizer on a dial read backwards.
    let private dialAnalyzerDraft : ItemDraft =
        { analyzerDraft with id = ItemId (Guid.Parse "37400000-0000-4000-8000-000000000004"); label = ItemLabel "LP2 dial"; dial = DialMounted DialNegative }

    let private plateDraft : ItemDraft = benchDraftOf "37400000-0000-4000-8000-000000000005" "Plate" (sampleChoiceOf SampleIds.glassPlate1mmId) FixedMounting

    /// A free-standing 200 nm glass film, whose interference makes the path depend on λ.
    let private filmDraft : ItemDraft = benchDraftOf "37400000-0000-4000-8000-000000000006" "Film" (sampleChoiceOf SampleIds.glassFilm200Id) FixedMounting

    let private intensityDraft : ItemDraft =
        benchDraftOf
            "37400000-0000-4000-8000-000000000007"
            "Camera"
            (LibraryModelChoice (BuiltinCatalogue.itemModel ItemModelIds.intensityDetectorModelId))
            FixedMounting

    let private polarimeterDraft : ItemDraft =
        benchDraftOf
            "37400000-0000-4000-8000-000000000008"
            "Polarimeter"
            (LibraryModelChoice (BuiltinCatalogue.itemModel ItemModelIds.muellerPolarimeterModelId))
            FixedMounting

    let private ellipsometerDraft : ItemDraft =
        benchDraftOf
            "37400000-0000-4000-8000-000000000009"
            "Ellipsometer"
            (LibraryModelChoice (BuiltinCatalogue.itemModel ItemModelIds.ellipsometerModelId))
            FixedMounting

    let private benchDrafts : ItemDraft list =
        [ laserDraft; prepDraft; analyzerDraft; dialAnalyzerDraft; plateDraft; filmDraft; intensityDraft; polarimeterDraft; ellipsometerDraft; qzDraft ]

    /// The item with one parameter known at that number, failing with the refusal's text.
    let private knownAt (name : ParameterName) (value : float) (item : Item) : Item =
        match item.withParameterState name (KnownParameter (ParameterValue value)) with
        | Ok known -> known
        | Error error -> failwith $"'{item.label.value}' refused {name.value}: {error.describe}"

    let private laser : Item = laserDraft.toItem
    let private prep : Item = prepDraft.toItem
    let private analyzer : Item = analyzerDraft.toItem
    let private dialAnalyzer : Item = dialAnalyzerDraft.toItem
    let private plate : Item = plateDraft.toItem
    let private film : Item = filmDraft.toItem

    /// An intensity detector at gain 2 and dark offset 0.1.
    let private camera : Item = intensityDraft.toItem |> knownAt ParameterName.gain 2.0 |> knownAt ParameterName.darkOffset 0.1

    let private polarimeter : Item = polarimeterDraft.toItem
    let private ellipsometer : Item = ellipsometerDraft.toItem

    let private prepSlot : SetupSlot = emptySlot (Library.ElementId "lp") LinearPolarizer { x = -0.3<meter>; y = 0.0<meter> }
    let private sampleSlot : SetupSlot = emptySlot (Library.ElementId "plate") Sample { x = 0.0<meter>; y = 0.0<meter> }
    let private analyzerSlot : SetupSlot = emptySlot (Library.ElementId "analyzer") LinearPolarizer { x = 0.3<meter>; y = 0.0<meter> }

    /// The slot holding the item as `AssignSlot` places it, its slot bound to the item's library entry, failing with the
    /// refusal's text.
    let private placing (item : Item) (slot : SetupSlot) : SetupSlot =
        let model =
            match benchDrafts |> List.tryFind (fun draft -> draft.id = item.id) with
            | Some draft -> draft.model
            | None -> failwith $"'{item.label.value}' is not on the bench"

        match slot.withItem item model with
        | Ok held -> held
        | Error error -> failwith error.describe

    /// The forward context over a kit of these items and setups, each item's model as its bench draft chose it.
    let private benchContext (items : Item list) (setups : Setup list) : StudyForwardContext =
        let benchChoiceOf (item : Item) : ItemModelChoice option =
            benchDrafts |> List.tryFind (fun draft -> draft.id = item.id) |> Option.map (fun draft -> draft.model)

        { forwardOver items layoutStudy [] setups with choiceOf = benchChoiceOf }

    /// F1 over the setup's current revision.
    let private runOver (setup : Setup) : Experiment = { e1Run with id = ExperimentId 5; name = ExperimentName "F1"; setup = setup.reference }

    /// No parameter takes a vector's value: each item is evaluated with its own.
    let private ownAssignment : ExperimentAssignment = { itemValues = Map.empty; crystals = Map.empty }

    let private boundOf (result : Result<BoundExperiment, StudyForwardError>) : BoundExperiment =
        match result with
        | Ok bound -> bound
        | Error error -> failwith $"expected a bound experiment, got {error.describe}"

    /// The run's parameter as `boundWith` builds it over the context and the assignment.
    let private runParamOf (context : StudyForwardContext) (assignment : ExperimentAssignment) : SlotRunParam =
        {
            library = context.library
            materials = assignment.materialProxyOver context.materials
            items = context.kit.items
            choiceOf = context.choiceOf
            valuesOf = assignment.valuesOf
        }

    let private deg (degrees : float) : Angle = Angle.degree (degrees * 1.0<deg>)

    let private nanometres (length : float) : WaveLength = WaveLength.nm (length * 1.0<nm>)

    let private nanometresIn (w : WaveLength) : float = (Units.wavelengthToUnit Units.Nanometer w).value

    /// Transmitting at that rotation and incidence.
    let private orientedAt (rotation : float) (incidence : float) : ElementOrientation =
        { rotation = deg rotation; incidence = IncidenceAngle.create (deg incidence); branch = BranchTransmitted }

    /// The slot's run step: its run element evaluated at the orientation and λ, failing when it binds none or refuses.
    let private runStepAt (param : SlotRunParam) (w : WaveLength) (slot : SetupSlot, orientation : ElementOrientation) : RunStep =
        match slot.runElementWith param with
        | Ok (Some element) ->
            match element.muellerAt orientation w with
            | Ok mueller -> slot.runStepOf mueller
            | Error error -> failwith error.describe
        | Ok None -> failwith $"the slot '{slot.id.value}' binds no run element"
        | Error error -> failwith error.describe

    /// What the light-source axes' defaults emit: unpolarized light at unit scale.
    let private defaultEmission : SourceEmission =
        match SourceEmission.ofValues (ParameterValues.defaultsOf SourcePreset.lightSourceAxes) with
        | Ok emission -> emission
        | Error error -> failwith error.describe

    /// S₀ of the default emission through the slots at their orientations and λ, as `ExperimentRun.read Intensity` reads it.
    let private s0Through (param : SlotRunParam) (w : WaveLength) (oriented : (SetupSlot * ElementOrientation) list) : float =
        match read Intensity defaultEmission (oriented |> List.map (runStepAt param w)) with
        | IntensityReading s0 -> s0.value
        | EllipsometricReading _ -> failwith "an intensity reading is S₀"

    let private settingAt (slot : SetupSlot) (variable : VariableParameter) (value : float) : ScanSetting =
        {
            elementId = slot.id
            variable = variable
            value = SweptValue value
        }

    let private signalOf (reading : Result<RecordedReading, StudyForwardError>) : float =
        match reading with
        | Ok (RecordedSignal signal) -> signal.value
        | Ok other -> failwith $"expected a recorded signal, got %A{other}"
        | Error error -> failwith error.describe

    let private assertSignalWithin (expected : float) (reading : Result<RecordedReading, StudyForwardError>) : unit =
        let actual = signalOf reading
        Assert.True(abs (expected - actual) <= 1.0e-12, $"expected {expected:R}, got {actual:R}")

    /// The source, the prep at R1 0° and the analyzer, then the detector slot holding the detector item.
    let private malusSlots (analyzerItem : Item) (detectorItem : Item) : SetupSlot list =
        [ placing laser sourceSlot; placing prep prepSlot; placing analyzerItem analyzerSlot; placing detectorItem detectorSlot ]

    [<Fact>]
    let ``an intensity detector reads S0 times its gain plus its dark level along the setup's path`` () : unit =
        let setup = setupOf (SetupName "Malus") (malusSlots analyzer camera)
        let slots = setup.slots
        let context = benchContext [ laser; prep; analyzer; camera ] [ setup ]
        let bound = boundOf ((runOver setup).boundWith context layoutStudy ownAssignment None)

        // The HeNe emits the default light at its own line; the path is the prep then the analyzer; the camera reads
        // through its own gain and dark offset.
        Assert.Equal<SourceEmission>(defaultEmission, bound.emission)
        Assert.Equal<WaveLength>((BuiltinCatalogue.source PresetIds.heNe633).waveLength, bound.operatingWaveLength)
        Assert.Equal<Library.ElementId list>([ prepSlot.id; analyzerSlot.id ], bound.path |> List.map (fun step -> step.slot.id))
        Assert.Equal<ItemId>(camera.id, bound.detectorItem)
        Assert.Equal<DetectorReadout>(IntensityDetectorReadout (ParameterValue 2.0, DarkMean 0.1), bound.readout)

        // The analyzer turned to 60°: Malus's cos² 60° = 1/4 of the prepared light.
        let param = runParamOf context ownAssignment
        let s0 = s0Through param bound.operatingWaveLength [ slots.[1], orientedAt 0.0 0.0; slots.[2], orientedAt 60.0 0.0 ]
        Assert.True(abs (s0 - 0.25) <= 1.0e-12, $"S₀ = {s0:R}")

        let turned = [ settingAt analyzerSlot VaryR1 60.0 ]
        assertSignalWithin (s0 * 2.0 + 0.1) (bound.readingAt turned None)

        // A session's dark level replaces the item's dark offset.
        let darkBound = boundOf ((runOver setup).boundWith context layoutStudy ownAssignment (Some (DarkMean 0.3)))
        Assert.Equal<DetectorReadout>(IntensityDetectorReadout (ParameterValue 2.0, DarkMean 0.3), darkBound.readout)
        assertSignalWithin (s0 * 2.0 + 0.3) (darkBound.readingAt turned None)

        // The gain the assignment holds is the one read.
        let fitted : ExperimentAssignment =
            { ownAssignment with itemValues = Map.ofList [ camera.id, (ownValuesOf camera).withValue ParameterName.gain (ParameterValue 3.0) ] }

        assertSignalWithin (s0 * 3.0 + 0.1) ((boundOf ((runOver setup).boundWith context layoutStudy fitted None)).readingAt turned None)

    [<Fact>]
    let ``a scan setting turns, tilts or retunes the path, and a dial reading turns its slot through the vector's zero`` () : unit =
        // The analyzer on a backwards dial whose zero is 10° in the assignment, its own zero 0°.
        let dialZero : ExperimentAssignment =
            {
                ownAssignment with
                    itemValues = Map.ofList [ dialAnalyzer.id, (ownValuesOf dialAnalyzer).withValue ParameterName.dialZero (ParameterValue (Math.PI / 18.0)) ]
            }

        let dialSetup = setupOf (SetupName "Dial") (malusSlots dialAnalyzer camera)
        let dialContext = benchContext [ laser; prep; dialAnalyzer; camera ] [ dialSetup ]
        let dialBound = boundOf ((runOver dialSetup).boundWith dialContext layoutStudy dialZero None)
        let dialParam = runParamOf dialContext dialZero
        let w = dialBound.operatingWaveLength

        let at30 = s0Through dialParam w [ dialSetup.slots.[1], orientedAt 0.0 0.0; dialSetup.slots.[2], orientedAt 30.0 0.0 ] * 2.0 + 0.1
        let at20 = s0Through dialParam w [ dialSetup.slots.[1], orientedAt 0.0 0.0; dialSetup.slots.[2], orientedAt 20.0 0.0 ] * 2.0 + 0.1
        Assert.True(abs (at30 - at20) > 1.0e-3, "the analyzer at 30° and at 20° read alike")

        // A dial reading of −20° is the analyzer at −(−20° − 10°) = 30°, not the 20° its own zero would give.
        Assert.Equal<ItemDial>(DialMounted DialNegative, dialBound.path.[1].dial)
        assertSignalWithin at30 (dialBound.readingAt [ settingAt analyzerSlot VaryDialReading (-20.0) ] None)

        // Recorded as the slot's dial turn, the same reading turns it alike without a setting.
        let recordedSlots =
            dialSetup.slots |> List.map (fun slot -> if slot.id = analyzerSlot.id then { slot with content = ItemInSlot (dialAnalyzer.id, DialTurn (deg (-20.0))) } else slot)

        let recordedSetup = setupOf (SetupName "Dial recorded") recordedSlots
        let recordedContext = benchContext [ laser; prep; dialAnalyzer; camera ] [ recordedSetup ]
        assertSignalWithin at30 ((boundOf ((runOver recordedSetup).boundWith recordedContext layoutStudy dialZero None)).readingAt [] None)

        // An R1 setting turns the dial-mounted analyzer to its value directly.
        assertSignalWithin at20 (dialBound.readingAt [ settingAt analyzerSlot VaryR1 20.0 ] None)

        // A glass sample between the polarizers, the analyzer at 45°: an R2 setting tilts it, a wavelength setting retunes
        // the path, and a requested wavelength wins over it. The film makes λ tell.
        for sampleItem in [ plate; film ] do
            let slots = [ placing laser sourceSlot; placing prep prepSlot; placing sampleItem sampleSlot; placing analyzer analyzerSlot; placing camera detectorSlot ]
            let setup = setupOf (SetupName $"Tilt {sampleItem.label.value}") slots
            let context = benchContext [ laser; prep; sampleItem; analyzer; camera ] [ setup ]
            let bound = boundOf ((runOver setup).boundWith context layoutStudy ownAssignment None)
            let param = runParamOf context ownAssignment
            let operating = bound.operatingWaveLength

            let expectedAt (incidence : float) (w : WaveLength) : float =
                s0Through param w [ slots.[1], orientedAt 0.0 0.0; slots.[2], orientedAt 0.0 incidence; slots.[3], orientedAt 45.0 0.0 ] * 2.0 + 0.1

            let turned = settingAt analyzerSlot VaryR1 45.0
            let tilted = [ turned; settingAt sampleSlot VaryR2 40.0 ]
            let retuned = tilted @ [ settingAt sourceSlot VaryWaveLength 500.0 ]

            Assert.Equal<Library.ElementId list>([ prepSlot.id; sampleSlot.id; analyzerSlot.id ], bound.path |> List.map (fun step -> step.slot.id))
            let drawn = bound.path.[1].drawnIncidence.angle.degrees
            Assert.True(abs drawn <= 1.0e-9, $"the sample is drawn at {drawn:R}°")
            assertSignalWithin (expectedAt 0.0 operating) (bound.readingAt [ turned ] None)
            assertSignalWithin (expectedAt 40.0 operating) (bound.readingAt tilted None)
            Assert.True(abs (expectedAt 40.0 operating - expectedAt 0.0 operating) > 1.0e-4, $"'{sampleItem.label.value}' reads alike at 0° and 40°")
            assertSignalWithin (expectedAt 40.0 (nanometres 500.0)) (bound.readingAt retuned None)
            assertSignalWithin (expectedAt 40.0 (nanometres 450.0)) (bound.readingAt retuned (Some (nanometres 450.0)))

        let filmSlots = [ placing laser sourceSlot; placing prep prepSlot; placing film sampleSlot; placing analyzer analyzerSlot; placing camera detectorSlot ]
        let filmParam = runParamOf (benchContext [ laser; prep; film; analyzer; camera ] []) ownAssignment

        let filmAt (w : WaveLength) : float =
            s0Through filmParam w [ filmSlots.[1], orientedAt 0.0 0.0; filmSlots.[2], orientedAt 0.0 40.0; filmSlots.[3], orientedAt 45.0 0.0 ]

        let operating = (BuiltinCatalogue.source PresetIds.heNe633).waveLength

        for w, other in [ nanometres 500.0, operating; nanometres 450.0, nanometres 500.0 ] do
            Assert.True(abs (filmAt w - filmAt other) > 1.0e-4, $"the film reads alike at {nanometresIn w:R} nm and {nanometresIn other:R} nm")

    [<Fact>]
    let ``a polarimeter reads its path's Mueller matrix, and an ellipsometer Psi and Delta of the arriving state`` () : unit =
        let turned = [ settingAt analyzerSlot VaryR1 30.0 ]
        let polarimeterSetup = setupOf (SetupName "Polarimeter") (malusSlots analyzer polarimeter)
        let polarimeterContext = benchContext [ laser; prep; analyzer; polarimeter ] [ polarimeterSetup ]
        let polarimeterBound = boundOf ((runOver polarimeterSetup).boundWith polarimeterContext layoutStudy ownAssignment None)
        let param = runParamOf polarimeterContext ownAssignment
        let w = polarimeterBound.operatingWaveLength
        let steps = [ polarimeterSetup.slots.[1], orientedAt 0.0 0.0; polarimeterSetup.slots.[2], orientedAt 30.0 0.0 ] |> List.map (runStepAt param w)

        Assert.Equal<DetectorReadout>(PolarimeterDetectorReadout, polarimeterBound.readout)

        // The analyzer's matrix times the prep's: the path's, its m₀₀ the throughput.
        let expected = steps.[1].mueller * steps.[0].mueller

        match polarimeterBound.readingAt turned None with
        | Ok (RecordedMueller recorded) ->
            for i in 0 .. 3 do
                for j in 0 .. 3 do
                    let e = expected.element i j
                    let a = recorded.element i j
                    Assert.True(abs (e - a) <= 1.0e-12, $"element ({i},{j}): expected {e:R}, got {a:R}")
        | Ok other -> Assert.Fail($"expected a recorded Mueller matrix, got %A{other}")
        | Error error -> Assert.Fail(error.describe)

        // An ellipsometer on the same path reads Ψ and Δ of the state arriving at it.
        let ellipsometerSetup = setupOf (SetupName "Ellipsometer") (malusSlots analyzer ellipsometer)
        let ellipsometerContext = benchContext [ laser; prep; analyzer; ellipsometer ] [ ellipsometerSetup ]
        let ellipsometerBound = boundOf ((runOver ellipsometerSetup).boundWith ellipsometerContext layoutStudy ownAssignment None)

        Assert.Equal<DetectorReadout>(EllipsometerDetectorReadout, ellipsometerBound.readout)

        match read Ellipsometer defaultEmission steps, ellipsometerBound.readingAt turned None with
        | EllipsometricReading expectedPsiDelta, Ok (RecordedPsiDelta psiDelta) ->
            Assert.True(abs (expectedPsiDelta.psi.value - psiDelta.psi.value) <= 1.0e-12, $"Ψ: expected {expectedPsiDelta.psi.degrees:R}°, got {psiDelta.psi.degrees:R}°")
            Assert.True(abs (expectedPsiDelta.delta.value - psiDelta.delta.value) <= 1.0e-12, $"Δ: expected {expectedPsiDelta.delta.degrees:R}°, got {psiDelta.delta.degrees:R}°")
        | IntensityReading _, _ -> Assert.Fail "an ellipsometric read is Ψ and Δ"
        | EllipsometricReading _, Ok other -> Assert.Fail($"expected a recorded Ψ and Δ, got %A{other}")
        | EllipsometricReading _, Error error -> Assert.Fail(error.describe)

    [<Fact>]
    let ``a setup without a detector item, or whose setup or slot item does not resolve, is refused`` () : unit =
        // The detector slot holds nothing.
        let unread = setupOf (SetupName "Unread") [ placing laser sourceSlot; placing prep prepSlot; placing analyzer analyzerSlot; detectorSlot ]
        let unreadRun = runOver unread

        Assert.Equal<Result<BoundExperiment, StudyForwardError>>(
            Error (StudyDetectorErr $"{unread.reference.describe}, which {unreadRun.name.value} references"),
            unreadRun.boundWith (benchContext [ laser; prep; analyzer ] [ unread ]) layoutStudy ownAssignment None)

        // The experiment references a setup the kit lacks.
        let missing : SetupReference = { name = SetupName "Missing"; revision = SetupRevision.first }
        let malus = setupOf (SetupName "Malus") (malusSlots analyzer camera)
        let items = [ laser; prep; analyzer; camera ]

        let missingRun = { runOver malus with setup = missing }

        Assert.Equal<Result<BoundExperiment, StudyForwardError>>(
            Error (ForwardSetupErr (UnknownSetupErr missing.describe)),
            missingRun.boundWith (benchContext items [ malus ]) layoutStudy ownAssignment None)

        // The analyzer slot holds an item the kit lacks.
        Assert.Equal<Result<BoundExperiment, StudyForwardError>>(
            Error (ForwardSlotErr (SlotItemErr (UnknownSlotItemErr $"the kit holds no item {analyzer.id.value}"))),
            (runOver malus).boundWith (benchContext [ laser; prep; camera ] [ malus ]) layoutStudy ownAssignment None)

    /// The probe crystal as an assignment holds it: n_o 1.544, n_e 1.553 and no gyration.
    let private observedParameters : UniaxialParameters =
        {
            ordinaryIndex = RefractionIndex 1.544
            extraordinaryIndex = RefractionIndex 1.553
            g11 = RhoValue 0.0
            g33 = RhoValue 0.0
        }

    /// Every item at its own values and the probe crystal at those parameters.
    let private observedAssignment : ExperimentAssignment =
        { ownAssignment with crystals = Map.ofList [ crystalVersion, UniaxialCrystal observedParameters ] }

    /// The bench's context over these items and setups, its material store also holding the probe crystal QZ's plate pins.
    let private crystalBenchContext (items : Item list) (setups : Setup list) : StudyForwardContext =
        let store = BuiltinCatalogue.materialStore (fun _ -> [])

        match store.saveMaterial crystalEntry with
        | Ok () -> { benchContext items setups with materials = store }
        | Error error -> failwith (SampleMaterialErr error).describe

    /// The source, QZ as the lone object, then the detector slot holding the detector item.
    let private crystalSlots (detectorItem : Item) : SetupSlot list =
        [ placing laser sourceSlot; placing qz sampleSlot; placing detectorItem detectorSlot ]

    /// An X-cut 1 mm plate at incidence 20°, azimuth 30° and 600 nm, transmitted.
    let private xCutConfiguration : MeasurementConfiguration =
        {
            cut = XCut
            thickness = Thickness.mm 1.0<mm>
            incidenceAngle = IncidenceAngle.create (deg 20.0)
            azimuth = SampleAzimuth.degree 30.0<deg>
            observable = TransmittedMueller
            waveLength = nanometres 600.0
        }

    let private observationAt (configuration : MeasurementConfiguration) : MeasuredRow =
        ObservationRow { configuration = configuration; measured = identityMueller }

    /// The recorded Mueller matrix of a reading, failing on any other reading or a refusal.
    let private recordedMuellerOf (reading : Result<RecordedReading, StudyForwardError>) : MuellerMatrix =
        match reading with
        | Ok (RecordedMueller matrix) -> matrix
        | Ok other -> failwith $"expected a recorded Mueller matrix, got %A{other}"
        | Error error -> failwith error.describe

    /// The matrix's sixteen elements, row by row.
    let private elementsOf (matrix : MuellerMatrix) : float list =
        [
            for i in 0 .. 3 do
                for j in 0 .. 3 do
                    matrix.element i j
        ]

    [<Fact>]
    let ``an observation is predicted as a cut plate of its object's opened crystal at its own configuration`` () : unit =
        let setup = setupOf (SetupName "Crystal") (crystalSlots polarimeter)
        let context = crystalBenchContext [ laser; qz; polarimeter ] [ setup ]
        let bound = boundOf ((runOver setup).boundWith context layoutStudy observedAssignment None)

        // QZ is the lone object and its plate is made of the opened probe version, so the assignment's crystal is
        // observed, solved with the context's solver parameters.
        Assert.Equal<DetectorReadout>(PolarimeterDetectorReadout, bound.readout)
        Assert.Equal<CrystalParameters option>(Some (UniaxialCrystal observedParameters), bound.observedCrystalOpt)
        Assert.Equal<SolverParameters>(SolverParameters.defaultValue, bound.solverParameters)

        // An assignment holding no crystal observes the study's working copy: n_o at its unknown's start, gyrating.
        let ownBound = boundOf ((runOver setup).boundWith context layoutStudy ownAssignment None)
        let copy = (openedOf layoutStudy crystalVersion).currentCrystal
        Assert.Equal<CrystalParameters option>(Some copy, ownBound.observedCrystalOpt)
        Assert.NotEqual<CrystalParameters>(UniaxialCrystal observedParameters, copy)

        // The configuration's orientation gives its azimuth, incidence and observable back.
        let reflected = { xCutConfiguration with observable = ReflectedMueller }

        for configuration in [ xCutConfiguration; reflected ] do
            let orientation = configuration.elementOrientation
            Assert.Equal<SampleAzimuth>(configuration.azimuth, orientation.sampleAzimuth)
            Assert.Equal<IncidenceAngle>(configuration.incidenceAngle, orientation.incidence)
            Assert.Equal<Observable>(configuration.observable, orientation.observable)

        Assert.Equal<Propagation.Branch>(BranchTransmitted, xCutConfiguration.elementOrientation.branch)
        Assert.Equal<Propagation.Branch>(BranchReflected, reflected.elementOrientation.branch)

        // Each observation reads exactly the spec 0044 forward's matrix at its own configuration and those parameters.
        let forward = createBerremanForward (fun (p : UniaxialParameters) -> p.opticalProperties) SolverParameters.defaultValue

        let forwardAt (configuration : MeasurementConfiguration) : MuellerMatrix =
            match forward.muellerOf configuration observedParameters with
            | Ok matrix -> matrix
            | Error (ForwardSolveFailedErr (_, reason)) -> failwith reason

        let transmittedReading = recordedMuellerOf (bound.predictedReadingOf (observationAt xCutConfiguration))
        let reflectedReading = recordedMuellerOf (bound.predictedReadingOf (observationAt reflected))

        Assert.Equal<float list>(elementsOf (forwardAt xCutConfiguration), elementsOf transmittedReading)
        Assert.Equal<float list>(elementsOf (forwardAt reflected), elementsOf reflectedReading)
        Assert.NotEqual<float list>(elementsOf transmittedReading, elementsOf reflectedReading)

        // The working copy's crystal predicts another transmitted matrix: the observed crystal is the one solved.
        let copyReading = recordedMuellerOf (ownBound.predictedReadingOf (observationAt xCutConfiguration))
        let largestChange = List.map2 (fun a b -> abs (a - b)) (elementsOf transmittedReading) (elementsOf copyReading) |> List.max
        Assert.True(largestChange > 1.0e-3, $"the copy's crystal changes the transmitted matrix by at most {largestChange:R}")

        // A settings row reads as `readingAt` does, at its own wavelength when it carries one; its measured value plays no part.
        let tilted = [ settingAt sampleSlot VaryR2 20.0 ]

        for waveLengthOpt in [ None; Some (nanometres 600.0) ] do
            Assert.Equal<float list>(
                elementsOf (recordedMuellerOf (bound.readingAt tilted waveLengthOpt)),
                elementsOf (recordedMuellerOf (bound.predictedReadingOf (SettingsRow (tilted, waveLengthOpt, RecordedMueller identityMueller)))))

    [<Fact>]
    let ``an observation without one opened plate, or under a detector that is no polarimeter, is refused`` () : unit =
        let observation = observationAt xCutConfiguration

        let boundOver (setup : Setup) (items : Item list) : BoundExperiment =
            boundOf ((runOver setup).boundWith (crystalBenchContext items [ setup ]) layoutStudy observedAssignment None)

        let plateRefusal (setup : Setup) : Result<RecordedReading, StudyForwardError> =
            Error (ObservationPlateErr $"{setup.reference.describe}, which {(runOver setup).name.value} references")

        // The glass plate is made of no opened material.
        let glass = setupOf (SetupName "Glass") [ placing laser sourceSlot; placing plate sampleSlot; placing polarimeter detectorSlot ]
        let glassBound = boundOver glass [ laser; plate; polarimeter ]
        Assert.Equal<CrystalParameters option>(None, glassBound.observedCrystalOpt)
        Assert.Equal<Result<RecordedReading, StudyForwardError>>(plateRefusal glass, glassBound.predictedReadingOf observation)

        // QZ beside the glass plate: two object items.
        let secondSampleSlot = emptySlot (Library.ElementId "plate2") Sample { x = 0.2<meter>; y = 0.0<meter> }

        let pair =
            setupOf (SetupName "Pair") [ placing laser sourceSlot; placing qz sampleSlot; placing plate secondSampleSlot; placing polarimeter detectorSlot ]

        let pairBound = boundOver pair [ laser; qz; plate; polarimeter ]
        Assert.Equal<CrystalParameters option>(None, pairBound.observedCrystalOpt)
        Assert.Equal<Result<RecordedReading, StudyForwardError>>(plateRefusal pair, pairBound.predictedReadingOf observation)

        // An object slot holding no item.
        let unheld = setupOf (SetupName "Unheld") [ placing laser sourceSlot; sampleSlot; placing polarimeter detectorSlot ]
        let unheldBound = boundOver unheld [ laser; polarimeter ]
        Assert.Equal<Result<RecordedReading, StudyForwardError>>(plateRefusal unheld, unheldBound.predictedReadingOf observation)

        // QZ under an intensity detector or an ellipsometer: its crystal is observed, but neither records a Mueller matrix.
        for detectorItem, kind in [ camera, "an intensity detector"; ellipsometer, "an ellipsometer" ] do
            let setup = setupOf (SetupName $"Crystal {detectorItem.label.value}") (crystalSlots detectorItem)
            let detectorBound = boundOver setup [ laser; qz; detectorItem ]
            Assert.Equal<CrystalParameters option>(Some (UniaxialCrystal observedParameters), detectorBound.observedCrystalOpt)

            Assert.Equal<Result<RecordedReading, StudyForwardError>>(
                Error (ReadingMismatchErr $"{(runOver setup).name.value} is read by {kind}, which records no Mueller matrix"),
                detectorBound.predictedReadingOf observation)

    /// The matrix with every element times `factor`, as a throughput scales it.
    let private scaledBy (factor : float) (matrix : MuellerMatrix) : MuellerMatrix =
        RealMatrix4x4.create [ for i in 0 .. 3 -> [ for j in 0 .. 3 -> factor * matrix.element i j ] ] |> MuellerMatrix

    /// The entries of a residual, failing on a refusal.
    let private entriesOf (residual : Result<ResidualVector, InversionError>) : float list =
        match residual with
        | Ok entries -> List.ofArray entries.value
        | Error error -> failwith $"the residual was refused: %A{error}"

    /// A row's residual, failing on a refusal.
    let private rowResidualOf (residual : Result<RowResidual, StudyForwardError>) : RowResidual =
        match residual with
        | Ok rowResidual -> rowResidual
        | Error error -> failwith error.describe

    [<Fact>]
    let ``a signal row's entry is the predicted signal less the measured`` () : unit =
        let residual = rowResidualOf ((RecordedSignal (MeasuredIntensity 2.5)).residualAgainst (RecordedSignal (MeasuredIntensity 2.0)))

        Assert.Equal<float list>([ 0.5 ], List.ofArray residual.elements.value)
        Assert.Equal<ResidualVector option>(None, residual.throughputOpt)

    [<Fact>]
    let ``a Psi and Delta row's entries are their differences, Delta's taken on the circle`` () : unit =
        let psiDelta (psi : float<deg>) (delta : float<deg>) : RecordedReading =
            RecordedPsiDelta { psi = Angle.degree psi; delta = Angle.degree delta }

        // Δ 179° against −179° is 2° apart on the circle, not 358°.
        let residual = rowResidualOf ((psiDelta 30.0<deg> 179.0<deg>).residualAgainst (psiDelta 28.0<deg> (-179.0<deg>)))
        let twoDegrees = (Angle.degree 2.0<deg>).value

        match List.ofArray residual.elements.value with
        | [ psi; delta ] ->
            Assert.Equal(twoDegrees, psi, 1e-12)
            Assert.Equal(-twoDegrees, delta, 1e-12)
        | entries -> Assert.Fail($"expected Ψ and Δ entries, got {entries.Length} entries")

        Assert.Equal<ResidualVector option>(None, residual.throughputOpt)

    [<Fact>]
    let ``a Mueller row's entries are the normalized element differences and the log-throughput ratio`` () : unit =
        let lp = analyzerMueller IdealLinear (Angle.degree 30.0<deg>)
        let predicted = scaledBy 0.8 lp
        let measured = scaledBy 0.5 lp

        let throughputOf (residual : RowResidual) : float list =
            match residual.throughputOpt with
            | Some throughput -> List.ofArray throughput.value
            | None -> failwith "a Mueller row's residual carries its log-throughput ratio"

        // The same polarizer at two throughputs: its normalized elements agree, and only the throughput ratio is left.
        let scaled = rowResidualOf ((RecordedMueller predicted).residualAgainst (RecordedMueller measured))
        let scaledElements = List.ofArray scaled.elements.value
        Assert.Equal<float list>(entriesOf (observationResidual measured predicted), scaledElements)
        Assert.Equal(15, scaledElements.Length)

        for entry in scaledElements do
            Assert.Equal(0.0, entry, 1e-15)

        match throughputOf scaled with
        | [ ratio ] -> Assert.Equal(log 0.8 - log 0.5, ratio, 1e-15)
        | entries -> Assert.Fail($"expected one throughput entry, got {entries.Length}")

        Assert.Equal<float list>(entriesOf (throughputResidual measured predicted), throughputOf scaled)

        // A measured retarder against the same prediction: the elements are `observationResidual`'s, and they differ.
        let retarder = retarderMueller (Angle.degree 20.0<deg>) (Retardance.degree 70.0<deg>)
        let crossed = rowResidualOf ((RecordedMueller predicted).residualAgainst (RecordedMueller retarder))
        let crossedElements = List.ofArray crossed.elements.value
        Assert.Equal<float list>(entriesOf (observationResidual retarder predicted), crossedElements)
        Assert.Equal(15, crossedElements.Length)
        Assert.True(crossedElements |> List.exists (fun entry -> abs entry > 0.1), "a retarder is not the polarizer")
        Assert.Equal<float list>(entriesOf (throughputResidual retarder predicted), throughputOf crossed)

    [<Fact>]
    let ``a reading of another kind than the row's, or a Mueller matrix without positive throughput, is refused`` () : unit =
        let signal = RecordedSignal (MeasuredIntensity 1.0)
        let psiDelta = RecordedPsiDelta { psi = Angle.degree 45.0<deg>; delta = Angle.degree 90.0<deg> }
        let lp = analyzerMueller IdealLinear (Angle.degree 0.0<deg>)
        let mueller = RecordedMueller lp

        let mismatch (predicted : string) (measured : string) : Result<RowResidual, StudyForwardError> =
            Error (ReadingMismatchErr $"the prediction is {predicted}, and the measured reading {measured}")

        // Every pair of different kinds names both.
        Assert.Equal<Result<RowResidual, StudyForwardError>>(mismatch "a signal" "a Mueller matrix", signal.residualAgainst mueller)
        Assert.Equal<Result<RowResidual, StudyForwardError>>(mismatch "a Mueller matrix" "a signal", mueller.residualAgainst signal)
        Assert.Equal<Result<RowResidual, StudyForwardError>>(mismatch "a signal" "Ψ and Δ", signal.residualAgainst psiDelta)
        Assert.Equal<Result<RowResidual, StudyForwardError>>(mismatch "Ψ and Δ" "a signal", psiDelta.residualAgainst signal)
        Assert.Equal<Result<RowResidual, StudyForwardError>>(mismatch "Ψ and Δ" "a Mueller matrix", psiDelta.residualAgainst mueller)
        Assert.Equal<Result<RowResidual, StudyForwardError>>(mismatch "a Mueller matrix" "Ψ and Δ", mueller.residualAgainst psiDelta)

        // A measured matrix of m₀₀ 0 is refused, and so is a prediction of negative m₀₀; the measured one is named first.
        let dark = scaledBy 0.0 lp
        let negative = scaledBy (-1.0) lp
        Assert.Equal<Result<RowResidual, StudyForwardError>>(Error (RowResidualErr (NonPositiveIntensityErr 0.0)), mueller.residualAgainst (RecordedMueller dark))
        Assert.Equal<Result<RowResidual, StudyForwardError>>(Error (RowResidualErr (NonPositiveIntensityErr (-0.5))), (RecordedMueller negative).residualAgainst mueller)
        Assert.Equal<Result<RowResidual, StudyForwardError>>(Error (RowResidualErr (NonPositiveIntensityErr 0.0)), (RecordedMueller negative).residualAgainst (RecordedMueller dark))

        // The throughput term refuses alone as its normalization does, the measured matrix first.
        Assert.Equal<Result<ResidualVector, InversionError>>(Error (NonPositiveIntensityErr 0.0), throughputResidual dark negative)
        Assert.Equal<Result<ResidualVector, InversionError>>(Error (NonPositiveIntensityErr (-0.5)), throughputResidual lp negative)

    let private k1 : ExperimentName = ExperimentName "K1"
    let private x1 : ExperimentName = ExperimentName "X1"
    let private p1 : ExperimentName = ExperimentName "P1"
    let private e9 : ExperimentName = ExperimentName "E9"

    /// Step 374's Malus setup: the HeNe, LP1, LP2 and the camera.
    let private prefitSetup : Setup = setupOf (SetupName "Malus") (malusSlots analyzer camera)

    /// LP2's R1 over 0…90° in 4 points.
    let private lp2Turn : ScanAxis = { polarizerTurn with elementId = analyzerSlot.id; label = ElementLabel "LP2" }

    /// A run of that role over the Malus setup, turning LP2 over 0…90° in 4 points, with no data.
    let private prefitRun (id : ExperimentId) (name : ExperimentName) (role : ExperimentRole) : Experiment =
        {
            e1Run with
                id = id
                name = name
                setup = prefitSetup.reference
                scan = ExperimentScan.oneAxis lp2Turn CaptureTransmitted None
                role = role
                sessionOpt = None
        }

    let private e1File : KitRelativePath = KitRelativePath "data/E1.csv"
    let private k1File : KitRelativePath = KitRelativePath "data/K1.csv"
    let private x1File : KitRelativePath = KitRelativePath "data/X1.csv"
    let private e3File : KitRelativePath = KitRelativePath "data/E3.csv"

    let private attachedHash : LibraryEntryFiles.ContentHash = LibraryEntryFiles.ContentHash "0f3c"
    let private changedHash : LibraryEntryFiles.ContentHash = LibraryEntryFiles.ContentHash "9a1d"

    /// The run with the file at that path attached at `attachedHash`, every row read and no point excluded.
    let private attachingAt (path : KitRelativePath) (run : Experiment) : Experiment =
        let data : MappedAttachment =
            {
                attachment = { path = path; contentHash = attachedHash }
                profile = ImportProfileName.intensitySeries
                rows = AllDataRows
                exclusions = []
            }

        { run with dataOpt = Some data }

    let private seriesOver (points : IntensityPoint list) : MappedPoints = SeriesPoints (IntensityData { points = points })

    /// Malus's law under LP2's turn, as an `Intensity series` file reads it.
    let private malusPoints : MappedPoints =
        seriesOver
            [
                intensityAt (MeasuredAbscissa 0.0) (MeasuredIntensity 1.0)
                intensityAt (MeasuredAbscissa 30.0) (MeasuredIntensity 0.75)
                intensityAt (MeasuredAbscissa 60.0) (MeasuredIntensity 0.25)
                intensityAt (MeasuredAbscissa 90.0) (MeasuredIntensity 0.0)
            ]

    /// One dark reading on a dark line.
    let private darkPoints : MappedPoints = seriesOver [ intensityAt (MeasuredAbscissa 0.0) (MeasuredIntensity 0.02) ]

    /// E1's, X1's and E3's files unchanged, K1's changed since attach.
    let private prefitStatuses : Map<KitRelativePath, Result<DataAttachmentStatus, KitError>> =
        Map.ofList
            [
                e1File, Ok AttachedDataUnchanged
                k1File, Ok (AttachedDataChanged changedHash)
                x1File, Ok AttachedDataUnchanged
                e3File, Ok AttachedDataUnchanged
            ]

    /// E1's, X1's and E3's files read as Malus's law, K1's as one dark reading.
    let private prefitPoints : Map<KitRelativePath, MappedPoints> =
        Map.ofList [ e1File, malusPoints; k1File, darkPoints; x1File, malusPoints; e3File, malusPoints ]

    /// A data seam answering `readMapped` by the points and `attachmentStatus` by the statuses, each by the attachment's
    /// path; nothing is picked or attached.
    let private prefitDataOver
        (statuses : Map<KitRelativePath, Result<DataAttachmentStatus, KitError>>)
        (points : Map<KitRelativePath, MappedPoints>)
        : ExperimentDataProxy =
        {
            readPickedFile = fun file -> Error (KitIoErr $"the test reads no picked file '{file.value}'")
            attachDataFile = fun file -> Error (KitIoErr $"the test attaches no file '{file.value}'")
            attachmentStatus =
                fun attachment ->
                    match statuses |> Map.tryFind attachment.path with
                    | Some status -> status
                    | None -> Error (MissingKitFileErr $"the test holds no file '{attachment.path.value}'")
            readMapped =
                fun data ->
                    match points |> Map.tryFind data.attachment.path with
                    | Some read -> Ok read
                    | None -> Error (MalformedDataFileErr $"the test holds no file '{data.attachment.path.value}'")
        }

    let private prefitE1 : Experiment = prefitRun (ExperimentId 11) e1 ScienceRun |> attachingAt e1File
    let private prefitK1 : Experiment = { prefitRun (ExperimentId 12) k1 DarkRun with scan = darkLine } |> attachingAt k1File
    let private prefitX1 : Experiment = prefitRun (ExperimentId 13) x1 DiagnosticRun |> attachingAt x1File
    let private prefitE2 : Experiment = prefitRun (ExperimentId 14) e2 CalibrationRun
    let private prefitE3 : Experiment = prefitRun (ExperimentId 15) e3 ScienceRun |> attachingAt e3File
    let private prefitP1 : Experiment = prefitRun (ExperimentId 16) p1 PlannedRun

    /// The kit's runs, in an order other than the study's.
    let private prefitRuns : Experiment list = [ prefitP1; prefitE3; prefitE2; prefitX1; prefitK1; prefitE1 ]

    /// E1, K1, X1, E2, E3 and P1 in that order, E3 disabled.
    let private prefitStudy : Study =
        let created = studyOf (Study.create (StudyName "Prefit") [ e1; k1; x1; e2; e3; p1 ] [] prefitRuns)
        studyOf (created.withExperimentEnablement e3 DisabledInStudy)

    /// The bench's context over the Malus setup, a kit of these runs holding the study, and that data seam.
    let private contextOverData (study : Study) (runs : Experiment list) (data : ExperimentDataProxy) : StudyForwardContext =
        let context = benchContext [ laser; prep; analyzer; camera ] [ prefitSetup ]

        {
            context with
                kit = { context.kit with studies = [ study ]; experiments = runs }
                experimentData = data
        }

    /// The bench's context over these runs, the files read as `prefitPoints` with the statuses given.
    let private prefitContextOver
        (study : Study)
        (runs : Experiment list)
        (statuses : Map<KitRelativePath, Result<DataAttachmentStatus, KitError>>)
        : StudyForwardContext =
        contextOverData study runs (prefitDataOver statuses prefitPoints)

    let private namesOfPrefits (result : Result<ExperimentPrefit list, StudyForwardError>) : ExperimentName list =
        match result with
        | Ok prefits -> prefits |> List.map (fun prefit -> prefit.experiment.name)
        | Error error -> failwith $"expected the study's pre-fit, got {error.describe}"

    [<Fact>]
    let ``a study's pre-fit checks every run a fit reads, in study order, with its data-change flag`` () : unit =
        let context = prefitContextOver prefitStudy prefitRuns prefitStatuses

        // The last fit run read E1's file at another hash.
        let fitted : DataAttachment list = [ { path = e1File; contentHash = changedHash } ]

        // E1 and the Dark K1 checked and flagged, E2 without data unflagged; the Diagnostic X1, the disabled E3 and the
        // Planned P1 are not read.
        let expected : ExperimentPrefit list =
            [
                { experiment = prefitE1; check = Ok malusPoints; changeOpt = Some (ChangedSinceFitFlag attachedHash) }
                { experiment = prefitK1; check = Ok darkPoints; changeOpt = Some (ChangedSinceAttachFlag changedHash) }
                {
                    experiment = prefitE2
                    check = Error (MissingDataErr "no measured-data file is attached to this experiment")
                    changeOpt = None
                }
            ]

        Assert.Equal<Result<ExperimentPrefit list, StudyForwardError>>(Ok expected, prefitStudy.prefitWith context fitted)

        // Each check is the run's own pre-fit check over the context.
        for prefit in expected do
            Assert.Equal<Result<MappedPoints, PrefitError>>(
                prefit.experiment.prefitCheckWith context.library context.experimentData context.kit.setups,
                prefit.check)

        // Before the study's first fit, E1's data is unchanged.
        match prefitStudy.prefitWith context [] with
        | Ok (first :: _) -> Assert.Equal<DataChangeFlag option>(Some DataUnchangedFlag, first.changeOpt)
        | other -> Assert.Fail($"expected E1 first, got %A{other}")

        // E3 enabled again is read after E2, where the study holds it.
        let enabled = studyOf (prefitStudy.withExperimentEnablement e3 EnabledInStudy)

        Assert.Equal<ExperimentName list>(
            [ e1; k1; e2; e3 ],
            namesOfPrefits (enabled.prefitWith (prefitContextOver enabled prefitRuns prefitStatuses) fitted))

    [<Fact>]
    let ``a study naming an experiment the kit lacks, or a data status the kit cannot read, is refused`` () : unit =
        // E9 was in the kit the study was made over, and the context's kit lacks it.
        let withE9 = studyOf (Study.create (StudyName "Prefit E9") [ e1; e9; k1 ] [] (prefitRuns @ [ prefitRun (ExperimentId 17) e9 ScienceRun ]))

        Assert.Equal<Result<ExperimentPrefit list, StudyForwardError>>(
            Error (MissingStudyRunErr "E9, which the study 'Prefit E9' names"),
            withE9.prefitWith (prefitContextOver withE9 prefitRuns prefitStatuses) [])

        // K1's status cannot be read.
        let unreadable = KitIoErr "the test cannot hash 'data/K1.csv'"

        Assert.Equal<Result<ExperimentPrefit list, StudyForwardError>>(
            Error (StudyRunKitErr unreadable),
            prefitStudy.prefitWith (prefitContextOver prefitStudy prefitRuns (prefitStatuses |> Map.add k1File (Error unreadable))) [])

        // The status of a run no fit reads is never asked: X1's unreadable status refuses nothing.
        Assert.Equal<ExperimentName list>(
            [ e1; k1; e2 ],
            namesOfPrefits (prefitStudy.prefitWith (prefitContextOver prefitStudy prefitRuns (prefitStatuses |> Map.add x1File (Error unreadable))) []))

    let private e4 : ExperimentName = ExperimentName "E4"
    let private k2 : ExperimentName = ExperimentName "K2"
    let private session3 : SessionName = SessionName "Session 3"

    let private e2File : KitRelativePath = KitRelativePath "data/E2.csv"
    let private e4File : KitRelativePath = KitRelativePath "data/E4.csv"
    let private k2File : KitRelativePath = KitRelativePath "data/K2.csv"

    /// The study's data from its pre-fit over a kit of these runs, each attached file read as `points` and unchanged.
    let private studyDataOver (study : Study) (runs : Experiment list) (points : Map<KitRelativePath, MappedPoints>) : Result<StudyData, StudyForwardError> =
        let statuses : Map<KitRelativePath, Result<DataAttachmentStatus, KitError>> = points |> Map.map (fun _ _ -> Ok AttachedDataUnchanged)
        let context = contextOverData study runs (prefitDataOver statuses points)
        study.prefitWith context [] |> Result.bind (study.dataWith context)

    let private dataOf (result : Result<StudyData, StudyForwardError>) : StudyData =
        match result with
        | Ok data -> data
        | Error error -> failwith $"expected the study's data, got {error.describe}"

    /// An `Intensity series` point of LP2's turn as the Malus scan places it: its R1 setting and its signal.
    let private malusRow (degrees : float) (signal : float) : MeasuredRow =
        SettingsRow ([ settingOn lp2Turn (SweptValue degrees) ], None, RecordedSignal (MeasuredIntensity signal))

    [<Fact>]
    let ``a study's data holds each fitted run's kept rows in study order`` () : unit =
        // E1 holds three points, the second excluded; E2 is attached too, so every check passes.
        let threePoints =
            seriesOver
                [
                    intensityAt (MeasuredAbscissa 0.0) (MeasuredIntensity 1.0)
                    intensityAt (MeasuredAbscissa 45.0) (MeasuredIntensity 0.5)
                    intensityAt (MeasuredAbscissa 90.0) (MeasuredIntensity 0.0)
                ]

        let exclusion : PointExclusion = { point = MappedPointNumber 2; reason = ExclusionReason "the beam was blocked" }

        let keptE1 : Experiment =
            match prefitE1.dataOpt with
            | Some data -> { prefitE1 with dataOpt = Some { data with exclusions = [ exclusion ] } }
            | None -> failwith "step 377's E1 is attached"

        let attachedE2 = prefitE2 |> attachingAt e2File
        let runs = [ prefitP1; prefitE3; attachedE2; prefitX1; prefitK1; keptE1 ]
        let points = prefitPoints |> Map.add e1File threePoints |> Map.add e2File malusPoints
        let data = dataOf (studyDataOver prefitStudy runs points)

        // E1's two kept rows, then E2's four, in study order though the kit holds E2 first.
        let expected : StudyRun list =
            [
                { experiment = keptE1; rows = [ malusRow 0.0 1.0; malusRow 90.0 0.0 ] }
                { experiment = attachedE2; rows = [ malusRow 0.0 1.0; malusRow 30.0 0.75; malusRow 60.0 0.25; malusRow 90.0 0.0 ] }
            ]

        Assert.Equal<StudyRun list>(expected, data.runs)

        // The Dark K1 gives its session's dark level, never residual rows; the Diagnostic, disabled and Planned runs nothing.
        Assert.Equal<ExperimentName list>([ e1; e2 ], data.runs |> List.map (fun run -> run.experiment.name))

    /// K1's two dark readings, 0.2 and 0.4.
    let private k1Signals : MappedPoints =
        seriesOver [ intensityAt (MeasuredAbscissa 0.0) (MeasuredIntensity 0.2); intensityAt (MeasuredAbscissa 0.0) (MeasuredIntensity 0.4) ]

    let private sessionE1 : Experiment = { prefitE1 with sessionOpt = Some session1 }
    let private sessionK1 : Experiment = { prefitK1 with sessionOpt = Some session1 }

    /// E4, a Science run of Malus data in `Session 2`, which holds no Dark run.
    let private sessionE4 : Experiment = { prefitRun (ExperimentId 18) e4 ScienceRun with sessionOpt = Some session2 } |> attachingAt e4File

    /// K2, a Dark run of one reading 0.9, alone in `Session 3`.
    let private sessionK2 : Experiment =
        { prefitRun (ExperimentId 19) k2 DarkRun with scan = darkLine; sessionOpt = Some session3 } |> attachingAt k2File

    let private sessionPoints : Map<KitRelativePath, MappedPoints> =
        Map.ofList
            [
                e1File, malusPoints
                k1File, k1Signals
                e4File, malusPoints
                k2File, seriesOver [ intensityAt (MeasuredAbscissa 0.0) (MeasuredIntensity 0.9) ]
            ]

    let private assertDark (session : SessionName) (expected : float) (data : StudyData) : unit =
        match data.darkOf session with
        | Some dark -> Assert.Equal(expected, dark.value, 1e-12)
        | None -> Assert.Fail($"expected a dark level for '{session.value}'")

    let private darkSessionsOf (data : StudyData) : SessionName list = data.darks |> List.map (fun held -> held.session)

    [<Fact>]
    let ``a session's dark level is the mean signal of its own dark runs, else of every dark run, and none without a dark run`` () : unit =
        let runs = [ sessionE1; sessionK1; sessionE4 ]
        let darkStudy = studyOf (Study.create (StudyName "Darks") [ e1; k1; e4 ] [] runs)
        let data = dataOf (studyDataOver darkStudy runs sessionPoints)

        // Session 1 reads its own K1; Session 2 holds no Dark run and reads every one the fit reads, which is K1.
        Assert.Equal<SessionName list>([ session1; session2 ], darkSessionsOf data)
        assertDark session1 0.3 data
        assertDark session2 0.3 data

        // With K2 in Session 3, Session 1 still reads only its own K1, and Session 2 every Dark row alike:
        // (0.2 + 0.4 + 0.9) / 3, not the mean of the runs' means. Session 3 lists no fitted run and takes no level.
        let withK2 = runs @ [ sessionK2 ]
        let twoDarkStudy = studyOf (Study.create (StudyName "Two darks") [ e1; k1; e4; k2 ] [] withK2)
        let twoDark = dataOf (studyDataOver twoDarkStudy withK2 sessionPoints)

        Assert.Equal<SessionName list>([ session1; session2 ], darkSessionsOf twoDark)
        assertDark session1 0.3 twoDark
        assertDark session2 0.5 twoDark
        Assert.Equal<DarkMean option>(None, twoDark.darkOf session3)

        // K1 disabled, the fit reads no Dark run: no session has a dark level, and E1 and E4 are still fitted.
        let noDarkStudy = studyOf (darkStudy.withExperimentEnablement k1 DisabledInStudy)
        let noDark = dataOf (studyDataOver noDarkStudy runs sessionPoints)

        Assert.Equal<SessionDark list>([], noDark.darks)
        Assert.Equal<DarkMean option>(None, noDark.darkOf session1)
        Assert.Equal<DarkMean option>(None, noDark.darkOf session2)
        Assert.Equal<ExperimentName list>([ e1; e4 ], noDark.runs |> List.map (fun run -> run.experiment.name))

    [<Fact>]
    let ``a failed pre-fit check or a dark run that records no signal is refused`` () : unit =
        let context = prefitContextOver prefitStudy prefitRuns prefitStatuses
        let missing = MissingDataErr "no measured-data file is attached to this experiment"

        // Step 377's E2 has no data: its failed check answers, naming it.
        Assert.Equal<Result<StudyData, StudyForwardError>>(
            Error (PrefitFailedErr (e2, missing)),
            prefitStudy.prefitWith context [] |> Result.bind (prefitStudy.dataWith context))

        let readingK1As (points : MappedPoints) : ExperimentPrefit list =
            [
                { experiment = prefitE1; check = Ok malusPoints; changeOpt = Some DataUnchangedFlag }
                { experiment = prefitK1; check = Ok points; changeOpt = Some DataUnchangedFlag }
            ]

        let refusalOf (points : MappedPoints) : Result<StudyData, StudyForwardError> = prefitStudy.dataWith context (readingK1As points)

        let psiDelta : EllipsometricPoint = { waveLength = nanometres 632.8; psi = deg 30.0; delta = deg 90.0; aoiOpt = None }
        let observation : KeyedObservation = { key = DataRowKey "K1"; observation = { configuration = xCutConfiguration; measured = identityMueller } }

        // A Dark run of Ψ and Δ, of Mueller matrices or of Mueller observations records no signal, nor does one keeping no point.
        Assert.Equal<Result<StudyData, StudyForwardError>>(
            Error (DarkReadingErr "K1 records Ψ and Δ, not a signal"),
            refusalOf (SeriesPoints (EllipsometricData { points = [ psiDelta ] })))

        Assert.Equal<Result<StudyData, StudyForwardError>>(
            Error (DarkReadingErr "K1 records a Mueller matrix, not a signal"),
            refusalOf (ColumnPoints [ columnRow [] (MuellerReading identityMueller) ]))

        Assert.Equal<Result<StudyData, StudyForwardError>>(
            Error (DarkReadingErr "K1 records a Mueller observation, not a signal"),
            refusalOf (ObservationPoints [ observation ]))

        Assert.Equal<Result<StudyData, StudyForwardError>>(Error (DarkReadingErr "K1 keeps no point"), refusalOf (seriesOver []))

        // A failed check answers before any data is read, even after a Dark run of no signal.
        let e2Failed : ExperimentPrefit = { experiment = prefitE2; check = Error missing; changeOpt = None }

        Assert.Equal<Result<StudyData, StudyForwardError>>(
            Error (PrefitFailedErr (e2, missing)),
            prefitStudy.dataWith context (readingK1As (SeriesPoints (EllipsometricData { points = [ psiDelta ] })) @ [ e2Failed ]))
