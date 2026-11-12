namespace OpticalConstructor.Tests

open System
open System.Text
open Xunit
open Softellect.Sys.Primitives
open Berreman.Constants
open Berreman.Fields
open Berreman.Geometry
open Berreman.Media
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.MeasuredData
open OpticalConstructor.Domain.MuellerReconstruction
open OpticalConstructor.Domain.MuellerInverse
open OpticalConstructor.Domain.Experiments
open OpticalConstructor.Domain.ImportProfiles

/// The built-in import profiles, the profile an experiment names, the points a profile maps a file to, the wizard's one
/// rule from column choices to roles, the one validation of mapped points and an experiment's rows of them
/// (spec 0050 S9.2).
module ImportProfileTests =

    let private scanCellOf (valueUnit : ScanColumnUnit) (magnitude : Units.UnitMagnitude) : ScanCell =
        let column : ScanColumn =
            {
                column = DataColumnNumber 1
                valueUnit = valueUnit
                reading = PhysicalScanValue
            }

        { column = column; magnitude = magnitude }

    let private describe (result : Result<ColumnRoles, ImportProfileError>) : string =
        match result with
        | Ok _ -> "the choices were accepted"
        | Error error -> error.describe

    /// `KitTests.labScanProfile`'s roles, one choice per column, given in reverse column order.
    let private labScanChoices : ColumnChoice list =
        [
            { column = DataColumnNumber 5; role = UnmappedColumnRole }
            { column = DataColumnNumber 4; role = KeyColumnRole }
            { column = DataColumnNumber 3; role = SigmaColumnRole }
            { column = DataColumnNumber 2; role = SignalColumnRole }
            { column = DataColumnNumber 1; role = ScanColumnRole (DegreeColumnUnit, PhysicalScanValue) }
        ]

    [<Fact>]
    let ``the built-in import profiles are named once, and a built-in is found before a kit profile`` () : unit =
        let labScan = KitTests.labScanProfile
        let phoneRoiImpostor = { labScan with name = ImportProfileName "Phone ROI CSV" }

        Assert.Equal<ImportProfileName list>(
            ImportProfileName.builtIns,
            BuiltInImportProfile.all |> List.map (fun (profile : BuiltInImportProfile) -> profile.name))

        for profile in BuiltInImportProfile.all do
            Assert.Equal<BuiltInImportProfile option>(Some profile, BuiltInImportProfile.tryOfName profile.name)

        Assert.Equal<BuiltInImportProfile option>(None, BuiltInImportProfile.tryOfName (ImportProfileName "Other"))

        let chosenLabScan = ChosenImportProfile.tryOfName [ labScan ] (ImportProfileName "Lab scan")
        Assert.Equal<ChosenImportProfile option>(Some (KitProfileChoice labScan), chosenLabScan)

        let chosenPhoneRoi = ChosenImportProfile.tryOfName [ phoneRoiImpostor ] (ImportProfileName "Phone ROI CSV")
        Assert.Equal<ChosenImportProfile option>(Some (BuiltInProfileChoice PhoneRoiCsvProfile), chosenPhoneRoi)

        Assert.Equal<ChosenImportProfile option>(None, ChosenImportProfile.tryOfName [ labScan ] (ImportProfileName "Nope"))
        Assert.Equal(ImportProfileName "Lab scan", (KitProfileChoice labScan).name)
        Assert.Equal(ImportProfileName.phoneRoiCsv, (BuiltInProfileChoice PhoneRoiCsvProfile).name)

    [<Fact>]
    let ``the observable and the cut carry disk tags, and a lab's observable cell reads T, R or the tag`` () : unit =
        let observables : MuellerInverse.Observable list = [ TransmittedMueller; ReflectedMueller ]
        let tilted = TiltedCut (Angle 0.3)
        let cuts : SampleCut list = [ ZCut; XCut; YCut; tilted ]

        Assert.Equal<int list>([ 1; 2 ], observables |> List.map (fun (observable : MuellerInverse.Observable) -> observable.value))

        for observable in observables do
            Assert.Equal<MuellerInverse.Observable option>(Some observable, MuellerInverse.Observable.tryOfWire observable.value)

        Assert.Equal<MuellerInverse.Observable option>(None, MuellerInverse.Observable.tryOfWire 3)

        Assert.Equal<MuellerInverse.Observable option list>(
            [ Some TransmittedMueller; Some ReflectedMueller; Some ReflectedMueller; None ],
            [ DataFileCell " t "; DataFileCell "R"; DataFileCell "2"; DataFileCell "x" ] |> List.map MuellerInverse.Observable.tryOfCell)

        Assert.Equal<int list>([ 1; 2; 3; 4 ], cuts |> List.map (fun (cut : SampleCut) -> cut.value))
        Assert.Equal(Angle 0.3, tilted.tilt)

        for cut in cuts do
            Assert.Equal<SampleCut option>(Some cut, SampleCut.tryOfWire cut.value cut.tilt)

        Assert.Equal<SampleCut option>(Some (TiltedCut (Angle 0.3)), SampleCut.tryOfWire 4 (Angle 0.3))
        Assert.Equal<SampleCut option>(Some ZCut, SampleCut.tryOfWire 1 Angle.zero)
        Assert.Equal<SampleCut option>(None, SampleCut.tryOfWire 1 (Angle 0.1))

    [<Fact>]
    let ``a scan cell gives its value in its quantity's display unit`` () : unit =
        let degrees = scanCellOf DegreeColumnUnit (Units.UnitMagnitude 45.0)
        let nanometres = scanCellOf NanometerColumnUnit (Units.UnitMagnitude 632.8)
        let radians = scanCellOf RadianColumnUnit (Units.UnitMagnitude (Math.PI / 4.0))
        let micrometres = scanCellOf MicrometerColumnUnit (Units.UnitMagnitude 0.6328)

        Assert.Equal<Propagation.SweptValue>(Propagation.SweptValue 45.0, degrees.displayValue)
        Assert.Equal<Propagation.SweptValue>(Propagation.SweptValue 632.8, nanometres.displayValue)
        Assert.Equal(45.0, radians.displayValue.value, 9)
        Assert.Equal(632.8, micrometres.displayValue.value, 9)

    [<Fact>]
    let ``a column mapping's roles come from one choice per column`` () : unit =
        let m00 = MuellerElementIndex 0
        let m12 = MuellerElementIndex 6
        let elementColumns : DataColumnNumber list = [ for number in 20 .. -1 .. 5 -> DataColumnNumber number ]
        let throughput = DataColumnNumber 21

        let muellerChoices : ColumnChoice list =
            List.map2 (fun index column -> { column = column; role = MuellerElementRole index }) MuellerElementIndex.all elementColumns
            @ [ { column = throughput; role = ThroughputColumnRole } ]

        let muellerRoles : ColumnRoles =
            {
                scan = []
                reading = MuellerElementColumns (elementColumns, Some throughput)
                timestampOpt = None
                captureIndexOpt = None
                observableOpt = None
                keyOpt = None
            }

        let scanChoice : ColumnChoice = { column = DataColumnNumber 1; role = ScanColumnRole (DegreeColumnUnit, PhysicalScanValue) }

        Assert.Equal("m12", m12.text)
        Assert.Equal<MuellerElementIndex option>(Some (MuellerElementIndex 15), MuellerElementIndex.tryCreate 15)
        Assert.Equal<MuellerElementIndex option>(None, MuellerElementIndex.tryCreate 16)
        Assert.Equal<MuellerElementIndex option>(None, MuellerElementIndex.tryCreate -1)

        Assert.Equal<Result<ColumnRoles, ImportProfileError>>(Ok KitTests.labScanProfile.roles, ColumnRoles.tryOfChoices labScanChoices)
        Assert.Equal<Result<ColumnRoles, ImportProfileError>>(Ok muellerRoles, ColumnRoles.tryOfChoices muellerChoices)

        let noSignal = ColumnRoles.tryOfChoices [ scanChoice; { column = DataColumnNumber 3; role = SigmaColumnRole } ]

        match noSignal with
        | Error (SignalColumnCountErr _) -> ()
        | other -> Assert.Fail($"no signal: expected SignalColumnCountErr: {describe other}")

        let twoSignals =
            ColumnRoles.tryOfChoices
                [
                    scanChoice
                    { column = DataColumnNumber 2; role = SignalColumnRole }
                    { column = DataColumnNumber 3; role = SignalColumnRole }
                ]

        match twoSignals with
        | Error (SignalColumnCountErr _) -> ()
        | other -> Assert.Fail($"two signals: expected SignalColumnCountErr: {describe other}")

        let signalBesideElement =
            ColumnRoles.tryOfChoices
                [
                    { column = DataColumnNumber 1; role = SignalColumnRole }
                    { column = DataColumnNumber 2; role = MuellerElementRole m00 }
                ]

        match signalBesideElement with
        | Error (MixedReadingColumnsErr _) -> ()
        | other -> Assert.Fail($"a signal beside m00: expected MixedReadingColumnsErr: {describe other}")

        match ColumnRoles.tryOfChoices (muellerChoices |> List.filter (fun choice -> choice.role <> MuellerElementRole m12)) with
        | Error (MuellerElementCountErr reason) -> Assert.Contains("m12", reason)
        | other -> Assert.Fail($"no m12: expected MuellerElementCountErr: {describe other}")

        match ColumnRoles.tryOfChoices (labScanChoices @ [ { column = DataColumnNumber 6; role = KeyColumnRole } ]) with
        | Error (RepeatedRoleErr reason) ->
            Assert.Contains("experiment key", reason)
            Assert.Contains("4", reason)
            Assert.Contains("6", reason)
        | other -> Assert.Fail($"two key columns: expected RepeatedRoleErr: {describe other}")

    /// A Phone ROI capture row of one family at the analyzer azimuth (spec 0050 S9.2).
    let private captureAt (index : CaptureIndex) (azimuth : Angle) : MuellerRawRow =
        {
            experiment = MuellerExperimentLabel "CPL-(QZ#90-LR#90)-CPL"
            captureIndex = index
            capturedAt = DateTimeOffset(2026, 11, 3, 12, 0, 0, TimeSpan.Zero)
            description = azimuth
            avgTotal = MeasuredIntensity 0.5
        }

    /// Phone ROI capture rows labelled in order, numbered from 1, the n-th at the analyzer azimuth 45° × (n − 1)
    /// (spec 0050 S9.3).
    let capturesLabelled (labels : MuellerExperimentLabel list) : MuellerRawRow list =
        labels
        |> List.mapi (fun index label ->
            { captureAt (CaptureIndex (index + 1)) (Angle.degree (45.0<deg> * float index)) with experiment = label })

    /// A Z-cut plate's transmitted Mueller matrix at normal incidence, measured as the identity (spec 0050 S9.2).
    let observationOf (key : DataRowKey) : KeyedObservation =
        let configuration : MeasurementConfiguration =
            {
                cut = ZCut
                thickness = Thickness.mm 1.0<mm>
                incidenceAngle = IncidenceAngle.normal
                azimuth = SampleAzimuth.degree 0.0<deg>
                observable = TransmittedMueller
                waveLength = WaveLength.nm 600.0<nm>
            }

        let observation : MuellerObservation = { configuration = configuration; measured = Propagation.identityMueller }
        { key = key; observation = observation }

    /// Step 300's two-point intensity series, 0 → 1.0 and 90 → 0.0.
    let twoPointIntensity : IntensitySeries =
        {
            points =
                [
                    { x = MeasuredAbscissa 0.0; intensity = MeasuredIntensity 1.0 }
                    { x = MeasuredAbscissa 90.0; intensity = MeasuredIntensity 0.0 }
                ]
        }

    /// Step 300's three Phone ROI capture rows, at 0, 45 and 90°.
    let private threeCaptures : MuellerRawRow list =
        [
            captureAt (CaptureIndex 1) (Angle.degree 0.0<deg>)
            captureAt (CaptureIndex 2) (Angle.degree 45.0<deg>)
            captureAt (CaptureIndex 3) (Angle.degree 90.0<deg>)
        ]

    /// Step 300's three keyed observations, QZ, LR and QZ+LR.
    let private threeObservations : KeyedObservation list =
        [ observationOf (DataRowKey "QZ"); observationOf (DataRowKey "LR"); observationOf (DataRowKey "QZ+LR") ]

    /// Step 300's three mapped rows of key QZ at 45°: a signal with its σ, a signal without one, and a Mueller matrix.
    let private threeMappedRows : MappedRow list =
        let rowOf (reading : MappedReading) : MappedRow =
            {
                keyOpt = Some (DataRowKey "QZ")
                scan = [ scanCellOf DegreeColumnUnit (Units.UnitMagnitude 45.0) ]
                reading = reading
                timestampOpt = Some (RowTimestamp (DateTimeOffset(2026, 11, 3, 12, 0, 0, TimeSpan.Zero)))
                captureIndexOpt = Some (CaptureIndex 1)
                observableOpt = Some TransmittedMueller
            }

        [
            rowOf (SignalReading (MeasuredIntensity 1.0, Some (MeasuredSigma 0.01)))
            rowOf (SignalReading (MeasuredIntensity 0.5, None))
            rowOf (MuellerReading Propagation.identityMueller)
        ]

    [<Fact>]
    let ``mapped points count what they hold`` () : unit =
        Assert.Equal(MeasuredPointCount 2, (SeriesPoints (IntensityData twoPointIntensity)).pointCount)
        Assert.Equal(MeasuredPointCount 3, (CapturePoints threeCaptures).pointCount)
        Assert.Equal(MeasuredPointCount 3, (ObservationPoints threeObservations).pointCount)
        Assert.Equal(MeasuredPointCount 3, (ColumnPoints threeMappedRows).pointCount)

    // ============================ spec 0050 S9.3: the points an attachment's exclusions keep ============================

    [<Fact>]
    let ``kept points leave out the excluded ones and keep the rest in order`` () : unit =
        let excludedAt (point : MappedPointNumber) : PointExclusion = { point = point; reason = ExclusionReason "beam blocked" }
        let second = [ excludedAt (MappedPointNumber 2) ]
        let ninth = [ excludedAt (MappedPointNumber 9) ]

        let series = SeriesPoints (IntensityData twoPointIntensity)

        let ellipsometric =
            let pointAt (nanometres : float<nm>) : EllipsometricPoint =
                { waveLength = WaveLength.nm nanometres; psi = Angle.degree 12.0<deg>; delta = Angle.degree 30.0<deg>; aoiOpt = None }

            SeriesPoints (EllipsometricData { points = [ pointAt 400.0<nm>; pointAt 500.0<nm>; pointAt 600.0<nm> ] })

        let captures = CapturePoints threeCaptures
        let observations = ObservationPoints threeObservations
        let rows = ColumnPoints threeMappedRows

        // Excluding point 2 keeps points 1 and 3 of each list in order, and a series keeps its kind over what it keeps.
        Assert.Equal<MappedPoints>(SeriesPoints (IntensityData { points = [ List.head twoPointIntensity.points ] }), series.keptBy second)

        match ellipsometric, ellipsometric.keptBy second with
        | SeriesPoints (EllipsometricData whole), SeriesPoints (EllipsometricData kept) ->
            Assert.Equal<EllipsometricPoint list>([ whole.points[0]; whole.points[2] ], kept.points)
        | _, other -> Assert.Fail($"expected the ellipsometric series' points 1 and 3, got %A{other}")

        Assert.Equal<MappedPoints>(CapturePoints [ threeCaptures[0]; threeCaptures[2] ], captures.keptBy second)
        Assert.Equal<MappedPoints>(ObservationPoints [ threeObservations[0]; threeObservations[2] ], observations.keptBy second)
        Assert.Equal<MappedPoints>(ColumnPoints [ threeMappedRows[0]; threeMappedRows[2] ], rows.keptBy second)

        // Every point but the first excluded keeps only the first.
        let allButFirst = [ excludedAt (MappedPointNumber 3); excludedAt (MappedPointNumber 2) ]
        Assert.Equal<MappedPoints>(CapturePoints [ threeCaptures[0] ], captures.keptBy allButFirst)

        // No exclusion keeps each whole, and an exclusion of point 9 changes nothing.
        for points in [ series; ellipsometric; captures; observations; rows ] do
            Assert.Equal<MappedPoints>(points, points.keptBy [])
            Assert.Equal<MappedPoints>(points, points.keptBy ninth)

    // ============================ spec 0050 S9.2: mapped points against the scan and detector ============================

    // Step 287's test-local slot a scan varies.
    let private sample : Library.ElementId = Library.ElementId "sample"

    /// The plate's four azimuths 0…67.5°, A1's one axis (001 §Q AT-2).
    let private plateAzimuths : ScanAxis =
        {
            elementId = sample
            label = ElementLabel "Sample #1"
            variable = VaryR1
            range =
                {
                    min = Propagation.SweptValue 0.0
                    max = Propagation.SweptValue 67.5
                    points = Propagation.CurvePointCount 4
                }
        }

    /// The plate's incidence over 20…60° in 3 points, A2's outer axis.
    let private plateIncidence : ScanAxis =
        {
            plateAzimuths with
                variable = VaryR2
                range =
                    {
                        min = Propagation.SweptValue 20.0
                        max = Propagation.SweptValue 60.0
                        points = Propagation.CurvePointCount 3
                    }
        }

    /// The plate's azimuth over 0…90° in 2 points, A2's inner axis.
    let private plateAzimuthPair : ScanAxis =
        {
            plateAzimuths with
                range =
                    {
                        min = Propagation.SweptValue 0.0
                        max = Propagation.SweptValue 90.0
                        points = Propagation.CurvePointCount 2
                    }
        }

    /// A1's azimuths read in T, then A2's incidence × azimuth read in T and R: the sample's R1, then its R2.
    let private plateScan : ExperimentScan =
        {
            blocks =
                [
                    { axes = [ plateAzimuths ]; capture = CaptureTransmitted }
                    { axes = [ plateIncidence; plateAzimuthPair ]; capture = CaptureBoth }
                ]
            sourceWaveLengthOpt = None
        }

    let private scanOf (axis : ScanAxis) : ExperimentScan = ExperimentScan.oneAxis axis CaptureTransmitted None

    /// The sample's R1 over the whole turn, 0…360°.
    let private turnScan : ExperimentScan = scanOf { plateAzimuths with range = VariableRange.forVariable VaryR1 }

    let private darkLine : ExperimentScan = ExperimentScan.darkLine CaptureTransmitted

    /// Compares two validations, printing both in full on a mismatch.
    let private assertValidation (expected : Result<MappedPoints, ExperimentDataError>) (actual : Result<MappedPoints, ExperimentDataError>) : unit =
        if expected <> actual then
            Assert.Fail($"expected %A{expected}, got %A{actual}")

    [<Fact>]
    let ``a scan's quantities join the axes that vary one slot's variable`` () : unit =
        let azimuth : ScanQuantity =
            {
                elementId = sample
                label = ElementLabel "Sample #1"
                variable = VaryR1
                ranges = [ plateAzimuths.range; plateAzimuthPair.range ]
            }

        let incidence : ScanQuantity =
            {
                azimuth with
                    variable = VaryR2
                    ranges = [ plateIncidence.range ]
            }

        Assert.Equal<ScanQuantity list>([ azimuth; incidence ], plateScan.quantities)
        Assert.Equal<ScanQuantity list>([], darkLine.quantities)

        // The first axis names its quantity, and another slot's R1 is another quantity.
        let analyzerTurn : ScanAxis =
            {
                plateAzimuthPair with
                    elementId = Library.ElementId "analyzer"
                    label = ElementLabel "Linear polarizer #2"
            }

        let relabelled : ExperimentScan =
            {
                plateScan with
                    blocks =
                        [
                            { axes = [ plateAzimuths ]; capture = CaptureTransmitted }
                            { axes = [ { plateAzimuthPair with label = ElementLabel "Plate" }; analyzerTurn ]; capture = CaptureTransmitted }
                        ]
            }

        let analyzer : ScanQuantity =
            {
                azimuth with
                    elementId = analyzerTurn.elementId
                    label = analyzerTurn.label
                    ranges = [ analyzerTurn.range ]
            }

        Assert.Equal<ScanQuantity list>([ azimuth; analyzer ], relabelled.quantities)

        // Some range admits a value: 80° lies in 0…90° only.
        Assert.True(azimuth.admits 80.0)
        Assert.False(azimuth.admits 95.0)
        Assert.False(incidence.admits 10.0)

    [<Fact>]
    let ``mapped capture rows and column rows are validated against the experiment's scan and detector`` () : unit =
        let captures = CapturePoints [ captureAt (CaptureIndex 1) (Angle.degree 0.0<deg>); captureAt (CaptureIndex 2) (Angle.degree 45.0<deg>) ]

        let upTo40 : ExperimentScan =
            scanOf
                {
                    plateAzimuths with
                        range =
                            {
                                min = Propagation.SweptValue 0.0
                                max = Propagation.SweptValue 40.0
                                points = Propagation.CurvePointCount 3
                            }
                }

        // Phone ROI rows: one quantity ranges their descriptions, and a dark line has none to range.
        assertValidation (Ok captures) (captures.validatedFor Library.Intensity turnScan)
        assertValidation (Ok captures) (captures.validatedFor Library.Intensity darkLine)

        assertValidation
            (Error (DataRangeMismatchErr "a description 45° lies outside every range of Sample #1 Rotation R1"))
            (captures.validatedFor Library.Intensity upTo40)

        assertValidation
            (Error (DataRangeMismatchErr "a Phone ROI CSV holds one variable (its description), and the scan varies 2"))
            (captures.validatedFor Library.Intensity plateScan)

        assertValidation
            (Error (DataUnitsMismatchErr "the detector is an ellipsometer (records Ψ/Δ), but the data file is a Phone ROI CSV of intensities"))
            (captures.validatedFor Library.Ellipsometer turnScan)

        // `KitTests.labScanProfile`'s two rows at 0° and 20°, their one scan column in the unit and reading given.
        let labScanIn (valueUnit : ScanColumnUnit) (reading : ScanReading) : MappedPoints =
            let column : ScanColumn = { column = DataColumnNumber 1; valueUnit = valueUnit; reading = reading }

            let rowAt (key : DataRowKey) (magnitude : Units.UnitMagnitude) (signal : MeasuredIntensity) : MappedRow =
                {
                    keyOpt = Some key
                    scan = [ { column = column; magnitude = magnitude } ]
                    reading = SignalReading (signal, Some (MeasuredSigma 0.01))
                    timestampOpt = None
                    captureIndexOpt = None
                    observableOpt = None
                }

            ColumnPoints
                [
                    rowAt (DataRowKey "CPL-(QZ#45)-CPL") (Units.UnitMagnitude 0.0) (MeasuredIntensity 0.5)
                    rowAt (DataRowKey "LP-(AIR#0)-LP") (Units.UnitMagnitude 20.0) (MeasuredIntensity 0.25)
                ]

        let labScan = labScanIn DegreeColumnUnit PhysicalScanValue
        let dialScan = scanOf { plateAzimuths with variable = VaryDialReading; range = VariableRange.forVariable VaryDialReading }

        let waveLengthScan =
            scanOf
                {
                    elementId = Library.ElementId "source"
                    label = ElementLabel "Light source #1"
                    variable = VaryWaveLength
                    range = VariableRange.forVariable VaryWaveLength
                }

        let assertUnitsMismatch (scan : ExperimentScan) (points : MappedPoints) (quantity : string) : unit =
            match points.validatedFor Library.Intensity scan with
            | Error (DataUnitsMismatchErr reason) ->
                Assert.Contains("column 1", reason)
                Assert.Contains(quantity, reason)
            | other -> Assert.Fail($"{quantity}: expected DataUnitsMismatchErr, got %A{other}")

        assertValidation (Ok labScan) (labScan.validatedFor Library.Intensity turnScan)
        assertUnitsMismatch dialScan labScan "Sample #1 Dial reading"
        assertUnitsMismatch waveLengthScan labScan "Light source #1 Wavelength"

        assertValidation
            (Error (DataUnitsMismatchErr "the import profile maps 1 scan column(s), and the scan varies 2"))
            (labScan.validatedFor Library.Intensity plateScan)

        assertValidation
            (Error (DataUnitsMismatchErr "the detector is an ellipsometer (records Ψ/Δ), but the mapping reads a signal"))
            (labScan.validatedFor Library.Ellipsometer turnScan)

        // A raw dial column reads a dial alone, a spectral column a wavelength alone, and every value is ranged.
        let rawDial = labScanIn DegreeColumnUnit RawDialReading
        let nanometres = labScanIn NanometerColumnUnit PhysicalScanValue
        assertValidation (Ok rawDial) (rawDial.validatedFor Library.Intensity dialScan)
        assertUnitsMismatch turnScan rawDial "Sample #1 Rotation R1"
        assertUnitsMismatch turnScan nanometres "Sample #1 Rotation R1"
        assertValidation (Ok labScan) (labScan.validatedFor Library.Intensity upTo40)

        match nanometres.validatedFor Library.Intensity waveLengthScan with
        | Error (DataRangeMismatchErr reason) ->
            Assert.Contains("0 nm", reason)
            Assert.Contains("Light source #1 Wavelength", reason)
        | other -> Assert.Fail($"0 nm: expected DataRangeMismatchErr, got %A{other}")

        match labScan.validatedFor Library.Intensity (scanOf plateIncidence) with
        | Error (DataRangeMismatchErr reason) ->
            Assert.Contains("0 °", reason)
            Assert.Contains("Sample #1 Incidence R2", reason)
        | other -> Assert.Fail($"0° below 20…60°: expected DataRangeMismatchErr, got %A{other}")

        // Keyed observations carry their own configurations and pass as read.
        let observations = ObservationPoints [ observationOf (DataRowKey "QZ"); observationOf (DataRowKey "LR") ]

        for detector in [ Library.Intensity; Library.Ellipsometer ] do
            for scan in [ turnScan; darkLine; plateScan; waveLengthScan ] do
                assertValidation (Ok observations) (observations.validatedFor detector scan)

    // ============================ spec 0050 S9.2: an experiment's rows of a data file ============================

    /// A `Lab scan` row, one scan column and a signal with its σ (spec 0050 S9.2).
    let labScanRow (keyOpt : DataRowKey option) (magnitude : Units.UnitMagnitude) : MappedRow =
        {
            keyOpt = keyOpt
            scan = [ scanCellOf DegreeColumnUnit magnitude ]
            reading = SignalReading (MeasuredIntensity 0.5, Some (MeasuredSigma 0.01))
            timestampOpt = None
            captureIndexOpt = None
            observableOpt = None
        }

    [<Fact>]
    let ``a row selection keeps every row or the rows of one key`` () : unit =
        let k1 = DataRowKey "K1"
        let k2 = DataRowKey "K2"
        let k3 = DataRowKey "K3"

        // Phone ROI rows labelled K1, K2, K1.
        let firstK1Capture = { captureAt (CaptureIndex 1) (Angle.degree 0.0<deg>) with experiment = MuellerExperimentLabel "K1" }
        let k2Capture = { captureAt (CaptureIndex 2) (Angle.degree 45.0<deg>) with experiment = MuellerExperimentLabel "K2" }
        let secondK1Capture = { captureAt (CaptureIndex 3) (Angle.degree 90.0<deg>) with experiment = MuellerExperimentLabel "K1" }
        let captures = CapturePoints [ firstK1Capture; k2Capture; secondK1Capture ]

        let k1Observation = observationOf k1
        let observations = ObservationPoints [ k1Observation; observationOf k2 ]

        let k1Row = labScanRow (Some k1) (Units.UnitMagnitude 0.0)
        let labRows = ColumnPoints [ k1Row; labScanRow (Some k2) (Units.UnitMagnitude 20.0) ]

        for points in [ captures; observations; labRows ] do
            assertValidation (Ok points) (points.selectedBy AllDataRows)

        assertValidation (Ok (CapturePoints [ firstK1Capture; secondK1Capture ])) (captures.selectedBy (KeyedDataRows k1))
        assertValidation (Ok (ObservationPoints [ k1Observation ])) (observations.selectedBy (KeyedDataRows k1))
        assertValidation (Ok (ColumnPoints [ k1Row ])) (labRows.selectedBy (KeyedDataRows k1))

        for points in [ captures; observations; labRows ] do
            assertValidation (Error (EmptyDataFileErr "no row of the data file holds the key 'K3'")) (points.selectedBy (KeyedDataRows k3))

        // An X,Y series holds no key column, and neither does a mapping whose rows hold no key.
        let series =
            SeriesPoints (IntensityData { points = [ { x = MeasuredAbscissa 0.0; intensity = MeasuredIntensity 1.0 } ] })

        assertValidation (Ok series) (series.selectedBy AllDataRows)
        assertValidation
            (Error (MalformedDataFileErr "an X,Y data file has no key column to select 'K1' by"))
            (series.selectedBy (KeyedDataRows k1))

        let unkeyed = ColumnPoints [ labScanRow None (Units.UnitMagnitude 0.0); labScanRow None (Units.UnitMagnitude 20.0) ]
        assertValidation
            (Error (MalformedDataFileErr "the import profile maps no key column to select 'K1' by"))
            (unkeyed.selectedBy (KeyedDataRows k1))

    // ============================ spec 0050 S9.3: the one reading of a point's key ============================

    [<Fact>]
    let ``a point's key is the one selection reads, and a selection keeps the points of its key`` () : unit =
        let k1 = DataRowKey "K1"
        let k2 = DataRowKey "K2"
        let k1Label = MuellerExperimentLabel "K1"

        let labelled = capturesLabelled [ k1Label; MuellerExperimentLabel "K2"; k1Label ]
        let captures = CapturePoints labelled
        let k1Observation = observationOf k1
        let observations = ObservationPoints [ k1Observation; observationOf k2 ]
        let k1Row = labScanRow (Some k1) (Units.UnitMagnitude 0.0)
        let labRows = ColumnPoints [ k1Row; labScanRow None (Units.UnitMagnitude 10.0); labScanRow (Some k2) (Units.UnitMagnitude 20.0) ]
        let series = SeriesPoints (IntensityData twoPointIntensity)

        Assert.Equal(k1, k1Label.rowKey)
        Assert.Equal<DataRowKey option list>([ Some k1; Some k2; Some k1 ], captures.pointKeys)
        Assert.Equal<DataRowKey option list>([ Some k1; Some k2 ], observations.pointKeys)
        Assert.Equal<DataRowKey option list>([ Some k1; None; Some k2 ], labRows.pointKeys)
        Assert.Equal<DataRowKey option list>([ None; None ], series.pointKeys)

        // The selection of K1 keeps exactly the points `pointKeys` names K1: two, one and one, as step 305's fact keeps them.
        assertValidation (Ok (CapturePoints [ labelled[0]; labelled[2] ])) (captures.selectedBy (KeyedDataRows k1))
        assertValidation (Ok (ObservationPoints [ k1Observation ])) (observations.selectedBy (KeyedDataRows k1))
        assertValidation (Ok (ColumnPoints [ k1Row ])) (labRows.selectedBy (KeyedDataRows k1))

        for points in [ captures; observations; labRows ] do
            let named = points.pointKeys |> List.filter (fun keyOpt -> keyOpt = Some k1)

            match points.selectedBy (KeyedDataRows k1) with
            | Ok selected ->
                Assert.Equal<DataRowKey option list>(named, selected.pointKeys)
                Assert.Equal(MeasuredPointCount named.Length, selected.pointCount)
            | Error error -> Assert.Fail($"the selection of K1 was refused: %A{error}")

    // ============================ spec 0050 S9.2: what a picked lab file suggests ============================

    /// A picked lab file whose bytes are `prefix`, then `text` in UTF-8.
    let private pickedLabFile (prefix : byte[]) (text : string) : LabFileSource =
        {
            file = FileName "C:/lab/picked.csv"
            content = Array.append prefix (Encoding.UTF8.GetBytes text) |> FileByteContent
        }

    let private sniffOf (bom : ByteOrderMark) (delimiter : DataDelimiter) (header : HeaderRowChoice) : LabFileSniff =
        { bom = bom; delimiter = delimiter; header = header }

    [<Fact>]
    let ``sniffing suggests the delimiter, the header and the byte-order mark of a lab file`` () : unit =
        let phoneRoi =
            pickedLabFile
                [| 0xEFuy; 0xBBuy; 0xBFuy |]
                ("experiment,capture_index,captured_at,description,avg_total\n"
                 + "CPL-(AIR#0)-CPL,0,2026-07-15T12:00:00+00:00,0,869.5\n"
                 + "CPL-(AIR#0)-CPL,1,2026-07-15T12:05:00+00:00,45,912.25\n")

        Assert.Equal<LabFileSniff>(sniffOf ByteOrderMarkPresent CommaDelimiter HeaderOnFirstRow, phoneRoi.sniffedLayout)

        // Ordinal: a culture-aware comparison ignores a leading U+FEFF, so it would pass a text that kept the mark.
        Assert.StartsWith("experiment,", phoneRoi.content.kitFileText.value, StringComparison.Ordinal)

        Assert.Equal<LabFileSniff>(
            sniffOf ByteOrderMarkAbsent SemicolonDelimiter NoHeaderRow, (pickedLabFile [||] "0;1.5\n20;1.25\n").sniffedLayout)

        Assert.Equal<LabFileSniff>(
            sniffOf ByteOrderMarkAbsent TabDelimiter HeaderOnFirstRow, (pickedLabFile [||] "phi\tI\n0\t0.5\n").sniffedLayout)

        Assert.Equal<LabFileSniff>(
            sniffOf ByteOrderMarkAbsent CommaDelimiter HeaderOnFirstRow, (pickedLabFile [||] "one column\n").sniffedLayout)

        // A header may name a number, when a text stands over the second line's number.
        Assert.Equal<LabFileSniff>(
            sniffOf ByteOrderMarkAbsent CommaDelimiter HeaderOnFirstRow, (pickedLabFile [||] "phi,633\n0,0.5\n").sniffedLayout)

        // Ten non-empty lines decide, each without its CR, so the note on the eleventh splits nothing.
        let longRun = "phi;I\r\n\r\n" + String.concat "" [ for row in 1 .. 9 -> $"{row};0.5\r\n" ] + "end of run\r\n"
        Assert.Equal<LabFileSniff>(sniffOf ByteOrderMarkAbsent SemicolonDelimiter HeaderOnFirstRow, (pickedLabFile [||] longRun).sniffedLayout)
