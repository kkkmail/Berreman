namespace OpticalConstructor.Tests

open System
open Xunit
open Berreman.Constants
open Berreman.Fields
open Berreman.Geometry
open Berreman.Media
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.MeasuredData
open OpticalConstructor.Domain.MuellerReconstruction
open OpticalConstructor.Domain.MuellerInverse
open OpticalConstructor.Domain.ImportProfiles

/// The built-in import profiles, the profile an experiment names, the points a profile maps a file to and the wizard's
/// one rule from column choices to roles (spec 0050 S9.2).
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

    [<Fact>]
    let ``mapped points count what they hold`` () : unit =
        let intensity : IntensitySeries =
            {
                points =
                    [
                        { x = MeasuredAbscissa 0.0; intensity = MeasuredIntensity 1.0 }
                        { x = MeasuredAbscissa 90.0; intensity = MeasuredIntensity 0.0 }
                    ]
            }

        let captureAt (index : CaptureIndex) (azimuth : Angle) : MuellerRawRow =
            {
                experiment = MuellerExperimentLabel "CPL-(QZ#90-LR#90)-CPL"
                captureIndex = index
                capturedAt = DateTimeOffset(2026, 11, 3, 12, 0, 0, TimeSpan.Zero)
                description = azimuth
                avgTotal = MeasuredIntensity 0.5
            }

        let configuration : MeasurementConfiguration =
            {
                cut = ZCut
                thickness = Thickness.mm 1.0<mm>
                incidenceAngle = IncidenceAngle.normal
                azimuth = SampleAzimuth.degree 0.0<deg>
                observable = TransmittedMueller
                waveLength = WaveLength.nm 600.0<nm>
            }

        let observationOf (key : DataRowKey) : KeyedObservation =
            let observation : MuellerObservation = { configuration = configuration; measured = Propagation.identityMueller }
            { key = key; observation = observation }

        let rowOf (reading : MappedReading) : MappedRow =
            {
                keyOpt = Some (DataRowKey "QZ")
                scan = [ scanCellOf DegreeColumnUnit (Units.UnitMagnitude 45.0) ]
                reading = reading
                timestampOpt = Some (RowTimestamp (DateTimeOffset(2026, 11, 3, 12, 0, 0, TimeSpan.Zero)))
                captureIndexOpt = Some (CaptureIndex 1)
                observableOpt = Some TransmittedMueller
            }

        let captures =
            [
                captureAt (CaptureIndex 1) (Angle.degree 0.0<deg>)
                captureAt (CaptureIndex 2) (Angle.degree 45.0<deg>)
                captureAt (CaptureIndex 3) (Angle.degree 90.0<deg>)
            ]

        let observations = [ observationOf (DataRowKey "QZ"); observationOf (DataRowKey "LR"); observationOf (DataRowKey "QZ+LR") ]

        let rows =
            [
                rowOf (SignalReading (MeasuredIntensity 1.0, Some (MeasuredSigma 0.01)))
                rowOf (SignalReading (MeasuredIntensity 0.5, None))
                rowOf (MuellerReading Propagation.identityMueller)
            ]

        Assert.Equal(MeasuredPointCount 2, (SeriesPoints (IntensityData intensity)).pointCount)
        Assert.Equal(MeasuredPointCount 3, (CapturePoints captures).pointCount)
        Assert.Equal(MeasuredPointCount 3, (ObservationPoints observations).pointCount)
        Assert.Equal(MeasuredPointCount 3, (ColumnPoints rows).pointCount)
