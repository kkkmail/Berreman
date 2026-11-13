namespace OpticalConstructor.Tests

open System
open Xunit
open Berreman.Constants
open Berreman.Fields
open Berreman.Geometry
open Analytics.AnalysisFunctions
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.Propagation
open OpticalConstructor.Domain.MeasuredData
open OpticalConstructor.Domain.MuellerReconstruction
open OpticalConstructor.Domain.MuellerInverse
open OpticalConstructor.Domain.Experiments
open OpticalConstructor.Domain.ImportProfiles
open OpticalConstructor.Domain.StudyForward

/// Spec 0050 S11.2 — what a detector records, and each mapped point placed on the scan its experiment was taken over.
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
