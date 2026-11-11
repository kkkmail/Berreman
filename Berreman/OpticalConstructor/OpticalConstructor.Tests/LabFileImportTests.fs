namespace OpticalConstructor.Tests

open System.Globalization
open Xunit
open Berreman.Constants
open Berreman.Fields
open Berreman.Geometry
open Berreman.Media
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.MeasuredData
open OpticalConstructor.Domain.PropagationReadouts
open OpticalConstructor.Domain.MuellerInverse
open OpticalConstructor.Domain.ImportProfiles
open OpticalConstructor.Storage.LabFileImport

/// The Mueller-matrix observation file the app writes and the `Mueller-matrix observations` profile reads, and a kit
/// profile's column mapping of a lab file (spec 0050 S9.2).
module LabFileImportTests =

    /// The header the slice fixes, written out here as the oracle the writer is held to.
    let private header : string =
        "sample,cut,cut_tilt_rad,thickness_m,incidence_rad,azimuth_rad,observable,wavelength_nm,"
        + "m00,m01,m02,m03,m10,m11,m12,m13,m20,m21,m22,m23,m30,m31,m32,m33,throughput"

    /// A matrix as `misreadAbsolute` builds one: the throughput as m₀₀ and throughput × nₖ elsewhere, with
    /// nₖ = (−1)ᵏ (k + 1) / 17 at k = 4i + j.
    let private absoluteMatrix (throughput : float) : MuellerMatrix =
        RealMatrix4x4.create
            [ for i in 0 .. 3 ->
                [ for j in 0 .. 3 ->
                    let k = 4 * i + j
                    let n = float (if k % 2 = 0 then k + 1 else -(k + 1)) / 17.0
                    if k = 0 then throughput else throughput * n ] ]
        |> MuellerMatrix

    let private keyedObservation (key : DataRowKey) (configuration : MeasurementConfiguration) (measured : MuellerMatrix) : KeyedObservation =
        let observation : MuellerObservation = { configuration = configuration; measured = measured }
        { key = key; observation = observation }

    let private zCutConfiguration : MeasurementConfiguration =
        {
            cut = ZCut
            thickness = Thickness.mkm 100.0<mkm>
            incidenceAngle = IncidenceAngle.create (Angle.degree 45.0<deg>)
            azimuth = SampleAzimuth.degree 22.5<deg>
            observable = ReflectedMueller
            waveLength = WaveLength.nm 632.8<nm>
        }

    let private opticAxisConfiguration : MeasurementConfiguration =
        {
            cut = TiltedCut (Angle 0.3141592653589793)
            thickness = Thickness.mkm 100.0<mkm>
            incidenceAngle = IncidenceAngle.normal
            azimuth = SampleAzimuth.degree 67.5<deg>
            observable = TransmittedMueller
            waveLength = WaveLength.nm 632.8<nm>
        }

    let private observations : KeyedObservation list =
        [
            keyedObservation (DataRowKey "Z-cut") zCutConfiguration (absoluteMatrix 0.8125)
            keyedObservation (DataRowKey "optic axis (A)") opticAxisConfiguration (absoluteMatrix 0.03)
        ]

    let private writtenText () : KitFileText =
        match KeyedObservation.observationFileText observations with
        | Ok text -> text
        | Error error -> failwith $"the two observations were refused: %A{error}"

    /// The written file's lines without the final line feed's empty remainder.
    let private writtenLines () : string list =
        let text = (writtenText ()).value
        Assert.EndsWith("\n", text)
        text.Substring(0, text.Length - 1).Split('\n') |> Array.toList

    let private assertMalformed (fragment : string) (result : Result<'Item, ExperimentDataError>) : unit =
        match result with
        | Error (MalformedDataFileErr reason) -> Assert.Contains(fragment, reason)
        | other -> Assert.Fail($"expected MalformedDataFileErr naming '{fragment}', got %A{other}")

    [<Fact>]
    let ``the observation file writes each observation's configuration, normalized elements and throughput in one row`` () : unit =
        let text = (writtenText ()).value
        Assert.False(text.StartsWith('﻿'), "no byte-order mark is written")
        Assert.DoesNotContain("\r", text)

        let lines = writtenLines ()
        Assert.Equal(3, List.length lines)
        Assert.Equal(header, lines[0])

        let first = lines[1].Split(',')
        Assert.StartsWith("Z-cut,1,0,", lines[1])
        Assert.Equal(25, first.Length)
        Assert.Equal("2", first[6])
        Assert.Equal("632.8", first[7])
        Assert.Equal("1", first[8])
        Assert.Equal("0.8125", first[24])

        let second = lines[2].Split(',')
        Assert.StartsWith("optic axis (A),4,0.3141592653589793,", lines[2])
        Assert.Equal(25, second.Length)
        Assert.Equal("1", second[6])
        Assert.Equal("1", second[8])
        Assert.Equal("0.03", second[24])

    [<Fact>]
    let ``an observation file reads back to the same observations, element for element, and writes back to the same text`` () : unit =
        let text = writtenText ()

        match text.parseMuellerObservations with
        | Error error -> Assert.Fail($"the written file was refused: %A{error}")
        | Ok readBack ->
            Assert.Equal(List.length observations, List.length readBack)

            for written, read in List.zip observations readBack do
                Assert.Equal<DataRowKey>(written.key, read.key)
                Assert.Equal<MeasurementConfiguration>(written.observation.configuration, read.observation.configuration)

                for i in 0 .. 3 do
                    for j in 0 .. 3 do
                        Assert.Equal<float>(written.observation.measured.element i j, read.observation.measured.element i j)

            Assert.Equal<Result<KitFileText, ExperimentDataError>>(Ok text, KeyedObservation.observationFileText readBack)

    [<Fact>]
    let ``an observation file of another shape, or one the profile cannot write, is refused`` () : unit =
        let lines = writtenLines ()
        let row = lines[1]
        let cells = row.Split(',')
        let fileOf (fileLines : string list) : KitFileText = fileLines |> List.map (fun line -> line + "\n") |> String.concat "" |> KitFileText

        let withoutM33 = header.Replace(",m33,", ",")
        assertMalformed withoutM33 (fileOf [ withoutM33; row ]).parseMuellerObservations

        match (fileOf [ header ]).parseMuellerObservations with
        | Error (EmptyDataFileErr _) -> ()
        | other -> Assert.Fail($"expected EmptyDataFileErr for the header alone, got %A{other}")

        let shortRow = cells[0 .. 23] |> String.concat ","
        assertMalformed "line 2" (fileOf [ header; shortRow ]).parseMuellerObservations

        let unknownCut = Array.concat [ cells[0 .. 0]; [| "5" |]; cells[2..] ] |> String.concat ","
        assertMalformed "line 2" (fileOf [ header; unknownCut ]).parseMuellerObservations

        let zCut = observations[0]
        let refusedSecond (bad : KeyedObservation) : unit =
            assertMalformed $"observation 2 ('{bad.key.value}')" (KeyedObservation.observationFileText [ zCut; bad ])

        refusedSecond (keyedObservation (DataRowKey "a,b") zCutConfiguration zCut.observation.measured)
        refusedSecond (keyedObservation (DataRowKey "infinite") { zCutConfiguration with thickness = Thickness.Infinity } zCut.observation.measured)
        refusedSecond (keyedObservation (DataRowKey "micrometres") { zCutConfiguration with waveLength = WaveLength.mkm 0.6328<mkm> } zCut.observation.measured)
        refusedSecond (keyedObservation (DataRowKey "dark") zCutConfiguration (absoluteMatrix 0.0))

    let private labScanColumn : ScanColumn =
        { column = DataColumnNumber 1; valueUnit = DegreeColumnUnit; reading = PhysicalScanValue }

    /// A comma file with a header row: the scan angle in degrees, the signal and its σ, and the experiment key.
    let private labScanProfile : ImportProfile =
        let roles : ColumnRoles =
            {
                scan = [ labScanColumn ]
                reading = SignalColumns (DataColumnNumber 2, Some (DataColumnNumber 3))
                timestampOpt = None
                captureIndexOpt = None
                observableOpt = None
                keyOpt = Some (DataColumnNumber 4)
            }

        {
            name = ImportProfileName "Lab scan"
            delimiter = CommaDelimiter
            header = HeaderOnFirstRow
            bom = ByteOrderMarkAbsent
            roles = roles
        }

    let private incidenceColumn : ScanColumn =
        { column = DataColumnNumber 1; valueUnit = RadianColumnUnit; reading = PhysicalScanValue }

    /// A semicolon file without a header: the incidence in radians, m00…m33 in columns 2 to 17, the throughput, the
    /// observable and the key.
    let private muellerProfile : ImportProfile =
        let roles : ColumnRoles =
            {
                scan = [ incidenceColumn ]
                reading = MuellerElementColumns ([ for column in 2 .. 17 -> DataColumnNumber column ], Some (DataColumnNumber 18))
                timestampOpt = None
                captureIndexOpt = None
                observableOpt = Some (DataColumnNumber 19)
                keyOpt = Some (DataColumnNumber 20)
            }

        {
            name = ImportProfileName "Mueller scan"
            delimiter = SemicolonDelimiter
            header = NoHeaderRow
            bom = ByteOrderMarkAbsent
            roles = roles
        }

    let private mappedRows (profile : ImportProfile) (text : KitFileText) : MappedRow list =
        match profile.mapText text with
        | Ok rows -> rows
        | Error error -> failwith $"the lab file was refused: %A{error}"

    [<Fact>]
    let ``a column mapping reads each row's scan values, signal, σ and key`` () : unit =
        let rows =
            mappedRows labScanProfile (KitFileText "phi,I,sigma,exp\n0,0.5,0.01,CPL-(QZ#45)-CPL\n20,0.25,0.01,\"LP-(AIR#0)-LP\"\n")

        let expected : MappedRow list =
            [
                {
                    keyOpt = Some (DataRowKey "CPL-(QZ#45)-CPL")
                    scan = [ { column = labScanColumn; magnitude = Units.UnitMagnitude 0.0 } ]
                    reading = SignalReading (MeasuredIntensity 0.5, Some (MeasuredSigma 0.01))
                    timestampOpt = None
                    captureIndexOpt = None
                    observableOpt = None
                }
                {
                    keyOpt = Some (DataRowKey "LP-(AIR#0)-LP")
                    scan = [ { column = labScanColumn; magnitude = Units.UnitMagnitude 20.0 } ]
                    reading = SignalReading (MeasuredIntensity 0.25, Some (MeasuredSigma 0.01))
                    timestampOpt = None
                    captureIndexOpt = None
                    observableOpt = None
                }
            ]

        Assert.Equal<MappedRow list>(expected, rows)

    [<Fact>]
    let ``a semicolon file without a header maps its Mueller elements and throughput`` () : unit =
        let normalized = absoluteMatrix 1.0
        let number (x : float) : string = x.ToString("R", CultureInfo.InvariantCulture)

        let text =
            [
                "0.25"
                for i in 0 .. 3 do
                    for j in 0 .. 3 do
                        number (normalized.element i j)
                "0.5"
                "R"
                "Z-cut"
            ]
            |> String.concat ";"
            |> fun row -> row + "\n"
            |> KitFileText

        match mappedRows muellerProfile text with
        | [ mapped ] ->
            Assert.Equal<ScanCell list>([ { column = incidenceColumn; magnitude = Units.UnitMagnitude 0.25 } ], mapped.scan)
            Assert.Equal<Observable option>(Some ReflectedMueller, mapped.observableOpt)
            Assert.Equal<DataRowKey option>(Some (DataRowKey "Z-cut"), mapped.keyOpt)

            match mapped.reading with
            | MuellerReading measured ->
                Assert.Equal<float>(0.5, measured.element 0 0)
                Assert.Equal<float>(0.5 * normalized.element 1 2, measured.element 1 2)
                let absolute = absoluteMatrix 0.5

                for i in 0 .. 3 do
                    for j in 0 .. 3 do
                        Assert.Equal<float>(absolute.element i j, measured.element i j)
            | SignalReading _ -> Assert.Fail("a Mueller mapping read a signal")
        | rows -> Assert.Fail($"expected one mapped row, got {List.length rows}")

    [<Fact>]
    let ``a cell the mapping cannot read names its row and column`` () : unit =
        assertMalformed "data row 2, column 2: 'n/a'" (labScanProfile.mapText (KitFileText "phi,I,sigma,exp\n0,0.5,0.01,A\n20,n/a,0.01,B\n"))

        // No key column at all, a blank key cell, and a row short of its header, which the CSV reader refuses.
        assertMalformed "data row 1, column 4" (labScanProfile.mapText (KitFileText "phi,I,sigma\n0,0.5,0.01\n"))
        assertMalformed "data row 2, column 4: ''" (labScanProfile.mapText (KitFileText "phi,I,sigma,exp\n0,0.5,0.01,A\n20,0.25,0.01,\n"))

        match labScanProfile.mapText (KitFileText "phi,I,sigma,exp\n0,0.5,0.01,A\n20,0.25,0.01\n") with
        | Error (MalformedDataFileErr _) -> ()
        | other -> Assert.Fail($"expected MalformedDataFileErr for a row short of the header, got %A{other}")

        match labScanProfile.mapText (KitFileText "phi,I,sigma,exp\n") with
        | Error (EmptyDataFileErr _) -> ()
        | other -> Assert.Fail($"expected EmptyDataFileErr for the header alone, got %A{other}")
