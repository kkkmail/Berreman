namespace OpticalConstructor.Tests

open Berreman.Geometry
open OpticalConstructor.Domain.Units
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.Experiments
open OpticalConstructor.Domain.MeasuredData
open Xunit

/// Spec 0038 Part I (step 034) — the pure measured-data PARSERS + validation. Good, malformed, and empty
/// CSV strings parse or reject with typed `ExperimentDataError`s; a range or units mismatch against a
/// step-25 experiment yields the typed validation error. Every case runs against an IN-MEMORY STRING —
/// no test performs file IO (the acceptance's hard rule).
module MeasuredDataTests =

    /// A CSV TEXT from ordered lines (LF-joined) — the parsers take the file text, never a path.
    let private csv (lines : string list) : string = System.String.Join("\n", lines)

    let private iPoint (x : float) (i : float) : IntensityPoint =
        { x = MeasuredAbscissa x; intensity = MeasuredIntensity i }

    let private iSeries (pts : (float * float) list) : IntensitySeries =
        { points = pts |> List.map (fun (x, i) -> iPoint x i) }

    let private ePoint (w : float) (psi : float) (delta : float) (aoi : float option) : EllipsometricPoint =
        {
            waveLength = toWaveLength Nanometer w
            psi = Angle.degree psi
            delta = Angle.degree delta
            aoiOpt = aoi |> Option.map Angle.degree
        }

    let private eSeries (pts : EllipsometricPoint list) : EllipsometricSeries = { points = pts }

    let private range (lo : float) (hi : float) : VariableRange = { min = lo; max = hi; points = 2 }

    // ============================ intensity parsing ============================

    [<Fact>]
    let ``a good intensity CSV parses to X,Y points with the labels row ignored`` () =
        let text = csv [ "angle_deg,intensity"; "0.0,1.0"; "45.0,0.5"; "90.0,0.0" ]
        match parseIntensitySeries text with
        | Ok series ->
            Assert.Equal(3, List.length series.points)
            let first = List.head series.points
            Assert.Equal(0.0, first.x.value, 9)
            Assert.Equal(1.0, first.intensity.value, 9)
            let last = List.last series.points
            Assert.Equal(90.0, last.x.value, 9)
            Assert.Equal(0.0, last.intensity.value, 9)
        | Error e -> failwith $"expected a parsed series, got {e}"

    [<Fact>]
    let ``a CRLF-pasted intensity CSV still parses (line endings trimmed)`` () =
        let text = "x,y\r\n1.0,2.0\r\n3.0,4.0\r\n"
        match parseIntensitySeries text with
        | Ok series ->
            Assert.Equal(2, List.length series.points)
            Assert.Equal(3.0, (List.last series.points).x.value, 9)
        | Error e -> failwith $"expected a parsed series, got {e}"

    [<Fact>]
    let ``a malformed intensity row yields MalformedDataFile naming the first bad row`` () =
        // Row 3 (1-based, header included) is the offender; rows 2 and 4 are valid.
        let text = csv [ "x,y"; "1.0,2.0"; "foo,3.0"; "4.0,5.0" ]
        match parseIntensitySeries text with
        | Error (MalformedDataFile reason) ->
            Assert.Contains("3", reason)      // the offending line number
            Assert.Contains("foo", reason)    // the offending content
        | other -> failwith $"expected MalformedDataFile, got {other}"

    [<Fact>]
    let ``an intensity row with the wrong column count is malformed (strict, no sniffing)`` () =
        let text = csv [ "x,y"; "1.0,2.0,3.0" ]
        match parseIntensitySeries text with
        | Error (MalformedDataFile _) -> ()
        | other -> failwith $"expected MalformedDataFile for a 3-column row, got {other}"

    [<Fact>]
    let ``an empty intensity string yields EmptyDataFile`` () =
        match parseIntensitySeries "" with
        | Error (EmptyDataFile _) -> ()
        | other -> failwith $"expected EmptyDataFile, got {other}"

    [<Fact>]
    let ``an intensity file with only a header (no data rows) yields EmptyDataFile`` () =
        match parseIntensitySeries (csv [ "x,y" ]) with
        | Error (EmptyDataFile _) -> ()
        | other -> failwith $"expected EmptyDataFile for a header-only file, got {other}"

    // ============================ ellipsometric parsing ============================

    [<Fact>]
    let ``a good 3-column ellipsometric CSV parses wavelength via Units and Ψ/Δ as angles`` () =
        let text = csv [ "wavelength_nm,psi_deg,delta_deg"; "200,10,20"; "500,30,120" ]
        match parseEllipsometricSeries text with
        | Ok series ->
            Assert.Equal(2, List.length series.points)
            let second = List.item 1 series.points
            Assert.Equal(500.0, wavelengthToUnit Nanometer second.waveLength, 6)
            Assert.Equal(30.0, second.psi.degrees, 9)
            Assert.Equal(120.0, second.delta.degrees, 9)
            Assert.True(Option.isNone second.aoiOpt)
        | Error e -> failwith $"expected a parsed series, got {e}"

    [<Fact>]
    let ``a good 4-column ellipsometric CSV carries the optional aoi_deg`` () =
        let text = csv [ "wavelength_nm,psi_deg,delta_deg,aoi_deg"; "633,25,90,70" ]
        match parseEllipsometricSeries text with
        | Ok series ->
            let p = List.head series.points
            match p.aoiOpt with
            | Some aoi -> Assert.Equal(70.0, aoi.degrees, 9)
            | None -> failwith "expected the aoi_deg column to be carried"
        | Error e -> failwith $"expected a parsed series, got {e}"

    [<Fact>]
    let ``an ellipsometric header with wrong column names yields MalformedDataFile`` () =
        // Wrong names (no unit heuristic rescues this) — a strict, LOUD reject.
        let text = csv [ "lambda,psi,delta"; "500,30,120" ]
        match parseEllipsometricSeries text with
        | Error (MalformedDataFile reason) ->
            Assert.Contains("wavelength_nm", reason)
        | other -> failwith $"expected MalformedDataFile for a bad header, got {other}"

    [<Fact>]
    let ``a malformed ellipsometric data row yields MalformedDataFile naming the row`` () =
        let text = csv [ "wavelength_nm,psi_deg,delta_deg"; "500,30,120"; "600,nan-here,40" ]
        match parseEllipsometricSeries text with
        | Error (MalformedDataFile reason) -> Assert.Contains("3", reason)
        | other -> failwith $"expected MalformedDataFile, got {other}"

    [<Fact>]
    let ``an empty ellipsometric string yields EmptyDataFile`` () =
        match parseEllipsometricSeries "" with
        | Error (EmptyDataFile _) -> ()
        | other -> failwith $"expected EmptyDataFile, got {other}"

    [<Fact>]
    let ``an ellipsometric file with only a header yields EmptyDataFile`` () =
        match parseEllipsometricSeries (csv [ "wavelength_nm,psi_deg,delta_deg" ]) with
        | Error (EmptyDataFile _) -> ()
        | other -> failwith $"expected EmptyDataFile for a header-only file, got {other}"

    // ============================ validateAgainstExperiment ============================

    [<Fact>]
    let ``an intensity series within the experiment range validates against an intensity detector`` () =
        let series = IntensityData (iSeries [ 0.0, 1.0; 45.0, 0.5; 90.0, 0.0 ])
        match validateAgainstExperiment (Some VaryR1) (range 0.0 360.0) Intensity series with
        | Ok back -> Assert.Equal<MeasuredSeries>(series, back)   // returned UNCHANGED — never trimmed
        | Error e -> failwith $"expected the series to validate, got {e}"

    [<Fact>]
    let ``an intensity abscissa outside the range yields DataRangeMismatch (LOUD, never trimmed)`` () =
        // 900° is outside the 0…360° R1 range — a LOUD reject, not a silent clip.
        let series = IntensityData (iSeries [ 0.0, 1.0; 900.0, 0.5 ])
        match validateAgainstExperiment (Some VaryR1) (range 0.0 360.0) Intensity series with
        | Error (DataRangeMismatch reason) -> Assert.Contains("900", reason)
        | other -> failwith $"expected DataRangeMismatch, got {other}"

    [<Fact>]
    let ``an intensity series against an ellipsometer detector yields DataUnitsMismatch`` () =
        let series = IntensityData (iSeries [ 0.0, 1.0 ])
        match validateAgainstExperiment (Some VaryR1) (range 0.0 360.0) Ellipsometer series with
        | Error (DataUnitsMismatch _) -> ()
        | other -> failwith $"expected DataUnitsMismatch, got {other}"

    [<Fact>]
    let ``an ellipsometric wavelength sweep within range validates against an ellipsometer`` () =
        let series = EllipsometricData (eSeries [ ePoint 200.0 10.0 20.0 None; ePoint 800.0 30.0 120.0 None ])
        match validateAgainstExperiment (Some VaryWaveLength) (range 200.0 800.0) Ellipsometer series with
        | Ok back -> Assert.Equal<MeasuredSeries>(series, back)
        | Error e -> failwith $"expected the series to validate, got {e}"

    [<Fact>]
    let ``an ellipsometric wavelength outside the swept range yields DataRangeMismatch`` () =
        // 1000 nm overruns a 200…800 nm wavelength sweep.
        let series = EllipsometricData (eSeries [ ePoint 200.0 10.0 20.0 None; ePoint 1000.0 30.0 120.0 None ])
        match validateAgainstExperiment (Some VaryWaveLength) (range 200.0 800.0) Ellipsometer series with
        | Error (DataRangeMismatch reason) -> Assert.Contains("1000", reason)
        | other -> failwith $"expected DataRangeMismatch, got {other}"

    [<Fact>]
    let ``an ellipsometric series against an intensity detector yields DataUnitsMismatch`` () =
        let series = EllipsometricData (eSeries [ ePoint 500.0 30.0 120.0 None ])
        match validateAgainstExperiment (Some VaryWaveLength) (range 200.0 800.0) Intensity series with
        | Error (DataUnitsMismatch _) -> ()
        | other -> failwith $"expected DataUnitsMismatch, got {other}"

    [<Fact>]
    let ``an ellipsometric series against a non-wavelength varied parameter skips the wavelength range check`` () =
        // The file's wavelength abscissa is NOT the swept quantity (R2 is), so a wide wavelength is not a
        // range mismatch — the wavelength range check does not apply to a non-wavelength sweep.
        let series = EllipsometricData (eSeries [ ePoint 200.0 10.0 20.0 (Some 40.0); ePoint 1000.0 30.0 120.0 (Some 80.0) ])
        match validateAgainstExperiment (Some VaryR2) (range 0.0 89.0) Ellipsometer series with
        | Ok back -> Assert.Equal<MeasuredSeries>(series, back)
        | Error e -> failwith $"expected the series to validate (no wavelength range check), got {e}"
