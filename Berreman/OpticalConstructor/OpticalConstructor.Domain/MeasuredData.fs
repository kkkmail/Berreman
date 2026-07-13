namespace OpticalConstructor.Domain

open System
open System.Globalization
open Berreman.Fields
open Berreman.Geometry
open OpticalConstructor.Domain.Units
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.Experiments

/// Spec 0038 Part I (step 034) — the pure measured-data PARSERS (text in, values out). Following the
/// text-taking parser precedent that keeps filesystem IO and its exceptions OUT of the parser
/// (`OpticalConstructor.Storage.SpectralImport.parseSpectrumCsv`), every function here takes the file
/// TEXT (a `string`) and returns `Result<_, ExperimentDataError>` — never a path, never a stream, never
/// a throw. The disk read that yields the text lives at the edge (a future `ExperimentDataProxy`, §15.4);
/// this module is referentially transparent, so every test drives it from an in-memory string.
///
/// Two STRICT schema-v1 shapes, one per detector kind (spec §4 / step 25 `DetectorKind`):
///   - INTENSITY: first row is labels (ignored); then comma-separated `X,Y` rows in invariant culture.
///     The X column MEANS whatever the experiment's varied parameter says (wavelength in nm, or R1/R2 in
///     degrees) — so it stays a meaning-agnostic `MeasuredAbscissa` at parse time and only
///     `validateAgainstExperiment` interprets it against the experiment.
///   - ELLIPSOMETRIC: header `wavelength_nm,psi_deg,delta_deg` with an OPTIONAL `aoi_deg`; one file per
///     experiment.
/// Deliberately NOT in scope (future parsers, NOT schema changes): NO delimiter sniffing, NO unit
/// heuristics, NO N/C/S or Mueller-element imports.
///
/// `validateAgainstExperiment` checks a parsed series against the step-25 experiment's varied parameter,
/// range, and detector kind and returns a typed range / units mismatch — a LOUD error, never a silent
/// trim. Compiles LAST (nothing else in the Domain depends on it); reuses `Units` (the sole unit seam),
/// the engine `Angle`, and the step-25 `VariableParameter` / `VariableRange` / `DetectorKind`.
module MeasuredData =

    /// The measured-data error channel (errors as values, §0) — each case carries a human-readable
    /// `reason` naming exactly what failed (the first malformed row, the empty file, the offending value
    /// and range, or the detector/shape mismatch). Never a bare error case: a reason is what a log needs.
    type ExperimentDataError =
        | MalformedDataFile of reason : string
        | EmptyDataFile of reason : string
        | DataRangeMismatch of reason : string
        | DataUnitsMismatch of reason : string

    /// One measured abscissa value — the intensity file's X column (spec 034). Its MEANING (wavelength in
    /// nm, or R1/R2 in degrees) is fixed by the experiment's varied parameter, NOT by the file, so it is
    /// elevated as a meaning-agnostic scalar here and interpreted only in `validateAgainstExperiment`.
    type MeasuredAbscissa =
        | MeasuredAbscissa of float

        member this.value = let (MeasuredAbscissa x) = this in x

    /// One measured detector intensity — the S0 an intensity detector records (spec §4). Elevated, never
    /// a bare `float` in the point record.
    type MeasuredIntensity =
        | MeasuredIntensity of float

        member this.value = let (MeasuredIntensity i) = this in i

    /// One intensity sample: the (meaning-agnostic) abscissa and the recorded intensity (spec 034). A
    /// record over the two elevated point types — never a bare `(float * float)`.
    type IntensityPoint =
        {
            x : MeasuredAbscissa
            intensity : MeasuredIntensity
        }

    /// A parsed intensity series (spec 034): the ordered samples of an `X,Y` intensity data file. The X
    /// meaning is the experiment's varied parameter; the parser never assumes it.
    type IntensitySeries =
        {
            points : IntensityPoint list
        }

    /// One ellipsometric sample (spec §4 / 034): the wavelength and the measured Ψ / Δ, plus the OPTIONAL
    /// angle of incidence when the file carried an `aoi_deg` column. `waveLength` reduces to the engine
    /// meter base through the sole `Units` seam; Ψ / Δ / aoi are engine `Angle`s built from degrees.
    type EllipsometricPoint =
        {
            waveLength : WaveLength
            psi : Angle
            delta : Angle
            aoiOpt : Angle option
        }

    /// A parsed ellipsometric series (spec 034): the ordered `wavelength_nm, psi_deg, delta_deg[, aoi_deg]`
    /// samples of one experiment's data file.
    type EllipsometricSeries =
        {
            points : EllipsometricPoint list
        }

    /// A parsed measured-data series tagged by the detector shape that produced it (spec 034): an intensity
    /// sweep (X, S0) or an ellipsometric sweep (λ, Ψ, Δ). The tag lets `validateAgainstExperiment` reject a
    /// file whose shape does not match the experiment's detector kind as a LOUD `DataUnitsMismatch` — the
    /// two records are never silently coerced into each other.
    type MeasuredSeries =
        | IntensityData of IntensitySeries
        | EllipsometricData of EllipsometricSeries

    // -------------------------------------------------------------------------------------------------
    // Parsing primitives (pure, exception-free — string split + invariant-culture parse never throw).
    // -------------------------------------------------------------------------------------------------

    let private inv = CultureInfo.InvariantCulture

    /// Parse one invariant-culture float, tolerating surrounding whitespace (NO thousands separators, NO
    /// culture heuristics). `None` on any non-numeric field — the caller turns the first `None` into a
    /// typed `MalformedDataFile`.
    let private tryFloat (s : string) : float option =
        match Double.TryParse(s.Trim(), NumberStyles.Float, inv) with
        | true, v -> Some v
        | _ -> None

    /// The file's non-empty lines paired with their 1-based ORIGINAL line number (so a malformed-row error
    /// names the line the reader sees). Splits on `\n` and trims each line's `\r`/whitespace, so a pasted
    /// CRLF string parses; fully blank lines are dropped, never a header or a data row.
    let private nonEmptyLines (text : string) : (int * string) list =
        text.Split('\n')
        |> Array.mapi (fun i raw -> i + 1, raw.Trim())
        |> Array.filter (fun (_, s) -> s.Length > 0)
        |> Array.toList

    /// Parse one intensity data row: EXACTLY two comma-separated invariant-culture floats (strict — no
    /// extra columns, no delimiter sniffing). `None` on the wrong column count or a non-numeric field.
    let private parseIntensityRow (raw : string) : IntensityPoint option =
        match raw.Split(',') with
        | [| xs; ys |] ->
            match tryFloat xs, tryFloat ys with
            | Some x, Some y -> Some { x = MeasuredAbscissa x; intensity = MeasuredIntensity y }
            | _ -> None
        | _ -> None

    /// Parse an intensity data file TEXT (spec 034, STRICT schema v1): the first non-empty line is labels
    /// (ignored); every remaining non-empty line is an `X,Y` pair in invariant culture. An empty file (or
    /// a header with no data rows) is `EmptyDataFile`; the FIRST malformed row is a `MalformedDataFile`
    /// naming that row. No delimiter sniffing, no unit heuristics.
    let parseIntensitySeries (csvText : string) : Result<IntensitySeries, ExperimentDataError> =
        match nonEmptyLines csvText with
        | [] -> Error (EmptyDataFile "the intensity data file is empty (no header row and no data rows)")
        | [ _header ] -> Error (EmptyDataFile "the intensity data file has a header row but no data rows")
        | _header :: dataLines ->
            let rec loop (rows : (int * string) list) (acc : IntensityPoint list) : Result<IntensitySeries, ExperimentDataError> =
                match rows with
                | [] -> Ok { points = List.rev acc }
                | (lineNo, raw) :: rest ->
                    match parseIntensityRow raw with
                    | Some p -> loop rest (p :: acc)
                    | None -> Error (MalformedDataFile $"row {lineNo} is not a valid comma-separated X,Y pair: '{raw}'")
            loop dataLines []

    /// The ellipsometric column layout an accepted header declares (spec 034): the mandatory
    /// `wavelength_nm,psi_deg,delta_deg` triple, optionally with a trailing `aoi_deg`. A DU, not a bool
    /// flag — the column count is data.
    type private EllipsometricColumns =
        | WavelengthPsiDelta
        | WavelengthPsiDeltaAoi

        member this.count =
            match this with
            | WavelengthPsiDelta -> 3
            | WavelengthPsiDeltaAoi -> 4

    /// Validate an ellipsometric header (spec 034, STRICT — the header is CHECKED, not sniffed): exactly
    /// `wavelength_nm,psi_deg,delta_deg` with an optional `aoi_deg`, compared case-insensitively after
    /// trimming. Anything else is a `MalformedDataFile` naming the offending header.
    let private ellipsometricColumns (headerRaw : string) : Result<EllipsometricColumns, ExperimentDataError> =
        let cols = headerRaw.Split(',') |> Array.map (fun c -> c.Trim().ToLowerInvariant())
        match cols with
        | [| "wavelength_nm"; "psi_deg"; "delta_deg" |] -> Ok WavelengthPsiDelta
        | [| "wavelength_nm"; "psi_deg"; "delta_deg"; "aoi_deg" |] -> Ok WavelengthPsiDeltaAoi
        | _ ->
            Error (MalformedDataFile $"the ellipsometric header must be 'wavelength_nm,psi_deg,delta_deg' with an optional 'aoi_deg', but was: '{headerRaw}'")

    /// Parse one ellipsometric data row against the header's declared layout (spec 034): 3 (or 4)
    /// invariant-culture floats. `wavelength_nm` crosses to the engine `WaveLength` via the sole `Units`
    /// seam; Ψ / Δ / aoi become engine `Angle`s from degrees. `None` on the wrong column count or a
    /// non-numeric field.
    let private parseEllipsometricRow (columns : EllipsometricColumns) (raw : string) : EllipsometricPoint option =
        match columns, raw.Split(',') with
        | WavelengthPsiDelta, [| ws; ps; ds |] ->
            match tryFloat ws, tryFloat ps, tryFloat ds with
            | Some w, Some p, Some d ->
                Some { waveLength = toWaveLength Nanometer w; psi = Angle.degree p; delta = Angle.degree d; aoiOpt = None }
            | _ -> None
        | WavelengthPsiDeltaAoi, [| ws; ps; ds; aos |] ->
            match tryFloat ws, tryFloat ps, tryFloat ds, tryFloat aos with
            | Some w, Some p, Some d, Some a ->
                Some { waveLength = toWaveLength Nanometer w; psi = Angle.degree p; delta = Angle.degree d; aoiOpt = Some (Angle.degree a) }
            | _ -> None
        | _ -> None

    /// Parse an ellipsometric data file TEXT (spec 034, STRICT schema v1): the first non-empty line is the
    /// `wavelength_nm,psi_deg,delta_deg[,aoi_deg]` header (VALIDATED, not sniffed); every remaining
    /// non-empty line is 3 (or 4) invariant-culture floats. An empty file (or a header with no data rows)
    /// is `EmptyDataFile`; a bad header or the FIRST malformed row is a `MalformedDataFile` naming it.
    let parseEllipsometricSeries (csvText : string) : Result<EllipsometricSeries, ExperimentDataError> =
        match nonEmptyLines csvText with
        | [] -> Error (EmptyDataFile "the ellipsometric data file is empty (no header row and no data rows)")
        | (_, headerRaw) :: dataLines ->
            match ellipsometricColumns headerRaw with
            | Error e -> Error e
            | Ok columns ->
                match dataLines with
                | [] -> Error (EmptyDataFile "the ellipsometric data file has a header row but no data rows")
                | _ ->
                    let rec loop (rows : (int * string) list) (acc : EllipsometricPoint list) : Result<EllipsometricSeries, ExperimentDataError> =
                        match rows with
                        | [] -> Ok { points = List.rev acc }
                        | (lineNo, raw) :: rest ->
                            match parseEllipsometricRow columns raw with
                            | Some p -> loop rest (p :: acc)
                            | None -> Error (MalformedDataFile $"row {lineNo} is not a valid ellipsometric row (expected {columns.count} invariant-culture numbers): '{raw}'")
                    loop dataLines []

    // -------------------------------------------------------------------------------------------------
    // Validation against a step-25 experiment (LOUD errors, never a silent trim).
    // -------------------------------------------------------------------------------------------------

    /// Whether `x` sits within `[lo, hi]`, absorbing float noise at the endpoints with a tiny
    /// range-relative epsilon (so a tabulated 800.0 nm never spuriously overruns an 800 nm range top) —
    /// but still LOUD for a genuine overrun.
    let private inRange (lo : float) (hi : float) (x : float) : bool =
        let eps = 1e-9 * max 1.0 (max (abs lo) (abs hi))
        x >= lo - eps && x <= hi + eps

    /// Validate a parsed series against a step-25 experiment's varied parameter, numeric range, and
    /// detector kind (spec 034). Returns the series UNCHANGED on success (never trims / clips); otherwise a
    /// typed, LOUD mismatch:
    ///   - the series shape must match the detector kind (an intensity file for an ellipsometer, or an
    ///     ellipsometric file for an intensity detector, is a `DataUnitsMismatch`);
    ///   - an intensity series' X abscissa is in the varied quantity's display unit (nm / degrees) BY the
    ///     schema, so any X outside `[range.min, range.max]` is a `DataRangeMismatch`;
    ///   - an ellipsometric series against a wavelength sweep (`VaryWaveLength`) range-checks its
    ///     wavelength (nm) the same way; against a non-wavelength varied parameter the file's wavelength
    ///     abscissa is NOT the swept quantity, so the wavelength range check does not apply.
    let validateAgainstExperiment
        (varied : VariableParameter option)
        (range : VariableRange)
        (detector : DetectorKind)
        (series : MeasuredSeries)
        : Result<MeasuredSeries, ExperimentDataError> =
        let lo = min range.min range.max
        let hi = max range.min range.max
        let unit = varied |> Option.map (fun v -> v.unitLabel) |> Option.defaultValue ""
        match detector, series with
        | Intensity, EllipsometricData _ ->
            Error (DataUnitsMismatch "the detector records intensity (S0), but the data file is an ellipsometric Ψ/Δ series")
        | Ellipsometer, IntensityData _ ->
            Error (DataUnitsMismatch "the detector is an ellipsometer (records Ψ/Δ), but the data file is a single-intensity series")
        | Intensity, IntensityData s ->
            match s.points |> List.tryFind (fun p -> not (inRange lo hi p.x.value)) with
            | Some p -> Error (DataRangeMismatch $"a measured abscissa {p.x.value} {unit} lies outside the experiment range [{lo}, {hi}] {unit}")
            | None -> Ok series
        | Ellipsometer, EllipsometricData s ->
            match varied with
            | Some VaryWaveLength ->
                match s.points |> List.tryFind (fun p -> not (inRange lo hi (wavelengthToUnit Nanometer p.waveLength))) with
                | Some p ->
                    let nm = wavelengthToUnit Nanometer p.waveLength
                    Error (DataRangeMismatch $"a measured wavelength {nm} nm lies outside the experiment range [{lo}, {hi}] nm")
                | None -> Ok series
            | Some VaryR1
            | Some VaryR2
            | None -> Ok series
