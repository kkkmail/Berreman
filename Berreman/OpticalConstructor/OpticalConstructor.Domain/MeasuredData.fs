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
/// TEXT (a `DataFileText`) and returns `Result<_, ExperimentDataError>` — never a path, never a stream,
/// never a throw. The disk read that yields the text lives at the edge (a future `ExperimentDataProxy`, §15.4);
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
        | MalformedDataFileErr of reason : string
        | EmptyDataFileErr of reason : string
        | DataRangeMismatchErr of reason : string
        | DataUnitsMismatchErr of reason : string

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
    // Parsing (pure, exception-free — string split + invariant-culture parse never throw).
    // -------------------------------------------------------------------------------------------------

    /// A 1-based line number of a data file, as the reader sees it (a malformed-row error names it).
    type DataLineNumber =
        | DataLineNumber of int

        member this.value = let (DataLineNumber n) = this in n

    /// One non-empty, trimmed line of a measured-data file: a header or a data row.
    type DataFileRow =
        | DataFileRow of string

        member this.value = let (DataFileRow r) = this in r

        /// The row's comma-separated cells as invariant-culture floats, each tolerating surrounding
        /// whitespace (NO thousands separators, NO culture heuristics); `None` for a non-numeric cell.
        member this.numbers : float option list =
            this.value.Split(',')
            |> Array.map (fun cell ->
                match Double.TryParse(cell.Trim(), NumberStyles.Float, CultureInfo.InvariantCulture) with
                | true, v -> Some v
                | _ -> None)
            |> Array.toList

        /// The row as an intensity sample: EXACTLY two comma-separated invariant-culture floats (strict — no
        /// extra columns, no delimiter sniffing). `None` on the wrong column count or a non-numeric field.
        member this.tryIntensityPoint : IntensityPoint option =
            match this.numbers with
            | [ Some x; Some y ] -> Some { x = MeasuredAbscissa x; intensity = MeasuredIntensity y }
            | _ -> None

    /// The ellipsometric column layout an accepted header declares (spec 034): the mandatory
    /// `wavelength_nm,psi_deg,delta_deg` triple, optionally with a trailing `aoi_deg`. A DU, not a bool
    /// flag — the column count is data.
    type EllipsometricColumns =
        | WavelengthPsiDelta
        | WavelengthPsiDeltaAoi

        member this.count : int =
            match this with
            | WavelengthPsiDelta -> 3
            | WavelengthPsiDeltaAoi -> 4

        /// Validate an ellipsometric header (spec 034, STRICT — the header is CHECKED, not sniffed): exactly
        /// `wavelength_nm,psi_deg,delta_deg` with an optional `aoi_deg`, compared case-insensitively after
        /// trimming. Anything else is a `MalformedDataFileErr` naming the offending header.
        static member tryOfHeader (header : DataFileRow) : Result<EllipsometricColumns, ExperimentDataError> =
            let cols = header.value.Split(',') |> Array.map (fun c -> c.Trim().ToLowerInvariant())
            match cols with
            | [| "wavelength_nm"; "psi_deg"; "delta_deg" |] -> Ok WavelengthPsiDelta
            | [| "wavelength_nm"; "psi_deg"; "delta_deg"; "aoi_deg" |] -> Ok WavelengthPsiDeltaAoi
            | _ ->
                Error (MalformedDataFileErr $"the ellipsometric header must be 'wavelength_nm,psi_deg,delta_deg' with an optional 'aoi_deg', but was: '{header.value}'")

        /// Parse one ellipsometric data row against this layout (spec 034): 3 (or 4) invariant-culture
        /// floats. `wavelength_nm` crosses to the engine `WaveLength` via the sole `Units` seam; Ψ / Δ / aoi
        /// become engine `Angle`s from degrees. `None` on the wrong column count or a non-numeric field.
        member this.tryPoint (row : DataFileRow) : EllipsometricPoint option =
            match this, row.numbers with
            | WavelengthPsiDelta, [ Some w; Some p; Some d ] ->
                Some { waveLength = toWaveLength Nanometer (UnitMagnitude w); psi = Angle.degree (p * 1.0<Berreman.Constants.deg>); delta = Angle.degree (d * 1.0<Berreman.Constants.deg>); aoiOpt = None }
            | WavelengthPsiDeltaAoi, [ Some w; Some p; Some d; Some a ] ->
                Some { waveLength = toWaveLength Nanometer (UnitMagnitude w); psi = Angle.degree (p * 1.0<Berreman.Constants.deg>); delta = Angle.degree (d * 1.0<Berreman.Constants.deg>); aoiOpt = Some (Angle.degree (a * 1.0<Berreman.Constants.deg>)) }
            | _ -> None

    /// The text of a measured-data file, as read at the IO edge (`ExperimentDataProxy`).
    type DataFileText =
        | DataFileText of string

        member this.value = let (DataFileText t) = this in t

        /// The file's non-empty lines paired with their 1-based ORIGINAL line number (so a malformed-row error
        /// names the line the reader sees). Splits on `\n` and trims each line's `\r`/whitespace, so a pasted
        /// CRLF string parses; fully blank lines are dropped, never a header or a data row.
        member this.nonEmptyRows : (DataLineNumber * DataFileRow) list =
            this.value.Split('\n')
            |> Array.mapi (fun i raw -> DataLineNumber (i + 1), raw.Trim())
            |> Array.filter (fun (_, s) -> s.Length > 0)
            |> Array.map (fun (lineNo, s) -> lineNo, DataFileRow s)
            |> Array.toList

        /// Parse the text as an intensity data file (spec 034, STRICT schema v1): the first non-empty line is
        /// labels (ignored); every remaining non-empty line is an `X,Y` pair in invariant culture. An empty file
        /// (or a header with no data rows) is `EmptyDataFileErr`; the FIRST malformed row is a
        /// `MalformedDataFileErr` naming that row. No delimiter sniffing, no unit heuristics.
        member this.parseIntensitySeries : Result<IntensitySeries, ExperimentDataError> =
            match this.nonEmptyRows with
            | [] -> Error (EmptyDataFileErr "the intensity data file is empty (no header row and no data rows)")
            | [ _header ] -> Error (EmptyDataFileErr "the intensity data file has a header row but no data rows")
            | _header :: dataLines ->
                let rec loop (rows : (DataLineNumber * DataFileRow) list) (acc : IntensityPoint list) : Result<IntensitySeries, ExperimentDataError> =
                    match rows with
                    | [] -> Ok { points = List.rev acc }
                    | (lineNo, row) :: rest ->
                        match row.tryIntensityPoint with
                        | Some p -> loop rest (p :: acc)
                        | None -> Error (MalformedDataFileErr $"row {lineNo.value} is not a valid comma-separated X,Y pair: '{row.value}'")
                loop dataLines []

        /// Parse the text as an ellipsometric data file (spec 034, STRICT schema v1): the first non-empty line
        /// is the `wavelength_nm,psi_deg,delta_deg[,aoi_deg]` header (VALIDATED, not sniffed); every remaining
        /// non-empty line is 3 (or 4) invariant-culture floats. An empty file (or a header with no data rows)
        /// is `EmptyDataFileErr`; a bad header or the FIRST malformed row is a `MalformedDataFileErr` naming it.
        member this.parseEllipsometricSeries : Result<EllipsometricSeries, ExperimentDataError> =
            match this.nonEmptyRows with
            | [] -> Error (EmptyDataFileErr "the ellipsometric data file is empty (no header row and no data rows)")
            | (_, header) :: dataLines ->
                match EllipsometricColumns.tryOfHeader header with
                | Error e -> Error e
                | Ok columns ->
                    match dataLines with
                    | [] -> Error (EmptyDataFileErr "the ellipsometric data file has a header row but no data rows")
                    | _ ->
                        let rec loop (rows : (DataLineNumber * DataFileRow) list) (acc : EllipsometricPoint list) : Result<EllipsometricSeries, ExperimentDataError> =
                            match rows with
                            | [] -> Ok { points = List.rev acc }
                            | (lineNo, row) :: rest ->
                                match columns.tryPoint row with
                                | Some p -> loop rest (p :: acc)
                                | None -> Error (MalformedDataFileErr $"row {lineNo.value} is not a valid ellipsometric row (expected {columns.count} invariant-culture numbers): '{row.value}'")
                        loop dataLines []

    // -------------------------------------------------------------------------------------------------
    // Validation against a step-25 experiment (LOUD errors, never a silent trim).
    // -------------------------------------------------------------------------------------------------

    type VariableRange with

        /// Whether `x` (in the range's display unit) sits within the range, either way round, absorbing
        /// float noise at the endpoints with a tiny range-relative epsilon (so a tabulated 800.0 nm never
        /// spuriously overruns an 800 nm range top) — but still LOUD for a genuine overrun.
        member this.admits (x : float) : bool =
            let lo = min this.min.value this.max.value
            let hi = max this.min.value this.max.value
            let eps = 1e-9 * max 1.0 (max (abs lo) (abs hi))
            x >= lo - eps && x <= hi + eps

    /// Validate a parsed series against a step-25 experiment's varied parameter, numeric range, and
    /// detector kind (spec 034). Returns the series UNCHANGED on success (never trims / clips); otherwise a
    /// typed, LOUD mismatch:
    ///   - the series shape must match the detector kind (an intensity file for an ellipsometer, or an
    ///     ellipsometric file for an intensity detector, is a `DataUnitsMismatchErr`);
    ///   - an intensity series' X abscissa is in the varied quantity's display unit (nm / degrees) BY the
    ///     schema, so any X outside `[range.min, range.max]` is a `DataRangeMismatchErr`;
    ///   - an ellipsometric series against a wavelength sweep (`VaryWaveLength`) range-checks its
    ///     wavelength (nm) the same way; against a non-wavelength varied parameter the file's wavelength
    ///     abscissa is NOT the swept quantity, so the wavelength range check does not apply.
    let validateAgainstExperiment
        (varied : VariableParameter option)
        (range : VariableRange)
        (detector : DetectorKind)
        (series : MeasuredSeries)
        : Result<MeasuredSeries, ExperimentDataError> =
        let lo = min range.min.value range.max.value
        let hi = max range.min.value range.max.value
        let unit = varied |> Option.map (fun v -> v.unitLabel) |> Option.defaultValue ""
        match detector, series with
        | Intensity, EllipsometricData _ ->
            Error (DataUnitsMismatchErr "the detector records intensity (S0), but the data file is an ellipsometric Ψ/Δ series")
        | Ellipsometer, IntensityData _ ->
            Error (DataUnitsMismatchErr "the detector is an ellipsometer (records Ψ/Δ), but the data file is a single-intensity series")
        | Intensity, IntensityData s ->
            match s.points |> List.tryFind (fun p -> not (range.admits p.x.value)) with
            | Some p -> Error (DataRangeMismatchErr $"a measured abscissa {p.x.value} {unit} lies outside the experiment range [{lo}, {hi}] {unit}")
            | None -> Ok series
        | Ellipsometer, EllipsometricData s ->
            match varied with
            | Some VaryWaveLength ->
                match s.points |> List.tryFind (fun p -> not (range.admits (wavelengthToUnit Nanometer p.waveLength).value)) with
                | Some p ->
                    let nm = (wavelengthToUnit Nanometer p.waveLength).value
                    Error (DataRangeMismatchErr $"a measured wavelength {nm} nm lies outside the experiment range [{lo}, {hi}] nm")
                | None -> Ok series
            | Some VaryR1
            | Some VaryR2
            | None -> Ok series
