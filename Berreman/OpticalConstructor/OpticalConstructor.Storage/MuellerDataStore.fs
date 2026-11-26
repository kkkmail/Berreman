namespace OpticalConstructor.Storage

open System
open System.Globalization
open FSharp.Data
open Berreman.Geometry                                 // Angle — `description` is elevated to Angle.degree (never a bare degree)
open OpticalConstructor.Domain.MeasuredData           // MeasuredIntensity — `avg_total` is elevated at the parse
open OpticalConstructor.Domain.MuellerReconstruction

/// The Phone ROI CSV parser, the body of the `Phone ROI CSV` import profile (spec 0042 step 6; spec 0050 S9.2),
/// whose archive store spec 0050 S9.4 deleted.
module MuellerDataStore =

    let private inv = CultureInfo.InvariantCulture

    /// The required columns of a Mueller capture family, read BY NAME (the file carries many more —
    /// `iso`, `exposure_ns`, `avg_R/G/B`, … — that the reconstruction does not consume).
    let private requiredColumns = [ "experiment"; "capture_index"; "captured_at"; "description"; "avg_total" ]

    /// One data row of a capture file: its 1-based number among the data rows (the diagnostics name it), the
    /// header's name -> column lookup, and its already-split cells.
    type private CaptureCsvRow =
        | CaptureCsvRow of rowNumber : int * indexOf : (string -> int) * cols : string[]

        /// Parse the row into a `MuellerRawRow`, reading each required cell by its resolved column index. Every
        /// field is parsed invariant-culture; the first bad cell short-circuits to a typed `MalformedRowErr`
        /// naming the row, the column, and the offending value — never a throw. `description` (the raw analyzer
        /// dial angle in degrees) is elevated to the engine `Angle` via `Angle.degree`.
        member this.parse : Result<MuellerRawRow, MuellerDataError> =
            let (CaptureCsvRow (rowNumber, indexOf, cols)) = this
            let cell (name : string) : Result<string, MuellerDataError> =
                let i = indexOf name
                if i < cols.Length then Ok (cols.[i].Trim())
                else Error (MalformedRowErr $"data row {rowNumber}: the '{name}' cell is missing (the row is too short)")
            let asInt (name : string) : Result<int, MuellerDataError> =
                cell name
                |> Result.bind (fun s ->
                    match Int32.TryParse(s, NumberStyles.Integer, inv) with
                    | true, v -> Ok v
                    | _ -> Error (MalformedRowErr $"data row {rowNumber}: '{name}' is not an integer: '{s}'"))
            let asFloat (name : string) : Result<float, MuellerDataError> =
                cell name
                |> Result.bind (fun s ->
                    match Double.TryParse(s, NumberStyles.Float, inv) with
                    | true, v -> Ok v
                    | _ -> Error (MalformedRowErr $"data row {rowNumber}: '{name}' is not a number: '{s}'"))
            let asDateTimeOffset (name : string) : Result<DateTimeOffset, MuellerDataError> =
                cell name
                |> Result.bind (fun s ->
                    match DateTimeOffset.TryParse(s, inv, DateTimeStyles.AssumeUniversal) with
                    | true, v -> Ok v
                    | _ -> Error (MalformedRowErr $"data row {rowNumber}: '{name}' is not an ISO-8601 timestamp: '{s}'"))
            cell "experiment"
            |> Result.bind (fun experiment ->
                asInt "capture_index"
                |> Result.bind (fun captureIndex ->
                    asDateTimeOffset "captured_at"
                    |> Result.bind (fun capturedAt ->
                        asFloat "description"
                        |> Result.bind (fun descriptionDeg ->
                            asFloat "avg_total"
                            |> Result.map (fun avgTotal ->
                                {
                                    experiment = MuellerExperimentLabel experiment
                                    captureIndex = CaptureIndex captureIndex
                                    capturedAt = capturedAt
                                    description = Angle.degree (descriptionDeg * 1.0<Berreman.Constants.deg>)
                                    avgTotal = MeasuredIntensity avgTotal
                                })))))

    type DataFileText with

        /// Parse this Mueller capture family's CSV TEXT (§7.2 measured-data load). Header-aware, BOM-tolerant,
        /// invariant-culture, and PURE — it never touches the filesystem. Reuses `CsvFile.Parse` exactly as
        /// `SpectrumSidecarText.parseSpectrum` (SpectralImport.fs): the required `experiment`, `capture_index`,
        /// `captured_at`, `description` and `avg_total` columns are read BY NAME (a trimmed header->index map, so
        /// the many unread columns and any column re-ordering are tolerated); `description` is elevated to the
        /// engine `Angle`. The result is the ordered `MuellerRawRow` list, or a typed `MuellerDataError`:
        /// `MalformedRowErr` (a missing required column, a short/unparseable row) or `EmptyFileErr` (empty text, or a
        /// header with no capture rows). No exception crosses the boundary — the whole body is under try/with.
        member this.parseMuellerCsv : Result<MuellerRawRow list, MuellerDataError> =
            // A BOM decodes to a single U+FEFF character which `String.Trim()` does NOT remove on .NET Core, so the
            // first header would never match by name; the capture files carry one on the header row.
            let stripBom (text : string) : string = text.TrimStart(char 0xFEFF)
            if String.IsNullOrWhiteSpace this.value then
                Error (EmptyFileErr "the Mueller CSV text was empty")
            else
                try
                    let csv = CsvFile.Parse(stripBom this.value)
                    match csv.Headers with
                    | Some headers when headers.Length > 0 ->
                        let headerIndex =
                            headers
                            |> Array.mapi (fun i h -> (h.Trim(), i))
                            |> Map.ofArray
                        match requiredColumns |> List.filter (fun name -> not (Map.containsKey name headerIndex)) with
                        | _ :: _ as missing ->
                            Error (MalformedRowErr $"""the Mueller CSV is missing required column(s): {String.Join(", ", List.toArray missing)}""")
                        | [] ->
                            let indexOf (name : string) : int = Map.find name headerIndex
                            // Non-blank data rows only: FSharp.Data can surface a trailing all-empty row for a final
                            // newline, which is not a capture and must not be parsed as a malformed one.
                            let dataRows =
                                csv.Rows
                                |> Seq.mapi (fun i r -> (i + 1, r.Columns))
                                |> Seq.filter (fun (_, cols) -> cols |> Array.exists (fun c -> not (String.IsNullOrWhiteSpace c)))
                                |> Seq.toList
                            let parsed =
                                (Ok [], dataRows)
                                ||> List.fold (fun acc (rowNumber, cols) ->
                                    acc |> Result.bind (fun rows ->
                                        (CaptureCsvRow (rowNumber, indexOf, cols)).parse |> Result.map (fun row -> row :: rows)))
                                |> Result.map List.rev
                            match parsed with
                            | Error e -> Error e
                            | Ok [] -> Error (EmptyFileErr "the Mueller CSV carried a header but no capture rows")
                            | Ok rows -> Ok rows
                    | _ -> Error (MalformedRowErr "the Mueller CSV has no header row")
                with e -> Error (MalformedRowErr $"could not parse the Mueller CSV: {e.Message}")
