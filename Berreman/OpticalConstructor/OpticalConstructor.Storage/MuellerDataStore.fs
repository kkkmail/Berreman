namespace OpticalConstructor.Storage

open System
open System.IO
open System.Globalization
open FSharp.Data
open Berreman.Geometry                                 // Angle — `description` is elevated to Angle.degree (never a bare degree)
open OpticalConstructor.Domain.Experiments             // DataFilePath (step 025) — the CSV-load seam key
open OpticalConstructor.Domain.MuellerReconstruction   // MuellerRawRow / MuellerDataError / MuellerDataProxy (DECLARED, step 005)

/// Spec 0042 (006, IMPLEMENT_CONTRACT STORE_XDUO_0009) — the real file-backed adapter behind the DECLARED
/// `MuellerDataProxy` (step 005). Two pieces:
///
///   • `parseMuellerCsv` — the PURE parser: header-aware, BOM-tolerant, invariant-culture. It reuses
///     `FSharp.Data`'s `CsvFile.Parse` EXACTLY as `SpectralImport.parseSpectrumCsv` (SpectralImport.fs:46) —
///     parse the TEXT, read `csv.Headers` / `csv.Rows`, `r.Columns` — but reads the `experiment`,
///     `capture_index`, `captured_at`, `description` and `avg_total` columns BY NAME (a trimmed header->index
///     map), elevating `description` to the engine `Angle` via `Angle.degree`. It never touches the
///     filesystem and never throws past the boundary: every parse failure is a typed `MuellerDataError`
///     (`MalformedRow` for a bad column/row, `EmptyFile` for a header with no capture rows).
///
///   • `createFileBacked` — the runtime replacement for the step-005 in-memory mock (canned rows keyed by
///     `DataFilePath.value`, in `BerremanTests`). It is the missing `path -> read CSV text -> rows` EDGE:
///     `File.ReadAllText` is the boundary, wrapped so any .NET IO exception (a missing file, a missing
///     directory, an access failure) is caught HERE and mapped to a typed `MuellerDataError` — an exception
///     NEVER crosses into the pure Domain — then the text is delegated UNCHANGED to `parseMuellerCsv`. This
///     mirrors `ExperimentDataStore.loadThrough` / `createFileBacked` (ExperimentDataStore.fs:34, :51).
module MuellerDataStore =

    let private inv = CultureInfo.InvariantCulture

    /// The required columns of a Mueller capture family, read BY NAME (the file carries many more —
    /// `iso`, `exposure_ns`, `avg_R/G/B`, … — that the reconstruction does not consume).
    let private requiredColumns = [ "experiment"; "capture_index"; "captured_at"; "description"; "avg_total" ]

    /// Strip a leading UTF-8 BOM. A BOM decodes to a single U+FEFF character which `String.Trim()` does NOT
    /// remove on .NET Core, so the first header would read "experiment" and never match by name; strip
    /// it explicitly to stay BOM-tolerant (the capture files carry a UTF-8 BOM on the header row).
    let private stripBom (text : string) : string = text.TrimStart(char 0xFEFF)

    /// Parse one already-column-split data row into a `MuellerRawRow`, reading each required cell by its
    /// resolved column index. Every field is parsed invariant-culture; the first bad cell short-circuits to a
    /// typed `MalformedRow` naming the row, the column, and the offending value — never a throw. `description`
    /// (the raw analyzer dial angle in degrees) is elevated to the engine `Angle` via `Angle.degree`.
    let private parseRow
        (rowNumber : int)
        (indexOf : string -> int)
        (cols : string[])
        : Result<MuellerRawRow, MuellerDataError> =
        let cell (name : string) : Result<string, MuellerDataError> =
            let i = indexOf name
            if i < cols.Length then Ok (cols.[i].Trim())
            else Error (MalformedRow $"data row {rowNumber}: the '{name}' cell is missing (the row is too short)")
        let asInt (name : string) : Result<int, MuellerDataError> =
            cell name
            |> Result.bind (fun s ->
                match Int32.TryParse(s, NumberStyles.Integer, inv) with
                | true, v -> Ok v
                | _ -> Error (MalformedRow $"data row {rowNumber}: '{name}' is not an integer: '{s}'"))
        let asFloat (name : string) : Result<float, MuellerDataError> =
            cell name
            |> Result.bind (fun s ->
                match Double.TryParse(s, NumberStyles.Float, inv) with
                | true, v -> Ok v
                | _ -> Error (MalformedRow $"data row {rowNumber}: '{name}' is not a number: '{s}'"))
        let asDateTimeOffset (name : string) : Result<DateTimeOffset, MuellerDataError> =
            cell name
            |> Result.bind (fun s ->
                match DateTimeOffset.TryParse(s, inv, DateTimeStyles.AssumeUniversal) with
                | true, v -> Ok v
                | _ -> Error (MalformedRow $"data row {rowNumber}: '{name}' is not an ISO-8601 timestamp: '{s}'"))
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
                                experiment = experiment
                                captureIndex = captureIndex
                                capturedAt = capturedAt
                                description = Angle.degree descriptionDeg
                                avgTotal = avgTotal
                            })))))

    /// Parse a Mueller capture family from CSV TEXT (§7.2 measured-data load). Header-aware, BOM-tolerant,
    /// invariant-culture, and PURE — it never touches the filesystem. Reuses `CsvFile.Parse` exactly as
    /// `SpectralImport.parseSpectrumCsv` (SpectralImport.fs:46): the required `experiment`, `capture_index`,
    /// `captured_at`, `description` and `avg_total` columns are read BY NAME (a trimmed header->index map, so
    /// the many unread columns and any column re-ordering are tolerated); `description` is elevated to the
    /// engine `Angle`. The result is the ordered `MuellerRawRow` list, or a typed `MuellerDataError`:
    /// `MalformedRow` (a missing required column, a short/unparseable row) or `EmptyFile` (empty text, or a
    /// header with no capture rows). No exception crosses the boundary — the whole body is under try/with.
    let parseMuellerCsv (csvText : string) : Result<MuellerRawRow list, MuellerDataError> =
        if String.IsNullOrWhiteSpace csvText then
            Error (EmptyFile "the Mueller CSV text was empty")
        else
            try
                let csv = CsvFile.Parse(stripBom csvText)
                match csv.Headers with
                | Some headers when headers.Length > 0 ->
                    let headerIndex =
                        headers
                        |> Array.mapi (fun i h -> (h.Trim(), i))
                        |> Map.ofArray
                    match requiredColumns |> List.filter (fun name -> not (Map.containsKey name headerIndex)) with
                    | _ :: _ as missing ->
                        Error (MalformedRow $"""the Mueller CSV is missing required column(s): {String.Join(", ", List.toArray missing)}""")
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
                                    parseRow rowNumber indexOf cols |> Result.map (fun row -> row :: rows)))
                            |> Result.map List.rev
                        match parsed with
                        | Error e -> Error e
                        | Ok [] -> Error (EmptyFile "the Mueller CSV carried a header but no capture rows")
                        | Ok rows -> Ok rows
                | _ -> Error (MalformedRow "the Mueller CSV has no header row")
            with e -> Error (MalformedRow $"could not parse the Mueller CSV: {e.Message}")

    /// Read the file at `path` and hand its TEXT to `parse` (the pure `parseMuellerCsv`). Filesystem access is
    /// the boundary: a missing file, a missing directory, or any other .NET IO exception is caught HERE and
    /// mapped to a typed `MalformedRow` naming the path and the underlying reason — never a throw across into
    /// the pure Domain, and staying WITHIN the DECLARED two-case channel (the step-005 `MalformedRow` doc:
    /// "a native/IO failure mapped onto the channel"). The parse itself is delegated UNCHANGED, so every
    /// missing-column / malformed-row / empty-file diagnostic is `parseMuellerCsv`'s, not this adapter's.
    let private loadThrough
        (parse : string -> Result<MuellerRawRow list, MuellerDataError>)
        (path : DataFilePath)
        : Result<MuellerRawRow list, MuellerDataError> =
        let read =
            try Ok (File.ReadAllText path.value)
            with e -> Error (MalformedRow $"could not read the Mueller CSV data file '{path.value}': {e.Message}")
        match read with
        | Error e -> Error e
        | Ok text -> parse text

    /// Build the real file-backed `MuellerDataProxy` (the `IMPLEMENT_CONTRACT STORE_XDUO_0009` construction
    /// behind the step-005 DECLARED seam): `tryLoadFamily` resolves an experiment family's elevated
    /// `DataFilePath` by reading the file's text at this IO boundary and delegating to `parseMuellerCsv`.
    /// Replaces the step-005 in-memory mock for a real store, leaving every consumer that holds the proxy
    /// unchanged.
    let createFileBacked () : MuellerDataProxy =
        {
            tryLoadFamily = loadThrough parseMuellerCsv
        }
