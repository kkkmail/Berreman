namespace OpticalConstructor.Storage

open System
open System.IO
open System.IO.Compression                             // ZipFile / ZipArchive — the measured data ships as one committed archive
open System.Globalization
open FSharp.Data
open Berreman.Geometry                                 // Angle — `description` is elevated to Angle.degree (never a bare degree)
open OpticalConstructor.Domain.MuellerReconstruction   // MuellerRawRow / MuellerDataError / MuellerDataProxy / MuellerDataSet

/// Spec 0042 (006, IMPLEMENT_CONTRACT STORE_XDUO_0009) — the real adapter behind the DECLARED
/// `MuellerDataProxy` (step 005), re-pointed at the committed archive by spec 0044 §8.3. Three pieces:
///
///   • `parseMuellerCsv` — the PURE parser: header-aware, BOM-tolerant, invariant-culture. It reuses
///     `FSharp.Data`'s `CsvFile.Parse` EXACTLY as `SpectralImport.parseSpectrumCsv` (SpectralImport.fs:46) —
///     parse the TEXT, read `csv.Headers` / `csv.Rows`, `r.Columns` — but reads the `experiment`,
///     `capture_index`, `captured_at`, `description` and `avg_total` columns BY NAME (a trimmed header->index
///     map), elevating `description` to the engine `Angle` via `Angle.degree`. It never touches the
///     filesystem and never throws past the boundary: every parse failure is a typed `MuellerDataError`
///     (`MalformedRow` for a bad column/row, `EmptyFile` for a header with no capture rows).
///
///   • `tryLoadFromArchive` — the storage EDGE: `MuellerArchivePath -> MuellerDataSet -> rows`. It owns the
///     `MuellerDataSet -> archive entry name` mapping, opens the zip, reads that entry's text, and delegates
///     the text UNCHANGED to `parseMuellerCsv`. Every .NET IO / compression exception is caught HERE and
///     mapped to a typed `MuellerDataError` — an exception NEVER crosses into the pure Domain.
///
///   • `createArchiveBacked` — the composition-root factory. It takes NO ARGUMENT: it resolves the committed
///     archive itself and partially applies `tryLoadFromArchive` to it, so a consumer holding the resulting
///     proxy states only WHICH data set it wants and knows nothing about archives, paths or entry names
///     (spec 0044 §8.3 / R5).
///
/// Spec 0044 §8.3 DELETED the previous `createFileBacked` / `loadThrough` pair. Those keyed the seam by
/// `DataFilePath`, which is a location, and the seam is now keyed by identity (`MuellerDataSet`); a
/// path-taking factory can no longer satisfy it. `parseMuellerCsv` — the valuable, pure part — is untouched
/// and keeps its own direct tests.
module MuellerDataStore =

    /// The on-disk location of the committed measured-data archive, elevated so no bare string is ever passed
    /// into a factory. It is a STORAGE concept and deliberately lives here rather than in the Domain: nothing
    /// above this boundary is allowed to learn that the data is packaged as an archive at all.
    type MuellerArchivePath =
        | MuellerArchivePath of string

        /// The raw path (the IO seam — read only where the filesystem is actually touched).
        member this.value = let (MuellerArchivePath p) = this in p

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

    /// The archive entry that carries each data set — the ONE place the identity → location mapping exists
    /// (spec 0044 §8.3). Zip entry names always use forward slashes, whatever the host platform. The five
    /// science sets plus the two check sets live under `data/raw/final/`; the archive also carries earlier
    /// exploratory sweeps under `data/raw/` and beam-shape descriptions under `data/shapes/`, none of which
    /// any data set names (see `Berreman/Data/MuellerMatrix/README.md` for the full inventory).
    let private entryName (dataSet : MuellerDataSet) : string =
        match dataSet with
        | LpLpFamily -> "data/raw/final/lp_lp.csv"
        | LpCplFamily -> "data/raw/final/lp_cpl.csv"
        | CplLpFamily -> "data/raw/final/cpl_lp.csv"
        | CplCplDay1 -> "data/raw/final/cpl_cpl_day1_main.csv"
        | CplCplDay2 -> "data/raw/final/cpl_cpl_day2_corrections.csv"
        | DarknessChecks -> "data/raw/final/darkness_checks.csv"
        | BullshitChecks -> "data/raw/final/bullshit_checks.csv"

    /// The committed measured-data archive, relative to the consuming assembly's output directory. The
    /// archive is copied there by the consuming project (`BerremanTests.fsproj` carries it as `Content` with
    /// `CopyToOutputDirectory`), exactly as `OpticalConstructor.Tests` copies its `fixtures\*`. Resolving it
    /// from `AppContext.BaseDirectory` — rather than from the source tree — is what makes the lookup
    /// independent of the working directory the test runner happens to choose.
    let private committedArchive () : MuellerArchivePath =
        Path.Combine(AppContext.BaseDirectory, "Data", "MuellerMatrix", "data.zip") |> MuellerArchivePath

    /// Read one data set out of the archive and hand its TEXT to `parseMuellerCsv`. This is the storage
    /// boundary and it is TOTAL: opening the archive, finding the entry and reading it all run under
    /// `try/with`, and every failure becomes a typed `MuellerDataError` — never a throw across into the pure
    /// Domain. The two archive-specific failures are reported distinctly because they call for different
    /// fixes: `ArchiveUnreadable` means the container is missing or corrupt (a deployment problem — the
    /// `Content` copy did not happen), while `DataSetMissing` means the container opened but does not carry
    /// that entry (a data problem — the archive is the wrong build). The parse itself is delegated
    /// UNCHANGED, so every missing-column / malformed-row / empty-file diagnostic is `parseMuellerCsv`'s.
    ///
    /// The archive is opened per call. That is deliberate: it is ~62 KB, a whole run touches it seven times,
    /// and caching would add lifetime and thread-safety questions for no measurable gain.
    let tryLoadFromArchive
        (archive : MuellerArchivePath)
        (dataSet : MuellerDataSet)
        : Result<MuellerRawRow list, MuellerDataError> =
        let entry = entryName dataSet
        let read =
            try
                use zip = ZipFile.OpenRead archive.value
                match zip.GetEntry entry with
                | null -> Error (DataSetMissing (dataSet, archive.value))
                | e ->
                    use stream = e.Open()
                    use reader = new StreamReader(stream)
                    Ok (reader.ReadToEnd())
            with e -> Error (ArchiveUnreadable (archive.value, e.Message))
        match read with
        | Error e -> Error e
        | Ok text -> parseMuellerCsv text

    /// Build the real archive-backed `MuellerDataProxy` (the `IMPLEMENT_CONTRACT STORE_XDUO_0009`
    /// construction behind the step-005 DECLARED seam, re-pointed by spec 0044 §8.3).
    ///
    /// It takes NO ARGUMENT by design. The whole point of the 0044 re-key is that a consumer names a data set
    /// and nothing else: this factory resolves the committed archive itself and partially applies
    /// `tryLoadFromArchive` to it, so the archive, its location and the entry naming never appear in any
    /// caller. `tryLoadFromArchive` stays public purely so the archive-failure paths remain reachable from a
    /// test (by pointing it at a deliberately absent archive) without any production caller ever naming a
    /// location.
    let createArchiveBacked () : MuellerDataProxy =
        {
            tryLoadDataSet = tryLoadFromArchive (committedArchive ())
        }
