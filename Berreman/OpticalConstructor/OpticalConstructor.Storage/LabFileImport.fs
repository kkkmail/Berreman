namespace OpticalConstructor.Storage

open System
open System.Globalization
open FSharp.Data
open Berreman.Constants
open Berreman.Geometry
open Berreman.Fields
open Berreman.Media
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.MeasuredData
open OpticalConstructor.Domain.PropagationReadouts
open OpticalConstructor.Domain.MuellerReconstruction
open OpticalConstructor.Domain.MuellerInverse
open OpticalConstructor.Domain.ImportProfiles
open OpticalConstructor.Storage.MuellerDataStore

/// The Mueller-matrix observation file, which the app writes and the `Mueller-matrix observations` profile reads back
/// (001 §Q AT-2), a kit profile's column mapping of any delimited lab file and its preview, and the one read of a lab
/// file through any profile (spec 0050 S9.2; 001 §K).
module LabFileImport =

    /// The observation file's columns in order, the one place its header is written (spec 0050 S9.2).
    let private observationFileColumns : string list =
        [ "sample"; "cut"; "cut_tilt_rad"; "thickness_m"; "incidence_rad"; "azimuth_rad"; "observable"; "wavelength_nm" ]
        @ (MuellerElementIndex.all |> List.map (fun index -> index.text))
        @ [ "throughput" ]

    /// A lab file's header cells, when its first row is a header, and its data rows' cells, read by FSharp.Data's
    /// `CsvFile` as `parseMuellerCsv` reads: a row of blank cells is skipped, and a refusal of the reader is the
    /// error. Every reading of a lab file by a layout goes through it (spec 0050 S9.2).
    let private labFileRows (delimiter : DataDelimiter) (header : HeaderRowChoice) (text : KitFileText)
        : Result<DataFileCell list option * DataFileCell list list, ExperimentDataError> =
        let cellsOf (columns : string[]) : DataFileCell list = columns |> Array.map DataFileCell |> Array.toList

        if String.IsNullOrWhiteSpace text.value then
            Error (EmptyDataFileErr "the lab file is empty")
        else
            try
                let hasHeaders =
                    match header with
                    | HeaderOnFirstRow -> true
                    | NoHeaderRow -> false

                let csv = CsvFile.Parse(text.value, separators = string delimiter.character, hasHeaders = hasHeaders)

                // FSharp.Data checks a row's width as it enumerates, so the rows are read inside the `try`.
                let rows =
                    csv.Rows
                    |> Seq.map (fun row -> row.Columns)
                    |> Seq.filter (fun columns -> columns |> Array.exists (fun cell -> not (String.IsNullOrWhiteSpace cell)))
                    |> Seq.map cellsOf
                    |> Seq.toList

                Ok (csv.Headers |> Option.map cellsOf, rows)
            with error ->
                Error (MalformedDataFileErr $"the lab file's rows could not be read by {delimiter.text}: {error.Message}")

    type KeyedObservation with

        /// The header, then one row per observation of its key, configuration, normalized m00…m33 and throughput, every
        /// number in round-trip text; the first observation it cannot write is refused by position and key (spec 0050 S9.2).
        static member observationFileText (observations : KeyedObservation list) : Result<KitFileText, ExperimentDataError> =
            let number (x : float) : string = x.ToString("R", CultureInfo.InvariantCulture)
            let tag (x : int) : string = x.ToString(CultureInfo.InvariantCulture)

            let rowOf (position : int) (keyed : KeyedObservation) : Result<string, ExperimentDataError> =
                let key = keyed.key.value
                let configuration = keyed.observation.configuration
                let measured = keyed.observation.measured

                let refused (reason : string) : Result<string, ExperimentDataError> =
                    Error (MalformedDataFileErr $"observation {position} ('{key}'): {reason}")

                if key.Length = 0 || key.IndexOfAny [| ','; '"'; '\r'; '\n' |] >= 0 then
                    refused "a key must be non-empty and hold no comma, quote, carriage return or line feed"
                else
                    match configuration.thickness.metersOpt, configuration.waveLength, measured.normalized with
                    | None, _, _ -> refused "an infinite thickness has no metres to write"
                    | Some _, Mkm _, _ -> refused "the file records the wavelength in nanometres, and this one is in micrometres"
                    | Some _, Nm _, Error _ ->
                        refused $"m00 = {number (measured.element 0 0)} is not a positive intensity, so the matrix has no normalized form"
                    | Some metres, Nm nanometres, Ok normalized ->
                        [
                            key
                            tag configuration.cut.value
                            number configuration.cut.tilt.value
                            number metres
                            number configuration.incidenceAngle.value
                            number configuration.azimuth.value
                            tag configuration.observable.value
                            number (float nanometres)
                            for i in 0 .. 3 do
                                for j in 0 .. 3 do
                                    number (normalized.element i j)
                            number (measured.element 0 0)
                        ]
                        |> String.concat ","
                        |> Ok

            match observations |> List.mapi (fun index keyed -> rowOf (index + 1) keyed) |> Softellect.Sys.Rop.unzip with
            | rows, [] ->
                String.concat "," observationFileColumns :: rows
                |> List.map (fun line -> line + "\n")
                |> String.concat ""
                |> KitFileText
                |> Ok
            | _, error :: _ -> Error error

    type KitFileText with

        /// The keyed observations an observation file holds, in row order, each matrix its throughput times the normalized
        /// elements; another shape is refused, naming the first line, and cell, that is not this one (spec 0050 S9.2).
        member this.parseMuellerObservations : Result<KeyedObservation list, ExperimentDataError> =
            let header = String.concat "," observationFileColumns
            let columnCount = List.length observationFileColumns

            let lines =
                this.value.Split('\n')
                |> Array.mapi (fun index line -> index + 1, (if line.EndsWith('\r') then line.Substring(0, line.Length - 1) else line))
                |> Array.filter (fun (_, line) -> not (String.IsNullOrWhiteSpace line))
                |> Array.toList

            let rowOf (lineNumber : int) (line : string) : Result<KeyedObservation, ExperimentDataError> =
                let cells = line.Split(',')

                let refused (cell : int) (reason : string) : ExperimentDataError =
                    MalformedDataFileErr $"line {lineNumber}, cell {cell} ({List.item (cell - 1) observationFileColumns}): '{cells[cell - 1]}' {reason}"

                let numberAt (cell : int) : Result<float, ExperimentDataError> =
                    match Double.TryParse(cells[cell - 1], NumberStyles.Float, CultureInfo.InvariantCulture) with
                    | true, x -> Ok x
                    | false, _ -> Error (refused cell "is not a number")

                let tagAt (cell : int) : Result<int, ExperimentDataError> =
                    match Int32.TryParse(cells[cell - 1], NumberStyles.Integer, CultureInfo.InvariantCulture) with
                    | true, x -> Ok x
                    | false, _ -> Error (refused cell "is not a tag")

                let numbersOf (first : int) (last : int) : Result<float[], ExperimentDataError> =
                    match [ first .. last ] |> List.map numberAt |> Softellect.Sys.Rop.unzip with
                    | numbers, [] -> Ok (List.toArray numbers)
                    | _, error :: _ -> Error error

                let cutOf (cutTag : int) (tilt : float) : Result<SampleCut, ExperimentDataError> =
                    match SampleCut.tryOfWire cutTag (Angle tilt) with
                    | Some cut -> Ok cut
                    | None -> Error (refused 2 $"with the tilt {cells[2]} names no cut")

                let observableOf (observableTag : int) : Result<Observable, ExperimentDataError> =
                    match MuellerInverse.Observable.tryOfWire observableTag with
                    | Some observable -> Ok observable
                    | None -> Error (refused 7 "names no observable")

                // The cells, left to right: key; cut tag and tilt; metres, incidence, azimuth; observable tag; then the
                // reading, which is the wavelength, the sixteen normalized elements and the throughput.
                if cells.Length <> columnCount then
                    Error (MalformedDataFileErr $"line {lineNumber}: a row holds {columnCount} cells, and this one holds {cells.Length}")
                else
                    tagAt 2
                    |> Result.bind (fun cutTag -> numberAt 3 |> Result.bind (cutOf cutTag))
                    |> Result.bind (fun cut ->
                        numbersOf 4 6
                        |> Result.bind (fun placement ->
                            tagAt 7
                            |> Result.bind observableOf
                            |> Result.bind (fun observable ->
                                numbersOf 8 columnCount
                                |> Result.map (fun reading ->
                                    let throughput = reading[17]

                                    let configuration : MeasurementConfiguration =
                                        {
                                            cut = cut
                                            thickness = Thickness (placement[0] * 1.0<meter>)
                                            incidenceAngle = IncidenceAngle (Angle (placement[1]))
                                            azimuth = SampleAzimuth (Angle (placement[2]))
                                            observable = observable
                                            waveLength = WaveLength.nm (reading[0] * 1.0<nm>)
                                        }

                                    let measured =
                                        RealMatrix4x4.create
                                            [ for i in 0 .. 3 ->
                                                [ for j in 0 .. 3 ->
                                                    if i = 0 && j = 0 then throughput else throughput * reading[1 + 4 * i + j] ] ]
                                        |> MuellerMatrix

                                    let observation : MuellerObservation = { configuration = configuration; measured = measured }
                                    let keyed : KeyedObservation = { key = DataRowKey (cells[0]); observation = observation }
                                    keyed))))

            match lines with
            | [] -> Error (EmptyDataFileErr "the observation file is empty")
            | (lineNumber, first) :: _ when first <> header ->
                Error (MalformedDataFileErr $"line {lineNumber}: the header must be '{header}', and it is '{first}'")
            | [ _ ] -> Error (EmptyDataFileErr "the observation file has its header and no observation row")
            | _ :: rows ->
                match rows |> List.map (fun (lineNumber, line) -> rowOf lineNumber line) |> Softellect.Sys.Rop.unzip with
                | observations, [] -> Ok observations
                | _, error :: _ -> Error error

        /// The text's column names and first twenty data rows through the mappings' one read, each cell trimmed as a
        /// mapping reads it: the header's cells, else `Column <n>` for each column of the widest row shown. An empty text
        /// is `EmptyDataFileErr`, and the read's refusal passes through (spec 0050 S9.2).
        member this.previewWith (delimiter : DataDelimiter) (header : HeaderRowChoice) : Result<LabFilePreview, ExperimentDataError> =
            let shownOf (cell : DataFileCell) : PreviewCell = PreviewCell (cell.value.Trim())

            labFileRows delimiter header this
            |> Result.map (fun (headerOpt, rows) ->
                let shown = List.truncate 20 rows

                let columnNames =
                    match headerOpt with
                    | Some cells -> cells |> List.map shownOf
                    | None ->
                        let width = shown |> List.map List.length |> List.fold max 0
                        [ for n in 1 .. width -> PreviewCell $"Column {n}" ]

                let preview : LabFilePreview =
                    {
                        columnNames = columnNames
                        rows = shown |> List.map (List.map shownOf >> PreviewRow)
                    }

                preview)

    type ImportProfile with

        /// The lab file's data rows read through this profile's delimiter, header and column roles, in row order. A row is
        /// refused at its leftmost cell that is missing, blank or does not read, naming the data row, the column and the
        /// cell; an unmapped column is read by nothing (spec 0050 S9.2; 001 §K).
        member this.mapText (text : KitFileText) : Result<MappedRow list, ExperimentDataError> =
            let roles = this.roles
            let invariant = CultureInfo.InvariantCulture
            let trimmedOf (cell : DataFileCell) : string = cell.value.Trim()

            let numberOf (cell : DataFileCell) : float option =
                match Double.TryParse(trimmedOf cell, NumberStyles.Float, invariant) with
                | true, x -> Some x
                | false, _ -> None

            // `parseMuellerCsv`'s timestamp rule.
            let timestampOf (cell : DataFileCell) : RowTimestamp option =
                match DateTimeOffset.TryParse(trimmedOf cell, invariant, DateTimeStyles.AssumeUniversal) with
                | true, instant -> Some (RowTimestamp instant)
                | false, _ -> None

            let captureIndexOf (cell : DataFileCell) : CaptureIndex option =
                match Int32.TryParse(trimmedOf cell, NumberStyles.Integer, invariant) with
                | true, index -> Some (CaptureIndex index)
                | false, _ -> None

            let keyOf (cell : DataFileCell) : DataRowKey option = Some (DataRowKey (trimmedOf cell))

            // A refusal keeps its column, so of two refused cells the row reports the leftmost, whichever role reads it.
            let both
                (first : Result<'First, DataColumnNumber * ExperimentDataError>)
                (second : Result<'Second, DataColumnNumber * ExperimentDataError>)
                : Result<'First * 'Second, DataColumnNumber * ExperimentDataError> =
                match first, second with
                | Ok firstValue, Ok secondValue -> Ok (firstValue, secondValue)
                | Error refusal, Ok _
                | Ok _, Error refusal -> Error refusal
                | Error (column, error), Error (otherColumn, otherError) ->
                    if otherColumn < column then Error (otherColumn, otherError) else Error (column, error)

            let all (results : Result<'Value, DataColumnNumber * ExperimentDataError> list)
                : Result<'Value list, DataColumnNumber * ExperimentDataError> =
                let prepend result values = both result values |> Result.map (fun (value, rest) -> value :: rest)
                List.foldBack prepend results (Ok [])

            let optionalAt
                (read : DataColumnNumber -> Result<'Value, DataColumnNumber * ExperimentDataError>)
                (columnOpt : DataColumnNumber option)
                : Result<'Value option, DataColumnNumber * ExperimentDataError> =
                match columnOpt with
                | Some column -> read column |> Result.map Some
                | None -> Ok None

            let rowOf (rowNumber : int) (cells : DataFileCell list) : Result<MappedRow, ExperimentDataError> =
                let readAt
                    (what : string)
                    (tryRead : DataFileCell -> 'Value option)
                    (column : DataColumnNumber)
                    : Result<'Value, DataColumnNumber * ExperimentDataError> =
                    let refused (reason : string) : Result<'Value, DataColumnNumber * ExperimentDataError> =
                        Error (column, MalformedDataFileErr $"data row {rowNumber}, column {column.value}: {reason}")

                    match List.tryItem column.index cells with
                    | None -> refused $"the row holds {List.length cells} cells, so this one is missing"
                    | Some cell when String.IsNullOrWhiteSpace cell.value -> refused $"'{cell.value}' is blank"
                    | Some cell ->
                        match tryRead cell with
                        | Some value -> Ok value
                        | None -> refused $"'{trimmedOf cell}' is not {what}"

                let numberAt (column : DataColumnNumber) : Result<float, DataColumnNumber * ExperimentDataError> =
                    readAt "a number" numberOf column

                let scanResult : Result<ScanCell list, DataColumnNumber * ExperimentDataError> =
                    roles.scan
                    |> List.map (fun scanColumn ->
                        readAt "a number" (numberOf >> Option.map Units.UnitMagnitude) scanColumn.column
                        |> Result.map (fun magnitude ->
                            let cell : ScanCell = { column = scanColumn; magnitude = magnitude }
                            cell))
                    |> all

                let readingResult : Result<MappedReading, DataColumnNumber * ExperimentDataError> =
                    match roles.reading with
                    | SignalColumns (signal, sigmaOpt) ->
                        both
                            (readAt "a number" (numberOf >> Option.map MeasuredIntensity) signal)
                            (optionalAt (readAt "a number" (numberOf >> Option.map MeasuredSigma)) sigmaOpt)
                        |> Result.map SignalReading
                    | MuellerElementColumns (elements, throughputOpt) ->
                        both (elements |> List.map numberAt |> all) (optionalAt numberAt throughputOpt)
                        |> Result.map (fun (values, throughputValueOpt) ->
                            let absolute =
                                match throughputValueOpt with
                                | Some throughput -> values |> List.map (fun value -> throughput * value)
                                | None -> values

                            // Element (i, j) is the column of index 4i + j, so the rows are the values in fours.
                            absolute |> List.chunkBySize 4 |> RealMatrix4x4.create |> MuellerMatrix |> MuellerReading)

                let detailsResult =
                    both
                        (optionalAt (readAt "a timestamp" timestampOf) roles.timestampOpt)
                        (both
                            (optionalAt (readAt "a capture index" captureIndexOf) roles.captureIndexOpt)
                            (both
                                (optionalAt (readAt "T, R or an observable tag" MuellerInverse.Observable.tryOfCell) roles.observableOpt)
                                (optionalAt (readAt "a key" keyOf) roles.keyOpt)))

                both scanResult (both readingResult detailsResult)
                |> Result.map (fun (scan, (reading, (timestampOpt, (captureIndexOpt, (observableOpt, keyOpt))))) ->
                    let row : MappedRow =
                        {
                            keyOpt = keyOpt
                            scan = scan
                            reading = reading
                            timestampOpt = timestampOpt
                            captureIndexOpt = captureIndexOpt
                            observableOpt = observableOpt
                        }

                    row)
                |> Result.mapError snd

            let elementCount = List.length MuellerElementIndex.all

            match roles.reading with
            | MuellerElementColumns (elements, _) when List.length elements <> elementCount ->
                let mapped = List.length elements
                Error (MalformedDataFileErr $"the profile '{this.name.value}' maps {mapped} Mueller element columns, and a matrix has {elementCount}")
            | MuellerElementColumns _
            | SignalColumns _ ->
                labFileRows this.delimiter this.header text
                |> Result.bind (fun (_, rows) ->
                    match rows with
                    | [] -> Error (EmptyDataFileErr "the lab file has no data row")
                    | _ ->
                        match rows |> List.mapi (fun index cells -> rowOf (index + 1) cells) |> Softellect.Sys.Rop.unzip with
                        | mapped, [] -> Ok mapped
                        | _, error :: _ -> Error error)

    type ChosenImportProfile with

        /// The lab file read through this profile: a built-in answers its parser's points and refusals unchanged, Phone
        /// ROI's on the data channel, and a kit profile its column mapping's (spec 0050 S9.2; 001 §K, §R).
        member this.mapText (text : KitFileText) : Result<MappedPoints, ExperimentDataError> =
            match this with
            | BuiltInProfileChoice IntensitySeriesProfile ->
                text.dataFileText.parseIntensitySeries |> Result.map (IntensityData >> SeriesPoints)
            | BuiltInProfileChoice EllipsometricSeriesProfile ->
                text.dataFileText.parseEllipsometricSeries |> Result.map (EllipsometricData >> SeriesPoints)
            | BuiltInProfileChoice PhoneRoiCsvProfile ->
                // A text parse never answers the two archive cases; they are mapped so the match stays total.
                text.dataFileText.parseMuellerCsv
                |> Result.map CapturePoints
                |> Result.mapError (fun error ->
                    match error with
                    | MalformedRowErr reason -> MalformedDataFileErr reason
                    | EmptyFileErr reason -> EmptyDataFileErr reason
                    | ArchiveUnreadableErr (_, reason) -> MalformedDataFileErr reason
                    | DataSetMissingErr (_, archive) -> MalformedDataFileErr archive.value)
            | BuiltInProfileChoice MuellerObservationProfile -> text.parseMuellerObservations |> Result.map ObservationPoints
            | KitProfileChoice profile -> profile.mapText text |> Result.map ColumnPoints
