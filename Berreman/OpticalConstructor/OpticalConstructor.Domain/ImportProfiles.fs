namespace OpticalConstructor.Domain

open System
open System.Globalization
open Berreman.Geometry
open Berreman.Fields
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.Experiments
open OpticalConstructor.Domain.MeasuredData
open OpticalConstructor.Domain.MuellerReconstruction
open OpticalConstructor.Domain.MuellerInverse

/// The built-in import profiles, the profile an experiment names, the points a profile maps a lab file to, their one
/// validation against an experiment's scan and detector, and an experiment's rows of them (spec 0050 S9.2; 001 §K).
module ImportProfiles =

    /// One cell of a lab file's row, as its file wrote it (spec 0050 S9.2).
    type DataFileCell =
        | DataFileCell of string

        member this.value : string = let (DataFileCell text) = this in text

    type Observable with

        /// The observable's disk tag, 1 to 2 in case order (spec 0050 S9.2).
        member this.value : int =
            match this with
            | TransmittedMueller -> 1
            | ReflectedMueller -> 2

        /// The observable a disk tag names; any other tag is `None`, the caller's typed error (spec 0050 S9.2).
        static member tryOfWire (tag : int) : Observable option =
            match tag with
            | 1 -> Some TransmittedMueller
            | 2 -> Some ReflectedMueller
            | _ -> None

        /// A lab file's observable cell, trimmed: `T` or `R` in either case, else its disk tag; `None` for anything
        /// else (spec 0050 S9.2).
        static member tryOfCell (cell : DataFileCell) : Observable option =
            match cell.value.Trim().ToUpperInvariant() with
            | "T" -> Some TransmittedMueller
            | "R" -> Some ReflectedMueller
            | text ->
                match Int32.TryParse(text, NumberStyles.Integer, CultureInfo.InvariantCulture) with
                | true, tag -> Observable.tryOfWire tag
                | false, _ -> None

    type SampleCut with

        /// The cut's disk tag, 1 to 4 in case order; a tilted cut's angle is stored beside it (spec 0050 S9.2).
        member this.value : int =
            match this with
            | ZCut -> 1
            | XCut -> 2
            | YCut -> 3
            | TiltedCut _ -> 4

        /// A tilted cut's angle; a principal cut has none, so zero (spec 0050 S9.2).
        member this.tilt : Angle =
            match this with
            | TiltedCut tilt -> tilt
            | ZCut
            | XCut
            | YCut -> Angle.zero

        /// The cut a disk tag and its stored tilt name: a principal cut only with a zero tilt; anything else is
        /// `None`, the caller's typed error (spec 0050 S9.2).
        static member tryOfWire (tag : int) (tilt : Angle) : SampleCut option =
            match tag with
            | 1 when tilt = Angle.zero -> Some ZCut
            | 2 when tilt = Angle.zero -> Some XCut
            | 3 when tilt = Angle.zero -> Some YCut
            | 4 -> Some (TiltedCut tilt)
            | _ -> None

    /// The import profiles that are code, over the existing parsers; they are named, never stored (spec 0050 S9.2;
    /// 001 §K).
    type BuiltInImportProfile =
        | IntensitySeriesProfile
        | EllipsometricSeriesProfile
        | PhoneRoiCsvProfile
        | MuellerObservationProfile

        member this.name : ImportProfileName =
            match this with
            | IntensitySeriesProfile -> ImportProfileName.intensitySeries
            | EllipsometricSeriesProfile -> ImportProfileName.ellipsometricSeries
            | PhoneRoiCsvProfile -> ImportProfileName.phoneRoiCsv
            | MuellerObservationProfile -> ImportProfileName.muellerObservations

        /// The four built-ins in `ImportProfileName.builtIns` order (spec 0050 S9.2).
        static member all : BuiltInImportProfile list =
            [ IntensitySeriesProfile; EllipsometricSeriesProfile; PhoneRoiCsvProfile; MuellerObservationProfile ]

        /// The built-in of that name, if any (spec 0050 S9.2).
        static member tryOfName (name : ImportProfileName) : BuiltInImportProfile option =
            BuiltInImportProfile.all |> List.tryFind (fun profile -> profile.name = name)

    /// The profile an experiment's profile name resolves to: a built-in, or one the kit file holds (spec 0050 S9.2).
    type ChosenImportProfile =
        | BuiltInProfileChoice of BuiltInImportProfile
        | KitProfileChoice of ImportProfile

        member this.name : ImportProfileName =
            match this with
            | BuiltInProfileChoice profile -> profile.name
            | KitProfileChoice profile -> profile.name

        /// A built-in first, then the first of `profiles` of that name; `None` when nothing has it (spec 0050 S9.2).
        static member tryOfName (profiles : ImportProfile list) (name : ImportProfileName) : ChosenImportProfile option =
            match BuiltInImportProfile.tryOfName name with
            | Some builtIn -> Some (BuiltInProfileChoice builtIn)
            | None -> profiles |> List.tryFind (fun profile -> profile.name = name) |> Option.map KitProfileChoice

    /// A signal's standard deviation, as a lab file records it beside the signal (spec 0050 S9.2).
    type MeasuredSigma =
        | MeasuredSigma of float

        member this.value : float = let (MeasuredSigma sigma) = this in sigma

    /// When a row was captured, as a lab file records it (spec 0050 S9.2).
    type RowTimestamp =
        | RowTimestamp of DateTimeOffset

        member this.value : DateTimeOffset = let (RowTimestamp instant) = this in instant

    /// One scan value as its file wrote it, in its column's unit (spec 0050 S9.2).
    type ScanCell =
        {
            column : ScanColumn
            magnitude : Units.UnitMagnitude
        }

        /// The value in its quantity's display unit, `VariableParameter.unitLabel`'s: degrees for an angle, nanometres
        /// for a spectral column (spec 0050 S9.2).
        member this.displayValue : Propagation.SweptValue =
            let display : float =
                match this.column.valueUnit.spectralUnitOpt with
                | Some Units.Nanometer -> this.magnitude.value
                | Some spectralUnit -> (Units.fromMeters Units.Nanometer (Units.toMeters spectralUnit this.magnitude)).value
                | None when this.column.valueUnit = RadianColumnUnit -> Angle.radiansToDegrees this.magnitude.value
                | None -> this.magnitude.value

            Propagation.SweptValue display

    /// What one mapped row measured: a detector signal and its σ, or a Mueller matrix (spec 0050 S9.2; 001 §K).
    type MappedReading =
        | SignalReading of signal : MeasuredIntensity * sigmaOpt : MeasuredSigma option
        | MuellerReading of MuellerMatrix

    /// One row of a kit profile's mapping, each role read from its column (spec 0050 S9.2).
    type MappedRow =
        {
            keyOpt : DataRowKey option
            scan : ScanCell list
            reading : MappedReading
            timestampOpt : RowTimestamp option
            captureIndexOpt : CaptureIndex option
            observableOpt : Observable option
        }

    /// One row of the Mueller-matrix observation file; its key names the sample it was measured on (spec 0050 S9.2).
    type KeyedObservation =
        {
            key : DataRowKey
            observation : MuellerObservation
        }

    /// The points a profile maps a file to: an X,Y built-in's series, Phone ROI capture rows, keyed observations, or a
    /// kit profile's mapped rows (spec 0050 S9.2; 001 §K).
    type MappedPoints =
        | SeriesPoints of MeasuredSeries
        | CapturePoints of MuellerRawRow list
        | ObservationPoints of KeyedObservation list
        | ColumnPoints of MappedRow list

        member this.pointCount : MeasuredPointCount =
            match this with
            | SeriesPoints series -> series.pointCount
            | CapturePoints rows -> MeasuredPointCount (List.length rows)
            | ObservationPoints observations -> MeasuredPointCount (List.length observations)
            | ColumnPoints rows -> MeasuredPointCount (List.length rows)

    /// One quantity a scan varies, one slot's variable, with every range an axis gives it (spec 0050 S9.2).
    type ScanQuantity =
        {
            elementId : ElementId
            label : ElementLabel
            variable : VariableParameter
            ranges : VariableRange list
        }

        /// Whether some range admits `x`, in the quantity's display unit, as `VariableRange.admits` reads it (spec 0050 S9.2).
        member this.admits (x : float) : bool = this.ranges |> List.exists (fun range -> range.admits x)

    type ExperimentScan with

        /// The distinct slot variables of `axes` in order of first appearance, each labelled by its first axis and ranged by
        /// its axes in order, so A1's and A2's azimuth on one plate is one quantity (spec 0050 S9.2; 001 §Q AT-2).
        member this.quantities : ScanQuantity list =
            let axes = this.axes

            axes
            |> List.distinctBy (fun (axis : ScanAxis) -> axis.elementId, axis.variable)
            |> List.map (fun (first : ScanAxis) ->
                let quantity : ScanQuantity =
                    {
                        elementId = first.elementId
                        label = first.label
                        variable = first.variable
                        ranges =
                            axes
                            |> List.filter (fun (axis : ScanAxis) -> axis.elementId = first.elementId && axis.variable = first.variable)
                            |> List.map (fun (axis : ScanAxis) -> axis.range)
                    }

                quantity)

    type MuellerExperimentLabel with

        /// A Phone ROI row's experiment label as the key it is selected and split by (spec 0050 S9.3).
        member this.rowKey : DataRowKey = DataRowKey this.value

    type MappedPoints with

        /// The one validation of mapped points against the experiment's detector and scan: the points unchanged, or the
        /// first mismatch; observations carry their own configurations and pass (spec 0050 S9.2; 001 §K).
        member this.validatedFor (detector : DetectorKind) (scan : ExperimentScan) : Result<MappedPoints, ExperimentDataError> =
            let quantities = scan.quantities

            match this with
            | SeriesPoints series ->
                // Spec 0050 S9.1: an X,Y file holds one variable; a dark line is ranged as R1's default.
                let validated =
                    match scan.blocks with
                    | [ { axes = [ axis ] } ] -> validateAgainstExperiment (Some axis.variable) axis.range detector series
                    | [ { axes = [] } ] -> validateAgainstExperiment None (VariableRange.forVariable VaryR1) detector series
                    | blocks ->
                        Error (DataRangeMismatchErr $"an X,Y data file holds one variable, and the scan varies {scan.axes.Length} in {blocks.Length} block(s)")

                validated |> Result.map SeriesPoints
            | CapturePoints rows ->
                match detector, quantities with
                | Ellipsometer, _ ->
                    Error (DataUnitsMismatchErr "the detector is an ellipsometer (records Ψ/Δ), but the data file is a Phone ROI CSV of intensities")
                | Intensity, [] -> Ok this
                | Intensity, [ quantity ] ->
                    match rows |> List.tryFind (fun row -> not (quantity.admits row.description.degrees)) with
                    | Some row ->
                        Error (DataRangeMismatchErr $"a description {row.description.degrees}° lies outside every range of {quantity.label.value} {quantity.variable.label}")
                    | None -> Ok this
                | Intensity, _ ->
                    Error (DataRangeMismatchErr $"a Phone ROI CSV holds one variable (its description), and the scan varies {quantities.Length}")
            | ObservationPoints _ -> Ok this
            | ColumnPoints rows ->
                let problemOpt =
                    rows
                    |> List.tryPick (fun (row : MappedRow) ->
                        match detector, row.reading with
                        | Ellipsometer, SignalReading _ ->
                            Some (DataUnitsMismatchErr "the detector is an ellipsometer (records Ψ/Δ), but the mapping reads a signal")
                        | _ when row.scan.Length <> quantities.Length ->
                            Some (DataUnitsMismatchErr $"the import profile maps {row.scan.Length} scan column(s), and the scan varies {quantities.Length}")
                        | _ ->
                            // The i-th scan cell is the i-th quantity's (spec 0050 S9.2; 001 §I: a dial records raw readings).
                            List.zip row.scan quantities
                            |> List.tryPick (fun (cell : ScanCell, quantity : ScanQuantity) ->
                                let column = cell.column
                                let named = $"{quantity.label.value} {quantity.variable.label}"

                                match column.valueUnit.spectralUnitOpt, quantity.variable, column.reading with
                                | None, VaryWaveLength, _ ->
                                    Some (DataUnitsMismatchErr $"column {column.column.value} holds an angle ({column.valueUnit.text}), and {named} is a wavelength")
                                | Some _, (VaryR1 | VaryR2 | VaryDialReading), _ ->
                                    Some (DataUnitsMismatchErr $"column {column.column.value} holds a spectral value ({column.valueUnit.text}), and {named} is an angle")
                                | _, (VaryWaveLength | VaryR1 | VaryR2), RawDialReading ->
                                    Some (DataUnitsMismatchErr $"column {column.column.value} holds a raw dial reading, and {named} is a physical value")
                                | _, VaryDialReading, PhysicalScanValue ->
                                    Some (DataUnitsMismatchErr $"column {column.column.value} holds a physical value, and {named} is a raw dial reading")
                                | _ when quantity.admits cell.displayValue.value -> None
                                | _ ->
                                    Some (DataRangeMismatchErr $"a scan value {cell.displayValue.value} {quantity.variable.unitLabel} lies outside every range of {named}")))

                match problemOpt with
                | Some problem -> Error problem
                | None -> Ok this

        /// Each point's key in point order: none for a series point, a capture's label, an observation's key and a mapped
        /// row's key column. The one reading of a point's key, which selection and splitting share (spec 0050 S9.3).
        member this.pointKeys : DataRowKey option list =
            match this with
            | SeriesPoints (IntensityData series) -> series.points |> List.map (fun _ -> None)
            | SeriesPoints (EllipsometricData series) -> series.points |> List.map (fun _ -> None)
            | CapturePoints captures -> captures |> List.map (fun capture -> Some capture.experiment.rowKey)
            | ObservationPoints observations -> observations |> List.map (fun observation -> Some observation.key)
            | ColumnPoints rows -> rows |> List.map (fun row -> row.keyOpt)

        /// The experiment's rows of the points: every point, or those whose `pointKeys` entry is the key; a series or rows
        /// without a key cannot be selected by one, and no row of the key is an empty file (spec 0050 S9.2, S9.3; 001 §K).
        member this.selectedBy (rows : DataRowSelection) : Result<MappedPoints, ExperimentDataError> =
            match rows with
            | AllDataRows -> Ok this
            | KeyedDataRows key ->
                let keys = this.pointKeys

                let kept (points : 'Point list) (mapped : 'Point list -> MappedPoints) : Result<MappedPoints, ExperimentDataError> =
                    match List.zip keys points |> List.filter (fun (keyOpt, _) -> keyOpt = Some key) |> List.map snd with
                    | [] -> Error (EmptyDataFileErr $"no row of the data file holds the key '{key.value}'")
                    | selected -> Ok (mapped selected)

                match this, List.choose id keys with
                | SeriesPoints _, _ -> Error (MalformedDataFileErr $"an X,Y data file has no key column to select '{key.value}' by")
                | ColumnPoints _, [] -> Error (MalformedDataFileErr $"the import profile maps no key column to select '{key.value}' by")
                | CapturePoints captures, _ -> kept captures CapturePoints
                | ObservationPoints observations, _ -> kept observations ObservationPoints
                | ColumnPoints mapped, _ -> kept mapped ColumnPoints

        /// The points in order without those an exclusion names, the case and a series' kind kept; an exclusion naming
        /// no point changes nothing. The one rule from an attachment's exclusions to the points read (spec 0050 S9.3).
        member this.keptBy (exclusions : PointExclusion list) : MappedPoints =
            let excluded = exclusions |> List.map (fun exclusion -> exclusion.point) |> Set.ofList

            let kept (points : 'Point list) : 'Point list =
                points
                |> List.mapi (fun index point -> MappedPointNumber (index + 1), point)
                |> List.filter (fun (number, _) -> not (Set.contains number excluded))
                |> List.map snd

            match this with
            | SeriesPoints (IntensityData series) -> SeriesPoints (IntensityData { series with points = kept series.points })
            | SeriesPoints (EllipsometricData series) -> SeriesPoints (EllipsometricData { series with points = kept series.points })
            | CapturePoints captures -> CapturePoints (kept captures)
            | ObservationPoints observations -> ObservationPoints (kept observations)
            | ColumnPoints rows -> ColumnPoints (kept rows)

    /// A Mueller element m<i><j> at 4i + j, 0 to 15 (spec 0050 S9.2).
    type MuellerElementIndex =
        | MuellerElementIndex of int

        member this.value : int = let (MuellerElementIndex index) = this in index

        static member tryCreate (index : int) : MuellerElementIndex option =
            if index < 0 || index > 15 then None else Some (MuellerElementIndex index)

        /// The sixteen elements in index order, m00 to m33 (spec 0050 S9.2).
        static member all : MuellerElementIndex list = [ for index in 0 .. 15 -> MuellerElementIndex index ]

        member this.text : string =
            let (MuellerElementIndex index) = this
            $"m{index / 4}{index % 4}"

    /// The role the wizard gives one column of a lab file (spec 0050 S9.2).
    type ColumnRoleChoice =
        | UnmappedColumnRole
        | ScanColumnRole of valueUnit : ScanColumnUnit * reading : ScanReading
        | SignalColumnRole
        | SigmaColumnRole
        | TimestampColumnRole
        | CaptureIndexColumnRole
        | ObservableColumnRole
        | KeyColumnRole
        | ThroughputColumnRole
        | MuellerElementRole of MuellerElementIndex

        member this.text : string =
            match this with
            | UnmappedColumnRole -> "—"
            | ScanColumnRole (valueUnit, reading) -> $"scan variable ({valueUnit.text}, {reading.text})"
            | SignalColumnRole -> "signal"
            | SigmaColumnRole -> "σ"
            | TimestampColumnRole -> "timestamp"
            | CaptureIndexColumnRole -> "capture index"
            | ObservableColumnRole -> "observable"
            | KeyColumnRole -> "experiment key"
            | ThroughputColumnRole -> "throughput"
            | MuellerElementRole index -> index.text

    /// One column and the role the wizard gives it (spec 0050 S9.2).
    type ColumnChoice =
        {
            column : DataColumnNumber
            role : ColumnRoleChoice
        }

    type ColumnRoles with

        /// The one rule from the wizard's column choices to a profile's roles, read in column order; a role held by
        /// one column is refused on a second, and an unmapped column is read by nothing (spec 0050 S9.2).
        static member tryOfChoices (choices : ColumnChoice list) : Result<ColumnRoles, ImportProfileError> =
            let ordered = choices |> List.sortBy (fun choice -> choice.column)

            let columnsOf (role : ColumnRoleChoice) : DataColumnNumber list =
                ordered |> List.filter (fun choice -> choice.role = role) |> List.map (fun choice -> choice.column)

            let columnOpt (role : ColumnRoleChoice) : DataColumnNumber option = columnsOf role |> List.tryHead

            let repeatedOpt =
                [ SigmaColumnRole; TimestampColumnRole; CaptureIndexColumnRole; ObservableColumnRole; KeyColumnRole; ThroughputColumnRole ]
                |> List.tryPick (fun role ->
                    match columnsOf role with
                    | first :: second :: _ -> Some (RepeatedRoleErr $"{role.text}: columns {first.value} and {second.value}")
                    | []
                    | [ _ ] -> None)

            let readsElements =
                ordered
                |> List.exists (fun choice ->
                    match choice.role with
                    | MuellerElementRole _
                    | ThroughputColumnRole -> true
                    | _ -> false)

            let readingResult : Result<ReadingColumns, ImportProfileError> =
                if readsElements then
                    match columnsOf SignalColumnRole @ columnsOf SigmaColumnRole with
                    | column :: _ -> Error (MixedReadingColumnsErr $"column {column.value} reads a signal beside Mueller elements")
                    | [] ->
                        let elementColumns = MuellerElementIndex.all |> List.map (fun index -> columnsOf (MuellerElementRole index))

                        let countReasonOpt =
                            List.zip MuellerElementIndex.all elementColumns
                            |> List.tryPick (fun (index, columns) ->
                                match columns with
                                | [ _ ] -> None
                                | [] -> Some $"{index.text} has no column"
                                | first :: second :: _ -> Some $"{index.text} is given to columns {first.value} and {second.value}")

                        match countReasonOpt with
                        | Some reason -> Error (MuellerElementCountErr reason)
                        | None -> Ok (MuellerElementColumns (List.concat elementColumns, columnOpt ThroughputColumnRole))
                else
                    match columnsOf SignalColumnRole with
                    | [ signal ] -> Ok (SignalColumns (signal, columnOpt SigmaColumnRole))
                    | [] -> Error (SignalColumnCountErr "no column is the signal")
                    | signals ->
                        let numbers = signals |> List.map (fun column -> $"{column.value}") |> String.concat ", "
                        Error (SignalColumnCountErr $"columns {numbers} are each the signal")

            match repeatedOpt, readingResult with
            | Some error, _
            | None, Error error -> Error error
            | None, Ok reading ->
                let roles : ColumnRoles =
                    {
                        scan =
                            ordered
                            |> List.choose (fun choice ->
                                match choice.role with
                                | ScanColumnRole (valueUnit, reading) ->
                                    let column : ScanColumn = { column = choice.column; valueUnit = valueUnit; reading = reading }
                                    Some column
                                | _ -> None)
                        reading = reading
                        timestampOpt = columnOpt TimestampColumnRole
                        captureIndexOpt = columnOpt CaptureIndexColumnRole
                        observableOpt = columnOpt ObservableColumnRole
                        keyOpt = columnOpt KeyColumnRole
                    }

                Ok roles

    /// What a lab file's bytes and first lines suggest for its layout, which the wizard shows and the user changes
    /// (spec 0050 S9.2; 001 §K).
    type LabFileSniff =
        {
            bom : ByteOrderMark
            delimiter : DataDelimiter
            header : HeaderRowChoice
        }

    /// One cell of a lab file's preview, a column name or a row's cell as shown (spec 0050 S9.2).
    type PreviewCell =
        | PreviewCell of string

        member this.value : string = let (PreviewCell text) = this in text

    /// One row of a lab file's preview, its cells in column order (spec 0050 S9.2).
    type PreviewRow =
        | PreviewRow of PreviewCell list

        member this.cells : PreviewCell list = let (PreviewRow cells) = this in cells

    /// A lab file's column names and first rows as a layout reads them (spec 0050 S9.2; 001 §O).
    type LabFilePreview =
        {
            columnNames : PreviewCell list
            rows : PreviewRow list
        }

    type LabFileSource with

        /// The layout suggested by the file's mark and its first ten non-empty lines: the first delimiter that splits
        /// them all into one count of two or more cells, else a comma, and a header when the first line reads no number,
        /// or reads text over the second line's number. It touches no disk (spec 0050 S9.2).
        member this.sniffedLayout : LabFileSniff =
            let lines =
                this.content.kitFileText.value.Split('\n')
                |> Array.map (fun line -> if line.EndsWith('\r') then line.Substring(0, line.Length - 1) else line)
                |> Array.filter (fun line -> not (String.IsNullOrWhiteSpace line))
                |> Array.truncate 10
                |> Array.toList

            let cellsOf (delimiter : DataDelimiter) (line : string) : string list = line.Split(delimiter.character) |> Array.toList

            let splitsEvenly (delimiter : DataDelimiter) : bool =
                match lines |> List.map (fun line -> List.length (cellsOf delimiter line)) |> List.distinct with
                | [ count ] -> count >= 2
                | _ -> false

            let delimiter = DataDelimiter.all |> List.tryFind splitsEvenly |> Option.defaultValue CommaDelimiter

            let readsNumber (cell : string) : bool =
                match Double.TryParse(cell, NumberStyles.Float, CultureInfo.InvariantCulture) with
                | true, _ -> true
                | false, _ -> false

            let header =
                match lines |> List.map (cellsOf delimiter) with
                | first :: rest ->
                    let below = List.tryHead rest |> Option.defaultValue []

                    let textOverNumber =
                        first
                        |> List.mapi (fun index cell -> not (readsNumber cell) && (List.tryItem index below |> Option.exists readsNumber))
                        |> List.contains true

                    if textOverNumber || not (List.exists readsNumber first) then HeaderOnFirstRow else NoHeaderRow
                | [] -> HeaderOnFirstRow

            let sniff : LabFileSniff =
                {
                    bom = this.content.byteOrderMark
                    delimiter = delimiter
                    header = header
                }

            sniff
