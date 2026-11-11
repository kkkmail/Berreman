namespace OpticalConstructor.Domain

open System
open System.Globalization
open Berreman.Geometry
open Berreman.Fields
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.MeasuredData
open OpticalConstructor.Domain.MuellerReconstruction
open OpticalConstructor.Domain.MuellerInverse

/// The built-in import profiles, the profile an experiment names, and the points a profile maps a lab file to
/// (spec 0050 S9.2; 001 §K).
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
