namespace OpticalConstructor.Storage

open System
open System.Globalization
open Berreman.Constants
open Berreman.Geometry
open Berreman.Fields
open Berreman.Media
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.MeasuredData
open OpticalConstructor.Domain.PropagationReadouts
open OpticalConstructor.Domain.MuellerInverse
open OpticalConstructor.Domain.ImportProfiles

/// The Mueller-matrix observation file: the app writes keyed observations to it and the `Mueller-matrix observations`
/// profile reads them back, element for element (spec 0050 S9.2; 001 §Q AT-2).
module LabFileImport =

    /// The observation file's columns in order, the one place its header is written (spec 0050 S9.2).
    let private observationFileColumns : string list =
        [ "sample"; "cut"; "cut_tilt_rad"; "thickness_m"; "incidence_rad"; "azimuth_rad"; "observable"; "wavelength_nm" ]
        @ (MuellerElementIndex.all |> List.map (fun index -> index.text))
        @ [ "throughput" ]

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
