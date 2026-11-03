namespace OpticalConstructor.Storage

open System
open System.Globalization
open System.IO
open Softellect.Sys.Primitives
open OpticalConstructor.Domain.ExperimentExport
open OpticalConstructor.Storage.Export

/// Spec 0046 — the file-backed `ExperimentExportProxy`: each exported run becomes one CSV file in a folder,
/// named by the export time. Series that share their x values are written WIDE (an x column, then one column
/// per series); otherwise LONG (series, x, y). Numbers use the round-trip format under the invariant culture
/// (`Export.exportTableCsv`). Platform IO throws are caught here, at the boundary, and returned as `ExportFailedErr`.
module ExperimentExportFile =

    /// When an export ran; it names the export's file.
    type ExportTime =
        | ExportTime of DateTime

        member this.value = let (ExportTime t) = this in t

        /// The time as the file-name stamp `yyyyMMdd-HHmmss-fff`.
        member this.fileStamp : string = this.value.ToString("yyyyMMdd-HHmmss-fff", CultureInfo.InvariantCulture)

    type ExportedRun with

        /// The table this run is written as.
        member this.exportTable : ExportTable =
            let xsOf (s : ExportedSeries) : float list = s.points |> List.map (fun p -> p.x)
            match this.series with
            | first :: rest when rest |> List.forall (fun s -> xsOf s = xsOf first) ->
                let rows =
                    first.points
                    |> List.mapi (fun i p -> Array.ofList (p.x :: (this.series |> List.map (fun s -> (List.item i s.points).y))))
                    |> Array.ofList
                { headers = this.xLabel :: (this.series |> List.map (fun s -> s.name)); rows = rows }
            | _ ->
                let names = this.series |> List.map (fun s -> s.name.value)
                let rows =
                    this.series
                    |> List.mapi (fun k s -> s.points |> List.map (fun p -> [| float k; p.x; p.y |]))
                    |> List.concat
                    |> Array.ofList
                // The series index column refers to the names, listed in a trailing comment-free header suffix.
                let seriesHeader = ExportText ("series (" + String.Join("; ", names |> List.mapi (fun k n -> $"%d{k} = %s{n}")) + ")")
                { headers = [ seriesHeader; this.xLabel; this.yLabel ]; rows = rows }

    /// A proxy writing each run to `<folder>/experiment-<time>.csv` (the folder is created when missing).
    /// `now` is the injected clock (the composition root passes the system clock; tests a fixed one).
    let createCsvFolder (folder : FolderName) (now : unit -> ExportTime) : ExperimentExportProxy =
        {
            exportRun =
                fun run ->
                    match run.series with
                    | [] -> Error (NothingToExportErr "the run has no data to export")
                    | _ ->
                        try
                            Directory.CreateDirectory folder.value |> ignore
                            let path = (FileName $"experiment-%s{(now ()).fileStamp}.csv").combine folder
                            match exportTableCsv path run.exportTable with
                            | Ok () -> Ok ()
                            | Error e -> Error (ExportFailedErr $"writing %s{path.value} failed: %A{e}")
                        with ex -> Error (ExportFailedErr $"exporting to %s{folder.value} failed: %s{ex.Message}")
        }
