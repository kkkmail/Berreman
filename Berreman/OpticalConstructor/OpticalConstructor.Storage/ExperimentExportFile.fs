namespace OpticalConstructor.Storage

open System
open System.Globalization
open System.IO
open OpticalConstructor.Domain.ExperimentExport

/// Spec 0046 — the file-backed `ExperimentExportProxy`: each exported run becomes one CSV file in a folder,
/// named by the export time. Series that share their x values are written WIDE (an x column, then one column
/// per series); otherwise LONG (series, x, y). Numbers use the round-trip format under the invariant culture
/// (`Export.exportCsv`). Platform IO throws are caught here, at the boundary, and returned as `ExportFailed`.
module ExperimentExportFile =

    let private fileStamp (t : DateTime) : string = t.ToString("yyyyMMdd-HHmmss-fff", CultureInfo.InvariantCulture)

    /// The (headers, rows) a run is written as.
    let tableOf (run : ExportedRun) : string list * float[][] =
        let xsOf (s : ExportedSeries) : float list = s.points |> List.map fst
        match run.series with
        | first :: rest when rest |> List.forall (fun s -> xsOf s = xsOf first) ->
            let headers = run.xLabel :: (run.series |> List.map (fun s -> s.name))
            let rows =
                first.points
                |> List.mapi (fun i (x, _) -> Array.ofList (x :: (run.series |> List.map (fun s -> snd (List.item i s.points)))))
                |> Array.ofList
            headers, rows
        | _ ->
            let names = run.series |> List.map (fun s -> s.name)
            let headers = [ "series"; run.xLabel; run.yLabel ]
            let rows =
                run.series
                |> List.mapi (fun k s -> s.points |> List.map (fun (x, y) -> [| float k; x; y |]))
                |> List.concat
                |> Array.ofList
            // The series index column refers to the names, listed in a trailing comment-free header suffix.
            ("series (" + String.Join("; ", names |> List.mapi (fun k n -> $"%d{k} = %s{n}")) + ")") :: List.tail headers, rows

    /// A proxy writing each run to `<folder>/experiment-<time>.csv` (the folder is created when missing).
    /// `now` is the injected clock (the composition root passes the system clock; tests a fixed one).
    let createCsvFolder (folder : string) (now : unit -> DateTime) : ExperimentExportProxy =
        {
            exportRun =
                fun run ->
                    match run.series with
                    | [] -> Error (NothingToExport "the run has no data to export")
                    | _ ->
                        try
                            Directory.CreateDirectory folder |> ignore
                            let path = Path.Combine(folder, $"experiment-%s{fileStamp (now ())}.csv")
                            match Export.exportCsv path (tableOf run) with
                            | Ok () -> Ok ()
                            | Error e -> Error (ExportFailed $"writing %s{path} failed: %A{e}")
                        with ex -> Error (ExportFailed $"exporting to %s{folder} failed: %s{ex.Message}")
        }
