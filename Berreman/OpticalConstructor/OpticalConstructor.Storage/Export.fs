namespace OpticalConstructor.Storage

open System
open System.Globalization
open System.IO
open System.Text
open ClosedXML.Excel
open Softellect.Sys.Primitives
open OpticalConstructor.Domain.ExperimentExport
open OpticalConstructor.Storage.Errors

/// Chart-data and chart-image export (§I.7 / R-3). `exportTableCsv`/`exportTableExcel`
/// SERIALIZE the already-computed sweep tuples the engine's `calculate`/`calculate3D`
/// (`Variables.fs:237,270`) produced and `plot`/`plotComparison`/`plot3D`
/// (`Charting.fs:21,38,64`) render — this module never re-runs or re-implements the
/// solver (AC-I8). Chart IMAGE export is DELEGATED to the owning chart library's own
/// writer through the UI-supplied `ChartHandle` closure; `OpticalConstructor.Storage`
/// holds no ScottPlot/Plotly/WebView2 dependency and contains no rasterizer or
/// vectorizer of its own (AC-I9). Every function returns `Result` on completion so
/// the UI can refresh the results pane and offer "Open containing folder" on `Ok`.
module Export =

    /// The image formats a chart can be saved to (§I.7). Net-new DU.
    type ChartImageFormat =
        | PngImage
        | SvgImage
        | PdfImage

    /// A chart's title: the report section heading it is exported under (§I.8).
    type ChartTitle =
        | ChartTitle of string

        member this.value = let (ChartTitle t) = this in t

    /// A renderer-neutral handle to a UI-built chart (§I.7). `ChartHandle` did not
    /// exist after slice 012 (Part H produced `Series1D`/`Surface` and `ScottPlot`/
    /// Plotly views, not a handle), so it is introduced here as the export/report
    /// boundary type. It carries the chart library's OWN writer as a closure supplied
    /// by the UI (Part J) — `ScottPlot.Plot.SavePng`/`SaveSvg` or the Plotly.NET /
    /// WebView2 image writer — so dispatch reaches the library's writer (AC-I9) while
    /// the Storage layer takes no chart-library or browser dependency and rasterizes
    /// nothing itself. `title` is the report section heading for this chart (§I.8).
    type ChartHandle =
        {
            title : ChartTitle
            saveImage : ChartImageFormat -> FileName -> Result<unit, exn>
        }

    /// Already-computed results an export writes: the column headers, then one row of numbers per sample.
    [<Softellect.Sys.NumericCarrier("the computed sweep or run results an export writes whole, one row of numbers per sample (R3: sweep and spectrum arrays)")>]
    type ExportTable =
        {
            headers : ExportText list
            rows : float[][]
        }

    let private inv = CultureInfo.InvariantCulture

    /// Write sweep results as CSV (§I.7): one header line of column names, then one
    /// comma-separated line per row of the already-computed `float[][]` (AC-I8). The
    /// solver is NOT invoked — this serializes the table handed in. Numbers use the
    /// round-trip "R" format under the invariant culture so a re-import is exact.
    let exportTableCsv (path : FileName) (table : ExportTable) : Result<unit, StorageError> =
        try
            let sb = StringBuilder()
            sb.AppendLine(table.headers |> List.map (fun (ExportText h) -> h) |> String.concat ",") |> ignore
            for row in table.rows do
                row
                |> Array.map (fun v -> v.ToString("R", inv))
                |> String.concat ","
                |> sb.AppendLine
                |> ignore
            File.WriteAllText(path.value, sb.ToString(), UTF8Encoding false)
            Ok ()
        with e -> Error (FileIoExn e)

    /// Write the same already-computed sweep results as an `.xlsx` workbook (§I.7),
    /// using the single in-tree Excel dependency (ClosedXML). One header row of
    /// column names, then one numeric row per `float[][]` row. No solver call, no
    /// Excel abstraction layer.
    let exportTableExcel (path : FileName) (table : ExportTable) : Result<unit, StorageError> =
        try
            use wb = new XLWorkbook()
            let ws = wb.AddWorksheet "Sweep"
            table.headers |> List.iteri (fun i (ExportText h) -> ws.Cell(1, i + 1).Value <- h)
            table.rows
            |> Array.iteri (fun r row ->
                row |> Array.iteri (fun c v -> ws.Cell(r + 2, c + 1).Value <- v))
            wb.SaveAs(path.value : string)
            Ok ()
        with e -> Error (FileIoExn e)

    /// Export a chart image (§I.7 / AC-I9) by DISPATCHING to the chart library's own
    /// writer carried by the `ChartHandle` — Plotly.NET through its WebView2-hosted
    /// image writer, ScottPlot through `SavePng`/`SaveSvg`. This module does not
    /// rasterize or vectorize; it only routes the call and folds a writer exception
    /// into `FileIoExn`.
    let exportChartImage (path : FileName) (format : ChartImageFormat) (handle : ChartHandle) : Result<unit, StorageError> =
        match handle.saveImage format path with
        | Ok () -> Ok ()
        | Error e -> Error (FileIoExn e)
