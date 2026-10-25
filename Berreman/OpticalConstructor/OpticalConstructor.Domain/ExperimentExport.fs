namespace OpticalConstructor.Domain

/// Spec 0046 — exporting an experiment's computed data through a functional proxy. The Main workbench hands
/// the run it just computed (the series a chart would draw) to `ExperimentExportProxy.exportRun`; the app
/// composes a file-backed proxy (CSV, `OpticalConstructor.Storage`), a test the in-memory store below, which
/// keeps every exported run so the numbers can be checked against an independent calculation.
module ExperimentExport =

    /// One exported series: its name (e.g. "Intensity (T)", "Ψ (R)") and its (x, y) points.
    type ExportedSeries =
        {
            name : string
            points : (float * float) list
        }

    /// One exported run: what was varied (the x label), what was read (the y label), a description of the
    /// scene and the run, and the series.
    type ExportedRun =
        {
            title : string
            xLabel : string
            yLabel : string
            description : string
            series : ExportedSeries list
        }

    /// The export error channel (errors as values; each case carries a reason).
    type ExportError =
        | NothingToExport of reason : string
        | ExportFailed of reason : string

        member this.reason : string =
            match this with
            | NothingToExport reason
            | ExportFailed reason -> reason

    /// The export seam: a record of a `Result`-returning function (the functional-proxy convention).
    /// Reference-compared, so a model holding one keeps its equality.
    [<ReferenceEquality>]
    type ExperimentExportProxy =
        {
            exportRun : ExportedRun -> Result<unit, ExportError>
        }

    /// The in-memory export store: the proxy plus a read of every run exported through it, in order.
    [<ReferenceEquality>]
    type ExperimentExportStore =
        {
            proxy : ExperimentExportProxy
            exported : unit -> ExportedRun list
        }

    /// A fresh, empty in-memory export store. A run with no series is refused (`NothingToExport`).
    let createInMemory () : ExperimentExportStore =
        let store = ref ([] : ExportedRun list)
        let exportRun (run : ExportedRun) : Result<unit, ExportError> =
            match run.series with
            | [] -> Error (NothingToExport "the run has no data to export")
            | _ ->
                store.Value <- store.Value @ [ run ]
                Ok ()
        {
            proxy = { exportRun = exportRun }
            exported = fun () -> store.Value
        }
