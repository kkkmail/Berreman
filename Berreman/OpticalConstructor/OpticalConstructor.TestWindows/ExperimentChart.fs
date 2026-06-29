namespace OpticalConstructor.TestWindows

/// Spec 0027 (026) — the renderer-neutral chart data the Experiments bay produces and BOTH the inline
/// scene chart and the pop-out chart window consume. Domain-neutral (no Berreman types) so it crosses the
/// Controls/Ui seam freely; ONE series for an intensity sweep, TWO (Ψ and Δ) for an ellipsometer sweep.
/// The description is prose built from the bound elements + what is swept.
module ExperimentChart =

    /// One named curve: a display name (legend) and its (x, y) points in axis-display units (degrees for an
    /// angle sweep, nm for a wavelength sweep; intensity / Ψ / Δ on y).
    type ChartSeries =
        {
            name : string
            points : (float * float) list
        }

    /// A whole chart: its series, axis labels, title, and the human description shown beneath it.
    type ExperimentChart =
        {
            series : ChartSeries list
            xLabel : string
            yLabel : string
            title : string
            description : string
        }

    /// The empty chart (no experiment built / no run to show).
    let empty : ExperimentChart =
        { series = []; xLabel = ""; yLabel = ""; title = ""; description = "" }

    /// The x-values to export / draw: the union of every series' x-coordinates, in ascending order. (The
    /// sweep builders share an x-grid per chart, so this is just the first series' xs in practice; taking
    /// the union keeps `toCsv` total when series differ in length.)
    let private sortedXs (chart : ExperimentChart) : float list =
        chart.series
        |> List.collect (fun s -> s.points |> List.map fst)
        |> List.distinct
        |> List.sort

    /// A pure CSV rendering of the chart: a header row `x,<series names…>` then one row per x with each
    /// series' y at that x (blank when a series has no point there). Used by the chart window's CSV export.
    let toCsv (chart : ExperimentChart) : string =
        let header = "x" :: (chart.series |> List.map (fun s -> s.name)) |> String.concat ","
        let lookups = chart.series |> List.map (fun s -> Map.ofList s.points)
        let rows =
            sortedXs chart
            |> List.map (fun x ->
                let cells =
                    string x
                    :: (lookups
                        |> List.map (fun m ->
                            match Map.tryFind x m with
                            | Some y -> string y
                            | None -> ""))
                cells |> String.concat ",")
        header :: rows |> String.concat "\n"
