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

/// Spec 0027 (028) — the pure, renderer-neutral font-size state for the pop-out chart window. The old
/// Font +/- affected only one text kind; the redesign lets the user SELECT which text to resize — the
/// header (title), the axis labels, the tick labels, or the legend — each with its OWN size, and Font +/-
/// applies to the SELECTED target, whose size is shown numerically. Kept pure (sizes are plain `float`, the
/// window converts to ScottPlot's `float32` at the boundary) so the selection / clamp logic is unit-testable
/// without opening a native window.
module ChartFont =

    /// Which text of the chart the Font +/- currently resizes.
    type ChartFontTarget =
        | Title
        | AxisLabels
        | TickLabels
        | Legend

        /// A human-readable label (the selector item text and the size readout prefix).
        member this.label : string =
            match this with
            | Title -> "Header"
            | AxisLabels -> "Axis labels"
            | TickLabels -> "Tick labels"
            | Legend -> "Legend"

    /// The four selectable targets, in display order.
    let allTargets : ChartFontTarget list = [ Title; AxisLabels; TickLabels; Legend ]

    /// The per-target font sizes (points) plus which target the Font +/- currently acts on.
    type ChartFontState =
        {
            title : float
            axisLabels : float
            tickLabels : float
            legend : float
            selected : ChartFontTarget
        }

    /// The allowed font-size band (points).
    let minSize : float = 6.0
    let maxSize : float = 40.0

    /// The default sizes (a readable header, slightly smaller axis labels / legend, smaller tick labels).
    let defaultState : ChartFontState =
        { title = 16.0; axisLabels = 13.0; tickLabels = 11.0; legend = 12.0; selected = Title }

    /// The current size of a given target.
    let sizeOf (target : ChartFontTarget) (s : ChartFontState) : float =
        match target with
        | Title -> s.title
        | AxisLabels -> s.axisLabels
        | TickLabels -> s.tickLabels
        | Legend -> s.legend

    /// The size of the currently-selected target (what the readout shows and Font +/- changes).
    let selectedSize (s : ChartFontState) : float = sizeOf s.selected s

    /// Select a different target (leaves every size unchanged; the readout then shows this target's size).
    let withSelected (target : ChartFontTarget) (s : ChartFontState) : ChartFontState =
        { s with selected = target }

    let private clamp (v : float) : float = max minSize (min maxSize v)

    /// Set a target's size (clamped to the band).
    let withSize (target : ChartFontTarget) (v : float) (s : ChartFontState) : ChartFontState =
        let v = clamp v
        match target with
        | Title -> { s with title = v }
        | AxisLabels -> { s with axisLabels = v }
        | TickLabels -> { s with tickLabels = v }
        | Legend -> { s with legend = v }

    /// Change the SELECTED target's size by `delta` points (clamped). Font + is +1, Font − is −1.
    let bumpSelected (delta : float) (s : ChartFontState) : ChartFontState =
        withSize s.selected (selectedSize s + delta) s

    /// The size readout text (e.g. "Tick labels: 11 pt").
    let readout (s : ChartFontState) : string =
        sprintf "%s: %g pt" s.selected.label (selectedSize s)
