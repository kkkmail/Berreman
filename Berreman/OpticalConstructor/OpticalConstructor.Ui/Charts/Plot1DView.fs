/// §H.3/§H.4/§H.9/§H.11 — the ScottPlot 1D interactive view: R/T/Ψ/Δ-vs-(λ|angle)
/// traces, overlay/comparison of several systems, the in-layer field-depth trace, and
/// the n,k/ρ/ε dispersion trace. Each visible trace from `ChartSettings` is drawn from
/// a `SeriesData` 1D series via `Plot.Add.Scatter`, legend-named with the engine's
/// `OpticalFunction.info.fullName` (`Charting.fs:31`). Zoom/pan use ScottPlot's
/// built-in interaction at the control level (no custom pan/zoom math here). The view
/// is a pure function of `ChartSettings` + the series; it holds NO mutable chart state
/// of its own. The engine's terminal `Chart.show` is never called.
module OpticalConstructor.Ui.Charts.Plot1DView

open Berreman.Constants
open Berreman.FieldFunctions
open Analytics.Variables
open OpticalConstructor.Domain
open OpticalConstructor.Ui.Charts.ChartSettings
open OpticalConstructor.Ui.Charts.SeriesData

let private styleFor (settings : ChartSettings) (name : string) : TraceSettings option =
    settings.traces |> List.tryFind (fun t -> t.name = name)

/// Core renderer: draw each series onto a fresh `ScottPlot.Plot` (omitting any whose
/// matching `ChartSettings` trace is `visible = false`), label/style it from the
/// matching trace, set the X axis label and — unless `ChartSettings` overrides the
/// range — the sweep's plotted range, then push all axis/legend/grid/visibility
/// settings through `applyToScottPlot` (the SOLE renderer-settings seam).
let render (settings : ChartSettings) (xLabel : string) (xRange : (float * float) option) (series : Series1D list) : ScottPlot.Plot =
    let plot = new ScottPlot.Plot()

    for s in series do
        let style = styleFor settings s.name
        let hidden = match style with | Some t -> not t.visible | None -> false
        if not hidden then
            let xs = s.points |> Array.map fst
            let ys = s.points |> Array.map snd
            let scatter = plot.Add.Scatter(xs, ys)
            scatter.LegendText <- s.name
            match style with
            | Some t ->
                scatter.Color <- scottPlotColor t.color
                scatter.LinePattern <- scottPlotLinePattern t.lineStyle
            | None -> ()

    plot.XLabel(xLabel)

    // Sweep range is the default; a `ChartSettings` override (applied next) wins.
    match xRange, settings.xMin, settings.xMax with
    | Some (lo, hi), None, None -> plot.Axes.SetLimitsX(lo, hi)
    | _ -> ()

    applyToScottPlot settings plot
    plot

/// §H.3 — the [Core] interactive 1D plot: one trace per requested `OpticalFunction`
/// over a single system, X axis label/range from the sweep variable (`Charting.fs:32`).
let renderSweep (settings : ChartSettings) (f : FixedInfo) (fns : OpticalFunction list) (x : RangedVariable) : ScottPlot.Plot =
    render settings x.name (Some (x.plotMinValue, x.plotMaxValue)) (series1DMany f fns x)

/// §H.4 — the [Core] overlay/comparison plot: the same `OpticalFunction` from several
/// systems, reusing `plotComparison`'s per-system layout (`Charting.fs:38-49`). The
/// system set is supplied by the caller (the Part J §J.5 multi-system workspace);
/// this view introduces no system-collection store and opens no browser tab.
let renderComparison (settings : ChartSettings) (systems : FixedInfo list) (fn : OpticalFunction) (x : RangedVariable) : ScottPlot.Plot =
    render settings x.name (Some (x.plotMinValue, x.plotMaxValue)) (seriesComparison systems fn x)

/// §H.9 — the [Standard] in-layer field-depth trace, depth (canonical meters)
/// converted to the layer's display unit at the boundary (§A.10). The per-depth
/// samples come from Part F §F.6 `fieldDepthProfile`; this view renders only.
let renderFieldDepth (settings : ChartSettings) (depthUnit : Units.UnitOfMeasure) (depthLabel : string) (profile : (float<meter> * float) list) : ScottPlot.Plot =
    render settings depthLabel None [ fieldDepthSeries depthUnit profile ]

/// §H.11 — the [Standard] n,k/ρ/ε dispersion trace(s), series prebuilt from the
/// engine dispersion helpers via `SeriesData` (no `Chart.show`, no re-derived
/// formulas). `xRange` is the wavelength sweep's plotted range.
let renderDispersion (settings : ChartSettings) (xRange : (float * float) option) (series : Series1D list) : ScottPlot.Plot =
    render settings "w (nm)" xRange series
