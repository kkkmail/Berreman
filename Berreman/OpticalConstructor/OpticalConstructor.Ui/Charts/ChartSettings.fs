/// §H.1 — the single immutable chart-settings record and the SOLE per-renderer
/// projection functions (`applyToScottPlot`/`applyToPlotly`). This is the §A.7
/// chart-settings slot's field type; Part H fills this shape and creates NO
/// parallel settings container. The set of plottable quantities is the engine DU
/// `OpticalFunction` (`FieldFunctions.fs:114`) — Part H does NOT redefine it.
///
/// Axis/legend/grid/trace-visibility are pushed to a renderer ONLY here. Schema
/// versioning, per-chart mutable config objects, and a settings-migration path are
/// out of scope (§H.1).
module OpticalConstructor.Ui.Charts.ChartSettings

open Berreman.Constants
open Berreman.FieldFunctions
open OpticalConstructor.Domain
open Plotly.NET

/// Per-axis scale. `Log10` is applied through the projection functions below — Part
/// H adds NO separate log-transform of the data (§H.6).
type AxisScale =
    | Linear
    | Log10

/// Per-trace line style (mapped to the renderer's own pattern in the projections).
type LineStyle =
    | SolidLine
    | DashedLine
    | DottedLine

/// One plotted trace: which engine `OpticalFunction` it draws, its legend name,
/// color (hex, e.g. "#1f77b4"), line style, and whether it is shown. Per-trace
/// show/hide flips `visible`; the projections omit hidden traces (§H.6).
type TraceSettings =
    {
        functionId : OpticalFunction
        name : string
        color : string
        lineStyle : LineStyle
        visible : bool
    }

/// The immutable chart-settings record (§H.1). Axis ranges are `float option`
/// (None = renderer auto-scale); when set they are stored in **canonical SI**
/// (meters for a length axis), per R-6/AC-H5 — NOT in the plotted series' display
/// units. The plotted series x-values carry plot-axis units (nm for a wavelength
/// sweep, degrees for angle), so the projection converts each stored bound to the
/// axis display unit (`xUnit`/`yUnit`) through the §A.10 `Units.fromMeters` seam
/// before pushing it to a renderer, so the override shares the trace x-values'
/// scale. `xUnit`/`yUnit = None` means no length-unit conversion (an angle /
/// dimensionless axis): the bound is passed through unchanged.
type ChartSettings =
    {
        xMin : float option
        xMax : float option
        yMin : float option
        yMax : float option
        /// Display unit of the X axis (the unit the trace x-values are shown in);
        /// `None` = no length-unit conversion. The §A.10 boundary conversion only,
        /// applied in the projections — never stored as a non-SI range.
        xUnit : Units.UnitOfMeasure option
        /// Display unit of the Y axis; `None` = no length-unit conversion.
        yUnit : Units.UnitOfMeasure option
        xScale : AxisScale
        yScale : AxisScale
        title : string
        showLegend : bool
        showGrid : bool
        traces : TraceSettings list
    }

    static member defaultValue =
        {
            xMin = None
            xMax = None
            yMin = None
            yMax = None
            xUnit = None
            yUnit = None
            xScale = Linear
            yScale = Linear
            title = ""
            showLegend = true
            showGrid = true
            traces = []
        }

/// §H.6 — the visible-trace projection: the views draw only these, so a
/// `visible = false` trace is omitted.
let visibleTraces (s : ChartSettings) : TraceSettings list =
    s.traces |> List.filter (fun t -> t.visible)

/// Map the renderer-neutral `LineStyle` to ScottPlot's `LinePattern`.
let scottPlotLinePattern (style : LineStyle) : ScottPlot.LinePattern =
    match style with
    | SolidLine -> ScottPlot.LinePattern.Solid
    | DashedLine -> ScottPlot.LinePattern.Dashed
    | DottedLine -> ScottPlot.LinePattern.Dotted

/// Parse a hex color string ("#rrggbb") into ScottPlot's `Color`.
let scottPlotColor (hex : string) : ScottPlot.Color =
    ScottPlot.Color.FromHex(hex)

/// Map `AxisScale` to Plotly's `AxisType` (`Log10` → genuine log axis).
let plotlyAxisType (scale : AxisScale) : StyleParam.AxisType =
    match scale with
    | Linear -> StyleParam.AxisType.Linear
    | Log10 -> StyleParam.AxisType.Log

let private toNullable (v : float option) : System.Nullable<float> =
    match v with
    | Some x -> System.Nullable(x)
    | None -> System.Nullable()

/// §A.10 boundary conversion: project a stored canonical-SI axis bound onto the
/// axis display unit so the range override shares the scale of the (already
/// display-unit) trace x-values. Routes through the SOLE `Units.fromMeters` seam —
/// no conversion is re-implemented here. `None` unit = no length-unit conversion
/// (angle / dimensionless axis): the bound passes through unchanged.
let private toDisplay (unit : Units.UnitOfMeasure option) (v : float option) : float option =
    match unit, v with
    | Some u, Some x -> Some (Units.fromMeters u (x * 1.0<meter>))
    | _, vo -> vo

/// Order-normalize a converted axis-bound pair so the renderer always receives
/// `lo <= hi`. The §A.10 maps for the inverse units `ElectronVolt`/`Wavenumber`
/// (`evNmProduct / nm`) are decreasing, so an ascending stored-SI window converts
/// to a *descending* display pair; left > right would reach the renderer as a
/// reversed/empty axis. Both bounds are still the same two converted values — only
/// their order is fixed. `None`/single-`Some` cases pass through unchanged so the
/// auto-scale semantics are preserved.
let private normalizePair (lo : float option) (hi : float option) : float option * float option =
    match lo, hi with
    | Some a, Some b -> Some (min a b), Some (max a b)
    | _ -> lo, hi

/// §H.1 — push axis ranges, scale, title, legend, gridlines, and per-trace
/// visibility onto a ScottPlot `Plot`. The ONLY place these settings reach
/// ScottPlot. Trace visibility is matched by legend text against `traces`.
let applyToScottPlot (s : ChartSettings) (plot : ScottPlot.Plot) : unit =
    plot.Title(s.title)

    if s.showLegend then plot.ShowLegend() |> ignore else plot.HideLegend() |> ignore
    plot.Legend.IsVisible <- s.showLegend
    plot.Grid.IsVisible <- s.showGrid

    // Axis-range overrides: stored canonical SI, converted to the axis display
    // unit at the boundary (§A.10) so they match the trace scale, then order-
    // normalized so the inverse units (eV/cm⁻¹) don't push left > right.
    // None = auto-scale.
    let xLo, xHi = normalizePair (toDisplay s.xUnit s.xMin) (toDisplay s.xUnit s.xMax)
    let yLo, yHi = normalizePair (toDisplay s.yUnit s.yMin) (toDisplay s.yUnit s.yMax)
    plot.Axes.SetLimits(toNullable xLo, toNullable xHi, toNullable yLo, toNullable yHi)

    // Log scale = a log minor-tick generator on the axis (no data transform, §H.6).
    let logTickGenerator () =
        let tg = ScottPlot.TickGenerators.NumericAutomatic()
        tg.MinorTickGenerator <- ScottPlot.TickGenerators.LogMinorTickGenerator()
        tg :> ScottPlot.ITickGenerator
    match s.xScale with
    | Log10 -> plot.Axes.Bottom.TickGenerator <- logTickGenerator ()
    | Linear -> ()
    match s.yScale with
    | Log10 -> plot.Axes.Left.TickGenerator <- logTickGenerator ()
    | Linear -> ()

    // Per-trace show/hide: flip `IsVisible` on the scatter whose legend matches.
    let visibility = s.traces |> List.map (fun t -> t.name, t.visible) |> Map.ofList
    for p in plot.GetPlottables() do
        match p with
        | :? ScottPlot.Plottables.Scatter as sc ->
            match Map.tryFind sc.LegendText visibility with
            | Some v -> sc.IsVisible <- v
            | None -> ()
        | _ -> ()

/// §H.1 — push axis ranges, scale, and legend visibility onto a Plotly.NET chart.
/// The ONLY place these settings reach Plotly. (Per-trace omission happens at the
/// view boundary via `visibleTraces`, since a combined `GenericChart` is built
/// from the visible traces only.)
let applyToPlotly (s : ChartSettings) (chart : Plotly.NET.GenericChart) : Plotly.NET.GenericChart =
    // Ranges are stored canonical SI; convert to the axis display unit (§A.10) so
    // they share the trace x-values' scale, exactly as the ScottPlot projection does.
    // Converted bounds are order-normalized (as in the ScottPlot projection) so the
    // inverse units (eV/cm⁻¹) reach Plotly's MinMax as `min, max`, not reversed.
    let xMinMax =
        match normalizePair (toDisplay s.xUnit s.xMin) (toDisplay s.xUnit s.xMax) with
        | Some a, Some b -> Some (a, b)
        | _ -> None
    let yMinMax =
        match normalizePair (toDisplay s.yUnit s.yMin) (toDisplay s.yUnit s.yMax) with
        | Some a, Some b -> Some (a, b)
        | _ -> None
    // The chart title is set via `withTitle` (matching the ScottPlot `plot.Title`
    // path); the axis labels set by the chart builder/view are left intact —
    // `withXAxisStyle` is NOT overloaded with the title (cf. `Charting.fs:32,76`).
    chart
    |> Chart.withXAxisStyle (?MinMax = xMinMax, AxisType = plotlyAxisType s.xScale)
    |> Chart.withYAxisStyle (?MinMax = yMinMax, AxisType = plotlyAxisType s.yScale)
    |> Chart.withTitle s.title
