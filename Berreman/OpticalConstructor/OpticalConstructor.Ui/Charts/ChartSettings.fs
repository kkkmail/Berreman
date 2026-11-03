/// §H.1 — the single immutable chart-settings record and the SOLE per-renderer
/// projection functions (`applyToScottPlot`/`applyToPlotly`). This is the §A.7
/// chart-settings slot's field type; Part H fills this shape and creates NO
/// parallel settings container. The set of plottable quantities is the engine DU
/// `OpticalFunction` (`FieldFunctions.fs:114`) — Part H does NOT redefine it.
///
/// Axis/legend/grid/trace-visibility are pushed to a renderer ONLY here. Schema
/// versioning, per-chart mutable config objects, and a settings-migration path are
/// out of scope (§H.1).
///
/// Spec 0033 (017): the renderer-neutral chart model (`ExperimentChart` / `ChartStyle`) and its
/// ScottPlot seam (`ChartWindow` / `ChartRender`) are the ONE shared model, now hosted in
/// `OpticalConstructor.Controls` where this project can reach them. This module stays the Ui-side
/// §H.1 settings record over that seam and introduces NO third settings type.
module OpticalConstructor.Ui.Charts.ChartSettings

open Berreman.Constants
open Berreman.FieldFunctions
open OpticalConstructor.Domain
open OpticalConstructor.Controls
open OpticalConstructor.Controls.ChartStyle
open Plotly.NET

/// Per-axis scale. `Log10` is applied through the projection functions below — Part
/// H adds NO separate log-transform of the data (§H.6).
type AxisScale =
    | Linear
    | Log10

    /// Plotly's `AxisType` for this scale (`Log10` → genuine log axis).
    member this.plotlyAxisType : StyleParam.AxisType =
        match this with
        | Linear -> StyleParam.AxisType.Linear
        | Log10 -> StyleParam.AxisType.Log

/// Per-trace line style (mapped to the renderer's own pattern in the projections).
type LineStyle =
    | SolidLine
    | DashedLine
    | DottedLine

    /// ScottPlot's `LinePattern` for this style.
    member this.scottPlotLinePattern : ScottPlot.LinePattern =
        match this with
        | SolidLine -> ScottPlot.LinePattern.Solid
        | DashedLine -> ScottPlot.LinePattern.Dashed
        | DottedLine -> ScottPlot.LinePattern.Dotted

/// One plotted trace: which engine `OpticalFunction` it draws, its legend name,
/// color (hex, e.g. "#1f77b4"), line style, and whether it is shown. Per-trace
/// show/hide flips `visible`; the projections omit hidden traces (§H.6).
type TraceSettings =
    {
        functionId : OpticalFunction
        name : ControlCaption
        color : SeriesColorHex
        lineStyle : LineStyle
        visible : ChartVisibility
    }

type Units.UnitMagnitude with

    /// This magnitude as a value on a chart axis shown in its unit.
    member this.toChartAxisValue : ChartAxisValue = ChartAxisValue this.value

/// An axis-range override as stored (R-6 / AC-H5): canonical SI (meters) on a length
/// axis, the plotted value itself on an angle or dimensionless axis.
type ChartAxisBound =
    | ChartAxisBound of float

    member this.value = let (ChartAxisBound b) = this in b

    /// §A.10 boundary conversion: this bound on the axis display unit, so the range
    /// override shares the scale of the (already display-unit) trace x-values. Routes
    /// through the SOLE `Units.fromMeters` seam — no conversion is re-implemented here.
    /// `None` unit = no length-unit conversion: the bound passes through unchanged.
    member this.displayed (unit : Units.UnitOfMeasure option) : ChartAxisValue =
        match unit with
        | Some u -> (Units.fromMeters u (this.value * 1.0<meter>)).toChartAxisValue
        | None -> ChartAxisValue this.value

/// The immutable chart-settings record (§H.1). Axis ranges are `ChartAxisBound option`
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
        xMin : ChartAxisBound option
        xMax : ChartAxisBound option
        yMin : ChartAxisBound option
        yMax : ChartAxisBound option
        /// Display unit of the X axis (the unit the trace x-values are shown in);
        /// `None` = no length-unit conversion. The §A.10 boundary conversion only,
        /// applied in the projections — never stored as a non-SI range.
        xUnit : Units.UnitOfMeasure option
        /// Display unit of the Y axis; `None` = no length-unit conversion.
        yUnit : Units.UnitOfMeasure option
        xScale : AxisScale
        yScale : AxisScale
        title : ControlCaption
        showLegend : ChartVisibility
        showGrid : ChartVisibility
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
            title = ControlCaption ""
            showLegend = ShownOnChart
            showGrid = ShownOnChart
            traces = []
        }

/// §H.6 — the visible-trace projection: the views draw only these, so a
/// `HiddenOnChart` trace is omitted.
let visibleTraces (s : ChartSettings) : TraceSettings list =
    s.traces |> List.filter (fun t -> t.visible.isDrawn)

/// One axis's range override on the display scale; an unset end auto-scales.
type private DisplayLimits =
    {
        low : ChartAxisValue option
        high : ChartAxisValue option
    }

/// Project a stored override pair onto the axis display unit (`ChartAxisBound.displayed`)
/// and order-normalize it so the renderer always receives `lo <= hi`. The §A.10 maps for
/// the inverse units `ElectronVolt`/`Wavenumber` (`evNmProduct / nm`) are decreasing, so an
/// ascending stored-SI window converts to a *descending* display pair; left > right would
/// reach the renderer as a reversed/empty axis. Both bounds are still the same two
/// converted values — only their order is fixed. `None`/single-`Some` cases pass through
/// unchanged so the auto-scale semantics are preserved.
let private displayLimits (unit : Units.UnitOfMeasure option) (lo : ChartAxisBound option) (hi : ChartAxisBound option) : DisplayLimits =
    let low = lo |> Option.map (fun b -> b.displayed unit)
    let high = hi |> Option.map (fun b -> b.displayed unit)
    match low, high with
    | Some a, Some b -> { low = Some (ChartAxisValue (min a.value b.value)); high = Some (ChartAxisValue (max a.value b.value)) }
    | _ -> { low = low; high = high }

/// §H.1 — push axis ranges, scale, title, legend, gridlines, and per-trace
/// visibility onto a ScottPlot `Plot`. The ONLY place these settings reach
/// ScottPlot. Trace visibility is matched by legend text against `traces`.
let applyToScottPlot (s : ChartSettings) (plot : ScottPlot.Plot) : unit =
    plot.Title(s.title.value)

    match s.showLegend with
    | ShownOnChart -> plot.ShowLegend() |> ignore
    | HiddenOnChart -> plot.HideLegend() |> ignore
    plot.Legend.IsVisible <- s.showLegend.isDrawn
    plot.Grid.IsVisible <- s.showGrid.isDrawn

    // Axis-range overrides: stored canonical SI, converted to the axis display
    // unit at the boundary (§A.10) so they match the trace scale, then order-
    // normalized so the inverse units (eV/cm⁻¹) don't push left > right.
    // None = auto-scale.
    let nullable (v : ChartAxisValue option) : System.Nullable<float> =
        match v with
        | Some x -> System.Nullable(x.value)
        | None -> System.Nullable()
    let xLimits = displayLimits s.xUnit s.xMin s.xMax
    let yLimits = displayLimits s.yUnit s.yMin s.yMax
    plot.Axes.SetLimits(nullable xLimits.low, nullable xLimits.high, nullable yLimits.low, nullable yLimits.high)

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
    let visibility = s.traces |> List.map (fun t -> t.name.value, t.visible.isDrawn) |> Map.ofList
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
    let minMax (limits : DisplayLimits) : (float * float) option =
        match limits.low, limits.high with
        | Some a, Some b -> Some (a.value, b.value)
        | _ -> None
    let xMinMax = minMax (displayLimits s.xUnit s.xMin s.xMax)
    let yMinMax = minMax (displayLimits s.yUnit s.yMin s.yMax)
    // The chart title is set via `withTitle` (matching the ScottPlot `plot.Title`
    // path); the axis labels set by the chart builder/view are left intact —
    // `withXAxisStyle` is NOT overloaded with the title (cf. `Charting.fs:32,76`).
    chart
    |> Chart.withXAxisStyle (?MinMax = xMinMax, AxisType = s.xScale.plotlyAxisType)
    |> Chart.withYAxisStyle (?MinMax = yMinMax, AxisType = s.yScale.plotlyAxisType)
    |> Chart.withTitle s.title.value
