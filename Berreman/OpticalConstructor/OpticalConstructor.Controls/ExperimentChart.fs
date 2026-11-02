namespace OpticalConstructor.Controls

/// Spec 0027 (026) — the renderer-neutral chart data the Experiments bay produces and BOTH the inline
/// scene chart and the pop-out chart window consume. Domain-neutral (no Berreman types) so it crosses the
/// Controls/Ui seam freely; ONE series for an intensity sweep, TWO (Ψ and Δ) for an ellipsometer sweep.
/// The description is prose built from the bound elements + what is swept. A series is the bay's own
/// `ExperimentControls.ChartSeries` (spec 0048 E011: one animal, one declaration).
module ExperimentChart =

    /// Spec 0027 (030): whether the x-axis is an ANGLE (degrees) — a rotation (R1, 0…360°) or an incidence
    /// (R2, 0…90°) sweep. Only an angular chart offers the polar / XY toggle in the pop-out window; purely
    /// a display hint — it does not change the data.
    type ChartXAxisKind =
        | AngularXAxis
        | NonAngularXAxis

    /// A whole chart: its series, axis labels, title, and the human description shown beneath it.
    type ExperimentChart =
        {
            series : ExperimentControls.ChartSeries list
            xLabel : ControlCaption
            yLabel : ControlCaption
            title : ControlCaption
            description : ControlCaption
            xAxisKind : ChartXAxisKind
        }

        /// A pure CSV rendering of the chart: a header row `x,<series names…>` then one row per x (the union of
        /// every series' x, ascending) with each series' y at that x (blank when a series has no point there).
        /// Used by the chart window's CSV export.
        member this.toCsv : string =
            let header = "x" :: (this.series |> List.map (fun s -> s.name.value)) |> String.concat ","
            let lookups = this.series |> List.map (fun s -> s.points |> List.map (fun p -> p.x, p.y) |> Map.ofList)
            let rows =
                this.series
                |> List.collect (fun s -> s.points |> List.map (fun p -> p.x))
                |> List.distinct
                |> List.sort
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

    /// The empty chart (no experiment built / no run to show).
    let emptyExperimentChart : ExperimentChart =
        {
            series = []
            xLabel = ControlCaption ""
            yLabel = ControlCaption ""
            title = ControlCaption ""
            description = ControlCaption ""
            xAxisKind = NonAngularXAxis
        }

/// Spec 0027 (028) — the pure, renderer-neutral font-size state for the pop-out chart window. The old
/// Font +/- affected only one text kind; the redesign lets the user SELECT which text to resize — the
/// header (title), the axis labels, the tick labels, or the legend — each with its OWN size, and Font +/-
/// applies to the SELECTED target, whose size is shown numerically. Kept pure (the window converts a size
/// to ScottPlot's `float32` at the boundary) so the selection / clamp logic is unit-testable without
/// opening a native window.
module ChartFont =

    /// A font measure in points: a text's size, or the signed step Font − / Font + moves a size by.
    type FontPoints =
        | FontPoints of float

        member this.value = let (FontPoints p) = this in p

        /// This size moved by a signed step.
        member this.plus (step : FontPoints) : FontPoints = FontPoints (this.value + step.value)

        /// This size held to the [lowest, highest] band.
        member this.within (lowest : FontPoints) (highest : FontPoints) : FontPoints =
            FontPoints (max lowest.value (min highest.value this.value))

    /// Which text of the chart the Font +/- currently resizes.
    type ChartFontTarget =
        | Title
        | AxisLabels
        | TickLabels
        | LegendFont

        /// A human-readable label (the selector item text and the size readout prefix).
        member this.label : string =
            match this with
            | Title -> "Header"
            | AxisLabels -> "Axis labels"
            | TickLabels -> "Tick labels"
            | LegendFont -> "Legend"

    /// The four selectable targets, in display order.
    let allTargets : ChartFontTarget list = [ Title; AxisLabels; TickLabels; LegendFont ]

    /// The per-target font sizes plus which target the Font +/- currently acts on.
    type ChartFontState =
        {
            title : FontPoints
            axisLabels : FontPoints
            tickLabels : FontPoints
            legend : FontPoints
            selected : ChartFontTarget
        }

        /// The current size of a given target.
        member this.sizeOf (target : ChartFontTarget) : FontPoints =
            match target with
            | Title -> this.title
            | AxisLabels -> this.axisLabels
            | TickLabels -> this.tickLabels
            | LegendFont -> this.legend

        /// The size of the currently-selected target (what the readout shows and Font +/- changes).
        member this.selectedSize : FontPoints = this.sizeOf this.selected

        /// The size readout text (e.g. "Tick labels: 11 pt").
        member this.readout : ControlCaption = ControlCaption $"%s{this.selected.label}: %g{this.selectedSize.value} pt"

    /// The allowed font-size band.
    let minSize : FontPoints = FontPoints 6.0
    let maxSize : FontPoints = FontPoints 40.0

    /// The default sizes (a readable header, slightly smaller axis labels / legend, smaller tick labels).
    let defaultChartFontState : ChartFontState =
        { title = FontPoints 16.0; axisLabels = FontPoints 13.0; tickLabels = FontPoints 11.0; legend = FontPoints 12.0; selected = Title }

    /// Select a different target (leaves every size unchanged; the readout then shows this target's size).
    let withSelected (target : ChartFontTarget) (s : ChartFontState) : ChartFontState =
        { s with selected = target }

    /// Set a target's size (clamped to the band).
    let withSize (target : ChartFontTarget) (size : FontPoints) (s : ChartFontState) : ChartFontState =
        let v = size.within minSize maxSize
        match target with
        | Title -> { s with title = v }
        | AxisLabels -> { s with axisLabels = v }
        | TickLabels -> { s with tickLabels = v }
        | LegendFont -> { s with legend = v }

    /// Change the SELECTED target's size by a signed step (clamped). Font + is +1 pt, Font − is −1 pt.
    let bumpSelected (step : FontPoints) (s : ChartFontState) : ChartFontState =
        withSize s.selected (s.selectedSize.plus step) s

/// Spec 0027 (030) — the pure, renderer-neutral STYLE model for the pop-out chart window: which part of the
/// chart is SELECTED (header, X / Y axis, legend, or a specific series) and the editable properties of each
/// part (axis range + auto + number format; legend visibility + placement; per-series thickness / colour /
/// markers; plus the shared font sizes). Modelled on Excel's "format the selected element" panel. Kept pure
/// (no ScottPlot types) so the selection / range / number-format / polar-conversion logic is unit-testable
/// without opening a native window; `ChartWindow` maps it onto ScottPlot at the IO boundary.
module ChartStyle =

    open ExperimentChart

    /// Which vertical axis a series (or a Y-axis selection) binds to. Spec 0033 (018): the chart carries
    /// TWO independent Y axes (ScottPlot's native left and right), so quantities with different scales
    /// (e.g. n and k) can share one chart without one flattening the other.
    type AxisSide =
        | LeftAxis
        | RightAxis

        /// A human-readable label (the series panel's axis picker item text).
        member this.label : string =
            match this with
            | LeftAxis -> "Left"
            | RightAxis -> "Right"

    /// The two sides, in display order.
    let allSides : AxisSide list = [ LeftAxis; RightAxis ]

    /// Where a series sits in its chart's series list (0 is the first curve).
    type ChartSeriesIndex =
        | ChartSeriesIndex of int

        member this.value = let (ChartSeriesIndex i) = this in i

    /// A selectable part of the chart. `Series i` selects the i-th curve; the two `YAxis` elements format
    /// the left / right vertical axes independently (spec 0033/018).
    type ChartElement =
        | Header
        | XAxis
        | YAxis of AxisSide
        | Legend
        | Series of ChartSeriesIndex

    /// The axis a range / format / digits mutator targets — X, left Y, or right Y (the tri-state that
    /// replaced the boolean `isX` flag when the right axis arrived — spec 0033/018).
    type ChartAxis =
        | ChartXAxis
        | ChartYAxis of AxisSide

    /// The digit count of a Fixed / Scientific number format.
    type DecimalDigits =
        | DecimalDigits of int

        member this.value = let (DecimalDigits d) = this in d

        /// One digit more, or one fewer.
        member this.stepped (direction : StepDirection) : DecimalDigits =
            match direction with
            | StepDecrease -> DecimalDigits (this.value - 1)
            | StepIncrease -> DecimalDigits (this.value + 1)

        /// This count held to the [fewest, most] band.
        member this.within (fewest : DecimalDigits) (most : DecimalDigits) : DecimalDigits =
            DecimalDigits (max fewest.value (min most.value this.value))

    /// How a number is rendered on an axis / readout (Excel-style). Carries its own digit count.
    type NumberFormat =
        | GeneralFormat
        | FixedFormat of decimals : DecimalDigits
        | ScientificFormat of decimals : DecimalDigits

        member this.label : string =
            match this with
            | GeneralFormat -> "General"
            | FixedFormat _ -> "Fixed"
            | ScientificFormat _ -> "Scientific"

        /// The digit count (0 for General, which has none to tune).
        member this.decimals : DecimalDigits =
            match this with
            | GeneralFormat -> DecimalDigits 0
            | FixedFormat d | ScientificFormat d -> d

    /// The digit band a Fixed / Scientific format allows.
    let minDecimals : DecimalDigits = DecimalDigits 0
    let maxDecimals : DecimalDigits = DecimalDigits 10

    /// A value on a chart axis, in that axis' display units (degrees, nm, intensity, Ψ / Δ, n, k, …).
    type ChartAxisValue =
        | ChartAxisValue of float

        member this.value = let (ChartAxisValue v) = this in v

    let private inv = System.Globalization.CultureInfo.InvariantCulture

    /// Render a value under a number format (pure — the axis tick formatter and any readout share this).
    let formatValue (fmt : NumberFormat) (v : ChartAxisValue) : ControlCaption =
        match fmt with
        | GeneralFormat -> ControlCaption (v.value.ToString("0.######", inv))
        | FixedFormat d -> ControlCaption (v.value.ToString("F" + string (d.within minDecimals maxDecimals).value, inv))
        | ScientificFormat d -> ControlCaption (v.value.ToString("E" + string (d.within minDecimals maxDecimals).value, inv))

    /// Set the digit count of a format (no-op for General, which has none).
    let withDecimals (d : DecimalDigits) (fmt : NumberFormat) : NumberFormat =
        let d = d.within minDecimals maxDecimals
        match fmt with
        | GeneralFormat -> GeneralFormat
        | FixedFormat _ -> FixedFormat d
        | ScientificFormat _ -> ScientificFormat d

    /// Whether an axis fits its range to the data, or keeps the range the user pinned.
    type AxisFit =
        | AutoFitAxis
        | PinnedAxis

    /// One axis's style: an explicit [min, max] range OR auto-fit to the data, plus a number format.
    type AxisStyle =
        {
            fit : AxisFit
            min : ChartAxisValue
            max : ChartAxisValue
            format : NumberFormat
        }

    /// The nine legend placements (a DU, mirroring ScottPlot's `Alignment`; `ChartWindow` maps it).
    type LegendPlacement =
        | UpperLeft | UpperCenter | UpperRight
        | MiddleLeft | MiddleCenter | MiddleRight
        | LowerLeft | LowerCenter | LowerRight

        member this.label : string =
            match this with
            | UpperLeft -> "Upper left"   | UpperCenter -> "Upper center"   | UpperRight -> "Upper right"
            | MiddleLeft -> "Middle left" | MiddleCenter -> "Middle center" | MiddleRight -> "Middle right"
            | LowerLeft -> "Lower left"   | LowerCenter -> "Lower center"   | LowerRight -> "Lower right"

    let allPlacements : LegendPlacement list =
        [ UpperLeft; UpperCenter; UpperRight; MiddleLeft; MiddleCenter; MiddleRight; LowerLeft; LowerCenter; LowerRight ]

    /// Whether a part of the chart — the legend, a series' line, a series' markers — is drawn.
    type ChartVisibility =
        | ShownOnChart
        | HiddenOnChart

        /// Whether the part is drawn: the one place this pair becomes ScottPlot's and Avalonia's own `bool`.
        member this.isDrawn : bool =
            match this with
            | ShownOnChart -> true
            | HiddenOnChart -> false

    type LegendStyle =
        {
            visible : ChartVisibility
            placement : LegendPlacement
        }

    /// A series line's width in pixels, or the signed step the thickness − / + moves a width by.
    type LinePixels =
        | LinePixels of float

        member this.value = let (LinePixels w) = this in w

        /// This width moved by a signed step.
        member this.plus (step : LinePixels) : LinePixels = LinePixels (this.value + step.value)

        /// This width held to the [thinnest, thickest] band.
        member this.within (thinnest : LinePixels) (thickest : LinePixels) : LinePixels =
            LinePixels (max thinnest.value (min thickest.value this.value))

    /// A series' `#RRGGBB` line colour.
    type SeriesColorHex =
        | SeriesColorHex of string

        member this.value = let (SeriesColorHex h) = this in h

    /// One series' style: visibility, line thickness, colour, whether markers (dots) show, and which
    /// vertical axis (left or right) the series plots against (spec 0033/018).
    type SeriesStyle =
        {
            visible : ChartVisibility
            thickness : LinePixels
            colorHex : SeriesColorHex
            markers : ChartVisibility
            axisSide : AxisSide
        }

    let minThickness : LinePixels = LinePixels 0.5
    let maxThickness : LinePixels = LinePixels 10.0

    /// The whole chart's style plus the selected element and the shared font sizes. The left and right
    /// Y axes carry INDEPENDENT styles (spec 0033/018).
    type ChartStyleState =
        {
            selected : ChartElement
            font : ChartFont.ChartFontState
            xAxis : AxisStyle
            yAxisLeft : AxisStyle
            yAxisRight : AxisStyle
            legend : LegendStyle
            series : SeriesStyle list
        }

    /// The default series colours, matching the pop-out window's palette; cycled past the end.
    let defaultColors : SeriesColorHex list =
        [ SeriesColorHex "#1E5AC8"; SeriesColorHex "#DC7814"; SeriesColorHex "#289646" ]

    /// A tiny colour palette the series-colour picker offers.
    let colorChoices : SeriesColorHex list =
        [
            SeriesColorHex "#1E5AC8"
            SeriesColorHex "#DC7814"
            SeriesColorHex "#289646"
            SeriesColorHex "#C81E32"
            SeriesColorHex "#7D3CB4"
            SeriesColorHex "#5A5A5A"
            SeriesColorHex "#000000"
        ]

    /// A padded [lower, upper] view range on one axis.
    type ChartAxisRange =
        {
            lower : ChartAxisValue
            upper : ChartAxisValue
        }

        /// The padded range of the plotted values: 3 % of the span (or a half-unit when the span is
        /// degenerate) so the curve clears the frame; a unit box when there are no values (so an axis never
        /// collapses). This is what initializes each axis to "what's in the data" (spec 030) rather than a
        /// default 0…1000.
        static member paddedOver (values : ChartAxisValue list) : ChartAxisRange =
            match values |> List.map (fun v -> v.value) with
            | [] -> { lower = ChartAxisValue 0.0; upper = ChartAxisValue 1.0 }
            | raw ->
                let lo = List.min raw
                let hi = List.max raw
                let span = hi - lo
                if span > 1e-12 then
                    let p = span * 0.03
                    { lower = ChartAxisValue (lo - p); upper = ChartAxisValue (hi + p) }
                else { lower = ChartAxisValue (lo - 0.5); upper = ChartAxisValue (hi + 0.5) }

    /// The padded view ranges of a chart's data: ONE shared x-range (all series share the x grid) and
    /// INDEPENDENT left / right y-ranges, each computed from only the series assigned to that side — a
    /// right-axis series must not stretch the left axis, and vice versa (spec 0033/018).
    type ChartBounds =
        {
            x : ChartAxisRange
            yLeft : ChartAxisRange
            yRight : ChartAxisRange
        }

    /// The bounds of the given (series, axis side) pairs: x over ALL series, each Y side over only its
    /// own series (unit fallback per side, so an unused right axis gets a finite box).
    let dataBounds (series : (ExperimentControls.ChartSeries * AxisSide) list) : ChartBounds =
        let pointsOn (side : AxisSide) : ExperimentControls.ChartPoint list =
            series |> List.filter (fun (_, sd) -> sd = side) |> List.collect (fun (s, _) -> s.points)
        {
            x = ChartAxisRange.paddedOver (series |> List.collect (fun (s, _) -> s.points) |> List.map (fun p -> ChartAxisValue p.x))
            yLeft = ChartAxisRange.paddedOver (pointsOn LeftAxis |> List.map (fun p -> ChartAxisValue p.y))
            yRight = ChartAxisRange.paddedOver (pointsOn RightAxis |> List.map (fun p -> ChartAxisValue p.y))
        }

    /// The initial style for a chart: all three axes auto-fit to the data (general format; every series
    /// starts on the LEFT axis, so the right axis seeds to the unit fallback), the legend visible at the
    /// upper-right, one default-styled series per curve, and the default font sizes.
    let defaultChartStyleState (chart : ExperimentChart) : ChartStyleState =
        let bounds = dataBounds (chart.series |> List.map (fun s -> s, LeftAxis))
        let axis (range : ChartAxisRange) : AxisStyle = { fit = AutoFitAxis; min = range.lower; max = range.upper; format = GeneralFormat }
        let seriesStyles =
            chart.series
            |> List.mapi (fun i _ ->
                {
                    visible = ShownOnChart
                    thickness = LinePixels 1.5
                    colorHex = List.item (i % List.length defaultColors) defaultColors
                    markers = HiddenOnChart
                    axisSide = LeftAxis
                })
        {
            selected = (match chart.series with [] -> Header | _ -> Series (ChartSeriesIndex 0))
            font = ChartFont.defaultChartFontState
            xAxis = axis bounds.x
            yAxisLeft = axis bounds.yLeft
            yAxisRight = axis bounds.yRight
            legend = { visible = ShownOnChart; placement = UpperRight }
            series = seriesStyles
        }

    /// The selectable elements offered by the window's element picker: the five fixed parts (header, the
    /// X axis, the two Y axes, the legend) then one per series.
    let elements (series : ExperimentControls.ChartSeries list) : ChartElement list =
        [ Header; XAxis; YAxis LeftAxis; YAxis RightAxis; Legend ] @ (series |> List.mapi (fun i _ -> Series (ChartSeriesIndex i)))

    /// A human label for an element (the series name is supplied since the model does not hold it).
    let elementLabel (seriesName : ChartSeriesIndex -> ControlCaption) (e : ChartElement) : ControlCaption =
        match e with
        | Header -> ControlCaption "Header"
        | XAxis -> ControlCaption "X axis"
        | YAxis LeftAxis -> ControlCaption "Y axis (left)"
        | YAxis RightAxis -> ControlCaption "Y axis (right)"
        | Legend -> ControlCaption "Legend"
        | Series i -> ControlCaption $"Line: %s{(seriesName i).value}"

    // -- selection + per-element updates (all pure, all clamped) --

    let selectElement (e : ChartElement) (s : ChartStyleState) : ChartStyleState = { s with selected = e }

    /// The style of the given axis (the tri-state lookup the mutators and the window's axis panel share).
    let axisStyleOf (axis : ChartAxis) (s : ChartStyleState) : AxisStyle =
        match axis with
        | ChartXAxis -> s.xAxis
        | ChartYAxis LeftAxis -> s.yAxisLeft
        | ChartYAxis RightAxis -> s.yAxisRight

    let private mapAxis (axis : ChartAxis) (f : AxisStyle -> AxisStyle) (s : ChartStyleState) : ChartStyleState =
        match axis with
        | ChartXAxis -> { s with xAxis = f s.xAxis }
        | ChartYAxis LeftAxis -> { s with yAxisLeft = f s.yAxisLeft }
        | ChartYAxis RightAxis -> { s with yAxisRight = f s.yAxisRight }

    let setAxisFit (axis : ChartAxis) (fit : AxisFit) (s : ChartStyleState) : ChartStyleState =
        mapAxis axis (fun a -> { a with fit = fit }) s

    let setAxisMin (axis : ChartAxis) (v : ChartAxisValue) (s : ChartStyleState) : ChartStyleState =
        mapAxis axis (fun a -> { a with min = v; fit = PinnedAxis }) s

    let setAxisMax (axis : ChartAxis) (v : ChartAxisValue) (s : ChartStyleState) : ChartStyleState =
        mapAxis axis (fun a -> { a with max = v; fit = PinnedAxis }) s

    let setAxisFormat (axis : ChartAxis) (fmt : NumberFormat) (s : ChartStyleState) : ChartStyleState =
        mapAxis axis (fun a -> { a with format = fmt }) s

    let bumpAxisDecimals (axis : ChartAxis) (direction : StepDirection) (s : ChartStyleState) : ChartStyleState =
        mapAxis axis (fun a -> { a with format = withDecimals (a.format.decimals.stepped direction) a.format }) s

    let setLegendVisible (v : ChartVisibility) (s : ChartStyleState) : ChartStyleState =
        { s with legend = { s.legend with visible = v } }

    let setLegendPlacement (p : LegendPlacement) (s : ChartStyleState) : ChartStyleState =
        { s with legend = { s.legend with placement = p } }

    let private mapSeries (i : ChartSeriesIndex) (f : SeriesStyle -> SeriesStyle) (s : ChartStyleState) : ChartStyleState =
        { s with series = s.series |> List.mapi (fun j st -> if j = i.value then f st else st) }

    let setSeriesVisible (i : ChartSeriesIndex) (v : ChartVisibility) (s : ChartStyleState) : ChartStyleState =
        mapSeries i (fun st -> { st with visible = v }) s

    let bumpSeriesThickness (i : ChartSeriesIndex) (step : LinePixels) (s : ChartStyleState) : ChartStyleState =
        mapSeries i (fun st -> { st with thickness = (st.thickness.plus step).within minThickness maxThickness }) s

    let setSeriesColor (i : ChartSeriesIndex) (hex : SeriesColorHex) (s : ChartStyleState) : ChartStyleState =
        mapSeries i (fun st -> { st with colorHex = hex }) s

    let setSeriesMarkers (i : ChartSeriesIndex) (v : ChartVisibility) (s : ChartStyleState) : ChartStyleState =
        mapSeries i (fun st -> { st with markers = v }) s

    /// Assign series `i` to the left or right Y axis (spec 0033/018).
    let setSeriesAxisSide (i : ChartSeriesIndex) (side : AxisSide) (s : ChartStyleState) : ChartStyleState =
        mapSeries i (fun st -> { st with axisSide = side }) s

    /// Bump the font size of whichever font target `target` names (the panel picks the target from the
    /// selected element — Header→Title, an axis→its labels/ticks, Legend→Legend).
    let bumpFont (target : ChartFont.ChartFontTarget) (step : ChartFont.FontPoints) (s : ChartStyleState) : ChartStyleState =
        { s with font = ChartFont.withSize target ((s.font.sizeOf target).plus step) s.font }

    /// The style of series `i` (falls back to a default when the index is out of range, keeping callers total).
    let seriesStyleOf (i : ChartSeriesIndex) (s : ChartStyleState) : SeriesStyle =
        match List.tryItem i.value s.series with
        | Some st -> st
        | None ->
            {
                visible = ShownOnChart
                thickness = LinePixels 1.5
                colorHex = List.head defaultColors
                markers = HiddenOnChart
                axisSide = LeftAxis
            }

    // -- polar representation (spec 030; the point conversion is `ChartPoint.polarXY`) --

    /// The outer radius for a polar plot: the largest |y| across the VISIBLE series (a unit fallback), so the
    /// polar grid's outer circle just contains the data.
    let polarRadius (state : ChartStyleState) (series : ExperimentControls.ChartSeries list) : ChartAxisValue =
        let visible =
            series
            |> List.mapi (fun i s -> ChartSeriesIndex i, s)
            |> List.filter (fun (i, _) -> (seriesStyleOf i state).visible = ShownOnChart)
            |> List.collect (fun (_, s) -> s.points |> List.map (fun p -> abs p.y))
        match visible with
        | [] -> ChartAxisValue 1.0
        | ys -> ChartAxisValue (max 1e-9 (List.max ys))
