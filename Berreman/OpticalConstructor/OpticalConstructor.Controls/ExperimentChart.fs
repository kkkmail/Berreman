namespace OpticalConstructor.Controls

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
            /// Spec 0027 (030): whether the x-axis is an ANGLE (degrees) — a rotation (R1, 0…360°) or an
            /// incidence (R2, 0…90°) sweep. Only angular charts offer the polar / XY representation toggle in
            /// the pop-out window (a wavelength sweep is not angular). Purely a display hint — it does not
            /// change the data.
            angular : bool
        }

    /// The empty chart (no experiment built / no run to show).
    let empty : ExperimentChart =
        { series = []; xLabel = ""; yLabel = ""; title = ""; description = ""; angular = false }

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

    /// A selectable part of the chart. `Series i` selects the i-th curve; the two `YAxis` elements format
    /// the left / right vertical axes independently (spec 0033/018).
    type ChartElement =
        | Header
        | XAxis
        | YAxis of AxisSide
        | Legend
        | Series of int

    /// The axis a range / format / digits mutator targets — X, left Y, or right Y (the tri-state that
    /// replaced the boolean `isX` flag when the right axis arrived — spec 0033/018).
    type ChartAxis =
        | AxisX
        | AxisY of AxisSide

    /// How a number is rendered on an axis / readout (Excel-style). Carries its own digit count.
    type NumberFormat =
        | GeneralFormat
        | FixedFormat of decimals : int
        | ScientificFormat of decimals : int

        member this.label : string =
            match this with
            | GeneralFormat -> "General"
            | FixedFormat _ -> "Fixed"
            | ScientificFormat _ -> "Scientific"

        /// The digit count (0 for General, which has none to tune).
        member this.decimals : int =
            match this with
            | GeneralFormat -> 0
            | FixedFormat d | ScientificFormat d -> d

    /// The digit band a Fixed / Scientific format allows.
    let minDecimals : int = 0
    let maxDecimals : int = 10

    let private inv = System.Globalization.CultureInfo.InvariantCulture

    /// Render a value under a number format (pure — the axis tick formatter and any readout share this).
    let formatValue (fmt : NumberFormat) (v : float) : string =
        match fmt with
        | GeneralFormat -> v.ToString("0.######", inv)
        | FixedFormat d -> v.ToString("F" + string (max minDecimals (min maxDecimals d)), inv)
        | ScientificFormat d -> v.ToString("E" + string (max minDecimals (min maxDecimals d)), inv)

    /// Set the digit count of a format (no-op for General, which has none).
    let withDecimals (d : int) (fmt : NumberFormat) : NumberFormat =
        let d = max minDecimals (min maxDecimals d)
        match fmt with
        | GeneralFormat -> GeneralFormat
        | FixedFormat _ -> FixedFormat d
        | ScientificFormat _ -> ScientificFormat d

    /// One axis's style: an explicit [min, max] range OR auto-fit to the data, plus a number format.
    type AxisStyle =
        {
            auto : bool
            min : float
            max : float
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

    type LegendStyle =
        {
            visible : bool
            placement : LegendPlacement
        }

    /// One series' style: visibility, line thickness, colour (`#RRGGBB`), whether markers (dots) show,
    /// and which vertical axis (left or right) the series plots against (spec 0033/018).
    type SeriesStyle =
        {
            visible : bool
            thickness : float
            colorHex : string
            showMarkers : bool
            axisSide : AxisSide
        }

    let minThickness : float = 0.5
    let maxThickness : float = 10.0

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

    /// The default series colours (`#RRGGBB`), matching the pop-out window's palette; cycled past the end.
    let defaultColors : string list = [ "#1E5AC8"; "#DC7814"; "#289646" ]

    /// A tiny colour palette the series-colour picker offers.
    let colorChoices : string list =
        [ "#1E5AC8"; "#DC7814"; "#289646"; "#C81E32"; "#7D3CB4"; "#5A5A5A"; "#000000" ]

    /// The padded [lo, hi] view ranges of a chart's data: ONE shared x-range (all series share the x grid)
    /// and INDEPENDENT left / right y-ranges, each computed from only the series assigned to that side —
    /// a right-axis series must not stretch the left axis, and vice versa (spec 0033/018).
    type ChartBounds =
        {
            x : float * float
            yLeft : float * float
            yRight : float * float
        }

    /// The padded [lo, hi] range of a value list: 3 % of the span (or a half-unit when the span is
    /// degenerate) so the curve clears the frame; a unit box when there are no values (so an axis never
    /// collapses). Pure — this is what initializes each axis to "what's in the data" (spec 030) rather
    /// than a default 0…1000.
    let private paddedRange (values : float list) : float * float =
        match values with
        | [] -> 0.0, 1.0
        | _ ->
            let lo, hi = List.min values, List.max values
            let span = hi - lo
            if span > 1e-12 then let p = span * 0.03 in lo - p, hi + p
            else lo - 0.5, hi + 0.5

    /// The bounds of the given (series, axis side) pairs: x over ALL series, each Y side over only its
    /// own series (unit fallback per side, so an unused right axis gets a finite box).
    let dataBounds (series : (ChartSeries * AxisSide) list) : ChartBounds =
        let pointsOn (side : AxisSide) : (float * float) list =
            series |> List.filter (fun (_, sd) -> sd = side) |> List.collect (fun (s, _) -> s.points)
        {
            x = paddedRange (series |> List.collect (fun (s, _) -> s.points) |> List.map fst)
            yLeft = paddedRange (pointsOn LeftAxis |> List.map snd)
            yRight = paddedRange (pointsOn RightAxis |> List.map snd)
        }

    /// The initial style for a chart: all three axes auto-fit to the data (general format; every series
    /// starts on the LEFT axis, so the right axis seeds to the unit fallback), the legend visible at the
    /// upper-right, one default-styled series per curve, and the default font sizes.
    let defaultState (chart : ExperimentChart) : ChartStyleState =
        let bounds = dataBounds (chart.series |> List.map (fun s -> s, LeftAxis))
        let axis (lo, hi) = { auto = true; min = lo; max = hi; format = GeneralFormat }
        let seriesStyles =
            chart.series
            |> List.mapi (fun i _ ->
                { visible = true; thickness = 1.5; colorHex = List.item (i % List.length defaultColors) defaultColors; showMarkers = false; axisSide = LeftAxis })
        {
            selected = (match chart.series with [] -> Header | _ -> Series 0)
            font = ChartFont.defaultState
            xAxis = axis bounds.x
            yAxisLeft = axis bounds.yLeft
            yAxisRight = axis bounds.yRight
            legend = { visible = true; placement = UpperRight }
            series = seriesStyles
        }

    /// The selectable elements offered by the window's element picker: the five fixed parts (header, the
    /// X axis, the two Y axes, the legend) then one per series (labelled by the series' name, supplied by
    /// the caller).
    let elements (seriesCount : int) : ChartElement list =
        [ Header; XAxis; YAxis LeftAxis; YAxis RightAxis; Legend ] @ [ for i in 0 .. seriesCount - 1 -> Series i ]

    /// A human label for an element (the series name is supplied since the model does not hold it).
    let elementLabel (seriesName : int -> string) (e : ChartElement) : string =
        match e with
        | Header -> "Header"
        | XAxis -> "X axis"
        | YAxis LeftAxis -> "Y axis (left)"
        | YAxis RightAxis -> "Y axis (right)"
        | Legend -> "Legend"
        | Series i -> sprintf "Line: %s" (seriesName i)

    // -- selection + per-element updates (all pure, all clamped) --

    let selectElement (e : ChartElement) (s : ChartStyleState) : ChartStyleState = { s with selected = e }

    /// The style of the given axis (the tri-state lookup the mutators and the window's axis panel share).
    let axisStyleOf (axis : ChartAxis) (s : ChartStyleState) : AxisStyle =
        match axis with
        | AxisX -> s.xAxis
        | AxisY LeftAxis -> s.yAxisLeft
        | AxisY RightAxis -> s.yAxisRight

    let private mapAxis (axis : ChartAxis) (f : AxisStyle -> AxisStyle) (s : ChartStyleState) : ChartStyleState =
        match axis with
        | AxisX -> { s with xAxis = f s.xAxis }
        | AxisY LeftAxis -> { s with yAxisLeft = f s.yAxisLeft }
        | AxisY RightAxis -> { s with yAxisRight = f s.yAxisRight }

    let setAxisAuto (axis : ChartAxis) (auto : bool) (s : ChartStyleState) : ChartStyleState =
        mapAxis axis (fun a -> { a with auto = auto }) s

    let setAxisMin (axis : ChartAxis) (v : float) (s : ChartStyleState) : ChartStyleState =
        mapAxis axis (fun a -> { a with min = v; auto = false }) s

    let setAxisMax (axis : ChartAxis) (v : float) (s : ChartStyleState) : ChartStyleState =
        mapAxis axis (fun a -> { a with max = v; auto = false }) s

    let setAxisFormat (axis : ChartAxis) (fmt : NumberFormat) (s : ChartStyleState) : ChartStyleState =
        mapAxis axis (fun a -> { a with format = fmt }) s

    let bumpAxisDecimals (axis : ChartAxis) (delta : int) (s : ChartStyleState) : ChartStyleState =
        mapAxis axis (fun a -> { a with format = withDecimals (a.format.decimals + delta) a.format }) s

    let setLegendVisible (v : bool) (s : ChartStyleState) : ChartStyleState =
        { s with legend = { s.legend with visible = v } }

    let setLegendPlacement (p : LegendPlacement) (s : ChartStyleState) : ChartStyleState =
        { s with legend = { s.legend with placement = p } }

    let private mapSeries (i : int) (f : SeriesStyle -> SeriesStyle) (s : ChartStyleState) : ChartStyleState =
        { s with series = s.series |> List.mapi (fun j st -> if j = i then f st else st) }

    let setSeriesVisible (i : int) (v : bool) (s : ChartStyleState) : ChartStyleState =
        mapSeries i (fun st -> { st with visible = v }) s

    let bumpSeriesThickness (i : int) (delta : float) (s : ChartStyleState) : ChartStyleState =
        mapSeries i (fun st -> { st with thickness = max minThickness (min maxThickness (st.thickness + delta)) }) s

    let setSeriesColor (i : int) (hex : string) (s : ChartStyleState) : ChartStyleState =
        mapSeries i (fun st -> { st with colorHex = hex }) s

    let setSeriesMarkers (i : int) (v : bool) (s : ChartStyleState) : ChartStyleState =
        mapSeries i (fun st -> { st with showMarkers = v }) s

    /// Assign series `i` to the left or right Y axis (spec 0033/018).
    let setSeriesAxisSide (i : int) (side : AxisSide) (s : ChartStyleState) : ChartStyleState =
        mapSeries i (fun st -> { st with axisSide = side }) s

    /// Bump the font size of whichever font target `target` names (the panel picks the target from the
    /// selected element — Header→Title, an axis→its labels/ticks, Legend→Legend).
    let bumpFont (target : ChartFont.ChartFontTarget) (delta : float) (s : ChartStyleState) : ChartStyleState =
        { s with font = ChartFont.withSize target (ChartFont.sizeOf target s.font + delta) s.font }

    /// The style of series `i` (falls back to a default when the index is out of range, keeping callers total).
    let seriesStyleOf (i : int) (s : ChartStyleState) : SeriesStyle =
        match List.tryItem i s.series with
        | Some st -> st
        | None -> { visible = true; thickness = 1.5; colorHex = List.head defaultColors; showMarkers = false; axisSide = LeftAxis }

    // -- polar representation (spec 030) --

    /// Convert an angular data point (angleDeg, radius) to cartesian (x, y). Pure counterpart of ScottPlot's
    /// `PolarAxis.GetCoordinates`, exercised by the tests; the window uses ScottPlot's own conversion so the
    /// polar GRID lines up with the plotted points.
    let polarXY (angleDeg : float, radius : float) : float * float =
        let t = angleDeg * System.Math.PI / 180.0
        radius * cos t, radius * sin t

    /// The outer radius for a polar plot: the largest |y| across the VISIBLE series (a unit fallback), so the
    /// polar grid's outer circle just contains the data.
    let polarRadius (state : ChartStyleState) (series : ChartSeries list) : float =
        let visible =
            series
            |> List.mapi (fun i s -> i, s)
            |> List.filter (fun (i, _) -> (seriesStyleOf i state).visible)
            |> List.collect (fun (_, s) -> s.points |> List.map (fun (_, y) -> abs y))
        match visible with
        | [] -> 1.0
        | ys -> max 1e-9 (List.max ys)
