namespace OpticalConstructor.TestWindows

open System
open System.IO
open Avalonia
open Avalonia.Controls
open Avalonia.Layout
open Avalonia.Media
open OpticalConstructor.TestWindows.ExperimentChart

/// Spec 0027 (026/028/030) — the pop-out interactive chart window (double-click the inline Experiments
/// chart). A plain Avalonia `Window` hosting a `ScottPlot.Avalonia.AvaPlot`. Spec 030 turns it into an
/// Excel-style "format the selected element" surface: the x-axis is initialized to the DATA range (not a
/// default 0…1000); an ELEMENT PICKER selects a part of the chart (header / X axis / Y axis / legend / a
/// series) and a contextual PROPERTIES panel edits what applies to that part (axis range + auto + number
/// format + font; legend visibility + placement + font; series thickness + colour + markers); and an
/// angular experiment (a rotation or an incidence sweep) offers a POLAR ⇄ XY toggle. The pure selection /
/// range / number-format / polar-conversion logic lives in `ChartStyle`; this file is the ScottPlot IO seam.
/// The window is never opened under the headless `ui-smoke` gate's frame render, so its native ScottPlot
/// rendering cannot break that gate.

/// Stable automation ids for the chart window's controls (CLAUDE.md: centralize ids).
[<RequireQualifiedAccess>]
module ChartWindowIds =
    let plot = "ChartWindowPlot"
    /// Spec 030: the element picker (which part of the chart is being formatted) and the properties panel.
    let elementSelector = "ChartWindowElement"
    let propertiesPanel = "ChartWindowProperties"
    /// The selected element's font stepper + readout (shown for the header / axes / legend).
    let fontMinus = "ChartWindowFontMinus"
    let fontPlus = "ChartWindowFontPlus"
    let fontSize = "ChartWindowFontSize"
    /// Axis property controls.
    let axisAuto = "ChartWindowAxisAuto"
    let axisMin = "ChartWindowAxisMin"
    let axisMax = "ChartWindowAxisMax"
    let axisFormat = "ChartWindowAxisFormat"
    let axisDecimalsMinus = "ChartWindowAxisDecimalsMinus"
    let axisDecimalsPlus = "ChartWindowAxisDecimalsPlus"
    let axisDecimals = "ChartWindowAxisDecimals"
    /// Legend property controls.
    let legendVisible = "ChartWindowLegendVisible"
    let legendPlacement = "ChartWindowLegendPlacement"
    /// Series (chart line) property controls.
    let seriesVisible = "ChartWindowSeriesVisible"
    let seriesThicknessMinus = "ChartWindowSeriesThickMinus"
    let seriesThicknessPlus = "ChartWindowSeriesThickPlus"
    let seriesThickness = "ChartWindowSeriesThick"
    let seriesColor = "ChartWindowSeriesColor"
    let seriesMarkers = "ChartWindowSeriesMarkers"
    /// The polar ⇄ XY toggle (only for angular charts).
    let polarToggle = "ChartWindowPolar"
    let majorGrid = "ChartWindowMajorGrid"
    let minorGrid = "ChartWindowMinorGrid"
    let exportPng = "ChartWindowExportPng"
    let exportCsv = "ChartWindowExportCsv"
    let description = "ChartWindowDescription"

type ChartWindow(chart : ExperimentChart) as this =
    inherit Window()

    let mutable style = ChartStyle.defaultState chart
    let mutable polar = false
    let seriesCount = List.length chart.series
    let seriesName (i : int) : string = match List.tryItem i chart.series with Some s -> s.name | None -> sprintf "series %d" i

    do
        this.Title <- (if chart.title = "" then "Experiment chart" else chart.title)
        this.Width <- 980.0
        this.Height <- 680.0

        let ava = new ScottPlot.Avalonia.AvaPlot(Name = ChartWindowIds.plot)
        let plot = ava.Plot

        let mutable scatters : ScottPlot.Plottables.Scatter list = []
        let mutable crosshair : ScottPlot.Plottables.Crosshair = null
        let mutable marker : ScottPlot.Plottables.Marker = null
        let mutable readout : ScottPlot.Plottables.Text = null

        let colorOf (hex : string) : ScottPlot.Color = ScottPlot.Color.FromHex hex

        let placementAlignment (p : ChartStyle.LegendPlacement) : ScottPlot.Alignment =
            match p with
            | ChartStyle.UpperLeft -> ScottPlot.Alignment.UpperLeft
            | ChartStyle.UpperCenter -> ScottPlot.Alignment.UpperCenter
            | ChartStyle.UpperRight -> ScottPlot.Alignment.UpperRight
            | ChartStyle.MiddleLeft -> ScottPlot.Alignment.MiddleLeft
            | ChartStyle.MiddleCenter -> ScottPlot.Alignment.MiddleCenter
            | ChartStyle.MiddleRight -> ScottPlot.Alignment.MiddleRight
            | ChartStyle.LowerLeft -> ScottPlot.Alignment.LowerLeft
            | ChartStyle.LowerCenter -> ScottPlot.Alignment.LowerCenter
            | ChartStyle.LowerRight -> ScottPlot.Alignment.LowerRight

        /// The data bounds of the currently-visible series (what "Auto" and the initial view fit to).
        let dataBoundsVisible () : float * float * float * float =
            let visible =
                chart.series
                |> List.mapi (fun i s -> i, s)
                |> List.filter (fun (i, _) -> (ChartStyle.seriesStyleOf i style).visible)
                |> List.map snd
            ChartStyle.dataBounds (match visible with [] -> chart.series | v -> v)

        let applyAxisFonts () : unit =
            let setAxis (panel : obj) : unit =
                match panel with
                | :? ScottPlot.AxisPanels.AxisBase as a ->
                    a.Label.FontSize <- float32 style.font.axisLabels
                    a.TickLabelStyle.FontSize <- float32 style.font.tickLabels
                | _ -> ()
            setAxis plot.Axes.Bottom
            setAxis plot.Axes.Left

        let applyLegendAndFonts () : unit =
            plot.Axes.Title.Label.FontSize <- float32 style.font.title
            applyAxisFonts ()
            plot.Legend.IsVisible <- style.legend.visible
            plot.Legend.Alignment <- placementAlignment style.legend.placement
            plot.Legend.FontSize <- System.Nullable (float32 style.font.legend)

        let applyAxisFormat () : unit =
            let setFmt (panel : obj) (fmt : ChartStyle.NumberFormat) : unit =
                match panel with
                | :? ScottPlot.AxisPanels.AxisBase as a ->
                    match a.TickGenerator with
                    | :? ScottPlot.TickGenerators.NumericAutomatic as na ->
                        na.LabelFormatter <- System.Func<float, string>(fun v -> ChartStyle.formatValue fmt v)
                    | _ -> ()
                | _ -> ()
            setFmt plot.Axes.Bottom style.xAxis.format
            setFmt plot.Axes.Left style.yAxis.format

        let applyAxisLimits () : unit =
            let xlo, xhi, ylo, yhi = dataBoundsVisible ()
            (if style.xAxis.auto then plot.Axes.SetLimitsX(xlo, xhi) else plot.Axes.SetLimitsX(style.xAxis.min, style.xAxis.max))
            (if style.yAxis.auto then plot.Axes.SetLimitsY(ylo, yhi) else plot.Axes.SetLimitsY(style.yAxis.min, style.yAxis.max))

        let applySeriesStyle () : unit =
            scatters
            |> List.iteri (fun i sc ->
                let st = ChartStyle.seriesStyleOf i style
                sc.IsVisible <- st.visible
                sc.LineWidth <- float32 st.thickness
                sc.Color <- colorOf st.colorHex
                sc.MarkerShape <- (if st.showMarkers then ScottPlot.MarkerShape.FilledCircle else ScottPlot.MarkerShape.None)
                sc.MarkerSize <- 5.0f)

        /// Show / hide the rectangular (cartesian) axes. `Plot.Add.PolarAxis` HIDES them so the polar grid
        /// reads cleanly; returning to XY must show them again or the plot renders with no axes / ticks /
        /// frame (spec 030 fix — switching back from polar broke the XY view).
        let setCartesianAxesVisible (v : bool) : unit =
            (match plot.Axes.Bottom with :? ScottPlot.AxisPanels.AxisBase as a -> a.IsVisible <- v | _ -> ())
            (match plot.Axes.Left with :? ScottPlot.AxisPanels.AxisBase as a -> a.IsVisible <- v | _ -> ())

        /// Push the whole style onto the plot: series look, legend, fonts, and (cartesian only) the axis
        /// number format + limits (polar auto-fits its circular grid instead).
        let applyStyle () : unit =
            applySeriesStyle ()
            applyLegendAndFonts ()
            if polar then plot.Axes.AutoScale()
            else
                setCartesianAxesVisible true
                applyAxisFormat ()
                applyAxisLimits ()
            ava.Refresh()

        // The snapping crosshair (cartesian only): a cross + dot + label following the nearest data point.
        let setupCrosshair () : unit =
            crosshair <- plot.Add.Crosshair(0.0, 0.0)
            crosshair.IsVisible <- false
            marker <- plot.Add.Marker(0.0, 0.0, ScottPlot.MarkerShape.FilledCircle, 8.0f, System.Nullable ())
            marker.IsVisible <- false
            readout <- plot.Add.Text("", 0.0, 0.0)
            readout.IsVisible <- false

        /// (Re)build the plottables — cartesian scatters, or a polar axis + polar-projected lines — then
        /// re-apply the style. Called on load and whenever the polar ⇄ XY toggle flips.
        let rebuildPlot () : unit =
            plot.Clear()
            scatters <-
                if polar then
                    let r = ChartStyle.polarRadius style chart.series
                    let pax = plot.Add.PolarAxis(r)
                    pax.SetCircles(r, 5)
                    chart.series
                    |> List.mapi (fun i s ->
                        let coords = s.points |> List.map (fun (deg, v) -> pax.GetCoordinates(v, deg)) |> List.toArray
                        let sc = plot.Add.ScatterLine(coords, System.Nullable (colorOf (ChartStyle.seriesStyleOf i style).colorHex))
                        sc.LegendText <- s.name
                        sc)
                else
                    chart.series
                    |> List.mapi (fun i s ->
                        let xs = s.points |> List.map fst |> List.toArray
                        let ys = s.points |> List.map snd |> List.toArray
                        let sc = plot.Add.Scatter(xs, ys, System.Nullable (colorOf (ChartStyle.seriesStyleOf i style).colorHex))
                        sc.LegendText <- s.name
                        sc)
            setupCrosshair ()
            applyStyle ()

        rebuildPlot ()
        plot.XLabel(chart.xLabel)
        plot.YLabel(chart.yLabel)
        plot.Title(this.Title)
        plot.ShowLegend() |> ignore

        ava.PointerMoved.Add(fun e ->
            if not polar then
                match scatters with
                | [] -> ()
                | sc :: _ ->
                    try
                        let p = e.GetPosition(ava)
                        let pixel = ScottPlot.Pixel(float32 p.X, float32 p.Y)
                        let coord = plot.GetCoordinates(pixel, plot.Axes.Bottom, plot.Axes.Left)
                        let near = sc.Data.GetNearest(coord, plot.LastRender, 1e10f)
                        if near.IsReal then
                            crosshair.Position <- near.Coordinates
                            crosshair.IsVisible <- true
                            marker.Location <- near.Coordinates
                            marker.IsVisible <- true
                            readout.Location <- near.Coordinates
                            readout.LabelText <- sprintf "x=%.4g, y=%.4g" near.Coordinates.X near.Coordinates.Y
                            readout.IsVisible <- true
                            ava.Refresh()
                    with _ -> ())

        // ----------------------------------------------------------------------------------------------
        // The properties panel (right dock): the element picker + the contextual editors for the selection.
        // ----------------------------------------------------------------------------------------------

        let smallLabel (t : string) : TextBlock =
            TextBlock(Text = t, VerticalAlignment = VerticalAlignment.Center, Margin = Thickness(0.0, 0.0, 6.0, 0.0))

        let row (children : Control list) : StackPanel =
            let sp = StackPanel(Orientation = Orientation.Horizontal, Spacing = 4.0, Margin = Thickness(0.0, 0.0, 0.0, 6.0))
            children |> List.iter (fun c -> sp.Children.Add c)
            sp

        let propertiesHost =
            StackPanel(Name = ChartWindowIds.propertiesPanel, Orientation = Orientation.Vertical, Margin = Thickness(8.0), Spacing = 2.0)

        // A number text field committing a float on Enter / blur.
        let numberField (id : string) (value : float) (enabled : bool) (onCommit : float -> unit) : TextBox =
            let tb = TextBox(Name = id, Width = 84.0, IsEnabled = enabled, Text = ChartStyle.formatValue ChartStyle.GeneralFormat value)
            let commit () =
                match Double.TryParse(tb.Text, Globalization.NumberStyles.Float, Globalization.CultureInfo.InvariantCulture) with
                | true, v -> onCommit v
                | _ -> ()
            tb.LostFocus.Add(fun _ -> commit ())
            tb.KeyDown.Add(fun e -> if e.Key = Input.Key.Enter then commit ())
            tb

        // Forward-declared so a control's handler can rebuild the panel after mutating `style`.
        let mutable rebuildProperties : unit -> unit = fun () -> ()

        /// A font stepper (−/+) + readout for the given font target.
        let fontRow (target : ChartFont.ChartFontTarget) (bump : float -> unit) : StackPanel =
            let minus = Button(Name = ChartWindowIds.fontMinus, Content = "A−")
            minus.Click.Add(fun _ -> bump -1.0; applyStyle (); rebuildProperties ())
            let plus = Button(Name = ChartWindowIds.fontPlus, Content = "A+", Margin = Thickness(4.0, 0.0, 0.0, 0.0))
            plus.Click.Add(fun _ -> bump 1.0; applyStyle (); rebuildProperties ())
            let sz = TextBlock(Name = ChartWindowIds.fontSize, VerticalAlignment = VerticalAlignment.Center, Margin = Thickness(6.0, 0.0, 0.0, 0.0), Text = sprintf "%g pt" (ChartFont.sizeOf target style.font))
            row [ smallLabel "Font:"; minus; plus; sz ]

        let axisPanel (isX : bool) : Control list =
            let axis = if isX then style.xAxis else style.yAxis
            let autoBox = CheckBox(Name = ChartWindowIds.axisAuto, Content = "Auto (fit data)", IsChecked = axis.auto)
            autoBox.IsCheckedChanged.Add(fun _ ->
                style <- ChartStyle.setAxisAuto isX (autoBox.IsChecked.GetValueOrDefault true) style
                applyStyle (); rebuildProperties ())
            let minF = numberField ChartWindowIds.axisMin axis.min (not axis.auto) (fun v -> style <- ChartStyle.setAxisMin isX v style; applyStyle (); rebuildProperties ())
            let maxF = numberField ChartWindowIds.axisMax axis.max (not axis.auto) (fun v -> style <- ChartStyle.setAxisMax isX v style; applyStyle (); rebuildProperties ())
            let fmtBox = ComboBox(Name = ChartWindowIds.axisFormat)
            for f in [ ChartStyle.GeneralFormat; ChartStyle.FixedFormat 2; ChartStyle.ScientificFormat 2 ] do
                fmtBox.Items.Add(ComboBoxItem(Content = f.label)) |> ignore
            fmtBox.SelectedIndex <- (match axis.format with ChartStyle.GeneralFormat -> 0 | ChartStyle.FixedFormat _ -> 1 | ChartStyle.ScientificFormat _ -> 2)
            fmtBox.SelectionChanged.Add(fun _ ->
                let d = axis.format.decimals
                let fmt =
                    match fmtBox.SelectedIndex with
                    | 1 -> ChartStyle.FixedFormat (if d = 0 then 2 else d)
                    | 2 -> ChartStyle.ScientificFormat (if d = 0 then 2 else d)
                    | _ -> ChartStyle.GeneralFormat
                style <- ChartStyle.setAxisFormat isX fmt style; applyStyle (); rebuildProperties ())
            let decEnabled = (match axis.format with ChartStyle.GeneralFormat -> false | _ -> true)
            let decMinus = Button(Name = ChartWindowIds.axisDecimalsMinus, Content = "−", IsEnabled = decEnabled)
            decMinus.Click.Add(fun _ -> style <- ChartStyle.bumpAxisDecimals isX -1 style; applyStyle (); rebuildProperties ())
            let decPlus = Button(Name = ChartWindowIds.axisDecimalsPlus, Content = "+", IsEnabled = decEnabled, Margin = Thickness(4.0, 0.0, 0.0, 0.0))
            decPlus.Click.Add(fun _ -> style <- ChartStyle.bumpAxisDecimals isX 1 style; applyStyle (); rebuildProperties ())
            let decReadout = TextBlock(Name = ChartWindowIds.axisDecimals, VerticalAlignment = VerticalAlignment.Center, Margin = Thickness(6.0, 0.0, 0.0, 0.0), Text = sprintf "%d digits" axis.format.decimals)
            let bumpAxisFont (delta : float) : unit =
                style <- ChartStyle.bumpFont ChartFont.AxisLabels delta (ChartStyle.bumpFont ChartFont.TickLabels delta style)
            [
                TextBlock(Text = (if isX then "X axis" else "Y axis"), FontWeight = FontWeight.Bold, Margin = Thickness(0.0, 0.0, 0.0, 6.0)) :> Control
                autoBox :> Control
                row [ smallLabel "Min:"; (minF :> Control) ] :> Control
                row [ smallLabel "Max:"; (maxF :> Control) ] :> Control
                row [ smallLabel "Format:"; (fmtBox :> Control) ] :> Control
                row [ smallLabel "Digits:"; (decMinus :> Control); (decPlus :> Control); (decReadout :> Control) ] :> Control
                fontRow ChartFont.TickLabels bumpAxisFont :> Control
            ]

        let legendPanel () : Control list =
            let visBox = CheckBox(Name = ChartWindowIds.legendVisible, Content = "Show legend", IsChecked = style.legend.visible)
            visBox.IsCheckedChanged.Add(fun _ -> style <- ChartStyle.setLegendVisible (visBox.IsChecked.GetValueOrDefault true) style; applyStyle (); rebuildProperties ())
            let placeBox = ComboBox(Name = ChartWindowIds.legendPlacement)
            for p in ChartStyle.allPlacements do placeBox.Items.Add(ComboBoxItem(Content = p.label)) |> ignore
            placeBox.SelectedIndex <- List.findIndex (fun p -> p = style.legend.placement) ChartStyle.allPlacements
            placeBox.SelectionChanged.Add(fun _ ->
                let i = placeBox.SelectedIndex
                if i >= 0 && i < List.length ChartStyle.allPlacements then
                    style <- ChartStyle.setLegendPlacement (List.item i ChartStyle.allPlacements) style; applyStyle ())
            [
                TextBlock(Text = "Legend", FontWeight = FontWeight.Bold, Margin = Thickness(0.0, 0.0, 0.0, 6.0)) :> Control
                visBox :> Control
                row [ smallLabel "Placement:"; (placeBox :> Control) ] :> Control
                fontRow ChartFont.Legend (fun d -> style <- ChartStyle.bumpFont ChartFont.Legend d style) :> Control
            ]

        let seriesPanel (i : int) : Control list =
            let st = ChartStyle.seriesStyleOf i style
            let visBox = CheckBox(Name = ChartWindowIds.seriesVisible, Content = "Show line", IsChecked = st.visible)
            visBox.IsCheckedChanged.Add(fun _ -> style <- ChartStyle.setSeriesVisible i (visBox.IsChecked.GetValueOrDefault true) style; applyStyle (); rebuildProperties ())
            let thickMinus = Button(Name = ChartWindowIds.seriesThicknessMinus, Content = "−")
            thickMinus.Click.Add(fun _ -> style <- ChartStyle.bumpSeriesThickness i -0.5 style; applyStyle (); rebuildProperties ())
            let thickPlus = Button(Name = ChartWindowIds.seriesThicknessPlus, Content = "+", Margin = Thickness(4.0, 0.0, 0.0, 0.0))
            thickPlus.Click.Add(fun _ -> style <- ChartStyle.bumpSeriesThickness i 0.5 style; applyStyle (); rebuildProperties ())
            let thickReadout = TextBlock(Name = ChartWindowIds.seriesThickness, VerticalAlignment = VerticalAlignment.Center, Margin = Thickness(6.0, 0.0, 0.0, 0.0), Text = sprintf "%g px" st.thickness)
            let colorBox = ComboBox(Name = ChartWindowIds.seriesColor)
            for c in ChartStyle.colorChoices do colorBox.Items.Add(ComboBoxItem(Content = c)) |> ignore
            colorBox.SelectedIndex <- (match List.tryFindIndex (fun c -> c = st.colorHex) ChartStyle.colorChoices with Some idx -> idx | None -> 0)
            colorBox.SelectionChanged.Add(fun _ ->
                let idx = colorBox.SelectedIndex
                if idx >= 0 && idx < List.length ChartStyle.colorChoices then
                    style <- ChartStyle.setSeriesColor i (List.item idx ChartStyle.colorChoices) style; applyStyle ())
            let dotsBox = CheckBox(Name = ChartWindowIds.seriesMarkers, Content = "Show dots", IsChecked = st.showMarkers)
            dotsBox.IsCheckedChanged.Add(fun _ -> style <- ChartStyle.setSeriesMarkers i (dotsBox.IsChecked.GetValueOrDefault false) style; applyStyle (); rebuildProperties ())
            [
                TextBlock(Text = sprintf "Line: %s" (seriesName i), FontWeight = FontWeight.Bold, Margin = Thickness(0.0, 0.0, 0.0, 6.0)) :> Control
                visBox :> Control
                row [ smallLabel "Thickness:"; (thickMinus :> Control); (thickPlus :> Control); (thickReadout :> Control) ] :> Control
                row [ smallLabel "Colour:"; (colorBox :> Control) ] :> Control
                dotsBox :> Control
            ]

        // The element picker (Header / X axis / Y axis / Legend / each series).
        let elementOptions = ChartStyle.elements seriesCount
        let elementSelector = ComboBox(Name = ChartWindowIds.elementSelector, Margin = Thickness(0.0, 0.0, 0.0, 8.0), HorizontalAlignment = HorizontalAlignment.Stretch)
        for e in elementOptions do elementSelector.Items.Add(ComboBoxItem(Content = ChartStyle.elementLabel seriesName e)) |> ignore

        rebuildProperties <-
            fun () ->
                propertiesHost.Children.Clear()
                let controls =
                    match style.selected with
                    | ChartStyle.Header -> [ TextBlock(Text = "Header", FontWeight = FontWeight.Bold, Margin = Thickness(0.0, 0.0, 0.0, 6.0)) :> Control; fontRow ChartFont.Title (fun d -> style <- ChartStyle.bumpFont ChartFont.Title d style) :> Control ]
                    | ChartStyle.XAxis -> axisPanel true
                    | ChartStyle.YAxis -> axisPanel false
                    | ChartStyle.Legend -> legendPanel ()
                    | ChartStyle.Series i -> seriesPanel i
                controls |> List.iter (fun c -> propertiesHost.Children.Add c)

        elementSelector.SelectedIndex <- (match List.tryFindIndex (fun e -> e = style.selected) elementOptions with Some i -> i | None -> 0)
        elementSelector.SelectionChanged.Add(fun _ ->
            let i = elementSelector.SelectedIndex
            if i >= 0 && i < List.length elementOptions then
                style <- ChartStyle.selectElement (List.item i elementOptions) style
                rebuildProperties ())
        rebuildProperties ()

        let rightPanel =
            let sp = StackPanel(Orientation = Orientation.Vertical, Margin = Thickness(8.0), Width = 250.0)
            sp.Children.Add(TextBlock(Text = "Format:", FontWeight = FontWeight.Bold, Margin = Thickness(0.0, 0.0, 0.0, 4.0)))
            sp.Children.Add elementSelector
            sp.Children.Add propertiesHost
            sp

        // ----------------------------------------------------------------------------------------------
        // The bottom toolbar: polar toggle (angular charts only), gridline toggles, PNG + CSV export.
        // ----------------------------------------------------------------------------------------------

        let toolbar = StackPanel(Orientation = Orientation.Horizontal, Margin = Thickness(8.0))

        if chart.angular then
            let polarBtn = Button(Name = ChartWindowIds.polarToggle, Content = "Polar view", Margin = Thickness(0.0, 0.0, 16.0, 0.0))
            polarBtn.Click.Add(fun _ ->
                polar <- not polar
                polarBtn.Content <- (if polar then "XY view" else "Polar view")
                rebuildPlot ())
            toolbar.Children.Add polarBtn

        let mutable majorOn = true
        let mutable minorOn = false
        let applyGrid () : unit =
            plot.Grid.MajorLineWidth <- (if majorOn then 1.0f else 0.0f)
            plot.Grid.MinorLineWidth <- (if minorOn then 1.0f else 0.0f)
            plot.Grid.IsVisible <- majorOn || minorOn
            ava.Refresh()

        let majorGrid = CheckBox(Name = ChartWindowIds.majorGrid, Content = "Major grid", IsChecked = true)
        majorGrid.IsCheckedChanged.Add(fun _ -> majorOn <- majorGrid.IsChecked.GetValueOrDefault true; applyGrid ())
        let minorGrid = CheckBox(Name = ChartWindowIds.minorGrid, Content = "Minor grid", IsChecked = false, Margin = Thickness(8.0, 0.0, 0.0, 0.0))
        minorGrid.IsCheckedChanged.Add(fun _ -> minorOn <- minorGrid.IsChecked.GetValueOrDefault false; applyGrid ())

        let exportPng = Button(Name = ChartWindowIds.exportPng, Content = "Export PNG", Margin = Thickness(16.0, 0.0, 0.0, 0.0))
        exportPng.Click.Add(fun _ ->
            try plot.SavePng(Path.Combine(Path.GetTempPath(), "experiment-chart.png"), 1200, 800) |> ignore with _ -> ())
        let exportCsv = Button(Name = ChartWindowIds.exportCsv, Content = "Export CSV", Margin = Thickness(8.0, 0.0, 0.0, 0.0))
        exportCsv.Click.Add(fun _ ->
            try File.WriteAllText(Path.Combine(Path.GetTempPath(), "experiment-chart.csv"), toCsv chart) with _ -> ())

        toolbar.Children.Add majorGrid
        toolbar.Children.Add minorGrid
        toolbar.Children.Add exportPng
        toolbar.Children.Add exportCsv

        let descriptionBox =
            TextBox(
                Name = ChartWindowIds.description,
                Text = chart.description,
                IsReadOnly = true,
                AcceptsReturn = true,
                TextWrapping = TextWrapping.Wrap,
                Margin = Thickness(8.0, 0.0, 8.0, 8.0))

        let bottom = StackPanel(Orientation = Orientation.Vertical)
        bottom.Children.Add toolbar
        bottom.Children.Add descriptionBox
        DockPanel.SetDock(bottom, Dock.Bottom)
        DockPanel.SetDock(rightPanel, Dock.Right)

        let root = DockPanel()
        root.Children.Add bottom
        root.Children.Add rightPanel
        root.Children.Add ava
        this.Content <- root

        applyGrid ()
        ava.Refresh()
