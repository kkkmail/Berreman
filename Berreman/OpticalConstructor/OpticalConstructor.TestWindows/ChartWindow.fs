namespace OpticalConstructor.TestWindows

open System
open System.IO
open Avalonia
open Avalonia.Controls
open Avalonia.Layout
open Avalonia.Media
open OpticalConstructor.TestWindows.ExperimentChart

/// Spec 0027 (026) Part 3 — the pop-out interactive chart window opened by double-clicking the inline
/// Experiments chart. A plain Avalonia `Window` (built imperatively like `LocalizationErrorWindow`) hosting a
/// `ScottPlot.Avalonia.AvaPlot`: ScottPlot 5 gives zoom/pan + the rendered axes/legend/gridlines for free; on
/// top we add a snapping crosshair that follows the mouse and shows its (x, y), a small toolbar (font-size +/-,
/// major- and minor-gridline toggles, PNG + CSV export), and the chart description beneath the plot. The window
/// is NEVER opened under the headless `ui-smoke` gate (which only renders frames, never double-clicks), so its
/// native ScottPlot rendering cannot break that gate.

/// Stable automation ids for the chart window's controls (CLAUDE.md: centralize ids).
[<RequireQualifiedAccess>]
module ChartWindowIds =
    let plot = "ChartWindowPlot"
    let fontMinus = "ChartWindowFontMinus"
    let fontPlus = "ChartWindowFontPlus"
    let majorGrid = "ChartWindowMajorGrid"
    let minorGrid = "ChartWindowMinorGrid"
    let exportPng = "ChartWindowExportPng"
    let exportCsv = "ChartWindowExportCsv"
    let description = "ChartWindowDescription"

type ChartWindow(chart : ExperimentChart) as this =
    inherit Window()

    // The palette mirrors the inline chart's series colours (blue / orange / green) so the pop-out reads as the
    // same chart at a larger size.
    static let palette : ScottPlot.Color[] =
        [|
            ScottPlot.Color.FromHex("#1E5AC8")
            ScottPlot.Color.FromHex("#DC7814")
            ScottPlot.Color.FromHex("#289646")
        |]

    do
        this.Title <- (if chart.title = "" then "Experiment chart" else chart.title)
        this.Width <- 900.0
        this.Height <- 660.0

        let ava = new ScottPlot.Avalonia.AvaPlot(Name = ChartWindowIds.plot)
        let plot = ava.Plot

        // One Scatter per series; zoom/pan are built into AvaPlot (no custom interaction math).
        let scatters =
            chart.series
            |> List.mapi (fun i s ->
                let xs = s.points |> List.map fst |> List.toArray
                let ys = s.points |> List.map snd |> List.toArray
                let sc = plot.Add.Scatter(xs, ys, System.Nullable palette.[i % palette.Length])
                sc.LegendText <- s.name
                sc)

        plot.XLabel(chart.xLabel)
        plot.YLabel(chart.yLabel)
        plot.Title(this.Title)
        plot.ShowLegend() |> ignore
        plot.Grid.IsVisible <- true

        // The snapping crosshair: a thin cross + a dot + a label that follow the nearest data point of the
        // first series as the mouse moves.
        let crosshair = plot.Add.Crosshair(0.0, 0.0)
        crosshair.IsVisible <- false
        let marker = plot.Add.Marker(0.0, 0.0, ScottPlot.MarkerShape.FilledCircle, 8.0f, System.Nullable ())
        marker.IsVisible <- false
        let readout = plot.Add.Text("", 0.0, 0.0)
        readout.IsVisible <- false

        ava.PointerMoved.Add(fun e ->
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

        // -- The toolbar: font-size +/-, gridline toggles, PNG + CSV export. --
        let mutable axisFontSize = 13.0f

        let setAxisFontSize (size : float32) : unit =
            axisFontSize <- size
            plot.Axes.Title.Label.FontSize <- size + 2.0f
            match plot.Axes.Bottom with
            | :? ScottPlot.AxisPanels.AxisBase as b -> b.Label.FontSize <- size
            | _ -> ()
            match plot.Axes.Left with
            | :? ScottPlot.AxisPanels.AxisBase as l -> l.Label.FontSize <- size
            | _ -> ()
            ava.Refresh()

        setAxisFontSize axisFontSize

        let fontMinus = Button(Name = ChartWindowIds.fontMinus, Content = "Font -")
        fontMinus.Click.Add(fun _ -> setAxisFontSize (max 6.0f (axisFontSize - 1.0f)))
        let fontPlus = Button(Name = ChartWindowIds.fontPlus, Content = "Font +", Margin = Thickness(6.0, 0.0, 0.0, 0.0))
        fontPlus.Click.Add(fun _ -> setAxisFontSize (min 40.0f (axisFontSize + 1.0f)))

        // Track the grid toggles locally (the ScottPlot line-width properties are write-only here), so the
        // overall `Grid.IsVisible` reflects whether EITHER tier is on.
        let mutable majorOn = true
        let mutable minorOn = false
        let applyGrid () : unit =
            plot.Grid.MajorLineWidth <- (if majorOn then 1.0f else 0.0f)
            plot.Grid.MinorLineWidth <- (if minorOn then 1.0f else 0.0f)
            plot.Grid.IsVisible <- majorOn || minorOn
            ava.Refresh()

        let majorGrid = CheckBox(Name = ChartWindowIds.majorGrid, Content = "Major grid", IsChecked = true, Margin = Thickness(16.0, 0.0, 0.0, 0.0))
        majorGrid.IsCheckedChanged.Add(fun _ ->
            majorOn <- majorGrid.IsChecked.GetValueOrDefault true
            applyGrid ())

        let minorGrid = CheckBox(Name = ChartWindowIds.minorGrid, Content = "Minor grid", IsChecked = false, Margin = Thickness(8.0, 0.0, 0.0, 0.0))
        minorGrid.IsCheckedChanged.Add(fun _ ->
            minorOn <- minorGrid.IsChecked.GetValueOrDefault false
            applyGrid ())

        let exportPng = Button(Name = ChartWindowIds.exportPng, Content = "Export PNG", Margin = Thickness(16.0, 0.0, 0.0, 0.0))
        exportPng.Click.Add(fun _ ->
            try
                let path = Path.Combine(Path.GetTempPath(), "experiment-chart.png")
                plot.SavePng(path, 1200, 800) |> ignore
            with _ -> ())

        let exportCsv = Button(Name = ChartWindowIds.exportCsv, Content = "Export CSV", Margin = Thickness(8.0, 0.0, 0.0, 0.0))
        exportCsv.Click.Add(fun _ ->
            try
                let path = Path.Combine(Path.GetTempPath(), "experiment-chart.csv")
                File.WriteAllText(path, toCsv chart)
            with _ -> ())

        let toolbar = StackPanel(Orientation = Orientation.Horizontal, Margin = Thickness(8.0))
        toolbar.Children.Add fontMinus
        toolbar.Children.Add fontPlus
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

        // Layout: the toolbar + description docked at the bottom, the plot filling the rest.
        let bottom = StackPanel(Orientation = Orientation.Vertical)
        bottom.Children.Add toolbar
        bottom.Children.Add descriptionBox
        DockPanel.SetDock(bottom, Dock.Bottom)

        let root = DockPanel()
        root.Children.Add bottom
        root.Children.Add ava
        this.Content <- root

        ava.Refresh()
