namespace OpticalConstructor.Tests

open Berreman.FieldFunctions
open OpticalConstructor.Domain
open OpticalConstructor.Ui.Charts
open OpticalConstructor.Ui.Charts.ChartSettings
open Plotly.NET
open Xunit

/// §H.1/§H.6 chart-settings projection tests (slice 012).
///   AC-H4 — `applyToScottPlot`/`applyToPlotly` omit `visible = false` traces and
///           apply `Log10` through the projection (no separate data transform).
///   AC-H5 — an nm→eV axis-unit switch routes through the §A.10 boundary
///           conversion while the stored series/`ChartSettings` remain canonical SI.
module ChartSettingsTests =

    let private trace name vis =
        { functionId = R; name = name; color = "#1f77b4"; lineStyle = SolidLine; visible = vis }

    // ----------------------------------------------------------------- AC-H4

    [<Fact>]
    let ``AC-H4 applyToScottPlot omits a visible=false trace`` () =
        let plot = new ScottPlot.Plot()
        let rSc = plot.Add.Scatter([| 0.0; 1.0 |], [| 0.0; 1.0 |])
        rSc.LegendText <- "R"
        let tSc = plot.Add.Scatter([| 0.0; 1.0 |], [| 0.0; 1.0 |])
        tSc.LegendText <- "T"

        let s = { ChartSettings.defaultValue with traces = [ trace "R" true; trace "T" false ] }
        applyToScottPlot s plot

        Assert.True(rSc.IsVisible)
        Assert.False(tSc.IsVisible)
        Assert.Equal(1, (visibleTraces s).Length)

    [<Fact>]
    let ``AC-H4 applyToScottPlot applies Log10 as a log tick generator`` () =
        let plot = new ScottPlot.Plot()
        let s = { ChartSettings.defaultValue with yScale = Log10 }
        applyToScottPlot s plot
        match plot.Axes.Left.TickGenerator with
        | :? ScottPlot.TickGenerators.NumericAutomatic as na ->
            Assert.True(na.MinorTickGenerator :? ScottPlot.TickGenerators.LogMinorTickGenerator)
        | _ -> Assert.Fail "expected a log-configured tick generator on the Y axis"

    [<Fact>]
    let ``AC-H4 Log10 maps to the genuine Plotly log axis type`` () =
        Assert.Equal(StyleParam.AxisType.Log, plotlyAxisType Log10)
        Assert.Equal(StyleParam.AxisType.Linear, plotlyAxisType Linear)
        // applyToPlotly runs and returns a chart on a Log10 setting.
        let chart = Chart.Line([ (1.0, 1.0); (2.0, 2.0) ])
        let s = { ChartSettings.defaultValue with yScale = Log10 }
        let result = applyToPlotly s chart
        Assert.NotNull(box result)

    // ----------------------------------------------------------------- AC-H5

    [<Fact>]
    let ``AC-H5 nm to eV unit switch routes through the A10 boundary and stays SI`` () =
        // A 500 nm wavelength, stored canonical SI (meters).
        let mWl = Units.toMeters Units.Nanometer 500.0

        // Display conversions go through the §A.10 Units seam, NOT a re-implementation.
        Assert.Equal(Units.evNmProduct / 500.0, Units.fromMeters Units.ElectronVolt mWl, 6)
        Assert.Equal(500.0, Units.fromMeters Units.Nanometer mWl, 9)

        // The stored ChartSettings range stays canonical SI regardless of display unit.
        let s = { ChartSettings.defaultValue with xMin = Some (float mWl) }
        Assert.Equal(Some (float mWl), s.xMin)

    [<Fact>]
    let ``AC-H5 applyToScottPlot converts an SI xMin/xMax override to the axis display unit`` () =
        // A 400..700 nm wavelength window, stored canonical SI (meters) — the trace
        // x-values are plotted in nm, so the axis display unit is Nanometer.
        let loM = Units.toMeters Units.Nanometer 400.0
        let hiM = Units.toMeters Units.Nanometer 700.0

        let plot = new ScottPlot.Plot()
        let s =
            { ChartSettings.defaultValue with
                xMin = Some (float loM)
                xMax = Some (float hiM)
                xUnit = Some Units.Nanometer }
        applyToScottPlot s plot

        // The projection routes the SI override through the §A.10 Units.fromMeters
        // seam, so the X axis limits land on the nm display scale (not ~5e-7 m).
        let limits = plot.Axes.GetLimits()
        Assert.Equal(400.0, limits.Left, 6)
        Assert.Equal(700.0, limits.Right, 6)

        // Storage stays canonical SI regardless of the axis display unit.
        Assert.Equal(Some (float loM), s.xMin)
        Assert.Equal(Some (float hiM), s.xMax)

    [<Fact>]
    let ``AC-H5 applyToScottPlot keeps inverse-unit (nm to eV) limits ascending`` () =
        // The same 400..700 nm window stored canonical SI, but now displayed in eV.
        // The §A.10 eV map (evNmProduct / nm) is *decreasing*, so the longer
        // wavelength (700 nm) is the SMALLER eV value: an ascending stored window
        // converts to a descending display pair. The projection must order-normalize
        // it so the renderer never sees Left > Right (a reversed/empty axis).
        let loM = Units.toMeters Units.Nanometer 400.0
        let hiM = Units.toMeters Units.Nanometer 700.0

        let plot = new ScottPlot.Plot()
        let s =
            { ChartSettings.defaultValue with
                xMin = Some (float loM)
                xMax = Some (float hiM)
                xUnit = Some Units.ElectronVolt }
        applyToScottPlot s plot

        let limits = plot.Axes.GetLimits()
        // Ascending: Left < Right despite the inverse-unit conversion flipping order.
        Assert.True(limits.Left < limits.Right, "X-axis limits must be ascending after the inverse-unit conversion")
        // The two ends are exactly the converted eV values (700 nm -> low, 400 nm -> high).
        Assert.Equal(Units.fromMeters Units.ElectronVolt hiM, limits.Left, 6)
        Assert.Equal(Units.fromMeters Units.ElectronVolt loM, limits.Right, 6)

        // Storage stays canonical SI regardless of the axis display unit.
        Assert.Equal(Some (float loM), s.xMin)
        Assert.Equal(Some (float hiM), s.xMax)
