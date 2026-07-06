namespace OpticalConstructor.Ui.Tests

open System
open Xunit
open OpticalConstructor.Controls
open OpticalConstructor.Controls.ExperimentChart
open OpticalConstructor.Controls.ChartStyle

/// Spec 0027 (030) — the pure chart-window STYLE model: the x-range is initialized to the DATA (not a
/// default 0…1000); each chart part (header / axes / legend / series) is selectable and independently
/// editable (axis range + auto + number format; legend placement + visibility; per-series thickness /
/// colour / markers); and an angular chart converts to polar. These tests exercise that logic without a
/// native window.
module ChartStyleTests =

    /// A wavelength-style chart: x 400…800, y 0…1, one series.
    let private wlChart : ExperimentChart =
        { ExperimentChart.empty with
            series = [ { name = "Intensity"; points = [ 400.0, 0.2; 600.0, 0.9; 800.0, 0.4 ] } ]
            xLabel = "Wavelength (nm)"
            angular = false }

    // ============================ data-fit bounds (the reported 0…1000 bug) ============================

    /// Pair every series with the LEFT axis (the pre-018 single-axis reading of a chart).
    let private allLeft (series : ChartSeries list) : (ChartSeries * AxisSide) list =
        series |> List.map (fun s -> s, LeftAxis)

    [<Fact>]
    let ``dataBounds fits the data range (not a default 0..1000) with a little padding`` () =
        let b = dataBounds (allLeft wlChart.series)
        let xlo, xhi = b.x
        let ylo, yhi = b.yLeft
        // The x-range brackets 400…800 tightly (padded < 5 % of the 400-wide span, so nowhere near 0 or 1000).
        Assert.True(xlo > 300.0 && xlo < 400.0, sprintf "xlo=%g" xlo)
        Assert.True(xhi > 800.0 && xhi < 900.0, sprintf "xhi=%g" xhi)
        Assert.True(ylo < 0.2 && yhi > 0.9)

    [<Fact>]
    let ``dataBounds of an empty chart is a finite unit box on every axis`` () =
        let b = dataBounds []
        Assert.Equal((0.0, 1.0), b.x)
        Assert.Equal((0.0, 1.0), b.yLeft)
        Assert.Equal((0.0, 1.0), b.yRight)

    [<Fact>]
    let ``dataBounds computes INDEPENDENT y bounds per axis side`` () =
        // The acceptance shape (spec 0033/018): one left-axis series (n-like, 1.5…2.5) and one right-axis
        // series (k-like, 0.01…0.09) — neither side's bounds may be stretched by the other's data.
        let n = { name = "n"; points = [ 400.0, 1.5; 800.0, 2.5 ] }
        let k = { name = "k"; points = [ 400.0, 0.01; 800.0, 0.09 ] }
        let b = dataBounds [ n, LeftAxis; k, RightAxis ]
        let xlo, xhi = b.x
        Assert.True(xlo < 400.0 && xhi > 800.0)             // x spans BOTH series
        let llo, lhi = b.yLeft
        Assert.True(llo > 1.0 && llo < 1.5, sprintf "left lo=%g" llo)   // brackets only 1.5…2.5
        Assert.True(lhi > 2.5 && lhi < 3.0, sprintf "left hi=%g" lhi)
        let rlo, rhi = b.yRight
        Assert.True(rlo < 0.01 && rlo > -0.1, sprintf "right lo=%g" rlo) // brackets only 0.01…0.09
        Assert.True(rhi > 0.09 && rhi < 0.2, sprintf "right hi=%g" rhi)  // nowhere near the left 2.5

    [<Fact>]
    let ``the default state fits the axes to the data per side and auto is on`` () =
        let s = defaultState wlChart
        Assert.True(s.xAxis.auto)
        Assert.True(s.yAxisLeft.auto)
        Assert.True(s.yAxisRight.auto)
        // The stored min/max are the data bounds (so toggling Auto off starts from the fitted view).
        let b = dataBounds (allLeft wlChart.series)
        Assert.Equal(fst b.x, s.xAxis.min)
        Assert.Equal(snd b.x, s.xAxis.max)
        Assert.Equal(fst b.yLeft, s.yAxisLeft.min)
        Assert.Equal(snd b.yLeft, s.yAxisLeft.max)
        // Every series starts on the left axis, so the right axis seeds to the unit fallback —
        // INDEPENDENT of the left data (not stretched to 0.2…0.9).
        Assert.Equal((0.0, 1.0), (s.yAxisRight.min, s.yAxisRight.max))

    // ============================ number format (digits / format) ============================

    [<Fact>]
    let ``formatValue honours General / Fixed / Scientific`` () =
        Assert.Equal("400", formatValue GeneralFormat 400.0)
        Assert.Equal("0.5", formatValue GeneralFormat 0.5)
        Assert.Equal("400.00", formatValue (FixedFormat 2) 400.0)
        Assert.Equal("3.142", formatValue (FixedFormat 3) 3.14159)
        Assert.StartsWith("1.5", formatValue (ScientificFormat 1) 1.5e3)   // "1.5E+003"-ish
        Assert.Contains("E", formatValue (ScientificFormat 1) 1.5e3)

    [<Fact>]
    let ``withDecimals clamps and is a no-op for General`` () =
        Assert.Equal(GeneralFormat, withDecimals 4 GeneralFormat)
        Assert.Equal(FixedFormat maxDecimals, withDecimals 999 (FixedFormat 2))
        Assert.Equal(FixedFormat minDecimals, withDecimals -5 (FixedFormat 2))

    [<Fact>]
    let ``bumpAxisDecimals steps the digit count and never below zero`` () =
        let s = { defaultState wlChart with xAxis = { (defaultState wlChart).xAxis with format = FixedFormat 2 } }
        let up = bumpAxisDecimals AxisX 1 s
        Assert.Equal(3, up.xAxis.format.decimals)
        let downLots = List.fold (fun st _ -> bumpAxisDecimals AxisX -1 st) s [ 1 .. 10 ]
        Assert.Equal(0, downLots.xAxis.format.decimals)

    // ============================ axis range editing ============================

    [<Fact>]
    let ``setting an axis min or max turns Auto off`` () =
        let s = defaultState wlChart
        Assert.True(s.xAxis.auto)
        let s1 = setAxisMin AxisX 450.0 s
        Assert.False(s1.xAxis.auto)
        Assert.Equal(450.0, s1.xAxis.min)
        let s2 = setAxisAuto AxisX true s1
        Assert.True(s2.xAxis.auto)

    [<Fact>]
    let ``the tri-state axis mutators edit X, left Y and right Y independently`` () =
        let s =
            defaultState wlChart
            |> setAxisMin AxisX 450.0
            |> setAxisMin (AxisY LeftAxis) 0.1
            |> setAxisMax (AxisY RightAxis) 9.0
            |> setAxisFormat (AxisY RightAxis) (FixedFormat 1)
        Assert.Equal(450.0, s.xAxis.min)
        Assert.Equal(0.1, s.yAxisLeft.min)
        Assert.Equal(9.0, s.yAxisRight.max)
        // Each edit landed on ITS axis only: the untouched partners keep the General format,
        // and only the edited axes dropped out of auto.
        Assert.Equal(GeneralFormat, s.xAxis.format)
        Assert.Equal(GeneralFormat, s.yAxisLeft.format)
        Assert.Equal(FixedFormat 1, s.yAxisRight.format)
        Assert.False(s.xAxis.auto)
        Assert.False(s.yAxisLeft.auto)
        Assert.False(s.yAxisRight.auto)

    // ============================ legend ============================

    [<Fact>]
    let ``legend visibility and placement round-trip`` () =
        let s = defaultState wlChart |> setLegendVisible false |> setLegendPlacement LowerLeft
        Assert.False(s.legend.visible)
        Assert.Equal(LowerLeft, s.legend.placement)
        Assert.Equal(9, List.length allPlacements)

    // ============================ series look ============================

    let private twoSeries : ExperimentChart =
        { ExperimentChart.empty with
            series =
                [ { name = "T"; points = [ 0.0, 1.0; 90.0, 0.5 ] }
                  { name = "R"; points = [ 0.0, 0.1; 90.0, 0.4 ] } ]
            angular = true }

    [<Fact>]
    let ``a series' thickness, colour, markers and visibility are independently editable`` () =
        let s0 = defaultState twoSeries
        let s =
            s0
            |> bumpSeriesThickness 1 0.5
            |> setSeriesColor 1 "#C81E32"
            |> setSeriesMarkers 1 true
            |> setSeriesVisible 0 false
        let a = seriesStyleOf 0 s
        let b = seriesStyleOf 1 s
        Assert.False(a.visible)          // only series 0 hidden
        Assert.True(b.visible)
        Assert.Equal(2.0, b.thickness)   // 1.5 + 0.5
        Assert.Equal("#C81E32", b.colorHex)
        Assert.True(b.showMarkers)
        Assert.False(a.showMarkers)      // series 0 untouched

    [<Fact>]
    let ``series thickness is clamped to the allowed band`` () =
        let s0 = defaultState twoSeries
        let thick = List.fold (fun st _ -> bumpSeriesThickness 0 1.0 st) s0 [ 1 .. 50 ]
        Assert.Equal(maxThickness, (seriesStyleOf 0 thick).thickness)
        let thin = List.fold (fun st _ -> bumpSeriesThickness 0 -1.0 st) s0 [ 1 .. 50 ]
        Assert.Equal(minThickness, (seriesStyleOf 0 thin).thickness)

    // ============================ axis-side assignment (spec 0033/018) ============================

    [<Fact>]
    let ``series default to the left axis and setSeriesAxisSide reassigns one side only`` () =
        let s0 = defaultState twoSeries
        Assert.Equal(LeftAxis, (seriesStyleOf 0 s0).axisSide)
        Assert.Equal(LeftAxis, (seriesStyleOf 1 s0).axisSide)
        let s = setSeriesAxisSide 1 RightAxis s0
        Assert.Equal(LeftAxis, (seriesStyleOf 0 s).axisSide)
        Assert.Equal(RightAxis, (seriesStyleOf 1 s).axisSide)

    [<Fact>]
    let ``toCsv is unchanged by the dual-axis model (it reads only the chart data)`` () =
        // `toCsv` takes the chart alone — no style, hence no axis-side influence; its output is pinned so
        // the 018 model growth provably left the CSV export byte-identical (spec 0033/018 acceptance).
        Assert.Equal("x,T,R\n0,1,0.1\n90,0.5,0.4", toCsv twoSeries)

    // ============================ selection + elements ============================

    [<Fact>]
    let ``elements are the five fixed parts plus one per series`` () =
        Assert.Equal<ChartElement list>(
            [ Header; XAxis; YAxis LeftAxis; YAxis RightAxis; Legend; Series 0; Series 1 ], elements 2)
        Assert.Equal("Line: R", elementLabel (fun i -> if i = 0 then "T" else "R") (Series 1))
        Assert.Equal("X axis", elementLabel (fun _ -> "") XAxis)
        Assert.Equal("Y axis (left)", elementLabel (fun _ -> "") (YAxis LeftAxis))
        Assert.Equal("Y axis (right)", elementLabel (fun _ -> "") (YAxis RightAxis))

    [<Fact>]
    let ``selectElement changes the selection only`` () =
        let s = defaultState twoSeries |> selectElement Legend
        Assert.Equal(Legend, s.selected)

    [<Fact>]
    let ``bumpFont resizes the named target only`` () =
        let s0 = defaultState twoSeries
        let before = ChartFont.sizeOf ChartFont.Title s0.font
        let s = bumpFont ChartFont.Title 2.0 s0
        Assert.Equal(before + 2.0, ChartFont.sizeOf ChartFont.Title s.font)
        Assert.Equal(ChartFont.sizeOf ChartFont.Legend s0.font, ChartFont.sizeOf ChartFont.Legend s.font)

    // ============================ polar ============================

    [<Fact>]
    let ``polarXY maps angle+radius onto the cartesian circle`` () =
        let close (a : float) (b : float) = abs (a - b) < 1e-9
        let x0, y0 = polarXY (0.0, 2.0)
        Assert.True(close 2.0 x0 && close 0.0 y0)          // 0°  → (r, 0)
        let x90, y90 = polarXY (90.0, 2.0)
        Assert.True(close 0.0 x90 && close 2.0 y90)        // 90° → (0, r)
        let x180, _ = polarXY (180.0, 2.0)
        Assert.True(close -2.0 x180)                        // 180°→ (−r, ·)

    [<Fact>]
    let ``polarRadius is the largest visible |y| (with a positive fallback)`` () =
        let s = defaultState twoSeries
        Assert.Equal(1.0, polarRadius s twoSeries.series)   // max |y| across both series = 1.0
        // Hiding the series that carries the max shrinks the radius.
        let s2 = setSeriesVisible 0 false s
        Assert.Equal(0.4, polarRadius s2 twoSeries.series)
        Assert.True(polarRadius s [] > 0.0)                 // fallback stays positive
