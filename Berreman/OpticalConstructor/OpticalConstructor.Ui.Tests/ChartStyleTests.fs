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

    /// A named series over (x, y) points.
    let private curve (name : string) (points : (float * float) list) : ExperimentControls.ChartSeries =
        {
            ExperimentControls.ChartSeries.name = ControlCaption name
            points = points |> List.map (fun (x, y) -> { ExperimentControls.ChartPoint.x = x; y = y })
        }

    /// A wavelength-style chart: x 400…800, y 0…1, one series.
    let private wlChart : ExperimentChart =
        { ExperimentChart.emptyExperimentChart with
            series = [ curve "Intensity" [ 400.0, 0.2; 600.0, 0.9; 800.0, 0.4 ] ]
            xLabel = ControlCaption "Wavelength (nm)"
            xAxisKind = NonAngularXAxis }

    /// The unit box every empty axis falls back to.
    let private unitBox : ChartAxisRange = { lower = ChartAxisValue 0.0; upper = ChartAxisValue 1.0 }

    // ============================ data-fit bounds (the reported 0…1000 bug) ============================

    /// Pair every series with the LEFT axis (the pre-018 single-axis reading of a chart).
    let private allLeft (series : ExperimentControls.ChartSeries list) : (ExperimentControls.ChartSeries * AxisSide) list =
        series |> List.map (fun s -> s, LeftAxis)

    [<Fact>]
    let ``dataBounds fits the data range (not a default 0..1000) with a little padding`` () =
        let b = dataBounds (allLeft wlChart.series)
        let xlo, xhi = b.x.lower.value, b.x.upper.value
        let ylo, yhi = b.yLeft.lower.value, b.yLeft.upper.value
        // The x-range brackets 400…800 tightly (padded < 5 % of the 400-wide span, so nowhere near 0 or 1000).
        Assert.True(xlo > 300.0 && xlo < 400.0, $"xlo=%g{xlo}")
        Assert.True(xhi > 800.0 && xhi < 900.0, $"xhi=%g{xhi}")
        Assert.True(ylo < 0.2 && yhi > 0.9)

    [<Fact>]
    let ``dataBounds of an empty chart is a finite unit box on every axis`` () =
        let b = dataBounds []
        Assert.Equal(unitBox, b.x)
        Assert.Equal(unitBox, b.yLeft)
        Assert.Equal(unitBox, b.yRight)

    [<Fact>]
    let ``dataBounds computes INDEPENDENT y bounds per axis side`` () =
        // The acceptance shape (spec 0033/018): one left-axis series (n-like, 1.5…2.5) and one right-axis
        // series (k-like, 0.01…0.09) — neither side's bounds may be stretched by the other's data.
        let n = curve "n" [ 400.0, 1.5; 800.0, 2.5 ]
        let k = curve "k" [ 400.0, 0.01; 800.0, 0.09 ]
        let b = dataBounds [ n, LeftAxis; k, RightAxis ]
        let xlo, xhi = b.x.lower.value, b.x.upper.value
        Assert.True(xlo < 400.0 && xhi > 800.0)             // x spans BOTH series
        let llo, lhi = b.yLeft.lower.value, b.yLeft.upper.value
        Assert.True(llo > 1.0 && llo < 1.5, $"left lo=%g{llo}")   // brackets only 1.5…2.5
        Assert.True(lhi > 2.5 && lhi < 3.0, $"left hi=%g{lhi}")
        let rlo, rhi = b.yRight.lower.value, b.yRight.upper.value
        Assert.True(rlo < 0.01 && rlo > -0.1, $"right lo=%g{rlo}") // brackets only 0.01…0.09
        Assert.True(rhi > 0.09 && rhi < 0.2, $"right hi=%g{rhi}")  // nowhere near the left 2.5

    [<Fact>]
    let ``the default state fits the axes to the data per side and auto is on`` () =
        let s = defaultChartStyleState wlChart
        Assert.Equal(AutoFitAxis, s.xAxis.fit)
        Assert.Equal(AutoFitAxis, s.yAxisLeft.fit)
        Assert.Equal(AutoFitAxis, s.yAxisRight.fit)
        // The stored min/max are the data bounds (so toggling Auto off starts from the fitted view).
        let b = dataBounds (allLeft wlChart.series)
        Assert.Equal(b.x.lower, s.xAxis.min)
        Assert.Equal(b.x.upper, s.xAxis.max)
        Assert.Equal(b.yLeft.lower, s.yAxisLeft.min)
        Assert.Equal(b.yLeft.upper, s.yAxisLeft.max)
        // Every series starts on the left axis, so the right axis seeds to the unit fallback —
        // INDEPENDENT of the left data (not stretched to 0.2…0.9).
        Assert.Equal(unitBox, { lower = s.yAxisRight.min; upper = s.yAxisRight.max })

    // ============================ number format (digits / format) ============================

    [<Fact>]
    let ``formatValue honours General / Fixed / Scientific`` () =
        let format (fmt : NumberFormat) (v : float) : string = (formatValue fmt (ChartAxisValue v)).value
        Assert.Equal("400", format GeneralFormat 400.0)
        Assert.Equal("0.5", format GeneralFormat 0.5)
        Assert.Equal("400.00", format (FixedFormat (DecimalDigits 2)) 400.0)
        Assert.Equal("3.142", format (FixedFormat (DecimalDigits 3)) 3.14159)
        Assert.StartsWith("1.5", format (ScientificFormat (DecimalDigits 1)) 1.5e3)   // "1.5E+003"-ish
        Assert.Contains("E", format (ScientificFormat (DecimalDigits 1)) 1.5e3)

    [<Fact>]
    let ``withDecimals clamps and is a no-op for General`` () =
        Assert.Equal(GeneralFormat, withDecimals (DecimalDigits 4) GeneralFormat)
        Assert.Equal(FixedFormat maxDecimals, withDecimals (DecimalDigits 999) (FixedFormat (DecimalDigits 2)))
        Assert.Equal(FixedFormat minDecimals, withDecimals (DecimalDigits -5) (FixedFormat (DecimalDigits 2)))

    [<Fact>]
    let ``bumpAxisDecimals steps the digit count and never below zero`` () =
        let s = { defaultChartStyleState wlChart with xAxis = { (defaultChartStyleState wlChart).xAxis with format = FixedFormat (DecimalDigits 2) } }
        let up = bumpAxisDecimals ChartXAxis StepIncrease s
        Assert.Equal(3, up.xAxis.format.decimals.value)
        let downLots = List.fold (fun st _ -> bumpAxisDecimals ChartXAxis StepDecrease st) s [ 1 .. 10 ]
        Assert.Equal(0, downLots.xAxis.format.decimals.value)

    // ============================ axis range editing ============================

    [<Fact>]
    let ``setting an axis min or max turns Auto off`` () =
        let s = defaultChartStyleState wlChart
        Assert.Equal(AutoFitAxis, s.xAxis.fit)
        let s1 = setAxisMin ChartXAxis (ChartAxisValue 450.0) s
        Assert.Equal(PinnedAxis, s1.xAxis.fit)
        Assert.Equal(450.0, s1.xAxis.min.value)
        let s2 = setAxisFit ChartXAxis AutoFitAxis s1
        Assert.Equal(AutoFitAxis, s2.xAxis.fit)

    [<Fact>]
    let ``the tri-state axis mutators edit X, left Y and right Y independently`` () =
        let s =
            defaultChartStyleState wlChart
            |> setAxisMin ChartXAxis (ChartAxisValue 450.0)
            |> setAxisMin (ChartYAxis LeftAxis) (ChartAxisValue 0.1)
            |> setAxisMax (ChartYAxis RightAxis) (ChartAxisValue 9.0)
            |> setAxisFormat (ChartYAxis RightAxis) (FixedFormat (DecimalDigits 1))
        Assert.Equal(450.0, s.xAxis.min.value)
        Assert.Equal(0.1, s.yAxisLeft.min.value)
        Assert.Equal(9.0, s.yAxisRight.max.value)
        // Each edit landed on ITS axis only: the untouched partners keep the General format,
        // and only the edited axes dropped out of auto.
        Assert.Equal(GeneralFormat, s.xAxis.format)
        Assert.Equal(GeneralFormat, s.yAxisLeft.format)
        Assert.Equal(FixedFormat (DecimalDigits 1), s.yAxisRight.format)
        Assert.Equal(PinnedAxis, s.xAxis.fit)
        Assert.Equal(PinnedAxis, s.yAxisLeft.fit)
        Assert.Equal(PinnedAxis, s.yAxisRight.fit)

    // ============================ legend ============================

    [<Fact>]
    let ``legend visibility and placement round-trip`` () =
        let s = defaultChartStyleState wlChart |> setLegendVisible HiddenOnChart |> setLegendPlacement LowerLeft
        Assert.Equal(HiddenOnChart, s.legend.visible)
        Assert.Equal(LowerLeft, s.legend.placement)
        Assert.Equal(9, List.length allPlacements)

    // ============================ series look ============================

    let private twoSeries : ExperimentChart =
        { ExperimentChart.emptyExperimentChart with
            series =
                [ curve "T" [ 0.0, 1.0; 90.0, 0.5 ]
                  curve "R" [ 0.0, 0.1; 90.0, 0.4 ] ]
            xAxisKind = AngularXAxis }

    let private first : ChartSeriesIndex = ChartSeriesIndex 0
    let private second : ChartSeriesIndex = ChartSeriesIndex 1

    [<Fact>]
    let ``a series' thickness, colour, markers and visibility are independently editable`` () =
        let s0 = defaultChartStyleState twoSeries
        let s =
            s0
            |> bumpSeriesThickness second (LinePixels 0.5)
            |> setSeriesColor second (SeriesColorHex "#C81E32")
            |> setSeriesMarkers second ShownOnChart
            |> setSeriesVisible first HiddenOnChart
        let a = seriesStyleOf first s
        let b = seriesStyleOf second s
        Assert.Equal(HiddenOnChart, a.visible)          // only series 0 hidden
        Assert.Equal(ShownOnChart, b.visible)
        Assert.Equal(2.0, b.thickness.value)   // 1.5 + 0.5
        Assert.Equal("#C81E32", b.colorHex.value)
        Assert.Equal(ShownOnChart, b.markers)
        Assert.Equal(HiddenOnChart, a.markers)      // series 0 untouched

    [<Fact>]
    let ``series thickness is clamped to the allowed band`` () =
        let s0 = defaultChartStyleState twoSeries
        let thick = List.fold (fun st _ -> bumpSeriesThickness first (LinePixels 1.0) st) s0 [ 1 .. 50 ]
        Assert.Equal(maxThickness, (seriesStyleOf first thick).thickness)
        let thin = List.fold (fun st _ -> bumpSeriesThickness first (LinePixels -1.0) st) s0 [ 1 .. 50 ]
        Assert.Equal(minThickness, (seriesStyleOf first thin).thickness)

    // ============================ axis-side assignment (spec 0033/018) ============================

    [<Fact>]
    let ``series default to the left axis and setSeriesAxisSide reassigns one side only`` () =
        let s0 = defaultChartStyleState twoSeries
        Assert.Equal(LeftAxis, (seriesStyleOf first s0).axisSide)
        Assert.Equal(LeftAxis, (seriesStyleOf second s0).axisSide)
        let s = setSeriesAxisSide second RightAxis s0
        Assert.Equal(LeftAxis, (seriesStyleOf first s).axisSide)
        Assert.Equal(RightAxis, (seriesStyleOf second s).axisSide)

    [<Fact>]
    let ``toCsv is unchanged by the dual-axis model (it reads only the chart data)`` () =
        // `toCsv` takes the chart alone — no style, hence no axis-side influence; its output is pinned so
        // the 018 model growth provably left the CSV export byte-identical (spec 0033/018 acceptance).
        Assert.Equal("x,T,R\n0,1,0.1\n90,0.5,0.4", twoSeries.toCsv)

    // ============================ selection + elements ============================

    [<Fact>]
    let ``elements are the five fixed parts plus one per series`` () =
        Assert.Equal<ChartElement list>(
            [ Header; XAxis; YAxis LeftAxis; YAxis RightAxis; Legend; Series first; Series second ], elements twoSeries.series)
        let label (seriesName : ChartSeriesIndex -> string) (e : ChartElement) : string =
            (elementLabel (fun i -> ControlCaption (seriesName i)) e).value
        Assert.Equal("Line: R", label (fun i -> if i = first then "T" else "R") (Series second))
        Assert.Equal("X axis", label (fun _ -> "") XAxis)
        Assert.Equal("Y axis (left)", label (fun _ -> "") (YAxis LeftAxis))
        Assert.Equal("Y axis (right)", label (fun _ -> "") (YAxis RightAxis))

    [<Fact>]
    let ``selectElement changes the selection only`` () =
        let s = defaultChartStyleState twoSeries |> selectElement Legend
        Assert.Equal(Legend, s.selected)

    [<Fact>]
    let ``bumpFont resizes the named target only`` () =
        let s0 = defaultChartStyleState twoSeries
        let before = s0.font.sizeOf ChartFont.Title
        let s = bumpFont ChartFont.Title (ChartFont.FontPoints 2.0) s0
        Assert.Equal(before.value + 2.0, (s.font.sizeOf ChartFont.Title).value)
        Assert.Equal(s0.font.sizeOf ChartFont.LegendFont, s.font.sizeOf ChartFont.LegendFont)

    // ============================ polar ============================

    [<Fact>]
    let ``polarXY maps angle+radius onto the cartesian circle`` () =
        let close (a : float) (b : float) = abs (a - b) < 1e-9
        let polar (angleDeg : float) (radius : float) : ExperimentControls.ChartPoint =
            let p : ExperimentControls.ChartPoint = { x = angleDeg; y = radius }
            p.polarXY
        let p0 = polar 0.0 2.0
        Assert.True(close 2.0 p0.x && close 0.0 p0.y)          // 0°  → (r, 0)
        let p90 = polar 90.0 2.0
        Assert.True(close 0.0 p90.x && close 2.0 p90.y)        // 90° → (0, r)
        let p180 = polar 180.0 2.0
        Assert.True(close -2.0 p180.x)                          // 180°→ (−r, ·)

    [<Fact>]
    let ``polarRadius is the largest visible |y| (with a positive fallback)`` () =
        let s = defaultChartStyleState twoSeries
        Assert.Equal(1.0, (polarRadius s twoSeries.series).value)   // max |y| across both series = 1.0
        // Hiding the series that carries the max shrinks the radius.
        let s2 = setSeriesVisible first HiddenOnChart s
        Assert.Equal(0.4, (polarRadius s2 twoSeries.series).value)
        Assert.True((polarRadius s []).value > 0.0)                 // fallback stays positive
