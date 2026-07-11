namespace OpticalConstructor.Ui.Tests

open Xunit
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Units
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Controls.ExperimentChart
open OpticalConstructor.Controls.ChartStyle
open OpticalConstructor.Ui

/// Spec 0033 (019) — the pure n/k dispersion chart builder on the 018 dual-axis spine.
/// Headless MODEL tests (no window): the builder yields the shared `ExperimentChart` with the
/// n series on the LEFT axis and the k series on the RIGHT (via the paired style seed), spanning
/// the requested spectral range in the display unit; a dispersive entry yields curves and a
/// non-dispersive entry yields flat lines through the SAME builder.
module NkDispersionChartTests =

    /// Silicon — a DISPERSIVE built-in (engine preset, dispersion coded, `complexity = None`).
    let private siliconEntry : MaterialEntry =
        builtInEntries |> List.find (fun e -> e.id = MaterialIds.silicon)

    /// Vacuum — a NON-dispersive built-in (n = 1, k = 0 at every wavelength).
    let private vacuumEntry : MaterialEntry =
        builtInEntries |> List.find (fun e -> e.id = MaterialIds.vacuum)

    [<Fact>]
    let ``AC: n lands on the left axis, k on the right, spanning the requested spectral range`` () =
        let range = SpectralAxis.spectralRange Nanometer 400.0 800.0 40
        let chart = NkDispersionChart.nkDispersionChart siliconEntry.properties Nanometer range
        // Two series in the builder's fixed order: n then k, sharing one x grid.
        Assert.Equal(2, List.length chart.series)
        Assert.Equal<string>("n", (List.item NkDispersionChart.nSeriesIndex chart.series).name)
        Assert.Equal<string>("k", (List.item NkDispersionChart.kSeriesIndex chart.series).name)
        let xs = (List.item NkDispersionChart.nSeriesIndex chart.series).points |> List.map fst
        Assert.Equal<float list>(xs, (List.item NkDispersionChart.kSeriesIndex chart.series).points |> List.map fst)
        // The x-axis spans the REQUESTED range in the display unit: 400…800 nm over 41 grid points.
        Assert.Equal(41, List.length xs)
        Assert.True(abs (List.head xs - 400.0) < 1e-9, $"first x = %g{List.head xs}")
        Assert.True(abs (List.last xs - 800.0) < 1e-9, $"last x = %g{List.last xs}")
        Assert.Equal<string>(SpectralAxis.axisLabel Nanometer, chart.xLabel)
        Assert.False(chart.angular)
        // The paired style seed carries the axis assignment (the side is STYLE, not data — 018):
        // n stays on the LEFT axis, k is flipped to the RIGHT.
        let style = NkDispersionChart.nkDispersionStyle chart
        Assert.Equal(LeftAxis, (seriesStyleOf NkDispersionChart.nSeriesIndex style).axisSide)
        Assert.Equal(RightAxis, (seriesStyleOf NkDispersionChart.kSeriesIndex style).axisSide)

    [<Fact>]
    let ``a dispersive entry yields a curve — silicon's n varies across 400…800 nm`` () =
        let range = SpectralAxis.spectralRange Nanometer 400.0 800.0 40
        let chart = NkDispersionChart.nkDispersionChart siliconEntry.properties Nanometer range
        let ys = (List.item NkDispersionChart.nSeriesIndex chart.series).points |> List.map snd
        let span = List.max ys - List.min ys
        Assert.True(span > 0.01, $"silicon n span across the range = %g{span} (expected a curve)")

    [<Fact>]
    let ``a non-dispersive entry yields flat lines through the SAME builder — vacuum n = 1, k = 0`` () =
        let range = SpectralAxis.spectralRange Nanometer 400.0 800.0 40
        let chart = NkDispersionChart.nkDispersionChart vacuumEntry.properties Nanometer range
        for s in chart.series do
            let ys = s.points |> List.map snd
            let span = List.max ys - List.min ys
            Assert.True(span < 1e-12, $"series %s{s.name} span = %g{span} (expected flat)")
        let nAt (i : int) = (List.item i chart.series).points |> List.head |> snd
        Assert.True(abs (nAt NkDispersionChart.nSeriesIndex - 1.0) < 1e-12, "vacuum n must be 1")
        Assert.True(abs (nAt NkDispersionChart.kSeriesIndex) < 1e-12, "vacuum k must be 0")

    [<Fact>]
    let ``the display unit rescales only the x-axis (AC-D7) — the y data is identical in eV and nm`` () =
        let range = SpectralAxis.spectralRange Nanometer 400.0 800.0 8
        let inNm = NkDispersionChart.nkDispersionChart siliconEntry.properties Nanometer range
        let inEv = NkDispersionChart.nkDispersionChart siliconEntry.properties ElectronVolt range
        let ys (c : ExperimentChart) (i : int) : float list = (List.item i c.series).points |> List.map snd
        Assert.Equal<float list>(ys inNm NkDispersionChart.nSeriesIndex, ys inEv NkDispersionChart.nSeriesIndex)
        Assert.Equal<float list>(ys inNm NkDispersionChart.kSeriesIndex, ys inEv NkDispersionChart.kSeriesIndex)
        Assert.Equal<string>(SpectralAxis.axisLabel ElectronVolt, inEv.xLabel)
        // eV ticks descend as the wavelength ascends — a rescale of the SAME grid, not a re-sample.
        let xsEv = (List.item NkDispersionChart.nSeriesIndex inEv.series).points |> List.map fst
        Assert.True(List.head xsEv > List.last xsEv, "eV axis must descend across an ascending-λ grid")
