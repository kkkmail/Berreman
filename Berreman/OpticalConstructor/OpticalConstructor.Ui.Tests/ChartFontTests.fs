namespace OpticalConstructor.Ui.Tests

open Xunit
open OpticalConstructor.Controls
open OpticalConstructor.Controls.ChartFont

/// Spec 0027 (028) — the pop-out chart window's per-target font-size state: the OLD Font +/- resized only
/// one text kind; the redesign lets the user SELECT which text (header / axis labels / tick labels /
/// legend) Font +/- resizes, each with its own size, shown numerically. These pure tests exercise the
/// selection / bump / clamp / readout logic without opening a native window.
module ChartFontTests =

    [<Fact>]
    let ``the four targets are distinct with non-empty labels`` () =
        Assert.Equal(4, List.length ChartFont.allTargets)
        Assert.Equal(4, ChartFont.allTargets |> List.distinct |> List.length)
        for t in ChartFont.allTargets do
            Assert.False(System.String.IsNullOrWhiteSpace t.label)
        // The header target reads "Header" (the chart title), tick labels "Tick labels".
        Assert.Equal("Header", ChartFont.Title.label)
        Assert.Equal("Tick labels", ChartFont.TickLabels.label)

    [<Fact>]
    let ``the default state selects the header and carries per-target sizes`` () =
        let s = ChartFont.defaultState
        Assert.Equal(ChartFont.Title, s.selected)
        Assert.Equal(s.title, ChartFont.selectedSize s)
        // Each target has its own size; they are not all forced equal.
        Assert.NotEqual(ChartFont.sizeOf ChartFont.Title s, ChartFont.sizeOf ChartFont.TickLabels s)

    [<Fact>]
    let ``withSelected changes only the selection, not any size`` () =
        let s0 = ChartFont.defaultState
        let s1 = ChartFont.withSelected ChartFont.Legend s0
        Assert.Equal(ChartFont.Legend, s1.selected)
        Assert.Equal(s0.title, s1.title)
        Assert.Equal(s0.legend, s1.legend)
        // The readout now reports the legend's size.
        Assert.Equal(ChartFont.sizeOf ChartFont.Legend s0, ChartFont.selectedSize s1)

    [<Fact>]
    let ``bumpSelected changes ONLY the selected target's size`` () =
        let s0 = ChartFont.withSelected ChartFont.TickLabels ChartFont.defaultState
        let before = ChartFont.sizeOf ChartFont.TickLabels s0
        let s1 = ChartFont.bumpSelected 1.0 s0
        Assert.Equal(before + 1.0, ChartFont.sizeOf ChartFont.TickLabels s1)
        // Every other target is untouched.
        Assert.Equal(s0.title, s1.title)
        Assert.Equal(s0.axisLabels, s1.axisLabels)
        Assert.Equal(s0.legend, s1.legend)

    [<Fact>]
    let ``bumpSelected clamps to the size band`` () =
        // Drive the selected size below the floor and above the ceiling.
        let low = List.fold (fun s _ -> ChartFont.bumpSelected -1.0 s) ChartFont.defaultState [ 1 .. 100 ]
        Assert.Equal(ChartFont.minSize, ChartFont.selectedSize low)
        let high = List.fold (fun s _ -> ChartFont.bumpSelected 1.0 s) ChartFont.defaultState [ 1 .. 100 ]
        Assert.Equal(ChartFont.maxSize, ChartFont.selectedSize high)

    [<Fact>]
    let ``withSize clamps and sets exactly the named target`` () =
        let s = ChartFont.withSize ChartFont.AxisLabels 1000.0 ChartFont.defaultState
        Assert.Equal(ChartFont.maxSize, ChartFont.sizeOf ChartFont.AxisLabels s)
        let s2 = ChartFont.withSize ChartFont.AxisLabels 18.0 ChartFont.defaultState
        Assert.Equal(18.0, ChartFont.sizeOf ChartFont.AxisLabels s2)

    [<Fact>]
    let ``the readout names the selected target and its size`` () =
        let s = ChartFont.withSelected ChartFont.TickLabels ChartFont.defaultState
        let text = ChartFont.readout s
        Assert.Contains("Tick labels", text)
        Assert.Contains("pt", text)
