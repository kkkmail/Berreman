/// §H.10 — the [Standard] CIE 1931 chromaticity diagram: the horseshoe (spectral
/// locus) boundary plus a supplied chromaticity point (x, y) and its sRGB swatch.
/// The XYZ→xy / XYZ→sRGB numbers and the spectra→XYZ integration are the net-new
/// color calculation owned by Part F §F.8 (`Analytics/Colorimetry.fs`); Part H
/// consumes the resulting `{ x; y; rgb }` and implements NO color-matching
/// integration or gamma/sRGB transform. The spectral-locus coordinates are static
/// reference data and live as a literal array here — the ONLY static numeric table
/// Part H introduces.
module OpticalConstructor.Ui.Charts.CieView

open OpticalConstructor.Ui.Charts.ChartSettings

/// The chromaticity point Part H consumes from §F.8: a point (x, y) and its
/// already-computed sRGB swatch (8-bit per channel).
type ChromaticityPoint =
    {
        x : float
        y : float
        rgb : byte * byte * byte
    }

/// CIE 1931 spectral-locus (x, y) chromaticity coordinates at 10 nm from 380 to
/// 700 nm — static reference data for the horseshoe boundary (R-10).
let spectralLocus : (float * float) [] =
    [|
        (0.1741, 0.0050)   // 380 nm
        (0.1738, 0.0049)   // 390
        (0.1733, 0.0048)   // 400
        (0.1726, 0.0048)   // 410
        (0.1714, 0.0051)   // 420
        (0.1689, 0.0069)   // 430
        (0.1644, 0.0109)   // 440
        (0.1566, 0.0177)   // 450
        (0.1440, 0.0297)   // 460
        (0.1241, 0.0578)   // 470
        (0.0913, 0.1327)   // 480
        (0.0454, 0.2950)   // 490
        (0.0082, 0.5384)   // 500
        (0.0139, 0.7502)   // 510
        (0.0743, 0.8338)   // 520
        (0.1547, 0.8059)   // 530
        (0.2296, 0.7543)   // 540
        (0.3016, 0.6923)   // 550
        (0.3731, 0.6245)   // 560
        (0.4441, 0.5547)   // 570
        (0.5125, 0.4866)   // 580
        (0.5752, 0.4242)   // 590
        (0.6270, 0.3725)   // 600
        (0.6658, 0.3340)   // 610
        (0.6915, 0.3083)   // 620
        (0.7079, 0.2920)   // 630
        (0.7190, 0.2809)   // 640
        (0.7260, 0.2740)   // 650
        (0.7300, 0.2700)   // 660
        (0.7320, 0.2680)   // 670
        (0.7334, 0.2666)   // 680
        (0.7344, 0.2656)   // 690
        (0.7347, 0.2653)   // 700
    |]

/// The closed horseshoe outline: the spectral locus plus the "line of purples"
/// closing segment back to the first point.
let horseshoeBoundary : (float * float) [] =
    Array.append spectralLocus [| spectralLocus.[0] |]

/// §H.10 — draw the CIE 1931 horseshoe boundary and plot the supplied chromaticity
/// point with its sRGB swatch onto a ScottPlot plot. No color is computed here.
let render (settings : ChartSettings) (point : ChromaticityPoint) : ScottPlot.Plot =
    let plot = new ScottPlot.Plot()

    let boundary = horseshoeBoundary
    let b = plot.Add.Scatter(boundary |> Array.map fst, boundary |> Array.map snd)
    b.LegendText <- "CIE 1931"

    let p = plot.Add.Scatter([| point.x |], [| point.y |])
    let (r, g, bch) = point.rgb
    p.Color <- ScottPlot.Color(r, g, bch, 255uy)
    p.LegendText <- "point"

    plot.XLabel("x")
    plot.YLabel("y")
    applyToScottPlot settings plot
    plot
