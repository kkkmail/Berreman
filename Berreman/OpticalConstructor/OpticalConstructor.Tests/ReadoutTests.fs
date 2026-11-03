namespace OpticalConstructor.Tests

open OpticalConstructor.Controls.ExperimentControls
open OpticalConstructor.Ui.Charts
open Xunit

/// §H.7 readout tests (slice 012). AC-H6 — peak/min/FWHM/two-cursor-delta computed
/// PURELY over a synthetic single-peak `SeriesData` 1D array, with no re-solve.
module ReadoutTests =

    /// A symmetric triangular peak on x ∈ [0, 10]: apex (5, 1), zero at the ends.
    /// Half-maximum 0.5 is crossed at x = 2.5 and 7.5, so FWHM = 5.0 exactly.
    let private series : ChartPoint [] =
        [| for i in 0..10 -> { x = float i; y = max 0.0 (1.0 - abs (float i - 5.0) / 5.0) } |]

    [<Fact>]
    let ``AC-H6 peak finder returns the apex`` () =
        let p = Readout.peak series
        Assert.Equal(5.0, p.x, 12)
        Assert.Equal(1.0, p.y, 12)

    [<Fact>]
    let ``AC-H6 min finder returns a trough`` () =
        let p = Readout.minimum series
        Assert.Equal(0.0, p.y, 12)

    [<Fact>]
    let ``AC-H6 FWHM is the half-maximum width`` () =
        match Readout.fwhm series with
        | Some w -> Assert.Equal(5.0, w.value, 9)
        | None -> Assert.Fail "expected a FWHM"

    [<Fact>]
    let ``AC-H6 nearest point snaps to the closest series sample`` () =
        let p = Readout.nearestPoint series { x = 4.9; y = 0.95 }
        Assert.Equal(5.0, p.x, 12)
        Assert.Equal(1.0, p.y, 12)

    [<Fact>]
    let ``AC-H6 two-cursor delta is the exact dX,dY between snapped markers`` () =
        // Cursors at the symmetric (3, 0.6) and (7, 0.6) samples.
        let d = Readout.twoCursorDelta series { x = 3.0; y = 0.6 } { x = 7.0; y = 0.6 }
        Assert.Equal(4.0, d.deltaX.value, 12)
        Assert.Equal(0.0, d.deltaY.value, 12)
