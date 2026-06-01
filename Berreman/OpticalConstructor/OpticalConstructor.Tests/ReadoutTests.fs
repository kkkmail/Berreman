namespace OpticalConstructor.Tests

open OpticalConstructor.Ui.Charts
open Xunit

/// §H.7 readout tests (slice 012). AC-H6 — peak/min/FWHM/two-cursor-delta computed
/// PURELY over a synthetic single-peak `SeriesData` 1D array, with no re-solve.
module ReadoutTests =

    /// A symmetric triangular peak on x ∈ [0, 10]: apex (5, 1), zero at the ends.
    /// Half-maximum 0.5 is crossed at x = 2.5 and 7.5, so FWHM = 5.0 exactly.
    let private series =
        [| for i in 0..10 -> (float i, max 0.0 (1.0 - abs (float i - 5.0) / 5.0)) |]

    [<Fact>]
    let ``AC-H6 peak finder returns the apex`` () =
        let (x, y) = Readout.peak series
        Assert.Equal(5.0, x, 12)
        Assert.Equal(1.0, y, 12)

    [<Fact>]
    let ``AC-H6 min finder returns a trough`` () =
        let (_, y) = Readout.minimum series
        Assert.Equal(0.0, y, 12)

    [<Fact>]
    let ``AC-H6 FWHM is the half-maximum width`` () =
        match Readout.fwhm series with
        | Some w -> Assert.Equal(5.0, w, 9)
        | None -> Assert.Fail "expected a FWHM"

    [<Fact>]
    let ``AC-H6 nearest point snaps to the closest series sample`` () =
        let (x, y) = Readout.nearestPoint series (4.9, 0.95)
        Assert.Equal(5.0, x, 12)
        Assert.Equal(1.0, y, 12)

    [<Fact>]
    let ``AC-H6 two-cursor delta is the exact dX,dY between snapped markers`` () =
        // Cursors at the symmetric (3, 0.6) and (7, 0.6) samples.
        let (dx, dy) = Readout.twoCursorDelta series (3.0, 0.6) (7.0, 0.6)
        Assert.Equal(4.0, dx, 12)
        Assert.Equal(0.0, dy, 12)
