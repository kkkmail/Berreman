/// §H.7 — the [Standard] cursor/readout & marker tools, operating on the ScottPlot
/// 1D plot's series. ALL of these are pure functions over a `SeriesData` 1D
/// `(float*float)[]` array plus cursor positions: hover readout (nearest series
/// point), peak/min finders (array scan), FWHM (half-maximum crossing interpolation),
/// and a two-cursor ΔX/ΔY. Part H recomputes NO physics and never re-solves to find a
/// peak. Marker positions are plain values the MVU chart-page model holds (`Markers`
/// below) — never renderer-held state.
module OpticalConstructor.Ui.Charts.Readout

/// An (x, y) point on a series.
type ReadoutPoint = float * float

/// Marker positions as a pure value held by the MVU model (R-7: no renderer-held
/// marker state). Each cursor is an (x, y) coordinate placed by the user.
type Markers =
    {
        cursor1 : ReadoutPoint option
        cursor2 : ReadoutPoint option
    }

    static member empty = { cursor1 = None; cursor2 = None }

/// Hover readout: the series point nearest a cursor (Euclidean), the pure equivalent
/// of ScottPlot's `GetNearest` over the same data. Precondition: non-empty series.
let nearestPoint (series : ReadoutPoint []) ((cx, cy) : ReadoutPoint) : ReadoutPoint =
    series |> Array.minBy (fun (x, y) -> (x - cx) * (x - cx) + (y - cy) * (y - cy))

/// Peak finder: the point of maximum y (pure array scan).
let peak (series : ReadoutPoint []) : ReadoutPoint =
    series |> Array.maxBy snd

/// Min finder: the point of minimum y (pure array scan).
let minimum (series : ReadoutPoint []) : ReadoutPoint =
    series |> Array.minBy snd

/// FWHM: the full width at half maximum of a single-peak trace. The half-maximum
/// level is taken relative to the baseline, `(yMax + yMin) / 2`; the width is the
/// linearly-interpolated distance between the half-maximum crossings on either side
/// of the apex. Returns `None` if there are too few points or no crossing.
let fwhm (series : ReadoutPoint []) : float option =
    if series.Length < 2 then None
    else
        let ys = series |> Array.map snd
        let yMax = Array.max ys
        let yMin = Array.min ys
        let level = (yMax + yMin) / 2.0
        let peakIdx = series |> Array.findIndex (fun (_, y) -> y = yMax)

        let crossing i j =
            let (xi, yi) = series.[i]
            let (xj, yj) = series.[j]
            if (yi - level) * (yj - level) <= 0.0 && yi <> yj then
                Some (xi + (level - yi) * (xj - xi) / (yj - yi))
            else None

        let mutable left = None
        let mutable k = peakIdx
        while left.IsNone && k > 0 do
            left <- crossing (k - 1) k
            k <- k - 1

        let mutable right = None
        let mutable m = peakIdx
        while right.IsNone && m < series.Length - 1 do
            right <- crossing m (m + 1)
            m <- m + 1

        match left, right with
        | Some l, Some r -> Some (r - l)
        | _ -> None

/// Two-cursor delta: ΔX and ΔY between the series points nearest the two placed
/// markers. Pure over the series array and the cursor coordinates.
let twoCursorDelta (series : ReadoutPoint []) (c1 : ReadoutPoint) (c2 : ReadoutPoint) : float * float =
    let (x1, y1) = nearestPoint series c1
    let (x2, y2) = nearestPoint series c2
    (x2 - x1, y2 - y1)
