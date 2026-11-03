/// §H.7 — the [Standard] cursor/readout & marker tools, operating on the ScottPlot
/// 1D plot's series. ALL of these are pure functions over a `SeriesData` 1D
/// `ChartPoint` array plus cursor positions: hover readout (nearest series
/// point), peak/min finders (array scan), FWHM (half-maximum crossing interpolation),
/// and a two-cursor ΔX/ΔY. Part H recomputes NO physics and never re-solves to find a
/// peak. Marker positions are plain values the MVU chart-page model holds (`Markers`
/// below) — never renderer-held state.
module OpticalConstructor.Ui.Charts.Readout

open OpticalConstructor.Controls.ExperimentControls
open OpticalConstructor.Controls.ChartStyle

/// Marker positions as a pure value held by the MVU model (R-7: no renderer-held
/// marker state). Each cursor is a point placed by the user.
type Markers =
    {
        cursor1 : ChartPoint option
        cursor2 : ChartPoint option
    }

    static member empty = { cursor1 = None; cursor2 = None }

/// The ΔX and ΔY between two snapped markers, each in its axis' display units.
type CursorDelta =
    {
        deltaX : ChartAxisValue
        deltaY : ChartAxisValue
    }

/// Hover readout: the series point nearest a cursor (Euclidean), the pure equivalent
/// of ScottPlot's `GetNearest` over the same data. Precondition: non-empty series.
let nearestPoint (series : ChartPoint []) (cursor : ChartPoint) : ChartPoint =
    series |> Array.minBy (fun p -> (p.x - cursor.x) * (p.x - cursor.x) + (p.y - cursor.y) * (p.y - cursor.y))

/// Peak finder: the point of maximum y (pure array scan).
let peak (series : ChartPoint []) : ChartPoint =
    series |> Array.maxBy (fun p -> p.y)

/// Min finder: the point of minimum y (pure array scan).
let minimum (series : ChartPoint []) : ChartPoint =
    series |> Array.minBy (fun p -> p.y)

/// FWHM: the full width at half maximum of a single-peak trace. The half-maximum
/// level is taken relative to the baseline, `(yMax + yMin) / 2`; the width is the
/// linearly-interpolated distance between the half-maximum crossings on either side
/// of the apex. Returns `None` if there are too few points or no crossing.
let fwhm (series : ChartPoint []) : ChartAxisValue option =
    if series.Length < 2 then None
    else
        let ys = series |> Array.map (fun p -> p.y)
        let yMax = Array.max ys
        let yMin = Array.min ys
        let level = (yMax + yMin) / 2.0
        let peakIdx = series |> Array.findIndex (fun p -> p.y = yMax)

        let crossing i j =
            let p = series.[i]
            let q = series.[j]
            if (p.y - level) * (q.y - level) <= 0.0 && p.y <> q.y then
                Some (p.x + (level - p.y) * (q.x - p.x) / (q.y - p.y))
            else None

        // The first crossing walking outwards from the apex on each side.
        let left = Seq.init peakIdx (fun n -> peakIdx - n) |> Seq.tryPick (fun k -> crossing (k - 1) k)
        let right = seq { peakIdx .. series.Length - 2 } |> Seq.tryPick (fun m -> crossing m (m + 1))

        match left, right with
        | Some l, Some r -> Some (ChartAxisValue (r - l))
        | _ -> None

/// Two-cursor delta: ΔX and ΔY between the series points nearest the two placed
/// markers. Pure over the series array and the cursor positions.
let twoCursorDelta (series : ChartPoint []) (c1 : ChartPoint) (c2 : ChartPoint) : CursorDelta =
    let p1 = nearestPoint series c1
    let p2 = nearestPoint series c2
    { deltaX = ChartAxisValue (p2.x - p1.x); deltaY = ChartAxisValue (p2.y - p1.y) }
