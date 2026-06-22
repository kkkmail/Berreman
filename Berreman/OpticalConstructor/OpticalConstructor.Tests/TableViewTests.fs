namespace OpticalConstructor.Tests

open Berreman.Constants
open Berreman.Geometry
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Placement
open OpticalConstructor.Domain.Table
open OpticalConstructor.Domain.TableView
open Xunit

/// Headless proof that the table's 3-D rotation + orthographic projection (Spec 0027,
/// task 002-rotate-table; `TableView.fs`) actually works — the thing that was missing
/// (only a flat in-plane R1 spin existed; R2/R3 were inert). Everything here is pure: no
/// Avalonia type is touched, so it runs under `constructor-unit-tests`. The on-screen
/// behaviour of the test window is the same `TableView` functions driven through its MVU.
module TableViewTests =

    let private tol = 1.0e-9
    let private close (a : float) (b : float) : bool = abs (a - b) <= tol

    let private ppm = 200.0
    let private center : ScreenPoint = { sx = 400.0; sy = 300.0 }
    let private table = Table.defaultTable
    let private vec (x : float) (y : float) (z : float) : Vector3 = Vector3.create x y z

    let private viewWith (r1 : float) (r2 : float) (r3 : float) : TableViewState =
        { Table.defaultView with r1 = Angle.degree r1; r2 = Angle.degree r2; r3 = Angle.degree r3 }

    // --- Default view is exactly the straight top-down layout --------------------------

    [<Fact>]
    let ``at the default top-down view a plane point projects to scale + Y-flip about the centre`` () =
        let p = vec 0.3 0.2 0.0
        let s = ppm * Table.defaultView.zoom
        let projected = TableView.project ppm center Table.defaultView p
        Assert.True(close projected.sx (center.sx + s * 0.3), $"sx = {projected.sx}")
        // Screen Y runs DOWN, so +Y maps above the centre (smaller sy).
        Assert.True(close projected.sy (center.sy - s * 0.2), $"sy = {projected.sy}")

    [<Fact>]
    let ``the table origin projects to the canvas centre under any rotation`` () =
        for (r1, r2, r3) in [ (0.0, 0.0, 0.0); (37.0, 0.0, 0.0); (10.0, 25.0, 40.0); (-90.0, 60.0, -15.0) ] do
            let projected = TableView.project ppm center (viewWith r1 r2 r3) (vec 0.0 0.0 0.0)
            Assert.True(close projected.sx center.sx && close projected.sy center.sy,
                        $"origin projected to ({projected.sx}, {projected.sy}) at ({r1},{r2},{r3})")

    // --- Each rotation actually moves the projection -----------------------------------

    [<Fact>]
    let ``R1 spins the +X axis toward screen up`` () =
        // A 90 deg in-plane spin sends +X (right) to +Y (up): screen x back to centre, y above.
        let s = ppm * Table.defaultView.zoom
        let projected = TableView.project ppm center (viewWith 90.0 0.0 0.0) (vec 1.0 0.0 0.0)
        Assert.True(close projected.sx center.sx, $"sx = {projected.sx}")
        Assert.True(close projected.sy (center.sy - s * 1.0), $"sy = {projected.sy}")

    [<Fact>]
    let ``R2 pitch tilts the +Y edge out of the plane and foreshortens it onto the centre line`` () =
        // A 90 deg pitch about the screen-horizontal axis sends +Y to the depth axis, so it
        // projects onto the centre line (no screen-Y offset) — the table is seen edge-on.
        let projected = TableView.project ppm center (viewWith 0.0 90.0 0.0) (vec 0.0 1.0 0.0)
        Assert.True(close projected.sx center.sx && close projected.sy center.sy,
                    $"(+Y) projected to ({projected.sx}, {projected.sy})")

    [<Fact>]
    let ``R3 yaw tilts the +X axis out of the plane and foreshortens it onto the centre line`` () =
        let projected = TableView.project ppm center (viewWith 0.0 0.0 90.0) (vec 1.0 0.0 0.0)
        Assert.True(close projected.sx center.sx && close projected.sy center.sy,
                    $"(+X) projected to ({projected.sx}, {projected.sy})")

    [<Fact>]
    let ``a small pitch genuinely changes a point's projection (R2 is no longer inert)`` () =
        let flat = TableView.project ppm center Table.defaultView (vec 0.0 0.5 0.0)
        let tilted = TableView.project ppm center (viewWith 0.0 30.0 0.0) (vec 0.0 0.5 0.0)
        Assert.True(abs (flat.sy - tilted.sy) > 1.0, $"pitch left the projection unchanged: {flat.sy} vs {tilted.sy}")

    // --- The projection inverts (round-trip on the table plane) ------------------------

    [<Fact>]
    let ``projecting a plane point then unprojecting returns it (round-trip)`` () =
        for (r1, r2, r3) in [ (0.0, 0.0, 0.0); (25.0, 0.0, 0.0); (15.0, 35.0, 20.0); (-40.0, -20.0, 65.0) ] do
            let view = viewWith r1 r2 r3
            for (x, y) in [ (0.0, 0.0); (0.4, -0.3); (-0.9, 0.55) ] do
                let projected = TableView.project ppm center view (vec x y 0.0)
                match TableView.unprojectToTablePlane ppm center view projected with
                | Some tp ->
                    Assert.True(close (tp.x / 1.0<meter>) x && close (tp.y / 1.0<meter>) y,
                                $"round-trip ({x},{y}) at ({r1},{r2},{r3}) -> ({tp.x / 1.0<meter>}, {tp.y / 1.0<meter>})")
                | None -> Assert.Fail($"unproject returned None for a non-edge-on view ({r1},{r2},{r3})")

    [<Fact>]
    let ``unproject returns None when the view is edge-on`` () =
        // A 90 deg pitch projects the whole table plane onto a single screen line (det = 0).
        let view = viewWith 0.0 90.0 0.0
        Assert.True((TableView.unprojectToTablePlane ppm center view center).IsNone)

    // --- The plate is the real optical table, drawn as a 3-D box -----------------------

    [<Fact>]
    let ``the plate is a 1.2 x 2.0 x 0.10 m box with eight corners and twelve edges`` () =
        Assert.Equal(8, List.length (TableView.plateCorners3D table))
        Assert.Equal(12, List.length TableView.plateEdges)
        let xs = TableView.plateCorners3D table |> List.map (fun c -> c.x)
        let ys = TableView.plateCorners3D table |> List.map (fun c -> c.y)
        let zs = TableView.plateCorners3D table |> List.map (fun c -> c.z)
        Assert.True(close (List.max xs - List.min xs) 2.0, "length (X) should be 2.0 m")
        Assert.True(close (List.max ys - List.min ys) 1.2, "width (Y) should be 1.2 m")
        Assert.True(close (List.max zs - List.min zs) 0.10, "thickness (Z) should be 0.10 m")

    // --- Selection: a click hits the projected plate, misses outside (AC-C3) -----------

    [<Fact>]
    let ``a click on the projected table hits it and a click far outside misses`` () =
        let view = Table.defaultView
        Assert.True(TableView.tableHit ppm center view table center, "centre of the plate should hit")
        Assert.False(TableView.tableHit ppm center view table { sx = center.sx + 10000.0; sy = center.sy },
                     "a click far off the plate should miss")

    [<Fact>]
    let ``the projected plate is still hittable after the table is tumbled in 3-D`` () =
        let view = viewWith 30.0 20.0 15.0
        // The centroid of the projected top face is, by convexity, inside the projected plate.
        let face = TableView.plateTopFace table |> List.map (TableView.project ppm center view)
        let cx = face |> List.averageBy (fun p -> p.sx)
        let cy = face |> List.averageBy (fun p -> p.sy)
        Assert.True(TableView.tableHit ppm center view table { sx = cx; sy = cy },
                    "the tumbled plate's own centre should still select it")
