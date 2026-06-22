namespace OpticalConstructor.Domain

open Berreman.Constants
open Berreman.Geometry
open OpticalConstructor.Domain.Placement
open OpticalConstructor.Domain.Table

/// Pure, headless-testable 3-D rotation + orthographic projection of the optical table to
/// the screen (Spec 0026 §C.2.4 — "the table's own R1/R2/R3 are measured relative to the
/// screen … all elements travel with it"). This is the SINGLE source of truth for turning
/// a `TableViewState` orientation into screen coordinates: no Avalonia type appears here
/// (constraint 0.3), so every projection/selection rule is provable from headless tests,
/// and both the constructor page and the standalone rotation test window can drive it.
///
/// The earlier top-down layout (`ConstructorTable.project`) applied only scale + Y-flip +
/// pan and a flat in-plane spin by R1; R2/R3 were inert ("top-down approximation"), which
/// is why tilting the table did nothing. This module replaces that with a real 3-D tumble.
///
/// Convention — all three rotations are about the FIXED screen axes (so they are "relative
/// to the screen"); at the rest pose the screen axes coincide with the table axes:
///   * screen +X = table +X = right;  screen +Y = table +Y = up;  depth +Z = toward viewer;
///   * R1 spins about the viewing axis (+Z) — the flat in-plane spin;
///   * R2 pitches about the screen-horizontal axis (+X) — tips the +Y (far) edge toward you;
///   * R3 yaws  about the screen-vertical axis (+Y).
/// A table-frame point is mapped as `p' = Ry(r3) (Rx(r2) (Rz(r1) p))`, then orthographically
/// projected (the depth axis is dropped). At `(0, 0, 0)` the rotation is the identity, so the
/// default view is exactly the straight top-down layout.
module TableView =

    /// A point projected to the screen / canvas in logical (pixel) units. Screen +x runs to
    /// the right and +y runs DOWN (the Avalonia convention). Pure floats — the FuncUI binding
    /// maps it to an `Avalonia.Point`.
    type ScreenPoint =
        {
            sx : float
            sy : float
        }

    /// Rotate a table-frame point about the fixed +Z (viewing) axis by `a` — the R1 spin.
    let private rotateZ (a : Angle) (p : Vector3) : Vector3 =
        let c = cos a.value
        let s = sin a.value
        Vector3.create (p.x * c - p.y * s) (p.x * s + p.y * c) p.z

    /// Rotate a table-frame point about the fixed +X (screen-horizontal) axis by `a` — the
    /// R2 pitch.
    let private rotateX (a : Angle) (p : Vector3) : Vector3 =
        let c = cos a.value
        let s = sin a.value
        Vector3.create p.x (p.y * c - p.z * s) (p.y * s + p.z * c)

    /// Rotate a table-frame point about the fixed +Y (screen-vertical) axis by `a` — the R3
    /// yaw.
    let private rotateY (a : Angle) (p : Vector3) : Vector3 =
        let c = cos a.value
        let s = sin a.value
        Vector3.create (p.x * c + p.z * s) p.y (-p.x * s + p.z * c)

    /// The composed table-view rotation (R1 spin, then R2 pitch, then R3 yaw), all about the
    /// fixed screen axes (§C.2.4). At `(0, 0, 0)` it is the identity.
    let rotate (view : TableViewState) (p : Vector3) : Vector3 =
        p |> rotateZ view.r1 |> rotateX view.r2 |> rotateY view.r3

    /// Orthographically project a table-frame point (canonical meters) to the screen under a
    /// view state: rotate by the view orientation, drop the depth axis, scale by
    /// `pixelsPerMeter × zoom`, flip Y (screen Y runs down), and translate by the canvas
    /// `center` plus the view pan. At the default top-down `(0, 0, 0)` / no-pan view this is
    /// exactly `center + (s·x, -s·y)` — the straight top-down layout.
    let project (pixelsPerMeter : float) (center : ScreenPoint) (view : TableViewState) (p : Vector3) : ScreenPoint =
        let r = rotate view p
        let s = pixelsPerMeter * view.zoom
        {
            sx = center.sx + view.panX + s * r.x
            sy = center.sy + view.panY - s * r.y
        }

    /// The eight corners of the optical-table plate as a 3-D box centred at the table origin:
    /// LENGTH along +X, WIDTH along +Y, THICKNESS along +Z (all canonical meters). The four
    /// TOP-face corners (z = +T/2) come first — counter-clockwise from `(-L/2, -W/2)` — then
    /// the four BOTTOM-face corners (z = -T/2) in the same order, so `plateEdges` can index them.
    let plateCorners3D (table : OpticalTable) : Vector3 list =
        let hl = (table.length / 2.0) / 1.0<meter>
        let hw = (table.width / 2.0) / 1.0<meter>
        let ht = (table.thickness / 2.0) / 1.0<meter>
        [ Vector3.create (-hl) (-hw) ht        // 0 top
          Vector3.create   hl  (-hw) ht        // 1 top
          Vector3.create   hl    hw  ht        // 2 top
          Vector3.create (-hl)   hw  ht        // 3 top
          Vector3.create (-hl) (-hw) (-ht)     // 4 bottom
          Vector3.create   hl  (-hw) (-ht)     // 5 bottom
          Vector3.create   hl    hw  (-ht)     // 6 bottom
          Vector3.create (-hl)   hw  (-ht) ]   // 7 bottom

    /// The twelve edges of the plate box as index pairs into `plateCorners3D` (4 top-face, 4
    /// bottom-face, 4 vertical).
    let plateEdges : (int * int) list =
        [ (0, 1); (1, 2); (2, 3); (3, 0)       // top face
          (4, 5); (5, 6); (6, 7); (7, 4)       // bottom face
          (0, 4); (1, 5); (2, 6); (3, 7) ]     // verticals

    /// The four TOP-face corners (z = +T/2) — the surface the elements sit on, used as the
    /// clickable footprint for table selection (§C.3).
    let plateTopFace (table : OpticalTable) : Vector3 list =
        plateCorners3D table |> List.take 4

    /// Whether a screen point lies inside a projected polygon — the even-odd ray cast (the
    /// same rule `ConstructorView.hitTest` uses), closing the polygon by wrapping the last
    /// vertex back to the first.
    let pointInPolygon (poly : ScreenPoint list) (pt : ScreenPoint) : bool =
        match poly with
        | [] | [ _ ] | [ _; _ ] -> false
        | first :: _ ->
            List.pairwise (poly @ [ first ])
            |> List.fold
                (fun inside (a, b) ->
                    if (a.sy > pt.sy) <> (b.sy > pt.sy) then
                        let xAtY = a.sx + (pt.sy - a.sy) * (b.sx - a.sx) / (b.sy - a.sy)
                        if pt.sx < xAtY then not inside else inside
                    else inside)
                false

    /// Whether a screen click hits the table: the point lies on the projected top face of the
    /// plate (§C.3 / AC-C3 — clicking the table selects it). Pure, so the test window's
    /// select / unselect is provable without a real pointer event.
    let tableHit (pixelsPerMeter : float) (center : ScreenPoint) (view : TableViewState) (table : OpticalTable) (pt : ScreenPoint) : bool =
        plateTopFace table
        |> List.map (project pixelsPerMeter center view)
        |> fun face -> pointInPolygon face pt

    /// The inverse of `project` restricted to the table plane (z = 0, where elements sit): map
    /// a screen point back to its table-frame point. `None` when the view is edge-on (the
    /// plane projects to a line, det ≈ 0). Proves the projection round-trips and maps a click
    /// on the plane to a table point.
    let unprojectToTablePlane (pixelsPerMeter : float) (center : ScreenPoint) (view : TableViewState) (pt : ScreenPoint) : TablePoint option =
        let c1, s1 = cos view.r1.value, sin view.r1.value
        let c2, s2 = cos view.r2.value, sin view.r2.value
        let c3, s3 = cos view.r3.value, sin view.r3.value
        // The (x, y, 0) -> (Xr, Yr) screen map is the 2x2 [ [a; b]; [cc; d] ].
        let a = c1 * c3 + s1 * s2 * s3
        let b = -s1 * c3 + c1 * s2 * s3
        let cc = s1 * c2
        let d = c1 * c2
        let det = a * d - b * cc
        if abs det < 1.0e-12 then None
        else
            let s = pixelsPerMeter * view.zoom
            let xr = (pt.sx - center.sx - view.panX) / s
            let yr = -(pt.sy - center.sy - view.panY) / s
            let x = (d * xr - b * yr) / det
            let y = (-cc * xr + a * yr) / det
            Some { x = x * 1.0<meter>; y = y * 1.0<meter> }
