/// §C.4 — the standard element drawer (Spec 0026 Part C, slice 004). A PURE function
/// (constraint 0.3): its inputs are the element's placement point, its three rotations,
/// its element KIND, and its element VALUE ID (`None` for the first build), and it
/// returns pure top-down draw geometry in table-frame meters — NO Avalonia type.
/// Passing the kind and the value id explicitly from the start is mandatory (C.4.1) so
/// per-role and (later) per-device/material looks can be added without rewiring
/// placement or ray logic.
///
/// For the first build there is exactly ONE standard drawer used by every element
/// (C.4.2): it draws the element as a CYLINDER — a visible frame around a very
/// light-grey, transparent interior so rays passing through stay visible. Two
/// role-based shadings depart from that default: the light source is darker; the
/// detector is almost black but keeps a visible frame so it can be seen and selected.
/// A show/hide bounding-box option (off by default, C.4.3) adds the twelve projected
/// box edges, making the element's R1/R2/R3 visible at a glance.
///
/// The rotations reuse `Placement.orientedBasis` (slice 001) — no rotation math is
/// re-derived here (constraint 0.1). The frame weight, interior colour/opacity, and the
/// frame colour are the named constants in `ConstructorTable.fs` (C.5.1), so a single
/// edit restyles the schematic. The view transform (zoom/pan/screen-tilt) is applied
/// later by `ConstructorTable.project` / slice 005; the drawer is view-independent.
module OpticalConstructor.Ui.Drawer

open Berreman.Constants
open Berreman.Geometry
open OpticalConstructor.Domain.Placement
open OpticalConstructor.Ui.Schematic

let private rgb (r : int) (g : int) (b : int) : SchematicColor = { red = byte r; green = byte g; blue = byte b }

let private addv (a : Vector3) (b : Vector3) : Vector3 = Vector3.create (a.x + b.x) (a.y + b.y) (a.z + b.z)
let private scalev (s : float) (a : Vector3) : Vector3 = Vector3.create (s * a.x) (s * a.y) (s * a.z)
let private toTablePoint (v : Vector3) : TablePoint = { x = v.x * 1.0<meter>; y = v.y * 1.0<meter> }

/// The interior fill of a drawn element (C.4.2): a colour plus an opacity. The default
/// is a very light grey at low opacity (transparent); the source is darker; the
/// detector is near-black.
type Fill = { color : SchematicColor; opacity : float }

/// The default cylinder interior (C.4.2 / C.5.1): very light grey, ~15% opacity, so
/// rays passing through remain visible.
let defaultInterior : Fill =
    { color = ConstructorTable.elementInteriorColor; opacity = ConstructorTable.elementInteriorOpacity }

/// The light-source shade (C.4.2): a darker grey, more opaque than the default.
let sourceShade : Fill = { color = rgb 90 90 90; opacity = 0.55 }

/// The detector shade (C.4.2): almost black and nearly opaque — but the frame is still
/// drawn (see `frameStroke`) so the detector can be seen and selected.
let detectorShade : Fill = { color = rgb 15 15 15; opacity = 0.90 }

/// The interior shade for a catalogue role (C.4.2). One standard cylinder for every
/// element, with two role-based departures: the source darker, the detector near-black.
let shadeFor (kind : CatalogueKind) : Fill =
    match kind with
    | LightSource -> sourceShade
    | Detector -> detectorShade
    | LinearPolarizer | CircularPolarizer | Sample | Lens | FlatMirror | CurvedMirror -> defaultInterior

/// The cylinder frame stroke (C.4.2 / C.5.1): a 1 px dark-grey outline at full opacity,
/// the SAME for every element — so even the near-black detector keeps a visible,
/// selectable frame against the grey plate.
let frameStroke : ConstructorTable.Stroke =
    { color = ConstructorTable.elementFrameColor; opacity = 1.0; weightPx = ConstructorTable.elementFrameWeightPx }

/// The show/hide bounding-box default (C.4.3): OFF. When on, the box edges make
/// R1/R2/R3 visible at a glance for inspecting an element's orientation.
let showBoundingBoxDefault : bool = false

/// The pure top-down draw geometry of one element drawn as a cylinder (C.4). All points
/// are table-frame canonical meters (`TablePoint`); the view transform is applied later
/// (slice 005). `frame` is the closed cylinder-body silhouette; `axisStart`/`axisEnd`
/// are the two cap centres and `capRadius` the cap radius, so the binding can draw the
/// rounded ends; `fill` is the role shade; `frameStroke` is the always-visible outline;
/// `boundingBoxEdges` is empty unless show-box is on, then the twelve projected box
/// edges.
type DrawerGeometry =
    {
        frame : TablePoint list
        axisStart : TablePoint
        axisEnd : TablePoint
        capRadius : float<meter>
        fill : Fill
        frameStroke : ConstructorTable.Stroke
        boundingBoxEdges : (TablePoint * TablePoint) list
    }

/// The ONE standard cylinder drawer (C.4.1 / C.4.2 / C.4.3). Inputs: the placement
/// point, the three rotations, the element kind, the element value id (`None` for the
/// first build), and whether to draw the bounding box. Returns pure top-down geometry.
///
/// The element box (`A × A × B`, the face perpendicular to the primary normal N1, depth
/// B along N1) is oriented by reusing `Placement.orientedBasis`. The cylinder is drawn
/// along N1 with its circular cross-section of radius `A/2`, so its silhouette is a
/// rectangle of length = the projected axis and width = 2·radius, independent of the R1
/// spin (a cylinder looks the same spun about its axis). The square box, in contrast,
/// reveals R1/R2/R3 — which is why the optional box edges are the orientation tell.
let draw
    (point : TablePoint)
    (r1 : Angle)
    (r2 : Angle)
    (r3 : Angle)
    (kind : CatalogueKind)
    (valueId : string option)
    (showBoundingBox : bool)
    : DrawerGeometry =
    // Reconstruct the placement so the slice-001 rotation law is reused, not re-derived.
    // The angles are set directly (the drawer renders whatever orientation it is given,
    // regardless of the editing locks); the value id is carried so future per-device
    // looks can branch on it without changing this signature.
    let p0 = ElementPlacement.create kind point
    let p = { p0 with r1 = r1; r2 = r2; r3 = r3; valueId = valueId }
    let (n1, n2, n3) = orientedBasis p

    let centerV = Vector3.create (point.x / 1.0<meter>) (point.y / 1.0<meter>) 0.0
    let halfB = (p.box.b / 2.0) / 1.0<meter>
    let halfA1 = (p.box.a1 / 2.0) / 1.0<meter>
    let halfA2 = (p.box.a2 / 2.0) / 1.0<meter>

    // --- Cylinder silhouette + cap axis (along N1, radius = face half-extent) --------
    let capPlus = addv centerV (scalev halfB n1)
    let capMinus = addv centerV (scalev -halfB n1)
    let axx = capPlus.x - capMinus.x
    let axy = capPlus.y - capMinus.y
    let axisLen2D = sqrt (axx * axx + axy * axy)
    // The in-plane perpendicular to the projected axis (the silhouette half-width
    // direction). When the axis projects to ~0 (the cylinder seen end-on, e.g. R3 ≈ 90°)
    // fall back to +X so the silhouette is still a well-formed (square) outline.
    let (px, py) = if axisLen2D < 1.0e-9 then (0.0, 1.0) else (-axy / axisLen2D, axx / axisLen2D)
    let r = halfA1
    let edgePoint (cap : Vector3) (sign : float) : TablePoint =
        { x = (cap.x + sign * r * px) * 1.0<meter>; y = (cap.y + sign * r * py) * 1.0<meter> }
    let frame =
        [ edgePoint capMinus 1.0
          edgePoint capPlus 1.0
          edgePoint capPlus -1.0
          edgePoint capMinus -1.0
          edgePoint capMinus 1.0 ]

    // --- Bounding-box edges (off by default; the 12 projected edges when on) ----------
    let cornerAt (sa : int) (sb : int) (sc : int) : TablePoint =
        let f i = if i = 1 then 1.0 else -1.0
        addv centerV (addv (scalev (f sa * halfB) n1) (addv (scalev (f sb * halfA1) n2) (scalev (f sc * halfA2) n3)))
        |> toTablePoint
    let boundingBoxEdges =
        if not showBoundingBox then []
        else
            [ for sb in [ 0; 1 ] do
                for sc in [ 0; 1 ] do
                    yield (cornerAt 0 sb sc, cornerAt 1 sb sc)        // 4 edges along N1
              for sa in [ 0; 1 ] do
                for sc in [ 0; 1 ] do
                    yield (cornerAt sa 0 sc, cornerAt sa 1 sc)        // 4 edges along N2
              for sa in [ 0; 1 ] do
                for sb in [ 0; 1 ] do
                    yield (cornerAt sa sb 0, cornerAt sa sb 1) ]      // 4 edges along N3

    {
        frame = frame
        axisStart = toTablePoint capMinus
        axisEnd = toTablePoint capPlus
        capRadius = r * 1.0<meter>
        fill = shadeFor kind
        frameStroke = frameStroke
        boundingBoxEdges = boundingBoxEdges
    }
