/// §C.5 / §C.6 / §C.7 / §C.2 — the top-down table layout geometry and the schematic
/// drawing weights (Spec 0026 Part C, slice 004). A PURE projection carrying NO
/// Avalonia type (constraint 0.3), exactly like the existing cross-section
/// `Schematic.fs` (Schematic.fs:18) — every rule here is provable headless. It does
/// NOT repurpose or fold into `Schematic.fs` (a different, vertical cross-section
/// view): the top-down surface is net-new (C.0 / C.8 / R-7).
///
/// It owns:
///   * the named drawing-weight constants (C.5.1) so a single edit restyles the whole
///     schematic — central vs side ray weights, the reflected-group opacity, the
///     element frame/interior, and the grid;
///   * the colours (C.5.2) as pure `SchematicColor` values reused from Schematic.fs:35
///     (R-7);
///   * the top-down layout: the grey plate drawn to scale (C.1.3) and the view
///     transform driven by `Table.TableViewState` (C.2.1);
///   * the active-element indicator (C.6.1): ≥ 2 px and ≥ 3:1 WCAG contrast against the
///     plate, proved from the pure `contrastRatio` here (AC-C5);
///   * the show-central-ray-only default (C.7.1, AC-C6) and the per-ray stroke map.
///
/// The FuncUI `Canvas` binding that turns these pure values into Avalonia geometry, and
/// the pan/zoom/rotate/select gestures that drive the view state, are slice 005.
module OpticalConstructor.Ui.ConstructorTable

open Berreman.Constants
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Placement
open OpticalConstructor.Domain.Table
open OpticalConstructor.Ui.Schematic

// ---------------------------------------------------------------------------
// Pure value shapes (no Avalonia type).
// ---------------------------------------------------------------------------

/// A point in top-down drawing space (logical units), produced by projecting a
/// table-frame point through the view transform. Pure floats — the FuncUI binding maps
/// it to an `Avalonia.Point`.
type DrawPoint = { dx : float; dy : float }

/// A pure stroke style — weight (px), opacity (0..1), and colour — the FuncUI binding
/// maps to an `Avalonia.Media.Pen`.
type Stroke = { color : SchematicColor; opacity : float; weightPx : float }

let private rgb (r : int) (g : int) (b : int) : SchematicColor = { red = byte r; green = byte g; blue = byte b }

// ---------------------------------------------------------------------------
// Named drawing-weight constants (C.5.1). There is no formal pixel-weight standard for
// optics ray diagrams, so these concrete defaults are fixed HERE as named constants so
// a single edit restyles the schematic. Central ray = the prominent chief-ray line;
// side rays = lighter marginal-ray analogues; the reflected group reads apart from the
// transmitted group by carrying less opacity.
// ---------------------------------------------------------------------------

/// Central (chief) ray: 2.0 px, full opacity (C.5.1).
let centralRayWeightPx : float = 2.0
let centralRayOpacity : float = 1.0

/// Side (marginal) rays: 1.0 px, ~60% opacity (C.5.1).
let sideRayWeightPx : float = 1.0
let sideRayOpacity : float = 0.6

/// Reflected group: same hue as the transmitted group at ~35% opacity — it carries
/// less energy, so reflected and transmitted read apart at a glance (C.5.1).
let reflectedGroupOpacity : float = 0.35

/// Element cylinder frame: 1.0 px outline; interior fill light-grey at ~15% opacity so
/// rays passing through stay visible (C.4.2 / C.5.1).
let elementFrameWeightPx : float = 1.0
let elementInteriorOpacity : float = 0.15

/// Grid lines (if drawn): 1.0 px at ≤ 25% opacity, so the grid never competes with the
/// geometry (C.5.1).
let gridLineWeightPx : float = 1.0
let gridLineOpacity : float = 0.25

// ---------------------------------------------------------------------------
// Colours (C.5.2), as pure `SchematicColor` values reused from Schematic.fs:35 (R-7).
// ---------------------------------------------------------------------------

/// The grey optical-table plate (C.1.3): a light grey so elements, beams, and the
/// near-black detector all read against it.
let tablePlateColor : SchematicColor = rgb 205 205 205

/// The ray hue (C.5.2): a clear blue shared by the incident/transmitted groups (full
/// strength) and the reflected group (same hue, lower opacity).
let rayColor : SchematicColor = rgb 30 90 200

/// The grid-line colour (C.5.1) — a faint grey kept under the geometry by its opacity.
let gridLineColor : SchematicColor = rgb 120 120 120

/// The element cylinder interior colour (C.4.2 / C.5.1): a very light grey, drawn
/// transparent (`elementInteriorOpacity`) so rays passing through stay visible.
let elementInteriorColor : SchematicColor = rgb 220 220 220

/// The element cylinder frame colour (C.4.2 / C.5.1): a dark grey outline, visible
/// against the grey plate AND against a near-black detector fill so the frame stays
/// selectable.
let elementFrameColor : SchematicColor = rgb 50 50 50

// ---------------------------------------------------------------------------
// Active-element clarity (C.6.1). The active element MUST be unmistakably the active
// one; acting on the wrong element silently is the main hazard. The indicator is ≥ 2 px
// and ≥ 3:1 contrast against the table background (the WCAG focus-indicator floor),
// proved from the pure `contrastRatio` below (AC-C5).
// ---------------------------------------------------------------------------

/// The active-element indicator stroke weight (C.6.1): ≥ 2 px.
let activeIndicatorWeightPx : float = 2.5

/// The active-element indicator colour (C.6.1): a strong, dark blue whose contrast
/// against the grey plate clears the WCAG 3:1 focus floor (asserted via `contrastRatio`).
let activeIndicatorColor : SchematicColor = rgb 0 60 160

/// The sRGB→linear transfer for one 0..1 channel (WCAG 2.x relative-luminance step).
let private linearize (c : float) : float =
    if c <= 0.03928 then c / 12.92 else ((c + 0.055) / 1.055) ** 2.4

/// The WCAG relative luminance of a colour (0 = black … 1 = white).
let relativeLuminance (c : SchematicColor) : float =
    let r = linearize (float c.red / 255.0)
    let g = linearize (float c.green / 255.0)
    let b = linearize (float c.blue / 255.0)
    0.2126 * r + 0.7152 * g + 0.0722 * b

/// The WCAG contrast ratio between two colours (1:1 … 21:1), order-independent. Used to
/// prove the active-element indicator clears the 3:1 focus floor against the plate
/// (AC-C5).
let contrastRatio (a : SchematicColor) (b : SchematicColor) : float =
    let la = relativeLuminance a
    let lb = relativeLuminance b
    let hi = max la lb
    let lo = min la lb
    (hi + 0.05) / (lo + 0.05)

// ---------------------------------------------------------------------------
// Show central ray only / all rays (C.7.1, AC-C6). Most reasoning is on the CR alone;
// the eight side rays are switched on for focusing or dispersion. The default is
// CR-only. The *Trace / View* ribbon toggle that flips it (and the redraw it triggers)
// is wired by slice 006; this slice owns the default state and the redraw-on-change
// geometry (a pure function of this flag).
// ---------------------------------------------------------------------------

/// The show-central-ray-only default (C.7.1, AC-C6): a fresh table shows ONLY the CR.
let showCentralRayOnlyDefault : bool = true

/// The ray groups DRAWN under the CR-only toggle (AC-C6): CR-only → just the central
/// ray; show-all → the central ray plus the eight side rays. The table redraws when the
/// flag changes because the drawn-ray set is a pure function of it.
let drawnSideRayCount (showCentralRayOnly : bool) : int =
    if showCentralRayOnly then 0 else RayModel.sideRayCount

/// The drawn stroke for a ray, by its group and whether it is the central (chief) ray
/// (C.5.1). The central ray is the prominent 2 px / full-opacity line; the side
/// (marginal) rays are 1 px / ~60%; a reflected-group ray keeps the SAME hue but is
/// drawn at ~35% of its would-be opacity (it carries less energy), so reflected and
/// transmitted read apart at a glance.
let rayStroke (group : RayModel.RayGroupKind) (isCentral : bool) : Stroke =
    let baseOpacity = if isCentral then centralRayOpacity else sideRayOpacity
    let weight = if isCentral then centralRayWeightPx else sideRayWeightPx
    let opacity =
        match group with
        | RayModel.Reflected -> baseOpacity * reflectedGroupOpacity
        | RayModel.Incident | RayModel.Transmitted -> baseOpacity
    { color = rayColor; opacity = opacity; weightPx = weight }

// ---------------------------------------------------------------------------
// Top-down layout (C.1.3 / C.2.1). The grey plate is drawn to scale and the view
// transform is driven by the `TableViewState`. This slice implements the straight
// top-down case (scale + Y-flip + pan); the screen-tilt rotations R1/R2/R3 the view
// state carries are applied by the interaction layer (slice 005), and at the default
// `(0,0,0)` they are the identity, so the default view is exactly top-down.
// ---------------------------------------------------------------------------

/// The base scale of the top-down layout (C.1.3): drawing units per canonical meter at
/// zoom = 1. The default 1.2 × 2.0 m plate draws ~360 × 600 units at the default zoom —
/// a sensible "bench fills the canvas" size; the view tests assert the plate is drawn
/// to scale (its drawn length:width ratio equals the physical plate's).
let basePixelsPerMeter : float = 300.0

/// Project a table-frame point (canonical meters) into top-down drawing space under a
/// view state (C.2.1): scale by `basePixelsPerMeter × zoom`, flip Y (screen Y runs
/// down), then pan. At the default top-down view this is the identity orientation.
let project (view : TableViewState) (p : TablePoint) : DrawPoint =
    let s = basePixelsPerMeter * view.zoom
    {
        dx = view.panX + s * (p.x / 1.0<meter>)
        dy = view.panY - s * (p.y / 1.0<meter>)
    }

/// The grey plate's four corners projected into drawing space (C.1.3). The plate is
/// centred at the table-frame origin, its LENGTH along +X (the central-ray axis) and
/// its WIDTH along +Y, so the projected rectangle carries the same length:width ratio
/// as the physical plate — i.e. it is drawn to scale, so elements and beams sit at true
/// relative scale.
let plateCorners (table : OpticalTable) (view : TableViewState) : DrawPoint list =
    let halfL = table.length / 2.0
    let halfW = table.width / 2.0
    [ { x = -halfL; y = -halfW }
      { x =  halfL; y = -halfW }
      { x =  halfL; y =  halfW }
      { x = -halfL; y =  halfW } ]
    |> List.map (project view)
