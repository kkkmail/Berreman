/// A diagnostic test window (Spec 0027, task 010): draw the optical elements through a SWAPPABLE
/// renderer, so we can iron out renderer quirks before the main screen adopts a nicer look. Two
/// renderers, switched at runtime from the top control row:
///   * the WIREFRAME renderer — today's look: the element's projected bounding-box edges plus its
///     N1/N2 normals;
///   * the SHAPE renderer — a round form drawn IN the element's oriented depth×face plane (so a
///     resting element, whose face is perpendicular to the beam, reads edge-on / along the beam, and
///     only opens into a disc as you tip it about R3), coloured by the element's nature and labelled
///     with a short code (S, LP, CP, Sa, L, FM, CM, D).
/// The renderer is a real abstraction (`ElementRenderer`), so only the chosen one draws. Like the other
/// scenes the table is rotatable / pannable / zoomable: the mouse wheel zooms (plain/Ctrl) or rotates
/// the SELECTION — the selected element, else the table view — (Shift/Ctrl+Shift/Alt), and a drag pans.
/// `Model`/`Msg`/`update` are pure and Avalonia-free.
module OpticalConstructor.TestWindows.RendererTestView

open Avalonia
open Avalonia.Controls
open Avalonia.Controls.Shapes
open Avalonia.Input
open Avalonia.Layout
open Avalonia.Media
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open Berreman.Constants
open Berreman.Geometry
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Placement
open OpticalConstructor.Domain.Table
open OpticalConstructor.Domain.TableView

[<RequireQualifiedAccess>]
module UiIds =
    let canvas = "RendererTestCanvas"
    let swapRenderer = "RendererSwapButton"
    let readout = "RendererTestReadout"
    let railsSlider = "RendererRailsSlider"
    let railsReadout = "RendererRailsReadout"
    let capCirclesSlider = "RendererCapCirclesSlider"
    let capRadialsSlider = "RendererCapRadialsSlider"
    let railOpacitySlider = "RendererRailOpacitySlider"
    let faceOpacitySlider = "RendererFaceOpacitySlider"
    let lineOpacitySlider = "RendererLineOpacitySlider"

// ---------------------------------------------------------------------------
// Wheel gesture map (the rotations act on whatever is selected; a plain / Ctrl wheel zooms the table).
// ---------------------------------------------------------------------------

type WheelModifier =
    | ModCtrl
    | ModShift
    | ModAlt

type WheelAction =
    | RotateSel1
    | RotateSel2
    | RotateSel3
    | ZoomTable
    | NoWheelAction

let wheelAction (mods : Set<WheelModifier>) : WheelAction =
    let has m = Set.contains m mods
    match has ModCtrl, has ModShift, has ModAlt with
    | false, true, false -> RotateSel1
    | true, true, false -> RotateSel2
    | false, false, true -> RotateSel3
    | false, false, false -> ZoomTable
    | true, false, false -> ZoomTable
    | _ -> NoWheelAction

// ---------------------------------------------------------------------------
// Pure model.
// ---------------------------------------------------------------------------

/// One drawn element: its spec placement, a per-element visual draw zoom (as in the other test scenes),
/// and a schematic optical sign for lenses / curved mirrors (a bare placement does not carry the signed
/// radius of curvature `CurvedElements` would supply): +1 = converging (convex lens / concave mirror),
/// −1 = diverging (concave lens / convex mirror), 0 = flat (drawn as a cylinder regardless).
type Element =
    {
        placement : ElementPlacement
        zoom : float
        opticalSign : int
    }

/// Which renderer draws the elements — the whole point of this screen is to swap between them.
type RendererKind =
    | Wireframe
    | Shape

type DragState =
    | NotPressed
    | Pressed of ScreenPoint
    | Panning of ScreenPoint

type Model =
    {
        table : OpticalTable
        view : TableViewState
        elements : Element list
        /// The selected element (drawn highlighted), or none. Clicking selects the nearest element.
        selected : int option
        renderer : RendererKind
        drag : DragState
        /// How many (partially transparent) side rails join a cylinder's two caps — a discrete on-screen
        /// choice (`railOptions`, default 72) to tune the look.
        rails : int
        /// Inner latitude circles on a lens / curved-mirror spherical cap (1..8, default 1).
        capCircles : int
        /// Meridian radials on a spherical cap (`radialOptions`, default 4).
        capRadials : int
        /// The three live transparency knobs (0..1) — the rails, the solid faces, the cap lines. The
        /// normals are excluded (always visible).
        railOpacity : float
        faceOpacity : float
        lineOpacity : float
    }

/// The per-frame shape-render configuration (the on-screen "how it looks" knobs), passed to the shape
/// renderer so the draw helpers stay free of the Model.
type ShapeConfig =
    {
        rails : int
        circles : int
        radials : int
        railOpacity : float
        faceOpacity : float
        lineOpacity : float
    }

/// The cylinder side-rail count is DISCRETE — these presets only (task 014). The nicely-looking count
/// depends on the cylinder diameter, so it is a live choice on screen; 72 is the default.
let railOptions : int list = [ 4; 8; 12; 24; 36; 72 ]
let railsMin : int = List.min railOptions
let railsMax : int = List.max railOptions
let defaultRails : int = 72

/// Snap an arbitrary rail count to the nearest allowed preset (so the value is always one of `railOptions`).
let snapRails (n : int) : int = railOptions |> List.minBy (fun o -> abs (o - n))

/// The slider index (0..n-1) of a rail count — the position of its nearest preset in `railOptions`.
let railIndex (n : int) : int = railOptions |> List.findIndex (fun o -> o = snapRails n)

/// The rail count at a slider index, clamped into range.
let railsAtIndex (i : int) : int = List.item (max 0 (min (List.length railOptions - 1) i)) railOptions

/// Inner latitude CIRCLES drawn on a lens / curved-mirror spherical cap — 1 (current default) … 8.
let capCirclesMin : int = 1
let capCirclesMax : int = 8
let defaultCapCircles : int = 1

/// Meridian RADIALS on a spherical cap — discrete presets, 4 the (current) default.
let radialOptions : int list = [ 4; 8; 12; 24; 36 ]
let defaultCapRadials : int = 4
let snapRadials (n : int) : int = radialOptions |> List.minBy (fun o -> abs (o - n))
let radialIndex (n : int) : int = radialOptions |> List.findIndex (fun o -> o = snapRadials n)
let radialsAtIndex (i : int) : int = List.item (max 0 (min (List.length radialOptions - 1) i)) radialOptions

/// The three transparency controls' defaults — the CURRENT look. The yellow / blue normals are NOT
/// affected by these (they are always fully visible). Range is the full 0 (clear) … 1 (opaque).
let defaultRailOpacity : float = 0.35      // the translucent side rails
let defaultFaceOpacity : float = 0.85      // the solid coloured cap / end faces
let defaultLineOpacity : float = 0.70      // the spherical-cap meridians + inner circles
let private clamp01 (x : float) : float = max 0.0 (min 1.0 x)

let defaultElementZoom : float = 5.0

// The canvas geometry / projection are the ONE shared optical-table scene (`TableScene`), so every test
// reuses the same table instead of re-deriving it.
let canvasWidth : float = TableScene.canvasWidth
let canvasHeight : float = TableScene.canvasHeight

let private elementSelectRadiusPx : float = 50.0
let private dragThresholdPx : float = 3.0
let private wheelStepDegrees : float = 5.0

/// One light source, a couple of optical elements, and a detector along the central ray, so the codes
/// and shapes for several kinds are visible at once. R3 is UNLOCKED here so you can tip elements out of
/// the plane and watch the shape renderer open up. The linear polarizer starts selected.
let init () : Model =
    let mk (x : float) (kind : CatalogueKind) (sign : int) : Element =
        { placement = { ElementPlacement.create kind { x = x * 1.0<meter>; y = 0.0<meter> } with r3Locked = false }
          zoom = 3.5
          opticalSign = sign }
    {
        table = Table.defaultTable
        view = Table.defaultView
        // One of each shape family so the renderer can be checked at a glance: a source and a flat
        // polarizer / detector (cylinders), a convex + a concave lens, and a concave (focusing) + a
        // convex curved mirror — spread along the beam.
        elements =
            [ mk -0.9 LightSource 0
              mk -0.6 LinearPolarizer 0
              mk -0.3 Lens 1
              mk 0.0 Lens -1
              mk 0.3 CurvedMirror 1
              mk 0.6 CurvedMirror -1
              mk 0.9 Detector 0 ]
        selected = Some 1
        renderer = Wireframe
        drag = NotPressed
        rails = defaultRails
        capCircles = defaultCapCircles
        capRadials = defaultCapRadials
        railOpacity = defaultRailOpacity
        faceOpacity = defaultFaceOpacity
        lineOpacity = defaultLineOpacity
    }

type Msg =
    /// Swap the active renderer (Wireframe ⇄ Shape).
    | SwapRenderer
    /// Set how many side rails join a cylinder's caps (snapped to the nearest preset).
    | SetRails of int
    /// Set the rail count by its slider PRESET INDEX (0..5 → railOptions).
    | SetRailsIndex of int
    /// Cap detail: inner circles (1..8) and meridian radials (by preset index → radialOptions).
    | SetCapCircles of int
    | SetCapRadials of int
    | SetCapRadialsIndex of int
    /// The three transparency knobs (0..1).
    | SetRailOpacity of float
    | SetFaceOpacity of float
    | SetLineOpacity of float
    | PointerDown of ScreenPoint
    | PointerMove of ScreenPoint
    | PointerUp of ScreenPoint
    | Wheel of Set<WheelModifier> * int

let nextRenderer (r : RendererKind) : RendererKind =
    match r with
    | Wireframe -> Shape
    | Shape -> Wireframe

// ---------------------------------------------------------------------------
// Projection + selection + gestures.
// ---------------------------------------------------------------------------

let private projectPt (view : TableViewState) (p : Vector3) : ScreenPoint = TableScene.project view p

let private dist (a : ScreenPoint) (b : ScreenPoint) : float = sqrt ((a.sx - b.sx) ** 2.0 + (a.sy - b.sy) ** 2.0)

let private elementCentre (view : TableViewState) (e : Element) : ScreenPoint =
    projectPt view (Vector3.create (e.placement.placementPoint.x / 1.0<meter>) (e.placement.placementPoint.y / 1.0<meter>) 0.0)

let private selectNearest (sp : ScreenPoint) (m : Model) : int option =
    m.elements
    |> List.mapi (fun i e -> i, dist sp (elementCentre m.view e))
    |> List.sortBy snd
    |> List.tryHead
    |> Option.bind (fun (i, d) -> if d <= elementSelectRadiusPx then Some i else None)

let private mapElement (i : int) (f : Element -> Element) (m : Model) : Model =
    { m with elements = m.elements |> List.mapi (fun j e -> if j = i then f e else e) }

let private panFrom (refPt : ScreenPoint) (pt : ScreenPoint) (m : Model) : Model =
    { m with
        view = { m.view with panX = m.view.panX + (pt.sx - refPt.sx); panY = m.view.panY + (pt.sy - refPt.sy) }
        drag = Panning pt }

let private zoomBy (notches : int) (m : Model) : Model =
    { m with view = { m.view with zoom = max 0.2 (min 5.0 (m.view.zoom * (1.1 ** float notches))) } }

/// Rotate the SELECTION about axis (1/2/3) by `notches` steps: the selected element, or the table view.
let private rotateSelected (axis : int) (notches : int) (m : Model) : Model =
    let a = Angle.degree (wheelStepDegrees * float notches)
    let rot (r : Angle) = r + a
    match m.selected with
    | Some i ->
        mapElement i (fun e ->
            { e with placement = (match axis with 1 -> withR1 (rot e.placement.r1) | 2 -> withR2 (rot e.placement.r2) | _ -> withR3 (rot e.placement.r3)) e.placement }) m
    | None ->
        let v = m.view
        let v' = match axis with 1 -> { v with r1 = rot v.r1 } | 2 -> { v with r2 = rot v.r2 } | _ -> { v with r3 = rot v.r3 }
        { m with view = v' }

let update (msg : Msg) (model : Model) : Model =
    match msg with
    | SwapRenderer -> { model with renderer = nextRenderer model.renderer }
    | SetRails n -> { model with rails = snapRails n }
    | SetRailsIndex i -> { model with rails = railsAtIndex i }
    | SetCapCircles n -> { model with capCircles = max capCirclesMin (min capCirclesMax n) }
    | SetCapRadials n -> { model with capRadials = snapRadials n }
    | SetCapRadialsIndex i -> { model with capRadials = radialsAtIndex i }
    | SetRailOpacity v -> { model with railOpacity = clamp01 v }
    | SetFaceOpacity v -> { model with faceOpacity = clamp01 v }
    | SetLineOpacity v -> { model with lineOpacity = clamp01 v }
    | PointerDown pt -> { model with drag = Pressed pt }
    | PointerMove pt ->
        match model.drag with
        | NotPressed -> model
        | Pressed start -> if dist start pt < dragThresholdPx then model else panFrom start pt model
        | Panning last -> panFrom last pt model
    | PointerUp _ ->
        // A clean click (no drag) selects the nearest element (or deselects); a pan does not.
        let model' =
            match model.drag with
            | Pressed start -> { model with selected = selectNearest start model }
            | _ -> model
        { model' with drag = NotPressed }
    | Wheel (mods, notches) ->
        match wheelAction mods with
        | RotateSel1 -> rotateSelected 1 notches model
        | RotateSel2 -> rotateSelected 2 notches model
        | RotateSel3 -> rotateSelected 3 notches model
        | ZoomTable -> zoomBy notches model
        | NoWheelAction -> model

// ---------------------------------------------------------------------------
// Element geometry (pure) — the oriented bounding box, same shape as the other test scenes.
// ---------------------------------------------------------------------------

let private elementCorners (e : Element) : Vector3 list =
    let (n1, n2, n3) = orientedBasis e.placement
    let cx = e.placement.placementPoint.x / 1.0<meter>
    let cy = e.placement.placementPoint.y / 1.0<meter>
    let hb = e.zoom * (e.placement.box.b / 2.0 / 1.0<meter>)
    let h1 = e.zoom * (e.placement.box.a1 / 2.0 / 1.0<meter>)
    let h2 = e.zoom * (e.placement.box.a2 / 2.0 / 1.0<meter>)
    let corner (sb : float) (s1 : float) (s2 : float) : Vector3 =
        Vector3.create
            (cx + sb * hb * n1.x + s1 * h1 * n2.x + s2 * h2 * n3.x)
            (cy + sb * hb * n1.y + s1 * h1 * n2.y + s2 * h2 * n3.y)
            (sb * hb * n1.z + s1 * h1 * n2.z + s2 * h2 * n3.z)
    [ corner 1.0 -1.0 -1.0; corner 1.0 -1.0 1.0; corner 1.0 1.0 1.0; corner 1.0 1.0 -1.0
      corner -1.0 -1.0 -1.0; corner -1.0 -1.0 1.0; corner -1.0 1.0 1.0; corner -1.0 1.0 -1.0 ]

// ---------------------------------------------------------------------------
// Colours + drawing primitives.
// ---------------------------------------------------------------------------

let private color (r : int) (g : int) (b : int) : Color = Color.FromRgb(byte r, byte g, byte b)
let private brush (c : Color) : IBrush = SolidColorBrush(c) :> IBrush
let private brushA (a : float) (c : Color) : IBrush =
    SolidColorBrush(Color.FromArgb(byte (255.0 * max 0.0 (min 1.0 a)), c.R, c.G, c.B)) :> IBrush

let private plateColor = color 205 205 205
let private edgeColor = color 50 50 50
let private selectedStroke = color 0 60 160
let private rayColor = color 30 90 200
let private elementColor = color 60 60 60
let private n1Color = color 30 90 200
let private n2Color = color 210 120 0
let private codeColor = color 20 20 30
// The roll-SEAM rail: one rail painted a distinct red, always visible, so the cylinder's spin about R1 is
// readable (a symmetric ring of identical rails has no visible phase). Distinct from blue N1 / yellow N2.
let private seamColor = color 215 45 45

let private toPoint (sp : ScreenPoint) : Point = Point(sp.sx, sp.sy)
let private v3 (x : float) (y : float) (z : float) : Vector3 = Vector3.create x y z

let private line (a : ScreenPoint) (b : ScreenPoint) (c : Color) (w : float) : IView =
    Line.create [
        Line.startPoint (toPoint a); Line.endPoint (toPoint b); Line.stroke (brush c); Line.strokeThickness w
    ] :> IView

/// The fill colour the SHAPE renderer gives each kind — a quick way to read an element's "nature".
let private kindColor (k : CatalogueKind) : Color =
    match k with
    | LightSource -> color 245 200 40
    | LinearPolarizer -> color 70 130 200
    | CircularPolarizer -> color 100 170 200
    | Sample -> color 90 180 110
    | Lens -> color 150 200 230
    | FlatMirror -> color 180 180 190
    | CurvedMirror -> color 150 150 165
    | Detector -> color 60 60 70

// ---------------------------------------------------------------------------
// The swappable renderers.
// ---------------------------------------------------------------------------

/// Draws ONE element. `project` maps a table-frame point to the screen through the live view; `selected`
/// is whether this is the selected element. Returns the FuncUI nodes for that element. Swapping the
/// renderer is the whole point of this screen.
type ElementRenderer =
    {
        name : string
        draw : (Vector3 -> ScreenPoint) -> bool -> Element -> IView list
    }

/// Today's look — the element's projected bounding-box edges plus its N1 (beam) and N2 (roll) normals.
let wireframeRenderer : ElementRenderer =
    {
        name = "Wireframe"
        draw =
            fun project selected e ->
                let corners = elementCorners e |> List.map (project >> toPoint) |> List.toArray
                let boxColor = if selected then selectedStroke else elementColor
                let weight = if selected then 2.5 else 1.0
                let edges =
                    TableView.plateEdges
                    |> List.map (fun (a, b) ->
                        Line.create [
                            Line.startPoint corners.[a]; Line.endPoint corners.[b]; Line.stroke (brush boxColor); Line.strokeThickness weight
                        ] :> IView)
                let (n1, n2, _) = orientedBasis e.placement
                let cx = e.placement.placementPoint.x / 1.0<meter>
                let cy = e.placement.placementPoint.y / 1.0<meter>
                let half = e.zoom * (e.placement.box.a2 / 2.0 / 1.0<meter>)
                let centreScr = project (v3 cx cy 0.0)
                let along (n : Vector3) (len : float) = project (v3 (cx + len * n.x) (cy + len * n.y) (len * n.z))
                edges @ [ line centreScr (along n1 (1.8 * half)) n1Color (if selected then 3.0 else 2.0)
                          line centreScr (along n2 (-(1.3 * half))) n2Color (if selected then 3.0 else 2.0) ]
    }

let private lineP (a : Point) (b : Point) (c : Color) (w : float) : IView =
    Line.create [ Line.startPoint a; Line.endPoint b; Line.stroke (brush c); Line.strokeThickness w ] :> IView

let private strokeOf (selected : bool) : Color = if selected then selectedStroke else edgeColor

/// A table-frame point: the element centre offset by (a·N1, b·N2, c·N3) / (a·N1, c·N3).
let private offset (cx : float) (cy : float) (n1 : Vector3) (n2 : Vector3) (n3 : Vector3) (a : float) (b : float) (c : float) : Vector3 =
    v3 (cx + a * n1.x + b * n2.x + c * n3.x) (cy + a * n1.y + b * n2.y + c * n3.y) (a * n1.z + b * n2.z + c * n3.z)
let private offset2 (cx : float) (cy : float) (n1 : Vector3) (n3 : Vector3) (a : float) (c : float) : Vector3 =
    v3 (cx + a * n1.x + c * n3.x) (cy + a * n1.y + c * n3.y) (a * n1.z + c * n3.z)

/// BOTH oriented normals — N1 (blue, the beam-facing axis) and N2 (yellow, the roll marker) — always
/// shown by both renderers.
let private normalsViews (project : Vector3 -> ScreenPoint) (selected : bool) (e : Element) : IView list =
    let (n1, n2, _) = orientedBasis e.placement
    let cx = e.placement.placementPoint.x / 1.0<meter>
    let cy = e.placement.placementPoint.y / 1.0<meter>
    let half = e.zoom * (e.placement.box.a2 / 2.0 / 1.0<meter>)
    let centre = project (v3 cx cy 0.0)
    let along (n : Vector3) (len : float) = project (v3 (cx + len * n.x) (cy + len * n.y) (len * n.z))
    [ line centre (along n1 (1.8 * half)) n1Color (if selected then 3.0 else 2.0)
      line centre (along n2 (-(1.3 * half))) n2Color (if selected then 3.0 else 2.0) ]

/// A PARTIALLY-TRANSPARENT line (the cylinder side rails).
let private lineT (a : ScreenPoint) (b : ScreenPoint) (c : Color) (alpha : float) (w : float) : IView =
    Line.create [ Line.startPoint (toPoint a); Line.endPoint (toPoint b); Line.stroke (brushA alpha c); Line.strokeThickness w ] :> IView

/// A flat cap circle of radius `r` in the N2–N3 face plane, `d` along N1 (24 points).
let private capPts (project : Vector3 -> ScreenPoint) (n1 : Vector3) (n2 : Vector3) (n3 : Vector3) (cx : float) (cy : float) (d : float) (r : float) : Point list =
    [ for k in 0 .. 23 ->
        let t = 2.0 * System.Math.PI * float k / 24.0
        toPoint (project (offset cx cy n1 n2 n3 d (r * cos t) (r * sin t))) ]

/// The side rails of a cylinder, connecting the rim circle on plane `dA` (along N1) to the rim on plane
/// `dB`. `dA`/`dB` need not be symmetric — a curved mirror joins its (inset) cap rim to its flat back.
/// Rail 0 sits at +N2 (the cap "top", opposite the yellow normal at −N2): it is the always-visible RED
/// roll SEAM, so the spin about R1 is readable — it follows +N2 (which rotates WITH R1), distinct from
/// both normals, where a symmetric ring of identical rails shows no phase. The rest are the translucent
/// rails at the user's `railOpacity`.
let private railViews (project : Vector3 -> ScreenPoint) (n1 : Vector3) (n2 : Vector3) (n3 : Vector3) (cx : float) (cy : float) (dA : float) (dB : float) (r : float) (rails : int) (railOpacity : float) (w : float) : IView list =
    [ for i in 0 .. rails - 1 ->
        let t = 2.0 * System.Math.PI * float i / float rails
        let a = project (offset cx cy n1 n2 n3 dA (r * cos t) (r * sin t))
        let b = project (offset cx cy n1 n2 n3 dB (r * cos t) (r * sin t))
        if i = 0 then lineT a b seamColor 0.95 (w * 1.9)
        else lineT a b edgeColor railOpacity w ]

/// A SPHERICAL cap (a part of a sphere) — what makes a lens / curved mirror a cylinder with curved caps
/// instead of flat circles. The cap spans from the RIM (radius `r` at distance `rimD` along N1) to the
/// APEX (radius 0 at `apexD`); a point at radius ρ sits at the sphere sagitta height
/// `d(ρ) = apexD + (rimD − apexD)·(1 − √(1 − (ρ/r)²))`, so the `radials` meridians are drawn as POLYLINES
/// that follow that curve — genuinely CURVED, not straight. `circles` inner latitude rings are drawn
/// inside the rim. Both `rimD` and `apexD` are within ±halfLen, so nothing bulges out of the box. The
/// solid rim FILL uses `faceOpacity`; the inner circles + meridians use `lineOpacity`.
let private capSurface (project : Vector3 -> ScreenPoint) (n1 : Vector3) (n2 : Vector3) (n3 : Vector3) (cx : float) (cy : float) (r : float) (rimD : float) (apexD : float) (circles : int) (radials : int) (fillC : Color) (strokeC : Color) (faceOpacity : float) (lineOpacity : float) (w : float) : IView list =
    let dAt (rho : float) : float = apexD + (rimD - apexD) * (1.0 - sqrt (max 0.0 (1.0 - (rho / r) ** 2.0)))
    let ringPts (rho : float) : Point list =
        [ for k in 0 .. 23 ->
            let t = 2.0 * System.Math.PI * float k / 24.0
            toPoint (project (offset cx cy n1 n2 n3 (dAt rho) (rho * cos t) (rho * sin t))) ]
    let meridianSteps = 12
    let meridian (t : float) : IView =
        let pts =
            [ for j in 0 .. meridianSteps ->
                let rho = r * (1.0 - float j / float meridianSteps)
                toPoint (project (offset cx cy n1 n2 n3 (dAt rho) (rho * cos t) (rho * sin t))) ]
        Polyline.create [ Polyline.points pts; Polyline.stroke (brushA lineOpacity strokeC); Polyline.strokeThickness (w * 0.6) ] :> IView
    let innerCircle (k : int) : IView =
        let rho = r * float k / float (circles + 1)
        Polygon.create [ Polygon.points (ringPts rho); Polygon.fill (brushA 0.0 fillC); Polygon.stroke (brushA lineOpacity strokeC); Polygon.strokeThickness (w * 0.5) ] :> IView
    [ Polygon.create [ Polygon.points (ringPts r); Polygon.fill (brushA faceOpacity fillC); Polygon.stroke (brush strokeC); Polygon.strokeThickness w ] :> IView ]
    @ [ for k in 1 .. circles -> innerCircle k ]
    @ [ for i in 0 .. radials - 1 -> meridian (2.0 * System.Math.PI * float i / float radials) ]

/// A flat optical element as a 3-D CYLINDER whose axis is N1 (the blue normal): two FLAT end-cap circles
/// joined by the side rails. A source (long along the beam) reads as a rod; a thin polarizer / detector
/// as a coin — edge-on at rest, opening into a disc as it tips.
let private cylinderViews (project : Vector3 -> ScreenPoint) (selected : bool) (cfg : ShapeConfig) (e : Element) : IView list =
    let (n1, n2, n3) = orientedBasis e.placement
    let cx = e.placement.placementPoint.x / 1.0<meter>
    let cy = e.placement.placementPoint.y / 1.0<meter>
    let halfLen = e.zoom * (e.placement.box.b / 2.0 / 1.0<meter>)
    let r = e.zoom * (e.placement.box.a2 / 2.0 / 1.0<meter>)
    let sc = strokeOf selected
    let w = if selected then 2.5 else 1.5
    [ Polygon.create [ Polygon.points (capPts project n1 n2 n3 cx cy (-halfLen) r); Polygon.fill (brushA 0.0 plateColor); Polygon.stroke (brush sc); Polygon.strokeThickness (w * 0.7) ] :> IView ]
    @ railViews project n1 n2 n3 cx cy halfLen (-halfLen) r cfg.rails cfg.railOpacity (w * 0.7)
    @ [ Polygon.create [ Polygon.points (capPts project n1 n2 n3 cx cy halfLen r); Polygon.fill (brushA cfg.faceOpacity (kindColor e.placement.catalogueKind)); Polygon.stroke (brush sc); Polygon.strokeThickness w ] :> IView ]

/// A lens: the same cylinder, but its two caps are SPHERICAL and FIT THE BOUNDING BOX. Biconvex (both
/// apexes pushed out to the box faces ±halfLen, rims inset) for a converging sign; biconcave (rims at the
/// box faces, apexes receding inward) for diverging. Schematic — not a real focal length — but nothing
/// sticks out of the box when the renderer is swapped.
let private lensViews (project : Vector3 -> ScreenPoint) (selected : bool) (cfg : ShapeConfig) (e : Element) : IView list =
    let (n1, n2, n3) = orientedBasis e.placement
    let cx = e.placement.placementPoint.x / 1.0<meter>
    let cy = e.placement.placementPoint.y / 1.0<meter>
    let halfLen = e.zoom * (e.placement.box.b / 2.0 / 1.0<meter>)
    let r = e.zoom * (e.placement.box.a2 / 2.0 / 1.0<meter>)
    let sag = 0.7 * halfLen                            // how deep the caps curve (kept < halfLen → in-box)
    let convex = e.opticalSign >= 0
    // Convex: apex out at the box face (±halfLen), rim inset. Concave: rim at the box face, apex inset.
    let rimD, apexD = if convex then (halfLen - sag, halfLen) else (halfLen, halfLen - sag)
    let sc = strokeOf selected
    let w = if selected then 2.5 else 1.5
    let kc = kindColor Lens
    capSurface project n1 n2 n3 cx cy r rimD apexD cfg.circles cfg.radials kc sc cfg.faceOpacity cfg.lineOpacity w
    @ capSurface project n1 n2 n3 cx cy r (-rimD) (-apexD) cfg.circles cfg.radials kc sc cfg.faceOpacity cfg.lineOpacity w
    @ railViews project n1 n2 n3 cx cy rimD (-rimD) r cfg.rails cfg.railOpacity (w * 0.7)

/// A curved mirror: a cylinder with ONE spherical reflective cap (concave for a converging / focusing
/// sign, convex for diverging) plus a flat back cap. The cap is kept inside the bounding box (its apex /
/// rim never exceed ±halfLen). Schematic only.
let private mirrorViews (project : Vector3 -> ScreenPoint) (selected : bool) (cfg : ShapeConfig) (e : Element) : IView list =
    let (n1, n2, n3) = orientedBasis e.placement
    let cx = e.placement.placementPoint.x / 1.0<meter>
    let cy = e.placement.placementPoint.y / 1.0<meter>
    let halfLen = e.zoom * (e.placement.box.b / 2.0 / 1.0<meter>)
    let r = e.zoom * (e.placement.box.a2 / 2.0 / 1.0<meter>)
    let sag = 0.7 * halfLen
    let concave = e.opticalSign >= 0
    // Concave: rim at the front face, apex receding inward. Convex: apex at the front face, rim inset.
    let rimD, apexD = if concave then (halfLen, halfLen - sag) else (halfLen - sag, halfLen)
    let sc = strokeOf selected
    let w = if selected then 2.5 else 1.5
    let kc = kindColor CurvedMirror
    [ Polygon.create [ Polygon.points (capPts project n1 n2 n3 cx cy (-halfLen) r); Polygon.fill (brushA 0.0 kc); Polygon.stroke (brush sc); Polygon.strokeThickness (w * 0.7) ] :> IView ]
    @ railViews project n1 n2 n3 cx cy rimD (-halfLen) r cfg.rails cfg.railOpacity (w * 0.7)
    @ capSurface project n1 n2 n3 cx cy r rimD apexD cfg.circles cfg.radials kc sc cfg.faceOpacity cfg.lineOpacity w

let private codeLabel (project : Vector3 -> ScreenPoint) (selected : bool) (e : Element) : IView =
    let cx = e.placement.placementPoint.x / 1.0<meter>
    let cy = e.placement.placementPoint.y / 1.0<meter>
    let c = project (v3 cx cy 0.0)
    TextBlock.create [
        TextBlock.left (c.sx + 8.0)
        TextBlock.top (c.sy - 26.0)
        TextBlock.text (TableAndElementRotationView.kindCode e.placement.catalogueKind)
        TextBlock.fontWeight (if selected then FontWeight.Bold else FontWeight.SemiBold)
        TextBlock.foreground (brush codeColor)
    ] :> IView

/// A nicer look — every element is a CYLINDER (axis = the blue N1); flat elements have flat caps, lenses
/// and curved mirrors have SPHERICAL caps per their +/- sign. All the "how it looks" knobs (rail count,
/// cap circles / radials, the three transparency values) come in via the `ShapeConfig`. Always with both
/// normals (never transparency-controlled) and the element code.
let shapeRenderer (cfg : ShapeConfig) : ElementRenderer =
    {
        name = "Shapes + codes"
        draw =
            fun project selected e ->
                let body =
                    match e.placement.catalogueKind with
                    | Lens -> lensViews project selected cfg e
                    | CurvedMirror -> mirrorViews project selected cfg e
                    | _ -> cylinderViews project selected cfg e
                body @ normalsViews project selected e @ [ codeLabel project selected e ]
    }

let rendererOf (kind : RendererKind) (cfg : ShapeConfig) : ElementRenderer =
    match kind with
    | Wireframe -> wireframeRenderer
    | Shape -> shapeRenderer cfg

/// The shape config the model currently dictates (the on-screen knobs).
let shapeConfig (model : Model) : ShapeConfig =
    {
        rails = model.rails
        circles = model.capCircles
        radials = model.capRadials
        railOpacity = model.railOpacity
        faceOpacity = model.faceOpacity
        lineOpacity = model.lineOpacity
    }

// ---------------------------------------------------------------------------
// The FuncUI view.
// ---------------------------------------------------------------------------

/// The table plate (top face + edges) and the central ray, for context — drawn the same regardless of
/// which element renderer is active, via the shared `TableScene`. The plate highlights when the table is
/// the rotation target (nothing is selected).
let private plateViews (model : Model) : IView list =
    TableScene.plateViews model.view model.table (model.selected = None)
    @ TableScene.sourceDetectorRayViews model.view

let private elementsViews (model : Model) : IView list =
    let project = projectPt model.view
    let renderer = rendererOf model.renderer (shapeConfig model)
    model.elements
    |> List.mapi (fun i e -> renderer.draw project (model.selected = Some i) e)
    |> List.concat

let private tableCanvas (model : Model) : IView =
    Canvas.create [
        Canvas.name UiIds.canvas
        Canvas.width canvasWidth
        Canvas.height canvasHeight
        Canvas.horizontalAlignment HorizontalAlignment.Left
        Canvas.verticalAlignment VerticalAlignment.Top
        Canvas.children (plateViews model @ elementsViews model)
    ] :> IView

let private clickBox (id : string) (label : string) (onClick : unit -> unit) : IView =
    Border.create [
        Border.name id
        Border.background (brush (color 232 232 232))
        Border.borderBrush (brush (color 120 120 120))
        Border.borderThickness 1.0
        Border.cornerRadius (CornerRadius 3.0)
        Border.padding (Thickness(10.0, 5.0))
        Border.margin (Thickness(0.0, 0.0, 8.0, 0.0))
        Border.verticalAlignment VerticalAlignment.Center
        Border.child (TextBlock.create [ TextBlock.text label ])
        Border.onPointerPressed ((fun e -> e.Handled <- true; onClick ()), SubPatchOptions.OnChangeOf (box label))
    ] :> IView

let private fixedLabel (text : string) : IView =
    TextBlock.create [ TextBlock.verticalAlignment VerticalAlignment.Center; TextBlock.margin (Thickness(14.0, 0.0, 6.0, 0.0)); TextBlock.text text ] :> IView

let private valueLabel (text : string) : IView =
    TextBlock.create [ TextBlock.verticalAlignment VerticalAlignment.Center; TextBlock.margin (Thickness(6.0, 0.0, 0.0, 0.0)); TextBlock.text text ] :> IView

let private row (children : IView list) : IView =
    StackPanel.create [ StackPanel.orientation Orientation.Horizontal; StackPanel.children children ] :> IView

/// A DISCRETE slider riding the INDEX of a preset list (rails / radials), snapping to whole ticks so it
/// only ever lands on a preset.
let private presetSlider (id : string) (width : float) (options : int list) (value : int) (indexOf : int -> int) (onIndex : int -> unit) : IView =
    Slider.create [
        Slider.name id
        Slider.width width
        Slider.minimum 0.0
        Slider.maximum (float (List.length options - 1))
        Slider.smallChange 1.0
        Slider.largeChange 1.0
        Slider.isSnapToTickEnabled true
        Slider.tickFrequency 1.0
        Slider.tickPlacement Avalonia.Controls.TickPlacement.BottomRight
        Slider.verticalAlignment VerticalAlignment.Center
        Slider.value (float (indexOf value))
        Slider.onValueChanged (fun v -> onIndex (int (System.Math.Round v)))
    ] :> IView

/// An integer slider over a contiguous range (cap circles), snapping to whole ticks.
let private intSlider (id : string) (lo : int) (hi : int) (value : int) (onValue : int -> unit) : IView =
    Slider.create [
        Slider.name id
        Slider.width 120.0
        Slider.minimum (float lo)
        Slider.maximum (float hi)
        Slider.smallChange 1.0
        Slider.largeChange 1.0
        Slider.isSnapToTickEnabled true
        Slider.tickFrequency 1.0
        Slider.tickPlacement Avalonia.Controls.TickPlacement.BottomRight
        Slider.verticalAlignment VerticalAlignment.Center
        Slider.value (float value)
        Slider.onValueChanged (fun v -> onValue (int (System.Math.Round v)))
    ] :> IView

/// A 0 … 1 opacity (transparency) slider.
let private opacitySlider (id : string) (value : float) (onValue : float -> unit) : IView =
    Slider.create [
        Slider.name id
        Slider.width 110.0
        Slider.minimum 0.0
        Slider.maximum 1.0
        Slider.smallChange 0.05
        Slider.verticalAlignment VerticalAlignment.Center
        Slider.value value
        Slider.onValueChanged onValue
    ] :> IView

let private controlBar (model : Model) (dispatch : Msg -> unit) : IView =
    let active = rendererOf model.renderer (shapeConfig model)
    StackPanel.create [
        StackPanel.orientation Orientation.Vertical
        StackPanel.spacing 6.0
        StackPanel.margin (Thickness 8.0)
        StackPanel.children [
            // Row 1 — swap + the cylinder rail count (discrete presets 4/8/12/24/36/72).
            row [
                clickBox UiIds.swapRenderer "Swap renderer" (fun () -> dispatch SwapRenderer)
                TextBlock.create [ TextBlock.name UiIds.readout; TextBlock.verticalAlignment VerticalAlignment.Center; TextBlock.text (sprintf "Renderer: %s" active.name) ]
                fixedLabel "Cylinder rails:"
                presetSlider UiIds.railsSlider 170.0 railOptions model.rails railIndex (fun i -> dispatch (SetRailsIndex i))
                TextBlock.create [ TextBlock.name UiIds.railsReadout; TextBlock.verticalAlignment VerticalAlignment.Center; TextBlock.margin (Thickness(6.0, 0.0, 0.0, 0.0)); TextBlock.text (string model.rails) ]
            ]
            // Row 2 — lens / curved-mirror cap detail: inner circles (1..8) and meridian radials (4/8/12/24/36).
            row [
                fixedLabel "Cap circles:"
                intSlider UiIds.capCirclesSlider capCirclesMin capCirclesMax model.capCircles (fun n -> dispatch (SetCapCircles n))
                valueLabel (string model.capCircles)
                fixedLabel "Cap radials:"
                presetSlider UiIds.capRadialsSlider 150.0 radialOptions model.capRadials radialIndex (fun i -> dispatch (SetCapRadialsIndex i))
                valueLabel (string model.capRadials)
            ]
            // Row 3 — the three transparency knobs (the yellow/blue normals stay fully visible).
            row [
                fixedLabel "Rail opacity:"
                opacitySlider UiIds.railOpacitySlider model.railOpacity (fun v -> dispatch (SetRailOpacity v))
                valueLabel (sprintf "%.2f" model.railOpacity)
                fixedLabel "Face opacity:"
                opacitySlider UiIds.faceOpacitySlider model.faceOpacity (fun v -> dispatch (SetFaceOpacity v))
                valueLabel (sprintf "%.2f" model.faceOpacity)
                fixedLabel "Line opacity:"
                opacitySlider UiIds.lineOpacitySlider model.lineOpacity (fun v -> dispatch (SetLineOpacity v))
                valueLabel (sprintf "%.2f" model.lineOpacity)
            ]
            TextBlock.create [
                TextBlock.foreground (brush (color 100 100 100))
                TextBlock.text "click an element to select it · drag = pan · wheel = zoom · Shift/Ctrl+Shift/Alt+wheel = R1/R2/R3 of the selection · the RED seam rail tracks the roll about R1 · L/CM caps are spherical (+/- sign); tune circles / radials / transparency above"
            ]
        ]
    ] :> IView

let view (model : Model) (dispatch : Msg -> unit) : IView =
    let toScreen (e : PointerEventArgs) : ScreenPoint = SceneInput.canvasPoint UiIds.canvas e
    let wheelMods (km : KeyModifiers) : Set<WheelModifier> =
        [ if km.HasFlag KeyModifiers.Control then ModCtrl
          if km.HasFlag KeyModifiers.Shift then ModShift
          if km.HasFlag KeyModifiers.Alt then ModAlt ]
        |> Set.ofList
    DockPanel.create [
        DockPanel.children [
            Border.create [ Border.dock Dock.Top; Border.child (controlBar model dispatch) ]
            Border.create [
                Border.background (brush (color 250 250 250))
                Border.onPointerPressed (fun e -> e.Handled <- true; dispatch (PointerDown (toScreen e)))
                Border.onPointerMoved (fun e -> e.Handled <- true; dispatch (PointerMove (toScreen e)))
                Border.onPointerReleased (fun e -> e.Handled <- true; dispatch (PointerUp (toScreen e)))
                Border.onPointerWheelChanged (fun e ->
                    e.Handled <- true
                    dispatch (Wheel (wheelMods e.KeyModifiers, (if e.Delta.Y >= 0.0 then 1 else -1))))
                Border.child (tableCanvas model)
            ]
        ]
    ] :> IView
