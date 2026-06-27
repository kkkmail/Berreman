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
    }

let defaultElementZoom : float = 5.0

[<Literal>]
let canvasWidth = 820.0
[<Literal>]
let canvasHeight = 560.0

let private center : ScreenPoint = { sx = canvasWidth / 2.0; sy = canvasHeight / 2.0 }
let private pixelsPerMeter : float = 200.0
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
    }

type Msg =
    /// Swap the active renderer (Wireframe ⇄ Shape).
    | SwapRenderer
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

let private projectPt (view : TableViewState) (p : Vector3) : ScreenPoint = TableView.project pixelsPerMeter center view p

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

/// A flat optical element as a 3-D CYLINDER whose axis is N1 (the blue normal): two end-cap circles in
/// the N2–N3 face plane joined by side rails. A source (long along the beam) reads as a rod; a thin
/// polarizer / detector as a coin — edge-on at rest, opening into a disc as it tips.
let private cylinderViews (project : Vector3 -> ScreenPoint) (selected : bool) (e : Element) : IView list =
    let (n1, n2, n3) = orientedBasis e.placement
    let cx = e.placement.placementPoint.x / 1.0<meter>
    let cy = e.placement.placementPoint.y / 1.0<meter>
    let halfLen = e.zoom * (e.placement.box.b / 2.0 / 1.0<meter>)   // along N1 — the cylinder length
    let r = e.zoom * (e.placement.box.a2 / 2.0 / 1.0<meter>)        // cap radius, in the N2–N3 face plane
    let capPts (d : float) : Point list =
        [ for k in 0 .. 23 ->
            let t = 2.0 * System.Math.PI * float k / 24.0
            toPoint (project (offset cx cy n1 n2 n3 d (r * cos t) (r * sin t))) ]
    let capA = capPts halfLen
    let capB = capPts (-halfLen)
    let sc = strokeOf selected
    let w = if selected then 2.5 else 1.5
    [ Polygon.create [ Polygon.points capB; Polygon.fill (brushA 0.0 plateColor); Polygon.stroke (brush sc); Polygon.strokeThickness (w * 0.7) ] :> IView ]
    @ [ for k in [ 0; 6; 12; 18 ] -> lineP capA.[k] capB.[k] sc (w * 0.7) ]
    @ [ Polygon.create [ Polygon.points capA; Polygon.fill (brushA 0.85 (kindColor e.placement.catalogueKind)); Polygon.stroke (brush sc); Polygon.strokeThickness w ] :> IView ]

/// A lens drawn schematically in the N1–N3 plane: biconvex (fat centre) for a converging sign, biconcave
/// (pinched centre) for diverging. Schematic — NOT curved to the real focal length.
let private lensViews (project : Vector3 -> ScreenPoint) (selected : bool) (e : Element) : IView list =
    let (n1, _, n3) = orientedBasis e.placement
    let cx = e.placement.placementPoint.x / 1.0<meter>
    let cy = e.placement.placementPoint.y / 1.0<meter>
    let apHalf = e.zoom * (e.placement.box.a2 / 2.0 / 1.0<meter>)
    let amp = 0.5 * apHalf
    let s (w : float) : float = sqrt (max 0.0 (1.0 - (w / apHalf) ** 2.0))
    let face (front : bool) (w : float) : float =
        let sign = if front then 1.0 else -1.0
        if e.opticalSign >= 0 then sign * amp * s w                  // convex vesica — fat centre
        else sign * (amp - 0.75 * amp * s w)                         // concave faces — pinched centre
    let steps = 18
    let pts (front : bool) (ks : int list) = [ for k in ks -> let w = -apHalf + 2.0 * apHalf * float k / float steps in toPoint (project (offset2 cx cy n1 n3 (face front w) w)) ]
    let outline = pts true [ 0 .. steps ] @ pts false [ steps .. -1 .. 0 ]
    [ Polygon.create [
        Polygon.points outline
        Polygon.fill (brushA 0.7 (kindColor Lens))
        Polygon.stroke (brush (strokeOf selected))
        Polygon.strokeThickness (if selected then 2.5 else 1.5)
      ] :> IView ]

/// A curved mirror drawn schematically in the N1–N3 plane: a concave (focusing) arc for a converging
/// sign, convex for diverging, with a thin backing arc so it reads as a mirror. Schematic only.
let private mirrorViews (project : Vector3 -> ScreenPoint) (selected : bool) (e : Element) : IView list =
    let (n1, _, n3) = orientedBasis e.placement
    let cx = e.placement.placementPoint.x / 1.0<meter>
    let cy = e.placement.placementPoint.y / 1.0<meter>
    let apHalf = e.zoom * (e.placement.box.a2 / 2.0 / 1.0<meter>)
    let amp = 0.45 * apHalf
    let backOff = 0.18 * apHalf
    let s (w : float) : float = sqrt (max 0.0 (1.0 - (w / apHalf) ** 2.0))
    let dir = if e.opticalSign >= 0 then -1.0 else 1.0    // concave bulges toward −N1, convex toward +N1
    let steps = 20
    let ws = [ for k in 0 .. steps -> -apHalf + 2.0 * apHalf * float k / float steps ]
    let surf = ws |> List.map (fun w -> toPoint (project (offset2 cx cy n1 n3 (dir * amp * s w) w)))
    let back = ws |> List.map (fun w -> toPoint (project (offset2 cx cy n1 n3 (dir * (amp * s w + backOff)) w)))
    let sc = strokeOf selected
    (surf |> List.pairwise |> List.map (fun (a, b) -> lineP a b (kindColor CurvedMirror) (if selected then 3.5 else 2.5)))
    @ (back |> List.pairwise |> List.map (fun (a, b) -> lineP a b sc 1.0))

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

/// A nicer look — flat optical elements as cylinders (axis = the blue N1), lenses / curved mirrors as
/// their +/- schematic — ALWAYS with both normals and the element code.
let shapeRenderer : ElementRenderer =
    {
        name = "Shapes + codes"
        draw =
            fun project selected e ->
                let body =
                    match e.placement.catalogueKind with
                    | Lens -> lensViews project selected e
                    | CurvedMirror -> mirrorViews project selected e
                    | _ -> cylinderViews project selected e
                body @ normalsViews project selected e @ [ codeLabel project selected e ]
    }

let rendererOf (kind : RendererKind) : ElementRenderer =
    match kind with
    | Wireframe -> wireframeRenderer
    | Shape -> shapeRenderer

// ---------------------------------------------------------------------------
// The FuncUI view.
// ---------------------------------------------------------------------------

/// The table plate (top face + edges) and the central ray, for context — drawn the same regardless of
/// which element renderer is active.
let private plateViews (model : Model) : IView list =
    let project = projectPt model.view
    let corners = TableView.plateCorners3D model.table |> List.map project |> List.toArray
    let topFace = corners.[0 .. 3] |> Array.map toPoint |> Array.toList
    let face =
        Polygon.create [
            Polygon.points topFace
            Polygon.fill (brushA 0.85 plateColor)
            Polygon.stroke (brush edgeColor)
            Polygon.strokeThickness 1.5
        ] :> IView
    let edges = TableView.plateEdges |> List.map (fun (a, b) -> line corners.[a] corners.[b] edgeColor 1.0)
    let s = project (RayModel.pointToVector3 RayModel.defaultSourcePoint)
    let d = project (RayModel.pointToVector3 RayModel.defaultDetectorPoint)
    (face :: edges) @ [ line s d rayColor 2.0 ]

let private elementsViews (model : Model) : IView list =
    let project = projectPt model.view
    let renderer = rendererOf model.renderer
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

let private controlBar (model : Model) (dispatch : Msg -> unit) : IView =
    let active = rendererOf model.renderer
    StackPanel.create [
        StackPanel.orientation Orientation.Vertical
        StackPanel.spacing 6.0
        StackPanel.margin (Thickness 8.0)
        StackPanel.children [
            StackPanel.create [
                StackPanel.orientation Orientation.Horizontal
                StackPanel.children [
                    clickBox UiIds.swapRenderer "Swap renderer" (fun () -> dispatch SwapRenderer)
                    TextBlock.create [
                        TextBlock.name UiIds.readout
                        TextBlock.verticalAlignment VerticalAlignment.Center
                        TextBlock.text (sprintf "Renderer: %s" active.name)
                    ]
                ]
            ]
            TextBlock.create [
                TextBlock.foreground (brush (color 100 100 100))
                TextBlock.text "click an element to select it · drag = pan · wheel = zoom · Shift/Ctrl+Shift/Alt+wheel = R1/R2/R3 of the selection (Alt+wheel tips it open) · Swap renderer toggles wireframe ⇄ shapes+codes (S/LP/CP/Sa/L/FM/CM/D)"
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
