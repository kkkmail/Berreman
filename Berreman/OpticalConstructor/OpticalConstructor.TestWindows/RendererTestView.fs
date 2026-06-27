/// A diagnostic test window (Spec 0027, task 010): draw the optical elements through a SWAPPABLE
/// renderer, so we can iron out renderer quirks before the main screen adopts a nicer look. Two
/// renderers, switched at runtime from the top control row:
///   * the WIREFRAME renderer — today's look: the element's projected bounding-box edges plus its
///     N1/N2 normals;
///   * the SHAPE renderer — round forms whose colour suggests the element's nature, each labelled with
///     a short element code (S, LP, CP, Sa, L, FM, CM, D) drawn beside it.
/// The renderer is a real abstraction (`ElementRenderer`), so only the chosen one draws; this is the
/// only screen that exposes the swap for now. `Model`/`Msg`/`update` are pure and Avalonia-free.
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
// Pure model.
// ---------------------------------------------------------------------------

/// One drawn element: its spec placement + a per-element visual draw zoom (as in the other test scenes).
type Element =
    {
        placement : ElementPlacement
        zoom : float
    }

/// Which renderer draws the elements — the whole point of this screen is to swap between them.
type RendererKind =
    | Wireframe
    | Shape

type Model =
    {
        table : OpticalTable
        view : TableViewState
        elements : Element list
        /// The selected element (drawn with the selected styling), or none. Click an element to select.
        selected : int option
        renderer : RendererKind
    }

let defaultElementZoom : float = 5.0

/// One light source, a couple of optical elements, and a detector along the central ray, so the codes
/// and shapes for several kinds are visible at once. The linear polarizer starts selected.
let init () : Model =
    let mk (x : float) (kind : CatalogueKind) : Element =
        { placement = ElementPlacement.create kind { x = x * 1.0<meter>; y = 0.0<meter> }; zoom = defaultElementZoom }
    {
        table = Table.defaultTable
        view = Table.defaultView
        elements =
            [ mk -0.8 LightSource
              mk -0.4 LinearPolarizer
              mk 0.0 Sample
              mk 0.4 FlatMirror
              mk 0.8 Detector ]
        selected = Some 1
        renderer = Wireframe
    }

type Msg =
    /// Swap the active renderer (Wireframe ⇄ Shape).
    | SwapRenderer
    /// Select the element nearest the click (or deselect if the click is far from every element).
    | SelectAt of ScreenPoint

let nextRenderer (r : RendererKind) : RendererKind =
    match r with
    | Wireframe -> Shape
    | Shape -> Wireframe

// ---------------------------------------------------------------------------
// Constants + projection.
// ---------------------------------------------------------------------------

[<Literal>]
let canvasWidth = 820.0
[<Literal>]
let canvasHeight = 560.0

let private center : ScreenPoint = { sx = canvasWidth / 2.0; sy = canvasHeight / 2.0 }
let private pixelsPerMeter : float = 200.0
let private elementSelectRadiusPx : float = 50.0

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

let update (msg : Msg) (model : Model) : Model =
    match msg with
    | SwapRenderer -> { model with renderer = nextRenderer model.renderer }
    | SelectAt sp -> { model with selected = selectNearest sp model }

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

/// A nicer look — a round form coloured by the element's nature, labelled with its short code.
let shapeRenderer : ElementRenderer =
    {
        name = "Shapes + codes"
        draw =
            fun project selected e ->
                let kind = e.placement.catalogueKind
                let cx = e.placement.placementPoint.x / 1.0<meter>
                let cy = e.placement.placementPoint.y / 1.0<meter>
                let centreScr = project (v3 cx cy 0.0)
                let halfMetres = e.zoom * (e.placement.box.a2 / 2.0 / 1.0<meter>)
                let radius = max 9.0 (dist centreScr (project (v3 (cx + halfMetres) cy 0.0)))
                let stroke = if selected then selectedStroke else edgeColor
                let weight = if selected then 3.0 else 1.5
                let shape =
                    Ellipse.create [
                        Ellipse.left (centreScr.sx - radius)
                        Ellipse.top (centreScr.sy - radius)
                        Ellipse.width (2.0 * radius)
                        Ellipse.height (2.0 * radius)
                        Ellipse.fill (brushA 0.85 (kindColor kind))
                        Ellipse.stroke (brush stroke)
                        Ellipse.strokeThickness weight
                    ] :> IView
                let label =
                    TextBlock.create [
                        TextBlock.left (centreScr.sx + radius + 3.0)
                        TextBlock.top (centreScr.sy - radius - 2.0)
                        TextBlock.text (TableAndElementRotationView.kindCode kind)
                        TextBlock.fontWeight (if selected then FontWeight.Bold else FontWeight.SemiBold)
                        TextBlock.foreground (brush codeColor)
                    ] :> IView
                [ shape; label ]
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
                TextBlock.text "click an element to select it · Swap renderer toggles wireframe ⇄ shapes+codes (S / LP / CP / Sa / L / FM / CM / D)"
            ]
        ]
    ] :> IView

let view (model : Model) (dispatch : Msg -> unit) : IView =
    let toScreen (e : PointerEventArgs) : ScreenPoint =
        let p = e.GetPosition null
        { sx = p.X; sy = p.Y }
    DockPanel.create [
        DockPanel.children [
            Border.create [ Border.dock Dock.Top; Border.child (controlBar model dispatch) ]
            Border.create [
                Border.background (brush (color 250 250 250))
                Border.onPointerPressed (fun e -> e.Handled <- true; dispatch (SelectAt (toScreen e)))
                Border.child (tableCanvas model)
            ]
        ]
    ] :> IView
