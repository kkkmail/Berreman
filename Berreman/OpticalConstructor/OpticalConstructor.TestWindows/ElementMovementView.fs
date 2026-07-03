/// The element-movement test window (Spec 0027, task 010): a SINGLE element in the middle of the
/// table that the user slides along the central ray, to work out the movement quirks before the main
/// screen supports it. Movement follows the spec's "slide along the ray" gesture (§E.4): a left-drag
/// on the element slides it along the beam, and ←/→ nudge it (Shift = a larger step). The view is a
/// fixed top-down projection (movement, not rotation, is the point here), so the screen→table inverse
/// is the straight `(sx - centre)/pixelsPerMeter`. The `Model`/`Msg`/`update` are pure and Avalonia-free.
module OpticalConstructor.TestWindows.ElementMovementView

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
open OpticalConstructor.Controls

[<RequireQualifiedAccess>]
module UiIds =
    let canvas = "ElementMovementCanvas"

// The canvas geometry / projection are the ONE shared optical-table scene (`TableScene`).
let canvasWidth : float = TableScene.canvasWidth
let canvasHeight : float = TableScene.canvasHeight
let private drawZoom : float = 5.0
let stepMeters : float = RayPositionControls.stepMeters false
let bigStepMeters : float = RayPositionControls.stepMeters true
let private selectRadiusPx : float = 70.0

type Model =
    {
        table : OpticalTable
        view : TableViewState        // fixed top-down (movement test, not rotation)
        element : ElementPlacement   // the single element the test slides
        dragging : bool              // true while a left-drag on the element is in progress
    }

/// A single sample element in the MIDDLE of the table — the position the test slides.
let init () : Model =
    {
        table = Table.defaultTable
        view = Table.defaultView
        element = ElementPlacement.create Sample { x = 0.0<meter>; y = 0.0<meter> }
        dragging = false
    }

type Msg =
    | SlideTo of float        // set the element's along-beam position x (table metres)
    | SlideBy of float        // nudge x by a delta (the arrow keys)
    | BeginDrag of ScreenPoint
    | DragTo of ScreenPoint
    | EndDrag
    | ResetPosition

let private halfLength (m : Model) : float = (m.table.length / 2.0) / 1.0<meter>
let private clampX (m : Model) (x : float) : float = max (-(halfLength m)) (min (halfLength m) x)
let elementX (m : Model) : float = m.element.placementPoint.x / 1.0<meter>

/// Move the element along the beam (its x), clamped to the plate; y stays 0 (it rides the central ray).
let private setX (x : float) (m : Model) : Model =
    { m with element = { m.element with placementPoint = { m.element.placementPoint with x = clampX m x * 1.0<meter> } } }

/// Screen x → table x for the fixed top-down view (no pan / zoom): the inverse of `TableScene.project`.
let private tableXOfScreen (sx : float) : float = (sx - TableScene.center.sx) / TableScene.pixelsPerMeter

let private elementCentreScreen (m : Model) : ScreenPoint =
    TableScene.project m.view (Vector3.create (elementX m) 0.0 0.0)

let private dist (a : ScreenPoint) (b : ScreenPoint) : float = sqrt ((a.sx - b.sx) ** 2.0 + (a.sy - b.sy) ** 2.0)

let update (msg : Msg) (model : Model) : Model =
    match msg with
    | SlideTo x -> setX x model
    | SlideBy dx -> setX (elementX model + dx) model
    // A press starts a drag ONLY when it lands on the element; then the element follows the pointer's x.
    | BeginDrag sp -> if dist sp (elementCentreScreen model) <= selectRadiusPx then { model with dragging = true } else model
    | DragTo sp -> if model.dragging then setX (tableXOfScreen sp.sx) model else model
    | EndDrag -> { model with dragging = false }
    | ResetPosition -> setX 0.0 model

// ---------------------------------------------------------------------------
// Colours + the FuncUI view.
// ---------------------------------------------------------------------------

let private color (r : int) (g : int) (b : int) : Color = Color.FromRgb(byte r, byte g, byte b)
let private brush (c : Color) : IBrush = SolidColorBrush(c) :> IBrush
let private brushA (a : float) (c : Color) : IBrush =
    SolidColorBrush(Color.FromArgb(byte (255.0 * max 0.0 (min 1.0 a)), c.R, c.G, c.B)) :> IBrush

let private plateColor = color 205 205 205
let private edgeColor = color 50 50 50
let private rayColor = color 30 90 200
let private sourceColor = color 70 70 70
let private detectorColor = color 20 20 20
let private elementColor = color 90 140 200
let private elementStroke = color 0 60 160

let private toPoint (sp : ScreenPoint) : Point = Point(sp.sx, sp.sy)
let private projectPt (m : Model) (p : Vector3) : ScreenPoint = TableScene.project m.view p

/// The element's top-down footprint (depth `b` along the beam × face `a2` across it, magnified by the
/// draw zoom), as a filled rectangle that highlights while dragging.
let private elementView (m : Model) : IView =
    let cx = elementX m
    let hb = drawZoom * (m.element.box.b / 2.0 / 1.0<meter>)
    let ha = drawZoom * (m.element.box.a2 / 2.0 / 1.0<meter>)
    let corners =
        [ (cx - hb, -ha); (cx + hb, -ha); (cx + hb, ha); (cx - hb, ha) ]
        |> List.map (fun (x, y) -> toPoint (projectPt m (Vector3.create x y 0.0)))
    Polygon.create [
        Polygon.points corners
        Polygon.fill (brushA 0.70 elementColor)
        Polygon.stroke (brush elementStroke)
        Polygon.strokeThickness (if m.dragging then 3.0 else 2.0)
    ] :> IView

let private tableCanvas (model : Model) : IView =
    Canvas.create [
        Canvas.name UiIds.canvas
        Canvas.width canvasWidth
        Canvas.height canvasHeight
        Canvas.horizontalAlignment HorizontalAlignment.Left
        Canvas.verticalAlignment VerticalAlignment.Top
        Canvas.children (TableScene.plateViews model.view model.table false @ TableScene.sourceDetectorRayViews model.view @ [ elementView model ])
    ] :> IView

let private kindName (k : CatalogueKind) : string =
    match k with
    | LightSource -> "Light source" | LinearPolarizer -> "Linear polarizer" | CircularPolarizer -> "Circular polarizer"
    | Sample -> "Sample" | Lens -> "Lens" | FlatMirror -> "Flat mirror" | CurvedMirror -> "Curved mirror" | Detector -> "Detector"

/// The control bar IS the shared `RayPositionControls` (the standardized position-on-the-ray control),
/// plus the element's name and a usage hint. The bar's − / + / field / Reset map straight onto the
/// pure `SlideBy` / `SlideTo` / `ResetPosition` messages; the host owns the clamp (±half the plate).
let private controlBar (model : Model) (dispatch : Msg -> unit) : IView =
    let half = halfLength model
    let state : RayPositionControls.State =
        { position = elementX model; minPosition = -half; maxPosition = half; enabled = true }
    let handlers : RayPositionControls.Handlers =
        {
            moveBy = fun dx -> dispatch (SlideBy dx)
            setPosition = fun x -> dispatch (SlideTo x)
            reset = fun () -> dispatch ResetPosition
        }
    StackPanel.create [
        StackPanel.orientation Orientation.Vertical
        StackPanel.spacing 6.0
        StackPanel.margin (Thickness 8.0)
        StackPanel.children [
            StackPanel.create [
                StackPanel.orientation Orientation.Horizontal
                StackPanel.spacing 10.0
                StackPanel.children [
                    RayPositionControls.view state handlers
                    TextBlock.create [ TextBlock.verticalAlignment VerticalAlignment.Center; TextBlock.text (kindName model.element.catalogueKind) ]
                ]
            ]
            TextBlock.create [
                TextBlock.foreground (brush (color 100 100 100))
                TextBlock.text "drag the element to slide it along the beam · ←/→ nudge it (Shift = larger step) · it stays on the central ray"
            ]
        ]
    ] :> IView

let view (model : Model) (dispatch : Msg -> unit) : IView =
    let toScreen (e : PointerEventArgs) : ScreenPoint = SceneInput.canvasPoint UiIds.canvas e
    DockPanel.create [
        DockPanel.children [
            Border.create [ Border.dock Dock.Top; Border.child (controlBar model dispatch) ]
            Border.create [
                Border.background (brush (color 250 250 250))
                Border.focusable true
                Border.onPointerPressed (fun e -> e.Handled <- true; dispatch (BeginDrag (toScreen e)))
                Border.onPointerMoved (fun e -> e.Handled <- true; dispatch (DragTo (toScreen e)))
                Border.onPointerReleased (fun e -> e.Handled <- true; dispatch EndDrag)
                Border.onKeyDown (fun e ->
                    let step = if e.KeyModifiers.HasFlag KeyModifiers.Shift then bigStepMeters else stepMeters
                    match e.Key with
                    | Key.Left -> e.Handled <- true; dispatch (SlideBy (-step))
                    | Key.Right -> e.Handled <- true; dispatch (SlideBy step)
                    | _ -> ())
                Border.child (tableCanvas model)
            ]
        ]
    ] :> IView
