namespace OpticalConstructor.Ui

open Avalonia
open Avalonia.Controls
open Avalonia.Controls.Shapes
open Avalonia.Media
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open Berreman.Geometry
open OpticalConstructor.Controls
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Placement
open OpticalConstructor.Domain.Table
open OpticalConstructor.Domain.TableView

/// A length on a scene canvas, in logical (device-independent) pixels.
type CanvasLength =
    | CanvasLength of float

    member this.value = let (CanvasLength l) = this in l

/// A mouse-wheel gesture's signed count of notches.
type WheelNotches =
    | WheelNotches of int

    member this.value = let (WheelNotches n) = this in n

/// Whether the table plate is drawn as the selected object.
type PlateHighlight =
    | PlateSelected
    | PlateNotSelected

/// A drawn line's thickness, in logical pixels.
type StrokeWidth =
    | StrokeWidth of float

    member this.value = let (StrokeWidth w) = this in w

/// The Avalonia face of the Avalonia-free screen point (spec 0048 E028, ELEV013).
[<AutoOpen>]
module ScreenPointAvalonia =

    type ScreenPoint with

        member this.toPoint : Point = Point(this.sx, this.sy)

/// The reusable optical-table scene (Spec 0027, task 012): the ONE standard table every test reuses
/// instead of re-creating it. It is projected through a live `TableViewState` that is rotatable /
/// zoomable / pannable, with the plate drawing, the source/detector/ray drawing, and the table-selection
/// hit-test. These are stateless helpers over `TableViewState` + `OpticalTable`, so each test keeps its
/// own model (the table, the view, its selection, its own elements) and simply calls into here — the
/// canvas size, projection, gestures and plate look are then identical across every test.
module TableScene =

    // The shared canvas geometry (every test draws into an 820×560 canvas at 200 px/m, table-centred).
    let canvasWidth : CanvasLength = CanvasLength 820.0
    let canvasHeight : CanvasLength = CanvasLength 560.0
    let center : ScreenPoint = { sx = canvasWidth.value / 2.0; sy = canvasHeight.value / 2.0 }
    let pixelsPerMeter : PixelsPerMeter = PixelsPerMeter 200.0

    // ----- projection / hit-test -----

    let projectOnScene (view : TableViewState) (p : Vector3) : ScreenPoint = TableView.project pixelsPerMeter center view p

    /// Whether a screen point lands on the plate (used to (de)select the table).
    let tableHitOnScene (view : TableViewState) (table : OpticalTable) (sp : ScreenPoint) : PolygonContainment =
        TableView.tableHit pixelsPerMeter center view table sp

    // ----- the table view gestures (pure `TableViewState` transforms; any R3 lock is the caller's job) -----

    /// Rotate the table VIEW about an axis by `a` (R1 spin, R2 pitch, R3 yaw — §C.2.4).
    let rotateViewBy (axis : RotationControls.RotationAxis) (a : Angle) (view : TableViewState) : TableViewState =
        match axis with
        | RotationControls.R1 -> { view with r1 = view.r1 + a }
        | RotationControls.R2 -> { view with r2 = view.r2 + a }
        | RotationControls.R3 -> { view with r3 = view.r3 + a }

    /// Zoom the table view by `notches` wheel steps, bounded [0.2, 5].
    let zoomViewBy (notches : WheelNotches) (view : TableViewState) : TableViewState =
        { view with zoom = TableZoom (max 0.2 (min 5.0 (view.zoom.value * (1.1 ** float notches.value)))) }

    /// Pan the table view by the screen-space delta of a drag from `dragFrom` to `dragTo`.
    let panViewBy (dragFrom : ScreenPoint) (dragTo : ScreenPoint) (view : TableViewState) : TableViewState =
        { view with
            panX = TablePan (view.panX.value + (dragTo.sx - dragFrom.sx))
            panY = TablePan (view.panY.value + (dragTo.sy - dragFrom.sy)) }

    // ----- drawing -----

    let private brush (c : Color) : IBrush = SolidColorBrush(c) :> IBrush
    let private brushA (opacity : RendererControls.RendererOpacity) (c : Color) : IBrush =
        SolidColorBrush(Color.FromArgb(byte (255.0 * opacity.clamped.value), c.R, c.G, c.B)) :> IBrush

    let private plateColor = Color.FromRgb(205uy, 205uy, 205uy)
    let private plateSelectedColor = Color.FromRgb(150uy, 185uy, 235uy)
    let private edgeColor = Color.FromRgb(50uy, 50uy, 50uy)
    let private selectedStroke = Color.FromRgb(0uy, 60uy, 160uy)
    let private rayColor = Color.FromRgb(30uy, 90uy, 200uy)
    let private sourceColor = Color.FromRgb(70uy, 70uy, 70uy)
    let private detectorColor = Color.FromRgb(20uy, 20uy, 20uy)

    let private line (a : ScreenPoint) (b : ScreenPoint) (c : Color) (w : StrokeWidth) : IView =
        Line.create [ Line.startPoint a.toPoint; Line.endPoint b.toPoint; Line.stroke (brush c); Line.strokeThickness w.value ] :> IView

    /// The plate as a 3-D box (top face + the twelve edges, so an R2/R3 tilt is visible), highlighting
    /// when the table is the selected object.
    let plateViews (view : TableViewState) (table : OpticalTable) (highlight : PlateHighlight) : IView list =
        let corners = table.plateCorners3D |> List.map (projectOnScene view) |> List.toArray
        let topFace = corners.[0 .. 3] |> Array.map (fun c -> c.toPoint) |> Array.toList
        let face =
            match highlight with
            | PlateSelected ->
                Polygon.create [
                    Polygon.points topFace
                    Polygon.fill (brushA (RendererControls.RendererOpacity 0.55) plateSelectedColor)
                    Polygon.stroke (brush selectedStroke)
                    Polygon.strokeThickness 3.0
                ] :> IView
            | PlateNotSelected ->
                Polygon.create [
                    Polygon.points topFace
                    Polygon.fill (brushA (RendererControls.RendererOpacity 0.85) plateColor)
                    Polygon.stroke (brush edgeColor)
                    Polygon.strokeThickness 1.5
                ] :> IView
        let edgeWidth =
            match highlight with
            | PlateSelected -> StrokeWidth 2.0
            | PlateNotSelected -> StrokeWidth 1.0
        let edges =
            TableView.plateEdges
            |> List.map (fun edge -> line (edge.fromCorner.pick corners) (edge.toCorner.pick corners) edgeColor edgeWidth)
        face :: edges

    /// The default central ray (source → detector) plus the source / detector markers.
    let sourceDetectorRayViews (view : TableViewState) : IView list =
        let s = projectOnScene view RayModel.defaultSourcePoint.toVector3
        let d = projectOnScene view RayModel.defaultDetectorPoint.toVector3
        let marker (sp : ScreenPoint) (c : Color) : IView =
            Ellipse.create [
                Ellipse.left (sp.sx - 6.0); Ellipse.top (sp.sy - 6.0); Ellipse.width 12.0; Ellipse.height 12.0
                Ellipse.fill (brush c); Ellipse.stroke (brush edgeColor); Ellipse.strokeThickness 1.0
            ] :> IView
        [ line s d rayColor (StrokeWidth 2.0); marker s sourceColor; marker d detectorColor ]
