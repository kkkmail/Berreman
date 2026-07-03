namespace OpticalConstructor.TestWindows

open Avalonia
open Avalonia.Controls
open Avalonia.Controls.Shapes
open Avalonia.Media
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open Berreman.Geometry
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Placement
open OpticalConstructor.Domain.Table
open OpticalConstructor.Domain.TableView

/// The reusable optical-table scene (Spec 0027, task 012): the ONE standard table every test reuses
/// instead of re-creating it. It is projected through a live `TableViewState` that is rotatable /
/// zoomable / pannable, with the plate drawing, the source/detector/ray drawing, and the table-selection
/// hit-test. These are stateless helpers over `TableViewState` + `OpticalTable`, so each test keeps its
/// own model (the table, the view, its selection, its own elements) and simply calls into here — the
/// canvas size, projection, gestures and plate look are then identical across every test.
module TableScene =

    // The shared canvas geometry (every test draws into an 820×560 canvas at 200 px/m, table-centred).
    let canvasWidth : float = 820.0
    let canvasHeight : float = 560.0
    let center : ScreenPoint = { sx = canvasWidth / 2.0; sy = canvasHeight / 2.0 }
    let pixelsPerMeter : float = 200.0

    // ----- projection / hit-test -----

    let project (view : TableViewState) (p : Vector3) : ScreenPoint = TableView.project pixelsPerMeter center view p
    let toPoint (sp : ScreenPoint) : Point = Point(sp.sx, sp.sy)

    /// Whether a screen point lands on the plate (used to (de)select the table).
    let tableHit (view : TableViewState) (table : OpticalTable) (sp : ScreenPoint) : bool =
        TableView.tableHit pixelsPerMeter center view table sp

    // ----- the table view gestures (pure `TableViewState` transforms; any R3 lock is the caller's job) -----

    /// Rotate the table VIEW about axis 1/2/3 by `deg` degrees (R1 spin, R2 pitch, R3 yaw — §C.2.4).
    let rotateViewBy (axis : int) (deg : float) (view : TableViewState) : TableViewState =
        let a = Angle.degree deg
        match axis with
        | 1 -> { view with r1 = view.r1 + a }
        | 2 -> { view with r2 = view.r2 + a }
        | _ -> { view with r3 = view.r3 + a }

    /// Zoom the table view by `notches` wheel steps, bounded [0.2, 5].
    let zoomViewBy (notches : int) (view : TableViewState) : TableViewState =
        { view with zoom = max 0.2 (min 5.0 (view.zoom * (1.1 ** float notches))) }

    /// Pan the table view by a screen-space delta (a drag).
    let panViewBy (dx : float) (dy : float) (view : TableViewState) : TableViewState =
        { view with panX = view.panX + dx; panY = view.panY + dy }

    /// Reset the table view to straight top-down (no pan / zoom / rotation).
    let resetView (view : TableViewState) : TableViewState = Table.resetView view

    // ----- drawing -----

    let private color (r : int) (g : int) (b : int) : Color = Color.FromRgb(byte r, byte g, byte b)
    let private brush (c : Color) : IBrush = SolidColorBrush(c) :> IBrush
    let private brushA (a : float) (c : Color) : IBrush =
        SolidColorBrush(Color.FromArgb(byte (255.0 * max 0.0 (min 1.0 a)), c.R, c.G, c.B)) :> IBrush

    let private plateColor = color 205 205 205
    let private plateSelectedColor = color 150 185 235
    let private edgeColor = color 50 50 50
    let private selectedStroke = color 0 60 160
    let private rayColor = color 30 90 200
    let private sourceColor = color 70 70 70
    let private detectorColor = color 20 20 20

    let private line (a : ScreenPoint) (b : ScreenPoint) (c : Color) (w : float) : IView =
        Line.create [ Line.startPoint (toPoint a); Line.endPoint (toPoint b); Line.stroke (brush c); Line.strokeThickness w ] :> IView

    /// The plate as a 3-D box (top face + the twelve edges, so an R2/R3 tilt is visible), highlighting
    /// when the table is the selected object.
    let plateViews (view : TableViewState) (table : OpticalTable) (selected : bool) : IView list =
        let corners = TableView.plateCorners3D table |> List.map (project view) |> List.toArray
        let topFace = corners.[0 .. 3] |> Array.map toPoint |> Array.toList
        let face =
            Polygon.create [
                Polygon.points topFace
                Polygon.fill (brushA (if selected then 0.55 else 0.85) (if selected then plateSelectedColor else plateColor))
                Polygon.stroke (brush (if selected then selectedStroke else edgeColor))
                Polygon.strokeThickness (if selected then 3.0 else 1.5)
            ] :> IView
        let edges =
            TableView.plateEdges
            |> List.map (fun (a, b) -> line corners.[a] corners.[b] edgeColor (if selected then 2.0 else 1.0))
        face :: edges

    /// The default central ray (source → detector) plus the source / detector markers.
    let sourceDetectorRayViews (view : TableViewState) : IView list =
        let s = project view (RayModel.pointToVector3 RayModel.defaultSourcePoint)
        let d = project view (RayModel.pointToVector3 RayModel.defaultDetectorPoint)
        let marker (sp : ScreenPoint) (c : Color) : IView =
            Ellipse.create [
                Ellipse.left (sp.sx - 6.0); Ellipse.top (sp.sy - 6.0); Ellipse.width 12.0; Ellipse.height 12.0
                Ellipse.fill (brush c); Ellipse.stroke (brush edgeColor); Ellipse.strokeThickness 1.0
            ] :> IView
        [ line s d rayColor 2.0; marker s sourceColor; marker d detectorColor ]
