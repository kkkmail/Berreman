/// The first test window (Spec 0027, task 002): a deliberately SIMPLE surface that shows
/// the optical table from the top and proves the table's 3-D rotation and select / unselect
/// actually work. It reuses the EXACT same table domain as the main app — `Table.defaultTable`
/// and `Table.TableViewState` — and the single-source-of-truth pure projection
/// `OpticalConstructor.Domain.TableView` (no parallel table, no parallel rotation). The
/// `Model`/`Msg`/`update` are pure and Avalonia-free, so every rotation / reset / selection
/// rule is provable headless; the `view` is the thin FuncUI `Canvas` binding over the pure
/// projection.
module OpticalConstructor.TestWindows.TableRotationView

open Avalonia
open Avalonia.Controls
open Avalonia.Controls.Shapes
open Avalonia.Layout
open Avalonia.Media
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open Berreman.Geometry
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Placement
open OpticalConstructor.Domain.Table
open OpticalConstructor.Domain.TableView

// ---------------------------------------------------------------------------
// Stable automation ids (CLAUDE.md UI guidance): one place, never duplicated, so the
// headless MVU tests and any external UI-Automation test address controls by intent.
// ---------------------------------------------------------------------------

[<RequireQualifiedAccess>]
module UiIds =
    let canvas = "TableRotationCanvas"
    let rotateR1Minus = "RotateR1MinusButton"
    let rotateR1Plus = "RotateR1PlusButton"
    let rotateR2Minus = "RotateR2MinusButton"
    let rotateR2Plus = "RotateR2PlusButton"
    let rotateR3Minus = "RotateR3MinusButton"
    let rotateR3Plus = "RotateR3PlusButton"
    let resetView = "ResetViewButton"
    let readout = "TableRotationReadout"

// ---------------------------------------------------------------------------
// Pure model (no Avalonia handle) — provable headless.
// ---------------------------------------------------------------------------

/// Whether the table is currently selected. A two-case named condition rather than a bool
/// (CLAUDE.md), so a match site reads as prose.
type TableSelection =
    | TableSelected
    | TableUnselected

/// The test-window model: the SAME optical table and view-orientation types the main app
/// uses, plus the binary table selection. Nothing Avalonia here.
type Model =
    {
        table : OpticalTable
        view : TableViewState
        selection : TableSelection
    }

/// The straight top-down starting state: the default `1.2 × 2.0 × 0.10 m` table, the
/// `(0, 0, 0)` top-down view, nothing selected.
let init () : Model =
    {
        table = Table.defaultTable
        view = Table.defaultView
        selection = TableUnselected
    }

/// The per-click rotation step in degrees — large enough that one click visibly tumbles the
/// table (this is a manual proving surface, not the 5° production nudge).
let rotationStepDegrees : float = 15.0

type Msg =
    /// Rotate the VIEW about screen axis Rk by `notches` steps (+1 / -1).
    | RotateR1 of int
    | RotateR2 of int
    | RotateR3 of int
    /// Reset the view to straight top-down `(0, 0, 0)` (the recover-from-tumble command).
    | ResetView
    /// A click on the canvas at the given screen point — selects the table iff it hits the
    /// projected plate, else clears the selection.
    | ClickAt of ScreenPoint

// ---------------------------------------------------------------------------
// Canvas layout constants + the projection inputs (the test window's "camera").
// ---------------------------------------------------------------------------

[<Literal>]
let canvasWidth = 820.0
[<Literal>]
let canvasHeight = 560.0

/// The canvas centre the table projects around.
let center : ScreenPoint = { sx = canvasWidth / 2.0; sy = canvasHeight / 2.0 }

/// Drawing units per canonical meter: the `2.0 m × 1.2 m` plate draws ~400 × 240 px at the
/// default zoom, leaving room for the tumble to swing corners out without clipping.
let pixelsPerMeter : float = 200.0

let private bump (current : Angle) (notches : int) : Angle =
    current + Angle.degree (float notches * rotationStepDegrees)

let update (msg : Msg) (model : Model) : Model =
    match msg with
    | RotateR1 n -> { model with view = { model.view with r1 = bump model.view.r1 n } }
    | RotateR2 n -> { model with view = { model.view with r2 = bump model.view.r2 n } }
    | RotateR3 n -> { model with view = { model.view with r3 = bump model.view.r3 n } }
    | ResetView -> { model with view = Table.resetView model.view }
    | ClickAt pt ->
        if TableView.tableHit pixelsPerMeter center model.view model.table pt
        then { model with selection = TableSelected }
        else { model with selection = TableUnselected }

// ---------------------------------------------------------------------------
// Colours (mirroring the constructor table's palette so the test reads like the real one).
// ---------------------------------------------------------------------------

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

// ---------------------------------------------------------------------------
// The FuncUI view (the only Avalonia-touching part). Geometry is the pure `TableView`
// projection; the click handler turns a canvas point into the pure `ClickAt` message.
// ---------------------------------------------------------------------------

let private toPoint (sp : ScreenPoint) : Point = Point(sp.sx, sp.sy)

let private projected (model : Model) (p : Vector3) : ScreenPoint =
    TableView.project pixelsPerMeter center model.view p

/// The plate: the filled top face plus the twelve box edges, so the slab reads as a 3-D
/// object when tumbled. The top face carries the selection indicator (≥ 2 px, strong-blue,
/// CLAUDE.md / AC-C5) when the table is selected.
let private plateViews (model : Model) : IView list =
    let corners = TableView.plateCorners3D model.table |> List.map (projected model) |> List.toArray
    let selected = model.selection = TableSelected
    let topFacePts = corners.[0 .. 3] |> Array.map toPoint |> Array.toList
    let face =
        Polygon.create [
            Polygon.points topFacePts
            Polygon.fill (brushA (if selected then 0.55 else 0.85) (if selected then plateSelectedColor else plateColor))
            Polygon.stroke (brush (if selected then selectedStroke else edgeColor))
            Polygon.strokeThickness (if selected then 3.0 else 1.5)
        ] :> IView
    let edges =
        TableView.plateEdges
        |> List.map (fun (i, j) ->
            Line.create [
                Line.startPoint (toPoint corners.[i])
                Line.endPoint (toPoint corners.[j])
                Line.stroke (brush edgeColor)
                Line.strokeThickness (if selected then 2.0 else 1.0)
            ] :> IView)
    face :: edges

/// A reference overlay drawn on the table plane (z = 0): the central-ray axis from the
/// source (left) to the detector (right), with a marker at each end, so the rotation is
/// unmistakable (you can see which way the bench is facing).
let private referenceViews (model : Model) : IView list =
    let s = projected model (RayModel.pointToVector3 RayModel.defaultSourcePoint)
    let d = projected model (RayModel.pointToVector3 RayModel.defaultDetectorPoint)
    let marker (sp : ScreenPoint) (c : Color) (radius : float) : IView =
        Ellipse.create [
            Ellipse.left (sp.sx - radius)
            Ellipse.top (sp.sy - radius)
            Ellipse.width (2.0 * radius)
            Ellipse.height (2.0 * radius)
            Ellipse.fill (brush c)
            Ellipse.stroke (brush edgeColor)
            Ellipse.strokeThickness 1.0
        ] :> IView
    [ Line.create [
        Line.startPoint (toPoint s)
        Line.endPoint (toPoint d)
        Line.stroke (brush rayColor)
        Line.strokeThickness 2.0
      ] :> IView
      marker s sourceColor 7.0
      marker d detectorColor 7.0 ]

let private degrees (a : Angle) : float = a.degrees

let private rotateButton (id : string) (label : string) (msg : Msg) (dispatch : Msg -> unit) : IView =
    Button.create [
        Button.name id
        Button.content label
        Button.minWidth 54.0
        Button.onClick (fun _ -> dispatch msg)
    ] :> IView

let private controlBar (model : Model) (dispatch : Msg -> unit) : IView =
    let readout =
        sprintf
            "R1 %+.0f°   R2 %+.0f°   R3 %+.0f°      Table: %s"
            (degrees model.view.r1) (degrees model.view.r2) (degrees model.view.r3)
            (match model.selection with TableSelected -> "SELECTED" | TableUnselected -> "not selected")
    StackPanel.create [
        StackPanel.orientation Orientation.Horizontal
        StackPanel.spacing 6.0
        StackPanel.margin (Thickness 8.0)
        StackPanel.children [
            rotateButton UiIds.rotateR1Minus "R1 −" (RotateR1 -1) dispatch
            rotateButton UiIds.rotateR1Plus "R1 +" (RotateR1 1) dispatch
            rotateButton UiIds.rotateR2Minus "R2 −" (RotateR2 -1) dispatch
            rotateButton UiIds.rotateR2Plus "R2 +" (RotateR2 1) dispatch
            rotateButton UiIds.rotateR3Minus "R3 −" (RotateR3 -1) dispatch
            rotateButton UiIds.rotateR3Plus "R3 +" (RotateR3 1) dispatch
            Button.create [
                Button.name UiIds.resetView
                Button.content "Reset (top-down)"
                Button.onClick (fun _ -> dispatch ResetView)
            ]
            TextBlock.create [
                TextBlock.name UiIds.readout
                TextBlock.verticalAlignment VerticalAlignment.Center
                TextBlock.margin (Thickness(12.0, 0.0, 0.0, 0.0))
                TextBlock.text readout
            ]
        ]
    ] :> IView

let private tableCanvas (model : Model) : IView =
    Canvas.create [
        Canvas.name UiIds.canvas
        Canvas.width canvasWidth
        Canvas.height canvasHeight
        // Pin to the top-left of the host Border (which sits at the window content origin),
        // so a top-level pointer position equals a canvas coordinate.
        Canvas.horizontalAlignment HorizontalAlignment.Left
        Canvas.verticalAlignment VerticalAlignment.Top
        Canvas.children (referenceViews model @ plateViews model)
    ] :> IView

/// The whole test surface: the table canvas — wrapped in a Border that owns the background
/// and the click handler (a `Panel` exposes no background attr, and the Border at the content
/// origin makes the top-level pointer position equal a canvas coordinate) — with the rotation
/// / reset controls and the live R1/R2/R3 + selection readout docked beneath it.
let view (model : Model) (dispatch : Msg -> unit) : IView =
    DockPanel.create [
        DockPanel.children [
            Border.create [
                Border.dock Dock.Bottom
                Border.child (controlBar model dispatch)
            ]
            Border.create [
                Border.background (brush (color 250 250 250))
                Border.onPointerPressed (fun e ->
                    let p = e.GetPosition null
                    dispatch (ClickAt { sx = p.X; sy = p.Y }))
                Border.child (tableCanvas model)
            ]
        ]
    ] :> IView
