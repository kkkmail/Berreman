/// The first test window (Spec 0027, task 002-rotate-table): a deliberately SIMPLE surface
/// that shows the optical table from the top and proves the table's 3-D rotation and select /
/// unselect actually work — by BUTTON and, crucially, by MOUSE (drag to tumble, wheel to
/// spin), which was the original broken interaction. It reuses the EXACT same table domain as
/// the main app (`Table.defaultTable` / `Table.TableViewState`) and the single-source-of-truth
/// pure projection `OpticalConstructor.Domain.TableView`. The `Model`/`Msg`/`update` are pure
/// and Avalonia-free; the pointer handlers are dumb event→message translators, so the whole
/// chain (real pointer event → message → `update` → rotated model) is provable headless.
module OpticalConstructor.TestWindows.TableRotationView

open Avalonia
open Avalonia.Controls
open Avalonia.Controls.Shapes
open Avalonia.Input
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
// headless tests and any external UI-Automation test address controls by intent.
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

/// A mouse gesture in progress over the canvas. `Pressed` is a click candidate (no movement
/// yet); once the pointer moves past the threshold it becomes `Orbiting` (a drag-to-tumble).
type DragState =
    | NotDragging
    | Pressed of ScreenPoint
    | Orbiting of ScreenPoint

/// The test-window model: the SAME optical table and view-orientation types the main app uses,
/// the binary table selection, and the transient mouse-gesture state. Nothing Avalonia here.
type Model =
    {
        table : OpticalTable
        view : TableViewState
        selection : TableSelection
        drag : DragState
    }

/// The straight top-down starting state: the default `1.2 × 2.0 × 0.10 m` table, the `(0,0,0)`
/// top-down view, nothing selected, no gesture in progress.
let init () : Model =
    {
        table = Table.defaultTable
        view = Table.defaultView
        selection = TableUnselected
        drag = NotDragging
    }

type Msg =
    /// Rotate the VIEW about screen axis Rk by a signed number of degrees (the buttons).
    | RotateR1By of float
    | RotateR2By of float
    | RotateR3By of float
    /// Reset the view to straight top-down `(0, 0, 0)`.
    | ResetView
    /// Raw mouse-gesture messages (the pointer handlers emit these verbatim; all gesture logic
    /// lives in `update`, so the handlers need no model and the chain is headless-testable).
    | PointerDown of ScreenPoint
    | PointerMove of ScreenPoint
    | PointerUp of ScreenPoint
    /// Mouse-wheel notches (+1 up / -1 down) → R1 in-plane spin.
    | WheelBy of int

// ---------------------------------------------------------------------------
// Constants.
// ---------------------------------------------------------------------------

/// The button rotation step: 15° normally, 5° with Shift held (a finer nudge to catch subtle
/// behaviour). The user asked for exactly this.
let buttonStepDegrees (shiftHeld : bool) : float = if shiftHeld then 5.0 else 15.0

/// Drag-to-tumble sensitivity (degrees of view rotation per pixel of mouse travel).
let orbitDegreesPerPixel : float = 0.4

/// One mouse-wheel notch spins the in-plane R1 by this many degrees.
let wheelSpinDegrees : float = 15.0

/// A press that never moves more than this many pixels is a click (select), not a drag.
let private dragThresholdPx : float = 3.0

[<Literal>]
let canvasWidth = 820.0
[<Literal>]
let canvasHeight = 560.0

/// The canvas centre the table projects around.
let center : ScreenPoint = { sx = canvasWidth / 2.0; sy = canvasHeight / 2.0 }

/// Drawing units per canonical meter: the `2.0 m × 1.2 m` plate draws ~400 × 240 px at the
/// default zoom, leaving room for the tumble to swing corners out without clipping.
let pixelsPerMeter : float = 200.0

// ---------------------------------------------------------------------------
// Pure update. Angles are normalised mod 360 (no spinors here — 370° ≡ 10°).
// ---------------------------------------------------------------------------

/// Wrap a degree value into `[0, 360)` — the user's "no angles more than 360, use mod 360".
let normalizeDegrees (d : float) : float =
    let m = d % 360.0
    if m < 0.0 then m + 360.0 else m

let private addDeg (a : Angle) (d : float) : Angle = Angle.degree (normalizeDegrees (a.degrees + d))

let private rotate1 (d : float) (m : Model) : Model = { m with view = { m.view with r1 = addDeg m.view.r1 d } }
let private rotate2 (d : float) (m : Model) : Model = { m with view = { m.view with r2 = addDeg m.view.r2 d } }
let private rotate3 (d : float) (m : Model) : Model = { m with view = { m.view with r3 = addDeg m.view.r3 d } }

let private dist (a : ScreenPoint) (b : ScreenPoint) : float =
    sqrt ((a.sx - b.sx) ** 2.0 + (a.sy - b.sy) ** 2.0)

/// Orbit from a reference screen point to the current one: horizontal travel yaws (R3),
/// vertical travel pitches (R2), so a drag tumbles the table the way a hand would turn it.
let private orbitFrom (refPt : ScreenPoint) (pt : ScreenPoint) (m : Model) : Model =
    let dx = pt.sx - refPt.sx
    let dy = pt.sy - refPt.sy
    m
    |> rotate3 (orbitDegreesPerPixel * dx)
    |> rotate2 (orbitDegreesPerPixel * dy)
    |> fun m' -> { m' with drag = Orbiting pt }

let update (msg : Msg) (model : Model) : Model =
    match msg with
    | RotateR1By d -> rotate1 d model
    | RotateR2By d -> rotate2 d model
    | RotateR3By d -> rotate3 d model
    | ResetView -> { model with view = Table.resetView model.view; drag = NotDragging }
    | PointerDown pt -> { model with drag = Pressed pt }
    | PointerMove pt ->
        match model.drag with
        | NotDragging -> model
        | Pressed start -> if dist start pt < dragThresholdPx then model else orbitFrom start pt model
        | Orbiting last -> orbitFrom last pt model
    | PointerUp _ ->
        // A press with no drag is a click → (de)select by hit-testing where the press landed.
        let selected =
            match model.drag with
            | Pressed start ->
                if TableView.tableHit pixelsPerMeter center model.view model.table start
                then TableSelected
                else TableUnselected
            | _ -> model.selection
        { model with selection = selected; drag = NotDragging }
    | WheelBy notches -> rotate1 (float notches * wheelSpinDegrees) model

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
// The FuncUI view. Geometry is the pure `TableView` projection; the pointer handlers turn
// raw events into the pure mouse messages (no model logic in the handlers).
// ---------------------------------------------------------------------------

let private toPoint (sp : ScreenPoint) : Point = Point(sp.sx, sp.sy)

let private projected (model : Model) (p : Vector3) : ScreenPoint =
    TableView.project pixelsPerMeter center model.view p

/// The plate: the filled top face plus the twelve box edges, so the slab reads as a 3-D object
/// when tumbled. The top face carries the selection indicator (≥ 2 px, strong-blue) when
/// selected.
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

/// A reference overlay drawn on the table plane (z = 0): the central-ray axis from the source
/// (left) to the detector (right), with a marker at each end, so the rotation is unmistakable
/// (you can see which way the bench is facing).
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

/// A clickable, button-styled `Border` (a real `Button.Click` carries no key modifiers, so a
/// Border + `onPointerPressed` is what lets Shift change the step). Dispatches `mk (sign·step)`
/// where the step is 5° with Shift, else 15°.
let private rotateButton (id : string) (label : string) (mk : float -> Msg) (sign : float) (dispatch : Msg -> unit) : IView =
    Border.create [
        Border.name id
        Border.background (brush (color 232 232 232))
        Border.borderBrush (brush (color 120 120 120))
        Border.borderThickness 1.0
        Border.cornerRadius (CornerRadius 3.0)
        Border.padding (Thickness(12.0, 6.0))
        Border.child (TextBlock.create [ TextBlock.text label ])
        Border.onPointerPressed (fun e ->
            // FuncUI subscribes pointer handlers for the event's full Tunnel|Bubble routing, so
            // the handler fires TWICE for the clicked element (tunnel down, then bubble up).
            // Marking it handled drops the second pass, so one click = one 5°/15° step.
            e.Handled <- true
            let step = buttonStepDegrees (e.KeyModifiers.HasFlag KeyModifiers.Shift)
            dispatch (mk (sign * step)))
    ] :> IView

let private degrees (a : Angle) : float = a.degrees

let private controlBar (model : Model) (dispatch : Msg -> unit) : IView =
    let readout =
        sprintf
            "R1 %.0f°   R2 %.0f°   R3 %.0f°      Table: %s"
            (degrees model.view.r1) (degrees model.view.r2) (degrees model.view.r3)
            (match model.selection with TableSelected -> "SELECTED" | TableUnselected -> "not selected")
    StackPanel.create [
        StackPanel.orientation Orientation.Vertical
        StackPanel.spacing 6.0
        StackPanel.margin (Thickness 8.0)
        StackPanel.children [
            StackPanel.create [
                StackPanel.orientation Orientation.Horizontal
                StackPanel.spacing 6.0
                StackPanel.children [
                    rotateButton UiIds.rotateR1Minus "R1 −" RotateR1By (-1.0) dispatch
                    rotateButton UiIds.rotateR1Plus "R1 +" RotateR1By 1.0 dispatch
                    rotateButton UiIds.rotateR2Minus "R2 −" RotateR2By (-1.0) dispatch
                    rotateButton UiIds.rotateR2Plus "R2 +" RotateR2By 1.0 dispatch
                    rotateButton UiIds.rotateR3Minus "R3 −" RotateR3By (-1.0) dispatch
                    rotateButton UiIds.rotateR3Plus "R3 +" RotateR3By 1.0 dispatch
                    Button.create [
                        Button.name UiIds.resetView
                        Button.content "Reset (top-down)"
                        Button.onClick (fun _ -> dispatch ResetView)
                    ]
                ]
            ]
            TextBlock.create [
                TextBlock.name UiIds.readout
                TextBlock.text readout
            ]
            TextBlock.create [
                TextBlock.foreground (brush (color 100 100 100))
                TextBlock.text "Drag the table to tumble it · mouse-wheel to spin (R1) · click to select/deselect · Shift+button = 5°"
            ]
        ]
    ] :> IView

let private tableCanvas (model : Model) : IView =
    Canvas.create [
        Canvas.name UiIds.canvas
        Canvas.width canvasWidth
        Canvas.height canvasHeight
        // Pin to the top-left of the host Border (which sits at the window content origin), so a
        // top-level pointer position equals a canvas coordinate.
        Canvas.horizontalAlignment HorizontalAlignment.Left
        Canvas.verticalAlignment VerticalAlignment.Top
        Canvas.children (referenceViews model @ plateViews model)
    ] :> IView

/// The whole test surface: the table canvas — wrapped in a Border that owns the background and
/// the pointer handlers (a `Panel` exposes no background attr, and the Border at the content
/// origin makes a top-level pointer position equal a canvas coordinate) — with the rotation /
/// reset controls and the live readout docked beneath it.
let view (model : Model) (dispatch : Msg -> unit) : IView =
    let toScreen (e : PointerEventArgs) : ScreenPoint =
        let p = e.GetPosition null
        { sx = p.X; sy = p.Y }
    DockPanel.create [
        DockPanel.children [
            Border.create [
                Border.dock Dock.Bottom
                Border.child (controlBar model dispatch)
            ]
            Border.create [
                Border.background (brush (color 250 250 250))
                // `e.Handled <- true` drops FuncUI's duplicate Tunnel|Bubble invocation, so a wheel
                // notch spins once and a drag step orbits once (the handlers accumulate rotation).
                Border.onPointerPressed (fun e -> e.Handled <- true; dispatch (PointerDown (toScreen e)))
                Border.onPointerMoved (fun e -> e.Handled <- true; dispatch (PointerMove (toScreen e)))
                Border.onPointerReleased (fun e -> e.Handled <- true; dispatch (PointerUp (toScreen e)))
                Border.onPointerWheelChanged (fun e -> e.Handled <- true; dispatch (WheelBy (if e.Delta.Y >= 0.0 then 1 else -1)))
                Border.child (tableCanvas model)
            ]
        ]
    ] :> IView
