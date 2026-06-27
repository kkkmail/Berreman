/// The first test window (Spec 0027, task 002-rotate-table): a SIMPLE surface that shows the
/// optical table from the top and proves the table's 3-D rotation + select / unselect work.
/// It reuses the EXACT same table domain as the main app (`Table.defaultTable` /
/// `Table.TableViewState`) and the single-source-of-truth pure projection
/// `OpticalConstructor.Domain.TableView`.
///
/// Rotation is CONSTRAINED, one axis at a time, by the documented gestures (Spec 0026 §E.3 /
/// §E.5, mirroring `OpticalConstructor.Ui/Commands.fs` — replicated here, with the citation,
/// to keep this diagnostic window isolated on the Domain only):
///   * `Shift`+wheel       → R1   (5° per notch)
///   * `Ctrl`+`Shift`+wheel → R2
///   * `Alt`+wheel         → R3
///   * plain / `Ctrl`+wheel → zoom
///   * any other modifier combination → NOTHING (exactly one thing per gesture, never several).
/// A plain click selects / deselects the table; a plain press→drag→release PANS the table across
/// the screen (the standard click-and-drag). The buttons rotate one axis by 15° (Shift = 5°).
///
/// The `Model`/`Msg`/`update` are pure and Avalonia-free; the pointer handlers are dumb
/// event→message translators, so the whole chain (real pointer event → message → `update` →
/// rotated model) is provable headless.
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
open OpticalConstructor.Controls

// ---------------------------------------------------------------------------
// Stable automation ids (CLAUDE.md UI guidance): one place, never duplicated.
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
// Pure model (no Avalonia handle).
// ---------------------------------------------------------------------------

/// Whether the table is currently selected (a named condition, not a bool — CLAUDE.md).
type TableSelection =
    | TableSelected
    | TableUnselected

/// A left-button gesture in progress. `Pressed` is a click candidate; once the pointer moves
/// past the threshold it becomes `Panning` and the drag PANS the table across the screen (the
/// standard click-and-drag). `Panning` carries the last pointer position. Only a clean
/// `Pressed`→release (no movement) is a click.
type DragState =
    | NotPressed
    | Pressed of ScreenPoint
    | Panning of ScreenPoint

/// The keyboard modifiers a wheel gesture carries (mirrors `Commands.Modifier`, §E.1/§E.3),
/// as a comparable set so the gesture maps one-to-one to an action.
type WheelModifier =
    | ModCtrl
    | ModShift
    | ModAlt

/// The single thing a wheel gesture resolves to (§E.3/§E.5). `NoWheelAction` is the explicit
/// "this combination does nothing" — the scientific-app constraint that a gesture does exactly
/// one thing or nothing, never several at once.
type WheelAction =
    | RotateView1
    | RotateView2
    | RotateView3
    | ZoomView
    | NoWheelAction

/// The test-window model: the SAME optical table + view-orientation types the main app uses,
/// the binary selection, and the transient left-button gesture state. Nothing Avalonia here.
type Model =
    {
        table : OpticalTable
        view : TableViewState
        selection : TableSelection
        drag : DragState
        /// The table's R3-lock (task 008): UNLOCKED by default, so the bar reads "Lock R3".
        r3Locked : bool
        /// Which reset (if any) the rotation-controls bar is awaiting confirmation for.
        rotationConfirm : RotationControls.ResetConfirm
    }

/// Straight top-down, default table, nothing selected, no gesture in progress.
let init () : Model =
    {
        table = Table.defaultTable
        view = Table.defaultView
        selection = TableUnselected
        drag = NotPressed
        r3Locked = false
        rotationConfirm = RotationControls.NoConfirm
    }

type Msg =
    /// Rotate the VIEW about screen axis Rk by a signed number of degrees (the buttons).
    | RotateR1By of float
    | RotateR2By of float
    | RotateR3By of float
    /// Reset the view to straight top-down `(0, 0, 0)`.
    | ResetView
    /// Raw left-button gesture messages (the handlers emit these verbatim; the click-vs-drag
    /// logic lives in `update`, so the handlers carry no model and the chain is testable).
    | PointerDown of ScreenPoint
    | PointerMove of ScreenPoint
    | PointerUp of ScreenPoint
    /// A wheel notch (+1 up / -1 down) with its modifiers — resolved against the documented
    /// gesture map in `update`.
    | Wheel of Set<WheelModifier> * int
    /// Rotation-controls bar messages (task 008): lock/unlock R3, set an axis to an exact angle,
    /// and the confirmation-gated Reset / Reset All.
    | ToggleR3Lock
    | RotSetAxis of RotationControls.Axis * float
    | RotRequestReset
    | RotRequestResetAll
    | RotConfirm
    | RotCancel

// ---------------------------------------------------------------------------
// Constants.
// ---------------------------------------------------------------------------

/// The BUTTON rotation step: 15° normally, 5° with Shift (a finer nudge).
let buttonStepDegrees (shiftHeld : bool) : float = if shiftHeld then 5.0 else 15.0

/// The WHEEL rotation step — the spec's documented default (§E.3): 5° per notch.
let wheelStepDegrees : float = 5.0

/// A press that never moves past this many pixels is a click, not a drag.
let private dragThresholdPx : float = 3.0

let private zoomStep : float = 1.1
let private zoomMin : float = 0.2
let private zoomMax : float = 5.0

[<Literal>]
let canvasWidth = 820.0
[<Literal>]
let canvasHeight = 560.0

let center : ScreenPoint = { sx = canvasWidth / 2.0; sy = canvasHeight / 2.0 }
let pixelsPerMeter : float = 200.0

// ---------------------------------------------------------------------------
// The documented wheel-gesture map (§E.3/§E.5, mirroring Commands.fs verbatim):
//   {Shift} -> R1 ; {Ctrl,Shift} -> R2 ; {Alt} -> R3 ; {} or {Ctrl} -> zoom ; else nothing.
// ---------------------------------------------------------------------------

let wheelAction (mods : Set<WheelModifier>) : WheelAction =
    let has m = Set.contains m mods
    match has ModCtrl, has ModShift, has ModAlt with
    | false, true, false -> RotateView1
    | true, true, false -> RotateView2
    | false, false, true -> RotateView3
    | false, false, false -> ZoomView
    | true, false, false -> ZoomView
    | _ -> NoWheelAction

// ---------------------------------------------------------------------------
// Pure update. Angles wrap mod 360 (no spinors — 370° ≡ 10°).
// ---------------------------------------------------------------------------

let normalizeDegrees (d : float) : float =
    let m = d % 360.0
    if m < 0.0 then m + 360.0 else m

let private addDeg (a : Angle) (d : float) : Angle = Angle.degree (normalizeDegrees (a.degrees + d))

let private rotate1 (d : float) (m : Model) : Model = { m with view = { m.view with r1 = addDeg m.view.r1 d } }
let private rotate2 (d : float) (m : Model) : Model = { m with view = { m.view with r2 = addDeg m.view.r2 d } }
/// R3 respects the table's R3-lock (task 008): a locked R3 ignores the rotation.
let private rotate3 (d : float) (m : Model) : Model = if m.r3Locked then m else { m with view = { m.view with r3 = addDeg m.view.r3 d } }

let private zoomBy (notches : int) (m : Model) : Model =
    let z = m.view.zoom * (zoomStep ** float notches)
    { m with view = { m.view with zoom = max zoomMin (min zoomMax z) } }

let private dist (a : ScreenPoint) (b : ScreenPoint) : float =
    sqrt ((a.sx - b.sx) ** 2.0 + (a.sy - b.sy) ** 2.0)

/// Pan the table by the screen delta from a reference point to the current one — the standard
/// click-and-drag move. Pan is a pure screen-space translation (`TableViewState.panX/panY`), so
/// it never rotates; rotation stays the documented modifier+wheel gesture.
let private panFrom (refPt : ScreenPoint) (pt : ScreenPoint) (m : Model) : Model =
    { m with
        view = { m.view with panX = m.view.panX + (pt.sx - refPt.sx); panY = m.view.panY + (pt.sy - refPt.sy) }
        drag = Panning pt }

/// The table is rotatable ONLY when it is selected (Spec 0027, task 006 #1): a rotation request
/// is dropped unless the table is currently selected. Pan / zoom / reset are not rotations and
/// stay available regardless.
let private rotateIfSelected (f : Model -> Model) (model : Model) : Model =
    match model.selection with
    | TableSelected -> f model
    | TableUnselected -> model

/// Set the view's axis to an exact angle (mod 360), lock-respecting for R3 (task 008).
let private setAxis (axis : RotationControls.Axis) (v : float) (m : Model) : Model =
    let a = Angle.degree (normalizeDegrees v)
    match axis with
    | RotationControls.R1 -> { m with view = { m.view with r1 = a } }
    | RotationControls.R2 -> { m with view = { m.view with r2 = a } }
    | RotationControls.R3 -> if m.r3Locked then m else { m with view = { m.view with r3 = a } }

/// Reset the table view's rotations to 0 (keeping pan / zoom). The only rotatable object here is
/// the table, so Reset (selection) and Reset All do the same thing.
let private resetRotations (m : Model) : Model =
    { m with view = { m.view with r1 = Angle.zero; r2 = Angle.zero; r3 = Angle.zero } }

let update (msg : Msg) (model : Model) : Model =
    match msg with
    | RotateR1By d -> rotateIfSelected (rotate1 d) model
    | RotateR2By d -> rotateIfSelected (rotate2 d) model
    | RotateR3By d -> rotateIfSelected (rotate3 d) model
    | ResetView -> { model with view = Table.resetView model.view; drag = NotPressed }
    | PointerDown pt -> { model with drag = Pressed pt }
    | PointerMove pt ->
        // Once a press moves past the threshold it becomes a pan-drag, then tracks the pointer.
        match model.drag with
        | NotPressed -> model
        | Pressed start -> if dist start pt < dragThresholdPx then model else panFrom start pt model
        | Panning last -> panFrom last pt model
    | PointerUp _ ->
        // Only a clean click (a press that never became a drag) (de)selects the table.
        let selection =
            match model.drag with
            | Pressed start ->
                if TableView.tableHit pixelsPerMeter center model.view model.table start
                then TableSelected
                else TableUnselected
            | _ -> model.selection
        { model with selection = selection; drag = NotPressed }
    | Wheel (mods, notches) ->
        match wheelAction mods with
        | RotateView1 -> rotateIfSelected (rotate1 (wheelStepDegrees * float notches)) model
        | RotateView2 -> rotateIfSelected (rotate2 (wheelStepDegrees * float notches)) model
        | RotateView3 -> rotateIfSelected (rotate3 (wheelStepDegrees * float notches)) model
        | ZoomView -> zoomBy notches model
        | NoWheelAction -> model
    | ToggleR3Lock -> { model with r3Locked = not model.r3Locked }
    | RotSetAxis (axis, v) -> rotateIfSelected (setAxis axis v) model
    | RotRequestReset -> { model with rotationConfirm = RotationControls.ConfirmReset }
    | RotRequestResetAll -> { model with rotationConfirm = RotationControls.ConfirmResetAll }
    | RotConfirm ->
        // Reset and Reset All both reset the only rotatable object here (the table).
        let m = match model.rotationConfirm with RotationControls.NoConfirm -> model | _ -> resetRotations model
        { m with rotationConfirm = RotationControls.NoConfirm }
    | RotCancel -> { model with rotationConfirm = RotationControls.NoConfirm }

// ---------------------------------------------------------------------------
// Colours (mirroring the constructor table's palette).
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
// raw events into the pure messages (no model logic in the handlers).
// ---------------------------------------------------------------------------

let private toPoint (sp : ScreenPoint) : Point = Point(sp.sx, sp.sy)

let private projected (model : Model) (p : Vector3) : ScreenPoint =
    TableView.project pixelsPerMeter center model.view p

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

/// Map the model to the shared rotation-controls bar's state (task 008). The "selection" is the
/// table, so the bar is enabled only when the table is selected.
let private rotationState (model : Model) : RotationControls.State =
    {
        r1 = model.view.r1.degrees
        r2 = model.view.r2.degrees
        r3 = model.view.r3.degrees
        r3Locked = model.r3Locked
        enabled = (model.selection = TableSelected)
        confirm = model.rotationConfirm
    }

let private rotationHandlers (dispatch : Msg -> unit) : RotationControls.Handlers =
    {
        rotate = fun axis d -> dispatch (match axis with RotationControls.R1 -> RotateR1By d | RotationControls.R2 -> RotateR2By d | RotationControls.R3 -> RotateR3By d)
        setAngle = fun axis v -> dispatch (RotSetAxis (axis, v))
        toggleR3Lock = fun () -> dispatch ToggleR3Lock
        requestReset = fun () -> dispatch RotRequestReset
        requestResetAll = fun () -> dispatch RotRequestResetAll
        confirm = fun () -> dispatch RotConfirm
        cancel = fun () -> dispatch RotCancel
    }

let private controlBar (model : Model) (dispatch : Msg -> unit) : IView =
    let readout =
        sprintf
            "R1 %.0f°   R2 %.0f°   R3 %.0f°   Zoom %.2f×      Table: %s"
            (degrees model.view.r1) (degrees model.view.r2) (degrees model.view.r3) model.view.zoom
            (match model.selection with TableSelected -> "SELECTED" | TableUnselected -> "not selected")
    StackPanel.create [
        StackPanel.orientation Orientation.Vertical
        StackPanel.spacing 6.0
        StackPanel.margin (Thickness 8.0)
        StackPanel.children [
            RotationControls.view (rotationState model) (rotationHandlers dispatch)
            TextBlock.create [
                TextBlock.name UiIds.readout
                TextBlock.text readout
            ]
            TextBlock.create [
                TextBlock.foreground (brush (color 100 100 100))
                TextBlock.text "click = select table (the controls enable only when the table is SELECTED) · drag = pan · wheel = zoom · Shift+wheel = R1 · Ctrl+Shift+wheel = R2 · Alt+wheel = R3"
            ]
        ]
    ] :> IView

let private tableCanvas (model : Model) : IView =
    Canvas.create [
        Canvas.name UiIds.canvas
        Canvas.width canvasWidth
        Canvas.height canvasHeight
        Canvas.horizontalAlignment HorizontalAlignment.Left
        Canvas.verticalAlignment VerticalAlignment.Top
        Canvas.children (referenceViews model @ plateViews model)
    ] :> IView

let private wheelModifiers (km : KeyModifiers) : Set<WheelModifier> =
    [ if km.HasFlag KeyModifiers.Control then ModCtrl
      if km.HasFlag KeyModifiers.Shift then ModShift
      if km.HasFlag KeyModifiers.Alt then ModAlt ]
    |> Set.ofList

/// The whole test surface. The canvas Border owns the background + pointer handlers; sitting at
/// the content origin, a top-level pointer position equals a canvas coordinate. `e.Handled <-
/// true` drops FuncUI's duplicate Tunnel|Bubble pass, so a wheel notch is exactly one step.
let view (model : Model) (dispatch : Msg -> unit) : IView =
    let toScreen (e : PointerEventArgs) : ScreenPoint = SceneInput.canvasPoint UiIds.canvas e
    DockPanel.create [
        DockPanel.children [
            Border.create [
                Border.dock Dock.Top
                Border.child (controlBar model dispatch)
            ]
            Border.create [
                Border.background (brush (color 250 250 250))
                Border.onPointerPressed (fun e -> e.Handled <- true; dispatch (PointerDown (toScreen e)))
                Border.onPointerMoved (fun e -> e.Handled <- true; dispatch (PointerMove (toScreen e)))
                Border.onPointerReleased (fun e -> e.Handled <- true; dispatch (PointerUp (toScreen e)))
                Border.onPointerWheelChanged (fun e ->
                    e.Handled <- true
                    let notches = if e.Delta.Y >= 0.0 then 1 else -1
                    dispatch (Wheel (wheelModifiers e.KeyModifiers, notches)))
                Border.child (tableCanvas model)
            ]
        ]
    ] :> IView
