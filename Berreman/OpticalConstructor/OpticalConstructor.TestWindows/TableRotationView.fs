/// The first test window (Spec 0027, task 002-rotate-table): a SIMPLE surface that shows the
/// optical table from the top and proves the table's 3-D rotation + select / unselect work.
/// It reuses the EXACT same table domain as the main app (`Table.defaultTable` /
/// `Table.TableViewState`) and the single-source-of-truth pure projection
/// `OpticalConstructor.Domain.TableView`.
///
/// Rotation is CONSTRAINED, one axis at a time, by the documented gestures (Spec 0026 §E.3 /
/// §E.5, mirroring `OpticalConstructor.Ui/Commands.fs`):
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
open OpticalConstructor.Ui
open OpticalConstructor.Ui.TableAndElementRotationView

// ---------------------------------------------------------------------------
// Pure model (no Avalonia handle).
// ---------------------------------------------------------------------------

/// Whether the table is currently selected (a named condition, not a bool — CLAUDE.md).
type TableSelection =
    | TableRotationTableSelected
    | TableUnselected

/// A left-button gesture in progress. `Pressed` is a click candidate; once the pointer moves
/// past the threshold it becomes `Panning` and the drag PANS the table across the screen (the
/// standard click-and-drag). `Panning` carries the last pointer position. Only a clean
/// `Pressed`→release (no movement) is a click.
type TableRotationDragState =
    | TableRotationNotPressed
    | TableRotationPressed of ScreenPoint
    | TableRotationPanning of ScreenPoint

/// The keyboard modifiers a wheel gesture carries (mirrors `Commands.Modifier`, §E.1/§E.3),
/// as a comparable set so the gesture maps one-to-one to an action.
type TableRotationWheelModifier =
    | TableRotationModCtrl
    | TableRotationModShift
    | TableRotationModAlt

/// The single thing a wheel gesture resolves to (§E.3/§E.5). `NoWheelAction` is the explicit
/// "this combination does nothing" — the scientific-app constraint that a gesture does exactly
/// one thing or nothing, never several at once.
type TableRotationWheelAction =
    | RotateView1
    | RotateView2
    | RotateView3
    | ZoomView
    | TableRotationNoWheelAction

/// The test-window model: the SAME optical table + view-orientation types the main app uses,
/// the binary selection, and the transient left-button gesture state. Nothing Avalonia here.
type TableRotationModel =
    {
        table : OpticalTable
        view : TableViewState
        selection : TableSelection
        drag : TableRotationDragState
        /// The table's R3-lock (task 008): UNLOCKED by default, so the bar reads "Lock R3".
        r3Locked : RotationLock
        /// Which reset (if any) the rotation-controls bar is awaiting confirmation for.
        rotationConfirm : RotationControls.ResetConfirm
    }

/// Straight top-down, default table, nothing selected, no gesture in progress.
let initTableRotation () : TableRotationModel =
    {
        table = Table.defaultTable
        view = Table.defaultView
        selection = TableUnselected
        drag = TableRotationNotPressed
        r3Locked = RotationFree
        rotationConfirm = RotationControls.NoConfirm
    }

type TableRotationMsg =
    /// Rotate the VIEW about screen axis Rk by a signed number of degrees (the buttons).
    | TableRotationRotateR1By of BarDegrees
    | TableRotationRotateR2By of BarDegrees
    | TableRotationRotateR3By of BarDegrees
    /// Reset the view to straight top-down `(0, 0, 0)`.
    | ResetView
    /// Raw left-button gesture messages (the handlers emit these verbatim; the click-vs-drag
    /// logic lives in `update`, so the handlers carry no model and the chain is testable).
    | TableRotationPointerDown of ScreenPoint
    | TableRotationPointerMove of ScreenPoint
    | TableRotationPointerUp of ScreenPoint
    /// A wheel notch (+1 up / -1 down) with its modifiers — resolved against the documented
    /// gesture map in `update`.
    | TableRotationWheel of Set<TableRotationWheelModifier> * WheelNotches
    /// Rotation-controls bar messages (task 008): lock/unlock R3, set an axis to an exact angle,
    /// and the confirmation-gated Reset / Reset All.
    | TableRotationToggleR3Lock
    | TableRotationRotSetAxis of RotationControls.RotationAxis * BarDegrees
    | TableRotationRotRequestReset
    | TableRotationRotRequestResetAll
    | TableRotationRotConfirm
    | TableRotationRotCancel

type TableRotationModel with

    /// The view's rotations reset to 0, pan / zoom kept. The table is the only rotatable object
    /// here, so Reset (selection) and Reset All do the same thing.
    member internal m.withRotationsReset : TableRotationModel =
        { m with view = { m.view with r1 = Angle.zero; r2 = Angle.zero; r3 = Angle.zero } }

    /// The plate is drawn highlighted while the table is selected.
    member internal m.plateHighlight : PlateHighlight =
        match m.selection with
        | TableRotationTableSelected -> PlateSelected
        | TableUnselected -> PlateNotSelected

    /// The shared rotation-controls bar's state (task 008). The "selection" is the table, so the
    /// bar is enabled only when the table is selected.
    member internal m.rotationState : RotationControls.State =
        {
            r1 = BarDegrees m.view.r1.degrees
            r2 = BarDegrees m.view.r2.degrees
            r3 = BarDegrees m.view.r3.degrees
            r1Lock = RotationControls.NotLockable
            r2Lock = RotationControls.NotLockable
            r3Lock = (match m.r3Locked with RotationLocked -> RotationControls.AxisLocked | RotationFree -> RotationControls.AxisFree)
            availability = (match m.selection with TableRotationTableSelected -> ControlOffered | TableUnselected -> ControlWithheld)
            confirm = m.rotationConfirm
        }

/// A press that never moves past this distance is a click, not a drag.
let private dragThresholdPx : CanvasLength = CanvasLength 3.0

// ---------------------------------------------------------------------------
// The documented wheel-gesture map (§E.3/§E.5, mirroring Commands.fs verbatim):
//   {Shift} -> R1 ; {Ctrl,Shift} -> R2 ; {Alt} -> R3 ; {} or {Ctrl} -> zoom ; else nothing.
// ---------------------------------------------------------------------------

let tableRotationWheelAction (mods : Set<TableRotationWheelModifier>) : TableRotationWheelAction =
    let has m = Set.contains m mods
    match has TableRotationModCtrl, has TableRotationModShift, has TableRotationModAlt with
    | false, true, false -> RotateView1
    | true, true, false -> RotateView2
    | false, false, true -> RotateView3
    | false, false, false -> ZoomView
    | true, false, false -> ZoomView
    | _ -> TableRotationNoWheelAction

// ---------------------------------------------------------------------------
// Pure update. Angles wrap mod 360 (no spinors — 370° ≡ 10°) through the Main screen's
// `BarDegrees.turning` / `wrappedAngle`; the wheel step and the zoom are the Main screen's too.
// ---------------------------------------------------------------------------

/// Rotate the view about `axis` by `d`. R3 respects the table's R3-lock (task 008).
let private rotateView (axis : RotationControls.RotationAxis) (d : BarDegrees) (m : TableRotationModel) : TableRotationModel =
    let v = m.view
    match axis, m.r3Locked with
    | RotationControls.R1, _ -> { m with view = { v with r1 = d.turning v.r1 } }
    | RotationControls.R2, _ -> { m with view = { v with r2 = d.turning v.r2 } }
    | RotationControls.R3, RotationLocked -> m
    | RotationControls.R3, RotationFree -> { m with view = { v with r3 = d.turning v.r3 } }

/// Pan the table by the screen delta from a reference point to the current one — the standard
/// click-and-drag move. Pan is a pure screen-space translation (`TableViewState.panX/panY`), so
/// it never rotates; rotation stays the documented modifier+wheel gesture.
let private panFrom (refPt : ScreenPoint) (pt : ScreenPoint) (m : TableRotationModel) : TableRotationModel =
    { m with
        view = { m.view with panX = TablePan (m.view.panX.value + (pt.sx - refPt.sx)); panY = TablePan (m.view.panY.value + (pt.sy - refPt.sy)) }
        drag = TableRotationPanning pt }

/// The table is rotatable ONLY when it is selected (Spec 0027, task 006 #1): a rotation request
/// is dropped unless the table is currently selected. Pan / zoom / reset are not rotations and
/// stay available regardless.
let private rotateIfSelected (f : TableRotationModel -> TableRotationModel) (model : TableRotationModel) : TableRotationModel =
    match model.selection with
    | TableRotationTableSelected -> f model
    | TableUnselected -> model

/// Set the view's axis to an exact angle (mod 360), lock-respecting for R3 (task 008).
let private setAxis (axis : RotationControls.RotationAxis) (d : BarDegrees) (m : TableRotationModel) : TableRotationModel =
    let a = d.wrappedAngle
    match axis, m.r3Locked with
    | RotationControls.R1, _ -> { m with view = { m.view with r1 = a } }
    | RotationControls.R2, _ -> { m with view = { m.view with r2 = a } }
    | RotationControls.R3, RotationLocked -> m
    | RotationControls.R3, RotationFree -> { m with view = { m.view with r3 = a } }

let updateTableRotation (msg : TableRotationMsg) (model : TableRotationModel) : TableRotationModel =
    match msg with
    | TableRotationRotateR1By d -> rotateIfSelected (rotateView RotationControls.R1 d) model
    | TableRotationRotateR2By d -> rotateIfSelected (rotateView RotationControls.R2 d) model
    | TableRotationRotateR3By d -> rotateIfSelected (rotateView RotationControls.R3 d) model
    | ResetView -> { model with view = Table.defaultView; drag = TableRotationNotPressed }
    | TableRotationPointerDown pt -> { model with drag = TableRotationPressed pt }
    | TableRotationPointerMove pt ->
        // Once a press moves past the threshold it becomes a pan-drag, then tracks the pointer.
        match model.drag with
        | TableRotationNotPressed -> model
        | TableRotationPressed start -> if start.distanceTo pt < dragThresholdPx then model else panFrom start pt model
        | TableRotationPanning last -> panFrom last pt model
    | TableRotationPointerUp _ ->
        // Only a clean click (a press that never became a drag) (de)selects the table.
        let selection =
            match model.drag with
            | TableRotationPressed start ->
                match TableScene.tableHitOnScene model.view model.table start with
                | InsidePolygon -> TableRotationTableSelected
                | OutsidePolygon -> TableUnselected
            | _ -> model.selection
        { model with selection = selection; drag = TableRotationNotPressed }
    | TableRotationWheel (mods, notches) ->
        let step = notches.turnedBy wheelStepDegrees
        match tableRotationWheelAction mods with
        | RotateView1 -> rotateIfSelected (rotateView RotationControls.R1 step) model
        | RotateView2 -> rotateIfSelected (rotateView RotationControls.R2 step) model
        | RotateView3 -> rotateIfSelected (rotateView RotationControls.R3 step) model
        | ZoomView -> { model with view = TableScene.zoomViewBy notches model.view }
        | TableRotationNoWheelAction -> model
    | TableRotationToggleR3Lock -> { model with r3Locked = model.r3Locked.toggled }
    | TableRotationRotSetAxis (axis, d) -> rotateIfSelected (setAxis axis d) model
    | TableRotationRotRequestReset -> { model with rotationConfirm = RotationControls.ConfirmReset }
    | TableRotationRotRequestResetAll -> { model with rotationConfirm = RotationControls.ConfirmResetAll }
    | TableRotationRotConfirm ->
        // Reset and Reset All both reset the only rotatable object here (the table).
        let m = match model.rotationConfirm with RotationControls.NoConfirm -> model | _ -> model.withRotationsReset
        { m with rotationConfirm = RotationControls.NoConfirm }
    | TableRotationRotCancel -> { model with rotationConfirm = RotationControls.NoConfirm }

// ---------------------------------------------------------------------------
// The FuncUI view. Geometry is the pure `TableView` projection; the pointer handlers turn
// raw events into the pure messages (no model logic in the handlers).
// ---------------------------------------------------------------------------

let private hintColor : Color = Color.FromRgb(100uy, 100uy, 100uy)
let private canvasBackgroundColor : Color = Color.FromRgb(250uy, 250uy, 250uy)
let private brush (c : Color) : IBrush = SolidColorBrush(c) :> IBrush

let private rotationHandlers (dispatch : TableRotationMsg -> unit) : RotationControls.Handlers =
    {
        rotate = fun axis d -> dispatch (match axis with RotationControls.R1 -> TableRotationRotateR1By d | RotationControls.R2 -> TableRotationRotateR2By d | RotationControls.R3 -> TableRotationRotateR3By d)
        setAngle = fun axis d -> dispatch (TableRotationRotSetAxis (axis, d))
        toggleLock = fun axis -> match axis with RotationControls.R3 -> dispatch TableRotationToggleR3Lock | RotationControls.R1 | RotationControls.R2 -> ()
        requestReset = fun () -> dispatch TableRotationRotRequestReset
        requestResetAll = fun () -> dispatch TableRotationRotRequestResetAll
        confirm = fun () -> dispatch TableRotationRotConfirm
        cancel = fun () -> dispatch TableRotationRotCancel
    }

let private controlBar (model : TableRotationModel) (dispatch : TableRotationMsg -> unit) : IView =
    let readout =
        $"""R1 %.0f{model.view.r1.degrees}°   R2 %.0f{model.view.r2.degrees}°   R3 %.0f{model.view.r3.degrees}°   Zoom %.2f{model.view.zoom.value}×      Table: %s{(match model.selection with TableRotationTableSelected -> "SELECTED" | TableUnselected -> "not selected")}"""
    StackPanel.create [
        StackPanel.orientation Orientation.Vertical
        StackPanel.spacing 6.0
        StackPanel.margin (Thickness 8.0)
        StackPanel.children [
            RotationControls.view model.rotationState (rotationHandlers dispatch)
            TextBlock.create [
                TextBlock.name ConstructorUiId.tableRotationReadout.value
                TextBlock.text readout
            ]
            TextBlock.create [
                TextBlock.foreground (brush hintColor)
                TextBlock.text "click = select table (the controls enable only when the table is SELECTED) · drag = pan · wheel = zoom · Shift+wheel = R1 · Ctrl+Shift+wheel = R2 · Alt+wheel = R3"
            ]
        ]
    ] :> IView

type TableRotationModel with

    /// The plate (highlighted when the table is selected) and the source → detector central ray — both
    /// from the shared `TableScene`, so this test draws the SAME table as every other screen.
    member internal model.tableCanvas : IView =
        Canvas.create [
            Canvas.name ConstructorUiId.tableRotationCanvas.value
            Canvas.width TableScene.canvasWidth.value
            Canvas.height TableScene.canvasHeight.value
            Canvas.horizontalAlignment HorizontalAlignment.Left
            Canvas.verticalAlignment VerticalAlignment.Top
            Canvas.children (TableScene.plateViews model.view model.table model.plateHighlight @ TableScene.sourceDetectorRayViews model.view)
        ] :> IView

let private wheelModifiers (km : KeyModifiers) : Set<TableRotationWheelModifier> =
    [ if km.HasFlag KeyModifiers.Control then TableRotationModCtrl
      if km.HasFlag KeyModifiers.Shift then TableRotationModShift
      if km.HasFlag KeyModifiers.Alt then TableRotationModAlt ]
    |> Set.ofList

/// The whole test surface. The canvas Border owns the background + pointer handlers; sitting at
/// the content origin, a top-level pointer position equals a canvas coordinate. `e.Handled <-
/// true` drops FuncUI's duplicate Tunnel|Bubble pass, so a wheel notch is exactly one step.
let tableRotationView (model : TableRotationModel) (dispatch : TableRotationMsg -> unit) : IView =
    let toScreen (e : PointerEventArgs) : ScreenPoint = SceneInput.canvasPoint ConstructorUiId.tableRotationCanvas e
    DockPanel.create [
        DockPanel.children [
            Border.create [
                Border.dock Dock.Top
                Border.child (controlBar model dispatch)
            ]
            Border.create [
                Border.background (brush canvasBackgroundColor)
                Border.onPointerPressed (fun e -> e.Handled <- true; dispatch (TableRotationPointerDown (toScreen e)))
                Border.onPointerMoved (fun e -> e.Handled <- true; dispatch (TableRotationPointerMove (toScreen e)))
                Border.onPointerReleased (fun e -> e.Handled <- true; dispatch (TableRotationPointerUp (toScreen e)))
                Border.onPointerWheelChanged (fun e ->
                    e.Handled <- true
                    dispatch (TableRotationWheel (wheelModifiers e.KeyModifiers, WheelNotches (if e.Delta.Y >= 0.0 then 1 else -1))))
                Border.child model.tableCanvas
            ]
        ]
    ] :> IView
