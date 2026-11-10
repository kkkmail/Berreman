/// The second test window (Spec 0027): a simple surface to test OPTICAL ELEMENT rotations,
/// looked at from the top. The table here stays top-down (it pans and zooms, task 006 #2, but
/// never rotates — that simplification is the whole point). A few optical
/// elements sit on the central ray at their spec-correct positions (placement point = box
/// centre; rest pose N1 along the central ray, N2 along the table normal, per `Placement.fs`).
/// The selected element rotates per the spec model `Placement.orientedBasis` / `withR1/R2/R3`,
/// and is drawn as its oriented 3-D bounding box plus its N1 (beam-facing) and N2 (roll) axes,
/// projected through the fixed top-down view.
///
/// Controls mirror the table test. Rotation (on the selected element): the R1±/R2±/R3± buttons
/// (15°, Shift = 5°) and the documented modifier-wheel gestures `Shift`+wheel = R1,
/// `Ctrl`+`Shift`+wheel = R2, `Alt`+wheel = R3 (5°/notch, §E.3). R3 starts LOCKED (spec A.1.2)
/// and the R3 controls are inert until "Unlock R3". Element ZOOM (a per-element visual
/// magnification, default 5×): `Ctrl`+`Alt`+wheel zooms the selected element,
/// `Ctrl`+`Alt`+`Shift`+wheel zooms ALL elements. A click selects the nearest element; a plain
/// drag pans the table. The `Model`/`Msg`/`update` are pure and Avalonia-free.
module OpticalConstructor.TestWindows.ElementRotationView

open Avalonia
open Avalonia.Controls
open Avalonia.Input
open Avalonia.Layout
open Avalonia.Media
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open Berreman.Constants
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Placement
open OpticalConstructor.Domain.Table
open OpticalConstructor.Domain.TableView
open OpticalConstructor.Controls
open OpticalConstructor.Ui
open OpticalConstructor.Ui.TableAndElementRotationView

// ---------------------------------------------------------------------------
// Wheel gesture map (mirrors Commands.fs §E.3 for the rotations; the two element-zoom
// gestures are net-new, on the previously-unused Ctrl+Alt / Ctrl+Alt+Shift combinations).
// ---------------------------------------------------------------------------

type ElementRotationWheelModifier =
    | ElementRotationModCtrl
    | ElementRotationModShift
    | ElementRotationModAlt

type ElementRotationWheelAction =
    | RotateSelectedR1
    | RotateSelectedR2
    | RotateSelectedR3
    | ZoomSelected
    | ZoomAll
    | ElementRotationZoomTable
    | ElementRotationNoWheelAction

/// `Shift`→R1, `Ctrl+Shift`→R2, `Alt`→R3 (the documented element-rotation gestures);
/// `Ctrl+Alt`→zoom the selected element, `Ctrl+Alt+Shift`→zoom all elements; a plain or `Ctrl`
/// wheel zooms the TABLE (task 006 #2 — the table is now pan/zoomable like the table test);
/// anything else (e.g. `Shift+Alt`) → nothing.
let elementRotationWheelAction (mods : Set<ElementRotationWheelModifier>) : ElementRotationWheelAction =
    let has m = Set.contains m mods
    match has ElementRotationModCtrl, has ElementRotationModShift, has ElementRotationModAlt with
    | false, true, false -> RotateSelectedR1
    | true, true, false -> RotateSelectedR2
    | false, false, true -> RotateSelectedR3
    | true, false, true -> ZoomSelected
    | true, true, true -> ZoomAll
    | false, false, false -> ElementRotationZoomTable
    | true, false, false -> ElementRotationZoomTable
    | _ -> ElementRotationNoWheelAction

// ---------------------------------------------------------------------------
// Pure model. An element is the Main screen's `Setups.SetupSlot`, its draw zoom included (spec 0048
// E038: the lower declaration wins).
// ---------------------------------------------------------------------------

/// A left-button gesture in progress. `Pressed` is a click candidate; once it moves past the
/// threshold it becomes `Panning` and the drag pans the table (#2 — standard click-and-drag).
type ElementRotationDragState =
    | ElementRotationNotPressed
    | ElementRotationPressed of ScreenPoint
    | ElementRotationPanning of ScreenPoint

type ElementRotationModel =
    {
        table : OpticalTable
        /// The table view (#2): pan + zoom are live (like the table test), but it is never rotated
        /// in THIS test — table rotation is the job of test #3. The elements are projected through
        /// it, so panning / zooming the table moves / scales them with it.
        view : TableViewState
        elements : Setups.SetupSlot list
        selected : SceneElementIndex
        drag : ElementRotationDragState
        /// Which reset (if any) the rotation-controls bar is awaiting confirmation for (task 008).
        rotationConfirm : RotationControls.ResetConfirm
    }

/// Three optical elements on the central ray, at spec-correct positions (y = 0, on the beam),
/// each fresh (rest pose, R3 locked) at the default 5× draw zoom. The middle one is selected.
let initElementRotation () : ElementRotationModel =
    let onRay (id : Library.ElementId) (x : float<meter>) (kind : CatalogueKind) : Setups.SetupSlot =
        {
            id = id
            placement = ElementPlacement.create kind { x = x; y = 0.0<meter> }
            zoom = defaultElementZoom
            beam = BeamLayout.OnMainBeam
            content = Setups.EmptySlotContent
        }
    {
        table = Table.defaultTable
        view = Table.defaultView
        elements =
            [ onRay (Library.ElementId "polarizer") -0.5<meter> LinearPolarizer
              onRay (Library.ElementId "sample") 0.0<meter> Sample
              onRay (Library.ElementId "mirror") 0.5<meter> FlatMirror ]
        selected = SceneElementIndex 1
        drag = ElementRotationNotPressed
        rotationConfirm = RotationControls.NoConfirm
    }

type ElementRotationMsg =
    /// Rotate the SELECTED element about Rk by signed degrees (the buttons; lock-respecting).
    | ElementRotationRotateR1By of BarDegrees
    | ElementRotationRotateR2By of BarDegrees
    | ElementRotationRotateR3By of BarDegrees
    /// Lock / unlock the selected element's R3 (spec A.4.5).
    | ElementRotationToggleR3Lock
    /// Rotation-controls bar (task 008): set an axis to an exact angle, and confirmation-gated
    /// Reset (the selected element's rotations) / Reset All (every element's rotations).
    | ElementRotationRotSetAxis of RotationControls.RotationAxis * BarDegrees
    | ElementRotationRotRequestReset
    | ElementRotationRotRequestResetAll
    | ElementRotationRotConfirm
    | ElementRotationRotCancel
    /// Zoom (visual magnification) the selected element / all elements by wheel notches.
    | ZoomSelectedBy of WheelNotches
    | ZoomAllBy of WheelNotches
    /// Raw left-button gesture messages (click selects the nearest element; a drag pans the table).
    | ElementRotationPointerDown of ScreenPoint
    | ElementRotationPointerMove of ScreenPoint
    | ElementRotationPointerUp of ScreenPoint
    /// A wheel notch with its modifiers — resolved against `elementRotationWheelAction`.
    | ElementRotationWheel of Set<ElementRotationWheelModifier> * WheelNotches

// ---------------------------------------------------------------------------
// Pure update. Angles wrap mod 360 through the Main screen's `BarDegrees.turning` /
// `wrappedAngle`; the wheel step and both zooms are the Main screen's too.
// ---------------------------------------------------------------------------

/// A press that never moves past this distance is a click, not a drag.
let private dragThresholdPx : CanvasLength = CanvasLength 3.0

type ElementRotationModel with

    /// The elements, each with its index.
    member model.indexedElements : (SceneElementIndex * Setups.SetupSlot) list =
        model.elements |> List.mapi (fun i e -> SceneElementIndex i, e)

    /// The selected element (there always is one here).
    member internal model.selectedElement : Setups.SetupSlot = List.item model.selected.value model.elements

    /// The element whose projected centre is nearest `pt` (there are always a few elements, so a
    /// click always selects one — no deselect, unlike the table test).
    member internal model.nearestElement (pt : ScreenPoint) : SceneElementIndex option =
        model.indexedElements
        |> List.map (fun (i, e) -> i, pt.distanceTo (TableScene.projectOnScene model.view e.placement.placementPoint.toVector3))
        |> List.sortBy snd
        |> List.tryHead
        |> Option.map fst

let private mapSelected (f : Setups.SetupSlot -> Setups.SetupSlot) (model : ElementRotationModel) : ElementRotationModel =
    { model with elements = model.indexedElements |> List.map (fun (i, e) -> if i = model.selected then f e else e) }

let private editSelectedPlacement (f : ElementPlacement -> ElementPlacement) (model : ElementRotationModel) : ElementRotationModel =
    mapSelected (fun e -> { e with placement = f e.placement }) model

/// Turn the selected element about `axis` by `d` (`withR3` respects the R3 lock).
let private turnSelected (axis : RotationControls.RotationAxis) (d : BarDegrees) (model : ElementRotationModel) : ElementRotationModel =
    editSelectedPlacement
        (fun p ->
            match axis with
            | RotationControls.R1 -> withR1 (d.turning p.r1) p
            | RotationControls.R2 -> withR2 (d.turning p.r2) p
            | RotationControls.R3 -> withR3 (d.turning p.r3) p)
        model

/// Set the selected element's axis to an exact angle (lock-respecting for R3, task 008).
let private setSelectedAxis (axis : RotationControls.RotationAxis) (v : BarDegrees) (model : ElementRotationModel) : ElementRotationModel =
    let a = v.wrappedAngle
    editSelectedPlacement (fun p -> match axis with RotationControls.R1 -> withR1 a p | RotationControls.R2 -> withR2 a p | RotationControls.R3 -> withR3 a p) model

let private zoomSelectedBy (notches : WheelNotches) (model : ElementRotationModel) : ElementRotationModel =
    mapSelected (fun e -> { e with zoom = e.zoom.zoomedBy notches }) model

let private zoomAllBy (notches : WheelNotches) (model : ElementRotationModel) : ElementRotationModel =
    { model with elements = model.elements |> List.map (fun e -> { e with zoom = e.zoom.zoomedBy notches }) }

/// Pan the table by the screen delta (#2) — a pure screen-space translation of the view, via TableScene.
let private panFrom (refPt : ScreenPoint) (pt : ScreenPoint) (model : ElementRotationModel) : ElementRotationModel =
    { model with
        view = TableScene.panViewBy refPt pt model.view
        drag = ElementRotationPanning pt }

type ElementRotationModel with

    /// Reset rotations only — bypassing locks, keeping zoom / position (task 008 Reset).
    member internal model.withSelectedRotationsReset : ElementRotationModel = editSelectedPlacement (fun p -> p.withRotationsReset) model

    /// Reset All: every element's rotations.
    member internal model.withAllRotationsReset : ElementRotationModel =
        { model with elements = model.elements |> List.map (fun e -> { e with placement = e.placement.withRotationsReset }) }

    /// The shared rotation-controls bar's state for the SELECTED element. An element is always
    /// selected here, so the bar is always enabled.
    member internal model.rotationState : RotationControls.State =
        let p = model.selectedElement.placement
        {
            r1 = BarDegrees p.r1.degrees
            r2 = BarDegrees p.r2.degrees
            r3 = BarDegrees p.r3.degrees
            r1Lock = RotationControls.NotLockable
            r2Lock = RotationControls.NotLockable
            r3Lock = (match p.r3Locked with RotationLocked -> RotationControls.AxisLocked | RotationFree -> RotationControls.AxisFree)
            availability = ControlOffered
            confirm = model.rotationConfirm
        }

let updateElementRotation (msg : ElementRotationMsg) (model : ElementRotationModel) : ElementRotationModel =
    match msg with
    | ElementRotationRotateR1By d -> turnSelected RotationControls.R1 d model
    | ElementRotationRotateR2By d -> turnSelected RotationControls.R2 d model
    | ElementRotationRotateR3By d -> turnSelected RotationControls.R3 d model
    | ElementRotationToggleR3Lock -> editSelectedPlacement (fun p -> setR3Locked p.r3Locked.toggled p) model
    | ElementRotationRotSetAxis (axis, v) -> setSelectedAxis axis v model
    | ElementRotationRotRequestReset -> { model with rotationConfirm = RotationControls.ConfirmReset }
    | ElementRotationRotRequestResetAll -> { model with rotationConfirm = RotationControls.ConfirmResetAll }
    | ElementRotationRotConfirm ->
        let m =
            match model.rotationConfirm with
            | RotationControls.ConfirmReset -> model.withSelectedRotationsReset
            | RotationControls.ConfirmResetAll -> model.withAllRotationsReset
            | RotationControls.NoConfirm -> model
        { m with rotationConfirm = RotationControls.NoConfirm }
    | ElementRotationRotCancel -> { model with rotationConfirm = RotationControls.NoConfirm }
    | ZoomSelectedBy n -> zoomSelectedBy n model
    | ZoomAllBy n -> zoomAllBy n model
    | ElementRotationPointerDown pt -> { model with drag = ElementRotationPressed pt }
    | ElementRotationPointerMove pt ->
        // A press that moves past the threshold becomes a pan-drag, then tracks the pointer.
        match model.drag with
        | ElementRotationNotPressed -> model
        | ElementRotationPressed start -> if start.distanceTo pt < dragThresholdPx then model else panFrom start pt model
        | ElementRotationPanning last -> panFrom last pt model
    | ElementRotationPointerUp _ ->
        // A clean click (no drag) selects the nearest element; a pan-drag does not change selection.
        let model' =
            match model.drag with
            | ElementRotationPressed start ->
                match model.nearestElement start with
                | Some i -> { model with selected = i }
                | None -> model
            | _ -> model
        { model' with drag = ElementRotationNotPressed }
    | ElementRotationWheel (mods, notches) ->
        let step = notches.turnedBy wheelStepDegrees
        match elementRotationWheelAction mods with
        | RotateSelectedR1 -> turnSelected RotationControls.R1 step model
        | RotateSelectedR2 -> turnSelected RotationControls.R2 step model
        | RotateSelectedR3 -> turnSelected RotationControls.R3 step model
        | ZoomSelected -> zoomSelectedBy notches model
        | ZoomAll -> zoomAllBy notches model
        | ElementRotationZoomTable -> { model with view = TableScene.zoomViewBy notches model.view }
        | ElementRotationNoWheelAction -> model

// ---------------------------------------------------------------------------
// The FuncUI view.
// ---------------------------------------------------------------------------

let private hintColor : Color = Color.FromRgb(100uy, 100uy, 100uy)
let private canvasBackgroundColor : Color = Color.FromRgb(250uy, 250uy, 250uy)
let private brush (c : Color) : IBrush = SolidColorBrush(c) :> IBrush

let private rotationHandlers (dispatch : ElementRotationMsg -> unit) : RotationControls.Handlers =
    {
        rotate = fun axis d -> dispatch (match axis with RotationControls.R1 -> ElementRotationRotateR1By d | RotationControls.R2 -> ElementRotationRotateR2By d | RotationControls.R3 -> ElementRotationRotateR3By d)
        setAngle = fun axis v -> dispatch (ElementRotationRotSetAxis (axis, v))
        toggleLock = fun axis -> match axis with RotationControls.R3 -> dispatch ElementRotationToggleR3Lock | RotationControls.R1 | RotationControls.R2 -> ()
        requestReset = fun () -> dispatch ElementRotationRotRequestReset
        requestResetAll = fun () -> dispatch ElementRotationRotRequestResetAll
        confirm = fun () -> dispatch ElementRotationRotConfirm
        cancel = fun () -> dispatch ElementRotationRotCancel
    }

let private controlBar (model : ElementRotationModel) (dispatch : ElementRotationMsg -> unit) : IView =
    let e = model.selectedElement
    let p = e.placement
    let readout =
        $"""Element %d{model.selected.value + 1}/%d{List.length model.elements} (%s{p.catalogueKind.kindName}):  R1 %.0f{p.r1.degrees}°   R2 %.0f{p.r2.degrees}°   R3 %.0f{p.r3.degrees}° (%s{(match p.r3Locked with RotationLocked -> "R3 locked" | RotationFree -> "R3 free")})   zoom %.1f{e.zoom.value}×"""
    StackPanel.create [
        StackPanel.orientation Orientation.Vertical
        StackPanel.spacing 6.0
        StackPanel.margin (Thickness 8.0)
        StackPanel.children [
            RotationControls.view model.rotationState (rotationHandlers dispatch)
            TextBlock.create [
                TextBlock.name ConstructorUiId.elemRotationReadout.value
                TextBlock.text readout
            ]
            TextBlock.create [
                TextBlock.foreground (brush hintColor)
                TextBlock.text "click = select element · drag = pan table · wheel = zoom table · Shift+wheel = R1 · Ctrl+Shift+wheel = R2 · Alt+wheel = R3 (unlock first) · Ctrl+Alt+wheel = zoom element · Ctrl+Alt+Shift+wheel = zoom all · Shift+button = 5°"
            ]
        ]
    ] :> IView

type ElementRotationModel with

    /// The shared `TableScene` plate (unhighlighted: the table is not selectable here), central ray and
    /// source / detector markers, under the live view, and the elements as the test scenes draw them.
    member internal model.tableCanvas : IView =
        let elementViews =
            model.indexedElements
            |> List.collect (fun (i, e) ->
                let highlight = if i = model.selected then ElementRenderer.ElementHighlighted else ElementRenderer.ElementNotHighlighted
                e.testSceneViews model.view highlight)
        Canvas.create [
            Canvas.name ConstructorUiId.elementRotationCanvas.value
            Canvas.width TableScene.canvasWidth.value
            Canvas.height TableScene.canvasHeight.value
            Canvas.horizontalAlignment HorizontalAlignment.Left
            Canvas.verticalAlignment VerticalAlignment.Top
            Canvas.children (TableScene.plateViews model.view model.table PlateNotSelected @ TableScene.sourceDetectorRayViews model.view @ elementViews)
        ] :> IView

let private wheelModifiers (km : KeyModifiers) : Set<ElementRotationWheelModifier> =
    [ if km.HasFlag KeyModifiers.Control then ElementRotationModCtrl
      if km.HasFlag KeyModifiers.Shift then ElementRotationModShift
      if km.HasFlag KeyModifiers.Alt then ElementRotationModAlt ]
    |> Set.ofList

/// The whole element-rotation test surface.
let elementRotationView (model : ElementRotationModel) (dispatch : ElementRotationMsg -> unit) : IView =
    let toScreen (e : PointerEventArgs) : ScreenPoint = SceneInput.canvasPoint ConstructorUiId.elementRotationCanvas e
    DockPanel.create [
        DockPanel.children [
            Border.create [
                Border.dock Dock.Top
                Border.child (controlBar model dispatch)
            ]
            Border.create [
                Border.background (brush canvasBackgroundColor)
                Border.onPointerPressed (fun e -> e.Handled <- true; dispatch (ElementRotationPointerDown (toScreen e)))
                Border.onPointerMoved (fun e -> e.Handled <- true; dispatch (ElementRotationPointerMove (toScreen e)))
                Border.onPointerReleased (fun e -> e.Handled <- true; dispatch (ElementRotationPointerUp (toScreen e)))
                Border.onPointerWheelChanged (fun e ->
                    e.Handled <- true
                    dispatch (ElementRotationWheel (wheelModifiers e.KeyModifiers, WheelNotches (if e.Delta.Y >= 0.0 then 1 else -1))))
                Border.child model.tableCanvas
            ]
        ]
    ] :> IView
