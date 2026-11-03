/// The second test window (Spec 0027): a simple surface to test OPTICAL ELEMENT rotations,
/// looked at from the top. The table here is FIXED top-down (it is drawn for context but is
/// never rotated / panned / zoomed — that simplification is the whole point). A few optical
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
/// drag does nothing (the table is fixed). The `Model`/`Msg`/`update` are pure and Avalonia-free.
module OpticalConstructor.TestWindows.ElementRotationView

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
open OpticalConstructor.Ui

// ---------------------------------------------------------------------------
// Stable automation ids (CLAUDE.md UI guidance).
// ---------------------------------------------------------------------------

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
// Pure model.
// ---------------------------------------------------------------------------

/// One optical element under test: its spec placement (position + the three rotations + locks +
/// kind + box) and its per-element visual zoom (magnification of the drawn box, NOT its position).
type TestElement =
    {
        placement : ElementPlacement
        zoom : float
    }

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
        elements : TestElement list
        selected : int
        drag : ElementRotationDragState
        /// Which reset (if any) the rotation-controls bar is awaiting confirmation for (task 008).
        rotationConfirm : RotationControls.ResetConfirm
    }

/// The default per-element visual zoom (the user's "default element zoom is 5×"). A constant for
/// now; a future real setting would carry it per element / per project.
let defaultElementZoom : float = 5.0

/// Three optical elements on the central ray, at spec-correct positions (y = 0, on the beam),
/// each fresh (rest pose, R3 locked) at the default 5× draw zoom. The middle one is selected.
let initElementRotation () : ElementRotationModel =
    let mk (x : float) (kind : CatalogueKind) : TestElement =
        { placement = ElementPlacement.create kind { x = x * 1.0<meter>; y = 0.0<meter> }; zoom = defaultElementZoom }
    {
        table = Table.defaultTable
        view = Table.defaultView
        elements = [ mk -0.5 LinearPolarizer; mk 0.0 Sample; mk 0.5 FlatMirror ]
        selected = 1
        drag = ElementRotationNotPressed
        rotationConfirm = RotationControls.NoConfirm
    }

type ElementRotationMsg =
    /// Rotate the SELECTED element about Rk by signed degrees (the buttons; lock-respecting).
    | ElementRotationRotateR1By of float
    | ElementRotationRotateR2By of float
    | ElementRotationRotateR3By of float
    /// Lock / unlock the selected element's R3 (spec A.4.5).
    | ElementRotationToggleR3Lock
    /// Rotation-controls bar (task 008): set an axis to an exact angle, and confirmation-gated
    /// Reset (the selected element's rotations) / Reset All (every element's rotations).
    | ElementRotationRotSetAxis of RotationControls.RotationAxis * float
    | ElementRotationRotRequestReset
    | ElementRotationRotRequestResetAll
    | ElementRotationRotConfirm
    | ElementRotationRotCancel
    /// Zoom (visual magnification) the selected element / all elements by wheel notches.
    | ZoomSelectedBy of int
    | ZoomAllBy of int
    /// Raw left-button gesture messages (click selects the nearest element; a drag is inert).
    | ElementRotationPointerDown of ScreenPoint
    | ElementRotationPointerMove of ScreenPoint
    | ElementRotationPointerUp of ScreenPoint
    /// A wheel notch with its modifiers — resolved against `wheelAction`.
    | ElementRotationWheel of Set<ElementRotationWheelModifier> * int

// ---------------------------------------------------------------------------
// Constants.
// ---------------------------------------------------------------------------

let elementRotationButtonStepDegrees (shiftHeld : bool) : float = if shiftHeld then 5.0 else 15.0
let elementRotationWheelStepDegrees : float = 5.0
let private zoomStep : float = 1.1
let private zoomMin : float = 1.0
let private zoomMax : float = 50.0
let private dragThresholdPx : float = 3.0

// The canvas geometry / projection are the ONE shared optical-table scene (`TableScene`).
let elementRotationCanvasWidth : float = TableScene.canvasWidth.value
let elementRotationCanvasHeight : float = TableScene.canvasHeight.value

// ---------------------------------------------------------------------------
// Pure update. Angles wrap mod 360.
// ---------------------------------------------------------------------------

let elementRotationNormalizeDegrees (d : float) : float =
    let m = d % 360.0
    if m < 0.0 then m + 360.0 else m

let private addDeg (a : Angle) (d : float) : Angle = Angle.degree (elementRotationNormalizeDegrees (a.degrees + d) * 1.0<deg>)

let private clampZoom (z : float) : float = max zoomMin (min zoomMax z)

let private dist (a : ScreenPoint) (b : ScreenPoint) : float =
    sqrt ((a.sx - b.sx) ** 2.0 + (a.sy - b.sy) ** 2.0)

let private mapSelected (f : TestElement -> TestElement) (model : ElementRotationModel) : ElementRotationModel =
    { model with elements = model.elements |> List.mapi (fun i e -> if i = model.selected then f e else e) }

let private editSelectedPlacement (f : ElementPlacement -> ElementPlacement) (model : ElementRotationModel) : ElementRotationModel =
    mapSelected (fun e -> { e with placement = f e.placement }) model

let private rotate1 (deg : float) (model : ElementRotationModel) : ElementRotationModel = editSelectedPlacement (fun p -> withR1 (addDeg p.r1 deg) p) model
let private rotate2 (deg : float) (model : ElementRotationModel) : ElementRotationModel = editSelectedPlacement (fun p -> withR2 (addDeg p.r2 deg) p) model
let private rotate3 (deg : float) (model : ElementRotationModel) : ElementRotationModel = editSelectedPlacement (fun p -> withR3 (addDeg p.r3 deg) p) model

/// Set the selected element's axis to an exact angle (lock-respecting for R3, task 008).
let private setSelectedAxis (axis : RotationControls.RotationAxis) (v : float) (model : ElementRotationModel) : ElementRotationModel =
    let a = Angle.degree (elementRotationNormalizeDegrees v * 1.0<deg>)
    editSelectedPlacement (fun p -> match axis with RotationControls.R1 -> withR1 a p | RotationControls.R2 -> withR2 a p | RotationControls.R3 -> withR3 a p) model

/// Reset rotations only — bypassing locks, keeping zoom / position (task 008 Reset).
let private resetPlacementRotations (p : ElementPlacement) : ElementPlacement = { p with r1 = Angle.zero; r2 = Angle.zero; r3 = Angle.zero }
let private resetSelectedRotations (model : ElementRotationModel) : ElementRotationModel = editSelectedPlacement resetPlacementRotations model
let private resetAllRotations (model : ElementRotationModel) : ElementRotationModel =
    { model with elements = model.elements |> List.map (fun e -> { e with placement = resetPlacementRotations e.placement }) }

let private zoomSelectedBy (notches : int) (model : ElementRotationModel) : ElementRotationModel =
    mapSelected (fun e -> { e with zoom = clampZoom (e.zoom * (zoomStep ** float notches)) }) model

let private zoomAllBy (notches : int) (model : ElementRotationModel) : ElementRotationModel =
    { model with elements = model.elements |> List.map (fun e -> { e with zoom = clampZoom (e.zoom * (zoomStep ** float notches)) }) }

/// Pan the table by the screen delta (#2) — a pure screen-space translation of the view, via TableScene.
let private panFrom (refPt : ScreenPoint) (pt : ScreenPoint) (model : ElementRotationModel) : ElementRotationModel =
    { model with
        view = TableScene.panViewBy refPt pt model.view
        drag = ElementRotationPanning pt }

/// Zoom the table view (#2), bounded [0.2, 5] like the table test (distinct from the per-element draw
/// zoom), via the shared `TableScene`.
let private zoomTableBy (notches : int) (model : ElementRotationModel) : ElementRotationModel =
    { model with view = TableScene.zoomViewBy (WheelNotches notches) model.view }

/// The projected centre of an element (its placement point) under the current view, for click
/// hit-testing.
let private projectedCenter (view : TableViewState) (e : TestElement) : ScreenPoint =
    TableScene.projectOnScene view
        (Vector3.create (e.placement.placementPoint.x / 1.0<meter>) (e.placement.placementPoint.y / 1.0<meter>) 0.0)

/// The index of the element whose projected centre is nearest the click (there are always a few
/// elements, so a click always selects one — no deselect, unlike the table test).
let nearestElement (pt : ScreenPoint) (model : ElementRotationModel) : int option =
    match model.elements with
    | [] -> None
    | _ ->
        model.elements
        |> List.mapi (fun i e -> i, dist pt (projectedCenter model.view e))
        |> List.minBy snd
        |> fst
        |> Some

let updateElementRotation (msg : ElementRotationMsg) (model : ElementRotationModel) : ElementRotationModel =
    match msg with
    | ElementRotationRotateR1By d -> rotate1 d model
    | ElementRotationRotateR2By d -> rotate2 d model
    | ElementRotationRotateR3By d -> rotate3 d model
    | ElementRotationToggleR3Lock -> editSelectedPlacement (fun p -> setR3Locked p.r3Locked.toggled p) model
    | ElementRotationRotSetAxis (axis, v) -> setSelectedAxis axis v model
    | ElementRotationRotRequestReset -> { model with rotationConfirm = RotationControls.ConfirmReset }
    | ElementRotationRotRequestResetAll -> { model with rotationConfirm = RotationControls.ConfirmResetAll }
    | ElementRotationRotConfirm ->
        let m =
            match model.rotationConfirm with
            | RotationControls.ConfirmReset -> resetSelectedRotations model
            | RotationControls.ConfirmResetAll -> resetAllRotations model
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
        | ElementRotationPressed start -> if dist start pt < dragThresholdPx then model else panFrom start pt model
        | ElementRotationPanning last -> panFrom last pt model
    | ElementRotationPointerUp _ ->
        // A clean click (no drag) selects the nearest element; a pan-drag does not change selection.
        let model' =
            match model.drag with
            | ElementRotationPressed start ->
                match nearestElement start model with
                | Some i -> { model with selected = i }
                | None -> model
            | _ -> model
        { model' with drag = ElementRotationNotPressed }
    | ElementRotationWheel (mods, notches) ->
        match elementRotationWheelAction mods with
        | RotateSelectedR1 -> rotate1 (elementRotationWheelStepDegrees * float notches) model
        | RotateSelectedR2 -> rotate2 (elementRotationWheelStepDegrees * float notches) model
        | RotateSelectedR3 -> rotate3 (elementRotationWheelStepDegrees * float notches) model
        | ZoomSelected -> zoomSelectedBy notches model
        | ZoomAll -> zoomAllBy notches model
        | ElementRotationZoomTable -> zoomTableBy notches model
        | ElementRotationNoWheelAction -> model

// ---------------------------------------------------------------------------
// Element geometry (pure): the oriented 3-D box and the N1/N2 axis markers, in table-frame
// metres, from the spec model `Placement.orientedBasis`, scaled by the per-element draw zoom.
// ---------------------------------------------------------------------------

/// The eight corners of the element's oriented bounding box (front face along +N1 first, then the
/// back face), centred at the placement point and magnified by the element's draw zoom.
let elementRotationElementCorners (e : TestElement) : Vector3 list =
    let { primaryNormal = n1; secondaryNormal = n2; thirdAxis = n3 } = orientedBasis e.placement
    let cx = e.placement.placementPoint.x / 1.0<meter>
    let cy = e.placement.placementPoint.y / 1.0<meter>
    let hb = e.zoom * (e.placement.box.b / 2.0 / 1.0<meter>)    // half-depth along N1
    let h1 = e.zoom * (e.placement.box.a1 / 2.0 / 1.0<meter>)   // half-face along N2
    let h2 = e.zoom * (e.placement.box.a2 / 2.0 / 1.0<meter>)   // half-face along N3
    let corner (sb : float) (s1 : float) (s2 : float) : Vector3 =
        Vector3.create
            (cx + sb * hb * n1.x + s1 * h1 * n2.x + s2 * h2 * n3.x)
            (cy + sb * hb * n1.y + s1 * h1 * n2.y + s2 * h2 * n3.y)
            (sb * hb * n1.z + s1 * h1 * n2.z + s2 * h2 * n3.z)
    [ corner 1.0 -1.0 -1.0; corner 1.0 -1.0 1.0; corner 1.0 1.0 1.0; corner 1.0 1.0 -1.0       // +N1 face
      corner -1.0 -1.0 -1.0; corner -1.0 -1.0 1.0; corner -1.0 1.0 1.0; corner -1.0 1.0 -1.0 ] // -N1 face

// ---------------------------------------------------------------------------
// Colours + the FuncUI view.
// ---------------------------------------------------------------------------

let private color (r : int) (g : int) (b : int) : Color = Color.FromRgb(byte r, byte g, byte b)
let private brush (c : Color) : IBrush = SolidColorBrush(c) :> IBrush

let private plateColor = color 205 205 205
let private edgeColor = color 50 50 50
let private rayColor = color 30 90 200
let private sourceColor = color 70 70 70
let private detectorColor = color 20 20 20
let private elementColor = color 60 60 60
let private selectedColor = color 0 60 160
let private n1Color = color 30 90 200      // beam-facing primary normal
let private n2Color = color 210 120 0      // roll marker (secondary normal)

let private toPoint (sp : ScreenPoint) : Point = Point(sp.sx, sp.sy)
let private projectPt (view : TableViewState) (p : Vector3) : ScreenPoint = TableScene.projectOnScene view p
let private v3 (x : float) (y : float) (z : float) : Vector3 = Vector3.create x y z

let private line (a : ScreenPoint) (b : ScreenPoint) (c : Color) (w : float) : IView =
    Line.create [
        Line.startPoint (toPoint a)
        Line.endPoint (toPoint b)
        Line.stroke (brush c)
        Line.strokeThickness w
    ] :> IView

/// The top-down table under the current (pannable / zoomable) view — the shared `TableScene` plate +
/// central ray + source / detector markers. The table is not separately selectable in this test, so the
/// plate is drawn unhighlighted.
let private tableViews (model : ElementRotationModel) : IView list =
    TableScene.plateViews model.view model.table PlateNotSelected
    @ TableScene.sourceDetectorRayViews model.view

/// One element: its oriented box (12 edges), the N1 (beam-facing) axis and the N2 (roll) marker.
/// The selected element is drawn heavier and in the selection colour.
let private elementView (i : int) (model : ElementRotationModel) (e : TestElement) : IView list =
    let selected = (i = model.selected)
    let corners = elementRotationElementCorners e |> List.map (projectPt model.view >> toPoint) |> List.toArray
    let boxColor = if selected then selectedColor else elementColor
    let weight = if selected then 2.5 else 1.0
    let edges =
        TableView.plateEdges
        |> List.map (fun edge ->
            Line.create [
                Line.startPoint (edge.fromCorner.pick corners)
                Line.endPoint (edge.toCorner.pick corners)
                Line.stroke (brush boxColor)
                Line.strokeThickness weight
            ] :> IView)
    let { primaryNormal = n1; secondaryNormal = n2 } = orientedBasis e.placement
    let cx = e.placement.placementPoint.x / 1.0<meter>
    let cy = e.placement.placementPoint.y / 1.0<meter>
    let half = e.zoom * (e.placement.box.a2 / 2.0 / 1.0<meter>)
    let centreScr = projectPt model.view (v3 cx cy 0.0)
    let along (n : Vector3) (len : float) = projectPt model.view (v3 (cx + len * n.x) (cy + len * n.y) (len * n.z))
    let n1Axis = line centreScr (along n1 (1.8 * half)) n1Color (if selected then 3.0 else 2.0)
    // Draw the secondary (roll) normal toward −N2 so the yellow marker points UP on screen rather than
    // down (a drawing convention only — the element's actual orientation is unchanged).
    let n2Axis = line centreScr (along n2 (-(1.3 * half))) n2Color (if selected then 3.0 else 2.0)
    edges @ [ n1Axis; n2Axis ]

let private elementViews (model : ElementRotationModel) : IView list =
    model.elements |> List.mapi (fun i e -> elementView i model e) |> List.concat

// ---------------------------------------------------------------------------
// Controls + readout.
// ---------------------------------------------------------------------------

let private rotationHandlers (dispatch : ElementRotationMsg -> unit) : RotationControls.Handlers =
    {
        rotate = fun axis d -> dispatch (match axis with RotationControls.R1 -> ElementRotationRotateR1By d.value | RotationControls.R2 -> ElementRotationRotateR2By d.value | RotationControls.R3 -> ElementRotationRotateR3By d.value)
        setAngle = fun axis v -> dispatch (ElementRotationRotSetAxis (axis, v.value))
        toggleLock = fun axis -> match axis with RotationControls.R3 -> dispatch ElementRotationToggleR3Lock | RotationControls.R1 | RotationControls.R2 -> ()
        requestReset = fun () -> dispatch ElementRotationRotRequestReset
        requestResetAll = fun () -> dispatch ElementRotationRotRequestResetAll
        confirm = fun () -> dispatch ElementRotationRotConfirm
        cancel = fun () -> dispatch ElementRotationRotCancel
    }

let private kindName (k : CatalogueKind) : string =
    match k with
    | LightSource -> "Light source"
    | LinearPolarizer -> "Linear polarizer"
    | CircularPolarizer -> "Circular polarizer"
    | Sample -> "Sample"
    | Lens -> "Lens"
    | FlatMirror -> "Flat mirror"
    | CurvedMirror -> "Curved mirror"
    | Detector -> "Detector"

let private selectedElement (model : ElementRotationModel) : TestElement = List.item model.selected model.elements

/// The shared rotation-controls bar's state for the SELECTED element. An element is always
/// selected here, so the bar is always enabled.
let private rotationState (model : ElementRotationModel) : RotationControls.State =
    let p = (selectedElement model).placement
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

let private controlBar (model : ElementRotationModel) (dispatch : ElementRotationMsg -> unit) : IView =
    let e = selectedElement model
    let p = e.placement
    let readout =
        $"""Element %d{model.selected + 1}/%d{List.length model.elements} (%s{kindName p.catalogueKind}):  R1 %.0f{p.r1.degrees}°   R2 %.0f{p.r2.degrees}°   R3 %.0f{p.r3.degrees}° (%s{(match p.r3Locked with RotationLocked -> "R3 locked" | RotationFree -> "R3 free")})   zoom %.1f{e.zoom}×"""
    StackPanel.create [
        StackPanel.orientation Orientation.Vertical
        StackPanel.spacing 6.0
        StackPanel.margin (Thickness 8.0)
        StackPanel.children [
            RotationControls.view (rotationState model) (rotationHandlers dispatch)
            TextBlock.create [
                TextBlock.name ConstructorUiId.elemRotationReadout.value
                TextBlock.text readout
            ]
            TextBlock.create [
                TextBlock.foreground (brush (color 100 100 100))
                TextBlock.text "click = select element · drag = pan table · wheel = zoom table · Shift+wheel = R1 · Ctrl+Shift+wheel = R2 · Alt+wheel = R3 (unlock first) · Ctrl+Alt+wheel = zoom element · Ctrl+Alt+Shift+wheel = zoom all · Shift+button = 5°"
            ]
        ]
    ] :> IView

let private wheelModifiers (km : KeyModifiers) : Set<ElementRotationWheelModifier> =
    [ if km.HasFlag KeyModifiers.Control then ElementRotationModCtrl
      if km.HasFlag KeyModifiers.Shift then ElementRotationModShift
      if km.HasFlag KeyModifiers.Alt then ElementRotationModAlt ]
    |> Set.ofList

let private tableCanvas (model : ElementRotationModel) : IView =
    Canvas.create [
        Canvas.name ConstructorUiId.elementRotationCanvas.value
        Canvas.width elementRotationCanvasWidth
        Canvas.height elementRotationCanvasHeight
        Canvas.horizontalAlignment HorizontalAlignment.Left
        Canvas.verticalAlignment VerticalAlignment.Top
        Canvas.children (tableViews model @ elementViews model)
    ] :> IView

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
                Border.background (brush (color 250 250 250))
                Border.onPointerPressed (fun e -> e.Handled <- true; dispatch (ElementRotationPointerDown (toScreen e)))
                Border.onPointerMoved (fun e -> e.Handled <- true; dispatch (ElementRotationPointerMove (toScreen e)))
                Border.onPointerReleased (fun e -> e.Handled <- true; dispatch (ElementRotationPointerUp (toScreen e)))
                Border.onPointerWheelChanged (fun e ->
                    e.Handled <- true
                    let notches = if e.Delta.Y >= 0.0 then 1 else -1
                    dispatch (ElementRotationWheel (wheelModifiers e.KeyModifiers, notches)))
                Border.child (tableCanvas model)
            ]
        ]
    ] :> IView
