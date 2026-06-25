/// The third test window (Spec 0027, task 006 #3): test #1 and test #2 together — rotate the
/// TABLE and the ELEMENTS. The table view is fully live (rotate when selected, pan, zoom) and a
/// few optical elements sit on it, each with its own spec rotation. The rotation gestures act on
/// whatever is SELECTED: click empty table → the table is selected and the gestures rotate the
/// VIEW; click an element → it is selected and the gestures rotate that ELEMENT. The elements are
/// drawn by projecting their table-frame geometry through the (rotatable) table view, so when the
/// table is rotated the elements are "snapped to it" — their own rotation angles do not change,
/// only how they project to the screen does. The `Model`/`Msg`/`update` are pure and Avalonia-free.
module OpticalConstructor.TestWindows.TableAndElementRotationView

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
    let canvas = "TableElementCanvas"
    let rotateR1Minus = "TeRotateR1MinusButton"
    let rotateR1Plus = "TeRotateR1PlusButton"
    let rotateR2Minus = "TeRotateR2MinusButton"
    let rotateR2Plus = "TeRotateR2PlusButton"
    let rotateR3Minus = "TeRotateR3MinusButton"
    let rotateR3Plus = "TeRotateR3PlusButton"
    let unlockR3 = "TeUnlockR3Button"
    let reset = "TeResetButton"
    let readout = "TeReadout"

// ---------------------------------------------------------------------------
// Wheel gesture map: the rotations act on whatever is selected; Ctrl+Alt(+Shift) zoom the
// element(s); a plain / Ctrl wheel zooms the table. Same as tests #1/#2, combined.
// ---------------------------------------------------------------------------

type WheelModifier =
    | ModCtrl
    | ModShift
    | ModAlt

type WheelAction =
    | RotateSel1
    | RotateSel2
    | RotateSel3
    | ZoomElementSelected
    | ZoomElementsAll
    | ZoomTable
    | NoWheelAction

let wheelAction (mods : Set<WheelModifier>) : WheelAction =
    let has m = Set.contains m mods
    match has ModCtrl, has ModShift, has ModAlt with
    | false, true, false -> RotateSel1
    | true, true, false -> RotateSel2
    | false, false, true -> RotateSel3
    | true, false, true -> ZoomElementSelected
    | true, true, true -> ZoomElementsAll
    | false, false, false -> ZoomTable
    | true, false, false -> ZoomTable
    | _ -> NoWheelAction

// ---------------------------------------------------------------------------
// Pure model.
// ---------------------------------------------------------------------------

type TestElement =
    {
        placement : ElementPlacement
        zoom : float
    }

/// What the rotation gestures currently target.
type Selection =
    | TableSelected
    | ElementSelected of int
    | NothingSelected

type DragState =
    | NotPressed
    | Pressed of ScreenPoint
    | Panning of ScreenPoint

type Model =
    {
        table : OpticalTable
        view : TableViewState        // rotatable (when the table is selected) + pannable + zoomable
        elements : TestElement list  // each with its own spec rotation + draw zoom
        selection : Selection
        drag : DragState
        /// The table's R3-lock (task 008): unlocked by default. Elements carry their own R3-lock.
        tableR3Locked : bool
        /// Which reset (if any) the rotation-controls bar is awaiting confirmation for.
        rotationConfirm : RotationControls.ResetConfirm
    }

let defaultElementZoom : float = 5.0

/// A live table view plus three optical elements on the central ray; the table is selected first.
let init () : Model =
    let mk (x : float) (kind : CatalogueKind) : TestElement =
        { placement = ElementPlacement.create kind { x = x * 1.0<meter>; y = 0.0<meter> }; zoom = defaultElementZoom }
    {
        table = Table.defaultTable
        view = Table.defaultView
        elements = [ mk -0.5 LinearPolarizer; mk 0.0 Sample; mk 0.5 FlatMirror ]
        selection = TableSelected
        drag = NotPressed
        tableR3Locked = false
        rotationConfirm = RotationControls.NoConfirm
    }

type Msg =
    | RotateR1By of float
    | RotateR2By of float
    | RotateR3By of float
    /// Lock / unlock the selection's R3 (the table's, or the selected element's).
    | ToggleR3Lock
    /// Rotation-controls bar (task 008): set the selection's axis to an exact angle, and the
    /// confirmation-gated Reset (selection's rotations) / Reset All (every object's rotations).
    | RotSetAxis of RotationControls.Axis * float
    | RotRequestReset
    | RotRequestResetAll
    | RotConfirm
    | RotCancel
    | PointerDown of ScreenPoint
    | PointerMove of ScreenPoint
    | PointerUp of ScreenPoint
    | Wheel of Set<WheelModifier> * int

// ---------------------------------------------------------------------------
// Constants.
// ---------------------------------------------------------------------------

let buttonStepDegrees (shiftHeld : bool) : float = if shiftHeld then 5.0 else 15.0
let wheelStepDegrees : float = 5.0
let private zoomStep : float = 1.1
let private elementZoomMin = 1.0
let private elementZoomMax = 50.0
let private tableZoomMin = 0.2
let private tableZoomMax = 5.0
let private dragThresholdPx : float = 3.0
/// A click within this many pixels of an element's projected centre selects that element; a click
/// further out selects the table.
let private elementSelectRadiusPx : float = 50.0

[<Literal>]
let canvasWidth = 820.0
[<Literal>]
let canvasHeight = 560.0

let center : ScreenPoint = { sx = canvasWidth / 2.0; sy = canvasHeight / 2.0 }
let pixelsPerMeter : float = 200.0

// ---------------------------------------------------------------------------
// Pure update. Angles wrap mod 360.
// ---------------------------------------------------------------------------

let normalizeDegrees (d : float) : float =
    let m = d % 360.0
    if m < 0.0 then m + 360.0 else m

let private bumpAngle (a : Angle) (d : float) : Angle = Angle.degree (normalizeDegrees (a.degrees + d))

let private dist (a : ScreenPoint) (b : ScreenPoint) : float =
    sqrt ((a.sx - b.sx) ** 2.0 + (a.sy - b.sy) ** 2.0)

let private mapElement (i : int) (f : TestElement -> TestElement) (m : Model) : Model =
    { m with elements = m.elements |> List.mapi (fun j e -> if j = i then f e else e) }

/// Rotate the TABLE VIEW about screen axis Rk by `deg`. R3 respects the table's R3-lock (task 008).
let private rotateView (axis : int) (deg : float) (m : Model) : Model =
    match axis with
    | 3 when m.tableR3Locked -> m
    | _ ->
        let v = m.view
        let v' =
            match axis with
            | 1 -> { v with r1 = bumpAngle v.r1 deg }
            | 2 -> { v with r2 = bumpAngle v.r2 deg }
            | _ -> { v with r3 = bumpAngle v.r3 deg }
        { m with view = v' }

/// Rotate ELEMENT `i` about Rk by `deg`, lock-respecting (R3 starts locked).
let private rotateElement (axis : int) (deg : float) (i : int) (m : Model) : Model =
    mapElement i
        (fun e ->
            let p = e.placement
            let p' =
                match axis with
                | 1 -> withR1 (bumpAngle p.r1 deg) p
                | 2 -> withR2 (bumpAngle p.r2 deg) p
                | _ -> withR3 (bumpAngle p.r3 deg) p
            { e with placement = p' })
        m

/// Rotate whatever is selected — the table view, or the selected element (task 006 #3).
let private rotateSelected (axis : int) (deg : float) (m : Model) : Model =
    match m.selection with
    | TableSelected -> rotateView axis deg m
    | ElementSelected i -> rotateElement axis deg i m
    | NothingSelected -> m

let private clamp (lo : float) (hi : float) (x : float) : float = max lo (min hi x)

let private zoomElement (i : int) (notches : int) (m : Model) : Model =
    mapElement i (fun e -> { e with zoom = clamp elementZoomMin elementZoomMax (e.zoom * (zoomStep ** float notches)) }) m

let private zoomAllElements (notches : int) (m : Model) : Model =
    { m with elements = m.elements |> List.map (fun e -> { e with zoom = clamp elementZoomMin elementZoomMax (e.zoom * (zoomStep ** float notches)) }) }

let private zoomTable (notches : int) (m : Model) : Model =
    { m with view = { m.view with zoom = clamp tableZoomMin tableZoomMax (m.view.zoom * (zoomStep ** float notches)) } }

let private panFrom (refPt : ScreenPoint) (pt : ScreenPoint) (m : Model) : Model =
    { m with
        view = { m.view with panX = m.view.panX + (pt.sx - refPt.sx); panY = m.view.panY + (pt.sy - refPt.sy) }
        drag = Panning pt }

let private elementCentreScreen (view : TableViewState) (e : TestElement) : ScreenPoint =
    TableView.project pixelsPerMeter center view
        (Vector3.create (e.placement.placementPoint.x / 1.0<meter>) (e.placement.placementPoint.y / 1.0<meter>) 0.0)

/// A click selects the nearest element within the select radius; else the table if the click landed
/// on the plate; else nothing — clicking outside the table unselects it, exactly like the table test.
let private selectionAt (pt : ScreenPoint) (m : Model) : Selection =
    let nearest =
        m.elements
        |> List.mapi (fun i e -> i, dist pt (elementCentreScreen m.view e))
        |> List.sortBy snd
        |> List.tryHead
    match nearest with
    | Some (i, d) when d <= elementSelectRadiusPx -> ElementSelected i
    | _ -> if TableView.tableHit pixelsPerMeter center m.view m.table pt then TableSelected else NothingSelected

/// Set the selection's axis to an exact angle (lock-respecting for R3, task 008).
let private setSelectedAxis (axis : RotationControls.Axis) (v : float) (m : Model) : Model =
    let a = Angle.degree (normalizeDegrees v)
    match m.selection with
    | TableSelected ->
        match axis with
        | RotationControls.R1 -> { m with view = { m.view with r1 = a } }
        | RotationControls.R2 -> { m with view = { m.view with r2 = a } }
        | RotationControls.R3 -> if m.tableR3Locked then m else { m with view = { m.view with r3 = a } }
    | ElementSelected i ->
        mapElement i (fun e -> { e with placement = (match axis with RotationControls.R1 -> withR1 a | RotationControls.R2 -> withR2 a | RotationControls.R3 -> withR3 a) e.placement }) m
    | NothingSelected -> m

/// Reset rotations only (bypassing locks), keeping pan / zoom / position (task 008).
let private resetViewRotations (m : Model) : Model = { m with view = { m.view with r1 = Angle.zero; r2 = Angle.zero; r3 = Angle.zero } }
let private resetPlacementRotations (p : ElementPlacement) : ElementPlacement = { p with r1 = Angle.zero; r2 = Angle.zero; r3 = Angle.zero }
let private resetSelectionRotations (m : Model) : Model =
    match m.selection with
    | TableSelected -> resetViewRotations m
    | ElementSelected i -> mapElement i (fun e -> { e with placement = resetPlacementRotations e.placement }) m
    | NothingSelected -> m
/// Reset All: every object's rotations — the table view AND all elements (task 008, your call).
let private resetAllRotations (m : Model) : Model =
    { (resetViewRotations m) with elements = m.elements |> List.map (fun e -> { e with placement = resetPlacementRotations e.placement }) }

let update (msg : Msg) (model : Model) : Model =
    match msg with
    | RotateR1By d -> rotateSelected 1 d model
    | RotateR2By d -> rotateSelected 2 d model
    | RotateR3By d -> rotateSelected 3 d model
    | ToggleR3Lock ->
        match model.selection with
        | TableSelected -> { model with tableR3Locked = not model.tableR3Locked }
        | ElementSelected i -> mapElement i (fun e -> { e with placement = setR3Locked (not e.placement.r3Locked) e.placement }) model
        | NothingSelected -> model
    | RotSetAxis (axis, v) -> setSelectedAxis axis v model
    | RotRequestReset -> { model with rotationConfirm = RotationControls.ConfirmReset }
    | RotRequestResetAll -> { model with rotationConfirm = RotationControls.ConfirmResetAll }
    | RotConfirm ->
        let m =
            match model.rotationConfirm with
            | RotationControls.ConfirmReset -> resetSelectionRotations model
            | RotationControls.ConfirmResetAll -> resetAllRotations model
            | RotationControls.NoConfirm -> model
        { m with rotationConfirm = RotationControls.NoConfirm }
    | RotCancel -> { model with rotationConfirm = RotationControls.NoConfirm }
    | PointerDown pt -> { model with drag = Pressed pt }
    | PointerMove pt ->
        match model.drag with
        | NotPressed -> model
        | Pressed start -> if dist start pt < dragThresholdPx then model else panFrom start pt model
        | Panning last -> panFrom last pt model
    | PointerUp _ ->
        // A clean click sets the selection (element or table); a pan-drag leaves it unchanged.
        let selection =
            match model.drag with
            | Pressed start -> selectionAt start model
            | _ -> model.selection
        { model with selection = selection; drag = NotPressed }
    | Wheel (mods, notches) ->
        match wheelAction mods with
        | RotateSel1 -> rotateSelected 1 (wheelStepDegrees * float notches) model
        | RotateSel2 -> rotateSelected 2 (wheelStepDegrees * float notches) model
        | RotateSel3 -> rotateSelected 3 (wheelStepDegrees * float notches) model
        | ZoomElementSelected ->
            match model.selection with
            | ElementSelected i -> zoomElement i notches model
            | TableSelected | NothingSelected -> model
        | ZoomElementsAll -> zoomAllElements notches model
        | ZoomTable -> zoomTable notches model
        | NoWheelAction -> model

// ---------------------------------------------------------------------------
// Geometry (pure): the element's oriented box, in table-frame metres, magnified by the per-element
// draw zoom (the SAME shape as test #2 — it is then projected through the live table view).
// ---------------------------------------------------------------------------

let elementCorners (e : TestElement) : Vector3 list =
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
// Colours + the FuncUI view.
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
let private elementColor = color 60 60 60
let private n1Color = color 30 90 200
let private n2Color = color 210 120 0

let private toPoint (sp : ScreenPoint) : Point = Point(sp.sx, sp.sy)
let private projectPt (view : TableViewState) (p : Vector3) : ScreenPoint = TableView.project pixelsPerMeter center view p
let private v3 (x : float) (y : float) (z : float) : Vector3 = Vector3.create x y z

let private line (a : ScreenPoint) (b : ScreenPoint) (c : Color) (w : float) : IView =
    Line.create [
        Line.startPoint (toPoint a); Line.endPoint (toPoint b); Line.stroke (brush c); Line.strokeThickness w
    ] :> IView

/// The table plate as a 3-D box (so an R2/R3 table tilt is visible), the central ray, and the
/// source / detector markers — all projected through the live view. The top face highlights when
/// the table is the selected object.
let private tableViews (model : Model) : IView list =
    let corners = TableView.plateCorners3D model.table |> List.map (projectPt model.view) |> List.toArray
    let tableSel = (model.selection = TableSelected)
    let topFace = corners.[0 .. 3] |> Array.map toPoint |> Array.toList
    let face =
        Polygon.create [
            Polygon.points topFace
            Polygon.fill (brushA (if tableSel then 0.55 else 0.85) (if tableSel then plateSelectedColor else plateColor))
            Polygon.stroke (brush (if tableSel then selectedStroke else edgeColor))
            Polygon.strokeThickness (if tableSel then 3.0 else 1.5)
        ] :> IView
    let edges =
        TableView.plateEdges
        |> List.map (fun (a, b) -> line { sx = (toPoint corners.[a]).X; sy = (toPoint corners.[a]).Y } { sx = (toPoint corners.[b]).X; sy = (toPoint corners.[b]).Y } edgeColor (if tableSel then 2.0 else 1.0))
    let s = projectPt model.view (RayModel.pointToVector3 RayModel.defaultSourcePoint)
    let d = projectPt model.view (RayModel.pointToVector3 RayModel.defaultDetectorPoint)
    let marker (sp : ScreenPoint) (c : Color) : IView =
        Ellipse.create [
            Ellipse.left (sp.sx - 6.0); Ellipse.top (sp.sy - 6.0); Ellipse.width 12.0; Ellipse.height 12.0
            Ellipse.fill (brush c); Ellipse.stroke (brush edgeColor); Ellipse.strokeThickness 1.0
        ] :> IView
    (face :: edges) @ [ line s d rayColor 2.0; marker s sourceColor; marker d detectorColor ]

let private elementView (i : int) (model : Model) (e : TestElement) : IView list =
    let selected = (model.selection = ElementSelected i)
    let corners = elementCorners e |> List.map (projectPt model.view >> toPoint) |> List.toArray
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
    let centreScr = projectPt model.view (v3 cx cy 0.0)
    let along (n : Vector3) (len : float) = projectPt model.view (v3 (cx + len * n.x) (cy + len * n.y) (len * n.z))
    // The secondary (roll) normal is drawn toward −N2 so the yellow marker points UP on screen rather
    // than down (a drawing convention only — the element's actual orientation is unchanged).
    edges @ [ line centreScr (along n1 (1.8 * half)) n1Color (if selected then 3.0 else 2.0)
              line centreScr (along n2 (-(1.3 * half))) n2Color (if selected then 3.0 else 2.0) ]

let private elementViews (model : Model) : IView list =
    model.elements |> List.mapi (fun i e -> elementView i model e) |> List.concat

/// The shared rotation-controls bar's state for whatever is selected (the table, or an element).
/// When nothing is selected the bar is disabled — there is nothing to rotate (task 008).
let private rotationState (model : Model) : RotationControls.State =
    match model.selection with
    | TableSelected ->
        { r1 = model.view.r1.degrees; r2 = model.view.r2.degrees; r3 = model.view.r3.degrees
          r3Locked = model.tableR3Locked; enabled = true; confirm = model.rotationConfirm }
    | ElementSelected i ->
        let p = (List.item i model.elements).placement
        { r1 = p.r1.degrees; r2 = p.r2.degrees; r3 = p.r3.degrees
          r3Locked = p.r3Locked; enabled = true; confirm = model.rotationConfirm }
    | NothingSelected ->
        { r1 = 0.0; r2 = 0.0; r3 = 0.0; r3Locked = false; enabled = false; confirm = model.rotationConfirm }

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

let private kindName (k : CatalogueKind) : string =
    match k with
    | LightSource -> "Light source" | LinearPolarizer -> "Linear polarizer" | CircularPolarizer -> "Circular polarizer"
    | Sample -> "Sample" | Lens -> "Lens" | FlatMirror -> "Flat mirror" | CurvedMirror -> "Curved mirror" | Detector -> "Detector"

let private readoutText (model : Model) : string =
    match model.selection with
    | TableSelected ->
        sprintf "Selected: TABLE   R1 %.0f°   R2 %.0f°   R3 %.0f°   zoom %.2f×"
            model.view.r1.degrees model.view.r2.degrees model.view.r3.degrees model.view.zoom
    | ElementSelected i ->
        let e = List.item i model.elements
        let p = e.placement
        sprintf "Selected: Element %d (%s)   R1 %.0f°   R2 %.0f°   R3 %.0f° (%s)   zoom %.1f×"
            (i + 1) (kindName p.catalogueKind) p.r1.degrees p.r2.degrees p.r3.degrees
            (if p.r3Locked then "R3 locked" else "R3 free") e.zoom
    | NothingSelected -> "Selected: none   (click the table or an element to select it)"

let private controlBar (model : Model) (dispatch : Msg -> unit) : IView =
    StackPanel.create [
        StackPanel.orientation Orientation.Vertical
        StackPanel.spacing 6.0
        StackPanel.margin (Thickness 8.0)
        StackPanel.children [
            RotationControls.view (rotationState model) (rotationHandlers dispatch)
            TextBlock.create [ TextBlock.name UiIds.readout; TextBlock.text (readoutText model) ]
            TextBlock.create [
                TextBlock.foreground (brush (color 100 100 100))
                TextBlock.text "click TABLE or an element to select it (rotation acts on the selection) · drag = pan · wheel = zoom table · Shift/Ctrl+Shift/Alt+wheel = R1/R2/R3 of the selection · Ctrl+Alt+wheel = zoom element · Ctrl+Alt+Shift+wheel = zoom all · Shift+button = 5°"
            ]
        ]
    ] :> IView

let private wheelModifiers (km : KeyModifiers) : Set<WheelModifier> =
    [ if km.HasFlag KeyModifiers.Control then ModCtrl
      if km.HasFlag KeyModifiers.Shift then ModShift
      if km.HasFlag KeyModifiers.Alt then ModAlt ]
    |> Set.ofList

let private tableCanvas (model : Model) : IView =
    Canvas.create [
        Canvas.name UiIds.canvas
        Canvas.width canvasWidth
        Canvas.height canvasHeight
        Canvas.horizontalAlignment HorizontalAlignment.Left
        Canvas.verticalAlignment VerticalAlignment.Top
        Canvas.children (tableViews model @ elementViews model)
    ] :> IView

let view (model : Model) (dispatch : Msg -> unit) : IView =
    let toScreen (e : PointerEventArgs) : ScreenPoint =
        let p = e.GetPosition null
        { sx = p.X; sy = p.Y }
    DockPanel.create [
        DockPanel.children [
            Border.create [ Border.dock Dock.Bottom; Border.child (controlBar model dispatch) ]
            Border.create [
                Border.background (brush (color 250 250 250))
                Border.onPointerPressed (fun e -> e.Handled <- true; dispatch (PointerDown (toScreen e)))
                Border.onPointerMoved (fun e -> e.Handled <- true; dispatch (PointerMove (toScreen e)))
                Border.onPointerReleased (fun e -> e.Handled <- true; dispatch (PointerUp (toScreen e)))
                Border.onPointerWheelChanged (fun e ->
                    e.Handled <- true
                    dispatch (Wheel (wheelModifiers e.KeyModifiers, (if e.Delta.Y >= 0.0 then 1 else -1))))
                Border.child (tableCanvas model)
            ]
        ]
    ] :> IView
