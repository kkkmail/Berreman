/// The snap-to-reflected-light test window (Spec 0027, task 012): a light source, a flat mirror (seeded
/// at R2 = 45°), and a detector. The light does NOT pass through the mirror — it is only REFLECTED — and
/// the detector SNAPS onto the reflected beam. Rotating the mirror's R2 or R3 re-aims the reflected ray
/// (the detector follows); rotating the mirror's R1 leaves the beam unchanged (R1 is the spin about the
/// face normal, so it does not change where the beam reflects — `RayModel.faceNormal`/`reflect`). The
/// reflected snap is `RayModel.snapChain` with a `BeamTree.Reflected` branch at the mirror. The table is
/// the shared `TableScene` (selectable / rotatable / zoomable / draggable), and the selected element is
/// driven by the shared `RotationControls` bar. `Model`/`Msg`/`update` are pure and Avalonia-free.
module OpticalConstructor.TestWindows.SnapToReflectedView

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
    let canvas = "SnapReflectedCanvas"
    let readout = "SnapReflectedReadout"

// ---------------------------------------------------------------------------
// Wheel gestures: rotate the selection (Shift/Ctrl+Shift/Alt) or zoom the table (plain/Ctrl).
// ---------------------------------------------------------------------------

type WheelModifier =
    | ModCtrl
    | ModShift
    | ModAlt

type WheelAction =
    | RotateSel1
    | RotateSel2
    | RotateSel3
    | ZoomTable
    | NoWheelAction

let wheelAction (mods : Set<WheelModifier>) : WheelAction =
    let has m = Set.contains m mods
    match has ModCtrl, has ModShift, has ModAlt with
    | false, true, false -> RotateSel1
    | true, true, false -> RotateSel2
    | false, false, true -> RotateSel3
    | false, false, false -> ZoomTable
    | true, false, false -> ZoomTable
    | _ -> NoWheelAction

// ---------------------------------------------------------------------------
// Pure model.
// ---------------------------------------------------------------------------

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
        view : TableViewState
        /// The three elements, in chain order: [ source ; mirror ; detector ]. The source is the ray
        /// root (fixed at the default source point); the mirror and detector are SNAPPED onto the beam.
        elements : ElementPlacement list
        /// The along-ray gaps of the downstream stops: [ source→mirror ; mirror→detector ].
        gaps : float<meter> list
        selection : Selection
        drag : DragState
        tableR3Locked : bool
        rotationConfirm : RotationControls.ResetConfirm
    }

let private wheelStepDegrees : float = 5.0
let private dragThresholdPx : float = 3.0
let private elementSelectRadiusPx : float = 50.0

/// Source on the left, a flat mirror at the table centre tilted R2 = 45° (R3 unlocked so R3 also steers),
/// and a detector half a metre past it. The mirror starts selected so the bar acts on it.
let init () : Model =
    let source = ElementPlacement.create LightSource RayModel.defaultSourcePoint
    let mirror = withR2 (Angle.degree 45.0) { ElementPlacement.create FlatMirror { x = 0.0<meter>; y = 0.0<meter> } with r3Locked = false }
    let detector = ElementPlacement.create Detector { x = 0.0<meter>; y = 0.0<meter> }
    {
        table = Table.defaultTable
        view = Table.defaultView
        elements = [ source; mirror; detector ]
        gaps = [ 1.0<meter>; 0.5<meter> ]
        selection = ElementSelected 1
        drag = NotPressed
        tableR3Locked = false
        rotationConfirm = RotationControls.NoConfirm
    }

type Msg =
    | RotateR1By of float
    | RotateR2By of float
    | RotateR3By of float
    | ToggleR3Lock
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
// The reflected snap (the spec law this screen demonstrates).
// ---------------------------------------------------------------------------

/// The downstream stops: the mirror REFLECTS (`BeamTree.Reflected`), the detector is the end
/// (`BeamTree.Transmitted`). `snapChain` turns the beam by reflection off the mirror's oriented face,
/// so the detector lands on the reflected ray. Rotating the mirror R2/R3 changes the face normal (and
/// the reflected direction); R1 spins about the face normal and does not change it.
let snapped (m : Model) : RayModel.SnappedElement list =
    match m.elements with
    | source :: stops ->
        let specs =
            List.zip stops m.gaps
            |> List.mapi (fun i (placement, gap) ->
                ({ placement = placement
                   gap = gap
                   branch = (if i = 0 then BeamTree.Reflected else BeamTree.Transmitted) } : RayModel.RaySegmentSpec))
        RayModel.snapChain (RayModel.pointToVector3 source.placementPoint) (r1Axis source) specs
    | [] -> []

/// Each element's absolute table-frame position: the source at its placement point, the mirror and
/// detector at their snapped positions.
let elementPositions (m : Model) : Vector3 list =
    match m.elements with
    | source :: _ -> RayModel.pointToVector3 source.placementPoint :: (snapped m |> List.map (fun s -> s.position))
    | [] -> []

// ---------------------------------------------------------------------------
// Pure update.
// ---------------------------------------------------------------------------

let normalizeDegrees (d : float) : float =
    let m = d % 360.0
    if m < 0.0 then m + 360.0 else m

let private bumpAngle (a : Angle) (d : float) : Angle = Angle.degree (normalizeDegrees (a.degrees + d))
let private dist (a : ScreenPoint) (b : ScreenPoint) : float = sqrt ((a.sx - b.sx) ** 2.0 + (a.sy - b.sy) ** 2.0)

let private mapElement (i : int) (f : ElementPlacement -> ElementPlacement) (m : Model) : Model =
    { m with elements = m.elements |> List.mapi (fun j p -> if j = i then f p else p) }

let private rotateView (axis : int) (deg : float) (m : Model) : Model =
    if axis = 3 && m.tableR3Locked then m
    else { m with view = TableScene.rotateViewBy axis deg m.view }

let private rotateElement (axis : int) (deg : float) (i : int) (m : Model) : Model =
    mapElement i (fun p ->
        match axis with
        | 1 -> withR1 (bumpAngle p.r1 deg) p
        | 2 -> withR2 (bumpAngle p.r2 deg) p
        | _ -> withR3 (bumpAngle p.r3 deg) p) m

let private rotateSelected (axis : int) (deg : float) (m : Model) : Model =
    match m.selection with
    | TableSelected -> rotateView axis deg m
    | ElementSelected i -> rotateElement axis deg i m
    | NothingSelected -> m

let private panFrom (refPt : ScreenPoint) (pt : ScreenPoint) (m : Model) : Model =
    { m with view = TableScene.panViewBy (pt.sx - refPt.sx) (pt.sy - refPt.sy) m.view; drag = Panning pt }

let private selectionAt (pt : ScreenPoint) (m : Model) : Selection =
    let nearest =
        elementPositions m
        |> List.mapi (fun i pos -> i, dist pt (TableScene.project m.view pos))
        |> List.sortBy snd
        |> List.tryHead
    match nearest with
    | Some (i, d) when d <= elementSelectRadiusPx -> ElementSelected i
    | _ -> if TableScene.tableHit m.view m.table pt then TableSelected else NothingSelected

let private setSelectedAxis (axis : RotationControls.Axis) (v : float) (m : Model) : Model =
    let a = Angle.degree (normalizeDegrees v)
    match m.selection with
    | TableSelected ->
        match axis with
        | RotationControls.R1 -> { m with view = { m.view with r1 = a } }
        | RotationControls.R2 -> { m with view = { m.view with r2 = a } }
        | RotationControls.R3 -> if m.tableR3Locked then m else { m with view = { m.view with r3 = a } }
    | ElementSelected i ->
        mapElement i (fun p -> (match axis with RotationControls.R1 -> withR1 a | RotationControls.R2 -> withR2 a | RotationControls.R3 -> withR3 a) p) m
    | NothingSelected -> m

let private resetViewRotations (m : Model) : Model = { m with view = { m.view with r1 = Angle.zero; r2 = Angle.zero; r3 = Angle.zero } }
let private resetPlacementRotations (p : ElementPlacement) : ElementPlacement = { p with r1 = Angle.zero; r2 = Angle.zero; r3 = Angle.zero }
let private resetSelectionRotations (m : Model) : Model =
    match m.selection with
    | TableSelected -> resetViewRotations m
    | ElementSelected i -> mapElement i resetPlacementRotations m
    | NothingSelected -> m
let private resetAllRotations (m : Model) : Model =
    { (resetViewRotations m) with elements = m.elements |> List.map resetPlacementRotations }

let update (msg : Msg) (model : Model) : Model =
    match msg with
    | RotateR1By d -> rotateSelected 1 d model
    | RotateR2By d -> rotateSelected 2 d model
    | RotateR3By d -> rotateSelected 3 d model
    | ToggleR3Lock ->
        match model.selection with
        | TableSelected -> { model with tableR3Locked = not model.tableR3Locked }
        | ElementSelected i -> mapElement i (fun p -> setR3Locked (not p.r3Locked) p) model
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
        let selection = match model.drag with Pressed start -> selectionAt start model | _ -> model.selection
        { model with selection = selection; drag = NotPressed }
    | Wheel (mods, notches) ->
        match wheelAction mods with
        | RotateSel1 -> rotateSelected 1 (wheelStepDegrees * float notches) model
        | RotateSel2 -> rotateSelected 2 (wheelStepDegrees * float notches) model
        | RotateSel3 -> rotateSelected 3 (wheelStepDegrees * float notches) model
        | ZoomTable -> { model with view = TableScene.zoomViewBy notches model.view }
        | NoWheelAction -> model

// ---------------------------------------------------------------------------
// Colours + the FuncUI view.
// ---------------------------------------------------------------------------

let private color (r : int) (g : int) (b : int) : Color = Color.FromRgb(byte r, byte g, byte b)
let private brush (c : Color) : IBrush = SolidColorBrush(c) :> IBrush

let private rayColor = color 30 90 200
let private edgeColor = color 50 50 50
let private selectedStroke = color 0 60 160
let private sourceColor = color 230 170 30
let private mirrorColor = color 150 150 165
let private detectorColor = color 20 20 20
let private n1Color = color 30 90 200
let private n2Color = color 210 120 0
let private codeColor = color 20 20 30

let private drawScale : float = 3.0
let private v3 (x : float) (y : float) (z : float) : Vector3 = Vector3.create x y z

let private line (a : ScreenPoint) (b : ScreenPoint) (c : Color) (w : float) : IView =
    Line.create [ Line.startPoint (TableScene.toPoint a); Line.endPoint (TableScene.toPoint b); Line.stroke (brush c); Line.strokeThickness w ] :> IView

let private roleColor (k : CatalogueKind) : Color =
    match k with
    | LightSource -> sourceColor
    | Detector -> detectorColor
    | _ -> mirrorColor

/// One element drawn at its (possibly snapped) position: a marker, its N1 (blue) and N2 (yellow)
/// normals, a face tick for the mirror, and its code.
let private elementMark (m : Model) (i : int) (placement : ElementPlacement) (pos : Vector3) : IView list =
    let selected = (m.selection = ElementSelected i)
    let proj = TableScene.project m.view
    let cScr = proj pos
    let (n1, n2, _) = orientedBasis placement
    let half = drawScale * (placement.box.a2 / 2.0 / 1.0<meter>)
    let along (n : Vector3) (len : float) = proj (v3 (pos.x + len * n.x) (pos.y + len * n.y) (pos.z + len * n.z))
    let marker =
        Ellipse.create [
            Ellipse.left (cScr.sx - 7.0); Ellipse.top (cScr.sy - 7.0); Ellipse.width 14.0; Ellipse.height 14.0
            Ellipse.fill (brush (roleColor placement.catalogueKind))
            Ellipse.stroke (brush (if selected then selectedStroke else edgeColor))
            Ellipse.strokeThickness (if selected then 2.5 else 1.0)
        ] :> IView
    let normals =
        [ line cScr (along n1 (1.8 * half)) n1Color (if selected then 3.0 else 2.0)
          line cScr (along n2 (-(1.3 * half))) n2Color (if selected then 3.0 else 2.0) ]
    let faceTick =
        match placement.catalogueKind with
        | FlatMirror | CurvedMirror ->
            let perp = (v3 (-n1.y) n1.x 0.0).normalized   // the reflective face, perpendicular to N1
            let a = proj (v3 (pos.x + half * perp.x) (pos.y + half * perp.y) pos.z)
            let b = proj (v3 (pos.x - half * perp.x) (pos.y - half * perp.y) pos.z)
            [ line a b (color 80 80 95) (if selected then 5.0 else 4.0) ]
        | _ -> []
    let label =
        TextBlock.create [
            TextBlock.left (cScr.sx + 9.0); TextBlock.top (cScr.sy - 24.0)
            TextBlock.text (TableAndElementRotationView.kindCode placement.catalogueKind)
            TextBlock.fontWeight (if selected then FontWeight.Bold else FontWeight.SemiBold)
            TextBlock.foreground (brush codeColor)
        ] :> IView
    faceTick @ [ marker ] @ normals @ [ label ]

let private sceneViews (m : Model) : IView list =
    let positions = elementPositions m
    let proj = TableScene.project m.view
    let beam = positions |> List.pairwise |> List.map (fun (a, b) -> line (proj a) (proj b) rayColor 2.5)
    let elements = List.zip m.elements positions |> List.mapi (fun i (p, pos) -> elementMark m i p pos) |> List.concat
    TableScene.plateViews m.view m.table (m.selection = TableSelected) @ beam @ elements

let private tableCanvas (model : Model) : IView =
    Canvas.create [
        Canvas.name UiIds.canvas
        Canvas.width TableScene.canvasWidth
        Canvas.height TableScene.canvasHeight
        Canvas.horizontalAlignment HorizontalAlignment.Left
        Canvas.verticalAlignment VerticalAlignment.Top
        Canvas.children (sceneViews model)
    ] :> IView

let private rotationState (model : Model) : RotationControls.State =
    match model.selection with
    | TableSelected ->
        { r1 = model.view.r1.degrees; r2 = model.view.r2.degrees; r3 = model.view.r3.degrees
          r3Locked = model.tableR3Locked; enabled = true; confirm = model.rotationConfirm }
    | ElementSelected i ->
        let p = List.item i model.elements
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

let private readoutText (m : Model) : string =
    let mirror = List.item 1 m.elements
    sprintf "Mirror  R1 %.0f°   R2 %.0f°   R3 %.0f°   ·   the detector is snapped to the REFLECTED beam (rotate R2/R3 to steer it; R1 does nothing)"
        mirror.r1.degrees mirror.r2.degrees mirror.r3.degrees

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
                TextBlock.text "click the mirror (or source/detector) to select it · the bar / wheel rotate the selection · drag = pan · wheel = zoom table"
            ]
        ]
    ] :> IView

let private wheelModifiers (km : KeyModifiers) : Set<WheelModifier> =
    [ if km.HasFlag KeyModifiers.Control then ModCtrl
      if km.HasFlag KeyModifiers.Shift then ModShift
      if km.HasFlag KeyModifiers.Alt then ModAlt ]
    |> Set.ofList

let view (model : Model) (dispatch : Msg -> unit) : IView =
    let toScreen (e : PointerEventArgs) : ScreenPoint = SceneInput.canvasPoint UiIds.canvas e
    DockPanel.create [
        DockPanel.children [
            Border.create [ Border.dock Dock.Top; Border.child (controlBar model dispatch) ]
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
