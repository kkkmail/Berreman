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
open OpticalConstructor.Ui

// ---------------------------------------------------------------------------
// Wheel gestures: rotate the selection (Shift/Ctrl+Shift/Alt) or zoom the table (plain/Ctrl).
// ---------------------------------------------------------------------------

type SnapToReflectedWheelModifier =
    | SnapToReflectedModCtrl
    | SnapToReflectedModShift
    | SnapToReflectedModAlt

type SnapToReflectedWheelAction =
    | SnapToReflectedRotateSel1
    | SnapToReflectedRotateSel2
    | SnapToReflectedRotateSel3
    | SnapToReflectedZoomTable
    | SnapToReflectedNoWheelAction

let snapToReflectedWheelAction (mods : Set<SnapToReflectedWheelModifier>) : SnapToReflectedWheelAction =
    let has m = Set.contains m mods
    match has SnapToReflectedModCtrl, has SnapToReflectedModShift, has SnapToReflectedModAlt with
    | false, true, false -> SnapToReflectedRotateSel1
    | true, true, false -> SnapToReflectedRotateSel2
    | false, false, true -> SnapToReflectedRotateSel3
    | false, false, false -> SnapToReflectedZoomTable
    | true, false, false -> SnapToReflectedZoomTable
    | _ -> SnapToReflectedNoWheelAction

// ---------------------------------------------------------------------------
// Pure model.
// ---------------------------------------------------------------------------

type Selection =
    | SnapToReflectedTableSelected
    | ElementSelected of int
    | NothingSelected

type SnapToReflectedDragState =
    | SnapToReflectedNotPressed
    | SnapToReflectedPressed of ScreenPoint
    | SnapToReflectedPanning of ScreenPoint

type SnapToReflectedModel =
    {
        table : OpticalTable
        view : TableViewState
        /// The three elements, in chain order: [ source ; mirror ; detector ]. The source is the ray
        /// root (fixed at the default source point); the mirror and detector are SNAPPED onto the beam.
        elements : ElementPlacement list
        /// The along-ray gaps of the downstream stops: [ source→mirror ; mirror→detector ].
        gaps : float<meter> list
        selection : Selection
        drag : SnapToReflectedDragState
        tableR3Locked : bool
        rotationConfirm : RotationControls.ResetConfirm
    }

let private wheelStepDegrees : float = 5.0
let private dragThresholdPx : float = 3.0
let private elementSelectRadiusPx : float = 50.0

/// Source on the left, a flat mirror at the table centre tilted R2 = 45° (R3 unlocked so R3 also steers),
/// and a detector half a metre past it. The mirror starts selected so the bar acts on it.
let initSnapToReflected () : SnapToReflectedModel =
    // This test window steers its source freely (spec 0046 locks a fresh source's R1 / R2).
    let source = { ElementPlacement.create LightSource RayModel.defaultSourcePoint with r1Locked = RotationFree; r2Locked = RotationFree }
    let mirror = withR2 (Angle.degree 45.0<deg>) { ElementPlacement.create FlatMirror { x = 0.0<meter>; y = 0.0<meter> } with r3Locked = RotationFree }
    let detector = ElementPlacement.create Detector { x = 0.0<meter>; y = 0.0<meter> }
    {
        table = Table.defaultTable
        view = Table.defaultView
        elements = [ source; mirror; detector ]
        gaps = [ 1.0<meter>; 0.5<meter> ]
        selection = ElementSelected 1
        drag = SnapToReflectedNotPressed
        tableR3Locked = false
        rotationConfirm = RotationControls.NoConfirm
    }

type SnapToReflectedMsg =
    | SnapToReflectedRotateR1By of float
    | SnapToReflectedRotateR2By of float
    | SnapToReflectedRotateR3By of float
    | SnapToReflectedToggleR3Lock
    | SnapToReflectedRotSetAxis of RotationControls.RotationAxis * float
    | SnapToReflectedRotRequestReset
    | SnapToReflectedRotRequestResetAll
    | SnapToReflectedRotConfirm
    | SnapToReflectedRotCancel
    | SnapToReflectedPointerDown of ScreenPoint
    | SnapToReflectedPointerMove of ScreenPoint
    | SnapToReflectedPointerUp of ScreenPoint
    | SnapToReflectedWheel of Set<SnapToReflectedWheelModifier> * int

// ---------------------------------------------------------------------------
// The reflected snap (the spec law this screen demonstrates).
// ---------------------------------------------------------------------------

/// The downstream stops: the mirror REFLECTS (`BeamTree.Reflected`), the detector is the end
/// (`BeamTree.Transmitted`). `snapChain` turns the beam by reflection off the mirror's oriented face,
/// so the detector lands on the reflected ray. Rotating the mirror R2/R3 changes the face normal (and
/// the reflected direction); R1 spins about the face normal and does not change it.
let snapped (m : SnapToReflectedModel) : RayModel.SnappedElement list =
    match m.elements with
    | source :: stops ->
        let specs =
            List.zip stops m.gaps
            |> List.map (fun (placement, gap) ->
                // The branch is DERIVED from the element (task: a flat mirror tracks REFLECTED light, a
                // detector tracks TRANSMITTED) — NOT hardcoded by position. So the chain figures out from
                // the mirror's emission that it must reflect.
                ({ placement = placement
                   gap = gap
                   branch = placement.primaryBranch } : RayModel.RaySegmentSpec))
        RayModel.snapChain source.placementPoint.toVector3 (r1Axis source) specs
    | [] -> []

/// Each element's absolute table-frame position: the source at its placement point, the mirror and
/// detector at their snapped positions.
let elementPositions (m : SnapToReflectedModel) : Vector3 list =
    match m.elements with
    | source :: _ -> source.placementPoint.toVector3 :: (snapped m |> List.map (fun s -> s.position))
    | [] -> []

/// The first element that REFLECTS the beam (its primary branch is `Reflected`) — DERIVED from the
/// elements, not hardcoded. Elements AFTER it are on the reflected beam (here the flat mirror), so they
/// are oriented relative to that beam. `MaxValue` when nothing reflects (a straight transmitted chain).
let reflectorIndex (m : SnapToReflectedModel) : int =
    m.elements
    |> List.tryFindIndex (fun p -> p.primaryBranch = BeamTree.ReflectedBeam)
    |> Option.defaultValue System.Int32.MaxValue

/// The absolute (R2, R3) orienting an element to face the beam in `dir` — now the shared domain helper
/// (`Vector3.beamOrientation`), used by the Main screen too.
let beamOrientation (dir : Vector3) : Angle * Angle =
    let o = dir.beamOrientation
    o.steer, o.tip

/// The placements used to DRAW each element. An element on the REFLECTED beam (after the reflector) is
/// oriented RELATIVE to that beam: its dialled R2/R3 are ADDED to the beam's own absolute R2/R3, so at
/// dialled 0 it faces the reflected beam and a dialled value tilts it relative to the beam (task 014 —
/// "anything on the reflected beam must have R2 and R3 relative to the beam; as we measure them in
/// absolute values we adjust R2/R3 to achieve that"). The source and the mirror keep their absolute
/// orientation — the mirror's R2 = 45 DEFINES the reflection. R1 of the mirror does not move the beam, so
/// the downstream orientation is unaffected by it.
let drawPlacements (m : SnapToReflectedModel) : ElementPlacement list =
    let snaps = snapped m |> List.mapi (fun stopIndex s -> stopIndex + 1, s) |> Map.ofList
    let reflector = reflectorIndex m
    m.elements
    |> List.mapi (fun i p ->
        if i > reflector then
            match Map.tryFind i snaps with
            | Some s ->
                let beamR2, beamR3 = beamOrientation s.incoming
                { p with r2 = beamR2 + p.r2; r3 = beamR3 + p.r3 }
            | None -> p
        else p)

// ---------------------------------------------------------------------------
// Pure update.
// ---------------------------------------------------------------------------

let snapToReflectedNormalizeDegrees (d : float) : float =
    let m = d % 360.0
    if m < 0.0 then m + 360.0 else m

let private bumpAngle (a : Angle) (d : float) : Angle = Angle.degree (snapToReflectedNormalizeDegrees (a.degrees + d) * 1.0<deg>)
let private dist (a : ScreenPoint) (b : ScreenPoint) : float = sqrt ((a.sx - b.sx) ** 2.0 + (a.sy - b.sy) ** 2.0)

let private mapElement (i : int) (f : ElementPlacement -> ElementPlacement) (m : SnapToReflectedModel) : SnapToReflectedModel =
    { m with elements = m.elements |> List.mapi (fun j p -> if j = i then f p else p) }

let private rotateView (axis : int) (deg : float) (m : SnapToReflectedModel) : SnapToReflectedModel =
    if axis = 3 && m.tableR3Locked then m
    else { m with view = TableScene.rotateViewBy (match axis with 1 -> RotationControls.R1 | 2 -> RotationControls.R2 | _ -> RotationControls.R3) (Angle.degree (deg * 1.0<deg>)) m.view }

let private rotateElement (axis : int) (deg : float) (i : int) (m : SnapToReflectedModel) : SnapToReflectedModel =
    mapElement i (fun p ->
        match axis with
        | 1 -> withR1 (bumpAngle p.r1 deg) p
        | 2 -> withR2 (bumpAngle p.r2 deg) p
        | _ -> withR3 (bumpAngle p.r3 deg) p) m

let private rotateSelected (axis : int) (deg : float) (m : SnapToReflectedModel) : SnapToReflectedModel =
    match m.selection with
    | SnapToReflectedTableSelected -> rotateView axis deg m
    | ElementSelected i -> rotateElement axis deg i m
    | NothingSelected -> m

let private panFrom (refPt : ScreenPoint) (pt : ScreenPoint) (m : SnapToReflectedModel) : SnapToReflectedModel =
    { m with view = TableScene.panViewBy refPt pt m.view; drag = SnapToReflectedPanning pt }

let private selectionAt (pt : ScreenPoint) (m : SnapToReflectedModel) : Selection =
    let nearest =
        elementPositions m
        |> List.mapi (fun i pos -> i, dist pt (TableScene.projectOnScene m.view pos))
        |> List.sortBy snd
        |> List.tryHead
    match nearest with
    | Some (i, d) when d <= elementSelectRadiusPx -> ElementSelected i
    | _ ->
        match TableScene.tableHitOnScene m.view m.table pt with
        | InsidePolygon -> SnapToReflectedTableSelected
        | OutsidePolygon -> NothingSelected

let private setSelectedAxis (axis : RotationControls.RotationAxis) (v : float) (m : SnapToReflectedModel) : SnapToReflectedModel =
    let a = Angle.degree (snapToReflectedNormalizeDegrees v * 1.0<deg>)
    match m.selection with
    | SnapToReflectedTableSelected ->
        match axis with
        | RotationControls.R1 -> { m with view = { m.view with r1 = a } }
        | RotationControls.R2 -> { m with view = { m.view with r2 = a } }
        | RotationControls.R3 -> if m.tableR3Locked then m else { m with view = { m.view with r3 = a } }
    | ElementSelected i ->
        mapElement i (fun p -> (match axis with RotationControls.R1 -> withR1 a | RotationControls.R2 -> withR2 a | RotationControls.R3 -> withR3 a) p) m
    | NothingSelected -> m

let private resetViewRotations (m : SnapToReflectedModel) : SnapToReflectedModel = { m with view = { m.view with r1 = Angle.zero; r2 = Angle.zero; r3 = Angle.zero } }
let private resetPlacementRotations (p : ElementPlacement) : ElementPlacement = { p with r1 = Angle.zero; r2 = Angle.zero; r3 = Angle.zero }
let private resetSelectionRotations (m : SnapToReflectedModel) : SnapToReflectedModel =
    match m.selection with
    | SnapToReflectedTableSelected -> resetViewRotations m
    | ElementSelected i -> mapElement i resetPlacementRotations m
    | NothingSelected -> m
let private resetAllRotations (m : SnapToReflectedModel) : SnapToReflectedModel =
    { (resetViewRotations m) with elements = m.elements |> List.map resetPlacementRotations }

let updateSnapToReflected (msg : SnapToReflectedMsg) (model : SnapToReflectedModel) : SnapToReflectedModel =
    match msg with
    | SnapToReflectedRotateR1By d -> rotateSelected 1 d model
    | SnapToReflectedRotateR2By d -> rotateSelected 2 d model
    | SnapToReflectedRotateR3By d -> rotateSelected 3 d model
    | SnapToReflectedToggleR3Lock ->
        match model.selection with
        | SnapToReflectedTableSelected -> { model with tableR3Locked = not model.tableR3Locked }
        | ElementSelected i -> mapElement i (fun p -> setR3Locked p.r3Locked.toggled p) model
        | NothingSelected -> model
    | SnapToReflectedRotSetAxis (axis, v) -> setSelectedAxis axis v model
    | SnapToReflectedRotRequestReset -> { model with rotationConfirm = RotationControls.ConfirmReset }
    | SnapToReflectedRotRequestResetAll -> { model with rotationConfirm = RotationControls.ConfirmResetAll }
    | SnapToReflectedRotConfirm ->
        let m =
            match model.rotationConfirm with
            | RotationControls.ConfirmReset -> resetSelectionRotations model
            | RotationControls.ConfirmResetAll -> resetAllRotations model
            | RotationControls.NoConfirm -> model
        { m with rotationConfirm = RotationControls.NoConfirm }
    | SnapToReflectedRotCancel -> { model with rotationConfirm = RotationControls.NoConfirm }
    | SnapToReflectedPointerDown pt -> { model with drag = SnapToReflectedPressed pt }
    | SnapToReflectedPointerMove pt ->
        match model.drag with
        | SnapToReflectedNotPressed -> model
        | SnapToReflectedPressed start -> if dist start pt < dragThresholdPx then model else panFrom start pt model
        | SnapToReflectedPanning last -> panFrom last pt model
    | SnapToReflectedPointerUp _ ->
        let selection = match model.drag with SnapToReflectedPressed start -> selectionAt start model | _ -> model.selection
        { model with selection = selection; drag = SnapToReflectedNotPressed }
    | SnapToReflectedWheel (mods, notches) ->
        match snapToReflectedWheelAction mods with
        | SnapToReflectedRotateSel1 -> rotateSelected 1 (wheelStepDegrees * float notches) model
        | SnapToReflectedRotateSel2 -> rotateSelected 2 (wheelStepDegrees * float notches) model
        | SnapToReflectedRotateSel3 -> rotateSelected 3 (wheelStepDegrees * float notches) model
        | SnapToReflectedZoomTable -> { model with view = TableScene.zoomViewBy (WheelNotches notches) model.view }
        | SnapToReflectedNoWheelAction -> model

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
    Line.create [ Line.startPoint a.toPoint; Line.endPoint b.toPoint; Line.stroke (brush c); Line.strokeThickness w ] :> IView

let private roleColor (k : CatalogueKind) : Color =
    match k with
    | LightSource -> sourceColor
    | Detector -> detectorColor
    | _ -> mirrorColor

/// One element drawn at its (possibly snapped) position: a marker, its N1 (blue) and N2 (yellow)
/// normals, a face tick for the mirror, and its code.
let private elementMark (m : SnapToReflectedModel) (i : int) (placement : ElementPlacement) (pos : Vector3) : IView list =
    let selected = (m.selection = ElementSelected i)
    let proj = TableScene.projectOnScene m.view
    let cScr = proj pos
    let { primaryNormal = n1; secondaryNormal = n2 } = orientedBasis placement
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
            TextBlock.text placement.catalogueKind.kindCode
            TextBlock.fontWeight (if selected then FontWeight.Bold else FontWeight.SemiBold)
            TextBlock.foreground (brush codeColor)
        ] :> IView
    faceTick @ [ marker ] @ normals @ [ label ]

let private sceneViews (m : SnapToReflectedModel) : IView list =
    let positions = elementPositions m
    let placements = drawPlacements m
    let proj = TableScene.projectOnScene m.view
    let beam = positions |> List.pairwise |> List.map (fun (a, b) -> line (proj a) (proj b) rayColor 2.5)
    let elements = List.mapi2 (fun i p pos -> elementMark m i p pos) placements positions |> List.concat
    TableScene.plateViews m.view m.table (match m.selection with SnapToReflectedTableSelected -> PlateSelected | _ -> PlateNotSelected) @ beam @ elements

let private tableCanvas (model : SnapToReflectedModel) : IView =
    Canvas.create [
        Canvas.name ConstructorUiId.snapReflectedCanvas.value
        Canvas.width TableScene.canvasWidth.value
        Canvas.height TableScene.canvasHeight.value
        Canvas.horizontalAlignment HorizontalAlignment.Left
        Canvas.verticalAlignment VerticalAlignment.Top
        Canvas.children (sceneViews model)
    ] :> IView

let private rotationState (model : SnapToReflectedModel) : RotationControls.State =
    match model.selection with
    | SnapToReflectedTableSelected ->
        { r1 = BarDegrees model.view.r1.degrees; r2 = BarDegrees model.view.r2.degrees; r3 = BarDegrees model.view.r3.degrees
          r1Lock = RotationControls.NotLockable; r2Lock = RotationControls.NotLockable
          r3Lock = (if model.tableR3Locked then RotationControls.AxisLocked else RotationControls.AxisFree); availability = ControlOffered; confirm = model.rotationConfirm }
    | ElementSelected i ->
        let p = List.item i model.elements
        { r1 = BarDegrees p.r1.degrees; r2 = BarDegrees p.r2.degrees; r3 = BarDegrees p.r3.degrees
          r1Lock = RotationControls.NotLockable; r2Lock = RotationControls.NotLockable
          r3Lock = (match p.r3Locked with RotationLocked -> RotationControls.AxisLocked | RotationFree -> RotationControls.AxisFree); availability = ControlOffered; confirm = model.rotationConfirm }
    | NothingSelected ->
        { r1 = BarDegrees 0.0; r2 = BarDegrees 0.0; r3 = BarDegrees 0.0; r1Lock = RotationControls.NotLockable; r2Lock = RotationControls.NotLockable; r3Lock = RotationControls.AxisFree; availability = ControlWithheld; confirm = model.rotationConfirm }

let private rotationHandlers (dispatch : SnapToReflectedMsg -> unit) : RotationControls.Handlers =
    {
        rotate = fun axis d -> dispatch (match axis with RotationControls.R1 -> SnapToReflectedRotateR1By d.value | RotationControls.R2 -> SnapToReflectedRotateR2By d.value | RotationControls.R3 -> SnapToReflectedRotateR3By d.value)
        setAngle = fun axis v -> dispatch (SnapToReflectedRotSetAxis (axis, v.value))
        toggleLock = fun axis -> match axis with RotationControls.R3 -> dispatch SnapToReflectedToggleR3Lock | RotationControls.R1 | RotationControls.R2 -> ()
        requestReset = fun () -> dispatch SnapToReflectedRotRequestReset
        requestResetAll = fun () -> dispatch SnapToReflectedRotRequestResetAll
        confirm = fun () -> dispatch SnapToReflectedRotConfirm
        cancel = fun () -> dispatch SnapToReflectedRotCancel
    }

let private readoutText (m : SnapToReflectedModel) : string =
    let mirror = List.item 1 m.elements
    $"Mirror  R1 %.0f{mirror.r1.degrees}°   R2 %.0f{mirror.r2.degrees}°   R3 %.0f{mirror.r3.degrees}°   ·   the detector is snapped to the REFLECTED beam (rotate R2/R3 to steer it; R1 does nothing)"

let private controlBar (model : SnapToReflectedModel) (dispatch : SnapToReflectedMsg -> unit) : IView =
    StackPanel.create [
        StackPanel.orientation Orientation.Vertical
        StackPanel.spacing 6.0
        StackPanel.margin (Thickness 8.0)
        StackPanel.children [
            RotationControls.view (rotationState model) (rotationHandlers dispatch)
            TextBlock.create [ TextBlock.name ConstructorUiId.snapReflectedReadout.value; TextBlock.text (readoutText model) ]
            TextBlock.create [
                TextBlock.foreground (brush (color 100 100 100))
                TextBlock.text "click the mirror (or source/detector) to select it · the bar / wheel rotate the selection · drag = pan · wheel = zoom table"
            ]
        ]
    ] :> IView

let private wheelModifiers (km : KeyModifiers) : Set<SnapToReflectedWheelModifier> =
    [ if km.HasFlag KeyModifiers.Control then SnapToReflectedModCtrl
      if km.HasFlag KeyModifiers.Shift then SnapToReflectedModShift
      if km.HasFlag KeyModifiers.Alt then SnapToReflectedModAlt ]
    |> Set.ofList

let snapToReflectedView (model : SnapToReflectedModel) (dispatch : SnapToReflectedMsg -> unit) : IView =
    let toScreen (e : PointerEventArgs) : ScreenPoint = SceneInput.canvasPoint ConstructorUiId.snapReflectedCanvas e
    DockPanel.create [
        DockPanel.children [
            Border.create [ Border.dock Dock.Top; Border.child (controlBar model dispatch) ]
            Border.create [
                Border.background (brush (color 250 250 250))
                Border.onPointerPressed (fun e -> e.Handled <- true; dispatch (SnapToReflectedPointerDown (toScreen e)))
                Border.onPointerMoved (fun e -> e.Handled <- true; dispatch (SnapToReflectedPointerMove (toScreen e)))
                Border.onPointerReleased (fun e -> e.Handled <- true; dispatch (SnapToReflectedPointerUp (toScreen e)))
                Border.onPointerWheelChanged (fun e ->
                    e.Handled <- true
                    dispatch (SnapToReflectedWheel (wheelModifiers e.KeyModifiers, (if e.Delta.Y >= 0.0 then 1 else -1))))
                Border.child (tableCanvas model)
            ]
        ]
    ] :> IView
