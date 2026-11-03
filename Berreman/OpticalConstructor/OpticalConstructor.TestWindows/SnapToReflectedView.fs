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
open OpticalConstructor.Ui.TableAndElementRotationView

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
// Pure model. The selection is the Main screen's `Selection` (spec 0048 E041: the lower declaration wins).
// ---------------------------------------------------------------------------

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
        tableR3Locked : RotationLock
        rotationConfirm : RotationControls.ResetConfirm
    }

/// A press that never moves past this distance is a click, not a drag.
let private dragThresholdPx : CanvasLength = CanvasLength 3.0

/// A click within this distance of an element's projected centre selects it.
let private elementSelectRadiusPx : CanvasLength = CanvasLength 50.0

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
        selection = ElementSelected (SceneElementIndex 1)
        drag = SnapToReflectedNotPressed
        tableR3Locked = RotationFree
        rotationConfirm = RotationControls.NoConfirm
    }

type SnapToReflectedMsg =
    | SnapToReflectedRotateR1By of BarDegrees
    | SnapToReflectedRotateR2By of BarDegrees
    | SnapToReflectedRotateR3By of BarDegrees
    | SnapToReflectedToggleR3Lock
    | SnapToReflectedRotSetAxis of RotationControls.RotationAxis * BarDegrees
    | SnapToReflectedRotRequestReset
    | SnapToReflectedRotRequestResetAll
    | SnapToReflectedRotConfirm
    | SnapToReflectedRotCancel
    | SnapToReflectedPointerDown of ScreenPoint
    | SnapToReflectedPointerMove of ScreenPoint
    | SnapToReflectedPointerUp of ScreenPoint
    | SnapToReflectedWheel of Set<SnapToReflectedWheelModifier> * WheelNotches

// ---------------------------------------------------------------------------
// The reflected snap (the spec law this screen demonstrates).
// ---------------------------------------------------------------------------

type SnapToReflectedModel with

    /// The elements, each with its index.
    member model.indexedElements : (SceneElementIndex * ElementPlacement) list =
        model.elements |> List.mapi (fun i p -> SceneElementIndex i, p)

    /// The downstream stops: the mirror REFLECTS (`BeamTree.Reflected`), the detector is the end
    /// (`BeamTree.Transmitted`). `snapChain` turns the beam by reflection off the mirror's oriented face,
    /// so the detector lands on the reflected ray. Rotating the mirror R2/R3 changes the face normal (and
    /// the reflected direction); R1 spins about the face normal and does not change it.
    member model.snapped : RayModel.SnappedElement list =
        match model.elements with
        | source :: stops ->
            let specs =
                List.zip stops model.gaps
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
    member model.elementPositions : Vector3 list =
        match model.elements with
        | source :: _ -> source.placementPoint.toVector3 :: (model.snapped |> List.map (fun s -> s.position))
        | [] -> []

    /// The first element that REFLECTS the beam (its primary branch is `Reflected`) — DERIVED from the
    /// elements, not hardcoded. Elements AFTER it are on the reflected beam (here the flat mirror), so they
    /// are oriented relative to that beam. `None` when nothing reflects (a straight transmitted chain).
    member model.reflectorIndex : SceneElementIndex option =
        model.indexedElements
        |> List.tryFind (fun (_, p) -> p.primaryBranch = BeamTree.ReflectedBeam)
        |> Option.map fst

    /// The placements used to DRAW each element. An element on the REFLECTED beam (after the reflector) is
    /// oriented RELATIVE to that beam: its dialled R2/R3 are ADDED to the beam's own absolute R2/R3
    /// (`Vector3.beamOrientation`), so at dialled 0 it faces the reflected beam and a dialled value tilts it
    /// relative to the beam (task 014). The source and the mirror keep their absolute orientation — the
    /// mirror's R2 = 45 DEFINES the reflection, and its R1 does not move the beam.
    member model.drawPlacements : ElementPlacement list =
        let snaps = model.snapped |> List.mapi (fun stopIndex s -> SceneElementIndex (stopIndex + 1), s) |> Map.ofList
        model.indexedElements
        |> List.map (fun (i, p) ->
            match model.reflectorIndex, Map.tryFind i snaps with
            | Some reflector, Some s when i > reflector ->
                let beam = s.incoming.beamOrientation
                { p with r2 = beam.steer + p.r2; r3 = beam.tip + p.r3 }
            | _ -> p)

// ---------------------------------------------------------------------------
// Pure update. Element angles wrap mod 360 through the Main screen's `BarDegrees.turning` /
// `wrappedAngle`; the wheel step and the zoom are the Main screen's too.
// ---------------------------------------------------------------------------

let private mapElement (i : SceneElementIndex) (f : ElementPlacement -> ElementPlacement) (m : SnapToReflectedModel) : SnapToReflectedModel =
    { m with elements = m.indexedElements |> List.map (fun (j, p) -> if j = i then f p else p) }

/// Rotate the table VIEW about `axis` by `d`, unwrapped; R3 respects the table's R3 lock.
let private rotateView (axis : RotationControls.RotationAxis) (d : BarDegrees) (m : SnapToReflectedModel) : SnapToReflectedModel =
    match axis, m.tableR3Locked with
    | RotationControls.R3, RotationLocked -> m
    | _ -> { m with view = TableScene.rotateViewBy axis (Angle.degree (d.value * 1.0<deg>)) m.view }

let private rotateElement (axis : RotationControls.RotationAxis) (d : BarDegrees) (i : SceneElementIndex) (m : SnapToReflectedModel) : SnapToReflectedModel =
    mapElement i (fun p ->
        match axis with
        | RotationControls.R1 -> withR1 (d.turning p.r1) p
        | RotationControls.R2 -> withR2 (d.turning p.r2) p
        | RotationControls.R3 -> withR3 (d.turning p.r3) p) m

let private rotateSelected (axis : RotationControls.RotationAxis) (d : BarDegrees) (m : SnapToReflectedModel) : SnapToReflectedModel =
    match m.selection with
    | TableSelected -> rotateView axis d m
    | ElementSelected i -> rotateElement axis d i m
    | NothingSelected -> m

let private panFrom (refPt : ScreenPoint) (pt : ScreenPoint) (m : SnapToReflectedModel) : SnapToReflectedModel =
    { m with view = TableScene.panViewBy refPt pt m.view; drag = SnapToReflectedPanning pt }

let private selectionAt (pt : ScreenPoint) (m : SnapToReflectedModel) : Selection =
    let nearest =
        m.elementPositions
        |> List.mapi (fun i pos -> SceneElementIndex i, pt.distanceTo (TableScene.projectOnScene m.view pos))
        |> List.sortBy snd
        |> List.tryHead
    match nearest with
    | Some (i, d) when d <= elementSelectRadiusPx -> ElementSelected i
    | _ ->
        match TableScene.tableHitOnScene m.view m.table pt with
        | InsidePolygon -> TableSelected
        | OutsidePolygon -> NothingSelected

let private setSelectedAxis (axis : RotationControls.RotationAxis) (v : BarDegrees) (m : SnapToReflectedModel) : SnapToReflectedModel =
    let a = v.wrappedAngle
    match m.selection with
    | TableSelected ->
        match axis, m.tableR3Locked with
        | RotationControls.R1, _ -> { m with view = { m.view with r1 = a } }
        | RotationControls.R2, _ -> { m with view = { m.view with r2 = a } }
        | RotationControls.R3, RotationLocked -> m
        | RotationControls.R3, RotationFree -> { m with view = { m.view with r3 = a } }
    | ElementSelected i ->
        mapElement i (fun p -> (match axis with RotationControls.R1 -> withR1 a | RotationControls.R2 -> withR2 a | RotationControls.R3 -> withR3 a) p) m
    | NothingSelected -> m

type SnapToReflectedModel with

    /// The view's rotations reset to 0, pan / zoom kept.
    member internal m.withViewRotationsReset : SnapToReflectedModel = { m with view = { m.view with r1 = Angle.zero; r2 = Angle.zero; r3 = Angle.zero } }

    member internal m.withSelectionRotationsReset : SnapToReflectedModel =
        match m.selection with
        | TableSelected -> m.withViewRotationsReset
        | ElementSelected i -> mapElement i (fun p -> p.withRotationsReset) m
        | NothingSelected -> m

    /// Reset All: the table view and every element.
    member internal m.withAllRotationsReset : SnapToReflectedModel =
        { m.withViewRotationsReset with elements = m.elements |> List.map (fun p -> p.withRotationsReset) }

let updateSnapToReflected (msg : SnapToReflectedMsg) (model : SnapToReflectedModel) : SnapToReflectedModel =
    match msg with
    | SnapToReflectedRotateR1By d -> rotateSelected RotationControls.R1 d model
    | SnapToReflectedRotateR2By d -> rotateSelected RotationControls.R2 d model
    | SnapToReflectedRotateR3By d -> rotateSelected RotationControls.R3 d model
    | SnapToReflectedToggleR3Lock ->
        match model.selection with
        | TableSelected -> { model with tableR3Locked = model.tableR3Locked.toggled }
        | ElementSelected i -> mapElement i (fun p -> setR3Locked p.r3Locked.toggled p) model
        | NothingSelected -> model
    | SnapToReflectedRotSetAxis (axis, v) -> setSelectedAxis axis v model
    | SnapToReflectedRotRequestReset -> { model with rotationConfirm = RotationControls.ConfirmReset }
    | SnapToReflectedRotRequestResetAll -> { model with rotationConfirm = RotationControls.ConfirmResetAll }
    | SnapToReflectedRotConfirm ->
        let m =
            match model.rotationConfirm with
            | RotationControls.ConfirmReset -> model.withSelectionRotationsReset
            | RotationControls.ConfirmResetAll -> model.withAllRotationsReset
            | RotationControls.NoConfirm -> model
        { m with rotationConfirm = RotationControls.NoConfirm }
    | SnapToReflectedRotCancel -> { model with rotationConfirm = RotationControls.NoConfirm }
    | SnapToReflectedPointerDown pt -> { model with drag = SnapToReflectedPressed pt }
    | SnapToReflectedPointerMove pt ->
        match model.drag with
        | SnapToReflectedNotPressed -> model
        | SnapToReflectedPressed start -> if start.distanceTo pt < dragThresholdPx then model else panFrom start pt model
        | SnapToReflectedPanning last -> panFrom last pt model
    | SnapToReflectedPointerUp _ ->
        let selection = match model.drag with SnapToReflectedPressed start -> selectionAt start model | _ -> model.selection
        { model with selection = selection; drag = SnapToReflectedNotPressed }
    | SnapToReflectedWheel (mods, notches) ->
        let step = notches.turnedBy wheelStepDegrees
        match snapToReflectedWheelAction mods with
        | SnapToReflectedRotateSel1 -> rotateSelected RotationControls.R1 step model
        | SnapToReflectedRotateSel2 -> rotateSelected RotationControls.R2 step model
        | SnapToReflectedRotateSel3 -> rotateSelected RotationControls.R3 step model
        | SnapToReflectedZoomTable -> { model with view = TableScene.zoomViewBy notches model.view }
        | SnapToReflectedNoWheelAction -> model

// ---------------------------------------------------------------------------
// Colours + the FuncUI view.
// ---------------------------------------------------------------------------

let private rayColor : Color = Color.FromRgb(30uy, 90uy, 200uy)
let private edgeColor : Color = Color.FromRgb(50uy, 50uy, 50uy)
let private selectedStroke : Color = Color.FromRgb(0uy, 60uy, 160uy)
let private sourceColor : Color = Color.FromRgb(230uy, 170uy, 30uy)
let private mirrorColor : Color = Color.FromRgb(150uy, 150uy, 165uy)
let private detectorColor : Color = Color.FromRgb(20uy, 20uy, 20uy)
let private n1Color : Color = Color.FromRgb(30uy, 90uy, 200uy)
let private n2Color : Color = Color.FromRgb(210uy, 120uy, 0uy)
let private faceColor : Color = Color.FromRgb(80uy, 80uy, 95uy)
let private codeColor : Color = Color.FromRgb(20uy, 20uy, 30uy)
let private hintColor : Color = Color.FromRgb(100uy, 100uy, 100uy)
let private canvasBackgroundColor : Color = Color.FromRgb(250uy, 250uy, 250uy)
let private brush (c : Color) : IBrush = SolidColorBrush(c) :> IBrush

/// The element marks' draw magnification: a real element box is a few pixels at table scale.
let private markZoom : BeamLayout.ElementZoom = BeamLayout.ElementZoom 3.0

let private line (a : ScreenPoint) (b : ScreenPoint) (c : Color) (w : StrokeWidth) : IView =
    Line.create [ Line.startPoint a.toPoint; Line.endPoint b.toPoint; Line.stroke (brush c); Line.strokeThickness w.value ] :> IView

let private roleColor (k : CatalogueKind) : Color =
    match k with
    | LightSource -> sourceColor
    | Detector -> detectorColor
    | _ -> mirrorColor

/// One element drawn at its (possibly snapped) position: a marker, its N1 (blue) and N2 (yellow)
/// normals, a face tick for the mirror, and its code.
let private elementMark (m : SnapToReflectedModel) (i : SceneElementIndex) (placement : ElementPlacement) (pos : Vector3) : IView list =
    let selected = (m.selection = ElementSelected i)
    let proj = TableScene.projectOnScene m.view
    let cScr = proj pos
    let { primaryNormal = n1; secondaryNormal = n2 } = orientedBasis placement
    let half = markZoom.value * (placement.box.a2 / 2.0 / 1.0<meter>)
    let along (n : Vector3) (len : float) = proj (Vector3.create (pos.x + len * n.x) (pos.y + len * n.y) (pos.z + len * n.z))
    let normalWidth = if selected then StrokeWidth 3.0 else StrokeWidth 2.0
    let marker =
        Ellipse.create [
            Ellipse.left (cScr.sx - 7.0); Ellipse.top (cScr.sy - 7.0); Ellipse.width 14.0; Ellipse.height 14.0
            Ellipse.fill (brush (roleColor placement.catalogueKind))
            Ellipse.stroke (brush (if selected then selectedStroke else edgeColor))
            Ellipse.strokeThickness (if selected then 2.5 else 1.0)
        ] :> IView
    let normals =
        [ line cScr (along n1 (1.8 * half)) n1Color normalWidth
          line cScr (along n2 (-(1.3 * half))) n2Color normalWidth ]
    let faceTick =
        match placement.catalogueKind with
        | FlatMirror | CurvedMirror ->
            let perp = (Vector3.create (-n1.y) n1.x 0.0).normalized   // the reflective face, perpendicular to N1
            let a = proj (Vector3.create (pos.x + half * perp.x) (pos.y + half * perp.y) pos.z)
            let b = proj (Vector3.create (pos.x - half * perp.x) (pos.y - half * perp.y) pos.z)
            [ line a b faceColor (if selected then StrokeWidth 5.0 else StrokeWidth 4.0) ]
        | _ -> []
    let label =
        TextBlock.create [
            TextBlock.left (cScr.sx + 9.0); TextBlock.top (cScr.sy - 24.0)
            TextBlock.text placement.catalogueKind.kindCode
            TextBlock.fontWeight (if selected then FontWeight.Bold else FontWeight.SemiBold)
            TextBlock.foreground (brush codeColor)
        ] :> IView
    faceTick @ [ marker ] @ normals @ [ label ]

type SnapToReflectedModel with

    member internal m.sceneViews : IView list =
        let positions = m.elementPositions
        let placements = m.drawPlacements |> List.mapi (fun i p -> SceneElementIndex i, p)
        let proj = TableScene.projectOnScene m.view
        let beam = positions |> List.pairwise |> List.map (fun (a, b) -> line (proj a) (proj b) rayColor (StrokeWidth 2.5))
        let elements = List.map2 (fun (i, p) pos -> elementMark m i p pos) placements positions |> List.concat
        let plate = match m.selection with TableSelected -> PlateSelected | ElementSelected _ | NothingSelected -> PlateNotSelected
        TableScene.plateViews m.view m.table plate @ beam @ elements

    member internal model.tableCanvas : IView =
        Canvas.create [
            Canvas.name ConstructorUiId.snapReflectedCanvas.value
            Canvas.width TableScene.canvasWidth.value
            Canvas.height TableScene.canvasHeight.value
            Canvas.horizontalAlignment HorizontalAlignment.Left
            Canvas.verticalAlignment VerticalAlignment.Top
            Canvas.children model.sceneViews
        ] :> IView

    member internal model.rotationState : RotationControls.State =
        match model.selection with
        | TableSelected ->
            { r1 = BarDegrees model.view.r1.degrees; r2 = BarDegrees model.view.r2.degrees; r3 = BarDegrees model.view.r3.degrees
              r1Lock = RotationControls.NotLockable; r2Lock = RotationControls.NotLockable
              r3Lock = (match model.tableR3Locked with RotationLocked -> RotationControls.AxisLocked | RotationFree -> RotationControls.AxisFree)
              availability = ControlOffered; confirm = model.rotationConfirm }
        | ElementSelected i ->
            let p = List.item i.value model.elements
            { r1 = BarDegrees p.r1.degrees; r2 = BarDegrees p.r2.degrees; r3 = BarDegrees p.r3.degrees
              r1Lock = RotationControls.NotLockable; r2Lock = RotationControls.NotLockable
              r3Lock = (match p.r3Locked with RotationLocked -> RotationControls.AxisLocked | RotationFree -> RotationControls.AxisFree)
              availability = ControlOffered; confirm = model.rotationConfirm }
        | NothingSelected ->
            { r1 = BarDegrees 0.0; r2 = BarDegrees 0.0; r3 = BarDegrees 0.0
              r1Lock = RotationControls.NotLockable; r2Lock = RotationControls.NotLockable; r3Lock = RotationControls.AxisFree
              availability = ControlWithheld; confirm = model.rotationConfirm }

    member internal m.readoutText : string =
        let mirror = List.item 1 m.elements
        $"Mirror  R1 %.0f{mirror.r1.degrees}°   R2 %.0f{mirror.r2.degrees}°   R3 %.0f{mirror.r3.degrees}°   ·   the detector is snapped to the REFLECTED beam (rotate R2/R3 to steer it; R1 does nothing)"

let private rotationHandlers (dispatch : SnapToReflectedMsg -> unit) : RotationControls.Handlers =
    {
        rotate = fun axis d -> dispatch (match axis with RotationControls.R1 -> SnapToReflectedRotateR1By d | RotationControls.R2 -> SnapToReflectedRotateR2By d | RotationControls.R3 -> SnapToReflectedRotateR3By d)
        setAngle = fun axis v -> dispatch (SnapToReflectedRotSetAxis (axis, v))
        toggleLock = fun axis -> match axis with RotationControls.R3 -> dispatch SnapToReflectedToggleR3Lock | RotationControls.R1 | RotationControls.R2 -> ()
        requestReset = fun () -> dispatch SnapToReflectedRotRequestReset
        requestResetAll = fun () -> dispatch SnapToReflectedRotRequestResetAll
        confirm = fun () -> dispatch SnapToReflectedRotConfirm
        cancel = fun () -> dispatch SnapToReflectedRotCancel
    }

let private controlBar (model : SnapToReflectedModel) (dispatch : SnapToReflectedMsg -> unit) : IView =
    StackPanel.create [
        StackPanel.orientation Orientation.Vertical
        StackPanel.spacing 6.0
        StackPanel.margin (Thickness 8.0)
        StackPanel.children [
            RotationControls.view model.rotationState (rotationHandlers dispatch)
            TextBlock.create [ TextBlock.name ConstructorUiId.snapReflectedReadout.value; TextBlock.text model.readoutText ]
            TextBlock.create [
                TextBlock.foreground (brush hintColor)
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
                Border.background (brush canvasBackgroundColor)
                Border.onPointerPressed (fun e -> e.Handled <- true; dispatch (SnapToReflectedPointerDown (toScreen e)))
                Border.onPointerMoved (fun e -> e.Handled <- true; dispatch (SnapToReflectedPointerMove (toScreen e)))
                Border.onPointerReleased (fun e -> e.Handled <- true; dispatch (SnapToReflectedPointerUp (toScreen e)))
                Border.onPointerWheelChanged (fun e ->
                    e.Handled <- true
                    dispatch (SnapToReflectedWheel (wheelModifiers e.KeyModifiers, WheelNotches (if e.Delta.Y >= 0.0 then 1 else -1))))
                Border.child model.tableCanvas
            ]
        ]
    ] :> IView
