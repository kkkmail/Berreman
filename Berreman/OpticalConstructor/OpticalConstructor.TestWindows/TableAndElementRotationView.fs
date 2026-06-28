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
        /// The catalogue kinds the user can ADD to the scene (the "Lego" palette). EMPTY for the static
        /// test windows (no add/remove UI, identical behaviour); the dynamic Main screen seeds it, which
        /// is the ONLY difference between the test scene and the Main scene — same table/zoom/selection/
        /// rotation logic, the Main screen just adds and removes elements at runtime.
        palette : CatalogueKind list
        /// The renderer "large control" config (task 018) — how the Main screen draws the elements
        /// (wireframe ⇄ shapes, cap detail, transparency). Used by `mainView`; the static test `view`
        /// ignores it.
        render : RendererControls.State
        /// The selected Main-screen ribbon Bay, by name (task 018). Used by `mainView` only.
        ribbon : string
        /// Whether the elements SNAP onto the beam (the Main "Lego constructor": the source is the ray
        /// root, downstream elements snap onto the beam reflecting at mirrors). `false` for the static test
        /// windows (free placement, unchanged). Set by `initMain`.
        snapChain : bool
    }

/// The Main-screen ribbon Bay names (the "large controls" the ribbon shows MS-Word-style).
module BayNames =
    let rotation = "Rotation"
    let move = "Move"
    let add = "Add"
    let render = "Render"
    let all = [ rotation; move; add; render ]

let defaultElementZoom : float = 5.0

let private mkElement (x : float) (kind : CatalogueKind) : TestElement =
    { placement = ElementPlacement.create kind { x = x * 1.0<meter>; y = 0.0<meter> }; zoom = defaultElementZoom }

/// The shared scene seed: the standard table, the straight top-down view, the given elements, the
/// table selected first, and an add/remove `palette`. `init` (the test window) passes an empty palette;
/// `initMain` (the Main screen) passes a non-empty one — that is the ONLY difference between them.
let initWith (elements : TestElement list) (palette : CatalogueKind list) : Model =
    {
        table = Table.defaultTable
        view = Table.defaultView
        elements = elements
        selection = TableSelected
        drag = NotPressed
        tableR3Locked = false
        rotationConfirm = RotationControls.NoConfirm
        palette = palette
        render = RendererControls.defaultState
        ribbon = BayNames.rotation
        snapChain = false
    }

/// The STATIC test scene (Spec 0027, task 006 #3): a live table plus three fixed optical elements on
/// the central ray, no add/remove palette. Behaviour is unchanged from before — the palette is empty.
let init () : Model =
    initWith [ mkElement -0.5 LinearPolarizer; mkElement 0.0 Sample; mkElement 0.5 FlatMirror ] []

/// The DYNAMIC Main scene: the same table/view/selection/rotation logic, seeded with a light source and
/// a detector at the ends of the beam, plus the catalogue palette the user can add elements from (the
/// "Lego constructor"). This is the Main screen — identical scene logic, elements added/removed at runtime.
let initMain () : Model =
    // The light source snaps to the table's LEFT edge and the detector to the RIGHT edge — i.e. the
    // central-ray endpoints, which sit exactly on the plate edges (`defaultSourceDetectorDistance` = the
    // table length). Added elements land between them on the beam.
    { initWith
        [ { placement = ElementPlacement.create LightSource RayModel.defaultSourcePoint; zoom = defaultElementZoom }
          { placement = ElementPlacement.create Detector RayModel.defaultDetectorPoint; zoom = defaultElementZoom } ]
        [ LinearPolarizer; CircularPolarizer; Sample; Lens; FlatMirror; CurvedMirror; Detector ]
        with snapChain = true }

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
    /// Lego constructor (Main screen): add a catalogue element to the scene (selected on add), or
    /// remove the currently-selected element. Never dispatched by the test windows (empty palette).
    | AddElement of CatalogueKind
    | RemoveSelected
    /// Main-screen MOVE bay: slide the selected element along the beam (its x), clamped to the plate.
    | SlideSelectedBy of float
    | SlideSelectedTo of float
    | ResetSelectedPosition
    /// Main-screen RENDER bay (task 018): tune how the elements are drawn.
    | RenderSwap
    | RenderSetRailsIndex of int
    | RenderSetCircles of int
    | RenderSetRadialsIndex of int
    | RenderSetRailOpacity of float
    | RenderSetFaceOpacity of float
    | RenderSetLineOpacity of float
    /// Main-screen ribbon: show this Bay (large control) by name.
    | SelectBay of string

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

/// The element's along-beam parameter (its x), in metres.
let elementX (e : TestElement) : float = e.placement.placementPoint.x / 1.0<meter>

/// The Main-screen snap (task — the Main window "follows the same approach" as the snap-to-reflected test):
/// the source is the ray root and every downstream element SNAPS onto the (possibly reflected) beam. The
/// non-source elements are ordered along the beam (by x); consecutive x-distances are the gaps; each
/// element's branch is DERIVED (`RayModel.primaryBranch` — a flat mirror REFLECTS, a polarizer / sample /
/// lens TRANSMITS). Returns each element's drawn table-frame position BY INDEX and the beam polyline
/// (source → each node in beam order). For the free-placement test scene (`snapChain = false`) it is just
/// each element at its own placement point, so selection / drawing are unchanged there.
/// One element's Main-screen draw info: its snapped table-frame centre and the beam direction ARRIVING at
/// it (the source — the ray root — has no incoming).
type MainNode =
    {
        position : Vector3
        incoming : Vector3 option
    }

let mainSnap (m : Model) : Map<int, MainNode> * Vector3 list =
    let freePos (e : TestElement) : Vector3 = Vector3.create (elementX e) (e.placement.placementPoint.y / 1.0<meter>) 0.0
    if not m.snapChain then
        (m.elements |> List.mapi (fun i e -> i, { position = freePos e; incoming = None }) |> Map.ofList), []
    else
        match m.elements with
        | [] -> Map.empty, []
        | source :: rest ->
            let sourceV = RayModel.pointToVector3 source.placement.placementPoint
            let ordered = rest |> List.mapi (fun j e -> j + 1, e) |> List.sortBy (fun (_, e) -> elementX e)
            let _, specsRev =
                ordered
                |> List.fold (fun (prev, acc) (i, e) ->
                    let s = elementX e
                    let gap = (max 0.0 (s - prev)) * 1.0<meter>
                    let spec : RayModel.RaySegmentSpec = { placement = e.placement; gap = gap; branch = RayModel.primaryBranch e.placement }
                    (s, (i, spec) :: acc)) (elementX source, [])
            let specs = List.rev specsRev
            let snaps = RayModel.snapChain sourceV (r1Axis source.placement) (specs |> List.map snd)
            let downstream = List.map2 (fun (i, _) (s : RayModel.SnappedElement) -> i, { position = s.position; incoming = Some s.incoming }) specs snaps
            let nodes = (0, { position = sourceV; incoming = None }) :: downstream |> Map.ofList
            let beamPath = sourceV :: (snaps |> List.map (fun s -> s.position))
            nodes, beamPath

/// Each element's drawn table-frame position by index (the Main scene snaps; the test scene is free).
let snappedCentres (m : Model) : Map<int, Vector3> = fst (mainSnap m) |> Map.map (fun _ n -> n.position)

/// A click selects the nearest element (by its DRAWN position) within the select radius; else the table if
/// the click landed on the plate; else nothing — clicking outside the table unselects it.
let private selectionAt (pt : ScreenPoint) (m : Model) : Selection =
    let centres = snappedCentres m
    let nearest =
        m.elements
        |> List.mapi (fun i _ -> i, dist pt (TableView.project pixelsPerMeter center m.view centres.[i]))
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

// ---------------------------------------------------------------------------
// Lego constructor (Main screen): add / remove elements at runtime. The scene logic is otherwise
// identical to the static test scene — these just grow / shrink the `elements` list.
// ---------------------------------------------------------------------------

/// Append a catalogue element to the scene and select it. New elements are spread along the beam so
/// they do not land exactly on top of one another; the user then rotates / configures the selection.
let private addElement (kind : CatalogueKind) (m : Model) : Model =
    let middleCount =
        m.elements
        |> List.filter (fun e -> e.placement.catalogueKind <> LightSource && e.placement.catalogueKind <> Detector)
        |> List.length
    let x = -0.3 + 0.2 * float middleCount
    let e = { placement = ElementPlacement.create kind { x = x * 1.0<meter>; y = 0.0<meter> }; zoom = defaultElementZoom }
    let elements' = m.elements @ [ e ]
    { m with elements = elements'; selection = ElementSelected (List.length elements' - 1) }

/// Remove the currently-selected element (inert when the table or nothing is selected). Selection
/// drops to nothing so the bar disables until the user picks another object.
let private removeSelected (m : Model) : Model =
    match m.selection with
    | ElementSelected i when i >= 0 && i < List.length m.elements ->
        let elements' = m.elements |> List.mapi (fun j e -> j, e) |> List.filter (fun (j, _) -> j <> i) |> List.map snd
        { m with elements = elements'; selection = NothingSelected }
    | _ -> m

// ---------------------------------------------------------------------------
// Main-screen MOVE bay: slide the selected element along the beam (its x), clamped to the plate.
// ---------------------------------------------------------------------------

/// The plate half-length — the clamp for along-beam motion, in metres.
let plateHalfLength (m : Model) : float = (m.table.length / 2.0) / 1.0<meter>

let private setElementX (i : int) (x : float) (m : Model) : Model =
    let half = plateHalfLength m
    let x' = max (-half) (min half x)
    mapElement i (fun e -> { e with placement = { e.placement with placementPoint = { e.placement.placementPoint with x = x' * 1.0<meter> } } }) m

let private slideSelectedBy (dx : float) (m : Model) : Model =
    match m.selection with
    | ElementSelected i -> setElementX i (elementX (List.item i m.elements) + dx) m
    | TableSelected | NothingSelected -> m

let private slideSelectedTo (x : float) (m : Model) : Model =
    match m.selection with
    | ElementSelected i -> setElementX i x m
    | TableSelected | NothingSelected -> m

let private resetSelectedPosition (m : Model) : Model =
    match m.selection with
    | ElementSelected i -> setElementX i 0.0 m
    | TableSelected | NothingSelected -> m

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
    | AddElement kind -> addElement kind model
    | RemoveSelected -> removeSelected model
    | SlideSelectedBy dx -> slideSelectedBy dx model
    | SlideSelectedTo x -> slideSelectedTo x model
    | ResetSelectedPosition -> resetSelectedPosition model
    | RenderSwap -> { model with render = RendererControls.swap model.render }
    | RenderSetRailsIndex i -> { model with render = RendererControls.withRailsIndex i model.render }
    | RenderSetCircles n -> { model with render = RendererControls.withCircles n model.render }
    | RenderSetRadialsIndex i -> { model with render = RendererControls.withRadialsIndex i model.render }
    | RenderSetRailOpacity v -> { model with render = RendererControls.withRailOpacity v model.render }
    | RenderSetFaceOpacity v -> { model with render = RendererControls.withFaceOpacity v model.render }
    | RenderSetLineOpacity v -> { model with render = RendererControls.withLineOpacity v model.render }
    | SelectBay name -> { model with ribbon = name }
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

// `kindName` / `kindCode` now live in the early `Catalogue` module (task 018) so the shared renderer can
// use them too; re-exported here for the existing references (the other scenes, the tests).
let kindName : CatalogueKind -> string = Catalogue.kindName
let kindCode : CatalogueKind -> string = Catalogue.kindCode

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

/// The add / remove "Lego" palette row — shown ONLY when the scene has a non-empty palette (the Main
/// screen). The static test windows pass an empty palette, so this row is absent and their UI is
/// unchanged. It is the shared `ElementPaletteControls` bar, styled to match the rotation bar.
let private paletteState (model : Model) : ElementPaletteControls.State =
    {
        addItems = model.palette |> List.map (fun k -> { ElementPaletteControls.AddItem.id = kindCode k; label = kindName k })
        canRemove = (match model.selection with ElementSelected _ -> true | _ -> false)
    }

let private paletteHandlers (model : Model) (dispatch : Msg -> unit) : ElementPaletteControls.Handlers =
    {
        add = fun id -> model.palette |> List.tryFind (fun k -> kindCode k = id) |> Option.iter (fun k -> dispatch (AddElement k))
        removeSelected = fun () -> dispatch RemoveSelected
    }

let private addRemoveBar (model : Model) (dispatch : Msg -> unit) : IView list =
    if List.isEmpty model.palette then []
    else [ ElementPaletteControls.view (paletteState model) (paletteHandlers model dispatch) ]

let private controlBar (model : Model) (dispatch : Msg -> unit) : IView =
    StackPanel.create [
        StackPanel.orientation Orientation.Vertical
        StackPanel.spacing 6.0
        StackPanel.margin (Thickness 8.0)
        StackPanel.children
            (addRemoveBar model dispatch
             @ [ RotationControls.view (rotationState model) (rotationHandlers dispatch)
                 TextBlock.create [ TextBlock.name UiIds.readout; TextBlock.text (readoutText model) ]
                 TextBlock.create [
                     TextBlock.foreground (brush (color 100 100 100))
                     TextBlock.text "click TABLE or an element to select it (rotation acts on the selection) · drag = pan · wheel = zoom table · Shift/Ctrl+Shift/Alt+wheel = R1/R2/R3 of the selection · Ctrl+Alt+wheel = zoom element · Ctrl+Alt+Shift+wheel = zoom all · Shift+button = 5°"
                 ] ])
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

// ---------------------------------------------------------------------------
// The Main screen (Spec 0027 task 018): the SAME scene (model / update / table / elements / selection),
// but its controls are the shared "large controls" laid out as a RIBBON of Bays (MS Word style — the top
// strip shows the bay names, the selected bay's controls show below), the elements are drawn through the
// shared `ElementRenderer` (so the Render bay actually changes the look), and the face-opacity knob also
// affects the table. The static test-window `view` above is unchanged.
// ---------------------------------------------------------------------------

/// The table plate (FILL driven by the render face-opacity knob, task 018) plus the SNAPPED beam — the
/// polyline from the source through every downstream element, bending at mirrors (task — the Main window
/// follows the snap approach, so adding a mirror reflects the beam). The elements (incl. source / detector)
/// draw themselves through the renderer, so no separate source / detector markers here.
let private mainTableViews (model : Model) : IView list =
    let corners = TableView.plateCorners3D model.table |> List.map (projectPt model.view) |> List.toArray
    let tableSel = (model.selection = TableSelected)
    let topFace = corners.[0 .. 3] |> Array.map toPoint |> Array.toList
    let face =
        Polygon.create [
            Polygon.points topFace
            Polygon.fill (brushA model.render.faceOpacity (if tableSel then plateSelectedColor else plateColor))
            Polygon.stroke (brush (if tableSel then selectedStroke else edgeColor))
            Polygon.strokeThickness (if tableSel then 3.0 else 1.5)
        ] :> IView
    let edges =
        TableView.plateEdges
        |> List.map (fun (a, b) -> line { sx = corners.[a].sx; sy = corners.[a].sy } { sx = corners.[b].sx; sy = corners.[b].sy } edgeColor (if tableSel then 2.0 else 1.0))
    let beam =
        snd (mainSnap model)
        |> List.map (projectPt model.view)
        |> List.pairwise
        |> List.map (fun (a, b) -> line a b rayColor 2.0)
    (face :: edges) @ beam

/// Whether an element AUTO-ORIENTS to the beam it sits on. A transmissive element faces its incoming beam
/// (so it stays perpendicular to the beam after a reflection); the SOURCE emits along its own aim and a
/// MIRROR's orientation DEFINES its reflection, so those keep their own (dialled) orientation.
let autoOrientsToBeam (kind : CatalogueKind) : bool =
    match kind with
    | LightSource | FlatMirror | CurvedMirror -> false
    | LinearPolarizer | CircularPolarizer | Sample | Lens | Detector -> true

/// The placement each Main element is DRAWN with: its snapped centre, plus — for a transmissive downstream
/// element — auto-orientation to face the beam (the beam's absolute R2/R3 from `RayModel.beamOrientation`
/// plus the element's own dialled R2/R3, exactly as the snap-to-reflected test). The source / mirrors keep
/// their own orientation.
let drawnPlacement (m : Model) (i : int) : ElementPlacement =
    let e = List.item i m.elements
    let node = (fst (mainSnap m)).[i]
    let centred = { e.placement with placementPoint = { x = node.position.x * 1.0<meter>; y = node.position.y * 1.0<meter> } }
    match node.incoming with
    | Some dir when autoOrientsToBeam e.placement.catalogueKind ->
        let beamR2, beamR3 = RayModel.beamOrientation dir
        { centred with r2 = beamR2 + e.placement.r2; r3 = beamR3 + e.placement.r3 }
    | _ -> centred

/// The elements drawn through the chosen renderer at their snapped, auto-oriented placements (zoom is the
/// element's own).
let private mainElementViews (model : Model) : IView list =
    let renderer = ElementRenderer.rendererOf model.render
    let project = projectPt model.view
    let nodes = fst (mainSnap model)
    model.elements
    |> List.mapi (fun i e ->
        // The CENTRE is the full 3-D snapped position (so an element snapped out of the table plane after an
        // R3-tilted mirror is drawn off the plane); `drawnPlacement` carries the orientation.
        renderer.draw project (model.selection = ElementSelected i)
            { placement = drawnPlacement model i; centre = nodes.[i].position; zoom = e.zoom; opticalSign = Catalogue.opticalSign e.placement.catalogueKind })
    |> List.concat

/// The MOVE bay state/handlers: slide the selected element along the beam (disabled unless an element is
/// selected). The clamp (±half the plate) is the host's.
let private moveState (model : Model) : RayPositionControls.State =
    let half = plateHalfLength model
    match model.selection with
    | ElementSelected i -> { position = elementX (List.item i model.elements); minPosition = -half; maxPosition = half; enabled = true }
    | TableSelected | NothingSelected -> { position = 0.0; minPosition = -half; maxPosition = half; enabled = false }

let private moveHandlers (dispatch : Msg -> unit) : RayPositionControls.Handlers =
    {
        moveBy = fun dx -> dispatch (SlideSelectedBy dx)
        setPosition = fun x -> dispatch (SlideSelectedTo x)
        reset = fun () -> dispatch ResetSelectedPosition
    }

let private renderHandlers (dispatch : Msg -> unit) : RendererControls.Handlers =
    {
        swap = fun () -> dispatch RenderSwap
        setRailsIndex = fun i -> dispatch (RenderSetRailsIndex i)
        setCircles = fun n -> dispatch (RenderSetCircles n)
        setRadialsIndex = fun i -> dispatch (RenderSetRadialsIndex i)
        setRailOpacity = fun v -> dispatch (RenderSetRailOpacity v)
        setFaceOpacity = fun v -> dispatch (RenderSetFaceOpacity v)
        setLineOpacity = fun v -> dispatch (RenderSetLineOpacity v)
    }

/// The Main-screen ribbon Bays — every large control, each bound to the current model / dispatch. Adding
/// or removing a Bay here is the ONLY change needed to add / remove a large control from the Main screen.
let mainBays (model : Model) (dispatch : Msg -> unit) : Ribbon.Bay list =
    [ { name = BayNames.rotation; content = RotationControls.view (rotationState model) (rotationHandlers dispatch) }
      { name = BayNames.move; content = RayPositionControls.view (moveState model) (moveHandlers dispatch) }
      { name = BayNames.add; content = ElementPaletteControls.view (paletteState model) (paletteHandlers model dispatch) }
      { name = BayNames.render; content = RendererControls.view model.render (renderHandlers dispatch) } ]

let private mainControlBar (model : Model) (dispatch : Msg -> unit) : IView =
    StackPanel.create [
        StackPanel.orientation Orientation.Vertical
        StackPanel.spacing 4.0
        StackPanel.children [
            Ribbon.view { bays = mainBays model dispatch; selected = model.ribbon } (fun name -> dispatch (SelectBay name))
            TextBlock.create [ TextBlock.name UiIds.readout; TextBlock.margin (Thickness(8.0, 0.0, 0.0, 4.0)); TextBlock.text (readoutText model) ]
        ]
    ] :> IView

let private mainTableCanvas (model : Model) : IView =
    Canvas.create [
        Canvas.name UiIds.canvas
        Canvas.width canvasWidth
        Canvas.height canvasHeight
        Canvas.horizontalAlignment HorizontalAlignment.Left
        Canvas.verticalAlignment VerticalAlignment.Top
        Canvas.children (mainTableViews model @ mainElementViews model)
    ] :> IView

/// The Main screen view: the ribbon of large controls on top, the shared table + renderer-drawn elements
/// below. Same selection / pan / zoom / rotate gestures as the test scene.
let mainView (model : Model) (dispatch : Msg -> unit) : IView =
    let toScreen (e : PointerEventArgs) : ScreenPoint = SceneInput.canvasPoint UiIds.canvas e
    DockPanel.create [
        DockPanel.children [
            Border.create [ Border.dock Dock.Top; Border.child (mainControlBar model dispatch) ]
            Border.create [
                Border.background (brush (color 250 250 250))
                Border.onPointerPressed (fun e -> e.Handled <- true; dispatch (PointerDown (toScreen e)))
                Border.onPointerMoved (fun e -> e.Handled <- true; dispatch (PointerMove (toScreen e)))
                Border.onPointerReleased (fun e -> e.Handled <- true; dispatch (PointerUp (toScreen e)))
                Border.onPointerWheelChanged (fun e ->
                    e.Handled <- true
                    dispatch (Wheel (wheelModifiers e.KeyModifiers, (if e.Delta.Y >= 0.0 then 1 else -1))))
                Border.child (mainTableCanvas model)
            ]
        ]
    ] :> IView
