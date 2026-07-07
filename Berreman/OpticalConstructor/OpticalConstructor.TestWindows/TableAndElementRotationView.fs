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
open Avalonia.Automation
open Avalonia.Controls
open Avalonia.Controls.Shapes
open Avalonia.Input
open Avalonia.Layout
open Avalonia.Media
open Avalonia.FuncUI.Builder
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open Berreman.Constants
open Berreman.Geometry
open Berreman.MaterialProperties
open Berreman.Fields
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
        /// Spec 0027 (024) §1/§5: each table element carries its own stable, serializable identity,
        /// distinct from `placement.valueId` (the binding key to a Library entry). Experiments and
        /// setup steps reference an element by this id, never by an in-memory reference.
        id : Library.ElementId
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

/// Which remove (if any) a workbench bay is awaiting inline confirmation for (spec 0033
/// step 024). The pending case CARRIES the id the Remove click targeted, so a selection change
/// between Remove and Confirm can never delete a different entry — the
/// `RotationControls.ResetConfirm` confirm-gating shape, with a payload. Genuinely generic:
/// the Materials bay instantiates it at `MaterialId`, the Library bay at `SampleId`.
type RemoveConfirm<'id> =
    | NoRemoveConfirm
    | ConfirmingRemove of 'id

/// The side-effecting "open an editor window" seam behind the workbench Add / Edit verbs
/// (spec 0033 step 024). Opening a `Window` is IO (the `openChartWindowHook` precedent), so the
/// verbs reach it through an injected function record (the functional-proxy convention): a
/// headless test substitutes recording launchers — possibly still constructing the real
/// windows — and observes exactly which editor a verb requested. Function-valued fields have no
/// structural equality, so the record compares by reference (the model keeps its
/// Elmish-required equality).
[<ReferenceEquality>]
type EditorLaunchers =
    {
        /// Open the step-023 Material editor: `None` = a new entry (Add), `Some entry` = Edit.
        openMaterialEditor : MaterialLibrary.MaterialProxy -> MaterialLibrary.MaterialEntry option -> unit
        /// Open the step-022 Sample editor: `None` = a new sample (Add / Make-multilayer),
        /// `Some sample` = Edit.
        openSampleEditor : MaterialLibrary.MaterialProxy -> Library.SampleProxy -> Library.Sample option -> unit
    }

    /// The real launchers — the step-022/023 editor windows themselves (compiled before this
    /// file since spec 0033 step 024). Never invoked by a render, only by an Add/Edit dispatch.
    static member defaults : EditorLaunchers =
        {
            openMaterialEditor = fun materials existing -> MaterialEditorWindow(materials, existing).Show()
            openSampleEditor = fun materials samples existing -> SampleEditorWindow(materials, samples, existing).Show()
        }

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
        /// Spec 0027 (024): the injected mock Library IO seam (the functional-proxy convention). Used by
        /// the Main-screen Selector bay to constrain the choosable entries by the selected element's kind
        /// and to resolve the bound entry name. Defaulted to the in-memory mock for the test scenes.
        library : Library.LibraryProxy
        /// Spec 0027 (024) Phase 2: the injected mock Experiments IO seam (the functional-proxy
        /// convention). Holds the editable / listable experiment-set templates; the bay itself lets the
        /// user pick which present element is the swept one. Defaulted to the in-memory mock.
        experiments : Experiments.ExperimentProxy
        /// Spec 0027 (028): the editable EXPERIMENT COLLECTION and its multi-step draft — choose the element
        /// to vary, the (element-constrained) variable, the T/R/both capture, the range; Add / edit / remove.
        /// Replaces the old single swept-element + experiment-kind + λ-range fields. An experiment references
        /// its element by the serializable id, so the collection survives save-load.
        experimentCollection : Experiments.ExperimentCollection
        /// Spec 0027 (026): the Library entry the user has SELECTED but not yet confirmed (the pending
        /// bind). Clicking a Library entry sets this and shows its full description; Confirm commits it to
        /// the selected element's `valueId`, Cancel clears it.
        pendingEntry : string option
        /// Spec 0033 (024): the injected materials WRITE seam (STORE_XDUO_0001) behind the Materials
        /// workbench bay. The bay projection re-queries it on EVERY render, so a verb's write shows in
        /// the same render pass.
        materials : MaterialLibrary.MaterialProxy
        /// Spec 0033 (024): the injected samples WRITE seam (STORE_XDUO_0002) behind the Library
        /// (samples workbench) bay.
        samples : Library.SampleProxy
        /// The Materials bay's live search query — the `MaterialQuery` DATA (text + category +
        /// dispersion facets) the search box / facet selectors drive through `searchMaterials`.
        materialQuery : MaterialLibrary.MaterialQuery
        /// The Materials bay's selected row (the Edit / Remove / View verbs' target).
        selectedMaterial : MaterialLibrary.MaterialId option
        /// Which material remove (if any) awaits its inline confirmation.
        materialRemoveConfirm : RemoveConfirm<MaterialLibrary.MaterialId>
        /// The last materials-store refusal, surfaced as the bay's inline message —
        /// `MaterialStillReferenced` NAMES the referencing samples; never a cascade, never a dialog.
        materialsError : MaterialLibrary.MaterialError option
        /// The material whose read-only metadata + n/k chart the View verb opened (a toggle).
        viewedMaterial : MaterialLibrary.MaterialId option
        /// The Library bay's live samples search query (text + substrate facet).
        sampleQuery : Library.SampleQuery
        /// The Library bay's selected row (the Edit / Remove / View verbs' target).
        selectedSample : Library.SampleId option
        /// Which sample remove (if any) awaits its inline confirmation.
        sampleRemoveConfirm : RemoveConfirm<Library.SampleId>
        /// The last samples-store refusal, surfaced as the bay's inline message.
        samplesError : Library.SampleError option
        /// The sample whose read-only metadata + band view the View verb opened (a toggle).
        viewedSample : Library.SampleId option
        /// Spec 0033 (024): the editor-window launcher seam behind the workbench Add / Edit verbs.
        launchers : EditorLaunchers
    }

/// The Main-screen ribbon Bay names (the "large controls" the ribbon shows MS-Word-style).
module BayNames =
    let rotation = "Rotation"
    let move = "Move"
    let add = "Add"
    let render = "Render"
    /// Spec 0027 (024): the Selector bay (labelled "Library" until spec 0033 step 014) — pick the
    /// choosable spec for the selected element. The kind-constrained, confirm-gated binding through
    /// `LibraryControls` and the read-only `LibraryProxy` is unchanged; only the label moved, freeing
    /// the word Library for the samples workbench.
    let selector = "Selector"
    /// Spec 0027 (024) Phase 2: the Experiments bay — pick which element's R1 sweeps a full circle.
    let experiments = "Experiments"
    /// Spec 0027 (026): the Details bay — the selected element's bound Library entry (what it is, its full
    /// description, and a layer-stack band view for a multilayer sample).
    let details = "Details"
    /// Spec 0033 (024): the Materials bay — the materials WORKBENCH (the step-015 `MaterialsControls`
    /// list surface over the step-006 `MaterialProxy` write seam): search box, category / dispersion
    /// facets, and the Add / Edit / Remove / View verbs.
    let materials = "Materials"
    /// Spec 0033 (024): the Library bay — the SAMPLES workbench (the step-016 `SampleLibraryControls`
    /// surface over the step-005 `SampleProxy` write seam). The label freed by the step-014 Selector
    /// rename: "Library" now names the samples collection, while element↔entry binding stays in the
    /// Selector bay.
    let library = "Library"
    // spec 0033 gap G2 (deferred): the operator wants Materials & Library LAST, but reordering the
    // ribbon panes breaks the Library bay's sample-row layout in the headless harness (and thus the
    // real ribbon) — see 008-close-the-gaps-implementation-log.md §4. The original order stands until
    // the Ribbon pane-hosting is made order-independent. Details stays LAST (the 0027/026 pin).
    let all = [ rotation; move; add; render; selector; materials; library; experiments; details ]

let defaultElementZoom : float = 5.0

/// Mint a fresh, serializable element id (spec §1/§5). Added elements get a GUID; the seeded
/// source/detector get deterministic ids (see `initMain`) so wiring/experiment tests can name them.
let private freshId () : Library.ElementId = Library.elementId (System.Guid.NewGuid().ToString())

let private mkElement (x : float) (kind : CatalogueKind) : TestElement =
    { id = freshId (); placement = ElementPlacement.create kind { x = x * 1.0<meter>; y = 0.0<meter> }; zoom = defaultElementZoom }

/// The default in-memory material / sample stores for the parameterless test scenes
/// (`init` / `initMain`) — the SAME composition the App performs (spec 0033 steps 005/006): the
/// samples store first, then the materials store whose remove-block consults the LIVE samples
/// through `samplesReferencing`. A nested module so the `Library` open (required to see the
/// optional `MaterialProxy.createInMemory` type extension) stays scoped away from this file's
/// Avalonia names.
module private DefaultStores =
    open OpticalConstructor.Domain.Library

    let create () : MaterialLibrary.MaterialProxy * SampleProxy =
        let samples = SampleProxy.createInMemory ()
        let materials = MaterialLibrary.MaterialProxy.createInMemory (samplesReferencing samples)
        materials, samples

/// The shared scene seed: the standard table, the straight top-down view, the given elements, the
/// table selected first, an add/remove `palette`, and the injected proxies (the read-only Library
/// and Experiments seams, plus — spec 0033 step 024 — the material / sample WRITE seams behind the
/// workbench bays). `init` (the test window) passes an empty palette; `initMain` (the Main screen)
/// passes a non-empty one — that is the ONLY behavioural difference between them.
let initWith
    (library : Library.LibraryProxy)
    (experiments : Experiments.ExperimentProxy)
    (materials : MaterialLibrary.MaterialProxy)
    (samples : Library.SampleProxy)
    (elements : TestElement list)
    (palette : CatalogueKind list) : Model =
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
        library = library
        experiments = experiments
        experimentCollection = Experiments.ExperimentCollection.empty
        pendingEntry = None
        materials = materials
        samples = samples
        materialQuery = MaterialLibrary.MaterialQuery.empty
        selectedMaterial = None
        materialRemoveConfirm = NoRemoveConfirm
        materialsError = None
        viewedMaterial = None
        sampleQuery = Library.SampleQuery.empty
        selectedSample = None
        sampleRemoveConfirm = NoRemoveConfirm
        samplesError = None
        viewedSample = None
        launchers = EditorLaunchers.defaults
    }

/// The STATIC test scene (Spec 0027, task 006 #3): a live table plus three fixed optical elements on
/// the central ray, no add/remove palette. Behaviour is unchanged from before — the palette is empty.
/// The proxies default to the in-memory mocks/stores (the test scene never shows the ribbon bays).
let init () : Model =
    let materials, samples = DefaultStores.create ()
    initWith (Library.createInMemory ()) (Experiments.createInMemory ()) materials samples [ mkElement -0.5 LinearPolarizer; mkElement 0.0 Sample; mkElement 0.5 FlatMirror ] []

/// The DYNAMIC Main scene: the same table/view/selection/rotation logic, seeded with a light source and
/// a detector at the ends of the beam, plus the catalogue palette the user can add elements from (the
/// "Lego constructor"). The proxies — incl. the step-024 material / sample write seams — are injected
/// at the composition root. This is the Main screen — identical scene logic, elements added/removed at
/// runtime.
let initMainWith
    (library : Library.LibraryProxy)
    (experiments : Experiments.ExperimentProxy)
    (materials : MaterialLibrary.MaterialProxy)
    (samples : Library.SampleProxy) : Model =
    // The light source snaps to the table's LEFT edge and the detector to the RIGHT edge — i.e. the
    // central-ray endpoints, which sit exactly on the plate edges (`defaultSourceDetectorDistance` = the
    // table length). Added elements land between them on the beam. The source/detector get DETERMINISTIC
    // ids so wiring/experiment tests can name them.
    { initWith
        library
        experiments
        materials
        samples
        [ { id = Library.elementId "src"; placement = ElementPlacement.create LightSource RayModel.defaultSourcePoint; zoom = defaultElementZoom }
          { id = Library.elementId "det"; placement = ElementPlacement.create Detector RayModel.defaultDetectorPoint; zoom = defaultElementZoom } ]
        [ LinearPolarizer; CircularPolarizer; Sample; Lens; FlatMirror; CurvedMirror; Detector ]
        with snapChain = true }

/// The Main scene with the default in-memory mock proxies / stores (the test default; the composition
/// root injects its own proxies via `initMainWith`).
let initMain () : Model =
    let materials, samples = DefaultStores.create ()
    initMainWith (Library.createInMemory ()) (Experiments.createInMemory ()) materials samples

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
    /// Main-screen LIBRARY bay (task 024): bind this Library entry id to the selected element's
    /// `valueId` (inert when the table or nothing is selected).
    | BindValueId of string
    /// Spec 0027 (026): the confirm-gated Library bind. `RequestBindValueId` SELECTS an entry as pending
    /// (shows its full description, no bind yet); `ConfirmBindValueId` COMMITS the pending entry to the
    /// selected element's `valueId`; `CancelBindValueId` clears the pending choice.
    | RequestBindValueId of string
    | ConfirmBindValueId
    | CancelBindValueId
    /// Main-screen EXPERIMENTS bay (spec 0027 / 028): the multi-step experiment editor + collection.
    /// Choose the element to vary (by id), the varied quantity, the T/R/both capture, and the range;
    /// commit (Add / Update), start a New draft, or Edit / Remove a collected experiment (by id string).
    | ExpChooseElement of string
    | ExpChooseVariable of ExperimentControls.VariableChoice
    | ExpChooseMeasurement of ExperimentControls.MeasurementChoice
    | ExpSetRangeMin of float
    | ExpSetRangeMax of float
    | ExpSetRangePoints of int
    | ExpCommit
    | ExpNew
    | ExpEdit of string
    | ExpRemove of string
    /// Spec 0027 (026): open the pop-out interactive chart window for the current DRAFT (double-click on the
    /// inline chart, or the "Open chart" button).
    | OpenExperimentChartWindow
    /// Spec 0027 (030 follow-up): open the chart window for a COLLECTED experiment by id (the per-row "View"
    /// button, and a double-click on the experiment row).
    | ViewExperiment of string
    /// Spec 0033 (024) — the MATERIALS workbench bay. The search box / facet selectors drive the
    /// `searchMaterials` query; a row click selects the verbs' target; Add / Edit open the step-023
    /// editor through the launcher seam; Remove is confirm-gated inline; View toggles the read-only
    /// metadata + n/k-chart panel. Facet picks carry the DOMAIN facet — the option-code mapping
    /// happens in the bay handlers at the control boundary.
    | MatSetSearchText of string
    | MatSelectCategory of MaterialLibrary.MaterialCategory option
    | MatSelectDispersion of MaterialLibrary.DispersionFilter
    | MatSelectRow of MaterialLibrary.MaterialId
    | MatAdd
    | MatEdit
    | MatView
    | MatRequestRemove
    | MatConfirmRemove
    | MatCancelRemove
    /// Spec 0033 (024) — the LIBRARY (samples workbench) bay: the same verb vocabulary over
    /// `searchSamples` / the step-022 editor; Make-multilayer is the second creation entry point
    /// (a new sample — the stack editor's fold vocabulary IS the multilayer flow).
    | SmpSetSearchText of string
    | SmpSelectSubstrate of Library.SubstrateKind option
    | SmpSelectRow of Library.SampleId
    | SmpAdd
    | SmpMakeMultilayer
    | SmpEdit
    | SmpView
    | SmpRequestRemove
    | SmpConfirmRemove
    | SmpCancelRemove

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
    let e = { id = freshId (); placement = ElementPlacement.create kind { x = x * 1.0<meter>; y = 0.0<meter> }; zoom = defaultElementZoom }
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

/// Spec 0027 (026) Part 3: the side-effecting "open the pop-out chart window" action, injected as a forward
/// reference so `update` can invoke it before `experimentResult` (and the `ChartWindow` host) are defined
/// further down the module. The real implementation is assigned right after `experimentResult`; under a
/// headless test host (which never dispatches `OpenExperimentChartWindow`) the default no-op is harmless.
let mutable private openChartWindowHook : Model -> unit = fun _ -> ()

/// Spec 0027 (030 follow-up): the side-effecting "open the chart window for a COLLECTED experiment (by id)"
/// action — the per-row "View" button and a row double-click both invoke it. Same forward-reference seam.
let mutable private viewExperimentHook : Model -> string -> unit = fun _ _ -> ()

/// Spec 0027 (028): the Controls-layer ⇄ domain mirror maps for the experiment variable / measurement DUs
/// (the bay is domain-free, so it mirrors the domain cases and the host maps them back).
let private ofVariableChoice (v : ExperimentControls.VariableChoice) : Experiments.VariableParameter =
    match v with
    | ExperimentControls.VaryWaveLength -> Experiments.VaryWaveLength
    | ExperimentControls.VaryR1 -> Experiments.VaryR1
    | ExperimentControls.VaryR2 -> Experiments.VaryR2

let private toVariableChoice (v : Experiments.VariableParameter) : ExperimentControls.VariableChoice =
    match v with
    | Experiments.VaryWaveLength -> ExperimentControls.VaryWaveLength
    | Experiments.VaryR1 -> ExperimentControls.VaryR1
    | Experiments.VaryR2 -> ExperimentControls.VaryR2

let private ofMeasurementChoice (m : ExperimentControls.MeasurementChoice) : Experiments.MeasurementMode =
    match m with
    | ExperimentControls.CaptureT -> Experiments.CaptureTransmitted
    | ExperimentControls.CaptureR -> Experiments.CaptureReflected
    | ExperimentControls.CaptureBoth -> Experiments.CaptureBoth

let private toMeasurementChoice (m : Experiments.MeasurementMode) : ExperimentControls.MeasurementChoice =
    match m with
    | Experiments.CaptureTransmitted -> ExperimentControls.CaptureT
    | Experiments.CaptureReflected -> ExperimentControls.CaptureR
    | Experiments.CaptureBoth -> ExperimentControls.CaptureBoth

/// Spec 0027 (028): the human label of a present element ("<kind> #<n>", 1-based, in scene order), captured
/// onto an experiment so its collection row survives the element's removal.
let private experimentElementLabel (model : Model) (id : Library.ElementId) : string =
    model.elements
    |> List.mapi (fun i e -> i, e)
    |> List.tryFind (fun (_, e) -> e.id = id)
    |> Option.map (fun (i, e) -> $"%s{(Catalogue.kindName e.placement.catalogueKind)} #%d{(i + 1)}")
    |> Option.defaultValue "(element)"

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
    | BindValueId entryId ->
        match model.selection with
        | ElementSelected i -> mapElement i (fun e -> { e with placement = { e.placement with valueId = Some entryId } }) { model with pendingEntry = None }
        | TableSelected | NothingSelected -> model
    | RequestBindValueId entryId ->
        // Spec 0027 (026): select the entry as PENDING (its full description is shown) without binding yet —
        // inert unless an element is selected (the Selector bay is only enabled for an element).
        match model.selection with
        | ElementSelected _ -> { model with pendingEntry = Some entryId }
        | TableSelected | NothingSelected -> model
    | ConfirmBindValueId ->
        // Spec 0027 (026): commit the pending entry to the selected element's valueId, then clear pending.
        match model.pendingEntry, model.selection with
        | Some entryId, ElementSelected i ->
            mapElement i (fun e -> { e with placement = { e.placement with valueId = Some entryId } }) { model with pendingEntry = None }
        | _ -> { model with pendingEntry = None }
    | CancelBindValueId -> { model with pendingEntry = None }
    | ExpChooseElement idStr ->
        // Choosing the element to vary sets the draft element AND, from the element's kind, the allowed
        // variables (data via `variablesFor`, never hard-coded here) and the default capture (from its
        // emission — a mirror ⇒ R, everything else both).
        match model.elements |> List.tryFind (fun e -> e.id.value = idStr) with
        | Some e ->
            let allowed = Experiments.variablesFor e.placement.catalogueKind
            let defaultMeasurement = Experiments.MeasurementMode.ofEmission e.placement.emission
            let label = experimentElementLabel model e.id
            { model with experimentCollection = Experiments.chooseElement e.id label allowed defaultMeasurement model.experimentCollection }
        | None -> model
    | ExpChooseVariable v -> { model with experimentCollection = Experiments.chooseVariable (ofVariableChoice v) model.experimentCollection }
    | ExpChooseMeasurement m -> { model with experimentCollection = Experiments.chooseMeasurement (ofMeasurementChoice m) model.experimentCollection }
    | ExpSetRangeMin v -> { model with experimentCollection = Experiments.setRangeMin v model.experimentCollection }
    | ExpSetRangeMax v -> { model with experimentCollection = Experiments.setRangeMax v model.experimentCollection }
    | ExpSetRangePoints n -> { model with experimentCollection = Experiments.setRangePoints n model.experimentCollection }
    | ExpCommit -> { model with experimentCollection = Experiments.commit model.experimentCollection }
    | ExpNew -> { model with experimentCollection = Experiments.newDraft model.experimentCollection }
    | ExpEdit idStr ->
        match System.Int32.TryParse idStr with
        | true, i -> { model with experimentCollection = Experiments.edit (Experiments.ExperimentId i) model.experimentCollection }
        | _ -> model
    | ExpRemove idStr ->
        match System.Int32.TryParse idStr with
        | true, i -> { model with experimentCollection = Experiments.remove (Experiments.ExperimentId i) model.experimentCollection }
        | _ -> model
    | OpenExperimentChartWindow ->
        // Spec 0027 (026) Part 3: a double-click on the inline chart opens the pop-out interactive ScottPlot
        // window for the current experiment chart. Opening a `Window` is a side effect, matching the existing
        // imperative-window pattern (`MainConstructorWindow().Show()`); it is guarded behind a non-empty chart
        // and is never reached under the headless `ui-smoke` gate (which renders frames but never double-clicks).
        openChartWindowHook model
        model
    | ViewExperiment idStr ->
        // Spec 0027 (030 follow-up): open the chart window for the collected experiment `idStr` (the "View"
        // button / a row double-click). Same side-effecting seam as OpenExperimentChartWindow.
        viewExperimentHook model idStr
        model
    // -- Spec 0033 (024): the Materials workbench bay. A query / selection change also disarms a
    // -- pending remove confirmation and clears the inline message — a stale confirm or refusal
    // -- never outlives the state it referred to. The bay projection re-queries the proxy on every
    // -- render, so each arm's write (if any) shows in the same render pass.
    | MatSetSearchText text ->
        { model with materialQuery = { model.materialQuery with text = text }; materialRemoveConfirm = NoRemoveConfirm; materialsError = None }
    | MatSelectCategory category ->
        { model with materialQuery = { model.materialQuery with category = category }; materialRemoveConfirm = NoRemoveConfirm; materialsError = None }
    | MatSelectDispersion dispersion ->
        { model with materialQuery = { model.materialQuery with dispersion = dispersion }; materialRemoveConfirm = NoRemoveConfirm; materialsError = None }
    | MatSelectRow id ->
        { model with selectedMaterial = Some id; materialRemoveConfirm = NoRemoveConfirm; materialsError = None }
    | MatAdd ->
        model.launchers.openMaterialEditor model.materials None
        { model with materialRemoveConfirm = NoRemoveConfirm; materialsError = None }
    | MatEdit ->
        // Edit opens the step-023 editor on the selected entry, resolved through the proxy at
        // dispatch time. (The bay REMOVES the Edit verb for a view-only selection; an entry with
        // `complexity = None` would open view-only anyway — the editor's own rule.)
        (match model.selectedMaterial with
         | Some id ->
             match model.materials.tryGetMaterial id with
             | Ok (Some entry) -> model.launchers.openMaterialEditor model.materials (Some entry)
             | Ok None | Error _ -> ()
         | None -> ())
        { model with materialRemoveConfirm = NoRemoveConfirm; materialsError = None }
    | MatView ->
        // Toggle the read-only panel: viewing the already-viewed selection closes it.
        let viewed =
            match model.selectedMaterial, model.viewedMaterial with
            | Some selected, Some shown when selected = shown -> None
            | selected, _ -> selected
        { model with viewedMaterial = viewed; materialRemoveConfirm = NoRemoveConfirm; materialsError = None }
    | MatRequestRemove ->
        match model.selectedMaterial with
        | Some id -> { model with materialRemoveConfirm = ConfirmingRemove id; materialsError = None }
        | None -> model
    | MatConfirmRemove ->
        match model.materialRemoveConfirm with
        | ConfirmingRemove id ->
            match model.materials.removeMaterial id with
            | Ok () ->
                { model with
                    materialRemoveConfirm = NoRemoveConfirm
                    materialsError = None
                    selectedMaterial = (if model.selectedMaterial = Some id then None else model.selectedMaterial)
                    viewedMaterial = (if model.viewedMaterial = Some id then None else model.viewedMaterial) }
            | Error err ->
                // The store refused (`MaterialStillReferenced` names the referencing samples) —
                // surface the reason inline and leave the store, the selection and the list untouched.
                { model with materialRemoveConfirm = NoRemoveConfirm; materialsError = Some err }
        | NoRemoveConfirm -> model
    | MatCancelRemove -> { model with materialRemoveConfirm = NoRemoveConfirm }
    // -- Spec 0033 (024): the Library (samples workbench) bay — the same discipline.
    | SmpSetSearchText text ->
        { model with sampleQuery = { model.sampleQuery with text = text }; sampleRemoveConfirm = NoRemoveConfirm; samplesError = None }
    | SmpSelectSubstrate substrate ->
        { model with sampleQuery = { model.sampleQuery with substrate = substrate }; sampleRemoveConfirm = NoRemoveConfirm; samplesError = None }
    | SmpSelectRow id ->
        { model with selectedSample = Some id; sampleRemoveConfirm = NoRemoveConfirm; samplesError = None }
    | SmpAdd | SmpMakeMultilayer ->
        // Both creation entry points open the step-022 editor on a NEW sample: the make-multilayer
        // flow is the stack editor's fold vocabulary (MakeRepeatBlock + the repeat-count steppers).
        model.launchers.openSampleEditor model.materials model.samples None
        { model with sampleRemoveConfirm = NoRemoveConfirm; samplesError = None }
    | SmpEdit ->
        (match model.selectedSample with
         | Some id ->
             match model.samples.tryGetSample id with
             | Ok (Some sample) -> model.launchers.openSampleEditor model.materials model.samples (Some sample)
             | Ok None | Error _ -> ()
         | None -> ())
        { model with sampleRemoveConfirm = NoRemoveConfirm; samplesError = None }
    | SmpView ->
        let viewed =
            match model.selectedSample, model.viewedSample with
            | Some selected, Some shown when selected = shown -> None
            | selected, _ -> selected
        { model with viewedSample = viewed; sampleRemoveConfirm = NoRemoveConfirm; samplesError = None }
    | SmpRequestRemove ->
        match model.selectedSample with
        | Some id -> { model with sampleRemoveConfirm = ConfirmingRemove id; samplesError = None }
        | None -> model
    | SmpConfirmRemove ->
        match model.sampleRemoveConfirm with
        | ConfirmingRemove id ->
            match model.samples.removeSample id with
            | Ok () ->
                { model with
                    sampleRemoveConfirm = NoRemoveConfirm
                    samplesError = None
                    selectedSample = (if model.selectedSample = Some id then None else model.selectedSample)
                    viewedSample = (if model.viewedSample = Some id then None else model.viewedSample) }
            | Error err ->
                { model with sampleRemoveConfirm = NoRemoveConfirm; samplesError = Some err }
        | NoRemoveConfirm -> model
    | SmpCancelRemove -> { model with sampleRemoveConfirm = NoRemoveConfirm }
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
        // Spec 0027 (026): a changed selection clears any pending (unconfirmed) Library bind, so a stale
        // pending choice never carries across elements.
        let pending = if selection = model.selection then model.pendingEntry else None
        { model with selection = selection; drag = NotPressed; pendingEntry = pending }
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
        $"Selected: TABLE   R1 %.0f{model.view.r1.degrees}°   R2 %.0f{model.view.r2.degrees}°   R3 %.0f{model.view.r3.degrees}°   zoom %.2f{model.view.zoom}×"
    | ElementSelected i ->
        let e = List.item i model.elements
        let p = e.placement
        // Spec 0027 (024): show the bound Library entry (resolved through the Library proxy), or "unbound".
        let boundName =
            match p.valueId with
            | Some id ->
                match model.library.tryGetEntry id with
                | Ok (Some entry) -> entry.displayName
                | Ok None | Error _ -> "unbound"
            | None -> "unbound"
        $"""Selected: Element %d{i + 1} (%s{kindName p.catalogueKind})   R1 %.0f{p.r1.degrees}°   R2 %.0f{p.r2.degrees}°   R3 %.0f{p.r3.degrees}° (%s{(if p.r3Locked then "R3 locked" else "R3 free")})   zoom %.1f{e.zoom}×   bound: %s{boundName}"""
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

// ---------------------------------------------------------------------------
// Main-screen LIBRARY bay (task 024): pick the choosable spec for the selected element. The Library is
// constrained by the selected element's catalogue kind, and selecting an entry binds the element's
// `valueId`. The bay is domain-free, so the host flattens the chosen grouping tree to `Row`s here.
// ---------------------------------------------------------------------------

/// The Library entry ids valid for `kind` (the kind-constrained set; `[]` on a proxy error).
let private allowedEntryIds (model : Model) (kind : CatalogueKind) : Set<string> =
    match model.library.entriesForKind kind with
    | Ok entries -> entries |> List.map (fun e -> e.entryId) |> Set.ofList
    | Error _ -> Set.empty

/// Flatten one grouping tree to `LibraryControls.Row`s, KIND-CONSTRAINED: emit a leaf row only when its
/// entry id is in `allowed`, and a group header only if it has a surviving descendant leaf (§2a — keep
/// the tree shape but show only entries valid for the selected element). `boundId` highlights the bound
/// leaf.
let rec private flattenNode (allowed : Set<string>) (boundId : string option) (depth : int) (node : Library.LibraryTreeNode) : LibraryControls.Row list =
    match node with
    | Library.Leaf (label, entryId) ->
        if Set.contains entryId allowed then
            [ { label = label.value; depth = depth; entryId = entryId; isBound = (boundId = Some entryId) } ]
        else []
    | Library.Group (label, children) ->
        let childRows = children |> List.collect (flattenNode allowed boundId (depth + 1))
        if List.isEmpty childRows then []
        else { label = label.value; depth = depth; entryId = ""; isBound = false } :: childRows

/// The Selector bay state for the current selection: an `ElementSelected` shows the kind-constrained
/// tree rows, the kind label, and the bound-entry readout; anything else disables the bay.
let private libraryState (model : Model) : LibraryControls.State =
    match model.selection with
    | ElementSelected i ->
        let e = List.item i model.elements
        let kind = e.placement.catalogueKind
        let allowed = allowedEntryIds model kind
        let boundId = e.placement.valueId
        let rows =
            match model.library.libraryTrees () with
            | Ok (tree :: _) -> flattenNode allowed boundId 0 tree.root
            | Ok [] | Error _ -> []
        let boundName =
            match boundId with
            | Some id ->
                match model.library.tryGetEntry id with
                | Ok (Some entry) -> Some entry.displayName
                | Ok None | Error _ -> None
            | None -> None
        // Spec 0027 (026): the pending (selected-not-confirmed) entry's name + full description, resolved
        // through the proxy (so a stale pending id yields no panel).
        let pendingName, pendingDescription =
            match model.pendingEntry with
            | Some id ->
                match model.library.tryGetEntry id with
                | Ok (Some entry) -> Some entry.displayName, entry.fullDescription
                | Ok None | Error _ -> None, ""
            | None -> None, ""
        {
            rows = rows
            kindLabel = kindName kind
            boundName = boundName
            enabled = true
            pendingEntryId = model.pendingEntry
            pendingName = pendingName
            pendingDescription = pendingDescription
        }
    | TableSelected | NothingSelected -> LibraryControls.empty

let private libraryHandlers (dispatch : Msg -> unit) : LibraryControls.Handlers =
    {
        selectEntry = fun entryId -> dispatch (RequestBindValueId entryId)
        confirmEntry = fun () -> dispatch ConfirmBindValueId
        cancelEntry = fun () -> dispatch CancelBindValueId
    }

// ---------------------------------------------------------------------------
// Main-screen EXPERIMENTS bay (spec 0027 / 028): the multi-step, editable experiment builder + collection.
// The SETUP is the live scene; the user picks the element to vary (the element's kind — via
// `Experiments.variablesFor` DATA — determines the allowed variables), the T/R/both capture, and the
// range, then Adds to a persistent collection (edit / remove supported). The bay is domain-free, so the
// host flattens the scene into candidates, the draft into the editor state, and the collection into rows.
// ---------------------------------------------------------------------------

/// One present element as a candidate-to-vary — its serializable id plus a human-readable label
/// ("<kind> #<n>", 1-based, in scene order).
let private sweepCandidates (model : Model) : ExperimentControls.SweepCandidate list =
    model.elements
    |> List.mapi (fun i e -> { ExperimentControls.SweepCandidate.elementId = e.id.value; label = $"%s{(kindName e.placement.catalogueKind)} #%d{(i + 1)}" })

/// The current draft experiment's readout (spec 028), or "" when the draft is incomplete / its element is
/// no longer present.
let private experimentReadout (model : Model) : string =
    let draft = model.experimentCollection.draft
    match draft.elementId, draft.variable with
    | Some id, Some v when model.elements |> List.exists (fun e -> e.id = id) ->
        $"Experiment: %s{draft.elementLabel} — vary %s{v.label} over %g{draft.range.min}…%g{draft.range.max} %s{v.unitLabel} (%d{draft.range.points} pts), capture %s{draft.measurement.label}"
    | _ -> ""

// ---------------------------------------------------------------------------
// Spec 0027 (024) Phase 3/4: run ONE experiment end-to-end from the LIVE scene. The host resolves the
// physical run from the present elements (source → λ, input polarizer → SV_in, sample → MM_sample, the
// chosen analyzer → its polarizer kind, detector → Intensity vs Ellipsometer), then drives the pure
// `Propagation` pipeline. Absent elements are SKIPPED (nothing synthesized; spec R1): no source → 600 nm,
// no input polarizer → unpolarized light, no sample → identity MM. The result is an (angleDeg, intensity)
// curve for an intensity detector, or a Ψ/Δ readout for an ellipsometer.
// ---------------------------------------------------------------------------

/// Resolve an element's bound Library entry (through the Library proxy), if it has a `valueId` that binds.
let private boundEntry (model : Model) (e : TestElement) : Library.LibraryEntry option =
    match e.placement.valueId with
    | Some id ->
        match model.library.tryGetEntry id with
        | Ok (Some entry) -> Some entry
        | Ok None | Error _ -> None
    | None -> None

/// The source wavelength from the first light-source element bound to a source preset (else 600 nm — an
/// absent / unbound source is not synthesized, just defaulted; spec R1).
let private runWaveLength (model : Model) : WaveLength =
    model.elements
    |> List.tryPick (fun e ->
        match boundEntry model e with
        | Some (Library.SourceItem s) -> Some s.waveLength
        | _ -> None)
    |> Option.defaultValue (WaveLength.nm 600.0<nm>)

/// The input Stokes vector from the FIRST polarizer element bound to an ideal-polarizer preset (its R1 is
/// the polarizer's orientation). No input polarizer → unpolarized natural light (spec R1).
let private runInputStokes (model : Model) : StokesVector =
    model.elements
    |> List.tryPick (fun e ->
        match boundEntry model e with
        | Some (Library.PolarizerItem p) -> Some (Propagation.inputStokes p.kind e.placement.r1)
        | _ -> None)
    |> Option.defaultValue Propagation.unpolarizedStokes

/// The analyzer's polarizer kind for the rotate-R1 experiment: the VARIED element if it is bound to a
/// polarizer preset, else the first polarizer bound in the scene, else an ideal linear analyzer (so the
/// rotate is always well-defined).
let private runAnalyzerKind (model : Model) (varied : Library.ElementId option) : Library.PolarizerKind =
    let variedKind =
        match varied with
        | Some chosen ->
            model.elements
            |> List.tryFind (fun e -> e.id = chosen)
            |> Option.bind (fun e ->
                match boundEntry model e with
                | Some (Library.PolarizerItem p) -> Some p.kind
                | _ -> None)
        | None -> None
    match variedKind with
    | Some k -> k
    | None ->
        model.elements
        |> List.choose (fun e ->
            match boundEntry model e with
            | Some (Library.PolarizerItem p) -> Some p.kind
            | _ -> None)
        |> List.tryLast
        |> Option.defaultValue Library.IdealLinear

/// Whether the scene's detector (the first detector element bound to a detector preset) is an ellipsometer.
let private runDetectorKind (model : Model) : Library.DetectorKind =
    model.elements
    |> List.tryPick (fun e ->
        match boundEntry model e with
        | Some (Library.DetectorItem d) -> Some d.kind
        | _ -> None)
    |> Option.defaultValue Library.Intensity

/// The FIRST sample element bound to a sample preset (the sample the R2 / λ sweeps re-solve), if any. No
/// sample present ⇒ `None` (an R2 / λ sweep has nothing to re-solve, so the chart is empty; spec R1).
let private runSampleOpt (model : Model) : Library.Sample option =
    model.elements
    |> List.tryPick (fun e ->
        match boundEntry model e with
        | Some (Library.SampleItem s) -> Some s
        | _ -> None)

/// The material library the host resolves sample structures against (the composition root's single
/// library instance, spec 0033 step 001).
let private materialLibrary : MaterialLibrary.MaterialLibrary = MaterialLibrary.standard

/// The bound sample resolved against the material library — resolved ONCE per run (spec 0033 step 001).
/// `None` = no sample bound; `Some (Error _)` = the sample references an unknown material id, which the
/// chart surfaces as its message (a typed error, never a fallback).
let private runResolvedSampleOpt (model : Model) : Result<Propagation.ResolvedSample, MaterialLibrary.MaterialError> option =
    runSampleOpt model |> Option.map (Propagation.resolveSampleMaterials materialLibrary)

/// The human-readable message a failed sample-material resolution surfaces on the chart. Every error
/// case carries a diagnostic `reason` (spec 0033 steps 002/003); only `UnknownMaterialId` can actually
/// arise from resolution — the write-seam cases (step 003) render a generic library message.
let private materialErrorText (err : MaterialLibrary.MaterialError) : string =
    match err with
    | MaterialLibrary.UnknownMaterialId reason ->
        $"Cannot run: the sample references an unknown material (%s{reason})."
    | MaterialLibrary.DuplicateMaterialId reason
    | MaterialLibrary.MaterialStillReferenced reason
    | MaterialLibrary.InvalidMaterial reason ->
        $"Cannot run: material library error (%s{reason})."

/// The analyzer (its polarizer kind + orientation R1) for the sweeps: the FIRST polarizer element bound to
/// a polarizer preset, with its live R1 as the orientation. `None` when no analyzer is present (the sweep
/// then reads the raw sample output, spec R1).
let private runAnalyzerOpt (model : Model) : (Library.PolarizerKind * Angle) option =
    model.elements
    |> List.tryPick (fun e ->
        match boundEntry model e with
        | Some (Library.PolarizerItem p) -> Some (p.kind, e.placement.r1)
        | _ -> None)

/// A `ChartSeries` from named (x, y) points.
let private series (name : string) (points : (float * float) list) : ExperimentChart.ChartSeries =
    { name = name; points = points }

/// The Propagation branches an experiment captures, each with a short tag for its series name (spec 028).
let private branchesFor (m : Experiments.MeasurementMode) : (Propagation.Branch * string) list =
    match m with
    | Experiments.CaptureTransmitted -> [ Propagation.BranchTransmitted, "T" ]
    | Experiments.CaptureReflected -> [ Propagation.BranchReflected, "R" ]
    | Experiments.CaptureBoth -> [ Propagation.BranchTransmitted, "T"; Propagation.BranchReflected, "R" ]

/// A series display name: the base name, suffixed with the branch tag only when capturing BOTH branches.
let private seriesName (m : Experiments.MeasurementMode) (branchTag : string) (baseName : string) : string =
    match m with
    | Experiments.CaptureBoth -> $"%s{baseName} (%s{branchTag})"
    | _ -> baseName

/// The sample's Mueller matrix for a branch, for the VaryR1 rotate (spec 028): the bound RESOLVED sample
/// re-solved at the run wavelength / normal incidence on that branch, or — with NO sample — the identity
/// MM for the transmitted branch (a transparent pass-through; the Malus law then holds exactly) and
/// `None` for the reflected branch (there is nothing to reflect).
let private rotateSampleMueller (model : Model) (sampleOpt : Propagation.ResolvedSample option) (branch : Propagation.Branch) : MuellerMatrix option =
    let w = runWaveLength model
    match sampleOpt with
    | Some s -> Some (Propagation.sampleMueller branch s w IncidenceAngle.normal)
    | None ->
        match branch with
        | Propagation.BranchTransmitted -> Some Propagation.identityMueller
        | Propagation.BranchReflected -> None

/// A prose description of the run: which source / sample / detector are bound, the varied element, the
/// capture mode, and what is varied.
let private describeRun (model : Model) (label : string) (captureText : string) (varyText : string) : string =
    let wNm = (runWaveLength model).value / nmToMeter / oneNanometer
    let sampleText =
        match runSampleOpt model with
        | Some s -> s.name
        | None -> "no sample (identity pass-through)"
    let detectorText =
        match runDetectorKind model with
        | Library.Ellipsometer -> "ellipsometer (Ψ/Δ)"
        | Library.Intensity -> "intensity detector (S₀)"
    $"Source %.0f{wNm} nm → %s{sampleText} → %s{detectorText}; varied element: %s{label}; capture %s{captureText}. Varying %s{varyText}."

/// The end-to-end experiment result as a renderer-neutral `ExperimentChart` (spec 028), computed from the
/// current DRAFT: VaryR1 ⇒ the rotating-analyzer intensity curve over the chosen R1 range (or a single-point
/// Ψ/Δ for an ellipsometer); VaryR2 ⇒ value-vs-incidence with the sample re-solved at each angle; VaryWave-
/// Length ⇒ value-vs-λ. The CAPTURE mode (T / R / both) selects the sample branch — "both" yields one series
/// per branch — and an ellipsometer detector yields Ψ/Δ series. Empty when the draft is incomplete, its
/// element is gone, or an R2 / λ vary has no bound sample. Public so the host's branches are unit-testable.
/// The chart for an EXPLICIT experiment configuration (element id + variable + capture + range + label),
/// used by both the live draft preview (`experimentResult`) and the per-experiment "View" action
/// (`chartForExperiment`). Empty when the element is no longer present.
let chartForParams
    (model : Model)
    (chosen : Library.ElementId)
    (variable : Experiments.VariableParameter)
    (measurement : Experiments.MeasurementMode)
    (range : Experiments.VariableRange)
    (label : string) : ExperimentChart.ExperimentChart =
    if not (model.elements |> List.exists (fun e -> e.id = chosen)) then ExperimentChart.empty
    else
    // Resolve the bound sample's materials ONCE per run (spec 0033 step 001); an unknown material id is
    // a typed error the chart surfaces as its message — no series are computed from a fallback.
    match runResolvedSampleOpt model with
    | Some (Error err) ->
        { ExperimentChart.empty with title = "Sample materials failed to resolve"; description = materialErrorText err }
    | resolved ->
        let sampleOpt = match resolved with Some (Ok s) -> Some s | _ -> None
        let svIn = runInputStokes model
        let w = runWaveLength model
        let detector = runDetectorKind model
        let captureText = measurement.label
        let n = max 2 range.points
        match variable with
        | Experiments.VaryR1 ->
            let varyText = $"the rotation R1 over %g{range.min}…%g{range.max}°"
            match detector with
            | Library.Ellipsometer ->
                {
                    series = []
                    xLabel = ""
                    yLabel = ""
                    title = "Ellipsometer readout"
                    description = describeRun model label captureText (varyText + " (single-point Ψ/Δ — use an R2 / λ vary for a curve)")
                    angular = true
                }
            | Library.Intensity ->
                let analyzerKind = runAnalyzerKind model (Some chosen)
                let seriesList =
                    branchesFor measurement
                    |> List.choose (fun (branch, tag) ->
                        rotateSampleMueller model sampleOpt branch
                        |> Option.map (fun mm ->
                            let curve = Propagation.rotatingAnalyzerCurveRange svIn mm analyzerKind range.min range.max n
                            series (seriesName measurement tag "Intensity") curve.points))
                {
                    series = seriesList
                    xLabel = "Rotation R1 (°)"
                    yLabel = "Intensity (S₀)"
                    title = "Rotating analyzer (Malus)"
                    description = describeRun model label captureText varyText
                    angular = true
                }
        | Experiments.VaryR2 ->
            match sampleOpt with
            | Some sample ->
                // The engine cannot solve exactly 90° incidence, so the top is clamped below it (drawn to 90).
                let lo = max 0.0 (min range.min range.max)
                let hi = min Propagation.r2SweepMaxDegrees (max range.min range.max)
                let varyText = $"the incidence angle R2 over %g{range.min}…%g{range.max}° (%g{hi}° computed, drawn to 90°)"
                match detector with
                | Library.Ellipsometer ->
                    let seriesList =
                        branchesFor measurement
                        |> List.collect (fun (branch, tag) ->
                            let psi, delta = Propagation.r2SweepPsiDelta branch svIn sample w lo hi n
                            [ series (seriesName measurement tag "Ψ") psi; series (seriesName measurement tag "Δ") delta ])
                    {
                        series = seriesList
                        xLabel = "Incidence angle R2 (°)"
                        yLabel = "Ψ, Δ (°)"
                        title = "Ellipsometric Ψ/Δ vs incidence"
                        description = describeRun model label captureText varyText
                        angular = true
                    }
                | Library.Intensity ->
                    let seriesList =
                        branchesFor measurement
                        |> List.map (fun (branch, tag) ->
                            let curve = Propagation.r2SweepCurve branch svIn sample w (runAnalyzerOpt model) lo hi n
                            series (seriesName measurement tag "Intensity") curve)
                    {
                        series = seriesList
                        xLabel = "Incidence angle R2 (°)"
                        yLabel = "Intensity (S₀)"
                        title = "Intensity vs incidence"
                        description = describeRun model label captureText varyText
                        angular = true
                    }
            | None -> ExperimentChart.empty
        | Experiments.VaryWaveLength ->
            match sampleOpt with
            | Some sample ->
                let lo = min range.min range.max
                let hi = max range.min range.max
                let inc = IncidenceAngle.normal
                let varyText = $"the wavelength over %g{lo}…%g{hi} nm"
                match detector with
                | Library.Ellipsometer ->
                    let seriesList =
                        branchesFor measurement
                        |> List.collect (fun (branch, tag) ->
                            let psi, delta = Propagation.waveLengthSweepPsiDelta branch svIn sample inc lo hi n
                            [ series (seriesName measurement tag "Ψ") psi; series (seriesName measurement tag "Δ") delta ])
                    {
                        series = seriesList
                        xLabel = "Wavelength (nm)"
                        yLabel = "Ψ, Δ (°)"
                        title = "Ellipsometric Ψ/Δ vs wavelength"
                        description = describeRun model label captureText varyText
                        angular = false
                    }
                | Library.Intensity ->
                    let seriesList =
                        branchesFor measurement
                        |> List.map (fun (branch, tag) ->
                            let curve = Propagation.waveLengthSweepIntensity branch svIn sample inc (runAnalyzerOpt model) lo hi n
                            series (seriesName measurement tag "Intensity") curve)
                    {
                        series = seriesList
                        xLabel = "Wavelength (nm)"
                        yLabel = "Intensity (S₀)"
                        title = "Intensity vs wavelength"
                        description = describeRun model label captureText varyText
                        angular = false
                    }
            | None -> ExperimentChart.empty

/// The live DRAFT's chart (the inline bay preview + the draft "Open chart" action). Public so the host's
/// branches are unit-testable without a window.
let experimentResult (model : Model) : ExperimentChart.ExperimentChart =
    let draft = model.experimentCollection.draft
    match draft.elementId, draft.variable with
    | Some chosen, Some variable -> chartForParams model chosen variable draft.measurement draft.range draft.elementLabel
    | _ -> ExperimentChart.empty

/// The chart of a specific COLLECTED experiment — the per-row "View" action.
let chartForExperiment (model : Model) (exp : Experiments.Experiment) : ExperimentChart.ExperimentChart =
    chartForParams model exp.elementId exp.variable exp.measurement exp.range exp.elementLabel

// Spec 0027: wire the forward-referenced hooks (defined once `chartForParams` exists) so a double-click /
// "Open chart" / per-row "View" opens the pop-out ScottPlot `ChartWindow` for the relevant chart (guarded
// behind a non-empty chart; never reached under the headless `ui-smoke` gate's frame render).
openChartWindowHook <-
    fun model ->
        let chart = experimentResult model
        if not (List.isEmpty chart.series) then ChartWindow(chart).Show()

viewExperimentHook <-
    fun model idStr ->
        match System.Int32.TryParse idStr with
        | true, i ->
            match model.experimentCollection.experiments |> List.tryFind (fun e -> e.id.value = i) with
            | Some exp ->
                let chart = chartForExperiment model exp
                if not (List.isEmpty chart.series) then ChartWindow(chart).Show()
            | None -> ()
        | _ -> ()

/// The single-point Ψ/Δ readout for the VaryR1 + ellipsometer case (the inline bay shows it as text). Kept
/// separate from `experimentResult` (which carries no Ψ/Δ for that case — there is no curve) so the bay can
/// still show the numeric reading. `None` for an intensity detector or any vary that produces series.
let private experimentPsiDelta (model : Model) : (float * float) option =
    let draft = model.experimentCollection.draft
    match draft.elementId, draft.variable with
    | Some chosen, Some Experiments.VaryR1 when model.elements |> List.exists (fun e -> e.id = chosen) ->
        match runDetectorKind model with
        | Library.Ellipsometer ->
            // A failed material resolution yields no readout — the chart already carries the message.
            match runResolvedSampleOpt model with
            | Some (Error _) -> None
            | resolved ->
                let sampleOpt = match resolved with Some (Ok s) -> Some s | _ -> None
                let svIn = runInputStokes model
                let branch = branchesFor draft.measurement |> List.head |> fst
                let mm = rotateSampleMueller model sampleOpt branch |> Option.defaultValue Propagation.identityMueller
                let pd = Propagation.ellipsometerReadout (mm * svIn)
                Some (pd.psi.degrees, pd.delta.degrees)
        | Library.Intensity -> None
    | _ -> None

/// The Experiments bay state for the current scene (spec 028): the present elements as candidates, the
/// element-constrained variable choices, the current draft (chosen element / variable / capture / range),
/// whether it can be committed and whether it is editing, the collection of added experiments, and the
/// inline chart of the draft's run. Disabled when the scene has no elements. Public so the host's bay
/// projection is unit-testable without mounting a window.
let experimentState (model : Model) : ExperimentControls.State =
    let col = model.experimentCollection
    let draft = col.draft
    let chart = experimentResult model
    let variableChoices =
        match draft.elementId with
        | Some id ->
            match model.elements |> List.tryFind (fun e -> e.id = id) with
            | Some e -> Experiments.variablesFor e.placement.catalogueKind |> List.map toVariableChoice
            | None -> []
        | None -> []
    {
        candidates = sweepCandidates model
        chosenId = draft.elementId |> Option.map (fun id -> id.value)
        variableChoices = variableChoices
        chosenVariable = draft.variable |> Option.map toVariableChoice
        measurement = toMeasurementChoice draft.measurement
        rangeMin = draft.range.min
        rangeMax = draft.range.max
        rangePoints = draft.range.points
        rangeUnitLabel = (match draft.variable with Some v -> v.unitLabel | None -> "°")
        canAdd = Experiments.canCommit col
        isEditing = (match draft.editingId with Some _ -> true | None -> false)
        collection =
            col.experiments
            |> List.map (fun e ->
                { ExperimentControls.ExperimentRow.id = string e.id.value; description = e.description; isEditing = (draft.editingId = Some e.id) })
        readout = experimentReadout model
        enabled = not (List.isEmpty model.elements)
        series = chart.series |> List.map (fun s -> { ExperimentControls.ChartSeries.name = s.name; points = s.points })
        xLabel = chart.xLabel
        yLabel = chart.yLabel
        description = chart.description
        psiDelta = experimentPsiDelta model
    }

let private experimentHandlers (dispatch : Msg -> unit) : ExperimentControls.Handlers =
    {
        chooseElement = fun id -> dispatch (ExpChooseElement id)
        chooseVariable = fun v -> dispatch (ExpChooseVariable v)
        chooseMeasurement = fun m -> dispatch (ExpChooseMeasurement m)
        setRangeMin = fun v -> dispatch (ExpSetRangeMin v)
        setRangeMax = fun v -> dispatch (ExpSetRangeMax v)
        setRangePoints = fun n -> dispatch (ExpSetRangePoints n)
        addOrUpdate = fun () -> dispatch ExpCommit
        newExperiment = fun () -> dispatch ExpNew
        editExperiment = fun idStr -> dispatch (ExpEdit idStr)
        removeExperiment = fun idStr -> dispatch (ExpRemove idStr)
        viewExperiment = fun idStr -> dispatch (ViewExperiment idStr)
        openChartWindow = fun () -> dispatch OpenExperimentChartWindow
    }

// ---------------------------------------------------------------------------
// Spec 0027 (026) — the DETAILS bay: the selected element's bound Library entry shown as "what it is" (the
// heading + full description) and, for a layered sample, a layer-stack BAND view (the reusable
// `LayerBandsControls`). The host flattens the sample's engine `OpticalSystem` (`Propagation.sampleToSystem`)
// into `Band`s — borrowing the V1 `Schematic.fs` colour / height idea (replicated here as a few inline
// helpers, since TestWindows does not reference the Ui project) — collapsing runs of identical-thickness
// films into a single "×N" band so the 41-layer / 100-pair multilayers stay readable.
// ---------------------------------------------------------------------------

/// A deterministic, process-independent hash of a string (FNV-1a fold; NOT `GetHashCode`, whose seed is
/// per-process randomised). Used to colour a band whose material has no curated colour.
let private stableHashStr (s : string) : int =
    (s |> Seq.fold (fun acc ch -> (acc ^^^ int ch) * 16777619) (int 2166136261u)) &&& 0x7fffffff

/// A small `#RRGGBB` palette for unknown band materials (mirrors the Schematic fallback palette).
let private bandPalette : string[] =
    [| "#1F77B4"; "#FF7F0E"; "#2CA02C"; "#D62728"; "#9467BD"; "#8C564B"; "#E377C2"; "#BCBD22" |]

/// Curated `#RRGGBB` colours for the built-in band materials (mirrors `Schematic.curated`), keyed by
/// the elevated `MaterialId` (spec 0033 step 002) — the same key the sample structure carries; the
/// old host id literals are gone with the string-id path.
let private bandColors : Map<MaterialLibrary.MaterialId, string> =
    Map
        [
            MaterialLibrary.MaterialIds.glass152, "#C8E1F5"
            MaterialLibrary.MaterialIds.glass150, "#CDE6FA"
            MaterialLibrary.MaterialIds.glass175, "#AACDEB"
            MaterialLibrary.MaterialIds.vacuum, "#F2F2F2"
            MaterialLibrary.MaterialIds.euvMolybdenum, "#5A5A6E"
            MaterialLibrary.MaterialIds.euvSilicon, "#5A5A6E"
            MaterialLibrary.MaterialIds.silicon, "#5A5A6E"
            MaterialLibrary.MaterialIds.langasite, "#78C8DC"
            MaterialLibrary.MaterialIds.uniaxialCrystal, "#AFE1AF"
            MaterialLibrary.MaterialIds.biaxialCrystal, "#96D296"
            MaterialLibrary.MaterialIds.activeCrystal, "#C8B4E6"
        ]

/// A curated colour for a known band material id, else a stable palette slot hashed from the id's
/// Guid string form. Pure and total.
let private bandColorHex (materialId : MaterialLibrary.MaterialId) : string =
    match Map.tryFind materialId bandColors with
    | Some hex -> hex
    | None -> bandPalette.[stableHashStr (string materialId.value) % bandPalette.Length]

/// The display name of a material id: the library entry's name, or the id's Guid string form when
/// unknown (an unknown id still LABELS its band — running the sample is what surfaces the typed
/// resolution error).
let private materialDisplayName (materialId : MaterialLibrary.MaterialId) : string =
    materialLibrary.entries
    |> List.tryFind (fun e -> e.id = materialId)
    |> Option.map (fun e -> e.name)
    |> Option.defaultValue (string materialId.value)

/// A band's thickness for the Details view — finite layers carry their thickness in metres; a half-space /
/// plate carries none (drawn as "semi-infinite"). This keeps the engine's `Thickness` DU (which collides
/// with `Avalonia.Thickness`) out of the UI layer — the metres come from `Propagation.thicknessMeters`.
type private BandThickness =
    | FiniteMeters of float
    | SemiInfinite

/// A human thickness label for a band (nm under 1 µm, µm under 1 mm, else mm). The SOLE display conversion
/// (a band label, never written back).
let private bandThicknessLabel (t : BandThickness) : string =
    match t with
    | SemiInfinite -> "semi-infinite"
    | FiniteMeters meters ->
        let nm = meters * 1.0e9
        if nm < 1000.0 then $"%.1f{nm} nm"
        elif nm < 1.0e6 then $"%.3g{(nm / 1000.0)} µm"
        else $"%.3g{(nm / 1.0e6)} mm"

/// The relative height weight of a band (log-compressed thickness in metres, so a 2.65 nm EUV layer and a
/// 1 cm plate are both visible). A half-space / plate gets a fixed mid weight.
let private bandWeight (t : BandThickness) : float =
    match t with
    | SemiInfinite -> 1.0
    | FiniteMeters m ->
        let meters = max 1.0e-12 m
        // log10 of nanometres, floored — a 1 nm layer ⇒ 0, a 1 µm layer ⇒ 3, a 1 mm layer ⇒ 6.
        max 0.1 (log10 (meters * 1.0e9))

/// The thickness of a sample-structure layer as a `BandThickness` (via `Propagation.thicknessMeters`,
/// so the engine `Thickness` DU never reaches this file).
let private layerBandThickness (layer : Library.SampleLayer) : BandThickness =
    match Propagation.thicknessMeters layer.thickness with
    | Some m -> FiniteMeters m
    | None -> SemiInfinite

/// The material-labelled layer list for the band view, read STRAIGHT off the sample's structure (spec
/// 0033 step 001 — no per-sample-id table): each band is (material id, thickness, repeat count). A
/// `Repeated` period group stays collapsed (one "×N" band per cell layer), the substrate plate follows
/// the films, and a non-vacuum lower half-space renders as a semi-infinite band.
let private sampleBandSpecs (sample : Library.Sample) : (MaterialLibrary.MaterialId * BandThickness * int) list =
    let filmBands =
        sample.structure.films
        |> List.collect (fun item ->
            match item with
            | Library.SingleLayer l -> [ l.materialId, layerBandThickness l, 1 ]
            | Library.Repeated g -> g.cell |> List.map (fun l -> l.materialId, layerBandThickness l, g.count))
    let substrateBands =
        match sample.structure.substrate with
        | Some l -> [ l.materialId, layerBandThickness l, 1 ]
        | None -> []
    let lowerBands =
        match sample.structure.lower with
        | Some materialId -> [ materialId, SemiInfinite, 1 ]
        | None -> []
    filmBands @ substrateBands @ lowerBands

/// The band-view state of ONE sample — its "×N"-collapsed stack (one band per unit-cell layer of a
/// `Repeated` group, then the substrate / lower half-space), titled with the sample's name +
/// description. Shared by the Details bay (the selected element's bound sample) and the Library
/// bay's View panel (spec 0033 step 024). Public so the hosts' band projection is unit-testable
/// without mounting a window.
let sampleBandsState (s : Library.Sample) : LayerBandsControls.State =
    let bands =
        sampleBandSpecs s
        |> List.map (fun (materialId, thickness, count) ->
            let countText = if count > 1 then $" ×%d{count}" else ""
            ({
                label = $"%s{(materialDisplayName materialId)} — %s{(bandThicknessLabel thickness)}%s{countText}"
                heightWeight = bandWeight thickness
                colorHex = bandColorHex materialId
             } : LayerBandsControls.Band))
    ({ title = $"%s{s.name} — %s{s.description}"; bands = bands } : LayerBandsControls.State)

/// Build the `LayerBandsControls.State` for the selected element. A bound layered sample yields the band
/// view (collapsed "×N" bands with material + thickness labels); a bound non-sample entry yields just the
/// title + full description; nothing selected / unbound yields a hint title and no bands.
let private detailsState (model : Model) : LayerBandsControls.State =
    match model.selection with
    | ElementSelected i ->
        let e = List.item i model.elements
        match boundEntry model e with
        | Some (Library.SampleItem s) -> sampleBandsState s
        | Some entry ->
            ({ title = $"%s{entry.displayName} — %s{entry.fullDescription}"; bands = [] } : LayerBandsControls.State)
        | None ->
            ({ title = "No Library entry bound — pick one in the Selector bay to see its details."; bands = [] } : LayerBandsControls.State)
    | TableSelected | NothingSelected ->
        ({ title = "Select an element to see what it is."; bands = [] } : LayerBandsControls.State)

// ---------------------------------------------------------------------------
// Spec 0033 (024) — the MATERIALS and LIBRARY (samples) workbench bays: the step-015/016 list
// surfaces (`MaterialsControls` / `SampleLibraryControls`) wired over the step-005/006 WRITE
// seams. The controls are domain-free, so the host flattens each search result into `Row`s and
// the facets into coded `FacetOption`s here — exactly as `libraryState` / `flattenNode` do for
// the Selector — and the projections re-query their proxy on EVERY render, so a verb's write
// refreshes the list in the same render pass. The host adds three surfaces the controls do not
// carry: the inline confirm-gated Remove row, the inline store-refusal message
// (`MaterialStillReferenced` naming the referencing samples — never a cascade), and the View
// panels (read-only metadata + the step-19 n/k chart for a material; the Details-bay band view
// for a sample).
// ---------------------------------------------------------------------------

/// Stable intent-named ids for the workbench surfaces THIS host adds around the shared controls
/// (whose own ids live in `MaterialsControls.UiIds` / `SampleLibraryControls.UiIds`).
[<RequireQualifiedAccess>]
module WorkbenchIds =
    [<Literal>]
    let materialsMessage = "MaterialsWorkbenchMessage"
    [<Literal>]
    let removeMaterialConfirm = "RemoveMaterialConfirmButton"
    [<Literal>]
    let removeMaterialCancel = "RemoveMaterialCancelButton"
    [<Literal>]
    let materialViewPanel = "MaterialViewPanel"
    [<Literal>]
    let materialNkChart = "MaterialWorkbenchNkChart"
    [<Literal>]
    let samplesMessage = "SamplesWorkbenchMessage"
    [<Literal>]
    let removeSampleConfirm = "RemoveSampleConfirmButton"
    [<Literal>]
    let removeSampleCancel = "RemoveSampleCancelButton"
    [<Literal>]
    let sampleViewPanel = "SampleViewPanel"

/// The category facet's domain choices, in display order (the "all" option is `None`).
let materialCategories : MaterialLibrary.MaterialCategory list =
    [ MaterialLibrary.Glass; MaterialLibrary.Metal; MaterialLibrary.Semiconductor; MaterialLibrary.Crystal; MaterialLibrary.Vacuum ]

/// The stable option code of a category facet choice (`None` = the match-everything "all") —
/// the string the control dispatches back and `materialCategoryOfCode` inverts.
let materialCategoryCode (category : MaterialLibrary.MaterialCategory option) : string =
    match category with
    | None -> "all"
    | Some MaterialLibrary.Glass -> "glass"
    | Some MaterialLibrary.Metal -> "metal"
    | Some MaterialLibrary.Semiconductor -> "semiconductor"
    | Some MaterialLibrary.Crystal -> "crystal"
    | Some MaterialLibrary.Vacuum -> "vacuum"

let private materialCategoryLabel (category : MaterialLibrary.MaterialCategory option) : string =
    match category with
    | None -> "All"
    | Some MaterialLibrary.Glass -> "Glass"
    | Some MaterialLibrary.Metal -> "Metal"
    | Some MaterialLibrary.Semiconductor -> "Semiconductor"
    | Some MaterialLibrary.Crystal -> "Crystal"
    | Some MaterialLibrary.Vacuum -> "Vacuum"

/// The inverse code → facet mapping (an unknown code is the match-everything "all").
let materialCategoryOfCode (code : string) : MaterialLibrary.MaterialCategory option =
    materialCategories |> List.tryFind (fun c -> materialCategoryCode (Some c) = code)

/// The dispersion facet's choices, in display order.
let dispersionFilters : MaterialLibrary.DispersionFilter list =
    [ MaterialLibrary.AnyDispersion; MaterialLibrary.OnlyDispersive; MaterialLibrary.OnlyNonDispersive ]

let dispersionFilterCode (filter : MaterialLibrary.DispersionFilter) : string =
    match filter with
    | MaterialLibrary.AnyDispersion -> "all"
    | MaterialLibrary.OnlyDispersive -> "dispersive"
    | MaterialLibrary.OnlyNonDispersive -> "constant"

let private dispersionFilterLabel (filter : MaterialLibrary.DispersionFilter) : string =
    match filter with
    | MaterialLibrary.AnyDispersion -> "All"
    | MaterialLibrary.OnlyDispersive -> "Dispersive"
    | MaterialLibrary.OnlyNonDispersive -> "Non-dispersive"

let dispersionFilterOfCode (code : string) : MaterialLibrary.DispersionFilter =
    dispersionFilters
    |> List.tryFind (fun f -> dispersionFilterCode f = code)
    |> Option.defaultValue MaterialLibrary.AnyDispersion

/// The substrate facet's choices, in display order (`None` = the match-everything "all").
let substrateFacets : Library.SubstrateKind option list =
    [ None; Some Library.ThinFilm; Some Library.Plate; Some Library.Wedge ]

let substrateFacetCode (substrate : Library.SubstrateKind option) : string =
    match substrate with
    | None -> "all"
    | Some Library.ThinFilm -> "thinfilm"
    | Some Library.Plate -> "plate"
    | Some Library.Wedge -> "wedge"

let private substrateFacetLabel (substrate : Library.SubstrateKind option) : string =
    match substrate with
    | None -> "All"
    | Some Library.ThinFilm -> "Thin film"
    | Some Library.Plate -> "Plate"
    | Some Library.Wedge -> "Wedge"

let substrateFacetOfCode (code : string) : Library.SubstrateKind option =
    substrateFacets
    |> List.choose id
    |> List.tryFind (fun k -> substrateFacetCode (Some k) = code)

/// The Materials bay state: the model's `MaterialQuery` run through the store's search seam and
/// flattened to `MaterialsControls.Row`s. A row is `Editable` iff its entry carries the step-013
/// edit model (`complexity = Some`); a `complexity = None` engine preset is `ViewOnly` (the
/// control REMOVES its Edit verb). Public so the bay projection is unit-testable without a window.
let materialsState (model : Model) : MaterialsControls.State =
    let rows =
        match model.materials.searchMaterials model.materialQuery with
        | Ok entries ->
            entries
            |> List.map (fun e ->
                ({
                    materialId = string e.id.value
                    label = e.name
                    editability =
                        match e.complexity with
                        | Some _ -> MaterialsControls.Editable
                        | None -> MaterialsControls.ViewOnly
                 } : MaterialsControls.Row))
        | Error _ -> []
    {
        searchText = model.materialQuery.text
        categoryOptions =
            None :: (materialCategories |> List.map Some)
            |> List.map (fun c -> ({ code = materialCategoryCode c; label = materialCategoryLabel c } : MaterialsControls.FacetOption))
        selectedCategory = materialCategoryCode model.materialQuery.category
        dispersionOptions =
            dispersionFilters
            |> List.map (fun f -> ({ code = dispersionFilterCode f; label = dispersionFilterLabel f } : MaterialsControls.FacetOption))
        selectedDispersion = dispersionFilterCode model.materialQuery.dispersion
        rows = rows
        selectedId = model.selectedMaterial |> Option.map (fun id -> string id.value)
    }

/// The Materials bay handlers: each control callback is one dispatched `Mat…` message; the
/// option codes and the row's Guid-string id are lifted back to their domain types HERE, at the
/// control boundary (an unparsable row id dispatches nothing).
let private materialsHandlers (dispatch : Msg -> unit) : MaterialsControls.Handlers =
    {
        setSearchText = fun text -> dispatch (MatSetSearchText text)
        selectCategory = fun code -> dispatch (MatSelectCategory (materialCategoryOfCode code))
        selectDispersion = fun code -> dispatch (MatSelectDispersion (dispersionFilterOfCode code))
        selectMaterial =
            fun idStr ->
                // `MaterialLibrary.MaterialId` in expression position names the union CASE (the
                // documented type/case collision), so parse the Guid and construct directly.
                match System.Guid.TryParse idStr with
                | true, g -> dispatch (MatSelectRow (MaterialLibrary.MaterialId g))
                | _ -> ()
        addMaterial = fun () -> dispatch MatAdd
        editMaterial = fun () -> dispatch MatEdit
        removeMaterial = fun () -> dispatch MatRequestRemove
        viewMaterial = fun () -> dispatch MatView
    }

/// The Library (samples workbench) bay state — the `SampleQuery` through `searchSamples`,
/// flattened to `SampleLibraryControls.Row`s. Public for the same testability reason.
let samplesState (model : Model) : SampleLibraryControls.State =
    let rows =
        match model.samples.searchSamples model.sampleQuery with
        | Ok found -> found |> List.map (fun s -> ({ sampleId = string s.id.value; label = s.name } : SampleLibraryControls.Row))
        | Error _ -> []
    {
        searchText = model.sampleQuery.text
        substrateOptions =
            substrateFacets
            |> List.map (fun k -> ({ code = substrateFacetCode k; label = substrateFacetLabel k } : SampleLibraryControls.FacetOption))
        selectedSubstrate = substrateFacetCode model.sampleQuery.substrate
        rows = rows
        selectedId = model.selectedSample |> Option.map (fun id -> string id.value)
    }

let private samplesHandlers (dispatch : Msg -> unit) : SampleLibraryControls.Handlers =
    {
        setSearchText = fun text -> dispatch (SmpSetSearchText text)
        selectSubstrate = fun code -> dispatch (SmpSelectSubstrate (substrateFacetOfCode code))
        selectSample =
            fun idStr ->
                match System.Guid.TryParse idStr with
                | true, g -> dispatch (SmpSelectRow (Library.SampleId g))
                | _ -> ()
        addSample = fun () -> dispatch SmpAdd
        editSample = fun () -> dispatch SmpEdit
        removeSample = fun () -> dispatch SmpRequestRemove
        viewSample = fun () -> dispatch SmpView
        makeMultilayer = fun () -> dispatch SmpMakeMultilayer
    }

/// Set `AutomationProperties.AutomationId` (freely mutable, unlike `Control.Name`) through
/// FuncUI's attr builder — the confirm buttons / panels appear and disappear with the model, so
/// they carry AutomationIds (the MaterialsControls discipline).
let private workbenchAutomationId (autoId : string) : IAttr<Border> =
    AttrBuilder<Border>.CreateProperty<string>(AutomationProperties.AutomationIdProperty, autoId, ValueNone)

/// A small clickable verb box for the host-added confirm rows (the MaterialsControls button look).
let private workbenchButton (autoId : string) (label : string) (onClick : unit -> unit) : IView =
    Border.create [
        workbenchAutomationId autoId
        Border.background (brush (color 232 232 232))
        Border.borderBrush (brush (color 120 120 120))
        Border.borderThickness 1.0
        Border.cornerRadius (CornerRadius 3.0)
        Border.padding (Thickness(12.0, 5.0))
        Border.margin (Thickness(0.0, 0.0, 8.0, 4.0))
        Border.verticalAlignment VerticalAlignment.Center
        Border.child (TextBlock.create [ TextBlock.text label ])
        Border.onPointerPressed ((fun e -> e.Handled <- true; onClick ()), SubPatchOptions.OnChangeOf autoId)
    ] :> IView

let private workbenchMessageColor = color 178 34 34

/// The inline remove confirmation for the Materials bay — present only while a remove is armed.
/// A WrapPanel row (the 022 headless-layout lesson: no long horizontal StackPanels).
let private materialConfirmRow (model : Model) (dispatch : Msg -> unit) : IView list =
    match model.materialRemoveConfirm with
    | NoRemoveConfirm -> []
    | ConfirmingRemove id ->
        let name =
            match model.materials.tryGetMaterial id with
            | Ok (Some entry) -> entry.name
            | Ok None | Error _ -> string id.value
        [ WrapPanel.create [
              WrapPanel.orientation Orientation.Horizontal
              WrapPanel.children [
                  TextBlock.create [
                      TextBlock.text $"Remove material '%s{name}'?"
                      TextBlock.verticalAlignment VerticalAlignment.Center
                      TextBlock.margin (Thickness(0.0, 0.0, 8.0, 4.0))
                  ]
                  workbenchButton WorkbenchIds.removeMaterialConfirm "Remove" (fun () -> dispatch MatConfirmRemove)
                  workbenchButton WorkbenchIds.removeMaterialCancel "Cancel" (fun () -> dispatch MatCancelRemove)
              ]
          ] :> IView ]

/// The Materials bay's inline store-refusal message. Every `MaterialError` case carries its
/// diagnostic reason (spec 0033 step 003); `MaterialStillReferenced`'s reason NAMES the
/// referencing samples, so the block reads as "why" — never a cascade, never a dialog.
let private materialsMessageRow (model : Model) : IView list =
    match model.materialsError with
    | None -> []
    | Some err ->
        let text =
            match err with
            | MaterialLibrary.UnknownMaterialId reason
            | MaterialLibrary.DuplicateMaterialId reason
            | MaterialLibrary.MaterialStillReferenced reason
            | MaterialLibrary.InvalidMaterial reason -> reason
        [ TextBlock.create [
              TextBlock.name WorkbenchIds.materialsMessage
              TextBlock.foreground (brush workbenchMessageColor)
              TextBlock.textWrapping TextWrapping.Wrap
              TextBlock.maxWidth 760.0
              TextBlock.text text
          ] :> IView ]

/// The Materials View panel: the entry's read-only metadata plus the step-19 dual-axis n/k
/// chart over the editor's preview range, drawn by the ONE shared inline renderer
/// (`NkDispersionChart.inlineCanvas`). Resolved through the proxy at render time, so a removed
/// entry's panel vanishes with its row.
let private materialViewPanel (model : Model) : IView list =
    match model.viewedMaterial with
    | None -> []
    | Some id ->
        match model.materials.tryGetMaterial id with
        | Ok (Some entry) ->
            let chart = NkDispersionChart.nkDispersionChart entry.properties Units.Nanometer MaterialEditorView.previewRange
            let editability =
                match entry.complexity with
                | Some _ -> ""
                | None -> " (view-only engine preset)"
            [ Border.create [
                  workbenchAutomationId WorkbenchIds.materialViewPanel
                  Border.child (
                      StackPanel.create [
                          StackPanel.orientation Orientation.Vertical
                          StackPanel.spacing 2.0
                          StackPanel.children [
                              TextBlock.create [
                                  TextBlock.fontWeight FontWeight.SemiBold
                                  TextBlock.text $"%s{entry.name} — %s{(materialCategoryLabel (Some entry.category))}%s{editability}"
                              ]
                              TextBlock.create [
                                  TextBlock.textWrapping TextWrapping.Wrap
                                  TextBlock.maxWidth 760.0
                                  TextBlock.text (entry.description |> Option.defaultValue "")
                              ]
                              NkDispersionChart.inlineCanvas WorkbenchIds.materialNkChart chart
                          ]
                      ])
              ] :> IView ]
        | Ok None | Error _ -> []

/// The Library bay's inline remove confirmation (samples remove is not reference-blocked — the
/// gate is the user's own confirm).
let private sampleConfirmRow (model : Model) (dispatch : Msg -> unit) : IView list =
    match model.sampleRemoveConfirm with
    | NoRemoveConfirm -> []
    | ConfirmingRemove id ->
        let name =
            match model.samples.tryGetSample id with
            | Ok (Some s) -> s.name
            | Ok None | Error _ -> string id.value
        [ WrapPanel.create [
              WrapPanel.orientation Orientation.Horizontal
              WrapPanel.children [
                  TextBlock.create [
                      TextBlock.text $"Remove sample '%s{name}'?"
                      TextBlock.verticalAlignment VerticalAlignment.Center
                      TextBlock.margin (Thickness(0.0, 0.0, 8.0, 4.0))
                  ]
                  workbenchButton WorkbenchIds.removeSampleConfirm "Remove" (fun () -> dispatch SmpConfirmRemove)
                  workbenchButton WorkbenchIds.removeSampleCancel "Cancel" (fun () -> dispatch SmpCancelRemove)
              ]
          ] :> IView ]

let private samplesMessageRow (model : Model) : IView list =
    match model.samplesError with
    | None -> []
    | Some err ->
        let text =
            match err with
            | Library.UnknownSampleId reason
            | Library.DuplicateSampleId reason
            | Library.InvalidSample reason -> reason
        [ TextBlock.create [
              TextBlock.name WorkbenchIds.samplesMessage
              TextBlock.foreground (brush workbenchMessageColor)
              TextBlock.textWrapping TextWrapping.Wrap
              TextBlock.maxWidth 760.0
              TextBlock.text text
          ] :> IView ]

/// The Library View panel: the sample's read-only metadata plus the `LayerBandsControls` band
/// view over its "×N"-collapsed stack — exactly what the Details bay renders for a bound sample
/// (the shared `sampleBandsState`).
let private sampleViewPanel (model : Model) : IView list =
    match model.viewedSample with
    | None -> []
    | Some id ->
        match model.samples.tryGetSample id with
        | Ok (Some s) ->
            [ Border.create [
                  workbenchAutomationId WorkbenchIds.sampleViewPanel
                  Border.child (
                      StackPanel.create [
                          StackPanel.orientation Orientation.Vertical
                          StackPanel.spacing 2.0
                          StackPanel.children [
                              TextBlock.create [
                                  TextBlock.fontWeight FontWeight.SemiBold
                                  TextBlock.text $"%s{s.name} — %s{(substrateFacetLabel (Some s.substrate))}"
                              ]
                              LayerBandsControls.view (sampleBandsState s)
                          ]
                      ])
              ] :> IView ]
        | Ok None | Error _ -> []

/// The Materials bay content: the shared list surface, then the host-added inline confirm /
/// message / View-panel rows.
let private materialsBay (model : Model) (dispatch : Msg -> unit) : IView =
    StackPanel.create [
        StackPanel.orientation Orientation.Vertical
        StackPanel.spacing 4.0
        StackPanel.children (
            [ MaterialsControls.view (materialsState model) (materialsHandlers dispatch) ]
            @ materialConfirmRow model dispatch
            @ materialsMessageRow model
            @ materialViewPanel model)
    ] :> IView

/// The Library (samples workbench) bay content.
let private samplesBay (model : Model) (dispatch : Msg -> unit) : IView =
    StackPanel.create [
        StackPanel.orientation Orientation.Vertical
        StackPanel.spacing 4.0
        StackPanel.children (
            [ SampleLibraryControls.view (samplesState model) (samplesHandlers dispatch) ]
            @ sampleConfirmRow model dispatch
            @ samplesMessageRow model
            @ sampleViewPanel model)
    ] :> IView

/// The Main-screen ribbon Bays — every large control, each bound to the current model / dispatch. Adding
/// or removing a Bay here is the ONLY change needed to add / remove a large control from the Main screen.
let mainBays (model : Model) (dispatch : Msg -> unit) : Ribbon.Bay list =
    [ { name = BayNames.rotation; content = RotationControls.view (rotationState model) (rotationHandlers dispatch) }
      { name = BayNames.move; content = RayPositionControls.view (moveState model) (moveHandlers dispatch) }
      { name = BayNames.add; content = ElementPaletteControls.view (paletteState model) (paletteHandlers model dispatch) }
      { name = BayNames.render; content = RendererControls.view model.render (renderHandlers dispatch) }
      { name = BayNames.selector; content = LibraryControls.view (libraryState model) (libraryHandlers dispatch) }
      { name = BayNames.materials; content = materialsBay model dispatch }
      { name = BayNames.library; content = samplesBay model dispatch }
      { name = BayNames.experiments; content = ExperimentControls.view (experimentState model) (experimentHandlers dispatch) }
      { name = BayNames.details; content = LayerBandsControls.view (detailsState model) } ]

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
