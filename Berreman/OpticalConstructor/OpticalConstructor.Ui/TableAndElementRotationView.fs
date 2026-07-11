/// The third test window (Spec 0027, task 006 #3): test #1 and test #2 together — rotate the
/// TABLE and the ELEMENTS. The table view is fully live (rotate when selected, pan, zoom) and a
/// few optical elements sit on it, each with its own spec rotation. The rotation gestures act on
/// whatever is SELECTED: click empty table → the table is selected and the gestures rotate the
/// VIEW; click an element → it is selected and the gestures rotate that ELEMENT. The elements are
/// drawn by projecting their table-frame geometry through the (rotatable) table view, so when the
/// table is rotated the elements are "snapped to it" — their own rotation angles do not change,
/// only how they project to the screen does. The `Model`/`Msg`/`update` are pure and Avalonia-free.
module OpticalConstructor.Ui.TableAndElementRotationView

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
// Spec 0038 (017): opened for `QuickPickThreshold.defaultValue` — the single-case DU's case
// shadows the type under `WorkbenchSettings.`-qualified expression resolution.
open OpticalConstructor.Domain.WorkbenchSettings
// Spec 0038 Part L (037): opened so `CollectionName.tryCreate` resolves to the TYPE's static member —
// the module-qualified `ExperimentCollectionStore.CollectionName.tryCreate` binds the same-named
// union CASE in expression position instead (the `WorkbenchSettings` shadowing note above).
open OpticalConstructor.Domain.ExperimentCollectionStore
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
    /// Spec 0038 (031): the out-of-band dispersion warning badge on scene element index `i` — an
    /// indexed id (the `LayerBandsControls.UiIds.band` pattern) so a headless test addresses the
    /// per-element badge. Its hover tooltip names the offending material(s) and both wavelength ranges.
    let badgePrefix = "OutOfBandBadge_"
    let outOfBandBadge (i : int) : string = $"%s{badgePrefix}%d{i}"

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

/// The side-effecting "open a window" seam behind the workbench strip buttons (spec 0033
/// step 024 / spec 0038 steps 013+015). Opening a `Window` is IO (the `openChartWindowHook`
/// precedent), so the verbs reach it through an injected function record (the functional-proxy
/// convention): a headless test substitutes recording launchers — possibly still constructing
/// the real windows — and observes exactly which window a verb requested. Function-valued
/// fields have no structural equality, so the record compares by reference (the model keeps
/// its Elmish-required equality).
[<ReferenceEquality>]
type EditorLaunchers =
    {
        /// Open the SINGLE-INSTANCE Materials window (spec 0038 step 013, UICOMP_XDUO_0009) over
        /// the app-scope material + category stores — the ribbon strip's `Materials…` button
        /// reaches it through this seam, so a headless test substitutes a recording launcher and
        /// observes the request. The Material / Category editor launchers the retired Materials
        /// bay carried here moved INTO that window's own composition (`MaterialsWindow`).
        openMaterialsWindow : MaterialLibrary.MaterialProxy -> MaterialLibrary.CategoryProxy -> unit
        /// Open the SINGLE-INSTANCE Library window (spec 0038 step 015, UICOMP_XDUO_0010) over
        /// the read-only Library seam plus the app-scope samples + materials + categories
        /// stores (the categories thread through to the Sample editor's step-019 Choose
        /// material… composition) — the ribbon strip's `Library…` button reaches it through
        /// this seam. The Sample-editor launcher the retired Library bay carried here
        /// (`openSampleEditor`) moved INTO that window's own composition (`LibraryWindow`),
        /// the step-013 Materials precedent.
        openLibraryWindow : Library.LibraryProxy -> Library.SampleProxy -> MaterialLibrary.MaterialProxy -> MaterialLibrary.CategoryProxy -> unit
        /// Spec 0038 (017): open the SAME single-instance Library window in SELECT state over
        /// the session context (the Selector bay's Choose… verb). A missing window opens fresh
        /// in Select mode; a LIVE one is RE-TARGETED at the new context (a second Choose…
        /// re-points the instance — the step-016 `RetargetedWindow` semantics). The step-005
        /// modality switch decides ShowDialog-vs-Show against the REQUESTING window. Returns
        /// the opened session's cancel-and-close handle — the `SelectSession` staleness lever —
        /// or None when the open failed (the typed error is dropped at this unit seam: a failed
        /// open leaves no window, exactly the user-visible outcome).
        openLibrarySelectWindow : Library.LibraryProxy -> Library.SampleProxy -> MaterialLibrary.MaterialProxy -> MaterialLibrary.CategoryProxy -> WorkbenchSettings.SelectWindowModality -> Window -> WindowMode.SelectionContext<Library.LibraryEntry> -> (unit -> unit) option
    }

    /// The real launchers — spec 0038 step 008: every verb-opened window goes THROUGH the
    /// SVC_XDUO_0001 window-policy seam over the host-layer `WindowRegistry`, so a second open
    /// ACTIVATES the live window instead of stacking a copy; the Materials window is
    /// single-instance under `MaterialsWindowKey` (step 013) and the Library window under
    /// `LibraryWindowKey` (step 015). Windows are Browse-mode opens (`BrowseOpen` — always
    /// `Show`n), so the Select-state modality switch is never consulted on this path; the
    /// step-016 Select windows compose their own `SelectOpen` launchers over the app scope's
    /// `AppContext.settings`. A launcher record is built per dispatch (closures over the
    /// call's proxies — the shared registry is what carries the single-instance semantics); the
    /// typed open error is deliberately dropped at this unit seam — a failed open leaves no
    /// window, exactly the user-visible outcome the verb had before. Never invoked by a render,
    /// only by a verb dispatch.
    static member defaults : EditorLaunchers =
        let openBrowse (key : WindowLauncher.WindowKey) (build : unit -> Window) : unit =
            let launcher =
                WindowLauncher.WindowLauncher.create
                    (fun (_ : WindowLauncher.WindowKey) -> build () |> Ok)
                    WorkbenchSettings.SelectWindowModality.defaultValue
                    WindowLauncher.BrowseOpen
            launcher.openOrActivate key |> ignore
        {
            openMaterialsWindow =
                fun materials categories ->
                    openBrowse WindowLauncher.MaterialsWindowKey (fun () -> MaterialsWindow(materials, categories) :> Window)
            openLibraryWindow =
                fun library samples materials categories ->
                    openBrowse WindowLauncher.LibraryWindowKey (fun () -> LibraryWindow(library, samples, materials, categories) :> Window)
            openLibrarySelectWindow =
                fun library samples materials categories modality requestingWindow selectCtx ->
                    // Spec 0038 (017): the Select-state open — the SAME registry key as the
                    // Browse opens above, so Choose… and the Library… strip button meet in one
                    // open-or-activate space. A LIVE window is re-pointed through the step-016
                    // `Retarget` seam (the concrete-window downcast is the launcher contract's
                    // baked-closure shape); a missing one opens fresh in Select mode and is
                    // shown per the step-005 modality switch against the requesting window.
                    let retargetWindow (live : Window) : unit =
                        match live with
                        | :? LibraryWindow as libraryWindow -> libraryWindow.Retarget selectCtx
                        | _ -> ()
                    let launcher =
                        WindowLauncher.WindowLauncher.create
                            (fun (_ : WindowLauncher.WindowKey) -> LibraryWindow(library, samples, materials, categories, mode = WindowMode.Select selectCtx) :> Window |> Ok)
                            modality
                            (WindowLauncher.SelectOpen (requestingWindow, retargetWindow))
                    match launcher.openOrActivate WindowLauncher.LibraryWindowKey with
                    | Ok (WindowLauncher.CreatedWindow window)
                    | Ok (WindowLauncher.ActivatedWindow window)
                    | Ok (WindowLauncher.RetargetedWindow window) -> Some (fun () -> window.Close ())
                    | Error _ -> None
        }

/// Spec 0038 (016): the handle of the ONE open Select-state window session this workbench
/// requested (the step-017 Choose… flow populates it; nothing else in the workbench opens a
/// Select window yet). The window itself is HOST state — the model only holds how to end the
/// session: a changed table selection, an element add (which selects the new element), or the
/// selected element's removal invokes `cancelAndClose` (the staleness rules — the
/// pending-bind-clears precedent), and CLOSING the window is what fires the session's
/// `onCancelled` (the window's own dismissal hook), so staleness, the window's Close verb and
/// the title-bar X share one cancel path. Function-valued field ⇒ reference equality (the
/// model keeps its Elmish-required equality).
[<ReferenceEquality>]
type SelectSession =
    {
        /// The table element the Select-state window serves (`SelectionTarget`'s element case —
        /// step 017 compares it to decide re-target vs fresh open).
        target : Library.ElementId
        /// Cancel the session and CLOSE its window.
        cancelAndClose : unit -> unit
    }

/// Spec 0038 Part L (037): which constructor flow this workbench hosts. The FORWARD constructor is
/// the ordinary Main scene (build a stack, preview its response). The INVERSE constructor opens the
/// same scene seeded with an UNBOUND sample (the unknown, drawn dashed — step 30) and gathers
/// measured data to solve for it later: with NO hint (the sample unbound) the forward experiment
/// chart has nothing to compute against, so its surface is ABSENT; binding a HINT sample restores
/// the chart. An elevated two-case DU (never a naked `bool`), so a match reads as prose.
type ConstructorMode =
    | ForwardConstructor
    | InverseConstructor

/// Spec 0038 Part L (037): the typed per-experiment measured-data status shown inline in the
/// collection builder. A file that loaded and passed `validateAgainstExperiment` reports its parsed
/// point count; a parse / empty-file failure and a range / units validation failure carry the
/// domain error's `reason`. Elevated — never a bare status string in the model map — with a `.text`
/// projection the bay renders. Derived from `ExperimentDataLoad.loadAndValidate`'s typed result.
type ExperimentFileStatus =
    | DataFileValidated of points : int
    | DataFileParseError of reason : string
    | DataFileValidationError of reason : string

    member this.text : string =
        match this with
        | DataFileValidated n -> $"Loaded — %d{n} point(s), schema-valid"
        | DataFileParseError reason -> $"Parse error — %s{reason}"
        | DataFileValidationError reason -> $"Validation error — %s{reason}"

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
        /// Spec 0038 (016): the open Select-state window session (if any) this workbench
        /// requested — cancelled and closed by the staleness rules (a changed table selection,
        /// an element add, the selected element's removal) and cleared by the targeted bind.
        activeSelect : SelectSession option
        /// Spec 0038 (016): the staleness status line — a targeted Select return whose element
        /// has vanished reports here (a no-op plus a status line, never a throw). Cleared by
        /// the next successful bind or table-selection change (the disarm discipline).
        selectStatus : string option
        /// Spec 0033 (024): the injected materials WRITE seam (STORE_XDUO_0001). Spec 0038
        /// steps 013/015: the strip buttons' Materials and Library windows operate over THIS
        /// store (the bays they replaced are gone).
        materials : MaterialLibrary.MaterialProxy
        /// Spec 0033 (024): the injected samples WRITE seam (STORE_XDUO_0002). Spec 0038
        /// step 015: the strip button's Library window (and the Sample editors it opens)
        /// operates over THIS store — the samples workbench bay it replaced is gone.
        samples : Library.SampleProxy
        /// Spec 0035 (009): the injected category WRITE seam (STORE_XDUO_0003) behind the material
        /// editor's create picker and — spec 0038 step 013 — the Materials window's category facet
        /// and its "Categories…" verb.
        categories : MaterialLibrary.CategoryProxy
        /// Spec 0033 (024): the window launcher seam behind the workbench strip buttons.
        launchers : EditorLaunchers
        /// Spec 0038 (017): the step-005 quick-pick cutoff — the Selector bay's inline strip
        /// renders only while the kind-constrained entry count is BELOW this; at or above it
        /// the bay offers Choose… alone. Defaulted here; the composition root threads the
        /// appsettings value by record update (step 47 owns the composition acceptance).
        quickPickThreshold : WorkbenchSettings.QuickPickThreshold
        /// Spec 0038 (017): how the Choose… verb's Select-state Library window opens (the
        /// step-005 `SelectWindowsModal` switch — ShowDialog owned by this workbench's window,
        /// or a modeless Show). Defaulted here; threaded like the threshold above.
        selectWindowModality : WorkbenchSettings.SelectWindowModality
        /// Spec 0038 Part L (037): which constructor flow this workbench hosts — the ordinary
        /// FORWARD Main scene, or the INVERSE flow `initInverse` seeds (unbound sample, chart
        /// gated on a hint). Defaults to `ForwardConstructor`; `initInverse` sets the inverse mode.
        constructorMode : ConstructorMode
        /// Spec 0038 Part L (037): the measured-data LOAD seam (STORE_XDUO_0006, step 035/036) —
        /// the collection builder's per-experiment file attach reads a picked `DataFilePath`
        /// through this proxy (intensity or ellipsometric per the experiment's detector kind).
        /// `[<ReferenceEquality>]` proxy ⇒ reference-compared, so the model keeps its equality.
        experimentData : ExperimentData.ExperimentDataProxy
        /// Spec 0038 Part I/L (037): the experiment-collection persistence seam (STORE_XDUO_0005,
        /// step 028/029) — named collections of the live experiments save / list / load through it.
        experimentCollections : ExperimentCollectionStore.ExperimentCollectionProxy
        /// Spec 0038 Part L (037): the in-progress collection NAME (the create / rename field). A
        /// raw editable string until `CollectionName.tryCreate` validates it at save/load (the
        /// `pendingEntry` / draft `elementLabel` precedent — UI text stays a string to the boundary).
        collectionName : string
        /// Spec 0038 Part L (037): the names of every collection stored through the proxy — the
        /// loadable list the builder shows, refreshed on save.
        savedCollections : ExperimentCollectionStore.CollectionName list
        /// Spec 0038 Part L (037): the last collection save / load status line (a no-op-plus-status
        /// on a blank name or a load miss, never a throw — the `selectStatus` precedent).
        collectionStatus : string option
        /// Spec 0038 Part L (037): the typed per-experiment measured-data status, keyed by the
        /// experiment id's value — set when a file is attached (load + validate through the proxy).
        experimentDataStatus : Map<int, ExperimentFileStatus>
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
    // Spec 0038 steps 013/015: the Materials AND Library bays are REMOVED from the ribbon — the
    // materials workbench is the single-instance Materials WINDOW (UICOMP_XDUO_0009) and the
    // samples/library workbench the single-instance Library WINDOW (UICOMP_XDUO_0010), each
    // opened by its right-aligned tab-strip-row button. No full-surface bay remains; every bay
    // keeps the shared table canvas below the strip.
    let all = [ rotation; move; add; render; selector; experiments; details ]

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
    open OpticalConstructor.Domain.Lifecycle           // VersionsInUse.empty
    open OpticalConstructor.Domain.MaterialStore       // MaterialProxy.createInMemory (versioned, spec 0038 step 021)
    open OpticalConstructor.Domain.SampleStore         // SampleProxy.createInMemory (versioned, spec 0038 step 022)

    /// Spec 0035 (009): the category store joins the composition LAST — its remove-block consults
    /// the live materials store through `materialsReferencingCategory` (the `samplesReferencing`
    /// precedent), so a category referenced by a seeded material is not silently removable. The
    /// materials store is versioned (spec 0038 step 021) and takes the `VersionsInUse` seam —
    /// empty for the test scenes, which hold no persisted experiments.
    let create () : MaterialLibrary.MaterialProxy * SampleProxy * MaterialLibrary.CategoryProxy =
        let samples = SampleProxy.createInMemory VersionsInUse.empty
        let materials = MaterialLibrary.MaterialProxy.createInMemory (samplesReferencing samples) VersionsInUse.empty
        let categories = MaterialLibrary.CategoryProxy.createInMemory (MaterialLibrary.materialsReferencingCategory materials)
        materials, samples, categories

/// Spec 0038 Part L (037): the default experiment-data + experiment-collection proxies for the
/// parameterless test scenes (`init` / `initMain` / the default `initInverse` caller). The
/// measured-data load seam is the real file-backed adapter (step 036 — the one file-read seam
/// spec-md §0.3c permits; the test scenes never attach a file, so it is never asked to read), and
/// the collection store is the step-029 in-memory `createInMemory`. The composition root injects its
/// own proxies via `initMainWith` / `initInverse`, threaded from `AppContext` (step 47 owns the
/// composition acceptance).
let private defaultExperimentProxies () : ExperimentData.ExperimentDataProxy * ExperimentCollectionStore.ExperimentCollectionProxy =
    OpticalConstructor.Storage.ExperimentDataStore.createFileBacked (),
    ExperimentCollectionStore.ExperimentCollectionProxy.createInMemory ()

/// The Main-screen "Lego" palette — the catalogue kinds the user can add to the scene. Shared by the
/// forward (`initMainWith`) and inverse (`initInverse`) Main scenes; the static test window seeds an
/// empty palette instead (the only behavioural difference between the test and Main scenes).
let private mainPalette : CatalogueKind list =
    [ LinearPolarizer; CircularPolarizer; Sample; Lens; FlatMirror; CurvedMirror; Detector ]

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
    (categories : MaterialLibrary.CategoryProxy)
    (experimentData : ExperimentData.ExperimentDataProxy)
    (experimentCollections : ExperimentCollectionStore.ExperimentCollectionProxy)
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
        activeSelect = None
        selectStatus = None
        materials = materials
        samples = samples
        categories = categories
        launchers = EditorLaunchers.defaults
        quickPickThreshold = QuickPickThreshold.defaultValue
        selectWindowModality = SelectWindowModality.defaultValue
        constructorMode = ForwardConstructor
        experimentData = experimentData
        experimentCollections = experimentCollections
        collectionName = ""
        savedCollections = []
        collectionStatus = None
        experimentDataStatus = Map.empty
    }

/// The STATIC test scene (Spec 0027, task 006 #3): a live table plus three fixed optical elements on
/// the central ray, no add/remove palette. Behaviour is unchanged from before — the palette is empty.
/// The proxies default to the in-memory mocks/stores (the test scene never shows the ribbon bays).
let init () : Model =
    let materials, samples, categories = DefaultStores.create ()
    let experimentData, experimentCollections = defaultExperimentProxies ()
    initWith (Library.createInMemory ()) (Experiments.createInMemory ()) materials samples categories experimentData experimentCollections [ mkElement -0.5 LinearPolarizer; mkElement 0.0 Sample; mkElement 0.5 FlatMirror ] []

/// The DYNAMIC Main scene: the same table/view/selection/rotation logic, seeded with a light source and
/// a detector at the ends of the beam, plus the catalogue palette the user can add elements from (the
/// "Lego constructor"). The proxies — incl. the step-024 material / sample write seams — are injected
/// at the composition root. This is the Main screen — identical scene logic, elements added/removed at
/// runtime.
/// The Main-screen seed elements: a light source and a detector at the central-ray endpoints (the plate
/// edges), with DETERMINISTIC ids so wiring / experiment tests can name them. The forward Main scene uses
/// exactly this; the inverse scene splices an unbound sample between them (`initInverse`).
let private mainSeedElements : TestElement list =
    [ { id = Library.elementId "src"; placement = ElementPlacement.create LightSource RayModel.defaultSourcePoint; zoom = defaultElementZoom }
      { id = Library.elementId "det"; placement = ElementPlacement.create Detector RayModel.defaultDetectorPoint; zoom = defaultElementZoom } ]

let initMainWith
    (library : Library.LibraryProxy)
    (experiments : Experiments.ExperimentProxy)
    (materials : MaterialLibrary.MaterialProxy)
    (samples : Library.SampleProxy)
    (categories : MaterialLibrary.CategoryProxy) : Model =
    // Spec 0038 Part L (037): the measured-data + experiment-collection proxies default to the real
    // file-backed load adapter (step 036) and the step-029 in-memory collection store; the composition
    // root threads its shared app-scope instances by record update (`AppContext.experimentData` /
    // `.experimentCollections`), so two Main windows share one saved-collection store.
    let experimentData, experimentCollections = defaultExperimentProxies ()
    { initWith
        library
        experiments
        materials
        samples
        categories
        experimentData
        experimentCollections
        mainSeedElements
        mainPalette
        with snapChain = true }

/// Spec 0038 Part L (037): the INVERSE Main scene. The SAME dynamic Main workbench (table / view /
/// selection / rotation logic, the add-remove palette, the snap chain, the injected proxies) opened
/// in the inverse-problem flow: seeded with a light source, an UNBOUND SAMPLE (the unknown — step 30
/// draws it dashed), and a detector on the beam. With the sample unbound (no hint) the forward
/// experiment chart has nothing to compute against, so its surface is absent (`experimentChartVisible`);
/// binding a HINT sample through the Library window's Select state (the same `BindValueIdTo` commit
/// path) restores the chart. The user then builds an experiment collection and attaches one measured-
/// data file per experiment. Proxies default like `initMainWith`; the composition root threads its
/// shared app-scope experiment proxies by record update (step 45/47 own the Inverse launcher + acceptance).
let initInverse
    (library : Library.LibraryProxy)
    (experiments : Experiments.ExperimentProxy)
    (materials : MaterialLibrary.MaterialProxy)
    (samples : Library.SampleProxy)
    (categories : MaterialLibrary.CategoryProxy) : Model =
    let experimentData, experimentCollections = defaultExperimentProxies ()
    { initWith
        library
        experiments
        materials
        samples
        categories
        experimentData
        experimentCollections
        [ { id = Library.elementId "src"; placement = ElementPlacement.create LightSource RayModel.defaultSourcePoint; zoom = defaultElementZoom }
          { id = Library.elementId "sample"; placement = ElementPlacement.create Sample { x = 0.0<meter>; y = 0.0<meter> }; zoom = defaultElementZoom }
          { id = Library.elementId "det"; placement = ElementPlacement.create Detector RayModel.defaultDetectorPoint; zoom = defaultElementZoom } ]
        mainPalette
        with snapChain = true; constructorMode = InverseConstructor }

/// The Main scene with the default in-memory mock proxies / stores (the test default; the composition
/// root injects its own proxies via `initMainWith`).
let initMain () : Model =
    let materials, samples, categories = DefaultStores.create ()
    initMainWith (Library.createInMemory ()) (Experiments.createInMemory ()) materials samples categories

// ---------------------------------------------------------------------------
// Scene capture / restore (spec 0038 step 027, IMPLEMENT_CONTRACT STORE_XDUO_0004).
// The pure, Avalonia-free projection between the LIVE workbench `Model` and the Domain
// `Scene.SceneSnapshot` the `SceneProxy` persists — no window, no live handle, so a headless
// test round-trips them. The proxy stores the DATA; these two functions are how the host turns
// its live scene into that data and back.
// ---------------------------------------------------------------------------

/// Project the live workbench model into a pure `Scene.SceneSnapshot`: the placed `elements`
/// (each element's serializable id, its full `placement` — which already carries the
/// `catalogueKind` tag and the `valueId` Library binding — and its draw `zoom`), the table `view`
/// state, and the `snapChain` flag elevated to `Scene.SnapMode`. The inverse is `restoreScene`.
let captureScene (model : Model) : Scene.SceneSnapshot =
    {
        Scene.SceneSnapshot.elements =
            model.elements
            |> List.map (fun e -> { Scene.SceneElement.id = e.id; placement = e.placement; zoom = e.zoom })
        view = model.view
        snap = Scene.SnapMode.create model.snapChain
    }

/// The inverse of `captureScene` — apply a saved `Scene.SceneSnapshot` back onto a live model,
/// replacing the placed `elements`, the table `view` state, and the snap flag (`SnapMode.value`),
/// and leaving every OTHER field (the injected proxies, palette, render config, experiment
/// collection, launchers) untouched. The `selection` resets to the table and `drag` clears: the
/// loaded elements are a FRESH set, so a stale `ElementSelected` index would no longer address the
/// same element. Pure and window-free.
let restoreScene (snapshot : Scene.SceneSnapshot) (model : Model) : Model =
    { model with
        elements =
            snapshot.elements
            |> List.map (fun e -> { TestElement.id = e.id; placement = e.placement; zoom = e.zoom })
        view = snapshot.view
        snapChain = snapshot.snap.value
        selection = TableSelected
        drag = NotPressed }

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
    /// Spec 0038 (018): `AddElement` DELEGATES to `AddElementBoundTo` with the kind's seeded
    /// default (`defaultSeedEntry`), so every new element except a sample lands PRE-BOUND.
    | AddElement of CatalogueKind
    /// Spec 0038 (018): add a catalogue element PRE-BOUND to an explicit seeded Library entry
    /// (`None` = unbound — the sample's inverse hook). The palette buttons dispatch this; the
    /// CPL / CPR pair shares the UNCHANGED CircularPolarizer kind and differs only here.
    | AddElementBoundTo of CatalogueKind * string option
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
    /// Spec 0038 (016): the TARGETED bind a Select-state window's `onSelected` dispatches —
    /// commit the entry id to the `valueId` of the element with THIS serializable id (never
    /// "the current selection": the selection may have moved while the modeless window was
    /// open). A vanished element is a no-op plus the `selectStatus` line, never a throw.
    /// Spec 0038 (017): this is THE one commit site of `placement.valueId` — the quick-pick
    /// strip's confirm and the direct bind above DELEGATE here, so binding through the strip
    /// and through the Choose… Select window land the identical message.
    | BindValueIdTo of Library.ElementId * string
    /// Spec 0038 (017): the Choose… flow's session bookkeeping. `SelectSessionStarted` stores
    /// the staleness handle of the Select-state window the verb just opened (or re-targeted);
    /// `SelectSessionEnded` clears exactly THAT session — reference-keyed — when the window
    /// ends it from its own side (the Close verb / title-bar X fire the session's
    /// `onCancelled`), so a session superseded by a re-target can never clear its successor.
    | SelectSessionStarted of SelectSession
    | SelectSessionEnded of SelectSession
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
    /// Spec 0038 Part L (037) — the experiment-collection BUILDER. `CollectionSetName` edits the
    /// in-progress collection name (create / rename); `CollectionSave` persists the live experiments
    /// under that name through the `ExperimentCollectionProxy` (and refreshes the saved list);
    /// `CollectionLoad` loads a stored collection (by name) back into the live collection.
    | CollectionSetName of string
    | CollectionSave
    | CollectionLoad of string
    /// Spec 0038 Part L (037): attach a picked measured-data file to a collected experiment — record
    /// the `DataFilePath` on the experiment AND load + validate it through the `ExperimentDataProxy`
    /// (intensity or ellipsometric per the experiment's detector kind), storing the typed
    /// parse/validation status. The file picker (IO) yields the `DataFilePath` at the edge; a headless
    /// test dispatches this directly over a mock proxy.
    | AttachDataFileTo of Experiments.ExperimentId * Experiments.DataFilePath
    /// Spec 0038 (013) — the ribbon tab-strip row's right-aligned "Materials…" button: open the
    /// SINGLE-INSTANCE Materials window (UICOMP_XDUO_0009) over the app-scope material + category
    /// stores through the launcher seam (a second click ACTIVATES the live window — the shared
    /// `WindowRegistry` under `MaterialsWindowKey`). The Materials BAY and its `Mat…` verb
    /// vocabulary left the ribbon with this step; the window carries the verbs now.
    | OpenMaterialsWindow
    /// Spec 0038 (015) — the ribbon tab-strip row's right-aligned "Library…" button: open the
    /// SINGLE-INSTANCE Library window (UICOMP_XDUO_0010) over the read-only Library seam plus
    /// the app-scope samples + materials stores through the launcher seam (a second click
    /// ACTIVATES the live window — the shared `WindowRegistry` under `LibraryWindowKey`). The
    /// LIBRARY (samples workbench) BAY and its `Smp…` verb vocabulary left the ribbon with this
    /// step; the window carries the verbs now.
    | OpenLibraryWindow

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

/// Spec 0038 (018): the stable ids of the seeded Library entries the pre-binding maps to
/// (`Library.seedEntries` — the Domain seeds; entry ids are strings by the spec-0033 string-id
/// decision). Centralized here so the add path, the palette buttons, and the tests never repeat
/// the literals.
[<RequireQualifiedAccess>]
module SeedEntryIds =
    [<Literal>]
    let source600 = "src-600"
    [<Literal>]
    let detectorIntensity = "det-intensity"
    [<Literal>]
    let polarizerLp = "pol-lp"
    [<Literal>]
    let polarizerCpLeft = "pol-cp-left"
    [<Literal>]
    let polarizerCpRight = "pol-cp-right"

/// Spec 0038 (018) pre-binding (spec Part G, operator Q7/Q28): the seeded Library entry a NEW
/// table element of this kind is created PRE-BOUND to. A sample stays UNBOUND — the inverse-flow
/// hook — and the lens / mirror kinds have no Library entries to bind (not bindable). The
/// circular-polarizer kind defaults to the LEFT seed (the seed order); the palette's CPR button
/// carries its explicit `pol-cp-right` pre-bind through `AddElementBoundTo`.
let defaultSeedEntry (kind : CatalogueKind) : string option =
    match kind with
    | LightSource -> Some SeedEntryIds.source600
    | Detector -> Some SeedEntryIds.detectorIntensity
    | LinearPolarizer -> Some SeedEntryIds.polarizerLp
    | CircularPolarizer -> Some SeedEntryIds.polarizerCpLeft
    | Sample -> None
    | Lens | FlatMirror | CurvedMirror -> None

/// Spec 0038 (016) staleness: cancel and CLOSE the open Select-state window session, if any
/// (closing the window fires the session's `onCancelled` through its own dismissal hook).
/// Invoked wherever the table selection changes or the session's target can disappear — the
/// pending-bind-clears precedent (`PointerUp` below).
let private cancelActiveSelect (m : Model) : Model =
    match m.activeSelect with
    | Some session ->
        session.cancelAndClose ()
        { m with activeSelect = None }
    | None -> m

/// Append a catalogue element to the scene and select it. New elements are spread along the beam so
/// they do not land exactly on top of one another; the user then rotates / configures the selection.
/// Selecting the new element IS a table-selection change, so an open Select session cancels (016).
/// Spec 0038 (018): the new element lands PRE-BOUND to `prebind` (the kind's seeded default, or
/// the palette button's explicit entry) — `None` keeps it unbound (the sample's inverse hook).
let private addElement (kind : CatalogueKind) (prebind : string option) (m : Model) : Model =
    let middleCount =
        m.elements
        |> List.filter (fun e -> e.placement.catalogueKind <> LightSource && e.placement.catalogueKind <> Detector)
        |> List.length
    let x = -0.3 + 0.2 * float middleCount
    let placement = { ElementPlacement.create kind { x = x * 1.0<meter>; y = 0.0<meter> } with valueId = prebind }
    let e = { id = freshId (); placement = placement; zoom = defaultElementZoom }
    let elements' = m.elements @ [ e ]
    { cancelActiveSelect m with elements = elements'; selection = ElementSelected (List.length elements' - 1) }

/// Remove the currently-selected element (inert when the table or nothing is selected). Selection
/// drops to nothing so the bar disables until the user picks another object. The removed element
/// may be an open Select session's target — the target's disappearance cancels and closes the
/// session's window (spec 0038 step 016 staleness).
let private removeSelected (m : Model) : Model =
    match m.selection with
    | ElementSelected i when i >= 0 && i < List.length m.elements ->
        let elements' = m.elements |> List.mapi (fun j e -> j, e) |> List.filter (fun (j, _) -> j <> i) |> List.map snd
        { cancelActiveSelect m with elements = elements'; selection = NothingSelected }
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

/// Spec 0038 Part I (step 025): resolve one present element's binding into an experiment descriptor's
/// `ElementBinding`. A bound Sample resolves through the Library to its `SampleId` and pins version one
/// (`SampleVersionId.firstOf` — every sample lives at v1 at this in-memory step), so the captured setup
/// references the sample BY VERSION (the `VersionsInUse` seam collects it); a bound protected preset
/// (source / detector / polarizer) binds by its entry id; an unbound element is `Unbound`.
let private descriptorBinding (model : Model) (kind : CatalogueKind) (valueIdOpt : string option) : Experiments.ElementBinding =
    match valueIdOpt with
    | None -> Experiments.Unbound
    | Some vid ->
        match kind with
        | Sample ->
            match model.library.tryGetEntry vid with
            | Ok (Some (Library.SampleItem s)) ->
                Experiments.BoundByVersion (Lifecycle.SampleVersionRef (Library.SampleVersionId.firstOf s.id))
            | Ok (Some _) | Ok None | Error _ -> Experiments.BoundByEntryId vid
        | LightSource | LinearPolarizer | CircularPolarizer | Lens | FlatMirror | CurvedMirror | Detector ->
            Experiments.BoundByEntryId vid

/// Spec 0038 Part I (step 025): capture the live scene as an experiment's ordered `setup` — one
/// `ElementDescriptor` per present element (in scene order), carrying its id, human label, catalogue kind,
/// placement/orientation summary and versioned/preset binding. The detector element is part of the chain
/// (its kind fixes the expected data-file shape). Built on commit; the descriptor snapshot survives the
/// element's later removal.
let private buildExperimentSetup (model : Model) : Experiments.ElementDescriptor list =
    model.elements
    |> List.map (fun e ->
        {
            Experiments.ElementDescriptor.elementId = e.id
            label = experimentElementLabel model e.id
            kind = e.placement.catalogueKind
            placement = Experiments.PlacementSummary.ofPlacement e.placement
            binding = descriptorBinding model e.placement.catalogueKind e.placement.valueId
        })

/// Spec 0038 Part L (037): the point count of a validated measured series — the collection builder's
/// per-file "N point(s), schema-valid" status.
let private measuredSeriesPointCount (series : MeasuredData.MeasuredSeries) : int =
    match series with
    | MeasuredData.IntensityData s -> List.length s.points
    | MeasuredData.EllipsometricData s -> List.length s.points

/// Spec 0038 Part L (037): classify a measured-data load error as a PARSE failure (a malformed /
/// empty file) or a VALIDATION failure (a range / units mismatch against the experiment), carrying
/// the domain error's diagnostic `reason` into the typed inline status.
let private classifyDataError (error : MeasuredData.ExperimentDataError) : ExperimentFileStatus =
    match error with
    | MeasuredData.MalformedDataFile reason
    | MeasuredData.EmptyDataFile reason -> DataFileParseError reason
    | MeasuredData.DataRangeMismatch reason
    | MeasuredData.DataUnitsMismatch reason -> DataFileValidationError reason

// `rec` (spec 0038 step 017): the BindValueId / ConfirmBindValueId arms DELEGATE to the one
// targeted commit arm (`BindValueIdTo`) instead of committing `placement.valueId` themselves.
let rec update (msg : Msg) (model : Model) : Model =
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
    | AddElement kind ->
        // Spec 0038 (018): the plain add CONVERGES on the pre-binding arm below with the kind's
        // seeded default — every new table element except a sample lands pre-bound.
        update (AddElementBoundTo (kind, defaultSeedEntry kind)) model
    | AddElementBoundTo (kind, prebind) -> addElement kind prebind model
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
        // Spec 0038 (017): the direct bind CONVERGES on the one targeted commit arm below —
        // resolve the selected element's serializable id and delegate to `BindValueIdTo`.
        match model.selection with
        | ElementSelected i when i >= 0 && i < List.length model.elements ->
            update (BindValueIdTo ((List.item i model.elements).id, entryId)) model
        | ElementSelected _ | TableSelected | NothingSelected -> model
    | RequestBindValueId entryId ->
        // Spec 0027 (026): select the entry as PENDING (its full description is shown) without binding yet —
        // inert unless an element is selected (the Selector bay is only enabled for an element).
        match model.selection with
        | ElementSelected _ -> { model with pendingEntry = Some entryId }
        | TableSelected | NothingSelected -> model
    | ConfirmBindValueId ->
        // Spec 0038 (017): the quick-pick strip's confirm CONVERGES on the SAME targeted bind
        // message the Select window's onSelected dispatches — `BindValueIdTo` is the one
        // commit site of placement.valueId. The strip's Confirm button is a stateless
        // `unit -> unit` control handler (it cannot carry the ids without a stale-closure
        // hazard under FuncUI's keyed re-subscription), so THIS arm resolves the pending
        // entry + selected element over the CURRENT model and delegates.
        match model.pendingEntry, model.selection with
        | Some entryId, ElementSelected i when i >= 0 && i < List.length model.elements ->
            update (BindValueIdTo ((List.item i model.elements).id, entryId)) model
        | _ -> { model with pendingEntry = None }
    | CancelBindValueId -> { model with pendingEntry = None }
    | BindValueIdTo (elementId, entryId) ->
        // Spec 0038 (016): the TARGETED bind of a Select-state window's onSelected — resolve
        // the element BY ITS ID at return time (the modeless window may outlive a selection
        // change) and commit its valueId. A vanished element is a no-op plus the status line —
        // never a throw (the staleness rules normally close the window first; this is the
        // belt-and-braces path for the modeless race). Spec 0038 (017): a committed bind ENDS
        // any live Choose… session — on the quick-pick path the Select window may still be
        // open, so cancel AND CLOSE it; on the window path the window already closed itself
        // and the extra Close() is Avalonia's safe no-op.
        let m = cancelActiveSelect model
        match m.elements |> List.tryFindIndex (fun e -> e.id = elementId) with
        | Some i ->
            mapElement i (fun e -> { e with placement = { e.placement with valueId = Some entryId } })
                { m with pendingEntry = None; selectStatus = None }
        | None ->
            { m with selectStatus = Some "The chosen entry was not bound — its target element is no longer on the table." }
    | SelectSessionStarted session ->
        // Spec 0038 (017): the Choose… verb just opened (or re-targeted) the Select-state
        // Library window — hold its staleness handle. Any prior session already ended through
        // its own path (a selection change closed it; a re-target cancelled it in the window).
        { model with activeSelect = Some session }
    | SelectSessionEnded session ->
        // Spec 0038 (017): the window ended the session from ITS side (the Close verb, the
        // title-bar X, or a re-target superseding it). Reference-keyed: a session that was
        // already replaced or cleared never clears its successor's handle.
        match model.activeSelect with
        | Some active when obj.ReferenceEquals (active, session) -> { model with activeSelect = None }
        | Some _ | None -> model
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
    | ExpCommit ->
        // Spec 0038 Part I (step 025): capture the live scene as the experiment's ordered setup and commit.
        { model with experimentCollection = Experiments.commit (buildExperimentSetup model) model.experimentCollection }
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
    | CollectionSetName name ->
        // Spec 0038 Part L (037): edit the in-progress collection name (create / rename). Raw text
        // until `CollectionName.tryCreate` validates it at save/load.
        { model with collectionName = name }
    | CollectionSave ->
        // Spec 0038 Part L (037): persist the live experiments under the named collection through the
        // `ExperimentCollectionProxy`, then refresh the loadable saved list. A blank name is rejected
        // at the store boundary (`CollectionName.tryCreate`) as a typed status, never a throw.
        match CollectionName.tryCreate model.collectionName with
        | Ok name ->
            let snapshot : ExperimentCollectionStore.ExperimentCollectionSnapshot =
                { name = name; experiments = model.experimentCollection.experiments }
            match model.experimentCollections.saveCollection snapshot with
            | Ok () ->
                let saved =
                    match model.experimentCollections.listCollections () with
                    | Ok names -> names
                    | Error _ -> model.savedCollections
                { model with
                    savedCollections = saved
                    collectionStatus = Some $"Saved collection '%s{name.value}' (%d{List.length model.experimentCollection.experiments} experiment(s))." }
            | Error (ExperimentCollectionStore.InvalidCollection reason) ->
                { model with collectionStatus = Some $"Save failed — %s{reason}" }
        | Error (ExperimentCollectionStore.InvalidCollection reason) ->
            { model with collectionStatus = Some $"Save failed — %s{reason}" }
    | CollectionLoad nameStr ->
        // Spec 0038 Part L (037): load a stored collection back into the live collection through the
        // proxy. The loaded experiments carry their captured setups AND their `dataFileOpt`
        // attachments; `nextId` advances past the loaded ids so a subsequent add never collides. The
        // per-experiment statuses are session-derived, so they clear on load (re-attach to re-validate).
        match CollectionName.tryCreate nameStr with
        | Ok name ->
            match model.experimentCollections.tryLoadCollection name with
            | Ok (Some snapshot) ->
                let experiments = snapshot.experiments
                let nextId = (experiments |> List.map (fun e -> e.id.value) |> List.fold max 0) + 1
                { model with
                    experimentCollection =
                        { model.experimentCollection with
                            experiments = experiments
                            nextId = nextId
                            draft = Experiments.ExperimentDraft.empty }
                    collectionName = name.value
                    experimentDataStatus = Map.empty
                    collectionStatus = Some $"Loaded collection '%s{name.value}' (%d{List.length experiments} experiment(s))." }
            | Ok None ->
                { model with collectionStatus = Some $"No collection named '%s{nameStr}' is stored." }
            | Error (ExperimentCollectionStore.InvalidCollection reason) ->
                { model with collectionStatus = Some $"Load failed — %s{reason}" }
        | Error (ExperimentCollectionStore.InvalidCollection reason) ->
            { model with collectionStatus = Some $"Load failed — %s{reason}" }
    | AttachDataFileTo (experimentId, path) ->
        // Spec 0038 Part L (037): attach the picked file to the experiment, then load + validate it
        // through the `ExperimentDataProxy` (in the shape its detector kind fixes) and record the
        // typed per-experiment status. The attachment is recorded regardless of the load outcome (the
        // file is the user's choice); the status reflects the parse / validation result.
        let collection = Experiments.attachDataFile experimentId path model.experimentCollection
        match collection.experiments |> List.tryFind (fun e -> e.id = experimentId) with
        | Some experiment ->
            let status =
                match ExperimentDataLoad.loadAndValidate model.library model.experimentData experiment path with
                | Ok series -> DataFileValidated (measuredSeriesPointCount series)
                | Error error -> classifyDataError error
            { model with
                experimentCollection = collection
                experimentDataStatus = model.experimentDataStatus |> Map.add experimentId.value status }
        | None -> model
    | OpenMaterialsWindow ->
        // Spec 0038 step 013: a pure launch of the single-instance Materials window
        // (UICOMP_XDUO_0009) over the app-scope material + category stores — the launcher seam's
        // shared registry keys it under `MaterialsWindowKey`, so a second click ACTIVATES the
        // live window. No model change: the window runs its own MVU loop over the same stores.
        model.launchers.openMaterialsWindow model.materials model.categories
        model
    | OpenLibraryWindow ->
        // Spec 0038 step 015: a pure launch of the single-instance Library window
        // (UICOMP_XDUO_0010) over the read-only Library seam plus the app-scope samples +
        // materials stores — the launcher seam's shared registry keys it under
        // `LibraryWindowKey`, so a second click ACTIVATES the live window. No model change: the
        // window runs its own MVU loop over the same stores (the retired samples-workbench bay's
        // query/selection/confirm state lives in the window's own model now).
        model.launchers.openLibraryWindow model.library model.samples model.materials model.categories
        model
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
        // pending choice never carries across elements. Spec 0038 (016): the same change also
        // CANCELS AND CLOSES the open Select-state window (the staleness rule extends the
        // pending-bind-clears precedent) and clears the staleness status line.
        if selection = model.selection then
            { model with selection = selection; drag = NotPressed }
        else
            { cancelActiveSelect model with selection = selection; drag = NotPressed; pendingEntry = None; selectStatus = None }
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

/// Spec 0038 (018): one add-palette BUTTON — its stable code (the `PaletteAdd_<code>` automation-id
/// suffix), its label, the catalogue kind it adds, and the seeded Library entry the new element
/// lands PRE-BOUND to (`None` = unbound: the sample's inverse hook, and the not-bindable lens /
/// mirror kinds).
type PaletteButton =
    {
        code : string
        label : string
        kind : CatalogueKind
        prebind : string option
    }

/// Spec 0038 (018): the palette buttons the model's catalogue palette EXPANDS to. The
/// circular-polarizer kind expands to the CPL / CPR pair, so the palette offers THREE polarizer
/// buttons — LP / CPL / CPR — over the UNCHANGED `CatalogueKind`s (LP adds a LinearPolarizer,
/// CPL and CPR both add a CircularPolarizer; only the label and the pre-bound seed entry differ).
/// Every other kind keeps its one `kindCode`/`kindName` button with the kind's seeded default.
/// Public so the mapping is provable without a window.
let paletteButtons (model : Model) : PaletteButton list =
    model.palette
    |> List.collect (fun k ->
        match k with
        | CircularPolarizer ->
            [ { code = "CPL"; label = $"%s{(kindName CircularPolarizer)} (L)"; kind = CircularPolarizer; prebind = Some SeedEntryIds.polarizerCpLeft }
              { code = "CPR"; label = $"%s{(kindName CircularPolarizer)} (R)"; kind = CircularPolarizer; prebind = Some SeedEntryIds.polarizerCpRight } ]
        | LightSource | LinearPolarizer | Sample | Lens | FlatMirror | CurvedMirror | Detector ->
            [ { code = kindCode k; label = kindName k; kind = k; prebind = defaultSeedEntry k } ])

/// The add / remove "Lego" palette row — shown ONLY when the scene has a non-empty palette (the Main
/// screen). The static test windows pass an empty palette, so this row is absent and their UI is
/// unchanged. It is the shared `ElementPaletteControls` bar, styled to match the rotation bar.
let private paletteState (model : Model) : ElementPaletteControls.State =
    {
        addItems = paletteButtons model |> List.map (fun b -> { ElementPaletteControls.AddItem.id = b.code; label = b.label })
        canRemove = (match model.selection with ElementSelected _ -> true | _ -> false)
    }

let private paletteHandlers (model : Model) (dispatch : Msg -> unit) : ElementPaletteControls.Handlers =
    {
        add = fun id -> paletteButtons model |> List.tryFind (fun b -> b.code = id) |> Option.iter (fun b -> dispatch (AddElementBoundTo (b.kind, b.prebind)))
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
        // R3-tilted mirror is drawn off the plane); `drawnPlacement` carries the orientation. The binding
        // state is derived from that placement's `valueId`, so an unbound element draws dashed-and-ghosted.
        let placement = drawnPlacement model i
        renderer.draw project (model.selection = ElementSelected i)
            { placement = placement; centre = nodes.[i].position; zoom = e.zoom; opticalSign = Catalogue.opticalSign e.placement.catalogueKind; bindingState = ElementRenderer.bindingStateOf placement })
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

/// Spec 0038 (017): what the Selector bay offers for the CURRENT selection — the inline
/// quick-pick strip beside the Choose… verb (the kind-constrained entry count sits BELOW the
/// step-005 threshold), the Choose… verb alone (at or above it — the full Select-state Library
/// window is the picking surface there), or nothing bindable (table / nothing selected — the
/// bay keeps its disabled prompt). A named three-case DU, never a bool pair.
type SelectorOffer =
    | QuickPickAndChoose
    | ChooseAlone
    | NoSelectorOffer

/// The bay's threshold decision (public so the gating is provable without a window). The count
/// is the kind-constrained entry set the strip's rows are built from (`entriesForKind` through
/// the read-only Library seam — the same set `libraryState` flattens), compared against the
/// model's `quickPickThreshold`: strictly below → the strip renders; at or above → Choose… alone.
let selectorOffer (model : Model) : SelectorOffer =
    match model.selection with
    | ElementSelected i when i >= 0 && i < List.length model.elements ->
        let kind = (List.item i model.elements).placement.catalogueKind
        if Set.count (allowedEntryIds model kind) < model.quickPickThreshold.value then QuickPickAndChoose
        else ChooseAlone
    | ElementSelected _ | TableSelected | NothingSelected -> NoSelectorOffer

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

/// The input Stokes vector from the FIRST polarizer element bound to a polarizer preset (its R1 is the
/// polarizer's orientation), synthesized through the behaviour seam (spec 0038 Part F —
/// `Propagation.behaviorInputStokes` routes `ComputedIdeal` through the same `inputStokes` as before).
/// No input polarizer → unpolarized natural light (spec R1).
let private runInputStokes (model : Model) : StokesVector =
    model.elements
    |> List.tryPick (fun e ->
        match boundEntry model e with
        | Some (Library.PolarizerItem p) -> Some (Propagation.behaviorInputStokes p.behavior e.placement.r1)
        | _ -> None)
    |> Option.defaultValue Propagation.unpolarizedStokes

// ---------------------------------------------------------------------------
// Spec 0038 (031): the OUT-OF-BAND dispersion diagnostic overlay. Independent of the step-30
// bound/unbound cue: for every element the materials reachable through its binding are checked
// against the wavelengths the scene requests (the Experiments-bay draft's wavelength sweep, or the
// fixed source λ). A material whose DEFINED dispersion segments do not cover the request raises a
// small warning badge with a hover tooltip naming the offending material(s) and both ranges; the
// same text lands in the Details bay. The diagnostic itself is the pure Domain
// `OutOfBandDiagnostic`; the host only resolves reachability and renders the badge.
// ---------------------------------------------------------------------------

/// Every pinned material VERSION a sample's structure references (its film layers — a repeat cell
/// counted once — its substrate plate, and its lower half-space), deduplicated.
let private sampleMaterialVersions (s : Library.Sample) : MaterialLibrary.MaterialVersionId list =
    let filmVersions =
        s.structure.films
        |> List.collect (fun item ->
            match item with
            | Library.SingleLayer l -> [ l.materialId ]
            | Library.Repeated g -> g.cell |> List.map (fun l -> l.materialId))
    let substrateVersions = s.structure.substrate |> Option.toList |> List.map (fun l -> l.materialId)
    let lowerVersions = s.structure.lower |> Option.toList
    filmVersions @ substrateVersions @ lowerVersions |> List.distinct

/// The materials reachable through an element's binding (spec 0038 step 031): a bound sample's layer
/// material VERSIONS resolved to their entries through the versioned material store; a source /
/// detector / polarizer / unbound element reaches none. (No table element binds a material DIRECTLY
/// today — only samples carry materials — so the sample path is the sole reachability route.)
let private reachableMaterialEntries (model : Model) (e : TestElement) : MaterialLibrary.MaterialEntry list =
    match boundEntry model e with
    | Some (Library.SampleItem s) ->
        sampleMaterialVersions s
        |> List.choose (fun mvid ->
            match model.materials.resolveVersion mvid with
            | Ok (Some entry) -> Some entry
            | Ok None | Error _ -> None)
    | Some _ | None -> []

/// The wavelengths the scene currently requests (spec 0038 step 031): the Experiments-bay draft's
/// wavelength sweep (its nm range) when one is being configured, otherwise the fixed source λ (the
/// existing `runWaveLength`, which defaults to 600 nm for an absent source).
let private sceneRequestedWavelengths (model : Model) : OutOfBandDiagnostic.RequestedWavelengths =
    OutOfBandDiagnostic.requestedWavelengthsFor
        (runWaveLength model)
        model.experimentCollection.draft.variable
        model.experimentCollection.draft.range

/// The out-of-band warning text for element `e`, if any (spec 0038 step 031). Public so the Details
/// bay and the badge overlay share ONE derivation and a headless test can drive it directly.
let outOfBandWarningFor (model : Model) (e : TestElement) : string option =
    OutOfBandDiagnostic.checkMaterialsOutOfBand (reachableMaterialEntries model e) (sceneRequestedWavelengths model)
    |> OutOfBandDiagnostic.coverageWarning

/// Attach an AutomationId to a badge TextBlock through FuncUI's attr builder (the
/// `workbenchAutomationId` precedent — a scene overlay has variable membership, so it carries an
/// AutomationId, never a write-once `Name`).
let private badgeAutomationId (autoId : string) : IAttr<TextBlock> =
    AttrBuilder<TextBlock>.CreateProperty<string>(AutomationProperties.AutomationIdProperty, autoId, ValueNone)

/// Attach a hover tooltip (Avalonia's attached `ToolTip.Tip`) to a badge TextBlock — a NEW pattern
/// (no prior tooltip in the app), set the same attr-builder way as the AutomationId above.
let private badgeToolTip (text : string) : IAttr<TextBlock> =
    AttrBuilder<TextBlock>.CreateProperty<obj>(ToolTip.TipProperty, box text, ValueNone)

/// The out-of-band warning badge for element index `i`, drawn at its projected centre `c` (spec 0038
/// step 031): a small "⚠" glyph with a hover tooltip = `text` (the offending material(s) and both
/// ranges) and a per-index AutomationId so a headless test can address it.
let outOfBandBadge (i : int) (c : ScreenPoint) (text : string) : IView =
    TextBlock.create [
        TextBlock.left (c.sx - 18.0)
        TextBlock.top (c.sy - 26.0)
        TextBlock.text "⚠"
        TextBlock.fontWeight FontWeight.Bold
        TextBlock.foreground (brush (color 200 60 40))
        badgeAutomationId (UiIds.outOfBandBadge i)
        badgeToolTip text
    ] :> IView

/// The out-of-band warning badges for every flagged element in the scene (spec 0038 step 031). Public
/// so a headless test renders them without mounting the whole workbench; appended to the main canvas.
let outOfBandBadges (model : Model) : IView list =
    let project = projectPt model.view
    let centres = snappedCentres model
    model.elements
    |> List.mapi (fun i e ->
        match outOfBandWarningFor model e with
        | Some text ->
            Map.tryFind i centres
            |> Option.map (fun centre -> outOfBandBadge i (project centre) text)
            |> Option.toList
        | None -> [])
    |> List.concat

/// The ideal polarizer kind of a bound polarizer entry, when its behaviour is `ComputedIdeal` (spec 0038
/// Part F). The rotate-R1 / sweep runners below drive the kind-typed pipeline sweeps, so a
/// `ConstantMueller` polarizer — no seed and no editor can produce one yet — is skipped exactly like a
/// non-polarizer element until those runners go behaviour-typed (Part G reworks this resolution anyway).
let private idealKindOf (p : Library.PolarizerPreset) : Library.PolarizerKind option =
    match p.behavior with
    | Library.ComputedIdeal kind -> Some kind
    | Library.ConstantMueller _ -> None

/// Spec 0038 (018): the rotate-R1 analyzer resolution, as a named two-case DU. The old silent
/// `Option.defaultValue Library.IdealLinear` tail is RETIRED: a scene holding NO bound polarizer
/// resolves to the typed `NoAnalyzerPresent` status the experiment surface reports — never a
/// silently assumed ideal linear analyzer.
type AnalyzerResolution =
    | ResolvedAnalyzer of Library.PolarizerKind
    | NoAnalyzerPresent

/// Spec 0038 (018): what the experiment surface shows for a rotate-R1 intensity run that resolved
/// `NoAnalyzerPresent` (the chart title + status text; no series are synthesized from a fallback).
[<Literal>]
let noAnalyzerTitle = "No analyzer present"

[<Literal>]
let noAnalyzerStatus =
    "Cannot run the rotating-analyzer experiment: no element in the scene is bound to a polarizer entry. Add a polarizer (LP / CPL / CPR) or bind one through the Selector bay."

/// The analyzer's polarizer kind for the rotate-R1 experiment: the VARIED element if it is bound to a
/// polarizer preset, else the last polarizer bound in the scene, else the typed `NoAnalyzerPresent`
/// status (spec 0038 step 018 — the silent IdealLinear fallback is retired; the bound entry's kind
/// is what runs). Public so the no-silent-fallback discipline is provable without a window.
let runAnalyzerKind (model : Model) (varied : Library.ElementId option) : AnalyzerResolution =
    let variedKind =
        match varied with
        | Some chosen ->
            model.elements
            |> List.tryFind (fun e -> e.id = chosen)
            |> Option.bind (fun e ->
                match boundEntry model e with
                | Some (Library.PolarizerItem p) -> idealKindOf p
                | _ -> None)
        | None -> None
    let sceneKind =
        model.elements
        |> List.choose (fun e ->
            match boundEntry model e with
            | Some (Library.PolarizerItem p) -> idealKindOf p
            | _ -> None)
        |> List.tryLast
    match variedKind, sceneKind with
    | Some k, _ -> ResolvedAnalyzer k
    | None, Some k -> ResolvedAnalyzer k
    | None, None -> NoAnalyzerPresent

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
    // Resolution flows through the LIVE versioned material store's by-version resolve (spec 0038
    // step 022): each layer's pinned `MaterialVersionId` resolves to the exact version it was built
    // against, so a later mint never rewrites a bound sample's physics.
    runSampleOpt model |> Option.map (Propagation.resolveSampleMaterials model.materials)

/// The human-readable message a failed sample-material resolution surfaces on the chart. Every error
/// case carries a diagnostic `reason` (spec 0033 steps 002/003); only `UnknownMaterialId` can actually
/// arise from resolution — the write-seam cases (step 003) render a generic library message.
let private materialErrorText (err : MaterialLibrary.MaterialError) : string =
    match err with
    | MaterialLibrary.UnknownMaterialId reason ->
        $"Cannot run: the sample references an unknown material (%s{reason})."
    | MaterialLibrary.DuplicateMaterialId reason
    | MaterialLibrary.MaterialStillReferenced reason
    | MaterialLibrary.MaterialVersionInUse reason
    | MaterialLibrary.InvalidMaterial reason ->
        $"Cannot run: material library error (%s{reason})."

/// The analyzer (its polarizer kind + orientation R1) for the sweeps: the FIRST polarizer element bound to
/// a `ComputedIdeal` polarizer preset, with its live R1 as the orientation. `None` when no analyzer is
/// present (the sweep then reads the raw sample output, spec R1). The kind-typed sweep builders drive
/// this, so a `ConstantMueller` polarizer is skipped like a non-polarizer (see `idealKindOf`).
let private runAnalyzerOpt (model : Model) : (Library.PolarizerKind * Angle) option =
    model.elements
    |> List.tryPick (fun e ->
        match boundEntry model e with
        | Some (Library.PolarizerItem p) -> idealKindOf p |> Option.map (fun k -> k, e.placement.r1)
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
                // Spec 0038 (018): the analyzer is READ from the bound polarizer entry; a scene
                // with none reports the typed 'no analyzer present' status — the old silent
                // IdealLinear fallback is retired, so no series are synthesized from a fallback.
                match runAnalyzerKind model (Some chosen) with
                | NoAnalyzerPresent ->
                    { ExperimentChart.empty with title = noAnalyzerTitle; description = noAnalyzerStatus }
                | ResolvedAnalyzer analyzerKind ->
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

/// Spec 0038 Part L (037): whether the forward experiment chart surface is present. The FORWARD
/// constructor always shows it (an unbound sample there yields an empty chart, as before). The
/// INVERSE constructor's forward preview needs a sample to compute against: with NO hint (the seeded
/// sample unbound) the chart surface is ABSENT; binding a HINT sample (any Sample element bound)
/// restores it and the chart behaves as today. Public so the host's gate is unit-testable.
let experimentChartVisible (model : Model) : bool =
    match model.constructorMode with
    | ForwardConstructor -> true
    | InverseConstructor ->
        model.elements
        |> List.exists (fun e ->
            e.placement.catalogueKind = Sample &&
            (match e.placement.valueId with Some _ -> true | None -> false))

/// The live DRAFT's chart (the inline bay preview + the draft "Open chart" action). Public so the host's
/// branches are unit-testable without a window. Spec 0038 Part L (037): EMPTY when the inverse chart
/// surface is gated off (inverse mode, no hint sample) — the surface is absent until a hint is bound.
let experimentResult (model : Model) : ExperimentChart.ExperimentChart =
    if not (experimentChartVisible model) then ExperimentChart.empty
    else
        let draft = model.experimentCollection.draft
        match draft.elementId, draft.variable with
        | Some chosen, Some variable -> chartForParams model chosen variable draft.measurement draft.range draft.elementLabel
        | _ -> ExperimentChart.empty

/// The chart of a specific COLLECTED experiment — the per-row "View" action. Spec 0038 Part I (step 025):
/// the varied element is one entry of the experiment's captured setup; a dark-line experiment (nothing
/// varied) has no chart. Spec 0038 Part L (037): also EMPTY when the inverse chart surface is gated off.
let chartForExperiment (model : Model) (exp : Experiments.Experiment) : ExperimentChart.ExperimentChart =
    if not (experimentChartVisible model) then ExperimentChart.empty
    else
        match exp.varied with
        | Some v -> chartForParams model v.elementId v.variable exp.measurement exp.range exp.variedLabel
        | None -> ExperimentChart.empty

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
    if not (experimentChartVisible model) then None
    else
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
// helpers, kept separate from `Schematic.fs` whose band vocabulary differs) — collapsing runs of identical-thickness
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
            | Library.SingleLayer l -> [ l.materialId.materialId, layerBandThickness l, 1 ]
            | Library.Repeated g -> g.cell |> List.map (fun l -> l.materialId.materialId, layerBandThickness l, g.count))
    let substrateBands =
        match sample.structure.substrate with
        | Some l -> [ l.materialId.materialId, layerBandThickness l, 1 ]
        | None -> []
    let lowerBands =
        match sample.structure.lower with
        | Some mvid -> [ mvid.materialId, SemiInfinite, 1 ]
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
        let baseState =
            match boundEntry model e with
            | Some (Library.SampleItem s) -> sampleBandsState s
            | Some entry ->
                ({ title = $"%s{entry.displayName} — %s{entry.fullDescription}"; bands = [] } : LayerBandsControls.State)
            | None ->
                ({ title = "No Library entry bound — pick one in the Selector bay to see its details."; bands = [] } : LayerBandsControls.State)
        // Spec 0038 (031): surface the SAME out-of-band warning text the badge tooltip shows, so the
        // Details bay states the offending material(s) and both ranges in prose besides the schematic cue.
        match outOfBandWarningFor model e with
        | Some warning -> { baseState with title = $"%s{baseState.title}\n⚠ %s{warning}" }
        | None -> baseState
    | TableSelected | NothingSelected ->
        ({ title = "Select an element to see what it is."; bands = [] } : LayerBandsControls.State)

// ---------------------------------------------------------------------------
// Spec 0038 steps 013/015 — the workbench strip buttons. The MATERIALS bay moved into the
// single-instance Materials WINDOW (step 013, `MaterialsWindowView`) and the LIBRARY (samples)
// workbench bay into the single-instance Library WINDOW (step 015, `LibraryWindowView`); the
// ribbon tab-strip row's right-aligned "Library…" / "Materials…" buttons below are what remain
// of them here. (`SampleLibraryControls` stays in the Controls project with its own tests until
// a later sweep retires it — the `MaterialsControls` precedent.)
// ---------------------------------------------------------------------------

/// Stable intent-named ids for the workbench surfaces THIS host adds around the shared controls
/// (the Materials window's ids live in `MaterialsWindowView.UiIds`; the Library window's in
/// `LibraryWindowView.UiIds`).
[<RequireQualifiedAccess>]
module WorkbenchIds =
    /// Spec 0038 (013): the ribbon tab-strip row's right-aligned button — opens the
    /// single-instance Materials window through the launcher seam.
    [<Literal>]
    let openMaterialsButton = "OpenMaterialsWindowButton"
    /// Spec 0038 (015): the ribbon tab-strip row's right-aligned button — opens the
    /// single-instance Library window through the launcher seam.
    [<Literal>]
    let openLibraryButton = "OpenLibraryWindowButton"
    /// Spec 0038 (016): the staleness status line under the readout — a targeted Select
    /// return whose element has vanished reports here (a no-op plus a status line).
    [<Literal>]
    let selectStatus = "WorkbenchSelectStatus"
    /// Spec 0038 (017): the Selector bay's Choose… verb — opens the single-instance Library
    /// window in Select state constrained to the selected element's kind.
    [<Literal>]
    let chooseButton = "SelectorChooseButton"
    /// Spec 0038 (017): the Selector bay's inline quick-pick strip (the kind-constrained rows
    /// + confirm panel) — rendered only below the step-005 QuickPickThreshold.
    [<Literal>]
    let quickPickStrip = "SelectorQuickPickStrip"

/// Set `AutomationProperties.AutomationId` (freely mutable, unlike `Control.Name`) through
/// FuncUI's attr builder — the strip buttons live in a variable-membership row, so they carry
/// AutomationIds (the MaterialsControls discipline).
let private workbenchAutomationId (autoId : string) : IAttr<Border> =
    AttrBuilder<Border>.CreateProperty<string>(AutomationProperties.AutomationIdProperty, autoId, ValueNone)

/// The TextBlock flavour of the same attr (the step-016 status line has variable membership —
/// it renders only while a staleness status is set — so it carries an AutomationId, never a
/// write-once `Name`).
let private workbenchTextAutomationId (autoId : string) : IAttr<TextBlock> =
    AttrBuilder<TextBlock>.CreateProperty<string>(AutomationProperties.AutomationIdProperty, autoId, ValueNone)

/// Spec 0038 (016): the staleness status line — present only while a status is set (keyed:
/// variable membership), rendered right under the readout.
let private selectStatusRow (model : Model) : IView list =
    match model.selectStatus with
    | None -> []
    | Some text ->
        [ (TextBlock.create [
              workbenchTextAutomationId WorkbenchIds.selectStatus
              TextBlock.foreground (brush (color 178 34 34))
              TextBlock.textWrapping TextWrapping.Wrap
              TextBlock.margin (Thickness(8.0, 0.0, 0.0, 4.0))
              TextBlock.text text
          ]
          |> Avalonia.FuncUI.DSL.View.withKey WorkbenchIds.selectStatus) :> IView ]

/// A small clickable verb box for the host-added strip buttons (the MaterialsControls button look).
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

// ---------------------------------------------------------------------------
// Spec 0038 (017) — the Selector bay's Choose… / quick-pick flow. Choose… opens (or re-targets)
// the single-instance Library window in SELECT state constrained to the selected element's
// kind, through the launcher seam; the inline quick-pick strip (today's kind-constrained rows
// + confirm panel via LibraryControls) renders only below the step-005 QuickPickThreshold.
// Both paths converge on the SAME targeted bind message (`BindValueIdTo`).
// ---------------------------------------------------------------------------

/// The Choose… verb — compose one Select session and open (or re-target) the Library window
/// through `openLibrarySelectWindow`. Composed in the VIEW: the workbench runs `mkSimple`, so
/// the render's `dispatch` is the one return path into the loop (the window hosts capture
/// theirs with `Cmd.ofEffect`; this host has no Cmd seam). `onSelected` bakes the TARGETED
/// bind — the SAME `BindValueIdTo` the strip's confirm converges on; `onCancelled` dispatches
/// the reference-keyed `SelectSessionEnded` through a ref cell filled once the session exists,
/// so a session superseded by a re-target (its cancel fires while the successor is being
/// stored) can never clear the successor's handle.
let private chooseFromLibrary (model : Model) (dispatch : Msg -> unit) (element : TestElement) (owner : Window) : unit =
    let sessionRef : SelectSession option ref = ref None
    let selectCtx : WindowMode.SelectionContext<Library.LibraryEntry> =
        {
            kindConstraint = WindowMode.KindConstraint element.placement.catalogueKind
            target = WindowMode.TableElementTarget element.id
            onSelected = fun (entry : Library.LibraryEntry) -> dispatch (BindValueIdTo (element.id, entry.entryId))
            onCancelled = fun () -> sessionRef.Value |> Option.iter (fun session -> dispatch (SelectSessionEnded session))
        }
    match model.launchers.openLibrarySelectWindow model.library model.samples model.materials model.categories model.selectWindowModality owner selectCtx with
    | Some cancelAndClose ->
        let session : SelectSession = { target = element.id; cancelAndClose = cancelAndClose }
        sessionRef.Value <- Some session
        dispatch (SelectSessionStarted session)
    | None -> ()

/// The Choose… button (the workbench verb-box look). The pointer subscription re-patches on
/// the SELECTED ELEMENT's id — the closure's only render-varying capture; the proxies, the
/// launcher seam and the modality are app-scope constants — so a selection change re-arms the
/// handler for the new element and a bind never leaves a stale one behind. The owner window
/// for a modal Select open is resolved from the click's own visual tree at dispatch time.
let private selectorChooseButton (model : Model) (dispatch : Msg -> unit) (element : TestElement) : IView =
    Border.create [
        workbenchAutomationId WorkbenchIds.chooseButton
        Border.background (brush (color 232 232 232))
        Border.borderBrush (brush (color 120 120 120))
        Border.borderThickness 1.0
        Border.cornerRadius (CornerRadius 3.0)
        Border.padding (Thickness(12.0, 5.0))
        Border.margin (Thickness(0.0, 2.0, 8.0, 4.0))
        Border.horizontalAlignment HorizontalAlignment.Left
        Border.child (TextBlock.create [ TextBlock.text "Choose…" ])
        Border.onPointerPressed ((fun e ->
            e.Handled <- true
            match e.Source with
            | :? Visual as source ->
                match TopLevel.GetTopLevel source with
                | :? Window as owner -> chooseFromLibrary model dispatch element owner
                | _ -> ()
            | _ -> ()), SubPatchOptions.OnChangeOf element.id.value)
    ]
    |> Avalonia.FuncUI.DSL.View.withKey WorkbenchIds.chooseButton
    :> IView

/// The Selector bay content (spec 0038 step 017). No element selected → today's disabled
/// prompt (the empty LibraryControls bay). An element selected → the Choose… verb, and BELOW
/// the threshold also the inline quick-pick strip — the unchanged LibraryControls surface
/// (kind label, bound readout, kind-constrained rows, confirm panel) wrapped in a keyed,
/// AutomationId'd box so the headless gating proofs address it as one unit.
let private selectorBayContent (model : Model) (dispatch : Msg -> unit) : IView =
    match selectorOffer model, model.selection with
    | (QuickPickAndChoose | ChooseAlone) as offer, ElementSelected i ->
        let element = List.item i model.elements
        let strip : IView list =
            match offer with
            | QuickPickAndChoose ->
                [ Border.create [
                      workbenchAutomationId WorkbenchIds.quickPickStrip
                      Border.child (LibraryControls.view (libraryState model) (libraryHandlers dispatch))
                  ]
                  |> Avalonia.FuncUI.DSL.View.withKey WorkbenchIds.quickPickStrip
                  :> IView ]
            | ChooseAlone | NoSelectorOffer -> []
        StackPanel.create [
            StackPanel.orientation Orientation.Vertical
            StackPanel.spacing 4.0
            StackPanel.children (selectorChooseButton model dispatch element :: strip)
        ] :> IView
    | _, (TableSelected | NothingSelected | ElementSelected _) ->
        LibraryControls.view LibraryControls.empty (libraryHandlers dispatch)

// ---------------------------------------------------------------------------
// Spec 0038 Part L (037) — the experiment-collection BUILDER, rendered in the Ui layer beneath the
// (untouched) Controls-layer `ExperimentControls.view`: name / save / list / load a named collection
// through the `ExperimentCollectionProxy`, and attach ONE measured-data file per experiment (through
// the picker) whose parse / validation status shows inline as a typed message.
// ---------------------------------------------------------------------------

/// The collection builder's stable automation ids (CLAUDE.md UI guidance).
[<RequireQualifiedAccess>]
module CollectionIds =
    [<Literal>]
    let nameField = "CollectionNameField"
    [<Literal>]
    let saveButton = "SaveCollectionButton"
    [<Literal>]
    let status = "CollectionStatus"
    [<Literal>]
    let savedList = "SavedCollections"
    /// A saved-collection's loadable row, by its name (prefixed so it cannot collide).
    let savedCollection (name : string) : string = "SavedCollection_" + name
    [<Literal>]
    let dataFiles = "ExperimentDataFiles"
    /// A collected experiment's "Attach data file…" verb, by its id.
    let attachButton (id : string) : string = "AttachDataFile_" + id
    /// A collected experiment's inline parse/validation status text, by its id.
    let dataFileStatus (id : string) : string = "DataFileStatus_" + id

/// Spec 0038 Part L (037): open a measured-data file picker over the requesting window's storage
/// provider and, on a confirmed selection, dispatch the chosen `DataFilePath` back on the UI thread.
/// IO edge — the `openChartWindowHook` seam precedent — wrapped so an unavailable provider (a
/// headless host) degrades to a no-op rather than throwing. A headless test never clicks Attach; it
/// dispatches `AttachDataFileTo` directly over a mock proxy, so the picker is off the tested path.
let private pickDataFile (owner : Window) (onPicked : Experiments.DataFilePath -> unit) : unit =
    try
        let options =
            Avalonia.Platform.Storage.FilePickerOpenOptions(
                Title = "Attach measured-data file",
                AllowMultiple = false)
        let picked =
            async {
                let! files = owner.StorageProvider.OpenFilePickerAsync options |> Async.AwaitTask
                match List.ofSeq files with
                // The single-case `DataFilePath` case constructor (its `.create` factory is identical);
                // the module-qualified `.create` would bind the same-named case in expression position.
                | file :: _ -> onPicked (Experiments.DataFilePath file.Path.LocalPath)
                | [] -> ()
            }
        Avalonia.Threading.Dispatcher.UIThread.Post(fun () -> Async.StartImmediate picked)
    with _ -> ()

/// The "Attach data file…" verb for one collected experiment — resolve the owner window from the
/// click's visual tree (the `selectorChooseButton` precedent) and open the picker, dispatching
/// `AttachDataFileTo` on a confirmed selection. Keyed + AutomationId'd by the experiment id.
let private attachDataFileButton (dispatch : Msg -> unit) (experiment : Experiments.Experiment) : IView =
    let idStr = string experiment.id.value
    Border.create [
        workbenchAutomationId (CollectionIds.attachButton idStr)
        Border.background (brush (color 232 232 232))
        Border.borderBrush (brush (color 120 120 120))
        Border.borderThickness 1.0
        Border.cornerRadius (CornerRadius 3.0)
        Border.padding (Thickness(10.0, 4.0))
        Border.margin (Thickness(0.0, 0.0, 8.0, 0.0))
        Border.verticalAlignment VerticalAlignment.Center
        Border.child (TextBlock.create [ TextBlock.text "Attach data file…" ])
        Border.onPointerPressed ((fun e ->
            e.Handled <- true
            match e.Source with
            | :? Visual as source ->
                match TopLevel.GetTopLevel source with
                | :? Window as owner -> pickDataFile owner (fun path -> dispatch (AttachDataFileTo (experiment.id, path)))
                | _ -> ()
            | _ -> ()), SubPatchOptions.OnChangeOf idStr)
    ]
    |> Avalonia.FuncUI.DSL.View.withKey (CollectionIds.attachButton idStr)
    :> IView

/// One experiment's data-file row: its description, the Attach verb + the currently-attached file
/// name, and the typed parse / validation status (shown only once a file has been attached).
let private dataFileRow (model : Model) (dispatch : Msg -> unit) (experiment : Experiments.Experiment) : IView =
    let idStr = string experiment.id.value
    let attachedText =
        match experiment.dataFileOpt with
        | Some path -> $"file: %s{System.IO.Path.GetFileName path.value}"
        | None -> "no file attached"
    let statusText =
        match model.experimentDataStatus |> Map.tryFind experiment.id.value with
        | Some status -> status.text
        | None -> ""
    StackPanel.create [
        StackPanel.orientation Orientation.Vertical
        StackPanel.margin (Thickness(0.0, 0.0, 0.0, 6.0))
        StackPanel.children [
            TextBlock.create [ TextBlock.text experiment.description; TextBlock.textWrapping TextWrapping.Wrap; TextBlock.maxWidth 360.0 ]
            StackPanel.create [
                StackPanel.orientation Orientation.Horizontal
                StackPanel.spacing 6.0
                StackPanel.children [
                    attachDataFileButton dispatch experiment
                    TextBlock.create [ TextBlock.text attachedText; TextBlock.foreground (brush (color 120 120 120)); TextBlock.verticalAlignment VerticalAlignment.Center ]
                ]
            ]
            TextBlock.create [
                workbenchTextAutomationId (CollectionIds.dataFileStatus idStr)
                TextBlock.text statusText
                TextBlock.textWrapping TextWrapping.Wrap
                TextBlock.maxWidth 360.0
                TextBlock.isVisible (statusText <> "")
                TextBlock.foreground (brush (color 90 90 90))
            ]
        ]
    ]
    |> Avalonia.FuncUI.DSL.View.withKey ("DataFileRow_" + idStr)
    :> IView

/// The experiment-collection builder (name / save / list / load + per-experiment data-file attach).
/// A pure projection of the model plus the dispatch seam — no window needed, so a headless test
/// drives the collection round-trip and the attach status through `update` alone.
let private collectionBuilderView (model : Model) (dispatch : Msg -> unit) : IView =
    let sectionHeading (text : string) : IView =
        TextBlock.create [ TextBlock.text text; TextBlock.fontWeight FontWeight.SemiBold; TextBlock.margin (Thickness(0.0, 6.0, 0.0, 2.0)) ] :> IView
    // Commit the collection name on Enter / blur (the `numberField` precedent) — an every-keystroke
    // dispatch that rewrites the model text is the FuncUI render-loop hazard we avoid here.
    let commitName (src : obj) : unit =
        match src with
        | :? TextBox as tb when not (isNull tb.Text) -> dispatch (CollectionSetName tb.Text)
        | _ -> ()
    let nameField : IView =
        TextBox.create [
            TextBox.name CollectionIds.nameField
            TextBox.width 180.0
            TextBox.text model.collectionName
            TextBox.onKeyDown (fun e -> if e.Key = Key.Enter then commitName e.Source)
            TextBox.onLostFocus (fun e -> commitName e.Source)
        ] :> IView
    let statusRow : IView list =
        match model.collectionStatus with
        | None -> []
        | Some text ->
            [ (TextBlock.create [
                  workbenchTextAutomationId CollectionIds.status
                  TextBlock.text text
                  TextBlock.foreground (brush (color 90 90 90))
                  TextBlock.textWrapping TextWrapping.Wrap
                  TextBlock.margin (Thickness(0.0, 2.0, 0.0, 2.0))
               ] |> Avalonia.FuncUI.DSL.View.withKey CollectionIds.status) :> IView ]
    let savedRow : IView list =
        match model.savedCollections with
        | [] -> []
        | names ->
            [ sectionHeading "Saved collections:"
              WrapPanel.create [
                  WrapPanel.name CollectionIds.savedList
                  WrapPanel.orientation Orientation.Horizontal
                  WrapPanel.children (names |> List.map (fun n -> workbenchButton (CollectionIds.savedCollection n.value) n.value (fun () -> dispatch (CollectionLoad n.value))))
              ] :> IView ]
    let dataFileRows : IView list =
        match model.experimentCollection.experiments with
        | [] -> [ TextBlock.create [ TextBlock.text "(add experiments above, then attach one data file to each)"; TextBlock.foreground (brush (color 120 120 120)) ] :> IView ]
        | experiments ->
            [ StackPanel.create [
                  StackPanel.name CollectionIds.dataFiles
                  StackPanel.orientation Orientation.Vertical
                  StackPanel.children (experiments |> List.map (dataFileRow model dispatch))
              ] :> IView ]
    StackPanel.create [
        StackPanel.orientation Orientation.Vertical
        StackPanel.spacing 2.0
        StackPanel.children (
            [ sectionHeading "Collection:"
              StackPanel.create [
                  StackPanel.orientation Orientation.Horizontal
                  StackPanel.spacing 6.0
                  StackPanel.children [ nameField; workbenchButton CollectionIds.saveButton "Save collection" (fun () -> dispatch CollectionSave) ]
              ] :> IView ]
            @ statusRow
            @ savedRow
            @ [ sectionHeading "Data files:" ]
            @ dataFileRows)
    ] :> IView

/// The Experiments bay content (spec 0038 Part L, 037): the (untouched) Controls-layer experiment
/// editor + chart, then the Ui-layer collection builder stacked below it.
let private experimentsBayContent (model : Model) (dispatch : Msg -> unit) : IView =
    StackPanel.create [
        StackPanel.orientation Orientation.Vertical
        StackPanel.spacing 6.0
        StackPanel.children [
            ExperimentControls.view (experimentState model) (experimentHandlers dispatch)
            collectionBuilderView model dispatch
        ]
    ] :> IView

/// The Main-screen ribbon Bays — every large control, each bound to the current model / dispatch. Adding
/// or removing a Bay here is the ONLY change needed to add / remove a large control from the Main screen.
/// (Spec 0038 step 015: no FULL-SURFACE bay remains — the samples workbench that used the mode is the
/// Library WINDOW now; the mode itself stays in the generic Ribbon control.)
let mainBays (model : Model) (dispatch : Msg -> unit) : Ribbon.Bay list =
    [ { name = BayNames.rotation; content = RotationControls.view (rotationState model) (rotationHandlers dispatch); mode = Ribbon.InRibbonPane }
      { name = BayNames.move; content = RayPositionControls.view (moveState model) (moveHandlers dispatch); mode = Ribbon.InRibbonPane }
      { name = BayNames.add; content = ElementPaletteControls.view (paletteState model) (paletteHandlers model dispatch); mode = Ribbon.InRibbonPane }
      { name = BayNames.render; content = RendererControls.view model.render (renderHandlers dispatch); mode = Ribbon.InRibbonPane }
      { name = BayNames.selector; content = selectorBayContent model dispatch; mode = Ribbon.InRibbonPane }
      { name = BayNames.experiments; content = experimentsBayContent model dispatch; mode = Ribbon.InRibbonPane }
      { name = BayNames.details; content = LayerBandsControls.view (detailsState model); mode = Ribbon.InRibbonPane } ]

let private mainControlBar (bays : Ribbon.Bay list) (model : Model) (dispatch : Msg -> unit) : IView =
    StackPanel.create [
        StackPanel.orientation Orientation.Vertical
        StackPanel.spacing 4.0
        StackPanel.children (
            [
                // The tab-strip row: the ribbon fills it; the right-aligned "Materials…" (spec 0038
                // step 013) and "Library…" (step 015) buttons ride its right edge, TOP-aligned
                // beside the tab strip, each opening its single-instance window through the launcher
                // seam. Docked-right FIRST so the ribbon (the fill child) can never push them
                // off-screen; Materials… docks first and therefore sits rightmost (its step-013
                // position), Library… lands immediately left of it.
                DockPanel.create [
                    DockPanel.children [
                        Border.create [
                            Border.dock Dock.Right
                            Border.verticalAlignment VerticalAlignment.Top
                            Border.margin (Thickness(0.0, 8.0, 8.0, 0.0))
                            Border.child (workbenchButton WorkbenchIds.openMaterialsButton "Materials…" (fun () -> dispatch OpenMaterialsWindow))
                        ]
                        Border.create [
                            Border.dock Dock.Right
                            Border.verticalAlignment VerticalAlignment.Top
                            Border.margin (Thickness(0.0, 8.0, 0.0, 0.0))
                            Border.child (workbenchButton WorkbenchIds.openLibraryButton "Library…" (fun () -> dispatch OpenLibraryWindow))
                        ]
                        Ribbon.view { bays = bays; selected = model.ribbon } (fun name -> dispatch (SelectBay name))
                    ]
                ] :> IView
                TextBlock.create [ TextBlock.name UiIds.readout; TextBlock.margin (Thickness(8.0, 0.0, 0.0, 4.0)); TextBlock.text (readoutText model) ] :> IView
            ]
            // Spec 0038 (016): the staleness status line — present only while a status is set.
            @ selectStatusRow model)
    ] :> IView

let private mainTableCanvas (model : Model) : IView =
    Canvas.create [
        Canvas.name UiIds.canvas
        Canvas.width canvasWidth
        Canvas.height canvasHeight
        Canvas.horizontalAlignment HorizontalAlignment.Left
        Canvas.verticalAlignment VerticalAlignment.Top
        // Spec 0038 (031): the out-of-band dispersion badges overlay the drawn elements — appended
        // last so they sit on top of the schematic; an in-band scene contributes none.
        Canvas.children (mainTableViews model @ mainElementViews model @ outOfBandBadges model)
    ] :> IView

/// The Main screen view: the ribbon of large controls on top, and BELOW the ribbon strip either the
/// shared table (for an in-pane "table" bay — same selection / pan / zoom / rotate gestures as the test
/// scene) or the active bay's OWN content (for a FULL-SURFACE bay — which fills that whole area and
/// wires NO table gestures; spec 0038 step 015: no current bay uses the mode — the samples workbench
/// that did is the Library WINDOW now — but the keyed slot stays for any future full-surface bay).
/// The bays are built once and shared: the ribbon hosts the in-pane bays' content and shows only a
/// tab for a full-surface bay, whose content is placed here below.
let mainView (model : Model) (dispatch : Msg -> unit) : IView =
    let toScreen (e : PointerEventArgs) : ScreenPoint = SceneInput.canvasPoint UiIds.canvas e
    // The shared table surface + its pointer / wheel gestures. A thunk, so it is only built for a table bay:
    // a full-surface bay never realizes the canvas and must not wire the table gestures.
    let tableSurface () : IView =
        Border.create [
            Border.background (brush (color 250 250 250))
            Border.onPointerPressed (fun e -> e.Handled <- true; dispatch (PointerDown (toScreen e)))
            Border.onPointerMoved (fun e -> e.Handled <- true; dispatch (PointerMove (toScreen e)))
            Border.onPointerReleased (fun e -> e.Handled <- true; dispatch (PointerUp (toScreen e)))
            Border.onPointerWheelChanged (fun e ->
                e.Handled <- true
                dispatch (Wheel (wheelModifiers e.KeyModifiers, (if e.Delta.Y >= 0.0 then 1 else -1))))
            Border.child (mainTableCanvas model)
        ] :> IView
    let bays = mainBays model dispatch
    // The area BELOW the ribbon strip: a full-surface bay fills it with the bay's OWN content (no table
    // canvas, no gestures — the workbench owns the whole surface); every in-pane / table bay keeps the
    // shared table canvas and its pan / zoom / rotate gestures.
    let belowStrip : IView =
        match Ribbon.activeBay { bays = bays; selected = model.ribbon } with
        | Some b ->
            match b.mode with
            | Ribbon.FullSurface ->
                // KEY the full-surface slot by bay name: a bay change then RECREATES the content
                // instead of patching one workbench's styled, named rows into another's ("Cannot set
                // Name : styled element already styled") — the same keyed-slot discipline step 007
                // gave the ribbon pane. `View` is fully qualified (the DSL `View` module vs the
                // `Types` `View<'t>` type are both in scope here).
                let surface =
                    Border.create [ Border.child b.content ]
                    |> Avalonia.FuncUI.DSL.View.withKey b.name
                surface :> IView
            | Ribbon.InRibbonPane -> tableSurface ()
        | None -> tableSurface ()
    DockPanel.create [
        DockPanel.children [
            Border.create [ Border.dock Dock.Top; Border.child (mainControlBar bays model dispatch) ]
            belowStrip
        ]
    ] :> IView
