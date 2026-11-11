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
open Avalonia.VisualTree
open Avalonia.FuncUI.Builder
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open Berreman.Constants
open Berreman.Geometry
open Berreman.MaterialProperties
open Berreman.Fields
open OpticalConstructor.Domain
open OpticalConstructor.Domain.ItemModels
open OpticalConstructor.Domain.ItemEvaluation
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.KitFolders
open OpticalConstructor.Domain.KitLibrary
open OpticalConstructor.Domain.Setups
open OpticalConstructor.Domain.NamingTemplates
open OpticalConstructor.Domain.ExperimentDataLoad
open OpticalConstructor.Storage.KitContentFiles
open OpticalConstructor.Storage.KitFolderStore
open OpticalConstructor.Ui.KitOpening
open OpticalConstructor.Domain.KitArchive
open Softellect.Sys.Primitives
open OpticalConstructor.Domain.CatalogueImport
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Storage.CatalogueImportStore
open OpticalConstructor.Domain.Placement
open OpticalConstructor.Domain.Table
open OpticalConstructor.Domain.TableView
// Spec 0038 (017): opened for `QuickPickThreshold.defaultValue` — the single-case DU's case
// shadows the type under `WorkbenchSettings.`-qualified expression resolution.
open OpticalConstructor.Domain.WorkbenchSettings
open OpticalConstructor.Controls

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

type Setups.SetupSlot with

    /// The element's along-beam parameter (its x), in metres.
    member e.elementX : float = e.placement.placementPoint.x / 1.0<meter>

    /// The element's oriented box, in table-frame metres, magnified by its draw zoom (the shape of test #2,
    /// projected through the live table view).
    member e.boxCorners : Vector3 list =
        let { primaryNormal = n1; secondaryNormal = n2; thirdAxis = n3 } = orientedBasis e.placement
        let cx = e.placement.placementPoint.x / 1.0<meter>
        let cy = e.placement.placementPoint.y / 1.0<meter>
        let hb = e.zoom.value * (e.placement.box.b / 2.0 / 1.0<meter>)
        let h1 = e.zoom.value * (e.placement.box.a1 / 2.0 / 1.0<meter>)
        let h2 = e.zoom.value * (e.placement.box.a2 / 2.0 / 1.0<meter>)
        let corner (sb : float) (s1 : float) (s2 : float) : Vector3 =
            Vector3.create
                (cx + sb * hb * n1.x + s1 * h1 * n2.x + s2 * h2 * n3.x)
                (cy + sb * hb * n1.y + s1 * h1 * n2.y + s2 * h2 * n3.y)
                (sb * hb * n1.z + s1 * h1 * n2.z + s2 * h2 * n3.z)
        [ corner 1.0 -1.0 -1.0; corner 1.0 -1.0 1.0; corner 1.0 1.0 1.0; corner 1.0 1.0 -1.0
          corner -1.0 -1.0 -1.0; corner -1.0 -1.0 1.0; corner -1.0 1.0 1.0; corner -1.0 1.0 -1.0 ]

/// An element's position in the scene's element list (the light source, the ray root, is 0).
type SceneElementIndex =
    | SceneElementIndex of int

    member this.value = let (SceneElementIndex i) = this in i

    /// The index as the position of the element's repeated controls (its out-of-band badge).
    member this.uiPosition : ConstructorUiPosition = ConstructorUiPosition this.value

/// Spec 0046: the open right-click ELEMENT MENU — the element it serves (by its serializable id, so a
/// later selection change cannot retarget it) and the canvas point it opened at. A model-driven overlay
/// rather than an Avalonia `ContextMenu`, so it is deterministic and provable headless.
type ElementMenu =
    {
        target : Library.ElementId
        at : ScreenPoint
    }

/// What the rotation gestures currently target.
type Selection =
    | TableSelected
    | ElementSelected of SceneElementIndex
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
        /// app-scope stores and import seam — the ribbon strip's `Materials…` button
        /// reaches it through this seam, so a headless test substitutes a recording launcher and
        /// observes the request. The Material / Category editor launchers the retired Materials
        /// bay carried here moved INTO that window's own composition (`MaterialsWindow`).
        openMaterialsWindow : MaterialLibrary.MaterialProxy -> MaterialLibrary.CategoryProxy -> CatalogueImportContext -> unit
        /// Open the SINGLE-INSTANCE Library window (spec 0038 step 015, UICOMP_XDUO_0010) over
        /// the Library seam, app-scope stores and catalogue imports
        /// (categories thread through to the Sample editor's step-019 Choose
        /// material… composition) — the ribbon strip's `Library…` button reaches it through
        /// this seam. The Sample-editor launcher the retired Library bay carried here
        /// (`openSampleEditor`) moved INTO that window's own composition (`LibraryWindow`),
        /// the step-013 Materials precedent.
        openLibraryWindow : Library.LibraryProxy -> Library.SampleProxy -> Library.SourceProxy -> MaterialLibrary.MaterialProxy -> MaterialLibrary.CategoryProxy -> LibraryBackup.LibraryBackupProxy -> CatalogueImportContext -> unit
        /// Spec 0038 (017): open the SAME single-instance Library window in SELECT state over
        /// the session context (the Select bay's Choose… verb). A missing window opens fresh
        /// in Select mode; a LIVE one is RE-TARGETED at the new context (a second Choose…
        /// re-points the instance — the step-016 `RetargetedWindow` semantics). The step-005
        /// modality switch decides ShowDialog-vs-Show against the REQUESTING window. Returns
        /// the opened session's cancel-and-close handle — the `SelectSession` staleness lever —
        /// or None when the open failed (the typed error is dropped at this unit seam: a failed
        /// open leaves no window, exactly the user-visible outcome).
        openLibrarySelectWindow : Library.LibraryProxy -> Library.SampleProxy -> Library.SourceProxy -> MaterialLibrary.MaterialProxy -> MaterialLibrary.CategoryProxy -> LibraryBackup.LibraryBackupProxy -> CatalogueImportContext -> WorkbenchSettings.SelectWindowModality -> Window -> WindowMode.SelectionContext<Library.LibraryEntry> -> (unit -> unit) option
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
                fun materials categories catalogueImports ->
                    openBrowse WindowLauncher.MaterialsWindowKey (fun () -> MaterialsWindow(materials, categories, catalogueImports) :> Window)
            openLibraryWindow =
                fun library samples sources materials categories libraryBackups catalogueImports ->
                    openBrowse WindowLauncher.LibraryWindowKey (fun () -> LibraryWindow(library, samples, sources, materials, categories, libraryBackups, catalogueImports) :> Window)
            openLibrarySelectWindow =
                fun library samples sources materials categories libraryBackups catalogueImports modality requestingWindow selectCtx ->
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
                            (fun (_ : WindowLauncher.WindowKey) -> LibraryWindow(library, samples, sources, materials, categories, libraryBackups, catalogueImports, mode = WindowMode.Select selectCtx) :> Window |> Ok)
                            modality
                            (WindowLauncher.SelectOpen (requestingWindow, retargetWindow))
                    match launcher.openOrActivate WindowLauncher.LibraryWindowKey with
                    | Ok (WindowLauncher.CreatedWindow window)
                    | Ok (WindowLauncher.ActivatedWindow window)
                    | Ok (WindowLauncher.RetargetedWindow window) -> Some (fun () -> window.Close ())
                    | Error _ -> None
        }

/// What the Kit menu reaches outside the model (spec 0050 S5.3); function-valued, so it compares by reference.
[<ReferenceEquality>]
type KitWindowShell =
    {
        /// The explorer that shows the kit's folder (spec 0050 S5.3, step 182).
        explorer : ExplorerProxy
        /// Closes the window hosting the workbench, which releases its kit as it closes (spec 0050 S5.3, step 154).
        requestClose : unit -> unit
        /// The Send kit… save dialog (spec 0050 S6, step 198).
        zipPicker : KitZipPicker
        /// The zip writer a send runs through (spec 0050 S6, step 197).
        archive : KitArchiveProxy
        /// Runs the send off the UI thread, with its progress and its end as messages (spec 0050 S6, step 200).
        archiveJobs : KitArchiveJob.KitArchiveJobHost
        /// The folder dialog that chooses the kit to copy experiments from (spec 0050 S6, step 145).
        kitPicker : KitFolderPicker
    }

    /// The test scenes' shell: it starts no Explorer, closes no window, picks no zip or kit and runs no job; its
    /// archive is an in-memory disk of its own (spec 0050 S5.3, S6).
    static member inert : KitWindowShell =
        {
            explorer = ExplorerProxy.createInMemory ignore
            requestClose = ignore
            zipPicker = KitZipPicker.none
            archive = KitArchiveProxy.createInMemory (InMemoryArchiveDisk.create ())
            archiveJobs = KitArchiveJob.KitArchiveJobHost.inert
            kitPicker =
                {
                    pickKitFolder = fun () -> async { return None }
                }
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

/// What asked for the check of kit files changed outside the app (spec 0050 S5.3).
type KitCheckOrigin =
    /// The window was activated; a check that finds nothing says nothing (spec 0050 S5.3).
    | KitCheckOnActivation
    /// The Kit menu's Reload changes; a check that finds nothing says so (spec 0050 S5.3).
    | KitCheckOnRequest

/// The kit chosen to copy experiments from, opened read-only, its experiments as read, and the ids chosen so far, in
/// the source's order (spec 0050 S6).
type KitCopySource =
    {
        source : KitContext
        experiments : Experiments.Experiment list
        chosen : Experiments.ExperimentId list
        /// The source kit's items, read when it opens (spec 0050 S7.2).
        items : Item list
        /// The source kit's setups, read when it opens, which the chosen experiments' items are resolved through (spec
        /// 0050 S8.2).
        setups : Setups.Setup list
        /// The mapping table: one row per item the chosen experiments use, with its choice (spec 0050 S7.2).
        itemRows : ItemCopyRow list
    }

    /// Chooses the experiment, or unchooses it when chosen, keeping the source's order; an id the source does not
    /// hold is never chosen (spec 0050 S6).
    member this.toggled (id : Experiments.ExperimentId) : KitCopySource =
        {
            this with
                chosen =
                    this.experiments
                    |> List.map (fun experiment -> experiment.id)
                    |> List.filter (fun held -> (held = id) <> List.contains held this.chosen)
        }

    /// Closes the source. A read-only source holds no lock, so a failed close is only logged (spec 0050 S6, step 131).
    member this.release () : unit =
        match this.source.close () with
        | Ok () -> ()
        | Error error -> Softellect.Sys.Logging.Logger.logError $"KitCopySource.release: could not close the copy source: {error.describe}"

/// The step of the constructor's flow strip the window shows, each reachable directly (spec 0050 S7.2, 001 §O). It is
/// window state, never stored, so it carries no wire tag.
type ConstructorFlowStep =
    /// The kit's Items panel (spec 0050 S7.2).
    | ItemsFlowStep
    /// The table (spec 0050 S7.2).
    | SetupsFlowStep
    /// The existing Experiments bay (spec 0050 S7.2).
    | ExperimentsFlowStep

    /// The steps in 001 §O's order; S9.3, S11 and S14 insert Data, Unknowns, Plan, Fit, Results and Publish when each
    /// works (spec 0050 S7.2, .spec-md §3.22).
    static member all : ConstructorFlowStep list = [ ItemsFlowStep; SetupsFlowStep; ExperimentsFlowStep ]

    /// The strip's caption of the step (spec 0050 S7.2).
    member this.caption : ControlCaption =
        match this with
        | ItemsFlowStep -> ControlCaption "Items"
        | SetupsFlowStep -> ControlCaption "Setups"
        | ExperimentsFlowStep -> ControlCaption "Experiments"

    /// The id of the step's box in the flow strip (spec 0050 S7.2).
    member this.flowUiId : ConstructorUiId =
        match this with
        | ItemsFlowStep -> ConstructorUiId.flowItemsStep
        | SetupsFlowStep -> ConstructorUiId.flowSetupsStep
        | ExperimentsFlowStep -> ConstructorUiId.flowExperimentsStep

/// The open "Add item from library model…" form: the new item's id, minted when it opens, the models it offers, the
/// one chosen, the label as typed (a raw caption until `ItemLabel.tryCreate` reads it) and the dial (spec 0050 S7.2).
type AddItemForm =
    {
        itemId : ItemId
        offers : ItemModelChoice list
        chosenOpt : ItemModelChoice option
        labelText : ControlCaption
        dial : ItemDial
    }

    /// The draft the form's Add sends: `None` until a model is chosen and the label reads as one (spec 0050 S7.2).
    member this.itemDraftOpt : ItemDraft option =
        match this.chosenOpt, ItemLabel.tryCreate this.labelText.value with
        | Some chosen, Ok label ->
            Some
                {
                    id = this.itemId
                    label = label
                    model = chosen
                    dial = this.dial
                }
        | Some _, Error _
        | None, _ -> None

/// The two states a user sets a parameter to by hand; a calibration comes only from a fit (spec 0050 S7.2, S11.5).
type ParameterStateChoice =
    | KnownStateChoice
    | UnknownStateChoice

/// A number the parameter editor takes as typed (spec 0050 S7.2).
type ParameterEditField =
    | ValueEditField
    | LowerEditField
    | UpperEditField
    | PriorSigmaEditField

    /// The field's name, as a refusal names it (spec 0050 S7.2).
    member this.text : string =
        match this with
        | ValueEditField -> "value"
        | LowerEditField -> "lower"
        | UpperEditField -> "upper"
        | PriorSigmaEditField -> "prior sigma"

/// The open parameter editor: the item and parameter it edits, the state chosen and the numbers as typed, raw captions
/// until `tryState` reads them (spec 0050 S7.2; 001 §O).
type ParameterEditDraft =
    {
        itemId : ItemId
        name : ItemModels.ParameterName
        choice : ParameterStateChoice
        valueText : ControlCaption
        lowerText : ControlCaption
        upperText : ControlCaption
        priorSigmaText : ControlCaption
    }

    /// The editor opened on the parameter's state: a calibration opens as known at its value, and a field the state
    /// has no number for is empty (spec 0050 S7.2).
    static member ofParameter (itemId : ItemId) (parameter : Parameter) : ParameterEditDraft =
        let written (number : double) : ControlCaption =
            ControlCaption (number.ToString("R", System.Globalization.CultureInfo.InvariantCulture))
        let empty = ControlCaption ""
        let known (value : ItemModels.ParameterValue) : ParameterEditDraft =
            {
                itemId = itemId
                name = parameter.name
                choice = KnownStateChoice
                valueText = written value.value
                lowerText = empty
                upperText = empty
                priorSigmaText = empty
            }
        match parameter.state with
        | KnownParameter value -> known value
        | CalibratedParameter calibrated -> known calibrated.value
        | UnknownParameter unknown ->
            {
                itemId = itemId
                name = parameter.name
                choice = UnknownStateChoice
                valueText = written unknown.start.value
                lowerText = written unknown.bounds.lower.value
                upperText = written unknown.bounds.upper.value
                priorSigmaText =
                    match unknown.priorOpt with
                    | Some prior -> written prior.sigma.value
                    | None -> empty
            }

    /// The draft with one field's text replaced (spec 0050 S7.2).
    member this.withFieldText (field : ParameterEditField) (text : ControlCaption) : ParameterEditDraft =
        match field with
        | ValueEditField -> { this with valueText = text }
        | LowerEditField -> { this with lowerText = text }
        | UpperEditField -> { this with upperText = text }
        | PriorSigmaEditField -> { this with priorSigmaText = text }

    /// The state the editor sets, a prior centred on the start; a needed text that is no number is refused naming the
    /// field. It checks no rule of the state: `withParameterState` applies `Parameter.validate` (spec 0050 S7.2).
    member this.tryState : Result<ParameterState, ItemError> =
        let invariant = System.Globalization.CultureInfo.InvariantCulture
        let read (field : ParameterEditField) (text : ControlCaption) : Result<double, ItemError> =
            match System.Double.TryParse(text.value, System.Globalization.NumberStyles.Float, invariant) with
            | true, number -> Ok number
            | false, _ ->
                Error (InvalidParameterStateErr $"{this.name.value}: the {field.text} '{text.value}' is not a number")
        let parameterValue (field : ParameterEditField) (text : ControlCaption) : Result<ItemModels.ParameterValue, ItemError> =
            read field text |> Result.map ItemModels.ParameterValue
        match this.choice with
        | KnownStateChoice -> parameterValue ValueEditField this.valueText |> Result.map KnownParameter
        | UnknownStateChoice ->
            let readSigma : Result<ParameterSigma option, ItemError> =
                if System.String.IsNullOrWhiteSpace this.priorSigmaText.value then Ok None
                else read PriorSigmaEditField this.priorSigmaText |> Result.map (ParameterSigma >> Some)
            parameterValue ValueEditField this.valueText
            |> Result.bind (fun start ->
                parameterValue LowerEditField this.lowerText
                |> Result.bind (fun lower ->
                    parameterValue UpperEditField this.upperText
                    |> Result.bind (fun upper ->
                        readSigma
                        |> Result.map (fun sigmaOpt ->
                            let bounds : ParameterInterval =
                                {
                                    lower = lower
                                    upper = upper
                                }
                            let prior (sigma : ParameterSigma) : ParameterPrior =
                                {
                                    centre = start
                                    sigma = sigma
                                }
                            UnknownParameter
                                {
                                    start = start
                                    bounds = bounds
                                    priorOpt = sigmaOpt |> Option.map prior
                                }))))

type TableAndElementRotationModel =
    {
        table : OpticalTable
        view : TableViewState        // rotatable (when the table is selected) + pannable + zoomable
        /// The table's slots, the light source (the ray root) first (spec 0050 S8.2).
        elements : Setups.SetupSlot list
        selection : Selection
        drag : DragState
        /// The table's R3-lock (task 008): unlocked by default. Elements carry their own R3-lock.
        tableR3Locked : RotationLock
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
        render : RendererControls.RendererControlsState
        /// The selected Main-screen ribbon Bay, by name (task 018). Used by `mainView` only.
        ribbon : Ribbon.BayName
        /// Whether the elements SNAP onto the beam (the Main "Lego constructor": the source is the ray
        /// root, downstream elements snap onto the beam reflecting at mirrors). `false` for the static test
        /// windows (free placement, unchanged). Set by `initMain`.
        snapChain : Scene.SnapMode
        /// Spec 0027 (024): the injected mock Library IO seam (the functional-proxy convention). Used by
        /// the Main-screen Select bay to constrain the choosable entries by the selected element's kind
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
        pendingEntry : ElementValueId option
        /// Spec 0038 (016): the open Select-state window session (if any) this workbench
        /// requested — cancelled and closed by the staleness rules (a changed table selection,
        /// an element add, the selected element's removal) and cleared by the targeted bind.
        activeSelect : SelectSession option
        /// Spec 0038 (016): the staleness status line — a targeted Select return whose element
        /// has vanished reports here (a no-op plus a status line, never a throw). Cleared by
        /// the next successful bind or table-selection change (the disarm discipline).
        selectStatus : ControlCaption option
        /// Spec 0033 (024): the injected materials WRITE seam (STORE_XDUO_0001). Spec 0038
        /// steps 013/015: the strip buttons' Materials and Library windows operate over THIS
        /// store (the bays they replaced are gone).
        materials : MaterialLibrary.MaterialProxy
        /// Spec 0033 (024): the injected samples WRITE seam (STORE_XDUO_0002). Spec 0038
        /// step 015: the strip button's Library window (and the Sample editors it opens)
        /// operates over THIS store — the samples workbench bay it replaced is gone.
        samples : Library.SampleProxy
        /// Spec 0046: the injected USER light-source WRITE seam behind the Library window's source
        /// editor. Defaulted to a fresh in-memory store; the composition root threads the app-scope one
        /// by record update (the `experimentData` precedent) — and composes `library` LIVE over it
        /// (`Library.liveLibrary`), so a user source binds and resolves on the table.
        sources : Library.SourceProxy
        /// The app-scope library backup store; test scenes use a name-only in-memory double.
        libraryBackups : LibraryBackup.LibraryBackupProxy
        /// The app-scope catalogue import seam.
        catalogueImports : CatalogueImportContext
        /// Spec 0035 (009): the injected category WRITE seam (STORE_XDUO_0003) behind the material
        /// editor's create picker and — spec 0038 step 013 — the Materials window's category facet
        /// and its "Categories…" verb.
        categories : MaterialLibrary.CategoryProxy
        /// Spec 0033 (024): the window launcher seam behind the workbench strip buttons.
        launchers : EditorLaunchers
        /// Spec 0038 (017): the step-005 quick-pick cutoff — the Select bay's inline strip
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
        /// The kit this constructor works in (spec 0050 S4.3).
        kit : KitContext
        /// The kit's folder, `None` for the scratch kit of a scene that opened no folder (spec 0050 S5.3).
        kitFolderOpt : KitFolder option
        /// The explorer and the window close the Kit menu reaches; `initWith` holds the inert one (spec 0050 S5.3).
        kitShell : KitWindowShell
        /// The kit files changed outside the app that the kit bar offers to reload, in `changedKitFiles`' order, empty when nothing is offered (spec 0050 S5.3).
        kitChanges : FileName list
        /// The window's one send of its kit, idle while none runs (spec 0050 S6, step 200).
        kitSend : KitArchiveJob.KitArchiveJobState
        /// The kit open to copy experiments from, `None` while no copy is being chosen (spec 0050 S6).
        kitCopyOpt : KitCopySource option
        /// The last kit read or write that needs telling (spec 0050 S4.3).
        kitStatus : ControlCaption option
        /// The naming template being typed, the kit file's wherever the window takes one (spec 0050 S8.3).
        namingTemplateText : ControlCaption
        /// The setup name being typed, validated when the setup is created (spec 0050 S8.2).
        setupName : ControlCaption
        /// The kit's setups in name order, as the window last read or wrote them (spec 0050 S8.2).
        kitSetups : Setups.Setup list
        /// The kit setup the table draws, as last created or loaded; `None` while the table is not yet one (spec 0050 S8.2).
        activeSetupOpt : Setups.Setup option
        /// The kit's items in label order, as the kit last read or wrote them (spec 0050 S6, S7.1).
        kitItems : Item list
        /// The slot whose chip's list is open, `None` while every chip is closed (spec 0050 S8.4).
        slotChipOpenOpt : Library.ElementId option
        /// The grid panel's draft while the panel is open, `None` while it is closed (spec 0050 S8.4).
        setupGridOpt : SetupGrids.SetupGridDraft option
        /// The flow strip's step the window shows, the same in the forward and inverse constructors (spec 0050 S7.2).
        flowStep : ConstructorFlowStep
        /// The item-model store the add form offers (spec 0050 S7.2, step 209).
        itemModels : ItemModels.ItemModelProxy
        /// Mints the id of the item the add form opens for (spec 0050 S7.2).
        itemIds : ItemIdProvider
        /// The item the Items panel shows, `None` while no row is pressed (spec 0050 S7.2).
        selectedItemOpt : ItemId option
        /// The open "Add item from library model…" form, `None` while it is closed (spec 0050 S7.2).
        addItemFormOpt : AddItemForm option
        /// The open parameter editor of the selected item, `None` while it is closed (spec 0050 S7.2).
        parameterEditOpt : ParameterEditDraft option
        /// Measured-data attachments read through kit (spec 0050 S4.2/S4.3).
        experimentData : ExperimentData.ExperimentDataProxy
        /// The data badge of each experiment with a mapped attachment, keyed by its id, set when a file is attached and
        /// when the kit opens (spec 0038 Part L; spec 0050 S9.1).
        experimentDataStatus : Map<Experiments.ExperimentId, ExperimentDataBadge>
        /// Spec 0046: the open right-click element menu (ray toggles, which beam the element sits on,
        /// remove), if any. Closed by any left click, a selection change, or its own Close verb.
        elementMenu : ElementMenu option
        /// Spec 0046: the GLOBAL rotation pivot — every element rotates about its centre (the default),
        /// or about the middle of its incident face, so the incident ray always meets it at that point.
        rotationPivot : BeamLayout.RotationPivot
        /// Spec 0046: the experiment-data EXPORT seam — the Experiments bay's Export data hands the draft's
        /// computed run to it. Defaulted to an in-memory store; the composition root threads the CSV one.
        experimentExport : ExperimentExport.ExperimentExportProxy
        /// Spec 0046: the last export's status line.
        exportStatus : ControlCaption option
    }

/// The Main-screen ribbon Bay names (the "large controls" the ribbon shows MS-Word-style).
module BayNames =
    /// Spec 0046 (operator): the verb "Rotate" (was "Rotation").
    let rotation : Ribbon.BayName = Ribbon.BayName "Rotate"
    let move : Ribbon.BayName = Ribbon.BayName "Move"
    let add : Ribbon.BayName = Ribbon.BayName "Add"
    let render : Ribbon.BayName = Ribbon.BayName "Render"
    /// Spec 0027 (024): the Select bay (labelled "Library" until spec 0033 step 014, "Selector" until spec
    /// 0046) — pick the choosable spec for the selected element. The kind-constrained, confirm-gated
    /// binding through `LibraryControls` and the read-only `LibraryProxy` is unchanged; only the label
    /// moved, freeing the word Library for the samples workbench.
    let selector : Ribbon.BayName = Ribbon.BayName "Select"
    /// Spec 0027 (024) Phase 2: the Experiments bay — pick which element's R1 sweeps a full circle.
    let experiments : Ribbon.BayName = Ribbon.BayName "Experiments"
    /// Spec 0027 (026): the Details bay — the selected element's bound Library entry (what it is, its full
    /// description, and a layer-stack band view for a multilayer sample).
    let details : Ribbon.BayName = Ribbon.BayName "Details"
    // Spec 0038 steps 013/015: the Materials AND Library bays are REMOVED from the ribbon — the
    // materials workbench is the single-instance Materials WINDOW (UICOMP_XDUO_0009) and the
    // samples/library workbench the single-instance Library WINDOW (UICOMP_XDUO_0010), each
    // opened by its right-aligned tab-strip-row button. No full-surface bay remains; every bay
    // keeps the shared table canvas below the strip.
    /// Spec 0046: Add comes first — a scene is built before it is turned or moved.
    let allBayNames : Ribbon.BayName list = [ add; rotation; move; render; selector; experiments; details ]

let defaultElementZoom : BeamLayout.ElementZoom = BeamLayout.ElementZoom 5.0

/// Mint a fresh, serializable element id (spec §1/§5). Added elements get a GUID; the seeded
/// source/detector get deterministic ids (see `initMain`) so wiring/experiment tests can name them.
let private freshId () : Library.ElementId = System.Guid.NewGuid().ToString() |> Library.ElementId

let private mkElement (point : TablePoint) (kind : CatalogueKind) : Setups.SetupSlot =
    {
        id = freshId ()
        placement = ElementPlacement.create kind point
        zoom = defaultElementZoom
        beam = BeamLayout.OnMainBeam
        content = Setups.EmptySlotContent
    }

/// Composes the parameterless scene's in-memory stores from the Built-in catalogue (spec 0050 §3.4–3.5).
module private DefaultStores =
    open Softellect.Sys.Logging
    open OpticalConstructor.Domain.Library
    open OpticalConstructor.Domain.CatalogueImport
    open OpticalConstructor.Domain.CatalogueFiles
    open OpticalConstructor.Storage.CatalogueFolderStore
    open OpticalConstructor.Storage.CatalogueImportStore

    /// The default stores of a test scene.
    type DefaultStoreSet =
        {
            materials : MaterialLibrary.MaterialProxy
            samples : SampleProxy
            categories : MaterialLibrary.CategoryProxy
            library : LibraryProxy
            sources : SourceProxy
            catalogueImports : CatalogueImportContext
            itemModels : ItemModels.ItemModelProxy
        }

    /// Imports fresh scene stores, logging any failure and retaining entries already imported.
    let create () : DefaultStoreSet =
        let stores = CatalogueImportProxy.createInMemory ()
        let context = CatalogueImportContext.create stores.proxy
        let confirmed =
            context.importFrom (CatalogueProxy.createBuiltIn ())
            |> Result.bind (fun rows -> rows |> List.map _.byDefault |> context.apply)
        match confirmed with
        | Ok _ -> ()
        | Error e -> Logger.logError $"DefaultStores.create: could not import the Built-in catalogue: {e.describe}"
        let sources = SourceProxy.createInMemory ()
        {
            materials = stores.materials
            samples = stores.samples
            categories = stores.categories
            library = liveLibrary stores.libraryEntries stores.samples sources
            sources = sources
            catalogueImports = context
            itemModels = stores.itemModels
        }

/// The Main-screen "Lego" palette — the catalogue kinds the user can add to the scene. Shared by the
/// forward (`initMainWith`) and inverse (`initInverse`) Main scenes; the static test window seeds an
/// empty palette instead (the only behavioural difference between the test and Main scenes).
let private mainPalette : CatalogueKind list =
    [ LinearPolarizer; CircularPolarizer; Sample; Lens; FlatMirror; CurvedMirror; Detector ]

/// Seeds a scene in an initialized kit, with the supplied stores, elements and palette (spec 0050 .manual/010).
/// The static diagnostic supplies an empty palette; constructor scenes supply the add/remove kinds.
let initWith
    (library : Library.LibraryProxy)
    (experiments : Experiments.ExperimentProxy)
    (materials : MaterialLibrary.MaterialProxy)
    (samples : Library.SampleProxy)
    (categories : MaterialLibrary.CategoryProxy)
    (kit : KitContext)
    (elements : Setups.SetupSlot list)
    (palette : CatalogueKind list) : TableAndElementRotationModel =
    {
        table = Table.defaultTable
        view = Table.defaultView
        elements = elements
        selection = TableSelected
        drag = NotPressed
        tableR3Locked = RotationFree
        rotationConfirm = RotationControls.NoConfirm
        palette = palette
        render = RendererControls.defaultState
        // Spec 0046 (operator): the window opens on its FIRST bay, Add.
        ribbon = BayNames.add
        snapChain = Scene.FreePlacement
        library = library
        experiments = experiments
        experimentCollection = Experiments.ExperimentCollection.empty
        pendingEntry = None
        activeSelect = None
        selectStatus = None
        materials = materials
        samples = samples
        sources = Library.SourceProxy.createInMemory ()
        libraryBackups = LibraryBackup.LibraryBackupProxy.createInMemory ()
        catalogueImports = CatalogueImportContext.create (CatalogueImportProxy.createInMemory ()).proxy
        categories = categories
        launchers = EditorLaunchers.defaults
        quickPickThreshold = QuickPickThreshold.defaultValue
        selectWindowModality = SelectWindowModality.defaultValue
        constructorMode = ForwardConstructor
        kit = kit
        kitFolderOpt = None
        kitShell = KitWindowShell.inert
        kitChanges = []
        kitSend = KitArchiveJob.KitArchiveJobState.idle
        kitCopyOpt = None
        kitStatus = None
        namingTemplateText = kit.kitFile.namingTemplate.caption
        setupName = ControlCaption ""
        kitSetups = []
        activeSetupOpt = None
        kitItems = []
        slotChipOpenOpt = None
        setupGridOpt = None
        // Spec 0050 S7.2: a window opens on the table and its Add bay, as before the strip.
        flowStep = SetupsFlowStep
        itemModels = ItemModels.ItemModelProxy.createInMemory ()
        itemIds = ItemIdProvider.system
        selectedItemOpt = None
        addItemFormOpt = None
        parameterEditOpt = None
        experimentData = OpticalConstructor.Storage.ExperimentDataStore.createFileBacked kit.proxy
        experimentDataStatus = Map.empty
        elementMenu = None
        rotationPivot = BeamLayout.PivotAtCentre
        experimentExport = (ExperimentExport.createInMemoryExperimentExport ()).proxy
        exportStatus = None
    }

/// The STATIC test scene (Spec 0027, task 006 #3): a live table plus three fixed optical elements on
/// the central ray, no add/remove palette. Behaviour is unchanged from before — the palette is empty.
/// The proxies default to the in-memory mocks/stores (the test scene never shows the ribbon bays).
let initTableAndElementRotation () : Result<TableAndElementRotationModel, KitError> =
    scratchKit ()
    |> Result.map (fun kit ->
        let stores = DefaultStores.create ()
        let elements =
            [ mkElement { x = -0.5<meter>; y = 0.0<meter> } LinearPolarizer
              mkElement { x = 0.0<meter>; y = 0.0<meter> } Sample
              mkElement { x = 0.5<meter>; y = 0.0<meter> } FlatMirror ]
        {
            initWith stores.library (Experiments.createInMemoryExperiments ()) stores.materials stores.samples stores.categories kit elements [] with
                sources = stores.sources
                catalogueImports = stores.catalogueImports
                itemModels = stores.itemModels
        })

/// The DYNAMIC Main scene: the same table/view/selection/rotation logic, seeded with a light source and
/// a detector at the ends of the beam, plus the catalogue palette the user can add elements from (the
/// "Lego constructor"). The proxies — incl. the step-024 material / sample write seams — are injected
/// at the composition root. This is the Main screen — identical scene logic, elements added/removed at
/// runtime.
/// The Main-screen seed elements: a light source and a detector at the central-ray endpoints (the plate
/// edges), with DETERMINISTIC ids so wiring / experiment tests can name them. The forward Main scene uses
/// exactly this; the inverse scene splices an unbound sample between them (`initInverse`).
let private mainSeedElements : Setups.SetupSlot list =
    [
        {
            id = Library.ElementId "src"
            placement = ElementPlacement.create LightSource RayModel.defaultSourcePoint
            zoom = defaultElementZoom
            beam = BeamLayout.OnMainBeam
            content = Setups.EmptySlotContent
        }
        {
            id = Library.ElementId "det"
            placement = ElementPlacement.create Detector RayModel.defaultDetectorPoint
            zoom = defaultElementZoom
            beam = BeamLayout.OnMainBeam
            content = Setups.EmptySlotContent
        }
    ]

/// Seeds a forward scene in its supplied kit without constructing a scratch kit (spec 0050 .manual/010).
let initMainWithKit
    (library : Library.LibraryProxy)
    (experiments : Experiments.ExperimentProxy)
    (materials : MaterialLibrary.MaterialProxy)
    (samples : Library.SampleProxy)
    (categories : MaterialLibrary.CategoryProxy)
    (kit : KitContext) : TableAndElementRotationModel =
    {
        initWith library experiments materials samples categories kit mainSeedElements mainPalette with
            snapChain = Scene.SnapToBeam
    }

/// Opens a forward scene in a fresh scratch kit, preserving initialization errors (spec 0050 .manual/010).
let initMainWith
    (library : Library.LibraryProxy)
    (experiments : Experiments.ExperimentProxy)
    (materials : MaterialLibrary.MaterialProxy)
    (samples : Library.SampleProxy)
    (categories : MaterialLibrary.CategoryProxy) : Result<TableAndElementRotationModel, KitError> =
    scratchKit () |> Result.map (initMainWithKit library experiments materials samples categories)

/// Seeds an inverse scene with an unbound sample in its supplied kit (spec 0038 Part L; spec 0050 S4.3, .manual/010).
let initInverseWithKit
    (library : Library.LibraryProxy)
    (experiments : Experiments.ExperimentProxy)
    (materials : MaterialLibrary.MaterialProxy)
    (samples : Library.SampleProxy)
    (categories : MaterialLibrary.CategoryProxy)
    (kit : KitContext) : TableAndElementRotationModel =
    let elements : Setups.SetupSlot list =
        [
            {
                id = Library.ElementId "src"
                placement = ElementPlacement.create LightSource RayModel.defaultSourcePoint
                zoom = defaultElementZoom
                beam = BeamLayout.OnMainBeam
                content = Setups.EmptySlotContent
            }
            {
                id = Library.ElementId "sample"
                placement =
                    ElementPlacement.create Sample
                        {
                            x = 0.0<meter>
                            y = 0.0<meter>
                        }
                zoom = defaultElementZoom
                beam = BeamLayout.OnMainBeam
                content = Setups.EmptySlotContent
            }
            {
                id = Library.ElementId "det"
                placement = ElementPlacement.create Detector RayModel.defaultDetectorPoint
                zoom = defaultElementZoom
                beam = BeamLayout.OnMainBeam
                content = Setups.EmptySlotContent
            }
        ]
    {
        initWith library experiments materials samples categories kit elements mainPalette with
            snapChain = Scene.SnapToBeam
            constructorMode = InverseConstructor
    }

/// Opens an inverse scene with an unbound sample in a fresh scratch kit (spec 0038 Part L; spec 0050 .manual/010).
/// A bound hint restores the forward chart; initialization errors propagate to the caller.
let initInverse
    (library : Library.LibraryProxy)
    (experiments : Experiments.ExperimentProxy)
    (materials : MaterialLibrary.MaterialProxy)
    (samples : Library.SampleProxy)
    (categories : MaterialLibrary.CategoryProxy) : Result<TableAndElementRotationModel, KitError> =
    scratchKit ()
    |> Result.map (initInverseWithKit library experiments materials samples categories)

/// Opens the default Main scene, preserving scratch-kit initialization errors (spec 0050 .manual/010).
let initMain () : Result<TableAndElementRotationModel, KitError> =
    let stores = DefaultStores.create ()
    initMainWith stores.library (Experiments.createInMemoryExperiments ()) stores.materials stores.samples stores.categories
    |> Result.map (fun model ->
        {
            model with
                sources = stores.sources
                catalogueImports = stores.catalogueImports
                itemModels = stores.itemModels
        })

type TableAndElementRotationMsg =
    | RotateR1By of BarDegrees
    | RotateR2By of BarDegrees
    | RotateR3By of BarDegrees
    /// Lock / unlock the selection's R3 (the table's, or the selected element's).
    | ToggleR3Lock
    /// Spec 0046: lock / unlock the selected ELEMENT's R1 / R2 (the table has no R1 / R2 lock). The light
    /// source starts with all three rotations locked.
    | ToggleR1Lock
    | ToggleR2Lock
    /// Rotation-controls bar (task 008): set the selection's axis to an exact angle, and the
    /// confirmation-gated Reset (selection's rotations) / Reset All (every object's rotations).
    | RotSetAxis of RotationControls.RotationAxis * BarDegrees
    | RotRequestReset
    | RotRequestResetAll
    | RotConfirm
    | RotCancel
    | PointerDown of ScreenPoint
    | PointerMove of ScreenPoint
    | PointerUp of ScreenPoint
    | Wheel of Set<WheelModifier> * WheelNotches
    /// Lego constructor (Main screen): add a catalogue element to the scene (selected on add), or
    /// remove the currently-selected element. Never dispatched by the test windows (empty palette).
    /// Spec 0038 (018): `AddElement` DELEGATES to `AddElementBoundTo` with the kind's seeded
    /// default (`defaultSeedEntry`), so every new element except a sample lands PRE-BOUND.
    | AddElement of CatalogueKind
    /// Spec 0038 (018): add a catalogue element PRE-BOUND to an explicit seeded Library entry
    /// (`None` = unbound — the sample's inverse hook). The palette buttons dispatch this; the
    /// CPL / CPR pair shares the UNCHANGED CircularPolarizer kind and differs only here.
    | AddElementBoundTo of CatalogueKind * ElementValueId option
    | RemoveSelectedElement
    /// Main-screen MOVE bay: slide the selected element along the beam (its x), clamped to the plate
    /// (spec 0046: a detector is not clamped).
    | SlideSelectedBy of BarMeters
    | SlideSelectedTo of BarMeters
    | ResetSelectedPosition
    /// Spec 0046: move the selected element along a TABLE axis — the light source in x, y and z (the one
    /// element placed freely); any other element only along its beam (x). Lock-respecting.
    | MoveSelectedBy of TableAxis * BarMeters
    | MoveSelectedTo of TableAxis * BarMeters
    /// Spec 0046: lock / unlock the selected light source's move along a table axis (all start locked).
    | ToggleMoveLock of TableAxis
    /// Main-screen RENDER bay (task 018): tune how the elements are drawn.
    | RenderSwap
    | RenderSetRailsIndex of RendererControls.RendererPresetIndex
    | RenderSetCircles of RendererControls.CapCircles
    | RenderSetRadialsIndex of RendererControls.RendererPresetIndex
    | RenderSetRailOpacity of RendererControls.RendererOpacity
    | RenderSetFaceOpacity of RendererControls.RendererOpacity
    | RenderSetLineOpacity of RendererControls.RendererOpacity
    /// Main-screen ribbon: show this Bay (large control) by name.
    | SelectBay of Ribbon.BayName
    /// A press of a flow-strip step (spec 0050 S7.2).
    | ShowFlowStep of ConstructorFlowStep
    /// A press of an item row of the Items panel (spec 0050 S7.2).
    | SelectKitItem of ItemId
    /// The Items panel's "Add item from library model…": opens the add form under a newly minted id (spec 0050 S7.2).
    | OpenAddItemForm
    /// Chooses the model the add form's item instantiates (spec 0050 S7.2).
    | ChooseNewItemModel of ItemModelChoice
    /// Edits the add form's label as typed (spec 0050 S7.2).
    | SetNewItemLabel of ControlCaption
    /// Sets how the add form's item is mounted (spec 0050 S7.2).
    | SetNewItemDial of ItemDial
    /// The add form's Cancel: closes it, writing nothing (spec 0050 S7.2).
    | CancelAddItemForm
    /// Adds the item to the kit: the add form's Add, and how a test adds an item (spec 0050 S7.2; 001 §P).
    | AddItem of ItemDraft
    /// Sets an item's parameter state and records it: the editor's Set, and how a test sets one (spec 0050 S7.2; 001 §P).
    | SetParameterState of ItemId * ItemModels.ParameterName * ParameterState
    /// A press of a parameter row of the selected item: opens the parameter editor on it (spec 0050 S7.2).
    | EditItemParameter of ItemModels.ParameterName
    /// Chooses the state the open parameter editor sets (spec 0050 S7.2).
    | SetParameterEditChoice of ParameterStateChoice
    /// Edits one number of the open parameter editor as typed (spec 0050 S7.2).
    | SetParameterEditText of ParameterEditField * ControlCaption
    /// The parameter editor's Cancel: closes it, writing nothing (spec 0050 S7.2).
    | CancelParameterEdit
    /// Gives an item a new label and renames its kit file (spec 0050 S7.2).
    | RelabelKitItem of ItemId * ItemLabel
    /// Replaces an item's notes (spec 0050 S7.2).
    | SetKitItemNotes of ItemId * ItemNotes
    /// Gives an item the photo file the picker answered, attached as a lab file (spec 0050 S7.2, S4.2).
    | SetKitItemPhoto of ItemId * FileName
    /// Drops an item's photo, keeping its file (spec 0050 S7.2).
    | RemoveKitItemPhoto of ItemId
    /// Main-screen LIBRARY bay (task 024): bind this Library entry id to the selected element's
    /// `valueId` (inert when the table or nothing is selected).
    | BindValueId of ElementValueId
    /// Spec 0027 (026): the confirm-gated Library bind. `RequestBindValueId` SELECTS an entry as pending
    /// (shows its full description, no bind yet); `ConfirmBindValueId` COMMITS the pending entry to the
    /// selected element's `valueId`; `CancelBindValueId` clears the pending choice.
    | RequestBindValueId of ElementValueId
    | ConfirmBindValueId
    | CancelBindValueId
    /// Spec 0038 (016): the TARGETED bind a Select-state window's `onSelected` dispatches —
    /// commit the entry id to the `valueId` of the element with THIS serializable id (never
    /// "the current selection": the selection may have moved while the modeless window was
    /// open). A vanished element is a no-op plus the `selectStatus` line, never a throw.
    /// Spec 0038 (017): this is THE one commit site of `placement.valueId` — the quick-pick
    /// strip's confirm and the direct bind above DELEGATE here, so binding through the strip
    /// and through the Choose… Select window land the identical message.
    | BindValueIdTo of Library.ElementId * ElementValueId
    /// Spec 0038 (017): the Choose… flow's session bookkeeping. `SelectSessionStarted` stores
    /// the staleness handle of the Select-state window the verb just opened (or re-targeted);
    /// `SelectSessionEnded` clears exactly THAT session — reference-keyed — when the window
    /// ends it from its own side (the Close verb / title-bar X fire the session's
    /// `onCancelled`), so a session superseded by a re-target can never clear its successor.
    | SelectSessionStarted of SelectSession
    | SelectSessionEnded of SelectSession
    /// Main-screen EXPERIMENTS bay (spec 0027 / 028): the multi-step experiment editor + collection.
    /// Choose the element to vary (by id), the varied quantity, the T/R/both capture, and the range;
    /// commit (Add / Update), start a New draft, or Edit / Remove a collected experiment (by id).
    | ExpChooseElement of Library.ElementId
    | ExpChooseVariable of ExperimentControls.VariableChoice
    | ExpChooseMeasurement of ExperimentControls.MeasurementChoice
    | ExpSetRangeMin of ExperimentControls.ExperimentRangeEnd
    | ExpSetRangeMax of ExperimentControls.ExperimentRangeEnd
    | ExpSetRangePoints of ExperimentControls.ExperimentPointCount
    /// Add variable: the edited axis joins the draft's current block and the editor takes the next (spec 0050 S9.1).
    | ExpAddScanAxis
    /// Add block: the draft's current block closes on the capture the detectors allow and a new one starts (spec 0050 S9.1).
    | ExpAddScanBlock
    /// Spec 0046: set a TUNABLE source's wavelength for the draft run (clamped to its limits).
    | ExpSetSourceWaveLength of ExperimentControls.BayNanometers
    /// Spec 0046: export the draft's computed run through the `experimentExport` seam.
    | ExportExperimentData
    | ExpCommit
    | ExpNew
    | ExpEdit of Experiments.ExperimentId
    | ExpRemove of Experiments.ExperimentId
    /// Spec 0027 (026): open the pop-out interactive chart window for the current DRAFT (double-click on the
    /// inline chart, or the "Open chart" button).
    | OpenExperimentChartWindow
    /// Spec 0027 (030 follow-up): open the chart window for a COLLECTED experiment by id (the per-row "View"
    /// button, and a double-click on the experiment row).
    | ViewExperiment of Experiments.ExperimentId
    /// Records a picked file through the kit, then loads and validates its attachment (spec 0050 S4.2).
    | AttachDataFileTo of Experiments.ExperimentId * FileName
    /// Records that role for the experiment in its kit file (spec 0050 S9.1; 001 §P `SetExperimentRole`).
    | SetExperimentRole of Experiments.ExperimentId * Experiments.ExperimentRole
    /// Records the typed session for the experiment in its kit file, a blank text clearing it (spec 0050 S9.1).
    | SetExperimentSession of Experiments.ExperimentId * ControlCaption
    /// Records the typed notes for the experiment in its kit file (spec 0050 S9.1; 001 §E).
    | SetExperimentNotes of Experiments.ExperimentId * ControlCaption
    /// Edits the name the next created setup takes (spec 0050 S8.2).
    | SetSetupName of ControlCaption
    /// The table becomes a new setup of the kit, named by the field (spec 0050 S8.2; 001 §P `CreateSetup`).
    | CreateSetup
    /// Writes the table over the active setup (spec 0050 S8.2).
    | SaveSetup
    /// The kit's setup of that name is drawn on the table (spec 0050 S8.2).
    | ActivateSetup of SetupName
    /// Puts a kit item, or nothing, into that slot of the table (spec 0050 S8.2; 001 §P `AssignSlot`).
    | AssignSlot of Library.ElementId * ItemId option
    /// The table as the setup that holds this item in that slot, a held one or a new one (spec 0050 S8.2; 001 §P `SwapSlot`).
    | SwapSlot of Library.ElementId * ItemId
    /// Every ticked combination of the grid over the table, as setups and experiments of the kit (spec 0050 S8.3; 001 §P).
    | GenerateSetupGrid of SetupGrids.SetupGrid
    /// A new experiment of the draft over the active setup (spec 0050 S8.4; 001 §P `RecordExperiment`).
    | RecordExperiment
    /// Opens that slot's chip list, or closes it when it is the open one (spec 0050 S8.4).
    | ToggleSlotChip of Library.ElementId
    /// Opens the grid panel, keeping a draft already open (spec 0050 S8.4).
    | OpenSetupGrid
    /// Closes the grid panel, forgetting its draft (spec 0050 S8.4).
    | CloseSetupGrid
    /// Tries the item in that slot, or stops trying it (spec 0050 S8.4).
    | ToggleSetupGridItem of Library.ElementId * ItemId
    /// Tries that slot's items at this R1 too (spec 0050 S8.4).
    | AddSetupGridAngle of Library.ElementId * BarDegrees
    /// Stops trying that slot's items at that R1 (spec 0050 S8.4).
    | RemoveSetupGridAngle of Library.ElementId * Angle
    /// Unticks a ticked combination of the grid, or ticks it again (spec 0050 S8.4).
    | ToggleSetupGridCombination of SetupGrids.GridCombination
    /// The kit bar's forward / inverse switch, which keeps the kit and the model (spec 0050 S5.3).
    | SwitchConstructorMode of ConstructorMode
    /// The Kit menu's Show in Explorer: the kit's folder in the shell's explorer (spec 0050 S5.3).
    | ShowKitInExplorer
    /// The Kit menu's Close kit: the shell closes the window, and nothing is lost (spec 0050 S5.3; .spec-md §10.5).
    | CloseKit
    /// Offers the kit files changed outside the app since the stamps were last recorded (spec 0050 S5.3).
    | CheckKitChanges of KitCheckOrigin
    /// The offer's Reload: re-reads the kit file, experiments, items and setups into the model (spec 0050 S5.3, S8.2).
    | ReloadKitChanges
    /// The offer's Keep mine: keeps the model and records the current stamps, so the offer clears (spec 0050 S5.3).
    | KeepMyKitChanges
    /// The kit bar's naming-template field's text (spec 0050 S8.3).
    | SetNamingTemplateText of ControlCaption
    /// The kit bar's Save template: the kit's naming template becomes the field's (spec 0050 S8.3).
    | SaveNamingTemplate
    /// Sends the kit to this zip: the Send kit button's message after its save dialog, and how a test sends a kit (spec 0050 S6).
    | SendKit of FileName
    /// The running send zipped one more file (spec 0050 S6).
    | SendKitProgressed of JobRunner.JobProgress
    /// The running send ended: its file count, its cancel or why it failed (spec 0050 S6).
    | SendKitEnded of Result<KitArchiveOutcome, KitArchiveError>
    /// Cancels the running send before its next file; its end message returns the window to idle (spec 0050 S6).
    | CancelSendKit
    /// Opens the kit the folder dialog chose read-only and lists its experiments to copy (spec 0050 S6).
    | OpenCopySource of KitFolder
    /// Chooses the source's experiment to copy, or unchooses it when chosen (spec 0050 S6).
    | ToggleCopiedExperiment of Experiments.ExperimentId
    /// A press of a choice of the mapping table: how the source item of this id comes into the kit (spec 0050 S7.2).
    | ChooseCopiedItem of ItemId * ItemCopyChoice
    /// The copy panel's Copy: copies the chosen experiments into the kit and shows them (spec 0050 S6).
    | CopyChosenExperiments
    /// The copy panel's Cancel: closes the source and copies nothing (spec 0050 S6).
    | CloseCopySource
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
    /// Spec 0046 — the right-click ELEMENT MENU. A right click on the canvas selects the element under
    /// the point and opens its menu there (a right click off every element just closes it).
    | OpenElementMenuAt of ScreenPoint
    | CloseElementMenu
    /// Spec 0046: switch the element's reflected / transmitted ray on or off (the `Emission` smart
    /// setters — both-off stays unrepresentable; mirrors and a reflect-only bound sample are fixed).
    | ToggleElementRay of Library.ElementId * BeamTree.BeamBranch
    /// Spec 0046: move the element onto a beam — the main beam, or the reflected ray of a
    /// partially-reflecting element (a loop-forming or unavailable target is refused).
    | PlaceOnBeam of Library.ElementId * BeamLayout.BeamHost
    /// Spec 0046: the GLOBAL rotation pivot (the Rotation bay's checkbox) — about each element's centre, or
    /// about the middle of its incident face.
    | SetRotationPivot of BeamLayout.RotationPivot

// ---------------------------------------------------------------------------
// Constants.
// ---------------------------------------------------------------------------

let wheelStepDegrees : BarDegrees = BarDegrees 5.0
let private dragThresholdPx : CanvasLength = CanvasLength 3.0
/// A click within this many pixels of an element's projected centre selects that element; a click
/// further out selects the table.
let private elementSelectRadiusPx : CanvasLength = CanvasLength 50.0

[<Literal>]
let tableAndElementRotationCanvasWidth = 820.0
[<Literal>]
let tableAndElementRotationCanvasHeight = 560.0

// ---------------------------------------------------------------------------
// Pure update. Angles wrap mod 360.
// ---------------------------------------------------------------------------

type BarDegrees with

    /// The angle wrapped into [0, 360).
    member this.normalized : BarDegrees =
        let m = this.value % 360.0
        BarDegrees (if m < 0.0 then m + 360.0 else m)

    /// The angle as the model's `Angle`, wrapped into [0, 360) first.
    member this.wrappedAngle : Angle = Angle.degree (this.normalized.value * 1.0<deg>)

    /// `a` turned by this step, wrapped into [0, 360).
    member this.turning (a : Angle) : Angle = (BarDegrees (a.degrees + this.value)).wrappedAngle

type WheelNotches with

    /// The rotation `step` per notch turned by this many notches.
    member this.turnedBy (step : BarDegrees) : BarDegrees = BarDegrees (step.value * float this.value)

type BeamLayout.ElementZoom with

    /// The draw zoom after `notches` wheel steps of ×1.1, bounded [1, 50].
    member this.zoomedBy (notches : WheelNotches) : BeamLayout.ElementZoom =
        BeamLayout.ElementZoom (max 1.0 (min 50.0 (this.value * (1.1 ** float notches.value))))

type ScreenPoint with

    /// The screen distance to `other`, in pixels.
    member this.distanceTo (other : ScreenPoint) : CanvasLength =
        CanvasLength (sqrt ((this.sx - other.sx) ** 2.0 + (this.sy - other.sy) ** 2.0))

type ElementPlacement with

    /// The placement with its three rotations reset (bypassing the locks), its position kept.
    member p.withRotationsReset : ElementPlacement = { p with r1 = Angle.zero; r2 = Angle.zero; r3 = Angle.zero }

let private mapElement (i : SceneElementIndex) (f : Setups.SetupSlot -> Setups.SetupSlot) (m : TableAndElementRotationModel) : TableAndElementRotationModel =
    { m with elements = m.elements |> List.mapi (fun j e -> if j = i.value then f e else e) }

/// Rotate the TABLE VIEW about screen axis Rk by `d`. R3 respects the table's R3-lock (task 008).
let private rotateView (axis : RotationControls.RotationAxis) (d : BarDegrees) (m : TableAndElementRotationModel) : TableAndElementRotationModel =
    match axis, m.tableR3Locked with
    | RotationControls.R3, RotationLocked -> m
    | _ ->
        let v = m.view
        let v' =
            match axis with
            | RotationControls.R1 -> { v with r1 = d.turning v.r1 }
            | RotationControls.R2 -> { v with r2 = d.turning v.r2 }
            | RotationControls.R3 -> { v with r3 = d.turning v.r3 }
        { m with view = v' }

/// Rotate ELEMENT `i` about Rk by `d`, lock-respecting (R3 starts locked).
let private rotateElement (axis : RotationControls.RotationAxis) (d : BarDegrees) (i : SceneElementIndex) (m : TableAndElementRotationModel) : TableAndElementRotationModel =
    mapElement i
        (fun e ->
            let p = e.placement
            let p' =
                match axis with
                | RotationControls.R1 -> withR1 (d.turning p.r1) p
                | RotationControls.R2 -> withR2 (d.turning p.r2) p
                | RotationControls.R3 -> withR3 (d.turning p.r3) p
            { e with placement = p' })
        m

/// Rotate whatever is selected — the table view, or the selected element (task 006 #3).
let private rotateSelected (axis : RotationControls.RotationAxis) (d : BarDegrees) (m : TableAndElementRotationModel) : TableAndElementRotationModel =
    match m.selection with
    | TableSelected -> rotateView axis d m
    | ElementSelected i -> rotateElement axis d i m
    | NothingSelected -> m

let private zoomElement (i : SceneElementIndex) (notches : WheelNotches) (m : TableAndElementRotationModel) : TableAndElementRotationModel =
    mapElement i (fun e -> { e with zoom = e.zoom.zoomedBy notches }) m

let private zoomAllElements (notches : WheelNotches) (m : TableAndElementRotationModel) : TableAndElementRotationModel =
    { m with elements = m.elements |> List.map (fun e -> { e with zoom = e.zoom.zoomedBy notches }) }

/// The Main-screen snap (spec 0046 — the beam TREE): the source is the ray root and every other element
/// SNAPS onto the beam it sits on (`SetupSlot.beam`) — the main beam, or the reflected ray of a
/// partially-reflecting element — through the pure `BeamLayout.layout`. On each beam the elements are
/// ordered by x and consecutive x-distances are the gaps; each element's branch is DERIVED
/// (`ElementPlacement.primaryBranch` — a flat mirror REFLECTS, a polarizer / sample / lens TRANSMITS) and every
/// element is crossed by the ONE `RayModel.crossElement` law at its drawn depth. Returns each element's
/// drawn table-frame position BY INDEX and the drawn ray segments. For the free-placement test scene
/// (`snapChain = FreePlacement`) it is just each element at its own placement point, so selection / drawing
/// are unchanged there.
/// One element's Main-screen draw info: its snapped table-frame centre and the beam direction ARRIVING at
/// it (the source — the ray root — has no incoming).
type MainNode =
    {
        position : Vector3
        incoming : Vector3 option
    }

type TableAndElementRotationModel with

    /// The scene's elements, each with its index.
    member model.indexedElements : (SceneElementIndex * Setups.SetupSlot) list =
        model.elements |> List.mapi (fun i e -> SceneElementIndex i, e)

    /// The element at `i`, when the scene holds one there.
    member model.tryElementAt (i : SceneElementIndex) : Setups.SetupSlot option = List.tryItem i.value model.elements

    /// The index of the element with id `id`, when it is on the table.
    member model.tryIndexOf (id : Library.ElementId) : SceneElementIndex option =
        model.indexedElements |> List.tryPick (fun (i, e) -> if e.id = id then Some i else None)

    /// The scene as the pure beam layout reads it (spec 0046): every element with its beam host and draw zoom
    /// (the ray meets, and an open ray stops at, the DRAWN box), the source first.
    member model.layoutElements : BeamLayout.LayoutElement list =
        model.elements |> List.map (fun s -> s.layoutElement)

    /// The laid-out Main scene (spec 0046) — `BeamLayout.layoutWith` the global rotation pivot over
    /// `layoutElements`.
    member model.sceneLayout : BeamLayout.SceneLayout = BeamLayout.layoutWith model.rotationPivot model.layoutElements

    member model.mainSnap : Map<SceneElementIndex, MainNode> * BeamLayout.RaySegment list =
        match model.snapChain with
        | Scene.FreePlacement ->
            (model.indexedElements |> List.map (fun (i, e) -> i, { position = centreOf e.placement; incoming = None }) |> Map.ofList), []
        | Scene.SnapToBeam ->
            let laid = model.sceneLayout
            (laid.elements |> List.mapi (fun i p -> SceneElementIndex i, { position = p.centre; incoming = p.incoming }) |> Map.ofList), laid.segments

    /// Each element's drawn table-frame position by index (the Main scene snaps; the test scene is free).
    member model.snappedCentres : Map<SceneElementIndex, Vector3> = fst model.mainSnap |> Map.map (fun _ n -> n.position)

    /// A click selects the nearest element (by its DRAWN position) within the select radius; else the table if
    /// the click landed on the plate; else nothing — clicking outside the table unselects it.
    member internal m.selectionAt (pt : ScreenPoint) : Selection =
        let centres = m.snappedCentres
        let nearest =
            m.indexedElements
            |> List.map (fun (i, _) -> i, (pt.distanceTo (TableScene.projectOnScene m.view centres.[i])).value)
            |> List.sortBy snd
            |> List.tryHead
        match nearest with
        | Some (i, d) when d <= elementSelectRadiusPx.value -> ElementSelected i
        | _ ->
            match TableScene.tableHitOnScene m.view m.table pt with
            | InsidePolygon -> TableSelected
            | OutsidePolygon -> NothingSelected

    /// Reset rotations only (bypassing locks), keeping pan / zoom / position (task 008).
    member internal m.withViewRotationsReset : TableAndElementRotationModel = { m with view = { m.view with r1 = Angle.zero; r2 = Angle.zero; r3 = Angle.zero } }

    member internal m.withSelectionRotationsReset : TableAndElementRotationModel =
        match m.selection with
        | TableSelected -> m.withViewRotationsReset
        | ElementSelected i -> mapElement i (fun e -> { e with placement = e.placement.withRotationsReset }) m
        | NothingSelected -> m

    /// Reset All: every object's rotations — the table view AND all elements (task 008, your call).
    member internal m.withAllRotationsReset : TableAndElementRotationModel =
        { m.withViewRotationsReset with elements = m.elements |> List.map (fun e -> { e with placement = e.placement.withRotationsReset }) }

/// Set the selection's axis to an exact angle (lock-respecting for R3, task 008).
let private setSelectedAxis (axis : RotationControls.RotationAxis) (v : BarDegrees) (m : TableAndElementRotationModel) : TableAndElementRotationModel =
    let a = v.wrappedAngle
    match m.selection with
    | TableSelected ->
        match axis with
        | RotationControls.R1 -> { m with view = { m.view with r1 = a } }
        | RotationControls.R2 -> { m with view = { m.view with r2 = a } }
        | RotationControls.R3 ->
            match m.tableR3Locked with
            | RotationLocked -> m
            | RotationFree -> { m with view = { m.view with r3 = a } }
    | ElementSelected i ->
        mapElement i (fun e -> { e with placement = (match axis with RotationControls.R1 -> withR1 a | RotationControls.R2 -> withR2 a | RotationControls.R3 -> withR3 a) e.placement }) m
    | NothingSelected -> m

// ---------------------------------------------------------------------------
// Lego constructor (Main screen): add / remove elements at runtime. The scene logic is otherwise
// identical to the static test scene — these just grow / shrink the `elements` list.
// ---------------------------------------------------------------------------

type CatalogueKind with

    /// Spec 0038 (018) pre-binding (spec Part G, operator Q7/Q28): the seeded Library entry a NEW
    /// table element of this kind is created PRE-BOUND to. A sample stays UNBOUND — the inverse-flow
    /// hook — and the lens / mirror kinds have no Library entries to bind (not bindable). The
    /// circular-polarizer kind defaults to the LEFT seed (the seed order); the palette's CPR button
    /// carries its explicit `PresetIds.polarizerCpRight` pre-bind through `AddElementBoundTo`.
    member kind.defaultSeedEntry : ElementValueId option =
        match kind with
        | LightSource -> Some Library.PresetIds.source600
        | Detector -> Some Library.PresetIds.detectorIntensity
        | LinearPolarizer -> Some Library.PresetIds.polarizerLp
        | CircularPolarizer -> Some Library.PresetIds.polarizerCpLeft
        | Sample -> None
        | Lens | FlatMirror | CurvedMirror -> None

    /// Whether an element of this kind AUTO-ORIENTS to the beam it sits on. Spec 0046: every element except
    /// the SOURCE (the ray root, which emits along its own aim) — mirrors included — has its R2 / R3 measured
    /// FROM its incoming beam (A.4.3), and that one orientation is both what is drawn and the face the ray
    /// crosses (`RayModel.orientedToBeam`), so a ray never reflects or refracts off a surface other than the
    /// drawn one.
    member kind.autoOrientsToBeam : bool =
        match kind with
        | LightSource -> false
        | LinearPolarizer | CircularPolarizer | Sample | Lens | FlatMirror | CurvedMirror | Detector -> true

type RayPositionControls.PositionRange with

    /// `v` held to this range.
    member range.clampedPosition (v : BarMeters) : BarMeters =
        match range with
        | RayPositionControls.BoundedRange span -> BarMeters (max span.lowest.value (min span.highest.value v.value))
        | RayPositionControls.AtLeast lowest -> BarMeters (max lowest.value v.value)
        | RayPositionControls.UnboundedRange -> v

/// How far along a ray an element lands past the element it follows — moved onto a reflected ray from
/// behind its new host, or added at the end of a ray (spec 0046).
let sideBeamGap : BarMeters = BarMeters 0.3

type TableAndElementRotationModel with

    /// Spec 0038 (016) staleness: cancel and CLOSE the open Select-state window session, if any
    /// (closing the window fires the session's `onCancelled` through its own dismissal hook).
    /// Invoked wherever the table selection changes or the session's target can disappear — the
    /// pending-bind-clears precedent (`PointerUp` below).
    member internal m.withActiveSelectCancelled : TableAndElementRotationModel =
        match m.activeSelect with
        | Some session ->
            session.cancelAndClose ()
            { m with activeSelect = None }
        | None -> m

    /// A position of the DEFAULT scene's beam, read against where the ray root (the source) now is (spec 0046):
    /// the ray moves rigidly with the source, so `defaultX` — an along-beam coordinate measured with the source
    /// at its default point — is carried by however far the source has since moved along x.
    member m.alongFromSource (defaultX : float) : float =
        let defaultSourceX = RayModel.defaultSourcePoint.x / 1.0<meter>
        let sourceX = m.elements |> List.tryHead |> Option.map (fun e -> e.elementX) |> Option.defaultValue defaultSourceX
        sourceX + (defaultX - defaultSourceX)

    /// Where a ray with no detector on it ends, along the beam (spec 0046): 1 m in the default scene (the
    /// default detector point, 2 m past the source) — carried with the source, like the whole ray.
    member m.openRayEndX : float = m.alongFromSource (RayModel.defaultDetectorPoint.x / 1.0<meter>)

    /// Where `ray` starts, along the beam (spec 0046): at the element it leaves — the source for the main
    /// beam, the host for a reflected ray.
    member m.rayStartX (ray : BeamLayout.BeamHost) : BarMeters =
        match ray with
        | BeamLayout.OnMainBeam -> m.elements |> List.tryHead
        | BeamLayout.OnReflectedBeamOf hostId -> m.elements |> List.tryFind (fun e -> e.id = hostId)
        |> Option.map (fun e -> BarMeters e.elementX)
        |> Option.defaultValue (BarMeters 0.0)

    /// The span of `ray` an element other than element `except` is held to (spec 0046 — one rule, the table
    /// plays no part): from the ray's start to its FIRST detector (other than `except`), or to 1 m along
    /// (`openRayEndX`) when no detector sits on it — never ending before it starts.
    member m.raySpan (ray : BeamLayout.BeamHost) (except : SceneElementIndex option) : RayPositionControls.PositionSpan =
        let start = (m.rayStartX ray).value
        let detectors =
            List.zip m.indexedElements m.sceneLayout.elements
            |> List.choose (fun ((j, e), placed) ->
                match e.placement.catalogueKind with
                | Detector when j.value > 0 && Some j <> except && placed.beam = ray -> Some e.elementX
                | LightSource | LinearPolarizer | CircularPolarizer | Sample | Lens | FlatMirror | CurvedMirror | Detector -> None)
        let finish =
            match detectors with
            | [] -> m.openRayEndX
            | xs -> List.min xs
        { lowest = BarMeters start; highest = BarMeters (max start finish) }

    /// Where a detector added to `ray` lands (spec 0046): at the end of the open ray (1 m along), or just past
    /// the last element on it when that lies further.
    member m.rayEndX (ray : BeamLayout.BeamHost) : BarMeters =
        let last =
            List.zip m.indexedElements m.sceneLayout.elements
            |> List.choose (fun ((i, e), placed) -> if i.value > 0 && placed.beam = ray then Some e.elementX else None)
            |> List.fold max (m.rayStartX ray).value
        BarMeters (max m.openRayEndX (last + sideBeamGap.value))

/// Spec 0040 Part D.3: a placed sample's emission is bounded by the SAMPLE it is bound to, not the
/// generic per-kind `defaultEmission` — a bound `Mirror` sample reflects only, a bound `ThinFilm` /
/// `Plate` emits both (or the R/T-only it was constrained to). Resolve the placement's `valueId` through the
/// Library proxy and, when it binds a sample entry, seed the emission from that sample's
/// geometry-constrained `supportedEmission` (`Library.placementEmissionForEntry`); a placement bound
/// to a non-sample entry, an unknown id, or nothing keeps the emission its kind already gave it. This
/// is the single seed point both `valueId` writes (the pre-bound add and the targeted bind) route
/// through, so the experiment's default `MeasurementMode` (which reads `placement.emission`) follows.
let private withBoundSampleEmission (m : TableAndElementRotationModel) (p : ElementPlacement) : ElementPlacement =
    match p.valueId with
    | Some id ->
        match m.library.tryGetEntry id with
        | Ok (Some entry) -> { p with emission = Library.placementEmissionForEntry p.emission entry }
        | Ok None | Error _ -> p
    | None -> p

/// Append a catalogue element to the scene and select it. New elements are spread along the beam so
/// they do not land exactly on top of one another; the user then rotates / configures the selection.
/// Selecting the new element IS a table-selection change, so an open Select session cancels (016).
/// Spec 0038 (018): the new element lands PRE-BOUND to `prebind` (the kind's seeded default, or
/// the palette button's explicit entry) — `None` keeps it unbound (the sample's inverse hook). Spec
/// 0040 (012): a pre-bound sample seeds its emission from that sample (`withBoundSampleEmission`).
let private addElement (kind : CatalogueKind) (prebind : ElementValueId option) (m : TableAndElementRotationModel) : TableAndElementRotationModel =
    let middleCount =
        m.elements
        |> List.filter (fun e -> e.placement.catalogueKind <> LightSource && e.placement.catalogueKind <> Detector)
        |> List.length
    // Spread along the beam from the default scene's -0.3 m, carried with the source (spec 0046).
    let defaultX = BarMeters (m.alongFromSource (-0.3 + 0.2 * float middleCount))
    // Spec 0046: a DETECTOR goes to the END of the first lit ray that no detector terminates yet (the
    // main beam, then the reflected rays in the order the light reaches their hosts); only when every
    // ray already ends on a detector does it go to the main beam, where it terminates the beam at its
    // position (a detector behind it then receives nothing).
    let host, x =
        match kind with
        | Detector ->
            match m.sceneLayout.firstFreeRay with
            | Some ray -> ray, m.rayEndX ray
            | None -> BeamLayout.OnMainBeam, defaultX
        | LightSource | LinearPolarizer | CircularPolarizer | Sample | Lens | FlatMirror | CurvedMirror ->
            // Held to the main beam's span (spec 0046), like any slide along it.
            BeamLayout.OnMainBeam, (RayPositionControls.BoundedRange (m.raySpan BeamLayout.OnMainBeam None)).clampedPosition defaultX
    let placement =
        { ElementPlacement.create kind { x = x.value * 1.0<meter>; y = 0.0<meter> } with valueId = prebind }
        |> withBoundSampleEmission m
    let e : Setups.SetupSlot =
        {
            id = freshId ()
            placement = placement
            zoom = defaultElementZoom
            beam = host
            content = Setups.EmptySlotContent
        }
    let elements' = m.elements @ [ e ]
    { m.withActiveSelectCancelled with elements = elements'; selection = ElementSelected (SceneElementIndex (List.length elements' - 1)) }

// ---------------------------------------------------------------------------
// Main-screen MOVE bay (spec 0046 — one rule, the table plays no part): an element slides along its RAY,
// which starts at the element it leaves and ends at its first detector (1 m along with none); a detector
// slides anywhere past the start, extending its ray; the light source — the one element placed freely —
// moves in x, y and z.
// ---------------------------------------------------------------------------

/// How far element `i` may slide along its ray (spec 0046): a DETECTOR anywhere from the ray's start on
/// (moving it extends the ray), every other element within `raySpan`. The light source is placed freely.
let alongBeamRange (m : TableAndElementRotationModel) (i : SceneElementIndex) : RayPositionControls.PositionRange =
    match m.tryElementAt i, List.tryItem i.value m.sceneLayout.elements with
    | Some e, Some placed when i.value > 0 ->
        match e.placement.catalogueKind with
        | Detector -> RayPositionControls.AtLeast (m.rayStartX placed.beam)
        | LightSource | LinearPolarizer | CircularPolarizer | Sample | Lens | FlatMirror | CurvedMirror ->
            RayPositionControls.BoundedRange (m.raySpan placed.beam (Some i))
    | Some _, Some _ | Some _, None | None, _ -> RayPositionControls.UnboundedRange

/// Carry the whole ray along x after the ray root (element 0, the source) moved there (spec 0046): every
/// other element's along-beam x shifts by the root's own shift `before` → `after`, so every gap is kept
/// and an x move of the source moves the ray and all its elements with it — exactly as its y / z moves
/// and its rotations do (those reach the elements through the beam itself).
let private carryRayWithRoot (before : TableAndElementRotationModel) (after : TableAndElementRotationModel) : TableAndElementRotationModel =
    match before.elements, after.elements with
    | root :: _, root' :: _ ->
        let dx = root'.elementX - root.elementX
        { after with
            elements =
                after.elements
                |> List.mapi (fun j e ->
                    if j = 0 then e
                    else { e with placement = { e.placement with placementPoint = { e.placement.placementPoint with x = e.placement.placementPoint.x + dx * 1.0<meter> } } }) }
    | _ -> after

/// Move the selected element to `v` along a table axis (spec 0046), lock-respecting: the light source along
/// any axis, freely; any other element only along its beam (x), within `alongBeamRange` — its y and z come
/// from the beam it is snapped to.
let private moveSelectedTo (axis : TableAxis) (v : BarMeters) (m : TableAndElementRotationModel) : TableAndElementRotationModel =
    match m.selection with
    | ElementSelected i ->
        match m.tryElementAt i with
        | Some element ->
            let moveTo (range : RayPositionControls.PositionRange) : TableAndElementRotationModel =
                mapElement i (fun e -> { e with placement = withPositionOn axis ((range.clampedPosition v).value * 1.0<meter>) e.placement }) m
            match element.placement.catalogueKind.mobility, axis with
            | MovesFreely, TableX when i.value = 0 -> carryRayWithRoot m (moveTo RayPositionControls.UnboundedRange)
            | MovesFreely, _ -> moveTo RayPositionControls.UnboundedRange
            | MovesAlongBeam, TableX -> moveTo (alongBeamRange m i)
            | MovesAlongBeam, (TableY | TableZ) -> m
        | None -> m
    | TableSelected | NothingSelected -> m

let private moveSelectedBy (axis : TableAxis) (d : BarMeters) (m : TableAndElementRotationModel) : TableAndElementRotationModel =
    match m.selection with
    | ElementSelected i ->
        match m.tryElementAt i with
        | Some e -> moveSelectedTo axis (BarMeters (positionOn axis e.placement / 1.0<meter> + d.value)) m
        | None -> m
    | TableSelected | NothingSelected -> m

/// Lock / unlock the selected light source's move along a table axis (spec 0046); inert for an element
/// snapped to its beam.
let private toggleMoveLock (axis : TableAxis) (m : TableAndElementRotationModel) : TableAndElementRotationModel =
    match m.selection with
    | ElementSelected i ->
        match m.tryElementAt i |> Option.map (fun e -> e.placement.catalogueKind.mobility) with
        | Some MovesFreely ->
            mapElement i (fun e ->
                let lock =
                    match moveLockOf axis e.placement with
                    | MoveLocked -> MoveFree
                    | MoveFree -> MoveLocked
                { e with placement = withMoveLock axis lock e.placement }) m
        | Some MovesAlongBeam | None -> m
    | TableSelected | NothingSelected -> m

type TableAndElementRotationModel with

    /// Remove the currently-selected element (inert when the table or nothing is selected). Selection
    /// drops to nothing so the bar disables until the user picks another object. The removed element
    /// may be an open Select session's target — the target's disappearance cancels and closes the
    /// session's window (spec 0038 step 016 staleness).
    member internal m.withSelectedRemoved : TableAndElementRotationModel =
        match m.selection with
        | ElementSelected i ->
            match m.tryElementAt i with
            | Some removedElement ->
                let removed = removedElement.id
                // Spec 0046: the elements sitting on the removed element's reflected ray move to the main beam
                // (the ray they sat on is gone), and its open element menu closes.
                let elements' =
                    m.indexedElements
                    |> List.filter (fun (j, _) -> j <> i)
                    |> List.map (fun (_, e) ->
                        match e.beam with
                        | BeamLayout.OnReflectedBeamOf host when host = removed -> { e with beam = BeamLayout.OnMainBeam }
                        | BeamLayout.OnReflectedBeamOf _ | BeamLayout.OnMainBeam -> e)
                { m.withActiveSelectCancelled with elements = elements'; selection = NothingSelected; elementMenu = None }
            | None -> m
        | TableSelected | NothingSelected -> m

    /// Reset the selected element's position (spec 0046), bypassing the move locks like the rotations' Reset:
    /// the light source back to its default point in the table plane, carrying the ray with it; a detector to
    /// 1 m along and anything else to 0 m — both the default scene's positions, read against the source
    /// (`alongFromSource`) and held to the element's ray (`alongBeamRange`).
    member internal m.withSelectedPositionReset : TableAndElementRotationModel =
        match m.selection with
        | ElementSelected i when Option.isSome (m.tryElementAt i) ->
            let range = alongBeamRange m i
            let reset =
                mapElement i (fun e ->
                    let p = e.placement
                    let alongTo (x : float) : ElementPlacement =
                        { p with placementPoint = { p.placementPoint with x = (range.clampedPosition (BarMeters x)).value * 1.0<meter> } }
                    let home =
                        match p.catalogueKind with
                        | LightSource -> { p with placementPoint = RayModel.defaultSourcePoint; height = 0.0<meter> }
                        | Detector -> alongTo m.openRayEndX
                        | LinearPolarizer | CircularPolarizer | Sample | Lens | FlatMirror | CurvedMirror -> alongTo (m.alongFromSource 0.0)
                    { e with placement = home }) m
            if i.value = 0 then carryRayWithRoot m reset else reset
        | ElementSelected _ | TableSelected | NothingSelected -> m

// ---------------------------------------------------------------------------
// Spec 0046 — the element menu's verbs: switch an element's reflected / transmitted rays and move it
// between beams. The rules themselves are the pure `BeamLayout` ones; these only apply them to the model.
// ---------------------------------------------------------------------------

/// What an element's binding lets its rays be (spec 0046): a bound sample's geometry-constrained
/// `supportedEmission` (`Library.placementEmissionForEntry`), `EmitBoth` for anything else.
let supportedEmissionOf (m : TableAndElementRotationModel) (p : ElementPlacement) : Emission =
    match p.valueId with
    | Some id ->
        match m.library.tryGetEntry id with
        | Ok (Some entry) -> Library.placementEmissionForEntry EmitBoth entry
        | Ok None | Error _ -> EmitBoth
    | None -> EmitBoth

/// Switch element `id`'s reflected or transmitted ray. The elements sitting on a reflected ray that is
/// switched off KEEP their host and fall back to the main beam while it is off (`BeamLayout.effectiveHosts`),
/// so switching it back on restores them.
let private toggleElementRay (id : Library.ElementId) (branch : BeamTree.BeamBranch) (m : TableAndElementRotationModel) : TableAndElementRotationModel =
    match m.tryIndexOf id with
    | Some i -> mapElement i (fun e -> { e with placement = BeamLayout.toggleRay branch (supportedEmissionOf m e.placement) e.placement }) m
    | None -> m

/// Move element `id` onto `host` (spec 0046). The root (the source) never moves; a reflected ray is
/// accepted only when `BeamLayout.attachableHosts` offers it (it exists and would not loop). An element
/// moved onto a reflected ray from at or behind its host's along-beam position lands `sideBeamGap` along
/// that ray, so it does not collapse onto the host.
let private placeOnBeam (id : Library.ElementId) (host : BeamLayout.BeamHost) (m : TableAndElementRotationModel) : TableAndElementRotationModel =
    match m.tryIndexOf id with
    | Some i when i.value > 0 ->
        match host with
        | BeamLayout.OnMainBeam -> mapElement i (fun e -> { e with beam = BeamLayout.OnMainBeam }) m
        | BeamLayout.OnReflectedBeamOf hostId ->
            match List.contains hostId (BeamLayout.attachableHosts m.layoutElements id), m.elements |> List.tryFind (fun e -> e.id = hostId) with
            | true, Some hostElement ->
                let hostX = hostElement.elementX
                let onRay =
                    mapElement i (fun e ->
                        let x = if e.elementX > hostX then e.elementX else hostX + sideBeamGap.value
                        { e with
                            beam = host
                            placement = { e.placement with placementPoint = { e.placement.placementPoint with x = x * 1.0<meter> } } }) m
                // Held to the span of the ray it now sits on (spec 0046).
                let range = alongBeamRange onRay i
                mapElement i (fun e -> { e with placement = { e.placement with placementPoint = { e.placement.placementPoint with x = (range.clampedPosition (BarMeters e.elementX)).value * 1.0<meter> } } }) onRay
            | _ -> m
    | Some _ | None -> m

/// Spec 0027 (026) Part 3: the side-effecting "open the pop-out chart window" action, injected as a forward
/// reference so `update` can invoke it before `experimentResult` (and the `ChartWindow` host) are defined
/// further down the module. The real implementation is assigned right after `experimentResult`; under a
/// headless test host (which never dispatches `OpenExperimentChartWindow`) the default no-op is harmless.
let mutable private openChartWindowHook : TableAndElementRotationModel -> unit = fun _ -> ()

/// Spec 0046: the draft's computed run, as a forward reference (`experimentResult` is defined after `update`);
/// assigned right after it. The default empty chart is what an unassigned hook yields.
let mutable private draftChartHook : TableAndElementRotationModel -> ExperimentChart.ExperimentChart = fun _ -> ExperimentChart.emptyExperimentChart

type ControlCaption with

    /// A chart caption as the text its exported run carries (spec 0048 E017).
    member private this.exportText : ExperimentExport.ExportText = ExperimentExport.ExportText this.value

/// Spec 0046: export the draft's computed run through the `experimentExport` seam and report the outcome.
let private exportDraftRun (model : TableAndElementRotationModel) : TableAndElementRotationModel =
    let chart = draftChartHook model
    let run : ExperimentExport.ExportedRun =
        {
            title = chart.title.exportText
            xLabel = chart.xLabel.exportText
            yLabel = chart.yLabel.exportText
            description = chart.description.exportText
            series =
                chart.series
                |> List.map (fun series ->
                    { ExperimentExport.ExportedSeries.name = series.name.exportText; points = series.points |> List.map (fun p -> { Propagation.CurvePoint.x = p.x; y = p.y }) })
        }
    match model.experimentExport.exportRun run with
    | Ok () -> { model with exportStatus = Some (ControlCaption $"Exported {List.length run.series} series ({run.title.value}).") }
    | Error e -> { model with exportStatus = Some (ControlCaption $"Nothing exported — {e.reason}") }

/// Spec 0027 (030 follow-up): the side-effecting "open the chart window for a COLLECTED experiment (by id)"
/// action — the per-row "View" button and a row double-click both invoke it. Same forward-reference seam.
let mutable private viewExperimentHook : TableAndElementRotationModel -> Experiments.ExperimentId -> unit = fun _ _ -> ()

// Spec 0027 (028): the Controls-layer ⇄ domain mirror maps for the experiment variable / measurement DUs
// (the bay is domain-free, so it mirrors the domain cases and the host maps them back).

type ExperimentControls.VariableChoice with

    member private v.variableParameter : Experiments.VariableParameter =
        match v with
        | ExperimentControls.VaryWaveLength -> Experiments.VaryWaveLength
        | ExperimentControls.VaryR1 -> Experiments.VaryR1
        | ExperimentControls.VaryR2 -> Experiments.VaryR2
        | ExperimentControls.VaryDialReading -> Experiments.VaryDialReading

type Experiments.VariableParameter with

    member private v.bayChoice : ExperimentControls.VariableChoice =
        match v with
        | Experiments.VaryWaveLength -> ExperimentControls.VaryWaveLength
        | Experiments.VaryR1 -> ExperimentControls.VaryR1
        | Experiments.VaryR2 -> ExperimentControls.VaryR2
        | Experiments.VaryDialReading -> ExperimentControls.VaryDialReading

type ExperimentControls.MeasurementChoice with

    member internal m.measurementMode : Experiments.MeasurementMode =
        match m with
        | ExperimentControls.CaptureT -> Experiments.CaptureTransmitted
        | ExperimentControls.CaptureR -> Experiments.CaptureReflected
        | ExperimentControls.CaptureBoth -> Experiments.CaptureBoth

type Experiments.MeasurementMode with

    member internal m.bayChoice : ExperimentControls.MeasurementChoice =
        match m with
        | Experiments.CaptureTransmitted -> ExperimentControls.CaptureT
        | Experiments.CaptureReflected -> ExperimentControls.CaptureR
        | Experiments.CaptureBoth -> ExperimentControls.CaptureBoth

type WaveLength with

    /// The wavelength in nanometres.
    member private w.nanometers : float = w.value / nmToMeter / oneNanometer

type TableAndElementRotationModel with

    /// The scene's light source entry (spec 0046): the first light-source element bound to a source entry.
    member model.boundSource : Library.SourcePreset option =
        model.elements
        |> List.tryPick (fun e ->
            match e.placement.catalogueKind, e.placement.valueId with
            | LightSource, Some id ->
                match model.library.tryGetEntry id with
                | Ok (Some (Library.SourceItem source)) -> Some source
                | Ok (Some _) | Ok None | Error _ -> None
            | (LightSource | LinearPolarizer | CircularPolarizer | Sample | Lens | FlatMirror | CurvedMirror | Detector), _ -> None)

/// The wavelength a run uses when it does not vary it (spec 0046): a TUNABLE source's setting for the run
/// (`requested`, clamped to its limits) or else its own setting; a fixed line's wavelength; 600 nm with no
/// bound source (an absent source is defaulted, never synthesized).
let operatingWaveLength (model : TableAndElementRotationModel) (requested : WaveLength option) : WaveLength =
    match model.boundSource with
    | Some source ->
        match source.tuning, requested with
        | Library.TunableBetween limits, Some w ->
            WaveLength.nm (max limits.lower.nanometers (min limits.upper.nanometers w.nanometers) * 1.0<nm>)
        | Library.TunableBetween _, None
        | Library.FixedLine, _ -> source.waveLength
    | None -> WaveLength.nm 600.0<nm>

/// What element `e` lets an experiment vary (spec 0046): its slot's scan variables, a dial-mounted item's reading in R1's
/// place (spec 0050 S9.1) — except a light source, whose wavelength varies only when the bound source is TUNABLE (a fixed
/// line, or no bound source, varies nothing).
let variablesOfElement (model : TableAndElementRotationModel) (e : Setups.SetupSlot) : Experiments.VariableParameter list =
    match e.placement.catalogueKind with
    | LightSource ->
        match model.boundSource |> Option.map (fun source -> source.tuning) with
        | Some (Library.TunableBetween _) -> [ Experiments.VaryWaveLength ]
        | Some Library.FixedLine | None -> []
    | LinearPolarizer | CircularPolarizer | Sample | Lens | FlatMirror | CurvedMirror | Detector -> e.scanVariablesWith model.kitItems

/// Keep a wavelength sweep inside a tunable source's limits (spec 0046); any other range as it is.
let private clampSweep (model : TableAndElementRotationModel) (range : Experiments.VariableRange) : Experiments.VariableRange =
    match model.experimentCollection.draft.variable, model.boundSource |> Option.bind (fun source -> source.tunableLimitsOpt) with
    | Some Experiments.VaryWaveLength, Some limits ->
        let lo, hi = limits.lower.nanometers, limits.upper.nanometers
        { range with min = Propagation.SweptValue (max lo (min hi range.min.value)); max = Propagation.SweptValue (max lo (min hi range.max.value)) }
    | Some Experiments.VaryWaveLength, None
    | Some (Experiments.VaryR1 | Experiments.VaryR2 | Experiments.VaryDialReading), _
    | None, _ -> range

type TableAndElementRotationModel with

    /// A wavelength sweep's range for the scene's source (spec 0046): a tunable source's limits; else the generic
    /// default.
    member internal model.wavelengthRange : Experiments.VariableRange =
        match model.boundSource |> Option.bind (fun source -> source.tunableLimitsOpt) with
        | Some limits ->
            { Experiments.VariableRange.forVariable Experiments.VaryWaveLength with
                min = Propagation.SweptValue limits.lower.nanometers
                max = Propagation.SweptValue limits.upper.nanometers }
        | None -> Experiments.VariableRange.forVariable Experiments.VaryWaveLength

    /// The objects (spec 0050 S8.1): the elements bound to a library sample, and the sample slots holding an item, so a
    /// sample item is an object of the capture rule (spec 0050 S8.2), in scene order.
    member internal model.objectIds : Library.ElementId list =
        model.elements
        |> List.choose (fun e ->
            match e.content, e.placement.catalogueKind, e.placement.valueId with
            | Setups.ItemInSlot _, Sample, _ -> Some e.id
            | _, _, Some id ->
                match model.library.tryGetEntry id with
                | Ok (Some (Library.SampleItem _)) -> Some e.id
                | Ok (Some _) | Ok None | Error _ -> None
            | _, _, None -> None)

    /// The scene's detector elements, in scene order.
    member internal model.detectorIds : Library.ElementId list =
        model.elements
        |> List.choose (fun e ->
            match e.placement.catalogueKind with
            | Detector -> Some e.id
            | LightSource | LinearPolarizer | CircularPolarizer | Sample | Lens | FlatMirror | CurvedMirror -> None)

    /// Each captured detector's branch and scene index, in scene order (`BeamLayout.detectorCaptures`): a detector
    /// captures the branch by which its light leaves the last object on its path (spec 0050 S8.1).
    member model.captureDetectors (varied : Library.ElementId) : (BeamTree.BeamBranch * SceneElementIndex) list =
        BeamLayout.detectorCaptures model.sceneLayout model.detectorIds varied model.objectIds
        |> List.choose (fun capture ->
            model.indexedElements
            |> List.tryFind (fun (_, e) -> e.id = capture.detector)
            |> Option.map (fun (j, _) -> capture.branch, j))

    /// What a detector element reads (spec 0046): its bound detector entry's kind — an unbound detector reads
    /// intensity.
    member model.detectorKindAt (j : SceneElementIndex) : Library.DetectorKind =
        match (List.item j.value model.elements).placement.valueId with
        | Some id ->
            match model.library.tryGetEntry id with
            | Ok (Some (Library.DetectorItem d)) -> d.kind
            | Ok (Some _) | Ok None | Error _ -> Library.Intensity
        | None -> Library.Intensity

/// Spec 0046 (task 5): the capture modes the scene's detectors allow for an experiment varying `varied`. A detector
/// captures the branch by which its light leaves the last object on its path (spec 0050 S8.1). Public so the rule is
/// provable without a window.
let availableCaptures (model : TableAndElementRotationModel) (varied : Library.ElementId) : Experiments.MeasurementMode list =
    BeamLayout.capturesFor (BeamLayout.detectedBranches model.sceneLayout model.detectorIds varied model.objectIds)

/// Spec 0027 (028): the human label of a present element ("<kind> #<n>", 1-based, in scene order), captured
/// onto an experiment so its collection row survives the element's removal.
let private experimentElementLabel (model : TableAndElementRotationModel) (id : Library.ElementId) : Experiments.ElementLabel =
    model.indexedElements
    |> List.tryFind (fun (_, e) -> e.id = id)
    |> Option.map (fun (i, e) -> Experiments.ElementLabel $"{e.placement.catalogueKind.kindName} #{i.value + 1}")
    |> Option.defaultValue (Experiments.ElementLabel "(element)")

type TableAndElementRotationModel with

    /// Spec 0046 (task 5): the capture the DRAFT actually runs with — its requested capture coerced to what
    /// the detectors allow (`BeamLayout.effectiveCapture`), or `None` while no detector captures anything.
    member model.draftCapture : Experiments.MeasurementMode option =
        let draft = model.experimentCollection.draft
        match draft.elementId with
        | Some id -> BeamLayout.effectiveCapture (availableCaptures model id) draft.measurement
        | None -> None

/// The experiment's data badge: its file's status through the kit and, while unchanged, its reading and validation
/// against the detector its setup revision holds. A revision the kit's setups lack fails as a units mismatch, that
/// detector being unknown (spec 0038 Part L; spec 0050 S8.2, S9.1).
let private checkExperimentData (model : TableAndElementRotationModel) (experiment : Experiments.Experiment) : ExperimentDataBadge =
    match experiment.dataOpt with
    | Some data ->
        match model.experimentData.attachmentStatus data.attachment with
        | Ok AttachedDataUnchanged ->
            let outcome =
                experiment.slotsIn model.kitSetups
                |> Result.mapError (fun error -> MeasuredData.DataUnitsMismatchErr error.describe)
                |> Result.bind (fun slots -> loadAndValidate model.library model.experimentData slots experiment data)
            ExperimentDataBadge.ofCheck experiment.dataOpt (Some AttachedDataUnchanged) (Some outcome)
        | Ok status -> ExperimentDataBadge.ofCheck experiment.dataOpt (Some status) None
        // A kit read failure is a malformed file, as `ExperimentDataStore` maps one (spec 0050 S4.2).
        | Error error -> ExperimentDataBadge.ofCheck experiment.dataOpt None (Some (Error (MeasuredData.MalformedDataFileErr error.describe)))
    | None -> ExperimentDataBadge.ofCheck None None None

/// Sets what a kit's content decides, so a reload sets exactly what an open sets (spec 0050 S4.3, S5.3, S8.2, S8.3;
/// 001 §E): the kit, its naming template in the field, its experiments with a fresh draft, their attachments rechecked
/// through the model's `experimentData`, its items, its setups with the active one as read, and no status or offer.
let private withKitContent
    (kit : KitContext)
    (experiments : Experiments.Experiment list)
    (items : Item list)
    (setups : Setups.Setup list)
    (model : TableAndElementRotationModel)
    : TableAndElementRotationModel =
    let loaded =
        {
            model with
                kit = kit
                namingTemplateText = kit.kitFile.namingTemplate.caption
                experimentCollection = Experiments.ExperimentCollection.ofLoaded experiments
                kitStatus = None
                kitChanges = []
                kitSetups = setups
                activeSetupOpt =
                    model.activeSetupOpt
                    |> Option.bind (fun active -> setups |> List.tryFind (fun setup -> setup.name = active.name))
                kitItems = items
        }
    let badges =
        experiments
        |> List.choose (fun experiment ->
            match experiment.dataOpt with
            | Some _ -> Some (experiment.id, checkExperimentData loaded experiment)
            | None -> None)
        |> Map.ofList
    {
        loaded with experimentDataStatus = badges
    }

type TableAndElementRotationModel with

    /// The experiment's data badge as last checked, else what its mapped attachment alone gives (spec 0050 S9.1).
    member model.dataBadgeOf (experiment : Experiments.Experiment) : ExperimentDataBadge =
        match model.experimentDataStatus |> Map.tryFind experiment.id with
        | Some badge -> badge
        | None -> ExperimentDataBadge.ofCheck experiment.dataOpt None None

    /// Works in the opened kit, loads its items and rechecks every recorded data attachment (spec 0050 S4.3, S6; 001 §E).
    member model.loadKit (opened : OpenedKit) : TableAndElementRotationModel =
        {
            model with
                kitFolderOpt = Some opened.folder
                experimentData = opened.experimentData
        }
        |> withKitContent opened.library.kit opened.experiments opened.items opened.setups

    /// The kit's name, ` (read-only)` when it was opened read-only, then ` — ` and its folder when it has one
    /// (spec 0050 S5.3; .manual/001 §O).
    member model.kitCaption : ControlCaption =
        let accessMarker =
            match model.kit.access with
            | ReadOnlyKitAccess -> " (read-only)"
            | ReadWriteKitAccess -> ""
        let folderPart =
            match model.kitFolderOpt with
            | Some folder -> $" — {folder.value.value}"
            | None -> ""
        ControlCaption $"{model.kit.kitFile.name.value}{accessMarker}{folderPart}"

    /// The constructor window's title: `kitCaption`, then the constructor in the launcher buttons' words (spec 0050 S5.3).
    member model.kitWindowTitle : ControlCaption =
        let constructorName =
            match model.constructorMode with
            | ForwardConstructor -> "Main"
            | InverseConstructor -> "Inverse"
        ControlCaption $"{model.kitCaption.value} — Optical Constructor — {constructorName}"

    /// Whether the kit takes writes: the one place its access decides a control or a write message (spec 0050 S5.3).
    member model.kitWrites : ControlAvailability =
        match model.kit.access with
        | ReadWriteKitAccess -> ControlOffered
        | ReadOnlyKitAccess -> ControlWithheld

    /// The item's model as the window's stores resolve it: its stored version through `itemModels`, its form checked
    /// against the instance, and only then its preset or sample version; `None` when any of them is unresolved, an
    /// error or of another form (spec 0050 S7.2, S8.2).
    member model.tryItemModelChoice (item : Item) : ItemModelChoice option =
        let tryModelVersion (versionId : ItemModels.ItemModelVersionId) : ItemModels.ItemModelVersion option =
            match model.itemModels.resolveItemModelVersion versionId with
            | Ok versionOpt -> versionOpt
            | Error _ -> None
        let tryPreset (preset : ElementValueId) : Library.LibraryEntry option =
            match model.library.tryGetEntry preset with
            | Ok entryOpt -> entryOpt
            | Error _ -> None
        match item.model with
        | ItemModelVersionInstance versionId ->
            tryModelVersion versionId
            |> Option.bind (fun version ->
                match version.entry.model.bindingKindOpt with
                | None -> Some (LibraryModelChoice version)
                | Some _ -> None)
        | PresetInstance (versionId, preset) ->
            tryModelVersion versionId
            |> Option.bind (fun version ->
                match version.entry.model.bindingKindOpt with
                | Some ItemModels.SourcePresetBinding ->
                    match tryPreset preset with
                    | Some (Library.SourceItem source) -> Some (SourcePresetChoice (version, source))
                    | Some (Library.SampleItem _)
                    | Some (Library.DetectorItem _)
                    | Some (Library.PolarizerItem _)
                    | None -> None
                | Some ItemModels.PolarizerPresetBinding ->
                    match tryPreset preset with
                    | Some (Library.PolarizerItem polarizer) -> Some (PolarizerPresetChoice (version, polarizer))
                    | Some (Library.SampleItem _)
                    | Some (Library.SourceItem _)
                    | Some (Library.DetectorItem _)
                    | None -> None
                | Some ItemModels.StackSampleBinding
                | None -> None)
        | SampleInstance (versionId, sampleVersion) ->
            tryModelVersion versionId
            |> Option.bind (fun version ->
                match version.entry.model.bindingKindOpt with
                | Some ItemModels.StackSampleBinding ->
                    match model.samples.resolveVersion sampleVersion with
                    | Ok (Some sample) -> Some (StackSampleChoice (version, sampleVersion, sample))
                    | Ok None
                    | Error _ -> None
                | Some ItemModels.SourcePresetBinding
                | Some ItemModels.PolarizerPresetBinding
                | None -> None)

    /// What the slot's chip lists: the kit's items that fit it, in `kitItems`' order (spec 0050 S8.4; 001 §J "each
    /// dropdown lists compatible items only").
    member model.slotChipItems (slot : Setups.SetupSlot) : Item list = slot.fittingItems model.kitItems model.tryItemModelChoice

    /// The open grid draft's grid over the table, which the panel's Generate sends; `None` while the panel is closed
    /// (spec 0050 S8.4).
    member model.setupGrid : SetupGrids.SetupGrid option =
        model.setupGridOpt |> Option.map (fun draft -> draft.gridOver model.elements model.kitItems model.tryItemModelChoice)

    /// The grid panel while open: a row per slot in light order with its fitting items and draft, a line per combination
    /// marked impossible with why, ticked or unticked, and Generate for a ticked combination over a slot under a writable
    /// kit (spec 0050 S8.4).
    member model.setupGridState : SetupGridView.SetupGridState option =
        (model.setupGridOpt, model.setupGrid)
        ||> Option.map2 (fun draft grid ->
            let ticked = grid.ticked
            let rowOf (combination : SetupGrids.GridCombination) : SetupGridView.GridCombinationState =
                let mark =
                    match combination.slotsOver model.elements model.kitItems model.tryItemModelChoice with
                    | Error error -> SetupGridView.CombinationImpossible (ControlCaption error.describe)
                    | Ok _ when List.contains combination ticked -> SetupGridView.CombinationTicked
                    | Ok _ -> SetupGridView.CombinationUnticked
                {
                    combination = combination
                    caption = combination.caption model.elements model.kitItems
                    mark = mark
                }
            let generateAvailability =
                match model.kitWrites, grid.axes, ticked with
                | ControlOffered, _ :: _, _ :: _ -> ControlOffered
                | ControlOffered, [], _
                | ControlOffered, _, []
                | ControlWithheld, _, _ -> ControlWithheld
            {
                grid = grid
                slots =
                    Setups.SetupSlot.inLightOrder model.elements
                    |> List.map (fun roled ->
                        {
                            SetupGridView.SetupGridSlotState.roled = roled
                            fitting = model.slotChipItems roled.slot
                            draft = draft.slotDraft roled.slot.id
                        })
                rows = grid.combinations |> List.map rowOf
                generateAvailability = generateAvailability
            })

    /// The name the window's stores give the item's model: its resolved version's `<name> v<n>`, after a preset's
    /// entry name or a sample's `<name> v<n>`; `(model not in the library)` when `tryItemModelChoice` answers `None`
    /// (spec 0050 S7.2, S8.2).
    member model.itemModelCaption (item : Item) : ControlCaption =
        let modelPart (held : ItemModels.ItemModelVersion) : string = $"{held.entry.name.value} v{held.version.value}"
        match model.tryItemModelChoice item with
        | Some (LibraryModelChoice held) -> ControlCaption (modelPart held)
        | Some (SourcePresetChoice (held, source)) -> ControlCaption $"{source.name.value} ({modelPart held})"
        | Some (PolarizerPresetChoice (held, polarizer)) -> ControlCaption $"{polarizer.name.value} ({modelPart held})"
        | Some (StackSampleChoice (held, version, sample)) -> ControlCaption $"{sample.name.value} v{version.version.value} ({modelPart held})"
        | None -> ControlCaption "(model not in the library)"

    /// The kit's item of that id and its model as the stores resolve it, else `UnknownSlotItemErr` naming it: one
    /// resolution for `AssignSlot`, `SwapSlot` and the run (spec 0050 S8.2).
    member internal model.resolveSlotItem (itemId : ItemId) : Result<Item * ItemModelChoice, SetupError> =
        match model.kitItems |> List.tryFind (fun item -> item.id = itemId) with
        | None -> Error (UnknownSlotItemErr $"the kit holds no item {itemId.value}")
        | Some item ->
            match model.tryItemModelChoice item with
            | Some choice -> Ok (item, choice)
            | None -> Error (UnknownSlotItemErr $"'{item.label.value}': its model is not in the library")

    /// The run's source: the first light-source slot's item emitting over its own parameters, else the default light;
    /// the error is what the failure chart says. Its λ stays `boundSource`'s, the preset `withItem` binds (spec 0050 S8.2).
    member model.runEmission : Result<ExperimentRun.SourceEmission, ControlCaption> =
        let heldItemOpt =
            model.elements
            |> List.tryPick (fun slot ->
                match slot.placement.catalogueKind, slot.content with
                | LightSource, Setups.ItemInSlot (itemId, _) -> Some itemId
                | LightSource, Setups.EmptySlotContent
                | (LinearPolarizer | CircularPolarizer | Sample | Lens | FlatMirror | CurvedMirror | Detector), _ -> None)
        match heldItemOpt with
        | Some itemId ->
            match model.resolveSlotItem itemId with
            | Ok (item, choice) ->
                choice.emissionWith (ParameterValues.ofParameters item.parameters)
                |> Result.mapError (fun error -> ControlCaption error.describe)
            | Error error -> Error (ControlCaption error.describe)
        | None ->
            ExperimentRun.SourceEmission.ofValues (ParameterValues.defaultsOf Library.SourcePreset.lightSourceAxes)
            |> Result.mapError (fun error -> ControlCaption error.describe)

/// Runs a kit write only while the kit takes writes; a read-only kit answers why and writes nothing (spec 0050 S5.3).
let private guardKitWrite (model : TableAndElementRotationModel) (write : unit -> TableAndElementRotationModel) : TableAndElementRotationModel =
    match model.kitWrites with
    | ControlOffered -> write ()
    | ControlWithheld ->
        {
            model with kitStatus = Some (ControlCaption "The kit is open read-only: nothing is saved.")
        }

/// Runs a step of the reload offer only for a kit with a folder: nothing outside the app changes a scratch kit (spec 0050 S5.3).
let private guardKitFolder (model : TableAndElementRotationModel) (step : unit -> TableAndElementRotationModel) : TableAndElementRotationModel =
    match model.kitFolderOpt with
    | Some _ -> step ()
    | None -> model

/// Tells why the kit could not be checked for outside changes, keeping the model and its offer (spec 0050 S5.3).
let private withKitCheckFailure (error : KitError) (model : TableAndElementRotationModel) : TableAndElementRotationModel =
    {
        model with kitStatus = Some (ControlCaption $"The kit could not be checked for changes — {error.describe}")
    }

type TableAndElementRotationModel with

    /// The table's slots as a setup records them, so a dial-mounted item keeps its raw reading (spec 0050 S8.2).
    member model.recordedSlots : Setups.SetupSlot list =
        model.elements |> List.map (fun slot -> slot.recordedWith model.kitItems)

    /// The slot bar: a chip per slot in light order, read as recorded so an object shows the angle a name writes, listing
    /// `slotChipItems`; the open chip; and Record experiment while the kit takes writes (spec 0050 S8.3, S8.4).
    member model.slotBarState : SlotBarView.SlotBarState =
        {
            chips =
                Setups.SetupSlot.inLightOrder model.recordedSlots
                |> List.map (fun roled ->
                    {
                        SlotBarView.SlotChipState.roled = roled
                        caption = roled.chipCaption model.kitItems
                        fitting = model.slotChipItems roled.slot
                    })
            openOpt = model.slotChipOpenOpt
            recordAvailability = model.kitWrites
        }

    /// `active` with the table written over it, minting its next revision when an experiment references the current
    /// one (spec 0050 S8.2).
    member model.tableWrittenOver (active : Setups.Setup) : Setups.Setup =
        active.revisedWith model.recordedSlots (active.pinFor model.experimentCollection.experiments)

    /// The table as the setup it is recorded as: the active one written over, else a new setup under the kit's template
    /// name of the table or S8.2's first free `Setup <n>`, made free among the held setups (spec 0050 S8.2, S8.3).
    member model.tableSetup : Result<Setups.Setup, KitLibraryError> =
        match model.activeSetupOpt with
        | Some active -> Ok (model.tableWrittenOver active)
        | None ->
            let taken = model.kitSetups |> List.map (fun held -> held.name)
            let slots = model.recordedSlots
            let name = model.kit.kitFile.namingTemplate.setupNameAmong slots model.kitItems taken (SetupName.firstFree taken)
            Setups.Setup.create name slots model.kitSetups |> Result.mapError KitLibrarySetupErr

    /// The model once `setup` is recorded in the kit: it replaces its name's among `kitSetups`, kept in ordinal name
    /// order, and is the active setup (spec 0050 S8.2).
    member model.withSetupRecorded (setup : Setups.Setup) : TableAndElementRotationModel =
        {
            model with
                kitSetups =
                    setup :: (model.kitSetups |> List.filter (fun held -> held.name <> setup.name))
                    |> List.sortWith (fun first second -> System.String.CompareOrdinal(first.name.value, second.name.value))
                activeSetupOpt = Some setup
        }

    /// Closes the open copy source, if any, and forgets it (spec 0050 S6).
    member model.closeCopySource () : TableAndElementRotationModel =
        match model.kitCopyOpt with
        | Some copySource -> copySource.release ()
        | None -> ()
        {
            model with kitCopyOpt = None
        }

    /// The add form's offers: the standalone models of `itemModels`, then the source presets, the polarizer presets
    /// and the samples at version 1, each under the binding model `itemModels` lists; nothing is offered for a failed
    /// library listing or for a binding model `itemModels` lacks (spec 0050 S7.2).
    member internal model.addItemOffersOver (itemModels : ItemModels.ItemModelVersion list) : ItemModelChoice list =
        let listed (kind : CatalogueKind) : Library.LibraryEntry list =
            match model.library.entriesForKind kind with
            | Ok entries -> entries
            | Error _ -> []
        let underBindingModel (kind : ItemModels.ItemModelBindingKind) (offer : ItemModels.ItemModelVersion -> ItemModelChoice list) : ItemModelChoice list =
            match itemModels |> List.tryFind (fun held -> held.entry.model.bindingKindOpt = Some kind) with
            | Some bindingModel -> offer bindingModel
            | None -> []
        let standalone =
            itemModels
            |> List.choose (fun held ->
                match held.entry.model.bindingKindOpt with
                | Some _ -> None
                | None -> Some (LibraryModelChoice held))
        let sources =
            underBindingModel ItemModels.SourcePresetBinding (fun sourceModel ->
                listed LightSource
                |> List.choose (fun entry ->
                    match entry with
                    | Library.SourceItem source -> Some (SourcePresetChoice (sourceModel, source))
                    | Library.SampleItem _ | Library.DetectorItem _ | Library.PolarizerItem _ -> None))
        // A compound polarizer serves both kinds, so both listings may name it (spec 0038 step 014).
        let polarizers =
            underBindingModel ItemModels.PolarizerPresetBinding (fun polarizerModel ->
                listed LinearPolarizer @ listed CircularPolarizer
                |> List.distinctBy (fun entry -> entry.entryId)
                |> List.choose (fun entry ->
                    match entry with
                    | Library.PolarizerItem polarizer -> Some (PolarizerPresetChoice (polarizerModel, polarizer))
                    | Library.SampleItem _ | Library.SourceItem _ | Library.DetectorItem _ -> None))
        let samples =
            underBindingModel ItemModels.StackSampleBinding (fun sampleModel ->
                listed Sample
                |> List.choose (fun entry ->
                    match entry with
                    | Library.SampleItem sample -> Some (StackSampleChoice (sampleModel, Library.SampleVersionId.firstOf sample.id, sample))
                    | Library.SourceItem _ | Library.DetectorItem _ | Library.PolarizerItem _ -> None))
        standalone @ sources @ polarizers @ samples

    /// The held items with `item` in its id's place, or added, in the ordinal label order `readItems` keeps
    /// (spec 0050 S7.2).
    member internal model.kitItemsWith (item : Item) : Item list =
        item :: (model.kitItems |> List.filter (fun held -> held.id <> item.id))
        |> List.sortWith (fun first second -> System.String.CompareOrdinal(first.label.value, second.label.value))

/// Why an item edit wrote nothing: the item refused it, or its photo could not be attached (spec 0050 S7.2).
type private KitItemEditError =
    | ItemEditRefusedErr of ItemError
    | PhotoAttachRefusedErr of KitError

    /// The status that tells the refusal (spec 0050 S7.2).
    member this.caption : ControlCaption =
        match this with
        | ItemEditRefusedErr error -> ControlCaption $"The item was not changed — {error.describe}"
        | PhotoAttachRefusedErr error -> ControlCaption $"The photo was not attached — {error.describe}"

/// Edits a held item and records it, after step 185's read-only guard: an id `kitItems` lacks changes nothing, a
/// refusal is told and writes nothing more, and a changed label is recorded as a relabel (spec 0050 S7.2, step 237).
let private recordKitItemEdit
    (id : ItemId)
    (edit : Item -> Result<Item, KitItemEditError>)
    (onRecorded : TableAndElementRotationModel -> TableAndElementRotationModel)
    (model : TableAndElementRotationModel)
    : TableAndElementRotationModel =
    guardKitWrite model (fun () ->
        match model.kitItems |> List.tryFind (fun item -> item.id = id) with
        | None -> model
        | Some held ->
            match edit held with
            | Error error ->
                {
                    model with kitStatus = Some error.caption
                }
            | Ok edited ->
                let library : KitLibraryContext =
                    {
                        kit = model.kit
                        imports = model.catalogueImports
                    }
                let recorded =
                    if edited.label = held.label then library.recordItem edited
                    else library.recordRelabelledItem held edited
                match recorded with
                | Ok () ->
                    onRecorded
                        {
                            model with
                                kitItems = model.kitItemsWith edited
                                kitStatus = None
                        }
                | Error error ->
                    {
                        model with kitStatus = Some (ControlCaption $"Not saved to the kit — {error.describe}")
                    })

/// The session name type itself: qualified as `Experiments.SessionName`, F# resolves its union case and finds no
/// `tryCreate` (spec 0050 S9.1).
type private ExperimentSessionName = Experiments.SessionName

type ControlCaption with

    /// The typed text as an experiment's notes (spec 0050 S9.1).
    member private this.toExperimentNotes : Experiments.ExperimentNotes = Experiments.ExperimentNotes this.value

/// Records the collection's experiment of that id changed, after step 185's read-only guard: an id the collection lacks
/// changes nothing, and a failed write keeps the model and tells why (spec 0050 S9.1).
let private recordExperimentEdit
    (id : Experiments.ExperimentId)
    (change : Experiments.Experiment -> Experiments.Experiment)
    (model : TableAndElementRotationModel)
    : TableAndElementRotationModel =
    guardKitWrite model (fun () ->
        match model.experimentCollection.experiments |> List.tryFind (fun experiment -> experiment.id = id) with
        | None -> model
        | Some held ->
            let changed = change held
            let library : KitLibraryContext =
                {
                    kit = model.kit
                    imports = model.catalogueImports
                }
            match library.recordExperiment changed with
            | Ok () ->
                {
                    model with
                        experimentCollection = model.experimentCollection.withStoredExperiment changed
                        kitStatus = None
                }
            | Error error ->
                {
                    model with kitStatus = Some (ControlCaption $"Not saved to the kit — {error.describe}")
                })

/// The table's slot of that id holding the kit's item of that id, or emptied; refused when the table lacks the slot,
/// the kit the item, the stores its model, or the slot does not fit it. `AssignSlot` and `SwapSlot` refuse alike (spec
/// 0050 S8.2).
let private slotAssigned
    (model : TableAndElementRotationModel)
    (slotId : Library.ElementId)
    (itemIdOpt : ItemId option)
    : Result<Setups.SetupSlot, SetupError> =
    match model.elements |> List.tryFind (fun slot -> slot.id = slotId), itemIdOpt with
    | None, _ -> Error (UnknownSetupSlotErr $"'{slotId.value}'")
    | Some slot, None -> Ok slot.withoutItem
    | Some slot, Some itemId -> model.resolveSlotItem itemId |> Result.bind (fun (item, choice) -> slot.withItem item choice)

/// The table with `filled` in its slot's place (spec 0050 S8.2).
let private withSlotFilled (filled : Setups.SetupSlot) (slots : Setups.SetupSlot list) : Setups.SetupSlot list =
    slots |> List.map (fun slot -> if slot.id = filled.id then filled else slot)

/// Records each write in order, the model holding each once it is written; the first failed write stops the run with
/// the model as written so far (spec 0050 S8.3).
let private writtenInOrder
    (record : 'Written -> Result<unit, KitLibraryError>)
    (held : TableAndElementRotationModel -> 'Written -> TableAndElementRotationModel)
    (writes : 'Written list)
    (model : TableAndElementRotationModel)
    : Result<TableAndElementRotationModel, TableAndElementRotationModel * KitLibraryError> =
    writes
    |> List.fold (fun written write ->
        written
        |> Result.bind (fun current ->
            match record write with
            | Ok () -> Ok (held current write)
            | Error error -> Error (current, error))) (Ok model)

/// The model once `setup`, the table's, is recorded as the active setup; the active setup unchanged writes nothing
/// (spec 0050 S8.2, S8.4).
let private withTableRecorded (setup : Setups.Setup) (model : TableAndElementRotationModel) : Result<TableAndElementRotationModel, KitLibraryError> =
    let library : KitLibraryContext =
        {
            kit = model.kit
            imports = model.catalogueImports
        }
    let recorded =
        match model.activeSetupOpt with
        | Some active when active = setup -> Ok ()
        | Some _
        | None -> library.recordSetup setup
    recorded |> Result.map (fun () -> model.withSetupRecorded setup)

/// The model once each of `committed`'s `experiments` is recorded in order, the collection taking `committed`'s draft
/// and each in its id's place or appended; the first failed write stops, keeping those before it (spec 0050 S8.2–S8.4).
let private withExperimentsRecorded
    (committed : Experiments.ExperimentCollection)
    (experiments : Experiments.Experiment list)
    (model : TableAndElementRotationModel)
    : Result<TableAndElementRotationModel, TableAndElementRotationModel * KitLibraryError> =
    let library : KitLibraryContext =
        {
            kit = model.kit
            imports = model.catalogueImports
        }
    let experimentHeld (current : TableAndElementRotationModel) (experiment : Experiments.Experiment) : TableAndElementRotationModel =
        let experiments = current.experimentCollection.experiments
        let placed =
            if experiments |> List.exists (fun held -> held.id = experiment.id) then
                experiments |> List.map (fun held -> if held.id = experiment.id then experiment else held)
            else
                experiments @ [ experiment ]
        {
            current with
                experimentCollection =
                    {
                        committed with experiments = placed
                    }
        }
    writtenInOrder library.recordExperiment experimentHeld experiments model

/// The experiments `committed` holds and `held` does not, in order: those its commit appended (spec 0050 S8.3, S8.4).
let private appendedOver (held : Experiments.ExperimentCollection) (committed : Experiments.ExperimentCollection) : Experiments.Experiment list =
    let heldIds = held.experiments |> List.map (fun experiment -> experiment.id)
    committed.experiments |> List.filter (fun experiment -> not (List.contains experiment.id heldIds))

/// The experiments to append over `setups`, named by the kit's template free of the collection's names, else `E<n>` of
/// the id each takes (spec 0050 S9.1).
let private experimentTargetsOver (setups : Setups.Setup list) (model : TableAndElementRotationModel) : Experiments.ExperimentTarget list =
    let collection = model.experimentCollection
    let taken = collection.experiments |> List.map (fun experiment -> experiment.name)
    model.kit.kitFile.namingTemplate.experimentTargets setups model.kitItems taken collection.nextId

/// The open grid draft edited, or the model as it is while the panel is closed (spec 0050 S8.4).
let private withGridDraftEdited
    (edit : SetupGrids.SetupGridDraft -> SetupGrids.SetupGridDraft)
    (model : TableAndElementRotationModel)
    : TableAndElementRotationModel =
    match model.setupGridOpt with
    | Some draft ->
        {
            model with setupGridOpt = Some (edit draft)
        }
    | None -> model

// `rec` (spec 0038 step 017): the BindValueId / ConfirmBindValueId arms DELEGATE to the one
// targeted commit arm (`BindValueIdTo`) instead of committing `placement.valueId` themselves.
let rec updateTableAndElementRotation (msg : TableAndElementRotationMsg) (model : TableAndElementRotationModel) : TableAndElementRotationModel =
    match msg with
    | RotateR1By d -> rotateSelected RotationControls.R1 d model
    | RotateR2By d -> rotateSelected RotationControls.R2 d model
    | RotateR3By d -> rotateSelected RotationControls.R3 d model
    | ToggleR3Lock ->
        match model.selection with
        | TableSelected -> { model with tableR3Locked = model.tableR3Locked.toggled }
        | ElementSelected i -> mapElement i (fun e -> { e with placement = setR3Locked e.placement.r3Locked.toggled e.placement }) model
        | NothingSelected -> model
    | ToggleR1Lock ->
        match model.selection with
        | ElementSelected i -> mapElement i (fun e -> { e with placement = setR1Locked e.placement.r1Locked.toggled e.placement }) model
        | TableSelected | NothingSelected -> model
    | ToggleR2Lock ->
        match model.selection with
        | ElementSelected i -> mapElement i (fun e -> { e with placement = setR2Locked e.placement.r2Locked.toggled e.placement }) model
        | TableSelected | NothingSelected -> model
    | RotSetAxis (axis, v) -> setSelectedAxis axis v model
    | RotRequestReset -> { model with rotationConfirm = RotationControls.ConfirmReset }
    | RotRequestResetAll -> { model with rotationConfirm = RotationControls.ConfirmResetAll }
    | RotConfirm ->
        let m =
            match model.rotationConfirm with
            | RotationControls.ConfirmReset -> model.withSelectionRotationsReset
            | RotationControls.ConfirmResetAll -> model.withAllRotationsReset
            | RotationControls.NoConfirm -> model
        { m with rotationConfirm = RotationControls.NoConfirm }
    | RotCancel -> { model with rotationConfirm = RotationControls.NoConfirm }
    | AddElement kind ->
        // Spec 0038 (018): the plain add CONVERGES on the pre-binding arm below with the kind's
        // seeded default — every new table element except a sample lands pre-bound.
        updateTableAndElementRotation (AddElementBoundTo (kind, kind.defaultSeedEntry)) model
    | AddElementBoundTo (kind, prebind) -> addElement kind prebind model
    | RemoveSelectedElement -> model.withSelectedRemoved
    | SlideSelectedBy dx -> moveSelectedBy TableX dx model
    | SlideSelectedTo x -> moveSelectedTo TableX x model
    | ResetSelectedPosition -> model.withSelectedPositionReset
    | MoveSelectedBy (axis, d) -> moveSelectedBy axis d model
    | MoveSelectedTo (axis, v) -> moveSelectedTo axis v model
    | ToggleMoveLock axis -> toggleMoveLock axis model
    | RenderSwap -> { model with render = model.render.swapped }
    | RenderSetRailsIndex i -> { model with render = model.render.withRailsIndex i }
    | RenderSetCircles n -> { model with render = model.render.withCircles n }
    | RenderSetRadialsIndex i -> { model with render = model.render.withRadialsIndex i }
    | RenderSetRailOpacity v -> { model with render = model.render.withRailOpacity v }
    | RenderSetFaceOpacity v -> { model with render = model.render.withFaceOpacity v }
    | RenderSetLineOpacity v -> { model with render = model.render.withLineOpacity v }
    | SelectBay name ->
        // Spec 0050 S7.2: the strip names the surface the ribbon shows.
        let flowStep = if name = BayNames.experiments then ExperimentsFlowStep else SetupsFlowStep
        { model with ribbon = name; flowStep = flowStep }
    | ShowFlowStep ItemsFlowStep -> { model with flowStep = ItemsFlowStep }
    | ShowFlowStep SetupsFlowStep ->
        // Spec 0050 S7.2: Setups leaves the Experiments bay for the table's first bay; any other open bay stays.
        let bay = if model.ribbon = BayNames.experiments then BayNames.add else model.ribbon
        { model with flowStep = SetupsFlowStep; ribbon = bay }
    | ShowFlowStep ExperimentsFlowStep -> { model with flowStep = ExperimentsFlowStep; ribbon = BayNames.experiments }
    | SelectKitItem id ->
        match model.kitItems |> List.exists (fun item -> item.id = id) with
        | true -> { model with selectedItemOpt = Some id; parameterEditOpt = None }
        | false -> model
    | OpenAddItemForm ->
        // Spec 0050 S7.2: the binding models come from this one listing, so a failed listing offers none of them.
        let listed = model.itemModels.listItemModels ()
        let itemModels =
            match listed with
            | Ok held -> held
            | Error _ -> []
        let form : AddItemForm =
            {
                itemId = model.itemIds.mintItemId ()
                offers = model.addItemOffersOver itemModels
                chosenOpt = None
                labelText = ControlCaption ""
                dial = FixedMounting
            }
        match listed with
        | Ok _ -> { model with addItemFormOpt = Some form }
        | Error error ->
            {
                model with
                    addItemFormOpt = Some form
                    kitStatus = Some (ControlCaption $"The item models could not be listed — {error.describe}")
            }
    | ChooseNewItemModel choice ->
        match model.addItemFormOpt with
        | Some form -> { model with addItemFormOpt = Some { form with chosenOpt = Some choice } }
        | None -> model
    | SetNewItemLabel text ->
        match model.addItemFormOpt with
        | Some form -> { model with addItemFormOpt = Some { form with labelText = text } }
        | None -> model
    | SetNewItemDial dial ->
        match model.addItemFormOpt with
        | Some form -> { model with addItemFormOpt = Some { form with dial = dial } }
        | None -> model
    | CancelAddItemForm -> { model with addItemFormOpt = None }
    | AddItem draft ->
        guardKitWrite model (fun () ->
            match draft.addTo model.kitItems with
            | Error error ->
                {
                    model with kitStatus = Some (ControlCaption $"The item was not added — {error.describe}")
                }
            | Ok item ->
                let library : KitLibraryContext =
                    {
                        kit = model.kit
                        imports = model.catalogueImports
                    }
                match library.recordItem item with
                | Ok () ->
                    {
                        model with
                            kitItems = model.kitItemsWith item
                            selectedItemOpt = Some item.id
                            addItemFormOpt = None
                            kitStatus = None
                    }
                | Error error ->
                    {
                        model with kitStatus = Some (ControlCaption $"Not saved to the kit — {error.describe}")
                    })
    | SetParameterState (itemId, name, state) ->
        model
        |> recordKitItemEdit
            itemId
            (fun held -> held.withParameterState name state |> Result.mapError ItemEditRefusedErr)
            (fun recorded -> { recorded with parameterEditOpt = None })
    | EditItemParameter name ->
        let selectedOpt =
            model.selectedItemOpt
            |> Option.bind (fun id -> model.kitItems |> List.tryFind (fun item -> item.id = id))
        match selectedOpt with
        | Some item ->
            match item.tryFindParameter name with
            | Some parameter -> { model with parameterEditOpt = Some (ParameterEditDraft.ofParameter item.id parameter) }
            | None -> model
        | None -> model
    | SetParameterEditChoice choice ->
        match model.parameterEditOpt with
        | Some draft -> { model with parameterEditOpt = Some { draft with choice = choice } }
        | None -> model
    | SetParameterEditText (field, text) ->
        match model.parameterEditOpt with
        | Some draft -> { model with parameterEditOpt = Some (draft.withFieldText field text) }
        | None -> model
    | CancelParameterEdit -> { model with parameterEditOpt = None }
    | RelabelKitItem (itemId, label) ->
        model
        |> recordKitItemEdit
            itemId
            (fun held -> held.relabelledAmong label model.kitItems |> Result.mapError ItemEditRefusedErr)
            (fun recorded -> recorded)
    | SetKitItemNotes (itemId, notes) ->
        model |> recordKitItemEdit itemId (fun held -> Ok (held.withNotes notes)) (fun recorded -> recorded)
    | SetKitItemPhoto (itemId, file) ->
        // Spec 0050 S4.2 (step 135): a file inside the kit is recorded in place, one outside is copied into data/.
        model
        |> recordKitItemEdit
            itemId
            (fun held ->
                model.experimentData.attachDataFile file
                |> Result.mapError PhotoAttachRefusedErr
                |> Result.map (fun attachment -> held.withPhoto (Some attachment.path)))
            (fun recorded -> recorded)
    | RemoveKitItemPhoto itemId ->
        model |> recordKitItemEdit itemId (fun held -> Ok (held.withPhoto None)) (fun recorded -> recorded)
    | BindValueId entryId ->
        // Spec 0038 (017): the direct bind CONVERGES on the one targeted commit arm below —
        // resolve the selected element's serializable id and delegate to `BindValueIdTo`.
        match model.selection with
        | ElementSelected i ->
            match model.tryElementAt i with
            | Some e -> updateTableAndElementRotation (BindValueIdTo (e.id, entryId)) model
            | None -> model
        | TableSelected | NothingSelected -> model
    | RequestBindValueId entryId ->
        // Spec 0027 (026): select the entry as PENDING (its full description is shown) without binding yet —
        // inert unless an element is selected (the Select bay is only enabled for an element).
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
        | Some entryId, ElementSelected i ->
            match model.tryElementAt i with
            | Some e -> updateTableAndElementRotation (BindValueIdTo (e.id, entryId)) model
            | None -> { model with pendingEntry = None }
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
        let m = model.withActiveSelectCancelled
        match m.tryIndexOf elementId with
        | Some i ->
            // Spec 0040 Part D.3: a bound sample seeds the placed element's emission from its
            // geometry-constrained `supportedEmission` (a Mirror ⇒ reflected-only); a non-sample
            // bind keeps the kind default (`withBoundSampleEmission`). Spec 0050 S7.2, S8.2: a slot bound to another entry
            // no longer holds the piece it held, so its identity and its binding never disagree.
            mapElement i (fun e ->
                let rebound = { e with placement = withBoundSampleEmission m { e.placement with valueId = Some entryId } }
                if e.placement.valueId = Some entryId then rebound else rebound.withoutItem)
                { m with pendingEntry = None; selectStatus = None }
        | None ->
            { m with selectStatus = Some (ControlCaption "The chosen entry was not bound — its target element is no longer on the table.") }
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
    | ExpChooseElement elementId ->
        // Choosing the element to vary sets the draft element AND, from the element's kind, the allowed
        // variables (data via `SetupSlot.scanVariablesWith`, never hard-coded here) and the default capture. Spec 0046:
        // the default is the best capture the scene's DETECTORS allow (Both when they see both branches,
        // else the one they see); with no capturing detector it falls back to the element's emission.
        // Spec 0046: what the element may vary comes from the element AND its binding (a fixed-line source
        // varies nothing); a wavelength sweep spans the tunable source's limits.
        match model.elements |> List.tryFind (fun e -> e.id = elementId) with
        | Some e ->
            let allowed = variablesOfElement model e
            let defaultMeasurement =
                BeamLayout.effectiveCapture (availableCaptures model e.id) Experiments.CaptureBoth
                |> Option.defaultValue (Experiments.MeasurementMode.ofEmission e.placement.emission)
            let label = experimentElementLabel model e.id
            let chosen = Experiments.chooseElement e.id label allowed defaultMeasurement model.experimentCollection
            let ranged =
                match chosen.draft.variable with
                | Some Experiments.VaryWaveLength -> Experiments.setRange model.wavelengthRange chosen
                | Some Experiments.VaryR1 | Some Experiments.VaryR2 | Some Experiments.VaryDialReading | None -> chosen
            { model with experimentCollection = ranged }
        | None -> model
    | ExpChooseVariable v ->
        let chosen = Experiments.chooseVariable v.variableParameter model.experimentCollection
        let ranged =
            match chosen.draft.variable with
            | Some Experiments.VaryWaveLength -> Experiments.setRange model.wavelengthRange chosen
            | Some Experiments.VaryR1 | Some Experiments.VaryR2 | Some Experiments.VaryDialReading | None -> chosen
        { model with experimentCollection = ranged }
    | ExpChooseMeasurement m -> { model with experimentCollection = Experiments.chooseMeasurement m.measurementMode model.experimentCollection }
    | ExpSetRangeMin v ->
        let set = Experiments.setRangeMin v.sweptValue model.experimentCollection
        { model with experimentCollection = Experiments.setRange (clampSweep model set.draft.range) set }
    | ExpSetRangeMax v ->
        let set = Experiments.setRangeMax v.sweptValue model.experimentCollection
        { model with experimentCollection = Experiments.setRange (clampSweep model set.draft.range) set }
    | ExpSetSourceWaveLength nm ->
        // Spec 0046: only a TUNABLE source takes a setting, and only within its limits.
        match model.boundSource |> Option.bind (fun source -> source.tunableLimitsOpt) with
        | Some limits ->
            let lo, hi = limits.lower.nanometers, limits.upper.nanometers
            let setting = WaveLength.nm (max lo (min hi nm.value) * 1.0<nm>)
            { model with experimentCollection = Experiments.setSourceWaveLength (Some setting) model.experimentCollection }
        | None -> model
    | ExportExperimentData -> exportDraftRun model
    | ExpSetRangePoints n -> { model with experimentCollection = Experiments.setRangePoints n.curvePointCount model.experimentCollection }
    // Spec 0050 S9.1: the scan is built in the draft and written only by a commit, so a read-only kit takes both.
    | ExpAddScanAxis -> { model with experimentCollection = model.experimentCollection.addScanAxis }
    | ExpAddScanBlock ->
        // The closed block is read on the detectors' capture, as ExpCommit reads the last; no axis or capture, no change.
        match model.experimentCollection.draft.axisOpt, model.draftCapture with
        | Some _, Some capture ->
            {
                model with experimentCollection = (Experiments.chooseMeasurement capture model.experimentCollection).addScanBlock
            }
        | Some _, None
        | None, _ -> model
    | ExpCommit ->
        guardKitWrite model (fun () ->
            // Spec 0046: the detectors' allowed measurement. Spec 0050 S8.2: the table is recorded as the active setup,
            // minting its next revision when an experiment references the current one, then the experiment over its
            // reference; the collection, the setups and the active setup change only once both are recorded.
            match model.draftCapture with
            | Some capture ->
                let notSaved (error : KitLibraryError) : TableAndElementRotationModel =
                    {
                        model with kitStatus = Some (ControlCaption $"Not saved to the kit — {error.describe}")
                    }
                match model.tableSetup with
                | Error error -> notSaved error
                | Ok setup ->
                    let collection = Experiments.chooseMeasurement capture model.experimentCollection
                    // Spec 0050 S9.1: an append is named by the template over the setup; an edit keeps its name.
                    let committed =
                        match experimentTargetsOver [ setup ] model with
                        | [ target ] -> Experiments.commit target collection
                        | _ -> model.experimentCollection
                    if committed = model.experimentCollection then model
                    else
                        match committed.draft.editingId |> Option.bind (fun id -> committed.experiments |> List.tryFind (fun experiment -> experiment.id = id)) with
                        | Some experiment ->
                            let recorded =
                                withTableRecorded setup model
                                |> Result.bind (fun tabled -> withExperimentsRecorded committed [ experiment ] tabled |> Result.mapError snd)
                            match recorded with
                            | Ok written ->
                                {
                                    written with kitStatus = None
                                }
                            | Error error -> notSaved error
                        | None -> model
            | None -> model)
    | ExpNew -> { model with experimentCollection = model.experimentCollection.newDraft }
    | ExpEdit id -> { model with experimentCollection = Experiments.edit id model.experimentCollection }
    | ExpRemove id ->
        guardKitWrite model (fun () ->
            // Spec 0050 S9.1: the file is the experiment's name's; an id the collection lacks has none to delete.
            let deleted =
                match model.experimentCollection.experiments |> List.tryFind (fun experiment -> experiment.id = id) with
                | Some experiment -> model.kit.proxy.deleteExperiment experiment.name
                | None -> Ok ()
            match deleted with
            | Ok () | Error (MissingKitFileErr _) ->
                {
                    model with
                        experimentCollection = Experiments.remove id model.experimentCollection
                        kitStatus = None
                }
            | Error error ->
                {
                    model with kitStatus = Some (ControlCaption $"Not saved to the kit — {error.describe}")
                })
    | OpenExperimentChartWindow ->
        // Spec 0027 (026) Part 3: a double-click on the inline chart opens the pop-out interactive ScottPlot
        // window for the current experiment chart. Opening a `Window` is a side effect, matching the existing
        // imperative-window pattern (`MainConstructorWindow().Show()`); it is guarded behind a non-empty chart
        // and is never reached under the headless `ui-smoke` gate (which renders frames but never double-clicks).
        openChartWindowHook model
        model
    | ViewExperiment id ->
        // Spec 0027 (030 follow-up): open the chart window for the collected experiment `id` (the "View"
        // button / a row double-click). Same side-effecting seam as OpenExperimentChartWindow.
        viewExperimentHook model id
        model
    | AttachDataFileTo (experimentId, file) ->
        guardKitWrite model (fun () ->
            match model.experimentData.attachDataFile file with
            | Error error ->
                // Spec 0050 S9.1: a refused attach keeps the experiment's data and badge.
                {
                    model with kitStatus = Some (ControlCaption $"Not attached — {error.describe}")
                }
            | Ok attachment ->
                // Spec 0038 Part L (037); spec 0050 S4.2: keep a successful attachment even when parsing or validation fails.
                // Spec 0050 S9.1: it is read through its detector's built-in profile, every row the run's.
                let slots =
                    model.experimentCollection.experiments
                    |> List.tryFind (fun e -> e.id = experimentId)
                    |> Option.bind (fun held -> held.slotsIn model.kitSetups |> Result.toOption)
                    |> Option.defaultValue []
                let data : MappedAttachment =
                    {
                        attachment = attachment
                        profile = (ExperimentDataLoad.detectorKindOf model.library slots).builtInProfile
                        rows = AllDataRows
                    }
                let collection = Experiments.attachDataFile experimentId data model.experimentCollection
                match collection.experiments |> List.tryFind (fun e -> e.id = experimentId) with
                | Some experiment ->
                    let library : KitLibraryContext =
                        {
                            kit = model.kit
                            imports = model.catalogueImports
                        }
                    match library.recordExperiment experiment with
                    | Ok () ->
                        {
                            model with
                                experimentCollection = collection
                                experimentDataStatus = model.experimentDataStatus |> Map.add experimentId (checkExperimentData model experiment)
                                kitStatus = None
                        }
                    | Error error ->
                        {
                            model with kitStatus = Some (ControlCaption $"Not saved to the kit — {error.describe}")
                        }
                | None -> model)
    | SetExperimentRole (experimentId, role) -> model |> recordExperimentEdit experimentId (fun held -> held.withRole role)
    | SetExperimentSession (experimentId, text) ->
        model |> recordExperimentEdit experimentId (fun held -> held.withSessionOpt (ExperimentSessionName.tryCreate text.value))
    | SetExperimentNotes (experimentId, text) ->
        model |> recordExperimentEdit experimentId (fun held -> held.withNotes text.toExperimentNotes)
    | SetSetupName name -> { model with setupName = name }
    | CreateSetup ->
        guardKitWrite model (fun () ->
            let created =
                SetupName.tryCreate model.setupName.value
                |> Result.bind (fun name -> Setups.Setup.create name model.recordedSlots model.kitSetups)
            match created with
            | Error error ->
                {
                    model with kitStatus = Some (ControlCaption $"Not created — {error.describe}")
                }
            | Ok setup ->
                let library : KitLibraryContext =
                    {
                        kit = model.kit
                        imports = model.catalogueImports
                    }
                match library.recordSetup setup with
                | Ok () ->
                    {
                        model.withSetupRecorded setup with kitStatus = Some (ControlCaption $"Created setup '{setup.name.value}'.")
                    }
                | Error error ->
                    {
                        model with kitStatus = Some (ControlCaption $"Not saved to the kit — {error.describe}")
                    })
    // Spec 0050 S8.2: in place until an experiment references the active revision, then as the next; an unchanged
    // table writes nothing.
    | SaveSetup ->
        guardKitWrite model (fun () ->
            match model.activeSetupOpt with
            | None ->
                {
                    model with kitStatus = Some (ControlCaption "No setup is active: create one first.")
                }
            | Some active ->
                let revised = model.tableWrittenOver active
                if revised = active then
                    {
                        model with kitStatus = Some (ControlCaption $"Setup '{active.name.value}' is unchanged.")
                    }
                else
                    let library : KitLibraryContext =
                        {
                            kit = model.kit
                            imports = model.catalogueImports
                        }
                    match library.recordSetup revised with
                    | Ok () ->
                        let saved = ControlCaption $"Saved setup '{revised.name.value}', revision {revised.revision.value}."
                        {
                            model.withSetupRecorded revised with kitStatus = Some saved
                        }
                    | Error error ->
                        {
                            model with kitStatus = Some (ControlCaption $"Not saved to the kit — {error.describe}")
                        })
    // Spec 0050 S8.2: it writes nothing, so a read-only kit loads setups; the view, snap mode and pivot are the window's.
    | ActivateSetup name ->
        match model.kitSetups |> List.tryFind (fun setup -> setup.name = name) with
        | Some setup ->
            {
                model with
                    elements = setup.slots |> List.map (fun slot -> slot.drawnWith model.kitItems)
                    activeSetupOpt = Some setup
                    selection = TableSelected
                    drag = NotPressed
                    elementMenu = None
                    kitStatus = Some (ControlCaption $"Loaded setup '{setup.name.value}'.")
            }
        | None ->
            let unknown = UnknownSetupErr $"'{name.value}'"
            {
                model with kitStatus = Some (ControlCaption $"Load failed — {unknown.describe}")
            }
    // Spec 0050 S8.2: it writes nothing, so a read-only kit assigns too; the setup records the slot at the next
    // CreateSetup, SaveSetup or commit.
    | AssignSlot (slotId, itemIdOpt) ->
        match slotAssigned model slotId itemIdOpt with
        | Ok filled ->
            {
                model with
                    elements = withSlotFilled filled model.elements
                    kitStatus = None
            }
        | Error error ->
            {
                model with kitStatus = Some (ControlCaption $"Not assigned — {error.describe}")
            }
    // Spec 0050 S8.2: a held setup is switched to by ActivateSetup, writing nothing, so a read-only kit takes it; only a
    // new setup is written, behind the read-only guard, and never the setup swapped from. S8.4: every outcome closes the
    // chip list, so a pick closes it whether the swap switched, created or was refused.
    | SwapSlot (slotId, itemId) ->
        let answered =
            match slotAssigned model slotId (Some itemId) with
            | Error error ->
                {
                    model with kitStatus = Some (ControlCaption $"Not swapped — {error.describe}")
                }
            | Ok filled ->
                let swapped = withSlotFilled filled model.elements |> List.map (fun slot -> slot.recordedWith model.kitItems)
                let held = model.kitSetups |> List.map (fun setup -> setup.name)
                let unnamed =
                    match model.activeSetupOpt with
                    | Some active -> active.name.nextFreeAmong held
                    | None -> SetupName.firstFree held
                // Spec 0050 S8.3: a swapped table the kit's template names takes that name made free, else S8.2's.
                let fresh = model.kit.kitFile.namingTemplate.setupNameAmong swapped model.kitItems held unnamed
                match Setups.Setup.swapTarget swapped model.kitSetups fresh with
                | Setups.SwitchToHeldSetup setup -> updateTableAndElementRotation (ActivateSetup setup.name) model
                | Setups.CreateSwappedSetup setup ->
                    guardKitWrite model (fun () ->
                        let library : KitLibraryContext =
                            {
                                kit = model.kit
                                imports = model.catalogueImports
                            }
                        match library.recordSetup setup with
                        | Ok () ->
                            {
                                model.withSetupRecorded setup with
                                    elements = swapped
                                    kitStatus = Some (ControlCaption $"Created setup '{setup.name.value}' by the swap.")
                            }
                        | Error error ->
                            {
                                model with kitStatus = Some (ControlCaption $"Not saved to the kit — {error.describe}")
                            })
        {
            answered with slotChipOpenOpt = None
        }
    // Spec 0050 S8.3: the setups the kit lacks, then the draft over every setup, as ExpCommit records them; the first
    // failed write stops the run, keeping what was written. The table, the active setup and the draft's choices stay.
    | GenerateSetupGrid grid ->
        guardKitWrite model (fun () ->
            match grid.generate model.elements model.kitItems model.tryItemModelChoice model.kit.kitFile.namingTemplate model.kitSetups with
            | Error error ->
                {
                    model with kitStatus = Some (ControlCaption $"Not generated — {error.describe}")
                }
            | Ok setups ->
                let library : KitLibraryContext =
                    {
                        kit = model.kit
                        imports = model.catalogueImports
                    }
                let fresh = setups |> List.distinct |> List.filter (fun setup -> not (List.contains setup model.kitSetups))
                let committed =
                    match model.draftCapture with
                    | Some capture ->
                        Experiments.chooseMeasurement capture model.experimentCollection
                        |> Experiments.commitOverSetups (experimentTargetsOver setups model)
                    | None -> model.experimentCollection
                let appended = appendedOver model.experimentCollection committed
                let setupHeld (current : TableAndElementRotationModel) (setup : Setups.Setup) : TableAndElementRotationModel =
                    {
                        current.withSetupRecorded setup with activeSetupOpt = current.activeSetupOpt
                    }
                let recorded =
                    writtenInOrder library.recordSetup setupHeld fresh model
                    |> Result.bind (withExperimentsRecorded committed appended)
                match recorded with
                | Ok generated ->
                    let made = setups |> List.distinct |> List.length
                    {
                        generated with
                            kitStatus = Some (ControlCaption $"Generated {made} setups ({fresh.Length} new) and {appended.Length} experiments.")
                    }
                | Error (written, error) ->
                    {
                        written with kitStatus = Some (ControlCaption $"Not saved to the kit — {error.describe}")
                    })
    // Spec 0050 S8.4: the table is recorded as ExpCommit records it, then the draft is appended over that setup, never
    // editing the draft's own; a draft that cannot be committed or that no detector captures writes nothing.
    | RecordExperiment ->
        guardKitWrite model (fun () ->
            let notSaved (error : KitLibraryError) (kept : TableAndElementRotationModel) : TableAndElementRotationModel =
                {
                    kept with kitStatus = Some (ControlCaption $"Not saved to the kit — {error.describe}")
                }
            if not model.experimentCollection.canCommit then
                {
                    model with
                        kitStatus = Some (ControlCaption "Not recorded — choose the element and the variable to vary in the Experiments bay first.")
                }
            else
                match model.draftCapture with
                | None ->
                    {
                        model with kitStatus = Some (ControlCaption "Not recorded — no detector captures this table.")
                    }
                | Some capture ->
                    match model.tableSetup |> Result.bind (fun setup -> withTableRecorded setup model |> Result.map (fun tabled -> tabled, setup)) with
                    | Error error -> notSaved error model
                    | Ok (tabled, setup) ->
                        let committed =
                            Experiments.chooseMeasurement capture tabled.experimentCollection
                            |> Experiments.commitOverSetups (experimentTargetsOver [ setup ] tabled)
                        let appended = appendedOver tabled.experimentCollection committed
                        match withExperimentsRecorded committed appended tabled with
                        | Ok recorded ->
                            let names = appended |> List.map (fun experiment -> experiment.name.value) |> String.concat ", "
                            {
                                recorded with kitStatus = Some (ControlCaption $"Recorded {names} over {setup.reference.describe}.")
                            }
                        | Error (kept, error) -> notSaved error kept)
    // Spec 0050 S8.4: it writes nothing, so a read-only kit takes it; an id the table lacks changes nothing.
    | ToggleSlotChip slotId ->
        if model.elements |> List.exists (fun slot -> slot.id = slotId) then
            let openOpt =
                match model.slotChipOpenOpt with
                | Some openId when openId = slotId -> None
                | Some _
                | None -> Some slotId
            {
                model with slotChipOpenOpt = openOpt
            }
        else model
    // Spec 0050 S8.4: the grid panel's edits write nothing, so a read-only kit takes them; a fit the grid refuses is
    // its own to untick (S8.3), and GenerateSetupGrid leaves the draft open, so a grid can be edited and sent again.
    | OpenSetupGrid ->
        {
            model with setupGridOpt = Some (model.setupGridOpt |> Option.defaultValue SetupGrids.SetupGridDraft.empty)
        }
    | CloseSetupGrid ->
        {
            model with setupGridOpt = None
        }
    | ToggleSetupGridItem (slotId, itemId) -> model |> withGridDraftEdited (fun draft -> draft.withItemToggled slotId itemId)
    | AddSetupGridAngle (slotId, degrees) -> model |> withGridDraftEdited (fun draft -> draft.withGridAngle slotId degrees.wrappedAngle)
    | RemoveSetupGridAngle (slotId, angle) -> model |> withGridDraftEdited (fun draft -> draft.withoutGridAngle slotId angle)
    | ToggleSetupGridCombination combination -> model |> withGridDraftEdited (fun draft -> draft.withCombinationToggled combination)
    // Spec 0050 S5.3: the chart and Data-files gating already follow `constructorMode`.
    | SwitchConstructorMode mode ->
        {
            model with constructorMode = mode
        }
    | ShowKitInExplorer ->
        // Spec 0050 S5.3: a kit with no folder has nothing to show.
        match model.kitFolderOpt with
        | Some folder ->
            match model.kitShell.explorer.showFolder folder.value with
            | Ok () -> model
            | Error error ->
                {
                    model with kitStatus = Some (ControlCaption $"The kit folder could not be shown — {error.describe}")
                }
        | None -> model
    | CloseKit ->
        // Spec 0050 S5.3, .spec-md §10.5: kit writes are already on disk; the window's Closed handler releases the kit (step 154).
        model.kitShell.requestClose ()
        model
    // Spec 0050 S5.3: none of the three writes a kit file, so a read-only kit is offered reloads too (step 131).
    | CheckKitChanges origin ->
        guardKitFolder model (fun () ->
            match model.kit.proxy.changedKitFiles () with
            | Ok [] ->
                match origin with
                | KitCheckOnRequest ->
                    {
                        model with
                            kitChanges = []
                            kitStatus = Some (ControlCaption "No kit file changed outside the app.")
                    }
                | KitCheckOnActivation ->
                    {
                        model with kitChanges = []
                    }
            | Ok files ->
                {
                    model with kitChanges = files
                }
            | Error error -> withKitCheckFailure error model)
    | ReloadKitChanges ->
        guardKitFolder model (fun () ->
            let proxy = model.kit.proxy
            let reloaded =
                proxy.readKitFile ()
                |> Result.bind (fun kitFile ->
                    proxy.readExperiments ()
                    |> Result.bind (fun experiments ->
                        proxy.readItems ()
                        |> Result.bind (fun items ->
                            proxy.readSetups ()
                            |> Result.bind (fun setups ->
                                proxy.recordStamps ()
                                |> Result.map (fun () ->
                                    let kit =
                                        {
                                            model.kit with kitFile = kitFile
                                        }
                                    withKitContent kit experiments items setups model)))))
            match reloaded with
            | Ok reloadedModel -> reloadedModel
            | Error error ->
                {
                    model with kitStatus = Some (ControlCaption $"The kit could not be reloaded — {error.describe}")
                })
    | KeepMyKitChanges ->
        guardKitFolder model (fun () ->
            match model.kit.proxy.recordStamps () with
            | Ok () ->
                {
                    model with kitChanges = []
                }
            | Error error -> withKitCheckFailure error model)
    | SetNamingTemplateText text ->
        {
            model with namingTemplateText = text
        }
    // Spec 0050 S8.3: the window's own write is no change offered for reload (step 130), and no setup or experiment is
    // renamed (step 271).
    | SaveNamingTemplate ->
        guardKitWrite model (fun () ->
            match NamingTemplate.tryCreate model.namingTemplateText.value with
            | Error error ->
                {
                    model with kitStatus = Some (ControlCaption $"Not saved — {error.describe}")
                }
            | Ok template when template = model.kit.kitFile.namingTemplate ->
                {
                    model with kitStatus = Some (ControlCaption "The naming template is unchanged.")
                }
            | Ok template ->
                let revised =
                    {
                        model.kit.kitFile with namingTemplate = template
                    }
                match model.kit.saveKitFile revised with
                | Ok kit ->
                    {
                        model with
                            kit = kit
                            kitStatus = Some (ControlCaption "Saved the naming template.")
                    }
                | Error error ->
                    {
                        model with kitStatus = Some (ControlCaption $"Not saved to the kit — {error.describe}")
                    })
    // Spec 0050 S6: a send writes no kit file, so it is not behind `guardKitWrite`: a kit opened read-only is sent as
    // it is, which is how a reviewer returns a kit (001 §E 'Review round-trip'). One archive job runs per window.
    | SendKit zip ->
        match model.kitFolderOpt with
        | None ->
            {
                model with kitStatus = Some (ControlCaption "A kit with no folder cannot be sent.")
            }
        | Some folder ->
            if model.kitSend.job.isRunning then
                model
            else
                let handle =
                    model.kitShell.archiveJobs.startArchiveJob (fun observer ->
                        model.kitShell.archive.sendKit model.kit.kitFile folder zip observer)
                {
                    model with
                        kitSend = model.kitSend.started handle
                        kitStatus = None
                }
    | SendKitProgressed progress ->
        {
            model with kitSend = model.kitSend.progressed progress
        }
    | SendKitEnded result ->
        let told =
            match result with
            | Ok (KitArchiveFinished files) -> $"The kit was sent: {files.value} file(s) zipped."
            | Ok KitArchiveCancelled -> "Send kit was cancelled: no zip was written."
            | Error error -> $"The kit could not be sent — {error.describe}"
        {
            model with
                kitSend = KitArchiveJob.KitArchiveJobState.idle
                kitStatus = Some (ControlCaption told)
        }
    | CancelSendKit ->
        model.kitSend.cancel ()
        model
    // Spec 0050 S6: opening a source and copying are behind `guardKitWrite`; choosing and Cancel write nothing.
    | OpenCopySource folder ->
        guardKitWrite model (fun () ->
            match model.kitFolderOpt with
            | Some own when own.canonical = folder.canonical ->
                {
                    model with kitStatus = Some (ControlCaption "A kit cannot copy experiments from itself.")
                }
            | _ ->
                let closed = model.closeCopySource ()
                // A read-only open writes nothing, not even `.oc` (spec 0050 S6, steps 126, 131).
                let opened =
                    KitContext.openKit (KitProxy.create folder) ReadOnlyKitAccess model.kit.clock
                    |> Result.bind (fun source ->
                        let unread : KitCopySource =
                            {
                                source = source
                                experiments = []
                                chosen = []
                                items = []
                                setups = []
                                itemRows = []
                            }
                        // Spec 0050 S7.2, S8.2: the items, then the setups, are read after the experiments; no row shows until
                        // an experiment is chosen.
                        let read =
                            source.proxy.readExperiments ()
                            |> Result.bind (fun experiments ->
                                source.proxy.readItems ()
                                |> Result.bind (fun items -> source.proxy.readSetups () |> Result.map (fun setups -> experiments, items, setups)))
                        match read with
                        | Ok (experiments, items, setups) ->
                            Ok
                                {
                                    unread with
                                        experiments = experiments
                                        items = items
                                        setups = setups
                                }
                        | Error error ->
                            unread.release ()
                            Error error)
                match opened with
                | Ok copySource ->
                    {
                        closed with
                            kitCopyOpt = Some copySource
                            kitStatus = None
                    }
                | Error error ->
                    {
                        closed with kitStatus = Some (ControlCaption $"The kit could not be opened to copy from — {error.describe}")
                    })
    | ToggleCopiedExperiment id ->
        match model.kitCopyOpt with
        | Some copySource ->
            let toggled = copySource.toggled id
            let chosenExperiments = toggled.experiments |> List.filter (fun experiment -> List.contains experiment.id toggled.chosen)
            // Spec 0050 S7.2, S8.2: a row comes at its default, keeps the user's choice while used, and goes when unused; a
            // dangling setup revision or item id keeps the selection and rows as they were.
            match Experiments.Experiment.itemsUsedBy chosenExperiments copySource.setups copySource.items with
            | Ok used ->
                {
                    model with
                        kitCopyOpt =
                            Some
                                {
                                    toggled with itemRows = ItemCopyRow.copyRowsFor used model.kitItems copySource.itemRows
                                }
                }
            | Error error ->
                {
                    model with kitStatus = Some (ControlCaption $"The experiment could not be chosen to copy — {error.describe}")
                }
        | None -> model
    | ChooseCopiedItem (id, choice) ->
        match model.kitCopyOpt with
        | Some copySource when copySource.itemRows |> List.exists (fun row -> row.sourceItem.id = id) ->
            let rows =
                copySource.itemRows
                |> List.map (fun row ->
                    if row.sourceItem.id = id then
                        {
                            row with choice = choice
                        }
                    else
                        row)
            {
                model with
                    kitCopyOpt =
                        Some
                            {
                                copySource with itemRows = rows
                            }
            }
        | _ -> model
    | CopyChosenExperiments ->
        guardKitWrite model (fun () ->
            match model.kitCopyOpt with
            | Some copySource when not (List.isEmpty copySource.chosen) ->
                // Spec 0050 S8.2: the setups are read after the items, so the window holds the setups the copy wrote.
                let copied =
                    model.kit.copyExperimentsFrom copySource.source copySource.chosen copySource.itemRows
                    |> Result.bind (fun report ->
                        model.kit.proxy.readExperiments ()
                        |> Result.bind (fun experiments ->
                            model.kit.proxy.readItems ()
                            |> Result.bind (fun items -> model.kit.proxy.readSetups () |> Result.map (fun setups -> experiments, items, setups)))
                        |> Result.map (fun content -> report, content)
                        |> Result.mapError KitLibraryKitErr)
                match copied with
                | Ok (report, (experiments, items, setups)) ->
                    // Spec 0050 S6, S7.2, S8.2: as a reload sets it (step 187), over the kit's items and setups as the copy
                    // left them.
                    let shown = withKitContent model.kit experiments items setups model
                    let copiedCount = List.length report.experiments
                    let sourceName = copySource.source.kitFile.name
                    let told =
                        match report.items with
                        | [] -> $"Copied {copiedCount} experiment(s) from {sourceName.value}."
                        | added -> $"Copied {copiedCount} experiment(s) and added {List.length added} item(s) from {sourceName.value}."
                    {
                        shown.closeCopySource () with kitStatus = Some (ControlCaption told)
                    }
                | Error error ->
                    {
                        model with kitStatus = Some (ControlCaption $"The experiments could not be copied — {error.describe}")
                    }
            | _ -> model)
    | CloseCopySource -> model.closeCopySource ()
    | OpenMaterialsWindow ->
        // Spec 0038 step 013: a pure launch of the single-instance Materials window
        // (UICOMP_XDUO_0009) over app-scope stores and import seam — the launcher seam's
        // shared registry keys it under `MaterialsWindowKey`, so a second click ACTIVATES the
        // live window. No model change: the window runs its own MVU loop over the same stores.
        model.launchers.openMaterialsWindow model.materials model.categories model.catalogueImports
        model
    | OpenLibraryWindow ->
        // Spec 0038 step 015: a pure launch of the single-instance Library window
        // (UICOMP_XDUO_0010) over the read-only Library seam plus the app-scope samples +
        // materials stores — the launcher seam's shared registry keys it under
        // `LibraryWindowKey`, so a second click ACTIVATES the live window. No model change: the
        // window runs its own MVU loop over the same stores (the retired samples-workbench bay's
        // query/selection/confirm state lives in the window's own model now).
        model.launchers.openLibraryWindow model.library model.samples model.sources model.materials model.categories model.libraryBackups model.catalogueImports
        model
    | OpenElementMenuAt pt ->
        // Spec 0046: a right click selects the element under the point (the same nearest-within-radius
        // rule as a left click) and opens its menu there; off every element it only closes the menu.
        match model.selectionAt pt with
        | ElementSelected i ->
            let opened = { target = (List.item i.value model.elements).id; at = pt }
            let selection = ElementSelected i
            if selection = model.selection then { model with elementMenu = Some opened; drag = NotPressed }
            else { model.withActiveSelectCancelled with selection = selection; elementMenu = Some opened; drag = NotPressed; pendingEntry = None; selectStatus = None }
        | TableSelected | NothingSelected -> { model with elementMenu = None; drag = NotPressed }
    | CloseElementMenu -> { model with elementMenu = None }
    | ToggleElementRay (id, branch) -> toggleElementRay id branch model
    | PlaceOnBeam (id, host) -> placeOnBeam id host model
    | SetRotationPivot pivot -> { model with rotationPivot = pivot }
    | PointerDown pt -> { model with drag = Pressed pt }
    | PointerMove pt ->
        match model.drag with
        | NotPressed -> model
        | Pressed start ->
            if (start.distanceTo pt).value < dragThresholdPx.value then model
            else { model with view = TableScene.panViewBy start pt model.view; drag = Panning pt }
        | Panning last -> { model with view = TableScene.panViewBy last pt model.view; drag = Panning pt }
    | PointerUp _ ->
        // A clean click sets the selection (element or table); a pan-drag leaves it unchanged.
        let selection =
            match model.drag with
            | Pressed start -> model.selectionAt start
            | _ -> model.selection
        // Spec 0027 (026): a changed selection clears any pending (unconfirmed) Library bind, so a stale
        // pending choice never carries across elements. Spec 0038 (016): the same change also
        // CANCELS AND CLOSES the open Select-state window (the staleness rule extends the
        // pending-bind-clears precedent) and clears the staleness status line. Spec 0046: any left
        // click or pan on the table closes an open element menu.
        if selection = model.selection then
            { model with selection = selection; drag = NotPressed; elementMenu = None }
        else
            { model.withActiveSelectCancelled with selection = selection; drag = NotPressed; pendingEntry = None; selectStatus = None; elementMenu = None }
    | Wheel (mods, notches) ->
        match wheelAction mods with
        | RotateSel1 -> rotateSelected RotationControls.R1 (notches.turnedBy wheelStepDegrees) model
        | RotateSel2 -> rotateSelected RotationControls.R2 (notches.turnedBy wheelStepDegrees) model
        | RotateSel3 -> rotateSelected RotationControls.R3 (notches.turnedBy wheelStepDegrees) model
        | ZoomElementSelected ->
            match model.selection with
            | ElementSelected i -> zoomElement i notches model
            | TableSelected | NothingSelected -> model
        | ZoomElementsAll -> zoomAllElements notches model
        | ZoomTable -> { model with view = TableScene.zoomViewBy notches model.view }
        | NoWheelAction -> model

// ---------------------------------------------------------------------------
// Colours + the FuncUI view.
// ---------------------------------------------------------------------------

let private brush (c : Color) : IBrush = SolidColorBrush(c) :> IBrush
let private brushA (opacity : RendererControls.RendererOpacity) (c : Color) : IBrush =
    SolidColorBrush(Color.FromArgb(byte (255.0 * opacity.clamped.value), c.R, c.G, c.B)) :> IBrush

let private plateColor = Color.FromRgb(205uy, 205uy, 205uy)
let private plateSelectedColor = Color.FromRgb(150uy, 185uy, 235uy)
let private edgeColor = Color.FromRgb(50uy, 50uy, 50uy)
let private selectedStroke = Color.FromRgb(0uy, 60uy, 160uy)
/// The ray colour. Spec 0046: no element marking shares it (`ElementRenderer.normalAxisColor`).
let rayColor : Color = Color.FromRgb(30uy, 90uy, 200uy)
let private elementColor = Color.FromRgb(60uy, 60uy, 60uy)
let private n1Color = ElementRenderer.normalAxisColor
let private n2Color = Color.FromRgb(210uy, 120uy, 0uy)

let private line (a : ScreenPoint) (b : ScreenPoint) (c : Color) (w : StrokeWidth) : IView =
    Line.create [
        Line.startPoint a.toPoint; Line.endPoint b.toPoint; Line.stroke (brush c); Line.strokeThickness w.value
    ] :> IView

type Setups.SetupSlot with

    /// The element as the test scenes draw it through `view`: its oriented box and its N1 / N2 markers,
    /// heavier and in the selection colour when highlighted.
    member e.testSceneViews (view : TableViewState) (highlight : ElementRenderer.ElementHighlight) : IView list =
        let corners = e.boxCorners |> List.map (fun c -> (TableScene.projectOnScene view c).toPoint) |> List.toArray
        let boxColor =
            match highlight with
            | ElementRenderer.ElementHighlighted -> selectedStroke
            | ElementRenderer.ElementNotHighlighted -> elementColor
        let weight =
            match highlight with
            | ElementRenderer.ElementHighlighted -> StrokeWidth 2.5
            | ElementRenderer.ElementNotHighlighted -> StrokeWidth 1.0
        let normalWidth =
            match highlight with
            | ElementRenderer.ElementHighlighted -> StrokeWidth 3.0
            | ElementRenderer.ElementNotHighlighted -> StrokeWidth 2.0
        let edges =
            TableView.plateEdges
            |> List.map (fun edge ->
                Line.create [
                    Line.startPoint (edge.fromCorner.pick corners); Line.endPoint (edge.toCorner.pick corners); Line.stroke (brush boxColor); Line.strokeThickness weight.value
                ] :> IView)
        let { primaryNormal = n1; secondaryNormal = n2 } = orientedBasis e.placement
        let cx = e.placement.placementPoint.x / 1.0<meter>
        let cy = e.placement.placementPoint.y / 1.0<meter>
        let half = e.zoom.value * (e.placement.box.a2 / 2.0 / 1.0<meter>)
        let centreScr = TableScene.projectOnScene view (Vector3.create cx cy 0.0)
        let along (n : Vector3) (len : float) = TableScene.projectOnScene view (Vector3.create (cx + len * n.x) (cy + len * n.y) (len * n.z))
        // The secondary (roll) normal is drawn toward −N2 so the yellow marker points UP on screen rather
        // than down (a drawing convention only — the element's actual orientation is unchanged).
        edges @ [ line centreScr (along n1 (1.8 * half)) n1Color normalWidth
                  line centreScr (along n2 (-(1.3 * half))) n2Color normalWidth ]

let private rotationHandlers (dispatch : TableAndElementRotationMsg -> unit) : RotationControls.Handlers =
    {
        rotate = fun axis d -> dispatch (match axis with RotationControls.R1 -> RotateR1By d | RotationControls.R2 -> RotateR2By d | RotationControls.R3 -> RotateR3By d)
        setAngle = fun axis v -> dispatch (RotSetAxis (axis, v))
        toggleLock = fun axis -> dispatch (match axis with RotationControls.R1 -> ToggleR1Lock | RotationControls.R2 -> ToggleR2Lock | RotationControls.R3 -> ToggleR3Lock)
        requestReset = fun () -> dispatch RotRequestReset
        requestResetAll = fun () -> dispatch RotRequestResetAll
        confirm = fun () -> dispatch RotConfirm
        cancel = fun () -> dispatch RotCancel
    }

/// Spec 0038 (018): one add-palette BUTTON — its stable code (the `PaletteAdd_<code>` automation-id
/// suffix), its label, the catalogue kind it adds, and the seeded Library entry the new element
/// lands PRE-BOUND to (`None` = unbound: the sample's inverse hook, and the not-bindable lens /
/// mirror kinds).
type PaletteButton =
    {
        code : ConstructorUiKey
        label : ControlCaption
        kind : CatalogueKind
        prebind : ElementValueId option
    }

type TableAndElementRotationModel with

    member internal model.elementViews : IView list =
        model.indexedElements
        |> List.collect (fun (i, e) ->
            let highlight =
                match model.selection with
                | ElementSelected s when s = i -> ElementRenderer.ElementHighlighted
                | _ -> ElementRenderer.ElementNotHighlighted
            e.testSceneViews model.view highlight)

    /// The shared rotation-controls bar's state for whatever is selected (the table, or an element).
    /// When nothing is selected the bar is disabled — there is nothing to rotate (task 008).
    member internal model.rotationState : RotationControls.State =
        // Spec 0046: an element offers R1 / R2 lock toggles too (the light source starts with all three
        // locked); the table locks R3 alone.
        let offer (lock : RotationLock) : RotationControls.AxisLockOffer =
            match lock with
            | RotationLocked -> RotationControls.Lockable RotationControls.AxisLocked
            | RotationFree -> RotationControls.Lockable RotationControls.AxisFree
        let lockState (lock : RotationLock) : RotationControls.AxisLock =
            match lock with
            | RotationLocked -> RotationControls.AxisLocked
            | RotationFree -> RotationControls.AxisFree
        let nothing : RotationControls.State =
            { r1 = BarDegrees 0.0; r2 = BarDegrees 0.0; r3 = BarDegrees 0.0; r1Lock = RotationControls.NotLockable; r2Lock = RotationControls.NotLockable
              r3Lock = RotationControls.AxisFree; availability = ControlWithheld; confirm = model.rotationConfirm }
        match model.selection with
        | TableSelected ->
            { r1 = BarDegrees model.view.r1.degrees; r2 = BarDegrees model.view.r2.degrees; r3 = BarDegrees model.view.r3.degrees
              r1Lock = RotationControls.NotLockable; r2Lock = RotationControls.NotLockable
              r3Lock = lockState model.tableR3Locked; availability = ControlOffered; confirm = model.rotationConfirm }
        | ElementSelected i ->
            match model.tryElementAt i with
            | Some e ->
                let p = e.placement
                { r1 = BarDegrees p.r1.degrees; r2 = BarDegrees p.r2.degrees; r3 = BarDegrees p.r3.degrees
                  r1Lock = offer p.r1Locked; r2Lock = offer p.r2Locked
                  r3Lock = lockState p.r3Locked; availability = ControlOffered; confirm = model.rotationConfirm }
            | None -> nothing
        | NothingSelected -> nothing

    member internal model.readoutText : string =
        match model.selection with
        | TableSelected ->
            $"Selected: TABLE   R1 %.0f{model.view.r1.degrees}°   R2 %.0f{model.view.r2.degrees}°   R3 %.0f{model.view.r3.degrees}°   zoom %.2f{model.view.zoom.value}×"
        | ElementSelected i ->
            let e = List.item i.value model.elements
            let p = e.placement
            // Spec 0027 (024): show the bound Library entry (resolved through the Library proxy), or "unbound".
            let boundName =
                match p.valueId with
                | Some id ->
                    match model.library.tryGetEntry id with
                    | Ok (Some entry) -> entry.displayName
                    | Ok None | Error _ -> "unbound"
                | None -> "unbound"
            // Spec 0046: name every locked rotation axis (the light source starts with all three locked).
            let lockText =
                match [ if p.r1Locked = RotationLocked then "R1"
                        if p.r2Locked = RotationLocked then "R2"
                        if p.r3Locked = RotationLocked then "R3" ] with
                | [] -> "rotations free"
                | locked -> (String.concat " " locked) + " locked"
            $"""Selected: Element %d{i.value + 1} (%s{p.catalogueKind.kindName})   R1 %.0f{p.r1.degrees}°   R2 %.0f{p.r2.degrees}°   R3 %.0f{p.r3.degrees}° (%s{lockText})   zoom %.1f{e.zoom.value}×   bound: %s{boundName}"""
        | NothingSelected -> "Selected: none   (click the table or an element to select it)"

    /// Spec 0038 (018): the palette buttons the model's catalogue palette EXPANDS to. The
    /// circular-polarizer kind expands to the CPL / CPR pair, so the palette offers THREE polarizer
    /// buttons — LP / CPL / CPR — over the UNCHANGED `CatalogueKind`s (LP adds a LinearPolarizer,
    /// CPL and CPR both add a CircularPolarizer; only the label and the pre-bound seed entry differ).
    /// Every other kind keeps its one `kindCode`/`kindName` button with the kind's seeded default.
    /// Public so the mapping is provable without a window.
    member model.paletteButtons : PaletteButton list =
        model.palette
        |> List.collect (fun k ->
            match k with
            | CircularPolarizer ->
                [ { code = ConstructorUiKey "CPL"; label = ControlCaption $"{CircularPolarizer.kindName} (L)"; kind = CircularPolarizer; prebind = Some Library.PresetIds.polarizerCpLeft }
                  { code = ConstructorUiKey "CPR"; label = ControlCaption $"{CircularPolarizer.kindName} (R)"; kind = CircularPolarizer; prebind = Some Library.PresetIds.polarizerCpRight } ]
            | LightSource | LinearPolarizer | Sample | Lens | FlatMirror | CurvedMirror | Detector ->
                [ { code = ConstructorUiKey k.kindCode; label = ControlCaption k.kindName; kind = k; prebind = k.defaultSeedEntry } ])

    /// The add / remove "Lego" palette row — shown ONLY when the scene has a non-empty palette (the Main
    /// screen). The static test windows pass an empty palette, so this row is absent and their UI is
    /// unchanged. It is the shared `ElementPaletteControls` bar, styled to match the rotation bar.
    member internal model.paletteState : ElementPaletteControls.ElementPaletteControlsState =
        {
            addItems = model.paletteButtons |> List.map (fun b -> { ElementPaletteControls.AddItem.id = b.code; label = b.label })
            removeAvailability = (match model.selection with ElementSelected _ -> ControlOffered | _ -> ControlWithheld)
        }

    member internal model.tableCanvas : IView =
        let highlight =
            match model.selection with
            | TableSelected -> PlateSelected
            | ElementSelected _ | NothingSelected -> PlateNotSelected
        Canvas.create [
            Canvas.name ConstructorUiId.tableElementCanvas.value
            Canvas.width tableAndElementRotationCanvasWidth
            Canvas.height tableAndElementRotationCanvasHeight
            Canvas.horizontalAlignment HorizontalAlignment.Left
            Canvas.verticalAlignment VerticalAlignment.Top
            Canvas.children (TableScene.plateViews model.view model.table highlight @ TableScene.sourceDetectorRayViews model.view @ model.elementViews)
        ] :> IView

let private paletteHandlers (model : TableAndElementRotationModel) (dispatch : TableAndElementRotationMsg -> unit) : ElementPaletteControls.ElementPaletteControlsHandlers =
    {
        add = fun id -> model.paletteButtons |> List.tryFind (fun b -> b.code = id) |> Option.iter (fun b -> dispatch (AddElementBoundTo (b.kind, b.prebind)))
        removeSelected = fun () -> dispatch RemoveSelectedElement
    }

let private addRemoveBar (model : TableAndElementRotationModel) (dispatch : TableAndElementRotationMsg -> unit) : IView list =
    if List.isEmpty model.palette then []
    else [ ElementPaletteControls.elementPaletteControlsView model.paletteState (paletteHandlers model dispatch) ]

let private controlBar (model : TableAndElementRotationModel) (dispatch : TableAndElementRotationMsg -> unit) : IView =
    StackPanel.create [
        StackPanel.orientation Orientation.Vertical
        StackPanel.spacing 6.0
        StackPanel.margin (Thickness 8.0)
        StackPanel.children
            (addRemoveBar model dispatch
             @ [ RotationControls.view model.rotationState (rotationHandlers dispatch)
                 TextBlock.create [ TextBlock.name ConstructorUiId.teReadout.value; TextBlock.text model.readoutText ]
                 TextBlock.create [
                     TextBlock.foreground (brush (Color.FromRgb(100uy, 100uy, 100uy)))
                     TextBlock.text "click TABLE or an element to select it (rotation acts on the selection) · drag = pan · wheel = zoom table · Shift/Ctrl+Shift/Alt+wheel = R1/R2/R3 of the selection · Ctrl+Alt+wheel = zoom element · Ctrl+Alt+Shift+wheel = zoom all · Shift+button = 5°"
                 ] ])
    ] :> IView

let private wheelModifiers (km : KeyModifiers) : Set<WheelModifier> =
    [ if km.HasFlag KeyModifiers.Control then ModCtrl
      if km.HasFlag KeyModifiers.Shift then ModShift
      if km.HasFlag KeyModifiers.Alt then ModAlt ]
    |> Set.ofList

let tableAndElementRotationView (model : TableAndElementRotationModel) (dispatch : TableAndElementRotationMsg -> unit) : IView =
    let toScreen (e : PointerEventArgs) : ScreenPoint = SceneInput.canvasPoint ConstructorUiId.tableElementCanvas e
    DockPanel.create [
        DockPanel.children [
            Border.create [ Border.dock Dock.Top; Border.child (controlBar model dispatch) ]
            Border.create [
                Border.background (brush (Color.FromRgb(250uy, 250uy, 250uy)))
                Border.onPointerPressed (fun e -> e.Handled <- true; dispatch (PointerDown (toScreen e)))
                Border.onPointerMoved (fun e -> e.Handled <- true; dispatch (PointerMove (toScreen e)))
                Border.onPointerReleased (fun e -> e.Handled <- true; dispatch (PointerUp (toScreen e)))
                Border.onPointerWheelChanged (fun e ->
                    e.Handled <- true
                    dispatch (Wheel (wheelModifiers e.KeyModifiers, WheelNotches (if e.Delta.Y >= 0.0 then 1 else -1))))
                Border.child model.tableCanvas
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

/// The placement each Main element is DRAWN with: its snapped centre, oriented on its beam
/// (`RayModel.orientedToBeam` — the beam's absolute R2/R3 plus the element's own dialled R2/R3, exactly the
/// orientation the beam layout crosses it at). The source keeps its own orientation.
let drawnPlacement (m : TableAndElementRotationModel) (i : SceneElementIndex) : ElementPlacement =
    let e = List.item i.value m.elements
    let node = (fst m.mainSnap).[i]
    let centred = { e.placement with placementPoint = { x = node.position.x * 1.0<meter>; y = node.position.y * 1.0<meter> } }
    match node.incoming with
    | Some dir when e.placement.catalogueKind.autoOrientsToBeam -> RayModel.orientedToBeam dir centred
    | _ -> centred

type TableAndElementRotationModel with

    /// The table plate (FILL driven by the render face-opacity knob, task 018) plus the SNAPPED beam — the
    /// polyline from the source through every downstream element, bending at mirrors (task — the Main window
    /// follows the snap approach, so adding a mirror reflects the beam). The elements (incl. source / detector)
    /// draw themselves through the renderer, so no separate source / detector markers here.
    member internal model.mainTableViews : IView list =
        let corners = model.table.plateCorners3D |> List.map (TableScene.projectOnScene model.view) |> List.toArray
        let tableSel = (model.selection = TableSelected)
        let topFace = corners.[0 .. 3] |> Array.map (fun c -> c.toPoint) |> Array.toList
        let face =
            Polygon.create [
                Polygon.points topFace
                Polygon.fill (brushA model.render.faceOpacity (if tableSel then plateSelectedColor else plateColor))
                Polygon.stroke (brush (if tableSel then selectedStroke else edgeColor))
                Polygon.strokeThickness (if tableSel then 3.0 else 1.5)
            ] :> IView
        let edgeWidth = if tableSel then StrokeWidth 2.0 else StrokeWidth 1.0
        let edges =
            TableView.plateEdges
            |> List.map (fun edge -> line (edge.fromCorner.pick corners) (edge.toCorner.pick corners) edgeColor edgeWidth)
        // Spec 0046: the ray as the beam layout drew it — free-space runs face to face, and the refracted run
        // INSIDE each transmitting element (between the faces the element is drawn with), so a tilted element
        // shows the three segments: in, through (bent), and out parallel to the incoming ray.
        let beam =
            snd model.mainSnap
            |> List.map (fun s -> line (TableScene.projectOnScene model.view s.start) (TableScene.projectOnScene model.view s.finish) rayColor (StrokeWidth 2.0))
        (face :: edges) @ beam

    /// The elements drawn through the chosen renderer at their snapped, auto-oriented placements (zoom is the
    /// element's own).
    member internal model.mainElementViews : IView list =
        let renderer = ElementRenderer.rendererOf model.render
        let project = TableScene.projectOnScene model.view
        let nodes = fst model.mainSnap
        model.indexedElements
        |> List.collect (fun (i, e) ->
            // The CENTRE is the full 3-D snapped position (so an element snapped out of the table plane after an
            // R3-tilted mirror is drawn off the plane); `drawnPlacement` carries the orientation. The binding
            // state is derived from that placement's `valueId`, so an unbound element draws dashed-and-ghosted.
            // Spec 0046: the orientation markers only on the SELECTED element — an unselected element's N1 lies on
            // the beam at normal incidence and ran across the next element's faces, where it read as the ray.
            let placement = drawnPlacement model i
            let highlight, normals =
                match model.selection with
                | ElementSelected s when s = i -> ElementRenderer.ElementHighlighted, ElementRenderer.ShowNormals
                | _ -> ElementRenderer.ElementNotHighlighted, ElementRenderer.HideNormals
            renderer.draw project highlight
                { placement = placement; centre = nodes.[i].position; zoom = e.zoom; opticalSign = e.placement.catalogueKind.opticalSign; normals = normals })

    /// The MOVE bay state/handlers: slide the selected element along the beam (disabled unless an element is
    /// selected). The clamp is the host's: ±half the plate, none for a detector (spec 0046).
    member internal model.moveState : RayPositionControls.RayPositionControlsState =
        let withheld : RayPositionControls.RayPositionControlsState =
            { position = BarMeters 0.0; range = RayPositionControls.UnboundedRange; availability = ControlWithheld }
        match model.selection with
        | ElementSelected i ->
            match model.tryElementAt i with
            | Some e -> { position = BarMeters e.elementX; range = alongBeamRange model i; availability = ControlOffered }
            | None -> withheld
        | TableSelected | NothingSelected -> withheld

let private moveHandlers (dispatch : TableAndElementRotationMsg -> unit) : RayPositionControls.RayPositionControlsHandlers =
    {
        moveBy = fun dx -> dispatch (SlideSelectedBy dx)
        setPosition = fun x -> dispatch (SlideSelectedTo x)
        reset = fun () -> dispatch ResetSelectedPosition
    }

type TablePositionControls.Axis with

    /// Spec 0046: the Controls-layer ⇄ domain mirror of the table axes (the bar is domain-free).
    member private axis.tableAxis : TableAxis =
        match axis with
        | TablePositionControls.AxisX -> TableX
        | TablePositionControls.AxisY -> TableY
        | TablePositionControls.AxisZ -> TableZ

type ElementPlacement with

    /// Spec 0046: the free table-position bar's state for the selected light source — its x / y / height and
    /// the three move locks.
    member private p.sourcePositionState : TablePositionControls.TablePositionControlsState =
        let axisState (axis : TableAxis) : TablePositionControls.AxisState =
            { position = BarMeters (positionOn axis p / 1.0<meter>)
              lock =
                match moveLockOf axis p with
                | MoveLocked -> RotationControls.AxisLocked
                | MoveFree -> RotationControls.AxisFree }
        { x = axisState TableX; y = axisState TableY; z = axisState TableZ; availability = ControlOffered }

let private sourcePositionHandlers (dispatch : TableAndElementRotationMsg -> unit) : TablePositionControls.TablePositionControlsHandlers =
    {
        moveBy = fun axis d -> dispatch (MoveSelectedBy (axis.tableAxis, d))
        setPosition = fun axis v -> dispatch (MoveSelectedTo (axis.tableAxis, v))
        toggleLock = fun axis -> dispatch (ToggleMoveLock axis.tableAxis)
        reset = fun () -> dispatch ResetSelectedPosition
    }

/// The Move bay (spec 0046): the light source — placed freely — gets the x / y / z bar with its move locks;
/// every other element (snapped to its beam) the along-beam bar. Each is KEYED, so switching the selection
/// between them recreates the bar rather than patching one's named fields into the other's.
let private moveBayContent (model : TableAndElementRotationModel) (dispatch : TableAndElementRotationMsg -> unit) : IView =
    let freeSource =
        match model.selection with
        | ElementSelected i ->
            match model.tryElementAt i with
            | Some e ->
                match e.placement.catalogueKind.mobility with
                | MovesFreely -> Some e.placement
                | MovesAlongBeam -> None
            | None -> None
        | TableSelected | NothingSelected -> None
    match freeSource with
    | Some p ->
        Border.create [ Border.child (TablePositionControls.tablePositionControlsView p.sourcePositionState (sourcePositionHandlers dispatch)) ]
        |> Avalonia.FuncUI.DSL.View.withKey "MoveBayFree"
        :> IView
    | None ->
        Border.create [ Border.child (RayPositionControls.rayPositionControlsView model.moveState (moveHandlers dispatch)) ]
        |> Avalonia.FuncUI.DSL.View.withKey "MoveBayAlongBeam"
        :> IView

let private renderHandlers (dispatch : TableAndElementRotationMsg -> unit) : RendererControls.RendererControlsHandlers =
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

/// The Library entry ids valid for `kind` (the kind-constrained set; empty on a proxy error).
let private allowedEntryIds (model : TableAndElementRotationModel) (kind : CatalogueKind) : Set<ElementValueId> =
    match model.library.entriesForKind kind with
    | Ok entries -> entries |> List.map (fun e -> e.entryId) |> Set.ofList
    | Error _ -> Set.empty

type Library.TreeLabel with

    /// The label as the caption of a Select-bay row.
    member this.rowCaption : ControlCaption = ControlCaption this.value

type LibraryControls.LibraryRowDepth with

    /// The depth of this row's children.
    member private this.deeper : LibraryControls.LibraryRowDepth = LibraryControls.LibraryRowDepth (this.value + 1)

type Library.LibraryTreeNode with

    /// Flatten this grouping tree to `LibraryControls.Row`s, KIND-CONSTRAINED: emit a leaf row only when its
    /// entry id is in `allowed`, and a group header only if it has a surviving descendant leaf (§2a — keep
    /// the tree shape but show only entries valid for the selected element). `boundId` highlights the bound
    /// leaf.
    member private node.selectRows (allowed : Set<ElementValueId>) (boundId : ElementValueId option) (depth : LibraryControls.LibraryRowDepth) : LibraryControls.Row list =
        match node with
        | Library.Leaf (label, entryId) ->
            if Set.contains entryId allowed then
                let binding = if boundId = Some entryId then LibraryControls.EntryBound else LibraryControls.EntryUnbound
                [ { label = label.rowCaption; depth = depth; kind = LibraryControls.LibraryEntryLeaf (entryId.uiKey, binding) } ]
            else []
        | Library.Group (label, children) ->
            let childRows = children |> List.collect (fun child -> child.selectRows allowed boundId depth.deeper)
            if List.isEmpty childRows then []
            else { label = label.rowCaption; depth = depth; kind = LibraryControls.LibraryGroupHeader } :: childRows

/// Spec 0038 (017): what the Select bay offers for the CURRENT selection — the inline
/// quick-pick strip beside the Choose… verb (the kind-constrained entry count sits BELOW the
/// step-005 threshold), the Choose… verb alone (at or above it — the full Select-state Library
/// window is the picking surface there), or nothing bindable (table / nothing selected — the
/// bay keeps its disabled prompt). A named three-case DU, never a bool pair.
type SelectorOffer =
    | QuickPickAndChoose
    | ChooseAlone
    | NoSelectorOffer

type TableAndElementRotationModel with

    /// The entry ids the Select bay offers the selected element (its kind's; none without a selected element).
    member internal model.selectableEntryIds : Set<ElementValueId> =
        match model.selection with
        | ElementSelected i ->
            match model.tryElementAt i with
            | Some e -> allowedEntryIds model e.placement.catalogueKind
            | None -> Set.empty
        | TableSelected | NothingSelected -> Set.empty

    /// The Select bay state for the current selection: an `ElementSelected` shows the kind-constrained
    /// tree rows, the kind label, and the bound-entry readout; anything else disables the bay.
    member internal model.libraryState : LibraryControls.LibraryControlsState =
        match model.selection with
        | ElementSelected i ->
            let e = List.item i.value model.elements
            let kind = e.placement.catalogueKind
            let allowed = allowedEntryIds model kind
            let rows =
                match model.library.libraryTrees () with
                | Ok (tree :: _) -> tree.root.selectRows allowed e.placement.valueId (LibraryControls.LibraryRowDepth 0)
                | Ok [] | Error _ -> []
            let boundName =
                match e.placement.valueId with
                | Some id ->
                    match model.library.tryGetEntry id with
                    | Ok (Some entry) -> Some (ControlCaption entry.displayName)
                    | Ok None | Error _ -> None
                | None -> None
            // Spec 0027 (026): the pending (selected-not-confirmed) entry's name + full description, resolved
            // through the proxy (so a stale pending id yields no panel).
            let pendingName, pendingDescription =
                match model.pendingEntry with
                | Some id ->
                    match model.library.tryGetEntry id with
                    | Ok (Some entry) -> Some (ControlCaption entry.displayName), ControlCaption entry.fullDescription
                    | Ok None | Error _ -> None, ControlCaption ""
                | None -> None, ControlCaption ""
            {
                rows = rows
                kindLabel = Some (ControlCaption kind.kindName)
                boundName = boundName
                availability = ControlOffered
                pendingEntryId = model.pendingEntry |> Option.map (fun id -> id.uiKey)
                pendingName = pendingName
                pendingDescription = pendingDescription
            }
        | TableSelected | NothingSelected -> LibraryControls.empty

    /// The bay's threshold decision (public so the gating is provable without a window). The count
    /// is the kind-constrained entry set the strip's rows are built from (`entriesForKind` through
    /// the read-only Library seam — the same set `libraryState` flattens), compared against the
    /// model's `quickPickThreshold`: strictly below → the strip renders; at or above → Choose… alone.
    member model.selectorOffer : SelectorOffer =
        match model.selection with
        | ElementSelected i ->
            match model.tryElementAt i with
            | Some e ->
                if Set.count (allowedEntryIds model e.placement.catalogueKind) < model.quickPickThreshold.value then QuickPickAndChoose
                else ChooseAlone
            | None -> NoSelectorOffer
        | TableSelected | NothingSelected -> NoSelectorOffer

/// The Select bay's handlers. A clicked row's key is resolved against the entries the bay offers, so the
/// pending choice is always one of them.
let private libraryHandlers (model : TableAndElementRotationModel) (dispatch : TableAndElementRotationMsg -> unit) : LibraryControls.LibraryControlsHandlers =
    {
        selectEntry = fun key -> model.selectableEntryIds |> Seq.tryFind (fun id -> id.uiKey = key) |> Option.iter (fun id -> dispatch (RequestBindValueId id))
        confirmEntry = fun () -> dispatch ConfirmBindValueId
        cancelEntry = fun () -> dispatch CancelBindValueId
    }

// ---------------------------------------------------------------------------
// Main-screen EXPERIMENTS bay (spec 0027 / 028): the multi-step, editable experiment builder + collection.
// The SETUP is the live scene; the user picks the element to vary (the element's slot — via
// `SetupSlot.scanVariablesWith` DATA — determines the allowed variables), the T/R/both capture, and the
// range, then Adds to a persistent collection (edit / remove supported). The bay is domain-free, so the
// host flattens the scene into candidates, the draft into the editor state, and the collection into rows.
// ---------------------------------------------------------------------------

/// What one element is, as its hover tooltip and the element menu's header show it (spec 0046).
type ElementInfo =
    {
        /// Its kind and scene number.
        heading : ControlCaption
        /// The Library entry it is bound to with that entry's full description, or that it is still unbound.
        detail : ControlCaption
    }

type TableAndElementRotationModel with

    /// One present element as a candidate-to-vary — its serializable id plus a human-readable label
    /// ("<kind> #<n>", 1-based, in scene order).
    member internal model.sweepCandidates : ExperimentControls.SweepCandidate list =
        model.indexedElements
        |> List.map (fun (i, e) -> { ExperimentControls.SweepCandidate.elementId = e.id.uiKey; label = ControlCaption $"{e.placement.catalogueKind.kindName} #{i.value + 1}" })

    /// The current draft experiment's readout (spec 028), or none when the draft is incomplete / its element
    /// is no longer present.
    member internal model.experimentReadout : ControlCaption option =
        let draft = model.experimentCollection.draft
        match draft.elementId, draft.variable with
        | Some id, Some v when model.elements |> List.exists (fun e -> e.id = id) ->
            Some (ControlCaption $"Experiment: %s{draft.elementLabel.value} — vary %s{v.label} over %g{draft.range.min.value}…%g{draft.range.max.value} %s{v.unitLabel} (%d{draft.range.points.value} pts), capture %s{draft.measurement.label}")
        | _ -> None

// ---------------------------------------------------------------------------
// Spec 0027 (024): run ONE experiment from the LIVE scene. Every bound element on a detector's path is evaluated
// through `ItemEvaluation` and folded in light order from the default source emission (spec 0050 S8.1).
// ---------------------------------------------------------------------------

/// Resolve an element's bound Library entry (through the Library proxy), if it has a `valueId` that binds.
let private boundEntry (model : TableAndElementRotationModel) (e : Setups.SetupSlot) : Library.LibraryEntry option =
    match e.placement.valueId with
    | Some id ->
        match model.library.tryGetEntry id with
        | Ok (Some entry) -> Some entry
        | Ok None | Error _ -> None
    | None -> None

/// What element `i` is (spec 0046): its kind and scene number, then the Library entry it is bound to with
/// that entry's full description — or that it is still unbound. The table's hover tooltip and the element
/// menu's header show exactly this. Public so it is provable without a window.
let elementInfo (model : TableAndElementRotationModel) (i : SceneElementIndex) : ElementInfo =
    let e = List.item i.value model.elements
    let detail =
        match boundEntry model e with
        | Some entry -> $"{entry.displayName} — {entry.fullDescription}"
        | None -> "Unbound — right-click it and Choose… what it is."
    { heading = ControlCaption $"{e.placement.catalogueKind.kindName} #{i.value + 1}"; detail = ControlCaption detail }

type TableAndElementRotationModel with

    /// The source wavelength from the first light-source element bound to a source preset (else 600 nm — an
    /// absent / unbound source is not synthesized, just defaulted; spec R1).
    member internal model.runWaveLength : WaveLength =
        operatingWaveLength model model.experimentCollection.draft.sourceWaveLengthOpt

    /// The wavelengths the scene currently requests (spec 0038 step 031): the Experiments-bay draft's
    /// wavelength sweep (its nm range) when one is being configured, otherwise the fixed source λ (the
    /// existing `runWaveLength`, which defaults to 600 nm for an absent source).
    member internal model.sceneRequestedWavelengths : OutOfBandDiagnostic.RequestedWavelengths =
        OutOfBandDiagnostic.requestedWavelengthsFor
            model.runWaveLength
            model.experimentCollection.draft.variable
            model.experimentCollection.draft.range

// ---------------------------------------------------------------------------
// Spec 0038 (031): the OUT-OF-BAND dispersion diagnostic overlay. Independent of the step-30
// bound/unbound cue: for every element the materials reachable through its binding are checked
// against the wavelengths the scene requests (the Experiments-bay draft's wavelength sweep, or the
// fixed source λ). A material whose DEFINED dispersion segments do not cover the request raises a
// small warning badge with a hover tooltip naming the offending material(s) and both ranges; the
// same text lands in the Details bay. The diagnostic itself is the pure Domain
// `OutOfBandDiagnostic`; the host only resolves reachability and renders the badge.
// ---------------------------------------------------------------------------

type Library.Sample with

    /// Every pinned material VERSION the sample's structure references (its film layers — a repeat cell
    /// counted once — its substrate plate, and its lower half-space), deduplicated.
    member private s.materialVersions : MaterialLibrary.MaterialVersionId list =
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
let private reachableMaterialEntries (model : TableAndElementRotationModel) (e : Setups.SetupSlot) : MaterialLibrary.MaterialEntry list =
    match boundEntry model e with
    | Some (Library.SampleItem s) ->
        s.materialVersions
        |> List.choose (fun mvid ->
            match model.materials.resolveVersion mvid with
            | Ok (Some entry) -> Some entry
            | Ok None | Error _ -> None)
    | Some _ | None -> []

/// The out-of-band warning text for element `e`, if any (spec 0038 step 031). Public so the Details
/// bay and the badge overlay share ONE derivation and a headless test can drive it directly.
let outOfBandWarningFor (model : TableAndElementRotationModel) (e : Setups.SetupSlot) : ControlCaption option =
    (OutOfBandDiagnostic.checkMaterialsOutOfBand (reachableMaterialEntries model e) model.sceneRequestedWavelengths).warningOpt
    |> Option.map ControlCaption

type ControlCaption with

    /// The caption as a hover tooltip (Avalonia's attached `ToolTip.Tip`), set the attr-builder way.
    member private this.toolTipAttribute<'Control when 'Control :> Control> () : IAttr<'Control> =
        AttrBuilder<'Control>.CreateProperty<obj>(ToolTip.TipProperty, box this.value, ValueNone)

/// The out-of-band warning badge for element `i`, drawn at its projected centre `c` (spec 0038
/// step 031): a small "⚠" glyph with a hover tooltip = `text` (the offending material(s) and both
/// ranges) and a per-index AutomationId so a headless test can address it.
let outOfBandBadge (i : SceneElementIndex) (c : ScreenPoint) (text : ControlCaption) : IView =
    TextBlock.create [
        TextBlock.left (c.sx - 18.0)
        TextBlock.top (c.sy - 26.0)
        TextBlock.text "⚠"
        TextBlock.fontWeight FontWeight.Bold
        TextBlock.foreground (brush (Color.FromRgb(200uy, 60uy, 40uy)))
        (ConstructorUiId.outOfBandBadge i.uiPosition).automationAttribute<TextBlock> ()
        text.toolTipAttribute<TextBlock> ()
    ] :> IView

type TableAndElementRotationModel with

    /// The out-of-band warning badges for every flagged element in the scene (spec 0038 step 031). Public
    /// so a headless test renders them without mounting the whole workbench; appended to the main canvas.
    member model.outOfBandBadges : IView list =
        let centres = model.snappedCentres
        model.indexedElements
        |> List.collect (fun (i, e) ->
            match outOfBandWarningFor model e with
            | Some text ->
                Map.tryFind i centres
                |> Option.map (fun centre -> outOfBandBadge i (TableScene.projectOnScene model.view centre) text)
                |> Option.toList
            | None -> [])

type Library.PolarizerPreset with

    /// The ideal polarizer kind of a bound polarizer entry, when its behaviour is `ComputedIdeal` (spec 0038
    /// Part F). The rotate-R1 / sweep runners below drive the kind-typed pipeline sweeps, so a
    /// `ConstantMueller` polarizer — no seed and no editor can produce one yet — is skipped exactly like a
    /// non-polarizer element until those runners go behaviour-typed (Part G reworks this resolution anyway).
    member private p.idealKindOpt : Library.PolarizerKind option =
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
    "Cannot run the rotating-analyzer experiment: no element in the scene is bound to a polarizer entry. Add a polarizer (LP / CPL / CPR) or bind one through the Select bay."

/// The analyzer's polarizer kind for the rotate-R1 experiment: the VARIED element if it is bound to a
/// polarizer preset, else the last polarizer bound in the scene, else the typed `NoAnalyzerPresent`
/// status (spec 0038 step 018 — the silent IdealLinear fallback is retired; the bound entry's kind
/// is what runs). Public so the no-silent-fallback discipline is provable without a window.
let runAnalyzerKind (model : TableAndElementRotationModel) (varied : Library.ElementId option) : AnalyzerResolution =
    let variedKind =
        match varied with
        | Some chosen ->
            model.elements
            |> List.tryFind (fun e -> e.id = chosen)
            |> Option.bind (fun e ->
                match boundEntry model e with
                | Some (Library.PolarizerItem p) -> p.idealKindOpt
                | _ -> None)
        | None -> None
    let sceneKind =
        model.elements
        |> List.choose (fun e ->
            match boundEntry model e with
            | Some (Library.PolarizerItem p) -> p.idealKindOpt
            | _ -> None)
        |> List.tryLast
    match variedKind, sceneKind with
    | Some k, _ -> ResolvedAnalyzer k
    | None, Some k -> ResolvedAnalyzer k
    | None, None -> NoAnalyzerPresent

type MaterialLibrary.MaterialError with

    /// The human-readable message a failed sample-material resolution surfaces on the chart. Every error
    /// case carries a diagnostic `reason` (spec 0033 steps 002/003); only `UnknownMaterialIdErr` can actually
    /// arise from resolution — the write-seam cases (step 003) render a generic library message.
    member private err.runFailureCaption : ControlCaption =
        match err with
        | MaterialLibrary.UnknownMaterialIdErr reason ->
            ControlCaption $"Cannot run: the sample references an unknown material ({reason})."
        | MaterialLibrary.DuplicateMaterialIdErr reason
        | MaterialLibrary.MaterialStillReferencedErr reason
        | MaterialLibrary.MaterialVersionPinnedErr reason
        | MaterialLibrary.MaterialStoreUnavailableErr reason
        | MaterialLibrary.InvalidMaterialErr reason ->
            ControlCaption $"Cannot run: material library error ({reason})."

/// A `ChartSeries` from named points.
let private series (name : ControlCaption) (points : ExperimentControls.ChartPoint list) : ExperimentControls.ChartSeries =
    { ExperimentControls.ChartSeries.name = name; points = points }

type Experiments.MeasurementMode with

    /// The scene branches an experiment captures, each with a short tag for its series name (spec 028).
    member internal m.capturedBranches : (BeamTree.BeamBranch * ControlCaption) list =
        match m with
        | Experiments.CaptureTransmitted -> [ BeamTree.TransmittedBeam, ControlCaption "T" ]
        | Experiments.CaptureReflected -> [ BeamTree.ReflectedBeam, ControlCaption "R" ]
        | Experiments.CaptureBoth -> [ BeamTree.TransmittedBeam, ControlCaption "T"; BeamTree.ReflectedBeam, ControlCaption "R" ]

    /// A series display name: the base name, suffixed with the branch tag only when capturing BOTH branches.
    member internal m.seriesCaption (branchTag : ControlCaption) (baseName : ControlCaption) : ControlCaption =
        match m with
        | Experiments.CaptureBoth -> ControlCaption $"{baseName.value} ({branchTag.value})"
        | Experiments.CaptureTransmitted | Experiments.CaptureReflected -> baseName

type Library.DetectorKind with

    /// What a detector kind reads, as prose (spec 0046 — the detector decides what is captured).
    member private kind.readsText : string =
        match kind with
        | Library.Intensity -> "intensity"
        | Library.Ellipsometer -> "Ψ, Δ (ellipsometer)"

type TableAndElementRotationModel with

    /// Spec 0046: a capture's label in the Experiments bay — the branch and what the detector on it reads.
    member model.captureLabel (varied : Library.ElementId) (capture : Experiments.MeasurementMode) : ControlCaption =
        let detectors = model.captureDetectors varied
        let readsOn (branch : BeamTree.BeamBranch) : string =
            detectors
            |> List.tryFind (fun (b, _) -> b = branch)
            |> Option.map (fun (_, j) -> (model.detectorKindAt j).readsText)
            |> Option.defaultValue "no detector"
        match capture with
        | Experiments.CaptureTransmitted -> ControlCaption $"Transmitted (T) — {readsOn BeamTree.TransmittedBeam}"
        | Experiments.CaptureReflected -> ControlCaption $"Reflected (R) — {readsOn BeamTree.ReflectedBeam}"
        | Experiments.CaptureBoth -> ControlCaption $"Both — T: {readsOn BeamTree.TransmittedBeam}; R: {readsOn BeamTree.ReflectedBeam}"

type BeamLayout.PlacedElement with

    /// The incidence angle the beam meets this placed element at: the angle between its DRAWN face normal
    /// and the arriving beam (spec 0046 — its R2 for an in-plane tilt), kept computable (below 90°).
    member private placed.incidenceOn (placement : ElementPlacement) : IncidenceAngle =
        let degrees =
            match placed.incoming with
            | Some d ->
                let n = r1Axis (RayModel.orientedToBeam d placement)
                let c = min 1.0 (abs (n.normalized.dot d.normalized).value)
                min Propagation.r2SweepMaxDegrees.value (acos c * 180.0 / System.Math.PI)
            | None -> 0.0
        IncidenceAngle.create (Angle.degree (degrees * 1.0<deg>))

/// One step of a detector's light path as a run builds it (spec 0046): the same for a whole scan block, or evaluated at
/// each point's settings, so a sample is re-solved only when something it depends on varies (spec 0050 S8.1, S9.1).
type private StepBuilder =
    | FixedStep of ExperimentRun.RunStep
    | SweptStep of (Experiments.ScanSetting list -> Result<ExperimentRun.RunStep, ItemEvaluationError>)

/// One drawn scan block (spec 0050 S9.1): its series, what its detectors read, and the title and description a scan of
/// that block's x axis alone shows.
type private ScanBlockChart =
    {
        blockSeries : ExperimentControls.ChartSeries list
        kinds : Library.DetectorKind list
        variable : Experiments.VariableParameter
        title : ControlCaption
        description : ControlCaption
    }

/// The chart of a scan (spec 0046; spec 0050 S8.1, S8.2, S9.1): per block, each captured branch's detector reads `runEmission`
/// through its path in light order, one series per value of the other axes over the last axis; one block of one axis is the
/// one-variable chart, and the first block that cannot be drawn shows why in its place.
let chartForRun (model : TableAndElementRotationModel) (scan : Experiments.ExperimentScan) : ExperimentChart.ExperimentChart =
    let layout = model.sceneLayout
    let w0 = operatingWaveLength model scan.sourceWaveLengthOpt
    let indexOf (id : Library.ElementId) : int option = model.elements |> List.tryFindIndex (fun e -> e.id = id)
    let boundOf (e : Setups.SetupSlot) : Library.LibraryEntry option =
        match e.placement.valueId with
        | Some id ->
            match model.library.tryGetEntry id with
            | Ok entry -> entry
            | Error _ -> None
        | None -> None
    let sweptAngle (x : float) : Angle = Angle.degree (x * 1.0<deg>)
    // An incidence axis stays computable: its range held to [0, r2SweepMaxDegrees], low end first (spec 0046).
    let computable (axis : Experiments.ScanAxis) : Experiments.ScanAxis =
        match axis.variable with
        | Experiments.VaryR2 ->
            let range = axis.range
            {
                axis with
                    range =
                        {
                            range with
                                min = Propagation.SweptValue (max 0.0 (min range.min.value range.max.value))
                                max = Propagation.SweptValue (min Propagation.r2SweepMaxDegrees.value (max range.min.value range.max.value))
                        }
            }
        | Experiments.VaryR1 | Experiments.VaryDialReading | Experiments.VaryWaveLength -> axis
    // The point's setting on that element over one of those variables, the innermost axis first (spec 0050 S9.1).
    let settingOn (settings : Experiments.ScanSetting list) (id : Library.ElementId) (variables : Experiments.VariableParameter list) : Experiments.ScanSetting option =
        settings |> List.tryFindBack (fun setting -> setting.elementId = id && List.contains setting.variable variables)
    let waveLengthAt (settings : Experiments.ScanSetting list) : WaveLength =
        match settings |> List.tryFindBack (fun setting -> setting.variable = Experiments.VaryWaveLength) with
        | Some setting -> WaveLength.nm (setting.value.value * 1.0<nm>)
        | None -> w0
    // One path step → its builder (or nothing: the source, a mirror, a lens, an unbound element), swept when an axis of the
    // block is on it or varies the wavelength. A slot holding an item is read from `slotItems`, which never holds the
    // source's: that item emits (spec 0050 S8.2, S9.1).
    let builderFor
        (axes : Experiments.ScanAxis list)
        (slotItems : Map<Library.ElementId, Item * ItemModelChoice>)
        (step : BeamLayout.PathStep)
        : Result<StepBuilder option, ItemEvaluationError> =
        match indexOf step.element with
        | None -> Ok None
        | Some j ->
            let e = List.item j model.elements
            let touched = axes |> List.exists (fun axis -> axis.elementId = e.id || axis.variable = Experiments.VaryWaveLength)
            let branch =
                match step.branch with
                | BeamTree.TransmittedBeam -> Propagation.BranchTransmitted
                | BeamTree.ReflectedBeam -> Propagation.BranchReflected
            // A dial-mounted item's reading turns it to its physical angle, any other rotation setting to the value itself,
            // as a slot with no dial reads its R1 (`SetupSlot.turnAngle`); else its drawn R1 (spec 0050 S9.1; 001 §I).
            let dialOpt = e.tryFindDialMounted model.kitItems
            let rotationAt (settings : Experiments.ScanSetting list) : Angle =
                match settingOn settings e.id [ Experiments.VaryR1; Experiments.VaryDialReading ] with
                | Some setting ->
                    match setting.variable, dialOpt with
                    | Experiments.VaryDialReading, Some (item, sign) -> sign.physicalAngleOf item.dialZeroAngle (sweptAngle setting.value.value)
                    | Experiments.VaryDialReading, None
                    | (Experiments.VaryR1 | Experiments.VaryR2 | Experiments.VaryWaveLength), _ -> sweptAngle setting.value.value
                | None -> e.placement.r1
            // At its drawn incidence and R1 and the operating wavelength, each unless the point sets it (spec 0050 S8.1, S9.1).
            let sweptOrFixed (bound : BoundOpticalElement) (stepOf : MuellerMatrix -> ExperimentRun.RunStep) : Result<StepBuilder, ItemEvaluationError> =
                let incidence0 = (List.item j layout.elements).incidenceOn e.placement
                let stepAt (incidence : IncidenceAngle) (rotation : Angle) (w : WaveLength) : Result<ExperimentRun.RunStep, ItemEvaluationError> =
                    let orientation : ElementOrientation = { rotation = rotation; incidence = incidence; branch = branch }
                    bound.muellerAt orientation w |> Result.map stepOf
                let incidenceAt (settings : Experiments.ScanSetting list) : IncidenceAngle =
                    match settingOn settings e.id [ Experiments.VaryR2 ] with
                    | Some setting -> IncidenceAngle.create (sweptAngle setting.value.value)
                    | None -> incidence0
                if touched then Ok (SweptStep (fun settings -> stepAt (incidenceAt settings) (rotationAt settings) (waveLengthAt settings)))
                else stepAt incidence0 e.placement.r1 w0 |> Result.map FixedStep
            match Map.tryFind e.id slotItems, e.placement.catalogueKind, boundOf e with
            | Some (item, choice), kind, _ ->
                // The item's own model over its own values, bound once; an object in a sample slot (spec 0050 S8.2).
                let stepOf : MuellerMatrix -> ExperimentRun.RunStep =
                    match kind with
                    | Sample -> ExperimentRun.ObjectStep
                    | LightSource | LinearPolarizer | CircularPolarizer | Lens | FlatMirror | CurvedMirror | Detector -> ExperimentRun.ElementStep
                choice.boundWith model.materials (ParameterValues.ofParameters item.parameters)
                |> Result.bind (fun bound -> sweptOrFixed bound stepOf)
                |> Result.map Some
            | None, (LinearPolarizer | CircularPolarizer), Some (Library.PolarizerItem p) ->
                let preset = BoundPolarizerPreset p.behavior
                let elementAt (rotation : Angle) (w : WaveLength) : Result<ExperimentRun.RunStep, ItemEvaluationError> =
                    let orientation : ElementOrientation = { rotation = rotation; incidence = IncidenceAngle.normal; branch = branch }
                    preset.muellerAt orientation w |> Result.map ExperimentRun.ElementStep
                if touched then Ok (Some (SweptStep (fun settings -> elementAt (rotationAt settings) (waveLengthAt settings))))
                else elementAt e.placement.r1 w0 |> Result.map (fun fixedStep -> Some (FixedStep fixedStep))
            | None, Sample, Some (Library.SampleItem sample) ->
                // Bound once per path step, so its materials resolve once for the whole block.
                BoundOpticalElement.ofStackSample model.materials (ParameterValues.defaultsOf sample.parameterAxes) sample
                |> Result.bind (fun bound -> sweptOrFixed bound ExperimentRun.ObjectStep)
                |> Result.map Some
            | _ -> Ok None
    let buildersFor
        (axes : Experiments.ScanAxis list)
        (slotItems : Map<Library.ElementId, Item * ItemModelChoice>)
        (path : BeamLayout.PathStep list)
        : Result<StepBuilder list, ItemEvaluationError> =
        path |> List.map (builderFor axes slotItems) |> ItemEvaluation.sequenced |> Result.map (List.choose id)
    let stepsAt (builders : StepBuilder list) (settings : Experiments.ScanSetting list) : Result<ExperimentRun.RunStep list, ItemEvaluationError> =
        builders
        |> List.map (fun b ->
            match b with
            | FixedStep step -> Ok step
            | SweptStep f -> f settings)
        |> ItemEvaluation.sequenced
    let xLabelOf (variable : Experiments.VariableParameter) : string =
        match variable with
        | Experiments.VaryR1 -> "Rotation R1 (°)"
        | Experiments.VaryR2 -> "Incidence angle R2 (°)"
        | Experiments.VaryDialReading -> "Dial reading (°)"
        | Experiments.VaryWaveLength -> "Wavelength (nm)"
    let yLabelOf (kinds : Library.DetectorKind list) : ControlCaption =
        match kinds with
        | [ Library.Ellipsometer ] -> ControlCaption "Ψ, Δ (°)"
        | [ Library.Intensity ] | [] -> ControlCaption "Intensity (S₀)"
        | _ -> ControlCaption "Intensity (S₀); Ψ, Δ (°)"
    // One block: nothing without an axis or a drawable varied element, else its series or the status that replaces them.
    let chartOfBlock (blockTags : string list) (block : Experiments.ScanBlock) : Result<ScanBlockChart option, ExperimentChart.ExperimentChart> =
        let axes = block.axes |> List.map computable
        match List.rev axes with
        | [] -> Ok None
        | xAxis :: outerReversed ->
            match indexOf xAxis.elementId with
            | None -> Ok None
            | Some variedIndex ->
                let outer = List.rev outerReversed
                let variedElement = List.item variedIndex model.elements
                let variable = xAxis.variable
                let measurement = block.capture
                let lo, hi = xAxis.range.min.value, xAxis.range.max.value
                // A varied slot holding an item has something to vary, whatever its library binding (spec 0050 S8.2).
                let variedIsBound = Option.isSome variedElement.content.itemOpt || Option.isSome (boundOf variedElement)
                let detectors = model.captureDetectors xAxis.elementId
                let captured =
                    measurement.capturedBranches
                    |> List.choose (fun (branch, tag) ->
                        detectors |> List.tryFind (fun (b, _) -> b = branch) |> Option.map (fun (_, j) -> tag, j))
                // The items the slots on the captured paths hold, resolved before any point is read; the source's item
                // emits, so it is no step (spec 0050 S8.2).
                let slotItems : Result<Map<Library.ElementId, Item * ItemModelChoice>, SetupError> =
                    captured
                    |> List.collect (fun (_, j) -> (List.item j.value layout.elements).path)
                    |> List.choose (fun step -> indexOf step.element |> Option.map (fun k -> List.item k model.elements))
                    |> List.distinctBy (fun slot -> slot.id)
                    |> List.choose (fun slot ->
                        match slot.placement.catalogueKind, slot.content with
                        | LightSource, _
                        | _, Setups.EmptySlotContent -> None
                        | (LinearPolarizer | CircularPolarizer | Sample | Lens | FlatMirror | CurvedMirror | Detector), Setups.ItemInSlot (itemId, _) ->
                            Some (slot.id, itemId))
                    |> List.fold (fun held (slotId, itemId) ->
                        held |> Result.bind (fun items -> model.resolveSlotItem itemId |> Result.map (fun resolved -> Map.add slotId resolved items))) (Ok Map.empty)
                let kinds = captured |> List.map (fun (_, j) -> model.detectorKindAt j) |> List.distinct
                let quantity =
                    match kinds with
                    | [ Library.Ellipsometer ] -> "Ellipsometric Ψ/Δ"
                    | _ -> "Intensity"
                let title : ControlCaption =
                    match variable, variedElement.placement.catalogueKind, kinds with
                    | Experiments.VaryR1, (LinearPolarizer | CircularPolarizer), [ Library.Intensity ] -> ControlCaption "Rotating analyzer (Malus)"
                    | Experiments.VaryR1, _, _ -> ControlCaption $"%s{quantity} vs rotation R1"
                    | Experiments.VaryR2, _, _ -> ControlCaption $"%s{quantity} vs incidence"
                    | Experiments.VaryDialReading, _, _ -> ControlCaption $"%s{quantity} vs dial reading"
                    | Experiments.VaryWaveLength, _, _ -> ControlCaption $"%s{quantity} vs wavelength"
                let varyText =
                    match variable with
                    | Experiments.VaryR1 -> $"the rotation R1 over %g{lo}…%g{hi}°"
                    | Experiments.VaryR2 -> $"the incidence angle R2 over %g{lo}…%g{hi}°"
                    | Experiments.VaryDialReading -> $"the dial reading over %g{lo}…%g{hi}°"
                    | Experiments.VaryWaveLength -> $"the wavelength over %g{lo}…%g{hi} nm"
                let description : ControlCaption =
                    let sourceText =
                        match variable with
                        | Experiments.VaryWaveLength -> "Source swept"
                        | Experiments.VaryR1 | Experiments.VaryR2 | Experiments.VaryDialReading -> $"Source %g{w0.nanometers} nm"
                    let detectorText =
                        captured
                        |> List.map (fun (tag, j) -> $"{tag.value}: {(model.detectorKindAt j).readsText}")
                        |> String.concat ", "
                    ControlCaption
                        $"%s{sourceText}; varied element: %s{xAxis.label.value}; capture %s{measurement.label} (%s{detectorText}). Varying %s{varyText}."
                // The rotate-R1 intensity run needs polarized light: with no polarizer on the path there is nothing a
                // rotation could change — the typed status, never a synthesized curve (spec 0038 step 018). A polarizer
                // slot holding an item is a polarizer (spec 0050 S8.2).
                let noPolarizerOnPath =
                    captured
                    |> List.forall (fun (_, j) ->
                        (List.item j.value layout.elements).path
                        |> List.forall (fun step ->
                            match indexOf step.element |> Option.map (fun k -> List.item k model.elements) with
                            | Some slot ->
                                match slot.placement.catalogueKind, slot.content, boundOf slot with
                                | (LinearPolarizer | CircularPolarizer), Setups.ItemInSlot _, _
                                | _, _, Some (Library.PolarizerItem _) -> false
                                | _ -> true
                            | None -> true))
                match variable, kinds, captured with
                | _, _, [] -> Ok None
                | (Experiments.VaryR1 | Experiments.VaryDialReading), [ Library.Intensity ], _ when noPolarizerOnPath ->
                    Error { ExperimentChart.emptyExperimentChart with title = ControlCaption noAnalyzerTitle; description = ControlCaption noAnalyzerStatus }
                | _ when not variedIsBound && variedElement.placement.catalogueKind <> LightSource -> Ok None
                | _ ->
                    // An evaluation error, or a source's or slot's item the kit or the stores lack, is told on the chart
                    // (spec 0050 S8.1, S8.2).
                    let cannotEvaluate (reason : ControlCaption) : ExperimentChart.ExperimentChart =
                        { ExperimentChart.emptyExperimentChart with title = ControlCaption "The run cannot be evaluated"; description = reason }
                    // Each value of the outer axes, as the block's own product orders them (spec 0050 S9.1).
                    let groups : Experiments.ScanSetting list list =
                        let outerBlock : Experiments.ScanBlock = { axes = outer; capture = measurement }
                        outerBlock.points |> List.map (fun point -> point.settings)
                    let xSettings : Experiments.ScanSetting list =
                        xAxis.range.sweptValues
                        |> List.map (fun value -> { Experiments.ScanSetting.elementId = xAxis.elementId; variable = variable; value = value })
                    // A series' name: the one-variable name, then its group's settings and the block's tag (spec 0050 S9.1).
                    let named (group : Experiments.ScanSetting list) (name : ControlCaption) : ControlCaption =
                        let settingTexts =
                            List.map2
                                (fun (axis : Experiments.ScanAxis) (setting : Experiments.ScanSetting) ->
                                    $"%s{axis.label.value}: %s{axis.variable.label} %g{setting.value.value}%s{axis.variable.unitLabel}")
                                outer
                                group
                        match settingTexts @ blockTags with
                        | [] -> name
                        | parts ->
                            let joined = String.concat ", " parts
                            ControlCaption $"{name.value} — {joined}"
                    // The source leads the light, so its emission is read before the slots' items (spec 0050 S8.2).
                    match model.runEmission, slotItems with
                    | Error reason, _ -> Error (cannotEvaluate reason)
                    | Ok _, Error error -> Error (cannotEvaluate (ControlCaption error.describe))
                    | Ok emission, Ok items ->
                        let evaluated =
                            captured
                            |> List.map (fun (tag, j) ->
                                buildersFor axes items (List.item j.value layout.elements).path
                                |> Result.map (fun builders -> tag, model.detectorKindAt j, builders))
                            |> ItemEvaluation.sequenced
                            |> Result.bind (fun branches ->
                                groups
                                |> List.map (fun group ->
                                    branches
                                    |> List.map (fun (tag, kind, builders) ->
                                        xSettings
                                        |> List.map (fun xSetting ->
                                            stepsAt builders (group @ [ xSetting ])
                                            |> Result.map (fun steps -> xSetting.value.value, ExperimentRun.read kind emission steps))
                                        |> ItemEvaluation.sequenced
                                        |> Result.map (fun readings -> tag, kind, readings))
                                    |> ItemEvaluation.sequenced
                                    |> Result.map (fun runs -> group, runs))
                                |> ItemEvaluation.sequenced)
                        match evaluated with
                        | Error (SampleMaterialErr err) ->
                            Error
                                {
                                    ExperimentChart.emptyExperimentChart with
                                        title = ControlCaption "Sample materials failed to resolve"
                                        description = err.runFailureCaption
                                }
                        | Error (MissingParameterValueErr _ | ParameterOutOfRangeErr _ | NotALightSourceErr _ | NotAnOpticalElementErr _ | UnmodelledBranchErr _ | CutPlateSolveErr _ as error) ->
                            Error (cannotEvaluate (ControlCaption error.describe))
                        | Ok groupRuns ->
                            let seriesList =
                                groupRuns
                                |> List.collect (fun (group, runs) ->
                                    runs
                                    |> List.collect (fun (tag, kind, readings) ->
                                        let caption (reading : ControlCaption) : ControlCaption = named group (measurement.seriesCaption tag reading)
                                        match kind with
                                        | Library.Intensity ->
                                            [ series (caption (ControlCaption "Intensity"))
                                                  (readings |> List.map (fun (x, r) -> { ExperimentControls.ChartPoint.x = x; y = (match r with ExperimentRun.IntensityReading i -> i.value | ExperimentRun.EllipsometricReading _ -> nan) })) ]
                                        | Library.Ellipsometer ->
                                            let pd (pick : Analytics.AnalysisFunctions.PsiDelta -> Angle) : ExperimentControls.ChartPoint list =
                                                readings |> List.map (fun (x, r) -> { ExperimentControls.ChartPoint.x = x; y = (match r with ExperimentRun.EllipsometricReading v -> (pick v).degrees | ExperimentRun.IntensityReading _ -> nan) })
                                            [ series (caption (ControlCaption "Ψ")) (pd (fun v -> v.psi))
                                              series (caption (ControlCaption "Δ")) (pd (fun v -> v.delta)) ]))
                            let drawn : ScanBlockChart =
                                {
                                    blockSeries = seriesList
                                    kinds = kinds
                                    variable = variable
                                    title = title
                                    description = description
                                }
                            Ok (Some drawn)
    let blockTagged = List.length scan.blocks > 1
    let blockCharts =
        scan.blocks
        |> List.mapi (fun k block -> chartOfBlock (if blockTagged then [ $"block %d{k + 1}" ] else []) block)
    match blockCharts |> List.tryPick (fun blockChart -> match blockChart with Error status -> Some status | Ok _ -> None) with
    | Some status -> status
    | None ->
        match blockCharts |> List.choose (fun blockChart -> match blockChart with Ok drawnOpt -> drawnOpt | Error _ -> None) with
        | [] -> ExperimentChart.emptyExperimentChart
        | first :: _ as drawn ->
            let variables = drawn |> List.map (fun block -> block.variable) |> List.distinct
            let description : ControlCaption =
                match scan.blocks with
                | [ block ] when List.length block.axes = 1 -> first.description
                | _ ->
                    // The scan as its experiment names it, which reads only the blocks of a scan with an axis (spec 0050 S9.1).
                    let named : Experiments.Experiment =
                        {
                            id = Experiments.ExperimentId 0
                            name = Experiments.ExperimentName ""
                            setup = ({ name = SetupName ""; revision = SetupRevision.first } : SetupReference)
                            scan = scan
                            role = Experiments.ScienceRun
                            sessionOpt = None
                            notes = Experiments.ExperimentNotes ""
                            dataOpt = None
                        }
                    let sourceText =
                        match List.contains Experiments.VaryWaveLength (scan.axes |> List.map (fun axis -> axis.variable)) with
                        | true -> "Source swept"
                        | false -> $"Source %g{w0.nanometers} nm"
                    ControlCaption $"%s{sourceText}; %s{named.description}."
            {
                ExperimentChart.ExperimentChart.series = drawn |> List.collect (fun block -> block.blockSeries)
                xLabel = ControlCaption (variables |> List.map xLabelOf |> String.concat " / ")
                yLabel = yLabelOf (drawn |> List.collect (fun block -> block.kinds) |> List.distinct)
                title = first.title
                description = description
                xAxisKind =
                    match List.contains Experiments.VaryWaveLength variables with
                    | true -> ExperimentChart.NonAngularXAxis
                    | false -> ExperimentChart.AngularXAxis
            }

type TableAndElementRotationModel with

    /// Spec 0038 Part L (037): whether the forward experiment chart surface is present. The FORWARD
    /// constructor always shows it (an unbound sample there yields an empty chart, as before). The
    /// INVERSE constructor's forward preview needs a sample to compute against: with NO hint (the seeded
    /// sample unbound) the chart surface is ABSENT; binding a HINT sample (any Sample element bound)
    /// restores it and the chart behaves as today. Public so the host's gate is unit-testable.
    member model.experimentChartVisible : bool =
        match model.constructorMode with
        | ForwardConstructor -> true
        | InverseConstructor ->
            model.elements
            |> List.exists (fun e ->
                e.placement.catalogueKind = Sample &&
                (match e.placement.valueId with Some _ -> true | None -> false))

    /// The live DRAFT's chart (the inline bay preview, the draft "Open chart" action and Export data). Public so
    /// the host's branches are unit-testable without a window. Spec 0038 Part L (037): EMPTY when the inverse
    /// chart surface is gated off (inverse mode, no hint sample) — the surface is absent until a hint is bound.
    member model.experimentResult : ExperimentChart.ExperimentChart =
        if not model.experimentChartVisible then ExperimentChart.emptyExperimentChart
        else
            // Spec 0046: the draft runs with the capture the detectors allow; with none there is no chart. Spec 0050 S9.1:
            // its whole scan, the last block read on that capture as ExpCommit records it.
            match model.draftCapture with
            | Some capture ->
                match (Experiments.chooseMeasurement capture model.experimentCollection).draft.scanOpt with
                | Some scan -> chartForRun model scan
                | None -> ExperimentChart.emptyExperimentChart
            | None -> ExperimentChart.emptyExperimentChart

/// The chart of a COLLECTED experiment, the per-row "View" action (spec 0038 Part I, Part L 037): its scan over its setup
/// revision's slots (spec 0050 S8.2, S9.1); EMPTY while the inverse chart surface is gated off.
let chartForExperiment (model : TableAndElementRotationModel) (exp : Experiments.Experiment) : ExperimentChart.ExperimentChart =
    if not model.experimentChartVisible then ExperimentChart.emptyExperimentChart
    else chartForRun model exp.scan

// Spec 0027: wire the forward-referenced hooks (defined once `experimentResult` exists) so a double-click /
// "Open chart" / per-row "View" opens the pop-out ScottPlot `ChartWindow` for the relevant chart (guarded
// behind a non-empty chart; never reached under the headless `ui-smoke` gate's frame render). Spec 0046: the
// export hook reads the same draft run.
draftChartHook <- fun model -> model.experimentResult

openChartWindowHook <-
    fun model ->
        let chart = model.experimentResult
        if not (List.isEmpty chart.series) then ChartWindow(chart).Show()

viewExperimentHook <-
    fun model id ->
        match model.experimentCollection.experiments |> List.tryFind (fun e -> e.id = id) with
        | Some exp ->
            let chart = chartForExperiment model exp
            if not (List.isEmpty chart.series) then ChartWindow(chart).Show()
        | None -> ()

type TableAndElementRotationModel with

    /// The Experiments bay state for the current scene (spec 028): the present elements as candidates, the
    /// element-constrained variable choices, the current draft (chosen element / variable / capture / range),
    /// whether it can be committed and whether it is editing, the collection of added experiments, and the
    /// inline chart of the draft's run. Disabled when the scene has no elements. Public so the host's bay
    /// projection is unit-testable without mounting a window. Spec 0050 S9.1: also the draft's scan lines, the edited
    /// experiment's role, session and notes, and each row's name, role and data badge.
    member model.experimentState : ExperimentControls.ExperimentControlsState =
        let col = model.experimentCollection
        let draft = col.draft
        let chart = model.experimentResult
        let variableChoices =
            match draft.elementId with
            | Some id ->
                match model.elements |> List.tryFind (fun e -> e.id = id) with
                | Some e -> variablesOfElement model e |> List.map (fun v -> v.bayChoice)
                | None -> []
            | None -> []
        // Spec 0046 (task 5): the bay offers ONLY the captures the scene's detectors allow, and shows the one
        // the draft actually runs with; with none, Add is disabled.
        let captures =
            match draft.elementId with
            | Some id -> availableCaptures model id
            | None -> []
        let capture = model.draftCapture
        // Spec 0050 S9.1: step 288's texts — a closed block's axes and capture, the current block's held axes.
        let axesText (axes : Experiments.ScanAxis list) : string =
            axes |> List.map (fun (axis : Experiments.ScanAxis) -> axis.describe) |> String.concat " × "
        let blockLines =
            draft.closedBlocks
            |> List.mapi (fun index (block : Experiments.ScanBlock) ->
                match block.axes with
                | [] -> ControlCaption $"Block {index + 1}: Dark line, capture {block.capture.label}"
                | axes -> ControlCaption $"Block {index + 1}: {axesText axes}, capture {block.capture.label}")
        let heldLines =
            match draft.outerAxes with
            | [] -> []
            | axes -> [ ControlCaption $"This block: {axesText axes}" ]
        let editedOpt =
            draft.editingId |> Option.bind (fun id -> col.experiments |> List.tryFind (fun experiment -> experiment.id = id))
        let addAvailability =
            match model.kitWrites with
            | ControlWithheld -> ControlWithheld
            | ControlOffered ->
                match col.canCommit, capture with
                | true, Some _ -> ControlOffered
                | true, None | false, _ -> ControlWithheld
        {
            candidates = model.sweepCandidates
            chosenId = draft.elementId |> Option.map (fun id -> id.uiKey)
            variableChoices = variableChoices
            chosenVariable = draft.variable |> Option.map (fun v -> v.bayChoice)
            measurement = (capture |> Option.defaultValue draft.measurement).bayChoice
            measurementOffers =
                match draft.elementId with
                | Some id -> captures |> List.map (fun c -> { ExperimentControls.MeasurementOffer.choice = c.bayChoice; label = model.captureLabel id c })
                | None -> []
            sourceWaveLength =
                match draft.variable, model.boundSource with
                | Some Experiments.VaryWaveLength, _ | None, _ | Some _, None -> ExperimentControls.NoSourceWaveLength
                | Some _, Some source ->
                    let setting = ExperimentControls.BayNanometers (operatingWaveLength model draft.sourceWaveLengthOpt).nanometers
                    match source.tunableLimitsOpt with
                    | Some limits ->
                        ExperimentControls.TunableSourceWaveLength
                            (setting,
                             { ExperimentControls.SourceWaveLengthLimits.lowest = ExperimentControls.BayNanometers limits.lower.nanometers
                               highest = ExperimentControls.BayNanometers limits.upper.nanometers })
                    | None -> ExperimentControls.FixedSourceWaveLength setting
            rangeMin = draft.range.min.rangeEnd
            rangeMax = draft.range.max.rangeEnd
            rangePoints = draft.range.points.pointCount
            rangeUnitLabel = ControlCaption (match draft.variable with Some v -> v.unitLabel | None -> "°")
            addAvailability = addAvailability
            scanLines = blockLines @ heldLines
            scanAvailability = addAvailability
            removalAvailability = model.kitWrites
            editorMode =
                match draft.editingId with
                | Some _ -> ExperimentControls.EditorUpdatesExisting
                | None -> ExperimentControls.EditorAddsNew
            roleOptions =
                match editedOpt with
                | Some edited ->
                    Experiments.ExperimentRole.all
                    |> List.map (fun role ->
                        {
                            ExperimentControls.ExperimentRoleOption.key = role.uiKey
                            caption = role.caption
                            mark = if role = edited.role then ExperimentControls.RoleChosen else ExperimentControls.RoleNotChosen
                        })
                | None -> []
            sessionText =
                match editedOpt |> Option.bind (fun edited -> edited.sessionOpt) with
                | Some session -> session.caption
                | None -> ControlCaption ""
            notesText =
                match editedOpt with
                | Some edited -> edited.notes.caption
                | None -> ControlCaption ""
            bookkeepingAvailability =
                match editedOpt, model.kitWrites with
                | Some _, ControlOffered -> ControlOffered
                | Some _, ControlWithheld | None, _ -> ControlWithheld
            collection =
                col.experiments
                |> List.map (fun e ->
                    {
                        ExperimentControls.ExperimentRow.id = e.id.uiKey
                        description = ControlCaption $"{e.name.value} [{e.role.text}] — {e.description}"
                        badge = ControlCaption (model.dataBadgeOf e).text
                        focus = if draft.editingId = Some e.id then ExperimentControls.LoadedInEditor else ExperimentControls.NotLoadedInEditor
                    })
            readout = model.experimentReadout
            availability =
                match model.elements with
                | [] -> ControlWithheld
                | _ -> ControlOffered
            series = chart.series
            xLabel = chart.xLabel
            yLabel = chart.yLabel
            description = chart.description
            psiDelta = None
        }

/// The Experiments bay's handlers. A clicked candidate or row key is resolved against the scene and the
/// collection it was built from; a role, session or notes edit is the edited experiment's (spec 0050 S9.1).
let private experimentHandlers (model : TableAndElementRotationModel) (dispatch : TableAndElementRotationMsg -> unit) : ExperimentControls.ExperimentControlsHandlers =
    let withExperiment (msg : Experiments.ExperimentId -> TableAndElementRotationMsg) (key : ConstructorUiKey) : unit =
        model.experimentCollection.experiments |> List.tryFind (fun e -> e.id.uiKey = key) |> Option.iter (fun e -> dispatch (msg e.id))
    let edited = model.experimentCollection.draft.editingId
    {
        chooseElement = fun key -> model.elements |> List.tryFind (fun e -> e.id.uiKey = key) |> Option.iter (fun e -> dispatch (ExpChooseElement e.id))
        chooseVariable = fun v -> dispatch (ExpChooseVariable v)
        chooseMeasurement = fun m -> dispatch (ExpChooseMeasurement m)
        setRangeMin = fun v -> dispatch (ExpSetRangeMin v)
        setRangeMax = fun v -> dispatch (ExpSetRangeMax v)
        setRangePoints = fun n -> dispatch (ExpSetRangePoints n)
        setSourceWaveLength = fun nm -> dispatch (ExpSetSourceWaveLength nm)
        addOrUpdate = fun () -> dispatch ExpCommit
        newExperiment = fun () -> dispatch ExpNew
        addScanAxis = fun () -> dispatch ExpAddScanAxis
        addScanBlock = fun () -> dispatch ExpAddScanBlock
        setRole =
            fun key ->
                match edited, Experiments.ExperimentRole.all |> List.tryFind (fun role -> role.uiKey = key) with
                | Some id, Some role -> dispatch (SetExperimentRole (id, role))
                | Some _, None | None, _ -> ()
        setSession = fun text -> edited |> Option.iter (fun id -> dispatch (SetExperimentSession (id, text)))
        setNotes = fun text -> edited |> Option.iter (fun id -> dispatch (SetExperimentNotes (id, text)))
        editExperiment = withExperiment ExpEdit
        removeExperiment = withExperiment ExpRemove
        viewExperiment = withExperiment ViewExperiment
        openChartWindow = fun () -> dispatch OpenExperimentChartWindow
    }

/// The slot bar's handlers. A typed letter is resolved against the table's slot of that id and the items its chip lists,
/// swapping in the pick; no pick sends nothing (spec 0050 S8.4).
let private slotBarHandlers (model : TableAndElementRotationModel) (dispatch : TableAndElementRotationMsg -> unit) : SlotBarView.SlotBarHandlers =
    {
        toggleChip = fun slotId -> dispatch (ToggleSlotChip slotId)
        swapSlot = fun slotId itemId -> dispatch (SwapSlot (slotId, itemId))
        pickByLetter =
            fun slotId letter ->
                model.elements
                |> List.tryFind (fun slot -> slot.id = slotId)
                |> Option.bind (fun slot -> slot.pickedByLetter letter (model.slotChipItems slot))
                |> Option.iter (fun picked -> dispatch (SwapSlot (slotId, picked.id)))
        recordExperiment = fun () -> dispatch RecordExperiment
        openSetupGrid = fun () -> dispatch OpenSetupGrid
    }

/// The grid panel's handlers: each sends its semantic message, Generate the grid the panel shows (spec 0050 S8.4).
let private setupGridHandlers (dispatch : TableAndElementRotationMsg -> unit) : SetupGridView.SetupGridHandlers =
    {
        toggleItem = fun slotId itemId -> dispatch (ToggleSetupGridItem (slotId, itemId))
        addAngle = fun slotId degrees -> dispatch (AddSetupGridAngle (slotId, degrees))
        removeAngle = fun slotId angle -> dispatch (RemoveSetupGridAngle (slotId, angle))
        toggleCombination = fun combination -> dispatch (ToggleSetupGridCombination combination)
        generateGrid = fun grid -> dispatch (GenerateSetupGrid grid)
        closeGrid = fun () -> dispatch CloseSetupGrid
    }

// ---------------------------------------------------------------------------
// Spec 0027 (026) — the DETAILS bay: the selected element's bound Library entry shown as "what it is" (the
// heading + full description) and, for a layered sample, a layer-stack BAND view (the reusable
// `LayerBandsControls`). The host flattens the sample's engine `OpticalSystem` (`Propagation.sampleToSystem`)
// into `Band`s — borrowing the V1 `Schematic.fs` colour / height idea (replicated here as a few inline
// helpers, kept separate from `Schematic.fs` whose band vocabulary differs) — collapsing runs of identical-thickness
// films into a single "×N" band so the 41-layer / 100-pair multilayers stay readable.
// ---------------------------------------------------------------------------

/// A small `#RRGGBB` palette for unknown band materials (mirrors the Schematic fallback palette).
let private bandPalette : LayerBandsControls.BandColorHex[] =
    [| "#1F77B4"; "#FF7F0E"; "#2CA02C"; "#D62728"; "#9467BD"; "#8C564B"; "#E377C2"; "#BCBD22" |]
    |> Array.map LayerBandsControls.BandColorHex

/// Curated `#RRGGBB` colours for the built-in band materials (mirrors `Schematic.curated`), keyed by
/// the elevated `MaterialId` (spec 0033 step 002) — the same key the sample structure carries; the
/// old host id literals are gone with the string-id path.
let private bandColors : Map<MaterialLibrary.MaterialId, LayerBandsControls.BandColorHex> =
    [
        MaterialLibrary.MaterialIds.glass152, "#C8E1F5"
        MaterialLibrary.MaterialIds.glass150, "#CDE6FA"
        MaterialLibrary.MaterialIds.glass175, "#AACDEB"
        MaterialLibrary.MaterialIds.vacuumMaterial, "#F2F2F2"
        MaterialLibrary.MaterialIds.euvMolybdenum, "#5A5A6E"
        MaterialLibrary.MaterialIds.euvSilicon, "#5A5A6E"
        MaterialLibrary.MaterialIds.silicon, "#5A5A6E"
        MaterialLibrary.MaterialIds.langasite, "#78C8DC"
        MaterialLibrary.MaterialIds.uniaxialCrystal, "#AFE1AF"
        MaterialLibrary.MaterialIds.biaxialCrystal, "#96D296"
        MaterialLibrary.MaterialIds.activeCrystal, "#C8B4E6"
    ]
    |> List.map (fun (id, hex) -> id, LayerBandsControls.BandColorHex hex)
    |> Map.ofList

type MaterialLibrary.MaterialId with

    /// A curated colour for a known band material id, else a stable palette slot hashed from the id's Guid
    /// string form by a deterministic, process-independent FNV-1a fold (NOT `GetHashCode`, whose seed is
    /// per-process randomised). Pure and total.
    member private materialId.bandColorHex : LayerBandsControls.BandColorHex =
        match Map.tryFind materialId bandColors with
        | Some hex -> hex
        | None ->
            let hash = (string materialId.value |> Seq.fold (fun acc ch -> (acc ^^^ int ch) * 16777619) (int 2166136261u)) &&& 0x7fffffff
            bandPalette.[hash % bandPalette.Length]

    /// The materials store's name, or the id's GUID when missing or lookup fails (spec 0050 S1.7).
    member private materialId.bandDisplayName (materials : MaterialLibrary.MaterialProxy) : MaterialLibrary.EntryName =
        match materials.tryGetMaterial materialId with
        | Ok (Some entry) -> entry.name
        | Ok None
        | Error _ -> MaterialLibrary.EntryName (string materialId.value)

type Berreman.Media.Thickness with

    /// A human thickness label for a band (nm under 1 µm, µm under 1 mm, else mm; a half-space / plate is
    /// "semi-infinite"). The SOLE display conversion (a band label, never written back).
    member private t.bandCaption : string =
        match t.metersOpt with
        | None -> "semi-infinite"
        | Some meters ->
            let nm = meters * 1.0e9
            if nm < 1000.0 then $"%.1f{nm} nm"
            elif nm < 1.0e6 then $"%.3g{(nm / 1000.0)} µm"
            else $"%.3g{(nm / 1.0e6)} mm"

    /// The relative height weight of a band (log-compressed thickness in metres, so a 2.65 nm EUV layer and a
    /// 1 cm plate are both visible). A half-space / plate gets a fixed mid weight.
    member private t.bandWeight : LayerBandsControls.BandWeight =
        match t.metersOpt with
        | None -> LayerBandsControls.BandWeight 1.0
        | Some m ->
            let meters = max 1.0e-12 m
            // log10 of nanometres, floored — a 1 nm layer ⇒ 0, a 1 µm layer ⇒ 3, a 1 mm layer ⇒ 6.
            LayerBandsControls.BandWeight (max 0.1 (log10 (meters * 1.0e9)))

/// One band of the Details view: its material, its thickness (`Infinity` for a half-space) and how many
/// periods of it a `Repeated` group stacks.
type private BandSpec =
    {
        material : MaterialLibrary.MaterialId
        thickness : Berreman.Media.Thickness
        repeats : Library.PeriodCount
    }

type Library.Sample with

    /// The material-labelled layer list for the band view, read STRAIGHT off the sample's structure (spec
    /// 0033 step 001 — no per-sample-id table). A `Repeated` period group stays collapsed (one "×N" band per
    /// cell layer), the substrate plate follows the films, and a non-vacuum lower half-space renders as a
    /// semi-infinite band.
    member private sample.bandSpecs : BandSpec list =
        let single (l : Library.SampleLayer) : BandSpec = { material = l.materialId.materialId; thickness = l.thickness; repeats = Library.PeriodCount 1 }
        let filmBands =
            sample.structure.films
            |> List.collect (fun item ->
                match item with
                | Library.SingleLayer l -> [ single l ]
                | Library.Repeated g -> g.cell |> List.map (fun l -> { single l with repeats = g.count }))
        let substrateBands = sample.structure.substrate |> Option.toList |> List.map single
        let lowerBands =
            sample.structure.lower
            |> Option.toList
            |> List.map (fun mvid -> { material = mvid.materialId; thickness = Berreman.Media.Infinity; repeats = Library.PeriodCount 1 })
        filmBands @ substrateBands @ lowerBands

    /// The sample's "×N"-collapsed stack, then its substrate / lower half-space, titled with
    /// its name and description. Band names come from the materials store.
    member s.bandsState (materials : MaterialLibrary.MaterialProxy) : LayerBandsControls.LayerBandsControlsState =
        let bands =
            s.bandSpecs
            |> List.map (fun spec ->
                let materialName = spec.material.bandDisplayName materials
                let countText = if spec.repeats.value > 1 then $" ×{spec.repeats.value}" else ""
                ({
                    label = ControlCaption $"{materialName.value} — {spec.thickness.bandCaption}{countText}"
                    heightWeight = spec.thickness.bandWeight
                    colorHex = spec.material.bandColorHex
                 } : LayerBandsControls.Band))
        ({ title = ControlCaption $"{s.name.value} — {s.description.value}"; bands = bands } : LayerBandsControls.LayerBandsControlsState)

type TableAndElementRotationModel with

    /// Credits the selected binding and its sample materials (spec 0050 S3.3).
    member model.detailsAttributionLines : EntryAttributionLine list =
        match model.selection with
        | ElementSelected i ->
            match model.tryElementAt i |> Option.bind (boundEntry model) with
            | Some entry ->
                match model.catalogueImports.proxy.attributionLinesOf entry.attributedEntryIds with
                | Ok lines -> lines
                | Error _ -> []
            | None -> []
        | TableSelected | NothingSelected -> []

    /// Build the `LayerBandsControls.State` for the selected element. A bound layered sample yields the band
    /// view (collapsed "×N" bands with material + thickness labels); a bound non-sample entry yields just the
    /// title + full description; nothing selected / unbound yields a hint title and no bands.
    member internal model.detailsState : LayerBandsControls.LayerBandsControlsState =
        let hint : LayerBandsControls.LayerBandsControlsState = { title = ControlCaption "Select an element to see what it is."; bands = [] }
        match model.selection with
        | ElementSelected i ->
            match model.tryElementAt i with
            | Some e ->
                let baseState =
                    match boundEntry model e with
                    | Some (Library.SampleItem s) -> s.bandsState model.materials
                    | Some entry ->
                        ({ title = ControlCaption $"{entry.displayName} — {entry.fullDescription}"; bands = [] } : LayerBandsControls.LayerBandsControlsState)
                    | None ->
                        ({ title = ControlCaption "No Library entry bound — pick one in the Select bay to see its details."; bands = [] } : LayerBandsControls.LayerBandsControlsState)
                // Spec 0038 (031), 0050 S3.3: keep the badge warning above the licence credits.
                let withWarning =
                    match outOfBandWarningFor model e with
                    | Some warning ->
                        {
                            baseState with
                                title = ControlCaption $"{baseState.title.value}\n⚠ {warning.value}"
                        }
                    | None -> baseState
                let title =
                    withWarning.title.value :: (model.detailsAttributionLines |> List.map _.text)
                    |> String.concat "\n"
                    |> ControlCaption
                {
                    withWarning with
                        title = title
                }
            | None -> hint
        | TableSelected | NothingSelected -> hint

// ---------------------------------------------------------------------------
// Spec 0038 steps 013/015 — the workbench strip buttons. The MATERIALS bay moved into the
// single-instance Materials WINDOW (step 013, `MaterialsWindowView`) and the LIBRARY (samples)
// workbench bay into the single-instance Library WINDOW (step 015, `LibraryWindowView`); the
// ribbon tab-strip row's right-aligned "Library…" / "Materials…" buttons below are what remain
// of them here. (`SampleLibraryControls` stays in the Controls project with its own tests until
// a later sweep retires it — the `MaterialsControls` precedent.) The ids are `ConstructorUiId`'s.
// ---------------------------------------------------------------------------

type TableAndElementRotationModel with

    /// Spec 0038 (016): the staleness status line — present only while a status is set (keyed:
    /// variable membership, so it carries an AutomationId, never a write-once `Name`), rendered right
    /// under the readout.
    member internal model.selectStatusRow : IView list =
        match model.selectStatus with
        | None -> []
        | Some text ->
            [ (TextBlock.create [
                  ConstructorUiId.workbenchSelectStatus.automationAttribute<TextBlock> ()
                  TextBlock.foreground (brush (Color.FromRgb(178uy, 34uy, 34uy)))
                  TextBlock.textWrapping TextWrapping.Wrap
                  TextBlock.margin (Thickness(8.0, 0.0, 0.0, 4.0))
                  TextBlock.text text.value
              ]
              |> Avalonia.FuncUI.DSL.View.withKey ConstructorUiId.workbenchSelectStatus.value) :> IView ]

/// A small clickable verb box for the host-added strip buttons (the MaterialsControls button look). The
/// strip buttons live in a variable-membership row, so they carry AutomationIds. Withheld, it is disabled
/// and dimmed, never hidden (spec 0050 S5.3).
let private availableWorkbenchButton (availability : ControlAvailability) (id : ConstructorUiId) (label : ControlCaption) (onClick : PointerPressedEventArgs -> unit) : IView =
    Border.create [
        id.automationAttribute<Border> ()
        AttrBuilder<Border>.CreateProperty<string>(AutomationProperties.NameProperty, label.value, ValueNone)
        Border.isEnabled availability.isEnabled
        Border.opacity (match availability with ControlOffered -> 1.0 | ControlWithheld -> 0.4)
        Border.background (brush (Color.FromRgb(232uy, 232uy, 232uy)))
        Border.borderBrush (brush (Color.FromRgb(120uy, 120uy, 120uy)))
        Border.borderThickness 1.0
        Border.cornerRadius (CornerRadius 3.0)
        Border.padding (Thickness(12.0, 5.0))
        Border.margin (Thickness(0.0, 0.0, 8.0, 4.0))
        Border.verticalAlignment VerticalAlignment.Center
        Border.child (TextBlock.create [ TextBlock.text label.value ])
        Border.onPointerPressed ((fun e -> e.Handled <- true; onClick e), SubPatchOptions.OnChangeOf id.value)
    ] :> IView

let private workbenchButton (id : ConstructorUiId) (label : ControlCaption) (onClick : PointerPressedEventArgs -> unit) : IView =
    availableWorkbenchButton ControlOffered id label onClick

// ---------------------------------------------------------------------------
// Spec 0038 (017) — the Select bay's Choose… / quick-pick flow. Choose… opens (or re-targets)
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
let private chooseFromLibrary (model : TableAndElementRotationModel) (dispatch : TableAndElementRotationMsg -> unit) (element : Setups.SetupSlot) (owner : Window) : unit =
    let sessionRef : SelectSession option ref = ref None
    let selectCtx : WindowMode.SelectionContext<Library.LibraryEntry> =
        {
            kindConstraint = WindowMode.KindConstraint element.placement.catalogueKind
            // Spec 0046: the Library opens with the element's current binding shown as chosen.
            currentEntryIdOpt = element.placement.valueId
            target = WindowMode.TableElementTarget element.id
            onSelected = fun (entry : Library.LibraryEntry) -> dispatch (BindValueIdTo (element.id, entry.entryId))
            onCancelled = fun () -> sessionRef.Value |> Option.iter (fun session -> dispatch (SelectSessionEnded session))
        }
    match model.launchers.openLibrarySelectWindow model.library model.samples model.sources model.materials model.categories model.libraryBackups model.catalogueImports model.selectWindowModality owner selectCtx with
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
let private selectorChooseButton (model : TableAndElementRotationModel) (dispatch : TableAndElementRotationMsg -> unit) (element : Setups.SetupSlot) : IView =
    Border.create [
        ConstructorUiId.selectorChooseButton.automationAttribute<Border> ()
        Border.background (brush (Color.FromRgb(232uy, 232uy, 232uy)))
        Border.borderBrush (brush (Color.FromRgb(120uy, 120uy, 120uy)))
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
    |> Avalonia.FuncUI.DSL.View.withKey ConstructorUiId.selectorChooseButton.value
    :> IView

/// The Select bay content (spec 0038 step 017). No element selected → today's disabled
/// prompt (the empty LibraryControls bay). An element selected → the Choose… verb, and BELOW
/// the threshold also the inline quick-pick strip — the unchanged LibraryControls surface
/// (kind label, bound readout, kind-constrained rows, confirm panel) wrapped in a keyed,
/// AutomationId'd box so the headless gating proofs address it as one unit.
let private selectorBayContent (model : TableAndElementRotationModel) (dispatch : TableAndElementRotationMsg -> unit) : IView =
    match model.selectorOffer, model.selection with
    | (QuickPickAndChoose | ChooseAlone) as offer, ElementSelected i ->
        let element = List.item i.value model.elements
        let strip : IView list =
            match offer with
            | QuickPickAndChoose ->
                [ Border.create [
                      ConstructorUiId.selectorQuickPickStrip.automationAttribute<Border> ()
                      Border.child (LibraryControls.libraryControlsView model.libraryState (libraryHandlers model dispatch))
                  ]
                  |> Avalonia.FuncUI.DSL.View.withKey ConstructorUiId.selectorQuickPickStrip.value
                  :> IView ]
            | ChooseAlone | NoSelectorOffer -> []
        StackPanel.create [
            StackPanel.orientation Orientation.Vertical
            StackPanel.spacing 4.0
            StackPanel.children (selectorChooseButton model dispatch element :: strip)
        ] :> IView
    | _, (TableSelected | NothingSelected | ElementSelected _) ->
        LibraryControls.libraryControlsView LibraryControls.empty (libraryHandlers model dispatch)

// ---------------------------------------------------------------------------
// Experiments are kept in the kit; inverse experiments attach measured data (spec 0050 S4.3).
// The kit status is shown by the kit bar alone, whichever bay is open (spec 0050 S5.3, `KitWindowView`).

/// Spec 0038 Part L (038): build the measured-data picker's options carrying the persisted start
/// folder. PURE — the resolved `suggested` folder (resolved from the persisted path through the
/// window's `StorageProvider` at the IO edge; `None` when the folder is unset/unresolvable or on a
/// headless host) is the sole input, so title / single-select / `SuggestedStartLocation` are
/// unit-tested without raising a real picker.
let buildDataFilePickerOptions
    (suggested : Avalonia.Platform.Storage.IStorageFolder option)
    : Avalonia.Platform.Storage.FilePickerOpenOptions =
    let options =
        Avalonia.Platform.Storage.FilePickerOpenOptions(
            Title = "Attach measured-data file",
            AllowMultiple = false)
    match suggested with
    | Some folder -> options.SuggestedStartLocation <- folder
    | None -> ()
    options

/// Picks a lab file under `title` and remembers its folder, as Attach data file… and the Items panel's photo do;
/// cancellation leaves settings unchanged (spec 0038 Part L steps 037/038; spec 0050 S4.2, S7.2).
let pickDataFile (owner : Window) (title : ControlCaption) (onPicked : FileName -> unit) : unit =
    try
        let settingsPath = UserEnvironment.settingsPath ()
        let settings = UserEnvironment.EnvironmentSettings.load settingsPath
        let picked =
            async {
                // Resolve the persisted folder to an `IStorageFolder` through the provider (`None` when
                // unset, unresolvable, or on a headless host) — the picker's `SuggestedStartLocation`.
                let! suggested =
                    match UserEnvironment.lastFolder UserEnvironment.measuredDataFolderKey settings with
                    | Some folder ->
                        async {
                            let! resolved =
                                Avalonia.Platform.Storage.StorageProviderExtensions.TryGetFolderFromPathAsync(owner.StorageProvider, folder.value)
                                |> Async.AwaitTask
                            return Option.ofObj resolved
                        }
                    | None -> async { return None }
                let options = buildDataFilePickerOptions suggested
                options.Title <- title.value
                let! files = owner.StorageProvider.OpenFilePickerAsync options |> Async.AwaitTask
                let pick =
                    match List.ofSeq files with
                    | file :: _ -> UserEnvironment.FilePicked (FileName file.Path.LocalPath)
                    | [] -> UserEnvironment.PickCancelled
                // Fold the outcome into the environment and persist ONLY when a confirmed selection
                // actually changed the folder (best-effort — a save failure must not sink the attach);
                // a cancel leaves `settings` unchanged, so nothing is written.
                let updated = UserEnvironment.applyPick UserEnvironment.measuredDataFolderKey pick settings
                if updated <> settings then updated.save settingsPath |> ignore
                match pick with
                | UserEnvironment.FilePicked dataFile -> onPicked dataFile
                | UserEnvironment.PickCancelled -> ()
            }
        Avalonia.Threading.Dispatcher.UIThread.Post(fun () -> Async.StartImmediate picked)
    with _ -> ()

/// The "Attach data file…" verb for one collected experiment — resolve the owner window from the
/// click's visual tree (the `selectorChooseButton` precedent) and open the picker, dispatching
/// `AttachDataFileTo` on a confirmed selection. Keyed + AutomationId'd by the experiment id.
/// Withheld, it is disabled and dimmed and opens no picker (spec 0050 S5.3).
let private attachDataFileButton (availability : ControlAvailability) (dispatch : TableAndElementRotationMsg -> unit) (experiment : Experiments.Experiment) : IView =
    let id = ConstructorUiId.attachDataFile experiment.id
    Border.create [
        id.automationAttribute<Border> ()
        Border.isEnabled availability.isEnabled
        Border.opacity (match availability with ControlOffered -> 1.0 | ControlWithheld -> 0.4)
        Border.background (brush (Color.FromRgb(232uy, 232uy, 232uy)))
        Border.borderBrush (brush (Color.FromRgb(120uy, 120uy, 120uy)))
        Border.borderThickness 1.0
        Border.cornerRadius (CornerRadius 3.0)
        Border.padding (Thickness(10.0, 4.0))
        Border.margin (Thickness(0.0, 0.0, 8.0, 0.0))
        Border.verticalAlignment VerticalAlignment.Center
        Border.child (TextBlock.create [ TextBlock.text "Attach data file…" ])
        Border.onPointerPressed ((fun e ->
            e.Handled <- true
            match availability with
            | ControlWithheld -> ()
            | ControlOffered ->
                match e.Source with
                | :? Visual as source ->
                    match TopLevel.GetTopLevel source with
                    | :? Window as owner ->
                        pickDataFile owner (ControlCaption "Attach measured-data file") (fun path -> dispatch (AttachDataFileTo (experiment.id, path)))
                    | _ -> ()
                | _ -> ()), SubPatchOptions.OnChangeOf (box (experiment.id.uiKey.value, availability)))
    ]
    |> Avalonia.FuncUI.DSL.View.withKey id.value
    :> IView

/// One experiment's data-file row: its description, the Attach verb + the currently-attached file
/// name, and its data badge (spec 0050 S9.1).
let private dataFileRow (model : TableAndElementRotationModel) (dispatch : TableAndElementRotationMsg -> unit) (experiment : Experiments.Experiment) : IView =
    let attachedText =
        match experiment.dataOpt with
        | Some data -> $"file: {data.attachment.path.value} ({data.profile.value})"
        | None -> "no file attached"
    let statusText = (model.dataBadgeOf experiment).caption.value
    StackPanel.create [
        StackPanel.orientation Orientation.Vertical
        StackPanel.margin (Thickness(0.0, 0.0, 0.0, 6.0))
        StackPanel.children [
            TextBlock.create [ TextBlock.text experiment.description; TextBlock.textWrapping TextWrapping.Wrap; TextBlock.maxWidth 360.0 ]
            StackPanel.create [
                StackPanel.orientation Orientation.Horizontal
                StackPanel.spacing 6.0
                StackPanel.children [
                    attachDataFileButton model.kitWrites dispatch experiment
                    TextBlock.create [ TextBlock.text attachedText; TextBlock.foreground (brush (Color.FromRgb(120uy, 120uy, 120uy))); TextBlock.verticalAlignment VerticalAlignment.Center ]
                ]
            ]
            TextBlock.create [
                (ConstructorUiId.dataFileStatus experiment.id).automationAttribute<TextBlock> ()
                TextBlock.text statusText
                TextBlock.textWrapping TextWrapping.Wrap
                TextBlock.maxWidth 360.0
                TextBlock.foreground (brush (Color.FromRgb(90uy, 90uy, 90uy)))
            ]
        ]
    ]
    |> Avalonia.FuncUI.DSL.View.withKey ("DataFileRow_" + experiment.id.uiKey.value)
    :> IView

/// Names the active setup, creates, saves and loads the kit's setups, and offers inverse data-file attachments (spec
/// 0050 S4.3, S8.2). The kit status is the kit bar's, so one control carries `kitStatus` (spec 0050 S5.3).
let private collectionBuilderView (model : TableAndElementRotationModel) (dispatch : TableAndElementRotationMsg -> unit) : IView =
    let sectionHeading (text : string) : IView =
        TextBlock.create [ TextBlock.text text; TextBlock.fontWeight FontWeight.SemiBold; TextBlock.margin (Thickness(0.0, 6.0, 0.0, 2.0)) ] :> IView
    let commitName (source : obj) : unit =
        match source with
        | :? TextBox as field ->
            let name = ControlCaption field.Text
            dispatch (SetSetupName name)
        | _ -> ()
    let createSetup (event : PointerPressedEventArgs) : unit =
        // Spec 0050 S4.3: the verb box does not blur the field; commit its live text before creating.
        match event.Source with
        | :? Visual as source ->
            match TopLevel.GetTopLevel source with
            | :? Window as owner ->
                owner.GetVisualDescendants()
                |> Seq.tryPick (function
                    | :? TextBox as field when AutomationProperties.GetAutomationId field = ConstructorUiId.setupNameField.value -> Some field
                    | _ -> None)
                |> Option.iter (fun field ->
                    commitName field
                    dispatch CreateSetup)
            | _ -> ()
        | _ -> ()
    // Spec 0050 S8.2: outside the horizontal row, so the line wraps (AGENTS.md).
    let activeSetupLine : IView =
        let text =
            match model.activeSetupOpt with
            | Some active -> $"Active setup: {active.name.value}, revision {active.revision.value}"
            | None -> "The table is not a setup of the kit yet."
        TextBlock.create [
            ConstructorUiId.activeSetupText.automationAttribute<TextBlock> ()
            TextBlock.textWrapping TextWrapping.Wrap
            TextBlock.text text
        ]
        |> Avalonia.FuncUI.DSL.View.withKey ConstructorUiId.activeSetupText.value
        :> IView
    let saveAvailability : ControlAvailability =
        match model.activeSetupOpt with
        | Some _ -> model.kitWrites
        | None -> ControlWithheld
    let setupRow : IView =
        StackPanel.create [
            StackPanel.orientation Orientation.Horizontal
            StackPanel.spacing 6.0
            StackPanel.children [
                TextBox.create [
                    ConstructorUiId.setupNameField.automationAttribute<TextBox> ()
                    AttrBuilder<TextBox>.CreateProperty<string>(AutomationProperties.NameProperty, "Setup name", ValueNone)
                    TextBox.width 180.0
                    TextBox.text model.setupName.value
                    TextBox.onKeyDown (fun event -> if event.Key = Key.Enter then commitName event.Source)
                    TextBox.onLostFocus (fun event -> commitName event.Source)
                ]
                availableWorkbenchButton model.kitWrites ConstructorUiId.createSetupButton (ControlCaption "Create setup") createSetup
                availableWorkbenchButton saveAvailability ConstructorUiId.saveSetupButton (ControlCaption "Save setup") (fun _ -> dispatch SaveSetup)
            ]
        ] :> IView
    let savedSetupSection : IView list =
        match model.kitSetups with
        | [] -> []
        | setups ->
            [ sectionHeading "Saved setups:"
              (WrapPanel.create [
                  // Spec 0050 S4.3: FuncUI conflates Name with AutomationProperties.Name;
                  // initialize the write-once control name outside its property diff.
                  WrapPanel.init (fun (panel : WrapPanel) -> panel.Name <- ConstructorUiId.savedSetups.value)
                  ConstructorUiId.savedSetups.automationAttribute<WrapPanel> ()
                  AttrBuilder<WrapPanel>.CreateProperty<string>(AutomationProperties.NameProperty, "Saved setups", ValueNone)
                  WrapPanel.orientation Orientation.Horizontal
                  WrapPanel.children (
                      setups |> List.map (fun setup ->
                          workbenchButton (ConstructorUiId.savedSetup setup.name) setup.name.caption (fun _ -> dispatch (ActivateSetup setup.name))))
               ] |> Avalonia.FuncUI.DSL.View.withKey ConstructorUiId.savedSetups.value) :> IView ]
    // Spec 0046: measured-data files belong to the INVERSE problem alone — the forward (Main) constructor
    // shows no "Data files" section at all.
    let dataFileSection : IView list =
        match model.constructorMode with
        | ForwardConstructor -> []
        | InverseConstructor ->
            sectionHeading "Data files:"
            :: (match model.experimentCollection.experiments with
                | [] -> [ TextBlock.create [ TextBlock.text "(add experiments above, then attach one data file to each)"; TextBlock.foreground (brush (Color.FromRgb(120uy, 120uy, 120uy))) ] :> IView ]
                | experiments ->
                    [ StackPanel.create [
                          StackPanel.name ConstructorUiId.experimentDataFiles.value
                          StackPanel.orientation Orientation.Vertical
                          StackPanel.children (experiments |> List.map (dataFileRow model dispatch))
                      ] :> IView ])
    StackPanel.create [
        StackPanel.orientation Orientation.Vertical
        StackPanel.spacing 2.0
        StackPanel.children ([ sectionHeading "Setup:"; activeSetupLine; setupRow ] @ savedSetupSection @ dataFileSection)
    ] :> IView

/// The Experiments bay content (spec 0038 Part L, 037): the (untouched) Controls-layer experiment
/// editor + chart, then the Ui-layer collection builder stacked below it.
let private experimentsBayContent (model : TableAndElementRotationModel) (dispatch : TableAndElementRotationMsg -> unit) : IView =
    // Spec 0046: Export data — the draft's computed run through the export seam, with its status line.
    let exportRow =
        StackPanel.create [
            StackPanel.orientation Orientation.Horizontal
            StackPanel.spacing 8.0
            StackPanel.children [
                workbenchButton ConstructorUiId.experimentExportButton (ControlCaption "Export data") (fun _ -> dispatch ExportExperimentData)
                TextBlock.create [
                    TextBlock.name ConstructorUiId.experimentExportStatus.value
                    TextBlock.verticalAlignment VerticalAlignment.Center
                    TextBlock.text (model.exportStatus |> Option.map (fun status -> status.value) |> Option.defaultValue "")
                ]
            ]
        ] :> IView
    StackPanel.create [
        StackPanel.orientation Orientation.Vertical
        StackPanel.spacing 6.0
        StackPanel.children [
            ExperimentControls.experimentControlsView model.experimentState (experimentHandlers model dispatch)
            exportRow
            collectionBuilderView model dispatch
        ]
    ] :> IView

/// Spec 0046: the GLOBAL rotation-pivot option — checked, every element rotates about the middle of its
/// incident face (the incident ray then always meets it there); unchecked, about its centre (as before).
/// `isChecked` is bound one way and the dispatch rides `onClick`, which fires only on user activation
/// (the `SampleEditorView.emissionCheck` precedent — no FuncUI feedback loop).
let private pivotCheckBox (model : TableAndElementRotationModel) (dispatch : TableAndElementRotationMsg -> unit) : IView =
    let isOn =
        match model.rotationPivot with
        | BeamLayout.PivotAtIncidentFace -> true
        | BeamLayout.PivotAtCentre -> false
    CheckBox.create [
        ConstructorUiId.rotateAboutIncidentFaceCheck.automationAttribute<CheckBox> ()
        CheckBox.isChecked isOn
        CheckBox.content "Rotate elements about the middle of their incident face"
        CheckBox.margin (Thickness(8.0, 0.0, 0.0, 0.0))
        CheckBox.onClick ((fun _ -> dispatch (SetRotationPivot (if isOn then BeamLayout.PivotAtCentre else BeamLayout.PivotAtIncidentFace))), SubPatchOptions.OnChangeOf (box isOn))
    ] :> IView

/// The Rotation bay: the shared rotation-controls bar (wrapping — an element's three lock toggles outgrow the
/// window, spec 0046) plus the global rotation-pivot option.
let private rotationBayContent (model : TableAndElementRotationModel) (dispatch : TableAndElementRotationMsg -> unit) : IView =
    StackPanel.create [
        StackPanel.orientation Orientation.Vertical
        StackPanel.spacing 4.0
        StackPanel.children [ RotationControls.wrappingView model.rotationState (rotationHandlers dispatch); pivotCheckBox model dispatch ]
    ] :> IView

/// The Main-screen ribbon Bays — every large control, each bound to the current model / dispatch. Adding
/// or removing a Bay here is the ONLY change needed to add / remove a large control from the Main screen.
/// (Spec 0038 step 015: no FULL-SURFACE bay remains — the samples workbench that used the mode is the
/// Library WINDOW now; the mode itself stays in the generic Ribbon control.)
let mainBays (model : TableAndElementRotationModel) (dispatch : TableAndElementRotationMsg -> unit) : Ribbon.Bay list =
    [ { name = BayNames.add; content = ElementPaletteControls.elementPaletteControlsView model.paletteState (paletteHandlers model dispatch); mode = Ribbon.InRibbonPane }
      { name = BayNames.rotation; content = rotationBayContent model dispatch; mode = Ribbon.InRibbonPane }
      { name = BayNames.move; content = moveBayContent model dispatch; mode = Ribbon.InRibbonPane }
      { name = BayNames.render; content = RendererControls.rendererControlsView model.render (renderHandlers dispatch); mode = Ribbon.InRibbonPane }
      { name = BayNames.selector; content = selectorBayContent model dispatch; mode = Ribbon.InRibbonPane }
      { name = BayNames.experiments; content = experimentsBayContent model dispatch; mode = Ribbon.InRibbonPane }
      { name = BayNames.details; content = model.detailsState.view; mode = Ribbon.InRibbonPane } ]

let private mainControlBar (bays : Ribbon.Bay list) (model : TableAndElementRotationModel) (dispatch : TableAndElementRotationMsg -> unit) : IView =
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
                            Border.child (workbenchButton ConstructorUiId.openMaterialsWindowButton (ControlCaption "Materials…") (fun _ -> dispatch OpenMaterialsWindow))
                        ]
                        Border.create [
                            Border.dock Dock.Right
                            Border.verticalAlignment VerticalAlignment.Top
                            Border.margin (Thickness(0.0, 8.0, 0.0, 0.0))
                            Border.child (workbenchButton ConstructorUiId.openLibraryWindowButton (ControlCaption "Library…") (fun _ -> dispatch OpenLibraryWindow))
                        ]
                        Ribbon.ribbonView { bays = bays; selected = model.ribbon } (fun name -> dispatch (SelectBay name))
                    ]
                ] :> IView
                TextBlock.create [ TextBlock.name ConstructorUiId.teReadout.value; TextBlock.margin (Thickness(8.0, 0.0, 0.0, 4.0)); TextBlock.text model.readoutText ] :> IView
            ]
            // Spec 0038 (016): the staleness status line — present only while a status is set.
            @ model.selectStatusRow)
    ] :> IView

// ---------------------------------------------------------------------------
// Spec 0046 — the right-click ELEMENT MENU, drawn as a model-driven overlay above the table canvas (a
// sibling of the table surface, so its rows receive their own clicks): the element's reflected /
// transmitted ray switches, the beam it sits on (the main beam or the reflected ray of a partially-
// reflecting element), Remove, and Close. Every row carries an intent-named AutomationId
// (`ConstructorUiId.elementMenu…`), and every verb is a plain `Msg`, so the behaviour is provable through `update`.
// ---------------------------------------------------------------------------

/// Spec 0046: the menu grows with its text up to `elementMenuMaxWidth` (then the lines wrap), never below
/// `elementMenuMinWidth`.
let private elementMenuMinWidth : CanvasLength = CanvasLength 260.0
let private elementMenuMaxWidth : CanvasLength = CanvasLength 460.0
let private elementMenuRowHeight : CanvasLength = CanvasLength 26.0

/// How an element-menu line that is not a verb is set: a section heading, or a greyed explanatory note.
type private ElementMenuLine =
    | ElementMenuHeading
    | ElementMenuNote

/// One clickable element-menu row (greyed and inert when withheld), keyed and AutomationId'd by `id`.
let private elementMenuItem (id : ConstructorUiId) (label : ControlCaption) (availability : ControlAvailability) (onClick : unit -> unit) : IView =
    Border.create [
        id.automationAttribute<Border> ()
        Border.isEnabled availability.isEnabled
        Border.opacity (match availability with ControlOffered -> 1.0 | ControlWithheld -> 0.45)
        Border.background (brush (Color.FromRgb(255uy, 255uy, 255uy)))
        Border.padding (Thickness(10.0, 4.0))
        Border.child (TextBlock.create [ TextBlock.text label.value; TextBlock.textWrapping TextWrapping.Wrap ])
        Border.onPointerPressed ((fun e -> e.Handled <- true; onClick ()), SubPatchOptions.OnChangeOf (box (id.value, label.value, availability.isEnabled)))
    ]
    |> Avalonia.FuncUI.DSL.View.withKey id.value
    :> IView

/// Spec 0046: the menu's Choose… row — the Select bay's Choose… verb (`chooseFromLibrary`) for the
/// element the menu serves, which then closes the menu. Like the Select bay's button, the owner window for a
/// modal Select open is resolved from the click's own visual tree, and the subscription re-patches on the
/// element's id (the closure's only render-varying capture).
let private elementMenuChooseItem (model : TableAndElementRotationModel) (dispatch : TableAndElementRotationMsg -> unit) (element : Setups.SetupSlot) : IView =
    Border.create [
        ConstructorUiId.elementMenuChoose.automationAttribute<Border> ()
        Border.background (brush (Color.FromRgb(255uy, 255uy, 255uy)))
        Border.padding (Thickness(10.0, 4.0))
        Border.child (TextBlock.create [ TextBlock.text "Choose…" ])
        Border.onPointerPressed ((fun e ->
            e.Handled <- true
            // Resolve the owner BEFORE closing the menu: closing re-renders it out of the visual tree.
            let ownerOpt =
                match e.Source with
                | :? Visual as source ->
                    match TopLevel.GetTopLevel source with
                    | :? Window as owner -> Some owner
                    | _ -> None
                | _ -> None
            dispatch CloseElementMenu
            ownerOpt |> Option.iter (chooseFromLibrary model dispatch element)), SubPatchOptions.OnChangeOf element.id.value)
    ]
    |> Avalonia.FuncUI.DSL.View.withKey ConstructorUiId.elementMenuChoose.value
    :> IView

/// A non-clickable element-menu line — a section heading, or a greyed explanatory note.
let private elementMenuNote (text : ControlCaption) (line : ElementMenuLine) : IView =
    TextBlock.create [
        TextBlock.text text.value
        TextBlock.textWrapping TextWrapping.Wrap
        TextBlock.margin (Thickness(10.0, 4.0, 10.0, 2.0))
        TextBlock.fontWeight (match line with ElementMenuHeading -> FontWeight.SemiBold | ElementMenuNote -> FontWeight.Normal)
        TextBlock.foreground (brush (match line with ElementMenuHeading -> Color.FromRgb(40uy, 40uy, 40uy) | ElementMenuNote -> Color.FromRgb(110uy, 110uy, 110uy)))
    ] :> IView

/// The element menu's rows for the element it serves: its title, Choose… (spec 0046 — the Select bay's
/// Choose…), the ray switches (a source / detector only explains why it has none; a mirror or reflect-only
/// sample shows them fixed, with the reason), the beam it sits on (never for the source — the ray root),
/// Remove, Close. Public so the row set is provable headless without a window.
let elementMenuRows (model : TableAndElementRotationModel) (dispatch : TableAndElementRotationMsg -> unit) (menu : ElementMenu) : IView list =
    match model.tryIndexOf menu.target with
    | None -> []
    | Some i ->
        let e = List.item i.value model.elements
        let p = e.placement
        let nameOf (j : int) : string = $"{(List.item j model.elements).placement.catalogueKind.kindName} #{j + 1}"
        let check (on : bool) : string = if on then "☑" else "☐"
        let radio (on : bool) : string = if on then "●" else "○"
        let control = BeamLayout.rayControlOf (supportedEmissionOf model p) p
        let switchable =
            match control with
            | BeamLayout.RaysSwitchable -> ControlOffered
            | BeamLayout.RaysFixed _ -> ControlWithheld
        let fixedNote =
            match control with
            | BeamLayout.RaysFixed reason -> [ elementMenuNote (ControlCaption $"({reason.text})") ElementMenuNote ]
            | BeamLayout.RaysSwitchable -> []
        let groups = p.outgoingGroups
        let rayRows =
            match p.catalogueKind with
            | LightSource | Detector -> fixedNote
            | LinearPolarizer | CircularPolarizer | Sample | Lens | FlatMirror | CurvedMirror ->
                [ elementMenuNote (ControlCaption "Rays") ElementMenuHeading
                  elementMenuItem ConstructorUiId.elementMenuToggleReflected (ControlCaption $"{check (List.contains ReflectedRayGroup groups)} Reflected ray") switchable (fun () -> dispatch (ToggleElementRay (e.id, BeamTree.ReflectedBeam)))
                  elementMenuItem ConstructorUiId.elementMenuToggleTransmitted (ControlCaption $"{check (List.contains TransmittedRayGroup groups)} Transmitted ray") switchable (fun () -> dispatch (ToggleElementRay (e.id, BeamTree.TransmittedBeam))) ]
                @ fixedNote
        let beamRows =
            match i.value with
            | 0 -> []
            | _ ->
                let current = (List.item i.value model.sceneLayout.elements).beam
                let reflectedRows =
                    BeamLayout.attachableHosts model.layoutElements e.id
                    |> List.choose (fun hostId ->
                        model.elements
                        |> List.tryFindIndex (fun x -> x.id = hostId)
                        |> Option.map (fun j ->
                            let host = BeamLayout.OnReflectedBeamOf hostId
                            elementMenuItem (ConstructorUiId.elementMenuPlaceOnReflectedOf hostId) (ControlCaption $"{radio (current = host)} Reflected ray of {nameOf j}") ControlOffered (fun () -> dispatch (PlaceOnBeam (e.id, host)))))
                [ elementMenuNote (ControlCaption "Sits on") ElementMenuHeading
                  elementMenuItem ConstructorUiId.elementMenuPlaceOnMainBeam (ControlCaption $"{radio (current = BeamLayout.OnMainBeam)} Main beam") ControlOffered (fun () -> dispatch (PlaceOnBeam (e.id, BeamLayout.OnMainBeam))) ]
                @ reflectedRows
        // Spec 0046: what the element is — the bound entry's name and description — under its title.
        let info =
            TextBlock.create [
                ConstructorUiId.elementMenuInfo.automationAttribute<TextBlock> ()
                TextBlock.text (elementInfo model i).detail.value
                TextBlock.textWrapping TextWrapping.Wrap
                TextBlock.margin (Thickness(10.0, 0.0, 10.0, 4.0))
                TextBlock.foreground (brush (Color.FromRgb(70uy, 70uy, 70uy)))
            ] :> IView
        [ elementMenuNote (ControlCaption (nameOf i.value)) ElementMenuHeading
          info
          elementMenuChooseItem model dispatch e ]
        @ rayRows
        @ beamRows
        @ [ elementMenuItem ConstructorUiId.elementMenuRemove (ControlCaption "Remove element") ControlOffered (fun () -> dispatch RemoveSelectedElement)
            elementMenuItem ConstructorUiId.elementMenuClose (ControlCaption "Close") ControlOffered (fun () -> dispatch CloseElementMenu) ]

/// The element-menu overlay layer — a background-less canvas the size of the table canvas (so clicks
/// off the menu fall through to the table), holding the open menu at its click point, kept on the canvas.
let private elementMenuOverlay (model : TableAndElementRotationModel) (dispatch : TableAndElementRotationMsg -> unit) : IView =
    let menuViews : IView list =
        match model.elementMenu with
        | None -> []
        | Some menu ->
            match elementMenuRows model dispatch menu with
            | [] -> []
            | rows ->
                let estimatedHeight = elementMenuRowHeight.value * float (List.length rows) + 12.0
                let left = max 0.0 (min (tableAndElementRotationCanvasWidth - elementMenuMaxWidth.value) (menu.at.sx + 6.0))
                let top = max 0.0 (min (tableAndElementRotationCanvasHeight - estimatedHeight) (menu.at.sy + 6.0))
                [ Border.create [
                      ConstructorUiId.elementMenu.automationAttribute<Border> ()
                      Canvas.left left
                      Canvas.top top
                      Border.minWidth elementMenuMinWidth.value
                      Border.maxWidth elementMenuMaxWidth.value
                      Border.background (brush (Color.FromRgb(255uy, 255uy, 255uy)))
                      Border.borderBrush (brush (Color.FromRgb(120uy, 120uy, 120uy)))
                      Border.borderThickness 1.0
                      Border.cornerRadius (CornerRadius 3.0)
                      Border.padding (Thickness(0.0, 4.0))
                      Border.child (StackPanel.create [ StackPanel.orientation Orientation.Vertical; StackPanel.children rows ])
                  ]
                  |> Avalonia.FuncUI.DSL.View.withKey ConstructorUiId.elementMenu.value
                  :> IView ]
    Canvas.create [
        Canvas.width tableAndElementRotationCanvasWidth
        Canvas.height tableAndElementRotationCanvasHeight
        Canvas.horizontalAlignment HorizontalAlignment.Left
        Canvas.verticalAlignment VerticalAlignment.Top
        Canvas.children menuViews
    ] :> IView

/// The radius of an element's hover target on the table, in pixels.
let private elementHoverRadiusPx : CanvasLength = CanvasLength 22.0

/// Spec 0046: where a mouse-wheel notch over the Main table goes — the table's own gesture (zoom, or with a
/// modifier rotate / zoom an element), or the scrolling page around it.
type WheelTarget =
    | WheelToTable
    | WheelToPage

/// A vertical distance on the scrolling Main page, in device-independent pixels.
type PagePixels =
    | PagePixels of float

    member this.value = let (PagePixels p) = this in p

/// The page's vertical scroll situation as seen from the table: the table's top / bottom edge relative to
/// the viewport's top, the viewport's height, and the page's scroll offset and its largest value.
type PageScroll =
    {
        tableTop : PagePixels
        tableBottom : PagePixels
        viewportHeight : PagePixels
        offset : PagePixels
        maxOffset : PagePixels
    }

/// Spec 0046: a PLAIN wheel over the table scrolls the page while the table is not fully in view and the
/// page can still scroll that way (wheel down = page down), so scrolling down always brings the whole table
/// into view; once it is in view (or the page cannot scroll further) the wheel zooms the table as before.
/// A wheel with any modifier is always the table's (rotate / zoom an element). No page — the table's.
let wheelTarget (mods : Set<WheelModifier>) (page : PageScroll option) (notches : WheelNotches) : WheelTarget =
    match page with
    | None -> WheelToTable
    | Some _ when not (Set.isEmpty mods) -> WheelToTable
    | Some p ->
        let tableInView = p.tableTop.value >= -0.5 && p.tableBottom.value <= p.viewportHeight.value + 0.5
        let canScroll = if notches.value < 0 then p.offset.value < p.maxOffset.value - 0.5 else p.offset.value > 0.5
        if (not tableInView) && canScroll then WheelToPage else WheelToTable

/// The live `PageScroll` for a wheel event raised inside the Main table (IO edge: reads the realized
/// page ScrollViewer and the table canvas's position in it); `None` outside a page viewer.
let private pageScrollOf (source : obj) : PageScroll option =
    match source with
    | :? Visual as v ->
        let viewerOpt =
            Avalonia.VisualTree.VisualExtensions.GetVisualAncestors v
            |> Seq.tryPick (function
                | :? ScrollViewer as s when s.Name = ConstructorUiId.tableElementScrollViewer.value -> Some s
                | _ -> None)
        match viewerOpt with
        | Some viewer ->
            Avalonia.VisualTree.VisualExtensions.GetVisualDescendants viewer
            |> Seq.tryPick (function
                | :? Canvas as c when c.Name = ConstructorUiId.tableElementCanvas.value -> Some c
                | _ -> None)
            |> Option.bind (fun canvas ->
                canvas.TranslatePoint(Point(0.0, 0.0), viewer)
                |> Option.ofNullable
                |> Option.map (fun top ->
                    {
                        tableTop = PagePixels top.Y
                        tableBottom = PagePixels (top.Y + canvas.Bounds.Height)
                        viewportHeight = PagePixels viewer.Viewport.Height
                        offset = PagePixels viewer.Offset.Y
                        maxOffset = PagePixels (max 0.0 (viewer.Extent.Height - viewer.Viewport.Height))
                    }))
        | None -> None
    | _ -> None

type TableAndElementRotationModel with

    /// Spec 0046: one invisible (transparent, hit-testable) disc over each element's drawn centre carrying the
    /// "what is it" tooltip (`elementInfo`). The discs only receive the hover — every press / release still
    /// bubbles to the table surface, which selects and pans exactly as before.
    member internal model.elementHoverTargets : IView list =
        let centres = model.snappedCentres
        let radius = elementHoverRadiusPx.value
        model.indexedElements
        |> List.collect (fun (i, e) ->
            match Map.tryFind i centres with
            | Some centre ->
                let c = TableScene.projectOnScene model.view centre
                let info = elementInfo model i
                [ Ellipse.create [
                      (ConstructorUiId.elementHover e.id).automationAttribute<Ellipse> ()
                      (ControlCaption $"{info.heading.value}\n{info.detail.value}").toolTipAttribute<Ellipse> ()
                      Ellipse.left (c.sx - radius)
                      Ellipse.top (c.sy - radius)
                      Ellipse.width (2.0 * radius)
                      Ellipse.height (2.0 * radius)
                      Ellipse.fill Brushes.Transparent
                  ]
                  |> Avalonia.FuncUI.DSL.View.withKey (ConstructorUiId.elementHover e.id).value
                  :> IView ]
            | None -> [])

    member internal model.mainTableCanvas : IView =
        Canvas.create [
            Canvas.name ConstructorUiId.tableElementCanvas.value
            Canvas.width tableAndElementRotationCanvasWidth
            Canvas.height tableAndElementRotationCanvasHeight
            Canvas.horizontalAlignment HorizontalAlignment.Left
            Canvas.verticalAlignment VerticalAlignment.Top
            // Spec 0038 (031): the out-of-band dispersion badges overlay the drawn elements — appended
            // last so they sit on top of the schematic; an in-band scene contributes none. Spec 0046: the
            // invisible hover targets (the "what is it" tooltips) go on top of everything.
            Canvas.children (model.mainTableViews @ model.mainElementViews @ model.outOfBandBadges @ model.elementHoverTargets)
        ] :> IView

/// The Main screen view: the ribbon of large controls on top, and BELOW the ribbon strip either the
/// shared table (for an in-pane "table" bay — same selection / pan / zoom / rotate gestures as the test
/// scene) or the active bay's OWN content (for a FULL-SURFACE bay — which fills that whole area and
/// wires NO table gestures; spec 0038 step 015: no current bay uses the mode — the samples workbench
/// that did is the Library WINDOW now — but the keyed slot stays for any future full-surface bay).
/// The bays are built once and shared: the ribbon hosts the in-pane bays' content and shows only a
/// tab for a full-surface bay, whose content is placed here below.
let mainView (model : TableAndElementRotationModel) (dispatch : TableAndElementRotationMsg -> unit) : IView =
    let toScreen (e : PointerEventArgs) : ScreenPoint = SceneInput.canvasPoint ConstructorUiId.tableElementCanvas e
    // The shared table surface + its pointer / wheel gestures. A thunk, so it is only built for a table bay:
    // a full-surface bay never realizes the canvas and must not wire the table gestures.
    // Spec 0046: a RIGHT click opens the element menu on release (its press starts no drag); the menu
    // overlay sits above the surface as a sibling, so its rows get their own clicks.
    // Spec 0050 S8.4: the slot bar sits above the table, and the grid panel, while open, between them.
    let tableSurface () : IView =
        let setupGridPanel : IView list =
            match model.setupGridState with
            | Some state -> [ SetupGridView.setupGridView state (setupGridHandlers dispatch) ]
            | None -> []
        DockPanel.create [
            DockPanel.children [
                StackPanel.create [
                    StackPanel.dock Dock.Top
                    StackPanel.orientation Orientation.Vertical
                    StackPanel.children (SlotBarView.slotBarView model.slotBarState (slotBarHandlers model dispatch) :: setupGridPanel)
                ]
                Panel.create [
                    Panel.children [
                        Border.create [
                            Border.background (brush (Color.FromRgb(250uy, 250uy, 250uy)))
                            Border.onPointerPressed (fun e ->
                                e.Handled <- true
                                match e.GetCurrentPoint(null).Properties.IsRightButtonPressed with
                                | true -> ()
                                | false -> dispatch (PointerDown (toScreen e)))
                            Border.onPointerMoved (fun e -> e.Handled <- true; dispatch (PointerMove (toScreen e)))
                            Border.onPointerReleased (fun e ->
                                e.Handled <- true
                                match e.InitialPressMouseButton with
                                | MouseButton.Right -> dispatch (OpenElementMenuAt (toScreen e))
                                | _ -> dispatch (PointerUp (toScreen e)))
                            // Spec 0046: a plain wheel scrolls the PAGE until the table is fully in view (left
                            // unhandled, it bubbles to the page's ScrollViewer); then — and always with a modifier —
                            // it is the table's gesture.
                            Border.onPointerWheelChanged (fun e ->
                                let mods = wheelModifiers e.KeyModifiers
                                let notches = WheelNotches (if e.Delta.Y >= 0.0 then 1 else -1)
                                match wheelTarget mods (pageScrollOf e.Source) notches with
                                | WheelToPage -> ()
                                | WheelToTable ->
                                    e.Handled <- true
                                    dispatch (Wheel (mods, notches)))
                            Border.child model.mainTableCanvas
                        ]
                        elementMenuOverlay model dispatch
                    ]
                ]
            ]
        ] :> IView
    let bays = mainBays model dispatch
    // The area BELOW the ribbon strip: a full-surface bay fills it with the bay's OWN content (no table
    // canvas, no gestures — the workbench owns the whole surface); every in-pane / table bay keeps the
    // shared table canvas and its pan / zoom / rotate gestures.
    let belowStrip : IView =
        match ({ bays = bays; selected = model.ribbon } : Ribbon.RibbonState).activeBay with
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
                    |> Avalonia.FuncUI.DSL.View.withKey b.name.value
                surface :> IView
            | Ribbon.InRibbonPane -> tableSurface ()
        | None -> tableSurface ()
    // Spec 0046: the whole surface scrolls, both ways — a tall bay (the Experiments editor + chart +
    // builder) pushes the table below the window's bottom edge, and a window narrower than the table cuts
    // its right side; nothing may become unreachable. The `MinWidthPanel` lays the page out at the
    // window's width (so the tab strip still wraps there and the right-docked Materials… / Library…
    // buttons stay on its right edge) but never narrower than the table, and its height is the content's
    // height at that width, so the extent is exactly bars + table. A plain wheel over the table scrolls
    // the page until the table is fully in view (`wheelTarget`), then zooms it.
    ScrollViewer.create [
        ScrollViewer.name ConstructorUiId.tableElementScrollViewer.value
        ScrollViewer.horizontalScrollBarVisibility Avalonia.Controls.Primitives.ScrollBarVisibility.Auto
        ScrollViewer.verticalScrollBarVisibility Avalonia.Controls.Primitives.ScrollBarVisibility.Auto
        ScrollViewer.content (
            MinWidthPanel.create [
                (PanelMinWidth tableAndElementRotationCanvasWidth).minWidthPanelAttribute ()
                MinWidthPanel.child (
                    DockPanel.create [
                        DockPanel.children [
                            Border.create [ Border.dock Dock.Top; Border.child (mainControlBar bays model dispatch) ]
                            belowStrip
                        ]
                    ])
            ])
    ] :> IView
