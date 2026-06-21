/// Root MVU surface for the Optical Constructor desktop app (spec 0024 Part U1 /
/// R-2..R-5). Holds the root model / message / dispatcher and the root view that the
/// Elmish program (mounted from `MainWindow`, Program.fs) and the headless UI tests
/// drive. Lives in `OpticalConstructor.Ui` (NOT the App, §U1.2) so the headless UI
/// test project can mount it without launching the desktop App.
///
/// U1 is the foundation the later parts extend: each part adds only the `RootMsg`
/// cases and effectful `Cmd`s it wires. U1 adds the `Construction`, `Workspace`, and
/// `Shell` cases and attaches `Cmd.none` to every transition (no effects yet, a U1
/// non-requirement). The model stays pure/serializable (§0.5): it holds no
/// `CancellationTokenSource`, renderer control, or `IStorageProvider`.
///
/// Authored against the public MIT `Avalonia.FuncUI` 1.6.0 DSL surface (§0.2) — no
/// reference to the audit-gated clone. Panel layout/visibility/dock route through the
/// existing pure reducers in `AppShell` (§0.3 public-Avalonia layout only).
module OpticalConstructor.Ui.Shell

open System.IO
open Avalonia.Controls
open Avalonia.Layout
open Avalonia.Media
open Avalonia.Platform.Storage
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open Elmish
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Project
open OpticalConstructor.Optimization
open OpticalConstructor.Optimization.OptimizationInterface
open OpticalConstructor.Storage
open OpticalConstructor.Ui.Charts
open OpticalConstructor.Ui.Sources
open OpticalConstructor.Ui.UserEnvironment

/// `Workspace` is both an existing module and a `RootMsg` case name below; the alias
/// keeps the module reachable unambiguously in value position.
module WS = OpticalConstructor.Ui.Workspace

/// `Ribbon` is both the new ribbon module (slice 006) and a `RootMsg`/sub-model name
/// below; the alias keeps the module reachable unambiguously in value position, exactly
/// as `WS` does for `Workspace`.
module Rb = OpticalConstructor.Ui.Ribbon

/// Host-layer holder for the in-flight fit's `CancellationTokenSource` (§0.5): the
/// fit page hosts ONE cancellable run at a time, so a single module-level cell is the
/// "host field" the spec admits. It is NEVER a field of the pure/serializable root
/// model — `SynthesisFitPage.Model.running`/`progress` is the single source of truth
/// for run state; this cell only carries the non-serializable cancellation handle.
let mutable private fitCts : System.Threading.CancellationTokenSource option = None

/// Host-layer holder for the Avalonia `IStorageProvider` the Open/Save pickers use
/// (§0.5): it is the non-serializable Avalonia handle the spec mandates live OUTSIDE
/// the root model. The composition root (`Program.fs`) registers the window's provider
/// on open; headless tests leave it `None` so the IO `Cmd`s take their deterministic
/// fallback path (the project's working folder).
let mutable private storageProvider : IStorageProvider option = None

/// Register the live window storage provider (called by the composition root, §0.5).
let setStorageProvider (provider : IStorageProvider) : unit = storageProvider <- Some provider

/// Test seam: force the picker-less fallback path regardless of any provider a prior
/// in-process headless App boot may have registered.
let clearStorageProviderForTests () : unit = storageProvider <- None

/// The destination the environment-persist `Cmd` writes to (R-3). Defaults to the
/// per-user settings path; a test seam overrides it so the round-trip is verified
/// without clobbering real per-user state.
let mutable private environmentPath : string = UserEnvironment.settingsPath ()

/// Test seam: redirect environment persistence to a scratch path (R-3 round-trip).
let setEnvironmentPathForTests (path : string) : unit = environmentPath <- path

// ---------------------------------------------------------------------------
// Root model, message, and the shell-level sub-message (R-2).
// ---------------------------------------------------------------------------

/// The top-level pages (spec 0024 R-2, extended by spec 0026 D.6). `Constructor` is the
/// new ribbon front door and the default landing (D.6); `Legacy` opens the present
/// dockable-panel main screen (D.5). The original `Construction` / `SynthesisFit` pages
/// are the legacy dockable screen's two sub-pages, reachable under `Legacy`.
type Page =
    | Construction
    | SynthesisFit
    | Constructor
    | Legacy

/// Shell-level edits the `Shell` `RootMsg` case carries (R-2 — navigation / theme /
/// panel-show / dock / UI language). Each routes through an existing pure reducer (or,
/// for `SetLanguage`, a plain `env` field write persisted like the theme).
type ShellMsg =
    | Navigate of Page
    | ToggleTheme
    | SetPanelVisible of panel : string * visible : bool
    | SetPanelDock of panel : string * dock : DockSide
    /// Spec 0026 §I.2 / D.1.1: set the UI language from the Settings ribbon. Updates the
    /// persisted `env.language`; every UI string then resolves through that language.
    | SetLanguage of Localization.Language

/// The root MVU model aggregating the existing sub-models (R-2). Pure and
/// serializable (§0.5): no Avalonia handle, renderer instance, or token source.
///
/// `[<ReferenceEquality>]`: the aggregated sub-models embed engine values
/// (`EmFieldSystem`'s `Matrix<Complex>`, etc.) that carry no structural equality, so
/// the record cannot derive one. The Elmish host only needs *an* equality to satisfy
/// its generic constraint; reference equality suffices because FuncUI re-renders and
/// diffs the view tree on every dispatch rather than comparing models.
[<ReferenceEquality>]
type RootModel =
    {
        /// Persisted environment (theme + panel layout) driving the first frame.
        env : EnvironmentSettings
        /// The currently displayed page.
        page : Page
        construction : ConstructionPage.Model
        workspace : WS.Model
        /// Opened on demand by Part U7; `None` until then.
        fit : SynthesisFitPage.Model option
        chart : ChartSettings.ChartSettings
        markers : Readout.Markers
        materials : MaterialLibrary.MaterialLibrary
        /// The materials panel's browse filter + selection (Part U3). Pure/serializable
        /// (§0.5): plain values, never a renderer handle.
        materialsFilter : MaterialsView.Filter
        /// The source the `sources` panel edits (Part U4). Pure/serializable (§0.5): a
        /// `SourceSpec` carries only plain engine values, never a renderer handle.
        source : SourceSpec.SourceSpec
        /// The last lifecycle/IO status or error surfaced to the toolbar (Part U8 / R-1).
        /// Pure: a plain string, never a renderer handle.
        status : string option
        /// The constructor MVU page (Spec 0026 Part C/E, slice 005) the new ribbon front
        /// door drives (D.6). Pure/serializable (§0.3): the canonical project + ephemeral
        /// view/selection/transient state, no Avalonia handle.
        constructor : ConstructorView.Model
        /// The ribbon shell's own UI state (Spec 0026 Part D, slice 006): collapsed flag +
        /// selected tab. Pure: plain values projected over the slice-005 command registry.
        ribbon : Rb.Model
        /// The reusable experiment collections loaded from the separate per-user groups
        /// library. The constructor edits group state; collections are preserved across
        /// those saves until their own authoring UI lands.
        collections : Groups.ExperimentCollection list
        /// The loaded EN/RU string resource (Spec 0026 Part I, slice 003), read once at
        /// startup; the ribbon/menus/nav resolve their labels against it for the active
        /// `env.language`. Pure data (a `Map`), never an Avalonia handle.
        strings : Localization.Resource
    }

/// The root message, wrapping each wired sub-`Msg` plus the shell-level case (R-2).
/// U1 wires exactly `Construction`, `Workspace`, and `Shell`; later parts add their
/// own cases (`Fit`, `Source`, `Chart`, `Io`) when they wire them.
type RootMsg =
    | Construction of ConstructionPage.Msg
    | Workspace of WS.Msg
    | Shell of ShellMsg
    | Chart of ChartView.ChartMsg
    | Materials of MaterialsView.MaterialsMsg
    | Source of SourceView.SourceViewMsg
    /// Part U7: the synthesis/fit page sub-message, routed through `SynthesisFitPage.update`.
    | Fit of SynthesisFitPage.Msg
    /// Part U8: lifecycle / IO (open / save / new / template / gallery), routed through
    /// `updateIo` with §0.4-marshaled file-picker / reader / writer `Cmd`s.
    | Io of LifecycleView.IoMsg
    /// Spec 0026 Part C/E (slice 006 front door): the constructor MVU page's sub-message,
    /// routed through `ConstructorView.update`.
    | Constructor of ConstructorView.Msg
    /// Spec 0026 Part D (slice 006): the ribbon shell's UI sub-message (collapse / tab
    /// select), routed through `Ribbon.update`.
    | Ribbon of Rb.Msg

// ---------------------------------------------------------------------------
// Initial project + model (R-2 / R-3 seed). Built through the existing
// `Templates` factory rather than an inline literal so the seed stack reuses the
// canonical scaffold (its films and its single `Templates.defaultLight` 550 nm
// source) instead of re-deriving them — the cycle-1 review's reuse finding and
// the 600→550 nm drift. `Templates.bandpassFilter` is a ≥2-layer stack, so the
// read path renders real rows (R-5 / AC-U1.2) and the `ui-tests` view test's
// `layerRows.Length >= 2` assertion holds. R-2 still requires the dual
// construction/workspace project seed — both are seeded from this one result.
// The whole tree is solved once by `ConstructionPage.init`.
// ---------------------------------------------------------------------------

let private initialProject : OpticalConstructorProject = Templates.bandpassFilter ()

/// The default working folder for a fresh, unsaved project — the user's Documents
/// folder, never the repository root (§0.6). U1 writes nothing here (no save path is
/// wired); it is only the model seed `ConstructionPage`/`SynthesisFitPage` carry.
let private defaultWorkingFolder : string =
    System.IO.Path.Combine(
        System.Environment.GetFolderPath System.Environment.SpecialFolder.MyDocuments,
        "OpticalConstructor")

/// The empty fall-back string resource (Spec 0026 §I.3.1): when the shipped
/// `strings.json` is missing/unreadable, lookups fall back to the keys themselves so the
/// UI never crashes or shows blank — the same total-failure posture `Localization.lookup`
/// guarantees per key.
let private emptyStrings : Localization.Resource = { entries = Map.empty }

/// Load the shipped EN/RU string resource once at startup (Spec 0026 Part I): read the
/// build-copied `strings.json` from beside the assembly. Any load failure falls back to
/// the empty resource (lookups then yield the keys); the localization completeness
/// surface (`Program.fs`) reports the failure copyably either way.
let private loadStrings () : Localization.Resource =
    match Localization.loadFromFile (Localization.resourcePath ()) with
    | Ok resource -> resource
    | Error _ -> emptyStrings

/// Build the root model for the supplied environment (R-3 — the App seeds `env` from
/// the persisted `Startup.settings`). `update` attaches no effects in U1, so `init`
/// carries `Cmd.none`.
let initFrom (env : EnvironmentSettings) : RootModel * Cmd<RootMsg> =
    let groupsLibrary = GroupsLibrary.load (GroupsLibrary.libraryPath ())
    let model =
        {
            env = env
            // D.6.1: the constructor is the default landing page when the app starts.
            page = Page.Constructor
            construction = ConstructionPage.init initialProject defaultWorkingFolder "untitled"
            workspace = WS.init initialProject
            fit = None
            chart = ChartSettings.ChartSettings.defaultValue
            markers = Readout.Markers.empty
            materials = MaterialLibrary.standard
            materialsFilter = MaterialsView.Filter.empty
            source = SourceEditorView.defaultSource "source-1"
            status = None
            // Spec 0026 slice 006: the constructor page seeded over the same project,
            // the ribbon's default (expanded, Build) state, and the loaded EN/RU strings.
            constructor = ConstructorView.initWithGroups env initialProject groupsLibrary.groups
            ribbon = Rb.init
            collections = groupsLibrary.collections
            strings = loadStrings ()
        }
    model, Cmd.none

/// Root `init` (R-2) seeded from the built-in environment defaults — the value the
/// headless tests mount; the App threads its persisted settings through `initFrom`.
let init : RootModel * Cmd<RootMsg> = initFrom UserEnvironment.defaults

// ---------------------------------------------------------------------------
// update — the root dispatcher. Each case calls the matching sub-`update` and
// re-wraps the result. Part U2 attaches the only compute-bearing effect in the
// construction page: the async node-solve `Cmd` (R-2). Every other case stays
// effect-free (`Cmd.none`).
// ---------------------------------------------------------------------------

/// The parent path of a non-root node ([] for a depth-1 node); the root ([]) has no
/// parent and maps to itself.
let private parentPath (path : ConstructionPage.NodePath) : ConstructionPage.NodePath =
    match List.rev path with
    | [] -> []
    | _ :: revParent -> List.rev revParent

/// The §0.4-compliant async node-solve `Cmd` (R-2): re-solve the sub-tree rooted at
/// `path` OFF the UI thread (`Task.Run`), then marshal `Construction (NodeSolved map)`
/// back ONTO the UI thread via `Dispatcher.UIThread.Post` so the busy→solve→refresh
/// cycle never dispatches cross-thread. The effect is host-layer only; no
/// `CancellationTokenSource` or solve handle enters the root model (§0.5).
let private nodeSolveCmd (path : ConstructionPage.NodePath) (root : BeamTree.BeamNode) : Cmd<RootMsg> =
    [ fun (dispatch : RootMsg -> unit) ->
        System.Threading.Tasks.Task.Run(fun () ->
            let solved = ConstructionPage.solveSubtree path root
            Avalonia.Threading.Dispatcher.UIThread.Post(fun () ->
                dispatch (RootMsg.Construction (ConstructionPage.NodeSolved solved))))
        |> ignore ]

/// The async node-solve `Cmd` for a busy-marking construction transition (R-2):
/// `EditStack`/`AttachChild` re-solve the edited node's sub-tree; `ConfirmDeleteNode`
/// re-solves the removed node's parent (the removed path no longer resolves), so
/// dependent views refresh from `NodeSolved` with no manual reload. Any other
/// construction message is effect-free.
let private constructionCmd
    (msg : ConstructionPage.Msg)
    (priorPending : (ConstructionPage.NodePath * int) option)
    (root : BeamTree.BeamNode)
    : Cmd<RootMsg> =
    match msg with
    | ConstructionPage.EditStack (p, _)
    | ConstructionPage.AttachChild (p, _, _) -> nodeSolveCmd p root
    | ConstructionPage.ConfirmDeleteNode ->
        match priorPending with
        | Some (p, _) -> nodeSolveCmd (parentPath p) root
        | None -> Cmd.none
    | _ -> Cmd.none

// ---------------------------------------------------------------------------
// Part U7 — open the fit page on demand (R-1) and the marshaled background-run
// `Cmd`s (R-2/R-3). The page model carries the working system, the design
// parameters / start vector, the targets, and the method; the run is threaded
// through the frozen `LocalRefinement.refineWith` seam (no frozen edit, §0.1).
// ---------------------------------------------------------------------------

/// The system the fit refines: the workspace's active system, else the first system.
let private workingSystemOf (model : RootModel) =
    let systems = model.workspace.project.systems
    match model.workspace.active with
    | Some i when i >= 0 && i < List.length systems -> Some systems.[i]
    | _ -> match systems with | s :: _ -> Some s | [] -> None

/// Open a fresh `SynthesisFitPage.Model` over the working system (R-1). The target /
/// parameter editors that populate the merit set are a later concern; this slice
/// wires the threading, so the page opens empty (the user-facing Start with no
/// targets fails gracefully through `FitFailed`).
let private openFit (model : RootModel) : SynthesisFitPage.Model option =
    workingSystemOf model
    |> Option.map (fun sys -> SynthesisFitPage.init sys [] [||] [] LevenbergMarquardt defaultWorkingFolder)

/// §0.4 — marshal a fit sub-message dispatched from a background worker onto the UI
/// thread, so no completion/progress dispatch ever crosses threads directly.
let private marshalFit (dispatch : RootMsg -> unit) (msg : SynthesisFitPage.Msg) : unit =
    Avalonia.Threading.Dispatcher.UIThread.Post(fun () -> dispatch (RootMsg.Fit msg))

/// The §0.4/§0.5-compliant background-fit `Cmd` (R-2): create a host-held
/// `CancellationTokenSource` (§0.5), build a run thunk over the optimization run loop,
/// and hand it to `JobRunner.startBackground`. The run loop advances the design vector
/// by ONE LM iteration per cooperative `runIterations` step (resumed from the prior
/// step's solution) so progress is genuinely per-iteration and a cancel is observed
/// between iterations; `refineWith`/`FitQuality` are used unchanged (§0.1). The
/// progress callback is threaded through the run loop (`step`), NOT through
/// `startBackground` (whose signature is `run`/`onDone` only). Every progress /
/// completion dispatch is marshaled per §0.4.
let private startFitCmd (fit : SynthesisFitPage.Model) : Cmd<RootMsg> =
    [ fun (dispatch : RootMsg -> unit) ->
        let cts = new System.Threading.CancellationTokenSource()
        fitCts <- Some cts
        let baseSystem = fit.workingSystem
        let parameters = fit.parameters
        let targets = fit.targets
        let method = fit.method
        let residual = MeritFunction.buildResidual baseSystem parameters targets
        let run () =
            let mutable vec = Array.copy fit.initial
            let mutable converged = false
            let step (i : int) =
                match LocalRefinement.refineWith method 1 LocalRefinement.defaultEpsX baseSystem parameters vec targets with
                | Ok (sys, res) ->
                    vec <- res.solution
                    converged <- (res.terminationReason <> MaxIterationsReached)
                    let chi = res.finalResiduals |> Array.sumBy (fun r -> r * r)
                    // §0.4: the per-iteration progress dispatch is UI-thread-marshaled.
                    marshalFit dispatch (SynthesisFitPage.ReportProgress (i + 1, chi))
                    sys, res
                | Error e -> raise (System.InvalidOperationException e)
            let hasMore (i : int) : bool = i < LocalRefinement.defaultMaxIterations && not converged
            JobRunner.runIterations cts.Token hasMore (fun _ -> ()) step
        let onDone outcome =
            match outcome with
            | Ok (JobRunner.RanToCompletion results) ->
                match List.tryLast results with
                | Some (sys, res) ->
                    marshalFit dispatch (SynthesisFitPage.FitCompleted (sys, FitQuality.fromResult residual res))
                | None -> marshalFit dispatch (SynthesisFitPage.FitFailed "no iterations ran")
            // Cooperative cancellation resolves to FitFailed "cancelled" (R-3); the
            // partial output is dropped and the prior committed results stay intact.
            | Ok (JobRunner.CancelledWith _) -> marshalFit dispatch (SynthesisFitPage.FitFailed "cancelled")
            | Error e -> marshalFit dispatch (SynthesisFitPage.FitFailed e)
        JobRunner.startBackground run onDone ]

/// `Fit CancelFit` cancels the host-held token (R-3); the run loop observes it between
/// iterations and the background `onDone` posts `FitFailed "cancelled"`.
let private cancelFitCmd () : Cmd<RootMsg> =
    [ fun (_dispatch : RootMsg -> unit) ->
        match fitCts with
        | Some cts -> cts.Cancel()
        | None -> () ]

/// The fit page's effectful `Cmd` for a given sub-message: `StartFit` (when a run is
/// not already in flight) attaches the background-run `Cmd`; `CancelFit` cancels the
/// host token. Every other fit message is effect-free.
let private fitCmd (msg : SynthesisFitPage.Msg) (fit : SynthesisFitPage.Model) : Cmd<RootMsg> =
    match msg with
    | SynthesisFitPage.StartFit when not fit.running -> startFitCmd fit
    | SynthesisFitPage.CancelFit -> cancelFitCmd ()
    | _ -> Cmd.none

// ---------------------------------------------------------------------------
// Part U8 — lifecycle (open / save / new / templates / gallery, R-1/R-2) and
// environment persistence (R-3). The `IStorageProvider` lives in the host-layer
// `storageProvider` field (§0.5); every background continuation marshals back onto
// the UI thread via `Dispatcher.UIThread.Post` (§0.4). The picker is only a path
// source — the read/write logic is in `OpenPath` / `SaveRequested`, so it is fully
// exercised headlessly with the picker absent.
// ---------------------------------------------------------------------------

/// §0.4 — marshal a lifecycle sub-message dispatched from a background worker onto the
/// UI thread, so no IO continuation ever dispatches cross-thread.
let private marshalIo (dispatch : RootMsg -> unit) (msg : LifecycleView.IoMsg) : unit =
    Avalonia.Threading.Dispatcher.UIThread.Post(fun () -> dispatch (RootMsg.Io msg))

/// Read + schema-validate a project at `path` OFF the UI thread through the existing
/// `ProjectFile.openProject` seam (validate-on-load, §A.7), then marshal `Io (Loaded …)`
/// (or an `Io (IoError …)` surface) back onto the UI thread (§0.4). No private deserialize.
let private openPathCmd (path : string) : Cmd<RootMsg> =
    [ fun (dispatch : RootMsg -> unit) ->
        System.Threading.Tasks.Task.Run(fun () ->
            match ProjectFile.openProject path with
            | Ok project -> marshalIo dispatch (LifecycleView.Loaded (project, path))
            | Error e -> marshalIo dispatch (LifecycleView.IoError (sprintf "%A" e)))
        |> ignore ]

/// Raise the `IStorageProvider` open picker (R-1, host layer §0.5). The picker is only
/// a path source: its continuation marshals `Io (OpenPath …)` onto the UI thread (§0.4),
/// and the actual read happens in `openPathCmd`. When no host provider is registered
/// (headless), this is a no-op — the load path is driven directly via `Io (OpenPath …)`.
let private openCmd () : Cmd<RootMsg> =
    [ fun (dispatch : RootMsg -> unit) ->
        match storageProvider with
        | None -> ()
        | Some provider ->
            let opts = FilePickerOpenOptions(AllowMultiple = false)
            (provider.OpenFilePickerAsync opts).ContinueWith(fun (t : System.Threading.Tasks.Task<System.Collections.Generic.IReadOnlyList<IStorageFile>>) ->
                if t.IsCompletedSuccessfully then
                    match Seq.tryHead t.Result with
                    | Some file -> marshalIo dispatch (LifecycleView.OpenPath file.Path.LocalPath)
                    | None -> ())
            |> ignore ]

/// Save the AUTHORITATIVE constructor project to `<name>.ocproj.json` (R-1 / AC-U8.1)
/// via the shared `ProjectJson.serializeProject` seam. The write is OFF the UI thread
/// and its `Io (Saved …)` / `Io (IoError …)` result marshals back (§0.4). When a host
/// provider is registered the save picker chooses the destination; otherwise the
/// working-folder fallback path is used. This deliberately accepts the project directly:
/// the constructor front door owns the live project, while the legacy construction page
/// is kept in sync as a secondary surface.
let private saveCmdForProject
    (project : OpticalConstructorProject)
    (workingFolder : string)
    (projectName : string)
    : Cmd<RootMsg> =
    [ fun (dispatch : RootMsg -> unit) ->
        match ProjectJson.serializeProject project with
        | Error e -> marshalIo dispatch (LifecycleView.IoError (sprintf "%A" e))
        | Ok json ->
            let defaultPath = Path.Combine(workingFolder, projectName + ".ocproj.json")
            let writeTo (path : string) =
                System.Threading.Tasks.Task.Run(fun () ->
                    try
                        Directory.CreateDirectory(Path.GetDirectoryName path) |> ignore
                        File.WriteAllText(path, json)
                        marshalIo dispatch (LifecycleView.Saved path)
                    with e -> marshalIo dispatch (LifecycleView.IoError e.Message))
                |> ignore
            match storageProvider with
            | Some provider ->
                let opts = FilePickerSaveOptions(SuggestedFileName = Path.GetFileName defaultPath)
                (provider.SaveFilePickerAsync opts).ContinueWith(fun (t : System.Threading.Tasks.Task<IStorageFile>) ->
                    if t.IsCompletedSuccessfully then
                        match t.Result with
                        | null -> ()
                        | file -> writeTo file.Path.LocalPath)
                |> ignore
            | None -> writeTo defaultPath ]

/// Fire-and-forget environment persistence (R-3): write the updated `env` through the
/// frozen `UserEnvironment.save` OFF the UI thread. It dispatches nothing, so §0.4 is
/// moot; a write failure is swallowed (persistence is best-effort, AC-J6).
let private persistEnvCmd (env : EnvironmentSettings) : Cmd<RootMsg> =
    [ fun (_dispatch : RootMsg -> unit) ->
        System.Threading.Tasks.Task.Run(fun () ->
            UserEnvironment.save environmentPath env |> ignore)
        |> ignore ]

let private saveGroupsLibraryCmd
    (groups : Groups.ElementGroup list)
    (collections : Groups.ExperimentCollection list)
    : Cmd<RootMsg> =
    [ fun (dispatch : RootMsg -> unit) ->
        System.Threading.Tasks.Task.Run(fun () ->
            match GroupsLibrary.save (GroupsLibrary.libraryPath ()) { groups = groups; collections = collections } with
            | Ok () -> ()
            | Error e -> marshalIo dispatch (LifecycleView.IoError (sprintf "Groups library error: %A" e)))
        |> ignore ]

/// Strip the committable `<name>.ocproj.json` suffix back to the bare project name.
let private projectNameOfPath (path : string) : string =
    let stem = Path.GetFileNameWithoutExtension path   // "<name>.ocproj"
    if stem.EndsWith ".ocproj" then stem.Substring(0, stem.Length - ".ocproj".Length) else stem

/// Keep the legacy construction/workspace surfaces synchronized with the constructor
/// project. The constructor is the front-door owner; legacy views are re-seeded from the
/// same project so navigating away from the constructor cannot reveal a stale snapshot.
let private syncLegacySurfaces (project : OpticalConstructorProject) (model : RootModel) : RootModel =
    { model with
        construction = ConstructionPage.init project model.construction.workingFolder model.construction.projectName
        workspace = WS.init project
        fit = None }

/// Load a freshly-opened/templated project into the root model: re-seed the constructor,
/// construction page (which solves the tree once), and workspace from ONE project, drop
/// any open fit page, and land on the constructor front door (R-1 / R-2 / Spec 0026 D.6).
let private loadProjectInto
    (project : OpticalConstructorProject)
    (folder : string)
    (name : string)
    (statusMsg : string)
    (model : RootModel)
    : RootModel =
    { model with
        construction = ConstructionPage.init project folder name
        workspace = WS.init project
        fit = None
        constructor = ConstructorView.initWithGroups model.env project model.constructor.groups
        page = Page.Constructor
        status = Some statusMsg }

let private updateIo (im : LifecycleView.IoMsg) (model : RootModel) : RootModel * Cmd<RootMsg> =
    match im with
    | LifecycleView.NewProject ->
        loadProjectInto initialProject defaultWorkingFolder "untitled" "New project" model, Cmd.none
    | LifecycleView.OpenRequested -> model, openCmd ()
    | LifecycleView.OpenPath path -> model, openPathCmd path
    | LifecycleView.Loaded (project, path) ->
        loadProjectInto project (Path.GetDirectoryName path) (projectNameOfPath path)
            (sprintf "Opened %s" (Path.GetFileName path)) model, Cmd.none
    | LifecycleView.SaveRequested ->
        model, saveCmdForProject model.constructor.project model.construction.workingFolder model.construction.projectName
    | LifecycleView.Saved path ->
        // Subsequent saves follow the just-written destination.
        { model with
            construction = { model.construction with workingFolder = Path.GetDirectoryName path; projectName = projectNameOfPath path }
            status = Some (sprintf "Saved %s" (Path.GetFileName path)) }, Cmd.none
    | LifecycleView.IoError e -> { model with status = Some (sprintf "Error: %s" e) }, Cmd.none
    | LifecycleView.LoadTemplate entry ->
        // R-2: load through the existing schema-validated factory (no private deserialize).
        match Templates.loadTemplate entry.build with
        | Ok project -> loadProjectInto project defaultWorkingFolder entry.title (sprintf "Loaded template %s" entry.title) model, Cmd.none
        | Error e -> { model with status = Some (sprintf "Template error: %A" e) }, Cmd.none
    | LifecycleView.LoadGallery entry ->
        match Help.openEntry entry with
        | Ok project -> loadProjectInto project defaultWorkingFolder entry.title (sprintf "Loaded sample %s" entry.title) model, Cmd.none
        | Error e -> { model with status = Some (sprintf "Gallery error: %A" e) }, Cmd.none

let private updateShell (msg : ShellMsg) (model : RootModel) : RootModel =
    match msg with
    | Navigate page ->
        match page with
        // R-1: navigating to the fit page opens a SynthesisFitPage.Model into `fit`
        // (once — an already-open page keeps its in-flight run/progress state).
        | Page.SynthesisFit when Option.isNone model.fit -> { model with page = page; fit = openFit model }
        | _ -> { model with page = page }
    | ToggleTheme -> { model with env = { model.env with theme = AppShell.toggleTheme model.env.theme } }
    | SetPanelVisible (panel, visible) ->
        { model with env = { model.env with layout = AppShell.setPanelVisible panel visible model.env.layout } }
    | SetPanelDock (panel, dock) ->
        { model with env = { model.env with layout = AppShell.dockPanel panel dock model.env.layout } }
    // D.1.1 / I.2: switch the UI language; the persisted `env.language` drives every
    // `Localization.lookup`, so the ribbon/menus/nav re-render in the chosen language.
    | SetLanguage language -> { model with env = { model.env with language = language } }

let update (msg : RootMsg) (model : RootModel) : RootModel * Cmd<RootMsg> =
    match msg with
    | RootMsg.Construction cm ->
        let construction = ConstructionPage.update cm model.construction
        // §0.4: a busy-marking structural edit (EditStack/AttachChild/ConfirmDeleteNode)
        // attaches the async node-solve Cmd; the pure update already set `busy` and the
        // host re-solves off-thread, marshaling NodeSolved back onto the UI thread.
        let cmd = constructionCmd cm model.construction.pendingDeletion construction.project.beamTree.root
        let model' = { model with construction = construction }
        if construction.project <> model.construction.project then
            { model' with
                constructor = ConstructorView.initWithGroups model.env construction.project model.constructor.groups
                workspace = WS.init construction.project
                fit = None }, cmd
        else model', cmd
    | RootMsg.Workspace wm -> { model with workspace = WS.update wm model.workspace }, Cmd.none
    | RootMsg.Shell sm ->
        // R-3: a theme / panel-visibility / panel-dock change attaches a fire-and-forget
        // `UserEnvironment.save` persist `Cmd` so the layout/theme round-trips live; plain
        // navigation does not touch `env` and so persists nothing.
        let model' = updateShell sm model
        let cmd =
            match sm with
            | ToggleTheme | SetPanelVisible _ | SetPanelDock _ | SetLanguage _ -> persistEnvCmd model'.env
            | Navigate _ -> Cmd.none
        model', cmd
    | RootMsg.Chart cm ->
        let chart, markers = ChartView.update cm (model.chart, model.markers)
        { model with chart = chart; markers = markers }, Cmd.none
    | RootMsg.Materials mm -> { model with materialsFilter = MaterialsView.update mm model.materialsFilter }, Cmd.none
    | RootMsg.Source sm -> { model with source = SourceView.update sm model.source }, Cmd.none
    | RootMsg.Fit fm ->
        // Part U7: route through the frozen `SynthesisFitPage.update` (which flips
        // `running` on StartFit) and attach the §0.4-marshaled background-run Cmd. The
        // Cmd is built from the PRE-update model so the StartFit guard sees the prior
        // `running` state and the run thunk reads the run inputs (R-2).
        match model.fit with
        | Some fit -> { model with fit = Some (SynthesisFitPage.update fm fit) }, fitCmd fm fit
        | None -> model, Cmd.none
    | RootMsg.Io im -> updateIo im model
    // Spec 0026: the constructor front door owns the live project. Route through its
    // reducer, synchronize the legacy surfaces when the project changed, and turn the
    // constructor Save command into the shared project-write Cmd.
    | RootMsg.Constructor cm ->
        let constructor = ConstructorView.update cm model.constructor
        let saveRequested = constructor.saveRequests > model.constructor.saveRequests
        let groupsChanged = constructor.groups <> model.constructor.groups
        let model' = { model with constructor = constructor }
        let model'' =
            if constructor.project <> model.constructor.project then syncLegacySurfaces constructor.project model'
            else model'
        let cmd : Cmd<RootMsg> =
            [ if saveRequested then
                  yield! saveCmdForProject constructor.project model''.construction.workingFolder model''.construction.projectName
              if groupsChanged then
                  yield! saveGroupsLibraryCmd constructor.groups model.collections ]
        model'', cmd
    | RootMsg.Ribbon rm -> { model with ribbon = Rb.update rm model.ribbon }, Cmd.none

// ---------------------------------------------------------------------------
// view (R-4) — a top-level navigation control over a DockPanel of the visible
// panels. The `stack` id renders real content; the other four render a titled
// placeholder until their part lands (R-8). The visible/ordered panel set and the
// dock edges keep coming from the existing pure `AppShell` reducers.
// ---------------------------------------------------------------------------

/// A navigation button (UX commitment 1 / AC-U1.1): the active page marked by a distinct
/// background, navigating through the single `RootMsg.Shell (Navigate …)` path (R-7 /
/// D.6.1 — navigation stays single-sourced). `isActive` is supplied explicitly so the
/// Legacy entry can read active across its three sub-pages.
let private navButtonActive (dispatch : RootMsg -> unit) (label : string) (target : Page) (isActive : bool) : IView =
    Button.create [
        Button.content label
        Button.background (if isActive then Brushes.SteelBlue :> IBrush else Brushes.Transparent :> IBrush)
        Button.onClick (fun _ -> dispatch (RootMsg.Shell (Navigate target)))
    ] :> IView

let private navButton (dispatch : RootMsg -> unit) (label : string) (target : Page) (active : Page) : IView =
    navButtonActive dispatch label target (target = active)

/// Whether a page is part of the legacy dockable screen (D.5) — the Legacy entry itself
/// or its two sub-pages — so the front-door Legacy nav button reads active across them.
let private isLegacyPage (page : Page) : bool =
    match page with
    | Page.Legacy | Page.Construction | Page.SynthesisFit -> true
    | Page.Constructor -> false

/// The front-door navigation (UX commitment 1 / D.6): the new Constructor page (the
/// default landing, marked active with the distinct `navButton` styling, R-11) and the
/// Legacy entry (D.5). Both single-sourced through `Navigate`.
let private frontDoorNav (model : RootModel) (dispatch : RootMsg -> unit) : IView =
    let text key = Localization.lookup model.strings model.env.language key
    StackPanel.create [
        StackPanel.dock Dock.Top
        StackPanel.orientation Orientation.Horizontal
        StackPanel.spacing 6.0
        StackPanel.children [
            navButtonActive dispatch (text "nav.constructor") Page.Constructor (model.page = Page.Constructor)
            navButtonActive dispatch (text "nav.legacy") Page.Legacy (isLegacyPage model.page)
        ]
    ] :> IView

/// The legacy sub-navigation (D.5): the present dockable screen's Construction /
/// Synthesis-Fit pages. `Legacy` lands on Construction. Reuses the existing `navButton`.
let private legacyNavBar (model : RootModel) (dispatch : RootMsg -> unit) : IView =
    let active = match model.page with | Page.SynthesisFit -> Page.SynthesisFit | _ -> Page.Construction
    StackPanel.create [
        StackPanel.dock Dock.Top
        StackPanel.orientation Orientation.Horizontal
        StackPanel.spacing 6.0
        StackPanel.children [
            navButton dispatch ConstructionPage.navEntry.title Page.Construction active
            navButton dispatch SynthesisFitPage.navEntry.title Page.SynthesisFit active
        ]
    ] :> IView

let private panelTitle (panel : string) : string =
    match panel with
    | "stack" -> "Stack"
    | "materials" -> "Materials"
    | "sources" -> "Sources"
    | "chart" -> "Chart"
    | "results" -> "Results"
    | other -> other

/// A titled placeholder for a panel/page whose real content lands in a later part
/// (R-8 — renders one frame headlessly without throwing).
let private placeholder (title : string) : IView =
    TextBlock.create [ TextBlock.text (title + " — coming soon") ] :> IView

let private panelContent (model : RootModel) (dispatch : RootMsg -> unit) (panel : string) : IView =
    match panel with
    | "stack" -> ConstructionView.stackPanel model.construction (RootMsg.Construction >> dispatch)
    | "chart" -> ChartView.chartPanel model.chart model.markers (RootMsg.Chart >> dispatch)
    | "materials" ->
        MaterialsView.materialsPanel
            model.materials model.materialsFilter model.construction
            (RootMsg.Materials >> dispatch) (RootMsg.Construction >> dispatch)
    | "sources" -> SourceView.sourcePanel model.source (RootMsg.Source >> dispatch)
    | "results" ->
        // Part U6 results surface + the Part U8 (R-4) 2-D-orthographic 3-D system view,
        // fed by the construction page's already-solved per-node results.
        StackPanel.create [
            StackPanel.orientation Orientation.Vertical
            StackPanel.spacing 6.0
            StackPanel.children [
                ResultsView.resultsPanel model.workspace model.source (RootMsg.Workspace >> dispatch)
                SystemView3DView.systemPanel model.construction
            ]
        ] :> IView
    | other -> placeholder (panelTitle other)

let private panelView (model : RootModel) (dispatch : RootMsg -> unit) (p : PanelState) : IView =
    let dockAttrs =
        match AppShell.toDock p.dock with
        | Some d -> [ Border.dock d ]
        | None -> []
    Border.create (
        dockAttrs @ [
            Border.borderThickness 1.0
            Border.padding 6.0
            Border.child (panelContent model dispatch p.panel)
        ]) :> IView

/// The Construction page body: the saved, visible panels laid out by their dock
/// edges via a public-Avalonia `DockPanel` (§0.3). Visible set + order from
/// `AppShell.visiblePanels`; dock edges through `AppShell.toDock` (R-4).
let private constructionBody (model : RootModel) (dispatch : RootMsg -> unit) : IView =
    DockPanel.create [
        DockPanel.children [
            for p in AppShell.visiblePanels model.env.layout -> panelView model dispatch p
        ]
    ] :> IView

/// The constructor front-door page (Spec 0026 Part D/F, slice 006): the ribbon shell
/// docked over the slice-005 `ConstructorView` canvas, with the value-id modal and the
/// local-help overlay layered on top when open. The ribbon's shell-level actions (open
/// Legacy / set language / toggle theme) are wired here to `RootMsg.Shell`, so the ribbon
/// itself never references this module (acyclic compile order). Both modal overlays
/// dismiss through the constructor's own messages (the page stays single-sourced).
let private constructorBody (model : RootModel) (dispatch : RootMsg -> unit) : IView =
    let onConstructor = RootMsg.Constructor >> dispatch
    let ribbonDispatch : Rb.Dispatch =
        {
            onRibbon = RootMsg.Ribbon >> dispatch
            onConstructor = onConstructor
            onNavigateLegacy = fun () -> dispatch (RootMsg.Shell (Navigate Page.Legacy))
            onSetLanguage = fun language -> dispatch (RootMsg.Shell (SetLanguage language))
            onToggleTheme = fun () -> dispatch (RootMsg.Shell ToggleTheme)
        }
    // A single-cell Grid stacks the page (ribbon + canvas) under the modal overlays, so
    // an open modal/help dialog covers the page in z-order (a schematic-grade modal).
    Grid.create [
        Grid.children [
            yield DockPanel.create [
                DockPanel.children [
                    Rb.view model.strings model.env.language model.ribbon model.constructor model.env ribbonDispatch
                    ConstructorView.view model.constructor onConstructor
                ]
            ] :> IView
            // F.2: the working, dismissible empty value-id modal, overlaid when open.
            if model.constructor.valueIdModalOpen then
                yield Rb.valueIdModal model.strings model.env.language onConstructor
            // E.2: the context-sensitive local-help overlay, when help is open.
            if model.constructor.helpOpen then
                yield Rb.helpOverlay model.strings model.env.language model.constructor onConstructor
            // E.2 (slice 007): the element-properties placeholder dialog, when open.
            if model.constructor.elementDialogOpen then
                yield Rb.elementDialogOverlay model.strings model.env.language onConstructor
            // E.2 (slice 007): the right-click element context menu, when open.
            if model.constructor.contextMenuOpen then
                yield Rb.contextMenuOverlay model.strings model.env.language model.constructor onConstructor
            // K.2 (slice 007): the same-row Confirm/Cancel destructive gate, when an action is pending.
            if Option.isSome model.constructor.pending then
                yield Rb.confirmGateOverlay model.strings model.env.language model.constructor onConstructor
        ]
    ] :> IView

/// The legacy dockable screen's active sub-page body (D.5): the Construction panels
/// (`constructionBody`) or the Synthesis/Fit page, unchanged from spec 0024.
let private legacyPageBody (model : RootModel) (dispatch : RootMsg -> unit) : IView =
    match model.page with
    | Page.SynthesisFit ->
        match model.fit with
        | Some fit -> FitView.fitPanel fit (RootMsg.Fit >> dispatch)
        | None -> placeholder SynthesisFitPage.navEntry.title
    | _ -> constructionBody model dispatch

/// The present dockable-panel main screen the *Legacy* entry opens (D.5.1): the legacy
/// sub-nav + the lifecycle toolbar + the active legacy sub-page, exactly as before — it
/// remains reachable and functional but is not evolved in lock-step with the new screen.
let private legacyBody (model : RootModel) (dispatch : RootMsg -> unit) : IView =
    DockPanel.create [
        DockPanel.children [
            legacyNavBar model dispatch
            // Part U8 (R-1/R-2): the lifecycle toolbar (New/Open/Save + templates + gallery).
            LifecycleView.lifecycleBar model.status (RootMsg.Io >> dispatch)
            legacyPageBody model dispatch
        ]
    ] :> IView

let private pageBody (model : RootModel) (dispatch : RootMsg -> unit) : IView =
    match model.page with
    | Page.Constructor -> constructorBody model dispatch
    | Page.Legacy | Page.Construction | Page.SynthesisFit -> legacyBody model dispatch

/// The root view (R-4 / D.6): the front-door nav (Constructor + Legacy) docked top, the
/// active page body filling the rest. The lifecycle toolbar now lives inside the legacy
/// screen; the constructor page carries its own ribbon.
let view (model : RootModel) (dispatch : RootMsg -> unit) : IView =
    DockPanel.create [
        DockPanel.children [
            frontDoorNav model dispatch
            pageBody model dispatch
        ]
    ] :> IView
