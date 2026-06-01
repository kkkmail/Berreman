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

open Avalonia.Controls
open Avalonia.Layout
open Avalonia.Media
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open Elmish
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Project
open OpticalConstructor.Ui.Charts
open OpticalConstructor.Ui.UserEnvironment

/// `Workspace` is both an existing module and a `RootMsg` case name below; the alias
/// keeps the module reachable unambiguously in value position.
module WS = OpticalConstructor.Ui.Workspace

// ---------------------------------------------------------------------------
// Root model, message, and the shell-level sub-message (R-2).
// ---------------------------------------------------------------------------

/// The two top-level pages, matching the two `navEntry` modules (R-2).
type Page =
    | Construction
    | SynthesisFit

/// Shell-level edits the `Shell` `RootMsg` case carries (R-2 — navigation / theme /
/// panel-show / dock). Each routes through an existing pure `AppShell` reducer.
type ShellMsg =
    | Navigate of Page
    | ToggleTheme
    | SetPanelVisible of panel : string * visible : bool
    | SetPanelDock of panel : string * dock : DockSide

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
    }

/// The root message, wrapping each wired sub-`Msg` plus the shell-level case (R-2).
/// U1 wires exactly `Construction`, `Workspace`, and `Shell`; later parts add their
/// own cases (`Fit`, `Source`, `Chart`, `Io`) when they wire them.
type RootMsg =
    | Construction of ConstructionPage.Msg
    | Workspace of WS.Msg
    | Shell of ShellMsg
    | Chart of ChartView.ChartMsg

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

/// Build the root model for the supplied environment (R-3 — the App seeds `env` from
/// the persisted `Startup.settings`). `update` attaches no effects in U1, so `init`
/// carries `Cmd.none`.
let initFrom (env : EnvironmentSettings) : RootModel * Cmd<RootMsg> =
    let model =
        {
            env = env
            page = Page.Construction
            construction = ConstructionPage.init initialProject defaultWorkingFolder "untitled"
            workspace = WS.init initialProject
            fit = None
            chart = ChartSettings.ChartSettings.defaultValue
            markers = Readout.Markers.empty
            materials = MaterialLibrary.standard
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

let private updateShell (msg : ShellMsg) (model : RootModel) : RootModel =
    match msg with
    | Navigate page -> { model with page = page }
    | ToggleTheme -> { model with env = { model.env with theme = AppShell.toggleTheme model.env.theme } }
    | SetPanelVisible (panel, visible) ->
        { model with env = { model.env with layout = AppShell.setPanelVisible panel visible model.env.layout } }
    | SetPanelDock (panel, dock) ->
        { model with env = { model.env with layout = AppShell.dockPanel panel dock model.env.layout } }

let update (msg : RootMsg) (model : RootModel) : RootModel * Cmd<RootMsg> =
    match msg with
    | RootMsg.Construction cm ->
        let construction = ConstructionPage.update cm model.construction
        // §0.4: a busy-marking structural edit (EditStack/AttachChild/ConfirmDeleteNode)
        // attaches the async node-solve Cmd; the pure update already set `busy` and the
        // host re-solves off-thread, marshaling NodeSolved back onto the UI thread.
        let cmd = constructionCmd cm model.construction.pendingDeletion construction.project.beamTree.root
        { model with construction = construction }, cmd
    | RootMsg.Workspace wm -> { model with workspace = WS.update wm model.workspace }, Cmd.none
    | RootMsg.Shell sm -> updateShell sm model, Cmd.none
    | RootMsg.Chart cm ->
        let chart, markers = ChartView.update cm (model.chart, model.markers)
        { model with chart = chart; markers = markers }, Cmd.none

// ---------------------------------------------------------------------------
// view (R-4) — a top-level navigation control over a DockPanel of the visible
// panels. The `stack` id renders real content; the other four render a titled
// placeholder until their part lands (R-8). The visible/ordered panel set and the
// dock edges keep coming from the existing pure `AppShell` reducers.
// ---------------------------------------------------------------------------

let private navButton (dispatch : RootMsg -> unit) (label : string) (target : Page) (active : Page) : IView =
    let isActive = target = active
    Button.create [
        Button.content label
        Button.background (if isActive then Brushes.SteelBlue :> IBrush else Brushes.Transparent :> IBrush)
        Button.onClick (fun _ -> dispatch (RootMsg.Shell (Navigate target)))
    ] :> IView

/// The top navigation bar (UX commitment 1 / AC-U1.1): the two pages with the active
/// one marked by a distinct background.
let private navBar (model : RootModel) (dispatch : RootMsg -> unit) : IView =
    StackPanel.create [
        StackPanel.dock Dock.Top
        StackPanel.orientation Orientation.Horizontal
        StackPanel.spacing 6.0
        StackPanel.children [
            navButton dispatch ConstructionPage.navEntry.title Page.Construction model.page
            navButton dispatch SynthesisFitPage.navEntry.title Page.SynthesisFit model.page
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

let private pageBody (model : RootModel) (dispatch : RootMsg -> unit) : IView =
    match model.page with
    | Page.Construction -> constructionBody model dispatch
    | Page.SynthesisFit -> placeholder SynthesisFitPage.navEntry.title

/// The root view (R-4): navigation docked top, the active page body filling the rest.
let view (model : RootModel) (dispatch : RootMsg -> unit) : IView =
    DockPanel.create [
        DockPanel.children [
            navBar model dispatch
            pageBody model dispatch
        ]
    ] :> IView
