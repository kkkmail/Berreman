/// Top-level construction / beam-tree page (§B.1, §B.10). Hosts the stack editor
/// and commits the operator-facing UX for destructive structural edits.
///
/// Per §0/P3 the model + `update` are Avalonia-free so the descendant-count
/// gate, per-node busy, results refresh, and single-level undo are unit-tested
/// without a UI. Per-node solving drives the REUSED single-system solver
/// (`OpticalSystemSolver` via `BeamTree.solve`, §B.1) — never a forked copy.
///
/// This module carries NO Avalonia type (P3); the FuncUI view that renders the
/// model is deferred to a later UI-wiring slice.
module OpticalConstructor.Ui.ConstructionPage

open Berreman.Fields
open Berreman.Media
open OpticalConstructor.Domain.BeamTree
open OpticalConstructor.Domain.Project
open OpticalConstructor.Storage
open OpticalConstructor.Storage.Errors

// ---------------------------------------------------------------------------
// Node addressing & tree navigation. A node is addressed by its path from the
// root as a list of branch keys ([] = root). BeamBranch is a comparable DU, so
// NodePath is a valid Map/Set key.
// ---------------------------------------------------------------------------

type NodePath = BeamBranch list

let rec tryGetNode (path : NodePath) (node : BeamNode) : BeamNode option =
    match path with
    | [] -> Some node
    | b :: rest -> node.children |> Map.tryFind b |> Option.bind (tryGetNode rest)

/// Apply `f` to the node at `path`, rebuilding the immutable spine above it.
let rec updateNodeAt (path : NodePath) (f : BeamNode -> BeamNode) (node : BeamNode) : BeamNode =
    match path with
    | [] -> f node
    | b :: rest ->
        match Map.tryFind b node.children with
        | Some child -> { node with children = Map.add b (updateNodeAt rest f child) node.children }
        | None -> node

/// Remove the node at `path` from its parent's children. The root ([]) cannot be
/// removed and is returned unchanged.
let removeNodeAt (path : NodePath) (root : BeamNode) : BeamNode =
    match List.rev path with
    | [] -> root
    | last :: revParent ->
        updateNodeAt (List.rev revParent) (fun parent -> { parent with children = Map.remove last parent.children }) root

/// Live-derived count of the node's descendants (NOT counting the node itself).
/// This is the count surfaced by the pre-confirmation gate (§B.10 item 3) — it
/// is recomputed from the current tree on every request, never cached.
let descendantCount (node : BeamNode) : int =
    let rec count n = n.children |> Map.fold (fun acc _ c -> acc + 1 + count c) 0
    count node

let rec private walk (path : NodePath) (node : BeamNode) : (NodePath * BeamNode) seq =
    seq {
        yield (path, node)
        for KeyValue (b, c) in node.children do
            yield! walk (path @ [ b ]) c
    }

let private subtreePaths (path : NodePath) (root : BeamNode) : Set<NodePath> =
    match tryGetNode path root with
    | Some n -> walk path n |> Seq.map fst |> Set.ofSeq
    | None -> Set.empty

// ---------------------------------------------------------------------------
// Per-node solving via the REUSED engine solver (§B.1 / AC-B1).
// ---------------------------------------------------------------------------

/// Solve a single node through the reused engine solver: `BeamTree.solve`
/// builds `OpticalSystemSolver(node.incident, node.system)` and reads
/// `Solution.emSys` (`Solvers.fs:199,156`) — the `EmFieldSystem` whose
/// `reflected`/`transmitted` feed the node's branches. No forked solver.
/// (`solve` is the opened `OpticalConstructor.Domain.BeamTree.solve`.)
let solveNode (node : BeamNode) : EmFieldSystem = solve node

/// Re-solve the affected sub-tree rooted at `path` (§B.10 item 4): every node in
/// the sub-tree is re-solved through the reused solver, keyed by its path, so the
/// dependent schematic/results views refresh from the new `EmFieldSystem` with no
/// manual reload. Solving one node never blocks the rest of the tree (§B.10 item 2).
let solveSubtree (path : NodePath) (root : BeamNode) : Map<NodePath, EmFieldSystem> =
    match tryGetNode path root with
    | Some node -> walk path node |> Seq.map (fun (p, n) -> p, solveNode n) |> Map.ofSeq
    | None -> Map.empty

// ---------------------------------------------------------------------------
// Navigation entry (§B.10 item 1): a top-level shell entry, reachable without a
// dialog, and the default landing surface for a new/opened project.
// ---------------------------------------------------------------------------

type NavEntry =
    {
        title : string
        isDefaultLanding : bool
    }

let navEntry : NavEntry = { title = "Construction"; isDefaultLanding = true }

// ---------------------------------------------------------------------------
// The page model & messages (Avalonia-free).
// ---------------------------------------------------------------------------

type Model =
    {
        /// The canonical project aggregate (§A.7) this page edits.
        project : OpticalConstructorProject
        /// The currently selected node (the stack editor edits this node's system).
        selected : NodePath
        /// Per-node solve results; dependent views read from here (§B.10 item 4).
        results : Map<NodePath, EmFieldSystem>
        /// Nodes currently re-solving — drives the node-granularity busy indicator
        /// (§B.10 item 2). A single short flat-element solve never sets the whole tree busy.
        busy : Set<NodePath>
        /// A pending sub-tree deletion awaiting confirmation: the path plus the
        /// LIVE descendant count to show in the gate (§B.10 item 3).
        pendingDeletion : (NodePath * int) option
        /// Single-level undo: the immediately prior immutable project snapshot
        /// (§B.10 item 5). Multi-level history is Part I §I.6.
        undo : OpticalConstructorProject option
        /// The project's working folder; the committable JSON file defaults here,
        /// NEVER the repository root (§B.10 item 6).
        workingFolder : string
        projectName : string
    }

type Msg =
    | SelectNode of NodePath
    /// A stack-editor operation on the selected node's system (add/delete/reorder/
    /// duplicate/group/rotate a layer, set a medium, toggle the substrate).
    | EditStack of NodePath * StackEditor.StackMsg
    /// Completion of a sub-tree re-solve; clears busy and refreshes results.
    | NodeSolved of Map<NodePath, EmFieldSystem>
    /// Request to delete a node (§B.10 item 3): a node WITH children opens the
    /// pre-confirmation gate; a childless node is removed directly.
    | RequestDeleteNode of NodePath
    | ConfirmDeleteNode
    | CancelDeleteNode
    /// Opposing attach action (§B.10 item 5): reuses `BeamNode.attach` validation.
    | AttachChild of NodePath * BeamBranch * BeamNode
    | Undo

// ---------------------------------------------------------------------------
// update (pure).
// ---------------------------------------------------------------------------

let private withTree (root : BeamNode) (model : Model) : Model =
    { model with project = { model.project with beamTree = { root = root } } }

/// Apply a system transform to the node at `path`, snapshot undo, and mark the
/// affected sub-tree busy. The caller (or the host loop) then issues the solve
/// and dispatches `NodeSolved`.
let private editNode (path : NodePath) (f : OpticalSystem -> OpticalSystem) (model : Model) : Model =
    let newRoot =
        updateNodeAt path (fun n ->
            let sys' = f n.system
            // Keep a Sample element's embedded system aligned with node.system.
            let element' =
                match n.element with
                | Sample _ -> Sample sys'
                | other -> other
            { n with system = sys'; element = element' }) model.project.beamTree.root
    { (withTree newRoot model) with
        undo = Some model.project
        busy = subtreePaths path newRoot }

let private deleteNodeNow (path : NodePath) (model : Model) : Model =
    let newRoot = removeNodeAt path model.project.beamTree.root
    { (withTree newRoot model) with undo = Some model.project }

let update (msg : Msg) (model : Model) : Model =
    match msg with
    | SelectNode p -> { model with selected = p }

    | EditStack (p, sm) -> editNode p (StackEditor.applyStackMsg sm) model

    | NodeSolved solved ->
        let solvedPaths = solved |> Map.toSeq |> Seq.map fst |> Set.ofSeq
        { model with
            results = solved |> Map.fold (fun acc k v -> Map.add k v acc) model.results
            busy = Set.difference model.busy solvedPaths }

    | RequestDeleteNode p ->
        match tryGetNode p model.project.beamTree.root with
        | Some n when not (List.isEmpty p) && descendantCount n > 0 ->
            // Sub-tree deletion: open the gate with the LIVE descendant count.
            { model with pendingDeletion = Some (p, descendantCount n) }
        | Some _ when not (List.isEmpty p) ->
            // Childless node: no gate required, remove directly.
            deleteNodeNow p model
        | _ -> model   // root or missing path: not deletable here

    | ConfirmDeleteNode ->
        match model.pendingDeletion with
        | Some (p, _) -> { (deleteNodeNow p model) with pendingDeletion = None }
        | None -> model

    | CancelDeleteNode -> { model with pendingDeletion = None }

    | AttachChild (p, branch, child) ->
        match tryGetNode p model.project.beamTree.root with
        | Some parent ->
            match BeamNode.attach branch child parent with
            | Ok parent' ->
                let newRoot = updateNodeAt p (fun _ -> parent') model.project.beamTree.root
                { (withTree newRoot model) with undo = Some model.project }
            | Error _ -> model   // e.g. a mirror rejecting a Transmitted attach (§B.3)
        | None -> model

    | Undo ->
        match model.undo with
        | Some prev -> { model with project = prev; undo = None }
        | None -> model

// ---------------------------------------------------------------------------
// Committable project file (§B.10 item 6) + canonical save (§B.9). Always JSON
// via the slice-003 store; NEVER a derived .binz sidecar from this page.
// ---------------------------------------------------------------------------

/// The committable JSON project path: defaults to the project's working folder,
/// never the repository root, and is a `.json` (never a `.binz` sidecar).
let projectFilePath (model : Model) : string =
    System.IO.Path.Combine(model.workingFolder, model.projectName + ".ocproj.json")

/// Serialize the project to its committable JSON text via the JSON-canonical
/// store (§B.9 — never FsPickler/`.binz`). Returns the `(path, json)` to write.
let saveProject (model : Model) : Result<string * string, StorageError> =
    model.project
    |> ProjectJson.serializeProject
    |> Result.map (fun json -> projectFilePath model, json)

// ---------------------------------------------------------------------------
// init.
// ---------------------------------------------------------------------------

/// Initialise the page for `project`, solving the whole tree once so the results
/// views have an initial `EmFieldSystem` per node.
let init (project : OpticalConstructorProject) (workingFolder : string) (projectName : string) : Model =
    {
        project = project
        selected = []
        results = solveSubtree [] project.beamTree.root
        busy = Set.empty
        pendingDeletion = None
        undo = None
        workingFolder = workingFolder
        projectName = projectName
    }

// ---------------------------------------------------------------------------
// Avalonia-free presentation helpers (§B.10). These compute the strings/flags a
// FuncUI view would render — the busy line, the live confirmation prompt, the
// undo availability — so the page's UX commitments are unit-testable per P3.
// The view itself (binding these to FuncUI controls) is a later UI-wiring slice.
// ---------------------------------------------------------------------------

/// Whether the node at `path` is currently re-solving — node-granularity busy
/// (§B.10 item 2). A node is busy independently of the rest of the tree.
let isNodeBusy (path : NodePath) (model : Model) : bool = Set.contains path model.busy

/// The pre-confirmation prompt for a pending sub-tree deletion (§B.10 item 3),
/// carrying the LIVE descendant count; `None` when no deletion is pending.
let confirmationPrompt (model : Model) : string option =
    model.pendingDeletion
    |> Option.map (fun (_, count) -> sprintf "Delete this node and its %d descendant node(s)?" count)

/// Whether the single-level undo action is available (§B.10 item 5).
let canUndo (model : Model) : bool = Option.isSome model.undo
