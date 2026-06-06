/// §E (centralized command model, mouse & keyboard) + the FuncUI `Canvas` MVU page that
/// hosts the top-down surface (Spec 0026 Part E + the Part C view interactions, slice
/// 005). The page reads the one `Commands` registry for both its key map and its mouse
/// map (AC-E1 / constraint 0.4) and drives the slice-004 geometry (`ConstructorTable`,
/// `Drawer`) and the slice-001/002 domain (`Placement`, `RayModel`, `Table`).
///
/// The `Model`/`Msg`/`update` are PURE and Avalonia-free (constraint 0.3), exactly like
/// `ConstructionPage`: the model holds the canonical `OpticalConstructorProject` + the
/// ephemeral `Table.TableViewState` + selection + the configured `KeyMap` + transient
/// drag/menu/confirm state — no Avalonia handle, renderer instance, or token (matching
/// `Shell.RootModel`, §0.3). Every acceptance criterion is therefore provable by
/// dispatching messages — the headless harness does NOT fire real pointer/wheel events,
/// so the interactions are proved against `update`, with one `ui-smoke` mount of `view`.
///
/// Because the FuncUI `view` is a pure function of the model, an edit that changes the
/// rays re-renders the table automatically — the redraw-on-drop (E.4.3) and the
/// re-render-on-edit (UX commitment 4) come for free. Rotations reuse the lock-respecting
/// `Placement.withR1/withR2/withR3`; the view reset reuses `Table.resetView`; the snap
/// reuses `RayModel`. No parallel element catalogue, solver, or project type is added
/// (constraint 0.1). Authored against the public MIT `Avalonia.FuncUI` DSL — the
/// audit-gated clone stays UNREFERENCED (§0.3).
module OpticalConstructor.Ui.ConstructorView

open Avalonia
open Avalonia.Controls
open Avalonia.Controls.Shapes
open Avalonia.Layout
open Avalonia.Media
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open Berreman.Constants
open Berreman.Geometry
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Placement
open OpticalConstructor.Domain.Project
open OpticalConstructor.Ui.Commands
open OpticalConstructor.Ui.UserEnvironment

// ---------------------------------------------------------------------------
// Pure model types (no Avalonia handle).
// ---------------------------------------------------------------------------

/// What is currently selected (§C.3 / AC-C3): the table as a whole, or one element by
/// its index into the project's `placements` list. Selecting the table is how the user
/// steps out of an element to operate on the workspace.
type Selection =
    | TableSelected
    | ElementSelected of int

/// A transient pointer drag in progress (§E.4 / §C.2.2). Pure data — no Avalonia handle.
type DragState =
    | NoDrag
    | PanDrag
    | SlideDrag of int
    | ReassignDrag of int

/// The rotation axis a context-menu lock/unlock targets (§E.2 / A.4.5).
type RotationAxis =
    | AxisR1
    | AxisR2
    | AxisR3

/// A pending destructive single action awaiting confirmation (§E.2 / UX commitment 3):
/// reset rotation always confirms; deleting a non-trivial element confirms.
type PendingAction =
    | ConfirmResetRotation of int
    | ConfirmDelete of int

/// The constructor MVU model (§E / §C). Pure and serializable (§0.3): the canonical
/// project, the ephemeral top-down view state, the selection, the configured key map,
/// and transient interaction state — no Avalonia handle.
type Model =
    {
        /// The canonical project aggregate (§A.7) this page edits — the element
        /// `placements` and the persisted `table` plate.
        project : OpticalConstructorProject
        /// The ephemeral top-down view (pan/zoom/screen-rotation) — NOT persisted (C.2.1).
        view : Table.TableViewState
        /// The selected object (§C.3).
        selection : Selection
        /// The configured key map + rotation step (§E.8.1 / §E.3.1), seeded from the env.
        keyMap : KeyMap
        /// Which ray each element index is attached to (ephemeral); default `CentralRay`.
        rayOf : Map<int, RayModel.RayId>
        showCentralRayOnly : bool
        showBoundingBox : bool
        drag : DragState
        /// The movable-element hint (§E.4.4): set when the inert plain-drag fires on an
        /// element, so the real gesture (Shift+drag) is discoverable.
        movableHint : bool
        elementDialogOpen : bool
        contextMenuOpen : bool
        helpOpen : bool
        /// The value-id binding modal (Spec 0026 Part F / F.2). A fully working,
        /// dismissible empty modal — opened from the element's local menu, with no
        /// picker content (the real device/material picker is out of scope, 0.6 / F.2.2).
        valueIdModalOpen : bool
        clipboard : ElementPlacement option
        pending : PendingAction option
        /// Single-level undo/redo (§E.6 binds the commands and dispatches the edits); the
        /// multi-level history + all-action snapshot-push participation is slice 007 (K).
        undo : OpticalConstructorProject option
        redo : OpticalConstructorProject option
        /// Incremented on each Save command (§E.6); the host wires the actual write (006).
        saveRequests : int
        /// The last command the surface invoked (the ribbon/menu of slice 006 reads it).
        lastCommand : Command option
    }

/// Build the page model for an environment + project (the host seeds both; slice 006
/// wires it into the shell). The view starts straight top-down and the table selected.
let init (env : EnvironmentSettings) (project : OpticalConstructorProject) : Model =
    {
        project = project
        view = Table.defaultView
        selection = TableSelected
        keyMap = env.keyMap
        rayOf = Map.empty
        showCentralRayOnly = ConstructorTable.showCentralRayOnlyDefault
        showBoundingBox = Drawer.showBoundingBoxDefault
        drag = NoDrag
        movableHint = false
        elementDialogOpen = false
        contextMenuOpen = false
        helpOpen = false
        valueIdModalOpen = false
        clipboard = None
        pending = None
        undo = None
        redo = None
        saveRequests = 0
        lastCommand = None
    }

// ---------------------------------------------------------------------------
// Pure helpers over the placements list.
// ---------------------------------------------------------------------------

let private placements (model : Model) : ElementPlacement list = model.project.placements

let private placementCount (model : Model) : int = List.length (placements model)

let private tryPlacement (i : int) (model : Model) : ElementPlacement option =
    placements model |> List.tryItem i

/// The active element index, when an element (not the table) is selected.
let activeIndex (model : Model) : int option =
    match model.selection with
    | ElementSelected i when i >= 0 && i < placementCount model -> Some i
    | _ -> None

/// The active element's placement, when an element (not the table) is selected
/// (§D.3): the ribbon's contextual Element tab reads this to appear/label itself, and
/// the local help resolves the active element's catalogue-kind help from it.
let activeElement (model : Model) : ElementPlacement option =
    activeIndex model |> Option.bind (fun i -> tryPlacement i model)

/// The ray an element index is attached to (default: the central ray).
let rayOfIndex (i : int) (model : Model) : RayModel.RayId =
    Map.tryFind i model.rayOf |> Option.defaultValue RayModel.CentralRay

let private withProject (project : OpticalConstructorProject) (model : Model) : Model =
    { model with project = project }

let private setPlacements (ps : ElementPlacement list) (model : Model) : Model =
    withProject { model.project with placements = ps } model

let private setPlacement (i : int) (p : ElementPlacement) (model : Model) : Model =
    setPlacements (placements model |> List.mapi (fun j q -> if j = i then p else q)) model

/// Snapshot the current project for single-level undo, dropping the redo slot (§E.6).
let private snapshot (model : Model) : Model =
    { model with undo = Some model.project; redo = None }

/// Apply a placement transform to the active element with an undo snapshot.
let private editActive (f : ElementPlacement -> ElementPlacement) (model : Model) : Model =
    match activeIndex model with
    | Some i ->
        match tryPlacement i model with
        | Some p -> setPlacement i (f p) (snapshot model)
        | None -> model
    | None -> model

// ---------------------------------------------------------------------------
// Selection / hit-test (§C.3 / AC-C3).
// ---------------------------------------------------------------------------

/// Hit-test a table-frame point against the elements (§C.3): the first element whose
/// placement point is within its face half-extent of the point, else the table. The
/// table becomes selected whenever the click is not on an optical element.
let hitTest (point : TablePoint) (model : Model) : Selection =
    let within (p : ElementPlacement) =
        let dx = (p.placementPoint.x - point.x) / 1.0<meter>
        let dy = (p.placementPoint.y - point.y) / 1.0<meter>
        let r = (p.box.a1 / 2.0) / 1.0<meter>
        sqrt (dx * dx + dy * dy) <= r
    match placements model |> List.tryFindIndex within with
    | Some i -> ElementSelected i
    | None -> TableSelected

// ---------------------------------------------------------------------------
// Central ray, slide bounds, and drag-to-place snapping (§E.4 / §E.7).
// ---------------------------------------------------------------------------

/// The central-ray endpoints (B.4): an explicit source/detector placement if present,
/// otherwise the `RayModel` defaults (source at x = -1 m, detector at x = +1 m).
let private centralRayEndpoints (model : Model) : TablePoint * TablePoint =
    let pointOf kind =
        placements model
        |> List.tryFind (fun p -> p.catalogueKind = kind)
        |> Option.map (fun p -> p.placementPoint)
    let source = pointOf LightSource |> Option.defaultValue RayModel.defaultSourcePoint
    let detector = pointOf Detector |> Option.defaultValue RayModel.defaultDetectorPoint
    (source, detector)

/// The middle of the closest central-ray path (§E.7.1): the midpoint of the CR. A
/// dropped element snaps here so it lands attached to the nearest beam, not floating.
let centralRayMiddle (model : Model) : TablePoint =
    let (s, d) = centralRayEndpoints model
    { x = (s.x + d.x) / 2.0; y = (s.y + d.y) / 2.0 }

/// The slide bounds for the active element along its ray (§E.4.1): the x-coordinates of
/// its two immediate neighbours on the SAME ray, falling back to the table edges. The
/// slide clamps to `[lower, upper]` and never passes a neighbour.
let private slideBounds (i : int) (model : Model) : float<meter> * float<meter> =
    let ray = rayOfIndex i model
    let xi = match tryPlacement i model with | Some p -> p.placementPoint.x | None -> 0.0<meter>
    let neighbourXs =
        placements model
        |> List.mapi (fun j p -> j, p)
        |> List.filter (fun (j, _) -> j <> i && rayOfIndex j model = ray)
        |> List.map (fun (_, p) -> p.placementPoint.x)
    let halfL = model.project.table.length / 2.0
    let lower =
        match neighbourXs |> List.filter (fun x -> x <= xi) with
        | [] -> -halfL
        | below -> List.max below
    let upper =
        match neighbourXs |> List.filter (fun x -> x >= xi) with
        | [] -> halfL
        | above -> List.min above
    (lower, upper)

/// Slide the active element to a target x along its ray, clamped to its neighbour bounds
/// (§E.4.1). The y stays on the ray (left/right slides are the same constrained move).
let private slideActiveToX (targetX : float<meter>) (model : Model) : Model =
    match activeIndex model with
    | Some i ->
        match tryPlacement i model with
        | Some p ->
            let (lo, hi) = slideBounds i model
            let clamped = max lo (min hi targetX)
            setPlacement i { p with placementPoint = { p.placementPoint with x = clamped } } model
        | None -> model
    | None -> model

// ---------------------------------------------------------------------------
// Element edits driving the commands.
// ---------------------------------------------------------------------------

/// Rotate the active element about an axis by `notches` steps (§E.3): the step is the
/// configured `rotationStepDegrees` (default 5°). Reuses the lock-respecting setters, so
/// a locked axis is inert and R3 (locked by default) requires unlocking first (AC-E2).
let private rotateActive (axis : RotationAxis) (notches : int) (model : Model) : Model =
    let step = Angle.degree (float notches * model.keyMap.rotationStepDegrees)
    editActive (fun p ->
        match axis with
        | AxisR1 -> withR1 (p.r1 + step) p
        | AxisR2 -> withR2 (p.r2 + step) p
        | AxisR3 -> withR3 (p.r3 + step) p) model

/// Lock/unlock a rotation axis of the active element (§E.2 — a context-menu element edit).
let private toggleLock (axis : RotationAxis) (model : Model) : Model =
    editActive (fun p ->
        match axis with
        | AxisR1 -> setR1Locked (not p.r1Locked) p
        | AxisR2 -> setR2Locked (not p.r2Locked) p
        | AxisR3 -> setR3Locked (not p.r3Locked) p) model

/// Toggle the active element's reflected/transmitted emission (§E.2). Reuses the
/// `Emission` smart setters, so the both-off state stays unrepresentable.
let private toggleEmission (branch : BeamTree.BeamBranch) (model : Model) : Model =
    editActive (fun p ->
        match branch with
        | BeamTree.Reflected -> { p with emission = Emission.withReflected (not p.emission.emitsReflected) p.emission }
        | BeamTree.Transmitted -> { p with emission = Emission.withTransmitted (not p.emission.emitsTransmitted) p.emission }) model

/// Reassign the active element to a different ray (§E.4.2): cycle CR -> reflected ->
/// transmitted -> CR. The only gesture that moves an element between rays.
let private nextRay (r : RayModel.RayId) : RayModel.RayId =
    match r with
    | RayModel.CentralRay -> RayModel.ReflectedBranch 0
    | RayModel.ReflectedBranch _ -> RayModel.TransmittedBranch 0
    | RayModel.TransmittedBranch _ -> RayModel.CentralRay

let private reassignActive (model : Model) : Model =
    match activeIndex model with
    | Some i -> { model with rayOf = Map.add i (nextRay (rayOfIndex i model)) model.rayOf }
    | None -> model

/// Whether a delete must confirm (§E.2 — "non-trivial element"): a rotated element or one
/// already bound to a device/material value. A trivial fresh element deletes directly.
let private isNonTrivial (p : ElementPlacement) : bool =
    p.r1.value <> 0.0 || p.r2.value <> 0.0 || p.r3.value <> 0.0 || Option.isSome p.valueId

let private deleteNow (i : int) (model : Model) : Model =
    let ps = placements model |> List.mapi (fun j p -> j, p) |> List.filter (fun (j, _) -> j <> i) |> List.map snd
    // Re-key rayOf around the removed index (indices above i shift down by one).
    let rayOf' =
        model.rayOf
        |> Map.toList
        |> List.choose (fun (j, r) -> if j = i then None elif j > i then Some (j - 1, r) else Some (j, r))
        |> Map.ofList
    { (snapshot model) with
        project = { model.project with placements = ps }
        rayOf = rayOf'
        selection = TableSelected }

let private resetRotationNow (i : int) (model : Model) : Model =
    match tryPlacement i model with
    | Some p -> setPlacement i { p with r1 = Angle.zero; r2 = Angle.zero; r3 = Angle.zero } (snapshot model)
    | None -> model

let private appendPlacement (p : ElementPlacement) (model : Model) : Model =
    let ps = placements model @ [ p ]
    { (snapshot model) with
        project = { model.project with placements = ps }
        selection = ElementSelected (List.length ps - 1) }

/// A small offset for a duplicated/pasted element so it does not land exactly on its source.
let private offset (p : ElementPlacement) : ElementPlacement =
    { p with placementPoint = { x = p.placementPoint.x + 0.05<meter>; y = p.placementPoint.y + 0.05<meter> } }

/// The element indices ordered along the beam path (by x), for Tab/Shift+Tab cycling.
let private orderedIndices (model : Model) : int list =
    placements model
    |> List.mapi (fun i p -> i, p.placementPoint.x)
    |> List.sortBy (fun (_, x) -> x / 1.0<meter>)
    |> List.map fst

/// Make the next/previous element active along the beam path (§E.2 — Tab / Shift+Tab).
let private cycleActive (forward : bool) (model : Model) : Model =
    match orderedIndices model with
    | [] -> model
    | order ->
        let n = List.length order
        let cur =
            match activeIndex model with
            | Some i -> List.tryFindIndex ((=) i) order |> Option.defaultValue -1
            | None -> -1
        let pos =
            if cur < 0 then (if forward then 0 else n - 1)
            elif forward then (cur + 1) % n
            else (cur - 1 + n) % n
        { model with selection = ElementSelected (List.item pos order) }

// ---------------------------------------------------------------------------
// View commands (§C.2 / §E.5).
// ---------------------------------------------------------------------------

let private panBy (dx : float) (dy : float) (model : Model) : Model =
    { model with view = { model.view with panX = model.view.panX + dx; panY = model.view.panY + dy } }

/// The zoom multiplier per wheel notch (C.2.3).
let zoomStep : float = 1.1

let private zoomBy (notches : int) (model : Model) : Model =
    let z = max 0.1 (min 20.0 (model.view.zoom * (zoomStep ** float notches)))
    { model with view = { model.view with zoom = z } }

// ---------------------------------------------------------------------------
// The command applier (§E) — the parameterless commands reachable from a key, the
// ribbon, or the context menu. Gesture-driven commands that need event context
// (rotations, slide, zoom, pan, ribbon drop) are handled by their own messages.
// ---------------------------------------------------------------------------

let private applyCommand (cmd : Command) (model : Model) : Model =
    let m = { model with lastCommand = Some cmd }
    match cmd with
    | OpenElementDialog -> { m with elementDialogOpen = true }
    | ElementContextMenu -> { m with contextMenuOpen = true }
    | ResetRotation ->
        match activeIndex m with
        | Some i -> { m with pending = Some (ConfirmResetRotation i) }
        | None -> m
    | DeleteElement ->
        match activeIndex m with
        | Some i ->
            match tryPlacement i m with
            | Some p when isNonTrivial p -> { m with pending = Some (ConfirmDelete i) }
            | Some _ -> deleteNow i m
            | None -> m
        | None -> m
    | DuplicateElement ->
        match activeIndex m with
        | Some i -> (match tryPlacement i m with | Some p -> appendPlacement (offset p) m | None -> m)
        | None -> m
    | CopyElement ->
        match activeIndex m with
        | Some i -> { m with clipboard = tryPlacement i m }
        | None -> m
    | PasteElement ->
        match m.clipboard with
        | Some p -> appendPlacement (offset p) m
        | None -> m
    | LocalHelp -> { m with helpOpen = true }
    | NextElement -> cycleActive true m
    | PreviousElement -> cycleActive false m
    | ResetView -> { m with view = Table.resetView m.view }
    | ToggleGroup -> m   // the number-key group-toggle SHAPE only; the behaviour is slice 007 (Part G)
    | Undo ->
        match m.undo with
        | Some prev -> { m with project = prev; undo = None; redo = Some m.project }
        | None -> m
    | Redo ->
        match m.redo with
        | Some next -> { m with project = next; redo = None; undo = Some m.project }
        | None -> m
    | SaveProject -> { m with saveRequests = m.saveRequests + 1 }
    | CancelOrDeselect ->
        { m with
            selection = TableSelected
            contextMenuOpen = false
            elementDialogOpen = false
            helpOpen = false
            valueIdModalOpen = false
            pending = None
            drag = NoDrag
            movableHint = false }
    // Gesture-driven commands handled by their own messages (not invokable parameterlessly):
    | RotateR1 | RotateR2 | RotateR3 | SlideAlongRay | MoveToRay
    | PanView | ZoomView | PlaceFromRibbon -> m

/// The ONE source of truth for the commands a ribbon/menu button MUST render DISABLED:
/// the commands with NO visible front-door effect on the constructor page. Two groups, one
/// list — the ribbon reads it through `isParameterlessInvokable`, and the `RibbonTests`
/// disabled-set derives from it, so there is no second hand-maintained copy to drift:
///
///   * the nine GESTURE-ONLY commands (`RotateR1/R2/R3`, `SlideAlongRay`, `MoveToRay`,
///     `PanView`, `ZoomView`, `PlaceFromRibbon`, and the slice-007 `ToggleGroup`
///     placeholder) — `applyCommand` returns the model UNCHANGED for these because they
///     need the event context (a wheel notch, a drag delta, a ribbon-drop kind/point) a
///     parameterless `Invoke` cannot supply, so a button that merely `Invoke`s them no-ops;
///   * the four ELEMENT-EDIT commands (`OpenElementDialog`, `ElementContextMenu`,
///     `ResetRotation`, `DeleteElement`) — these DO mutate the model in `applyCommand` (a
///     flag, or an armed `pending`), but no overlay renders their surface on the constructor
///     front door yet (slice 007 wires the context-menu / dialog / confirm overlays), so an
///     enabled button sets state the user can neither see nor clear.
///
/// This list is therefore DELIBERATELY BROADER than `applyCommand`'s inert-return arms: the
/// seam is "no rendered front-door surface," NOT model-equality (the four element-edit
/// commands change the model, so a model-equality test would wrongly call them invokable).
/// When slice 007 renders an overlay for one of the four, drop it from this list.
let commandsWithoutFrontDoorSurface : Command list =
    [ // gesture-only — inert in `applyCommand` (need event context)
      RotateR1; RotateR2; RotateR3; SlideAlongRay; MoveToRay
      PanView; ZoomView; PlaceFromRibbon; ToggleGroup
      // element-edit — mutate the model, but no rendered front-door surface until slice 007
      OpenElementDialog; ElementContextMenu; ResetRotation; DeleteElement ]

/// Whether a command has a visible effect when invoked PARAMETERLESSLY from a ribbon/menu
/// button — i.e. it is NOT one of `commandsWithoutFrontDoorSurface`. The ribbon's
/// `commandButton` reads this to render the surface-less commands DISABLED rather than as
/// clickable controls that silently change nothing the user can see. Every other registry
/// command is invokable here.
let isParameterlessInvokable (cmd : Command) : bool =
    not (List.contains cmd commandsWithoutFrontDoorSurface)

/// Slide the active element via the arrow keys (§E.4.1): the base step, or a larger step
/// with Shift. Left/Down slide negative, Right/Up positive — both clamped to the bounds.
let private slideKeyStep : float<meter> = 0.02<meter>
let private slideKeyStepLarge : float<meter> = 0.10<meter>

let private arrowSlide (g : KeyGesture) (model : Model) : Model =
    match activeIndex model with
    | Some i ->
        match tryPlacement i model with
        | Some p ->
            let mag = if g.modifiers.Contains Shift then slideKeyStepLarge else slideKeyStep
            let dir =
                match g.key with
                | ArrowRight | ArrowUp -> 1.0
                | ArrowLeft | ArrowDown -> -1.0
                | _ -> 0.0
            slideActiveToX (p.placementPoint.x + dir * mag) (snapshot model)
        | None -> model
    | None -> model

let private overridePairs (model : Model) : (string * string) list =
    model.keyMap.overrides |> List.map (fun (o : KeyBindingOverride) -> o.command, o.gesture)

// ---------------------------------------------------------------------------
// Message + update (pure, Avalonia-free).
// ---------------------------------------------------------------------------

/// The constructor page messages (§E / §C). All carry pure data (table-frame points,
/// modifiers, key gestures) — the Avalonia `view` translates raw pointer/wheel/key
/// events into these via the `Commands` registry.
type Msg =
    /// A plain click → select the table or an element by hit-test (§C.3 / AC-C3).
    | SelectAt of TablePoint
    /// Double-click → open the active element's dialog (§E.2).
    | OpenDialogAt of TablePoint
    /// Right-click → open the element context menu (§E.2).
    | ContextMenuAt of TablePoint
    /// Mouse wheel: `notches` is +1 up / -1 down (AC-C2 zoom / AC-E2 rotate).
    | WheelAt of Modifier list * int * TablePoint
    /// Begin a drag: classify pan / slide / reassign / inert from the modifiers + hit (§E.4 / §C.2.2).
    | BeginDrag of Modifier list * TablePoint
    /// Pan the view by a screen delta (only acts during a pan drag, §C.2.2).
    | PanByScreen of float * float
    /// Slide the active element to a table point (only acts during a slide drag, §E.4.1).
    | SlideTo of TablePoint
    /// End the current drag — finalize a reassign, redraw on drop (§E.4.3).
    | EndDrag
    /// A key gesture, resolved against the CONFIGURED key map (§E.8.1).
    | KeyPress of KeyGesture
    /// Drop an element dragged from the Build ribbon, snapped to the CR middle (§E.7 / AC-E4).
    | RibbonDrop of CatalogueKind * TablePoint
    /// Invoke a command directly (the ribbon / context menu of slice 006 dispatch here).
    | Invoke of Command
    /// Context-menu element edits (§E.2): lock/unlock a rotation, toggle reflected/transmitted.
    | MenuToggleLock of RotationAxis
    | MenuToggleEmission of BeamTree.BeamBranch
    /// Open / dismiss the value-id binding modal (§F.2): the element's local-menu action
    /// opens a fully working, dismissible empty modal (no picker content ships, 0.6).
    | OpenValueIdModal
    | CloseValueIdModal
    /// Dismiss the local-help overlay WITHOUT touching the selection (mirrors
    /// `CloseValueIdModal`): closing context help must not deselect the active element /
    /// collapse the contextual Element tab — which `Invoke CancelOrDeselect` would do.
    | CloseHelp
    /// Apply / cancel a pending destructive single action (§E.2).
    | ConfirmPending
    | CancelPending

let update (msg : Msg) (model : Model) : Model =
    match msg with
    | SelectAt point ->
        { model with selection = hitTest point model; movableHint = false; contextMenuOpen = false }

    | OpenDialogAt point ->
        match hitTest point model with
        | ElementSelected _ as sel -> applyCommand OpenElementDialog { model with selection = sel }
        | TableSelected -> { model with selection = TableSelected }

    | ContextMenuAt point ->
        match hitTest point model with
        | ElementSelected _ as sel -> applyCommand ElementContextMenu { model with selection = sel }
        | TableSelected -> { model with selection = TableSelected }

    | WheelAt (mods, notches, _point) ->
        match lookupMouse (WheelGesture (Set.ofList mods)) with
        | Some ZoomView -> zoomBy notches model
        | Some RotateR1 -> rotateActive AxisR1 notches model
        | Some RotateR2 -> rotateActive AxisR2 notches model
        | Some RotateR3 -> rotateActive AxisR3 notches model
        | _ -> model

    | BeginDrag (mods, point) ->
        let hit = hitTest point model
        match lookupMouse (DragGesture (LeftButton, Set.ofList mods)) with
        | Some PanView ->
            match hit with
            | TableSelected -> { model with drag = PanDrag; selection = TableSelected; movableHint = false }
            // Plain left-drag on an element does NOTHING — show the movable hint (§E.4.4).
            | ElementSelected i -> { model with drag = NoDrag; selection = ElementSelected i; movableHint = true }
        | Some SlideAlongRay ->
            match hit with
            | ElementSelected i -> { (snapshot model) with drag = SlideDrag i; selection = ElementSelected i }
            | TableSelected -> model
        | Some MoveToRay ->
            match hit with
            | ElementSelected i -> { model with drag = ReassignDrag i; selection = ElementSelected i }
            | TableSelected -> model
        | _ -> model

    | PanByScreen (dx, dy) ->
        match model.drag with
        | PanDrag -> panBy dx dy model
        | _ -> model

    | SlideTo point ->
        match model.drag with
        | SlideDrag _ -> slideActiveToX point.x model
        | _ -> model

    | EndDrag ->
        let model' =
            match model.drag with
            | ReassignDrag _ -> reassignActive model
            | _ -> model
        { model' with drag = NoDrag }

    | KeyPress g ->
        match resolveKey (overridePairs model) g with
        | Some SlideAlongRay -> arrowSlide g model
        | Some cmd -> applyCommand cmd model
        | None -> model

    | RibbonDrop (kind, _point) ->
        appendPlacement (ElementPlacement.create kind (centralRayMiddle model)) model

    | Invoke cmd -> applyCommand cmd model

    | MenuToggleLock axis -> toggleLock axis model

    | MenuToggleEmission branch -> toggleEmission branch model

    | OpenValueIdModal -> { model with valueIdModalOpen = true; lastCommand = None }

    | CloseValueIdModal -> { model with valueIdModalOpen = false }

    | CloseHelp -> { model with helpOpen = false }

    | ConfirmPending ->
        match model.pending with
        | Some (ConfirmResetRotation i) -> { (resetRotationNow i model) with pending = None }
        | Some (ConfirmDelete i) -> { (deleteNow i model) with pending = None }
        | None -> model

    | CancelPending -> { model with pending = None }

// ---------------------------------------------------------------------------
// Avalonia-free presentation helpers (the menu/ribbon of slice 006 read these).
// ---------------------------------------------------------------------------

/// The element context-menu contents (§E.2): the registry commands flagged
/// `inContextMenu` (Element dialog, Local help, Reset rotation, Delete, Duplicate)
/// — projected from the ONE registry — surfaced when an element is active.
let contextMenuCommands : Command list = Commands.contextMenuCommands

/// The confirmation prompt for a pending destructive action (§E.2), or `None`.
let confirmationPrompt (model : Model) : string option =
    match model.pending with
    | Some (ConfirmResetRotation _) -> Some "Reset this element's rotation to (0, 0, 0)?"
    | Some (ConfirmDelete _) -> Some "Delete this element?"
    | None -> None

// ---------------------------------------------------------------------------
// The FuncUI Canvas view + event translation (Avalonia boundary). The geometry is the
// pure slice-004 projection; the handlers translate raw events into the pure messages
// through the `Commands` registry. Headless pointer/wheel events do not fire, so this is
// exercised by the `ui-smoke` mount only; the interaction logic is proved on `update`.
// ---------------------------------------------------------------------------

[<Literal>]
let private canvasWidth = 760.0
[<Literal>]
let private canvasHeight = 480.0
let private centerX = canvasWidth / 2.0
let private centerY = canvasHeight / 2.0

/// Host-layer scratch for the last pointer position (pan/slide deltas), kept OUT of the
/// pure model (matching the `Shell.fs` module-level host fields, §0.3).
let mutable private lastPointer : Point option = None

let private toBrush (c : Schematic.SchematicColor) : IBrush =
    SolidColorBrush(Color.FromRgb(c.red, c.green, c.blue)) :> IBrush

let private toBrushA (opacity : float) (c : Schematic.SchematicColor) : IBrush =
    SolidColorBrush(Color.FromArgb(byte (255.0 * max 0.0 (min 1.0 opacity)), c.red, c.green, c.blue)) :> IBrush

/// In-plane screen rotation of a vector by `a` (§C.2.4 — the view's R1 is measured
/// relative to the screen). The 2-D top-down surface realises R1 as an in-plane spin
/// about the canvas centre, so the whole table — plate, elements, rays — rotates
/// together (all geometry goes through `projectToCanvas`). R2/R3 (out-of-plane tilt)
/// keep the top-down approximation, consistent with the schematic-not-physical mandate
/// (constraint 0.5) and the slice-004 top-down `project`.
let private rotateInPlane (a : Angle) (px : float, py : float) : float * float =
    let c = cos a.value
    let s = sin a.value
    (px * c - py * s, px * s + py * c)

/// Project a table-frame point to canvas coordinates (a pure float pair — no Avalonia):
/// the slice-004 transform (scale + Y-flip + pan), the view's in-plane R1 rotation about
/// the canvas centre (so elements travel with the table, C.2.4), then the centre offset.
let projectToCanvas (view : Table.TableViewState) (p : TablePoint) : float * float =
    let d = ConstructorTable.project view p
    let (rx, ry) = rotateInPlane view.r1 (d.dx, d.dy)
    (centerX + rx, centerY + ry)

/// Project a table-frame point to an Avalonia canvas point.
let private toScreen (view : Table.TableViewState) (p : TablePoint) : Point =
    let (x, y) = projectToCanvas view p
    Point(x, y)

/// The inverse of `projectToCanvas` — a canvas point back to a table-frame point (the R1
/// view rotation is undone first, then the pan/zoom/Y-flip).
let private fromScreen (view : Table.TableViewState) (sx : float) (sy : float) : TablePoint =
    let s = ConstructorTable.basePixelsPerMeter * view.zoom
    let (dx, dy) = rotateInPlane (- view.r1) (sx - centerX, sy - centerY)
    { x = ((dx - view.panX) / s) * 1.0<meter>
      y = ((view.panY - dy) / s) * 1.0<meter> }

let private plateView (model : Model) : IView =
    let corners =
        ConstructorTable.plateCorners model.project.table model.view
        |> List.map (fun d -> let (rx, ry) = rotateInPlane model.view.r1 (d.dx, d.dy) in (centerX + rx, centerY + ry))
    let xs = corners |> List.map fst
    let ys = corners |> List.map snd
    let left = List.min xs
    let top = List.min ys
    Rectangle.create [
        Rectangle.left left
        Rectangle.top top
        Rectangle.width (List.max xs - left)
        Rectangle.height (List.max ys - top)
        Rectangle.fill (toBrush ConstructorTable.tablePlateColor)
        Rectangle.stroke (toBrush ConstructorTable.elementFrameColor)
        Rectangle.strokeThickness 1.0
    ] :> IView

let private centralRayView (model : Model) : IView =
    let (s, d) = centralRayEndpoints model
    let p0 = toScreen model.view s
    let p1 = toScreen model.view d
    let stroke = ConstructorTable.rayStroke RayModel.Incident true
    Line.create [
        Line.startPoint p0
        Line.endPoint p1
        Line.stroke (toBrush stroke.color)
        Line.strokeThickness stroke.weightPx
    ] :> IView

let private elementView (model : Model) (p : ElementPlacement) : IView =
    let center = toScreen model.view p.placementPoint
    let s = ConstructorTable.basePixelsPerMeter * model.view.zoom
    let r = ((p.box.a1 / 2.0) / 1.0<meter>) * s
    let fill = Drawer.shadeFor p.catalogueKind
    Ellipse.create [
        Ellipse.left (center.X - r)
        Ellipse.top (center.Y - r)
        Ellipse.width (2.0 * r)
        Ellipse.height (2.0 * r)
        Ellipse.fill (toBrushA fill.opacity fill.color)
        Ellipse.stroke (toBrush ConstructorTable.elementFrameColor)
        Ellipse.strokeThickness ConstructorTable.elementFrameWeightPx
    ] :> IView

let private indicatorView (model : Model) : IView list =
    match activeIndex model with
    | Some i ->
        match tryPlacement i model with
        | Some p ->
            let center = toScreen model.view p.placementPoint
            let s = ConstructorTable.basePixelsPerMeter * model.view.zoom
            let r = ((p.box.a1 / 2.0) / 1.0<meter>) * s + 6.0
            [ Ellipse.create [
                  Ellipse.left (center.X - r)
                  Ellipse.top (center.Y - r)
                  Ellipse.width (2.0 * r)
                  Ellipse.height (2.0 * r)
                  Ellipse.fill (Brushes.Transparent :> IBrush)
                  Ellipse.stroke (toBrush ConstructorTable.activeIndicatorColor)
                  Ellipse.strokeThickness ConstructorTable.activeIndicatorWeightPx
              ] :> IView ]
        | None -> []
    | None -> []

let private modifiersOf (km : Avalonia.Input.KeyModifiers) : Modifier list =
    [ if km.HasFlag Avalonia.Input.KeyModifiers.Control then Ctrl
      if km.HasFlag Avalonia.Input.KeyModifiers.Shift then Shift
      if km.HasFlag Avalonia.Input.KeyModifiers.Alt then Alt ]

/// Map an Avalonia key to a `Commands.Key`, `None` for keys the constructor does not bind.
let private keyOf (k : Avalonia.Input.Key) : Key option =
    let ki = int k
    if ki >= int Avalonia.Input.Key.A && ki <= int Avalonia.Input.Key.Z then
        Some (Letter (char (int 'A' + (ki - int Avalonia.Input.Key.A))))
    elif ki >= int Avalonia.Input.Key.D0 && ki <= int Avalonia.Input.Key.D9 then
        Some (Digit (ki - int Avalonia.Input.Key.D0))
    else
        match k with
        | Avalonia.Input.Key.Left -> Some ArrowLeft
        | Avalonia.Input.Key.Right -> Some ArrowRight
        | Avalonia.Input.Key.Up -> Some ArrowUp
        | Avalonia.Input.Key.Down -> Some ArrowDown
        | Avalonia.Input.Key.Tab -> Some Tab
        | Avalonia.Input.Key.Delete -> Some Delete
        | Avalonia.Input.Key.Back -> Some Backspace
        | Avalonia.Input.Key.Escape -> Some Escape
        | Avalonia.Input.Key.F1 -> Some F1
        | _ -> None

/// The constructor `Canvas` MVU page (§E / §C). Renders the grey plate, the central ray,
/// each element drawer, and the active-element indicator, and translates pointer/wheel/key
/// events into the pure messages through the one `Commands` registry. The view is a pure
/// function of the model, so any edit re-renders the table (redraw-on-drop, E.4.3).
let view (model : Model) (dispatch : Msg -> unit) : IView =
    let children =
        [ plateView model; centralRayView model ]
        @ (placements model |> List.map (elementView model))
        @ indicatorView model
    let canvas =
        Canvas.create [
            Canvas.width canvasWidth
            Canvas.height canvasHeight
            Canvas.children children
        ]
    Border.create [
        Border.width canvasWidth
        Border.height canvasHeight
        Border.focusable true
        Border.background (Brushes.White :> IBrush)
        Border.child canvas
        Border.onPointerPressed (fun e ->
            let pt = e.GetPosition null
            let tp = fromScreen model.view pt.X pt.Y
            let mods = modifiersOf e.KeyModifiers
            lastPointer <- Some pt
            if e.GetCurrentPoint(null).Properties.IsRightButtonPressed then dispatch (ContextMenuAt tp)
            elif e.ClickCount >= 2 then dispatch (OpenDialogAt tp)
            else dispatch (BeginDrag (mods, tp)))
        Border.onPointerMoved (fun e ->
            let pt = e.GetPosition null
            match lastPointer with
            | Some last ->
                dispatch (PanByScreen (pt.X - last.X, pt.Y - last.Y))
                dispatch (SlideTo (fromScreen model.view pt.X pt.Y))
                lastPointer <- Some pt
            | None -> ())
        Border.onPointerReleased (fun _ ->
            lastPointer <- None
            dispatch EndDrag)
        Border.onPointerWheelChanged (fun e ->
            let pt = e.GetPosition null
            let notches = if e.Delta.Y >= 0.0 then 1 else -1
            dispatch (WheelAt (modifiersOf e.KeyModifiers, notches, fromScreen model.view pt.X pt.Y)))
        Border.onKeyDown (fun e ->
            match keyOf e.Key with
            | Some k -> dispatch (KeyPress { key = k; modifiers = Set.ofList (modifiersOf e.KeyModifiers) })
            | None -> ())
    ] :> IView
