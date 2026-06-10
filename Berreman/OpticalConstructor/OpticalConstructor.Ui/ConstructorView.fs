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

open System.Collections.Concurrent
open System.Globalization
open Avalonia
open Avalonia.Controls
open Avalonia.Controls.Shapes
open Avalonia.Input
open Avalonia.Layout
open Avalonia.Media
open Avalonia.Media.Imaging
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open Berreman.Constants
open Berreman.Geometry
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Placement
open OpticalConstructor.Domain.Project
open OpticalConstructor.Storage
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

/// A catalogue element picked from Build and waiting for its table click. The preview
/// point appears only after the pointer moves over the table; once present, it is the
/// centre of the element bounding box while the user is choosing the drop point.
type PlacementDraft =
    {
        kind : CatalogueKind
        previewPoint : TablePoint option
    }

/// The rotation axis a context-menu lock/unlock targets (§E.2 / A.4.5).
type RotationAxis =
    | AxisR1
    | AxisR2
    | AxisR3

/// A pending destructive single action awaiting confirmation (§E.2 / UX commitment 3 / K.2):
/// reset rotation always confirms; deleting a non-trivial element confirms; resetting the view
/// confirms (it is ephemeral view state, NOT captured by the project-snapshot `EditHistory`, so
/// K.2 requires it be confirmation-gated rather than undoable).
type PendingAction =
    | ConfirmResetRotation of int
    | ConfirmDelete of int
    | ConfirmResetView

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
        placementDraft : PlacementDraft option
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
        /// Multi-level undo/redo over WHOLE-PROJECT snapshots, reusing the shared
        /// `History.EditHistory` (History.fs:20 / Part K, R-8 — no parallel history). Every
        /// project-mutating edit `commit`s the new project onto it (K.1.1); `Undo`/`Redo` step
        /// `History.undo`/`redo`, so they are multi-level (K.1.2). INVARIANT: `history.present`
        /// equals `project` between edits — `commit` re-establishes it after each edit.
        history : History.EditHistory
        /// The element groups currently in the workspace (Part G / G.1). Each group holds its
        /// members' FULL configurations; toggling a member on/off (or swapping) flips only which
        /// members are in the beam — reflected into `project.placements` so the change is a
        /// snapshot edit (K.1) — never their stored configuration (AC-G1). Loaded from /
        /// persisted to the separate app-data library (`GroupsLibrary`, G.3).
        groups : Groups.ElementGroup list
        /// Incremented on each Save command (§E.6); the host wires the actual write (006).
        saveRequests : int
        /// The last command the surface invoked (the ribbon/menu of slice 006 reads it).
        lastCommand : Command option
    }

let private placementExists (placement : ElementPlacement) (project : OpticalConstructorProject) : bool =
    project.placements |> List.exists ((=) placement)

let private syncGroupsWithProject (project : OpticalConstructorProject) (groups : Groups.ElementGroup list) : Groups.ElementGroup list =
    groups
    |> List.map (fun g ->
        { g with
            members =
                g.members
                |> List.map (fun m -> { m with inBeam = placementExists m.placement project }) })

/// Build the page model for an environment + project + reusable group library. The view
/// starts straight top-down and the table selected; group `inBeam` flags are aligned to
/// this project so the Experiment tab cannot show phantom members after a project load,
/// undo, or legacy-page edit.
let initWithGroups
    (env : EnvironmentSettings)
    (project : OpticalConstructorProject)
    (groups : Groups.ElementGroup list)
    : Model =
    {
        project = project
        view = Table.defaultView
        selection = TableSelected
        keyMap = env.keyMap
        rayOf = Map.empty
        showCentralRayOnly = ConstructorTable.showCentralRayOnlyDefault
        showBoundingBox = Drawer.showBoundingBoxDefault
        placementDraft = None
        drag = NoDrag
        movableHint = false
        elementDialogOpen = false
        contextMenuOpen = false
        helpOpen = false
        valueIdModalOpen = false
        clipboard = None
        pending = None
        history = History.ofPresent project
        groups = syncGroupsWithProject project groups
        saveRequests = 0
        lastCommand = None
    }

/// Build the page model for an environment + project (the host seeds both; slice 006
/// wires it into the shell). The view starts straight top-down and the table selected.
let init (env : EnvironmentSettings) (project : OpticalConstructorProject) : Model =
    initWithGroups env project []

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
    { model with project = project; groups = syncGroupsWithProject project model.groups }

let private setPlacements (ps : ElementPlacement list) (model : Model) : Model =
    withProject { model.project with placements = ps } model

let private setPlacement (i : int) (p : ElementPlacement) (model : Model) : Model =
    setPlacements (placements model |> List.mapi (fun j q -> if j = i then p else q)) model

/// Commit the model's freshly-updated project as one undoable edit on the shared multi-level
/// `EditHistory` (K.1 — every project-mutating action is a snapshot). PRECONDITION: `model.project`
/// is the NEW project and `model.history.present` is still the OLD project (the between-edits
/// invariant); `History.push` moves the old present onto `past`, sets `present` to the new project,
/// and clears the redo future — re-establishing the invariant. Pure; no parallel history (R-8).
let private commit (model : Model) : Model =
    { model with history = History.push model.project model.history }

/// Commit one undoable edit ONLY when the project actually changed from the history present —
/// the no-op guard (K.1 risk: never push on a no-op edit). A slide that hit a clamp bound, or a
/// drag that ended without moving, leaves `model.project = model.history.present`, so nothing is
/// pushed and `Ctrl+Z` is not littered with empty steps. Relies on the between-edits invariant
/// (`history.present = project`) holding when the edit began.
let private commitIfChanged (model : Model) : Model =
    if model.project = model.history.present then model else commit model

/// Apply a placement transform to the active element, committing one undoable edit — guarded by
/// `commitIfChanged`, so an INERT transform pushes no empty undo step (K.1 risk: never push on a
/// no-op edit). The transform is inert when the lock-respecting setters drop it: a rotation about a
/// locked axis (R3 is locked by default, A.1.2) or an emission toggle that would create the
/// unrepresentable both-off state both leave the placement — and so the project — unchanged.
let private editActive (f : ElementPlacement -> ElementPlacement) (model : Model) : Model =
    match activeIndex model with
    | Some i ->
        match tryPlacement i model with
        | Some p -> commitIfChanged (setPlacement i (f p) model)
        | None -> model
    | None -> model

// ---------------------------------------------------------------------------
// Selection / hit-test (§C.3 / AC-C3).
// ---------------------------------------------------------------------------

/// Hit-test a table-frame point against the elements (§C.3): the first element whose
/// projected drawer footprint contains the point, else the table. This keeps the live
/// selection target aligned with the live orthographic drawing: an unrotated cylinder
/// whose axis lies along the CR is a rectangle on screen, so the clickable footprint is
/// that rectangle rather than an unrelated centre circle.
let hitTest (point : TablePoint) (model : Model) : Selection =
    let px = point.x / 1.0<meter>
    let py = point.y / 1.0<meter>
    let closeToCap (radius : float<meter>) (center : TablePoint) =
        let dx = (center.x - point.x) / 1.0<meter>
        let dy = (center.y - point.y) / 1.0<meter>
        sqrt (dx * dx + dy * dy) <= (radius / 1.0<meter>)
    let onSegment (a : TablePoint) (b : TablePoint) =
        let ax = a.x / 1.0<meter>
        let ay = a.y / 1.0<meter>
        let bx = b.x / 1.0<meter>
        let by = b.y / 1.0<meter>
        let vx = bx - ax
        let vy = by - ay
        let wx = px - ax
        let wy = py - ay
        let cross = abs (vx * wy - vy * wx)
        let dot = wx * vx + wy * vy
        let len2 = vx * vx + vy * vy
        cross < 1.0e-9 && dot >= -1.0e-9 && dot <= len2 + 1.0e-9
    let pointInFrame (frame : TablePoint list) =
        match frame with
        | [] -> false
        | _ ->
            let edges = frame |> List.pairwise
            if edges |> List.exists (fun (a, b) -> onSegment a b) then true
            else
                edges
                |> List.fold (fun inside (a, b) ->
                    let ax = a.x / 1.0<meter>
                    let ay = a.y / 1.0<meter>
                    let bx = b.x / 1.0<meter>
                    let by = b.y / 1.0<meter>
                    if (ay > py) <> (by > py) then
                        let xAtY = ax + (py - ay) * (bx - ax) / (by - ay)
                        if px < xAtY then not inside else inside
                    else inside) false
    let within (p : ElementPlacement) =
        let g = Drawer.draw p.placementPoint p.r1 p.r2 p.r3 p.catalogueKind p.valueId false
        pointInFrame g.frame || (g.visibleCapCenters |> List.exists (closeToCap g.capRadius))
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

/// Snap an arbitrary table point to the nearest point on the current central-ray segment
/// (E.7.1). A ribbon/button drop at the table origin therefore lands at the beam centre,
/// while a real drag drop respects where the user released it and only removes the
/// perpendicular offset from the ray.
let snapToCentralRay (point : TablePoint) (model : Model) : TablePoint =
    let (s, d) = centralRayEndpoints model
    let sx = s.x / 1.0<meter>
    let sy = s.y / 1.0<meter>
    let dx = (d.x - s.x) / 1.0<meter>
    let dy = (d.y - s.y) / 1.0<meter>
    let len2 = dx * dx + dy * dy
    if len2 < 1.0e-12 then s
    else
        let px = point.x / 1.0<meter>
        let py = point.y / 1.0<meter>
        let t = max 0.0 (min 1.0 (((px - sx) * dx + (py - sy) * dy) / len2))
        { x = (sx + t * dx) * 1.0<meter>; y = (sy + t * dy) * 1.0<meter> }

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
    commit
        (withProject { model.project with placements = ps }
            { model with
                rayOf = rayOf'
                selection = TableSelected })

/// Reset the active element's rotation to zero (K.2, confirmation-gated). Guarded by
/// `commitIfChanged`, so confirming the reset on an element that is ALREADY at zero rotation is
/// inert and pushes no empty undo step (K.1 risk: never push on a no-op edit).
let private resetRotationNow (i : int) (model : Model) : Model =
    match tryPlacement i model with
    | Some p -> commitIfChanged (setPlacement i { p with r1 = Angle.zero; r2 = Angle.zero; r3 = Angle.zero } model)
    | None -> model

let private appendPlacement (p : ElementPlacement) (model : Model) : Model =
    let ps = placements model @ [ p ]
    commit
        (withProject { model.project with placements = ps }
            { model with selection = ElementSelected (List.length ps - 1) })

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
// Multiple detectors & the primary one (Part G / G.2). Detectors are `Detector`-kind
// placements; the primary is the FIRST detector (the convention `centralRayEndpoints` already
// uses). "Set as primary" reorders the placements so the chosen detector becomes first — a real
// project mutation that round-trips and pushes an undoable snapshot (K.1), with NO new project
// field (the project record is constructed in many sites; a new field would break them all and
// the spec keeps the placement/table anchors stable).
// ---------------------------------------------------------------------------

/// The indices of the detector placements (G.2): there may be more than one when a branch (e.g. a
/// reflected arm) needs its own (G.2.1).
let detectorIndices (model : Model) : int list =
    placements model
    |> List.mapi (fun i p -> i, p)
    |> List.filter (fun (_, p) -> p.catalogueKind = Detector)
    |> List.map fst

/// The primary detector's placement index (G.2): the FIRST detector. Results and the headline
/// readout follow it; secondary detectors report their own branch independently (consistent with
/// each branch carrying its own field, BeamTree.fs:98). `None` when no detector is placed.
let primaryDetectorIndex (model : Model) : int option =
    detectorIndices model |> List.tryHead

/// Move item `i` to the front of a list (used to make a detector primary). Out-of-range is inert.
let private moveToFront (i : int) (xs : 'a list) : 'a list =
    match List.tryItem i xs with
    | Some x -> x :: (xs |> List.mapi (fun j y -> j, y) |> List.filter (fun (j, _) -> j <> i) |> List.map snd)
    | None -> xs

/// Re-key an index after moving item `i` to the front: `i -> 0`; an index below `i` shifts up by
/// one; an index above `i` is unchanged. Used to keep `rayOf`/`selection` consistent across a
/// set-primary reorder.
let private remapToFront (i : int) (j : int) : int =
    if j = i then 0 elif j < i then j + 1 else j

/// Make the detector at index `i` the primary one (G.2 / AC-G2): move it to the front of the
/// placements so it becomes the first detector. A real project mutation — it round-trips and
/// pushes an undoable snapshot (K.1) — with `rayOf`/`selection` index-remapped. Guarded by
/// `commitIfChanged`, so it is inert (no empty undo step) when `i` is not a detector or is ALREADY
/// the primary (index 0, where `moveToFront 0` is identity).
let private setPrimaryDetectorNow (i : int) (model : Model) : Model =
    match tryPlacement i model with
    | Some p when p.catalogueKind = Detector ->
        let ps' = moveToFront i (placements model)
        let rayOf' = model.rayOf |> Map.toList |> List.map (fun (j, r) -> remapToFront i j, r) |> Map.ofList
        let selection' =
            match model.selection with
            | ElementSelected k -> ElementSelected (remapToFront i k)
            | TableSelected -> TableSelected
        commitIfChanged (withProject { model.project with placements = ps' } { model with rayOf = rayOf'; selection = selection' })
    | _ -> model

/// Add a detector (G.2.1): append a fresh `Detector` placement snapped to the central-ray middle,
/// pushing an undoable snapshot (K.1). The user may place more than one detector this way.
let private addDetectorNow (model : Model) : Model =
    appendPlacement (ElementPlacement.create Detector (centralRayMiddle model)) model

/// Remove the detector at index `i` (G.2.1): deletes the placement (reusing `deleteNow`, which
/// re-keys `rayOf` and pushes a snapshot). Inert when `i` is not a detector.
let private removeDetectorNow (i : int) (model : Model) : Model =
    match tryPlacement i model with
    | Some p when p.catalogueKind = Detector -> deleteNow i model
    | _ -> model

// ---------------------------------------------------------------------------
// Element groups in the workspace (Part G / G.1). The group holds its members' FULL
// configurations; toggling a member on/off (or swapping) flips only which members are in the beam
// and reflects that into `project.placements` (so it is a snapshot edit, K.1), never the stored
// configuration (AC-G1). The group's own member-mode rules live in the pure `Groups` domain.
// ---------------------------------------------------------------------------

/// Remove the FIRST placement equal to `x` from a list (the inverse of an append). Used to take a
/// group member's placement out of the beam.
let rec private removeFirst (x : 'a) (xs : 'a list) : 'a list =
    match xs with
    | [] -> []
    | h :: t -> if h = x then t else h :: removeFirst x t

/// Reflect a group on/off (or swap) into the project placements (G.1.2): for every member whose
/// `inBeam` changed, add its placement when it came IN, remove its first matching placement when it
/// went OUT. `oldG`/`newG` have identical members in identical order (only `inBeam` flips), so the
/// diff is positional. Members that did not change leave the project untouched.
let private syncGroupChange (oldG : Groups.ElementGroup) (newG : Groups.ElementGroup) (project : OpticalConstructorProject) : OpticalConstructorProject =
    List.zip oldG.members newG.members
    |> List.fold (fun proj (o, n) ->
        if o.inBeam = n.inBeam then proj
        elif n.inBeam then { proj with placements = proj.placements @ [ n.placement ] }
        else { proj with placements = removeFirst n.placement proj.placements }) project

/// Apply a group transform (`setInBeam`/`swapTo`) to the group at index `gi`, reflect it into the
/// project, and commit one undoable snapshot IFF the project actually changed (K.1 — no no-op
/// pushes). The stored member configurations are preserved by the pure `Groups` operation (AC-G1).
let private applyGroupChange (gi : int) (f : Groups.ElementGroup -> Groups.ElementGroup) (model : Model) : Model =
    match List.tryItem gi model.groups with
    | Some g ->
        let g' = f g
        let project' = syncGroupChange g g' model.project
        let groups' = model.groups |> List.mapi (fun j x -> if j = gi then g' else x)
        let model' = { model with groups = groups' }
        if project' = model.project then model'
        else commit (withProject project' model')
    | None -> model

/// Create a one-member group from the active element (a small workspace creation path for G.1): the
/// new multi-select group carries the active element's full configuration, marked in-beam (it is
/// already placed), so it can then be toggled out / back in. Adds the group only — not a project
/// mutation, so no snapshot is pushed.
let private groupActiveElementNow (model : Model) : Model =
    match activeElement model with
    | Some p ->
        let name = sprintf "Group %d" (List.length model.groups + 1)
        let g =
            Groups.ElementGroup.create name Groups.MultiSelect
            |> Groups.ElementGroup.addMember p
            |> Groups.ElementGroup.setInBeam 0 true
        { model with groups = model.groups @ [ g ] }
    | None -> model

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

let private rotateViewBy (notches : int) (model : Model) : Model =
    let step = Angle.degree (float notches * model.keyMap.rotationStepDegrees)
    { model with view = { model.view with r1 = model.view.r1 + step } }

let private tryPositiveFloat (text : string) : float option =
    let mutable value = 0.0
    if System.Double.TryParse(text, NumberStyles.Float, CultureInfo.InvariantCulture, &value) && value > 0.0
    then Some value
    else None

let private setTableLengthMeters (text : string) (model : Model) : Model =
    match tryPositiveFloat text with
    | Some length ->
        let table' = { model.project.table with length = length * 1.0<meter> }
        commitIfChanged (withProject { model.project with table = table' } model)
    | None -> model

let private setTableWidthMeters (text : string) (model : Model) : Model =
    match tryPositiveFloat text with
    | Some width ->
        let table' = { model.project.table with width = width * 1.0<meter> }
        commitIfChanged (withProject { model.project with table = table' } model)
    | None -> model

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
    | RotateR1 -> rotateActive AxisR1 1 m
    | RotateR2 -> rotateActive AxisR2 1 m
    | RotateR3 -> rotateActive AxisR3 1 m
    // Reset view is ephemeral view state, NOT captured by the project-snapshot EditHistory, so K.2
    // requires it be confirmation-gated rather than undoable: arm a pending confirm (resolved by
    // ConfirmPending, rendered as the same-row Confirm/Cancel gate).
    | ResetView -> { m with pending = Some ConfirmResetView }
    // Detectors (G.2). Add appends a detector; remove/set-primary act on the ACTIVE detector (the
    // selection the context-menu "Set as primary" / "Remove detector" target). Each is a real
    // project mutation that pushes an undoable snapshot (K.1).
    | AddDetector -> addDetectorNow m
    | RemoveDetector ->
        match activeIndex m with
        | Some i -> removeDetectorNow i m
        | None -> m
    | SetPrimaryDetector ->
        match activeIndex m with
        | Some i -> setPrimaryDetectorNow i m
        | None -> m
    // The number-key group-toggle (ToggleGroup) and the group swap are parameterized by the group /
    // member, supplied by the Experiment-tab control or the digit event — handled by their own
    // messages (GroupToggle / GroupSwap), inert when Invoke'd parameterlessly.
    | ToggleGroup -> m
    | SwapGroup -> m
    | Undo ->
        let h = History.undo m.history
        withProject h.present { m with history = h }
    | Redo ->
        let h = History.redo m.history
        withProject h.present { m with history = h }
    | SaveProject -> { m with saveRequests = m.saveRequests + 1 }
    | CancelOrDeselect ->
        { m with
            selection = TableSelected
            contextMenuOpen = false
            elementDialogOpen = false
            helpOpen = false
            valueIdModalOpen = false
            pending = None
            placementDraft = None
            drag = NoDrag
            movableHint = false }
    // Gesture-driven commands handled by their own messages (not invokable parameterlessly):
    | SlideAlongRay | MoveToRay | PanView | ZoomView | PlaceFromRibbon -> m

/// The ONE source of truth for the commands a ribbon/menu button MUST render DISABLED:
/// the commands with NO visible front-door effect on the constructor page — the seven
/// GESTURE-ONLY / PARAMETERIZED commands. The ribbon reads it through `isParameterlessInvokable`,
/// and the `RibbonTests` disabled-set derives from it, so there is no second hand-maintained copy
/// to drift:
///
///   * the gesture-only commands (`SlideAlongRay`, `MoveToRay`, `PanView`, `ZoomView`,
///     `PlaceFromRibbon`, and the `ToggleGroup` number-key group-toggle) — `applyCommand`
///     returns the model UNCHANGED because they need event context (a wheel notch, a drag delta, a
///     ribbon-drop kind/point, a group/member) a parameterless `Invoke` cannot supply;
///   * the slice-007 `SwapGroup` — parameterized by the group + target member (the Experiment-tab
///     per-member control supplies them via `GroupSwap`), so a parameterless `Invoke` no-ops.
///
/// The four element-edit commands (`OpenElementDialog`, `ElementContextMenu`, `ResetRotation`,
/// `DeleteElement`) are NO LONGER here: slice 007 renders their front-door overlays (the element
/// dialog, the right-click context menu, and the same-row confirm gate), so an enabled ribbon
/// button now leads to a visible, dismissible surface — they re-enable (the slice-006 hand-off).
/// `RotateR1`/`RotateR2`/`RotateR3` now also have parameterless effects: each applies one configured
/// rotation step to the active element, while the wheel gestures remain the faster mouse binding.
/// `AddDetector`/`RemoveDetector`/`SetPrimaryDetector` have real `applyCommand` effects, so they are
/// invokable and absent here too. When a later slice gives a gesture-only command a parameterless
/// step, drop it from this list.
let commandsWithoutFrontDoorSurface : Command list =
    [ // gesture-only — inert in `applyCommand` (need event context)
      SlideAlongRay; MoveToRay; PanView; ZoomView; PlaceFromRibbon; ToggleGroup
      // parameterized group swap — driven by the Experiment-tab control via `GroupSwap`
      SwapGroup ]

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
            // A discrete keyboard arrow press is one undoable edit: slide, then commit iff it moved
            // (the clamp at a neighbour bound makes a press at the edge a no-op — no empty snapshot).
            commitIfChanged (slideActiveToX (p.placementPoint.x + dir * mag) model)
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
    /// Pick a Build-catalogue element and show it under the pointer until the table click-drop.
    | StartPlacement of CatalogueKind
    /// Move the pending element preview; the pointer is the element's bounding-box centre.
    | PreviewPlacementAt of TablePoint
    /// Commit the pending element at the click/drop point, snapped onto the nearest CR path.
    | DropPendingPlacement of TablePoint
    /// Drop an element dragged from the Build ribbon, snapped to the CR middle (§E.7 / AC-E4).
    | RibbonDrop of CatalogueKind * TablePoint
    /// Trace / View toggles (§C.4/C.7): CR-only vs all rays, and element bounding boxes.
    | SetCentralRayOnly of bool
    | SetBoundingBoxVisible of bool
    /// Trace / View table controls (§C.1/C.2): persisted table dimensions and in-plane view spin.
    | SetTableLengthMeters of string
    | SetTableWidthMeters of string
    | RotateViewR1 of int
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
    /// Dismiss the right-click context-menu / element-dialog overlays WITHOUT deselecting (mirrors
    /// `CloseValueIdModal`/`CloseHelp`): closing the overlay leaves the active element selected.
    | CloseContextMenu
    | CloseElementDialog
    /// Element groups (Part G, slice 007): put member `mi` of group `gi` in/out of the beam
    /// (`GroupToggle`, respecting the member mode), or swap the mutually-exclusive group `gi` to
    /// member `mi` (`GroupSwap`). Both flip only `inBeam` and reflect into the project as ONE
    /// undoable snapshot (K.1), never the members' stored configuration (AC-G1).
    | GroupToggle of int * int * bool
    | GroupSwap of int * int
    /// Create a one-member group from the active element (a small workspace group-creation path).
    | GroupActiveElement

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
            // Begin the slide WITHOUT snapshotting: mid-drag `SlideTo` mutates the project directly,
            // and `EndDrag` commits ONE undoable snapshot for the whole drag (guarded against a no-op
            // move) — so a drag is a single undo step, not one per pointer move (K.1, no double-push).
            | ElementSelected i -> { model with drag = SlideDrag i; selection = ElementSelected i }
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
            // The slide mutated the project across the drag; commit ONE undoable snapshot now,
            // iff it actually moved (a begin-then-end with no move pushes nothing, K.1).
            | SlideDrag _ -> commitIfChanged model
            | _ -> model
        { model' with drag = NoDrag }

    | KeyPress g ->
        match resolveKey (overridePairs model) g with
        | Some SlideAlongRay -> arrowSlide g model
        | Some cmd -> applyCommand cmd model
        | None -> model

    | StartPlacement kind ->
        { model with
            placementDraft = Some { kind = kind; previewPoint = None }
            selection = TableSelected
            drag = NoDrag
            movableHint = false
            contextMenuOpen = false
            elementDialogOpen = false
            pending = None }

    | PreviewPlacementAt point ->
        match model.placementDraft with
        | Some draft -> { model with placementDraft = Some { draft with previewPoint = Some point } }
        | None -> model

    | DropPendingPlacement point ->
        match model.placementDraft with
        | Some draft ->
            appendPlacement (ElementPlacement.create draft.kind (snapToCentralRay point model)) { model with placementDraft = None }
        | None -> model

    | RibbonDrop (kind, point) ->
        appendPlacement (ElementPlacement.create kind (snapToCentralRay point model)) model

    | SetCentralRayOnly on -> { model with showCentralRayOnly = on }

    | SetBoundingBoxVisible on -> { model with showBoundingBox = on }

    | SetTableLengthMeters text -> setTableLengthMeters text model

    | SetTableWidthMeters text -> setTableWidthMeters text model

    | RotateViewR1 notches -> rotateViewBy notches model

    | Invoke cmd -> applyCommand cmd model

    | MenuToggleLock axis -> toggleLock axis model

    | MenuToggleEmission branch -> toggleEmission branch model

    | OpenValueIdModal -> { model with valueIdModalOpen = true; lastCommand = None }

    | CloseValueIdModal -> { model with valueIdModalOpen = false }

    | CloseHelp -> { model with helpOpen = false }

    | CloseContextMenu -> { model with contextMenuOpen = false }

    | CloseElementDialog -> { model with elementDialogOpen = false }

    | ConfirmPending ->
        match model.pending with
        | Some (ConfirmResetRotation i) -> { (resetRotationNow i model) with pending = None }
        | Some (ConfirmDelete i) -> { (deleteNow i model) with pending = None }
        // Reset view confirmed (K.2): reset the ephemeral top-down view state. It is NOT a project
        // edit, so it is confirmation-gated (this) rather than undoable through `EditHistory`.
        | Some ConfirmResetView -> { model with view = Table.resetView model.view; pending = None }
        | None -> model

    | CancelPending -> { model with pending = None }

    // Element groups (Part G, slice 007). `applyGroupChange` runs the pure `Groups` operation,
    // reflects the in-beam diff into the project, and commits one undoable snapshot iff it changed.
    | GroupToggle (gi, mi, on) -> applyGroupChange gi (Groups.ElementGroup.setInBeam mi on) model
    | GroupSwap (gi, mi) -> applyGroupChange gi (Groups.ElementGroup.swapTo mi) model
    | GroupActiveElement -> groupActiveElementNow model

// ---------------------------------------------------------------------------
// Avalonia-free presentation helpers (the menu/ribbon of slice 006 read these).
// ---------------------------------------------------------------------------

/// The element context-menu contents (§E.2): the registry commands flagged
/// `inContextMenu` (Element dialog, Local help, Reset rotation, Delete, Duplicate)
/// — projected from the ONE registry — surfaced when an element is active.
let contextMenuCommands : Command list = Commands.contextMenuCommands

/// The localization key for the pending destructive action's confirm prompt (§E.2 / K.2), or
/// `None` when nothing is pending. The shell resolves the key through `Localization` and renders
/// the same-row Confirm/Cancel destructive gate (`Controls.destructiveGate`, distinct CTA colours).
let pendingPromptKey (model : Model) : string option =
    match model.pending with
    | Some (ConfirmResetRotation _) -> Some "confirm.resetRotation"
    | Some (ConfirmDelete _) -> Some "confirm.delete"
    | Some ConfirmResetView -> Some "confirm.resetView"
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

let private placementCursorCache = ConcurrentDictionary<CatalogueKind, Cursor>()

let private placementCursor (kind : CatalogueKind) : Cursor =
    placementCursorCache.GetOrAdd(
        kind,
        System.Func<CatalogueKind, Cursor>(fun k ->
            try
                let size = 48
                let bitmap = new RenderTargetBitmap(PixelSize(size, size))
                let placement = ElementPlacement.create k { x = 0.0<meter>; y = 0.0<meter> }
                let g = Drawer.draw placement.placementPoint placement.r1 placement.r2 placement.r3 placement.catalogueKind placement.valueId false
                let points = g.frame @ [ g.axisStart; g.axisEnd ] @ g.visibleCapCenters
                let extent =
                    points
                    |> List.map (fun p -> max (abs (p.x / 1.0<meter>)) (abs (p.y / 1.0<meter>)))
                    |> function
                       | [] -> 0.05
                       | xs -> List.max xs
                let radius = g.capRadius / 1.0<meter>
                let scale = (float size * 0.36) / max 0.001 (extent + radius)
                let center = float size / 2.0
                let toPoint (p : TablePoint) =
                    Point(center + (p.x / 1.0<meter>) * scale, center - (p.y / 1.0<meter>) * scale)
                let toRadius (r : float<meter>) = (r / 1.0<meter>) * scale
                use ctx = bitmap.CreateDrawingContext()
                let fill = toBrushA (min 0.45 g.fill.opacity) g.fill.color
                let framePen = Pen(toBrushA 1.0 ConstructorTable.elementFrameColor, 2.0)
                let axisPen = Pen(toBrushA 1.0 ConstructorTable.rayColor, 2.0)
                for a, b in g.frame |> List.pairwise do
                    ctx.DrawLine(framePen, toPoint a, toPoint b)
                for c in g.visibleCapCenters do
                    let r = toRadius g.capRadius
                    ctx.DrawEllipse(fill, framePen, toPoint c, r, r)
                ctx.DrawLine(axisPen, toPoint g.axisStart, toPoint g.axisEnd)
                new Cursor(bitmap, PixelPoint(size / 2, size / 2))
            with _ ->
                new Cursor(StandardCursorType.Cross)))

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

let private toTablePoint (v : Vector3) : TablePoint =
    { x = v.x * 1.0<meter>; y = v.y * 1.0<meter> }

let private addV (a : Vector3) (b : Vector3) : Vector3 =
    Vector3.create (a.x + b.x) (a.y + b.y) (a.z + b.z)

let private scaleV (s : float) (v : Vector3) : Vector3 =
    Vector3.create (s * v.x) (s * v.y) (s * v.z)

let private centralRayDirectionOf (model : Model) : Vector3 =
    let (s, d) = centralRayEndpoints model
    let v = Vector3.create ((d.x - s.x) / 1.0<meter>) ((d.y - s.y) / 1.0<meter>) 0.0
    if v.norm < 1.0e-12 then centralRayDirection else v.normalized

let private endpointAtTableEdge (start : TablePoint) (direction : Vector3) (model : Model) : TablePoint =
    let d = direction.normalized
    let x0 = start.x / 1.0<meter>
    let y0 = start.y / 1.0<meter>
    let halfL = model.project.table.length / 2.0 / 1.0<meter>
    let halfW = model.project.table.width / 2.0 / 1.0<meter>
    let eps = 1.0e-12
    let candidates =
        [ if d.x > eps then (halfL - x0) / d.x
          if d.x < -eps then (-halfL - x0) / d.x
          if d.y > eps then (halfW - y0) / d.y
          if d.y < -eps then (-halfW - y0) / d.y ]
        |> List.filter (fun t -> t > eps)
    let t = match candidates with | [] -> 0.25 | ts -> List.min ts
    addV (RayModel.pointToVector3 start) (scaleV t d) |> toTablePoint

/// One pure table-frame ray segment the live canvas draws. Kept public so tests can
/// assert the CR/all-rays toggle and branch tracing without needing a headless render.
type DrawnRaySegment =
    {
        group : RayModel.RayGroupKind
        isCentral : bool
        startPoint : TablePoint
        endPoint : TablePoint
    }

let private bundleSegments
    (group : RayModel.RayGroupKind)
    (startPoint : TablePoint)
    (endPoint : TablePoint)
    (direction : Vector3)
    (faceExtent : float<meter>)
    : DrawnRaySegment list =
    let b0 = RayModel.bundleAt (RayModel.pointToVector3 startPoint) direction faceExtent
    let b1 = RayModel.bundleAt (RayModel.pointToVector3 endPoint) direction faceExtent
    List.zip b0.sideRays b1.sideRays
    |> List.map (fun (a, b) ->
        { group = group
          isCentral = false
          startPoint = toTablePoint a
          endPoint = toTablePoint b })

/// The ray segments rendered by the constructor table: the incident central ray, the
/// optional eight incident side rays, and outgoing reflected/transmitted branches from
/// each non-source/non-detector element.
let drawnRaySegments (model : Model) : DrawnRaySegment list =
    let (source, detector) = centralRayEndpoints model
    let incoming = centralRayDirectionOf model
    let incidentCentral =
        { group = RayModel.Incident
          isCentral = true
          startPoint = source
          endPoint = detector }
    let incidentSides =
        if model.showCentralRayOnly then []
        else bundleSegments RayModel.Incident source detector incoming opticalFaceExtent
    let branchSegments =
        placements model
        |> List.filter (fun p -> p.catalogueKind <> LightSource && p.catalogueKind <> Detector)
        |> List.collect (fun p ->
            RayModel.outgoingGroups p
            |> List.choose (fun group ->
                RayModel.toBeamBranch group
                |> Option.map (fun branch ->
                    let direction = RayModel.outgoingDirection branch incoming p
                    let finish = endpointAtTableEdge p.placementPoint direction model
                    let central =
                        { group = group
                          isCentral = true
                          startPoint = p.placementPoint
                          endPoint = finish }
                    let sides =
                        if model.showCentralRayOnly then []
                        else bundleSegments group p.placementPoint finish direction p.box.a1
                    central :: sides))
            |> List.concat)
    [ incidentCentral ] @ incidentSides @ branchSegments

/// The geometry rendered for one placement. Public for regression tests so they can
/// lock the live view to the same standard drawer the pure C.4 tests exercise.
let elementGeometry (model : Model) (p : ElementPlacement) : Drawer.DrawerGeometry =
    Drawer.draw p.placementPoint p.r1 p.r2 p.r3 p.catalogueKind p.valueId model.showBoundingBox

let private viewLine (model : Model) (stroke : ConstructorTable.Stroke) (a : TablePoint) (b : TablePoint) : IView =
    Line.create [
        Line.startPoint (toScreen model.view a)
        Line.endPoint (toScreen model.view b)
        Line.stroke (toBrushA stroke.opacity stroke.color)
        Line.strokeThickness stroke.weightPx
    ] :> IView

let private capView (model : Model) (fill : Drawer.Fill) (stroke : ConstructorTable.Stroke) (center : TablePoint) (radius : float<meter>) : IView =
    let c = toScreen model.view center
    let r = (radius / 1.0<meter>) * ConstructorTable.basePixelsPerMeter * model.view.zoom
    Ellipse.create [
        Ellipse.left (c.X - r)
        Ellipse.top (c.Y - r)
        Ellipse.width (2.0 * r)
        Ellipse.height (2.0 * r)
        Ellipse.fill (toBrushA fill.opacity fill.color)
        Ellipse.stroke (toBrushA stroke.opacity stroke.color)
        Ellipse.strokeThickness stroke.weightPx
    ] :> IView

let private plateView (model : Model) : IView list =
    let corners =
        ConstructorTable.plateCorners model.project.table model.view
        |> List.map (fun d -> let (rx, ry) = rotateInPlane model.view.r1 (d.dx, d.dy) in (centerX + rx, centerY + ry))
    let xs = corners |> List.map fst
    let ys = corners |> List.map snd
    let left = List.min xs
    let top = List.min ys
    let fill =
        Rectangle.create [
            Rectangle.left left
            Rectangle.top top
            Rectangle.width (List.max xs - left)
            Rectangle.height (List.max ys - top)
            Rectangle.fill (toBrush ConstructorTable.tablePlateColor)
            Rectangle.stroke (toBrush ConstructorTable.elementFrameColor)
            Rectangle.strokeThickness 1.0
        ] :> IView
    let halfL = model.project.table.length / 2.0
    let halfW = model.project.table.width / 2.0
    let plateCorners =
        [ { x = -halfL; y = -halfW }
          { x =  halfL; y = -halfW }
          { x =  halfL; y =  halfW }
          { x = -halfL; y =  halfW }
          { x = -halfL; y = -halfW } ]
    let outlineStroke = { Drawer.frameStroke with opacity = 1.0 }
    fill :: (plateCorners |> List.pairwise |> List.map (fun (a, b) -> viewLine model outlineStroke a b))

let private raySegmentView (model : Model) (segment : DrawnRaySegment) : IView =
    viewLine model (ConstructorTable.rayStroke segment.group segment.isCentral) segment.startPoint segment.endPoint

let private elementView (model : Model) (p : ElementPlacement) : IView list =
    let g = elementGeometry model p
    let frame =
        g.frame
        |> List.pairwise
        |> List.map (fun (a, b) -> viewLine model g.frameStroke a b)
    let axisStroke = { g.frameStroke with weightPx = max g.frameStroke.weightPx ConstructorTable.centralRayWeightPx }
    let axis = viewLine model axisStroke g.axisStart g.axisEnd
    let caps =
        g.visibleCapCenters
        |> List.map (fun c -> capView model g.fill g.frameStroke c g.capRadius)
    let boxStroke = { g.frameStroke with opacity = 0.55; weightPx = 0.75 }
    let box =
        g.boundingBoxEdges
        |> List.map (fun (a, b) -> viewLine model boxStroke a b)
    caps @ frame @ [ axis ] @ box

let private placementDraftView (model : Model) : IView list =
    match model.placementDraft with
    | Some { kind = kind; previewPoint = Some point } -> ElementPlacement.create kind point |> elementView model
    | Some { previewPoint = None } -> []
    | None -> []

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
        plateView model
        @ (drawnRaySegments model |> List.map (raySegmentView model))
        @ (placements model |> List.collect (elementView model))
        @ placementDraftView model
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
        Border.cursor (
            match model.placementDraft with
            | Some draft -> placementCursor draft.kind
            | None -> new Cursor(StandardCursorType.Arrow))
        Border.background (Brushes.White :> IBrush)
        Border.child canvas
        Border.onPointerPressed (fun e ->
            let pt = e.GetPosition null
            let tp = fromScreen model.view pt.X pt.Y
            let mods = modifiersOf e.KeyModifiers
            if e.GetCurrentPoint(null).Properties.IsRightButtonPressed then
                if Option.isSome model.placementDraft then dispatch (Invoke CancelOrDeselect)
                else dispatch (ContextMenuAt tp)
            elif Option.isSome model.placementDraft then
                dispatch (DropPendingPlacement tp)
            else
                lastPointer <- Some pt
                if e.ClickCount >= 2 then dispatch (OpenDialogAt tp)
                else
                    dispatch (SelectAt tp)
                    dispatch (BeginDrag (mods, tp)))
        Border.onPointerMoved (fun e ->
            let pt = e.GetPosition null
            if Option.isSome model.placementDraft then
                dispatch (PreviewPlacementAt (fromScreen model.view pt.X pt.Y))
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
