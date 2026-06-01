/// §J.5 — multi-system comparison workspace [Standard]. Holds several
/// `OpticalSystem` values side by side for comparison, each addressed by the open
/// project's system collection (`OpticalConstructorProject.systems`, §A.7). The
/// workspace owns LAYOUT and SELECTION state only — which systems are visible and
/// which is active; it holds no comparison document of its own (more than the
/// systems already in the open project is out of scope).
///
/// For overlaying the visible systems' computed curves the workspace DELEGATES to
/// the engine's existing comparison plot `plotComparison` (`Charting.fs:38`),
/// surfaced through the Part H §H.4 ScottPlot overlay (`Plot1DView.renderComparison`
/// / `SeriesData.seriesComparison`, which reuse `plotComparison`'s per-system
/// layout, `Charting.fs:38-49`) rather than the engine's terminal `Chart.show`.
/// Part J implements NO multi-curve overlay of its own.
///
/// Per the §0/P3 testability mandate and the sibling-module precedent the model +
/// `update` carry NO Avalonia type; the FuncUI view binding is a later UI-wiring
/// slice.
module OpticalConstructor.Ui.Workspace

open Berreman.Media
open Berreman.FieldFunctions
open Analytics.Variables
open OpticalConstructor.Domain.Project
open OpticalConstructor.Ui.Charts

// ---------------------------------------------------------------------------
// Selection / layout state over the open project's systems.
// ---------------------------------------------------------------------------

/// The workspace model: the open project plus the comparison selection state.
/// Systems are addressed by their index in `OpticalConstructorProject.systems`.
type Model =
    {
        project : OpticalConstructorProject
        /// Indices of the systems currently marked visible for comparison.
        visible : Set<int>
        /// The active system index (the one the editors focus), if any.
        active : int option
    }

type Msg =
    /// Toggle a system's visibility in the comparison overlay.
    | ToggleVisible of int
    /// Mark a system the active (focused) one.
    | SetActive of int

/// Valid system indices for the open project.
let private indices (model : Model) : Set<int> =
    set [ 0 .. List.length model.project.systems - 1 ]

/// Pure update: toggle visibility / set active, ignoring out-of-range indices.
let update (msg : Msg) (model : Model) : Model =
    match msg with
    | ToggleVisible i ->
        if not (Set.contains i (indices model)) then model
        elif Set.contains i model.visible then { model with visible = Set.remove i model.visible }
        else { model with visible = Set.add i model.visible }
    | SetActive i ->
        if Set.contains i (indices model) then { model with active = Some i } else model

/// Initialise the workspace for a project: the first system (if any) is visible
/// and active; the rest start hidden.
let init (project : OpticalConstructorProject) : Model =
    let hasSystems = not (List.isEmpty project.systems)
    {
        project = project
        visible = if hasSystems then set [ 0 ] else Set.empty
        active = if hasSystems then Some 0 else None
    }

/// The visible system indices, in ascending (display) order.
let visibleIndices (model : Model) : int list = model.visible |> Set.toList |> List.sort

/// The visible systems in display order — the layout/selection output the overlay
/// consumes (`OpticalSystem` values addressed by `systems`, §A.7).
let visibleSystems (model : Model) : OpticalSystem list =
    let systems = model.project.systems
    visibleIndices model |> List.map (fun i -> systems.[i])

/// Whether an overlay is meaningful: two or more systems are marked visible
/// (AC-J5 — "after two or more systems are marked visible").
let canOverlay (model : Model) : bool = Set.count model.visible >= 2

// ---------------------------------------------------------------------------
// Overlay delegation (R-5 / AC-J5). The workspace selects WHICH systems overlay;
// the overlay itself is Part H §H.4 / plotComparison. The OpticalSystem -> FixedInfo
// lift (the incident light / source a system is plotted under) is owned by the
// source layer (Part E) and supplied by the caller, so the workspace introduces
// no source store of its own.
// ---------------------------------------------------------------------------

/// The visible systems' comparison series for one `OpticalFunction`, DELEGATED to
/// the Part H §H.4 overlay data layer (`SeriesData.seriesComparison`, which reuses
/// `Charting.plotComparison`'s per-system layout). No Part-J-local overlay loop.
let comparisonSeries
    (lift : OpticalSystem -> FixedInfo)
    (fn : OpticalFunction)
    (x : RangedVariable)
    (model : Model)
    : SeriesData.Series1D list =
    SeriesData.seriesComparison (visibleSystems model |> List.map lift) fn x

/// Render the visible systems' overlay onto the Part H §H.4 ScottPlot surface
/// (`Plot1DView.renderComparison`, the §H.4 surfacing of `Charting.plotComparison`
/// without the engine's terminal `Chart.show`). The workspace constructs no plot
/// of its own.
let renderOverlay
    (settings : ChartSettings.ChartSettings)
    (lift : OpticalSystem -> FixedInfo)
    (fn : OpticalFunction)
    (x : RangedVariable)
    (model : Model)
    : ScottPlot.Plot =
    Plot1DView.renderComparison settings (visibleSystems model |> List.map lift) fn x
