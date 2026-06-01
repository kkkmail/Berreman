/// Visual stack/layer editor over the reused engine stack types (§B.5–B.7).
///
/// Per the §0/P3 testability mandate the EDIT MODEL is Avalonia-free: every
/// operation is a pure, immutable transformation producing a NEW
/// `OpticalSystem` by record copy-and-update over the reused `Layer`/
/// `OpticalSystem` records (`Media.fs:24,94`) — the engine records are never
/// mutated. The selectors set the reused `upper`/`lower` half-spaces
/// (`Media.fs:97,100`) from a RESOLVED `OpticalProperties` (consumed, never
/// re-derived — slice 004's `resolveMaterial` seam, §D.8), and the substrate
/// switch toggles the reused `Substrate` DU (`Media.fs:40`).
///
/// This module carries NO Avalonia type (P3): it is the pure edit seam the
/// construction page and the tests drive. The Avalonia/FuncUI view that renders
/// it is deferred to a later UI-wiring slice (the project references the public
/// MIT `Avalonia.FuncUI` NuGet from slice 001, never the audit-gated clone).
module OpticalConstructor.Ui.StackEditor

open Berreman.Constants
open Berreman.Geometry
open Berreman.Fields
open Berreman.MaterialProperties
open Berreman.Media
open OpticalConstructor.Domain.Units
open OpticalConstructor.Domain.MaterialLibrary

// ---------------------------------------------------------------------------
// Pure, immutable layer-stack transforms over OpticalSystem.films (Media.fs:98).
// Each returns a NEW OpticalSystem by record copy-and-update; the engine
// Layer/OpticalSystem records are never mutated (Part A constraint 1 / AC-B5).
// ---------------------------------------------------------------------------

let private removeAt (index : int) (xs : 'a list) : 'a list =
    xs |> List.indexed |> List.filter (fun (i, _) -> i <> index) |> List.map snd

let private insertAt (index : int) (item : 'a) (xs : 'a list) : 'a list =
    let i = max 0 (min index (List.length xs))
    (xs |> List.take i) @ [ item ] @ (xs |> List.skip i)

/// Append a new layer to the bottom of the stack (§B.5 "add").
let addLayer (layer : Layer) (sys : OpticalSystem) : OpticalSystem =
    { sys with films = sys.films @ [ layer ] }

/// Delete the layer at `index`; an out-of-range index is a no-op (§B.5 "delete").
let deleteLayer (index : int) (sys : OpticalSystem) : OpticalSystem =
    if index < 0 || index >= List.length sys.films then sys
    else { sys with films = removeAt index sys.films }

/// Duplicate the layer at `index`, inserting the copy immediately below it
/// (§B.5 "duplicate"). Because `Layer` is an immutable record the copy shares
/// the same tensor/thickness value without aliasing any mutable state.
let duplicateLayer (index : int) (sys : OpticalSystem) : OpticalSystem =
    if index < 0 || index >= List.length sys.films then sys
    else { sys with films = insertAt (index + 1) sys.films.[index] sys.films }

/// Move the layer at `fromIndex` to `toIndex` (§B.5 "reorder"); out-of-range is
/// a no-op. Indices are interpreted against the original list.
let reorderLayer (fromIndex : int) (toIndex : int) (sys : OpticalSystem) : OpticalSystem =
    let n = List.length sys.films
    if fromIndex < 0 || fromIndex >= n || toIndex < 0 || toIndex >= n then sys
    else
        let item = sys.films.[fromIndex]
        { sys with films = sys.films |> removeAt fromIndex |> insertAt toIndex item }

/// Group the selected layers into a contiguous block at the position of the
/// first selected index, preserving their relative order (§B.5 "group").
/// `films` is a flat `List<Layer>`, so grouping is the pure transformation that
/// gathers a selection contiguously; the [Standard] repeat/period builder is
/// NOT this (it is Part J §J.2). Duplicate/out-of-range indices are ignored.
let groupLayers (indices : int list) (sys : OpticalSystem) : OpticalSystem =
    let n = List.length sys.films
    let valid = indices |> List.filter (fun i -> i >= 0 && i < n) |> List.distinct |> List.sort
    match valid with
    | [] -> sys
    | first :: _ ->
        let selectedSet = Set.ofList valid
        let selected = valid |> List.map (fun i -> sys.films.[i])
        let rest = sys.films |> List.indexed |> List.filter (fun (i, _) -> not (selectedSet.Contains i)) |> List.map snd
        let at = min first (List.length rest)
        { sys with films = (rest |> List.take at) @ selected @ (rest |> List.skip at) }

/// Crystal-axis ORIENTATION control for one layer ([Standard], 010 §1): REUSE
/// the engine's own `Layer.rotate` (`Media.fs:30`) — no Euler rotation is
/// re-derived here. Out-of-range index is a no-op.
let rotateLayer (index : int) (r : Rotation) (sys : OpticalSystem) : OpticalSystem =
    if index < 0 || index >= List.length sys.films then sys
    else { sys with films = sys.films |> List.mapi (fun i l -> if i = index then l.rotate r else l) }

/// Orientation control for the whole system ([Standard], 010 §1): REUSE the
/// engine's `OpticalSystem.rotate` (`Media.fs:120`).
let rotateSystem (r : Rotation) (sys : OpticalSystem) : OpticalSystem = sys.rotate r

// ---------------------------------------------------------------------------
// Incident & exit medium selectors (§B.6). Set the reused upper/lower half-space
// OpticalProperties (Media.fs:97,100), preserving films and substrate (AC-B6).
// ---------------------------------------------------------------------------

/// Set the incident (upper) half-space medium, preserving `films`/`substrate`/`lower`.
let setIncidentMedium (props : OpticalProperties) (sys : OpticalSystem) : OpticalSystem =
    { sys with upper = props }

/// Set the exit (lower) half-space medium, preserving `films`/`substrate`/`upper`.
let setExitMedium (props : OpticalProperties) (sys : OpticalSystem) : OpticalSystem =
    { sys with lower = props }

/// Resolve a medium by refractive-index entry, REUSING the engine's own
/// `OpticalProperties.fromRefractionIndex` constructor — Part B consumes the
/// engine tensor builder, it does NOT re-implement index→tensor construction (§B.6).
let mediumFromRefractionIndex (n : float) : OpticalProperties =
    OpticalProperties.fromRefractionIndex (RefractionIndex n)

/// Resolve a medium by materials-library reference through slice 004's single
/// by-id `resolveMaterial` seam (§D.8); dispersion resolution is NOT re-done here.
let mediumFromMaterial (lib : MaterialLibrary) (id : string) (w : WaveLength) : Result<OpticalProperties, MaterialError> =
    resolveMaterial lib id w

// ---------------------------------------------------------------------------
// Three-state substrate switch (§B.7). Toggles the reused Substrate DU
// (Media.fs:40); re-solving routes UNCHANGED through OpticalSystemSolver's
// substrate branch (Solvers.fs:201-230) — this module does not touch the solver.
// ---------------------------------------------------------------------------

/// The exactly-three states of the substrate switch. The case names avoid a
/// clash with the engine `Substrate` cases `Plate`/`Wedge` (`Media.fs:40`).
type SubstrateChoice =
    | AsThinFilm
    | AsPlate of Layer
    | AsWedge of WedgeLayer

/// Apply the substrate switch: thin film ⇒ `None`; plate ⇒ `Some (Plate _)`;
/// wedge ⇒ `Some (Wedge _)` (`Media.fs:99`). `films`/`upper`/`lower` are preserved.
let applySubstrate (choice : SubstrateChoice) (sys : OpticalSystem) : OpticalSystem =
    let substrate =
        match choice with
        | AsThinFilm -> None
        | AsPlate layer -> Some (Plate layer)
        | AsWedge wedge -> Some (Wedge wedge)
    { sys with substrate = substrate }

// ---------------------------------------------------------------------------
// The editor's pure update: one message DU covering every operation above, and
// a single `applyStackMsg` that maps a message onto the immutable transform.
// This is the Avalonia-free seam the construction page and the tests drive.
// ---------------------------------------------------------------------------

type StackMsg =
    | AddLayer of Layer
    | DeleteLayer of int
    | DuplicateLayer of int
    | ReorderLayer of int * int
    | GroupLayers of int list
    | RotateLayer of int * Rotation
    | SetIncidentMedium of OpticalProperties
    | SetExitMedium of OpticalProperties
    | SetSubstrate of SubstrateChoice

/// Pure editor update: apply a `StackMsg` to a system, yielding a NEW system.
let applyStackMsg (msg : StackMsg) (sys : OpticalSystem) : OpticalSystem =
    match msg with
    | AddLayer l -> addLayer l sys
    | DeleteLayer i -> deleteLayer i sys
    | DuplicateLayer i -> duplicateLayer i sys
    | ReorderLayer (a, b) -> reorderLayer a b sys
    | GroupLayers idx -> groupLayers idx sys
    | RotateLayer (i, r) -> rotateLayer i r sys
    | SetIncidentMedium p -> setIncidentMedium p sys
    | SetExitMedium p -> setExitMedium p sys
    | SetSubstrate c -> applySubstrate c sys

// ---------------------------------------------------------------------------
// Default-unit display (§B.8). Renders a stored canonical-SI thickness in a
// chosen boundary unit through the SOLE Units conversion seam (D.2); display
// only — the stored Thickness stays canonical SI (Part A constraint 3).
// ---------------------------------------------------------------------------

/// A reasonable default new layer: a 100 nm vacuum film (canonical SI thickness).
let defaultNewLayer : Layer =
    { properties = OpticalProperties.vacuum; thickness = Thickness.nm 100.0<nm> }

/// Human-facing thickness label for a layer row in the node's default unit.
let displayThickness (u : UnitOfMeasure) (t : Thickness) : string =
    match t with
    | Thickness.Infinity -> "∞ (substrate)"
    | Thickness.Thickness m -> sprintf "%g %A" (fromMeters u m) u

/// Human-facing labels for the stack-editor toolbar rows a view will render
/// (display metadata only; the view layer is deferred to a later UI-wiring slice).
let layerRowLabels (u : UnitOfMeasure) (sys : OpticalSystem) : string list =
    sys.films |> List.mapi (fun i l -> sprintf "Layer %d — %s" i (displayThickness u l.thickness))
