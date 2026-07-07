/// Spec 0033 (021) — the pure, Avalonia-free SAMPLE-stack edit model over
/// `SampleStructure` (ElementId.fs). It mirrors the message-DU discipline of
/// the OpticalSystem editor (`StackEditor.StackMsg` / `applyStackMsg`,
/// OpticalConstructor.Ui/StackEditor.fs:161,176) but at the MATERIAL-ID level:
/// layers are `SampleLayer`s (materialId + thickness + orientation), never
/// resolved engine tensors, and the repeat/period structure (`Repeated` /
/// `PeriodGroup`) is edited as DATA — editing a period's cell edits every
/// repetition; changing the count adds or removes whole periods.
///
/// This module is DISTINCT from the untouched `StackEditor.groupLayers`
/// (StackEditor.fs:69): the existing OpticalSystem editor stays as-is. Every
/// operation is a pure, immutable transformation producing a NEW state by
/// record copy-and-update; the two repeat arms reject bad input with a TYPED
/// error (the same `count >= 1` rule `Validation.validateRepeatCount`,
/// OpticalConstructor.Ui/Validation.fs:56, enforces at the Ui boundary —
/// Domain cannot reference Ui, so the rule is restated here, not imported).
module OpticalConstructor.Domain.SampleStackEditor

open Berreman.Media
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.Library

/// Where an editable film layer lives inside `SampleStructure.films`: a
/// top-level `SingleLayer` slot, or ONE cell slot of a `Repeated` period
/// group. A repetition adds NO position — the cell slot is the single
/// editable identity, so an edit through it reaches every repetition.
/// (A `Repeated` item itself is not a layer position; groups are addressed
/// by their films index in `SetRepeatCount`.)
type LayerPosition =
    | AtSingleLayer of itemIndex : int
    | AtCellLayer of itemIndex : int * cellIndex : int

/// The editor state (the slice's "state record"): the structure under edit
/// plus the selected layer positions. Selection is a Set — no duplicates, no
/// order; every structural message keeps it valid (edits preserve shape,
/// moves remap it, remove/fold clear it).
type SampleStackEditState =
    {
        structure : SampleStructure
        selection : Set<LayerPosition>
    }

    /// A fresh editor over `structure` with nothing selected (a panel's
    /// initial state).
    static member ofStructure (structure : SampleStructure) : SampleStackEditState =
        { structure = structure; selection = Set.empty }

/// The typed rejections of the two repeat arms (errors as values, each case
/// carrying a diagnostic `reason` — never a throw).
type SampleStackEditError =
    /// A repeat count `< 1` — the same rule `Validation.validateRepeatCount`
    /// (Ui/Validation.fs:56) enforces at the Ui boundary.
    | InvalidRepeatCount of reason : string
    /// `MakeRepeatBlock` needs a non-empty, contiguous run of TOP-LEVEL
    /// single layers (a period cell / group cannot fold into another group).
    | SelectionNotFoldable of reason : string
    /// `SetRepeatCount` aimed at a films index that is not a `Repeated` group.
    | NotARepeatGroup of reason : string

/// The editor's message DU (the `SampleStructure` mirror of
/// `StackEditor.StackMsg`): one arm per operation, applied by
/// `applySampleStackMsg`.
type SampleStackMsg =
    /// Add a valid layer position to the selection (multi-select accumulates;
    /// an invalid position is a no-op, the StackEditor out-of-range precedent).
    | SelectLayer of LayerPosition
    /// REPLACE the selection with every position whose layer references the
    /// material — top-level single layers and period-cell slots alike.
    | SelectByMaterial of MaterialId
    | ClearSelection
    | SetThicknessOfSelected of Thickness
    | SetMaterialOfSelected of MaterialId
    | SetOrientationOfSelected of CrystalOrientation
    | RemoveSelected
    | MoveSelectedUp
    | MoveSelectedDown
    /// Fold the selected contiguous top-level single layers into ONE
    /// `Repeated` period group of `count` repetitions.
    | MakeRepeatBlock of count : int
    /// Set the repetition count of the `Repeated` group at a films index —
    /// whole periods are added or removed (the cell is untouched).
    | SetRepeatCount of groupIndex : int * count : int
    /// Append ONE single film layer to the bottom of the stack (spec 0033 gap
    /// G13 — the pure Domain arm the editor's Add-layer button previously
    /// lacked, forcing a view-level structural edit that broke the
    /// window-free-testability rule).
    | AddLayer of SampleLayer
    /// Set (`Some`) or clear (`None`) the thick substrate plate beneath the
    /// films (spec 0033 gap G12 — the sample editor could neither show nor
    /// edit `SampleStructure.substrate`).
    | SetSubstrate of SampleLayer option
    /// Set (`Some`) or clear (`None` = vacuum) the lower half-space material
    /// (spec 0033 gap G12 — likewise for `SampleStructure.lower`).
    | SetLower of MaterialId option

// ---------------------------------------------------------------------------
// Selection helpers.
// ---------------------------------------------------------------------------

/// Whether `position` names an existing film layer of `structure`.
let private isValidPosition (structure : SampleStructure) (position : LayerPosition) : bool =
    match position with
    | AtSingleLayer i ->
        match List.tryItem i structure.films with
        | Some (SingleLayer _) -> true
        | Some (Repeated _) | None -> false
    | AtCellLayer (i, j) ->
        match List.tryItem i structure.films with
        | Some (Repeated g) -> j >= 0 && j < List.length g.cell
        | Some (SingleLayer _) | None -> false

/// Every position whose layer references `materialId`. A `Repeated` group
/// contributes its matching CELL slots (once — repetition adds no position).
let private positionsOfMaterial (materialId : MaterialId) (structure : SampleStructure) : Set<LayerPosition> =
    structure.films
    |> List.indexed
    |> List.collect (fun (i, item) ->
        match item with
        | SingleLayer l -> if l.materialId = materialId then [ AtSingleLayer i ] else []
        | Repeated g ->
            g.cell
            |> List.indexed
            |> List.choose (fun (j, l) -> if l.materialId = materialId then Some (AtCellLayer (i, j)) else None))
    |> Set.ofList

// ---------------------------------------------------------------------------
// Pure structure transforms over the selection.
// ---------------------------------------------------------------------------

/// Apply `f` to every SELECTED layer, leaving everything else untouched. A
/// selected cell slot updates the ONE cell layer — and therefore every
/// repetition of the period.
let private mapSelected (selection : Set<LayerPosition>) (f : SampleLayer -> SampleLayer) (structure : SampleStructure) : SampleStructure =
    let films =
        structure.films
        |> List.mapi (fun i item ->
            match item with
            | SingleLayer l ->
                if Set.contains (AtSingleLayer i) selection then SingleLayer (f l) else item
            | Repeated g ->
                let cell = g.cell |> List.mapi (fun j l -> if Set.contains (AtCellLayer (i, j)) selection then f l else l)
                Repeated { g with cell = cell })
    { structure with films = films }

/// Remove every selected layer. Removing a cell slot removes that layer from
/// every repetition; a group whose cell empties is dropped whole (an empty
/// cell expands to nothing).
let private removeSelected (selection : Set<LayerPosition>) (structure : SampleStructure) : SampleStructure =
    let films =
        structure.films
        |> List.indexed
        |> List.choose (fun (i, item) ->
            match item with
            | SingleLayer _ ->
                if Set.contains (AtSingleLayer i) selection then None else Some item
            | Repeated g ->
                let cell =
                    g.cell
                    |> List.indexed
                    |> List.filter (fun (j, _) -> not (Set.contains (AtCellLayer (i, j)) selection))
                    |> List.map snd
                match cell with
                | [] -> None
                | _ -> Some (Repeated { g with cell = cell }))
    { structure with films = films }

// ---------------------------------------------------------------------------
// Move up / down: the standard pinned-frontier sequential-swap algorithm, per
// container (the top-level films list; each selected group's cell). Selected
// items shift one slot; a block already resting against the boundary stays
// pinned. The permutation is returned for EVERY index so the selection can
// follow the layers (and AtCellLayer group indices survive top-level moves).
// ---------------------------------------------------------------------------

/// Which way a move message shifts the selected layers.
type private MoveDirection =
    | TowardsTop
    | TowardsBottom

let private swapAt (i : int) (j : int) (xs : 'a list) : 'a list =
    let a = xs.[i]
    let b = xs.[j]
    xs |> List.mapi (fun k x -> if k = i then b elif k = j then a else x)

/// Move the items at `selected` one slot within `xs`, returning the reordered
/// list plus the old-index → new-index map for every item (movers and
/// displaced neighbours alike; unmoved indices map to themselves).
let private moveWithin (direction : MoveDirection) (selected : Set<int>) (xs : 'a list) : 'a list * Map<int, int> =
    let tagged = xs |> List.mapi (fun i x -> (i, x))
    let ordered =
        match direction with
        | TowardsTop -> Set.toList selected
        | TowardsBottom -> Set.toList selected |> List.rev
    let initialFrontier =
        match direction with
        | TowardsTop -> 0
        | TowardsBottom -> List.length xs - 1
    let moved, _ =
        ordered
        |> List.fold
            (fun (items : (int * 'a) list, frontier) i ->
                if i = frontier then
                    // Pinned against the boundary (or a pinned selected neighbour).
                    match direction with
                    | TowardsTop -> (items, frontier + 1)
                    | TowardsBottom -> (items, frontier - 1)
                else
                    match direction with
                    | TowardsTop -> (swapAt (i - 1) i items, frontier)
                    | TowardsBottom -> (swapAt i (i + 1) items, frontier))
            (tagged, initialFrontier)
    let mapping = moved |> List.mapi (fun newIndex (oldIndex, _) -> (oldIndex, newIndex)) |> Map.ofList
    (moved |> List.map snd, mapping)

/// One move step over the whole state: cell selections move within their own
/// period cell (reordering every repetition), top-level selections move
/// within `films`, and the selection follows the layers to their new
/// positions.
let private moveSelected (direction : MoveDirection) (state : SampleStackEditState) : SampleStackEditState =
    let topSelected =
        state.selection
        |> Set.toList
        |> List.choose (fun p -> match p with AtSingleLayer i -> Some i | AtCellLayer _ -> None)
        |> Set.ofList
    let cellSelectedByGroup =
        state.selection
        |> Set.toList
        |> List.choose (fun p -> match p with AtCellLayer (i, j) -> Some (i, j) | AtSingleLayer _ -> None)
        |> List.groupBy fst
        |> List.map (fun (g, pairs) -> (g, pairs |> List.map snd |> Set.ofList))
        |> Map.ofList
    // Cell-level moves first, at each group's OLD films index.
    let filmsAfterCells, cellMappings =
        state.structure.films
        |> List.mapi (fun i item ->
            match item, Map.tryFind i cellSelectedByGroup with
            | Repeated g, Some sel ->
                let cell, mapping = moveWithin direction sel g.cell
                (Repeated { g with cell = cell }, Some (i, mapping))
            | _, _ -> (item, None))
        |> List.unzip
        |> fun (items, mappings) -> (items, mappings |> List.choose id |> Map.ofList)
    // Then the top-level move.
    let films, topMapping = moveWithin direction topSelected filmsAfterCells
    let selection =
        state.selection
        |> Set.map (fun position ->
            match position with
            | AtSingleLayer i -> AtSingleLayer topMapping.[i]
            | AtCellLayer (i, j) ->
                let cellIndex =
                    match Map.tryFind i cellMappings with
                    | Some m -> m.[j]
                    | None -> j
                AtCellLayer (topMapping.[i], cellIndex))
    { structure = { state.structure with films = films }; selection = selection }

// ---------------------------------------------------------------------------
// The two repeat arms (the typed-error surface).
// ---------------------------------------------------------------------------

/// The `count >= 1` rule (`Validation.validateRepeatCount`'s rule, restated
/// here because Domain cannot reference Ui).
let private checkRepeatCount (count : int) : Result<int, SampleStackEditError> =
    if count >= 1 then Ok count
    else Error (InvalidRepeatCount (sprintf "repeat count must be at least 1, got %d" count))

/// Fold the selected CONTIGUOUS run of top-level single layers into ONE
/// `Repeated { cell; count }` at the run's position; the selection clears
/// (its positions no longer exist). Cell positions, groups, gaps and an empty
/// selection are `SelectionNotFoldable`.
let private makeRepeatBlock (count : int) (state : SampleStackEditState) : Result<SampleStackEditState, SampleStackEditError> =
    checkRepeatCount count
    |> Result.bind (fun count ->
        let hasCellPosition =
            state.selection |> Set.exists (fun p -> match p with AtCellLayer _ -> true | AtSingleLayer _ -> false)
        if hasCellPosition then
            Error (SelectionNotFoldable "the selection includes period-cell layers; only top-level single layers fold into a repeat block")
        else
            let indices =
                state.selection
                |> Set.toList
                |> List.choose (fun p -> match p with AtSingleLayer i -> Some i | AtCellLayer _ -> None)
            match indices with
            | [] -> Error (SelectionNotFoldable "nothing is selected")
            | first :: _ ->
                let contiguous = indices |> List.mapi (fun k i -> i = first + k) |> List.forall id
                if not contiguous then
                    Error (SelectionNotFoldable (sprintf "the selected layers are not contiguous: indices %A" indices))
                else
                    let cell =
                        indices
                        |> List.map (fun i -> state.structure.films.[i])
                        |> List.choose (fun item -> match item with SingleLayer l -> Some l | Repeated _ -> None)
                    if List.length cell <> List.length indices then
                        // Unreachable while the selection invariant holds (AtSingleLayer only
                        // ever names SingleLayer items) — kept total rather than throwing.
                        Error (SelectionNotFoldable "the selection names a period group; a group cannot fold into another repeat block")
                    else
                        let before = state.structure.films |> List.take first
                        let after = state.structure.films |> List.skip (first + List.length indices)
                        let films = before @ [ Repeated { cell = cell; count = count } ] @ after
                        Ok { structure = { state.structure with films = films }; selection = Set.empty })

/// Set the repetition count of the `Repeated` group at `groupIndex`. The cell
/// is untouched, so whole periods are added or removed and every selection
/// position stays valid.
let private setRepeatCount (groupIndex : int) (count : int) (state : SampleStackEditState) : Result<SampleStackEditState, SampleStackEditError> =
    checkRepeatCount count
    |> Result.bind (fun count ->
        match List.tryItem groupIndex state.structure.films with
        | Some (Repeated g) ->
            let films =
                state.structure.films
                |> List.mapi (fun i item -> if i = groupIndex then Repeated { g with count = count } else item)
            Ok { state with structure = { state.structure with films = films } }
        | Some (SingleLayer _) ->
            Error (NotARepeatGroup (sprintf "films item %d is a single layer, not a repeat group" groupIndex))
        | None ->
            Error (NotARepeatGroup (sprintf "films index %d is out of range (%d items)" groupIndex (List.length state.structure.films))))

// ---------------------------------------------------------------------------
// The editor's pure update (the SampleStructure mirror of `applyStackMsg`,
// StackEditor.fs:176): one message onto one immutable transform. Uniformly
// Result-shaped — the selection/edit arms always return `Ok`; only the two
// repeat arms reject, with the typed errors above.
// ---------------------------------------------------------------------------

let applySampleStackMsg (msg : SampleStackMsg) (state : SampleStackEditState) : Result<SampleStackEditState, SampleStackEditError> =
    match msg with
    | SelectLayer position ->
        if isValidPosition state.structure position
        then Ok { state with selection = Set.add position state.selection }
        else Ok state
    | SelectByMaterial materialId ->
        Ok { state with selection = positionsOfMaterial materialId state.structure }
    | ClearSelection ->
        Ok { state with selection = Set.empty }
    | SetThicknessOfSelected thickness ->
        Ok { state with structure = mapSelected state.selection (fun l -> { l with thickness = thickness }) state.structure }
    | SetMaterialOfSelected materialId ->
        Ok { state with structure = mapSelected state.selection (fun l -> { l with materialId = materialId }) state.structure }
    | SetOrientationOfSelected orientation ->
        Ok { state with structure = mapSelected state.selection (fun l -> { l with orientation = orientation }) state.structure }
    | RemoveSelected ->
        Ok { structure = removeSelected state.selection state.structure; selection = Set.empty }
    | MoveSelectedUp ->
        Ok (moveSelected TowardsTop state)
    | MoveSelectedDown ->
        Ok (moveSelected TowardsBottom state)
    | MakeRepeatBlock count ->
        makeRepeatBlock count state
    | SetRepeatCount (groupIndex, count) ->
        setRepeatCount groupIndex count state
    | AddLayer layer ->
        Ok { state with structure = { state.structure with films = state.structure.films @ [ SingleLayer layer ] } }
    | SetSubstrate substrate ->
        Ok { state with structure = { state.structure with substrate = substrate } }
    | SetLower lower ->
        Ok { state with structure = { state.structure with lower = lower } }
