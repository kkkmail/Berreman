namespace OpticalConstructor.Tests

open Xunit
open Berreman.Constants
open Berreman.Geometry
open Berreman.Media
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.SampleStackEditor

/// Spec 0033 step 021 — the pure, Avalonia-free SAMPLE-stack edit model
/// (`SampleStackEditor` over `SampleStructure`). Every message arm is exercised
/// here WITHOUT a window, including both acceptance criteria: `MakeRepeatBlock`
/// over 2 selected layers with count K expands to `2*K` films, and
/// `SelectByMaterial` → `SetThicknessOfSelected` updates exactly the matching
/// layers. Editing a period's cell edits every repetition (checked through
/// `SampleStructure.expandedFilms`); the repeat arms reject bad counts with the
/// TYPED errors (the same `count >= 1` rule as `Validation.validateRepeatCount`).
module SampleStackEditorTests =

    let private nm (t : float) : Thickness = Thickness.nm (t * 1.0<nm>)

    let private deg (d : float) : Angle = Angle.degree d

    let private layerOf (materialId : MaterialId) (thicknessNm : float) : SampleLayer =
        { materialId = materialId; thickness = nm thicknessNm; orientation = PrimaryAxes }

    /// films = [ glass 100 nm; vacuum 50 nm; glass 100 nm ] — three top-level single layers.
    let private threeSingles : SampleStructure =
        {
            films =
                [
                    SingleLayer (layerOf MaterialIds.glass152 100.0)
                    SingleLayer (layerOf MaterialIds.vacuum 50.0)
                    SingleLayer (layerOf MaterialIds.glass152 100.0)
                ]
            substrate = None
            lower = None
        }

    /// films = [ Repeated (glass 10 / vacuum 20, ×3); glass175 30 nm ] — a period group then a single.
    let private withGroup : SampleStructure =
        {
            films =
                [
                    Repeated
                        {
                            cell = [ layerOf MaterialIds.glass152 10.0; layerOf MaterialIds.vacuum 20.0 ]
                            count = 3
                        }
                    SingleLayer (layerOf MaterialIds.glass175 30.0)
                ]
            substrate = None
            lower = None
        }

    let private editorOf (structure : SampleStructure) : SampleStackEditState =
        SampleStackEditState.ofStructure structure

    /// Apply a message sequence, failing the test on any unexpected typed error.
    let private applyAll (msgs : SampleStackMsg list) (state : SampleStackEditState) : SampleStackEditState =
        msgs
        |> List.fold
            (fun s m ->
                match applySampleStackMsg m s with
                | Ok next -> next
                | Error e -> failwith $"unexpected editor error on %A{m}: %A{e}")
            state

    /// Apply one message expected to be rejected, returning the typed error.
    let private expectError (msg : SampleStackMsg) (state : SampleStackEditState) : SampleStackEditError =
        match applySampleStackMsg msg state with
        | Ok _ -> failwith $"expected a typed error for %A{msg}, got Ok"
        | Error e -> e

    // ================================ selection ================================

    [<Fact>]
    let ``SelectLayer adds a valid top-level position without touching the structure`` () =
        let state = editorOf threeSingles |> applyAll [ SelectLayer (AtSingleLayer 0) ]
        Assert.Equal<Set<LayerPosition>>(Set.ofList [ AtSingleLayer 0 ], state.selection)
        Assert.Equal(threeSingles, state.structure)

    [<Fact>]
    let ``SelectLayer accumulates positions (multi-select)`` () =
        let state = editorOf threeSingles |> applyAll [ SelectLayer (AtSingleLayer 0); SelectLayer (AtSingleLayer 2) ]
        Assert.Equal<Set<LayerPosition>>(Set.ofList [ AtSingleLayer 0; AtSingleLayer 2 ], state.selection)

    [<Fact>]
    let ``SelectLayer adds a valid period-cell position`` () =
        let state = editorOf withGroup |> applyAll [ SelectLayer (AtCellLayer (0, 1)) ]
        Assert.Equal<Set<LayerPosition>>(Set.ofList [ AtCellLayer (0, 1) ], state.selection)

    [<Fact>]
    let ``SelectLayer with an invalid position is a no-op`` () =
        // Out of range; a films index that names a GROUP, not a single layer; and a
        // cell address aimed at a single layer — none of them is a layer position.
        let state =
            editorOf withGroup
            |> applyAll
                [
                    SelectLayer (AtSingleLayer 7)
                    SelectLayer (AtSingleLayer 0)
                    SelectLayer (AtCellLayer (1, 0))
                    SelectLayer (AtCellLayer (0, 2))
                ]
        Assert.Equal<Set<LayerPosition>>(Set.empty, state.selection)

    [<Fact>]
    let ``SelectByMaterial selects exactly the matching positions, singles and cells alike`` () =
        let flat = editorOf threeSingles |> applyAll [ SelectByMaterial MaterialIds.glass152 ]
        Assert.Equal<Set<LayerPosition>>(Set.ofList [ AtSingleLayer 0; AtSingleLayer 2 ], flat.selection)
        let grouped = editorOf withGroup |> applyAll [ SelectByMaterial MaterialIds.glass152 ]
        Assert.Equal<Set<LayerPosition>>(Set.ofList [ AtCellLayer (0, 0) ], grouped.selection)

    [<Fact>]
    let ``SelectByMaterial replaces any prior selection`` () =
        let state =
            editorOf threeSingles
            |> applyAll [ SelectLayer (AtSingleLayer 1); SelectByMaterial MaterialIds.glass152 ]
        Assert.False(Set.contains (AtSingleLayer 1) state.selection)
        Assert.Equal<Set<LayerPosition>>(Set.ofList [ AtSingleLayer 0; AtSingleLayer 2 ], state.selection)

    [<Fact>]
    let ``ClearSelection empties the selection`` () =
        let state =
            editorOf threeSingles
            |> applyAll [ SelectByMaterial MaterialIds.glass152; ClearSelection ]
        Assert.Equal<Set<LayerPosition>>(Set.empty, state.selection)

    // ============================ editing the selected ============================

    [<Fact>]
    let ``SelectByMaterial then SetThicknessOfSelected updates exactly the matching layers`` () =
        // The slice's second acceptance criterion, over top-level single layers.
        let state =
            editorOf threeSingles
            |> applyAll [ SelectByMaterial MaterialIds.glass152; SetThicknessOfSelected (nm 77.0) ]
        let thicknesses = state.structure.expandedFilms |> List.map (fun l -> l.thickness)
        Assert.Equal<Thickness list>([ nm 77.0; nm 50.0; nm 77.0 ], thicknesses)

    [<Fact>]
    let ``SetThicknessOfSelected through a period cell edits every repetition`` () =
        // Editing the ONE selected cell slot changes that layer in all 3 periods;
        // the other cell layer is untouched in all of them.
        let state =
            editorOf withGroup
            |> applyAll [ SelectByMaterial MaterialIds.glass152; SetThicknessOfSelected (nm 77.0) ]
        let expanded = state.structure.expandedFilms
        Assert.Equal(7, List.length expanded)
        let glassLayers = expanded |> List.filter (fun l -> l.materialId = MaterialIds.glass152)
        Assert.Equal(3, List.length glassLayers)
        Assert.All(glassLayers, fun l -> Assert.Equal(nm 77.0, l.thickness))
        let vacuumLayers = expanded |> List.filter (fun l -> l.materialId = MaterialIds.vacuum)
        Assert.All(vacuumLayers, fun l -> Assert.Equal(nm 20.0, l.thickness))

    [<Fact>]
    let ``SetMaterialOfSelected rebinds exactly the selected layers`` () =
        let state =
            editorOf threeSingles
            |> applyAll [ SelectLayer (AtSingleLayer 1); SetMaterialOfSelected MaterialIds.glass175 ]
        let materials = state.structure.expandedFilms |> List.map (fun l -> l.materialId)
        Assert.Equal<MaterialId list>([ MaterialIds.glass152; MaterialIds.glass175; MaterialIds.glass152 ], materials)

    [<Fact>]
    let ``SetOrientationOfSelected orients exactly the selected layers`` () =
        let orientation = EulerRotation (ZmXpZm, deg 30.0, deg 40.0, deg 50.0)
        let state =
            editorOf threeSingles
            |> applyAll [ SelectLayer (AtSingleLayer 0); SetOrientationOfSelected orientation ]
        let orientations = state.structure.expandedFilms |> List.map (fun l -> l.orientation)
        Assert.Equal<CrystalOrientation list>([ orientation; PrimaryAxes; PrimaryAxes ], orientations)

    [<Fact>]
    let ``editing with an empty selection changes nothing`` () =
        let state = editorOf threeSingles |> applyAll [ SetThicknessOfSelected (nm 1.0) ]
        Assert.Equal(threeSingles, state.structure)

    // ================================ removing ================================

    [<Fact>]
    let ``RemoveSelected removes exactly the selected layers and clears the selection`` () =
        let state =
            editorOf threeSingles
            |> applyAll [ SelectByMaterial MaterialIds.glass152; RemoveSelected ]
        Assert.Equal<SampleLayer list>([ layerOf MaterialIds.vacuum 50.0 ], state.structure.expandedFilms)
        Assert.Equal<Set<LayerPosition>>(Set.empty, state.selection)

    [<Fact>]
    let ``RemoveSelected on a cell layer removes it from every repetition`` () =
        let state =
            editorOf withGroup
            |> applyAll [ SelectLayer (AtCellLayer (0, 0)); RemoveSelected ]
        // The 3 periods each lose their glass layer; the vacuum cell layer stays ×3.
        let expanded = state.structure.expandedFilms
        Assert.Equal<SampleLayer list>(
            [
                layerOf MaterialIds.vacuum 20.0
                layerOf MaterialIds.vacuum 20.0
                layerOf MaterialIds.vacuum 20.0
                layerOf MaterialIds.glass175 30.0
            ],
            expanded)

    [<Fact>]
    let ``RemoveSelected dropping the last cell layer drops the whole group`` () =
        let state =
            editorOf withGroup
            |> applyAll
                [
                    SelectLayer (AtCellLayer (0, 0))
                    SelectLayer (AtCellLayer (0, 1))
                    RemoveSelected
                ]
        Assert.Equal<StackItem list>([ SingleLayer (layerOf MaterialIds.glass175 30.0) ], state.structure.films)

    // ================================= moving =================================

    [<Fact>]
    let ``MoveSelectedUp moves the selected layer one slot up and the selection follows`` () =
        let state =
            editorOf threeSingles
            |> applyAll [ SelectLayer (AtSingleLayer 1); MoveSelectedUp ]
        let materials = state.structure.expandedFilms |> List.map (fun l -> l.materialId)
        Assert.Equal<MaterialId list>([ MaterialIds.vacuum; MaterialIds.glass152; MaterialIds.glass152 ], materials)
        Assert.Equal<Set<LayerPosition>>(Set.ofList [ AtSingleLayer 0 ], state.selection)

    [<Fact>]
    let ``MoveSelectedUp pinned at the top is a no-op`` () =
        let before = editorOf threeSingles |> applyAll [ SelectLayer (AtSingleLayer 0); SelectLayer (AtSingleLayer 1) ]
        let after = applyAll [ MoveSelectedUp ] before
        Assert.Equal(before.structure, after.structure)
        Assert.Equal<Set<LayerPosition>>(before.selection, after.selection)

    [<Fact>]
    let ``MoveSelectedDown pinned at the bottom is a no-op`` () =
        let before = editorOf threeSingles |> applyAll [ SelectLayer (AtSingleLayer 2) ]
        let after = applyAll [ MoveSelectedDown ] before
        Assert.Equal(before.structure, after.structure)
        Assert.Equal<Set<LayerPosition>>(before.selection, after.selection)

    [<Fact>]
    let ``MoveSelectedDown moves the selected layer one slot down`` () =
        let state =
            editorOf threeSingles
            |> applyAll [ SelectLayer (AtSingleLayer 0); MoveSelectedDown ]
        let materials = state.structure.expandedFilms |> List.map (fun l -> l.materialId)
        Assert.Equal<MaterialId list>([ MaterialIds.vacuum; MaterialIds.glass152; MaterialIds.glass152 ], materials)
        Assert.Equal<Set<LayerPosition>>(Set.ofList [ AtSingleLayer 1 ], state.selection)

    [<Fact>]
    let ``MoveSelectedUp within a period cell reorders every repetition`` () =
        let state =
            editorOf withGroup
            |> applyAll [ SelectLayer (AtCellLayer (0, 1)); MoveSelectedUp ]
        // Every period now starts with the vacuum layer.
        let expandedMaterials = state.structure.expandedFilms |> List.map (fun l -> l.materialId)
        Assert.Equal<MaterialId list>(
            [
                MaterialIds.vacuum; MaterialIds.glass152
                MaterialIds.vacuum; MaterialIds.glass152
                MaterialIds.vacuum; MaterialIds.glass152
                MaterialIds.glass175
            ],
            expandedMaterials)
        Assert.Equal<Set<LayerPosition>>(Set.ofList [ AtCellLayer (0, 0) ], state.selection)

    [<Fact>]
    let ``MoveSelectedUp moves a single layer past a whole period group as one unit`` () =
        let state =
            editorOf withGroup
            |> applyAll [ SelectLayer (AtSingleLayer 1); MoveSelectedUp ]
        match state.structure.films with
        | [ SingleLayer single; Repeated group ] ->
            Assert.Equal(MaterialIds.glass175, single.materialId)
            Assert.Equal(3, group.count)
        | films -> failwith $"unexpected films shape: %A{films}"
        Assert.Equal<Set<LayerPosition>>(Set.ofList [ AtSingleLayer 0 ], state.selection)

    // ============================== MakeRepeatBlock ==============================

    [<Fact>]
    let ``MakeRepeatBlock over 2 selected layers with count K expands to 2*K films`` () =
        // The slice's first acceptance criterion.
        let twoLayers : SampleStructure =
            { threeSingles with
                films =
                    [
                        SingleLayer (layerOf MaterialIds.glass152 100.0)
                        SingleLayer (layerOf MaterialIds.vacuum 50.0)
                    ] }
        let k = 7
        let state =
            editorOf twoLayers
            |> applyAll [ SelectLayer (AtSingleLayer 0); SelectLayer (AtSingleLayer 1); MakeRepeatBlock k ]
        let expanded = state.structure.expandedFilms
        Assert.Equal(2 * k, List.length expanded)
        // The fold preserves the cell order across every period.
        expanded
        |> List.chunkBySize 2
        |> List.iter (fun period ->
            Assert.Equal<MaterialId list>(
                [ MaterialIds.glass152; MaterialIds.vacuum ],
                period |> List.map (fun l -> l.materialId)))
        Assert.Equal<Set<LayerPosition>>(Set.empty, state.selection)

    [<Fact>]
    let ``MakeRepeatBlock folds in place, leaving unselected neighbours around the group`` () =
        let state =
            editorOf threeSingles
            |> applyAll [ SelectLayer (AtSingleLayer 0); SelectLayer (AtSingleLayer 1); MakeRepeatBlock 3 ]
        match state.structure.films with
        | [ Repeated group; SingleLayer trailing ] ->
            Assert.Equal(3, group.count)
            Assert.Equal(2, List.length group.cell)
            Assert.Equal(MaterialIds.glass152, trailing.materialId)
        | films -> failwith $"unexpected films shape: %A{films}"
        Assert.Equal(2 * 3 + 1, List.length state.structure.expandedFilms)

    [<Fact>]
    let ``MakeRepeatBlock rejects a count below 1 with the typed error`` () =
        let selected = editorOf threeSingles |> applyAll [ SelectLayer (AtSingleLayer 0); SelectLayer (AtSingleLayer 1) ]
        match expectError (MakeRepeatBlock 0) selected with
        | InvalidRepeatCount _ -> ()
        | e -> failwith $"expected InvalidRepeatCount, got %A{e}"
        match expectError (MakeRepeatBlock -3) selected with
        | InvalidRepeatCount _ -> ()
        | e -> failwith $"expected InvalidRepeatCount, got %A{e}"

    [<Fact>]
    let ``MakeRepeatBlock rejects a non-contiguous selection`` () =
        let selected = editorOf threeSingles |> applyAll [ SelectLayer (AtSingleLayer 0); SelectLayer (AtSingleLayer 2) ]
        match expectError (MakeRepeatBlock 2) selected with
        | SelectionNotFoldable _ -> ()
        | e -> failwith $"expected SelectionNotFoldable, got %A{e}"

    [<Fact>]
    let ``MakeRepeatBlock rejects an empty selection`` () =
        match expectError (MakeRepeatBlock 2) (editorOf threeSingles) with
        | SelectionNotFoldable _ -> ()
        | e -> failwith $"expected SelectionNotFoldable, got %A{e}"

    [<Fact>]
    let ``MakeRepeatBlock rejects a selection holding period-cell layers`` () =
        let selected = editorOf withGroup |> applyAll [ SelectLayer (AtCellLayer (0, 0)) ]
        match expectError (MakeRepeatBlock 2) selected with
        | SelectionNotFoldable _ -> ()
        | e -> failwith $"expected SelectionNotFoldable, got %A{e}"

    // ============================== SetRepeatCount ==============================

    [<Fact>]
    let ``SetRepeatCount adds or removes whole periods`` () =
        let grown = editorOf withGroup |> applyAll [ SetRepeatCount (0, 5) ]
        Assert.Equal(5 * 2 + 1, List.length grown.structure.expandedFilms)
        let shrunk = editorOf withGroup |> applyAll [ SetRepeatCount (0, 1) ]
        Assert.Equal(1 * 2 + 1, List.length shrunk.structure.expandedFilms)

    [<Fact>]
    let ``SetRepeatCount rejects a count below 1 with the typed error — the validateRepeatCount rule`` () =
        match expectError (SetRepeatCount (0, 0)) (editorOf withGroup) with
        | InvalidRepeatCount _ -> ()
        | e -> failwith $"expected InvalidRepeatCount, got %A{e}"
        // The SAME rule the Ui boundary enforces: validateRepeatCount rejects 0, accepts 1.
        match OpticalConstructor.Ui.Validation.validateRepeatCount 0 with
        | Error _ -> ()
        | Ok c -> failwith $"Ui validateRepeatCount unexpectedly accepted %d{c}"
        match OpticalConstructor.Ui.Validation.validateRepeatCount 1 with
        | Ok _ -> ()
        | Error e -> failwith $"Ui validateRepeatCount unexpectedly rejected 1: %A{e}"
        let accepted = editorOf withGroup |> applyAll [ SetRepeatCount (0, 1) ]
        Assert.Equal(3, List.length accepted.structure.expandedFilms)

    [<Fact>]
    let ``SetRepeatCount aimed at a non-group position is rejected with the typed error`` () =
        match expectError (SetRepeatCount (1, 2)) (editorOf withGroup) with
        | NotARepeatGroup _ -> ()
        | e -> failwith $"expected NotARepeatGroup, got %A{e}"
        match expectError (SetRepeatCount (7, 2)) (editorOf withGroup) with
        | NotARepeatGroup _ -> ()
        | e -> failwith $"expected NotARepeatGroup, got %A{e}"
