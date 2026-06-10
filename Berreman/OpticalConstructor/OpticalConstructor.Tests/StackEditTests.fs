namespace OpticalConstructor.Tests

open Berreman.Constants
open Berreman.Geometry
open Berreman.Fields
open Berreman.MaterialProperties
open Berreman.Media
open Berreman.Solvers
open OpticalConstructor.Domain.Units
open OpticalConstructor.Domain.BeamTree
open OpticalConstructor.Domain.Project
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Ui
open Xunit

/// Part B construction-layer tests (slice 005). Exercise the Avalonia-free pure
/// cores (P3): the stack/layer editor transforms (AC-B5), the medium selectors
/// (AC-B6), the three-state substrate switch + reused-solver routing (AC-B7),
/// per-node solving through the reused engine solver (AC-B1), and the
/// construction-page UX — live descendant-count gate, node-granularity busy,
/// results refresh, and single-level undo (AC-B10).
module StackEditTests =

    let private glass = OpticalProperties.fromRefractionIndex (RefractionIndex 1.5)

    let private mkLayer (t : float<nm>) : Layer =
        { properties = OpticalProperties.vacuum; thickness = Thickness.nm t }

    // Distinct layer instances so reference identity proves copy-not-mutate.
    let private l0 = mkLayer 100.0<nm>
    let private l1 = mkLayer 200.0<nm>
    let private l2 = mkLayer 300.0<nm>

    let private baseSystem : OpticalSystem =
        {
            description = None
            upper = OpticalProperties.vacuum
            films = [ l0; l1; l2 ]
            substrate = None
            lower = OpticalProperties.vacuum
        }

    let private light = IncidentLightInfo.create (WaveLength.nm 600.0<nm>)
    let private refEq (a : obj) (b : obj) = System.Object.ReferenceEquals(a, b)
    let private eNorm (f : EmField) : float = f.e.value.norm

    // ---------------------------------------------------------------- AC-B5

    [<Fact>]
    let ``AC-B5 add appends a new film and reuses the existing Layer records`` () =
        let newL = mkLayer 400.0<nm>
        let r = StackEditor.addLayer newL baseSystem
        Assert.Equal(4, List.length r.films)
        Assert.True(refEq r.films.[0] l0 && refEq r.films.[1] l1 && refEq r.films.[2] l2)
        Assert.True(refEq r.films.[3] newL)
        // The engine OpticalSystem/films are unmutated: the source is a new value.
        Assert.Equal(3, List.length baseSystem.films)
        Assert.False(refEq r baseSystem)

    [<Fact>]
    let ``AC-B5 delete removes the indexed film, preserving the others by identity`` () =
        let r = StackEditor.deleteLayer 1 baseSystem
        Assert.Equal<Layer list>([ l0; l2 ], r.films)
        Assert.Equal(3, List.length baseSystem.films)

    [<Fact>]
    let ``AC-B5 reorder moves a film to the target index`` () =
        let r = StackEditor.reorderLayer 0 2 baseSystem
        Assert.Equal<Layer list>([ l1; l2; l0 ], r.films)

    [<Fact>]
    let ``AC-B5 duplicate inserts an identical record below the source`` () =
        let r = StackEditor.duplicateLayer 0 baseSystem
        Assert.Equal<Layer list>([ l0; l0; l1; l2 ], r.films)

    [<Fact>]
    let ``AC-B5 group gathers the selection contiguously at the first index`` () =
        let r = StackEditor.groupLayers [ 0; 2 ] baseSystem
        Assert.Equal<Layer list>([ l0; l2; l1 ], r.films)

    [<Fact>]
    let ``AC-B5 rotate-layer reuses the engine Layer.rotate member`` () =
        let r = StackEditor.rotateLayer 1 Rotation.rotatePiX baseSystem
        // Reuses Media.fs:30 Layer.rotate — the rotated row equals the engine member.
        Assert.Equal(l1.rotate Rotation.rotatePiX, r.films.[1])
        Assert.True(refEq r.films.[0] l0)

    // ---------------------------------------------------------------- AC-B6

    [<Fact>]
    let ``AC-B6 incident & exit medium selectors set upper/lower, preserving films and substrate`` () =
        let inc = StackEditor.setIncidentMedium glass baseSystem
        Assert.True(refEq inc.upper glass)
        Assert.True(refEq inc.films baseSystem.films)
        Assert.Equal(None, inc.substrate)
        let ex = StackEditor.setExitMedium glass baseSystem
        Assert.True(refEq ex.lower glass)
        Assert.True(refEq ex.films baseSystem.films)

    [<Fact>]
    let ``AC-B6 a medium resolves by material-library reference via the slice-004 seam`` () =
        match StackEditor.mediumFromMaterial standard "silicon" (WaveLength.nm 600.0<nm>) with
        | Ok props ->
            let r = StackEditor.setIncidentMedium props baseSystem
            Assert.True(refEq r.upper props)
        | Error e -> failwith $"expected a resolved medium, got {e}"
        match StackEditor.mediumFromMaterial standard "no-such-id" (WaveLength.nm 600.0<nm>) with
        | Error (UnknownMaterialId "no-such-id") -> ()
        | other -> failwith $"expected UnknownMaterialId, got {other}"

    // ---------------------------------------------------------------- AC-B7

    [<Fact>]
    let ``AC-B7 the substrate switch sets None / Some Plate / Some Wedge`` () =
        Assert.Equal(None, (StackEditor.applySubstrate StackEditor.AsThinFilm baseSystem).substrate)

        let plate = StackEditor.applySubstrate (StackEditor.AsPlate l0) baseSystem
        match plate.substrate with
        | Some (Plate p) -> Assert.True(refEq p l0)
        | other -> failwith $"expected Some (Plate _), got {other}"

        let wedgeLayer = { layer = l0; angle = WedgeAngle (Angle.degree 3.0) }
        let wedge = StackEditor.applySubstrate (StackEditor.AsWedge wedgeLayer) baseSystem
        match wedge.substrate with
        | Some (Wedge w) -> Assert.Equal(WedgeAngle (Angle.degree 3.0), w.angle)
        | other -> failwith $"expected Some (Wedge _), got {other}"
        // films/upper/lower preserved through every switch state.
        Assert.True(refEq wedge.films baseSystem.films)

    [<Fact>]
    let ``AC-B7 re-solving a plate substrate routes through the existing multi-reflection branch`` () =
        let plateSys =
            { baseSystem with
                films = []
                substrate = Some (Plate { properties = glass; thickness = Thickness.mm 1.0<mm> })
                lower = OpticalProperties.vacuum }
        // The substrate switch does NOT change how internal reflections are computed:
        // OpticalSystemSolver branches on system.substrate and yields a Multiple solution.
        match OpticalSystemSolver(light, plateSys).solution with
        | Multiple _ -> ()
        | Single _ -> Assert.Fail("a Plate substrate MUST route into the multi-reflection branch")
        // The page's per-node solve goes through that same reused branch.
        let node = { element = Sample plateSys; system = plateSys; incident = light; children = Map.empty; defaultUnit = Nanometer }
        let viaPage = ConstructionPage.solveNode node
        let direct = OpticalSystemSolver(light, plateSys).solution.emSys
        Assert.Equal(eNorm direct.reflected, eNorm viaPage.reflected, 12)
        Assert.Equal(eNorm direct.transmitted, eNorm viaPage.transmitted, 12)

    // ---------------------------------------------------------------- AC-B1

    [<Fact>]
    let ``AC-B1 a node solve obtains reflected/transmitted via the reused OpticalSystemSolver`` () =
        let sampleSys = { baseSystem with films = []; lower = glass }
        let node = { element = Sample sampleSys; system = sampleSys; incident = light; children = Map.empty; defaultUnit = Nanometer }
        let ems = ConstructionPage.solveNode node
        let direct = OpticalSystemSolver(light, sampleSys).solution.emSys
        Assert.Equal(eNorm direct.reflected, eNorm ems.reflected, 12)
        Assert.Equal(eNorm direct.transmitted, eNorm ems.transmitted, 12)
        Assert.False(List.isEmpty ems.reflected.emComponents)
        Assert.False(List.isEmpty ems.transmitted.emComponents)

    // ---------------------------------------------------------------- AC-B10

    let private mkNode element children : BeamNode =
        { element = element; system = baseSystem; incident = light; children = children; defaultUnit = Nanometer }

    /// root ─Reflected→ mirror(leaf); root ─Transmitted→ sample ─Transmitted→ detector.
    let private project : OpticalConstructorProject =
        let detector = mkNode Detector Map.empty
        let sample = mkNode (Sample baseSystem) (Map.ofList [ BeamBranch.Transmitted, detector ])
        let mirror = mkNode FlatMirror Map.empty
        let root = mkNode (Sample baseSystem) (Map.ofList [ BeamBranch.Reflected, mirror; BeamBranch.Transmitted, sample ])
        { beamTree = { root = root }; systems = [ baseSystem ]; sources = []; placements = []; table = OpticalConstructor.Domain.Table.defaultTable }

    let private model0 = ConstructionPage.init project "C:\\projects\\demo" "demo"

    [<Fact>]
    let ``AC-B10 descendant count is live-derived from the current tree`` () =
        Assert.Equal(3, ConstructionPage.descendantCount project.beamTree.root)
        let sample = project.beamTree.root.children.[BeamBranch.Transmitted]
        Assert.Equal(1, ConstructionPage.descendantCount sample)

    [<Fact>]
    let ``AC-B10 deleting a node with children opens a gate stating the live count, then confirms`` () =
        let path = [ BeamBranch.Transmitted ]   // the sample sub-tree (1 descendant)
        let gated = ConstructionPage.update (ConstructionPage.RequestDeleteNode path) model0
        match gated.pendingDeletion with
        | Some (p, count) -> Assert.Equal<BeamBranch list>(path, p); Assert.Equal(1, count)
        | None -> Assert.Fail("a node with children MUST open the pre-confirmation gate")
        Assert.Equal(Some "Delete this node and its 1 descendant node(s)?", ConstructionPage.confirmationPrompt gated)
        // The tree is untouched until confirmed.
        Assert.True(gated.project.beamTree.root.children.ContainsKey BeamBranch.Transmitted)
        // Confirm removes the sub-tree and arms undo.
        let deleted = ConstructionPage.update ConstructionPage.ConfirmDeleteNode gated
        Assert.False(deleted.project.beamTree.root.children.ContainsKey BeamBranch.Transmitted)
        Assert.Equal(None, deleted.pendingDeletion)
        Assert.True(ConstructionPage.canUndo deleted)
        Assert.Equal(1, ConstructionPage.descendantCount deleted.project.beamTree.root)

    [<Fact>]
    let ``AC-B10 cancelling the gate leaves the tree unchanged`` () =
        let gated = ConstructionPage.update (ConstructionPage.RequestDeleteNode [ BeamBranch.Transmitted ]) model0
        let cancelled = ConstructionPage.update ConstructionPage.CancelDeleteNode gated
        Assert.Equal(None, cancelled.pendingDeletion)
        Assert.True(cancelled.project.beamTree.root.children.ContainsKey BeamBranch.Transmitted)

    [<Fact>]
    let ``AC-B10 a childless node is deleted one-click without a gate`` () =
        let deleted = ConstructionPage.update (ConstructionPage.RequestDeleteNode [ BeamBranch.Reflected ]) model0
        Assert.Equal(None, deleted.pendingDeletion)
        Assert.False(deleted.project.beamTree.root.children.ContainsKey BeamBranch.Reflected)
        Assert.True(ConstructionPage.canUndo deleted)

    [<Fact>]
    let ``AC-B10 the root node is not deletable from the page`` () =
        let r = ConstructionPage.update (ConstructionPage.RequestDeleteNode []) model0
        Assert.Equal(2, r.project.beamTree.root.children.Count)
        Assert.Equal(None, r.pendingDeletion)

    [<Fact>]
    let ``AC-B10 a structural edit marks the node busy then refreshes results from the new EmFieldSystem`` () =
        let path = [ BeamBranch.Transmitted ]
        // Edit (add a layer to the sample sub-tree) → the affected node is busy.
        let edited = ConstructionPage.update (ConstructionPage.EditStack (path, StackEditor.AddLayer (mkLayer 500.0<nm>))) model0
        Assert.True(ConstructionPage.isNodeBusy path edited)
        // A short flat-element solve must not block the whole tree.
        Assert.False(ConstructionPage.isNodeBusy [ BeamBranch.Reflected ] edited)
        // The edit produced a new films list on the addressed node (immutable update).
        let editedSample = edited.project.beamTree.root.children.[BeamBranch.Transmitted]
        Assert.Equal(4, List.length editedSample.system.films)
        // Re-solve the affected sub-tree and complete → busy clears, results refresh.
        let solved = ConstructionPage.solveSubtree path edited.project.beamTree.root
        let refreshed = ConstructionPage.update (ConstructionPage.NodeSolved solved) edited
        Assert.False(ConstructionPage.isNodeBusy path refreshed)
        Assert.True(refreshed.results.ContainsKey path)
        // The refreshed result is the EmFieldSystem of the EDITED node (no manual reload).
        let expected = ConstructionPage.solveNode editedSample
        Assert.Equal(eNorm expected.transmitted, eNorm refreshed.results.[path].transmitted, 12)

    [<Fact>]
    let ``AC-B10 single-level undo restores the immediately prior immutable tree`` () =
        let path = [ BeamBranch.Transmitted ]
        let edited = ConstructionPage.update (ConstructionPage.EditStack (path, StackEditor.AddLayer (mkLayer 500.0<nm>))) model0
        let undone = ConstructionPage.update ConstructionPage.Undo edited
        let sample = undone.project.beamTree.root.children.[BeamBranch.Transmitted]
        Assert.Equal(3, List.length sample.system.films)
        Assert.False(ConstructionPage.canUndo undone)

    [<Fact>]
    let ``AC-B10 the opposing attach action reuses BeamNode.attach validation`` () =
        // Attach a detector on the mirror's Reflected branch — accepted.
        let detector = mkNode Detector Map.empty
        let attached = ConstructionPage.update (ConstructionPage.AttachChild ([ BeamBranch.Reflected ], BeamBranch.Reflected, detector)) model0
        Assert.True(attached.project.beamTree.root.children.[BeamBranch.Reflected].children.ContainsKey BeamBranch.Reflected)
        // A mirror rejects a Transmitted attach (§B.3) → the tree is unchanged.
        let rejected = ConstructionPage.update (ConstructionPage.AttachChild ([ BeamBranch.Reflected ], BeamBranch.Transmitted, detector)) model0
        Assert.True(rejected.project.beamTree.root.children.[BeamBranch.Reflected].children.IsEmpty)

    // ------------------------------------------------- §B.10 items 1 & 6

    [<Fact>]
    let ``B10-1 the construction page is a default-landing nav entry`` () =
        Assert.True(ConstructionPage.navEntry.isDefaultLanding)
        Assert.False(System.String.IsNullOrWhiteSpace ConstructionPage.navEntry.title)

    // ---------------------------------------------------------------- AC-J4
    // §J.4 drag-drop of a library material onto a layer row (slice 014): a drop
    // emits a single MVU message the owning update applies immutably, replacing
    // the layer's properties with the RESOLVED OpticalProperties while leaving
    // thickness unchanged; resolution goes through the slice-004 resolveMaterial.

    [<Fact>]
    let ``AC-J4 a material dropped on a layer row replaces its properties via the slice-004 resolver`` () =
        let w = WaveLength.nm 600.0<nm>
        // The drop payload carries only the materialEntry id; resolution is the
        // slice-004 by-id seam producing the StackMsg the owning update applies.
        let resolved = StackEditor.mediumFromMaterial standard "silicon" w |> function Ok p -> p | Error e -> failwith $"{e}"
        match StackEditor.layerMaterialDrop standard w 1 "silicon" with
        | Ok (StackEditor.SetLayerMaterial (idx, props)) ->
            Assert.Equal(1, idx)
            // The message carries the resolveMaterial-resolved OpticalProperties (no re-resolution).
            Assert.Equal(resolved, props)
            // The owning update applies it immutably: a NEW system, layer 1's
            // properties replaced, its thickness and the other layers unchanged.
            let r = StackEditor.applyStackMsg (StackEditor.SetLayerMaterial (idx, props)) baseSystem
            Assert.Equal(resolved, r.films.[1].properties)
            Assert.Equal(l1.thickness, r.films.[1].thickness)
            Assert.True(refEq r.films.[0] l0 && refEq r.films.[2] l2)
            Assert.False(refEq r baseSystem)
            Assert.Equal(3, List.length baseSystem.films)   // source untouched
        | other -> failwith $"expected Ok (SetLayerMaterial _), got {other}"

    [<Fact>]
    let ``AC-J4 dropping an unknown material id is a no-op error, never throwing`` () =
        match StackEditor.layerMaterialDrop standard (WaveLength.nm 600.0<nm>) 0 "no-such-id" with
        | Error (UnknownMaterialId "no-such-id") -> ()
        | other -> failwith $"expected UnknownMaterialId, got {other}"
        // setLayerMaterial on an out-of-range index leaves the system unchanged.
        let r = StackEditor.setLayerMaterial 99 glass baseSystem
        Assert.True(refEq r.films baseSystem.films)

    [<Fact>]
    let ``B10-6 the committable project file defaults to the working folder as JSON, never .binz`` () =
        let path = ConstructionPage.projectFilePath model0
        Assert.StartsWith("C:\\projects\\demo", path)
        Assert.EndsWith(".json", path)
        Assert.DoesNotContain(".binz", path)
        match ConstructionPage.saveProject model0 with
        | Ok (p, json) ->
            Assert.EndsWith(".json", p)
            Assert.DoesNotContain(".binz", json)
            Assert.StartsWith("{", json.TrimStart())
        | Error e -> failwith $"unexpected save error: {e}"
