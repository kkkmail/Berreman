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
/// (AC-B6), the three-state substrate switch + reused-solver routing (AC-B7), and
/// the material-drop message path (AC-J4). The construction-page UX tests this
/// module also carried (AC-B1/AC-B10) were retired with the `ConstructionPage`
/// module (spec 0038 Part B.1).
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
        match StackEditor.mediumFromMaterial standard MaterialIds.silicon (WaveLength.nm 600.0<nm>) with
        | Ok props ->
            let r = StackEditor.setIncidentMedium props baseSystem
            Assert.True(refEq r.upper props)
        | Error e -> failwith $"expected a resolved medium, got {e}"
        let unknown = MaterialId.create ()
        match StackEditor.mediumFromMaterial standard unknown (WaveLength.nm 600.0<nm>) with
        | Error (UnknownMaterialId reason) -> Assert.Contains(string unknown.value, reason)
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
        let resolved = StackEditor.mediumFromMaterial standard MaterialIds.silicon w |> function Ok p -> p | Error e -> failwith $"{e}"
        match StackEditor.layerMaterialDrop standard w 1 MaterialIds.silicon with
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
        let unknown = MaterialId.create ()
        match StackEditor.layerMaterialDrop standard (WaveLength.nm 600.0<nm>) 0 unknown with
        | Error (UnknownMaterialId reason) -> Assert.Contains(string unknown.value, reason)
        | other -> failwith $"expected UnknownMaterialId, got {other}"
        // setLayerMaterial on an out-of-range index leaves the system unchanged.
        let r = StackEditor.setLayerMaterial 99 glass baseSystem
        Assert.True(refEq r.films baseSystem.films)
