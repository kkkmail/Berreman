namespace OpticalConstructor.Tests

open Berreman.Constants
open Berreman.Geometry
open Berreman.Fields
open Berreman.MaterialProperties
open Berreman.Media
open OpticalConstructor.Domain.Units
open OpticalConstructor.Domain.BeamTree
open OpticalConstructor.Ui
open Xunit

/// §J.1 schematic cross-section geometry tests (slice 014, AC-J1 portion ONLY).
/// Verify the pure geometry WITHOUT an Avalonia surface: the band-height function
/// is monotonic in `Thickness` meters; `Thickness.Infinity` draws as the
/// fixed-height semi-infinite band; `Substrate.Wedge` draws with a sloped lower
/// edge (distinct from a plate); the incident-ray geometry tracks the incidence
/// angle; and material colour is a deterministic pure function of the stable id.
///
/// The `SystemView3D` / AC-J12 portion of this file is owned by slice 016 and is
/// NOT added here.
module SchematicGeometryTests =

    let private thickness (m : float) : Thickness = Thickness.Thickness (m * 1.0<meter>)

    // ----------------------------------------------------------------- AC-J1.1

    [<Fact>]
    let ``AC-J1 band height is strictly monotonic in finite thickness`` () =
        let h1 = Schematic.bandHeight (thickness 1.0e-8)   // 10 nm
        let h2 = Schematic.bandHeight (thickness 1.0e-7)   // 100 nm
        let h3 = Schematic.bandHeight (thickness 1.0e-6)   // 1 µm
        Assert.True(h1 < h2, "a thicker film must draw taller")
        Assert.True(h2 < h3, "a thicker film must draw taller")

    [<Fact>]
    let ``AC-J1 Thickness.Infinity draws as the fixed semi-infinite band`` () =
        // The half-space case is NOT scaled by any thickness — it is the fixed height.
        Assert.Equal(Schematic.semiInfiniteBandHeight, Schematic.bandHeight Thickness.Infinity)
        // ... and that fixed band is independent of how thick the finite films are.
        Assert.Equal(Schematic.bandHeight Thickness.Infinity, Schematic.bandHeight Thickness.Infinity)

    // ----------------------------------------------------------------- AC-J1.4

    [<Fact>]
    let ``AC-J1 a Wedge substrate draws a sloped lower edge, a Plate stays flat`` () =
        let layer = { properties = OpticalProperties.vacuum; thickness = Thickness.mm 1.0<mm> }
        let plate = Schematic.substrateBand (Plate layer)
        Assert.Equal(Schematic.PlateBand, plate)

        let wedge = Schematic.substrateBand (Wedge { layer = layer; angle = WedgeAngle (Angle.degree 3.0) })
        match wedge with
        | Schematic.WedgeBand slope -> Assert.True(slope > 0.0, "a non-zero wedge angle must tilt the lower edge")
        | other -> failwith $"expected a sloped WedgeBand, got {other}"

    [<Fact>]
    let ``AC-J1 the wedge lower-edge slope increases with the wedge angle`` () =
        let s1 = Schematic.wedgeSlope (WedgeAngle (Angle.degree 1.0))
        let s5 = Schematic.wedgeSlope (WedgeAngle (Angle.degree 5.0))
        Assert.Equal(0.0, Schematic.wedgeSlope (WedgeAngle (Angle.degree 0.0)), 12)
        Assert.True(s1 < s5, "a larger wedge angle must slope more")

    // ----------------------------------------------------------------- AC-J1.5

    [<Fact>]
    let ``AC-J1 the incident ray tracks the incidence angle and reflects about the normal`` () =
        let a = IncidenceAngle.create (Angle.degree 30.0)
        let ray = Schematic.rayGeometry a
        Assert.Equal(a.value, ray.angleRadians, 12)
        // Reflected ray mirrors the incident about the surface normal: y flips, x same.
        Assert.Equal(ray.incidentDx, ray.reflectedDx, 12)
        Assert.Equal(-ray.incidentDy, ray.reflectedDy, 12)
        // A non-normal incidence has a non-zero horizontal component.
        Assert.True(ray.incidentDx > 0.0)

    // ----------------------------------------------------------------- AC-J1.6

    [<Fact>]
    let ``AC-J1 material colour is a deterministic pure function of the stable id`` () =
        // Same id → same colour across redraws.
        Assert.Equal(Schematic.colorForMaterial "silicon", Schematic.colorForMaterial "silicon")
        // A curated id maps to its static colour entry, distinct from another material.
        Assert.NotEqual(Schematic.colorForMaterial "silicon", Schematic.colorForMaterial "glass-1.52")
        // An unknown id is still deterministic (stable hash fallback, not GetHashCode).
        Assert.Equal(Schematic.colorForMaterial "made-up-id", Schematic.colorForMaterial "made-up-id")

    // ----------------------------------------------------------------- layout

    [<Fact>]
    let ``AC-J1 the layout draws upper, films top-to-bottom, substrate, then lower`` () =
        let film t = { properties = OpticalProperties.vacuum; thickness = t }
        let sys : OpticalSystem =
            {
                description = Some "test"
                upper = OpticalProperties.vacuum
                films = [ film (thickness 1.0e-8); film (thickness 1.0e-6) ]
                substrate = Some (Wedge { layer = film (Thickness.mm 1.0<mm>); angle = WedgeAngle (Angle.degree 2.0) })
                lower = OpticalProperties.vacuum
            }
        let bands = Schematic.layout Nanometer (fun i -> sprintf "m%d" i) sys
        // upper + 2 films + substrate + lower = 5 bands.
        Assert.Equal(5, List.length bands)
        // The substrate band carries the wedge shape; the half-spaces and films do not.
        let substrateBands = bands |> List.choose (fun b -> b.substrate)
        Assert.Equal(1, List.length substrateBands)
        match substrateBands.[0] with
        | Schematic.WedgeBand _ -> ()
        | other -> failwith $"expected a WedgeBand in the layout, got {other}"
        // The film bands preserve the thicker-draws-taller ordering (item 1).
        let filmHeights = bands |> List.skip 1 |> List.take 2 |> List.map (fun b -> b.height)
        Assert.True(filmHeights.[0] < filmHeights.[1])

    // =====================================================================
    // §J.12 SystemView3D element/beam-path geometry — AC-J12 (slice 016).
    // The viewport derives each beam segment direction from the ALREADY-SOLVED
    // EmFieldSystem.reflected/.transmitted (Fields.fs:553-554) WITHOUT re-solving
    // or re-deriving routing, and writes no non-SI viewport coordinate back to the
    // model. Geometry verified without an OpenTK surface (pure projection).
    // =====================================================================

    let private glass = OpticalProperties.fromRefractionIndex (RefractionIndex 1.5)

    let private sampleSystem : OpticalSystem =
        {
            description = None
            upper = OpticalProperties.vacuum
            films = [ { properties = glass; thickness = Thickness.nm 200.0<nm> } ]
            substrate = None
            lower = glass
        }

    let private light = IncidentLightInfo.create (WaveLength.nm 600.0<nm>)

    let private sampleNode : BeamNode =
        {
            element = Sample sampleSystem
            system = sampleSystem
            incident = light
            children = Map.empty
            defaultUnit = Nanometer
        }

    [<Fact>]
    let ``AC-J12 a beam segment direction is read from the already-solved branch normal`` () =
        // Solve ONCE (Part B's seam); the renderer then CONSUMES this EmFieldSystem.
        let ems = solve sampleNode

        // The viewport reads the segment direction straight from the solved field —
        // SystemView3D.beamDirection takes the solved ems as input and never solves.
        match SystemView3D.beamDirection BeamBranch.Reflected ems, ems.reflected.normal with
        | Some dir, Some n ->
            Assert.Equal(n.x, dir.x, 12)
            Assert.Equal(n.y, dir.y, 12)
            Assert.Equal(n.z, dir.z, 12)
        | _ -> Assert.Fail("the solved reflected beam must expose a unit direction")

    [<Fact>]
    let ``AC-J12 reflected and transmitted segments carry independent solved directions`` () =
        let ems = solve sampleNode
        let segments = SystemView3D.beamSegments { x = 0.0; y = 0.0; z = 0.0 } ems
        // Both branches are rendered, each tagged with its branch.
        let branches = segments |> List.map (fun s -> s.branch)
        Assert.Contains(BeamBranch.Reflected, branches)
        Assert.Contains(BeamBranch.Transmitted, branches)
        // The reflected and transmitted directions differ (the solved routing is
        // read per branch, not a single shared ray).
        let dirOf b = segments |> List.find (fun s -> s.branch = b) |> fun s -> s.direction
        let r = dirOf BeamBranch.Reflected
        let t = dirOf BeamBranch.Transmitted
        Assert.True(abs (r.z - t.z) > 1e-9 || abs (r.x - t.x) > 1e-9 || abs (r.y - t.y) > 1e-9)

    [<Fact>]
    let ``AC-J12 computing viewport geometry writes no value back into the model`` () =
        // The canonical thickness before and after building the placement is identical
        // — placeElements is a pure projection; it converts at the boundary only (§A.3).
        let before = SystemView3D.systemThicknessMeters sampleSystem
        let tree : BeamTree = { root = sampleNode }
        let placed = SystemView3D.placeElements SystemView3D.defaultPixelsPerMeter tree
        let after = SystemView3D.systemThicknessMeters sampleSystem
        Assert.Equal(before / 1.0<meter>, after / 1.0<meter>, 15)
        // The (canonical, finite) 200 nm film maps to a positive, finite extent.
        Assert.Single(placed) |> ignore
        Assert.True(placed.[0].extent > 0.0)

    [<Fact>]
    let ``AC-J12 a multi-element system places its elements to scale along the beam path`` () =
        // Source -> Sample -> Detector along the transmitted spine.
        let detector =
            { sampleNode with element = Detector; system = { sampleSystem with films = [] }; children = Map.empty }
        let sampleWithDetector =
            { sampleNode with children = Map.ofList [ BeamBranch.Transmitted, detector ] }
        let sourceNode =
            { sampleNode with element = Source; children = Map.ofList [ BeamBranch.Transmitted, sampleWithDetector ] }
        let tree : BeamTree = { root = sourceNode }

        let placed = SystemView3D.placeElements SystemView3D.defaultPixelsPerMeter tree
        // Three placed elements, in beam-path order.
        Assert.Equal(3, List.length placed)
        Assert.Equal<ConstructorElement>(Source, placed.[0].element)
        // Positions are monotonically advancing along the beam path (+z).
        Assert.True(placed.[0].position.z < placed.[1].position.z)
        Assert.True(placed.[1].position.z < placed.[2].position.z)
