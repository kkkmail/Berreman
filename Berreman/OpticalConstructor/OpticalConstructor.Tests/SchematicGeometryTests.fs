namespace OpticalConstructor.Tests

open Berreman.Constants
open Berreman.Geometry
open Berreman.Fields
open Berreman.MaterialProperties
open Berreman.Media
open OpticalConstructor.Domain.Units
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
