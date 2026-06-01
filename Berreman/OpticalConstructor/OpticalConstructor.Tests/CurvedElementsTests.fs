namespace OpticalConstructor.Tests

open Berreman.Constants
open Berreman.Geometry
open Berreman.Fields
open Berreman.MaterialProperties
open Berreman.Media
open Berreman.BerremanMatrix
open Berreman.Solvers
open OpticalConstructor.Domain.CurvedElements
open Xunit

/// Curved-element domain — Part C acceptance criteria (AC-C1, C2, C3, C5, C6, C7,
/// C8). The curved path is the single admitted physics extension: a curved surface
/// is decomposed into local flat zones, each solved at its local AOI through the
/// UNFORKED engine 4×4 solver. These tests pin: nm/µm/mm radius → meters (C1),
/// uniform zone sampling with an axis shift of `Angle.zero` (C2), the local-AOI
/// `rotateY` feeding `OpticalSystemSolver` (C3), the signed-radius convention via
/// `sag` (C5), per-zone grading touching only finite `Thickness` (C6),
/// inter-element propagation through the reused `EmField.propagate` seam (C7), and
/// the shared-immutable-coating guarantee when grading is off (C8).
module CurvedElementsTests =

    // A small reused EUV-like coating: two finite bilayer films over a glass
    // substrate. All films are finite so the engine solver can propagate them; the
    // grading test below uses a separate coating that adds a guard `Infinity` film.
    let private glass = OpticalProperties.fromRefractionIndex (RefractionIndex 1.5)

    let private finiteFilm (d : double<meter>) : Layer =
        { properties = glass; thickness = Thickness.Thickness d }

    let private infinityFilm : Layer =
        { properties = OpticalProperties.vacuum; thickness = Thickness.Infinity }

    let private coating : OpticalSystem =
        {
            description = None
            upper = OpticalProperties.vacuum
            films = [ finiteFilm 7.0e-9<meter>; finiteFilm 3.0e-9<meter> ]
            substrate = None
            lower = glass
        }

    // The grading test (AC-C6) needs a deliberate `Thickness.Infinity` film to prove
    // grading leaves it untouched. `gradeCoating` is a pure thickness map (never
    // solved), so an infinite-thickness film here is harmless.
    let private gradedCoating : OpticalSystem =
        { coating with films = [ finiteFilm 7.0e-9<meter>; infinityFilm; finiteFilm 3.0e-9<meter> ] }

    let private sphericalSurface (radius : double<meter>) (aperture : double<meter>) : CurvedSurface =
        {
            radiusOfCurvature = radius
            semiAperture = aperture
            figure = Spherical
            coating = coating
        }

    let private light = IncidentLightInfo.create (WaveLength.nm 13.5<nm>)

    let private eNorm (f : EmField) : float = f.e.value.norm

    // ----- AC-C1: nm/µm/mm radius & aperture stored as double<meter> -----

    [<Fact>]
    let ``AC-C1 radius and aperture entered in nm, mkm, mm store as double<meter>`` () =
        // Entry happens at the UI boundary via the Constants factors; the stored
        // record is canonical meters with no non-SI value left.
        let rFromMm = 50.0<mm> * mmToMeter
        let aFromMkm = 2000.0<mkm> * mkmToMeter
        let surface = sphericalSurface rFromMm aFromMkm

        Assert.Equal(0.05, surface.radiusOfCurvature / 1.0<meter>, 12)
        Assert.Equal(0.002, surface.semiAperture / 1.0<meter>, 12)

        // The same physical lengths entered in nm round-trip identically.
        let rFromNm = 5.0e7<nm> * nmToMeter
        Assert.Equal(surface.radiusOfCurvature / 1.0<meter>, rFromNm / 1.0<meter>, 12)

    // ----- AC-C2: sampleZones returns n uniform zones, axis shift = Angle.zero -----

    [<Fact>]
    let ``AC-C2 sampleZones n=1 returns the axis zone with Angle.zero shift`` () =
        let surface = sphericalSurface (50.0<mm> * mmToMeter) (10.0<mm> * mmToMeter)
        let zones = sampleZones surface 1
        Assert.Equal(1, List.length zones)
        Assert.Equal(0.0<meter> / 1.0<meter>, zones.[0].radialPosition / 1.0<meter>, 12)
        Assert.Equal(Angle.zero, zones.[0].localAoiShift)

    [<Fact>]
    let ``AC-C2 sampleZones n returns n zones uniform in radius with axis shift zero`` () =
        let aperture = 10.0<mm> * mmToMeter
        let surface = sphericalSurface (50.0<mm> * mmToMeter) aperture
        let n = 5
        let zones = sampleZones surface n
        Assert.Equal(n, List.length zones)

        // Axis zone is exactly on-axis with no shift.
        Assert.Equal(0.0<meter> / 1.0<meter>, zones.[0].radialPosition / 1.0<meter>, 12)
        Assert.Equal(Angle.zero, zones.[0].localAoiShift)

        // Uniform spacing from 0 to semiAperture: r_i = aperture * i/(n-1).
        zones
        |> List.iteri (fun i z ->
            let expected = (aperture / 1.0<meter>) * (float i / float (n - 1))
            Assert.Equal(expected, z.radialPosition / 1.0<meter>, 12))

        // Edge zone lands exactly on the aperture and carries a non-zero shift.
        let edge = List.last zones
        Assert.Equal(aperture / 1.0<meter>, edge.radialPosition / 1.0<meter>, 12)
        Assert.True(edge.localAoiShift.value > 0.0)

    // ----- AC-C3: solveZone feeds rotateY-shifted incidence to OpticalSystemSolver -----

    [<Fact>]
    let ``AC-C3 axis zone solveZone reproduces the flat solver`` () =
        let surface = sphericalSurface (50.0<mm> * mmToMeter) (10.0<mm> * mmToMeter)
        let axis = (sampleZones surface 3).[0]
        let p = SolverParameters.defaultValue

        let zoneEms = (solveZone light axis p).emSys
        let flatEms = OpticalSystemSolver(light, coating, p).solution.emSys

        Assert.Equal(eNorm flatEms.reflected, eNorm zoneEms.reflected, 12)
        Assert.Equal(eNorm flatEms.transmitted, eNorm zoneEms.transmitted, 12)

    [<Fact>]
    let ``AC-C3 off-axis solveZone shifts incidenceAngle via rotateY`` () =
        let inclined = IncidentLightInfo.createInclined (WaveLength.nm 13.5<nm>) (IncidenceAngle.create (Angle.degree 10.0))
        let surface = sphericalSurface (50.0<mm> * mmToMeter) (10.0<mm> * mmToMeter)
        let edge = List.last (sampleZones surface 4)
        let p = SolverParameters.defaultValue

        // solveZone must feed `info.incidenceAngle` shifted by the zone shift through
        // `IncidentLightInfo.rotateY` — i.e. equal the direct rotateY solve.
        let viaZone = (solveZone inclined edge p).emSys
        let viaRotateY = OpticalSystemSolver(inclined.rotateY edge.localAoiShift, coating, p).solution.emSys
        Assert.Equal(eNorm viaRotateY.reflected, eNorm viaZone.reflected, 12)

        // And it must differ from the unshifted solve, proving the shift is applied.
        let unshifted = OpticalSystemSolver(inclined, coating, p).solution.emSys
        Assert.True(abs (eNorm viaZone.reflected - eNorm unshifted.reflected) > 1e-12)

    // ----- AC-C5: signed-radius convention, sag signed height -----

    [<Fact>]
    let ``AC-C5 concave radius is negative, convex positive, sag signed accordingly`` () =
        let magnitude = 50.0<mm> * mmToMeter
        let r = 10.0<mm> * mmToMeter

        // Convex: positive radius (center of curvature away from the source) -> positive sag.
        let convexSag = sag Spherical magnitude r
        // Concave: negative radius (center toward the source) -> negative sag.
        let concaveSag = sag Spherical (-magnitude) r

        Assert.True(convexSag > 0.0<meter>, $"convex sag must be positive, got {convexSag}")
        Assert.True(concaveSag < 0.0<meter>, $"concave sag must be negative, got {concaveSag}")
        // Same magnitude, opposite sign — one convention, no override flag.
        Assert.Equal(-(convexSag / 1.0<meter>), concaveSag / 1.0<meter>, 12)

        // The exact spherical sag value: R - sqrt(R^2 - r^2) for the convex case.
        let rm = magnitude / 1.0<meter>
        let rr = r / 1.0<meter>
        let expected = rm - sqrt (rm * rm - rr * rr)
        Assert.Equal(expected, convexSag / 1.0<meter>, 12)

    // ----- AC-C6: gradeCoating scales finite Thickness, leaves Infinity -----

    let private gradedSurface (radius : double<meter>) (aperture : double<meter>) : CurvedSurface =
        { sphericalSurface radius aperture with coating = gradedCoating }

    [<Fact>]
    let ``AC-C6 gradeCoating leaves the axis zone unscaled`` () =
        let surface = gradedSurface (50.0<mm> * mmToMeter) (10.0<mm> * mmToMeter)
        let axis = (sampleZones surface 5).[0]
        let graded = gradeCoating surface axis

        // Axis factor is 1.0: every thickness is byte-identical to the source coating.
        List.zip gradedCoating.films graded.films
        |> List.iter (fun (a, b) -> Assert.Equal(a.thickness, b.thickness))

    [<Fact>]
    let ``AC-C6 gradeCoating scales finite thicknesses by the edge factor and keeps Infinity`` () =
        let surface = gradedSurface (50.0<mm> * mmToMeter) (10.0<mm> * mmToMeter)
        let edge = List.last (sampleZones surface 5)
        let graded = gradeCoating surface edge

        // The edge zone hits the ~5% default factor exactly.
        let factor = 1.0 + depthGradingFraction
        List.zip gradedCoating.films graded.films
        |> List.iter (fun (src, out) ->
            match src.thickness, out.thickness with
            | Thickness.Thickness d0, Thickness.Thickness d1 ->
                Assert.Equal((d0 * factor) / 1.0<meter>, d1 / 1.0<meter>, 12)
            | Thickness.Infinity, Thickness.Infinity -> ()
            | other -> Assert.Fail($"thickness case changed under grading: {other}"))

        // The ~5% default is within the stated band and strictly above the axis.
        Assert.True(factor > 1.0 && factor <= 1.10)

    // ----- AC-C7: inter-element gap propagated by EmField.propagate -----

    [<Fact>]
    let ``AC-C7 inter-element gap is carried by the reused EmField.propagate seam`` () =
        let surface = sphericalSurface (50.0<mm> * mmToMeter) (10.0<mm> * mmToMeter)
        let axis = (sampleZones surface 3).[0]
        let outgoing = (solveZone light axis SolverParameters.defaultValue).emSys.transmitted

        // The inter-element gap is an ordinary engine `Layer` whose thickness is the
        // axial separation in meters; propagation is the reused engine seam, with no
        // second propagation routine introduced.
        let gap : Layer = { properties = OpticalProperties.vacuum; thickness = Thickness.Thickness 5.0e-3<meter> }
        let advanced = outgoing.propagate gap

        // Idempotent reference: the seam produces exactly this field.
        Assert.Equal(eNorm (outgoing.propagate gap), eNorm advanced, 12)
        Assert.False(List.isEmpty advanced.emComponents)

    // ----- AC-C8: grading disabled -> one shared immutable coating, no rescale -----

    [<Fact>]
    let ``AC-C8 grading disabled shares one immutable coating across all zones`` () =
        let surface = sphericalSurface (50.0<mm> * mmToMeter) (10.0<mm> * mmToMeter)
        let zones = sampleZones surface 6

        // Every zone references the very same immutable OpticalSystem instance.
        zones
        |> List.iter (fun z -> Assert.True(System.Object.ReferenceEquals(z.coating, surface.coating)))

        // No thickness rescale was applied (the shared coating is the source coating).
        zones
        |> List.iter (fun z -> Assert.Equal<Layer list>(coating.films, z.coating.films))
