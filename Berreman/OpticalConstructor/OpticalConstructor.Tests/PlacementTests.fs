namespace OpticalConstructor.Tests

open Berreman.Constants
open Berreman.Geometry
open Berreman.Fields
open Berreman.MaterialProperties
open Berreman.Media
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Units
open OpticalConstructor.Domain.Project
open OpticalConstructor.Domain.Placement
open OpticalConstructor.Storage
open OpticalConstructor.Storage.Errors
open Xunit

/// Part A headless geometry / placement tests (Spec 0026, slice 001): rest-pose
/// normals (AC-A1), default locks (AC-A2), per-axis lock behaviour (A.4.5),
/// `r2 + R2` additivity (AC-A3), R3 non-orthogonality (AC-A4), the emission
/// invariant (AC-A5), the placement round-trip + schema validation (AC-A6), and
/// the catalogue-kind -> domain mapping (AC-A7). Everything here is provable
/// headless — no Avalonia type is touched (constraint 0.3).
module PlacementTests =

    /// Absolute float tolerance, matching the project's `abs (a - b) <= eps`
    /// comparison precedent (e.g. `UnitsTests.fs`).
    let private tol = 1.0e-9
    let private close (a : float) (b : float) : bool = abs (a - b) <= tol

    // --- AC-A1: rest-pose normals -------------------------------------------

    [<Fact>]
    let ``AC-A1 a freshly placed unrotated element has N1 along the central ray and N2 along the table normal`` () =
        let p = ElementPlacement.create LinearPolarizer TablePoint.origin
        let (n1, n2) = orientedNormals p
        // N1 points along the central ray (+X) ...
        Assert.True(close n1.x centralRayDirection.x && close n1.y centralRayDirection.y && close n1.z centralRayDirection.z,
                    $"N1 = ({n1.x}, {n1.y}, {n1.z})")
        // ... and N2 points along the table normal (+Z).
        Assert.True(close n2.x tableNormal.x && close n2.y tableNormal.y && close n2.z tableNormal.z,
                    $"N2 = ({n2.x}, {n2.y}, {n2.z})")

    // --- AC-A2: default locks ----------------------------------------------

    [<Fact>]
    let ``AC-A2 a new element locks R3 and leaves R1 and R2 unlocked`` () =
        let p = ElementPlacement.create Sample TablePoint.origin
        Assert.True(p.r3Locked)
        Assert.False(p.r1Locked)
        Assert.False(p.r2Locked)

    // --- A.4.5: per-axis lock respected ------------------------------------

    [<Fact>]
    let ``A45 a locked rotation ignores a change request and an unlocked one applies it`` () =
        let p = ElementPlacement.create Lens TablePoint.origin
        // R3 starts locked (A.1.2), so `withR3` is inert.
        let stillZero = withR3 (Angle.degree 30.0) p
        Assert.True(close stillZero.r3.degrees 0.0, $"locked R3 changed to {stillZero.r3.degrees} deg")
        // Once unlocked, the change applies.
        let changed = p |> setR3Locked false |> withR3 (Angle.degree 30.0)
        Assert.True(close changed.r3.degrees 30.0, $"unlocked R3 = {changed.r3.degrees} deg")

    // --- AC-A3: r2 + R2 additivity -----------------------------------------

    [<Fact>]
    let ``AC-A3 the table-frame orientation is the incident-ray angle plus the element's own R2`` () =
        let incident = Angle.degree 20.0
        let ownR2 = Angle.degree 15.0
        let result = tableFrameOrientation incident ownR2
        Assert.True(close result.degrees 35.0, $"expected 35 deg, got {result.degrees}")

    // --- AC-A4: R3 breaks R1 perpendicular-to-R2 ---------------------------

    [<Fact>]
    let ``AC-A4 R3 non-zero tips the R1 axis so it is no longer perpendicular to the R2 axis`` () =
        let p = ElementPlacement.create FlatMirror TablePoint.origin
        // With R3 = 0 the R1 axis (face normal) is perpendicular to the R2 axis (table normal).
        Assert.True(close ((r1Axis p).dot r2Axis) 0.0, "R1 should be perpendicular to R2 at rest")
        // Setting R3 != 0 tips N1 out of the table plane: the dot is no longer ~0
        // (the basis is no longer a fixed orthogonal triad — A.4.4). The record is
        // copied directly to prove the geometry math independent of the lock.
        let tilted = { p with r3 = Angle.degree 30.0 }
        let dotted = (r1Axis tilted).dot r2Axis
        Assert.True(abs dotted > 1.0e-6, $"expected R1 not perpendicular to R2, got dot = {dotted}")

    // --- AC-A5: emission invariant -----------------------------------------

    [<Fact>]
    let ``AC-A5 emission can never suppress both groups and turning the second off re-enables the first`` () =
        // Start emitting both, then suppress the reflected group: transmitted stays on.
        let afterReflectedOff = Emission.withReflected false EmitBoth
        Assert.False(afterReflectedOff.emitsReflected)
        Assert.True(afterReflectedOff.emitsTransmitted)
        // Now turn the SECOND (transmitted) off -> the first (reflected) is re-enabled,
        // never the impossible both-off state.
        let afterSecondOff = Emission.withTransmitted false afterReflectedOff
        Assert.True(afterSecondOff.emitsReflected)
        Assert.False(afterSecondOff.emitsTransmitted)
        // Symmetric direction: suppress transmitted first, then reflected.
        let afterTransmittedOff = Emission.withTransmitted false EmitBoth
        Assert.False(afterTransmittedOff.emitsTransmitted)
        let afterSecondOff2 = Emission.withReflected false afterTransmittedOff
        Assert.True(afterSecondOff2.emitsTransmitted)
        Assert.False(afterSecondOff2.emitsReflected)

    // --- AC-A7: catalogue-kind -> domain mapping ---------------------------

    [<Fact>]
    let ``AC-A7 both polarizers map to Polarizer, Detector maps to Detector, and nothing maps to Analyzer`` () =
        Assert.Equal(BeamTree.Polarizer, toConstructorElement LinearPolarizer)
        Assert.Equal(BeamTree.Polarizer, toConstructorElement CircularPolarizer)
        Assert.Equal(BeamTree.Detector, toConstructorElement Detector)
        // Sample maps to the engine Sample case (carrying a placeholder system).
        match toConstructorElement Sample with
        | BeamTree.Sample _ -> ()
        | other -> failwith $"expected Sample, got {other}"
        // No catalogue kind ever maps to the engine `Analyzer` case (A.5.2).
        let allKinds =
            [ LightSource; LinearPolarizer; CircularPolarizer; Sample; Lens; FlatMirror; CurvedMirror; Detector ]
        for k in allKinds do
            match toConstructorElement k with
            | BeamTree.Analyzer -> failwith $"catalogue kind {k} must not map to Analyzer"
            | _ -> ()

    // --- AC-A6: placement round-trip + schema validation -------------------

    let private vacuumSystem : OpticalSystem =
        {
            description = None
            upper = OpticalProperties.vacuum
            films = []
            substrate = None
            lower = OpticalProperties.vacuum
        }

    let private light = IncidentLightInfo.create (WaveLength.nm 600.0<nm>)

    let private rootNode : BeamTree.BeamNode =
        {
            element = BeamTree.Detector
            system = vacuumSystem
            incident = light
            children = Map.empty
            defaultUnit = Nanometer
        }

    let private okOr (r : Result<'a, _>) : 'a =
        match r with
        | Ok v -> v
        | Error e -> failwith $"unexpected storage error: {e}"

    [<Fact>]
    let ``AC-A6 a project carrying placement round-trips through the canonical JSON and passes schema validation`` () =
        // A placement carrying the full §A.8 shape: box, three angles, three locks,
        // catalogue kind, valueId = None, emission metadata, display unit.
        let placement =
            ElementPlacement.create CircularPolarizer { x = 0.01<meter>; y = -0.02<meter> }
            |> setR2Locked false
            |> withR2 (Angle.degree 12.0)
        let project : OpticalConstructorProject =
            { beamTree = { root = rootNode }; systems = [ vacuumSystem ]; sources = []; placements = [ placement ] }
        // serialize -> validate-on-load -> bind. validate-on-load runs the published
        // schema against the document before binding (AC-A6).
        let json = project |> ProjectJson.serializeProject |> okOr
        Assert.StartsWith("{", json.TrimStart())
        let back = json |> ProjectJson.deserializeProject |> okOr
        // Exactly one placement survived ...
        Assert.Equal(1, List.length back.placements)
        let bp = List.head back.placements
        // ... with its canonical-SI scalars intact (angles in radians, points/box in meters) ...
        Assert.True(close bp.r2.value placement.r2.value, $"R2 = {bp.r2.value}")
        Assert.True(close (bp.placementPoint.x / 1.0<meter>) 0.01, $"point.x = {bp.placementPoint.x}")
        Assert.True(close (bp.placementPoint.y / 1.0<meter>) -0.02, $"point.y = {bp.placementPoint.y}")
        Assert.True(close (bp.box.a1 / 1.0<meter>) (opticalFaceExtent / 1.0<meter>), $"box.a1 = {bp.box.a1}")
        Assert.True(close (bp.box.b / 1.0<meter>) (opticalDepth / 1.0<meter>), $"box.b = {bp.box.b}")
        // ... the lock flags, catalogue kind, emission, display unit, and the
        // unbound (None) value id all preserved.
        Assert.True(bp.r3Locked)
        Assert.False(bp.r2Locked)
        Assert.Equal(CircularPolarizer, bp.catalogueKind)
        Assert.Equal(EmitBoth, bp.emission)
        Assert.Equal(Millimeter, bp.displayUnit)
        Assert.True(Option.isNone bp.valueId)
        // The box normals (the rest-pose basis) round-trip too.
        Assert.True(close bp.box.n1.x centralRayDirection.x, $"n1.x = {bp.box.n1.x}")
        Assert.True(close bp.box.n2.z tableNormal.z, $"n2.z = {bp.box.n2.z}")
