namespace OpticalConstructor.Tests

open Berreman.Constants
open Berreman.Geometry
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Placement
open Xunit

/// Headless placement geometry, locks, emission and catalogue-kind mapping
/// (spec 0026 Part A, AC-A1–5 / AC-A7; spec 0050 S4.4).
module PlacementTests =

    /// Absolute float tolerance, matching the project's `abs (a - b) <= eps`
    /// comparison precedent (e.g. `UnitsTests.fs`).
    let private tol = 1.0e-9
    let private close (a : float) (b : float) : bool = abs (a - b) <= tol

    // --- AC-A1: rest-pose normals -------------------------------------------

    [<Fact>]
    let ``AC-A1 a freshly placed unrotated element has N1 along the central ray and N2 along the table normal`` () =
        let p = ElementPlacement.create LinearPolarizer TablePoint.origin
        let { primaryNormal = n1; secondaryNormal = n2 } = orientedBasis p
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
        Assert.Equal(RotationLocked, p.r3Locked)
        Assert.Equal(RotationFree, p.r1Locked)
        Assert.Equal(RotationFree, p.r2Locked)

    [<Fact>]
    let ``spec 0046 the light source alone moves freely and starts with every move and rotation locked`` () =
        let source = ElementPlacement.create LightSource TablePoint.origin
        Assert.Equal(MovesFreely, LightSource.mobility)
        Assert.True([ source.r1Locked; source.r2Locked; source.r3Locked ] |> List.forall (fun lock -> lock = RotationLocked))
        Assert.Equal<MoveLocks>({ xLock = MoveLocked; yLock = MoveLocked; zLock = MoveLocked }, source.moveLocks)
        Assert.Equal(0.0<meter>, source.height)
        for kind in [ LinearPolarizer; CircularPolarizer; Sample; Lens; FlatMirror; CurvedMirror; Detector ] do
            let p = ElementPlacement.create kind TablePoint.origin
            Assert.Equal(MovesAlongBeam, kind.mobility)
            Assert.Equal<MoveLocks>({ xLock = MoveFree; yLock = MoveFree; zLock = MoveFree }, p.moveLocks)

    [<Fact>]
    let ``spec 0046 a locked move axis ignores a change request and an unlocked one applies it`` () =
        let p = ElementPlacement.create LightSource { x = -1.0<meter>; y = 0.0<meter> }
        Assert.Equal<ElementPlacement>(p, withPositionOn TableZ 0.2<meter> p)
        let raised = p |> withMoveLock TableZ MoveFree |> withPositionOn TableZ 0.2<meter>
        Assert.Equal(0.2<meter>, positionOn TableZ raised)
        let c = centreOf raised
        Assert.True(close c.x -1.0 && close c.y 0.0 && close c.z 0.2, $"centre = ({c.x}, {c.y}, {c.z})")
        let shifted = raised |> withMoveLock TableY MoveFree |> withPositionOn TableY 0.3<meter>
        Assert.Equal(0.3<meter>, shifted.placementPoint.y)
        Assert.Equal(MoveLocked, moveLockOf TableX shifted)

    // --- A.4.5: per-axis lock respected ------------------------------------

    [<Fact>]
    let ``A45 a locked rotation ignores a change request and an unlocked one applies it`` () =
        let p = ElementPlacement.create Lens TablePoint.origin
        // R3 starts locked (A.1.2), so `withR3` is inert.
        let stillZero = withR3 (Angle.degree 30.0<deg>) p
        Assert.True(close stillZero.r3.degrees 0.0, $"locked R3 changed to {stillZero.r3.degrees} deg")
        // Once unlocked, the change applies.
        let changed = p |> setR3Locked RotationFree |> withR3 (Angle.degree 30.0<deg>)
        Assert.True(close changed.r3.degrees 30.0, $"unlocked R3 = {changed.r3.degrees} deg")

    // --- AC-A3: r2 + R2 additivity -----------------------------------------

    [<Fact>]
    let ``AC-A3 the table-frame orientation is the incident-ray angle plus the element's own R2`` () =
        let incident = Angle.degree 20.0<deg>
        let ownR2 = Angle.degree 15.0<deg>
        let result = tableFrameOrientation incident ownR2
        Assert.True(close result.degrees 35.0, $"expected 35 deg, got {result.degrees}")

    // --- AC-A4: R3 breaks R1 perpendicular-to-R2 ---------------------------

    [<Fact>]
    let ``AC-A4 R3 non-zero tips the R1 axis so it is no longer perpendicular to the R2 axis`` () =
        let p = ElementPlacement.create FlatMirror TablePoint.origin
        // With R3 = 0 the R1 axis (face normal) is perpendicular to the R2 axis (table normal).
        Assert.True(close (((r1Axis p).dot r2Axis).value) 0.0, "R1 should be perpendicular to R2 at rest")
        // Setting R3 != 0 tips N1 out of the table plane: the dot is no longer ~0
        // (the basis is no longer a fixed orthogonal triad — A.4.4). The record is
        // copied directly to prove the geometry math independent of the lock.
        let tilted = { p with r3 = Angle.degree 30.0<deg> }
        let dotted = ((r1Axis tilted).dot r2Axis).value
        Assert.True(abs dotted > 1.0e-6, $"expected R1 not perpendicular to R2, got dot = {dotted}")

    // --- AC-A5: emission invariant -----------------------------------------

    [<Fact>]
    let ``AC-A5 emission can never suppress both groups and turning the second off re-enables the first`` () =
        // Start emitting both, then suppress the reflected group: transmitted stays on.
        let afterReflectedOff = Emission.withReflected RayGroupOff EmitBoth
        Assert.False(afterReflectedOff.emitsReflected)
        Assert.True(afterReflectedOff.emitsTransmitted)
        // Now turn the SECOND (transmitted) off -> the first (reflected) is re-enabled,
        // never the impossible both-off state.
        let afterSecondOff = Emission.withTransmitted RayGroupOff afterReflectedOff
        Assert.True(afterSecondOff.emitsReflected)
        Assert.False(afterSecondOff.emitsTransmitted)
        // Symmetric direction: suppress transmitted first, then reflected.
        let afterTransmittedOff = Emission.withTransmitted RayGroupOff EmitBoth
        Assert.False(afterTransmittedOff.emitsTransmitted)
        let afterSecondOff2 = Emission.withReflected RayGroupOff afterTransmittedOff
        Assert.True(afterSecondOff2.emitsTransmitted)
        Assert.False(afterSecondOff2.emitsReflected)

    // --- AC-A7: catalogue-kind -> domain mapping ---------------------------

    [<Fact>]
    let ``AC-A7 both polarizers map to Polarizer, Detector maps to Detector, and nothing maps to Analyzer`` () =
        Assert.Equal(BeamTree.PolarizerElement, toConstructorElement LinearPolarizer)
        Assert.Equal(BeamTree.PolarizerElement, toConstructorElement CircularPolarizer)
        Assert.Equal(BeamTree.DetectorElement, toConstructorElement Detector)
        // Sample maps to the engine Sample case (carrying a placeholder system).
        match toConstructorElement Sample with
        | BeamTree.SampleElement _ -> ()
        | other -> failwith $"expected Sample, got {other}"
        // No catalogue kind ever maps to the engine `Analyzer` case (A.5.2).
        let allKinds =
            [ LightSource; LinearPolarizer; CircularPolarizer; Sample; Lens; FlatMirror; CurvedMirror; Detector ]
        for k in allKinds do
            match toConstructorElement k with
            | BeamTree.AnalyzerElement -> failwith $"catalogue kind {k} must not map to Analyzer"
            | _ -> ()
