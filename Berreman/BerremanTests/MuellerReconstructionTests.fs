namespace BerremanTests

open Berreman.Geometry
open Berreman.Fields
open OpticalConstructor.Domain
open OpticalConstructor.Domain.MuellerReconstruction
open Xunit
open BerremanTests.MatrixComparison

/// Spec 0042 (001) — acceptance for `MuellerReconstruction.retarderMueller`. Reuses the element-by-element
/// Mueller compare loop from `MuellerMatrixTests.fs:20` and the shared `allowedDiff` tolerance
/// (`MatrixComparison.fs:13`) — no hand-rolled epsilon logic.
type MuellerReconstructionTests() =

    /// The same element-by-element compare loop as `MuellerMatrixTests`, over the shared `allowedDiff`.
    let assertMuellerEqual (MuellerMatrix expected) (MuellerMatrix actual) =
        for i in 0..3 do
            for j in 0..3 do
                let d = abs (expected.[i, j] - actual.[i, j])
                Assert.True(d < allowedDiff, $"M[{i},{j}] differs by {d}")

    [<Fact>]
    member _.``zero retardance is the identity Mueller matrix (arbitrary azimuth)`` () =
        // A wave plate with no phase delay is a pass-through, whatever its azimuth: R(-θ)·I·R(θ) = I.
        let actual = retarderMueller (Angle.degree 37.0) (Retardance.degree 0.0)
        assertMuellerEqual Propagation.identityMueller actual

    [<Fact>]
    member _.``zero retardance at zero azimuth is the identity Mueller matrix`` () =
        let actual = retarderMueller Angle.zero (Retardance.degree 0.0)
        assertMuellerEqual Propagation.identityMueller actual

    [<Fact>]
    member _.``quarter-wave retarder at azimuth 0 is the standard QWP Mueller form`` () =
        // Standard QWP, fast axis horizontal: [[1,0,0,0],[0,1,0,0],[0,0,0,1],[0,0,-1,0]].
        let expected =
            Propagation.muellerOfRows
                [ [ 1.0; 0.0; 0.0; 0.0 ]
                  [ 0.0; 1.0; 0.0; 0.0 ]
                  [ 0.0; 0.0; 0.0; 1.0 ]
                  [ 0.0; 0.0; -1.0; 0.0 ] ]
        let actual = retarderMueller Angle.zero (Retardance.degree 90.0)
        assertMuellerEqual expected actual

    [<Fact>]
    member _.``quarter-wave retarder at azimuth 45 is the standard rotated QWP Mueller form`` () =
        // Standard QWP, fast axis at 45°: [[1,0,0,0],[0,0,0,-1],[0,0,1,0],[0,1,0,0]] — exercises rotateMueller.
        let expected =
            Propagation.muellerOfRows
                [ [ 1.0; 0.0; 0.0; 0.0 ]
                  [ 0.0; 0.0; 0.0; -1.0 ]
                  [ 0.0; 0.0; 1.0; 0.0 ]
                  [ 0.0; 1.0; 0.0; 0.0 ] ]
        let actual = retarderMueller (Angle.degree 45.0) (Retardance.degree 90.0)
        assertMuellerEqual expected actual

    [<Fact>]
    member _.``Retardance degrees and value round-trip`` () =
        let r = Retardance.degree 90.0
        Assert.True(abs (r.degrees - 90.0) < allowedDiff, $"degrees = {r.degrees}")
        Assert.True(abs (r.value - (System.Math.PI / 2.0)) < allowedDiff, $"value = {r.value}")
