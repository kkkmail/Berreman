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

    // Spec 0042 (002) — the column-major linear-algebra core. A NON-symmetric 4×4 with distinct entries so a
    // row/column-major mix-up is observable (M[i,j] ≠ M[j,i] off the diagonal); reused by the round-trip and
    // the transpose guard. Bound here, ahead of the members: in an F# class type every `let` binding must
    // precede the first member (FS0960), so this fixture cannot sit down among the [<Fact>] members below.
    let asymmetricM =
        Propagation.muellerOfRows
            [ [ 1.0; 0.2; -0.3; 0.4 ]
              [ 0.5; 0.6; 0.7; -0.8 ]
              [ -0.9; 1.0; 0.1; 0.2 ]
              [ 0.3; -0.4; 0.5; 0.6 ] ]

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

    [<Fact>]
    member _.``vecColumnMajor round-trips through muellerOfVecColumnMajor`` () =
        // muellerOfVecColumnMajor (vecColumnMajor M) = M for every 4×4 (the vec round-trip acceptance).
        let actual = muellerOfVecColumnMajor (vecColumnMajor asymmetricM)
        assertMuellerEqual asymmetricM actual

    [<Fact>]
    member _.``kron4 dotted with the column-major vec is a dot (M times s) (transpose guard)`` () =
        // The design row aᵀ⊗sᵀ dotted with vec_F(M) MUST equal a·(M·s); the non-symmetric M makes this fail
        // under a row-major flatten, so the identity pins the column-major (order F) convention.
        let s = StokesVector.create [ 1.0; 0.3; -0.7; 0.5 ]
        let a = RealVector4.create [ 0.2; -1.1; 0.4; 0.9 ]
        let lhs = Array.map2 (*) (kron4 s a) (vecColumnMajor asymmetricM) |> Array.sum
        let (StokesVector ms) = asymmetricM * s
        let rhs = [ 0 .. 3 ] |> List.sumBy (fun j -> a.[j] * ms.[j])
        Assert.True(abs (lhs - rhs) < allowedDiff, $"lhs = {lhs}, rhs = {rhs}, diff = {abs (lhs - rhs)}")

    [<Fact>]
    member _.``frobeniusDiff of a matrix with itself is zero`` () =
        let d = frobeniusDiff asymmetricM asymmetricM
        Assert.True(d.frobenius < allowedDiff, $"frobenius = {d.frobenius}")
        Assert.True(d.maxAbs < allowedDiff, $"maxAbs = {d.maxAbs}")
        Assert.True(d.meanAbs < allowedDiff, $"meanAbs = {d.meanAbs}")

    [<Fact>]
    member _.``frobeniusDiff localizes the largest element difference and its index`` () =
        // Perturb the single element at (2,1) from 1.0 to 3.0 (a diff of 2.0); the readout must localize it.
        let perturbed =
            Propagation.muellerOfRows
                [ [ 1.0; 0.2; -0.3; 0.4 ]
                  [ 0.5; 0.6; 0.7; -0.8 ]
                  [ -0.9; 3.0; 0.1; 0.2 ]
                  [ 0.3; -0.4; 0.5; 0.6 ] ]
        let d = frobeniusDiff asymmetricM perturbed
        let (mi, mj) = d.argMax
        Assert.Equal(2, mi)
        Assert.Equal(1, mj)
        Assert.True(abs (d.maxAbs - 2.0) < allowedDiff, $"maxAbs = {d.maxAbs}")
        Assert.True(abs (d.frobenius - 2.0) < allowedDiff, $"frobenius = {d.frobenius}")
        Assert.True(abs (d.meanAbs - (2.0 / 16.0)) < allowedDiff, $"meanAbs = {d.meanAbs}")
