namespace BerremanTests

open Berreman.Geometry
open Berreman.Fields
open Berreman.Media
open Berreman.Solvers
open Berreman.FieldFunctions
open OpticalProperties.Standard
open Xunit
open BerremanTests.MatrixComparison

/// Mandatory same-slice regression guard for the engine-tree edit at
/// `FieldFunctions.fs:93` (Part F §F.3, AC-F3 engine-edit guard): after a solution is
/// computed, the completed `member em.muellerMatrix` MUST return the same matrix as the
/// existing constructor `MuellerMatrix.fromEmFields` (Fields.fs:636), confirming the
/// member is a pure delegation and re-derives no 4×4 algebra. The FULL AC-F3
/// Mueller-equivalence suite stays in slice 011's `OptimizationTests.fs`.
type MuellerMatrixTests() =

    let assertMuellerEqual (MuellerMatrix expected) (MuellerMatrix actual) =
        for i in 0..3 do
            for j in 0..3 do
                let d = abs (expected.[i, j] - actual.[i, j])
                Assert.True(d < allowedDiff, $"M[{i},{j}] differs by {d}")

    [<Fact>]
    member _.``em.muellerMatrix delegates to MuellerMatrix.fromEmFields`` () =
        let info =
            { light600nmInclinedDegreeLPs 19.0 with
                polarization = Angle.degree 27.0 |> Polarization
                ellipticity = Ellipticity 0.58 }

        let solver = OpticalSystemSolver(info, BaseOpticalSystem.transparentGlassSystem.fullSystem)
        let em = solver.solution.emSys.reflected

        let expected = MuellerMatrix.fromEmFields em em
        let actual = em.muellerMatrix
        assertMuellerEqual expected actual
