namespace OpticalConstructor.Tests

open System
open Xunit
open Berreman.Constants
open Berreman.Fields
open Berreman.Geometry
open Berreman.MathNetNumericsMath
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.MuellerReconstruction
open OpticalConstructor.Domain.MuellerInverse
open OpticalConstructor.Domain.FitRuns

/// Spec 0050 S11.4 — a fit's results over their result shapes and synthetic least-squares problems.
module FitResultsTests =

    [<Fact>]
    let ``a Jacobian's rank counts the directions its data constrains, above forward-difference noise`` () : unit =
        let independent = ResidualJacobian [| [| 1.0; 0.0 |]; [| 0.0; 1.0 |]; [| 1.0; 1.0 |] |]
        let proportional = ResidualJacobian [| [| 1.0; 1.0 |]; [| 2.0; 2.0 |]; [| 3.0; 3.0 |] |]
        let proportionalToNoise = ResidualJacobian [| [| 1.0; 1.0 + 1e-9 |]; [| 2.0; 2.0 - 1e-9 |]; [| 3.0; 3.0 + 2e-9 |] |]
        let weakDirection = ResidualJacobian [| [| 1.0; 0.0 |]; [| 0.0; 1e-4 |] |]
        let noRow = ResidualJacobian [||]

        Assert.Equal(MatrixRank 2, independent.rank)
        Assert.Equal(MatrixRank 1, proportional.rank)
        Assert.Equal(MatrixRank 1, proportionalToNoise.rank)
        Assert.Equal(MatrixRank 2, weakDirection.rank)
        Assert.Equal(MatrixRank 0, noRow.rank)

    [<Fact>]
    let ``the residuals of a solution over a least-squares system are its design times the solution less its right-hand side`` () : unit =
        let system : LeastSquaresSystem =
            {
                design = [| DesignRow [| 1.0; 0.0 |]; DesignRow [| 0.0; 1.0 |]; DesignRow [| 1.0; 1.0 |] |]
                rhs = [| 0.1; 2.1; 1.9 |]
            }
        let solution = RealVector.create [ 0.0; 2.0 ]
        let (RealVector residuals) = system.residualsAt solution

        Assert.Equal(3, residuals.Count)
        Assert.Equal(-0.1, residuals.[0], 1e-15)
        Assert.Equal(-0.1, residuals.[1], 1e-15)
        Assert.Equal(0.1, residuals.[2], 1e-15)
        Assert.Equal(0.1, (system.residualRmseAt solution).value, 1e-15)

    [<Fact>]
    let ``a fitted Mueller matrix is realizable when no coherency eigenvalue is negative beyond round-off`` () : unit =
        let item = ItemId (Guid.Parse "39600000-0000-4000-8000-000000000001")

        let realizabilityOf (eigenvalues : CoherencyEigenvalue list) : MuellerRealizability =
            ({ item = item; eigenvalues = eigenvalues } : ObjectRealizability).realizability

        let eigenvaluesOf (values : float list) : CoherencyEigenvalue list = values |> List.map CoherencyEigenvalue

        let diagonal =
            RealMatrix4x4.create
                [ [ 1.0; 0.0; 0.0; 0.0 ]
                  [ 0.0; 1.5; 0.0; 0.0 ]
                  [ 0.0; 0.0; 0.0; 0.0 ]
                  [ 0.0; 0.0; 0.0; 0.0 ] ]
            |> MuellerMatrix

        let retarder = retarderMueller (Angle.degree 30.0<deg>) (Retardance.degree 40.0<deg>)

        List.iter2
            (fun (expected : float) (actual : CoherencyEigenvalue) -> Assert.Equal(expected, actual.value, 1e-12))
            [ 0.625; 0.625; -0.125; -0.125 ]
            diagonal.cloudeEigenvalues

        Assert.Equal(RealizableMueller, realizabilityOf Propagation.identityMueller.cloudeEigenvalues)
        Assert.Equal(RealizableMueller, realizabilityOf retarder.cloudeEigenvalues)
        Assert.Equal(UnrealizableMueller, realizabilityOf diagonal.cloudeEigenvalues)
        Assert.Equal(RealizableMueller, realizabilityOf (eigenvaluesOf [ 1.0; 0.0; 0.0; -1e-14 ]))
        Assert.Equal(UnrealizableMueller, realizabilityOf (eigenvaluesOf [ 1.0; 0.0; 0.0; -1e-6 ]))
