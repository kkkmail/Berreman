namespace OpticalConstructor.Tests

open Xunit
open Berreman.MathNetNumericsMath
open OpticalConstructor.Domain.MuellerReconstruction
open OpticalConstructor.Domain.MuellerInverse

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
