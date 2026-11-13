namespace BerremanTests

open Berreman.Fields
open OpticalConstructor.Domain                          // PropagationReadouts: MuellerMatrix.element
open OpticalConstructor.Domain.ItemModels
open OpticalConstructor.Domain.MuellerInverse
open OpticalConstructor.Optimization.MuellerFitting
open OpticalConstructor.Optimization.BenchNoise
open Xunit
open BerremanTests.MatrixComparison

/// The test-side assertions and formatting the inverse-problem suites share. The machinery they drive is
/// `OpticalConstructor.Optimization`'s `MuellerFitting`, `BenchNoise`, `RecoveryEnsembles` and `FitUncertainty`
/// (spec 0050 S10.1).
module InverseFitHarness =

    /// A moved product call's value, or the test fails with its typed error (spec 0050 S10.1).
    let expectOk (result : Result<'Value, MuellerFitError>) : 'Value =
        match result with
        | Ok value -> value
        | Error e -> failwith $"%A{e}"

    /// One line per parameter: name and relative error, joined for an assertion message.
    let describeErrors (errors : (ParameterName * RelativeError) list) : string =
        System.String.Join("; ", [ for (n, e) in errors -> $"{n.value} rel err {e.value}" ])

    /// One line per parameter: name and Jacobian column norm, joined for an assertion message.
    let describeColumnNorms (axes : ParameterAxis<'Parameters> list) (norms : float[]) : string =
        System.String.Join("; ", [ for (i, a) in List.indexed axes -> $"{a.name.value} |J| = {norms.[i]}" ])

    /// The same element-by-element Mueller compare loop `MuellerMatrixTests` and
    /// `MuellerReconstructionTests` use, over the shared `allowedDiff` tolerance — no hand-rolled
    /// epsilon.
    let assertMuellerEqual (expected : MuellerMatrix) (actual : MuellerMatrix) : unit =
        for i in 0 .. 3 do
            for j in 0 .. 3 do
                let e = expected.element i j
                let a = actual.element i j
                Assert.True(abs (e - a) < allowedDiff, $"M[{i},{j}]: expected {e}, got {a}")

    /// The fixed, distinct seeds, one noisy experiment each, so a whole ensemble is reproducible bit for bit from
    /// this list alone. Both suites use the same eight, which makes their ensembles directly comparable.
    let ensembleSeeds : NoiseSeed list = [ 1; 2; 3; 4; 5; 6; 7; 8 ] |> List.map NoiseSeed
