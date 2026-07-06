namespace BerremanTests

open System.Numerics
open Berreman.Constants
open Berreman.MaterialProperties
open Berreman.Fields
open Berreman.Dispersion
open Xunit
open BerremanTests.MatrixComparison

/// Slice 010 (spec 0033 Part B): the serializable mu (Polder / gyromagnetic)
/// tree. MuWithDispValue.toMuWithDisp must assemble the Polder tensor rows
/// [mu, +i·g, 0], [-i·g, mu, 0], [0, 0, muParallel] permuted by the gyration
/// axis (AlongZ = Faraday, the default; transverse = Voigt) — expected
/// matrices are built through Mu.create from raw complex literals,
/// independently of the assembly under test. ScalarMu must equal the scaled
/// identity, the constant cases must short-circuit to MuWithoutDisp, and a
/// dispersive Polder value evaluated at a wavelength must equal the constant
/// assembly with the same magnitudes.
type MuWithDispValueTests() =
    let output = TestContext.Current.TestOutputHelper

    /// An arbitrary probe wavelength — the constant cases are wavelength-independent.
    let probe = WaveLength.nm 550.0<nm>

    /// Evaluates a value's engine closure at a wavelength.
    let muAt (v : MuWithDispValue) (w : WaveLength) : Mu = v.toMuWithDisp.getMu w

    let re (v : double) = Complex (v, 0.0)
    let im (v : double) = Complex (0.0, v)
    let zero = Complex.Zero

    /// Ferrite-like magnitudes: three DISTINCT values so a wrong permutation
    /// or a swapped sign cannot alias the right tensor.
    let muDiag = 1.1
    let muPar = 1.3
    let gyr = 0.4

    let polder (axis : GyrationAxis) : PolderValue<MuValue> =
        {
            muDiagonal = MuValue muDiag
            muParallel = MuValue muPar
            gyration = MuValue gyr
            axis = axis
        }

    let constantPolder (axis : GyrationAxis) : MuWithDispValue =
        MuWithoutDispValue (GyromagneticMu (polder axis))

    /// A linear real formula: a + b·x over the reduced wavelength x (µm).
    let linearFormula (a : double) (b : double) : DispersionFormula =
        {
            terms = [ { lambda = 0.0; coefficients = [| a; b |]; power = 1; multiplier = 1.0 } ]
            wavelengthScale = 1.0e-6
        }

    /// The Faraday (AlongZ) tensor: rows [mu, +i·g, 0], [-i·g, mu, 0], [0, 0, muParallel].
    let expectedAlongZ : Mu =
        [
            [ re muDiag; im gyr; zero ]
            [ im (-gyr); re muDiag; zero ]
            [ zero; zero; re muPar ]
        ]
        |> Mu.create


    [<Fact>]
    member _.``Polder tensor along z is the Faraday form with muParallel in the last slot`` () =
        let result = muAt (constantPolder AlongZ) probe
        verifyMatrixEqualityMu output result expectedAlongZ


    [<Fact>]
    member _.``Polder tensor along x cycles muParallel to the first slot`` () =
        let result = muAt (constantPolder AlongX) probe

        let expected =
            [
                [ re muPar; zero; zero ]
                [ zero; re muDiag; im gyr ]
                [ zero; im (-gyr); re muDiag ]
            ]
            |> Mu.create

        verifyMatrixEqualityMu output result expected


    [<Fact>]
    member _.``Polder tensor along y cycles muParallel to the middle slot`` () =
        let result = muAt (constantPolder AlongY) probe

        let expected =
            [
                [ re muDiag; zero; im (-gyr) ]
                [ zero; re muPar; zero ]
                [ im gyr; zero; re muDiag ]
            ]
            |> Mu.create

        verifyMatrixEqualityMu output result expected


    [<Fact>]
    member _.``Scalar mu assembles mu times the identity`` () =
        let m = 3.7
        let result = muAt (MuWithoutDispValue (ScalarMu (MuValue m))) probe

        let expected =
            [
                [ re m; zero; zero ]
                [ zero; re m; zero ]
                [ zero; zero; re m ]
            ]
            |> Mu.create

        verifyMatrixEqualityMu output result expected


    [<Fact>]
    member _.``Constant cases short-circuit to MuWithoutDisp`` () =
        match (constantPolder AlongZ).toMuWithDisp with
        | MuWithoutDisp _ -> ()
        | MuWithDisp _ -> Assert.Fail "a constant Polder value must short-circuit to MuWithoutDisp"

        match (MuWithoutDispValue (ScalarMu (MuValue 2.0))).toMuWithDisp with
        | MuWithoutDisp _ -> ()
        | MuWithDisp _ -> Assert.Fail "a ScalarMu value must short-circuit to MuWithoutDisp"


    [<Fact>]
    member _.``Default gyration axis is AlongZ the Faraday geometry`` () =
        Assert.Equal(AlongZ, GyrationAxis.defaultValue)


    [<Fact>]
    member _.``Dispersive Polder value at a wavelength equals the constant assembly with the same magnitudes`` () =
        // At 500 nm the reduced wavelength is x = 0.5 (µm scale), so the three
        // linear formulas evaluate to exactly the constant magnitudes above:
        // muDiagonal = 0.9 + 0.4·0.5 = 1.1, muParallel = 1.5 − 0.4·0.5 = 1.3,
        // gyration = 0.1 + 0.6·0.5 = 0.4.
        let dispersive : PolderValue<DispersionFormula> =
            {
                muDiagonal = linearFormula 0.9 0.4
                muParallel = linearFormula 1.5 (-0.4)
                gyration = linearFormula 0.1 0.6
                axis = AlongY
            }

        let w = WaveLength.nm 500.0<nm>
        let result = muAt (MuWithDispValue dispersive) w
        verifyMatrixEqualityMu output result (muAt (constantPolder AlongY) w)


    [<Fact>]
    member _.``Dispersive Polder value short-circuits nothing and stays a closure`` () =
        let dispersive : PolderValue<DispersionFormula> =
            {
                muDiagonal = linearFormula 1.0 0.0
                muParallel = linearFormula 1.0 0.0
                gyration = linearFormula 0.0 0.0
                axis = AlongZ
            }

        match (MuWithDispValue dispersive).toMuWithDisp with
        | MuWithDisp _ -> ()
        | MuWithoutDisp _ -> Assert.Fail "a dispersive Polder value must stay wavelength-dependent"
