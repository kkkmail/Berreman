namespace BerremanTests

open System.Numerics
open Berreman.Constants
open Berreman.Fields
open Berreman.Dispersion
open Xunit
open FluentAssertions
open BerremanTests.MatrixComparison

/// Slice 007 (spec 0033 Part B): the generic serializable dispersion formula
/// blocks. DispersionFormula.evaluate must reproduce hand-computed polynomial
/// (Cauchy-shaped Laurent) and inverse-polynomial (Sellmeier-shaped) values
/// within the project tolerance (MatrixComparison.allowedDiff). Every expected
/// value below is computed inline from the direct mathematical form,
/// independently of the term/Horner/pown evaluation path under test.
type DispersionFormulaTests() =

    /// Compares a formula value against an independently computed expectation
    /// using the project's shared tolerance — no new epsilon logic.
    let verifyValue (result : double) (expected : double) =
        (abs (result - expected)).Should().BeLessThan(allowedDiff, $"%A{result} should equal %A{expected} within the project tolerance") |> ignore


    [<Fact>]
    member _.``Cauchy-shaped Laurent polynomial reproduces the hand-computed value`` () =
        // n(λ) = A + B/λ² + C/λ⁴ with the coefficients tabulated in µm
        // (wavelengthScale = 1.0e-6 metres per µm), evaluated at 500 nm = 0.5 µm.
        let a = 1.5046
        let b = 0.0042
        let c = 0.00003

        let formula : DispersionFormula =
            {
                terms =
                    [
                        { lambda = 0.0; coefficients = [| a |]; power = 1; multiplier = 1.0 }
                        { lambda = 0.0; coefficients = [| 0.0; 0.0; 1.0 |]; power = -1; multiplier = b }
                        { lambda = 0.0; coefficients = [| 0.0; 0.0; 0.0; 0.0; 1.0 |]; power = -1; multiplier = c }
                    ]
                wavelengthScale = 1.0e-6
            }

        let x = 0.5
        let expected = a + b / (x * x) + c / (x * x * x * x)   // = 1.52188
        let result = formula.evaluate (WaveLength.nm 500.0<nm>)
        verifyValue result expected


    [<Fact>]
    member _.``Sellmeier-shaped inverse term reproduces the hand-computed value`` () =
        // n²(λ) = 1 + B·λ²/(λ² − C) (BK7's first Sellmeier term; λ in µm, C in µm²),
        // expressed in term form via the partial fraction (1 + B) + B·C·(λ² − C)^(−1),
        // evaluated at 587.6 nm = 0.5876 µm against the direct rational form.
        let b = 1.03961212
        let c = 0.00600069867

        let formula : DispersionFormula =
            {
                terms =
                    [
                        { lambda = 0.0; coefficients = [| 1.0 + b |]; power = 1; multiplier = 1.0 }
                        { lambda = 0.0; coefficients = [| -c; 0.0; 1.0 |]; power = -1; multiplier = b * c }
                    ]
                wavelengthScale = 1.0e-6
            }

        let x = 0.5876
        let expected = 1.0 + b * x * x / (x * x - c)
        let result = formula.evaluate (WaveLength.nm 587.6<nm>)
        verifyValue result expected


    [<Fact>]
    member _.``Shifted-centre squared term pins the x minus lambda centring and the outer power`` () =
        // 2.5 · (0.3 + 1.2·(x − 0.4))² at x = 0.6 µm — a nonzero expansion centre
        // and a positive outer power.
        let formula : DispersionFormula =
            {
                terms = [ { lambda = 0.4; coefficients = [| 0.3; 1.2 |]; power = 2; multiplier = 2.5 } ]
                wavelengthScale = 1.0e-6
            }

        let inner = 0.3 + 1.2 * (0.6 - 0.4)
        let expected = 2.5 * inner * inner
        let result = formula.evaluate (WaveLength.nm 600.0<nm>)
        verifyValue result expected


    [<Fact>]
    member _.``Complex mirror reproduces the hand-computed Lorentz-shaped value`` () =
        // A single Lorentz-shaped pole ε(x) = 1 + S/(x₀² − x² − i·γ·x) over Complex
        // (x in µm): the denominator is the complex polynomial
        // x₀² + (−i·γ)·x + (−1)·x² raised to the power −1.
        let s = Complex (2.0, 0.0)
        let gamma = 0.05
        let x0Sq = 0.36

        let formula : ComplexDispersionFormula =
            {
                terms =
                    [
                        { lambda = Complex.Zero; coefficients = [| Complex.One |]; power = 1; multiplier = Complex.One }
                        { lambda = Complex.Zero; coefficients = [| Complex (x0Sq, 0.0); Complex (0.0, -gamma); Complex (-1.0, 0.0) |]; power = -1; multiplier = s }
                    ]
                wavelengthScale = 1.0e-6
            }

        let x = Complex (0.5, 0.0)
        let expected = Complex.One + s / (Complex (x0Sq, 0.0) - x * x - Complex (0.0, gamma) * x)
        let result = formula.evaluate (WaveLength.nm 500.0<nm>)
        (result - expected).Magnitude.Should().BeLessThan(allowedDiff, $"%A{result} should equal %A{expected} within the project tolerance") |> ignore


    [<Fact>]
    member _.``WaveLengthInterval carries its elevated endpoints unchanged`` () =
        let interval : WaveLengthInterval =
            {
                lower = WaveLength.nm 400.0<nm>
                upper = WaveLength.nm 700.0<nm>
            }

        verifyValue (interval.lower.value / 1.0<meter>) 4.0e-7
        verifyValue (interval.upper.value / 1.0<meter>) 7.0e-7
