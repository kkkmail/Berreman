namespace BerremanTests

open System.Numerics
open Berreman.Constants
open Berreman.MaterialProperties
open Berreman.Fields
open Berreman.Dispersion
open Xunit
open BerremanTests.MatrixComparison

/// Slice 008 (spec 0033 Part B): the serializable eps tree. Every ConstantEpsValue
/// case routed through EpsWithDispValue.toEpsWithDisp must reproduce the
/// directly-constructed engine Eps; dispersive segment selection must pick the
/// FIRST segment whose interval covers the wavelength (top-of-list wins on
/// overlap) and extrapolate with the topmost segment when no interval covers;
/// the uniaxial and biaxial dispersive closures must equal the per-axis analytic
/// values. Expected matrices are built through the engine constructors from
/// analytic index values computed inline, independently of the tree under test.
type EpsWithDispValueTests() =
    let output = TestContext.Current.TestOutputHelper

    /// An arbitrary probe wavelength — the constant cases are wavelength-independent.
    let probe = WaveLength.nm 550.0<nm>

    /// Evaluates a value's engine closure at a wavelength.
    let epsAt (v : EpsWithDispValue) (w : WaveLength) : Eps = v.toEpsWithDisp.getEps w

    /// A constant real formula: value at every reduced wavelength x (µm).
    let constFormula (value : double) : DispersionFormula =
        {
            terms = [ { lambda = 0.0; coefficients = [| value |]; power = 1; multiplier = 1.0 } ]
            wavelengthScale = 1.0e-6
        }

    /// A linear real formula: a + b·x over the reduced wavelength x (µm).
    let linearFormula (a : double) (b : double) : DispersionFormula =
        {
            terms = [ { lambda = 0.0; coefficients = [| a; b |]; power = 1; multiplier = 1.0 } ]
            wavelengthScale = 1.0e-6
        }

    /// A Cauchy-shaped real formula: a + b/x² over the reduced wavelength x (µm).
    let cauchyFormula (a : double) (b : double) : DispersionFormula =
        {
            terms =
                [
                    { lambda = 0.0; coefficients = [| a |]; power = 1; multiplier = 1.0 }
                    { lambda = 0.0; coefficients = [| 0.0; 0.0; 1.0 |]; power = -1; multiplier = b }
                ]
            wavelengthScale = 1.0e-6
        }

    /// The zero formula — the k of a transparent medium.
    let zeroFormula : DispersionFormula = { terms = []; wavelengthScale = 1.0e-6 }

    let interval (lo : double<nm>) (hi : double<nm>) : WaveLengthInterval =
        { lower = WaveLength.nm lo; upper = WaveLength.nm hi }

    /// The Eps the isotropic dispersive path must produce for a real index n.
    let isotropicEpsOf (n : double) : Eps =
        Complex (n, 0.0) |> ComplexRefractionIndex |> Eps.fromComplexRefractionIndex

    /// Two isotropic segments: the topmost covers 400–700 nm with the
    /// wavelength-DEPENDENT n(x) = 1.4 + 0.05·x; the second covers 600–900 nm
    /// with the constant n = 2.0. The dependence makes genuine extrapolation
    /// (evaluate the topmost formula AT the out-of-range wavelength)
    /// distinguishable from clamping to the interval edge.
    let overlappingSegments : IsotropicEpsSegment list =
        [
            { wavelengthInterval = interval 400.0<nm> 700.0<nm>; dispersion = RealNK (linearFormula 1.4 0.05, zeroFormula) }
            { wavelengthInterval = interval 600.0<nm> 900.0<nm>; dispersion = RealNK (constFormula 2.0, zeroFormula) }
        ]


    [<Fact>]
    member _.``Constant isotropic transparent value reproduces the directly-constructed Eps`` () =
        let n = RefractionIndex 1.52
        let result = epsAt (EpsWithoutDispValue (IsotropicTransparent n)) probe
        verifyMatrixEqualityEps output result (Eps.fromRefractionIndex n)


    [<Fact>]
    member _.``Constant isotropic absorbing value reproduces the directly-constructed Eps`` () =
        let n = ComplexRefractionIndex (Complex (1.9, 0.4))
        let result = epsAt (EpsWithoutDispValue (IsotropicAbsorbing n)) probe
        verifyMatrixEqualityEps output result (Eps.fromComplexRefractionIndex n)


    [<Fact>]
    member _.``Constant uniaxial transparent value maps to the n_o n_e n_o triple`` () =
        let nO = RefractionIndex 1.55
        let nE = RefractionIndex 1.78
        let result = epsAt (EpsWithoutDispValue (UniaxialTransparent (nO, nE))) probe
        verifyMatrixEqualityEps output result (Eps.fromRefractionIndex (nO, nE, nO))


    [<Fact>]
    member _.``Constant uniaxial absorbing value maps to the n_o n_e n_o triple`` () =
        let nO = ComplexRefractionIndex (Complex (2.1, 0.3))
        let nE = ComplexRefractionIndex (Complex (2.4, 0.1))
        let result = epsAt (EpsWithoutDispValue (UniaxialAbsorbing (nO, nE))) probe
        verifyMatrixEqualityEps output result (Eps.fromComplexRefractionIndex (nO, nE, nO))


    [<Fact>]
    member _.``Constant biaxial transparent value reproduces the directly-constructed Eps`` () =
        let nx = RefractionIndex 1.5
        let ny = RefractionIndex 1.65
        let nz = RefractionIndex 1.75
        let result = epsAt (EpsWithoutDispValue (BiaxialTransparent (nx, ny, nz))) probe
        verifyMatrixEqualityEps output result (Eps.fromRefractionIndex (nx, ny, nz))


    [<Fact>]
    member _.``Constant biaxial absorbing value reproduces the directly-constructed Eps`` () =
        let nx = ComplexRefractionIndex (Complex (1.6, 0.05))
        let ny = ComplexRefractionIndex (Complex (1.8, 0.15))
        let nz = ComplexRefractionIndex (Complex (2.0, 0.25))
        let result = epsAt (EpsWithoutDispValue (BiaxialAbsorbing (nx, ny, nz))) probe
        verifyMatrixEqualityEps output result (Eps.fromComplexRefractionIndex (nx, ny, nz))


    [<Fact>]
    member _.``On overlap the first covering segment wins`` () =
        // 650 nm is covered by both segments: the topmost (n = 1.4 + 0.05·0.65)
        // must win over the second (n = 2.0).
        let result = epsAt (EpsWithDispValue (IsotropicDispersive overlappingSegments)) (WaveLength.nm 650.0<nm>)
        verifyMatrixEqualityEps output result (isotropicEpsOf (1.4 + 0.05 * 0.65))


    [<Fact>]
    member _.``A wavelength covered only by a later segment selects that segment`` () =
        // 800 nm is outside the topmost interval and inside the second:
        // the FIRST COVERING segment is the second one (n = 2.0).
        let result = epsAt (EpsWithDispValue (IsotropicDispersive overlappingSegments)) (WaveLength.nm 800.0<nm>)
        verifyMatrixEqualityEps output result (isotropicEpsOf 2.0)


    [<Fact>]
    member _.``When no segment covers the wavelength the topmost segment extrapolates`` () =
        // 1200 nm is covered by neither segment: the topmost formula evaluates
        // AT 1.2 µm (n = 1.4 + 0.05·1.2 = 1.46) — no clamping to the 700 nm edge
        // (which would give 1.435) and no nearest-segment fallback (2.0).
        let result = epsAt (EpsWithDispValue (IsotropicDispersive overlappingSegments)) (WaveLength.nm 1200.0<nm>)
        verifyMatrixEqualityEps output result (isotropicEpsOf (1.4 + 0.05 * 1.2))


    [<Fact>]
    member _.``Uniaxial dispersive closure equals the per-axis analytic values`` () =
        // n_o(x) = 1.5 + 0.004/x², n_e(x) = 1.6 + 0.007/x² (x in µm), at 500 nm;
        // the eps triple is (n_o, n_e, n_o).
        let aO = 1.5
        let bO = 0.004
        let aE = 1.6
        let bE = 0.007

        let segments : UniaxialEpsSegment list =
            [
                {
                    wavelengthInterval = interval 400.0<nm> 700.0<nm>
                    ordinaryDispersion = RealNK (cauchyFormula aO bO, zeroFormula)
                    extraordinaryDispersion = RealNK (cauchyFormula aE bE, zeroFormula)
                }
            ]

        let x = 0.5
        let nO = ComplexRefractionIndex (Complex (aO + bO / (x * x), 0.0))
        let nE = ComplexRefractionIndex (Complex (aE + bE / (x * x), 0.0))
        let result = epsAt (EpsWithDispValue (UniaxialDispersive segments)) (WaveLength.nm 500.0<nm>)
        verifyMatrixEqualityEps output result (Eps.fromComplexRefractionIndex (nO, nE, nO))


    [<Fact>]
    member _.``Biaxial dispersive closure equals the per-axis analytic values`` () =
        // x axis: n(x) = 1.9 + 0.01/x² with the constant k = 0.2 (RealNK, absorbing);
        // y axis: constant transparent n = 1.7 (RealNK, zero k);
        // z axis: constant complex eps = 4.0 + 0.5i (ComplexEps — complexIndex
        // takes its square root). Evaluated at 500 nm (x = 0.5 µm).
        let ax = 1.9
        let bx = 0.01
        let kx = 0.2
        let epsZ = Complex (4.0, 0.5)

        let zFormula : ComplexDispersionFormula =
            {
                terms = [ { lambda = Complex.Zero; coefficients = [| epsZ |]; power = 1; multiplier = Complex.One } ]
                wavelengthScale = 1.0e-6
            }

        let segments : BiaxialEpsSegment list =
            [
                {
                    wavelengthInterval = interval 400.0<nm> 700.0<nm>
                    xDispersion = RealNK (cauchyFormula ax bx, constFormula kx)
                    yDispersion = RealNK (constFormula 1.7, zeroFormula)
                    zDispersion = ComplexEps zFormula
                }
            ]

        // The z expectation is the principal square root of eps_z computed from
        // the closed real/imaginary form — independent of Complex.Sqrt.
        let x = 0.5
        let nx = ComplexRefractionIndex (Complex (ax + bx / (x * x), kx))
        let ny = ComplexRefractionIndex (Complex (1.7, 0.0))
        let modulus = sqrt (epsZ.Real * epsZ.Real + epsZ.Imaginary * epsZ.Imaginary)
        let nz = ComplexRefractionIndex (Complex (sqrt ((modulus + epsZ.Real) / 2.0), sqrt ((modulus - epsZ.Real) / 2.0)))

        let result = epsAt (EpsWithDispValue (BiaxialDispersive segments)) (WaveLength.nm 500.0<nm>)
        verifyMatrixEqualityEps output result (Eps.fromComplexRefractionIndex (nx, ny, nz))
