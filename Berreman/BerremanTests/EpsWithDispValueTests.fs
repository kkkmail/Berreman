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
            wavelengthScale = 1.0e-6<meter>
        }

    /// A linear real formula: a + b·x over the reduced wavelength x (µm).
    let linearFormula (a : double) (b : double) : DispersionFormula =
        {
            terms = [ { lambda = 0.0; coefficients = [| a; b |]; power = 1; multiplier = 1.0 } ]
            wavelengthScale = 1.0e-6<meter>
        }

    /// A Cauchy-shaped real formula: a + b/x² over the reduced wavelength x (µm).
    let cauchyFormula (a : double) (b : double) : DispersionFormula =
        {
            terms =
                [
                    { lambda = 0.0; coefficients = [| a |]; power = 1; multiplier = 1.0 }
                    { lambda = 0.0; coefficients = [| 0.0; 0.0; 1.0 |]; power = -1; multiplier = b }
                ]
            wavelengthScale = 1.0e-6<meter>
        }

    /// The zero formula — the k of a transparent medium.
    let zeroFormula : DispersionFormula = { terms = []; wavelengthScale = 1.0e-6<meter> }

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
            { wavelengthInterval = interval 400.0<nm> 700.0<nm>; dispersion = RealNK { nFormula = linearFormula 1.4 0.05; kFormula = zeroFormula } }
            { wavelengthInterval = interval 600.0<nm> 900.0<nm>; dispersion = RealNK { nFormula = constFormula 2.0; kFormula = zeroFormula } }
        ]

    /// The hand-written table of the tabulated facts (spec 0050 step 2): 400, 500 and 700 nm,
    /// n 1.50, 1.60 and 1.90, k 0.00, 0.10 and 0.30.
    let tableWaveLengthsNm : double<nm> array = [| 400.0<nm>; 500.0<nm>; 700.0<nm> |]
    let tableN : double array = [| 1.50; 1.60; 1.90 |]
    let tableK : double array = [| 0.00; 0.10; 0.30 |]

    /// The table over wavelengths given in nm, each reduced to metres through `WaveLength.nm`.
    let tryTable (wavelengthsNm : double<nm> array) (n : double array) (k : double array) : Result<TabulatedNkTable, TabulatedTableError> =
        TabulatedNkTable.tryCreate (wavelengthsNm |> Array.map (fun w -> (WaveLength.nm w).value)) n k

    let tableOf (wavelengthsNm : double<nm> array) (n : double array) (k : double array) : TabulatedNkTable =
        match tryTable wavelengthsNm n k with
        | Ok table -> table
        | Error (EmptyTableErr reason)
        | Error (SampleCountMismatchErr reason)
        | Error (UnsortedSamplesErr reason) -> failwith reason

    /// Compares an index with the expected n + ik through the engine Eps both build.
    let verifyIndex (result : ComplexRefractionIndex) (n : double) (k : double) =
        verifyMatrixEqualityEps output (Eps.fromComplexRefractionIndex result) (ComplexRefractionIndex (Complex (n, k)) |> Eps.fromComplexRefractionIndex)

    /// The reason a refusal carries, failing the fact on an accepted table.
    let refusalOf (result : Result<TabulatedNkTable, TabulatedTableError>) : TabulatedTableError =
        match result with
        | Error e -> e
        | Ok table -> failwith $"expected a refusal, got a table over {table.wavelengths.Length} samples"

    /// The electron-volt abscissa E = 1239.84 / λ[nm], as `AbscissaKind.ofUnit ElectronVolt` derives it.
    let electronVolt : AbscissaKind = ReciprocalInX { wavelengthScale = 1.0e-9<meter>; numerator = 1239.84 }

    /// A law's index equals each reference exactly. The references are the start tree's
    /// `DispersionModel.evaluate` as G17, recorded before the kernels moved into the core (spec 0050 steps 5 and 7).
    let verifyLawReferences (complexIndex : WaveLength -> ComplexRefractionIndex) (references : (WaveLength * ComplexRefractionIndex) list) : unit =
        for (w, expected) in references do
            Assert.Equal<ComplexRefractionIndex>(expected, complexIndex w)


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
        let result = epsAt (EpsWithoutDispValue (UniaxialTransparent { ordinary = nO; extraordinary = nE })) probe
        verifyMatrixEqualityEps output result (Eps.fromRefractionIndex (nO, nE, nO))


    [<Fact>]
    member _.``Constant uniaxial absorbing value maps to the n_o n_e n_o triple`` () =
        let nO = ComplexRefractionIndex (Complex (2.1, 0.3))
        let nE = ComplexRefractionIndex (Complex (2.4, 0.1))
        let result = epsAt (EpsWithoutDispValue (UniaxialAbsorbing { ordinaryComplex = nO; extraordinaryComplex = nE })) probe
        verifyMatrixEqualityEps output result (Eps.fromComplexRefractionIndex (nO, nE, nO))


    [<Fact>]
    member _.``Constant biaxial transparent value reproduces the directly-constructed Eps`` () =
        let nx = RefractionIndex 1.5
        let ny = RefractionIndex 1.65
        let nz = RefractionIndex 1.75
        let result = epsAt (EpsWithoutDispValue (BiaxialTransparent { nx = nx; ny = ny; nz = nz })) probe
        verifyMatrixEqualityEps output result (Eps.fromRefractionIndex (nx, ny, nz))


    [<Fact>]
    member _.``Constant biaxial absorbing value reproduces the directly-constructed Eps`` () =
        let nx = ComplexRefractionIndex (Complex (1.6, 0.05))
        let ny = ComplexRefractionIndex (Complex (1.8, 0.15))
        let nz = ComplexRefractionIndex (Complex (2.0, 0.25))
        let result = epsAt (EpsWithoutDispValue (BiaxialAbsorbing { nxComplex = nx; nyComplex = ny; nzComplex = nz })) probe
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
                    ordinaryDispersion = RealNK { nFormula = cauchyFormula aO bO; kFormula = zeroFormula }
                    extraordinaryDispersion = RealNK { nFormula = cauchyFormula aE bE; kFormula = zeroFormula }
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
                wavelengthScale = 1.0e-6<meter>
            }

        let segments : BiaxialEpsSegment list =
            [
                {
                    wavelengthInterval = interval 400.0<nm> 700.0<nm>
                    xDispersion = RealNK { nFormula = cauchyFormula ax bx; kFormula = constFormula kx }
                    yDispersion = RealNK { nFormula = constFormula 1.7; kFormula = zeroFormula }
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


    [<Fact>]
    member _.``A tabulated axis returns each sample at its own wavelength and interpolates linearly between neighbours`` () =
        let table = tableOf tableWaveLengthsNm tableN tableK
        let axis = TabulatedNK table

        for i in 0 .. tableWaveLengthsNm.Length - 1 do
            verifyIndex (axis.complexIndex (WaveLength.nm tableWaveLengthsNm.[i])) tableN.[i] tableK.[i]

        // Between neighbours i and i + 1 the index is linear in λ: t = (λ − λᵢ)/(λᵢ₊₁ − λᵢ).
        let lerp (i : int) (lambda : double<nm>) : Complex =
            let t = (lambda - tableWaveLengthsNm.[i]) / (tableWaveLengthsNm.[i + 1] - tableWaveLengthsNm.[i])
            Complex (tableN.[i] + (tableN.[i + 1] - tableN.[i]) * t, tableK.[i] + (tableK.[i + 1] - tableK.[i]) * t)

        for (i, lambda) in [ (0, 450.0<nm>); (0, 420.0<nm>); (1, 600.0<nm>); (1, 650.0<nm>) ] do
            let expected = lerp i lambda
            verifyIndex (axis.complexIndex (WaveLength.nm lambda)) expected.Real expected.Imaginary

        // The engine Eps the tree builds is the Eps of the interpolated index.
        let value = EpsWithDispValue (IsotropicDispersive [ { wavelengthInterval = table.sampleRange; dispersion = axis } ])
        let expected = lerp 1 650.0<nm>
        let result = epsAt value (WaveLength.nm 650.0<nm>)
        verifyMatrixEqualityEps output result (Eps.fromComplexRefractionIndex (ComplexRefractionIndex expected))


    [<Fact>]
    member _.``A tabulated axis clamps to its first sample below the table and to its last sample above it`` () =
        let axis = TabulatedNK (tableOf tableWaveLengthsNm tableN tableK)
        let last = tableWaveLengthsNm.Length - 1

        for lambda in [ 200.0<nm>; 399.0<nm> ] do
            verifyIndex (axis.complexIndex (WaveLength.nm lambda)) tableN.[0] tableK.[0]

        for lambda in [ 701.0<nm>; 1500.0<nm> ] do
            verifyIndex (axis.complexIndex (WaveLength.nm lambda)) tableN.[last] tableK.[last]


    [<Fact>]
    member _.``A single-sample tabulated axis is constant at every wavelength`` () =
        let axis = TabulatedNK (tableOf [| 550.0<nm> |] [| 1.70 |] [| 0.05 |])

        for lambda in [ 200.0<nm>; 549.0<nm>; 550.0<nm>; 551.0<nm>; 1500.0<nm> ] do
            verifyIndex (axis.complexIndex (WaveLength.nm lambda)) 1.70 0.05


    [<Fact>]
    member _.``TabulatedNkTable tryCreate refuses an empty table, unequal columns and a decreasing wavelength column`` () =
        match refusalOf (tryTable [||] [||] [||]) with
        | EmptyTableErr _ -> ()
        | other -> Assert.Fail($"an empty table: expected EmptyTableErr, got %A{other}")

        match refusalOf (tryTable tableWaveLengthsNm [| 1.50; 1.60 |] tableK) with
        | SampleCountMismatchErr reason -> Assert.Contains("2 n values", reason)
        | other -> Assert.Fail($"a short n column: expected SampleCountMismatchErr, got %A{other}")

        match refusalOf (tryTable tableWaveLengthsNm tableN [| 0.00 |]) with
        | SampleCountMismatchErr reason -> Assert.Contains("1 k values", reason)
        | other -> Assert.Fail($"a short k column: expected SampleCountMismatchErr, got %A{other}")

        match refusalOf (tryTable [| 400.0<nm>; 700.0<nm>; 500.0<nm> |] tableN tableK) with
        | UnsortedSamplesErr reason -> Assert.Contains("index 2", reason)
        | other -> Assert.Fail($"a decreasing wavelength column: expected UnsortedSamplesErr, got %A{other}")

        // Accepted columns are kept as given (never sorted), read linearly and clamped; a
        // repeated wavelength does not decrease, so it is accepted.
        let table = tableOf tableWaveLengthsNm tableN tableK
        Assert.Equal<double array>(tableN, table.n)
        Assert.Equal<double array>(tableK, table.k)
        Assert.Equal<TabulatedInterpolation>(LinearInWaveLengthClamped, table.interpolation)
        match tryTable [| 400.0<nm>; 400.0<nm>; 500.0<nm> |] tableN tableK with
        | Ok _ -> ()
        | Error e -> Assert.Fail($"a repeated wavelength must be accepted, got %A{e}")


    [<Fact>]
    member _.``The tabulated interpolation rule round-trips its wire tag and refuses an unknown tag`` () =
        Assert.Equal(1, LinearInWaveLengthClamped.value)
        Assert.Equal<TabulatedInterpolation option>(Some LinearInWaveLengthClamped, TabulatedInterpolation.tryOfWire LinearInWaveLengthClamped.value)

        for tag in [ 0; 2; -1 ] do
            Assert.Equal<TabulatedInterpolation option>(None, TabulatedInterpolation.tryOfWire tag)


    [<Fact>]
    member _.``The Tauc-Lorentz law reproduces its reference index on both sides of the band gap`` () =
        // Eg = 2.2 eV sits at 563.6 nm: 300 and 450 nm absorb; 600, 900 and 2000 nm give √ε∞ alone.
        let law : TaucLorentzLaw =
            {
                epsInf = 1.5
                amplitude = 100.0
                resonance = 3.5
                broadening = 0.8
                bandGap = 2.2
                abscissa = electronVolt
            }

        verifyLawReferences law.complexIndex
            [
                WaveLength.nm 300.0<nm>, ComplexRefractionIndex (Complex (2.1258164116865936, 1.7375544354626891))
                WaveLength.nm 450.0<nm>, ComplexRefractionIndex (Complex (1.3053863577573146, 0.45170072284523471))
                WaveLength.nm 600.0<nm>, ComplexRefractionIndex (Complex (1.2247448713915889, 0.0))
                WaveLength.nm 900.0<nm>, ComplexRefractionIndex (Complex (1.2247448713915889, 0.0))
                WaveLength.nm 2000.0<nm>, ComplexRefractionIndex (Complex (1.2247448713915889, 0.0))
            ]


    [<Fact>]
    member _.``The Tauc-Lorentz law over a micrometre abscissa reproduces its reference index`` () =
        // The abscissa is λ in µm and Eg = 0.7 µm: 300, 450 and 600 nm give √ε∞ = 1.5; 900 and 2000 nm absorb.
        let law : TaucLorentzLaw =
            {
                epsInf = 2.25
                amplitude = 60.0
                resonance = 1.1
                broadening = 0.6
                bandGap = 0.7
                abscissa = LinearInX 1.0e-6<meter>
            }

        verifyLawReferences law.complexIndex
            [
                WaveLength.nm 300.0<nm>, ComplexRefractionIndex (Complex (1.5, 0.0))
                WaveLength.nm 450.0<nm>, ComplexRefractionIndex (Complex (1.5, 0.0))
                WaveLength.nm 600.0<nm>, ComplexRefractionIndex (Complex (1.5, 0.0))
                WaveLength.nm 900.0<nm>, ComplexRefractionIndex (Complex (1.8371337933094558, 1.0606887264978306))
                WaveLength.nm 2000.0<nm>, ComplexRefractionIndex (Complex (1.8053783060899984, 1.0046844420515288))
            ]


    [<Fact>]
    member _.``The Gaussian-oscillator law reproduces its reference index`` () =
        // E0 = 2.5 eV sits at 495.9 nm: the absorption peaks between 450 and 600 nm and falls off on both sides.
        let law : GaussianOscillatorLaw =
            {
                epsInf = 2.0
                amplitude = 0.5
                energy = 2.5
                broadening = 0.4
                abscissa = electronVolt
            }

        verifyLawReferences law.complexIndex
            [
                WaveLength.nm 300.0<nm>, ComplexRefractionIndex (Complex (1.4142135623730951, 1.0254061649130814E-08))
                WaveLength.nm 450.0<nm>, ComplexRefractionIndex (Complex (1.4190667922939262, 0.11726278604644001))
                WaveLength.nm 600.0<nm>, ComplexRefractionIndex (Complex (1.4152651785687922, 0.054548379163411681))
                WaveLength.nm 900.0<nm>, ComplexRefractionIndex (Complex (1.4142135639739488, 6.7289661413197214E-05))
                WaveLength.nm 2000.0<nm>, ComplexRefractionIndex (Complex (1.4142135623730951, 4.4982481974802987E-11))
            ]


    [<Fact>]
    member _.``The Forouhi-Bloomer law reproduces its reference index on both sides of the band gap`` () =
        // The AC-B6 a-Si set: Eg = 0.906 eV sits at 1368.5 nm, so 300 to 900 nm absorb and 2000 nm gives k = 0.
        let law : ForouhiBloomerLaw =
            {
                nInf = sqrt 3.453
                a = 0.865
                b = 6.703
                c = 13.237
                bandGap = 0.906
                abscissa = electronVolt
            }

        verifyLawReferences law.complexIndex
            [
                WaveLength.nm 300.0<nm>, ComplexRefractionIndex (Complex (3.4230520145142367, 3.4443626075262639))
                WaveLength.nm 450.0<nm>, ComplexRefractionIndex (Complex (5.0100310741344316, 1.25333719578412))
                WaveLength.nm 600.0<nm>, ComplexRefractionIndex (Complex (4.3504991890846751, 0.31859114776865921))
                WaveLength.nm 900.0<nm>, ComplexRefractionIndex (Complex (3.6859362676891587, 0.032603035269730463))
                WaveLength.nm 2000.0<nm>, ComplexRefractionIndex (Complex (3.1919915065386339, 0.0))
            ]


    [<Fact>]
    member _.``The Brendel-Bormann law reproduces its reference index`` () =
        // The AC-B6 gold set (Rakić 1998): interband absorption at 300 and 450 nm, the free-carrier metal beyond.
        let law : BrendelBormannLaw =
            {
                plasmaFrequency = 9.03
                intrabandStrength = 0.770
                intrabandDamping = 0.050
                strength = [ 0.054; 0.050; 0.312; 0.719; 1.648 ]
                resonance = [ 0.218; 2.885; 4.069; 6.137; 27.97 ]
                damping = [ 0.074; 0.035; 0.083; 0.125; 0.179 ]
                broadening = [ 0.742; 0.349; 0.830; 1.246; 1.795 ]
                abscissa = electronVolt
            }

        verifyLawReferences law.complexIndex
            [
                WaveLength.nm 300.0<nm>, ComplexRefractionIndex (Complex (1.7302516384193967, 1.8741449400944645))
                WaveLength.nm 450.0<nm>, ComplexRefractionIndex (Complex (1.374058131504631, 1.7702875111294298))
                WaveLength.nm 600.0<nm>, ComplexRefractionIndex (Complex (0.24146838171580332, 2.9155245932327909))
                WaveLength.nm 900.0<nm>, ComplexRefractionIndex (Complex (0.23066700009923077, 5.4619876195461128))
                WaveLength.nm 2000.0<nm>, ComplexRefractionIndex (Complex (0.84433172406313572, 12.757953577132453))
            ]
