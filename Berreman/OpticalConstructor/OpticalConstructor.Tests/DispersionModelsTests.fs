namespace OpticalConstructor.Tests

open System.Numerics
open System.Text.Json
open Berreman.Constants
open Berreman.MathNetNumericsMath
open Berreman.Fields
open Berreman.MaterialProperties
open Berreman.Dispersion
open OpticalProperties.Dispersive
open OpticalConstructor.Domain.Units
open OpticalConstructor.Domain.DispersionModels
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Storage
open Xunit

/// Dispersion-model evaluation, anisotropy, thermo-optic, and the by-id resolution
/// seam (§D.5–D.8, §D.12). Proves `evaluate` reproduces the analytic closure shape
/// `Silicon`/`Langasite` build by hand, uniaxial maps to the engine's three-index
/// constructor (AC-D5), the thermo-optic correction is `None`-byte-identical / `Some`
/// first-order with the operating temperature unstored (AC-D8), and `resolveMaterial`
/// is the errors-as-values seam slices 005/014 delegate to.
module DispersionModelsTests =

    let private closeC (tol : float) (a : Complex) (b : Complex) =
        abs (a.Real - b.Real) <= tol && abs (a.Imaginary - b.Imaginary) <= tol

    let private epsClose (tol : float) (e1 : Eps) (e2 : Eps) =
        seq { for i in 0..2 do for j in 0..2 -> closeC tol e1.[i, j] e2.[i, j] } |> Seq.forall id

    /// A non-dispersive constant-index model (no thermo-optic).
    let private cnk n k = ConstantNK { n = n; k = k; wavelengthUnit = Nanometer; thermoOptic = None }

    [<Fact>]
    let ``AC-D4 evaluate Sellmeier reproduces the analytic closure at three wavelengths`` () =
        // Reuses the engine closure shape: read WaveLength.value, normalise to the
        // record's unit, apply n^2 = 1 + Σ bᵢ λ²/(λ² − cᵢ).
        let model = Sellmeier { b = [ 1.04 ]; c = [ 0.006 ]; wavelengthUnit = Micrometer; thermoOptic = None }
        let f = evaluate model
        for lamUm in [ 0.4; 0.5; 0.6 ] do
            let w = toWaveLength Micrometer lamUm
            let lam2 = lamUm * lamUm
            let nExpected = sqrt (1.0 + 1.04 * lam2 / (lam2 - 0.006))
            let got = (f w).value
            Assert.True(abs (got.Real - nExpected) <= 1e-9 && abs got.Imaginary <= 1e-9, $"λ={lamUm}µm: got {got}, expected n={nExpected}")

    [<Fact>]
    let ``the silicon library entry reproduces the engine Silicon preset bit-for-bit`` () =
        // Built-in entries reuse the engine presets directly — no dispersion re-derived.
        let entry = builtInEntries |> List.find (fun e -> e.id = MaterialIds.silicon)
        for nm in [ 400.0<nm>; 500.0<nm>; 600.0<nm> ] do
            let w = WaveLength.nm nm
            let got = entry.properties.epsWithDisp.getEps w
            let expected = siliconOpticalProperties.epsWithDisp.getEps w
            Assert.True(epsClose 1e-12 got expected, $"λ={nm}")

    [<Fact>]
    let ``AC-D5 uniaxial maps to Eps.fromComplexRefractionIndex (n_o, n_e, n_o)`` () =
        // The anisotropy lives INSIDE the serializable EpsWithDispValue (slice 011,
        // superseding the removed AnisotropicModel): the constant uniaxial case and a
        // per-axis dispersive segment tree both map to the engine's (n_o, n_e, n_o).
        let no = createComplex 1.5 0.0 |> ComplexRefractionIndex
        let ne = createComplex 1.65 0.0 |> ComplexRefractionIndex
        let w = WaveLength.nm 500.0<nm>
        let expected = Eps.fromComplexRefractionIndex (no, ne, no)
        let op = toAnisotropicOpticalProperties (EpsWithoutDispValue (UniaxialAbsorbing (no, ne)))
        Assert.True(epsClose 1e-12 (op.epsWithDisp.getEps w) expected, "constant uniaxial value")
        Assert.True(epsClose 1e-12 (uniaxialEps no ne) expected, "direct uniaxialEps helper")
        // Per-axis dispersive route: the same indices as one uniaxial segment tree.
        match toEpsAxis (cnk 1.5 0.0), toEpsAxis (cnk 1.65 0.0) with
        | Ok axisO, Ok axisE ->
            let segment =
                {
                    wavelengthInterval = { lower = WaveLength.nm 400.0<nm>; upper = WaveLength.nm 700.0<nm> }
                    ordinaryDispersion = axisO
                    extraordinaryDispersion = axisE
                }
            let opDisp = toAnisotropicOpticalProperties (EpsWithDispValue (UniaxialDispersive [ segment ]))
            Assert.True(epsClose 1e-12 (opDisp.epsWithDisp.getEps w) expected, "dispersive uniaxial segment tree")
        | other -> Assert.Fail($"expected two lowered axes, got {other}")

    [<Fact>]
    let ``ConstantNK composes to EpsWithoutDisp (no closure overhead)`` () =
        match (toOpticalProperties (cnk 2.0 0.1)).epsWithDisp with
        | EpsWithoutDisp _ -> ()
        | EpsWithDisp _ -> Assert.Fail("ConstantNK must emit EpsWithoutDisp")

    [<Fact>]
    let ``AC-D8 thermoOptic None is byte-identical to the isothermal index`` () =
        let model = cnk 2.0 0.05
        let w = WaveLength.nm 500.0<nm>
        let iso = createComplex 2.0 0.05
        Assert.Equal(iso, (evaluate model w).value)
        // With None, evaluateAt any operating temperature is also identical (no overhead).
        Assert.Equal(iso, (evaluateAt 350.0<K> model w).value)

    [<Fact>]
    let ``AC-D8 Some thermoOptic applies first-order dndT correction at operating T`` () =
        let th = { dndT = 1.0e-4<1/K>; referenceTemperature = 293.15<K> }
        let model = ConstantNK { n = 2.0; k = 0.05; wavelengthUnit = Nanometer; thermoOptic = Some th }
        let w = WaveLength.nm 500.0<nm>
        let got = (evaluateAt 343.15<K> model w).value
        let expectedReal = 2.0 + 1.0e-4 * 50.0 // dndT·(T − ref) = 1e-4 · 50 = 5e-3
        Assert.True(abs (got.Real - expectedReal) <= 1e-12, $"real part {got.Real}")
        Assert.Equal(0.05, got.Imaginary) // k is unchanged
        // At T = referenceTemperature the correction is zero (isothermal).
        Assert.True(abs ((evaluateAt 293.15<K> model w).value.Real - 2.0) <= 1e-12)

    [<Fact>]
    let ``AC-D8 only dndT and referenceTemperature persist through JSON (operating T unstored)`` () =
        let model =
            Sellmeier
                {
                    b = [ 1.04 ]
                    c = [ 0.006 ]
                    wavelengthUnit = Micrometer
                    thermoOptic = Some { dndT = 1.0e-4<1/K>; referenceTemperature = 293.15<K> }
                }
        let json = JsonSerializer.Serialize(model, ProjectJson.options)
        let back = JsonSerializer.Deserialize<DispersionModel>(json, ProjectJson.options)
        Assert.Equal(model, back)
        Assert.Contains("dndT", json)
        Assert.Contains("referenceTemperature", json)
        Assert.DoesNotContain("operatingTemperature", json)

    [<Fact>]
    let ``resolveMaterial returns the concrete tensor for a known id at the supplied wavelength`` () =
        let w = WaveLength.nm 500.0<nm>
        match resolveMaterial standard MaterialIds.silicon w with
        | Ok op -> Assert.True(epsClose 1e-12 op.eps (siliconOpticalProperties.getProperties w).eps)
        | Error e -> Assert.Fail($"expected Ok, got {e}")

    [<Fact>]
    let ``resolveMaterial returns Error UnknownMaterialId for an unknown id and never throws`` () =
        // The error's reason (spec 0033 step 002) names the offending id's Guid string form.
        let unknown = MaterialId.create ()
        match resolveMaterial standard unknown (WaveLength.nm 500.0<nm>) with
        | Error (UnknownMaterialId reason) -> Assert.Contains(string unknown.value, reason)
        | other -> Assert.Fail($"expected Error (UnknownMaterialId _), got {other}")

    [<Fact>]
    let ``a minted MaterialId round-trips create-store-lookup through the library`` () =
        // Spec 0033 step 002 acceptance: create an entry with a MINTED id, store it in a library,
        // and the by-id lookup returns THAT entry's properties; the id also round-trips its Guid
        // string form through the `tryCreate` IO-boundary parse.
        let id = MaterialId.create ()
        let entry =
            {
                id = id
                name = "Round-trip entry"
                category = Glass
                description = None
                properties = siliconOpticalProperties
            }
        let lib : MaterialLibrary = { entries = entry :: standard.entries }
        match resolveMaterialWithDisp lib id with
        | Ok p -> Assert.True(System.Object.ReferenceEquals(p, entry.properties), "lookup must return the STORED entry's properties")
        | Error e -> Assert.Fail($"expected Ok, got {e}")
        Assert.Equal(Some id, MaterialId.tryCreate (string id.value))
        Assert.Equal(None, MaterialId.tryCreate "not-a-guid")

    // ----------------------------------------------------------------------
    // AC-B5 (slice 011): toEpsAxis lowers each analytic model to the
    // serializable per-axis term data of Dispersion.fs, matching `evaluate`
    // across a sampled wavelength grid within tolerance.
    // ----------------------------------------------------------------------

    /// 400–800 nm in 50 nm steps.
    let private visibleGrid : WaveLength list =
        [ 400.0 .. 50.0 .. 800.0 ] |> List.map (fun lamNm -> WaveLength.nm (lamNm * 1.0<nm>))

    /// The lowered term data must reproduce `evaluate` at every grid wavelength.
    let private assertLoweringMatchesEvaluate (tol : float) (model : DispersionModel) =
        match toEpsAxis model with
        | Error e -> Assert.Fail($"expected a lowering, got Error {e}")
        | Ok axis ->
            let f = evaluate model
            for w in visibleGrid do
                let got = (axis.complexIndex w).value
                let expected = (f w).value
                Assert.True(closeC tol got expected, $"λ={w}: lowered {got} vs evaluate {expected}")

    [<Fact>]
    let ``AC-B5 Sellmeier lowering matches evaluate on the grid (micrometer abscissa)`` () =
        // BK7 (three oscillators, c in µm²). Lowered as ComplexEps with REAL term
        // values: RealNK cannot express n = √(1 + Σ) — the √ lives in
        // ComplexEps.complexIndex, which evaluates exactly √(1 + Σ).
        let model =
            Sellmeier
                {
                    b = [ 1.03961212; 0.231792344; 1.01046945 ]
                    c = [ 0.00600069867; 0.0200179144; 103.560653 ]
                    wavelengthUnit = Micrometer
                    thermoOptic = None
                }
        match toEpsAxis model with
        | Ok (ComplexEps _) -> ()
        | other -> Assert.Fail($"Sellmeier must lower to ComplexEps (√ of the ε term sum), got {other}")
        assertLoweringMatchesEvaluate 1e-8 model

    [<Fact>]
    let ``AC-B5 Sellmeier lowering matches evaluate on the grid (electron-volt abscissa)`` () =
        // Reciprocal abscissa a = k/x: each oscillator collapses to the single
        // inverse term Bᵢ·k²/(k² − cᵢ·x²).
        let model = Sellmeier { b = [ 1.0 ]; c = [ 0.5 ]; wavelengthUnit = ElectronVolt; thermoOptic = None }
        assertLoweringMatchesEvaluate 1e-8 model

    [<Fact>]
    let ``AC-B5 Cauchy lowering is RealNK Laurent terms and matches evaluate on the grid`` () =
        let model = Cauchy { a = 1.5046; b = 0.0042; c = 0.00003; wavelengthUnit = Micrometer; thermoOptic = None }
        match toEpsAxis model with
        | Ok (RealNK (_, k)) -> Assert.True(List.isEmpty k.terms, "k must be the zero formula")
        | other -> Assert.Fail($"Cauchy must lower to RealNK, got {other}")
        assertLoweringMatchesEvaluate 1e-9 model

    [<Fact>]
    let ``AC-B5 Cauchy lowering matches evaluate on the grid (electron-volt abscissa)`` () =
        // a = k/x turns the Laurent form A + B/a² + C/a⁴ into a plain polynomial in x.
        let model = Cauchy { a = 1.4; b = 0.5; c = 0.02; wavelengthUnit = ElectronVolt; thermoOptic = None }
        assertLoweringMatchesEvaluate 1e-9 model

    [<Fact>]
    let ``AC-B5 ConstantNK lowering is RealNK constants and matches evaluate on the grid`` () =
        let model = cnk 2.0 0.1
        match toEpsAxis model with
        | Ok (RealNK _) -> ()
        | other -> Assert.Fail($"ConstantNK must lower to RealNK constants, got {other}")
        assertLoweringMatchesEvaluate 1e-12 model

    [<Fact>]
    let ``AC-B5 Lorentz lowering matches evaluate on the grid (micrometer abscissa)`` () =
        // Linear abscissa: the literal inverse term { r²; −i·d; −1 } per oscillator.
        let model =
            Lorentz
                {
                    epsInf = 2.25
                    strength = [ 2.0 ]
                    resonance = [ 0.25 ]
                    damping = [ 0.05 ]
                    wavelengthUnit = Micrometer
                    thermoOptic = None
                }
        assertLoweringMatchesEvaluate 1e-8 model

    [<Fact>]
    let ``AC-B5 Lorentz lowering matches evaluate on the grid (electron-volt abscissa)`` () =
        // The conventional eV tabulation: a = k/x is reciprocal, each oscillator is
        // s·x²/(r²·x² − i·d·k·x − k²) — lowered exactly by partial fractions.
        let model =
            Lorentz
                {
                    epsInf = 1.5
                    strength = [ 1.5; 0.8 ]
                    resonance = [ 4.0; 6.5 ]
                    damping = [ 0.3; 0.6 ]
                    wavelengthUnit = ElectronVolt
                    thermoOptic = None
                }
        assertLoweringMatchesEvaluate 1e-8 model

    [<Fact>]
    let ``AC-B5 Lorentz eV critical damping takes the double-pole branch and matches evaluate`` () =
        // d = 2r makes the oscillator denominator a perfect square (one double pole).
        let model =
            Lorentz
                {
                    epsInf = 2.0
                    strength = [ 1.0 ]
                    resonance = [ 2.0 ]
                    damping = [ 4.0 ]
                    wavelengthUnit = ElectronVolt
                    thermoOptic = None
                }
        assertLoweringMatchesEvaluate 1e-8 model

    [<Fact>]
    let ``AC-B5 Drude lowering matches evaluate on the grid (electron-volt abscissa)`` () =
        let model =
            Drude
                {
                    epsInf = 1.0
                    plasmaFrequency = 9.0
                    dampingFrequency = 0.05
                    wavelengthUnit = ElectronVolt
                    thermoOptic = None
                }
        assertLoweringMatchesEvaluate 1e-8 model

    [<Fact>]
    let ``AC-B5 Drude eV with zero damping lowers to a plain polynomial and matches evaluate`` () =
        let model =
            Drude
                {
                    epsInf = 1.0
                    plasmaFrequency = 9.0
                    dampingFrequency = 0.0
                    wavelengthUnit = ElectronVolt
                    thermoOptic = None
                }
        assertLoweringMatchesEvaluate 1e-8 model

    [<Fact>]
    let ``AC-B5 transcendental models are a typed lowering error and evaluate directly`` () =
        // TaucLorentz / GaussianOscillator are not finite term sums (band-gap step,
        // exp): toEpsAxis reports the typed error, and toOpticalProperties keeps them
        // fully working by wrapping `evaluate` in the engine EpsWithDisp closure.
        let tl =
            TaucLorentz
                {
                    epsInf = 1.5
                    amplitude = 100.0
                    resonance = 3.5
                    broadening = 0.8
                    bandGap = 2.2
                    wavelengthUnit = ElectronVolt
                    thermoOptic = None
                }
        let go =
            GaussianOscillator
                {
                    epsInf = 2.0
                    amplitude = 0.5
                    energy = 2.5
                    broadening = 0.4
                    wavelengthUnit = ElectronVolt
                    thermoOptic = None
                }
        for model in [ tl; go ] do
            match toEpsAxis model with
            | Error (NotAFiniteTermSum _) -> ()
            | other -> Assert.Fail($"expected Error (NotAFiniteTermSum _), got {other}")
            let f = evaluate model
            let op = toOpticalProperties model
            for w in visibleGrid do
                let expected = Eps.fromComplexRefractionIndex (f w)
                Assert.True(epsClose 1e-12 (op.epsWithDisp.getEps w) expected, $"λ={w}")

    [<Fact>]
    let ``AC-B5 SumOfTerms is the identity under toEpsAxis and evaluates through complexIndex`` () =
        // The raw escape hatch carries term data as-is: no tabulation unit of its own
        // (each formula embeds its wavelengthScale — the canonical Meter is reported)
        // and no thermo-optic record.
        let axis =
            RealNK (
                { terms = [ { lambda = 0.0; coefficients = [| 1.7; 0.1 |]; power = 1; multiplier = 1.0 } ]; wavelengthScale = 1.0e-6 },
                { terms = []; wavelengthScale = 1.0e-6 })
        let model = SumOfTerms axis
        Assert.Equal(Ok axis, toEpsAxis model)
        Assert.Equal(Meter, wavelengthUnitOf model)
        Assert.Equal(None, thermoOpticOf model)
        let f = evaluate model
        for w in visibleGrid do
            Assert.Equal((axis.complexIndex w).value, (f w).value)

    [<Fact>]
    let ``AC-B5 toEpsValue short-circuits ConstantNK and builds a single-segment tree otherwise`` () =
        match toEpsValue (cnk 2.0 0.1) with
        | Ok (EpsWithoutDispValue (IsotropicAbsorbing _)) -> ()
        | other -> Assert.Fail($"ConstantNK must become the constant value, got {other}")
        let sellmeier = Sellmeier { b = [ 1.04 ]; c = [ 0.006 ]; wavelengthUnit = Micrometer; thermoOptic = None }
        match toEpsValue sellmeier with
        | Ok (EpsWithDispValue (IsotropicDispersive [ _ ])) -> ()
        | other -> Assert.Fail($"a lowerable model must become one isotropic segment, got {other}")
        let tl =
            TaucLorentz
                {
                    epsInf = 1.5
                    amplitude = 100.0
                    resonance = 3.5
                    broadening = 0.8
                    bandGap = 2.2
                    wavelengthUnit = ElectronVolt
                    thermoOptic = None
                }
        match toEpsValue tl with
        | Error (NotAFiniteTermSum _) -> ()
        | other -> Assert.Fail($"a transcendental model must be a typed lowering error, got {other}")

    [<Fact>]
    let ``AC-B5 toOpticalProperties routes a lowerable model through the serializable tree`` () =
        let model = Sellmeier { b = [ 1.04 ]; c = [ 0.006 ]; wavelengthUnit = Micrometer; thermoOptic = None }
        let op = toOpticalProperties model
        let f = evaluate model
        for w in visibleGrid do
            let expected = Eps.fromComplexRefractionIndex (f w)
            Assert.True(epsClose 1e-8 (op.epsWithDisp.getEps w) expected, $"λ={w}")

    // ----------------------------------------------------------------------
    // AC-B6 (slice 012): ForouhiBloomer / BrendelBormann evaluate against
    // documented reference values for one published coefficient set each
    // (Forouhi & Bloomer 1986; Rakić et al. 1998 Au).
    // ----------------------------------------------------------------------

    /// The published Forouhi–Bloomer a-Si coefficient set (Horiba TN13, which
    /// implements the 1986 five-parameter form and tabulates ε∞ = 3.453 —
    /// hence n∞ = √ε∞ — A = 0.865 eV, B = 6.703 eV, C = 13.237 eV²,
    /// Eg = 0.906 eV over 0.6–5 eV).
    let private forouhiBloomerASi =
        ForouhiBloomer
            {
                nInf = sqrt 3.453
                a = 0.865
                b = 6.703
                c = 13.237
                bandGap = 0.906
                wavelengthUnit = ElectronVolt
                thermoOptic = None
            }

    /// Rakić et al. 1998 (Appl. Opt. 37, 5271) Brendel–Bormann parameters for
    /// gold — the set behind the refractiveindex.info Au/Rakic-BB entry.
    let private brendelBormannGold =
        BrendelBormann
            {
                plasmaFrequency = 9.03
                intrabandStrength = 0.770
                intrabandDamping = 0.050
                strength = [ 0.054; 0.050; 0.312; 0.719; 1.648 ]
                resonance = [ 0.218; 2.885; 4.069; 6.137; 27.97 ]
                damping = [ 0.074; 0.035; 0.083; 0.125; 0.179 ]
                broadening = [ 0.742; 0.349; 0.830; 1.246; 1.795 ]
                wavelengthUnit = ElectronVolt
                thermoOptic = None
            }

    [<Fact>]
    let ``AC-B6 evaluate ForouhiBloomer reproduces the published a-Si reference values`` () =
        // The TN13 note displays n = 3.182, k = 0.000 at E = 0.6 eV for this
        // set (the closed form gives n(0.6) = 3.1818311; 0.6 eV sits below the
        // gap, so the Θ ∝ (E − Eg)² step makes k exactly zero). Above the gap
        // both branches are pinned by the published closed form: Q = √(4C−B²)/2
        // = 1.4157852, B₀ = −2.4292240, C₀ = 14.1313281; at E = 3 eV the
        // denominator is 9 − 3B + C = 2.128, so n = n∞ + (3B₀ + C₀)/2.128 =
        // 5.0742287 and k = A·(3 − Eg)²/2.128 = 1.7823699.
        let f = evaluate forouhiBloomerASi
        let below = (f (toWaveLength ElectronVolt 0.6)).value
        Assert.True(abs (below.Real - 3.182) <= 5e-4, $"n(0.6 eV) = {below.Real}, expected 3.182")
        Assert.Equal(0.0, below.Imaginary)
        let above = (f (toWaveLength ElectronVolt 3.0)).value
        Assert.True(abs (above.Real - 5.0742287) <= 1e-6, $"n(3 eV) = {above.Real}, expected 5.0742287")
        Assert.True(abs (above.Imaginary - 1.7823699) <= 1e-6, $"k(3 eV) = {above.Imaginary}, expected 1.7823699")

    [<Fact>]
    let ``AC-B6 evaluate BrendelBormann reproduces the Rakić 1998 gold reference values`` () =
        // Expected n/k are the CC0 refractiveindex.info Au/Rakic-BB tabulation
        // rows (quoted verbatim), computed from the same published parameter
        // set. 2e-3 absolute covers the table's 5-significant-digit rounding
        // of both λ and n/k plus the seam's evNmProduct vs the table's h·c
        // (1.6e-6 relative).
        let f = evaluate brendelBormannGold
        let rows =
            [
                0.49712, 0.89849, 1.8312
                0.62346, 0.20533, 3.1621
                1.0129, 0.28761, 6.2718
                2.0306, 0.86294, 12.953
            ]
        for lamUm, nExpected, kExpected in rows do
            let got = (f (toWaveLength Micrometer lamUm)).value
            Assert.True(abs (got.Real - nExpected) <= 2e-3, $"λ={lamUm}µm: n = {got.Real}, expected {nExpected}")
            Assert.True(abs (got.Imaginary - kExpected) <= 2e-3, $"λ={lamUm}µm: k = {got.Imaginary}, expected {kExpected}")

    [<Fact>]
    let ``AC-B6 the BrendelBormann intraband part is the Drude free-carrier term`` () =
        // With no oscillators the model is ε = 1 − f₀·ωp²/(E² + i·Γ₀·E) —
        // exactly the existing Drude case with plasmaFrequency √f₀·ωp.
        let bb =
            BrendelBormann
                {
                    plasmaFrequency = 9.03
                    intrabandStrength = 0.770
                    intrabandDamping = 0.050
                    strength = []
                    resonance = []
                    damping = []
                    broadening = []
                    wavelengthUnit = ElectronVolt
                    thermoOptic = None
                }
        let drude =
            Drude
                {
                    epsInf = 1.0
                    plasmaFrequency = sqrt 0.770 * 9.03
                    dampingFrequency = 0.050
                    wavelengthUnit = ElectronVolt
                    thermoOptic = None
                }
        let fBb = evaluate bb
        let fDrude = evaluate drude
        for w in visibleGrid do
            Assert.True(closeC 1e-12 (fBb w).value (fDrude w).value, $"λ={w}")

    [<Fact>]
    let ``AC-B6 a narrow BrendelBormann oscillator collapses to the matching Lorentz oscillator`` () =
        // σ → 0 turns the Gaussian superposition back into a single Lorentz
        // oscillator with strength f·ωp²: χ → f·ωp²/(ω₀² − E² − i·Γ·E). The
        // deviation is O(σ²), so σ = 1e-3 pins the Voigt closed form and the
        // Faddeeva evaluation against the independently-implemented Lorentz
        // case well below 1e-4.
        let bb =
            BrendelBormann
                {
                    plasmaFrequency = 5.0
                    intrabandStrength = 0.0
                    intrabandDamping = 0.06
                    strength = [ 0.4 ]
                    resonance = [ 3.1 ]
                    damping = [ 0.35 ]
                    broadening = [ 1.0e-3 ]
                    wavelengthUnit = ElectronVolt
                    thermoOptic = None
                }
        let lorentz =
            Lorentz
                {
                    epsInf = 1.0
                    strength = [ 0.4 * 5.0 * 5.0 ]
                    resonance = [ 3.1 ]
                    damping = [ 0.35 ]
                    wavelengthUnit = ElectronVolt
                    thermoOptic = None
                }
        let fBb = evaluate bb
        let fLorentz = evaluate lorentz
        for w in visibleGrid do
            Assert.True(closeC 1e-4 (fBb w).value (fLorentz w).value, $"λ={w}: BB {(fBb w).value} vs Lorentz {(fLorentz w).value}")

    [<Fact>]
    let ``AC-B6 ForouhiBloomer and BrendelBormann are a typed lowering error and evaluate directly`` () =
        // Slice 011's recorded route for non-finite-term models: `ComplexEps`
        // carries pure term data and cannot hold a closure, so toEpsAxis
        // surfaces the typed error and toOpticalProperties keeps both models
        // fully working by wrapping `evaluate` in the engine EpsWithDisp
        // closure.
        for model in [ forouhiBloomerASi; brendelBormannGold ] do
            Assert.Equal(ElectronVolt, wavelengthUnitOf model)
            Assert.Equal(None, thermoOpticOf model)
            match toEpsAxis model with
            | Error (NotAFiniteTermSum _) -> ()
            | other -> Assert.Fail($"expected Error (NotAFiniteTermSum _), got {other}")
            let f = evaluate model
            let op = toOpticalProperties model
            for w in visibleGrid do
                let expected = Eps.fromComplexRefractionIndex (f w)
                Assert.True(epsClose 1e-12 (op.epsWithDisp.getEps w) expected, $"λ={w}")
