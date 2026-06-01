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
        let entry = builtInEntries |> List.find (fun e -> e.id = "silicon")
        for nm in [ 400.0<nm>; 500.0<nm>; 600.0<nm> ] do
            let w = WaveLength.nm nm
            let got = entry.properties.epsWithDisp.getEps w
            let expected = siliconOpticalProperties.epsWithDisp.getEps w
            Assert.True(epsClose 1e-12 got expected, $"λ={nm}")

    [<Fact>]
    let ``AC-D5 uniaxial maps to Eps.fromComplexRefractionIndex (n_o, n_e, n_o)`` () =
        let no = createComplex 1.5 0.0 |> ComplexRefractionIndex
        let ne = createComplex 1.65 0.0 |> ComplexRefractionIndex
        let model = Uniaxial(cnk 1.5 0.0, cnk 1.65 0.0)
        let op = toAnisotropicOpticalProperties model
        let w = WaveLength.nm 500.0<nm>
        let expected = Eps.fromComplexRefractionIndex (no, ne, no)
        Assert.True(epsClose 1e-12 (op.epsWithDisp.getEps w) expected, "dispersive uniaxial closure")
        Assert.True(epsClose 1e-12 (uniaxialEps no ne) expected, "direct uniaxialEps helper")

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
        match resolveMaterial standard "silicon" w with
        | Ok op -> Assert.True(epsClose 1e-12 op.eps (siliconOpticalProperties.getProperties w).eps)
        | Error e -> Assert.Fail($"expected Ok, got {e}")

    [<Fact>]
    let ``resolveMaterial returns Error UnknownMaterialId for an unknown id and never throws`` () =
        match resolveMaterial standard "no-such-material" (WaveLength.nm 500.0<nm>) with
        | Error (UnknownMaterialId id) -> Assert.Equal("no-such-material", id)
        | other -> Assert.Fail($"expected Error (UnknownMaterialId _), got {other}")
