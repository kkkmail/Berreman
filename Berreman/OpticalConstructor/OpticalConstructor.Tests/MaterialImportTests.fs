namespace OpticalConstructor.Tests

open System.IO
open System.Numerics
open Berreman.Constants
open Berreman.MathNetNumericsMath
open Berreman.Fields
open Berreman.MaterialProperties
open Berreman.Dispersion
open Analytics.Variables
open OpticalConstructor.Domain.Units
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Storage.MaterialImport
open Xunit

/// refractiveindex.info YAML / CSV import + CSV export (§D.9, AC-D3). Proves λ is
/// reduced to the engine meter base on import (µm YAML, nm CSV), the resulting
/// `getEps` reproduces the tabulated n,k, tabulated entries interpolate linearly,
/// and `exportCsv` round-trips through `importCsv`.
module MaterialImportTests =

    let private fixtures = Path.Combine(System.AppContext.BaseDirectory, "fixtures")
    let private readFixture name = File.ReadAllText(Path.Combine(fixtures, name))

    let private closeC (tol : float) (a : Complex) (b : Complex) =
        abs (a.Real - b.Real) <= tol && abs (a.Imaginary - b.Imaginary) <= tol

    let private epsAt (entry : MaterialEntry) (w : WaveLength) = (entry.properties.epsWithDisp.getEps w).[0, 0]

    let private nkSquared n k =
        (Eps.fromComplexRefractionIndex (ComplexRefractionIndex(createComplex n k))).[0, 0]

    [<Fact>]
    let ``AC-D3 refractiveindex.info µm YAML import reduces λ to meters and reproduces n,k`` () =
        match importRefractiveIndexInfo (readFixture "Si.yml") with
        | Error e -> Assert.Fail($"{e}")
        | Ok entry ->
            // 0.4 µm is a tabulated sample (n = 5.570, k = 0.387).
            let got = epsAt entry (toWaveLength Micrometer 0.4)
            Assert.True(closeC 1e-9 got (nkSquared 5.570 0.387), $"got {got}")

    [<Fact>]
    let ``nm CSV import reduces λ to meters and reproduces n,k`` () =
        match importCsv (readFixture "sample-nk.csv") with
        | Error e -> Assert.Fail($"{e}")
        | Ok entry ->
            // 500 nm is a tabulated sample (n = 1.480, k = 0.005).
            let got = epsAt entry (toWaveLength Nanometer 500.0)
            Assert.True(closeC 1e-9 got (nkSquared 1.480 0.005), $"got {got}")

    [<Fact>]
    let ``tabulated import interpolates linearly between samples`` () =
        match importRefractiveIndexInfo (readFixture "Si.yml") with
        | Error e -> Assert.Fail($"{e}")
        | Ok entry ->
            // 0.55 µm is the midpoint of 0.5 (4.293, 0.045) and 0.6 (3.939, 0.025).
            let got = epsAt entry (toWaveLength Micrometer 0.55)
            Assert.True(closeC 1e-9 got (nkSquared 4.116 0.035), $"got {got}")

    /// The imported entry MUST be editable (spec 0033 step 025): a `Some`
    /// complexity whose eps is the dispersive segment tree (`EpsWithDispValue`).
    let private assertEditableDispersive (entry : MaterialEntry) =
        match entry.complexity with
        | Some c ->
            match c.eps with
            | EpsWithDispValue _ -> ()
            | EpsWithoutDispValue _ -> Assert.Fail "expected a dispersive EpsWithDispValue-backed complexity"
        | None -> Assert.Fail "expected an editable complexity on the imported formula entry"

    /// Import a formula fixture and pin the evaluated index (via the entry's
    /// engine `getEps` path) against the published-formula reference `expectedN`
    /// (hand-computed in each test from the fixture coefficients) at sample
    /// wavelengths in µm — plus the editable-complexity shape.
    let private assertFormulaIndex (fixture : string) (expectedN : float -> float) (samplesMkm : float list) =
        match importRefractiveIndexInfo (readFixture fixture) with
        | Error e -> Assert.Fail($"{e}")
        | Ok entry ->
            assertEditableDispersive entry
            for lam in samplesMkm do
                let got = epsAt entry (toWaveLength Micrometer lam)
                let want = nkSquared (expectedN lam) 0.0
                Assert.True(closeC 1e-9 got want, $"{fixture} at {lam} µm: got {got}, want {want}")

    [<Fact>]
    let ``formula 1 (Sellmeier, squared resonances) lowers to editable term data and reproduces n`` () =
        // n² = 1 + 0.6961663·λ²/(λ² − 0.0684043²) + 0.4079426·λ²/(λ² − 0.1162414²)
        let expected (lam : float) =
            let l2 = lam * lam
            sqrt (1.0 + 0.6961663 * l2 / (l2 - 0.0684043 ** 2.0) + 0.4079426 * l2 / (l2 - 0.1162414 ** 2.0))
        assertFormulaIndex "formula1.yml" expected [ 0.5; 1.5 ]

    [<Fact>]
    let ``formula 2 (Sellmeier-2, unsquared resonances) lowers to editable term data and reproduces n`` () =
        // n² = 1 + 0.5 + 1.0·λ²/(λ² − 0.01) + 0.2·λ²/(λ² − 100)
        let expected (lam : float) =
            let l2 = lam * lam
            sqrt (1.0 + 0.5 + 1.0 * l2 / (l2 - 0.01) + 0.2 * l2 / (l2 - 100.0))
        assertFormulaIndex "formula2.yml" expected [ 0.5; 1.5 ]

    [<Fact>]
    let ``formula 2 segment carries the page's wavelength_range in µm`` () =
        match importRefractiveIndexInfo (readFixture "formula2.yml") with
        | Error e -> Assert.Fail($"{e}")
        | Ok entry ->
            match entry.complexity with
            | Some { eps = EpsWithDispValue (IsotropicDispersive [ segment ]) } ->
                Assert.Equal(toWaveLength Micrometer 0.3, segment.wavelengthInterval.lower)
                Assert.Equal(toWaveLength Micrometer 2.5, segment.wavelengthInterval.upper)
            | other -> Assert.Fail($"expected a single isotropic dispersive segment, got {other}")

    [<Fact>]
    let ``formula 3 (polynomial) lowers to editable term data and reproduces n`` () =
        // n² = 2.2 + 0.02·λ² + 0.01·λ⁻²
        let expected (lam : float) =
            let l2 = lam * lam
            sqrt (2.2 + 0.02 * l2 + 0.01 / l2)
        assertFormulaIndex "formula3.yml" expected [ 0.5; 1.5 ]

    [<Fact>]
    let ``formula 4 (RefractiveIndex.INFO formula) lowers to editable term data and reproduces n`` () =
        // n² = 1 + 0.8·λ²/(λ² − 0.1²) + 0.05·λ/(λ² − 0.2²) + 0.01·λ²
        let expected (lam : float) =
            let l2 = lam * lam
            sqrt (1.0 + 0.8 * l2 / (l2 - 0.01) + 0.05 * lam / (l2 - 0.04) + 0.01 * l2)
        assertFormulaIndex "formula4.yml" expected [ 0.5; 1.0 ]

    [<Fact>]
    let ``formula 5 (Cauchy) lowers to editable term data and reproduces n`` () =
        // n = 1.45 + 0.0048·λ⁻² + 0.0001·λ⁻⁴ (at 0.5 µm exactly 1.4708)
        let expected (lam : float) =
            let l2 = lam * lam
            1.45 + 0.0048 / l2 + 0.0001 / (l2 * l2)
        assertFormulaIndex "formula5.yml" expected [ 0.5; 1.0 ]

    [<Fact>]
    let ``formula 6 (gases) lowers to editable term data and reproduces n`` () =
        // n = 1 + 0.0001 + 0.01/(50 − λ⁻²) + 0.001/(10 − λ⁻²)
        let expected (lam : float) =
            let invL2 = 1.0 / (lam * lam)
            1.0 + 0.0001 + 0.01 / (50.0 - invL2) + 0.001 / (10.0 - invL2)
        assertFormulaIndex "formula6.yml" expected [ 0.5; 1.0 ]

    [<Fact>]
    let ``formula 7 (Herzberger) lowers to editable term data and reproduces n`` () =
        // n = 2.4 + 0.05/(λ² − 0.028) + 0.001/(λ² − 0.028)² − 0.01·λ² + 0.002·λ⁴ − 0.0001·λ⁶
        let expected (lam : float) =
            let l2 = lam * lam
            let d = l2 - 0.028
            2.4 + 0.05 / d + 0.001 / (d * d) - 0.01 * l2 + 0.002 * l2 * l2 - 0.0001 * l2 * l2 * l2
        assertFormulaIndex "formula7.yml" expected [ 1.0; 2.0 ]

    [<Fact>]
    let ``formulas 8 and 9 produce the typed unsupported-formula error`` () =
        match importRefractiveIndexInfo (readFixture "formula8.yml") with
        | Error (UnsupportedFormula (8, _)) -> ()
        | other -> Assert.Fail($"formula 8: expected UnsupportedFormula, got {other}")
        match importRefractiveIndexInfo (readFixture "formula9.yml") with
        | Error (UnsupportedFormula (9, _)) -> ()
        | other -> Assert.Fail($"formula 9: expected UnsupportedFormula, got {other}")

    [<Fact>]
    let ``exportCsv samples n,k over the range and round-trips through importCsv`` () =
        let entry =
            match importCsv (readFixture "sample-nk.csv") with
            | Ok e -> e
            | Error e -> failwith (string e)
        let range : Range<WaveLength> =
            { startValue = WaveLength.nm 450.0<nm>; endValue = WaveLength.nm 650.0<nm>; numberOfPoints = 4 }
        let csv = exportCsv entry range
        Assert.Contains("wavelength_nm,n,k", csv)
        match importCsv csv with
        | Ok back ->
            let w = toWaveLength Nanometer 500.0 // a shared sample of both tables
            Assert.True(closeC 1e-6 (epsAt entry w) (epsAt back w), "round-trip n,k at 500 nm")
        | Error e -> Assert.Fail($"{e}")
