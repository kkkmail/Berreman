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
open OpticalConstructor.Domain.MaterialComplexityEditor
open OpticalConstructor.Storage.MaterialImport
open Xunit

/// refractiveindex.info YAML / CSV import + CSV export (§D.9, AC-D3). Proves λ is
/// reduced to the engine meter base on import (µm YAML, nm CSV), the resulting
/// `getEps` reproduces the tabulated n,k, tabulated entries interpolate linearly,
/// and `exportCsv` round-trips through `NkCsvText.importMaterial`.
module MaterialImportTests =

    let private fixtures = Path.Combine(System.AppContext.BaseDirectory, "fixtures")
    let private readFixture name = File.ReadAllText(Path.Combine(fixtures, name))
    let private readYaml (name : string) : RefractiveIndexInfoText = RefractiveIndexInfoText (readFixture name)
    let private readCsv (name : string) : NkCsvText = NkCsvText (readFixture name)

    let private closeC (tol : float) (a : Complex) (b : Complex) =
        abs (a.Real - b.Real) <= tol && abs (a.Imaginary - b.Imaginary) <= tol

    let private epsAt (entry : MaterialEntry) (w : WaveLength) = (entry.properties.epsWithDisp.getEps w).[0, 0]

    let private nkSquared n k =
        (Eps.fromComplexRefractionIndex (ComplexRefractionIndex(Complex.create n k))).[0, 0]

    [<Fact>]
    let ``AC-D3 refractiveindex.info µm YAML import reduces λ to meters and reproduces n,k`` () =
        match (readYaml "Si.yml").importMaterial with
        | Error e -> Assert.Fail($"{e}")
        | Ok entry ->
            // 0.4 µm is a tabulated sample (n = 5.570, k = 0.387).
            let got = epsAt entry (toWaveLength Micrometer (UnitMagnitude 0.4))
            Assert.True(closeC 1e-9 got (nkSquared 5.570 0.387), $"got {got}")

    [<Fact>]
    let ``nm CSV import reduces λ to meters and reproduces n,k`` () =
        match (readCsv "sample-nk.csv").importMaterial with
        | Error e -> Assert.Fail($"{e}")
        | Ok entry ->
            // 500 nm is a tabulated sample (n = 1.480, k = 0.005).
            let got = epsAt entry (toWaveLength Nanometer (UnitMagnitude 500.0))
            Assert.True(closeC 1e-9 got (nkSquared 1.480 0.005), $"got {got}")

    [<Fact>]
    let ``tabulated import interpolates linearly between samples`` () =
        match (readYaml "Si.yml").importMaterial with
        | Error e -> Assert.Fail($"{e}")
        | Ok entry ->
            // 0.55 µm is the midpoint of 0.5 (4.293, 0.045) and 0.6 (3.939, 0.025).
            let got = epsAt entry (toWaveLength Micrometer (UnitMagnitude 0.55))
            Assert.True(closeC 1e-9 got (nkSquared 4.116 0.035), $"got {got}")

    /// One fixture row written out by hand, λ in the file's own unit, so no fact parses a
    /// fixture a second way (spec 0050 step 1).
    type private TabulatedSample =
        {
            magnitude : UnitMagnitude
            n : float
            k : float
        }

    type private TabulatedFixture =
        {
            waveLengthUnit : UnitOfMeasure
            samples : TabulatedSample list
        }

        member this.waveLengthAt (magnitude : UnitMagnitude) : WaveLength = toWaveLength this.waveLengthUnit magnitude

        member this.first : WaveLength = this.waveLengthAt (List.head this.samples).magnitude

        member this.last : WaveLength = this.waveLengthAt (List.last this.samples).magnitude

    let private siYmlRows : TabulatedFixture =
        {
            waveLengthUnit = Micrometer
            samples =
                [
                    { magnitude = UnitMagnitude 0.250; n = 1.694; k = 3.666 }
                    { magnitude = UnitMagnitude 0.300; n = 5.020; k = 4.220 }
                    { magnitude = UnitMagnitude 0.400; n = 5.570; k = 0.387 }
                    { magnitude = UnitMagnitude 0.500; n = 4.293; k = 0.045 }
                    { magnitude = UnitMagnitude 0.600; n = 3.939; k = 0.025 }
                    { magnitude = UnitMagnitude 0.700; n = 3.785; k = 0.013 }
                ]
        }

    let private sampleNkCsvRows : TabulatedFixture =
        {
            waveLengthUnit = Nanometer
            samples =
                [
                    { magnitude = UnitMagnitude 400.0; n = 1.500; k = 0.010 }
                    { magnitude = UnitMagnitude 500.0; n = 1.480; k = 0.005 }
                    { magnitude = UnitMagnitude 600.0; n = 1.460; k = 0.002 }
                    { magnitude = UnitMagnitude 700.0; n = 1.450; k = 0.001 }
                ]
        }

    /// A tabulated import is axis data (spec 0050 step 1, 001 §B): one isotropic dispersive
    /// segment over the sample range, view-only (a table is not editable term data), and
    /// `properties` IS `complexity.toProperties` (the seed invariant).
    let private assertViewOnlyTabulatedSegment (fixture : TabulatedFixture) (imported : Result<MaterialEntry, ImportError>) =
        match imported with
        | Error e -> Assert.Fail($"{e}")
        | Ok entry ->
            match entry.complexity with
            | None -> Assert.Fail "expected the tabulated import to carry a dispersive complexity, got a None complexity"
            | Some c ->
                match c.eps with
                | EpsWithDispValue (IsotropicDispersive [ segment ]) ->
                    let lower = segment.wavelengthInterval.lower.value
                    let upper = segment.wavelengthInterval.upper.value
                    Assert.True(abs (lower - fixture.first.value) <= 1e-15<meter>, $"segment lower {lower}, want {fixture.first.value}")
                    Assert.True(abs (upper - fixture.last.value) <= 1e-15<meter>, $"segment upper {upper}, want {fixture.last.value}")
                | other -> Assert.Fail($"expected a single isotropic dispersive segment, got {other}")
                match c.magnetic, c.active with
                | None, None -> ()
                | magnetic, active -> Assert.Fail($"expected no magnetic and no active aspect, got {magnetic} and {active}")
                Assert.False(c.isEditable, "a tabulated table is not editable term data")
                for sample in fixture.samples do
                    let w = fixture.waveLengthAt sample.magnitude
                    Assert.True(entry.properties.epsWithDisp.getEps w = c.toProperties.epsWithDisp.getEps w, $"properties = complexity.toProperties at {w.value}")

    [<Fact>]
    let ``a tabulated refractiveindex.info page imports as one view-only dispersive segment over its sample range`` () =
        assertViewOnlyTabulatedSegment siYmlRows (readYaml "Si.yml").importMaterial

    [<Fact>]
    let ``an n,k CSV imports as one view-only dispersive segment over its sample range`` () =
        assertViewOnlyTabulatedSegment sampleNkCsvRows (readCsv "sample-nk.csv").importMaterial

    /// Every sample, and the linear interpolation at the midpoint of every neighbouring pair: green
    /// on the closure and on the table that replaces it, so no number moves (spec 0050 step 1).
    let private assertSamplesAndMidpoints (fixture : TabulatedFixture) (imported : Result<MaterialEntry, ImportError>) =
        match imported with
        | Error e -> Assert.Fail($"{e}")
        | Ok entry ->
            for sample in fixture.samples do
                let w = fixture.waveLengthAt sample.magnitude
                let got = epsAt entry w
                Assert.True(closeC 1e-12 got (nkSquared sample.n sample.k), $"sample at {w.value}: got {got}")
            for (a, b) in List.pairwise fixture.samples do
                let w = fixture.waveLengthAt (UnitMagnitude ((a.magnitude.value + b.magnitude.value) / 2.0))
                let got = epsAt entry w
                let want = nkSquared ((a.n + b.n) / 2.0) ((a.k + b.k) / 2.0)
                Assert.True(closeC 1e-12 got want, $"midpoint at {w.value}: got {got}, want {want}")

    [<Fact>]
    let ``tabulated imports reproduce every sample and the linear interpolation between neighbours`` () =
        assertSamplesAndMidpoints siYmlRows (readYaml "Si.yml").importMaterial
        assertSamplesAndMidpoints sampleNkCsvRows (readCsv "sample-nk.csv").importMaterial

    /// The table a tabulated import carries: the fixture's rows in ascending λ, in metres, read
    /// linearly and clamped (spec 0050 step 2).
    let private assertTableColumns (fixture : TabulatedFixture) (imported : Result<MaterialEntry, ImportError>) =
        match imported with
        | Error e -> Assert.Fail($"{e}")
        | Ok entry ->
            match entry.complexity with
            | Some { eps = EpsWithDispValue (IsotropicDispersive [ { dispersion = TabulatedNK table } ]) } ->
                Assert.Equal<TabulatedInterpolation>(LinearInWaveLengthClamped, table.interpolation)
                Assert.Equal(List.length fixture.samples, table.wavelengths.Length)
                Assert.Equal(List.length fixture.samples, table.n.Length)
                Assert.Equal(List.length fixture.samples, table.k.Length)
                fixture.samples
                |> List.iteri (fun i sample ->
                    let want = (fixture.waveLengthAt sample.magnitude).value
                    Assert.True(abs (table.wavelengths.[i] - want) <= 1e-15<meter>, $"wavelength {i}: {table.wavelengths.[i]} m, want {want} m")
                    Assert.Equal(sample.n, table.n.[i])
                    Assert.Equal(sample.k, table.k.[i]))
            | other -> Assert.Fail($"expected one isotropic segment carrying a TabulatedNK table, got {other}")

    [<Fact>]
    let ``the imported table holds the samples sorted by wavelength in metres under the linear clamped rule`` () =
        assertTableColumns siYmlRows (readYaml "Si.yml").importMaterial
        // The same rows shuffled: the importer sorts them before the table is built.
        let shuffled = NkCsvText "wavelength_nm,n,k\n600,1.460,0.002\n400,1.500,0.010\n700,1.450,0.001\n500,1.480,0.005\n"
        assertTableColumns sampleNkCsvRows shuffled.importMaterial

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
        match (readYaml fixture).importMaterial with
        | Error e -> Assert.Fail($"{e}")
        | Ok entry ->
            assertEditableDispersive entry
            for lam in samplesMkm do
                let got = epsAt entry (toWaveLength Micrometer (UnitMagnitude lam))
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
        match (readYaml "formula2.yml").importMaterial with
        | Error e -> Assert.Fail($"{e}")
        | Ok entry ->
            match entry.complexity with
            | Some { eps = EpsWithDispValue (IsotropicDispersive [ segment ]) } ->
                Assert.Equal(toWaveLength Micrometer (UnitMagnitude 0.3), segment.wavelengthInterval.lower)
                Assert.Equal(toWaveLength Micrometer (UnitMagnitude 2.5), segment.wavelengthInterval.upper)
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
        match (readYaml "formula8.yml").importMaterial with
        | Error (UnsupportedFormulaErr (RiiFormulaNumber 8, _)) -> ()
        | other -> Assert.Fail($"formula 8: expected UnsupportedFormulaErr, got {other}")
        match (readYaml "formula9.yml").importMaterial with
        | Error (UnsupportedFormulaErr (RiiFormulaNumber 9, _)) -> ()
        | other -> Assert.Fail($"formula 9: expected UnsupportedFormulaErr, got {other}")

    [<Fact>]
    let ``exportCsv samples n,k over the range and round-trips through importCsv`` () =
        let entry =
            match (readCsv "sample-nk.csv").importMaterial with
            | Ok e -> e
            | Error e -> failwith (string e)
        let range : Range<WaveLength> =
            { startValue = WaveLength.nm 450.0<nm>; endValue = WaveLength.nm 650.0<nm>; numberOfPoints = NumberOfPoints 4 }
        let csv = exportCsv entry range
        Assert.Contains("wavelength_nm,n,k", csv.value)
        match csv.importMaterial with
        | Ok back ->
            let w = toWaveLength Nanometer (UnitMagnitude 500.0) // a shared sample of both tables
            Assert.True(closeC 1e-6 (epsAt entry w) (epsAt back w), "round-trip n,k at 500 nm")
        | Error e -> Assert.Fail($"{e}")
