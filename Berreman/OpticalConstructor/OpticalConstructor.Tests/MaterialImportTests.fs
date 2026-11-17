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
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.MaterialSources
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
    /// `properties` IS its physics' `complexity.toProperties` (the seed invariant).
    let private assertViewOnlyTabulatedSegment (fixture : TabulatedFixture) (imported : Result<MaterialEntry, ImportError>) =
        match imported with
        | Error e -> Assert.Fail($"{e}")
        | Ok entry ->
            let c =
                match entry.physics with
                | ComplexityPhysics complexity -> complexity
                | CrystalParameterPhysics crystal -> failwith $"expected a complexity physics, got %A{crystal}"
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
            match entry.physics with
            | ComplexityPhysics { eps = EpsWithDispValue (IsotropicDispersive [ { dispersion = TabulatedNK table } ]) } ->
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

    /// The imported entry MUST be editable (spec 0033 step 025): a complexity whose
    /// eps is the dispersive segment tree (`EpsWithDispValue`).
    let private assertEditableDispersive (entry : MaterialEntry) =
        match entry.physics with
        | ComplexityPhysics { eps = EpsWithDispValue _ } -> ()
        | ComplexityPhysics { eps = EpsWithoutDispValue _ }
        | CrystalParameterPhysics _ -> Assert.Fail "expected a dispersive EpsWithDispValue-backed complexity"

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
            match entry.physics with
            | ComplexityPhysics { eps = EpsWithDispValue (IsotropicDispersive [ segment ]) } ->
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

    [<Fact>]
    let ``the refractiveindex.info catalog lists each page as a hit named by its book and page`` () =
        let fixtureHits : MaterialSourceHit list =
            [
                { record = MaterialSourceRecordId "main/Si/nk/Test.yml"; name = EntryName "Si (Silicon), Test 2026: n,k 0.25–0.70 µm" }
                { record = MaterialSourceRecordId "glass/test/BK7/nk/Test.yml"; name = EntryName "BK7 (test), Test 2026: n 0.3–2.5 µm" }
            ]
        match (RefractiveIndexInfoCatalogText (readFixture "catalog-nk.yml")).hits with
        | Ok hits -> Assert.Equal<MaterialSourceHit list>(fixtureHits, hits)
        | Error e -> Assert.Fail(e.describe)

        let unnamedPage =
            RefractiveIndexInfoCatalogText
                "- SHELF: main\n  name: \"MAIN\"\n  content:\n    - BOOK: Ge\n      name: \"Ge (Germanium)\"\n      content:\n        - PAGE: Bare\n          data: \"main/Ge/nk/Bare.yml\"\n"
        let unnamedHits : MaterialSourceHit list =
            [ { record = MaterialSourceRecordId "main/Ge/nk/Bare.yml"; name = EntryName "Ge (Germanium), Bare" } ]
        match unnamedPage.hits with
        | Ok hits -> Assert.Equal<MaterialSourceHit list>(unnamedHits, hits)
        | Error e -> Assert.Fail(e.describe)

        let headerOnly =
            RefractiveIndexInfoCatalogText
                "# this file is part of refractiveindex.info database\n# refractiveindex.info database is in the public domain\n# copyright and related rights waived via CC0 1.0\n"
        match headerOnly.hits with
        | Error (NoDataErr _) -> ()
        | Error e -> Assert.Fail($"a catalog of header comments alone: expected NoDataErr, got {e.describe}")
        | Ok hits -> Assert.Fail($"a catalog of header comments alone: expected NoDataErr, got {List.length hits} hit(s)")

    [<Fact>]
    let ``a page's references are its citation, a block's lines joined`` () =
        Assert.Equal<EntryCitation option>(
            Some (EntryCitation "Test fixture — minimal refractiveindex.info-style tabulated page (NOT real Si data)."),
            (readYaml "Si.yml").referencesOpt)

        let blockPage =
            RefractiveIndexInfoText
                "REFERENCES: |\n    A. Author and B. Author,\n    Test optical constants,\n    J. Test 1, 1 (2026)\nCOMMENTS: \"block test\"\nDATA:\n  - type: tabulated n\n    data: |\n      0.5 1.5\n"
        Assert.Equal<EntryCitation option>(
            Some (EntryCitation "A. Author and B. Author, Test optical constants, J. Test 1, 1 (2026)"),
            blockPage.referencesOpt)

        let unreferencedPage =
            RefractiveIndexInfoText "COMMENTS: \"no references\"\nDATA:\n  - type: tabulated n\n    data: |\n      0.5 1.5\n"
        Assert.Equal<EntryCitation option>(None, unreferencedPage.referencesOpt)

    [<Fact>]
    let ``every import error renders its condition and its payload`` () =
        let errors : (ImportError * string) list =
            [
                MalformedYamlErr "a tab before the DATA key", "the page is malformed: a tab before the DATA key"
                MalformedCsvErr "row 3 has one column", "the CSV is malformed: row 3 has one column"
                NoDataErr "no tabulated rows found", "the file holds no data: no tabulated rows found"
                UnsupportedFormulaErr (RiiFormulaNumber 8, "a rational transform"), "formula 8 is not supported: a rational transform"
                MalformedSourceFileErr "line 9 holds 6 values where the header names 7 columns",
                "the source file is malformed: line 9 holds 6 values where the header names 7 columns"
            ]
        for error, rendered in errors do
            Assert.Equal(rendered, error.describe)
        let texts = errors |> List.map (fun (error, _) -> error.describe)
        Assert.Equal(List.length texts, List.length (List.distinct texts))

    /// One row of `fixtures/ptb-Co.txt` written out by hand: its sample (λ in nm, 1 − δ, β) and its 3-sigma bounds
    /// (spec 0050 S13.2).
    type private PtbBoundedSample =
        {
            sample : TabulatedSample
            nLow : float
            nHigh : float
            kLow : float
            kHigh : float
        }

    let private ptbCoRows : PtbBoundedSample list =
        [
            { sample = { magnitude = UnitMagnitude 8.0; n = 0.96788; k = 0.02267 }; nLow = 0.96772; nHigh = 0.96804; kLow = 0.02253; kHigh = 0.0228 }
            { sample = { magnitude = UnitMagnitude 11.2; n = 0.95225; k = 0.05349 }; nLow = 0.95194; nHigh = 0.95256; kLow = 0.05327; kHigh = 0.05371 }
            { sample = { magnitude = UnitMagnitude 25.0; n = 0.92771; k = 0.10418 }; nLow = 0.92583; nHigh = 0.92962; kLow = 0.10319; kHigh = 0.10517 }
        ]

    let private ptbCoTable : TabulatedFixture = { waveLengthUnit = Nanometer; samples = ptbCoRows |> List.map _.sample }

    /// The table of an imported entry's one isotropic segment, with no magnetic or active part.
    let private importedTable (imported : Result<MaterialEntry, ImportError>) : TabulatedNkTable =
        match imported with
        | Ok { physics = ComplexityPhysics { eps = EpsWithDispValue (IsotropicDispersive [ { dispersion = TabulatedNK table } ]); magnetic = None; active = None } } -> table
        | Ok entry -> failwith $"expected one isotropic segment carrying a TabulatedNK table, got %A{entry.physics}"
        | Error e -> failwith e.describe

    [<Fact>]
    let ``a PTB OCDB record reads n as 1 - delta and k as beta, its 3-sigma bounds as one-sigma uncertainties`` () =
        let record = PtbOcdbText (readFixture "ptb-Co.txt")
        let imported = record.importMaterial
        assertViewOnlyTabulatedSegment ptbCoTable imported
        assertTableColumns ptbCoTable imported
        match (importedTable imported).uncertaintyOpt with
        | Some uncertainty ->
            Assert.Equal(List.length ptbCoRows, uncertainty.sigmaN.Length)
            Assert.Equal(List.length ptbCoRows, uncertainty.sigmaK.Length)
            ptbCoRows
            |> List.iteri (fun i row ->
                let sigmaN = (row.nHigh - row.nLow) / 6.0
                let sigmaK = (row.kHigh - row.kLow) / 6.0
                Assert.True(abs (uncertainty.sigmaN.[i] - sigmaN) <= 1e-15, $"sigma n {i}: {uncertainty.sigmaN.[i]}, want {sigmaN}")
                Assert.True(abs (uncertainty.sigmaK.[i] - sigmaK) <= 1e-15, $"sigma k {i}: {uncertainty.sigmaK.[i]}, want {sigmaK}"))
        | None -> Assert.Fail "expected the record's 3-sigma bounds as one-sigma uncertainties"
        match imported with
        | Ok entry ->
            Assert.Equal<EntryName>(EntryName "Imported (PTB OCDB)", entry.name)
            Assert.Equal<CategoryId>(CategoryIds.semiconductor, entry.category)
            Assert.Equal<EntryDescription option>(Some (EntryDescription "Imported tabulated n,k (PTB OCDB)."), entry.description)
        | Error e -> Assert.Fail(e.describe)
        Assert.Equal<EntryCitation option>(Some (EntryCitation "Optical constants for Co created by PTB"), record.headlineOpt)

        // The same rows reversed under CRLF line endings: sorted by λ, each uncertainty with its sample.
        let reversed =
            PtbOcdbText (
                "# Optical constants for Co created by PTB\r\n"
                + "# The values are provided with their 3-sigma uncertainty bounds. HB: Higher Bound. LB: Lower Bound.\r\n"
                + "#lambda/nm\t1-delta\tbeta\t1-delta_LB\t1-delta_HB\tbeta_LB\tbeta_HB\r\n"
                + "25.0\t0.92771\t0.10418\t0.92583\t0.92962\t0.10319\t0.10517\r\n"
                + "11.2\t0.95225\t0.05349\t0.95194\t0.95256\t0.05327\t0.05371\r\n"
                + "8.0\t0.96788\t0.02267\t0.96772\t0.96804\t0.02253\t0.0228\r\n")
        Assert.True((importedTable reversed.importMaterial = importedTable imported), "the reversed rows read into the fixture's table")

    /// A malformed PTB OCDB text and the word its refusal names (spec 0050 S13.2).
    type private RefusedPtbText =
        {
            label : string
            text : PtbOcdbText
            named : string
        }

    [<Fact>]
    let ``a PTB OCDB record without bounds has no uncertainty, and a malformed record is refused`` () =
        let headline = "# Optical constants for Ru created by PTB\n# n = (1-delta) - (i*beta)\n"
        let sigmaLine = "# The values are provided with their 3-sigma uncertainty bounds. HB: Higher Bound. LB: Lower Bound.\n"
        let boundedHeader = "#lambda/nm\t1-delta\tbeta\t1-delta_LB\t1-delta_HB\tbeta_LB\tbeta_HB\n# ------------------------\n"
        let boundedRow = "13.5\t0.88\t0.017\t0.87\t0.89\t0.016\t0.018\n"

        let withoutBounds = PtbOcdbText (headline + "#lambda/nm\t1-delta\tbeta\n# ------------------------\n13.5\t0.88\t0.017\n12.5\t0.91\t0.015\n")
        let unbounded : TabulatedFixture =
            {
                waveLengthUnit = Nanometer
                samples = [ { magnitude = UnitMagnitude 12.5; n = 0.91; k = 0.015 }; { magnitude = UnitMagnitude 13.5; n = 0.88; k = 0.017 } ]
            }
        assertViewOnlyTabulatedSegment unbounded withoutBounds.importMaterial
        assertTableColumns unbounded withoutBounds.importMaterial
        Assert.Equal<TabulatedNkUncertainty option>(None, (importedTable withoutBounds.importMaterial).uncertaintyOpt)

        let refused : RefusedPtbText list =
            [
                { label = "a header without beta"; text = PtbOcdbText (headline + "#lambda/nm\t1-delta\n13.5\t0.88\n"); named = "beta" }
                { label = "beta_LB alone"; text = PtbOcdbText (headline + sigmaLine + "#lambda/nm\t1-delta\tbeta\tbeta_LB\n13.5\t0.88\t0.017\t0.016\n"); named = "beta_HB" }
                { label = "bounds without a sigma line"; text = PtbOcdbText (headline + boundedHeader + boundedRow); named = "sigma level" }
                { label = "a row of six values"; text = PtbOcdbText (headline + sigmaLine + boundedHeader + boundedRow + "14.5\t0.85\t0.019\t0.84\t0.86\t0.018\n"); named = "line 7" }
            ]
        for case in refused do
            match case.text.importMaterial with
            | Error (MalformedSourceFileErr reason) -> Assert.True(reason.Contains(case.named), $"{case.label}: '{reason}' does not name {case.named}")
            | Error e -> Assert.Fail($"{case.label}: expected MalformedSourceFileErr, got {e.describe}")
            | Ok _ -> Assert.Fail($"{case.label}: expected MalformedSourceFileErr, got an entry")

        match (PtbOcdbText (headline + sigmaLine + boundedHeader)).importMaterial with
        | Error (NoDataErr _) -> ()
        | Error e -> Assert.Fail($"a header without a row: expected NoDataErr, got {e.describe}")
        | Ok _ -> Assert.Fail "a header without a row: expected NoDataErr, got an entry"

    /// A malformed CXRO scattering-factor text and the line its refusal names (spec 0050 S13.2).
    type private RefusedHenkeText =
        {
            label : string
            text : HenkeScatteringText
            named : string
        }

    [<Fact>]
    let ``a CXRO scattering-factor file reads its energies and factors, leaving out the rows without f1`` () =
        match (HenkeScatteringText (readFixture "si.nff")).factors with
        | Ok factors ->
            Assert.Equal<float list>([ 100.0; 200.0; 300.0 ], List.ofArray factors.energies)
            Assert.Equal<float list>([ 10.0; 12.0; 13.0 ], List.ofArray factors.f1)
            Assert.Equal<float list>([ 2.0; 1.0; 0.5 ], List.ofArray factors.f2)
        | Error e -> Assert.Fail($"si.nff: {e.describe}")

        let header = "E(eV)\tf1\tf2\n"
        let refused : RefusedHenkeText list =
            [
                { label = "a header that is not CXRO's"; text = HenkeScatteringText "Energy\tf1\tf2\n100\t10\t2\n"; named = "line 1" }
                { label = "a row that does not read"; text = HenkeScatteringText (header + "100\t10\t2\n200 x 1\n"); named = "line 3" }
            ]
        for case in refused do
            match case.text.factors with
            | Error (MalformedSourceFileErr reason) -> Assert.True(reason.Contains(case.named), $"{case.label}: '{reason}' does not name {case.named}")
            | Error e -> Assert.Fail($"{case.label}: expected MalformedSourceFileErr, got {e.describe}")
            | Ok _ -> Assert.Fail($"{case.label}: expected MalformedSourceFileErr, got factors")

        match (HenkeScatteringText header).factors with
        | Error (NoDataErr _) -> ()
        | Error e -> Assert.Fail($"a header alone: expected NoDataErr, got {e.describe}")
        | Ok _ -> Assert.Fail "a header alone: expected NoDataErr, got factors"
