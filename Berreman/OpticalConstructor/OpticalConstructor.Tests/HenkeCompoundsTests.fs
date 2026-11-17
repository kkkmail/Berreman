namespace OpticalConstructor.Tests

open Xunit
open Berreman.Dispersion
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.MaterialSources
open OpticalConstructor.Domain.Units
open OpticalConstructor.Domain.HenkeCompounds

/// The Henke compound calculator's physics: the element table, formulas, densities, queries and records, and a
/// compound's n = 1 − δ, k = β over its elements' common energies (spec 0050 S13.2).
module HenkeCompoundsTests =

    /// The interpolated factors the fact expects for SiO2 at one energy, worked by hand from the test tables below.
    type private ExpectedFactors =
        {
            energy : float
            siliconF1 : float
            siliconF2 : float
            oxygenF1 : float
            oxygenF2 : float
        }

    let private formulaOf (text : string) : ChemicalFormula =
        match ChemicalFormula.tryCreate text with
        | Ok formula -> formula
        | Error error -> failwith $"'{text}': {error.describe}"

    let private countsOf (formula : ChemicalFormula) : (string * float) list =
        formula.counts |> List.map (fun held -> held.element.symbol.value, held.count.value)

    let private factorsOf (energies : float array) (f1 : float array) (f2 : float array) : AtomicScatteringFactors =
        match AtomicScatteringFactors.tryCreate energies f1 f2 with
        | Ok factors -> factors
        | Error error -> failwith error.describe

    let private siliconFactors () : AtomicScatteringFactors =
        factorsOf [| 100.0; 200.0; 300.0 |] [| 10.0; 12.0; 13.0 |] [| 2.0; 1.0; 0.5 |]

    let private oxygenFactors () : AtomicScatteringFactors =
        factorsOf [| 150.0; 250.0; 400.0 |] [| 6.0; 7.0; 7.5 |] [| 3.0; 2.0; 1.0 |]

    let private silicaAt (density : float) : CompoundSpec =
        match CompoundDensity.tryCreate density with
        | Ok compoundDensity -> { formula = formulaOf "SiO2"; density = compoundDensity }
        | Error error -> failwith error.describe

    let private errorOf (what : string) (result : Result<'Value, HenkeCompoundError>) : HenkeCompoundError =
        match result with
        | Error error -> error
        | Ok _ -> failwith $"{what}: expected an error, got a result"

    let private assertRelative (label : string) (expected : float) (actual : float) : unit =
        Assert.True(abs ((actual - expected) / expected) <= 1e-12, $"{label}: expected {expected}, got {actual}")

    [<Fact>]
    let ``the element table holds the 92 elements from hydrogen to uranium with their standard atomic weights`` () =
        let elements = ChemicalElement.all
        Assert.Equal(92, elements.Length)
        Assert.Equal<int list>([ 1 .. 92 ], elements |> List.map _.atomicNumber.value)
        Assert.Equal(92, elements |> List.map _.symbol |> List.distinct |> List.length)
        Assert.Equal(ElementSymbol "H", elements.Head.symbol)
        Assert.Equal(ElementSymbol "U", (List.last elements).symbol)

        for (symbol, weight) in [ "H", 1.0080; "O", 15.999; "Si", 28.085; "Mo", 95.95; "Tc", 98.0; "U", 238.03 ] do
            match ChemicalElement.tryOfSymbol (ElementSymbol symbol) with
            | Some element -> Assert.Equal(AtomicWeight weight, element.atomicWeight)
            | None -> Assert.Fail $"'{symbol}' is not in the table"

        Assert.Equal<ChemicalElement option>(None, ChemicalElement.tryOfSymbol (ElementSymbol "CO"))

    [<Fact>]
    let ``a chemical formula reads element symbols and counts, merging repeats, and refuses anything else`` () =
        let silica = formulaOf "SiO2"
        Assert.Equal<(string * float) list>([ "Si", 1.0; "O", 2.0 ], countsOf silica)
        Assert.Equal("SiO2", silica.text)

        Assert.Equal<(string * float) list>([ "Si", 3.0; "N", 4.0 ], countsOf (formulaOf "Si3N4"))
        Assert.Equal<(string * float) list>([ "Mo", 1.0; "Si", 2.0 ], countsOf (formulaOf "MoSi2"))
        Assert.Equal<(string * float) list>([ "Si", 0.5; "Ge", 0.5 ], countsOf (formulaOf "Si0.5Ge0.5"))
        Assert.Equal("Si0.5Ge0.5", (formulaOf "Si0.5Ge0.5").text)

        Assert.Equal<(string * float) list>([ "C", 1.0; "O", 1.0 ], countsOf (formulaOf "CO"))
        match (formulaOf "Co").counts with
        | [ cobalt ] ->
            Assert.Equal(AtomicNumber 27, cobalt.element.atomicNumber)
            Assert.Equal(AtomCount 1.0, cobalt.count)
        | counts -> Assert.Fail $"'Co': expected cobalt alone, got {counts.Length} elements"

        let acetic = formulaOf "CH3COOH"
        Assert.Equal<(string * float) list>([ "C", 2.0; "H", 4.0; "O", 2.0 ], countsOf acetic)
        Assert.Equal("C2H4O2", acetic.text)

        for text in [ ""; "Ca(OH)2"; "SiO2 "; "si"; "O0" ] do
            match errorOf $"'{text}'" (ChemicalFormula.tryCreate text) with
            | InvalidFormulaErr _ -> ()
            | error -> Assert.Fail $"'{text}': expected InvalidFormulaErr, got {error.describe}"

        match errorOf "'Xx2'" (ChemicalFormula.tryCreate "Xx2") with
        | UnknownElementErr reason -> Assert.Contains("'Xx'", reason)
        | error -> Assert.Fail $"'Xx2': expected UnknownElementErr, got {error.describe}"

    [<Fact>]
    let ``a compound query and record name a formula and a density, and anything else is refused`` () =
        match CompoundSpec.tryOfQuery (MaterialSourceQuery "SiO2 2.2") with
        | Ok spec ->
            Assert.Equal("SiO2", spec.formula.text)
            Assert.Equal(CompoundDensity 2.2, spec.density)
            Assert.Equal(MaterialSourceRecordId "SiO2@2.2", spec.recordId)
            Assert.Equal(EntryName "SiO2, ρ = 2.2 g/cm³ (CXRO Henke)", spec.entryName)
            Assert.Equal<Result<CompoundSpec, HenkeCompoundError>>(Ok spec, CompoundSpec.tryOfRecord spec.recordId)
        | Error error -> Assert.Fail error.describe

        for text in [ "SiO2"; "SiO2 abc"; "SiO2 2.2 x" ] do
            match errorOf $"'{text}'" (CompoundSpec.tryOfQuery (MaterialSourceQuery text)) with
            | InvalidCompoundQueryErr reason -> Assert.EndsWith("write a chemical formula and a density in g/cm³, for example SiO2 2.2", reason)
            | error -> Assert.Fail $"'{text}': expected InvalidCompoundQueryErr, got {error.describe}"

        match errorOf "'SiO2 -1'" (CompoundSpec.tryOfQuery (MaterialSourceQuery "SiO2 -1")) with
        | InvalidDensityErr _ -> ()
        | error -> Assert.Fail $"'SiO2 -1': expected InvalidDensityErr, got {error.describe}"

        match errorOf "the record 'SiO2'" (CompoundSpec.tryOfRecord (MaterialSourceRecordId "SiO2")) with
        | InvalidCompoundQueryErr _ -> ()
        | error -> Assert.Fail $"the record 'SiO2': expected InvalidCompoundQueryErr, got {error.describe}"

        for density in [ nan; infinity; 0.0; -2.2 ] do
            match errorOf $"the density {density}" (CompoundDensity.tryCreate density) with
            | InvalidDensityErr _ -> ()
            | error -> Assert.Fail $"the density {density}: expected InvalidDensityErr, got {error.describe}"

    [<Fact>]
    let ``a compound's index is one minus delta plus i beta over its elements' common energies`` () =
        let factors = Map.ofList [ ElementSymbol "Si", siliconFactors (); ElementSymbol "O", oxygenFactors () ]

        let expectedRows : ExpectedFactors list =
            [
                { energy = 300.0; siliconF1 = 13.0; siliconF2 = 0.5; oxygenF1 = 7.0 + 0.5 / 3.0; oxygenF2 = 2.0 - 1.0 / 3.0 }
                { energy = 250.0; siliconF1 = 12.5; siliconF2 = 0.75; oxygenF1 = 7.0; oxygenF2 = 2.0 }
                { energy = 200.0; siliconF1 = 12.0; siliconF2 = 1.0; oxygenF1 = 6.5; oxygenF2 = 2.5 }
                { energy = 150.0; siliconF1 = 11.0; siliconF2 = 1.5; oxygenF1 = 6.0; oxygenF2 = 3.0 }
            ]

        let classicalElectronRadius = 2.8179403262e-15
        let avogadroConstant = 6.02214076e23
        let formulaUnits = 2.2 * 1.0e6 * avogadroConstant / (28.085 + 2.0 * 15.999)

        match (silicaAt 2.2).indexTableWith factors with
        | Ok table ->
            let expectedWavelengths = expectedRows |> List.map (fun row -> float (toMeters ElectronVolt (UnitMagnitude row.energy)))
            Assert.Equal<float list>(expectedWavelengths, table.wavelengths |> Array.map float |> Array.toList)
            Assert.Equal<TabulatedNkUncertainty option>(None, table.uncertaintyOpt)

            expectedRows
            |> List.iteri (fun i row ->
                let lambda = float (toMeters ElectronVolt (UnitMagnitude row.energy))
                let scale = classicalElectronRadius * lambda * lambda / (2.0 * System.Math.PI)
                let delta = scale * formulaUnits * (row.siliconF1 + 2.0 * row.oxygenF1)
                let beta = scale * formulaUnits * (row.siliconF2 + 2.0 * row.oxygenF2)
                assertRelative $"n at {row.energy} eV" (1.0 - delta) table.n.[i]
                assertRelative $"k at {row.energy} eV" beta table.k.[i])
        | Error error -> Assert.Fail error.describe

    [<Fact>]
    let ``a compound lacking one element's factors, or a common energy, is refused`` () =
        let siliconOnly = Map.ofList [ ElementSymbol "Si", siliconFactors () ]

        match errorOf "SiO2 over Si alone" ((silicaAt 2.2).indexTableWith siliconOnly) with
        | MissingScatteringFactorsErr reason -> Assert.Contains("'O'", reason)
        | error -> Assert.Fail $"SiO2 over Si alone: expected MissingScatteringFactorsErr, got {error.describe}"

        let disjoint =
            Map.ofList [ ElementSymbol "Si", siliconFactors (); ElementSymbol "O", factorsOf [| 400.0; 500.0 |] [| 7.5; 7.8 |] [| 1.0; 0.8 |] ]

        match errorOf "SiO2 over disjoint tables" ((silicaAt 2.2).indexTableWith disjoint) with
        | NoCommonEnergyErr _ -> ()
        | error -> Assert.Fail $"SiO2 over disjoint tables: expected NoCommonEnergyErr, got {error.describe}"

        for (label, energies, f1, f2) in
            [
                "an empty table", [||], [||], [||]
                "unequal columns", [| 100.0; 200.0 |], [| 10.0 |], [| 2.0; 1.0 |]
                "a decreasing energy", [| 200.0; 100.0 |], [| 12.0; 10.0 |], [| 1.0; 2.0 |]
            ] do
            match errorOf label (AtomicScatteringFactors.tryCreate energies f1 f2) with
            | InvalidScatteringFactorsErr _ -> ()
            | error -> Assert.Fail $"{label}: expected InvalidScatteringFactorsErr, got {error.describe}"

    [<Fact>]
    let ``every Henke compound error renders its condition and its payload`` () =
        let rendered =
            [
                InvalidFormulaErr "formula reason", "the formula is not valid: formula reason"
                UnknownElementErr "element reason", "the formula names no known element: element reason"
                InvalidDensityErr "density reason", "the density is not valid: density reason"
                InvalidCompoundQueryErr "query reason", "the query names no compound: query reason"
                MissingScatteringFactorsErr "missing reason", "an element's scattering factors are missing: missing reason"
                InvalidScatteringFactorsErr "factors reason", "the scattering factors are not valid: factors reason"
                NoCommonEnergyErr "energy reason", "the elements share no energy: energy reason"
                CompoundTableErr "table reason", "the compound's table could not be built: table reason"
            ]

        for (error, expected) in rendered do
            Assert.Equal(expected, error.describe)

        Assert.Equal(rendered.Length, rendered |> List.map (fun (error, _) -> error.describe) |> List.distinct |> List.length)
