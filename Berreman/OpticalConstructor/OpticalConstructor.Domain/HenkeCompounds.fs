namespace OpticalConstructor.Domain

open Berreman.Dispersion
open OpticalConstructor.Domain.Units
open OpticalConstructor.Domain.LibraryEntryFiles

/// The Henke compound calculator's physics (spec 0050 S13.2; 001 §D): a chemical formula and a density, with each
/// element's CXRO atomic scattering factors f1 and f2, give n = 1 − δ and k = β over the elements' common energies.
module HenkeCompounds =

    /// Why a formula, a density, a compound query or record, or a compound's scattering factors were refused; the reason
    /// names the text, element or energies (spec 0050 S13.2).
    type HenkeCompoundError =
        /// The formula's text is not element symbols and counts (spec 0050 S13.2).
        | InvalidFormulaErr of reason : string
        /// The formula names a symbol the element table lacks (spec 0050 S13.2).
        | UnknownElementErr of reason : string
        /// The density is NaN, infinite, zero or negative (spec 0050 S13.2).
        | InvalidDensityErr of reason : string
        /// The query or record is not a formula and a density (spec 0050 S13.2).
        | InvalidCompoundQueryErr of reason : string
        /// An element of the formula has no scattering factors (spec 0050 S13.2).
        | MissingScatteringFactorsErr of reason : string
        /// A scattering-factor table is empty, uneven or decreasing in energy (spec 0050 S13.2).
        | InvalidScatteringFactorsErr of reason : string
        /// The formula's elements have no energy range in common (spec 0050 S13.2).
        | NoCommonEnergyErr of reason : string
        /// The computed columns are not a tabulated n,k table (spec 0050 S13.2).
        | CompoundTableErr of reason : string

        member this.describe : string =
            match this with
            | InvalidFormulaErr reason -> $"the formula is not valid: {reason}"
            | UnknownElementErr reason -> $"the formula names no known element: {reason}"
            | InvalidDensityErr reason -> $"the density is not valid: {reason}"
            | InvalidCompoundQueryErr reason -> $"the query names no compound: {reason}"
            | MissingScatteringFactorsErr reason -> $"an element's scattering factors are missing: {reason}"
            | InvalidScatteringFactorsErr reason -> $"the scattering factors are not valid: {reason}"
            | NoCommonEnergyErr reason -> $"the elements share no energy: {reason}"
            | CompoundTableErr reason -> $"the compound's table could not be built: {reason}"

    /// An element's atomic number Z (spec 0050 S13.2).
    type AtomicNumber =
        | AtomicNumber of int

        member this.value : int = let (AtomicNumber z) = this in z

    /// An element's symbol, case-sensitive: `Co` is cobalt, `CO` no symbol (spec 0050 S13.2).
    type ElementSymbol =
        | ElementSymbol of string

        member this.value : string = let (ElementSymbol s) = this in s

    /// A standard atomic weight, or a formula's molar mass, in g/mol (spec 0050 S13.2).
    type AtomicWeight =
        | AtomicWeight of float

        member this.value : float = let (AtomicWeight w) = this in w

    /// One element CXRO tabulates: its atomic number, symbol and standard atomic weight (spec 0050 S13.2).
    type ChemicalElement =
        {
            atomicNumber : AtomicNumber
            symbol : ElementSymbol
            atomicWeight : AtomicWeight
        }

        /// Hydrogen to uranium in order, at the CIAAW 2021 abridged standard atomic weights; Tc, Pm, Po, At, Rn, Fr, Ra
        /// and Ac, which have none, at the mass number periodic tables give in brackets (spec 0050 S13.2).
        static member all : ChemicalElement list =
            [
                { atomicNumber = AtomicNumber 1; symbol = ElementSymbol "H"; atomicWeight = AtomicWeight 1.0080 }
                { atomicNumber = AtomicNumber 2; symbol = ElementSymbol "He"; atomicWeight = AtomicWeight 4.0026 }
                { atomicNumber = AtomicNumber 3; symbol = ElementSymbol "Li"; atomicWeight = AtomicWeight 6.94 }
                { atomicNumber = AtomicNumber 4; symbol = ElementSymbol "Be"; atomicWeight = AtomicWeight 9.0122 }
                { atomicNumber = AtomicNumber 5; symbol = ElementSymbol "B"; atomicWeight = AtomicWeight 10.81 }
                { atomicNumber = AtomicNumber 6; symbol = ElementSymbol "C"; atomicWeight = AtomicWeight 12.011 }
                { atomicNumber = AtomicNumber 7; symbol = ElementSymbol "N"; atomicWeight = AtomicWeight 14.007 }
                { atomicNumber = AtomicNumber 8; symbol = ElementSymbol "O"; atomicWeight = AtomicWeight 15.999 }
                { atomicNumber = AtomicNumber 9; symbol = ElementSymbol "F"; atomicWeight = AtomicWeight 18.998 }
                { atomicNumber = AtomicNumber 10; symbol = ElementSymbol "Ne"; atomicWeight = AtomicWeight 20.180 }
                { atomicNumber = AtomicNumber 11; symbol = ElementSymbol "Na"; atomicWeight = AtomicWeight 22.990 }
                { atomicNumber = AtomicNumber 12; symbol = ElementSymbol "Mg"; atomicWeight = AtomicWeight 24.305 }
                { atomicNumber = AtomicNumber 13; symbol = ElementSymbol "Al"; atomicWeight = AtomicWeight 26.982 }
                { atomicNumber = AtomicNumber 14; symbol = ElementSymbol "Si"; atomicWeight = AtomicWeight 28.085 }
                { atomicNumber = AtomicNumber 15; symbol = ElementSymbol "P"; atomicWeight = AtomicWeight 30.974 }
                { atomicNumber = AtomicNumber 16; symbol = ElementSymbol "S"; atomicWeight = AtomicWeight 32.06 }
                { atomicNumber = AtomicNumber 17; symbol = ElementSymbol "Cl"; atomicWeight = AtomicWeight 35.45 }
                { atomicNumber = AtomicNumber 18; symbol = ElementSymbol "Ar"; atomicWeight = AtomicWeight 39.95 }
                { atomicNumber = AtomicNumber 19; symbol = ElementSymbol "K"; atomicWeight = AtomicWeight 39.098 }
                { atomicNumber = AtomicNumber 20; symbol = ElementSymbol "Ca"; atomicWeight = AtomicWeight 40.078 }
                { atomicNumber = AtomicNumber 21; symbol = ElementSymbol "Sc"; atomicWeight = AtomicWeight 44.956 }
                { atomicNumber = AtomicNumber 22; symbol = ElementSymbol "Ti"; atomicWeight = AtomicWeight 47.867 }
                { atomicNumber = AtomicNumber 23; symbol = ElementSymbol "V"; atomicWeight = AtomicWeight 50.942 }
                { atomicNumber = AtomicNumber 24; symbol = ElementSymbol "Cr"; atomicWeight = AtomicWeight 51.996 }
                { atomicNumber = AtomicNumber 25; symbol = ElementSymbol "Mn"; atomicWeight = AtomicWeight 54.938 }
                { atomicNumber = AtomicNumber 26; symbol = ElementSymbol "Fe"; atomicWeight = AtomicWeight 55.845 }
                { atomicNumber = AtomicNumber 27; symbol = ElementSymbol "Co"; atomicWeight = AtomicWeight 58.933 }
                { atomicNumber = AtomicNumber 28; symbol = ElementSymbol "Ni"; atomicWeight = AtomicWeight 58.693 }
                { atomicNumber = AtomicNumber 29; symbol = ElementSymbol "Cu"; atomicWeight = AtomicWeight 63.546 }
                { atomicNumber = AtomicNumber 30; symbol = ElementSymbol "Zn"; atomicWeight = AtomicWeight 65.38 }
                { atomicNumber = AtomicNumber 31; symbol = ElementSymbol "Ga"; atomicWeight = AtomicWeight 69.723 }
                { atomicNumber = AtomicNumber 32; symbol = ElementSymbol "Ge"; atomicWeight = AtomicWeight 72.630 }
                { atomicNumber = AtomicNumber 33; symbol = ElementSymbol "As"; atomicWeight = AtomicWeight 74.922 }
                { atomicNumber = AtomicNumber 34; symbol = ElementSymbol "Se"; atomicWeight = AtomicWeight 78.971 }
                { atomicNumber = AtomicNumber 35; symbol = ElementSymbol "Br"; atomicWeight = AtomicWeight 79.904 }
                { atomicNumber = AtomicNumber 36; symbol = ElementSymbol "Kr"; atomicWeight = AtomicWeight 83.798 }
                { atomicNumber = AtomicNumber 37; symbol = ElementSymbol "Rb"; atomicWeight = AtomicWeight 85.468 }
                { atomicNumber = AtomicNumber 38; symbol = ElementSymbol "Sr"; atomicWeight = AtomicWeight 87.62 }
                { atomicNumber = AtomicNumber 39; symbol = ElementSymbol "Y"; atomicWeight = AtomicWeight 88.906 }
                { atomicNumber = AtomicNumber 40; symbol = ElementSymbol "Zr"; atomicWeight = AtomicWeight 91.224 }
                { atomicNumber = AtomicNumber 41; symbol = ElementSymbol "Nb"; atomicWeight = AtomicWeight 92.906 }
                { atomicNumber = AtomicNumber 42; symbol = ElementSymbol "Mo"; atomicWeight = AtomicWeight 95.95 }
                { atomicNumber = AtomicNumber 43; symbol = ElementSymbol "Tc"; atomicWeight = AtomicWeight 98.0 }
                { atomicNumber = AtomicNumber 44; symbol = ElementSymbol "Ru"; atomicWeight = AtomicWeight 101.07 }
                { atomicNumber = AtomicNumber 45; symbol = ElementSymbol "Rh"; atomicWeight = AtomicWeight 102.91 }
                { atomicNumber = AtomicNumber 46; symbol = ElementSymbol "Pd"; atomicWeight = AtomicWeight 106.42 }
                { atomicNumber = AtomicNumber 47; symbol = ElementSymbol "Ag"; atomicWeight = AtomicWeight 107.87 }
                { atomicNumber = AtomicNumber 48; symbol = ElementSymbol "Cd"; atomicWeight = AtomicWeight 112.41 }
                { atomicNumber = AtomicNumber 49; symbol = ElementSymbol "In"; atomicWeight = AtomicWeight 114.82 }
                { atomicNumber = AtomicNumber 50; symbol = ElementSymbol "Sn"; atomicWeight = AtomicWeight 118.71 }
                { atomicNumber = AtomicNumber 51; symbol = ElementSymbol "Sb"; atomicWeight = AtomicWeight 121.76 }
                { atomicNumber = AtomicNumber 52; symbol = ElementSymbol "Te"; atomicWeight = AtomicWeight 127.60 }
                { atomicNumber = AtomicNumber 53; symbol = ElementSymbol "I"; atomicWeight = AtomicWeight 126.90 }
                { atomicNumber = AtomicNumber 54; symbol = ElementSymbol "Xe"; atomicWeight = AtomicWeight 131.29 }
                { atomicNumber = AtomicNumber 55; symbol = ElementSymbol "Cs"; atomicWeight = AtomicWeight 132.91 }
                { atomicNumber = AtomicNumber 56; symbol = ElementSymbol "Ba"; atomicWeight = AtomicWeight 137.33 }
                { atomicNumber = AtomicNumber 57; symbol = ElementSymbol "La"; atomicWeight = AtomicWeight 138.91 }
                { atomicNumber = AtomicNumber 58; symbol = ElementSymbol "Ce"; atomicWeight = AtomicWeight 140.12 }
                { atomicNumber = AtomicNumber 59; symbol = ElementSymbol "Pr"; atomicWeight = AtomicWeight 140.91 }
                { atomicNumber = AtomicNumber 60; symbol = ElementSymbol "Nd"; atomicWeight = AtomicWeight 144.24 }
                { atomicNumber = AtomicNumber 61; symbol = ElementSymbol "Pm"; atomicWeight = AtomicWeight 145.0 }
                { atomicNumber = AtomicNumber 62; symbol = ElementSymbol "Sm"; atomicWeight = AtomicWeight 150.36 }
                { atomicNumber = AtomicNumber 63; symbol = ElementSymbol "Eu"; atomicWeight = AtomicWeight 151.96 }
                { atomicNumber = AtomicNumber 64; symbol = ElementSymbol "Gd"; atomicWeight = AtomicWeight 157.25 }
                { atomicNumber = AtomicNumber 65; symbol = ElementSymbol "Tb"; atomicWeight = AtomicWeight 158.93 }
                { atomicNumber = AtomicNumber 66; symbol = ElementSymbol "Dy"; atomicWeight = AtomicWeight 162.50 }
                { atomicNumber = AtomicNumber 67; symbol = ElementSymbol "Ho"; atomicWeight = AtomicWeight 164.93 }
                { atomicNumber = AtomicNumber 68; symbol = ElementSymbol "Er"; atomicWeight = AtomicWeight 167.26 }
                { atomicNumber = AtomicNumber 69; symbol = ElementSymbol "Tm"; atomicWeight = AtomicWeight 168.93 }
                { atomicNumber = AtomicNumber 70; symbol = ElementSymbol "Yb"; atomicWeight = AtomicWeight 173.05 }
                { atomicNumber = AtomicNumber 71; symbol = ElementSymbol "Lu"; atomicWeight = AtomicWeight 174.97 }
                { atomicNumber = AtomicNumber 72; symbol = ElementSymbol "Hf"; atomicWeight = AtomicWeight 178.49 }
                { atomicNumber = AtomicNumber 73; symbol = ElementSymbol "Ta"; atomicWeight = AtomicWeight 180.95 }
                { atomicNumber = AtomicNumber 74; symbol = ElementSymbol "W"; atomicWeight = AtomicWeight 183.84 }
                { atomicNumber = AtomicNumber 75; symbol = ElementSymbol "Re"; atomicWeight = AtomicWeight 186.21 }
                { atomicNumber = AtomicNumber 76; symbol = ElementSymbol "Os"; atomicWeight = AtomicWeight 190.23 }
                { atomicNumber = AtomicNumber 77; symbol = ElementSymbol "Ir"; atomicWeight = AtomicWeight 192.22 }
                { atomicNumber = AtomicNumber 78; symbol = ElementSymbol "Pt"; atomicWeight = AtomicWeight 195.08 }
                { atomicNumber = AtomicNumber 79; symbol = ElementSymbol "Au"; atomicWeight = AtomicWeight 196.97 }
                { atomicNumber = AtomicNumber 80; symbol = ElementSymbol "Hg"; atomicWeight = AtomicWeight 200.59 }
                { atomicNumber = AtomicNumber 81; symbol = ElementSymbol "Tl"; atomicWeight = AtomicWeight 204.38 }
                { atomicNumber = AtomicNumber 82; symbol = ElementSymbol "Pb"; atomicWeight = AtomicWeight 207.2 }
                { atomicNumber = AtomicNumber 83; symbol = ElementSymbol "Bi"; atomicWeight = AtomicWeight 208.98 }
                { atomicNumber = AtomicNumber 84; symbol = ElementSymbol "Po"; atomicWeight = AtomicWeight 209.0 }
                { atomicNumber = AtomicNumber 85; symbol = ElementSymbol "At"; atomicWeight = AtomicWeight 210.0 }
                { atomicNumber = AtomicNumber 86; symbol = ElementSymbol "Rn"; atomicWeight = AtomicWeight 222.0 }
                { atomicNumber = AtomicNumber 87; symbol = ElementSymbol "Fr"; atomicWeight = AtomicWeight 223.0 }
                { atomicNumber = AtomicNumber 88; symbol = ElementSymbol "Ra"; atomicWeight = AtomicWeight 226.0 }
                { atomicNumber = AtomicNumber 89; symbol = ElementSymbol "Ac"; atomicWeight = AtomicWeight 227.0 }
                { atomicNumber = AtomicNumber 90; symbol = ElementSymbol "Th"; atomicWeight = AtomicWeight 232.04 }
                { atomicNumber = AtomicNumber 91; symbol = ElementSymbol "Pa"; atomicWeight = AtomicWeight 231.04 }
                { atomicNumber = AtomicNumber 92; symbol = ElementSymbol "U"; atomicWeight = AtomicWeight 238.03 }
            ]

        /// The element of this symbol, matched case-sensitively, or None (spec 0050 S13.2).
        static member tryOfSymbol (symbol : ElementSymbol) : ChemicalElement option =
            ChemicalElement.all |> List.tryFind (fun element -> element.symbol = symbol)

    /// How many atoms of an element one formula unit holds, fractional for an alloy such as `Si0.5Ge0.5` (spec 0050 S13.2).
    type AtomCount =
        | AtomCount of float

        member this.value : float = let (AtomCount c) = this in c

    /// One element of a formula and its count (spec 0050 S13.2).
    type FormulaCount =
        {
            element : ChemicalElement
            count : AtomCount
        }

    /// A chemical formula: each element once, in the order its text first names it (spec 0050 S13.2).
    type ChemicalFormula =
        {
            counts : FormulaCount list
        }

        /// Reads element symbols, each with an optional count (`SiO2`, `Si0.5Ge0.5`), adding a repeated element's counts
        /// at its first position; refuses anything else, naming the first unread character or the unknown symbol
        /// (spec 0050 S13.2).
        static member tryCreate (text : string) : Result<ChemicalFormula, HenkeCompoundError> =
            let isUpper (c : char) : bool = c >= 'A' && c <= 'Z'
            let isLower (c : char) : bool = c >= 'a' && c <= 'z'
            let isDigit (c : char) : bool = c >= '0' && c <= '9'

            let rec digitsEnd (at : int) : int =
                if at < text.Length && isDigit text.[at] then digitsEnd (at + 1) else at

            let added (counts : FormulaCount list) (element : ChemicalElement) (count : float) : FormulaCount list =
                match counts |> List.tryFind (fun held -> held.element = element) with
                | Some _ ->
                    counts
                    |> List.map (fun held ->
                        if held.element = element then { held with count = AtomCount (held.count.value + count) } else held)
                | None -> counts @ [ { element = element; count = AtomCount count } ]

            let rec groups (at : int) (counts : FormulaCount list) : Result<ChemicalFormula, HenkeCompoundError> =
                if at = text.Length then
                    Ok { counts = counts }
                elif not (isUpper text.[at]) then
                    Error (InvalidFormulaErr $"'{text}' has '{text.[at]}' at character {at + 1}, where an element symbol was expected")
                else
                    let symbolLength = if at + 1 < text.Length && isLower text.[at + 1] then 2 else 1
                    let symbol = ElementSymbol (text.Substring(at, symbolLength))
                    match ChemicalElement.tryOfSymbol symbol with
                    | None -> Error (UnknownElementErr $"'{symbol.value}' at character {at + 1} of '{text}' is no element from H to U")
                    | Some element ->
                        let countStart = at + symbolLength
                        let wholeEnd = digitsEnd countStart
                        let countEnd =
                            if wholeEnd > countStart && wholeEnd + 1 < text.Length && text.[wholeEnd] = '.' && isDigit text.[wholeEnd + 1] then
                                digitsEnd (wholeEnd + 1)
                            else
                                wholeEnd

                        if countEnd = countStart then
                            groups countEnd (added counts element 1.0)
                        else
                            let countText = text.Substring(countStart, countEnd - countStart)
                            match System.Double.TryParse(countText, System.Globalization.NumberStyles.AllowDecimalPoint, System.Globalization.CultureInfo.InvariantCulture) with
                            | true, count when count > 0.0 && not (System.Double.IsInfinity count) -> groups countEnd (added counts element count)
                            | _ -> Error (InvalidFormulaErr $"'{text}' gives '{symbol.value}' the count {countText} at character {countStart + 1}; a count is positive")

            if text.Length = 0 then
                Error (InvalidFormulaErr "the text is empty; write element symbols and counts, for example SiO2")
            else
                groups 0 []

        /// Each symbol followed by its count when that is not 1 (G6, invariant culture), in order (spec 0050 S13.2).
        member this.text : string =
            this.counts
            |> List.map (fun held ->
                if held.count.value = 1.0 then held.element.symbol.value
                else held.element.symbol.value + held.count.value.ToString("G6", System.Globalization.CultureInfo.InvariantCulture))
            |> String.concat ""

        /// Σ count × atomic weight, in g/mol (spec 0050 S13.2).
        member this.molarMass : AtomicWeight =
            this.counts |> List.sumBy (fun held -> held.count.value * held.element.atomicWeight.value) |> AtomicWeight

    /// A compound's mass density, in g/cm³ (spec 0050 S13.2).
    type CompoundDensity =
        | CompoundDensity of float

        member this.value : float = let (CompoundDensity d) = this in d

        /// Refuses a NaN, infinite, zero or negative density (spec 0050 S13.2).
        static member tryCreate (value : float) : Result<CompoundDensity, HenkeCompoundError> =
            if System.Double.IsNaN value || System.Double.IsInfinity value || value <= 0.0 then
                Error (InvalidDensityErr $"{value} g/cm³; a density is finite and positive")
            else
                Ok (CompoundDensity value)

    /// A compound the calculator computes: its formula and its density (spec 0050 S13.2).
    type CompoundSpec =
        {
            formula : ChemicalFormula
            density : CompoundDensity
        }

        member private this.densityText : string =
            this.density.value.ToString("G6", System.Globalization.CultureInfo.InvariantCulture)

        /// `<formula text>@<density, G6>`, the record a pull names (spec 0050 S13.2).
        member this.recordId : MaterialSourceRecordId = MaterialSourceRecordId $"{this.formula.text}@{this.densityText}"

        /// `<formula text>, ρ = <density, G6> g/cm³ (CXRO Henke)`, the name a pulled material takes (spec 0050 S13.2).
        member this.entryName : MaterialLibrary.EntryName =
            MaterialLibrary.EntryName $"{this.formula.text}, ρ = {this.densityText} g/cm³ (CXRO Henke)"

        /// The compound `recordId` names: a formula and an invariant-culture density joined by `@` (spec 0050 S13.2).
        static member tryOfRecord (record : MaterialSourceRecordId) : Result<CompoundSpec, HenkeCompoundError> =
            let advice = "write a chemical formula and a density in g/cm³, for example SiO2 2.2"

            match record.value.Split('@') with
            | [| formulaText; densityText |] ->
                match ChemicalFormula.tryCreate formulaText with
                | Error error -> Error error
                | Ok formula ->
                    match System.Double.TryParse(densityText, System.Globalization.NumberStyles.Float, System.Globalization.CultureInfo.InvariantCulture) with
                    | true, value -> CompoundDensity.tryCreate value |> Result.map (fun density -> { formula = formula; density = density })
                    | false, _ -> Error (InvalidCompoundQueryErr $"'{densityText}' is not a density; {advice}")
            | _ -> Error (InvalidCompoundQueryErr $"'{record.value}' is not a formula and a density joined by '@'; {advice}")

        /// The compound a query writes as two words, a formula and a density (`SiO2 2.2`), read as its record (spec 0050
        /// S13.2).
        static member tryOfQuery (query : MaterialSources.MaterialSourceQuery) : Result<CompoundSpec, HenkeCompoundError> =
            match query.value.Split(Array.empty<char>, System.StringSplitOptions.RemoveEmptyEntries) with
            | [| formulaText; densityText |] -> CompoundSpec.tryOfRecord (MaterialSourceRecordId $"{formulaText}@{densityText}")
            | words ->
                Error (InvalidCompoundQueryErr $"'{query.value}' is {words.Length} words; write a chemical formula and a density in g/cm³, for example SiO2 2.2")

    /// One element's CXRO atomic scattering factors: f1 and f2 over photon energy in eV, non-decreasing (spec 0050 S13.2).
    [<Softellect.Sys.NumericCarrier("one element's tabulated f1, f2 over photon energy, read whole for each compound sample")>]
    type AtomicScatteringFactors =
        {
            energies : float array
            f1 : float array
            f2 : float array
        }

        /// Refuses an empty table, columns of unequal length and an energy that decreases anywhere; never sorts
        /// (spec 0050 S13.2).
        static member tryCreate (energies : float array) (f1 : float array) (f2 : float array) : Result<AtomicScatteringFactors, HenkeCompoundError> =
            if f1.Length <> energies.Length || f2.Length <> energies.Length then
                Error (InvalidScatteringFactorsErr $"{energies.Length} energies, {f1.Length} f1 values and {f2.Length} f2 values")
            elif energies.Length = 0 then
                Error (InvalidScatteringFactorsErr "a scattering-factor table needs at least one energy")
            else
                match energies |> Array.pairwise |> Array.tryFindIndex (fun (a, b) -> b < a) with
                | Some i -> Error (InvalidScatteringFactorsErr $"the energy at index {i + 1} ({energies.[i + 1]} eV) is below the one at index {i} ({energies.[i]} eV)")
                | None -> Ok { energies = energies; f1 = f1; f2 = f2 }

    /// The classical electron radius r₀ in m, CODATA 2018 (spec 0050 S13.2).
    [<Literal>]
    let private classicalElectronRadius : float = 2.8179403262e-15

    /// The Avogadro constant N_A in mol⁻¹, exact (spec 0050 S13.2).
    [<Literal>]
    let private avogadroConstant : float = 6.02214076e23

    type CompoundSpec with

        /// n = 1 − δ and k = β, δ and β = r₀ λ² Σ_q N_q f1_q and f2_q / 2π, at every element's energy inside the
        /// elements' common range, each element's factors linear in E between its rows; by ascending λ, with no
        /// uncertainty (spec 0050 S13.2).
        member this.indexTableWith (factors : Map<ElementSymbol, AtomicScatteringFactors>) : Result<TabulatedNkTable, HenkeCompoundError> =
            let rec withFactors (pending : FormulaCount list) (found : (FormulaCount * AtomicScatteringFactors) list) =
                match pending with
                | [] -> Ok (List.rev found)
                | held :: rest ->
                    match factors |> Map.tryFind held.element.symbol with
                    | None -> Error (MissingScatteringFactorsErr $"'{held.element.symbol.value}' of '{this.formula.text}' has none")
                    | Some table ->
                        match AtomicScatteringFactors.tryCreate table.energies table.f1 table.f2 with
                        | Ok checkedTable -> withFactors rest ((held, checkedTable) :: found)
                        | Error (InvalidScatteringFactorsErr reason) -> Error (InvalidScatteringFactorsErr $"'{held.element.symbol.value}': {reason}")
                        | Error error -> Error error

            // The grid lies inside every table's range, so the clamped ends are never read.
            let valueAt (table : AtomicScatteringFactors) (column : float array) (energy : float) : float =
                match table.energies |> Array.tryFindIndex (fun e -> e = energy) with
                | Some row -> column.[row]
                | None ->
                    match table.energies |> Array.tryFindIndex (fun e -> e > energy) with
                    | Some upper when upper > 0 ->
                        let lower = upper - 1
                        let t = (energy - table.energies.[lower]) / (table.energies.[upper] - table.energies.[lower])
                        column.[lower] + (column.[upper] - column.[lower]) * t
                    | Some _ -> column.[0]
                    | None -> column.[column.Length - 1]

            match withFactors this.formula.counts [] with
            | Error error -> Error error
            | Ok elements ->
                let lowest = elements |> List.fold (fun bound (_, table) -> max bound table.energies.[0]) -infinity
                let highest = elements |> List.fold (fun bound (_, table) -> min bound table.energies.[table.energies.Length - 1]) infinity

                if lowest > highest then
                    Error (NoCommonEnergyErr $"in '{this.formula.text}' the latest first energy, {lowest} eV, is above the earliest last one, {highest} eV")
                else
                    let energies =
                        elements
                        |> List.collect (fun (_, table) -> table.energies |> Array.filter (fun e -> e >= lowest && e <= highest) |> Array.toList)
                        |> List.distinct
                        |> List.sortDescending
                        |> List.toArray

                    let formulaUnits = this.density.value * 1.0e6 * avogadroConstant / this.formula.molarMass.value
                    let wavelengths = energies |> Array.map (fun energy -> toMeters ElectronVolt (UnitMagnitude energy))

                    let decrement (column : AtomicScatteringFactors -> float array) : float array =
                        Array.map2
                            (fun energy wavelength ->
                                let lambda = float wavelength
                                let sum = elements |> List.sumBy (fun (held, table) -> held.count.value * formulaUnits * valueAt table (column table) energy)
                                classicalElectronRadius * lambda * lambda * sum / (2.0 * System.Math.PI))
                            energies
                            wavelengths

                    let n = decrement _.f1 |> Array.map (fun delta -> 1.0 - delta)

                    match TabulatedNkTable.tryCreate wavelengths n (decrement _.f2) with
                    | Ok table -> Ok table
                    | Error (EmptyTableErr reason)
                    | Error (SampleCountMismatchErr reason)
                    | Error (UnsortedSamplesErr reason)
                    | Error (InvalidUncertaintyErr reason) -> Error (CompoundTableErr reason)
