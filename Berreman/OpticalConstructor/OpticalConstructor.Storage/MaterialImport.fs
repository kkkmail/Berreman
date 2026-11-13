namespace OpticalConstructor.Storage

open System
open System.Text
open System.Numerics
open System.Globalization
open FSharp.Data
open Berreman.Constants
open Berreman.MathNetNumericsMath
open Berreman.Fields
open Berreman.MaterialProperties
open Berreman.Dispersion
open Analytics.Variables
open OpticalConstructor.Domain.Units
open OpticalConstructor.Domain.DispersionModels
open OpticalConstructor.Domain.MaterialLibrary

/// refractiveindex.info YAML + CSV import and CSV export (§D.9). Importers produce
/// in-memory `MaterialEntry` values (never FsPickler-pickled, §I.4); every parsed
/// wavelength is reduced to the engine meter base through the sole `Units` seam
/// (§D.2/§D.11) according to the file's declared unit. A tabulated page or CSV becomes
/// a `TabulatedNK` axis table, read linearly in λ and clamped to its end samples (spec
/// 0050 step 2). The error channel is `Result<_, ImportError>` — parsing never throws
/// past the boundary. The importers read, and `exportCsv` returns, the raw file TEXT
/// (`RefractiveIndexInfoText` / `NkCsvText`), keeping filesystem IO and its exceptions
/// out of the parser.
module MaterialImport =

    /// A refractiveindex.info `type: formula N` number; 1–7 lower to editable term data.
    type RiiFormulaNumber =
        | RiiFormulaNumber of int

        member this.value = let (RiiFormulaNumber n) = this in n

        /// Why this formula cannot produce editable term data (spec 0033 step 025): 8 (Retro)
        /// hides n behind the rational transform (n² − 1)/(n² + 2), which no finite term sum
        /// inverts; 9 (Exotic) is outside the importer's supported set; any other number is not a
        /// refractiveindex.info formula. `None` = supported (1–7).
        member this.unsupportedReason : string option =
            match this with
            | RiiFormulaNumber (1 | 2 | 3 | 4 | 5 | 6 | 7) -> None
            | RiiFormulaNumber 8 -> Some "formula 8 (Retro) defines (n² − 1)/(n² + 2) — a rational transform with no finite-term eps lowering"
            | RiiFormulaNumber 9 -> Some "formula 9 (Exotic) is not lowered to editable term data"
            | RiiFormulaNumber n -> Some $"unknown refractiveindex.info dispersion formula %d{n} (supported: 1-7)"

        /// The display family of a supported formula number (entry description).
        member this.family : string =
            match this with
            | RiiFormulaNumber 1 -> "Sellmeier"
            | RiiFormulaNumber 2 -> "Sellmeier-2"
            | RiiFormulaNumber 3 -> "polynomial"
            | RiiFormulaNumber 4 -> "RefractiveIndex.INFO-formula"
            | RiiFormulaNumber 5 -> "Cauchy"
            | RiiFormulaNumber 6 -> "gases"
            | RiiFormulaNumber 7 -> "Herzberger"
            | RiiFormulaNumber _ -> "dispersion-formula"

    /// Net-new import error channel (errors as values, §0 / §D.9).
    /// `UnsupportedFormulaErr` (spec 0033 step 025) types a refractiveindex.info
    /// dispersion formula the importer cannot lower to editable term data
    /// (formulas 8/9, an unknown number, or a non-integer exponent); it carries
    /// the offending formula number plus a diagnostic reason.
    type ImportError =
        | MalformedYamlErr of string
        | MalformedCsvErr of string
        | NoDataErr of string
        | UnsupportedFormulaErr of formulaNumber : RiiFormulaNumber * reason : string

    /// The text of a refractiveindex.info YAML page.
    type RefractiveIndexInfoText =
        | RefractiveIndexInfoText of string

        member this.value = let (RefractiveIndexInfoText t) = this in t

    /// The text of an n,k CSV table: `wavelength, n[, k]`, the wavelength header naming its unit.
    type NkCsvText =
        | NkCsvText of string

        member this.value = let (NkCsvText t) = this in t

    let private inv = CultureInfo.InvariantCulture

    /// One row of an imported n,k table: λ in meters, n and k.
    [<Softellect.Sys.NumericCarrier("one row of an imported n,k table, a measured data series (R3): the rows are sorted by λ and split into the columns of a TabulatedNkTable")>]
    type private NkTableRow =
        {
            wavelength : float<meter>
            n : float
            k : float
        }

    /// A refractiveindex.info page, split into lines.
    type private RiiPage =
        | RiiPage of string[]

        /// All floating-point tokens on a line (comma/space/tab separated).
        static member floatsOn (line : string) : float[] =
            line.Split([| ' '; '\t'; ',' |], StringSplitOptions.RemoveEmptyEntries)
            |> Array.choose (fun token ->
                match Double.TryParse(token.Trim(), NumberStyles.Float, inv) with
                | true, v -> Some v
                | _ -> None)

        /// Collect tabulated `(λ in meters, n, k)` rows from data lines (those starting
        /// with a number), reducing λ from `unit` to the meter base via `Units.toMeters`.
        /// Two-column `tabulated n` rows map k = 0.
        member this.tabulatedRows (unit : UnitOfMeasure) : NkTableRow[] =
            let (RiiPage lines) = this
            lines
            |> Array.choose (fun line ->
                let t = line.Trim()
                if t.Length > 0 && (Char.IsDigit t.[0] || t.[0] = '-' || t.[0] = '.') then
                    let f = RiiPage.floatsOn t
                    if f.Length >= 2 then
                        Some { wavelength = toMeters unit (UnitMagnitude f.[0]); n = f.[1]; k = (if f.Length >= 3 then f.[2] else 0.0) }
                    else None
                else None)

        /// The `type: formula N` declaration of an analytic RII page (None on a
        /// tabulated page). N is read but NOT validated here — `unsupportedReason`
        /// types the numbers the importer cannot lower.
        member this.tryFormulaNumber : RiiFormulaNumber option =
            let (RiiPage lines) = this
            lines
            |> Array.tryPick (fun l ->
                if l.Contains "type:" && l.Contains "formula" then
                    let after = l.Substring(l.IndexOf("formula", StringComparison.Ordinal) + "formula".Length)
                    after.Split([| ' '; '\t'; '"'; '\'' |], StringSplitOptions.RemoveEmptyEntries)
                    |> Array.tryHead
                    |> Option.bind (fun token ->
                        match Int32.TryParse(token, NumberStyles.Integer, inv) with
                        | true, v -> Some (RiiFormulaNumber v)
                        | _ -> None)
                else None)

        /// The page's `wavelength_range: a b` (µm, the RII convention) as the
        /// imported segment's interval, through the `Units` seam. A page without one
        /// gets the zero nominal interval — with a single segment the topmost
        /// segment extrapolates for every wavelength (`Dispersion.fs:346`), so the
        /// bounds are descriptive, not clamping.
        member this.rangeInterval : WaveLengthInterval =
            let (RiiPage lines) = this
            let bounds =
                lines
                |> Array.tryFind (fun l ->
                    let t = l.TrimStart()
                    t.StartsWith "wavelength_range:" || t.StartsWith "range:")
                |> Option.map (fun l -> RiiPage.floatsOn (l.Substring(l.IndexOf(':') + 1)))
            match bounds with
            | Some f when f.Length >= 2 -> { lower = toWaveLength Micrometer (UnitMagnitude f.[0]); upper = toWaveLength Micrometer (UnitMagnitude f.[1]) }
            | _ ->
                let nominal = toWaveLength Micrometer (UnitMagnitude 0.0)
                { lower = nominal; upper = nominal }

    /// A tabulated import is axis data (spec 0050 step 2, 001 §B): its rows, sorted by λ, become
    /// one `TabulatedNK` table carried as a single isotropic dispersive segment over the sample
    /// range, view-only because a table is not editable term data. A refused table is
    /// `NoDataErr` with its reason. An imported entry MINTS a fresh `MaterialId` (spec 0033
    /// step 002) — imports carry no persisted identity of their own; the stable Guid ids live
    /// with the library seeds.
    let private entryFromTabulated (name : EntryName) (category : CategoryId) (points : NkTableRow[]) : Result<MaterialEntry, ImportError> =
        let sorted = points |> Array.sortBy (fun r -> r.wavelength)
        let wavelengths = sorted |> Array.map (fun r -> r.wavelength)
        let n = sorted |> Array.map (fun r -> r.n)
        let k = sorted |> Array.map (fun r -> r.k)
        match TabulatedNkTable.tryCreate wavelengths n k with
        | Error (EmptyTableErr reason)
        | Error (SampleCountMismatchErr reason)
        | Error (UnsortedSamplesErr reason) -> Error (NoDataErr reason)
        | Ok table ->
            let complexity =
                {
                    eps = EpsWithDispValue (IsotropicDispersive [ { wavelengthInterval = table.sampleRange; dispersion = TabulatedNK table } ])
                    magnetic = None
                    active = None
                }
            Ok
                {
                    id = MaterialId.create ()
                    name = name
                    category = category
                    description = Some (EntryDescription "Imported tabulated n,k (refractiveindex.info).")
                    physics = ComplexityPhysics complexity
                }

    // ==========================================================================
    // Analytic formula blocks (spec 0033 step 025). Each refractiveindex.info
    // `formula N` coefficient block (N = 1..7) is lowered — through the catalogue
    // `DispersionModel` (formulas 1/2) or directly to the serializable term data
    // of `Dispersion.fs` (the `SumOfTerms` shape) — to an `EpsAxisDispersion`
    // carried in an `EpsWithDispValue`-backed `MaterialComplexity`, so formula
    // imports are EDITABLE. All coefficients follow the µm RII convention; the
    // reduction to the meter base rides the term data's `wavelengthScale`,
    // derived through the sole `Units` seam (§D.2/§D.11).
    // ==========================================================================

    /// Metres per µm — the refractiveindex.info coefficient tabulation unit —
    /// derived through the sole `Units` seam (§D.11), never a literal factor.
    let private micrometerScale : float<meter> = toMeters Micrometer (UnitMagnitude 1.0)

    /// The k of a transparent analytic formula: the empty (zero) formula.
    let private zeroK : DispersionFormula =
        { terms = []; wavelengthScale = micrometerScale }

    /// An integer power of λ in one lowered term.
    type private TermPower =
        | TermPower of int

        member this.value = let (TermPower p) = this in p

    /// One coefficient of a formula block (the µm RII convention).
    type private RiiCoefficient =
        | RiiCoefficient of float

        member this.value = let (RiiCoefficient c) = this in c

        /// The real constant term c·x⁰.
        member this.realConstTerm : DispersionTerm =
            { lambda = 0.0; coefficients = [| this.value |]; power = 1; multiplier = 1.0 }

        /// This amplitude times x^p for any integer p (a negative p inverts the monomial).
        member this.realPowerTerm (power : TermPower) : DispersionTerm =
            let (TermPower p) = power
            let degree = abs p
            let coefficients = Array.init (degree + 1) (fun i -> if i = degree then 1.0 else 0.0)
            { lambda = 0.0; coefficients = coefficients; power = (if p >= 0 then 1 else -1); multiplier = this.value }

        /// The complex-formula constant term c·x⁰.
        member this.complexConstTerm : ComplexDispersionTerm =
            { lambda = Complex.Zero; coefficients = [| Complex.create this.value 0.0 |]; power = 1; multiplier = Complex.One }

        /// This amplitude times x^p over Complex for any integer p (a negative p inverts the monomial).
        member this.complexPowerTerm (power : TermPower) : ComplexDispersionTerm =
            let (TermPower p) = power
            let degree = abs p
            let coefficients = Array.init (degree + 1) (fun i -> if i = degree then Complex.One else Complex.Zero)
            { lambda = Complex.Zero; coefficients = coefficients; power = (if p >= 0 then 1 else -1); multiplier = Complex.create this.value 0.0 }

        /// A formula exponent usable in finite term data must be an integer.
        member this.tryTermPower : TermPower option =
            let p = this.value
            let r = Math.Round p
            if abs (p - r) < 1e-9 then Some (TermPower (int r)) else None

    /// One (amplitude, partner) pair of a formula block: a monomial's exponent, a resonance or an oscillator constant.
    type private CoefficientPair =
        {
            amplitude : RiiCoefficient
            partner : RiiCoefficient
        }

    /// The coefficients of a formula block, in the page's order.
    [<Softellect.Sys.NumericCarrier("the coefficient block of one refractiveindex.info formula page, read by position as the formula is lowered to term data (R3; as DispersionModels.SellmeierCoefficients)")>]
    type private FormulaCoefficients =
        | FormulaCoefficients of float[]

        /// Consecutive (amplitude, partner) coefficient pairs; a dangling odd tail
        /// drops (the pre-existing formula-1 behaviour).
        member this.coefficientPairs : CoefficientPair list =
            let (FormulaCoefficients values) = this
            values
            |> Array.chunkBySize 2
            |> Array.choose (fun p -> if p.Length = 2 then Some { amplitude = RiiCoefficient p.[0]; partner = RiiCoefficient p.[1] } else None)
            |> Array.toList

    /// Whether a Sellmeier page squares its resonances (formula 1) or tabulates them squared already (formula 2).
    type private ResonanceShape =
        | SquaredResonance
        | PlainResonance

    /// Σ cᵢ·x^{pᵢ} coefficient pairs as monomial terms via `build` (real or
    /// complex); zero-amplitude pairs drop, and a non-integer exponent is the
    /// typed unsupported-formula error — finite term data cannot carry λ^2.5.
    let private monomialTerms (formula : RiiFormulaNumber) (build : RiiCoefficient -> TermPower -> 'Term) (pairs : CoefficientPair list) : Result<'Term list, ImportError> =
        pairs
        |> List.filter (fun pair -> pair.amplitude.value <> 0.0)
        |> List.fold
            (fun acc pair ->
                match acc with
                | Error e -> Error e
                | Ok terms ->
                    match pair.partner.tryTermPower with
                    | Some p -> Ok (terms @ [ build pair.amplitude p ])
                    | None -> Error (UnsupportedFormulaErr (formula, $"exponent %g{pair.partner.value} is not an integer — not a finite term sum")))
            (Ok [])

    /// Exact ComplexEps terms for amplitude·x^p/(x² − a), integer p ≥ 0 (the RII
    /// formula-4 resonant shape): long division by (x² − a) (x² ≡ a) leaves the
    /// polynomial quotient Σ_{k<⌊p/2⌋} a^k·x^(p−2−2k) plus the residual pole
    /// a^⌊p/2⌋·x^(p mod 2)/(x² − a); an odd residual splits exactly at the ±√a
    /// simple poles (complex when a < 0). a = 0 degenerates to the monomial x^(p−2).
    let private resonantTerms (amplitude : RiiCoefficient) (power : TermPower) (resonance : RiiCoefficient) : ComplexDispersionTerm list =
        let (TermPower p) = power
        let (RiiCoefficient a) = resonance
        if a = 0.0 then [ amplitude.complexPowerTerm (TermPower (p - 2)) ]
        else
            let m = p / 2
            let quotient =
                if m = 0 then []
                else
                    let degree = p - 2
                    let coefficients =
                        Array.init (degree + 1) (fun i ->
                            let diff = degree - i
                            if diff % 2 = 0 && diff / 2 < m then Complex.create (a ** float (diff / 2)) 0.0 else Complex.Zero)
                    [ { lambda = Complex.Zero; coefficients = coefficients; power = 1; multiplier = Complex.create amplitude.value 0.0 } ]
            let residual = amplitude.value * a ** float m
            let pole =
                if p % 2 = 0 then
                    [ { lambda = Complex.Zero; coefficients = [| Complex.create (-a) 0.0; Complex.Zero; Complex.One |]; power = -1; multiplier = Complex.create residual 0.0 } ]
                else
                    let root = Complex.Sqrt (Complex.create a 0.0)
                    let half = Complex.create (residual / 2.0) 0.0
                    [
                        { lambda = Complex.Zero; coefficients = [| -root; Complex.One |]; power = -1; multiplier = half }
                        { lambda = Complex.Zero; coefficients = [| root; Complex.One |]; power = -1; multiplier = half }
                    ]
            quotient @ pole

    /// RII formulas 1/2 (Sellmeier / Sellmeier-2): n² − 1 = c₀ + Σ Aᵢ·λ²/(λ² − Bᵢ[²])
    /// (formula 1 squares its resonances, formula 2 does not). The constant c₀
    /// rides as a resonance-free Sellmeier term (A = c₀, B = 0: c₀·λ²/λ² = c₀),
    /// so the whole page maps onto the catalogue `Sellmeier` model and its exact
    /// `toEpsAxis` lowering — no oscillator identity re-derived here. `toEpsAxis`
    /// is total, so the lowering cannot fail.
    let private sellmeierAxis (resonance : ResonanceShape) (coefficients : FormulaCoefficients) : Result<EpsAxisDispersion, ImportError> =
        let (FormulaCoefficients nums) = coefficients
        let pairs = (FormulaCoefficients nums.[1..]).coefficientPairs
        let model =
            Sellmeier
                {
                    b = nums.[0] :: (pairs |> List.map (fun pair -> pair.amplitude.value))
                    c =
                        0.0
                        :: (pairs
                            |> List.map (fun pair ->
                                let r = pair.partner.value
                                match resonance with
                                | SquaredResonance -> r * r
                                | PlainResonance -> r))
                    wavelengthUnit = Micrometer
                    thermoOptic = None
                }
        Ok model.toEpsAxis

    /// RII formula 3 (polynomial): n² = c₀ + Σ cᵢ·λ^{pᵢ} — ε term data directly.
    let private polynomialAxis (coefficients : FormulaCoefficients) : Result<EpsAxisDispersion, ImportError> =
        let (FormulaCoefficients nums) = coefficients
        monomialTerms (RiiFormulaNumber 3) (fun amplitude p -> amplitude.complexPowerTerm p) (FormulaCoefficients nums.[1..]).coefficientPairs
        |> Result.map (fun terms -> ComplexEps { terms = (RiiCoefficient nums.[0]).complexConstTerm :: terms; wavelengthScale = micrometerScale })

    /// RII formula 4 (the RefractiveIndex.INFO formula): n² = c₀
    /// + c₁·λ^{c₂}/(λ² − c₃^{c₄}) + c₅·λ^{c₆}/(λ² − c₇^{c₈}) + Σ cⱼ·λ^{cⱼ₊₁}
    /// (j = 9, 11, 13, 15) — up to 17 coefficients, absent ones zero. The two
    /// resonant terms reduce exactly through `resonantTerms`; the tail is plain
    /// monomials.
    let private riiFormulaAxis (coefficients : FormulaCoefficients) : Result<EpsAxisDispersion, ImportError> =
        let (FormulaCoefficients nums) = coefficients
        let c = Array.init 17 (fun i -> if i < nums.Length then nums.[i] else 0.0)
        let resonant (amplitude : float) (exponent : float) (resonanceBase : float) (resonancePower : float) : Result<ComplexDispersionTerm list, ImportError> =
            if amplitude = 0.0 then Ok []
            else
                match (RiiCoefficient exponent).tryTermPower with
                | Some (TermPower p as power) when p >= 0 -> Ok (resonantTerms (RiiCoefficient amplitude) power (RiiCoefficient (resonanceBase ** resonancePower)))
                | Some (TermPower p) -> Error (UnsupportedFormulaErr (RiiFormulaNumber 4, $"negative resonant-term exponent %d{p} is not supported"))
                | None -> Error (UnsupportedFormulaErr (RiiFormulaNumber 4, $"exponent %g{exponent} is not an integer — not a finite term sum"))
        match resonant c.[1] c.[2] c.[3] c.[4] with
        | Error e -> Error e
        | Ok first ->
            match resonant c.[5] c.[6] c.[7] c.[8] with
            | Error e -> Error e
            | Ok second ->
                monomialTerms (RiiFormulaNumber 4) (fun amplitude p -> amplitude.complexPowerTerm p) (FormulaCoefficients c.[9..]).coefficientPairs
                |> Result.map (fun tail ->
                    ComplexEps { terms = (RiiCoefficient c.[0]).complexConstTerm :: first @ second @ tail; wavelengthScale = micrometerScale })

    /// RII formula 5 (Cauchy): n = c₀ + Σ cᵢ·λ^{pᵢ} — an n formula directly; k = 0.
    let private cauchyAxis (coefficients : FormulaCoefficients) : Result<EpsAxisDispersion, ImportError> =
        let (FormulaCoefficients nums) = coefficients
        monomialTerms (RiiFormulaNumber 5) (fun amplitude p -> amplitude.realPowerTerm p) (FormulaCoefficients nums.[1..]).coefficientPairs
        |> Result.map (fun terms ->
            RealNK { nFormula = { terms = (RiiCoefficient nums.[0]).realConstTerm :: terms; wavelengthScale = micrometerScale }; kFormula = zeroK })

    /// RII formula 6 (gases): n = 1 + c₀ + Σ cᵢ/(dᵢ − λ⁻²). Each oscillator is
    /// exact real term data: cᵢ/(dᵢ − x⁻²) = cᵢ·x²/(dᵢ·x² − 1)
    /// = cᵢ/dᵢ + (cᵢ/dᵢ²)/(x² − 1/dᵢ), and dᵢ = 0 degenerates to −cᵢ·x²; k = 0.
    let private gasesAxis (coefficients : FormulaCoefficients) : Result<EpsAxisDispersion, ImportError> =
        let (FormulaCoefficients nums) = coefficients
        let oscillators =
            (FormulaCoefficients nums.[1..]).coefficientPairs
            |> List.filter (fun pair -> pair.amplitude.value <> 0.0)
            |> List.collect (fun pair ->
                let amplitude = pair.amplitude.value
                let d = pair.partner.value
                if d = 0.0 then [ (RiiCoefficient (-amplitude)).realPowerTerm (TermPower 2) ]
                else
                    [
                        (RiiCoefficient (amplitude / d)).realConstTerm
                        { lambda = 0.0; coefficients = [| -1.0 / d; 0.0; 1.0 |]; power = -1; multiplier = amplitude / (d * d) }
                    ])
        Ok (RealNK { nFormula = { terms = (RiiCoefficient (1.0 + nums.[0])).realConstTerm :: oscillators; wavelengthScale = micrometerScale }; kFormula = zeroK })

    /// Herzberger's fixed 0.028 µm² pole — a constant of the PUBLISHED formula
    /// (Herzberger, JOSA 49, 1959), not a unit conversion.
    let private herzbergerPole : RiiCoefficient = RiiCoefficient 0.028

    /// RII formula 7 (Herzberger): n = c₀ + c₁/(λ² − 0.028) + c₂/(λ² − 0.028)²
    /// + c₃·λ² + c₄·λ⁴ + c₅·λ⁶ — an n formula directly (absent coefficients
    /// zero); k = 0.
    let private herzbergerAxis (coefficients : FormulaCoefficients) : Result<EpsAxisDispersion, ImportError> =
        let (FormulaCoefficients nums) = coefficients
        let c = Array.init 6 (fun i -> RiiCoefficient (if i < nums.Length then nums.[i] else 0.0))
        let pole = [| -herzbergerPole.value; 0.0; 1.0 |]
        let terms =
            [
                c.[0].realConstTerm
                { lambda = 0.0; coefficients = pole; power = -1; multiplier = c.[1].value }
                { lambda = 0.0; coefficients = pole; power = -2; multiplier = c.[2].value }
                c.[3].realPowerTerm (TermPower 2)
                c.[4].realPowerTerm (TermPower 4)
                c.[5].realPowerTerm (TermPower 6)
            ]
        Ok (RealNK { nFormula = { terms = terms; wavelengthScale = micrometerScale }; kFormula = zeroK })

    /// Lower a supported formula's coefficient block to the per-axis
    /// serializable term data (spec 0033 step 025 / §B). The catch-all restates
    /// the `unsupportedReason` gate so the dispatch stays total.
    let private lowerFormulaAxis (formula : RiiFormulaNumber) (coefficients : FormulaCoefficients) : Result<EpsAxisDispersion, ImportError> =
        match formula with
        | RiiFormulaNumber 1 -> sellmeierAxis SquaredResonance coefficients
        | RiiFormulaNumber 2 -> sellmeierAxis PlainResonance coefficients
        | RiiFormulaNumber 3 -> polynomialAxis coefficients
        | RiiFormulaNumber 4 -> riiFormulaAxis coefficients
        | RiiFormulaNumber 5 -> cauchyAxis coefficients
        | RiiFormulaNumber 6 -> gasesAxis coefficients
        | RiiFormulaNumber 7 -> herzbergerAxis coefficients
        | RiiFormulaNumber n -> Error (UnsupportedFormulaErr (formula, $"unknown refractiveindex.info dispersion formula %d{n} (supported: 1-7)"))

    /// Assemble the imported entry for a lowered formula (spec 0033 step 025):
    /// the single-segment isotropic tree over the page's wavelength range
    /// carried in an `EpsWithDispValue`-backed `MaterialComplexity`, so the
    /// import is EDITABLE. A fresh `MaterialId` is minted — imports carry no persisted
    /// identity of their own (step 002).
    let private entryOfAxis (formula : RiiFormulaNumber) (interval : WaveLengthInterval) (axis : EpsAxisDispersion) : MaterialEntry =
        let complexity =
            {
                eps = EpsWithDispValue (IsotropicDispersive [ { wavelengthInterval = interval; dispersion = axis } ])
                magnetic = None
                active = None
            }
        {
            id = MaterialId.create ()
            name = EntryName $"Imported (refractiveindex.info formula %d{formula.value})"
            category = CategoryIds.glass
            description = Some (EntryDescription $"Imported %s{formula.family} (refractiveindex.info formula %d{formula.value}).")
            physics = ComplexityPhysics complexity
        }

    type RefractiveIndexInfoText with

        /// Import this refractiveindex.info YAML page (§D.9). Supports the `tabulated n`/
        /// `tabulated nk` layout and the analytic coefficient blocks `formula 1`–
        /// `formula 7` (Sellmeier, Sellmeier-2, polynomial, the RII formula, Cauchy,
        /// gases, Herzberger); λ is taken in µm (the refractiveindex.info convention)
        /// and reduced to meters through the sole `Units` seam. Each formula is
        /// lowered to an `EpsAxisDispersion` carried in an `EpsWithDispValue`-backed
        /// `MaterialComplexity`, so formula imports are EDITABLE (spec 0033 step
        /// 025); `formula 8`/`formula 9` (and any unknown number) return the typed
        /// `UnsupportedFormulaErr` error.
        member this.importMaterial : Result<MaterialEntry, ImportError> =
            try
                let lines = this.value.Replace("\r\n", "\n").Split('\n')
                let page = RiiPage lines
                match page.tryFormulaNumber with
                | Some formula ->
                    match formula.unsupportedReason with
                    | Some reason -> Error (UnsupportedFormulaErr (formula, reason))
                    | None ->
                        match lines |> Array.tryFind (fun l -> l.TrimStart().StartsWith "coefficients:") with
                        | Some cl ->
                            let nums = RiiPage.floatsOn (cl.Substring(cl.IndexOf(':') + 1))
                            if nums.Length < 1 then Error (NoDataErr $"formula %d{formula.value}: no coefficients")
                            else
                                lowerFormulaAxis formula (FormulaCoefficients nums)
                                |> Result.map (entryOfAxis formula page.rangeInterval)
                        | None -> Error (MalformedYamlErr $"formula %d{formula.value} block has no coefficients line")
                | None ->
                    let rows = page.tabulatedRows Micrometer
                    if rows.Length = 0 then Error (NoDataErr "no tabulated rows found")
                    else entryFromTabulated (EntryName "Imported (refractiveindex.info)") CategoryIds.semiconductor rows
            with e -> Error (MalformedYamlErr e.Message)

    type NkCsvText with

        /// Import this refractiveindex.info-style n,k CSV (§D.9). Columns are
        /// `wavelength, n[, k]`; the wavelength column header declares the unit (nm/µm/Å),
        /// defaulting to µm. λ is reduced to the meter base via `Units.toMeters`.
        member this.importMaterial : Result<MaterialEntry, ImportError> =
            let tryFloat (s : string) : float option =
                match Double.TryParse(s.Trim(), NumberStyles.Float, inv) with
                | true, v -> Some v
                | _ -> None

            // A boundary unit from the wavelength-column header (refractiveindex.info tabulates µm; a CSV
            // may declare nm). Defaults to µm.
            let unitFromHeader (header : string) : UnitOfMeasure =
                let h = header.ToLowerInvariant()
                if h.Contains "nm" then Nanometer
                elif h.Contains "µm" || h.Contains "um" || h.Contains "mkm" || h.Contains "micro" then Micrometer
                elif h.Contains "ang" || h.Contains "å" then Angstrom
                else Micrometer

            try
                let csv = CsvFile.Parse(this.value)
                let unit =
                    match csv.Headers with
                    | Some h when h.Length > 0 -> unitFromHeader h.[0]
                    | _ -> Micrometer
                let rows =
                    csv.Rows
                    |> Seq.choose (fun r ->
                        let cols = r.Columns
                        if cols.Length >= 2 then
                            match tryFloat cols.[0], tryFloat cols.[1] with
                            | Some lam, Some n ->
                                let k = if cols.Length >= 3 then (tryFloat cols.[2] |> Option.defaultValue 0.0) else 0.0
                                Some { wavelength = toMeters unit (UnitMagnitude lam); n = n; k = k }
                            | _ -> None
                        else None)
                    |> Array.ofSeq
                if rows.Length = 0 then Error (NoDataErr "no CSV data rows found")
                else entryFromTabulated (EntryName "Imported (CSV)") CategoryIds.glass rows
            with e -> Error (MalformedCsvErr e.Message)

    /// Export n,k sampled over a `Range<WaveLength>` to CSV text (§D.9), reading the
    /// entry's dispersion through the engine `getEps` path. n is recovered as
    /// `√(ε₁₁)`. λ is written in nm via the `Units` seam (display only; the engine
    /// data stays in meters).
    let exportCsv (entry : MaterialEntry) (range : Range<WaveLength>) : NkCsvText =
        let sb = StringBuilder()
        sb.AppendLine("wavelength_nm,n,k") |> ignore
        // Sample λ linearly in the canonical meter base over the range (the engine's
        // `getWaveLengthValue` re-wraps the meter magnitude as a Nm value, so we walk
        // meters directly and build the sample WaveLength via the engine `nmToMeter`).
        let s = range.startValue.value
        let e = range.endValue.value
        let n = max 1 range.numberOfPoints.value
        for i in 0 .. range.numberOfPoints.value do
            let m = s + (e - s) * (float i) / (float n)
            let w = WaveLength.Nm(m / nmToMeter)
            let nComplex = Complex.Sqrt (entry.properties.epsWithDisp.getEps w).[0, 0]
            let nm = (wavelengthToUnit Nanometer w).value
            sb.AppendLine(String.Format(inv, "{0:G},{1:G},{2:G}", nm, nComplex.Real, nComplex.Imaginary)) |> ignore
        NkCsvText (sb.ToString())
