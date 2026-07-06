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
/// (§D.2/§D.11) according to the file's declared unit. Tabulated entries interpolate
/// LINEARLY in λ so the closure is total over the chart range (no caching layer).
/// The error channel is `Result<_, ImportError>` — parsing never throws past the
/// boundary. The `string` argument and `exportCsv`'s result are the raw file TEXT,
/// keeping filesystem IO and its exceptions out of the parser.
module MaterialImport =

    /// Net-new import error channel (errors as values, §0 / §D.9).
    /// `UnsupportedFormula` (spec 0033 step 025) types a refractiveindex.info
    /// dispersion formula the importer cannot lower to editable term data
    /// (formulas 8/9, an unknown number, or a non-integer exponent); it carries
    /// the offending formula number plus a diagnostic reason.
    type ImportError =
        | MalformedYaml of string
        | MalformedCsv of string
        | NoData of string
        | UnsupportedFormula of formulaNumber : int * reason : string

    let private inv = CultureInfo.InvariantCulture

    let private tryFloat (s : string) : float option =
        match Double.TryParse(s.Trim(), NumberStyles.Float, inv) with
        | true, v -> Some v
        | _ -> None

    /// All floating-point tokens on a line (comma/space/tab separated).
    let private parseFloats (line : string) : float[] =
        line.Split([| ' '; '\t'; ',' |], StringSplitOptions.RemoveEmptyEntries)
        |> Array.choose tryFloat

    /// Pick a boundary unit from a wavelength-column header (refractiveindex.info
    /// tabulates µm; a CSV may declare nm). Defaults to µm.
    let private unitFromHeader (header : string) : UnitOfMeasure =
        let h = header.ToLowerInvariant()
        if h.Contains "nm" then Nanometer
        elif h.Contains "µm" || h.Contains "um" || h.Contains "mkm" || h.Contains "micro" then Micrometer
        elif h.Contains "ang" || h.Contains "å" then Angstrom
        else Micrometer

    /// Linear-in-λ interpolation over the tabulated samples, clamped to the ends so
    /// the closure is total. Samples are `(λ in meters, n, k)`, sorted by λ.
    let private buildTabulatedClosure (points : (float * float * float)[]) : WaveLength -> Eps =
        let sorted = points |> Array.sortBy (fun (l, _, _) -> l)
        fun w ->
            let lam = w.value / 1.0<meter>
            let n, k =
                if sorted.Length = 1 then
                    let (_, n, k) = sorted.[0] in n, k
                else
                    let (l0, n0, k0) = sorted.[0]
                    let (lN, nN, kN) = sorted.[sorted.Length - 1]
                    if lam <= l0 then n0, k0
                    elif lam >= lN then nN, kN
                    else
                        // Find the bracketing pair [i, i+1] and lerp.
                        let mutable i = 0
                        while i < sorted.Length - 2 && (let (l, _, _) = sorted.[i + 1] in l < lam) do
                            i <- i + 1
                        let (la, na, ka) = sorted.[i]
                        let (lb, nb, kb) = sorted.[i + 1]
                        let t = if lb = la then 0.0 else (lam - la) / (lb - la)
                        na + (nb - na) * t, ka + (kb - ka) * t
            createComplex n k |> ComplexRefractionIndex |> Eps.fromComplexRefractionIndex

    /// An imported entry MINTS a fresh `MaterialId` (spec 0033 step 002) — imports carry no
    /// persisted identity of their own; the stable Guid ids live with the library seeds.
    let private entryFromTabulated (name : string) (category : MaterialCategory) (points : (float * float * float)[]) : MaterialEntry =
        {
            id = MaterialId.create ()
            name = name
            category = category
            description = Some "Imported tabulated n,k (refractiveindex.info)."
            properties = isotropicProperties (EpsWithDisp (buildTabulatedClosure points))
            // Tabulated imports stay closure-backed (view-only): linear interpolation
            // over samples is not finite term data. Formula imports carry an editable
            // complexity instead (spec 0033 step 025).
            complexity = None
        }

    /// Collect tabulated `(λ in meters, n, k)` rows from data lines (those starting
    /// with a number), reducing λ from `unit` to the meter base via `Units.toMeters`.
    /// Two-column `tabulated n` rows map k = 0.
    let private tabulatedRows (unit : UnitOfMeasure) (lines : string[]) : (float * float * float)[] =
        lines
        |> Array.choose (fun line ->
            let t = line.Trim()
            if t.Length > 0 && (Char.IsDigit t.[0] || t.[0] = '-' || t.[0] = '.') then
                let f = parseFloats t
                if f.Length >= 2 then
                    let lamMeters = (toMeters unit f.[0]) / 1.0<meter>
                    Some (lamMeters, f.[1], (if f.Length >= 3 then f.[2] else 0.0))
                else None
            else None)

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
    let private micrometerScale : double = (toMeters Micrometer 1.0) / 1.0<meter>

    /// The k of a transparent analytic formula: the empty (zero) formula.
    let private zeroK : DispersionFormula =
        { terms = []; wavelengthScale = micrometerScale }

    /// A real constant term c·x⁰.
    let private realConstTerm (c : float) : DispersionTerm =
        { lambda = 0.0; coefficients = [| c |]; power = 1; multiplier = 1.0 }

    /// amplitude·x^p for any integer p (a negative p inverts the monomial).
    let private realPowerTerm (amplitude : float) (p : int) : DispersionTerm =
        let degree = abs p
        let coefficients = Array.init (degree + 1) (fun i -> if i = degree then 1.0 else 0.0)
        { lambda = 0.0; coefficients = coefficients; power = (if p >= 0 then 1 else -1); multiplier = amplitude }

    /// A complex-formula constant term c·x⁰.
    let private complexConstTerm (c : float) : ComplexDispersionTerm =
        { lambda = Complex.Zero; coefficients = [| createComplex c 0.0 |]; power = 1; multiplier = Complex.One }

    /// amplitude·x^p over Complex for any integer p (a negative p inverts the monomial).
    let private complexPowerTerm (amplitude : float) (p : int) : ComplexDispersionTerm =
        let degree = abs p
        let coefficients = Array.init (degree + 1) (fun i -> if i = degree then Complex.One else Complex.Zero)
        { lambda = Complex.Zero; coefficients = coefficients; power = (if p >= 0 then 1 else -1); multiplier = createComplex amplitude 0.0 }

    /// A formula exponent usable in finite term data must be an integer.
    let private tryIntegerExponent (p : float) : int option =
        let r = Math.Round p
        if abs (p - r) < 1e-9 then Some (int r) else None

    /// Consecutive (amplitude, partner) coefficient pairs; a dangling odd tail
    /// drops (the pre-existing formula-1 behaviour).
    let private coefficientPairs (values : float[]) : (float * float) list =
        values
        |> Array.chunkBySize 2
        |> Array.choose (fun p -> if p.Length = 2 then Some (p.[0], p.[1]) else None)
        |> Array.toList

    /// Σ cᵢ·x^{pᵢ} coefficient pairs as monomial terms via `build` (real or
    /// complex); zero-amplitude pairs drop, and a non-integer exponent is the
    /// typed unsupported-formula error — finite term data cannot carry λ^2.5.
    let private monomialTerms (formulaNumber : int) (build : float -> int -> 'term) (pairs : (float * float) list) : Result<'term list, ImportError> =
        pairs
        |> List.filter (fun (amplitude, _) -> amplitude <> 0.0)
        |> List.fold
            (fun acc (amplitude, exponent) ->
                match acc with
                | Error e -> Error e
                | Ok terms ->
                    match tryIntegerExponent exponent with
                    | Some p -> Ok (terms @ [ build amplitude p ])
                    | None -> Error (UnsupportedFormula (formulaNumber, sprintf "exponent %g is not an integer — not a finite term sum" exponent)))
            (Ok [])

    /// Exact ComplexEps terms for amplitude·x^p/(x² − a), integer p ≥ 0 (the RII
    /// formula-4 resonant shape): long division by (x² − a) (x² ≡ a) leaves the
    /// polynomial quotient Σ_{k<⌊p/2⌋} a^k·x^(p−2−2k) plus the residual pole
    /// a^⌊p/2⌋·x^(p mod 2)/(x² − a); an odd residual splits exactly at the ±√a
    /// simple poles (complex when a < 0). a = 0 degenerates to the monomial x^(p−2).
    let private resonantTerms (amplitude : float) (p : int) (a : float) : ComplexDispersionTerm list =
        if a = 0.0 then [ complexPowerTerm amplitude (p - 2) ]
        else
            let m = p / 2
            let quotient =
                if m = 0 then []
                else
                    let degree = p - 2
                    let coefficients =
                        Array.init (degree + 1) (fun i ->
                            let diff = degree - i
                            if diff % 2 = 0 && diff / 2 < m then createComplex (a ** float (diff / 2)) 0.0 else Complex.Zero)
                    [ { lambda = Complex.Zero; coefficients = coefficients; power = 1; multiplier = createComplex amplitude 0.0 } ]
            let residual = amplitude * a ** float m
            let pole =
                if p % 2 = 0 then
                    [ { lambda = Complex.Zero; coefficients = [| createComplex (-a) 0.0; Complex.Zero; Complex.One |]; power = -1; multiplier = createComplex residual 0.0 } ]
                else
                    let root = Complex.Sqrt (createComplex a 0.0)
                    let half = createComplex (residual / 2.0) 0.0
                    [
                        { lambda = Complex.Zero; coefficients = [| -root; Complex.One |]; power = -1; multiplier = half }
                        { lambda = Complex.Zero; coefficients = [| root; Complex.One |]; power = -1; multiplier = half }
                    ]
            quotient @ pole

    /// RII formulas 1/2 (Sellmeier / Sellmeier-2): n² − 1 = c₀ + Σ Aᵢ·λ²/(λ² − Bᵢ[²])
    /// (formula 1 squares its resonances, formula 2 does not). The constant c₀
    /// rides as a resonance-free Sellmeier term (A = c₀, B = 0: c₀·λ²/λ² = c₀),
    /// so the whole page maps onto the catalogue `Sellmeier` model and its exact
    /// `toEpsAxis` lowering — no oscillator identity re-derived here. The error
    /// branch is unreachable (a Sellmeier model always lowers) but keeps the
    /// match total.
    let private sellmeierAxis (squaredResonance : bool) (nums : float[]) : Result<EpsAxisDispersion, ImportError> =
        let pairs = coefficientPairs nums.[1..]
        let model =
            Sellmeier
                {
                    b = nums.[0] :: (pairs |> List.map fst)
                    c = 0.0 :: (pairs |> List.map (fun (_, resonance) -> if squaredResonance then resonance * resonance else resonance))
                    wavelengthUnit = Micrometer
                    thermoOptic = None
                }
        match toEpsAxis model with
        | Ok axis -> Ok axis
        | Error (NotAFiniteTermSum reason) -> Error (MalformedYaml reason)

    /// RII formula 3 (polynomial): n² = c₀ + Σ cᵢ·λ^{pᵢ} — ε term data directly.
    let private polynomialAxis (nums : float[]) : Result<EpsAxisDispersion, ImportError> =
        monomialTerms 3 complexPowerTerm (coefficientPairs nums.[1..])
        |> Result.map (fun terms -> ComplexEps { terms = complexConstTerm nums.[0] :: terms; wavelengthScale = micrometerScale })

    /// RII formula 4 (the RefractiveIndex.INFO formula): n² = c₀
    /// + c₁·λ^{c₂}/(λ² − c₃^{c₄}) + c₅·λ^{c₆}/(λ² − c₇^{c₈}) + Σ cⱼ·λ^{cⱼ₊₁}
    /// (j = 9, 11, 13, 15) — up to 17 coefficients, absent ones zero. The two
    /// resonant terms reduce exactly through `resonantTerms`; the tail is plain
    /// monomials.
    let private riiFormulaAxis (nums : float[]) : Result<EpsAxisDispersion, ImportError> =
        let c = Array.init 17 (fun i -> if i < nums.Length then nums.[i] else 0.0)
        let resonant (amplitude : float) (exponent : float) (resonanceBase : float) (resonancePower : float) : Result<ComplexDispersionTerm list, ImportError> =
            if amplitude = 0.0 then Ok []
            else
                match tryIntegerExponent exponent with
                | Some p when p >= 0 -> Ok (resonantTerms amplitude p (resonanceBase ** resonancePower))
                | Some p -> Error (UnsupportedFormula (4, sprintf "negative resonant-term exponent %d is not supported" p))
                | None -> Error (UnsupportedFormula (4, sprintf "exponent %g is not an integer — not a finite term sum" exponent))
        match resonant c.[1] c.[2] c.[3] c.[4] with
        | Error e -> Error e
        | Ok first ->
            match resonant c.[5] c.[6] c.[7] c.[8] with
            | Error e -> Error e
            | Ok second ->
                monomialTerms 4 complexPowerTerm (coefficientPairs c.[9..])
                |> Result.map (fun tail ->
                    ComplexEps { terms = complexConstTerm c.[0] :: first @ second @ tail; wavelengthScale = micrometerScale })

    /// RII formula 5 (Cauchy): n = c₀ + Σ cᵢ·λ^{pᵢ} — an n formula directly; k = 0.
    let private cauchyAxis (nums : float[]) : Result<EpsAxisDispersion, ImportError> =
        monomialTerms 5 realPowerTerm (coefficientPairs nums.[1..])
        |> Result.map (fun terms ->
            RealNK ({ terms = realConstTerm nums.[0] :: terms; wavelengthScale = micrometerScale }, zeroK))

    /// RII formula 6 (gases): n = 1 + c₀ + Σ cᵢ/(dᵢ − λ⁻²). Each oscillator is
    /// exact real term data: cᵢ/(dᵢ − x⁻²) = cᵢ·x²/(dᵢ·x² − 1)
    /// = cᵢ/dᵢ + (cᵢ/dᵢ²)/(x² − 1/dᵢ), and dᵢ = 0 degenerates to −cᵢ·x²; k = 0.
    let private gasesAxis (nums : float[]) : Result<EpsAxisDispersion, ImportError> =
        let oscillators =
            coefficientPairs nums.[1..]
            |> List.filter (fun (amplitude, _) -> amplitude <> 0.0)
            |> List.collect (fun (amplitude, d) ->
                if d = 0.0 then [ realPowerTerm (-amplitude) 2 ]
                else
                    [
                        realConstTerm (amplitude / d)
                        { lambda = 0.0; coefficients = [| -1.0 / d; 0.0; 1.0 |]; power = -1; multiplier = amplitude / (d * d) }
                    ])
        Ok (RealNK ({ terms = realConstTerm (1.0 + nums.[0]) :: oscillators; wavelengthScale = micrometerScale }, zeroK))

    /// Herzberger's fixed 0.028 µm² pole — a constant of the PUBLISHED formula
    /// (Herzberger, JOSA 49, 1959), not a unit conversion.
    let private herzbergerPole = 0.028

    /// RII formula 7 (Herzberger): n = c₀ + c₁/(λ² − 0.028) + c₂/(λ² − 0.028)²
    /// + c₃·λ² + c₄·λ⁴ + c₅·λ⁶ — an n formula directly (absent coefficients
    /// zero); k = 0.
    let private herzbergerAxis (nums : float[]) : Result<EpsAxisDispersion, ImportError> =
        let c = Array.init 6 (fun i -> if i < nums.Length then nums.[i] else 0.0)
        let pole = [| -herzbergerPole; 0.0; 1.0 |]
        let terms =
            [
                realConstTerm c.[0]
                { lambda = 0.0; coefficients = pole; power = -1; multiplier = c.[1] }
                { lambda = 0.0; coefficients = pole; power = -2; multiplier = c.[2] }
                realPowerTerm c.[3] 2
                realPowerTerm c.[4] 4
                realPowerTerm c.[5] 6
            ]
        Ok (RealNK ({ terms = terms; wavelengthScale = micrometerScale }, zeroK))

    /// Why a formula number cannot produce editable term data (spec 0033 step
    /// 025): 8 (Retro) hides n behind the rational transform (n² − 1)/(n² + 2),
    /// which no finite term sum inverts; 9 (Exotic) is outside the importer's
    /// supported set; any other number is not a refractiveindex.info formula.
    /// `None` = supported (1–7).
    let private unsupportedReason (formulaNumber : int) : string option =
        match formulaNumber with
        | 1 | 2 | 3 | 4 | 5 | 6 | 7 -> None
        | 8 -> Some "formula 8 (Retro) defines (n² − 1)/(n² + 2) — a rational transform with no finite-term eps lowering"
        | 9 -> Some "formula 9 (Exotic) is not lowered to editable term data"
        | n -> Some (sprintf "unknown refractiveindex.info dispersion formula %d (supported: 1-7)" n)

    /// Lower a supported formula's coefficient block to the per-axis
    /// serializable term data (spec 0033 step 025 / §B). The catch-all restates
    /// the `unsupportedReason` gate so the dispatch stays total.
    let private lowerFormulaAxis (formulaNumber : int) (nums : float[]) : Result<EpsAxisDispersion, ImportError> =
        match formulaNumber with
        | 1 -> sellmeierAxis true nums
        | 2 -> sellmeierAxis false nums
        | 3 -> polynomialAxis nums
        | 4 -> riiFormulaAxis nums
        | 5 -> cauchyAxis nums
        | 6 -> gasesAxis nums
        | 7 -> herzbergerAxis nums
        | n -> Error (UnsupportedFormula (n, sprintf "unknown refractiveindex.info dispersion formula %d (supported: 1-7)" n))

    /// The `type: formula N` declaration of an analytic RII page (None on a
    /// tabulated page). N is read but NOT validated here — `unsupportedReason`
    /// types the numbers the importer cannot lower.
    let private tryFormulaNumber (lines : string[]) : int option =
        lines
        |> Array.tryPick (fun l ->
            if l.Contains "type:" && l.Contains "formula" then
                let after = l.Substring(l.IndexOf("formula", StringComparison.Ordinal) + "formula".Length)
                after.Split([| ' '; '\t'; '"'; '\'' |], StringSplitOptions.RemoveEmptyEntries)
                |> Array.tryHead
                |> Option.bind (fun token ->
                    match Int32.TryParse(token, NumberStyles.Integer, inv) with
                    | true, v -> Some v
                    | _ -> None)
            else None)

    /// The page's `wavelength_range: a b` (µm, the RII convention) as the
    /// imported segment's interval, through the `Units` seam. A page without one
    /// gets the zero nominal interval — with a single segment the topmost
    /// segment extrapolates for every wavelength (`Dispersion.fs:346`), so the
    /// bounds are descriptive, not clamping.
    let private rangeInterval (lines : string[]) : WaveLengthInterval =
        let bounds =
            lines
            |> Array.tryFind (fun l ->
                let t = l.TrimStart()
                t.StartsWith "wavelength_range:" || t.StartsWith "range:")
            |> Option.map (fun l -> parseFloats (l.Substring(l.IndexOf(':') + 1)))
        match bounds with
        | Some f when f.Length >= 2 -> { lower = toWaveLength Micrometer f.[0]; upper = toWaveLength Micrometer f.[1] }
        | _ ->
            let nominal = toWaveLength Micrometer 0.0
            { lower = nominal; upper = nominal }

    /// The display family of a supported formula number (entry description).
    let private formulaFamily (formulaNumber : int) : string =
        match formulaNumber with
        | 1 -> "Sellmeier"
        | 2 -> "Sellmeier-2"
        | 3 -> "polynomial"
        | 4 -> "RefractiveIndex.INFO-formula"
        | 5 -> "Cauchy"
        | 6 -> "gases"
        | 7 -> "Herzberger"
        | _ -> "dispersion-formula"

    /// Assemble the imported entry for a lowered formula (spec 0033 step 025):
    /// the single-segment isotropic tree over the page's wavelength range
    /// carried in an `EpsWithDispValue`-backed `MaterialComplexity`, so the
    /// import is EDITABLE; `properties` IS `complexity.toProperties` (the seed
    /// invariant). A fresh `MaterialId` is minted — imports carry no persisted
    /// identity of their own (step 002).
    let private entryOfAxis (formulaNumber : int) (interval : WaveLengthInterval) (axis : EpsAxisDispersion) : MaterialEntry =
        let complexity =
            {
                eps = EpsWithDispValue (IsotropicDispersive [ { wavelengthInterval = interval; dispersion = axis } ])
                magnetic = None
                active = None
            }
        {
            id = MaterialId.create ()
            name = sprintf "Imported (refractiveindex.info formula %d)" formulaNumber
            category = Glass
            description = Some (sprintf "Imported %s (refractiveindex.info formula %d)." (formulaFamily formulaNumber) formulaNumber)
            properties = complexity.toProperties
            complexity = Some complexity
        }

    /// Import a refractiveindex.info YAML page (§D.9). Supports the `tabulated n`/
    /// `tabulated nk` layout and the analytic coefficient blocks `formula 1`–
    /// `formula 7` (Sellmeier, Sellmeier-2, polynomial, the RII formula, Cauchy,
    /// gases, Herzberger); λ is taken in µm (the refractiveindex.info convention)
    /// and reduced to meters through the sole `Units` seam. Each formula is
    /// lowered to an `EpsAxisDispersion` carried in an `EpsWithDispValue`-backed
    /// `MaterialComplexity`, so formula imports are EDITABLE (spec 0033 step
    /// 025); `formula 8`/`formula 9` (and any unknown number) return the typed
    /// `UnsupportedFormula` error.
    let importRefractiveIndexInfo (yamlText : string) : Result<MaterialEntry, ImportError> =
        try
            let lines = yamlText.Replace("\r\n", "\n").Split('\n')
            match tryFormulaNumber lines with
            | Some formulaNumber ->
                match unsupportedReason formulaNumber with
                | Some reason -> Error (UnsupportedFormula (formulaNumber, reason))
                | None ->
                    match lines |> Array.tryFind (fun l -> l.TrimStart().StartsWith "coefficients:") with
                    | Some cl ->
                        let nums = parseFloats (cl.Substring(cl.IndexOf(':') + 1))
                        if nums.Length < 1 then Error (NoData (sprintf "formula %d: no coefficients" formulaNumber))
                        else
                            lowerFormulaAxis formulaNumber nums
                            |> Result.map (entryOfAxis formulaNumber (rangeInterval lines))
                    | None -> Error (MalformedYaml (sprintf "formula %d block has no coefficients line" formulaNumber))
            | None ->
                let rows = tabulatedRows Micrometer lines
                if rows.Length = 0 then Error (NoData "no tabulated rows found")
                else Ok (entryFromTabulated "Imported (refractiveindex.info)" Semiconductor rows)
        with e -> Error (MalformedYaml e.Message)

    /// Import a refractiveindex.info-style n,k CSV (§D.9). Columns are
    /// `wavelength, n[, k]`; the wavelength column header declares the unit (nm/µm/Å),
    /// defaulting to µm. λ is reduced to the meter base via `Units.toMeters`.
    let importCsv (csvText : string) : Result<MaterialEntry, ImportError> =
        try
            let csv = CsvFile.Parse(csvText)
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
                            Some ((toMeters unit lam) / 1.0<meter>, n, k)
                        | _ -> None
                    else None)
                |> Array.ofSeq
            if rows.Length = 0 then Error (NoData "no CSV data rows found")
            else Ok (entryFromTabulated "Imported (CSV)" Glass rows)
        with e -> Error (MalformedCsv e.Message)

    /// Export n,k sampled over a `Range<WaveLength>` to CSV text (§D.9), reading the
    /// entry's dispersion through the engine `getEps` path. n is recovered as
    /// `√(ε₁₁)`. λ is written in nm via the `Units` seam (display only; the engine
    /// data stays in meters).
    let exportCsv (entry : MaterialEntry) (range : Range<WaveLength>) : string =
        let sb = StringBuilder()
        sb.AppendLine("wavelength_nm,n,k") |> ignore
        // Sample λ linearly in the canonical meter base over the range (the engine's
        // `getWaveLengthValue` re-wraps the meter magnitude as a Nm value, so we walk
        // meters directly and build the sample WaveLength via the engine `nmToMeter`).
        let s = range.startValue.value
        let e = range.endValue.value
        let n = max 1 range.numberOfPoints
        for i in 0 .. range.numberOfPoints do
            let m = s + (e - s) * (float i) / (float n)
            let w = WaveLength.Nm(m / nmToMeter)
            let nComplex = Complex.Sqrt (entry.properties.epsWithDisp.getEps w).[0, 0]
            let nm = wavelengthToUnit Nanometer w
            sb.AppendLine(String.Format(inv, "{0:G},{1:G},{2:G}", nm, nComplex.Real, nComplex.Imaginary)) |> ignore
        sb.ToString()
