namespace OpticalConstructor.Domain

open System.Numerics
open Berreman.MathNetNumericsMath
open Berreman.Fields
open Berreman.MaterialProperties
open Berreman.Dispersion
open OpticalConstructor.Domain.Units

/// Dispersion-model coefficient catalogue and evaluation (§D.5–D.7, §D.12).
/// The catalogue is DATA, not code: one F# record per analytic model named in
/// 010 §2, each holding only its coefficient scalars plus the `UnitOfMeasure`
/// (D.1, slice 002) its coefficients were tabulated in. `evaluate` reduces the
/// canonical `WaveLength` to meters via `WaveLength.value` (`Fields.fs:284`),
/// normalises against that unit through the sole `Units` seam (no literal
/// factors, §D.11), applies the analytic formula, and returns the engine's
/// `ComplexRefractionIndex` (`MaterialProperties.fs:43`). `toEpsAxis` lowers each
/// finite-term model to the serializable per-axis term data of `Dispersion.fs`
/// (spec 0033 §B), and `toOpticalProperties` composes through
/// `EpsWithDispValue.toEpsWithDisp` exactly as `Silicon`/`Langasite` build by hand
/// (`OpticalProperties/Dispersive.fs:87,52`); the transcendental cases stay
/// closure-evaluated. No parallel tensor type, no re-derived EVD/matrix-exponential
/// math.
module DispersionModels =

    /// Net-new SI temperature measure for the optional thermo-optic parameters
    /// (§D.12). The engine has no kelvin measure today; this lives here and is NOT
    /// a shadow of `Constants.fs`. Temperatures and dn/dT are stored in SI K and
    /// 1/K, so the §0 canonical-units rule admits no non-SI unit.
    [<Measure>] type K

    /// Optional first-order thermo-optic parameters (§D.12). `dndT`/`referenceTemperature`
    /// are the ONLY temperature-related values persisted (as the `materialEntry`
    /// `$def` optional thermo-optic fields, §A.7). The operating temperature `T` is
    /// supplied by the caller at the evaluation boundary and is NEVER a field here,
    /// so it cannot be persisted.
    type ThermoOptic =
        {
            dndT : float<1/K>
            referenceTemperature : float<K>
        }

    /// Sellmeier `n² = 1 + Σ bᵢ·λ²/(λ² − cᵢ)`. The `c` terms are conventionally
    /// µm², so `wavelengthUnit` is typically `Micrometer`.
    [<Softellect.Sys.NumericCarrier("the B and C coefficient lists of one Sellmeier formula, read by its evaluation kernel at every wavelength of a sweep (R3; as Dispersion.DispersionTerm, the term data it lowers to)")>]
    type SellmeierCoefficients =
        {
            b : float list
            c : float list
            wavelengthUnit : UnitOfMeasure
            thermoOptic : ThermoOptic option
        }

    /// Cauchy `n = a + b/λ² + c/λ⁴`.
    [<Softellect.Sys.NumericCarrier("the A, B and C coefficients of one Cauchy formula, read by its evaluation kernel at every wavelength of a sweep (R3; as Dispersion.DispersionTerm, the term data it lowers to)")>]
    type CauchyCoefficients =
        {
            a : float
            b : float
            c : float
            wavelengthUnit : UnitOfMeasure
            thermoOptic : ThermoOptic option
        }

    /// Drude–Lorentz oscillators `ε = εInf + Σ sⱼ/(rⱼ² − E² − i·dⱼ·E)`, with the
    /// abscissa E in the record's unit (oscillator models are conventionally in eV,
    /// so `wavelengthUnit` is typically `ElectronVolt`).
    [<Softellect.Sys.NumericCarrier("the eps-infinity and the per-oscillator strength, resonance and damping lists of one Lorentz formula, read by its evaluation kernel at every wavelength of a sweep (R3; as Dispersion.DispersionTerm, the term data it lowers to)")>]
    type LorentzCoefficients =
        {
            epsInf : float
            strength : float list
            resonance : float list
            damping : float list
            wavelengthUnit : UnitOfMeasure
            thermoOptic : ThermoOptic option
        }

    /// Drude free-carrier `ε = εInf − ωp²/(E² + i·γ·E)`.
    [<Softellect.Sys.NumericCarrier("the eps-infinity, plasma and damping frequencies of one Drude formula, read by its evaluation kernel at every wavelength of a sweep (R3; as Dispersion.DispersionTerm, the term data it lowers to)")>]
    type DrudeCoefficients =
        {
            epsInf : float
            plasmaFrequency : float
            dampingFrequency : float
            wavelengthUnit : UnitOfMeasure
            thermoOptic : ThermoOptic option
        }

    /// Tauc–Lorentz: `ε₂(E)` above the band gap, `ε₁ = εInf` (the full
    /// Kramers–Kronig `ε₁` integral is out of scope — minimum implementation, §0).
    [<Softellect.Sys.NumericCarrier("the five coefficients of one Tauc-Lorentz formula, read by its evaluation kernel at every wavelength of a sweep (R3; as Dispersion.DispersionTerm)")>]
    type TaucLorentzCoefficients =
        {
            epsInf : float
            amplitude : float
            resonance : float
            broadening : float
            bandGap : float
            wavelengthUnit : UnitOfMeasure
            thermoOptic : ThermoOptic option
        }

    /// Gaussian oscillator: `ε₂(E) = A·exp(−((E − E₀)/σ)²)`, `ε₁ = εInf`.
    [<Softellect.Sys.NumericCarrier("the four coefficients of one Gaussian-oscillator formula, read by its evaluation kernel at every wavelength of a sweep (R3; as Dispersion.DispersionTerm)")>]
    type GaussianOscillatorCoefficients =
        {
            epsInf : float
            amplitude : float
            energy : float
            broadening : float
            wavelengthUnit : UnitOfMeasure
            thermoOptic : ThermoOptic option
        }

    /// Forouhi–Bloomer five-parameter amorphous form (Forouhi & Bloomer,
    /// Phys. Rev. B 34, 7018 (1986)): `k(E) = a·(E − bandGap)²/(E² − b·E + c)`
    /// above the band gap and zero below it (the Θ ∝ (E − Eg)² density-of-states
    /// factor), with the Kramers–Kronig closed form
    /// `n(E) = nInf + (B₀·E + C₀)/(E² − b·E + c)` where `Q = √(4c − b²)/2`,
    /// `B₀ = (a/Q)·(−b²/2 + bandGap·b − bandGap² + c)` and
    /// `C₀ = (a/Q)·((bandGap² + c)·b/2 − 2·bandGap·c)`. The abscissa E is photon
    /// energy, so `wavelengthUnit` is conventionally `ElectronVolt` (a, b in eV;
    /// c in eV²).
    [<Softellect.Sys.NumericCarrier("the five coefficients of one Forouhi-Bloomer formula, read by its evaluation kernel at every wavelength of a sweep (R3; as Dispersion.DispersionTerm)")>]
    type ForouhiBloomerCoefficients =
        {
            nInf : float
            a : float
            b : float
            c : float
            bandGap : float
            wavelengthUnit : UnitOfMeasure
            thermoOptic : ThermoOptic option
        }

    /// Brendel–Bormann Gaussian-broadened (Voigt) oscillator model (Rakić,
    /// Djurišić, Elazar & Majewski, Appl. Opt. 37, 5271 (1998)):
    /// `ε(E) = 1 − f₀·ωp²/(E² + i·Γ₀·E) + Σⱼ χⱼ(E)` with
    /// `χⱼ(E) = i·√π·fⱼ·ωp²/(2·√2·αⱼ·σⱼ)·[w((αⱼ−ωⱼ)/(√2·σⱼ)) + w((αⱼ+ωⱼ)/(√2·σⱼ))]`,
    /// `αⱼ = √(E² + i·Γⱼ·E)` (principal root) and `w` the Faddeeva function —
    /// signs in this codebase's Im ε ≥ 0 (k ≥ 0) convention, like Lorentz/Drude.
    /// The four per-oscillator lists (strength fⱼ, resonance ωⱼ, damping Γⱼ,
    /// broadening σⱼ) run in step; the published model carries no ε∞ — the
    /// constant term is 1. Oscillator models are conventionally in eV.
    [<Softellect.Sys.NumericCarrier("the intraband coefficients and the per-oscillator lists of one Brendel-Bormann formula, read by its evaluation kernel (the Faddeeva series) at every wavelength of a sweep (R3; as Dispersion.DispersionTerm)")>]
    type BrendelBormannCoefficients =
        {
            plasmaFrequency : float
            intrabandStrength : float
            intrabandDamping : float
            strength : float list
            resonance : float list
            damping : float list
            broadening : float list
            wavelengthUnit : UnitOfMeasure
            thermoOptic : ThermoOptic option
        }

    /// Constant complex index `n + ik` (no dispersion). Emitted as `EpsWithoutDisp`
    /// by `toOpticalProperties` so a non-dispersive entry incurs no closure overhead.
    [<Softellect.Sys.NumericCarrier("the n and k of one constant complex index, the numbers its evaluation returns at every wavelength of a sweep (R3; as Dispersion.DispersionTerm, the term data it lowers to)")>]
    type ConstantNKCoefficients =
        {
            n : float
            k : float
            wavelengthUnit : UnitOfMeasure
            thermoOptic : ThermoOptic option
        }

    /// The wrapping DU enumerating the coefficient records so the editor and schema
    /// can describe a single tagged choice (§D.5). This is the realisation of the
    /// schema `$def` `materialEntry` dispersion sub-object (§A.7). `SumOfTerms` is
    /// the raw escape hatch (spec 0033 §B): a per-axis dispersion already expressed
    /// in the serializable term data of `Dispersion.fs:306` — the identity under
    /// `toEpsAxis`.
    type DispersionModel =
        | Sellmeier of SellmeierCoefficients
        | Cauchy of CauchyCoefficients
        | Lorentz of LorentzCoefficients
        | Drude of DrudeCoefficients
        | TaucLorentz of TaucLorentzCoefficients
        | GaussianOscillator of GaussianOscillatorCoefficients
        | ForouhiBloomer of ForouhiBloomerCoefficients
        | BrendelBormann of BrendelBormannCoefficients
        | ConstantNK of ConstantNKCoefficients
        | SumOfTerms of EpsAxisDispersion

        /// The unit the model's coefficients were tabulated in (§D.5). A raw
        /// `SumOfTerms` has no tabulation unit of its own — each of its formulas embeds
        /// its metres-per-coefficient-unit `wavelengthScale` — so it reports the
        /// canonical SI `Meter`.
        member this.wavelengthUnit : UnitOfMeasure =
            match this with
            | Sellmeier c -> c.wavelengthUnit
            | Cauchy c -> c.wavelengthUnit
            | Lorentz c -> c.wavelengthUnit
            | Drude c -> c.wavelengthUnit
            | TaucLorentz c -> c.wavelengthUnit
            | GaussianOscillator c -> c.wavelengthUnit
            | ForouhiBloomer c -> c.wavelengthUnit
            | BrendelBormann c -> c.wavelengthUnit
            | ConstantNK c -> c.wavelengthUnit
            | SumOfTerms _ -> Meter

        /// The optional thermo-optic parameters carried alongside the coefficient record
        /// (§D.12). The raw `SumOfTerms` escape hatch carries plain term data and no
        /// thermo-optic record.
        member this.thermoOptic : ThermoOptic option =
            match this with
            | Sellmeier c -> c.thermoOptic
            | Cauchy c -> c.thermoOptic
            | Lorentz c -> c.thermoOptic
            | Drude c -> c.thermoOptic
            | TaucLorentz c -> c.thermoOptic
            | GaussianOscillator c -> c.thermoOptic
            | ForouhiBloomer c -> c.thermoOptic
            | BrendelBormann c -> c.thermoOptic
            | ConstantNK c -> c.thermoOptic
            | SumOfTerms _ -> None

    /// A model parameter's stable key: the suffix of its coefficient box's automation id.
    type ModelParameterKey =
        | ModelParameterKey of string

        member this.value = let (ModelParameterKey k) = this in k

    /// A model parameter's display label.
    type ModelParameterLabel =
        | ModelParameterLabel of string

        member this.value = let (ModelParameterLabel l) = this in l

    /// The unit a dimensioned coefficient is read in (spec 0033 comment 009): a
    /// length-power symbol (e.g. `nm²`) or an energy symbol (`eV`).
    type CoefficientUnit =
        | CoefficientUnit of string

        member this.value = let (CoefficientUnit u) = this in u

    /// One coefficient's value, as the editor shows and edits it.
    type CoefficientValue =
        | CoefficientValue of float

        member this.value = let (CoefficientValue v) = this in v

    /// One editable scalar coefficient of a dispersion model (spec 0033 gap G7 —
    /// the parameter-entry surface the editor lacked). `update` rebuilds the WHOLE
    /// model with this one coefficient replaced, so the editor can dispatch it
    /// straight back as the segment's new model. `unitOpt` is the parameter's unit of
    /// measure (spec 0033 comment 009), `None` for a dimensionless parameter. A
    /// list-valued coefficient contributes ONE `ModelParameter` per element, its
    /// `key`/`label` carrying the 1-based index. A transient UI descriptor (a record of
    /// functions), never stored or serialized.
    type ModelParameter =
        {
            key : ModelParameterKey
            label : ModelParameterLabel
            unitOpt : CoefficientUnit option
            coefficient : CoefficientValue
            update : CoefficientValue -> DispersionModel
        }

        static member create (key : ModelParameterKey) (label : ModelParameterLabel) (unitOpt : CoefficientUnit option) (coefficient : CoefficientValue) (update : CoefficientValue -> DispersionModel) : ModelParameter =
            { key = key; label = label; unitOpt = unitOpt; coefficient = coefficient; update = update }

    /// The position of an entry in one of a model's lists: an oscillator's coefficient,
    /// a formula's term or a term's coefficient.
    type private DispersionEntryIndex =
        | DispersionEntryIndex of int

        member this.value = let (DispersionEntryIndex i) = this in i

        /// The 1-based number a label shows.
        member this.number : int = this.value + 1

        /// Every entry of a list with its index.
        static member indexed (xs : 'Item list) : (DispersionEntryIndex * 'Item) list =
            xs |> List.mapi (fun i x -> DispersionEntryIndex i, x)

        /// The list with this entry updated (identity when the index is out of range).
        member this.updateIn (g : 'Item -> 'Item) (xs : 'Item list) : 'Item list =
            xs |> List.mapi (fun i x -> if i = this.value then g x else x)

        /// The array with this entry updated (identity when the index is out of range).
        member this.updateInArray (g : 'Item -> 'Item) (a : 'Item array) : 'Item array =
            a |> Array.mapi (fun i x -> if i = this.value then g x else x)

    /// Which formula of a raw `SumOfTerms` axis a term scalar belongs to; its prefix
    /// starts the scalar's key and label.
    type private TermFormulaSlot =
        | RefractionSlot
        | ExtinctionSlot
        | PermittivitySlot

        member this.prefix : string =
            match this with
            | RefractionSlot -> "n"
            | ExtinctionSlot -> "k"
            | PermittivitySlot -> "eps"

    // -- the raw SumOfTerms escape hatch, editable per term (spec 0033 comment 009).
    // Each DispersionTerm is m·(Σ cₖ·(λ−λ₀)^k)^power; every scalar is exposed. λ₀ is a
    // wavelength offset (nm — the segment's reduced abscissa), the rest are raw.

    let private realFormulaParams (slot : TermFormulaSlot) (f : DispersionFormula) (rebuild : DispersionFormula -> DispersionModel) : ModelParameter list =
        let prefix = slot.prefix
        let p = ModelParameter.create
        let nanometer = Some (CoefficientUnit "nm")
        DispersionEntryIndex.indexed f.terms
        |> List.map (fun (j, t) ->
            let onTerm (g : DispersionTerm -> DispersionTerm) = rebuild { f with terms = j.updateIn g f.terms }
            [
                p (ModelParameterKey $"{prefix}t{j.value}mul") (ModelParameterLabel $"{prefix} t{j.number} mult") None (CoefficientValue t.multiplier) (fun v -> onTerm (fun t -> { t with multiplier = v.value }))
                p (ModelParameterKey $"{prefix}t{j.value}lam") (ModelParameterLabel $"{prefix} t{j.number} λ0") nanometer (CoefficientValue t.lambda) (fun v -> onTerm (fun t -> { t with lambda = v.value }))
                p (ModelParameterKey $"{prefix}t{j.value}pow") (ModelParameterLabel $"{prefix} t{j.number} power") None (CoefficientValue (float t.power)) (fun v -> onTerm (fun t -> { t with power = int (round v.value) }))
            ]
            @ (DispersionEntryIndex.indexed (Array.toList t.coefficients)
               |> List.map (fun (k, ck) ->
                   p (ModelParameterKey $"{prefix}t{j.value}c{k.value}") (ModelParameterLabel $"{prefix} t{j.number} c{k.value}") None (CoefficientValue ck) (fun v ->
                       onTerm (fun t -> { t with coefficients = k.updateInArray (fun _ -> v.value) t.coefficients })))))
        |> List.concat

    let private complexFormulaParams (slot : TermFormulaSlot) (f : ComplexDispersionFormula) (rebuild : ComplexDispersionFormula -> DispersionModel) : ModelParameter list =
        let prefix = slot.prefix
        let p = ModelParameter.create
        let nanometer = Some (CoefficientUnit "nm")
        DispersionEntryIndex.indexed f.terms
        |> List.map (fun (j, t) ->
            let onTerm (g : ComplexDispersionTerm -> ComplexDispersionTerm) = rebuild { f with terms = j.updateIn g f.terms }
            [
                p (ModelParameterKey $"{prefix}t{j.value}mulR") (ModelParameterLabel $"{prefix} t{j.number} mult re") None (CoefficientValue t.multiplier.Real) (fun v -> onTerm (fun t -> { t with multiplier = Complex.create v.value t.multiplier.Imaginary }))
                p (ModelParameterKey $"{prefix}t{j.value}mulI") (ModelParameterLabel $"{prefix} t{j.number} mult im") None (CoefficientValue t.multiplier.Imaginary) (fun v -> onTerm (fun t -> { t with multiplier = Complex.create t.multiplier.Real v.value }))
                p (ModelParameterKey $"{prefix}t{j.value}lamR") (ModelParameterLabel $"{prefix} t{j.number} λ0 re") nanometer (CoefficientValue t.lambda.Real) (fun v -> onTerm (fun t -> { t with lambda = Complex.create v.value t.lambda.Imaginary }))
                p (ModelParameterKey $"{prefix}t{j.value}pow") (ModelParameterLabel $"{prefix} t{j.number} power") None (CoefficientValue (float t.power)) (fun v -> onTerm (fun t -> { t with power = int (round v.value) }))
            ]
            @ (DispersionEntryIndex.indexed (Array.toList t.coefficients)
               |> List.map (fun (k, ck) ->
                   [
                       p (ModelParameterKey $"{prefix}t{j.value}c{k.value}R") (ModelParameterLabel $"{prefix} t{j.number} c{k.value} re") None (CoefficientValue ck.Real) (fun v ->
                           onTerm (fun t -> { t with coefficients = k.updateInArray (fun c -> Complex.create v.value c.Imaginary) t.coefficients }))
                       p (ModelParameterKey $"{prefix}t{j.value}c{k.value}I") (ModelParameterLabel $"{prefix} t{j.number} c{k.value} im") None (CoefficientValue ck.Imaginary) (fun v ->
                           onTerm (fun t -> { t with coefficients = k.updateInArray (fun c -> Complex.create c.Real v.value) t.coefficients }))
                   ])
               |> List.concat))
        |> List.concat

    type DispersionModel with

        /// The editable scalar coefficients of a model, in display order (spec 0033
        /// gap G7 / comment 009). Every analytic model exposes its full scalar
        /// parameter set (with units) and one box per oscillator-list element; the raw
        /// `SumOfTerms` escape hatch exposes every term scalar of its n/k (or complex ε)
        /// formulas, so it is fully editable too; over a tabulated n,k table or the
        /// evaluated case it exposes none.
        member this.parameters : ModelParameter list =
            let p = ModelParameter.create
            let listParams (name : string) (label : string) (unitOpt : CoefficientUnit option) (xs : float list) (rebuild : float list -> DispersionModel) : ModelParameter list =
                DispersionEntryIndex.indexed xs
                |> List.map (fun (i, x) ->
                    p (ModelParameterKey $"{name}{i.value}") (ModelParameterLabel $"{label}{i.number}") unitOpt (CoefficientValue x) (fun v -> rebuild (i.updateIn (fun _ -> v.value) xs)))
            // The model's abscissa unit and its length-power derivatives (spec 0033
            // comment 009): dimensioned parameters read in these.
            let abbreviation = this.wavelengthUnit.abbreviation
            let un = Some (CoefficientUnit abbreviation)
            let un2 = Some (CoefficientUnit $"{abbreviation}²")
            let un4 = Some (CoefficientUnit $"{abbreviation}⁴")
            match this with
            | ConstantNK c ->
                [
                    p (ModelParameterKey "n") (ModelParameterLabel "n") None (CoefficientValue c.n) (fun v -> ConstantNK { c with n = v.value })
                    p (ModelParameterKey "k") (ModelParameterLabel "k") None (CoefficientValue c.k) (fun v -> ConstantNK { c with k = v.value })
                ]
            | Cauchy c ->
                [
                    p (ModelParameterKey "a") (ModelParameterLabel "A") None (CoefficientValue c.a) (fun v -> Cauchy { c with a = v.value })
                    p (ModelParameterKey "b") (ModelParameterLabel "B") un2 (CoefficientValue c.b) (fun v -> Cauchy { c with b = v.value })
                    p (ModelParameterKey "c") (ModelParameterLabel "C") un4 (CoefficientValue c.c) (fun v -> Cauchy { c with c = v.value })
                ]
            | Sellmeier c ->
                listParams "b" "B" None c.b (fun xs -> Sellmeier { c with b = xs })
                @ listParams "c" "C" un2 c.c (fun xs -> Sellmeier { c with c = xs })
            | Lorentz c ->
                [ p (ModelParameterKey "epsInf") (ModelParameterLabel "eInf") None (CoefficientValue c.epsInf) (fun v -> Lorentz { c with epsInf = v.value }) ]
                @ listParams "s" "s" None c.strength (fun xs -> Lorentz { c with strength = xs })
                @ listParams "r" "E" un c.resonance (fun xs -> Lorentz { c with resonance = xs })
                @ listParams "d" "G" un c.damping (fun xs -> Lorentz { c with damping = xs })
            | Drude c ->
                [
                    p (ModelParameterKey "epsInf") (ModelParameterLabel "eInf") None (CoefficientValue c.epsInf) (fun v -> Drude { c with epsInf = v.value })
                    p (ModelParameterKey "wp") (ModelParameterLabel "wp") un (CoefficientValue c.plasmaFrequency) (fun v -> Drude { c with plasmaFrequency = v.value })
                    p (ModelParameterKey "gamma") (ModelParameterLabel "gamma") un (CoefficientValue c.dampingFrequency) (fun v -> Drude { c with dampingFrequency = v.value })
                ]
            | TaucLorentz c ->
                [
                    p (ModelParameterKey "epsInf") (ModelParameterLabel "eInf") None (CoefficientValue c.epsInf) (fun v -> TaucLorentz { c with epsInf = v.value })
                    p (ModelParameterKey "amp") (ModelParameterLabel "A") un (CoefficientValue c.amplitude) (fun v -> TaucLorentz { c with amplitude = v.value })
                    p (ModelParameterKey "res") (ModelParameterLabel "E0") un (CoefficientValue c.resonance) (fun v -> TaucLorentz { c with resonance = v.value })
                    p (ModelParameterKey "br") (ModelParameterLabel "C") un (CoefficientValue c.broadening) (fun v -> TaucLorentz { c with broadening = v.value })
                    p (ModelParameterKey "eg") (ModelParameterLabel "Eg") un (CoefficientValue c.bandGap) (fun v -> TaucLorentz { c with bandGap = v.value })
                ]
            | GaussianOscillator c ->
                [
                    p (ModelParameterKey "epsInf") (ModelParameterLabel "eInf") None (CoefficientValue c.epsInf) (fun v -> GaussianOscillator { c with epsInf = v.value })
                    p (ModelParameterKey "amp") (ModelParameterLabel "A") None (CoefficientValue c.amplitude) (fun v -> GaussianOscillator { c with amplitude = v.value })
                    p (ModelParameterKey "en") (ModelParameterLabel "E0") un (CoefficientValue c.energy) (fun v -> GaussianOscillator { c with energy = v.value })
                    p (ModelParameterKey "br") (ModelParameterLabel "sigma") un (CoefficientValue c.broadening) (fun v -> GaussianOscillator { c with broadening = v.value })
                ]
            | ForouhiBloomer c ->
                [
                    p (ModelParameterKey "nInf") (ModelParameterLabel "nInf") None (CoefficientValue c.nInf) (fun v -> ForouhiBloomer { c with nInf = v.value })
                    p (ModelParameterKey "a") (ModelParameterLabel "A") None (CoefficientValue c.a) (fun v -> ForouhiBloomer { c with a = v.value })
                    p (ModelParameterKey "b") (ModelParameterLabel "B") un (CoefficientValue c.b) (fun v -> ForouhiBloomer { c with b = v.value })
                    p (ModelParameterKey "c") (ModelParameterLabel "C") un2 (CoefficientValue c.c) (fun v -> ForouhiBloomer { c with c = v.value })
                    p (ModelParameterKey "eg") (ModelParameterLabel "Eg") un (CoefficientValue c.bandGap) (fun v -> ForouhiBloomer { c with bandGap = v.value })
                ]
            | BrendelBormann c ->
                [
                    p (ModelParameterKey "wp") (ModelParameterLabel "wp") un (CoefficientValue c.plasmaFrequency) (fun v -> BrendelBormann { c with plasmaFrequency = v.value })
                    p (ModelParameterKey "f0") (ModelParameterLabel "f0") None (CoefficientValue c.intrabandStrength) (fun v -> BrendelBormann { c with intrabandStrength = v.value })
                    p (ModelParameterKey "g0") (ModelParameterLabel "G0") un (CoefficientValue c.intrabandDamping) (fun v -> BrendelBormann { c with intrabandDamping = v.value })
                ]
                @ listParams "f" "f" None c.strength (fun xs -> BrendelBormann { c with strength = xs })
                @ listParams "w" "w" un c.resonance (fun xs -> BrendelBormann { c with resonance = xs })
                @ listParams "g" "G" un c.damping (fun xs -> BrendelBormann { c with damping = xs })
                @ listParams "sig" "sigma" un c.broadening (fun xs -> BrendelBormann { c with broadening = xs })
            | SumOfTerms axis ->
                match axis with
                | RealNK nk ->
                    realFormulaParams RefractionSlot nk.nFormula (fun f -> SumOfTerms (RealNK { nk with nFormula = f }))
                    @ realFormulaParams ExtinctionSlot nk.kFormula (fun f -> SumOfTerms (RealNK { nk with kFormula = f }))
                | ComplexEps cF ->
                    complexFormulaParams PermittivitySlot cF (fun f -> SumOfTerms (ComplexEps f))
                // A tabulated n,k table (spec 0050 step 2) and the evaluated case (a
                // transcendental model lowered to its closure) carry no finite term data,
                // so they expose no editable term scalars.
                | TabulatedNK _
                | EpsAxisEvaluated _ -> []

    /// Term count of the Faddeeva rational series below. Weideman shows N = 24
    /// already reaches near-machine accuracy over the closed upper half plane.
    [<Literal>]
    let private faddeevaTermCount = 24

    /// Weideman's rational series for the Faddeeva function (SIAM J. Numer. Anal.
    /// 31, 1497 (1994)): the optimal scale L = √(N/√2) of its Möbius map
    /// (L + iz)/(L − iz), and its coefficients a₁..a_N.
    [<Softellect.Sys.NumericCarrier("the scale and the coefficients of Weideman's rational series, the numbers the Faddeeva kernel computes with for every Brendel-Bormann oscillator at every wavelength of a sweep (R3)")>]
    type private FaddeevaSeries =
        {
            scale : double
            coefficients : double array
        }

    /// The coefficients are the cosine-transform samples
    /// aₙ = (1/2M)·Σₖ f(θₖ)·cos(n·θₖ) of the even function
    /// f(θ) = e^(−t²)·(L² + t²) under t = L·tan(θ/2), θₖ = kπ/M,
    /// k = −M+1 .. M−1, M = 2N — computed once from the definition, not a
    /// hand-copied table (f vanishes at θ = ±π, so those endpoints drop out).
    let private faddeevaSeries : FaddeevaSeries =
        let l = sqrt (float faddeevaTermCount / sqrt 2.0)
        let m = 2 * faddeevaTermCount
        let samples =
            [| for k in -m + 1 .. m - 1 ->
                let theta = float k * System.Math.PI / float m
                let t = l * tan (0.5 * theta)
                theta, exp (-(t * t)) * (l * l + t * t) |]
        {
            scale = l
            coefficients =
                [| for j in 1 .. faddeevaTermCount ->
                    (samples |> Array.sumBy (fun (theta, f) -> f * cos (float j * theta))) / float (2 * m) |]
        }

    /// The Faddeeva function w(z) = e^(−z²)·erfc(−i·z) for Im z ≥ 0 — the kernel
    /// of the Brendel–Bormann Gaussian-broadened oscillator. Weideman's rational
    /// series: with Z = (L + iz)/(L − iz),
    /// w(z) = 2·(Σₙ aₙ·Zⁿ⁻¹)/(L − iz)² + (1/√π)/(L − iz). Every argument built in
    /// `baseIndex` has Im ≥ 0 (α is the principal root of E² + i·Γ·E), so the
    /// half-plane restriction is safe by construction.
    let private faddeeva (z : Complex) : Complex =
        let l = Complex.create faddeevaSeries.scale 0.0
        let iz = Complex.create 0.0 1.0 * z
        let ratio = (l + iz) / (l - iz)
        let p = Array.foldBack (fun a acc -> acc * ratio + Complex.create a 0.0) faddeevaSeries.coefficients Complex.Zero
        Complex.create 2.0 0.0 * p / ((l - iz) * (l - iz)) + Complex.create (1.0 / sqrt System.Math.PI) 0.0 / (l - iz)

    /// The isothermal complex index at the reference temperature: the analytic
    /// formula evaluated against the model's abscissa (λ for Sellmeier/Cauchy in
    /// `wavelengthUnit`; photon energy E for the oscillator models). The abscissa
    /// is obtained from the canonical meters via `Units.fromMeters` (the sole
    /// conversion seam, §D.11) — no literal factor appears here.
    let private baseIndex (model : DispersionModel) (w : WaveLength) : Complex =
        let x = (fromMeters model.wavelengthUnit w.value).value
        match model with
        | Sellmeier c ->
            let lam2 = x * x
            let s = List.map2 (fun b ci -> b * lam2 / (lam2 - ci)) c.b c.c |> List.sum
            Complex.create (sqrt (1.0 + s)) 0.0
        | Cauchy c ->
            let n = c.a + c.b / (x * x) + c.c / (x * x * x * x)
            Complex.create n 0.0
        | Lorentz c ->
            let eps =
                List.map3 (fun s r d -> Complex(s, 0.0) / Complex(r * r - x * x, -d * x)) c.strength c.resonance c.damping
                |> List.fold (+) (Complex(c.epsInf, 0.0))
            Complex.Sqrt eps
        | Drude c ->
            let eps = Complex(c.epsInf, 0.0) - Complex(c.plasmaFrequency * c.plasmaFrequency, 0.0) / Complex(x * x, c.dampingFrequency * x)
            Complex.Sqrt eps
        | TaucLorentz c ->
            let eps2 =
                if x > c.bandGap then
                    (c.amplitude * c.resonance * c.broadening * (x - c.bandGap) ** 2.0)
                    / (x * ((x * x - c.resonance * c.resonance) ** 2.0 + c.broadening * c.broadening * x * x))
                else 0.0
            Complex.Sqrt (Complex(c.epsInf, eps2))
        | GaussianOscillator c ->
            let d = (x - c.energy) / c.broadening
            Complex.Sqrt (Complex(c.epsInf, c.amplitude * exp (-(d * d))))
        | ForouhiBloomer c ->
            let q = 0.5 * sqrt (4.0 * c.c - c.b * c.b)
            let b0 = c.a / q * (-0.5 * c.b * c.b + c.bandGap * c.b - c.bandGap * c.bandGap + c.c)
            let c0 = c.a / q * (0.5 * c.b * (c.bandGap * c.bandGap + c.c) - 2.0 * c.bandGap * c.c)
            let denominator = x * x - c.b * x + c.c
            let n = c.nInf + (b0 * x + c0) / denominator
            let k = if x > c.bandGap then c.a * (x - c.bandGap) * (x - c.bandGap) / denominator else 0.0
            Complex.create n k
        | BrendelBormann c ->
            let wp2 = c.plasmaFrequency * c.plasmaFrequency
            let intraband = Complex.create (c.intrabandStrength * wp2) 0.0 / Complex(x * x, c.intrabandDamping * x)
            let oscillator (f : double) (r : double) (d : double) (s : double) : Complex =
                let alpha = Complex.Sqrt (Complex(x * x, d * x))
                let width = Complex.create (sqrt 2.0 * s) 0.0
                let za = (alpha - Complex.create r 0.0) / width
                let zb = (alpha + Complex.create r 0.0) / width
                Complex.create 0.0 (sqrt System.Math.PI * f * wp2) / (Complex.create 2.0 0.0 * alpha * width) * (faddeeva za + faddeeva zb)
            let interband =
                List.map2 (fun (f, r, d) s -> oscillator f r d s) (List.zip3 c.strength c.resonance c.damping) c.broadening
                |> List.fold (+) Complex.Zero
            Complex.Sqrt (Complex.One - intraband + interband)
        | ConstantNK c ->
            Complex.create c.n c.k
        | SumOfTerms axis ->
            // Raw term data evaluates through its own complexIndex; each formula
            // carries its own wavelengthScale, so the reduced x above is unused.
            let (ComplexRefractionIndex n) = axis.complexIndex w
            n

    /// Evaluate the model at the operating temperature `T` (§D.12). With
    /// `thermoOptic = None` this is the base (isothermal) closure with NO overhead;
    /// with `Some`, the closure adds the first-order correction
    /// `Δn = dndT·(T − referenceTemperature)` to the real index. `T` enters ONLY
    /// here, at the evaluation boundary — it is never stored.
    let evaluateAt (t : float<K>) (model : DispersionModel) : WaveLength -> ComplexRefractionIndex =
        match model.thermoOptic with
        | None -> fun w -> baseIndex model w |> ComplexRefractionIndex
        | Some th ->
            fun w ->
                let n0 = baseIndex model w
                let dn = th.dndT * (t - th.referenceTemperature)
                Complex.create (n0.Real + float dn) n0.Imaginary |> ComplexRefractionIndex

    type DispersionModel with

        /// The §D.6 evaluation closure `WaveLength -> ComplexRefractionIndex` of exactly
        /// the shape `Silicon`/`Langasite` build by hand. With `thermoOptic = None` it is
        /// byte-identical to the base isothermal index (no `+0.0` overhead); with `Some`
        /// it is taken at the reference temperature (Δn = 0), so the persisted entry is
        /// isothermal until a caller asks for a different `T` via `evaluateAt`.
        member this.evaluate : WaveLength -> ComplexRefractionIndex =
            match this.thermoOptic with
            | None -> fun w -> baseIndex this w |> ComplexRefractionIndex
            | Some th -> evaluateAt th.referenceTemperature this

    // ==========================================================================
    // Lowering to the serializable eps tree (spec 0033 Part B / §6.1).
    // toEpsAxis turns each finite-term analytic model into the per-axis term
    // DATA of `Dispersion.fs` (`EpsAxisDispersion`), so catalogue materials
    // become serializable and editable; `evaluate` stays the closure route. The
    // AC-B5 grid tests in OpticalConstructor.Tests pin the two encodings of each
    // formula together.
    // ==========================================================================

    type AbscissaKind with

        /// Classify a tabulation unit against the formula variable. The scale and the
        /// reciprocal numerator are derived THROUGH the `Units` seam (§D.11):
        /// scale = `toMeters u 1.0` for length units; the reciprocal units use
        /// x = λ[nm] and numerator = `fromMeters u (toMeters Nanometer 1.0)`
        /// (`evNmProduct` for eV, the cm⁻¹ relation for `Wavenumber`) — no literal
        /// factor appears here.
        static member ofUnit (u : UnitOfMeasure) : AbscissaKind =
            match u with
            | Meter | Millimeter | Micrometer | Nanometer | Angstrom ->
                LinearInX (toMeters u (UnitMagnitude 1.0))
            | ElectronVolt | Wavenumber ->
                let nanometer = toMeters Nanometer (UnitMagnitude 1.0)
                ReciprocalInX { wavelengthScale = nanometer; numerator = (fromMeters u nanometer).value }

    /// The zero formula — the k of a transparent medium.
    let private zeroFormula (scale : float<Berreman.Constants.meter>) : DispersionFormula =
        { terms = []; wavelengthScale = scale }

    type ComplexDispersionTerm with

        /// A complex constant term c·x⁰.
        static member private constant (c : Complex) : ComplexDispersionTerm =
            { lambda = Complex.Zero; coefficients = [| c |]; power = 1; multiplier = Complex.One }

        /// A complex polynomial term m·(c₀ + c₁·x + …).
        static member private polynomial (m : Complex) (coefficients : Complex array) : ComplexDispersionTerm =
            { lambda = Complex.Zero; coefficients = coefficients; power = 1; multiplier = m }

        /// A complex inverse term m·(c₀ + c₁·x + …)⁻¹.
        static member private inverse (m : Complex) (coefficients : Complex array) : ComplexDispersionTerm =
            { lambda = Complex.Zero; coefficients = coefficients; power = -1; multiplier = m }

        /// A complex inverse-square term m·(c₀ + c₁·x + …)⁻².
        static member private inverseSquare (m : Complex) (coefficients : Complex array) : ComplexDispersionTerm =
            { lambda = Complex.Zero; coefficients = coefficients; power = -2; multiplier = m }

    /// Exact terms for s·x²/(c₀ + c₁·x + c₂·x²) — the shape every
    /// reciprocal-abscissa oscillator reduces to (abscissa = k/x multiplies the
    /// oscillator denominator by x²). A quadratic denominator splits at its two
    /// complex roots (simple-pole residues s·xᵢ²/q′(xᵢ)), or at one double pole
    /// when the discriminant vanishes; a linear denominator long-divides; a
    /// constant denominator leaves a plain polynomial. The discriminant check is
    /// exact — a merely near-degenerate root pair stays two simple poles.
    let private rationalXSquaredTerms (s : Complex) (c0 : Complex) (c1 : Complex) (c2 : Complex) : ComplexDispersionTerm list =
        let two = Complex (2.0, 0.0)
        let four = Complex (4.0, 0.0)
        if c2 <> Complex.Zero then
            let leading = ComplexDispersionTerm.constant (s / c2)
            let disc = c1 * c1 - four * c2 * c0
            if disc = Complex.Zero then
                // q = c₂·(x − x₀)²: x² = (x − x₀)² + 2·x₀·(x − x₀) + x₀².
                let x0 = -c1 / (two * c2)
                [
                    leading
                    ComplexDispersionTerm.inverse (two * x0 * s / c2) [| -x0; Complex.One |]
                    ComplexDispersionTerm.inverseSquare (x0 * x0 * s / c2) [| -x0; Complex.One |]
                ]
            else
                let root = sqrt disc
                let x1 = (-c1 + root) / (two * c2)
                let x2 = (-c1 - root) / (two * c2)
                let residueAt (x : Complex) : Complex = s * x * x / (c1 + two * c2 * x)
                [
                    leading
                    ComplexDispersionTerm.inverse (residueAt x1) [| -x1; Complex.One |]
                    ComplexDispersionTerm.inverse (residueAt x2) [| -x2; Complex.One |]
                ]
        elif c1 <> Complex.Zero then
            // s·x²/(c₀ + c₁·x) = (s/c₁)·(x + p) + (s·p²/c₁)/(x − p) with p = −c₀/c₁.
            let p = -c0 / c1
            [
                ComplexDispersionTerm.polynomial (s / c1) [| p; Complex.One |]
                ComplexDispersionTerm.inverse (s / c1 * p * p) [| -p; Complex.One |]
            ]
        else
            [ ComplexDispersionTerm.polynomial (s / c0) [| Complex.Zero; Complex.Zero; Complex.One |] ]

    type DispersionModel with

        /// Lower a model to the per-axis serializable dispersion (§6.1): Cauchy to
        /// `RealNK` Laurent terms (k = the zero formula); Sellmeier — via the
        /// oscillator identity Bᵢ·a²/(a²−cᵢ) = Bᵢ + Bᵢ·cᵢ/(a²−cᵢ) — to real-valued ε
        /// terms carried as `ComplexEps`, because a `RealNK` n-formula is a finite
        /// term sum and cannot take the square root, while `ComplexEps.complexIndex`
        /// evaluates exactly the required √(1 + Σ); ConstantNK to `RealNK` constants;
        /// Lorentz and Drude to `ComplexEps` inverse terms (complex is genuine — the
        /// damping); `SumOfTerms` to itself. Reciprocal-abscissa tabulations (eV,
        /// cm⁻¹ — the oscillator convention) substitute abscissa = k/x and stay exact
        /// through `rationalXSquaredTerms`. TaucLorentz / GaussianOscillator /
        /// ForouhiBloomer / BrendelBormann are NOT finite term sums (band-gap steps,
        /// exp, the Faddeeva function), so they lower to the evaluated case
        /// `EpsAxisEvaluated` carrying `evaluate model` — the same closure route the
        /// engine's own `EpsWithDisp` case takes — which makes the lowering TOTAL (all
        /// ten models lower). The lowering is isothermal: like `evaluate`, a `Some`
        /// thermo-optic model is taken at its reference temperature (Δn = 0); the
        /// operating temperature enters only at the `evaluateAt` boundary (§D.12).
        member this.toEpsAxis : EpsAxisDispersion =
            // A real constant term c·x⁰.
            let realConstant (c : double) : DispersionTerm =
                { lambda = 0.0; coefficients = [| c |]; power = 1; multiplier = 1.0 }
            // A real inverse-power term m·x^(−n): the monomial xⁿ raised to power −1.
            let realInversePower (m : double) (n : int) : DispersionTerm =
                { lambda = 0.0; coefficients = Array.init (n + 1) (fun i -> if i = n then 1.0 else 0.0); power = -1; multiplier = m }
            match this with
            | SumOfTerms axis -> axis
            | ConstantNK c ->
                let scale =
                    match AbscissaKind.ofUnit c.wavelengthUnit with
                    | LinearInX scale -> scale
                    | ReciprocalInX reciprocal -> reciprocal.wavelengthScale
                RealNK
                    {
                        nFormula = { terms = [ realConstant c.n ]; wavelengthScale = scale }
                        kFormula = { terms = [ realConstant c.k ]; wavelengthScale = scale }
                    }
            | Cauchy c ->
                match AbscissaKind.ofUnit c.wavelengthUnit with
                | LinearInX scale ->
                    // n(x) = A + B·x⁻² + C·x⁻⁴ — a real Laurent polynomial.
                    RealNK
                        {
                            nFormula = { terms = [ realConstant c.a; realInversePower c.b 2; realInversePower c.c 4 ]; wavelengthScale = scale }
                            kFormula = zeroFormula scale
                        }
                | ReciprocalInX reciprocal ->
                    // abscissa = k/x: n = A + (B/k²)·x² + (C/k⁴)·x⁴ — a plain polynomial.
                    let k = reciprocal.numerator
                    RealNK
                        {
                            nFormula = { terms = [ { lambda = 0.0; coefficients = [| c.a; 0.0; c.b / (k * k); 0.0; c.c / (k * k * k * k) |]; power = 1; multiplier = 1.0 } ]; wavelengthScale = reciprocal.wavelengthScale }
                            kFormula = zeroFormula reciprocal.wavelengthScale
                        }
            | Sellmeier c ->
                match AbscissaKind.ofUnit c.wavelengthUnit with
                | LinearInX scale ->
                    // Bᵢ·x²/(x²−cᵢ) = Bᵢ + Bᵢ·cᵢ/(x²−cᵢ): ε = (1 + ΣBᵢ) + Σ inverse terms.
                    let constant = ComplexDispersionTerm.constant (Complex (1.0 + List.sum c.b, 0.0))
                    let oscillator (b : double) (ci : double) : ComplexDispersionTerm =
                        ComplexDispersionTerm.inverse (Complex (b * ci, 0.0)) [| Complex (-ci, 0.0); Complex.Zero; Complex.One |]
                    ComplexEps { terms = constant :: List.map2 oscillator c.b c.c; wavelengthScale = scale }
                | ReciprocalInX reciprocal ->
                    // abscissa = k/x: Bᵢ·a²/(a²−cᵢ) = Bᵢ·k²/(k² − cᵢ·x²) — one inverse term each.
                    let k = reciprocal.numerator
                    let oscillator (b : double) (ci : double) : ComplexDispersionTerm =
                        ComplexDispersionTerm.inverse (Complex (b * k * k, 0.0)) [| Complex (k * k, 0.0); Complex.Zero; Complex (-ci, 0.0) |]
                    ComplexEps { terms = ComplexDispersionTerm.constant Complex.One :: List.map2 oscillator c.b c.c; wavelengthScale = reciprocal.wavelengthScale }
            | Lorentz c ->
                let constant = ComplexDispersionTerm.constant (Complex (c.epsInf, 0.0))
                match AbscissaKind.ofUnit c.wavelengthUnit with
                | LinearInX scale ->
                    // ε = ε∞ + Σ sⱼ/(rⱼ² − x² − i·dⱼ·x): one literal inverse term per oscillator.
                    let oscillator (s : double) (r : double) (d : double) : ComplexDispersionTerm =
                        ComplexDispersionTerm.inverse (Complex (s, 0.0)) [| Complex (r * r, 0.0); Complex (0.0, -d); Complex (-1.0, 0.0) |]
                    ComplexEps { terms = constant :: List.map3 oscillator c.strength c.resonance c.damping; wavelengthScale = scale }
                | ReciprocalInX reciprocal ->
                    // abscissa = k/x: sⱼ/(rⱼ² − a² − i·dⱼ·a) = sⱼ·x²/(rⱼ²·x² − i·dⱼ·k·x − k²).
                    let k = reciprocal.numerator
                    let oscillator (s : double) (r : double) (d : double) : ComplexDispersionTerm list =
                        rationalXSquaredTerms (Complex (s, 0.0)) (Complex (-k * k, 0.0)) (Complex (0.0, -d * k)) (Complex (r * r, 0.0))
                    ComplexEps { terms = constant :: (List.map3 oscillator c.strength c.resonance c.damping |> List.concat); wavelengthScale = reciprocal.wavelengthScale }
            | Drude c ->
                let constant = ComplexDispersionTerm.constant (Complex (c.epsInf, 0.0))
                let negPlasmaSq = Complex (-c.plasmaFrequency * c.plasmaFrequency, 0.0)
                match AbscissaKind.ofUnit c.wavelengthUnit with
                | LinearInX scale ->
                    // ε = ε∞ − ωp²/(x² + i·γ·x).
                    let free = ComplexDispersionTerm.inverse negPlasmaSq [| Complex.Zero; Complex (0.0, c.dampingFrequency); Complex.One |]
                    ComplexEps { terms = [ constant; free ]; wavelengthScale = scale }
                | ReciprocalInX reciprocal ->
                    // abscissa = k/x: −ωp²/(a² + i·γ·a) = −ωp²·x²/(k² + i·γ·k·x).
                    let k = reciprocal.numerator
                    let free = rationalXSquaredTerms negPlasmaSq (Complex (k * k, 0.0)) (Complex (0.0, c.dampingFrequency * k)) Complex.Zero
                    ComplexEps { terms = constant :: free; wavelengthScale = reciprocal.wavelengthScale }
            // The transcendental models are not finite term sums (band-gap steps, exp,
            // the Faddeeva function), so they lower to the evaluated case: the SAME
            // `evaluate` closure the engine's `EpsWithDisp` case applies.
            | TaucLorentz _
            | GaussianOscillator _
            | ForouhiBloomer _
            | BrendelBormann _ -> EpsAxisEvaluated this.evaluate

    type EpsWithDisp with

        /// Wrap an `EpsWithDisp` into an isotropic `OpticalPropertiesWithDisp` with the
        /// `Mu.vacuum`/`Rho.vacuum` dispersive defaults — mirroring `Silicon`'s record
        /// (`OpticalProperties/Dispersive.fs:90-95`). This is the single site for the
        /// vacuum-μ/ρ convention: `toOpticalProperties`/`toAnisotropicOpticalProperties`
        /// and `MaterialImport` all build their isotropic-dispersive entries through it.
        member this.isotropicProperties : OpticalPropertiesWithDisp =
            {
                epsWithDisp = this
                muWithDisp = Mu.vacuum.dispersive
                rhoWithDisp = Rho.vacuum.dispersive
            }

    /// Nominal interval for a single-segment tree: with one segment the topmost
    /// segment extrapolates for every wavelength (`Dispersion.fs:346`), so a model
    /// lowered whole needs no real bounds.
    let private nominalInterval : WaveLengthInterval =
        { lower = toWaveLength Nanometer (UnitMagnitude 0.0); upper = toWaveLength Nanometer (UnitMagnitude 0.0) }

    type DispersionModel with

        /// The serializable eps value a model composes to (§D.6 / spec 0033 §B):
        /// `ConstantNK` short-circuits to the non-dispersive constant
        /// (`EpsWithoutDispValue`, `Dispersion.fs:379` — no closure overhead); every
        /// other model becomes a single-segment isotropic tree over its lowered
        /// dispersion. The transcendental cases lower to the evaluated segment (their
        /// `EpsAxisEvaluated` closure), so this is TOTAL — no lowering error.
        member this.toEpsValue : EpsWithDispValue =
            match this with
            | ConstantNK c ->
                Complex.create c.n c.k |> ComplexRefractionIndex |> IsotropicAbsorbing |> EpsWithoutDispValue
            | _ ->
                EpsWithDispValue (IsotropicDispersive [ { wavelengthInterval = nominalInterval; dispersion = this.toEpsAxis } ])

        /// Compose a model into the engine's `OpticalPropertiesWithDisp` (§D.6)
        /// through the serializable eps tree: `toEpsValue`, then
        /// `EpsWithDispValue.toEpsWithDisp` (`Dispersion.fs:383`), then the
        /// `isotropicProperties` vacuum-μ/ρ wrapper. `ConstantNK` still emits
        /// `EpsWithoutDisp` (no closure overhead); the transcendental cases ride the
        /// evaluated segment, whose `complexIndex` applies `evaluate` — identical to
        /// wrapping the closure directly. One route, no lowering error.
        member this.toOpticalProperties : OpticalPropertiesWithDisp =
            this.toEpsValue.toEpsWithDisp.isotropicProperties

    /// Build a uniaxial principal-axis `Eps` from (n_o, n_e) through the engine's
    /// existing three-argument constructor, mapped as `(n_o, n_e, n_o)`
    /// (`MaterialProperties.fs:87`, AC-D5). No tensor is re-typed here.
    let uniaxialEps (no : ComplexRefractionIndex) (ne : ComplexRefractionIndex) : Eps =
        Eps.fromComplexRefractionIndex (no, ne, no)

    /// Build a biaxial principal-axis `Eps` from (n_x, n_y, n_z) through the same
    /// engine constructor.
    let biaxialEps (nx : ComplexRefractionIndex) (ny : ComplexRefractionIndex) (nz : ComplexRefractionIndex) : Eps =
        Eps.fromComplexRefractionIndex (nx, ny, nz)

    type EpsWithDispValue with

        /// Compose a (possibly anisotropic) serializable eps value into the engine's
        /// `OpticalPropertiesWithDisp` (§D.7). Anisotropy lives INSIDE
        /// `EpsWithDispValue` — the uniaxial/biaxial constant cases and the per-axis
        /// dispersive segment trees (`Dispersion.fs:285,355`) supersede the removed
        /// `AnisotropicModel` — so composition is `toEpsWithDisp` through the same
        /// vacuum-μ/ρ wrapper. Crystal-axis ORIENTATION is NOT here — rotation reuses
        /// `Layer.rotate` (Part B).
        member this.toAnisotropicOpticalProperties : OpticalPropertiesWithDisp =
            this.toEpsWithDisp.isotropicProperties
