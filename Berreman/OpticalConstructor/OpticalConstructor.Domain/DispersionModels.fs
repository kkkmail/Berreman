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
    type SellmeierCoefficients =
        {
            b : float list
            c : float list
            wavelengthUnit : UnitOfMeasure
            thermoOptic : ThermoOptic option
        }

    /// Cauchy `n = a + b/λ² + c/λ⁴`.
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
    let wavelengthUnitOf (model : DispersionModel) : UnitOfMeasure =
        match model with
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
    let thermoOpticOf (model : DispersionModel) : ThermoOptic option =
        match model with
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

    /// One editable scalar coefficient of a dispersion model (spec 0033 gap G7 —
    /// the parameter-entry surface the editor lacked). `update` rebuilds the WHOLE
    /// model with this one coefficient replaced, so the editor can dispatch it
    /// straight back as the segment's new model. `unit` is the parameter's unit of
    /// measure (spec 0033 comment 009): a length-power symbol (e.g. `nm²`), an
    /// energy symbol (`eV`), or "" for a dimensionless parameter. A list-valued
    /// coefficient contributes ONE `ModelParameter` per element, its `key`/`label`
    /// carrying the 1-based index. A transient UI descriptor (a record of
    /// functions), never stored or serialized.
    type ModelParameter =
        {
            key : string
            label : string
            unit : string
            value : float
            update : float -> DispersionModel
        }

    /// Replace the i-th element of a list (identity when i is out of range).
    let private replaceAt (i : int) (v : 'a) (xs : 'a list) : 'a list =
        xs |> List.mapi (fun j x -> if j = i then v else x)

    let private mkParam (key : string) (label : string) (unit : string) (value : float) (update : float -> DispersionModel) : ModelParameter =
        { key = key; label = label; unit = unit; value = value; update = update }

    // -- the raw SumOfTerms escape hatch, editable per term (spec 0033 comment 009).
    // Each DispersionTerm is m·(Σ cₖ·(λ−λ₀)^k)^power; every scalar is exposed. λ₀ is a
    // wavelength offset (nm — the segment's reduced abscissa), the rest are raw.

    let private setTermAt (j : int) (g : DispersionTerm -> DispersionTerm) (terms : DispersionTerm list) : DispersionTerm list =
        terms |> List.mapi (fun i t -> if i = j then g t else t)

    let private setArrayAt (k : int) (v : 'a) (a : 'a array) : 'a array =
        a |> Array.mapi (fun idx old -> if idx = k then v else old)

    let private realFormulaParams (prefix : string) (f : DispersionFormula) (rebuild : DispersionFormula -> DispersionModel) : ModelParameter list =
        f.terms
        |> List.mapi (fun j t ->
            let onTerm (g : DispersionTerm -> DispersionTerm) = rebuild { f with terms = setTermAt j g f.terms }
            [
                mkParam $"{prefix}t{j}mul" $"{prefix} t{j + 1} mult" "" t.multiplier (fun v -> onTerm (fun t -> { t with multiplier = v }))
                mkParam $"{prefix}t{j}lam" $"{prefix} t{j + 1} λ0" "nm" t.lambda (fun v -> onTerm (fun t -> { t with lambda = v }))
                mkParam $"{prefix}t{j}pow" $"{prefix} t{j + 1} power" "" (float t.power) (fun v -> onTerm (fun t -> { t with power = int (round v) }))
            ]
            @ (t.coefficients
               |> Array.toList
               |> List.mapi (fun k ck ->
                   mkParam $"{prefix}t{j}c{k}" $"{prefix} t{j + 1} c{k}" "" ck (fun v -> onTerm (fun t -> { t with coefficients = setArrayAt k v t.coefficients })))))
        |> List.concat

    let private setCTermAt (j : int) (g : ComplexDispersionTerm -> ComplexDispersionTerm) (terms : ComplexDispersionTerm list) : ComplexDispersionTerm list =
        terms |> List.mapi (fun i t -> if i = j then g t else t)

    let private complexFormulaParams (prefix : string) (f : ComplexDispersionFormula) (rebuild : ComplexDispersionFormula -> DispersionModel) : ModelParameter list =
        f.terms
        |> List.mapi (fun j t ->
            let onTerm (g : ComplexDispersionTerm -> ComplexDispersionTerm) = rebuild { f with terms = setCTermAt j g f.terms }
            [
                mkParam $"{prefix}t{j}mulR" $"{prefix} t{j + 1} mult re" "" t.multiplier.Real (fun v -> onTerm (fun t -> { t with multiplier = createComplex v t.multiplier.Imaginary }))
                mkParam $"{prefix}t{j}mulI" $"{prefix} t{j + 1} mult im" "" t.multiplier.Imaginary (fun v -> onTerm (fun t -> { t with multiplier = createComplex t.multiplier.Real v }))
                mkParam $"{prefix}t{j}lamR" $"{prefix} t{j + 1} λ0 re" "nm" t.lambda.Real (fun v -> onTerm (fun t -> { t with lambda = createComplex v t.lambda.Imaginary }))
                mkParam $"{prefix}t{j}pow" $"{prefix} t{j + 1} power" "" (float t.power) (fun v -> onTerm (fun t -> { t with power = int (round v) }))
            ]
            @ (t.coefficients
               |> Array.toList
               |> List.mapi (fun k ck ->
                   [
                       mkParam $"{prefix}t{j}c{k}R" $"{prefix} t{j + 1} c{k} re" "" ck.Real (fun v -> onTerm (fun t -> { t with coefficients = setArrayAt k (createComplex v t.coefficients.[k].Imaginary) t.coefficients }))
                       mkParam $"{prefix}t{j}c{k}I" $"{prefix} t{j + 1} c{k} im" "" ck.Imaginary (fun v -> onTerm (fun t -> { t with coefficients = setArrayAt k (createComplex t.coefficients.[k].Real v) t.coefficients }))
                   ])
               |> List.concat))
        |> List.concat

    /// The editable scalar coefficients of a model, in display order (spec 0033
    /// gap G7 / comment 009). Every analytic model exposes its full scalar
    /// parameter set (with units) and one box per oscillator-list element; the raw
    /// `SumOfTerms` escape hatch exposes every term scalar of its n/k (or complex ε)
    /// formulas, so it is fully editable too.
    let modelParameters (model : DispersionModel) : ModelParameter list =
        let p (key : string) (label : string) (unit : string) (value : float) (update : float -> DispersionModel) : ModelParameter =
            mkParam key label unit value update
        let listParams (name : string) (label : string) (unit : string) (xs : float list) (rebuild : float list -> DispersionModel) : ModelParameter list =
            xs |> List.mapi (fun i x -> p $"{name}{i}" $"{label}{i + 1}" unit x (fun v -> rebuild (replaceAt i v xs)))
        // The model's abscissa unit and its length-power derivatives (spec 0033
        // comment 009): dimensioned parameters read in these.
        let un = unitAbbrev (wavelengthUnitOf model)
        let un2 = un + "²"
        let un4 = un + "⁴"
        match model with
        | ConstantNK c ->
            [
                p "n" "n" "" c.n (fun v -> ConstantNK { c with n = v })
                p "k" "k" "" c.k (fun v -> ConstantNK { c with k = v })
            ]
        | Cauchy c ->
            [
                p "a" "A" "" c.a (fun v -> Cauchy { c with a = v })
                p "b" "B" un2 c.b (fun v -> Cauchy { c with b = v })
                p "c" "C" un4 c.c (fun v -> Cauchy { c with c = v })
            ]
        | Sellmeier c ->
            listParams "b" "B" "" c.b (fun xs -> Sellmeier { c with b = xs })
            @ listParams "c" "C" un2 c.c (fun xs -> Sellmeier { c with c = xs })
        | Lorentz c ->
            [ p "epsInf" "eInf" "" c.epsInf (fun v -> Lorentz { c with epsInf = v }) ]
            @ listParams "s" "s" "" c.strength (fun xs -> Lorentz { c with strength = xs })
            @ listParams "r" "E" un c.resonance (fun xs -> Lorentz { c with resonance = xs })
            @ listParams "d" "G" un c.damping (fun xs -> Lorentz { c with damping = xs })
        | Drude c ->
            [
                p "epsInf" "eInf" "" c.epsInf (fun v -> Drude { c with epsInf = v })
                p "wp" "wp" un c.plasmaFrequency (fun v -> Drude { c with plasmaFrequency = v })
                p "gamma" "gamma" un c.dampingFrequency (fun v -> Drude { c with dampingFrequency = v })
            ]
        | TaucLorentz c ->
            [
                p "epsInf" "eInf" "" c.epsInf (fun v -> TaucLorentz { c with epsInf = v })
                p "amp" "A" un c.amplitude (fun v -> TaucLorentz { c with amplitude = v })
                p "res" "E0" un c.resonance (fun v -> TaucLorentz { c with resonance = v })
                p "br" "C" un c.broadening (fun v -> TaucLorentz { c with broadening = v })
                p "eg" "Eg" un c.bandGap (fun v -> TaucLorentz { c with bandGap = v })
            ]
        | GaussianOscillator c ->
            [
                p "epsInf" "eInf" "" c.epsInf (fun v -> GaussianOscillator { c with epsInf = v })
                p "amp" "A" "" c.amplitude (fun v -> GaussianOscillator { c with amplitude = v })
                p "en" "E0" un c.energy (fun v -> GaussianOscillator { c with energy = v })
                p "br" "sigma" un c.broadening (fun v -> GaussianOscillator { c with broadening = v })
            ]
        | ForouhiBloomer c ->
            [
                p "nInf" "nInf" "" c.nInf (fun v -> ForouhiBloomer { c with nInf = v })
                p "a" "A" "" c.a (fun v -> ForouhiBloomer { c with a = v })
                p "b" "B" un c.b (fun v -> ForouhiBloomer { c with b = v })
                p "c" "C" un2 c.c (fun v -> ForouhiBloomer { c with c = v })
                p "eg" "Eg" un c.bandGap (fun v -> ForouhiBloomer { c with bandGap = v })
            ]
        | BrendelBormann c ->
            [
                p "wp" "wp" un c.plasmaFrequency (fun v -> BrendelBormann { c with plasmaFrequency = v })
                p "f0" "f0" "" c.intrabandStrength (fun v -> BrendelBormann { c with intrabandStrength = v })
                p "g0" "G0" un c.intrabandDamping (fun v -> BrendelBormann { c with intrabandDamping = v })
            ]
            @ listParams "f" "f" "" c.strength (fun xs -> BrendelBormann { c with strength = xs })
            @ listParams "w" "w" un c.resonance (fun xs -> BrendelBormann { c with resonance = xs })
            @ listParams "g" "G" un c.damping (fun xs -> BrendelBormann { c with damping = xs })
            @ listParams "sig" "sigma" un c.broadening (fun xs -> BrendelBormann { c with broadening = xs })
        | SumOfTerms axis ->
            match axis with
            | RealNK (nF, kF) ->
                realFormulaParams "n" nF (fun f -> SumOfTerms (RealNK (f, kF)))
                @ realFormulaParams "k" kF (fun f -> SumOfTerms (RealNK (nF, f)))
            | ComplexEps cF ->
                complexFormulaParams "eps" cF (fun f -> SumOfTerms (ComplexEps f))
            // The evaluated case (a transcendental model lowered to its closure)
            // carries no finite term data, so it exposes no editable term scalars.
            | EpsAxisEvaluated _ -> []

    /// Term count of the Faddeeva rational series below. Weideman shows N = 24
    /// already reaches near-machine accuracy over the closed upper half plane.
    let private faddeevaTermCount = 24

    /// Weideman's optimal scale L = √(N/√2) for the Möbius map (L + iz)/(L − iz).
    let private faddeevaScale = sqrt (float faddeevaTermCount / sqrt 2.0)

    /// Coefficients a₁..a_N of Weideman's rational series for the Faddeeva
    /// function (SIAM J. Numer. Anal. 31, 1497 (1994)): the cosine-transform
    /// samples aₙ = (1/2M)·Σₖ f(θₖ)·cos(n·θₖ) of the even function
    /// f(θ) = e^(−t²)·(L² + t²) under t = L·tan(θ/2), θₖ = kπ/M,
    /// k = −M+1 .. M−1, M = 2N — computed once from the definition, not a
    /// hand-copied table (f vanishes at θ = ±π, so those endpoints drop out).
    let private faddeevaCoefficients : double array =
        let m = 2 * faddeevaTermCount
        let l = faddeevaScale
        let samples =
            [| for k in -m + 1 .. m - 1 ->
                let theta = float k * System.Math.PI / float m
                let t = l * tan (0.5 * theta)
                theta, exp (-(t * t)) * (l * l + t * t) |]
        [| for j in 1 .. faddeevaTermCount ->
            (samples |> Array.sumBy (fun (theta, f) -> f * cos (float j * theta))) / float (2 * m) |]

    /// The Faddeeva function w(z) = e^(−z²)·erfc(−i·z) for Im z ≥ 0 — the kernel
    /// of the Brendel–Bormann Gaussian-broadened oscillator. Weideman's rational
    /// series: with Z = (L + iz)/(L − iz),
    /// w(z) = 2·(Σₙ aₙ·Zⁿ⁻¹)/(L − iz)² + (1/√π)/(L − iz). Every argument built in
    /// `baseIndex` has Im ≥ 0 (α is the principal root of E² + i·Γ·E), so the
    /// half-plane restriction is safe by construction.
    let private faddeeva (z : Complex) : Complex =
        let l = createComplex faddeevaScale 0.0
        let iz = createComplex 0.0 1.0 * z
        let ratio = (l + iz) / (l - iz)
        let p = Array.foldBack (fun a acc -> acc * ratio + createComplex a 0.0) faddeevaCoefficients Complex.Zero
        createComplex 2.0 0.0 * p / ((l - iz) * (l - iz)) + createComplex (1.0 / sqrt System.Math.PI) 0.0 / (l - iz)

    /// The isothermal complex index at the reference temperature: the analytic
    /// formula evaluated against the model's abscissa (λ for Sellmeier/Cauchy in
    /// `wavelengthUnit`; photon energy E for the oscillator models). The abscissa
    /// is obtained from the canonical meters via `Units.fromMeters` (the sole
    /// conversion seam, §D.11) — no literal factor appears here.
    let private baseIndex (model : DispersionModel) (w : WaveLength) : Complex =
        let x = fromMeters (wavelengthUnitOf model) w.value
        match model with
        | Sellmeier c ->
            let lam2 = x * x
            let s = List.map2 (fun b ci -> b * lam2 / (lam2 - ci)) c.b c.c |> List.sum
            createComplex (sqrt (1.0 + s)) 0.0
        | Cauchy c ->
            let n = c.a + c.b / (x * x) + c.c / (x * x * x * x)
            createComplex n 0.0
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
            createComplex n k
        | BrendelBormann c ->
            let wp2 = c.plasmaFrequency * c.plasmaFrequency
            let intraband = createComplex (c.intrabandStrength * wp2) 0.0 / Complex(x * x, c.intrabandDamping * x)
            let oscillator (f : double) (r : double) (d : double) (s : double) : Complex =
                let alpha = Complex.Sqrt (Complex(x * x, d * x))
                let width = createComplex (sqrt 2.0 * s) 0.0
                let za = (alpha - createComplex r 0.0) / width
                let zb = (alpha + createComplex r 0.0) / width
                createComplex 0.0 (sqrt System.Math.PI * f * wp2) / (createComplex 2.0 0.0 * alpha * width) * (faddeeva za + faddeeva zb)
            let interband =
                List.map2 (fun (f, r, d) s -> oscillator f r d s) (List.zip3 c.strength c.resonance c.damping) c.broadening
                |> List.fold (+) Complex.Zero
            Complex.Sqrt (Complex.One - intraband + interband)
        | ConstantNK c ->
            createComplex c.n c.k
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
        match thermoOpticOf model with
        | None -> fun w -> baseIndex model w |> ComplexRefractionIndex
        | Some th ->
            fun w ->
                let n0 = baseIndex model w
                let dn = th.dndT * (t - th.referenceTemperature)
                createComplex (n0.Real + float dn) n0.Imaginary |> ComplexRefractionIndex

    /// The §D.6 evaluation closure `WaveLength -> ComplexRefractionIndex` of exactly
    /// the shape `Silicon`/`Langasite` build by hand. With `thermoOptic = None` it is
    /// byte-identical to the base isothermal index (no `+0.0` overhead); with `Some`
    /// it is taken at the reference temperature (Δn = 0), so the persisted entry is
    /// isothermal until a caller asks for a different `T` via `evaluateAt`.
    let evaluate (model : DispersionModel) : WaveLength -> ComplexRefractionIndex =
        match thermoOpticOf model with
        | None -> fun w -> baseIndex model w |> ComplexRefractionIndex
        | Some th -> evaluateAt th.referenceTemperature model

    // ==========================================================================
    // Lowering to the serializable eps tree (spec 0033 Part B / §6.1).
    // toEpsAxis turns each finite-term analytic model into the per-axis term
    // DATA of `Dispersion.fs` (`EpsAxisDispersion`), so catalogue materials
    // become serializable and editable; `evaluate` stays the closure route. The
    // AC-B5 grid tests in OpticalConstructor.Tests pin the two encodings of each
    // formula together.
    // ==========================================================================

    /// How a model's abscissa relates to the formula's reduced wavelength
    /// x = λ / wavelengthScale (`Dispersion.fs:238`): a length unit reads its
    /// abscissa as x itself; the energy-like units are reciprocal,
    /// abscissa = numerator / x with x in nanometers.
    type private AbscissaKind =
        | LinearInX of wavelengthScale : double
        | ReciprocalInX of wavelengthScale : double * numerator : double

    /// Classify a tabulation unit against the formula variable. The scale and the
    /// reciprocal numerator are derived THROUGH the `Units` seam (§D.11):
    /// scale = `toMeters u 1.0` for length units; the reciprocal units use
    /// x = λ[nm] and numerator = `fromMeters u (toMeters Nanometer 1.0)`
    /// (`evNmProduct` for eV, the cm⁻¹ relation for `Wavenumber`) — no literal
    /// factor appears here.
    let private abscissaKindOf (u : UnitOfMeasure) : AbscissaKind =
        match u with
        | Meter | Millimeter | Micrometer | Nanometer | Angstrom ->
            LinearInX (float (toMeters u 1.0))
        | ElectronVolt | Wavenumber ->
            let nanometer = toMeters Nanometer 1.0
            ReciprocalInX (float nanometer, fromMeters u nanometer)

    /// The zero formula — the k of a transparent medium.
    let private zeroFormula (scale : double) : DispersionFormula =
        { terms = []; wavelengthScale = scale }

    /// A real constant term c·x⁰.
    let private realConstTerm (c : double) : DispersionTerm =
        { lambda = 0.0; coefficients = [| c |]; power = 1; multiplier = 1.0 }

    /// A real inverse-power term m·x^(−n): the monomial xⁿ raised to power −1.
    let private realInversePowerTerm (m : double) (n : int) : DispersionTerm =
        { lambda = 0.0; coefficients = Array.init (n + 1) (fun i -> if i = n then 1.0 else 0.0); power = -1; multiplier = m }

    /// A complex constant term c·x⁰.
    let private complexConstTerm (c : Complex) : ComplexDispersionTerm =
        { lambda = Complex.Zero; coefficients = [| c |]; power = 1; multiplier = Complex.One }

    /// A complex term m·(c₀ + c₁·x + …)^power.
    let private complexPolyTerm (m : Complex) (power : int) (coefficients : Complex array) : ComplexDispersionTerm =
        { lambda = Complex.Zero; coefficients = coefficients; power = power; multiplier = m }

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
            let leading = complexConstTerm (s / c2)
            let disc = c1 * c1 - four * c2 * c0
            if disc = Complex.Zero then
                // q = c₂·(x − x₀)²: x² = (x − x₀)² + 2·x₀·(x − x₀) + x₀².
                let x0 = -c1 / (two * c2)
                [
                    leading
                    complexPolyTerm (two * x0 * s / c2) (-1) [| -x0; Complex.One |]
                    complexPolyTerm (x0 * x0 * s / c2) (-2) [| -x0; Complex.One |]
                ]
            else
                let root = sqrt disc
                let x1 = (-c1 + root) / (two * c2)
                let x2 = (-c1 - root) / (two * c2)
                let residueAt (x : Complex) : Complex = s * x * x / (c1 + two * c2 * x)
                [
                    leading
                    complexPolyTerm (residueAt x1) (-1) [| -x1; Complex.One |]
                    complexPolyTerm (residueAt x2) (-1) [| -x2; Complex.One |]
                ]
        elif c1 <> Complex.Zero then
            // s·x²/(c₀ + c₁·x) = (s/c₁)·(x + p) + (s·p²/c₁)/(x − p) with p = −c₀/c₁.
            let p = -c0 / c1
            [
                complexPolyTerm (s / c1) 1 [| p; Complex.One |]
                complexPolyTerm (s / c1 * p * p) (-1) [| -p; Complex.One |]
            ]
        else
            [ complexPolyTerm (s / c0) 1 [| Complex.Zero; Complex.Zero; Complex.One |] ]

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
    let toEpsAxis (model : DispersionModel) : EpsAxisDispersion =
        match model with
        | SumOfTerms axis -> axis
        | ConstantNK c ->
            let scale =
                match abscissaKindOf c.wavelengthUnit with
                | LinearInX scale -> scale
                | ReciprocalInX (scale, _) -> scale
            RealNK (
                { terms = [ realConstTerm c.n ]; wavelengthScale = scale },
                { terms = [ realConstTerm c.k ]; wavelengthScale = scale })
        | Cauchy c ->
            match abscissaKindOf c.wavelengthUnit with
            | LinearInX scale ->
                // n(x) = A + B·x⁻² + C·x⁻⁴ — a real Laurent polynomial.
                RealNK (
                    { terms = [ realConstTerm c.a; realInversePowerTerm c.b 2; realInversePowerTerm c.c 4 ]; wavelengthScale = scale },
                    zeroFormula scale)
            | ReciprocalInX (scale, k) ->
                // abscissa = k/x: n = A + (B/k²)·x² + (C/k⁴)·x⁴ — a plain polynomial.
                RealNK (
                    { terms = [ { lambda = 0.0; coefficients = [| c.a; 0.0; c.b / (k * k); 0.0; c.c / (k * k * k * k) |]; power = 1; multiplier = 1.0 } ]; wavelengthScale = scale },
                    zeroFormula scale)
        | Sellmeier c ->
            match abscissaKindOf c.wavelengthUnit with
            | LinearInX scale ->
                // Bᵢ·x²/(x²−cᵢ) = Bᵢ + Bᵢ·cᵢ/(x²−cᵢ): ε = (1 + ΣBᵢ) + Σ inverse terms.
                let constant = complexConstTerm (Complex (1.0 + List.sum c.b, 0.0))
                let oscillator (b : double) (ci : double) : ComplexDispersionTerm =
                    complexPolyTerm (Complex (b * ci, 0.0)) (-1) [| Complex (-ci, 0.0); Complex.Zero; Complex.One |]
                ComplexEps { terms = constant :: List.map2 oscillator c.b c.c; wavelengthScale = scale }
            | ReciprocalInX (scale, k) ->
                // abscissa = k/x: Bᵢ·a²/(a²−cᵢ) = Bᵢ·k²/(k² − cᵢ·x²) — one inverse term each.
                let oscillator (b : double) (ci : double) : ComplexDispersionTerm =
                    complexPolyTerm (Complex (b * k * k, 0.0)) (-1) [| Complex (k * k, 0.0); Complex.Zero; Complex (-ci, 0.0) |]
                ComplexEps { terms = complexConstTerm Complex.One :: List.map2 oscillator c.b c.c; wavelengthScale = scale }
        | Lorentz c ->
            let constant = complexConstTerm (Complex (c.epsInf, 0.0))
            match abscissaKindOf c.wavelengthUnit with
            | LinearInX scale ->
                // ε = ε∞ + Σ sⱼ/(rⱼ² − x² − i·dⱼ·x): one literal inverse term per oscillator.
                let oscillator (s : double) (r : double) (d : double) : ComplexDispersionTerm =
                    complexPolyTerm (Complex (s, 0.0)) (-1) [| Complex (r * r, 0.0); Complex (0.0, -d); Complex (-1.0, 0.0) |]
                ComplexEps { terms = constant :: List.map3 oscillator c.strength c.resonance c.damping; wavelengthScale = scale }
            | ReciprocalInX (scale, k) ->
                // abscissa = k/x: sⱼ/(rⱼ² − a² − i·dⱼ·a) = sⱼ·x²/(rⱼ²·x² − i·dⱼ·k·x − k²).
                let oscillator (s : double) (r : double) (d : double) : ComplexDispersionTerm list =
                    rationalXSquaredTerms (Complex (s, 0.0)) (Complex (-k * k, 0.0)) (Complex (0.0, -d * k)) (Complex (r * r, 0.0))
                ComplexEps { terms = constant :: (List.map3 oscillator c.strength c.resonance c.damping |> List.concat); wavelengthScale = scale }
        | Drude c ->
            let constant = complexConstTerm (Complex (c.epsInf, 0.0))
            let negPlasmaSq = Complex (-c.plasmaFrequency * c.plasmaFrequency, 0.0)
            match abscissaKindOf c.wavelengthUnit with
            | LinearInX scale ->
                // ε = ε∞ − ωp²/(x² + i·γ·x).
                let free = complexPolyTerm negPlasmaSq (-1) [| Complex.Zero; Complex (0.0, c.dampingFrequency); Complex.One |]
                ComplexEps { terms = [ constant; free ]; wavelengthScale = scale }
            | ReciprocalInX (scale, k) ->
                // abscissa = k/x: −ωp²/(a² + i·γ·a) = −ωp²·x²/(k² + i·γ·k·x).
                let free = rationalXSquaredTerms negPlasmaSq (Complex (k * k, 0.0)) (Complex (0.0, c.dampingFrequency * k)) Complex.Zero
                ComplexEps { terms = constant :: free; wavelengthScale = scale }
        // The transcendental models are not finite term sums (band-gap steps, exp,
        // the Faddeeva function), so they lower to the evaluated case: the SAME
        // `evaluate` closure the engine's `EpsWithDisp` case applies.
        | TaucLorentz _
        | GaussianOscillator _
        | ForouhiBloomer _
        | BrendelBormann _ -> EpsAxisEvaluated (evaluate model)

    /// Wrap an `EpsWithDisp` into an isotropic `OpticalPropertiesWithDisp` with the
    /// `Mu.vacuum`/`Rho.vacuum` dispersive defaults — mirroring `Silicon`'s record
    /// (`OpticalProperties/Dispersive.fs:90-95`). This is the single site for the
    /// vacuum-μ/ρ convention: `toOpticalProperties`/`toAnisotropicOpticalProperties`
    /// and `MaterialImport` all build their isotropic-dispersive entries through it.
    let isotropicProperties (epsWithDisp : EpsWithDisp) : OpticalPropertiesWithDisp =
        {
            epsWithDisp = epsWithDisp
            muWithDisp = Mu.vacuum.dispersive
            rhoWithDisp = Rho.vacuum.dispersive
        }

    /// Nominal interval for a single-segment tree: with one segment the topmost
    /// segment extrapolates for every wavelength (`Dispersion.fs:346`), so a model
    /// lowered whole needs no real bounds.
    let private nominalInterval : WaveLengthInterval =
        { lower = toWaveLength Nanometer 0.0; upper = toWaveLength Nanometer 0.0 }

    /// The serializable eps value a model composes to (§D.6 / spec 0033 §B):
    /// `ConstantNK` short-circuits to the non-dispersive constant
    /// (`EpsWithoutDispValue`, `Dispersion.fs:379` — no closure overhead); every
    /// other model becomes a single-segment isotropic tree over its lowered
    /// dispersion. The transcendental cases lower to the evaluated segment (their
    /// `EpsAxisEvaluated` closure), so this is TOTAL — no lowering error.
    let toEpsValue (model : DispersionModel) : EpsWithDispValue =
        match model with
        | ConstantNK c ->
            createComplex c.n c.k |> ComplexRefractionIndex |> IsotropicAbsorbing |> EpsWithoutDispValue
        | _ ->
            EpsWithDispValue (IsotropicDispersive [ { wavelengthInterval = nominalInterval; dispersion = toEpsAxis model } ])

    /// Compose a model into the engine's `OpticalPropertiesWithDisp` (§D.6)
    /// through the serializable eps tree: `toEpsValue`, then
    /// `EpsWithDispValue.toEpsWithDisp` (`Dispersion.fs:383`), then the
    /// `isotropicProperties` vacuum-μ/ρ wrapper. `ConstantNK` still emits
    /// `EpsWithoutDisp` (no closure overhead); the transcendental cases ride the
    /// evaluated segment, whose `complexIndex` applies `evaluate` — identical to
    /// wrapping the closure directly. One route, no lowering error.
    let toOpticalProperties (model : DispersionModel) : OpticalPropertiesWithDisp =
        (toEpsValue model).toEpsWithDisp |> isotropicProperties

    /// Build a uniaxial principal-axis `Eps` from (n_o, n_e) through the engine's
    /// existing three-argument constructor, mapped as `(n_o, n_e, n_o)`
    /// (`MaterialProperties.fs:87`, AC-D5). No tensor is re-typed here.
    let uniaxialEps (no : ComplexRefractionIndex) (ne : ComplexRefractionIndex) : Eps =
        Eps.fromComplexRefractionIndex (no, ne, no)

    /// Build a biaxial principal-axis `Eps` from (n_x, n_y, n_z) through the same
    /// engine constructor.
    let biaxialEps (nx : ComplexRefractionIndex) (ny : ComplexRefractionIndex) (nz : ComplexRefractionIndex) : Eps =
        Eps.fromComplexRefractionIndex (nx, ny, nz)

    /// Compose a (possibly anisotropic) serializable eps value into the engine's
    /// `OpticalPropertiesWithDisp` (§D.7). Anisotropy lives INSIDE
    /// `EpsWithDispValue` — the uniaxial/biaxial constant cases and the per-axis
    /// dispersive segment trees (`Dispersion.fs:285,355`) supersede the removed
    /// `AnisotropicModel` — so composition is `toEpsWithDisp` through the same
    /// vacuum-μ/ρ wrapper. Crystal-axis ORIENTATION is NOT here — rotation reuses
    /// `Layer.rotate` (Part B).
    let toAnisotropicOpticalProperties (eps : EpsWithDispValue) : OpticalPropertiesWithDisp =
        eps.toEpsWithDisp |> isotropicProperties
