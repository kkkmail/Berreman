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
        | ConstantNK c -> c.thermoOptic
        | SumOfTerms _ -> None

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

    /// Why a model resists lowering to finite term data (§6.1): the transcendental
    /// cases (band-gap step, exp) are not finite term sums — they stay named
    /// `DispersionModel` cases evaluated directly through `evaluate`, and
    /// `toOpticalProperties` wraps that closure instead.
    type EpsAxisLoweringError =
        | NotAFiniteTermSum of reason : string

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

    /// Lower a model to the per-axis serializable term data (§6.1): Cauchy to
    /// `RealNK` Laurent terms (k = the zero formula); Sellmeier — via the
    /// oscillator identity Bᵢ·a²/(a²−cᵢ) = Bᵢ + Bᵢ·cᵢ/(a²−cᵢ) — to real-valued ε
    /// terms carried as `ComplexEps`, because a `RealNK` n-formula is a finite
    /// term sum and cannot take the square root, while `ComplexEps.complexIndex`
    /// evaluates exactly the required √(1 + Σ); ConstantNK to `RealNK` constants;
    /// Lorentz and Drude to `ComplexEps` inverse terms (complex is genuine — the
    /// damping); `SumOfTerms` to itself. Reciprocal-abscissa tabulations (eV,
    /// cm⁻¹ — the oscillator convention) substitute abscissa = k/x and stay exact
    /// through `rationalXSquaredTerms`. TaucLorentz / GaussianOscillator are NOT
    /// finite term sums — a typed error; they keep evaluating directly through
    /// `evaluate`. The lowering is isothermal: like `evaluate`, it takes a `Some`
    /// thermo-optic model at its reference temperature (Δn = 0); the operating
    /// temperature enters only at the `evaluateAt` boundary (§D.12).
    let toEpsAxis (model : DispersionModel) : Result<EpsAxisDispersion, EpsAxisLoweringError> =
        match model with
        | SumOfTerms axis -> Ok axis
        | ConstantNK c ->
            let scale =
                match abscissaKindOf c.wavelengthUnit with
                | LinearInX scale -> scale
                | ReciprocalInX (scale, _) -> scale
            Ok (RealNK (
                { terms = [ realConstTerm c.n ]; wavelengthScale = scale },
                { terms = [ realConstTerm c.k ]; wavelengthScale = scale }))
        | Cauchy c ->
            match abscissaKindOf c.wavelengthUnit with
            | LinearInX scale ->
                // n(x) = A + B·x⁻² + C·x⁻⁴ — a real Laurent polynomial.
                Ok (RealNK (
                    { terms = [ realConstTerm c.a; realInversePowerTerm c.b 2; realInversePowerTerm c.c 4 ]; wavelengthScale = scale },
                    zeroFormula scale))
            | ReciprocalInX (scale, k) ->
                // abscissa = k/x: n = A + (B/k²)·x² + (C/k⁴)·x⁴ — a plain polynomial.
                Ok (RealNK (
                    { terms = [ { lambda = 0.0; coefficients = [| c.a; 0.0; c.b / (k * k); 0.0; c.c / (k * k * k * k) |]; power = 1; multiplier = 1.0 } ]; wavelengthScale = scale },
                    zeroFormula scale))
        | Sellmeier c ->
            match abscissaKindOf c.wavelengthUnit with
            | LinearInX scale ->
                // Bᵢ·x²/(x²−cᵢ) = Bᵢ + Bᵢ·cᵢ/(x²−cᵢ): ε = (1 + ΣBᵢ) + Σ inverse terms.
                let constant = complexConstTerm (Complex (1.0 + List.sum c.b, 0.0))
                let oscillator (b : double) (ci : double) : ComplexDispersionTerm =
                    complexPolyTerm (Complex (b * ci, 0.0)) (-1) [| Complex (-ci, 0.0); Complex.Zero; Complex.One |]
                Ok (ComplexEps { terms = constant :: List.map2 oscillator c.b c.c; wavelengthScale = scale })
            | ReciprocalInX (scale, k) ->
                // abscissa = k/x: Bᵢ·a²/(a²−cᵢ) = Bᵢ·k²/(k² − cᵢ·x²) — one inverse term each.
                let oscillator (b : double) (ci : double) : ComplexDispersionTerm =
                    complexPolyTerm (Complex (b * k * k, 0.0)) (-1) [| Complex (k * k, 0.0); Complex.Zero; Complex (-ci, 0.0) |]
                Ok (ComplexEps { terms = complexConstTerm Complex.One :: List.map2 oscillator c.b c.c; wavelengthScale = scale })
        | Lorentz c ->
            let constant = complexConstTerm (Complex (c.epsInf, 0.0))
            match abscissaKindOf c.wavelengthUnit with
            | LinearInX scale ->
                // ε = ε∞ + Σ sⱼ/(rⱼ² − x² − i·dⱼ·x): one literal inverse term per oscillator.
                let oscillator (s : double) (r : double) (d : double) : ComplexDispersionTerm =
                    complexPolyTerm (Complex (s, 0.0)) (-1) [| Complex (r * r, 0.0); Complex (0.0, -d); Complex (-1.0, 0.0) |]
                Ok (ComplexEps { terms = constant :: List.map3 oscillator c.strength c.resonance c.damping; wavelengthScale = scale })
            | ReciprocalInX (scale, k) ->
                // abscissa = k/x: sⱼ/(rⱼ² − a² − i·dⱼ·a) = sⱼ·x²/(rⱼ²·x² − i·dⱼ·k·x − k²).
                let oscillator (s : double) (r : double) (d : double) : ComplexDispersionTerm list =
                    rationalXSquaredTerms (Complex (s, 0.0)) (Complex (-k * k, 0.0)) (Complex (0.0, -d * k)) (Complex (r * r, 0.0))
                Ok (ComplexEps { terms = constant :: (List.map3 oscillator c.strength c.resonance c.damping |> List.concat); wavelengthScale = scale })
        | Drude c ->
            let constant = complexConstTerm (Complex (c.epsInf, 0.0))
            let negPlasmaSq = Complex (-c.plasmaFrequency * c.plasmaFrequency, 0.0)
            match abscissaKindOf c.wavelengthUnit with
            | LinearInX scale ->
                // ε = ε∞ − ωp²/(x² + i·γ·x).
                let free = complexPolyTerm negPlasmaSq (-1) [| Complex.Zero; Complex (0.0, c.dampingFrequency); Complex.One |]
                Ok (ComplexEps { terms = [ constant; free ]; wavelengthScale = scale })
            | ReciprocalInX (scale, k) ->
                // abscissa = k/x: −ωp²/(a² + i·γ·a) = −ωp²·x²/(k² + i·γ·k·x).
                let free = rationalXSquaredTerms negPlasmaSq (Complex (k * k, 0.0)) (Complex (0.0, c.dampingFrequency * k)) Complex.Zero
                Ok (ComplexEps { terms = constant :: free; wavelengthScale = scale })
        | TaucLorentz _ ->
            Error (NotAFiniteTermSum "TaucLorentz is piecewise transcendental (band-gap step); it stays a named case evaluated directly")
        | GaussianOscillator _ ->
            Error (NotAFiniteTermSum "GaussianOscillator is transcendental (exp); it stays a named case evaluated directly")

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
    /// other lowerable model becomes a single-segment isotropic tree over its
    /// lowered term data; the transcendental cases carry no finite term data and
    /// surface the typed lowering error (`toOpticalProperties` wraps `evaluate`
    /// for them instead).
    let toEpsValue (model : DispersionModel) : Result<EpsWithDispValue, EpsAxisLoweringError> =
        match model with
        | ConstantNK c ->
            createComplex c.n c.k |> ComplexRefractionIndex |> IsotropicAbsorbing |> EpsWithoutDispValue |> Ok
        | _ ->
            toEpsAxis model
            |> Result.map (fun axis ->
                EpsWithDispValue (IsotropicDispersive [ { wavelengthInterval = nominalInterval; dispersion = axis } ]))

    /// Compose a model into the engine's `OpticalPropertiesWithDisp` (§D.6)
    /// through the serializable eps tree: `toEpsValue`, then
    /// `EpsWithDispValue.toEpsWithDisp` (`Dispersion.fs:383`), then the
    /// `isotropicProperties` vacuum-μ/ρ wrapper. `ConstantNK` still emits
    /// `EpsWithoutDisp` (no closure overhead), and the transcendental named cases
    /// — which have no finite term data — wrap `evaluate` directly; both routes
    /// end at the same engine type.
    let toOpticalProperties (model : DispersionModel) : OpticalPropertiesWithDisp =
        match toEpsValue model with
        | Ok value -> value.toEpsWithDisp |> isotropicProperties
        | Error (NotAFiniteTermSum _) ->
            let f = evaluate model
            (fun w -> Eps.fromComplexRefractionIndex (f w)) |> EpsWithDisp |> isotropicProperties

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
