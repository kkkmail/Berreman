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
/// `ComplexRefractionIndex` (`MaterialProperties.fs:43`). `toOpticalProperties`
/// composes the closure into the engine's `OpticalPropertiesWithDisp` exactly as
/// `Silicon`/`Langasite` build by hand (`OpticalProperties/Dispersive.fs:87,52`):
/// no parallel tensor type, no re-derived EVD/matrix-exponential math.
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
    /// schema `$def` `materialEntry` dispersion sub-object (§A.7).
    type DispersionModel =
        | Sellmeier of SellmeierCoefficients
        | Cauchy of CauchyCoefficients
        | Lorentz of LorentzCoefficients
        | Drude of DrudeCoefficients
        | TaucLorentz of TaucLorentzCoefficients
        | GaussianOscillator of GaussianOscillatorCoefficients
        | ConstantNK of ConstantNKCoefficients

    /// The unit the model's coefficients were tabulated in (§D.5).
    let wavelengthUnitOf (model : DispersionModel) : UnitOfMeasure =
        match model with
        | Sellmeier c -> c.wavelengthUnit
        | Cauchy c -> c.wavelengthUnit
        | Lorentz c -> c.wavelengthUnit
        | Drude c -> c.wavelengthUnit
        | TaucLorentz c -> c.wavelengthUnit
        | GaussianOscillator c -> c.wavelengthUnit
        | ConstantNK c -> c.wavelengthUnit

    /// The optional thermo-optic parameters carried alongside the coefficient record (§D.12).
    let thermoOpticOf (model : DispersionModel) : ThermoOptic option =
        match model with
        | Sellmeier c -> c.thermoOptic
        | Cauchy c -> c.thermoOptic
        | Lorentz c -> c.thermoOptic
        | Drude c -> c.thermoOptic
        | TaucLorentz c -> c.thermoOptic
        | GaussianOscillator c -> c.thermoOptic
        | ConstantNK c -> c.thermoOptic

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

    /// Compose a model into the engine's `OpticalPropertiesWithDisp` (§D.6),
    /// reusing `Eps.fromComplexRefractionIndex` (`MaterialProperties.fs:87`) and
    /// the `isotropicProperties` vacuum-μ/ρ wrapper — mirroring `Silicon`'s record
    /// (`OpticalProperties/Dispersive.fs:90-95`). `ConstantNK` emits `EpsWithoutDisp`
    /// (`Dispersion.fs:11`) so a non-dispersive entry incurs no closure overhead.
    let toOpticalProperties (model : DispersionModel) : OpticalPropertiesWithDisp =
        match model with
        | ConstantNK c ->
            let n = createComplex c.n c.k |> ComplexRefractionIndex
            Eps.fromComplexRefractionIndex n |> EpsWithoutDisp |> isotropicProperties
        | _ ->
            let f = evaluate model
            (fun w -> Eps.fromComplexRefractionIndex (f w)) |> EpsWithDisp |> isotropicProperties

    /// Anisotropic principal-axis definitions (§D.7). Each principal index may
    /// itself be a per-axis `DispersionModel`, so a crystal can disperse differently
    /// along each axis exactly as `Langasite` evaluates ordinary/extraordinary
    /// separately (`OpticalProperties/Dispersive.fs:52-55`). Crystal-axis ORIENTATION
    /// is NOT here — rotation reuses `Layer.rotate` (Part B).
    type AnisotropicModel =
        | Uniaxial of ordinary : DispersionModel * extraordinary : DispersionModel
        | Biaxial of x : DispersionModel * y : DispersionModel * z : DispersionModel

    /// Build a uniaxial principal-axis `Eps` from (n_o, n_e) through the engine's
    /// existing three-argument constructor, mapped as `(n_o, n_e, n_o)`
    /// (`MaterialProperties.fs:87`, AC-D5). No tensor is re-typed here.
    let uniaxialEps (no : ComplexRefractionIndex) (ne : ComplexRefractionIndex) : Eps =
        Eps.fromComplexRefractionIndex (no, ne, no)

    /// Build a biaxial principal-axis `Eps` from (n_x, n_y, n_z) through the same
    /// engine constructor.
    let biaxialEps (nx : ComplexRefractionIndex) (ny : ComplexRefractionIndex) (nz : ComplexRefractionIndex) : Eps =
        Eps.fromComplexRefractionIndex (nx, ny, nz)

    /// Compose a possibly-per-axis-dispersive anisotropic model into the engine's
    /// `OpticalPropertiesWithDisp` (§D.7), reusing the three-index constructor.
    let toAnisotropicOpticalProperties (model : AnisotropicModel) : OpticalPropertiesWithDisp =
        let epsClosure =
            match model with
            | Uniaxial (mo, me) ->
                let fo = evaluate mo
                let fe = evaluate me
                fun w -> uniaxialEps (fo w) (fe w)
            | Biaxial (mx, my, mz) ->
                let fx = evaluate mx
                let fy = evaluate my
                let fz = evaluate mz
                fun w -> biaxialEps (fx w) (fy w) (fz w)
        EpsWithDisp epsClosure |> isotropicProperties
