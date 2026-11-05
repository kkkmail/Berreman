namespace Berreman
open System.Numerics
open MathNetNumericsMath
open Berreman.Geometry
open Constants
open MaterialProperties
open Fields
open Media

module Dispersion =

    type EpsWithDisp =
        | EpsWithDisp of (WaveLength -> Eps)
        | EpsWithoutDisp of Eps

        member this.getEps w =
            match this with
            | EpsWithDisp f -> f w
            | EpsWithoutDisp e -> e


    type Eps
        with
        member eps.dispersive = EpsWithoutDisp eps


    type MuWithDisp =
        | MuWithDisp of (WaveLength -> Mu)
        | MuWithoutDisp of Mu

        member this.getMu w =
            match this with
            | MuWithDisp f -> f w
            | MuWithoutDisp e -> e

    type Mu
        with
        member mu.dispersive = MuWithoutDisp mu


    type RhoWithDisp =
        | RhoWithDisp of (WaveLength -> Rho)
        | RhoWithoutDisp of Rho

        member this.getRho w =
            match this with
            | RhoWithDisp f -> f w
            | RhoWithoutDisp e -> e


    type Rho
        with
        member rho.dispersive = RhoWithoutDisp rho


    type OpticalPropertiesWithDisp =
        {
            epsWithDisp : EpsWithDisp
            muWithDisp : MuWithDisp
            rhoWithDisp : RhoWithDisp
        }

        member this.getProperties w =
            {
                eps = this.epsWithDisp.getEps w
                mu = this.muWithDisp.getMu w
                rho = this.rhoWithDisp.getRho w
            }


    type OpticalProperties
        with
        member this.dispersive =
            {
                epsWithDisp = this.eps.dispersive
                muWithDisp = this.mu.dispersive
                rhoWithDisp = this.rho.dispersive
            }


    type LayerWithDisp =
        {
            propertiesWithDisp : OpticalPropertiesWithDisp
            thickness : Thickness
        }

        member this.getLayer w =
            {
                properties = this.propertiesWithDisp.getProperties w
                thickness = this.thickness
            }

    type InclinedLayerWithDisp =
        {
            layerWithDisp : LayerWithDisp
            angle : WedgeAngle
        }

        member this.getInclinedLayer w v =
            {
                layer = this.layerWithDisp.getLayer w
                angle = v
            }


    type SubstrateWithDisp =
        | PlateWithDisp of LayerWithDisp
        | WedgeWithDisp of InclinedLayerWithDisp

        member this.getSubstrate w v =
            match this with
            | PlateWithDisp e -> e.getLayer w |> Plate
            | WedgeWithDisp e -> e.getInclinedLayer w v |> Wedge


    type Layer
        with
        member this.dispersive =
            {
                propertiesWithDisp = this.properties.dispersive
                thickness = this.thickness
            }


    type WedgeLayer
        with
        member this.dispersive =
            {
                layerWithDisp = this.layer.dispersive
                angle = this.angle
            }


    type Substrate
        with
        member this.dispersive =
            match this with
            | Plate e -> PlateWithDisp e.dispersive
            | Wedge e -> WedgeWithDisp e.dispersive


    type OpticalSystemWithDisp =
        {
            description : OpticalSystemDescription option
            upperWithDisp : OpticalPropertiesWithDisp
            filmsWithDisp : List<LayerWithDisp>
            substrateWithDisp : SubstrateWithDisp option
            lowerWithDisp : OpticalPropertiesWithDisp
        }

        member this.getSystem w v =
            {
                description = this.description
                upper = this.upperWithDisp.getProperties w
                films = this.filmsWithDisp |> List.map (fun f -> f.getLayer w)
                substrate =
                    match this.substrateWithDisp with
                    | Some s -> s.getSubstrate w v |> Some
                    | None -> None
                lower = this.lowerWithDisp.getProperties w
            }

        member this.getWedgeAngle() =
            match this.substrateWithDisp with
            | Some (WedgeWithDisp w) -> Some w.angle
            | _ -> None


    type OpticalSystem
        with
        member this.dispersive =
            {
                description = this.description
                upperWithDisp = this.upper.dispersive
                filmsWithDisp = this.films |> List.map (fun f -> f.dispersive)
                substrateWithDisp =
                    match this.substrate with
                    | Some s -> s.dispersive |> Some
                    | None -> None
                lowerWithDisp = this.lower.dispersive
            }


    // ==========================================================================
    // Generic serializable dispersion formula blocks (spec 0033 Part B).
    // Pure data + evaluation from which closures BUILD the …WithDisp funcs above;
    // the engine unions in this file stay unchanged and non-serializable.
    // ==========================================================================


    /// A wavelength interval — pure serializable data: two elevated endpoints.
    /// An interval is just an interval: no ordering validation and no clamping —
    /// overlap ("first covering segment wins") and extrapolation ("topmost
    /// segment") rules belong to the consumers, not to the interval.
    type WaveLengthInterval =
        {
            lower : WaveLength
            upper : WaveLength
        }


    /// One additive term of a real dispersion formula over the REDUCED
    /// wavelength x (the canonical wavelength divided by the owning formula's
    /// wavelengthScale): multiplier * (Σ_k coefficients.[k] * (x - lambda)^k) ^ power.
    /// A negative power expresses the inverse (Sellmeier / Laurent) shapes.
    [<Softellect.Sys.NumericCarrier("the centre, the coefficients, the exponent and the multiplier of one term of a dispersion formula, evaluated at every wavelength of a sweep (R3; as STL's TaylorApproximation)")>]
    type DispersionTerm =
        {
            lambda : double
            coefficients : double array
            power : int
            multiplier : double
        }

        /// Evaluates the term at the reduced wavelength x (already in coefficient units).
        member this.evaluate (x : double) : double =
            let d = x - this.lambda
            let poly = Array.foldBack (fun c acc -> acc * d + c) this.coefficients 0.0
            this.multiplier * pown poly this.power


    /// The value a real dispersion formula takes at one wavelength: an n, a k, a mu or a gyration component.
    type DispersionValue =
        | DispersionValue of float

        member this.value = let (DispersionValue v) = this in v
        member this.toMuValue : MuValue = let (DispersionValue v) = this in MuValue v
        member this.toRhoValue : RhoValue = let (DispersionValue v) = this in RhoValue v


    /// A real-valued dispersion formula: the sum of its terms over the reduced
    /// wavelength. wavelengthScale is METRES PER COEFFICIENT UNIT (e.g. 1.0e-6
    /// for coefficients tabulated in µm): evaluate reduces the canonical
    /// wavelength (metres) to the coefficient unit and sums the terms.
    type DispersionFormula =
        {
            terms : DispersionTerm list
            wavelengthScale : float<meter>
        }

        member this.evaluate (w : WaveLength) : DispersionValue =
            let x = w.value / this.wavelengthScale
            this.terms |> List.sumBy (fun t -> t.evaluate x) |> DispersionValue


    /// Mirrors DispersionTerm over System.Numerics.Complex — used ONLY where a
    /// model is inherently complex (e.g. Lorentz / Drude ε). The exponent stays
    /// an integer; the value fields (lambda, coefficients, multiplier) are complex.
    [<Softellect.Sys.NumericCarrier("the centre, the coefficients, the exponent and the multiplier of one complex term of a dispersion formula, evaluated at every wavelength of a sweep (R3; as STL's TaylorApproximation)")>]
    type ComplexDispersionTerm =
        {
            lambda : Complex
            coefficients : Complex array
            power : int
            multiplier : Complex
        }

        /// Evaluates the term at the reduced wavelength x (already in coefficient units).
        member this.evaluate (x : double) : Complex =
            // Integer power over Complex by repeated multiplication (Complex has no pown); a negative
            // exponent inverts, so a pole stays a pole.
            let complexPown (z : Complex) (n : int) : Complex =
                let p = seq { 1 .. abs n } |> Seq.fold (fun acc _ -> acc * z) Complex.One
                if n >= 0 then p else Complex.One / p

            let d = Complex (x, 0.0) - this.lambda
            let poly = Array.foldBack (fun c acc -> acc * d + c) this.coefficients Complex.Zero
            this.multiplier * complexPown poly this.power


    /// Mirrors DispersionFormula over System.Numerics.Complex; wavelengthScale
    /// stays real (METRES PER COEFFICIENT UNIT — a unit scale, not a value).
    type ComplexDispersionFormula =
        {
            terms : ComplexDispersionTerm list
            wavelengthScale : float<meter>
        }

        member this.evaluate (w : WaveLength) : Complex =
            let x = w.value / this.wavelengthScale
            this.terms |> List.fold (fun acc t -> acc + t.evaluate x) Complex.Zero


    /// A reciprocal abscissa: abscissa = numerator / x, with x = λ / wavelengthScale.
    [<Softellect.Sys.NumericCarrier("the reduced-wavelength scale and the numerator of a reciprocal abscissa, folded into every coefficient a reciprocal-abscissa lowering builds (R3)")>]
    type ReciprocalAbscissa =
        {
            wavelengthScale : float<meter>
            numerator : float
        }


    /// A dispersion law's abscissa at one wavelength: a reduced wavelength or a photon energy.
    type AbscissaValue =
        | AbscissaValue of float

        member this.value = let (AbscissaValue v) = this in v


    /// How a model's abscissa relates to the formula's reduced wavelength
    /// x = λ / wavelengthScale (`DispersionFormula.wavelengthScale`): a length unit reads its
    /// abscissa as x itself; the energy-like units are reciprocal,
    /// abscissa = numerator / x with x in nanometers. In the core so a law carries it as data
    /// (spec 0050 §3.1).
    type AbscissaKind =
        | LinearInX of wavelengthScale : float<meter>
        | ReciprocalInX of ReciprocalAbscissa

        /// The abscissa at `w`. It is the arithmetic `Units.fromMeters` performs over the scale
        /// and numerator `ofUnit` derives, so the value is bit-identical for every unit (spec
        /// 0050 step 5).
        member this.abscissaAt (w : WaveLength) : AbscissaValue =
            match this with
            | LinearInX scale -> w.value / scale |> AbscissaValue
            | ReciprocalInX r -> r.numerator / (w.value / r.wavelengthScale) |> AbscissaValue


    /// Tauc–Lorentz: `ε₂(E)` above the band gap, `ε₁ = εInf` (no Kramers–Kronig `ε₁`), over the
    /// model's abscissa. The kernel lives here once (spec 0050 §3.1).
    [<Softellect.Sys.NumericCarrier("the five coefficients and the abscissa of one Tauc-Lorentz law, read by its kernel at every wavelength of a sweep (R3; as DispersionTerm)")>]
    type TaucLorentzLaw =
        {
            epsInf : float
            amplitude : float
            resonance : float
            broadening : float
            bandGap : float
            abscissa : AbscissaKind
        }

        member c.complexIndex (w : WaveLength) : ComplexRefractionIndex =
            let (AbscissaValue x) = c.abscissa.abscissaAt w
            let eps2 =
                if x > c.bandGap then
                    (c.amplitude * c.resonance * c.broadening * (x - c.bandGap) ** 2.0)
                    / (x * ((x * x - c.resonance * c.resonance) ** 2.0 + c.broadening * c.broadening * x * x))
                else 0.0
            Complex.Sqrt (Complex(c.epsInf, eps2)) |> ComplexRefractionIndex


    /// Gaussian oscillator: `ε₂(E) = A·exp(−((E − E₀)/σ)²)`, `ε₁ = εInf`, over the model's
    /// abscissa. The kernel lives here once (spec 0050 §3.1).
    [<Softellect.Sys.NumericCarrier("the four coefficients and the abscissa of one Gaussian-oscillator law, read by its kernel at every wavelength of a sweep (R3; as DispersionTerm)")>]
    type GaussianOscillatorLaw =
        {
            epsInf : float
            amplitude : float
            energy : float
            broadening : float
            abscissa : AbscissaKind
        }

        member c.complexIndex (w : WaveLength) : ComplexRefractionIndex =
            let (AbscissaValue x) = c.abscissa.abscissaAt w
            let d = (x - c.energy) / c.broadening
            Complex.Sqrt (Complex(c.epsInf, c.amplitude * exp (-(d * d)))) |> ComplexRefractionIndex


    /// Forouhi–Bloomer five-parameter amorphous form: `k(E) = a·(E − bandGap)²/(E² − b·E + c)`
    /// above the band gap, zero below it, with the Kramers–Kronig closed form for `n(E)`, over the
    /// model's abscissa. The kernel lives here once (spec 0050 §3.1).
    [<Softellect.Sys.NumericCarrier("the five coefficients and the abscissa of one Forouhi-Bloomer law, read by its kernel at every wavelength of a sweep (R3; as DispersionTerm)")>]
    type ForouhiBloomerLaw =
        {
            nInf : float
            a : float
            b : float
            c : float
            bandGap : float
            abscissa : AbscissaKind
        }

        member c.complexIndex (w : WaveLength) : ComplexRefractionIndex =
            let (AbscissaValue x) = c.abscissa.abscissaAt w
            let q = 0.5 * sqrt (4.0 * c.c - c.b * c.b)
            let b0 = c.a / q * (-0.5 * c.b * c.b + c.bandGap * c.b - c.bandGap * c.bandGap + c.c)
            let c0 = c.a / q * (0.5 * c.b * (c.bandGap * c.bandGap + c.c) - 2.0 * c.bandGap * c.c)
            let denominator = x * x - c.b * x + c.c
            let n = c.nInf + (b0 * x + c0) / denominator
            let k = if x > c.bandGap then c.a * (x - c.bandGap) * (x - c.bandGap) / denominator else 0.0
            Complex.create n k |> ComplexRefractionIndex


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
    /// `BrendelBormannLaw.complexIndex` has Im ≥ 0 (α is the principal root of
    /// E² + i·Γ·E), so the half-plane restriction is safe by construction.
    let private faddeeva (z : Complex) : Complex =
        let l = Complex.create faddeevaSeries.scale 0.0
        let iz = Complex.create 0.0 1.0 * z
        let ratio = (l + iz) / (l - iz)
        let p = Array.foldBack (fun a acc -> acc * ratio + Complex.create a 0.0) faddeevaSeries.coefficients Complex.Zero
        Complex.create 2.0 0.0 * p / ((l - iz) * (l - iz)) + Complex.create (1.0 / sqrt System.Math.PI) 0.0 / (l - iz)


    /// Brendel–Bormann Gaussian-broadened (Voigt) oscillators:
    /// `ε(E) = 1 − f₀·ωp²/(E² + i·Γ₀·E) + Σⱼ χⱼ(E)`, each `χⱼ` a Faddeeva-function pair, over the
    /// model's abscissa. The four per-oscillator lists run in step. The kernel lives here once
    /// (spec 0050 §3.1).
    [<Softellect.Sys.NumericCarrier("the intraband coefficients, the per-oscillator lists and the abscissa of one Brendel-Bormann law, read by its kernel (the Faddeeva series) at every wavelength of a sweep (R3; as DispersionTerm)")>]
    type BrendelBormannLaw =
        {
            plasmaFrequency : float
            intrabandStrength : float
            intrabandDamping : float
            strength : float list
            resonance : float list
            damping : float list
            broadening : float list
            abscissa : AbscissaKind
        }

        member c.complexIndex (w : WaveLength) : ComplexRefractionIndex =
            let (AbscissaValue x) = c.abscissa.abscissaAt w
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
            Complex.Sqrt (Complex.One - intraband + interband) |> ComplexRefractionIndex


    // ==========================================================================
    // Serializable eps tree (spec 0033 Part B).
    // Pure data from which toEpsWithDisp BUILDS the engine's EpsWithDisp above
    // through the existing Eps constructors; the engine unions stay unchanged.
    // ==========================================================================


    /// The ordinary and extraordinary indices of a transparent uniaxial medium.
    type UniaxialIndices =
        {
            ordinary : RefractionIndex
            extraordinary : RefractionIndex
        }


    /// The ordinary and extraordinary indices of an absorbing uniaxial medium.
    type UniaxialComplexIndices =
        {
            ordinaryComplex : ComplexRefractionIndex
            extraordinaryComplex : ComplexRefractionIndex
        }


    /// The three principal indices of a transparent biaxial medium.
    type BiaxialIndices =
        {
            nx : RefractionIndex
            ny : RefractionIndex
            nz : RefractionIndex
        }


    /// The three principal indices of an absorbing biaxial medium.
    type BiaxialComplexIndices =
        {
            nxComplex : ComplexRefractionIndex
            nyComplex : ComplexRefractionIndex
            nzComplex : ComplexRefractionIndex
        }


    /// A non-dispersive eps — a DESCRIPTIVE DU, never a bare Eps: the case says
    /// what the medium is (symmetry × transparency), and toEps builds the engine
    /// matrix through the existing constructors. The uniaxial cases map to the
    /// (ordinary, extraordinary, ordinary) diagonal — the epsLa3Ga5SiO14 precedent.
    type ConstantEpsValue =
        | IsotropicTransparent of RefractionIndex
        | IsotropicAbsorbing of ComplexRefractionIndex
        | UniaxialTransparent of UniaxialIndices
        | UniaxialAbsorbing of UniaxialComplexIndices
        | BiaxialTransparent of BiaxialIndices
        | BiaxialAbsorbing of BiaxialComplexIndices

        member this.toEps : Eps =
            match this with
            | IsotropicTransparent n -> Eps.fromRefractionIndex n
            | IsotropicAbsorbing n -> Eps.fromComplexRefractionIndex n
            | UniaxialTransparent u -> Eps.fromRefractionIndex (u.ordinary, u.extraordinary, u.ordinary)
            | UniaxialAbsorbing u -> Eps.fromComplexRefractionIndex (u.ordinaryComplex, u.extraordinaryComplex, u.ordinaryComplex)
            | BiaxialTransparent b -> Eps.fromRefractionIndex (b.nx, b.ny, b.nz)
            | BiaxialAbsorbing b -> Eps.fromComplexRefractionIndex (b.nxComplex, b.nyComplex, b.nzComplex)


    /// The real n and k formulas of one principal axis.
    type RealNkFormulas =
        {
            nFormula : DispersionFormula
            kFormula : DispersionFormula
        }


    /// How a tabulated n,k table reads between and beyond its samples. `value` is the wire tag.
    type TabulatedInterpolation =
        /// Linear in λ between neighbouring samples, clamped to the end samples outside the table.
        | LinearInWaveLengthClamped

        member this.value : int =
            match this with
            | LinearInWaveLengthClamped -> 1

        static member tryOfWire (v : int) : TabulatedInterpolation option =
            match v with
            | 1 -> Some LinearInWaveLengthClamped
            | _ -> None


    /// Why `TabulatedNkTable.tryCreate` refused its columns.
    type TabulatedTableError =
        | EmptyTableErr of reason : string
        | SampleCountMismatchErr of reason : string
        | UnsortedSamplesErr of reason : string


    /// A tabulated n,k table (spec 0050 step 2, 001 §B): the samples of one principal axis,
    /// wavelengths ascending, read through `interpolation`.
    [<Softellect.Sys.NumericCarrier("the wavelength, n and k columns of a tabulated n,k table, a measured data series (R3): complexIndex scans them at every wavelength of a sweep")>]
    type TabulatedNkTable =
        {
            wavelengths : float<meter> array
            n : float array
            k : float array
            interpolation : TabulatedInterpolation
        }

        /// The table over these columns, read linearly in λ and clamped. Refuses an empty table,
        /// columns of unequal length and a wavelength column that decreases anywhere; never sorts.
        static member tryCreate (wavelengths : float<meter> array) (n : float array) (k : float array) : Result<TabulatedNkTable, TabulatedTableError> =
            if n.Length <> wavelengths.Length || k.Length <> wavelengths.Length then
                Error (SampleCountMismatchErr $"{wavelengths.Length} wavelengths, {n.Length} n values and {k.Length} k values")
            elif wavelengths.Length = 0 then
                Error (EmptyTableErr "a tabulated n,k table needs at least one sample")
            else
                match wavelengths |> Array.pairwise |> Array.tryFindIndex (fun (a, b) -> b < a) with
                | Some i -> Error (UnsortedSamplesErr $"the wavelength at index {i + 1} ({wavelengths.[i + 1]} m) is below the one at index {i} ({wavelengths.[i]} m)")
                | None -> Ok { wavelengths = wavelengths; n = n; k = k; interpolation = LinearInWaveLengthClamped }

        /// The first to the last sample.
        member this.sampleRange : WaveLengthInterval =
            {
                lower = WaveLength.nm (this.wavelengths.[0] / nmToMeter)
                upper = WaveLength.nm (this.wavelengths.[this.wavelengths.Length - 1] / nmToMeter)
            }

        /// The index at `w`: one sample answers itself; at or below the first sample the first, at
        /// or above the last the last; otherwise linear in λ between the bracketing pair (spec 0050
        /// step 2).
        member this.complexIndex (w : WaveLength) : ComplexRefractionIndex =
            match this.interpolation with
            | LinearInWaveLengthClamped ->
                let lam = w.value
                let count = this.wavelengths.Length
                let n, k =
                    if count = 1 then
                        this.n.[0], this.k.[0]
                    else
                        let last = count - 1
                        if lam <= this.wavelengths.[0] then this.n.[0], this.k.[0]
                        elif lam >= this.wavelengths.[last] then this.n.[last], this.k.[last]
                        else
                            let mutable i = 0
                            while i < count - 2 && this.wavelengths.[i + 1] < lam do
                                i <- i + 1
                            let a = this.wavelengths.[i]
                            let b = this.wavelengths.[i + 1]
                            let t = if b = a then 0.0 else (lam - a) / (b - a)
                            this.n.[i] + (this.n.[i + 1] - this.n.[i]) * t, this.k.[i] + (this.k.[i + 1] - this.k.[i]) * t
                Complex (n, k) |> ComplexRefractionIndex


    /// The dispersion of ONE principal axis: either separate real n and k
    /// formulas (k = the zero formula for a transparent medium), one complex
    /// eps formula for inherently complex models (Lorentz / Drude), a tabulated
    /// n,k table (`TabulatedNK`, spec 0050 step 2), a Tauc–Lorentz or
    /// Gaussian-oscillator law (spec 0050 step 5), a Forouhi–Bloomer or
    /// Brendel–Bormann law (step 7), or an evaluated-directly closure, which no
    /// analytic model lowers to any more (the coded presets still carry one) — it carries a
    /// `WaveLength -> ComplexRefractionIndex` exactly as the engine's own `EpsWithDisp`
    /// case does, and `complexIndex` applies it. A function-typed case removes F#'s
    /// structural equality type-wide, so the type provides custom equality: every data
    /// case compares structurally (keeping the `DispersionModel` / `EpsWithDispValue` /
    /// `MaterialComplexity` equality the round-trip tests use) and the evaluated case
    /// compares by closure reference.
    [<CustomEquality; NoComparison>]
    type EpsAxisDispersion =
        | RealNK of RealNkFormulas
        | ComplexEps of ComplexDispersionFormula
        | TabulatedNK of TabulatedNkTable
        | TaucLorentzAxis of TaucLorentzLaw
        | GaussianOscillatorAxis of GaussianOscillatorLaw
        | ForouhiBloomerAxis of ForouhiBloomerLaw
        | BrendelBormannAxis of BrendelBormannLaw
        | EpsAxisEvaluated of (WaveLength -> ComplexRefractionIndex)

        member this.complexIndex (w : WaveLength) : ComplexRefractionIndex =
            match this with
            | RealNK f ->
                let (DispersionValue n) = f.nFormula.evaluate w
                let (DispersionValue k) = f.kFormula.evaluate w
                Complex (n, k) |> ComplexRefractionIndex
            | ComplexEps eps -> eps.evaluate w |> sqrt |> ComplexRefractionIndex
            | TabulatedNK table -> table.complexIndex w
            | TaucLorentzAxis law -> law.complexIndex w
            | GaussianOscillatorAxis law -> law.complexIndex w
            | ForouhiBloomerAxis law -> law.complexIndex w
            | BrendelBormannAxis law -> law.complexIndex w
            | EpsAxisEvaluated f -> f w

        override this.Equals (o : obj) : bool =
            match o with
            | :? EpsAxisDispersion as other ->
                match this, other with
                | RealNK f1, RealNK f2 -> f1 = f2
                | ComplexEps e1, ComplexEps e2 -> e1 = e2
                | TabulatedNK t1, TabulatedNK t2 -> t1 = t2
                | TaucLorentzAxis l1, TaucLorentzAxis l2 -> l1 = l2
                | GaussianOscillatorAxis l1, GaussianOscillatorAxis l2 -> l1 = l2
                | ForouhiBloomerAxis l1, ForouhiBloomerAxis l2 -> l1 = l2
                | BrendelBormannAxis l1, BrendelBormannAxis l2 -> l1 = l2
                | EpsAxisEvaluated f1, EpsAxisEvaluated f2 -> LanguagePrimitives.PhysicalEquality f1 f2
                | _ -> false
            | _ -> false

        override this.GetHashCode () : int =
            match this with
            | RealNK f -> hash (0, f.nFormula, f.kFormula)
            | ComplexEps e -> hash (1, e)
            | TabulatedNK t -> hash (2, t)
            | TaucLorentzAxis l -> hash (3, l)
            | GaussianOscillatorAxis l -> hash (4, l)
            | ForouhiBloomerAxis l -> hash (5, l)
            | BrendelBormannAxis l -> hash (6, l)
            | EpsAxisEvaluated f -> LanguagePrimitives.PhysicalHash f


    /// One isotropic dispersion segment: a single axis over one interval.
    type IsotropicEpsSegment =
        {
            wavelengthInterval : WaveLengthInterval
            dispersion : EpsAxisDispersion
        }


    /// One uniaxial dispersion segment: the interval is SHARED by both axes.
    type UniaxialEpsSegment =
        {
            wavelengthInterval : WaveLengthInterval
            ordinaryDispersion : EpsAxisDispersion
            extraordinaryDispersion : EpsAxisDispersion
        }


    /// One biaxial dispersion segment: the interval is SHARED by all three axes.
    type BiaxialEpsSegment =
        {
            wavelengthInterval : WaveLengthInterval
            xDispersion : EpsAxisDispersion
            yDispersion : EpsAxisDispersion
            zDispersion : EpsAxisDispersion
        }


    /// Selects the segment for a wavelength: the FIRST segment whose interval
    /// covers it (inclusive endpoints) — top-of-list wins on overlap; when none
    /// covers, the topmost segment extrapolates. No clamps, no validation.
    let private selectSegment (segments : 'S list) (intervalOf : 'S -> WaveLengthInterval) (w : WaveLength) : 'S =
        let covers s =
            let i = intervalOf s
            i.lower.value <= w.value && w.value <= i.upper.value
        match segments |> List.tryFind covers with
        | Some s -> s
        | None -> segments |> List.head


    /// A dispersive eps as data: a homogeneous segment list per symmetry.
    type EpsDispersiveValue =
        | IsotropicDispersive of IsotropicEpsSegment list
        | UniaxialDispersive of UniaxialEpsSegment list
        | BiaxialDispersive of BiaxialEpsSegment list

        member this.getEps (w : WaveLength) : Eps =
            match this with
            | IsotropicDispersive segments ->
                let s = selectSegment segments (fun e -> e.wavelengthInterval) w
                s.dispersion.complexIndex w |> Eps.fromComplexRefractionIndex
            | UniaxialDispersive segments ->
                let s = selectSegment segments (fun e -> e.wavelengthInterval) w
                let nO = s.ordinaryDispersion.complexIndex w
                let nE = s.extraordinaryDispersion.complexIndex w
                Eps.fromComplexRefractionIndex (nO, nE, nO)
            | BiaxialDispersive segments ->
                let s = selectSegment segments (fun e -> e.wavelengthInterval) w
                Eps.fromComplexRefractionIndex (s.xDispersion.complexIndex w, s.yDispersion.complexIndex w, s.zDispersion.complexIndex w)


    /// The serializable counterpart of the engine's EpsWithDisp: either a
    /// dispersive segment tree or a constant value. toEpsWithDisp builds the
    /// engine type — the constant case short-circuits to EpsWithoutDisp.
    type EpsWithDispValue =
        | EpsWithDispValue of EpsDispersiveValue
        | EpsWithoutDispValue of ConstantEpsValue

        member this.toEpsWithDisp : EpsWithDisp =
            match this with
            | EpsWithDispValue d -> EpsWithDisp (fun w -> d.getEps w)
            | EpsWithoutDispValue c -> EpsWithoutDisp c.toEps


    // ==========================================================================
    // Serializable rho (gyration) tree (spec 0033 Part B).
    // Pure data — symmetry-class gyration ONLY (the rotation-producing classes;
    // deliberately no free 3x3 case). toRhoWithDisp lives in
    // OpticalProperties/Active.fs as a type extension: assembly needs the
    // crystal-class Rho builders there and the core cannot reference that project.
    // ==========================================================================


    /// The screw sense of an optically active crystal: the two enantiomorphs
    /// differ by ONE overall sign flip of the gyration tensor. RightHanded keeps
    /// the components as specified; LeftHanded negates the assembled tensor.
    type Handedness =
        | LeftHanded
        | RightHanded

        member this.sign : double =
            match this with
            | LeftHanded -> -1.0
            | RightHanded -> 1.0


    /// Uniaxial gyration components: the tensor is diag (g11, g11, g33).
    type UniaxialGyration<'g> =
        {
            g11 : 'g
            g33 : 'g
        }

        member this.map (f : 'g -> 'h) : UniaxialGyration<'h> =
            {
                g11 = f this.g11
                g33 = f this.g33
            }


    /// Orthorhombic class 222 gyration components: diag (g11, g22, g33).
    type Orthorhombic222Gyration<'g> =
        {
            g11 : 'g
            g22 : 'g
            g33 : 'g
        }

        member this.map (f : 'g -> 'h) : Orthorhombic222Gyration<'h> =
            {
                g11 = f this.g11
                g22 = f this.g22
                g33 = f this.g33
            }


    /// Monoclinic class 2 gyration components (two-fold axis along x2):
    /// the diagonal plus g13 = g31.
    type Monoclinic2Gyration<'g> =
        {
            g11 : 'g
            g22 : 'g
            g33 : 'g
            g13 : 'g
        }

        member this.map (f : 'g -> 'h) : Monoclinic2Gyration<'h> =
            {
                g11 = f this.g11
                g22 = f this.g22
                g33 = f this.g33
                g13 = f this.g13
            }


    /// Monoclinic class m gyration components (mirror normal to x2):
    /// only g12 = g21 and g23 = g32 survive.
    type MonoclinicMGyration<'g> =
        {
            g12 : 'g
            g23 : 'g
        }

        member this.map (f : 'g -> 'h) : MonoclinicMGyration<'h> =
            {
                g12 = f this.g12
                g23 = f this.g23
            }


    /// Triclinic class 1 gyration components: the full symmetric tensor.
    type Triclinic1Gyration<'g> =
        {
            g11 : 'g
            g22 : 'g
            g33 : 'g
            g23 : 'g
            g13 : 'g
            g12 : 'g
        }

        member this.map (f : 'g -> 'h) : Triclinic1Gyration<'h> =
            {
                g11 = f this.g11
                g22 = f this.g22
                g33 = f this.g33
                g23 = f this.g23
                g13 = f this.g13
                g12 = f this.g12
            }


    /// A symmetry-class gyration tensor over an abstract component 'g (RhoValue
    /// for a constant tensor, DispersionFormula for a dispersive one). ONLY the
    /// rotation-producing classes appear; the multi-component cases carry the
    /// NAMED records above, never anonymous tuples.
    type GyrationClass<'g> =
        | CubicActive of 'g
        | UniaxialActive of UniaxialGyration<'g>
        | PlanarActive of 'g
        | Orthorhombic222 of Orthorhombic222Gyration<'g>
        | Monoclinic2 of Monoclinic2Gyration<'g>
        | MonoclinicM of MonoclinicMGyration<'g>
        | Triclinic1 of Triclinic1Gyration<'g>

        member this.map (f : 'g -> 'h) : GyrationClass<'h> =
            match this with
            | CubicActive g11 -> CubicActive (f g11)
            | UniaxialActive u -> UniaxialActive (u.map f)
            | PlanarActive g12 -> PlanarActive (f g12)
            | Orthorhombic222 o -> Orthorhombic222 (o.map f)
            | Monoclinic2 m -> Monoclinic2 (m.map f)
            | MonoclinicM m -> MonoclinicM (m.map f)
            | Triclinic1 t -> Triclinic1 (t.map f)


    /// A gyration tensor plus the crystal's handedness — reused by both
    /// RhoWithDispValue cases.
    type GyrotropicValue<'g> =
        {
            gyration : GyrationClass<'g>
            hand : Handedness
        }


    /// The serializable counterpart of the engine's RhoWithDisp: a symmetry-class
    /// gyration whose components are either DispersionFormula (dispersive) or
    /// RhoValue (constant). toRhoWithDisp — the assembly onto the engine type —
    /// is a type extension in OpticalProperties/Active.fs, next to the
    /// crystal-class Rho builders it routes through.
    type RhoWithDispValue =
        | RhoWithDispValue of GyrotropicValue<DispersionFormula>
        | RhoWithoutDispValue of GyrotropicValue<RhoValue>


    // ==========================================================================
    // Serializable mu (Polder / gyromagnetic) tree (spec 0033 Part B).
    // Pure data + assembly: ONE generic Polder record covers both the constant
    // and dispersive cases, and toMuWithDisp BUILDS the engine's MuWithDisp
    // above through Mu.create — the engine unions stay unchanged. No solver
    // work: the Berreman matrix already reads off-diagonal mu.
    // ==========================================================================


    /// The magnetization axis of a gyromagnetic (Polder) permeability tensor.
    /// AlongZ is the Faraday geometry (propagation along the magnetization)
    /// and the default; the transverse axes (AlongX / AlongY) give the Voigt
    /// geometry.
    type GyrationAxis =
        | AlongX
        | AlongY
        | AlongZ

        static member defaultValue : GyrationAxis = AlongZ


    /// The Polder (gyromagnetic) permeability components over an abstract
    /// component 'g (MuValue for a constant tensor, DispersionFormula for a
    /// dispersive one): muDiagonal fills the two transverse diagonal slots,
    /// muParallel the axis slot, gyration the off-diagonal ±i·g pair. ONE
    /// generic record reused by both MuWithDispValue cases.
    type PolderValue<'g> =
        {
            muDiagonal : 'g
            muParallel : 'g
            gyration : 'g
            axis : GyrationAxis
        }

        member this.map (f : 'g -> 'h) : PolderValue<'h> =
            {
                muDiagonal = f this.muDiagonal
                muParallel = f this.muParallel
                gyration = f this.gyration
                axis = this.axis
            }


    /// A non-dispersive mu — a DESCRIPTIVE DU, never a bare Mu: either a
    /// scalar permeability (mu times the identity) or a constant gyromagnetic
    /// Polder tensor.
    type ConstantMuValue =
        | ScalarMu of MuValue
        | GyromagneticMu of PolderValue<MuValue>

        /// For the Polder tensor in the AlongZ (Faraday) geometry the rows are [mu, +i·g, 0], [-i·g, mu, 0],
        /// [0, 0, muParallel]; the transverse (Voigt) axes are its cyclic permutations, so the ±i·g pair keeps
        /// its right-handed sense about the magnetization axis.
        member this.toMu : Mu =
            match this with
            | ScalarMu (MuValue m) -> Complex (m, 0.0) * ComplexMatrix3x3.identity |> Mu
            | GyromagneticMu p ->
                let (MuValue mu) = p.muDiagonal
                let (MuValue muPar) = p.muParallel
                let (MuValue g) = p.gyration

                let m = Complex (mu, 0.0)
                let mPar = Complex (muPar, 0.0)
                let iG = Complex (0.0, g)
                let zero = Complex.Zero

                let rows =
                    match p.axis with
                    | AlongX ->
                        [
                            [ mPar; zero; zero ]
                            [ zero; m; iG ]
                            [ zero; -iG; m ]
                        ]
                    | AlongY ->
                        [
                            [ m; zero; -iG ]
                            [ zero; mPar; zero ]
                            [ iG; zero; m ]
                        ]
                    | AlongZ ->
                        [
                            [ m; iG; zero ]
                            [ -iG; m; zero ]
                            [ zero; zero; mPar ]
                        ]

                rows |> Mu.create


    /// The serializable counterpart of the engine's MuWithDisp: either a
    /// dispersive Polder tensor or a constant value. toMuWithDisp builds the
    /// engine type — the constant cases short-circuit to MuWithoutDisp; the
    /// dispersive case evaluates each component's formula at the wavelength
    /// and assembles per call.
    type MuWithDispValue =
        | MuWithDispValue of PolderValue<DispersionFormula>
        | MuWithoutDispValue of ConstantMuValue

        member this.toMuWithDisp : MuWithDisp =
            match this with
            | MuWithDispValue p ->
                MuWithDisp (fun w -> (p.map (fun (f : DispersionFormula) -> (f.evaluate w).toMuValue) |> GyromagneticMu).toMu)
            | MuWithoutDispValue c -> MuWithoutDisp c.toMu
