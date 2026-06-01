namespace OpticalConstructor.Domain

open System
open System.Numerics
open Berreman
open Berreman.Constants
open Berreman.Geometry
open Berreman.Fields
open Analytics.Variables
open OpticalConstructor.Domain.Units

/// Source / illumination domain (§E.1–E.9). The NET-NEW orchestration layer that
/// wraps the engine's canonical `IncidentLightInfo` (`Fields.fs:338`) and carries
/// ONLY the orchestration fields that have no engine equivalent. Every [Standard]
/// feature here (spectral profiles, cone fans, unpolarized split, multi-source,
/// Gaussian beam) EXPANDS a base `IncidentLightInfo` into a list of WEIGHTED
/// `IncidentLightInfo` values; the averaging is performed on the solver's
/// `StokesVector`/intensity OUTPUTS (`SourceCombination.fs`), never on `EmField`
/// amplitudes (§E-risk). The canonical `IncidentLightInfo` is never changed.
///
/// Nothing here forks an engine primitive: wavelength conversion routes through
/// the sole `Units` seam (`toWaveLength`, §D.2), the Gaussian angular spectrum
/// reuses `createGaussian` (`FourierTransform.fs:74`) + `FourierTransform.fft`
/// (`FourierTransform.fs:137`), and `IncidentLightInfo` is built via the engine's
/// own `create`/`createInclined` (`Fields.fs:375,384`). All physical quantities
/// stay in canonical SI: wavelength lives in `IncidentLightInfo.waveLength` whose
/// `.value` is meters, the beam waist is `double<meter>`, temperature is kelvin.
module SourceSpec =

    /// Kelvin. The engine `Constants.fs` carries no temperature measure; this
    /// net-new unit traces to §E.5's `Blackbody of double<K>` directive and is the
    /// only place a Kelvin literal appears. It is a thermodynamic temperature, the
    /// SI base unit, so no boundary conversion is needed (it is already canonical).
    [<Measure>] type K

    // --- Planck's law (§E.5 blackbody / standard-illuminant weight tables) ------

    /// Planck spectral radiance B(λ,T) in SI (W·sr⁻¹·m⁻³): the relative spectral
    /// weight of a blackbody at temperature `tempK` (kelvin) at wavelength
    /// `lambdaM` (canonical meters). Only the SHAPE matters for a spectral weight,
    /// so the absolute radiometric scale is irrelevant — the weights are
    /// re-normalised by the averaging layer (§E.8).
    let private planck (tempK : float) (lambdaM : float) : float =
        let h  = 6.62607015e-34   // Planck constant, J·s
        let c  = 2.99792458e8     // speed of light, m/s
        let kB = 1.380649e-23     // Boltzmann constant, J/K
        if lambdaM <= 0.0 || tempK <= 0.0 then 0.0
        else
            let numerator = 2.0 * h * c * c
            let denominator = (lambdaM ** 5.0) * (exp (h * c / (lambdaM * kB * tempK)) - 1.0)
            numerator / denominator

    // --- Discriminated unions (§E.3–E.7, E.9) -----------------------------------

    /// The two CIE standard illuminants in scope (§E.5). `A` is exactly a Planckian
    /// radiator at 2855.54 K; `D65` is approximated here by a 6504 K Planckian
    /// (the full CIE daylight S0/S1/S2 series is out of [Standard] minimum scope —
    /// see Gotchas).
    type StandardIlluminant =
        | D65
        | A

    /// Spectral source profile (§E.5). `LaserLine` reuses the engine `WaveLength`
    /// (`Fields.fs:280`); `ImportedSpectrum` carries a sidecar RELATIVE path to a
    /// tabulated λ→weight spectrum (parsed with FSharp.Data by the storage seam,
    /// never embedded as a `.binz` pickle).
    type SpectralProfile =
        | Flat
        | Blackbody of double<K>
        | Illuminant of StandardIlluminant
        | LaserLine of WaveLength
        | ImportedSpectrum of string

    /// Cone / acceptance fan (§E.6). `Angle` is the engine geometry type already
    /// wrapped by `IncidenceAngle` (`Fields.fs:22`). Collimated illumination is the
    /// ABSENCE of a cone (`cone = None`), never a zero-angle cone.
    type ConeAcceptance =
        {
            halfAngle : Angle
            samples : int
        }

    /// Coherent / incoherent / unpolarized flag (§E.7). `Coherent` maps to the
    /// engine's existing coherent solve; `Incoherent` selects the substrate-driven
    /// `Multiple`/`MultipleEmFieldSystem` path; `Unpolarized` is expanded into the
    /// incoherent average of `Polarization.s`/`Polarization.p`, averaged on output.
    type Coherence =
        | Coherent
        | Incoherent
        | Unpolarized

    /// Gaussian-beam source (§E.9). The waist is stored in canonical meters; the
    /// far-field divergence half-angle bounds the angular-spectrum fan.
    type GaussianBeamSpec =
        {
            waist : double<meter>
            divergence : Angle
        }

    /// Single-value-or-range axis (§E.3). The `Ranged` case reuses the engine's
    /// `RangedVariable` (`Variables.fs:42`) rather than introducing a new range
    /// type; for wavelength the ranged value is a `WaveLengthRange`, for angle an
    /// `IncidenceAngleRange`. `Fixed` carries the single committed value.
    type SourceAxis<'T> =
        | Fixed of 'T
        | Ranged of RangedVariable

    // --- Spectral profile sampling (§E.5) ---------------------------------------

    type SpectralProfile with

        /// Expand a spectral profile into a list of (wavelength, spectral weight)
        /// pairs over the wavelength `range` of §E.3 (§E.5). Each pair produces one
        /// `IncidentLightInfo` for the solver and a weight for §E.8 averaging. The
        /// blackbody / D65 / A weights are COMPUTED in canonical SI (λ via
        /// `WaveLength.value`, T in kelvin). A `LaserLine` is a monochromatic delta
        /// and returns its single line wavelength regardless of `range`.
        /// `ImportedSpectrum` is loaded from its sidecar by the storage seam
        /// (FSharp.Data), so the pure sampler treats an unloaded import as empty.
        static member sample (profile : SpectralProfile) (range : RangedVariable) : (WaveLength * float) list =
            // The §E.3 wavelength axis is sampled directly in the canonical meter
            // base and each point is re-wrapped as a `WaveLength.Nm` whose `.value`
            // reduces back to meters (the `MaterialImport.exportCsv` precedent) —
            // NOT through `Analytics.getWaveLengthValue`, which re-wraps the meter
            // magnitude as a nm scalar and so mis-scales `.value` by `nmToMeter`.
            match profile, range with
            | LaserLine wl, _ -> [ (wl, 1.0) ]
            | ImportedSpectrum _, _ -> []
            | _, WaveLengthRange r ->
                let weightOf =
                    match profile with
                    | Flat -> fun (_ : WaveLength) -> 1.0
                    | Blackbody t -> fun (wl : WaveLength) -> planck (float (t / 1.0<K>)) (wl.value / 1.0<meter>)
                    | Illuminant ill ->
                        let temp = match ill with | A -> 2855.54 | D65 -> 6504.0
                        fun (wl : WaveLength) -> planck temp (wl.value / 1.0<meter>)
                    | LaserLine _ | ImportedSpectrum _ -> fun (_ : WaveLength) -> 1.0  // handled above
                let s = r.startValue.value
                let e = r.endValue.value
                let n = max 1 r.numberOfPoints   // guard n=0 (matches the cone's `max 1 samples`)
                [ for i in 0 .. n ->
                    let m = s + (e - s) * (float i) / (float n)
                    let wl = WaveLength.Nm (m / nmToMeter)
                    (wl, weightOf wl) ]
            | _, _ -> []   // a non-wavelength range cannot be spectrally sampled

    // --- Cone / acceptance fan (§E.6) -------------------------------------------

    type ConeAcceptance with

        /// Fan the base incidence angle into `samples` sub-angles uniformly across
        /// ±`halfAngle` (§E.6). Each produces one `IncidentLightInfo` via a record
        /// copy on `incidenceAngle` (done by `SourceSpec.expand`); the expanded
        /// angles are averaged on the solver's `StokesVector`/intensity OUTPUTS,
        /// never on field amplitudes. `samples <= 1` collapses to the base angle.
        static member sample (cone : ConeAcceptance) (baseAngle : IncidenceAngle) : IncidenceAngle list =
            let n = max 1 cone.samples
            let (Angle half) = cone.halfAngle
            if n = 1 then [ baseAngle ]
            else
                [ for i in 0 .. n - 1 ->
                    // fraction in [-1, +1] across the cone aperture.
                    let frac = (float i / float (n - 1)) * 2.0 - 1.0
                    baseAngle + Angle (frac * half) ]

    // --- Gaussian-beam angular spectrum (§E.9) ----------------------------------

    type GaussianBeamSpec with

        /// Decompose the Gaussian beam into weighted plane-wave incidence angles
        /// (§E.9), REUSING `createGaussian` (`FourierTransform.fs:74`) for the
        /// transverse profile and `FourierTransform.fft` (`FourierTransform.fs:137`)
        /// for the transform — no new FFT, Gaussian generator, or beam-propagation
        /// engine. A real-space Gaussian of the beam waist is transformed; FFT bin
        /// `j` maps to a transverse spatial frequency `f` and thus to a plane-wave
        /// angle via `sinθ = f·λ`; the weight is the spectral intensity `|F[j]|²`.
        /// The fan is bounded by the beam `divergence` half-angle and the weights
        /// are normalised to sum 1. Angles feed the same expand/averaging path as
        /// §E.6/§E.8.
        static member angularSpectrum (beam : GaussianBeamSpec) (w : WaveLength) : (IncidenceAngle * float) list =
            let n = 64                               // power of 2 for FourierTransform.fft
            let lambda = w.value / 1.0<meter>        // canonical meters
            let w0 = beam.waist / 1.0<meter>         // waist in meters
            let (Angle divHalf) = beam.divergence
            if w0 <= 0.0 || lambda <= 0.0 then [ IncidenceAngle.normal, 1.0 ]
            else
                // Spatial window spanning several waists; σ in createGaussian's
                // normalised [-0.5,0.5) domain so the physical field profile is
                // exp(-x²/w0²): 2σ² = (w0/L)² ⇒ σ = w0/(L·√2).
                let windowFactor = 8.0
                let lWindow = windowFactor * w0
                let sigma = w0 / (lWindow * sqrt 2.0)
                let profile = FourierTransformPrimitives.createGaussian n 0.0 sigma
                let spectrum = FourierTransform.fft FourierTransformPrimitives.ForwardTransform profile
                let raw =
                    [ for j in 0 .. n - 1 ->
                        // FFT bin -> signed spatial frequency (cycles/m), matching
                        // createGaussian's index wrap-around (0..N/2 positive, then negative).
                        let jj = if j <= n / 2 then j else j - n
                        let freq = float jj / lWindow
                        let sinTheta = freq * lambda
                        let c : Complex = spectrum.[j]
                        let weight = c.Magnitude * c.Magnitude
                        (sinTheta, weight) ]
                    |> List.filter (fun (sinTheta, _) ->
                        abs sinTheta < 1.0 && abs (asin sinTheta) <= divHalf + 1.0e-12)
                let total = raw |> List.sumBy snd
                if total <= 0.0 then [ IncidenceAngle.normal, 1.0 ]
                else
                    raw
                    |> List.map (fun (sinTheta, weight) ->
                        IncidenceAngle.create (Angle (abs (asin sinTheta))), weight / total)

    // --- The source record (§E.1) + projection/expansion (§E.2, E.8) ------------

    /// The net-new orchestration source record (§E.1). It wraps the canonical
    /// `IncidentLightInfo` (`Fields.fs:338`) as `light` — storing the base
    /// monochromatic state ONCE, never duplicating its five fields — and adds only
    /// the orchestration fields with no engine equivalent. `displayUnit` is a
    /// presentation-only tag (the `unitOfMeasure` `$def`, §A.7); `intensity` is a
    /// unit-scalar per-source weight (§E.2/E.8), NOT threaded into
    /// `IncidentLightInfo` (which has no intensity field). Immutable F# record.
    type SourceSpec =
        {
            sourceId : string
            name : string option
            light : IncidentLightInfo
            displayUnit : UnitOfMeasure
            intensity : float
            spectralProfile : SpectralProfile option
            cone : ConeAcceptance option
            gaussianBeam : GaussianBeamSpec option
            coherence : Coherence
        }

        /// Project the source's base state to the canonical `IncidentLightInfo`
        /// (§E.2): build via `IncidentLightInfo.create` (`Fields.fs:375`) at normal
        /// incidence or `IncidentLightInfo.createInclined` (`Fields.fs:384`) at a
        /// non-zero angle, then set `polarization`/`ellipticity`/`refractionIndex`
        /// by record copy. The 4×4 solver is NOT called here (Part F owns fields).
        static member toIncidentLight (s : SourceSpec) : IncidentLightInfo =
            let baseInfo =
                if s.light.incidenceAngle.value = 0.0 then IncidentLightInfo.create s.light.waveLength
                else IncidentLightInfo.createInclined s.light.waveLength s.light.incidenceAngle
            { baseInfo with
                polarization = s.light.polarization
                ellipticity = s.light.ellipticity
                refractionIndex = s.light.refractionIndex }

        /// Expand the source into a list of weighted `IncidentLightInfo` values
        /// (§E.8), composing §E.4 (polarization, already carried by the base light),
        /// the §E.6 cone fan, the §E.9 Gaussian angular fan, the §E.7 unpolarized
        /// split, and the §E.2/§E.8 per-source intensity weight. Each entry is fed
        /// to the single-system solver and combined on the OUTPUT side
        /// (`SourceCombination.combine`). Recomputed on demand from the immutable
        /// `SourceSpec` — no cached expansion list (§E.8, constraint 6).
        ///
        /// NOTE: `expand` carries no wavelength range, so the range-based part of
        /// §E.5 (Flat/Blackbody/Illuminant) is `SpectralProfile.sample`, consumed
        /// by the sweep layer (Part F); the range-free `LaserLine` IS folded in
        /// here by substituting its line wavelength.
        static member expand (s : SourceSpec) : (IncidentLightInfo * float) list =
            let baseInfo = SourceSpec.toIncidentLight s

            // §E.5 (range-free): a LaserLine fixes the wavelength.
            let withSpectrum =
                match s.spectralProfile with
                | Some (LaserLine wl) -> { baseInfo with waveLength = wl }
                | _ -> baseInfo

            // §E.6: cone fan (absolute angles) or collimated (the single base angle).
            let angleInfos =
                match s.cone with
                | Some cone ->
                    ConeAcceptance.sample cone withSpectrum.incidenceAngle
                    |> List.map (fun a -> { withSpectrum with incidenceAngle = a })
                | None -> [ withSpectrum ]

            // §E.9: the Gaussian angular fan layered onto each cone angle (if set);
            // otherwise the cone fan is AVERAGED (§E.6), not summed — each of the N
            // angles carries weight 1/N so the combined Stokes/intensity output is the
            // mean over the cone aperture, matching the sibling Gaussian path (weights
            // sum to 1) and the unpolarized 0.5/0.5 split. A collimated source (N = 1)
            // reduces to the single base angle at unit weight.
            let beamInfos =
                let coneWeight = 1.0 / float (List.length angleInfos)
                match s.gaussianBeam with
                | Some beam ->
                    angleInfos
                    |> List.collect (fun info ->
                        GaussianBeamSpec.angularSpectrum beam info.waveLength
                        |> List.map (fun (a, weight) -> { info with incidenceAngle = a }, weight * coneWeight))
                | None ->
                    angleInfos |> List.map (fun info -> info, coneWeight)

            // §E.7: an Unpolarized source is the incoherent average of s/p
            // (averaged on output by §E.8); Coherent/Incoherent pass through (the
            // flag selects the solver path downstream, not the expansion shape).
            let polInfos =
                match s.coherence with
                | Unpolarized ->
                    beamInfos
                    |> List.collect (fun (info, weight) ->
                        [ { info with polarization = Polarization.s }, weight * 0.5
                          { info with polarization = Polarization.p }, weight * 0.5 ])
                | Coherent | Incoherent -> beamInfos

            // §E.2/§E.8: the per-source intensity weight.
            polInfos |> List.map (fun (info, weight) -> info, weight * s.intensity)
