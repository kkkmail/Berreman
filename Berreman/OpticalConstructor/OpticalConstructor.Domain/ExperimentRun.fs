namespace OpticalConstructor.Domain

open Berreman.Constants
open Berreman.Fields
open Berreman.Geometry
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.ItemModels

/// What a detector reads at the end of the light's actual path (spec 0046): the source's emission folded through every
/// step's Mueller matrix in light order, no step special-cased (spec 0050 S8.1). Pure: the host evaluates the steps.
module ExperimentRun =

    /// One step on the way to a detector, in light order, already evaluated (spec 0050 S8.1).
    type RunStep =
        /// An element that is not an object, at its orientation: a polarizer, a prep or an analyzer item (spec 0050 S8.1).
        | ElementStep of mueller : MuellerMatrix
        /// An object, solved for the branch the detector's path leaves it by (spec 0050 S8.1).
        | ObjectStep of mueller : MuellerMatrix

        /// The step's Mueller matrix (spec 0050 S8.1).
        member this.mueller : MuellerMatrix =
            match this with
            | ElementStep mueller
            | ObjectStep mueller -> mueller

    /// The light a source emits: its polarization's azimuth ψ and ellipticity angle χ (radians), its degree of
    /// polarization p and its intensity scale I (spec 0050 S8.1; 001 §I, §N).
    type SourceEmission =
        {
            azimuth : Angle
            ellipticity : Angle
            degreeOfPolarization : ParameterValue
            intensityScale : ParameterValue
        }

        /// I · [1, p·cos 2χ·cos 2ψ, p·cos 2χ·sin 2ψ, p·sin 2χ]; a positive χ is +S₃, the handedness
        /// `Propagation.inputStokes` calls left (spec 0050 S8.1).
        member this.stokes : StokesVector =
            let intensity = this.intensityScale.value
            let polarized = intensity * this.degreeOfPolarization.value
            let twoPsi = 2.0 * this.azimuth.value
            let twoChi = 2.0 * this.ellipticity.value
            StokesVector.create [ intensity; polarized * cos twoChi * cos twoPsi; polarized * cos twoChi * sin twoPsi; polarized * sin twoChi ]

        /// `stokes` times the path's intensity reference: the inverse of the S₀ a path's leading element passes of
        /// unpolarized light, when it passes any; 1 for a path opening on an object, or on nothing (spec 0050 S8.1; 001 §N).
        member this.emittedInto (steps : RunStep list) : StokesVector =
            let reference =
                match steps with
                | ElementStep first :: _ ->
                    let passed = (first * Propagation.unpolarizedStokes).s0
                    if passed > 0.0 then 1.0 / passed else 1.0
                | ObjectStep _ :: _
                | [] -> 1.0

            let emitted = this.stokes
            StokesVector.create [ reference * emitted.s0; reference * emitted.s1; reference * emitted.s2; reference * emitted.s3 ]

    /// What a detector reads (spec 0046 — the detector decides what is captured).
    type Reading =
        | IntensityReading of intensity : DetectorIntensity
        | EllipsometricReading of Analytics.AnalysisFunctions.PsiDelta

    /// The polarization state arriving at the detector: `emission` into the path, through each step's matrix in light
    /// order (spec 0050 S8.1).
    let detectorStokes (emission : SourceEmission) (steps : RunStep list) : StokesVector =
        steps |> List.fold (fun sv step -> step.mueller * sv) (emission.emittedInto steps)

    /// What a detector of `kind` reads after `steps` from `emission` (spec 0046; spec 0050 S8.1).
    let read (kind : DetectorKind) (emission : SourceEmission) (steps : RunStep list) : Reading =
        let sv = detectorStokes emission steps
        match kind with
        | Intensity -> IntensityReading sv.intensity
        | Ellipsometer -> EllipsometricReading sv.psiDelta

    type RunStep with

        /// The path's Mueller matrix: the steps' matrices multiplied in light order, the identity for no step; a
        /// polarimeter's recorded matrix (spec 0050 S11.2).
        static member productOf (steps : RunStep list) : MuellerMatrix =
            steps |> List.fold (fun product step -> step.mueller * product) Propagation.identityMueller

    type SourcePreset with

        /// The wavelength a run uses when it does not vary it (spec 0046; spec 0050 S11.2): a TUNABLE source's `requested`
        /// setting clamped to its limits, else its own setting; a fixed line's own wavelength; 600 nm with no source.
        static member operatingWaveLengthOf (sourceOpt : SourcePreset option) (requested : WaveLength option) : WaveLength =
            match sourceOpt with
            | Some source ->
                match source.tuning, requested with
                | TunableBetween limits, Some w ->
                    let inNanometers (x : WaveLength) : Units.UnitMagnitude = Units.wavelengthToUnit Units.Nanometer x
                    Units.toWaveLength Units.Nanometer (max (inNanometers limits.lower) (min (inNanometers limits.upper) (inNanometers w)))
                | TunableBetween _, None
                | FixedLine, _ -> source.waveLength
            | None -> WaveLength.nm 600.0<nm>
