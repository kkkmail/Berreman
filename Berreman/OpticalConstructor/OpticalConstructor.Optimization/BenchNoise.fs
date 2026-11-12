namespace OpticalConstructor.Optimization

open Berreman.Constants
open Berreman.Geometry
open Berreman.Fields
open Berreman.Media
open OpticalConstructor.Domain
open OpticalConstructor.Domain.ItemModels
open OpticalConstructor.Domain.MuellerInverse
open OpticalConstructor.Optimization.MuellerFitting

/// The bench measurement-error model: angle errors filed against the nominal configuration and detector errors on the
/// normalized elements, each a uniform max error drawn from a seeded provider. Magnitudes and sources:
/// specs/0044/.manual/011-noisy-measurement-tests.md and, for the throughput, 019-absorbing-biaxial-noisy-tests.md (spec 0050 S10.1).
module BenchNoise =

    /// The half-width of the uniform error on a recorded rotation angle: logged 20°, really 20° ± this.
    type AngleUncertainty =
        | AngleUncertainty of Angle

        member this.angle : Angle = let (AngleUncertainty a) = this in a

        /// In radians.
        member this.value : double = let (AngleUncertainty a) = this in a.value

        /// In degrees, as rotation-stage data sheets quote it.
        member this.degrees : double = let (AngleUncertainty a) = this in a.degrees

        static member degree (d : float<deg>) : AngleUncertainty = Angle.degree d |> AngleUncertainty

        /// The detector noise axis value, in radians (spec 0050 S10.1).
        member this.parameterValue : ParameterValue = let (AngleUncertainty (Angle radians)) = this in ParameterValue radians

    /// The half-width of the uniform error on one NORMALIZED Mueller element, a fraction of full scale.
    type ElementUncertainty =
        | ElementUncertainty of double

        member this.value : double = let (ElementUncertainty u) = this in u

        /// The detector noise axis value (spec 0050 S10.1).
        member this.parameterValue : ParameterValue = let (ElementUncertainty u) = this in ParameterValue u

    /// The half-width of the uniform error on the ABSOLUTE throughput `m₀₀`, a fraction of the reading; only an
    /// absorbing sample reads it. Looser than `ElementUncertainty`: a throughput is a ratio of two acquisitions.
    type ThroughputUncertainty =
        | ThroughputUncertainty of double

        member this.value : double = let (ThroughputUncertainty u) = this in u

        /// The detector noise axis value (spec 0050 S10.1).
        member this.parameterValue : ParameterValue = let (ThroughputUncertainty u) = this in ParameterValue u

    /// One measurement-error budget of MAX errors: each draw is uniform on `[−u, +u]` (σ = u/√3), because a stage's
    /// setting error and a detector's linearity error are bounded and have no tail.
    type NoiseParam =
        {
            /// Applied independently to the incidence angle and the sample azimuth of every configuration.
            rotation : AngleUncertainty
            /// Applied independently to each of the 15 normalized Mueller elements other than `m₀₀`.
            element : ElementUncertainty
            /// Applied to the absolute throughput `m₀₀`; every transparent path normalizes it away.
            throughput : ThroughputUncertainty
        }

        /// The decent-but-ordinary bench; a property of the apparatus, so every suite measures on it.
        static member decentLab : NoiseParam =
            {
                rotation = AngleUncertainty.degree 0.2<deg>
                element = ElementUncertainty 0.005
                throughput = ThroughputUncertainty 0.01
            }

        /// Nothing uncertain: the control that proves the noise is a pure overlay.
        static member noiseless : NoiseParam =
            {
                rotation = AngleUncertainty.degree 0.0<deg>
                element = ElementUncertainty 0.0
                throughput = ThroughputUncertainty 0.0
            }

        /// The budget as a detector's noise axes, in Domain's names and order (spec 0050 S10.1).
        member this.parameterAxes : ItemParameterAxis list =
            ItemParameterAxis.detectorNoiseAxes this.rotation.parameterValue this.element.parameterValue this.throughput.parameterValue

    /// The seed that fixes one noisy realization of a whole experiment.
    type NoiseSeed =
        | NoiseSeed of int

        member this.value : int = let (NoiseSeed s) = this in s

    /// One draw, uniform on [−1, +1].
    type UniformDeviate =
        | UniformDeviate of double

        member this.value : double = let (UniformDeviate d) = this in d

    /// Ambient randomness as a provider record, so a noisy experiment is a pure function of its seed.
    [<ReferenceEquality>]
    type UniformDeviateProvider =
        {
            nextDeviate : unit -> UniformDeviate
        }

        /// `System.Random(seed)` keeps its legacy algorithm, so a seeded stream is reproducible across runtimes.
        static member fromSeed (seed : NoiseSeed) : UniformDeviateProvider =
            let rng = System.Random(seed.value)
            { nextDeviate = fun () -> 2.0 * rng.NextDouble() - 1.0 |> UniformDeviate }

    /// Where the sample really was when a nominal configuration was recorded: incidence and azimuth wobble
    /// independently. `IncidenceAngle + Angle` keeps a tilt past normal signed, where `IncidenceAngle.create` folds it.
    let disturb (draws : UniformDeviateProvider) (noise : NoiseParam) (c : MeasurementConfiguration) : MeasurementConfiguration =
        let wobble () = noise.rotation.value * (draws.nextDeviate ()).value |> Angle
        {
            c with
                incidenceAngle = c.incidenceAngle + wobble ()
                azimuth = c.azimuth.angle + wobble () |> SampleAzimuth
        }

    /// What the receiver reports: the matrix normalized, then each element but `m₀₀` misread, row by row.
    /// `m₀₀` stays exactly 1 because every comparison divides the absolute throughput out.
    let misread (draws : UniformDeviateProvider) (noise : NoiseParam) (m : MuellerMatrix) : Result<MuellerMatrix, MuellerFitError> =
        match m.normalized with
        | Ok n ->
            RealMatrix4x4.create
                [ for i in 0 .. 3 ->
                    [ for j in 0 .. 3 ->
                        if i = 0 && j = 0 then 1.0
                        else n.element i j + noise.element.value * (draws.nextDeviate ()).value ] ]
            |> MuellerMatrix
            |> Ok
        | Error e -> Error (MuellerNormalizationErr e)

    /// What the receiver reports for an absorbing sample: `m₀₀` is the throughput misread by its own budget, and every
    /// other element is that throughput times the normalized element misread as `misread` draws it, in the same order.
    /// The throughput draw comes first, so a seed reproduces the experiment (specs/0044/.manual/019).
    let misreadAbsolute (draws : UniformDeviateProvider) (noise : NoiseParam) (m : MuellerMatrix) : Result<MuellerMatrix, MuellerFitError> =
        match m.normalized with
        | Ok n ->
            let trueThroughput = m.element 0 0
            let reportedThroughput = trueThroughput * (1.0 + noise.throughput.value * (draws.nextDeviate ()).value)
            RealMatrix4x4.create
                [ for i in 0 .. 3 ->
                    [ for j in 0 .. 3 ->
                        if i = 0 && j = 0 then reportedThroughput
                        else reportedThroughput * (n.element i j + noise.element.value * (draws.nextDeviate ()).value) ] ]
            |> MuellerMatrix
            |> Ok
        | Error e -> Error (MuellerNormalizationErr e)

    /// The walk both noisy observers share, one reading rule apart (spec 0050 S10.1): each configuration, in order, is
    /// disturbed, evaluated there and read, then filed against the NOMINAL configuration. The first error stops it.
    let private noisyObserveThrough
        (readWith : UniformDeviateProvider -> NoiseParam -> MuellerMatrix -> Result<MuellerMatrix, MuellerFitError>)
        (forward : ForwardModelProxy<'Parameters>)
        (truth : 'Parameters)
        (noise : NoiseParam)
        (seed : NoiseSeed)
        (configurations : MeasurementConfiguration list)
        : Result<MuellerObservation list, MuellerFitError> =
        let draws = UniformDeviateProvider.fromSeed seed
        let observe (nominal : MeasurementConfiguration) : Result<MuellerObservation, MuellerFitError> =
            forward.muellerOf (disturb draws noise nominal) truth
            |> Result.mapError ObservationForwardErr
            |> Result.bind (readWith draws noise)
            |> Result.map (fun measured -> { configuration = nominal; measured = measured })
        (Ok [], configurations)
        ||> List.fold (fun acc nominal -> acc |> Result.bind (fun os -> observe nominal |> Result.map (fun o -> o :: os)))
        |> Result.map List.rev

    /// One noisy realization of a whole experiment, each reading misread and filed against the nominal configuration,
    /// which is all a notebook holds.
    let noisyObserveWith
        (forward : ForwardModelProxy<'Parameters>)
        (truth : 'Parameters)
        (noise : NoiseParam)
        (seed : NoiseSeed)
        (configurations : MeasurementConfiguration list)
        : Result<MuellerObservation list, MuellerFitError> =
        noisyObserveThrough misread forward truth noise seed configurations

    /// `noisyObserveWith` for an absorbing sample: each reading keeps its absolute throughput through `misreadAbsolute`.
    let noisyObserveAbsoluteWith
        (forward : ForwardModelProxy<'Parameters>)
        (truth : 'Parameters)
        (noise : NoiseParam)
        (seed : NoiseSeed)
        (configurations : MeasurementConfiguration list)
        : Result<MuellerObservation list, MuellerFitError> =
        noisyObserveThrough misreadAbsolute forward truth noise seed configurations
