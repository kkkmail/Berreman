namespace OpticalConstructor.Domain

open Berreman.Fields
open Berreman.Geometry
open OpticalConstructor.Domain.Library

/// Spec 0046 — what a detector reads at the end of the light's actual path. The host walks the beam from the
/// source to the detector (`BeamLayout`'s path for that detector) and hands this module the optical steps in
/// light order; nothing here knows about the table. The rules:
///
///   * the source's light is unpolarized natural light;
///   * every polarizer BEFORE the first sample prepares the input: the first one sets a unit-intensity
///     polarized state (`Propagation.behaviorInputStokes` — the convention every intensity in the app is
///     relative to), any further one acts through its Mueller matrix;
///   * a sample acts through the engine's Mueller matrix for the branch the path leaves it by (already
///     solved by the host at the run's wavelength, incidence and spin);
///   * every polarizer AFTER a sample is an analyzer and acts through its Mueller matrix;
///   * an intensity detector reads S₀, an ellipsometer reads Ψ and Δ of the arriving polarization state.
///
/// Pure: the host does the engine solves, so a sweep re-solves a sample only when its parameters change.
module ExperimentRun =

    /// One optical step on the way to a detector, in light order.
    type RunStep =
        /// A polarizer at its orientation (its R1).
        | PolarizerStep of behavior : PolarizerBehavior * orientation : Angle
        /// A sample, already solved: the engine's Mueller matrix for the branch the light leaves it by.
        | SampleStep of mueller : MuellerMatrix

    /// What a detector reads (spec 0046 — the detector decides what is captured).
    type Reading =
        | IntensityReading of intensity : float
        | EllipsometricReading of Propagation.PsiDelta

    /// The polarization state arriving at the detector after `steps`.
    let detectorStokes (steps : RunStep list) : StokesVector =
        let folder ((sv, beforeSample, prepared) : StokesVector * bool * bool) (step : RunStep) : StokesVector * bool * bool =
            match step with
            | PolarizerStep (behavior, orientation) when beforeSample && not prepared ->
                Propagation.behaviorInputStokes behavior orientation, beforeSample, true
            | PolarizerStep (behavior, orientation) ->
                Propagation.behaviorMueller behavior orientation * sv, beforeSample, prepared
            | SampleStep mueller -> mueller * sv, false, prepared
        let sv, _, _ = steps |> List.fold folder (Propagation.unpolarizedStokes, true, false)
        sv

    /// What a detector of `kind` reads after `steps`.
    let read (kind : DetectorKind) (steps : RunStep list) : Reading =
        let sv = detectorStokes steps
        match kind with
        | Intensity -> IntensityReading (Propagation.intensity sv)
        | Ellipsometer -> EllipsometricReading (Propagation.ellipsometerReadout sv)
