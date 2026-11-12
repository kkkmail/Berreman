namespace OpticalConstructor.Optimization

open Berreman.Constants
open Berreman.Geometry
open Berreman.Fields
open Berreman.Media
open OpticalConstructor.Domain
open OpticalConstructor.Domain.MuellerInverse

/// The Mueller-matrix fit machinery the inverse-problem suites, the study engine and the Planner share, moved from
/// `BerremanTests/InverseFitHarness.fs` by spec 0050 S10.1 (spec 0044 manual tasks 010-016).
module MuellerFitting =

    /// A typed failure of generating observations or fitting them, never a throw.
    type MuellerFitError =
        | ObservationForwardErr of ForwardModelError

    /// One measurement configuration, its incidence and sample azimuth given in degrees.
    /// `IncidenceAngle.create` folds the incidence into [0, 90) deg, so a negative one is not a tilt the other way.
    let configurationOf
        (cut : SampleCut)
        (thickness : Thickness)
        (incidence : float<deg>)
        (azimuth : float<deg>)
        (observable : Observable)
        (waveLength : WaveLength)
        : MeasurementConfiguration =
        {
            cut = cut
            thickness = thickness
            incidenceAngle = Angle.degree incidence |> IncidenceAngle.create
            azimuth = SampleAzimuth.degree azimuth
            observable = observable
            waveLength = waveLength
        }

    /// The measured data a configuration set records from known truth, in configuration order. The first forward
    /// failure is the error and nothing after it is evaluated: a failure on the ground truth is not a datum.
    let observeWith
        (forward : ForwardModelProxy<'Parameters>)
        (truth : 'Parameters)
        (configurations : MeasurementConfiguration list)
        : Result<MuellerObservation list, MuellerFitError> =
        (Ok [], configurations)
        ||> List.fold (fun acc c ->
            acc |> Result.bind (fun os -> forward.muellerOf c truth |> Result.map (fun m -> { configuration = c; measured = m } :: os)))
        |> Result.map List.rev
        |> Result.mapError ObservationForwardErr

    /// The residual closure the optimizer drives: 15 normalized Mueller element differences per observation.
    /// Total and of fixed length: the optimizer hands it parameters the forward model cannot solve, so a failure is a
    /// finite 1.0e3 penalty of full width, never a throw or a short vector that would change the problem minimized.
    let residualFor
        (forward : ForwardModelProxy<'Parameters>)
        (observations : MuellerObservation list)
        (scaling : ParameterScaling<'Parameters>)
        : ScaledVector -> ResidualVector =
        let width = 15 * List.length observations
        fun (v : ScaledVector) ->
            match forwardModels forward (ofScaled scaling v) observations with
            | Error _ -> Array.create width 1.0e3 |> ResidualVector
            | Ok models ->
                match residualVector observations models with
                | Ok r -> r
                | Error _ -> Array.create width 1.0e3 |> ResidualVector

    /// The residual closure for an ABSORBING sample: `residualFor`'s 15 per observation plus a 16th,
    /// `ln(m₀₀ model) − ln(m₀₀ measured)`, total and of fixed length like it. The log term restores the isotropic
    /// absorption that normalization discards (specs/0044/.manual/018-absorbing-biaxial-inverse.md).
    let throughputResidualFor
        (forward : ForwardModelProxy<'Parameters>)
        (observations : MuellerObservation list)
        (scaling : ParameterScaling<'Parameters>)
        : ScaledVector -> ResidualVector =
        let width = 16 * List.length observations
        let logThroughput (m : MuellerMatrix) =
            let m00 = m.element 0 0
            if m00 <= 0.0 || not (System.Double.IsFinite m00) then None else Some (log m00)
        fun (v : ScaledVector) ->
            match forwardModels forward (ofScaled scaling v) observations with
            | Error _ -> Array.create width 1.0e3 |> ResidualVector
            | Ok models ->
                match residualVector observations models with
                | Error _ -> Array.create width 1.0e3 |> ResidualVector
                | Ok (ResidualVector normalized) ->
                    let throughput =
                        List.zip observations models
                        |> List.map (fun (o, model) ->
                            match logThroughput o.measured, logThroughput model with
                            | Some measured, Some predicted -> predicted - measured
                            | _ -> 1.0e3)
                        |> Array.ofList
                    Array.append normalized throughput |> ResidualVector

    /// The residual Jacobian of a typed residual closure at a scaled point, by FitQuality's finite differences.
    let residualJacobianAt (residual : ScaledVector -> ResidualVector) (point : ScaledVector) : ResidualJacobian =
        let (ScaledVector x) = point
        FitQuality.residualJacobian (fun (OptimizationInterface.DesignVector v) -> residual (ScaledVector v)) (OptimizationInterface.DesignVector x)

    /// The dimensionless fit space centred on a start guess: the initial scaled vector is zero, and `scale` is what one
    /// unit of each coordinate means physically. ALGLIB's Levenberg–Marquardt differentiates with a FIXED ABSOLUTE
    /// step of 1e-6, so pick `scale` to keep that step linear in every coordinate (1e-3 per index, 1e-5 per gyration).
    let scalingAround
        (axes : ParameterAxis<'Parameters> list)
        (scale : 'Parameters)
        (centre : 'Parameters)
        : ParameterScaling<'Parameters> =
        {
            axes = axes
            centre = centre
            scale = scale
        }
