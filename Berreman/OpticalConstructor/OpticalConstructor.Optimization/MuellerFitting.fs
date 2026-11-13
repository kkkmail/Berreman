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
        | NonlinearFitErr of NonlinearSolverError
        /// A generated matrix could not be normalized.
        | MuellerNormalizationErr of InversionError
        /// No ensemble row has that name (`RecoveryEnsembles.rowFor`).
        | UnknownQuantityErr of ItemModels.ParameterName
        /// No fitted parameter has that name (`FitUncertainty.differenceWeights`).
        | UnknownAxisErr of ItemModels.ParameterName

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
    /// `throughputResidual`'s `ln(m₀₀ model) − ln(m₀₀ measured)`, total and of fixed length like it. The log term restores
    /// the isotropic absorption that normalization discards (specs/0044/.manual/018-absorbing-biaxial-inverse.md; spec
    /// 0050 S11.2).
    let throughputResidualFor
        (forward : ForwardModelProxy<'Parameters>)
        (observations : MuellerObservation list)
        (scaling : ParameterScaling<'Parameters>)
        : ScaledVector -> ResidualVector =
        let width = 16 * List.length observations
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
                            match throughputResidual o.measured model with
                            | Ok entry -> entry.value
                            | Error _ -> [| 1.0e3 |])
                        |> Array.concat
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

    /// The box the optimizer may search, as a half-width in SCALED units. A box that excludes the truth yields a
    /// converged-looking fit pinned against a bound, so a far start guess needs a wider one.
    type SearchBox =
        | SearchBox of double

        member this.halfWidth = let (SearchBox h) = this in h

    /// What a fit produced: the recovered constants, the solver's account of the run, the scaling the vector lives
    /// in, and the residual closure a Jacobian is taken of.
    [<ReferenceEquality>]
    type MuellerFitData<'Parameters> =
        {
            recovered : 'Parameters
            solution : NonlinearSolution
            scaling : ParameterScaling<'Parameters>
            residual : ScaledVector -> ResidualVector
        }

        /// Σ residualᵢ² at the solution.
        member this.chiSquared : double = this.solution.finalResiduals.value |> Array.sumBy (fun r -> r * r)

        /// The root-mean-square residual entry at the solution — the noise level, when there is noise.
        member this.rmsResidual : double =
            sqrt (this.chiSquared / float (max 1 this.solution.finalResiduals.value.Length))

        /// FitQuality's report at the solution, in the scaled space the fit searches (spec 0050 S10.1).
        member this.report : FitQuality.FitReport =
            let (ScaledVector solution) = this.solution.solution
            FitQuality.reportFrom
                (fun (OptimizationInterface.DesignVector v) -> this.residual (ScaledVector v))
                (OptimizationInterface.DesignVector solution)
                this.solution.finalResiduals

    /// Minimize a residual closure by ALGLIB Levenberg–Marquardt from the zero scaled vector, within the box.
    let fitWithResidual
        (box : SearchBox)
        (residual : ScaledVector -> ResidualVector)
        (scaling : ParameterScaling<'Parameters>)
        : Result<MuellerFitData<'Parameters>, MuellerFitError> =
        let solver = MuellerInverseSolver.createAlglibLevenbergMarquardt ()
        let request =
            {
                residual = residual
                initial = Array.zeroCreate scaling.dimension |> ScaledVector
                lowerBounds = Array.create scaling.dimension -box.halfWidth |> ScaledVector
                upperBounds = Array.create scaling.dimension box.halfWidth |> ScaledVector
                maxIterations = IterationCount 400
                epsX = StepTolerance 1.0e-12
            }
        match solver.solveNonlinearLeastSquares request with
        | Ok solution ->
            Ok
                {
                    recovered = ofScaled scaling solution.solution
                    solution = solution
                    scaling = scaling
                    residual = residual
                }
        | Error e -> Error (NonlinearFitErr e)

    /// Fit a TRANSPARENT sample's observations: normalized Mueller elements only (`residualFor`).
    let fitObservations
        (forward : ForwardModelProxy<'Parameters>)
        (box : SearchBox)
        (observations : MuellerObservation list)
        (scaling : ParameterScaling<'Parameters>)
        : Result<MuellerFitData<'Parameters>, MuellerFitError> =
        fitWithResidual box (residualFor forward observations scaling) scaling

    /// Fit an ABSORBING sample's observations: normalized elements plus absolute throughput (`throughputResidualFor`),
    /// whose log term is mandatory because normalization alone cannot see the isotropic absorption.
    let fitAbsorbingObservations
        (forward : ForwardModelProxy<'Parameters>)
        (box : SearchBox)
        (observations : MuellerObservation list)
        (scaling : ParameterScaling<'Parameters>)
        : Result<MuellerFitData<'Parameters>, MuellerFitError> =
        fitWithResidual box (throughputResidualFor forward observations scaling) scaling

    /// A recovered quantity's value in its own physical units (an index, a gyration or ε″ component, a birefringence).
    type QuantityValue =
        | QuantityValue of double

        member this.value = let (QuantityValue v) = this in v

    /// A dimensionless error against a known true value, `|actual − expected| / |expected|`. Elevated so that an
    /// absolute difference read as a relative one, or the reverse, is a compile error rather than a passing band.
    type RelativeError =
        | RelativeErr of double

        /// The error as a fraction.
        member this.value = let (RelativeErr e) = this in e

        /// The error of a recovered value against the truth it should reproduce.
        static member between (actual : QuantityValue) (expected : QuantityValue) : RelativeError =
            abs (actual.value - expected.value) / abs expected.value |> RelativeErr

    /// The relative error of every fitted parameter against the truth, paired with its name, in axis order.
    let recoveryErrors
        (scaling : ParameterScaling<'Parameters>)
        (truth : 'Parameters)
        (recovered : 'Parameters)
        : (ItemModels.ParameterName * RelativeError) list =
        scaling.axes
        |> List.map (fun a -> a.name, RelativeError.between (QuantityValue (a.read recovered)) (QuantityValue (a.read truth)))
