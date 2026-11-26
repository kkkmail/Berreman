namespace OpticalConstructor.Optimization

open System
open System.Globalization
open Berreman.MathNetNumericsMath
open Berreman.Geometry
open OpticalConstructor.Domain.ItemModels
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.MuellerReconstruction
open OpticalConstructor.Domain.MuellerInverse
open OpticalConstructor.Domain.MeasurementPlanner
open OpticalConstructor.Optimization.OptimizationInterface
open OpticalConstructor.Optimization.FitUncertainty
open OpticalConstructor.Optimization.MuellerFitting
open OpticalConstructor.Optimization.BenchNoise
open OpticalConstructor.Optimization.FittedProblems

/// The Measurement Planner's numerical layer, the Fisher information of candidate experiments at a nominal point
/// (spec 0050 S14.2).
module PlannerInformation =

    /// One singular value of a residual Jacobian (spec 0050 S14.2).
    type SingularValue =
        | SingularValue of float

        member this.value : float = let (SingularValue singular) = this in singular

    /// A residual Jacobian's singular values in descending order, one per coordinate, and its right singular vectors
    /// (spec 0050 S14.2).
    [<Softellect.Sys.NumericCarrier("the singular values and right singular vectors of a residual Jacobian, read whole from the MathNet SVD")>]
    type SingularSpectrum =
        {
            values : float[]
            rightVectors : float[][]
        }

        /// MathNet's values padded with zeros to one per coordinate and the rows of its VT; with no row (or no
        /// coordinate) the zeros and the identity's rows (spec 0050 S14.2).
        [<Softellect.Sys.ElevationBoundary(nameof Softellect.Sys.InteropBoundary, "the MathNet SVD of a residual Jacobian, its right singular vectors read whole")>]
        static member ofRows (jacobian : ResidualJacobian) (coordinates : ParameterName list) : SingularSpectrum =
            let (ResidualJacobian rows) = jacobian
            let coordinateCount = List.length coordinates

            let square (entry : int -> int -> float) : float[][] =
                Array.init coordinateCount (fun (k : int) -> Array.init coordinateCount (entry k))

            if rows.Length = 0 || coordinateCount = 0 then
                {
                    values = Array.zeroCreate coordinateCount
                    rightVectors = square (fun (k : int) (p : int) -> if p = k then 1.0 else 0.0)
                }
            else
                let (RealMatrix a) = RealMatrix.create rows
                let svd = a.Svd(true)

                {
                    values = Array.init coordinateCount (fun (k : int) -> if k < svd.S.Count then svd.S.[k] else 0.0)
                    rightVectors = square (fun (k : int) (p : int) -> svd.VT.[k, p])
                }

    /// A combination of the targets as the planner writes it, `+0.71 a -0.71 b` (spec 0050 S14.2).
    type CombinationText =
        | CombinationText of string

        member this.value : string = let (CombinationText text) = this in text

    /// A combination of the targets and how strongly the candidates determine it (spec 0050 S14.2).
    type SingularDirection =
        {
            singularValue : SingularValue
            weights : ParameterWeights
        }

        /// Each coordinate weighing at least a tenth of the largest, in coordinate order, with two decimals
        /// (spec 0050 S14.2).
        member this.text (coordinates : ParameterName list) : CombinationText =
            let (ParameterWeights weights) = this.weights
            let largest = weights |> Array.fold (fun (held : float) (weight : float) -> max held (abs weight)) 0.0

            Seq.zip coordinates weights
            |> Seq.filter (fun (_, weight : float) -> abs weight >= 0.1 * largest)
            |> Seq.map (fun (name : ParameterName, weight : float) ->
                let sign = if weight < 0.0 then "-" else "+"
                let magnitude = (abs weight).ToString("F2", CultureInfo.InvariantCulture)
                $"{sign}{magnitude} {name.value}")
            |> String.concat " "
            |> CombinationText

    /// The log pseudo-determinant of the Fisher information over its identifiable combinations (spec 0050 S14.2).
    type InformationLogDeterminant =
        | InformationLogDeterminant of float

        member this.value : float = let (InformationLogDeterminant logDeterminant) = this in logDeterminant

    /// The trace of the Cramér–Rao covariance in the scaled coordinates, the sum of the targets' variances
    /// (spec 0050 S14.2).
    type CovarianceTrace =
        | CovarianceTrace of float

        member this.value : float = let (CovarianceTrace trace) = this in trace

    /// A target's Cramér–Rao bound in its physical units (spec 0050 S14.2).
    type TargetBound =
        {
            name : ParameterName
            sigma : ParameterSigma
        }

    /// The targets, every candidate's rows stacked in order and scaled to the reference noise, the first candidate's
    /// (spec 0050 S14.2).
    type PlannerInformation =
        {
            coordinates : ParameterName list
            rows : ResidualJacobian
            noise : NoiseVariance
        }

        /// The candidates' information over `coordinates`, each row times √(noise / its noise); a noiseless or
        /// misshapen candidate refuses it whole (spec 0050 S14.2).
        static member over
            (coordinates : ParameterName list)
            (candidates : CandidateRows list)
            : Result<PlannerInformation, PlannerError> =
            let count = List.length coordinates

            let refusalOf (position : int) (candidate : CandidateRows) : PlannerError option =
                let (NoiseVariance noise) = candidate.noise
                let (ResidualJacobian rows) = candidate.jacobian

                if not (Double.IsFinite noise) || noise <= 0.0 then
                    let noiseText = noise.ToString("R", CultureInfo.InvariantCulture)
                    Some (NoiselessPlannerCandidateErr $"candidate {position} has noise variance {noiseText}")
                else
                    rows
                    |> Array.tryFind (fun (row : float[]) -> row.Length <> count)
                    |> Option.map (fun (row : float[]) ->
                        PlannerShapeErr $"candidate {position} has a row of {row.Length} entries for {count} targets")

            let refusalOpt =
                candidates
                |> List.indexed
                |> List.tryPick (fun (index : int, candidate : CandidateRows) -> refusalOf (index + 1) candidate)

            match refusalOpt with
            | Some refusal -> Error refusal
            | None ->
                let noise =
                    match candidates with
                    | first :: _ -> first.noise
                    | [] -> NoiseVariance 1.0

                let (NoiseVariance reference) = noise

                let rows =
                    candidates
                    |> Array.ofList
                    |> Array.collect (fun (candidate : CandidateRows) ->
                        let (NoiseVariance own) = candidate.noise
                        let (ResidualJacobian candidateRows) = candidate.jacobian
                        let factor = sqrt (reference / own)
                        candidateRows |> Array.map (Array.map (fun (entry : float) -> entry * factor)))

                Ok { coordinates = coordinates; rows = ResidualJacobian rows; noise = noise }

        /// The number of identifiable combinations, step 394's rank of the stacked rows (spec 0050 S14.2).
        member this.rank : MatrixRank = this.rows.rank

        /// The stacked rows' singular values and right singular vectors (spec 0050 S14.2).
        member this.spectrum : SingularSpectrum = SingularSpectrum.ofRows this.rows this.coordinates

        /// Every direction of the spectrum, in its order (spec 0050 S14.2).
        member private this.directions : SingularDirection list =
            let spectrum = this.spectrum

            List.init spectrum.values.Length (fun (k : int) ->
                {
                    singularValue = SingularValue spectrum.values.[k]
                    weights = ParameterWeights spectrum.rightVectors.[k]
                })

        /// The combinations the candidates do not determine, below step 394's floor, in the spectrum's order
        /// (spec 0050 S14.2).
        member this.confounded : SingularDirection list =
            let (MatrixRank rank) = this.rank
            this.directions |> List.skip rank

        /// The identifiable combination the candidates determine worst (spec 0050 S14.2).
        member this.weakestOpt : SingularDirection option =
            match this.rank with
            | MatrixRank rank when rank >= 1 -> this.directions |> List.tryItem (rank - 1)
            | MatrixRank _ -> None

        /// The stacked rows' condition number at full rank, when finite (spec 0050 S14.2).
        member this.conditionOpt : ConditionNumber option =
            match this.coordinates with
            | [] -> None
            | _ when this.rank.value = List.length this.coordinates ->
                let condition = this.rows.conditionNumber
                if Double.IsFinite condition.value then Some condition else None
            | _ -> None

        /// The Cramér–Rao bound in the scaled coordinates at full rank: the σ a fit of the candidates reports when its
        /// residual level is their noise (spec 0050 S14.2).
        member this.scaledSigmasOpt : DesignVector option =
            if this.rank.value = List.length this.coordinates then
                let (NoiseVariance noise) = this.noise
                let (FitQuality.ParameterMatrix covariance) =
                    FitQuality.covarianceOf this.rows (FitQuality.ReducedChiSquared noise)

                Some (DesignVector [| for k in 0 .. covariance.Length - 1 -> sqrt covariance.[k].[k] |])
            else
                None

        /// Σ 2 ln s over the identifiable singular values, less rank · ln noise; 0 at rank 0 (spec 0050 S14.2).
        member this.logDeterminant : InformationLogDeterminant =
            let (MatrixRank rank) = this.rank
            let (NoiseVariance noise) = this.noise
            let identifiable = this.spectrum.values |> Array.take rank
            (identifiable |> Array.sumBy (fun (singular : float) -> 2.0 * log singular)) - float rank * log noise
            |> InformationLogDeterminant

        /// The sum of the targets' variances in the scaled coordinates, at full rank (spec 0050 S14.2).
        member this.traceOpt : CovarianceTrace option =
            this.scaledSigmasOpt
            |> Option.map (fun (DesignVector sigmas) ->
                sigmas |> Array.sumBy (fun (sigma : float) -> sigma * sigma) |> CovarianceTrace)

        /// Each target's bound in physical units, its scaled σ times its axis's scale, at full rank and with one axis
        /// per coordinate (spec 0050 S14.2).
        member this.boundsOver (scaling : ParameterScaling<'Parameters>) : TargetBound list option =
            match this.scaledSigmasOpt with
            | Some (DesignVector sigmas) when scaling.dimension = sigmas.Length ->
                List.zip this.coordinates scaling.axes
                |> List.mapi (fun (k : int) (name : ParameterName, axis : ParameterAxis<'Parameters>) ->
                    { name = name; sigma = ParameterSigma (sigmas.[k] * abs (axis.read scaling.scale)) })
                |> Some
            | Some _
            | None -> None

    type FitQuality.ReducedChiSquared with

        /// The noise variance a fit's reduced χ² estimates: the residual level of measured data read as a candidate's
        /// noise (spec 0050 S14.2).
        member this.noiseVariance : NoiseVariance = NoiseVariance this.value

    /// What the planner ranks candidates by: the most information or the smallest target variances; window state,
    /// never stored, so it carries no wire tag (spec 0050 S14.2).
    type PlanCriterion =
        | DeterminantPlanCriterion
        | TargetTracePlanCriterion

        /// The criterion as the Plan view names it (spec 0050 S14.2).
        member this.text : string =
            match this with
            | DeterminantPlanCriterion -> "D-optimal: most information"
            | TargetTracePlanCriterion -> "A-optimal: smallest target variances"

    /// One candidate and its information (spec 0050 S14.2).
    type PlanOption<'Candidate> =
        {
            candidate : 'Candidate
            rows : CandidateRows
        }

    /// An option with the information the baseline and every option ranked up to it hold (spec 0050 S14.2).
    type RankedOption<'Candidate> =
        {
            option : PlanOption<'Candidate>
            information : PlannerInformation
        }

    type PlannerInformation with

        /// Every option once, greedily: each round the option left that, joined to the baseline and the options ranked,
        /// gives the higher rank, then the larger log determinant (D) or the smaller trace at full rank (A); a tie keeps
        /// `options`' order, and the first refusal answers whole (spec 0050 S14.2; 001 §M).
        static member rankWith
            (criterion : PlanCriterion)
            (coordinates : ParameterName list)
            (baseline : CandidateRows list)
            (options : PlanOption<'Candidate> list)
            : Result<RankedOption<'Candidate> list, PlannerError> =
            // A trace exists only at full rank, so A falls back on the determinant below it (spec 0050 S14.2).
            let better (challenger : PlannerInformation) (held : PlannerInformation) : bool =
                let (MatrixRank challengerRank) = challenger.rank
                let (MatrixRank heldRank) = held.rank
                let (InformationLogDeterminant challengerLog) = challenger.logDeterminant
                let (InformationLogDeterminant heldLog) = held.logDeterminant

                if challengerRank <> heldRank then
                    challengerRank > heldRank
                else
                    match criterion, challenger.traceOpt, held.traceOpt with
                    | TargetTracePlanCriterion, Some (CovarianceTrace challengerTrace), Some (CovarianceTrace heldTrace) ->
                        challengerTrace < heldTrace
                    | TargetTracePlanCriterion, _, _
                    | DeterminantPlanCriterion, _, _ -> challengerLog > heldLog

            // The best of `left` joining `held`, beside its position in `left`.
            let rec bestFrom
                (held : CandidateRows list)
                (position : int)
                (bestOpt : (int * RankedOption<'Candidate>) option)
                (left : PlanOption<'Candidate> list)
                : Result<(int * RankedOption<'Candidate>) option, PlannerError> =
                match left with
                | [] -> Ok bestOpt
                | planOption :: rest ->
                    match PlannerInformation.over coordinates (held @ [ planOption.rows ]) with
                    | Error error -> Error error
                    | Ok information ->
                        let nextOpt =
                            match bestOpt with
                            | Some (_, best) when not (better information best.information) -> bestOpt
                            | Some _
                            | None -> Some (position, { option = planOption; information = information })

                        bestFrom held (position + 1) nextOpt rest

            let rec rankFrom
                (held : CandidateRows list)
                (ranked : RankedOption<'Candidate> list)
                (left : PlanOption<'Candidate> list)
                : Result<RankedOption<'Candidate> list, PlannerError> =
                match bestFrom held 0 None left with
                | Error error -> Error error
                | Ok None -> Ok (List.rev ranked)
                | Ok (Some (position, best)) ->
                    rankFrom (held @ [ best.option.rows ]) (best :: ranked) (List.removeAt position left)

            rankFrom baseline [] options

    /// The residual `MuellerFitting.residualFor` gives every entry when the forward cannot solve (step 336); the angle
    /// sensitivity's closure uses it to stay total (spec 0050 S14.2).
    [<Literal>]
    let private unsolvedPenalty : float = 1.0e3

    type NoiseParam with

        /// The mean over the throughput residual's entries of each reading's uniform variance u² / 3 plus the rotation's,
        /// turned through the entry's sensitivity to the incidence and the azimuth at `nominal`: the reduced χ² a fit of
        /// the configurations' noisy data expects (spec 0050 S14.2; 001 §M, §Q).
        member this.noiseVarianceOver
            (forward : ForwardModelProxy<'Parameters>)
            (nominal : 'Parameters)
            (configurations : MeasurementConfiguration list)
            : Result<NoiseVariance, MuellerFitError> =
            observeWith forward nominal configurations
            |> Result.map (fun (observations : MuellerObservation list) ->
                let normalizedCount = 15 * List.length observations
                let width = 16 * List.length observations
                let atNominal : ParameterScaling<'Parameters> = scalingAround [] nominal nominal

                // Readings with the sample turned by (Δi, Δa) radians, filed against the nominal configurations as
                // `noisyObserveWith` files them, against the model at `nominal`.
                let misalignedResidual (ScaledVector angles) : ResidualVector =
                    let incidence = Angle angles.[0]
                    let azimuth = Angle angles.[1]

                    let turned : ForwardModelProxy<'Parameters> =
                        {
                            muellerOf =
                                fun (configuration : MeasurementConfiguration) ->
                                    forward.muellerOf (configuration.misalignedBy incidence azimuth)
                        }

                    match observeWith turned nominal configurations with
                    | Ok readings -> throughputResidualFor forward readings atNominal (ScaledVector [||])
                    | Error _ -> Array.create width unsolvedPenalty |> ResidualVector

                let (ResidualJacobian sensitivity) = (ScaledVector [| 0.0; 0.0 |]).offsetJacobianOf misalignedResidual
                let (AngleUncertainty (Angle rotation)) = this.rotation
                let (ElementUncertainty element) = this.element
                let (ThroughputUncertainty throughput) = this.throughput

                let varianceOf (entry : int) (angleRow : float[]) : float =
                    let reading = if entry < normalizedCount then element * element / 3.0 else throughput * throughput / 3.0
                    reading + rotation * rotation / 3.0 * (angleRow |> Array.sumBy (fun (a : float) -> a * a))

                match width with
                | 0 -> NoiseVariance 0.0
                | _ -> (sensitivity |> Array.mapi varianceOf |> Array.sum) / float width |> NoiseVariance)

    /// A candidate's rows: the offset-frame Jacobian at `nominal`, in `scaling`'s units, of the throughput residual of its
    /// noiseless observations (the study residual's there, its readings entering as constants), and the budget's variance
    /// over its configurations (spec 0050 S14.2).
    let candidateRowsOver
        (forward : ForwardModelProxy<'Parameters>)
        (scaling : ParameterScaling<'Parameters>)
        (nominal : 'Parameters)
        (noise : NoiseParam)
        (configurations : MeasurementConfiguration list)
        : Result<CandidateRows, MuellerFitError> =
        observeWith forward nominal configurations
        |> Result.bind (fun (observations : MuellerObservation list) ->
            noise.noiseVarianceOver forward nominal configurations
            |> Result.map (fun (variance : NoiseVariance) ->
                let rows : CandidateRows =
                    {
                        jacobian = (toScaled scaling nominal).offsetJacobianOf (throughputResidualFor forward observations scaling)
                        noise = variance
                    }

                rows))
