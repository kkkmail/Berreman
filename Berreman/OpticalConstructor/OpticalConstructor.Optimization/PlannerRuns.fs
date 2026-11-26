namespace OpticalConstructor.Optimization

open System.Globalization
open System.Threading
open Berreman.Constants
open Berreman.Solvers
open OpticalConstructor.Domain
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.CrystalPhysics
open OpticalConstructor.Domain.ItemModels
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.ItemEvaluation
open OpticalConstructor.Domain.MuellerInverse
open OpticalConstructor.Domain.Propagation
open OpticalConstructor.Domain.Studies
open OpticalConstructor.Domain.StudyForward
open OpticalConstructor.Domain.MeasurementPlanner
open OpticalConstructor.Optimization.MuellerFitting
open OpticalConstructor.Optimization.BenchNoise
open OpticalConstructor.Optimization.StudyEngine
open OpticalConstructor.Optimization.PlannerInformation

/// The Measurement Planner's run over a kit (spec 0050 S14.2).
module PlannerRuns =

    /// A number of a plan's candidates (spec 0050 S14.2).
    type PlanCandidateCount =
        | PlanCandidateCount of int

        member this.value : int = let (PlanCandidateCount count) = this in count

    /// How many of a plan's candidates are evaluated, of how many (spec 0050 S14.2).
    type PlanProgress =
        {
            evaluated : PlanCandidateCount
            total : PlanCandidateCount
        }

    /// Told after each candidate how many of how many are evaluated, and read before each whether to stop (spec 0050
    /// S14.2).
    [<ReferenceEquality>]
    type PlanObserver =
        {
            reportCandidate : PlanProgress -> unit
            cancellation : CancellationToken
        }

    type PlanCandidate with

        /// The sixteen configurations its experiment records, point for point: each incidence of R2's range, outermost,
        /// at each azimuth of R1's, as `ScanBlock.points` orders an R2 axis before an R1 axis (spec 0050 S14.2).
        member this.configurations : MeasurementConfiguration list =
            [
                for (SweptValue incidence) in PlanCandidate.incidenceRange.sweptValues do
                    for (SweptValue azimuth) in PlanCandidate.azimuthRange.sweptValues do
                        configurationOf
                            this.cut
                            this.plate.thickness
                            (incidence * 1.0<deg>)
                            (azimuth * 1.0<deg>)
                            this.observable
                            this.source.waveLength
            ]

    type MuellerFitError with

        /// The fit machinery's refusal as a planner reason, the forward's own where it has one (spec 0050 S14.2).
        member private this.plannerReason : string =
            match this with
            | ObservationForwardErr (ForwardSolveFailedErr (_, reason))
            | NonlinearFitErr (DidNotConvergeErr reason)
            | NonlinearFitErr (InvalidNonlinearRequestErr reason)
            | MuellerNormalizationErr (DegenerateRotationErr reason) -> reason
            | MuellerNormalizationErr (NonPositiveIntensityErr m00) ->
                let rendered = m00.ToString("R", CultureInfo.InvariantCulture)
                $"m00 = {rendered} is not above zero"
            | MuellerNormalizationErr (MismatchedObservationsErr counts) ->
                let (ObservationCount measured) = counts.measuredCount
                let (ObservationCount modelled) = counts.modelCount
                $"{measured} measured and {modelled} modelled observations"
            | UnknownQuantityErr name -> $"no ensemble row is named {name.value}"
            | UnknownAxisErr name -> $"no fitted parameter is named {name.value}"

    /// A target's crystal axis and the unknown state the study gives it (spec 0050 S14.2).
    type private PlanTarget =
        {
            axis : ParameterAxis<CrystalParameters>
            unknown : UnknownParameterData
        }

    type PlannerInputs with

        /// Each target's axis of the prior crystal and its unknown state in the opened copy, in target order; a target
        /// the crystal lacks or the copy does not leave unknown refuses (spec 0050 S14.2).
        member private this.plannedTargets : Result<PlanTarget list, PlannerError> =
            let material : OpenedMaterial = this.prior.material
            let axes : ParameterAxis<CrystalParameters> list = this.prior.crystal.parameterAxes

            this.targets
            |> List.map (fun (name : ParameterName) ->
                let axisOpt = axes |> List.tryFind (fun (axis : ParameterAxis<CrystalParameters>) -> axis.name = name)

                let stateOpt =
                    material.parameters
                    |> List.tryFind (fun (parameter : Parameter) -> parameter.name = name)
                    |> Option.map (fun (parameter : Parameter) -> parameter.state)

                match axisOpt, stateOpt with
                | Some axis, Some (UnknownParameter unknown) -> Ok { axis = axis; unknown = unknown }
                | Some _, Some (KnownParameter _)
                | Some _, Some (CalibratedParameter _) ->
                    Error (UnknownPlannerTargetErr $"{name.value} is not an unknown of '{material.name.value}'")
                | Some _, None
                | None, _ ->
                    Error (UnknownPlannerTargetErr $"{name.value} is not a parameter of '{material.name.value}'"))
            |> ItemEvaluation.sequenced

        /// The targets' axes in target order, centred at the prior crystal, each scaled to its unknown's search unit,
        /// the study engine's (spec 0050 S14.2).
        member this.scalingWith : Result<ParameterScaling<CrystalParameters>, PlannerError> =
            this.plannedTargets
            |> Result.map (fun (planned : PlanTarget list) ->
                let nominal : CrystalParameters = this.prior.crystal

                let scale : CrystalParameters =
                    planned
                    |> List.fold
                        (fun (held : CrystalParameters) (target : PlanTarget) ->
                            target.axis.writeValue target.unknown.bounds.searchUnit held)
                        nominal

                scalingAround (planned |> List.map (fun (target : PlanTarget) -> target.axis)) scale nominal)

        /// The forward the study engine solves a Mueller observation with (spec 0050 S14.2).
        member this.forwardWith (solverParameters : SolverParameters) : ForwardModelProxy<CrystalParameters> =
            createBerremanForward (fun (crystal : CrystalParameters) -> crystal.opticalProperties) solverParameters

        /// The candidate's rows at the prior crystal under the polarimeter's own noise budget; no polarimeter, or a
        /// budget its noise values refuse, is UnresolvedPlannerItemErr naming the detector, and an unsolved
        /// configuration PlannerForwardErr (spec 0050 S14.2).
        member this.candidateRowsWith
            (solverParameters : SolverParameters)
            (candidate : PlanCandidate)
            : Result<CandidateRows, PlannerError> =
            match this.polarimeterOpt with
            | None -> Error (UnresolvedPlannerItemErr "no detector of the kit records a full Mueller matrix")
            | Some polarimeter ->
                polarimeter.noise.benchNoise
                |> Result.mapError (fun (error : ItemEvaluationError) ->
                    UnresolvedPlannerItemErr $"{polarimeter.label.value}: {error.describe}")
                |> Result.bind (fun (noise : NoiseParam) ->
                    this.scalingWith
                    |> Result.bind (fun (scaling : ParameterScaling<CrystalParameters>) ->
                        candidateRowsOver
                            (this.forwardWith solverParameters)
                            scaling
                            this.prior.crystal
                            noise
                            candidate.configurations
                        |> Result.mapError (fun (error : MuellerFitError) ->
                            PlannerForwardErr $"{candidate.caption}: {error.plannerReason}")))

        /// One unit-noise row per target with a prior, in target order: its search unit over its σ at its own
        /// coordinate, the scaled derivative of the study engine's prior row (x − centre) / σ (spec 0050 S14.2).
        member this.priorRows : Result<CandidateRows list, PlannerError> =
            this.plannedTargets
            |> Result.map (fun (planned : PlanTarget list) ->
                planned
                |> List.indexed
                |> List.choose (fun (position : int, target : PlanTarget) ->
                    target.unknown.priorOpt
                    |> Option.map (fun (prior : ParameterPrior) ->
                        let (ParameterValue unit) = target.unknown.bounds.searchUnit
                        let (ParameterSigma sigma) = prior.sigma

                        let row : float[] =
                            Array.init planned.Length (fun (k : int) -> if k = position then unit / sigma else 0.0)

                        let rows : CandidateRows = { jacobian = ResidualJacobian [| row |]; noise = NoiseVariance 1.0 }
                        rows)))

    /// What a set of candidates and the kit's knowledge determine of the targets, and each target's bound at full rank
    /// (spec 0050 S14.2).
    type PlanStanding =
        {
            information : PlannerInformation
            boundsOpt : TargetBound list option
        }

    /// A proposed experiment with the standing the baseline and every step up to it reach (spec 0050 S14.2).
    type RankedPlanStep =
        {
            candidate : PlanCandidate
            standing : PlanStanding
        }

    /// A plan: the structural findings, the kit's baseline, its candidates already held, the open ones ranked, the
    /// plates without a candidate and the instrument findings (spec 0050 S14.2; 001 §M).
    type PlanReport =
        {
            study : StudyName
            material : MaterialVersionId
            criterion : PlanCriterion
            structural : RecoverabilityReport
            nominal : CrystalParameters
            baseline : PlanStanding
            inKit : PlanCandidate list
            ranked : RankedPlanStep list
            unplacedPlates : PlannerPlate list
            polarimeterOpt : PlannerDetector option
            instrument : InstrumentFinding list
        }

    type PlannerInputs with

        /// The structural report, the instrument findings, then each candidate's rows through `rowsOf` with progress
        /// and Cancel; the targets' priors and the candidates a kit experiment holds form the baseline, over which the
        /// open candidates are ranked greedily. The first error stops the run (spec 0050 S14.2; 001 §M, §P).
        member this.planWith
            (context : StudyForwardContext)
            (study : StudyName)
            (material : MaterialVersionId)
            (criterion : PlanCriterion)
            (rowsOf : PlanCandidate -> Result<CandidateRows, PlannerError>)
            (observer : PlanObserver)
            : Result<PlanReport, PlannerError> =
            let candidates : PlanCandidate list = this.candidates
            let total : PlanCandidateCount = PlanCandidateCount candidates.Length

            let rec evaluate
                (evaluated : PlanOption<PlanCandidate> list)
                (left : PlanCandidate list)
                : Result<PlanOption<PlanCandidate> list, PlannerError> =
                match left with
                | [] -> Ok (List.rev evaluated)
                | candidate :: rest ->
                    if observer.cancellation.IsCancellationRequested then
                        Error (PlanCancelledErr $"{evaluated.Length} of {candidates.Length} candidates were evaluated")
                    else
                        rowsOf candidate
                        |> Result.bind (fun (rows : CandidateRows) ->
                            let option : PlanOption<PlanCandidate> = { candidate = candidate; rows = rows }
                            let held : PlanOption<PlanCandidate> list = option :: evaluated
                            let progress : PlanProgress = { evaluated = PlanCandidateCount held.Length; total = total }
                            observer.reportCandidate progress
                            evaluate held rest)

            let coverageOf (option : PlanOption<PlanCandidate>) : PlanCoverage =
                option.candidate.coverageIn context.kit.setups context.kit.experiments

            let reportOver
                (structural : RecoverabilityReport)
                (instrument : InstrumentFinding list)
                (scaling : ParameterScaling<CrystalParameters>)
                (options : PlanOption<PlanCandidate> list)
                : Result<PlanReport, PlannerError> =
                let inKit : PlanOption<PlanCandidate> list =
                    options
                    |> List.choose (fun (option : PlanOption<PlanCandidate>) ->
                        match coverageOf option with
                        | CandidateInKit _ -> Some option
                        | CandidateNotInKit -> None)

                let openOptions : PlanOption<PlanCandidate> list =
                    options
                    |> List.choose (fun (option : PlanOption<PlanCandidate>) ->
                        match coverageOf option with
                        | CandidateInKit _ -> None
                        | CandidateNotInKit -> Some option)

                let standingOf (information : PlannerInformation) : PlanStanding =
                    { information = information; boundsOpt = information.boundsOver scaling }

                let stepOf (ranked : RankedOption<PlanCandidate>) : RankedPlanStep =
                    { candidate = ranked.option.candidate; standing = standingOf ranked.information }

                this.priorRows
                |> Result.map (fun (priors : CandidateRows list) ->
                    priors @ (inKit |> List.map (fun (option : PlanOption<PlanCandidate>) -> option.rows)))
                |> Result.bind (fun (baselineRows : CandidateRows list) ->
                    PlannerInformation.over this.targets baselineRows
                    |> Result.bind (fun (baseline : PlannerInformation) ->
                        PlannerInformation.rankWith criterion this.targets baselineRows openOptions
                        |> Result.map (fun (ranked : RankedOption<PlanCandidate> list) ->
                            let report : PlanReport =
                                {
                                    study = study
                                    material = material
                                    criterion = criterion
                                    structural = structural
                                    nominal = this.prior.crystal
                                    baseline = standingOf baseline
                                    inKit = inKit |> List.map (fun (held : PlanOption<PlanCandidate>) -> held.candidate)
                                    ranked = ranked |> List.map stepOf
                                    unplacedPlates = this.unplacedPlates
                                    polarimeterOpt = this.polarimeterOpt
                                    instrument = instrument
                                }

                            report)))

            this.structuralReport
            |> Result.bind (fun (structural : RecoverabilityReport) ->
                this.instrumentFindingsWith context.kit.items context.choiceOf context.materials
                |> Result.bind (fun (instrument : InstrumentFinding list) ->
                    this.scalingWith
                    |> Result.bind (fun (scaling : ParameterScaling<CrystalParameters>) ->
                        evaluate [] candidates |> Result.bind (reportOver structural instrument scaling))))
