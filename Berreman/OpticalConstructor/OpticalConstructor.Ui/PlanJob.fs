/// A Measurement Planner run off the UI thread with per-candidate progress and Cancel, through `JobRunner`; a plan writes
/// no kit file, only its candidate cache (spec 0050 S14.2; 001 §P).
module OpticalConstructor.Ui.PlanJob

open System.Diagnostics
open System.Threading
open Avalonia.Threading
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.KitFolders
open OpticalConstructor.Domain.Studies
open OpticalConstructor.Domain.StudyForward
open OpticalConstructor.Domain.MeasurementPlanner
open OpticalConstructor.Optimization.PlannerInformation
open OpticalConstructor.Optimization.PlannerRuns
open OpticalConstructor.Storage.PlannerCacheStore
open OpticalConstructor.Ui.JobRunner

type PlanCandidateCount with

    /// The count as the job's steps, one step per candidate (spec 0050 S14.2).
    member private this.toJobStepCount : JobStepCount = JobStepCount this.value

/// How a started plan is cancelled; the plan reads the cancel before its next candidate (spec 0050 S14.2).
[<ReferenceEquality>]
type PlanJobHandle =
    {
        cancelPlan : unit -> unit
    }

/// Starts one plan and answers its handle; the plan's progress and its end reach the two callbacks the host was built
/// with (spec 0050 S14.2).
[<ReferenceEquality>]
type PlanJobHost =
    {
        startPlan : (PlanObserver -> Result<PlanReport, PlannerError>) -> PlanJobHandle
    }

    /// Runs each plan through `JobRunner.startBackground` with a cancellation source and a stopwatch of its own; each
    /// candidate evaluated, against the known total, and the plan's end go through `post`, and a plan that raised ends
    /// with PlanRaisedErr (spec 0050 S14.2).
    static member posting
        (post : (unit -> unit) -> unit)
        (progressed : JobProgress -> unit)
        (ended : Result<PlanReport, PlannerError> -> unit)
        : PlanJobHost =
        {
            startPlan =
                fun plan ->
                    // Never disposed: a Cancel may come after the plan ended, and a plain source holds no timer.
                    let source = new CancellationTokenSource ()
                    let stopwatch = Stopwatch.StartNew ()

                    let observer : PlanObserver =
                        {
                            reportCandidate =
                                fun (candidates : PlanProgress) ->
                                    let progress : JobProgress =
                                        {
                                            completed = candidates.evaluated.toJobStepCount
                                            total = Some candidates.total.toJobStepCount
                                            elapsed = JobElapsed stopwatch.Elapsed
                                        }

                                    post (fun () -> progressed progress)
                            cancellation = source.Token
                        }

                    startBackground
                        (fun () -> RanToCompletion [ plan observer ])
                        (fun finished ->
                            let planEnd =
                                match finished with
                                | Ok (RanToCompletion [ planned ])
                                | Ok (CancelledWith [ planned ]) -> planned
                                | Ok (RanToCompletion results)
                                | Ok (CancelledWith results) -> Error (PlanRaisedErr $"the plan job answered {results.Length} results instead of one")
                                | Error (JobRunExn e) -> Error (PlanRaisedErr e.Message)
                            post (fun () -> ended planEnd))
                    {
                        cancelPlan = fun () -> source.Cancel ()
                    }
        }

    /// `posting` through Avalonia's UI dispatcher, so a window's messages arrive on its own thread: the host a window
    /// composes (spec 0050 S14.2).
    static member onUiThread
        (progressed : JobProgress -> unit)
        (ended : Result<PlanReport, PlannerError> -> unit)
        : PlanJobHost =
        PlanJobHost.posting (fun action -> Dispatcher.UIThread.Post(fun () -> action ())) progressed ended

    /// Runs nothing and answers an inactive handle for scenes that run no plan (spec 0050 S14.2).
    static member inert : PlanJobHost =
        {
            startPlan = fun _ ->
                {
                    cancelPlan = ignore
                }
        }

/// The one plan a surface runs: the runner's model, whose committed result is the last completed plan's report, the
/// running plan's handle and the last plan's refusal (spec 0050 S14.2).
type PlanJobState =
    {
        job : JobModel<PlanReport>
        handleOpt : PlanJobHandle option
        errorOpt : PlannerError option
    }

    /// No plan runs, none has completed and none has failed (spec 0050 S14.2).
    static member idle : PlanJobState =
        {
            job = initModel
            handleOpt = None
            errorOpt = None
        }

/// What a plan is asked for: the study whose material it plans, that material and the criterion it ranks by (spec
/// 0050 S14.2).
type PlanRequest =
    {
        study : StudyName
        material : MaterialVersionId
        criterion : PlanCriterion
    }

/// What a surface's plan is told (spec 0050 S14.2; 001 §P).
type PlanMsg =
    /// Plan the request: the semantic message `PlanRunButton` and the tests send (spec 0050 S14.2; 001 §P).
    | RunPlan of request : PlanRequest
    /// Stop the running plan before its next candidate (spec 0050 S14.2).
    | CancelPlan
    /// The running plan evaluated one more candidate of its total (spec 0050 S14.2).
    | PlanProgressed of progress : JobProgress
    /// The running plan ended with its report or its refusal (spec 0050 S14.2).
    | PlanEnded of result : Result<PlanReport, PlannerError>

/// What a surface's plan runs on: the host starting it and the plan itself (spec 0050 S14.2).
[<ReferenceEquality>]
type PlanEnvironment =
    {
        host : PlanJobHost
        planWith : PlanRequest -> PlanObserver -> Result<PlanReport, PlannerError>
    }

    /// Each plan reads the named study of `forward`'s kit and plans its material, each candidate's rows read from
    /// `kit`'s cache under `.oc/planner/` or solved and cached there, a read-only kit caching nothing (spec 0050 S14.2).
    static member create (host : PlanJobHost) (kit : KitContext) (forward : StudyForwardContext) : PlanEnvironment =
        let planWith (request : PlanRequest) (observer : PlanObserver) : Result<PlanReport, PlannerError> =
            match forward.kit.studies |> List.tryFind (fun (study : Study) -> study.name = request.study) with
            | None -> Error (UnknownPlannerStudyErr request.study.value)
            | Some study ->
                study.plannerInputsWith forward.kit forward.choiceOf request.material
                |> Result.bind (fun (inputs : PlannerInputs) ->
                    let rowsOf (candidate : PlanCandidate) : Result<CandidateRows, PlannerError> =
                        inputs.cacheKeyWith forward.kit.items candidate
                        |> Result.bind (fun (key : PlannerCacheKey) ->
                            kit.cachedPlannerRows key (fun () -> inputs.candidateRowsWith forward.solverParameters candidate))

                    inputs.planWith forward request.study request.material request.criterion rowsOf observer)

        {
            host = host
            planWith = planWith
        }

type PlanMsg with

    /// One plan at a time: `RunPlan` while a plan runs changes nothing. A completed plan commits its report; a cancelled
    /// one keeps the committed report; any other refusal returns the job to Idle, the report kept, and is held (spec 0050
    /// S14.2).
    member this.updateWith (environment : PlanEnvironment) (state : PlanJobState) : PlanJobState =
        match this with
        | RunPlan request ->
            if state.job.isRunning then
                state
            else
                let handle = environment.host.startPlan (environment.planWith request)
                {
                    job = updateJob StartRequested state.job
                    handleOpt = Some handle
                    errorOpt = None
                }
        | CancelPlan ->
            match state.handleOpt, state.job.status with
            | Some handle, Running -> handle.cancelPlan ()
            | Some _, Idle
            | Some _, Completed
            | Some _, Cancelled
            | Some _, Failed _
            | None, _ -> ()
            state
        | PlanProgressed progress -> { state with job = updateJob (ProgressReported progress) state.job }
        | PlanEnded (Ok report) ->
            {
                job = updateJob (RunCompleted report) state.job
                handleOpt = None
                errorOpt = None
            }
        | PlanEnded (Error (PlanCancelledErr _)) -> { state with job = updateJob RunCancelled state.job; handleOpt = None }
        | PlanEnded (Error error) ->
            {
                job = { state.job with status = Idle; progress = None }
                handleOpt = None
                errorOpt = Some error
            }
