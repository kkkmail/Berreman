/// A study fit off the UI thread with per-iteration progress and Cancel, through `JobRunner`; its fit run is written on
/// completion only (spec 0050 S11.3; 001 §N, §P).
module OpticalConstructor.Ui.StudyFitJob

open System.Diagnostics
open System.Threading
open Avalonia.Threading
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.KitFolders
open OpticalConstructor.Domain.MuellerInverse
open OpticalConstructor.Domain.MuellerReconstruction
open OpticalConstructor.Domain.StudyForward
open OpticalConstructor.Domain.FitRuns
open OpticalConstructor.Optimization.OptimizationInterface
open OpticalConstructor.Optimization.BenchNoise
open OpticalConstructor.Optimization.StudyEngine
open OpticalConstructor.Storage.KitContentFiles
open OpticalConstructor.Ui.JobRunner

/// How a started fit is cancelled; the fit reads the cancel after its next iteration (spec 0050 S11.3).
[<ReferenceEquality>]
type StudyFitJobHandle =
    {
        cancelStudyFit : unit -> unit
    }

/// Starts one fit and answers its handle; the fit's progress and its end reach the two callbacks the host was built
/// with (spec 0050 S11.3).
[<ReferenceEquality>]
type StudyFitJobHost =
    {
        startStudyFit : (IterationWatch -> Result<FitRun, StudyFitError>) -> StudyFitJobHandle
    }

    /// One fit's observer: each iteration count, an iteration count with no known total timed from the fit's start, goes
    /// to `report`, and the fit reads `source` after each iteration (spec 0050 S11.3).
    static member private observerOf (source : CancellationTokenSource) (report : JobProgress -> unit) : OptimizerIterationObserver =
        let stopwatch = Stopwatch.StartNew ()
        {
            reportIteration =
                fun (IterationCount iterations) ->
                    let completed = JobStepCount iterations
                    let elapsed = JobElapsed stopwatch.Elapsed
                    report { completed = completed; total = None; elapsed = elapsed }
            cancellation = source.Token
        }

    /// Runs each fit through `JobRunner.startBackground` on a thread-pool worker, with a cancellation source of its own;
    /// its progress and its end go through `post`, and a fit that raised ends with FitRunRaisedErr (spec 0050 S11.3).
    static member posting
        (post : (unit -> unit) -> unit)
        (progressed : JobProgress -> unit)
        (ended : Result<FitRun, StudyFitError> -> unit)
        : StudyFitJobHost =
        {
            startStudyFit =
                fun fit ->
                    // Never disposed: a Cancel may come after the fit ended, and a plain source holds no timer.
                    let source = new CancellationTokenSource ()
                    let observer = StudyFitJobHost.observerOf source (fun progress -> post (fun () -> progressed progress))
                    startBackground
                        (fun () -> RanToCompletion [ fit (WatchedIterations observer) ])
                        (fun finished ->
                            let fitEnd =
                                match finished with
                                | Ok (RanToCompletion [ fitted ])
                                | Ok (CancelledWith [ fitted ]) -> fitted
                                | Ok (RanToCompletion results)
                                | Ok (CancelledWith results) -> Error (FitRunRaisedErr $"the fit job answered {results.Length} results instead of one")
                                | Error (JobRunExn e) -> Error (FitRunRaisedErr e.Message)
                            post (fun () -> ended fitEnd))
                    {
                        cancelStudyFit = fun () -> source.Cancel ()
                    }
        }

    /// `posting` through Avalonia's UI dispatcher, so a window's messages arrive on its own thread: the host a window
    /// composes (spec 0050 S11.3).
    static member onUiThread
        (progressed : JobProgress -> unit)
        (ended : Result<FitRun, StudyFitError> -> unit)
        : StudyFitJobHost =
        StudyFitJobHost.posting (fun action -> Dispatcher.UIThread.Post(fun () -> action ())) progressed ended

    /// Runs nothing and answers an inactive handle for scenes that run no fit (spec 0050 S11.5).
    static member inert : StudyFitJobHost =
        {
            startStudyFit = fun _ ->
                {
                    cancelStudyFit = ignore
                }
        }

/// The one fit a surface runs: the runner's model, whose committed result is the last fit run written, the running
/// fit's handle and the last fit's refusal (spec 0050 S11.3).
type StudyFitJobState =
    {
        job : JobModel<FitRunName>
        handleOpt : StudyFitJobHandle option
        errorOpt : StudyFitError option
    }

    /// No fit runs, none has been written and none has failed (spec 0050 S11.3).
    static member idle : StudyFitJobState =
        {
            job = initModel
            handleOpt = None
            errorOpt = None
        }

/// What a surface's fit is told (spec 0050 S11.3; 001 §P).
type StudyFitMsg =
    /// Fit the study: the semantic message the Run joint and Run stage buttons and the tests send (spec 0050 S11.3).
    | RunFit of request : StudyFitRequest
    /// Stop the running fit after its next iteration (spec 0050 S11.3).
    | CancelStudyFit
    /// The running fit made one more iteration (spec 0050 S11.3).
    | StudyFitProgressed of progress : JobProgress
    /// The running fit ended with its fit run or its refusal, the run not yet written (spec 0050 S11.3).
    | StudyFitEnded of result : Result<FitRun, StudyFitError>

/// What a surface's fit runs on: the host starting it, the fit itself and the kit its fit run is written to (spec 0050
/// S11.3).
[<ReferenceEquality>]
type StudyFitEnvironment =
    {
        host : StudyFitJobHost
        fitWith : StudyFitRequest -> IterationWatch -> Result<FitRun, StudyFitError>
        kit : KitProxy
    }

    /// Each fit reads the study's fit runs from `kit` and runs `StudyFitParam.runFit` over the real SVD with one fixed
    /// seed, so a study refits reproducibly and every run records its starts (spec 0050 S11.3).
    static member create (host : StudyFitJobHost) (kit : KitProxy) (forward : StudyForwardContext) (clock : ClockProvider) : StudyFitEnvironment =
        let fitWith (request : StudyFitRequest) (watch : IterationWatch) : Result<FitRun, StudyFitError> =
            kit.readFitRuns request.study
            |> Result.mapError FitRunKitErr
            |> Result.bind (fun fitRuns ->
                let seed = NoiseSeed 1
                let fit : StudyFitParam =
                    {
                        forward = forward
                        clock = clock
                        linearSolver = createMathNetSvd ()
                        deviates = UniformDeviateProvider.fromSeed seed
                        fitRuns = fitRuns
                    }
                fit.runFit request watch)
        {
            host = host
            fitWith = fitWith
            kit = kit
        }

type StudyFitMsg with

    /// One fit at a time: `RunFit` while a fit runs changes nothing. The fit run is written here, once, when its fit
    /// completes; a cancelled or refused fit writes nothing and keeps the committed fit run (spec 0050 S11.3).
    member this.updateWith (environment : StudyFitEnvironment) (state : StudyFitJobState) : StudyFitJobState =
        let refusedWith (error : StudyFitError) : StudyFitJobState =
            {
                job = { state.job with status = Idle; progress = None }
                handleOpt = None
                errorOpt = Some error
            }

        match this with
        | RunFit request ->
            if state.job.isRunning then
                state
            else
                let handle = environment.host.startStudyFit (environment.fitWith request)
                {
                    job = updateJob StartRequested state.job
                    handleOpt = Some handle
                    errorOpt = None
                }
        | CancelStudyFit ->
            match state.handleOpt, state.job.status with
            | Some handle, Running -> handle.cancelStudyFit ()
            | Some _, Idle
            | Some _, Completed
            | Some _, Cancelled
            | Some _, Failed _
            | None, _ -> ()
            state
        | StudyFitProgressed progress -> { state with job = updateJob (ProgressReported progress) state.job }
        | StudyFitEnded (Ok run) ->
            match environment.kit.writeFitRun run with
            | Ok () ->
                {
                    job = updateJob (RunCompleted run.name) state.job
                    handleOpt = None
                    errorOpt = None
                }
            | Error error -> refusedWith (FitRunKitErr error)
        | StudyFitEnded (Error (FitCancelledErr _)) -> { state with job = updateJob RunCancelled state.job; handleOpt = None }
        | StudyFitEnded (Error error) -> refusedWith error
