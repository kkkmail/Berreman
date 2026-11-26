namespace OpticalConstructor.Tests

open System
open System.Threading
open Xunit
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.KitFolders
open OpticalConstructor.Domain.MuellerInverse
open OpticalConstructor.Domain.Studies
open OpticalConstructor.Domain.StudyForward
open OpticalConstructor.Domain.MeasurementPlanner
open OpticalConstructor.Storage.KitFolderStore
open OpticalConstructor.Optimization.PlannerInformation
open OpticalConstructor.Optimization.PlannerRuns
open OpticalConstructor.Ui.JobRunner
open OpticalConstructor.Ui.PlanJob
open PlannerInputsTests
open PlanRunTests

/// The Measurement Planner's run hosted on `JobRunner` through `PlanMsg.updateWith`: one plan at a time off the UI
/// thread, per-candidate progress against the total, Cancel keeping the committed report, and the plan environment
/// planning a kit's study through its candidate cache (spec 0050 S14.2; 001 §P).
module PlanJobTests =

    /// One stub plan: what the host runs on its worker, given the plan's observer.
    type private StubPlan = PlanObserver -> Result<PlanReport, PlannerError>

    /// A surface's one plan: its messages handled one at a time under one lock, as Elmish dispatches them, so a worker's
    /// post never races the `RunPlan` that started it. `ended` is set after each `PlanEnded` is handled.
    [<ReferenceEquality>]
    type private PlanJobSurface =
        {
            send : PlanMsg -> unit
            current : unit -> PlanJobState
            told : unit -> JobProgress list
            ended : ManualResetEventSlim
            starts : unit -> int
            planCalls : unit -> int
        }

    let private tenSeconds : TimeSpan = TimeSpan.FromSeconds 10.0

    let private reportValue (result : Result<PlanReport, PlannerError>) : PlanReport =
        result |> Result.defaultWith (fun error -> failwith error.describe)

    let private kitValue<'Value> (result : Result<'Value, KitError>) : 'Value =
        result |> Result.defaultWith (fun error -> failwith error.describe)

    /// `PlanRunTests`' study over a kit of Laser, Polarimeter and QZ-Z, with no setup and no experiment.
    let private forward : StudyForwardContext = contextOver planStudy [ laser; polarimeter; qzZ ] [] []

    let private request : PlanRequest =
        {
            study = planStudy.name
            material = crystalVersion
            criterion = DeterminantPlanCriterion
        }

    /// The report the stubs answer: the study's D-optimal plan over one literal row per candidate, no solver run.
    let private stubReport : PlanReport =
        let rows : CandidateRows = { jacobian = ResidualJacobian [| [| 1.0; 0.0 |] |]; noise = NoiseVariance 1.0e-4 }
        let unread : PlanObserver = { reportCandidate = ignore; cancellation = CancellationToken.None }
        let inputs = planStudy.plannerInputsWith forward.kit choiceOf crystalVersion |> inputsValue
        inputs.planWith forward request.study request.material request.criterion (fun _ -> Ok rows) unread |> reportValue

    /// A surface whose host is `posting` with an immediate `post`; its n-th plan is `plans`' n-th stub. Every start of
    /// the host is counted, as is every call of the plan.
    let private surfaceOver (plans : StubPlan list) : PlanJobSurface =
        let sync = obj ()
        let state = ref PlanJobState.idle
        let told = ResizeArray<JobProgress> ()
        let ended = new ManualResetEventSlim (false)
        let starts = ref 0
        let calls = ref 0
        let environmentCell : PlanEnvironment option ref = ref None

        let send (message : PlanMsg) : unit =
            lock sync (fun () ->
                match environmentCell.Value with
                | Some environment -> state.Value <- message.updateWith environment state.Value
                | None -> failwith "the surface was sent a message before its environment was built")

        let posting =
            PlanJobHost.posting
                (fun action -> action ())
                (fun progress ->
                    lock sync (fun () -> told.Add progress)
                    send (PlanProgressed progress))
                (fun result ->
                    send (PlanEnded result)
                    ended.Set ())

        let host : PlanJobHost =
            {
                startPlan =
                    fun plan ->
                        Interlocked.Increment starts |> ignore
                        posting.startPlan plan
            }

        let planWith (_ : PlanRequest) (observer : PlanObserver) : Result<PlanReport, PlannerError> =
            let call = Interlocked.Increment calls
            match List.tryItem (call - 1) plans with
            | Some plan -> plan observer
            | None -> Error (PlannerForwardErr $"plan {call} was not expected")

        let environment : PlanEnvironment =
            {
                host = host
                planWith = planWith
            }

        environmentCell.Value <- Some environment

        {
            send = send
            current = fun () -> lock sync (fun () -> state.Value)
            told = fun () -> lock sync (fun () -> List.ofSeq told)
            ended = ended
            starts = fun () -> starts.Value
            planCalls = fun () -> calls.Value
        }

    /// Waits, never sleeps, until the surface has handled its plan's end.
    let private awaitEnd (surface : PlanJobSurface) : unit =
        Assert.True(surface.ended.Wait(tenSeconds), "the plan did not end within ten seconds")

    /// Reports 1 of 2 and 2 of 2 through its observer, then answers `report`.
    let private reportingTwoThen (report : PlanReport) (observer : PlanObserver) : Result<PlanReport, PlannerError> =
        for evaluated in 1 .. 2 do
            observer.reportCandidate { evaluated = PlanCandidateCount evaluated; total = PlanCandidateCount 2 }

        Ok report

    /// Waits until its observer's cancellation is requested, then answers `PlanCancelledErr`, as a plan does before its
    /// next candidate.
    let private untilCancelled (observer : PlanObserver) : Result<PlanReport, PlannerError> =
        if observer.cancellation.WaitHandle.WaitOne(tenSeconds) then
            Error (PlanCancelledErr "the stub stopped on its observer's cancellation")
        else
            Error (PlannerForwardErr "the stub's observer was not cancelled within ten seconds")

    /// The kit's `.oc/planner/` cache files.
    let private cacheFilesOf (folder : InMemoryKitFolder) : string list =
        folder.paths ()
        |> List.map (fun path -> path.value)
        |> List.filter (fun path -> path.EndsWith(".candidate-json", StringComparison.Ordinal))

    let private pathsOf (folder : InMemoryKitFolder) : string list = folder.paths () |> List.map (fun path -> path.value)

    [<Fact>]
    let ``RunPlan reports each candidate against the total and commits the report once the plan completes`` () : unit =
        let surface = surfaceOver [ reportingTwoThen stubReport ]

        surface.send (RunPlan request)
        awaitEnd surface

        let state = surface.current ()
        Assert.Equal<JobStatus>(Completed, state.job.status)
        Assert.Equal<PlanReport option>(Some stubReport, state.job.committedResults)
        Assert.True(Option.isNone state.handleOpt, "a completed plan holds no handle")
        Assert.True(Option.isNone state.errorOpt, "a completed plan holds no error")

        // Each candidate was told in order, against the plan's known total.
        let told = surface.told ()
        Assert.Equal<JobStepCount list>([ JobStepCount 1; JobStepCount 2 ], told |> List.map (fun progress -> progress.completed))
        Assert.All(told, fun progress -> Assert.Equal<JobStepCount option>(Some (JobStepCount 2), progress.total))

        match state.job.progressText with
        | Some text -> Assert.StartsWith("2 / 2", text)
        | None -> Assert.Fail("the completed plan shows no progress text")

        Assert.Equal(1, surface.starts ())
        Assert.Equal(1, surface.planCalls ())

    [<Fact>]
    let ``a cancelled plan keeps the last committed report`` () : unit =
        let surface = surfaceOver [ reportingTwoThen stubReport; untilCancelled ]

        surface.send (RunPlan request)
        awaitEnd surface
        Assert.Equal<PlanReport option>(Some stubReport, (surface.current ()).job.committedResults)

        surface.ended.Reset ()
        surface.send (RunPlan { request with criterion = TargetTracePlanCriterion })
        Assert.Equal<JobStatus>(Running, (surface.current ()).job.status)
        surface.send CancelPlan
        awaitEnd surface

        let state = surface.current ()
        Assert.Equal<JobStatus>(Cancelled, state.job.status)
        Assert.Equal<PlanReport option>(Some stubReport, state.job.committedResults)
        Assert.True(Option.isNone state.handleOpt, "a cancelled plan holds no handle")
        Assert.True(Option.isNone state.errorOpt, "a cancel is not an error")
        Assert.Equal(2, surface.starts ())
        Assert.Equal(2, surface.planCalls ())

    [<Fact>]
    let ``a plan that raises ends with PlanRaisedErr carrying its message`` () : unit =
        let raising : StubPlan = fun _ -> raise (InvalidOperationException "solver gone")
        let surface = surfaceOver [ raising ]

        surface.send (RunPlan request)
        awaitEnd surface

        let state = surface.current ()

        match state.errorOpt with
        | Some (PlanRaisedErr reason as error) ->
            Assert.Contains("solver gone", reason)
            Assert.StartsWith("the plan raised: ", error.describe)
        | Some error -> Assert.Fail($"expected PlanRaisedErr, got {error.describe}")
        | None -> Assert.Fail("a plan that raised holds no error")

        Assert.Equal<JobStatus>(Idle, state.job.status)
        Assert.Equal<PlanReport option>(None, state.job.committedResults)
        Assert.True(Option.isNone state.handleOpt, "a plan that raised holds no handle")

    [<Fact>]
    let ``RunPlan while a plan runs starts nothing`` () : unit =
        use release = new ManualResetEventSlim (false)

        let held : StubPlan =
            fun _ ->
                if release.Wait(tenSeconds) then
                    Ok stubReport
                else
                    Error (PlannerForwardErr "the held plan was not released within ten seconds")

        let surface = surfaceOver [ held ]
        surface.send (RunPlan request)
        let running = surface.current ()
        Assert.Equal<JobStatus>(Running, running.job.status)

        surface.send (RunPlan { request with criterion = TargetTracePlanCriterion })
        let still = surface.current ()
        Assert.Equal<JobStatus>(Running, still.job.status)

        match running.handleOpt, still.handleOpt with
        | Some first, Some kept -> Assert.True(obj.ReferenceEquals(first, kept), "the second RunPlan replaced the running plan's handle")
        | _ -> Assert.Fail("the running plan holds no handle")

        Assert.Equal(1, surface.starts ())

        release.Set ()
        awaitEnd surface

        let state = surface.current ()
        Assert.Equal<JobStatus>(Completed, state.job.status)
        Assert.Equal<PlanReport option>(Some stubReport, state.job.committedResults)
        Assert.Equal(1, surface.starts ())
        Assert.Equal(1, surface.planCalls ())

    [<Fact>]
    let ``the plan environment caches the kit's candidates under .oc/planner, and a read-only kit caches nothing`` () : unit =
        let folder = PlannerCacheTests.newKitFolder ()
        let writer : KitContext = PlannerCacheTests.openOver folder ReadWriteKitAccess
        Assert.Empty(cacheFilesOf folder)

        // Run here, not on the host: the real solve is this fact's subject, and the host's wait limit is the stubs'.
        let told = ResizeArray<PlanProgress> ()
        let recording : PlanObserver = { reportCandidate = told.Add; cancellation = CancellationToken.None }
        let environment = PlanEnvironment.create PlanJobHost.inert writer forward
        let report = environment.planWith request recording |> reportValue

        Assert.Equal(planStudy.name, report.study)
        Assert.Equal(crystalVersion, report.material)
        Assert.Equal(DeterminantPlanCriterion, report.criterion)
        Assert.Equal(2, report.ranked.Length)

        let progressOf (evaluated : int) : PlanProgress =
            { evaluated = PlanCandidateCount evaluated; total = PlanCandidateCount 2 }

        Assert.Equal<PlanProgress list>([ progressOf 1; progressOf 2 ], List.ofSeq told)

        // One file per candidate, each named by its key's hash.
        let cached = cacheFilesOf folder
        Assert.Equal(2, cached.Length)
        Assert.All(cached, fun path -> Assert.Matches("^\\.oc/planner/[0-9a-f]{64}\\.candidate-json$", path))
        Assert.Equal<Result<unit, KitError>>(Ok (), writer.close ())

        // The same folder read-only reads the cached rows to an equal report and writes nothing.
        let reader : KitContext = PlannerCacheTests.openOver folder ReadOnlyKitAccess
        let before = pathsOf folder
        let unread : PlanObserver = { reportCandidate = ignore; cancellation = CancellationToken.None }
        let readOnly = PlanEnvironment.create PlanJobHost.inert reader forward
        Assert.Equal<PlanReport>(report, readOnly.planWith request unread |> reportValue)
        Assert.Equal<string list>(before, pathsOf folder)

        // A study the kit does not hold is refused, naming it.
        match readOnly.planWith { request with study = StudyName "No such study" } unread with
        | Error (UnknownPlannerStudyErr reason as error) ->
            Assert.Equal("No such study", reason)
            Assert.Equal("the kit has no such study: No such study", error.describe)
        | Error error -> Assert.Fail($"expected UnknownPlannerStudyErr, got {error.describe}")
        | Ok _ -> Assert.Fail("a study the kit does not hold was planned")

        Assert.Equal<Result<unit, KitError>>(Ok (), reader.close ())
