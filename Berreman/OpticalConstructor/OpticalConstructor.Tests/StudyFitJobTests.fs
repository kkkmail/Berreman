namespace OpticalConstructor.Tests

open System
open System.Threading
open Xunit
open Softellect.Sys.Primitives
open OpticalConstructor.Domain.ItemModels
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.KitFolders
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.Studies
open OpticalConstructor.Domain.FitRuns
open OpticalConstructor.Domain.MuellerInverse
open OpticalConstructor.Optimization.OptimizationInterface
open OpticalConstructor.Optimization.StudyEngine
open OpticalConstructor.Storage.KitContentFiles
open OpticalConstructor.Storage.KitFolderStore
open OpticalConstructor.Ui.JobRunner
open OpticalConstructor.Ui.StudyFitJob
open FitRunFileFixtures

/// A study fit hosted on `JobRunner` through `StudyFitMsg.updateWith`, over an in-memory kit: per-iteration progress,
/// Cancel, and the fit run written once on completion only (spec 0050 S11.3).
module StudyFitJobTests =

    /// One stub fit: what the host runs on its worker, given the fit's watch.
    type private StubFit = IterationWatch -> Result<FitRun, StudyFitError>

    /// A surface's one fit: its messages handled one at a time under one lock, as Elmish dispatches them, so a worker's
    /// post never races the `RunFit` that started it. `ended` is set after each `StudyFitEnded` is handled.
    [<ReferenceEquality>]
    type private FitSurface =
        {
            kit : KitProxy
            send : StudyFitMsg -> unit
            current : unit -> StudyFitJobState
            told : unit -> JobProgress list
            ended : ManualResetEventSlim
            starts : unit -> int
            fitCalls : unit -> int
        }

    let private request : StudyFitRequest =
        {
            study = qzLrFitRun.study
            stage = ObjectStage
            calibrationUse = CalibrationHeldFixed
        }

    let private tenSeconds : TimeSpan = TimeSpan.FromSeconds 10.0

    let private kitValue<'Value> (result : Result<'Value, KitError>) : 'Value =
        result |> Result.defaultWith (fun error -> failwith error.describe)

    /// An initialised in-memory kit's proxy (step 125).
    let private newKit () : KitProxy =
        let folder = InMemoryKitFolder.create ()
        let proxy = KitProxy.createInMemory folder
        let at = ClockTime (DateTime(2026, 11, 14, 12, 0, 0, DateTimeKind.Utc))
        let kitFile = KitFile.create (KitId (Guid.Parse "39300000-0000-4000-8000-000000000001")) (KitName "Study fit job kit") (KitAuthor "Study fit job tests") at
        proxy.initialiseKit kitFile |> kitValue
        proxy

    /// A surface over `kit` whose host is `posting` with an immediate `post`; its n-th fit is `fits`' n-th stub. Every
    /// start of the host is counted, as is every call of the fit.
    let private surfaceOver (kit : KitProxy) (fits : StubFit list) : FitSurface =
        let sync = obj ()
        let state = ref StudyFitJobState.idle
        let told = ResizeArray<JobProgress> ()
        let ended = new ManualResetEventSlim (false)
        let starts = ref 0
        let calls = ref 0
        let environmentCell : StudyFitEnvironment option ref = ref None

        let send (message : StudyFitMsg) : unit =
            lock sync (fun () ->
                match environmentCell.Value with
                | Some environment -> state.Value <- message.updateWith environment state.Value
                | None -> failwith "the surface was sent a message before its environment was built")

        let posting =
            StudyFitJobHost.posting
                (fun action -> action ())
                (fun progress ->
                    lock sync (fun () -> told.Add progress)
                    send (StudyFitProgressed progress))
                (fun result ->
                    send (StudyFitEnded result)
                    ended.Set ())

        let host : StudyFitJobHost =
            {
                startStudyFit =
                    fun fit ->
                        Interlocked.Increment starts |> ignore
                        posting.startStudyFit fit
            }

        let fitWith (_ : StudyFitRequest) (watch : IterationWatch) : Result<FitRun, StudyFitError> =
            let call = Interlocked.Increment calls
            match List.tryItem (call - 1) fits with
            | Some fit -> fit watch
            | None -> Error (NoConvergedMemberErr $"fit {call} was not expected")

        let environment : StudyFitEnvironment =
            {
                host = host
                fitWith = fitWith
                kit = kit
            }

        environmentCell.Value <- Some environment

        {
            kit = kit
            send = send
            current = fun () -> lock sync (fun () -> state.Value)
            told = fun () -> lock sync (fun () -> List.ofSeq told)
            ended = ended
            starts = fun () -> starts.Value
            fitCalls = fun () -> calls.Value
        }

    /// Waits, never sleeps, until the surface has handled its fit's end.
    let private awaitEnd (surface : FitSurface) : unit =
        Assert.True(surface.ended.Wait(tenSeconds), "the fit did not end within ten seconds")

    /// Reports iterations 1, 2 and 3 through its watch, then answers `run`.
    let private reportingThreeThen (run : FitRun) (watch : IterationWatch) : Result<FitRun, StudyFitError> =
        match watch with
        | WatchedIterations observer ->
            for k in 1 .. 3 do
                observer.reportIteration (IterationCount k)
            Ok run
        | UnwatchedIterations -> Error (NoConvergedMemberErr "the stub was given no watch")

    /// Waits until its watch's cancellation is requested, then answers `FitCancelledErr`, as a watched solve does.
    let private untilCancelled (watch : IterationWatch) : Result<FitRun, StudyFitError> =
        match watch with
        | WatchedIterations observer ->
            if observer.cancellation.WaitHandle.WaitOne(tenSeconds) then
                Error (FitCancelledErr "the stub stopped on its watch's cancellation")
            else
                Error (NoConvergedMemberErr "the stub's watch was not cancelled within ten seconds")
        | UnwatchedIterations -> Error (NoConvergedMemberErr "the stub was given no watch")

    let private fitRunStems (kit : KitProxy) : FileStem list = kit.listKindFiles FitRunKitFile |> kitValue

    /// The file QZ-LR's first fit run is written to, at the kit's root.
    let private qzLrFitRunPath : KitRelativePath = KitRelativePath "QZ-LR@20261101T120000Z.fitrun-json"

    /// The bytes the kit holds for QZ-LR's first fit run.
    let private qzLrFitRunBytes (kit : KitProxy) : FileByteContent = (kit.readLabFile qzLrFitRunPath |> kitValue).content

    [<Fact>]
    let ``RunFit reports each iteration, then writes the fit run once the fit completes`` () : unit =
        let kit = newKit ()
        let surface = surfaceOver kit [ reportingThreeThen qzLrFitRun ]

        surface.send (RunFit request)
        awaitEnd surface

        let state = surface.current ()
        Assert.Equal<JobStatus>(Completed, state.job.status)
        Assert.Equal<FitRunName option>(Some qzLrFitRun.name, state.job.committedResults)
        Assert.True(Option.isNone state.handleOpt, "a completed fit holds no handle")
        Assert.True(Option.isNone state.errorOpt, "a completed fit holds no error")

        // Each iteration was told in order, an iteration count with no known total.
        let told = surface.told ()
        Assert.Equal<JobStepCount list>([ JobStepCount 1; JobStepCount 2; JobStepCount 3 ], told |> List.map (fun progress -> progress.completed))
        Assert.All(told, fun progress -> Assert.True(Option.isNone progress.total, "an iteration count has no total"))

        match state.job.progress with
        | Some progress ->
            Assert.Equal<JobStepCount>(JobStepCount 3, progress.completed)
            Assert.True(Option.isNone progress.total, "the last progress has no total")
        | None -> Assert.Fail("the completed fit kept no progress")

        match state.job.progressText with
        | Some text -> Assert.StartsWith("3 iterations", text)
        | None -> Assert.Fail("the completed fit shows no progress text")

        Assert.Equal<FitRun list>([ qzLrFitRun ], kit.readFitRuns qzLrFitRun.study |> kitValue)
        Assert.Equal<FileStem list>([ qzLrFitRun.name.fileStem ], fitRunStems kit)
        Assert.Equal(1, surface.starts ())
        Assert.Equal(1, surface.fitCalls ())

    [<Fact>]
    let ``a cancelled fit writes nothing and keeps the last committed fit run`` () : unit =
        let kit = newKit ()
        let surface = surfaceOver kit [ reportingThreeThen qzLrFitRun; untilCancelled ]

        surface.send (RunFit request)
        awaitEnd surface
        Assert.Equal<FitRunName option>(Some qzLrFitRun.name, (surface.current ()).job.committedResults)
        let before = qzLrFitRunBytes kit

        surface.ended.Reset ()
        surface.send (RunFit { request with stage = JointStage })
        Assert.Equal<JobStatus>(Running, (surface.current ()).job.status)
        surface.send CancelStudyFit
        awaitEnd surface

        let state = surface.current ()
        Assert.Equal<JobStatus>(Cancelled, state.job.status)
        Assert.Equal<FitRunName option>(Some qzLrFitRun.name, state.job.committedResults)
        Assert.True(Option.isNone state.handleOpt, "a cancelled fit holds no handle")
        Assert.True(Option.isNone state.errorOpt, "a cancel is not an error")

        Assert.Equal<FileStem list>([ qzLrFitRun.name.fileStem ], fitRunStems kit)
        Assert.Equal<FileByteContent>(before, qzLrFitRunBytes kit)
        Assert.Equal<FitRun list>([ qzLrFitRun ], kit.readFitRuns qzLrFitRun.study |> kitValue)
        Assert.Equal(2, surface.fitCalls ())

    [<Fact>]
    let ``a fit run that would overwrite an existing one is refused and nothing is written`` () : unit =
        let kit = newKit ()
        kit.writeFitRun qzLrFitRun |> kitValue
        let before = qzLrFitRunBytes kit

        // The same name over other entries, so a rewrite would change the file.
        let sameName : FitRun =
            {
                qzLrFitRun with
                    entries = qzLrFitRun.entries |> List.map (fun entry -> { entry with fitted = ParameterValue 0.5 })
            }

        let surface = surfaceOver kit [ reportingThreeThen sameName ]
        surface.send (RunFit request)
        awaitEnd surface

        let state = surface.current ()
        match state.errorOpt with
        | Some (FitRunKitErr (FitRunExistsErr reason)) -> Assert.Contains(qzLrFitRunPath.value, reason)
        | Some error -> Assert.Fail($"expected FitRunKitErr (FitRunExistsErr _), got {error.describe}")
        | None -> Assert.Fail("a fit run over an existing name was not refused")

        Assert.Equal<JobStatus>(Idle, state.job.status)
        Assert.Equal<FitRunName option>(None, state.job.committedResults)
        Assert.True(Option.isNone state.handleOpt, "a refused fit holds no handle")

        Assert.Equal<FileByteContent>(before, qzLrFitRunBytes kit)
        Assert.Equal<FileStem list>([ qzLrFitRun.name.fileStem ], fitRunStems kit)
        Assert.Equal<FitRun list>([ qzLrFitRun ], kit.readFitRuns qzLrFitRun.study |> kitValue)

    [<Fact>]
    let ``a fit that raises ends with FitRunRaisedErr carrying its message`` () : unit =
        let kit = newKit ()
        let raising : StubFit = fun _ -> raise (InvalidOperationException "solver gone")
        let surface = surfaceOver kit [ raising ]

        surface.send (RunFit request)
        awaitEnd surface

        let state = surface.current ()
        match state.errorOpt with
        | Some (FitRunRaisedErr reason) -> Assert.Contains("solver gone", reason)
        | Some error -> Assert.Fail($"expected FitRunRaisedErr, got {error.describe}")
        | None -> Assert.Fail("a fit that raised holds no error")

        Assert.Equal<JobStatus>(Idle, state.job.status)
        Assert.Equal<FitRunName option>(None, state.job.committedResults)
        Assert.True(Option.isNone state.handleOpt, "a fit that raised holds no handle")
        Assert.Empty(fitRunStems kit)

    [<Fact>]
    let ``RunFit while a fit runs starts nothing`` () : unit =
        let kit = newKit ()
        use release = new ManualResetEventSlim (false)

        let held : StubFit =
            fun _ ->
                if release.Wait(tenSeconds) then
                    Ok qzLrFitRun
                else
                    Error (NoConvergedMemberErr "the held fit was not released within ten seconds")

        let surface = surfaceOver kit [ held ]
        surface.send (RunFit request)
        let running = surface.current ()
        Assert.Equal<JobStatus>(Running, running.job.status)

        surface.send (RunFit { request with stage = JointStage })
        let still = surface.current ()
        Assert.Equal<JobStatus>(Running, still.job.status)
        match running.handleOpt, still.handleOpt with
        | Some first, Some kept -> Assert.True(obj.ReferenceEquals(first, kept), "the second RunFit replaced the running fit's handle")
        | _ -> Assert.Fail("the running fit holds no handle")
        Assert.Equal(1, surface.starts ())

        release.Set ()
        awaitEnd surface

        let state = surface.current ()
        Assert.Equal<JobStatus>(Completed, state.job.status)
        Assert.Equal<FitRunName option>(Some qzLrFitRun.name, state.job.committedResults)
        Assert.Equal(1, surface.starts ())
        Assert.Equal(1, surface.fitCalls ())
        Assert.Equal<FileStem list>([ qzLrFitRun.name.fileStem ], fitRunStems kit)
