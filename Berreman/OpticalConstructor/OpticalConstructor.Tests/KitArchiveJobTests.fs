namespace OpticalConstructor.Tests

open System
open System.IO
open System.Threading
open Xunit
open Softellect.Sys.Primitives
open OpticalConstructor.Domain.KitArchive
open OpticalConstructor.Ui.JobRunner
open OpticalConstructor.Ui.KitArchiveJob

module KitArchiveJobTests =

    /// What a job host told its callbacks, in order, with the thread that told it.
    type private RecordedArchiveCall =
        | ArchiveProgressRecorded of progress : JobProgress * threadId : int
        | ArchiveEndRecorded of archiveEnd : Result<KitArchiveOutcome, KitArchiveError> * threadId : int

    let private waitLimit : TimeSpan = TimeSpan.FromSeconds 10.0

    let private filesOf (filesDone : int) (filesTotal : int) : KitArchiveProgress =
        {
            filesDone = KitArchiveFileCount filesDone
            filesTotal = KitArchiveFileCount filesTotal
        }

    /// Posts at once, on the thread that posts.
    let private postAtOnce (action : unit -> unit) : unit = action ()

    [<Fact>]
    let ``an immediate job reports each file and then its end on the caller's thread`` () : unit =
        let testThread = Thread.CurrentThread.ManagedThreadId
        let calls = ResizeArray<RecordedArchiveCall>()
        let host =
            KitArchiveJobHost.immediate
                (fun progress -> calls.Add(ArchiveProgressRecorded (progress, Thread.CurrentThread.ManagedThreadId)))
                (fun archiveEnd -> calls.Add(ArchiveEndRecorded (archiveEnd, Thread.CurrentThread.ManagedThreadId)))
        let job (observer : KitArchiveObserver) : Result<KitArchiveOutcome, KitArchiveError> =
            observer.reportArchiveProgress (filesOf 1 2)
            observer.reportArchiveProgress (filesOf 2 2)
            Ok (KitArchiveFinished (KitArchiveFileCount 2))

        host.startArchiveJob job |> ignore

        match List.ofSeq calls with
        | [ ArchiveProgressRecorded (first, firstThread)
            ArchiveProgressRecorded (second, secondThread)
            ArchiveEndRecorded (archiveEnd, endThread) ] ->
            Assert.Equal<JobStepCount>(JobStepCount 1, first.completed)
            Assert.Equal<JobStepCount option>(Some (JobStepCount 2), first.total)
            Assert.Equal<JobStepCount>(JobStepCount 2, second.completed)
            Assert.Equal<JobStepCount option>(Some (JobStepCount 2), second.total)
            Assert.Equal<Result<KitArchiveOutcome, KitArchiveError>>(Ok (KitArchiveFinished (KitArchiveFileCount 2)), archiveEnd)
            Assert.Equal<int list>([ testThread; testThread; testThread ], [ firstThread; secondThread; endThread ])
        | other -> Assert.Fail $"expected two progresses and then the end, before the job answered; got %A{other}"

    [<Fact>]
    let ``a posting job hands its cancellation to the run before its next file`` () : unit =
        use firstProgress = new ManualResetEventSlim(false)
        use jobGate = new ManualResetEventSlim(false)
        use jobEnded = new ManualResetEventSlim(false)
        let ends = ResizeArray<Result<KitArchiveOutcome, KitArchiveError>>()
        let host =
            KitArchiveJobHost.posting
                postAtOnce
                (fun _ -> firstProgress.Set())
                (fun archiveEnd ->
                    lock ends (fun () -> ends.Add archiveEnd)
                    jobEnded.Set())
        let job (observer : KitArchiveObserver) : Result<KitArchiveOutcome, KitArchiveError> =
            observer.reportArchiveProgress (filesOf 1 2)
            jobGate.Wait waitLimit |> ignore
            if observer.cancellation.IsCancellationRequested then
                Ok KitArchiveCancelled
            else
                observer.reportArchiveProgress (filesOf 2 2)
                Ok (KitArchiveFinished (KitArchiveFileCount 2))

        let handle = host.startArchiveJob job
        Assert.True(firstProgress.Wait waitLimit, "the job's first progress never arrived")
        handle.cancelArchiveJob ()
        jobGate.Set()

        Assert.True(jobEnded.Wait waitLimit, "the job's end never arrived")
        Assert.Equal<Result<KitArchiveOutcome, KitArchiveError> list>([ Ok KitArchiveCancelled ], lock ends (fun () -> List.ofSeq ends))

    [<Fact>]
    let ``a posting run that raises ends with KitArchiveIoErr carrying its message`` () : unit =
        use jobEnded = new ManualResetEventSlim(false)
        let ends = ResizeArray<Result<KitArchiveOutcome, KitArchiveError>>()
        let host =
            KitArchiveJobHost.posting
                postAtOnce
                ignore
                (fun archiveEnd ->
                    lock ends (fun () -> ends.Add archiveEnd)
                    jobEnded.Set())

        host.startArchiveJob (fun _ -> raise (IOException "disk gone")) |> ignore

        Assert.True(jobEnded.Wait waitLimit, "the job's end never arrived")
        match lock ends (fun () -> List.ofSeq ends) with
        | [ Error (KitArchiveIoErr reason) ] -> Assert.Contains("disk gone", reason)
        | other -> Assert.Fail $"expected one KitArchiveIoErr end; got %A{other}"

    [<Fact>]
    let ``a job state runs from start through progress back to idle`` () : unit =
        let idle = KitArchiveJobState.idle
        Assert.False idle.job.isRunning
        Assert.Equal<string option>(None, idle.job.progressText)

        let cancels = ResizeArray<unit>()
        let handle : KitArchiveJobHandle =
            {
                cancelArchiveJob = fun () -> cancels.Add ()
            }
        let started = idle.started handle
        Assert.True started.job.isRunning

        let progressed = started.progressed ((filesOf 1 3).jobProgress (JobElapsed TimeSpan.Zero))
        match progressed.job.progressText with
        | Some text -> Assert.StartsWith("1 / 3", text)
        | None -> Assert.Fail "a progressed job reads no progress text"

        progressed.cancel ()
        Assert.Single cancels |> ignore

        Assert.False KitArchiveJobState.idle.job.isRunning

    [<Fact>]
    let ``the zip pickers hand on the file the dialog answers and nothing on a cancel`` () : unit =
        let zip = FileName (Path.Combine(AppContext.BaseDirectory, "KitArchiveJobTests", "Optics QZ.zip"))
        let suggested = FileName "Optics QZ.zip"
        let suggestions = ResizeArray<FileName>()
        let answering : KitZipPicker =
            {
                pickZipToWrite =
                    fun name ->
                        suggestions.Add name
                        async { return Some zip }
                pickZipToOpen = fun () -> async { return Some zip }
            }

        let written = ResizeArray<FileName>()
        answering.pickZipToWriteThen suggested written.Add
        Assert.Equal<FileName list>([ zip ], List.ofSeq written)
        Assert.Equal<FileName list>([ suggested ], List.ofSeq suggestions)

        let opened = ResizeArray<FileName>()
        answering.pickZipToOpenThen opened.Add
        Assert.Equal<FileName list>([ zip ], List.ofSeq opened)

        let cancelled = ResizeArray<FileName>()
        KitZipPicker.none.pickZipToWriteThen suggested cancelled.Add
        KitZipPicker.none.pickZipToOpenThen cancelled.Add
        Assert.Empty cancelled
