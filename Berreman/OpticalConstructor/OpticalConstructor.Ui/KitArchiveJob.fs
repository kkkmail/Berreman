/// A kit zip or unzip off the UI thread with per-file progress and Cancel, through `JobRunner`, and the Send kit… and
/// Open kit from zip… dialogs (spec 0050 S6).
module OpticalConstructor.Ui.KitArchiveJob

open System
open System.Diagnostics
open System.Threading
open Avalonia.Controls
open Avalonia.Layout
open Avalonia.Platform.Storage
open Avalonia.Threading
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open Softellect.Sys.Primitives
open OpticalConstructor.Controls
open OpticalConstructor.Domain.KitArchive
open OpticalConstructor.Ui.JobRunner
open OpticalConstructor.Ui.KitOpening

type KitArchiveFileCount with

    /// A file count as the runner's step count (spec 0050 S6).
    member this.jobStepCount : JobStepCount =
        let (KitArchiveFileCount count) = this
        JobStepCount count

type KitArchiveProgress with

    /// The files done over the files total as the runner's progress, `elapsed` into the job (spec 0050 S6).
    member this.jobProgress (elapsed : JobElapsed) : JobProgress =
        {
            completed = this.filesDone.jobStepCount
            total = Some this.filesTotal.jobStepCount
            elapsed = elapsed
        }

/// How a started archive job is cancelled; the job reads the cancel before its next file (spec 0050 S6).
[<ReferenceEquality>]
type KitArchiveJobHandle =
    {
        cancelArchiveJob : unit -> unit
    }

/// Starts one archive job and answers its handle; the job's progress and its end reach the two callbacks the host was
/// built with (spec 0050 S6).
[<ReferenceEquality>]
type KitArchiveJobHost =
    {
        startArchiveJob : (KitArchiveObserver -> Result<KitArchiveOutcome, KitArchiveError>) -> KitArchiveJobHandle
    }

    /// One job's observer: each file's progress, timed from the job's start, goes to `report`, and the job reads
    /// `source` before each file (spec 0050 S6).
    static member private observerOf (source : CancellationTokenSource) (report : JobProgress -> unit) : KitArchiveObserver =
        let stopwatch = Stopwatch.StartNew ()
        {
            reportArchiveProgress =
                fun progress ->
                    let elapsed = JobElapsed stopwatch.Elapsed
                    report (progress.jobProgress elapsed)
            cancellation = source.Token
        }

    /// The source is never disposed: a Cancel may come after the job ended, and a plain source holds no timer or wait
    /// handle (spec 0050 S6).
    static member private handleOf (source : CancellationTokenSource) : KitArchiveJobHandle =
        {
            cancelArchiveJob = fun () -> source.Cancel ()
        }

    /// Runs each job through `JobRunner.startBackground` on a thread-pool worker, with a cancellation source of its
    /// own; its progress and its end go through `post`, and a job that raised ends with KitArchiveIoErr (spec 0050 S6).
    static member posting
        (post : (unit -> unit) -> unit)
        (progressed : JobProgress -> unit)
        (ended : Result<KitArchiveOutcome, KitArchiveError> -> unit)
        : KitArchiveJobHost =
        {
            startArchiveJob =
                fun job ->
                    let source = new CancellationTokenSource ()
                    let observer = KitArchiveJobHost.observerOf source (fun progress -> post (fun () -> progressed progress))
                    startBackground
                        (fun () -> RanToCompletion [ job observer ])
                        (fun finished ->
                            let archiveEnd =
                                match finished with
                                | Ok (RanToCompletion [ archived ])
                                | Ok (CancelledWith [ archived ]) -> archived
                                | Ok (RanToCompletion _)
                                | Ok (CancelledWith _) -> Error (KitArchiveIoErr "the archive job answered no single result")
                                | Error (JobRunExn e) -> Error (KitArchiveIoErr e.Message)
                            post (fun () -> ended archiveEnd))
                    KitArchiveJobHost.handleOf source
        }

    /// `posting` through Avalonia's UI dispatcher, so a window's messages arrive on its own thread: the host the
    /// windows compose (spec 0050 S6).
    static member onUiThread
        (progressed : JobProgress -> unit)
        (ended : Result<KitArchiveOutcome, KitArchiveError> -> unit)
        : KitArchiveJobHost =
        KitArchiveJobHost.posting (fun action -> Dispatcher.UIThread.Post(fun () -> action ())) progressed ended

    /// Runs the job on the caller's thread before it answers, with a cancellation source of its own, telling each file
    /// and then the end: the host of headless tests (spec 0050 S6).
    static member immediate
        (progressed : JobProgress -> unit)
        (ended : Result<KitArchiveOutcome, KitArchiveError> -> unit)
        : KitArchiveJobHost =
        {
            startArchiveJob =
                fun job ->
                    let source = new CancellationTokenSource ()
                    let archiveEnd = job (KitArchiveJobHost.observerOf source progressed)
                    ended archiveEnd
                    KitArchiveJobHost.handleOf source
        }

    /// Runs nothing and answers a handle that does nothing: the default of scenes that run no job (spec 0050 S6).
    static member inert : KitArchiveJobHost =
        {
            startArchiveJob = fun _ -> { cancelArchiveJob = ignore }
        }

/// The one archive job a surface runs: the runner's model and the running job's handle. A surface returns to `idle`
/// when its job ends and tells the outcome in its own status (spec 0050 S6).
type KitArchiveJobState =
    {
        job : JobModel<KitArchiveFileCount>
        handleOpt : KitArchiveJobHandle option
    }

    /// No archive job runs (spec 0050 S6).
    static member idle : KitArchiveJobState =
        {
            job = initModel
            handleOpt = None
        }

    /// The job runs, cancelled through `handle` (spec 0050 S6).
    member this.started (handle : KitArchiveJobHandle) : KitArchiveJobState =
        {
            job = updateJob StartRequested this.job
            handleOpt = Some handle
        }

    /// The job told one more file (spec 0050 S6).
    member this.progressed (progress : JobProgress) : KitArchiveJobState =
        {
            this with job = updateJob (ProgressReported progress) this.job
        }

    /// Cancels the running job before its next file, and nothing while no job runs (spec 0050 S6).
    member this.cancel () : unit =
        match this.handleOpt, this.job.status with
        | Some handle, Running -> handle.cancelArchiveJob ()
        | Some _, Idle
        | Some _, Completed
        | Some _, Cancelled
        | Some _, Failed _
        | None, _ -> ()

/// The automation ids of one surface's archive progress row (spec 0050 S6).
type KitArchiveProgressIds =
    {
        bar : ConstructorUiId
        text : ConstructorUiId
        cancel : ConstructorUiId
    }

/// The progress row of both surfaces: the bar and its text, `Starting…` before the first file, beside Cancel. The
/// shared button is a plain view, so a right-docked border holds it (spec 0050 S6).
let kitArchiveProgressView (state : KitArchiveJobState) (ids : KitArchiveProgressIds) (cancel : unit -> unit) : IView =
    let fraction = state.job.progress |> Option.bind (fun progress -> progress.fraction) |> Option.defaultValue 0.0
    DockPanel.create [
        DockPanel.children [
            Border.create [
                Border.dock Dock.Right
                Border.child (
                    MaterialsWindowView.actionButton ids.cancel (ControlCaption "Cancel") MaterialsWindowView.negativeBackground cancel)
            ]
            StackPanel.create [
                StackPanel.orientation Orientation.Vertical
                StackPanel.children [
                    ProgressBar.create [
                        ids.bar.automationAttribute<ProgressBar> ()
                        ProgressBar.minimum 0.0
                        ProgressBar.maximum 1.0
                        ProgressBar.value fraction
                        ProgressBar.isIndeterminate (Option.isNone state.job.progress)
                    ]
                    TextBlock.create [
                        ids.text.automationAttribute<TextBlock> ()
                        TextBlock.text (state.job.progressText |> Option.defaultValue "Starting…")
                    ]
                ]
            ]
        ]
    ]
    :> IView

/// Adapts a window's storage provider to the zip dialogs (spec 0050 S6).
type KitZipPicker with

    /// The Send kit… save dialog and the Open kit from zip… open dialog, one zip each, starting in `suggested` when it
    /// resolves. A cancel, a file with no local path or a provider that throws answers None; no folder is made (spec 0050 S6).
    static member ofWindow (owner : TopLevel) (suggested : FolderName) : KitZipPicker =
        let zipFileType = FilePickerFileType("Zip archive", Patterns = [ "*.zip" ])
        let tryStartLocation () : Async<IStorageFolder option> =
            async {
                try
                    let! start =
                        StorageProviderExtensions.TryGetFolderFromPathAsync(owner.StorageProvider, suggested.value)
                        |> Async.AwaitTask
                    return Option.ofObj start
                with _ -> return None
            }
        let tryLocalZipOf (file : IStorageFile) : FileName option =
            match file.TryGetLocalPath () |> Option.ofObj with
            | Some path ->
                let zip = FileName path
                Some zip
            | None -> None
        {
            pickZipToWrite =
                fun name ->
                    async {
                        try
                            let options =
                                FilePickerSaveOptions(
                                    Title = "Send kit",
                                    SuggestedFileName = name.value,
                                    DefaultExtension = "zip",
                                    FileTypeChoices = [ zipFileType ],
                                    ShowOverwritePrompt = Nullable true)
                            let! startOpt = tryStartLocation ()
                            match startOpt with
                            | Some folder -> options.SuggestedStartLocation <- folder
                            | None -> ()
                            let! file = owner.StorageProvider.SaveFilePickerAsync options |> Async.AwaitTask
                            return file |> Option.ofObj |> Option.bind tryLocalZipOf
                        with _ -> return None
                    }
            pickZipToOpen =
                fun () ->
                    async {
                        try
                            let options =
                                FilePickerOpenOptions(
                                    Title = "Open kit from zip",
                                    AllowMultiple = false,
                                    FileTypeFilter = [ zipFileType ])
                            let! startOpt = tryStartLocation ()
                            match startOpt with
                            | Some folder -> options.SuggestedStartLocation <- folder
                            | None -> ()
                            let! files = owner.StorageProvider.OpenFilePickerAsync options |> Async.AwaitTask
                            return files |> Seq.tryHead |> Option.bind tryLocalZipOf
                        with _ -> return None
                    }
        }

    /// Starts the save dialog on the calling UI thread and hands on the zip it answers; a cancel calls nothing (spec 0050 S6).
    member this.pickZipToWriteThen (name : FileName) (onPicked : FileName -> unit) : unit = startPick (this.pickZipToWrite name) onPicked

    /// Starts the open dialog on the calling UI thread and hands on the zip it answers; a cancel calls nothing (spec 0050 S6).
    member this.pickZipToOpenThen (onPicked : FileName -> unit) : unit = startPick (this.pickZipToOpen ()) onPicked
