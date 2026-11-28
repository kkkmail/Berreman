/// The Online sources surface: search and pull over the import context's material sources, and Update snapshot off the
/// UI thread with per-file progress and Cancel, one update at a time (spec 0050 S13.1; 001 §O, §P).
module OpticalConstructor.Ui.MaterialSourcesView

open System.Diagnostics
open System.Threading
open Avalonia
open Avalonia.Controls
open Avalonia.Controls.Primitives
open Avalonia.Input
open Avalonia.Layout
open Avalonia.Media
open Avalonia.Threading
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open OpticalConstructor.Controls
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.CatalogueImport
open OpticalConstructor.Domain.MaterialSources
open OpticalConstructor.Domain.MaterialSourcePulls
open OpticalConstructor.Ui.JobRunner

type SourceSnapshotFileCount with

    /// A file count as the runner's step count (spec 0050 S13.1).
    member this.jobStepCount : JobStepCount =
        let (SourceSnapshotFileCount count) = this
        JobStepCount count

type SourceSnapshotProgress with

    /// The files written over the files in all as the runner's progress, `elapsed` into the update (spec 0050 S13.1).
    member this.jobProgress (elapsed : JobElapsed) : JobProgress =
        {
            completed = this.filesDone.jobStepCount
            total = Some this.filesTotal.jobStepCount
            elapsed = elapsed
        }

/// How a started snapshot update is cancelled; the update reads the cancel before its next file (spec 0050 S13.1).
[<ReferenceEquality>]
type SourceSnapshotJobHandle =
    {
        cancelSourceSnapshot : unit -> unit
    }

/// Starts one snapshot update and answers its handle; its progress and its end reach the two callbacks the host was
/// built with (spec 0050 S13.1).
[<ReferenceEquality>]
type SourceSnapshotJobHost =
    {
        startSourceSnapshot : (SourceSnapshotObserver -> Result<SourceRefresh, MaterialSourcePullError>) -> SourceSnapshotJobHandle
    }

    /// One update's observer: each file's progress, timed from the update's start, goes to `report`, and the update
    /// reads `source` before each file (spec 0050 S13.1).
    static member private observerOf (source : CancellationTokenSource) (report : JobProgress -> unit) : SourceSnapshotObserver =
        let stopwatch = Stopwatch.StartNew ()
        {
            reportSnapshotProgress =
                fun progress ->
                    let elapsed = JobElapsed stopwatch.Elapsed
                    report (progress.jobProgress elapsed)
            cancellation = source.Token
        }

    /// The source is never disposed: a Cancel may come after the update ended, and a plain source holds no timer or
    /// wait handle (spec 0050 S13.1).
    static member private handleOf (source : CancellationTokenSource) : SourceSnapshotJobHandle =
        {
            cancelSourceSnapshot = fun () -> source.Cancel ()
        }

    /// Runs each update through `JobRunner.startBackground` on a thread-pool worker, with a cancellation source of its
    /// own; its progress and its end go through `post`, and an update that raised ends with SourceSnapshotIoErr (spec 0050 S13.1).
    static member posting
        (post : (unit -> unit) -> unit)
        (progressed : JobProgress -> unit)
        (ended : Result<SourceRefresh, MaterialSourcePullError> -> unit)
        : SourceSnapshotJobHost =
        {
            startSourceSnapshot =
                fun update ->
                    let source = new CancellationTokenSource ()
                    let observer = SourceSnapshotJobHost.observerOf source (fun progress -> post (fun () -> progressed progress))
                    startBackground
                        (fun () -> RanToCompletion [ update observer ])
                        (fun finished ->
                            let updateEnd =
                                match finished with
                                | Ok (RanToCompletion [ updated ])
                                | Ok (CancelledWith [ updated ]) -> updated
                                | Ok (RanToCompletion _)
                                | Ok (CancelledWith _) -> Error (PullSourceErr (SourceSnapshotIoErr "the snapshot update answered no single result"))
                                | Error (JobRunExn e) -> Error (PullSourceErr (SourceSnapshotIoErr e.Message))
                            post (fun () -> ended updateEnd))
                    SourceSnapshotJobHost.handleOf source
        }

    /// `posting` through Avalonia's UI dispatcher, so the window's messages arrive on its own thread: the host the
    /// Online sources window composes (spec 0050 S13.1).
    static member onUiThread
        (progressed : JobProgress -> unit)
        (ended : Result<SourceRefresh, MaterialSourcePullError> -> unit)
        : SourceSnapshotJobHost =
        SourceSnapshotJobHost.posting (fun action -> Dispatcher.UIThread.Post(fun () -> action ())) progressed ended

    /// Runs the update on the caller's thread before it answers, with a cancellation source of its own, telling each
    /// file and then the end: the host of tests (spec 0050 S13.1).
    static member immediate
        (progressed : JobProgress -> unit)
        (ended : Result<SourceRefresh, MaterialSourcePullError> -> unit)
        : SourceSnapshotJobHost =
        {
            startSourceSnapshot =
                fun update ->
                    let source = new CancellationTokenSource ()
                    let updateEnd = update (SourceSnapshotJobHost.observerOf source progressed)
                    ended updateEnd
                    SourceSnapshotJobHost.handleOf source
        }

    /// Runs nothing and answers a handle that does nothing: the default of surfaces that run no update (spec 0050 S13.1).
    static member inert : SourceSnapshotJobHost =
        {
            startSourceSnapshot = fun _ -> { cancelSourceSnapshot = ignore }
        }

/// What the surface reaches IO through; the composition root bakes the real seams in and tests substitute doubles
/// (spec 0050 S13.1).
[<ReferenceEquality>]
type MaterialSourcesContext =
    {
        /// The import seam the sources and their pulls ride (spec 0050 S13.1).
        imports : CatalogueImportContext
        /// The host Update snapshot runs on (spec 0050 S13.1).
        jobs : SourceSnapshotJobHost
        /// Tells the window that opened the surface what the library gained, as a caption (spec 0050 S13.1).
        pullsApplied : ControlCaption -> unit
    }

/// The surface's model: the shown source and its snapshot, the search, the one update and the last outcome (spec 0050
/// S13.1).
type MaterialSourcesModel =
    {
        /// The seams the update reaches IO through (spec 0050 S13.1).
        context : MaterialSourcesContext
        /// The source shown; None when the import context offers none (spec 0050 S13.1).
        shownSourceOpt : MaterialSourceName option
        /// The shown source's held snapshot version; None before its first update (spec 0050 S13.1).
        snapshotOpt : MaterialSourceVersion option
        /// The search text as typed (spec 0050 S13.1).
        queryDraft : MaterialSourceQuery
        /// The records the last search found in the shown source (spec 0050 S13.1).
        hits : MaterialSourceHit list
        /// The update's runner model; its committed result is the last installed snapshot's file count (spec 0050 S13.1).
        job : JobModel<SourceSnapshotFileCount>
        /// The running update's handle (spec 0050 S13.1).
        handleOpt : SourceSnapshotJobHandle option
        /// What the last pull or update did (spec 0050 S13.1).
        status : ControlCaption option
        /// Why the last snapshot read, search, pull or update was refused (spec 0050 S13.1).
        error : MaterialSourcePullError option
    }

    /// The context's source of the shown name (spec 0050 S13.1).
    member this.shownSource : MaterialSourceProxy option =
        match this.shownSourceOpt with
        | Some name -> this.context.imports.materialSources |> List.tryFind (fun source -> source.sourceName = name)
        | None -> None

    /// Shows `source` with its held snapshot version, its refusal held as PullSourceErr, and no hit, status or error
    /// before it (spec 0050 S13.1).
    member internal this.showing (source : MaterialSourceProxy) : MaterialSourcesModel =
        let shown =
            {
                this with
                    shownSourceOpt = Some source.sourceName
                    hits = []
                    status = None
            }
        match source.snapshotVersion () with
        | Ok versionOpt -> { shown with snapshotOpt = versionOpt; error = None }
        | Error error -> { shown with snapshotOpt = None; error = Some (PullSourceErr error) }

/// The surface's semantic messages: its buttons and the tests send the same ones (spec 0050 S13.1; 001 §P).
type MaterialSourcesMsg =
    /// Show the named source and its held snapshot (spec 0050 S13.1).
    | ShowMaterialSource of name : MaterialSourceName
    /// The search text changed (spec 0050 S13.1).
    | SetSourceQueryDraft of query : MaterialSourceQuery
    /// Search the shown source's held snapshot (spec 0050 S13.1).
    | SearchMaterialSource of query : MaterialSourceQuery
    /// Pull the shown source's record into the library (spec 0050 S13.1).
    | PullSourceRecord of record : MaterialSourceRecordId
    /// Update snapshot: install the shown source's current snapshot off the UI thread, then refresh its pulls (spec 0050 S13.1).
    | UpdateSourceSnapshot
    /// Stop the running update before its next file (spec 0050 S13.1).
    | CancelSourceSnapshot
    /// The running update wrote one more file (spec 0050 S13.1).
    | SourceSnapshotProgressed of progress : JobProgress
    /// The running update ended: refreshed, cancelled or refused (spec 0050 S13.1).
    | SourceSnapshotEnded of result : Result<SourceRefresh, MaterialSourcePullError>

type MaterialSourcesContext with

    /// The surface as it opens: the first source shown with its held snapshot (None when there is no source), an
    /// empty query, no hit, no update and no status (spec 0050 S13.1).
    member this.initMaterialSources : MaterialSourcesModel =
        let opened : MaterialSourcesModel =
            {
                context = this
                shownSourceOpt = None
                snapshotOpt = None
                queryDraft = MaterialSourceQuery ""
                hits = []
                job = initModel
                handleOpt = None
                status = None
                error = None
            }
        match this.imports.materialSources with
        | first :: _ -> opened.showing first
        | [] -> opened

/// One update at a time, and no other source shown while it runs; a pull's and a refresh's captions reach
/// `pullsApplied`, so the opener re-renders what the library gained (spec 0050 S13.1; .spec-md §10.4).
let updateMaterialSources (msg : MaterialSourcesMsg) (m : MaterialSourcesModel) : MaterialSourcesModel =
    match msg with
    | ShowMaterialSource name ->
        match m.job.status, m.context.imports.materialSources |> List.tryFind (fun source -> source.sourceName = name) with
        | Running, _ -> m
        | (Idle | Completed | Cancelled | Failed _), Some source -> m.showing source
        | (Idle | Completed | Cancelled | Failed _), None -> m
    | SetSourceQueryDraft query -> { m with queryDraft = query }
    | SearchMaterialSource query ->
        match m.shownSource with
        | Some source ->
            match source.searchRecords query with
            | Ok hits -> { m with hits = hits; error = None }
            | Error error -> { m with hits = []; error = Some (PullSourceErr error) }
        | None -> m
    | PullSourceRecord record ->
        match m.shownSource with
        | Some source ->
            match m.context.imports.pullRecord source record with
            | Ok results ->
                let pulled = results |> List.map _.caption.value |> String.concat " " |> ControlCaption
                m.context.pullsApplied pulled
                { m with status = Some pulled; error = None }
            | Error error -> { m with error = Some error }
        | None -> m
    | UpdateSourceSnapshot ->
        match m.job.status, m.shownSource with
        | Running, _ -> m
        | (Idle | Completed | Cancelled | Failed _), Some source ->
            let handle = m.context.jobs.startSourceSnapshot (m.context.imports.updateSourceSnapshot source)
            {
                m with
                    job = updateJob StartRequested m.job
                    handleOpt = Some handle
                    status = None
                    error = None
            }
        | (Idle | Completed | Cancelled | Failed _), None -> m
    | CancelSourceSnapshot ->
        match m.handleOpt, m.job.status with
        | Some handle, Running -> handle.cancelSourceSnapshot ()
        | Some _, (Idle | Completed | Cancelled | Failed _)
        | None, _ -> ()
        m
    | SourceSnapshotProgressed progress -> { m with job = updateJob (ProgressReported progress) m.job }
    | SourceSnapshotEnded (Ok (SourceRefreshed info)) ->
        let refreshed = info.caption
        m.context.pullsApplied refreshed
        {
            m with
                job = updateJob (RunCompleted info.snapshot.files) m.job
                handleOpt = None
                snapshotOpt = Some info.snapshot.version
                status = Some refreshed
        }
    | SourceSnapshotEnded (Ok SourceRefreshCancelled) ->
        {
            m with
                job = updateJob RunCancelled m.job
                handleOpt = None
                status = Some (ControlCaption "The snapshot update was cancelled; the held snapshot is unchanged.")
        }
    | SourceSnapshotEnded (Error error) ->
        {
            m with
                job = { m.job with status = Idle; progress = None }
                handleOpt = None
                error = Some error
        }

/// The surface lays out at least this wide and scrolls sideways below it, as the Data locations screen does (spec 0050 S13.1).
let private materialSourcesMinWidth : PanelMinWidth = PanelMinWidth 520.0

/// One hit: Pull on the right, the semi-bold name over the record in the fill, keyed by its id so a new search never
/// patches one hit into another (spec 0050 S13.1).
let private materialSourceHitView (dispatch : MaterialSourcesMsg -> unit) (hit : MaterialSourceHit) : IView =
    let rowId = ConstructorUiId.materialSourceHitRow hit.record
    DockPanel.create [
        yield! rowId.groupAttributes<DockPanel> ()
        yield! rowId.groupAttributes<DockPanel> ()
        DockPanel.margin (Thickness(0.0, 0.0, 0.0, 6.0))
        DockPanel.children [
            Border.create [
                Border.dock Dock.Right
                Border.verticalAlignment VerticalAlignment.Center
                Border.child (
                    CommandControls.verbCommand (ConstructorUiId.materialSourcePullButton hit.record) (ControlCaption "Pull") (fun () ->
                        dispatch (PullSourceRecord hit.record)))
            ]
            StackPanel.create [
                StackPanel.orientation Orientation.Vertical
                StackPanel.children [
                    TextBlock.create [
                        TextBlock.fontWeight FontWeight.SemiBold
                        TextBlock.textWrapping TextWrapping.Wrap
                        TextBlock.text hit.name.value
                    ]
                    TextBlock.create [
                        TextBlock.textWrapping TextWrapping.Wrap
                        TextBlock.text hit.record.value
                    ]
                ]
            ]
        ]
    ]
    |> Avalonia.FuncUI.DSL.View.withKey rowId.value
    :> IView

/// The source choices, the snapshot with Update snapshot and, while it runs, its progress and Cancel, the search, its
/// hits and the last outcome (spec 0050 S13.1).
let private materialSourceRows (m : MaterialSourcesModel) (dispatch : MaterialSourcesMsg -> unit) : IView list =
    let choices : IView =
        WrapPanel.create [
            WrapPanel.orientation Orientation.Horizontal
            WrapPanel.children (
                m.context.imports.materialSources
                |> List.map (fun source ->
                    let name = source.sourceName
                    let choiceId = ConstructorUiId.materialSourceChoice name
                    let show () = dispatch (ShowMaterialSource name)
                    let mark =
                        match m.shownSourceOpt with
                        | Some shown when shown = name -> ChosenChoice
                        | Some _
                        | None -> UnchosenChoice
                    CommandControls.choiceCommand choiceId name.caption mark show))
        ]

    let updateAvailability : ControlAvailability =
        match m.job.status with
        | Running -> ControlWithheld
        | Idle
        | Completed
        | Cancelled
        | Failed _ -> ControlOffered

    let snapshotRow : IView =
        DockPanel.create [
            DockPanel.children [
                Border.create [
                    Border.dock Dock.Right
                    Border.verticalAlignment VerticalAlignment.Center
                    Border.isEnabled updateAvailability.isEnabled
                    Border.opacity (match updateAvailability with ControlOffered -> 1.0 | ControlWithheld -> 0.4)
                    Border.child (
                        CommandControls.verbCommand ConstructorUiId.materialSourceUpdateButton (ControlCaption "Update snapshot") (fun () ->
                            dispatch UpdateSourceSnapshot))
                ]
                SelectableTextBlock.create [
                    ConstructorUiId.materialSourceSnapshotText.automationAttribute<SelectableTextBlock> ()
                    ConstructorUiId.materialSourceSnapshotText.automationAttribute<SelectableTextBlock> ()
                    TextBlock.verticalAlignment VerticalAlignment.Center
                    TextBlock.textWrapping TextWrapping.Wrap
                    TextBlock.text (
                        match m.snapshotOpt with
                        | Some version -> $"Snapshot {version.value}"
                        | None -> "No snapshot yet; Update snapshot downloads the database.")
                ]
            ]
        ]

    let progressRows : IView list =
        match m.job.status with
        | Running ->
            let fraction = m.job.progress |> Option.bind _.fraction |> Option.defaultValue 0.0
            [
                DockPanel.create [
                    DockPanel.children [
                        Border.create [
                            Border.dock Dock.Right
                            Border.child (
                                CommandControls.actionCommand
                                    ConstructorUiId.materialSourceUpdateCancelButton
                                    (ControlCaption "Cancel")
                                    NegativeCommand
                                    (fun () -> dispatch CancelSourceSnapshot))
                        ]
                        StackPanel.create [
                            StackPanel.orientation Orientation.Vertical
                            StackPanel.children [
                                ProgressBar.create [
                                    ConstructorUiId.materialSourceUpdateProgressBar.automationAttribute<ProgressBar> ()
                                    ConstructorUiId.materialSourceUpdateProgressBar.automationAttribute<ProgressBar> ()
                                    ProgressBar.minimum 0.0
                                    ProgressBar.maximum 1.0
                                    ProgressBar.value fraction
                                    ProgressBar.isIndeterminate (Option.isNone m.job.progress)
                                ]
                                SelectableTextBlock.create [
                                    ConstructorUiId.materialSourceUpdateProgressText.automationAttribute<SelectableTextBlock> ()
                                    ConstructorUiId.materialSourceUpdateProgressText.automationAttribute<SelectableTextBlock> ()
                                    TextBlock.text (m.job.progressText |> Option.defaultValue "Downloading…")
                                ]
                            ]
                        ]
                    ]
                ] :> IView
            ]
        | Idle
        | Completed
        | Cancelled
        | Failed _ -> []

    // The progress row shares a panel with the snapshot row, so its coming and going never shifts the query box.
    let updateSection : IView =
        StackPanel.create [
            StackPanel.orientation Orientation.Vertical
            StackPanel.spacing 6.0
            StackPanel.children (snapshotRow :: progressRows)
        ]

    let searchTyped (source : obj) : unit =
        match source with
        | :? TextBox as queryBox ->
            let typed = MaterialSourceQuery (string queryBox.Text)
            dispatch (SearchMaterialSource typed)
        | _ -> ()

    let searchRow : IView =
        DockPanel.create [
            DockPanel.children [
                Border.create [
                    Border.dock Dock.Right
                    Border.verticalAlignment VerticalAlignment.Center
                    Border.margin (Thickness(8.0, 0.0, 0.0, 0.0))
                    Border.child (
                        CommandControls.verbCommand ConstructorUiId.materialSourceSearchButton (ControlCaption "Search") (fun () ->
                            dispatch (SearchMaterialSource m.queryDraft)))
                ]
                TextBox.create [
                    yield! ConstructorUiId.materialSourceQueryBox.inputAttributes<TextBox> ()
                    ConstructorUiId.materialSourceQueryBox.automationAttribute<TextBox> ()
                    TextBox.verticalAlignment VerticalAlignment.Center
                    TextBox.text m.queryDraft.value
                    TextBox.onTextChanged (MaterialSourceQuery >> SetSourceQueryDraft >> dispatch)
                    TextBox.onKeyDown ((fun e -> if e.Key = Key.Enter then searchTyped e.Source), SubPatchOptions.Always)
                ]
            ]
        ]

    let hitList : IView =
        StackPanel.create [
            yield! ConstructorUiId.materialSourceHitList.groupAttributes<StackPanel> ()
            yield! ConstructorUiId.materialSourceHitList.groupAttributes<StackPanel> ()
            StackPanel.orientation Orientation.Vertical
            StackPanel.children (m.hits |> List.map (materialSourceHitView dispatch))
        ]

    let statusRows : IView list =
        match m.error, m.status with
        | Some error, _ ->
            [
                SelectableTextBlock.create [
                    ConstructorUiId.materialSourcesStatusText.automationAttribute<SelectableTextBlock> ()
                    ConstructorUiId.materialSourcesStatusText.automationAttribute<SelectableTextBlock> ()
                    TextBlock.foreground (MaterialsWindowView.brush MaterialsWindowView.messageColor)
                    TextBlock.textWrapping TextWrapping.Wrap
                    TextBlock.text error.caption.value
                ] :> IView
            ]
        | None, Some status ->
            [
                SelectableTextBlock.create [
                    ConstructorUiId.materialSourcesStatusText.automationAttribute<SelectableTextBlock> ()
                    ConstructorUiId.materialSourcesStatusText.automationAttribute<SelectableTextBlock> ()
                    TextBlock.textWrapping TextWrapping.Wrap
                    TextBlock.text status.value
                ] :> IView
            ]
        | None, None -> []

    [ choices; updateSection; searchRow; hitList ] @ statusRows

/// The surface as a pure projection of its model, every control under its `ConstructorUiId`; with no source it says
/// only that none is configured (spec 0050 S13.1).
let materialSourcesView (m : MaterialSourcesModel) (dispatch : MaterialSourcesMsg -> unit) : IView =
    let rows : IView list =
        match m.shownSource with
        | Some _ -> materialSourceRows m dispatch
        | None ->
            [
                SelectableTextBlock.create [
                    ConstructorUiId.materialSourcesStatusText.automationAttribute<SelectableTextBlock> ()
                    ConstructorUiId.materialSourcesStatusText.automationAttribute<SelectableTextBlock> ()
                    TextBlock.textWrapping TextWrapping.Wrap
                    TextBlock.text "No online material source is configured."
                ] :> IView
            ]

    ScrollViewer.create [
        ScrollViewer.horizontalScrollBarVisibility ScrollBarVisibility.Auto
        ScrollViewer.verticalScrollBarVisibility ScrollBarVisibility.Auto
        ScrollViewer.content (
            MinWidthPanel.create [
                materialSourcesMinWidth.minWidthPanelAttribute ()
                MinWidthPanel.child (
                    StackPanel.create [
                        StackPanel.orientation Orientation.Vertical
                        StackPanel.margin 12.0
                        StackPanel.spacing 8.0
                        StackPanel.children rows
                    ])
            ])
    ] :> IView
