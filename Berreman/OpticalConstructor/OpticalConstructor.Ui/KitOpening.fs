/// Opens a kit and its session library without applying pending imports (spec 0050 S4.3).
module OpticalConstructor.Ui.KitOpening

open System
open System.Diagnostics
open System.IO
open Avalonia.Controls
open Avalonia.Platform.Storage
open Softellect.Sys.Logging
open Softellect.Sys.Primitives
open OpticalConstructor.Domain
open OpticalConstructor.Domain.CatalogueImport
open OpticalConstructor.Domain.Experiments
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.KitFolders
open OpticalConstructor.Domain.KitLibrary
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.Scene
open OpticalConstructor.Storage
open OpticalConstructor.Storage.KitContentFiles
open OpticalConstructor.Storage.KitFolderStore
open OpticalConstructor.Ui.ImportDialogView

/// Supplies the clock at the kit-opening composition boundary (spec 0050 S4.3).
type ClockProvider with

    /// Reads the current UTC instant on every invocation (spec 0050 S4.3).
    static member system : ClockProvider =
        {
            now = fun () -> ClockTime DateTime.UtcNow
        }

/// A constructor session's opened kit, content and unconfirmed snapshot decisions (spec 0050 S4.3).
[<ReferenceEquality>]
type OpenedKit =
    {
        /// The folder the kit was opened from (spec 0050 S5.3).
        folder : KitFolder
        library : KitLibraryContext
        experimentData : ExperimentData.ExperimentDataProxy
        experiments : Experiment list
        scenes : SceneName list
        decisions : ImportDecision list
        overlay : KitLibraryOverlay
    }

    /// Returns every snapshot decision in its original order, including reuse (spec 0050 S4.3).
    member this.pendingDecisions : ImportDecision list = this.decisions

    /// Reopens the session overlay with confirmed rebindings, preserving the kit and content (spec 0050 S4.3).
    member this.withRebindings (rebindings : EntryRebinding list) : Result<OpenedKit, KitLibraryError> =
        this.library.openOverlay rebindings
        |> Result.map (fun overlay ->
            {
                this with overlay = overlay
            })

    /// Shows the kit at once only when it has no snapshot row; otherwise the import dialog confirms every row, Reuse
    /// included, before `show` (spec 0050 S4.3, S5.2). A cancelled dialog or a failed re-binding releases the kit.
    member this.resolveAndShow (imports : CatalogueImportContext) (show : OpenedKit -> unit) (onFailed : KitLibraryError -> unit) : unit =
        match this.pendingDecisions with
        | [] -> show this
        | _ ->
            let closePendingKit () : unit =
                match this.library.kit.close () with
                | Ok () -> ()
                | Error error -> Logger.logError $"OpenedKit.resolveAndShow: could not close the pending kit: {error.describe}"
            // Spec 0050 S4.3 step 0156: Apply transfers ownership before the dialog's Closed event.
            let mutable wasApplied = false
            let onApplied (applied : ImportDialogApplied) : unit =
                wasApplied <- true
                match this.withRebindings applied.outcome.rebindings with
                | Ok resolved -> show resolved
                | Error error ->
                    closePendingKit ()
                    onFailed error
            let dialog = ImportDialogWindow(imports, ResolveKitSnapshot (this.library, this.decisions), onApplied)
            dialog.Closed.Add(fun _ ->
                if not wasApplied then closePendingKit ())
            dialog.Show()

/// Reads an opened kit's experiments, scenes and snapshot decisions and opens its overlay (spec 0050 S4.3, S5.2).
/// Any failure closes the kit, logging a failed close, and answers the first error.
let private openedOver (folder : KitFolder) (context : KitContext) (imports : CatalogueImportContext) : Result<OpenedKit, KitLibraryError> =
    let library : KitLibraryContext =
        {
            kit = context
            imports = imports
        }
    let result =
        context.proxy.readExperiments ()
        |> Result.mapError KitLibraryKitErr
        |> Result.bind (fun experiments ->
            context.proxy.readSceneNames ()
            |> Result.mapError KitLibraryKitErr
            |> Result.bind (fun scenes ->
                library.resolveSnapshots ()
                |> Result.bind (fun decisions ->
                    library.openOverlay []
                    |> Result.map (fun overlay ->
                        {
                            folder = folder
                            library = library
                            experimentData = ExperimentDataStore.createFileBacked context.proxy
                            experiments = experiments
                            scenes = scenes
                            decisions = decisions
                            overlay = overlay
                        }))))
    match result with
    | Ok kit -> Ok kit
    | Error error ->
        match context.close () with
        | Ok () -> ()
        | Error closeError -> Logger.logError $"KitOpening.openedOver: could not close the failed open: {closeError.describe}"
        Error error

/// Opens or initializes a picked kit, reads its content and resolves snapshots without importing (spec 0050 S4.3).
/// Any failure after opening releases the kit and returns the original failure.
let openKitAt
    (folder : KitFolder)
    (access : KitAccess)
    (clock : ClockProvider)
    (author : KitAuthor)
    (imports : CatalogueImportContext)
    : Result<OpenedKit, KitLibraryError> =
    let proxy = KitProxy.create folder
    let opened =
        match access with
        | ReadWriteKitAccess ->
            let kitId = KitId (Guid.NewGuid ())
            KitContext.openOrInitialise proxy clock (folder.namedKitFile kitId author)
        | ReadOnlyKitAccess -> KitContext.openKit proxy ReadOnlyKitAccess clock
    opened
    |> Result.mapError KitLibraryKitErr
    |> Result.bind (fun context -> openedOver folder context imports)

/// Opens a folder that is already a kit read-write and never initialises one, so Open kit… can offer Make this a kit (spec 0050 S5.2).
/// A folder without .kit-json is NotAKitFolderErr and a kit open elsewhere KitLockedErr; neither writes anything.
let openExistingKitAt
    (folder : KitFolder)
    (clock : ClockProvider)
    (imports : CatalogueImportContext)
    : Result<OpenedKit, KitLibraryError> =
    KitContext.openKit (KitProxy.create folder) ReadWriteKitAccess clock
    |> Result.mapError KitLibraryKitErr
    |> Result.bind (fun context -> openedOver folder context imports)

/// Adapts a window's storage provider to the kit-folder picker seam (spec 0050 S4.3).
type KitFolderPicker with

    /// Picks one local folder; cancellation, non-local selections and provider failures return None (spec 0050 S4.3).
    static member ofWindow (owner : TopLevel) (suggested : FolderName) : KitFolderPicker =
        {
            pickKitFolder = fun () ->
                async {
                    try
                        let options = FolderPickerOpenOptions(Title = "Choose a kit folder", AllowMultiple = false)
                        let! start =
                            StorageProviderExtensions.TryGetFolderFromPathAsync(owner.StorageProvider, suggested.value)
                            |> Async.AwaitTask
                        match Option.ofObj start with
                        | Some folder -> options.SuggestedStartLocation <- folder
                        | None -> ()
                        let! folders = owner.StorageProvider.OpenFolderPickerAsync options |> Async.AwaitTask
                        return
                            folders
                            |> Seq.tryHead
                            |> Option.bind (fun folder ->
                                match folder.TryGetLocalPath () |> Option.ofObj with
                                | Some path ->
                                    let location = FolderName path
                                    let picked = KitFolder location
                                    Some picked
                                | None -> None)
                    with _ -> return None
                }
        }

    /// Starts the picker on the calling UI thread and hands on the folder it answers; a cancel calls nothing (spec 0050 S5.2).
    /// It catches nothing: `ofWindow` already answers None when the dialog cannot open.
    member this.pickThen (onPicked : KitFolder -> unit) : unit =
        async {
            let! folderOpt = this.pickKitFolder ()
            match folderOpt with
            | Some folder -> onPicked folder
            | None -> ()
        }
        |> Async.StartImmediate

/// Whether a constructor window held a chosen kit (spec 0050 S5.3).
type KitWindowPresence =
    /// A window held the kit and was brought forward (spec 0050 S5.3).
    | KitWindowActivated
    /// No window holds the kit (spec 0050 S5.3).
    | NoKitWindowOpen

/// One window per kit, over the host-layer window registry (spec 0050 S5.3).
type KitFolder with

    /// The registry key of the kit's constructor window: every spelling of one folder is one key (spec 0050 S5.3).
    member this.windowKey : WindowLauncher.WindowKey = WindowLauncher.KitWindowKey this.canonical

    /// Brings forward the window registered under `windowKey`, and never opens one: the factory builds no window, so
    /// every answer but an activation is `NoKitWindowOpen` (spec 0050 S5.3).
    member this.activateKitWindow () : KitWindowPresence =
        let launcher =
            WindowLauncher.WindowLauncher.create
                (fun key -> Error (WindowLauncher.WindowNotRegisteredErr key))
                WorkbenchSettings.SelectWindowModality.defaultValue
                WindowLauncher.BrowseOpen
        match launcher.openOrActivate this.windowKey with
        | Ok (WindowLauncher.ActivatedWindow _) -> KitWindowActivated
        | Ok (WindowLauncher.CreatedWindow _)
        | Ok (WindowLauncher.RetargetedWindow _)
        | Error _ -> NoKitWindowOpen

/// Adapts the system's file browser to the explorer seam (spec 0050 S5.3).
type ExplorerProxy with

    /// Starts explorer.exe on the folder's full path and never waits for it; a missing folder is MissingFolderErr,
    /// told and never made (spec 0050 S5.3; .manual/001 §G). Any exception is ExplorerLaunchErr carrying its message.
    static member system : ExplorerProxy =
        {
            showFolder =
                fun folder ->
                    try
                        let fullPath = Path.GetFullPath folder.value
                        match Directory.Exists fullPath with
                        | false -> Error (MissingFolderErr fullPath)
                        | true ->
                            let startInfo = ProcessStartInfo("explorer.exe", UseShellExecute = false)
                            startInfo.ArgumentList.Add fullPath
                            use explorer = Process.Start(startInfo)
                            Ok ()
                    with e -> Error (ExplorerLaunchErr e.Message)
        }

/// Creates an independent scratch kit, preserving initialization errors (spec 0050 S4.3; .manual/010).
let scratchKit () : Result<KitContext, KitError> =
    let clock = ClockProvider.system
    let kitId = KitId (Guid.NewGuid ())
    let name = KitName "Scratch kit"
    let author = KitAuthor ""
    let kitFile = KitFile.create kitId name author (clock.now ())
    KitContext.createInMemory kitFile clock
