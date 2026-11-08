/// Opens a kit and its session library without applying pending imports (spec 0050 S4.3).
module OpticalConstructor.Ui.KitOpening

open System
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
    |> Result.bind (fun context ->
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
            | Error closeError -> Logger.logError $"KitOpening.openKitAt: could not close the failed open: {closeError.describe}"
            Error error)

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

/// Creates an independent scratch kit, preserving initialization errors (spec 0050 S4.3; .manual/010).
let scratchKit () : Result<KitContext, KitError> =
    let clock = ClockProvider.system
    let kitId = KitId (Guid.NewGuid ())
    let name = KitName "Scratch kit"
    let author = KitAuthor ""
    let kitFile = KitFile.create kitId name author (clock.now ())
    KitContext.createInMemory kitFile clock
