/// The kit start screen's state, its history messages and its open messages: an update that reaches the kit history,
/// the kit folders and the window only through its injected context, and the view projecting it (spec 0050 S5.2).
module OpticalConstructor.Ui.KitStartView

open Avalonia
open Avalonia.Controls
open Avalonia.Controls.Primitives
open Avalonia.Layout
open Avalonia.Media
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open Softellect.Sys.Primitives
open OpticalConstructor.Domain.CatalogueImport
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.KitArchive
open OpticalConstructor.Domain.KitFolders
open OpticalConstructor.Domain.KitHistory
open OpticalConstructor.Domain.KitLibrary
open OpticalConstructor.Domain.WorkbenchSettings
open OpticalConstructor.Controls
open OpticalConstructor.Ui.KitArchiveJob
open OpticalConstructor.Ui.KitOpening
open OpticalConstructor.Ui.TableAndElementRotationView

/// The start screen's injected seams; the composition root bakes the real IO into them and tests substitute
/// doubles. Function-valued fields compare by reference (spec 0050 S5.2).
[<ReferenceEquality>]
type KitStartContext =
    {
        /// The kit history the screen lists and writes (spec 0050 S5.2).
        kitHistory : KitHistoryProxy
        /// The history's cap a bump evicts down to (spec 0050 S5.2).
        limit : KitHistoryLimit
        /// The clock a bump reads (spec 0050 S5.2).
        clock : ClockProvider
        /// The author a new kit records (spec 0050 S5.2).
        author : KitAuthor
        /// The library's import seam an opened kit's snapshot resolves against (spec 0050 S5.2).
        imports : CatalogueImportContext
        /// The folder dialog of New kit…, Open kit… and Locate… (spec 0050 S5.2).
        picker : KitFolderPicker
        /// Hands an opened kit to the screen's window (spec 0050 S5.2).
        showKit : OpenedKit -> unit
        /// Brings forward the window that already holds a chosen kit, if one does (spec 0050 S5.3).
        activateKitWindow : KitFolder -> KitWindowPresence
        /// Open kit from zip…'s zip dialog; the folder to extract into comes from `picker` (spec 0050 S6).
        zipPicker : KitZipPicker
        /// The zip seam an Open kit from zip… extracts through (spec 0050 S6).
        archive : KitArchiveProxy
        /// Runs the extraction off the UI thread and reports its progress and its end as messages (spec 0050 S6).
        archiveJobs : KitArchiveJobHost
    }

/// The question the screen asks after an open (spec 0050 S5.2).
type KitStartOffer =
    /// No question is pending (spec 0050 S5.2).
    | NoKitStartOffer
    /// Open kit… met a folder without `.kit-json`: offer Make this a kit (spec 0050 S5.2).
    | MakeKitOffer of folder : KitFolder
    /// The kit is open elsewhere, by `holder`: offer to open it read-only (spec 0050 S5.2).
    | ReadOnlyKitOffer of folder : KitFolder * holder : KitLockHolder

/// Locate…'s answer for a missing kit (spec 0050 S5.2).
type KitRelocation =
    {
        /// The folder the history holds, no longer on disk (spec 0050 S5.2).
        missing : KitFolder
        /// The folder Locate… chose for it (spec 0050 S5.2).
        found : KitFolder
    }

/// Open kit from zip…'s two answers: a zip and the folder it is extracted into, then opened (spec 0050 S6).
type KitZipOpening =
    {
        /// The zip to extract (spec 0050 S6).
        zip : FileName
        /// The empty or missing folder the zip is extracted into (spec 0050 S6).
        target : KitFolder
    }

/// The start screen's model; its rows are always the history's `rowsMatching filter` over the context's probe
/// (spec 0050 S5.2).
type KitStartModel =
    {
        /// The seams the update reaches IO through (spec 0050 S5.2).
        context : KitStartContext
        /// The constructor a chosen kit opens in: Main is `ForwardConstructor`, Inverse `InverseConstructor` (spec 0050 S5.2).
        mode : ConstructorMode
        /// The history as last read or written (spec 0050 S5.2).
        history : KitHistory
        /// The filter box's text (spec 0050 S5.2).
        filter : KitHistoryFilter
        /// The listed rows, in display order, each with its folder's presence (spec 0050 S5.2).
        rows : KitHistoryRow list
        /// The open question (spec 0050 S5.2).
        offer : KitStartOffer
        /// The last failure that needs telling (spec 0050 S5.2).
        status : ControlCaption option
        /// The screen's one extraction, idle when none runs (spec 0050 S6).
        zipJob : KitArchiveJobState
        /// What the running extraction extracts, opened once it finishes (spec 0050 S6).
        zipOpeningOpt : KitZipOpening option
    }

/// The start screen's semantic messages: the button and the test send the same one (spec 0050 S5.2).
type KitStartMsg =
    /// Star the kit in the folder (spec 0050 S5.2).
    | StarKit of KitFolder
    /// Unstar the kit in the folder (spec 0050 S5.2).
    | UnstarKit of KitFolder
    /// Remove from history: no undo, the folder is untouched and opening it again re-adds it (spec 0050 S5.2).
    | ForgetKit of KitFolder
    /// Re-point a missing kit's row at the folder Locate… chose (spec 0050 S5.2).
    | LocateKit of KitRelocation
    /// Narrow the rows to the filter's text, writing nothing (spec 0050 S5.2).
    | FilterKitHistory of KitHistoryFilter
    /// New kit… and Make this a kit: open the kit in the folder, or initialise a folder without one, keeping its files
    /// (spec 0050 S5.2).
    | CreateKit of KitFolder
    /// Open kit… and a history row: open only a kit; any other folder is offered Make this a kit (spec 0050 S5.2).
    | OpenKit of KitFolder
    /// The read-only offer's Open read-only, and how a test opens a committed kit; it writes nothing to the kit
    /// (spec 0050 S5.2, .spec-md §3.12).
    | OpenKitReadOnly of KitFolder
    /// An offer's Cancel: drop the question, writing nothing (spec 0050 S5.2).
    | DismissKitStartOffer
    /// The window reports a kit whose resolution failed after it was handed over (spec 0050 S5.2).
    | KitOpenFailed of KitLibraryError
    /// Extracts the zip into the folder, then opens it: the button's message after its two dialogs, and how a test opens
    /// a zip (spec 0050 S6).
    | OpenKitZip of KitZipOpening
    /// The running extraction extracted one more file (spec 0050 S6).
    | OpenKitZipProgressed of JobRunner.JobProgress
    /// The running extraction ended: finished, cancelled or why it failed (spec 0050 S6).
    | OpenKitZipEnded of Result<KitArchiveOutcome, KitArchiveError>
    /// Cancels the running extraction before its next file; its end message returns the screen to idle (spec 0050 S6).
    | CancelOpenKitZip

/// The one listing rule: `history`'s rows under `filter`, probed through the context (spec 0050 S5.2).
let private kitStartRows (context : KitStartContext) (history : KitHistory) (filter : KitHistoryFilter) : KitHistoryRow list =
    history.rowsMatching filter context.kitHistory.probeFolder

/// Writes `change` through the history store: the written history is re-listed with no status, and a refused write
/// keeps the history and rows and tells why (spec 0050 S5.2).
let private kitStartChanged (change : KitHistory -> KitHistory) (m : KitStartModel) : KitStartModel =
    match m.context.kitHistory.changeHistory change with
    | Ok history ->
        {
            m with
                history = history
                rows = kitStartRows m.context history m.filter
                status = None
        }
    | Error error ->
        {
            m with
                status = Some error.notSavedCaption
        }

/// A failed open tells why and drops any question (spec 0050 S5.2).
let private kitStartFailed (error : KitLibraryError) (m : KitStartModel) : KitStartModel =
    {
        m with
            offer = NoKitStartOffer
            status = Some error.openFailedCaption
    }

/// Reads every open's answer for `folder`: an opened kit bumps the history, then is handed to `showKit` once, even when
/// the bump is refused; a kit open elsewhere or a folder without a kit becomes an offer; an error hands nothing over
/// and bumps nothing (spec 0050 S5.2).
let private kitStartOpened (folder : KitFolder) (answer : Result<OpenedKit, KitLibraryError>) (m : KitStartModel) : KitStartModel =
    match answer with
    | Ok opened ->
        let bumped =
            m |> kitStartChanged (fun history -> history.bump m.context.limit m.context.clock folder opened.library.kit.kitFile)
        m.context.showKit opened
        { bumped with offer = NoKitStartOffer }
    | Error (KitLibraryKitErr (KitLockedErr holder)) ->
        {
            m with
                offer = ReadOnlyKitOffer (folder, holder)
                status = None
        }
    | Error (KitLibraryKitErr (NotAKitFolderErr _)) ->
        {
            m with
                offer = MakeKitOffer folder
                status = None
        }
    | Error error -> m |> kitStartFailed error

/// A kit whose window is open is brought forward, and nothing is opened, bumped or handed over: that window holds it,
/// and its entry was bumped when it opened. Otherwise `openKit` runs (spec 0050 S5.3).
let private kitStartChosen (folder : KitFolder) (openKit : unit -> Result<OpenedKit, KitLibraryError>) (m : KitStartModel) : KitStartModel =
    match m.context.activateKitWindow folder with
    | KitWindowActivated ->
        {
            m with
                offer = NoKitStartOffer
                status = None
        }
    | NoKitWindowOpen -> m |> kitStartOpened folder (openKit ())

/// Open kit…'s one path, which an extracted zip's folder also takes (spec 0050 S5.2, S6).
let private kitStartOpenExisting (folder : KitFolder) (m : KitStartModel) : KitStartModel =
    m |> kitStartChosen folder (fun () -> openExistingKitAt folder m.context.clock m.context.imports)

/// The screen as it opens in `mode`: the history read once, the empty filter, its rows, no question, no status and no
/// extraction (spec 0050 S5.2, S6).
let initKitStart (context : KitStartContext) (mode : ConstructorMode) : KitStartModel =
    let history = context.kitHistory.readHistory ()
    let filter = KitHistoryFilter.none

    {
        context = context
        mode = mode
        history = history
        filter = filter
        rows = kitStartRows context history filter
        offer = NoKitStartOffer
        status = None
        zipJob = KitArchiveJobState.idle
        zipOpeningOpt = None
    }

/// Star, unstar, forget and locate write the history through `changeHistory`; the filter only re-lists. The three opens
/// answer through `KitOpening`, and a read-only open bumps the history too, because the user chose the kit (spec 0050 S5.2).
/// Each open first brings forward a window that already holds the kit (spec 0050 S5.3). One extraction runs at a time,
/// and a finished one is opened as Open kit opens its folder (spec 0050 S6).
let updateKitStart (msg : KitStartMsg) (m : KitStartModel) : KitStartModel =
    match msg with
    | StarKit folder -> m |> kitStartChanged (fun history -> history.star folder)
    | UnstarKit folder -> m |> kitStartChanged (fun history -> history.unstar folder)
    | ForgetKit folder -> m |> kitStartChanged (fun history -> history.forget folder)
    | LocateKit relocation -> m |> kitStartChanged (fun history -> history.relocate relocation.missing relocation.found)
    | FilterKitHistory filter ->
        {
            m with
                filter = filter
                rows = kitStartRows m.context m.history filter
        }
    | CreateKit folder ->
        m |> kitStartChosen folder (fun () -> openKitAt folder ReadWriteKitAccess m.context.clock m.context.author m.context.imports)
    | OpenKit folder -> m |> kitStartOpenExisting folder
    | OpenKitReadOnly folder ->
        m |> kitStartChosen folder (fun () -> openKitAt folder ReadOnlyKitAccess m.context.clock m.context.author m.context.imports)
    | DismissKitStartOffer -> { m with offer = NoKitStartOffer }
    | KitOpenFailed error -> m |> kitStartFailed error
    | OpenKitZip opening ->
        if m.zipJob.job.isRunning then
            m
        else
            let handle =
                m.context.archiveJobs.startArchiveJob (fun observer ->
                    m.context.archive.receiveKit opening.zip opening.target observer)
            {
                m with
                    zipJob = m.zipJob.started handle
                    zipOpeningOpt = Some opening
                    offer = NoKitStartOffer
                    status = None
            }
    | OpenKitZipProgressed progress ->
        {
            m with zipJob = m.zipJob.progressed progress
        }
    | OpenKitZipEnded result ->
        let ended =
            {
                m with
                    zipJob = KitArchiveJobState.idle
                    zipOpeningOpt = None
            }
        match result, m.zipOpeningOpt with
        | Ok (KitArchiveFinished _), Some opening -> ended |> kitStartOpenExisting opening.target
        | Ok (KitArchiveFinished _), None -> ended
        | Ok KitArchiveCancelled, _ ->
            {
                ended with status = Some (ControlCaption "Open kit from zip was cancelled: nothing was extracted.")
            }
        | Error error, _ ->
            {
                ended with status = Some (ControlCaption $"The zip could not be opened — {error.describe}")
            }
    | CancelOpenKitZip ->
        m.zipJob.cancel ()
        m

/// The screen lays out at least this wide and scrolls sideways below it, as the material editor does (spec 0050 S5.2).
let private kitStartMinWidth : PanelMinWidth = PanelMinWidth 520.0

/// New kit… and Open kit…: each asks the picker for a folder and sends its open (spec 0050 S5.2). Open kit from zip…
/// asks for the zip, then the folder to extract it into, and a cancel of either sends nothing; it is hidden while its
/// extraction runs (spec 0050 S6).
let private kitStartActionsRow (m : KitStartModel) (dispatch : KitStartMsg -> unit) : IView =
    let openKitZip : IView list =
        if m.zipJob.job.isRunning then
            []
        else
            [
                MaterialsWindowView.verbButton ConstructorUiId.openKitZipButton (ControlCaption "Open kit from zip…") (fun () ->
                    m.context.zipPicker.pickZipToOpenThen (fun zip ->
                        m.context.picker.pickThen (fun target ->
                            let opening : KitZipOpening = { zip = zip; target = target }
                            dispatch (OpenKitZip opening))))
            ]

    StackPanel.create [
        StackPanel.orientation Orientation.Horizontal
        StackPanel.children (
            [
                MaterialsWindowView.verbButton ConstructorUiId.newKitButton (ControlCaption "New kit…") (fun () ->
                    m.context.picker.pickThen (CreateKit >> dispatch))
                MaterialsWindowView.verbButton ConstructorUiId.openKitButton (ControlCaption "Open kit…") (fun () ->
                    m.context.picker.pickThen (OpenKit >> dispatch))
            ]
            @ openKitZip)
    ] :> IView

/// The running extraction's progress row directly under the first row, its Cancel ending the extraction before its next
/// file; nothing while none runs (spec 0050 S6; .spec-md §10.2).
let private kitStartZipRows (m : KitStartModel) (dispatch : KitStartMsg -> unit) : IView list =
    if m.zipJob.job.isRunning then
        let ids : KitArchiveProgressIds =
            {
                bar = ConstructorUiId.openKitZipProgressBar
                text = ConstructorUiId.openKitZipProgressText
                cancel = ConstructorUiId.openKitZipCancelButton
            }
        [ kitArchiveProgressView m.zipJob ids (fun () -> dispatch CancelOpenKitZip) ]
    else
        []

/// An offer's question over its opposing pair on one row, Cancel to the right after the accepting box's margin
/// (.spec-md §10.5). It is keyed by `offerKey`, so another folder's offer recreates the pair: an `actionButton`
/// subscribes its press once per id (spec 0050 S5.2).
let private kitStartOfferBlock (offerKey : ConstructorUiKey) (question : ControlCaption) (accept : IView) (dispatch : KitStartMsg -> unit) : IView =
    StackPanel.create [
        StackPanel.orientation Orientation.Vertical
        StackPanel.children [
            TextBlock.create [
                ConstructorUiId.kitStartOfferText.automationAttribute<TextBlock> ()
                TextBlock.textWrapping TextWrapping.Wrap
                TextBlock.margin (Thickness(0.0, 0.0, 0.0, 4.0))
                TextBlock.text question.value
            ]
            StackPanel.create [
                StackPanel.orientation Orientation.Horizontal
                StackPanel.children [
                    accept
                    MaterialsWindowView.actionButton
                        ConstructorUiId.cancelKitStartOfferButton
                        (ControlCaption "Cancel")
                        MaterialsWindowView.negativeBackground
                        (fun () -> dispatch DismissKitStartOffer)
                ]
            ]
        ]
    ]
    |> Avalonia.FuncUI.DSL.View.withKey offerKey.value
    :> IView

/// Make this a kit for a folder without a kit, Open read-only for a kit held elsewhere; nothing without a question
/// (spec 0050 S5.2).
let private kitStartOfferRows (m : KitStartModel) (dispatch : KitStartMsg -> unit) : IView list =
    match m.offer with
    | NoKitStartOffer -> []
    | MakeKitOffer folder ->
        let offerKey = ConstructorUiKey $"MakeKitOffer_{folder.value.value}"
        let accept =
            MaterialsWindowView.actionButton
                ConstructorUiId.makeThisAKitButton
                (ControlCaption "Make this a kit")
                MaterialsWindowView.positiveBackground
                (fun () -> dispatch (CreateKit folder))
        [ kitStartOfferBlock offerKey folder.notAKitCaption accept dispatch ]
    | ReadOnlyKitOffer (folder, holder) ->
        let offerKey = ConstructorUiKey $"ReadOnlyKitOffer_{folder.value.value}"
        let accept =
            MaterialsWindowView.actionButton
                ConstructorUiId.openKitReadOnlyButton
                (ControlCaption "Open read-only")
                MaterialsWindowView.positiveBackground
                (fun () -> dispatch (OpenKitReadOnly folder))
        [ kitStartOfferBlock offerKey holder.openElsewhereCaption accept dispatch ]

/// The filter box: every change re-lists the rows. A null text (a box not yet given one) is no filter, because
/// `entriesMatching` trims the text (spec 0050 S5.2).
let private kitHistoryFilterBox (m : KitStartModel) (dispatch : KitStartMsg -> unit) : IView =
    TextBox.create [
        ConstructorUiId.kitHistoryFilter.automationAttribute<TextBox> ()
        TextBox.placeHolderText "Filter kits by name or folder"
        TextBox.text m.filter.value
        TextBox.onTextChanged (Option.ofObj >> Option.map KitHistoryFilter >> Option.iter (FilterKitHistory >> dispatch))
    ] :> IView

/// One listed kit at `position`: its star, and Locate… / Remove from history for a missing folder only, on the right;
/// its name, folder and last open in the fill. A present row opens its kit when pressed; a missing row is greyed and
/// inert, its buttons at full opacity (spec 0050 S5.2).
let private kitHistoryRowView (m : KitStartModel) (dispatch : KitStartMsg -> unit) (position : ConstructorUiPosition) (row : KitHistoryRow) : IView =
    let folder = row.entry.folder
    let starToggle = ConstructorUiId.kitHistoryStarToggle position

    let star =
        match row.entry.star with
        | StarredKit -> MaterialsWindowView.verbButton starToggle (ControlCaption "★") (fun () -> dispatch (UnstarKit folder))
        | UnstarredKit -> MaterialsWindowView.verbButton starToggle (ControlCaption "☆") (fun () -> dispatch (StarKit folder))

    let missingVerbs =
        match row.presence with
        | KitFolderPresent -> []
        | KitFolderMissing ->
            [
                MaterialsWindowView.verbButton (ConstructorUiId.kitHistoryLocateButton position) (ControlCaption "Locate…") (fun () ->
                    m.context.picker.pickThen (fun found -> dispatch (LocateKit { missing = folder; found = found })))
                MaterialsWindowView.verbButton (ConstructorUiId.kitHistoryRemoveButton position) (ControlCaption "Remove from history") (fun () ->
                    dispatch (ForgetKit folder))
            ]

    let rowOpacity =
        match row.presence with
        | KitFolderPresent -> 1.0
        | KitFolderMissing -> 0.5

    let rowId = ConstructorUiId.kitHistoryRow position

    let rowBox =
        Border.create [
            rowId.automationAttribute<Border> ()
            Border.background (MaterialsWindowView.brush Colors.Transparent)
            Border.padding (Thickness(4.0, 2.0))
            Border.opacity rowOpacity
            Border.child (
                StackPanel.create [
                    StackPanel.orientation Orientation.Vertical
                    StackPanel.children [
                        TextBlock.create [
                            TextBlock.fontWeight FontWeight.SemiBold
                            TextBlock.text row.entry.name.value
                        ]
                        TextBlock.create [
                            TextBlock.textWrapping TextWrapping.Wrap
                            TextBlock.text folder.value.value
                        ]
                        TextBlock.create [ TextBlock.text row.entry.lastOpenedCaption.value ]
                    ]
                ])
            Border.onPointerPressed (
                (fun e ->
                    e.Handled <- true
                    match row.presence with
                    | KitFolderPresent -> dispatch (OpenKit folder)
                    | KitFolderMissing -> ()),
                SubPatchOptions.Always)
        ]
        |> Avalonia.FuncUI.DSL.View.withKey rowId.value

    DockPanel.create [
        DockPanel.margin (Thickness(0.0, 0.0, 0.0, 6.0))
        DockPanel.children [
            StackPanel.create [
                StackPanel.dock Dock.Right
                StackPanel.orientation Orientation.Horizontal
                StackPanel.verticalAlignment VerticalAlignment.Center
                StackPanel.children (missingVerbs @ [ star ])
            ]
            rowBox
        ]
    ] :> IView

/// The listed kits in display order, each row's ids over its place in the list (spec 0050 S5.2).
let private kitHistoryListView (m : KitStartModel) (dispatch : KitStartMsg -> unit) : IView =
    StackPanel.create [
        ConstructorUiId.kitHistoryList.automationAttribute<StackPanel> ()
        StackPanel.orientation Orientation.Vertical
        StackPanel.children (
            m.rows
            |> List.mapi (fun index row ->
                let position = ConstructorUiPosition index
                kitHistoryRowView m dispatch position row))
    ] :> IView

/// The start screen as a pure projection of its model: New kit…, Open kit… and Open kit from zip…, the running
/// extraction's progress, the status, the offer, the filter box and the history list, every control under its
/// `ConstructorUiId` (spec 0050 S5.2, S6).
let kitStartView (m : KitStartModel) (dispatch : KitStartMsg -> unit) : IView =
    // The last failure, in the library windows' refusal colour.
    let statusRows : IView list =
        match m.status with
        | Some status ->
            [
                TextBlock.create [
                    ConstructorUiId.kitStartStatusText.automationAttribute<TextBlock> ()
                    TextBlock.foreground (MaterialsWindowView.brush MaterialsWindowView.messageColor)
                    TextBlock.textWrapping TextWrapping.Wrap
                    TextBlock.text status.value
                ] :> IView
            ]
        | None -> []

    ScrollViewer.create [
        ScrollViewer.horizontalScrollBarVisibility ScrollBarVisibility.Auto
        ScrollViewer.verticalScrollBarVisibility ScrollBarVisibility.Auto
        ScrollViewer.content (
            MinWidthPanel.create [
                kitStartMinWidth.minWidthPanelAttribute ()
                MinWidthPanel.child (
                    StackPanel.create [
                        StackPanel.orientation Orientation.Vertical
                        StackPanel.margin 12.0
                        StackPanel.spacing 8.0
                        StackPanel.children (
                            [ kitStartActionsRow m dispatch ]
                            @ kitStartZipRows m dispatch
                            @ statusRows
                            @ kitStartOfferRows m dispatch
                            @ [ kitHistoryFilterBox m dispatch; kitHistoryListView m dispatch ])
                    ])
            ])
    ] :> IView
