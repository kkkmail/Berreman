/// The kit start screen's state and its history messages: a pure update that reaches the kit history only
/// through its injected context (spec 0050 S5.2).
module OpticalConstructor.Ui.KitStartView

open OpticalConstructor.Domain.CatalogueImport
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.KitFolders
open OpticalConstructor.Domain.KitHistory
open OpticalConstructor.Domain.WorkbenchSettings
open OpticalConstructor.Controls
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

/// The screen as it opens in `mode`: the history read once, the empty filter, its rows, no question and no status
/// (spec 0050 S5.2).
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
    }

/// Star, unstar, forget and locate write the history through `changeHistory`; the filter only re-lists (spec 0050 S5.2).
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
