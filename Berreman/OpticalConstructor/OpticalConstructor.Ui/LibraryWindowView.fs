/// Spec 0038 Part F (step 015) — the Library window view (UICOMP_XDUO_0010): the pure MVU model
/// and FuncUI projection behind `LibraryWindow`, the single-instance Library window over the
/// WHOLE `LibraryEntry` corpus — samples, sources, detectors, polarizers. The model projects the
/// LIVE corpus (the shared `SampleProxy` store for samples; the read-only `LibraryProxy` for the
/// preset entries) through the step-011 library facet catalogue
/// (`LibraryFacets.libraryFacets` — the kind facet first, the polarizer-category facet, every
/// material facet lifted to samples, the sample-structural facets) and the step-009/010 engine
/// (`Facets.filter` / `countFor` / `breadcrumbCounts` / `buildTree`,
/// `FacetBuckets.bucketsFor`) into the step-012 domain-free `FacetedTreeControls.State`, beside
/// a view panel (the selected entry's kind, protection and full description) and the former
/// Library bay's verbs rewired to THIS window: Add sample / Make multilayer / Edit open the
/// Sample editor through the injected context launcher (the composition root routes them
/// through the step-008 `WindowLauncher` under `SampleEditorKey`); Remove keeps its inline
/// confirm gate and typed blocks and REFUSES a `ProtectedBuiltIn` entry with a typed reason
/// (spec F.0 — protected entries cannot be deleted). Every projection re-queries the proxies,
/// so any verb's write shows in the same render pass. Step 016 adds the Browse/Select mode
/// (`WindowMode.LibraryWindowMode` — the SAME window in code, never a copy): Select pre-applies
/// the session's kind constraint at the corpus seam (NON-REMOVABLE, no breadcrumb chip) and
/// adds exactly the Select/Close pair; everything else IS the ordinary window, so
/// add-on-the-fly works because it is the library. Pure: `update` only reaches IO through
/// the context's proxy / launcher / close fields — behaviour is testable without a window
/// (tests substitute recording stubs).
module OpticalConstructor.Ui.LibraryWindowView

open System
open Avalonia
open Avalonia.Automation
open Avalonia.Controls
open Avalonia.Input
open Avalonia.Layout
open Avalonia.Media
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Builder
open Avalonia.FuncUI.Types
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Facets
open OpticalConstructor.Domain.FacetBuckets
open OpticalConstructor.Domain.LibraryFacets
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.LibraryBackup
open OpticalConstructor.Domain.CatalogueImport
open OpticalConstructor.Domain.LibraryEntryFiles
open Softellect.Sys.Primitives
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.Lifecycle
open OpticalConstructor.Domain.Placement
open OpticalConstructor.Domain.WindowMode
open OpticalConstructor.Domain.WorkbenchSettings
open OpticalConstructor.Controls

// ---------------------------------------------------------------------------
// Elevated window-local states (named DUs, never bools). The lifecycle action, the
// tree-build request and the expanded-node set are the Materials window's: one entity,
// so the lower declaration serves both windows (spec 0048 R2).
// ---------------------------------------------------------------------------

/// Which remove (if any) awaits its inline confirmation. A pending case CARRIES the id the Remove
/// click targeted, so a selection change between Remove and Confirm can never delete a different
/// entry (the step-013 `MaterialRemoveGate` shape). Only a SAMPLE or (spec 0046) a USER light source
/// can ever arm this gate — a protected entry's Remove refuses at the verb click and a preset has
/// no removal store (see `RequestRemoveSelectedEntry`).
type LibraryRemoveGate =
    | NoPendingLibraryRemove
    | PendingSampleRemove of SampleId
    | PendingSourceRemove of ElementValueId

/// The restore panel: closed, or open over listed backups with an optional picked backup.
type LibraryRestoreGate =
    | NoPendingLibraryRestore
    | PendingLibraryRestore of backups : LibraryBackupName list * pickedOpt : LibraryBackupName option

/// Which lifecycle verb (if any) awaits its inline confirmation, carrying the `SampleId` the verb
/// click targeted (the `LibraryRemoveGate` confirm-gating shape). Only a SAMPLE can arm this — a
/// protected preset exposes no lifecycle verbs at all. Parallel to `removeGate`, never armed at the
/// same time (arming either disarms the other), so the step-015 remove tests stay intact.
type SampleLifecycleGate =
    | NoPendingSampleLifecycle
    | PendingSampleLifecycle of SampleId * MaterialsWindowView.LifecycleAction

/// The window's typed refusals, surfaced as the inline message (errors are values — never a
/// dialog, never a throw). Each case carries its diagnostic payload.
type LibraryWindowError =
    /// Remove was clicked on a `ProtectedBuiltIn` entry (spec 0038 F.0: protected entries
    /// cannot be deleted) — the reason NAMES the entry.
    | ProtectedEntryRefusedErr of reason : string
    /// Remove was clicked on a `UserManaged` NON-sample entry — no preset store exists to
    /// remove from. Unreachable over today's corpus (every non-sample is `ProtectedBuiltIn`),
    /// declared so the protection match is total without a throw.
    | PresetNotRemovableErr of reason : string
    /// The samples store refused the confirmed remove — the typed `SampleError` carries its
    /// own reason.
    | SampleRemoveRefusedErr of SampleError
    /// Spec 0046: the user-sources store refused the confirmed remove.
    | SourceRemoveRefusedErr of SourceError
    /// The samples store refused a confirmed lifecycle verb (spec 0038 step 023 — e.g. the sample
    /// vanished behind the window's back) — the typed `SampleError` carries its own reason.
    | SampleLifecycleRefusedErr of SampleError
    /// The backup store refused a backup, listing or restore request.
    | LibraryBackupRefusedErr of LibraryBackupError
    /// The catalogue import store refused registration or unregistering.
    | CatalogueImportRefusedErr of CatalogueImportError

    /// The refusal's reason as the inline message shows it.
    member this.caption : ControlCaption =
        match this with
        | ProtectedEntryRefusedErr reason
        | PresetNotRemovableErr reason -> ControlCaption reason
        | SampleRemoveRefusedErr sampleError
        | SampleLifecycleRefusedErr sampleError -> sampleError.caption
        | SourceRemoveRefusedErr sourceError -> sourceError.caption
        | LibraryBackupRefusedErr backupError -> backupError.caption
        | CatalogueImportRefusedErr importError -> importError.caption

/// One named tree-shaping representation the window offers (spec 0038 §D.0: search order ≠
/// representation order): a stable `code` the control dispatches back, the picker label, and
/// the facet order handed to the engine's `buildTree`.
type LibraryRepresentation =
    {
        code : ConstructorUiKey
        label : ControlCaption
        order : AttributeKey list
    }

/// The seeded default representation "By kind" (spec 0038 step 015) — the step-011 catalogue
/// order: the kind facet first, the polarizer-category facet (present only over polarizer
/// populations), every material facet lifted to samples, then the sample-structural facets.
/// Applied constraints never reshuffle it.
let byKindRepresentation : LibraryRepresentation =
    {
        code = ConstructorUiKey "by-kind"
        label = ControlCaption "By kind"
        order =
            [
                entryKindFacetKey
                polarizerCategoryFacetKey
                materialCategoryKey
                materialAnisotropyKey
                materialDispersionKey
                materialTransparencyKey
                materialDispersionModelKey
                materialGyrationClassKey
                materialHandednessKey
                materialMagneticKey
                sampleSubstrateMaterialKey
                sampleHasThinFilmsKey
                sampleFilmMaterialKey
                sampleFilmThicknessKey
            ]
    }

/// The representations offered. ONE this round — the slice seeds "By kind" only; a later step
/// adds alternatives without a shape change (the step-013 two-representation precedent).
let offeredLibraryRepresentations : LibraryRepresentation list =
    [ byKindRepresentation ]

/// The corpus group's tree-node code (`entries`); its leaves are `ElementValueId.libraryEntryCode`.
let private libraryEntriesCode : ConstructorUiKey = ConstructorUiKey "entries"

// ---------------------------------------------------------------------------
// The window's IO seam (the functional-proxy Context convention).
// ---------------------------------------------------------------------------

/// The window's context: the read-only Library seam (the preset entries), the app-scope
/// samples write store, materials, backups and catalogue import seam, the Domain
/// tree-gating threshold and bucket cap, and the Sample-editor launcher (the composition root —
/// `LibraryWindow` — bakes the step-008 `WindowLauncher` `SampleEditorKey` into it; tests
/// substitute recording stubs). Function-valued fields have no structural equality, so the
/// record compares by reference.
[<ReferenceEquality>]
type LibraryWindowContext =
    {
        library : LibraryProxy
        samples : SampleProxy
        materials : MaterialProxy
        /// Live category store for facet names (spec 0050 S1.7).
        categories : CategoryProxy
        /// The app-scope library backup and restore store.
        libraryBackups : LibraryBackupProxy
        /// The app-scope catalogue import seam.
        catalogueImports : CatalogueImportContext
        treeAutoBuildThreshold : TreeAutoBuildThreshold
        thicknessBucketCap : ThicknessBucketCap
        /// Open the Sample editor by intent: `NewBlankSample` / `NewSeededMultilayer` (the Add /
        /// Make-multilayer verbs mint the id AT dispatch — spec 0038 step 008) or `EditSample`.
        openSampleEditor : SampleEditorView.SampleEditorIntent -> unit
        /// Spec 0046: the app-scope USER-SOURCES write store — its sources join the corpus, and the
        /// Remove verb deletes from it.
        sources : SourceProxy
        /// Spec 0046: open the light-source editor by intent: `NewSource` (the Add source verb mints
        /// the entry id AT dispatch) or `EditSource` (a user source).
        openSourceEditor : LightSourceEditorView.SourceEditorIntent -> unit
        /// Close THIS window (the host passes `this.Close`; tests substitute a recording stub —
        /// the `SampleEditorContext` precedent). The Select-state verbs reach it: 'Select'
        /// closes after `onSelected`, 'Close' after `onCancelled` (spec 0038 step 016).
        requestClose : unit -> unit
    }

// ---------------------------------------------------------------------------
// Live projections (each pass re-queries the proxies).
// ---------------------------------------------------------------------------

/// The non-sample catalogue kinds the read-only `LibraryProxy` serves entries for.
let private presetKinds : CatalogueKind list =
    [ LightSource; Detector; LinearPolarizer; CircularPolarizer ]

/// The WHOLE live entry corpus of one projection pass: the samples from the LIVE shared
/// `SampleProxy` store (so a sample Add / Edit / Remove — this window's own verbs or another
/// window's — shows on the next pass), then the preset entries (sources, detectors,
/// polarizers) from the read-only `LibraryProxy`, deduped by `entryId` (a compound/custom
/// polarizer serves BOTH polarizer kinds — spec 0038 step 014 — so `entriesForKind` can return
/// it twice). The proxy's own STATIC seeded samples are excluded in favour of the live store.
let liveEntries (context : LibraryWindowContext) (scope : InactiveVisibility) : LibraryEntry list =
    let samples =
        match context.samples.listSamples scope with
        | Ok stored -> stored |> List.map SampleItem
        | Error _ -> []
    let presets =
        presetKinds
        |> List.collect (fun kind ->
            match context.library.entriesForKind kind with
            | Ok entries -> entries
            | Error _ -> [])
        |> List.filter (fun entry ->
            match entry with
            | SampleItem _ -> false
            | SourceItem _ | DetectorItem _ | PolarizerItem _ -> true)
    // Spec 0046: the user light sources (the source editor's store) follow the presets; the
    // `distinctBy` keeps one copy when the injected Library is the live one that already lists them.
    let userSources =
        match context.sources.listSources () with
        | Ok stored -> stored |> List.map SourceItem
        | Error _ -> []
    samples @ (presets @ userSources |> List.distinctBy (fun entry -> entry.entryId))

/// Enumerates stored versions in ascending order by resolving each version until absent;
/// stops at 1000 to bound a malformed proxy (spec 0038 step 023; spec 0050 S2.2).
let selectedVersionsOf (samples : SampleProxy) (id : SampleId) : (VersionNumber * Sample) list =
    let rec loop (version : VersionNumber) (acc : (VersionNumber * Sample) list) : (VersionNumber * Sample) list =
        if version.value > 1000 then List.rev acc
        else
            match samples.resolveVersion { sampleId = id; version = version } with
            | Ok (Some sample) -> loop version.next ((version, sample) :: acc)
            | Ok None | Error _ -> List.rev acc
    loop VersionNumber.first []

/// The window's pure model. The engine inputs (corpus, facet defs) are NOT cached here — every
/// projection re-queries the context's proxies, so a verb's write (or another window's) shows
/// in the same render pass.
type LibraryWindowModel =
    {
        context : LibraryWindowContext
        /// Browse (the ordinary window) or Select (spec 0038 step 016): the SAME window
        /// pre-constrained to the session context's kind — the corpus seam applies the
        /// constraint, so it is structurally NON-REMOVABLE (no breadcrumb chip) — plus the
        /// Select/Close pair. A `RetargetLibrarySelect` re-points a live window's session; a
        /// resolved or cancelled session flips back to Browse so the host's close hook can never
        /// fire `onCancelled` after `onSelected`.
        mode : LibraryWindowMode<LibraryEntry>
        /// The applied facet constraints, in application (breadcrumb) order — at most one per
        /// facet key (this window applies single-key selections; the engine's key-set OR stays
        /// available to a later slice). A numeric selection is an ordinary member.
        appliedFacets : AppliedConstraint list
        /// The committed text filter (the box's draft echoes it; Enter/LostFocus commit only).
        textFilter : TextQuery
        /// The active tree-shaping representation.
        representation : LibraryRepresentation
        /// Whether the user has explicitly materialized a gated tree.
        buildRequest : MaterialsWindowView.TreeBuildRequest
        /// The tree-node codes the user has explicitly expanded (spec 0040 step 002) — the tree is
        /// collapsed by default, so this starts empty and every top-level node opens collapsed.
        expandedNodes : MaterialsWindowView.ExpandedNodes
        /// The selected entry (the Edit / Remove verbs' target and the view panel's subject); a
        /// sample's `SampleId` crosses as `SampleId.toElementValueId`, the `LibraryEntry.entryId` seam.
        selectedEntryId : ElementValueId option
        /// The EXACT tree-node code of the selection — the control highlights the row whose code
        /// matches, so an entry picked THROUGH a facet branch highlights that branch row, not the
        /// corpus-group copy (spec 0040 step 003). `None` = nothing selected. `selectedEntryId`
        /// stays the domain selection; this is only its row.
        selectedNodeOpt : ConstructorUiKey option
        /// Whether the show-inactive/superseded toggle is on (spec 0038 step 023): `ActiveOnly`
        /// (the default — pickers/facet counts exclude retired samples) or `IncludeInactive` (the
        /// toggle adds retired samples to the tree, badged). The Domain DU, never a bool. Presets
        /// carry no lifecycle, so the toggle only affects the sample half of the corpus. Select
        /// mode ignores this and always lists `ActiveOnly` (a retired entry is never a valid pick).
        showInactive : InactiveVisibility
        /// Which sample or source remove (if any) awaits its inline confirmation.
        removeGate : LibraryRemoveGate
        /// Which lifecycle verb (if any) awaits its inline confirmation.
        lifecycleGate : SampleLifecycleGate
        /// The listed backups and optional pick awaiting restore confirmation.
        restoreGate : LibraryRestoreGate
        /// The most recent successful backup or restore, naming its backups.
        backupStatus : ControlCaption option
        /// The latest catalogue-folder registration or unregister status.
        importStatus : ControlCaption option
        /// Registrations whose complete row lists await the import dialog.
        pendingImports : PendingCatalogueImport list
        /// Which OLDER version of the selected SAMPLE the view panel is showing read-only
        /// (`None` = the latest, the editable default; `Some v` = version `v` view-only — spec
        /// 0038 step 023). Reset to `None` on every selection change.
        viewedVersion : VersionNumber option
        /// The last typed refusal, surfaced as the inline message — never a cascade, never a
        /// dialog.
        lastError : LibraryWindowError option
    }

    /// The scope the corpus lists samples at (spec 0038 step 023): `ActiveOnly` in Select mode (a
    /// retired sample is never a valid pick target), else the window's `showInactive` toggle. The
    /// default `ActiveOnly` keeps offers, facet counts and Select byte-for-byte the step-022 behaviour.
    member this.effectiveScope : InactiveVisibility =
        match this.mode with
        | Select _ -> ActiveOnly
        | Browse -> this.showInactive

    /// The corpus one projection pass works over: the live entries — in Select state pre-narrowed
    /// to the session's kind constraint (spec 0038 step 016). The constraint is applied at THIS
    /// corpus seam, so it is structurally NON-REMOVABLE: no breadcrumb chip exists to remove, and
    /// every count, offer, tree row and selection already lives inside it.
    /// `LibraryEntry.forKinds` is the existing kind-eligibility rule (a compound/custom polarizer
    /// serves BOTH polarizer kinds — spec 0038 step 014), never re-derived.
    member this.constrainedEntries : LibraryEntry list =
        let entries = liveEntries this.context this.effectiveScope
        match this.mode with
        | Browse -> entries
        | Select context -> entries |> List.filter (fun e -> e.forKinds |> List.contains context.kindConstraint.value)

    /// The selected entry resolved through the LIVE (and, in Select state, kind-constrained)
    /// corpus — a removed sample's selection resolves to nothing, so its panel vanishes with its
    /// row, and a Select session can only ever return an entry inside its constraint.
    member this.selectedEntry : LibraryEntry option =
        match this.selectedEntryId with
        | Some id -> this.constrainedEntries |> List.tryFind (fun e -> e.entryId = id)
        | None -> None

    /// The selected entry as an editable SAMPLE — the Edit verb's target. Only samples have an
    /// editor; a preset selection offers NO Edit affordance (removed, not greyed — the
    /// `MaterialsControls` discipline).
    member this.editableSample : Sample option =
        match this.selectedEntry with
        | Some (SampleItem sample) -> Some sample
        | Some (SourceItem _ | DetectorItem _ | PolarizerItem _) | None -> None

    /// Spec 0046: the selected entry as an editable USER light source — the Edit verb's other target. A
    /// protected seeded source has no editor (its Edit is absent, like any preset's).
    member this.editableSource : SourcePreset option =
        match this.selectedEntry with
        | Some (SourceItem source) ->
            match source.protection with
            | UserManaged -> Some source
            | ProtectedBuiltIn -> None
        | Some (SampleItem _ | DetectorItem _ | PolarizerItem _) | None -> None

    /// The sample ids whose LATEST version is active (the `ActiveOnly` listing — read live every pass).
    /// A sample present in `IncludeInactive` but NOT here has a retired latest version; that is how the
    /// window distinguishes an inactive sample to badge it and to offer Mark active.
    member internal this.activeSampleIds : Set<SampleId> =
        match this.context.samples.listSamples ActiveOnly with
        | Ok samples -> samples |> List.map (fun s -> s.id) |> Set.ofList
        | Error _ -> Set.empty

    /// A SAMPLE's lifecycle (spec 0038 step 023): `ActiveEntry` when its latest version is in the
    /// active set, `InactiveEntry` when it resolves but its latest is retired, `None` when it no
    /// longer resolves.
    member this.sampleLifecycle (id : SampleId) : EntryLifecycle option =
        match this.context.samples.tryGetSample id with
        | Ok (Some _) ->
            if Set.contains id this.activeSampleIds then Some ActiveEntry else Some InactiveEntry
        | Ok None | Error _ -> None

    /// The lifecycle verbs offered for the current selection (spec 0038 step 023) — the ONE source of
    /// truth the verbs row renders from and the tests assert against. `ProtectedBuiltIn` entries (the
    /// presets) offer NONE (removed, not greyed — requirement 2); a `UserManaged` non-sample has no
    /// version store, so none either; a selected SAMPLE offers Mark inactive + Supersede… when active,
    /// Mark active when retired. Any Select-mode selection offers none.
    member this.offeredLifecycleActions : MaterialsWindowView.LifecycleAction list =
        match this.mode with
        | Select _ -> []
        | Browse ->
            match this.selectedEntry with
            | None -> []
            | Some entry ->
                match entry.protection with
                | ProtectedBuiltIn -> []
                | UserManaged ->
                    match entry with
                    | SampleItem sample ->
                        match this.sampleLifecycle sample.id with
                        | Some ActiveEntry -> [ MaterialsWindowView.MarkInactiveAction; MaterialsWindowView.SupersedeAction ]
                        | Some InactiveEntry -> [ MaterialsWindowView.MarkActiveAction ]
                        | None -> []
                    | SourceItem _ | DetectorItem _ | PolarizerItem _ -> []

    /// The number of samples whose latest version is retired (spec 0038 step 023) — the visible count
    /// badge on the show-inactive toggle. Read over the whole samples store (not the filtered corpus),
    /// so the badge is a stable "N retired exist" hint whether the toggle is on or off.
    member this.inactiveCount : ItemCount =
        let active = this.activeSampleIds
        match this.context.samples.listSamples IncludeInactive with
        | Ok all -> all |> List.filter (fun s -> not (Set.contains s.id active)) |> List.length |> ItemCount
        | Error _ -> ItemCount 0

    /// The selected SAMPLE's versions (empty for a preset, no selection, or a vanished id).
    member this.selectedVersions : (VersionNumber * Sample) list =
        match this.editableSample with
        | Some sample -> selectedVersionsOf this.context.samples sample.id
        | None -> []

    /// A query / selection change also disarms a pending remove confirmation and clears the inline
    /// message — a stale confirm or refusal never outlives the state it referred to (the retired
    /// bay's disarm discipline).
    member internal this.disarmed : LibraryWindowModel =
        { this with removeGate = NoPendingLibraryRemove; lifecycleGate = NoPendingSampleLifecycle; lastError = None }

    /// Spec 0046: a Select session opens with the entry its target is bound to now shown as CHOSEN —
    /// selected, highlighted, and the entries group expanded so its row is visible. Nothing to show (no
    /// binding, or one outside the session's kind) leaves the model as it is.
    member internal this.showCurrentEntry : LibraryWindowModel =
        match this.mode with
        | Select session ->
            match session.currentEntryIdOpt with
            | Some id when this.constrainedEntries |> List.exists (fun e -> e.entryId = id) ->
                let expanded =
                    match this.expandedNodes.nodeExpansion libraryEntriesCode with
                    | FacetedTreeControls.ExpandedNode -> this.expandedNodes
                    | FacetedTreeControls.CollapsedNode -> this.expandedNodes.toggle libraryEntriesCode
                { this with selectedEntryId = Some id; selectedNodeOpt = Some id.libraryEntryCode; expandedNodes = expanded }
            | Some _ | None -> this
        | Browse -> this

/// The window's model as it opens, before a Select session's current entry is shown (`initLibraryWindow`).
let private initModel (context : LibraryWindowContext) (mode : LibraryWindowMode<LibraryEntry>) : LibraryWindowModel =
    {
        context = context
        mode = mode
        appliedFacets = []
        textFilter = TextQuery ""
        representation = byKindRepresentation
        buildRequest = MaterialsWindowView.NoTreeBuildRequest
        expandedNodes = MaterialsWindowView.ExpandedNodes Set.empty
        selectedEntryId = None
        selectedNodeOpt = None
        showInactive = ActiveOnly
        removeGate = NoPendingLibraryRemove
        lifecycleGate = NoPendingSampleLifecycle
        restoreGate = NoPendingLibraryRestore
        backupStatus = None
        importStatus = None
        pendingImports = []
        viewedVersion = None
        lastError = None
    }

type LibraryWindowMsg =
    /// Apply the selection as this facet's (single) constraint — an offered-value or bucket
    /// click, or a parsed manual min–max entry; re-applying a constrained facet REPLACES its
    /// selection.
    | ApplyFacetConstraint of AttributeKey * FacetSelection
    /// Remove the applied constraint of this facet — a breadcrumb-chip click.
    | RemoveLibraryFacet of AttributeKey
    /// Commit the filter box's text (Enter/LostFocus only — never per keystroke).
    | CommitLibraryTextFilter of TextQuery
    /// Choose the named representation with this code (tree reshaping only — the applied
    /// constraints and the result set are untouched: search order ≠ representation order).
    | ChooseLibraryRepresentation of ConstructorUiKey
    /// Materialize the gated tree (the Show/Search button).
    | RequestLibraryTreeBuild
    /// Toggle one tree node's expansion (a disclosure-chevron press — spec 0040 step 002).
    | ToggleLibraryNode of ConstructorUiKey
    /// Select the entry leaf with this `entryId` (the view panel's subject). The selection
    /// highlights the CORPUS-group row (the canonical `entry:<entryId>` code) — the id-only path
    /// for programmatic / verb-driven selection.
    | SelectLibraryEntry of ElementValueId
    /// Select an entry leaf by its EXACT tree-node code (the `entryId` it resolves to, plus the
    /// code the user actually clicked). Highlights that node — so picking an entry through a facet
    /// branch highlights the branch row, not the corpus-group copy (spec 0040 step 003).
    | SelectLibraryEntryNode of ElementValueId * ConstructorUiKey
    /// The verbs, rewired from the retired Library bay (spec 0038 step 015).
    | AddSample
    | MakeMultilayer
    /// Spec 0046: open the light-source editor on a NEW user source.
    | AddSource
    | EditSelectedEntry
    | RequestRemoveSelectedEntry
    | ConfirmEntryRemove
    | CancelEntryRemove
    /// Back up the whole library and report the backup's name.
    | BackUpLibraryNow
    /// List available backups and open the restore confirmation panel.
    | RequestLibraryRestore
    /// Pick one of the backups listed in the open restore panel.
    | PickLibraryBackup of LibraryBackupName
    /// Restore the picked backup, preserving the current library first.
    | ConfirmLibraryRestore
    /// Close the restore panel without restoring anything.
    | CancelLibraryRestore
    /// Inspect a catalogue folder and hold every import row for confirmation.
    | RegisterCatalogueFolder of FolderName
    /// Forget a registered folder while keeping all its library entries.
    | UnregisterCatalogueFolder of CatalogueId
    /// The show-inactive/superseded toggle (spec 0038 step 023): flip the sample scope between
    /// `ActiveOnly` and `IncludeInactive`.
    | ToggleShowInactiveEntries
    /// The lifecycle verbs on the selected SAMPLE, confirm-gated inline (spec 0038 step 023).
    /// `Request…` arms the gate; `ConfirmSampleLifecycle` runs the matching proxy verb (a refusal
    /// surfaces as the typed inline message); `CancelSampleLifecycle` disarms.
    | RequestMarkSampleInactive
    | RequestMarkSampleActive
    | RequestSupersedeSample
    | ConfirmSampleLifecycle
    | CancelSampleLifecycle
    /// Show an OLDER version of the selected sample read-only in the view panel (spec 0038 step 023).
    | ViewSampleVersion of VersionNumber
    /// The Select-state pair (spec 0038 step 016). 'Select' returns the HIGHLIGHTED entry
    /// through the session's `onSelected` (a targeted dispatch), then closes; no highlight →
    /// inert. 'Close' fires `onCancelled`, then closes.
    | ConfirmLibrarySelect
    | CancelLibrarySelect
    /// A Select-state open met this LIVE window: re-point the session at the new context
    /// (the superseded session is cancelled — a second Choose closes the first, logically)
    /// and clear the highlight; the window itself stays (the launcher's re-target seam).
    | RetargetLibrarySelect of SelectionContext<LibraryEntry>
    /// The host window CLOSED (the title-bar X, or a staleness `Close()` from the requesting
    /// surface): cancel a still-pending session exactly once — a session already resolved by
    /// Select/Close flipped the mode to Browse first, so this can never double-fire.
    | LibrarySelectDismissed

/// The text filter's facet key (the filter is an ORDINARY engine constraint — spec 0038 §D.0 —
/// over the entry's display name; its UI surface stays the filter box, not a chip).
let libraryTextFilterKey : AttributeKey = AttributeKey "library-text"

/// One projection pass's engine inputs, read from the live proxies exactly once.
type private LibraryProjectionInputs =
    {
        defs : AttributeDef<LibraryEntry> list
        /// The committed text filter as an applied constraint — empty query applies nothing.
        textApplied : AppliedConstraint list
        /// The FULL applied list: the text filter (first) then the facet chips in order.
        appliedAll : AppliedConstraint list
        corpus : LibraryEntry list
    }

let private projectionInputs (m : LibraryWindowModel) : LibraryProjectionInputs =
    let categories =
        match m.context.categories.listCategories () with
        | Ok entries -> entries
        | Error _ -> []
    let materialCorpus =
        match m.context.materials.listMaterials ActiveOnly with
        | Ok entries -> entries
        | Error _ -> []
    let textDef = textFilterDef libraryTextFilterKey (FacetCaption "Text") (fun (e : LibraryEntry) -> e.entryName) m.textFilter
    let defs = textDef :: libraryFacets categories materialCorpus
    let textApplied =
        match m.textFilter.value with
        | "" -> []
        | _ -> [ textFilterConstraint libraryTextFilterKey ]
    {
        defs = defs
        textApplied = textApplied
        appliedAll = textApplied @ m.appliedFacets
        corpus = m.constrainedEntries
    }

type LibraryWindowModel with

    /// The result set under everything applied (text filter AND facet constraints) — public so
    /// narrowing is unit-testable without a window.
    member this.filteredEntries : LibraryEntry list =
        let inputs = projectionInputs this
        Facets.filter inputs.defs inputs.appliedAll inputs.corpus

/// An entry's lifecycle against the active sample-id set, for the tree-leaf badge (spec 0038 step
/// 023); presets never retire.
let private libraryEntryLifecycle (activeIds : Set<SampleId>) (entry : LibraryEntry) : EntryLifecycle =
    match entry with
    | SampleItem sample -> if Set.contains sample.id activeIds then ActiveEntry else InactiveEntry
    | SourceItem _ | DetectorItem _ | PolarizerItem _ -> ActiveEntry

/// The window's model as it opens — in a Select session, with the target's current entry shown as chosen.
let initLibraryWindow (context : LibraryWindowContext) (mode : LibraryWindowMode<LibraryEntry>) : LibraryWindowModel =
    (initModel context mode).showCurrentEntry

// ---------------------------------------------------------------------------
// Pure update.
// ---------------------------------------------------------------------------

/// Arm the lifecycle confirm gate on the selected SAMPLE (spec 0038 step 023) — clears the remove
/// gate and the inline message so at most one confirm is ever pending. A non-sample or empty
/// selection is inert (its verbs never render).
let private requestLifecycle (action : MaterialsWindowView.LifecycleAction) (m : LibraryWindowModel) : LibraryWindowModel =
    match m.editableSample with
    | Some sample ->
        {
            m with
                lifecycleGate = PendingSampleLifecycle (sample.id, action)
                removeGate = NoPendingLibraryRemove
                restoreGate = NoPendingLibraryRestore
                lastError = None
        }
    | None -> m

let updateLibraryWindow (msg : LibraryWindowMsg) (m : LibraryWindowModel) : LibraryWindowModel =
    match msg with
    | RegisterCatalogueFolder folder ->
        match m.context.catalogueImports.registerFolder folder with
        | Ok (CatalogueImported entry) ->
            {
                m with
                    importStatus = Some (ControlCaption $"Registered catalogue folder {folder.value} ({entry.entryCount.value} entries).")
                    lastError = None
            }
        | Ok (CatalogueAwaitsChoices pending) ->
            {
                m with
                    pendingImports = m.pendingImports @ [ pending ]
                    importStatus = Some (ControlCaption $"Review {folder.value}; {pending.decisions.Length} entries await confirmation. Nothing has been imported or registered.")
                    lastError = None
            }
        | Ok (CatalogueUnchanged _) ->
            {
                m with
                    importStatus = Some (ControlCaption $"Catalogue folder {folder.value} is unchanged.")
                    lastError = None
            }
        | Error error ->
            {
                m with
                    lastError = Some (CatalogueImportRefusedErr error)
                    importStatus = None
            }
    | UnregisterCatalogueFolder id ->
        let forgotten =
            m.context.catalogueImports.proxy.registeredFolders ()
            |> Result.bind (fun entries ->
                match entries |> List.tryFind (fun entry -> entry.catalogueId = id) with
                | Some entry ->
                    m.context.catalogueImports.proxy.forgetImport id
                    |> Result.map (fun () -> entry.folder)
                | None -> Error (ImportRejectedErr $"catalogue {id.value} is not a registered folder"))
        match forgotten with
        | Ok folder ->
            {
                m with
                    importStatus = Some (ControlCaption $"Unregistered {folder.value}. Its entries stay in the library.")
                    lastError = None
            }
        | Error error ->
            {
                m with
                    lastError = Some (CatalogueImportRefusedErr error)
                    importStatus = None
            }
    | ApplyFacetConstraint (key, selection) ->
        // Single selection per facet: re-applying a constrained facet REPLACES its chip
        // (removed from its old breadcrumb position, re-applied at the end).
        let withoutKey = m.appliedFacets |> List.filter (fun c -> c.key <> key)
        { m.disarmed with appliedFacets = withoutKey @ [ { key = key; selection = selection } ] }
    | RemoveLibraryFacet key ->
        { m.disarmed with appliedFacets = m.appliedFacets |> List.filter (fun c -> c.key <> key) }
    | CommitLibraryTextFilter text ->
        { m.disarmed with textFilter = text }
    | ChooseLibraryRepresentation code ->
        // Reshapes the tree only — never the applied constraints or the result set.
        match offeredLibraryRepresentations |> List.tryFind (fun r -> r.code = code) with
        | Some r -> { m with representation = r }
        | None -> m
    | RequestLibraryTreeBuild ->
        { m with buildRequest = MaterialsWindowView.RequestedTreeBuild }
    | ToggleLibraryNode code ->
        // Flip one tree node's expansion (a disclosure-chevron press) — a display-only change, so
        // no gate is disarmed and no selection moves.
        { m with expandedNodes = m.expandedNodes.toggle code }
    | SelectLibraryEntry id ->
        // A new selection shows its LATEST version (the editable default) and disarms both gates.
        // The id-only path highlights the corpus-group row (the canonical `entry:<entryId>` code).
        { m.disarmed with selectedEntryId = Some id; selectedNodeOpt = Some id.libraryEntryCode; viewedVersion = None }
    | SelectLibraryEntryNode (id, code) ->
        // The same selection, but highlighting the EXACT clicked node — so an entry picked through
        // a facet branch highlights the branch row rather than the corpus-group copy.
        { m.disarmed with selectedEntryId = Some id; selectedNodeOpt = Some code; viewedVersion = None }
    | AddSample ->
        // Add mints the sample's SampleId HERE — at the window-open dispatch, off the save path
        // (spec 0038 step 008) — so the launcher's registry keys the new editor by the SAME id
        // its Save will persist under (a second Add mints a second id: two windows).
        m.context.openSampleEditor (SampleEditorView.NewBlankSample (newSampleId ()))
        m.disarmed
    | MakeMultilayer ->
        // The DISTINCT Make-multilayer creation path (spec 0035 step 014): a NEW sample
        // pre-seeded with a foldable starter period — still minted at open, just not blank.
        m.context.openSampleEditor (SampleEditorView.NewSeededMultilayer (newSampleId ()))
        m.disarmed
    | AddSource ->
        // Spec 0046: the source's entry id is minted HERE, at the window-open dispatch (the Add
        // sample precedent), so the launcher keys the new editor by the id its Save stores under.
        m.context.openSourceEditor (LightSourceEditorView.NewSource (newSourceId ()))
        m.disarmed
    | EditSelectedEntry ->
        // Edit opens the Sample editor on the selected SAMPLE, or (spec 0046) the source editor on a
        // selected USER source, resolved through the live corpus at dispatch time; a vanished or
        // preset selection reaches no launcher (the verb is simply absent for it — removed, not
        // greyed).
        (match m.editableSample, m.editableSource with
         | Some sample, _ -> m.context.openSampleEditor (SampleEditorView.EditSample sample)
         | None, Some source -> m.context.openSourceEditor (LightSourceEditorView.EditSource source)
         | None, None -> ())
        m.disarmed
    | RequestRemoveSelectedEntry ->
        match m.selectedEntry with
        | None -> m
        | Some entry ->
            match entry.protection with
            | ProtectedBuiltIn ->
                // Spec 0038 F.0: a protected built-in cannot be deleted — the verb REFUSES with
                // the typed reason AT the click, so the confirm gate never arms for it.
                { m with
                    removeGate = NoPendingLibraryRemove
                    lastError = Some (ProtectedEntryRefusedErr $"'%s{entry.displayName}' is a protected built-in entry — it cannot be removed.") }
            | UserManaged ->
                match entry with
                | SampleItem sample ->
                    {
                        m with
                            removeGate = PendingSampleRemove sample.id
                            lifecycleGate = NoPendingSampleLifecycle
                            restoreGate = NoPendingLibraryRestore
                            lastError = None
                    }
                | SourceItem source ->
                    {
                        m with
                            removeGate = PendingSourceRemove source.id
                            lifecycleGate = NoPendingSampleLifecycle
                            restoreGate = NoPendingLibraryRestore
                            lastError = None
                    }
                | DetectorItem _ | PolarizerItem _ ->
                    // Unreachable over today's corpus (every detector / polarizer seed is
                    // protected) — total handling, never a throw.
                    { m with
                        removeGate = NoPendingLibraryRemove
                        lastError = Some (PresetNotRemovableErr $"'%s{entry.displayName}' has no removal store — preset entries cannot be removed here.") }
    | ConfirmEntryRemove ->
        match m.removeGate with
        | PendingSampleRemove id ->
            match m.context.samples.removeSample id with
            | Ok () ->
                { m with
                    removeGate = NoPendingLibraryRemove
                    lastError = None
                    selectedEntryId = (if m.selectedEntryId = Some id.toElementValueId then None else m.selectedEntryId)
                    selectedNodeOpt = (if m.selectedEntryId = Some id.toElementValueId then None else m.selectedNodeOpt) }
            | Error err ->
                // The store refused (a typed SampleError) — surface the reason inline and leave
                // the store, selection and list untouched.
                { m with removeGate = NoPendingLibraryRemove; lastError = Some (SampleRemoveRefusedErr err) }
        | PendingSourceRemove id ->
            match m.context.sources.removeSource id with
            | Ok () ->
                { m with
                    removeGate = NoPendingLibraryRemove
                    lastError = None
                    selectedEntryId = (if m.selectedEntryId = Some id then None else m.selectedEntryId)
                    selectedNodeOpt = (if m.selectedEntryId = Some id then None else m.selectedNodeOpt) }
            | Error err -> { m with removeGate = NoPendingLibraryRemove; lastError = Some (SourceRemoveRefusedErr err) }
        | NoPendingLibraryRemove -> m
    | CancelEntryRemove ->
        // Cancel only ever disarms — it never dismisses the inline message (the next query /
        // selection change clears it).
        { m with removeGate = NoPendingLibraryRemove }
    | BackUpLibraryNow ->
        match m.context.libraryBackups.backUpLibrary () with
        | Ok name ->
            {
                m with
                    backupStatus = Some (ControlCaption $"Library backed up to {name.value}.")
                    lastError = None
            }
        | Error error ->
            {
                m with
                    lastError = Some (LibraryBackupRefusedErr error)
                    backupStatus = None
            }
    | RequestLibraryRestore ->
        match m.context.libraryBackups.listBackups () with
        | Ok backups ->
            {
                m with
                    restoreGate = PendingLibraryRestore (backups, None)
                    removeGate = NoPendingLibraryRemove
                    lifecycleGate = NoPendingSampleLifecycle
                    lastError = None
                    backupStatus = None
            }
        | Error error ->
            {
                m with lastError = Some (LibraryBackupRefusedErr error)
            }
    | PickLibraryBackup name ->
        match m.restoreGate with
        | PendingLibraryRestore (backups, _) when List.contains name backups ->
            {
                m with restoreGate = PendingLibraryRestore (backups, Some name)
            }
        | PendingLibraryRestore _ | NoPendingLibraryRestore -> m
    | ConfirmLibraryRestore ->
        match m.restoreGate with
        | PendingLibraryRestore (_, Some picked) ->
            match m.context.libraryBackups.restoreLibrary picked with
            | Ok before ->
                {
                    m with
                        restoreGate = NoPendingLibraryRestore
                        selectedEntryId = None
                        selectedNodeOpt = None
                        viewedVersion = None
                        lastError = None
                        backupStatus = Some (ControlCaption $"Library restored from {picked.value}. The library before the restore is backed up as {before.value}.")
                }
            | Error error ->
                {
                    m with lastError = Some (LibraryBackupRefusedErr error)
                }
        | PendingLibraryRestore (_, None) | NoPendingLibraryRestore -> m
    | CancelLibraryRestore ->
        {
            m with restoreGate = NoPendingLibraryRestore
        }
    | ToggleShowInactiveEntries ->
        let flipped =
            match m.showInactive with
            | ActiveOnly -> IncludeInactive
            | IncludeInactive -> ActiveOnly
        { m.disarmed with showInactive = flipped }
    | RequestMarkSampleInactive -> requestLifecycle MaterialsWindowView.MarkInactiveAction m
    | RequestMarkSampleActive -> requestLifecycle MaterialsWindowView.MarkActiveAction m
    | RequestSupersedeSample -> requestLifecycle MaterialsWindowView.SupersedeAction m
    | ConfirmSampleLifecycle ->
        match m.lifecycleGate with
        | PendingSampleLifecycle (id, action) ->
            let outcome =
                match action with
                | MaterialsWindowView.MarkInactiveAction -> m.context.samples.markSampleInactive id
                | MaterialsWindowView.MarkActiveAction -> m.context.samples.markSampleActive id
                | MaterialsWindowView.SupersedeAction -> m.context.samples.supersedeSample id
            match outcome with
            | Ok () -> { m with lifecycleGate = NoPendingSampleLifecycle; lastError = None }
            | Error err ->
                // The store refused (e.g. the sample vanished behind the window's back) — surface
                // the typed reason inline and leave the store and selection untouched.
                { m with lifecycleGate = NoPendingSampleLifecycle; lastError = Some (SampleLifecycleRefusedErr err) }
        | NoPendingSampleLifecycle -> m
    | CancelSampleLifecycle ->
        // Cancel only ever disarms — the next query / selection change clears any inline message.
        { m with lifecycleGate = NoPendingSampleLifecycle }
    | ViewSampleVersion version ->
        { m with viewedVersion = Some version }
    | ConfirmLibrarySelect ->
        // Spec 0038 step 016: 'Select' returns the HIGHLIGHTED entry through the session's
        // onSelected — a TARGETED dispatch (the requesting surface routes it by the context's
        // target and treats a vanished target as a no-op plus a status line) — then closes.
        // The mode flips to Browse in the SAME update, so the host's Closed hook
        // (LibrarySelectDismissed, queued behind this message) finds no pending session: onSelected
        // and onCancelled can never both fire. No highlight → inert (the pair stays, §0.7).
        match m.mode with
        | Select context ->
            match m.selectedEntry with
            | Some entry ->
                context.onSelected entry
                m.context.requestClose ()
                { m with mode = Browse }
            | None -> m
        | Browse -> m
    | CancelLibrarySelect ->
        // The negative half of the pair: end the session without a choice, then close.
        match m.mode with
        | Select context ->
            context.onCancelled ()
            m.context.requestClose ()
            { m with mode = Browse }
        | Browse -> m
    | RetargetLibrarySelect context ->
        // A Select-state open met this LIVE window (the launcher's re-target seam): the
        // superseded session is CANCELLED — a second Choose closes the first, logically —
        // and the window re-points at the new constraint/target with a fresh highlight
        // (the browsing state — filter, chips, representation — is the user's and stays).
        (match m.mode with
         | Select superseded -> superseded.onCancelled ()
         | Browse -> ())
        let retargeted = { m.disarmed with mode = Select context; selectedEntryId = None; selectedNodeOpt = None }
        retargeted.showCurrentEntry
    | LibrarySelectDismissed ->
        // The host window closed (title-bar X, or a staleness Close() from the requesting
        // surface): a STILL-PENDING session cancels exactly once — Select/Close already
        // flipped a resolved session to Browse, so this arm is a no-op after them.
        match m.mode with
        | Select context ->
            context.onCancelled ()
            { m with mode = Browse }
        | Browse -> m

// ---------------------------------------------------------------------------
// The FacetedTreeControls projection.
// ---------------------------------------------------------------------------

/// The distinct discrete values a facet offers over the filtered population, in the engine's
/// key order.
let private offeredValuesFor (def : AttributeDef<LibraryEntry>) (filtered : LibraryEntry list) : DiscreteKey list =
    filtered
    |> List.collect (fun entry ->
        match def.appliesTo entry with
        | ApplicableAttribute -> def.extract entry |> List.distinct
        | InapplicableAttribute -> [])
    |> List.choose (fun value ->
        match value with
        | DiscreteValue k -> Some k
        | NumericValue _ -> None)
    |> List.distinct
    |> List.sortBy (fun k -> k.value)

/// A chip's display label: the facet's name plus its selection — the discrete key(s), or the
/// numeric range through the step-010 unit switch.
let private chipLabel (defs : AttributeDef<LibraryEntry> list) (applied : AppliedConstraint) : ControlCaption =
    let facetName =
        match defs |> List.tryFind (fun d -> d.key = applied.key) with
        | Some def -> def.name.value
        | None -> applied.key.value
    let valueText =
        match applied.selection with
        | DiscreteSelection keys -> keys |> Set.toList |> List.map (fun k -> k.value) |> String.concat ", "
        | NumericRangeSelection range -> range.caption.value
    ControlCaption $"%s{facetName}: %s{valueText}"

/// Sort projected tree nodes alphabetically (case-insensitive, ordinal) by their display
/// label (operator 010/Q1): the top-level facet groups and the entry leaves read
/// alphabetically, OVERRIDING the representation / corpus order the engine hands back.
let private sortNodesByLabel (nodes : FacetedTreeControls.TreeNode list) : FacetedTreeControls.TreeNode list =
    nodes
    |> List.sortWith (fun (a : FacetedTreeControls.TreeNode) (b : FacetedTreeControls.TreeNode) ->
        String.Compare(a.label.value, b.label.value, StringComparison.OrdinalIgnoreCase))

/// Project the live corpus through the engine into the domain-free control state. Everything
/// here is recomputed per render over the CURRENT proxies (offers, counts, buckets, tree,
/// previews), so a verb's write — this window's or another's — shows in the same pass.
///
/// Node codes, unique across the tree: `entries` / `entry:<entryId>` name the corpus group and its
/// leaves; `facet:<facet-key>` / `branch:<facet-key>:<value-key>` the representation's grouping,
/// whose member leaves are `branch:<facet-key>:<value-key>:entry:<entryId>` (so one entry can leaf
/// the corpus group and every branch it belongs to). Only an entry-leaf code selects.
let libraryFacetedState (m : LibraryWindowModel) : FacetedTreeControls.FacetedTreeControlsState =
    let inputs = projectionInputs m
    let filtered = Facets.filter inputs.defs inputs.appliedAll inputs.corpus
    let resultCount = List.length filtered
    // The active sample-id set, for the retired-sample leaf badge (spec 0038 step 023): with the
    // toggle off the sample corpus is `ActiveOnly`, so no leaf ever badges; with it on, a retired
    // sample's latest version reads `label — inactive` (presets never badge).
    let activeIds = m.activeSampleIds
    // One selectable entry leaf under the given (tree-unique) code — the shared shape both the
    // corpus "entries" group and every facet branch project (an entry leafs its branch AND the
    // corpus group, under distinct codes so the tree stays uniquely keyed). The tree is collapsed by
    // default (spec 0040 step 002): only a code in the persisted expanded-node set renders expanded.
    let entryLeafNode (leafCode : ConstructorUiKey) (entry : LibraryEntry) : FacetedTreeControls.TreeNode =
        {
            code = leafCode
            label =
                match libraryEntryLifecycle activeIds entry with
                | ActiveEntry -> entry.entryName.caption
                | InactiveEntry -> entry.entryName.inactiveCaption
            countOpt = None
            expansion = m.expandedNodes.nodeExpansion leafCode
            children = []
        }
    // The host decides gating (result count above the Domain threshold, no explicit build yet);
    // the control only obeys (step 012). A gated pass projects NO tree at all — the whole point
    // is skipping the one potentially heavy render (§0.7).
    let materialization =
        if resultCount > m.context.treeAutoBuildThreshold.value then
            match m.buildRequest with
            | MaterialsWindowView.RequestedTreeBuild -> FacetedTreeControls.TreeMaterialized
            | MaterialsWindowView.NoTreeBuildRequest -> FacetedTreeControls.TreeGated
        else FacetedTreeControls.TreeMaterialized
    // Breadcrumbs: the facet chips in application order, after-counts cumulative over the
    // text-searched population (the filter box narrows first; it takes no chip of its own —
    // its committed text stays visible in the box).
    let searched = Facets.filter inputs.defs inputs.textApplied inputs.corpus
    let breadcrumbs =
        Facets.breadcrumbCounts inputs.defs m.appliedFacets searched
        |> List.map (fun bc ->
            ({
                code = bc.applied.key.uiKey
                label = chipLabel inputs.defs bc.applied
                afterCount = bc.afterCount.facetCount
             } : FacetedTreeControls.BreadcrumbChip))
    // The buckets of one numeric facet over the CURRENTLY CONSTRAINED population (step 010 —
    // recomputed here per pass; a bucket applied as a chip reproduces exactly its count).
    let bucketsOf (def : AttributeDef<LibraryEntry>) : NumericBucket list =
        FacetBuckets.bucketsFor m.context.thicknessBucketCap def inputs.defs inputs.appliedAll inputs.corpus
    // Offers: every UNCONSTRAINED facet, in representation order, count-previewed over
    // everything already applied; a facet inapplicable to (or valueless over) the whole
    // filtered population vanishes entirely (the engine discipline). A NUMERIC facet offers its
    // step-010 buckets plus the manual min–max box (spec 0038 §D.0).
    let appliedKeys = m.appliedFacets |> List.map (fun c -> c.key) |> Set.ofList
    let offers =
        m.representation.order
        |> List.filter (fun key -> not (Set.contains key appliedKeys))
        |> List.choose (fun key -> inputs.defs |> List.tryFind (fun d -> d.key = key))
        |> List.choose (fun def ->
            match def.kind with
            | NumericAttribute ->
                match bucketsOf def with
                | [] -> None
                | buckets ->
                    Some
                        ({
                            code = def.key.uiKey
                            title = def.name.caption
                            values =
                                buckets
                                |> List.map (fun bucket ->
                                    ({
                                        code = bucket.range.uiKey
                                        label = bucket.range.caption
                                        // The bucket derives from the constrained population,
                                        // so its count IS the apply preview (pinned, step 010).
                                        previewCount = bucket.count.facetCount
                                     } : FacetedTreeControls.OfferedValue))
                            manualRange = FacetedTreeControls.ManualRangeOffered
                         } : FacetedTreeControls.OfferGroup)
            | DiscreteAttribute ->
                match offeredValuesFor def filtered with
                | [] -> None
                | values ->
                    Some
                        ({
                            code = def.key.uiKey
                            title = def.name.caption
                            values =
                                values
                                |> List.map (fun k ->
                                    let candidate : AppliedConstraint =
                                        { key = def.key; selection = DiscreteSelection (Set.singleton k) }
                                    ({
                                        code = k.uiKey
                                        label = k.caption
                                        previewCount = (Facets.countFor inputs.defs inputs.appliedAll candidate inputs.corpus).facetCount
                                     } : FacetedTreeControls.OfferedValue))
                            manualRange = FacetedTreeControls.NoManualRange
                         } : FacetedTreeControls.OfferGroup))
    // The tree: the filtered corpus as selectable entry leaves FIRST (the browsed objects —
    // and the rows a headless click must reach without scrolling), then the representation's
    // facet/branch grouping from the engine's buildTree — with the NUMERIC facet's branches
    // re-projected as step-010 buckets (§D.0: a numeric branch is a bucket, never a raw
    // magnitude).
    let tree =
        match materialization with
        | FacetedTreeControls.TreeGated -> []
        | FacetedTreeControls.TreeMaterialized ->
            let entriesNode : FacetedTreeControls.TreeNode =
                {
                    code = libraryEntriesCode
                    label = ControlCaption "Library"
                    countOpt = Some (FacetedTreeControls.FacetItemCount resultCount)
                    expansion = m.expandedNodes.nodeExpansion libraryEntriesCode
                    children =
                        filtered
                        |> List.map (fun entry -> entryLeafNode entry.entryId.libraryEntryCode entry)
                        |> sortNodesByLabel
                }
            let engineTree = Facets.buildTree (Representation m.representation.order) inputs.defs inputs.appliedAll inputs.corpus
            let facetNodes =
                engineTree.facets
                |> List.map (fun facet ->
                    // Each branch as (code, label, count, member entries): a numeric facet projects
                    // step-010 buckets, a discrete facet the engine's value branches. Members come
                    // from the SAME engine path as the count (a bucket / value applied as a chip),
                    // so a branch's leaves and its `(count)` badge always agree.
                    let branchesWithMembers : (ConstructorUiKey * ControlCaption * FacetedTreeControls.FacetItemCount * LibraryEntry list) list =
                        match inputs.defs |> List.tryFind (fun d -> d.key = facet.key) with
                        | Some def when def.kind = NumericAttribute ->
                            bucketsOf def
                            |> List.map (fun bucket ->
                                let branchCode = ConstructorUiKey $"branch:{facet.key.value}:{bucket.range.uiKey.value}"
                                let members = Facets.filter inputs.defs (inputs.appliedAll @ [ { key = facet.key; selection = NumericRangeSelection bucket.range } ]) inputs.corpus
                                // The badge is the DISTINCT member entries (what expanding reveals),
                                // not `bucket.count` — a numeric bucket counts value-occurrences, so an
                                // entry with two in-range layer thicknesses would over-count the leaves.
                                branchCode, bucket.range.caption, FacetedTreeControls.FacetItemCount (List.length members), members)
                        | Some _ | None ->
                            facet.branches
                            |> List.map (fun branch ->
                                let branchCode = ConstructorUiKey $"branch:{facet.key.value}:{branch.value.label}"
                                let members =
                                    match branch.value with
                                    | DiscreteValue dk -> Facets.filter inputs.defs (inputs.appliedAll @ [ { key = facet.key; selection = DiscreteSelection (Set.singleton dk) } ]) inputs.corpus
                                    | NumericValue _ -> []
                                branchCode, branch.label.caption, branch.count.facetCount, members)
                    let branchNodes =
                        branchesWithMembers
                        |> List.map (fun (branchCode, label, count, members) ->
                            ({
                                code = branchCode
                                label = label
                                countOpt = Some count
                                expansion = m.expandedNodes.nodeExpansion branchCode
                                // The branch expands to its member entries as selectable leaves
                                // (path-shaped codes keep them tree-unique), alphabetical by label.
                                children =
                                    members
                                    |> List.map (fun entry -> entryLeafNode (branchCode.libraryBranchLeaf entry.entryId) entry)
                                    |> sortNodesByLabel
                             } : FacetedTreeControls.TreeNode))
                    // The facet group shows its total: the DISTINCT filtered entries it classifies (an
                    // entry a multi-valued facet — film material or thickness — lists under several
                    // branches counts once).
                    let groupCount =
                        branchesWithMembers
                        |> List.collect (fun (_, _, _, members) -> members |> List.map (fun entry -> entry.entryId))
                        |> List.distinct
                        |> List.length
                    let facetCode = ConstructorUiKey $"facet:{facet.key.value}"
                    ({
                        code = facetCode
                        label = facet.name.caption
                        countOpt = Some (FacetedTreeControls.FacetItemCount groupCount)
                        expansion = m.expandedNodes.nodeExpansion facetCode
                        children = branchNodes
                     } : FacetedTreeControls.TreeNode))
                |> sortNodesByLabel
            entriesNode :: facetNodes
    {
        tree = tree
        breadcrumbs = breadcrumbs
        offers = offers
        representations =
            offeredLibraryRepresentations
            |> List.map (fun r -> ({ code = r.code; label = r.label } : FacetOption))
        activeRepresentationOpt = Some m.representation.code
        filterDraft = m.textFilter.caption
        resultCount = FacetedTreeControls.FacetItemCount resultCount
        materialization = materialization
        // The EXACT selected node code (spec 0040 step 003) — the control highlights the row whose
        // code matches, so an entry picked through a facet branch highlights that branch row, not
        // the corpus-group copy. The Model tracks the clicked code.
        selectedCodeOpt = m.selectedNodeOpt
    }

/// The control's behaviour seam: every token is lifted back to its domain value HERE, at the
/// control boundary. Only an `entry:` node code selects (branches/headings are grouping
/// display; the offers are the apply surface); the film-thickness group's tokens parse as
/// numeric ranges (a bucket's `NumericRange.uiKey`, or the manual box's raw "lo-hi" text) — an
/// unparseable token is the documented no-op.
let libraryFacetedHandlers (dispatch : LibraryWindowMsg -> unit) : FacetedTreeControls.FacetedTreeControlsHandlers =
    {
        applyConstraint =
            fun group value ->
                match AttributeKey.tryCreate group.value with
                | Some key when key = sampleFilmThicknessKey ->
                    match value.tryNumericRange with
                    | Some range -> dispatch (ApplyFacetConstraint (key, NumericRangeSelection range))
                    | None -> ()
                | Some key ->
                    match DiscreteKey.tryCreate value.value with
                    | Some discrete -> dispatch (ApplyFacetConstraint (key, DiscreteSelection (Set.singleton discrete)))
                    | None -> ()
                | None -> ()
        removeConstraint = fun chip -> AttributeKey.tryCreate chip.value |> Option.iter (RemoveLibraryFacet >> dispatch)
        commitTextFilter = fun text -> dispatch (CommitLibraryTextFilter text.toTextQuery)
        chooseRepresentation = fun code -> dispatch (ChooseLibraryRepresentation code)
        requestBuild = fun () -> dispatch RequestLibraryTreeBuild
        selectNode =
            fun code ->
                match code.tryLibraryEntryId with
                | Some id -> dispatch (SelectLibraryEntryNode (id, code))
                | None -> ()
        toggleNode = fun code -> dispatch (ToggleLibraryNode code)
        applyManualRange =
            fun group raw ->
                match AttributeKey.tryCreate group.value, raw.tryManualNumericRange with
                | Some key, Some range -> dispatch (ApplyFacetConstraint (key, NumericRangeSelection range))
                | _ -> ()
    }

// ---------------------------------------------------------------------------
// The view: the faceted tree beside the verbs + view panel.
// ---------------------------------------------------------------------------

let private brush (c : Color) : IBrush = SolidColorBrush(c) :> IBrush
let private idleBackground = Color.FromRgb(232uy, 232uy, 232uy)
let private idleBorder = Color.FromRgb(120uy, 120uy, 120uy)
let private messageColor = Color.FromRgb(178uy, 34uy, 34uy)
// The positive/negative action pair backgrounds (the Save/Cancel precedent, SampleEditorView).
let private positiveBackground = Color.FromRgb(186uy, 224uy, 186uy)
let private negativeBackground = Color.FromRgb(236uy, 202uy, 202uy)

/// A clickable verb box (the shared workbench button look), KEYED by its id so membership
/// changes recreate a shifted box; `e.Handled <- true` drops FuncUI's duplicate Tunnel|Bubble
/// pass. The verbs, confirm row, message and panel all have variable membership, so every id is
/// an AutomationId, never `StyledElement.Name` (the `FacetedTreeControls` discipline).
let private libraryVerbButtonWithBackground (id : ConstructorUiId) (label : ControlCaption) (background : Color) (onPressed : PointerPressedEventArgs -> unit) : IView =
    let keyedBox =
        Border.create [
            id.automationAttribute<Border> ()
            AttrBuilder<Border>.CreateProperty<string>(AutomationProperties.NameProperty, label.value, ValueNone)
            Border.background (brush background)
            Border.borderBrush (brush idleBorder)
            Border.borderThickness 1.0
            Border.cornerRadius (CornerRadius 3.0)
            Border.padding (Thickness(12.0, 5.0))
            Border.margin (Thickness(0.0, 0.0, 8.0, 4.0))
            Border.verticalAlignment VerticalAlignment.Center
            Border.child (TextBlock.create [ TextBlock.text label.value ])
            Border.onPointerPressed ((fun e -> e.Handled <- true; onPressed e), SubPatchOptions.OnChangeOf (box id.value))
        ]
        |> Avalonia.FuncUI.DSL.View.withKey id.value
    keyedBox :> IView

let private verbButtonOn (id : ConstructorUiId) (label : ControlCaption) (onPressed : PointerPressedEventArgs -> unit) : IView =
    libraryVerbButtonWithBackground id label idleBackground onPressed

let private verbButton (id : ConstructorUiId) (label : ControlCaption) (onClick : unit -> unit) : IView =
    verbButtonOn id label (fun _ -> onClick ())

let private pickCatalogueFolder (owner : Window) (onPicked : FolderName -> unit) : unit =
    let pick =
        async {
            try
                let options = Avalonia.Platform.Storage.FolderPickerOpenOptions(Title = "Register catalogue folder", AllowMultiple = false)
                let! folders = owner.StorageProvider.OpenFolderPickerAsync options |> Async.AwaitTask
                match List.ofSeq folders with
                | folder :: _ ->
                    let picked = FolderName folder.Path.LocalPath
                    Avalonia.Threading.Dispatcher.UIThread.Post(fun () -> onPicked picked)
                | [] -> ()
            with _ -> ()
        }
    Avalonia.Threading.Dispatcher.UIThread.Post(fun () -> Async.StartImmediate pick)

/// A positive/negative ACTION box (the Save/Cancel styling precedent) — same look as
/// `verbButton` but colour-coded and wider-padded; keyed, AutomationId'd (variable
/// membership: the pair exists only in Select state).
let private actionButton (id : ConstructorUiId) (label : ControlCaption) (background : Color) (onClick : unit -> unit) : IView =
    let keyedBox =
        Border.create [
            id.automationAttribute<Border> ()
            AttrBuilder<Border>.CreateProperty<string>(AutomationProperties.NameProperty, label.value, ValueNone)
            Border.background (brush background)
            Border.borderBrush (brush idleBorder)
            Border.borderThickness 1.0
            Border.cornerRadius (CornerRadius 3.0)
            Border.padding (Thickness(22.0, 6.0))
            Border.margin (Thickness(0.0, 0.0, 10.0, 4.0))
            Border.verticalAlignment VerticalAlignment.Center
            Border.child (TextBlock.create [ TextBlock.text label.value ])
            Border.onPointerPressed ((fun e -> e.Handled <- true; onClick ()), SubPatchOptions.OnChangeOf (box id.value))
        ]
        |> Avalonia.FuncUI.DSL.View.withKey id.value
    keyedBox :> IView

type SelectionTarget with

    /// The requesting surface a Select session serves, as the Library window's banner prose.
    member private this.libraryTargetCaption : ControlCaption =
        match this with
        | TableElementTarget _ -> ControlCaption "the requesting table element"
        | SampleLayerTarget _ -> ControlCaption "the requesting sample layer"
        | SampleSubstrateTarget -> ControlCaption "the requesting sample substrate plate"

/// The Select-state surface (spec 0038 step 016): exactly TWO buttons in ONE row with distinct
/// positive/negative styling and a visible gap (§0.7) — 'Select' returns the highlighted entry,
/// 'Close' cancels — above the pre-applied kind constraint shown as a FIXED banner: it is not a
/// breadcrumb chip and offers no remove affordance (the constraint lives at the corpus seam).
/// Absent in Browse mode — everything else IS the ordinary window.
let private selectModeRows (m : LibraryWindowModel) (dispatch : LibraryWindowMsg -> unit) : IView list =
    match m.mode with
    | Browse -> []
    | Select context ->
        [ WrapPanel.create [
              WrapPanel.orientation Orientation.Horizontal
              WrapPanel.children [
                  actionButton ConstructorUiId.librarySelectButton (ControlCaption "Select") positiveBackground (fun () -> dispatch ConfirmLibrarySelect)
                  actionButton ConstructorUiId.librarySelectCloseButton (ControlCaption "Close") negativeBackground (fun () -> dispatch CancelLibrarySelect)
              ]
          ] :> IView
          (TextBlock.create [
              ConstructorUiId.librarySelectConstraint.automationAttribute<TextBlock> ()
              TextBlock.fontWeight FontWeight.SemiBold
              TextBlock.textWrapping TextWrapping.Wrap
              TextBlock.maxWidth 380.0
              TextBlock.text $"Choose a %s{context.kindConstraint.value.kindName} for %s{context.target.libraryTargetCaption.value} — the kind constraint is fixed and cannot be removed."
          ]
          |> Avalonia.FuncUI.DSL.View.withKey ConstructorUiId.librarySelectConstraint.value) :> IView ]

/// One lifecycle verb box (spec 0038 step 023), by its action.
let private lifecycleButton (dispatch : LibraryWindowMsg -> unit) (action : MaterialsWindowView.LifecycleAction) : IView =
    match action with
    | MaterialsWindowView.MarkInactiveAction -> verbButton ConstructorUiId.libraryMarkInactiveButton (ControlCaption "Mark inactive") (fun () -> dispatch RequestMarkSampleInactive)
    | MaterialsWindowView.MarkActiveAction -> verbButton ConstructorUiId.libraryMarkActiveButton (ControlCaption "Mark active") (fun () -> dispatch RequestMarkSampleActive)
    | MaterialsWindowView.SupersedeAction -> verbButton ConstructorUiId.librarySupersedeButton (ControlCaption "Supersede…") (fun () -> dispatch RequestSupersedeSample)

/// The verbs row: Add sample, Make multilayer and (spec 0046) Add source always; Edit only for a
/// SAMPLE or USER-SOURCE selection (a preset has no editor — the verb is removed, not greyed); Remove
/// while an entry is selected
/// (a protected entry's Remove REFUSES with the typed reason instead of arming the gate); the
/// lifecycle verbs from `offeredLifecycleActions` (only for a UserManaged sample — a protected
/// preset exposes NONE, spec 0038 step 023).
let private verbsRow (m : LibraryWindowModel) (dispatch : LibraryWindowMsg -> unit) : IView =
    WrapPanel.create [
        WrapPanel.orientation Orientation.Horizontal
        WrapPanel.children (
            [
                verbButton ConstructorUiId.libraryAddSampleButton (ControlCaption "Add sample") (fun () -> dispatch AddSample)
                verbButton ConstructorUiId.libraryMakeMultilayerButton (ControlCaption "Make multilayer") (fun () -> dispatch MakeMultilayer)
                verbButton ConstructorUiId.libraryAddSourceButton (ControlCaption "Add source") (fun () -> dispatch AddSource)
            ]
            @ (match m.editableSample, m.editableSource with
               | Some _, _ | None, Some _ -> [ verbButton ConstructorUiId.libraryEditButton (ControlCaption "Edit") (fun () -> dispatch EditSelectedEntry) ]
               | None, None -> [])
            @ (match m.selectedEntry with
               | Some _ -> [ verbButton ConstructorUiId.libraryRemoveButton (ControlCaption "Remove") (fun () -> dispatch RequestRemoveSelectedEntry) ]
               | None -> [])
            @ (m.offeredLifecycleActions |> List.map (lifecycleButton dispatch))
            @ [
                verbButtonOn ConstructorUiId.libraryRegisterCatalogueFolderButton (ControlCaption "Register catalogue folder…") (fun e ->
                    match e.Source with
                    | :? Visual as source ->
                        match TopLevel.GetTopLevel source with
                        | :? Window as owner -> pickCatalogueFolder owner (RegisterCatalogueFolder >> dispatch)
                        | _ -> ()
                    | _ -> ())
                verbButton ConstructorUiId.libraryBackUpNowButton (ControlCaption "Back up library now…") (fun () -> dispatch BackUpLibraryNow)
                verbButton ConstructorUiId.libraryRestoreButton (ControlCaption "Restore library…") (fun () -> dispatch RequestLibraryRestore)
            ])
    ] :> IView

let private catalogueFolderRows (m : LibraryWindowModel) (dispatch : LibraryWindowMsg -> unit) : IView list =
    match m.context.catalogueImports.proxy.registeredFolders () with
    | Error _ | Ok [] -> []
    | Ok entries ->
        [ StackPanel.create [
              ConstructorUiId.libraryCatalogueFoldersPanel.automationAttribute<StackPanel> ()
              AttrBuilder<StackPanel>.CreateProperty<string>(AutomationProperties.NameProperty, "Catalogue folders", ValueNone)
              StackPanel.spacing 6.0
              StackPanel.children (
                  [ TextBlock.create [ TextBlock.text "Catalogue folders"; TextBlock.fontWeight FontWeight.SemiBold ] :> IView ]
                  @ (entries |> List.map (fun entry ->
                      let id = ConstructorUiId.libraryCatalogueFolderRow entry.catalogueId.uiKey
                      let importedAt = entry.importedAt.value.ToString("yyyy-MM-dd HH:mm", Globalization.CultureInfo.InvariantCulture)
                      StackPanel.create [
                          id.automationAttribute<StackPanel> ()
                          AttrBuilder<StackPanel>.CreateProperty<string>(AutomationProperties.NameProperty, entry.folder.value, ValueNone)
                          StackPanel.spacing 4.0
                          StackPanel.children [
                              TextBlock.create [ TextBlock.text entry.folder.value; TextBlock.textWrapping TextWrapping.Wrap ]
                              TextBlock.create [ TextBlock.text $"imported {importedAt} UTC" ]
                              TextBlock.create [ TextBlock.text $"{entry.entryCount.value} entries" ]
                              verbButton (ConstructorUiId.libraryUnregisterCatalogueButton entry.catalogueId.uiKey) (ControlCaption "Unregister") (fun () -> dispatch (UnregisterCatalogueFolder entry.catalogueId))
                          ]
                      ]
                      |> Avalonia.FuncUI.DSL.View.withKey id.value
                      :> IView)))
          ] :> IView ]

let private libraryRestoreRows (m : LibraryWindowModel) (dispatch : LibraryWindowMsg -> unit) : IView list =
    match m.restoreGate with
    | NoPendingLibraryRestore -> []
    | PendingLibraryRestore (backups, pickedOpt) ->
        let prompt = ControlCaption "Restore the whole library from a backup. The current library is backed up first."
        [ StackPanel.create [
              ConstructorUiId.libraryRestorePanel.automationAttribute<StackPanel> ()
              AttrBuilder<StackPanel>.CreateProperty<string>(AutomationProperties.NameProperty, prompt.value, ValueNone)
              StackPanel.spacing 6.0
              StackPanel.children (
                  [ TextBlock.create [
                        TextBlock.text prompt.value
                        TextBlock.textWrapping TextWrapping.Wrap
                    ] :> IView ]
                  @ (match backups with
                     | [] -> [ TextBlock.create [ TextBlock.text "No backup yet." ] :> IView ]
                     | names ->
                         names |> List.map (fun name ->
                             let background = if pickedOpt = Some name then positiveBackground else idleBackground
                             libraryVerbButtonWithBackground (ConstructorUiId.libraryBackupRow name.uiKey) name.caption background (fun _ -> dispatch (PickLibraryBackup name))))
                  @ [ WrapPanel.create [
                          WrapPanel.orientation Orientation.Horizontal
                          WrapPanel.children [
                              actionButton ConstructorUiId.libraryRestoreConfirmButton (ControlCaption "Restore") positiveBackground (fun () -> dispatch ConfirmLibraryRestore)
                              actionButton ConstructorUiId.libraryRestoreCancelButton (ControlCaption "Cancel") negativeBackground (fun () -> dispatch CancelLibraryRestore)
                          ]
                      ] :> IView ])
          ] :> IView ]

type LibraryWindowModel with

    member internal this.libraryImportStatusRow : IView list =
        match this.importStatus with
        | None -> []
        | Some status ->
            [ TextBlock.create [
                  ConstructorUiId.libraryImportStatusText.automationAttribute<TextBlock> ()
                  AttrBuilder<TextBlock>.CreateProperty<string>(AutomationProperties.NameProperty, status.value, ValueNone)
                  TextBlock.text status.value
                  TextBlock.textWrapping TextWrapping.Wrap
              ] :> IView ]

    member internal this.libraryBackupStatusRow : IView list =
        match this.backupStatus with
        | None -> []
        | Some status ->
            [ TextBlock.create [
                  ConstructorUiId.libraryBackupStatusText.automationAttribute<TextBlock> ()
                  AttrBuilder<TextBlock>.CreateProperty<string>(AutomationProperties.NameProperty, status.value, ValueNone)
                  TextBlock.text status.value
                  TextBlock.textWrapping TextWrapping.Wrap
              ] :> IView ]

/// The show-inactive/superseded toggle row (spec 0038 step 023): a keyed verb box carrying the
/// visible count badge — `Show inactive (N)` while hidden, `Hide inactive (N)` while shown.
/// Browse-mode only (Select always lists `ActiveOnly`).
let private toggleRow (m : LibraryWindowModel) (dispatch : LibraryWindowMsg -> unit) : IView list =
    match m.mode with
    | Select _ -> []
    | Browse ->
        let n = m.inactiveCount
        let label =
            match m.showInactive with
            | ActiveOnly -> ControlCaption $"Show inactive (%d{n.value})"
            | IncludeInactive -> ControlCaption $"Hide inactive (%d{n.value})"
        [ verbButton ConstructorUiId.libraryShowInactiveToggle label (fun () -> dispatch ToggleShowInactiveEntries) ]

/// The inline remove confirmation — present only while a sample or source remove is armed.
let private confirmRow (m : LibraryWindowModel) (dispatch : LibraryWindowMsg -> unit) : IView list =
    let prompt =
        match m.removeGate with
        | NoPendingLibraryRemove -> None
        | PendingSampleRemove id ->
            match m.context.samples.tryGetSample id with
            | Ok (Some sample) -> Some $"Remove sample '%s{sample.name.value}'?"
            | Ok None | Error _ -> Some $"Remove sample '%s{string id.value}'?"
        | PendingSourceRemove id ->
            match m.context.sources.tryGetSource id with
            | Ok (Some source) -> Some $"Remove source '%s{source.name.value}'?"
            | Ok None | Error _ -> Some $"Remove source '%s{id.value}'?"
    match prompt with
    | None -> []
    | Some text ->
        [ WrapPanel.create [
              WrapPanel.orientation Orientation.Horizontal
              WrapPanel.children [
                  TextBlock.create [
                      TextBlock.text text
                      TextBlock.verticalAlignment VerticalAlignment.Center
                      TextBlock.margin (Thickness(0.0, 0.0, 8.0, 4.0))
                  ]
                  verbButton ConstructorUiId.libraryRemoveConfirmButton (ControlCaption "Remove") (fun () -> dispatch ConfirmEntryRemove)
                  verbButton ConstructorUiId.libraryRemoveCancelButton (ControlCaption "Cancel") (fun () -> dispatch CancelEntryRemove)
              ]
          ] :> IView ]

/// The inline lifecycle confirmation (spec 0038 step 023) — present only while a lifecycle verb is
/// armed. The prompt NAMES the sample and the transition (the verb the user pressed); Confirm runs
/// the matching proxy verb, Cancel disarms.
let private lifecycleConfirmRow (m : LibraryWindowModel) (dispatch : LibraryWindowMsg -> unit) : IView list =
    match m.lifecycleGate with
    | NoPendingSampleLifecycle -> []
    | PendingSampleLifecycle (id, action) ->
        let name =
            match m.context.samples.tryGetSample id with
            | Ok (Some sample) -> sample.name.value
            | Ok None | Error _ -> string id.value
        let prompt =
            match action with
            | MaterialsWindowView.MarkInactiveAction -> $"Mark sample '%s{name}' inactive?"
            | MaterialsWindowView.MarkActiveAction -> $"Mark sample '%s{name}' active?"
            | MaterialsWindowView.SupersedeAction -> $"Supersede sample '%s{name}'?"
        [ WrapPanel.create [
              WrapPanel.orientation Orientation.Horizontal
              WrapPanel.children [
                  TextBlock.create [
                      TextBlock.text prompt
                      TextBlock.verticalAlignment VerticalAlignment.Center
                      TextBlock.margin (Thickness(0.0, 0.0, 8.0, 4.0))
                  ]
                  verbButton ConstructorUiId.libraryLifecycleConfirmButton (ControlCaption "Confirm") (fun () -> dispatch ConfirmSampleLifecycle)
                  verbButton ConstructorUiId.libraryLifecycleCancelButton (ControlCaption "Cancel") (fun () -> dispatch CancelSampleLifecycle)
              ]
          ] :> IView ]

/// The inline typed-refusal message. Every case carries its diagnostic reason — never a
/// cascade, never a dialog.
let private messageRow (m : LibraryWindowModel) : IView list =
    match m.lastError with
    | None -> []
    | Some err ->
        [ TextBlock.create [
              ConstructorUiId.libraryWindowMessage.automationAttribute<TextBlock> ()
              TextBlock.foreground (brush messageColor)
              TextBlock.textWrapping TextWrapping.Wrap
              TextBlock.maxWidth 380.0
              TextBlock.text err.caption.value
          ] :> IView ]

/// The version list (spec 0038 step 023): one clickable row per stored SAMPLE version, the latest
/// marked `(latest)` and the currently-viewed one marked `▸`. Clicking a row shows THAT version in
/// the panel — the latest editable through the Edit verb, an older one view-only. Absent for a
/// preset or a single-version sample (the common case in the live store until step 25).
let private versionsRow (versions : (VersionNumber * Sample) list) (latest : VersionNumber) (shown : VersionNumber) (dispatch : LibraryWindowMsg -> unit) : IView list =
    match versions with
    | [] | [ _ ] -> []
    | _ ->
        [ StackPanel.create [
              ConstructorUiId.libraryVersionsPanel.automationAttribute<StackPanel> ()
              StackPanel.orientation Orientation.Horizontal
              StackPanel.spacing 0.0
              StackPanel.children (
                  TextBlock.create [
                      TextBlock.text "Versions:"
                      TextBlock.verticalAlignment VerticalAlignment.Center
                      TextBlock.margin (Thickness(0.0, 0.0, 8.0, 4.0))
                  ]
                  :: (versions
                      |> List.map (fun (version, _) ->
                          let marker = if version = shown then "▸ " else ""
                          let latestTag = if version = latest then " (latest)" else ""
                          verbButton (ConstructorUiId.libraryVersionRow version) (ControlCaption $"%s{marker}v%d{version.value}%s{latestTag}") (fun () -> dispatch (ViewSampleVersion version)))))
          ] :> IView ]

/// The view panel: the selected entry's kind, protection state and FULL description (the
/// Library confirm-step / Details prose — `fullDescription` spells out what the entry IS), plus a
/// SAMPLE's version list (spec 0038 step 023). By default it shows the LATEST version (the editable
/// one — the library always edits the latest); selecting an OLDER sample version shows it
/// VIEW-ONLY (a `viewOnlyNote`, no Save path — older versions have no editor). Resolved through the
/// live corpus at render time, so a removed sample's panel vanishes with its row. The retired bay's
/// View verb is subsumed by selection.
let private viewPanel (m : LibraryWindowModel) (dispatch : LibraryWindowMsg -> unit) : IView list =
    match m.selectedEntry with
    | None -> []
    | Some entry ->
        let versions =
            match entry with
            | SampleItem sample -> selectedVersionsOf m.context.samples sample.id
            | SourceItem _ | DetectorItem _ | PolarizerItem _ -> []
        let latestVersion =
            match versions |> List.map fst with
            | [] -> VersionNumber.first
            | numbers -> List.max numbers
        let shownVersion =
            match m.viewedVersion with
            | Some v when versions |> List.exists (fun (vn, _) -> vn = v) -> v
            | _ -> latestVersion
        let viewingOlder = (not (List.isEmpty versions)) && shownVersion <> latestVersion
        let shownOlderSample = versions |> List.tryPick (fun (vn, s) -> if vn = shownVersion && viewingOlder then Some s else None)
        let displayName, description, note =
            match shownOlderSample with
            | Some sample -> sample.name.value, sample.description.value, $" (version {shownVersion.value} — view-only)"
            | None ->
                let protectionNote =
                    match entry.protection with
                    | ProtectedBuiltIn -> " — protected built-in"
                    | UserManaged -> ""
                entry.displayName, entry.fullDescription, protectionNote
        let viewOnlyNote : IView list =
            if viewingOlder then
                [ TextBlock.create [
                      ConstructorUiId.libraryViewOnlyNote.automationAttribute<TextBlock> ()
                      TextBlock.foreground (brush idleBorder)
                      TextBlock.textWrapping TextWrapping.Wrap
                      TextBlock.maxWidth 380.0
                      TextBlock.text $"Viewing version %d{shownVersion.value} (view-only) — the library edits the latest, version %d{latestVersion.value}."
                  ] :> IView ]
            else []
        [ Border.create [
              ConstructorUiId.libraryViewPanel.automationAttribute<Border> ()
              Border.child (
                  StackPanel.create [
                      StackPanel.orientation Orientation.Vertical
                      StackPanel.spacing 2.0
                      StackPanel.children (
                          [ TextBlock.create [
                                TextBlock.fontWeight FontWeight.SemiBold
                                TextBlock.textWrapping TextWrapping.Wrap
                                TextBlock.maxWidth 380.0
                                TextBlock.text $"%s{displayName} — %s{entry.kindFacetKey.value}%s{note}"
                            ] :> IView
                            TextBlock.create [
                                TextBlock.textWrapping TextWrapping.Wrap
                                TextBlock.maxWidth 380.0
                                TextBlock.text description
                            ] :> IView ]
                          @ viewOnlyNote
                          @ versionsRow versions latestVersion shownVersion dispatch)
                  ])
          ] :> IView ]

/// The window surface: the faceted tree (filter, representation picker, breadcrumbs, live
/// count, offers, tree) fills the window beside the right-hand panel carrying — in Select
/// state — the Select/Close pair and the fixed-constraint banner FIRST (step 016), then the
/// ordinary verbs, the inline confirm gate, the typed refusal message, and the selected
/// entry's view panel.
let libraryWindowView (m : LibraryWindowModel) (dispatch : LibraryWindowMsg -> unit) : IView =
    DockPanel.create [
        DockPanel.children [
            Border.create [
                Border.dock Dock.Right
                Border.width 420.0
                Border.padding (Thickness 8.0)
                Border.child (
                    ScrollViewer.create [
                        ScrollViewer.content (
                            StackPanel.create [
                                StackPanel.orientation Orientation.Vertical
                                StackPanel.spacing 6.0
                                StackPanel.children (
                                    selectModeRows m dispatch
                                    @ toggleRow m dispatch
                                    @ [ verbsRow m dispatch ]
                                    @ catalogueFolderRows m dispatch
                                    @ m.libraryImportStatusRow
                                    @ confirmRow m dispatch
                                    @ lifecycleConfirmRow m dispatch
                                    @ libraryRestoreRows m dispatch
                                    @ m.libraryBackupStatusRow
                                    @ messageRow m
                                    @ viewPanel m dispatch)
                            ])
                    ])
            ]
            Border.create [
                ConstructorUiId.libraryFacetTreeHost.automationAttribute<Border> ()
                Border.padding (Thickness 8.0)
                Border.child (FacetedTreeControls.facetedTreeControlsView (libraryFacetedState m) (libraryFacetedHandlers dispatch))
            ]
        ]
    ] :> IView
