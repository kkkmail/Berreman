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
open System.Globalization
open Avalonia
open Avalonia.Automation
open Avalonia.Controls
open Avalonia.Layout
open Avalonia.Media
open Avalonia.FuncUI.Builder
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Facets
open OpticalConstructor.Domain.FacetBuckets
open OpticalConstructor.Domain.LibraryFacets
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.Lifecycle
open OpticalConstructor.Domain.Placement
open OpticalConstructor.Domain.WindowMode
open OpticalConstructor.Domain.WorkbenchSettings
open OpticalConstructor.Controls

// ---------------------------------------------------------------------------
// Elevated window-local states (named DUs, never bools).
// ---------------------------------------------------------------------------

/// Which sample remove (if any) awaits its inline confirmation. The pending case CARRIES the
/// `SampleId` the Remove click targeted, so a selection change between Remove and Confirm can
/// never delete a different entry (the step-013 `MaterialRemoveGate` shape). Only a SAMPLE can
/// ever arm this gate — a protected entry's Remove refuses at the verb click and a preset has
/// no removal store (see `RequestRemoveSelected`).
type LibraryRemoveGate =
    | NoPendingRemove
    | PendingRemove of SampleId

/// Which lifecycle transition a confirm gate targets (spec 0038 step 023): a named DU — never a
/// bare string — so the confirm prose and the proxy call both read from ONE value. `Supersede` and
/// `MarkInactive` both retire the latest version (they share `InactiveEntry` in the store — steps
/// 021/022), but stay DISTINCT actions so the verb the user pressed is what the confirm prompt
/// names and a future step can diverge them.
type LifecycleAction =
    | MarkInactiveAction
    | MarkActiveAction
    | SupersedeAction

/// Which lifecycle verb (if any) awaits its inline confirmation, carrying the `SampleId` the verb
/// click targeted (the `LibraryRemoveGate` confirm-gating shape). Only a SAMPLE can arm this — a
/// protected preset exposes no lifecycle verbs at all. Parallel to `removeGate`, never armed at the
/// same time (arming either disarms the other), so the step-015 remove tests stay intact.
type SampleLifecycleGate =
    | NoPendingLifecycle
    | PendingLifecycle of SampleId * LifecycleAction

/// Whether the user has explicitly materialized a tree the result count gated behind the
/// Show/Search button (spec 0038 §0.7). Sticky for the window's lifetime: once shown, later
/// constraint changes keep the tree materialized — the gate protects the FIRST heavy render.
type TreeBuildRequest =
    | RequestedTreeBuild
    | NoTreeBuildRequest

/// The tree-node codes the user has explicitly expanded (spec 0040 step 002): an elevated named
/// wrapper over the code set — never a naked `Set<string>` on the model surface. The tree is
/// COLLAPSED by default (a code absent from the set projects `CollapsedNode`); a disclosure-chevron
/// press toggles exactly one code's membership, and the choice PERSISTS across projections.
type ExpandedNodes =
    | ExpandedNodes of Set<string>

    member this.value = let (ExpandedNodes codes) = this in codes
    /// Whether the node with this code is expanded (its children render).
    member this.isExpanded (code : string) : bool = this.value.Contains code
    /// Flip one code's membership — expand a collapsed node, collapse an expanded one.
    member this.toggle (code : string) : ExpandedNodes =
        let codes = this.value
        ExpandedNodes (if codes.Contains code then Set.remove code codes else Set.add code codes)

    static member empty : ExpandedNodes = ExpandedNodes Set.empty

/// The window's typed refusals, surfaced as the inline message (errors are values — never a
/// dialog, never a throw). Each case carries its diagnostic payload.
type LibraryWindowError =
    /// Remove was clicked on a `ProtectedBuiltIn` entry (spec 0038 F.0: protected entries
    /// cannot be deleted) — the reason NAMES the entry.
    | ProtectedEntryRefused of reason : string
    /// Remove was clicked on a `UserManaged` NON-sample entry — no preset store exists to
    /// remove from. Unreachable over today's corpus (every non-sample is `ProtectedBuiltIn`),
    /// declared so the protection match is total without a throw.
    | PresetNotRemovable of reason : string
    /// The samples store refused the confirmed remove — the typed `SampleError` carries its
    /// own reason.
    | SampleRemoveRefused of SampleError
    /// The samples store refused a confirmed lifecycle verb (spec 0038 step 023 — e.g. the sample
    /// vanished behind the window's back) — the typed `SampleError` carries its own reason.
    | SampleLifecycleRefused of SampleError

/// One named tree-shaping representation the window offers (spec 0038 §D.0: search order ≠
/// representation order): a stable `code` the control dispatches back, the picker label, and
/// the facet order handed to the engine's `buildTree`.
type LibraryRepresentation =
    {
        code : string
        label : string
        order : AttributeKey list
    }

/// The seeded default representation "By kind" (spec 0038 step 015) — the step-011 catalogue
/// order: the kind facet first, the polarizer-category facet (present only over polarizer
/// populations), every material facet lifted to samples, then the sample-structural facets.
/// Applied constraints never reshuffle it.
let byKindRepresentation : LibraryRepresentation =
    {
        code = "by-kind"
        label = "By kind"
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
let offeredRepresentations : LibraryRepresentation list =
    [ byKindRepresentation ]

// ---------------------------------------------------------------------------
// The window's IO seam (the functional-proxy Context convention).
// ---------------------------------------------------------------------------

/// The window's context: the read-only Library seam (the preset entries), the app-scope
/// samples WRITE store (the live sample corpus AND the Remove verb's target), the materials
/// store (the step-011 catalogue resolves sample constituents through it), the Domain
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
        treeAutoBuildThreshold : TreeAutoBuildThreshold
        thicknessBucketCap : ThicknessBucketCap
        /// Open the Sample editor by intent: `NewBlankSample` / `NewSeededMultilayer` (the Add /
        /// Make-multilayer verbs mint the id AT dispatch — spec 0038 step 008) or `EditSample`.
        openSampleEditor : SampleEditorView.SampleEditorIntent -> unit
        /// Close THIS window (the host passes `this.Close`; tests substitute a recording stub —
        /// the `SampleEditorContext` precedent). The Select-state verbs reach it: 'Select'
        /// closes after `onSelected`, 'Close' after `onCancelled` (spec 0038 step 016).
        requestClose : unit -> unit
    }

/// The window's pure model. The engine inputs (corpus, facet defs) are NOT cached here — every
/// projection re-queries the context's proxies, so a verb's write (or another window's) shows
/// in the same render pass.
type Model =
    {
        context : LibraryWindowContext
        /// Browse (the ordinary window) or Select (spec 0038 step 016): the SAME window
        /// pre-constrained to the session context's kind — the corpus seam applies the
        /// constraint, so it is structurally NON-REMOVABLE (no breadcrumb chip) — plus the
        /// Select/Close pair. A `RetargetSelect` re-points a live window's session; a resolved
        /// or cancelled session flips back to Browse so the host's close hook can never fire
        /// `onCancelled` after `onSelected`.
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
        buildRequest : TreeBuildRequest
        /// The tree-node codes the user has explicitly expanded (spec 0040 step 002) — the tree is
        /// collapsed by default, so this starts empty and every top-level node opens collapsed.
        expandedNodes : ExpandedNodes
        /// The selected entry's `entryId` (the Edit / Remove verbs' target and the view panel's
        /// subject). The entry-id STRING is the Library domain's uniform entry-identity seam
        /// (`LibraryEntry.entryId` / `tryGetEntry` — a sample's elevated `SampleId` crosses it
        /// as its Guid string form; the workbench `pendingEntry` precedent).
        selectedEntryId : string option
        /// The EXACT tree-node code of the selection — the control highlights the row whose code
        /// matches, so an entry picked THROUGH a facet branch highlights that branch row, not the
        /// corpus-group copy (spec 0040 step 003; §0.2 keeps the code a control-seam `string` token,
        /// "" = nothing selected). `selectedEntryId` stays the domain selection; this is only its row.
        selectedNodeCode : string
        /// Whether the show-inactive/superseded toggle is on (spec 0038 step 023): `ActiveOnly`
        /// (the default — pickers/facet counts exclude retired samples) or `IncludeInactive` (the
        /// toggle adds retired samples to the tree, badged). The Domain DU, never a bool. Presets
        /// carry no lifecycle, so the toggle only affects the sample half of the corpus. Select
        /// mode ignores this and always lists `ActiveOnly` (a retired entry is never a valid pick).
        showInactive : InactiveVisibility
        /// Which sample remove (if any) awaits its inline confirmation.
        removeGate : LibraryRemoveGate
        /// Which lifecycle verb (if any) awaits its inline confirmation.
        lifecycleGate : SampleLifecycleGate
        /// Which OLDER version of the selected SAMPLE the view panel is showing read-only
        /// (`None` = the latest, the editable default; `Some v` = version `v` view-only — spec
        /// 0038 step 023). Reset to `None` on every selection change.
        viewedVersion : VersionNumber option
        /// The last typed refusal, surfaced as the inline message — never a cascade, never a
        /// dialog.
        lastError : LibraryWindowError option
    }

let init (context : LibraryWindowContext) (mode : LibraryWindowMode<LibraryEntry>) : Model =
    {
        context = context
        mode = mode
        appliedFacets = []
        textFilter = TextQuery ""
        representation = byKindRepresentation
        buildRequest = NoTreeBuildRequest
        expandedNodes = ExpandedNodes.empty
        selectedEntryId = None
        selectedNodeCode = ""
        showInactive = ActiveOnly
        removeGate = NoPendingRemove
        lifecycleGate = NoPendingLifecycle
        viewedVersion = None
        lastError = None
    }

type Msg =
    /// Apply the selection as this facet's (single) constraint — an offered-value or bucket
    /// click, or a parsed manual min–max entry; re-applying a constrained facet REPLACES its
    /// selection.
    | ApplyFacetConstraint of AttributeKey * FacetSelection
    /// Remove the applied constraint of this facet — a breadcrumb-chip click.
    | RemoveFacet of AttributeKey
    /// Commit the filter box's text (Enter/LostFocus only — never per keystroke).
    | CommitTextFilter of string
    /// Choose the named representation with this code (tree reshaping only — the applied
    /// constraints and the result set are untouched: search order ≠ representation order).
    | ChooseRepresentation of string
    /// Materialize the gated tree (the Show/Search button).
    | RequestTreeBuild
    /// Toggle one tree node's expansion (a disclosure-chevron press — spec 0040 step 002).
    | ToggleNode of string
    /// Select the entry leaf with this `entryId` (the view panel's subject). The selection
    /// highlights the CORPUS-group row (the canonical `entry:<entryId>` code) — the id-only path
    /// for programmatic / verb-driven selection.
    | SelectEntry of string
    /// Select an entry leaf by its EXACT tree-node code (the `entryId` it resolves to, plus the
    /// code the user actually clicked). Highlights that node — so picking an entry through a facet
    /// branch highlights the branch row, not the corpus-group copy (spec 0040 step 003).
    | SelectEntryNode of string * string
    /// The verbs, rewired from the retired Library bay (spec 0038 step 015).
    | AddSample
    | MakeMultilayer
    | EditSelected
    | RequestRemoveSelected
    | ConfirmRemove
    | CancelRemove
    /// The show-inactive/superseded toggle (spec 0038 step 023): flip the sample scope between
    /// `ActiveOnly` and `IncludeInactive`.
    | ToggleShowInactive
    /// The lifecycle verbs on the selected SAMPLE, confirm-gated inline (spec 0038 step 023).
    /// `Request…` arms the gate; `ConfirmLifecycle` runs the matching proxy verb (a refusal
    /// surfaces as the typed inline message); `CancelLifecycle` disarms.
    | RequestMarkInactive
    | RequestMarkActive
    | RequestSupersede
    | ConfirmLifecycle
    | CancelLifecycle
    /// Show an OLDER version of the selected sample read-only in the view panel (spec 0038 step 023).
    | ViewVersion of VersionNumber
    /// The Select-state pair (spec 0038 step 016). 'Select' returns the HIGHLIGHTED entry
    /// through the session's `onSelected` (a targeted dispatch), then closes; no highlight →
    /// inert. 'Close' fires `onCancelled`, then closes.
    | ConfirmSelect
    | CancelSelect
    /// A Select-state open met this LIVE window: re-point the session at the new context
    /// (the superseded session is cancelled — a second Choose closes the first, logically)
    /// and clear the highlight; the window itself stays (the launcher's re-target seam).
    | RetargetSelect of SelectionContext<LibraryEntry>
    /// The host window CLOSED (the title-bar X, or a staleness `Close()` from the requesting
    /// surface): cancel a still-pending session exactly once — a session already resolved by
    /// Select/Close flipped the mode to Browse first, so this can never double-fire.
    | SelectDismissed

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
        |> List.distinctBy (fun entry -> entry.entryId)
    samples @ presets

/// The corpus one projection pass works over: the live entries — in Select state pre-narrowed
/// to the session's kind constraint (spec 0038 step 016). The constraint is applied at THIS
/// corpus seam, so it is structurally NON-REMOVABLE: no breadcrumb chip exists to remove, and
/// every count, offer, tree row and selection already lives inside it.
/// `LibraryEntry.forKinds` is the existing kind-eligibility rule (a compound/custom polarizer
/// serves BOTH polarizer kinds — spec 0038 step 014), never re-derived.
/// The scope the corpus lists samples at (spec 0038 step 023): `ActiveOnly` in Select mode (a
/// retired sample is never a valid pick target), else the window's `showInactive` toggle. The
/// default `ActiveOnly` keeps offers, facet counts and Select byte-for-byte the step-022 behaviour.
let effectiveScope (m : Model) : InactiveVisibility =
    match m.mode with
    | Select _ -> ActiveOnly
    | Browse -> m.showInactive

let constrainedEntries (m : Model) : LibraryEntry list =
    let entries = liveEntries m.context (effectiveScope m)
    match m.mode with
    | Browse -> entries
    | Select context -> entries |> List.filter (fun e -> e.forKinds |> List.contains context.kindConstraint.value)

/// The text filter's facet key (the filter is an ORDINARY engine constraint — spec 0038 §D.0 —
/// over the entry's display name; its UI surface stays the filter box, not a chip).
let libraryTextFilterKey : AttributeKey = AttributeKey "library-text"

/// One projection pass's engine inputs, read from the live proxies exactly once.
type private ProjectionInputs =
    {
        defs : AttributeDef<LibraryEntry> list
        /// The committed text filter as an applied constraint — empty query applies nothing.
        textApplied : AppliedConstraint list
        /// The FULL applied list: the text filter (first) then the facet chips in order.
        appliedAll : AppliedConstraint list
        corpus : LibraryEntry list
    }

let private projectionInputs (m : Model) : ProjectionInputs =
    let materialCorpus =
        match m.context.materials.listMaterials ActiveOnly with
        | Ok entries -> entries
        | Error _ -> []
    let textDef = textFilterDef libraryTextFilterKey "Text" (fun (e : LibraryEntry) -> e.displayName) m.textFilter
    let defs = textDef :: libraryFacets materialCorpus
    let textApplied =
        match m.textFilter.value with
        | "" -> []
        | _ -> [ textFilterConstraint libraryTextFilterKey ]
    {
        defs = defs
        textApplied = textApplied
        appliedAll = textApplied @ m.appliedFacets
        corpus = constrainedEntries m
    }

/// The result set under everything applied (text filter AND facet constraints) — public so
/// narrowing is unit-testable without a window.
let filteredEntries (m : Model) : LibraryEntry list =
    let inputs = projectionInputs m
    Facets.filter inputs.defs inputs.appliedAll inputs.corpus

/// The selected entry resolved through the LIVE (and, in Select state, kind-constrained)
/// corpus — a removed sample's selection resolves to nothing, so its panel vanishes with its
/// row, and a Select session can only ever return an entry inside its constraint.
let selectedEntry (m : Model) : LibraryEntry option =
    match m.selectedEntryId with
    | Some id -> constrainedEntries m |> List.tryFind (fun e -> e.entryId = id)
    | None -> None

/// The selected entry as an editable SAMPLE — the Edit verb's target. Only samples have an
/// editor; a preset selection offers NO Edit affordance (removed, not greyed — the
/// `MaterialsControls` discipline).
let editableSample (m : Model) : Sample option =
    match selectedEntry m with
    | Some (SampleItem sample) -> Some sample
    | Some (SourceItem _ | DetectorItem _ | PolarizerItem _) | None -> None

// ---------------------------------------------------------------------------
// Lifecycle (spec 0038 step 023): only SAMPLES version and retire (presets are
// protected — no version store, no lifecycle verbs). The active-id set, the
// selected sample's live/retired state, the offered verbs, the version list.
// ---------------------------------------------------------------------------

/// The sample ids whose LATEST version is active (the `ActiveOnly` listing — read live every pass).
/// A sample present in `IncludeInactive` but NOT here has a retired latest version; that is how the
/// window distinguishes an inactive sample to badge it and to offer Mark active.
let private activeSampleIds (m : Model) : Set<SampleId> =
    match m.context.samples.listSamples ActiveOnly with
    | Ok samples -> samples |> List.map (fun s -> s.id) |> Set.ofList
    | Error _ -> Set.empty

/// A selected SAMPLE's lifecycle (spec 0038 step 023): `ActiveEntry` when its latest version is in
/// the active set, `InactiveEntry` when it resolves but its latest is retired, `None` when it no
/// longer resolves.
let sampleLifecycle (m : Model) (id : SampleId) : EntryLifecycle option =
    match m.context.samples.tryGetSample id with
    | Ok (Some _) ->
        if Set.contains id (activeSampleIds m) then Some ActiveEntry else Some InactiveEntry
    | Ok None | Error _ -> None

/// The lifecycle verbs offered for the current selection (spec 0038 step 023) — the ONE source of
/// truth the verbs row renders from and the tests assert against. `ProtectedBuiltIn` entries (the
/// presets) offer NONE (removed, not greyed — requirement 2); a `UserManaged` non-sample has no
/// version store, so none either; a selected SAMPLE offers Mark inactive + Supersede… when active,
/// Mark active when retired. Any Select-mode selection offers none.
let offeredLifecycleActions (m : Model) : LifecycleAction list =
    match m.mode with
    | Select _ -> []
    | Browse ->
        match selectedEntry m with
        | None -> []
        | Some entry ->
            match entry.protection with
            | ProtectedBuiltIn -> []
            | UserManaged ->
                match entry with
                | SampleItem sample ->
                    match sampleLifecycle m sample.id with
                    | Some ActiveEntry -> [ MarkInactiveAction; SupersedeAction ]
                    | Some InactiveEntry -> [ MarkActiveAction ]
                    | None -> []
                | SourceItem _ | DetectorItem _ | PolarizerItem _ -> []

/// The number of samples whose latest version is retired (spec 0038 step 023) — the visible count
/// badge on the show-inactive toggle. Read over the whole samples store (not the filtered corpus),
/// so the badge is a stable "N retired exist" hint whether the toggle is on or off.
let inactiveCount (m : Model) : int =
    let active = activeSampleIds m
    match m.context.samples.listSamples IncludeInactive with
    | Ok all -> all |> List.filter (fun s -> not (Set.contains s.id active)) |> List.length
    | Error _ -> 0

/// Whether a library entry (its latest version) is a retired sample, for the tree-leaf badge.
/// Presets never retire.
let private isEntryInactive (activeIds : Set<SampleId>) (entry : LibraryEntry) : bool =
    match entry with
    | SampleItem sample -> not (Set.contains sample.id activeIds)
    | SourceItem _ | DetectorItem _ | PolarizerItem _ -> false

/// The badge appended to a retired sample's tree-leaf label when the show-inactive toggle reveals
/// it (spec 0038 step 023). Supersede and mark-inactive share `InactiveEntry`, so it reads
/// "inactive" for both.
let inactiveBadge : string = " — inactive"

/// The stored versions of a SAMPLE, ascending (latest last), enumerated by probing the proxy's
/// by-version `resolveVersion` from version 1 upward until it resolves nothing (spec 0038 step 023
/// — the store exposes no list-versions field). In the live in-memory store this is a single
/// version until step 25 (mints need a used version, and `VersionsInUse` is empty); a stub proxy
/// returns a longer history. The `1000` cap is an unreachable runaway guard.
let selectedVersionsOf (samples : SampleProxy) (id : SampleId) : (VersionNumber * Sample) list =
    let rec loop (version : VersionNumber) (acc : (VersionNumber * Sample) list) : (VersionNumber * Sample) list =
        if version.value > 1000 then List.rev acc
        else
            match samples.resolveVersion { sampleId = id; version = version } with
            | Ok (Some sample) -> loop version.next ((version, sample) :: acc)
            | Ok None | Error _ -> List.rev acc
    loop VersionNumber.first []

/// The selected SAMPLE's versions (empty for a preset, no selection, or a vanished id).
let selectedVersions (m : Model) : (VersionNumber * Sample) list =
    match editableSample m with
    | Some sample -> selectedVersionsOf m.context.samples sample.id
    | None -> []

// ---------------------------------------------------------------------------
// Numeric range tokens (the domain-free control speaks string codes; every
// token is lifted back to an elevated engine value at the handler boundary).
// ---------------------------------------------------------------------------

let private tryParseDouble (s : string) : double option =
    match Double.TryParse(s.Trim(), NumberStyles.Float, CultureInfo.InvariantCulture) with
    | true, v -> Some v
    | _ -> None

/// A numeric range's stable value code: round-trip-formatted endpoints joined by ':' — a
/// separator no numeric text form contains (an exponent's '-' could split a hyphen scheme).
let numericRangeCode (range : NumericRange) : string =
    let part (v : double) : string = v.ToString("R", CultureInfo.InvariantCulture)
    $"{part range.lower}:{part range.upper}"

let numericRangeOfCode (code : string) : NumericRange option =
    match code.Split ':' with
    | [| lo; hi |] ->
        match tryParseDouble lo, tryParseDouble hi with
        | Some lower, Some upper -> Some { lower = lower; upper = upper }
        | _ -> None
    | _ -> None

/// Parse the manual min–max box's RAW text (spec 0038 §D.0: a manual entry applies as an
/// ordinary constraint chip): "lo-hi" (both endpoints in nm), or a single value as the
/// degenerate exact-value range. Anything else is the host's no-op — the control's contract.
let parseManualRange (raw : string) : NumericRange option =
    let text = raw.Trim()
    match text with
    | "" -> None
    | _ ->
        match text.Split '-' with
        | [| single |] -> tryParseDouble single |> Option.map (fun v -> { lower = v; upper = v })
        | [| lo; hi |] ->
            match tryParseDouble lo, tryParseDouble hi with
            | Some lower, Some upper -> Some { lower = lower; upper = upper }
            | _ -> None
        | _ -> None

/// A range's display text: the step-010 bucket label MINUS its trailing count — the ONE home
/// of the nm/µm unit switch (`NumericBucket.label`, format pinned by the FacetBuckets tests)
/// reused rather than re-derived (§D.0).
let numericRangeText (range : NumericRange) : string =
    let labelled = ({ range = range; count = ItemCount 0 } : NumericBucket).label
    match labelled.LastIndexOf " (" with
    | index when index > 0 -> labelled.Substring(0, index)
    | _ -> labelled

// ---------------------------------------------------------------------------
// Tree node codes (host-supplied stable tokens, unique across the whole tree).
// ---------------------------------------------------------------------------

/// `entries` / `entry:<entryId>` name the corpus group and its selectable leaves;
/// `facet:<facet-key>` / `branch:<facet-key>:<value-key>` name the representation's facet
/// grouping, and a facet branch's own selectable member leaves are path-shaped
/// `branch:<facet-key>:<value-key>:entry:<entryId>` (tree-unique, so the SAME entry can leaf both
/// the corpus group and every branch it belongs to). Only an ENTRY-leaf code — the top-level
/// `entry:<entryId>` OR a branch-nested `…:entry:<entryId>` — means anything to `selectNode`; a
/// bare `facet:` / `branch:` code is grouping display (the OFFERS are the apply surface).
let entryNodeCode (entryId : string) : string = "entry:" + entryId

/// Resolve any entry-leaf node code back to its `entryId`: the top-level `entry:<entryId>`
/// (prefix) or a branch-nested `…:entry:<entryId>` (the `:entry:` separator the branch code adds
/// before the leaf). A bare `facet:` / `branch:` / `entries` code carries no `entry:` marker and
/// resolves to `None`, so it stays inert at `selectNode`.
let entryIdOfNodeCode (code : string) : string option =
    let flatPrefix = "entry:"
    let nestedMarker = ":entry:"
    if code.StartsWith flatPrefix then Some (code.Substring flatPrefix.Length)
    else
        match code.LastIndexOf(nestedMarker, System.StringComparison.Ordinal) with
        | -1 -> None
        | idx -> Some (code.Substring (idx + nestedMarker.Length))

/// Spec 0038 (044): the Domain-typed id helpers that outlived the per-view `UiIds` module —
/// `OpticalConstructor.Controls` is domain-free, so `versionRow` (VersionNumber) and `entryNode`
/// (over the view-local `entryNodeCode`) stay beside the view; every fixed Library-window id literal
/// moved to `UiIds.LibraryWindow`.
let versionRow (version : VersionNumber) : string = "LibraryVersionRow_" + string version.value
let entryNode (entryId : string) : string = UiIds.FacetedTree.treeNode (entryNodeCode entryId)

// ---------------------------------------------------------------------------
// Pure update.
// ---------------------------------------------------------------------------

/// A query / selection change also disarms a pending remove confirmation and clears the inline
/// message — a stale confirm or refusal never outlives the state it referred to (the retired
/// bay's disarm discipline).
let private disarmed (m : Model) : Model =
    { m with removeGate = NoPendingRemove; lifecycleGate = NoPendingLifecycle; lastError = None }

/// Arm the lifecycle confirm gate on the selected SAMPLE (spec 0038 step 023) — clears the remove
/// gate and the inline message so at most one confirm is ever pending. A non-sample or empty
/// selection is inert (its verbs never render).
let private requestLifecycle (action : LifecycleAction) (m : Model) : Model =
    match editableSample m with
    | Some sample -> { m with lifecycleGate = PendingLifecycle (sample.id, action); removeGate = NoPendingRemove; lastError = None }
    | None -> m

let update (msg : Msg) (m : Model) : Model =
    match msg with
    | ApplyFacetConstraint (key, selection) ->
        // Single selection per facet: re-applying a constrained facet REPLACES its chip
        // (removed from its old breadcrumb position, re-applied at the end).
        let withoutKey = m.appliedFacets |> List.filter (fun c -> c.key <> key)
        { disarmed m with appliedFacets = withoutKey @ [ { key = key; selection = selection } ] }
    | RemoveFacet key ->
        { disarmed m with appliedFacets = m.appliedFacets |> List.filter (fun c -> c.key <> key) }
    | CommitTextFilter text ->
        { disarmed m with textFilter = TextQuery text }
    | ChooseRepresentation code ->
        // Reshapes the tree only — never the applied constraints or the result set.
        match offeredRepresentations |> List.tryFind (fun r -> r.code = code) with
        | Some r -> { m with representation = r }
        | None -> m
    | RequestTreeBuild ->
        { m with buildRequest = RequestedTreeBuild }
    | ToggleNode code ->
        // Flip one tree node's expansion (a disclosure-chevron press) — a display-only change, so
        // no gate is disarmed and no selection moves.
        { m with expandedNodes = m.expandedNodes.toggle code }
    | SelectEntry id ->
        // A new selection shows its LATEST version (the editable default) and disarms both gates.
        // The id-only path highlights the corpus-group row (the canonical `entry:<entryId>` code).
        { disarmed m with selectedEntryId = Some id; selectedNodeCode = entryNodeCode id; viewedVersion = None }
    | SelectEntryNode (id, code) ->
        // The same selection, but highlighting the EXACT clicked node — so an entry picked through
        // a facet branch highlights that branch row rather than the corpus-group copy.
        { disarmed m with selectedEntryId = Some id; selectedNodeCode = code; viewedVersion = None }
    | AddSample ->
        // Add mints the sample's SampleId HERE — at the window-open dispatch, off the save path
        // (spec 0038 step 008) — so the launcher's registry keys the new editor by the SAME id
        // its Save will persist under (a second Add mints a second id: two windows).
        m.context.openSampleEditor (SampleEditorView.NewBlankSample (newSampleId ()))
        disarmed m
    | MakeMultilayer ->
        // The DISTINCT Make-multilayer creation path (spec 0035 step 014): a NEW sample
        // pre-seeded with a foldable starter period — still minted at open, just not blank.
        m.context.openSampleEditor (SampleEditorView.NewSeededMultilayer (newSampleId ()))
        disarmed m
    | EditSelected ->
        // Edit opens the Sample editor on the selected SAMPLE, resolved through the live corpus
        // at dispatch time; a vanished or preset selection reaches no launcher (the verb is
        // simply absent for it — removed, not greyed).
        (match editableSample m with
         | Some sample -> m.context.openSampleEditor (SampleEditorView.EditSample sample)
         | None -> ())
        disarmed m
    | RequestRemoveSelected ->
        match selectedEntry m with
        | None -> m
        | Some entry ->
            match entry.protection with
            | ProtectedBuiltIn ->
                // Spec 0038 F.0: a protected built-in cannot be deleted — the verb REFUSES with
                // the typed reason AT the click, so the confirm gate never arms for it.
                { m with
                    removeGate = NoPendingRemove
                    lastError = Some (ProtectedEntryRefused $"'%s{entry.displayName}' is a protected built-in entry — it cannot be removed.") }
            | UserManaged ->
                match entry with
                | SampleItem sample -> { m with removeGate = PendingRemove sample.id; lastError = None }
                | SourceItem _ | DetectorItem _ | PolarizerItem _ ->
                    // Unreachable over today's corpus (every non-sample seed is protected) —
                    // total handling, never a throw.
                    { m with
                        removeGate = NoPendingRemove
                        lastError = Some (PresetNotRemovable $"'%s{entry.displayName}' has no removal store — preset entries cannot be removed here.") }
    | ConfirmRemove ->
        match m.removeGate with
        | PendingRemove id ->
            match m.context.samples.removeSample id with
            | Ok () ->
                { m with
                    removeGate = NoPendingRemove
                    lastError = None
                    selectedEntryId = (if m.selectedEntryId = Some (string id.value) then None else m.selectedEntryId)
                    selectedNodeCode = (if m.selectedEntryId = Some (string id.value) then "" else m.selectedNodeCode) }
            | Error err ->
                // The store refused (a typed SampleError) — surface the reason inline and leave
                // the store, selection and list untouched.
                { m with removeGate = NoPendingRemove; lastError = Some (SampleRemoveRefused err) }
        | NoPendingRemove -> m
    | CancelRemove ->
        // Cancel only ever disarms — it never dismisses the inline message (the next query /
        // selection change clears it).
        { m with removeGate = NoPendingRemove }
    | ToggleShowInactive ->
        let flipped =
            match m.showInactive with
            | ActiveOnly -> IncludeInactive
            | IncludeInactive -> ActiveOnly
        { disarmed m with showInactive = flipped }
    | RequestMarkInactive -> requestLifecycle MarkInactiveAction m
    | RequestMarkActive -> requestLifecycle MarkActiveAction m
    | RequestSupersede -> requestLifecycle SupersedeAction m
    | ConfirmLifecycle ->
        match m.lifecycleGate with
        | PendingLifecycle (id, action) ->
            let outcome =
                match action with
                | MarkInactiveAction -> m.context.samples.markSampleInactive id
                | MarkActiveAction -> m.context.samples.markSampleActive id
                | SupersedeAction -> m.context.samples.supersedeSample id
            match outcome with
            | Ok () -> { m with lifecycleGate = NoPendingLifecycle; lastError = None }
            | Error err ->
                // The store refused (e.g. the sample vanished behind the window's back) — surface
                // the typed reason inline and leave the store and selection untouched.
                { m with lifecycleGate = NoPendingLifecycle; lastError = Some (SampleLifecycleRefused err) }
        | NoPendingLifecycle -> m
    | CancelLifecycle ->
        // Cancel only ever disarms — the next query / selection change clears any inline message.
        { m with lifecycleGate = NoPendingLifecycle }
    | ViewVersion version ->
        { m with viewedVersion = Some version }
    | ConfirmSelect ->
        // Spec 0038 step 016: 'Select' returns the HIGHLIGHTED entry through the session's
        // onSelected — a TARGETED dispatch (the requesting surface routes it by the context's
        // target and treats a vanished target as a no-op plus a status line) — then closes.
        // The mode flips to Browse in the SAME update, so the host's Closed hook
        // (SelectDismissed, queued behind this message) finds no pending session: onSelected
        // and onCancelled can never both fire. No highlight → inert (the pair stays, §0.7).
        match m.mode with
        | Select context ->
            match selectedEntry m with
            | Some entry ->
                context.onSelected entry
                m.context.requestClose ()
                { m with mode = Browse }
            | None -> m
        | Browse -> m
    | CancelSelect ->
        // The negative half of the pair: end the session without a choice, then close.
        match m.mode with
        | Select context ->
            context.onCancelled ()
            m.context.requestClose ()
            { m with mode = Browse }
        | Browse -> m
    | RetargetSelect context ->
        // A Select-state open met this LIVE window (the launcher's re-target seam): the
        // superseded session is CANCELLED — a second Choose closes the first, logically —
        // and the window re-points at the new constraint/target with a fresh highlight
        // (the browsing state — filter, chips, representation — is the user's and stays).
        (match m.mode with
         | Select superseded -> superseded.onCancelled ()
         | Browse -> ())
        { disarmed m with mode = Select context; selectedEntryId = None; selectedNodeCode = "" }
    | SelectDismissed ->
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

let private isApplicableTo (def : AttributeDef<LibraryEntry>) (entry : LibraryEntry) : bool =
    match def.appliesTo entry with
    | ApplicableAttribute -> true
    | InapplicableAttribute -> false

/// The distinct discrete values a facet offers over the filtered population, in the engine's
/// key order.
let private offeredValuesFor (def : AttributeDef<LibraryEntry>) (filtered : LibraryEntry list) : DiscreteKey list =
    filtered
    |> List.filter (isApplicableTo def)
    |> List.collect (fun entry -> def.extract entry |> List.distinct)
    |> List.choose (fun value ->
        match value with
        | DiscreteValue k -> Some k
        | NumericValue _ -> None)
    |> List.distinct
    |> List.sortBy (fun k -> k.value)

/// A chip's display label: the facet's name plus its selection — the discrete key(s), or the
/// numeric range through the step-010 unit switch.
let private chipLabel (defs : AttributeDef<LibraryEntry> list) (applied : AppliedConstraint) : string =
    let facetName =
        match defs |> List.tryFind (fun d -> d.key = applied.key) with
        | Some def -> def.name
        | None -> applied.key.value
    let valueText =
        match applied.selection with
        | DiscreteSelection keys -> keys |> Set.toList |> List.map (fun k -> k.value) |> String.concat ", "
        | NumericRangeSelection range -> numericRangeText range
    $"%s{facetName}: %s{valueText}"

/// Sort projected tree nodes alphabetically (case-insensitive, ordinal) by their display
/// label (operator 010/Q1): the top-level facet groups and the entry leaves read
/// alphabetically, OVERRIDING the representation / corpus order the engine hands back.
let private sortNodesByLabel (nodes : FacetedTreeControls.TreeNode list) : FacetedTreeControls.TreeNode list =
    nodes
    |> List.sortWith (fun (a : FacetedTreeControls.TreeNode) (b : FacetedTreeControls.TreeNode) ->
        String.Compare(a.label, b.label, StringComparison.OrdinalIgnoreCase))

/// Project the live corpus through the engine into the domain-free control state. Everything
/// here is recomputed per render over the CURRENT proxies (offers, counts, buckets, tree,
/// previews), so a verb's write — this window's or another's — shows in the same pass.
let facetedState (m : Model) : FacetedTreeControls.State =
    let inputs = projectionInputs m
    let filtered = Facets.filter inputs.defs inputs.appliedAll inputs.corpus
    let resultCount = List.length filtered
    // The active sample-id set, for the retired-sample leaf badge (spec 0038 step 023): with the
    // toggle off the sample corpus is `ActiveOnly`, so no leaf ever badges; with it on, a retired
    // sample's latest version reads `label — inactive` (presets never badge).
    let activeIds = activeSampleIds m
    // The tree is collapsed by default (spec 0040 step 002): a node renders `ExpandedNode` only
    // when its code is in the persisted expanded-node set, `CollapsedNode` otherwise.
    let expansionOf (code : string) : FacetedTreeControls.NodeExpansion =
        if m.expandedNodes.isExpanded code then FacetedTreeControls.ExpandedNode
        else FacetedTreeControls.CollapsedNode
    // One selectable entry leaf under the given (tree-unique) code — the shared shape both the
    // corpus "entries" group and every facet branch project (an entry leafs its branch AND the
    // corpus group, under distinct codes so the tree stays uniquely keyed).
    let entryLeafNode (leafCode : string) (entry : LibraryEntry) : FacetedTreeControls.TreeNode =
        {
            code = leafCode
            label = (if isEntryInactive activeIds entry then entry.displayName + inactiveBadge else entry.displayName)
            countOpt = None
            expansion = expansionOf leafCode
            children = []
        }
    // The host decides gating (result count above the Domain threshold, no explicit build yet);
    // the control only obeys (step 012). A gated pass projects NO tree at all — the whole point
    // is skipping the one potentially heavy render (§0.7).
    let materialization =
        if resultCount > m.context.treeAutoBuildThreshold.value then
            match m.buildRequest with
            | RequestedTreeBuild -> FacetedTreeControls.TreeMaterialized
            | NoTreeBuildRequest -> FacetedTreeControls.TreeGated
        else FacetedTreeControls.TreeMaterialized
    // Breadcrumbs: the facet chips in application order, after-counts cumulative over the
    // text-searched population (the filter box narrows first; it takes no chip of its own —
    // its committed text stays visible in the box).
    let searched = Facets.filter inputs.defs inputs.textApplied inputs.corpus
    let breadcrumbs =
        Facets.breadcrumbCounts inputs.defs m.appliedFacets searched
        |> List.map (fun bc ->
            ({
                code = bc.applied.key.value
                label = chipLabel inputs.defs bc.applied
                afterCount = bc.afterCount.value
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
                            code = def.key.value
                            title = def.name
                            values =
                                buckets
                                |> List.map (fun bucket ->
                                    ({
                                        code = numericRangeCode bucket.range
                                        label = numericRangeText bucket.range
                                        // The bucket derives from the constrained population,
                                        // so its count IS the apply preview (pinned, step 010).
                                        previewCount = bucket.count.value
                                     } : FacetedTreeControls.OfferedValue))
                            manualRange = FacetedTreeControls.ManualRangeOffered
                         } : FacetedTreeControls.OfferGroup)
            | DiscreteAttribute ->
                match offeredValuesFor def filtered with
                | [] -> None
                | values ->
                    Some
                        ({
                            code = def.key.value
                            title = def.name
                            values =
                                values
                                |> List.map (fun k ->
                                    let candidate : AppliedConstraint =
                                        { key = def.key; selection = DiscreteSelection (Set.singleton k) }
                                    ({
                                        code = k.value
                                        label = k.value
                                        previewCount = (Facets.countFor inputs.defs inputs.appliedAll candidate inputs.corpus).value
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
                    code = "entries"
                    label = "Library"
                    countOpt = Some resultCount
                    expansion = expansionOf "entries"
                    children =
                        filtered
                        |> List.map (fun entry -> entryLeafNode (entryNodeCode entry.entryId) entry)
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
                    let branchesWithMembers : (string * string * int * LibraryEntry list) list =
                        match inputs.defs |> List.tryFind (fun d -> d.key = facet.key) with
                        | Some def when def.kind = NumericAttribute ->
                            bucketsOf def
                            |> List.map (fun bucket ->
                                let branchCode = "branch:" + facet.key.value + ":" + numericRangeCode bucket.range
                                let members = Facets.filter inputs.defs (inputs.appliedAll @ [ { key = facet.key; selection = NumericRangeSelection bucket.range } ]) inputs.corpus
                                // The badge is the DISTINCT member entries (what expanding reveals),
                                // not `bucket.count` — a numeric bucket counts value-occurrences, so an
                                // entry with two in-range layer thicknesses would over-count the leaves.
                                branchCode, numericRangeText bucket.range, List.length members, members)
                        | Some _ | None ->
                            facet.branches
                            |> List.map (fun branch ->
                                let branchCode = "branch:" + facet.key.value + ":" + branch.value.label
                                let members =
                                    match branch.value with
                                    | DiscreteValue dk -> Facets.filter inputs.defs (inputs.appliedAll @ [ { key = facet.key; selection = DiscreteSelection (Set.singleton dk) } ]) inputs.corpus
                                    | NumericValue _ -> []
                                branchCode, branch.label, branch.count.value, members)
                    let branchNodes =
                        branchesWithMembers
                        |> List.map (fun (branchCode, label, count, members) ->
                            ({
                                code = branchCode
                                label = label
                                countOpt = Some count
                                expansion = expansionOf branchCode
                                // The branch expands to its member entries as selectable leaves
                                // (path-shaped codes keep them tree-unique), alphabetical by label.
                                children =
                                    members
                                    |> List.map (fun entry -> entryLeafNode (branchCode + ":" + entryNodeCode entry.entryId) entry)
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
                    ({
                        code = "facet:" + facet.key.value
                        label = facet.name
                        countOpt = Some groupCount
                        expansion = expansionOf ("facet:" + facet.key.value)
                        children = branchNodes
                     } : FacetedTreeControls.TreeNode))
                |> sortNodesByLabel
            entriesNode :: facetNodes
    {
        tree = tree
        breadcrumbs = breadcrumbs
        offers = offers
        representations =
            offeredRepresentations
            |> List.map (fun r -> ({ code = r.code; label = r.label } : FacetedTreeControls.NamedRepresentation))
        activeRepresentation = m.representation.code
        filterDraft = m.textFilter.value
        resultCount = resultCount
        materialization = materialization
        // The EXACT selected node code (spec 0040 step 003) — the control highlights the row whose
        // code matches, so an entry picked through a facet branch highlights that branch row, not
        // the corpus-group copy. The Model tracks the clicked code; "" = nothing selected.
        selectedCode = m.selectedNodeCode
    }

/// The control's behaviour seam: every token is lifted back to its domain value HERE, at the
/// control boundary. Only an `entry:` node code selects (branches/headings are grouping
/// display; the offers are the apply surface); the film-thickness group's tokens parse as
/// numeric ranges (a bucket's `numericRangeCode`, or the manual box's raw "lo-hi" text) — an
/// unparseable token is the documented no-op.
let facetedHandlers (dispatch : Msg -> unit) : FacetedTreeControls.Handlers =
    {
        applyConstraint =
            fun groupCode valueCode ->
                let key = AttributeKey groupCode
                if key = sampleFilmThicknessKey then
                    match numericRangeOfCode valueCode with
                    | Some range -> dispatch (ApplyFacetConstraint (key, NumericRangeSelection range))
                    | None -> ()
                else
                    dispatch (ApplyFacetConstraint (key, DiscreteSelection (Set.singleton (DiscreteKey valueCode))))
        removeConstraint = fun chipCode -> dispatch (RemoveFacet (AttributeKey chipCode))
        commitTextFilter = fun text -> dispatch (CommitTextFilter text)
        chooseRepresentation = fun code -> dispatch (ChooseRepresentation code)
        requestBuild = fun () -> dispatch RequestTreeBuild
        selectNode =
            fun code ->
                match entryIdOfNodeCode code with
                | Some id -> dispatch (SelectEntryNode (id, code))
                | None -> ()
        toggleNode = fun code -> dispatch (ToggleNode code)
        applyManualRange =
            fun groupCode raw ->
                match parseManualRange raw with
                | Some range -> dispatch (ApplyFacetConstraint (AttributeKey groupCode, NumericRangeSelection range))
                | None -> ()
    }

// ---------------------------------------------------------------------------
// The view: the faceted tree beside the verbs + view panel.
// ---------------------------------------------------------------------------

let private color (r : int) (g : int) (b : int) : Color = Color.FromRgb(byte r, byte g, byte b)
let private brush (c : Color) : IBrush = SolidColorBrush(c) :> IBrush
let private idleBackground = color 232 232 232
let private idleBorder = color 120 120 120
let private messageColor = color 178 34 34
// The positive/negative action pair backgrounds (the Save/Cancel precedent, SampleEditorView).
let private positiveBackground = color 186 224 186
let private negativeBackground = color 236 202 202

/// Set `AutomationProperties.AutomationId` (a freely-mutable attached property — unlike
/// `Control.Name`) through FuncUI's attr builder: the verbs, confirm row, message and panel
/// all have variable membership, so nothing here sets `StyledElement.Name` (the
/// `FacetedTreeControls` discipline).
let private automationId<'View when 'View :> Control> (autoId : string) : IAttr<'View> =
    AttrBuilder<'View>.CreateProperty<string>(AutomationProperties.AutomationIdProperty, autoId, ValueNone)

/// A clickable verb box (the shared workbench button look), KEYED by its id so membership
/// changes recreate a shifted box; `e.Handled <- true` drops FuncUI's duplicate Tunnel|Bubble
/// pass.
let private verbButton (autoId : string) (label : string) (onClick : unit -> unit) : IView =
    let keyedBox =
        Border.create [
            automationId<Border> autoId
            Border.background (brush idleBackground)
            Border.borderBrush (brush idleBorder)
            Border.borderThickness 1.0
            Border.cornerRadius (CornerRadius 3.0)
            Border.padding (Thickness(12.0, 5.0))
            Border.margin (Thickness(0.0, 0.0, 8.0, 4.0))
            Border.verticalAlignment VerticalAlignment.Center
            Border.child (TextBlock.create [ TextBlock.text label ])
            Border.onPointerPressed ((fun e -> e.Handled <- true; onClick ()), SubPatchOptions.OnChangeOf autoId)
        ]
        |> Avalonia.FuncUI.DSL.View.withKey autoId
    keyedBox :> IView

/// A positive/negative ACTION box (the Save/Cancel styling precedent) — same look as
/// `verbButton` but colour-coded and wider-padded; keyed, AutomationId'd (variable
/// membership: the pair exists only in Select state).
let private actionButton (autoId : string) (label : string) (background : Color) (onClick : unit -> unit) : IView =
    let keyedBox =
        Border.create [
            automationId<Border> autoId
            Border.background (brush background)
            Border.borderBrush (brush idleBorder)
            Border.borderThickness 1.0
            Border.cornerRadius (CornerRadius 3.0)
            Border.padding (Thickness(22.0, 6.0))
            Border.margin (Thickness(0.0, 0.0, 10.0, 4.0))
            Border.verticalAlignment VerticalAlignment.Center
            Border.child (TextBlock.create [ TextBlock.text label ])
            Border.onPointerPressed ((fun e -> e.Handled <- true; onClick ()), SubPatchOptions.OnChangeOf autoId)
        ]
        |> Avalonia.FuncUI.DSL.View.withKey autoId
    keyedBox :> IView

/// The requesting surface a Select session serves, as banner prose.
let private targetText (target : SelectionTarget) : string =
    match target with
    | TableElementTarget _ -> "the requesting table element"
    | SampleLayerTarget _ -> "the requesting sample layer"
    | SampleSubstrateTarget -> "the requesting sample substrate plate"

/// The Select-state surface (spec 0038 step 016): exactly TWO buttons in ONE row with distinct
/// positive/negative styling and a visible gap (§0.7) — 'Select' returns the highlighted entry,
/// 'Close' cancels — above the pre-applied kind constraint shown as a FIXED banner: it is not a
/// breadcrumb chip and offers no remove affordance (the constraint lives at the corpus seam).
/// Absent in Browse mode — everything else IS the ordinary window.
let private selectModeRows (m : Model) (dispatch : Msg -> unit) : IView list =
    match m.mode with
    | Browse -> []
    | Select context ->
        [ WrapPanel.create [
              WrapPanel.orientation Orientation.Horizontal
              WrapPanel.children [
                  actionButton UiIds.LibraryWindow.selectButton "Select" positiveBackground (fun () -> dispatch ConfirmSelect)
                  actionButton UiIds.LibraryWindow.selectCloseButton "Close" negativeBackground (fun () -> dispatch CancelSelect)
              ]
          ] :> IView
          (TextBlock.create [
              automationId<TextBlock> UiIds.LibraryWindow.selectConstraint
              TextBlock.fontWeight FontWeight.SemiBold
              TextBlock.textWrapping TextWrapping.Wrap
              TextBlock.maxWidth 380.0
              TextBlock.text $"Choose a %s{Catalogue.kindName context.kindConstraint.value} for %s{targetText context.target} — the kind constraint is fixed and cannot be removed."
          ]
          |> Avalonia.FuncUI.DSL.View.withKey UiIds.LibraryWindow.selectConstraint) :> IView ]

/// One lifecycle verb box (spec 0038 step 023), by its action.
let private lifecycleButton (dispatch : Msg -> unit) (action : LifecycleAction) : IView =
    match action with
    | MarkInactiveAction -> verbButton UiIds.LibraryWindow.markInactiveButton "Mark inactive" (fun () -> dispatch RequestMarkInactive)
    | MarkActiveAction -> verbButton UiIds.LibraryWindow.markActiveButton "Mark active" (fun () -> dispatch RequestMarkActive)
    | SupersedeAction -> verbButton UiIds.LibraryWindow.supersedeButton "Supersede…" (fun () -> dispatch RequestSupersede)

/// The verbs row: Add sample and Make multilayer always; Edit only for a SAMPLE selection (a
/// preset has no editor — the verb is removed, not greyed); Remove while an entry is selected
/// (a protected entry's Remove REFUSES with the typed reason instead of arming the gate); the
/// lifecycle verbs from `offeredLifecycleActions` (only for a UserManaged sample — a protected
/// preset exposes NONE, spec 0038 step 023).
let private verbsRow (m : Model) (dispatch : Msg -> unit) : IView =
    WrapPanel.create [
        WrapPanel.orientation Orientation.Horizontal
        WrapPanel.children (
            [
                verbButton UiIds.LibraryWindow.addSampleButton "Add sample" (fun () -> dispatch AddSample)
                verbButton UiIds.LibraryWindow.makeMultilayerButton "Make multilayer" (fun () -> dispatch MakeMultilayer)
            ]
            @ (match editableSample m with
               | Some _ -> [ verbButton UiIds.LibraryWindow.editButton "Edit" (fun () -> dispatch EditSelected) ]
               | None -> [])
            @ (match selectedEntry m with
               | Some _ -> [ verbButton UiIds.LibraryWindow.removeButton "Remove" (fun () -> dispatch RequestRemoveSelected) ]
               | None -> [])
            @ (offeredLifecycleActions m |> List.map (lifecycleButton dispatch)))
    ] :> IView

/// The show-inactive/superseded toggle row (spec 0038 step 023): a keyed verb box carrying the
/// visible count badge — `Show inactive (N)` while hidden, `Hide inactive (N)` while shown.
/// Browse-mode only (Select always lists `ActiveOnly`).
let private toggleRow (m : Model) (dispatch : Msg -> unit) : IView list =
    match m.mode with
    | Select _ -> []
    | Browse ->
        let n = inactiveCount m
        let label =
            match m.showInactive with
            | ActiveOnly -> $"Show inactive (%d{n})"
            | IncludeInactive -> $"Hide inactive (%d{n})"
        [ verbButton UiIds.LibraryWindow.showInactiveToggle label (fun () -> dispatch ToggleShowInactive) ]

/// The inline remove confirmation — present only while a sample remove is armed.
let private confirmRow (m : Model) (dispatch : Msg -> unit) : IView list =
    match m.removeGate with
    | NoPendingRemove -> []
    | PendingRemove id ->
        let name =
            match m.context.samples.tryGetSample id with
            | Ok (Some sample) -> sample.name
            | Ok None | Error _ -> string id.value
        [ WrapPanel.create [
              WrapPanel.orientation Orientation.Horizontal
              WrapPanel.children [
                  TextBlock.create [
                      TextBlock.text $"Remove sample '%s{name}'?"
                      TextBlock.verticalAlignment VerticalAlignment.Center
                      TextBlock.margin (Thickness(0.0, 0.0, 8.0, 4.0))
                  ]
                  verbButton UiIds.LibraryWindow.removeConfirmButton "Remove" (fun () -> dispatch ConfirmRemove)
                  verbButton UiIds.LibraryWindow.removeCancelButton "Cancel" (fun () -> dispatch CancelRemove)
              ]
          ] :> IView ]

/// The inline lifecycle confirmation (spec 0038 step 023) — present only while a lifecycle verb is
/// armed. The prompt NAMES the sample and the transition (the verb the user pressed); Confirm runs
/// the matching proxy verb, Cancel disarms.
let private lifecycleConfirmRow (m : Model) (dispatch : Msg -> unit) : IView list =
    match m.lifecycleGate with
    | NoPendingLifecycle -> []
    | PendingLifecycle (id, action) ->
        let name =
            match m.context.samples.tryGetSample id with
            | Ok (Some sample) -> sample.name
            | Ok None | Error _ -> string id.value
        let prompt =
            match action with
            | MarkInactiveAction -> $"Mark sample '%s{name}' inactive?"
            | MarkActiveAction -> $"Mark sample '%s{name}' active?"
            | SupersedeAction -> $"Supersede sample '%s{name}'?"
        [ WrapPanel.create [
              WrapPanel.orientation Orientation.Horizontal
              WrapPanel.children [
                  TextBlock.create [
                      TextBlock.text prompt
                      TextBlock.verticalAlignment VerticalAlignment.Center
                      TextBlock.margin (Thickness(0.0, 0.0, 8.0, 4.0))
                  ]
                  verbButton UiIds.LibraryWindow.lifecycleConfirmButton "Confirm" (fun () -> dispatch ConfirmLifecycle)
                  verbButton UiIds.LibraryWindow.lifecycleCancelButton "Cancel" (fun () -> dispatch CancelLifecycle)
              ]
          ] :> IView ]

/// The inline typed-refusal message. Every case carries its diagnostic reason — never a
/// cascade, never a dialog.
let private messageRow (m : Model) : IView list =
    match m.lastError with
    | None -> []
    | Some err ->
        let text =
            match err with
            | ProtectedEntryRefused reason
            | PresetNotRemovable reason -> reason
            | SampleRemoveRefused sampleError
            | SampleLifecycleRefused sampleError ->
                match sampleError with
                | UnknownSampleId reason
                | DuplicateSampleId reason
                | SampleVersionInUse reason
                | InvalidSample reason -> reason
        [ TextBlock.create [
              automationId<TextBlock> UiIds.LibraryWindow.message
              TextBlock.foreground (brush messageColor)
              TextBlock.textWrapping TextWrapping.Wrap
              TextBlock.maxWidth 380.0
              TextBlock.text text
          ] :> IView ]

/// The version list (spec 0038 step 023): one clickable row per stored SAMPLE version, the latest
/// marked `(latest)` and the currently-viewed one marked `▸`. Clicking a row shows THAT version in
/// the panel — the latest editable through the Edit verb, an older one view-only. Absent for a
/// preset or a single-version sample (the common case in the live store until step 25).
let private versionsRow (versions : (VersionNumber * Sample) list) (latest : VersionNumber) (shown : VersionNumber) (dispatch : Msg -> unit) : IView list =
    match versions with
    | [] | [ _ ] -> []
    | _ ->
        [ StackPanel.create [
              automationId<StackPanel> UiIds.LibraryWindow.versionsPanel
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
                          verbButton (versionRow version) $"%s{marker}v%d{version.value}%s{latestTag}" (fun () -> dispatch (ViewVersion version)))))
          ] :> IView ]

/// The view panel: the selected entry's kind, protection state and FULL description (the
/// Library confirm-step / Details prose — `fullDescription` spells out what the entry IS), plus a
/// SAMPLE's version list (spec 0038 step 023). By default it shows the LATEST version (the editable
/// one — the library always edits the latest); selecting an OLDER sample version shows it
/// VIEW-ONLY (a `viewOnlyNote`, no Save path — older versions have no editor). Resolved through the
/// live corpus at render time, so a removed sample's panel vanishes with its row. The retired bay's
/// View verb is subsumed by selection.
let private viewPanel (m : Model) (dispatch : Msg -> unit) : IView list =
    match selectedEntry m with
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
            | Some sample -> sample.name, sample.description, $" (version {shownVersion.value} — view-only)"
            | None ->
                let protectionNote =
                    match entry.protection with
                    | ProtectedBuiltIn -> " — protected built-in"
                    | UserManaged -> ""
                entry.displayName, entry.fullDescription, protectionNote
        let viewOnlyNote : IView list =
            if viewingOlder then
                [ TextBlock.create [
                      automationId<TextBlock> UiIds.LibraryWindow.viewOnlyNote
                      TextBlock.foreground (brush idleBorder)
                      TextBlock.textWrapping TextWrapping.Wrap
                      TextBlock.maxWidth 380.0
                      TextBlock.text $"Viewing version %d{shownVersion.value} (view-only) — the library edits the latest, version %d{latestVersion.value}."
                  ] :> IView ]
            else []
        [ Border.create [
              automationId<Border> UiIds.LibraryWindow.viewPanel
              Border.child (
                  StackPanel.create [
                      StackPanel.orientation Orientation.Vertical
                      StackPanel.spacing 2.0
                      StackPanel.children (
                          [ TextBlock.create [
                                TextBlock.fontWeight FontWeight.SemiBold
                                TextBlock.textWrapping TextWrapping.Wrap
                                TextBlock.maxWidth 380.0
                                TextBlock.text $"%s{displayName} — %s{(entryKindKey entry).value}%s{note}"
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
let view (m : Model) (dispatch : Msg -> unit) : IView =
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
                                    @ confirmRow m dispatch
                                    @ lifecycleConfirmRow m dispatch
                                    @ messageRow m
                                    @ viewPanel m dispatch)
                            ])
                    ])
            ]
            Border.create [
                automationId<Border> UiIds.LibraryWindow.treeHost
                Border.padding (Thickness 8.0)
                Border.child (FacetedTreeControls.view (facetedState m) (facetedHandlers dispatch))
            ]
        ]
    ] :> IView
