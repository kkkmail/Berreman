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

/// Whether the user has explicitly materialized a tree the result count gated behind the
/// Show/Search button (spec 0038 §0.7). Sticky for the window's lifetime: once shown, later
/// constraint changes keep the tree materialized — the gate protects the FIRST heavy render.
type TreeBuildRequest =
    | RequestedTreeBuild
    | NoTreeBuildRequest

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
        /// The selected entry's `entryId` (the Edit / Remove verbs' target and the view panel's
        /// subject). The entry-id STRING is the Library domain's uniform entry-identity seam
        /// (`LibraryEntry.entryId` / `tryGetEntry` — a sample's elevated `SampleId` crosses it
        /// as its Guid string form; the workbench `pendingEntry` precedent).
        selectedEntryId : string option
        /// Which sample remove (if any) awaits its inline confirmation.
        removeGate : LibraryRemoveGate
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
        selectedEntryId = None
        removeGate = NoPendingRemove
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
    /// Select the entry leaf with this `entryId` (the view panel's subject).
    | SelectEntry of string
    /// The verbs, rewired from the retired Library bay (spec 0038 step 015).
    | AddSample
    | MakeMultilayer
    | EditSelected
    | RequestRemoveSelected
    | ConfirmRemove
    | CancelRemove
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
let liveEntries (context : LibraryWindowContext) : LibraryEntry list =
    let samples =
        match context.samples.listSamples () with
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
let constrainedEntries (m : Model) : LibraryEntry list =
    let entries = liveEntries m.context
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
/// grouping. Only an `entry:` code means anything to `selectNode` — branches are grouping
/// display (the OFFERS are the apply surface).
let entryNodeCode (entryId : string) : string = "entry:" + entryId

let entryIdOfNodeCode (code : string) : string option =
    let prefix = "entry:"
    if code.StartsWith prefix then Some (code.Substring prefix.Length) else None

/// Stable intent-named automation ids (CLAUDE.md UI guidance). The faceted tree's own ids live
/// in `FacetedTreeControls.UiIds`; `entryNode` derives an entry leaf's id from its node code.
[<RequireQualifiedAccess>]
module UiIds =
    [<Literal>]
    let window = "LibraryWindow"
    [<Literal>]
    let treeHost = "LibraryFacetTreeHost"
    [<Literal>]
    let viewPanel = "LibraryViewPanel"
    [<Literal>]
    let addSampleButton = "LibraryAddSampleButton"
    [<Literal>]
    let makeMultilayerButton = "LibraryMakeMultilayerButton"
    [<Literal>]
    let editButton = "LibraryEditButton"
    [<Literal>]
    let removeButton = "LibraryRemoveButton"
    [<Literal>]
    let removeConfirmButton = "LibraryRemoveConfirmButton"
    [<Literal>]
    let removeCancelButton = "LibraryRemoveCancelButton"
    [<Literal>]
    let message = "LibraryWindowMessage"
    /// The Select-state pair and the fixed-constraint banner (spec 0038 step 016).
    [<Literal>]
    let selectButton = "LibrarySelectButton"
    [<Literal>]
    let selectCloseButton = "LibrarySelectCloseButton"
    [<Literal>]
    let selectConstraint = "LibrarySelectConstraint"
    /// The clickable tree leaf of one library entry, by its `entryId`.
    let entryNode (entryId : string) : string = FacetedTreeControls.UiIds.treeNode (entryNodeCode entryId)

// ---------------------------------------------------------------------------
// Pure update.
// ---------------------------------------------------------------------------

/// A query / selection change also disarms a pending remove confirmation and clears the inline
/// message — a stale confirm or refusal never outlives the state it referred to (the retired
/// bay's disarm discipline).
let private disarmed (m : Model) : Model =
    { m with removeGate = NoPendingRemove; lastError = None }

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
    | SelectEntry id ->
        { disarmed m with selectedEntryId = Some id }
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
                    selectedEntryId = (if m.selectedEntryId = Some (string id.value) then None else m.selectedEntryId) }
            | Error err ->
                // The store refused (a typed SampleError) — surface the reason inline and leave
                // the store, selection and list untouched.
                { m with removeGate = NoPendingRemove; lastError = Some (SampleRemoveRefused err) }
        | NoPendingRemove -> m
    | CancelRemove ->
        // Cancel only ever disarms — it never dismisses the inline message (the next query /
        // selection change clears it).
        { m with removeGate = NoPendingRemove }
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
        { disarmed m with mode = Select context; selectedEntryId = None }
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

/// Project the live corpus through the engine into the domain-free control state. Everything
/// here is recomputed per render over the CURRENT proxies (offers, counts, buckets, tree,
/// previews), so a verb's write — this window's or another's — shows in the same pass.
let facetedState (m : Model) : FacetedTreeControls.State =
    let inputs = projectionInputs m
    let filtered = Facets.filter inputs.defs inputs.appliedAll inputs.corpus
    let resultCount = List.length filtered
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
                    expansion = FacetedTreeControls.ExpandedNode
                    children =
                        filtered
                        |> List.map (fun entry ->
                            ({
                                code = entryNodeCode entry.entryId
                                label = entry.displayName
                                countOpt = None
                                expansion = FacetedTreeControls.ExpandedNode
                                children = []
                             } : FacetedTreeControls.TreeNode))
                }
            let engineTree = Facets.buildTree (Representation m.representation.order) inputs.defs inputs.appliedAll inputs.corpus
            let facetNodes =
                engineTree.facets
                |> List.map (fun facet ->
                    let branches : FacetedTreeControls.TreeNode list =
                        match inputs.defs |> List.tryFind (fun d -> d.key = facet.key) with
                        | Some def when def.kind = NumericAttribute ->
                            bucketsOf def
                            |> List.map (fun bucket ->
                                ({
                                    code = "branch:" + facet.key.value + ":" + numericRangeCode bucket.range
                                    label = numericRangeText bucket.range
                                    countOpt = Some bucket.count.value
                                    expansion = FacetedTreeControls.ExpandedNode
                                    children = []
                                 } : FacetedTreeControls.TreeNode))
                        | Some _ | None ->
                            facet.branches
                            |> List.map (fun branch ->
                                ({
                                    code = "branch:" + facet.key.value + ":" + branch.value.label
                                    label = branch.label
                                    countOpt = Some branch.count.value
                                    expansion = FacetedTreeControls.ExpandedNode
                                    children = []
                                 } : FacetedTreeControls.TreeNode))
                    ({
                        code = "facet:" + facet.key.value
                        label = facet.name
                        countOpt = None
                        expansion = FacetedTreeControls.ExpandedNode
                        children = branches
                     } : FacetedTreeControls.TreeNode))
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
                | Some id -> dispatch (SelectEntry id)
                | None -> ()
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
                  actionButton UiIds.selectButton "Select" positiveBackground (fun () -> dispatch ConfirmSelect)
                  actionButton UiIds.selectCloseButton "Close" negativeBackground (fun () -> dispatch CancelSelect)
              ]
          ] :> IView
          (TextBlock.create [
              automationId<TextBlock> UiIds.selectConstraint
              TextBlock.fontWeight FontWeight.SemiBold
              TextBlock.textWrapping TextWrapping.Wrap
              TextBlock.maxWidth 380.0
              TextBlock.text $"Choose a %s{Catalogue.kindName context.kindConstraint.value} for %s{targetText context.target} — the kind constraint is fixed and cannot be removed."
          ]
          |> Avalonia.FuncUI.DSL.View.withKey UiIds.selectConstraint) :> IView ]

/// The verbs row: Add sample and Make multilayer always; Edit only for a SAMPLE selection (a
/// preset has no editor — the verb is removed, not greyed); Remove while an entry is selected
/// (a protected entry's Remove REFUSES with the typed reason instead of arming the gate).
let private verbsRow (m : Model) (dispatch : Msg -> unit) : IView =
    WrapPanel.create [
        WrapPanel.orientation Orientation.Horizontal
        WrapPanel.children (
            [
                verbButton UiIds.addSampleButton "Add sample" (fun () -> dispatch AddSample)
                verbButton UiIds.makeMultilayerButton "Make multilayer" (fun () -> dispatch MakeMultilayer)
            ]
            @ (match editableSample m with
               | Some _ -> [ verbButton UiIds.editButton "Edit" (fun () -> dispatch EditSelected) ]
               | None -> [])
            @ (match selectedEntry m with
               | Some _ -> [ verbButton UiIds.removeButton "Remove" (fun () -> dispatch RequestRemoveSelected) ]
               | None -> []))
    ] :> IView

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
                  verbButton UiIds.removeConfirmButton "Remove" (fun () -> dispatch ConfirmRemove)
                  verbButton UiIds.removeCancelButton "Cancel" (fun () -> dispatch CancelRemove)
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
            | SampleRemoveRefused sampleError ->
                match sampleError with
                | UnknownSampleId reason
                | DuplicateSampleId reason
                | InvalidSample reason -> reason
        [ TextBlock.create [
              automationId<TextBlock> UiIds.message
              TextBlock.foreground (brush messageColor)
              TextBlock.textWrapping TextWrapping.Wrap
              TextBlock.maxWidth 380.0
              TextBlock.text text
          ] :> IView ]

/// The view panel: the selected entry's kind, protection state and FULL description (the
/// Library confirm-step / Details prose — `fullDescription` spells out what the entry IS).
/// Resolved through the live corpus at render time, so a removed sample's panel vanishes with
/// its row. The retired bay's View verb is subsumed by selection.
let private viewPanel (m : Model) : IView list =
    match selectedEntry m with
    | None -> []
    | Some entry ->
        let protectionNote =
            match entry.protection with
            | ProtectedBuiltIn -> " — protected built-in"
            | UserManaged -> ""
        [ Border.create [
              automationId<Border> UiIds.viewPanel
              Border.child (
                  StackPanel.create [
                      StackPanel.orientation Orientation.Vertical
                      StackPanel.spacing 2.0
                      StackPanel.children [
                          TextBlock.create [
                              TextBlock.fontWeight FontWeight.SemiBold
                              TextBlock.textWrapping TextWrapping.Wrap
                              TextBlock.maxWidth 380.0
                              TextBlock.text $"%s{entry.displayName} — %s{(entryKindKey entry).value}%s{protectionNote}"
                          ]
                          TextBlock.create [
                              TextBlock.textWrapping TextWrapping.Wrap
                              TextBlock.maxWidth 380.0
                              TextBlock.text entry.fullDescription
                          ]
                      ]
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
                                    @ [ verbsRow m dispatch ]
                                    @ confirmRow m dispatch
                                    @ messageRow m
                                    @ viewPanel m)
                            ])
                    ])
            ]
            Border.create [
                automationId<Border> UiIds.treeHost
                Border.padding (Thickness 8.0)
                Border.child (FacetedTreeControls.view (facetedState m) (facetedHandlers dispatch))
            ]
        ]
    ] :> IView
