/// Spec 0038 Part E (step 013) — the Materials window view (UICOMP_XDUO_0009): the pure MVU
/// model and FuncUI projection behind `MaterialsWindow`, the single-instance Materials window.
/// The model projects the LIVE `MaterialProxy` corpus through the step-011 material facet
/// catalogue (`LibraryFacets.materialFacets`) and the step-009 engine (`Facets.filter` /
/// `countFor` / `breadcrumbCounts` / `buildTree`) into the step-012 domain-free
/// `FacetedTreeControls.State`, beside a view panel (the selected entry's read-only metadata
/// plus the shared embeddable dual-axis n/k chart, `OpticalConstructor.Controls.EmbeddedChart`)
/// and the workbench's former Materials verbs — Add / Edit / Remove / Categories… — rewired to
/// THIS window: Add / Edit / Categories… open through the injected context launchers (the
/// composition root routes them through the step-008 `WindowLauncher` editor keys), Remove
/// keeps its inline confirm gate and the typed `MaterialStillReferenced` block. Every
/// projection re-queries the proxies, so any verb's write shows in the same render pass.
/// Step 016 adds the Browse/Select mode (the SAME `WindowMode.LibraryWindowMode` DU the
/// Library window uses, over `MaterialEntry`): Select adds exactly the Select/Close pair and
/// the fixed-constraint banner — the material corpus ALREADY satisfies a sample-layer pick
/// structurally (every material is layer-eligible; materials carry no `CatalogueKind`), so
/// the pre-applied kind constraint narrows nothing here and shows as the non-removable banner
/// only; everything else IS the ordinary window, so add-on-the-fly works. Pure: `update` only
/// reaches IO through the context's proxy / launcher / close fields — behaviour is testable
/// without a window (tests substitute recording stubs).
module OpticalConstructor.Ui.MaterialsWindowView

open Avalonia
open Avalonia.Automation
open Avalonia.Controls
open Avalonia.Layout
open Avalonia.Media
open Avalonia.FuncUI.Builder
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Units
open OpticalConstructor.Domain.Facets
open OpticalConstructor.Domain.LibraryFacets
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.Lifecycle
open OpticalConstructor.Domain.WindowMode
open OpticalConstructor.Domain.WorkbenchSettings
open OpticalConstructor.Controls

// ---------------------------------------------------------------------------
// Elevated window-local states (named DUs, never bools).
// ---------------------------------------------------------------------------

/// Which remove (if any) awaits its inline confirmation. The pending case CARRIES the id the
/// Remove click targeted, so a selection change between Remove and Confirm can never delete a
/// different entry (the workbench `RemoveConfirm` confirm-gating shape, window-local).
type MaterialRemoveGate =
    | NoPendingRemove
    | PendingRemove of MaterialId

/// Which lifecycle transition a confirm gate targets (spec 0038 step 023): a named DU — never a
/// bare string — so the confirm prose and the proxy call both read from ONE value. `Supersede`
/// and `MarkInactive` both retire the latest version (they share `InactiveEntry` in the store —
/// steps 021/022), but stay DISTINCT actions so the verb the user pressed is what the confirm
/// prompt names and a future step can diverge them (e.g. make supersede irreversible).
type LifecycleAction =
    | MarkInactiveAction
    | MarkActiveAction
    | SupersedeAction

/// Which lifecycle verb (if any) awaits its inline confirmation, carrying the id the verb click
/// targeted (the `MaterialRemoveGate` confirm-gating shape). Parallel to `removeGate`, never armed
/// at the same time (arming either disarms the other), so the step-013 remove tests stay intact.
type MaterialLifecycleGate =
    | NoPendingLifecycle
    | PendingLifecycle of MaterialId * LifecycleAction

/// Whether the user has explicitly materialized a tree the result count gated behind the
/// Show/Search button (spec 0038 §0.7). Sticky for the window's lifetime: once shown, later
/// constraint changes keep the tree materialized — the gate protects the FIRST heavy render.
type TreeBuildRequest =
    | RequestedTreeBuild
    | NoTreeBuildRequest

/// One named tree-shaping representation the window offers (spec 0038 §D.0: search order ≠
/// representation order): a stable `code` the control dispatches back, the picker label, and
/// the facet order handed to the engine's `buildTree`.
type MaterialsRepresentation =
    {
        code : string
        label : string
        order : AttributeKey list
    }

/// The two representations offered (the step-011 catalogue order first — the default — and a
/// physics-first reshuffle). Applied constraints never reshuffle either.
let byCategoryRepresentation : MaterialsRepresentation =
    {
        code = "by-category"
        label = "By category"
        order =
            [
                materialCategoryKey
                materialDispersionKey
                materialTransparencyKey
                materialAnisotropyKey
                materialDispersionModelKey
                materialGyrationClassKey
                materialHandednessKey
                materialMagneticKey
            ]
    }

let byPhysicsRepresentation : MaterialsRepresentation =
    {
        code = "by-physics"
        label = "By physics"
        order =
            [
                materialAnisotropyKey
                materialDispersionKey
                materialDispersionModelKey
                materialTransparencyKey
                materialGyrationClassKey
                materialHandednessKey
                materialMagneticKey
                materialCategoryKey
            ]
    }

let offeredRepresentations : MaterialsRepresentation list =
    [ byCategoryRepresentation; byPhysicsRepresentation ]

// ---------------------------------------------------------------------------
// The window's IO seam (the functional-proxy Context convention).
// ---------------------------------------------------------------------------

/// The window's context: the app-scope material + category stores, the Domain tree-gating
/// threshold, and the editor launchers (the composition root — `MaterialsWindow` — bakes the
/// step-008 `WindowLauncher` editor keys into these; tests substitute recording stubs).
/// Function-valued fields have no structural equality, so the record compares by reference.
[<ReferenceEquality>]
type MaterialsWindowContext =
    {
        materials : MaterialProxy
        categories : CategoryProxy
        treeAutoBuildThreshold : TreeAutoBuildThreshold
        /// Open the Material editor by intent: `NewMaterial mintedId` (the Add verb mints the
        /// id AT dispatch — spec 0038 step 008) or `EditMaterial entry`.
        openMaterialEditor : MaterialEditorView.MaterialEditorIntent -> unit
        /// Open the single-instance Category editor.
        openCategoryEditor : unit -> unit
        /// Close THIS window (the host passes `this.Close`; tests substitute a recording stub —
        /// the `SampleEditorContext` precedent). The Select-state verbs reach it: 'Select'
        /// closes after `onSelected`, 'Close' after `onCancelled` (spec 0038 step 016).
        requestClose : unit -> unit
    }

/// The window's pure model. The engine inputs (corpus, live facet defs) are NOT cached here —
/// every projection re-queries the context's proxies, so a verb's write (or another window's)
/// shows in the same render pass.
type Model =
    {
        context : MaterialsWindowContext
        /// Browse (the ordinary window) or Select (spec 0038 step 016): the SAME window with
        /// the Select/Close pair and the fixed-constraint banner. The material corpus already
        /// satisfies the session's kind constraint structurally (see the module doc), so the
        /// mode changes NO projection — only the Select surface and the return path.
        mode : LibraryWindowMode<MaterialEntry>
        /// The applied facet constraints, in application (breadcrumb) order — at most one per
        /// facet key (this window applies single-key selections; the engine's key-set OR stays
        /// available to a later slice).
        appliedFacets : AppliedConstraint list
        /// The committed text filter (the box's draft echoes it; Enter/LostFocus commit only).
        textFilter : TextQuery
        /// The active tree-shaping representation.
        representation : MaterialsRepresentation
        /// Whether the user has explicitly materialized a gated tree.
        buildRequest : TreeBuildRequest
        /// The selected entry (the Edit / Remove / lifecycle verbs' target and the view panel's
        /// subject).
        selectedId : MaterialId option
        /// Whether the show-inactive/superseded toggle is on (spec 0038 step 023): `ActiveOnly`
        /// (the default — pickers/facet counts exclude retired entries) or `IncludeInactive` (the
        /// toggle adds them to the tree, badged). The Domain DU, never a bool. Select mode ignores
        /// this and always lists `ActiveOnly` (a retired entry is never a valid pick target).
        showInactive : InactiveVisibility
        /// Which remove (if any) awaits its inline confirmation.
        removeGate : MaterialRemoveGate
        /// Which lifecycle verb (if any) awaits its inline confirmation.
        lifecycleGate : MaterialLifecycleGate
        /// Which OLDER version of the selected entry the view panel is showing read-only
        /// (`None` = the latest, the editable default; `Some v` = version `v` view-only — spec
        /// 0038 step 023). Reset to `None` on every selection change.
        viewedVersion : VersionNumber option
        /// The last store refusal, surfaced as the inline message — `MaterialStillReferenced`
        /// NAMES the referencing samples; never a cascade, never a dialog.
        lastError : MaterialError option
    }

let init (context : MaterialsWindowContext) (mode : LibraryWindowMode<MaterialEntry>) : Model =
    {
        context = context
        mode = mode
        appliedFacets = []
        textFilter = TextQuery ""
        representation = byCategoryRepresentation
        buildRequest = NoTreeBuildRequest
        selectedId = None
        showInactive = ActiveOnly
        removeGate = NoPendingRemove
        lifecycleGate = NoPendingLifecycle
        viewedVersion = None
        lastError = None
    }

type Msg =
    /// Apply the offered value as this facet's (single-key) constraint — an offered-value
    /// click; re-applying a constrained facet REPLACES its selection.
    | ApplyFacetValue of AttributeKey * DiscreteKey
    /// Remove the applied constraint of this facet — a breadcrumb-chip click.
    | RemoveFacet of AttributeKey
    /// Commit the filter box's text (Enter/LostFocus only — never per keystroke).
    | CommitTextFilter of string
    /// Choose the named representation with this code (tree reshaping only — the applied
    /// constraints and the result set are untouched: search order ≠ representation order).
    | ChooseRepresentation of string
    /// Materialize the gated tree (the Show/Search button).
    | RequestTreeBuild
    /// Select the entry leaf with this id (the view panel's subject).
    | SelectEntry of MaterialId
    /// The verbs, rewired from the retired Materials bay (spec 0038 step 013).
    | AddMaterial
    | EditSelected
    | RequestRemoveSelected
    | ConfirmRemove
    | CancelRemove
    | OpenCategories
    /// The show-inactive/superseded toggle (spec 0038 step 023): flip the corpus scope between
    /// `ActiveOnly` and `IncludeInactive`.
    | ToggleShowInactive
    /// The lifecycle verbs, confirm-gated inline (spec 0038 step 023). `Request…` arms the gate on
    /// the selected entry; `ConfirmLifecycle` runs the matching proxy verb (a refusal surfaces as
    /// the typed inline message); `CancelLifecycle` disarms.
    | RequestMarkInactive
    | RequestMarkActive
    | RequestSupersede
    | ConfirmLifecycle
    | CancelLifecycle
    /// Show an OLDER version of the selected entry read-only in the view panel (spec 0038 step 023).
    | ViewVersion of VersionNumber
    /// The Select-state pair (spec 0038 step 016). 'Select' returns the HIGHLIGHTED entry
    /// through the session's `onSelected` (a targeted dispatch), then closes; no highlight →
    /// inert. 'Close' fires `onCancelled`, then closes.
    | ConfirmSelect
    | CancelSelect
    /// A Select-state open met this LIVE window: re-point the session at the new context
    /// (the superseded session is cancelled — a second Choose closes the first, logically)
    /// and clear the highlight; the window itself stays (the launcher's re-target seam).
    | RetargetSelect of SelectionContext<MaterialEntry>
    /// The host window CLOSED (the title-bar X, or a staleness `Close()` from the requesting
    /// surface): cancel a still-pending session exactly once — a session already resolved by
    /// Select/Close flipped the mode to Browse first, so this can never double-fire.
    | SelectDismissed

// ---------------------------------------------------------------------------
// Live projections (each pass re-queries the proxies).
// ---------------------------------------------------------------------------

/// Resolve a category's display NAME through the LIVE catalogue: the store's `listCategories`
/// is the source of truth, so a renamed category reads its new name here (the retired bay's
/// `liveCategoryName` discipline). An id the store no longer holds falls back to the seeded
/// `categoryName` (a diagnostic, never a throw).
let liveCategoryName (categories : CategoryProxy) (id : CategoryId) : string =
    match categories.listCategories () with
    | Ok cats ->
        match cats |> List.tryFind (fun c -> c.id = id) with
        | Some c -> c.name
        | None -> categoryName id
    | Error _ -> categoryName id

/// The step-011 material facet catalogue with the category def's extractor re-pointed at the
/// LIVE category store: `materialFacets`' own category def resolves names through the STATIC
/// seeded catalogue (`categoryName`), so a category renamed through `CategoryProxy` would keep
/// its stale branch label there. The facet KEY is unchanged — only name resolution goes live —
/// and the catalogue is read ONCE per projection (the discrete value key doubles as the branch
/// label, step 009, so a rename re-labels branch, offer, and chip alike).
let liveMaterialFacets (categories : CategoryProxy) : AttributeDef<MaterialEntry> list =
    let liveNames : Map<CategoryId, string> =
        match categories.listCategories () with
        | Ok cats -> cats |> List.map (fun c -> c.id, c.name) |> Map.ofList
        | Error _ -> Map.empty
    let liveName (id : CategoryId) : string =
        match Map.tryFind id liveNames with
        | Some name -> name
        | None -> categoryName id
    materialFacets
    |> List.map (fun def ->
        if def.key = materialCategoryKey
        then { def with extract = fun entry -> [ DiscreteValue (DiscreteKey (liveName entry.category)) ] }
        else def)

/// The text filter's facet key (the filter is an ORDINARY engine constraint — spec 0038 §D.0 —
/// over the entry's display name; its UI surface stays the filter box, not a chip).
let materialTextFilterKey : AttributeKey = AttributeKey "material-text"

/// One projection pass's engine inputs, read from the live proxies exactly once.
type private ProjectionInputs =
    {
        defs : AttributeDef<MaterialEntry> list
        /// The committed text filter as an applied constraint — empty query applies nothing.
        textApplied : AppliedConstraint list
        /// The FULL applied list: the text filter (first) then the facet chips in order.
        appliedAll : AppliedConstraint list
        corpus : MaterialEntry list
    }

/// The scope the corpus projection lists at (spec 0038 step 023): `ActiveOnly` in Select mode (a
/// retired entry is never a valid pick target — Select ALWAYS excludes inactive/superseded), else
/// the window's `showInactive` toggle. The default `ActiveOnly` keeps offers, facet counts and
/// Select byte-for-byte the step-021 behaviour.
let effectiveScope (m : Model) : InactiveVisibility =
    match m.mode with
    | Select _ -> ActiveOnly
    | Browse -> m.showInactive

let private projectionInputs (m : Model) : ProjectionInputs =
    let textDef = textFilterDef materialTextFilterKey "Text" (fun (e : MaterialEntry) -> e.name) m.textFilter
    let defs = textDef :: liveMaterialFacets m.context.categories
    let textApplied =
        match m.textFilter.value with
        | "" -> []
        | _ -> [ textFilterConstraint materialTextFilterKey ]
    let corpus =
        // Latest version of each material at the effective scope — `ActiveOnly` by default (offers
        // / facets / Select exclude retired entries, spec 0038 step 021), `IncludeInactive` when
        // the show-inactive toggle is on (spec 0038 step 023).
        match m.context.materials.listMaterials (effectiveScope m) with
        | Ok entries -> entries
        | Error _ -> []
    {
        defs = defs
        textApplied = textApplied
        appliedAll = textApplied @ m.appliedFacets
        corpus = corpus
    }

/// The result set under everything applied (text filter AND facet constraints) — public so
/// narrowing is unit-testable without a window.
let filteredEntries (m : Model) : MaterialEntry list =
    let inputs = projectionInputs m
    Facets.filter inputs.defs inputs.appliedAll inputs.corpus

/// The selected entry resolved through the LIVE store (a removed entry's selection resolves to
/// nothing, so its panel vanishes with its row).
let selectedEntry (m : Model) : MaterialEntry option =
    match m.selectedId with
    | Some id ->
        match m.context.materials.tryGetMaterial id with
        | Ok (Some entry) -> Some entry
        | Ok None | Error _ -> None
    | None -> None

/// The selected entry when it carries the step-013 edit model (`complexity = Some`) — the Edit
/// verb's target. A view-only engine preset offers NO Edit affordance (removed, not greyed —
/// the `MaterialsControls` discipline).
let editableSelection (m : Model) : MaterialEntry option =
    match selectedEntry m with
    | Some entry ->
        match entry.complexity with
        | Some _ -> Some entry
        | None -> None
    | None -> None

// ---------------------------------------------------------------------------
// Lifecycle (spec 0038 step 023): the active-id set, the selected entry's
// live/retired state, the offered verbs, and the version enumeration.
// ---------------------------------------------------------------------------

/// The ids whose LATEST version is active (the `ActiveOnly` listing — read live every pass). A
/// material present in `IncludeInactive` but NOT here has a retired latest version; that is how the
/// window distinguishes an inactive entry to badge it and to offer Mark active.
let private activeMaterialIds (m : Model) : Set<MaterialId> =
    match m.context.materials.listMaterials ActiveOnly with
    | Ok entries -> entries |> List.map (fun e -> e.id) |> Set.ofList
    | Error _ -> Set.empty

/// The selected entry's lifecycle (spec 0038 step 023): `ActiveEntry` when its latest version is
/// in the active set, `InactiveEntry` when it resolves but its latest is retired, `None` when no
/// entry is selected or the id no longer resolves.
let selectedLifecycle (m : Model) : EntryLifecycle option =
    match m.selectedId with
    | None -> None
    | Some id ->
        match m.context.materials.tryGetMaterial id with
        | Ok (Some _) ->
            if Set.contains id (activeMaterialIds m) then Some ActiveEntry else Some InactiveEntry
        | Ok None | Error _ -> None

/// The lifecycle verbs offered for the current selection (spec 0038 step 023) — the ONE source of
/// truth the verbs row renders from and the tests assert against. An active selection offers
/// Mark inactive + Supersede…; a retired one offers Mark active; a bare Browse-mode selection with
/// no resolvable entry — or any Select-mode selection — offers none. Materials carry no protection
/// (`MaterialEntry` has no `EntryProtection`; the store has no built-in guard), so EVERY material
/// is lifecycle-eligible here (unlike the Library window's protected presets).
let offeredLifecycleActions (m : Model) : LifecycleAction list =
    match m.mode with
    | Select _ -> []
    | Browse ->
        match selectedLifecycle m with
        | Some ActiveEntry -> [ MarkInactiveAction; SupersedeAction ]
        | Some InactiveEntry -> [ MarkActiveAction ]
        | None -> []

/// The number of materials whose latest version is retired (spec 0038 step 023) — the visible
/// count badge on the show-inactive toggle. Read over the whole store (not the filtered corpus) so
/// the badge is a stable "N retired exist" hint whether the toggle is on or off.
let inactiveCount (m : Model) : int =
    let active = activeMaterialIds m
    match m.context.materials.listMaterials IncludeInactive with
    | Ok all -> all |> List.filter (fun e -> not (Set.contains e.id active)) |> List.length
    | Error _ -> 0

/// Whether an entry (its latest version) is retired, for the tree-leaf badge.
let private isEntryInactive (activeIds : Set<MaterialId>) (entry : MaterialEntry) : bool =
    not (Set.contains entry.id activeIds)

/// The badge appended to a retired entry's tree-leaf label when the show-inactive toggle reveals
/// it (spec 0038 step 023). Supersede and mark-inactive share `InactiveEntry` in the store, so the
/// badge reads "inactive" for both.
let inactiveBadge : string = " — inactive"

/// The stored versions of a material, ascending (latest last), enumerated by probing the proxy's
/// by-version `resolveVersion` from version 1 upward until it resolves nothing (spec 0038 step 023
/// — the store exposes no list-versions field). In the live in-memory store this is a single
/// version until step 25 (mints need a used version, and `VersionsInUse` is empty); a stub proxy
/// returns a longer history. The `1000` cap is an unreachable runaway guard, never hit in practice.
let selectedVersionsOf (materials : MaterialProxy) (id : MaterialId) : (VersionNumber * MaterialEntry) list =
    let rec loop (version : VersionNumber) (acc : (VersionNumber * MaterialEntry) list) : (VersionNumber * MaterialEntry) list =
        if version.value > 1000 then List.rev acc
        else
            match materials.resolveVersion { materialId = id; version = version } with
            | Ok (Some entry) -> loop version.next ((version, entry) :: acc)
            | Ok None | Error _ -> List.rev acc
    loop VersionNumber.first []

/// The selected entry's versions (empty when nothing is selected or the id no longer resolves).
let selectedVersions (m : Model) : (VersionNumber * MaterialEntry) list =
    match m.selectedId with
    | Some id -> selectedVersionsOf m.context.materials id
    | None -> []

// ---------------------------------------------------------------------------
// Tree node codes (host-supplied stable tokens, unique across the whole tree).
// ---------------------------------------------------------------------------

/// `entries` / `entry:<guid>` name the corpus group and its selectable leaves;
/// `facet:<facet-key>` / `branch:<facet-key>:<value-key>` name the representation's facet
/// grouping. Only an `entry:` code means anything to `selectNode` — branches are grouping
/// display (the OFFERS are the apply surface).
let entryNodeCode (id : MaterialId) : string = "entry:" + string id.value

let entryIdOfNodeCode (code : string) : MaterialId option =
    let prefix = "entry:"
    if code.StartsWith prefix then
        match System.Guid.TryParse (code.Substring prefix.Length) with
        | true, g -> Some (MaterialId g)
        | _ -> None
    else None

/// Stable intent-named automation ids (CLAUDE.md UI guidance). The faceted tree's own ids live
/// in `FacetedTreeControls.UiIds`; `entryNode` derives an entry leaf's id from its node code.
[<RequireQualifiedAccess>]
module UiIds =
    [<Literal>]
    let window = "MaterialsWindow"
    [<Literal>]
    let treeHost = "MaterialsFacetTreeHost"
    [<Literal>]
    let viewPanel = "MaterialsViewPanel"
    [<Literal>]
    let viewPanelChart = "MaterialsViewPanelNkChart"
    [<Literal>]
    let addButton = "MaterialsAddButton"
    [<Literal>]
    let editButton = "MaterialsEditButton"
    [<Literal>]
    let removeButton = "MaterialsRemoveButton"
    [<Literal>]
    let categoriesButton = "MaterialsCategoriesButton"
    [<Literal>]
    let removeConfirmButton = "MaterialsRemoveConfirmButton"
    [<Literal>]
    let removeCancelButton = "MaterialsRemoveCancelButton"
    [<Literal>]
    let message = "MaterialsWindowMessage"
    /// The lifecycle surface (spec 0038 step 023): the show-inactive toggle, the three verbs, the
    /// lifecycle confirm pair, the versions panel and its per-version rows, and the view-only note.
    [<Literal>]
    let showInactiveToggle = "MaterialsShowInactiveToggle"
    [<Literal>]
    let markInactiveButton = "MaterialsMarkInactiveButton"
    [<Literal>]
    let markActiveButton = "MaterialsMarkActiveButton"
    [<Literal>]
    let supersedeButton = "MaterialsSupersedeButton"
    [<Literal>]
    let lifecycleConfirmButton = "MaterialsLifecycleConfirmButton"
    [<Literal>]
    let lifecycleCancelButton = "MaterialsLifecycleCancelButton"
    [<Literal>]
    let versionsPanel = "MaterialsVersionsPanel"
    [<Literal>]
    let viewOnlyNote = "MaterialsViewOnlyNote"
    /// A clickable version row in the view panel's version list, by its version number.
    let versionRow (version : VersionNumber) : string = "MaterialsVersionRow_" + string version.value
    /// The Select-state pair and the fixed-constraint banner (spec 0038 step 016).
    [<Literal>]
    let selectButton = "MaterialsSelectButton"
    [<Literal>]
    let selectCloseButton = "MaterialsSelectCloseButton"
    [<Literal>]
    let selectConstraint = "MaterialsSelectConstraint"
    /// The clickable tree leaf of one material entry.
    let entryNode (id : MaterialId) : string = FacetedTreeControls.UiIds.treeNode (entryNodeCode id)

// ---------------------------------------------------------------------------
// Pure update.
// ---------------------------------------------------------------------------

/// A query / selection change also disarms a pending remove confirmation and clears the inline
/// message — a stale confirm or refusal never outlives the state it referred to (the retired
/// bay's disarm discipline).
let private disarmed (m : Model) : Model =
    { m with removeGate = NoPendingRemove; lifecycleGate = NoPendingLifecycle; lastError = None }

/// Arm the lifecycle confirm gate on the current selection (spec 0038 step 023) — clears the
/// remove gate and the inline message so at most one confirm is ever pending. No selection is inert.
let private requestLifecycle (action : LifecycleAction) (m : Model) : Model =
    match m.selectedId with
    | Some id -> { m with lifecycleGate = PendingLifecycle (id, action); removeGate = NoPendingRemove; lastError = None }
    | None -> m

let update (msg : Msg) (m : Model) : Model =
    match msg with
    | ApplyFacetValue (key, value) ->
        // Single-key selection per facet: re-applying a constrained facet REPLACES its chip
        // (removed from its old breadcrumb position, re-applied at the end).
        let withoutKey = m.appliedFacets |> List.filter (fun c -> c.key <> key)
        { disarmed m with appliedFacets = withoutKey @ [ { key = key; selection = DiscreteSelection (Set.singleton value) } ] }
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
        // A new selection shows its LATEST version (the editable default) and disarms both gates.
        { disarmed m with selectedId = Some id; viewedVersion = None }
    | AddMaterial ->
        // Add mints the entry's MaterialId HERE — at the window-open dispatch, off the save
        // path (spec 0038 step 008) — so the launcher's registry keys the new editor by the
        // SAME id its Save will persist under (a second Add mints a second id: two windows).
        m.context.openMaterialEditor (MaterialEditorView.NewMaterial (newMaterialId ()))
        disarmed m
    | EditSelected ->
        // Edit opens the editor on the selected entry, resolved through the proxy at dispatch
        // time; a vanished or view-only selection reaches no launcher (the editor's own
        // view-only rule would apply anyway — the verb is simply absent for it).
        (match editableSelection m with
         | Some entry -> m.context.openMaterialEditor (MaterialEditorView.EditMaterial entry)
         | None -> ())
        disarmed m
    | RequestRemoveSelected ->
        match m.selectedId with
        | Some id -> { m with removeGate = PendingRemove id; lastError = None }
        | None -> m
    | ConfirmRemove ->
        match m.removeGate with
        | PendingRemove id ->
            match m.context.materials.removeMaterial id with
            | Ok () ->
                { m with
                    removeGate = NoPendingRemove
                    lastError = None
                    selectedId = (if m.selectedId = Some id then None else m.selectedId) }
            | Error err ->
                // The store refused (`MaterialStillReferenced` names the referencing samples) —
                // surface the reason inline and leave the store, selection and list untouched.
                { m with removeGate = NoPendingRemove; lastError = Some err }
        | NoPendingRemove -> m
    | CancelRemove ->
        // Cancel only ever disarms — it never dismisses the inline message (the next query /
        // selection change clears it).
        { m with removeGate = NoPendingRemove }
    | OpenCategories ->
        // A pure launch of the Category editor over the live category store; the window
        // re-queries `listCategories` on its next render, so a rename there re-labels the
        // category facet in the same render pass.
        m.context.openCategoryEditor ()
        m
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
                | MarkInactiveAction -> m.context.materials.markMaterialInactive id
                | MarkActiveAction -> m.context.materials.markMaterialActive id
                | SupersedeAction -> m.context.materials.supersedeMaterial id
            match outcome with
            | Ok () -> { m with lifecycleGate = NoPendingLifecycle; lastError = None }
            | Error err ->
                // The store refused (e.g. the entry vanished behind the window's back) — surface
                // the typed reason inline and leave the store and selection untouched.
                { m with lifecycleGate = NoPendingLifecycle; lastError = Some err }
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
        { disarmed m with mode = Select context; selectedId = None }
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

let private isApplicableTo (def : AttributeDef<MaterialEntry>) (entry : MaterialEntry) : bool =
    match def.appliesTo entry with
    | ApplicableAttribute -> true
    | InapplicableAttribute -> false

/// The distinct discrete values a facet offers over the filtered population, in the engine's
/// key order (every material facet is discrete — the catalogue carries no numeric facet, so no
/// manual min–max box is ever offered).
let private offeredValuesFor (def : AttributeDef<MaterialEntry>) (filtered : MaterialEntry list) : DiscreteKey list =
    filtered
    |> List.filter (isApplicableTo def)
    |> List.collect (fun entry -> def.extract entry |> List.distinct)
    |> List.choose (fun value ->
        match value with
        | DiscreteValue k -> Some k
        | NumericValue _ -> None)
    |> List.distinct
    |> List.sortBy (fun k -> k.value)

/// A chip's display label: the facet's name plus its selected value key(s).
let private chipLabel (defs : AttributeDef<MaterialEntry> list) (applied : AppliedConstraint) : string =
    let facetName =
        match defs |> List.tryFind (fun d -> d.key = applied.key) with
        | Some def -> def.name
        | None -> applied.key.value
    let valueText =
        match applied.selection with
        | DiscreteSelection keys -> keys |> Set.toList |> List.map (fun k -> k.value) |> String.concat ", "
        | NumericRangeSelection range -> $"%g{range.lower}–%g{range.upper}"
    $"%s{facetName}: %s{valueText}"

/// Project the live corpus through the engine into the domain-free control state. Everything
/// here is recomputed per render over the CURRENT proxies (offers, counts, tree, previews), so
/// a verb's write — or a category rename through the shared proxy — shows in the same pass.
let facetedState (m : Model) : FacetedTreeControls.State =
    let inputs = projectionInputs m
    let filtered = Facets.filter inputs.defs inputs.appliedAll inputs.corpus
    let resultCount = List.length filtered
    // The active-id set, for the retired-entry leaf badge (spec 0038 step 023): with the toggle
    // off the corpus is `ActiveOnly`, so no leaf ever badges; with it on, a retired latest version
    // reads `label — inactive`.
    let activeIds = activeMaterialIds m
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
    // Offers: every UNCONSTRAINED facet, in representation order, its values count-previewed
    // over everything already applied; a facet inapplicable to (or valueless over) the whole
    // filtered population vanishes entirely (the engine discipline).
    let appliedKeys = m.appliedFacets |> List.map (fun c -> c.key) |> Set.ofList
    let offers =
        m.representation.order
        |> List.filter (fun key -> not (Set.contains key appliedKeys))
        |> List.choose (fun key -> inputs.defs |> List.tryFind (fun d -> d.key = key))
        |> List.choose (fun def ->
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
    // facet/branch grouping from the engine's buildTree.
    let tree =
        match materialization with
        | FacetedTreeControls.TreeGated -> []
        | FacetedTreeControls.TreeMaterialized ->
            let entriesNode : FacetedTreeControls.TreeNode =
                {
                    code = "entries"
                    label = "Materials"
                    countOpt = Some resultCount
                    expansion = FacetedTreeControls.ExpandedNode
                    children =
                        filtered
                        |> List.map (fun entry ->
                            ({
                                code = entryNodeCode entry.id
                                label = (if isEntryInactive activeIds entry then entry.name + inactiveBadge else entry.name)
                                countOpt = None
                                expansion = FacetedTreeControls.ExpandedNode
                                children = []
                             } : FacetedTreeControls.TreeNode))
                }
            let engineTree = Facets.buildTree (Representation m.representation.order) inputs.defs inputs.appliedAll inputs.corpus
            let facetNodes =
                engineTree.facets
                |> List.map (fun facet ->
                    ({
                        code = "facet:" + facet.key.value
                        label = facet.name
                        countOpt = None
                        expansion = FacetedTreeControls.ExpandedNode
                        children =
                            facet.branches
                            |> List.map (fun branch ->
                                ({
                                    code = "branch:" + facet.key.value + ":" + branch.value.label
                                    label = branch.label
                                    countOpt = Some branch.count.value
                                    expansion = FacetedTreeControls.ExpandedNode
                                    children = []
                                 } : FacetedTreeControls.TreeNode))
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
/// display; the offers are the apply surface); `applyManualRange` is unreachable — the material
/// catalogue offers no numeric facet, so no manual range box is ever generated.
let facetedHandlers (dispatch : Msg -> unit) : FacetedTreeControls.Handlers =
    {
        applyConstraint = fun groupCode valueCode -> dispatch (ApplyFacetValue (AttributeKey groupCode, DiscreteKey valueCode))
        removeConstraint = fun chipCode -> dispatch (RemoveFacet (AttributeKey chipCode))
        commitTextFilter = fun text -> dispatch (CommitTextFilter text)
        chooseRepresentation = fun code -> dispatch (ChooseRepresentation code)
        requestBuild = fun () -> dispatch RequestTreeBuild
        selectNode =
            fun code ->
                match entryIdOfNodeCode code with
                | Some id -> dispatch (SelectEntry id)
                | None -> ()
        applyManualRange = fun _ _ -> ()
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
/// breadcrumb chip and offers no remove affordance (the material corpus satisfies it
/// structurally — see the module doc). Absent in Browse mode.
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
              TextBlock.text $"Choose a material for %s{targetText context.target} — the %s{Catalogue.kindName context.kindConstraint.value} kind constraint is fixed and cannot be removed."
          ]
          |> Avalonia.FuncUI.DSL.View.withKey UiIds.selectConstraint) :> IView ]

/// One lifecycle verb box (spec 0038 step 023), by its action.
let private lifecycleButton (dispatch : Msg -> unit) (action : LifecycleAction) : IView =
    match action with
    | MarkInactiveAction -> verbButton UiIds.markInactiveButton "Mark inactive" (fun () -> dispatch RequestMarkInactive)
    | MarkActiveAction -> verbButton UiIds.markActiveButton "Mark active" (fun () -> dispatch RequestMarkActive)
    | SupersedeAction -> verbButton UiIds.supersedeButton "Supersede…" (fun () -> dispatch RequestSupersede)

/// The verbs row: Add and Categories… always; Edit only for an EDITABLE selection (a view-only
/// engine preset loses the verb — removed, not greyed); Remove only while an entry is selected; the
/// lifecycle verbs from `offeredLifecycleActions` (Mark inactive + Supersede… for an active
/// selection, Mark active for a retired one — spec 0038 step 023).
let private verbsRow (m : Model) (dispatch : Msg -> unit) : IView =
    WrapPanel.create [
        WrapPanel.orientation Orientation.Horizontal
        WrapPanel.children (
            [ verbButton UiIds.addButton "Add" (fun () -> dispatch AddMaterial) ]
            @ (match editableSelection m with
               | Some _ -> [ verbButton UiIds.editButton "Edit" (fun () -> dispatch EditSelected) ]
               | None -> [])
            @ (match m.selectedId with
               | Some _ -> [ verbButton UiIds.removeButton "Remove" (fun () -> dispatch RequestRemoveSelected) ]
               | None -> [])
            @ (offeredLifecycleActions m |> List.map (lifecycleButton dispatch))
            @ [ verbButton UiIds.categoriesButton "Categories…" (fun () -> dispatch OpenCategories) ])
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
        [ verbButton UiIds.showInactiveToggle label (fun () -> dispatch ToggleShowInactive) ]

/// The inline remove confirmation — present only while a remove is armed.
let private confirmRow (m : Model) (dispatch : Msg -> unit) : IView list =
    match m.removeGate with
    | NoPendingRemove -> []
    | PendingRemove id ->
        let name =
            match m.context.materials.tryGetMaterial id with
            | Ok (Some entry) -> entry.name
            | Ok None | Error _ -> string id.value
        [ WrapPanel.create [
              WrapPanel.orientation Orientation.Horizontal
              WrapPanel.children [
                  TextBlock.create [
                      TextBlock.text $"Remove material '%s{name}'?"
                      TextBlock.verticalAlignment VerticalAlignment.Center
                      TextBlock.margin (Thickness(0.0, 0.0, 8.0, 4.0))
                  ]
                  verbButton UiIds.removeConfirmButton "Remove" (fun () -> dispatch ConfirmRemove)
                  verbButton UiIds.removeCancelButton "Cancel" (fun () -> dispatch CancelRemove)
              ]
          ] :> IView ]

/// The inline lifecycle confirmation (spec 0038 step 023) — present only while a lifecycle verb is
/// armed. The prompt NAMES the entry and the transition (the verb the user pressed); Confirm runs
/// the matching proxy verb, Cancel disarms.
let private lifecycleConfirmRow (m : Model) (dispatch : Msg -> unit) : IView list =
    match m.lifecycleGate with
    | NoPendingLifecycle -> []
    | PendingLifecycle (id, action) ->
        let name =
            match m.context.materials.tryGetMaterial id with
            | Ok (Some entry) -> entry.name
            | Ok None | Error _ -> string id.value
        let prompt =
            match action with
            | MarkInactiveAction -> $"Mark material '%s{name}' inactive?"
            | MarkActiveAction -> $"Mark material '%s{name}' active?"
            | SupersedeAction -> $"Supersede material '%s{name}'?"
        [ WrapPanel.create [
              WrapPanel.orientation Orientation.Horizontal
              WrapPanel.children [
                  TextBlock.create [
                      TextBlock.text prompt
                      TextBlock.verticalAlignment VerticalAlignment.Center
                      TextBlock.margin (Thickness(0.0, 0.0, 8.0, 4.0))
                  ]
                  verbButton UiIds.lifecycleConfirmButton "Confirm" (fun () -> dispatch ConfirmLifecycle)
                  verbButton UiIds.lifecycleCancelButton "Cancel" (fun () -> dispatch CancelLifecycle)
              ]
          ] :> IView ]

/// The inline store-refusal message. Every `MaterialError` case carries its diagnostic reason;
/// `MaterialStillReferenced`'s NAMES the referencing samples — never a cascade, never a dialog.
let private messageRow (m : Model) : IView list =
    match m.lastError with
    | None -> []
    | Some err ->
        let text =
            match err with
            | UnknownMaterialId reason
            | DuplicateMaterialId reason
            | MaterialStillReferenced reason
            | MaterialVersionInUse reason
            | InvalidMaterial reason -> reason
        [ TextBlock.create [
              automationId<TextBlock> UiIds.message
              TextBlock.foreground (brush messageColor)
              TextBlock.textWrapping TextWrapping.Wrap
              TextBlock.maxWidth 380.0
              TextBlock.text text
          ] :> IView ]

/// The version list (spec 0038 step 023): one clickable row per stored version, the latest marked
/// `(latest)` and the currently-viewed one marked `▸`. Clicking a row shows THAT version in the
/// panel — the latest editable through the Edit verb, an older one view-only. Absent when the
/// selection resolves to a single version (the common case in the live store until step 25).
let private versionsRow (m : Model) (versions : (VersionNumber * MaterialEntry) list) (latest : VersionNumber) (shown : VersionNumber) (dispatch : Msg -> unit) : IView list =
    match versions with
    | [] | [ _ ] -> []
    | _ ->
        [ StackPanel.create [
              automationId<StackPanel> UiIds.versionsPanel
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
                          verbButton (UiIds.versionRow version) $"%s{marker}v%d{version.value}%s{latestTag}" (fun () -> dispatch (ViewVersion version)))))
          ] :> IView ]

/// The view panel: the selected entry's read-only metadata plus the dual-axis n/k chart over the
/// editor's preview range, embedded through the ONE shared ScottPlot chart control, plus the
/// version list (spec 0038 step 023). By default it shows the LATEST version (the editable one —
/// the library always edits the latest); selecting an OLDER version shows it VIEW-ONLY (a
/// `viewOnlyNote`, no Save path — older versions have no editor). Resolved through the proxy at
/// render time, so a removed entry's panel vanishes with its row.
let private viewPanel (m : Model) (dispatch : Msg -> unit) : IView list =
    match selectedEntry m with
    | None -> []
    | Some latestEntry ->
        let versions = selectedVersionsOf m.context.materials latestEntry.id
        let latestVersion =
            match versions |> List.map fst with
            | [] -> VersionNumber.first
            | numbers -> List.max numbers
        let shownVersion =
            match m.viewedVersion with
            | Some v when versions |> List.exists (fun (vn, _) -> vn = v) -> v
            | _ -> latestVersion
        let shownEntry =
            versions
            |> List.tryPick (fun (vn, e) -> if vn = shownVersion then Some e else None)
            |> Option.defaultValue latestEntry
        let viewingOlder = shownVersion <> latestVersion
        let chart = NkDispersionChart.nkDispersionChart shownEntry.properties Nanometer MaterialEditorView.previewRange
        let editability =
            if viewingOlder then $" (version {shownVersion.value} — view-only)"
            else
                match shownEntry.complexity with
                | Some _ -> ""
                | None -> " (view-only engine preset)"
        let viewOnlyNote : IView list =
            if viewingOlder then
                [ TextBlock.create [
                      automationId<TextBlock> UiIds.viewOnlyNote
                      TextBlock.foreground (brush idleBorder)
                      TextBlock.textWrapping TextWrapping.Wrap
                      TextBlock.maxWidth 380.0
                      TextBlock.text $"Viewing version %d{shownVersion.value} (view-only) — the library edits the latest, version %d{latestVersion.value}."
                  ] :> IView ]
            else []
        [ Border.create [
              automationId<Border> UiIds.viewPanel
              Border.child (
                  StackPanel.create [
                      StackPanel.orientation Orientation.Vertical
                      StackPanel.spacing 2.0
                      StackPanel.children (
                          [ TextBlock.create [
                                TextBlock.fontWeight FontWeight.SemiBold
                                TextBlock.textWrapping TextWrapping.Wrap
                                TextBlock.maxWidth 380.0
                                TextBlock.text $"%s{shownEntry.name} — %s{liveCategoryName m.context.categories shownEntry.category}%s{editability}"
                            ] :> IView
                            TextBlock.create [
                                TextBlock.textWrapping TextWrapping.Wrap
                                TextBlock.maxWidth 380.0
                                TextBlock.text (shownEntry.description |> Option.defaultValue "")
                            ] :> IView ]
                          @ viewOnlyNote
                          @ versionsRow m versions latestVersion shownVersion dispatch
                          @ [ EmbeddedChart.create UiIds.viewPanelChart chart (NkDispersionChart.nkDispersionStyle chart) ])
                  ])
          ] :> IView ]

/// The window surface: the faceted tree (filter, representation picker, breadcrumbs, live
/// count, offers, tree) fills the window beside the right-hand panel carrying the verbs, the
/// inline confirm gate, the typed refusal message, and the selected entry's view panel.
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
                automationId<Border> UiIds.treeHost
                Border.padding (Thickness 8.0)
                Border.child (FacetedTreeControls.view (facetedState m) (facetedHandlers dispatch))
            ]
        ]
    ] :> IView
