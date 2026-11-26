/// Spec 0033 (022) — the Sample editor view (UICOMP_XDUO_0003): the pure MVU model and the
/// FuncUI projection behind `SampleEditorWindow`. The stack table sits over the step-21 Domain
/// `SampleStackEditor` — every bulk-toolbar verb dispatches a `SampleStackMsg` through
/// `applySampleStackMsg`, and each `Repeated` period group renders as ONE collapsible super-row
/// (a rotating-triangle expander) with an inline repeat-count stepper and its unit-cell layers
/// nested beneath. Material choice goes through the MATERIALS window in Select state (spec 0038
/// step 019 — the inline WrapPanel picker is gone): each layer row carries a Choose material…
/// verb opening (or re-targeting) the single-instance Materials window targeted at that
/// `LayerPosition` (`SelectionTarget.SampleLayerTarget`); the return is the TARGETED
/// `BindMaterialToLayer` — a vanished row is a no-op plus the status line — whose picked id
/// also becomes the toolbar's chosen material (the by-`MaterialId` selection contract,
/// `ChooseMaterial`, is unchanged). The material list RE-QUERIES `MaterialProxy.listMaterials`
/// whenever a Select session returns and on window activation (`RefreshMaterials` — the
/// load-once snapshot is gone; live cross-window notifications stay out of scope); the
/// per-layer orientation editor is INCLUDED only for
/// anisotropic materials (absent — not greyed — for isotropic ones); the optional QWOT entry
/// derives the physical thickness t = λ/(4n) read-only into canonical metres (the DBR λ/4
/// precedent, `Templates.dbrCell` / `Templates.dbrPeriods`, OpticalConstructor.Ui/Templates.fs).
/// Save persists through `SampleProxy`, routing on the target's `EntryFreshness` (spec 0038
/// step 008) — `addSample` for a `NewUnsaved` sample whose `SampleId` was minted AT WINDOW
/// OPEN (both NEW `SampleEditorIntent` cases carry it; the mint left the save path),
/// `updateSample` for a `Persisted` one — via the injected `SampleEditorContext` (the
/// functional-proxy/Context seam; tests substitute stubs); Cancel discards. Pure: `update`
/// only reaches IO through the context's proxy fields.
module OpticalConstructor.Ui.SampleEditorView

open System
open OpticalConstructor.Controls
open System.Globalization
open System.Numerics
open Avalonia
open Avalonia.Automation
open Avalonia.Controls
open Avalonia.Layout
open Avalonia.Media
open Avalonia.FuncUI.Builder
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open Berreman.Constants
open Berreman.Fields
open Berreman.Geometry
open Berreman.MaterialProperties
open Berreman.Media
open OpticalConstructor.Domain
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.SampleStackEditor
open OpticalConstructor.Domain.WindowMode

/// What Save targets (spec 0038 step 008): the sample id — ALWAYS present, minted at
/// Add-window open — plus its `EntryFreshness`. Save routes on the freshness (`NewUnsaved`
/// → `addSample`, `Persisted` → `updateSample`); no save path mints an id anymore.
type EditorTarget =
    {
        sampleId : SampleId
        freshness : WindowLauncher.EntryFreshness
    }

/// How the Sample editor OPENS (spec 0035 step 014): a blank NEW sample (the Add verb), a NEW
/// sample pre-seeded with a foldable starter multilayer period (the Make-multilayer verb — its
/// distinct launcher path), or an EXISTING sample updated in place (the Edit verb). A DU — not a
/// `Sample option` plus a bool — so the three open intents are NAMED. Both NEW intents carry a
/// `SampleId` minted AT WINDOW OPEN (spec 0038 step 008 — the id-mint left the save path, so
/// the window-policy seam keys the editor by the SAME id Save persists under); they differ
/// only in the seeded structure.
type SampleEditorIntent =
    | NewBlankSample of mintedId : SampleId
    | NewSeededMultilayer of mintedId : SampleId
    | EditSample of Sample

/// The window's IO seam (the functional-proxy Context convention): the materials read-seam the
/// step-019 re-query reaches, the samples write-seam the Save verb persists through, the
/// Materials-window Select-state opener behind the per-layer Choose material… verb, plus the
/// host's close request (the window passes `this.Close`; tests substitute recording stubs).
/// Function-valued fields have no structural equality, so the context compares by reference —
/// the model holding it keeps its equality.
[<ReferenceEquality>]
type SampleEditorContext =
    {
        /// The LIVE materials read seam (spec 0038 step 019): `RefreshMaterials` re-queries
        /// `listMaterials` through this on every Select-session return and window activation,
        /// replacing the load-once snapshot.
        materials : MaterialProxy
        samples : SampleProxy
        /// Open (or re-target) the single-instance MATERIALS window in SELECT state over the
        /// session context (the per-layer Choose material… verb, spec 0038 step 019). The
        /// composition root bakes the step-008 `WindowLauncher` under `MaterialsWindowKey`,
        /// the app category store and the step-005 modality switch; the `Window` argument is
        /// the requesting owner a modal Select open dialogs against.
        openMaterialsSelect : Window -> SelectionContext<MaterialEntry> -> unit
        requestClose : unit -> unit
    }

/// Which toolbar orientation-angle box a text edit targets.
type AngleSlot =
    | PhiSlot
    | ThetaSlot
    | PsiSlot

/// Whether the editor is editing normally or showing the unsaved-edit exit confirm surface
/// (spec 0038 step 033). A named two-case DU, never a naked `bool`: a dirty editor being closed
/// through Cancel or the window chrome flips to `ConfirmingDiscard`, which offers Discard / Keep.
type ExitPrompt =
    | Editing
    | ConfirmingDiscard

/// The structurally-comparable slice of the model Save persists — the identity facets, the
/// geometry-constrained supported emission (spec 0040 Part D.2 step 010), and the stack
/// structure (spec 0038 step 033's dirtiness snapshot). Every component is a plain record/DU
/// with structural equality; the reference-compared context / material list and the pure-UI
/// state (selection, collapsed groups, transient entry texts) are excluded.
type SampleEditSnapshot = EntryName * EntryDescription * SubstrateKind * Placement.Emission * SampleStructure

/// How many whole periods a repeat-count stepper moves a count by (negative removes).
type PeriodCountStep =
    | PeriodCountStep of int

    member this.value = let (PeriodCountStep n) = this in n

/// The editor's model: the sample identity facets (name / description / `SubstrateKind`), the
/// step-21 stack edit state, the material list resolved from `MaterialProxy.listMaterials`
/// (re-queried on every Select-session return and window activation — spec 0038 step 019),
/// and the transient entry texts. Reference-compared: a `MaterialEntry`'s engine properties
/// carry dispersion FUNCTION cases (no structural equality), and `update` returns a fresh
/// record anyway — so the Elmish equality gate sees every dispatch as a change and re-renders,
/// which is exactly this window's contract.
[<ReferenceEquality>]
type SampleEditorModel =
    {
        context : SampleEditorContext
        target : EditorTarget
        name : EntryName
        description : EntryDescription
        substrate : SubstrateKind
        /// The ray groups this sample can emit, geometry-constrained (spec 0040 Part D.2 step 010):
        /// a `Mirror` is pinned to `EmitReflectedOnly` (R fixed on, unclearable); a `ThinFilm` or a `Plate`
        /// is constrainable to R-only or T-only through the R/T checkboxes (spec 0046). Every write routes through
        /// `constrainEmission m.substrate`, so the "both off" state stays unrepresentable and a mirror
        /// can never clear its reflected branch.
        supportedEmission : Placement.Emission
        editor : SampleStackEditState
        materials : MaterialEntry list
        chosenMaterial : MaterialId option
        /// Film indices of the period groups whose super-row is collapsed (expanded default).
        collapsedGroups : Set<FilmItemIndex>
        thicknessText : ControlCaption
        qwotText : ControlCaption
        phiText : ControlCaption
        thetaText : ControlCaption
        psiText : ControlCaption
        /// The toolbar stepper's fold count `MakeRepeatBlock` uses (always >= 1).
        foldCount : PeriodCount
        /// Spec 0038 (033): the edit snapshot captured at load — dirtiness is `initialEdit`
        /// structurally unequal to the current edit slice (no dirty flag maintained).
        initialEdit : SampleEditSnapshot
        /// Spec 0038 (033): normal editing vs the unsaved-edit exit confirm surface.
        exit : ExitPrompt
        /// The last typed-error reason (or entry-validation hint) surfaced to the user.
        status : ControlCaption option
    }

type SampleEditorMsg =
    | SetName of EntryName
    | SetDescription of EntryDescription
    | SetSampleSubstrate of SubstrateKind
    /// Spec 0040 (D.2 step 010): the R / T emission checkboxes. Each carries the box's NEW
    /// switch; the update arm routes it through `Emission.withReflected` /
    /// `withTransmitted` then re-imposes the geometry constraint, so "both off" is impossible
    /// and a `Mirror` stays `EmitReflectedOnly` even if a message reaches it.
    | SetReflectedEmission of Placement.RayGroupSwitch
    | SetTransmittedEmission of Placement.RayGroupSwitch
    | ChooseMaterial of MaterialId
    /// Spec 0038 (019): re-query the material list from the LIVE `MaterialProxy` — dispatched
    /// whenever a Select session returns (chosen or cancelled) and on window activation,
    /// replacing the load-once snapshot. Live cross-window notifications stay out of scope.
    | RefreshMaterials
    /// A row click: adds an unselected position to the multi-selection, removes a selected one.
    | ToggleLayer of LayerPosition
    | ClearSelectionClicked
    /// The super-row's rotating-triangle expander.
    | ToggleGroup of FilmItemIndex
    | AddLayerClicked
    | SelectByMaterialClicked
    | SetThicknessText of ControlCaption
    | SetQwotText of ControlCaption
    | SetLayerHeightClicked
    | SetLayerMaterialClicked
    | SetOrientationText of AngleSlot * ControlCaption
    | SetOrientationClicked
    /// The per-layer orientation editor (only anisotropic layers render one).
    | SetLayerOrientation of LayerPosition * CrystalOrientation
    /// Spec 0038 (016/019): the TARGETED return of a Materials-window Select session
    /// (`SelectionTarget.SampleLayerTarget` — the per-layer Choose material… verb composes
    /// it): set THIS layer position's material, never "the current selection" (the modeless
    /// window may outlive a selection change). A vanished row is a no-op plus the status
    /// line, never a throw. A live row's pick also becomes the toolbar's CHOSEN material —
    /// the bulk verbs' one remaining source now that the inline picker is gone (step 019).
    | BindMaterialToLayer of LayerPosition * MaterialId
    | RemoveSelectedClicked
    | MoveUpClicked
    | MoveDownClicked
    /// The toolbar fold-count stepper (clamped at 1).
    | FoldCountBy of PeriodCountStep
    | MakeRepeatBlockClicked
    /// A group's inline stepper: resize the `Repeated` group at a films index by whole periods.
    | GroupCountBy of FilmItemIndex * PeriodCountStep
    /// Spec 0040 (D.4 step 011): the TARGETED return of a substrate Materials-window Select
    /// session (`SelectionTarget.SampleSubstrateTarget` — the substrate Set… verb composes it):
    /// set the thick substrate plate to the picked material. The substrate is a single slot with
    /// no positional identity (unlike a film row), so the pick always lands; the picked id also
    /// becomes the toolbar's chosen material (the `BindMaterialToLayer` precedent). A closed
    /// editor makes the dispatch a no-op.
    | BindMaterialToSubstrate of MaterialId
    /// Clear the substrate plate (spec 0033 gap G12).
    | ClearSubstrateClicked
    /// Set the lower half-space to the chosen material (spec 0033 gap G12).
    | SetLowerClicked
    /// Clear the lower half-space back to vacuum (spec 0033 gap G12).
    | ClearLowerClicked
    | SaveClicked
    /// Requested exit (the Cancel button, or the window chrome routed through `OnClosing`):
    /// closes a pristine editor silently, shows the discard confirm on a dirty one (spec 0038 step 033).
    | CancelClicked
    /// Spec 0038 (033): the confirm surface's Discard changes — close without saving.
    | DiscardConfirmed
    /// Spec 0038 (033): the confirm surface's Keep editing — dismiss the confirm, stay in the editor.
    | KeepEditing

// ---------------------------------------------------------------------------
// Pure helpers.
// ---------------------------------------------------------------------------

/// A new layer's starting thickness (the editor's placeholder; the bulk verbs then refine it).
let defaultLayerThickness : Thickness = Thickness.nm 100.0<nm>

/// The Euler convention the editors write (the step-20 tests' convention).
let private defaultConvention : RotationConvention = ZmXpZm

/// The reference wavelength the anisotropy classification evaluates dispersive tensors at
/// (a dispersive entry has no single tensor until a wavelength is chosen — 600 nm is the
/// repo's common visible-band reference).
let private referenceWaveLength : WaveLength = WaveLength.nm 600.0<nm>

type MaterialEntry with

    /// Whether the material is anisotropic — i.e. its crystal orientation matters. True
    /// when the eps or mu tensor (evaluated at the reference wavelength through the engine
    /// `getProperties` path) has any off-diagonal component or unequal diagonal, or when ANY rho
    /// component is nonzero (gyrotropy is orientation-sensitive; the isotropic default rho is the
    /// zero matrix). The per-layer orientation editor renders ONLY for these entries.
    member this.isAnisotropic : bool =
        let tolerance = 1.0e-12
        let p = this.properties.getProperties referenceWaveLength
        let offDiagonalOrUnequalDiagonal (get : int -> int -> Complex) : bool =
            let offDiagonal =
                [ (0, 1); (0, 2); (1, 0); (1, 2); (2, 0); (2, 1) ]
                |> List.exists (fun (i, j) -> (get i j).Magnitude > tolerance)
            let unequalDiagonal =
                (get 0 0 - get 1 1).Magnitude > tolerance
                || (get 1 1 - get 2 2).Magnitude > tolerance
            offDiagonal || unequalDiagonal
        let anyNonZero (get : int -> int -> Complex) : bool =
            [ for i in 0 .. 2 do for j in 0 .. 2 -> (i, j) ]
            |> List.exists (fun (i, j) -> (get i j).Magnitude > tolerance)
        offDiagonalOrUnequalDiagonal (fun i j -> p.eps.[i, j])
        || offDiagonalOrUnequalDiagonal (fun i j -> p.mu.[i, j])
        || anyNonZero (fun i j -> p.rho.[i, j])

/// n = Re[√ε₁₁] at the wavelength, through the engine's own eps dispersion path (the
/// NkDispersionChart / MaterialImport.exportCsv extraction — no n formula re-derived).
let refractionIndexAt (entry : MaterialEntry) (w : WaveLength) : RefractionIndex =
    (Complex.Sqrt (entry.properties.epsWithDisp.getEps w).[0, 0]).Real |> RefractionIndex

/// The quarter-wave optical thickness: t = λ/(4n), in the wavelength's own unit through the
/// engine `Thickness` constructors (the DBR template λ/4 precedent, Templates.fs:103,106).
let qwotThickness (w : WaveLength) (n : RefractionIndex) : Thickness =
    match w with
    | Nm lambdaNm -> Thickness.nm (lambdaNm / (4.0 * n.value))
    | Mkm lambdaMkm -> Thickness.mkm (lambdaMkm / (4.0 * n.value))

type ControlCaption with

    /// The entry text as an invariant-culture number; `None` when it does not parse.
    member private this.tryEntryNumber : float option =
        match Double.TryParse(this.value, NumberStyles.Float, CultureInfo.InvariantCulture) with
        | true, v -> Some v
        | _ -> None

type Thickness with

    /// The thickness as a readout shows it, in display nanometres (`∞` for the semi-infinite case).
    member this.nanometreCaption : ControlCaption =
        match this with
        | Thickness meters -> ControlCaption $"%g{meters / nmToMeter / oneNanometer} nm"
        | Infinity -> ControlCaption "∞"

type EulerAngles with

    /// Angles → orientation: all-zero angles mean the material's own principal axes (the
    /// identity is DATA, not an EulerRotation carrying zeros); otherwise the editors' convention.
    member private this.editorOrientation : CrystalOrientation =
        if this.phi.degrees = 0.0 && this.theta.degrees = 0.0 && this.psi.degrees = 0.0 then PrimaryAxes
        else EulerRotation { this with convention = defaultConvention }

type CrystalOrientation with

    /// The Euler angles the per-layer editor shows: the principal axes are all three at zero.
    member private this.editorAngles : EulerAngles =
        match this with
        | PrimaryAxes -> { EulerAngles.convention = defaultConvention; phi = Angle.zero; theta = Angle.zero; psi = Angle.zero }
        | EulerRotation angles -> angles

    /// The orientation as a layer row summarises it.
    member private this.editorCaption : ControlCaption =
        match this with
        | PrimaryAxes -> ControlCaption "primary axes"
        | EulerRotation angles -> ControlCaption $"φ=%g{angles.phi.degrees}° θ=%g{angles.theta.degrees}° ψ=%g{angles.psi.degrees}°"

type PeriodCount with

    /// This count moved by `step` whole periods (unclamped: `SetRepeatCount` rejects a count below 1).
    member this.steppedBy (step : PeriodCountStep) : PeriodCount = PeriodCount (this.value + step.value)

type SampleEditorModel with

    member private this.chosenEntry : MaterialEntry option =
        match this.chosenMaterial with
        | Some id -> this.materials |> List.tryFind (fun e -> e.id = id)
        | None -> None

    /// The QWOT-derived physical thickness, when the entry text parses to a positive wavelength
    /// (nm) and a material is chosen to take n from. `None` otherwise — the readout shows a dash
    /// and the set-thickness verb falls back to the plain nm entry.
    member this.qwotDerived : Thickness option =
        match this.chosenEntry, this.qwotText.tryEntryNumber with
        | Some entry, Some lambdaNm when lambdaNm > 0.0 ->
            let w = WaveLength.nm (lambdaNm * 1.0<nm>)
            let n = refractionIndexAt entry w
            if n.value > 0.0 then Some (qwotThickness w n) else None
        | _, _ -> None

    /// The flattened film count — each `Repeated` group expands to count × cell (the structure
    /// readout the "2-layer selection repeated K times shows 2*K films" acceptance observes).
    member this.filmsCount : int =
        this.editor.structure.expandedFilms |> List.length

    /// The current edit slice compared for dirtiness (spec 0038 step 033): the identity facets, the
    /// supported emission (spec 0040 Part D.2 step 010 — an R/T change dirties the editor) and the
    /// stack structure, all plain records/DUs. The transient entry texts, the selection, the
    /// collapsed-group set and the re-queried material list are pure UI / read state and excluded.
    member private this.currentEdit : SampleEditSnapshot =
        (this.name, this.description, this.substrate, this.supportedEmission, this.editor.structure)

    /// Whether the editor carries unsaved changes: the current edit slice differs structurally from
    /// the slice captured at load. No dirty flag is maintained (spec 0038 step 033).
    member this.isDirty : bool =
        this.initialEdit <> this.currentEdit

    member internal this.selectionAvailability : ControlAvailability =
        if Set.isEmpty this.editor.selection then ControlWithheld else ControlOffered

    member internal this.chosenMaterialAvailability : ControlAvailability =
        match this.chosenMaterial with
        | Some _ -> ControlOffered
        | None -> ControlWithheld

    /// Set material needs both a selection and a chosen material.
    member internal this.setMaterialAvailability : ControlAvailability =
        match this.selectionAvailability, this.chosenMaterialAvailability with
        | ControlOffered, ControlOffered -> ControlOffered
        | _, _ -> ControlWithheld

    /// A material's display name by id (a placeholder when the id is unresolved).
    member internal this.materialCaption (id : MaterialId) : ControlCaption =
        match this.materials |> List.tryFind (fun e -> e.id = id) with
        | Some e -> e.name.caption
        | None -> ControlCaption $"unknown ({id.value})"

// ---------------------------------------------------------------------------
// init / toSample / update (pure — IO only through the context's proxy fields).
// ---------------------------------------------------------------------------

let private emptyStructure : SampleStructure =
    {
        films = []
        substrate = None
        lower = None
    }

let initSampleEditor (context : SampleEditorContext) (materials : MaterialEntry list) (intent : SampleEditorIntent) : SampleEditorModel =
    let target, name, description, substrate, structure, emission =
        // Both NEW intents carry the id minted AT WINDOW OPEN as a NewUnsaved target (spec 0038
        // step 008); the seeded multilayer path opens onto the Domain starter period (spec 0035
        // step 014), the blank Add onto nothing. A NEW sample takes the geometry default supported
        // emission (spec 0040 Part D.2 step 010); an edited one carries the sample's own value.
        match intent with
        | NewBlankSample mintedId ->
            ({ sampleId = mintedId; freshness = WindowLauncher.NewUnsaved }, EntryName "", EntryDescription "", ThinFilm, emptyStructure, ThinFilm.defaultSupportedEmission)
        | NewSeededMultilayer mintedId ->
            ({ sampleId = mintedId; freshness = WindowLauncher.NewUnsaved }, EntryName "", EntryDescription "", ThinFilm, starterMultilayerStructure, ThinFilm.defaultSupportedEmission)
        | EditSample s ->
            ({ sampleId = s.id; freshness = WindowLauncher.Persisted }, s.name, s.description, s.substrate, s.structure, s.supportedEmission)
    // Normalise the seed emission to its geometry constraint so the model starts consistent even
    // if an inconsistent sample is opened (spec 0040 Part D.2 — the single enforcement point).
    let supportedEmission = constrainEmission substrate emission
    {
        context = context
        target = target
        name = name
        description = description
        substrate = substrate
        supportedEmission = supportedEmission
        editor = SampleStackEditState.ofStructure structure
        materials = materials
        chosenMaterial = None
        collapsedGroups = Set.empty
        thicknessText = ControlCaption ""
        qwotText = ControlCaption ""
        phiText = ControlCaption ""
        thetaText = ControlCaption ""
        psiText = ControlCaption ""
        foldCount = PeriodCount 2
        // The edit snapshot at load — a freshly opened editor (including a seeded starter) is
        // pristine and closes silently (spec 0038 step 033).
        initialEdit = (name, description, substrate, supportedEmission, structure)
        exit = Editing
        status = None
    }

/// The sample the model currently denotes, under the given id (Save chooses the id by target).
let toSample (id : SampleId) (m : SampleEditorModel) : Sample =
    {
        id = id
        name = m.name
        structure = m.editor.structure
        substrate = m.substrate
        description = m.description
        // The user's edited emission (spec 0040 Part D.2 step 010): the R/T checkboxes drive
        // `m.supportedEmission`, already kept inside the geometry constraint by every update arm
        // (a Mirror stays EmitReflectedOnly; a ThinFilm / Plate keeps its R-only / T-only / both choice).
        supportedEmission = m.supportedEmission
    }

/// Route one step-21 editor message; a typed rejection surfaces its reason as the status.
let private applyStack (msg : SampleStackMsg) (m : SampleEditorModel) : SampleEditorModel =
    match applySampleStackMsg msg m.editor with
    | Ok editor -> { m with editor = editor; status = None }
    | Error e -> { m with status = Some e.caption }

let updateSampleEditor (msg : SampleEditorMsg) (m : SampleEditorModel) : SampleEditorModel =
    match msg with
    | SetName s -> { m with name = s }
    | SetDescription s -> { m with description = s }
    | SetSampleSubstrate kind ->
        // Re-impose the new geometry's emission constraint (spec 0040 Part D.2 step 010): flipping
        // to Mirror re-pins R-only, so the R-fixed-on invariant stays true and the model stays
        // consistent with what `toSample` persists. Spec 0046: leaving Mirror restores the new
        // geometry's default (R + T) rather than keeping the R-only a mirror forced.
        let emission =
            match m.substrate, kind with
            | Mirror, (ThinFilm | ThickPlate) -> kind.defaultSupportedEmission
            | _ -> constrainEmission kind m.supportedEmission
        { m with substrate = kind; supportedEmission = emission }
    | SetReflectedEmission switch ->
        // Turn the reflected group on/off (spec 0040 Part D.2 step 010): the smart setter forbids
        // the both-off state, and `constrainEmission` re-pins a Mirror to EmitReflectedOnly.
        { m with supportedEmission = constrainEmission m.substrate (Placement.Emission.withReflected switch m.supportedEmission) }
    | SetTransmittedEmission switch ->
        { m with supportedEmission = constrainEmission m.substrate (Placement.Emission.withTransmitted switch m.supportedEmission) }
    | ChooseMaterial id -> { m with chosenMaterial = Some id }
    | RefreshMaterials ->
        // Spec 0038 (019): re-query the material list from the LIVE proxy — a material added
        // through another window appears here. The status is left alone (a vanished-row
        // message must survive the refresh the same return dispatches); a store refusal keeps
        // the current list and surfaces its typed reason instead.
        match m.context.materials.listMaterials MaterialLibrary.ActiveOnly with
        | Ok entries -> { m with materials = entries }
        | Error e -> { m with status = Some e.caption }
    | ToggleLayer position ->
        if Set.contains position m.editor.selection
        then { m with editor = { m.editor with selection = Set.remove position m.editor.selection } }
        else applyStack (SelectLayer position) m
    | ClearSelectionClicked -> applyStack ClearSelection m
    | ToggleGroup groupIndex ->
        if Set.contains groupIndex m.collapsedGroups
        then { m with collapsedGroups = Set.remove groupIndex m.collapsedGroups }
        else { m with collapsedGroups = Set.add groupIndex m.collapsedGroups }
    | AddLayerClicked ->
        // spec 0033 gap G13: appending a layer now routes through the pure Domain
        // `AddLayer` arm (`SampleStackEditor`), not a view-level structural edit — so the
        // behaviour stays testable without a window.
        let materialIdOpt =
            match m.chosenMaterial with
            | Some id -> Some id
            | None -> m.materials |> List.tryHead |> Option.map (fun e -> e.id)
        match materialIdOpt with
        | Some id ->
            applyStack (AddLayer { materialId = MaterialVersionId.firstOf id; thickness = defaultLayerThickness; orientation = PrimaryAxes }) m
        | None -> { m with status = Some (ControlCaption "no materials are available to add a layer from") }
    | SelectByMaterialClicked ->
        match m.chosenMaterial with
        | Some id -> applyStack (SelectByMaterial id) m
        | None -> { m with status = Some (ControlCaption "choose a material to select by") }
    | SetThicknessText s -> { m with thicknessText = s }
    | SetQwotText s -> { m with qwotText = s }
    | SetLayerHeightClicked ->
        // A valid QWOT derivation wins; otherwise the plain nm entry applies.
        match m.qwotDerived with
        | Some t -> applyStack (SetThicknessOfSelected t) m
        | None ->
            match m.thicknessText.tryEntryNumber with
            | Some v when v > 0.0 -> applyStack (SetThicknessOfSelected (Thickness.nm (v * 1.0<nm>))) m
            | Some _ | None -> { m with status = Some (ControlCaption "enter a positive thickness in nm (or a QWOT wavelength with a chosen material)") }
    | SetLayerMaterialClicked ->
        match m.chosenMaterial with
        | Some id -> applyStack (SetMaterialOfSelected (MaterialVersionId.firstOf id)) m
        | None -> { m with status = Some (ControlCaption "choose a material to set") }
    | SetOrientationText (slot, text) ->
        match slot with
        | PhiSlot -> { m with phiText = text }
        | ThetaSlot -> { m with thetaText = text }
        | PsiSlot -> { m with psiText = text }
    | SetOrientationClicked ->
        let angleOf (text : ControlCaption) : Angle option =
            if String.IsNullOrWhiteSpace text.value then Some (Angle.degree 0.0<deg>)
            else text.tryEntryNumber |> Option.map (fun degrees -> Angle.degree (degrees * 1.0<deg>))
        match angleOf m.phiText, angleOf m.thetaText, angleOf m.psiText with
        | Some phi, Some theta, Some psi ->
            let angles = { EulerAngles.convention = defaultConvention; phi = phi; theta = theta; psi = psi }
            applyStack (SetOrientationOfSelected angles.editorOrientation) m
        | _, _, _ -> { m with status = Some (ControlCaption "the orientation angles must be numbers (degrees)") }
    | SetLayerOrientation (position, orientation) ->
        // One layer's editor: apply the selection-shaped Domain transform to JUST this
        // position, then restore the user's multi-selection (the transform never rejects).
        let single = { m.editor with selection = Set.ofList [ position ] }
        match applySampleStackMsg (SetOrientationOfSelected orientation) single with
        | Ok next -> { m with editor = { next with selection = m.editor.selection }; status = None }
        | Error e -> { m with status = Some e.caption }
    | BindMaterialToLayer (position, materialId) ->
        // Spec 0038 (016): a Materials-window Select session returned for THIS layer position.
        // The position is validated against the CURRENT structure first (the Domain seam
        // `SampleStructure.tryFindLayer` — the row may have been deleted while the modeless window was
        // open): a vanished row is a no-op plus the status line, never a throw. A live row
        // takes the material through the same selection-shaped transform-and-restore dance
        // as the per-layer orientation editor above — and the picked id also becomes the
        // toolbar's CHOSEN material (step 019: the bulk verbs' one remaining source now that
        // the inline picker is gone; the vanished-row no-op chooses nothing).
        match m.editor.structure.tryFindLayer position with
        | Some _ ->
            let single = { m.editor with selection = Set.ofList [ position ] }
            match applySampleStackMsg (SetMaterialOfSelected (MaterialVersionId.firstOf materialId)) single with
            | Ok next -> { m with editor = { next with selection = m.editor.selection }; chosenMaterial = Some materialId; status = None }
            | Error e -> { m with status = Some e.caption }
        | None ->
            { m with status = Some (ControlCaption "the chosen material was not applied — its target layer is no longer in the stack") }
    | RemoveSelectedClicked -> applyStack RemoveSelected m
    | MoveUpClicked -> applyStack MoveSelectedUp m
    | MoveDownClicked -> applyStack MoveSelectedDown m
    | FoldCountBy step -> { m with foldCount = max (PeriodCount 1) (m.foldCount.steppedBy step) }
    | MakeRepeatBlockClicked -> applyStack (MakeRepeatBlock m.foldCount) m
    | GroupCountBy (groupIndex, step) ->
        match List.tryItem groupIndex.value m.editor.structure.films with
        | Some (Repeated g) -> applyStack (SetRepeatCount (groupIndex, g.count.steppedBy step)) m
        | Some (SingleLayer _) | None -> { m with status = Some (ControlCaption $"films item %d{groupIndex.value} is not a repeat group") }
    | BindMaterialToSubstrate materialId ->
        // Spec 0040 (D.4 step 011): a Materials-window Select session returned for the substrate
        // slot (the substrate Set… verb's return). A sample holds ONE substrate plate — no
        // positional identity to validate, unlike a film row (`BindMaterialToLayer`) — so the
        // pick always lands: build the plate from the chosen material's current version (pin v1,
        // spec 0038 step 022) and set it. The picked id also becomes the toolbar's CHOSEN
        // material, matching the layer pick. `SetSubstrate` never rejects; the Error arm is
        // defensive totality (the status-line path), and a closed editor makes this a dead-
        // dispatch no-op. The Domain case is qualified — the view `Msg` also has a `SetSubstrate`
        // (the geometry facet), so the bare name would resolve to the wrong DU.
        let layer = { materialId = MaterialVersionId.firstOf materialId; thickness = defaultLayerThickness; orientation = PrimaryAxes }
        match applySampleStackMsg (SampleStackMsg.SetSubstrate (Some layer)) m.editor with
        | Ok editor -> { m with editor = editor; chosenMaterial = Some materialId; status = None }
        | Error e -> { m with status = Some e.caption }
    | ClearSubstrateClicked -> applyStack (SampleStackMsg.SetSubstrate None) m
    | SetLowerClicked ->
        match m.chosenMaterial with
        | Some id -> applyStack (SampleStackMsg.SetLower (Some (MaterialVersionId.firstOf id))) m
        | None -> { m with status = Some (ControlCaption "choose a material to set as the lower half-space") }
    | ClearLowerClicked -> applyStack (SampleStackMsg.SetLower None) m
    | SaveClicked ->
        // The store mints on pinned structure edits and mutates unpinned versions
        // (spec 0038 step 022; spec 0050 S2.2).
        let saved = m.context.samples.saveSample (toSample m.target.sampleId m)
        match saved with
        | Ok () ->
            m.context.requestClose ()
            { m with status = None }
        | Error e -> { m with status = Some e.caption }
    | CancelClicked ->
        // Spec 0038 (033): a dirty editor MUST NOT close silently — show the discard confirm;
        // a pristine one closes as today. The window chrome (`OnClosing`) routes here too, so
        // both exits are equally gated.
        if m.isDirty then { m with exit = ConfirmingDiscard }
        else
            m.context.requestClose ()
            m
    | DiscardConfirmed ->
        // Close WITHOUT saving — the discard confirm's negative action (spec 0038 step 033).
        m.context.requestClose ()
        m
    | KeepEditing ->
        // Dismiss the confirm and stay in the editor (spec 0038 step 033).
        { m with exit = Editing }

// ---------------------------------------------------------------------------
// The FuncUI view. Styling matches the sibling bars' idle/chosen boxes; every control in a
// variable-membership list carries a mutable AutomationId, never `Name` (a styled control
// cannot be renamed when FuncUI recycles it — the SampleLibraryControls precedent, and this
// window re-renders on every Elmish message).
// ---------------------------------------------------------------------------

let private brush (c : Color) : IBrush = SolidColorBrush(c) :> IBrush
let private idleBackground = Color.FromRgb(232uy, 232uy, 232uy)
let private chosenBackground = Color.FromRgb(150uy, 185uy, 235uy)
let private groupBackground = Color.FromRgb(214uy, 224uy, 238uy)
let private saveBackground = Color.FromRgb(186uy, 224uy, 186uy)
let private cancelBackground = Color.FromRgb(236uy, 202uy, 202uy)
let private idleBorder = Color.FromRgb(120uy, 120uy, 120uy)
let private hintColor = Color.FromRgb(110uy, 110uy, 110uy)
let private errorColor = Color.FromRgb(165uy, 40uy, 40uy)

// Avalonia layout thicknesses are written `Avalonia.Thickness(…)`: the bare `Thickness` in this file
// is the DOMAIN layer thickness (Berreman.Media), opened after Avalonia.

/// A clickable, styled box on `background` (the chosen look or the idle one). `e.Handled <- true`
/// drops FuncUI's duplicate Tunnel|Bubble pass; re-subscribe when the id or the look changes.
let private clickBoxView (id : ConstructorUiId) (background : Color) (child : IView) (onClick : unit -> unit) : IView =
    Border.create [
        id.automationAttribute<Border> ()
        Border.background (brush background)
        Border.borderBrush (brush idleBorder)
        Border.borderThickness 1.0
        Border.cornerRadius (CornerRadius 3.0)
        Border.padding (Avalonia.Thickness(10.0, 4.0))
        Border.margin (Avalonia.Thickness(0.0, 0.0, 6.0, 4.0))
        Border.verticalAlignment VerticalAlignment.Center
        Border.child child
        Border.onPointerPressed ((fun e -> e.Handled <- true; onClick ()), SubPatchOptions.OnChangeOf (box (id.value, background)))
    ] :> IView

let private clickBox (id : ConstructorUiId) (label : ControlCaption) (background : Color) (onClick : unit -> unit) : IView =
    clickBoxView id background (TextBlock.create [ TextBlock.text label.value ] :> IView) onClick

/// A toolbar verb button on `background` — withheld verbs are present but inert (a disabled
/// Border dispatches no pointer event).
let private verbButton (id : ConstructorUiId) (label : ControlCaption) (background : Color) (availability : ControlAvailability) (onClick : unit -> unit) : IView =
    Border.create [
        id.automationAttribute<Border> ()
        Border.isEnabled availability.isEnabled
        Border.opacity (match availability with ControlOffered -> 1.0 | ControlWithheld -> 0.4)
        Border.background (brush background)
        Border.borderBrush (brush idleBorder)
        Border.borderThickness 1.0
        Border.cornerRadius (CornerRadius 3.0)
        Border.padding (Avalonia.Thickness(12.0, 5.0))
        Border.margin (Avalonia.Thickness(0.0, 0.0, 8.0, 0.0))
        Border.verticalAlignment VerticalAlignment.Center
        Border.child (TextBlock.create [ TextBlock.text label.value ])
        Border.onPointerPressed ((fun e -> e.Handled <- true; onClick ()), SubPatchOptions.OnChangeOf (box (id.value, availability.isEnabled)))
    ] :> IView

/// The Save / Cancel actions (one row, distinct positive/negative styling).
let private actionButton (id : ConstructorUiId) (label : ControlCaption) (background : Color) (onClick : unit -> unit) : IView =
    Border.create [
        id.automationAttribute<Border> ()
        Border.background (brush background)
        Border.borderBrush (brush idleBorder)
        Border.borderThickness 1.0
        Border.cornerRadius (CornerRadius 3.0)
        Border.padding (Avalonia.Thickness(22.0, 6.0))
        Border.margin (Avalonia.Thickness(0.0, 0.0, 10.0, 0.0))
        Border.verticalAlignment VerticalAlignment.Center
        Border.child (TextBlock.create [ TextBlock.text label.value ])
        Border.onPointerPressed ((fun e -> e.Handled <- true; onClick ()), SubPatchOptions.OnChangeOf (box id.value))
    ] :> IView

/// A −/count/+ stepper (the toolbar fold count and each group's inline period count).
let private stepper (containerId : ConstructorUiId) (minusId : ConstructorUiId) (plusId : ConstructorUiId) (count : PeriodCount) (stepBy : PeriodCountStep -> unit) : IView =
    StackPanel.create [
        containerId.automationAttribute<StackPanel> ()
        StackPanel.orientation Orientation.Horizontal
        StackPanel.spacing 2.0
        StackPanel.children [
            clickBox minusId (ControlCaption "−") idleBackground (fun () -> stepBy (PeriodCountStep -1))
            TextBlock.create [
                TextBlock.text (string count.value)
                TextBlock.verticalAlignment VerticalAlignment.Center
                TextBlock.margin (Avalonia.Thickness(4.0, 0.0))
            ] :> IView
            clickBox plusId (ControlCaption "+") idleBackground (fun () -> stepBy (PeriodCountStep 1))
        ]
    ] :> IView

type ControlCaption with

    /// The caption as a row's vertically centred label.
    member private this.sampleEditorLabel : IView =
        TextBlock.create [ TextBlock.text this.value; TextBlock.verticalAlignment VerticalAlignment.Center ] :> IView

// -- the identity / facet rows -------------------------------------------------------------

let private nameRow (m : SampleEditorModel) (dispatch : SampleEditorMsg -> unit) : IView =
    StackPanel.create [
        StackPanel.orientation Orientation.Horizontal
        StackPanel.spacing 6.0
        StackPanel.children [
            (ControlCaption "Name:").sampleEditorLabel
            TextBox.create [
                TextBox.name ConstructorUiId.sampleNameBox.value
                TextBox.width 340.0
                TextBox.text m.name.value
                TextBox.onTextChanged (EntryName >> SetName >> dispatch)
            ] :> IView
        ]
    ] :> IView

/// Spec 0046: the description box FILLS the row beside its docked label (a fixed 620 px box ran off the
/// edge of a window narrower than the label plus the box).
let private descriptionRow (m : SampleEditorModel) (dispatch : SampleEditorMsg -> unit) : IView =
    DockPanel.create [
        DockPanel.children [
            Border.create [ Border.dock Dock.Left; Border.margin (Avalonia.Thickness(0.0, 0.0, 6.0, 0.0)); Border.child ((ControlCaption "Description:").sampleEditorLabel) ]
            TextBox.create [
                TextBox.name ConstructorUiId.sampleDescriptionBox.value
                TextBox.text m.description.value
                TextBox.onTextChanged (EntryDescription >> SetDescription >> dispatch)
            ] :> IView
        ]
    ] :> IView

let private substrateRow (m : SampleEditorModel) (dispatch : SampleEditorMsg -> unit) : IView =
    StackPanel.create [
        StackPanel.orientation Orientation.Horizontal
        StackPanel.spacing 6.0
        StackPanel.children (
            (ControlCaption "Geometry:").sampleEditorLabel
            :: ([ ThinFilm; ThickPlate; Mirror ]
                |> List.map (fun kind ->
                    let background = if m.substrate = kind then chosenBackground else idleBackground
                    clickBox (ConstructorUiId.sampleSubstrateKind kind.uiKey) kind.caption background (fun () -> dispatch (SetSampleSubstrate kind)))))
    ] :> IView

/// One emission group checkbox (spec 0040 Part D.2 step 010). `isChecked` is bound ONE-WAY from
/// the model; the dispatch rides `Button.onClick`, which fires only on USER activation (pointer /
/// keyboard), never on the render's programmatic `IsChecked` set — so there is no FuncUI feedback
/// loop. A withheld box (the Mirror R box) dispatches nothing.
let private emissionCheck
    (id : ConstructorUiId)
    (label : ControlCaption)
    (switch : Placement.RayGroupSwitch)
    (availability : ControlAvailability)
    (toMsg : Placement.RayGroupSwitch -> SampleEditorMsg)
    (dispatch : SampleEditorMsg -> unit) : IView =
    let isChecked, toggled =
        match switch with
        | Placement.RayGroupOn -> true, Placement.RayGroupOff
        | Placement.RayGroupOff -> false, Placement.RayGroupOn
    CheckBox.create [
        id.automationAttribute<CheckBox> ()
        // Reach `isEnabled` (InputElement) and `isChecked` (ToggleButton) through the in-scope
        // `CheckBox` class — the FuncUI DSL augments each control class with its inherited
        // attributes, and only `Avalonia.Controls` is opened here (not `.Primitives` / `.Input`),
        // so the base-class module names do not resolve (the `Border.isEnabled` precedent).
        CheckBox.isEnabled availability.isEnabled
        CheckBox.isChecked isChecked
        CheckBox.content label.value
        CheckBox.onClick ((fun _ -> dispatch (toMsg toggled)), SubPatchOptions.OnChangeOf (box (id.value, isChecked, availability.isEnabled)))
    ] :> IView

/// The supported-emission facet (spec 0040 Part D.2 step 010; spec 0046): the ray groups this sample
/// emits. A `ThinFilm` or a `Plate` exposes BOTH the R and T checkboxes so the user constrains emission
/// to R-only or T-only (the `Emission` smart setters keep "both off" unrepresentable); a `Mirror` shows R
/// fixed on + disabled and renders NO T box (a mirror is observed in reflection alone).
let private emissionRow (m : SampleEditorModel) (dispatch : SampleEditorMsg -> unit) : IView =
    let reflected = if m.supportedEmission.emitsReflected then Placement.RayGroupOn else Placement.RayGroupOff
    let transmitted = if m.supportedEmission.emitsTransmitted then Placement.RayGroupOn else Placement.RayGroupOff
    let reflectedAvailability =
        match m.substrate with
        | ThinFilm
        | ThickPlate -> ControlOffered
        | Mirror -> ControlWithheld
    StackPanel.create [
        StackPanel.orientation Orientation.Horizontal
        StackPanel.spacing 10.0
        StackPanel.children (
            [
                (ControlCaption "Emission:").sampleEditorLabel
                emissionCheck ConstructorUiId.sampleEmitReflectedCheck (ControlCaption "Reflected (R)") reflected reflectedAvailability SetReflectedEmission dispatch
            ]
            @ (match m.substrate with
               | ThinFilm
               | ThickPlate -> [ emissionCheck ConstructorUiId.sampleEmitTransmittedCheck (ControlCaption "Transmitted (T)") transmitted ControlOffered SetTransmittedEmission dispatch ]
               | Mirror -> []))
    ] :> IView

// -- the stack table -------------------------------------------------------------------------

/// Compose one Select session for a layer row and open (or re-target) the Materials window
/// through the context seam (spec 0038 step 019 — the step-017 `chooseFromLibrary` precedent:
/// composed in the VIEW, where the render's dispatch is the return path into the loop).
/// `onSelected` bakes the TARGETED `BindMaterialToLayer` — the vanished-row no-op lives in its
/// arm — and BOTH outcomes re-query the material list (`RefreshMaterials`): a Select window
/// returning is one of the two re-query triggers, however the session ended.
let private chooseMaterialForLayer (m : SampleEditorModel) (dispatch : SampleEditorMsg -> unit) (position : LayerPosition) (owner : Window) : unit =
    let selectContext : SelectionContext<MaterialEntry> =
        {
            // The sample-layer pick's fixed kind (the step-016 banner names it); the material
            // corpus satisfies it structurally, so it narrows nothing in the Materials window.
            kindConstraint = KindConstraint Placement.CatalogueKind.Sample
            currentEntryIdOpt = None
            target = SampleLayerTarget position
            onSelected =
                fun (entry : MaterialEntry) ->
                    dispatch (BindMaterialToLayer (position, entry.id))
                    dispatch RefreshMaterials
            onCancelled = fun () -> dispatch RefreshMaterials
        }
    m.context.openMaterialsSelect owner selectContext

/// The per-layer Choose material… verb (spec 0038 step 019): opens the Materials window in
/// Select state targeted at THIS row's position. Rendered BESIDE its row, never inside the
/// row's Border — the row's tunnel-phase ToggleLayer press handler would swallow a nested
/// clickable. The owner window for a modal Select open is resolved from the click's own visual
/// tree at dispatch time (the step-017 precedent); the subscription re-patches on the id — the
/// closure's only other captures (the context seam, dispatch) are render-stable.
let private chooseMaterialVerb (m : SampleEditorModel) (dispatch : SampleEditorMsg -> unit) (position : LayerPosition) (chooseId : ConstructorUiId) : IView =
    Border.create [
        chooseId.automationAttribute<Border> ()
        Border.background (brush idleBackground)
        Border.borderBrush (brush idleBorder)
        Border.borderThickness 1.0
        Border.cornerRadius (CornerRadius 3.0)
        Border.padding (Avalonia.Thickness(10.0, 3.0))
        Border.margin (Avalonia.Thickness(6.0, 0.0, 0.0, 3.0))
        Border.verticalAlignment VerticalAlignment.Top
        Border.child (TextBlock.create [ TextBlock.text "Choose material…" ])
        Border.onPointerPressed ((fun e ->
            e.Handled <- true
            match e.Source with
            | :? Visual as source ->
                match TopLevel.GetTopLevel source with
                | :? Window as owner -> chooseMaterialForLayer m dispatch position owner
                | _ -> ()
            | _ -> ()), SubPatchOptions.OnChangeOf chooseId.value)
    ] :> IView

/// Compose one Select session for the SUBSTRATE plate and open (or re-target) the Materials
/// window through the context seam (spec 0040 Part D.4 step 011 — the `chooseMaterialForLayer`
/// precedent). A film-less Plate has no layer row to reach the Materials window from, so the
/// substrate slot carries its OWN pick verb. The substrate has no positional identity, so the
/// return routes through the captured dispatch (`BindMaterialToSubstrate`) — a closed editor
/// makes it a no-op. Both outcomes re-query the material list (`RefreshMaterials`).
let private chooseMaterialForSubstrate (m : SampleEditorModel) (dispatch : SampleEditorMsg -> unit) (owner : Window) : unit =
    let selectContext : SelectionContext<MaterialEntry> =
        {
            // The sample substrate pick's fixed kind (the step-016 banner names it); the material
            // corpus satisfies it structurally, so it narrows nothing in the Materials window.
            kindConstraint = KindConstraint Placement.CatalogueKind.Sample
            currentEntryIdOpt = None
            target = SampleSubstrateTarget
            onSelected =
                fun (entry : MaterialEntry) ->
                    dispatch (BindMaterialToSubstrate entry.id)
                    dispatch RefreshMaterials
            onCancelled = fun () -> dispatch RefreshMaterials
        }
    m.context.openMaterialsSelect owner selectContext

/// The substrate plate's Set… verb (spec 0040 Part D.4 step 011): opens the Materials window in
/// Select state targeted at the substrate slot, REPLACING the old chosen-material-gated
/// 'Set from chosen' substrate button. Unlike that button it needs NO pre-chosen material — it
/// IS the pick, so a film-less Plate (no layer rows, hence no Choose material… verb) can still
/// choose a substrate material. The owner window for a modal Select open is resolved from the
/// click's own visual tree at dispatch time (the `chooseMaterialVerb` precedent). Styled as a
/// toolbar verb; it reuses the `setSubstrateButton` id.
let private setSubstrateVerb (m : SampleEditorModel) (dispatch : SampleEditorMsg -> unit) : IView =
    Border.create [
        ConstructorUiId.setSubstrateButton.automationAttribute<Border> ()
        Border.background (brush idleBackground)
        Border.borderBrush (brush idleBorder)
        Border.borderThickness 1.0
        Border.cornerRadius (CornerRadius 3.0)
        Border.padding (Avalonia.Thickness(12.0, 5.0))
        Border.margin (Avalonia.Thickness(0.0, 0.0, 8.0, 0.0))
        Border.verticalAlignment VerticalAlignment.Center
        Border.child (TextBlock.create [ TextBlock.text "Set…" ])
        Border.onPointerPressed ((fun e ->
            e.Handled <- true
            match e.Source with
            | :? Visual as source ->
                match TopLevel.GetTopLevel source with
                | :? Window as owner -> chooseMaterialForSubstrate m dispatch owner
                | _ -> ()
            | _ -> ()), SubPatchOptions.OnChangeOf ConstructorUiId.setSubstrateButton.value)
    ] :> IView

/// One layer's inline orientation editor (φ/θ/ψ in degrees) — rendered ONLY when the layer's
/// material is anisotropic; an unparsable entry dispatches nothing.
let private orientationEditorView (dispatch : SampleEditorMsg -> unit) (position : LayerPosition) (orientationId : ConstructorUiId) (orientation : CrystalOrientation) : IView =
    let angles = orientation.editorAngles
    let angleBox (label : ControlCaption) (current : Angle) (withAngle : Angle -> EulerAngles) : IView list =
        [
            label.sampleEditorLabel
            TextBox.create [
                TextBox.width 46.0
                TextBox.text $"%g{current.degrees}"
                TextBox.onTextChanged (
                    ControlCaption >> (fun text ->
                        match text.tryEntryNumber with
                        | Some degrees -> dispatch (SetLayerOrientation (position, (withAngle (Angle.degree (degrees * 1.0<deg>))).editorOrientation))
                        | None -> ()),
                    SubPatchOptions.OnChangeOf (box (orientationId.value, label.value, angles.phi.value, angles.theta.value, angles.psi.value)))
            ] :> IView
        ]
    StackPanel.create [
        orientationId.automationAttribute<StackPanel> ()
        StackPanel.orientation Orientation.Horizontal
        StackPanel.spacing 4.0
        StackPanel.children (
            angleBox (ControlCaption "φ°") angles.phi (fun a -> { angles with phi = a })
            @ angleBox (ControlCaption "θ°") angles.theta (fun a -> { angles with theta = a })
            @ angleBox (ControlCaption "ψ°") angles.psi (fun a -> { angles with psi = a }))
    ] :> IView

/// One film-layer row (a top-level single or a group's nested cell slot): clickable
/// multi-select, the material name, the thickness readout cell, the orientation summary, and
/// — for anisotropic materials only — the inline orientation editor.
let private layerRowView
    (m : SampleEditorModel)
    (dispatch : SampleEditorMsg -> unit)
    (position : LayerPosition)
    (rowId : ConstructorUiId)
    (thicknessId : ConstructorUiId)
    (orientationId : ConstructorUiId)
    (margin : Avalonia.Thickness)
    (layer : SampleLayer) : IView =
    let selected = Set.contains position m.editor.selection
    let entryOpt = m.materials |> List.tryFind (fun e -> e.id = layer.materialId.materialId)
    let materialName =
        match entryOpt with
        | Some entry -> entry.name.caption
        | None -> ControlCaption $"unknown material %s{string layer.materialId.materialId.value}"
    let orientationEditor =
        match entryOpt with
        | Some entry when entry.isAnisotropic -> [ orientationEditorView dispatch position orientationId layer.orientation ]
        | Some _ | None -> []
    Border.create [
        rowId.automationAttribute<Border> ()
        Border.margin margin
        Border.background (brush (if selected then chosenBackground else idleBackground))
        Border.borderBrush (brush idleBorder)
        Border.borderThickness 1.0
        Border.cornerRadius (CornerRadius 3.0)
        Border.padding (Avalonia.Thickness(8.0, 3.0))
        Border.onPointerPressed ((fun e -> e.Handled <- true; dispatch (ToggleLayer position)), SubPatchOptions.OnChangeOf (box (rowId.value, selected)))
        Border.child (
            StackPanel.create [
                StackPanel.orientation Orientation.Horizontal
                StackPanel.spacing 10.0
                StackPanel.children (
                    [
                        TextBlock.create [
                            TextBlock.text materialName.value
                            TextBlock.width 230.0
                            TextBlock.verticalAlignment VerticalAlignment.Center
                        ] :> IView
                        TextBlock.create [
                            thicknessId.automationAttribute<TextBlock> ()
                            TextBlock.text layer.thickness.nanometreCaption.value
                            TextBlock.width 90.0
                            TextBlock.verticalAlignment VerticalAlignment.Center
                        ] :> IView
                        TextBlock.create [
                            TextBlock.text layer.orientation.editorCaption.value
                            TextBlock.foreground (brush hintColor)
                            TextBlock.verticalAlignment VerticalAlignment.Center
                        ] :> IView
                    ]
                    @ orientationEditor)
            ])
    ] :> IView

/// A period group's ONE collapsible super-row: the rotating-triangle expander, the cell
/// summary, and the inline repeat-count stepper (whole periods through `SetRepeatCount`).
let private groupRowView (m : SampleEditorModel) (dispatch : SampleEditorMsg -> unit) (groupIndex : FilmItemIndex) (group : PeriodGroup) : IView =
    let collapsed = Set.contains groupIndex m.collapsedGroups
    let triangle =
        TextBlock.create [
            TextBlock.text "▶"
            // The "rotating-triangle" affordance: 0° collapsed, 90° expanded.
            AttrBuilder<TextBlock>.CreateProperty<ITransform>(Visual.RenderTransformProperty, (RotateTransform (if collapsed then 0.0 else 90.0) :> ITransform), ValueNone)
            TextBlock.verticalAlignment VerticalAlignment.Center
        ] :> IView
    Border.create [
        (ConstructorUiId.sampleGroupRow groupIndex.uiPosition).automationAttribute<Border> ()
        Border.margin (Avalonia.Thickness(0.0, 0.0, 0.0, 3.0))
        Border.background (brush groupBackground)
        Border.borderBrush (brush idleBorder)
        Border.borderThickness 1.0
        Border.cornerRadius (CornerRadius 3.0)
        Border.padding (Avalonia.Thickness(8.0, 3.0))
        Border.child (
            StackPanel.create [
                StackPanel.orientation Orientation.Horizontal
                StackPanel.spacing 8.0
                StackPanel.children [
                    clickBoxView (ConstructorUiId.sampleGroupExpander groupIndex.uiPosition) idleBackground triangle (fun () -> dispatch (ToggleGroup groupIndex))
                    TextBlock.create [
                        TextBlock.text $"%d{List.length group.cell}-layer cell"
                        TextBlock.verticalAlignment VerticalAlignment.Center
                    ] :> IView
                    stepper
                        (ConstructorUiId.repeatCountStepperOfGroup groupIndex.uiPosition)
                        (ConstructorUiId.repeatCountStepperMinusOfGroup groupIndex.uiPosition)
                        (ConstructorUiId.repeatCountStepperPlusOfGroup groupIndex.uiPosition)
                        group.count
                        (fun step -> dispatch (GroupCountBy (groupIndex, step)))
                    TextBlock.create [
                        TextBlock.text $"× %d{group.count.value} periods = %d{group.count.value * List.length group.cell} films"
                        TextBlock.foreground (brush hintColor)
                        TextBlock.verticalAlignment VerticalAlignment.Center
                    ] :> IView
                ]
            ])
    ] :> IView

let private stackRows (m : SampleEditorModel) (dispatch : SampleEditorMsg -> unit) : IView list =
    // Each layer row is paired with its Choose material… verb in one horizontal slot (spec
    // 0038 step 019) — the verb sits beside the row, outside its toggling Border.
    let rowWithChooser (row : IView) (position : LayerPosition) (chooseId : ConstructorUiId) : IView =
        StackPanel.create [
            StackPanel.orientation Orientation.Horizontal
            StackPanel.children [ row; chooseMaterialVerb m dispatch position chooseId ]
        ] :> IView
    m.editor.structure.films
    |> List.mapi (fun i item ->
        let index = FilmItemIndex i
        let itemPosition = index.uiPosition
        match item with
        | SingleLayer layer ->
            [ rowWithChooser
                  (layerRowView m dispatch (AtSingleLayer index) (ConstructorUiId.sampleLayerRow itemPosition) (ConstructorUiId.sampleLayerThickness itemPosition) (ConstructorUiId.sampleLayerOrientation itemPosition) (Avalonia.Thickness(0.0, 0.0, 0.0, 3.0)) layer)
                  (AtSingleLayer index)
                  (ConstructorUiId.chooseMaterialButton itemPosition) ]
        | Repeated group ->
            let superRow = groupRowView m dispatch index group
            let cellRows =
                if Set.contains index m.collapsedGroups then []
                else
                    group.cell
                    |> List.mapi (fun j layer ->
                        let cellIndex = CellLayerIndex j
                        let cellPosition = cellIndex.uiPosition
                        rowWithChooser
                            (layerRowView m dispatch (AtCellLayer (index, cellIndex)) (ConstructorUiId.sampleCellLayerRow itemPosition cellPosition) (ConstructorUiId.sampleCellLayerThickness itemPosition cellPosition) (ConstructorUiId.sampleCellLayerOrientation itemPosition cellPosition) (Avalonia.Thickness(28.0, 0.0, 0.0, 3.0)) layer)
                            (AtCellLayer (index, cellIndex))
                            (ConstructorUiId.cellChooseMaterialButton itemPosition cellPosition))
            superRow :: cellRows)
    |> List.concat

// -- the bulk toolbar / QWOT / status / actions ----------------------------------------------

/// The substrate-plate and lower-half-space editor (spec 0033 gap G12): both
/// `SampleStructure` fields were previously invisible and uneditable. Each shows
/// its current material and a Clear (lower clears to vacuum). The substrate's Set…
/// verb opens the Materials window in Select state (spec 0040 Part D.4 step 011 —
/// so a film-less Plate can pick a substrate material without a film row); the
/// lower half-space keeps its `chosenMaterialAvailability`-gated Set-from-chosen button.
/// Clear is always available.
let private halfSpacesRow (m : SampleEditorModel) (dispatch : SampleEditorMsg -> unit) : IView =
    let substrateText =
        match m.editor.structure.substrate with
        | Some layer -> ControlCaption $"{(m.materialCaption layer.materialId.materialId).value} ({layer.thickness.nanometreCaption.value})"
        | None -> ControlCaption "none"
    let lowerText =
        match m.editor.structure.lower with
        | Some mvid -> m.materialCaption mvid.materialId
        | None -> ControlCaption "vacuum"
    let summary (id : ConstructorUiId) (text : ControlCaption) : IView =
        TextBlock.create [
            TextBlock.name id.value
            TextBlock.text text.value
            TextBlock.width 200.0
            TextBlock.verticalAlignment VerticalAlignment.Center
        ] :> IView
    StackPanel.create [
        StackPanel.orientation Orientation.Vertical
        StackPanel.spacing 2.0
        StackPanel.children [
            WrapPanel.create [
                WrapPanel.orientation Orientation.Horizontal
                WrapPanel.children [
                    (ControlCaption "Substrate plate:").sampleEditorLabel
                    summary ConstructorUiId.sampleSubstrateSummary substrateText
                    setSubstrateVerb m dispatch
                    verbButton ConstructorUiId.clearSubstrateButton (ControlCaption "Clear") idleBackground ControlOffered (fun () -> dispatch ClearSubstrateClicked)
                ]
            ] :> IView
            WrapPanel.create [
                WrapPanel.orientation Orientation.Horizontal
                WrapPanel.children [
                    (ControlCaption "Lower half-space:").sampleEditorLabel
                    summary ConstructorUiId.sampleLowerSummary lowerText
                    verbButton ConstructorUiId.setLowerButton (ControlCaption "Set from chosen") idleBackground m.chosenMaterialAvailability (fun () -> dispatch SetLowerClicked)
                    verbButton ConstructorUiId.clearLowerButton (ControlCaption "Clear (vacuum)") idleBackground ControlOffered (fun () -> dispatch ClearLowerClicked)
                ]
            ] :> IView
        ]
    ] :> IView

/// Selection verbs: select-by-material, clear, remove, move up/down, and the make-repeat-block
/// entry with its fold-count stepper (the mandated `RepeatCountStepper`). A WRAP panel: the
/// headless font metrics run a single row past the window edge, and an off-screen verb cannot
/// be clicked.
let private selectionToolbar (m : SampleEditorModel) (dispatch : SampleEditorMsg -> unit) : IView =
    let addAvailability =
        match m.materials with
        | [] -> ControlWithheld
        | _ :: _ -> ControlOffered
    WrapPanel.create [
        WrapPanel.orientation Orientation.Horizontal
        WrapPanel.children [
            verbButton ConstructorUiId.addLayerButton (ControlCaption "Add layer") chosenBackground addAvailability (fun () -> dispatch AddLayerClicked)
            verbButton ConstructorUiId.selectByMaterialButton (ControlCaption "Select by material") idleBackground m.chosenMaterialAvailability (fun () -> dispatch SelectByMaterialClicked)
            verbButton ConstructorUiId.clearSelectionButton (ControlCaption "Clear selection") idleBackground m.selectionAvailability (fun () -> dispatch ClearSelectionClicked)
            verbButton ConstructorUiId.removeSelectedLayersButton (ControlCaption "Remove") idleBackground m.selectionAvailability (fun () -> dispatch RemoveSelectedClicked)
            verbButton ConstructorUiId.moveSelectedUpButton (ControlCaption "Move up") idleBackground m.selectionAvailability (fun () -> dispatch MoveUpClicked)
            verbButton ConstructorUiId.moveSelectedDownButton (ControlCaption "Move down") idleBackground m.selectionAvailability (fun () -> dispatch MoveDownClicked)
            stepper ConstructorUiId.repeatCountStepper ConstructorUiId.repeatCountStepperMinus ConstructorUiId.repeatCountStepperPlus m.foldCount (FoldCountBy >> dispatch)
            verbButton ConstructorUiId.makeRepeatBlockButton (ControlCaption "Make repeat block") idleBackground m.selectionAvailability (fun () -> dispatch MakeRepeatBlockClicked)
        ]
    ] :> IView

/// Edit verbs over the selection: thickness (nm), material, and the toolbar orientation entry.
/// A WRAP panel (same off-screen-verb reason as the selection toolbar); each label+box pair is
/// one wrap item so a wrap never splits a label from its entry.
let private editToolbar (m : SampleEditorModel) (dispatch : SampleEditorMsg -> unit) : IView =
    let labelled (label : ControlCaption) (entry : IView) : IView =
        StackPanel.create [
            StackPanel.orientation Orientation.Horizontal
            StackPanel.spacing 4.0
            StackPanel.margin (Avalonia.Thickness(0.0, 0.0, 8.0, 0.0))
            StackPanel.children [ label.sampleEditorLabel; entry ]
        ] :> IView
    let angleBox (boxId : ConstructorUiId) (label : ControlCaption) (text : ControlCaption) (slot : AngleSlot) : IView =
        labelled label (
            TextBox.create [
                TextBox.name boxId.value
                TextBox.width 46.0
                TextBox.text text.value
                TextBox.onTextChanged (ControlCaption >> (fun entered -> dispatch (SetOrientationText (slot, entered))))
            ] :> IView)
    WrapPanel.create [
        WrapPanel.orientation Orientation.Horizontal
        WrapPanel.children [
            labelled (ControlCaption "Thickness (nm):") (
                TextBox.create [
                    TextBox.name ConstructorUiId.sampleLayerHeightBox.value
                    TextBox.width 70.0
                    TextBox.text m.thicknessText.value
                    TextBox.onTextChanged (ControlCaption >> SetThicknessText >> dispatch)
                ] :> IView)
            verbButton ConstructorUiId.setLayerHeightButton (ControlCaption "Set thickness") idleBackground m.selectionAvailability (fun () -> dispatch SetLayerHeightClicked)
            verbButton ConstructorUiId.setLayerMaterialButton (ControlCaption "Set material") idleBackground m.setMaterialAvailability (fun () -> dispatch SetLayerMaterialClicked)
            angleBox ConstructorUiId.sampleOrientationPhiBox (ControlCaption "φ°") m.phiText PhiSlot
            angleBox ConstructorUiId.sampleOrientationThetaBox (ControlCaption "θ°") m.thetaText ThetaSlot
            angleBox ConstructorUiId.sampleOrientationPsiBox (ControlCaption "ψ°") m.psiText PsiSlot
            verbButton ConstructorUiId.setOrientationOfSelectedButton (ControlCaption "Set orientation") idleBackground m.selectionAvailability (fun () -> dispatch SetOrientationClicked)
        ]
    ] :> IView

/// The optional QWOT entry: λ (nm) → the read-only derived t = λ/(4n) in canonical metres
/// (n from the chosen material at λ); Set-thickness applies it while it is valid.
let private qwotRow (m : SampleEditorModel) (dispatch : SampleEditorMsg -> unit) : IView =
    // spec 0033 gap G14.1: render the derived thickness in display nanometres (the unit
    // every other thickness readout uses), not raw metres.
    let derivedLabel =
        match m.qwotDerived with
        | Some t -> t.nanometreCaption
        | None -> ControlCaption "—"
    StackPanel.create [
        StackPanel.orientation Orientation.Horizontal
        StackPanel.spacing 6.0
        StackPanel.children [
            (ControlCaption "QWOT λ (nm):").sampleEditorLabel
            TextBox.create [
                TextBox.name ConstructorUiId.qwotEntryBox.value
                TextBox.width 70.0
                TextBox.text m.qwotText.value
                TextBox.onTextChanged (ControlCaption >> SetQwotText >> dispatch)
            ] :> IView
            (ControlCaption "→ t = λ/(4n) =").sampleEditorLabel
            TextBlock.create [
                TextBlock.name ConstructorUiId.qwotDerivedText.value
                TextBlock.text derivedLabel.value
                TextBlock.verticalAlignment VerticalAlignment.Center
            ] :> IView
        ]
    ] :> IView

let private statusRow (m : SampleEditorModel) : IView =
    TextBlock.create [
        TextBlock.name ConstructorUiId.sampleEditorStatus.value
        TextBlock.foreground (brush errorColor)
        TextBlock.text (
            match m.status with
            | Some reason -> reason.value
            | None -> "")
    ] :> IView

let private saveCancelRow (dispatch : SampleEditorMsg -> unit) : IView =
    StackPanel.create [
        StackPanel.orientation Orientation.Horizontal
        StackPanel.spacing 0.0
        StackPanel.children [
            actionButton ConstructorUiId.sampleEditorSaveButton (ControlCaption "Save") saveBackground (fun () -> dispatch SaveClicked)
            actionButton ConstructorUiId.sampleEditorCancelButton (ControlCaption "Cancel") cancelBackground (fun () -> dispatch CancelClicked)
        ]
    ] :> IView

/// The unsaved-edit exit confirm surface (spec 0038 step 033): shown in place of the Save/Cancel
/// row while a dirty editor is being closed. Discard changes (negative styling) closes without
/// saving; Keep editing (positive styling) returns to the editor — one row, distinct styling.
let private exitConfirmRow (dispatch : SampleEditorMsg -> unit) : IView =
    StackPanel.create [
        ConstructorUiId.sampleEditorExitConfirm.automationAttribute<StackPanel> ()
        StackPanel.orientation Orientation.Horizontal
        StackPanel.spacing 0.0
        StackPanel.children [
            TextBlock.create [
                TextBlock.text "Discard unsaved changes?"
                TextBlock.foreground (brush errorColor)
                TextBlock.verticalAlignment VerticalAlignment.Center
                TextBlock.margin (Avalonia.Thickness(0.0, 0.0, 12.0, 0.0))
            ] :> IView
            actionButton ConstructorUiId.sampleEditorDiscardButton (ControlCaption "Discard changes") cancelBackground (fun () -> dispatch DiscardConfirmed)
            actionButton ConstructorUiId.sampleEditorKeepEditingButton (ControlCaption "Keep editing") saveBackground (fun () -> dispatch KeepEditing)
        ]
    ] :> IView

/// The bottom action area: the normal Save/Cancel row, or — while `exit = ConfirmingDiscard` —
/// the discard confirm surface (spec 0038 step 033).
let private actionsRow (m : SampleEditorModel) (dispatch : SampleEditorMsg -> unit) : IView =
    match m.exit with
    | Editing -> saveCancelRow dispatch
    | ConfirmingDiscard -> exitConfirmRow dispatch

/// The whole editor: identity + facets on top (material picking moved to the per-layer Choose
/// material… verbs — spec 0038 step 019); the bulk toolbars, QWOT row, status line and the
/// Save/Cancel row pinned to the bottom; the stack table (with the expanded-films readout)
/// filling the centre.
let sampleEditorView (m : SampleEditorModel) (dispatch : SampleEditorMsg -> unit) : IView =
    DockPanel.create [
        DockPanel.children [
            Border.create [
                Border.dock Dock.Top
                Border.padding (Avalonia.Thickness(8.0))
                Border.child (
                    StackPanel.create [
                        StackPanel.orientation Orientation.Vertical
                        StackPanel.spacing 6.0
                        StackPanel.children [
                            nameRow m dispatch
                            descriptionRow m dispatch
                            substrateRow m dispatch
                            emissionRow m dispatch
                        ]
                    ])
            ]
            Border.create [ Border.dock Dock.Bottom; Border.padding (Avalonia.Thickness(8.0, 6.0)); Border.child (actionsRow m dispatch) ]
            Border.create [ Border.dock Dock.Bottom; Border.padding (Avalonia.Thickness(8.0, 0.0)); Border.child (statusRow m) ]
            Border.create [ Border.dock Dock.Bottom; Border.padding (Avalonia.Thickness(8.0, 2.0)); Border.child (qwotRow m dispatch) ]
            Border.create [ Border.dock Dock.Bottom; Border.padding (Avalonia.Thickness(8.0, 2.0)); Border.child (halfSpacesRow m dispatch) ]
            Border.create [ Border.dock Dock.Bottom; Border.padding (Avalonia.Thickness(8.0, 2.0)); Border.child (editToolbar m dispatch) ]
            Border.create [ Border.dock Dock.Bottom; Border.padding (Avalonia.Thickness(8.0, 2.0)); Border.child (selectionToolbar m dispatch) ]
            Border.create [
                Border.padding (Avalonia.Thickness(8.0, 4.0))
                Border.child (
                    DockPanel.create [
                        DockPanel.children [
                            // The expanded-films readout.
                            Border.create [
                                Border.dock Dock.Top
                                Border.child (
                                    StackPanel.create [
                                        StackPanel.orientation Orientation.Horizontal
                                        StackPanel.spacing 6.0
                                        StackPanel.margin (Avalonia.Thickness(0.0, 0.0, 0.0, 4.0))
                                        StackPanel.children [
                                            (ControlCaption "Films (expanded):").sampleEditorLabel
                                            TextBlock.create [
                                                TextBlock.name ConstructorUiId.sampleFilmsCount.value
                                                TextBlock.text (string m.filmsCount)
                                                TextBlock.verticalAlignment VerticalAlignment.Center
                                            ] :> IView
                                        ]
                                    ])
                            ]
                            // Spec 0046: the layer table's rows are fixed-width cells, so in a narrow window
                            // the table scrolls sideways too instead of clipping its right-hand columns.
                            ScrollViewer.create [
                                ScrollViewer.horizontalScrollBarVisibility Avalonia.Controls.Primitives.ScrollBarVisibility.Auto
                                ScrollViewer.content (
                                    StackPanel.create [
                                        StackPanel.name ConstructorUiId.sampleStackTable.value
                                        StackPanel.orientation Orientation.Vertical
                                        StackPanel.children (stackRows m dispatch)
                                    ])
                            ] :> IView
                        ]
                    ])
            ]
        ]
    ] :> IView
