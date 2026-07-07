/// Spec 0033 (022) — the Sample editor view (UICOMP_XDUO_0003): the pure MVU model and the
/// FuncUI projection behind `SampleEditorWindow`. The stack table sits over the step-21 Domain
/// `SampleStackEditor` — every bulk-toolbar verb dispatches a `SampleStackMsg` through
/// `applySampleStackMsg`, and each `Repeated` period group renders as ONE collapsible super-row
/// (a rotating-triangle expander) with an inline repeat-count stepper and its unit-cell layers
/// nested beneath. Material choice is by `MaterialId` over the entries the window resolves ONCE
/// from `MaterialProxy.listMaterials`; the per-layer orientation editor is INCLUDED only for
/// anisotropic materials (absent — not greyed — for isotropic ones); the optional QWOT entry
/// derives the physical thickness t = λ/(4n) read-only into canonical metres (the DBR λ/4
/// precedent, `Templates.dbrCell` / `Templates.dbrPeriods`, OpticalConstructor.Ui/Templates.fs).
/// Save persists through `SampleProxy` — `addSample` for a new sample (minting
/// `SampleId.create`), `updateSample` for an existing one — via the injected
/// `SampleEditorContext` (the functional-proxy/Context seam; tests substitute stubs); Cancel
/// discards. Pure: `update` only reaches IO through the context's proxy fields.
module OpticalConstructor.TestWindows.SampleEditorView

open System
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
open Berreman.Media
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.SampleStackEditor

/// Stable intent-named automation ids (CLAUDE.md UI guidance): the thirteen slice-mandated ids
/// as `[<Literal>]`s plus the derived per-row / per-group id families (prefixed so they cannot
/// collide — the SampleLibraryControls convention).
[<RequireQualifiedAccess>]
module UiIds =
    [<Literal>]
    let window = "SampleEditorWindow"
    [<Literal>]
    let nameBox = "SampleNameBox"
    [<Literal>]
    let addLayerButton = "AddLayerButton"
    [<Literal>]
    let makeRepeatBlockButton = "MakeRepeatBlockButton"
    [<Literal>]
    let selectByMaterialButton = "SelectByMaterialButton"
    [<Literal>]
    let setLayerHeightButton = "SetLayerHeightButton"
    [<Literal>]
    let setLayerMaterialButton = "SetLayerMaterialButton"
    [<Literal>]
    let setOrientationOfSelectedButton = "SetOrientationOfSelectedButton"
    [<Literal>]
    let removeSelectedLayersButton = "RemoveSelectedLayersButton"
    [<Literal>]
    let repeatCountStepper = "RepeatCountStepper"
    [<Literal>]
    let qwotEntryBox = "QwotEntryBox"
    [<Literal>]
    let saveButton = "SampleEditorSaveButton"
    [<Literal>]
    let cancelButton = "SampleEditorCancelButton"
    // Supporting fixed ids (not slice-mandated, same naming discipline).
    [<Literal>]
    let descriptionBox = "SampleDescriptionBox"
    [<Literal>]
    let layerHeightBox = "SampleLayerHeightBox"
    [<Literal>]
    let phiBox = "SampleOrientationPhiBox"
    [<Literal>]
    let thetaBox = "SampleOrientationThetaBox"
    [<Literal>]
    let psiBox = "SampleOrientationPsiBox"
    [<Literal>]
    let clearSelectionButton = "ClearSelectionButton"
    [<Literal>]
    let moveUpButton = "MoveSelectedUpButton"
    [<Literal>]
    let moveDownButton = "MoveSelectedDownButton"
    [<Literal>]
    let qwotDerivedText = "QwotDerivedText"
    [<Literal>]
    let filmsCount = "SampleFilmsCount"
    // Substrate / lower half-space surfaces (spec 0033 gap G12).
    [<Literal>]
    let substrateSummary = "SampleSubstrateSummary"
    [<Literal>]
    let lowerSummary = "SampleLowerSummary"
    [<Literal>]
    let setSubstrateButton = "SetSubstrateButton"
    [<Literal>]
    let clearSubstrateButton = "ClearSubstrateButton"
    [<Literal>]
    let setLowerButton = "SetLowerButton"
    [<Literal>]
    let clearLowerButton = "ClearLowerButton"
    [<Literal>]
    let statusText = "SampleEditorStatus"
    [<Literal>]
    let materialPicker = "SampleMaterialPicker"
    [<Literal>]
    let stackTable = "SampleStackTable"
    [<Literal>]
    let repeatCountStepperPlus = "RepeatCountStepperPlus"
    [<Literal>]
    let repeatCountStepperMinus = "RepeatCountStepperMinus"
    /// The toolbar fold stepper carries the mandated literal; each period group's INLINE
    /// stepper carries the group-indexed member of the same family.
    let groupStepper (groupIndex : int) : string = $"RepeatCountStepper_%d{groupIndex}"
    let groupStepperPlus (groupIndex : int) : string = $"RepeatCountStepperPlus_%d{groupIndex}"
    let groupStepperMinus (groupIndex : int) : string = $"RepeatCountStepperMinus_%d{groupIndex}"
    /// A top-level single layer's row / a period group's nested cell-layer row.
    let layerRow (itemIndex : int) : string = $"SampleLayerRow_%d{itemIndex}"
    let cellLayerRow (itemIndex : int) (cellIndex : int) : string = $"SampleLayerRow_%d{itemIndex}_%d{cellIndex}"
    /// A period group's collapsible super-row and its rotating-triangle expander.
    let groupRow (itemIndex : int) : string = $"SampleGroupRow_%d{itemIndex}"
    let groupExpander (itemIndex : int) : string = $"SampleGroupExpander_%d{itemIndex}"
    /// A row's thickness readout cell (what the bulk set-thickness acceptance observes).
    let layerThickness (itemIndex : int) : string = $"SampleLayerThickness_%d{itemIndex}"
    let cellLayerThickness (itemIndex : int) (cellIndex : int) : string = $"SampleLayerThickness_%d{itemIndex}_%d{cellIndex}"
    /// A row's per-layer orientation editor (present ONLY for anisotropic materials).
    let layerOrientation (itemIndex : int) : string = $"SampleLayerOrientation_%d{itemIndex}"
    let cellLayerOrientation (itemIndex : int) (cellIndex : int) : string = $"SampleLayerOrientation_%d{itemIndex}_%d{cellIndex}"
    /// A material option's clickable id, by the MaterialId's Guid string form.
    let materialOption (materialId : string) : string = "SampleMaterialOption_" + materialId
    /// A SubstrateKind facet option's clickable id, by its stable code.
    let substrateOption (code : string) : string = "SampleSubstrateKind_" + code

/// What Save targets: a brand-new sample (mint a fresh `SampleId`) or an existing one (keep
/// its id and update in place). A DU, not a naked bool.
type EditorTarget =
    | NewSample
    | ExistingSample of SampleId

/// The window's IO seam (the functional-proxy Context convention): the samples write-seam the
/// Save verb persists through, plus the host's close request (the window passes `this.Close`;
/// tests substitute recording stubs). Function-valued fields have no structural equality, so
/// the context compares by reference — the model holding it keeps its equality.
[<ReferenceEquality>]
type SampleEditorContext =
    {
        samples : SampleProxy
        requestClose : unit -> unit
    }

/// Which toolbar orientation-angle box a text edit targets.
type AngleSlot =
    | PhiSlot
    | ThetaSlot
    | PsiSlot

/// The editor's model: the sample identity facets (name / description / `SubstrateKind`), the
/// step-21 stack edit state, the material choices the window resolved from
/// `MaterialProxy.listMaterials`, and the transient entry texts. Reference-compared: a
/// `MaterialEntry`'s engine properties carry dispersion FUNCTION cases (no structural
/// equality), and `update` returns a fresh record anyway — so the Elmish equality gate sees
/// every dispatch as a change and re-renders, which is exactly this window's contract.
[<ReferenceEquality>]
type Model =
    {
        context : SampleEditorContext
        target : EditorTarget
        name : string
        description : string
        substrate : SubstrateKind
        editor : SampleStackEditState
        materials : MaterialEntry list
        chosenMaterial : MaterialId option
        /// Film indices of the period groups whose super-row is collapsed (expanded default).
        collapsedGroups : Set<int>
        thicknessText : string
        qwotText : string
        phiText : string
        thetaText : string
        psiText : string
        /// The toolbar stepper's fold count `MakeRepeatBlock` uses (always >= 1).
        foldCount : int
        /// The last typed-error reason (or entry-validation hint) surfaced to the user.
        status : string option
    }

type Msg =
    | SetName of string
    | SetDescription of string
    | SetSubstrate of SubstrateKind
    | ChooseMaterial of MaterialId
    /// A row click: adds an unselected position to the multi-selection, removes a selected one.
    | ToggleLayer of LayerPosition
    | ClearSelectionClicked
    /// The super-row's rotating-triangle expander.
    | ToggleGroup of int
    | AddLayerClicked
    | SelectByMaterialClicked
    | SetThicknessText of string
    | SetQwotText of string
    | SetLayerHeightClicked
    | SetLayerMaterialClicked
    | SetOrientationText of AngleSlot * string
    | SetOrientationClicked
    /// The per-layer orientation editor (degrees; only anisotropic layers render one).
    | SetLayerOrientation of LayerPosition * float * float * float
    | RemoveSelectedClicked
    | MoveUpClicked
    | MoveDownClicked
    /// The toolbar fold-count stepper (clamped at 1).
    | FoldCountBy of int
    | MakeRepeatBlockClicked
    /// A group's inline stepper: resize the `Repeated` group at a films index by whole periods.
    | GroupCountBy of int * int
    /// Set the thick substrate plate to the chosen material (spec 0033 gap G12).
    | SetSubstrateClicked
    /// Clear the substrate plate (spec 0033 gap G12).
    | ClearSubstrateClicked
    /// Set the lower half-space to the chosen material (spec 0033 gap G12).
    | SetLowerClicked
    /// Clear the lower half-space back to vacuum (spec 0033 gap G12).
    | ClearLowerClicked
    | SaveClicked
    | CancelClicked

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

let private anisotropyTolerance : float = 1.0e-12

/// Whether an entry's material is anisotropic — i.e. its crystal orientation matters. True
/// when the eps or mu tensor (evaluated at the reference wavelength through the engine
/// `getProperties` path) has any off-diagonal component or unequal diagonal, or when ANY rho
/// component is nonzero (gyrotropy is orientation-sensitive; the isotropic default rho is the
/// zero matrix). The per-layer orientation editor renders ONLY for these entries.
let isAnisotropicEntry (entry : MaterialEntry) : bool =
    let p = entry.properties.getProperties referenceWaveLength
    let offDiagonalOrUnequalDiagonal (get : int -> int -> Complex) : bool =
        let offDiagonal =
            [ (0, 1); (0, 2); (1, 0); (1, 2); (2, 0); (2, 1) ]
            |> List.exists (fun (i, j) -> (get i j).Magnitude > anisotropyTolerance)
        let unequalDiagonal =
            (get 0 0 - get 1 1).Magnitude > anisotropyTolerance
            || (get 1 1 - get 2 2).Magnitude > anisotropyTolerance
        offDiagonal || unequalDiagonal
    let anyNonZero (get : int -> int -> Complex) : bool =
        [ for i in 0 .. 2 do for j in 0 .. 2 -> (i, j) ]
        |> List.exists (fun (i, j) -> (get i j).Magnitude > anisotropyTolerance)
    offDiagonalOrUnequalDiagonal (fun i j -> p.eps.[i, j])
    || offDiagonalOrUnequalDiagonal (fun i j -> p.mu.[i, j])
    || anyNonZero (fun i j -> p.rho.[i, j])

/// n = Re[√ε₁₁] at the wavelength, through the engine's own eps dispersion path (the
/// NkDispersionChart / MaterialImport.exportCsv extraction — no n formula re-derived).
let refractionIndexAt (entry : MaterialEntry) (w : WaveLength) : float =
    (Complex.Sqrt (entry.properties.epsWithDisp.getEps w).[0, 0]).Real

/// The quarter-wave optical thickness: t = λ/(4n), stored canonical-SI through the engine
/// `Thickness.nm` constructor (the DBR template λ/4 precedent, Templates.fs:103,106).
let qwotThickness (lambdaNm : float) (n : float) : Thickness =
    Thickness.nm (lambdaNm / (4.0 * n) * 1.0<nm>)

let private parseFloat (s : string) : float option =
    match Double.TryParse(s, NumberStyles.Float, CultureInfo.InvariantCulture) with
    | true, v -> Some v
    | _ -> None

let private chosenEntry (m : Model) : MaterialEntry option =
    match m.chosenMaterial with
    | Some id -> m.materials |> List.tryFind (fun e -> e.id = id)
    | None -> None

/// The QWOT-derived physical thickness, when the entry text parses to a positive wavelength
/// (nm) and a material is chosen to take n from. `None` otherwise — the readout shows a dash
/// and the set-thickness verb falls back to the plain nm entry.
let qwotDerived (m : Model) : Thickness option =
    match chosenEntry m, parseFloat m.qwotText with
    | Some entry, Some lambdaNm when lambdaNm > 0.0 ->
        let n = refractionIndexAt entry (WaveLength.nm (lambdaNm * 1.0<nm>))
        if n > 0.0 then Some (qwotThickness lambdaNm n) else None
    | _, _ -> None

/// A thickness readout in display nanometres (`∞` for the semi-infinite case).
let thicknessLabel (t : Thickness) : string =
    match t with
    | Thickness meters -> $"%g{meters / nmToMeter / oneNanometer} nm"
    | Infinity -> "∞"

/// The flattened film count — each `Repeated` group expands to count × cell (the structure
/// readout the "2-layer selection repeated K times shows 2*K films" acceptance observes).
let filmsCount (m : Model) : int =
    m.editor.structure.expandedFilms |> List.length

/// A `SubstrateKind` facet option's stable code (the derived UiIds key).
let substrateCode (kind : SubstrateKind) : string =
    match kind with
    | ThinFilm -> "ThinFilm"
    | Plate -> "Plate"
    | Wedge -> "Wedge"

let private substrateLabel (kind : SubstrateKind) : string =
    match kind with
    | ThinFilm -> "Thin film"
    | Plate -> "Plate"
    | Wedge -> "Wedge"

let private orientationDegrees (o : CrystalOrientation) : float * float * float =
    match o with
    | PrimaryAxes -> (0.0, 0.0, 0.0)
    | EulerRotation (_, phi, theta, psi) -> (phi.degrees, theta.degrees, psi.degrees)

/// Degrees → orientation: all-zero angles mean the material's own principal axes (the
/// identity is DATA, not an EulerRotation carrying zeros).
let private orientationOf (phi : float) (theta : float) (psi : float) : CrystalOrientation =
    if phi = 0.0 && theta = 0.0 && psi = 0.0 then PrimaryAxes
    else EulerRotation (defaultConvention, Angle.degree phi, Angle.degree theta, Angle.degree psi)

let private orientationLabel (o : CrystalOrientation) : string =
    match o with
    | PrimaryAxes -> "primary axes"
    | EulerRotation (_, phi, theta, psi) -> $"φ=%g{phi.degrees}° θ=%g{theta.degrees}° ψ=%g{psi.degrees}°"

// ---------------------------------------------------------------------------
// init / toSample / update (pure — IO only through the context's proxy fields).
// ---------------------------------------------------------------------------

let private emptyStructure : SampleStructure =
    {
        films = []
        substrate = None
        lower = None
    }

let init (context : SampleEditorContext) (materials : MaterialEntry list) (existing : Sample option) : Model =
    let target, name, description, substrate, structure =
        match existing with
        | Some s -> (ExistingSample s.id, s.name, s.description, s.substrate, s.structure)
        | None -> (NewSample, "", "", ThinFilm, emptyStructure)
    {
        context = context
        target = target
        name = name
        description = description
        substrate = substrate
        editor = SampleStackEditState.ofStructure structure
        materials = materials
        chosenMaterial = None
        collapsedGroups = Set.empty
        thicknessText = ""
        qwotText = ""
        phiText = ""
        thetaText = ""
        psiText = ""
        foldCount = 2
        status = None
    }

/// The sample the model currently denotes, under the given id (Save chooses the id by target).
let toSample (id : SampleId) (m : Model) : Sample =
    {
        id = id
        name = m.name
        structure = m.editor.structure
        substrate = m.substrate
        description = m.description
    }

let private stackErrorReason (e : SampleStackEditError) : string =
    match e with
    | InvalidRepeatCount reason
    | SelectionNotFoldable reason
    | NotARepeatGroup reason -> reason

let private sampleErrorReason (e : SampleError) : string =
    match e with
    | UnknownSampleId reason
    | DuplicateSampleId reason
    | InvalidSample reason -> reason

/// Route one step-21 editor message; a typed rejection surfaces its reason as the status.
let private applyStack (msg : SampleStackMsg) (m : Model) : Model =
    match applySampleStackMsg msg m.editor with
    | Ok editor -> { m with editor = editor; status = None }
    | Error e -> { m with status = Some (stackErrorReason e) }

let update (msg : Msg) (m : Model) : Model =
    match msg with
    | SetName s -> { m with name = s }
    | SetDescription s -> { m with description = s }
    | SetSubstrate kind -> { m with substrate = kind }
    | ChooseMaterial id -> { m with chosenMaterial = Some id }
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
            applyStack (AddLayer { materialId = id; thickness = defaultLayerThickness; orientation = PrimaryAxes }) m
        | None -> { m with status = Some "no materials are available to add a layer from" }
    | SelectByMaterialClicked ->
        match m.chosenMaterial with
        | Some id -> applyStack (SelectByMaterial id) m
        | None -> { m with status = Some "choose a material to select by" }
    | SetThicknessText s -> { m with thicknessText = s }
    | SetQwotText s -> { m with qwotText = s }
    | SetLayerHeightClicked ->
        // A valid QWOT derivation wins; otherwise the plain nm entry applies.
        match qwotDerived m with
        | Some t -> applyStack (SetThicknessOfSelected t) m
        | None ->
            match parseFloat m.thicknessText with
            | Some v when v > 0.0 -> applyStack (SetThicknessOfSelected (Thickness.nm (v * 1.0<nm>))) m
            | Some _ | None -> { m with status = Some "enter a positive thickness in nm (or a QWOT wavelength with a chosen material)" }
    | SetLayerMaterialClicked ->
        match m.chosenMaterial with
        | Some id -> applyStack (SetMaterialOfSelected id) m
        | None -> { m with status = Some "choose a material to set" }
    | SetOrientationText (slot, s) ->
        match slot with
        | PhiSlot -> { m with phiText = s }
        | ThetaSlot -> { m with thetaText = s }
        | PsiSlot -> { m with psiText = s }
    | SetOrientationClicked ->
        let angleOf (s : string) : float option =
            if String.IsNullOrWhiteSpace s then Some 0.0 else parseFloat s
        match angleOf m.phiText, angleOf m.thetaText, angleOf m.psiText with
        | Some phi, Some theta, Some psi -> applyStack (SetOrientationOfSelected (orientationOf phi theta psi)) m
        | _, _, _ -> { m with status = Some "the orientation angles must be numbers (degrees)" }
    | SetLayerOrientation (position, phi, theta, psi) ->
        // One layer's editor: apply the selection-shaped Domain transform to JUST this
        // position, then restore the user's multi-selection (the transform never rejects).
        let single = { m.editor with selection = Set.ofList [ position ] }
        match applySampleStackMsg (SetOrientationOfSelected (orientationOf phi theta psi)) single with
        | Ok next -> { m with editor = { next with selection = m.editor.selection }; status = None }
        | Error e -> { m with status = Some (stackErrorReason e) }
    | RemoveSelectedClicked -> applyStack RemoveSelected m
    | MoveUpClicked -> applyStack MoveSelectedUp m
    | MoveDownClicked -> applyStack MoveSelectedDown m
    | FoldCountBy delta -> { m with foldCount = max 1 (m.foldCount + delta) }
    | MakeRepeatBlockClicked -> applyStack (MakeRepeatBlock m.foldCount) m
    | GroupCountBy (groupIndex, delta) ->
        match List.tryItem groupIndex m.editor.structure.films with
        | Some (Repeated g) -> applyStack (SetRepeatCount (groupIndex, g.count + delta)) m
        | Some (SingleLayer _) | None -> { m with status = Some $"films item %d{groupIndex} is not a repeat group" }
    | SetSubstrateClicked ->
        // Qualify the Domain case — the view `Msg` also has a `SetSubstrate` (the geometry
        // facet), so the bare name would resolve to the wrong DU.
        match m.chosenMaterial with
        | Some id -> applyStack (SampleStackMsg.SetSubstrate (Some { materialId = id; thickness = defaultLayerThickness; orientation = PrimaryAxes })) m
        | None -> { m with status = Some "choose a material to set as the substrate plate" }
    | ClearSubstrateClicked -> applyStack (SampleStackMsg.SetSubstrate None) m
    | SetLowerClicked ->
        match m.chosenMaterial with
        | Some id -> applyStack (SampleStackMsg.SetLower (Some id)) m
        | None -> { m with status = Some "choose a material to set as the lower half-space" }
    | ClearLowerClicked -> applyStack (SampleStackMsg.SetLower None) m
    | SaveClicked ->
        let saved =
            match m.target with
            | NewSample -> m.context.samples.addSample (toSample (newSampleId ()) m)
            | ExistingSample id -> m.context.samples.updateSample (toSample id m)
        match saved with
        | Ok () ->
            m.context.requestClose ()
            { m with status = None }
        | Error e -> { m with status = Some (sampleErrorReason e) }
    | CancelClicked ->
        m.context.requestClose ()
        m

// ---------------------------------------------------------------------------
// The FuncUI view. Styling matches the sibling bars' idle/chosen boxes; every control in a
// variable-membership list carries a mutable AutomationId, never `Name` (a styled control
// cannot be renamed when FuncUI recycles it — the SampleLibraryControls precedent, and this
// window re-renders on every Elmish message).
// ---------------------------------------------------------------------------

let private color (r : int) (g : int) (b : int) : Color = Color.FromRgb(byte r, byte g, byte b)
let private brush (c : Color) : IBrush = SolidColorBrush(c) :> IBrush
let private idleBackground = color 232 232 232
let private chosenBackground = color 150 185 235
let private groupBackground = color 214 224 238
let private saveBackground = color 186 224 186
let private cancelBackground = color 236 202 202
let private idleBorder = color 120 120 120
let private hintColor = color 110 110 110
let private errorColor = color 165 40 40

/// Avalonia layout thicknesses, qualified: the bare `Thickness` in this file is the DOMAIN
/// layer thickness (Berreman.Media), opened after Avalonia.
let private thick (uniform : float) : Avalonia.Thickness = Avalonia.Thickness(uniform)
let private thickLR (horizontal : float) (vertical : float) : Avalonia.Thickness = Avalonia.Thickness(horizontal, vertical)
let private thickOf (l : float) (t : float) (r : float) (b : float) : Avalonia.Thickness = Avalonia.Thickness(l, t, r, b)

/// Set `AutomationProperties.AutomationId` (freely mutable, unlike `Control.Name`) through
/// FuncUI's attr builder — the id survives FuncUI recycling a control onto another item's slot.
let private automationId<'t when 't :> Control> (autoId : string) : IAttr<'t> =
    AttrBuilder<'t>.CreateProperty<string>(AutomationProperties.AutomationIdProperty, autoId, ValueNone)

/// The expander triangle's literal rotation (the "rotating-triangle" affordance: 0° collapsed,
/// 90° expanded).
let private renderRotation (degrees : float) : IAttr<TextBlock> =
    AttrBuilder<TextBlock>.CreateProperty<ITransform>(Visual.RenderTransformProperty, (RotateTransform degrees :> ITransform), ValueNone)

/// A clickable, styled box, highlighted when chosen. `e.Handled <- true` drops FuncUI's
/// duplicate Tunnel|Bubble pass; re-subscribe when the id or the highlight changes.
let private clickBoxView (autoId : string) (chosen : bool) (child : IView) (onClick : unit -> unit) : IView =
    Border.create [
        automationId autoId
        Border.background (brush (if chosen then chosenBackground else idleBackground))
        Border.borderBrush (brush idleBorder)
        Border.borderThickness 1.0
        Border.cornerRadius (CornerRadius 3.0)
        Border.padding (thickLR 10.0 4.0)
        Border.margin (thickOf 0.0 0.0 6.0 4.0)
        Border.verticalAlignment VerticalAlignment.Center
        Border.child child
        Border.onPointerPressed ((fun e -> e.Handled <- true; onClick ()), SubPatchOptions.OnChangeOf (box (autoId, chosen)))
    ] :> IView

let private clickBox (autoId : string) (label : string) (chosen : bool) (onClick : unit -> unit) : IView =
    clickBoxView autoId chosen (TextBlock.create [ TextBlock.text label ] :> IView) onClick

/// A toolbar verb button — disabled verbs are present but inert (a disabled Border dispatches
/// no pointer event).
let private verbButton (autoId : string) (label : string) (accent : bool) (enabled : bool) (onClick : unit -> unit) : IView =
    Border.create [
        automationId autoId
        Border.isEnabled enabled
        Border.opacity (if enabled then 1.0 else 0.4)
        Border.background (brush (if accent then chosenBackground else idleBackground))
        Border.borderBrush (brush idleBorder)
        Border.borderThickness 1.0
        Border.cornerRadius (CornerRadius 3.0)
        Border.padding (thickLR 12.0 5.0)
        Border.margin (thickOf 0.0 0.0 8.0 0.0)
        Border.verticalAlignment VerticalAlignment.Center
        Border.child (TextBlock.create [ TextBlock.text label ])
        Border.onPointerPressed ((fun e -> e.Handled <- true; onClick ()), SubPatchOptions.OnChangeOf (box (autoId, enabled)))
    ] :> IView

/// The Save / Cancel actions (one row, distinct positive/negative styling).
let private actionButton (autoId : string) (label : string) (background : Color) (onClick : unit -> unit) : IView =
    Border.create [
        automationId autoId
        Border.background (brush background)
        Border.borderBrush (brush idleBorder)
        Border.borderThickness 1.0
        Border.cornerRadius (CornerRadius 3.0)
        Border.padding (thickLR 22.0 6.0)
        Border.margin (thickOf 0.0 0.0 10.0 0.0)
        Border.verticalAlignment VerticalAlignment.Center
        Border.child (TextBlock.create [ TextBlock.text label ])
        Border.onPointerPressed ((fun e -> e.Handled <- true; onClick ()), SubPatchOptions.OnChangeOf (box autoId))
    ] :> IView

/// A −/count/+ stepper (the toolbar fold count and each group's inline period count).
let private stepper (containerId : string) (minusId : string) (plusId : string) (count : int) (stepBy : int -> unit) : IView =
    StackPanel.create [
        automationId containerId
        StackPanel.orientation Orientation.Horizontal
        StackPanel.spacing 2.0
        StackPanel.children [
            clickBox minusId "−" false (fun () -> stepBy (-1))
            TextBlock.create [
                TextBlock.text (string count)
                TextBlock.verticalAlignment VerticalAlignment.Center
                TextBlock.margin (thickLR 4.0 0.0)
            ] :> IView
            clickBox plusId "+" false (fun () -> stepBy 1)
        ]
    ] :> IView

let private labelBlock (label : string) : IView =
    TextBlock.create [ TextBlock.text label; TextBlock.verticalAlignment VerticalAlignment.Center ] :> IView

// -- the identity / facet rows -------------------------------------------------------------

let private nameRow (m : Model) (dispatch : Msg -> unit) : IView =
    StackPanel.create [
        StackPanel.orientation Orientation.Horizontal
        StackPanel.spacing 6.0
        StackPanel.children [
            labelBlock "Name:"
            TextBox.create [
                TextBox.name UiIds.nameBox
                TextBox.width 340.0
                TextBox.text m.name
                TextBox.onTextChanged (SetName >> dispatch)
            ] :> IView
        ]
    ] :> IView

let private descriptionRow (m : Model) (dispatch : Msg -> unit) : IView =
    StackPanel.create [
        StackPanel.orientation Orientation.Horizontal
        StackPanel.spacing 6.0
        StackPanel.children [
            labelBlock "Description:"
            TextBox.create [
                TextBox.name UiIds.descriptionBox
                TextBox.width 620.0
                TextBox.text m.description
                TextBox.onTextChanged (SetDescription >> dispatch)
            ] :> IView
        ]
    ] :> IView

let private substrateRow (m : Model) (dispatch : Msg -> unit) : IView =
    StackPanel.create [
        StackPanel.orientation Orientation.Horizontal
        StackPanel.spacing 6.0
        StackPanel.children (
            labelBlock "Geometry:"
            :: ([ ThinFilm; Plate; Wedge ]
                |> List.map (fun kind ->
                    clickBox (UiIds.substrateOption (substrateCode kind)) (substrateLabel kind) (m.substrate = kind) (fun () -> dispatch (SetSubstrate kind)))))
    ] :> IView

/// The picker stacks the label ABOVE the wrap panel so the panel is measured at the window's
/// finite width and actually wraps — inside a horizontal StackPanel it would be offered
/// infinite width and run every option off-screen (found by the headless click proofs).
let private materialRow (m : Model) (dispatch : Msg -> unit) : IView =
    StackPanel.create [
        StackPanel.orientation Orientation.Vertical
        StackPanel.spacing 2.0
        StackPanel.children [
            labelBlock "Material:"
            WrapPanel.create [
                WrapPanel.name UiIds.materialPicker
                WrapPanel.orientation Orientation.Horizontal
                WrapPanel.children (
                    m.materials
                    |> List.map (fun entry ->
                        let idString = string entry.id.value
                        clickBox (UiIds.materialOption idString) entry.name (m.chosenMaterial = Some entry.id) (fun () -> dispatch (ChooseMaterial entry.id))))
            ] :> IView
        ]
    ] :> IView

// -- the stack table -------------------------------------------------------------------------

/// One layer's inline orientation editor (φ/θ/ψ in degrees) — rendered ONLY when the layer's
/// material is anisotropic; an unparsable entry dispatches nothing.
let private orientationEditorView (dispatch : Msg -> unit) (position : LayerPosition) (orientationId : string) (orientation : CrystalOrientation) : IView =
    let phi, theta, psi = orientationDegrees orientation
    let angleBox (label : string) (current : float) (toMsg : float -> Msg) : IView list =
        [
            labelBlock label
            TextBox.create [
                TextBox.width 46.0
                TextBox.text $"%g{current}"
                TextBox.onTextChanged (
                    (fun s ->
                        match parseFloat s with
                        | Some v -> dispatch (toMsg v)
                        | None -> ()),
                    SubPatchOptions.OnChangeOf (box (orientationId, label, phi, theta, psi)))
            ] :> IView
        ]
    StackPanel.create [
        automationId orientationId
        StackPanel.orientation Orientation.Horizontal
        StackPanel.spacing 4.0
        StackPanel.children (
            angleBox "φ°" phi (fun v -> SetLayerOrientation (position, v, theta, psi))
            @ angleBox "θ°" theta (fun v -> SetLayerOrientation (position, phi, v, psi))
            @ angleBox "ψ°" psi (fun v -> SetLayerOrientation (position, phi, theta, v)))
    ] :> IView

/// One film-layer row (a top-level single or a group's nested cell slot): clickable
/// multi-select, the material name, the thickness readout cell, the orientation summary, and
/// — for anisotropic materials only — the inline orientation editor.
let private layerRowView
    (m : Model)
    (dispatch : Msg -> unit)
    (position : LayerPosition)
    (rowId : string)
    (thicknessId : string)
    (orientationId : string)
    (indent : float)
    (layer : SampleLayer) : IView =
    let selected = Set.contains position m.editor.selection
    let entryOpt = m.materials |> List.tryFind (fun e -> e.id = layer.materialId)
    let materialName =
        match entryOpt with
        | Some entry -> entry.name
        | None -> $"unknown material %s{string layer.materialId.value}"
    let orientationEditor =
        match entryOpt with
        | Some entry when isAnisotropicEntry entry -> [ orientationEditorView dispatch position orientationId layer.orientation ]
        | Some _ | None -> []
    Border.create [
        automationId rowId
        Border.margin (thickOf indent 0.0 0.0 3.0)
        Border.background (brush (if selected then chosenBackground else idleBackground))
        Border.borderBrush (brush idleBorder)
        Border.borderThickness 1.0
        Border.cornerRadius (CornerRadius 3.0)
        Border.padding (thickLR 8.0 3.0)
        Border.onPointerPressed ((fun e -> e.Handled <- true; dispatch (ToggleLayer position)), SubPatchOptions.OnChangeOf (box (rowId, selected)))
        Border.child (
            StackPanel.create [
                StackPanel.orientation Orientation.Horizontal
                StackPanel.spacing 10.0
                StackPanel.children (
                    [
                        TextBlock.create [
                            TextBlock.text materialName
                            TextBlock.width 230.0
                            TextBlock.verticalAlignment VerticalAlignment.Center
                        ] :> IView
                        TextBlock.create [
                            automationId thicknessId
                            TextBlock.text (thicknessLabel layer.thickness)
                            TextBlock.width 90.0
                            TextBlock.verticalAlignment VerticalAlignment.Center
                        ] :> IView
                        TextBlock.create [
                            TextBlock.text (orientationLabel layer.orientation)
                            TextBlock.foreground (brush hintColor)
                            TextBlock.verticalAlignment VerticalAlignment.Center
                        ] :> IView
                    ]
                    @ orientationEditor)
            ])
    ] :> IView

/// A period group's ONE collapsible super-row: the rotating-triangle expander, the cell
/// summary, and the inline repeat-count stepper (whole periods through `SetRepeatCount`).
let private groupRowView (m : Model) (dispatch : Msg -> unit) (groupIndex : int) (group : PeriodGroup) : IView =
    let collapsed = Set.contains groupIndex m.collapsedGroups
    let triangle =
        TextBlock.create [
            TextBlock.text "▶"
            renderRotation (if collapsed then 0.0 else 90.0)
            TextBlock.verticalAlignment VerticalAlignment.Center
        ] :> IView
    Border.create [
        automationId (UiIds.groupRow groupIndex)
        Border.margin (thickOf 0.0 0.0 0.0 3.0)
        Border.background (brush groupBackground)
        Border.borderBrush (brush idleBorder)
        Border.borderThickness 1.0
        Border.cornerRadius (CornerRadius 3.0)
        Border.padding (thickLR 8.0 3.0)
        Border.child (
            StackPanel.create [
                StackPanel.orientation Orientation.Horizontal
                StackPanel.spacing 8.0
                StackPanel.children [
                    clickBoxView (UiIds.groupExpander groupIndex) false triangle (fun () -> dispatch (ToggleGroup groupIndex))
                    TextBlock.create [
                        TextBlock.text $"%d{List.length group.cell}-layer cell"
                        TextBlock.verticalAlignment VerticalAlignment.Center
                    ] :> IView
                    stepper
                        (UiIds.groupStepper groupIndex)
                        (UiIds.groupStepperMinus groupIndex)
                        (UiIds.groupStepperPlus groupIndex)
                        group.count
                        (fun delta -> dispatch (GroupCountBy (groupIndex, delta)))
                    TextBlock.create [
                        TextBlock.text $"× %d{group.count} periods = %d{group.count * List.length group.cell} films"
                        TextBlock.foreground (brush hintColor)
                        TextBlock.verticalAlignment VerticalAlignment.Center
                    ] :> IView
                ]
            ])
    ] :> IView

let private stackRows (m : Model) (dispatch : Msg -> unit) : IView list =
    m.editor.structure.films
    |> List.mapi (fun i item ->
        match item with
        | SingleLayer layer ->
            [ layerRowView m dispatch (AtSingleLayer i) (UiIds.layerRow i) (UiIds.layerThickness i) (UiIds.layerOrientation i) 0.0 layer ]
        | Repeated group ->
            let superRow = groupRowView m dispatch i group
            let cellRows =
                if Set.contains i m.collapsedGroups then []
                else
                    group.cell
                    |> List.mapi (fun j layer ->
                        layerRowView m dispatch (AtCellLayer (i, j)) (UiIds.cellLayerRow i j) (UiIds.cellLayerThickness i j) (UiIds.cellLayerOrientation i j) 28.0 layer)
            superRow :: cellRows)
    |> List.concat

let private filmsCountRow (m : Model) : IView =
    StackPanel.create [
        StackPanel.orientation Orientation.Horizontal
        StackPanel.spacing 6.0
        StackPanel.margin (thickOf 0.0 0.0 0.0 4.0)
        StackPanel.children [
            labelBlock "Films (expanded):"
            TextBlock.create [
                TextBlock.name UiIds.filmsCount
                TextBlock.text (string (filmsCount m))
                TextBlock.verticalAlignment VerticalAlignment.Center
            ] :> IView
        ]
    ] :> IView

// -- the bulk toolbar / QWOT / status / actions ----------------------------------------------

let private hasSelection (m : Model) : bool =
    not (Set.isEmpty m.editor.selection)

let private hasChosenMaterial (m : Model) : bool =
    match m.chosenMaterial with
    | Some _ -> true
    | None -> false

/// A material's display name by id (a placeholder when the id is unresolved).
let private nameOfMaterialId (m : Model) (id : MaterialId) : string =
    match m.materials |> List.tryFind (fun e -> e.id = id) with
    | Some e -> e.name
    | None -> $"unknown ({id.value})"

/// The substrate-plate and lower-half-space editor (spec 0033 gap G12): both
/// `SampleStructure` fields were previously invisible and uneditable. Each shows
/// its current material and offers Set-from-chosen / Clear (lower clears to
/// vacuum). Set requires a chosen material; Clear is always available.
let private halfSpacesRow (m : Model) (dispatch : Msg -> unit) : IView =
    let substrateText =
        match m.editor.structure.substrate with
        | Some layer -> $"{nameOfMaterialId m layer.materialId} ({thicknessLabel layer.thickness})"
        | None -> "none"
    let lowerText =
        match m.editor.structure.lower with
        | Some id -> nameOfMaterialId m id
        | None -> "vacuum"
    let summary (autoId : string) (text : string) : IView =
        TextBlock.create [
            TextBlock.name autoId
            TextBlock.text text
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
                    labelBlock "Substrate plate:"
                    summary UiIds.substrateSummary substrateText
                    verbButton UiIds.setSubstrateButton "Set from chosen" false (hasChosenMaterial m) (fun () -> dispatch SetSubstrateClicked)
                    verbButton UiIds.clearSubstrateButton "Clear" false true (fun () -> dispatch ClearSubstrateClicked)
                ]
            ] :> IView
            WrapPanel.create [
                WrapPanel.orientation Orientation.Horizontal
                WrapPanel.children [
                    labelBlock "Lower half-space:"
                    summary UiIds.lowerSummary lowerText
                    verbButton UiIds.setLowerButton "Set from chosen" false (hasChosenMaterial m) (fun () -> dispatch SetLowerClicked)
                    verbButton UiIds.clearLowerButton "Clear (vacuum)" false true (fun () -> dispatch ClearLowerClicked)
                ]
            ] :> IView
        ]
    ] :> IView

/// Selection verbs: select-by-material, clear, remove, move up/down, and the make-repeat-block
/// entry with its fold-count stepper (the mandated `RepeatCountStepper`). A WRAP panel: the
/// headless font metrics run a single row past the window edge, and an off-screen verb cannot
/// be clicked.
let private selectionToolbar (m : Model) (dispatch : Msg -> unit) : IView =
    WrapPanel.create [
        WrapPanel.orientation Orientation.Horizontal
        WrapPanel.children [
            verbButton UiIds.addLayerButton "Add layer" true (not (List.isEmpty m.materials)) (fun () -> dispatch AddLayerClicked)
            verbButton UiIds.selectByMaterialButton "Select by material" false (hasChosenMaterial m) (fun () -> dispatch SelectByMaterialClicked)
            verbButton UiIds.clearSelectionButton "Clear selection" false (hasSelection m) (fun () -> dispatch ClearSelectionClicked)
            verbButton UiIds.removeSelectedLayersButton "Remove" false (hasSelection m) (fun () -> dispatch RemoveSelectedClicked)
            verbButton UiIds.moveUpButton "Move up" false (hasSelection m) (fun () -> dispatch MoveUpClicked)
            verbButton UiIds.moveDownButton "Move down" false (hasSelection m) (fun () -> dispatch MoveDownClicked)
            stepper UiIds.repeatCountStepper UiIds.repeatCountStepperMinus UiIds.repeatCountStepperPlus m.foldCount (fun delta -> dispatch (FoldCountBy delta))
            verbButton UiIds.makeRepeatBlockButton "Make repeat block" false (hasSelection m) (fun () -> dispatch MakeRepeatBlockClicked)
        ]
    ] :> IView

/// Edit verbs over the selection: thickness (nm), material, and the toolbar orientation entry.
/// A WRAP panel (same off-screen-verb reason as the selection toolbar); each label+box pair is
/// one wrap item so a wrap never splits a label from its entry.
let private editToolbar (m : Model) (dispatch : Msg -> unit) : IView =
    let labelled (label : string) (entry : IView) : IView =
        StackPanel.create [
            StackPanel.orientation Orientation.Horizontal
            StackPanel.spacing 4.0
            StackPanel.margin (thickOf 0.0 0.0 8.0 0.0)
            StackPanel.children [ labelBlock label; entry ]
        ] :> IView
    let angleBox (boxId : string) (label : string) (text : string) (slot : AngleSlot) : IView =
        labelled label (
            TextBox.create [
                TextBox.name boxId
                TextBox.width 46.0
                TextBox.text text
                TextBox.onTextChanged (fun s -> dispatch (SetOrientationText (slot, s)))
            ] :> IView)
    WrapPanel.create [
        WrapPanel.orientation Orientation.Horizontal
        WrapPanel.children [
            labelled "Thickness (nm):" (
                TextBox.create [
                    TextBox.name UiIds.layerHeightBox
                    TextBox.width 70.0
                    TextBox.text m.thicknessText
                    TextBox.onTextChanged (SetThicknessText >> dispatch)
                ] :> IView)
            verbButton UiIds.setLayerHeightButton "Set thickness" false (hasSelection m) (fun () -> dispatch SetLayerHeightClicked)
            verbButton UiIds.setLayerMaterialButton "Set material" false (hasSelection m && hasChosenMaterial m) (fun () -> dispatch SetLayerMaterialClicked)
            angleBox UiIds.phiBox "φ°" m.phiText PhiSlot
            angleBox UiIds.thetaBox "θ°" m.thetaText ThetaSlot
            angleBox UiIds.psiBox "ψ°" m.psiText PsiSlot
            verbButton UiIds.setOrientationOfSelectedButton "Set orientation" false (hasSelection m) (fun () -> dispatch SetOrientationClicked)
        ]
    ] :> IView

/// The optional QWOT entry: λ (nm) → the read-only derived t = λ/(4n) in canonical metres
/// (n from the chosen material at λ); Set-thickness applies it while it is valid.
let private qwotRow (m : Model) (dispatch : Msg -> unit) : IView =
    // spec 0033 gap G14.1: render the derived thickness in display nanometres (the unit
    // every other thickness readout uses), not raw metres.
    let derivedLabel =
        match qwotDerived m with
        | Some t -> thicknessLabel t
        | None -> "—"
    StackPanel.create [
        StackPanel.orientation Orientation.Horizontal
        StackPanel.spacing 6.0
        StackPanel.children [
            labelBlock "QWOT λ (nm):"
            TextBox.create [
                TextBox.name UiIds.qwotEntryBox
                TextBox.width 70.0
                TextBox.text m.qwotText
                TextBox.onTextChanged (SetQwotText >> dispatch)
            ] :> IView
            labelBlock "→ t = λ/(4n) ="
            TextBlock.create [
                TextBlock.name UiIds.qwotDerivedText
                TextBlock.text derivedLabel
                TextBlock.verticalAlignment VerticalAlignment.Center
            ] :> IView
        ]
    ] :> IView

let private statusRow (m : Model) : IView =
    TextBlock.create [
        TextBlock.name UiIds.statusText
        TextBlock.foreground (brush errorColor)
        TextBlock.text (
            match m.status with
            | Some reason -> reason
            | None -> "")
    ] :> IView

let private saveCancelRow (dispatch : Msg -> unit) : IView =
    StackPanel.create [
        StackPanel.orientation Orientation.Horizontal
        StackPanel.spacing 0.0
        StackPanel.children [
            actionButton UiIds.saveButton "Save" saveBackground (fun () -> dispatch SaveClicked)
            actionButton UiIds.cancelButton "Cancel" cancelBackground (fun () -> dispatch CancelClicked)
        ]
    ] :> IView

/// The whole editor: identity + facets + material picker on top; the bulk toolbars, QWOT row,
/// status line and the Save/Cancel row pinned to the bottom; the stack table (with the
/// expanded-films readout) filling the centre.
let view (m : Model) (dispatch : Msg -> unit) : IView =
    DockPanel.create [
        DockPanel.children [
            Border.create [
                Border.dock Dock.Top
                Border.padding (thick 8.0)
                Border.child (
                    StackPanel.create [
                        StackPanel.orientation Orientation.Vertical
                        StackPanel.spacing 6.0
                        StackPanel.children [
                            nameRow m dispatch
                            descriptionRow m dispatch
                            substrateRow m dispatch
                            materialRow m dispatch
                        ]
                    ])
            ]
            Border.create [ Border.dock Dock.Bottom; Border.padding (thickLR 8.0 6.0); Border.child (saveCancelRow dispatch) ]
            Border.create [ Border.dock Dock.Bottom; Border.padding (thickLR 8.0 0.0); Border.child (statusRow m) ]
            Border.create [ Border.dock Dock.Bottom; Border.padding (thickLR 8.0 2.0); Border.child (qwotRow m dispatch) ]
            Border.create [ Border.dock Dock.Bottom; Border.padding (thickLR 8.0 2.0); Border.child (halfSpacesRow m dispatch) ]
            Border.create [ Border.dock Dock.Bottom; Border.padding (thickLR 8.0 2.0); Border.child (editToolbar m dispatch) ]
            Border.create [ Border.dock Dock.Bottom; Border.padding (thickLR 8.0 2.0); Border.child (selectionToolbar m dispatch) ]
            Border.create [
                Border.padding (thickLR 8.0 4.0)
                Border.child (
                    DockPanel.create [
                        DockPanel.children [
                            Border.create [ Border.dock Dock.Top; Border.child (filmsCountRow m) ]
                            ScrollViewer.create [
                                ScrollViewer.content (
                                    StackPanel.create [
                                        StackPanel.name UiIds.stackTable
                                        StackPanel.orientation Orientation.Vertical
                                        StackPanel.children (stackRows m dispatch)
                                    ])
                            ] :> IView
                        ]
                    ])
            ]
        ]
    ] :> IView
