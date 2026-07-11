/// Spec 0033 (023) — the Material editor view (UICOMP_XDUO_0004): the pure MVU model and the
/// FuncUI projection behind `MaterialEditorWindow`. The progressive-unlock ladder sits over the
/// pure Domain `MaterialComplexityEditor` — every ladder verb dispatches a
/// `MaterialComplexityMsg` through `applyMaterialComplexityMsg`, so behaviour is testable
/// without a window. The anisotropy 3-way choice constrains the symmetry-class gyration picker
/// (`availableGyrationClasses`); the absorbing / dispersive / OPTIONAL active + magnetic
/// toggles each lift exactly one aspect off its default and uncheck restores it losslessly;
/// the per-segment dispersion editor offers the full `DispersionModel` catalogue (incl.
/// ForouhiBloomer / BrendelBormann / the raw SumOfTerms escape hatch) lowered via `toEpsAxis`.
/// The live preview embeds the step-19 dual-axis n/k chart (`NkDispersionChart` +
/// `nkDispersionStyle`, per-side bounds from the 018 `ChartStyle.dataBounds`) as an inline
/// canvas; a negative sampled k surfaces the advisory gain warning (the
/// `imaginaryIndexGainWarning` rule, restated in Domain — Ui/Validation.fs:92). Save persists
/// through `MaterialProxy`, routing on the target's `EntryFreshness` (spec 0038 step 008):
/// `addMaterial` for a `NewUnsaved` entry — whose `MaterialId` was minted AT WINDOW OPEN
/// (`MaterialEditorIntent.NewMaterial`), off the save path — and `updateMaterial` for a
/// `Persisted` one; both store `complexity = Some model` with
/// `properties = model.toProperties`. Entries with `complexity = None` (silicon, langasite)
/// open VIEW-ONLY and offer no Edit affordance (no ladder, no Save). Pure: `update` only
/// reaches IO through the context's proxy fields.
module OpticalConstructor.Ui.MaterialEditorView

open System
open System.Globalization
open Avalonia
open Avalonia.Automation
open Avalonia.Controls
open Avalonia.Controls.Shapes
open Avalonia.Layout
open Avalonia.Media
open Avalonia.FuncUI.Builder
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open Berreman.MathNetNumericsMath
open Berreman.Fields
open Berreman.MaterialProperties
open Berreman.Dispersion
open Analytics.Variables
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Units
open OpticalConstructor.Domain.DispersionModels
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.MaterialComplexityEditor
open OpticalConstructor.Controls
open OpticalConstructor.Controls.ExperimentChart

/// Stable intent-named automation ids (CLAUDE.md UI guidance): the thirteen slice-mandated ids
/// as `[<Literal>]`s plus the derived per-axis / per-segment / per-option id families
/// (prefixed so they cannot collide — the SampleEditorWindow convention).
[<RequireQualifiedAccess>]
module UiIds =
    [<Literal>]
    let window = "MaterialEditorWindow"
    [<Literal>]
    let nameBox = "MaterialNameBox"
    [<Literal>]
    let anisotropyToggle = "AnisotropyToggle"
    [<Literal>]
    let absorbingToggle = "AbsorbingToggle"
    /// Spec 0035 (017): the eps branch is two mutually-exclusive options — Constant /
    /// Dispersive — routed through the SAME `SetDispersion` message (`NonDispersive` /
    /// `DispersiveSegments`), replacing the former sticky single Dispersive toggle. The
    /// `dispersiveToggle` id is kept for the Dispersive option; `constantToggle` is its pair.
    [<Literal>]
    let constantToggle = "ConstantToggle"
    [<Literal>]
    let dispersiveToggle = "DispersiveToggle"
    [<Literal>]
    let activeToggle = "ActiveToggle"
    [<Literal>]
    let magneticToggle = "MagneticToggle"
    /// Spec 0035 (011) — the per-rung Constant/Dispersive sub-toggles: on the activity
    /// rung and the magnetic rung, they flip each symmetry-allowed component between a
    /// constant box and a dispersion-formula coefficient editor.
    [<Literal>]
    let activityDispersiveToggle = "ActivityDispersiveToggle"
    [<Literal>]
    let magneticDispersiveToggle = "MagneticDispersiveToggle"
    [<Literal>]
    let gyrationClassPicker = "GyrationClassPicker"
    [<Literal>]
    let handednessSwitch = "HandednessSwitch"
    [<Literal>]
    let dispersionModelPicker = "DispersionModelPicker"
    [<Literal>]
    let addSegmentButton = "AddSegmentButton"
    [<Literal>]
    let saveButton = "MaterialEditorSaveButton"
    [<Literal>]
    let cancelButton = "MaterialEditorCancelButton"
    // Supporting fixed ids (not slice-mandated, same naming discipline).
    [<Literal>]
    let descriptionBox = "MaterialDescriptionBox"
    [<Literal>]
    let categoryPicker = "MaterialCategoryPicker"
    [<Literal>]
    let gyrationAxisPicker = "GyrationAxisPicker"
    [<Literal>]
    let muDiagonalBox = "MuDiagonalBox"
    [<Literal>]
    let muParallelBox = "MuParallelBox"
    [<Literal>]
    let muGyrationBox = "MuGyrationBox"
    [<Literal>]
    let statusText = "MaterialEditorStatus"
    [<Literal>]
    let summaryText = "MaterialComplexitySummary"
    [<Literal>]
    let previewChart = "MaterialPreviewChart"
    [<Literal>]
    let gainWarning = "MaterialGainWarning"
    [<Literal>]
    let viewOnlyNote = "MaterialViewOnlyNote"
    /// Spec 0038 (032): the two-pane split's vertical GridSplitter, the tabbed preview's TabControl,
    /// its three tabs, and the gyration / μ tab chart hosts (the n/k tab reuses `previewChart`).
    [<Literal>]
    let splitter = "MaterialEditorSplitter"
    [<Literal>]
    let previewTabs = "MaterialPreviewTabs"
    [<Literal>]
    let nkTab = "MaterialPreviewNkTab"
    [<Literal>]
    let gyrationTab = "MaterialPreviewGyrationTab"
    [<Literal>]
    let muTab = "MaterialPreviewMuTab"
    [<Literal>]
    let gyrationChart = "MaterialGyrationChart"
    [<Literal>]
    let muChart = "MaterialMuChart"
    /// A per-series show/hide toggle in a preview tab, by the tab's stable code and the series name.
    let seriesToggle (tabCode : string) (seriesName : string) : string = $"PreviewSeriesToggle_%s{tabCode}_%s{seriesName}"
    /// An anisotropy option, by the choice's stable code.
    let anisotropyOption (code : string) : string = "AnisotropyOption_" + code
    /// A material-category option, by the category's stable code.
    let categoryOption (code : string) : string = "MaterialCategoryOption_" + code
    /// A principal-index entry (1-based axis slot; present per the anisotropy choice).
    let indexBox (axisNumber : int) : string = $"PrincipalIndexBox_%d{axisNumber}"
    /// A principal absorption (k) entry (present only while the absorbing rung is unlocked).
    let absorptionBox (axisNumber : int) : string = $"AbsorptionIndexBox_%d{axisNumber}"
    /// Segment 0's model picker carries the mandated literal; later segments carry the
    /// indexed member of the same family (the 022 RepeatCountStepper precedent).
    let segmentModelPicker (segmentIndex : int) : string =
        if segmentIndex = 0 then dispersionModelPicker else $"DispersionModelPicker_%d{segmentIndex}"
    let segmentLowerBox (segmentIndex : int) : string = $"SegmentLowerBox_%d{segmentIndex}"
    let segmentUpperBox (segmentIndex : int) : string = $"SegmentUpperBox_%d{segmentIndex}"
    let segmentRemoveButton (segmentIndex : int) : string = $"SegmentRemoveButton_%d{segmentIndex}"
    /// A segment's dispersion-model option, by segment index and the model kind's code.
    let modelOption (segmentIndex : int) (code : string) : string = $"DispersionModelOption_%d{segmentIndex}_%s{code}"
    /// A gyration-class option, by the class's stable code.
    let gyrationClassOption (code : string) : string = "GyrationClassOption_" + code
    /// A handedness option ("Right" / "Left").
    let handednessOption (code : string) : string = "HandednessOption_" + code
    /// A magnetization-axis option ("X" / "Y" / "Z").
    let gyrationAxisOption (code : string) : string = "GyrationAxisOption_" + code
    /// A Polder-mu kind option ("Scalar" / "Gyromagnetic").
    let muKindOption (code : string) : string = "MuKindOption_" + code
    /// A per-segment dispersion-model COEFFICIENT entry (spec 0033 gap G7), by
    /// segment index and the parameter's stable key.
    let segmentParamBox (segmentIndex : int) (key : string) : string = $"SegmentParamBox_{segmentIndex}_{key}"
    /// Per-principal-axis id families (spec 0033 comment 009 — uniaxial / biaxial
    /// dispersive media carry one formula per axis). The FIRST axis reduces to the
    /// single-axis ids above, so the slice-mandated literals and existing tests stay
    /// valid; the extraordinary / y / z axes get an axis-suffixed member.
    let axisModelPickerOf (segmentIndex : int) (isFirst : bool) (axisCode : string) : string =
        if isFirst then segmentModelPicker segmentIndex else $"{segmentModelPicker segmentIndex}_ax{axisCode}"
    let axisModelOptionOf (segmentIndex : int) (isFirst : bool) (axisCode : string) (code : string) : string =
        if isFirst then modelOption segmentIndex code else $"{modelOption segmentIndex code}_ax{axisCode}"
    let axisParamBoxOf (segmentIndex : int) (isFirst : bool) (axisCode : string) (key : string) : string =
        if isFirst then segmentParamBox segmentIndex key else $"{segmentParamBox segmentIndex key}_ax{axisCode}"
    /// A gyration-tensor COMPONENT entry (spec 0033 gap G9), by the component's
    /// stable code (`g11` / `g33` / …).
    let gyrationComponentBox (code : string) : string = "GyrationComponentBox_" + code
    /// Spec 0035 (011): a gyration component's DISPERSION-FORMULA editor container (the
    /// activity Dispersive sub-branch), by the component's stable code — the id the
    /// headless proof probes for a per-component formula editor.
    let gyrationComponentFormulaEditor (code : string) : string = "GyrationFormulaEditor_" + code
    /// One coefficient entry inside a gyration component's dispersion-formula editor, by
    /// the component code and the wrapped model parameter's key (the raw `SumOfTerms`
    /// coefficient surface the segments already use).
    let gyrationComponentFormulaBox (code : string) (key : string) : string = $"GyrationFormulaBox_{code}_{key}"
    /// Spec 0035 (011): a Polder component's DISPERSION-FORMULA editor container (the
    /// magnetic Dispersive sub-branch), by the component's stable code
    /// (`muDiagonal` / `muParallel` / `muGyration`).
    let polderComponentFormulaEditor (code : string) : string = "PolderFormulaEditor_" + code
    /// One coefficient entry inside a Polder component's dispersion-formula editor.
    let polderComponentFormulaBox (code : string) (key : string) : string = $"PolderFormulaBox_{code}_{key}"

/// How the Material editor OPENS (spec 0038 step 008): Add mints the entry's `MaterialId`
/// AT WINDOW OPEN — the id-mint left the save path, so the window-policy seam
/// (`WindowLauncher`) can key the editor's registry entry by the SAME id the eventual Save
/// persists under — while Edit carries the store entry. A DU, not an option-plus-mint: the
/// open intents are NAMED (the `SampleEditorIntent` precedent).
type MaterialEditorIntent =
    | NewMaterial of mintedId : MaterialId
    | EditMaterial of entry : MaterialEntry

/// What Save targets (spec 0038 step 008): the entity id — ALWAYS present, minted at
/// Add-window open — plus its `EntryFreshness`. Save routes on the freshness (`NewUnsaved`
/// → `addMaterial`, `Persisted` → `updateMaterial`); no save path mints an id anymore.
type EditorTarget =
    {
        materialId : MaterialId
        freshness : WindowLauncher.EntryFreshness
    }

/// Whether the window edits or only shows. `ViewOnlyMaterial` carries the WHY (an entry whose
/// physics is engine-coded — `complexity = None` — or a complexity this editor cannot seed):
/// such an entry offers NO Edit affordance — the ladder and Save are REMOVED, not greyed.
type EditorMode =
    | EditableMaterial
    | ViewOnlyMaterial of reason : string

/// The window's IO seam (the functional-proxy Context convention): the materials write-seam
/// Save persists through, plus the host's close request (the window passes `this.Close`;
/// tests substitute recording stubs). Function-valued fields have no structural equality, so
/// the context compares by reference — the model holding it keeps its equality.
[<ReferenceEquality>]
type MaterialEditorContext =
    {
        materials : MaterialProxy
        /// Spec 0035 (009): the category write-seam behind the create picker. The picker reads the
        /// LIVE catalogue (`listCategories`) at render, so a category renamed through the SHARED
        /// proxy (e.g. in the step-6 Category editor) re-labels this picker on the editor's next open.
        categories : CategoryProxy
        requestClose : unit -> unit
    }

/// The editor's model: the entry identity facets (name / category / description), the pure
/// Domain ladder state, and — for a view-only entry — the preset engine properties the preview
/// still charts. Reference-compared: `OpticalPropertiesWithDisp` carries dispersion FUNCTION
/// cases (no structural equality), and `update` returns a fresh record anyway — so the Elmish
/// equality gate sees every dispatch as a change and re-renders.
[<ReferenceEquality>]
type Model =
    {
        context : MaterialEditorContext
        target : EditorTarget
        mode : EditorMode
        name : string
        description : string
        category : CategoryId
        editor : MaterialComplexityEditState
        /// A view-only entry's own engine properties (the preview's source there).
        presetProperties : OpticalPropertiesWithDisp option
        /// Spec 0038 (032): the preview curves the user hid, keyed by `UiIds.seriesToggle`'s tab code
        /// + series name (so a toggle in one tab never hides a same-named curve in another).
        hiddenSeries : Set<string>
        /// The last typed-error reason (or proxy rejection) surfaced to the user.
        status : string option
    }

type Msg =
    | SetName of string
    | SetDescription of string
    | ChooseCategory of CategoryId
    /// One ladder edit, routed through the pure Domain apply.
    | EditorMsg of MaterialComplexityMsg
    /// Spec 0038 (032): flip one preview curve's visibility, by its `UiIds.seriesToggle` key.
    | ToggleSeriesVisibility of key : string
    | SaveClicked
    | CancelClicked

// ---------------------------------------------------------------------------
// Pure helpers.
// ---------------------------------------------------------------------------

/// An anisotropy choice's stable code (the derived UiIds key).
let anisotropyCode (anisotropy : Anisotropy) : string =
    match anisotropy with
    | Isotropic -> "Isotropic"
    | Uniaxial -> "Uniaxial"
    | Biaxial -> "Biaxial"

let private anisotropyLabel (anisotropy : Anisotropy) : string =
    match anisotropy with
    | Isotropic -> "Isotropic"
    | Uniaxial -> "Uniaxial"
    | Biaxial -> "Biaxial"

/// A material category's stable code (the derived UiIds key): the display name resolved
/// through the seeded catalogue by `CategoryId` (spec 0035 step 001), not a closed-union match.
let categoryCode (category : CategoryId) : string =
    categoryName category

/// The create picker's category choices (spec 0035 step 009): the LIVE catalogue from the context's
/// `CategoryProxy` (`listCategories`, re-read at render), EXCLUDING every `HiddenOnCreate` category
/// (Vacuum) — those categorise seed entries but are not user-selectable creation targets, so they
/// are REMOVED from the picker, not greyed. A store listing error yields no options (never a throw).
let selectableCategories (m : Model) : MaterialCategory list =
    match m.context.categories.listCategories () with
    | Ok cats -> cats |> List.filter (fun c -> c.visibility <> HiddenOnCreate)
    | Error _ -> []

/// A handedness option's stable code (the derived UiIds key).
let handednessCode (hand : Handedness) : string =
    match hand with
    | RightHanded -> "Right"
    | LeftHanded -> "Left"

/// A magnetization axis option's stable code (the derived UiIds key).
let gyrationAxisCode (axis : GyrationAxis) : string =
    match axis with
    | AlongX -> "X"
    | AlongY -> "Y"
    | AlongZ -> "Z"

/// A Polder-mu kind option's stable code (the derived UiIds key).
let muKindCode (kind : MuKind) : string =
    match kind with
    | ScalarMuKind -> "Scalar"
    | GyromagneticMuKind -> "Gyromagnetic"

let private parseFloat (s : string) : float option =
    match Double.TryParse(s, NumberStyles.Float, CultureInfo.InvariantCulture) with
    | true, v -> Some v
    | _ -> None

let editErrorReason (e : MaterialComplexityEditError) : string =
    match e with
    | NoSuchSegment reason
    | LastSegmentNotRemovable reason -> reason

let private materialErrorReason (e : MaterialError) : string =
    match e with
    | UnknownMaterialId reason
    | DuplicateMaterialId reason
    | MaterialStillReferenced reason
    | MaterialVersionInUse reason
    | InvalidMaterial reason -> reason

/// The live preview's fixed spectral window: the visible band in canonical meters (the
/// display unit is nm; AC-D7 — the unit never changes the sampled range).
let previewRange : Range<WaveLength> = SpectralAxis.spectralRange Nanometer 400.0 700.0 60

/// What the preview charts: the derived complexity's engine properties in edit mode, the
/// entry's own preset properties in view-only mode, nothing while the ladder is underivable.
let private previewProperties (m : Model) : OpticalPropertiesWithDisp option =
    match m.mode with
    | ViewOnlyMaterial _ -> m.presetProperties
    | EditableMaterial ->
        match toComplexity m.editor with
        | Ok c -> Some c.toProperties
        | Error _ -> None

/// The advisory gain warning over the n/k chart's sampled k curves — every per-axis k series (the
/// restated `imaginaryIndexGainWarning` rule; empty when nothing warns).
let private gainWarningOf (chartOpt : ExperimentChart option) : string =
    match chartOpt with
    | None -> ""
    | Some chart ->
        chart.series
        |> List.filter (fun s -> s.name.StartsWith "k")
        |> List.tryPick (fun s -> s.points |> List.tryPick (fun (_, k) -> imaginaryIndexGainWarning k))
        |> Option.defaultValue ""

let private epsCaseLabel (eps : EpsWithDispValue) : string =
    match eps with
    | EpsWithoutDispValue (IsotropicTransparent _) -> "isotropic transparent"
    | EpsWithoutDispValue (IsotropicAbsorbing _) -> "isotropic absorbing"
    | EpsWithoutDispValue (UniaxialTransparent _) -> "uniaxial transparent"
    | EpsWithoutDispValue (UniaxialAbsorbing _) -> "uniaxial absorbing"
    | EpsWithoutDispValue (BiaxialTransparent _) -> "biaxial transparent"
    | EpsWithoutDispValue (BiaxialAbsorbing _) -> "biaxial absorbing"
    | EpsWithDispValue (IsotropicDispersive segments) -> $"isotropic dispersive (%d{List.length segments} segments)"
    | EpsWithDispValue (UniaxialDispersive segments) -> $"uniaxial dispersive (%d{List.length segments} segments)"
    | EpsWithDispValue (BiaxialDispersive segments) -> $"biaxial dispersive (%d{List.length segments} segments)"

/// The derived-model readout: the eps case, the optional aspects, and a structural digest —
/// two states with the SAME summary denote the SAME derived complexity (the lossless-uncheck
/// acceptance observes this text); an underivable ladder shows the typed reason instead.
let complexitySummary (m : Model) : string =
    match m.mode with
    | ViewOnlyMaterial _ -> "view-only engine preset"
    | EditableMaterial ->
        match toComplexity m.editor with
        | Error e -> $"not derivable — %s{editErrorReason e}"
        | Ok c ->
            let aspect (label : string) (o : 'a option) : string =
                match o with
                | Some _ -> $"%s{label} on"
                | None -> $"%s{label} off"
            $"""eps %s{epsCaseLabel c.eps}; %s{aspect "active" c.active}; %s{aspect "magnetic" c.magnetic} [%08x{hash c}]"""

// ---------------------------------------------------------------------------
// init / update (pure — IO only through the context's proxy fields).
// ---------------------------------------------------------------------------

let init (context : MaterialEditorContext) (intent : MaterialEditorIntent) : Model =
    match intent with
    | NewMaterial mintedId ->
        // The Add path: the id was minted AT WINDOW OPEN (spec 0038 step 008) and rides the
        // model as a NewUnsaved target, so Save persists under the SAME id the launcher's
        // registry already keys this window by.
        {
            context = context
            target = { materialId = mintedId; freshness = WindowLauncher.NewUnsaved }
            mode = EditableMaterial
            name = ""
            description = ""
            category = CategoryIds.glass
            editor = defaultState
            presetProperties = None
            hiddenSeries = Set.empty
            status = None
        }
    | EditMaterial entry ->
        let seeded =
            {
                context = context
                target = { materialId = entry.id; freshness = WindowLauncher.Persisted }
                mode = EditableMaterial
                name = entry.name
                description = (match entry.description with Some d -> d | None -> "")
                category = entry.category
                editor = defaultState
                presetProperties = None
                hiddenSeries = Set.empty
                status = None
            }
        match entry.complexity with
        | None ->
            { seeded with
                mode = ViewOnlyMaterial "this entry's physics is coded in the engine (complexity = None) — view-only"
                presetProperties = Some entry.properties }
        | Some complexity ->
            match ofComplexity complexity with
            | Ok editor -> { seeded with editor = editor }
            | Error e ->
                { seeded with
                    mode = ViewOnlyMaterial (editErrorReason e)
                    presetProperties = Some entry.properties }

let update (msg : Msg) (m : Model) : Model =
    match msg with
    | SetName s -> { m with name = s }
    | SetDescription s -> { m with description = s }
    | ChooseCategory category -> { m with category = category }
    | EditorMsg editorMsg ->
        match applyMaterialComplexityMsg editorMsg m.editor with
        // A structurally-unchanged edit returns the SAME model (no new record), so the Elmish
        // equality gate does NOT re-render. This is what stops a coefficient / component box's
        // programmatic re-render echo (which re-dispatches the round-tripped, unchanged value)
        // from spinning an infinite render loop (spec 0033 gaps G7 / G9).
        | Ok editor when editor = m.editor -> m
        | Ok editor -> { m with editor = editor; status = None }
        | Error e -> { m with status = Some (editErrorReason e) }
    | ToggleSeriesVisibility key ->
        // Preview-only UI state (which curves are hidden); never touches the derived complexity.
        let hidden =
            if Set.contains key m.hiddenSeries then Set.remove key m.hiddenSeries else Set.add key m.hiddenSeries
        { m with hiddenSeries = hidden }
    | SaveClicked ->
        match m.mode with
        | ViewOnlyMaterial reason -> { m with status = Some reason }
        | EditableMaterial ->
            match toComplexity m.editor with
            | Error e -> { m with status = Some (editErrorReason e) }
            | Ok complexity ->
                let entryUnder (id : MaterialId) : MaterialEntry =
                    {
                        id = id
                        name = m.name
                        category = m.category
                        description = (if String.IsNullOrWhiteSpace m.description then None else Some m.description)
                        properties = complexity.toProperties
                        complexity = Some complexity
                    }
                // Spec 0038 step 021: Save routes through the versioned write-seam — the store
                // itself decides insert-v1 / mutate-in-place / mint-next-version from the id's
                // presence and the shared `decideVersioning` rule, so the freshness split at the
                // editor collapses to ONE `saveMaterial` (a fresh id, minted at window open per
                // step 008, is simply absent from the store and inserted as version 1).
                let saved = m.context.materials.saveMaterial (entryUnder m.target.materialId)
                match saved with
                | Ok () ->
                    m.context.requestClose ()
                    { m with status = None }
                | Error e -> { m with status = Some (materialErrorReason e) }
    | CancelClicked ->
        m.context.requestClose ()
        m

// ---------------------------------------------------------------------------
// The FuncUI view. Styling matches the sibling editors' idle/chosen boxes; every control in
// the conditional ladder subtrees carries a mutable AutomationId, never `Name` (FuncUI
// recycling cannot rename a styled control, and the ladder's membership changes on every
// toggle — the SampleEditorWindow precedent).
// ---------------------------------------------------------------------------

let private color (r : int) (g : int) (b : int) : Color = Color.FromRgb(byte r, byte g, byte b)
let private brush (c : Color) : IBrush = SolidColorBrush(c) :> IBrush
let private idleBackground = color 232 232 232
let private chosenBackground = color 150 185 235
let private saveBackground = color 186 224 186
let private cancelBackground = color 236 202 202
let private idleBorder = color 120 120 120
let private hintColor = color 110 110 110
let private errorColor = color 165 40 40
let private warningColor = color 170 110 20

let private thick (uniform : float) : Thickness = Thickness(uniform)
let private thickLR (horizontal : float) (vertical : float) : Thickness = Thickness(horizontal, vertical)
let private thickOf (l : float) (t : float) (r : float) (b : float) : Thickness = Thickness(l, t, r, b)

/// Set `AutomationProperties.AutomationId` (freely mutable, unlike `Control.Name`) through
/// FuncUI's attr builder — the id survives FuncUI recycling a control onto another item's slot.
let private automationId<'t when 't :> Control> (autoId : string) : IAttr<'t> =
    AttrBuilder<'t>.CreateProperty<string>(AutomationProperties.AutomationIdProperty, autoId, ValueNone)

/// A clickable, styled box, highlighted when chosen (an option or a toggle). `e.Handled <- true`
/// drops FuncUI's duplicate Tunnel|Bubble pass; re-subscribe when the id or highlight changes.
let private clickBox (autoId : string) (label : string) (chosen : bool) (onClick : unit -> unit) : IView =
    Border.create [
        automationId autoId
        Border.background (brush (if chosen then chosenBackground else idleBackground))
        Border.borderBrush (brush idleBorder)
        Border.borderThickness 1.0
        Border.cornerRadius (CornerRadius 3.0)
        Border.padding (thickLR 10.0 4.0)
        Border.margin (thickOf 0.0 0.0 6.0 4.0)
        Border.verticalAlignment VerticalAlignment.Center
        Border.child (TextBlock.create [ TextBlock.text label ])
        Border.onPointerPressed ((fun e -> e.Handled <- true; onClick ()), SubPatchOptions.OnChangeOf (box (autoId, chosen)))
    ] :> IView

/// A verb button — disabled verbs are present but inert (a disabled Border dispatches no
/// pointer event). Left-aligned so a button never stretches to its container's full width
/// (spec 0033 gap G11 — the add-segment verb sat in a vertical stack and spanned it).
let private verbButton (autoId : string) (label : string) (enabled : bool) (onClick : unit -> unit) : IView =
    Border.create [
        automationId autoId
        Border.isEnabled enabled
        Border.opacity (if enabled then 1.0 else 0.4)
        Border.background (brush idleBackground)
        Border.borderBrush (brush idleBorder)
        Border.borderThickness 1.0
        Border.cornerRadius (CornerRadius 3.0)
        Border.padding (thickLR 12.0 5.0)
        Border.margin (thickOf 0.0 0.0 8.0 4.0)
        Border.horizontalAlignment HorizontalAlignment.Left
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

let private labelBlock (label : string) : IView =
    TextBlock.create [ TextBlock.text label; TextBlock.verticalAlignment VerticalAlignment.Center ] :> IView

/// A label + entry pair as ONE wrap item (a wrap never splits a label from its box).
let private labelled (label : string) (entry : IView) : IView =
    StackPanel.create [
        StackPanel.orientation Orientation.Horizontal
        StackPanel.spacing 4.0
        StackPanel.margin (thickOf 0.0 0.0 10.0 4.0)
        StackPanel.children [ labelBlock label; entry ]
    ] :> IView

/// A numeric entry: shows the model's value, parses on change, dispatches only a parsable
/// float (an unparsable draft stays local until the next model-driven render). AutomationId,
/// never Name — the ladder boxes live in variable-membership subtrees.
let private numberBox (autoId : string) (width : float) (value : float) (onCommit : float -> unit) : IView =
    TextBox.create [
        automationId autoId
        TextBox.width width
        TextBox.text $"%g{value}"
        TextBox.onTextChanged (
            (fun s ->
                match parseFloat s with
                | Some v -> onCommit v
                | None -> ()),
            SubPatchOptions.OnChangeOf (box (autoId, value)))
    ] :> IView

/// A numeric entry for a coefficient / tensor component (spec 0033 gaps G7 & G9). It commits on
/// FOCUS LOSS, not on every keystroke — deliberately UNLIKE `numberBox`. These boxes rebuild the
/// whole dispersion model on commit, and a `TextChanged` commit re-fires on FuncUI's programmatic
/// re-render echo, which (because each commit rebuilds the model) spins an infinite render loop.
/// `LostFocus` fires only on a real user focus change, never during render, so the loop cannot
/// form; the value is shown as the shortest round-trippable string so no precision is lost.
let private coeffNumberBox (autoId : string) (width : float) (value : float) (onCommit : float -> unit) : IView =
    TextBox.create [
        automationId autoId
        TextBox.width width
        TextBox.text (value.ToString("R", CultureInfo.InvariantCulture))
        TextBox.onLostFocus (fun e ->
            match e.Source with
            | :? TextBox as tb ->
                match parseFloat tb.Text with
                | Some v -> onCommit v
                | None -> ()
            | _ -> ())
    ] :> IView

// -- the identity rows -----------------------------------------------------------------------

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

let private categoryRow (m : Model) (dispatch : Msg -> unit) : IView =
    StackPanel.create [
        StackPanel.orientation Orientation.Vertical
        StackPanel.spacing 2.0
        StackPanel.children [
            labelBlock "Category:"
            WrapPanel.create [
                automationId UiIds.categoryPicker
                WrapPanel.orientation Orientation.Horizontal
                // Spec 0035 (009): the create picker offers the LIVE catalogue minus every
                // HiddenOnCreate category (Vacuum is removed, not greyed). Each option's id is the
                // category's `CategoryId` Guid string — STABLE across a rename, so the option box is
                // patched (only its label changes) rather than re-created — and its label is the
                // record NAME, so a category renamed through the shared proxy re-labels the picker.
                WrapPanel.children (
                    selectableCategories m
                    |> List.map (fun category ->
                        clickBox (UiIds.categoryOption (string category.id.value)) category.name (m.category = category.id) (fun () -> dispatch (ChooseCategory category.id))))
            ] :> IView
        ]
    ] :> IView

// -- the ladder ------------------------------------------------------------------------------

let private anisotropyRow (m : Model) (dispatch : Msg -> unit) : IView =
    StackPanel.create [
        StackPanel.orientation Orientation.Vertical
        StackPanel.spacing 2.0
        StackPanel.children [
            labelBlock "Anisotropy:"
            WrapPanel.create [
                automationId UiIds.anisotropyToggle
                WrapPanel.orientation Orientation.Horizontal
                WrapPanel.children (
                    [ Isotropic; Uniaxial; Biaxial ]
                    |> List.map (fun choice ->
                        clickBox (UiIds.anisotropyOption (anisotropyCode choice)) (anisotropyLabel choice) (m.editor.anisotropy = choice) (fun () -> dispatch (EditorMsg (ChooseAnisotropy choice)))))
            ] :> IView
        ]
    ] :> IView

/// The unlock toggles (spec 0033 gap G6; spec 0035 step 017). The eps MODEL choice leads:
/// two MUTUALLY-EXCLUSIVE options — Constant / Dispersive — select `NonDispersive` /
/// `DispersiveSegments`, both routed through the SAME `SetDispersion` message, so the derived
/// model is unchanged (this restyle is cosmetic — it replaces the former sticky single
/// Dispersive toggle). The Constant branch's transparent-vs-absorbing sub-choice is the
/// `Absorbing` toggle — shown ONLY in the Constant branch. A dispersive medium carries its
/// absorption inside the formulas, so a constant-case absorbing toggle would be inert there
/// and is REMOVED, not greyed (Part F). The optional activity / magnetic aspects follow; the
/// activity toggle is REMOVED when the anisotropy choice offers no rotating gyration class
/// (`availableGyrationClasses`).
let private togglesRow (m : Model) (dispatch : Msg -> unit) : IView =
    let absorbing = m.editor.transparency = Absorbing
    let dispersive = m.editor.dispersion = DispersiveSegments
    let active = m.editor.activity = ActivityOn
    let magnetic = m.editor.magnetic = MagneticOn
    let activityOffered = not (List.isEmpty (availableGyrationClasses m.editor.anisotropy))
    StackPanel.create [
        StackPanel.orientation Orientation.Vertical
        StackPanel.spacing 2.0
        StackPanel.children [
            labelBlock "Dispersion model:"
            WrapPanel.create [
                WrapPanel.orientation Orientation.Horizontal
                WrapPanel.children (
                    [
                        clickBox UiIds.constantToggle "Constant" (not dispersive) (fun () ->
                            dispatch (EditorMsg (SetDispersion NonDispersive)))
                        clickBox UiIds.dispersiveToggle "Dispersive" dispersive (fun () ->
                            dispatch (EditorMsg (SetDispersion DispersiveSegments)))
                    ]
                    @ (if dispersive then []
                       else
                           [ clickBox UiIds.absorbingToggle "Absorbing" absorbing (fun () ->
                                 dispatch (EditorMsg (SetTransparency (if absorbing then Transparent else Absorbing)))) ])
                    @ (if activityOffered then
                           [ clickBox UiIds.activeToggle "Optically active" active (fun () ->
                                 dispatch (EditorMsg (SetActivity (if active then ActivityOff else ActivityOn)))) ]
                       else [])
                    @ [
                        clickBox UiIds.magneticToggle "Magnetic (Polder μ)" magnetic (fun () ->
                            dispatch (EditorMsg (SetMagnetic (if magnetic then MagneticOff else MagneticOn))))
                    ])
            ] :> IView
        ]
    ] :> IView

/// The constant principal-index fields (the non-dispersive rung): 1 / 2 / 3 boxes per the
/// anisotropy choice, plus the k boxes while the absorbing rung is unlocked.
let private indexFieldsRow (m : Model) (dispatch : Msg -> unit) : IView =
    let slots =
        match m.editor.anisotropy with
        | Isotropic -> [ (1, FirstAxis, m.editor.index1, "n₁:") ]
        | Uniaxial -> [ (1, FirstAxis, m.editor.index1, "n₁ (ordinary):"); (2, SecondAxis, m.editor.index2, "n₂ (extraordinary):") ]
        | Biaxial -> [ (1, FirstAxis, m.editor.index1, "n₁:"); (2, SecondAxis, m.editor.index2, "n₂:"); (3, ThirdAxis, m.editor.index3, "n₃:") ]
    let boxes =
        slots
        |> List.collect (fun (number, slot, index, label) ->
            let re = index.value.Real
            let im = index.value.Imaginary
            [
                labelled label (numberBox (UiIds.indexBox number) 70.0 re (fun v ->
                    dispatch (EditorMsg (SetPrincipalIndex (slot, ComplexRefractionIndex (createComplex v im))))))
            ]
            @ (match m.editor.transparency with
               | Absorbing ->
                   [
                       labelled $"k%d{number}:" (numberBox (UiIds.absorptionBox number) 70.0 im (fun v ->
                           dispatch (EditorMsg (SetPrincipalIndex (slot, ComplexRefractionIndex (createComplex re v))))))
                   ]
               | Transparent -> []))
    WrapPanel.create [
        WrapPanel.orientation Orientation.Horizontal
        WrapPanel.children boxes
    ] :> IView

/// One coefficient entry with its unit of measure shown (spec 0033 comment 009): a dimensioned
/// parameter reads "label: [box] unit"; a dimensionless one omits the unit.
let private paramBoxWithUnit (autoId : string) (p : ModelParameter) (onCommit : float -> unit) : IView =
    StackPanel.create [
        StackPanel.orientation Orientation.Horizontal
        StackPanel.spacing 4.0
        StackPanel.margin (thickOf 0.0 0.0 10.0 4.0)
        StackPanel.children (
            [ labelBlock (p.label + ":"); coeffNumberBox autoId 100.0 p.value onCommit ]
            @ (if p.unit = "" then [] else [ labelBlock p.unit ]))
    ] :> IView

// -- the dispersive-component formula surface (spec 0035 step 011) ----------------------------

/// Wrap one scalar component's `DispersionFormula` (a gyration `g₍ᵢⱼ₎` or a Polder
/// component — spec §C.0: a component IS a `DispersionFormula`) as the raw
/// `SumOfTerms` model, so the SAME `modelParameters` coefficient surface the segments
/// use edits it. The component is a single REAL formula, so the k slot is the empty
/// formula — it contributes no boxes, and only the n formula's term scalars are exposed.
let private formulaAsModel (formula : DispersionFormula) : DispersionModel =
    SumOfTerms (RealNK (formula, { terms = []; wavelengthScale = formula.wavelengthScale }))

/// Read the edited scalar formula back out of a wrapped component model (the n slot).
/// `formulaAsModel` always builds `RealNK`, so the fallback is unreachable — it keeps
/// the prior formula rather than throwing.
let private formulaOfModel (fallback : DispersionFormula) (model : DispersionModel) : DispersionFormula =
    match model with
    | SumOfTerms (RealNK (n, _)) -> n
    | _ -> fallback

/// One scalar component's dispersion-formula coefficient editor (spec 0035 step 011 —
/// the gyration / Polder analogue of `segmentParamsView`): the `modelParameters` boxes
/// of the component's raw `SumOfTerms` formula inside a WrapPanel that carries
/// `editorId` (the per-component container id the headless proof probes). Each edit
/// rebuilds the formula and dispatches it through `dispatchFormula`. The boxes commit
/// on focus loss (`coeffNumberBox`), never on the render echo — the gap-G7/G9 loop
/// guard.
let private componentFormulaView (editorId : string) (boxId : string -> string) (formula : DispersionFormula) (dispatchFormula : DispersionFormula -> unit) : IView =
    let boxes =
        modelParameters (formulaAsModel formula)
        |> List.map (fun p ->
            paramBoxWithUnit (boxId p.key) p (fun v -> dispatchFormula (formulaOfModel formula (p.update v))))
    WrapPanel.create [
        automationId editorId
        WrapPanel.orientation Orientation.Horizontal
        WrapPanel.children boxes
    ] :> IView

/// The coefficient-entry surface for ONE principal axis of a segment (spec 0033 gap G7 +
/// comment 009): one unit-labelled box per editable coefficient (`DispersionModels.modelParameters`,
/// which now covers the raw `SumOfTerms` term data too), each edit rebuilding that axis's model
/// through `SetSegmentAxisModel`.
let private segmentParamsView (dispatch : Msg -> unit) (segmentIndex : int) (slot : PrincipalAxisSlot) (isFirst : bool) (axisCode : string) (model : DispersionModel) : IView =
    let boxes =
        modelParameters model
        |> List.map (fun p ->
            paramBoxWithUnit (UiIds.axisParamBoxOf segmentIndex isFirst axisCode p.key) p (fun v ->
                dispatch (EditorMsg (SetSegmentAxisModel (segmentIndex, slot, p.update v)))))
    match boxes with
    | [] -> TextBlock.create [ TextBlock.text "(no editable coefficients)"; TextBlock.foreground (brush hintColor) ] :> IView
    | _ ->
        WrapPanel.create [
            WrapPanel.orientation Orientation.Horizontal
            WrapPanel.children boxes
        ] :> IView

/// The principal axes a dispersive segment exposes, by anisotropy (spec 0033 comment 009):
/// isotropic → one unlabelled axis; uniaxial → ordinary + extraordinary; biaxial → x/y/z. The
/// tuple is (slot, display label, id-code); the first slot always uses the single-axis ids.
let private axisSlotsOf (anisotropy : Anisotropy) : (PrincipalAxisSlot * string * string) list =
    match anisotropy with
    | Isotropic -> [ (FirstAxis, "", "o") ]
    | Uniaxial -> [ (FirstAxis, "Ordinary (o):", "o"); (SecondAxis, "Extraordinary (e):", "e") ]
    | Biaxial -> [ (FirstAxis, "X:", "x"); (SecondAxis, "Y:", "y"); (ThirdAxis, "Z:", "z") ]

/// One principal axis's model picker + coefficient boxes within a segment (spec 0033 comment 009).
let private segmentAxisView (dispatch : Msg -> unit) (segmentIndex : int) (slot : PrincipalAxisSlot) (axisLabel : string) (axisCode : string) (model : DispersionModel) : IView =
    let isFirst = (slot = FirstAxis)
    let currentCode = modelKindCode model
    StackPanel.create [
        StackPanel.orientation Orientation.Vertical
        StackPanel.spacing 2.0
        StackPanel.children (
            (if axisLabel = "" then [] else [ labelBlock axisLabel ])
            @ [
                WrapPanel.create [
                    automationId (UiIds.axisModelPickerOf segmentIndex isFirst axisCode)
                    WrapPanel.orientation Orientation.Horizontal
                    WrapPanel.children (
                        defaultModelChoices
                        |> List.map (fun candidate ->
                            let code = modelKindCode candidate
                            clickBox (UiIds.axisModelOptionOf segmentIndex isFirst axisCode code) (modelKindLabel candidate) (currentCode = code) (fun () ->
                                dispatch (EditorMsg (ChooseSegmentAxisModel (segmentIndex, slot, candidate))))))
                ] :> IView
                segmentParamsView dispatch segmentIndex slot isFirst axisCode model
            ])
    ] :> IView

/// One dispersion segment's editor: the shared wavelengthInterval bounds (nm), then — PER
/// PRINCIPAL AXIS of the anisotropy (spec 0033 comment 009) — a model picker + unit-labelled
/// coefficient boxes.
let private segmentView (m : Model) (dispatch : Msg -> unit) (segmentIndex : int) (segment : EditSegment) : IView =
    let lowerNm = wavelengthToUnit Nanometer segment.interval.lower
    let upperNm = wavelengthToUnit Nanometer segment.interval.upper
    StackPanel.create [
        StackPanel.orientation Orientation.Vertical
        StackPanel.spacing 2.0
        StackPanel.children (
            [
                WrapPanel.create [
                    WrapPanel.orientation Orientation.Horizontal
                    WrapPanel.children [
                        labelBlock $"Segment {segmentIndex} —"
                        labelled "λ from (nm):" (numberBox (UiIds.segmentLowerBox segmentIndex) 70.0 lowerNm (fun v ->
                            dispatch (EditorMsg (SetSegmentInterval (segmentIndex, { lower = toWaveLength Nanometer v; upper = segment.interval.upper })))))
                        labelled "to (nm):" (numberBox (UiIds.segmentUpperBox segmentIndex) 70.0 upperNm (fun v ->
                            dispatch (EditorMsg (SetSegmentInterval (segmentIndex, { lower = segment.interval.lower; upper = toWaveLength Nanometer v })))))
                        verbButton (UiIds.segmentRemoveButton segmentIndex) "Remove" (List.length m.editor.segments > 1) (fun () ->
                            dispatch (EditorMsg (RemoveSegment segmentIndex)))
                    ]
                ] :> IView
            ]
            @ (axisSlotsOf m.editor.anisotropy
               |> List.map (fun (slot, axisLabel, axisCode) ->
                   segmentAxisView dispatch segmentIndex slot axisLabel axisCode (axisModelOf slot segment))))
    ] :> IView

/// The per-segment dispersion editor (the dispersive rung): each segment's bounds + model
/// picker, then the mandated add-segment verb.
let private segmentsPanel (m : Model) (dispatch : Msg -> unit) : IView =
    StackPanel.create [
        StackPanel.orientation Orientation.Vertical
        StackPanel.spacing 2.0
        StackPanel.children (
            [ labelBlock "Dispersion segments (first covering segment wins; the topmost extrapolates):" ]
            @ (m.editor.segments |> List.mapi (fun i seg -> segmentView m dispatch i seg))
            @ [ verbButton UiIds.addSegmentButton "Add segment" true (fun () -> dispatch (EditorMsg AddSegment)) ])
    ] :> IView

/// The gyration-component entry surface (the activity rung's Constant vs Dispersive
/// sub-branch — spec 0035 step 011): under `ConstantComponents` the per-component
/// constant `coeffNumberBox`es; under `DispersiveComponents` one dispersion-formula
/// editor per symmetry-allowed component of `gyrationDispersion` (class-synced with
/// `gyration`, so the component set matches the class picker either way).
let private gyrationComponentsSection (m : Model) (dispatch : Msg -> unit) : IView =
    match m.editor.activityDispersion with
    | ConstantComponents ->
        WrapPanel.create [
            WrapPanel.orientation Orientation.Horizontal
            WrapPanel.children (
                gyrationComponents m.editor.gyration
                |> List.map (fun (comp, RhoValue value) ->
                    labelled (gyrationComponentLabel comp + ":") (coeffNumberBox (UiIds.gyrationComponentBox (gyrationComponentCode comp)) 100.0 value (fun v ->
                        dispatch (EditorMsg (SetGyrationComponent (comp, RhoValue v)))))))
        ] :> IView
    | DispersiveComponents ->
        StackPanel.create [
            StackPanel.orientation Orientation.Vertical
            StackPanel.spacing 4.0
            StackPanel.children (
                gyrationComponents m.editor.gyrationDispersion
                |> List.map (fun (comp, formula) ->
                    let code = gyrationComponentCode comp
                    StackPanel.create [
                        StackPanel.orientation Orientation.Vertical
                        StackPanel.spacing 2.0
                        StackPanel.children [
                            labelBlock (gyrationComponentLabel comp + " (dispersion formula):")
                            componentFormulaView (UiIds.gyrationComponentFormulaEditor code) (UiIds.gyrationComponentFormulaBox code) formula (fun f ->
                                dispatch (EditorMsg (SetGyrationComponentDispersion (comp, f))))
                        ]
                    ] :> IView))
        ] :> IView

/// The symmetry-class gyration panel (the activity rung): the class picker CONSTRAINED by the
/// anisotropy choice, the Constant/Dispersive component sub-toggle, and the handedness switch.
let private gyrationPanel (m : Model) (dispatch : Msg -> unit) : IView =
    let currentCode = gyrationClassCode m.editor.gyration
    let dispersive = m.editor.activityDispersion = DispersiveComponents
    StackPanel.create [
        StackPanel.orientation Orientation.Vertical
        StackPanel.spacing 2.0
        StackPanel.children [
            labelBlock "Gyration symmetry class:"
            WrapPanel.create [
                automationId UiIds.gyrationClassPicker
                WrapPanel.orientation Orientation.Horizontal
                WrapPanel.children (
                    availableGyrationClasses m.editor.anisotropy
                    |> List.map (fun offered ->
                        let code = gyrationClassCode offered
                        clickBox (UiIds.gyrationClassOption code) (gyrationClassLabel offered) (currentCode = code) (fun () ->
                            dispatch (EditorMsg (ChooseGyrationClass offered)))))
            ] :> IView
            labelBlock "Gyration component model (off = constant g):"
            WrapPanel.create [
                WrapPanel.orientation Orientation.Horizontal
                WrapPanel.children [
                    clickBox UiIds.activityDispersiveToggle "Dispersive components" dispersive (fun () ->
                        dispatch (EditorMsg (SetActivityDispersion (if dispersive then ConstantComponents else DispersiveComponents))))
                ]
            ] :> IView
            labelBlock "Gyration components (the symmetry-allowed g₍ᵢⱼ₎, typically ~10⁻⁵):"
            gyrationComponentsSection m dispatch
            labelBlock "Handedness (the enantiomorph — one overall sign):"
            WrapPanel.create [
                automationId UiIds.handednessSwitch
                WrapPanel.orientation Orientation.Horizontal
                WrapPanel.children (
                    [ RightHanded; LeftHanded ]
                    |> List.map (fun hand ->
                        clickBox (UiIds.handednessOption (handednessCode hand)) (handednessCode hand) (m.editor.hand = hand) (fun () ->
                            dispatch (EditorMsg (SetHandedness hand)))))
            ] :> IView
        ]
    ] :> IView

/// The Polder-mu panel (the magnetic rung): the Constant/Dispersive component sub-toggle,
/// then — under Constant — scalar vs gyromagnetic + the constant component entries, or —
/// under Dispersive — one dispersion-formula editor per Polder component (always the full
/// tensor, as the engine's `MuWithDispValue` has no scalar case); the magnetization-axis
/// picker follows whenever the tensor is the full gyromagnetic one.
let private muPanel (m : Model) (dispatch : Msg -> unit) : IView =
    let kind = m.editor.muKind
    let dispersive = m.editor.magneticDispersion = DispersiveComponents
    let (MuValue muDiagonal) = m.editor.polder.muDiagonal
    let (MuValue muParallel) = m.editor.polder.muParallel
    let (MuValue muGyration) = m.editor.polder.gyration
    // The magnetization-axis picker: the axis is one physical choice shared by both facets
    // (`ChooseGyrationAxis` sets it on each), so its highlight reads `m.editor.polder.axis`.
    let axisRow (show : bool) : IView list =
        if not show then []
        else
            [
                labelBlock "Magnetization axis (Z = Faraday; X/Y = Voigt):"
                WrapPanel.create [
                    automationId UiIds.gyrationAxisPicker
                    WrapPanel.orientation Orientation.Horizontal
                    WrapPanel.children (
                        [ AlongX; AlongY; AlongZ ]
                        |> List.map (fun axis ->
                            clickBox (UiIds.gyrationAxisOption (gyrationAxisCode axis)) (gyrationAxisCode axis) (m.editor.polder.axis = axis) (fun () ->
                                dispatch (EditorMsg (ChooseGyrationAxis axis)))))
                ] :> IView
            ]
    let subToggleRow : IView =
        WrapPanel.create [
            WrapPanel.orientation Orientation.Horizontal
            WrapPanel.children [
                clickBox UiIds.magneticDispersiveToggle "Dispersive components" dispersive (fun () ->
                    dispatch (EditorMsg (SetMagneticDispersion (if dispersive then ConstantComponents else DispersiveComponents))))
            ]
        ] :> IView
    let constantBody : IView list =
        let kindOptions =
            [ ScalarMuKind; GyromagneticMuKind ]
            |> List.map (fun candidate ->
                clickBox (UiIds.muKindOption (muKindCode candidate)) (muKindCode candidate) (kind = candidate) (fun () ->
                    dispatch (EditorMsg (SetMuKind candidate))))
        let componentBoxes =
            [ labelled "μ (diagonal):" (numberBox UiIds.muDiagonalBox 70.0 muDiagonal (fun v -> dispatch (EditorMsg (SetMuDiagonal (MuValue v))))) ]
            @ (match kind with
               | GyromagneticMuKind ->
                   [
                       labelled "μ (parallel):" (numberBox UiIds.muParallelBox 70.0 muParallel (fun v -> dispatch (EditorMsg (SetMuParallel (MuValue v)))))
                       labelled "g (gyration):" (numberBox UiIds.muGyrationBox 70.0 muGyration (fun v -> dispatch (EditorMsg (SetMuGyration (MuValue v)))))
                   ]
               | ScalarMuKind -> [])
        [ WrapPanel.create [ WrapPanel.orientation Orientation.Horizontal; WrapPanel.children (kindOptions @ componentBoxes) ] :> IView ]
        @ axisRow (kind = GyromagneticMuKind)
    let dispersiveBody : IView list =
        // The dispersive Polder is always the full tensor, so every component gets a
        // formula editor and the axis always applies.
        let editors =
            [
                "muDiagonal", "μ (diagonal, dispersion formula):", m.editor.polderDispersion.muDiagonal, (fun (f : DispersionFormula) -> SetMuDiagonalDispersion f)
                "muParallel", "μ (parallel, dispersion formula):", m.editor.polderDispersion.muParallel, (fun f -> SetMuParallelDispersion f)
                "muGyration", "g (gyration, dispersion formula):", m.editor.polderDispersion.gyration, (fun f -> SetMuGyrationDispersion f)
            ]
            |> List.map (fun (code, label, formula, toMsg) ->
                StackPanel.create [
                    StackPanel.orientation Orientation.Vertical
                    StackPanel.spacing 2.0
                    StackPanel.children [
                        labelBlock label
                        componentFormulaView (UiIds.polderComponentFormulaEditor code) (UiIds.polderComponentFormulaBox code) formula (fun f -> dispatch (EditorMsg (toMsg f)))
                    ]
                ] :> IView)
        editors @ axisRow true
    StackPanel.create [
        StackPanel.orientation Orientation.Vertical
        StackPanel.spacing 2.0
        StackPanel.children (
            [ labelBlock "Polder μ:"; subToggleRow ]
            @ (match m.editor.magneticDispersion with
               | ConstantComponents -> constantBody
               | DispersiveComponents -> dispersiveBody))
    ] :> IView

// -- the live preview (the step-32 tabbed multi-curve dual-axis charts, embedded) --------------

/// Lower the per-series hidden state (`hiddenSeries`, keyed by tab code + series name) onto a chart's
/// paired style seed, hiding a curve the user toggled off — preview-only, never the derived model.
/// Exposed (non-private) so the lowering is proved against the REAL mapping (spec 0038 032 retry) —
/// a test asserts `applyHidden` flips the toggled curve's exact index, rather than re-deriving it.
let applyHidden (hidden : Set<string>) (tabCode : string) (chart : ExperimentChart) (style : ChartStyle.ChartStyleState) : ChartStyle.ChartStyleState =
    chart.series
    |> List.mapi (fun i s -> i, UiIds.seriesToggle tabCode s.name)
    |> List.fold (fun st (i, key) -> if Set.contains key hidden then ChartStyle.setSeriesVisible i false st else st) style

/// One preview tab: a row of per-series show/hide toggles pinned above the embedded dual-axis chart,
/// which fills the rest of the tab. The chart embeds the ONE shared ScottPlot control under `hostId`
/// (`EmbeddedChart`, degrading to a placeholder rather than throwing headlessly); the toggles flip
/// `hiddenSeries`, which `applyHidden` lowers onto the chart's style. `tabCode` namespaces the
/// per-series toggle ids so a curve name shared across tabs never collides.
let private chartTab (tabId : string) (tabCode : string) (header : string) (hostId : string) (m : Model) (dispatch : Msg -> unit) (chart : ExperimentChart) (style : ChartStyle.ChartStyleState) : IView =
    let toggles : IView =
        WrapPanel.create [
            WrapPanel.orientation Orientation.Horizontal
            WrapPanel.children (
                chart.series
                |> List.map (fun s ->
                    let key = UiIds.seriesToggle tabCode s.name
                    clickBox key s.name (not (Set.contains key m.hiddenSeries)) (fun () ->
                        dispatch (ToggleSeriesVisibility key))))
        ] :> IView
    let host = OpticalConstructor.Controls.EmbeddedChart.create hostId chart (applyHidden m.hiddenSeries tabCode chart style)
    TabItem.create [
        automationId<TabItem> tabId
        TabItem.header header
        TabItem.content (
            DockPanel.create [
                DockPanel.children [
                    Border.create [ Border.dock Dock.Top; Border.padding (thickLR 4.0 4.0); Border.child toggles ]
                    Border.create [ Border.padding (thick 4.0); Border.child host ]
                ]
            ])
    ] :> IView

/// The tabbed preview pane (the right pane): the n/k tab ALWAYS, then the Gyration tab exactly when
/// the entry is optically active and the μ tab exactly when magnetic — so the visible tabs plus the
/// per-tab legend always answer what is being drawn. In edit mode the optional tabs track the ladder
/// toggles; a view-only preset (no ladder) is classified from its assembled tensors instead. Returns
/// the pane view and the gain-warning text over the n/k tab's sampled k curves.
let private previewPane (m : Model) (dispatch : Msg -> unit) : IView * string =
    match previewProperties m with
    | None ->
        let placeholder =
            TextBlock.create [
                TextBlock.text "no preview — the ladder is not derivable (see the readout on the left)"
                TextBlock.foreground (brush hintColor)
                TextBlock.verticalAlignment VerticalAlignment.Center
                TextBlock.horizontalAlignment HorizontalAlignment.Center
            ] :> IView
        placeholder, ""
    | Some o ->
        let nkChart = NkDispersionChart.nkDispersionChart o Nanometer previewRange
        let showGyration =
            match m.mode with
            | EditableMaterial -> m.editor.activity = ActivityOn
            | ViewOnlyMaterial _ -> NkDispersionChart.hasGyration o
        let showMu =
            match m.mode with
            | EditableMaterial -> m.editor.magnetic = MagneticOn
            | ViewOnlyMaterial _ -> NkDispersionChart.hasMagnetic o
        let tabs =
            [ chartTab UiIds.nkTab "nk" "n, k" UiIds.previewChart m dispatch nkChart (NkDispersionChart.nkDispersionStyle nkChart) ]
            @ (if showGyration then
                   let g = NkDispersionChart.gyrationChart o Nanometer previewRange
                   [ chartTab UiIds.gyrationTab "gyration" "Gyration" UiIds.gyrationChart m dispatch g (NkDispersionChart.gyrationStyle g) ]
               else [])
            @ (if showMu then
                   let mu = NkDispersionChart.muChart o Nanometer previewRange
                   [ chartTab UiIds.muTab "mu" "μ (Polder)" UiIds.muChart m dispatch mu (NkDispersionChart.muStyle mu) ]
               else [])
        let pane =
            TabControl.create [
                automationId<TabControl> UiIds.previewTabs
                TabControl.viewItems tabs
            ] :> IView
        pane, gainWarningOf (Some nkChart)

/// The two-pane split: the identity / ladder pane (left) and the tabbed preview (right), a vertical
/// GridSplitter between them, each pane with a sensible minimum width so neither collapses.
let private twoPane (leftPane : IView) (rightPane : IView) : IView =
    Grid.create [
        Grid.columnDefinitions "3*,Auto,2*"
        Grid.children [
            Border.create [ Border.column 0; Border.minWidth 520.0; Border.child leftPane ]
            GridSplitter.create [
                automationId<GridSplitter> UiIds.splitter
                GridSplitter.column 1
                GridSplitter.width 6.0
                GridSplitter.resizeDirection GridResizeDirection.Columns
                GridSplitter.background (brush idleBorder)
            ]
            Border.create [ Border.column 2; Border.minWidth 380.0; Border.child rightPane ]
        ]
    ] :> IView

// -- readouts / actions ------------------------------------------------------------------------

let private summaryRow (m : Model) : IView =
    StackPanel.create [
        StackPanel.orientation Orientation.Horizontal
        StackPanel.spacing 6.0
        StackPanel.children [
            labelBlock "Derived model:"
            TextBlock.create [
                TextBlock.name UiIds.summaryText
                TextBlock.text (complexitySummary m)
                TextBlock.verticalAlignment VerticalAlignment.Center
            ] :> IView
        ]
    ] :> IView

let private gainWarningRow (warning : string) : IView =
    TextBlock.create [
        TextBlock.name UiIds.gainWarning
        TextBlock.foreground (brush warningColor)
        TextBlock.text warning
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

let private saveCancelRow (dispatch : Msg -> unit) (withSave : bool) : IView =
    StackPanel.create [
        StackPanel.orientation Orientation.Horizontal
        StackPanel.spacing 0.0
        StackPanel.children (
            (if withSave then [ actionButton UiIds.saveButton "Save" saveBackground (fun () -> dispatch SaveClicked) ] else [])
            @ [ actionButton UiIds.cancelButton (if withSave then "Cancel" else "Close") cancelBackground (fun () -> dispatch CancelClicked) ])
    ] :> IView

/// The whole editor — a two-pane vertical split (spec 0038 step 032). Edit mode: LEFT pane = the
/// identity rows + the progressive ladder inside a `ScrollViewer`, with the derived-model / gain /
/// status readouts and the Save/Cancel actions pinned below it; RIGHT pane = the full-height tabbed
/// preview (per-axis n/k always, Gyration when active, μ when magnetic). View-only mode (an entry
/// whose physics is not data): the identity header + the WHY note (scrolling) with a Close action,
/// beside the same tabbed preview — NO ladder, NO Save.
let view (m : Model) (dispatch : Msg -> unit) : IView =
    let previewView, warning = previewPane m dispatch
    let scrolling (content : IView) : IView =
        Border.create [
            Border.padding (thick 8.0)
            Border.child (ScrollViewer.create [ ScrollViewer.content content ])
        ] :> IView
    match m.mode with
    | ViewOnlyMaterial reason ->
        let leftPane =
            DockPanel.create [
                DockPanel.children [
                    Border.create [ Border.dock Dock.Bottom; Border.padding (thickLR 8.0 6.0); Border.child (saveCancelRow dispatch false) ]
                    scrolling (
                        StackPanel.create [
                            StackPanel.orientation Orientation.Vertical
                            StackPanel.spacing 6.0
                            StackPanel.children [
                                labelBlock $"%s{m.name} — %s{categoryCode m.category}"
                                TextBlock.create [ TextBlock.text m.description; TextBlock.foreground (brush hintColor) ] :> IView
                                TextBlock.create [
                                    TextBlock.name UiIds.viewOnlyNote
                                    TextBlock.text reason
                                    TextBlock.foreground (brush errorColor)
                                ] :> IView
                            ]
                        ] :> IView)
                ]
            ] :> IView
        twoPane leftPane previewView
    | EditableMaterial ->
        let ladderPanels =
            (match m.editor.dispersion with
             | NonDispersive -> [ indexFieldsRow m dispatch ]
             | DispersiveSegments -> [ segmentsPanel m dispatch ])
            @ (match m.editor.activity with
               | ActivityOn -> [ gyrationPanel m dispatch ]
               | ActivityOff -> [])
            @ (match m.editor.magnetic with
               | MagneticOn -> [ muPanel m dispatch ]
               | MagneticOff -> [])
        let leftPane =
            DockPanel.create [
                DockPanel.children [
                    Border.create [ Border.dock Dock.Bottom; Border.padding (thickLR 8.0 6.0); Border.child (saveCancelRow dispatch true) ]
                    Border.create [ Border.dock Dock.Bottom; Border.padding (thickLR 8.0 0.0); Border.child (statusRow m) ]
                    Border.create [ Border.dock Dock.Bottom; Border.padding (thickLR 8.0 0.0); Border.child (gainWarningRow warning) ]
                    Border.create [ Border.dock Dock.Bottom; Border.padding (thickLR 8.0 2.0); Border.child (summaryRow m) ]
                    scrolling (
                        StackPanel.create [
                            StackPanel.orientation Orientation.Vertical
                            StackPanel.spacing 6.0
                            StackPanel.children (
                                [
                                    nameRow m dispatch
                                    descriptionRow m dispatch
                                    categoryRow m dispatch
                                    anisotropyRow m dispatch
                                    togglesRow m dispatch
                                ]
                                @ ladderPanels)
                        ] :> IView)
                ]
            ] :> IView
        twoPane leftPane previewView
