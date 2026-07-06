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
/// through `MaterialProxy` — `addMaterial` minting `MaterialId.create` for a new entry,
/// `updateMaterial` for an existing one — storing `complexity = Some model` with
/// `properties = model.toProperties`; entries with `complexity = None` (silicon, langasite)
/// open VIEW-ONLY and offer no Edit affordance (no ladder, no Save). Pure: `update` only
/// reaches IO through the context's proxy fields.
module OpticalConstructor.TestWindows.MaterialEditorView

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
    [<Literal>]
    let dispersiveToggle = "DispersiveToggle"
    [<Literal>]
    let activeToggle = "ActiveToggle"
    [<Literal>]
    let magneticToggle = "MagneticToggle"
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
    /// An anisotropy option, by the choice's stable code.
    let anisotropyOption (code : string) : string = "AnisotropyOption_" + code
    /// A material-category option, by the category's stable code.
    let categoryOption (code : string) : string = "MaterialCategoryOption_" + code
    /// A principal-index entry (1-based axis slot; present per the anisotropy choice).
    let indexBox (axisNumber : int) : string = sprintf "PrincipalIndexBox_%d" axisNumber
    /// A principal absorption (k) entry (present only while the absorbing rung is unlocked).
    let absorptionBox (axisNumber : int) : string = sprintf "AbsorptionIndexBox_%d" axisNumber
    /// Segment 0's model picker carries the mandated literal; later segments carry the
    /// indexed member of the same family (the 022 RepeatCountStepper precedent).
    let segmentModelPicker (segmentIndex : int) : string =
        if segmentIndex = 0 then dispersionModelPicker else sprintf "DispersionModelPicker_%d" segmentIndex
    let segmentLowerBox (segmentIndex : int) : string = sprintf "SegmentLowerBox_%d" segmentIndex
    let segmentUpperBox (segmentIndex : int) : string = sprintf "SegmentUpperBox_%d" segmentIndex
    let segmentRemoveButton (segmentIndex : int) : string = sprintf "SegmentRemoveButton_%d" segmentIndex
    /// A segment's dispersion-model option, by segment index and the model kind's code.
    let modelOption (segmentIndex : int) (code : string) : string = sprintf "DispersionModelOption_%d_%s" segmentIndex code
    /// A gyration-class option, by the class's stable code.
    let gyrationClassOption (code : string) : string = "GyrationClassOption_" + code
    /// A handedness option ("Right" / "Left").
    let handednessOption (code : string) : string = "HandednessOption_" + code
    /// A magnetization-axis option ("X" / "Y" / "Z").
    let gyrationAxisOption (code : string) : string = "GyrationAxisOption_" + code
    /// A Polder-mu kind option ("Scalar" / "Gyromagnetic").
    let muKindOption (code : string) : string = "MuKindOption_" + code

/// What Save targets: a brand-new entry (mint a fresh `MaterialId`) or an existing one (keep
/// its id and update in place). A DU, not a naked bool.
type EditorTarget =
    | NewMaterial
    | ExistingMaterial of MaterialId

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
        category : MaterialCategory
        editor : MaterialComplexityEditState
        /// A view-only entry's own engine properties (the preview's source there).
        presetProperties : OpticalPropertiesWithDisp option
        /// The last typed-error reason (or proxy rejection) surfaced to the user.
        status : string option
    }

type Msg =
    | SetName of string
    | SetDescription of string
    | ChooseCategory of MaterialCategory
    /// One ladder edit, routed through the pure Domain apply.
    | EditorMsg of MaterialComplexityMsg
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

/// A material category's stable code (the derived UiIds key).
let categoryCode (category : MaterialCategory) : string =
    match category with
    | Glass -> "Glass"
    | Metal -> "Metal"
    | Semiconductor -> "Semiconductor"
    | Crystal -> "Crystal"
    | Vacuum -> "Vacuum"

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
    | SegmentNotLowerable reason
    | NoSuchSegment reason
    | LastSegmentNotRemovable reason
    | UnsupportedComplexity reason -> reason

let private materialErrorReason (e : MaterialError) : string =
    match e with
    | UnknownMaterialId reason
    | DuplicateMaterialId reason
    | MaterialStillReferenced reason
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

/// The advisory gain warning over the preview's sampled k series (the restated
/// `imaginaryIndexGainWarning` rule; empty when nothing warns).
let private gainWarningOf (chartOpt : ExperimentChart option) : string =
    match chartOpt with
    | None -> ""
    | Some chart ->
        chart.series
        |> List.tryItem NkDispersionChart.kSeriesIndex
        |> Option.bind (fun s -> s.points |> List.tryPick (fun (_, k) -> imaginaryIndexGainWarning k))
        |> Option.defaultValue ""

let private epsCaseLabel (eps : EpsWithDispValue) : string =
    match eps with
    | EpsWithoutDispValue (IsotropicTransparent _) -> "isotropic transparent"
    | EpsWithoutDispValue (IsotropicAbsorbing _) -> "isotropic absorbing"
    | EpsWithoutDispValue (UniaxialTransparent _) -> "uniaxial transparent"
    | EpsWithoutDispValue (UniaxialAbsorbing _) -> "uniaxial absorbing"
    | EpsWithoutDispValue (BiaxialTransparent _) -> "biaxial transparent"
    | EpsWithoutDispValue (BiaxialAbsorbing _) -> "biaxial absorbing"
    | EpsWithDispValue (IsotropicDispersive segments) -> sprintf "isotropic dispersive (%d segments)" (List.length segments)
    | EpsWithDispValue (UniaxialDispersive segments) -> sprintf "uniaxial dispersive (%d segments)" (List.length segments)
    | EpsWithDispValue (BiaxialDispersive segments) -> sprintf "biaxial dispersive (%d segments)" (List.length segments)

/// The derived-model readout: the eps case, the optional aspects, and a structural digest —
/// two states with the SAME summary denote the SAME derived complexity (the lossless-uncheck
/// acceptance observes this text); an underivable ladder shows the typed reason instead.
let complexitySummary (m : Model) : string =
    match m.mode with
    | ViewOnlyMaterial _ -> "view-only engine preset"
    | EditableMaterial ->
        match toComplexity m.editor with
        | Error e -> sprintf "not derivable — %s" (editErrorReason e)
        | Ok c ->
            let aspect (label : string) (o : 'a option) : string =
                match o with
                | Some _ -> sprintf "%s on" label
                | None -> sprintf "%s off" label
            sprintf "eps %s; %s; %s [%08x]" (epsCaseLabel c.eps) (aspect "active" c.active) (aspect "magnetic" c.magnetic) (hash c)

// ---------------------------------------------------------------------------
// init / update (pure — IO only through the context's proxy fields).
// ---------------------------------------------------------------------------

let init (context : MaterialEditorContext) (existing : MaterialEntry option) : Model =
    let blank =
        {
            context = context
            target = NewMaterial
            mode = EditableMaterial
            name = ""
            description = ""
            category = Glass
            editor = defaultState
            presetProperties = None
            status = None
        }
    match existing with
    | None -> blank
    | Some entry ->
        let seeded =
            { blank with
                target = ExistingMaterial entry.id
                name = entry.name
                description = (match entry.description with Some d -> d | None -> "")
                category = entry.category }
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
        | Ok editor -> { m with editor = editor; status = None }
        | Error e -> { m with status = Some (editErrorReason e) }
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
                let saved =
                    match m.target with
                    | NewMaterial -> m.context.materials.addMaterial (entryUnder (newMaterialId ()))
                    | ExistingMaterial id -> m.context.materials.updateMaterial (entryUnder id)
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
/// pointer event).
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
        TextBox.text (sprintf "%g" value)
        TextBox.onTextChanged (
            (fun s ->
                match parseFloat s with
                | Some v -> onCommit v
                | None -> ()),
            SubPatchOptions.OnChangeOf (box (autoId, value)))
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
                WrapPanel.children (
                    [ Glass; Metal; Semiconductor; Crystal; Vacuum ]
                    |> List.map (fun category ->
                        clickBox (UiIds.categoryOption (categoryCode category)) (categoryCode category) (m.category = category) (fun () -> dispatch (ChooseCategory category))))
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

/// The unlock toggles. The activity toggle is REMOVED — not greyed — when the current
/// anisotropy choice offers no rotating gyration class (`availableGyrationClasses`).
let private togglesRow (m : Model) (dispatch : Msg -> unit) : IView =
    let absorbing = m.editor.transparency = Absorbing
    let dispersive = m.editor.dispersion = DispersiveSegments
    let active = m.editor.activity = ActivityOn
    let magnetic = m.editor.magnetic = MagneticOn
    let activityOffered = not (List.isEmpty (availableGyrationClasses m.editor.anisotropy))
    WrapPanel.create [
        WrapPanel.orientation Orientation.Horizontal
        WrapPanel.children (
            [
                clickBox UiIds.absorbingToggle "Absorbing" absorbing (fun () ->
                    dispatch (EditorMsg (SetTransparency (if absorbing then Transparent else Absorbing))))
                clickBox UiIds.dispersiveToggle "Dispersive" dispersive (fun () ->
                    dispatch (EditorMsg (SetDispersion (if dispersive then NonDispersive else DispersiveSegments))))
            ]
            @ (if activityOffered then
                   [ clickBox UiIds.activeToggle "Optically active" active (fun () ->
                         dispatch (EditorMsg (SetActivity (if active then ActivityOff else ActivityOn)))) ]
               else [])
            @ [
                clickBox UiIds.magneticToggle "Magnetic (Polder μ)" magnetic (fun () ->
                    dispatch (EditorMsg (SetMagnetic (if magnetic then MagneticOff else MagneticOn))))
            ])
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
                       labelled (sprintf "k%d:" number) (numberBox (UiIds.absorptionBox number) 70.0 im (fun v ->
                           dispatch (EditorMsg (SetPrincipalIndex (slot, ComplexRefractionIndex (createComplex re v))))))
                   ]
               | Transparent -> []))
    WrapPanel.create [
        WrapPanel.orientation Orientation.Horizontal
        WrapPanel.children boxes
    ] :> IView

/// One dispersion segment's editor: the shared wavelengthInterval bounds (nm) and the model
/// picker over the full catalogue (the picked kind is highlighted).
let private segmentView (m : Model) (dispatch : Msg -> unit) (segmentIndex : int) (segment : EditSegment) : IView =
    let lowerNm = wavelengthToUnit Nanometer segment.interval.lower
    let upperNm = wavelengthToUnit Nanometer segment.interval.upper
    let currentCode = modelKindCode segment.model1
    StackPanel.create [
        StackPanel.orientation Orientation.Vertical
        StackPanel.spacing 2.0
        StackPanel.children [
            WrapPanel.create [
                WrapPanel.orientation Orientation.Horizontal
                WrapPanel.children [
                    labelBlock (sprintf "Segment %d —" segmentIndex)
                    labelled "λ from (nm):" (numberBox (UiIds.segmentLowerBox segmentIndex) 70.0 lowerNm (fun v ->
                        dispatch (EditorMsg (SetSegmentInterval (segmentIndex, { lower = toWaveLength Nanometer v; upper = segment.interval.upper })))))
                    labelled "to (nm):" (numberBox (UiIds.segmentUpperBox segmentIndex) 70.0 upperNm (fun v ->
                        dispatch (EditorMsg (SetSegmentInterval (segmentIndex, { lower = segment.interval.lower; upper = toWaveLength Nanometer v })))))
                    verbButton (UiIds.segmentRemoveButton segmentIndex) "Remove" (List.length m.editor.segments > 1) (fun () ->
                        dispatch (EditorMsg (RemoveSegment segmentIndex)))
                ]
            ] :> IView
            WrapPanel.create [
                automationId (UiIds.segmentModelPicker segmentIndex)
                WrapPanel.orientation Orientation.Horizontal
                WrapPanel.children (
                    defaultModelChoices
                    |> List.map (fun candidate ->
                        let code = modelKindCode candidate
                        clickBox (UiIds.modelOption segmentIndex code) (modelKindLabel candidate) (currentCode = code) (fun () ->
                            dispatch (EditorMsg (ChooseSegmentModel (segmentIndex, candidate))))))
            ] :> IView
        ]
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

/// The symmetry-class gyration panel (the activity rung): the class picker CONSTRAINED by the
/// anisotropy choice, and the handedness switch.
let private gyrationPanel (m : Model) (dispatch : Msg -> unit) : IView =
    let currentCode = gyrationClassCode m.editor.gyration
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

/// The Polder-mu panel (the magnetic rung): scalar vs gyromagnetic, the component entries,
/// and — gyromagnetic only — the magnetization-axis picker.
let private muPanel (m : Model) (dispatch : Msg -> unit) : IView =
    let kind = m.editor.muKind
    let (MuValue muDiagonal) = m.editor.polder.muDiagonal
    let (MuValue muParallel) = m.editor.polder.muParallel
    let (MuValue muGyration) = m.editor.polder.gyration
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
    let axisRow =
        match kind with
        | GyromagneticMuKind ->
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
        | ScalarMuKind -> []
    StackPanel.create [
        StackPanel.orientation Orientation.Vertical
        StackPanel.spacing 2.0
        StackPanel.children (
            [
                labelBlock "Polder μ:"
                WrapPanel.create [ WrapPanel.orientation Orientation.Horizontal; WrapPanel.children (kindOptions @ componentBoxes) ] :> IView
            ]
            @ axisRow)
    ] :> IView

// -- the live preview (the step-19 dual-axis n/k chart, inline) -------------------------------

/// The inline dual-axis rendering of the step-19 chart — the ONE shared canvas renderer
/// (`NkDispersionChart.inlineCanvas`, spec 0033 step 024: the Materials workbench's View panel
/// draws through it too), under this editor's stable preview id.
let private previewCanvas (chart : ExperimentChart) : IView =
    NkDispersionChart.inlineCanvas UiIds.previewChart chart

let private previewSection (m : Model) : IView * string =
    let chartOpt = previewProperties m |> Option.map (fun p -> NkDispersionChart.nkDispersionChart p Nanometer previewRange)
    let view =
        match chartOpt with
        | Some chart -> previewCanvas chart
        | None ->
            TextBlock.create [
                TextBlock.text "no preview — the ladder is not derivable (see the readout below)"
                TextBlock.foreground (brush hintColor)
            ] :> IView
    view, gainWarningOf chartOpt

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

/// The whole editor. Edit mode: identity rows + the ladder on top, the readouts / actions
/// pinned to the bottom, the live preview filling the centre. View-only mode (an entry whose
/// physics is not data): the identity header + the WHY note + the preview — NO ladder, NO Save.
let view (m : Model) (dispatch : Msg -> unit) : IView =
    let previewView, warning = previewSection m
    match m.mode with
    | ViewOnlyMaterial reason ->
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
                                labelBlock (sprintf "%s — %s" m.name (categoryCode m.category))
                                TextBlock.create [ TextBlock.text m.description; TextBlock.foreground (brush hintColor) ] :> IView
                                TextBlock.create [
                                    TextBlock.name UiIds.viewOnlyNote
                                    TextBlock.text reason
                                    TextBlock.foreground (brush errorColor)
                                ] :> IView
                            ]
                        ])
                ]
                Border.create [ Border.dock Dock.Bottom; Border.padding (thickLR 8.0 6.0); Border.child (saveCancelRow dispatch false) ]
                Border.create [ Border.padding (thick 8.0); Border.child previewView ]
            ]
        ] :> IView
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
        DockPanel.create [
            DockPanel.children [
                Border.create [
                    Border.dock Dock.Top
                    Border.padding (thick 8.0)
                    Border.child (
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
                        ])
                ]
                Border.create [ Border.dock Dock.Bottom; Border.padding (thickLR 8.0 6.0); Border.child (saveCancelRow dispatch true) ]
                Border.create [ Border.dock Dock.Bottom; Border.padding (thickLR 8.0 0.0); Border.child (statusRow m) ]
                Border.create [ Border.dock Dock.Bottom; Border.padding (thickLR 8.0 0.0); Border.child (gainWarningRow warning) ]
                Border.create [ Border.dock Dock.Bottom; Border.padding (thickLR 8.0 2.0); Border.child (summaryRow m) ]
                Border.create [ Border.padding (thick 8.0); Border.child previewView ]
            ]
        ] :> IView
