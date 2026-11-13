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
/// `imaginaryIndexGainWarning` rule, restated in Domain as `ExtinctionCoefficient.gainWarning` — Ui/Validation.fs:92). Save persists
/// through `MaterialProxy`, routing on the target's `EntryFreshness` (spec 0038 step 008):
/// `addMaterial` for a `NewUnsaved` entry — whose `MaterialId` was minted AT WINDOW OPEN
/// (`MaterialEditorIntent.NewMaterial`), off the save path — and `updateMaterial` for a
/// `Persisted` one; both store the derived model as the entry's physics. An entry whose
/// physics is not editable term data (silicon, langasite) opens VIEW-ONLY and offers no
/// Edit affordance (no ladder, no Save). Pure: `update` only reaches IO through the
/// context's proxy fields.
module OpticalConstructor.Ui.MaterialEditorView

open System
open System.Globalization
open Avalonia
open Avalonia.Controls
open Avalonia.Controls.Shapes
open Avalonia.Layout
open Avalonia.Media
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
open OpticalConstructor.Ui.NkDispersionChart

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
type MaterialEditorTarget =
    {
        materialId : MaterialId
        freshness : WindowLauncher.EntryFreshness
    }

/// Whether the window edits or only shows. `ViewOnlyMaterial` carries the WHY (a physics
/// that is not editable term data, or one this editor cannot seed) and the physics it shows
/// (spec 0050 S11.1): such an entry offers NO Edit affordance — the ladder and Save are REMOVED, not greyed.
type EditorMode =
    | EditableMaterial
    | ViewOnlyMaterial of reason : ControlCaption * shown : MaterialPhysics

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

/// Whether the editor is editing normally or showing the unsaved-edit exit confirm surface
/// (spec 0038 step 033). A named two-case DU, never a naked `bool`: a dirty editor being closed
/// through Cancel or the window chrome flips to `ConfirmingDiscard`, which offers Discard / Keep.
type MaterialEditorExitPrompt =
    | MaterialEditing
    | MaterialConfirmingDiscard

/// The structurally-comparable slice of the model Save persists — the identity facets and the
/// ladder edit state (spec 0038 step 033's dirtiness snapshot). Every component is a plain
/// record/DU with structural equality; the reference-compared context, the mode and the
/// pure-UI state (hidden series, status) are excluded.
type MaterialEditSnapshot = EntryName * EntryDescription * CategoryId * MaterialComplexityEditState

/// One tab of the live preview (spec 0038 step 032). Its key namespaces the per-series toggle ids, so a
/// curve name shared across tabs never collides.
type MaterialPreviewTab =
    | NkPreviewTab
    | GyrationPreviewTab
    | MuPreviewTab

    member this.uiKey : ConstructorUiKey =
        match this with
        | NkPreviewTab -> ConstructorUiKey "nk"
        | GyrationPreviewTab -> ConstructorUiKey "gyration"
        | MuPreviewTab -> ConstructorUiKey "mu"

    /// The tab's header.
    member this.caption : ControlCaption =
        match this with
        | NkPreviewTab -> ControlCaption "n, k"
        | GyrationPreviewTab -> ControlCaption "Gyration"
        | MuPreviewTab -> ControlCaption "μ (Polder)"

    member this.tabId : ConstructorUiId =
        match this with
        | NkPreviewTab -> ConstructorUiId.materialPreviewNkTab
        | GyrationPreviewTab -> ConstructorUiId.materialPreviewGyrationTab
        | MuPreviewTab -> ConstructorUiId.materialPreviewMuTab

    /// The host of the tab's embedded chart.
    member this.chartId : ConstructorUiId =
        match this with
        | NkPreviewTab -> ConstructorUiId.materialPreviewChart
        | GyrationPreviewTab -> ConstructorUiId.materialGyrationChart
        | MuPreviewTab -> ConstructorUiId.materialMuChart

/// The editor's model: the entry identity facets (name / category / description), the pure
/// Domain ladder state and the mode (a view-only entry's physics rides `ViewOnlyMaterial`).
/// Reference-compared, so the Elmish equality gate sees every fresh record `update` returns as a
/// change and re-renders.
[<ReferenceEquality>]
type MaterialEditorModel =
    {
        context : MaterialEditorContext
        target : MaterialEditorTarget
        mode : EditorMode
        name : EntryName
        description : EntryDescription
        category : CategoryId
        editor : MaterialComplexityEditState
        /// Spec 0038 (032): the preview curves the user hid, by their `ConstructorUiId.previewSeriesToggle` id
        /// (tab + series name, so a toggle in one tab never hides a same-named curve in another).
        hiddenSeries : Set<ConstructorUiId>
        /// Spec 0038 (033): the edit snapshot captured at load — dirtiness is `initialEdit`
        /// structurally unequal to the current edit slice (no dirty flag maintained).
        initialEdit : MaterialEditSnapshot
        /// Spec 0038 (033): normal editing vs the unsaved-edit exit confirm surface.
        exit : MaterialEditorExitPrompt
        /// The last typed-error reason (or proxy rejection) surfaced to the user.
        status : ControlCaption option
    }

type MaterialEditorMsg =
    | SetMaterialName of EntryName
    | SetMaterialDescription of EntryDescription
    | ChooseCategory of CategoryId
    /// One ladder edit, routed through the pure Domain apply.
    | EditorMsg of MaterialComplexityMsg
    /// Spec 0038 (032): flip one preview curve's visibility, by its `ConstructorUiId.previewSeriesToggle` id.
    | ToggleSeriesVisibility of key : ConstructorUiId
    | MaterialSaveClicked
    /// Requested exit (the Cancel button, or the window chrome routed through `OnClosing`):
    /// closes a pristine editor silently, shows the discard confirm on a dirty one (spec 0038 step 033).
    | MaterialCancelClicked
    /// Spec 0038 (033): the confirm surface's Discard changes — close without saving.
    | MaterialDiscardConfirmed
    /// Spec 0038 (033): the confirm surface's Keep editing — dismiss the confirm, stay in the editor.
    | MaterialKeepEditing

// ---------------------------------------------------------------------------
// Pure helpers.
// ---------------------------------------------------------------------------

/// The live preview's fixed spectral window: the visible band in canonical meters (the
/// display unit is nm; AC-D7 — the unit never changes the sampled range).
let previewRange : Range<WaveLength> = SpectralAxis.spectralRange Nanometer (UnitMagnitude 400.0) (UnitMagnitude 700.0) (NumberOfPoints 60)

type EpsWithDispValue with

    /// The eps case as the derived-model readout names it.
    member private this.editorCaseCaption : ControlCaption =
        match this with
        | EpsWithoutDispValue (IsotropicTransparent _) -> ControlCaption "isotropic transparent"
        | EpsWithoutDispValue (IsotropicAbsorbing _) -> ControlCaption "isotropic absorbing"
        | EpsWithoutDispValue (UniaxialTransparent _) -> ControlCaption "uniaxial transparent"
        | EpsWithoutDispValue (UniaxialAbsorbing _) -> ControlCaption "uniaxial absorbing"
        | EpsWithoutDispValue (BiaxialTransparent _) -> ControlCaption "biaxial transparent"
        | EpsWithoutDispValue (BiaxialAbsorbing _) -> ControlCaption "biaxial absorbing"
        | EpsWithDispValue (IsotropicDispersive segments) -> ControlCaption $"isotropic dispersive (%d{List.length segments} segments)"
        | EpsWithDispValue (UniaxialDispersive segments) -> ControlCaption $"uniaxial dispersive (%d{List.length segments} segments)"
        | EpsWithDispValue (BiaxialDispersive segments) -> ControlCaption $"biaxial dispersive (%d{List.length segments} segments)"

type ExperimentChart with

    /// The advisory gain warning over the n/k chart's sampled k curves — every per-axis k series (the
    /// restated `ExtinctionCoefficient.gainWarning` rule; empty when nothing warns).
    member private this.gainWarningCaption : ControlCaption =
        this.series
        |> List.filter (fun s -> s.name.value.StartsWith "k")
        |> List.tryPick (fun s -> s.points |> List.tryPick (fun p -> (ExtinctionCoefficient p.y).gainWarning))
        |> Option.map ControlCaption
        |> Option.defaultValue (ControlCaption "")

/// One principal axis of a dispersive segment, as the segment editor lays it out (spec 0033 comment 009).
type private DispersionSegmentAxis =
    {
        slot : PrincipalAxisSlot
        /// The axis's heading; `None` for the single isotropic axis.
        captionOpt : ControlCaption option
        /// The key the axis's ids carry; `None` for the first axis, which keeps the single-axis ids.
        laterAxisOpt : ConstructorUiKey option
    }

type Anisotropy with

    /// The principal axes a dispersive segment exposes (spec 0033 comment 009): isotropic → one
    /// unlabelled axis; uniaxial → ordinary + extraordinary; biaxial → x/y/z.
    member private this.segmentAxes : DispersionSegmentAxis list =
        match this with
        | Isotropic -> [ { slot = FirstAxis; captionOpt = None; laterAxisOpt = None } ]
        | Uniaxial ->
            [
                { slot = FirstAxis; captionOpt = Some (ControlCaption "Ordinary (o):"); laterAxisOpt = None }
                { slot = SecondAxis; captionOpt = Some (ControlCaption "Extraordinary (e):"); laterAxisOpt = Some (ConstructorUiKey "e") }
            ]
        | Biaxial ->
            [
                { slot = FirstAxis; captionOpt = Some (ControlCaption "X:"); laterAxisOpt = None }
                { slot = SecondAxis; captionOpt = Some (ControlCaption "Y:"); laterAxisOpt = Some (ConstructorUiKey "y") }
                { slot = ThirdAxis; captionOpt = Some (ControlCaption "Z:"); laterAxisOpt = Some (ConstructorUiKey "z") }
            ]

type MaterialEditorModel with

    /// The create picker's category choices (spec 0035 step 009): the LIVE catalogue from the context's
    /// `CategoryProxy` (`listCategories`, re-read at render), EXCLUDING every `HiddenOnCreate` category
    /// (Vacuum) — those categorise seed entries but are not user-selectable creation targets, so they
    /// are REMOVED from the picker, not greyed. A store listing error yields no options (never a throw).
    member this.selectableCategories : MaterialCategory list =
        match this.context.categories.listCategories () with
        | Ok cats -> cats |> List.filter (fun c -> c.visibility <> HiddenOnCreate)
        | Error _ -> []

    /// What the preview charts: the derived complexity's engine properties in edit mode, the
    /// shown physics' in view-only mode, nothing while the ladder is underivable.
    member internal this.previewProperties : OpticalPropertiesWithDisp option =
        match this.mode with
        | ViewOnlyMaterial (_, shown) -> Some shown.toProperties
        | EditableMaterial ->
            match this.editor.toComplexity with
            | Ok c -> Some c.toProperties
            | Error _ -> None

    /// The gyration components the preview draws (spec 0040 step 006): in edit mode the editor's own
    /// gyration class decides them; a view-only entry reads the class off the physics it shows,
    /// and every component is drawn when that physics names no gyration class. Public so the
    /// window tests verify the threading without a rendered frame.
    member this.previewGyrationComponents : GyrationComponent list =
        match this.mode with
        | EditableMaterial -> this.editor.gyration.components |> List.map fst
        | ViewOnlyMaterial (_, shown) -> shown.gyrationComponents

    /// The Polder-μ kind the preview draws (spec 0040 step 006): in edit mode the editor's `MuKind`; a
    /// view-only entry reads it off the magnetic tree of the physics it shows. Public so the window
    /// tests verify the threading.
    member this.previewMuKind : MuKind =
        match this.mode with
        | EditableMaterial -> this.editor.muKind
        | ViewOnlyMaterial (_, shown) -> shown.muKind

    /// The derived-model readout: the eps case, the optional aspects, and a structural digest —
    /// two states with the SAME summary denote the SAME derived complexity (the lossless-uncheck
    /// acceptance observes this text); an underivable ladder shows the typed reason instead.
    member this.complexitySummary : ControlCaption =
        match this.mode with
        | ViewOnlyMaterial _ -> ControlCaption "view-only"
        | EditableMaterial ->
            match this.editor.toComplexity with
            | Error e -> ControlCaption $"not derivable — %s{e.caption.value}"
            | Ok c ->
                let aspect (label : string) (o : 'Aspect option) : string =
                    match o with
                    | Some _ -> $"%s{label} on"
                    | None -> $"%s{label} off"
                ControlCaption $"""eps %s{c.eps.editorCaseCaption.value}; %s{aspect "active" c.active}; %s{aspect "magnetic" c.magnetic} [%08x{hash c}]"""

    /// The current edit slice compared for dirtiness (spec 0038 step 033): the identity facets and
    /// the ladder edit state, all plain records/DUs. A view-only entry never mutates these, so it is
    /// never dirty and always closes silently.
    member internal this.currentEdit : MaterialEditSnapshot =
        (this.name, this.description, this.category, this.editor)

    /// Whether the editor carries unsaved changes: the current edit slice differs structurally from
    /// the slice captured at load. No dirty flag is maintained (spec 0038 step 033).
    member this.isDirty : bool =
        this.initialEdit <> this.currentEdit

// ---------------------------------------------------------------------------
// init / update (pure — IO only through the context's proxy fields).
// ---------------------------------------------------------------------------

let initMaterialEditor (context : MaterialEditorContext) (intent : MaterialEditorIntent) : MaterialEditorModel =
    let built =
        match intent with
        | NewMaterial mintedId ->
            // The Add path: the id was minted AT WINDOW OPEN (spec 0038 step 008) and rides the
            // model as a NewUnsaved target, so Save persists under the SAME id the launcher's
            // registry already keys this window by.
            {
                context = context
                target = { materialId = mintedId; freshness = WindowLauncher.NewUnsaved }
                mode = EditableMaterial
                name = EntryName ""
                description = EntryDescription ""
                category = CategoryIds.glass
                editor = defaultState
                hiddenSeries = Set.empty
                initialEdit = (EntryName "", EntryDescription "", CategoryIds.glass, defaultState)
                exit = MaterialEditing
                status = None
            }
        | EditMaterial entry ->
            let seeded =
                {
                    context = context
                    target = { materialId = entry.id; freshness = WindowLauncher.Persisted }
                    mode = EditableMaterial
                    name = entry.name
                    description = entry.description |> Option.defaultValue (EntryDescription "")
                    category = entry.category
                    editor = defaultState
                    hiddenSeries = Set.empty
                    initialEdit = (EntryName "", EntryDescription "", CategoryIds.glass, defaultState)
                    exit = MaterialEditing
                    status = None
                }
            // Editability is `MaterialPhysics.editableComplexityOpt` (spec 0050 S11.1): a physics that is
            // not editable term data (Silicon / Langasite carry published laws) opens VIEW-ONLY.
            match entry.physics.editableComplexityOpt with
            | Some complexity ->
                match ofComplexity complexity with
                | Ok editor -> { seeded with editor = editor }
                | Error e -> { seeded with mode = ViewOnlyMaterial (e.caption, entry.physics) }
            // The view-only mode keeps the physics so the preview can restrict the gyration / μ
            // curves to the components its class / kind admits (spec 0040 step 006).
            | None ->
                { seeded with
                    mode = ViewOnlyMaterial (ControlCaption "this entry's physics is not editable term data — view-only", entry.physics) }
    // Capture the edit snapshot from the fully-seeded model, so a freshly opened editor is
    // pristine and closes silently (spec 0038 step 033).
    { built with initialEdit = built.currentEdit }

let updateMaterialEditor (msg : MaterialEditorMsg) (m : MaterialEditorModel) : MaterialEditorModel =
    match msg with
    | SetMaterialName name -> { m with name = name }
    | SetMaterialDescription description -> { m with description = description }
    | ChooseCategory category -> { m with category = category }
    | EditorMsg editorMsg ->
        match applyMaterialComplexityMsg editorMsg m.editor with
        // A structurally-unchanged edit returns the SAME model (no new record), so the Elmish
        // equality gate does NOT re-render. This is what stops a coefficient / component box's
        // programmatic re-render echo (which re-dispatches the round-tripped, unchanged value)
        // from spinning an infinite render loop (spec 0033 gaps G7 / G9).
        | Ok editor when editor = m.editor -> m
        | Ok editor -> { m with editor = editor; status = None }
        | Error e -> { m with status = Some e.caption }
    | ToggleSeriesVisibility key ->
        // Preview-only UI state (which curves are hidden); never touches the derived complexity.
        let hidden =
            if Set.contains key m.hiddenSeries then Set.remove key m.hiddenSeries else Set.add key m.hiddenSeries
        { m with hiddenSeries = hidden }
    | MaterialSaveClicked ->
        match m.mode with
        | ViewOnlyMaterial (reason, _) -> { m with status = Some reason }
        | EditableMaterial ->
            match m.editor.toComplexity with
            | Error e -> { m with status = Some e.caption }
            | Ok complexity ->
                let entryUnder (id : MaterialId) : MaterialEntry =
                    {
                        id = id
                        name = m.name
                        category = m.category
                        description = (if String.IsNullOrWhiteSpace m.description.value then None else Some m.description)
                        physics = ComplexityPhysics complexity
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
                | Error e -> { m with status = Some e.caption }
    | MaterialCancelClicked ->
        // Spec 0038 (033): a dirty editor MUST NOT close silently — show the discard confirm;
        // a pristine one closes as today. The window chrome (`OnClosing`) routes here too, so
        // both exits are equally gated.
        if m.isDirty then { m with exit = MaterialConfirmingDiscard }
        else
            m.context.requestClose ()
            m
    | MaterialDiscardConfirmed ->
        // Close WITHOUT saving — the discard confirm's negative action (spec 0038 step 033).
        m.context.requestClose ()
        m
    | MaterialKeepEditing ->
        // Dismiss the confirm and stay in the editor (spec 0038 step 033).
        { m with exit = MaterialEditing }

// ---------------------------------------------------------------------------
// The FuncUI view. Styling matches the sibling editors' idle/chosen boxes; every control in
// the conditional ladder subtrees carries a mutable AutomationId, never `Name` (FuncUI
// recycling cannot rename a styled control, and the ladder's membership changes on every
// toggle — the SampleEditorWindow precedent).
// ---------------------------------------------------------------------------

let private brush (c : Color) : IBrush = SolidColorBrush(c) :> IBrush
let private idleBackground = Color.FromRgb(232uy, 232uy, 232uy)
let private chosenBackground = Color.FromRgb(150uy, 185uy, 235uy)
let private saveBackground = Color.FromRgb(186uy, 224uy, 186uy)
let private cancelBackground = Color.FromRgb(236uy, 202uy, 202uy)
let private idleBorder = Color.FromRgb(120uy, 120uy, 120uy)
let private hintColor = Color.FromRgb(110uy, 110uy, 110uy)
let private errorColor = Color.FromRgb(165uy, 40uy, 40uy)
let private warningColor = Color.FromRgb(170uy, 110uy, 20uy)

/// A clickable, styled box on `background` (the chosen look or the idle one). `e.Handled <- true`
/// drops FuncUI's duplicate Tunnel|Bubble pass; re-subscribe when the id or the look changes.
let private clickBox (id : ConstructorUiId) (label : ControlCaption) (background : Color) (onClick : unit -> unit) : IView =
    Border.create [
        id.automationAttribute<Border> ()
        Border.background (brush background)
        Border.borderBrush (brush idleBorder)
        Border.borderThickness 1.0
        Border.cornerRadius (CornerRadius 3.0)
        Border.padding (Thickness(10.0, 4.0))
        Border.margin (Thickness(0.0, 0.0, 6.0, 4.0))
        Border.verticalAlignment VerticalAlignment.Center
        Border.child (TextBlock.create [ TextBlock.text label.value ])
        Border.onPointerPressed ((fun e -> e.Handled <- true; onClick ()), SubPatchOptions.OnChangeOf (box (id.value, background)))
    ] :> IView

/// An option (or toggle) box, chosen while `current` is the option it offers.
let private optionBox<'Choice when 'Choice : equality> (id : ConstructorUiId) (label : ControlCaption) (current : 'Choice) (offered : 'Choice) (onClick : unit -> unit) : IView =
    clickBox id label (if current = offered then chosenBackground else idleBackground) onClick

/// A verb button — withheld verbs are present but inert (a disabled Border dispatches no
/// pointer event). Left-aligned so a button never stretches to its container's full width
/// (spec 0033 gap G11 — the add-segment verb sat in a vertical stack and spanned it).
let private verbButton (id : ConstructorUiId) (label : ControlCaption) (availability : ControlAvailability) (onClick : unit -> unit) : IView =
    Border.create [
        id.automationAttribute<Border> ()
        Border.isEnabled availability.isEnabled
        Border.opacity (match availability with ControlOffered -> 1.0 | ControlWithheld -> 0.4)
        Border.background (brush idleBackground)
        Border.borderBrush (brush idleBorder)
        Border.borderThickness 1.0
        Border.cornerRadius (CornerRadius 3.0)
        Border.padding (Thickness(12.0, 5.0))
        Border.margin (Thickness(0.0, 0.0, 8.0, 4.0))
        Border.horizontalAlignment HorizontalAlignment.Left
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
        Border.padding (Thickness(22.0, 6.0))
        Border.margin (Thickness(0.0, 0.0, 10.0, 0.0))
        Border.verticalAlignment VerticalAlignment.Center
        Border.child (TextBlock.create [ TextBlock.text label.value ])
        Border.onPointerPressed ((fun e -> e.Handled <- true; onClick ()), SubPatchOptions.OnChangeOf (box id.value))
    ] :> IView

type ControlCaption with

    /// The caption as a row's vertically centred label.
    member private this.materialEditorLabel : IView =
        TextBlock.create [ TextBlock.text this.value; TextBlock.verticalAlignment VerticalAlignment.Center ] :> IView

    /// The typed text as an invariant-culture number; `None` when it does not parse.
    member private this.materialEntryNumber : float option =
        match Double.TryParse(this.value, NumberStyles.Float, CultureInfo.InvariantCulture) with
        | true, v -> Some v
        | _ -> None

/// A label + entry pair as ONE wrap item (a wrap never splits a label from its box).
let private labelled (label : ControlCaption) (entry : IView) : IView =
    StackPanel.create [
        StackPanel.orientation Orientation.Horizontal
        StackPanel.spacing 4.0
        StackPanel.margin (Thickness(0.0, 0.0, 10.0, 4.0))
        StackPanel.children [ label.materialEditorLabel; entry ]
    ] :> IView

/// How an entry box shows one quantity and reads a typed text back into it, so the box speaks the
/// quantity, not a bare float (spec 0048 E031). `tryRead` gets the current value too: a box that edits
/// one part of a value (the n of n + ik) keeps the rest.
type private MaterialEntryFormat<'Value> =
    {
        caption : 'Value -> ControlCaption
        tryRead : 'Value -> ControlCaption -> 'Value option
    }

/// The n of a principal index n + ik.
let private refractivePartEntry : MaterialEntryFormat<ComplexRefractionIndex> =
    {
        caption = fun index -> ControlCaption $"%g{index.value.Real}"
        tryRead = fun index text -> text.materialEntryNumber |> Option.map (fun n -> ComplexRefractionIndex (System.Numerics.Complex.create n index.value.Imaginary))
    }

/// The k of a principal index n + ik.
let private absorptivePartEntry : MaterialEntryFormat<ComplexRefractionIndex> =
    {
        caption = fun index -> ControlCaption $"%g{index.value.Imaginary}"
        tryRead = fun index text -> text.materialEntryNumber |> Option.map (fun k -> ComplexRefractionIndex (System.Numerics.Complex.create index.value.Real k))
    }

/// A segment bound, in display nanometres.
let private nanometreEntry : MaterialEntryFormat<WaveLength> =
    {
        caption = fun w -> ControlCaption $"%g{(wavelengthToUnit Nanometer w).value}"
        tryRead = fun _ text -> text.materialEntryNumber |> Option.map (fun nm -> toWaveLength Nanometer (UnitMagnitude nm))
    }

let private muEntry : MaterialEntryFormat<MuValue> =
    {
        caption = fun mu -> ControlCaption $"%g{mu.value}"
        tryRead = fun _ text -> text.materialEntryNumber |> Option.map MuValue
    }

/// A dispersion coefficient, shown as the shortest round-trippable text so no precision is lost.
let private coefficientEntry : MaterialEntryFormat<CoefficientValue> =
    {
        caption = fun c -> ControlCaption (c.value.ToString("R", CultureInfo.InvariantCulture))
        tryRead = fun _ text -> text.materialEntryNumber |> Option.map CoefficientValue
    }

/// A constant gyration component, shown as the shortest round-trippable text so no precision is lost.
let private rhoEntry : MaterialEntryFormat<RhoValue> =
    {
        caption = fun rho -> ControlCaption (rho.value.ToString("R", CultureInfo.InvariantCulture))
        tryRead = fun _ text -> text.materialEntryNumber |> Option.map RhoValue
    }

/// A numeric entry: shows the model's value, reads each change and dispatches only a readable
/// value (an unreadable draft stays local until the next model-driven render). AutomationId,
/// never Name — the ladder boxes live in variable-membership subtrees.
let private numberBox<'Value> (id : ConstructorUiId) (format : MaterialEntryFormat<'Value>) (value : 'Value) (onCommit : 'Value -> unit) : IView =
    TextBox.create [
        id.automationAttribute<TextBox> ()
        TextBox.width 70.0
        TextBox.text (format.caption value).value
        TextBox.onTextChanged (
            (fun text ->
                match format.tryRead value (ControlCaption text) with
                | Some typed -> onCommit typed
                | None -> ()),
            SubPatchOptions.OnChangeOf (box (id.value, value)))
    ] :> IView

/// A numeric entry for a coefficient / tensor component (spec 0033 gaps G7 & G9). It commits on
/// FOCUS LOSS, not on every keystroke — deliberately UNLIKE `numberBox`. These boxes rebuild the
/// whole dispersion model on commit, and a `TextChanged` commit re-fires on FuncUI's programmatic
/// re-render echo, which (because each commit rebuilds the model) spins an infinite render loop.
/// `LostFocus` fires only on a real user focus change, never during render, so the loop cannot
/// form.
let private coeffNumberBox<'Value> (id : ConstructorUiId) (format : MaterialEntryFormat<'Value>) (value : 'Value) (onCommit : 'Value -> unit) : IView =
    TextBox.create [
        id.automationAttribute<TextBox> ()
        TextBox.width 100.0
        TextBox.text (format.caption value).value
        TextBox.onLostFocus (fun e ->
            match e.Source with
            | :? TextBox as tb ->
                match format.tryRead value (ControlCaption tb.Text) with
                | Some typed -> onCommit typed
                | None -> ()
            | _ -> ())
    ] :> IView

// -- the identity rows -----------------------------------------------------------------------

let private nameRow (m : MaterialEditorModel) (dispatch : MaterialEditorMsg -> unit) : IView =
    StackPanel.create [
        StackPanel.orientation Orientation.Horizontal
        StackPanel.spacing 6.0
        StackPanel.children [
            (ControlCaption "Name:").materialEditorLabel
            TextBox.create [
                TextBox.name ConstructorUiId.materialNameBox.value
                TextBox.width 340.0
                TextBox.text m.name.value
                TextBox.onTextChanged (EntryName >> SetMaterialName >> dispatch)
            ] :> IView
        ]
    ] :> IView

let private descriptionRow (m : MaterialEditorModel) (dispatch : MaterialEditorMsg -> unit) : IView =
    StackPanel.create [
        StackPanel.orientation Orientation.Horizontal
        StackPanel.spacing 6.0
        StackPanel.children [
            (ControlCaption "Description:").materialEditorLabel
            TextBox.create [
                TextBox.name ConstructorUiId.materialDescriptionBox.value
                // Spec 0040 (007), Part C.2: wrap instead of clip. A MinWidth (not a fixed width that
                // exceeds the left pane and meets the splitter) lets the box grow multiline within the
                // pane; AcceptsReturn + Wrap turn a long description into extra lines rather than a
                // horizontal overflow that clips against the splitter.
                TextBox.minWidth 340.0
                TextBox.acceptsReturn true
                TextBox.textWrapping TextWrapping.Wrap
                TextBox.text m.description.value
                TextBox.onTextChanged (EntryDescription >> SetMaterialDescription >> dispatch)
            ] :> IView
        ]
    ] :> IView

let private categoryRow (m : MaterialEditorModel) (dispatch : MaterialEditorMsg -> unit) : IView =
    StackPanel.create [
        StackPanel.orientation Orientation.Vertical
        StackPanel.spacing 2.0
        StackPanel.children [
            (ControlCaption "Category:").materialEditorLabel
            WrapPanel.create [
                ConstructorUiId.materialCategoryPicker.automationAttribute<WrapPanel> ()
                WrapPanel.orientation Orientation.Horizontal
                // Spec 0035 (009): the create picker offers the LIVE catalogue minus every
                // HiddenOnCreate category (Vacuum is removed, not greyed). Each option's id is the
                // category's `CategoryId` Guid string — STABLE across a rename, so the option box is
                // patched (only its label changes) rather than re-created — and its label is the
                // record NAME, so a category renamed through the shared proxy re-labels the picker.
                WrapPanel.children (
                    m.selectableCategories
                    |> List.map (fun category ->
                        optionBox (ConstructorUiId.materialEditorCategoryOption category.id) category.name.caption m.category category.id (fun () -> dispatch (ChooseCategory category.id))))
            ] :> IView
        ]
    ] :> IView

// -- the ladder ------------------------------------------------------------------------------

let private anisotropyRow (m : MaterialEditorModel) (dispatch : MaterialEditorMsg -> unit) : IView =
    StackPanel.create [
        StackPanel.orientation Orientation.Vertical
        StackPanel.spacing 2.0
        StackPanel.children [
            (ControlCaption "Anisotropy:").materialEditorLabel
            WrapPanel.create [
                ConstructorUiId.anisotropyToggle.automationAttribute<WrapPanel> ()
                WrapPanel.orientation Orientation.Horizontal
                WrapPanel.children (
                    [ Isotropic; Uniaxial; Biaxial ]
                    |> List.map (fun choice ->
                        optionBox (ConstructorUiId.anisotropyOption choice.uiKey) choice.caption m.editor.anisotropy choice (fun () -> dispatch (EditorMsg (ChooseAnisotropy choice)))))
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
let private togglesRow (m : MaterialEditorModel) (dispatch : MaterialEditorMsg -> unit) : IView =
    let absorbing = m.editor.transparency = Absorbing
    let dispersive = m.editor.dispersion = DispersiveSegments
    let active = m.editor.activity = ActivityOn
    let magnetic = m.editor.magnetic = MagneticOn
    let activityOffered = not (List.isEmpty (availableGyrationClasses m.editor.anisotropy))
    StackPanel.create [
        StackPanel.orientation Orientation.Vertical
        StackPanel.spacing 2.0
        StackPanel.children [
            (ControlCaption "Dispersion model:").materialEditorLabel
            WrapPanel.create [
                WrapPanel.orientation Orientation.Horizontal
                WrapPanel.children (
                    [
                        optionBox ConstructorUiId.constantToggle (ControlCaption "Constant") m.editor.dispersion NonDispersive (fun () ->
                            dispatch (EditorMsg (SetDispersion NonDispersive)))
                        optionBox ConstructorUiId.dispersiveToggle (ControlCaption "Dispersive") m.editor.dispersion DispersiveSegments (fun () ->
                            dispatch (EditorMsg (SetDispersion DispersiveSegments)))
                    ]
                    @ (if dispersive then []
                       else
                           [ optionBox ConstructorUiId.absorbingToggle (ControlCaption "Absorbing") m.editor.transparency Absorbing (fun () ->
                                 dispatch (EditorMsg (SetTransparency (if absorbing then Transparent else Absorbing)))) ])
                    @ (if activityOffered then
                           [ optionBox ConstructorUiId.activeToggle (ControlCaption "Optically active") m.editor.activity ActivityOn (fun () ->
                                 dispatch (EditorMsg (SetActivity (if active then ActivityOff else ActivityOn)))) ]
                       else [])
                    @ [
                        optionBox ConstructorUiId.magneticToggle (ControlCaption "Magnetic (Polder μ)") m.editor.magnetic MagneticOn (fun () ->
                            dispatch (EditorMsg (SetMagnetic (if magnetic then MagneticOff else MagneticOn))))
                    ])
            ] :> IView
        ]
    ] :> IView

/// The constant principal-index fields (the non-dispersive rung): 1 / 2 / 3 boxes per the
/// anisotropy choice, plus the k boxes while the absorbing rung is unlocked.
let private indexFieldsRow (m : MaterialEditorModel) (dispatch : MaterialEditorMsg -> unit) : IView =
    let slots =
        match m.editor.anisotropy with
        | Isotropic -> [ (1, FirstAxis, m.editor.index1, ControlCaption "n₁:") ]
        | Uniaxial -> [ (1, FirstAxis, m.editor.index1, ControlCaption "n₁ (ordinary):"); (2, SecondAxis, m.editor.index2, ControlCaption "n₂ (extraordinary):") ]
        | Biaxial -> [ (1, FirstAxis, m.editor.index1, ControlCaption "n₁:"); (2, SecondAxis, m.editor.index2, ControlCaption "n₂:"); (3, ThirdAxis, m.editor.index3, ControlCaption "n₃:") ]
    let boxes =
        slots
        |> List.collect (fun (number, slot, index, label) ->
            let position = ConstructorUiPosition number
            [
                labelled label (numberBox (ConstructorUiId.principalIndexBox position) refractivePartEntry index (fun edited ->
                    dispatch (EditorMsg (SetPrincipalIndex (slot, edited)))))
            ]
            @ (match m.editor.transparency with
               | Absorbing ->
                   [
                       labelled (ControlCaption $"k%d{number}:") (numberBox (ConstructorUiId.absorptionIndexBox position) absorptivePartEntry index (fun edited ->
                           dispatch (EditorMsg (SetPrincipalIndex (slot, edited)))))
                   ]
               | Transparent -> []))
    WrapPanel.create [
        WrapPanel.orientation Orientation.Horizontal
        WrapPanel.children boxes
    ] :> IView

/// One coefficient entry with its unit of measure shown (spec 0033 comment 009): a dimensioned
/// parameter reads "label: [box] unit"; a dimensionless one omits the unit.
let private paramBoxWithUnit (id : ConstructorUiId) (p : ModelParameter) (onCommit : CoefficientValue -> unit) : IView =
    StackPanel.create [
        StackPanel.orientation Orientation.Horizontal
        StackPanel.spacing 4.0
        StackPanel.margin (Thickness(0.0, 0.0, 10.0, 4.0))
        StackPanel.children (
            [ (ControlCaption $"{p.label.value}:").materialEditorLabel; coeffNumberBox id coefficientEntry p.coefficient onCommit ]
            @ (match p.unitOpt with
               | Some u -> [ u.caption.materialEditorLabel ]
               | None -> []))
    ] :> IView

// -- the dispersive-component formula surface (spec 0035 step 011) ----------------------------

type DispersionFormula with

    /// Wrap one scalar component's `DispersionFormula` (a gyration `g₍ᵢⱼ₎` or a Polder
    /// component — spec §C.0: a component IS a `DispersionFormula`) as the raw
    /// `SumOfTerms` model, so the SAME `modelParameters` coefficient surface the segments
    /// use edits it. The component is a single REAL formula, so the k slot is the empty
    /// formula — it contributes no boxes, and only the n formula's term scalars are exposed.
    member private this.componentModel : DispersionModel =
        SumOfTerms (RealNK { nFormula = this; kFormula = { terms = []; wavelengthScale = this.wavelengthScale } })

/// Read the edited scalar formula back out of a wrapped component model (the n slot).
/// `componentModel` always builds `RealNK`, so the fallback is unreachable — it keeps
/// the prior formula rather than throwing.
let private formulaOfModel (fallback : DispersionFormula) (model : DispersionModel) : DispersionFormula =
    match model with
    | SumOfTerms (RealNK nk) -> nk.nFormula
    | _ -> fallback

/// One scalar component's dispersion-formula coefficient editor (spec 0035 step 011 —
/// the gyration / Polder analogue of `segmentParamsView`): the `modelParameters` boxes
/// of the component's raw `SumOfTerms` formula inside a WrapPanel that carries
/// `editorId` (the per-component container id the headless proof probes). Each edit
/// rebuilds the formula and dispatches it through `dispatchFormula`. The boxes commit
/// on focus loss (`coeffNumberBox`), never on the render echo — the gap-G7/G9 loop
/// guard.
let private componentFormulaView (editorId : ConstructorUiId) (boxId : ConstructorUiKey -> ConstructorUiId) (formula : DispersionFormula) (dispatchFormula : DispersionFormula -> unit) : IView =
    let boxes =
        formula.componentModel.parameters
        |> List.map (fun p ->
            paramBoxWithUnit (boxId p.key.uiKey) p (fun v -> dispatchFormula (formulaOfModel formula (p.update v))))
    WrapPanel.create [
        editorId.automationAttribute<WrapPanel> ()
        WrapPanel.orientation Orientation.Horizontal
        WrapPanel.children boxes
    ] :> IView

/// The coefficient-entry surface for ONE principal axis of a segment (spec 0033 gap G7 +
/// comment 009): one unit-labelled box per editable coefficient (`DispersionModels.modelParameters`,
/// which now covers the raw `SumOfTerms` term data too), each edit rebuilding that axis's model
/// through `SetSegmentAxisModel`.
let private segmentParamsView (dispatch : MaterialEditorMsg -> unit) (segmentIndex : SegmentIndex) (axis : DispersionSegmentAxis) (model : DispersionModel) : IView =
    let boxes =
        model.parameters
        |> List.map (fun p ->
            paramBoxWithUnit (ConstructorUiId.segmentParamBoxOfAxis segmentIndex.uiPosition axis.laterAxisOpt p.key.uiKey) p (fun v ->
                dispatch (EditorMsg (SetSegmentAxisModel (segmentIndex, axis.slot, p.update v)))))
    match boxes with
    | [] -> TextBlock.create [ TextBlock.text "(no editable coefficients)"; TextBlock.foreground (brush hintColor) ] :> IView
    | _ ->
        WrapPanel.create [
            WrapPanel.orientation Orientation.Horizontal
            WrapPanel.children boxes
        ] :> IView

/// One principal axis's model picker + coefficient boxes within a segment (spec 0033 comment 009).
let private segmentAxisView (dispatch : MaterialEditorMsg -> unit) (segmentIndex : SegmentIndex) (axis : DispersionSegmentAxis) (model : DispersionModel) : IView =
    let currentCode = model.kindCode
    StackPanel.create [
        StackPanel.orientation Orientation.Vertical
        StackPanel.spacing 2.0
        StackPanel.children (
            (match axis.captionOpt with
             | Some caption -> [ caption.materialEditorLabel ]
             | None -> [])
            @ [
                WrapPanel.create [
                    (ConstructorUiId.dispersionModelPickerOfAxis segmentIndex.uiPosition axis.laterAxisOpt).automationAttribute<WrapPanel> ()
                    WrapPanel.orientation Orientation.Horizontal
                    WrapPanel.children (
                        defaultModelChoices
                        |> List.map (fun candidate ->
                            let code = candidate.kindCode
                            optionBox (ConstructorUiId.dispersionModelOptionOfAxis segmentIndex.uiPosition axis.laterAxisOpt (ConstructorUiKey code)) (ControlCaption candidate.kindLabel) currentCode code (fun () ->
                                dispatch (EditorMsg (ChooseSegmentAxisModel (segmentIndex, axis.slot, candidate))))))
                ] :> IView
                segmentParamsView dispatch segmentIndex axis model
            ])
    ] :> IView

/// One dispersion segment's editor: the shared wavelengthInterval bounds (nm), then — PER
/// PRINCIPAL AXIS of the anisotropy (spec 0033 comment 009) — a model picker + unit-labelled
/// coefficient boxes.
let private segmentView (m : MaterialEditorModel) (dispatch : MaterialEditorMsg -> unit) (segmentIndex : SegmentIndex) (segment : EditSegment) : IView =
    let removable = if List.length m.editor.segments > 1 then ControlOffered else ControlWithheld
    StackPanel.create [
        StackPanel.orientation Orientation.Vertical
        StackPanel.spacing 2.0
        StackPanel.children (
            [
                WrapPanel.create [
                    WrapPanel.orientation Orientation.Horizontal
                    WrapPanel.children [
                        (ControlCaption $"Segment {segmentIndex.value} —").materialEditorLabel
                        labelled (ControlCaption "λ from (nm):") (numberBox (ConstructorUiId.segmentLowerBox segmentIndex.uiPosition) nanometreEntry segment.interval.lower (fun lower ->
                            dispatch (EditorMsg (SetSegmentInterval (segmentIndex, { lower = lower; upper = segment.interval.upper })))))
                        labelled (ControlCaption "to (nm):") (numberBox (ConstructorUiId.segmentUpperBox segmentIndex.uiPosition) nanometreEntry segment.interval.upper (fun upper ->
                            dispatch (EditorMsg (SetSegmentInterval (segmentIndex, { lower = segment.interval.lower; upper = upper })))))
                        verbButton (ConstructorUiId.segmentRemoveButton segmentIndex.uiPosition) (ControlCaption "Remove") removable (fun () ->
                            dispatch (EditorMsg (RemoveSegment segmentIndex)))
                    ]
                ] :> IView
            ]
            @ (m.editor.anisotropy.segmentAxes
               |> List.map (fun axis -> segmentAxisView dispatch segmentIndex axis (axisModelOf axis.slot segment))))
    ] :> IView

/// The per-segment dispersion editor (the dispersive rung): each segment's bounds + model
/// picker, then the mandated add-segment verb.
let private segmentsPanel (m : MaterialEditorModel) (dispatch : MaterialEditorMsg -> unit) : IView =
    StackPanel.create [
        StackPanel.orientation Orientation.Vertical
        StackPanel.spacing 2.0
        StackPanel.children (
            [ (ControlCaption "Dispersion segments (first covering segment wins; the topmost extrapolates):").materialEditorLabel ]
            @ (m.editor.segments |> List.mapi (fun i seg -> segmentView m dispatch (SegmentIndex i) seg))
            @ [ verbButton ConstructorUiId.addSegmentButton (ControlCaption "Add segment") ControlOffered (fun () -> dispatch (EditorMsg AddSegment)) ])
    ] :> IView

/// The gyration-component entry surface (the activity rung's Constant vs Dispersive
/// sub-branch — spec 0035 step 011): under `ConstantComponents` the per-component
/// constant `coeffNumberBox`es; under `DispersiveComponents` one dispersion-formula
/// editor per symmetry-allowed component of `gyrationDispersion` (class-synced with
/// `gyration`, so the component set matches the class picker either way).
let private gyrationComponentsSection (m : MaterialEditorModel) (dispatch : MaterialEditorMsg -> unit) : IView =
    match m.editor.activityDispersion with
    | ConstantComponents ->
        WrapPanel.create [
            WrapPanel.orientation Orientation.Horizontal
            WrapPanel.children (
                m.editor.gyration.components
                |> List.map (fun (comp, rho) ->
                    labelled (ControlCaption (comp.label + ":")) (coeffNumberBox (ConstructorUiId.gyrationComponentBox (ConstructorUiKey comp.code)) rhoEntry rho (fun v ->
                        dispatch (EditorMsg (SetGyrationComponent (comp, v)))))))
        ] :> IView
    | DispersiveComponents ->
        StackPanel.create [
            StackPanel.orientation Orientation.Vertical
            StackPanel.spacing 4.0
            StackPanel.children (
                m.editor.gyrationDispersion.components
                |> List.map (fun (comp, formula) ->
                    let key = ConstructorUiKey comp.code
                    StackPanel.create [
                        StackPanel.orientation Orientation.Vertical
                        StackPanel.spacing 2.0
                        StackPanel.children [
                            (ControlCaption (comp.label + " (dispersion formula):")).materialEditorLabel
                            componentFormulaView (ConstructorUiId.gyrationFormulaEditor key) (ConstructorUiId.gyrationFormulaBox key) formula (fun f ->
                                dispatch (EditorMsg (SetGyrationComponentDispersion (comp, f))))
                        ]
                    ] :> IView))
        ] :> IView

/// The symmetry-class gyration panel (the activity rung): the class picker CONSTRAINED by the
/// anisotropy choice, the Constant/Dispersive component sub-toggle, and the handedness switch.
let private gyrationPanel (m : MaterialEditorModel) (dispatch : MaterialEditorMsg -> unit) : IView =
    let currentCode = m.editor.gyration.classCode
    let dispersive = m.editor.activityDispersion = DispersiveComponents
    StackPanel.create [
        StackPanel.orientation Orientation.Vertical
        StackPanel.spacing 2.0
        StackPanel.children [
            (ControlCaption "Gyration symmetry class:").materialEditorLabel
            WrapPanel.create [
                ConstructorUiId.gyrationClassPicker.automationAttribute<WrapPanel> ()
                WrapPanel.orientation Orientation.Horizontal
                WrapPanel.children (
                    availableGyrationClasses m.editor.anisotropy
                    |> List.map (fun offered ->
                        let code = offered.classCode
                        optionBox (ConstructorUiId.gyrationClassOption (ConstructorUiKey code)) (ControlCaption offered.classLabel) currentCode code (fun () ->
                            dispatch (EditorMsg (ChooseGyrationClass offered)))))
            ] :> IView
            (ControlCaption "Gyration component model (off = constant g):").materialEditorLabel
            WrapPanel.create [
                WrapPanel.orientation Orientation.Horizontal
                WrapPanel.children [
                    optionBox ConstructorUiId.activityDispersiveToggle (ControlCaption "Dispersive components") m.editor.activityDispersion DispersiveComponents (fun () ->
                        dispatch (EditorMsg (SetActivityDispersion (if dispersive then ConstantComponents else DispersiveComponents))))
                ]
            ] :> IView
            (ControlCaption "Gyration components (the symmetry-allowed g₍ᵢⱼ₎, typically ~10⁻⁵):").materialEditorLabel
            gyrationComponentsSection m dispatch
            (ControlCaption "Handedness (the enantiomorph — one overall sign):").materialEditorLabel
            WrapPanel.create [
                ConstructorUiId.handednessSwitch.automationAttribute<WrapPanel> ()
                WrapPanel.orientation Orientation.Horizontal
                WrapPanel.children (
                    [ RightHanded; LeftHanded ]
                    |> List.map (fun hand ->
                        optionBox (ConstructorUiId.handednessOption hand.uiKey) hand.caption m.editor.hand hand (fun () ->
                            dispatch (EditorMsg (SetHandedness hand)))))
            ] :> IView
        ]
    ] :> IView

/// The Polder-mu panel (the magnetic rung): the Constant/Dispersive component sub-toggle,
/// then — under Constant — scalar vs gyromagnetic + the constant component entries, or —
/// under Dispersive — one dispersion-formula editor per Polder component (always the full
/// tensor, as the engine's `MuWithDispValue` has no scalar case); the magnetization-axis
/// picker follows whenever the tensor is the full gyromagnetic one.
let private muPanel (m : MaterialEditorModel) (dispatch : MaterialEditorMsg -> unit) : IView =
    let kind = m.editor.muKind
    let dispersive = m.editor.magneticDispersion = DispersiveComponents
    // The magnetization-axis picker: the axis is one physical choice shared by both facets
    // (`ChooseGyrationAxis` sets it on each), so its highlight reads `m.editor.polder.axis`.
    let axisRow (show : bool) : IView list =
        if not show then []
        else
            [
                (ControlCaption "Magnetization axis (Z = Faraday; X/Y = Voigt):").materialEditorLabel
                WrapPanel.create [
                    ConstructorUiId.gyrationAxisPicker.automationAttribute<WrapPanel> ()
                    WrapPanel.orientation Orientation.Horizontal
                    WrapPanel.children (
                        [ AlongX; AlongY; AlongZ ]
                        |> List.map (fun axis ->
                            optionBox (ConstructorUiId.gyrationAxisOption axis.uiKey) axis.caption m.editor.polder.axis axis (fun () ->
                                dispatch (EditorMsg (ChooseGyrationAxis axis)))))
                ] :> IView
            ]
    let subToggleRow : IView =
        WrapPanel.create [
            WrapPanel.orientation Orientation.Horizontal
            WrapPanel.children [
                optionBox ConstructorUiId.magneticDispersiveToggle (ControlCaption "Dispersive components") m.editor.magneticDispersion DispersiveComponents (fun () ->
                    dispatch (EditorMsg (SetMagneticDispersion (if dispersive then ConstantComponents else DispersiveComponents))))
            ]
        ] :> IView
    let constantBody : IView list =
        let kindOptions =
            [ ScalarMuKind; GyromagneticMuKind ]
            |> List.map (fun candidate ->
                optionBox (ConstructorUiId.muKindOption candidate.uiKey) candidate.caption kind candidate (fun () ->
                    dispatch (EditorMsg (SetMuKind candidate))))
        let componentBoxes =
            [ labelled (ControlCaption "μ (diagonal):") (numberBox ConstructorUiId.muDiagonalBox muEntry m.editor.polder.muDiagonal (fun v -> dispatch (EditorMsg (SetMuDiagonal v)))) ]
            @ (match kind with
               | GyromagneticMuKind ->
                   [
                       labelled (ControlCaption "μ (parallel):") (numberBox ConstructorUiId.muParallelBox muEntry m.editor.polder.muParallel (fun v -> dispatch (EditorMsg (SetMuParallel v))))
                       labelled (ControlCaption "g (gyration):") (numberBox ConstructorUiId.muGyrationBox muEntry m.editor.polder.gyration (fun v -> dispatch (EditorMsg (SetMuGyration v))))
                   ]
               | ScalarMuKind -> [])
        [ WrapPanel.create [ WrapPanel.orientation Orientation.Horizontal; WrapPanel.children (kindOptions @ componentBoxes) ] :> IView ]
        @ axisRow (kind = GyromagneticMuKind)
    let dispersiveBody : IView list =
        // The dispersive Polder is always the full tensor, so every component gets a
        // formula editor and the axis always applies.
        let editors =
            [
                ConstructorUiKey "muDiagonal", ControlCaption "μ (diagonal, dispersion formula):", m.editor.polderDispersion.muDiagonal, (fun (f : DispersionFormula) -> SetMuDiagonalDispersion f)
                ConstructorUiKey "muParallel", ControlCaption "μ (parallel, dispersion formula):", m.editor.polderDispersion.muParallel, (fun f -> SetMuParallelDispersion f)
                ConstructorUiKey "muGyration", ControlCaption "g (gyration, dispersion formula):", m.editor.polderDispersion.gyration, (fun f -> SetMuGyrationDispersion f)
            ]
            |> List.map (fun (key, label, formula, toMsg) ->
                StackPanel.create [
                    StackPanel.orientation Orientation.Vertical
                    StackPanel.spacing 2.0
                    StackPanel.children [
                        label.materialEditorLabel
                        componentFormulaView (ConstructorUiId.polderFormulaEditor key) (ConstructorUiId.polderFormulaBox key) formula (fun f -> dispatch (EditorMsg (toMsg f)))
                    ]
                ] :> IView)
        editors @ axisRow true
    StackPanel.create [
        StackPanel.orientation Orientation.Vertical
        StackPanel.spacing 2.0
        StackPanel.children (
            [ (ControlCaption "Polder μ:").materialEditorLabel; subToggleRow ]
            @ (match m.editor.magneticDispersion with
               | ConstantComponents -> constantBody
               | DispersiveComponents -> dispersiveBody))
    ] :> IView

// -- the live preview (the step-32 tabbed multi-curve dual-axis charts, embedded) --------------

/// Lower the per-series hidden state (`hiddenSeries`, by tab + series name) onto a chart's paired
/// style seed, hiding a curve the user toggled off — preview-only, never the derived model.
/// Exposed (non-private) so the lowering is proved against the REAL mapping (spec 0038 032 retry) —
/// a test asserts `applyHidden` flips the toggled curve's exact index, rather than re-deriving it.
let applyHidden (hidden : Set<ConstructorUiId>) (tab : MaterialPreviewTab) (chart : ExperimentChart) (style : ChartStyle.ChartStyleState) : ChartStyle.ChartStyleState =
    chart.series
    |> List.mapi (fun i s -> ChartStyle.ChartSeriesIndex i, ConstructorUiId.previewSeriesToggle tab.uiKey s.name.uiKey)
    |> List.fold (fun st (i, key) -> if Set.contains key hidden then ChartStyle.setSeriesVisible i ChartStyle.HiddenOnChart st else st) style

/// One preview tab: a row of per-series show/hide toggles pinned above the embedded dual-axis chart,
/// which fills the rest of the tab. The chart embeds the ONE shared ScottPlot control under the tab's
/// chart id (`EmbeddedChart`, degrading to a placeholder rather than throwing headlessly); the toggles
/// flip `hiddenSeries`, which `applyHidden` lowers onto the chart's style.
let private chartTab (tab : MaterialPreviewTab) (m : MaterialEditorModel) (dispatch : MaterialEditorMsg -> unit) (chart : ExperimentChart) (style : ChartStyle.ChartStyleState) : IView =
    let toggles : IView =
        WrapPanel.create [
            WrapPanel.orientation Orientation.Horizontal
            WrapPanel.children (
                chart.series
                |> List.map (fun s ->
                    let key = ConstructorUiId.previewSeriesToggle tab.uiKey s.name.uiKey
                    clickBox key s.name (if Set.contains key m.hiddenSeries then idleBackground else chosenBackground) (fun () ->
                        dispatch (ToggleSeriesVisibility key))))
        ] :> IView
    let host = OpticalConstructor.Controls.EmbeddedChart.embeddedChartView tab.chartId chart (applyHidden m.hiddenSeries tab chart style)
    TabItem.create [
        tab.tabId.automationAttribute<TabItem> ()
        TabItem.header tab.caption.value
        TabItem.content (
            DockPanel.create [
                DockPanel.children [
                    Border.create [ Border.dock Dock.Top; Border.padding (Thickness(4.0, 4.0)); Border.child toggles ]
                    Border.create [ Border.padding (Thickness(4.0)); Border.child host ]
                ]
            ])
    ] :> IView

/// The tabbed preview pane (the right pane): the n/k tab ALWAYS, then the Gyration tab exactly when
/// the entry is optically active and the μ tab exactly when magnetic — so the visible tabs plus the
/// per-tab legend always answer what is being drawn. In edit mode the optional tabs track the ladder
/// toggles; a view-only entry (no ladder) is classified from its assembled tensors instead. Returns
/// the pane view and the gain-warning text over the n/k tab's sampled k curves.
let private previewPane (m : MaterialEditorModel) (dispatch : MaterialEditorMsg -> unit) : IView * ControlCaption =
    match m.previewProperties with
    | None ->
        let placeholder =
            TextBlock.create [
                TextBlock.text "no preview — the ladder is not derivable (see the readout on the left)"
                TextBlock.foreground (brush hintColor)
                TextBlock.verticalAlignment VerticalAlignment.Center
                TextBlock.horizontalAlignment HorizontalAlignment.Center
            ] :> IView
        placeholder, ControlCaption ""
    | Some o ->
        let nkChart = NkDispersionChart.nkDispersionChart m.editor.anisotropy o Nanometer previewRange
        let activity =
            match m.mode with
            | EditableMaterial -> m.editor.activity
            | ViewOnlyMaterial _ -> NkDispersionChart.assembledActivity o
        let magnetic =
            match m.mode with
            | EditableMaterial -> m.editor.magnetic
            | ViewOnlyMaterial _ -> NkDispersionChart.assembledMagnetic o
        let tabs =
            [ chartTab NkPreviewTab m dispatch nkChart (NkDispersionChart.nkDispersionStyle m.editor.anisotropy nkChart) ]
            @ (match activity with
               | ActivityOn ->
                   let g = NkDispersionChart.gyrationChart m.previewGyrationComponents o Nanometer previewRange
                   [ chartTab GyrationPreviewTab m dispatch g g.gyrationStyle ]
               | ActivityOff -> [])
            @ (match magnetic with
               | MagneticOn ->
                   let mu = NkDispersionChart.muChart m.previewMuKind o Nanometer previewRange
                   [ chartTab MuPreviewTab m dispatch mu (NkDispersionChart.muStyle mu) ]
               | MagneticOff -> [])
        let pane =
            TabControl.create [
                ConstructorUiId.materialPreviewTabs.automationAttribute<TabControl> ()
                TabControl.viewItems tabs
            ] :> IView
        pane, nkChart.gainWarningCaption

/// The two-pane split: the identity / ladder pane (left) and the tabbed preview (right), a vertical
/// GridSplitter between them, each pane with a sensible minimum width so neither collapses.
let private leftPaneMinWidth : PanelMinWidth = PanelMinWidth 520.0
let private splitterWidth : PanelMinWidth = PanelMinWidth 6.0
let private rightPaneMinWidth : PanelMinWidth = PanelMinWidth 380.0
/// The narrowest split that gives BOTH panes their minimum under the 3* : 2* column ratio.
let private twoPaneMinWidth : PanelMinWidth =
    PanelMinWidth (splitterWidth.value + 5.0 * max (leftPaneMinWidth.value / 3.0) (rightPaneMinWidth.value / 2.0))

/// Spec 0046: the two panes keep their minimum widths, so a window narrower than both scrolls
/// horizontally (`MinWidthPanel`) instead of clipping the editor; vertically the panes fill the window
/// (the left pane scrolls its own content).
let private twoPane (leftPane : IView) (rightPane : IView) : IView =
    ScrollViewer.create [
        ScrollViewer.horizontalScrollBarVisibility Avalonia.Controls.Primitives.ScrollBarVisibility.Auto
        ScrollViewer.verticalScrollBarVisibility Avalonia.Controls.Primitives.ScrollBarVisibility.Disabled
        ScrollViewer.content (
            MinWidthPanel.create [
                twoPaneMinWidth.minWidthPanelAttribute ()
                MinWidthPanel.child (
                    Grid.create [
                        Grid.columnDefinitions "3*,Auto,2*"
                        Grid.children [
                            Border.create [ Border.column 0; Border.minWidth leftPaneMinWidth.value; Border.child leftPane ]
                            GridSplitter.create [
                                ConstructorUiId.materialEditorSplitter.automationAttribute<GridSplitter> ()
                                GridSplitter.column 1
                                GridSplitter.width splitterWidth.value
                                GridSplitter.resizeDirection GridResizeDirection.Columns
                                GridSplitter.background (brush idleBorder)
                            ]
                            Border.create [ Border.column 2; Border.minWidth rightPaneMinWidth.value; Border.child rightPane ]
                        ]
                    ])
            ])
    ] :> IView

// -- readouts / actions ------------------------------------------------------------------------

type MaterialEditorModel with

    /// Spec 0046: the summary WRAPS beside its docked label (a horizontal stack ran it off the pane's edge).
    member internal this.summaryRow : IView =
        DockPanel.create [
            DockPanel.children [
                Border.create [ Border.dock Dock.Left; Border.margin (Thickness(0.0, 0.0, 6.0, 0.0)); Border.child (ControlCaption "Derived model:").materialEditorLabel ]
                TextBlock.create [
                    TextBlock.name ConstructorUiId.materialComplexitySummary.value
                    TextBlock.text this.complexitySummary.value
                    TextBlock.textWrapping TextWrapping.Wrap
                    TextBlock.verticalAlignment VerticalAlignment.Center
                ] :> IView
            ]
        ] :> IView

let private gainWarningRow (warning : ControlCaption) : IView =
    TextBlock.create [
        TextBlock.name ConstructorUiId.materialGainWarning.value
        TextBlock.foreground (brush warningColor)
        TextBlock.text warning.value
    ] :> IView

let private statusRow (m : MaterialEditorModel) : IView =
    TextBlock.create [
        TextBlock.name ConstructorUiId.materialEditorStatus.value
        TextBlock.foreground (brush errorColor)
        TextBlock.text (
            match m.status with
            | Some reason -> reason.value
            | None -> "")
    ] :> IView

/// The Save / Cancel actions of an editable entry, or the lone Close of a view-only one.
let private saveCancelRow (dispatch : MaterialEditorMsg -> unit) (mode : EditorMode) : IView =
    let cancel (caption : ControlCaption) : IView =
        actionButton ConstructorUiId.materialEditorCancelButton caption cancelBackground (fun () -> dispatch MaterialCancelClicked)
    StackPanel.create [
        StackPanel.orientation Orientation.Horizontal
        StackPanel.spacing 0.0
        StackPanel.children (
            match mode with
            | EditableMaterial ->
                [
                    actionButton ConstructorUiId.materialEditorSaveButton (ControlCaption "Save") saveBackground (fun () -> dispatch MaterialSaveClicked)
                    cancel (ControlCaption "Cancel")
                ]
            | ViewOnlyMaterial _ -> [ cancel (ControlCaption "Close") ])
    ] :> IView

/// The unsaved-edit exit confirm surface (spec 0038 step 033): shown in place of the Save/Cancel
/// row while a dirty editor is being closed. Discard changes (negative styling) closes without
/// saving; Keep editing (positive styling) returns to the editor — one row, distinct styling.
let private exitConfirmRow (dispatch : MaterialEditorMsg -> unit) : IView =
    StackPanel.create [
        ConstructorUiId.materialEditorExitConfirm.automationAttribute<StackPanel> ()
        StackPanel.orientation Orientation.Horizontal
        StackPanel.spacing 0.0
        StackPanel.children [
            TextBlock.create [
                TextBlock.text "Discard unsaved changes?"
                TextBlock.foreground (brush warningColor)
                TextBlock.verticalAlignment VerticalAlignment.Center
                TextBlock.margin (Thickness(0.0, 0.0, 12.0, 0.0))
            ] :> IView
            actionButton ConstructorUiId.materialEditorDiscardButton (ControlCaption "Discard changes") cancelBackground (fun () -> dispatch MaterialDiscardConfirmed)
            actionButton ConstructorUiId.materialEditorKeepEditingButton (ControlCaption "Keep editing") saveBackground (fun () -> dispatch MaterialKeepEditing)
        ]
    ] :> IView

/// The bottom action area: the normal Save/Cancel (or Close) row, or — while `exit =
/// ConfirmingDiscard` — the discard confirm surface (spec 0038 step 033).
let private actionsRow (m : MaterialEditorModel) (dispatch : MaterialEditorMsg -> unit) : IView =
    match m.exit with
    | MaterialEditing -> saveCancelRow dispatch m.mode
    | MaterialConfirmingDiscard -> exitConfirmRow dispatch

/// The whole editor — a two-pane vertical split (spec 0038 step 032). Edit mode: LEFT pane = the
/// identity rows + the progressive ladder inside a `ScrollViewer`, with the derived-model / gain /
/// status readouts and the Save/Cancel actions pinned below it; RIGHT pane = the full-height tabbed
/// preview (per-axis n/k always, Gyration when active, μ when magnetic). View-only mode (an entry
/// whose physics is not data): the identity header + the WHY note (scrolling) with a Close action,
/// beside the same tabbed preview — NO ladder, NO Save.
let materialEditorView (m : MaterialEditorModel) (dispatch : MaterialEditorMsg -> unit) : IView =
    let previewView, warning = previewPane m dispatch
    let scrolling (content : IView) : IView =
        Border.create [
            Border.padding (Thickness(8.0))
            Border.child (ScrollViewer.create [ ScrollViewer.content content ])
        ] :> IView
    match m.mode with
    | ViewOnlyMaterial (reason, _) ->
        let categories =
            match m.context.categories.listCategories () with
            | Ok entries -> entries
            | Error _ -> []
        let leftPane =
            DockPanel.create [
                DockPanel.children [
                    Border.create [ Border.dock Dock.Bottom; Border.padding (Thickness(8.0, 6.0)); Border.child (actionsRow m dispatch) ]
                    scrolling (
                        StackPanel.create [
                            StackPanel.orientation Orientation.Vertical
                            StackPanel.spacing 6.0
                            StackPanel.children [
                                (ControlCaption $"{m.name.value} — {(m.category.categoryName categories).value}").materialEditorLabel
                                TextBlock.create [ TextBlock.text m.description.value; TextBlock.foreground (brush hintColor) ] :> IView
                                TextBlock.create [
                                    TextBlock.name ConstructorUiId.materialViewOnlyNote.value
                                    TextBlock.text reason.value
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
                    Border.create [ Border.dock Dock.Bottom; Border.padding (Thickness(8.0, 6.0)); Border.child (actionsRow m dispatch) ]
                    Border.create [ Border.dock Dock.Bottom; Border.padding (Thickness(8.0, 0.0)); Border.child (statusRow m) ]
                    Border.create [ Border.dock Dock.Bottom; Border.padding (Thickness(8.0, 0.0)); Border.child (gainWarningRow warning) ]
                    Border.create [ Border.dock Dock.Bottom; Border.padding (Thickness(8.0, 2.0)); Border.child m.summaryRow ]
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
