/// Spec 0046 — the Library's light-source editor: the pure MVU model and FuncUI projection behind
/// `LightSourceEditorWindow`, the Sample editor's sibling for SOURCES. A source is monochromatic (spec
/// Q3: a source is its wavelength), so the editor edits its name and its wavelength — typed in a chosen
/// unit (nm, µm, Å, eV or cm⁻¹; an EUV source reads naturally in nm or eV) and converted at the boundary
/// through the one `Units` seam. Save persists through the injected `SourceProxy` (a NEW source's entry
/// id was minted at window open, so the launcher keys the editor by the id Save stores under); Cancel
/// discards, and a dirty editor confirms before it closes (the Sample editor's exit gate). Pure: `update`
/// reaches IO only through the context's fields, so tests substitute stubs.
module OpticalConstructor.Ui.LightSourceEditorView

open System
open System.Globalization
open Avalonia
open Avalonia.Controls
open Avalonia.Layout
open Avalonia.Media
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open Berreman.Constants
open Berreman.Fields
open OpticalConstructor.Controls
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.Units

/// How the editor OPENS: a NEW source (the Library's Add source verb — its entry id minted at window
/// open) or an EXISTING user source edited in place (the Edit verb).
type SourceEditorIntent =
    | NewSource of mintedId : OpticalConstructor.Domain.Placement.ElementValueId
    | EditSource of SourcePreset

/// The editor's IO seam (the functional-proxy Context convention): the user-sources write store Save
/// persists through, and the host's close request (the window passes `this.Close`). Reference-compared.
[<ReferenceEquality>]
type SourceEditorContext =
    {
        sources : SourceProxy
        requestClose : unit -> unit
    }

/// Spec 0046: whether the source is a fixed line or tunable between two limits (the editor's choice).
type TuningChoice =
    | FixedChoice
    | TunableChoice

/// Whether the editor is editing normally or showing the unsaved-edit exit confirm (the Sample editor's
/// two-case gate).
type LightSourceEditorExitPrompt =
    | LightSourceEditing
    | LightSourceConfirmingDiscard

/// The slice Save persists, compared for dirtiness: the name, the wavelength in metres (`None` while the typed
/// value does not parse), the tuning choice and the tunable limits in metres.
type SourceEditSnapshot = EntryName * float<meter> option * TuningChoice * float<meter> option * float<meter> option

/// The editor's model. `waveLengthText` is the raw box text; `waveLength` is what it parses to in `unit`
/// (kept unchanged when only the unit changes, so switching units never moves the value).
type LightSourceEditorModel =
    {
        context : SourceEditorContext
        sourceId : OpticalConstructor.Domain.Placement.ElementValueId
        freshness : WindowLauncher.EntryFreshness
        name : EntryName
        waveLengthText : ControlCaption
        waveLength : WaveLength option
        /// Spec 0046: fixed line or tunable, and a tunable source's limits (raw texts + parsed values).
        tuning : TuningChoice
        minimumText : ControlCaption
        minimum : WaveLength option
        maximumText : ControlCaption
        maximum : WaveLength option
        unit : UnitOfMeasure
        initialEdit : SourceEditSnapshot
        exit : LightSourceEditorExitPrompt
        status : ControlCaption option
    }

type LightSourceEditorMsg =
    | SetLightSourceName of EntryName
    | SetWaveLengthText of ControlCaption
    /// Spec 0046: fixed line or tunable, and the tunable limits' texts.
    | SetTuning of TuningChoice
    | SetMinimumText of ControlCaption
    | SetMaximumText of ControlCaption
    /// Show (and type) the wavelength in another unit — the value itself is unchanged.
    | SetUnit of UnitOfMeasure
    | LightSourceSaveClicked
    /// Requested exit (the Cancel button, or the window chrome through `OnClosing`): a pristine editor
    /// closes, a dirty one shows the discard confirm.
    | LightSourceCancelClicked
    | LightSourceDiscardConfirmed
    | LightSourceKeepEditing

/// The wavelength units the editor offers, in display order.
let waveLengthUnits : UnitOfMeasure list = [ Nanometer; Micrometer; Angstrom; ElectronVolt; Wavenumber ]

/// A NEW source's starting wavelength — the seeded source's 600 nm.
let defaultWaveLength : WaveLength = WaveLength.nm 600.0<nm>

/// The wavelength as the box shows it in `u` (six significant digits, invariant culture).
let formatIn (u : UnitOfMeasure) (w : WaveLength) : ControlCaption =
    ControlCaption ((wavelengthToUnit u w).value.ToString("G6", CultureInfo.InvariantCulture))

/// Parse a typed wavelength in `u`: a positive finite number, else `None`.
let parseIn (u : UnitOfMeasure) (text : ControlCaption) : WaveLength option =
    match Double.TryParse(text.value, NumberStyles.Float, CultureInfo.InvariantCulture) with
    | true, v when v > 0.0 && not (Double.IsInfinity v) -> Some (toWaveLength u (UnitMagnitude v))
    | _ -> None

let private metresOf (w : WaveLength option) : float<meter> option = w |> Option.map (fun x -> x.value)

type EntryName with

    /// The name as Save stores it, without the box's leading and trailing blanks.
    member private this.trimmed : EntryName = EntryName (this.value.Trim())

type LightSourceEditorModel with

    member internal this.currentEdit : SourceEditSnapshot =
        (this.name, metresOf this.waveLength, this.tuning, metresOf this.minimum, metresOf this.maximum)

    /// Whether the editor carries unsaved changes (the edit slice differs from the one captured at load).
    member this.isDirty : bool = this.initialEdit <> this.currentEdit

    /// The source Save would store, or the reason it cannot yet.
    member this.toSource : Result<SourcePreset, ControlCaption> =
        match this.waveLength, this.tuning, this.minimum, this.maximum with
        | None, _, _, _ -> Error (ControlCaption $"enter a positive wavelength in %s{this.unit.abbreviation}")
        | Some w, FixedChoice, _, _ ->
            Ok { id = this.sourceId; name = this.name.trimmed; waveLength = w; tuning = FixedLine; protection = UserManaged }
        | Some w, TunableChoice, Some lo, Some hi ->
            Ok
                {
                    id = this.sourceId
                    name = this.name.trimmed
                    waveLength = w
                    tuning = TunableBetween { Berreman.Dispersion.WaveLengthInterval.lower = lo; upper = hi }
                    protection = UserManaged
                }
        | Some _, TunableChoice, _, _ -> Error (ControlCaption $"enter positive tunable limits in %s{this.unit.abbreviation}")

    /// The wavelength restated in nm and eV (the units visible-light and EUV work read), or the parse hint.
    member this.readoutCaption : ControlCaption =
        match this.waveLength with
        | Some w ->
            let nm = (wavelengthToUnit Nanometer w).value
            let ev = (wavelengthToUnit ElectronVolt w).value
            ControlCaption (String.Format(CultureInfo.InvariantCulture, "λ = {0:G6} nm  ·  {1:G6} eV", nm, ev))
        | None -> ControlCaption $"enter a positive wavelength in %s{this.unit.abbreviation}"

let initLightSourceEditor (context : SourceEditorContext) (intent : SourceEditorIntent) : LightSourceEditorModel =
    let sourceId, freshness, name, waveLength, tuning =
        match intent with
        | NewSource mintedId -> mintedId, WindowLauncher.NewUnsaved, EntryName "", defaultWaveLength, FixedLine
        | EditSource s -> s.id, WindowLauncher.Persisted, s.name, s.waveLength, s.tuning
    // A fixed line opens with sensible limits ready should it be switched to tunable.
    let choice, minimum, maximum =
        match tuning with
        | FixedLine -> FixedChoice, WaveLength.nm 400.0<nm>, WaveLength.nm 800.0<nm>
        | TunableBetween limits -> TunableChoice, limits.lower, limits.upper
    {
        context = context
        sourceId = sourceId
        freshness = freshness
        name = name
        waveLengthText = formatIn Nanometer waveLength
        waveLength = Some waveLength
        tuning = choice
        minimumText = formatIn Nanometer minimum
        minimum = Some minimum
        maximumText = formatIn Nanometer maximum
        maximum = Some maximum
        unit = Nanometer
        initialEdit = (name, Some waveLength.value, choice, Some minimum.value, Some maximum.value)
        exit = LightSourceEditing
        status = None
    }

let updateLightSourceEditor (msg : LightSourceEditorMsg) (m : LightSourceEditorModel) : LightSourceEditorModel =
    match msg with
    | SetLightSourceName s -> { m with name = s }
    | SetWaveLengthText t ->
        // The box echoes every re-render of the formatted value; text equal to the current value's own
        // formatting keeps the stored value exactly (no rounding drift from a unit switch).
        let unchanged =
            match m.waveLength with
            | Some w -> formatIn m.unit w = t
            | None -> false
        if unchanged then { m with waveLengthText = t }
        else { m with waveLengthText = t; waveLength = parseIn m.unit t }
    | SetTuning choice -> { m with tuning = choice }
    | SetMinimumText t ->
        let unchanged = match m.minimum with Some w -> formatIn m.unit w = t | None -> false
        if unchanged then { m with minimumText = t } else { m with minimumText = t; minimum = parseIn m.unit t }
    | SetMaximumText t ->
        let unchanged = match m.maximum with Some w -> formatIn m.unit w = t | None -> false
        if unchanged then { m with maximumText = t } else { m with maximumText = t; maximum = parseIn m.unit t }
    | SetUnit u ->
        // Every wavelength the editor shows switches unit together; the values themselves never move.
        let shown (value : WaveLength option) (text : ControlCaption) : ControlCaption * WaveLength option =
            match value with
            | Some w -> formatIn u w, Some w
            | None -> text, parseIn u text
        let waveLengthText, waveLength = shown m.waveLength m.waveLengthText
        let minimumText, minimum = shown m.minimum m.minimumText
        let maximumText, maximum = shown m.maximum m.maximumText
        { m with
            unit = u
            waveLengthText = waveLengthText
            waveLength = waveLength
            minimumText = minimumText
            minimum = minimum
            maximumText = maximumText
            maximum = maximum }
    | LightSourceSaveClicked ->
        match m.toSource with
        | Ok source ->
            match m.context.sources.saveSource source with
            | Ok () ->
                m.context.requestClose ()
                { m with status = None }
            | Error e -> { m with status = Some e.caption }
        | Error reason -> { m with status = Some reason }
    | LightSourceCancelClicked ->
        if m.isDirty then { m with exit = LightSourceConfirmingDiscard }
        else
            m.context.requestClose ()
            m
    | LightSourceDiscardConfirmed ->
        m.context.requestClose ()
        m
    | LightSourceKeepEditing -> { m with exit = LightSourceEditing }

// ---------------------------------------------------------------------------
// The FuncUI view (the Sample editor's look: idle / chosen boxes, Save / Cancel action pair).
// ---------------------------------------------------------------------------

let private brush (c : Color) : IBrush = SolidColorBrush(c) :> IBrush
let private idleBackground = Color.FromRgb(232uy, 232uy, 232uy)
let private chosenBackground = Color.FromRgb(150uy, 185uy, 235uy)
let private saveBackground = Color.FromRgb(186uy, 224uy, 186uy)
let private cancelBackground = Color.FromRgb(236uy, 202uy, 202uy)
let private idleBorder = Color.FromRgb(120uy, 120uy, 120uy)
let private hintColor = Color.FromRgb(110uy, 110uy, 110uy)
let private errorColor = Color.FromRgb(165uy, 40uy, 40uy)

/// A clickable box (keyed by its id; highlighted when chosen).
let private clickBox (id : ConstructorUiId) (label : ControlCaption) (background : Color) (onClick : unit -> unit) : IView =
    Border.create [
        id.automationAttribute<Border> ()
        Border.background (brush background)
        Border.borderBrush (brush idleBorder)
        Border.borderThickness 1.0
        Border.cornerRadius (CornerRadius 3.0)
        Border.padding (Thickness(10.0, 4.0))
        Border.margin (Thickness(0.0, 0.0, 6.0, 0.0))
        Border.verticalAlignment VerticalAlignment.Center
        Border.child (TextBlock.create [ TextBlock.text label.value ])
        Border.onPointerPressed ((fun e -> e.Handled <- true; onClick ()), SubPatchOptions.OnChangeOf (box (id.value, label.value, background)))
    ]
    |> Avalonia.FuncUI.DSL.View.withKey id.value
    :> IView

let private actionButton (id : ConstructorUiId) (label : ControlCaption) (background : Color) (onClick : unit -> unit) : IView =
    Border.create [
        id.automationAttribute<Border> ()
        Border.background (brush background)
        Border.borderBrush (brush idleBorder)
        Border.borderThickness 1.0
        Border.cornerRadius (CornerRadius 3.0)
        Border.padding (Thickness(22.0, 6.0))
        Border.margin (Thickness(0.0, 0.0, 10.0, 0.0))
        Border.child (TextBlock.create [ TextBlock.text label.value ])
        Border.onPointerPressed ((fun e -> e.Handled <- true; onClick ()), SubPatchOptions.OnChangeOf (box id.value))
    ]
    |> Avalonia.FuncUI.DSL.View.withKey id.value
    :> IView

type ControlCaption with

    /// The caption as a row's fixed-width label.
    member private this.lightSourceEditorLabel : IView =
        TextBlock.create [ TextBlock.text this.value; TextBlock.width 90.0; TextBlock.verticalAlignment VerticalAlignment.Center ] :> IView

let private nameRow (m : LightSourceEditorModel) (dispatch : LightSourceEditorMsg -> unit) : IView =
    StackPanel.create [
        StackPanel.orientation Orientation.Horizontal
        StackPanel.spacing 6.0
        StackPanel.children [
            (ControlCaption "Name:").lightSourceEditorLabel
            TextBox.create [
                TextBox.name ConstructorUiId.sourceNameBox.value
                TextBox.width 320.0
                TextBox.text m.name.value
                TextBox.onTextChanged (EntryName >> SetLightSourceName >> dispatch)
            ] :> IView
        ]
    ] :> IView

/// Spec 0046: the fixed-line / tunable choice and — for a tunable source — its limits (in the editor's
/// unit). The limits row is always present and only its visibility flips (stable named fields).
let private tuningRows (m : LightSourceEditorModel) (dispatch : LightSourceEditorMsg -> unit) : IView list =
    let tunable = (m.tuning = TunableChoice)
    [ StackPanel.create [
          StackPanel.orientation Orientation.Horizontal
          StackPanel.spacing 6.0
          StackPanel.children [
              (ControlCaption "Tuning:").lightSourceEditorLabel
              clickBox ConstructorUiId.sourceTuningFixed (ControlCaption "Fixed line") (if tunable then idleBackground else chosenBackground) (fun () -> dispatch (SetTuning FixedChoice))
              clickBox ConstructorUiId.sourceTuningTunable (ControlCaption "Tunable") (if tunable then chosenBackground else idleBackground) (fun () -> dispatch (SetTuning TunableChoice))
          ]
      ] :> IView
      StackPanel.create [
          StackPanel.orientation Orientation.Horizontal
          StackPanel.spacing 6.0
          StackPanel.isVisible tunable
          StackPanel.children [
              (ControlCaption "Limits:").lightSourceEditorLabel
              TextBox.create [
                  TextBox.name ConstructorUiId.sourceTunableMinimumBox.value
                  TextBox.width 110.0
                  TextBox.text m.minimumText.value
                  TextBox.onTextChanged (ControlCaption >> SetMinimumText >> dispatch)
              ] :> IView
              TextBlock.create [ TextBlock.text "to"; TextBlock.verticalAlignment VerticalAlignment.Center ] :> IView
              TextBox.create [
                  TextBox.name ConstructorUiId.sourceTunableMaximumBox.value
                  TextBox.width 110.0
                  TextBox.text m.maximumText.value
                  TextBox.onTextChanged (ControlCaption >> SetMaximumText >> dispatch)
              ] :> IView
              TextBlock.create [ TextBlock.text (m.unit.abbreviation); TextBlock.verticalAlignment VerticalAlignment.Center ] :> IView
          ]
      ] :> IView ]

let private waveLengthRow (m : LightSourceEditorModel) (dispatch : LightSourceEditorMsg -> unit) : IView =
    StackPanel.create [
        StackPanel.orientation Orientation.Horizontal
        StackPanel.spacing 6.0
        StackPanel.children (
            [ (match m.tuning with
               | TunableChoice -> ControlCaption "Setting:"
               | FixedChoice -> ControlCaption "Wavelength:").lightSourceEditorLabel
              TextBox.create [
                  TextBox.name ConstructorUiId.sourceWaveLengthBox.value
                  TextBox.width 110.0
                  TextBox.text m.waveLengthText.value
                  TextBox.onTextChanged (ControlCaption >> SetWaveLengthText >> dispatch)
              ] :> IView ]
            @ (waveLengthUnits
               |> List.map (fun u ->
                   clickBox (ConstructorUiId.sourceWaveLengthUnit u.uiKey) u.caption (if u = m.unit then chosenBackground else idleBackground) (fun () -> dispatch (SetUnit u)))))
    ] :> IView

type LightSourceEditorModel with

    member internal this.readoutRow : IView =
        TextBlock.create [
            TextBlock.name ConstructorUiId.sourceWaveLengthReadout.value
            TextBlock.margin (Thickness(96.0, 0.0, 0.0, 0.0))
            TextBlock.text this.readoutCaption.value
        ] :> IView

/// A function, not a value: a brush built at module initialisation belongs to whichever thread first
/// touched the module, and rendering it on the UI thread would then throw.
let private noteRow () : IView =
    TextBlock.create [
        TextBlock.foreground (brush hintColor)
        TextBlock.textWrapping TextWrapping.Wrap
        TextBlock.text "A light source is monochromatic. A fixed line always runs at its wavelength; a tunable source runs at its setting unless an experiment varies its wavelength (within the limits) or sets another value."
    ] :> IView

let private statusRow (m : LightSourceEditorModel) : IView =
    TextBlock.create [
        TextBlock.name ConstructorUiId.sourceEditorStatus.value
        TextBlock.foreground (brush errorColor)
        TextBlock.text (
            match m.status with
            | Some reason -> reason.value
            | None -> "")
    ] :> IView

let private actionsRow (m : LightSourceEditorModel) (dispatch : LightSourceEditorMsg -> unit) : IView =
    match m.exit with
    | LightSourceEditing ->
        StackPanel.create [
            StackPanel.orientation Orientation.Horizontal
            StackPanel.children [
                actionButton ConstructorUiId.sourceSaveButton (ControlCaption "Save") saveBackground (fun () -> dispatch LightSourceSaveClicked)
                actionButton ConstructorUiId.sourceCancelButton (ControlCaption "Cancel") cancelBackground (fun () -> dispatch LightSourceCancelClicked)
            ]
        ] :> IView
    | LightSourceConfirmingDiscard ->
        StackPanel.create [
            ConstructorUiId.sourceExitConfirm.automationAttribute<StackPanel> ()
            StackPanel.orientation Orientation.Horizontal
            StackPanel.children [
                TextBlock.create [
                    TextBlock.text "Discard unsaved changes?"
                    TextBlock.foreground (brush errorColor)
                    TextBlock.verticalAlignment VerticalAlignment.Center
                    TextBlock.margin (Thickness(0.0, 0.0, 12.0, 0.0))
                ] :> IView
                actionButton ConstructorUiId.sourceDiscardButton (ControlCaption "Discard changes") cancelBackground (fun () -> dispatch LightSourceDiscardConfirmed)
                actionButton ConstructorUiId.sourceKeepEditingButton (ControlCaption "Keep editing") saveBackground (fun () -> dispatch LightSourceKeepEditing)
            ]
        ] :> IView

let lightSourceEditorView (m : LightSourceEditorModel) (dispatch : LightSourceEditorMsg -> unit) : IView =
    StackPanel.create [
        StackPanel.orientation Orientation.Vertical
        StackPanel.spacing 8.0
        StackPanel.margin (Thickness 12.0)
        StackPanel.children (
            [ nameRow m dispatch ]
            @ tuningRows m dispatch
            @ [
            waveLengthRow m dispatch
            m.readoutRow
            noteRow ()
            statusRow m
            actionsRow m dispatch
            ])
    ] :> IView
