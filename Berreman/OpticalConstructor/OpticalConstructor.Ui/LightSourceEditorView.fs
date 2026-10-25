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
open Avalonia.Automation
open Avalonia.Controls
open Avalonia.Layout
open Avalonia.Media
open Avalonia.FuncUI.Builder
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open Berreman.Constants
open Berreman.Fields
open OpticalConstructor.Controls
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.Units

/// How the editor OPENS: a NEW source (the Library's Add source verb — its entry id minted at window
/// open) or an EXISTING user source edited in place (the Edit verb).
type SourceEditorIntent =
    | NewSource of mintedId : string
    | EditSource of SourcePreset

/// The editor's IO seam (the functional-proxy Context convention): the user-sources write store Save
/// persists through, and the host's close request (the window passes `this.Close`). Reference-compared.
[<ReferenceEquality>]
type SourceEditorContext =
    {
        sources : SourceProxy
        requestClose : unit -> unit
    }

/// Whether the editor is editing normally or showing the unsaved-edit exit confirm (the Sample editor's
/// two-case gate).
type ExitPrompt =
    | Editing
    | ConfirmingDiscard

/// The slice Save persists, compared for dirtiness: the name and the wavelength in metres (`None` while
/// the typed wavelength does not parse).
type SourceEditSnapshot = string * float<meter> option

/// The editor's model. `waveLengthText` is the raw box text; `waveLength` is what it parses to in `unit`
/// (kept unchanged when only the unit changes, so switching units never moves the value).
type Model =
    {
        context : SourceEditorContext
        sourceId : string
        freshness : WindowLauncher.EntryFreshness
        name : string
        waveLengthText : string
        waveLength : WaveLength option
        unit : UnitOfMeasure
        initialEdit : SourceEditSnapshot
        exit : ExitPrompt
        status : string option
    }

type Msg =
    | SetName of string
    | SetWaveLengthText of string
    /// Show (and type) the wavelength in another unit — the value itself is unchanged.
    | SetUnit of UnitOfMeasure
    | SaveClicked
    /// Requested exit (the Cancel button, or the window chrome through `OnClosing`): a pristine editor
    /// closes, a dirty one shows the discard confirm.
    | CancelClicked
    | DiscardConfirmed
    | KeepEditing

/// The wavelength units the editor offers, in display order.
let waveLengthUnits : UnitOfMeasure list = [ Nanometer; Micrometer; Angstrom; ElectronVolt; Wavenumber ]

/// The ASCII automation-id code of a unit choice (the display symbols carry µ / ⁻¹).
let unitCode (u : UnitOfMeasure) : string =
    match u with
    | Nanometer -> "nm"
    | Micrometer -> "um"
    | Angstrom -> "A"
    | ElectronVolt -> "eV"
    | Wavenumber -> "cm-1"
    | Meter -> "m"
    | Millimeter -> "mm"

/// A NEW source's starting wavelength — the seeded source's 600 nm.
let defaultWaveLength : WaveLength = WaveLength.nm 600.0<nm>

/// The wavelength as the box shows it in `u` (six significant digits, invariant culture).
let formatIn (u : UnitOfMeasure) (w : WaveLength) : string =
    (wavelengthToUnit u w).ToString("G6", CultureInfo.InvariantCulture)

/// Parse a typed wavelength in `u`: a positive finite number, else `None`.
let parseIn (u : UnitOfMeasure) (text : string) : WaveLength option =
    match Double.TryParse(text, NumberStyles.Float, CultureInfo.InvariantCulture) with
    | true, v when v > 0.0 && not (Double.IsInfinity v) -> Some (toWaveLength u v)
    | _ -> None

let private metresOf (w : WaveLength option) : float<meter> option = w |> Option.map (fun x -> x.value)

let private currentEdit (m : Model) : SourceEditSnapshot = (m.name, metresOf m.waveLength)

/// Whether the editor carries unsaved changes (the edit slice differs from the one captured at load).
let isDirty (m : Model) : bool = m.initialEdit <> currentEdit m

let init (context : SourceEditorContext) (intent : SourceEditorIntent) : Model =
    let sourceId, freshness, name, waveLength =
        match intent with
        | NewSource mintedId -> mintedId, WindowLauncher.NewUnsaved, "", defaultWaveLength
        | EditSource s -> s.id, WindowLauncher.Persisted, s.name, s.waveLength
    {
        context = context
        sourceId = sourceId
        freshness = freshness
        name = name
        waveLengthText = formatIn Nanometer waveLength
        waveLength = Some waveLength
        unit = Nanometer
        initialEdit = (name, Some waveLength.value)
        exit = Editing
        status = None
    }

/// The source Save would store, or the reason it cannot yet.
let toSource (m : Model) : Result<SourcePreset, string> =
    match m.waveLength with
    | Some w -> Ok { id = m.sourceId; name = m.name.Trim(); waveLength = w; protection = UserManaged }
    | None -> Error $"enter a positive wavelength in %s{unitAbbrev m.unit}"

let update (msg : Msg) (m : Model) : Model =
    match msg with
    | SetName s -> { m with name = s }
    | SetWaveLengthText t ->
        // The box echoes every re-render of the formatted value; text equal to the current value's own
        // formatting keeps the stored value exactly (no rounding drift from a unit switch).
        let unchanged =
            match m.waveLength with
            | Some w -> formatIn m.unit w = t
            | None -> false
        if unchanged then { m with waveLengthText = t }
        else { m with waveLengthText = t; waveLength = parseIn m.unit t }
    | SetUnit u ->
        match m.waveLength with
        | Some w -> { m with unit = u; waveLengthText = formatIn u w }
        | None -> { m with unit = u; waveLength = parseIn u m.waveLengthText }
    | SaveClicked ->
        match toSource m with
        | Ok source ->
            match m.context.sources.saveSource source with
            | Ok () ->
                m.context.requestClose ()
                { m with status = None }
            | Error e -> { m with status = Some e.reason }
        | Error reason -> { m with status = Some reason }
    | CancelClicked ->
        if isDirty m then { m with exit = ConfirmingDiscard }
        else
            m.context.requestClose ()
            m
    | DiscardConfirmed ->
        m.context.requestClose ()
        m
    | KeepEditing -> { m with exit = Editing }

// ---------------------------------------------------------------------------
// The FuncUI view (the Sample editor's look: idle / chosen boxes, Save / Cancel action pair).
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

let private automationId<'t when 't :> Control> (autoId : string) : IAttr<'t> =
    AttrBuilder<'t>.CreateProperty<string>(AutomationProperties.AutomationIdProperty, autoId, ValueNone)

/// A clickable box (keyed by its id; highlighted when chosen).
let private clickBox (autoId : string) (label : string) (background : Color) (onClick : unit -> unit) : IView =
    Border.create [
        automationId<Border> autoId
        Border.background (brush background)
        Border.borderBrush (brush idleBorder)
        Border.borderThickness 1.0
        Border.cornerRadius (CornerRadius 3.0)
        Border.padding (Thickness(10.0, 4.0))
        Border.margin (Thickness(0.0, 0.0, 6.0, 0.0))
        Border.verticalAlignment VerticalAlignment.Center
        Border.child (TextBlock.create [ TextBlock.text label ])
        Border.onPointerPressed ((fun e -> e.Handled <- true; onClick ()), SubPatchOptions.OnChangeOf (box (autoId, label, background)))
    ]
    |> Avalonia.FuncUI.DSL.View.withKey autoId
    :> IView

let private actionButton (autoId : string) (label : string) (background : Color) (onClick : unit -> unit) : IView =
    Border.create [
        automationId<Border> autoId
        Border.background (brush background)
        Border.borderBrush (brush idleBorder)
        Border.borderThickness 1.0
        Border.cornerRadius (CornerRadius 3.0)
        Border.padding (Thickness(22.0, 6.0))
        Border.margin (Thickness(0.0, 0.0, 10.0, 0.0))
        Border.child (TextBlock.create [ TextBlock.text label ])
        Border.onPointerPressed ((fun e -> e.Handled <- true; onClick ()), SubPatchOptions.OnChangeOf autoId)
    ]
    |> Avalonia.FuncUI.DSL.View.withKey autoId
    :> IView

let private labelBlock (text : string) : IView =
    TextBlock.create [ TextBlock.text text; TextBlock.width 90.0; TextBlock.verticalAlignment VerticalAlignment.Center ] :> IView

let private nameRow (m : Model) (dispatch : Msg -> unit) : IView =
    StackPanel.create [
        StackPanel.orientation Orientation.Horizontal
        StackPanel.spacing 6.0
        StackPanel.children [
            labelBlock "Name:"
            TextBox.create [
                TextBox.name UiIds.SourceEditor.nameBox
                TextBox.width 320.0
                TextBox.text m.name
                TextBox.onTextChanged (SetName >> dispatch)
            ] :> IView
        ]
    ] :> IView

let private waveLengthRow (m : Model) (dispatch : Msg -> unit) : IView =
    StackPanel.create [
        StackPanel.orientation Orientation.Horizontal
        StackPanel.spacing 6.0
        StackPanel.children (
            [ labelBlock "Wavelength:"
              TextBox.create [
                  TextBox.name UiIds.SourceEditor.waveLengthBox
                  TextBox.width 110.0
                  TextBox.text m.waveLengthText
                  TextBox.onTextChanged (SetWaveLengthText >> dispatch)
              ] :> IView ]
            @ (waveLengthUnits
               |> List.map (fun u ->
                   clickBox (UiIds.SourceEditor.unitButton (unitCode u)) (unitAbbrev u) (if u = m.unit then chosenBackground else idleBackground) (fun () -> dispatch (SetUnit u)))))
    ] :> IView

/// The wavelength restated in nm and eV (the units visible-light and EUV work read), or the parse hint.
let readoutText (m : Model) : string =
    match m.waveLength with
    | Some w ->
        let nm = wavelengthToUnit Nanometer w
        let ev = wavelengthToUnit ElectronVolt w
        String.Format(CultureInfo.InvariantCulture, "λ = {0:G6} nm  ·  {1:G6} eV", nm, ev)
    | None -> $"enter a positive wavelength in %s{unitAbbrev m.unit}"

let private readoutRow (m : Model) : IView =
    TextBlock.create [
        TextBlock.name UiIds.SourceEditor.waveLengthReadout
        TextBlock.margin (Thickness(96.0, 0.0, 0.0, 0.0))
        TextBlock.text (readoutText m)
    ] :> IView

/// A function, not a value: a brush built at module initialisation belongs to whichever thread first
/// touched the module, and rendering it on the UI thread would then throw.
let private noteRow () : IView =
    TextBlock.create [
        TextBlock.foreground (brush hintColor)
        TextBlock.textWrapping TextWrapping.Wrap
        TextBlock.text "A light source is monochromatic: it sets the wavelength of every experiment run on a table it is bound to."
    ] :> IView

let private statusRow (m : Model) : IView =
    TextBlock.create [
        TextBlock.name UiIds.SourceEditor.statusText
        TextBlock.foreground (brush errorColor)
        TextBlock.text (m.status |> Option.defaultValue "")
    ] :> IView

let private actionsRow (m : Model) (dispatch : Msg -> unit) : IView =
    match m.exit with
    | Editing ->
        StackPanel.create [
            StackPanel.orientation Orientation.Horizontal
            StackPanel.children [
                actionButton UiIds.SourceEditor.saveButton "Save" saveBackground (fun () -> dispatch SaveClicked)
                actionButton UiIds.SourceEditor.cancelButton "Cancel" cancelBackground (fun () -> dispatch CancelClicked)
            ]
        ] :> IView
    | ConfirmingDiscard ->
        StackPanel.create [
            automationId<StackPanel> UiIds.SourceEditor.exitConfirm
            StackPanel.orientation Orientation.Horizontal
            StackPanel.children [
                TextBlock.create [
                    TextBlock.text "Discard unsaved changes?"
                    TextBlock.foreground (brush errorColor)
                    TextBlock.verticalAlignment VerticalAlignment.Center
                    TextBlock.margin (Thickness(0.0, 0.0, 12.0, 0.0))
                ] :> IView
                actionButton UiIds.SourceEditor.discardButton "Discard changes" cancelBackground (fun () -> dispatch DiscardConfirmed)
                actionButton UiIds.SourceEditor.keepEditingButton "Keep editing" saveBackground (fun () -> dispatch KeepEditing)
            ]
        ] :> IView

let view (m : Model) (dispatch : Msg -> unit) : IView =
    StackPanel.create [
        StackPanel.orientation Orientation.Vertical
        StackPanel.spacing 8.0
        StackPanel.margin (Thickness 12.0)
        StackPanel.children [
            nameRow m dispatch
            waveLengthRow m dispatch
            readoutRow m
            noteRow ()
            statusRow m
            actionsRow m dispatch
        ]
    ] :> IView
