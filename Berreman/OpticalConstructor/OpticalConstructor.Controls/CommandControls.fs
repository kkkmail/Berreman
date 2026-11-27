namespace OpticalConstructor.Controls

open Avalonia
open Avalonia.Automation
open Avalonia.Automation.Peers
open Avalonia.Controls
open Avalonia.Controls.Primitives
open Avalonia.Input
open Avalonia.Layout
open Avalonia.Media
open Avalonia.FuncUI.Builder
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types

/// Whether a bar, or one control of it, is offered to the user (spec 0048, the shape of STL's
/// `VncCommandAvailability`). Shared by every command control.
type ControlAvailability =
    | ControlOffered
    | ControlWithheld

    /// Whether the control is enabled: the one place this pair becomes Avalonia's own `bool`.
    member this.isEnabled : bool =
        match this with
        | ControlOffered -> true
        | ControlWithheld -> false

/// The text a control shows: a button's caption, a knob's or a row's label, a title, a description, or
/// the text in a text box.
type ControlCaption =
    | ControlCaption of string

    member this.value = let (ControlCaption c) = this in c

    /// This text as the key that tells its control apart from its siblings (a chart series' toggle).
    member this.uiKey : ConstructorUiKey = ConstructorUiKey this.value

    /// This text as a command control's content: it wraps when the control is given a width.
    member this.wrappingText : IView = TextBlock.create [ TextBlock.text this.value; TextBlock.textWrapping TextWrapping.Wrap ] :> IView

/// The fill and outline a command control is drawn with, the one palette of the app's buttons and chips
/// (spec 0051 `.manual/003` 1.3).
type CommandEmphasis =
    | NeutralCommand
    | PositiveCommand
    | NegativeCommand
    | ChosenCommand
    | ArmedCommand
    | PendingCommand
    | MenuCommand

    member this.fill : Color =
        match this with
        | NeutralCommand -> Color.FromRgb(232uy, 232uy, 232uy)
        | PositiveCommand -> Color.FromRgb(186uy, 224uy, 186uy)
        | NegativeCommand -> Color.FromRgb(236uy, 202uy, 202uy)
        | ChosenCommand -> Color.FromRgb(150uy, 185uy, 235uy)
        | ArmedCommand -> Color.FromRgb(204uy, 232uy, 255uy)
        | PendingCommand -> Color.FromRgb(255uy, 224uy, 160uy)
        | MenuCommand -> Color.FromRgb(255uy, 255uy, 255uy)

    member this.outline : Color =
        match this with
        | ArmedCommand -> Color.FromRgb(0uy, 120uy, 212uy)
        | MenuCommand -> Color.FromRgb(255uy, 255uy, 255uy)
        | NeutralCommand | PositiveCommand | NegativeCommand | ChosenCommand | PendingCommand -> Color.FromRgb(120uy, 120uy, 120uy)

/// Whether a choice is the chosen one; the model alone decides it (spec 0051 `.manual/003` 1.2 item 5).
type ChoiceMark =
    | ChosenChoice
    | UnchosenChoice

    member this.isChecked : bool =
        match this with
        | ChosenChoice -> true
        | UnchosenChoice -> false

/// The Fluent theme's resource key stem of a button family.
type private ResourceKeyPrefix =
    | ButtonKeys
    | ToggleButtonKeys

    member this.background : string =
        match this with
        | ButtonKeys -> "ButtonBackground"
        | ToggleButtonKeys -> "ToggleButtonBackground"

    member this.border : string =
        match this with
        | ButtonKeys -> "ButtonBorderBrush"
        | ToggleButtonKeys -> "ToggleButtonBorderBrush"

    member this.foreground : string =
        match this with
        | ButtonKeys -> "ButtonForeground"
        | ToggleButtonKeys -> "ToggleButtonForeground"

/// The visual states a look is written for: the plain ones, or a toggle's checked or indeterminate ones.
type private ResourceKeyStates =
    | PlainStates
    | CheckedStates
    | IndeterminateStates

    member this.suffixes : string list =
        match this with
        | PlainStates -> [ ""; "PointerOver"; "Pressed"; "Disabled" ]
        | CheckedStates -> [ "Checked"; "CheckedPointerOver"; "CheckedPressed"; "CheckedDisabled" ]
        | IndeterminateStates -> [ "Indeterminate"; "IndeterminatePointerOver"; "IndeterminatePressed"; "IndeterminateDisabled" ]

module private CommandPaint =

    /// Writes `fill`, `outline` and black text over every state key of `prefix` in the Fluent theme, so pointer-over,
    /// press and disabled keep the look (spec 0051 `.manual/003` 1.3). The brushes are made per control: a brush belongs
    /// to the thread that made it, so none is shared between UI threads.
    let paint (resources : IResourceDictionary) (prefix : ResourceKeyPrefix) (states : ResourceKeyStates) (emphasis : CommandEmphasis) : unit =
        let fill = SolidColorBrush(emphasis.fill) :> IBrush
        let outline = SolidColorBrush(emphasis.outline) :> IBrush
        let textBrush = SolidColorBrush(Color.FromRgb(0uy, 0uy, 0uy)) :> IBrush
        for state in states.suffixes do
            resources[prefix.background + state] <- fill
            resources[prefix.border + state] <- outline
            resources[prefix.foreground + state] <- textBrush

/// A real `Button` drawn in one `CommandEmphasis`: UI Automation sees it, Tab reaches it and Enter or Space
/// presses it (spec 0051 `.manual/003` 1.2). It remembers the modifiers of the press that clicked it.
type CommandButton() as this =
    inherit Button()

    static let emphasisProperty : StyledProperty<CommandEmphasis> =
        AvaloniaProperty.Register<CommandButton, CommandEmphasis>("Emphasis", NeutralCommand)

    let mutable modifiers = KeyModifiers.None

    do CommandPaint.paint this.Resources ButtonKeys PlainStates NeutralCommand

    static member EmphasisProperty : StyledProperty<CommandEmphasis> = emphasisProperty

    member this.Emphasis
        with get () : CommandEmphasis = this.GetValue(emphasisProperty)
        and set (value : CommandEmphasis) = this.SetValue(emphasisProperty, value) |> ignore

    /// The modifiers held when the click began; none for a keyboard click without them.
    member _.pressModifiers : KeyModifiers = modifiers

    override _.StyleKeyOverride = typeof<Button>

    override _.OnPointerPressed(e : PointerPressedEventArgs) =
        modifiers <- e.KeyModifiers
        base.OnPointerPressed e

    override _.OnKeyDown(e : KeyEventArgs) =
        modifiers <- e.KeyModifiers
        base.OnKeyDown e

    override this.OnPropertyChanged(change : AvaloniaPropertyChangedEventArgs) =
        base.OnPropertyChanged change
        if change.Property = (emphasisProperty :> AvaloniaProperty) then
            CommandPaint.paint this.Resources ButtonKeys PlainStates this.Emphasis

/// A `ToggleButton` whose checked state only the model sets: a click asks for the choice and the next render shows it
/// (spec 0051 `.manual/003` 1.2 item 5). UI Automation reads the choice through its toggle pattern.
type ChoiceToggle() as this =
    inherit ToggleButton()

    static let emphasisProperty : StyledProperty<CommandEmphasis> =
        AvaloniaProperty.Register<ChoiceToggle, CommandEmphasis>("Emphasis", NeutralCommand)

    static let chosenEmphasisProperty : StyledProperty<CommandEmphasis> =
        AvaloniaProperty.Register<ChoiceToggle, CommandEmphasis>("ChosenEmphasis", PositiveCommand)

    do
        CommandPaint.paint this.Resources ToggleButtonKeys PlainStates NeutralCommand
        CommandPaint.paint this.Resources ToggleButtonKeys IndeterminateStates NeutralCommand
        CommandPaint.paint this.Resources ToggleButtonKeys CheckedStates PositiveCommand

    static member EmphasisProperty : StyledProperty<CommandEmphasis> = emphasisProperty

    static member ChosenEmphasisProperty : StyledProperty<CommandEmphasis> = chosenEmphasisProperty

    member this.Emphasis
        with get () : CommandEmphasis = this.GetValue(emphasisProperty)
        and set (value : CommandEmphasis) = this.SetValue(emphasisProperty, value) |> ignore

    member this.ChosenEmphasis
        with get () : CommandEmphasis = this.GetValue(chosenEmphasisProperty)
        and set (value : CommandEmphasis) = this.SetValue(chosenEmphasisProperty, value) |> ignore

    override _.StyleKeyOverride = typeof<ToggleButton>

    override _.Toggle() = ()

    override this.OnPropertyChanged(change : AvaloniaPropertyChangedEventArgs) =
        base.OnPropertyChanged change
        if change.Property = (emphasisProperty :> AvaloniaProperty) then
            CommandPaint.paint this.Resources ToggleButtonKeys PlainStates this.Emphasis
            CommandPaint.paint this.Resources ToggleButtonKeys IndeterminateStates this.Emphasis
        elif change.Property = (chosenEmphasisProperty :> AvaloniaProperty) then
            CommandPaint.paint this.Resources ToggleButtonKeys CheckedStates this.ChosenEmphasis

/// The only builders of pressable and choosable controls (spec 0051 `.manual/003` 1.3): each carries its id as
/// `AutomationProperties.AutomationId` and its caption as `AutomationProperties.Name`.
module CommandControls =

    /// `defaults` without the attributes `extras` sets, then `extras`: FuncUI keeps the first attribute of a property,
    /// so an override must replace its default rather than follow it.
    let private merged<'Control> (defaults : IAttr<'Control> list) (extras : IAttr<'Control> list) : IAttr<'Control> list =
        let overridden = extras |> List.map (fun (attr : IAttr<'Control>) -> (attr :> IAttr).UniqueName) |> Set.ofList
        (defaults |> List.filter (fun (attr : IAttr<'Control>) -> not (overridden.Contains (attr :> IAttr).UniqueName))) @ extras

    let private commandAttributes (id : ConstructorUiId) (caption : ControlCaption) (content : IView) (emphasis : CommandEmphasis) (availability : ControlAvailability) : IAttr<CommandButton> list =
        [
            id.automationAttribute<CommandButton> ()
            AttrBuilder<CommandButton>.CreateProperty<string>(AutomationProperties.NameProperty, caption.value, ValueNone)
            AttrBuilder<CommandButton>.CreateProperty<CommandEmphasis>(CommandButton.EmphasisProperty, emphasis, ValueNone)
            Button.content content
            Button.isEnabled availability.isEnabled
            Button.opacity (match availability with ControlOffered -> 1.0 | ControlWithheld -> 0.4)
            Button.borderThickness 1.0
            Button.cornerRadius (CornerRadius 3.0)
            Button.padding (Thickness(12.0, 5.0))
            Button.verticalAlignment VerticalAlignment.Center
        ]

    /// A command button showing `content` (a figure, or a row of texts) and named by `caption` for UI Automation,
    /// handed the button it was pressed on. `extras` comes last, so it overrides the defaults (padding, margin, width).
    let commandButtonShowing
        (id : ConstructorUiId)
        (caption : ControlCaption)
        (content : IView)
        (emphasis : CommandEmphasis)
        (availability : ControlAvailability)
        (onClick : CommandButton -> unit)
        (extras : IAttr<CommandButton> list)
        : IView =
        let click (e : Avalonia.Interactivity.RoutedEventArgs) : unit =
            e.Handled <- true
            match e.Source with
            | :? CommandButton as button -> onClick button
            | _ -> ()
        ViewBuilder.Create<CommandButton>(
            merged (commandAttributes id caption content emphasis availability @ [ Button.onClick (click, SubPatchOptions.Always) ]) extras)
        |> Avalonia.FuncUI.DSL.View.withKey id.value
        :> IView

    /// A command button handed the button it was pressed on, for the window it sits in, a sibling text box or the
    /// press's modifiers.
    let commandButtonOn
        (id : ConstructorUiId)
        (caption : ControlCaption)
        (emphasis : CommandEmphasis)
        (availability : ControlAvailability)
        (onClick : CommandButton -> unit)
        (extras : IAttr<CommandButton> list)
        : IView =
        commandButtonShowing id caption caption.wrappingText emphasis availability onClick extras

    /// A command button whose action needs nothing from the press.
    let commandButton
        (id : ConstructorUiId)
        (caption : ControlCaption)
        (emphasis : CommandEmphasis)
        (availability : ControlAvailability)
        (onClick : unit -> unit)
        (extras : IAttr<CommandButton> list)
        : IView =
        commandButtonOn id caption emphasis availability (fun _ -> onClick ()) extras

    /// A choice showing `content` and named by `caption`, the model marking it chosen or not, drawn neutral or in
    /// `chosenLook`; a click asks for it through `onChoose`.
    let choiceToggleShowing
        (id : ConstructorUiId)
        (caption : ControlCaption)
        (content : IView)
        (mark : ChoiceMark)
        (chosenLook : CommandEmphasis)
        (availability : ControlAvailability)
        (onChoose : unit -> unit)
        (extras : IAttr<ChoiceToggle> list)
        : IView =
        let choose (e : Avalonia.Interactivity.RoutedEventArgs) : unit =
            e.Handled <- true
            onChoose ()
        ViewBuilder.Create<ChoiceToggle>(
            merged
                [
                    id.automationAttribute<ChoiceToggle> ()
                    AttrBuilder<ChoiceToggle>.CreateProperty<string>(AutomationProperties.NameProperty, caption.value, ValueNone)
                    AttrBuilder<ChoiceToggle>.CreateProperty<CommandEmphasis>(ChoiceToggle.ChosenEmphasisProperty, chosenLook, ValueNone)
                    ToggleButton.isChecked mark.isChecked
                    ToggleButton.content content
                    ToggleButton.isEnabled availability.isEnabled
                    ToggleButton.opacity (match availability with ControlOffered -> 1.0 | ControlWithheld -> 0.4)
                    ToggleButton.borderThickness 1.0
                    ToggleButton.cornerRadius (CornerRadius 3.0)
                    ToggleButton.padding (Thickness(12.0, 5.0))
                    ToggleButton.verticalAlignment VerticalAlignment.Center
                    ToggleButton.onClick (choose, SubPatchOptions.Always)
                ]
                extras)
        |> Avalonia.FuncUI.DSL.View.withKey id.value
        :> IView

    /// A choice the model marks chosen or not, showing its caption.
    let choiceToggle
        (id : ConstructorUiId)
        (caption : ControlCaption)
        (mark : ChoiceMark)
        (chosenLook : CommandEmphasis)
        (availability : ControlAvailability)
        (onChoose : unit -> unit)
        (extras : IAttr<ChoiceToggle> list)
        : IView =
        choiceToggleShowing id caption caption.wrappingText mark chosenLook availability onChoose extras

    /// A neutral verb (Add, Edit, Export…), handed the pressed button for its owner window or a sibling text box.
    let verbCommandOn (id : ConstructorUiId) (caption : ControlCaption) (onPressed : CommandButton -> unit) : IView =
        commandButtonOn id caption NeutralCommand ControlOffered onPressed [ Button.margin (Thickness(0.0, 0.0, 8.0, 4.0)) ]

    /// A neutral verb whose action needs nothing from the press.
    let verbCommand (id : ConstructorUiId) (caption : ControlCaption) (onClick : unit -> unit) : IView =
        verbCommandOn id caption (fun _ -> onClick ())

    /// The one line saying why a control beside it is withheld, or nothing while it is offered (spec 0051 `.manual/003`
    /// 3.6: a precondition is visible, never a hidden control).
    let withheldReason (id : ConstructorUiId) (reasonOpt : ControlCaption option) : IView list =
        match reasonOpt with
        | Some reason ->
            [
                TextBlock.create [
                    id.automationAttribute<TextBlock> ()
                    TextBlock.text reason.value
                    TextBlock.textWrapping TextWrapping.Wrap
                    TextBlock.fontSize 11.0
                    TextBlock.foreground (SolidColorBrush(Color.FromRgb(90uy, 90uy, 90uy)))
                    TextBlock.margin (Thickness(0.0, 2.0, 0.0, 4.0))
                ]
                |> Avalonia.FuncUI.DSL.View.withKey id.value
                :> IView
            ]
        | None -> []

    /// A window-level action (Save, Cancel, Apply, Select…), wider-padded than a verb, offered or withheld.
    let actionCommandWhen
        (availability : ControlAvailability)
        (id : ConstructorUiId)
        (caption : ControlCaption)
        (emphasis : CommandEmphasis)
        (onClick : unit -> unit)
        : IView =
        commandButton id caption emphasis availability onClick
            [ Button.padding (Thickness(22.0, 6.0)); Button.margin (Thickness(0.0, 0.0, 10.0, 4.0)) ]

    /// A window-level action, always offered.
    let actionCommand (id : ConstructorUiId) (caption : ControlCaption) (emphasis : CommandEmphasis) (onClick : unit -> unit) : IView =
        actionCommandWhen ControlOffered id caption emphasis onClick

    /// A choice that turns green while it is the chosen one.
    let choiceCommand (id : ConstructorUiId) (caption : ControlCaption) (mark : ChoiceMark) (onChoose : unit -> unit) : IView =
        choiceToggle id caption mark PositiveCommand ControlOffered onChoose [ ToggleButton.margin (Thickness(0.0, 0.0, 8.0, 4.0)) ]
