/// The Options window's state, update and view: the tooltip delay for now, typed, Save offered only when it changed and
/// reads, Cancel asking first when it changed; it reaches the environment file and the app's style only through its proxy
/// (spec 0051 `.manual/007` 1.5, `006-11`).
module OpticalConstructor.Ui.OptionsView

open Avalonia
open Avalonia.Controls
open Avalonia.Layout
open Avalonia.Media
open Avalonia.Styling
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open OpticalConstructor.Controls
open OpticalConstructor.Ui.UserEnvironment

/// The style that holds the app's tooltip delay, so a new delay replaces the old one (spec 0051 `.manual/007` 1.5).
type ToolTipDelayStyle(delay : ToolTipDelay) as this =
    inherit Style(fun selector -> selector.Is<Control>())

    do this.Setters.Add(Setter(ToolTip.ShowDelayProperty, box delay.value))

    /// Puts `delay` into `styles`, in place of the delay they held (spec 0051 `.manual/007` 1.5).
    static member applyTo (styles : Styles) (delay : ToolTipDelay) : unit =
        styles |> Seq.filter (fun style -> style :? ToolTipDelayStyle) |> List.ofSeq |> List.iter (styles.Remove >> ignore)
        styles.Add(ToolTipDelayStyle delay)

/// What the Options window reads and writes: the saved delay, the save of a new one and its application to the running
/// app; the composition root bakes `environment.json` and the app's styles in, a test records (spec 0051 `.manual/007`
/// 1.5).
[<ReferenceEquality>]
type OptionsProxy =
    {
        loadDelay : unit -> ToolTipDelay
        saveDelay : ToolTipDelay -> Result<unit, ControlCaption>
        applyDelay : ToolTipDelay -> unit
    }

    /// The options kept in `file`, applied to `styles`.
    static member ofEnvironment (file : Softellect.Sys.Primitives.FileName) (styles : Styles) : OptionsProxy =
        {
            loadDelay = fun () -> (EnvironmentSettings.load file).preferences.toolTipDelay
            saveDelay =
                fun delay ->
                    let settings = EnvironmentSettings.load file
                    { settings with preferences = { settings.preferences with toolTipDelay = delay } }.save file
                    |> Result.mapError (fun error -> ControlCaption error.describe)
            applyDelay = ToolTipDelayStyle.applyTo styles
        }

/// Whether Cancel waits for "Discard your changes?" (spec 0051 `.manual/007` 1.2).
type OptionsQuestion =
    | NoOptionsQuestion
    | DiscardOptionsQuestion

/// The window's state: the saved delay, the delay as typed, the last refusal, whether Cancel waits for its answer.
type OptionsModel =
    {
        proxy : OptionsProxy
        saved : ToolTipDelay
        delayText : ControlCaption
        statusOpt : ControlCaption option
        question : OptionsQuestion
    }

    /// The delay the text reads as, when it reads.
    member this.typedDelayOpt : ToolTipDelay option = ToolTipDelay.tryParse this.delayText.value

    /// Whether the typed delay differs from the saved one.
    member this.draftState : TableAndElementRotationView.DraftState =
        match this.typedDelayOpt with
        | Some delay when delay = this.saved -> TableAndElementRotationView.DraftUnchanged
        | Some _
        | None -> TableAndElementRotationView.DraftChanged

    /// The window over the saved options.
    static member ofProxy (proxy : OptionsProxy) : OptionsModel =
        let saved = proxy.loadDelay ()
        {
            proxy = proxy
            saved = saved
            delayText = ControlCaption (saved.value.ToString(System.Globalization.CultureInfo.InvariantCulture))
            statusOpt = None
            question = NoOptionsQuestion
        }

/// The window's messages: the button and the test send the same one (spec 0051 `.manual/007` 1.5).
type OptionsMsg =
    | SetToolTipDelayText of ControlCaption
    | SaveOptions
    | CancelOptions
    | ConfirmDiscardOptions
    | KeepEditingOptions

/// What the update answers: the next state, and whether the window closes.
type OptionsOutcome =
    | OptionsStayOpen of OptionsModel
    | OptionsClose

/// The update: Save writes and applies the typed delay, Cancel closes at once when nothing changed and asks first when it
/// did (spec 0051 `.manual/007` 1.2, 1.5).
let updateOptions (msg : OptionsMsg) (model : OptionsModel) : OptionsOutcome =
    match msg with
    | SetToolTipDelayText text -> OptionsStayOpen { model with delayText = text; statusOpt = None }
    | SaveOptions ->
        match model.typedDelayOpt with
        | None -> OptionsStayOpen { model with statusOpt = Some (ControlCaption "The delay is a whole number of milliseconds from 0 to 5000.") }
        | Some delay when delay = model.saved -> OptionsStayOpen model
        | Some delay ->
            match model.proxy.saveDelay delay with
            | Ok () ->
                model.proxy.applyDelay delay
                OptionsStayOpen { model with saved = delay; statusOpt = Some (ControlCaption $"Saved: a tooltip now waits {delay.value} ms.") }
            | Error reason -> OptionsStayOpen { model with statusOpt = Some (ControlCaption $"Not saved — {reason.value}") }
    | CancelOptions ->
        match model.draftState with
        | TableAndElementRotationView.DraftUnchanged -> OptionsClose
        | TableAndElementRotationView.DraftChanged -> OptionsStayOpen { model with question = DiscardOptionsQuestion }
    | ConfirmDiscardOptions -> OptionsClose
    | KeepEditingOptions -> OptionsStayOpen { model with question = NoOptionsQuestion }

/// The Options window's help (spec 0051 `.manual/007` 1.6).
let optionsHelp : PageHelp =
    {
        paragraphs =
            [
                ControlCaption "The app's options. The tooltip delay is how long a button's explanation waits under the pointer before it shows."
                ControlCaption "Save writes the options and applies them at once; Cancel closes, asking first when something changed."
            ]
    }

/// The Options window's view (spec 0051 `.manual/007` 1.5).
let optionsView (model : OptionsModel) (dispatch : OptionsMsg -> unit) : IView =
    let saveAvailability =
        match model.draftState, model.typedDelayOpt with
        | TableAndElementRotationView.DraftChanged, Some _ -> ControlOffered
        | _ -> ControlWithheld
    let question : IView list =
        match model.question with
        | DiscardOptionsQuestion ->
            [
                WrapPanel.create [
                    WrapPanel.children [
                        PageControls.copyableText ConstructorUiId.pageLeaveQuestionText (ControlCaption "Discard your changes?") [ SelectableTextBlock.verticalAlignment VerticalAlignment.Center; SelectableTextBlock.margin (Thickness(0.0, 0.0, 10.0, 4.0)) ]
                        CommandControls.actionCommand ConstructorUiId.pageLeaveYesButton (ControlCaption "Yes, discard") NegativeCommand (fun () -> dispatch ConfirmDiscardOptions)
                        CommandControls.actionCommand ConstructorUiId.pageLeaveNoButton (ControlCaption "No, keep editing") NeutralCommand (fun () -> dispatch KeepEditingOptions)
                    ]
                ]
                :> IView
            ]
        | NoOptionsQuestion -> []
    let status : IView list =
        match model.statusOpt with
        | Some told -> [ PageControls.copyableText ConstructorUiId.optionsStatusText told [] ]
        | None -> []
    StackPanel.create [
        StackPanel.orientation Orientation.Vertical
        StackPanel.margin (Thickness 12.0)
        StackPanel.children (
            [
                PageControls.pageHelpView (ConstructorUiId.pageHelpPanel (ConstructorUiKey "Options")) optionsHelp
                DockPanel.create [
                    DockPanel.margin (Thickness(0.0, 4.0, 0.0, 6.0))
                    DockPanel.children [
                        TextBlock.create [
                            TextBlock.dock Dock.Left
                            TextBlock.verticalAlignment VerticalAlignment.Center
                            TextBlock.margin (Thickness(0.0, 0.0, 6.0, 0.0))
                            TextBlock.text "Tooltip delay (ms):"
                        ]
                        TextBox.create [
                            yield! ConstructorUiId.optionsToolTipDelayField.inputAttributes<TextBox> ()
                            TextBox.width 120.0
                            TextBox.horizontalAlignment HorizontalAlignment.Left
                            TextBox.text model.delayText.value
                            TextBox.onTextChanged (Option.ofObj >> Option.map ControlCaption >> Option.iter (SetToolTipDelayText >> dispatch))
                        ]
                    ]
                ]
                :> IView
            ]
            @ status
            @ question
            @ [
                StackPanel.create [
                    StackPanel.orientation Orientation.Horizontal
                    StackPanel.children [
                        CommandControls.actionCommandWhen saveAvailability ConstructorUiId.optionsSaveButton (ControlCaption "Save") PositiveCommand (fun () -> dispatch SaveOptions)
                        CommandControls.actionCommand ConstructorUiId.optionsCancelButton (ControlCaption "Cancel") NegativeCommand (fun () -> dispatch CancelOptions)
                    ]
                ]
                |> View.withKey $"OptionsActions_{saveAvailability.isEnabled}"
                :> IView
            ])
    ]
    :> IView
