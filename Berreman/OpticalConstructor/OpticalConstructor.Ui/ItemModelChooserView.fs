/// The library's model chooser, a window of its own the Items page's New item… opens, so a large library has the whole
/// screen: every model, light source, polarizer and sample a kit item can instantiate, narrowed by a filter, one chosen,
/// then Choose (spec 0051 `.manual/007` 4.3).
module OpticalConstructor.Ui.ItemModelChooserView

open Avalonia
open Avalonia.Automation
open Avalonia.Controls
open Avalonia.Controls.Primitives
open Avalonia.Layout
open Avalonia.Media
open Avalonia.FuncUI.Builder
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open OpticalConstructor.Controls
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Ui.TableAndElementRotationView

/// The chooser's state: the offers, the filter as typed and the one chosen (spec 0051 `.manual/007` 4.3).
type ItemModelChooserModel =
    {
        offers : ItemModelChoice list
        filter : ControlCaption
        chosenOpt : ItemModelChoice option
    }

    /// The chooser over the request's offers, its chosen model already chosen.
    static member ofRequest (request : ItemModelChooserRequest) : ItemModelChooserModel =
        {
            offers = request.offers
            filter = ControlCaption ""
            chosenOpt = request.chosenOpt
        }

    /// The offers whose caption holds every word of the filter, ignoring case; all of them for a blank filter.
    member this.shown : ItemModelChoice list =
        let words = this.filter.value.Split([| ' ' |], System.StringSplitOptions.RemoveEmptyEntries) |> List.ofArray
        this.offers
        |> List.filter (fun offer ->
            words |> List.forall (fun word -> offer.caption.value.IndexOf(word, System.StringComparison.OrdinalIgnoreCase) >= 0))

/// The chooser's messages: the button and the test send the same one (spec 0051 `.manual/007` 4.3).
type ItemModelChooserMsg =
    | SetChooserFilter of ControlCaption
    | ChooseChooserOffer of ItemModelChoice

/// The chooser's update: it writes nothing; Choose hands the chosen offer to the request (spec 0051 `.manual/007` 4.3).
let updateItemModelChooser (msg : ItemModelChooserMsg) (model : ItemModelChooserModel) : ItemModelChooserModel =
    match msg with
    | SetChooserFilter filter -> { model with filter = filter }
    | ChooseChooserOffer offer when List.contains offer model.offers -> { model with chosenOpt = Some offer }
    | ChooseChooserOffer _ -> model

/// The offer's group heading: what kind of library entry it instantiates.
type ItemModelChoice with

    member this.chooserGroup : ControlCaption =
        match this with
        | LibraryModelChoice _ -> ControlCaption "Item models"
        | SourcePresetChoice _ -> ControlCaption "Light sources"
        | PolarizerPresetChoice _ -> ControlCaption "Polarizers"
        | StackSampleChoice _ -> ControlCaption "Samples"

/// The chooser: the filter, the shown offers by group in a scroller that fills the window, then Choose (withheld until
/// one is chosen) and Cancel; `confirm` and `cancel` close the window (spec 0051 `.manual/007` 4.3).
let itemModelChooserView
    (model : ItemModelChooserModel)
    (dispatch : ItemModelChooserMsg -> unit)
    (confirm : ItemModelChoice -> unit)
    (cancel : unit -> unit)
    : IView =
    let groups =
        model.shown
        |> List.groupBy (fun offer -> offer.chooserGroup)
        |> List.collect (fun (heading, offers) ->
            TextBlock.create [
                TextBlock.fontWeight FontWeight.SemiBold
                TextBlock.margin (Thickness(0.0, 8.0, 0.0, 2.0))
                TextBlock.text heading.value
            ]
            :> IView
            :: (offers
                |> List.map (fun offer ->
                    let mark = if model.chosenOpt = Some offer then ChosenChoice else UnchosenChoice
                    Border.create [
                        Border.horizontalAlignment HorizontalAlignment.Left
                        // A caption names its binding, model, version and GUIDs (spec 0050 S7.2): it wraps at the window's width.
                        Border.child (
                            CommandControls.choiceToggleShowing
                                (ConstructorUiId.addItemOffer offer)
                                offer.caption
                                offer.caption.wrappingText
                                mark
                                PositiveCommand
                                ControlOffered
                                (fun () -> dispatch (ChooseChooserOffer offer))
                                [ ToggleButton.margin (Thickness(0.0, 0.0, 8.0, 4.0)) ])
                    ]
                    :> IView)))
    let listed : IView list =
        match groups with
        | [] -> [ TextBlock.create [ TextBlock.text "Nothing in the library matches the filter." ] :> IView ]
        | shown -> shown
    let chooseAvailability =
        match model.chosenOpt with
        | Some _ -> ControlOffered
        | None -> ControlWithheld
    DockPanel.create [
        DockPanel.margin (Thickness 10.0)
        DockPanel.children [
            StackPanel.create [
                StackPanel.dock Dock.Top
                StackPanel.orientation Orientation.Vertical
                StackPanel.children [
                    TextBlock.create [
                        TextBlock.textWrapping TextWrapping.Wrap
                        TextBlock.margin (Thickness(0.0, 0.0, 0.0, 6.0))
                        TextBlock.text "Choose the model the new kit item instantiates. Type words to narrow the list."
                    ]
                    TextBox.create [
                        yield! ConstructorUiId.itemModelChooserFilter.inputAttributes<TextBox> ()
                        TextBox.placeHolderText "Filter"
                        TextBox.margin (Thickness(0.0, 0.0, 0.0, 6.0))
                        TextBox.text model.filter.value
                        TextBox.onTextChanged (Option.ofObj >> Option.map ControlCaption >> Option.iter (SetChooserFilter >> dispatch))
                    ]
                ]
            ]
            StackPanel.create [
                StackPanel.dock Dock.Bottom
                StackPanel.orientation Orientation.Horizontal
                StackPanel.margin (Thickness(0.0, 8.0, 0.0, 0.0))
                StackPanel.children [
                    CommandControls.actionCommandWhen chooseAvailability ConstructorUiId.itemModelChooserChooseButton (ControlCaption "Choose") PositiveCommand (fun () ->
                        model.chosenOpt |> Option.iter confirm)
                    CommandControls.actionCommand ConstructorUiId.itemModelChooserCancelButton (ControlCaption "Cancel") NegativeCommand cancel
                ]
            ]
            |> View.withKey $"ChooserActions_{chooseAvailability.isEnabled}"
            ScrollViewer.create [
                yield! ConstructorUiId.addItemOfferList.groupAttributes<ScrollViewer> ()
                ScrollViewer.verticalScrollBarVisibility ScrollBarVisibility.Auto
                ScrollViewer.horizontalScrollBarVisibility ScrollBarVisibility.Disabled
                ScrollViewer.content (
                    StackPanel.create [
                        StackPanel.orientation Orientation.Vertical
                        StackPanel.children listed
                    ])
            ]
        ]
    ]
    :> IView
