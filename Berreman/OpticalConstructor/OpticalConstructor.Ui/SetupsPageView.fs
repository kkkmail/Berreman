/// The Setups page: the kit's setups as dynamic buttons, the chosen one's slot line, Edit and Copy… and its table under
/// its row, read-only until Edit; New setup and Copy… create (spec 0050 S8.2; 0051 `.manual/007` Phase 5).
module OpticalConstructor.Ui.SetupsPageView

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
open OpticalConstructor.Ui.KitPageFrame

/// The new setup's or the copy's name, as typed (spec 0050 S8.2; 0051 `.manual/007` 5.4, 5.6).
let private setupNameField (model : TableAndElementRotationModel) (dispatch : TableAndElementRotationMsg -> unit) : IView =
    DockPanel.create [
        DockPanel.margin (Thickness(0.0, 2.0, 0.0, 4.0))
        DockPanel.children [
            TextBlock.create [
                TextBlock.dock Dock.Left
                TextBlock.verticalAlignment VerticalAlignment.Center
                TextBlock.margin (Thickness(0.0, 0.0, 6.0, 0.0))
                TextBlock.text "Name:"
            ]
            TextBox.create [
                yield! ConstructorUiId.setupNameField.inputAttributes<TextBox> ()
                TextBox.width 320.0
                TextBox.horizontalAlignment HorizontalAlignment.Left
                TextBox.text model.setupName.value
                TextBox.onTextChanged (Option.ofObj >> Option.map ControlCaption >> Option.iter (SetSetupName >> dispatch))
            ]
        ]
    ]
    :> IView

/// The chosen setup's line: its slots in light order, then Edit and Copy… while the kit takes writes (spec 0051
/// `.manual/007` 5.1).
let private setupLineRows (model : TableAndElementRotationModel) (setup : Setups.Setup) (dispatch : TableAndElementRotationMsg -> unit) : IView list =
    let verbs : IView list =
        match model.kitWrites, model.pageMode with
        | ControlOffered, BrowsingPage ->
            [
                CommandControls.verbCommand ConstructorUiId.pageEditButton (ControlCaption "Edit") (fun () -> dispatch BeginPageEdit)
                CommandControls.verbCommand ConstructorUiId.copySetupButton (ControlCaption "Copy…") (fun () -> dispatch BeginSetupCopy)
            ]
        | ControlOffered, (EditingPageEntry | CreatingPageEntry)
        | ControlWithheld, _ -> []
    // Edit and Copy… sit on the line itself (`006-3`); a line wider than the page pushes them to the next row.
    let line =
        PageControls.copyableText
            ConstructorUiId.setupLineText
            (setup.slotLine model.kitItems)
            [ SelectableTextBlock.margin (Thickness(0.0, 0.0, 12.0, 4.0)); SelectableTextBlock.verticalAlignment VerticalAlignment.Center ]
    [
        WrapPanel.create [ WrapPanel.orientation Orientation.Horizontal; WrapPanel.children (line :: verbs) ] :> IView
        PageControls.copyableLine (ControlCaption $"Revision {setup.revision.value}") [ SelectableTextBlock.foreground (SolidColorBrush(Color.FromRgb(90uy, 90uy, 90uy))) ]
    ]

/// What the page shows under the chosen setup: its line and its read-only table, or, in Editing, its table with the
/// ribbon and the chips, then Save and Cancel (spec 0051 `.manual/007` 5.1, 5.3).
let private chosenSetupRows (model : TableAndElementRotationModel) (setup : Setups.Setup) (dispatch : TableAndElementRotationMsg -> unit) : IView list =
    match model.pageMode with
    | EditingPageEntry ->
        setupLineRows model setup dispatch
        @ [
            (ControlCaption "Put the kit's items in the slots with the chips; the tabs add, turn, move and draw the table's elements.").pageLine
            setupTableView model dispatch EditableTable
            model.draftActions (ControlCaption "Save") dispatch
        ]
    | BrowsingPage
    | CreatingPageEntry -> setupLineRows model setup dispatch @ [ setupTableView model dispatch ReadOnlyTable ]

/// The Setups page: New setup while the kit takes writes, then the setups, the chosen one open under its row; in
/// Creating, the new setup's name and table, or the copy's name, then Create and Cancel (spec 0051 `.manual/007` 5.1–5.6).
let setupsPageView (model : TableAndElementRotationModel) (dispatch : TableAndElementRotationMsg -> unit) : IView =
    let body : IView list =
        match model.pageMode, model.setupCreationOpt with
        | CreatingPageEntry, Some NewSetupCreation ->
            [
                (ControlCaption "New setup").pageHeading
                setupNameField model dispatch
                (ControlCaption "It starts from a light source and a detector. Add elements with the Add tab, and put the kit's items in the slots with the chips.").pageLine
                setupTableView model dispatch EditableTable
                model.draftActions (ControlCaption "Create") dispatch
            ]
        | CreatingPageEntry, Some (CopySetupCreation source) ->
            let line =
                match model.kitSetups |> List.tryFind (fun setup -> setup.name = source) with
                | Some setup -> [ PageControls.copyableText ConstructorUiId.setupLineText (setup.slotLine model.kitItems) [] ]
                | None -> []
            [ (ControlCaption $"Copy of {source.value}").pageHeading ]
            @ line
            @ [ setupNameField model dispatch; model.draftActions (ControlCaption "Create") dispatch ]
        | _ ->
            let newVerb : IView list =
                match model.kitWrites, model.pageMode with
                | ControlOffered, BrowsingPage ->
                    [
                        Border.create [
                            Border.horizontalAlignment HorizontalAlignment.Left
                            Border.child (CommandControls.verbCommand ConstructorUiId.newSetupButton (ControlCaption "New setup") (fun () -> dispatch BeginPageCreate))
                        ]
                        :> IView
                    ]
                | ControlOffered, (EditingPageEntry | CreatingPageEntry)
                | ControlWithheld, _ -> []
            let chosenName = model.activeSetupOpt |> Option.map (fun setup -> setup.name)
            let row (setup : Setups.Setup) : IView =
                match model.renameRowsOf (RenameSetupTarget setup.name) dispatch with
                | Some rows -> StackPanel.create [ StackPanel.children rows ] :> IView
                | None ->
                    let chosen = chosenName = Some setup.name
                    let availability =
                        match model.pageMode with
                        | BrowsingPage -> ControlOffered
                        | EditingPageEntry
                        | CreatingPageEntry -> ControlWithheld
                    let menu = model.entryMenuOf setup.name.caption (Some (RenameSetupTarget setup.name)) RenameAvailable dispatch
                    let choose () = dispatch (if chosen then DeselectSetup else ActivateSetup setup.name)
                    PageControls.entryToggle
                        (ConstructorUiId.savedSetup setup.name)
                        setup.name.caption
                        (if chosen then ChosenChoice else UnchosenChoice)
                        NeutralCommand
                        availability
                        menu
                        choose
                        []
            let under (setup : Setups.Setup) : IView list =
                match model.activeSetupOpt with
                | Some active when active.name = setup.name ->
                    underEntry (ConstructorUiKey $"ChosenSetup_{setup.name.value}_{model.pageMode.uiKey.value}") (chosenSetupRows model active dispatch)
                | Some _
                | None -> []
            let setups : IView list =
                match model.kitSetups with
                | [] -> [ (ControlCaption "This kit has no setup yet: New setup makes one from a light source and a detector.").pageLine ]
                | held ->
                    [
                        StackPanel.create [
                            yield! ConstructorUiId.savedSetups.groupAttributes<StackPanel> ()
                            AttrBuilder<StackPanel>.CreateProperty<string>(AutomationProperties.NameProperty, "Saved setups", ValueNone)
                            StackPanel.orientation Orientation.Vertical
                            StackPanel.margin (Thickness(0.0, 4.0, 0.0, 4.0))
                            StackPanel.children (entryRows held row under)
                        ]
                        :> IView
                    ]
            newVerb @ setups
    // The table's wheel scrolls this page until the table is in view (`pageScrollOf` finds the page by this id).
    ScrollViewer.create [
        ConstructorUiId.tableElementScrollViewer.automationAttribute<ScrollViewer> ()
        AttrBuilder<ScrollViewer>.CreateProperty<bool>(ScrollViewer.AllowAutoHideProperty, false, ValueNone)
        ScrollViewer.horizontalScrollBarVisibility ScrollBarVisibility.Auto
        ScrollViewer.verticalScrollBarVisibility ScrollBarVisibility.Auto
        ScrollViewer.content (
            MinWidthPanel.create [
                (PanelMinWidth 520.0).minWidthPanelAttribute ()
                MinWidthPanel.child (
                    StackPanel.create [
                        yield! ConstructorUiId.setupsPagePanel.groupAttributes<StackPanel> ()
                        StackPanel.orientation Orientation.Vertical
                        StackPanel.margin (Thickness 8.0)
                        StackPanel.children (model.pageHeader dispatch @ body)
                    ])
            ])
    ]
    :> IView
