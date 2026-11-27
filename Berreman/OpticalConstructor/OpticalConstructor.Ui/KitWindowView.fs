/// The kit window: the kit bar above the workbench, showing the kit, its Kit menu, the constructor and its switch, the
/// reload offer and the kit status, every control under its `ConstructorUiId` (spec 0050 S5.3).
module OpticalConstructor.Ui.KitWindowView

open Avalonia
open Avalonia.Controls
open Avalonia.Controls.Primitives
open Avalonia.Layout
open Avalonia.Media
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open OpticalConstructor.Controls
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.KitArchive
open OpticalConstructor.Ui.KitArchiveJob
open OpticalConstructor.Ui.KitOpening
open OpticalConstructor.Ui.TableAndElementRotationView

type TableAndElementRotationModel with

    /// `Kit:` and the kit's caption, which wraps: it is the dock's fill, never a horizontal stack's child
    /// (spec 0050 S5.3; AGENTS.md).
    member internal model.kitCaptionRow : IView =
        DockPanel.create [
            DockPanel.children [
                TextBlock.create [
                    TextBlock.dock Dock.Left
                    TextBlock.fontWeight FontWeight.SemiBold
                    TextBlock.margin (Thickness(0.0, 0.0, 6.0, 0.0))
                    TextBlock.text "Kit:"
                ]
                TextBlock.create [
                    ConstructorUiId.kitCaptionText.automationAttribute<TextBlock> ()
                    TextBlock.textWrapping TextWrapping.Wrap
                    TextBlock.text model.kitCaption.value
                ]
            ]
        ] :> IView

    /// The last kit read or write that needs telling: a done step in the text colour, a refusal in firebrick; the one
    /// control carrying `kitStatus`, whichever bay is open (spec 0050 S5.3; 0051 `.manual/003` 5.1).
    member internal model.kitStatusRows : IView list =
        match model.kitStatus with
        | Some status ->
            let colour =
                match status with
                | KitStatusDone _ -> Color.FromRgb(40uy, 40uy, 40uy)
                | KitStatusRefused _ -> Color.FromRgb(178uy, 34uy, 34uy)
            [
                TextBlock.create [
                    ConstructorUiId.kitStatus.automationAttribute<TextBlock> ()
                    TextBlock.foreground (MaterialsWindowView.brush colour)
                    TextBlock.textWrapping TextWrapping.Wrap
                    TextBlock.margin (Thickness(0.0, 2.0, 0.0, 2.0))
                    TextBlock.text status.caption.value
                ]
                |> Avalonia.FuncUI.DSL.View.withKey ConstructorUiId.kitStatus.value
                :> IView
            ]
        | None -> []

/// The Kit menu as the bar's verb row, then the constructor and its switch (spec 0050 S5.3; 001 §O). A row, not an
/// Avalonia `Menu`: a popup cannot be proven headless, as the model-driven element menu shows.
let private kitMenuRow (model : TableAndElementRotationModel) (dispatch : TableAndElementRotationMsg -> unit) : IView =
    // Spec 0050 S6: the save dialog suggests `<kit name>.zip`, and a cancelled dialog sends nothing. Hidden while a
    // send runs, one per window.
    let sendKit : IView list =
        match model.kitFolderOpt with
        | Some _ when not model.kitSend.job.isRunning ->
            [
                CommandControls.verbCommand ConstructorUiId.sendKitButton (ControlCaption "Send kit…") (fun () ->
                    model.kitShell.zipPicker.pickZipToWriteThen model.kit.kitFile.name.zipFileName (SendKit >> dispatch))
            ]
        | Some _
        | None -> []
    let showInExplorer : IView list =
        match model.kitFolderOpt with
        | Some _ ->
            [ CommandControls.verbCommand ConstructorUiId.kitShowInExplorerButton (ControlCaption "Show in Explorer") (fun () -> dispatch ShowKitInExplorer) ]
        | None -> []
    // Spec 0050 S6: offered while the kit takes writes and no source is open; a cancelled dialog sends nothing.
    let copyExperiments : IView list =
        match model.kitWrites, model.kitCopyOpt with
        | ControlOffered, None ->
            [
                CommandControls.verbCommand ConstructorUiId.kitCopyExperimentsButton (ControlCaption "Copy experiments from another kit…") (fun () ->
                    model.kitShell.kitPicker.pickThen OpticalConstructor.Domain.KitFolders.ChooseKitFolder (OpenCopySource >> dispatch))
            ]
        | ControlOffered, Some _
        | ControlWithheld, _ -> []
    let modeText (caption : ControlCaption) : IView =
        TextBlock.create [
            ConstructorUiId.kitModeText.automationAttribute<TextBlock> ()
            TextBlock.verticalAlignment VerticalAlignment.Center
            TextBlock.margin (Thickness(8.0, 0.0, 8.0, 4.0))
            TextBlock.text caption.value
        ]
        |> Avalonia.FuncUI.DSL.View.withKey ConstructorUiId.kitModeText.value
        :> IView
    let switchButton (label : ControlCaption) (target : ConstructorMode) : IView =
        CommandControls.verbCommand ConstructorUiId.kitSwitchModeButton label (fun () -> dispatch (SwitchConstructorMode target))
    let kitVerbs : IView list =
        [
            CommandControls.verbCommand ConstructorUiId.kitReloadChangesButton (ControlCaption "Reload changes") (fun () -> dispatch (CheckKitChanges KitCheckOnRequest))
            CommandControls.verbCommand ConstructorUiId.kitCloseButton (ControlCaption "Close kit") (fun () -> dispatch CloseKit)
        ]
    let constructorItems : IView list =
        match model.constructorMode with
        | ForwardConstructor -> [ modeText (ControlCaption "Main constructor"); switchButton (ControlCaption "Switch to Inverse") InverseConstructor ]
        | InverseConstructor -> [ modeText (ControlCaption "Inverse constructor"); switchButton (ControlCaption "Switch to Main") ForwardConstructor ]
    WrapPanel.create [
        WrapPanel.orientation Orientation.Horizontal
        WrapPanel.children (sendKit @ showInExplorer @ copyExperiments @ kitVerbs @ constructorItems)
    ] :> IView

/// The kit's naming template in a field that fills the row, Save template right of it while the kit takes writes
/// (spec 0050 S8.3; 001 §J). A dock, never a horizontal stack, so the field is bounded by the bar's width (AGENTS.md).
let private namingTemplateRow (model : TableAndElementRotationModel) (dispatch : TableAndElementRotationMsg -> unit) : IView =
    let saveTemplate : IView list =
        match model.kitWrites with
        | ControlOffered ->
            [
                Border.create [
                    Border.dock Dock.Right
                    Border.child (
                        CommandControls.verbCommand ConstructorUiId.saveNamingTemplateButton (ControlCaption "Save template") (fun () ->
                            dispatch SaveNamingTemplate))
                ]
            ]
        | ControlWithheld -> []
    DockPanel.create [
        DockPanel.margin (Thickness(0.0, 0.0, 0.0, 4.0))
        DockPanel.children (
            [
                TextBlock.create [
                    TextBlock.dock Dock.Left
                    TextBlock.fontWeight FontWeight.SemiBold
                    TextBlock.verticalAlignment VerticalAlignment.Center
                    TextBlock.margin (Thickness(0.0, 0.0, 6.0, 0.0))
                    TextBlock.text "Naming template:"
                ]
                :> IView
            ]
            @ saveTemplate
            @ [
                TextBox.create [
                    yield! ConstructorUiId.namingTemplateField.inputAttributes<TextBox> ()
                    TextBox.verticalAlignment VerticalAlignment.Center
                    TextBox.margin (Thickness(0.0, 0.0, 6.0, 0.0))
                    TextBox.text model.namingTemplateText.value
                    TextBox.onTextChanged (Option.ofObj >> Option.map ControlCaption >> Option.iter (SetNamingTemplateText >> dispatch))
                ]
                :> IView
            ])
    ] :> IView

/// The running send's progress row directly under the Kit menu, its Cancel ending the send before its next file
/// (spec 0050 S6; .spec-md §10.2). Nothing while no send runs.
let private kitSendRows (model : TableAndElementRotationModel) (dispatch : TableAndElementRotationMsg -> unit) : IView list =
    if model.kitSend.job.isRunning then
        let ids : KitArchiveProgressIds =
            {
                bar = ConstructorUiId.sendKitProgressBar
                text = ConstructorUiId.sendKitProgressText
                cancel = ConstructorUiId.sendKitCancelButton
            }
        [ kitArchiveProgressView model.kitSend ids (fun () -> dispatch CancelSendKit) ]
    else
        []

/// The mapping table, one block per used item: its label and model, then Add and Use for each kit item, the choice
/// filled; a heading and a 160-px scroller of its own beside the experiments, so neither list hides the other and the
/// bar grows no taller (spec 0050 S7.2; 0051 `.manual/003` 4.4).
let private kitCopyItemRows
    (model : TableAndElementRotationModel)
    (copySource : KitCopySource)
    (dispatch : TableAndElementRotationMsg -> unit)
    : IView list =
    let block (row : ItemCopyRow) : IView =
        let source = row.sourceItem
        let line = $"{source.label.value} — {(copySource.modelCaptionOf source).value}"
        let choiceBox (choice : ItemCopyChoice) (caption : ControlCaption) : IView =
            let id = ConstructorUiId.kitCopyItemChoice source.id choice
            let choose () = dispatch (ChooseCopiedItem (source.id, choice))
            CommandControls.choiceCommand id caption (if choice = row.choice then ChosenChoice else UnchosenChoice) choose
        let choices =
            choiceBox AddCopiedItem (ControlCaption "Add")
            :: (model.kitItems |> List.map (fun item -> choiceBox (MapOntoKitItem item.id) (ControlCaption $"Use {item.label.value}")))
        StackPanel.create [
            StackPanel.orientation Orientation.Vertical
            StackPanel.margin (Thickness(0.0, 2.0, 0.0, 2.0))
            StackPanel.children [
                TextBlock.create [
                    TextBlock.textWrapping TextWrapping.Wrap
                    TextBlock.text line
                ]
                WrapPanel.create [
                    WrapPanel.orientation Orientation.Horizontal
                    WrapPanel.children choices
                ]
            ]
        ] :> IView
    match copySource.itemRows with
    | [] -> []
    | rows ->
        [
            ScrollViewer.create [
                ScrollViewer.verticalScrollBarVisibility ScrollBarVisibility.Auto
                ScrollViewer.maxHeight 160.0
                ScrollViewer.content (
                    StackPanel.create [
                        StackPanel.orientation Orientation.Vertical
                        StackPanel.children [
                            TextBlock.create [
                                TextBlock.textWrapping TextWrapping.Wrap
                                TextBlock.fontWeight FontWeight.SemiBold
                                TextBlock.margin (Thickness(0.0, 0.0, 0.0, 2.0))
                                TextBlock.text "Items the chosen experiments use: add each to this kit, or use an item it already has."
                            ]
                            StackPanel.create [
                                yield! ConstructorUiId.kitCopyItemList.groupAttributes<StackPanel> ()
                                StackPanel.orientation Orientation.Vertical
                                StackPanel.children (rows |> List.map block)
                            ]
                        ]
                    ])
            ]
            |> Avalonia.FuncUI.DSL.View.withKey ConstructorUiId.kitCopyItemList.value
            :> IView
        ]

/// The open copy source: its name, a toggle per experiment in a list that scrolls inside its own height, because the
/// bar sits outside the page's scroll, then Copy and Cancel on one row, the Copy box's right margin parting the pair
/// (spec 0050 S6; .spec-md §10.5). Nothing while no source is open.
let private kitCopyRows (model : TableAndElementRotationModel) (dispatch : TableAndElementRotationMsg -> unit) : IView list =
    match model.kitCopyOpt with
    | Some copySource ->
        let toggle (experiment : OpticalConstructor.Domain.Experiments.Experiment) : IView =
            let id = ConstructorUiId.kitCopyExperimentToggle experiment.id
            CheckBox.create [
                id.automationAttribute<CheckBox> ()
                CheckBox.isChecked (List.contains experiment.id copySource.chosen)
                CheckBox.content (
                    TextBlock.create [
                        TextBlock.textWrapping TextWrapping.Wrap
                        TextBlock.text $"E{experiment.id.value} — {(copySource.describe experiment).value}"
                    ])
                // `onClick` fires on the user's press alone, never on the render's `IsChecked` (the `pivotCheckBox` precedent).
                CheckBox.onClick ((fun _ -> dispatch (ToggleCopiedExperiment experiment.id)), SubPatchOptions.OnChangeOf (box id.value))
            ]
            |> Avalonia.FuncUI.DSL.View.withKey id.value
            :> IView
        let copy : IView list =
            match copySource.chosen with
            | [] -> []
            | _ :: _ ->
                [
                    CommandControls.actionCommand
                        ConstructorUiId.kitCopyChosenButton
                        (ControlCaption "Copy")
                        PositiveCommand
                        (fun () -> dispatch CopyChosenExperiments)
                ]
        let cancel : IView =
            CommandControls.actionCommand
                ConstructorUiId.kitCopyCancelButton
                (ControlCaption "Cancel")
                NegativeCommand
                (fun () -> dispatch CloseCopySource)
        [
            StackPanel.create [
                StackPanel.orientation Orientation.Vertical
                StackPanel.children (
                [
                    TextBlock.create [
                        ConstructorUiId.kitCopySourceText.automationAttribute<TextBlock> ()
                        TextBlock.textWrapping TextWrapping.Wrap
                        TextBlock.margin (Thickness(0.0, 2.0, 0.0, 4.0))
                        TextBlock.text $"Copy from {copySource.source.kitFile.name.value}: choose the experiments to copy."
                    ]
                    // The experiments and the items they use side by side, each in its own 160-px scroller.
                    let experimentList : IView =
                        ScrollViewer.create [
                            ConstructorUiId.kitCopyExperimentList.automationAttribute<ScrollViewer> ()
                            Grid.column 0
                            ScrollViewer.verticalScrollBarVisibility ScrollBarVisibility.Auto
                            ScrollViewer.maxHeight 160.0
                            ScrollViewer.margin (Thickness(0.0, 0.0, 8.0, 4.0))
                            ScrollViewer.content (
                                StackPanel.create [
                                    StackPanel.orientation Orientation.Vertical
                                    StackPanel.children (copySource.experiments |> List.map toggle)
                                ])
                        ]
                    let items : IView list =
                        match kitCopyItemRows model copySource dispatch with
                        | [] -> []
                        | rows ->
                            [
                                StackPanel.create [
                                    Grid.column 1
                                    StackPanel.orientation Orientation.Vertical
                                    StackPanel.children rows
                                ]
                            ]
                    Grid.create [
                        Grid.columnDefinitions (match items with [] -> "*" | _ :: _ -> "*,*")
                        Grid.children (experimentList :: items)
                    ]
                ]
                @ [
                    StackPanel.create [
                        StackPanel.orientation Orientation.Horizontal
                        StackPanel.children (copy @ [ cancel ])
                    ]
                ])
            ]
            |> Avalonia.FuncUI.DSL.View.withKey ConstructorUiId.kitCopySourceText.value
            :> IView
        ]
    | None -> []

/// The reload offer: the files changed outside the app over Reload and Keep mine on one row, the Reload box's right
/// margin parting the opposing pair (spec 0050 S5.3; .spec-md §10.5). Nothing while nothing is offered.
let private kitChangesRows (model : TableAndElementRotationModel) (dispatch : TableAndElementRotationMsg -> unit) : IView list =
    match model.kitChanges with
    | [] -> []
    | files ->
        let names = files |> List.map (fun file -> file.value) |> String.concat ", "
        [
            StackPanel.create [
                StackPanel.orientation Orientation.Vertical
                StackPanel.children [
                    TextBlock.create [
                        ConstructorUiId.kitChangesText.automationAttribute<TextBlock> ()
                        TextBlock.textWrapping TextWrapping.Wrap
                        TextBlock.margin (Thickness(0.0, 2.0, 0.0, 4.0))
                        TextBlock.text $"Changed outside the app: {names}. Reload them, or keep your version?"
                    ]
                    StackPanel.create [
                        StackPanel.orientation Orientation.Horizontal
                        StackPanel.children [
                            CommandControls.actionCommand
                                ConstructorUiId.kitChangesReloadButton
                                (ControlCaption "Reload")
                                PositiveCommand
                                (fun () -> dispatch ReloadKitChanges)
                            CommandControls.actionCommand
                                ConstructorUiId.kitChangesKeepMineButton
                                (ControlCaption "Keep mine")
                                NegativeCommand
                                (fun () -> dispatch KeepMyKitChanges)
                        ]
                    ]
                ]
            ]
            |> Avalonia.FuncUI.DSL.View.withKey ConstructorUiId.kitChangesText.value
            :> IView
        ]

/// The kit window as a pure projection of the workbench model: the kit bar docked above, the flow strip left of the
/// step's surface, keyed so a change between surfaces recreates it and Setups ↔ Experiments keeps the workbench
/// (spec 0050 S5.3, S7.2, S9.3, S11.5, S14.2).
let kitWindowView (model : TableAndElementRotationModel) (dispatch : TableAndElementRotationMsg -> unit) : IView =
    // `View.withKey` keys a typed view, so the surface sits in a keyed `Border`, as `Ribbon.ribbonView`'s pane does.
    let surface : IView =
        match model.flowStep with
        | ItemsFlowStep ->
            Border.create [ Border.child (ItemsPanelView.itemsPanelView model dispatch) ]
            |> Avalonia.FuncUI.DSL.View.withKey "items"
            :> IView
        | SetupsFlowStep
        | ExperimentsFlowStep ->
            Border.create [ Border.child (mainView model dispatch) ]
            |> Avalonia.FuncUI.DSL.View.withKey "workbench"
            :> IView
        | DataFlowStep ->
            Border.create [ Border.child (DataStepView.dataStepView model dispatch) ]
            |> Avalonia.FuncUI.DSL.View.withKey "data"
            :> IView
        | UnknownsFlowStep ->
            Border.create [ Border.child (StudyUnknownsView.studyUnknownsView model dispatch) ]
            |> Avalonia.FuncUI.DSL.View.withKey "unknowns"
            :> IView
        | PlanFlowStep ->
            Border.create [ Border.child (PlanView.planView model dispatch) ]
            |> Avalonia.FuncUI.DSL.View.withKey "plan"
            :> IView
        | FitFlowStep ->
            Border.create [ Border.child (StudyFitView.studyFitView model dispatch) ]
            |> Avalonia.FuncUI.DSL.View.withKey "fit"
            :> IView
        | ResultsFlowStep ->
            Border.create [ Border.child (StudyResultsView.studyResultsView model dispatch) ]
            |> Avalonia.FuncUI.DSL.View.withKey "results"
            :> IView
        | PublishFlowStep ->
            Border.create [ Border.child (StudyPublishView.studyPublishView model dispatch) ]
            |> Avalonia.FuncUI.DSL.View.withKey "publish"
            :> IView
    DockPanel.create [
        DockPanel.children [
            StackPanel.create [
                StackPanel.dock Dock.Top
                yield! ConstructorUiId.kitBar.groupAttributes<StackPanel> ()
                StackPanel.orientation Orientation.Vertical
                StackPanel.margin (Thickness(8.0, 6.0, 8.0, 2.0))
                StackPanel.children (
                    [ model.kitCaptionRow; kitMenuRow model dispatch; namingTemplateRow model dispatch ]
                    @ kitSendRows model dispatch
                    @ kitCopyRows model dispatch
                    @ kitChangesRows model dispatch
                    @ model.kitStatusRows)
            ]
            // Scrolls: the bar above can leave a short window too little height for the strip (spec 0050 S7.2; AGENTS.md).
            ScrollViewer.create [
                ScrollViewer.dock Dock.Left
                ScrollViewer.verticalScrollBarVisibility ScrollBarVisibility.Auto
                ScrollViewer.content (FlowStripView.flowStripView model dispatch)
            ]
            surface
        ]
    ] :> IView
