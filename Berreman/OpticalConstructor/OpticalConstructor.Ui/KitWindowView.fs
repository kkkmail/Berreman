/// The kit window: the page row above the shown page, with the reload offer and a running send while there is one, and
/// the Kit page with the kit's own operations and Publish (spec 0050 S5.3; 0051 `.manual/007` Phase 2).
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
open OpticalConstructor.Ui.KitPageFrame

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
                    SelectableTextBlock.create [
                        ConstructorUiId.kitCopySourceText.automationAttribute<SelectableTextBlock> ()
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
                    SelectableTextBlock.create [
                        ConstructorUiId.kitChangesText.automationAttribute<SelectableTextBlock> ()
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

/// The page row: Items, Setups and Experiments, the switch to the other constructor, the Inverse pages in Inverse, then
/// Kit; the shown page is chosen, and the row wraps at the window's width (spec 0050 S7.2; 0051 `.manual/007` 2.1, D1).
let private pageRowView (model : TableAndElementRotationModel) (dispatch : TableAndElementRotationMsg -> unit) : IView =
    let stepBox (step : ConstructorFlowStep) : IView =
        CommandControls.choiceCommand step.flowUiId step.caption (if step = model.flowStep then ChosenChoice else UnchosenChoice) (fun () ->
            dispatch (ShowFlowStep step))
    let switch : IView =
        match model.constructorMode with
        | ForwardConstructor ->
            CommandControls.verbCommand ConstructorUiId.kitSwitchModeButton (ControlCaption "Inverse") (fun () -> dispatch (SwitchConstructorMode InverseConstructor))
        | InverseConstructor ->
            CommandControls.verbCommand ConstructorUiId.kitSwitchModeButton (ControlCaption "Main") (fun () -> dispatch (SwitchConstructorMode ForwardConstructor))
    let steps = ConstructorFlowStep.stepsOf model.constructorMode
    let first = steps |> List.filter (fun step -> List.contains step [ ItemsFlowStep; SetupsFlowStep; ExperimentsFlowStep ])
    let rest = steps |> List.filter (fun step -> not (List.contains step [ ItemsFlowStep; SetupsFlowStep; ExperimentsFlowStep ]))
    WrapPanel.create [
        yield! ConstructorUiId.pageRow.groupAttributes<WrapPanel> ()
        WrapPanel.orientation Orientation.Horizontal
        WrapPanel.margin (Thickness(8.0, 6.0, 8.0, 2.0))
        WrapPanel.children ((first |> List.map stepBox) @ [ switch ] @ (rest |> List.map stepBox))
    ]
    :> IView

/// A wrapping line of the Kit page.
let private kitLine (id : ConstructorUiId) (text : ControlCaption) : IView =
    PageControls.copyableText id text [ SelectableTextBlock.margin (Thickness(0.0, 1.0, 0.0, 1.0)) ]

/// The kit's verbs: Send kit…, Show in Explorer, Copy experiments from another kit…, Reload changes and Close kit
/// (spec 0050 S5.3, S6; 0051 `.manual/007` 2.2).
let private kitVerbsRow (model : TableAndElementRotationModel) (dispatch : TableAndElementRotationMsg -> unit) : IView =
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
                    model.kitShell.kitPicker.pickThen OpticalConstructor.Domain.KitFolders.ChooseKitFile (OpenCopySource >> dispatch))
            ]
        | ControlOffered, Some _
        | ControlWithheld, _ -> []
    WrapPanel.create [
        WrapPanel.orientation Orientation.Horizontal
        WrapPanel.children (
            sendKit
            @ showInExplorer
            @ copyExperiments
            @ [
                CommandControls.verbCommand ConstructorUiId.kitReloadChangesButton (ControlCaption "Reload changes") (fun () -> dispatch (CheckKitChanges KitCheckOnRequest))
                CommandControls.verbCommand ConstructorUiId.kitCloseButton (ControlCaption "Close kit") (fun () -> dispatch CloseKit)
                CommandControls.verbCommand ConstructorUiId.kitGuideButton (ControlCaption "Guide…") (fun () -> model.kitShell.openGuide ())
            ])
    ]
    :> IView

/// The naming template: read-only with Edit, or, in Editing, its field and Save and Cancel (spec 0050 S8.3; 0051
/// `.manual/007` 2.2).
let private namingTemplateRows (model : TableAndElementRotationModel) (dispatch : TableAndElementRotationMsg -> unit) : IView list =
    match model.pageMode with
    | EditingPageEntry ->
        [
            TextBox.create [
                yield! ConstructorUiId.namingTemplateField.inputAttributes<TextBox> ()
                TextBox.horizontalAlignment HorizontalAlignment.Stretch
                TextBox.margin (Thickness(0.0, 0.0, 0.0, 4.0))
                TextBox.text model.namingTemplateText.value
                TextBox.onTextChanged (Option.ofObj >> Option.map ControlCaption >> Option.iter (SetNamingTemplateText >> dispatch))
            ]
            :> IView
            model.draftActions (ControlCaption "Save") dispatch
        ]
    | BrowsingPage
    | CreatingPageEntry ->
        let edit : IView list =
            match model.kitWrites with
            | ControlOffered -> [ CommandControls.verbCommand ConstructorUiId.pageEditButton (ControlCaption "Edit") (fun () -> dispatch BeginPageEdit) ]
            | ControlWithheld -> []
        [
            WrapPanel.create [
                WrapPanel.orientation Orientation.Horizontal
                WrapPanel.children (
                    PageControls.copyableText ConstructorUiId.namingTemplateText model.kit.kitFile.namingTemplate.caption [
                        SelectableTextBlock.verticalAlignment VerticalAlignment.Center
                        SelectableTextBlock.margin (Thickness(0.0, 0.0, 10.0, 4.0))
                    ]
                    :: edit)
            ]
            :> IView
        ]

/// Publish: the kit's fit runs as dynamic buttons, the chosen one's calibration preview and promotions under it (spec
/// 0050 S11.5; 0051 `.manual/007` 2.2, 8.6).
let private publishRows (model : TableAndElementRotationModel) (dispatch : TableAndElementRotationMsg -> unit) : IView list =
    match model.kitFitRuns |> List.sortByDescending (fun run -> run.stamp) with
    | [] -> [ kitLine ConstructorUiId.publishNothingText (ControlCaption "No fit run yet: a study's fit runs are made on the Fit page, in Inverse.") ]
    | runs ->
        let chosen (run : OpticalConstructor.Domain.FitRuns.FitRun) : bool =
            model.studySurface.fitRunOpt = Some run.name && model.studySurface.studyOpt = Some run.study
        let row (run : OpticalConstructor.Domain.FitRuns.FitRun) : IView =
            let caption = ControlCaption $"{run.name.value} — study {run.study.value}, {run.stage.text}"
            let menu = model.entryMenuOf run.name.caption None RenameAvailable dispatch
            PageControls.entryToggle (ConstructorUiId.fitRunRow run.name) caption (if chosen run then ChosenChoice else UnchosenChoice) NeutralCommand ControlOffered menu (fun () ->
                dispatch (ChoosePublishRun run.name)) []
        let under (run : OpticalConstructor.Domain.FitRuns.FitRun) : IView list =
            match chosen run, model.kitStudies |> List.tryFind (fun study -> study.name = run.study) with
            | true, Some study -> underEntry (ConstructorUiKey $"PublishRun_{run.name.value}") (StudyPublishView.publishRunRows model study run dispatch)
            | true, None -> underEntry (ConstructorUiKey $"PublishRun_{run.name.value}") [ kitLine ConstructorUiId.publishNothingText (ControlCaption $"The kit holds no study {run.study.value}.") ]
            | false, _ -> []
        [
            StackPanel.create [
                yield! ConstructorUiId.studyPublishPanel.groupAttributes<StackPanel> ()
                StackPanel.orientation Orientation.Vertical
                StackPanel.children (entryRows runs row under)
            ]
            :> IView
        ]

/// The Kit page: the kit's name, folder and stamps, its verbs, the copy source while one is open, the naming template,
/// and Publish (spec 0050 S5.3, S6, S8.3, S11.5; 0051 `.manual/007` 2.2).
let private kitPageView (model : TableAndElementRotationModel) (dispatch : TableAndElementRotationMsg -> unit) : IView =
    let stamp (time : ClockTime) : string = time.value.ToLocalTime().ToString("yyyy-MM-dd HH:mm")
    let folder =
        match model.kitFolderOpt with
        | Some kitFolder -> ControlCaption $"Folder: {kitFolder.value.value}"
        | None -> ControlCaption "Folder: none (a scratch kit)"
    let content =
        model.pageHeader dispatch
        @ [
            kitLine ConstructorUiId.kitCaptionText model.kitCaption
            kitLine ConstructorUiId.kitFolderText folder
            kitLine ConstructorUiId.kitCreatedText (ControlCaption $"Created: {stamp model.kit.kitFile.created}")
            kitLine ConstructorUiId.kitModifiedText (ControlCaption $"Last saved: {stamp model.kit.kitFile.modified}")
            kitVerbsRow model dispatch
        ]
        @ kitSendRows model dispatch
        @ kitCopyRows model dispatch
        @ [ (ControlCaption "Naming template").pageHeading ]
        @ namingTemplateRows model dispatch
        @ [ (ControlCaption "Publish").pageHeading ]
        @ publishRows model dispatch
    pageScroll ConstructorUiId.kitPagePanel content

/// The kit window as a pure projection of the workbench model: the page row, the reload offer while files changed
/// outside the app and a running send's progress, then the shown page, keyed so a change of page recreates it (spec 0050
/// S5.3, S7.2, S9.3, S11.5, S14.2; 0051 `.manual/007` 2.1, 2.2).
let kitWindowView (model : TableAndElementRotationModel) (dispatch : TableAndElementRotationMsg -> unit) : IView =
    let keyed (key : string) (view : IView) : IView =
        Border.create [ Border.child view ] |> Avalonia.FuncUI.DSL.View.withKey key :> IView
    let surface : IView =
        match model.flowStep with
        | ItemsFlowStep -> keyed "items" (ItemsPanelView.itemsPanelView model dispatch)
        | SetupsFlowStep -> keyed "setups" (SetupsPageView.setupsPageView model dispatch)
        | ExperimentsFlowStep -> keyed "experiments" (ExperimentsPageView.experimentsPageView model dispatch)
        | DataFlowStep -> keyed "data" (DataStepView.dataStepView model dispatch)
        | UnknownsFlowStep -> keyed "unknowns" (StudyUnknownsView.studyUnknownsView model dispatch)
        | PlanFlowStep -> keyed "plan" (PlanView.planView model dispatch)
        | FitFlowStep -> keyed "fit" (StudyFitView.studyFitView model dispatch)
        | ResultsFlowStep -> keyed "results" (StudyResultsView.studyResultsView model dispatch)
        | KitFlowStep -> keyed "kit" (kitPageView model dispatch)
    // The reload offer and a running send concern whatever page is shown (spec 0051 `.manual/007` 1.7).
    let banners : IView list =
        match model.flowStep with
        | KitFlowStep -> kitChangesRows model dispatch
        | ItemsFlowStep
        | SetupsFlowStep
        | ExperimentsFlowStep
        | DataFlowStep
        | UnknownsFlowStep
        | PlanFlowStep
        | FitFlowStep
        | ResultsFlowStep -> kitChangesRows model dispatch @ kitSendRows model dispatch
    DockPanel.create [
        DockPanel.children [
            StackPanel.create [
                StackPanel.dock Dock.Top
                yield! ConstructorUiId.kitBar.groupAttributes<StackPanel> ()
                StackPanel.orientation Orientation.Vertical
                StackPanel.children (pageRowView model dispatch :: (banners |> List.map (fun banner -> Border.create [ Border.margin (Thickness(8.0, 0.0, 8.0, 2.0)); Border.child banner ] :> IView)))
            ]
            surface
        ]
    ]
    :> IView
