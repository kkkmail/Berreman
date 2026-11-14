/// The study's Unknowns step: owners, states, bounds, priors, scopes and links, with the shared parameter editor
/// (001 §O; spec 0050 S11.5).
module OpticalConstructor.Ui.StudyUnknownsView

open Avalonia
open Avalonia.Controls
open Avalonia.Controls.Primitives
open Avalonia.Layout
open Avalonia.Media
open Avalonia.FuncUI.Builder
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open OpticalConstructor.Controls
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.Studies
open OpticalConstructor.Ui.TableAndElementRotationView

/// The study selection and last command status shared by every study surface (spec 0050 S11.5).
let studyBarView (model : TableAndElementRotationModel) (dispatch : TableAndElementRotationMsg -> unit) : IView =
    let choices : IView list =
        match model.kitStudies with
        | [] -> [ TextBlock.create [ TextBlock.text "This kit holds no study yet." ] ]
        | studies ->
            studies
            |> List.map (fun study ->
                let id = ConstructorUiId.studyChoice study.name
                let caption = study.name.caption
                let select () = dispatch (SelectStudy study.name)
                if model.studySurface.studyOpt = Some study.name then
                    MaterialsWindowView.actionButton id caption MaterialsWindowView.positiveBackground select
                else
                    MaterialsWindowView.verbButton id caption select)
    let status : IView list =
        match model.kitStatus with
        | None -> []
        | Some caption ->
            [
                TextBlock.create [
                    ConstructorUiId.studyStatusLine.automationAttribute<TextBlock> ()
                    TextBlock.textWrapping TextWrapping.Wrap
                    TextBlock.text caption.value
                ]
            ]
    StackPanel.create [
        StackPanel.children (
            [
                TextBlock.create [ TextBlock.text "Study:"; TextBlock.fontWeight FontWeight.SemiBold ]
                WrapPanel.create [
                    ConstructorUiId.studyList.automationAttribute<WrapPanel> ()
                    WrapPanel.children choices
                ]
            ] @ status)
    ] :> IView

let private newStudyFormRows (model : TableAndElementRotationModel) (dispatch : TableAndElementRotationMsg -> unit) : IView list =
    match model.kitWrites with
    | ControlWithheld -> []
    | ControlOffered ->
        let chosen = model.studySurface.newStudyExperiments
        let create : IView list =
            match StudyName.tryCreate model.studySurface.newStudyName.value with
            | Error _ -> []
            | Ok name ->
                let chosenKey = chosen |> List.map _.fileStem.value |> String.concat ","
                [
                    Border.create [
                        Border.horizontalAlignment HorizontalAlignment.Left
                        Border.child (
                            MaterialsWindowView.actionButton ConstructorUiId.createStudyButton (ControlCaption "Create study")
                                MaterialsWindowView.positiveBackground (fun () -> dispatch (StudyCommand (CreateStudy (name, chosen)))))
                    ]
                    // Spec 0050 S11.5: actionButton retains its handler per key; every draft needs its own closure.
                    |> View.withKey $"CreateStudy_{name.value}_{chosenKey}"
                    :> IView
                ]
        [
            DockPanel.create [
                DockPanel.margin (Thickness(0.0, 8.0, 0.0, 4.0))
                DockPanel.children [
                    TextBlock.create [
                        TextBlock.dock Dock.Left
                        TextBlock.verticalAlignment VerticalAlignment.Center
                        TextBlock.margin (Thickness(0.0, 0.0, 6.0, 0.0))
                        TextBlock.text "New study:"
                    ]
                    TextBox.create [
                        ConstructorUiId.newStudyNameField.automationAttribute<TextBox> ()
                        TextBox.width 220.0
                        TextBox.horizontalAlignment HorizontalAlignment.Left
                        TextBox.text model.studySurface.newStudyName.value
                        TextBox.onTextChanged (Option.ofObj >> Option.map ControlCaption >> Option.iter (SetNewStudyName >> dispatch))
                    ]
                ]
            ]
            WrapPanel.create [
                WrapPanel.children (
                    model.experimentCollection.experiments
                    |> List.map (fun experiment ->
                        let id = ConstructorUiId.newStudyExperimentToggle experiment.name
                        let caption = experiment.name.caption
                        let toggle () = dispatch (ToggleNewStudyExperiment experiment.name)
                        if List.contains experiment.name chosen then
                            MaterialsWindowView.actionButton id caption MaterialsWindowView.positiveBackground toggle
                        else
                            MaterialsWindowView.verbButton id caption toggle))
            ]
        ] @ create

let private studyOfferRows
    (model : TableAndElementRotationModel)
    (study : Study)
    (used : Item list)
    (dispatch : TableAndElementRotationMsg -> unit)
    : IView list =
    match model.kitWrites with
    | ControlWithheld -> []
    | ControlOffered ->
        let experiments =
            model.experimentCollection.experiments
            |> List.filter (fun experiment -> not (List.contains experiment.name study.experiments))
            |> List.map (fun experiment ->
                MaterialsWindowView.verbButton (ConstructorUiId.addExperimentToStudyButton experiment.name)
                    (ControlCaption $"Add {experiment.name.value}")
                    (fun () -> dispatch (StudyCommand (AddExperimentsToStudy (study.name, [ experiment.name ])))))
        let materials =
            used
            |> List.collect (fun item ->
                match model.tryItemModelChoice item with
                | Some (StackSampleChoice (_, _, sample)) -> sample.structure.referencedMaterialVersions |> Set.toList
                | _ -> [])
            |> List.distinct
            |> List.filter (fun version -> not (study.materials |> List.exists (fun opened -> opened.version = version)))
            |> List.map (fun version ->
                let caption =
                    match model.materials.resolveVersion version with
                    | Ok (Some entry) -> $"{entry.name.value} v{version.version.value}"
                    | Ok None | Error _ -> $"{version.materialId.value} v{version.version.value}"
                MaterialsWindowView.verbButton (ConstructorUiId.openMaterialButton version) (ControlCaption $"Open {caption}")
                    (fun () -> dispatch (StudyCommand (OpenMaterial (study.name, version)))))
        [
            WrapPanel.create [ WrapPanel.children (experiments @ materials) ]
            |> View.withKey $"StudyOffers_{study.name.fileStem.value}"
            :> IView
        ]

let private studyParameterGrid
    (model : TableAndElementRotationModel)
    (study : Study)
    (rows : (StudyParameterRef * Parameter * ControlCaption) list)
    (dispatch : TableAndElementRotationMsg -> unit)
    : IView =
    let cell (row : int) (column : int) (weight : FontWeight) (caption : ControlCaption) : IView =
        TextBlock.create [
            TextBlock.row row
            TextBlock.column column
            TextBlock.fontWeight weight
            TextBlock.verticalAlignment VerticalAlignment.Center
            TextBlock.margin (Thickness(0.0, 0.0, 12.0, 4.0))
            TextBlock.text caption.value
        ] :> IView
    let unknowns =
        rows |> List.choose (fun (reference, parameter, _) ->
            match parameter.state with
            | UnknownParameter _ -> Some reference
            | KnownParameter _ | CalibratedParameter _ -> None)
    let rowView (index : int) (entry : StudyParameterRef * Parameter * ControlCaption) : IView list =
        let reference, parameter, owner = entry
        let row = index + 1
        let cells = parameter.gridCells
        let scope : IView =
            match parameter.state with
            | KnownParameter _ | CalibratedParameter _ -> cell row 6 FontWeight.Normal (ControlCaption "—")
            | UnknownParameter _ ->
                ComboBox.create [
                    (ConstructorUiId.studyParameterScopeChoice reference).automationAttribute<ComboBox> ()
                    ComboBox.row row
                    ComboBox.column 6
                    ComboBox.margin (Thickness(0.0, 0.0, 12.0, 4.0))
                    ComboBox.isEnabled model.kitWrites.isEnabled
                    ComboBox.dataItems (ParameterScope.all |> List.map _.text)
                    ComboBox.selectedIndex (ParameterScope.all |> List.findIndex ((=) (study.scopeOf reference)))
                    ComboBox.onSelectedIndexChanged ((fun index ->
                        match ParameterScope.all |> List.tryItem index with
                        | Some scope when scope <> study.scopeOf reference ->
                            let scoped : ScopedParameter =
                                {
                                    parameter = reference
                                    scope = scope
                                }
                            dispatch (StudyCommand (SetParameterScope (study.name, scoped)))
                        | _ -> ()), SubPatchOptions.Always)
                ] :> IView
        let link : IView =
            match study.links |> List.tryFind (fun link -> link.follower = reference) with
            | Some link -> cell row 7 FontWeight.Normal (ControlCaption $"= {(model.studyParameterCaption link.leader).value}")
            | None ->
                match parameter.state with
                | KnownParameter _ | CalibratedParameter _ -> cell row 7 FontWeight.Normal (ControlCaption "—")
                | UnknownParameter _ ->
                    let leaders = unknowns |> List.filter ((<>) reference)
                    ComboBox.create [
                        (ConstructorUiId.studyParameterLinkChoice reference).automationAttribute<ComboBox> ()
                        ComboBox.row row
                        ComboBox.column 7
                        ComboBox.margin (Thickness(0.0, 0.0, 12.0, 4.0))
                        ComboBox.isEnabled model.kitWrites.isEnabled
                        ComboBox.dataItems ("(no link)" :: (leaders |> List.map (fun leader -> (model.studyParameterCaption leader).value)))
                        ComboBox.selectedIndex 0
                        ComboBox.onSelectedIndexChanged ((fun index ->
                            if index > 0 then
                                leaders |> List.tryItem (index - 1) |> Option.iter (fun leader ->
                                    let link : ParameterLink =
                                        {
                                            follower = reference
                                            leader = leader
                                        }
                                    dispatch (StudyCommand (LinkParameters (study.name, link))))),
                            SubPatchOptions.Always)
                    ] :> IView
        [
            cell row 0 FontWeight.Normal owner
            Border.create [
                Border.row row
                Border.column 1
                Border.child (
                    MaterialsWindowView.verbButton (ConstructorUiId.studyParameterRow reference) parameter.name.caption
                        (fun () -> dispatch (EditStudyParameter reference)))
            ]
            cell row 2 FontWeight.Normal cells.state
            cell row 3 FontWeight.Normal cells.value
            cell row 4 FontWeight.Normal cells.bounds
            cell row 5 FontWeight.Normal cells.sigma
            scope
            link
        ]
    let headers =
        [
            ControlCaption "Owner"
            ControlCaption "Parameter"
            ControlCaption "State"
            ControlCaption "Value"
            ControlCaption "Bounds"
            ControlCaption "σ"
            ControlCaption "Scope"
            ControlCaption "Link"
        ]
        |> List.mapi (fun column caption -> cell 0 column FontWeight.SemiBold caption)
    Grid.create [
        ConstructorUiId.studyUnknownsGrid.automationAttribute<Grid> ()
        Grid.margin (Thickness(0.0, 6.0, 0.0, 4.0))
        Grid.columnDefinitions "Auto,Auto,Auto,Auto,Auto,Auto,Auto,Auto"
        Grid.rowDefinitions (List.replicate (rows.Length + 1) "Auto" |> String.concat ",")
        Grid.children (headers @ (rows |> List.mapi rowView |> List.concat))
    ]
    |> View.withKey $"StudyGrid_{study.name.fileStem.value}"
    :> IView

/// The selected study's ordered parameter grid and editor, with writes offered only by writable kits (spec 0050 S11.5).
let studyUnknownsView (model : TableAndElementRotationModel) (dispatch : TableAndElementRotationMsg -> unit) : IView =
    let selected : IView list =
        match model.selectedStudyOpt with
        | None -> [ TextBlock.create [ TextBlock.text "No study is chosen: create one above." ] ]
        | Some study ->
            let used = model.studyContext.itemsUsedBy study
            let rows =
                (used |> List.collect (fun item ->
                    item.parameters |> List.map (fun parameter -> ItemParameterRef (item.id, parameter.name), parameter, item.label.caption)))
                @ (study.materials |> List.collect (fun material ->
                    let owner = ControlCaption $"{material.name.value} v{material.version.version.value}"
                    material.parameters |> List.map (fun parameter -> MaterialParameterRef (material.version, parameter.name), parameter, owner)))
            let editor : IView list =
                match model.parameterEditOpt with
                | Some draft when rows |> List.exists (fun (reference, _, _) -> reference = draft.parameter) ->
                    let setWith (state : ParameterState) : TableAndElementRotationMsg =
                        match draft.parameter with
                        | ItemParameterRef (id, name) -> SetParameterState (id, name, state)
                        | MaterialParameterRef (version, name) ->
                            let parameter : Parameter =
                                {
                                    name = name
                                    state = state
                                }
                            StudyCommand (SetOpenedMaterialParameter (study.name, version, parameter))
                    [
                        // Spec 0050 S11.5: equal parameter names on different owners must not retain another owner's Set handler.
                        Border.create [ Border.child (ItemsPanelView.parameterEditorView model draft setWith dispatch) ]
                        |> View.withKey $"StudyEditor_{study.name.fileStem.value}_{(ConstructorUiId.studyParameterRow draft.parameter).value}"
                        :> IView
                    ]
                | _ -> []
            let experimentNames = study.experiments |> List.map _.value |> String.concat ", "
            [
                TextBlock.create [
                    ConstructorUiId.studyExperimentsText.automationAttribute<TextBlock> ()
                    TextBlock.textWrapping TextWrapping.Wrap
                    TextBlock.text $"Experiments: {experimentNames}"
                ]
            ]
            @ studyOfferRows model study used dispatch
            @ [ studyParameterGrid model study rows dispatch ]
            @ editor
    ScrollViewer.create [
        // Spec 0050 S11.5: reserve scrollbar space so a row brought to the viewport edge remains clickable.
        AttrBuilder<ScrollViewer>.CreateProperty<bool>(ScrollViewer.AllowAutoHideProperty, false, ValueNone)
        ScrollViewer.horizontalScrollBarVisibility ScrollBarVisibility.Auto
        ScrollViewer.verticalScrollBarVisibility ScrollBarVisibility.Auto
        ScrollViewer.content (
            StackPanel.create [
                ConstructorUiId.studyUnknownsPanel.automationAttribute<StackPanel> ()
                StackPanel.orientation Orientation.Vertical
                StackPanel.margin 8.0
                StackPanel.children ([ studyBarView model dispatch ] @ newStudyFormRows model dispatch @ selected)
            ])
    ] :> IView
