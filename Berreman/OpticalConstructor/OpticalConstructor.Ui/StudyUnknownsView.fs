/// The Unknowns page: the kit's studies as dynamic buttons, the chosen one's experiments, parameters and sessions under
/// its row, edited in place, each parameter's editor right under it; New study creates (001 §O; spec 0050 S11.5; 0051
/// `.manual/007` 8.2).
module OpticalConstructor.Ui.StudyUnknownsView

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
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.Studies
open OpticalConstructor.Ui.TableAndElementRotationView
open OpticalConstructor.Ui.KitPageFrame

/// A semi-bold heading of a block of the page.
let private studyHeading (text : ControlCaption) : IView =
    TextBlock.create [
        TextBlock.fontWeight FontWeight.SemiBold
        TextBlock.margin (Thickness(0.0, 6.0, 0.0, 2.0))
        TextBlock.text text.value
    ]
    :> IView

type TableAndElementRotationModel with

    /// The study as the page shows it: its working copy while it is edited, else as held (spec 0051 `.manual/007` 8.2).
    member model.shownStudy (study : Study) : Study =
        match model.studyEditOpt with
        | Some edit when edit.draft.name = study.name -> edit.draft
        | Some _
        | None -> study

    /// The kit's items with the edited study's working copies over them (spec 0051 `.manual/007` 8.2).
    member model.studyItems : Item list =
        match model.studyEditOpt with
        | Some edit -> model.kitItems |> List.map (fun item -> edit.items |> List.tryFind (fun held -> held.id = item.id) |> Option.defaultValue item)
        | None -> model.kitItems

    /// The study's parameters: those of the items its experiments use, then those of the materials it opens, each with
    /// its owner (spec 0050 S11.5).
    member model.studyParameterRows (study : Study) : (StudyParameterRef * Parameter * ControlCaption) list =
        let used =
            model.studyContext.itemsUsedBy study
            |> List.map (fun item -> model.studyItems |> List.tryFind (fun held -> held.id = item.id) |> Option.defaultValue item)
        (used |> List.collect (fun item ->
            item.parameters |> List.map (fun parameter -> ItemParameterRef (item.id, parameter.name), parameter, item.label.caption)))
        @ (study.materials |> List.collect (fun material ->
            let owner = ControlCaption $"{material.name.value} v{material.version.version.value}"
            material.parameters |> List.map (fun parameter -> MaterialParameterRef (material.version, parameter.name), parameter, owner)))

/// The open parameter's editor: its state, value, bounds and prior, then, for an unknown, its scope and its link, each
/// writing into the study's draft (spec 0050 S11.5; 0051 `.manual/007` 8.2).
let private parameterEditorRows
    (model : TableAndElementRotationModel)
    (study : Study)
    (draft : ParameterEditDraft)
    (unknowns : StudyParameterRef list)
    (dispatch : TableAndElementRotationMsg -> unit)
    : IView list =
    let setWith (state : ParameterState) : TableAndElementRotationMsg =
        match draft.parameter with
        | ItemParameterRef (id, name) -> SetParameterState (id, name, state)
        | MaterialParameterRef (version, name) ->
            let parameter : Parameter = { name = name; state = state }
            StudyCommand (SetOpenedMaterialParameter (study.name, version, parameter))
    let reference = draft.parameter
    let scopeAndLink : IView list =
        match List.contains reference unknowns with
        | false -> []
        | true ->
            let scope =
                ComboBox.create [
                    yield! (ConstructorUiId.studyParameterScopeChoice reference).inputAttributes<ComboBox> ()
                    ComboBox.margin (Thickness(0.0, 0.0, 12.0, 4.0))
                    ComboBox.dataItems (ParameterScope.all |> List.map _.text)
                    ComboBox.selectedIndex (ParameterScope.all |> List.findIndex ((=) (study.scopeOf reference)))
                    ComboBox.onSelectedIndexChanged ((fun index ->
                        match ParameterScope.all |> List.tryItem index with
                        | Some scope when scope <> study.scopeOf reference ->
                            let scoped : ScopedParameter = { parameter = reference; scope = scope }
                            dispatch (StudyCommand (SetParameterScope (study.name, scoped)))
                        | _ -> ()), SubPatchOptions.Always)
                ]
                :> IView
            let link : IView =
                match study.links |> List.tryFind (fun link -> link.follower = reference) with
                | Some link -> PageControls.copyableLine (ControlCaption $"Linked: = {(model.studyParameterCaption link.leader).value}") []
                | None ->
                    let leaders = unknowns |> List.filter ((<>) reference)
                    ComboBox.create [
                        yield! (ConstructorUiId.studyParameterLinkChoice reference).inputAttributes<ComboBox> ()
                        ComboBox.margin (Thickness(0.0, 0.0, 12.0, 4.0))
                        ComboBox.dataItems ("(no link)" :: (leaders |> List.map (fun leader -> (model.studyParameterCaption leader).value)))
                        ComboBox.selectedIndex 0
                        ComboBox.onSelectedIndexChanged ((fun index ->
                            if index > 0 then
                                leaders |> List.tryItem (index - 1) |> Option.iter (fun leader ->
                                    let link : ParameterLink = { follower = reference; leader = leader }
                                    dispatch (StudyCommand (LinkParameters (study.name, link))))),
                            SubPatchOptions.Always)
                    ]
                    :> IView
            [
                WrapPanel.create [
                    WrapPanel.orientation Orientation.Horizontal
                    WrapPanel.children [
                        TextBlock.create [ TextBlock.text "Scope:"; TextBlock.verticalAlignment VerticalAlignment.Center; TextBlock.margin (Thickness(0.0, 0.0, 6.0, 4.0)) ]
                        scope
                        TextBlock.create [ TextBlock.text "Link:"; TextBlock.verticalAlignment VerticalAlignment.Center; TextBlock.margin (Thickness(0.0, 0.0, 6.0, 4.0)) ]
                        link
                    ]
                ]
                :> IView
            ]
    [
        Border.create [ Border.child (ItemsPanelView.parameterEditorView model draft setWith dispatch) ]
        |> View.withKey $"StudyEditor_{study.name.fileStem.value}_{(ConstructorUiId.studyParameterRow draft.parameter).value}"
        :> IView
    ]
    @ scopeAndLink

/// The study's parameters as a grid: in Editing each name is a button that opens its editor in a row right under it,
/// and scope and link change only there (spec 0050 S11.5; 0051 `.manual/007` 8.2).
let private studyParameterGrid (model : TableAndElementRotationModel) (study : Study) (dispatch : TableAndElementRotationMsg -> unit) : IView =
    let editing =
        match model.pageMode with
        | EditingPageEntry -> true
        | BrowsingPage
        | CreatingPageEntry -> false
    let rows = model.studyParameterRows study
    let unknowns =
        rows |> List.choose (fun (reference, parameter, _) ->
            match parameter.state with
            | UnknownParameter _ -> Some reference
            | KnownParameter _ | CalibratedParameter _ -> None)
    let gridRows =
        rows
        |> List.collect (fun (reference, parameter, owner) ->
            let cells = parameter.gridCells
            let scope, link =
                match parameter.state with
                | KnownParameter _ | CalibratedParameter _ -> ControlCaption "—", ControlCaption "—"
                | UnknownParameter _ ->
                    let linkText =
                        match study.links |> List.tryFind (fun link -> link.follower = reference) with
                        | Some link -> ControlCaption $"= {(model.studyParameterCaption link.leader).value}"
                        | None -> ControlCaption "(no link)"
                    ControlCaption (study.scopeOf reference).text, linkText
            let nameCell =
                if editing then
                    ViewGridCell (
                        parameter.name.caption,
                        CommandControls.verbCommand (ConstructorUiId.studyParameterRow reference) parameter.name.caption (fun () -> dispatch (EditStudyParameter reference)))
                else TextGridCell parameter.name.caption
            let editor =
                match model.parameterEditOpt with
                | Some draft when editing && draft.parameter = reference -> [ SpanningGridRow (StackPanel.create [ StackPanel.children (parameterEditorRows model study draft unknowns dispatch) ]) ]
                | Some _
                | None -> []
            CellsGridRow [ TextGridCell owner; nameCell; TextGridCell cells.state; TextGridCell cells.value; TextGridCell cells.bounds; TextGridCell cells.sigma; TextGridCell scope; TextGridCell link ]
            :: editor)
    let table : GridTable =
        {
            id = ConstructorUiId.studyUnknownsGrid
            header =
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
            rows = gridRows
        }
    Border.create [ Border.child (PageControls.gridTableView table (fun text -> dispatch (CopyToClipboard text))) ]
    |> View.withKey $"StudyGrid_{study.name.fileStem.value}_{editing}"
    :> IView

/// The study's sessions and how each one's gain is known, the gain a choice in Editing (spec 0050 S11.5, S12.1).
let private studySessionRows (model : TableAndElementRotationModel) (study : Study) (dispatch : TableAndElementRotationMsg -> unit) : IView list =
    let sessions = study.sessionsIn model.experimentCollection.experiments
    match sessions with
    | [] -> [ studyHeading (ControlCaption "Sessions and gains:"); PageControls.copyableLine (ControlCaption "This study's experiments form no session.") [] ]
    | _ ->
        let gainCell (session : StudySession) : GridCell =
            let selected = study.gainModelOf session.name
            match model.pageMode with
            | EditingPageEntry ->
                let offers = session.gainModelOffers model.experimentCollection.experiments
                ViewGridCell (
                    selected.caption,
                    ComboBox.create [
                        (ConstructorUiId.studySessionGainChoice session.name).automationAttribute<ComboBox> ()
                        AttrBuilder<ComboBox>.CreateProperty<string>(AutomationProperties.NameProperty, $"Gain model for {session.name.value}", ValueNone)
                        ComboBox.dataItems (offers |> List.map _.caption.value)
                        ComboBox.selectedIndex (offers |> List.tryFindIndex ((=) selected) |> Option.defaultValue -1)
                        ComboBox.onSelectedIndexChanged ((fun selectedIndex ->
                            match offers |> List.tryItem selectedIndex with
                            | Some gainModel when gainModel <> selected ->
                                let setting : SessionGainSetting = { session = session.name; model = gainModel }
                                dispatch (StudyCommand (SetSessionGainModel (study.name, setting)))
                            | _ -> ()), SubPatchOptions.Always)
                    ])
            | BrowsingPage
            | CreatingPageEntry -> TextGridCell selected.caption
        let table : GridTable =
            {
                id = ConstructorUiId.studySessionGrid
                header = [ ControlCaption "Session"; ControlCaption "Experiments"; ControlCaption "Gain" ]
                rows =
                    sessions
                    |> List.map (fun session ->
                        let names = session.experiments |> List.map _.value |> String.concat ", " |> ControlCaption
                        CellsGridRow [ TextGridCell session.name.caption; TextGridCell names; gainCell session ])
            }
        [ studyHeading (ControlCaption "Sessions and gains:"); PageControls.gridTableView table (fun text -> dispatch (CopyToClipboard text)) ]

/// The edited study's offers: each kit experiment it lacks and each material its items use that it has not opened
/// (spec 0050 S11.5).
let private studyOfferRows (model : TableAndElementRotationModel) (study : Study) (dispatch : TableAndElementRotationMsg -> unit) : IView list =
    let used = model.studyContext.itemsUsedBy study
    let experiments =
        model.experimentCollection.experiments
        |> List.filter (fun experiment -> not (List.contains experiment.name study.experiments))
        |> List.map (fun experiment ->
            CommandControls.verbCommand (ConstructorUiId.addExperimentToStudyButton experiment.name)
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
            CommandControls.verbCommand (ConstructorUiId.openMaterialButton version) (ControlCaption $"Open {caption}")
                (fun () -> dispatch (StudyCommand (OpenMaterial (study.name, version)))))
    match experiments @ materials with
    | [] -> []
    | offers ->
        [
            studyHeading (ControlCaption "Add to the study:")
            WrapPanel.create [ WrapPanel.children offers ]
            |> View.withKey $"StudyOffers_{study.name.fileStem.value}"
            :> IView
        ]

/// What the page shows under the chosen study: Edit, its experiments, its parameters and sessions; in Editing, its
/// offers, its parameters with their editor, then Save and Cancel (spec 0051 `.manual/007` 8.2).
let private chosenStudyRows (model : TableAndElementRotationModel) (held : Study) (dispatch : TableAndElementRotationMsg -> unit) : IView list =
    let study = model.shownStudy held
    let experimentNames = study.experiments |> List.map _.value |> String.concat ", "
    let edit : IView list =
        match model.kitWrites, model.pageMode with
        | ControlOffered, BrowsingPage -> [ CommandControls.verbCommand ConstructorUiId.pageEditButton (ControlCaption "Edit") (fun () -> dispatch BeginPageEdit) ]
        | _ -> []
    let offers : IView list =
        match model.pageMode with
        | EditingPageEntry -> studyOfferRows model study dispatch
        | BrowsingPage
        | CreatingPageEntry -> []
    let actions : IView list =
        match model.pageMode with
        | EditingPageEntry -> [ model.draftActions (ControlCaption "Save") dispatch ]
        | BrowsingPage
        | CreatingPageEntry -> []
    edit
    @ [ PageControls.copyableText ConstructorUiId.studyExperimentsText (ControlCaption $"Experiments: {experimentNames}") [] ]
    @ offers
    @ [
        studyHeading (
            match model.pageMode with
            | EditingPageEntry -> ControlCaption "Parameters: press a name to edit it."
            | BrowsingPage
            | CreatingPageEntry -> ControlCaption "Parameters:")
        studyParameterGrid model study dispatch
    ]
    @ studySessionRows model study dispatch
    @ actions

/// The New study form: its name and the experiments to fit, each with its role and data badge, then Create and Cancel
/// (spec 0050 S11.5; 0051 `.manual/007` 8.2).
let private newStudyRows (model : TableAndElementRotationModel) (dispatch : TableAndElementRotationMsg -> unit) : IView list =
    let chosen = model.studySurface.newStudyExperiments
    [
        studyHeading (ControlCaption "New study")
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
                    yield! ConstructorUiId.newStudyNameField.inputAttributes<TextBox> ()
                    TextBox.width 260.0
                    TextBox.horizontalAlignment HorizontalAlignment.Left
                    TextBox.text model.studySurface.newStudyName.value
                    TextBox.onTextChanged (Option.ofObj >> Option.map ControlCaption >> Option.iter (SetNewStudyName >> dispatch))
                ]
            ]
        ]
        studyHeading (ControlCaption "Experiments to fit:")
        WrapPanel.create [
            WrapPanel.children (
                model.experimentCollection.experiments
                |> List.map (fun experiment ->
                    let id = ConstructorUiId.newStudyExperimentToggle experiment.name
                    let caption = ControlCaption $"{experiment.name.value} [{experiment.role.text}] — {(model.dataBadgeOf experiment).text}"
                    CommandControls.choiceCommand id caption (if List.contains experiment.name chosen then ChosenChoice else UnchosenChoice) (fun () ->
                        dispatch (ToggleNewStudyExperiment experiment.name))))
        ]
        model.draftActions (ControlCaption "Create") dispatch
    ]

/// The Unknowns page (spec 0050 S11.5; 0051 `.manual/007` 8.2).
let studyUnknownsView (model : TableAndElementRotationModel) (dispatch : TableAndElementRotationMsg -> unit) : IView =
    let body : IView list =
        match model.pageMode with
        | CreatingPageEntry -> newStudyRows model dispatch
        | BrowsingPage
        | EditingPageEntry ->
            let newVerb : IView list =
                match model.kitWrites, model.pageMode with
                | ControlOffered, BrowsingPage ->
                    [
                        Border.create [
                            Border.horizontalAlignment HorizontalAlignment.Left
                            Border.child (CommandControls.verbCommand ConstructorUiId.newStudyButton (ControlCaption "New study") (fun () -> dispatch BeginPageCreate))
                        ]
                        :> IView
                    ]
                | _ -> []
            let row (study : Study) : IView =
                match model.renameRowsOf (RenameStudyTarget study.name) dispatch with
                | Some rows -> StackPanel.create [ StackPanel.children rows ] :> IView
                | None ->
                    let chosen = model.studySurface.studyOpt = Some study.name
                    let availability =
                        match model.pageMode with
                        | BrowsingPage -> ControlOffered
                        | EditingPageEntry
                        | CreatingPageEntry -> ControlWithheld
                    let menu = model.entryMenuOf study.name.caption (Some (RenameStudyTarget study.name)) (model.studyRenaming study.name) dispatch
                    PageControls.entryToggle (ConstructorUiId.studyChoice study.name) study.name.caption (if chosen then ChosenChoice else UnchosenChoice) NeutralCommand availability menu (fun () ->
                        dispatch (if chosen then DeselectStudy else SelectStudy study.name)) []
            let under (study : Study) : IView list =
                match model.studySurface.studyOpt with
                | Some name when name = study.name ->
                    underEntry (ConstructorUiKey $"ChosenStudy_{study.name.fileStem.value}_{model.pageMode.uiKey.value}") (chosenStudyRows model study dispatch)
                | Some _
                | None -> []
            let studies : IView list =
                match model.kitStudies with
                | [] -> [ PageControls.copyableLine (ControlCaption "This kit holds no study yet: New study makes one of the experiments to fit.") [] ]
                | held ->
                    [
                        StackPanel.create [
                            yield! ConstructorUiId.studyList.groupAttributes<StackPanel> ()
                            StackPanel.orientation Orientation.Vertical
                            StackPanel.children (entryRows held row under)
                        ]
                        :> IView
                    ]
            newVerb @ studies
    pageScroll ConstructorUiId.studyUnknownsPanel (model.pageHeader dispatch @ body)

/// The line heading Plan, Fit and Results: the study chosen on Unknowns, the page's context, or why the page needs one
/// (spec 0051 `.manual/007` 8.1).
let studyContextRows (model : TableAndElementRotationModel) : IView list =
    match model.selectedStudyOpt with
    | Some study -> [ PageControls.copyableText ConstructorUiId.studyContextText (ControlCaption $"Study: {study.name.value}") [ SelectableTextBlock.fontWeight FontWeight.SemiBold ] ]
    | None -> [ PageControls.copyableText ConstructorUiId.studyContextText (ControlCaption "Choose a study in Unknowns first: press Unknowns, then the study.") [] ]
