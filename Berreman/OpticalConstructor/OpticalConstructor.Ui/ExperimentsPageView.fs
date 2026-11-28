/// The Experiments page: no table and no table buttons; the selected setup named in its header; the kit's experiments as
/// dynamic buttons, the chosen one's setup, scan and bookkeeping under its row, edited in place; New experiment over the
/// selected setup, asking first when the setup already has experiments; New experiments from a setup grid… (spec 0050
/// S9.1; 0051 `.manual/007` Phase 6).
module OpticalConstructor.Ui.ExperimentsPageView

open Avalonia
open Avalonia.Controls
open Avalonia.Controls.Primitives
open Avalonia.Layout
open Avalonia.Media
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open OpticalConstructor.Controls
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.Setups
open OpticalConstructor.Ui.TableAndElementRotationView
open OpticalConstructor.Ui.KitPageFrame

/// A labelled text field of the edit, docked so its label never leaves the row.
let private labelledField (label : ControlCaption) (id : ConstructorUiId) (text : ControlCaption) (onChanged : ControlCaption -> unit) : IView =
    DockPanel.create [
        DockPanel.margin (Thickness(0.0, 2.0, 0.0, 2.0))
        DockPanel.children [
            TextBlock.create [
                TextBlock.dock Dock.Left
                TextBlock.verticalAlignment VerticalAlignment.Center
                TextBlock.margin (Thickness(0.0, 0.0, 6.0, 0.0))
                TextBlock.text label.value
            ]
            TextBox.create [
                yield! id.inputAttributes<TextBox> ()
                TextBox.width 320.0
                TextBox.horizontalAlignment HorizontalAlignment.Left
                TextBox.text text.value
                TextBox.onTextChanged (Option.ofObj >> Option.map ControlCaption >> Option.iter onChanged)
            ]
        ]
    ]
    |> View.withKey id.value
    :> IView

type TableAndElementRotationModel with

    /// The experiment's setup revision as a line: its name and revision, then its slots; why it cannot be drawn when the
    /// kit lacks the revision (spec 0051 `.manual/007` 6.2, D16).
    member private model.experimentSetupLine (experiment : Experiments.Experiment) : ControlCaption =
        match experiment.slotsIn model.kitSetups with
        | Ok slots ->
            ControlCaption $"Setup: {experiment.setup.describe} — {(Setups.SetupSlot.slotLineOf slots model.kitItems).value}"
        | Error error -> ControlCaption $"Setup: {experiment.setup.describe} — {error.describe}"

    /// The experiment as its details read: setup, scan, source wavelength, role, session, notes and data (spec 0051
    /// `.manual/007` 6.2).
    member private model.experimentDetailLines (experiment : Experiments.Experiment) : ControlCaption list =
        let wavelength =
            match experiment.scan.sourceWaveLengthOpt with
            | Some waveLength -> ControlCaption $"Source wavelength: %g{waveLength.nanometers} nm"
            | None -> ControlCaption "Source wavelength: the source's own"
        let session =
            match experiment.sessionOpt with
            | Some name -> name.caption
            | None -> ControlCaption "(none)"
        let notes =
            match experiment.notes.caption.value with
            | "" -> ControlCaption "(none)"
            | _ -> experiment.notes.caption
        [
            model.experimentSetupLine experiment
            ControlCaption $"Scan: {(model.experimentDescription experiment).value}"
            wavelength
            ControlCaption $"Role: {experiment.role.text}"
            ControlCaption $"Session: {session.value}"
            ControlCaption $"Notes: {notes.value}"
            ControlCaption $"Data: {(model.dataBadgeOf experiment).text}"
        ]

type Experiments.Experiment with

    /// The experiment's audit trail: one line per setup correction, oldest first; nothing for an experiment never re-pointed
    /// (spec 0050 S9.3; 001 §K).
    member private experiment.correctionTrailRows : IView list =
        match experiment.corrections with
        | [] -> []
        | corrections ->
            [
                StackPanel.create [
                    yield! ConstructorUiId.dataCorrectionTrail.groupAttributes<StackPanel> ()
                    StackPanel.orientation Orientation.Vertical
                    StackPanel.margin (Thickness(0.0, 2.0, 0.0, 4.0))
                    StackPanel.children (
                        (ControlCaption "Setup corrections:").pageLine
                        :: (corrections
                            |> List.map (fun correction ->
                                PageControls.copyableLine (ControlCaption $"{correction.previous.describe} → {correction.corrected.describe}: {correction.note.value}") [])))
                ]
                :> IView
            ]

/// The chosen experiment's verbs: View ↗ (withheld with its reason when there is no chart), Export data, Edit and
/// Remove, Remove asking first (spec 0051 `.manual/003` 2.3, `.manual/007` 6.2, 6.6).
let private experimentVerbs (model : TableAndElementRotationModel) (experiment : Experiments.Experiment) (dispatch : TableAndElementRotationMsg -> unit) : IView list =
    let view : IView list =
        match model.chartAvailabilityOf experiment with
        | ChartReady ->
            [ CommandControls.verbCommand (ConstructorUiId.experimentViewButton experiment.id.uiKey) (ControlCaption "View chart ↗") (fun () -> dispatch (ViewExperiment experiment.id)) ]
        | ChartWithheldFor reason ->
            [
                CommandControls.commandButton (ConstructorUiId.experimentViewButton experiment.id.uiKey) (ControlCaption "View chart ↗") NeutralCommand ControlWithheld ignore
                    [ Button.margin (Thickness(0.0, 0.0, 8.0, 4.0)) ]
                PageControls.copyableText ConstructorUiId.experimentChartReason (ControlCaption $"No chart: {reason.value}") [
                    SelectableTextBlock.foreground (SolidColorBrush(Color.FromRgb(90uy, 90uy, 90uy)))
                    SelectableTextBlock.verticalAlignment VerticalAlignment.Center
                    SelectableTextBlock.margin (Thickness(0.0, 0.0, 8.0, 4.0))
                ]
            ]
    let writes : IView list =
        match model.kitWrites, model.experimentRemovalOpt with
        | ControlOffered, Some pending when pending = experiment.id -> []
        | ControlOffered, _ ->
            [
                CommandControls.verbCommand ConstructorUiId.pageEditButton (ControlCaption "Edit") (fun () -> dispatch BeginPageEdit)
                CommandControls.verbCommand (ConstructorUiId.experimentRemoveButton experiment.id.uiKey) (ControlCaption "Remove") (fun () ->
                    dispatch (ExpRemove experiment.id))
            ]
        | ControlWithheld, _ -> []
    let question : IView list =
        match model.experimentRemovalOpt with
        | Some pending when pending = experiment.id ->
            [
                WrapPanel.create [
                    WrapPanel.orientation Orientation.Horizontal
                    WrapPanel.children [
                        PageControls.copyableText ConstructorUiId.experimentRemovePrompt (ControlCaption "Remove this experiment and its file?") [
                            SelectableTextBlock.verticalAlignment VerticalAlignment.Center
                            SelectableTextBlock.margin (Thickness(0.0, 0.0, 8.0, 4.0))
                        ]
                        CommandControls.actionCommand ConstructorUiId.experimentRemoveConfirmButton (ControlCaption "Yes, remove") NegativeCommand (fun () -> dispatch ExpConfirmRemove)
                        CommandControls.actionCommand ConstructorUiId.experimentRemoveCancelButton (ControlCaption "No") NeutralCommand (fun () -> dispatch ExpCancelRemove)
                    ]
                ]
                :> IView
            ]
        | Some _
        | None -> []
    [
        WrapPanel.create [
            WrapPanel.orientation Orientation.Horizontal
            WrapPanel.children (view @ writes)
        ]
        :> IView
    ]
    @ question

/// Export data: the edited or new experiment's computed run through the export seam, and its status (spec 0046).
let private exportRows (model : TableAndElementRotationModel) (dispatch : TableAndElementRotationMsg -> unit) : IView list =
    let status : IView list =
        match model.exportStatus with
        | Some told -> [ PageControls.copyableText ConstructorUiId.experimentExportStatus told [ SelectableTextBlock.verticalAlignment VerticalAlignment.Center ] ]
        | None -> []
    [
        WrapPanel.create [
            WrapPanel.orientation Orientation.Horizontal
            WrapPanel.children (CommandControls.verbCommand ConstructorUiId.experimentExportButton (ControlCaption "Export data") (fun () -> dispatch ExportExperimentData) :: status)
        ]
        :> IView
    ]

/// The chosen experiment read-only (spec 0051 `.manual/007` 6.2).
let private experimentDetailsRows (model : TableAndElementRotationModel) (experiment : Experiments.Experiment) (dispatch : TableAndElementRotationMsg -> unit) : IView list =
    let lines =
        model.experimentDetailLines experiment
        |> List.map (fun line -> line.value)
        |> String.concat "\n"
    experimentVerbs model experiment dispatch
    @ [ PageControls.copyableText ConstructorUiId.experimentDetailsText (ControlCaption lines) [ SelectableTextBlock.margin (Thickness(0.0, 2.0, 0.0, 4.0)) ] ]
    @ experiment.correctionTrailRows

/// The chosen experiment's edit in place: the editor over its own setup revision, then its role, session and notes, the
/// re-point to another revision of its setup with the audit note, then Save and Cancel (spec 0051 `.manual/007` 6.3).
let private experimentEditRows
    (model : TableAndElementRotationModel)
    (experiment : Experiments.Experiment)
    (edit : ExperimentEditDraft)
    (dispatch : TableAndElementRotationMsg -> unit)
    : IView list =
    let roleBox (role : Experiments.ExperimentRole) : IView =
        CommandControls.choiceCommand (ConstructorUiId.experimentRoleChoice role.uiKey) role.caption (if role = edit.role then ChosenChoice else UnchosenChoice) (fun () ->
            dispatch (SetExperimentDraftRole role))
    let repointBoxes : IView list =
        // Spec 0050 S9.3: a re-point is to another setup of the kit, at its current revision.
        let others = model.kitSetups |> List.filter (fun setup -> setup.reference <> experiment.setup)
        match others with
        | [] -> [ (ControlCaption "The kit has no other setup to re-point it to.").pageLine ]
        | held ->
            [
                WrapPanel.create [
                    WrapPanel.orientation Orientation.Horizontal
                    WrapPanel.children (
                        held
                        |> List.map (fun setup ->
                            let reference = setup.reference
                            let mark = if edit.repointOpt = Some reference then ChosenChoice else UnchosenChoice
                            let choose () = dispatch (SetExperimentDraftRepoint (if edit.repointOpt = Some reference then None else Some reference))
                            CommandControls.choiceCommand (ConstructorUiId.dataCorrectSetupChoice setup.name) (ControlCaption $"Re-point to {reference.describe}") mark choose))
                ]
                :> IView
                labelledField (ControlCaption "Audit note:") ConstructorUiId.dataCorrectionNoteField model.dataDraft.correctionNote (SetCorrectionNote >> dispatch)
            ]
    [
        PageControls.copyableText ConstructorUiId.experimentDetailsText (model.experimentSetupLine experiment) []
        ExperimentControls.experimentEditorView model.experimentState (experimentHandlers model dispatch)
    ]
    @ exportRows model dispatch
    @ [
        (ControlCaption "Role:").pageHeading
        WrapPanel.create [ WrapPanel.orientation Orientation.Horizontal; WrapPanel.children (Experiments.ExperimentRole.all |> List.map roleBox) ] :> IView
        labelledField (ControlCaption "Session:") ConstructorUiId.experimentSessionField edit.sessionText (SetExperimentDraftSession >> dispatch)
        labelledField (ControlCaption "Notes:") ConstructorUiId.experimentNotesField edit.notesText (SetExperimentDraftNotes >> dispatch)
        (ControlCaption "Its setup revision:").pageHeading
    ]
    @ repointBoxes
    @ experiment.correctionTrailRows
    @ [ model.draftActions (ControlCaption "Save") dispatch ]

/// "E3 and E7 already use setup X. Create another?" with OK, Cancel and Show (spec 0051 `.manual/007` 6.4, D8, D12).
let private sameSetupRows (model : TableAndElementRotationModel) (dispatch : TableAndElementRotationMsg -> unit) : IView list =
    match model.sameSetupWarningOpt, model.activeSetupOpt with
    | Some ids, Some active ->
        let names =
            ids
            |> List.choose (fun id -> model.experimentCollection.experiments |> List.tryFind (fun experiment -> experiment.id = id))
            |> List.map (fun experiment -> experiment.name.value)
            |> String.concat ", "
        [
            Border.create [
                Border.background (SolidColorBrush(Color.FromRgb(255uy, 244uy, 214uy)))
                Border.borderBrush (SolidColorBrush(Color.FromRgb(200uy, 160uy, 60uy)))
                Border.borderThickness (Thickness 1.0)
                Border.padding (Thickness(8.0, 4.0))
                Border.margin (Thickness(0.0, 4.0, 0.0, 6.0))
                Border.child (
                    WrapPanel.create [
                        WrapPanel.orientation Orientation.Horizontal
                        WrapPanel.children [
                            PageControls.copyableText ConstructorUiId.sameSetupWarningText (ControlCaption $"{names} already use setup {active.name.value}. Create another?") [
                                SelectableTextBlock.verticalAlignment VerticalAlignment.Center
                                SelectableTextBlock.margin (Thickness(0.0, 0.0, 10.0, 4.0))
                            ]
                            CommandControls.actionCommand ConstructorUiId.sameSetupOkButton (ControlCaption "OK") PositiveCommand (fun () -> dispatch ConfirmSameSetup)
                            CommandControls.actionCommand ConstructorUiId.sameSetupCancelButton (ControlCaption "Cancel") NeutralCommand (fun () -> dispatch CancelSameSetup)
                            CommandControls.actionCommand ConstructorUiId.sameSetupShowButton (ControlCaption "Show") NeutralCommand (fun () -> dispatch ShowSameSetupExperiment)
                        ]
                    ])
            ]
            :> IView
        ]
    | _ -> []

/// The Experiments page (spec 0051 `.manual/007` 6.1–6.6).
let experimentsPageView (model : TableAndElementRotationModel) (dispatch : TableAndElementRotationMsg -> unit) : IView =
    let context : IView list =
        match model.activeSetupOpt with
        | Some active ->
            [ PageControls.copyableText ConstructorUiId.experimentsContextText (ControlCaption $"New experiments use setup {active.name.value}, revision {active.revision.value}.") [] ]
        | None ->
            [ PageControls.copyableText ConstructorUiId.experimentsContextText (ControlCaption "Choose a setup in Setups first: press Setups, then the setup. A new experiment is made over the chosen setup.") [] ]
    let body : IView list =
        match model.pageMode, model.setupGridState, model.activeSetupOpt with
        | CreatingPageEntry, Some grid, _ ->
            [ (ControlCaption "New experiments from a setup grid").pageHeading; SetupGridView.setupGridView grid (setupGridHandlers dispatch) ]
        | CreatingPageEntry, None, Some active ->
            [
                (ControlCaption $"New experiment over {active.name.value}").pageHeading
                ExperimentControls.experimentEditorView model.experimentState (experimentHandlers model dispatch)
            ]
            @ exportRows model dispatch
            @ [ labelledField (ControlCaption "Note for its name:") ConstructorUiId.recordNoteField model.recordNote (SetRecordNote >> dispatch) ]
            @ sameSetupRows model dispatch
            @ [ model.draftActions (ControlCaption "Create") dispatch ]
        | _ ->
            let newVerbs : IView list =
                match model.kitWrites, model.pageMode, model.activeSetupOpt with
                | ControlOffered, BrowsingPage, Some _ ->
                    [
                        WrapPanel.create [
                            WrapPanel.orientation Orientation.Horizontal
                            WrapPanel.children [
                                CommandControls.verbCommand ConstructorUiId.newExperimentButton (ControlCaption "New experiment") (fun () -> dispatch BeginPageCreate)
                                CommandControls.verbCommand ConstructorUiId.newExperimentGridButton (ControlCaption "New experiments from a setup grid…") (fun () ->
                                    dispatch BeginExperimentGrid)
                            ]
                        ]
                        :> IView
                    ]
                | _ -> []
            let row (experiment : Experiments.Experiment) : IView =
                match model.renameRowsOf (RenameExperimentTarget experiment.id) dispatch with
                | Some rows -> StackPanel.create [ StackPanel.children rows ] :> IView
                | None ->
                    let chosen = model.chosenExperimentOpt = Some experiment.id
                    let availability =
                        match model.pageMode with
                        | BrowsingPage -> ControlOffered
                        | EditingPageEntry
                        | CreatingPageEntry -> ControlWithheld
                    let caption = ControlCaption $"{experiment.name.value} [{experiment.role.text}] — {(model.dataBadgeOf experiment).text}"
                    let menu = model.entryMenuOf experiment.name.caption (Some (RenameExperimentTarget experiment.id)) RenameAvailable dispatch
                    PageControls.entryToggle
                        (ConstructorUiId.experimentEditButton experiment.id.uiKey)
                        caption
                        (if chosen then ChosenChoice else UnchosenChoice)
                        NeutralCommand
                        availability
                        menu
                        (fun () -> dispatch (ChooseExperiment experiment.id))
                        [ ToggleButton.maxWidth 640.0 ]
            let under (experiment : Experiments.Experiment) : IView list =
                match model.chosenExperimentOpt with
                | Some id when id = experiment.id ->
                    let key = ConstructorUiKey $"ChosenExperiment_{experiment.id.value}_{model.pageMode.uiKey.value}"
                    match model.pageMode, model.experimentEditOpt with
                    | EditingPageEntry, Some edit when edit.experimentId = experiment.id -> underEntry key (experimentEditRows model experiment edit dispatch)
                    | _ -> underEntry key (experimentDetailsRows model experiment dispatch)
                | Some _
                | None -> []
            let experiments : IView list =
                match model.experimentCollection.experiments with
                | [] -> [ (ControlCaption "This kit has no experiment yet.").pageLine ]
                | held ->
                    [
                        StackPanel.create [
                            yield! ConstructorUiId.experimentCollection.groupAttributes<StackPanel> ()
                            StackPanel.orientation Orientation.Vertical
                            StackPanel.margin (Thickness(0.0, 4.0, 0.0, 4.0))
                            StackPanel.children (entryRows held row under)
                        ]
                        :> IView
                    ]
            newVerbs @ experiments
    pageScroll ConstructorUiId.experimentsPagePanel (model.pageHeader dispatch @ context @ body)
