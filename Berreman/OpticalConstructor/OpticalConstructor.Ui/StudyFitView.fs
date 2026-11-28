/// The Fit page: the chosen study's fit runs as dynamic buttons, the chosen one's summary under it, and New fit run…, whose
/// form holds the pre-fit checks, the mode and the calibration use, written into the study when Run starts the stage
/// (spec 0050 S11.5; 0051 `.manual/007` 8.4).
module OpticalConstructor.Ui.StudyFitView

open Avalonia
open Avalonia.Controls
open Avalonia.Automation
open Avalonia.Controls.Primitives
open Avalonia.Layout
open Avalonia.Media
open Avalonia.FuncUI.Builder
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open OpticalConstructor.Controls
open OpticalConstructor.Domain.FitRuns
open OpticalConstructor.Domain.PrefitChecks
open OpticalConstructor.Domain.Studies
open OpticalConstructor.Optimization.StudyEngine
open OpticalConstructor.Ui.StudyFitJob
open OpticalConstructor.Ui.TableAndElementRotationView
open OpticalConstructor.Ui.KitPageFrame
open OpticalConstructor.Ui.StudyUnknownsView

type TableAndElementRotationModel with

    /// Each experiment's check before the fit: ready with its point count, or why it is not (spec 0050 S11.5).
    member private model.prefitRows : IView list =
        let prefits : IView list =
            match model.studyPrefitsOpt with
            | None | Some (Ok []) -> [ PageControls.copyableLine (ControlCaption "The study holds no run a fit reads.") [] ]
            | Some (Error error) ->
                [ PageControls.copyableLine (ControlCaption error.describe) [ SelectableTextBlock.foreground (SolidColorBrush MaterialsWindowView.messageColor) ] ]
            | Some (Ok checks) ->
                checks |> List.map (fun prefit ->
                    let status =
                        match prefit.check with
                        | Ok points -> $"ready, {points.pointCount.value} point(s)"
                        | Error error -> error.describe
                    let changed =
                        match prefit.changeOpt with
                        | None | Some DataUnchangedFlag -> ""
                        | Some flag -> $" — {flag.text}"
                    let caption = ControlCaption $"{prefit.experiment.name.value}: {status}{changed}"
                    PageControls.copyableText (ConstructorUiId.studyPrefitRow prefit.experiment.name) caption [
                        match prefit.check with
                        | Error _ -> SelectableTextBlock.foreground (SolidColorBrush MaterialsWindowView.messageColor)
                        | Ok _ -> SelectableTextBlock.foreground (SolidColorBrush(Color.FromRgb(40uy, 40uy, 40uy)))
                    ])
        [
            (ControlCaption "Before the fit:").pageHeading
            StackPanel.create [
                yield! ConstructorUiId.studyPrefitList.groupAttributes<StackPanel> ()
                AttrBuilder<StackPanel>.CreateProperty<string>(AutomationProperties.NameProperty, "Before the fit", ValueNone)
                StackPanel.children prefits
            ]
            :> IView
        ]

/// The New fit run… form: the checks, the fit mode, how stage 2 holds the calibration, then a Run per stage and Cancel;
/// the study takes the form's settings when a run starts (spec 0050 S11.3, S11.5; 0051 `.manual/007` 8.4).
let private newFitRunRows (model : TableAndElementRotationModel) (study : Study) (draft : FitRunDraft) (dispatch : TableAndElementRotationMsg -> unit) : IView list =
    let modeChoices : IView =
        WrapPanel.create [
            WrapPanel.children (
                FitMode.all |> List.map (fun mode ->
                    CommandControls.choiceCommand (ConstructorUiId.studyFitModeChoice mode) (ControlCaption mode.text) (if mode = draft.mode then ChosenChoice else UnchosenChoice) (fun () ->
                        dispatch (SetFitRunDraftMode mode))))
        ]
        :> IView
    let calibration : IView =
        WrapPanel.create [
            WrapPanel.children (
                [ CalibrationHeldFixed, ConstructorUiId.studyCalibrationFixedChoice
                  CalibrationAsPrior, ConstructorUiId.studyCalibrationPriorChoice ]
                |> List.map (fun (choice, id) ->
                    CommandControls.choiceCommand id (ControlCaption choice.text) (if choice = draft.calibrationUse then ChosenChoice else UnchosenChoice) (fun () ->
                        dispatch (SetFitRunDraftCalibration choice))))
        ]
        :> IView
    let runs : IView list =
        let stages =
            match draft.mode with
            | JointFit -> [ JointStage, ConstructorUiId.studyRunFitButton, ControlCaption "Run joint" ]
            | StagedFit ->
                [
                    CalibrationStage, ConstructorUiId.studyRunStageOneButton, ControlCaption "Run stage 1 (calibration)"
                    ObjectStage, ConstructorUiId.studyRunStageTwoButton, ControlCaption "Run stage 2 (objects)"
                ]
        [
            WrapPanel.create [
                WrapPanel.orientation Orientation.Horizontal
                WrapPanel.isEnabled (not model.studyFit.job.isRunning)
                WrapPanel.children (
                    (stages |> List.map (fun (stage, id, caption) -> CommandControls.actionCommand id caption PositiveCommand (fun () -> dispatch (StartFitRun stage))))
                    @ [ CommandControls.actionCommand ConstructorUiId.pageCancelButton (ControlCaption "Cancel") NegativeCommand (fun () -> dispatch CancelPageDraft) ])
            ]
            |> View.withKey $"StudyFitActions_{study.name.fileStem.value}_{draft.mode.value}_{draft.calibrationUse.value}"
            :> IView
        ]
    [ (ControlCaption "New fit run").pageHeading ]
    @ model.prefitRows
    @ [ (ControlCaption "Fit mode:").pageHeading; modeChoices ]
    @ (match draft.mode with
       | StagedFit -> [ (ControlCaption "Stage 2 holds the calibration:").pageHeading; calibration ]
       | JointFit -> [])
    @ runs

/// The running fit's progress and Cancel, then the last refusal (spec 0050 S11.5).
let private fitProgressRows (model : TableAndElementRotationModel) (dispatch : TableAndElementRotationMsg -> unit) : IView list =
    let progress : IView list =
        if model.studyFit.job.isRunning then
            [
                ProgressBar.create [
                    ConstructorUiId.studyFitProgressBar.automationAttribute<ProgressBar> ()
                    AttrBuilder<ProgressBar>.CreateProperty<string>(AutomationProperties.NameProperty, "Fit running", ValueNone)
                    ProgressBar.isIndeterminate true
                    ProgressBar.height 12.0
                ]
                PageControls.copyableText ConstructorUiId.studyFitProgressText (ControlCaption (model.studyFit.job.progressText |> Option.defaultValue "")) []
                CommandControls.actionCommand ConstructorUiId.studyFitCancelButton (ControlCaption "Cancel the fit") NegativeCommand (fun () -> dispatch (StudyFit CancelStudyFit))
            ]
        else []
    let errors : IView list =
        match model.studyFit.errorOpt with
        | None -> []
        | Some error ->
            [ PageControls.copyableText ConstructorUiId.studyFitErrorText (ControlCaption error.describe) [ SelectableTextBlock.foreground (SolidColorBrush MaterialsWindowView.messageColor) ] ]
    progress @ errors

/// The Fit page (spec 0050 S11.5; 0051 `.manual/007` 8.1, 8.4).
let studyFitView (model : TableAndElementRotationModel) (dispatch : TableAndElementRotationMsg -> unit) : IView =
    let body : IView list =
        match model.selectedStudyOpt, model.pageMode, model.fitRunDraftOpt with
        | None, _, _ -> []
        | Some study, CreatingPageEntry, Some draft -> newFitRunRows model study draft dispatch
        | Some study, _, _ ->
            let newRun : IView list =
                match model.kitWrites, model.studyFit.job.isRunning with
                | ControlOffered, false ->
                    [
                        Border.create [
                            Border.horizontalAlignment HorizontalAlignment.Left
                            Border.child (CommandControls.verbCommand ConstructorUiId.newFitRunButton (ControlCaption "New fit run…") (fun () -> dispatch BeginPageCreate))
                        ]
                        :> IView
                    ]
                | _ -> []
            let row (run : FitRun) : IView =
                let caption = ControlCaption $"{run.name.value} — {run.stage.text}, {run.engine.text}, {run.runs.Length} run(s)"
                let chosen = model.studySurface.fitRunOpt = Some run.name
                let menu = model.entryMenuOf run.name.caption None RenameAvailable dispatch
                PageControls.entryToggle (ConstructorUiId.fitRunRow run.name) caption (if chosen then ChosenChoice else UnchosenChoice) NeutralCommand ControlOffered menu (fun () ->
                    dispatch (ChooseFitRun run.name)) []
            let under (run : FitRun) : IView list =
                match model.studySurface.fitRunOpt with
                | Some name when name = run.name ->
                    let stamp = run.stamp.value.ToLocalTime().ToString("yyyy-MM-dd HH:mm")
                    let runs = run.runs |> List.map _.value |> String.concat ", "
                    underEntry (ConstructorUiKey $"ChosenFitRun_{run.name.value}") [
                        PageControls.copyableLine (ControlCaption $"Run {stamp}: mode {run.mode.text}, stage {run.stage.text}, calibration {run.calibrationUse.text}, engine {run.engine.text}") []
                        PageControls.copyableLine (ControlCaption $"Experiments read: {runs}") []
                        Border.create [
                            Border.horizontalAlignment HorizontalAlignment.Left
                            Border.child (CommandControls.verbCommand ConstructorUiId.openFitResultsButton (ControlCaption "Open results") (fun () -> dispatch (ShowFlowStep ResultsFlowStep)))
                        ]
                        :> IView
                    ]
                | Some _
                | None -> []
            let history : IView list =
                match model.studyFitRuns |> List.rev with
                | [] -> [ PageControls.copyableLine (ControlCaption "No fit run yet: New fit run… runs the study.") [] ]
                | runs ->
                    [
                        (ControlCaption "Fit runs:").pageHeading
                        StackPanel.create [
                            yield! ConstructorUiId.fitRunHistory.groupAttributes<StackPanel> ()
                            AttrBuilder<StackPanel>.CreateProperty<string>(AutomationProperties.NameProperty, "Fit runs", ValueNone)
                            StackPanel.children (entryRows runs row under)
                        ]
                        :> IView
                    ]
            newRun @ fitProgressRows model dispatch @ history
    pageScroll ConstructorUiId.studyFitPanel (model.pageHeader dispatch @ model.studyContextRows @ body)
