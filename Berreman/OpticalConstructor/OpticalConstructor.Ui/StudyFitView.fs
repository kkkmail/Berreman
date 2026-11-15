/// The study's pre-fit checks, joint and staged fits, progress and history (spec 0050 S11.5).
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

let private namedFitActionButton
    (id : ConstructorUiId)
    (caption : ControlCaption)
    (background : Color)
    (onClick : unit -> unit)
    : IView =
    let action = MaterialsWindowView.actionButton id caption background onClick :?> IView<Border>
    action
    |> View.withAttrs [ AttrBuilder<Border>.CreateProperty<string>(AutomationProperties.NameProperty, caption.value, ValueNone) ]
    :> IView

/// Chooses the fit mode, runs the study through the shell's host and shows checks and history (spec 0050 S11.5; S12.1).
let studyFitView (model : TableAndElementRotationModel) (dispatch : TableAndElementRotationMsg -> unit) : IView =
    let selected : IView list =
        match model.selectedStudyOpt with
        | None -> [ TextBlock.create [ TextBlock.text "No study is chosen: choose one in Unknowns." ] ]
        | Some study ->
            let prefits : IView list =
                match model.studyPrefitsOpt with
                | None | Some (Ok []) ->
                    [ TextBlock.create [ TextBlock.text "The study holds no run a fit reads." ] ]
                | Some (Error error) ->
                    [
                        TextBlock.create [
                            TextBlock.textWrapping TextWrapping.Wrap
                            TextBlock.foreground (MaterialsWindowView.brush MaterialsWindowView.messageColor)
                            TextBlock.text error.describe
                        ]
                    ]
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
                        TextBlock.create [
                            (ConstructorUiId.studyPrefitRow prefit.experiment.name).automationAttribute<TextBlock> ()
                            AttrBuilder<TextBlock>.CreateProperty<string>(AutomationProperties.NameProperty, caption.value, ValueNone)
                            TextBlock.textWrapping TextWrapping.Wrap
                            TextBlock.text caption.value
                            match prefit.check with
                            | Error _ -> TextBlock.foreground (MaterialsWindowView.brush MaterialsWindowView.messageColor)
                            | Ok _ -> ()
                        ] :> IView)
            let modeChoices : IView =
                match model.kitWrites with
                | ControlWithheld -> TextBlock.create [ TextBlock.text $"Fit mode: {study.settings.mode.text}" ] :> IView
                | ControlOffered ->
                    WrapPanel.create [
                        WrapPanel.children (
                            [
                                TextBlock.create [
                                    TextBlock.text "Fit mode:"
                                    TextBlock.verticalAlignment VerticalAlignment.Center
                                    TextBlock.margin (Thickness(0.0, 0.0, 8.0, 4.0))
                                ] :> IView
                            ]
                            @ (FitMode.all |> List.map (fun mode ->
                                let id = ConstructorUiId.studyFitModeChoice mode
                                let caption = ControlCaption mode.text
                                let select () = dispatch (StudyCommand (SetFitMode (study.name, mode)))
                                if mode = study.settings.mode then
                                    namedFitActionButton id caption MaterialsWindowView.positiveBackground select
                                else
                                    MaterialsWindowView.verbButton id caption select)))
                    ]
                    // Spec 0050 S12.1: actionButton retains its handler, so another study needs a fresh closure.
                    |> View.withKey $"StudyFitModes_{study.name.fileStem.value}"
                    :> IView
            let runButtons : IView list =
                match model.kitWrites with
                | ControlWithheld -> []
                | ControlOffered ->
                    [
                        StackPanel.create [
                            StackPanel.orientation Orientation.Horizontal
                            StackPanel.isEnabled (not model.studyFit.job.isRunning)
                            StackPanel.children (
                                [
                                    JointStage, ConstructorUiId.studyRunFitButton, ControlCaption "Run joint"
                                    CalibrationStage, ConstructorUiId.studyRunStageOneButton, ControlCaption "Run stage 1 (calibration)"
                                    ObjectStage, ConstructorUiId.studyRunStageTwoButton, ControlCaption "Run stage 2 (objects)"
                                ]
                                |> List.map (fun (stage, id, caption) ->
                                    let request : StudyFitRequest =
                                        {
                                            study = study.name
                                            stage = stage
                                            calibrationUse = model.studySurface.calibrationUse
                                        }
                                    namedFitActionButton id caption MaterialsWindowView.positiveBackground
                                        (fun () -> dispatch (StudyFit (RunFit request)))))
                        ]
                        // Spec 0050 S11.5: actionButton retains its handler, so each request needs a fresh closure.
                        |> View.withKey $"StudyFitActions_{study.name.fileStem.value}_{model.studySurface.calibrationUse.value}"
                        :> IView
                    ]
            let calibration : IView =
                StackPanel.create [
                    StackPanel.orientation Orientation.Horizontal
                    StackPanel.children (
                        [ CalibrationHeldFixed, ConstructorUiId.studyCalibrationFixedChoice
                          CalibrationAsPrior, ConstructorUiId.studyCalibrationPriorChoice ]
                        |> List.map (fun (choice, id) ->
                            let caption = ControlCaption choice.text
                            let select () = dispatch (SetStudyCalibrationUse choice)
                            if choice = model.studySurface.calibrationUse then
                                namedFitActionButton id caption MaterialsWindowView.positiveBackground select
                            else
                                MaterialsWindowView.verbButton id caption select))
                ] :> IView
            let progress : IView list =
                if model.studyFit.job.isRunning then
                    [
                        ProgressBar.create [
                            ConstructorUiId.studyFitProgressBar.automationAttribute<ProgressBar> ()
                            AttrBuilder<ProgressBar>.CreateProperty<string>(AutomationProperties.NameProperty, "Fit running", ValueNone)
                            ProgressBar.isIndeterminate true
                            ProgressBar.height 12.0
                        ]
                        TextBlock.create [
                            ConstructorUiId.studyFitProgressText.automationAttribute<TextBlock> ()
                            AttrBuilder<TextBlock>.CreateProperty<string>(AutomationProperties.NameProperty, "Fit progress", ValueNone)
                            TextBlock.text (model.studyFit.job.progressText |> Option.defaultValue "")
                        ]
                        namedFitActionButton ConstructorUiId.studyFitCancelButton (ControlCaption "Cancel")
                            MaterialsWindowView.negativeBackground (fun () -> dispatch (StudyFit CancelStudyFit))
                    ]
                else []
            let errors : IView list =
                match model.studyFit.errorOpt with
                | None -> []
                | Some error ->
                    [
                        TextBlock.create [
                            ConstructorUiId.studyFitErrorText.automationAttribute<TextBlock> ()
                            AttrBuilder<TextBlock>.CreateProperty<string>(AutomationProperties.NameProperty, error.describe, ValueNone)
                            TextBlock.textWrapping TextWrapping.Wrap
                            TextBlock.foreground (MaterialsWindowView.brush MaterialsWindowView.messageColor)
                            TextBlock.text error.describe
                        ]
                    ]
            let history : IView list =
                match model.studyFitRuns with
                | [] -> [ TextBlock.create [ TextBlock.text "No fit run yet." ] ]
                | runs ->
                    runs |> List.map (fun run ->
                        let id = ConstructorUiId.fitRunRow run.name
                        let caption = ControlCaption $"{run.name.value} — {run.stage.text}, {run.engine.text}, {run.runs.Length} run(s)"
                        let select () = dispatch (SelectFitRun run.name)
                        if model.studySurface.fitRunOpt = Some run.name then
                            namedFitActionButton id caption MaterialsWindowView.positiveBackground select
                        else
                            MaterialsWindowView.verbButton id caption select)
            [
                TextBlock.create [ TextBlock.text "Before the fit:" ]
                StackPanel.create [
                    ConstructorUiId.studyPrefitList.automationAttribute<StackPanel> ()
                    AttrBuilder<StackPanel>.CreateProperty<string>(AutomationProperties.NameProperty, "Before the fit", ValueNone)
                    StackPanel.children prefits
                ]
            ]
            @ [ modeChoices ]
            @ runButtons
            @ [ TextBlock.create [ TextBlock.text "Stage 2 holds the calibration:" ]; calibration ]
            @ progress
            @ errors
            @ [
                TextBlock.create [ TextBlock.text "Fit runs:" ]
                StackPanel.create [
                    ConstructorUiId.fitRunHistory.automationAttribute<StackPanel> ()
                    AttrBuilder<StackPanel>.CreateProperty<string>(AutomationProperties.NameProperty, "Fit runs", ValueNone)
                    StackPanel.children history
                ]
            ]
    ScrollViewer.create [
        // Spec 0050 S11.5: reserve scrollbar space so scrolled controls remain clickable at the viewport edge.
        AttrBuilder<ScrollViewer>.CreateProperty<bool>(ScrollViewer.AllowAutoHideProperty, false, ValueNone)
        ScrollViewer.horizontalScrollBarVisibility ScrollBarVisibility.Auto
        ScrollViewer.verticalScrollBarVisibility ScrollBarVisibility.Auto
        ScrollViewer.content (
            StackPanel.create [
                ConstructorUiId.studyFitPanel.automationAttribute<StackPanel> ()
                AttrBuilder<StackPanel>.CreateProperty<string>(AutomationProperties.NameProperty, "Study fit", ValueNone)
                StackPanel.orientation Orientation.Vertical
                StackPanel.margin 8.0
                StackPanel.children ([ StudyUnknownsView.studyBarView model dispatch ] @ selected)
            ])
    ] :> IView
