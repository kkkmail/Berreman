/// The step-by-step guides: building a direct problem (the `multilayer-thin-film` kit) and an inverse one (the
/// `optics-mueller` kit), each step naming its page and its button (spec 0051 `.manual/007` 9.2, `006-9`, `006-14`).
module OpticalConstructor.Ui.GuideView

open Avalonia
open Avalonia.Controls
open Avalonia.Controls.Primitives
open Avalonia.Layout
open Avalonia.Media
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open OpticalConstructor.Controls
open OpticalConstructor.Ui.TableAndElementRotationView

/// One step of a guide: the page it is done on, none for the Launcher or the start screen, the command it presses, and
/// what it does and writes into the kit.
type GuideStep =
    {
        pageOpt : ConstructorFlowStep option
        command : ConstructorUiId
        text : ControlCaption
    }

/// Which guide the window shows.
type GuideKind =
    | DirectProblemGuide
    | InverseProblemGuide

    member this.caption : ControlCaption =
        match this with
        | DirectProblemGuide -> ControlCaption "Build a direct problem"
        | InverseProblemGuide -> ControlCaption "Build an inverse problem"

    member this.choiceUiId : ConstructorUiId =
        match this with
        | DirectProblemGuide -> ConstructorUiId.guideDirectChoice
        | InverseProblemGuide -> ConstructorUiId.guideInverseChoice

    /// The constructor the guide is done in.
    member this.mode : ConstructorMode =
        match this with
        | DirectProblemGuide -> ForwardConstructor
        | InverseProblemGuide -> InverseConstructor

    /// The guide's steps, in order.
    member this.steps : GuideStep list =
        let step (pageOpt : ConstructorFlowStep option) (command : ConstructorUiId) (text : string) : GuideStep =
            { pageOpt = pageOpt; command = command; text = ControlCaption text }
        match this with
        | DirectProblemGuide ->
            [
                step None ConstructorUiId.openMainButton "On the Launcher, press Main. On the start screen, New kit… makes a kit in an empty folder; its .optics-kit file is written there."
                step (Some ItemsFlowStep) ConstructorUiId.newItemButton "Items → New item…: the library opens in its own window. Choose \"Xenon lamp + monochromator, 190–1000 nm\" and press Choose, give it a label, and Create. Do the same for a linear polarizer, the sample \"Quarter-wave glass n = 1.75 / 1.50 multilayer (100 pairs) on glass\", and two detectors."
                step (Some SetupsFlowStep) ConstructorUiId.newSetupButton "Setups → New setup: name it. The table starts with a light source and a detector; add a polarizer, a sample and a second detector with the Add tab, and put each item in its slot with the chips above the table: one detector on the reflected ray, one on the transmitted ray. Create writes the setup."
                step (Some ExperimentsFlowStep) ConstructorUiId.newExperimentButton "Experiments, with the setup chosen on Setups → New experiment: choose the sample, vary its incidence R2 from 0 to 89°, capture both rays, 90 points, at 600 nm. Create writes it. Make a second one varying the wavelength from 500 to 700 nm."
                step (Some ExperimentsFlowStep) (ConstructorUiId.experimentViewButton (ConstructorUiKey "1")) "Experiments: press an experiment, then View chart ↗: its chart window shows R and T; its Export CSV… saves the numbers."
            ]
        | InverseProblemGuide ->
            [
                step None ConstructorUiId.openInverseButton "On the Launcher, press Inverse, then Open kit… and choose the kit's .optics-kit file."
                step (Some ItemsFlowStep) ConstructorUiId.pageEditButton "Items: the instruments and samples. Press an item, Edit, and press each parameter to fit: make it Unknown, with its bounds. Save writes the item."
                step (Some SetupsFlowStep) ConstructorUiId.newSetupButton "Setups: one setup per arrangement you measured; the naming template names it from its slots."
                step (Some ExperimentsFlowStep) ConstructorUiId.newExperimentButton "Experiments: one experiment per measured run, over its setup: what was varied, over which range, and which ray was captured."
                step (Some DataFlowStep) (ConstructorUiId.attachDataFile (OpticalConstructor.Domain.Experiments.ExperimentId 1)) "Data: press an experiment in the matrix, Edit, then Attach data file… to map its lab file through an import profile; or Split a file by key… for one file holding many runs. Exclude bad points with their reason; Save writes the exclusions. The lab file is never changed."
                step (Some UnknownsFlowStep) ConstructorUiId.newStudyButton "Unknowns → New study: name it and choose the experiments to fit. Then press the study, Edit: open materials, set each unknown's scope and link, and the sessions' gains."
                step (Some PlanFlowStep) ConstructorUiId.planRunButton "Plan, for a kit with a Mueller polarimeter: what the study can recover, and the experiments that would recover more; Add to kit adds one."
                step (Some FitFlowStep) ConstructorUiId.newFitRunButton "Fit → New fit run…: a staged fit runs stage 1 on the reference (AIR) runs to calibrate the instruments, then stage 2 on the samples; a joint fit runs everything at once. Every run is kept."
                step (Some ResultsFlowStep) ConstructorUiId.fitResultsValuesTab "Results: the chosen run's fitted values, σ and correlations; every table copies as CSV."
                step (Some KitFlowStep) ConstructorUiId.publishCalibrationButton "Kit → Publish: press the fit run, then Publish calibration writes its calibrated values into the items as their next revision, and Promote makes a new library version of a fitted material."
            ]

/// The guide window's view: the two guides as choices, then the chosen one's steps, numbered and copyable (spec 0051
/// `.manual/007` 9.2).
let guideView (shown : GuideKind) (choose : GuideKind -> unit) : IView =
    let steps =
        shown.steps
        |> List.mapi (fun index step ->
            let page =
                match step.pageOpt with
                | Some page -> $"[{page.caption.value}] "
                | None -> ""
            PageControls.copyableLine (ControlCaption $"{index + 1}. {page}{step.text.value}") [ SelectableTextBlock.margin (Thickness(0.0, 0.0, 0.0, 8.0)) ])
    DockPanel.create [
        DockPanel.margin (Thickness 12.0)
        DockPanel.children [
            WrapPanel.create [
                DockPanel.dock Dock.Top
                WrapPanel.children (
                    [ DirectProblemGuide; InverseProblemGuide ]
                    |> List.map (fun kind -> CommandControls.choiceCommand kind.choiceUiId kind.caption (if kind = shown then ChosenChoice else UnchosenChoice) (fun () -> choose kind)))
            ]
            ScrollViewer.create [
                ScrollViewer.verticalScrollBarVisibility ScrollBarVisibility.Auto
                ScrollViewer.content (
                    StackPanel.create [
                        yield! ConstructorUiId.guideSteps.groupAttributes<StackPanel> ()
                        StackPanel.orientation Orientation.Vertical
                        StackPanel.margin (Thickness(0.0, 8.0, 0.0, 0.0))
                        StackPanel.children steps
                    ])
            ]
        ]
    ]
    :> IView
