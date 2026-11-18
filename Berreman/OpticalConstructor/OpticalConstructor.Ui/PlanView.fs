/// The study's Plan step: Run plan with per-candidate progress and Cancel, the recoverability report — findings, rank,
/// confounded combinations, each target's bound and the instrument's states — and the ranked candidates with Add to kit
/// (001 §O; spec 0050 S14.2).
module OpticalConstructor.Ui.PlanView

open System.Globalization
open Avalonia
open Avalonia.Controls
open Avalonia.Controls.Primitives
open Avalonia.Layout
open Avalonia.Media
open Avalonia.FuncUI.Builder
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open OpticalConstructor.Controls
open OpticalConstructor.Domain.ItemModels
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.MuellerInverse
open OpticalConstructor.Domain.MuellerReconstruction
open OpticalConstructor.Domain.Studies
open OpticalConstructor.Domain.MeasurementPlanner
open OpticalConstructor.Optimization.PlannerInformation
open OpticalConstructor.Optimization.PlannerRuns
open OpticalConstructor.Ui.PlanJob
open OpticalConstructor.Ui.TableAndElementRotationView

type ControlCaption with

    /// The caption as a wrapping line of the plan (spec 0050 S14.2).
    member private this.planLine : IView =
        TextBlock.create [
            TextBlock.textWrapping TextWrapping.Wrap
            TextBlock.margin (Thickness(0.0, 0.0, 0.0, 2.0))
            TextBlock.text this.value
        ] :> IView

    /// The caption as a heading over one part of the plan (spec 0050 S14.2).
    member private this.planHeading : IView =
        TextBlock.create [
            TextBlock.fontWeight FontWeight.SemiBold
            TextBlock.margin (Thickness(0.0, 8.0, 0.0, 2.0))
            TextBlock.text this.value
        ] :> IView

/// A vertical list carrying `id`, one wrapping line per caption (spec 0050 S14.2).
let private planLines (id : ConstructorUiId) (captions : ControlCaption list) : IView =
    StackPanel.create [
        id.automationAttribute<StackPanel> ()
        StackPanel.orientation Orientation.Vertical
        StackPanel.children (captions |> List.map _.planLine)
    ] :> IView

type CombinationText with

    /// The combination as the Plan view lists it (spec 0050 S14.2).
    member private this.planCaption : ControlCaption = ControlCaption this.value

type PlanReport with

    /// One line per structural finding, then per plate the plan places no candidate on, then a missing polarimeter
    /// (spec 0050 S14.1, S14.2).
    member private this.planFindingCaptions : ControlCaption list =
        let findings = this.structural.findings |> List.map (fun (finding : PlannerFinding) -> ControlCaption finding.text)
        let unplaced =
            this.unplacedPlates
            |> List.map (fun (plate : PlannerPlate) ->
                ControlCaption $"{plate.label.value}: no principal axis lies on its normal, so the plan places no candidate on it.")
        let polarimeter =
            match this.polarimeterOpt with
            | Some _ -> []
            | None -> [ ControlCaption "The kit has no Mueller polarimeter, so the plan has no candidate." ]
        findings @ unplaced @ polarimeter

type PlannerInformation with

    /// `Rank r of n from what the kit holds`, with the condition at full rank, else the count confounded (spec 0050
    /// S14.2).
    member private this.planRankCaption : ControlCaption =
        let (MatrixRank rank) = this.rank
        let targets = this.coordinates.Length
        if rank < targets then
            ControlCaption $"Rank {rank} of {targets} from what the kit holds: {targets - rank} combination(s) confounded."
        else
            match this.conditionOpt with
            | Some (ConditionNumber kappa) ->
                let condition = kappa.ToString("F2", CultureInfo.InvariantCulture)
                ControlCaption $"Rank {rank} of {targets} from what the kit holds; condition {condition}."
            | None -> ControlCaption $"Rank {rank} of {targets} from what the kit holds."

    /// Each confounded combination over the targets, then the weakest identifiable one (spec 0050 S14.2).
    member private this.planConfoundedCaptions : ControlCaption list =
        let captionOf (direction : SingularDirection) : ControlCaption = (direction.text this.coordinates).planCaption
        let weakest =
            this.weakestOpt
            |> Option.map (fun (direction : SingularDirection) -> ControlCaption $"Weakest: {(captionOf direction).value}")
            |> Option.toList
        (this.confounded |> List.map captionOf) @ weakest

/// Target, Value, σ and σ / |value| per bound, the value the target's axis reads in the plan's nominal crystal, each
/// number in invariant culture to six significant digits (spec 0050 S14.2).
let private boundsGrid (report : PlanReport) (bounds : TargetBound list) : IView =
    let numberCaption (number : float) : ControlCaption = ControlCaption (number.ToString("G6", CultureInfo.InvariantCulture))
    let cell (row : int) (column : int) (weight : FontWeight) (caption : ControlCaption) : IView =
        TextBlock.create [
            TextBlock.row row
            TextBlock.column column
            TextBlock.fontWeight weight
            TextBlock.margin (Thickness(0.0, 0.0, 12.0, 2.0))
            TextBlock.text caption.value
        ] :> IView
    let axes = report.nominal.parameterAxes
    let rowOf (index : int) (bound : TargetBound) : IView list =
        let row = index + 1
        let (ParameterSigma sigma) = bound.sigma
        let valueOpt =
            axes
            |> List.tryFind (fun axis -> axis.name = bound.name)
            |> Option.map (fun axis -> axis.read report.nominal)
        let relative =
            match valueOpt with
            | Some value when value <> 0.0 -> numberCaption (sigma / abs value)
            | Some _
            | None -> ControlCaption "—"
        [
            cell row 0 FontWeight.Normal bound.name.caption
            cell row 1 FontWeight.Normal (valueOpt |> Option.map numberCaption |> Option.defaultValue (ControlCaption "—"))
            cell row 2 FontWeight.Normal (numberCaption sigma)
            cell row 3 FontWeight.Normal relative
        ]
    let headers =
        [ ControlCaption "Target"; ControlCaption "Value"; ControlCaption "σ"; ControlCaption "σ / value" ]
        |> List.mapi (fun column caption -> cell 0 column FontWeight.SemiBold caption)
    Grid.create [
        ConstructorUiId.planBoundsGrid.automationAttribute<Grid> ()
        Grid.margin (Thickness(0.0, 4.0, 0.0, 2.0))
        Grid.columnDefinitions "Auto,Auto,Auto,Auto"
        Grid.rowDefinitions (List.replicate (bounds.Length + 1) "Auto" |> String.concat ",")
        Grid.children (headers @ (bounds |> List.mapi rowOf |> List.concat))
    ] :> IView

type PlanReport with

    /// The committed report: findings, rank, confounded combinations, bounds and instrument states (spec 0050 S14.2).
    member private this.planReportRows : IView list =
        let information = this.baseline.information
        let instrument = this.instrument |> List.map (fun (finding : InstrumentFinding) -> ControlCaption finding.text)
        let bounds : IView =
            match this.baseline.boundsOpt with
            | Some held -> boundsGrid this held
            | None -> (ControlCaption "No bound yet: what the kit holds leaves a combination confounded.").planLine
        [
            (ControlCaption "What the kit can recover:").planHeading
            planLines ConstructorUiId.planFindingsList this.planFindingCaptions
            TextBlock.create [
                ConstructorUiId.planRankText.automationAttribute<TextBlock> ()
                TextBlock.textWrapping TextWrapping.Wrap
                TextBlock.margin (Thickness(0.0, 4.0, 0.0, 2.0))
                TextBlock.text information.planRankCaption.value
            ]
            planLines ConstructorUiId.planConfoundedList information.planConfoundedCaptions
            bounds
            (ControlCaption "Instrument states:").planHeading
            planLines ConstructorUiId.planInstrumentList instrument
        ]

type RankedPlanStep with

    /// `<caption> — rank <r>`, the criterion's measure, then each target's σ when the step bounds them; the list numbers
    /// it (spec 0050 S14.2).
    member private this.planStepCaption (criterion : PlanCriterion) : ControlCaption =
        let information = this.standing.information
        let (MatrixRank rank) = information.rank
        let measure : string =
            match criterion, information.traceOpt with
            | DeterminantPlanCriterion, _ ->
                let (InformationLogDeterminant logDeterminant) = information.logDeterminant
                let shown = logDeterminant.ToString("F3", CultureInfo.InvariantCulture)
                $", log det {shown}"
            | TargetTracePlanCriterion, Some (CovarianceTrace trace) ->
                let shown = trace.ToString("G4", CultureInfo.InvariantCulture)
                $", Σσ² {shown}"
            | TargetTracePlanCriterion, None -> ""
        let sigmas : string =
            match this.standing.boundsOpt with
            | Some bounds ->
                let each =
                    bounds
                    |> List.map (fun (bound : TargetBound) ->
                        let (ParameterSigma sigma) = bound.sigma
                        let shown = sigma.ToString("G3", CultureInfo.InvariantCulture)
                        $"{bound.name.value} {shown}")
                    |> String.concat ", "
                $"; σ: {each}"
            | None -> ""
        ControlCaption $"{this.candidate.caption} — rank {rank}{measure}{sigmas}"

/// The candidates the kit holds, then each ranked one, with Add to kit while the kit takes writes and lacks it; its place
/// in the kit is read now, so one added since the plan ran shows as in the kit (spec 0050 S14.2; .spec-md §10.4).
let private planCandidateRows
    (model : TableAndElementRotationModel)
    (report : PlanReport)
    (dispatch : TableAndElementRotationMsg -> unit)
    : IView list =
    let held =
        report.inKit
        |> List.map (fun (candidate : PlanCandidate) -> (ControlCaption $"Already in the kit: {candidate.caption}.").planLine)
    let rowOf (index : int) (step : RankedPlanStep) : IView =
        let candidate = step.candidate
        let id = ConstructorUiId.planCandidateRow candidate
        let ending : IView list =
            match candidate.coverageIn model.kitSetups model.experimentCollection.experiments, model.kitWrites with
            | CandidateInKit name, _ ->
                [
                    TextBlock.create [
                        TextBlock.verticalAlignment VerticalAlignment.Center
                        TextBlock.margin (Thickness(0.0, 0.0, 0.0, 4.0))
                        TextBlock.text $"In the kit as {name.value}."
                    ]
                ]
            | CandidateNotInKit, ControlOffered ->
                [
                    MaterialsWindowView.actionButton (ConstructorUiId.addPlanCandidateButton candidate) (ControlCaption "Add to kit")
                        MaterialsWindowView.positiveBackground (fun () -> dispatch (AddPlannedExperiment candidate))
                ]
            | CandidateNotInKit, ControlWithheld -> []
        let caption : IView =
            TextBlock.create [
                TextBlock.verticalAlignment VerticalAlignment.Center
                TextBlock.margin (Thickness(0.0, 0.0, 10.0, 4.0))
                TextBlock.text $"{index + 1}. {(step.planStepCaption report.criterion).value}"
            ]
        StackPanel.create [
            id.automationAttribute<StackPanel> ()
            StackPanel.orientation Orientation.Horizontal
            StackPanel.children (caption :: ending)
        ]
        |> View.withKey id.value
        :> IView
    let ranked : IView list =
        match report.ranked with
        | [] ->
            [
                (ControlCaption "No candidate is open: every candidate is in the kit, or the kit has no Mueller polarimeter or placed plate.")
                    .planLine
            ]
        | steps -> steps |> List.mapi rowOf
    [
        (ControlCaption "Next experiments, in the order the plan adds them:").planHeading
        StackPanel.create [
            ConstructorUiId.planCandidateList.automationAttribute<StackPanel> ()
            StackPanel.orientation Orientation.Vertical
            StackPanel.children (held @ ranked)
        ]
    ]

/// The study's crystal materials, the two criteria, the run row and its refusal, then the committed report and its
/// candidates (spec 0050 S14.2; .spec-md §10.2, §10.3, §10.5).
let private chosenStudyRows
    (model : TableAndElementRotationModel)
    (study : Study)
    (dispatch : TableAndElementRotationMsg -> unit)
    : IView list =
    let job = model.planJob.job
    let materialRow : IView =
        match model.planMaterials with
        | [] -> (ControlCaption "The study opens no crystal material: open one in Unknowns.").planLine
        | materials ->
            WrapPanel.create [
                WrapPanel.children (
                    materials
                    |> List.map (fun (material : OpenedMaterial) ->
                        let id = ConstructorUiId.planMaterialChoice material.version
                        let caption = ControlCaption $"{material.name.value} v{material.version.version.value}"
                        let choose () = dispatch (SetPlanMaterial material.version)
                        if model.planMaterialOpt = Some material.version then
                            MaterialsWindowView.actionButton id caption MaterialsWindowView.positiveBackground choose
                        else
                            MaterialsWindowView.verbButton id caption choose))
            ] :> IView
    let criterionRow : IView =
        WrapPanel.create [
            WrapPanel.children (
                [
                    DeterminantPlanCriterion, ConstructorUiId.planCriterionDeterminantChoice
                    TargetTracePlanCriterion, ConstructorUiId.planCriterionTraceChoice
                ]
                |> List.map (fun (criterion, id) ->
                    let caption = ControlCaption criterion.text
                    let choose () = dispatch (SetPlanCriterion criterion)
                    if model.planSurface.criterion = criterion then
                        MaterialsWindowView.actionButton id caption MaterialsWindowView.positiveBackground choose
                    else
                        MaterialsWindowView.verbButton id caption choose))
        ] :> IView
    let run : IView list =
        match model.planMaterialOpt with
        | None -> []
        | Some material ->
            let request : PlanRequest =
                {
                    study = study.name
                    material = material
                    criterion = model.planSurface.criterion
                }
            let button =
                MaterialsWindowView.actionButton ConstructorUiId.planRunButton (ControlCaption "Run plan")
                    MaterialsWindowView.positiveBackground (fun () -> dispatch (StudyPlan (RunPlan request)))
                :?> IView<Border>
            [ button |> View.withAttrs [ Border.isEnabled (not job.isRunning) ] :> IView ]
    let running : IView list =
        if job.isRunning then
            [
                MaterialsWindowView.actionButton ConstructorUiId.planCancelButton (ControlCaption "Cancel")
                    MaterialsWindowView.negativeBackground (fun () -> dispatch (StudyPlan CancelPlan))
                ProgressBar.create [
                    ConstructorUiId.planProgressBar.automationAttribute<ProgressBar> ()
                    ProgressBar.width 160.0
                    ProgressBar.height 12.0
                    ProgressBar.verticalAlignment VerticalAlignment.Center
                    ProgressBar.margin (Thickness(0.0, 0.0, 10.0, 4.0))
                    ProgressBar.minimum 0.0
                    ProgressBar.maximum 1.0
                    ProgressBar.isIndeterminate false
                    ProgressBar.value (job.progress |> Option.bind _.fraction |> Option.defaultValue 0.0)
                ]
                TextBlock.create [
                    ConstructorUiId.planProgressText.automationAttribute<TextBlock> ()
                    TextBlock.verticalAlignment VerticalAlignment.Center
                    TextBlock.margin (Thickness(0.0, 0.0, 0.0, 4.0))
                    TextBlock.text (job.progressText |> Option.defaultValue "Starting…")
                ]
            ]
        else
            []
    // Spec 0050 S14.2: actionButton keeps its first handler per key, so each request needs a row of its own.
    let runKey =
        let materialKey =
            match model.planMaterialOpt with
            | Some material -> $"{material.materialId.value}_v{material.version.value}"
            | None -> "none"
        $"PlanRun_{study.name.fileStem.value}_{materialKey}_{model.planSurface.criterion.text}"
    let runRow : IView list =
        match run @ running with
        | [] -> []
        | children ->
            [
                StackPanel.create [
                    StackPanel.orientation Orientation.Horizontal
                    StackPanel.margin (Thickness(0.0, 6.0, 0.0, 0.0))
                    StackPanel.children children
                ]
                |> View.withKey runKey
                :> IView
            ]
    let error : IView list =
        match model.planJob.errorOpt with
        | None -> []
        | Some refusal ->
            [
                TextBlock.create [
                    ConstructorUiId.planErrorText.automationAttribute<TextBlock> ()
                    TextBlock.textWrapping TextWrapping.Wrap
                    TextBlock.foreground (MaterialsWindowView.brush MaterialsWindowView.messageColor)
                    TextBlock.text refusal.describe
                ]
            ]
    let report : IView list =
        match model.planReportOpt with
        | Some held -> held.planReportRows @ planCandidateRows model held dispatch
        | None -> []
    [
        (ControlCaption "Material:").planHeading
        materialRow
        (ControlCaption "Rank candidates by:").planHeading
        criterionRow
    ]
    @ runRow
    @ error
    @ report

/// The Plan step over the window's model: the study bar, then the chosen study's plan controls, its committed report and
/// the report's candidates, scrolling both ways (001 §O; spec 0050 S14.2).
let planView (model : TableAndElementRotationModel) (dispatch : TableAndElementRotationMsg -> unit) : IView =
    let selected : IView list =
        match model.selectedStudyOpt with
        | None -> [ (ControlCaption "No study is chosen: choose one in Unknowns.").planLine ]
        | Some study -> chosenStudyRows model study dispatch
    ScrollViewer.create [
        // Spec 0050 S11.5: reserve scrollbar space so a row brought to the viewport edge remains clickable.
        AttrBuilder<ScrollViewer>.CreateProperty<bool>(ScrollViewer.AllowAutoHideProperty, false, ValueNone)
        ScrollViewer.horizontalScrollBarVisibility ScrollBarVisibility.Auto
        ScrollViewer.verticalScrollBarVisibility ScrollBarVisibility.Auto
        ScrollViewer.content (
            StackPanel.create [
                ConstructorUiId.planPanel.automationAttribute<StackPanel> ()
                StackPanel.orientation Orientation.Vertical
                StackPanel.margin 8.0
                StackPanel.children ([ StudyUnknownsView.studyBarView model dispatch ] @ selected)
            ])
    ] :> IView
