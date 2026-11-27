namespace OpticalConstructor.Ui.Tests

open System
open System.Collections.Concurrent
open System.Globalization
open System.Threading
open Avalonia.Automation
open Avalonia.Controls
open Avalonia.FuncUI.Hosts
open Avalonia.FuncUI.Elmish
open Avalonia.Layout
open Avalonia.Threading
open Avalonia.VisualTree
open Elmish
open Xunit
open Softellect.Sys.Primitives
open Berreman.Constants
open Berreman.Fields
open Berreman.Media
open OpticalConstructor.Controls
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.MeasurementPlanner
open OpticalConstructor.Domain.MuellerInverse
open OpticalConstructor.Optimization.PlannerInformation
open OpticalConstructor.Optimization.PlannerRuns
open OpticalConstructor.Storage.KitContentFiles
open OpticalConstructor.Tests
open OpticalConstructor.Ui
open OpticalConstructor.Ui.JobRunner
open OpticalConstructor.Ui.PlanJob
open OpticalConstructor.Ui.TableAndElementRotationView

/// The study's Plan step: its stable ids, the committed report shown under them, and Run plan with per-candidate
/// progress and Cancel through the shell's host (spec 0050 S14.2; 001 §O).
module PlanViewTests =

    let private owner : FolderName = FolderName "PlanViewTests"

    /// How long a real plan of the fixture may take: two candidates of sixteen configurations each.
    let private planWait : TimeSpan = TimeSpan.FromMinutes 5.0

    /// Shows `kitWindowView` over `model` headless at 1000 × 700, as `KitWindowTests` mounts it, runs `check`, then closes
    /// the window.
    let private withPlanWindow (model : TableAndElementRotationModel) (check : Window -> unit) : unit =
        let window = HostWindow()
        Program.mkSimple (fun () -> model) updateTableAndElementRotation KitWindowView.kitWindowView
        |> Program.withHost window
        |> Program.run
        window.Width <- 1000.0
        window.Height <- 700.0
        try
            window.Show()
            Dispatcher.UIThread.RunJobs()
            check window
        finally
            window.Close()
            Dispatcher.UIThread.RunJobs()

    /// The plan the model's Plan step would run: its chosen study and material by its chosen criterion.
    let private requestOf (model : TableAndElementRotationModel) : PlanRequest =
        match model.selectedStudyOpt, model.planMaterialOpt with
        | Some study, Some material ->
            {
                study = study.name
                material = material
                criterion = model.planSurface.criterion
            }
        | _ -> failwith "The plan fixture must choose a study that opens a crystal"

    /// Runs the model's plan through `PlanJobHost.posting` as `PlanJobTests` drives it: the worker posts each message as
    /// it comes, and once the plan has ended they are folded into the model in order, as Elmish dispatches them.
    let private plannedThroughPosting (model : TableAndElementRotationModel) : TableAndElementRotationModel =
        let posted = ConcurrentQueue<PlanMsg> ()
        // Never disposed: a plan that outlives the wait still sets it from its worker.
        let ended = new ManualResetEventSlim (false)
        let host =
            PlanJobHost.posting
                (fun action -> action ())
                (fun progress -> posted.Enqueue (PlanProgressed progress))
                (fun result ->
                    posted.Enqueue (PlanEnded result)
                    ended.Set ())
        let hosted =
            {
                model with
                    kitShell =
                        {
                            model.kitShell with planRuns = host
                        }
            }
        let started = hosted |> updateTableAndElementRotation (StudyPlan (RunPlan (requestOf hosted)))
        Assert.True(started.planJob.job.isRunning, "Run plan must start the plan")
        Assert.True(ended.Wait planWait, $"the plan did not end within {planWait}")
        posted |> Seq.fold (fun state message -> state |> updateTableAndElementRotation (StudyPlan message)) started

    /// The lines of the vertical list carrying `id`, in order.
    let private linesOf (window : Window) (id : ConstructorUiId) : string list =
        Assert.IsType<StackPanel>(UiDriver.ById.findControl window id).Children
        |> Seq.map (fun line -> Assert.IsType<TextBlock>(line).Text)
        |> List.ofSeq

    /// Every text shown under `root`, a window or one row of it; a block holding no text is skipped.
    let private textsOf (root : Avalonia.Visual) : string list =
        root.GetVisualDescendants() |> Seq.choose (function :? TextBlock as text -> Option.ofObj text.Text | _ -> None) |> List.ofSeq

    /// The list carrying `planCandidateList`.
    let private candidateListOf (window : Window) : StackPanel =
        Assert.IsType<StackPanel>(UiDriver.ById.findControl window ConstructorUiId.planCandidateList)

    /// The ids the list's children carry, in order; a plain line carries none.
    let private childIdsOf (list : StackPanel) : string list =
        list.Children |> Seq.map (fun child -> AutomationProperties.GetAutomationId child |> Option.ofObj |> Option.defaultValue "") |> List.ofSeq

    /// The caption of the row carrying `id`, its first child.
    /// A candidate row's caption, the DockPanel's filling last child beside its docked verb (spec 0051 `.manual/003` 4.3).
    let private rowCaptionOf (window : Window) (id : ConstructorUiId) : string =
        let row = Assert.IsType<DockPanel>(UiDriver.ById.findControl window id)
        Assert.IsType<TextBlock>(row.Children[row.Children.Count - 1]).Text

    /// The brief's row text for the ranked step at `position` by `criterion`, written apart from the view.
    let private expectedRowText (position : int) (criterion : PlanCriterion) (step : RankedPlanStep) : string =
        let invariant (format : string) (number : float) : string = number.ToString(format, CultureInfo.InvariantCulture)
        let information = step.standing.information
        let (MuellerReconstruction.MatrixRank rank) = information.rank
        let measure =
            match criterion, information.traceOpt with
            | DeterminantPlanCriterion, _ ->
                let shown = invariant "F3" information.logDeterminant.value
                $", log det {shown}"
            | TargetTracePlanCriterion, Some trace ->
                let shown = invariant "G4" trace.value
                $", Σσ² {shown}"
            | TargetTracePlanCriterion, None -> ""
        let sigmas =
            match step.standing.boundsOpt with
            | Some bounds ->
                let each =
                    bounds
                    |> List.map (fun (bound : TargetBound) ->
                        let shown = invariant "G3" bound.sigma.value
                        $"{bound.name.value} {shown}")
                    |> String.concat ", "
                $"; σ: {each}"
            | None -> ""
        $"{position}. {step.candidate.caption} — rank {rank}{measure}{sigmas}"

    [<Fact>]
    let ``the plan view UiIds are the stable intent-named ids`` () : unit =
        Assert.Equal("FlowPlanStep", ConstructorUiId.flowPlanStep.value)
        Assert.Equal("PlanPanel", ConstructorUiId.planPanel.value)
        Assert.Equal("PlanRunButton", ConstructorUiId.planRunButton.value)
        Assert.Equal("PlanCriterionDeterminantChoice", ConstructorUiId.planCriterionDeterminantChoice.value)
        Assert.Equal("PlanCriterionTraceChoice", ConstructorUiId.planCriterionTraceChoice.value)
        Assert.Equal("PlanProgressBar", ConstructorUiId.planProgressBar.value)
        Assert.Equal("PlanProgressText", ConstructorUiId.planProgressText.value)
        Assert.Equal("PlanCancelButton", ConstructorUiId.planCancelButton.value)
        Assert.Equal("PlanErrorText", ConstructorUiId.planErrorText.value)
        Assert.Equal("PlanFindingsList", ConstructorUiId.planFindingsList.value)
        Assert.Equal("PlanRankText", ConstructorUiId.planRankText.value)
        Assert.Equal("PlanConfoundedList", ConstructorUiId.planConfoundedList.value)
        Assert.Equal("PlanBoundsGrid", ConstructorUiId.planBoundsGrid.value)
        Assert.Equal("PlanInstrumentList", ConstructorUiId.planInstrumentList.value)
        Assert.Equal("PlanCandidateList", ConstructorUiId.planCandidateList.value)
        let version = MaterialVersionId.firstOf (MaterialId (Guid.Parse "49000000-0000-4000-8000-0000000000aa"))
        Assert.Equal("PlanMaterial_49000000-0000-4000-8000-0000000000aa_v1", (ConstructorUiId.planMaterialChoice version).value)
        let source : PlannerSource =
            {
                item = ItemId (Guid.Parse "49000000-0000-4000-8000-0000000000ab")
                label = ItemLabel "Laser"
                waveLength = WaveLength.nm 532.0<nm>
                tunableLimitsOpt = None
            }
        let plate : PlannerPlate =
            {
                item = ItemId (Guid.Parse "49000000-0000-4000-8000-0000000000ac")
                label = ItemLabel "QZ-Z"
                sample = Library.SampleVersionId.firstOf (Library.SampleId (Guid.Parse "49000000-0000-4000-8000-0000000000ad"))
                thickness = Thickness.mm 0.5<mm>
                substrate = Library.ThickPlate
                emission = Placement.EmitBoth
                normalAxisOpt = Some PlannerSymmetry.ThirdCrystalAxis
            }
        let reflected : PlanCandidate =
            {
                source = source
                plate = plate
                cut = ZCut
                observable = ReflectedMueller
            }
        Assert.Equal("PlanCandidate_49000000-0000-4000-8000-0000000000ac_r_532.0", (ConstructorUiId.planCandidateRow reflected).value)
        Assert.Equal(
            "AddPlanCandidate_49000000-0000-4000-8000-0000000000ac_r_532.0",
            (ConstructorUiId.addPlanCandidateButton reflected).value)
        Assert.Equal(
            "PlanCandidate_49000000-0000-4000-8000-0000000000ac_t_532.0",
            (ConstructorUiId.planCandidateRow { reflected with observable = TransmittedMueller }).value)

    [<Fact>]
    let ``the Plan step shows a plan's findings, rank, confounded combinations, bounds and instrument choices`` () : unit =
        HeadlessSession.run (fun () ->
            let model = TestAppScope.planKitIn owner
            try
                let planned = plannedThroughPosting model
                Assert.Equal<PlannerError option>(None, planned.planJob.errorOpt)
                Assert.Equal<JobStatus>(Completed, planned.planJob.job.status)
                let report =
                    planned.planReportOpt |> Option.defaultWith (fun () -> failwith "The completed plan must be the report shown")
                let offAxis =
                    match report.structural.offAxisCutOpt with
                    | Some need -> (OffAxisCutFinding need).text
                    | None -> failwith "A kit of one z-cut plate needs an off-axis cut"
                Assert.StartsWith("You need an off-axis cut: g33 lie along the normal of every plate", offAxis)
                let information = report.baseline.information
                let confounded =
                    information.confounded |> List.map (fun (direction : SingularDirection) -> (direction.text information.coordinates).value)
                Assert.Equal<string list>([ "+1.00 g11"; "+1.00 g33" ], confounded)
                withPlanWindow (planned |> updateTableAndElementRotation (ShowFlowStep PlanFlowStep)) (fun window ->
                    UiDriver.ById.findControl window ConstructorUiId.planPanel |> ignore
                    let findings = linesOf window ConstructorUiId.planFindingsList
                    Assert.Contains(offAxis, findings)
                    Assert.Equal<string list>(report.structural.findings |> List.map (fun (finding : PlannerFinding) -> finding.text), findings)
                    Assert.Equal(
                        "Rank 0 of 2 from what the kit holds: 2 combination(s) confounded.",
                        Assert.IsType<TextBlock>(UiDriver.ById.findControl window ConstructorUiId.planRankText).Text)
                    Assert.Equal<string list>(confounded, linesOf window ConstructorUiId.planConfoundedList)
                    UiDriver.ById.assertAbsent window ConstructorUiId.planBoundsGrid
                    Assert.Contains("No bound yet: what the kit holds leaves a combination confounded.", textsOf window)
                    Assert.Empty(linesOf window ConstructorUiId.planInstrumentList)
                    UiDriver.ById.assertAbsent window ConstructorUiId.planErrorText
                    UiDriver.ById.assertAbsent window ConstructorUiId.planProgressBar)
                // The same report at its last ranked step's standing: full rank, so a condition, the weakest combination
                // and a bound per target.
                let standing = (List.last report.ranked).standing
                let bounds = standing.boundsOpt |> Option.defaultWith (fun () -> failwith "Both candidates must bound both targets")
                let fullRank =
                    {
                        planned with
                            planJob =
                                {
                                    planned.planJob with
                                        job =
                                            {
                                                planned.planJob.job with committedResults = Some { report with baseline = standing }
                                            }
                                }
                            flowStep = PlanFlowStep
                    }
                let full = standing.information
                let condition =
                    match full.conditionOpt with
                    | Some (ConditionNumber kappa) -> kappa.ToString("F2", CultureInfo.InvariantCulture)
                    | None -> failwith "A full-rank standing must have a finite condition"
                let weakest =
                    match full.weakestOpt with
                    | Some direction -> (direction.text full.coordinates).value
                    | None -> failwith "A full-rank standing must have a weakest combination"
                let numberText (number : float) : string = number.ToString("G6", CultureInfo.InvariantCulture)
                withPlanWindow fullRank (fun window ->
                    Assert.Equal(
                        $"Rank 2 of 2 from what the kit holds; condition {condition}.",
                        Assert.IsType<TextBlock>(UiDriver.ById.findControl window ConstructorUiId.planRankText).Text)
                    Assert.Equal<string list>([ $"Weakest: {weakest}" ], linesOf window ConstructorUiId.planConfoundedList)
                    let grid = Assert.IsType<Grid>(UiDriver.ById.findControl window ConstructorUiId.planBoundsGrid)
                    let cellText (row : int) (column : int) : string =
                        let cell = grid.Children |> Seq.find (fun cell -> Grid.GetRow cell = row && Grid.GetColumn cell = column)
                        Assert.IsType<TextBlock>(cell).Text
                    Assert.Equal<string list>([ "Target"; "Value"; "σ"; "σ / value" ], [ for column in 0 .. 3 -> cellText 0 column ])
                    Assert.Equal<string list>([ "g11"; "3E-05" ], [ cellText 1 0; cellText 1 1 ])
                    Assert.Equal<string list>([ "g33"; "-1E-05" ], [ cellText 2 0; cellText 2 1 ])
                    for row, (bound : TargetBound), value in List.zip3 [ 1; 2 ] bounds [ 3.0e-5; -1.0e-5 ] do
                        let (ParameterSigma sigma) = bound.sigma
                        Assert.Equal(numberText sigma, cellText row 2)
                        Assert.Equal(numberText (sigma / abs value), cellText row 3)
                    Assert.DoesNotContain("No bound yet: what the kit holds leaves a combination confounded.", textsOf window))
            finally model.kit.close () |> SceneTestFixture.kitSuccess)

    [<Fact>]
    let ``the Plan step ranks the candidates and offers Add to kit for each the kit does not hold`` () : unit =
        HeadlessSession.run (fun () ->
            let model = TestAppScope.planKitIn owner
            try
                let planned = plannedThroughPosting model |> updateTableAndElementRotation (ShowFlowStep PlanFlowStep)
                let report =
                    planned.planReportOpt |> Option.defaultWith (fun () -> failwith "The completed plan must be the report shown")
                Assert.Equal(DeterminantPlanCriterion, report.criterion)
                Assert.Empty report.inKit
                // QZ-Z's two candidates, its transmitted and its reflected branch, both open and both ranked.
                let first, second =
                    match report.ranked with
                    | [ first; second ] -> first, second
                    | _ -> failwith "The plan must rank QZ-Z's two candidates"
                Assert.All([ first; second ], fun (step : RankedPlanStep) -> Assert.Equal("QZ-Z", step.candidate.plate.label.value))
                Assert.NotEqual(first.candidate.observable, second.candidate.observable)
                let rowIds = [ first; second ] |> List.map (fun (step : RankedPlanStep) -> (ConstructorUiId.planCandidateRow step.candidate).value)
                let noneOpen = "No candidate is open: every candidate is in the kit, or the kit has no Mueller polarimeter or placed plate."
                withPlanWindow planned (fun window ->
                    let list = candidateListOf window
                    Assert.Equal<string list>(rowIds, childIdsOf list)
                    let firstRow = rowCaptionOf window (ConstructorUiId.planCandidateRow first.candidate)
                    Assert.StartsWith("1. QZ-Z (z-cut plate), ", firstRow)
                    Assert.Contains("rank", firstRow)
                    [ first; second ]
                    |> List.iteri (fun (index : int) (step : RankedPlanStep) ->
                        let rowId = ConstructorUiId.planCandidateRow step.candidate
                        let addId = ConstructorUiId.addPlanCandidateButton step.candidate
                        let row = Assert.IsType<DockPanel>(UiDriver.ById.findControl window rowId)
                        Assert.Equal(expectedRowText (index + 1) DeterminantPlanCriterion step, rowCaptionOf window rowId)
                        Assert.Same(row, (UiDriver.ById.findControl window addId).Parent.Parent)
                        Assert.Equal("Add to kit", UiDriver.ById.captionOf window addId))
                    Assert.DoesNotContain(noneOpen, textsOf window))

                // Over the kit opened read-only the rows stay and no Add to kit is offered.
                let readOnly =
                    {
                        planned with
                            kit =
                                {
                                    planned.kit with access = ReadOnlyKitAccess
                                }
                    }
                withPlanWindow readOnly (fun window ->
                    Assert.Equal<string list>(rowIds, childIdsOf (candidateListOf window))
                    for step in [ first; second ] do
                        UiDriver.ById.assertAbsent window (ConstructorUiId.addPlanCandidateButton step.candidate)
                    Assert.DoesNotContain(textsOf window, fun (text : string) -> text.StartsWith("In the kit as ", StringComparison.Ordinal)))

                // The view shows what the report holds: its held candidates first, its criterion's measure, and the line
                // for a report with no open candidate.
                let showing (shown : PlanReport) : TableAndElementRotationModel =
                    {
                        planned with
                            planJob =
                                {
                                    planned.planJob with
                                        job =
                                            {
                                                planned.planJob.job with committedResults = Some shown
                                            }
                                }
                    }
                let traced =
                    {
                        report with
                            criterion = TargetTracePlanCriterion
                            inKit = [ first.candidate ]
                            ranked = [ second ]
                    }
                withPlanWindow (showing traced) (fun window ->
                    let list = candidateListOf window
                    Assert.Equal<string list>([ ""; (ConstructorUiId.planCandidateRow second.candidate).value ], childIdsOf list)
                    Assert.Equal($"Already in the kit: {first.candidate.caption}.", Assert.IsType<TextBlock>(list.Children[0]).Text)
                    let row = rowCaptionOf window (ConstructorUiId.planCandidateRow second.candidate)
                    Assert.Equal(expectedRowText 1 TargetTracePlanCriterion second, row)
                    Assert.Contains(", Σσ² ", row)
                    Assert.Contains("; σ: g11 ", row))
                withPlanWindow (showing { report with ranked = [] }) (fun window ->
                    Assert.Equal<string list>([ noneOpen ], linesOf window ConstructorUiId.planCandidateList))
            finally model.kit.close () |> SceneTestFixture.kitSuccess)

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``pressing Run plan starts one plan through the shell's host, and Cancel cancels it`` () : unit =
        HeadlessSession.run (fun () ->
            let model = TestAppScope.planKitIn owner
            try
                let jobs = ResizeArray<PlanObserver -> Result<PlanReport, PlannerError>> ()
                let cancels = ResizeArray<unit> ()
                let host : PlanJobHost =
                    {
                        startPlan =
                            fun plan ->
                                jobs.Add plan
                                {
                                    cancelPlan = fun () -> cancels.Add ()
                                }
                    }
                let hosted =
                    {
                        model with
                            kitShell =
                                {
                                    model.kitShell with planRuns = host
                                }
                    }
                withPlanWindow hosted (fun window ->
                    UiDriver.ById.assertAbsent window ConstructorUiId.planPanel
                    UiDriver.ById.clickOn window ConstructorUiId.flowPlanStep
                    UiDriver.ById.findControl window ConstructorUiId.planPanel |> ignore
                    UiDriver.ById.assertAbsent window ConstructorUiId.planProgressBar
                    UiDriver.ById.assertAbsent window ConstructorUiId.planCancelButton

                    UiDriver.ById.clickOn window ConstructorUiId.planRunButton
                    Assert.Single jobs |> ignore
                    Assert.Empty cancels
                    let run = UiDriver.ById.findControl window ConstructorUiId.planRunButton
                    Assert.False(run.IsEffectivelyEnabled, "Run plan must be disabled while its plan runs")
                    let row = Assert.IsType<WrapPanel>(run.Parent)
                    Assert.Equal(Orientation.Horizontal, row.Orientation)
                    let bar = Assert.IsType<ProgressBar>(UiDriver.ById.findControl window ConstructorUiId.planProgressBar)
                    let cancel = UiDriver.ById.findControl window ConstructorUiId.planCancelButton
                    Assert.Same(row, bar.Parent)
                    Assert.Same(row, cancel.Parent)
                    Assert.False(bar.IsIndeterminate, "the plan's bar counts its candidates")
                    // .spec-md §10.5: the opposing pair shares the row, Cancel right of Run plan past a gap.
                    let runRight = (SlotBarTests.topLeftOf window run).X + run.Bounds.Width
                    Assert.True((SlotBarTests.topLeftOf window cancel).X > runRight, "Cancel must stand right of Run plan, past a gap")
                    Assert.Equal("Cancel", UiDriver.ById.captionOf window ConstructorUiId.planCancelButton)

                    UiDriver.ById.clickOn window ConstructorUiId.planCancelButton
                    Assert.Single cancels |> ignore
                    Assert.Single jobs |> ignore)
            finally model.kit.close () |> SceneTestFixture.kitSuccess)

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``pressing Add to kit records a planned experiment of the candidate in the kit`` () : unit =
        HeadlessSession.run (fun () ->
            let model = TestAppScope.planKitIn owner
            try
                let planned = plannedThroughPosting model |> updateTableAndElementRotation (ShowFlowStep PlanFlowStep)
                let report =
                    planned.planReportOpt |> Option.defaultWith (fun () -> failwith "The completed plan must be the report shown")
                let candidateOn (observable : Observable) : PlanCandidate =
                    report.ranked
                    |> List.map (fun (step : RankedPlanStep) -> step.candidate)
                    |> List.find (fun (candidate : PlanCandidate) -> candidate.observable = observable)
                let transmitted = candidateOn TransmittedMueller
                let reflected = candidateOn ReflectedMueller
                let stemsOf (kind : KitFileKind) : LibraryEntryFiles.FileStem list =
                    model.kit.proxy.listKindFiles kind |> SceneTestFixture.kitSuccess
                let setupsBefore = stemsOf SetupKitFile
                Assert.Empty(stemsOf ExperimentKitFile)
                withPlanWindow planned (fun window ->
                    UiDriver.ById.clickOn window (ConstructorUiId.addPlanCandidateButton transmitted)

                    // The kit's folder holds one more setup and one planned experiment of the candidate.
                    Assert.Equal(setupsBefore.Length + 1, (stemsOf SetupKitFile).Length)
                    Assert.Single(stemsOf ExperimentKitFile) |> ignore
                    let experiment = Assert.Single(model.kit.proxy.readExperiments () |> SceneTestFixture.kitSuccess)
                    Assert.Equal(Experiments.PlannedRun, experiment.role)
                    let setups = model.kit.proxy.readSetups () |> SceneTestFixture.kitSuccess
                    Assert.Equal(CandidateInKit experiment.name, transmitted.coverageIn setups [ experiment ])

                    // Its row now reads it in the kit, with no new plan run; the other candidate still offers Add to kit.
                    let row = UiDriver.ById.findControl window (ConstructorUiId.planCandidateRow transmitted)
                    let rowTexts = textsOf row
                    Assert.Contains(rowTexts, fun (text : string) -> text.StartsWith("In the kit as ", StringComparison.Ordinal))
                    Assert.Contains($"In the kit as {experiment.name.value}.", rowTexts)
                    UiDriver.ById.assertAbsent window (ConstructorUiId.addPlanCandidateButton transmitted)
                    Assert.Equal("Add to kit", UiDriver.ById.captionOf window (ConstructorUiId.addPlanCandidateButton reflected)))
            finally model.kit.close () |> SceneTestFixture.kitSuccess)
