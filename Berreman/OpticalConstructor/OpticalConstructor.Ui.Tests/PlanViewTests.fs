namespace OpticalConstructor.Ui.Tests

open System
open System.Collections.Concurrent
open System.Globalization
open System.Threading
open Avalonia.Controls
open Avalonia.FuncUI.Hosts
open Avalonia.FuncUI.Elmish
open Avalonia.Layout
open Avalonia.Threading
open Avalonia.VisualTree
open Elmish
open Xunit
open Softellect.Sys.Primitives
open OpticalConstructor.Controls
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.MeasurementPlanner
open OpticalConstructor.Domain.MuellerInverse
open OpticalConstructor.Optimization.PlannerInformation
open OpticalConstructor.Optimization.PlannerRuns
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
        Assert.IsType<StackPanel>(SlotBarTests.controlById window id).Children
        |> Seq.map (fun line -> Assert.IsType<TextBlock>(line).Text)
        |> List.ofSeq

    /// Every text the window shows.
    let private textsOf (window : Window) : string list =
        window.GetVisualDescendants() |> Seq.choose (function :? TextBlock as text -> Some text.Text | _ -> None) |> List.ofSeq

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
        let version = MaterialVersionId.firstOf (MaterialId (Guid.Parse "49000000-0000-4000-8000-0000000000aa"))
        Assert.Equal("PlanMaterial_49000000-0000-4000-8000-0000000000aa_v1", (ConstructorUiId.planMaterialChoice version).value)

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
                    SlotBarTests.controlById window ConstructorUiId.planPanel |> ignore
                    let findings = linesOf window ConstructorUiId.planFindingsList
                    Assert.Contains(offAxis, findings)
                    Assert.Equal<string list>(report.structural.findings |> List.map (fun (finding : PlannerFinding) -> finding.text), findings)
                    Assert.Equal(
                        "Rank 0 of 2 from what the kit holds: 2 combination(s) confounded.",
                        Assert.IsType<TextBlock>(SlotBarTests.controlById window ConstructorUiId.planRankText).Text)
                    Assert.Equal<string list>(confounded, linesOf window ConstructorUiId.planConfoundedList)
                    SlotBarTests.assertAbsent window ConstructorUiId.planBoundsGrid
                    Assert.Contains("No bound yet: what the kit holds leaves a combination confounded.", textsOf window)
                    Assert.Empty(linesOf window ConstructorUiId.planInstrumentList)
                    SlotBarTests.assertAbsent window ConstructorUiId.planErrorText
                    SlotBarTests.assertAbsent window ConstructorUiId.planProgressBar)
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
                        Assert.IsType<TextBlock>(SlotBarTests.controlById window ConstructorUiId.planRankText).Text)
                    Assert.Equal<string list>([ $"Weakest: {weakest}" ], linesOf window ConstructorUiId.planConfoundedList)
                    let grid = Assert.IsType<Grid>(SlotBarTests.controlById window ConstructorUiId.planBoundsGrid)
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
                    SlotBarTests.assertAbsent window ConstructorUiId.planPanel
                    SlotBarTests.pressBox window ConstructorUiId.flowPlanStep
                    SlotBarTests.controlById window ConstructorUiId.planPanel |> ignore
                    SlotBarTests.assertAbsent window ConstructorUiId.planProgressBar
                    SlotBarTests.assertAbsent window ConstructorUiId.planCancelButton

                    SlotBarTests.pressBox window ConstructorUiId.planRunButton
                    Assert.Single jobs |> ignore
                    Assert.Empty cancels
                    let run = SlotBarTests.controlById window ConstructorUiId.planRunButton
                    Assert.False(run.IsEffectivelyEnabled, "Run plan must be disabled while its plan runs")
                    let row = Assert.IsType<StackPanel>(run.Parent)
                    Assert.Equal(Orientation.Horizontal, row.Orientation)
                    let bar = Assert.IsType<ProgressBar>(SlotBarTests.controlById window ConstructorUiId.planProgressBar)
                    let cancel = SlotBarTests.controlById window ConstructorUiId.planCancelButton
                    Assert.Same(row, bar.Parent)
                    Assert.Same(row, cancel.Parent)
                    Assert.False(bar.IsIndeterminate, "the plan's bar counts its candidates")
                    // .spec-md §10.5: the opposing pair shares the row, Cancel right of Run plan past a gap.
                    let runRight = (SlotBarTests.topLeftOf window run).X + run.Bounds.Width
                    Assert.True((SlotBarTests.topLeftOf window cancel).X > runRight, "Cancel must stand right of Run plan, past a gap")
                    Assert.Equal("Cancel", SlotBarTests.boxLabelOf window ConstructorUiId.planCancelButton)

                    SlotBarTests.pressBox window ConstructorUiId.planCancelButton
                    Assert.Single cancels |> ignore
                    Assert.Single jobs |> ignore)
            finally model.kit.close () |> SceneTestFixture.kitSuccess)
