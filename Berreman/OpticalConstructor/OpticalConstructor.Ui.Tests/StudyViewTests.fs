namespace OpticalConstructor.Ui.Tests

open System.IO
open Avalonia
open Avalonia.Automation
open Avalonia.Controls
open Avalonia.FuncUI.Hosts
open Avalonia.FuncUI.Elmish
open Avalonia.Headless
open Avalonia.Input
open Avalonia.Media
open Avalonia.Layout
open Avalonia.Threading
open Avalonia.VisualTree
open Elmish
open Xunit
open Softellect.Sys.Primitives
open OpticalConstructor.Controls
open OpticalConstructor.Domain.ExperimentData
open OpticalConstructor.Domain.Experiments
open OpticalConstructor.Domain.FitRuns
open OpticalConstructor.Domain.ImportProfiles
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.ItemModels
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.MeasuredData
open OpticalConstructor.Domain.MuellerInverse
open OpticalConstructor.Domain.MuellerReconstruction
open OpticalConstructor.Domain.PrefitChecks
open OpticalConstructor.Domain.Studies
open OpticalConstructor.Domain.StudyPublishing
open OpticalConstructor.Storage.KitContentFiles
open OpticalConstructor.Tests
open OpticalConstructor.Ui
open OpticalConstructor.Ui.TableAndElementRotationView
open OpticalConstructor.Ui.StudyFitJob
open OpticalConstructor.Ui.JobRunner
open OpticalConstructor.Optimization.OptimizationInterface
open OpticalConstructor.Optimization.StudyEngine

/// The study's Unknowns, Fit, Results and Publish controls, driven by stable IDs (spec 0050 S11.5).
module StudyViewTests =

    let private owner = FolderName "StudyViewTests"
    let private retardance = ParameterName "c2.retardance"

    let private itemNamed (label : ItemLabel) (model : TableAndElementRotationModel) : Item =
        model.kitItems |> List.find (fun item -> item.label = label)

    let private withRecordingStudyWindow
        (model : TableAndElementRotationModel)
        (record : TableAndElementRotationMsg -> unit)
        (check : Window -> unit)
        : unit =
        let update (message : TableAndElementRotationMsg) (state : TableAndElementRotationModel) : TableAndElementRotationModel =
            record message
            updateTableAndElementRotation message state
        let window = HostWindow()
        Program.mkSimple (fun () -> model) update KitWindowView.kitWindowView
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

    let private withStudyWindow (model : TableAndElementRotationModel) (check : Window -> unit) : unit =
        withRecordingStudyWindow model ignore check

    let private withGainCalibrationRuns (model : TableAndElementRotationModel) : TableAndElementRotationModel =
        [ ExperimentName "E1"; ExperimentName "E3" ]
        |> List.fold (fun (state : TableAndElementRotationModel) name ->
            let experiment = state.experimentCollection.experiments |> List.find (fun experiment -> experiment.name = name)
            let changed = state |> updateTableAndElementRotation (SetExperimentRole (experiment.id, CalibrationRun))
            Assert.Equal<ControlCaption option>(None, changed.kitStatus)
            changed) model

    let private typeField (window : Window) (id : ConstructorUiId) (text : string) : unit =
        let field = Assert.IsType<TextBox>(SlotBarTests.controlById window id)
        field.BringIntoView()
        Dispatcher.UIThread.RunJobs()
        Assert.True(field.Focus())
        field.SelectAll()
        window.KeyTextInput text
        Dispatcher.UIThread.RunJobs()

    let private pressStudyBox (window : Window) (id : ConstructorUiId) : unit =
        let target = SlotBarTests.controlById window id
        target.BringIntoView()
        Dispatcher.UIThread.RunJobs()
        let centre = SlotBarTests.topLeftOf window target + Vector(target.Bounds.Width / 2.0, target.Bounds.Height / 2.0)
        let hitsTarget () : bool =
            match window.InputHitTest centre with
            | :? Visual as hit -> obj.ReferenceEquals(target, hit) || target.IsVisualAncestorOf hit
            | _ -> false
        // Spec 0050 S11.5: hit testing reads the last rendered frame, which can trail a scroll by a tick or more,
        // so render until the pressed centre resolves to the control.
        let rec settle (ticks : int) : unit =
            if not (hitsTarget ()) then
                if ticks = 0 then
                    let hitName = window.InputHitTest centre |> Option.ofObj |> Option.map (fun hit -> hit.GetType().Name)
                    Assert.Fail $"{id.value}'s centre ({centre.X}, {centre.Y}) still hits {hitName} at bounds {target.Bounds}"
                else
                    AvaloniaHeadlessPlatform.ForceRenderTimerTick()
                    Dispatcher.UIThread.RunJobs()
                    settle (ticks - 1)
        settle 50
        SlotBarTests.pressBox window id

    let private gridOf (window : Window) : Grid =
        Assert.IsType<Grid>(SlotBarTests.controlById window ConstructorUiId.studyUnknownsGrid)

    let private rowNumber (window : Window) (reference : StudyParameterRef) : int =
        let box = SlotBarTests.controlById window (ConstructorUiId.studyParameterRow reference)
        let grid = gridOf window
        let row =
            box.GetVisualAncestors()
            |> Seq.choose (function :? Control as control -> Some control | _ -> None)
            |> Seq.find (fun control -> grid.Children.Contains control)
        Grid.GetRow row

    let private cellAt (grid : Grid) (row : int) (column : int) : Control =
        grid.Children |> Seq.find (fun control -> Grid.GetRow control = row && Grid.GetColumn control = column)

    let private cellText (grid : Grid) (row : int) (column : int) : string =
        Assert.IsType<TextBlock>(cellAt grid row column).Text

    let private fitModelWith (runs : FitRun list) (model : TableAndElementRotationModel) : TableAndElementRotationModel =
        for run in runs do model.kit.proxy.writeFitRun run |> SceneTestFixture.kitSuccess
        for setup in model.kitSetups do
            let specified =
                {
                    setup with
                        slots = setup.slots |> List.map (fun slot ->
                            if slot.placement.catalogueKind = OpticalConstructor.Domain.Placement.LightSource then
                                {
                                    slot with
                                        placement =
                                            {
                                                slot.placement with valueId = Some PresetIds.heNe633
                                            }
                                }
                            else slot)
                }
            model.kit.proxy.writeSetup specified |> SceneTestFixture.kitSuccess
        let loaded =
            model
            |> updateTableAndElementRotation ReloadKitChanges
            |> updateTableAndElementRotation (ShowFlowStep FitFlowStep)
        Assert.Equal<ControlCaption option>(None, loaded.kitStatus)
        loaded

    let private fitBoxColor (window : Window) (id : ConstructorUiId) : Color =
        Assert.IsType<SolidColorBrush>(Assert.IsType<Border>(SlotBarTests.controlById window id).Background).Color

    let private resultsRunOf (model : TableAndElementRotationModel) : FitRun =
        let study = List.exactlyOne model.kitStudies
        let source = StudyFitViewFixture.fitViewRunOf study StudyFitViewFixture.firstFitViewRunStamp
        let first = itemNamed (ItemLabel "CPL1") model
        let second = itemNamed (ItemLabel "CPL2") model
        let firstRef : FitEntryRef =
            {
                parameter = ItemParameterRef (first.id, retardance)
                owner = ItemOwner first.id
            }
        let secondRef : FitEntryRef =
            {
                parameter = ItemParameterRef (second.id, retardance)
                owner = ItemOwner second.id
            }
        let entries : FitRunEntry list =
            [ firstRef; secondRef ] |> List.map (fun entry ->
                {
                    parameter = entry.parameter
                    owner = entry.owner
                    start = ParameterValue 1.0
                    fitted = ParameterValue 0.98
                })
        let studyProblem : FitProblemQuality =
            {
                subject = StudyEntriesProblem
                coordinates = [ EntryCoordinate [ firstRef ]; EntryCoordinate [ secondRef ] ]
                rank = MatrixRank 2
                conditionOpt = Some (ConditionNumber 12.5)
                columnNorms = JacobianColumnNorms [| 3.0; 0.25 |]
                correlationOpt = Some (CorrelationMatrix [| [| 1.0; -0.5 |]; [| -0.5; 1.0 |] |])
                chiSquared = FitChiSquared 0.5
                rmse = ResidualRmse 0.01
            }
        let objectProblem : FitProblemQuality =
            {
                studyProblem with
                    subject = ObjectSolveProblem [ first.id; second.id ]
                    coordinates = [ ElementCoordinate (MuellerRowIndex 1, MuellerColumnIndex 2); ElementCoordinate (MuellerRowIndex 3, MuellerColumnIndex 0) ]
                    rank = MatrixRank 1
                    conditionOpt = None
                    columnNorms = JacobianColumnNorms [| 2.0; 0.0 |]
                    correlationOpt = None
                    chiSquared = FitChiSquared 0.125
                    rmse = ResidualRmse 0.02
            }
        let fringeProblem : FitProblemQuality =
            {
                studyProblem with
                    subject = AirFringeProblem [ ExperimentName "A1"; ExperimentName "A2" ]
                    coordinates = [ EntryCoordinate [ secondRef ]; FringeVisibilityCoordinate ]
                    conditionOpt = Some (ConditionNumber 4.0)
                    columnNorms = JacobianColumnNorms [| 1.5; 0.75 |]
                    correlationOpt = Some (CorrelationMatrix [| [| 1.0; 0.25 |]; [| 0.25; 1.0 |] |])
                    chiSquared = FitChiSquared 0.0625
                    rmse = ResidualRmse 0.005
            }
        let residual : RunResidual =
            {
                run = ExperimentName "E1"
                residuals = ResidualVector [| 0.01; -0.02 |]
                rmse = ResidualRmse 0.0158
            }
        let realizability : ObjectRealizability =
            {
                item = first.id
                eigenvalues = [ CoherencyEigenvalue 0.97; CoherencyEigenvalue 0.01; CoherencyEigenvalue 0.002; CoherencyEigenvalue -1.0e-14 ]
            }
        let product : SeriesProductCheck =
            {
                items = [ first.id; second.id ]
                cascade = List.init 16 (fun index -> if index % 5 = 0 then ParameterValue 0.97 else ParameterValue 0.001)
                difference =
                    {
                        frobenius = MatrixDeviation 0.003
                        meanAbs = MatrixDeviation 0.0005
                        maxAbs = MatrixDeviation 0.002
                        argMax = (MuellerRowIndex 3, MuellerColumnIndex 2)
                    }
                fitRmse = ResidualRmse 0.01
                productRmse = ResidualRmse 0.012
            }
        let profile : ParameterProfile =
            {
                entry = secondRef
                reasons = [ UndeterminedSigmaReason; WideSigmaReason (SigmaFraction 0.25); CorrelatedEntryReason (firstRef, FitCorrelation -0.97) ]
                points = List.init 7 (fun index ->
                    {
                        value = ParameterValue (1.5 + 0.25 * float index)
                        outcome =
                            if index = 1 then
                                ProfileFailed (FitMemberFailure "the solver did not converge: 100 iterations")
                            else ProfileConverged (FitChiSquared (0.75 + float index))
                    })
            }
        let results : FitRunResults =
            {
                sigmas =
                    [
                        {
                            entry = firstRef
                            sigmaOpt = Some (ParameterSigma 0.002)
                        }
                        {
                            entry = secondRef
                            sigmaOpt = None
                        }
                    ]
                problems = [ studyProblem; objectProblem; fringeProblem ]
                runResiduals = [ residual ]
                realizability = [ realizability ]
                productChecks = [ product ]
                profiles = [ profile ]
            }
        {
            source with
                stage = ObjectStage
                engine = LinearStudyEngine
                entries = entries
                results = results
        }

    let private studyPanelTexts (window : Window) (id : ConstructorUiId) : string list =
        (SlotBarTests.controlById window id).GetVisualDescendants()
        |> Seq.choose (function :? TextBlock as text -> Some text.Text | _ -> None)
        |> Seq.toList

    let private resultTexts (window : Window) : string list =
        studyPanelTexts window ConstructorUiId.fitResultsTable

    let private calibrationViewRunOf (model : TableAndElementRotationModel) : FitRun =
        let first = itemNamed (ItemLabel "CPL1") model
        let fitted = ParameterValue 1.55
        let entry = StudyPublishViewFixture.publicationItemEntry first retardance fitted
        StudyPublishViewFixture.publicationRunOf model StudyPublishViewFixture.firstPublicationStamp [ entry ]

    // Spec 0050 S11.5: isolate status rendering from disk contents while retaining real scan validation.
    let private prefitModelWithData (model : TableAndElementRotationModel) : TableAndElementRotationModel =
        let loaded = fitModelWith [] model
        let pointsByPath =
            [
                ExperimentName "E1", DataFileText "x,y\n0,1\n45,0.6\n90,0.2\n"
                ExperimentName "E3", DataFileText "x,y\n0,1\n120,0.5\n"
            ]
            |> List.map (fun (name, text) ->
                let experiment : Experiment = loaded.experimentCollection.experiments |> List.find (fun experiment -> experiment.name = name)
                let data = experiment.dataOpt |> Option.defaultWith (fun () -> failwith $"{name.value} must have an attachment")
                let points =
                    text.parseIntensitySeries
                    |> Result.map (IntensityData >> SeriesPoints)
                    |> Result.defaultWith (fun _ -> failwith $"The pre-fit fixture for {name.value} must parse")
                data.attachment.path, points)
            |> Map.ofList
        let dataProxy : ExperimentDataProxy =
            {
                loaded.experimentData with
                    attachmentStatus = fun _ -> Ok AttachedDataUnchanged
                    readMapped = fun data ->
                        match pointsByPath |> Map.tryFind data.attachment.path with
                        | Some points -> Ok points
                        | None -> Error (MalformedDataFileErr $"No pre-fit fixture data for '{data.attachment.path.value}'")
            }
        {
            loaded with experimentData = dataProxy
        }

    [<Fact>]
    let ``the study view UiIds are the stable intent-named ids`` () : unit =
        Assert.Equal("FlowUnknownsStep", ConstructorUiId.flowUnknownsStep.value)
        Assert.Equal("StudyUnknownsPanel", ConstructorUiId.studyUnknownsPanel.value)
        Assert.Equal("StudyList", ConstructorUiId.studyList.value)
        Assert.Equal("StudyStatusLine", ConstructorUiId.studyStatusLine.value)
        Assert.Equal("NewStudyNameField", ConstructorUiId.newStudyNameField.value)
        Assert.Equal("CreateStudyButton", ConstructorUiId.createStudyButton.value)
        Assert.Equal("StudyExperimentsText", ConstructorUiId.studyExperimentsText.value)
        Assert.Equal("StudyUnknownsGrid", ConstructorUiId.studyUnknownsGrid.value)
        Assert.Equal("StudySessionGrid", ConstructorUiId.studySessionGrid.value)
        let session = SessionName "Session 1"
        Assert.Equal("StudySessionGain_Session 1", (ConstructorUiId.studySessionGainChoice session).value)
        Assert.Equal("StudyFitMode_2", (ConstructorUiId.studyFitModeChoice StagedFit).value)
        Assert.Equal("FlowFitStep", ConstructorUiId.flowFitStep.value)
        Assert.Equal("StudyFitPanel", ConstructorUiId.studyFitPanel.value)
        Assert.Equal("StudyPrefitList", ConstructorUiId.studyPrefitList.value)
        Assert.Equal("StudyRunFitButton", ConstructorUiId.studyRunFitButton.value)
        Assert.Equal("StudyRunStageOneButton", ConstructorUiId.studyRunStageOneButton.value)
        Assert.Equal("StudyRunStageTwoButton", ConstructorUiId.studyRunStageTwoButton.value)
        Assert.Equal("StudyCalibrationFixedChoice", ConstructorUiId.studyCalibrationFixedChoice.value)
        Assert.Equal("StudyCalibrationPriorChoice", ConstructorUiId.studyCalibrationPriorChoice.value)
        Assert.Equal("StudyFitProgressBar", ConstructorUiId.studyFitProgressBar.value)
        Assert.Equal("StudyFitProgressText", ConstructorUiId.studyFitProgressText.value)
        Assert.Equal("StudyFitCancelButton", ConstructorUiId.studyFitCancelButton.value)
        Assert.Equal("StudyFitErrorText", ConstructorUiId.studyFitErrorText.value)
        Assert.Equal("FitRunHistory", ConstructorUiId.fitRunHistory.value)
        Assert.Equal("FlowResultsStep", ConstructorUiId.flowResultsStep.value)
        Assert.Equal("StudyResultsPanel", ConstructorUiId.studyResultsPanel.value)
        Assert.Equal("FitResultsRunText", ConstructorUiId.fitResultsRunText.value)
        Assert.Equal("FitResultsTabStrip", ConstructorUiId.fitResultsTabStrip.value)
        Assert.Equal("FitResultsValuesTab", ConstructorUiId.fitResultsValuesTab.value)
        Assert.Equal("FitResultsCorrelationTab", ConstructorUiId.fitResultsCorrelationTab.value)
        Assert.Equal("FitResultsResidualsTab", ConstructorUiId.fitResultsResidualsTab.value)
        Assert.Equal("FitResultsRealizabilityTab", ConstructorUiId.fitResultsRealizabilityTab.value)
        Assert.Equal("FitResultsProductCheckTab", ConstructorUiId.fitResultsProductCheckTab.value)
        Assert.Equal("FitResultsProfileTab", ConstructorUiId.fitResultsProfileTab.value)
        Assert.Equal("FitResultsTable", ConstructorUiId.fitResultsTable.value)
        Assert.Equal("FlowPublishStep", ConstructorUiId.flowPublishStep.value)
        Assert.Equal("StudyPublishPanel", ConstructorUiId.studyPublishPanel.value)
        Assert.Equal("PublishCalibrationPreview", ConstructorUiId.publishCalibrationPreview.value)
        Assert.Equal("PublishCalibrationButton", ConstructorUiId.publishCalibrationButton.value)
        Assert.Equal("PromotionList", ConstructorUiId.promotionList.value)
        Assert.Equal<ConstructorUiId list>(
            [ ConstructorUiId.fitResultsValuesTab; ConstructorUiId.fitResultsCorrelationTab; ConstructorUiId.fitResultsResidualsTab;
              ConstructorUiId.fitResultsRealizabilityTab; ConstructorUiId.fitResultsProductCheckTab; ConstructorUiId.fitResultsProfileTab ],
            FitResultsTab.all |> List.map _.tabUiId)
        let study = StudyName "QZ-LR"
        let experiment = ExperimentName "E1"
        let fitRun = FitRunName "QZ-LR@20261101T120000Z"
        Assert.Equal("StudyPrefitRow_E1", (ConstructorUiId.studyPrefitRow experiment).value)
        Assert.Equal("FitRunRow_QZ-LR@20261101T120000Z", (ConstructorUiId.fitRunRow fitRun).value)
        let cpl1 = ItemId (System.Guid.Parse "41400000-0000-4000-8000-000000000011")
        let itemParameter = ItemParameterRef (cpl1, retardance)
        let material = MaterialId (System.Guid.Parse "41400000-0000-4000-8000-000000000102")
        let version = MaterialVersionId.firstOf material
        let n1 = ParameterName "n1"
        let materialParameter = MaterialParameterRef (version, n1)
        let materialSubject = MaterialPromotion version
        let itemSubject = BlackBoxPromotion cpl1
        Assert.Equal("Promote_41400000-0000-4000-8000-000000000102_v1", (ConstructorUiId.promoteButton materialSubject).value)
        Assert.Equal("Promote_41400000-0000-4000-8000-000000000011", (ConstructorUiId.promoteButton itemSubject).value)
        Assert.Equal("StudyChoice_QZ-LR", (ConstructorUiId.studyChoice study).value)
        Assert.Equal("NewStudyExperiment_E1", (ConstructorUiId.newStudyExperimentToggle experiment).value)
        Assert.Equal("AddExperimentToStudy_E1", (ConstructorUiId.addExperimentToStudyButton experiment).value)
        Assert.Equal("OpenMaterial_41400000-0000-4000-8000-000000000102_v1", (ConstructorUiId.openMaterialButton version).value)
        for reference, suffix in
            [ itemParameter, "41400000-0000-4000-8000-000000000011_c2.retardance"
              materialParameter, "41400000-0000-4000-8000-000000000102_v1_n1" ] do
            Assert.Equal($"StudyParameterRow_{suffix}", (ConstructorUiId.studyParameterRow reference).value)
            Assert.Equal($"StudyParameterScope_{suffix}", (ConstructorUiId.studyParameterScopeChoice reference).value)
            Assert.Equal($"StudyParameterLink_{suffix}", (ConstructorUiId.studyParameterLinkChoice reference).value)

    [<Fact>]
    let ``each session of the study offers its gain models, and choosing one sends SetSessionGainModel`` () : unit =
        HeadlessSession.run (fun () ->
            let model = TestAppScope.studyKitIn owner
            try
                let shown = model |> withGainCalibrationRuns |> updateTableAndElementRotation (ShowFlowStep UnknownsFlowStep)
                let study = StudyName "QZ-LR"
                let session = SessionName "Session 1"
                let e1 = ExperimentName "E1"
                let e3 = ExperimentName "E3"
                let repeats : AirRepeatPair =
                    {
                        opening = e1
                        closing = e3
                    }
                let messages = ResizeArray<StudyMsg> ()
                let record (message : TableAndElementRotationMsg) : unit =
                    match message with
                    | StudyCommand command -> messages.Add command
                    | _ -> ()
                withRecordingStudyWindow shown record (fun window ->
                    let grid = Assert.IsType<Grid>(SlotBarTests.controlById window ConstructorUiId.studySessionGrid)
                    Assert.Equal("Sessions and gains", AutomationProperties.GetName grid)
                    Assert.Equal(3, grid.ColumnDefinitions.Count)
                    Assert.All(grid.ColumnDefinitions, fun column -> Assert.Equal<GridUnitType>(GridUnitType.Auto, column.Width.GridUnitType))
                    Assert.Equal(2, grid.RowDefinitions.Count)
                    Assert.Equal<string list>([ "Session"; "Experiments"; "Gain" ], [ for column in 0 .. 2 -> cellText grid 0 column ])
                    for column in 0 .. 2 do
                        Assert.Equal<FontWeight>(FontWeight.SemiBold, Assert.IsType<TextBlock>(cellAt grid 0 column).FontWeight)
                    Assert.Equal("Session 1", cellText grid 1 0)
                    Assert.Equal("E1, E2, E3", cellText grid 1 1)
                    let gain = Assert.IsType<ComboBox>(SlotBarTests.controlById window (ConstructorUiId.studySessionGainChoice session))
                    Assert.Same(cellAt grid 1 2, gain)
                    Assert.False(System.String.IsNullOrWhiteSpace(AutomationProperties.GetName gain))
                    Assert.True gain.IsEffectivelyEnabled
                    Assert.Equal<string list>([ "Free"; "Anchored to E1, E3"; "Interpolated from E1 to E3" ], gain.Items |> Seq.cast<string> |> Seq.toList)
                    Assert.Equal("Free", Assert.IsType<string> gain.SelectedItem)
                    Assert.Empty messages
                    for index, expected in [ 1, AnchoredSessionGain [ e1; e3 ]; 2, InterpolatedSessionGain repeats; 0, FreeSessionGain ] do
                        messages.Clear()
                        gain.SelectedIndex <- index
                        Dispatcher.UIThread.RunJobs()
                        match Assert.Single messages with
                        | SetSessionGainModel (name, setting) ->
                            Assert.Equal<StudyName>(study, name)
                            Assert.Equal<SessionName>(session, setting.session)
                            Assert.Equal<SessionGainModel>(expected, setting.model)
                        | _ -> Assert.Fail "The gain choice must send SetSessionGainModel"
                        let recorded = model.kit.proxy.readStudies () |> SceneTestFixture.kitSuccess |> List.exactlyOne
                        Assert.Equal<SessionGainModel>(expected, recorded.gainModelOf session)
                        Assert.Equal(expected.caption.value, Assert.IsType<string> gain.SelectedItem))
                let emptyStudy =
                    {
                        List.exactlyOne shown.kitStudies with experiments = []
                    }
                let empty =
                    {
                        shown with kitStudies = [ emptyStudy ]
                    }
                withStudyWindow empty (fun window ->
                    Assert.Contains("This study's experiments form no session.", studyPanelTexts window ConstructorUiId.studyUnknownsPanel)
                    SlotBarTests.assertAbsent window (ConstructorUiId.studySessionGainChoice session))
            finally model.kit.close () |> SceneTestFixture.kitSuccess)

    [<Fact>]
    let ``the Fit step's mode choice sends SetFitMode`` () : unit =
        HeadlessSession.run (fun () ->
            let model = TestAppScope.studyKitIn owner
            try
                let shown = model |> withGainCalibrationRuns |> updateTableAndElementRotation (ShowFlowStep FitFlowStep)
                let study = StudyName "QZ-LR"
                let messages = ResizeArray<StudyMsg> ()
                let record (message : TableAndElementRotationMsg) : unit =
                    match message with
                    | StudyCommand command -> messages.Add command
                    | _ -> ()
                withRecordingStudyWindow shown record (fun window ->
                    let joint = ConstructorUiId.studyFitModeChoice JointFit
                    let staged = ConstructorUiId.studyFitModeChoice StagedFit
                    Assert.Equal(MaterialsWindowView.positiveBackground, fitBoxColor window joint)
                    Assert.NotEqual(MaterialsWindowView.positiveBackground, fitBoxColor window staged)
                    for mode in FitMode.all do
                        let id = ConstructorUiId.studyFitModeChoice mode
                        Assert.Equal(mode.text, SlotBarTests.boxLabelOf window id)
                        Assert.Equal(mode.text, AutomationProperties.GetName(SlotBarTests.controlById window id))
                    Assert.Contains("Fit mode:", studyPanelTexts window ConstructorUiId.studyFitPanel)
                    let modeBox = SlotBarTests.controlById window staged
                    let runBox = SlotBarTests.controlById window ConstructorUiId.studyRunFitButton
                    Assert.True((SlotBarTests.topLeftOf window modeBox).Y + modeBox.Bounds.Height <= (SlotBarTests.topLeftOf window runBox).Y)
                    Assert.Empty messages
                    for mode in [ StagedFit; JointFit ] do
                        messages.Clear()
                        pressStudyBox window (ConstructorUiId.studyFitModeChoice mode)
                        match Assert.Single messages with
                        | SetFitMode (name, chosen) ->
                            Assert.Equal<StudyName>(study, name)
                            Assert.Equal<FitMode>(mode, chosen)
                        | _ -> Assert.Fail "The mode choice must send SetFitMode"
                        let recorded = model.kit.proxy.readStudies () |> SceneTestFixture.kitSuccess |> List.exactlyOne
                        Assert.Equal<FitMode>(mode, recorded.settings.mode)
                        Assert.Equal(MaterialsWindowView.positiveBackground, fitBoxColor window (ConstructorUiId.studyFitModeChoice mode))
                        for other in FitMode.all |> List.filter ((<>) mode) do
                            Assert.NotEqual(MaterialsWindowView.positiveBackground, fitBoxColor window (ConstructorUiId.studyFitModeChoice other)))
            finally model.kit.close () |> SceneTestFixture.kitSuccess)

    [<Fact>]
    let ``the Unknowns step lists the study's item parameters and opened material components with their owner, state, bounds, sigma, scope and link`` () : unit =
        HeadlessSession.run (fun () ->
            let model = TestAppScope.studyKitIn owner
            try
                let opened = TestAppScope.withStudyMaterial model
                let first = itemNamed (ItemLabel "CPL1") opened
                let second = itemNamed (ItemLabel "CPL2") opened
                let phone = itemNamed (ItemLabel "Phone") opened
                let study = List.exactlyOne opened.kitStudies
                let link : ParameterLink =
                    {
                        follower = ItemParameterRef (second.id, retardance)
                        leader = ItemParameterRef (first.id, retardance)
                    }
                let linked = opened |> updateTableAndElementRotation (StudyCommand (LinkParameters (study.name, link)))
                Assert.Equal<ControlCaption option>(None, linked.kitStatus)
                let shown = linked |> updateTableAndElementRotation (ShowFlowStep UnknownsFlowStep)
                let material = List.exactlyOne study.materials
                Assert.Equal(15, material.parameters.Length)
                Assert.Equal<ControlCaption>(ControlCaption "CPL1 c2.retardance", shown.studyParameterCaption link.leader)
                let expected =
                    ([ first; second; phone ] |> List.collect (fun item ->
                        item.parameters |> List.map (fun parameter -> ItemParameterRef (item.id, parameter.name))))
                    @ (material.parameters |> List.map (fun parameter -> MaterialParameterRef (material.version, parameter.name)))
                withStudyWindow shown (fun window ->
                    let grid = gridOf window
                    Assert.Equal(8, grid.ColumnDefinitions.Count)
                    Assert.Equal(expected.Length + 1, grid.RowDefinitions.Count)
                    Assert.Equal<string list>(
                        [ "Owner"; "Parameter"; "State"; "Value"; "Bounds"; "σ"; "Scope"; "Link" ],
                        [ for column in 0 .. 7 -> cellText grid 0 column ])
                    for index, reference in List.indexed expected do
                        Assert.Equal(index + 1, rowNumber window reference)
                        Assert.Equal(reference.name.value, SlotBarTests.boxLabelOf window (ConstructorUiId.studyParameterRow reference))
                    for item in [ first; second; phone ] do
                        for parameter in item.parameters do
                            let reference = ItemParameterRef (item.id, parameter.name)
                            let row = rowNumber window reference
                            Assert.Equal(item.label.value, cellText grid row 0)
                            let cells = parameter.gridCells
                            Assert.Equal<string list>([ cells.state.value; cells.value.value; cells.bounds.value; cells.sigma.value ],
                                                      [ for column in 2 .. 5 -> cellText grid row column ])
                    for parameter in material.parameters do
                        let reference = MaterialParameterRef (material.version, parameter.name)
                        let row = rowNumber window reference
                        Assert.Equal("Study absorbing crystal v1", cellText grid row 0)
                        Assert.Equal("Known", cellText grid row 2)
                        let cells = parameter.gridCells
                        Assert.Equal<string list>([ cells.value.value; cells.bounds.value; cells.sigma.value ],
                                                  [ for column in 3 .. 5 -> cellText grid row column ])
                        Assert.Equal("—", cellText grid row 6)
                        Assert.Equal("—", cellText grid row 7)
                        Assert.Equal<ControlCaption>(ControlCaption $"Study absorbing crystal v1 {parameter.name.value}", shown.studyParameterCaption reference)
                    let gain = ItemParameterRef (phone.id, ParameterName.gain)
                    let gainRow = rowNumber window gain
                    Assert.Equal("Unknown", cellText grid gainRow 2)
                    Assert.Equal("1000", cellText grid gainRow 3)
                    Assert.Equal("[100, 10000]", cellText grid gainRow 4)
                    Assert.Equal("—", cellText grid gainRow 5)
                    let scope = Assert.IsType<ComboBox>(SlotBarTests.controlById window (ConstructorUiId.studyParameterScopeChoice gain))
                    Assert.Equal("Session", Assert.IsType<string>(scope.SelectedItem))
                    Assert.Equal<string list>(ParameterScope.all |> List.map _.text, scope.Items |> Seq.cast<string> |> List.ofSeq)
                    Assert.Equal("= CPL1 c2.retardance", cellText grid (rowNumber window link.follower) 7)
                    SlotBarTests.assertAbsent window (ConstructorUiId.studyParameterLinkChoice link.follower)
                    let leaders = Assert.IsType<ComboBox>(SlotBarTests.controlById window (ConstructorUiId.studyParameterLinkChoice gain))
                    Assert.Equal<string list>([ "(no link)"; "CPL1 c2.retardance"; "CPL2 c2.retardance" ], leaders.Items |> Seq.cast<string> |> List.ofSeq)
                    let darkOffset = ParameterName "darkOffset"
                    let known = ItemParameterRef (phone.id, darkOffset)
                    Assert.Equal("—", cellText grid (rowNumber window known) 6)
                    Assert.Equal("Experiments: E1, E2, E3", Assert.IsType<TextBlock>(SlotBarTests.controlById window ConstructorUiId.studyExperimentsText).Text)
                    scope.SelectedIndex <- ParameterScope.all |> List.findIndex ((=) ExperimentScope)
                    Dispatcher.UIThread.RunJobs()
                    let recorded = model.kit.proxy.readStudies () |> SceneTestFixture.kitSuccess |> List.exactlyOne
                    Assert.Equal<ParameterScope>(ExperimentScope, recorded.scopeOf gain))
            finally model.kit.close () |> SceneTestFixture.kitSuccess)

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``typing a study name and pressing Create study records the study in the kit`` () : unit =
        HeadlessSession.run (fun () ->
            let model = TestAppScope.studyKitIn owner
            try
                withStudyWindow model (fun window ->
                    pressStudyBox window ConstructorUiId.flowUnknownsStep
                    typeField window ConstructorUiId.newStudyNameField "LR"
                    let e1 = ExperimentName "E1"
                    pressStudyBox window (ConstructorUiId.newStudyExperimentToggle e1)
                    pressStudyBox window ConstructorUiId.createStudyButton
                    let name = StudyName "LR"
                    let recorded = model.kit.proxy.readStudies () |> SceneTestFixture.kitSuccess |> List.find (fun study -> study.name = name)
                    Assert.Equal<ExperimentName list>([ e1 ], recorded.experiments)
                    let folder = model.kitFolderOpt |> Option.defaultWith (fun () -> failwith "The study kit must have a folder")
                    Assert.True(File.Exists(Path.Combine(folder.value.value, "LR.study-json")))
                    Assert.Equal("LR", SlotBarTests.boxLabelOf window (ConstructorUiId.studyChoice name))
                    Assert.Equal("Experiments: E1", Assert.IsType<TextBlock>(SlotBarTests.controlById window ConstructorUiId.studyExperimentsText).Text)
                    let e2 = ExperimentName "E2"
                    pressStudyBox window (ConstructorUiId.addExperimentToStudyButton e2)
                    let expanded = model.kit.proxy.readStudies () |> SceneTestFixture.kitSuccess |> List.find (fun study -> study.name = name)
                    Assert.Equal<ExperimentName list>([ e1; e2 ], expanded.experiments)
                    SlotBarTests.assertAbsent window (ConstructorUiId.addExperimentToStudyButton e2))
            finally model.kit.close () |> SceneTestFixture.kitSuccess)

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``pressing a study parameter's row, Unknown and Set frees it in the kit`` () : unit =
        HeadlessSession.run (fun () ->
            let model = TestAppScope.studyKitIn owner
            try
                let phone = itemNamed (ItemLabel "Phone") model
                let name = ParameterName "darkOffset"
                let reference = ItemParameterRef (phone.id, name)
                withStudyWindow (model |> updateTableAndElementRotation (ShowFlowStep UnknownsFlowStep)) (fun window ->
                    pressStudyBox window (ConstructorUiId.studyParameterRow reference)
                    Assert.Single(SlotBarTests.controlsUnder window ConstructorUiId.parameterEditPanel) |> ignore
                    Assert.Equal("Known", SlotBarTests.boxLabelOf window ConstructorUiId.parameterEditKnownChoice)
                    pressStudyBox window ConstructorUiId.parameterEditUnknownChoice
                    typeField window ConstructorUiId.parameterEditLowerField "-1"
                    typeField window ConstructorUiId.parameterEditUpperField "1"
                    pressStudyBox window ConstructorUiId.parameterEditSetButton
                    let recorded = model.kit.proxy.readItems () |> SceneTestFixture.kitSuccess |> List.find (fun item -> item.id = phone.id)
                    match recorded.tryFindParameter name with
                    | Some { state = UnknownParameter unknown } ->
                        Assert.Equal<ParameterValue>(ParameterValue (-1.0), unknown.bounds.lower)
                        Assert.Equal<ParameterValue>(ParameterValue 1.0, unknown.bounds.upper)
                        Assert.Equal<ParameterValue>(ParameterValue 0.0, unknown.start)
                    | _ -> Assert.Fail "Phone.darkOffset must be stored as Unknown"
                    let folder = model.kitFolderOpt |> Option.defaultWith (fun () -> failwith "The study kit must have a folder")
                    Assert.True(File.Exists(Path.Combine(folder.value.value, "Phone.item-json")))
                    let grid = gridOf window
                    Assert.Equal("Unknown", cellText grid (rowNumber window reference) 2)
                    Assert.Equal("[-1, 1]", cellText grid (rowNumber window reference) 4))
            finally model.kit.close () |> SceneTestFixture.kitSuccess)

    [<Fact>]
    let ``the Fit step checks every run the fit reads, in study order, under the chosen study`` () : unit =
        HeadlessSession.run (fun () ->
            let model = TestAppScope.studyKitIn owner
            try
                let shown = prefitModelWithData model
                let expectedStudy = StudyName "QZ-LR"
                let expectedRuns = [ ExperimentName "E1"; ExperimentName "E2"; ExperimentName "E3" ]
                let expectedPoints = MeasuredPointCount 3
                let study = shown.selectedStudyOpt |> Option.defaultWith (fun () -> failwith "The study must be selected")
                Assert.Equal<StudyName>(expectedStudy, study.name)
                Assert.Equal<ExperimentName list>(expectedRuns, study.experiments)
                match shown.studyPrefitsOpt with
                | Some (Ok [ ready; missing; inconsistent ]) ->
                    Assert.Equal<ExperimentName list>(expectedRuns, [ ready.experiment.name; missing.experiment.name; inconsistent.experiment.name ])
                    match ready.check with
                    | Ok points -> Assert.Equal<MeasuredPointCount>(expectedPoints, points.pointCount)
                    | Error error -> Assert.Fail $"E1 must be ready: {error.describe}"
                    match missing.check with
                    | Error (MissingDataErr _) -> ()
                    | _ -> Assert.Fail "E2 must report MissingDataErr"
                    match inconsistent.check with
                    | Error (InconsistentDataErr _) -> ()
                    | _ -> Assert.Fail "E3 must report InconsistentDataErr"
                | _ -> Assert.Fail "The study must return exactly three pre-fit checks"
            finally model.kit.close () |> SceneTestFixture.kitSuccess)

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the Fit step mounts its pre-fit list with one row per run the fit reads`` () : unit =
        HeadlessSession.run (fun () ->
            let model = TestAppScope.studyKitIn owner
            try
                let shown = prefitModelWithData model
                let expectedRuns = [ ExperimentName "E1"; ExperimentName "E2"; ExperimentName "E3" ]
                Assert.Equal<ConstructorFlowStep>(FitFlowStep, shown.flowStep)
                withStudyWindow shown (fun window ->
                    Assert.Single(SlotBarTests.controlsUnder window ConstructorUiId.studyFitPanel) |> ignore
                    let prefits = Assert.IsType<StackPanel>(SlotBarTests.controlById window ConstructorUiId.studyPrefitList)
                    Assert.Equal<string list>(
                        expectedRuns |> List.map (fun name -> (ConstructorUiId.studyPrefitRow name).value),
                        prefits.Children |> Seq.map AutomationProperties.GetAutomationId |> List.ofSeq)
                    for name in expectedRuns do
                        let row = Assert.Single(SlotBarTests.controlsUnder window (ConstructorUiId.studyPrefitRow name))
                        Assert.Same(prefits, row.Parent))
            finally model.kit.close () |> SceneTestFixture.kitSuccess)

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``a ready run shows its point count, and a run without data or out of range shows its typed message`` () : unit =
        HeadlessSession.run (fun () ->
            let model = TestAppScope.studyKitIn owner
            try
                withStudyWindow (prefitModelWithData model) (fun window ->
                    let textOf (name : ExperimentName) : string =
                        Assert.IsType<TextBlock>(SlotBarTests.controlById window (ConstructorUiId.studyPrefitRow name)).Text
                    let ready = ExperimentName "E1"
                    let missing = ExperimentName "E2"
                    let inconsistent = ExperimentName "E3"
                    Assert.Equal("E1: ready, 3 point(s)", textOf ready)
                    Assert.Contains("no data is attached", textOf missing)
                    Assert.Contains("the data does not fit the scan", textOf inconsistent))
            finally model.kit.close () |> SceneTestFixture.kitSuccess)

    [<Fact>]
    let ``the Results step shows the chosen fit run's six tabs and each tab's table`` () : unit =
        HeadlessSession.run (fun () ->
            let model = TestAppScope.studyKitIn owner |> TestAppScope.withStudyMaterial
            let previousCulture = System.Globalization.CultureInfo.CurrentCulture
            try
                System.Globalization.CultureInfo.CurrentCulture <- System.Globalization.CultureInfo.GetCultureInfo "fr-FR"
                let run = resultsRunOf model
                let study = List.exactlyOne model.kitStudies
                let laterStamp = ClockTime (run.stamp.value.AddHours 1.0)
                let later = StudyFitViewFixture.fitViewRunOf study laterStamp
                let loaded = fitModelWith [ later; run ] model |> updateTableAndElementRotation (ShowFlowStep ResultsFlowStep)
                Assert.Equal<FitRun option>(Some later, loaded.selectedFitRunOpt)
                let selected = loaded |> updateTableAndElementRotation (SelectFitRun run.name)
                let material = List.exactlyOne study.materials
                Assert.Equal<ControlCaption>(
                    ControlCaption $"{material.name.value} v{material.version.version.value}",
                    selected.ownerCaption (MaterialOwner material.version))
                for tab in FitResultsTab.all do
                    let shown = selected |> updateTableAndElementRotation (ShowResultsTab tab)
                    withStudyWindow shown (fun window ->
                        let title = Assert.IsType<TextBlock>(SlotBarTests.controlById window ConstructorUiId.fitResultsRunText)
                        Assert.Equal($"Fit run {run.name.value} — Stage 2 (objects), linear", title.Text)
                        let strip = Assert.IsType<WrapPanel>(SlotBarTests.controlById window ConstructorUiId.fitResultsTabStrip)
                        Assert.Equal(6, strip.Children.Count)
                        for choice in FitResultsTab.all do
                            let box = SlotBarTests.controlById window choice.tabUiId
                            Assert.Contains<Visual>(strip, box.GetVisualAncestors())
                            Assert.Equal(choice.caption.value, AutomationProperties.GetName box)
                            if choice = tab then Assert.Equal(MaterialsWindowView.positiveBackground, fitBoxColor window choice.tabUiId)
                            else Assert.NotEqual(MaterialsWindowView.positiveBackground, fitBoxColor window choice.tabUiId)
                        let texts = resultTexts window
                        let table = SlotBarTests.controlById window ConstructorUiId.fitResultsTable
                        Assert.Equal(tab.caption.value, AutomationProperties.GetName table)
                        let grids = table.GetVisualDescendants() |> Seq.choose (function :? Grid as grid -> Some grid | _ -> None) |> Seq.toList
                        match tab with
                        | ValuesResultsTab ->
                            let grid = Assert.Single grids
                            Assert.Equal<string list>([ "Parameter"; "Start"; "Fitted"; "σ" ], [ for column in 0 .. 3 -> cellText grid 0 column ])
                            Assert.Equal<string list>([ "CPL1 c2.retardance"; "1"; "0.98"; "0.002" ], [ for column in 0 .. 3 -> cellText grid 1 column ])
                            Assert.Equal("CPL2 c2.retardance", cellText grid 2 0)
                            Assert.Equal("undetermined", cellText grid 2 3)
                        | CorrelationResultsTab ->
                            Assert.Contains("Study residual: rank 2, condition 12.5, χ² 0.5, RMSE 0.01", texts)
                            Assert.Contains("Object CPL1, CPL2: rank 1, condition —, χ² 0.125, RMSE 0.02", texts)
                            Assert.Contains("no correlation: the Jacobian is rank-deficient", texts)
                            Assert.Contains("AIR fringe A1, A2: rank 2, condition 4, χ² 0.0625, RMSE 0.005", texts)
                            Assert.Equal(2, grids.Length)
                            let grid = grids.Head
                            Assert.Equal(3, grid.RowDefinitions.Count)
                            Assert.Equal(3, grid.ColumnDefinitions.Count)
                            Assert.Equal("CPL1 c2.retardance", cellText grid 0 1)
                            Assert.Equal("CPL2 c2.retardance", cellText grid 0 2)
                            Assert.Equal("CPL1 c2.retardance", cellText grid 1 0)
                            Assert.Equal("CPL2 c2.retardance", cellText grid 2 0)
                            Assert.Equal<string list>([ "1.000"; "-0.500"; "-0.500"; "1.000" ], [ for row in 1 .. 2 do for column in 1 .. 2 -> cellText grid row column ])
                            Assert.Equal("fringe visibility", cellText grids.[1] 0 2)
                        | ResidualsResultsTab ->
                            Assert.Equal<string list>([ "E1: RMSE 0.0158 over 2 residual(s)" ], texts)
                        | RealizabilityResultsTab ->
                            Assert.Equal<string list>([ "CPL1: realizable; eigenvalues 0.97, 0.01, 0.002, -1E-14" ], texts)
                        | ProductCheckResultsTab ->
                            Assert.Equal<string list>([ "CPL1 → CPL2: product RMSE 0.012, fit RMSE 0.01, ratio 1.2, max |ΔM| 0.002" ], texts)
                        | ProfileResultsTab ->
                            Assert.Equal<string list>(
                                [ "CPL2 c2.retardance (σ undetermined; σ is 0.25 of its bounds; correlated with CPL1 c2.retardance (ρ = -0.97))"
                                  "1.5: χ² 0.75"; "1.75: the solver did not converge: 100 iterations"; "2: χ² 2.75"
                                  "2.25: χ² 3.75"; "2.5: χ² 4.75"; "2.75: χ² 5.75"; "3: χ² 6.75" ], texts))
                    withStudyWindow (loaded |> updateTableAndElementRotation (ShowResultsTab tab)) (fun window ->
                        Assert.Equal<string list>([ "Nothing to show for this fit run." ], resultTexts window))
                let objectProblem = run.results.problems.[1]
                let elementProblem =
                    {
                        objectProblem with correlationOpt = run.results.problems.Head.correlationOpt
                    }
                let unrealizable =
                    {
                        run.results.realizability.Head with
                            eigenvalues = [ CoherencyEigenvalue 0.9; CoherencyEigenvalue 0.2; CoherencyEigenvalue 0.1; CoherencyEigenvalue -0.1 ]
                    }
                let variant =
                    {
                        run with
                            results =
                                {
                                    run.results with problems = [ elementProblem ]; realizability = [ unrealizable ]; sigmas = []
                                }
                    }
                let variantModel =
                    {
                        selected with kitFitRuns = [ variant ]
                    }
                withStudyWindow (variantModel |> updateTableAndElementRotation (ShowResultsTab CorrelationResultsTab)) (fun window ->
                    Assert.Contains("m12", resultTexts window)
                    Assert.Contains("m30", resultTexts window))
                withStudyWindow (variantModel |> updateTableAndElementRotation (ShowResultsTab RealizabilityResultsTab)) (fun window ->
                    Assert.Equal<string list>([ "CPL1: not realizable; eigenvalues 0.9, 0.2, 0.1, -0.1" ], resultTexts window))
                withStudyWindow (variantModel |> updateTableAndElementRotation (ShowResultsTab ValuesResultsTab)) (fun window ->
                    Assert.Equal(2, resultTexts window |> List.filter ((=) "undetermined") |> List.length))
                let empty =
                    {
                        loaded with kitFitRuns = []
                    }
                withStudyWindow empty (fun window ->
                    let panel = SlotBarTests.controlById window ConstructorUiId.studyResultsPanel
                    let texts = panel.GetVisualDescendants() |> Seq.choose (function :? TextBlock as text -> Some text.Text | _ -> None)
                    Assert.Contains("No fit run yet: run the fit in the Fit step.", texts)
                    Assert.Empty(SlotBarTests.controlsUnder window ConstructorUiId.fitResultsTabStrip))
            finally
                System.Globalization.CultureInfo.CurrentCulture <- previousCulture
                model.kit.close () |> SceneTestFixture.kitSuccess)

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``pressing each results tab shows its table`` () : unit =
        HeadlessSession.run (fun () ->
            let model = TestAppScope.studyKitIn owner
            try
                let run = resultsRunOf model
                let shown = fitModelWith [ run ] model |> updateTableAndElementRotation (ShowFlowStep ResultsFlowStep)
                withStudyWindow shown (fun window ->
                    for tab in FitResultsTab.all @ [ ValuesResultsTab ] do
                        pressStudyBox window tab.tabUiId
                        let table = SlotBarTests.controlById window ConstructorUiId.fitResultsTable
                        Assert.Equal(tab.caption.value, AutomationProperties.GetName table)
                        Assert.Equal(MaterialsWindowView.positiveBackground, fitBoxColor window tab.tabUiId)
                        let expected =
                            match tab with
                            | ValuesResultsTab -> "CPL1 c2.retardance"
                            | CorrelationResultsTab -> "Study residual: rank 2, condition 12.5, χ² 0.5, RMSE 0.01"
                            | ResidualsResultsTab -> "E1: RMSE 0.0158 over 2 residual(s)"
                            | RealizabilityResultsTab -> "CPL1: realizable; eigenvalues 0.97, 0.01, 0.002, -1E-14"
                            | ProductCheckResultsTab -> "CPL1 → CPL2: product RMSE 0.012, fit RMSE 0.01, ratio 1.2, max |ΔM| 0.002"
                            | ProfileResultsTab -> "3: χ² 6.75"
                        Assert.Contains(expected, resultTexts window))
                Assert.Equal<FitRun list>([ run ], model.kit.proxy.readFitRuns run.study |> SceneTestFixture.kitSuccess)
            finally model.kit.close () |> SceneTestFixture.kitSuccess)

    [<Fact>]
    let ``the Publish step previews the chosen run's calibration and lists what it can promote`` () : unit =
        HeadlessSession.run (fun () ->
            let model = TestAppScope.studyKitIn owner
            let previousCulture = System.Globalization.CultureInfo.CurrentCulture
            try
                System.Globalization.CultureInfo.CurrentCulture <- System.Globalization.CultureInfo.GetCultureInfo "fr-FR"
                let run = calibrationViewRunOf model
                model.kit.proxy.writeFitRun run |> SceneTestFixture.kitSuccess
                let loaded =
                    model
                    |> updateTableAndElementRotation ReloadKitChanges
                    |> updateTableAndElementRotation (ShowFlowStep PublishFlowStep)
                withStudyWindow loaded (fun window ->
                    Assert.Equal<string list>([ "CPL1 → revision 2: c2.retardance = 1.55 ± 0.002" ],
                                              studyPanelTexts window ConstructorUiId.publishCalibrationPreview)
                    Assert.Equal("Publish calibration", SlotBarTests.boxLabelOf window ConstructorUiId.publishCalibrationButton)
                    Assert.Equal(MaterialsWindowView.positiveBackground, fitBoxColor window ConstructorUiId.publishCalibrationButton)
                    Assert.Equal<string list>([ "This fit run fitted no material or black box." ], studyPanelTexts window ConstructorUiId.promotionList)
                    let texts = studyPanelTexts window ConstructorUiId.studyPublishPanel
                    Assert.Contains($"Fit run {run.name.value}", texts)
                    Assert.Contains("Publishing writes each item's next revision; the values it replaces stay in the item's revision history.", texts)
                    Assert.Contains("Promoting mints a new library version under the fit's provenance; the version the study opened stays as it is.", texts))
                let invalidRun =
                    {
                        run with
                            results =
                                {
                                    run.results with sigmas = []
                                }
                    }
                let invalid =
                    {
                        loaded with kitFitRuns = [ invalidRun ]
                    }
                let study = List.exactlyOne invalid.kitStudies
                let error =
                    match study.calibratedItemsWith invalidRun invalid.kitItems with
                    | Error error -> error
                    | Ok _ -> failwith "A missing sigma must refuse publication"
                withStudyWindow invalid (fun window ->
                    Assert.Equal<string list>([ error.describe ], studyPanelTexts window ConstructorUiId.publishCalibrationPreview)
                    let panel = Assert.IsType<StackPanel>(SlotBarTests.controlById window ConstructorUiId.publishCalibrationPreview)
                    let text = Assert.IsType<TextBlock>(Assert.Single panel.Children)
                    Assert.Equal(MaterialsWindowView.messageColor, Assert.IsType<SolidColorBrush>(text.Foreground).Color)
                    SlotBarTests.assertAbsent window ConstructorUiId.publishCalibrationButton)
                let empty =
                    {
                        loaded with kitFitRuns = []
                    }
                withStudyWindow empty (fun window ->
                    Assert.Contains("No fit run yet: run the fit in the Fit step.", studyPanelTexts window ConstructorUiId.studyPublishPanel)
                    SlotBarTests.assertAbsent window ConstructorUiId.publishCalibrationButton
                    SlotBarTests.assertAbsent window ConstructorUiId.promotionList)
                let promoted = StudyPublishViewFixture.publicationModelWithSubjects loaded
                let second = promoted.selectedFitRunOpt |> Option.defaultWith (fun () -> failwith "The second run must be selected")
                let opened = List.exactlyOne (List.exactlyOne promoted.kitStudies).materials
                let blackBox = itemNamed (ItemLabel "QZ") promoted
                let subjects = [ MaterialPromotion opened.version; BlackBoxPromotion blackBox.id ]
                let originalMaterial = promoted.materials.resolveVersion opened.version
                withStudyWindow promoted (fun window ->
                    let texts = studyPanelTexts window ConstructorUiId.promotionList
                    Assert.Contains("Study absorbing crystal v1 → its next library version", texts)
                    Assert.Contains("QZ (black box) → a custom-Mueller polarizer", texts)
                    for subject in subjects do
                        let id = ConstructorUiId.promoteButton subject
                        Assert.Equal("Promote", SlotBarTests.boxLabelOf window id)
                        Assert.Equal(MaterialsWindowView.positiveBackground, fitBoxColor window id)
                        pressStudyBox window id
                        let status = Assert.IsType<TextBlock>(SlotBarTests.controlById window ConstructorUiId.studyStatusLine).Text
                        Assert.StartsWith("Promoted to the library as ", status))
                Assert.Equal<Result<MaterialEntry option, MaterialError>>(originalMaterial, promoted.materials.resolveVersion opened.version)
                let envelopes = promoted.kit.proxy.listSnapshots () |> SceneTestFixture.kitSuccess
                for entryId in [ opened.version.materialId.toLibraryEntryId; blackBox.id.toLibraryEntryId ] do
                    let envelope = envelopes |> List.filter (fun envelope -> envelope.content.id = entryId) |> List.maxBy _.content.version
                    Assert.Equal<EntryProvenance>(second.provenanceIn promoted.kit.kitFile.id, envelope.content.provenance)
                let earlier = promoted |> updateTableAndElementRotation (SelectFitRun run.name)
                withStudyWindow earlier (fun window ->
                    Assert.Equal<string list>([ "CPL1 → revision 2: c2.retardance = 1.55 ± 0.002" ],
                                              studyPanelTexts window ConstructorUiId.publishCalibrationPreview)
                    Assert.Equal<string list>([ "This fit run fitted no material or black box." ], studyPanelTexts window ConstructorUiId.promotionList))
                Assert.Equal<FitRun list>([ run; second ], model.kit.proxy.readFitRuns run.study |> SceneTestFixture.kitSuccess)
            finally
                System.Globalization.CultureInfo.CurrentCulture <- previousCulture
                model.kit.close () |> SceneTestFixture.kitSuccess)

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``pressing Publish calibration writes the calibrated items' next revisions into the kit`` () : unit =
        HeadlessSession.run (fun () ->
            let model = TestAppScope.studyKitIn owner
            try
                let run = calibrationViewRunOf model
                model.kit.proxy.writeFitRun run |> SceneTestFixture.kitSuccess
                let loaded = model |> updateTableAndElementRotation ReloadKitChanges
                let first = itemNamed (ItemLabel "CPL1") loaded
                let file = KitRelativePath "CPL1.item-json"
                let before = model.kit.proxy.readLabFile file |> SceneTestFixture.kitSuccess
                withStudyWindow loaded (fun window ->
                    pressStudyBox window ConstructorUiId.flowPublishStep
                    pressStudyBox window ConstructorUiId.publishCalibrationButton
                    Assert.Equal("Published the calibration of CPL1 from QZ-LR@20261101T120000Z.",
                                 Assert.IsType<TextBlock>(SlotBarTests.controlById window ConstructorUiId.studyStatusLine).Text)
                    SlotBarTests.assertAbsent window ConstructorUiId.publishCalibrationButton)
                let recorded = model.kit.proxy.readItems () |> SceneTestFixture.kitSuccess
                let published = recorded |> List.find (fun item -> item.id = first.id)
                let calibration : CalibratedParameterData =
                    {
                        value = ParameterValue 1.55
                        sigma = ParameterSigma 0.002
                        fitRun = run.name
                    }
                Assert.Equal<ItemRevision>(ItemRevision 2, published.revision)
                Assert.Equal<ParameterState option>(Some (CalibratedParameter calibration), published.tryFindParameter retardance |> Option.map _.state)
                let history : ItemRevisionRecord =
                    {
                        revision = first.revision
                        parameters = first.parameters
                    }
                Assert.Equal<ItemRevisionRecord list>([ history ], published.history)
                let after = model.kit.proxy.readLabFile file |> SceneTestFixture.kitSuccess
                Assert.NotEqual<FileByteContent>(before.content, after.content)
                Assert.Equal<Item list>(loaded.kitItems |> List.filter (fun item -> item.id <> first.id),
                                        recorded |> List.filter (fun item -> item.id <> first.id))
                Assert.Equal<FitRun list>([ run ], model.kit.proxy.readFitRuns run.study |> SceneTestFixture.kitSuccess)
            finally model.kit.close () |> SceneTestFixture.kitSuccess)

    [<Fact>]
    let ``the Fit step offers Run joint and the two stages on one row and lists the study's fit runs, oldest first`` () : unit =
        HeadlessSession.run (fun () ->
            let model = TestAppScope.studyKitIn owner
            try
                let study = List.exactlyOne model.kitStudies
                let first = StudyFitViewFixture.fitViewRunOf study StudyFitViewFixture.firstFitViewRunStamp
                let secondStamp = ClockTime (first.stamp.value.AddHours 1.0)
                let second = StudyFitViewFixture.fitViewRunOf study secondStamp
                let shown = fitModelWith [ second; first ] model
                withStudyWindow shown (fun window ->
                    let ids = [ ConstructorUiId.studyRunFitButton; ConstructorUiId.studyRunStageOneButton; ConstructorUiId.studyRunStageTwoButton ]
                    let buttons = ids |> List.map (SlotBarTests.controlById window)
                    let row = Assert.IsType<StackPanel>(buttons.Head.Parent)
                    Assert.Equal<Orientation>(Orientation.Horizontal, row.Orientation)
                    Assert.Equal<Control list>(buttons, row.Children |> Seq.toList)
                    Assert.Equal<string list>([ "Run joint"; "Run stage 1 (calibration)"; "Run stage 2 (objects)" ], ids |> List.map (SlotBarTests.boxLabelOf window))
                    Assert.Equal<string list>([ "Run joint"; "Run stage 1 (calibration)"; "Run stage 2 (objects)" ], buttons |> List.map AutomationProperties.GetName)
                    for button in buttons do
                        Assert.Same(row, button.Parent)
                        Assert.True button.IsEffectivelyEnabled
                    for left, right in List.pairwise buttons do
                        Assert.True(left.Bounds.Right < right.Bounds.Left, "Fit verbs must run left to right with a gap")
                        Assert.Equal(left.Bounds.Y, right.Bounds.Y)
                    Assert.Equal("fixed", SlotBarTests.boxLabelOf window ConstructorUiId.studyCalibrationFixedChoice)
                    Assert.Equal("as priors", SlotBarTests.boxLabelOf window ConstructorUiId.studyCalibrationPriorChoice)
                    let fixedBox = SlotBarTests.controlById window ConstructorUiId.studyCalibrationFixedChoice
                    let priorBox = SlotBarTests.controlById window ConstructorUiId.studyCalibrationPriorChoice
                    Assert.Same(fixedBox.Parent, priorBox.Parent)
                    Assert.True(fixedBox.Bounds.Right < priorBox.Bounds.Left)
                    Assert.True((SlotBarTests.topLeftOf window fixedBox).Y >= (SlotBarTests.topLeftOf window buttons.Head).Y + buttons.Head.Bounds.Height)
                    Assert.Equal(MaterialsWindowView.positiveBackground, fitBoxColor window ConstructorUiId.studyCalibrationFixedChoice)
                    Assert.NotEqual(MaterialsWindowView.positiveBackground, fitBoxColor window ConstructorUiId.studyCalibrationPriorChoice)
                    let history = Assert.IsType<StackPanel>(SlotBarTests.controlById window ConstructorUiId.fitRunHistory)
                    Assert.Equal<string list>(
                        [ (ConstructorUiId.fitRunRow first.name).value; (ConstructorUiId.fitRunRow second.name).value ],
                        history.Children |> Seq.map AutomationProperties.GetAutomationId |> List.ofSeq)
                    for run in [ first; second ] do
                        Assert.Equal($"{run.name.value} — {run.stage.text}, {run.engine.text}, {run.runs.Length} run(s)",
                                     SlotBarTests.boxLabelOf window (ConstructorUiId.fitRunRow run.name))
                    let prefits = Assert.IsType<StackPanel>(SlotBarTests.controlById window ConstructorUiId.studyPrefitList)
                    Assert.Equal<string list>(study.experiments |> List.map (fun name -> (ConstructorUiId.studyPrefitRow name).value),
                                             prefits.Children |> Seq.map AutomationProperties.GetAutomationId |> List.ofSeq)
                    Assert.Equal("E1: ready, 4 point(s)", Assert.IsType<TextBlock>(prefits.Children[0]).Text)
                    Assert.Contains("E2: no data is attached:", Assert.IsType<TextBlock>(prefits.Children[1]).Text)
                    Assert.Equal(MaterialsWindowView.messageColor, Assert.IsType<SolidColorBrush>(Assert.IsType<TextBlock>(prefits.Children[1]).Foreground).Color)
                    Assert.Equal("E3: ready, 4 point(s)", Assert.IsType<TextBlock>(prefits.Children[2]).Text)
                    pressStudyBox window (ConstructorUiId.fitRunRow first.name)
                    Assert.Equal(MaterialsWindowView.positiveBackground, fitBoxColor window (ConstructorUiId.fitRunRow first.name))
                    pressStudyBox window (ConstructorUiId.fitRunRow second.name)
                    Assert.Equal(MaterialsWindowView.positiveBackground, fitBoxColor window (ConstructorUiId.fitRunRow second.name))
                    Assert.NotEqual(MaterialsWindowView.positiveBackground, fitBoxColor window (ConstructorUiId.fitRunRow first.name)))
                let attachment =
                    (model.experimentCollection.experiments |> List.find (fun experiment -> experiment.name = ExperimentName "E1")).dataOpt
                    |> Option.map _.attachment
                    |> Option.defaultWith (fun () -> failwith "E1 must be attached")
                let olderData =
                    {
                        attachment with contentHash = ContentHash (String.replicate 64 "0")
                    }
                let latestWithOlderData =
                    {
                        second with dataRead = [ olderData ]
                    }
                let changed =
                    {
                        shown with kitFitRuns = [ first; latestWithOlderData ]
                    }
                withStudyWindow changed (fun window ->
                    let e1 = ExperimentName "E1"
                    let text = Assert.IsType<TextBlock>(SlotBarTests.controlById window (ConstructorUiId.studyPrefitRow e1)).Text
                    Assert.Equal("E1: ready, 4 point(s) — data changed since the last fit run", text))
                let emptyStudy =
                    {
                        study with experiments = []
                    }
                let empty =
                    {
                        shown with
                            kitFitRuns = []
                            kitStudies = [ emptyStudy ]
                    }
                withStudyWindow empty (fun window ->
                    let prefits = Assert.IsType<StackPanel>(SlotBarTests.controlById window ConstructorUiId.studyPrefitList)
                    Assert.Equal("The study holds no run a fit reads.", Assert.IsType<TextBlock>(Assert.Single prefits.Children).Text)
                    let history = Assert.IsType<StackPanel>(SlotBarTests.controlById window ConstructorUiId.fitRunHistory)
                    Assert.Equal("No fit run yet.", Assert.IsType<TextBlock>(Assert.Single history.Children).Text))
                let missing = ExperimentName "Absent"
                let invalidStudy =
                    {
                        study with experiments = [ missing ]
                    }
                let invalid =
                    {
                        shown with kitStudies = [ invalidStudy ]
                    }
                withStudyWindow invalid (fun window ->
                    let prefits = Assert.IsType<StackPanel>(SlotBarTests.controlById window ConstructorUiId.studyPrefitList)
                    let text = Assert.IsType<TextBlock>(Assert.Single prefits.Children)
                    Assert.Contains(missing.value, text.Text)
                    Assert.Equal(MaterialsWindowView.messageColor, Assert.IsType<SolidColorBrush>(text.Foreground).Color))
            finally model.kit.close () |> SceneTestFixture.kitSuccess)

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``pressing Run joint starts one fit through the shell's host, and Cancel cancels it`` () : unit =
        HeadlessSession.run (fun () ->
            let model = TestAppScope.studyKitIn owner
            try
                let study = List.exactlyOne model.kitStudies
                let first = StudyFitViewFixture.fitViewRunOf study StudyFitViewFixture.firstFitViewRunStamp
                let secondStamp = ClockTime (first.stamp.value.AddHours 1.0)
                let second = StudyFitViewFixture.fitViewRunOf study secondStamp
                let loaded = fitModelWith [ second; first ] model
                let jobs = ResizeArray<IterationWatch -> Result<FitRun, StudyFitError>> ()
                let cancels = ResizeArray<unit> ()
                let host : StudyFitJobHost =
                    {
                        startStudyFit = fun job ->
                            jobs.Add job
                            {
                                cancelStudyFit = fun () -> cancels.Add ()
                            }
                    }
                let initial =
                    {
                        loaded with
                            kitShell =
                                {
                                    loaded.kitShell with studyFits = host
                                }
                    }
                let messages = ResizeArray<TableAndElementRotationMsg> ()
                let mutable send : TableAndElementRotationMsg -> unit = ignore
                let update (message : TableAndElementRotationMsg) (state : TableAndElementRotationModel) : TableAndElementRotationModel =
                    messages.Add message
                    updateTableAndElementRotation message state
                let view (state : TableAndElementRotationModel) (dispatch : TableAndElementRotationMsg -> unit) : Avalonia.FuncUI.Types.IView =
                    send <- dispatch
                    KitWindowView.kitWindowView state dispatch
                let window = HostWindow()
                Program.mkSimple (fun () -> initial) update view |> Program.withHost window |> Program.run
                window.Width <- 1000.0
                window.Height <- 700.0
                let report (message : StudyFitMsg) : unit =
                    send (StudyFit message)
                    Dispatcher.UIThread.RunJobs()
                let assertRequest (stage : FitStage) (calibrationUse : CalibrationUse) : unit =
                    let request =
                        messages |> Seq.choose (function StudyFit (RunFit request) -> Some request | _ -> None) |> Seq.last
                    Assert.Equal<StudyName>(study.name, request.study)
                    Assert.Equal<FitStage>(stage, request.stage)
                    Assert.Equal<CalibrationUse>(calibrationUse, request.calibrationUse)
                try
                    window.Show()
                    Dispatcher.UIThread.RunJobs()
                    pressStudyBox window ConstructorUiId.studyCalibrationPriorChoice
                    Assert.Equal(MaterialsWindowView.positiveBackground, fitBoxColor window ConstructorUiId.studyCalibrationPriorChoice)
                    pressStudyBox window ConstructorUiId.studyRunFitButton
                    Assert.Single jobs |> ignore
                    assertRequest JointStage CalibrationAsPrior
                    Assert.True(Assert.IsType<ProgressBar>(SlotBarTests.controlById window ConstructorUiId.studyFitProgressBar).IsIndeterminate)
                    SlotBarTests.controlById window ConstructorUiId.studyFitCancelButton |> ignore
                    for id in [ ConstructorUiId.studyRunFitButton; ConstructorUiId.studyRunStageOneButton; ConstructorUiId.studyRunStageTwoButton ] do
                        Assert.False((SlotBarTests.controlById window id).IsEffectivelyEnabled)
                    let progress : JobProgress =
                        {
                            completed = JobStepCount 3
                            total = None
                            elapsed = JobElapsed (System.TimeSpan.FromSeconds 0.2)
                        }
                    report (StudyFitProgressed progress)
                    Assert.Contains("3 iterations", Assert.IsType<TextBlock>(SlotBarTests.controlById window ConstructorUiId.studyFitProgressText).Text)
                    pressStudyBox window ConstructorUiId.studyFitCancelButton
                    Assert.Single cancels |> ignore
                    Assert.Single jobs |> ignore
                    report (StudyFitEnded (Error (FitCancelledErr "cancelled by the test host")))
                    SlotBarTests.assertAbsent window ConstructorUiId.studyFitProgressBar
                    SlotBarTests.assertAbsent window ConstructorUiId.studyFitCancelButton
                    Assert.Equal<FitRun list>([ first; second ], model.kit.proxy.readFitRuns study.name |> SceneTestFixture.kitSuccess)
                    pressStudyBox window ConstructorUiId.studyCalibrationFixedChoice
                    pressStudyBox window ConstructorUiId.studyRunStageOneButton
                    Assert.Equal(2, jobs.Count)
                    assertRequest CalibrationStage CalibrationHeldFixed
                    let completedStamp = ClockTime (second.stamp.value.AddHours 1.0)
                    let completed : FitRun =
                        {
                            StudyFitViewFixture.fitViewRunOf study completedStamp with stage = CalibrationStage
                        }
                    report (StudyFitEnded (Ok completed))
                    let history = Assert.IsType<StackPanel>(SlotBarTests.controlById window ConstructorUiId.fitRunHistory)
                    Assert.Equal(3, history.Children.Count)
                    Assert.Equal(MaterialsWindowView.positiveBackground, fitBoxColor window (ConstructorUiId.fitRunRow completed.name))
                    Assert.Equal<FitRun list>([ first; second; completed ], model.kit.proxy.readFitRuns study.name |> SceneTestFixture.kitSuccess)
                    SlotBarTests.assertAbsent window ConstructorUiId.studyFitProgressBar
                    pressStudyBox window ConstructorUiId.studyCalibrationPriorChoice
                    pressStudyBox window ConstructorUiId.studyRunStageTwoButton
                    Assert.Equal(3, jobs.Count)
                    assertRequest ObjectStage CalibrationAsPrior
                    let error = FitRunRaisedErr "recorded fit failed"
                    report (StudyFitEnded (Error error))
                    let errorText = Assert.IsType<TextBlock>(SlotBarTests.controlById window ConstructorUiId.studyFitErrorText)
                    Assert.Equal(error.describe, errorText.Text)
                    Assert.Equal(MaterialsWindowView.messageColor, Assert.IsType<SolidColorBrush>(errorText.Foreground).Color)
                    SlotBarTests.assertAbsent window ConstructorUiId.studyFitCancelButton
                    Assert.Equal(3, Assert.IsType<StackPanel>(SlotBarTests.controlById window ConstructorUiId.fitRunHistory).Children.Count)
                finally
                    window.Close()
                    Dispatcher.UIThread.RunJobs()
            finally model.kit.close () |> SceneTestFixture.kitSuccess)
