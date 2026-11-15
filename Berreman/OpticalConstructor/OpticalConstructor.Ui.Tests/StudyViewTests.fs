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
open OpticalConstructor.Domain.PrefitChecks
open OpticalConstructor.Domain.Studies
open OpticalConstructor.Storage.KitContentFiles
open OpticalConstructor.Tests
open OpticalConstructor.Ui
open OpticalConstructor.Ui.TableAndElementRotationView
open OpticalConstructor.Ui.StudyFitJob
open OpticalConstructor.Ui.JobRunner
open OpticalConstructor.Optimization.OptimizationInterface
open OpticalConstructor.Optimization.StudyEngine

/// The study's Unknowns and Fit controls, driven by stable IDs (spec 0050 S11.5).
module StudyViewTests =

    let private owner = FolderName "StudyViewTests"
    let private retardance = ParameterName "c2.retardance"

    let private itemNamed (label : ItemLabel) (model : TableAndElementRotationModel) : Item =
        model.kitItems |> List.find (fun item -> item.label = label)

    let private withStudyWindow (model : TableAndElementRotationModel) (check : Window -> unit) : unit =
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
