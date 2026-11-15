namespace OpticalConstructor.Ui.Tests

open System.IO
open Avalonia
open Avalonia.Automation
open Avalonia.Controls
open Avalonia.FuncUI.Hosts
open Avalonia.FuncUI.Elmish
open Avalonia.Headless
open Avalonia.Input
open Avalonia.Threading
open Avalonia.VisualTree
open Elmish
open Xunit
open Softellect.Sys.Primitives
open OpticalConstructor.Controls
open OpticalConstructor.Domain.Experiments
open OpticalConstructor.Domain.ItemModels
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.Studies
open OpticalConstructor.Storage.KitContentFiles
open OpticalConstructor.Tests
open OpticalConstructor.Ui
open OpticalConstructor.Ui.TableAndElementRotationView

/// The Unknowns step's controls and persisted study edits, driven by stable IDs (spec 0050 S11.5).
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
        let study = StudyName "QZ-LR"
        let experiment = ExperimentName "E1"
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
