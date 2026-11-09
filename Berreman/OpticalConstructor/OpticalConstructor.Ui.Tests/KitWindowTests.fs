namespace OpticalConstructor.Ui.Tests

open System.IO
open Avalonia
open Avalonia.Automation
open Avalonia.Controls
open Avalonia.FuncUI.Types
open Avalonia.Headless
open Avalonia.Input
open Avalonia.Media
open Avalonia.Threading
open Avalonia.VisualTree
open Xunit
open Softellect.Sys.Primitives
open OpticalConstructor.Controls
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.KitFolders
open OpticalConstructor.Storage.KitContentFiles
open OpticalConstructor.Storage.KitFolderStore
open OpticalConstructor.Tests
open OpticalConstructor.Ui
open OpticalConstructor.Ui.KitOpening
open OpticalConstructor.Ui.TableAndElementRotationView
open OpticalConstructor.App
open Avalonia.FuncUI.Hosts
open Avalonia.FuncUI.Elmish
open global.Elmish

module KitWindowTests =

    let private owner : FolderName = FolderName "KitWindowTests"

    let private kitAuthor : KitAuthor = KitAuthor "test"

    /// Shows the window headless, asserts its kit title, then closes it and checks its kit is released (spec 0050 S5.3).
    let private assertTitledByKit (window : #Window) (opened : OpenedKit) (constructorName : string) : unit =
        let lockFile = Path.Combine(opened.folder.value.value, ".oc", "lock")
        let expected = $"{opened.library.kit.kitFile.name.value} — {opened.folder.value.value} — Optical Constructor — {constructorName}"
        try
            window.Show()
            Dispatcher.UIThread.RunJobs()
            Assert.Equal(expected, window.Title)
        finally
            window.Close()
            Dispatcher.UIThread.RunJobs()
        Assert.False(File.Exists(lockFile), "closing the constructor window must release its kit")

    /// The experiment `E1` the kit holds before a window opens it.
    let private storedExperiment : Experiments.Experiment =
        {
            id = Experiments.ExperimentId 1
            setup = []
            varied = None
            measurement = Experiments.CaptureTransmitted
            range = Experiments.VariableRange.forVariable Experiments.VaryR1
            dataFileOpt = None
            sourceWaveLengthOpt = None
        }

    /// The controls that send the kit's write messages: Add, `E1`'s Remove and Attach data file, and Save scene.
    let private writeControlIds : ConstructorUiId list =
        [
            ConstructorUiId.experimentAddButton
            ConstructorUiId.experimentRemoveButton storedExperiment.id.uiKey
            ConstructorUiId.attachDataFile storedExperiment.id
            ConstructorUiId.saveSceneButton
        ]

    let private openedAt (folder : KitFolder) (access : KitAccess) (ctx : AppContext) : OpenedKit =
        openKitAt folder access ClockProvider.system kitAuthor ctx.catalogueImports
        |> Result.defaultWith (fun error -> failwith error.describe)

    /// Every control in the window carrying `id` as its AutomationId.
    let private controlsById (window : Window) (id : ConstructorUiId) : Control list =
        window.GetVisualDescendants()
        |> Seq.choose (function :? Control as control -> Some control | _ -> None)
        |> Seq.filter (fun control -> AutomationProperties.GetAutomationId control = id.value)
        |> List.ofSeq

    let private tryControlById (window : Window) (id : ConstructorUiId) : Control option =
        controlsById window id |> List.tryHead

    let private controlById (window : Window) (id : ConstructorUiId) : Control =
        tryControlById window id
        |> Option.defaultWith (fun () -> failwith $"{id.value} must be in the window")

    /// The text of the `TextBlock` carrying `id`.
    let private textOf (window : Window) (id : ConstructorUiId) : string =
        Assert.IsType<TextBlock>(controlById window id).Text

    /// The label of the verb or action box carrying `id`: the text of the box's child.
    let private boxLabelOf (window : Window) (id : ConstructorUiId) : string =
        Assert.IsType<TextBlock>(Assert.IsType<Border>(controlById window id).Child).Text

    /// Hosts `view` over `model` in a FuncUI window, as `ScrollExtentTests.constructorWindow` mounts a view.
    let private hostedWindow
        (view : TableAndElementRotationModel -> (TableAndElementRotationMsg -> unit) -> IView)
        (model : TableAndElementRotationModel)
        : HostWindow =
        let window = HostWindow()
        Program.mkSimple (fun () -> model) updateTableAndElementRotation view |> Program.withHost window |> Program.run
        window

    /// Shows `kitWindowView` over `model` headless at 1000 × 700, runs `check` over the window, then closes it
    /// (spec 0050 S5.3).
    let private withKitWindow (model : TableAndElementRotationModel) (check : Window -> unit) : unit =
        let window = hostedWindow KitWindowView.kitWindowView model
        window.Width <- 1000.0
        window.Height <- 700.0
        try
            window.Show()
            Dispatcher.UIThread.RunJobs()
            check window
        finally
            window.Close()
            Dispatcher.UIThread.RunJobs()

    /// Presses `pressed` at its centre, as `LibraryWindowTests.clickOn` does; a press that closes the window gets no
    /// release, as `LauncherTests.clickBorder` does.
    let private pressAtCentre (window : Window) (pressed : Border) (name : string) : unit =
        Assert.True(pressed.IsEffectivelyVisible, $"{name} must be visible")
        pressed.BringIntoView() |> ignore
        Dispatcher.UIThread.RunJobs()
        match pressed.TranslatePoint(Point(pressed.Bounds.Width / 2.0, pressed.Bounds.Height / 2.0), window) |> Option.ofNullable with
        | Some centre ->
            window.MouseDown(centre, MouseButton.Left, RawInputModifiers.None)
            Dispatcher.UIThread.RunJobs()
            if window.IsVisible then
                window.MouseUp(centre, MouseButton.Left, RawInputModifiers.None)
                Dispatcher.UIThread.RunJobs()
        | None -> Assert.Fail $"{name} has no position in its window"

    /// Presses the box carrying `id` at its centre.
    let private pressBox (window : Window) (id : ConstructorUiId) : unit =
        pressAtCentre window (Assert.IsType<Border>(controlById window id)) id.value

    /// Presses the ribbon tab of `bay`, which carries its id as its `Name` (`Ribbon.tab`).
    let private pressRibbonTab (window : Window) (bay : Ribbon.BayName) : unit =
        let id = ConstructorUiId.ribbonTab bay.uiKey
        let tab =
            window.GetVisualDescendants()
            |> Seq.tryPick (function :? Border as border when border.Name = id.value -> Some border | _ -> None)
            |> Option.defaultWith (fun () -> failwith $"{id.value} must be in the window")
        pressAtCentre window tab id.value

    /// A shell that records every folder it is asked to show and every window close it is asked for. The step-186
    /// shell is private to `OpticalConstructor.Tests`, which this project does not reference.
    let private recordingShell (shown : ResizeArray<FolderName>) (closes : ResizeArray<unit>) : KitWindowShell =
        {
            KitWindowShell.explorer = ExplorerProxy.createInMemory shown.Add
            requestClose = fun () -> closes.Add ()
        }

    /// The forward or inverse constructor loaded from the opened kit, its Kit menu reaching `shell` (spec 0050 S5.3).
    let private kitWindowModel (ctx : AppContext) (opened : OpenedKit) (mode : ConstructorMode) (shell : KitWindowShell) : TableAndElementRotationModel =
        let seed =
            match mode with
            | ForwardConstructor -> initMainWithKit ctx.library ctx.experiments ctx.materials ctx.samples ctx.categories opened.library.kit
            | InverseConstructor -> initInverseWithKit ctx.library ctx.experiments ctx.materials ctx.samples ctx.categories opened.library.kit
        let seeded =
            {
                seed with
                    catalogueImports = ctx.catalogueImports
                    sources = ctx.sources
            }
        let loaded = seeded.loadKit opened
        {
            loaded with kitShell = shell
        }

    /// Mounts `mainView` over the inverse model loaded from the kit opened with `access`, with a draft ready to
    /// commit and the Experiments bay selected, and checks every write control against `expected` (spec 0050 S5.3).
    let private assertWriteControls (folder : KitFolder) (ctx : AppContext) (access : KitAccess) (expected : ControlAvailability) : unit =
        let opened = openedAt folder access ctx
        try
            let loaded = kitWindowModel ctx opened InverseConstructor KitWindowShell.inert
            let sample = loaded.elements |> List.find (fun element -> element.placement.catalogueKind = Placement.Sample)
            let model =
                loaded
                |> updateTableAndElementRotation (ExpChooseElement sample.id)
                |> updateTableAndElementRotation (ExpChooseVariable ExperimentControls.VaryR2)
                |> updateTableAndElementRotation (SelectBay BayNames.experiments)
            Assert.Equal<Experiments.Experiment list>([ storedExperiment ], model.experimentCollection.experiments)
            Assert.True(model.experimentCollection.canCommit, "the draft must be ready to commit")
            let window = hostedWindow mainView model
            try
                window.Show()
                Dispatcher.UIThread.RunJobs()
                for id in writeControlIds do
                    let control = controlById window id
                    match expected with
                    | ControlOffered -> Assert.True(control.IsEnabled, $"{id.value} must be enabled over a read-write kit")
                    | ControlWithheld -> Assert.False(control.IsEnabled, $"{id.value} must be disabled over a read-only kit")
            finally
                window.Close()
                Dispatcher.UIThread.RunJobs()
        finally opened.library.kit.close () |> SceneTestFixture.kitSuccess

    [<Fact>]
    let ``a constructor window is titled by its kit`` () : unit =
        HeadlessSession.run (fun () ->
            let ctx = TestAppScope.freshAppContext owner
            let mainKit = TestAppScope.openedKitIn owner ctx.catalogueImports
            assertTitledByKit (MainConstructorWindow(ctx, mainKit)) mainKit "Main"
            let inverseKit = TestAppScope.openedKitIn owner ctx.catalogueImports
            assertTitledByKit (InverseConstructorWindow(ctx, inverseKit)) inverseKit "Inverse")

    [<Fact>]
    let ``a read-only window withholds every write control`` () : unit =
        HeadlessSession.run (fun () ->
            let ctx = TestAppScope.freshAppContext owner
            let folder = TestAppScope.freshKitFolder owner
            let created = openedAt folder ReadWriteKitAccess ctx
            try created.library.kit.proxy.writeExperiment storedExperiment |> SceneTestFixture.kitSuccess
            finally created.library.kit.close () |> SceneTestFixture.kitSuccess
            assertWriteControls folder ctx ReadOnlyKitAccess ControlWithheld
            assertWriteControls folder ctx ReadWriteKitAccess ControlOffered)

    [<Fact>]
    let ``the kit window UiIds are the stable intent-named ids`` () : unit =
        Assert.Equal("KitBar", ConstructorUiId.kitBar.value)
        Assert.Equal("KitCaptionText", ConstructorUiId.kitCaptionText.value)
        Assert.Equal("KitShowInExplorerButton", ConstructorUiId.kitShowInExplorerButton.value)
        Assert.Equal("KitReloadChangesButton", ConstructorUiId.kitReloadChangesButton.value)
        Assert.Equal("KitCloseButton", ConstructorUiId.kitCloseButton.value)
        Assert.Equal("KitModeText", ConstructorUiId.kitModeText.value)
        Assert.Equal("KitSwitchModeButton", ConstructorUiId.kitSwitchModeButton.value)
        Assert.Equal("KitChangesText", ConstructorUiId.kitChangesText.value)
        Assert.Equal("KitChangesReloadButton", ConstructorUiId.kitChangesReloadButton.value)
        Assert.Equal("KitChangesKeepMineButton", ConstructorUiId.kitChangesKeepMineButton.value)

    [<Fact>]
    let ``the kit bar shows the kit, its Kit menu and the constructor`` () : unit =
        HeadlessSession.run (fun () ->
            let ctx = TestAppScope.freshAppContext owner
            let opened = TestAppScope.openedKitIn owner ctx.catalogueImports
            let shown = ResizeArray<FolderName> ()
            let closes = ResizeArray<unit> ()
            try
                let forward = kitWindowModel ctx opened ForwardConstructor (recordingShell shown closes)
                withKitWindow forward (fun window ->
                    Assert.Equal(forward.kitCaption.value, textOf window ConstructorUiId.kitCaptionText)
                    Assert.Equal("Show in Explorer", boxLabelOf window ConstructorUiId.kitShowInExplorerButton)
                    Assert.Equal("Reload changes", boxLabelOf window ConstructorUiId.kitReloadChangesButton)
                    Assert.Equal("Close kit", boxLabelOf window ConstructorUiId.kitCloseButton)
                    Assert.Equal("Main constructor", textOf window ConstructorUiId.kitModeText)
                    Assert.Equal("Switch to Inverse", boxLabelOf window ConstructorUiId.kitSwitchModeButton))
                let inverse = kitWindowModel ctx opened InverseConstructor (recordingShell shown closes)
                withKitWindow inverse (fun window ->
                    Assert.Equal(inverse.kitCaption.value, textOf window ConstructorUiId.kitCaptionText)
                    Assert.Equal("Inverse constructor", textOf window ConstructorUiId.kitModeText)
                    Assert.Equal("Switch to Main", boxLabelOf window ConstructorUiId.kitSwitchModeButton))
                Assert.Empty(shown)
                Assert.Empty(closes)
            finally opened.library.kit.close () |> SceneTestFixture.kitSuccess
            let scratch = initMain () |> SceneTestFixture.kitSuccess
            try
                withKitWindow scratch (fun window ->
                    Assert.Equal(scratch.kitCaption.value, textOf window ConstructorUiId.kitCaptionText)
                    Assert.Equal<Control option>(None, tryControlById window ConstructorUiId.kitShowInExplorerButton)
                    Assert.Equal("Reload changes", boxLabelOf window ConstructorUiId.kitReloadChangesButton)
                    Assert.Equal("Close kit", boxLabelOf window ConstructorUiId.kitCloseButton))
            finally scratch.kit.close () |> SceneTestFixture.kitSuccess)

    [<Fact>]
    let ``a changed kit offers Reload and Keep mine on one row, Keep mine to the right`` () : unit =
        HeadlessSession.run (fun () ->
            let ctx = TestAppScope.freshAppContext owner
            let opened = TestAppScope.openedKitIn owner ctx.catalogueImports
            try
                let model = kitWindowModel ctx opened ForwardConstructor (recordingShell (ResizeArray ()) (ResizeArray ()))
                let offerIds = [ ConstructorUiId.kitChangesText; ConstructorUiId.kitChangesReloadButton; ConstructorUiId.kitChangesKeepMineButton ]
                withKitWindow model (fun window ->
                    for id in offerIds do
                        Assert.Equal<Control option>(None, tryControlById window id))
                // Another writer (step 126) adds `E2` and renames the kit, so both of its files are offered (step 187).
                let writer = KitProxy.create opened.folder
                let e2 =
                    {
                        storedExperiment with id = Experiments.ExperimentId 2
                    }
                writer.writeExperiment e2 |> SceneTestFixture.kitSuccess
                let kitFile = writer.readKitFile () |> SceneTestFixture.kitSuccess
                let renamed =
                    {
                        kitFile with name = KitName "Renamed"
                    }
                writer.writeKitFile renamed |> SceneTestFixture.kitSuccess
                let offered = model |> updateTableAndElementRotation (CheckKitChanges KitCheckOnActivation)
                Assert.Equal<FileName list>([ FileName ".kit-json"; FileName "E2.experiment-json" ], offered.kitChanges)
                withKitWindow offered (fun window ->
                    Assert.Equal(
                        "Changed outside the app: .kit-json, E2.experiment-json. Reload them, or keep your version?",
                        textOf window ConstructorUiId.kitChangesText)
                    let reload = Assert.IsType<Border>(controlById window ConstructorUiId.kitChangesReloadButton)
                    let keepMine = Assert.IsType<Border>(controlById window ConstructorUiId.kitChangesKeepMineButton)
                    Assert.Equal("Reload", boxLabelOf window ConstructorUiId.kitChangesReloadButton)
                    Assert.Equal("Keep mine", boxLabelOf window ConstructorUiId.kitChangesKeepMineButton)
                    let row = Assert.IsType<StackPanel>(reload.Parent)
                    Assert.Same(row, keepMine.Parent)
                    Assert.Equal(Avalonia.Layout.Orientation.Horizontal, row.Orientation)
                    Assert.Equal(reload.Bounds.Top, keepMine.Bounds.Top)
                    Assert.True(keepMine.Bounds.Left > reload.Bounds.Right, "Keep mine must sit to the right of Reload, past a gap")
                    Assert.Equal(MaterialsWindowView.positiveBackground, Assert.IsType<SolidColorBrush>(reload.Background).Color)
                    Assert.Equal(MaterialsWindowView.negativeBackground, Assert.IsType<SolidColorBrush>(keepMine.Background).Color))
            finally opened.library.kit.close () |> SceneTestFixture.kitSuccess)

    [<Fact>]
    let ``the kit status shows in the kit bar whichever bay is open`` () : unit =
        HeadlessSession.run (fun () ->
            let ctx = TestAppScope.freshAppContext owner
            let opened = TestAppScope.openedKitIn owner ctx.catalogueImports
            try
                let model =
                    kitWindowModel ctx opened ForwardConstructor (recordingShell (ResizeArray ()) (ResizeArray ()))
                    |> updateTableAndElementRotation (CheckKitChanges KitCheckOnRequest)
                Assert.Equal<ControlCaption option>(Some (ControlCaption "No kit file changed outside the app."), model.kitStatus)
                for bay in [ BayNames.add; BayNames.experiments ] do
                    withKitWindow (model |> updateTableAndElementRotation (SelectBay bay)) (fun window ->
                        let status = Assert.IsType<TextBlock>(Assert.Single(controlsById window ConstructorUiId.kitStatus))
                        Assert.Equal("No kit file changed outside the app.", status.Text)
                        let bar = controlById window ConstructorUiId.kitBar
                        Assert.Contains<Visual>(bar, status.GetVisualAncestors()))
            finally opened.library.kit.close () |> SceneTestFixture.kitSuccess)

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``pressing the kit bar's buttons sends their messages`` () : unit =
        HeadlessSession.run (fun () ->
            let ctx = TestAppScope.freshAppContext owner
            let opened = TestAppScope.openedKitIn owner ctx.catalogueImports
            let shown = ResizeArray<FolderName> ()
            let closes = ResizeArray<unit> ()
            try
                let model = kitWindowModel ctx opened ForwardConstructor (recordingShell shown closes)
                withKitWindow model (fun window ->
                    pressBox window ConstructorUiId.kitSwitchModeButton
                    Assert.Equal("Inverse constructor", textOf window ConstructorUiId.kitModeText)
                    Assert.Equal("Switch to Main", boxLabelOf window ConstructorUiId.kitSwitchModeButton)
                    pressBox window ConstructorUiId.kitShowInExplorerButton
                    Assert.Equal<FolderName list>([ opened.folder.value ], List.ofSeq shown)
                    Assert.Equal<Control option>(None, tryControlById window ConstructorUiId.kitStatus)
                    pressBox window ConstructorUiId.kitReloadChangesButton
                    Assert.Equal("No kit file changed outside the app.", textOf window ConstructorUiId.kitStatus)
                    Assert.Empty(closes)
                    pressBox window ConstructorUiId.kitCloseButton
                    Assert.Equal(1, closes.Count)
                    Assert.Equal<FolderName list>([ opened.folder.value ], List.ofSeq shown))
            finally opened.library.kit.close () |> SceneTestFixture.kitSuccess)

    /// The experiment another writer adds to the kit: one committed in a scratch Main model, given the id `E2`. It
    /// varies a polarizer added to the scene, because the scene's unbound source varies nothing.
    let private outsideExperiment () : Experiments.Experiment =
        let scratch = initMain () |> SceneTestFixture.kitSuccess
        try
            let withPolarizer = scratch |> updateTableAndElementRotation (AddElement Placement.LinearPolarizer)
            let polarizer = withPolarizer.elements |> List.find (fun element -> element.placement.catalogueKind = Placement.LinearPolarizer)
            let committed =
                withPolarizer
                |> updateTableAndElementRotation (ExpChooseElement polarizer.id)
                |> updateTableAndElementRotation ExpCommit
            let experiment = Assert.Single(committed.experimentCollection.experiments)
            {
                experiment with id = Experiments.ExperimentId 2
            }
        finally scratch.kit.close () |> SceneTestFixture.kitSuccess

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``acceptance: activating a constructor window offers the kit files changed outside the app, and Reload reads them in`` () : unit =
        HeadlessSession.run (fun () ->
            let ctx = TestAppScope.freshAppContext owner
            let opened = TestAppScope.openedKitIn owner ctx.catalogueImports
            let lockFile = Path.Combine(opened.folder.value.value, ".oc", "lock")
            let e2 = outsideExperiment ()
            let window = MainConstructorWindow(ctx, opened)
            let other = Window()
            try
                window.Show()
                Dispatcher.UIThread.RunJobs()
                // Showing activates the window: its first check finds nothing, because opening the kit recorded its stamps.
                Assert.True((controlById window ConstructorUiId.kitBar).IsEffectivelyVisible, "the kit bar must show above the workbench")
                Assert.Equal<Control option>(None, tryControlById window ConstructorUiId.kitChangesText)
                // Another writer (step 126) adds `E2` while the window is open.
                let writer = KitProxy.create opened.folder
                writer.writeExperiment e2 |> SceneTestFixture.kitSuccess
                other.Show()
                other.Activate()
                Dispatcher.UIThread.RunJobs()
                window.Activate()
                Dispatcher.UIThread.RunJobs()
                Assert.Contains("E2.experiment-json", textOf window ConstructorUiId.kitChangesText)
                pressBox window ConstructorUiId.kitChangesReloadButton
                Assert.Equal<Control option>(None, tryControlById window ConstructorUiId.kitChangesText)
                pressRibbonTab window BayNames.experiments
                let row = controlById window (ConstructorUiId.experimentEditButton e2.id.uiKey)
                Assert.True(row.IsEffectivelyVisible, "the reloaded kit's E2 must be a row of the collection")
            finally
                other.Close()
                window.Close()
                Dispatcher.UIThread.RunJobs()
            Assert.False(File.Exists(lockFile), "closing the constructor window must release its kit"))

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``Close kit closes the window and releases the kit`` () : unit =
        HeadlessSession.run (fun () ->
            let ctx = TestAppScope.freshAppContext owner
            let opened = TestAppScope.openedKitIn owner ctx.catalogueImports
            let lockFile = Path.Combine(opened.folder.value.value, ".oc", "lock")
            let window = MainConstructorWindow(ctx, opened)
            let closings = ResizeArray<unit> ()
            window.Closed.Add(fun _ -> closings.Add ())
            try
                window.Show()
                Dispatcher.UIThread.RunJobs()
                Assert.True(File.Exists(lockFile), "the open kit must hold its lock")
                pressBox window ConstructorUiId.kitCloseButton
                Assert.Equal(1, closings.Count)
                Assert.False(window.IsVisible, "Close kit must close the window")
                Assert.False(File.Exists(lockFile), "closing the constructor window must release its kit")
            finally
                if window.IsVisible then
                    window.Close()
                    Dispatcher.UIThread.RunJobs())
