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
open OpticalConstructor.Domain.Experiments
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.KitArchive
open OpticalConstructor.Domain.KitFolders
open OpticalConstructor.Domain.KitLibrary
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Storage.KitContentFiles
open OpticalConstructor.Storage.KitFolderStore
open OpticalConstructor.Tests
open OpticalConstructor.Ui
open OpticalConstructor.Ui.KitArchiveJob
open OpticalConstructor.Ui.KitOpening
open OpticalConstructor.Ui.TableAndElementRotationView
open OpticalConstructor.Domain.Setups
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

    /// The dark line's setup, holding no slot (spec 0050 S8.2).
    let private darkSetup : Setup =
        {
            name = SetupName "Dark"
            revision = SetupRevision.first
            slots = []
            history = []
        }

    /// The experiment `E1` the kit holds before a window opens it.
    let private storedExperiment : Experiments.Experiment =
        {
            id = Experiments.ExperimentId 1
            name = (Experiments.ExperimentId 1).defaultName
            setup = darkSetup.reference
            scan = Experiments.ExperimentScan.darkLine Experiments.CaptureTransmitted
            role = Experiments.ScienceRun
            sessionOpt = None
            notes = ExperimentNotes.none
            dataOpt = None
        }

    /// The controls that send the kit's write messages: Add, `E1`'s Remove and Attach data file, Create setup and Save
    /// setup.
    let private writeControlIds : ConstructorUiId list =
        [
            ConstructorUiId.experimentAddButton
            ConstructorUiId.experimentRemoveButton storedExperiment.id.uiKey
            ConstructorUiId.attachDataFile storedExperiment.id
            ConstructorUiId.createSetupButton
            ConstructorUiId.saveSetupButton
        ]

    /// The setup the write-control windows draw, recorded while the kit is read-write, so Save setup is withheld for the
    /// kit's access alone (spec 0050 S8.2).
    let private storedBench : SetupName = SetupName "Stored bench"

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
    let private pressAtCentre (window : Window) (pressed : Control) (name : string) : unit =
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
            KitWindowShell.inert with
                explorer = ExplorerProxy.createInMemory shown.Add
                requestClose = fun () -> closes.Add ()
        }

    /// A send's handle that records each cancel it is asked for (spec 0050 S6).
    let private recordingHandle (cancels : ResizeArray<unit>) : KitArchiveJobHandle =
        {
            cancelArchiveJob = fun () -> cancels.Add ()
        }

    /// `filesDone` of `filesTotal` files zipped, as a send's progress (spec 0050 S6).
    let private zippedFiles (filesDone : int) (filesTotal : int) : JobRunner.JobProgress =
        let progress : KitArchiveProgress =
            {
                filesDone = KitArchiveFileCount filesDone
                filesTotal = KitArchiveFileCount filesTotal
            }
        progress.jobProgress (JobRunner.JobElapsed System.TimeSpan.Zero)

    /// The running send's progress row: its bar, its count and its Cancel.
    let private sendProgressIds : ConstructorUiId list =
        [ ConstructorUiId.sendKitProgressBar; ConstructorUiId.sendKitProgressText; ConstructorUiId.sendKitCancelButton ]

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

    /// Mounts `mainView` over the inverse model loaded from the kit opened with `access`, `storedBench` drawn, with a
    /// draft ready to commit and the Experiments bay selected, and checks every write control against `expected` (spec
    /// 0050 S5.3, S8.2, S8.4).
    let private assertWriteControls (folder : KitFolder) (ctx : AppContext) (access : KitAccess) (expected : ControlAvailability) : unit =
        let opened = openedAt folder access ctx
        try
            let loaded = kitWindowModel ctx opened InverseConstructor KitWindowShell.inert
            let active = loaded |> updateTableAndElementRotation (ActivateSetup storedBench)
            Assert.Equal<SetupName option>(Some storedBench, active.activeSetupOpt |> Option.map (fun setup -> setup.name))
            let sample = active.elements |> List.find (fun element -> element.placement.catalogueKind = Placement.Sample)
            let model =
                active
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
                // Spec 0050 S8.4: the slot bar's Record experiment is present only while the kit takes writes.
                match expected, tryControlById window ConstructorUiId.recordExperimentButton with
                | ControlOffered, Some _
                | ControlWithheld, None -> ()
                | ControlOffered, None -> Assert.Fail "Record experiment must be on the slot bar of a read-write kit"
                | ControlWithheld, Some _ -> Assert.Fail "Record experiment must not be on the slot bar of a read-only kit"
            finally
                window.Close()
                Dispatcher.UIThread.RunJobs()
            // Spec 0050 S8.3: the kit bar's Save template is present only while the kit takes writes.
            withKitWindow model (fun kitWindow ->
                match expected, tryControlById kitWindow ConstructorUiId.saveNamingTemplateButton with
                | ControlOffered, Some _
                | ControlWithheld, None -> ()
                | ControlOffered, None -> Assert.Fail "Save template must be in the kit bar of a read-write kit"
                | ControlWithheld, Some _ -> Assert.Fail "Save template must not be in the kit bar of a read-only kit")
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
            try
                created.library.kit.proxy.writeExperiment storedExperiment |> SceneTestFixture.kitSuccess
                let recorded =
                    kitWindowModel ctx created InverseConstructor KitWindowShell.inert
                    |> updateTableAndElementRotation (SetSetupName storedBench.caption)
                    |> updateTableAndElementRotation CreateSetup
                Assert.Equal<SetupName list>([ storedBench ], recorded.kitSetups |> List.map (fun setup -> setup.name))
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
    let ``the Send kit UiIds are the stable intent-named ids`` () : unit =
        Assert.Equal("SendKitButton", ConstructorUiId.sendKitButton.value)
        Assert.Equal("SendKitProgressBar", ConstructorUiId.sendKitProgressBar.value)
        Assert.Equal("SendKitProgressText", ConstructorUiId.sendKitProgressText.value)
        Assert.Equal("SendKitCancelButton", ConstructorUiId.sendKitCancelButton.value)

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
    let ``the kit bar shows the kit's naming template with Save template beside it`` () : unit =
        Assert.Equal("NamingTemplateField", ConstructorUiId.namingTemplateField.value)
        Assert.Equal("SaveNamingTemplateButton", ConstructorUiId.saveNamingTemplateButton.value)
        HeadlessSession.run (fun () ->
            let ctx = TestAppScope.freshAppContext owner
            let opened = TestAppScope.openedKitIn owner ctx.catalogueImports
            try
                let model = kitWindowModel ctx opened ForwardConstructor KitWindowShell.inert
                withKitWindow model (fun window ->
                    let field = Assert.IsType<TextBox>(controlById window ConstructorUiId.namingTemplateField)
                    Assert.Equal(opened.library.kit.kitFile.namingTemplate.value, field.Text)
                    Assert.Contains<Visual>(controlById window ConstructorUiId.kitBar, field.GetVisualAncestors())
                    let save = Assert.IsType<Border>(controlById window ConstructorUiId.saveNamingTemplateButton)
                    Assert.Equal("Save template", boxLabelOf window ConstructorUiId.saveNamingTemplateButton)
                    let windowX (control : Control) (x : float) : float =
                        match control.TranslatePoint(Point(x, 0.0), window) |> Option.ofNullable with
                        | Some point -> point.X
                        | None -> failwith "the control must have a position in its window"
                    let fieldRight = windowX field field.Bounds.Width
                    let saveLeft = windowX save 0.0
                    Assert.True(saveLeft > fieldRight, $"Save template ({saveLeft}) must start right of the field's end ({fieldRight})")
                    // Spec 0050 S8.3: the field and the button send the messages the model facts send.
                    field.Text <- "{obj1}"
                    Dispatcher.UIThread.RunJobs()
                    pressBox window ConstructorUiId.saveNamingTemplateButton
                    Assert.Equal("Saved the naming template.", textOf window ConstructorUiId.kitStatus)
                    let stored = (KitProxy.create opened.folder).readKitFile () |> SceneTestFixture.kitSuccess
                    Assert.Equal<NamingTemplate>(NamingTemplate "{obj1}", stored.namingTemplate))
            finally opened.library.kit.close () |> SceneTestFixture.kitSuccess)

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
                        storedExperiment with
                            id = Experiments.ExperimentId 2
                            name = (Experiments.ExperimentId 2).defaultName
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
    let ``a running send shows its progress bar, its count and Cancel, and hides Send kit`` () : unit =
        HeadlessSession.run (fun () ->
            let ctx = TestAppScope.freshAppContext owner
            let opened = TestAppScope.openedKitIn owner ctx.catalogueImports
            let cancels = ResizeArray<unit> ()
            try
                let idle = kitWindowModel ctx opened ForwardConstructor KitWindowShell.inert
                let running =
                    {
                        idle with kitSend = (idle.kitSend.started (recordingHandle cancels)).progressed (zippedFiles 2 5)
                    }
                withKitWindow running (fun window ->
                    Assert.StartsWith("2 / 5", textOf window ConstructorUiId.sendKitProgressText)
                    let bar = Assert.IsType<ProgressBar>(controlById window ConstructorUiId.sendKitProgressBar)
                    Assert.Equal(0.4, bar.Value, 12)
                    Assert.False(bar.IsIndeterminate, "a send that told its files must not show an indeterminate bar")
                    Assert.Equal("Cancel", boxLabelOf window ConstructorUiId.sendKitCancelButton)
                    Assert.Equal<Control option>(None, tryControlById window ConstructorUiId.sendKitButton))
                withKitWindow idle (fun window ->
                    Assert.Equal("Send kit…", boxLabelOf window ConstructorUiId.sendKitButton)
                    for id in sendProgressIds do
                        Assert.Equal<Control option>(None, tryControlById window id))
                Assert.Empty(cancels)
            finally opened.library.kit.close () |> SceneTestFixture.kitSuccess
            let scratch = initMain () |> SceneTestFixture.kitSuccess
            try
                withKitWindow scratch (fun window ->
                    Assert.Equal<Control option>(None, tryControlById window ConstructorUiId.sendKitButton)
                    Assert.Equal("Close kit", boxLabelOf window ConstructorUiId.kitCloseButton))
            finally scratch.kit.close () |> SceneTestFixture.kitSuccess)

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

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``pressing Send kit asks for a zip named for the kit and starts the send, and Cancel cancels it`` () : unit =
        HeadlessSession.run (fun () ->
            let ctx = TestAppScope.freshAppContext owner
            let opened = TestAppScope.openedKitIn owner ctx.catalogueImports
            let factFolder = (TestAppScope.testLocations owner).exportsFolder.combine (FolderName (System.Guid.NewGuid().ToString("N")))
            let kitZipName = FileName $"{opened.library.kit.kitFile.name.value}.zip"
            let zip = FileName (Path.Combine(factFolder.value, kitZipName.value))
            let suggestions = ResizeArray<FileName> ()
            let jobs = ResizeArray<KitArchiveObserver -> Result<KitArchiveOutcome, KitArchiveError>> ()
            let cancels = ResizeArray<unit> ()
            let picker : KitZipPicker =
                {
                    KitZipPicker.none with
                        pickZipToWrite =
                            fun name ->
                                suggestions.Add name
                                async { return Some zip }
                }
            let deferred : KitArchiveJobHost =
                {
                    startArchiveJob =
                        fun job ->
                            jobs.Add job
                            recordingHandle cancels
                }
            try
                let shell =
                    {
                        KitWindowShell.inert with
                            zipPicker = picker
                            archiveJobs = deferred
                    }
                let model = kitWindowModel ctx opened ForwardConstructor shell
                withKitWindow model (fun window ->
                    for id in sendProgressIds do
                        Assert.Equal<Control option>(None, tryControlById window id)
                    pressBox window ConstructorUiId.sendKitButton
                    Assert.Equal<FileName list>([ kitZipName ], List.ofSeq suggestions)
                    Assert.Single(jobs) |> ignore
                    let progressText = controlById window ConstructorUiId.sendKitProgressText
                    Assert.True(progressText.IsEffectivelyVisible, "a started send must show its progress")
                    Assert.Equal("Starting…", textOf window ConstructorUiId.sendKitProgressText)
                    Assert.Equal<Control option>(None, tryControlById window ConstructorUiId.sendKitButton)
                    Assert.Empty(cancels)
                    pressBox window ConstructorUiId.sendKitCancelButton
                    Assert.Equal(1, cancels.Count)
                    Assert.Single(jobs) |> ignore)
                Assert.False(File.Exists zip.value, "a kept job writes no zip")
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
                experiment with
                    id = Experiments.ExperimentId 2
                    name = (Experiments.ExperimentId 2).defaultName
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

    /// A kit made in a fresh folder holding the dark setup, recorded first, and `experiments`, then closed, so a fact can
    /// open it again (spec 0050 S6, S8.2).
    let private closedKitHolding (ctx : AppContext) (experiments : Experiments.Experiment list) : KitFolder =
        let folder = TestAppScope.freshKitFolder owner
        let created = openedAt folder ReadWriteKitAccess ctx
        try
            created.library.recordSetup darkSetup |> Result.defaultWith (fun error -> failwith error.describe)
            for experiment in experiments do
                created.library.kit.proxy.writeExperiment experiment |> SceneTestFixture.kitSuccess
        finally created.library.kit.close () |> SceneTestFixture.kitSuccess
        folder

    /// `storedExperiment` as the source's experiment `n`.
    let private sourceExperiment (n : int) : Experiments.Experiment =
        {
            storedExperiment with
                id = Experiments.ExperimentId n
                name = (Experiments.ExperimentId n).defaultName
        }

    /// The copy panel's controls besides the experiments' toggles.
    let private copyPanelIds : ConstructorUiId list =
        [
            ConstructorUiId.kitCopySourceText
            ConstructorUiId.kitCopyExperimentList
            ConstructorUiId.kitCopyChosenButton
            ConstructorUiId.kitCopyCancelButton
        ]

    /// The `CheckBox` carrying the copy toggle of `experimentId`.
    let private copyToggleOf (window : Window) (experimentId : Experiments.ExperimentId) : CheckBox =
        Assert.IsType<CheckBox>(controlById window (ConstructorUiId.kitCopyExperimentToggle experimentId))

    [<Fact>]
    let ``the copy UiIds are the stable intent-named ids`` () : unit =
        Assert.Equal("KitCopyExperimentsButton", ConstructorUiId.kitCopyExperimentsButton.value)
        Assert.Equal("KitCopySourceText", ConstructorUiId.kitCopySourceText.value)
        Assert.Equal("KitCopyExperimentList", ConstructorUiId.kitCopyExperimentList.value)
        Assert.Equal("KitCopyChosenButton", ConstructorUiId.kitCopyChosenButton.value)
        Assert.Equal("KitCopyCancelButton", ConstructorUiId.kitCopyCancelButton.value)
        Assert.Equal("KitCopyExperimentToggle_3", (ConstructorUiId.kitCopyExperimentToggle (Experiments.ExperimentId 3)).value)

    [<Fact>]
    let ``an open source lists its experiments with a toggle each, and Copy and Cancel share a row, Cancel to the right`` () : unit =
        HeadlessSession.run (fun () ->
            let ctx = TestAppScope.freshAppContext owner
            let e1 = sourceExperiment 1
            let e2 = sourceExperiment 2
            let sourceFolder = closedKitHolding ctx [ e1; e2 ]
            let opened = TestAppScope.openedKitIn owner ctx.catalogueImports
            try
                let listed =
                    kitWindowModel ctx opened ForwardConstructor KitWindowShell.inert
                    |> updateTableAndElementRotation (OpenCopySource sourceFolder)
                let sourceName =
                    match listed.kitCopyOpt with
                    | Some copySource -> copySource.source.kitFile.name.value
                    | None -> failwith "the source kit was not opened to copy from"
                try
                    let chosen = listed |> updateTableAndElementRotation (ToggleCopiedExperiment e2.id)
                    withKitWindow chosen (fun window ->
                        Assert.Equal($"Copy from {sourceName}: choose the experiments to copy.", textOf window ConstructorUiId.kitCopySourceText)
                        let list = Assert.IsType<ScrollViewer>(controlById window ConstructorUiId.kitCopyExperimentList)
                        Assert.Equal(160.0, list.MaxHeight)
                        let e1Toggle = copyToggleOf window e1.id
                        let e2Toggle = copyToggleOf window e2.id
                        Assert.Equal($"E1 — {e1.description}", Assert.IsType<TextBlock>(e1Toggle.Content).Text)
                        Assert.Equal(System.Nullable false, e1Toggle.IsChecked)
                        Assert.Equal(System.Nullable true, e2Toggle.IsChecked)
                        Assert.Contains<Visual>(list, e2Toggle.GetVisualAncestors())
                        let copy = Assert.IsType<Border>(controlById window ConstructorUiId.kitCopyChosenButton)
                        let cancel = Assert.IsType<Border>(controlById window ConstructorUiId.kitCopyCancelButton)
                        Assert.Equal("Copy", boxLabelOf window ConstructorUiId.kitCopyChosenButton)
                        Assert.Equal("Cancel", boxLabelOf window ConstructorUiId.kitCopyCancelButton)
                        let row = Assert.IsType<StackPanel>(copy.Parent)
                        Assert.Same(row, cancel.Parent)
                        Assert.Equal(Avalonia.Layout.Orientation.Horizontal, row.Orientation)
                        Assert.Equal(copy.Bounds.Top, cancel.Bounds.Top)
                        Assert.True(cancel.Bounds.Left > copy.Bounds.Right, "Cancel must sit to the right of Copy, past a gap")
                        Assert.Equal(MaterialsWindowView.positiveBackground, Assert.IsType<SolidColorBrush>(copy.Background).Color)
                        Assert.Equal(MaterialsWindowView.negativeBackground, Assert.IsType<SolidColorBrush>(cancel.Background).Color)
                        Assert.Equal<Control option>(None, tryControlById window ConstructorUiId.kitCopyExperimentsButton))
                    withKitWindow listed (fun window ->
                        Assert.Equal(System.Nullable false, (copyToggleOf window e2.id).IsChecked)
                        Assert.Equal<Control option>(None, tryControlById window ConstructorUiId.kitCopyChosenButton)
                        Assert.Equal("Cancel", boxLabelOf window ConstructorUiId.kitCopyCancelButton))
                finally listed.closeCopySource () |> ignore
            finally opened.library.kit.close () |> SceneTestFixture.kitSuccess)

    [<Fact>]
    let ``a kit opened read-only offers no Copy experiments`` () : unit =
        HeadlessSession.run (fun () ->
            let ctx = TestAppScope.freshAppContext owner
            let folder = closedKitHolding ctx []
            let assertOffered (access : KitAccess) (expected : ControlAvailability) : unit =
                let opened = openedAt folder access ctx
                try
                    withKitWindow (kitWindowModel ctx opened ForwardConstructor KitWindowShell.inert) (fun window ->
                        match expected with
                        | ControlOffered ->
                            Assert.Equal("Copy experiments from another kit…", boxLabelOf window ConstructorUiId.kitCopyExperimentsButton)
                        | ControlWithheld ->
                            Assert.Equal<Control option>(None, tryControlById window ConstructorUiId.kitCopyExperimentsButton)
                            Assert.Equal("Show in Explorer", boxLabelOf window ConstructorUiId.kitShowInExplorerButton)
                        for id in copyPanelIds do
                            Assert.Equal<Control option>(None, tryControlById window id))
                finally opened.library.kit.close () |> SceneTestFixture.kitSuccess
            assertOffered ReadOnlyKitAccess ControlWithheld
            assertOffered ReadWriteKitAccess ControlOffered)

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``a constructor window's kit bar offers Send kit, and Copy experiments only to a kit opened read-write`` () : unit =
        HeadlessSession.run (fun () ->
            // Spec 0050 S5.3, S6: neither button is pressed, because a real window's dialogs are the system's.
            let ctx = TestAppScope.freshAppContext owner
            let assertKitBar (opened : OpenedKit) (expected : ControlAvailability) : unit =
                let window = MainConstructorWindow(ctx, opened)
                try
                    window.Show()
                    Dispatcher.UIThread.RunJobs()
                    Assert.Equal("Send kit…", boxLabelOf window ConstructorUiId.sendKitButton)
                    match expected with
                    | ControlOffered ->
                        Assert.Equal("Copy experiments from another kit…", boxLabelOf window ConstructorUiId.kitCopyExperimentsButton)
                    | ControlWithheld ->
                        Assert.Equal<Control option>(None, tryControlById window ConstructorUiId.kitCopyExperimentsButton)
                finally
                    window.Close()
                    Dispatcher.UIThread.RunJobs()
            let readWrite = TestAppScope.openedKitIn owner ctx.catalogueImports
            assertKitBar readWrite ControlOffered
            assertKitBar (openedAt readWrite.folder ReadOnlyKitAccess ctx) ControlWithheld)

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``pressing Copy experiments, a toggle and Copy brings the experiment into the kit`` () : unit =
        HeadlessSession.run (fun () ->
            let ctx = TestAppScope.freshAppContext owner
            let e2 = sourceExperiment 2
            let sourceFolder = closedKitHolding ctx [ e2 ]
            let opened = TestAppScope.openedKitIn owner ctx.catalogueImports
            let copiedFile = Path.Combine(opened.folder.value.value, "E2.experiment-json")
            try
                let shell =
                    {
                        KitWindowShell.inert with kitPicker = TestAppScope.kitPickerOf sourceFolder
                    }
                withKitWindow (kitWindowModel ctx opened ForwardConstructor shell) (fun window ->
                    for id in copyPanelIds do
                        Assert.Equal<Control option>(None, tryControlById window id)
                    pressBox window ConstructorUiId.kitCopyExperimentsButton
                    Assert.True((controlById window ConstructorUiId.kitCopySourceText).IsEffectivelyVisible, "the chosen kit must open the copy panel")
                    Assert.Equal<Control option>(None, tryControlById window ConstructorUiId.kitCopyExperimentsButton)
                    Assert.Equal<Control option>(None, tryControlById window ConstructorUiId.kitCopyChosenButton)
                    let toggleId = ConstructorUiId.kitCopyExperimentToggle e2.id
                    pressAtCentre window (copyToggleOf window e2.id) toggleId.value
                    Assert.Equal(System.Nullable true, (copyToggleOf window e2.id).IsChecked)
                    Assert.False(File.Exists copiedFile, "choosing an experiment must copy nothing yet")
                    pressBox window ConstructorUiId.kitCopyChosenButton
                    for id in copyPanelIds do
                        Assert.Equal<Control option>(None, tryControlById window id)
                    Assert.StartsWith("Copied 1 experiment(s)", textOf window ConstructorUiId.kitStatus)
                    Assert.Equal("Copy experiments from another kit…", boxLabelOf window ConstructorUiId.kitCopyExperimentsButton))
                Assert.True(File.Exists copiedFile, "Copy must leave E2.experiment-json in the window's kit folder")
            finally opened.library.kit.close () |> SceneTestFixture.kitSuccess)

    let private libraryValue (result : Result<'Value, KitLibrary.KitLibraryError>) : 'Value =
        result |> Result.defaultWith (fun error -> failwith error.describe)

    /// The 600 nm source preset over the shipped source-preset item model (spec 0050 S7.1, step 210).
    let private laser600Choice : ItemModelChoice =
        SourcePresetChoice (BuiltinCatalogue.itemModel ItemModels.ItemModelIds.sourcePresetItemModelId, BuiltinCatalogue.source Library.PresetIds.source600)

    /// The 1 mm glass plate's first version over the shipped stack-sample item model (spec 0050 S7.1, step 210).
    let private plateChoice : ItemModelChoice =
        StackSampleChoice (
            BuiltinCatalogue.itemModel ItemModels.ItemModelIds.stackSampleItemModelId,
            Library.SampleVersionId.firstOf Library.SampleIds.glassPlate1mmId,
            BuiltinCatalogue.sample Library.SampleIds.glassPlate1mmId
        )

    /// A fixed-mounted piece of the model at revision 1, its parameters at their defaults (spec 0050 S7.1).
    let private pieceOf (guid : string) (label : string) (model : ItemModelChoice) : Item =
        let draft : ItemDraft =
            {
                id = ItemId (System.Guid.Parse guid)
                label = ItemLabel label
                model = model
                dial = FixedMounting
            }
        draft.toItem

    /// A Laser600 piece with its `intensityScale` known at `value`, so same-model pieces differ in their parameters.
    let private scaledLaser600 (guid : string) (label : string) (value : float) : Item =
        let item = pieceOf guid label laser600Choice
        let name = ItemModels.ParameterName "intensityScale"
        match item.tryFindParameter name with
        | Some _ -> ()
        | None -> Assert.Fail "the source-preset item model must publish 'intensityScale'"
        {
            item with
                parameters =
                    item.parameters
                    |> List.map (fun parameter ->
                        if parameter.name = name then
                            {
                                parameter with state = KnownParameter (ItemModels.ParameterValue value)
                            }
                        else
                            parameter)
        }

    /// The source's Laser600, which E1 assigns at its light source (spec 0050 S7.2).
    let private copiedLaser600 : Item = scaledLaser600 "22900000-0000-4000-8000-000000000001" "Laser600" 0.75

    /// The source's second piece of the same preset, which no experiment assigns.
    let private spareLaser600 : Item = scaledLaser600 "22900000-0000-4000-8000-000000000002" "Laser600 spare" 0.5

    /// The source's QZ, which E2 assigns at its plate (spec 0050 S7.2).
    let private sourceQz : Item = pieceOf "22900000-0000-4000-8000-000000000003" "QZ" plateChoice

    /// The window kit's own Laser600: the same preset and label under another id, onto which the copied Laser600 maps
    /// by default.
    let private ownLaser600 : Item = pieceOf "22900000-0000-4000-8000-000000000004" "Laser600" laser600Choice

    /// A bench piece of the same preset under a third id, its parameters unlike either source piece's.
    let private benchLaser600 : Item = scaledLaser600 "22900000-0000-4000-8000-000000000005" "Laser600 bench" 0.25

    /// Setup `name` of one rest-pose slot of that kind, bound to the entry and holding the item (spec 0050 S8.2).
    let private setupHolding
        (name : string)
        (element : string)
        (kind : Placement.CatalogueKind)
        (binding : Placement.ElementValueId)
        (item : Item)
        : Setup =
        let slot : SetupSlot =
            {
                id = Library.ElementId element
                placement =
                    {
                        Placement.ElementPlacement.create kind Placement.TablePoint.origin with valueId = Some binding
                    }
                zoom = BeamLayout.ElementZoom 1.0
                beam = BeamLayout.OnMainBeam
                content = ItemInSlot (item.id, PlacementTurn)
            }
        {
            darkSetup with
                name = SetupName name
                slots = [ slot ]
        }

    /// E1's `Laser bench` binds the 600 nm source preset at its light source, which holds the copied Laser600.
    let private laserBench : Setup =
        setupHolding "Laser bench" "copied-source" Placement.CatalogueKind.LightSource Library.PresetIds.source600 copiedLaser600

    /// E2's `Plate bench` binds the 1 mm glass plate at its sample, which holds QZ.
    let private plateBench : Setup =
        setupHolding "Plate bench" "copied-plate" Placement.CatalogueKind.Sample Library.SampleIds.glassPlate1mmId.toElementValueId sourceQz

    let private itemE1 : Experiments.Experiment =
        {
            storedExperiment with setup = laserBench.reference
        }

    let private itemE2 : Experiments.Experiment =
        {
            storedExperiment with
                id = Experiments.ExperimentId 2
                name = (Experiments.ExperimentId 2).defaultName
                setup = plateBench.reference
        }

    /// The kit to copy from, made as step 228 makes it: the copied Laser600, its spare and QZ through `recordItem`, their
    /// setups through `recordSetup`, then E1 and E2 through `recordExperiment`; closed, so the window opens it (spec 0050
    /// S7.2, S8.2).
    let private itemSourceKit (ctx : AppContext) : KitFolder =
        let folder = TestAppScope.freshKitFolder owner
        let created = openedAt folder ReadWriteKitAccess ctx
        try
            for item in [ copiedLaser600; spareLaser600; sourceQz ] do
                created.library.recordItem item |> libraryValue
            for setup in [ laserBench; plateBench ] do
                created.library.recordSetup setup |> libraryValue
            for experiment in [ itemE1; itemE2 ] do
                created.library.recordExperiment experiment |> libraryValue
        finally created.library.kit.close () |> SceneTestFixture.kitSuccess
        folder

    /// A kit made in a fresh folder holding the items, each recorded with its model snapshots, then opened read-write
    /// as a window opens it (spec 0050 S7.2).
    let private openedHolding (ctx : AppContext) (items : Item list) : OpenedKit =
        let folder = TestAppScope.freshKitFolder owner
        let created = openedAt folder ReadWriteKitAccess ctx
        try
            for item in items do
                created.library.recordItem item |> libraryValue
        finally created.library.kit.close () |> SceneTestFixture.kitSuccess
        openedAt folder ReadWriteKitAccess ctx

    /// The files at the kit folder's root that `pattern` matches, in ordinal order.
    let private kitFilesIn (folder : KitFolder) (pattern : string) : string list =
        Directory.GetFiles(folder.value.value, pattern) |> Array.map Path.GetFileName |> Array.sort |> List.ofArray

    /// The item files at the kit folder's root, in ordinal order.
    let private itemFilesIn (folder : KitFolder) : string list = kitFilesIn folder "*.item-json"

    /// What a kit holds once closed and reopened read-only: its experiments, the items their setups' slots resolve to
    /// through `Experiment.itemsUsedBy`, all its items and its setups (spec 0050 S7.2, S8.2).
    type private ReopenedKitCopy =
        {
            experiments : Experiments.Experiment list
            resolved : Result<Item list, SetupError>
            items : Item list
            setups : Setup list
        }

    let private reopenedCopy (folder : KitFolder) : ReopenedKitCopy =
        let later = KitContext.openKit (KitProxy.create folder) ReadOnlyKitAccess ClockProvider.system |> SceneTestFixture.kitSuccess
        try
            let experiments = later.proxy.readExperiments () |> SceneTestFixture.kitSuccess
            let items = later.proxy.readItems () |> SceneTestFixture.kitSuccess
            let setups = later.proxy.readSetups () |> SceneTestFixture.kitSuccess
            let reopened : ReopenedKitCopy =
                {
                    experiments = experiments
                    resolved = Experiments.Experiment.itemsUsedBy experiments setups items
                    items = items
                    setups = setups
                }
            reopened
        finally later.close () |> SceneTestFixture.kitSuccess

    /// The fill of the choice box carrying `id`: the chosen one is on `positiveBackground`.
    let private fillOf (window : Window) (id : ConstructorUiId) : Color =
        Assert.IsType<SolidColorBrush>(Assert.IsType<Border>(controlById window id).Background).Color

    /// Every control carrying a mapping choice's id.
    let private choiceBoxesIn (window : Window) : Control list =
        window.GetVisualDescendants()
        |> Seq.choose (function :? Control as control -> Some control | _ -> None)
        |> Seq.filter (fun control ->
            let automationId = AutomationProperties.GetAutomationId control
            not (isNull automationId) && automationId.StartsWith "KitCopyItemChoice_")
        |> List.ofSeq

    /// Every `TextBlock` in the window showing the mapping table's intro line.
    let private mappingIntrosIn (window : Window) : TextBlock list =
        let intro = "Items the chosen experiments use: add each to this kit, or use an item it already has."
        window.GetVisualDescendants()
        |> Seq.choose (function :? TextBlock as text when text.Text = intro -> Some text | _ -> None)
        |> List.ofSeq

    [<Fact>]
    let ``the copy-mapping UiIds are the stable intent-named ids`` () : unit =
        let sourceItem = ItemId (System.Guid.Parse "22900000-0000-4000-8000-0000000000a1")
        let kitItem = ItemId (System.Guid.Parse "22900000-0000-4000-8000-0000000000b2")
        Assert.Equal("KitCopyItemList", ConstructorUiId.kitCopyItemList.value)
        Assert.Equal("KitCopyItemChoice_22900000-0000-4000-8000-0000000000a1_Add", (ConstructorUiId.kitCopyItemChoice sourceItem AddCopiedItem).value)
        Assert.Equal(
            "KitCopyItemChoice_22900000-0000-4000-8000-0000000000a1_22900000-0000-4000-8000-0000000000b2",
            (ConstructorUiId.kitCopyItemChoice sourceItem (MapOntoKitItem kitItem)).value)

    [<Fact>]
    let ``an open source lists the items its chosen experiments use, each with Add and the kit's items, the choice marked`` () : unit =
        HeadlessSession.run (fun () ->
            let ctx = TestAppScope.freshAppContext owner
            let sourceFolder = itemSourceKit ctx
            let opened = openedHolding ctx [ ownLaser600 ]
            try
                let listed =
                    kitWindowModel ctx opened ForwardConstructor KitWindowShell.inert
                    |> updateTableAndElementRotation (OpenCopySource sourceFolder)
                try
                    let chosen =
                        listed
                        |> updateTableAndElementRotation (ToggleCopiedExperiment itemE1.id)
                        |> updateTableAndElementRotation (ToggleCopiedExperiment itemE2.id)
                    let expectedRows : ItemCopyRow list =
                        [
                            {
                                sourceItem = copiedLaser600
                                choice = MapOntoKitItem ownLaser600.id
                            }
                            {
                                sourceItem = sourceQz
                                choice = AddCopiedItem
                            }
                        ]
                    match chosen.kitCopyOpt with
                    | Some copySource -> Assert.Equal<ItemCopyRow list>(expectedRows, copySource.itemRows)
                    | None -> Assert.Fail "the copy source must stay open while its experiments are chosen"
                    let laserAdd = ConstructorUiId.kitCopyItemChoice copiedLaser600.id AddCopiedItem
                    let laserUse = ConstructorUiId.kitCopyItemChoice copiedLaser600.id (MapOntoKitItem ownLaser600.id)
                    let qzAdd = ConstructorUiId.kitCopyItemChoice sourceQz.id AddCopiedItem
                    let qzUse = ConstructorUiId.kitCopyItemChoice sourceQz.id (MapOntoKitItem ownLaser600.id)
                    withKitWindow chosen (fun window ->
                        let list = Assert.IsType<ScrollViewer>(controlById window ConstructorUiId.kitCopyExperimentList)
                        let intro = Assert.Single(mappingIntrosIn window)
                        Assert.Contains<Visual>(list, intro.GetVisualAncestors())
                        let table = Assert.IsType<StackPanel>(controlById window ConstructorUiId.kitCopyItemList)
                        Assert.Contains<Visual>(list, table.GetVisualAncestors())
                        Assert.Equal(2, table.Children.Count)
                        let lineOf (index : int) : string =
                            Assert.IsType<TextBlock>(Assert.IsType<StackPanel>(table.Children[index]).Children[0]).Text
                        let laserModel = (BuiltinCatalogue.itemModel ItemModels.ItemModelIds.sourcePresetItemModelId).versionId.entryReference
                        let plateModel = (BuiltinCatalogue.itemModel ItemModels.ItemModelIds.stackSampleItemModelId).versionId.entryReference
                        Assert.Equal($"Laser600 — {laserModel.describe}", lineOf 0)
                        Assert.Equal($"QZ — {plateModel.describe}", lineOf 1)
                        for id, label, block in [ laserAdd, "Add", 0; laserUse, "Use Laser600", 0; qzAdd, "Add", 1; qzUse, "Use Laser600", 1 ] do
                            Assert.Equal(label, boxLabelOf window id)
                            Assert.Contains<Visual>(table.Children[block], (controlById window id).GetVisualAncestors())
                        Assert.Equal(MaterialsWindowView.positiveBackground, fillOf window laserUse)
                        Assert.NotEqual(MaterialsWindowView.positiveBackground, fillOf window laserAdd)
                        Assert.Equal(MaterialsWindowView.positiveBackground, fillOf window qzAdd)
                        Assert.NotEqual(MaterialsWindowView.positiveBackground, fillOf window qzUse)
                        let boxes = choiceBoxesIn window
                        Assert.Equal(4, List.length boxes)
                        for box in boxes do
                            Assert.Contains<Visual>(list, box.GetVisualAncestors())
                        Assert.Equal<Control option>(None, tryControlById window (ConstructorUiId.kitCopyItemChoice spareLaser600.id AddCopiedItem)))
                    withKitWindow listed (fun window ->
                        Assert.Equal<Control option>(None, tryControlById window ConstructorUiId.kitCopyItemList)
                        Assert.Empty(choiceBoxesIn window)
                        Assert.Empty(mappingIntrosIn window))
                finally listed.closeCopySource () |> ignore
            finally opened.library.kit.close () |> SceneTestFixture.kitSuccess)

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``pressing a mapping choice and Copy adds or maps the item as chosen`` () : unit =
        HeadlessSession.run (fun () ->
            let ctx = TestAppScope.freshAppContext owner
            let sourceFolder = itemSourceKit ctx
            let shell =
                {
                    KitWindowShell.inert with kitPicker = TestAppScope.kitPickerOf sourceFolder
                }
            // Presses Copy experiments, E1's toggle, the choice and Copy in a window over the kit, then closes the kit.
            let copyE1Choosing (opened : OpenedKit) (choice : ItemCopyChoice) (told : string) : unit =
                let byDefault = ConstructorUiId.kitCopyItemChoice copiedLaser600.id (MapOntoKitItem ownLaser600.id)
                let pressed = ConstructorUiId.kitCopyItemChoice copiedLaser600.id choice
                let filesBefore = itemFilesIn opened.folder
                try
                    withKitWindow (kitWindowModel ctx opened ForwardConstructor shell) (fun window ->
                        Assert.Equal<Control option>(None, tryControlById window ConstructorUiId.kitCopyItemList)
                        pressBox window ConstructorUiId.kitCopyExperimentsButton
                        let toggleId = ConstructorUiId.kitCopyExperimentToggle itemE1.id
                        pressAtCentre window (copyToggleOf window itemE1.id) toggleId.value
                        Assert.Equal(MaterialsWindowView.positiveBackground, fillOf window byDefault)
                        Assert.NotEqual(MaterialsWindowView.positiveBackground, fillOf window pressed)
                        pressBox window pressed
                        Assert.Equal(MaterialsWindowView.positiveBackground, fillOf window pressed)
                        Assert.NotEqual(MaterialsWindowView.positiveBackground, fillOf window byDefault)
                        Assert.Equal<string list>(filesBefore, itemFilesIn opened.folder)
                        pressBox window ConstructorUiId.kitCopyChosenButton
                        Assert.Equal<Control option>(None, tryControlById window ConstructorUiId.kitCopyItemList)
                        Assert.StartsWith(told, textOf window ConstructorUiId.kitStatus))
                finally opened.library.kit.close () |> SceneTestFixture.kitSuccess

            // Add: the copied Laser600 comes in under its own id beside the kit's, relabelled.
            let target = openedHolding ctx [ ownLaser600 ]
            copyE1Choosing target AddCopiedItem "Copied 1 experiment(s) and added 1 item(s)"
            Assert.Equal<string list>([ "Laser600 (2).item-json"; "Laser600.item-json" ], itemFilesIn target.folder)
            let added = reopenedCopy target.folder
            match added.experiments, added.resolved with
            | [ copiedE1 ], Ok [ piece ] ->
                Assert.Equal<Result<ItemId option list, SetupError>>(
                    Ok [ Some copiedLaser600.id ], copiedE1.slotsIn added.setups |> Result.map (List.map (fun slot -> slot.content.itemOpt)))
                Assert.Equal<Item>({ copiedLaser600 with label = ItemLabel "Laser600 (2)" }, piece)
            | experiments, _ -> Assert.Fail $"E1 must be copied alone and resolve the added piece, not {List.length experiments} experiment(s)"
            Assert.Equal<Set<ItemId>>(set [ ownLaser600.id; copiedLaser600.id ], added.items |> List.map (fun item -> item.id) |> set)

            // Use: the copied E1 is bound to the bench's own third piece, and neither source piece comes in.
            let bench = openedHolding ctx [ ownLaser600; benchLaser600 ]
            let benchFile = Path.Combine(bench.folder.value.value, "Laser600 bench.item-json")
            let benchBytes = File.ReadAllBytes benchFile
            copyE1Choosing bench (MapOntoKitItem benchLaser600.id) "Copied 1 experiment(s) from"
            Assert.Equal<string list>([ "Laser600 bench.item-json"; "Laser600.item-json" ], itemFilesIn bench.folder)
            Assert.Equal<byte[]>(benchBytes, File.ReadAllBytes benchFile)
            let mapped = reopenedCopy bench.folder
            match mapped.experiments, mapped.resolved with
            | [ copiedE1 ], Ok [ piece ] ->
                Assert.Equal<Result<ItemId option list, SetupError>>(
                    Ok [ Some benchLaser600.id ], copiedE1.slotsIn mapped.setups |> Result.map (List.map (fun slot -> slot.content.itemOpt)))
                Assert.Equal<ItemId>(benchLaser600.id, piece.id)
                Assert.Equal<Parameter list>(benchLaser600.parameters, piece.parameters)
                for sourcePiece in [ copiedLaser600; spareLaser600 ] do
                    Assert.NotEqual<Parameter list>(sourcePiece.parameters, piece.parameters)
            | experiments, _ -> Assert.Fail $"E1 must be copied alone and resolve exactly the chosen piece, not {List.length experiments} experiment(s)"
            Assert.Equal<Set<ItemId>>(set [ ownLaser600.id; benchLaser600.id ], mapped.items |> List.map (fun item -> item.id) |> set))

    /// The flow strip's step boxes, top to bottom (spec 0050 S7.2).
    let private flowStepIds : ConstructorUiId list =
        [ ConstructorUiId.flowItemsStep; ConstructorUiId.flowSetupsStep; ConstructorUiId.flowExperimentsStep ]

    /// The Experiments bay's pane, which carries its id as its `Name` (`Ribbon.ribbonView`).
    let private experimentsPane : ConstructorUiId = ConstructorUiId.ribbonPane BayNames.experiments.uiKey

    /// The control carrying `id` as its `Name`, as the table's canvas and a ribbon pane do.
    let private tryNamedControl (window : Window) (id : ConstructorUiId) : Control option =
        window.GetVisualDescendants()
        |> Seq.tryPick (function :? Control as control when control.Name = id.value -> Some control | _ -> None)

    let private namedControl (window : Window) (id : ConstructorUiId) : Control =
        tryNamedControl window id
        |> Option.defaultWith (fun () -> failwith $"{id.value} must be in the window")

    /// The control's rectangle in window coordinates.
    let private windowRectOf (window : Window) (control : Control) : Rect =
        match control.TranslatePoint(Point(0.0, 0.0), window) |> Option.ofNullable with
        | Some topLeft -> Rect(topLeft, control.Bounds.Size)
        | None -> failwith $"{control.GetType().Name} has no position in its window"

    [<Fact>]
    let ``the flow strip UiIds are the stable intent-named ids`` () : unit =
        Assert.Equal("FlowStrip", ConstructorUiId.flowStrip.value)
        Assert.Equal("FlowItemsStep", ConstructorUiId.flowItemsStep.value)
        Assert.Equal("FlowSetupsStep", ConstructorUiId.flowSetupsStep.value)
        Assert.Equal("FlowExperimentsStep", ConstructorUiId.flowExperimentsStep.value)
        Assert.Equal<ConstructorUiId list>(flowStepIds, ConstructorFlowStep.all |> List.map _.flowUiId)
        Assert.Equal<ConstructorUiId>(ConstructorUiId.flowItemsStep, ItemsFlowStep.flowUiId)
        Assert.Equal<ConstructorUiId>(ConstructorUiId.flowSetupsStep, SetupsFlowStep.flowUiId)
        Assert.Equal<ConstructorUiId>(ConstructorUiId.flowExperimentsStep, ExperimentsFlowStep.flowUiId)

    [<Fact>]
    let ``the flow strip shows Items, Setups and Experiments left of the surface its step shows`` () : unit =
        HeadlessSession.run (fun () ->
            let ctx = TestAppScope.freshAppContext owner
            let opened = TestAppScope.openedKitIn owner ctx.catalogueImports
            try
                let loaded = kitWindowModel ctx opened ForwardConstructor KitWindowShell.inert
                Assert.Equal<ConstructorFlowStep>(SetupsFlowStep, loaded.flowStep)
                // Shows the window on `step`, checks the strip against the surface `surfaceOf` finds, then runs `check`.
                let onStep (step : ConstructorFlowStep) (surfaceOf : Window -> Control) (check : Window -> unit) : unit =
                    withKitWindow (loaded |> updateTableAndElementRotation (ShowFlowStep step)) (fun window ->
                        let strip = Assert.IsType<Border>(Assert.Single(controlsById window ConstructorUiId.flowStrip))
                        let boxes = flowStepIds |> List.map (controlById window)
                        for box in boxes do
                            Assert.Contains<Visual>(strip, box.GetVisualAncestors())
                        Assert.Equal<string list>([ "Items"; "Setups"; "Experiments" ], flowStepIds |> List.map (boxLabelOf window))
                        let rects = boxes |> List.map (windowRectOf window)
                        for upper, lower in List.pairwise rects do
                            Assert.True(upper.Bottom <= lower.Top, $"the strip's boxes must stand top to bottom: {upper} above {lower}")
                        let surface = surfaceOf window
                        Assert.True(surface.IsEffectivelyVisible, "the step's surface must be visible")
                        let surfaceRect = windowRectOf window surface
                        for rect in rects do
                            Assert.True(rect.Right <= surfaceRect.Left, $"the strip's box {rect} must sit left of the surface {surfaceRect}")
                        for shown in ConstructorFlowStep.all do
                            if shown = step then
                                Assert.Equal(MaterialsWindowView.positiveBackground, fillOf window shown.flowUiId)
                            else
                                Assert.NotEqual(MaterialsWindowView.positiveBackground, fillOf window shown.flowUiId)
                        check window)
                onStep SetupsFlowStep (fun window -> namedControl window ConstructorUiId.tableElementCanvas) (fun window ->
                    Assert.Equal<Control option>(None, tryControlById window ConstructorUiId.itemsPanel)
                    Assert.Equal<Control option>(None, tryNamedControl window experimentsPane))
                onStep ItemsFlowStep (fun window -> controlById window ConstructorUiId.itemsPanel) (fun window ->
                    Assert.Equal<Control option>(None, tryNamedControl window ConstructorUiId.tableElementCanvas)
                    Assert.Equal<Control option>(None, tryNamedControl window experimentsPane))
                onStep ExperimentsFlowStep (fun window -> namedControl window experimentsPane) (fun window ->
                    Assert.Equal<Control option>(None, tryControlById window ConstructorUiId.itemsPanel))
            finally opened.library.kit.close () |> SceneTestFixture.kitSuccess)

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``pressing the flow strip's steps shows the Items panel, the Experiments bay and the table`` () : unit =
        HeadlessSession.run (fun () ->
            let ctx = TestAppScope.freshAppContext owner
            let opened = TestAppScope.openedKitIn owner ctx.catalogueImports
            try
                withKitWindow (kitWindowModel ctx opened ForwardConstructor KitWindowShell.inert) (fun window ->
                    let openingTable = namedControl window ConstructorUiId.tableElementCanvas
                    Assert.True(openingTable.IsEffectivelyVisible, "the window must open on the table")
                    Assert.Equal<Control option>(None, tryControlById window ConstructorUiId.itemsPanel)

                    pressBox window ConstructorUiId.flowItemsStep
                    Assert.True((controlById window ConstructorUiId.itemsPanel).IsEffectivelyVisible, "Items must show the Items panel")
                    Assert.Equal<Control option>(None, tryNamedControl window ConstructorUiId.tableElementCanvas)
                    Assert.Equal(MaterialsWindowView.positiveBackground, fillOf window ConstructorUiId.flowItemsStep)

                    pressBox window ConstructorUiId.flowExperimentsStep
                    Assert.True((namedControl window experimentsPane).IsEffectivelyVisible, "Experiments must show the Experiments bay")
                    Assert.Equal<Control option>(None, tryControlById window ConstructorUiId.itemsPanel)
                    Assert.Equal(MaterialsWindowView.positiveBackground, fillOf window ConstructorUiId.flowExperimentsStep)
                    // Spec 0050 S7.2: leaving Items recreates the workbench; Setups ↔ Experiments keeps it.
                    let experimentsTable = namedControl window ConstructorUiId.tableElementCanvas
                    Assert.NotSame(openingTable, experimentsTable)

                    pressBox window ConstructorUiId.flowSetupsStep
                    let setupsTable = namedControl window ConstructorUiId.tableElementCanvas
                    Assert.True(setupsTable.IsEffectivelyVisible, "Setups must show the table")
                    Assert.Same(experimentsTable, setupsTable)
                    Assert.Equal<Control option>(None, tryNamedControl window experimentsPane)
                    Assert.Equal<Control option>(None, tryControlById window ConstructorUiId.itemsPanel)
                    Assert.Equal(MaterialsWindowView.positiveBackground, fillOf window ConstructorUiId.flowSetupsStep))
            finally opened.library.kit.close () |> SceneTestFixture.kitSuccess)

    /// The add form's offer of the shipped circular polarizer model, a standalone item model (spec 0050 S7.2).
    let private circularOffer : ConstructorUiId =
        ConstructorUiId.addItemOffer (LibraryModelChoice (BuiltinCatalogue.itemModel ItemModels.ItemModelIds.circularPolarizerModelId))

    /// Presses the box carrying `id` at the centre of its part inside the window and every viewer around it, where a user
    /// reaches it: in the headless font an offer's caption makes its box wider than the window.
    let private pressShownPart (window : Window) (id : ConstructorUiId) : unit =
        let pressed = Assert.IsType<Border>(controlById window id)
        Assert.True(pressed.IsEffectivelyVisible, $"{id.value} must be visible")
        pressed.BringIntoView() |> ignore
        Dispatcher.UIThread.RunJobs()
        let shown =
            pressed.GetVisualAncestors()
            |> Seq.choose (function :? ScrollViewer as viewer -> Some viewer | _ -> None)
            |> Seq.fold (fun (part : Rect) viewer -> part.Intersect(windowRectOf window viewer)) (Rect window.ClientSize)
            |> fun part -> part.Intersect(windowRectOf window pressed)
        Assert.True(shown.Width > 0.0 && shown.Height > 0.0, $"{id.value} must show a part inside the window")
        window.MouseDown(shown.Center, MouseButton.Left, RawInputModifiers.None)
        Dispatcher.UIThread.RunJobs()
        window.MouseUp(shown.Center, MouseButton.Left, RawInputModifiers.None)
        Dispatcher.UIThread.RunJobs()

    /// CPL1's row as the Items panel names it: the model's caption resolves through the window's item models.
    let private cpl1RowCaption : string = "CPL1 — Circular polarizer (linear polarizer and retarder) v1 — revision 1"

    /// The Items panel shows the row of `item`, under its model's caption.
    let private assertItemRowShown (window : Window) (item : Item) : unit =
        let id = ConstructorUiId.kitItemRow item.id
        Assert.True((controlById window id).IsEffectivelyVisible, $"{id.value} must show in the Items panel")
        Assert.Equal(cpl1RowCaption, boxLabelOf window id)

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``acceptance: an item added in a constructor window is a kit file with its model's snapshot, and the window reopened lists it`` () : unit =
        HeadlessSession.run (fun () ->
            let ctx = TestAppScope.freshAppContext owner
            let opened = TestAppScope.openedKitIn owner ctx.catalogueImports
            let kitRoot = opened.folder.value.value
            let window = MainConstructorWindow(ctx, opened)
            let cpl1 =
                try
                    window.Show()
                    Dispatcher.UIThread.RunJobs()
                    pressBox window ConstructorUiId.flowItemsStep
                    pressBox window ConstructorUiId.addItemButton
                    pressShownPart window circularOffer
                    Assert.Equal(MaterialsWindowView.positiveBackground, fillOf window circularOffer)
                    let field = Assert.IsType<TextBox>(controlById window ConstructorUiId.newItemLabelField)
                    field.Text <- "CPL1"
                    Dispatcher.UIThread.RunJobs()
                    pressBox window ConstructorUiId.addItemConfirmButton
                    // Spec 0050 S7.2, step 239: the window mints with the system provider, so the kit names CPL1's id.
                    let added : Item = Assert.Single(opened.library.kit.proxy.readItems () |> SceneTestFixture.kitSuccess)
                    Assert.Equal<ItemLabel>(ItemLabel "CPL1", added.label)
                    assertItemRowShown window added
                    added
                finally
                    window.Close()
                    Dispatcher.UIThread.RunJobs()
            Assert.True(File.Exists(Path.Combine(kitRoot, "CPL1.item-json")), "Add must leave CPL1.item-json in the kit's folder")
            Assert.True(
                File.Exists(Path.Combine(kitRoot, "library", "33486eec-836a-4cbf-ac79-e3f14439ad4b@v1.item-model-json")),
                "Add must leave the circular polarizer model's snapshot in the kit's library folder")
            let reopened = MainConstructorWindow(ctx, openedAt opened.folder ReadWriteKitAccess ctx)
            try
                reopened.Show()
                Dispatcher.UIThread.RunJobs()
                Assert.Equal<Control option>(None, tryControlById reopened (ConstructorUiId.kitItemRow cpl1.id))
                pressBox reopened ConstructorUiId.flowItemsStep
                assertItemRowShown reopened cpl1
            finally
                reopened.Close()
                Dispatcher.UIThread.RunJobs())

    /// The model with an item labelled `label` added through the Items panel's messages, of the offer the open add form
    /// lists that `offered` accepts (spec 0050 S7.2).
    let private withOfferAdded
        (label : ControlCaption)
        (offered : ItemModelChoice -> bool)
        (model : TableAndElementRotationModel)
        : TableAndElementRotationModel =
        let opened = model |> updateTableAndElementRotation OpenAddItemForm
        let choice =
            match opened.addItemFormOpt |> Option.bind (fun form -> form.offers |> List.tryFind offered) with
            | Some offer -> offer
            | None -> failwith $"The add form must offer a model for {label.value}."
        let filled =
            opened
            |> updateTableAndElementRotation (ChooseNewItemModel choice)
            |> updateTableAndElementRotation (SetNewItemLabel label)
        match filled.addItemFormOpt |> Option.bind (fun form -> form.itemDraftOpt) with
        | Some draft -> filled |> updateTableAndElementRotation (AddItem draft)
        | None -> failwith $"The add form must send {label.value}."

    /// The add form's offer of the linear polarizer preset.
    let private linearPolarizerOffer (offer : ItemModelChoice) : bool =
        match offer with
        | PolarizerPresetChoice (_, polarizer) -> polarizer.id = Library.PresetIds.polarizerLp
        | SourcePresetChoice _
        | StackSampleChoice _
        | LibraryModelChoice _ -> false

    /// The add form's offer of the glass plate `plate`.
    let private plateOffer (plate : Library.SampleId) (offer : ItemModelChoice) : bool =
        match offer with
        | StackSampleChoice (_, _, sample) -> sample.id = plate
        | SourcePresetChoice _
        | PolarizerPresetChoice _
        | LibraryModelChoice _ -> false

    /// The direct problem's sample slot and the two plates swapped into it (spec 0050 S8.4).
    type private DirectProblemPieces =
        {
            sample : Library.ElementId
            qz : ItemId
            qz2 : ItemId
        }

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``acceptance: in a kit window, Record experiment twice, a chip swap and Record experiment again write two setups and three experiments, and the reopened window lists them`` () : unit =
        HeadlessSession.run (fun () ->
            let ctx = TestAppScope.freshAppContext owner
            let folder = TestAppScope.freshKitFolder owner
            let bench = SetupName "Bench"
            let swappedBench = SetupName "Bench (2)"
            let objectRole = ObjectSlotRole (SlotPosition 1)
            let opened = openedAt folder ReadWriteKitAccess ctx
            let pieces =
                try
                    let loaded = kitWindowModel ctx opened ForwardConstructor KitWindowShell.inert
                    // Spec 0050 S7.2: the add form offers the app's item models over the kit's, as the App's windows do.
                    let offering =
                        {
                            loaded with itemModels = ctx.itemModels.overlaidWith opened.overlay
                        }
                    let stocked =
                        offering
                        |> withOfferAdded (ControlCaption "LP1") linearPolarizerOffer
                        |> withOfferAdded (ControlCaption "QZ") (plateOffer Library.SampleIds.glassPlate1mmId)
                        |> withOfferAdded (ControlCaption "QZ2") (plateOffer Library.SampleIds.glassPlate2mmId)
                    let itemOf (label : string) : ItemId =
                        match stocked.kitItems |> List.tryFind (fun item -> item.label = ItemLabel label) with
                        | Some item -> item.id
                        | None -> failwith $"The kit must hold {label}."
                    let slotOf (kind : Placement.CatalogueKind) (model : TableAndElementRotationModel) : Library.ElementId =
                        (model.elements |> List.find (fun slot -> slot.placement.catalogueKind = kind)).id
                    let withPolarizer = stocked |> updateTableAndElementRotation (AddElement Placement.LinearPolarizer)
                    let polarizer = slotOf Placement.LinearPolarizer withPolarizer
                    let withSample = withPolarizer |> updateTableAndElementRotation (AddElement Placement.Sample)
                    let held : DirectProblemPieces =
                        {
                            sample = slotOf Placement.Sample withSample
                            qz = itemOf "QZ"
                            qz2 = itemOf "QZ2"
                        }
                    let ready =
                        withSample
                        |> updateTableAndElementRotation (AssignSlot (polarizer, Some (itemOf "LP1")))
                        |> updateTableAndElementRotation (AssignSlot (held.sample, Some held.qz))
                        |> updateTableAndElementRotation (SetSetupName (ControlCaption "Bench"))
                        |> updateTableAndElementRotation CreateSetup
                        |> updateTableAndElementRotation (ExpChooseElement polarizer)
                    Assert.Equal<ConstructorFlowStep>(SetupsFlowStep, ready.flowStep)
                    Assert.Equal<SetupName option>(Some bench, ready.activeSetupOpt |> Option.map (fun setup -> setup.name))
                    withKitWindow ready (fun window ->
                        pressBox window ConstructorUiId.recordExperimentButton
                        pressBox window ConstructorUiId.recordExperimentButton
                        Assert.Equal("Recorded E2 over 'Bench' revision 1.", textOf window ConstructorUiId.kitStatus)
                        pressBox window (ConstructorUiId.slotChip objectRole)
                        pressBox window (ConstructorUiId.slotChipChoice objectRole held.qz2)
                        Assert.Equal("Created setup 'Bench (2)' by the swap.", textOf window ConstructorUiId.kitStatus)
                        Assert.Equal("Obj 1: QZ2 @ 0° ▾", boxLabelOf window (ConstructorUiId.slotChip objectRole))
                        pressBox window ConstructorUiId.recordExperimentButton
                        Assert.Equal("Recorded E3 over 'Bench (2)' revision 1.", textOf window ConstructorUiId.kitStatus))
                    held
                finally opened.library.kit.close () |> SceneTestFixture.kitSuccess

            Assert.Equal<string list>([ "Bench (2).setup-json"; "Bench.setup-json" ], kitFilesIn folder "*.setup-json")
            Assert.Equal<string list>(
                [ "E1.experiment-json"; "E2.experiment-json"; "E3.experiment-json" ], kitFilesIn folder "*.experiment-json")
            let written = reopenedCopy folder
            let sampleHeldIn (name : SetupName) : ItemId option =
                match written.setups |> List.tryFind (fun setup -> setup.name = name) with
                | Some setup ->
                    Assert.Equal<SetupRevision>(SetupRevision.first, setup.revision)
                    setup.slots |> List.tryFind (fun slot -> slot.id = pieces.sample) |> Option.bind (fun slot -> slot.content.itemOpt)
                | None -> failwith $"The kit must hold the setup '{name.value}'."
            Assert.Equal<ItemId option>(Some pieces.qz, sampleHeldIn bench)
            Assert.Equal<ItemId option>(Some pieces.qz2, sampleHeldIn swappedBench)
            Assert.Equal<string list>(
                [ "E1 over 'Bench' revision 1"; "E2 over 'Bench' revision 1"; "E3 over 'Bench (2)' revision 1" ],
                written.experiments |> List.map (fun experiment -> $"{experiment.name.value} over {experiment.setup.describe}"))
            let reopened = openedAt folder ReadWriteKitAccess ctx
            try
                let reloaded = kitWindowModel ctx reopened ForwardConstructor KitWindowShell.inert
                let experimentIds = [ 1; 2; 3 ] |> List.map Experiments.ExperimentId
                Assert.Equal<SetupName list>([ bench; swappedBench ], reloaded.kitSetups |> List.map (fun setup -> setup.name))
                Assert.Equal<Experiments.ExperimentId list>(
                    experimentIds, reloaded.experimentCollection.experiments |> List.map (fun experiment -> experiment.id))
                withKitWindow (reloaded |> updateTableAndElementRotation (ShowFlowStep ExperimentsFlowStep)) (fun window ->
                    for name in [ bench; swappedBench ] do
                        let id = ConstructorUiId.savedSetup name
                        Assert.True((controlById window id).IsEffectivelyVisible, $"{id.value} must show in the reopened window")
                    for experimentId in experimentIds do
                        let id = ConstructorUiId.experimentEditButton experimentId.uiKey
                        Assert.True((controlById window id).IsEffectivelyVisible, $"{id.value} must show in the reopened window"))
            finally reopened.library.kit.close () |> SceneTestFixture.kitSuccess)
