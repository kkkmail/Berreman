namespace OpticalConstructor.Ui.Tests

open System.IO
open Avalonia.Automation
open Avalonia.Controls
open Avalonia.Threading
open Avalonia.VisualTree
open Xunit
open Softellect.Sys.Primitives
open OpticalConstructor.Controls
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Storage.KitContentFiles
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

    let private controlById (window : Window) (id : ConstructorUiId) : Control =
        window.GetVisualDescendants()
        |> Seq.choose (function :? Control as control -> Some control | _ -> None)
        |> Seq.tryFind (fun control -> AutomationProperties.GetAutomationId control = id.value)
        |> Option.defaultWith (fun () -> failwith $"{id.value} must be in the window")

    /// Mounts `mainView` over the inverse model loaded from the kit opened with `access`, with a draft ready to
    /// commit and the Experiments bay selected, and checks every write control against `expected` (spec 0050 S5.3).
    let private assertWriteControls (folder : KitFolder) (ctx : AppContext) (access : KitAccess) (expected : ControlAvailability) : unit =
        let opened = openedAt folder access ctx
        try
            let seed =
                {
                    initInverseWithKit ctx.library ctx.experiments ctx.materials ctx.samples ctx.categories opened.library.kit with
                        catalogueImports = ctx.catalogueImports
                        sources = ctx.sources
                }
            let loaded = seed.loadKit opened
            let sample = loaded.elements |> List.find (fun element -> element.placement.catalogueKind = Placement.Sample)
            let model =
                loaded
                |> updateTableAndElementRotation (ExpChooseElement sample.id)
                |> updateTableAndElementRotation (ExpChooseVariable ExperimentControls.VaryR2)
                |> updateTableAndElementRotation (SelectBay BayNames.experiments)
            Assert.Equal<Experiments.Experiment list>([ storedExperiment ], model.experimentCollection.experiments)
            Assert.True(model.experimentCollection.canCommit, "the draft must be ready to commit")
            let window = HostWindow()
            Program.mkSimple (fun () -> model) updateTableAndElementRotation mainView |> Program.withHost window |> Program.run
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
