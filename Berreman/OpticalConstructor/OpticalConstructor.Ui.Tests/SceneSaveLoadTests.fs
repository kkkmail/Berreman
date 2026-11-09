namespace OpticalConstructor.Ui.Tests

open System.IO
open Avalonia
open Avalonia.Automation
open Avalonia.Controls
open Avalonia.FuncUI
open Avalonia.FuncUI.Elmish
open Avalonia.FuncUI.Hosts
open Avalonia.Headless
open Avalonia.Input
open Avalonia.Interactivity
open Avalonia.Threading
open Avalonia.VisualTree
open global.Elmish
open Xunit
open Softellect.Sys.Primitives
open OpticalConstructor.Controls
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.KitFolders
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.Scene
open OpticalConstructor.Storage.KitContentFiles
open OpticalConstructor.Tests
open OpticalConstructor.Ui
open OpticalConstructor.Ui.KitOpening
open OpticalConstructor.Ui.TableAndElementRotationView

/// Scene commands persist the table and its library references in the opened kit (spec 0050 S4.3).
module SceneSaveLoadTests =

    let private owner : FolderName = FolderName "SceneSaveLoadTests"

    let private sceneInKit (stores : AppContext) (opened : OpenedKit) : TableAndElementRotationModel =
        let model =
            {
                initMainWithKit stores.library stores.experiments stores.materials stores.samples stores.categories opened.library.kit with
                    catalogueImports = stores.catalogueImports
                    sources = stores.sources
            }
        model.loadKit opened

    let private openSceneKit (folder : KitFolder) (stores : AppContext) : TableAndElementRotationModel =
        let author = KitAuthor "test"
        openKitAt folder ReadWriteKitAccess ClockProvider.system author stores.catalogueImports
        |> Result.defaultWith (fun error -> failwith error.describe)
        |> sceneInKit stores

    let private findSceneControl (id : ConstructorUiId) (window : Window) : Control =
        window.GetVisualDescendants()
        |> Seq.choose (function :? Control as control -> Some control | _ -> None)
        |> Seq.find (fun control -> AutomationProperties.GetAutomationId(control) = id.value)

    let private clickSceneControl (id : ConstructorUiId) (window : Window) : unit =
        let button = findSceneControl id window :?> Border
        Assert.False(System.String.IsNullOrWhiteSpace(AutomationProperties.GetName button))
        button.BringIntoView() |> ignore
        Dispatcher.UIThread.RunJobs()
        match button.TranslatePoint(Point(button.Bounds.Width / 2.0, button.Bounds.Height / 2.0), window) |> Option.ofNullable with
        | Some point ->
            window.MouseDown(point, MouseButton.Left, RawInputModifiers.None)
            window.MouseUp(point, MouseButton.Left, RawInputModifiers.None)
            Dispatcher.UIThread.RunJobs()
        | None -> Assert.Fail($"{id.value} must have a position in its window.")

    [<Fact>]
    let ``a saved scene is a setup file in the kit and loads back onto the table`` () : unit =
        let stores = TestAppScope.freshAppContext owner
        let folder = TestAppScope.freshKitFolder owner
        let initial = openSceneKit folder stores
        let name = SceneName "Bench: A"
        let caption = ControlCaption "Bench: A"
        try
            let placed = initial |> updateTableAndElementRotation (AddElement Placement.LinearPolarizer)
            let polarizer = placed.elements |> List.find (fun element -> element.placement.catalogueKind = Placement.LinearPolarizer)
            Assert.Equal<Placement.ElementValueId option>(Some Library.PresetIds.polarizerLp, polarizer.placement.valueId)
            let entryId =
                match polarizer.placement.valueId |> Option.bind _.tryToLibraryEntryId with
                | Some id -> id
                | None -> failwith "The added linear polarizer must be bound to its catalogue preset."
            let saved =
                placed
                |> updateTableAndElementRotation (SceneSetName caption)
                |> updateTableAndElementRotation SceneSave
            Assert.Equal(caption, saved.sceneName)
            Assert.Equal<SceneName list>([ name ], saved.savedScenes)
            Assert.Equal<ControlCaption option>(Some (ControlCaption "Saved scene 'Bench: A'."), saved.kitStatus)
            let setupName = FileName "Bench_ A.setup-json"
            let setupPath = setupName.combine folder.value
            Assert.True(File.Exists setupPath.value)
            Assert.Equal(placed.captureScene, saved.kit.proxy.readScene name |> SceneTestFixture.kitSuccess)
            let snapshots = saved.kit.proxy.listSnapshots () |> SceneTestFixture.kitSuccess
            let snapshot = snapshots |> List.find (fun envelope -> envelope.content.id = entryId)
            Assert.StartsWith("library/", snapshot.snapshotPath.value)
            Assert.True(File.Exists(Path.Combine(folder.value.value, snapshot.snapshotPath.value)))
            let removed = saved |> updateTableAndElementRotation RemoveSelectedElement
            Assert.Equal<TestElement list>(initial.elements, removed.elements)
            let loaded = removed |> updateTableAndElementRotation (SceneLoad name)
            Assert.Equal<TestElement list>(placed.elements, loaded.elements)
            Assert.Equal(placed.captureScene, loaded.captureScene)
            Assert.Equal(TableSelected, loaded.selection)
            Assert.Equal<ControlCaption option>(Some (ControlCaption "Loaded scene 'Bench: A'."), loaded.kitStatus)

            let repeated = loaded |> updateTableAndElementRotation SceneSave
            Assert.Equal<SceneName list>([ name ], repeated.savedScenes)
            Assert.Single(Directory.GetFiles(folder.value.value, "*.setup-json")) |> ignore
            let upperName = SceneName "Z bench"
            let lowerName = SceneName "a bench"
            let upperCaption = ControlCaption "Z bench"
            let lowerCaption = ControlCaption "a bench"
            let ordered =
                repeated
                |> updateTableAndElementRotation (SceneSetName lowerCaption)
                |> updateTableAndElementRotation SceneSave
                |> updateTableAndElementRotation (SceneSetName upperCaption)
                |> updateTableAndElementRotation SceneSave
            Assert.Equal<SceneName list>([ name; upperName; lowerName ], ordered.savedScenes)
            let protectedModel =
                {
                    ordered with
                        kit =
                            {
                                ordered.kit with
                                    proxy = ordered.kit.proxy.readOnlyView
                            }
                }
            let refused = protectedModel |> updateTableAndElementRotation SceneSave
            Assert.Equal<SceneName list>(ordered.savedScenes, refused.savedScenes)
            Assert.Equal(ordered.captureScene, refused.captureScene)
            Assert.Equal(ordered.sceneName, refused.sceneName)
            match refused.kitStatus with
            | Some status ->
                Assert.StartsWith("Not saved to the kit — ", status.value)
                Assert.Contains("read-only", status.value)
            | None -> Assert.Fail("A refused scene save must explain its kit error.")
            Assert.Equal<SceneName list>(ordered.savedScenes, initial.kit.proxy.readSceneNames () |> SceneTestFixture.kitSuccess)
        finally
            initial.kit.close () |> SceneTestFixture.kitSuccess

        let reopened = openSceneKit folder stores
        try
            Assert.Contains<SceneName>(name, reopened.savedScenes)
            let restored = reopened |> updateTableAndElementRotation (SceneLoad name)
            Assert.Contains<TestElement>(restored.elements, fun element -> element.placement.catalogueKind = Placement.LinearPolarizer)
        finally
            reopened.kit.close () |> SceneTestFixture.kitSuccess

    [<Fact>]
    let ``saving a scene with a blank name reports a typed status and writes nothing`` () : unit =
        let stores = TestAppScope.freshAppContext owner
        let folder = TestAppScope.freshKitFolder owner
        let initial = openSceneKit folder stores
        let caption = ControlCaption "   "
        try
            let draft = initial |> updateTableAndElementRotation (SceneSetName caption)
            let refused = draft |> updateTableAndElementRotation SceneSave
            Assert.Equal(caption, refused.sceneName)
            Assert.Equal(initial.captureScene, refused.captureScene)
            Assert.Empty(refused.savedScenes)
            Assert.Empty(Directory.GetFiles(folder.value.value, "*.setup-json"))
            Assert.Empty(initial.kit.proxy.listSnapshots () |> SceneTestFixture.kitSuccess)
            match refused.kitStatus with
            | Some status ->
                Assert.StartsWith("Save failed", status.value)
                match SceneName.tryCreate caption.value with
                | Error (InvalidSceneErr reason) -> Assert.Contains(reason, status.value)
                | Ok _ -> Assert.Fail("The blank scene name must be rejected.")
            | None -> Assert.Fail("A blank scene name must report its validation error.")
        finally
            initial.kit.close () |> SceneTestFixture.kitSuccess

    [<Fact>]
    let ``loading a scene the kit does not hold reports a typed status and leaves the table alone`` () : unit =
        let stores = TestAppScope.freshAppContext owner
        let initial = TestAppScope.openedKitIn owner stores.catalogueImports |> sceneInKit stores
        let name = SceneName "nowhere"
        try
            let placed = initial |> updateTableAndElementRotation (AddElement Placement.LinearPolarizer)
            let refused = placed |> updateTableAndElementRotation (SceneLoad name)
            Assert.Equal<TestElement list>(placed.elements, refused.elements)
            Assert.Equal(placed.captureScene, refused.captureScene)
            Assert.Equal(placed.selection, refused.selection)
            Assert.Equal<SceneName list>(placed.savedScenes, refused.savedScenes)
            match refused.kitStatus with
            | Some status ->
                Assert.StartsWith("Load failed", status.value)
                match initial.kit.proxy.readScene name with
                | Error error -> Assert.Contains(error.describe, status.value)
                | Ok _ -> Assert.Fail("The missing scene must return a typed kit error.")
            | None -> Assert.Fail("A missing scene must report its kit error.")
        finally
            initial.kit.close () |> SceneTestFixture.kitSuccess

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the scene row renders its name field, Save scene and the saved scenes`` () : unit =
        HeadlessSession.run (fun () ->
            let stores = TestAppScope.freshAppContext owner
            let initial = TestAppScope.openedKitIn owner stores.catalogueImports |> sceneInKit stores
            let name = SceneName "Bench: A"
            let caption = ControlCaption "Bench: A"
            try
                let saved =
                    initial
                    |> updateTableAndElementRotation (SceneSetName caption)
                    |> updateTableAndElementRotation SceneSave
                    |> updateTableAndElementRotation (SelectBay BayNames.experiments)
                for mode in [ ForwardConstructor; InverseConstructor ] do
                    let model = { saved with constructorMode = mode }
                    let messages = ResizeArray<TableAndElementRotationMsg>()
                    let window = Window(Width = 1000.0, Height = 980.0)
                    window.Content <- Component(fun _ -> mainView model messages.Add)
                    try
                        window.Show()
                        Dispatcher.UIThread.RunJobs()
                        let nameField = findSceneControl ConstructorUiId.sceneNameField window :?> TextBox
                        let savedScenes = findSceneControl ConstructorUiId.savedScenes window :?> WrapPanel
                        Assert.Equal(caption.value, nameField.Text)
                        Assert.Equal(ConstructorUiId.savedScenes.value, savedScenes.Name)
                        Assert.Equal("Scene name", AutomationProperties.GetName nameField)
                        Assert.Equal("Saved scenes", AutomationProperties.GetName savedScenes)
                        let committed = ControlCaption "Renamed bench"
                        nameField.Text <- committed.value
                        nameField.RaiseEvent(KeyEventArgs(RoutedEvent = InputElement.KeyDownEvent, Key = Key.Enter))
                        Assert.True(messages |> Seq.exists (function SceneSetName text -> text = committed | _ -> false))
                        messages.Clear()
                        nameField.RaiseEvent(FocusChangedEventArgs(InputElement.LostFocusEvent))
                        Assert.True(messages |> Seq.exists (function SceneSetName text -> text = committed | _ -> false))
                        for id in [ ConstructorUiId.saveSceneButton; ConstructorUiId.savedScene name ] do
                            clickSceneControl id window
                        Assert.True(messages |> Seq.exists (function SceneSave -> true | _ -> false))
                        Assert.True(messages |> Seq.exists (function SceneLoad scene -> scene = name | _ -> false))
                    finally
                        window.Close()

                    let emptyCaption = ControlCaption ""
                    let seed =
                        {
                            saved with
                                constructorMode = mode
                                sceneName = emptyCaption
                        }
                        |> updateTableAndElementRotation (AddElement Placement.LinearPolarizer)
                    let mutable current = seed
                    let directMessages = ResizeArray<TableAndElementRotationMsg>()
                    let saveInputs = ResizeArray<TableAndElementRotationModel>()
                    let renderErrors = ResizeArray<System.Exception>()
                    let reduce (message : TableAndElementRotationMsg) (model : TableAndElementRotationModel) : TableAndElementRotationModel =
                        directMessages.Add message
                        match message with
                        | SceneSave -> saveInputs.Add model
                        | _ -> ()
                        current <- updateTableAndElementRotation message model
                        current
                    let liveWindow = HostWindow(Width = 1000.0, Height = 980.0)
                    Program.mkSimple (fun () -> seed) reduce mainView
                    |> Program.withHost liveWindow
                    |> Program.withErrorHandler (fun (_, error) -> renderErrors.Add error)
                    |> Program.run
                    try
                        liveWindow.Show()
                        Dispatcher.UIThread.RunJobs()
                        let editedNames =
                            match mode with
                            | ForwardConstructor -> [ SceneName "Forward direct first"; SceneName "Forward direct second" ]
                            | InverseConstructor -> [ SceneName "Inverse direct first"; SceneName "Inverse direct second" ]
                        for editedName in editedNames do
                            let editedCaption = editedName.caption
                            let previousCaption = current.sceneName
                            let previousScenes =
                                current.kit.proxy.readSceneNames ()
                                |> SceneTestFixture.kitSuccess
                                |> List.map (fun sceneName -> sceneName, current.kit.proxy.readScene sceneName |> SceneTestFixture.kitSuccess)
                            Assert.DoesNotContain<SceneName>(editedName, previousScenes |> List.map fst)
                            let nameField = findSceneControl ConstructorUiId.sceneNameField liveWindow :?> TextBox
                            nameField.BringIntoView() |> ignore
                            Assert.True(nameField.Focus())
                            Dispatcher.UIThread.RunJobs()
                            nameField.SelectAll()
                            directMessages.Clear()
                            saveInputs.Clear()
                            liveWindow.KeyTextInput(editedCaption.value)
                            Dispatcher.UIThread.RunJobs()
                            Assert.True(nameField.IsFocused)
                            Assert.Equal(editedCaption.value, nameField.Text)
                            Assert.Equal(previousCaption, current.sceneName)
                            Assert.Empty(directMessages)

                            clickSceneControl ConstructorUiId.saveSceneButton liveWindow

                            Assert.Empty renderErrors
                            let savedScenes = findSceneControl ConstructorUiId.savedScenes liveWindow :?> WrapPanel
                            Assert.Equal(ConstructorUiId.savedScenes.value, savedScenes.Name)
                            Assert.Equal("Saved scenes", AutomationProperties.GetName savedScenes)
                            Assert.Equal<TableAndElementRotationMsg list>([ SceneSetName editedCaption; SceneSave ], List.ofSeq directMessages)
                            let saveInput = Assert.Single(saveInputs)
                            Assert.Equal(editedCaption, saveInput.sceneName)
                            Assert.Equal(editedCaption, current.sceneName)
                            Assert.Contains<SceneName>(editedName, current.savedScenes)
                            let expectedStatus = ControlCaption $"Saved scene '{editedName.value}'."
                            Assert.Equal<ControlCaption option>(Some expectedStatus, current.kitStatus)
                            let persisted = current.kit.proxy.readScene editedName |> SceneTestFixture.kitSuccess
                            Assert.Equal(seed.captureScene, persisted)
                            Assert.Contains<SceneName>(editedName, current.kit.proxy.readSceneNames () |> SceneTestFixture.kitSuccess)
                            for previousName, previousScene in previousScenes do
                                Assert.Equal(previousScene, current.kit.proxy.readScene previousName |> SceneTestFixture.kitSuccess)
                            let savedButton = findSceneControl (ConstructorUiId.savedScene editedName) liveWindow
                            Assert.Equal(editedCaption.value, AutomationProperties.GetName savedButton)
                    finally
                        liveWindow.Close()
            finally
                initial.kit.close () |> SceneTestFixture.kitSuccess)
