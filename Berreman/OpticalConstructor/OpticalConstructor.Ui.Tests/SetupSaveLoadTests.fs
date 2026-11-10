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
open OpticalConstructor.Storage.KitContentFiles
open OpticalConstructor.Tests
open OpticalConstructor.Ui
open OpticalConstructor.Ui.KitOpening
open OpticalConstructor.Ui.TableAndElementRotationView

/// Setup commands record the table and its library references in the opened kit and draw a kit setup back on the
/// table (spec 0050 S4.3, S8.2).
module SetupSaveLoadTests =

    let private owner : FolderName = FolderName "SetupSaveLoadTests"

    let private setupModelInKit (stores : AppContext) (opened : OpenedKit) : TableAndElementRotationModel =
        let model =
            {
                initMainWithKit stores.library stores.experiments stores.materials stores.samples stores.categories opened.library.kit with
                    catalogueImports = stores.catalogueImports
                    sources = stores.sources
            }
        model.loadKit opened

    let private openSetupKit (folder : KitFolder) (stores : AppContext) : TableAndElementRotationModel =
        let author = KitAuthor "test"
        openKitAt folder ReadWriteKitAccess ClockProvider.system author stores.catalogueImports
        |> Result.defaultWith (fun error -> failwith error.describe)
        |> setupModelInKit stores

    let private findSetupControl (id : ConstructorUiId) (window : Window) : Control =
        window.GetVisualDescendants()
        |> Seq.choose (function :? Control as control -> Some control | _ -> None)
        |> Seq.find (fun control -> AutomationProperties.GetAutomationId(control) = id.value)

    let private clickSetupControl (id : ConstructorUiId) (window : Window) : unit =
        let button = findSetupControl id window :?> Border
        Assert.False(System.String.IsNullOrWhiteSpace(AutomationProperties.GetName button))
        button.BringIntoView() |> ignore
        Dispatcher.UIThread.RunJobs()
        match button.TranslatePoint(Point(button.Bounds.Width / 2.0, button.Bounds.Height / 2.0), window) |> Option.ofNullable with
        | Some point ->
            window.MouseDown(point, MouseButton.Left, RawInputModifiers.None)
            window.MouseUp(point, MouseButton.Left, RawInputModifiers.None)
            Dispatcher.UIThread.RunJobs()
        | None -> Assert.Fail($"{id.value} must have a position in its window.")

    let private createdAs (caption : ControlCaption) (model : TableAndElementRotationModel) : TableAndElementRotationModel =
        model
        |> updateTableAndElementRotation (SetSetupName caption)
        |> updateTableAndElementRotation CreateSetup

    /// The kit folder's setup files, by name in ordinal order.
    let private setupFilesIn (folder : KitFolder) : string list =
        Directory.GetFiles(folder.value.value, "*.setup-json")
        |> Array.map Path.GetFileName
        |> Array.sort
        |> List.ofArray

    let private setupNames (setups : Setups.Setup list) : SetupName list = setups |> List.map (fun setup -> setup.name)

    [<Fact>]
    let ``a created setup is a setup file in the kit and loads back onto the table`` () : unit =
        let stores = TestAppScope.freshAppContext owner
        let folder = TestAppScope.freshKitFolder owner
        let initial = openSetupKit folder stores
        let name = SetupName "Bench: A"
        let caption = ControlCaption "Bench: A"
        try
            let placed = initial |> updateTableAndElementRotation (AddElement Placement.LinearPolarizer)
            let polarizer = placed.elements |> List.find (fun element -> element.placement.catalogueKind = Placement.LinearPolarizer)
            Assert.Equal<Placement.ElementValueId option>(Some Library.PresetIds.polarizerLp, polarizer.placement.valueId)
            let entryId =
                match polarizer.placement.valueId |> Option.bind _.tryToLibraryEntryId with
                | Some id -> id
                | None -> failwith "The added linear polarizer must be bound to its catalogue preset."
            let created = placed |> createdAs caption
            Assert.Equal(caption, created.setupName)
            let bench = Assert.Single(created.kitSetups)
            Assert.Equal<SetupName>(name, bench.name)
            Assert.Equal<Setups.SetupSlot list>(placed.elements, bench.slots)
            Assert.Equal<Setups.Setup option>(Some bench, created.activeSetupOpt)
            Assert.Equal<ControlCaption option>(Some (ControlCaption "Created setup 'Bench: A'."), created.kitStatus)
            Assert.Equal<string list>([ "Bench_ A.setup-json" ], setupFilesIn folder)
            Assert.Equal<Setups.Setup list>([ bench ], created.kit.proxy.readSetups () |> SceneTestFixture.kitSuccess)
            let snapshots = created.kit.proxy.listSnapshots () |> SceneTestFixture.kitSuccess
            let snapshot = snapshots |> List.find (fun envelope -> envelope.content.id = entryId)
            Assert.StartsWith("library/", snapshot.snapshotPath.value)
            Assert.True(File.Exists(Path.Combine(folder.value.value, snapshot.snapshotPath.value)))
            let removed = created |> updateTableAndElementRotation RemoveSelectedElement
            Assert.Equal<Setups.SetupSlot list>(initial.elements, removed.elements)
            let loaded = removed |> updateTableAndElementRotation (ActivateSetup name)
            Assert.Equal<Setups.SetupSlot list>(placed.elements, loaded.elements)
            Assert.Equal<Setups.Setup option>(Some bench, loaded.activeSetupOpt)
            Assert.Equal(TableSelected, loaded.selection)
            Assert.Equal<ControlCaption option>(Some (ControlCaption "Loaded setup 'Bench: A'."), loaded.kitStatus)

            // Spec 0050 S8.2: a setup is created once; its name, held again, is refused and its one file stays.
            let repeated = loaded |> updateTableAndElementRotation CreateSetup
            Assert.Equal<Setups.Setup list>([ bench ], repeated.kitSetups)
            match repeated.kitStatus with
            | Some status -> Assert.StartsWith("Not created", status.value)
            | None -> Assert.Fail("A name the kit holds must be told.")
            Assert.Equal<string list>([ "Bench_ A.setup-json" ], setupFilesIn folder)
            let upperName = SetupName "Z bench"
            let lowerName = SetupName "a bench"
            let ordered =
                repeated
                |> createdAs (ControlCaption "a bench")
                |> createdAs (ControlCaption "Z bench")
            Assert.Equal<SetupName list>([ name; upperName; lowerName ], setupNames ordered.kitSetups)
            let protectedModel =
                {
                    ordered with
                        kit =
                            {
                                ordered.kit with
                                    proxy = ordered.kit.proxy.readOnlyView
                            }
                }
            let protectedCaption = ControlCaption "Protected bench"
            let refused = protectedModel |> createdAs protectedCaption
            Assert.Equal<Setups.Setup list>(ordered.kitSetups, refused.kitSetups)
            Assert.Equal<Setups.SetupSlot list>(ordered.elements, refused.elements)
            Assert.Equal<Setups.Setup option>(ordered.activeSetupOpt, refused.activeSetupOpt)
            Assert.Equal(protectedCaption, refused.setupName)
            match refused.kitStatus with
            | Some status ->
                Assert.StartsWith("Not saved to the kit — ", status.value)
                Assert.Contains("read-only", status.value)
            | None -> Assert.Fail("A refused setup write must explain its kit error.")
            Assert.Equal<Setups.Setup list>(ordered.kitSetups, initial.kit.proxy.readSetups () |> SceneTestFixture.kitSuccess)
        finally
            initial.kit.close () |> SceneTestFixture.kitSuccess

        let reopened = openSetupKit folder stores
        try
            Assert.Contains<SetupName>(name, setupNames reopened.kitSetups)
            let restored = reopened |> updateTableAndElementRotation (ActivateSetup name)
            Assert.Contains<Setups.SetupSlot>(restored.elements, fun element -> element.placement.catalogueKind = Placement.LinearPolarizer)
        finally
            reopened.kit.close () |> SceneTestFixture.kitSuccess

    [<Fact>]
    let ``creating a setup with a blank name, or one the kit holds, reports a typed status and writes nothing`` () : unit =
        let stores = TestAppScope.freshAppContext owner
        let folder = TestAppScope.freshKitFolder owner
        let initial = openSetupKit folder stores
        let blankCaption = ControlCaption "   "
        try
            let refused = initial |> createdAs blankCaption
            Assert.Equal(blankCaption, refused.setupName)
            Assert.Equal<Setups.SetupSlot list>(initial.elements, refused.elements)
            Assert.Empty(refused.kitSetups)
            Assert.Equal<Setups.Setup option>(None, refused.activeSetupOpt)
            Assert.Empty(setupFilesIn folder)
            Assert.Empty(initial.kit.proxy.listSnapshots () |> SceneTestFixture.kitSuccess)
            match refused.kitStatus with
            | Some status ->
                Assert.StartsWith("Not created", status.value)
                match SetupName.tryCreate blankCaption.value with
                | Error error -> Assert.Contains(error.describe, status.value)
                | Ok _ -> Assert.Fail("The blank setup name must be rejected.")
            | None -> Assert.Fail("A blank setup name must report its validation error.")

            let created = initial |> createdAs (ControlCaption "Bench: A")
            let bench = Assert.Single(created.kitSetups)
            let benchFile = Path.Combine(folder.value.value, "Bench_ A.setup-json")
            let benchText = File.ReadAllText benchFile
            let snapshotsBefore = created.kit.proxy.listSnapshots () |> SceneTestFixture.kitSuccess
            let placed = created |> updateTableAndElementRotation (AddElement Placement.LinearPolarizer)
            let again = placed |> updateTableAndElementRotation CreateSetup
            Assert.Equal<Setups.Setup list>([ bench ], again.kitSetups)
            Assert.Equal<Setups.Setup option>(Some bench, again.activeSetupOpt)
            Assert.Equal<Setups.SetupSlot list>(placed.elements, again.elements)
            match again.kitStatus with
            | Some status ->
                Assert.StartsWith("Not created", status.value)
                match Setups.Setup.create bench.name placed.elements [ bench ] with
                | Error error -> Assert.Contains(error.describe, status.value)
                | Ok _ -> Assert.Fail("A name the kit holds must be refused.")
            | None -> Assert.Fail("A name the kit holds must report its refusal.")
            Assert.Equal<string list>([ "Bench_ A.setup-json" ], setupFilesIn folder)
            Assert.Equal(benchText, File.ReadAllText benchFile)
            Assert.Equal<LibraryEntryEnvelope list>(snapshotsBefore, again.kit.proxy.listSnapshots () |> SceneTestFixture.kitSuccess)
        finally
            initial.kit.close () |> SceneTestFixture.kitSuccess

    [<Fact>]
    let ``loading a setup the kit does not hold reports a typed status and leaves the table alone`` () : unit =
        let stores = TestAppScope.freshAppContext owner
        let initial = TestAppScope.openedKitIn owner stores.catalogueImports |> setupModelInKit stores
        let name = SetupName "nowhere"
        try
            let placed = initial |> updateTableAndElementRotation (AddElement Placement.LinearPolarizer)
            let refused = placed |> updateTableAndElementRotation (ActivateSetup name)
            Assert.Equal<Setups.SetupSlot list>(placed.elements, refused.elements)
            Assert.Equal<Table.TableViewState>(placed.view, refused.view)
            Assert.Equal(placed.selection, refused.selection)
            Assert.Equal<Setups.Setup list>(placed.kitSetups, refused.kitSetups)
            Assert.Equal<Setups.Setup option>(placed.activeSetupOpt, refused.activeSetupOpt)
            let unknown = UnknownSetupErr $"'{name.value}'"
            Assert.Equal<ControlCaption option>(Some (ControlCaption $"Load failed — {unknown.describe}"), refused.kitStatus)
        finally
            initial.kit.close () |> SceneTestFixture.kitSuccess

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the setup row renders its name field, Create setup and the saved setups`` () : unit =
        HeadlessSession.run (fun () ->
            let stores = TestAppScope.freshAppContext owner
            let initial = TestAppScope.openedKitIn owner stores.catalogueImports |> setupModelInKit stores
            let name = SetupName "Bench: A"
            let caption = ControlCaption "Bench: A"
            try
                let created =
                    initial
                    |> createdAs caption
                    |> updateTableAndElementRotation (SelectBay BayNames.experiments)
                for mode in [ ForwardConstructor; InverseConstructor ] do
                    let model = { created with constructorMode = mode }
                    let messages = ResizeArray<TableAndElementRotationMsg>()
                    let window = Window(Width = 1000.0, Height = 980.0)
                    window.Content <- Component(fun _ -> mainView model messages.Add)
                    try
                        window.Show()
                        Dispatcher.UIThread.RunJobs()
                        let nameField = findSetupControl ConstructorUiId.setupNameField window :?> TextBox
                        let savedSetups = findSetupControl ConstructorUiId.savedSetups window :?> WrapPanel
                        Assert.Equal(caption.value, nameField.Text)
                        Assert.Equal(ConstructorUiId.savedSetups.value, savedSetups.Name)
                        Assert.Equal("Setup name", AutomationProperties.GetName nameField)
                        Assert.Equal("Saved setups", AutomationProperties.GetName savedSetups)
                        let committed = ControlCaption "Renamed bench"
                        nameField.Text <- committed.value
                        nameField.RaiseEvent(KeyEventArgs(RoutedEvent = InputElement.KeyDownEvent, Key = Key.Enter))
                        Assert.True(messages |> Seq.exists (function SetSetupName text -> text = committed | _ -> false))
                        messages.Clear()
                        nameField.RaiseEvent(FocusChangedEventArgs(InputElement.LostFocusEvent))
                        Assert.True(messages |> Seq.exists (function SetSetupName text -> text = committed | _ -> false))
                        for id in [ ConstructorUiId.createSetupButton; ConstructorUiId.savedSetup name ] do
                            clickSetupControl id window
                        Assert.True(messages |> Seq.exists (function CreateSetup -> true | _ -> false))
                        Assert.True(messages |> Seq.exists (function ActivateSetup setup -> setup = name | _ -> false))
                    finally
                        window.Close()

                    let emptyCaption = ControlCaption ""
                    let seed =
                        {
                            created with
                                constructorMode = mode
                                setupName = emptyCaption
                        }
                        |> updateTableAndElementRotation (AddElement Placement.LinearPolarizer)
                    let mutable current = seed
                    let directMessages = ResizeArray<TableAndElementRotationMsg>()
                    let createInputs = ResizeArray<TableAndElementRotationModel>()
                    let renderErrors = ResizeArray<System.Exception>()
                    let reduce (message : TableAndElementRotationMsg) (model : TableAndElementRotationModel) : TableAndElementRotationModel =
                        directMessages.Add message
                        match message with
                        | CreateSetup -> createInputs.Add model
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
                            | ForwardConstructor -> [ SetupName "Forward direct first"; SetupName "Forward direct second" ]
                            | InverseConstructor -> [ SetupName "Inverse direct first"; SetupName "Inverse direct second" ]
                        for editedName in editedNames do
                            let editedCaption = editedName.caption
                            let previousCaption = current.setupName
                            let previousSetups = current.kit.proxy.readSetups () |> SceneTestFixture.kitSuccess
                            Assert.DoesNotContain<SetupName>(editedName, setupNames previousSetups)
                            let nameField = findSetupControl ConstructorUiId.setupNameField liveWindow :?> TextBox
                            nameField.BringIntoView() |> ignore
                            Assert.True(nameField.Focus())
                            Dispatcher.UIThread.RunJobs()
                            nameField.SelectAll()
                            directMessages.Clear()
                            createInputs.Clear()
                            liveWindow.KeyTextInput(editedCaption.value)
                            Dispatcher.UIThread.RunJobs()
                            Assert.True(nameField.IsFocused)
                            Assert.Equal(editedCaption.value, nameField.Text)
                            Assert.Equal(previousCaption, current.setupName)
                            Assert.Empty(directMessages)

                            clickSetupControl ConstructorUiId.createSetupButton liveWindow

                            Assert.Empty renderErrors
                            let savedSetups = findSetupControl ConstructorUiId.savedSetups liveWindow :?> WrapPanel
                            Assert.Equal(ConstructorUiId.savedSetups.value, savedSetups.Name)
                            Assert.Equal("Saved setups", AutomationProperties.GetName savedSetups)
                            Assert.Equal<TableAndElementRotationMsg list>([ SetSetupName editedCaption; CreateSetup ], List.ofSeq directMessages)
                            let createInput = Assert.Single(createInputs)
                            Assert.Equal(editedCaption, createInput.setupName)
                            Assert.Equal(editedCaption, current.setupName)
                            Assert.Contains<SetupName>(editedName, setupNames current.kitSetups)
                            let expectedStatus = ControlCaption $"Created setup '{editedName.value}'."
                            Assert.Equal<ControlCaption option>(Some expectedStatus, current.kitStatus)
                            let persisted = current.kit.proxy.readSetups () |> SceneTestFixture.kitSuccess
                            let createdSetup = persisted |> List.find (fun setup -> setup.name = editedName)
                            Assert.Equal<Setups.SetupSlot list>(seed.elements, createdSetup.slots)
                            for previous in previousSetups do
                                Assert.Contains<Setups.Setup>(previous, persisted)
                            let savedButton = findSetupControl (ConstructorUiId.savedSetup editedName) liveWindow
                            Assert.Equal(editedCaption.value, AutomationProperties.GetName savedButton)
                    finally
                        liveWindow.Close()
            finally
                initial.kit.close () |> SceneTestFixture.kitSuccess)
