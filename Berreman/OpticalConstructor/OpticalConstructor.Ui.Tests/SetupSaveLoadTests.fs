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
open FsCheck
open FsCheck.FSharp
open BerremanTestSupport
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

/// Setup commands record the table and its library references in the opened kit, write it over the active setup and
/// draw a kit setup back on the table (spec 0050 S4.3, S8.2).
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

    /// Presses the box carrying `id` at its centre.
    let private pressSetupControl (id : ConstructorUiId) (window : Window) : unit =
        let button = findSetupControl id window
        button.BringIntoView() |> ignore
        Dispatcher.UIThread.RunJobs()
        match button.TranslatePoint(Point(button.Bounds.Width / 2.0, button.Bounds.Height / 2.0), window) |> Option.ofNullable with
        | Some point ->
            window.MouseDown(point, MouseButton.Left, RawInputModifiers.None)
            window.MouseUp(point, MouseButton.Left, RawInputModifiers.None)
            Dispatcher.UIThread.RunJobs()
        | None -> Assert.Fail($"{id.value} must have a position in its window.")

    /// Presses the setup row's box carrying `id`, which must carry an automation name.
    let private clickSetupControl (id : ConstructorUiId) (window : Window) : unit =
        Assert.False(System.String.IsNullOrWhiteSpace(AutomationProperties.GetName(findSetupControl id window)))
        pressSetupControl id window

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
            let placed = initial |> SceneTestFixture.withPresetElement Placement.LinearPolarizer
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
            Assert.Equal<ControlCaption option>(Some (ControlCaption "Created setup 'Bench: A'."), created.kitStatusCaption)
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
            // Spec 0051 `.manual/007` 5.1: choosing a setup is browsing, so it says nothing.
            Assert.Equal<ControlCaption option>(removed.kitStatusCaption, loaded.kitStatusCaption)

            // Spec 0050 S8.2: a setup is created once; its name, held again, is refused and its one file stays.
            let repeated = loaded |> updateTableAndElementRotation CreateSetup
            Assert.Equal<Setups.Setup list>([ bench ], repeated.kitSetups)
            match repeated.kitStatusCaption with
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
            match refused.kitStatusCaption with
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
            match refused.kitStatusCaption with
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
            let placed = created |> SceneTestFixture.withPresetElement Placement.LinearPolarizer
            let again = placed |> updateTableAndElementRotation CreateSetup
            Assert.Equal<Setups.Setup list>([ bench ], again.kitSetups)
            Assert.Equal<Setups.Setup option>(Some bench, again.activeSetupOpt)
            Assert.Equal<Setups.SetupSlot list>(placed.elements, again.elements)
            match again.kitStatusCaption with
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
            let placed = initial |> SceneTestFixture.withPresetElement Placement.LinearPolarizer
            let refused = placed |> updateTableAndElementRotation (ActivateSetup name)
            Assert.Equal<Setups.SetupSlot list>(placed.elements, refused.elements)
            Assert.Equal<Table.TableViewState>(placed.view, refused.view)
            Assert.Equal(placed.selection, refused.selection)
            Assert.Equal<Setups.Setup list>(placed.kitSetups, refused.kitSetups)
            Assert.Equal<Setups.Setup option>(placed.activeSetupOpt, refused.activeSetupOpt)
            let unknown = UnknownSetupErr $"'{name.value}'"
            Assert.Equal<ControlCaption option>(Some (ControlCaption $"Load failed — {unknown.describe}"), refused.kitStatusCaption)
        finally
            initial.kit.close () |> SceneTestFixture.kitSuccess

    /// What a new setup starts from (spec 0051 `.manual/007` D14).
    let private sourceAndDetector : Placement.CatalogueKind list = [ Placement.LightSource; Placement.Detector ]

    /// The kit window's own view over `seed`, re-rendered from every message, with the model its last message left and a
    /// way to send a message as the window's controls do (spec 0051 `.manual/007` 2.1).
    let private mountKitWindow (seed : TableAndElementRotationModel) : HostWindow * (unit -> TableAndElementRotationModel) * (TableAndElementRotationMsg -> unit) * ResizeArray<System.Exception> =
        let mutable current = seed
        let mutable dispatchOpt : (TableAndElementRotationMsg -> unit) option = None
        let renderErrors = ResizeArray<System.Exception>()
        let reduce (message : TableAndElementRotationMsg) (model : TableAndElementRotationModel) : TableAndElementRotationModel =
            current <- updateTableAndElementRotation message model
            current
        let view (model : TableAndElementRotationModel) (dispatch : TableAndElementRotationMsg -> unit) : Avalonia.FuncUI.Types.IView =
            dispatchOpt <- Some dispatch
            KitWindowView.kitWindowView model dispatch
        let window = HostWindow(Width = 1000.0, Height = 980.0)
        Program.mkSimple (fun () -> seed) reduce view
        |> Program.withHost window
        |> Program.withErrorHandler (fun (_, error) -> renderErrors.Add error)
        |> Program.run
        window.Show()
        Dispatcher.UIThread.RunJobs()
        let send (message : TableAndElementRotationMsg) : unit =
            match dispatchOpt with
            | Some dispatch ->
                dispatch message
                Dispatcher.UIThread.RunJobs()
            | None -> Assert.Fail("The view must have been given its dispatch.")
        window, (fun () -> current), send, renderErrors

    [<Fact>]
    let ``Save over a setup an experiment uses keeps the earlier revision in the setup's kit file`` () : unit =
        // Spec 0051 `.manual/007` 5.2–5.4, D14: New setup starts from a light source and a detector; Create writes it;
        // an experiment over it, then Edit and Save write the next revision and keep the earlier one in the file.
        HeadlessSession.run (fun () ->
            let stores = TestAppScope.freshAppContext owner
            let folder = TestAppScope.freshKitFolder owner
            let initial = openSetupKit folder stores
            let name = SetupName "Bench"
            try
                let window, current, send, renderErrors = mountKitWindow (initial |> updateTableAndElementRotation (ShowFlowStep SetupsFlowStep))
                let created, saved =
                    try
                        clickSetupControl ConstructorUiId.newSetupButton window
                        Assert.Equal<PageMode>(CreatingPageEntry, (current ()).pageMode)
                        Assert.Equal<Placement.CatalogueKind list>(sourceAndDetector, (current ()).elements |> List.map (fun slot -> slot.placement.catalogueKind))
                        send (SetSetupName name.caption)
                        send (AddElement Placement.LinearPolarizer)
                        clickSetupControl ConstructorUiId.pageSaveButton window
                        let created =
                            match (current ()).activeSetupOpt with
                            | Some setup -> setup
                            | None -> failwith "Create must make the new setup the chosen one."
                        Assert.Equal<PageMode>(BrowsingPage, (current ()).pageMode)
                        Assert.Equal<SetupName>(name, created.name)
                        Assert.Equal<SetupRevision>(SetupRevision.first, created.revision)

                        // The Experiments page's New experiment, over the chosen setup, varies the polarizer.
                        let polarizer = (current ()).elements |> List.find (fun element -> element.placement.catalogueKind = Placement.LinearPolarizer)
                        clickSetupControl ConstructorUiId.flowExperimentsStep window
                        clickSetupControl ConstructorUiId.newExperimentButton window
                        send (ExpChooseElement polarizer.id)
                        clickSetupControl ConstructorUiId.pageSaveButton window
                        let e1 = Assert.Single((current ()).experimentCollection.experiments)
                        Assert.Equal<SetupReference>(created.reference, e1.setup)

                        // Back on the Setups page, Edit opens the chosen setup's table; Save writes its next revision.
                        clickSetupControl ConstructorUiId.flowSetupsStep window
                        clickSetupControl ConstructorUiId.pageEditButton window
                        Assert.Equal<PageMode>(EditingPageEntry, (current ()).pageMode)
                        send (SelectTableElement polarizer.id)
                        send (RotSetAxis (RotationControls.R1, BarDegrees 30.0))
                        Assert.NotEqual<Setups.SetupSlot list>(created.slots, (current ()).recordedSlots)
                        let edited = (current ()).recordedSlots
                        clickSetupControl ConstructorUiId.pageSaveButton window
                        Assert.Empty(renderErrors)
                        Assert.Equal<PageMode>(BrowsingPage, (current ()).pageMode)
                        Assert.Equal<ControlCaption option>(Some (ControlCaption "Saved setup 'Bench', revision 2."), (current ()).kitStatusCaption)
                        created, edited
                    finally
                        window.Close()

                Assert.Equal<string list>([ "Bench.setup-json" ], setupFilesIn folder)
                let bench = Assert.Single(initial.kit.proxy.readSetups () |> SceneTestFixture.kitSuccess)
                Assert.Equal<SetupName>(name, bench.name)
                Assert.Equal<SetupRevision>(SetupRevision 2, bench.revision)
                Assert.Equal<Setups.SetupSlot list>(saved, bench.slots)
                let earlier : Setups.SetupRevisionRecord =
                    {
                        revision = SetupRevision.first
                        slots = created.slots
                    }
                Assert.Equal<Setups.SetupRevisionRecord list>([ earlier ], bench.history)
                let stored = Assert.Single(initial.kit.proxy.readExperiments () |> SceneTestFixture.kitSuccess)
                Assert.Equal<SetupReference>(created.reference, stored.setup)
            finally
                initial.kit.close () |> SceneTestFixture.kitSuccess)

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the Setups page lists the kit's setups, New setup names one in place, and Create writes it`` () : unit =
        HeadlessSession.run (fun () ->
            let stores = TestAppScope.freshAppContext owner
            let initial = TestAppScope.openedKitIn owner stores.catalogueImports |> setupModelInKit stores
            try
                for mode, name in [ ForwardConstructor, SetupName "Bench: A"; InverseConstructor, SetupName "Bench: B" ] do
                    // Spec 0051 `.manual/007` 5.1: a kit with no setup says so under New setup.
                    let window, current, send, renderErrors =
                        mountKitWindow ({ initial with constructorMode = mode } |> updateTableAndElementRotation (ShowFlowStep SetupsFlowStep))
                    try
                        Assert.Empty((current ()).kitSetups)
                        UiDriver.ById.assertAbsent window ConstructorUiId.savedSetups
                        clickSetupControl ConstructorUiId.newSetupButton window
                        let nameField = findSetupControl ConstructorUiId.setupNameField window :?> TextBox
                        Assert.Equal("Setup name field", AutomationProperties.GetName nameField)
                        nameField.BringIntoView() |> ignore
                        Assert.True(nameField.Focus())
                        Dispatcher.UIThread.RunJobs()
                        window.KeyTextInput(name.value)
                        Dispatcher.UIThread.RunJobs()
                        Assert.Equal(name.value, nameField.Text)
                        // The name is the draft's as it is typed, so Create is offered at once.
                        Assert.Equal(name.caption, (current ()).setupName)
                        clickSetupControl ConstructorUiId.pageSaveButton window
                        Assert.Empty(renderErrors)
                        let expectedStatus = ControlCaption $"Created setup '{name.value}'."
                        Assert.Equal<ControlCaption option>(Some expectedStatus, (current ()).kitStatusCaption)
                        let persisted = (current ()).kit.proxy.readSetups () |> SceneTestFixture.kitSuccess
                        let createdSetup = persisted |> List.find (fun setup -> setup.name = name)
                        Assert.Equal<Placement.CatalogueKind list>(sourceAndDetector, createdSetup.slots |> List.map (fun slot -> slot.placement.catalogueKind))
                        let savedSetups = findSetupControl ConstructorUiId.savedSetups window
                        Assert.Equal("Saved setups", AutomationProperties.GetName savedSetups)
                        let savedButton = findSetupControl (ConstructorUiId.savedSetup name) window
                        Assert.Equal(name.value, AutomationProperties.GetName savedButton)
                        // A press on the chosen setup unchooses it; a second chooses it again.
                        clickSetupControl (ConstructorUiId.savedSetup name) window
                        Assert.Equal<Setups.Setup option>(None, (current ()).activeSetupOpt)
                        clickSetupControl (ConstructorUiId.savedSetup name) window
                        Assert.Equal(Some name, (current ()).activeSetupOpt |> Option.map (fun setup -> setup.name))
                        // Spec 0051 `.manual/007` 5.1, `006-3`: Edit and Copy… sit on the setup's line, its read-only table
                        // under it.
                        let line = findSetupControl ConstructorUiId.setupLineText window
                        let topOf (control : Control) : float =
                            match control.TranslatePoint(Point(0.0, 0.0), window) |> Option.ofNullable with
                            | Some point -> point.Y
                            | None -> nan
                        let middleOf (control : Control) : float = topOf control + control.Bounds.Height / 2.0
                        for id in [ ConstructorUiId.pageEditButton; ConstructorUiId.copySetupButton ] do
                            Assert.True(abs (middleOf (findSetupControl id window) - middleOf line) < 12.0, $"{id.value} must sit on the setup's line")
                        Assert.True(topOf (findSetupControl ConstructorUiId.tableElementCanvas window) >= topOf line + line.Bounds.Height, "the table lies under the line")
                    finally
                        window.Close()
            finally
                initial.kit.close () |> SceneTestFixture.kitSuccess)

    /// Spec 0051 `.manual/003` 2.1: the table is a setup's slots drawn, so writing it back over the setup it was drawn
    /// from changes nothing, whatever was active before; FsCheck picks the order the committed kits' setups are drawn in.
    [<Fact>]
    let ``activating any sequence of the committed kits' setups, then writing the table back, gives the active setup`` () =
        for kit in [ OpticsMuellerKit; AbsorbingBiaxialNoisyKit; MultilayerThinFilmKit; LangasiteOnSiliconKit ] do
            let session = TestAppScope.openCommittedReadOnly owner kit
            try
                let model = session.model
                let names = model.kitSetups |> List.map _.name |> List.distinct
                Assert.NotEmpty(names)
                let roundTrips (picks : int list) : bool =
                    let drawn =
                        picks
                        |> List.fold (fun held pick -> held |> updateTableAndElementRotation (ActivateSetup (List.item pick names))) model
                    match drawn.activeSetupOpt with
                    | Some active -> drawn.tableWrittenOver active = active
                    | None -> false
                let picks = Gen.choose (0, names.Length - 1) |> Gen.nonEmptyListOf
                Check.One(Config.QuickThrowOnFailure.WithMaxTest(40), Prop.forAll (Arb.fromGen picks) roundTrips)
            finally
                try session.opened.library.kit.close () |> TestAppScope.readKitResult KitFile.fileName
                finally TestAppScope.assertCommittedUnchanged kit session.recorded

