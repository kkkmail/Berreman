namespace OpticalConstructor.Ui.Tests

open Avalonia
open Avalonia.Automation
open Avalonia.Controls
open Avalonia.Headless
open Avalonia.Input
open Avalonia.Threading
open Avalonia.VisualTree
open Avalonia.FuncUI.Hosts
open Avalonia.FuncUI.Elmish
open Elmish
open Xunit
open Softellect.Sys.Primitives
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.LibraryBackup
open OpticalConstructor.Domain.CatalogueImport
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.Lifecycle
open OpticalConstructor.Storage.CatalogueImportStore
open OpticalConstructor.Storage.EntryFileJson
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.KitFolders
open OpticalConstructor.Domain.KitHistory
open OpticalConstructor.Domain.ImportProfiles
open OpticalConstructor.Domain.Studies
open OpticalConstructor.Domain.FitRuns
open OpticalConstructor.Domain.ItemModels
open OpticalConstructor.Domain.StudyPublishing
open OpticalConstructor.Storage.KitContentFiles
open OpticalConstructor.Optimization.StudyEngine
open OpticalConstructor.Controls
open OpticalConstructor.Tests
open OpticalConstructor.Ui
open OpticalConstructor.Ui.ImportDialogView
open OpticalConstructor.Ui.ImportWizardView
open OpticalConstructor.Ui.KitArchiveJob
open OpticalConstructor.Ui.JobRunner
open OpticalConstructor.Ui.StudyFitJob
open OpticalConstructor.Ui.TableAndElementRotationView

// Spec 0050 S11.5: this file compiles before StudyViewTests, so both UI fixtures share one history constructor.
module internal StudyFitViewFixture =

    let firstFitViewRunStamp : ClockTime =
        ClockTime (System.DateTime(2026, 11, 14, 12, 0, 0, System.DateTimeKind.Utc))

    let fitViewRunOf (study : Study) (at : ClockTime) : FitRun =
        {
            schemaVersion = FitRun.currentSchemaVersion
            name = study.name.fitRunNameAt at
            study = study.name
            stamp = at
            mode = study.settings.mode
            stage = JointStage
            calibrationUse = CalibrationHeldFixed
            engine = NonlinearStudyEngine
            runs = study.experiments
            dataRead = []
            parameters = []
            entries = []
            members = []
            objectMatrices = []
            results =
                {
                    sigmas = []
                    problems = []
                    runResiduals = []
                    realizability = []
                    productChecks = []
                    profiles = []
                }
            notes = FitRunNotes.none
        }

// Spec 0050 S11.5: shared by the Publish reachability, read-only and interaction facts compiled after this file.
module internal StudyPublishViewFixture =

    let firstPublicationStamp : ClockTime =
        ClockTime (System.DateTime(2026, 11, 1, 12, 0, 0, System.DateTimeKind.Utc))

    let publicationItemEntry (item : Item) (name : ParameterName) (fitted : ParameterValue) : FitRunEntry =
        let parameter = item.tryFindParameter name |> Option.defaultWith (fun () -> failwith name.value)
        {
            parameter = ItemParameterRef (item.id, name)
            owner = ItemOwner item.id
            start = parameter.state.currentValue
            fitted = fitted
        }

    let publicationRunOf (model : TableAndElementRotationModel) (stamp : ClockTime) (entries : FitRunEntry list) : FitRun =
        let study = List.exactlyOne model.kitStudies
        let source = StudyFitViewFixture.fitViewRunOf study stamp
        let parameters : FitRunParameter list =
            (model.kitItems |> List.collect (fun item ->
                item.parameters |> List.map (fun parameter ->
                    {
                        parameter = ItemParameterRef (item.id, parameter.name)
                        state = parameter.state
                    })))
            @ (study.materials |> List.collect (fun material ->
                material.parameters |> List.map (fun parameter ->
                    {
                        parameter = MaterialParameterRef (material.version, parameter.name)
                        state = parameter.state
                    })))
        {
            source with
                parameters = parameters
                entries = entries
                results =
                    {
                        source.results with
                            sigmas = entries |> List.map (fun entry ->
                                {
                                    entry =
                                        {
                                            parameter = entry.parameter
                                            owner = entry.owner
                                        }
                                    sigmaOpt = Some (ParameterSigma 0.002)
                                })
                    }
        }

    let publicationModelWithSubjects (model : TableAndElementRotationModel) : TableAndElementRotationModel =
        let draft : ItemDraft =
            {
                id = ItemId (System.Guid.Parse "41800000-0000-4000-8000-000000000001")
                label = ItemLabel "QZ"
                model = LibraryModelChoice (BuiltinCatalogue.itemModel ItemModelIds.blackBoxMuellerModelId)
                dial = FixedMounting
            }
        let added = model |> TestAppScope.withStudyMaterial |> updateTableAndElementRotation (AddItem draft)
        Assert.Equal<ControlCaption option>(None, added.kitStatus)
        let material = List.exactlyOne (List.exactlyOne added.kitStudies).materials
        let n1 = ParameterName "n1"
        let materialParameter = material.parameters |> List.find (fun parameter -> parameter.name = n1)
        let materialEntry : FitRunEntry =
            {
                parameter = MaterialParameterRef (material.version, n1)
                owner = MaterialOwner material.version
                start = materialParameter.state.currentValue
                fitted = ParameterValue 1.65
            }
        let entries =
            [ ItemLabel "CPL1"; ItemLabel "CPL2"; draft.label ]
            |> List.map (fun label ->
                let item = added.kitItems |> List.find (fun item -> item.label = label)
                let name, fitted =
                    if label = draft.label then ParameterName "m00", ParameterValue 0.9
                    else ParameterName "c2.retardance", ParameterValue 1.56
                publicationItemEntry item name fitted)
        let stamp = ClockTime (firstPublicationStamp.value.AddHours 1.0)
        let run = publicationRunOf added stamp (entries @ [ materialEntry ])
        added.kit.proxy.writeFitRun run |> SceneTestFixture.kitSuccess
        added
        |> updateTableAndElementRotation ReloadKitChanges
        |> updateTableAndElementRotation (SelectFitRun run.name)
        |> updateTableAndElementRotation (ShowFlowStep PublishFlowStep)

/// Spec 0046 (operator): "if we put a tall control above another, the window must have enough scroll length
/// to accommodate both — a simple arithmetic that must hold", in every window. Proven as a LAW over each
/// window at two small-screen client sizes (800 × 600, and a 125–175 %-scaled laptop's 1000 × 540): every
/// control the window shows can be scrolled so that BOTH its top-left and its bottom-right corner are on
/// screen — nothing is ever clipped out of reach. Plus the two rules that make the law hold on a real
/// screen: a window never opens larger than its screen's working area (`WindowFit`), and a plain wheel over
/// the Main table scrolls the page until the table is fully in view (`wheelTarget`).
module ScrollExtentTests =

    /// The small-screen client sizes the law is checked at.
    let private smallScreens : (float * float) list = [ 800.0, 600.0; 1000.0, 540.0 ]

    let private labelOf (c : Control) : string =
        let text =
            match c with
            | :? TextBlock as t -> t.Text
            | _ -> ""
        $"%s{c.GetType().Name} id=%s{AutomationProperties.GetAutomationId c} name=%s{c.Name} text=%s{text}"

    /// The controls the law speaks about: every visible text, input, button, or named / AutomationId'd
    /// control of OURS (template parts excluded), except the absolutely-positioned drawing inside a canvas
    /// (the table schematic may lie beyond its canvas when zoomed — that is panning, not scrolling) and the
    /// plot's own internals.
    let private subjects (window : Window) : Control list =
        window.GetVisualDescendants()
        |> Seq.choose (function :? Control as c -> Some c | _ -> None)
        |> Seq.filter (fun c ->
            c.IsEffectivelyVisible
            && c.Bounds.Width > 1.0 && c.Bounds.Height > 1.0
            && isNull c.TemplatedParent
            && not (c.GetVisualAncestors() |> Seq.exists (fun a -> a :? Canvas || a.GetType().Name = "AvaPlot"))
            && (match c with
                | :? TextBlock as t -> not (System.String.IsNullOrEmpty t.Text)
                | :? TextBox | :? Button | :? CheckBox | :? ComboBox -> true
                | _ -> not (System.String.IsNullOrEmpty (AutomationProperties.GetAutomationId c)) || not (System.String.IsNullOrEmpty c.Name)))
        |> List.ofSeq

    /// The point `p` of control `c` is on screen: inside the window's client area AND inside every clipping
    /// ancestor (a scroll viewer's viewport clips).
    let private onScreen (window : Window) (c : Control) (p : Point) : bool =
        let within (width : float) (height : float) (q : Point) : bool =
            q.X >= -1.0 && q.Y >= -1.0 && q.X <= width + 1.0 && q.Y <= height + 1.0
        let insideOf (v : Visual) : bool =
            match c.TranslatePoint(p, v) |> Option.ofNullable with
            | Some q -> within v.Bounds.Width v.Bounds.Height q
            | None -> false
        (match c.TranslatePoint(p, window) |> Option.ofNullable with
         | Some q -> within window.ClientSize.Width window.ClientSize.Height q
         | None -> false)
        && (c.GetVisualAncestors() |> Seq.filter (fun a -> a.ClipToBounds) |> Seq.forall insideOf)

    /// Every subject corner that cannot be scrolled on screen, brought into view through every enclosing viewer. A
    /// presenter that scrolls marks the request handled, so a viewer nested in another takes one request per level
    /// (spec 0050 S7.2: the Items panel's offer list scrolls inside the panel).
    let private unreachable (window : Window) : string list =
        subjects window
        |> List.collect (fun c ->
            let w, h = c.Bounds.Width, c.Bounds.Height
            let viewers = c.GetVisualAncestors() |> Seq.filter (fun a -> a :? ScrollViewer) |> Seq.length
            [ Point(0.5, 0.5), "top-left"; Point(w - 0.5, h - 0.5), "bottom-right" ]
            |> List.choose (fun (p, corner) ->
                for _ in 1 .. max 1 viewers do
                    c.BringIntoView(Rect(p.X - 0.5, p.Y - 0.5, 1.0, 1.0))
                    Dispatcher.UIThread.RunJobs()
                match onScreen window c p with
                | true -> None
                | false -> Some $"%s{corner} of %s{labelOf c} %A{c.Bounds}"))

    /// Assert the law for a freshly built window at every small-screen size.
    let private assertReachable (name : string) (makeWindow : unit -> Window) : unit =
        HeadlessSession.run (fun () ->
            for w, h in smallScreens do
                let window = makeWindow ()
                window.Width <- w
                window.Height <- h
                window.Show()
                Dispatcher.UIThread.RunJobs()
                let found = try unreachable window finally window.Close()
                let firstFew = found |> List.truncate 8 |> String.concat "; "
                Assert.True(List.isEmpty found, $"%s{name} at %g{w}×%g{h}: %d{List.length found} unreachable — %s{firstFew}"))

    let private stores () : MaterialProxy * SampleProxy * CategoryProxy * LibraryProxy =
        let samples = BuiltinCatalogue.sampleStore ()
        let materials = BuiltinCatalogue.materialStore (samplesReferencing samples)
        let categories = BuiltinCatalogue.categoryStore (materialsReferencingCategory materials)
        materials, samples, categories, Library.liveLibrary (BuiltinCatalogue.library ()) samples (SourceProxy.createInMemory ())

    /// Hosts `view` over `model0` in a FuncUI window: the workbench's `mainView`, or the kit window over it.
    let private viewWindow
        (view : TableAndElementRotationModel -> (TableAndElementRotationMsg -> unit) -> Avalonia.FuncUI.Types.IView)
        (model0 : TableAndElementRotationModel)
        : Window =
        let window = HostWindow()
        Program.mkSimple (fun () -> model0) updateTableAndElementRotation view |> Program.withHost window |> Program.run
        window :> Window

    let private constructorWindow (model0 : TableAndElementRotationModel) : Window = viewWindow mainView model0

    /// Main with a polarizer whose R1 run is collected AND being edited — the tallest Experiments bay (editor,
    /// chart, description, collection row, export, collection builder) above the table.
    let private filledExperiments () : TableAndElementRotationModel =
        let m = SceneTestFixture.mainScene () |> updateTableAndElementRotation (AddElement Placement.LinearPolarizer)
        let pid = (List.item 2 m.elements).id
        m |> updateTableAndElementRotation (ExpChooseElement pid) |> updateTableAndElementRotation ExpCommit |> updateTableAndElementRotation (ExpChooseElement pid) |> updateTableAndElementRotation (SelectBay BayNames.experiments)

    // ------------------------------ the law, window by window ------------------------------

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``Main: every bay leaves the whole page reachable — the table below a tall bay included`` () =
        for bay in BayNames.allBayNames do
            assertReachable $"Main/%s{bay.value}" (fun () -> constructorWindow ({ SceneTestFixture.mainScene () with selection = ElementSelected (SceneElementIndex 0) } |> updateTableAndElementRotation (SelectBay bay)))
        assertReachable "Main/Experiments (collected + chart)" (fun () -> constructorWindow (filledExperiments ()))

    /// Spec 0050 S8.4: the grid panel docks between the slot bar and the table, so the page grows by it.
    [<Fact>]
    let ``the Main page with the setup grid open leaves everything reachable`` () : unit =
        assertReachable "MainSetupGrid" (fun () ->
            let filled = filledExperiments ()
            let polarizer = (List.item 2 filled.elements).id
            let withAngle (model : TableAndElementRotationModel) (degrees : float) : TableAndElementRotationModel =
                model |> updateTableAndElementRotation (AddSetupGridAngle (polarizer, BarDegrees degrees))
            constructorWindow ([ 0.0; 45.0; 90.0 ] |> List.fold withAngle (filled |> updateTableAndElementRotation OpenSetupGrid)))

    [<Fact>]
    let ``Inverse: the Experiments bay leaves everything reachable`` () =
        let materials, samples, categories, library = stores ()
        assertReachable "Inverse/Experiments" (fun () ->
            let collection = (filledExperiments ()).experimentCollection
            // Spec 0050 S9.1: E1 loaded, its axis held, so the role row, the session and notes fields and a scan line show.
            let loaded =
                { SceneTestFixture.inverseScene library (Experiments.createInMemoryExperiments ()) materials samples categories with
                    ribbon = BayNames.experiments
                    experimentCollection = collection }
                |> updateTableAndElementRotation (ExpEdit (List.head collection.experiments).id)
                |> updateTableAndElementRotation ExpAddScanAxis
            Assert.Equal(ControlOffered, loaded.experimentState.bookkeepingAvailability)
            Assert.Equal(7, List.length loaded.experimentState.roleOptions)
            Assert.Single(loaded.experimentState.scanLines) |> ignore
            constructorWindow loaded)

    [<Fact>]
    let ``Library and Materials windows: the facet rows wrap, nothing is clipped`` () =
        let materials, samples, categories, library = stores ()
        assertReachable "Library" (fun () -> LibraryWindow(library, samples, SourceProxy.createInMemory (), materials, categories, LibraryBackupProxy.createInMemory (), CatalogueImportContext.create (CatalogueImportProxy.createInMemory ()).proxy))
        assertReachable "Materials" (fun () -> MaterialsWindow(materials, categories, CatalogueImportContext.create (CatalogueImportProxy.createInMemory ()).proxy))

    [<Fact>]
    let ``the import dialog leaves everything reachable`` () : unit =
        let envelopes = (BuiltinCatalogue.catalogueData ()).envelopes
        let decisions =
            envelopes |> List.map (fun envelope ->
                let origin = DroppedFileOrigin envelope.content.fileName
                ImportDecision.decide origin envelope None [])
        let material = envelopes |> List.find (fun envelope -> envelope.content.schema.kind = MaterialEntryKind)
        let collisionContent =
            {
                material.content with
                    id = LibraryEntryId (System.Guid.Parse "31801b01-444f-43c8-8440-000000000108")
                    provenance = UserProvenance
            }
        let collision = collisionContent.``sealed`` |> Result.defaultWith (fun error -> failwith error.describe)
        let firstHeld : HeldEntry =
            {
                id = material.content.id
                kind = material.content.schema.kind
                name = material.content.name
                latest = material.content.version
                lifecycle = ActiveEntry
                provenance = material.content.provenance
                versions =
                    [
                        {
                            version = material.content.version
                            contentHash = material.contentHash
                        }
                    ]
            }
        let secondHeld =
            {
                firstHeld with
                    id = LibraryEntryId (System.Guid.Parse "31801b01-444f-43c8-8440-000000000208")
                    provenance = UserProvenance
            }
        let collisionFile = FileName "collision.material-json"
        let collisionOrigin = DroppedFileOrigin collisionFile
        let collisionDecision = ImportDecision.decide collisionOrigin collision None [ firstHeld; secondHeld ]
        let request = ApplyDroppedEntries (decisions @ [ collisionDecision ])
        assertReachable "ImportDialog" (fun () ->
            let imports = CatalogueImportContext.create (CatalogueImportProxy.createInMemory ()).proxy
            ImportDialogWindow(imports, request, ignore))

    /// A picked lab file of twelve comma columns and forty rows, read in memory, so the wizard's preview is wider and
    /// taller than the screen (spec 0050 S9.2).
    let private wideLabFileRequest () : ImportWizardRequest =
        let columns = [ 1 .. 12 ]
        let header = columns |> List.map (fun column -> $"reading_%02d{column}") |> String.concat ","
        let rowOf (row : int) : string = columns |> List.map (fun column -> $"{row * 100 + column}.25") |> String.concat ","
        let text = (header :: ([ 1 .. 40 ] |> List.map rowOf) |> String.concat "\n") + "\n"
        let content = FileByteContent (System.Text.Encoding.UTF8.GetBytes text)
        let readPickedFile (picked : FileName) : Result<LabFileSource, KitError> =
            let source : LabFileSource = { file = picked; content = content }
            Ok source
        {
            target = AttachWizardTarget (Experiments.ExperimentId 1)
            file = FileName "C:/Lab/ScrollExtentTests/twelve-columns.csv"
            readPickedFile = readPickedFile
            kitProfiles = []
            defaultProfile = ImportProfileName.intensitySeries
        }

    [<Fact>]
    let ``the import wizard leaves everything reachable`` () : unit =
        let request = wideLabFileRequest ()
        assertReachable "ImportWizard" (fun () -> ImportWizardWindow(request, ignore))
        assertReachable "ImportWizard (mapping)" (fun () ->
            let context : ImportWizardContext =
                {
                    request = request
                    onApplied = ignore
                    requestClose = ignore
                }
            let scanColumn : ColumnChoice =
                {
                    column = DataColumnNumber 12
                    role = ScanColumnRole (DegreeColumnUnit, PhysicalScanValue)
                }
            let mapping =
                context.initImportWizard
                |> updateImportWizard StartColumnMapping
                |> updateImportWizard (SetColumnRole (scanColumn.column, scanColumn.role))
            Assert.Equal(12, List.length mapping.columns)
            Assert.Equal<ColumnChoice option>(Some scanColumn, List.tryLast mapping.columns)
            let window = HostWindow()
            Program.mkSimple (fun () -> mapping) updateImportWizard importWizardView |> Program.withHost window |> Program.run
            window :> Window)

    [<Fact>]
    let ``Material, Sample, Category and Source editors leave everything reachable`` () =
        let materials, samples, categories, _ = stores ()
        assertReachable "MaterialEditor" (fun () -> MaterialEditorWindow(materials, MaterialEditorView.NewMaterial (newMaterialId ()), BuiltinCatalogue.categoryStore (fun _ -> [])))
        assertReachable "SampleEditor" (fun () -> SampleEditorWindow(materials, samples, categories, CatalogueImportContext.create (CatalogueImportProxy.createInMemory ()).proxy, SampleEditorView.NewSeededMultilayer (newSampleId ())))
        assertReachable "CategoryEditor" (fun () -> CategoryEditorWindow(categories))
        assertReachable "SourceEditor" (fun () -> LightSourceEditorWindow(SourceProxy.createInMemory (), LightSourceEditorView.NewSource (Placement.ElementValueId "src-x")))

    [<Fact>]
    let ``the chart window leaves everything reachable`` () =
        let chart : ExperimentChart.ExperimentChart =
            {
                series =
                    [
                        {
                            ExperimentControls.ChartSeries.name = ControlCaption "Ψ"
                            points = [ { ExperimentControls.ChartPoint.x = 0.0; y = 10.0 }; { x = 1.0; y = 12.0 } ]
                        }
                    ]
                xLabel = ControlCaption "x"
                yLabel = ControlCaption "y"
                title = ControlCaption "t"
                description = ControlCaption "d"
                xAxisKind = ExperimentChart.AngularXAxis
            }
        assertReachable "Chart" (fun () -> ChartWindow(chart))

    /// Forty kits in memory, every fifth folder missing and every seventh kit starred, so the start screen's list is
    /// longer than the screen (spec 0050 S5.2).
    let private fortyKitHistory () : KitHistoryProxy =
        let firstOpen = System.DateTime(2026, 11, 3, 12, 0, 0, System.DateTimeKind.Utc)
        let folderOf (index : int) : KitFolder = KitFolder (FolderName $"C:/kits/bench-%02d{index}")
        let entryOf (index : int) : KitHistoryEntry =
            {
                folder = folderOf index
                kitId = KitId (System.Guid.NewGuid ())
                name = KitName $"Bench kit %02d{index}"
                lastOpened = ClockTime (firstOpen.AddMinutes (float index))
                star =
                    match index % 7 with
                    | 0 -> StarredKit
                    | _ -> UnstarredKit
            }
        let indices = [ 1 .. 40 ]
        let present = indices |> List.filter (fun index -> index % 5 <> 0) |> List.map folderOf
        let store = KitHistoryProxy.createInMemory present
        store.writeHistory { KitHistory.empty with entries = indices |> List.map entryOf }
        |> Result.defaultWith (fun error -> failwith error.describe)
        store

    [<Fact>]
    let ``the kit start screen leaves everything reachable`` () : unit =
        let owner = FolderName "ScrollExtentTests"
        let context = { TestAppScope.freshAppContext owner with kitHistory = fortyKitHistory () }
        let picker = TestAppScope.kitPickerOf (TestAppScope.freshKitFolder owner)
        assertReachable "KitStart" (fun () ->
            KitStartWindow(context, ForwardConstructor, (fun _ -> picker), (fun _ -> KitArchive.KitZipPicker.none), (fun _ _ -> ())))

    /// The tallest Experiments bay under the kit bar at its tallest: a read-only kit with a folder, so its caption is
    /// longest and Show in Explorer is offered, a running send, an open copy source of forty experiments, all chosen,
    /// whose twenty items each have a row offering Add and the kit's three items, three files offered for reload and a
    /// status (spec 0050 S5.3, S6, S7.2).
    let private tallestKitBar () : TableAndElementRotationModel =
        let filled = filledExperiments ()
        let readOnlyKit =
            {
                filled.kit with access = ReadOnlyKitAccess
            }
        let scratch = KitOpening.scratchKit () |> SceneTestFixture.kitSuccess
        let sourceKit : KitContext =
            {
                scratch with
                    kitFile =
                        {
                            scratch.kitFile with name = KitName "a bench kit to copy experiments from, with a long name"
                        }
                    access = ReadOnlyKitAccess
            }
        let collected = List.head filled.experimentCollection.experiments
        let laser600 : ItemModelChoice =
            SourcePresetChoice (BuiltinCatalogue.itemModel ItemModels.ItemModelIds.sourcePresetItemModelId, BuiltinCatalogue.source PresetIds.source600)
        let pieceOf (n : int) (label : string) : Item =
            let draft : ItemDraft =
                {
                    id = ItemId (System.Guid.Parse $"22900000-0000-4000-8000-%012d{n}")
                    label = ItemLabel label
                    model = laser600
                    dial = FixedMounting
                }
            draft.toItem
        let kitItems = [ 1 .. 3 ] |> List.map (fun n -> pieceOf (100 + n) $"Bench laser {n}")
        let sourceItems = [ 1 .. 20 ] |> List.map (fun n -> pieceOf n $"Laser600 piece {n}")
        let rowOf (index : int) (item : Item) : ItemCopyRow =
            {
                sourceItem = item
                choice =
                    match index % 2 with
                    | 0 -> AddCopiedItem
                    | _ -> MapOntoKitItem (List.item (index % 3) kitItems).id
            }
        let experiments =
            [ 1 .. 40 ]
            |> List.map (fun n ->
                {
                    collected with
                        id = Experiments.ExperimentId n
                        name = (Experiments.ExperimentId n).defaultName
                })
        let copySource : KitCopySource =
            {
                source = sourceKit
                experiments = experiments
                chosen = experiments |> List.map (fun experiment -> experiment.id)
                items = sourceItems
                setups = filled.kitSetups
                itemRows = sourceItems |> List.mapi rowOf
            }
        let handle : KitArchiveJobHandle =
            {
                cancelArchiveJob = ignore
            }
        let progress : KitArchive.KitArchiveProgress =
            {
                filesDone = KitArchive.KitArchiveFileCount 2
                filesTotal = KitArchive.KitArchiveFileCount 5
            }
        {
            filled with
                kit = readOnlyKit
                kitFolderOpt = Some (KitFolder (FolderName "C:/kits/ScrollExtentTests/a bench kit with a long folder name"))
                kitSend = (filled.kitSend.started handle).progressed (progress.jobProgress (JobRunner.JobElapsed System.TimeSpan.Zero))
                kitCopyOpt = Some copySource
                kitItems = kitItems
                kitChanges =[ FileName ".kit-json"; FileName "E1.experiment-json"; FileName "E2.experiment-json" ]
                kitStatus = Some (ControlCaption "The kit is open read-only: nothing is saved.")
        }

    [<Fact>]
    let ``the kit window: the kit bar and the page stay reachable`` () : unit =
        assertReachable "KitWindow" (fun () -> viewWindow KitWindowView.kitWindowView (tallestKitBar ()))

    /// The kit window on its Items step: three circular polarizers added through `AddItem`, the second selected with
    /// its first parameter's editor open as unknown, and the add form open (spec 0050 S7.2).
    let private itemsStepKitWindow () : Window =
        let update (msg : TableAndElementRotationMsg) (model : TableAndElementRotationModel) : TableAndElementRotationModel =
            updateTableAndElementRotation msg model
        let circular : ItemModelChoice =
            LibraryModelChoice (BuiltinCatalogue.itemModel ItemModels.ItemModelIds.circularPolarizerModelId)
        let draftOf (n : int) : ItemDraft =
            {
                id = ItemId (System.Guid.Parse $"24200000-0000-4000-8000-%012d{n}")
                label = ItemLabel $"CPL{n}"
                model = circular
                dial = FixedMounting
            }
        let added = [ 1 .. 3 ] |> List.fold (fun model n -> model |> update (AddItem (draftOf n))) (initMain () |> SceneTestFixture.kitSuccess)
        let second = (draftOf 2).id
        let selected = added |> update (SelectKitItem second)
        let firstParameter =
            match selected.kitItems |> List.filter (fun item -> item.id = second) |> List.collect _.parameters with
            | parameter :: _ -> parameter
            | [] -> failwith "the second item must be in the kit and hold a parameter"
        let model =
            selected
            |> update (EditItemParameter firstParameter.name)
            |> update (SetParameterEditChoice UnknownStateChoice)
            |> update OpenAddItemForm
            |> update (ShowFlowStep ItemsFlowStep)
        Assert.Equal(3, List.length model.kitItems)
        Assert.Equal<ItemId option>(Some second, model.selectedItemOpt)
        match model.parameterEditOpt, model.addItemFormOpt with
        | Some draft, Some _ -> Assert.Equal<ParameterStateChoice>(UnknownStateChoice, draft.choice)
        | _ -> Assert.Fail "the parameter editor and the add form must both be open"
        viewWindow KitWindowView.kitWindowView model

    [<Fact>]
    let ``the kit window's Items step leaves everything reachable`` () : unit =
        assertReachable "KitWindowItems" itemsStepKitWindow

    /// The study grids and open editor remain reachable at both small screen sizes (spec 0050 S11.5; S12.1).
    [<Fact>]
    let ``the kit window's Unknowns step leaves everything reachable`` () : unit =
        let makeWindow () : Window =
            let owner = FolderName "ScrollExtentUnknowns"
            let model = TestAppScope.studyKitIn owner |> TestAppScope.withStudyMaterial
            let first = model.kitItems |> List.find (fun item -> item.label = ItemLabel "CPL1")
            let name = ItemModels.ParameterName "c2.retardance"
            let reference = ItemParameterRef (first.id, name)
            let edited =
                model
                |> updateTableAndElementRotation (ShowFlowStep UnknownsFlowStep)
                |> updateTableAndElementRotation (EditStudyParameter reference)
            match edited.parameterEditOpt with
            | Some draft -> Assert.Equal<StudyParameterRef>(reference, draft.parameter)
            | None -> Assert.Fail "The study parameter editor must be open"
            Assert.Equal(15, (List.exactlyOne (List.exactlyOne edited.kitStudies).materials).parameters.Length)
            let window = viewWindow KitWindowView.kitWindowView edited
            window.Opened.Add(fun _ ->
                Dispatcher.UIThread.RunJobs()
                let scrollSubjects = subjects window
                let grid =
                    Assert.Single<Control>(scrollSubjects, fun control ->
                        AutomationProperties.GetAutomationId control = ConstructorUiId.studySessionGrid.value)
                    |> Assert.IsType<Grid>
                let session = OpticalConstructor.Domain.Experiments.SessionName "Session 1"
                let gainId = ConstructorUiId.studySessionGainChoice session
                let gain =
                    Assert.Single<Control>(scrollSubjects, fun control ->
                        AutomationProperties.GetAutomationId control = gainId.value)
                    |> Assert.IsType<ComboBox>
                Assert.Contains<Control>(grid, subjects window)
                Assert.Contains<Control>(gain, subjects window))
            window.Closed.Add(fun _ -> model.kit.close () |> SceneTestFixture.kitSuccess)
            window
        assertReachable "KitWindowUnknowns" makeWindow

    /// The Fit step's run controls, live progress and history remain reachable on small screens (spec 0050 S11.5).
    [<Fact>]
    let ``the kit window's Fit step leaves everything reachable`` () : unit =
        let makeWindow () : Window =
            let owner = FolderName "ScrollExtentFit"
            let model = TestAppScope.studyKitIn owner
            let study = List.exactlyOne model.kitStudies
            for hour in [ 2.0; 0.0; 1.0 ] do
                let stamp = ClockTime (StudyFitViewFixture.firstFitViewRunStamp.value.AddHours hour)
                let run = StudyFitViewFixture.fitViewRunOf study stamp
                model.kit.proxy.writeFitRun run |> SceneTestFixture.kitSuccess
            let loaded = model |> updateTableAndElementRotation ReloadKitChanges
            Assert.Equal<ControlCaption option>(None, loaded.kitStatus)
            Assert.Equal(3, loaded.studyFitRuns.Length)
            let mutable started = false
            let host : StudyFitJobHost =
                {
                    startStudyFit = fun _ ->
                        started <- true
                        {
                            cancelStudyFit = ignore
                        }
                }
            let ready =
                {
                    loaded with
                        kitShell =
                            {
                                loaded.kitShell with studyFits = host
                            }
                }
            let request : StudyFitRequest =
                {
                    study = study.name
                    stage = JointStage
                    calibrationUse = ready.studySurface.calibrationUse
                }
            let progress : JobProgress =
                {
                    completed = JobStepCount 12
                    total = None
                    elapsed = JobElapsed (System.TimeSpan.FromSeconds 2.0)
                }
            let running =
                ready
                |> updateTableAndElementRotation (ShowFlowStep FitFlowStep)
                |> updateTableAndElementRotation (StudyFit (RunFit request))
                |> updateTableAndElementRotation (StudyFit (StudyFitProgressed progress))
            Assert.True started
            Assert.True running.studyFit.job.isRunning
            Assert.Equal<JobProgress option>(Some progress, running.studyFit.job.progress)
            let window = viewWindow KitWindowView.kitWindowView running
            window.Closed.Add(fun _ -> model.kit.close () |> SceneTestFixture.kitSuccess)
            window
        assertReachable "KitWindowFit" makeWindow

    /// A full Mueller correlation table remains reachable at both small screen sizes (spec 0050 S11.5).
    [<Fact>]
    let ``the kit window's Results step leaves everything reachable`` () : unit =
        let makeWindow () : Window =
            let owner = FolderName "ScrollExtentResults"
            let model = TestAppScope.studyKitIn owner
            let study = List.exactlyOne model.kitStudies
            let source = StudyFitViewFixture.fitViewRunOf study StudyFitViewFixture.firstFitViewRunStamp
            let problem : FitProblemQuality =
                {
                    subject = StudyEntriesProblem
                    coordinates =
                        [ for row in 0 .. 3 do
                            for column in 0 .. 3 do
                                let rowIndex = MuellerReconstruction.MuellerRowIndex row
                                let columnIndex = MuellerReconstruction.MuellerColumnIndex column
                                yield ElementCoordinate (rowIndex, columnIndex) ]
                    rank = MuellerReconstruction.MatrixRank 16
                    conditionOpt = Some (MuellerInverse.ConditionNumber 12.5)
                    columnNorms = JacobianColumnNorms (Array.create 16 1.0)
                    correlationOpt = Some (CorrelationMatrix (Array.init 16 (fun row ->
                        Array.init 16 (fun column -> if row = column then 1.0 else 0.125))))
                    chiSquared = FitChiSquared 0.5
                    rmse = MuellerReconstruction.ResidualRmse 0.01
                }
            let run =
                {
                    source with
                        results =
                            {
                                source.results with problems = [ problem ]
                            }
                }
            model.kit.proxy.writeFitRun run |> SceneTestFixture.kitSuccess
            let shown =
                model
                |> updateTableAndElementRotation ReloadKitChanges
                |> updateTableAndElementRotation (ShowFlowStep ResultsFlowStep)
                |> updateTableAndElementRotation (ShowResultsTab CorrelationResultsTab)
            Assert.Equal<ControlCaption option>(None, shown.kitStatus)
            Assert.Equal<FitRun option>(Some run, shown.selectedFitRunOpt)
            let window = viewWindow KitWindowView.kitWindowView shown
            window.Closed.Add(fun _ -> model.kit.close () |> SceneTestFixture.kitSuccess)
            window
        assertReachable "KitWindowResults" makeWindow

    [<Fact>]
    let ``the kit window's Publish step leaves everything reachable`` () : unit =
        let makeWindow () : Window =
            let owner = FolderName "ScrollExtentPublish"
            let model = TestAppScope.studyKitIn owner
            let shown = StudyPublishViewFixture.publicationModelWithSubjects model
            let study = List.exactlyOne shown.kitStudies
            let run = shown.selectedFitRunOpt |> Option.defaultWith (fun () -> failwith "The publication run must be selected")
            let calibrated = study.calibratedItemsWith run shown.kitItems |> Result.defaultWith (fun error -> failwith error.describe)
            Assert.Equal(3, calibrated.Length)
            let material = List.exactlyOne study.materials
            let blackBox = shown.kitItems |> List.find (fun item -> item.label = ItemLabel "QZ")
            Assert.Equal<PromotionSubject list>(
                [ MaterialPromotion material.version; BlackBoxPromotion blackBox.id ],
                run.promotionSubjectsWith study shown.kitItems shown.tryItemModelChoice)
            let window = viewWindow KitWindowView.kitWindowView shown
            window.Closed.Add(fun _ -> model.kit.close () |> SceneTestFixture.kitSuccess)
            window
        assertReachable "KitWindowPublish" makeWindow

    /// Spec 0050 S9.3: the Inverse kit window on its Data step over the matrix of three experiments, copies of the collected
    /// one of `filledExperiments` over its setup: the first reviewed, three points with one excluded and one setup
    /// correction, under a pending split of three groups.
    [<Fact>]
    let ``the kit window's Data step leaves everything reachable`` () : unit =
        let materials, samples, categories, library = stores ()
        let dataStepWindow () : Window =
            let filled = filledExperiments ()
            let collection = filled.experimentCollection
            let collected = List.head collection.experiments
            let attachment : DataAttachment =
                {
                    path = KitRelativePath "data/a lab file of the bench's long overnight run.csv"
                    contentHash = ContentHash "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"
                }
            let exclusion : PointExclusion =
                {
                    point = MappedPointNumber 2
                    reason = ExclusionReason "the beam was blocked by the iris while the polarizer turned"
                }
            let correction : Experiments.SetupCorrection =
                {
                    previous =
                        {
                            name = SetupName "A bench recorded before its polarizer was turned"
                            revision = SetupRevision.first
                        }
                    corrected = collected.setup
                    note = Experiments.CorrectionNote "the polarizer was at 30°, not at 0°, for the whole run"
                }
            let reviewed =
                {
                    collected with
                        dataOpt =
                            Some
                                {
                                    attachment = attachment
                                    profile = ImportProfileName.intensitySeries
                                    rows = AllDataRows
                                    exclusions = [ exclusion ]
                                }
                        corrections = [ correction ]
                }
            let copyOf (id : Experiments.ExperimentId) : Experiments.Experiment = { collected with id = id; name = id.defaultName }
            let experiments = [ reviewed; copyOf (Experiments.ExperimentId 2); copyOf (Experiments.ExperimentId 3) ]
            let reading (x : float) (intensity : float) : MeasuredData.IntensityPoint =
                {
                    x = MeasuredData.MeasuredAbscissa x
                    intensity = MeasuredData.MeasuredIntensity intensity
                }
            let points = SeriesPoints (MeasuredData.IntensityData { points = [ reading 0.0 1.0; reading 45.0 0.5; reading 90.0 0.0 ] })
            let groupOf (key : DataRowKey) (matching : DataSplits.SplitGroupMatch) : DataSplits.SplitGroup =
                {
                    key = key
                    points = MeasuredData.MeasuredPointCount 12
                    matching = matching
                }
            let offeredSetup : Setups.Setup =
                {
                    name = SetupName "A bench the kit lacks, offered with a long name"
                    revision = SetupRevision.first
                    slots = []
                    history = []
                }
            let offer : DataSplits.SplitSetupOffer =
                {
                    setup = offeredSetup
                    experiment = Experiments.ExperimentName "E4"
                }
            let split : DataSplitState =
                {
                    file = FileName "C:/Lab/ScrollExtentTests/a lab file holding many runs, split by its experiment column.csv"
                    profile = ImportProfileName.phoneRoiCsv
                    key = DataSplits.SplitKeyColumn "experiment"
                    split =
                        {
                            groups =
                                [
                                    groupOf (DataRowKey "E1") (DataSplits.ExperimentForGroup collected.name)
                                    groupOf (DataRowKey "K2") (DataSplits.SetupOfferForGroup offer)
                                    groupOf (DataRowKey "K3") (DataSplits.GroupWithoutMatch (DataSplits.SplitKeyTwiceErr "two groups read as one experiment"))
                                ]
                        }
                    creating = []
                }
            let model =
                { SceneTestFixture.inverseScene library (Experiments.createInMemoryExperiments ()) materials samples categories with
                    experimentCollection = { collection with experiments = experiments }
                    kitSetups = filled.kitSetups
                    kitItems = filled.kitItems
                    reviewedDataOpt = Some { experimentId = reviewed.id; points = Ok points }
                    dataSplitOpt = Some split }
                |> updateTableAndElementRotation (ShowFlowStep DataFlowStep)
            Assert.Equal<ConstructorFlowStep>(DataFlowStep, model.flowStep)
            Assert.NotEmpty(model.experimentCollection.experiments)
            let matrix = ExperimentMatrices.ExperimentMatrix.ofExperiments model.experimentCollection.experiments model.kitSetups model.kitItems
            Assert.Empty(matrix.unplaced)
            Assert.Equal(3, matrix.cells |> List.sumBy (fun cell -> cell.experiments.Length))
            viewWindow KitWindowView.kitWindowView model
        assertReachable "KitWindowData" dataStepWindow
        // The window shows what the law is held over: the matrix's three entries, the split and the corrected review.
        HeadlessSession.run (fun () ->
            let window = dataStepWindow ()
            window.Width <- 800.0
            window.Height <- 600.0
            window.Show()
            Dispatcher.UIThread.RunJobs()
            try
                let shown =
                    window.GetVisualDescendants()
                    |> Seq.choose (function :? Control as control -> Option.ofObj (AutomationProperties.GetAutomationId control) | _ -> None)
                    |> Set.ofSeq
                let expected =
                    [ ConstructorUiId.experimentMatrix; ConstructorUiId.dataSplitKeyColumn; ConstructorUiId.dataReviewPanel; ConstructorUiId.dataCorrectionTrail ]
                    @ ([ 1 .. 3 ] |> List.map (Experiments.ExperimentId >> ConstructorUiId.experimentMatrixEntry))
                    @ ([ 1 .. 3 ] |> List.map (MappedPointNumber >> ConstructorUiId.dataPointRow))
                    @ [ ConstructorUiId.dataIncludePointButton (MappedPointNumber 2) ]
                    @ ([ "E1"; "K2"; "K3" ] |> List.map (DataRowKey >> ConstructorUiId.dataSplitGroupRow))
                for id in expected do
                    Assert.True(Set.contains id.value shown, $"{id.value} must show in the Data step")
            finally
                window.Close())

    [<Fact>]
    let ``the Data locations window leaves everything reachable`` () : unit =
        assertReachable "DataLocations" (fun () ->
            DataLocationsWindow(TestAppScope.testLocations (FolderName "ScrollExtentTests"), ExplorerProxy.createInMemory ignore))

    // ------------------------------ a window never opens larger than its screen ------------------------------

    [<Fact>]
    let ``fitWithin shrinks a too-large window to the working area and moves it inside`` () =
        // A 1080p laptop at 150 %: 1920 × 1032 px of working area = 1280 × 688 DIP; the frame adds 16 × 39 DIP.
        let work = PixelRect(0, 0, 1920, 1032)
        let decoration = Size(16.0, 39.0)
        let fitted, at = WindowFit.fitWithin work (WindowFit.ScreenScaling 1.5) decoration (PixelPoint(300, 200)) (Size(1400.0, 1000.0))
        Assert.Equal(Size(1264.0, 649.0), fitted)
        Assert.Equal(PixelPoint(0, 0), at)
        // A window that fits keeps its size and its position.
        let small, same = WindowFit.fitWithin work (WindowFit.ScreenScaling 1.5) decoration (PixelPoint(300, 200)) (Size(620.0, 280.0))
        Assert.Equal(Size(620.0, 280.0), small)
        Assert.Equal(PixelPoint(300, 200), same)
        // One that fits but hangs off the bottom-right is moved back inside, unshrunk.
        let _, moved = WindowFit.fitWithin work (WindowFit.ScreenScaling 1.0) decoration (PixelPoint(1800, 1000)) (Size(620.0, 280.0))
        Assert.Equal(PixelPoint(1920 - 636, 1032 - 319), moved)
        // A second monitor's work area to the right is honoured (positions are virtual-screen pixels).
        let _, onSecond = WindowFit.fitWithin (PixelRect(1920, 0, 1280, 1024)) (WindowFit.ScreenScaling 1.0) decoration (PixelPoint(100, 100)) (Size(620.0, 280.0))
        Assert.Equal(PixelPoint(1920, 100), onSecond)

    [<Fact>]
    let ``a window opened larger than the screen is fitted to it`` () =
        HeadlessSession.run (fun () ->
            let window = Window(Width = 4000.0, Height = 3000.0)
            WindowFit.keepOnScreen window
            window.Show()
            Dispatcher.UIThread.RunJobs()
            try
                match Option.ofObj window.Screens |> Option.bind (fun s -> Option.ofObj s.Primary) with
                | Some screen ->
                    let area = screen.WorkingArea
                    Assert.True(window.ClientSize.Width <= float area.Width / screen.Scaling + 0.5, $"width %g{window.ClientSize.Width}")
                    Assert.True(window.ClientSize.Height <= float area.Height / screen.Scaling + 0.5, $"height %g{window.ClientSize.Height}")
                | None -> Assert.Fail "the headless host reports no screen"
            finally
                window.Close())

    // ------------------------------ the wheel over the Main table ------------------------------

    [<Fact>]
    let ``wheelTarget: the page scrolls first until the table is in view; a modifier is always the table's`` () =
        let page (top : float) (offset : float) : PageScroll option =
            Some { tableTop = PagePixels top; tableBottom = PagePixels (top + 560.0); viewportHeight = PagePixels 600.0; offset = PagePixels offset; maxOffset = PagePixels 400.0 }
        Assert.Equal(WheelToPage, wheelTarget Set.empty (page 458.0 0.0) (WheelNotches -1))          // table cut off, page can go down
        Assert.Equal(WheelToTable, wheelTarget Set.empty (page 20.0 400.0) (WheelNotches -1))        // table fully in view
        Assert.Equal(WheelToTable, wheelTarget Set.empty (page 458.0 0.0) (WheelNotches 1))          // cut off, but the page is at its top
        Assert.Equal(WheelToPage, wheelTarget Set.empty (page -200.0 300.0) (WheelNotches 1))        // cut off at the top, page can go up
        Assert.Equal(WheelToTable, wheelTarget (Set.ofList [ ModCtrl ]) (page 458.0 0.0) (WheelNotches -1))
        Assert.Equal(WheelToTable, wheelTarget (Set.ofList [ ModShift ]) (page 458.0 0.0) (WheelNotches -1))
        Assert.Equal(WheelToTable, wheelTarget Set.empty None (WheelNotches -1))
        // A table taller than the viewport is never fully in view: the page scrolls to its end, then the table zooms.
        let tall = Some { tableTop = PagePixels -40.0; tableBottom = PagePixels 520.0; viewportHeight = PagePixels 480.0; offset = PagePixels 600.0; maxOffset = PagePixels 600.0 }
        Assert.Equal(WheelToTable, wheelTarget Set.empty tall (WheelNotches -1))

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``wheeling down over the table scrolls the page until the whole table is visible, then zooms it`` () =
        HeadlessSession.run (fun () ->
            let model0 = filledExperiments ()
            let mutable latest = model0
            let window = HostWindow(Width = tableAndElementRotationCanvasWidth, Height = 600.0)
            Program.mkSimple (fun () -> model0) (fun msg m -> let next = updateTableAndElementRotation msg m in latest <- next; next) mainView
            |> Program.withHost window
            |> Program.run
            window.Show()
            Dispatcher.UIThread.RunJobs()
            try
                let viewer = window.GetVisualDescendants() |> Seq.pick (function :? ScrollViewer as s when s.Name = ConstructorUiId.tableElementScrollViewer.value -> Some s | _ -> None)
                let canvas = window.GetVisualDescendants() |> Seq.pick (function :? Canvas as c when c.Name = ConstructorUiId.tableElementCanvas.value -> Some c | _ -> None)
                let tableBottomInWindow () : float =
                    match canvas.TranslatePoint(Point(0.0, canvas.Bounds.Height), window) |> Option.ofNullable with
                    | Some p -> p.Y
                    | None -> infinity
                Assert.True(tableBottomInWindow () > window.ClientSize.Height + 1.0, "precondition: the tall Experiments bay pushes the table's bottom off the window")
                let zoom0 = latest.view.zoom.value
                // Wheel over the visible strip of the table, near the window's bottom edge, until it stops scrolling.
                let mutable notches = 0
                while tableBottomInWindow () > window.ClientSize.Height + 1.0 && notches < 200 do
                    window.MouseWheel(Point(tableAndElementRotationCanvasWidth / 2.0, window.ClientSize.Height - 10.0), Vector(0.0, -1.0), RawInputModifiers.None)
                    Dispatcher.UIThread.RunJobs()
                    notches <- notches + 1
                Assert.True(tableBottomInWindow () <= window.ClientSize.Height + 1.0, $"the wheel never brought the table's bottom into view (offset %g{viewer.Offset.Y} of %g{viewer.Extent.Height - viewer.Viewport.Height})")
                Assert.Equal(zoom0, latest.view.zoom.value)                                      // the table did not zoom meanwhile
                // Now the table is in view: the next notch zooms it.
                window.MouseWheel(Point(tableAndElementRotationCanvasWidth / 2.0, window.ClientSize.Height - 10.0), Vector(0.0, -1.0), RawInputModifiers.None)
                Dispatcher.UIThread.RunJobs()
                Assert.NotEqual(zoom0, latest.view.zoom.value)
            finally
                window.Close())

    // ------------------------------ Main / Inverse ribbon and Experiments bay (operator items) ------------------------------

    /// Spec 0050 S9.3: the Experiments bay's data-file section moved into the Inverse constructor's Data step.
    [<Fact>]
    let ``the Main window opens on Add; the Attach data file section belongs to the Inverse window's Data step alone`` () =
        let materials, samples, categories, library = stores ()
        Assert.Equal(BayNames.add, (SceneTestFixture.mainScene ()).ribbon)
        Assert.Equal(BayNames.add, (SceneTestFixture.inverseScene library (Experiments.createInMemoryExperiments ()) materials samples categories).ribbon)
        Assert.DoesNotContain(DataFlowStep, ConstructorFlowStep.stepsOf ForwardConstructor)
        HeadlessSession.run (fun () ->
            let textsOf (view : TableAndElementRotationModel -> (TableAndElementRotationMsg -> unit) -> Avalonia.FuncUI.Types.IView) (model0 : TableAndElementRotationModel) : string list =
                let window = viewWindow view model0
                window.Width <- 1000.0
                window.Height <- 1100.0
                window.Show()
                Dispatcher.UIThread.RunJobs()
                try window.GetVisualDescendants() |> Seq.choose (function :? TextBlock as t -> Option.ofObj t.Text | _ -> None) |> List.ofSeq
                finally window.Close()
            let texts (model0 : TableAndElementRotationModel) : string list = textsOf mainView model0
            let forward = texts (filledExperiments ())
            Assert.DoesNotContain("Attach data file…", forward)
            Assert.DoesNotContain("Data files:", forward)
            let inverseModel =
                let m = { SceneTestFixture.inverseScene library (Experiments.createInMemoryExperiments ()) materials samples categories with ribbon = BayNames.experiments }
                { m with experimentCollection = (filledExperiments ()).experimentCollection }
            let inverse = texts inverseModel
            Assert.DoesNotContain("Data files:", inverse)
            Assert.DoesNotContain("Attach data file…", inverse)
            let dataStep = textsOf DataStepView.dataStepView inverseModel
            Assert.Contains("Data files:", dataStep)
            Assert.Contains("Attach data file…", dataStep))
