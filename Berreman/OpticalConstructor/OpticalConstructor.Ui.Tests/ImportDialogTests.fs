namespace OpticalConstructor.Ui.Tests

open System
open System.IO
open Avalonia
open Avalonia.Automation
open Avalonia.Controls
open Avalonia.FuncUI.Elmish
open Avalonia.FuncUI.Hosts
open Avalonia.Headless
open Avalonia.Input
open Avalonia.Interactivity
open Avalonia.Media
open Avalonia.Platform.Storage
open Avalonia.Threading
open Avalonia.VisualTree
open Elmish
open Xunit
open Softellect.Sys.Primitives
open OpticalConstructor.Controls
open OpticalConstructor.Domain.CatalogueFiles
open OpticalConstructor.Domain.CatalogueImport
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.KitFolders
open OpticalConstructor.Domain.KitLibrary
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.Placement
open OpticalConstructor.Storage.CatalogueFolderStore
open OpticalConstructor.Storage.CatalogueImportStore
open OpticalConstructor.Storage.EntryFileJson
open OpticalConstructor.Storage.Errors
open OpticalConstructor.Storage.KitFolderStore
open OpticalConstructor.Tests
open OpticalConstructor.Ui
open OpticalConstructor.Ui.ImportDialogView

/// Spec 0050 S3.2, S4.2: import choices stay pending until Apply, including in the mounted MVU view.
module ImportDialogTests =

    type private DialogFixture =
        {
            folder : FolderName
            imports : CatalogueImportContext
            heldSilicon : LibraryEntryEnvelope
            newMaterial : LibraryEntryEnvelope
            collision : LibraryEntryEnvelope
            nextSilicon : LibraryEntryEnvelope
            decisions : ImportDecision list
            writes : ResizeArray<EntryVersionPut>
        }

    type private DialogObservation =
        {
            context : ImportDialogContext
            applied : ResizeArray<ImportDialogApplied>
            closes : ResizeArray<unit>
            events : ResizeArray<string>
        }

    let private importOk (result : Result<'Outcome, CatalogueImportError>) : 'Outcome =
        result |> Result.defaultWith (fun error -> failwith error.describe)

    let private storageOk (result : Result<'Outcome, StorageError>) : 'Outcome =
        result |> Result.defaultWith (fun error -> failwith error.describe)

    let private writeEntry (folder : FolderName) (file : FileName) (envelope : LibraryEntryEnvelope) : FileName =
        let path = file.combine folder
        Directory.CreateDirectory folder.value |> ignore
        let text = envelope.fileText |> storageOk
        File.WriteAllText(path.value, text.value)
        path

    let private freshDialog () : DialogFixture =
        let created = CatalogueImportContext.create (CatalogueImportProxy.createInMemory ()).proxy
        let importedAt = CatalogueImportTime (DateTime(2026, 11, 7, 12, 0, 0, DateTimeKind.Utc))
        let imports = { created with now = fun () -> importedAt }
        imports.importFrom (CatalogueProxy.createBuiltIn ()) |> importOk
        |> List.map _.byDefault |> imports.apply |> importOk |> ignore
        let heldSilicon =
            (BuiltinCatalogue.catalogueData ()).envelopes
            |> List.find (fun envelope -> envelope.content.id = MaterialIds.silicon.toLibraryEntryId)
        let newContent =
            {
                heldSilicon.content with
                    id = LibraryEntryId (Guid.NewGuid())
                    name = EntryName "Dialog material"
                    provenance = UserProvenance
            }
        let collisionContent =
            {
                newContent with
                    id = LibraryEntryId (Guid.NewGuid())
                    name = heldSilicon.content.name
            }
        let nextContent =
            {
                heldSilicon.content with
                    version = heldSilicon.content.version.next
                    descriptionOpt = Some (EntryDescription "Silicon with a dialog-import description")
            }
        let newMaterial = newContent.``sealed`` |> storageOk
        let collision = collisionContent.``sealed`` |> storageOk
        let nextSilicon = nextContent.``sealed`` |> storageOk
        let folder = FolderName (Path.Combine(AppContext.BaseDirectory, "ImportDialogTests", Guid.NewGuid().ToString("N")))
        let newFile = FileName "new.material-json"
        let collisionFile = FileName "collision.material-json"
        let nextFile = FileName "next.material-json"
        let files =
            [ writeEntry folder newFile newMaterial
              writeEntry folder collisionFile collision
              writeEntry folder nextFile nextSilicon ]
        let writes = ResizeArray<EntryVersionPut>()
        let observed =
            {
                imports with
                    proxy =
                        {
                            imports.proxy with
                                putEntryVersion = fun put ->
                                    writes.Add put
                                    imports.proxy.putEntryVersion put
                        }
            }
        {
            folder = folder
            imports = observed
            heldSilicon = heldSilicon
            newMaterial = newMaterial
            collision = collision
            nextSilicon = nextSilicon
            decisions = observed.decideDroppedFiles files |> importOk
            writes = writes
        }

    let private observeDialog (imports : CatalogueImportContext) (request : ImportDialogRequest) : DialogObservation =
        let applied = ResizeArray<ImportDialogApplied>()
        let closes = ResizeArray<unit>()
        let events = ResizeArray<string>()
        {
            context =
                {
                    imports = imports
                    request = request
                    onApplied = fun result ->
                        applied.Add result
                        events.Add "applied"
                    requestClose = fun () ->
                        closes.Add ()
                        events.Add "closed"
                }
            applied = applied
            closes = closes
            events = events
        }

    type private KitDialogFixture =
        {
            context : KitLibraryContext
            snapshots : LibraryEntryEnvelope list
            decisions : ImportDecision list
            writes : ResizeArray<EntryVersionPut>
            pins : ResizeArray<LibraryVersionRef>
        }

    let private kitOk (result : Result<'Outcome, KitError>) : 'Outcome =
        result |> Result.defaultWith (fun error -> failwith error.describe)

    let private kitLibraryOk (result : Result<'Outcome, KitLibraryError>) : 'Outcome =
        result |> Result.defaultWith (fun error -> failwith error.describe)

    let private freshKitDialog () : KitDialogFixture =
        let folder = InMemoryKitFolder.create ()
        let proxy = KitProxy.createInMemory folder
        let instant = ClockTime (DateTime(2026, 11, 7, 12, 0, 0, DateTimeKind.Utc))
        let clock : ClockProvider =
            {
                now = fun () -> instant
            }
        let id = KitId (Guid.NewGuid())
        let name = KitName "Multilayer import kit"
        let author = KitAuthor "Import dialog tests"
        let kitFile = KitFile.create id name author instant
        proxy.initialiseKit kitFile |> kitOk
        let original : KitLibraryContext =
            {
                kit = KitContext.openKit proxy ReadWriteKitAccess clock |> kitOk
                imports = CatalogueImportContext.create (BuiltinCatalogue.freshStores ()).proxy
            }
        let reference = EntryLatestReference SampleIds.multilayerQwId.toLibraryEntryId
        original.snapshotReferences [ reference ] |> kitLibraryOk |> ignore
        let receiving = (CatalogueImportProxy.createInMemory ()).proxy
        let writes = ResizeArray<EntryVersionPut>()
        let pins = ResizeArray<LibraryVersionRef>()
        let recording =
            {
                receiving with
                    putEntryVersion = fun put ->
                        writes.Add put
                        receiving.putEntryVersion put
                    pinEntryVersion = fun entryId version ->
                        let reference : LibraryVersionRef =
                            {
                                entryId = entryId
                                version = version
                            }
                        pins.Add reference
                        receiving.pinEntryVersion entryId version
            }
        let context : KitLibraryContext =
            {
                kit = KitContext.openKit (KitProxy.createInMemory folder) ReadOnlyKitAccess clock |> kitOk
                imports = CatalogueImportContext.create recording
            }
        {
            context = context
            snapshots = context.kit.proxy.listSnapshots () |> kitOk
            decisions = context.resolveSnapshots () |> kitLibraryOk
            writes = writes
            pins = pins
        }

    let private assertKitDialogPending (fixture : KitDialogFixture) (observed : DialogObservation) : unit =
        Assert.Empty fixture.writes
        Assert.Empty fixture.pins
        Assert.Empty observed.applied
        Assert.Empty observed.closes
        Assert.Empty (fixture.context.imports.proxy.loggedImports () |> importOk)
        for snapshot in fixture.snapshots do
            Assert.Equal<HeldEntry option>(None, fixture.context.imports.proxy.tryHeldEntry snapshot.content.id |> importOk)

    let private heldEntry (imports : CatalogueImportContext) (id : LibraryEntryId) : HeldEntry =
        match imports.proxy.tryHeldEntry id |> importOk with
        | Some held -> held
        | None -> failwith $"Expected held entry {id.value}."

    let private tryControl (root : Control) (id : ConstructorUiId) : Control option =
        root.GetVisualDescendants()
        |> Seq.tryPick (function
            | :? Control as control when AutomationProperties.GetAutomationId control = id.value -> Some control
            | _ -> None)

    let private controlOf (root : Control) (id : ConstructorUiId) : Control =
        match tryControl root id with
        | Some control -> control
        | None -> failwith $"Missing control {id.value}."

    let private textsUnder (control : Control) : string list =
        control.GetVisualDescendants()
        |> Seq.choose (function :? TextBlock as text -> Some text.Text | _ -> None)
        |> Seq.toList

    let private pressControl (window : Window) (id : ConstructorUiId) : unit =
        let control = controlOf window id
        control.BringIntoView() |> ignore
        Dispatcher.UIThread.RunJobs()
        let point = control.TranslatePoint(Point(control.Bounds.Width / 2.0, control.Bounds.Height / 2.0), window)
        Assert.True(point.HasValue, $"{id.value} must be on screen.")
        window.MouseDown(point.Value, MouseButton.Left, RawInputModifiers.None)
        Dispatcher.UIThread.RunJobs()
        if window.IsVisible then
            window.MouseUp(point.Value, MouseButton.Left, RawInputModifiers.None)
            Dispatcher.UIThread.RunJobs()

    let private mountDialog (context : ImportDialogContext) : HostWindow =
        let window = HostWindow(Width = 900.0, Height = 760.0)
        Program.mkSimple (fun () -> context.initImportDialog) updateImportDialog importDialogView
        |> Program.withHost window
        |> Program.run
        window.Show()
        Dispatcher.UIThread.RunJobs()
        window

    let private droppedStorageItem (getPath : unit -> Uri) : IStorageItem =
        {
            new IStorageItem with
                member _.Path = getPath ()
                member _.Name = invalidOp "Entry drops must read the full path, not the display name."
                member _.CanBookmark = invalidOp "Entry drops must not inspect bookmarking."
                member _.GetBasicPropertiesAsync () = invalidOp "Entry drops must not inspect file properties."
                member _.SaveBookmarkAsync () = invalidOp "Entry drops must not save bookmarks."
                member _.GetParentAsync () = invalidOp "Entry drops must not inspect parent folders."
                member _.DeleteAsync () = invalidOp "Entry drops must not delete files."
                member _.MoveAsync _ = invalidOp "Entry drops must not move files."
                member _.Dispose () = ()
        }

    let private withEntryDropWindow (assertDrop : Window -> ResizeArray<FileName list> -> unit) : unit =
        HeadlessSession.run (fun () ->
            let window = Window()
            let dropped = ResizeArray<FileName list>()
            try
                EntryFileDrop.acceptEntryFiles window dropped.Add
                Assert.True(DragDrop.GetAllowDrop window)
                window.Show()
                Dispatcher.UIThread.RunJobs()
                assertDrop window dropped
            finally window.Close())

    let private raiseEntryDropEvent (window : Window) (routedEvent : RoutedEvent<DragEventArgs>) (data : IDataTransfer) (effects : DragDropEffects) : DragEventArgs =
        let event = DragEventArgs(routedEvent, data, window, Point(), KeyModifiers.None)
        event.DragEffects <- effects
        window.RaiseEvent event
        event

    let private assertEntryFileDragOverFeedback () : unit =
        withEntryDropWindow (fun window dropped ->
            use files = new DataTransfer()
            let reads = ResizeArray<unit>()
            files.Add(DataTransferItem.Create(DataFormat.File, Func<IStorageItem>(fun () ->
                reads.Add ()
                raise (IOException "Drag-over must not read file contents."))))
            let copy = raiseEntryDropEvent window DragDrop.DragOverEvent files DragDropEffects.Move
            Assert.Equal(DragDropEffects.Copy, copy.DragEffects)
            Assert.Empty reads
            use text = new DataTransfer()
            text.Add(DataTransferItem.CreateText "Not a file drop")
            use empty = new DataTransfer()
            for data in [ text; empty ] do
                let rejected = raiseEntryDropEvent window DragDrop.DragOverEvent data DragDropEffects.Copy
                Assert.Equal(DragDropEffects.None, rejected.DragEffects)
            Assert.Empty dropped)

    let private assertEntryFileDropOrder () : unit =
        withEntryDropWindow (fun window dropped ->
            let first = FileName (Path.Combine(AppContext.BaseDirectory, "Drop entries", "z silicon #1.material-json"))
            let second = FileName (Path.Combine(AppContext.BaseDirectory, "Drop entries", "a quartz.material-json"))
            let expected = [ first; second; first ]
            use data = new DataTransfer()
            data.Add(DataTransferItem.CreateText "Ignore non-file content")
            for file in expected do
                let item = droppedStorageItem (fun () -> Uri file.value)
                data.Add(DataTransferItem.CreateFile item)
            raiseEntryDropEvent window DragDrop.DropEvent data DragDropEffects.Copy |> ignore
            Assert.Equal<FileName list>(expected, Assert.Single dropped))

    let private assertUnavailableEntryFileDrops () : unit =
        withEntryDropWindow (fun window dropped ->
            use empty = new DataTransfer()
            use text = new DataTransfer()
            text.Add(DataTransferItem.CreateText "Not a file drop")
            use unavailable = new DataTransfer()
            let reads = ResizeArray<unit>()
            unavailable.Add(DataTransferItem.Create(DataFormat.File, Func<IStorageItem>(fun () ->
                reads.Add ()
                null)))
            for data in [ empty; text; unavailable ] do
                raiseEntryDropEvent window DragDrop.DropEvent data DragDropEffects.Copy |> ignore
                Assert.Empty dropped
            Assert.Single reads |> ignore)

    let private assertEntryDragOverFormatFailure () : unit =
        withEntryDropWindow (fun window dropped ->
            let reads = ResizeArray<unit>()
            use data =
                {
                    new IDataTransfer with
                        member _.Formats =
                            reads.Add ()
                            raise (IOException "Platform formats are unavailable.")
                        member _.Items = Array.empty<IDataTransferItem>
                        member _.Dispose () = ()
                }
            let event = raiseEntryDropEvent window DragDrop.DragOverEvent data DragDropEffects.Move
            Assert.Single reads |> ignore
            Assert.Equal(DragDropEffects.Move, event.DragEffects)
            Assert.Empty dropped)

    let private assertEntryFileDropItemListFailure () : unit =
        withEntryDropWindow (fun window dropped ->
            let reads = ResizeArray<unit>()
            use data =
                {
                    new IDataTransfer with
                        member _.Formats = [| DataFormat.File |]
                        member _.Items =
                            reads.Add ()
                            raise (IOException "Platform items are unavailable.")
                        member _.Dispose () = ()
                }
            raiseEntryDropEvent window DragDrop.DropEvent data DragDropEffects.Copy |> ignore
            Assert.Single reads |> ignore
            Assert.Empty dropped)

    let private assertEntryFileDropItemReadFailure () : unit =
        withEntryDropWindow (fun window dropped ->
            use data = new DataTransfer()
            let reads = ResizeArray<unit>()
            data.Add(DataTransferItem.Create(DataFormat.File, Func<IStorageItem>(fun () ->
                reads.Add ()
                raise (IOException "The platform file is unavailable."))))
            raiseEntryDropEvent window DragDrop.DropEvent data DragDropEffects.Copy |> ignore
            Assert.Single reads |> ignore
            Assert.Empty dropped)

    let private assertEntryFileDropPathFailure () : unit =
        withEntryDropWindow (fun window dropped ->
            let first = FileName (Path.Combine(AppContext.BaseDirectory, "valid.material-json"))
            let second = FileName (Path.Combine(AppContext.BaseDirectory, "unavailable.material-json"))
            let reads = ResizeArray<FileName>()
            let firstItem =
                droppedStorageItem (fun () ->
                    reads.Add first
                    Uri first.value)
            let failedItem =
                droppedStorageItem (fun () ->
                    reads.Add second
                    raise (IOException "The second path is unavailable."))
            use data = new DataTransfer()
            data.Add(DataTransferItem.CreateFile firstItem)
            data.Add(DataTransferItem.CreateFile failedItem)
            raiseEntryDropEvent window DragDrop.DropEvent data DragDropEffects.Copy |> ignore
            Assert.Equal<FileName list>([ first; second ], List.ofSeq reads)
            Assert.Empty dropped)

    [<Fact>]
    let ``the import dialog UiIds are the stable intent-named ids`` () : unit =
        let entry = LibraryEntryId (Guid.Parse "31801b01-444f-43c8-8440-000000000107")
        let action = RebindToHeldEntry entry
        let ids =
            [ ConstructorUiId.importDialogWindow
              ConstructorUiId.importDialogHeading
              ConstructorUiId.importDecisionList
              ConstructorUiId.importDecisionRow entry.uiKey
              ConstructorUiId.importActionOption entry.uiKey action.uiKey
              ConstructorUiId.importDiffToggle entry.uiKey
              ConstructorUiId.importDiffPreview entry.uiKey
              ConstructorUiId.importDialogMessage
              ConstructorUiId.importApplyButton
              ConstructorUiId.importCancelButton ]
        Assert.Equal<string list>(
            [ "ImportDialogWindow"; "ImportDialogHeading"; "ImportDecisionList"
              $"ImportDecisionRow_{entry.value}"
              $"ImportActionOption_{entry.value}_rebind-{entry.value}"
              $"ImportDiffToggle_{entry.value}"; $"ImportDiffPreview_{entry.value}"
              "ImportDialogMessage"; "ImportApplyButton"; "ImportCancelButton" ],
            ids |> List.map _.value)

    [<Fact>]
    let ``the dialog opens one row per decision with its default action chosen and nothing applied`` () : unit =
        let fixture = freshDialog ()
        let observed = observeDialog fixture.imports (ApplyDroppedEntries fixture.decisions)
        let model = observed.context.initImportDialog
        Assert.Equal<ImportChoice list>(fixture.decisions |> List.map _.byDefault, model.choices)
        Assert.Equal<LibraryEntryId list>(
            [ fixture.newMaterial.content.id; fixture.collision.content.id; fixture.nextSilicon.content.id ],
            model.choices |> List.map (fun choice -> choice.decision.entryId))
        Assert.Equal(NoImportDiffShown, model.diff)
        Assert.Equal<CatalogueImportError option>(None, model.lastError)
        Assert.Equal("Import 3 dropped entry files.", observed.context.request.heading.value)
        Assert.Equal<HeldEntry option>(None, fixture.imports.proxy.tryHeldEntry fixture.newMaterial.content.id |> importOk)
        Assert.Empty fixture.writes
        Assert.Empty observed.applied
        Assert.Empty observed.closes

    [<Fact>]
    let ``an override takes one of the row's alternatives, Decide restores the default, and an action the row does not offer is inert`` () : unit =
        let fixture = freshDialog ()
        let observed = observeDialog fixture.imports (ApplyDroppedEntries fixture.decisions)
        let initial = observed.context.initImportDialog
        let collision = fixture.decisions[1]
        let rebind = RebindToHeldEntry fixture.heldSilicon.content.id
        let chosen = updateImportDialog (OverrideImportRow (collision.entryId, rebind)) initial
        Assert.Equal(rebind, chosen.choices[1].action)
        let invalid = updateImportDialog (OverrideImportRow (fixture.newMaterial.content.id, ForkIncomingEntry)) chosen
        Assert.Equal<ImportChoice list>(chosen.choices, invalid.choices)
        let absent = LibraryEntryId (Guid.NewGuid())
        let absentResult = updateImportDialog (OverrideImportRow (absent, SkipIncomingEntry)) invalid
        Assert.Equal<ImportChoice list>(chosen.choices, absentResult.choices)
        let withError = { absentResult with lastError = Some (ImportRejectedErr "previous refusal") }
        let restored = updateImportDialog (DecideImportRow collision.entryId) withError
        Assert.Equal<ImportChoice list>(initial.choices, restored.choices)
        Assert.Equal<CatalogueImportError option>(None, restored.lastError)
        let cleared = updateImportDialog (OverrideImportRow (absent, SkipIncomingEntry)) withError
        Assert.Equal<CatalogueImportError option>(None, cleared.lastError)
        let firstDiff = updateImportDialog (ToggleImportDiff collision.entryId) restored
        let nextDiff = updateImportDialog (ToggleImportDiff fixture.nextSilicon.content.id) firstDiff
        Assert.Equal(ImportDiffShown fixture.nextSilicon.content.id, nextDiff.diff)
        Assert.Empty fixture.writes
        Assert.Empty observed.applied
        Assert.Empty observed.closes

    [<Fact>]
    let ``Apply hands every row's choice to the import seam once, reports it and closes`` () : unit =
        let fixture = freshDialog ()
        let observed = observeDialog fixture.imports (ApplyDroppedEntries fixture.decisions)
        let model =
            observed.context.initImportDialog
            |> updateImportDialog (OverrideImportRow (fixture.collision.content.id, SkipIncomingEntry))
        let after = updateImportDialog ApplyImportChoices model
        let applied = Assert.Single observed.applied
        Assert.Equal<ImportChoice list>(model.choices, applied.choices)
        Assert.Equal(observed.context.request, applied.request)
        Assert.Empty applied.outcome.forks
        Assert.Empty applied.outcome.rebindings
        Assert.Equal("Imported the dropped entry files (decisions applied: 3).", applied.summary.value)
        Assert.Single observed.closes |> ignore
        Assert.Equal<string list>([ "applied"; "closed" ], List.ofSeq observed.events)
        Assert.Equal<CatalogueImportError option>(None, after.lastError)
        Assert.Equal(fixture.newMaterial.content.id, (heldEntry fixture.imports fixture.newMaterial.content.id).id)
        let silicon = heldEntry fixture.imports fixture.heldSilicon.content.id
        Assert.Equal<VersionNumber list>([ VersionNumber.first; VersionNumber.first.next ], silicon.versions |> List.map _.version)
        Assert.Equal(VersionNumber.first, silicon.latest)
        let named = fixture.imports.proxy.heldEntriesNamed MaterialEntryKind fixture.collision.content.name |> importOk
        Assert.Equal(silicon.id, (Assert.Single named).id)
        Assert.Equal<LibraryEntryId list>(
            [ fixture.newMaterial.content.id; fixture.nextSilicon.content.id ],
            fixture.writes |> Seq.map (fun put -> put.envelope.content.id) |> Seq.toList)

    [<Fact>]
    let ``Apply on a pending catalogue settles it and logs the catalogue`` () : unit =
        let fixture = freshDialog ()
        let catalogueId = CatalogueId (Guid.NewGuid())
        let origin = CatalogueImportOrigin (catalogueId, CatalogueName "Lab")
        let decision = fixture.imports.proxy.decideEntry origin fixture.collision |> importOk
        let pending : PendingCatalogueImport =
            {
                entry =
                    {
                        catalogueId = catalogueId
                        folder = fixture.folder
                        manifestHash = fixture.collision.contentHash
                        importedAt = fixture.imports.now ()
                        entryCount = CatalogueEntryCount 1
                    }
                decisions = [ decision ]
            }
        let observed = observeDialog fixture.imports (SettlePendingCatalogue pending)
        let model = observed.context.initImportDialog
        Assert.Equal($"Import from {fixture.folder.value}: 1 entries need a decision.", model.context.request.heading.value)
        Assert.DoesNotContain(fixture.imports.proxy.loggedImports () |> importOk, fun entry -> entry.catalogueId = catalogueId)
        let after = updateImportDialog ApplyImportChoices model
        Assert.Equal<CatalogueImportError option>(None, after.lastError)
        let named = fixture.imports.proxy.heldEntriesNamed MaterialEntryKind fixture.collision.content.name |> importOk
        Assert.Equal<Set<LibraryEntryId>>(Set.ofList [ fixture.heldSilicon.content.id; fixture.collision.content.id ], named |> List.map _.id |> Set.ofList)
        let logged = fixture.imports.proxy.loggedImports () |> importOk |> List.filter (fun entry -> entry.catalogueId = catalogueId) |> Assert.Single
        Assert.Equal(pending.entry, logged)
        let applied = Assert.Single observed.applied
        Assert.Equal($"Imported {fixture.folder.value} (decisions applied: 1).", applied.summary.value)
        Assert.Single observed.closes |> ignore
        Assert.Single fixture.writes |> ignore

    [<Fact>]
    let ``a refused Apply shows its reason and keeps the dialog open`` () : unit =
        let fixture = freshDialog ()
        let material = fixture.newMaterial.content.toMaterialEntry |> Result.defaultWith (fun error -> failwith error.describe)
        let materialVersion : MaterialVersionId =
            {
                materialId = material.id
                version = fixture.newMaterial.content.version
            }
        let sample : Sample =
            {
                id = SampleId (Guid.NewGuid())
                name = EntryName "Dialog sample"
                structure =
                    {
                        films = []
                        substrate = None
                        lower = Some materialVersion
                    }
                substrate = ThinFilm
                description = EntryDescription "References the pending material"
                supportedEmission = EmitBoth
            }
        let sampleContent = LibraryEntryContent.ofSample VersionNumber.first UserProvenance sample
        let envelope = sampleContent.``sealed`` |> storageOk
        let materialFile = FileName "new.material-json"
        let sampleFile = FileName "sample.sample-json"
        let files = [ materialFile.combine fixture.folder; writeEntry fixture.folder sampleFile envelope ]
        let decisions = fixture.imports.decideDroppedFiles files |> importOk
        let observed = observeDialog fixture.imports (ApplyDroppedEntries decisions)
        let chosen =
            observed.context.initImportDialog
            |> updateImportDialog (OverrideImportRow (material.id.toLibraryEntryId, SkipIncomingEntry))
        let refused = updateImportDialog ApplyImportChoices chosen
        match refused.lastError with
        | Some (DanglingReferenceErr _) -> ()
        | _ -> Assert.Fail("Apply must refuse the sample's dangling material reference.")
        Assert.Equal<ImportChoice list>(chosen.choices, refused.choices)
        Assert.Empty observed.applied
        Assert.Empty observed.closes
        Assert.Empty fixture.writes
        for id in [ fixture.newMaterial.content.id; envelope.content.id ] do
            Assert.Equal<HeldEntry option>(None, fixture.imports.proxy.tryHeldEntry id |> importOk)
        HeadlessSession.run (fun () ->
            let window = HostWindow(Width = 900.0, Height = 760.0)
            try
                Program.mkSimple (fun () -> refused) updateImportDialog importDialogView
                |> Program.withHost window
                |> Program.run
                window.Show()
                Dispatcher.UIThread.RunJobs()
                let message = controlOf window ConstructorUiId.importDialogMessage :?> TextBlock
                match refused.lastError with
                | Some error -> Assert.Equal(error.caption.value, message.Text)
                | None -> Assert.Fail("The refusal must remain in the model.")
            finally window.Close())

    [<Fact>]
    let ``Cancel closes the dialog and applies nothing`` () : unit =
        let fixture = freshDialog ()
        let observed = observeDialog fixture.imports (ApplyDroppedEntries fixture.decisions)
        let model = observed.context.initImportDialog
        let after = updateImportDialog CancelImportDialog model
        Assert.Equal<ImportChoice list>(model.choices, after.choices)
        Assert.Single observed.closes |> ignore
        Assert.Empty observed.applied
        Assert.Empty fixture.writes
        Assert.Equal<HeldEntry option>(None, fixture.imports.proxy.tryHeldEntry fixture.newMaterial.content.id |> importOk)

    [<Fact>]
    let ``each row shows its name, kind, version, status, default action and every action it offers`` () : unit =
        let fixture = freshDialog ()
        let observed = observeDialog fixture.imports (ApplyDroppedEntries fixture.decisions)
        HeadlessSession.run (fun () ->
            let window = mountDialog observed.context
            try
                let row = controlOf window (ConstructorUiId.importDecisionRow fixture.collision.content.id.uiKey)
                let text = textsUnder row |> String.concat "\n"
                for expected in [ "Silicon — from collision.material-json"; "Material"; "version 1"; "Same name as a held entry"; "Default: Side by side" ] do
                    Assert.Contains(expected, text)
                let collision = fixture.decisions[1]
                let offered = collision.defaultAction :: collision.alternatives
                for action in offered do
                    let option = controlOf window (ConstructorUiId.importActionOption collision.entryId.uiKey action.uiKey)
                    Assert.Contains((collision.actionCaption action).value, textsUnder option)
                Assert.Equal<Control option>(None, tryControl window (ConstructorUiId.importDiffToggle fixture.newMaterial.content.id.uiKey))
                let heading = controlOf window ConstructorUiId.importDialogHeading :?> TextBlock
                Assert.Contains("Nothing is imported until you press Apply.", heading.Text)
                Assert.Equal(3, (controlOf window ConstructorUiId.importDecisionList :?> StackPanel).Children.Count)
                let apply = controlOf window ConstructorUiId.importApplyButton :?> Border
                let cancel = controlOf window ConstructorUiId.importCancelButton :?> Border
                Assert.Same(apply.Parent, cancel.Parent)
                Assert.NotEqual((apply.Background :?> SolidColorBrush).Color, (cancel.Background :?> SolidColorBrush).Color)
                let skip = SkipIncomingEntry
                let skipId = ConstructorUiId.importActionOption collision.entryId.uiKey skip.uiKey
                pressControl window skipId
                Assert.Equal(OpticalConstructor.Ui.MaterialsWindowView.positiveBackground, ((controlOf window skipId :?> Border).Background :?> SolidColorBrush).Color)
                Assert.Empty fixture.writes
            finally window.Close())

    [<Fact>]
    let ``a row's diff preview shows the lines that differ between the held and the incoming payload`` () : unit =
        let fixture = freshDialog ()
        let observed = observeDialog fixture.imports (ApplyDroppedEntries fixture.decisions)
        HeadlessSession.run (fun () ->
            let window = mountDialog observed.context
            try
                let entry = fixture.nextSilicon.content.id
                let toggle = ConstructorUiId.importDiffToggle entry.uiKey
                let preview = ConstructorUiId.importDiffPreview entry.uiKey
                Assert.Equal<Control option>(None, tryControl window preview)
                pressControl window toggle
                let lines = textsUnder (controlOf window preview)
                let description (envelope : LibraryEntryEnvelope) : string =
                    match envelope.content.descriptionOpt with
                    | Some description -> description.value
                    | None -> failwith "The Silicon fixture must have a description."
                Assert.Contains(lines, fun line -> line.StartsWith("- ", StringComparison.Ordinal) && line.Contains(description fixture.heldSilicon))
                Assert.Contains(lines, fun line -> line.StartsWith("+ ", StringComparison.Ordinal) && line.Contains(description fixture.nextSilicon))
                Assert.Contains("Hide diff", textsUnder (controlOf window toggle))
                pressControl window toggle
                Assert.Equal<Control option>(None, tryControl window preview)
                Assert.Contains("Show diff", textsUnder (controlOf window toggle))
                Assert.Empty fixture.writes
                Assert.Empty observed.applied
                Assert.Empty observed.closes
            finally window.Close())

    [<Fact>]
    let ``a kit snapshot request names the kit and opens every row on the kit's default`` () : unit =
        let fixture = freshKitDialog ()
        let request = ResolveKitSnapshot (fixture.context, fixture.decisions)
        let observed = observeDialog fixture.context.imports request
        let model = observed.context.initImportDialog
        Assert.Equal(ReadOnlyKitAccess, fixture.context.kit.access)
        Assert.NotEmpty fixture.snapshots
        Assert.Equal<LibraryEntryId list>(fixture.snapshots |> List.map (fun snapshot -> snapshot.content.id), request.decisions |> List.map _.entryId)
        Assert.Equal<ImportChoice list>(fixture.decisions |> List.map _.byDefault, model.choices)
        for choice in model.choices do
            Assert.Equal(SkipIncomingEntry, choice.action)
        Assert.Equal(NoImportDiffShown, model.diff)
        Assert.Equal<CatalogueImportError option>(None, model.lastError)
        Assert.Equal($"Open kit {fixture.context.kit.kitFile.name.value}: {fixture.snapshots.Length} library entries need a decision.", request.heading.value)
        assertKitDialogPending fixture observed
        HeadlessSession.run (fun () ->
            let window = mountDialog observed.context
            try
                let heading = controlOf window ConstructorUiId.importDialogHeading :?> TextBlock
                Assert.Contains(request.heading.value, heading.Text)
                Assert.Equal(fixture.snapshots.Length, (controlOf window ConstructorUiId.importDecisionList :?> StackPanel).Children.Count)
                for decision in fixture.decisions do
                    controlOf window (ConstructorUiId.importDecisionRow decision.entryId.uiKey) |> ignore
                assertKitDialogPending fixture observed
            finally window.Close())
        let changed = updateImportDialog (OverrideImportRow (SampleIds.multilayerQwId.toLibraryEntryId, AddIncomingEntry)) model
        assertKitDialogPending fixture observed
        let cancelled = updateImportDialog CancelImportDialog changed
        Assert.Equal<ImportChoice list>(changed.choices, cancelled.choices)
        Assert.Single observed.closes |> ignore
        Assert.Empty observed.applied
        Assert.Empty fixture.writes
        Assert.Empty fixture.pins
        Assert.Empty (fixture.context.imports.proxy.loggedImports () |> importOk)
        for snapshot in fixture.snapshots do
            Assert.Equal<HeldEntry option>(None, fixture.context.imports.proxy.tryHeldEntry snapshot.content.id |> importOk)
        let defaults = observeDialog fixture.context.imports request
        let appliedDefaults = updateImportDialog ApplyImportChoices defaults.context.initImportDialog
        Assert.Equal<CatalogueImportError option>(None, appliedDefaults.lastError)
        Assert.Single defaults.applied |> ignore
        Assert.Single defaults.closes |> ignore
        Assert.Empty fixture.writes
        Assert.Empty fixture.pins

    [<Fact>]
    let ``applying a kit snapshot request confirms through the kit library and closes`` () : unit =
        let fixture = freshKitDialog ()
        let request = ResolveKitSnapshot (fixture.context, fixture.decisions)
        let observed = observeDialog fixture.context.imports request
        let sampleOnly =
            observed.context.initImportDialog
            |> updateImportDialog (OverrideImportRow (SampleIds.multilayerQwId.toLibraryEntryId, AddIncomingEntry))
        assertKitDialogPending fixture observed
        let refused = updateImportDialog ApplyImportChoices sampleOnly
        match refused.lastError with
        | Some (DanglingReferenceErr _) -> ()
        | _ -> Assert.Fail("Adding only the sample must refuse its skipped material references.")
        Assert.Equal<ImportChoice list>(sampleOnly.choices, refused.choices)
        assertKitDialogPending fixture observed
        let chosen =
            fixture.decisions
            |> List.fold (fun model decision -> updateImportDialog (OverrideImportRow (decision.entryId, AddIncomingEntry)) model) refused
        Assert.Equal<CatalogueImportError option>(None, chosen.lastError)
        assertKitDialogPending fixture observed
        let after = updateImportDialog ApplyImportChoices chosen
        Assert.Equal<CatalogueImportError option>(None, after.lastError)
        let applied = Assert.Single observed.applied
        Assert.Equal(request, applied.request)
        Assert.Equal<ImportChoice list>(chosen.choices, applied.choices)
        Assert.Empty applied.outcome.rebindings
        Assert.Empty applied.outcome.forks
        Assert.Equal($"Opened the library of kit {fixture.context.kit.kitFile.name.value} (decisions applied: {fixture.decisions.Length}).", applied.summary.value)
        Assert.Single observed.closes |> ignore
        Assert.Equal<string list>([ "applied"; "closed" ], List.ofSeq observed.events)
        Assert.Equal(fixture.snapshots.Length, fixture.writes.Count)
        Assert.Equal<LibraryVersionRef list>(fixture.snapshots |> List.map _.libraryVersionRef, List.ofSeq fixture.pins)
        for snapshot in fixture.snapshots do
            Assert.Equal<LibraryEntryEnvelope option>(
                Some snapshot, fixture.context.imports.proxy.tryHeldEnvelope snapshot.content.id snapshot.content.version |> importOk)
        Assert.Empty (fixture.context.imports.proxy.loggedImports () |> importOk)
        Assert.Equal<LibraryEntryEnvelope list>(fixture.snapshots, fixture.context.kit.proxy.listSnapshots () |> kitOk)
        Assert.Equal(fixture.context.kit.kitFile, fixture.context.kit.proxy.readKitFile () |> kitOk)
        let reused = fixture.context.resolveSnapshots () |> kitLibraryOk
        for decision in reused do
            match decision.status with
            | SameContentHash _ -> Assert.Equal(ReuseHeldEntry, decision.defaultAction)
            | _ -> Assert.Fail("Each confirmed snapshot must resolve to a reuse row.")
        let reuseRequest = ResolveKitSnapshot (fixture.context, reused)
        let reuseObserved = observeDialog fixture.context.imports reuseRequest
        let reuseModel = reuseObserved.context.initImportDialog
        Assert.Equal<ImportChoice list>(reused |> List.map _.byDefault, reuseModel.choices)
        Assert.Equal(request.heading, reuseRequest.heading)
        HeadlessSession.run (fun () ->
            let window = mountDialog reuseObserved.context
            try
                Assert.Equal(fixture.snapshots.Length, (controlOf window ConstructorUiId.importDecisionList :?> StackPanel).Children.Count)
                Assert.Empty reuseObserved.applied
                Assert.Empty reuseObserved.closes
            finally window.Close())
        Assert.Equal(fixture.snapshots.Length, fixture.writes.Count)
        Assert.Equal(fixture.snapshots.Length, fixture.pins.Count)
        updateImportDialog ApplyImportChoices reuseModel |> ignore
        Assert.Single reuseObserved.applied |> ignore
        Assert.Single reuseObserved.closes |> ignore
        Assert.Equal(fixture.snapshots.Length, fixture.writes.Count)
        Assert.Equal(fixture.snapshots.Length, fixture.pins.Count)
        Assert.Empty (fixture.context.imports.proxy.loggedImports () |> importOk)

    [<Fact>]
    let ``a kit snapshot row offers to keep the kit's copy or to add it to my library`` () : unit =
        let fixture = freshKitDialog ()
        let request = ResolveKitSnapshot (fixture.context, fixture.decisions)
        let observed = observeDialog fixture.context.imports request
        let sample = fixture.decisions |> List.find (fun row -> row.entryId = SampleIds.multilayerQwId.toLibraryEntryId)
        Assert.Equal("Keep the kit's copy (read-only)", (sample.actionCaption sample.defaultAction).value)
        Assert.Equal("Add to my library", (sample.actionCaption AddIncomingEntry).value)
        for move in [ MoveLatestVersion; KeepLatestVersion ] do
            let action = AddIncomingVersion move
            Assert.Equal("Add this version to my library", (sample.actionCaption action).value)
        Assert.Equal("Add to my library as a fork", (sample.actionCaption ForkIncomingEntry).value)
        Assert.Equal("Reuse the held entry", (sample.actionCaption ReuseHeldEntry).value)
        Assert.Equal("Mark inactive", (sample.actionCaption RetireHeldEntry).value)
        let catalogue = freshDialog ()
        let kitOrigin = KitSnapshotOrigin fixture.context.kit.kitFile.name
        let collision = catalogue.imports.proxy.decideEntry kitOrigin catalogue.collision |> importOk
        match collision.status with
        | NameCollision _ -> ()
        | _ -> Assert.Fail("The fixture must offer a same-name collision.")
        Assert.Equal("Add to my library, side by side", (collision.actionCaption AddIncomingEntry).value)
        let rebind = RebindToHeldEntry catalogue.heldSilicon.content.id
        Assert.Equal($"Use my {catalogue.heldSilicon.content.name.value} instead", (collision.actionCaption rebind).value)
        let catalogueId = CatalogueId (Guid.NewGuid())
        let catalogueName = CatalogueName "Lab"
        let catalogueOrigin = CatalogueImportOrigin (catalogueId, catalogueName)
        let catalogueRow = catalogue.imports.proxy.decideEntry catalogueOrigin catalogue.collision |> importOk
        for row in [ catalogueRow; catalogue.decisions[1] ] do
            Assert.Equal("Skip", (row.actionCaption SkipIncomingEntry).value)
            Assert.Equal("Side by side", (row.actionCaption AddIncomingEntry).value)
            Assert.Equal("Fork as a new entry", (row.actionCaption ForkIncomingEntry).value)
            let makeLatest = AddIncomingVersion MoveLatestVersion
            let keepLatest = AddIncomingVersion KeepLatestVersion
            Assert.Equal("Add version, made latest", (row.actionCaption makeLatest).value)
            Assert.Equal("Add version, latest kept", (row.actionCaption keepLatest).value)
        HeadlessSession.run (fun () ->
            let window = mountDialog observed.context
            try
                for action in sample.defaultAction :: sample.alternatives do
                    let control = controlOf window (ConstructorUiId.importActionOption sample.entryId.uiKey action.uiKey)
                    Assert.Contains((sample.actionCaption action).value, textsUnder control)
                assertKitDialogPending fixture observed
            finally window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``acceptance: the import dialog window renders its rows with Apply and Cancel in one row, and Apply imports the chosen entries and closes it`` () : unit =
        assertEntryFileDragOverFeedback ()
        assertEntryFileDropOrder ()
        assertUnavailableEntryFileDrops ()
        assertEntryDragOverFormatFailure ()
        assertEntryFileDropItemListFailure ()
        assertEntryFileDropItemReadFailure ()
        assertEntryFileDropPathFailure ()
        let fixture = freshDialog ()
        let decisions = fixture.decisions |> List.filter (fun decision -> decision.entryId <> fixture.collision.content.id)
        let request = ApplyDroppedEntries decisions
        let applied = ResizeArray<ImportDialogApplied>()
        let closed = ResizeArray<unit>()
        HeadlessSession.run (fun () ->
            let window = ImportDialogWindow(fixture.imports, request, applied.Add)
            window.Closed.Add (fun _ -> closed.Add ())
            try
                window.Show()
                Dispatcher.UIThread.RunJobs()
                Assert.Equal("Import decisions", window.Title)
                Assert.Equal(ConstructorUiId.importDialogWindow.value, window.Name)
                Assert.Equal(ConstructorUiId.importDialogWindow.value, AutomationProperties.GetAutomationId window)
                Assert.True((controlOf window ConstructorUiId.importDialogHeading).IsEffectivelyVisible)
                let rows = controlOf window ConstructorUiId.importDecisionList :?> StackPanel
                Assert.Equal(2, rows.Children.Count)
                for decision in decisions do
                    let row = controlOf rows (ConstructorUiId.importDecisionRow decision.entryId.uiKey)
                    Assert.True(row.IsEffectivelyVisible)
                let apply = controlOf window ConstructorUiId.importApplyButton
                let cancel = controlOf window ConstructorUiId.importCancelButton
                let position (control : Control) : Point =
                    control.TranslatePoint(Point(0.0, 0.0), window)
                    |> Option.ofNullable
                    |> Option.defaultWith (fun () -> failwith "The action must be on screen.")
                let applyAt = position apply
                let cancelAt = position cancel
                Assert.Equal(applyAt.Y, cancelAt.Y)
                Assert.True(cancelAt.X > applyAt.X + apply.Bounds.Width, "Cancel must be to the right of Apply with a gap.")
                Assert.Empty fixture.writes
                Assert.Empty applied
                Assert.Empty closed
                pressControl window ConstructorUiId.importApplyButton
                let result = Assert.Single applied
                Assert.Equal(request, result.request)
                Assert.Equal<ImportChoice list>(decisions |> List.map _.byDefault, result.choices)
                Assert.Equal<LibraryEntryId list>(
                    decisions |> List.map _.entryId,
                    fixture.writes |> Seq.map (fun put -> put.envelope.content.id) |> Seq.toList)
                for envelope in [ fixture.newMaterial; fixture.nextSilicon ] do
                    let held = fixture.imports.proxy.tryHeldEnvelope envelope.content.id envelope.content.version |> importOk
                    Assert.Equal<LibraryEntryEnvelope option>(Some envelope, held)
                Assert.Single closed |> ignore
                Assert.False(window.IsVisible)
            finally window.Close())
