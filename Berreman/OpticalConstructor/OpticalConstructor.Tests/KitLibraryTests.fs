namespace OpticalConstructor.Tests

open System
open Xunit
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Experiments
open OpticalConstructor.Domain.Placement
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.Lifecycle
open OpticalConstructor.Domain.ItemModels
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.CatalogueFiles
open OpticalConstructor.Domain.CatalogueImport
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.KitFolders
open OpticalConstructor.Domain.KitLibrary
open OpticalConstructor.Storage.CatalogueImportStore
open OpticalConstructor.Storage.KitContentFiles
open OpticalConstructor.Storage.KitFolderStore
open OpticalConstructor.Storage.LibraryDatabaseStore

module KitLibraryTests =

    type private KitSnapshotOperation =
        | PinSnapshotVersion of LibraryVersionRef
        | WriteSnapshotVersion of LibraryVersionRef

    type private KitLibraryFixture =
        {
            folder : InMemoryKitFolder
            context : KitLibraryContext
            pins : ResizeArray<LibraryVersionRef>
            operations : ResizeArray<KitSnapshotOperation>
        }

    let private expectKitOk (result : Result<'Value, KitError>) : 'Value =
        result |> Result.defaultWith (fun error -> failwith error.describe)

    let private expectImportOk (result : Result<'Value, CatalogueImportError>) : 'Value =
        result |> Result.defaultWith (fun error -> failwith error.describe)

    let private expectSnapshotOk (result : Result<'Value, KitLibraryError>) : 'Value =
        result |> Result.defaultWith (fun error -> failwith error.describe)

    let private expectOverlayStoreOk (result : Result<'Value, 'Error>) : 'Value =
        result |> Result.defaultWith (fun _ -> failwith "The overlay store operation must succeed.")

    let private storesForOverlay (stores : InMemoryCatalogueStores) : KitLibraryStores =
        {
            library = liveLibrary stores.libraryEntries stores.samples (SourceProxy.createInMemory ())
            materials = stores.materials
            samples = stores.samples
            categories = stores.categories
        }

    let private catalogueEnvelope (id : LibraryEntryId) : LibraryEntryEnvelope =
        (BuiltinCatalogue.catalogueData ()).envelopes
        |> List.find (fun envelope -> envelope.content.id = id)

    let private sampleEnvelope () : LibraryEntryEnvelope =
        catalogueEnvelope SampleIds.multilayerQwId.toLibraryEntryId

    let private sampleClosure () : LibraryEntryEnvelope list =
        [
            sampleEnvelope ()
            catalogueEnvelope MaterialIds.vacuumMaterial.toLibraryEntryId
            catalogueEnvelope CategoryIds.vacuumCategory.toLibraryEntryId
            catalogueEnvelope MaterialIds.glass152.toLibraryEntryId
            catalogueEnvelope CategoryIds.glass.toLibraryEntryId
        ]

    let private snapshotFixture (proxy : CatalogueImportProxy) (access : KitAccess) : KitLibraryFixture =
        let pins = ResizeArray<LibraryVersionRef>()
        let operations = ResizeArray<KitSnapshotOperation>()
        let recordingProxy =
            {
                proxy with
                    pinEntryVersion = fun id version ->
                        let reference : LibraryVersionRef =
                            {
                                entryId = id
                                version = version
                            }
                        pins.Add reference
                        operations.Add(PinSnapshotVersion reference)
                        proxy.pinEntryVersion id version
            }
        let folder = InMemoryKitFolder.create ()
        let kitProxy = KitProxy.createInMemory folder
        let mutable next = ClockTime (DateTime(2026, 11, 7, 12, 0, 0, DateTimeKind.Utc))
        let clock : ClockProvider =
            {
                now = fun () ->
                    let current = next
                    next <- ClockTime (current.value.AddSeconds 1.0)
                    current
            }
        let kitId = KitId (Guid.Parse "13900000-0000-4000-8000-000000000001")
        let kitName = KitName "Snapshot test kit"
        let author = KitAuthor "Kit library tests"
        let kitFile = KitFile.create kitId kitName author (clock.now ())
        kitProxy.initialiseKit kitFile |> expectKitOk
        let recordingKitProxy =
            {
                kitProxy with
                    writeSnapshot = fun envelope ->
                        operations.Add(WriteSnapshotVersion envelope.libraryVersionRef)
                        kitProxy.writeSnapshot envelope
            }
        {
            folder = folder
            context =
                {
                    kit = KitContext.openKit recordingKitProxy access clock |> expectKitOk
                    imports = CatalogueImportContext.create recordingProxy
                }
            pins = pins
            operations = operations
        }

    let private freshSnapshotFixture () : KitLibraryFixture =
        snapshotFixture (BuiltinCatalogue.freshStores ()).proxy ReadWriteKitAccess

    let private assertSnapshots (expected : LibraryEntryEnvelope list) (fixture : KitLibraryFixture) : unit =
        let ordered =
            expected
            |> List.sortWith (fun first second -> String.CompareOrdinal(first.snapshotFileName.value, second.snapshotFileName.value))
        Assert.Equal<LibraryEntryEnvelope list>(ordered, fixture.context.kit.proxy.listSnapshots () |> expectKitOk)

    let private recordedSlot (id : ElementId) (kind : CatalogueKind) (binding : ElementValueId option) : Setups.SetupSlot =
        {
            id = id
            placement =
                {
                    ElementPlacement.create kind TablePoint.origin with valueId = binding
                }
            zoom = BeamLayout.ElementZoom 1.0
            beam = BeamLayout.OnMainBeam
            content = Setups.EmptySlotContent
        }

    let private recordingSetup : Setups.Setup =
        let sourceId = ElementId "recorded-source"
        let sampleId = ElementId "recorded-sample"
        let detectorId = ElementId "recorded-detector"
        {
            name = SetupName "Bench"
            revision = SetupRevision.first
            slots =
                [
                    recordedSlot sourceId LightSource (Some PresetIds.source600)
                    recordedSlot sampleId CatalogueKind.Sample (Some SampleIds.multilayerQwId.toElementValueId)
                    recordedSlot detectorId Detector None
                ]
            history = []
        }

    /// A dark line over `recordingSetup`'s revision (spec 0050 S8.2).
    let private recordingExperiment : Experiment =
        {
            id = ExperimentId 149
            name = (ExperimentId 149).defaultName
            setup = recordingSetup.reference
            scan = ExperimentScan.darkLine CaptureTransmitted
            role = ScienceRun
            sessionOpt = None
            notes = ExperimentNotes.none
            dataOpt = None
            corrections = []
        }

    let private recordingClosure () : LibraryEntryEnvelope list =
        let sourceId = PresetIds.source600.tryToLibraryEntryId |> Option.defaultWith (fun () -> failwith "The source must have an entry id.")
        catalogueEnvelope sourceId :: sampleClosure ()

    let private observeContentWrites
        (expected : LibraryEntryEnvelope list)
        (fixture : KitLibraryFixture)
        : KitLibraryContext * ResizeArray<KitFileKind> =
        let writes = ResizeArray<KitFileKind>()
        let proxy =
            {
                fixture.context.kit.proxy with
                    writeKindFile = fun kind stem text ->
                        assertSnapshots expected fixture
                        let versions = expected |> List.map (fun envelope -> envelope.libraryVersionRef)
                        Assert.Equal<LibraryVersionRef list>(versions, List.ofSeq fixture.pins)
                        Assert.Equal<KitSnapshotOperation list>(
                            versions |> List.collect (fun version -> [ PinSnapshotVersion version; WriteSnapshotVersion version ]),
                            List.ofSeq fixture.operations)
                        writes.Add kind
                        fixture.context.kit.proxy.writeKindFile kind stem text
            }
        let context =
            {
                fixture.context with
                    kit =
                        {
                            fixture.context.kit with proxy = proxy
                        }
            }
        context, writes

    /// The fixture's context with every content write refused by the error, once the expected snapshots are in place.
    let private refusingContentWrites
        (expected : LibraryEntryEnvelope list)
        (writeError : KitError)
        (fixture : KitLibraryFixture)
        : KitLibraryContext =
        let failingProxy =
            {
                fixture.context.kit.proxy with
                    writeKindFile = fun _ _ _ ->
                        assertSnapshots expected fixture
                        Error writeError
            }
        {
            fixture.context with
                kit =
                    {
                        fixture.context.kit with proxy = failingProxy
                    }
        }

    let private missingResolutionContent () : LibraryEntryContent =
        let id = MaterialId (Guid.Parse "14100000-0000-4000-8000-000000000001")
        let name = EntryName "Kit resolution material"
        BuiltinCatalogue.userMaterialContent id name

    let private collidingResolutionContent () : LibraryEntryContent =
        let id = MaterialId (Guid.Parse "14100000-0000-4000-8000-000000000002")
        let name = (catalogueEnvelope MaterialIds.silicon.toLibraryEntryId).content.name
        BuiltinCatalogue.userMaterialContent id name

    type private KitConfirmationOperation =
        | WriteConfirmedVersion of LibraryVersionRef
        | PinConfirmedVersion of LibraryVersionRef

    type private KitConfirmationFixture =
        {
            original : KitLibraryFixture
            context : KitLibraryContext
            snapshots : LibraryEntryEnvelope list
            paths : KitRelativePath list
            metadata : KitFile
            operations : ResizeArray<KitConfirmationOperation>
        }

    let private openConfirmationFixture (original : KitLibraryFixture) (proxy : CatalogueImportProxy) : KitConfirmationFixture =
        let operations = ResizeArray<KitConfirmationOperation>()
        let refuseWrite () = failwith "Confirming kit choices must only write added library versions and their pins."
        let opened =
            KitContext.openKit (KitProxy.createInMemory original.folder) ReadOnlyKitAccess original.context.kit.clock
            |> expectKitOk
        let kitProxy =
            {
                opened.proxy with
                    initialiseKit = fun _ -> refuseWrite ()
                    writeKitFile = fun _ -> refuseWrite ()
                    writeKindFile = fun _ _ _ -> refuseWrite ()
                    deleteKindFile = fun _ _ -> refuseWrite ()
                    copyInFile = fun _ _ _ -> refuseWrite ()
                    writeSnapshot = fun _ -> refuseWrite ()
                    acquireLock = fun _ -> refuseWrite ()
                    releaseLock = refuseWrite
            }
        let recordingProxy =
            {
                proxy with
                    putEntryVersion = fun put ->
                        operations.Add(WriteConfirmedVersion put.envelope.libraryVersionRef)
                        proxy.putEntryVersion put
                    pinEntryVersion = fun id version ->
                        let reference : LibraryVersionRef =
                            {
                                entryId = id
                                version = version
                            }
                        operations.Add(PinConfirmedVersion reference)
                        proxy.pinEntryVersion id version
                    retireEntry = fun _ -> refuseWrite ()
                    logImport = fun _ -> refuseWrite ()
                    forgetImport = fun _ -> refuseWrite ()
            }
        {
            original = original
            context =
                {
                    kit =
                        {
                            opened with proxy = kitProxy
                        }
                    imports = CatalogueImportContext.create recordingProxy
                }
            snapshots = opened.proxy.listSnapshots () |> expectKitOk
            paths = original.folder.paths ()
            metadata = opened.proxy.readKitFile () |> expectKitOk
            operations = operations
        }

    let private freshConfirmationFixture () : KitConfirmationFixture =
        let original = freshSnapshotFixture ()
        let sourceId = PresetIds.source600.tryToLibraryEntryId |> Option.defaultWith (fun () -> failwith "The preset must have an entry id.")
        original.context.snapshotReferences [ EntryVersionReference (sampleEnvelope ()).libraryVersionRef; EntryLatestReference sourceId ]
        |> expectSnapshotOk |> ignore
        openConfirmationFixture original (CatalogueImportProxy.createInMemory ()).proxy

    let private assertConfirmationKitUnchanged (fixture : KitConfirmationFixture) : unit =
        Assert.Equal<KitRelativePath list>(fixture.paths, fixture.original.folder.paths ())
        Assert.Equal<KitFile>(fixture.metadata, fixture.original.context.kit.proxy.readKitFile () |> expectKitOk)
        assertSnapshots fixture.snapshots fixture.original

    [<Fact>]
    let ``referencing a sample snapshots it with its materials and their categories, each once, and pins each`` () : unit =
        let fixture = freshSnapshotFixture ()
        let expected = sampleClosure ()
        let versions = expected |> List.map (fun envelope -> envelope.libraryVersionRef)
        let reference = EntryVersionReference (sampleEnvelope ()).libraryVersionRef
        let repeatedMaterial = EntryVersionReference (catalogueEnvelope MaterialIds.glass152.toLibraryEntryId).libraryVersionRef
        Assert.Equal<LibraryVersionRef list>(versions, fixture.context.snapshotReferences [ reference; reference; repeatedMaterial ] |> expectSnapshotOk)
        assertSnapshots expected fixture
        Assert.Equal<LibraryVersionRef list>(versions, List.ofSeq fixture.pins)
        Assert.Equal<KitSnapshotOperation list>(
            versions |> List.collect (fun version -> [ PinSnapshotVersion version; WriteSnapshotVersion version ]),
            List.ofSeq fixture.operations)
        for envelope in expected do
            Assert.Contains(envelope.snapshotPath, fixture.folder.paths ())

        let sharedCategory = freshSnapshotFixture ()
        let glass = catalogueEnvelope MaterialIds.glass150.toLibraryEntryId
        let glassReference = EntryVersionReference glass.libraryVersionRef
        let sharedVersions = versions @ [ glass.libraryVersionRef ]
        Assert.Equal<LibraryVersionRef list>(
            sharedVersions, sharedCategory.context.snapshotReferences [ reference; glassReference ] |> expectSnapshotOk)
        Assert.Equal<LibraryVersionRef list>(sharedVersions, List.ofSeq sharedCategory.pins)
        assertSnapshots (expected @ [ glass ]) sharedCategory

    [<Fact>]
    let ``the closure of references walks each entry's references depth first, each version once`` () : unit =
        let envelopes = (BuiltinCatalogue.catalogueData ()).envelopes
        let resolve (reference : EntryReference) : Result<LibraryEntryEnvelope, KitLibraryError> =
            let resolved =
                match reference with
                | EntryVersionReference version -> envelopes |> List.tryFind (fun envelope -> envelope.libraryVersionRef = version)
                | EntryLatestReference id ->
                    envelopes
                    |> List.filter (fun envelope -> envelope.content.id = id)
                    |> List.sortByDescending (fun envelope -> envelope.content.version)
                    |> List.tryHead
            match resolved with
            | Some envelope -> Ok envelope
            | None -> Error (UnheldEntryErr reference.describe)
        let multilayer : LibraryVersionRef =
            {
                entryId = SampleIds.multilayerQwId.toLibraryEntryId
                version = VersionNumber.first
            }
        let reference = EntryVersionReference multilayer
        Assert.Equal<Result<LibraryEntryEnvelope list, KitLibraryError>>(Ok (sampleClosure ()), referenceClosure resolve [ reference; reference ])

        let localId = LibraryEntryId (Guid.Parse "20300000-0000-4000-8000-000000000001")
        let localVersion : LibraryVersionRef =
            {
                entryId = localId
                version = VersionNumber.first
            }
        for unheld in [ EntryLatestReference localId; EntryVersionReference localVersion ] do
            Assert.Equal<Result<LibraryEntryEnvelope list, KitLibraryError>>(
                Error (UnheldEntryErr unheld.describe), referenceClosure resolve [ reference; unheld ])

    [<Fact>]
    let ``referencing a version the kit already holds writes and pins nothing`` () : unit =
        let fixture = freshSnapshotFixture ()
        let reference = EntryVersionReference (sampleEnvelope ()).libraryVersionRef
        fixture.context.snapshotReferences [ reference ] |> expectSnapshotOk |> ignore
        fixture.pins.Clear()
        fixture.operations.Clear()
        let paths = fixture.folder.paths ()
        Assert.Equal<Result<LibraryVersionRef list, KitLibraryError>>(Ok [], fixture.context.snapshotReferences [ reference ])
        Assert.Empty fixture.pins
        Assert.Empty fixture.operations
        Assert.Equal<KitRelativePath list>(paths, fixture.folder.paths ())
        assertSnapshots (sampleClosure ()) fixture

        // Spec 0050 S4.2: a held parent must still complete its closure after a partial failure.
        let material = catalogueEnvelope MaterialIds.vacuumMaterial.toLibraryEntryId
        let pinFailure = ImportRejectedErr "the first material cannot be pinned"
        let writeFailure = KitIoErr "the first material cannot be written"
        for failure in [ KitLibraryImportErr pinFailure; KitLibraryKitErr writeFailure ] do
            let partial = freshSnapshotFixture ()
            let context = partial.context
            let failingContext =
                match failure with
                | KitLibraryImportErr _ ->
                    let proxy =
                        {
                            context.imports.proxy with
                                pinEntryVersion = fun id version ->
                                    if id = material.content.id then Error pinFailure
                                    else context.imports.proxy.pinEntryVersion id version
                        }
                    {
                        context with
                            imports =
                                {
                                    context.imports with proxy = proxy
                                }
                    }
                | KitLibraryKitErr _ ->
                    let proxy =
                        {
                            context.kit.proxy with
                                writeSnapshot = fun envelope ->
                                    if envelope.libraryVersionRef = material.libraryVersionRef then Error writeFailure
                                    else context.kit.proxy.writeSnapshot envelope
                        }
                    {
                        context with
                            kit =
                                {
                                    context.kit with proxy = proxy
                                }
                    }
                | UnheldEntryErr _
                | KitLibraryItemErr _
                | KitLibrarySetupErr _ -> failwith "Only pin and write failures belong to this fixture."
            Assert.Equal<Result<LibraryVersionRef list, KitLibraryError>>(Error failure, failingContext.snapshotReferences [ reference ])
            assertSnapshots [ sampleEnvelope () ] partial
            let expectedPins =
                match failure with
                | KitLibraryImportErr _ -> [ (sampleEnvelope ()).libraryVersionRef ]
                | KitLibraryKitErr _ -> [ (sampleEnvelope ()).libraryVersionRef; material.libraryVersionRef ]
                | UnheldEntryErr _
                | KitLibraryItemErr _
                | KitLibrarySetupErr _ -> []
            Assert.Equal<LibraryVersionRef list>(expectedPins, List.ofSeq partial.pins)
            partial.pins.Clear()
            partial.operations.Clear()
            let remaining = sampleClosure () |> List.tail |> List.map (fun envelope -> envelope.libraryVersionRef)
            Assert.Equal<LibraryVersionRef list>(remaining, context.snapshotReferences [ reference ] |> expectSnapshotOk)
            Assert.Equal<LibraryVersionRef list>(remaining, List.ofSeq partial.pins)
            assertSnapshots (sampleClosure ()) partial

    [<Fact>]
    let ``a preset referenced by its id snapshots the library's latest version`` () : unit =
        let fixture = freshSnapshotFixture ()
        let sourceId = PresetIds.source600.tryToLibraryEntryId |> Option.defaultWith (fun () -> failwith "The built-in source id must be a GUID.")
        let source = catalogueEnvelope sourceId
        let reference = EntryLatestReference sourceId
        Assert.Equal<LibraryVersionRef list>([ source.libraryVersionRef ], fixture.context.snapshotReferences [ reference ] |> expectSnapshotOk)
        assertSnapshots [ source ] fixture
        Assert.Equal<LibraryVersionRef list>([ source.libraryVersionRef ], List.ofSeq fixture.pins)

        let nextContent =
            {
                source.content with
                    version = VersionNumber 2
                    name = EntryName "Library latest source"
            }
        BuiltinCatalogue.putSealedEntries fixture.context.imports [ nextContent ]
        let latest = fixture.context.imports.sealEntry nextContent |> expectImportOk
        let higherContent =
            {
                source.content with
                    version = VersionNumber 10
            }
        let higher = fixture.context.imports.sealEntry higherContent |> expectImportOk
        fixture.context.kit.proxy.writeSnapshot higher |> expectKitOk
        fixture.pins.Clear()
        fixture.operations.Clear()
        Assert.Equal<LibraryVersionRef list>([ latest.libraryVersionRef ], fixture.context.snapshotReferences [ reference ] |> expectSnapshotOk)
        Assert.Equal<LibraryVersionRef list>([ latest.libraryVersionRef ], List.ofSeq fixture.pins)
        assertSnapshots [ source; latest; higher ] fixture

    [<Fact>]
    let ``a version the kit holds is kept even when the library's differs, and one the library lacks resolves from the kit`` () : unit =
        let fixture = freshSnapshotFixture ()
        let silicon = catalogueEnvelope MaterialIds.silicon.toLibraryEntryId
        let content =
            {
                silicon.content with
                    descriptionOpt = Some (EntryDescription "The kit's own Silicon description")
            }
        let sealedCopy = fixture.context.imports.sealEntry content |> expectImportOk
        let category = catalogueEnvelope CategoryIds.semiconductor.toLibraryEntryId
        for envelope in [ sealedCopy; category ] do
            fixture.context.kit.proxy.writeSnapshot envelope |> expectKitOk
        fixture.operations.Clear()
        let paths = fixture.folder.paths ()
        let unavailable = ImportStoreUnavailableErr "An exact held version must not read its library copy"
        let guardedProxy =
            {
                fixture.context.imports.proxy with
                    tryHeldEnvelope = fun id version ->
                        if id = silicon.content.id then Error unavailable
                        else fixture.context.imports.proxy.tryHeldEnvelope id version
            }
        let guarded =
            {
                fixture.context with
                    imports =
                        {
                            fixture.context.imports with proxy = guardedProxy
                        }
            }
        let exact = EntryVersionReference sealedCopy.libraryVersionRef
        Assert.Equal<Result<LibraryVersionRef list, KitLibraryError>>(Ok [], guarded.snapshotReferences [ exact ])
        let latest = EntryLatestReference silicon.content.id
        Assert.Equal<Result<LibraryVersionRef list, KitLibraryError>>(Ok [], fixture.context.snapshotReferences [ latest ])
        Assert.Empty fixture.pins
        Assert.Empty fixture.operations
        Assert.Equal<KitRelativePath list>(paths, fixture.folder.paths ())
        assertSnapshots [ sealedCopy; category ] fixture
        Assert.NotEqual<ContentHash>(silicon.contentHash, sealedCopy.contentHash)

        let userId = MaterialId (Guid.Parse "13900000-0000-4000-8000-000000000002")
        let userName = EntryName "Kit-only material"
        let userContent = BuiltinCatalogue.userMaterialContent userId userName
        let user = fixture.context.imports.sealEntry userContent |> expectImportOk
        fixture.context.kit.proxy.writeSnapshot user |> expectKitOk
        fixture.operations.Clear()
        let heldPaths = fixture.folder.paths ()
        Assert.Equal<LibraryEntryEnvelope option>(None, fixture.context.imports.proxy.tryLatestEnvelope userContent.id |> expectImportOk)
        let userExact = EntryVersionReference user.libraryVersionRef
        Assert.Equal<Result<LibraryVersionRef list, KitLibraryError>>(Ok [], fixture.context.snapshotReferences [ userExact ])
        Assert.Empty fixture.pins
        Assert.Empty fixture.operations
        Assert.Equal<KitRelativePath list>(heldPaths, fixture.folder.paths ())
        assertSnapshots [ sealedCopy; category; user ] fixture

        // Spec 0050 S4.2: latest falls back to the highest numeric kit version and walks that copy's closure.
        let glassCategory = catalogueEnvelope CategoryIds.glass.toLibraryEntryId
        let higherContent =
            {
                userContent with
                    version = VersionNumber 10
                    categoryOpt = Some CategoryIds.glass
            }
        let middleContent =
            {
                userContent with
                    version = VersionNumber 2
            }
        let higher = fixture.context.imports.sealEntry higherContent |> expectImportOk
        let middle = fixture.context.imports.sealEntry middleContent |> expectImportOk
        for envelope in [ higher; middle ] do
            fixture.context.kit.proxy.writeSnapshot envelope |> expectKitOk
        fixture.operations.Clear()
        let userLatest = EntryLatestReference userContent.id
        Assert.Equal<LibraryVersionRef list>([ glassCategory.libraryVersionRef ], fixture.context.snapshotReferences [ userLatest ] |> expectSnapshotOk)
        Assert.Equal<LibraryVersionRef list>([ glassCategory.libraryVersionRef ], List.ofSeq fixture.pins)
        assertSnapshots [ sealedCopy; category; user; middle; higher; glassCategory ] fixture

    [<Fact>]
    let ``a reference neither the library nor the kit holds is UnheldEntryErr and writes nothing`` () : unit =
        let fixture = freshSnapshotFixture ()
        let missingId = LibraryEntryId (Guid.Parse "13900000-0000-4000-8000-000000000003")
        let missingVersion : LibraryVersionRef =
            {
                entryId = missingId
                version = VersionNumber 7
            }
        let absentSampleVersion : LibraryVersionRef =
            {
                entryId = SampleIds.multilayerQwId.toLibraryEntryId
                version = VersionNumber 7
            }
        let paths = fixture.folder.paths ()
        for reference in [ EntryVersionReference missingVersion; EntryLatestReference missingId; EntryVersionReference absentSampleVersion ] do
            match fixture.context.snapshotReferences [ reference ] with
            | Error (UnheldEntryErr reason) -> Assert.Contains(reference.describe, reason)
            | Error error -> Assert.Fail($"Expected an unheld reference: {error.describe}")
            | Ok _ -> Assert.Fail("An absent reference must be refused.")
        Assert.Empty fixture.pins
        Assert.Empty fixture.operations
        Assert.Equal<KitRelativePath list>(paths, fixture.folder.paths ())

        let reference = EntryVersionReference (sampleEnvelope ()).libraryVersionRef
        let readError = KitIoErr "snapshot listing failed"
        let failingKitProxy =
            {
                fixture.context.kit.proxy with listSnapshots = fun () -> Error readError
            }
        let failingKit =
            {
                fixture.context with
                    kit =
                        {
                            fixture.context.kit with proxy = failingKitProxy
                        }
            }
        Assert.Equal<Result<LibraryVersionRef list, KitLibraryError>>(
            Error (KitLibraryKitErr readError), failingKit.snapshotReferences [ reference ])
        let importError = ImportStoreUnavailableErr "library lookup failed"
        let failingImportProxy =
            {
                fixture.context.imports.proxy with
                    tryHeldEntry = fun _ -> Error importError
                    tryHeldEnvelope = fun _ _ -> Error importError
            }
        let failingImport =
            {
                fixture.context with
                    imports =
                        {
                            fixture.context.imports with proxy = failingImportProxy
                        }
            }
        for selected in [ reference; EntryLatestReference (sampleEnvelope ()).content.id ] do
            Assert.Equal<Result<LibraryVersionRef list, KitLibraryError>>(
                Error (KitLibraryImportErr importError), failingImport.snapshotReferences [ selected ])
        Assert.Empty fixture.pins
        Assert.Empty fixture.operations
        Assert.Equal<KitRelativePath list>(paths, fixture.folder.paths ())

    [<Fact>]
    let ``a read-only kit refuses to snapshot and pins nothing`` () : unit =
        let fixture = snapshotFixture (BuiltinCatalogue.freshStores ()).proxy ReadOnlyKitAccess
        let paths = fixture.folder.paths ()
        let unused () = failwith "A read-only snapshot request must not access either proxy."
        let kitProxy =
            {
                fixture.context.kit.proxy with
                    listSnapshots = unused
                    writeSnapshot = fun _ -> unused ()
            }
        let importProxy =
            {
                fixture.context.imports.proxy with
                    tryHeldEntry = fun _ -> unused ()
                    tryHeldEnvelope = fun _ _ -> unused ()
                    pinEntryVersion = fun _ _ -> unused ()
            }
        let context =
            {
                kit =
                    {
                        fixture.context.kit with proxy = kitProxy
                    }
                imports =
                    {
                        fixture.context.imports with proxy = importProxy
                    }
            }
        let reference = EntryVersionReference (sampleEnvelope ()).libraryVersionRef
        for references in [ []; [ reference ] ] do
            Assert.Equal<Result<LibraryVersionRef list, KitLibraryError>>(
                Error (KitLibraryKitErr (KitReadOnlyErr "snapshotReferences")), context.snapshotReferences references)
        Assert.Empty fixture.pins
        Assert.Empty fixture.operations
        Assert.Equal<KitRelativePath list>(paths, fixture.folder.paths ())

    [<Fact>]
    let ``a snapshot pins its version rows in the library database`` () : unit =
        use database = BuiltinCatalogue.freshLibraryDatabase ()
        let fixture = snapshotFixture (CatalogueImportProxy.createInDatabase database) ReadWriteKitAccess
        let reference = EntryVersionReference (sampleEnvelope ()).libraryVersionRef
        let written = fixture.context.snapshotReferences [ reference ] |> expectSnapshotOk
        let expected = sampleClosure ()
        Assert.Equal<LibraryVersionRef list>(expected |> List.map (fun envelope -> envelope.libraryVersionRef), written)
        Assert.Equal<LibraryVersionRef list>(written, List.ofSeq fixture.pins)
        assertSnapshots expected fixture
        let writtenVersions = Set.ofList written
        database.run (fun context ->
            for envelope in (BuiltinCatalogue.catalogueData ()).envelopes do
                match context.tryStoredVersion envelope.content.id envelope.content.version with
                | Error error -> Assert.Fail(error.describe)
                | Ok None -> Assert.Fail($"The catalogue version must remain held: {envelope.content.describe}")
                | Ok (Some stored) ->
                    let expectedPin =
                        if Set.contains envelope.libraryVersionRef writtenVersions then VersionPinned else VersionUnpinned
                    Assert.Equal<PinState>(expectedPin, stored.state.pin)
                    Assert.Equal<LibraryEntryEnvelope>(envelope, stored.envelope))
        |> Result.defaultWith (fun error -> failwith error.describe)

    [<Fact>]
    let ``a snapshot the library holds byte for byte is reused`` () : unit =
        let fixture = freshSnapshotFixture ()
        let silicon = catalogueEnvelope MaterialIds.silicon.toLibraryEntryId
        fixture.context.kit.proxy.writeSnapshot silicon |> expectKitOk
        let rows = fixture.context.resolveSnapshots () |> expectSnapshotOk
        let row = Assert.Single rows
        let origin = KitSnapshotOrigin fixture.context.kit.kitFile.name
        let held =
            fixture.context.imports.proxy.tryHeldEntry silicon.content.id
            |> expectImportOk
            |> Option.defaultWith (fun () -> failwith "The Built-in library must hold Silicon.")
        Assert.Equal<ImportOrigin>(origin, row.origin)
        Assert.Equal<ImportStatus>(SameContentHash (silicon, held), row.status)
        Assert.Equal<ImportAction>(ReuseHeldEntry, row.defaultAction)
        Assert.Empty row.alternatives
        Assert.Equal<ImportDecision list>(rows, (ImportDecision.split rows).awaiting)
        assertSnapshots [ silicon ] fixture

    [<Fact>]
    let ``a snapshot the library lacks stays in the kit read-only, with Add to my library offered`` () : unit =
        let fixture = freshSnapshotFixture ()
        let incoming = fixture.context.imports.sealEntry (missingResolutionContent ()) |> expectImportOk
        fixture.context.kit.proxy.writeSnapshot incoming |> expectKitOk
        let row = fixture.context.resolveSnapshots () |> expectSnapshotOk |> Assert.Single
        let origin = KitSnapshotOrigin fixture.context.kit.kitFile.name
        Assert.Equal<ImportOrigin>(origin, row.origin)
        Assert.Equal<ImportStatus>(EntryIdNotHeld incoming, row.status)
        Assert.Equal<ImportAction>(SkipIncomingEntry, row.defaultAction)
        Assert.Equal<ImportAction list>([ AddIncomingEntry ], row.alternatives)
        Assert.Equal<HeldEntry option>(None, fixture.context.imports.proxy.tryHeldEntry incoming.content.id |> expectImportOk)
        assertSnapshots [ incoming ] fixture

    [<Fact>]
    let ``a divergent snapshot is reported and the kit keeps it`` () : unit =
        let fixture = freshSnapshotFixture ()
        let silicon = catalogueEnvelope MaterialIds.silicon.toLibraryEntryId
        let content =
            {
                silicon.content with
                    descriptionOpt = Some (EntryDescription "Silicon measured for this kit")
            }
        let incoming = fixture.context.imports.sealEntry content |> expectImportOk
        fixture.context.kit.proxy.writeSnapshot incoming |> expectKitOk
        let row = fixture.context.resolveSnapshots () |> expectSnapshotOk |> Assert.Single
        let origin = KitSnapshotOrigin fixture.context.kit.kitFile.name
        let held =
            fixture.context.imports.proxy.tryHeldEntry silicon.content.id
            |> expectImportOk
            |> Option.defaultWith (fun () -> failwith "The Built-in library must hold Silicon.")
        Assert.Equal<VersionNumber>(VersionNumber.first, incoming.content.version)
        Assert.NotEqual<ContentHash>(silicon.contentHash, incoming.contentHash)
        Assert.Equal<ImportOrigin>(origin, row.origin)
        Assert.Equal<ImportStatus>(DivergentContent (incoming, held), row.status)
        Assert.Equal<ImportAction>(SkipIncomingEntry, row.defaultAction)
        Assert.Equal<ImportAction list>([ ForkIncomingEntry ], row.alternatives)
        Assert.Equal<LibraryEntryEnvelope option>(
            Some silicon, fixture.context.imports.proxy.tryHeldEnvelope silicon.content.id silicon.content.version |> expectImportOk)
        assertSnapshots [ incoming ] fixture

    [<Fact>]
    let ``a snapshot named like a held entry offers side by side and an explicit re-binding`` () : unit =
        let fixture = freshSnapshotFixture ()
        let silicon = catalogueEnvelope MaterialIds.silicon.toLibraryEntryId
        let incoming = fixture.context.imports.sealEntry (collidingResolutionContent ()) |> expectImportOk
        fixture.context.kit.proxy.writeSnapshot incoming |> expectKitOk
        let row = fixture.context.resolveSnapshots () |> expectSnapshotOk |> Assert.Single
        let origin = KitSnapshotOrigin fixture.context.kit.kitFile.name
        let sameName =
            fixture.context.imports.proxy.heldEntriesNamed incoming.content.schema.kind incoming.content.name |> expectImportOk
        let expectedName = EntryName $"Silicon — from {fixture.context.kit.kitFile.name.value}"
        Assert.NotEqual<LibraryEntryId>(silicon.content.id, incoming.content.id)
        Assert.Equal<ImportOrigin>(origin, row.origin)
        Assert.Equal<ImportStatus>(NameCollision (incoming, sameName), row.status)
        Assert.Equal<ImportAction>(SkipIncomingEntry, row.defaultAction)
        Assert.Equal<ImportAction list>([ AddIncomingEntry; RebindToHeldEntry silicon.content.id ], row.alternatives)
        Assert.Equal<EntryName>(expectedName, row.rowName)
        Assert.Equal<HeldEntry option>(None, fixture.context.imports.proxy.tryHeldEntry incoming.content.id |> expectImportOk)
        assertSnapshots [ incoming ] fixture

    [<Fact>]
    let ``resolving the snapshots writes nothing, through a read-only kit too`` () : unit =
        use database = BuiltinCatalogue.freshLibraryDatabase ()
        let proxy = CatalogueImportProxy.createInDatabase database
        let fixture = snapshotFixture proxy ReadWriteKitAccess
        let silicon = catalogueEnvelope MaterialIds.silicon.toLibraryEntryId
        let missing = fixture.context.imports.sealEntry (missingResolutionContent ()) |> expectImportOk
        let colliding = fixture.context.imports.sealEntry (collidingResolutionContent ()) |> expectImportOk
        for envelope in [ silicon; missing; colliding ] do
            fixture.context.kit.proxy.writeSnapshot envelope |> expectKitOk
        fixture.operations.Clear()
        let snapshots = fixture.context.kit.proxy.listSnapshots () |> expectKitOk
        let origin = KitSnapshotOrigin fixture.context.kit.kitFile.name
        let expected = snapshots |> List.map (fun envelope -> proxy.decideEntry origin envelope |> expectImportOk)
        let heldBefore = proxy.heldEntriesOf CatalogueIds.builtInCatalogue |> expectImportOk
        let logBefore = proxy.loggedImports () |> expectImportOk
        let paths = fixture.folder.paths ()
        let metadata = fixture.context.kit.proxy.readKitFile () |> expectKitOk
        let reads = ResizeArray<LibraryEntryId>()
        let listings = ResizeArray<KitName>()
        let refuseWrite () = failwith "Snapshot resolution must not call a write seam."
        let guardedProxy =
            {
                proxy with
                    tryHeldEntry = fun id ->
                        reads.Add id
                        proxy.tryHeldEntry id
                    pinEntryVersion = fun _ _ -> refuseWrite ()
                    putEntryVersion = fun _ -> refuseWrite ()
                    retireEntry = fun _ -> refuseWrite ()
                    logImport = fun _ -> refuseWrite ()
                    forgetImport = fun _ -> refuseWrite ()
            }
        for access in [ ReadWriteKitAccess; ReadOnlyKitAccess ] do
            let opened =
                match access with
                | ReadWriteKitAccess -> fixture.context.kit
                | ReadOnlyKitAccess ->
                    KitContext.openKit (KitProxy.createInMemory fixture.folder) access fixture.context.kit.clock |> expectKitOk
            let kitProxy =
                {
                    opened.proxy with
                        listSnapshots = fun () ->
                            listings.Add opened.kitFile.name
                            opened.proxy.listSnapshots ()
                        initialiseKit = fun _ -> refuseWrite ()
                        writeKitFile = fun _ -> refuseWrite ()
                        writeKindFile = fun _ _ _ -> refuseWrite ()
                        deleteKindFile = fun _ _ -> refuseWrite ()
                        copyInFile = fun _ _ _ -> refuseWrite ()
                        writeSnapshot = fun _ -> refuseWrite ()
                        acquireLock = fun _ -> refuseWrite ()
                        releaseLock = refuseWrite
                }
            let context =
                {
                    kit =
                        {
                            opened with proxy = kitProxy
                        }
                    imports = CatalogueImportContext.create guardedProxy
                }
            reads.Clear()
            listings.Clear()
            let rows = context.resolveSnapshots () |> expectSnapshotOk
            Assert.Equal<ImportDecision list>(expected, rows)
            Assert.Equal<ImportDecision list>(rows, (ImportDecision.split rows).awaiting)
            Assert.Equal<LibraryEntryId list>(snapshots |> List.map (fun envelope -> envelope.content.id), List.ofSeq reads)
            Assert.Equal<KitName>(opened.kitFile.name, Assert.Single listings)
            Assert.Equal<KitRelativePath list>(paths, fixture.folder.paths ())
            Assert.Equal<KitFile>(metadata, fixture.context.kit.proxy.readKitFile () |> expectKitOk)
            Assert.Equal<LibraryEntryEnvelope list>(snapshots, fixture.context.kit.proxy.listSnapshots () |> expectKitOk)
            Assert.Equal<HeldEntry list>(heldBefore, proxy.heldEntriesOf CatalogueIds.builtInCatalogue |> expectImportOk)
            Assert.Empty(proxy.heldUserEntries () |> expectImportOk)
            Assert.Equal<CatalogueImportLogEntry list>(logBefore, proxy.loggedImports () |> expectImportOk)
            Assert.Empty fixture.pins
            Assert.Empty fixture.operations

            let emptyFixture = snapshotFixture guardedProxy access
            let emptyPaths = emptyFixture.folder.paths ()
            reads.Clear()
            Assert.Equal<Result<ImportDecision list, KitLibraryError>>(Ok [], emptyFixture.context.resolveSnapshots ())
            Assert.Empty reads
            Assert.Equal<KitRelativePath list>(emptyPaths, emptyFixture.folder.paths ())

            let readError = KitIoErr "snapshot resolution listing failed"
            let failingKitProxy =
                {
                    kitProxy with listSnapshots = fun () -> Error readError
                }
            let failingKitContext =
                {
                    context with
                        kit =
                            {
                                context.kit with proxy = failingKitProxy
                            }
                }
            Assert.Equal<Result<ImportDecision list, KitLibraryError>>(
                Error (KitLibraryKitErr readError), failingKitContext.resolveSnapshots ())
            Assert.Empty reads

            let importError = ImportStoreUnavailableErr "snapshot resolution lookup failed"
            let failingImportProxy =
                {
                    guardedProxy with
                        tryHeldEntry = fun id ->
                            reads.Add id
                            Error importError
                }
            let failingImportContext =
                {
                    context with imports = CatalogueImportContext.create failingImportProxy
                }
            Assert.Equal<Result<ImportDecision list, KitLibraryError>>(
                Error (KitLibraryImportErr importError), failingImportContext.resolveSnapshots ())
            Assert.Equal<LibraryEntryId list>([ (List.head snapshots).content.id ], List.ofSeq reads)

        database.run (fun context ->
            for envelope in (BuiltinCatalogue.catalogueData ()).envelopes do
                match context.tryStoredVersion envelope.content.id envelope.content.version with
                | Error error -> Assert.Fail(error.describe)
                | Ok None -> Assert.Fail($"Resolution removed {envelope.content.describe} from the library.")
                | Ok (Some stored) ->
                    Assert.Equal<LibraryEntryEnvelope>(envelope, stored.envelope)
                    Assert.Equal<PinState>(VersionUnpinned, stored.state.pin)
                    Assert.Equal<EntryLifecycle>(ActiveEntry, stored.state.lifecycle))
        |> Result.defaultWith (fun error -> failwith error.describe)

    [<Fact>]
    let ``a kit opened against an empty library resolves every entry from its snapshots`` () : unit =
        let fixture = freshSnapshotFixture ()
        let sourceId = PresetIds.source600.tryToLibraryEntryId |> Option.defaultWith (fun () -> failwith "The preset must have an entry id.")
        let references =
            [ EntryVersionReference (sampleEnvelope ()).libraryVersionRef; EntryLatestReference sourceId ]
        fixture.context.snapshotReferences references |> expectSnapshotOk |> ignore
        let snapshots = fixture.context.kit.proxy.listSnapshots () |> expectKitOk
        Assert.Equal<LibraryEntryEnvelope list>(
            (sampleClosure () @ [ catalogueEnvelope sourceId ]) |> List.sortBy (fun envelope -> envelope.content.id),
            snapshots |> List.sortBy (fun envelope -> envelope.content.id))
        let empty = CatalogueImportProxy.createInMemory ()
        let stores = storesForOverlay empty
        let opened =
            KitContext.openKit (KitProxy.createInMemory fixture.folder) ReadOnlyKitAccess fixture.context.kit.clock |> expectKitOk
        let context : KitLibraryContext =
            {
                kit = opened
                imports = CatalogueImportContext.create empty.proxy
            }
        let paths = fixture.folder.paths ()
        let metadata = opened.proxy.readKitFile () |> expectKitOk
        let rows = context.resolveSnapshots () |> expectSnapshotOk
        Assert.Equal(snapshots.Length, rows.Length)
        for snapshot, row in List.zip snapshots rows do
            Assert.Equal<ImportStatus>(EntryIdNotHeld snapshot, row.status)
        let overlay = context.openOverlay [] |> expectSnapshotOk
        Assert.Equal<LibraryEntryEnvelope list>(snapshots, overlay.snapshots)
        Assert.Empty overlay.replacements
        let session = stores.overlaidWith overlay
        let categories = session.categories.listCategories () |> expectOverlayStoreOk
        for snapshot in snapshots do
            let content = snapshot.content
            match content.schema.kind with
            | SampleEntryKind ->
                let sample = content.toSample |> expectOverlayStoreOk
                Assert.Equal<Sample>(BuiltinCatalogue.sample sample.id, sample)
                let version = SampleVersionId.firstOf sample.id
                Assert.Equal<Result<Sample option, SampleError>>(Ok (Some sample), session.samples.resolveVersion version)
                Assert.Equal<Result<Sample option, SampleError>>(Ok (Some sample), session.samples.tryGetSample sample.id)
                Assert.Equal<Result<LibraryEntry option, LibraryError>>(Ok (Some (SampleItem sample)), session.library.tryGetEntry sample.id.toElementValueId)
            | MaterialEntryKind ->
                let material = content.toMaterialEntry |> expectOverlayStoreOk
                Assert.Equal<MaterialEntry>(BuiltinCatalogue.material material.id, material)
                let version = MaterialVersionId.firstOf material.id
                Assert.Equal<Result<MaterialEntry option, MaterialError>>(Ok (Some material), session.materials.resolveVersion version)
                Assert.Equal<Result<MaterialEntry option, MaterialError>>(Ok (Some material), session.materials.tryGetMaterial material.id)
            | CategoryEntryKind ->
                let category = content.toMaterialCategory |> expectOverlayStoreOk
                Assert.Equal<MaterialCategory>(BuiltinCatalogue.category category.id, category)
                Assert.Contains(category, categories)
            | SourceEntryKind ->
                let source = content.toSourcePreset |> expectOverlayStoreOk |> SourceItem
                Assert.Equal<LibraryEntry>(BuiltinCatalogue.preset PresetIds.source600, source)
                Assert.Equal<Result<LibraryEntry option, LibraryError>>(Ok (Some source), session.library.tryGetEntry source.entryId)
                Assert.Equal<Result<LibraryEntry list, LibraryError>>(Ok [ source ], session.library.entriesForKind LightSource)
            | _ -> Assert.Fail("The sample and source closure must contain only samples, materials, categories and sources.")
        let expectedEntries = [ SampleItem (BuiltinCatalogue.sample SampleIds.multilayerQwId); BuiltinCatalogue.preset PresetIds.source600 ]
        Assert.Equal<Result<LibraryTree list, LibraryError>>(Ok [ LibraryTree.byKind expectedEntries ], session.library.libraryTrees ())
        Assert.Equal<KitRelativePath list>(paths, fixture.folder.paths ())
        Assert.Equal<KitFile>(metadata, opened.proxy.readKitFile () |> expectKitOk)
        Assert.Empty(empty.proxy.heldEntriesOf CatalogueIds.builtInCatalogue |> expectImportOk)
        Assert.Empty(empty.proxy.heldUserEntries () |> expectImportOk)
        Assert.Empty(empty.proxy.loggedImports () |> expectImportOk)

        let higherSnapshots =
            snapshots |> List.map (fun snapshot ->
                let content =
                    {
                        snapshot.content with
                            version = snapshot.content.version.next
                            name = EntryName $"{snapshot.content.name.value} kit v2"
                    }
                context.imports.sealEntry content |> expectImportOk)
        let multipleVersions : KitLibraryOverlay =
            {
                snapshots = higherSnapshots @ snapshots
                replacements = []
            }
        let latest = stores.overlaidWith multipleVersions
        for snapshot in higherSnapshots do
            match snapshot.content.schema.kind with
            | SampleEntryKind ->
                let sample = snapshot.content.toSample |> expectOverlayStoreOk
                Assert.Equal<Result<Sample option, SampleError>>(Ok (Some sample), latest.samples.tryGetSample sample.id)
                Assert.Equal<Result<LibraryEntry option, LibraryError>>(Ok (Some (SampleItem sample)), latest.library.tryGetEntry sample.id.toElementValueId)
                Assert.Equal<Result<LibraryEntry list, LibraryError>>(Ok [ SampleItem sample ], latest.library.entriesForKind CatalogueKind.Sample)
                let first = SampleVersionId.firstOf sample.id
                Assert.Equal<Result<Sample option, SampleError>>(Ok (Some (BuiltinCatalogue.sample sample.id)), latest.samples.resolveVersion first)
            | MaterialEntryKind ->
                let material = snapshot.content.toMaterialEntry |> expectOverlayStoreOk
                Assert.Equal<Result<MaterialEntry option, MaterialError>>(Ok (Some material), latest.materials.tryGetMaterial material.id)
                let first = MaterialVersionId.firstOf material.id
                Assert.Equal<Result<MaterialEntry option, MaterialError>>(Ok (Some (BuiltinCatalogue.material material.id)), latest.materials.resolveVersion first)
            | CategoryEntryKind ->
                let category = snapshot.content.toMaterialCategory |> expectOverlayStoreOk
                let found = latest.categories.listCategories () |> expectOverlayStoreOk |> List.filter (fun entry -> entry.id = category.id)
                Assert.Equal<MaterialCategory>(category, Assert.Single found)
            | SourceEntryKind ->
                let source = snapshot.content.toSourcePreset |> expectOverlayStoreOk |> SourceItem
                Assert.Equal<Result<LibraryEntry option, LibraryError>>(Ok (Some source), latest.library.tryGetEntry source.entryId)
                Assert.Equal<Result<LibraryEntry list, LibraryError>>(Ok [ source ], latest.library.entriesForKind LightSource)
            | _ -> Assert.Fail("Unexpected kind in the higher snapshot versions.")
        let unknownSample = SampleId (Guid.Parse "14200000-0000-4000-8000-000000000001")
        let unknownVersion = SampleVersionId.firstOf unknownSample
        Assert.Equal<Result<Sample option, SampleError>>(Ok None, latest.samples.resolveVersion unknownVersion)
        Assert.Equal<Result<Sample option, SampleError>>(Ok None, latest.samples.tryGetSample unknownSample)
        Assert.Equal<Result<LibraryEntry option, LibraryError>>(Ok None, latest.library.tryGetEntry unknownSample.toElementValueId)
        let textId = ElementValueId "not-a-guid"
        Assert.Equal<Result<LibraryEntry option, LibraryError>>(Ok None, latest.library.tryGetEntry textId)

    [<Fact>]
    let ``the session resolves a divergent version to the kit's snapshot`` () : unit =
        let held = BuiltinCatalogue.freshStores ()
        let stores = storesForOverlay held
        let fixture = snapshotFixture held.proxy ReadWriteKitAccess
        let silicon = catalogueEnvelope MaterialIds.silicon.toLibraryEntryId
        let content =
            {
                silicon.content with descriptionOpt = Some (EntryDescription "Silicon measured for this kit")
            }
        let incoming = fixture.context.imports.sealEntry content |> expectImportOk
        fixture.context.kit.proxy.writeSnapshot incoming |> expectKitOk
        let overlay = fixture.context.openOverlay [] |> expectSnapshotOk
        let session = stores.overlaidWith overlay
        let version = MaterialVersionId.firstOf MaterialIds.silicon
        let kitMaterial = content.toMaterialEntry |> expectOverlayStoreOk
        Assert.Equal<Result<MaterialEntry option, MaterialError>>(Ok (Some kitMaterial), session.materials.resolveVersion version)
        Assert.Equal<Result<MaterialEntry option, MaterialError>>(
            Ok (Some (BuiltinCatalogue.material MaterialIds.silicon)), session.materials.tryGetMaterial MaterialIds.silicon)
        Assert.Equal<Result<unit, MaterialError>>(Ok (), stores.materials.pinMaterialVersion version)
        // Spec 0050 S2.2/S4.2: a pinned version advances only when its physics changes.
        let newer =
            {
                BuiltinCatalogue.material MaterialIds.silicon with
                    description = Some (EntryDescription "Latest library Silicon")
                    complexity = (BuiltinCatalogue.material MaterialIds.glass152).complexity
            }
        Assert.Equal<Result<unit, MaterialError>>(Ok (), stores.materials.saveMaterial newer)
        Assert.Equal<Result<MaterialEntry option, MaterialError>>(Ok (Some newer), session.materials.tryGetMaterial MaterialIds.silicon)
        Assert.Equal<Result<MaterialEntry option, MaterialError>>(Ok (Some kitMaterial), session.materials.resolveVersion version)
        let newerVersion : MaterialVersionId =
            {
                materialId = MaterialIds.silicon
                version = VersionNumber.first.next
            }
        Assert.Equal<Result<MaterialEntry option, MaterialError>>(
            Ok (Some (BuiltinCatalogue.material MaterialIds.silicon)), stores.materials.resolveVersion version)
        Assert.Equal<Result<MaterialEntry option, MaterialError>>(Ok (Some newer), stores.materials.resolveVersion newerVersion)
        Assert.Equal<Result<MaterialEntry option, MaterialError>>(Ok (Some newer), session.materials.resolveVersion newerVersion)

        let sampleContent =
            {
                (sampleEnvelope ()).content with descriptionOpt = Some (EntryDescription "Kit multilayer measurement")
            }
        let kitSample = sampleContent.toSample |> expectOverlayStoreOk
        let sampleSnapshot = fixture.context.imports.sealEntry sampleContent |> expectImportOk
        let extended : KitLibraryOverlay =
            {
                overlay with snapshots = sampleSnapshot :: overlay.snapshots
            }
        let sampleSession = stores.overlaidWith extended
        let sampleVersion = SampleVersionId.firstOf kitSample.id
        Assert.Equal<Result<Sample option, SampleError>>(Ok (Some kitSample), sampleSession.samples.resolveVersion sampleVersion)
        Assert.Equal<Result<Sample option, SampleError>>(stores.samples.tryGetSample kitSample.id, sampleSession.samples.tryGetSample kitSample.id)
        Assert.Equal<Result<LibraryEntry option, LibraryError>>(
            stores.library.tryGetEntry kitSample.id.toElementValueId, sampleSession.library.tryGetEntry kitSample.id.toElementValueId)
        Assert.Equal<Result<LibraryEntry list, LibraryError>>(
            stores.library.entriesForKind CatalogueKind.Sample, sampleSession.library.entriesForKind CatalogueKind.Sample)
        let hiddenLibrary =
            {
                stores.library with entriesForKind = fun _ -> Ok []
            }
        Assert.Equal<Result<LibraryEntry list, LibraryError>>(
            Ok [], (hiddenLibrary.overlaidWith extended).entriesForKind CatalogueKind.Sample)
        let categorySnapshot = catalogueEnvelope CategoryIds.glass.toLibraryEntryId
        let categoryContent =
            {
                categorySnapshot.content with name = EntryName "Kit-specific glass category"
            }
        let divergentCategory = fixture.context.imports.sealEntry categoryContent |> expectImportOk
        let categoryOverlay : KitLibraryOverlay =
            {
                extended with snapshots = divergentCategory :: extended.snapshots
            }
        Assert.Equal<Result<MaterialCategory list, CategoryError>>(
            stores.categories.listCategories (), (stores.overlaidWith categoryOverlay).categories.listCategories ())

        let materialError = MaterialStoreUnavailableErr "material read unavailable"
        let sampleError = SampleStoreUnavailableErr "sample read unavailable"
        let libraryError = LibraryUnavailableErr "library read unavailable"
        let categoryError = CategoryStoreUnavailableErr "category read unavailable"
        let failingStores : KitLibraryStores =
            {
                materials =
                    {
                        stores.materials with
                            tryGetMaterial = fun _ -> Error materialError
                            resolveVersion = fun _ -> Error materialError
                    }
                samples =
                    {
                        stores.samples with
                            tryGetSample = fun _ -> Error sampleError
                            resolveVersion = fun _ -> Error sampleError
                    }
                library =
                    {
                        stores.library with
                            tryGetEntry = fun _ -> Error libraryError
                            entriesForKind = fun _ -> Error libraryError
                    }
                categories =
                    {
                        stores.categories with listCategories = fun () -> Error categoryError
                    }
            }
        let failed = failingStores.overlaidWith extended
        Assert.Equal<Result<MaterialEntry option, MaterialError>>(Ok (Some kitMaterial), failed.materials.resolveVersion version)
        Assert.Equal<Result<MaterialEntry option, MaterialError>>(Error materialError, failed.materials.tryGetMaterial MaterialIds.silicon)
        Assert.Equal<Result<MaterialEntry option, MaterialError>>(Error materialError, failed.materials.resolveVersion newerVersion)
        Assert.Equal<Result<Sample option, SampleError>>(Ok (Some kitSample), failed.samples.resolveVersion sampleVersion)
        Assert.Equal<Result<Sample option, SampleError>>(Error sampleError, failed.samples.tryGetSample kitSample.id)
        Assert.Equal<Result<LibraryEntry option, LibraryError>>(Error libraryError, failed.library.tryGetEntry kitSample.id.toElementValueId)
        Assert.Equal<Result<LibraryEntry list, LibraryError>>(Error libraryError, failed.library.entriesForKind CatalogueKind.Sample)
        Assert.Equal<Result<LibraryTree list, LibraryError>>(Error libraryError, failed.library.libraryTrees ())
        Assert.Equal<Result<MaterialCategory list, CategoryError>>(Error categoryError, failed.categories.listCategories ())

        let missingCategory =
            {
                content with categoryOpt = None
            }
        let missingDescription =
            {
                sampleContent with descriptionOpt = None
            }
        let category = catalogueEnvelope CategoryIds.glass.toLibraryEntryId
        let wrongCategory =
            {
                category.content with payload = content.payload
            }
        let invalidOverlay : KitLibraryOverlay =
            {
                snapshots =
                    [
                        {
                            incoming with content = missingCategory
                        }
                        {
                            sampleSnapshot with content = missingDescription
                        }
                        {
                            category with content = wrongCategory
                        }
                    ]
                replacements = []
            }
        let invalid = (storesForOverlay (CatalogueImportProxy.createInMemory ())).overlaidWith invalidOverlay
        let refusalDescription (decoded : Result<'Entry, LibraryEntryError>) : string =
            match decoded with
            | Error error -> error.describe
            | Ok _ -> failwith "The intentionally incomplete content must not decode."
        let materialReason = $"snapshot {missingCategory.describe}: {refusalDescription missingCategory.toMaterialEntry}"
        let sampleReason = $"snapshot {missingDescription.describe}: {refusalDescription missingDescription.toSample}"
        let categoryReason = $"snapshot {wrongCategory.describe}: {refusalDescription wrongCategory.toMaterialCategory}"
        Assert.Equal<Result<MaterialEntry option, MaterialError>>(Error (MaterialStoreUnavailableErr materialReason), invalid.materials.resolveVersion version)
        Assert.Equal<Result<MaterialEntry option, MaterialError>>(Error (MaterialStoreUnavailableErr materialReason), invalid.materials.tryGetMaterial MaterialIds.silicon)
        Assert.Equal<Result<Sample option, SampleError>>(Error (SampleStoreUnavailableErr sampleReason), invalid.samples.resolveVersion sampleVersion)
        Assert.Equal<Result<Sample option, SampleError>>(Error (SampleStoreUnavailableErr sampleReason), invalid.samples.tryGetSample kitSample.id)
        Assert.Equal<Result<LibraryEntry option, LibraryError>>(Error (LibraryUnavailableErr sampleReason), invalid.library.tryGetEntry kitSample.id.toElementValueId)
        Assert.Equal<Result<LibraryEntry list, LibraryError>>(Error (LibraryUnavailableErr sampleReason), invalid.library.entriesForKind CatalogueKind.Sample)
        Assert.Equal<Result<MaterialCategory list, CategoryError>>(Error (CategoryStoreUnavailableErr categoryReason), invalid.categories.listCategories ())

    [<Fact>]
    let ``a re-bound entry resolves to the held entry`` () : unit =
        let held = BuiltinCatalogue.freshStores ()
        let stores = storesForOverlay held
        let fixture = snapshotFixture held.proxy ReadWriteKitAccess
        let sample : Sample =
            {
                BuiltinCatalogue.sample SampleIds.glassFilm200Id with
                    id = SampleId (Guid.Parse "14200000-0000-4000-8000-000000000002")
                    name = EntryName "Kit-only sample"
            }
        let kitVersion = VersionNumber 7
        let incoming =
            LibraryEntryContent.ofSample kitVersion UserProvenance sample
            |> fixture.context.imports.sealEntry |> expectImportOk
        fixture.context.kit.proxy.writeSnapshot incoming |> expectKitOk
        let rebinding : EntryRebinding =
            {
                incoming = incoming.content.id
                held = SampleIds.multilayerQwId.toLibraryEntryId
            }
        let paths = fixture.folder.paths ()
        let overlay = fixture.context.openOverlay [ rebinding ] |> expectSnapshotOk
        let replacement = Assert.Single overlay.replacements
        Assert.Equal<LibraryEntryId>(incoming.content.id, replacement.incoming)
        Assert.Equal<LibraryEntryEnvelope>(sampleEnvelope (), replacement.held)
        let session = stores.overlaidWith overlay
        let expected = BuiltinCatalogue.sample SampleIds.multilayerQwId
        let incomingVersion : SampleVersionId =
            {
                sampleId = sample.id
                version = incoming.content.version
            }
        Assert.Equal<Result<Sample option, SampleError>>(Ok (Some expected), session.samples.tryGetSample sample.id)
        Assert.Equal<Result<Sample option, SampleError>>(Ok (Some expected), session.samples.resolveVersion incomingVersion)
        Assert.Equal<Result<LibraryEntry option, LibraryError>>(Ok (Some (SampleItem expected)), session.library.tryGetEntry sample.id.toElementValueId)
        let offered = session.library.entriesForKind CatalogueKind.Sample |> expectOverlayStoreOk
        Assert.DoesNotContain(sample.id.toElementValueId, offered |> List.map (fun entry -> entry.entryId))
        Assert.Equal<Result<LibraryEntry list, LibraryError>>(stores.library.entriesForKind CatalogueKind.Sample, Ok offered)
        Assert.Equal<Result<Sample option, SampleError>>(Ok None, stores.samples.tryGetSample sample.id)
        Assert.Equal<KitRelativePath list>(paths, fixture.folder.paths ())
        assertSnapshots [ incoming ] fixture
        Assert.Empty fixture.pins

        let heldVersion = SampleVersionId.firstOf expected.id
        let newer =
            {
                expected with description = EntryDescription "Latest held multilayer"
            }
        Assert.Equal<Result<unit, SampleError>>(Ok (), stores.samples.pinSampleVersion heldVersion)
        let newerEnvelope =
            LibraryEntryContent.ofSample VersionNumber.first.next UserProvenance newer
            |> fixture.context.imports.sealEntry |> expectImportOk
        let put : EntryVersionPut =
            {
                envelope = newerEnvelope
                latest = MoveLatestVersion
            }
        held.proxy.putEntryVersion put |> expectImportOk
        let reopened = fixture.context.openOverlay [ rebinding ] |> expectSnapshotOk
        Assert.Equal<LibraryEntryEnvelope>(newerEnvelope, (Assert.Single reopened.replacements).held)
        Assert.Equal<Result<Sample option, SampleError>>(Ok (Some newer), (stores.overlaidWith reopened).samples.resolveVersion incomingVersion)

        let missingId = LibraryEntryId (Guid.Parse "14200000-0000-4000-8000-000000000003")
        let missingRebinding : EntryRebinding =
            {
                rebinding with held = missingId
            }
        match fixture.context.openOverlay [ missingRebinding ] with
        | Error (UnheldEntryErr reason) ->
            Assert.Contains(string missingId.value, reason)
            Assert.Contains(string incoming.content.id.value, reason)
        | _ -> Assert.Fail("A missing rebinding target must return UnheldEntryErr.")
        let reads = ResizeArray<LibraryEntryId>()
        let readError = ImportStoreUnavailableErr "held entry lookup unavailable"
        let failedImports =
            {
                held.proxy with
                    tryHeldEntry = fun id ->
                        reads.Add id
                        Error readError
            }
        let failedContext =
            {
                fixture.context with imports = CatalogueImportContext.create failedImports
            }
        Assert.Equal<Result<KitLibraryOverlay, KitLibraryError>>(
            Error (KitLibraryImportErr readError), failedContext.openOverlay [ rebinding; missingRebinding ])
        Assert.Equal<LibraryEntryId>(rebinding.held, Assert.Single reads)
        reads.Clear()
        let kitError = KitIoErr "overlay snapshot listing unavailable"
        let failedKitProxy =
            {
                fixture.context.kit.proxy with listSnapshots = fun () -> Error kitError
            }
        let failedKitContext =
            {
                failedContext with
                    kit =
                        {
                            failedContext.kit with proxy = failedKitProxy
                        }
            }
        Assert.Equal<Result<KitLibraryOverlay, KitLibraryError>>(
            Error (KitLibraryKitErr kitError), failedKitContext.openOverlay [ rebinding ])
        Assert.Empty reads

    [<Fact>]
    let ``the overlay passes every write to the library and never lists a kit entry in it`` () : unit =
        let fixture = freshSnapshotFixture ()
        let sourceId = PresetIds.source600.tryToLibraryEntryId |> Option.defaultWith (fun () -> failwith "The preset must have an entry id.")
        fixture.context.snapshotReferences [ EntryVersionReference (sampleEnvelope ()).libraryVersionRef; EntryLatestReference sourceId ]
        |> expectSnapshotOk |> ignore
        let overlay = fixture.context.openOverlay [] |> expectSnapshotOk
        let empty = CatalogueImportProxy.createInMemory ()
        let stores = storesForOverlay empty
        let session = stores.overlaidWith overlay
        Assert.Same(stores.materials.saveMaterial, session.materials.saveMaterial)
        Assert.Same(stores.materials.markMaterialInactive, session.materials.markMaterialInactive)
        Assert.Same(stores.materials.markMaterialActive, session.materials.markMaterialActive)
        Assert.Same(stores.materials.supersedeMaterial, session.materials.supersedeMaterial)
        Assert.Same(stores.materials.pinMaterialVersion, session.materials.pinMaterialVersion)
        Assert.Same(stores.materials.removeMaterial, session.materials.removeMaterial)
        Assert.Same(stores.samples.saveSample, session.samples.saveSample)
        Assert.Same(stores.samples.markSampleInactive, session.samples.markSampleInactive)
        Assert.Same(stores.samples.markSampleActive, session.samples.markSampleActive)
        Assert.Same(stores.samples.supersedeSample, session.samples.supersedeSample)
        Assert.Same(stores.samples.pinSampleVersion, session.samples.pinSampleVersion)
        Assert.Same(stores.samples.removeSample, session.samples.removeSample)
        Assert.Same(stores.categories.addCategory, session.categories.addCategory)
        Assert.Same(stores.categories.updateCategory, session.categories.updateCategory)
        Assert.Same(stores.categories.removeCategory, session.categories.removeCategory)
        Assert.Same(stores.materials.listMaterials, session.materials.listMaterials)
        Assert.Same(stores.materials.searchMaterials, session.materials.searchMaterials)
        Assert.Same(stores.samples.listSamples, session.samples.listSamples)
        Assert.Same(stores.samples.searchSamples, session.samples.searchSamples)
        let paths = fixture.folder.paths ()
        let sample : Sample =
            {
                BuiltinCatalogue.sample SampleIds.glassFilm200Id with
                    id = SampleId (Guid.Parse "14200000-0000-4000-8000-000000000004")
                    name = EntryName "Written through the kit session"
            }
        Assert.Equal<Result<unit, SampleError>>(Ok (), session.samples.saveSample sample)
        Assert.Equal<Result<Sample option, SampleError>>(Ok (Some sample), stores.samples.tryGetSample sample.id)
        for visibility in [ ActiveOnly; IncludeInactive ] do
            Assert.Equal<Result<Sample list, SampleError>>(Ok [ sample ], stores.samples.listSamples visibility)
            Assert.Equal<Result<Sample list, SampleError>>(Ok [ sample ], session.samples.listSamples visibility)
            Assert.Equal<Result<MaterialEntry list, MaterialError>>(Ok [], stores.materials.listMaterials visibility)
            Assert.Equal<Result<MaterialEntry list, MaterialError>>(Ok [], session.materials.listMaterials visibility)
        Assert.Equal<Result<Sample list, SampleError>>(Ok [ sample ], session.samples.searchSamples SampleQuery.empty)
        Assert.Equal<Result<MaterialEntry list, MaterialError>>(Ok [], session.materials.searchMaterials MaterialQuery.empty)
        Assert.Equal<Result<Sample list, SampleError>>(Ok [ sample ], stores.samples.searchSamples SampleQuery.empty)
        Assert.Equal<Result<MaterialEntry list, MaterialError>>(Ok [], stores.materials.searchMaterials MaterialQuery.empty)
        Assert.Equal<Result<MaterialCategory list, CategoryError>>(Ok [], stores.categories.listCategories ())
        for kind in [ CatalogueKind.Sample; LightSource; Detector; LinearPolarizer; CircularPolarizer ] do
            let expected = if kind = CatalogueKind.Sample then [ SampleItem sample ] else []
            Assert.Equal<Result<LibraryEntry list, LibraryError>>(Ok expected, stores.library.entriesForKind kind)
        Assert.Equal<Result<LibraryTree list, LibraryError>>(Ok [ LibraryTree.byKind [ SampleItem sample ] ], stores.library.libraryTrees ())
        Assert.Equal<Result<LibraryEntry list, LibraryError>>(
            Ok [ SampleItem sample; SampleItem (BuiltinCatalogue.sample SampleIds.multilayerQwId) ], session.library.entriesForKind CatalogueKind.Sample)
        Assert.Equal<KitRelativePath list>(paths, fixture.folder.paths ())
        Assert.Equal<LibraryEntryEnvelope list>(overlay.snapshots, fixture.context.kit.proxy.listSnapshots () |> expectKitOk)
        Assert.Empty(empty.proxy.heldEntriesOf CatalogueIds.builtInCatalogue |> expectImportOk)
        Assert.Empty(empty.proxy.heldUserEntries () |> expectImportOk)
        Assert.Empty(empty.proxy.loggedImports () |> expectImportOk)

    [<Fact>]
    let ``adding a kit's entry to the library holds it and pins the version the kit references`` () : unit =
        let fixture = freshConfirmationFixture ()
        let source = fixture.snapshots |> List.find (fun envelope -> envelope.content.schema.kind = SourceEntryKind)
        let choices =
            fixture.context.resolveSnapshots () |> expectSnapshotOk
            |> List.map (fun row ->
                if row.entryId = source.content.id then
                    {
                        decision = row
                        action = AddIncomingEntry
                    }
                else row.byDefault)
        let outcome = fixture.context.confirm choices |> expectImportOk
        Assert.Empty outcome.rebindings
        Assert.Empty outcome.forks
        Assert.Equal<KitConfirmationOperation list>(
            [ WriteConfirmedVersion source.libraryVersionRef; PinConfirmedVersion source.libraryVersionRef ],
            List.ofSeq fixture.operations)
        for envelope in fixture.snapshots do
            let held = fixture.context.imports.proxy.tryHeldEntry envelope.content.id |> expectImportOk
            if envelope.content.id = source.content.id then
                let entry = held |> Option.defaultWith (fun () -> failwith "The selected source must be held.")
                Assert.Equal<VersionNumber>(source.content.version, entry.latest)
                Assert.Equal<ContentHash>(source.contentHash, (Assert.Single entry.versions).contentHash)
            else Assert.Equal<HeldEntry option>(None, held)
        Assert.Equal<LibraryEntryEnvelope option>(
            Some source, fixture.context.imports.proxy.tryHeldEnvelope source.content.id source.content.version |> expectImportOk)
        assertConfirmationKitUnchanged fixture

        let newerContent =
            {
                source.content with version = VersionNumber 2
            }
        let versionStores = CatalogueImportProxy.createInMemory ()
        BuiltinCatalogue.putSealedEntries (CatalogueImportContext.create versionStores.proxy) [ newerContent ]
        let versionFixture = openConfirmationFixture fixture.original versionStores.proxy
        let versionRow =
            versionFixture.context.resolveSnapshots () |> expectSnapshotOk
            |> List.find (fun row -> row.entryId = source.content.id)
        let versionChoice : ImportChoice =
            {
                decision = versionRow
                action = AddIncomingVersion KeepLatestVersion
            }
        versionFixture.context.confirm [ versionChoice ] |> expectImportOk |> ignore
        let held =
            versionStores.proxy.tryHeldEntry source.content.id |> expectImportOk
            |> Option.defaultWith (fun () -> failwith "Both source versions must remain held.")
        Assert.Equal<VersionNumber>(newerContent.version, held.latest)
        Assert.Equal<VersionNumber list>([ source.content.version; newerContent.version ], held.versions |> List.map _.version)
        Assert.Equal<LibraryEntryEnvelope option>(
            Some source, versionStores.proxy.tryHeldEnvelope source.content.id source.content.version |> expectImportOk)
        Assert.Equal<KitConfirmationOperation list>(
            [ WriteConfirmedVersion source.libraryVersionRef; PinConfirmedVersion source.libraryVersionRef ],
            List.ofSeq versionFixture.operations)
        assertConfirmationKitUnchanged versionFixture

        let divergentContent =
            {
                source.content with descriptionOpt = Some (EntryDescription "The receiving library's source")
            }
        let forkStores = CatalogueImportProxy.createInMemory ()
        BuiltinCatalogue.putSealedEntries (CatalogueImportContext.create forkStores.proxy) [ divergentContent ]
        let forkFixture = openConfirmationFixture fixture.original forkStores.proxy
        let forkRow =
            forkFixture.context.resolveSnapshots () |> expectSnapshotOk
            |> List.find (fun row -> row.entryId = source.content.id)
        let forkChoice : ImportChoice =
            {
                decision = forkRow
                action = ForkIncomingEntry
            }
        let forkOutcome = forkFixture.context.confirm [ forkChoice ] |> expectImportOk
        let fork = Assert.Single forkOutcome.forks
        Assert.Empty forkOutcome.rebindings
        Assert.Equal<LibraryEntryId>(source.content.id, fork.original)
        Assert.NotEqual<LibraryEntryId>(source.content.id, fork.fork)
        let forkEnvelope =
            forkStores.proxy.tryHeldEnvelope fork.fork VersionNumber.first |> expectImportOk
            |> Option.defaultWith (fun () -> failwith "The fork must be held under its new identity.")
        Assert.Equal<KitConfirmationOperation list>([ WriteConfirmedVersion forkEnvelope.libraryVersionRef ], List.ofSeq forkFixture.operations)
        Assert.Equal<LibraryEntryPayload>(source.content.payload, forkEnvelope.content.payload)
        assertConfirmationKitUnchanged forkFixture

        let writeFailure = ImportStoreUnavailableErr "the source cannot be written"
        let empty = CatalogueImportProxy.createInMemory ()
        let failedProxy =
            {
                empty.proxy with putEntryVersion = fun _ -> Error writeFailure
            }
        let failedFixture = openConfirmationFixture fixture.original failedProxy
        Assert.Equal<Result<ImportOutcome, CatalogueImportError>>(Error writeFailure, failedFixture.context.confirm choices)
        Assert.Equal<KitConfirmationOperation list>([ WriteConfirmedVersion source.libraryVersionRef ], List.ofSeq failedFixture.operations)
        Assert.Empty(empty.proxy.heldEntriesOf CatalogueIds.builtInCatalogue |> expectImportOk)
        assertConfirmationKitUnchanged failedFixture

    [<Fact>]
    let ``adding a sample while its material stays in the kit is refused and writes nothing`` () : unit =
        let fixture = freshConfirmationFixture ()
        let sample = sampleEnvelope ()
        let rows = fixture.context.resolveSnapshots () |> expectSnapshotOk
        let sourceRow = rows |> List.find (fun row -> row.kind = SourceEntryKind)
        let remaining = rows |> List.filter (fun row -> row.entryId <> sourceRow.entryId)
        let choices =
            sourceRow :: remaining
            |> List.map (fun row ->
                if row.entryId = sample.content.id || row.entryId = sourceRow.entryId then
                    {
                        decision = row
                        action = AddIncomingEntry
                    }
                else row.byDefault)
        match fixture.context.confirm choices with
        | Error (DanglingReferenceErr reason) -> Assert.Contains(sample.content.id.value.ToString(), reason)
        | Error error -> Assert.Fail(error.describe)
        | Ok _ -> Assert.Fail("A sample whose materials stay in the kit must be refused.")
        Assert.Empty fixture.operations
        for envelope in fixture.snapshots do
            Assert.Equal<HeldEntry option>(None, fixture.context.imports.proxy.tryHeldEntry envelope.content.id |> expectImportOk)
        assertConfirmationKitUnchanged fixture

        let invalidChoices =
            choices |> List.map (fun choice ->
                if choice.decision.entryId = sample.content.id then
                    {
                        choice with action = AddIncomingVersion KeepLatestVersion
                    }
                else choice)
        match fixture.context.confirm invalidChoices with
        | Error (InvalidImportChoiceErr reason) -> Assert.Contains(sample.content.id.value.ToString(), reason)
        | Error error -> Assert.Fail(error.describe)
        | Ok _ -> Assert.Fail("An action absent from the row must be refused.")
        Assert.Empty fixture.operations
        Assert.Empty(fixture.context.imports.proxy.heldEntriesOf CatalogueIds.builtInCatalogue |> expectImportOk)
        assertConfirmationKitUnchanged fixture

    [<Fact>]
    let ``confirming the kit's defaults writes nothing, and a re-binding is answered for the session`` () : unit =
        let fixture = freshConfirmationFixture ()
        let choices = fixture.context.resolveSnapshots () |> expectSnapshotOk |> List.map _.byDefault
        let outcome = fixture.context.confirm choices |> expectImportOk
        Assert.Empty outcome.rebindings
        Assert.Empty outcome.forks
        Assert.Empty fixture.operations
        for envelope in fixture.snapshots do
            Assert.Equal<HeldEntry option>(None, fixture.context.imports.proxy.tryHeldEntry envelope.content.id |> expectImportOk)
        let emptyOutcome = fixture.context.confirm [] |> expectImportOk
        Assert.Equal<ImportOutcome>(outcome, emptyOutcome)
        Assert.Empty fixture.operations
        assertConfirmationKitUnchanged fixture

        let incoming = fixture.original.context.imports.sealEntry (collidingResolutionContent ()) |> expectImportOk
        fixture.original.context.kit.proxy.writeSnapshot incoming |> expectKitOk
        let builtIn = BuiltinCatalogue.freshStores ()
        let heldBefore = builtIn.proxy.heldEntriesOf CatalogueIds.builtInCatalogue |> expectImportOk
        let reboundFixture = openConfirmationFixture fixture.original builtIn.proxy
        let silicon = catalogueEnvelope MaterialIds.silicon.toLibraryEntryId
        let rebindingChoices =
            reboundFixture.context.resolveSnapshots () |> expectSnapshotOk
            |> List.map (fun row ->
                if row.entryId = incoming.content.id then
                    {
                        decision = row
                        action = RebindToHeldEntry silicon.content.id
                    }
                else row.byDefault)
        let rebound = reboundFixture.context.confirm rebindingChoices |> expectImportOk
        let expected : EntryRebinding =
            {
                incoming = incoming.content.id
                held = silicon.content.id
            }
        Assert.Equal<EntryRebinding list>([ expected ], rebound.rebindings)
        Assert.Empty rebound.forks
        Assert.Empty reboundFixture.operations
        Assert.Equal<HeldEntry option>(None, builtIn.proxy.tryHeldEntry incoming.content.id |> expectImportOk)
        Assert.Equal<HeldEntry list>(heldBefore, builtIn.proxy.heldEntriesOf CatalogueIds.builtInCatalogue |> expectImportOk)
        Assert.Empty(builtIn.proxy.heldUserEntries () |> expectImportOk)
        Assert.Empty(builtIn.proxy.loggedImports () |> expectImportOk)
        let overlay = reboundFixture.context.openOverlay rebound.rebindings |> expectSnapshotOk
        let replacement = Assert.Single overlay.replacements
        Assert.Equal<LibraryEntryId>(incoming.content.id, replacement.incoming)
        Assert.Equal<LibraryEntryEnvelope>(silicon, replacement.held)
        Assert.Empty reboundFixture.operations
        assertConfirmationKitUnchanged reboundFixture

    [<Fact>]
    let ``once added, a kit's entries resolve as reused`` () : unit =
        let fixture = freshConfirmationFixture ()
        let choices =
            fixture.context.resolveSnapshots () |> expectSnapshotOk |> List.rev
            |> List.map (fun row ->
                {
                    decision = row
                    action = AddIncomingEntry
                })
        let versions =
            choices |> List.map (fun choice ->
                choice.decision.incomingOpt
                |> Option.defaultWith (fun () -> failwith "Every kit row must have its snapshot.")
                |> fun envelope -> envelope.libraryVersionRef)
        let outcome = fixture.context.confirm choices |> expectImportOk
        Assert.Empty outcome.rebindings
        Assert.Empty outcome.forks
        let operations = List.ofSeq fixture.operations
        let writes = operations |> List.take versions.Length
        Assert.Equal<Set<KitConfirmationOperation>>(versions |> List.map WriteConfirmedVersion |> Set.ofList, Set.ofList writes)
        Assert.Equal<KitConfirmationOperation list>(versions |> List.map PinConfirmedVersion, operations |> List.skip versions.Length)
        let rows = fixture.context.resolveSnapshots () |> expectSnapshotOk
        Assert.Equal<LibraryEntryId list>(fixture.snapshots |> List.map (fun envelope -> envelope.content.id), rows |> List.map _.entryId)
        for row in rows do
            match row.status with
            | SameContentHash (incoming, held) ->
                Assert.Equal<LibraryEntryId>(incoming.content.id, held.id)
                Assert.Equal<VersionNumber>(incoming.content.version, held.latest)
                Assert.Equal<LibraryEntryEnvelope option>(
                    Some incoming, fixture.context.imports.proxy.tryHeldEnvelope incoming.content.id incoming.content.version |> expectImportOk)
            | _ -> Assert.Fail("Every confirmed snapshot must now resolve as reused.")
            Assert.Equal<ImportAction>(ReuseHeldEntry, row.defaultAction)
        fixture.context.confirm (rows |> List.map _.byDefault) |> expectImportOk |> ignore
        Assert.Equal<KitConfirmationOperation list>(operations, List.ofSeq fixture.operations)
        assertConfirmationKitUnchanged fixture

        let failedVersion = versions |> List.item 1
        let pinFailure = ImportStoreUnavailableErr "the second confirmed version cannot be pinned"
        let empty = CatalogueImportProxy.createInMemory ()
        let failingProxy =
            {
                empty.proxy with
                    pinEntryVersion = fun id version ->
                        if id = failedVersion.entryId && version = failedVersion.version then Error pinFailure
                        else empty.proxy.pinEntryVersion id version
            }
        let failedFixture = openConfirmationFixture fixture.original failingProxy
        Assert.Equal<Result<ImportOutcome, CatalogueImportError>>(Error pinFailure, failedFixture.context.confirm choices)
        Assert.Equal<KitConfirmationOperation list>(
            writes @ (versions |> List.take 2 |> List.map PinConfirmedVersion), List.ofSeq failedFixture.operations)
        for envelope in fixture.snapshots do
            Assert.Equal<LibraryEntryEnvelope option>(
                Some envelope, empty.proxy.tryHeldEnvelope envelope.content.id envelope.content.version |> expectImportOk)
        assertConfirmationKitUnchanged failedFixture

    [<Fact>]
    let ``recording an experiment over a setup revision the kit holds writes its file and snapshots nothing of its own`` () : unit =
        use database = BuiltinCatalogue.freshLibraryDatabase ()
        let fixture = snapshotFixture (CatalogueImportProxy.createInDatabase database) ReadWriteKitAccess
        let newerSample =
            {
                (sampleEnvelope ()).content with version = VersionNumber 2
            }
        BuiltinCatalogue.putSealedEntries fixture.context.imports [ newerSample ]
        let latest = fixture.context.imports.sealEntry newerSample |> expectImportOk
        // The setup binds the multilayer at its latest; the experiment adds nothing to what `recordSetup` wrote.
        fixture.context.recordSetup recordingSetup |> expectSnapshotOk
        let expected =
            recordingClosure () |> List.map (fun envelope -> if envelope.content.id = latest.content.id then latest else envelope)
        assertSnapshots expected fixture
        let context, writes = observeContentWrites expected fixture
        context.recordExperiment recordingExperiment |> expectSnapshotOk
        Assert.Equal<KitFileKind list>([ ExperimentKitFile ], List.ofSeq writes)
        assertSnapshots expected fixture
        Assert.Equal<Experiment list>([ recordingExperiment ], context.kit.proxy.readExperiments () |> expectKitOk)

        context.recordExperiment recordingExperiment |> expectSnapshotOk
        Assert.Equal<KitFileKind list>([ ExperimentKitFile; ExperimentKitFile ], List.ofSeq writes)
        Assert.Equal<Experiment list>([ recordingExperiment ], context.kit.proxy.readExperiments () |> expectKitOk)

        let refused = freshSnapshotFixture ()
        refused.context.recordSetup recordingSetup |> expectSnapshotOk
        let expected = recordingClosure ()
        let writeError = KitIoErr "experiment content cannot be written"
        let failingContext = refusingContentWrites expected writeError refused
        Assert.Equal<Result<unit, KitLibraryError>>(
            Error (KitLibraryKitErr writeError), failingContext.recordExperiment recordingExperiment)
        assertSnapshots expected refused
        Assert.Empty(refused.context.kit.proxy.readExperiments () |> expectKitOk)
        refused.context.recordExperiment recordingExperiment |> expectSnapshotOk
        Assert.Equal<LibraryVersionRef list>(expected |> List.map _.libraryVersionRef, List.ofSeq refused.pins)
        Assert.Equal<Experiment list>([ recordingExperiment ], refused.context.kit.proxy.readExperiments () |> expectKitOk)

    [<Fact>]
    let ``recording a setup snapshots the entries its slots bind in every revision, then writes its file`` () : unit =
        use database = BuiltinCatalogue.freshLibraryDatabase ()
        let fixture = snapshotFixture (CatalogueImportProxy.createInDatabase database) ReadWriteKitAccess
        let newerSample =
            {
                (sampleEnvelope ()).content with version = VersionNumber 2
            }
        BuiltinCatalogue.putSealedEntries fixture.context.imports [ newerSample ]
        let latest = fixture.context.imports.sealEntry newerSample |> expectImportOk
        // The plate's material and category are the multilayer's, so its own snapshot is the only one it adds.
        let plate = catalogueEnvelope SampleIds.glassPlate1mmId.toLibraryEntryId
        let expected =
            (recordingClosure ()
             |> List.map (fun envelope -> if envelope.content.id = latest.content.id then latest else envelope))
            @ [ plate ]
        let firstRevision : Setups.SetupRevisionRecord =
            {
                revision = SetupRevision.first
                slots = [ recordedSlot (ElementId "recorded-plate") CatalogueKind.Sample (Some SampleIds.glassPlate1mmId.toElementValueId) ]
            }
        let setup =
            {
                recordingSetup with
                    revision = SetupRevision 2
                    history = [ firstRevision ]
            }
        let context, writes = observeContentWrites expected fixture
        context.recordSetup setup |> expectSnapshotOk
        Assert.Equal<KitFileKind list>([ SetupKitFile ], List.ofSeq writes)
        assertSnapshots expected fixture
        Assert.Equal<Setups.Setup list>([ setup ], context.kit.proxy.readSetups () |> expectKitOk)

        context.recordSetup setup |> expectSnapshotOk
        Assert.Equal<KitFileKind list>([ SetupKitFile; SetupKitFile ], List.ofSeq writes)
        Assert.Equal<Setups.Setup list>([ setup ], context.kit.proxy.readSetups () |> expectKitOk)

        let refused = freshSnapshotFixture ()
        let writeError = KitIoErr "setup content cannot be written"
        let refusedExpected = recordingClosure () @ [ plate ]
        let failingContext = refusingContentWrites refusedExpected writeError refused
        Assert.Equal<Result<unit, KitLibraryError>>(
            Error (KitLibraryKitErr writeError), failingContext.recordSetup setup)
        assertSnapshots refusedExpected refused
        Assert.Empty(refused.context.kit.proxy.readSetups () |> expectKitOk)
        refused.context.recordSetup setup |> expectSnapshotOk
        Assert.Equal<LibraryVersionRef list>(refusedExpected |> List.map _.libraryVersionRef, List.ofSeq refused.pins)
        Assert.Equal<Setups.Setup list>([ setup ], refused.context.kit.proxy.readSetups () |> expectKitOk)

    [<Fact>]
    let ``a reference the library and the kit cannot resolve records nothing`` () : unit =
        let missingValue = ElementValueId "14900000-0000-4000-8000-000000000001"
        let missingId = missingValue.tryToLibraryEntryId |> Option.defaultWith (fun () -> failwith "The test binding must be a GUID.")
        let missingSlot =
            let source = List.head recordingSetup.slots
            {
                source with
                    placement =
                        {
                            source.placement with valueId = Some missingValue
                        }
            }
        let setup =
            {
                recordingSetup with slots = [ missingSlot ]
            }
        let fixture = freshSnapshotFixture ()
        let paths = fixture.folder.paths ()
        match fixture.context.recordSetup setup with
        | Error (UnheldEntryErr reason) -> Assert.Contains(missingId.value.ToString(), reason)
        | Error error -> Assert.Fail(error.describe)
        | Ok () -> Assert.Fail("An unresolved reference must prevent the content write.")
        // The setup was not recorded, so an experiment over it records nothing either.
        match fixture.context.recordExperiment recordingExperiment with
        | Error (KitLibrarySetupErr (UnknownSetupErr reason)) -> Assert.Contains(recordingSetup.reference.describe, reason)
        | Error error -> Assert.Fail(error.describe)
        | Ok () -> Assert.Fail("An experiment over a setup the kit lacks must record nothing.")
        Assert.Empty(fixture.context.kit.proxy.readExperiments () |> expectKitOk)
        Assert.Empty(fixture.context.kit.proxy.readSetups () |> expectKitOk)
        assertSnapshots [] fixture
        Assert.Empty fixture.pins
        Assert.Empty fixture.operations
        Assert.Equal<KitRelativePath list>(paths, fixture.folder.paths ())

        let partialSetup =
            {
                recordingSetup with slots = recordingSetup.slots @ [ missingSlot ]
            }
        let partial = freshSnapshotFixture ()
        match partial.context.recordSetup partialSetup with
        | Error (UnheldEntryErr reason) -> Assert.Contains(missingId.value.ToString(), reason)
        | Error error -> Assert.Fail(error.describe)
        | Ok () -> Assert.Fail("An unresolved later reference must also prevent the content write.")
        assertSnapshots (recordingClosure ()) partial
        Assert.Equal<LibraryVersionRef list>(recordingClosure () |> List.map _.libraryVersionRef, List.ofSeq partial.pins)
        Assert.Empty(partial.context.kit.proxy.readExperiments () |> expectKitOk)
        Assert.Empty(partial.context.kit.proxy.readSetups () |> expectKitOk)

    [<Fact>]
    let ``a read-only kit records nothing`` () : unit =
        let stores = BuiltinCatalogue.freshStores ()
        let writable = snapshotFixture stores.proxy ReadWriteKitAccess
        writable.context.recordSetup recordingSetup |> expectSnapshotOk
        let snapshots = writable.context.kit.proxy.listSnapshots () |> expectKitOk
        // The same folder reopened read-only: its setup is held, so recording an experiment over it reaches the write.
        let readOnlyKit =
            KitContext.openKit writable.context.kit.proxy ReadOnlyKitAccess writable.context.kit.clock |> expectKitOk
        let fixture =
            {
                writable with
                    context =
                        {
                            writable.context with kit = readOnlyKit
                        }
            }
        fixture.pins.Clear()
        fixture.operations.Clear()
        let paths = fixture.folder.paths ()
        let emptySetup =
            {
                recordingSetup with slots = []
            }
        let writes =
            [
                (fun () -> fixture.context.recordExperiment recordingExperiment)
                (fun () -> fixture.context.recordSetup recordingSetup)
                (fun () -> fixture.context.recordSetup emptySetup)
            ]
        for write in writes do
            match write () with
            | Error (KitLibraryKitErr (KitReadOnlyErr reason)) -> Assert.False(String.IsNullOrWhiteSpace reason)
            | Error error -> Assert.Fail(error.describe)
            | Ok () -> Assert.Fail("A read-only kit must refuse recording even without references.")
            Assert.Empty fixture.pins
            Assert.Empty fixture.operations
            Assert.Equal<KitRelativePath list>(paths, fixture.folder.paths ())
            Assert.Equal<LibraryEntryEnvelope list>(snapshots, fixture.context.kit.proxy.listSnapshots () |> expectKitOk)
            Assert.Empty(fixture.context.kit.proxy.readExperiments () |> expectKitOk)
            Assert.Equal<Setups.Setup list>([ recordingSetup ], fixture.context.kit.proxy.readSetups () |> expectKitOk)

    [<Fact>]
    let ``an experiment over a setup revision the kit lacks records nothing`` () : unit =
        let fixture = freshSnapshotFixture ()
        fixture.context.recordSetup recordingSetup |> expectSnapshotOk
        let snapshots = fixture.context.kit.proxy.listSnapshots () |> expectKitOk
        fixture.pins.Clear()
        fixture.operations.Clear()
        let paths = fixture.folder.paths ()
        let missing : SetupReference =
            {
                recordingSetup.reference with revision = SetupRevision 2
            }
        let experiment =
            {
                recordingExperiment with setup = missing
            }

        match fixture.context.recordExperiment experiment with
        | Error (KitLibrarySetupErr (UnknownSetupRevisionErr reason)) -> Assert.Contains(missing.describe, reason)
        | Error error -> Assert.Fail($"Expected KitLibrarySetupErr (UnknownSetupRevisionErr _), got {error.describe}")
        | Ok () -> Assert.Fail("An experiment over a revision the kit lacks must record nothing.")

        Assert.Empty(fixture.context.kit.proxy.readExperiments () |> expectKitOk)
        Assert.Equal<Setups.Setup list>([ recordingSetup ], fixture.context.kit.proxy.readSetups () |> expectKitOk)
        Assert.Equal<LibraryEntryEnvelope list>(snapshots, fixture.context.kit.proxy.listSnapshots () |> expectKitOk)
        Assert.Empty fixture.pins
        Assert.Empty fixture.operations
        Assert.Equal<KitRelativePath list>(paths, fixture.folder.paths ())

    let private presetEntryId (preset : ElementValueId) : LibraryEntryId =
        preset.tryToLibraryEntryId |> Option.defaultWith (fun () -> failwith $"The Built-in preset '{preset.value}' must have an entry id.")

    /// The Built-in catalogue's envelope of the shipped item model's version (spec 0050 S7.1).
    let private itemModelEnvelope (id : ItemModelId) : LibraryEntryEnvelope =
        catalogueEnvelope id.toLibraryEntryId

    let private cpl1Item : Item =
        let draft : ItemDraft =
            {
                id = ItemId (Guid.Parse "21900000-0000-4000-8000-000000000001")
                label = ItemLabel "CPL1"
                model = LibraryModelChoice (BuiltinCatalogue.itemModel ItemModelIds.circularPolarizerModelId)
                dial = DialMounted DialPositive
            }
        draft.toItem

    let private qzDraft : ItemDraft =
        {
            id = ItemId (Guid.Parse "21900000-0000-4000-8000-000000000002")
            label = ItemLabel "QZ"
            model =
                StackSampleChoice (
                    BuiltinCatalogue.itemModel ItemModelIds.stackSampleItemModelId,
                    SampleVersionId.firstOf SampleIds.glassPlate1mmId,
                    BuiltinCatalogue.sample SampleIds.glassPlate1mmId
                )
            dial = DialMounted DialPositive
        }

    let private qzItem : Item = qzDraft.toItem

    let private laserItem : Item =
        let draft : ItemDraft =
            {
                id = ItemId (Guid.Parse "21900000-0000-4000-8000-000000000003")
                label = ItemLabel "Laser"
                model = SourcePresetChoice (BuiltinCatalogue.itemModel ItemModelIds.sourcePresetItemModelId, BuiltinCatalogue.source PresetIds.heNe633)
                dial = FixedMounting
            }
        draft.toItem

    let private lp1Item : Item =
        let polarizer = (catalogueEnvelope (presetEntryId PresetIds.polarizerLp)).content.toPolarizerPreset |> expectOverlayStoreOk
        let draft : ItemDraft =
            {
                id = ItemId (Guid.Parse "21900000-0000-4000-8000-000000000004")
                label = ItemLabel "LP1"
                model = PolarizerPresetChoice (BuiltinCatalogue.itemModel ItemModelIds.idealPolarizerItemModelId, polarizer)
                dial = DialMounted DialNegative
            }
        draft.toItem

    /// Reopens the kit read-only against an empty library: it reads its items back and resolves each one's model from
    /// its `.item-model-json` snapshot, then its bound physics from the kit's snapshots (spec 0050 S7.1, S4.2).
    let private assertItemsReopenAgainstEmptyLibrary (fixture : KitLibraryFixture) (items : Item list) : unit =
        let empty = CatalogueImportProxy.createInMemory ()
        let opened =
            KitContext.openKit (KitProxy.createInMemory fixture.folder) ReadOnlyKitAccess fixture.context.kit.clock |> expectKitOk
        let context : KitLibraryContext =
            {
                kit = opened
                imports = CatalogueImportContext.create empty.proxy
            }
        Assert.Equal<Item list>(items, opened.proxy.readItems () |> expectKitOk)
        let overlay = context.openOverlay [] |> expectSnapshotOk
        let session = (storesForOverlay empty).overlaidWith overlay
        let itemModels = empty.itemModels.overlaidWith overlay
        for item in items do
            let modelVersion =
                match item.model with
                | PresetInstance (model, _)
                | SampleInstance (model, _)
                | ItemModelVersionInstance model -> model
            let shipped = BuiltinCatalogue.itemModel modelVersion.itemModelId
            let snapshot =
                overlay.snapshots
                |> List.filter (fun envelope -> EntryVersionReference envelope.libraryVersionRef = modelVersion.entryReference)
                |> Assert.Single
            Assert.Equal<ItemModelVersionId>(shipped.versionId, modelVersion)
            Assert.Equal<LibraryEntryKind>(ItemModelEntryKind, snapshot.content.schema.kind)
            Assert.Equal<VersionNumber>(shipped.version, snapshot.content.version)
            Assert.Equal<ItemModelEntry>(shipped.entry, snapshot.content.toItemModel |> expectOverlayStoreOk)
            Assert.Equal<Result<ItemModelVersion option, ItemModelError>>(Ok (Some shipped), itemModels.resolveItemModelVersion modelVersion)
            match item.model with
            | PresetInstance (_, preset) ->
                Assert.Equal<Result<LibraryEntry option, LibraryError>>(
                    Ok (Some (BuiltinCatalogue.preset preset)), session.library.tryGetEntry preset)
            | SampleInstance (_, version) ->
                let sample = BuiltinCatalogue.sample version.sampleId
                Assert.Equal<Result<Sample option, SampleError>>(Ok (Some sample), session.samples.resolveVersion version)
                let categories = session.categories.listCategories () |> expectOverlayStoreOk
                for materialVersion in sample.structure.referencedMaterialVersions do
                    let material = BuiltinCatalogue.material materialVersion.materialId
                    Assert.Equal<Result<MaterialEntry option, MaterialError>>(
                        Ok (Some material), session.materials.resolveVersion materialVersion)
                    Assert.Contains(BuiltinCatalogue.category material.category, categories)
            | ItemModelVersionInstance _ -> ()
        Assert.Empty(empty.proxy.heldUserEntries () |> expectImportOk)

    [<Fact>]
    let ``recording an item snapshots its model's library version and pins it, then writes its file`` () : unit =
        let fixture = freshSnapshotFixture ()
        let model = itemModelEnvelope ItemModelIds.circularPolarizerModelId
        let expected = [ model ]
        Assert.Equal<VersionNumber>(VersionNumber.first, model.content.version)
        Assert.Equal<EntryReference list>([ EntryVersionReference model.libraryVersionRef ], cpl1Item.libraryReferences)
        let snapshotPath = KitRelativePath $"library/{ItemModelIds.circularPolarizerModelId.value:D}@v1.item-model-json"
        let itemPath = KitRelativePath "CPL1.item-json"
        Assert.Equal<KitRelativePath>(snapshotPath, model.snapshotPath)
        let before = fixture.folder.paths ()
        let context, writes = observeContentWrites expected fixture
        context.recordItem cpl1Item |> expectSnapshotOk
        Assert.Equal<KitFileKind list>([ ItemKitFile ], List.ofSeq writes)
        assertSnapshots expected fixture
        Assert.Equal<LibraryVersionRef list>([ model.libraryVersionRef ], List.ofSeq fixture.pins)
        Assert.Equal<KitRelativePath list>([ itemPath; snapshotPath ], fixture.folder.paths () |> List.except before)
        Assert.Equal<Item list>([ cpl1Item ], context.kit.proxy.readItems () |> expectKitOk)

        // Recording it again rewrites its file and snapshots and pins nothing more.
        context.recordItem cpl1Item |> expectSnapshotOk
        Assert.Equal<KitFileKind list>([ ItemKitFile; ItemKitFile ], List.ofSeq writes)
        Assert.Equal<LibraryVersionRef list>([ model.libraryVersionRef ], List.ofSeq fixture.pins)
        Assert.Equal<Item list>([ cpl1Item ], context.kit.proxy.readItems () |> expectKitOk)
        assertItemsReopenAgainstEmptyLibrary fixture [ cpl1Item ]

        // A refused file write keeps the snapshot it follows and writes no item.
        let refused = freshSnapshotFixture ()
        let writeError = KitIoErr "item content cannot be written"
        let failingContext = refusingContentWrites expected writeError refused
        Assert.Equal<Result<unit, KitLibraryError>>(Error (KitLibraryKitErr writeError), failingContext.recordItem cpl1Item)
        assertSnapshots expected refused
        Assert.Empty(refused.context.kit.proxy.readItems () |> expectKitOk)
        refused.context.recordItem cpl1Item |> expectSnapshotOk
        Assert.Equal<LibraryVersionRef list>([ model.libraryVersionRef ], List.ofSeq refused.pins)
        Assert.Equal<Item list>([ cpl1Item ], refused.context.kit.proxy.readItems () |> expectKitOk)

    [<Fact>]
    let ``recording a sample item snapshots the sample with its materials and their categories`` () : unit =
        let fixture = freshSnapshotFixture ()
        let sample = catalogueEnvelope SampleIds.glassPlate1mmId.toLibraryEntryId
        let expected =
            [
                itemModelEnvelope ItemModelIds.stackSampleItemModelId
                sample
                catalogueEnvelope MaterialIds.glass152.toLibraryEntryId
                catalogueEnvelope CategoryIds.glass.toLibraryEntryId
            ]
        Assert.Equal<VersionNumber>(VersionNumber.first, sample.content.version)
        let context, writes = observeContentWrites expected fixture
        context.recordItem qzItem |> expectSnapshotOk
        Assert.Equal<KitFileKind list>([ ItemKitFile ], List.ofSeq writes)
        assertSnapshots expected fixture
        Assert.Equal<Item list>([ qzItem ], context.kit.proxy.readItems () |> expectKitOk)

        // A second item over the same sample version finds its whole closure held, so each stays snapshotted once.
        let qz2Draft : ItemDraft =
            {
                qzDraft with
                    id = ItemId (Guid.Parse "21900000-0000-4000-8000-000000000005")
                    label = ItemLabel "QZ2"
            }
        let qz2 = qz2Draft.toItem
        context.recordItem qz2 |> expectSnapshotOk
        Assert.Equal<KitFileKind list>([ ItemKitFile; ItemKitFile ], List.ofSeq writes)
        assertSnapshots expected fixture
        Assert.Equal<LibraryVersionRef list>(expected |> List.map _.libraryVersionRef, List.ofSeq fixture.pins)
        assertItemsReopenAgainstEmptyLibrary fixture [ qzItem; qz2 ]

    [<Fact>]
    let ``recording a preset item snapshots the preset's latest version`` () : unit =
        let fixture = freshSnapshotFixture ()
        let heNeId = presetEntryId PresetIds.heNe633
        let heNe = catalogueEnvelope heNeId
        let laserModel = itemModelEnvelope ItemModelIds.sourcePresetItemModelId
        let laserExpected = [ laserModel; heNe ]
        Assert.Equal<VersionNumber>(VersionNumber.first, heNe.content.version)
        Assert.Equal<EntryReference list>(
            [ EntryVersionReference laserModel.libraryVersionRef; EntryLatestReference heNeId ], laserItem.libraryReferences)
        let laserContext, laserWrites = observeContentWrites laserExpected fixture
        laserContext.recordItem laserItem |> expectSnapshotOk
        Assert.Equal<KitFileKind list>([ ItemKitFile ], List.ofSeq laserWrites)
        assertSnapshots laserExpected fixture
        Assert.Equal<Item list>([ laserItem ], laserContext.kit.proxy.readItems () |> expectKitOk)

        // An ideal polarizer binds its preset the same way, at the library's latest version.
        let polarizerId = presetEntryId PresetIds.polarizerLp
        let expected = laserExpected @ [ itemModelEnvelope ItemModelIds.idealPolarizerItemModelId; catalogueEnvelope polarizerId ]
        let context, writes = observeContentWrites expected fixture
        context.recordItem lp1Item |> expectSnapshotOk
        Assert.Equal<KitFileKind list>([ ItemKitFile ], List.ofSeq writes)
        assertSnapshots expected fixture
        assertItemsReopenAgainstEmptyLibrary fixture [ lp1Item; laserItem ]

        // A library holding the preset but not the model refuses the item: the preset never stands in for its model.
        let presetOnly = CatalogueImportProxy.createInMemory ()
        BuiltinCatalogue.putSealedEntries (CatalogueImportContext.create presetOnly.proxy) [ heNe.content ]
        Assert.Equal<Result<LibraryEntryEnvelope option, CatalogueImportError>>(Ok (Some heNe), presetOnly.proxy.tryLatestEnvelope heNeId)
        let unmodelled = snapshotFixture presetOnly.proxy ReadWriteKitAccess
        let paths = unmodelled.folder.paths ()
        match unmodelled.context.recordItem laserItem with
        | Error (UnheldEntryErr reason) -> Assert.Contains(ItemModelIds.sourcePresetItemModelId.value.ToString(), reason)
        | Error error -> Assert.Fail(error.describe)
        | Ok () -> Assert.Fail("An item whose model neither the library nor the kit holds must record nothing.")
        assertSnapshots [] unmodelled
        Assert.Empty unmodelled.pins
        Assert.Empty unmodelled.operations
        Assert.Equal<KitRelativePath list>(paths, unmodelled.folder.paths ())
        Assert.Empty(unmodelled.context.kit.proxy.readItems () |> expectKitOk)

    [<Fact>]
    let ``a read-only kit records no item`` () : unit =
        let fixture = snapshotFixture (BuiltinCatalogue.freshStores ()).proxy ReadOnlyKitAccess
        let paths = fixture.folder.paths ()
        for item in [ cpl1Item; qzItem; laserItem; lp1Item ] do
            match fixture.context.recordItem item with
            | Error (KitLibraryKitErr (KitReadOnlyErr reason)) -> Assert.False(String.IsNullOrWhiteSpace reason)
            | Error error -> Assert.Fail(error.describe)
            | Ok () -> Assert.Fail($"A read-only kit must refuse recording '{item.label.value}'.")
            Assert.Empty fixture.pins
            Assert.Empty fixture.operations
            Assert.Equal<KitRelativePath list>(paths, fixture.folder.paths ())
            assertSnapshots [] fixture
            Assert.Empty(fixture.context.kit.proxy.readItems () |> expectKitOk)

    /// The item under the new label, alone in its kit (spec 0050 S7.2).
    let private relabelledItem (item : Item) (label : ItemLabel) : Item =
        item.relabelledAmong label [ item ] |> Result.defaultWith (fun error -> failwith error.describe)

    let private itemFilePaths (fixture : KitLibraryFixture) : KitRelativePath list =
        fixture.folder.paths ()
        |> List.filter (fun path -> path.value.EndsWith(ItemKitFile.extension.value, StringComparison.OrdinalIgnoreCase))

    /// The fixture's folder reopened read-only: a relabel through it is refused and leaves every path as it was.
    let private assertReadOnlyRelabelRefused (fixture : KitLibraryFixture) (previous : Item) (relabelled : Item) : unit =
        let readOnly : KitLibraryContext =
            {
                fixture.context with
                    kit =
                        KitContext.openKit (KitProxy.createInMemory fixture.folder) ReadOnlyKitAccess fixture.context.kit.clock
                        |> expectKitOk
            }
        let paths = fixture.folder.paths ()
        match readOnly.recordRelabelledItem previous relabelled with
        | Error (KitLibraryKitErr (KitReadOnlyErr reason)) -> Assert.False(String.IsNullOrWhiteSpace reason)
        | Error error -> Assert.Fail(error.describe)
        | Ok () -> Assert.Fail($"A read-only kit must refuse relabelling '{previous.label.value}'.")
        Assert.Equal<KitRelativePath list>(paths, fixture.folder.paths ())

    [<Fact>]
    let ``a relabelled item moves to the file of its new label`` () : unit =
        let fixture = freshSnapshotFixture ()
        fixture.context.recordItem cpl1Item |> expectSnapshotOk
        let relabelled = relabelledItem cpl1Item (ItemLabel "CPL 1")
        assertReadOnlyRelabelRefused fixture cpl1Item relabelled
        Assert.Equal<Item list>([ cpl1Item ], fixture.context.kit.proxy.readItems () |> expectKitOk)

        fixture.context.recordRelabelledItem cpl1Item relabelled |> expectSnapshotOk
        let paths = fixture.folder.paths ()
        Assert.Contains(KitRelativePath "CPL 1.item-json", paths)
        Assert.DoesNotContain(KitRelativePath "CPL1.item-json", paths)
        Assert.Equal<KitRelativePath list>([ KitRelativePath "CPL 1.item-json" ], itemFilePaths fixture)
        let read = fixture.context.kit.proxy.readItems () |> expectKitOk |> Assert.Single
        Assert.Equal<ItemLabel>(ItemLabel "CPL 1", read.label)
        Assert.Equal<ItemId>(cpl1Item.id, read.id)
        Assert.Equal<Item>(relabelled, read)

    [<Fact>]
    let ``a relabel that changes only the case rewrites the one file`` () : unit =
        let fixture = freshSnapshotFixture ()
        let lower = relabelledItem cpl1Item (ItemLabel "cpl1")
        fixture.context.recordItem lower |> expectSnapshotOk
        Assert.Equal<KitRelativePath list>([ KitRelativePath "cpl1.item-json" ], itemFilePaths fixture)
        let upper = relabelledItem lower (ItemLabel "CPL1")
        assertReadOnlyRelabelRefused fixture lower upper

        fixture.context.recordRelabelledItem lower upper |> expectSnapshotOk
        Assert.Equal<KitRelativePath list>([ KitRelativePath "CPL1.item-json" ], itemFilePaths fixture)
        let read = fixture.context.kit.proxy.readItems () |> expectKitOk |> Assert.Single
        Assert.Equal<ItemLabel>(ItemLabel "CPL1", read.label)
        Assert.Equal<Item>(upper, read)

        // A record that fails once the old file is deleted writes the previous item back under its own label.
        let listError = KitIoErr "the kit's snapshots cannot be listed"
        let failing : KitLibraryContext =
            {
                fixture.context with
                    kit =
                        {
                            fixture.context.kit with
                                proxy =
                                    {
                                        fixture.context.kit.proxy with listSnapshots = fun () -> Error listError
                                    }
                        }
            }
        Assert.Equal<Result<unit, KitLibraryError>>(Error (KitLibraryKitErr listError), failing.recordRelabelledItem upper lower)
        Assert.Equal<KitRelativePath list>([ KitRelativePath "CPL1.item-json" ], itemFilePaths fixture)
        Assert.Equal<Item list>([ upper ], fixture.context.kit.proxy.readItems () |> expectKitOk)

    [<Fact>]
    let ``a relabel whose old file cannot be deleted leaves the item under its old label`` () : unit =
        let fixture = freshSnapshotFixture ()
        fixture.context.recordItem cpl1Item |> expectSnapshotOk
        let proxy = fixture.context.kit.proxy
        let deleteError = KitIoErr "CPL1.item-json is held open by another program"
        let refusing : KitLibraryContext =
            {
                fixture.context with
                    kit =
                        {
                            fixture.context.kit with
                                proxy =
                                    {
                                        proxy with
                                            deleteKindFile = fun kind stem ->
                                                if stem = cpl1Item.label.fileStem then Error deleteError else proxy.deleteKindFile kind stem
                                    }
                        }
            }
        let relabelled = relabelledItem cpl1Item (ItemLabel "CPL 1")

        match refusing.recordRelabelledItem cpl1Item relabelled with
        | Error (KitLibraryKitErr (KitIoErr reason)) -> Assert.Contains("CPL1.item-json", reason)
        | Error error -> Assert.Fail($"Expected KitLibraryKitErr (KitIoErr _), got {error.describe}")
        | Ok () -> Assert.Fail("A relabel whose old file cannot be deleted must not succeed.")

        Assert.Equal<KitRelativePath list>([ KitRelativePath "CPL1.item-json" ], itemFilePaths fixture)
        Assert.Equal<Item list>([ cpl1Item ], proxy.readItems () |> expectKitOk)

    [<Fact>]
    let ``recording an experiment refuses a missing physical item before writing`` () : unit =
        let laser600 : Item =
            let draft : ItemDraft =
                {
                    id = ItemId (Guid.Parse "22700000-0000-4000-8000-000000000041")
                    label = ItemLabel "Laser600"
                    model = SourcePresetChoice (BuiltinCatalogue.itemModel ItemModelIds.sourcePresetItemModelId, BuiltinCatalogue.source PresetIds.source600)
                    dial = FixedMounting
                }
            draft.toItem
        // The setup's source slot holds the piece; the setup file is written without its snapshots.
        let setup =
            {
                recordingSetup with
                    slots =
                        recordingSetup.slots
                        |> List.map (fun slot ->
                            match slot.placement.catalogueKind with
                            | LightSource ->
                                {
                                    slot with content = Setups.ItemInSlot (laser600.id, Setups.PlacementTurn)
                                }
                            | _ -> slot)
            }
        let experiment = recordingExperiment
        match experiment.experimentFile.fileText |> Result.bind (fun text -> text.parseExperimentFile) with
        | Ok parsed -> Assert.Equal<Experiment>(experiment, parsed.experiment)
        | Error error -> Assert.Fail($"The experiment must be valid against its schema: {error.describe}")
        let fixture = freshSnapshotFixture ()
        fixture.context.kit.proxy.writeSetup setup |> expectKitOk
        let paths = fixture.folder.paths ()

        match fixture.context.recordExperiment experiment with
        | Error (KitLibrarySetupErr (SetupItemReferenceErr (UnknownKitItemErr reason))) -> Assert.Contains(laser600.id.value.ToString(), reason)
        | Error error -> Assert.Fail($"Expected KitLibrarySetupErr (SetupItemReferenceErr (UnknownKitItemErr _)), got {error.describe}")
        | Ok () -> Assert.Fail("An experiment naming an item the kit lacks must record nothing.")

        assertSnapshots [] fixture
        Assert.Empty fixture.pins
        Assert.Empty fixture.operations
        Assert.Equal<KitRelativePath list>(paths, fixture.folder.paths ())
        Assert.Empty(fixture.context.kit.proxy.readExperiments () |> expectKitOk)

        // With the piece's file in the kit, the same experiment records, and snapshots nothing of its own: the setup's
        // and the item's entries are theirs to snapshot (spec 0050 S8.2).
        fixture.context.kit.proxy.writeItem laser600 |> expectKitOk
        let context, writes = observeContentWrites [] fixture
        context.recordExperiment experiment |> expectSnapshotOk
        Assert.Equal<KitFileKind list>([ ExperimentKitFile ], List.ofSeq writes)
        assertSnapshots [] fixture
        Assert.Equal<Experiment list>([ experiment ], context.kit.proxy.readExperiments () |> expectKitOk)

    /// A test-local, user-managed item model at its first version (spec 0050 S7.2).
    let private userItemModel (id : ItemModelId) (name : EntryName) (model : ItemModelForm) : ItemModelVersion =
        {
            version = VersionNumber.first
            entry =
                {
                    id = id
                    name = name
                    description = EntryDescription $"{name.value}, kept by this test"
                    model = model
                    protection = UserManaged
                }
        }

    [<Fact>]
    let ``a kit's item models resolve from its snapshots against an empty library`` () : unit =
        let fixture = freshSnapshotFixture ()
        let versionIds : ItemModelVersionId list =
            [ ItemModelIds.retarderPlateModelId; ItemModelIds.circularPolarizerModelId ]
            |> List.map (fun id ->
                {
                    itemModelId = id
                    version = VersionNumber.first
                })
        let expected = versionIds |> List.map (fun versionId -> itemModelEnvelope versionId.itemModelId)
        Assert.Equal<LibraryVersionRef list>(
            expected |> List.map _.libraryVersionRef,
            fixture.context.snapshotReferences (versionIds |> List.map _.entryReference) |> expectSnapshotOk)
        assertSnapshots expected fixture

        let empty = CatalogueImportProxy.createInMemory ()
        let opened =
            KitContext.openKit (KitProxy.createInMemory fixture.folder) ReadOnlyKitAccess fixture.context.kit.clock |> expectKitOk
        let context : KitLibraryContext =
            {
                kit = opened
                imports = CatalogueImportContext.create empty.proxy
            }
        let paths = fixture.folder.paths ()
        let overlay = context.openOverlay [] |> expectSnapshotOk
        let store = ItemModelProxy.createInMemory ()
        let session = store.overlaidWith overlay
        for versionId in versionIds do
            let shipped = BuiltinCatalogue.itemModel versionId.itemModelId
            Assert.Equal<ItemModelVersionId>(versionId, shipped.versionId)
            Assert.Equal<Result<ItemModelVersion option, ItemModelError>>(Ok (Some shipped), session.resolveItemModelVersion versionId)
            Assert.Equal<Result<ItemModelVersion option, ItemModelError>>(
                Ok None, session.resolveItemModelVersion { versionId with version = VersionNumber.first.next })
            Assert.Equal<Result<ItemModelVersion option, ItemModelError>>(Ok None, store.resolveItemModelVersion versionId)
        let circular = BuiltinCatalogue.itemModel ItemModelIds.circularPolarizerModelId
        let retarder = BuiltinCatalogue.itemModel ItemModelIds.retarderPlateModelId
        Assert.True(circular.entry.name < retarder.entry.name)
        Assert.Equal<Result<ItemModelVersion list, ItemModelError>>(Ok [ circular; retarder ], session.listItemModels ())
        Assert.Equal<Result<ItemModelVersion list, ItemModelError>>(Ok [], store.listItemModels ())
        Assert.Equal<KitRelativePath list>(paths, fixture.folder.paths ())
        Assert.Empty(empty.proxy.heldEntriesOf CatalogueIds.builtInCatalogue |> expectImportOk)
        Assert.Empty(empty.proxy.heldUserEntries () |> expectImportOk)

    [<Fact>]
    let ``the overlaid item-model store answers the kit's divergent version and passes a save through`` () : unit =
        let held = BuiltinCatalogue.freshStores ()
        let fixture = snapshotFixture held.proxy ReadWriteKitAccess
        let shipped = BuiltinCatalogue.itemModel ItemModelIds.circularPolarizerModelId
        let content =
            {
                (itemModelEnvelope ItemModelIds.circularPolarizerModelId).content with
                    descriptionOpt = Some (EntryDescription "The circular polarizer as this kit measured it")
            }
        let divergent = fixture.context.imports.sealEntry content |> expectImportOk
        fixture.context.kit.proxy.writeSnapshot divergent |> expectKitOk
        // Both names sort before every Built-in model's and their ids the other way round, so the listing shows the
        // store-first and the by-name rules.
        let kitOnly =
            userItemModel
                (ItemModelId (Guid.Parse "22200000-0000-4000-8000-000000000003"))
                (EntryName "A kit-only wave plate")
                (OpticalElementModel (RetarderPlateModel ConstantRetardance))
        let kitOnlyPolarizer =
            userItemModel
                (ItemModelId (Guid.Parse "22200000-0000-4000-8000-000000000001"))
                (EntryName "B kit-only polarizer")
                (OpticalElementModel (RealPolarizerModel ExtinctionRatioLeakage))
        let snapshotOf (itemModel : ItemModelVersion) : LibraryEntryEnvelope =
            let snapshot =
                LibraryEntryContent.ofItemModel itemModel.version UserProvenance itemModel.entry
                |> expectOverlayStoreOk
                |> fixture.context.imports.sealEntry
                |> expectImportOk
            fixture.context.kit.proxy.writeSnapshot snapshot |> expectKitOk
            snapshot
        let kitOnlySnapshot = snapshotOf kitOnly
        let kitOnlyContent = kitOnlySnapshot.content
        let kitOnlyPolarizerSnapshot = snapshotOf kitOnlyPolarizer
        let overlay = fixture.context.openOverlay [] |> expectSnapshotOk
        let session = held.itemModels.overlaidWith overlay

        let kitModel : ItemModelVersion =
            {
                version = content.version
                entry = content.toItemModel |> expectOverlayStoreOk
            }
        Assert.Equal<ItemModelVersionId>(shipped.versionId, kitModel.versionId)
        Assert.NotEqual<EntryDescription>(shipped.entry.description, kitModel.entry.description)
        Assert.Equal<Result<ItemModelVersion option, ItemModelError>>(Ok (Some kitModel), session.resolveItemModelVersion shipped.versionId)
        Assert.Equal<Result<ItemModelVersion option, ItemModelError>>(Ok (Some shipped), held.itemModels.resolveItemModelVersion shipped.versionId)
        Assert.Equal<Result<ItemModelVersion option, ItemModelError>>(Ok (Some kitOnly), session.resolveItemModelVersion kitOnly.versionId)
        let stored = held.itemModels.listItemModels () |> expectOverlayStoreOk
        Assert.Equal<ItemModelVersion list>(
            BuiltinCatalogue.itemModels () |> List.sortBy (fun itemModel -> itemModel.entry.name, itemModel.entry.id), stored)
        let listed = session.listItemModels () |> expectOverlayStoreOk
        Assert.True(kitOnlyPolarizer.entry.id < kitOnly.entry.id)
        Assert.Equal<ItemModelVersion list>(stored @ [ kitOnly; kitOnlyPolarizer ], listed)
        Assert.Equal<ItemModelVersion>(shipped, listed |> List.filter (fun itemModel -> itemModel.entry.id = shipped.entry.id) |> Assert.Single)

        let saved =
            userItemModel
                (ItemModelId (Guid.Parse "22200000-0000-4000-8000-000000000002"))
                (EntryName "Bench quarter-wave plate")
                (OpticalElementModel (RetarderPlateModel ConstantRetardance))
        Assert.Equal<Result<unit, ItemModelError>>(Ok (), session.saveItemModel saved)
        Assert.Equal<Result<ItemModelVersion option, ItemModelError>>(Ok (Some saved), held.itemModels.resolveItemModelVersion saved.versionId)
        match session.saveItemModel kitModel with
        | Error (DuplicateItemModelErr reason) -> Assert.Contains(string shipped.entry.id.value, reason)
        | Error error -> Assert.Fail(error.describe)
        | Ok () -> Assert.Fail("A save of a held id must be the store's own refusal.")
        Assert.Equal<Result<ItemModelVersion option, ItemModelError>>(Ok (Some shipped), held.itemModels.resolveItemModelVersion shipped.versionId)
        assertSnapshots [ divergent; kitOnlySnapshot; kitOnlyPolarizerSnapshot ] fixture

        // A re-bound kit model resolves to the held entry and leaves the listing (spec 0050 S4.2).
        let rebinding : EntryRebinding =
            {
                incoming = kitOnlyContent.id
                held = shipped.entry.id.toLibraryEntryId
            }
        let rebound = held.itemModels.overlaidWith (fixture.context.openOverlay [ rebinding ] |> expectSnapshotOk)
        Assert.Equal<Result<ItemModelVersion option, ItemModelError>>(Ok (Some shipped), rebound.resolveItemModelVersion kitOnly.versionId)
        Assert.Equal<Result<ItemModelVersion list, ItemModelError>>(
            Ok ((held.itemModels.listItemModels () |> expectOverlayStoreOk) @ [ kitOnlyPolarizer ]), rebound.listItemModels ())

        // A snapshot that is no item model is the store's typed refusal, naming the snapshot.
        let undescribed =
            {
                kitOnlyContent with descriptionOpt = None
            }
        let refusal =
            match undescribed.toItemModel with
            | Error error -> error.describe
            | Ok _ -> failwith "An item model without a description must not decode."
        let invalidOverlay : KitLibraryOverlay =
            {
                snapshots =
                    [
                        {
                            kitOnlySnapshot with content = undescribed
                        }
                    ]
                replacements = []
            }
        let invalid = held.itemModels.overlaidWith invalidOverlay
        let expectedError = ItemModelStoreUnavailableErr $"snapshot {undescribed.describe}: {refusal}"
        Assert.Equal<Result<ItemModelVersion option, ItemModelError>>(Error expectedError, invalid.resolveItemModelVersion kitOnly.versionId)
        Assert.Equal<Result<ItemModelVersion list, ItemModelError>>(Error expectedError, invalid.listItemModels ())

    [<Fact>]
    let ``every kit library error renders its condition and its payload`` () : unit =
        let kitError = KitIoErr "library/snapshot cannot be read"
        let importError = ImportRejectedErr "the requested version cannot be pinned"
        let reason = "entry 13900000-0000-4000-8000-000000000004 v9"
        let itemError = UnknownKitItemErr "'ML' maps onto 22700000-0000-4000-8000-0000000000ff"
        let setupError = UnknownSetupRevisionErr "'Bench' revision 2"
        let cases : (KitLibraryError * string) list =
            [
                KitLibraryKitErr kitError, $"the kit refused the library snapshot: {kitError.describe}"
                KitLibraryImportErr importError, $"the library refused the kit snapshot: {importError.describe}"
                UnheldEntryErr reason, $"neither the library nor the kit holds the reference: {reason}"
                KitLibraryItemErr itemError, $"the items could not be copied: {itemError.describe}"
                KitLibrarySetupErr setupError, $"the setup could not be used: {setupError.describe}"
            ]
        for error, expected in cases do
            Assert.Equal<string>(expected, error.describe)
        Assert.Contains("'Bench' revision 2", (KitLibrarySetupErr setupError).describe)
        Assert.Equal(5, cases |> List.map (fun (error, _) -> error.describe) |> List.distinct |> List.length)
