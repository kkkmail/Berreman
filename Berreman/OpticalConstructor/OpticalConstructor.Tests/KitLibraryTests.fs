namespace OpticalConstructor.Tests

open System
open Xunit
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.Lifecycle
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.CatalogueImport
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.KitFolders
open OpticalConstructor.Domain.KitLibrary
open OpticalConstructor.Storage.CatalogueImportStore
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
                | UnheldEntryErr _ -> failwith "Only pin and write failures belong to this fixture."
            Assert.Equal<Result<LibraryVersionRef list, KitLibraryError>>(Error failure, failingContext.snapshotReferences [ reference ])
            assertSnapshots [ sampleEnvelope () ] partial
            let expectedPins =
                match failure with
                | KitLibraryImportErr _ -> [ (sampleEnvelope ()).libraryVersionRef ]
                | KitLibraryKitErr _ -> [ (sampleEnvelope ()).libraryVersionRef; material.libraryVersionRef ]
                | UnheldEntryErr _ -> []
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
    let ``every kit library error renders its condition and its payload`` () : unit =
        let kitError = KitIoErr "library/snapshot cannot be read"
        let importError = ImportRejectedErr "the requested version cannot be pinned"
        let reason = "entry 13900000-0000-4000-8000-000000000004 v9"
        let cases : (KitLibraryError * string) list =
            [
                KitLibraryKitErr kitError, $"the kit refused the library snapshot: {kitError.describe}"
                KitLibraryImportErr importError, $"the library refused the kit snapshot: {importError.describe}"
                UnheldEntryErr reason, $"neither the library nor the kit holds the reference: {reason}"
            ]
        for error, expected in cases do
            Assert.Equal<string>(expected, error.describe)
        Assert.Equal(3, cases |> List.map (fun (error, _) -> error.describe) |> List.distinct |> List.length)
