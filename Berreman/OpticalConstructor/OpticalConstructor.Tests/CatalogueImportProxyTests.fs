namespace OpticalConstructor.Tests

open System
open Xunit
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.Lifecycle
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.CatalogueFiles
open OpticalConstructor.Domain.CatalogueImport
open OpticalConstructor.Storage.CatalogueFolderStore
open OpticalConstructor.Storage.CatalogueImportStore
open OpticalConstructor.Storage.EntryFileJson

module internal CatalogueImportTestData =

    let decodeImportEntry (result : Result<'Entry, LibraryEntryError>) : 'Entry =
        result |> Result.defaultWith (fun error -> failwith error.describe)

    let readImportCatalogue () : CatalogueData =
        (CatalogueProxy.createBuiltIn ()).readCatalogue ()
        |> Result.defaultWith (fun error -> failwith error.describe)

    let expectImportOk (result : Result<'Value, CatalogueImportError>) : 'Value =
        result |> Result.defaultWith (fun error -> failwith error.describe)

/// Pins explicit confirmation, envelope writes and failure short-circuiting (spec 0050 S3.1).
module CatalogueImportProxyTests =

    open CatalogueImportTestData

    type private ImportWrite =
        | PutVersion of EntryVersionPut
        | RetireEntry of LibraryEntryId

    let private recordingProxy (tape : ResizeArray<ImportWrite>) (refuse : EntryVersionPut -> bool) : CatalogueImportProxy =
        {
            tryHeldEntry = fun _ -> Ok None
            tryHeldEnvelope = fun _ _ -> Ok None
            heldEntriesNamed = fun _ _ -> Ok []
            heldEntriesOf = fun _ -> Ok []
            heldUserEntries = fun () -> Ok []
            pinEntryVersion = fun _ _ -> Ok ()
            putEntryVersion = fun put ->
                if refuse put then Error (ImportRejectedErr "the stub rejects every material")
                else
                    tape.Add(PutVersion put)
                    Ok ()
            retireEntry = fun id -> tape.Add(RetireEntry id); Ok ()
            loggedImports = fun () -> Ok []
            logImport = fun _ -> Ok ()
            forgetImport = fun _ -> Ok ()
        }

    let private forkId = LibraryEntryId (Guid.Parse "121b9a30-fd22-4b18-b22b-bd7c07fd42c5")

    let private importContext (proxy : CatalogueImportProxy) : CatalogueImportContext =
        {
            CatalogueImportContext.create proxy with
                newEntryId = fun () -> forkId
        }

    let private expectedPut (envelope : LibraryEntryEnvelope) : ImportWrite =
        let put =
            {
                envelope = envelope
                latest = MoveLatestVersion
            }
        PutVersion put

    let private expectedPuts (catalogue : CatalogueData) : ImportWrite list =
        [ CategoryEntryKind; MaterialEntryKind; SampleEntryKind; SourceEntryKind; PolarizerEntryKind; DetectorEntryKind ]
        |> List.collect (fun kind -> catalogue.envelopes |> List.filter (fun envelope -> envelope.content.schema.kind = kind))
        |> List.map expectedPut

    let private heldEntry (envelope : LibraryEntryEnvelope) : HeldEntry =
        {
            id = envelope.content.id
            kind = envelope.content.schema.kind
            name = envelope.content.name
            latest = envelope.content.version
            lifecycle = ActiveEntry
            provenance = envelope.content.provenance
            versions =
                [
                    {
                        version = envelope.content.version
                        contentHash = envelope.contentHash
                    }
                ]
        }

    [<Fact>]
    let ``importCatalogue pushes every entry once, categories then materials then samples then presets, through the exact signatures`` () : unit =
        let catalogue = readImportCatalogue ()
        let tape = ResizeArray<ImportWrite>()
        let context = importContext (recordingProxy tape (fun _ -> false))
        let import : CatalogueData -> Result<ImportDecision list, CatalogueImportError> = context.importCatalogue
        let rows = import catalogue |> expectImportOk
        Assert.Equal(catalogue.envelopes.Length, rows.Length)
        Assert.Equal<LibraryEntryId list>(catalogue.envelopes |> List.map _.content.id, rows |> List.map _.entryId)
        Assert.Empty tape
        let outcome = rows |> List.map _.byDefault |> context.apply |> expectImportOk
        Assert.Empty outcome.rebindings
        Assert.Empty outcome.forks
        Assert.Equal<ImportWrite list>(expectedPuts catalogue, List.ofSeq tape)

    [<Fact>]
    let ``importCatalogue stops at the first rejected entry and answers its ImportRejectedErr`` () : unit =
        let catalogue = readImportCatalogue ()
        let tape = ResizeArray<ImportWrite>()
        let context = importContext (recordingProxy tape (fun put -> put.envelope.content.schema.kind = MaterialEntryKind))
        let rows = context.importCatalogue catalogue |> expectImportOk
        Assert.Equal(catalogue.envelopes.Length, rows.Length)
        Assert.Empty tape
        Assert.Equal<Result<ImportOutcome, CatalogueImportError>>(
            Error (ImportRejectedErr "the stub rejects every material"), rows |> List.map _.byDefault |> context.apply)
        let categories = expectedPuts catalogue |> List.filter (function PutVersion put -> put.envelope.content.schema.kind = CategoryEntryKind | _ -> false)
        Assert.Equal<ImportWrite list>(categories, List.ofSeq tape)

    [<Fact>]
    let ``every import error renders its condition and its payload`` () : unit =
        let entryError = MissingEntryFieldErr "Silicon has no category"
        let catalogueError = CatalogueIoErr "the catalogue folder is unavailable"
        Assert.Equal("the import store is unavailable: database was disposed", (ImportStoreUnavailableErr "database was disposed").describe)
        Assert.Equal("a catalogue entry was rejected: store refused Silicon", (ImportRejectedErr "store refused Silicon").describe)
        Assert.Equal($"a catalogue entry could not be decoded: {entryError.describe}", (UndecodableEntryErr entryError).describe)
        Assert.Equal($"the catalogue could not be read: {catalogueError.describe}", (CatalogueUnreadableErr catalogueError).describe)
        Assert.Equal("the catalogue is already registered: Lab is registered under the shared folder", (CatalogueAlreadyRegisteredErr "Lab is registered under the shared folder").describe)
        Assert.Equal("an import choice is invalid: Silicon cannot be reused", (InvalidImportChoiceErr "Silicon cannot be reused").describe)
        Assert.Equal("an import would leave a dangling reference: Plate references skipped Silicon", (DanglingReferenceErr "Plate references skipped Silicon").describe)
        Assert.Equal("a catalogue entry could not be hashed: Silicon cannot be sealed", (EntryHashingErr "Silicon cannot be sealed").describe)
        Assert.Equal("a dropped file was refused: notes.txt: not a library entry file", (DroppedFileRefusedErr "notes.txt: not a library entry file").describe)
        Assert.Equal("a dropped entry has an unresolved reference: Plate references absent Silicon", (UnresolvedReferenceErr "Plate references absent Silicon").describe)

    [<Fact>]
    let ``every export error renders its condition and its payload`` () : unit =
        let missingReason = "entry 31801b03-444f-43c8-8440-000000000116 is absent"
        let importError = ImportRejectedErr "the latest version cannot be pinned"
        let writeReason = "Lab glass.material-json: access was denied"
        let catalogueError = InvalidCatalogueEntriesErr (DuplicateEntryFileNameErr "Lab glass names two material entries")
        let occupiedReason = "D:/Exports/Lab catalogue already holds a manifest"
        let emptyReason = "D:/Exports/Empty catalogue has no entries to export"
        let cases =
            [
                ExportedEntryNotHeldErr missingReason, $"the entry to export is not held: {missingReason}"
                ExportLibraryRefusedErr importError, $"the library refused the export: {importError.describe}"
                ExportFileWriteErr writeReason, $"the entry file could not be written: {writeReason}"
                ExportCatalogueRefusedErr catalogueError, $"the catalogue export was refused: {catalogueError.describe}"
                ExportFolderHoldsCatalogueErr occupiedReason, $"the export folder already holds a catalogue: {occupiedReason}"
                EmptyLibraryExportErr emptyReason, $"there is nothing to export: {emptyReason}"
            ]
        for error, expected in cases do
            Assert.Equal(expected, error.describe)
        Assert.Equal(6, cases |> List.map (fun (error, _) -> error.describe) |> List.distinct |> List.length)

    [<Fact>]
    let ``a CatalogueImportProxy compares by reference (the [<ReferenceEquality>] contract)`` () : unit =
        let make () : CatalogueImportProxy = recordingProxy (ResizeArray<ImportWrite>()) (fun _ -> false)
        let proxy = make ()
        let same = proxy
        Assert.True((proxy = same))
        Assert.False((proxy = make ()))

    [<Fact>]
    let ``apply refuses an action its row does not offer, and a write that would leave a reference dangling, before it writes`` () : unit =
        let catalogue = readImportCatalogue ()
        let tape = ResizeArray<ImportWrite>()
        let context = importContext (recordingProxy tape (fun _ -> false))
        let rows = context.importCatalogue catalogue |> expectImportOk
        Assert.Empty tape
        let invalid = rows |> List.map (fun row ->
            if row = rows.Head then
                {
                    row.byDefault with
                        action = ReuseHeldEntry
                }
            else row.byDefault)
        match context.apply invalid with
        | Error (InvalidImportChoiceErr reason) -> Assert.Contains(rows.Head.entryId.value.ToString(), reason)
        | other -> Assert.Fail($"Expected InvalidImportChoiceErr, got {other}")
        Assert.Empty tape
        let sample = catalogue.envelopes |> List.find (fun envelope -> envelope.content.schema.kind = SampleEntryKind && not envelope.content.referencedEntryIds.IsEmpty)
        let materialId = sample.content.referencedEntryIds |> Set.minElement
        let dangling = rows |> List.map (fun row ->
            if row.entryId = materialId then
                {
                    row.byDefault with
                        action = SkipIncomingEntry
                }
            else row.byDefault)
        match context.apply dangling with
        | Error (DanglingReferenceErr reason) ->
            Assert.Contains(materialId.value.ToString(), reason)
            Assert.Contains(sample.content.id.value.ToString(), reason)
        | other -> Assert.Fail($"Expected DanglingReferenceErr, got {other}")
        Assert.Empty tape

    [<Fact>]
    let ``apply forks a divergent entry under a minted id whose provenance keeps the original`` () : unit =
        let catalogue = readImportCatalogue ()
        let original = catalogue.envelopes |> List.find (fun envelope -> envelope.content.schema.kind = SourceEntryKind)
        let incomingContent =
            {
                original.content with
                    name = EntryName "Divergent source"
            }
        let incoming = incomingContent.``sealed`` |> Result.defaultWith (fun error -> failwith error.describe)
        let partial = CatalogueData.tryCreate catalogue.manifest.id catalogue.manifest.name catalogue.manifest.version [ incoming ] |> Result.defaultWith (fun error -> failwith error.describe)
        let tape = ResizeArray<ImportWrite>()
        let proxy =
            {
                recordingProxy tape (fun _ -> false) with
                    tryHeldEntry = fun _ -> Ok (Some (heldEntry original))
            }
        let context = importContext proxy
        let rows = context.importCatalogue partial |> expectImportOk
        Assert.Empty tape
        Assert.Equal(ForkIncomingEntry, (Assert.Single rows).defaultAction)
        let outcome = rows |> List.map _.byDefault |> context.apply |> expectImportOk
        let expectedContent =
            {
                incoming.content with
                    id = forkId
                    version = VersionNumber.first
                    provenance = ForkProvenance
                        {
                            originalId = incoming.content.id
                            originalVersion = incoming.content.version
                            originalProvenance = incoming.content.provenance
                        }
            }
        let expected = expectedContent.``sealed`` |> Result.defaultWith (fun error -> failwith error.describe)
        let expectedFork =
            {
                original = incoming.content.id
                fork = forkId
            }
        Assert.Equal<ImportWrite>(expectedPut expected, Assert.Single tape)
        Assert.Equal<EntryFork>(expectedFork, Assert.Single outcome.forks)
        Assert.Empty outcome.rebindings

    [<Fact>]
    let ``apply writes nothing for a reuse, a skip or a re-binding, answers the binding, and retires after every put`` () : unit =
        let catalogue = readImportCatalogue ()
        let sources = catalogue.envelopes |> List.filter (fun envelope -> envelope.content.schema.kind = SourceEntryKind)
        let added = sources.[0]
        let reused = sources.[1]
        let skipped = sources.[2]
        let rebound = sources.[3]
        let retired = heldEntry sources.[4]
        let binding =
            {
                heldEntry rebound with
                    id = forkId
            }
        let partial = CatalogueData.tryCreate catalogue.manifest.id catalogue.manifest.name catalogue.manifest.version [ added; reused; skipped; rebound ] |> Result.defaultWith (fun error -> failwith error.describe)
        let tape = ResizeArray<ImportWrite>()
        let proxy =
            {
                recordingProxy tape (fun _ -> false) with
                    tryHeldEntry = fun id -> Ok (if id = reused.content.id then Some (heldEntry reused) else None)
                    heldEntriesNamed = fun _ name -> Ok (if name = rebound.content.name then [ binding ] else [])
                    heldEntriesOf = fun _ -> Ok [ retired ]
            }
        let context = importContext proxy
        let rows = context.importCatalogue partial |> expectImportOk
        Assert.Equal(5, rows.Length)
        Assert.Empty tape
        let choiceFor (id : LibraryEntryId) : ImportChoice = (rows |> List.find (fun row -> row.entryId = id)).byDefault
        let choices =
            [
                choiceFor retired.id
                choiceFor added.content.id
                choiceFor reused.content.id
                {
                    choiceFor skipped.content.id with
                        action = SkipIncomingEntry
                }
                {
                    choiceFor rebound.content.id with
                        action = RebindToHeldEntry binding.id
                }
            ]
        let outcome = context.apply choices |> expectImportOk
        let expectedBinding =
            {
                incoming = rebound.content.id
                held = binding.id
            }
        Assert.Equal<ImportWrite list>([ expectedPut added; RetireEntry retired.id ], List.ofSeq tape)
        Assert.Equal<EntryRebinding>(expectedBinding, Assert.Single outcome.rebindings)
        Assert.Empty outcome.forks
