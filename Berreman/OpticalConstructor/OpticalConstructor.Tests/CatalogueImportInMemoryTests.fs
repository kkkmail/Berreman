namespace OpticalConstructor.Tests

open System
open Softellect.Sys.Primitives
open Xunit
open OpticalConstructor.Domain.Lifecycle
open OpticalConstructor.Domain.Placement
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.CatalogueFiles
open OpticalConstructor.Domain.CatalogueImport
open OpticalConstructor.Storage.CatalogueFolderStore
open OpticalConstructor.Storage.CatalogueImportStore
open OpticalConstructor.Storage.CategoryDatabaseStore
open OpticalConstructor.Storage.EntryFileJson
open OpticalConstructor.Storage.LibraryDatabaseStore
open OpticalConstructor.Storage.MaterialDatabaseStore
open OpticalConstructor.Storage.PresetDatabaseStore
open OpticalConstructor.Storage.SampleDatabaseStore
open OpticalConstructor.Storage.Errors
open CatalogueImportTestData

type ImportStoreSet =
    {
        context : CatalogueImportContext
        categories : CategoryProxy
        materials : MaterialProxy
        samples : SampleProxy
        presets : LibraryProxy
    }

module private CatalogueImportContractData =

    let expectImportDatabaseOk (result : Result<'Outcome, StorageError>) : 'Outcome =
        result |> Result.defaultWith (fun error -> failwith error.describe)

    let presetEntries (stores : ImportStoreSet) : LibraryEntry list =
        [ LightSource; Detector; LinearPolarizer; CircularPolarizer ]
        |> List.collect (fun kind ->
            match stores.presets.entriesForKind kind with
            | Ok entries -> entries
            | Error _ -> failwith "The preset store must support every preset kind.")
        |> List.distinctBy (fun entry -> entry.entryId)

    let assertEmptyStores (stores : ImportStoreSet) : unit =
        Assert.Equal<Result<MaterialCategory list, CategoryError>>(Ok [], stores.categories.listCategories ())
        Assert.Equal<Result<MaterialEntry list, MaterialError>>(Ok [], stores.materials.listMaterials IncludeInactive)
        Assert.Equal<Result<Sample list, SampleError>>(Ok [], stores.samples.listSamples IncludeInactive)
        Assert.Empty(presetEntries stores)

open CatalogueImportContractData

/// The same import contract runs over empty in-memory and SQLite stores (spec 0050 S3.1).
[<AbstractClass>]
type CatalogueImportContract () =

    abstract freshImport : unit -> ImportStoreSet

    [<Fact>]
    member this.``importFrom fills empty stores with the whole Built-in catalogue`` () : unit =
        let catalogue = readImportCatalogue ()
        let stores = this.freshImport ()
        let context = stores.context
        let import : CatalogueProxy -> Result<ImportDecision list, CatalogueImportError> = context.importFrom
        let rows = import (CatalogueProxy.createBuiltIn ()) |> expectImportOk
        Assert.Equal(catalogue.envelopes.Length, rows.Length)
        assertEmptyStores stores
        rows |> List.map _.byDefault |> context.apply |> expectImportOk |> ignore

        let countOf (kind : LibraryEntryKind) : int =
            catalogue.envelopes |> List.filter (fun envelope -> envelope.content.schema.kind = kind) |> List.length

        Assert.Equal<Result<int, CategoryError>>(Ok (countOf CategoryEntryKind), stores.categories.listCategories () |> Result.map List.length)
        Assert.Equal<Result<int, MaterialError>>(Ok (countOf MaterialEntryKind), stores.materials.listMaterials IncludeInactive |> Result.map List.length)
        Assert.Equal<Result<int, SampleError>>(Ok (countOf SampleEntryKind), stores.samples.listSamples IncludeInactive |> Result.map List.length)
        Assert.Equal(countOf SourceEntryKind + countOf PolarizerEntryKind + countOf DetectorEntryKind, (presetEntries stores).Length)

    [<Fact>]
    member this.``every imported entry resolves by its id to its decoded value`` () : unit =
        let catalogue = readImportCatalogue ()
        let stores = this.freshImport ()
        let context = stores.context
        let rows = context.importCatalogue catalogue |> expectImportOk
        Assert.Equal(catalogue.envelopes.Length, rows.Length)
        assertEmptyStores stores
        rows |> List.map _.byDefault |> context.apply |> expectImportOk |> ignore

        let assertPreset (entry : LibraryEntry) : unit =
            Assert.True((Ok (Some entry) = stores.presets.tryGetEntry entry.entryId), "A preset id must resolve to its decoded value.")

        for envelope in catalogue.envelopes do
            let content = envelope.content
            match content.schema.kind with
            | CategoryEntryKind ->
                let expected = content.toMaterialCategory |> decodeImportEntry
                let actual = stores.categories.listCategories () |> Result.map (List.tryFind (fun category -> category.id = expected.id))
                Assert.True((Ok (Some expected) = actual), "A category id must resolve to its decoded value.")
            | MaterialEntryKind ->
                let expected = content.toMaterialEntry |> decodeImportEntry
                Assert.True((Ok (Some expected) = stores.materials.tryGetMaterial expected.id), "A material id must resolve to its decoded value.")
            | SampleEntryKind ->
                let expected = content.toSample |> decodeImportEntry
                Assert.True((Ok (Some expected) = stores.samples.tryGetSample expected.id), "A sample id must resolve to its decoded value.")
            | SourceEntryKind -> content.toSourcePreset |> decodeImportEntry |> SourceItem |> assertPreset
            | PolarizerEntryKind -> content.toPolarizerPreset |> decodeImportEntry |> PolarizerItem |> assertPreset
            | DetectorEntryKind -> content.toDetectorPreset |> decodeImportEntry |> DetectorItem |> assertPreset

    [<Fact>]
    member this.``importing a catalogue the stores already hold reuses every entry and writes nothing`` () : unit =
        let catalogue = readImportCatalogue ()
        let stores = this.freshImport ()
        let writes = ResizeArray<EntryVersionPut>()
        let proxy =
            {
                stores.context.proxy with
                    putEntryVersion = fun put ->
                        writes.Add put
                        stores.context.proxy.putEntryVersion put
            }
        let context = CatalogueImportContext.create proxy
        let rows = context.importCatalogue catalogue |> expectImportOk
        Assert.Empty writes
        assertEmptyStores stores
        rows |> List.map _.byDefault |> context.apply |> expectImportOk |> ignore
        let categories = stores.categories.listCategories ()
        let materials = stores.materials.listMaterials IncludeInactive
        let samples = stores.samples.listSamples IncludeInactive
        let presets = presetEntries stores
        writes.Clear()
        let reused = context.importCatalogue catalogue |> expectImportOk
        Assert.Equal(catalogue.envelopes.Length, reused.Length)
        for row in reused do
            match row.status with
            | SameContentHash _ -> Assert.Equal(ReuseHeldEntry, row.defaultAction)
            | _ -> Assert.Fail("Every held catalogue entry must be reused.")
        Assert.Empty writes
        reused |> List.map _.byDefault |> context.apply |> expectImportOk |> ignore
        Assert.Empty writes
        Assert.Equal(categories, stores.categories.listCategories ())
        Assert.Equal(materials, stores.materials.listMaterials IncludeInactive)
        Assert.Equal(samples, stores.samples.listSamples IncludeInactive)
        Assert.Equal<LibraryEntry list>(presets, presetEntries stores)

    [<Fact>]
    member this.``importFrom an unreadable catalogue imports nothing and answers CatalogueUnreadableErr`` () : unit =
        let stores = this.freshImport ()
        let unreadable = CatalogueProxy.createInMemory ()

        let context = stores.context
        match context.importFrom unreadable with
        | Error (CatalogueUnreadableErr (CatalogueIoErr reason)) -> Assert.False(System.String.IsNullOrWhiteSpace reason)
        | Error error -> Assert.Fail($"Expected a catalogue IO error, got {error.describe}")
        | Ok _ -> Assert.Fail("An unwritten catalogue must not import successfully.")

        Assert.Equal<Result<MaterialCategory list, CategoryError>>(Ok [], stores.categories.listCategories ())
        Assert.Equal<Result<MaterialEntry list, MaterialError>>(Ok [], stores.materials.listMaterials IncludeInactive)
        Assert.Equal<Result<Sample list, SampleError>>(Ok [], stores.samples.listSamples IncludeInactive)
        Assert.Empty(presetEntries stores)

    [<Fact>]
    member this.``putEntryVersion refuses a version the entry holds and an id held as another kind`` () : unit =
        let catalogue = readImportCatalogue ()
        let stores = this.freshImport ()
        let context = stores.context
        let rows = context.importFrom (CatalogueProxy.createBuiltIn ()) |> expectImportOk
        rows |> List.map _.byDefault |> context.apply |> expectImportOk |> ignore
        let material = catalogue.envelopes |> List.find (fun envelope -> envelope.content.schema.kind = MaterialEntryKind)
        let before = context.proxy.tryHeldEntry material.content.id |> expectImportOk
        Assert.True(Option.isSome before)
        let duplicate =
            {
                envelope = material
                latest = MoveLatestVersion
            }
        match context.proxy.putEntryVersion duplicate with
        | Error (ImportRejectedErr reason) ->
            Assert.Contains(material.content.id.value.ToString(), reason)
            Assert.Contains(material.content.version.value.ToString(), reason)
        | Error error -> Assert.Fail($"Expected a duplicate-version refusal, got {error.describe}")
        | Ok () -> Assert.Fail("A held version must not be overwritten.")
        Assert.Equal<HeldEntry option>(before, context.proxy.tryHeldEntry material.content.id |> expectImportOk)

        let sample = catalogue.envelopes |> List.find (fun envelope -> envelope.content.schema.kind = SampleEntryKind)
        let conflictingContent =
            {
                sample.content with
                    id = material.content.id
                    version = material.content.version.next
            }
        let conflicting =
            {
                envelope = conflictingContent.``sealed`` |> expectImportDatabaseOk
                latest = MoveLatestVersion
            }
        match context.proxy.putEntryVersion conflicting with
        | Error (ImportRejectedErr reason) ->
            Assert.Contains(material.content.id.value.ToString(), reason)
            Assert.Contains(MaterialEntryKind.extension.value, reason)
            Assert.Contains(SampleEntryKind.extension.value, reason)
        | Error error -> Assert.Fail($"Expected a kind-conflict refusal, got {error.describe}")
        | Ok () -> Assert.Fail("An entry id must not be reused as another kind.")
        Assert.Equal<HeldEntry option>(before, context.proxy.tryHeldEntry material.content.id |> expectImportOk)

type CatalogueImportInMemoryTests () =
    inherit CatalogueImportContract ()

    override _.freshImport () : ImportStoreSet =
        let stores = CatalogueImportProxy.createInMemory ()
        {
            context = CatalogueImportContext.create stores.proxy
            categories = stores.categories
            materials = stores.materials
            samples = stores.samples
            presets = stores.libraryEntries
        }

    [<Fact>]
    member this.``an entry that does not decode stops the import with UndecodableEntryErr`` () : unit =
        let catalogue = readImportCatalogue ()
        let firstEnvelope (kind : LibraryEntryKind) : LibraryEntryEnvelope =
            catalogue.envelopes |> List.find (fun envelope -> envelope.content.schema.kind = kind)
        let category = firstEnvelope CategoryEntryKind
        let material = firstEnvelope MaterialEntryKind
        let malformed =
            {
                material with
                    content =
                        {
                            material.content with
                                categoryOpt = None
                        }
            }
        let partial =
            CatalogueData.tryCreate catalogue.manifest.id catalogue.manifest.name catalogue.manifest.version
                [ category; malformed; firstEnvelope SampleEntryKind ]
            |> Result.defaultWith (fun error -> failwith error.describe)
        let stores = this.freshImport ()
        let context = stores.context
        let rows = context.importCatalogue partial |> expectImportOk
        Assert.Equal(partial.envelopes.Length, rows.Length)
        assertEmptyStores stores
        match rows |> List.map _.byDefault |> context.apply with
        | Error (UndecodableEntryErr (MissingEntryFieldErr reason)) -> Assert.Contains(material.content.name.value, reason)
        | Error error -> Assert.Fail($"Expected a missing entry field, got {error.describe}")
        | Ok _ -> Assert.Fail("An undecodable material must stop the import.")
        Assert.Equal<Result<MaterialCategory list, CategoryError>>(
            Ok [ category.content.toMaterialCategory |> decodeImportEntry ], stores.categories.listCategories ())
        Assert.Equal<Result<MaterialEntry list, MaterialError>>(Ok [], stores.materials.listMaterials IncludeInactive)
        Assert.Equal<Result<Sample list, SampleError>>(Ok [], stores.samples.listSamples IncludeInactive)
        Assert.Empty(presetEntries stores)
        Assert.Equal<Result<HeldEntry option, CatalogueImportError>>(Ok None, stores.context.proxy.tryHeldEntry material.content.id)

type CatalogueImportSqliteTests () =
    inherit CatalogueImportContract ()

    let databases = ResizeArray<LibraryDatabase>()

    member private _.openDatabase () : LibraryDatabase =
        let connectionString = ConnectionString "Data Source=:memory:"
        let database = LibraryDatabase.openOrCreate connectionString |> expectImportDatabaseOk
        databases.Add database
        database

    override this.freshImport () : ImportStoreSet =
        let database = this.openDatabase ()
        {
            context = CatalogueImportContext.create (CatalogueImportProxy.createInDatabase database)
            categories = CategoryProxy.createInDatabase database
            materials = MaterialProxy.createInDatabase database
            samples = SampleProxy.createInDatabase database
            presets = LibraryProxy.createInDatabase database
        }

    [<Fact>]
    member this.``the database import holds every Built-in envelope byte for byte under its own provenance`` () : unit =
        let database = this.openDatabase ()
        let context = CatalogueImportContext.create (CatalogueImportProxy.createInDatabase database)
        let catalogue = readImportCatalogue ()
        let rows = context.importFrom (CatalogueProxy.createBuiltIn ()) |> expectImportOk
        Assert.Equal(catalogue.envelopes.Length, rows.Length)
        database.run (fun stored ->
            Assert.Empty(stored.EntryVersions)
            Assert.Empty(stored.LatestVersions))
        |> expectImportDatabaseOk
        rows |> List.map _.byDefault |> context.apply |> expectImportOk |> ignore

        let readVersions () : StoredEntryVersion list =
            database.run (fun stored ->
                Assert.Equal(catalogue.envelopes.Length, stored.EntryVersions |> Seq.length)
                Assert.Equal(catalogue.envelopes.Length, stored.LatestVersions |> Seq.length)
                LibraryEntryKind.all
                |> List.collect (fun kind ->
                    let expected =
                        catalogue.envelopes |> List.filter (fun envelope -> envelope.content.schema.kind = kind)
                        |> List.sortBy _.content.id
                    let actual = stored.latestStoredOfKind kind |> expectImportDatabaseOk |> List.sortBy _.envelope.content.id
                    Assert.Equal<LibraryEntryEnvelope list>(expected, actual |> List.map _.envelope)
                    for version in actual do
                        Assert.Equal(BuiltInProvenance CatalogueIds.builtInCatalogue, version.envelope.content.provenance)
                        Assert.Equal(VersionNumber.first, version.envelope.content.version)
                        Assert.Equal(ActiveEntry, version.state.lifecycle)
                        Assert.Equal(VersionUnpinned, version.state.pin)
                        let row = stored.EntryVersions.Find(version.envelope.content.id.value, version.envelope.content.version.value)
                        let original = expected |> List.find (fun envelope -> envelope.content.id = version.envelope.content.id)
                        Assert.Equal((original.fileText |> expectImportDatabaseOk).value, row.PayloadText)
                    actual)
                |> List.sortBy (fun stored -> stored.envelope.content.id, stored.envelope.content.version))
            |> expectImportDatabaseOk
        let before = readVersions ()
        let held = context.proxy.heldEntriesOf CatalogueIds.builtInCatalogue |> expectImportOk
        Assert.Equal<Set<LibraryEntryId>>(catalogue.envelopes |> List.map _.content.id |> Set.ofList, held |> List.map _.id |> Set.ofList)
        for entry in held do
            Assert.Equal<HeldEntry option>(Some entry, context.proxy.tryHeldEntry entry.id |> expectImportOk)
            Assert.Contains(entry, context.proxy.heldEntriesNamed entry.kind entry.name |> expectImportOk)
        let reused = context.importFrom (CatalogueProxy.createBuiltIn ()) |> expectImportOk
        Assert.Equal(catalogue.envelopes.Length, reused.Length)
        for row in reused do
            match row.status with
            | SameContentHash _ -> Assert.Equal(ReuseHeldEntry, row.defaultAction)
            | _ -> Assert.Fail("Every held catalogue entry must be reused.")
        reused |> List.map _.byDefault |> context.apply |> expectImportOk |> ignore
        Assert.Equal<StoredEntryVersion list>(before, readVersions ())

    [<Fact>]
    member this.``a database failure below the import seam is ImportStoreUnavailableErr, never a throw`` () : unit =
        let database = this.openDatabase ()
        let proxy = CatalogueImportProxy.createInDatabase database
        let envelope = (readImportCatalogue ()).envelopes.Head
        let put =
            {
                envelope = envelope
                latest = MoveLatestVersion
            }
        (database :> IDisposable).Dispose()
        let assertUnavailable (result : Result<'Outcome, CatalogueImportError>) : unit =
            match result with
            | Error (ImportStoreUnavailableErr reason) ->
                Assert.False(String.IsNullOrWhiteSpace reason)
                Assert.Contains(nameof LibraryDatabase, reason)
            | Error error -> Assert.Fail($"Expected a storage failure, got {error.describe}")
            | Ok _ -> Assert.Fail("A disposed database must fail through the typed import error.")
        proxy.tryHeldEntry envelope.content.id |> assertUnavailable
        proxy.heldEntriesNamed envelope.content.schema.kind envelope.content.name |> assertUnavailable
        proxy.heldEntriesOf CatalogueIds.builtInCatalogue |> assertUnavailable
        proxy.putEntryVersion put |> assertUnavailable
        proxy.retireEntry envelope.content.id |> assertUnavailable

    interface IDisposable with
        member _.Dispose () : unit =
            for database in databases do
                (database :> IDisposable).Dispose()
