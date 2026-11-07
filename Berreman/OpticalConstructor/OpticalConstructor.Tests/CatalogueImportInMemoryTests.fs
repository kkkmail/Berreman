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

    type ImportRowEntries =
        {
            builtInName : CatalogueName
            category : LibraryEntryEnvelope
            silicon : LibraryEntryEnvelope
            source : LibraryEntryEnvelope
        }

    let labCatalogueId = CatalogueId (Guid.Parse "31801b01-444f-43c8-8440-000000000096")
    let labCatalogueName = CatalogueName "Lab"
    let forkEntryId = LibraryEntryId (Guid.Parse "31801b02-444f-43c8-8440-000000000096")
    let labMaterialId = LibraryEntryId (Guid.Parse "31801b03-444f-43c8-8440-000000000096")

    let readImportRowEntries () : ImportRowEntries =
        let catalogue = readImportCatalogue ()
        let silicon = catalogue.envelopes |> List.find (fun envelope -> envelope.content.id = MaterialIds.silicon.toLibraryEntryId)
        {
            builtInName = catalogue.manifest.name
            category = catalogue.envelopes |> List.find (fun envelope -> Set.contains envelope.content.id silicon.content.referencedEntryIds)
            silicon = silicon
            source = catalogue.envelopes |> List.find (fun envelope -> envelope.content.schema.kind = SourceEntryKind)
        }

    let rowCatalogue (id : CatalogueId) (name : CatalogueName) (envelopes : LibraryEntryEnvelope list) : CatalogueData =
        CatalogueData.tryCreate id name VersionNumber.first envelopes
        |> Result.defaultWith (fun error -> failwith error.describe)

    let userImportMaterial (silicon : LibraryEntryEnvelope) : LibraryEntryEnvelope =
        let content =
            {
                silicon.content with
                    id = labMaterialId
                    name = EntryName "Lab material"
                    provenance = UserProvenance
            }
        content.``sealed`` |> expectImportDatabaseOk

    let nextImportEnvelope (envelope : LibraryEntryEnvelope) : LibraryEntryEnvelope =
        let content =
            {
                envelope.content with
                    version = envelope.content.version.next
                    descriptionOpt = Some (EntryDescription "Updated catalogue description")
            }
        content.``sealed`` |> expectImportDatabaseOk

    let seedImportRows (context : CatalogueImportContext) (envelopes : LibraryEntryEnvelope list) : unit =
        for envelope in envelopes do
            let put =
                {
                    envelope = envelope
                    latest = MoveLatestVersion
                }
            context.proxy.putEntryVersion put |> expectImportOk

    let observeImportRows (context : CatalogueImportContext) : CatalogueImportContext * ResizeArray<EntryVersionPut> * ResizeArray<LibraryEntryId> =
        let puts = ResizeArray<EntryVersionPut>()
        let retirements = ResizeArray<LibraryEntryId>()
        let proxy =
            {
                context.proxy with
                    putEntryVersion = fun put ->
                        puts.Add put
                        context.proxy.putEntryVersion put
                    retireEntry = fun id ->
                        retirements.Add id
                        context.proxy.retireEntry id
            }
        let observed =
            {
                context with
                    proxy = proxy
                    newEntryId = fun () -> forkEntryId
            }
        observed, puts, retirements

    let importRowSnapshot (context : CatalogueImportContext) (ids : LibraryEntryId list) : (LibraryEntryId * HeldEntry option) list =
        ids |> List.distinct |> List.map (fun id -> id, context.proxy.tryHeldEntry id |> expectImportOk)

    let assertImportRowsPending (context : CatalogueImportContext) (puts : ResizeArray<EntryVersionPut>)
        (retirements : ResizeArray<LibraryEntryId>) (before : (LibraryEntryId * HeldEntry option) list)
        (rows : ImportDecision list) : unit =
        Assert.Equal<ImportDecision list>(rows, (ImportDecision.split rows).awaiting)
        Assert.Empty puts
        Assert.Empty retirements
        Assert.Equal<(LibraryEntryId * HeldEntry option) list>(before, importRowSnapshot context (before |> List.map fst))

    let heldImportRow (context : CatalogueImportContext) (id : LibraryEntryId) : HeldEntry =
        match context.proxy.tryHeldEntry id |> expectImportOk with
        | Some held -> held
        | None -> failwith $"Expected held import entry {id.value}."

    let importVersions (envelopes : LibraryEntryEnvelope list) : HeldEntryVersion list =
        envelopes |> List.map (fun envelope ->
            {
                version = envelope.content.version
                contentHash = envelope.contentHash
            })

open CatalogueImportContractData

/// The same import contract runs over empty in-memory and SQLite stores (spec 0050 S3.1).
[<AbstractClass>]
type CatalogueImportContract () =

    abstract freshImport : unit -> ImportStoreSet

    [<Fact>]
    member this.``the import log records, replaces and forgets a catalogue by its id`` () : unit =
        let proxy = (this.freshImport ()).context.proxy
        let first =
            {
                catalogueId = CatalogueIds.builtInCatalogue
                folder = FolderName "b"
                manifestHash = ContentHash (String('a', 64))
                importedAt = CatalogueImportTime (DateTime(2026, 11, 6, 12, 30, 45, DateTimeKind.Utc).AddTicks(1234567L))
                entryCount = CatalogueEntryCount 12
            }
        let second =
            {
                catalogueId = labCatalogueId
                folder = FolderName "a"
                manifestHash = ContentHash (String('b', 64))
                importedAt = CatalogueImportTime (DateTime(2026, 11, 6, 13, 30, 45, DateTimeKind.Utc).AddTicks(7654321L))
                entryCount = CatalogueEntryCount 3
            }
        let assertLog (expected : CatalogueImportLogEntry list) : unit =
            let actual = proxy.loggedImports () |> expectImportOk
            Assert.Equal<CatalogueImportLogEntry list>(expected, actual)
            for entry in actual do
                Assert.Equal(DateTimeKind.Utc, entry.importedAt.value.Kind)

        assertLog []
        proxy.logImport first |> expectImportOk
        assertLog [ first ]
        proxy.logImport second |> expectImportOk
        assertLog [ second; first ]
        Assert.Empty((this.freshImport ()).context.proxy.loggedImports () |> expectImportOk)

        let replacement =
            {
                first with
                    folder = FolderName "B"
                    manifestHash = ContentHash (String('c', 64))
                    importedAt = CatalogueImportTime (DateTime(2026, 11, 7, 1, 2, 3, DateTimeKind.Utc).AddTicks(9876543L))
                    entryCount = CatalogueEntryCount 15
            }
        proxy.logImport replacement |> expectImportOk
        assertLog [ replacement; second ]
        proxy.forgetImport first.catalogueId |> expectImportOk
        assertLog [ second ]
        Assert.Equal<Result<unit, CatalogueImportError>>(Ok (), proxy.forgetImport first.catalogueId)
        assertLog [ second ]
        proxy.forgetImport second.catalogueId |> expectImportOk
        assertLog []

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

    [<Fact>]
    member this.``a new id is imported after confirmation as its own latest version`` () : unit =
        let entries = readImportRowEntries ()
        let incoming = userImportMaterial entries.silicon
        let stores = this.freshImport ()
        seedImportRows stores.context [ entries.category ]
        let context, puts, retirements = observeImportRows stores.context
        let catalogue = rowCatalogue labCatalogueId labCatalogueName [ incoming ]
        let before = importRowSnapshot context [ entries.category.content.id; incoming.content.id; forkEntryId ]
        let rows = context.importCatalogue catalogue |> expectImportOk
        let row = Assert.Single rows
        Assert.Equal(EntryIdNotHeld incoming, row.status)
        Assert.Equal(AddIncomingEntry, row.defaultAction)
        assertImportRowsPending context puts retirements before rows
        Assert.Equal<Result<HeldEntry option, CatalogueImportError>>(Ok None, context.proxy.tryHeldEntry incoming.content.id)

        let outcome = context.apply [ row.byDefault ] |> expectImportOk
        let held = heldImportRow context incoming.content.id
        Assert.Equal(incoming.content.id, held.id)
        Assert.Equal(incoming.content.schema.kind, held.kind)
        Assert.Equal(incoming.content.name, held.name)
        Assert.Equal(incoming.content.version, held.latest)
        Assert.Equal(incoming.content.provenance, held.provenance)
        Assert.Equal(ActiveEntry, held.lifecycle)
        Assert.Equal<HeldEntryVersion list>(importVersions [ incoming ], held.versions)
        Assert.Equal(incoming, (Assert.Single puts).envelope)
        Assert.Empty retirements
        Assert.Empty outcome.rebindings
        Assert.Empty outcome.forks

    [<Fact>]
    member this.``the same id with the same hash is reused and nothing is written`` () : unit =
        let entries = readImportRowEntries ()
        let stores = this.freshImport ()
        seedImportRows stores.context [ entries.category; entries.silicon ]
        let context, puts, retirements = observeImportRows stores.context
        let catalogue = rowCatalogue labCatalogueId labCatalogueName [ entries.silicon ]
        let before = importRowSnapshot context [ entries.category.content.id; entries.silicon.content.id; forkEntryId ]
        let held = heldImportRow context entries.silicon.content.id
        let rows = context.importCatalogue catalogue |> expectImportOk
        let row = Assert.Single rows
        Assert.Equal(SameContentHash (entries.silicon, held), row.status)
        Assert.Equal(ReuseHeldEntry, row.defaultAction)
        assertImportRowsPending context puts retirements before rows

        let outcome = context.apply [ row.byDefault ] |> expectImportOk
        Assert.Equal<(LibraryEntryId * HeldEntry option) list>(before, importRowSnapshot context (before |> List.map fst))
        Assert.Equal<HeldEntryVersion list>(importVersions [ entries.silicon ], (heldImportRow context held.id).versions)
        Assert.Empty puts
        Assert.Empty retirements
        Assert.Empty outcome.rebindings
        Assert.Empty outcome.forks

    [<Fact>]
    member this.``a version not held is added after confirmation, and the latest moves only for a protected built-in from its own catalogue`` () : unit =
        let entries = readImportRowEntries ()
        let scenarios =
            [
                CatalogueIds.builtInCatalogue, entries.silicon, MoveLatestVersion
                labCatalogueId, userImportMaterial entries.silicon, KeepLatestVersion
                labCatalogueId, entries.silicon, KeepLatestVersion
            ]
        for catalogueId, original, latestMove in scenarios do
            let stores = this.freshImport ()
            seedImportRows stores.context [ entries.category; original ]
            let context, puts, retirements = observeImportRows stores.context
            let incoming = nextImportEnvelope original
            let catalogueName = if catalogueId = labCatalogueId then labCatalogueName else entries.builtInName
            let catalogue = rowCatalogue catalogueId catalogueName [ entries.category; incoming ]
            let before = importRowSnapshot context [ entries.category.content.id; original.content.id; forkEntryId ]
            let held = heldImportRow context original.content.id
            let rows = context.importCatalogue catalogue |> expectImportOk
            Assert.Equal(2, rows.Length)
            let row = rows |> List.find (fun row -> row.entryId = original.content.id)
            Assert.Equal(VersionNotHeld (incoming, held), row.status)
            Assert.Equal(AddIncomingVersion latestMove, row.defaultAction)
            assertImportRowsPending context puts retirements before rows

            rows |> List.map _.byDefault |> context.apply |> expectImportOk |> ignore
            let latest = match latestMove with MoveLatestVersion -> incoming.content.version | KeepLatestVersion -> original.content.version
            let expected =
                {
                    held with
                        latest = latest
                        versions = importVersions [ original; incoming ]
                }
            Assert.Equal(expected, heldImportRow context original.content.id)
            let put = Assert.Single puts
            Assert.Equal(incoming, put.envelope)
            Assert.Equal(latestMove, put.latest)
            Assert.Empty retirements

    [<Fact>]
    member this.``the same id and version with another hash awaits a choice, and forking imports it under a new id that keeps the original`` () : unit =
        let entries = readImportRowEntries ()
        let changedContent =
            {
                entries.silicon.content with
                    descriptionOpt = Some (EntryDescription "Divergent Silicon description")
            }
        let incoming = changedContent.``sealed`` |> expectImportDatabaseOk
        let catalogue = rowCatalogue labCatalogueId labCatalogueName [ incoming ]
        for action in [ ForkIncomingEntry; SkipIncomingEntry ] do
            let stores = this.freshImport ()
            seedImportRows stores.context [ entries.category; entries.silicon ]
            let context, puts, retirements = observeImportRows stores.context
            let originalIds = [ entries.category.content.id; entries.silicon.content.id ]
            let originals = importRowSnapshot context originalIds
            let before = importRowSnapshot context (forkEntryId :: originalIds)
            let held = heldImportRow context entries.silicon.content.id
            let rows = context.importCatalogue catalogue |> expectImportOk
            let row = Assert.Single rows
            Assert.Equal(DivergentContent (incoming, held), row.status)
            Assert.Equal(ForkIncomingEntry, row.defaultAction)
            Assert.Contains(SkipIncomingEntry, row.alternatives)
            assertImportRowsPending context puts retirements before rows

            let choice =
                match action with
                | ForkIncomingEntry -> row.byDefault
                | _ ->
                    {
                        decision = row
                        action = action
                    }
            let outcome = context.apply [ choice ] |> expectImportOk
            Assert.Equal<(LibraryEntryId * HeldEntry option) list>(originals, importRowSnapshot context originalIds)
            Assert.Empty retirements
            Assert.Empty outcome.rebindings
            match action with
            | ForkIncomingEntry ->
                let ancestry =
                    {
                        originalId = entries.silicon.content.id
                        originalVersion = entries.silicon.content.version
                        originalProvenance = entries.silicon.content.provenance
                    }
                let forkContent =
                    {
                        incoming.content with
                            id = forkEntryId
                            version = VersionNumber.first
                            provenance = ForkProvenance ancestry
                    }
                let forkEnvelope = forkContent.``sealed`` |> expectImportDatabaseOk
                let fork = heldImportRow context forkEntryId
                Assert.Equal(ForkProvenance ancestry, fork.provenance)
                Assert.Equal(VersionNumber.first, fork.latest)
                Assert.Equal(incoming.content.name, fork.name)
                Assert.Equal<HeldEntryVersion list>(importVersions [ forkEnvelope ], fork.versions)
                Assert.Equal(forkEnvelope, (Assert.Single puts).envelope)
                let binding = Assert.Single outcome.forks
                Assert.Equal(entries.silicon.content.id, binding.original)
                Assert.Equal(forkEntryId, binding.fork)
            | _ ->
                Assert.Equal<Result<HeldEntry option, CatalogueImportError>>(Ok None, context.proxy.tryHeldEntry forkEntryId)
                Assert.Empty puts
                Assert.Empty outcome.forks

    [<Fact>]
    member this.``a new id with a held entry's name awaits a choice; side by side keeps both and a re-binding writes nothing`` () : unit =
        let entries = readImportRowEntries ()
        let content =
            {
                (userImportMaterial entries.silicon).content with
                    name = entries.silicon.content.name
            }
        let incoming = content.``sealed`` |> expectImportDatabaseOk
        let catalogue = rowCatalogue labCatalogueId labCatalogueName [ incoming ]
        for action in [ AddIncomingEntry; RebindToHeldEntry entries.silicon.content.id ] do
            let stores = this.freshImport ()
            seedImportRows stores.context [ entries.category; entries.silicon ]
            let context, puts, retirements = observeImportRows stores.context
            let before = importRowSnapshot context [ entries.category.content.id; entries.silicon.content.id; incoming.content.id; forkEntryId ]
            let original = heldImportRow context entries.silicon.content.id
            let named () = context.proxy.heldEntriesNamed MaterialEntryKind incoming.content.name |> expectImportOk |> List.sortBy _.id
            Assert.Equal<HeldEntry list>([ original ], named ())
            let rows = context.importCatalogue catalogue |> expectImportOk
            let row = Assert.Single rows
            Assert.Equal(NameCollision (incoming, [ original ]), row.status)
            Assert.Equal(AddIncomingEntry, row.defaultAction)
            Assert.Contains(RebindToHeldEntry original.id, row.alternatives)
            assertImportRowsPending context puts retirements before rows
            Assert.Equal<HeldEntry list>([ original ], named ())

            let choice =
                match action with
                | AddIncomingEntry -> row.byDefault
                | _ ->
                    {
                        decision = row
                        action = action
                    }
            let outcome = context.apply [ choice ] |> expectImportOk
            Assert.Equal(original, heldImportRow context original.id)
            Assert.Empty retirements
            Assert.Empty outcome.forks
            match action with
            | AddIncomingEntry ->
                let added = heldImportRow context incoming.content.id
                Assert.Equal(incoming.content.name, added.name)
                Assert.Equal(incoming.content.version, added.latest)
                Assert.Equal<HeldEntryVersion list>(importVersions [ incoming ], added.versions)
                Assert.Equal<HeldEntry list>([ original; added ] |> List.sortBy _.id, named ())
                Assert.Equal(incoming, (Assert.Single puts).envelope)
                Assert.Empty outcome.rebindings
            | _ ->
                Assert.Equal<(LibraryEntryId * HeldEntry option) list>(before, importRowSnapshot context (before |> List.map fst))
                Assert.Equal<HeldEntry list>([ original ], named ())
                Assert.Empty puts
                let binding = Assert.Single outcome.rebindings
                Assert.Equal(incoming.content.id, binding.incoming)
                Assert.Equal(original.id, binding.held)

    [<Fact>]
    member this.``an active built-in its catalogue no longer lists is retired after confirmation and stays held`` () : unit =
        let entries = readImportRowEntries ()
        let newer = nextImportEnvelope entries.silicon
        let stores = this.freshImport ()
        seedImportRows stores.context [ entries.category; entries.silicon; newer ]
        let context, puts, retirements = observeImportRows stores.context
        let catalogue = rowCatalogue CatalogueIds.builtInCatalogue entries.builtInName [ entries.category ]
        let before = importRowSnapshot context [ entries.category.content.id; entries.silicon.content.id; forkEntryId ]
        let original = heldImportRow context entries.silicon.content.id
        let rows = context.importCatalogue catalogue |> expectImportOk
        Assert.Equal(2, rows.Length)
        let retiredRow = rows |> List.find (fun row -> row.entryId = original.id)
        Assert.Equal(EntryUnlisted original, retiredRow.status)
        Assert.Equal(RetireHeldEntry, retiredRow.defaultAction)
        assertImportRowsPending context puts retirements before rows
        Assert.Equal(ActiveEntry, (heldImportRow context original.id).lifecycle)

        rows |> List.map _.byDefault |> context.apply |> expectImportOk |> ignore
        let retired =
            {
                original with
                    lifecycle = InactiveEntry
            }
        Assert.Equal(retired, heldImportRow context original.id)
        Assert.Equal<HeldEntryVersion list>(importVersions [ entries.silicon; newer ], retired.versions)
        Assert.Contains(retired, context.proxy.heldEntriesNamed MaterialEntryKind original.name |> expectImportOk)
        Assert.Empty puts
        Assert.Equal(original.id, Assert.Single retirements)

    [<Fact>]
    member this.``a user entry is never touched by a catalogue import`` () : unit =
        let entries = readImportRowEntries ()
        let user = userImportMaterial entries.silicon
        let stores = this.freshImport ()
        seedImportRows stores.context [ entries.category; user ]
        let context, puts, retirements = observeImportRows stores.context
        let before = importRowSnapshot context [ entries.category.content.id; user.content.id; forkEntryId ]
        let original = heldImportRow context user.content.id
        let builtIn = rowCatalogue CatalogueIds.builtInCatalogue entries.builtInName [ entries.category ]
        let rows = context.importCatalogue builtIn |> expectImportOk
        let categoryRow = Assert.Single rows
        Assert.Equal(entries.category.content.id, categoryRow.entryId)
        Assert.Equal(ReuseHeldEntry, categoryRow.defaultAction)
        assertImportRowsPending context puts retirements before rows
        rows |> List.map _.byDefault |> context.apply |> expectImportOk |> ignore
        Assert.Equal<(LibraryEntryId * HeldEntry option) list>(before, importRowSnapshot context (before |> List.map fst))
        Assert.Empty puts
        Assert.Empty retirements

        let incoming = nextImportEnvelope user
        let lab = rowCatalogue labCatalogueId labCatalogueName [ incoming ]
        let labRows = context.importCatalogue lab |> expectImportOk
        let row = Assert.Single labRows
        Assert.Equal(VersionNotHeld (incoming, original), row.status)
        Assert.Equal(AddIncomingVersion KeepLatestVersion, row.defaultAction)
        assertImportRowsPending context puts retirements before labRows
        context.apply [ row.byDefault ] |> expectImportOk |> ignore
        let expected =
            {
                original with
                    versions = importVersions [ user; incoming ]
            }
        Assert.Equal(expected, heldImportRow context user.content.id)
        Assert.Equal(VersionNumber.first, (heldImportRow context user.content.id).latest)
        Assert.Equal(incoming, (Assert.Single puts).envelope)
        Assert.Empty retirements

    [<Fact>]
    member this.``a row that names an entry awaiting a choice waits with it and nothing of either is written`` () : unit =
        let entries = readImportRowEntries ()
        let categoryId = CategoryId (Guid.Parse "31801b04-444f-43c8-8440-000000000096")
        let categoryContent =
            {
                entries.category.content with
                    id = categoryId.toLibraryEntryId
                    provenance = UserProvenance
            }
        let category = categoryContent.``sealed`` |> expectImportDatabaseOk
        let materialContent =
            {
                (userImportMaterial entries.silicon).content with
                    categoryOpt = Some categoryId
            }
        let material = materialContent.``sealed`` |> expectImportDatabaseOk
        let sourceId = LibraryEntryId (Guid.Parse "31801b05-444f-43c8-8440-000000000096")
        let sourceContent =
            {
                entries.source.content with
                    id = sourceId
                    name = EntryName "Lab source"
                    provenance = UserProvenance
            }
        let source = sourceContent.``sealed`` |> expectImportDatabaseOk
        let stores = this.freshImport ()
        seedImportRows stores.context [ entries.category ]
        let context, puts, retirements = observeImportRows stores.context
        let catalogue = rowCatalogue labCatalogueId labCatalogueName [ category; material; source ]
        let before = importRowSnapshot context [ entries.category.content.id; category.content.id; material.content.id; sourceId; forkEntryId ]
        let original = heldImportRow context entries.category.content.id
        let rows = context.importCatalogue catalogue |> expectImportOk
        Assert.Equal(3, rows.Length)
        let categoryRow = rows |> List.find (fun row -> row.entryId = category.content.id)
        let materialRow = rows |> List.find (fun row -> row.entryId = material.content.id)
        let sourceRow = rows |> List.find (fun row -> row.entryId = sourceId)
        Assert.Equal(NameCollision (category, [ original ]), categoryRow.status)
        Assert.Equal(EntryIdNotHeld material, materialRow.status)
        Assert.Equal(EntryIdNotHeld source, sourceRow.status)
        Assert.Equal<Set<LibraryEntryId>>(Set.singleton category.content.id, material.content.referencedEntryIds)
        assertImportRowsPending context puts retirements before rows
        for envelope in catalogue.envelopes do
            Assert.Equal<Result<HeldEntry option, CatalogueImportError>>(Ok None, context.proxy.tryHeldEntry envelope.content.id)

        rows |> List.map _.byDefault |> context.apply |> expectImportOk |> ignore
        for envelope in catalogue.envelopes do
            let held = heldImportRow context envelope.content.id
            Assert.Equal(envelope.content.version, held.latest)
            Assert.Equal<HeldEntryVersion list>(importVersions [ envelope ], held.versions)
        Assert.Equal(original, heldImportRow context original.id)
        let named = context.proxy.heldEntriesNamed CategoryEntryKind category.content.name |> expectImportOk
        Assert.Equal<Set<LibraryEntryId>>(Set.ofList [ original.id; category.content.id ], named |> List.map _.id |> Set.ofList)
        Assert.Equal<LibraryEntryEnvelope list>([ category; material; source ], puts |> Seq.map _.envelope |> Seq.toList)
        Assert.Empty retirements

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
    member this.``an imported version is what the database stores answer, and a retired built-in stays resolvable`` () : unit =
        let entries = readImportRowEntries ()
        let incoming = nextImportEnvelope entries.silicon
        let first = entries.silicon.content.toMaterialEntry |> decodeImportEntry
        let second = incoming.content.toMaterialEntry |> decodeImportEntry
        let stores = this.freshImport ()
        seedImportRows stores.context [ entries.category; entries.silicon ]
        let context, puts, retirements = observeImportRows stores.context
        let catalogue = rowCatalogue CatalogueIds.builtInCatalogue entries.builtInName [ entries.category; incoming ]
        let before = importRowSnapshot context [ entries.category.content.id; entries.silicon.content.id; forkEntryId ]
        let original = heldImportRow context entries.silicon.content.id
        let firstVersion = MaterialVersionId.firstOf MaterialIds.silicon
        let secondVersion =
            {
                firstVersion with
                    version = incoming.content.version
            }
        let rows = context.importCatalogue catalogue |> expectImportOk
        Assert.Equal(2, rows.Length)
        let updateRow = rows |> List.find (fun row -> row.entryId = original.id)
        Assert.Equal(VersionNotHeld (incoming, original), updateRow.status)
        Assert.Equal(AddIncomingVersion MoveLatestVersion, updateRow.defaultAction)
        assertImportRowsPending context puts retirements before rows
        Assert.Equal<Result<MaterialEntry option, MaterialError>>(Ok (Some first), stores.materials.tryGetMaterial MaterialIds.silicon)
        Assert.Equal<Result<MaterialEntry option, MaterialError>>(Ok None, stores.materials.resolveVersion secondVersion)

        rows |> List.map _.byDefault |> context.apply |> expectImportOk |> ignore
        Assert.Equal<Result<MaterialEntry option, MaterialError>>(Ok (Some second), stores.materials.tryGetMaterial MaterialIds.silicon)
        Assert.Equal<Result<MaterialEntry option, MaterialError>>(Ok (Some first), stores.materials.resolveVersion firstVersion)
        Assert.Equal<Result<MaterialEntry option, MaterialError>>(Ok (Some second), stores.materials.resolveVersion secondVersion)
        Assert.Equal(incoming, (Assert.Single puts).envelope)
        Assert.Empty retirements

        let retirementContext, retirementPuts, retirementCalls = observeImportRows stores.context
        let withoutSilicon = rowCatalogue CatalogueIds.builtInCatalogue entries.builtInName [ entries.category ]
        let beforeRetirement = importRowSnapshot retirementContext (before |> List.map fst)
        let latest = heldImportRow retirementContext original.id
        let retirementRows = retirementContext.importCatalogue withoutSilicon |> expectImportOk
        Assert.Equal(2, retirementRows.Length)
        let retirementRow = retirementRows |> List.find (fun row -> row.entryId = original.id)
        Assert.Equal(EntryUnlisted latest, retirementRow.status)
        Assert.Equal(RetireHeldEntry, retirementRow.defaultAction)
        assertImportRowsPending retirementContext retirementPuts retirementCalls beforeRetirement retirementRows
        Assert.Equal<Result<MaterialEntry list, MaterialError>>(Ok [ second ], stores.materials.listMaterials ActiveOnly)

        retirementRows |> List.map _.byDefault |> retirementContext.apply |> expectImportOk |> ignore
        let retired =
            {
                latest with
                    lifecycle = InactiveEntry
            }
        Assert.Equal(retired, heldImportRow retirementContext original.id)
        Assert.Equal<Result<MaterialEntry list, MaterialError>>(Ok [], stores.materials.listMaterials ActiveOnly)
        Assert.Equal<Result<MaterialEntry option, MaterialError>>(Ok (Some first), stores.materials.resolveVersion firstVersion)
        Assert.Equal<Result<MaterialEntry option, MaterialError>>(Ok (Some second), stores.materials.resolveVersion secondVersion)
        Assert.Empty retirementPuts
        Assert.Equal(original.id, Assert.Single retirementCalls)

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
        let logEntry =
            {
                catalogueId = CatalogueIds.builtInCatalogue
                folder = FolderName "b"
                manifestHash = envelope.contentHash
                importedAt = CatalogueImportTime (DateTime(2026, 11, 6, 12, 0, 0, DateTimeKind.Utc))
                entryCount = CatalogueEntryCount 1
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
        proxy.loggedImports () |> assertUnavailable
        proxy.logImport logEntry |> assertUnavailable
        proxy.forgetImport logEntry.catalogueId |> assertUnavailable

    interface IDisposable with
        member _.Dispose () : unit =
            for database in databases do
                (database :> IDisposable).Dispose()
