namespace OpticalConstructor.Tests

open System
open System.IO
open Softellect.Sys.Primitives
open Xunit
open OpticalConstructor.Domain.ItemModels
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
open OpticalConstructor.Storage.ItemModelDatabaseStore
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
        itemModels : ItemModelProxy
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

    let startupImportTime = CatalogueImportTime (DateTime(2026, 11, 6, 15, 30, 0, DateTimeKind.Utc))

    let deterministicImportContext (proxy : CatalogueImportProxy) : CatalogueImportContext =
        {
            CatalogueImportContext.create proxy with
                now = fun () -> startupImportTime
        }

    let freshImportFolder () : FolderName =
        FolderName (Path.Combine(AppContext.BaseDirectory, "CatalogueImportTests", Guid.NewGuid().ToString("N")))

    let writeDroppedEntry (folder : FolderName) (envelope : LibraryEntryEnvelope) : FileName =
        Directory.CreateDirectory folder.value |> ignore
        let file = envelope.content.fileName.combine folder
        let text = envelope.fileText |> expectImportDatabaseOk
        text.writeAtomically file |> expectImportDatabaseOk
        file

    let writeImportFolder (folder : FolderName) (catalogue : CatalogueData) : unit =
        (CatalogueProxy.create folder).writeCatalogue catalogue
        |> Result.defaultWith (fun error -> failwith error.describe)

    let writtenImportFolder (catalogue : CatalogueData) : FolderName =
        let folder = freshImportFolder ()
        writeImportFolder folder catalogue
        folder

    type StartupImportObservation =
        {
            context : CatalogueImportContext
            puts : ResizeArray<EntryVersionPut>
            retirements : ResizeArray<LibraryEntryId>
            logs : ResizeArray<CatalogueImportLogEntry>
            opened : ResizeArray<FolderName>
            manifestsRead : ResizeArray<FolderName>
            cataloguesRead : ResizeArray<FolderName>
        }

    let observeStartupImport (context : CatalogueImportContext) : StartupImportObservation =
        let observed, puts, retirements = observeImportRows context
        let logs = ResizeArray<CatalogueImportLogEntry>()
        let opened = ResizeArray<FolderName>()
        let manifestsRead = ResizeArray<FolderName>()
        let cataloguesRead = ResizeArray<FolderName>()
        {
            context =
                {
                    observed with
                        proxy =
                            {
                                observed.proxy with
                                    logImport = fun entry ->
                                        logs.Add entry
                                        observed.proxy.logImport entry
                            }
                        openCatalogue = fun folder ->
                            opened.Add folder
                            let catalogue = context.openCatalogue folder
                            {
                                catalogue with
                                    readManifest = fun () ->
                                        manifestsRead.Add folder
                                        catalogue.readManifest ()
                                    readCatalogue = fun () ->
                                        cataloguesRead.Add folder
                                        catalogue.readCatalogue ()
                            }
                }
            puts = puts
            retirements = retirements
            logs = logs
            opened = opened
            manifestsRead = manifestsRead
            cataloguesRead = cataloguesRead
        }

    let pendingImport (outcome : Result<FolderImportResult, CatalogueImportError>) : PendingCatalogueImport =
        match expectImportOk outcome with
        | CatalogueAwaitsChoices pending -> pending
        | _ -> failwith "A changed catalogue must await confirmation of every row."

    let assertNoStartupWrites (observed : StartupImportObservation) : unit =
        Assert.Empty observed.puts
        Assert.Empty observed.retirements
        Assert.Empty observed.logs

    let confirmImportDefaults (context : CatalogueImportContext) (pending : PendingCatalogueImport) : CatalogueImportLogEntry =
        pending.decisions |> List.map _.byDefault |> context.settle pending |> expectImportOk |> ignore
        context.proxy.loggedImports () |> expectImportOk
        |> List.find (fun entry -> entry.catalogueId = pending.entry.catalogueId)

    type LabImportFixture =
        {
            entries : ImportRowEntries
            user : LibraryEntryEnvelope
            collision : LibraryEntryEnvelope
            shipped : FolderName
            lab : CatalogueData
            labFolder : FolderName
        }

    let prepareLabImport (context : CatalogueImportContext) : LabImportFixture =
        let entries = readImportRowEntries ()
        let userContent =
            {
                (userImportMaterial entries.silicon).content with
                    name = EntryName "Lab glass"
            }
        let user = userContent.``sealed`` |> expectImportDatabaseOk
        let collisionContent =
            {
                user.content with
                    id = forkEntryId
            }
        let collision = collisionContent.``sealed`` |> expectImportDatabaseOk
        let builtIn = rowCatalogue CatalogueIds.builtInCatalogue entries.builtInName [ entries.category ]
        let shipped = writtenImportFolder builtIn
        let lab = rowCatalogue labCatalogueId labCatalogueName [ entries.category; user ]
        let first = context.importOnStart shipped |> expectImportOk
        confirmImportDefaults context (Assert.Single first.pending) |> ignore
        {
            entries = entries
            user = user
            collision = collision
            shipped = shipped
            lab = lab
            labFolder = writtenImportFolder lab
        }

    let changeLabImport (fixture : LabImportFixture) : CatalogueData =
        File.Delete((fixture.user.content.fileName.combine fixture.labFolder).value)
        let changed = rowCatalogue labCatalogueId labCatalogueName [ fixture.entries.category; fixture.collision; fixture.entries.source ]
        writeImportFolder fixture.labFolder changed
        changed

    let freshCatalogueExportTarget () : CatalogueExportTarget =
        let folder = FolderName (Path.Combine(AppContext.BaseDirectory, "LibraryExportTests", Guid.NewGuid().ToString("N")))
        CatalogueExportTarget.ofFolder folder

    let expectCatalogueExportOk (result : Result<'Outcome, LibraryExportError>) : 'Outcome =
        result |> Result.defaultWith (fun error -> failwith error.describe)

    type CatalogueExportFixture =
        {
            category : LibraryEntryContent
            material : LibraryEntryContent
            sample : LibraryEntryContent
        }

    let prepareCatalogueExport (context : CatalogueImportContext) : CatalogueExportFixture =
        context.importFrom (CatalogueProxy.createBuiltIn ()) |> expectImportOk
        |> List.map _.byDefault |> context.apply |> expectImportOk |> ignore
        let categoryId = CategoryId (Guid.Parse "31801b01-444f-43c8-8440-000000000117")
        let materialId = MaterialId (Guid.Parse "31801b02-444f-43c8-8440-000000000117")
        let sampleId = SampleId (Guid.Parse "31801b03-444f-43c8-8440-000000000117")
        let categoryName = CategoryName "Lab category"
        let materialName = EntryName "Lab glass"
        let sampleName = EntryName "Lab stack"
        let category = BuiltinCatalogue.userCategoryContent categoryId categoryName
        let material =
            {
                BuiltinCatalogue.userMaterialContent materialId materialName with
                    categoryOpt = Some categoryId
            }
        let sample = BuiltinCatalogue.sampleContentOver sampleId materialId sampleName
        BuiltinCatalogue.putSealedEntries context [ category; material; sample ]
        {
            category = category
            material = material
            sample = sample
        }

open CatalogueImportContractData

/// The same import contract runs over empty in-memory and SQLite stores (spec 0050 S3.1).
[<AbstractClass>]
type CatalogueImportContract () =

    abstract freshImport : unit -> ImportStoreSet

    [<Fact>]
    member this.``a catalogue export carries the user entries its entries reference, and importing it back reuses every entry`` () : unit =
        let context = (this.freshImport ()).context
        let fixture = prepareCatalogueExport context
        let target = freshCatalogueExportTarget ()
        Assert.False(Directory.Exists target.folder.value)
        let catalogueId = CatalogueId (Guid.Parse "31801b04-444f-43c8-8440-000000000117")
        let deterministic =
            {
                context with
                    newCatalogueId = fun () -> catalogueId
            }
        let exported = deterministic.exportEntries [ fixture.sample.id ] target |> expectCatalogueExportOk
        let expected =
            [ fixture.category; fixture.material; fixture.sample ]
            |> List.map (fun content ->
                match context.proxy.tryLatestEnvelope content.id |> expectImportOk with
                | Some envelope -> envelope
                | None -> failwith $"Expected latest envelope for {content.describe}.")
        Assert.Equal<LibraryEntryEnvelope list>(expected |> List.sortBy _.content.id, exported.envelopes |> List.sortBy _.content.id)
        Assert.Equal(catalogueId, exported.manifest.id)
        Assert.Equal(target.name, exported.manifest.name)
        Assert.Equal(VersionNumber.first, exported.manifest.version)
        let readBack = (CatalogueProxy.create target.folder).readCatalogue () |> Result.defaultWith (fun error -> failwith error.describe)
        Assert.Equal(exported, readBack)
        let before = context.proxy.heldUserEntries () |> expectImportOk
        let logsBefore = context.proxy.loggedImports () |> expectImportOk
        let observed = observeStartupImport context
        let decisions = observed.context.proxy.decideCatalogue readBack |> expectImportOk
        Assert.Equal(3, decisions.Length)
        for row in decisions do
            let envelope = expected |> List.find (fun envelope -> envelope.content.id = row.entryId)
            Assert.Equal(SameContentHash (envelope, heldImportRow context row.entryId), row.status)
            Assert.Equal(ReuseHeldEntry, row.defaultAction)
        let pending = observed.context.importCatalogue readBack |> expectImportOk
        Assert.Equal<ImportDecision list>(decisions, pending)
        Assert.Equal<ImportDecision list>(decisions, (ImportDecision.split pending).awaiting)
        Assert.Empty observed.puts
        Assert.Empty observed.retirements
        Assert.Empty observed.logs
        Assert.Equal<HeldEntry list>(before, context.proxy.heldUserEntries () |> expectImportOk)
        Assert.Equal<CatalogueImportLogEntry list>(logsBefore, context.proxy.loggedImports () |> expectImportOk)
        pending |> List.map _.byDefault |> observed.context.apply |> expectImportOk |> ignore
        Assert.Equal<HeldEntry list>(before, context.proxy.heldUserEntries () |> expectImportOk)
        Assert.Empty observed.puts
        Assert.Empty observed.retirements
        Assert.Empty observed.logs

        let duplicated = context.exportEntries [ fixture.sample.id; fixture.material.id; fixture.sample.id ] (freshCatalogueExportTarget ()) |> expectCatalogueExportOk
        Assert.Equal<LibraryEntryEnvelope list>(exported.envelopes, duplicated.envelopes)
        let siliconSampleId = SampleId (Guid.Parse "31801b05-444f-43c8-8440-000000000117")
        let siliconSampleName = EntryName "Silicon stack"
        let siliconSample = BuiltinCatalogue.sampleContentOver siliconSampleId MaterialIds.silicon siliconSampleName
        BuiltinCatalogue.putSealedEntries context [ siliconSample ]
        let siliconExport = context.exportEntries [ siliconSample.id ] (freshCatalogueExportTarget ()) |> expectCatalogueExportOk
        Assert.Equal(siliconSample.id, (Assert.Single siliconExport.envelopes).content.id)

    [<Fact>]
    member this.``a catalogue export pins every exported version`` () : unit =
        let stores = this.freshImport ()
        let context = stores.context
        let fixture = prepareCatalogueExport context
        let target = freshCatalogueExportTarget ()
        let pins = ResizeArray<LibraryEntryId * VersionNumber>()
        let observed =
            {
                context with
                    proxy =
                        {
                            context.proxy with
                                pinEntryVersion = fun id version ->
                                    Assert.False(Directory.Exists target.folder.value)
                                    context.proxy.pinEntryVersion id version
                                    |> Result.map (fun () -> pins.Add(id, version))
                        }
                    openCatalogue = fun folder ->
                        let catalogue = context.openCatalogue folder
                        {
                            catalogue with
                                writeCatalogue = fun data ->
                                    Assert.Equal<Set<LibraryEntryId * VersionNumber>>(
                                        data.envelopes |> List.map (fun envelope -> envelope.content.id, envelope.content.version) |> Set.ofList,
                                        pins |> Set.ofSeq)
                                    Assert.Equal(data.envelopes.Length, pins.Count)
                                    catalogue.writeCatalogue data
                        }
            }
        observed.exportEntries [ fixture.sample.id ] target |> expectCatalogueExportOk |> ignore
        let material = fixture.material.toMaterialEntry |> decodeImportEntry
        let langasite = BuiltinCatalogue.material MaterialIds.langasite
        let edited =
            {
                material with
                    physics = langasite.physics
            }
        Assert.Equal<Result<unit, MaterialError>>(Ok (), stores.materials.saveMaterial edited)
        let first = MaterialVersionId.firstOf material.id
        let second =
            {
                first with
                    version = first.version.next
            }
        Assert.Equal<Result<MaterialEntry option, MaterialError>>(Ok (Some material), stores.materials.resolveVersion first)
        Assert.Equal<Result<MaterialEntry option, MaterialError>>(Ok (Some edited), stores.materials.resolveVersion second)
        let sample = fixture.sample.toSample |> decodeImportEntry
        match stores.samples.removeSample sample.id with
        | Error (SampleVersionPinnedErr reason) ->
            Assert.Contains(sample.name.value, reason)
            Assert.Contains("v1", reason)
        | _ -> Assert.Fail("A catalogue export must pin the sample's version against removal.")

    [<Fact>]
    member this.``a catalogue export of two entries of one kind with one file name is refused, naming both, and pins and writes nothing`` () : unit =
        let stores = this.freshImport ()
        let context = stores.context
        context.importFrom (CatalogueProxy.createBuiltIn ()) |> expectImportOk
        |> List.map _.byDefault |> context.apply |> expectImportOk |> ignore
        let firstId = MaterialId (Guid.Parse "31801b06-444f-43c8-8440-000000000117")
        let secondId = MaterialId (Guid.Parse "31801b07-444f-43c8-8440-000000000117")
        let name = EntryName "Lab glass"
        let first = BuiltinCatalogue.userMaterialContent firstId name
        let second = BuiltinCatalogue.userMaterialContent secondId name
        BuiltinCatalogue.putSealedEntries context [ first; second ]
        let target = freshCatalogueExportTarget ()
        let refusing =
            {
                context with
                    proxy =
                        {
                            context.proxy with
                                pinEntryVersion = fun _ _ -> failwith "Duplicate file names must be refused before pinning."
                        }
                    openCatalogue = fun _ -> failwith "Duplicate file names must be refused before inspecting the target."
            }
        match refusing.exportEntries [ first.id; second.id ] target with
        | Error (ExportCatalogueRefusedErr (InvalidCatalogueEntriesErr (DuplicateEntryFileNameErr reason))) ->
            Assert.Contains(name.value, reason)
            Assert.Contains(first.id.value.ToString(), reason)
            Assert.Contains(second.id.value.ToString(), reason)
        | _ -> Assert.Fail("Duplicate file names must name both exported entries in the catalogue refusal.")
        Assert.False(Directory.Exists target.folder.value)
        let material = first.toMaterialEntry |> decodeImportEntry
        let langasite = BuiltinCatalogue.material MaterialIds.langasite
        let edited =
            {
                material with
                    physics = langasite.physics
            }
        Assert.Equal<Result<unit, MaterialError>>(Ok (), stores.materials.saveMaterial edited)
        let firstVersion = MaterialVersionId.firstOf firstId
        let secondVersion =
            {
                firstVersion with
                    version = firstVersion.version.next
            }
        Assert.Equal<Result<MaterialEntry option, MaterialError>>(Ok (Some edited), stores.materials.resolveVersion firstVersion)
        Assert.Equal<Result<MaterialEntry option, MaterialError>>(Ok None, stores.materials.resolveVersion secondVersion)

    [<Fact>]
    member this.``a catalogue export refuses a folder that already holds a catalogue`` () : unit =
        let context = (this.freshImport ()).context
        let fixture = prepareCatalogueExport context
        let target = freshCatalogueExportTarget ()
        context.exportEntries [ fixture.material.id ] target |> expectCatalogueExportOk |> ignore
        let texts () : Map<FileName, LibraryFileText> =
            Directory.GetFiles target.folder.value
            |> Array.map FileName
            |> Array.map (fun file -> file, LibraryFileText (File.ReadAllText file.value))
            |> Map.ofArray
        let refusing =
            {
                context with
                    proxy =
                        {
                            context.proxy with
                                pinEntryVersion = fun _ _ -> failwith "An occupied folder must be refused before pinning."
                        }
            }
        let assertOccupied () : unit =
            let before = texts ()
            match refusing.exportEntries [ fixture.sample.id ] target with
            | Error (ExportFolderHoldsCatalogueErr reason) -> Assert.Contains(target.folder.value, reason)
            | _ -> Assert.Fail("A catalogue export must not overwrite an existing manifest.")
            Assert.Equal<Map<FileName, LibraryFileText>>(before, texts ())
        assertOccupied ()
        let manifestFile = CatalogueManifest.fileName.combine target.folder
        File.WriteAllText(manifestFile.value, "invalid manifest")
        assertOccupied ()

    [<Fact>]
    member this.``Export my library writes every user entry, inactive ones included, and no built-in`` () : unit =
        let context = (this.freshImport ()).context
        let fixture = prepareCatalogueExport context
        let ptbId = MaterialId (Guid.Parse "31801b08-444f-43c8-8440-000000000117")
        let ptbName = EntryName "PTB glass"
        let licence = EntryLicence "CC BY 4.0"
        let citation = EntryCitation "PTB optical constants"
        let ptb = BuiltinCatalogue.licensedMaterialContent ptbId ptbName licence citation
        BuiltinCatalogue.putSealedEntries context [ ptb ]
        context.proxy.retireEntry ptb.id |> expectImportOk
        let held = context.proxy.heldUserEntries () |> expectImportOk
        Assert.Equal(4, held.Length)
        Assert.Equal(InactiveEntry, (held |> List.find (fun entry -> entry.id = ptb.id)).lifecycle)
        let target = freshCatalogueExportTarget ()
        let exported = context.exportUserLibrary target |> expectCatalogueExportOk
        Assert.Equal<Set<LibraryEntryId>>(held |> List.map _.id |> Set.ofList, exported.envelopes |> List.map _.content.id |> Set.ofList)
        Assert.Equal<Set<LibraryEntryId>>(
            Set.ofList [ fixture.category.id; fixture.material.id; fixture.sample.id; ptb.id ],
            exported.envelopes |> List.map _.content.id |> Set.ofList)
        for envelope in exported.envelopes do
            match envelope.content.provenance with
            | BuiltInProvenance _ -> Assert.Fail("Export my library must omit the shipped entries.")
            | _ -> ()
            Assert.Equal<LibraryEntryEnvelope option>(Some envelope, context.proxy.tryLatestEnvelope envelope.content.id |> expectImportOk)
        let readBack = (CatalogueProxy.create target.folder).readCatalogue () |> Result.defaultWith (fun error -> failwith error.describe)
        Assert.Equal(exported, readBack)

    [<Fact>]
    member this.``an export with nothing to write is refused`` () : unit =
        let context = (this.freshImport ()).context
        context.importFrom (CatalogueProxy.createBuiltIn ()) |> expectImportOk
        |> List.map _.byDefault |> context.apply |> expectImportOk |> ignore
        let target = freshCatalogueExportTarget ()
        let refusing =
            {
                context with
                    proxy =
                        {
                            context.proxy with
                                pinEntryVersion = fun _ _ -> failwith "An empty export must not pin."
                        }
                    openCatalogue = fun _ -> failwith "An empty export must not open its target."
                    newCatalogueId = fun () -> failwith "An empty export must not mint a catalogue identity."
            }
        for result in [ refusing.exportEntries [] target; refusing.exportUserLibrary target ] do
            match result with
            | Error (EmptyLibraryExportErr reason) -> Assert.Contains(target.folder.value, reason)
            | _ -> Assert.Fail("An empty catalogue export must be refused with its destination.")
        Assert.False(Directory.Exists target.folder.value)

    [<Fact>]
    member this.``exporting one entry pins its latest version and writes its entry file, which a drop decides as a reuse`` () : unit =
        let stores = this.freshImport ()
        let context = stores.context
        Assert.Equal<FolderName option>(None, context.exportFolderOpt)
        context.importFrom (CatalogueProxy.createBuiltIn ()) |> expectImportOk
        |> List.map _.byDefault |> context.apply |> expectImportOk |> ignore
        let labId = MaterialId (Guid.Parse "31801b01-444f-43c8-8440-000000000116")
        let labName = EntryName "Lab glass"
        let lab = BuiltinCatalogue.userMaterialContent labId labName
        BuiltinCatalogue.putSealedEntries context [ lab ]
        let expected = context.sealEntry lab |> expectImportOk
        let folder = FolderName (Path.Combine(AppContext.BaseDirectory, "LibraryExportTests", Guid.NewGuid().ToString("N")))
        Directory.CreateDirectory folder.value |> ignore
        let file = lab.fileName.combine folder
        let pins = ResizeArray<LibraryEntryId * VersionNumber>()
        let observed =
            {
                context with
                    proxy =
                        {
                            context.proxy with
                                pinEntryVersion = fun id version ->
                                    context.proxy.pinEntryVersion id version
                                    |> Result.map (fun () -> pins.Add(id, version))
                        }
                    writeEntryFile = fun destination envelope ->
                        Assert.Equal<LibraryEntryId * VersionNumber>(
                            (envelope.content.id, envelope.content.version), Assert.Single pins)
                        context.writeEntryFile destination envelope
            }
        let exported = observed.exportEntry lab.id file |> Result.defaultWith (fun error -> failwith error.describe)
        Assert.Equal(expected, exported)
        Assert.Equal(expected, context.readEntryFile MaterialEntryKind file |> expectImportOk)
        let row = context.decideDroppedFiles [ file ] |> expectImportOk |> Assert.Single
        Assert.Equal(SameContentHash (expected, heldImportRow context lab.id), row.status)
        Assert.Equal(ReuseHeldEntry, row.defaultAction)

        let material = lab.toMaterialEntry |> decodeImportEntry
        let langasite = BuiltinCatalogue.material MaterialIds.langasite
        let edited =
            {
                material with
                    physics = langasite.physics
            }
        Assert.Equal<Result<unit, MaterialError>>(Ok (), stores.materials.saveMaterial edited)
        let first = MaterialVersionId.firstOf labId
        let second =
            {
                first with
                    version = first.version.next
            }
        Assert.Equal<Result<MaterialEntry option, MaterialError>>(Ok (Some material), stores.materials.resolveVersion first)
        Assert.Equal<Result<MaterialEntry option, MaterialError>>(Ok (Some edited), stores.materials.resolveVersion second)
        Assert.Equal(expected, context.readEntryFile MaterialEntryKind file |> expectImportOk)
        pins.Clear()
        let siliconFile = FileName (Path.Combine(folder.value, "Silicon.material-json"))
        observed.exportEntry MaterialIds.silicon.toLibraryEntryId siliconFile
        |> Result.defaultWith (fun error -> failwith error.describe) |> ignore
        let shipped = FileName (Path.Combine(AppContext.BaseDirectory, "Catalogue", "Builtin", "Silicon.material-json"))
        Assert.Equal<byte>(File.ReadAllBytes shipped.value, File.ReadAllBytes siliconFile.value)

    [<Fact>]
    member this.``exporting an entry the library does not hold is refused and writes nothing`` () : unit =
        let context = (this.freshImport ()).context
        let missingId = LibraryEntryId (Guid.Parse "31801b02-444f-43c8-8440-000000000116")
        let folder = FolderName (Path.Combine(AppContext.BaseDirectory, "LibraryExportTests", Guid.NewGuid().ToString("N")))
        Directory.CreateDirectory folder.value |> ignore
        let file = FileName (Path.Combine(folder.value, "Missing.material-json"))
        let unwritable =
            {
                context with
                    proxy =
                        {
                            context.proxy with
                                tryHeldEnvelope = fun _ _ -> failwith "An unheld entry must not read an envelope."
                                pinEntryVersion = fun _ _ -> failwith "An unheld entry must not be pinned."
                        }
                    writeEntryFile = fun _ _ -> failwith "A refused export must not write a file."
            }
        match unwritable.exportEntry missingId file with
        | Error (ExportedEntryNotHeldErr reason) -> Assert.Contains(missingId.value.ToString(), reason)
        | _ -> Assert.Fail("An unheld entry must be refused with its id.")

        context.importFrom (CatalogueProxy.createBuiltIn ()) |> expectImportOk
        |> List.map _.byDefault |> context.apply |> expectImportOk |> ignore
        let id = MaterialIds.silicon.toLibraryEntryId
        let noPin =
            {
                context.proxy with
                    pinEntryVersion = fun _ _ -> failwith "A failed envelope read must not pin a version."
            }
        let readError = ImportStoreUnavailableErr "the export metadata read failed"
        let envelopeError = ImportRejectedErr "the export envelope read failed"
        let pinError = ImportRejectedErr "the export version could not be pinned"
        let refused =
            [
                {
                    noPin with
                        tryHeldEntry = fun _ -> Error readError
                        tryHeldEnvelope = fun _ _ -> failwith "A failed metadata read must not read an envelope."
                }, readError
                {
                    noPin with
                        tryHeldEnvelope = fun _ _ -> Error envelopeError
                }, envelopeError
                {
                    context.proxy with
                        pinEntryVersion = fun _ _ -> Error pinError
                }, pinError
            ]
        for proxy, error in refused do
            let failing =
                {
                    unwritable with
                        proxy = proxy
                }
            Assert.Equal<Result<LibraryEntryEnvelope, LibraryExportError>>(
                Error (ExportLibraryRefusedErr error), failing.exportEntry id file)
        let absentEnvelope =
            {
                unwritable with
                    proxy =
                        {
                            noPin with
                                tryHeldEnvelope = fun _ _ -> Ok None
                        }
            }
        match absentEnvelope.exportEntry id file with
        | Error (ExportedEntryNotHeldErr reason) -> Assert.Contains(id.value.ToString(), reason)
        | _ -> Assert.Fail("An absent latest envelope must be refused with its id.")
        let invalidFile = FileName (Path.Combine(folder.value, "absent", "Silicon.material-json"))
        match context.exportEntry id invalidFile with
        | Error (ExportFileWriteErr reason) ->
            Assert.Contains(invalidFile.value, reason)
            Assert.Contains($"{invalidFile.value}.tmp", reason)
        | _ -> Assert.Fail("A failed atomic write must name its destination and storage error.")
        Assert.Empty(Directory.EnumerateFileSystemEntries folder.value)

    [<Fact>]
    member this.``heldUserEntries answers every entry whose latest version is not a built-in`` () : unit =
        let context = (this.freshImport ()).context
        Assert.Empty(context.proxy.heldUserEntries () |> expectImportOk)
        context.importFrom (CatalogueProxy.createBuiltIn ()) |> expectImportOk
        |> List.map _.byDefault |> context.apply |> expectImportOk |> ignore
        Assert.Empty(context.proxy.heldUserEntries () |> expectImportOk)
        let labId = MaterialId (Guid.Parse "31801b01-444f-43c8-8440-000000000115")
        let ptbId = MaterialId (Guid.Parse "31801b02-444f-43c8-8440-000000000115")
        let forkId = LibraryEntryId (Guid.Parse "31801b03-444f-43c8-8440-000000000115")
        let labName = EntryName "Lab glass"
        let ptbName = EntryName "PTB glass"
        let licence = EntryLicence "CC-BY-4.0"
        let citation = EntryCitation "A. Author, Test optical constants (2026), doi:10.0000/test"
        let lab = BuiltinCatalogue.userMaterialContent labId labName
        let ptb = BuiltinCatalogue.licensedMaterialContent ptbId ptbName licence citation
        let fork =
            {
                ptb with
                    id = forkId
                    name = EntryName "PTB glass (fork)"
                    provenance =
                        ForkProvenance
                            {
                                originalId = ptb.id
                                originalVersion = ptb.version
                                originalProvenance = ptb.provenance
                            }
            }
        let contents = [ lab; ptb; fork ]
        BuiltinCatalogue.putSealedEntries context contents
        context.proxy.retireEntry lab.id |> expectImportOk
        let held = context.proxy.heldUserEntries () |> expectImportOk
        Assert.Equal<Set<LibraryEntryId>>(contents |> List.map _.id |> Set.ofList, held |> List.map _.id |> Set.ofList)
        for content in contents do
            let entry = held |> List.find (fun entry -> entry.id = content.id)
            Assert.Equal(content.name, entry.name)
            Assert.Equal<HeldEntry option>(Some entry, context.proxy.tryHeldEntry content.id |> expectImportOk)
        Assert.Equal(InactiveEntry, (held |> List.find (fun entry -> entry.id = lab.id)).lifecycle)

    [<Fact>]
    member this.``pinEntryVersion pins a held version so that its next physics edit mints a version, and refuses a version not held`` () : unit =
        let stores = this.freshImport ()
        let context = stores.context
        context.importFrom (CatalogueProxy.createBuiltIn ()) |> expectImportOk
        |> List.map _.byDefault |> context.apply |> expectImportOk |> ignore
        let labId = MaterialId (Guid.Parse "31801b04-444f-43c8-8440-000000000115")
        let sampleId = SampleId (Guid.Parse "31801b05-444f-43c8-8440-000000000115")
        let missingId = LibraryEntryId (Guid.Parse "31801b06-444f-43c8-8440-000000000115")
        let labName = EntryName "Lab glass"
        let sampleName = EntryName "Lab stack"
        let lab = BuiltinCatalogue.userMaterialContent labId labName
        BuiltinCatalogue.putSealedEntries context [ lab ]
        Assert.Equal<Result<unit, CatalogueImportError>>(Ok (), context.proxy.pinEntryVersion lab.id lab.version)
        Assert.Equal<Result<unit, CatalogueImportError>>(Ok (), context.proxy.pinEntryVersion lab.id lab.version)
        let material = lab.toMaterialEntry |> decodeImportEntry
        let langasite = BuiltinCatalogue.material MaterialIds.langasite
        let edited =
            {
                material with
                    physics = langasite.physics
            }
        Assert.Equal<Result<unit, MaterialError>>(Ok (), stores.materials.saveMaterial edited)
        let first = MaterialVersionId.firstOf labId
        let second : MaterialVersionId =
            {
                first with
                    version = first.version.next
            }
        match stores.materials.resolveVersion first with
        | Ok (Some original) -> Assert.Equal((BuiltinCatalogue.material MaterialIds.silicon).physics, original.physics)
        | _ -> Assert.Fail("Pinned version 1 must remain resolvable with Silicon's physics.")
        match stores.materials.resolveVersion second with
        | Ok (Some minted) -> Assert.Equal(langasite.physics, minted.physics)
        | _ -> Assert.Fail("Editing the pinned material must mint version 2 with Langasite's physics.")

        let sample = BuiltinCatalogue.sampleContentOver sampleId labId sampleName
        BuiltinCatalogue.putSealedEntries context [ sample ]
        Assert.Equal<Result<unit, CatalogueImportError>>(Ok (), context.proxy.pinEntryVersion sample.id sample.version)
        match stores.samples.removeSample sampleId with
        | Error (SampleVersionPinnedErr reason) ->
            Assert.Contains(sample.name.value, reason)
            Assert.Contains("v1", reason)
        | _ -> Assert.Fail("Pinning a sample through the import seam must prevent its removal.")

        let third = second.version.next
        for id, version in [ lab.id, third; missingId, VersionNumber.first ] do
            match context.proxy.pinEntryVersion id version with
            | Error (ImportRejectedErr reason) ->
                Assert.Contains(id.value.ToString(), reason)
                Assert.Contains($"version {version.value}", reason)
            | _ -> Assert.Fail("A version not held must be rejected with its id and version.")

    [<Fact>]
    member this.``attributionLinesOf names each held entry whose provenance carries a licence, in the order asked`` () : unit =
        let context = (this.freshImport ()).context
        context.importFrom (CatalogueProxy.createBuiltIn ()) |> expectImportOk
        |> List.map _.byDefault |> context.apply |> expectImportOk |> ignore
        let ptbId = MaterialId (Guid.Parse "31801b01-444f-43c8-8440-000000000114")
        let riiId = MaterialId (Guid.Parse "31801b02-444f-43c8-8440-000000000114")
        let labId = MaterialId (Guid.Parse "31801b03-444f-43c8-8440-000000000114")
        let forkId = LibraryEntryId (Guid.Parse "31801b04-444f-43c8-8440-000000000114")
        let missingId = LibraryEntryId (Guid.Parse "31801b05-444f-43c8-8440-000000000114")
        let ptbName = EntryName "PTB glass"
        let riiName = EntryName "RII glass"
        let labName = EntryName "Lab glass"
        let ccBy = EntryLicence "CC-BY-4.0"
        let cc0 = EntryLicence "CC0-1.0"
        let citation = EntryCitation "A. Author, Test optical constants (2026), doi:10.0000/test"
        let ptb = BuiltinCatalogue.licensedMaterialContent ptbId ptbName ccBy citation
        let rii = BuiltinCatalogue.licensedMaterialContent riiId riiName cc0 citation
        let fork =
            {
                ptb with
                    id = forkId
                    name = EntryName "PTB glass (fork)"
                    provenance =
                        ForkProvenance
                            {
                                originalId = ptb.id
                                originalVersion = ptb.version
                                originalProvenance = ptb.provenance
                            }
            }
        let lab = BuiltinCatalogue.userMaterialContent labId labName
        BuiltinCatalogue.putSealedEntries context [ ptb; rii; fork; lab ]
        let ids = [ lab.id; MaterialIds.silicon.toLibraryEntryId; ptb.id; rii.id; fork.id; missingId; ptb.id ]
        let before = importRowSnapshot context ids
        let observed = observeStartupImport context
        let reads = ResizeArray<LibraryEntryId>()
        let proxy =
            {
                observed.context.proxy with
                    tryHeldEntry = fun id ->
                        reads.Add id
                        context.proxy.tryHeldEntry id
                    tryHeldEnvelope = fun _ _ -> failwith "Attribution reads the latest metadata, not version envelopes."
                    forgetImport = fun _ -> failwith "Attribution must not change catalogue registrations."
            }
        Assert.Empty(proxy.attributionLinesOf [] |> expectImportOk)
        Assert.Empty reads
        let lines = proxy.attributionLinesOf ids |> expectImportOk
        Assert.Equal<string list>(
            [
                "PTB glass: Licence CC-BY-4.0 — cite: A. Author, Test optical constants (2026), doi:10.0000/test"
                "RII glass: Licence CC0-1.0"
                "PTB glass (fork): Licence CC-BY-4.0 — cite: A. Author, Test optical constants (2026), doi:10.0000/test"
            ], lines |> List.map _.text)
        Assert.Equal<LibraryEntryId list>(
            [ lab.id; MaterialIds.silicon.toLibraryEntryId; ptb.id; rii.id; fork.id; missingId ], List.ofSeq reads)
        Assert.Equal<(LibraryEntryId * HeldEntry option) list>(before, importRowSnapshot context ids)
        assertNoStartupWrites observed

        let updated =
            {
                ptb with
                    version = ptb.version.next
                    provenance = rii.provenance
            }
        BuiltinCatalogue.putSealedEntries context [ updated ]
        let latest = proxy.attributionLinesOf [ ptb.id ] |> expectImportOk |> Assert.Single
        Assert.Equal(ptb.name, latest.entryName)
        Assert.Equal<EntryAttribution>(LicenceNameOnly cc0, latest.attribution)

        let failure = ImportStoreUnavailableErr "attribution lookup failed"
        let failedReads = ResizeArray<LibraryEntryId>()
        let failingProxy =
            {
                proxy with
                    tryHeldEntry = fun id ->
                        failedReads.Add id
                        if id = missingId then Error failure
                        else context.proxy.tryHeldEntry id
            }
        Assert.Equal<Result<EntryAttributionLine list, CatalogueImportError>>(
            Error failure, failingProxy.attributionLinesOf [ ptb.id; missingId; rii.id ])
        Assert.Equal<LibraryEntryId list>([ ptb.id; missingId ], List.ofSeq failedReads)
        assertNoStartupWrites observed

    [<Fact>]
    member this.``tryHeldEnvelope answers every held version's envelope and nothing for a version not held`` () : unit =
        let context = (this.freshImport ()).context
        context.importFrom (CatalogueProxy.createBuiltIn ()) |> expectImportOk
        |> List.map _.byDefault |> context.apply |> expectImportOk |> ignore
        let silicon = (readImportRowEntries ()).silicon
        let incoming = nextImportEnvelope silicon
        let put =
            {
                envelope = incoming
                latest = KeepLatestVersion
            }
        context.proxy.putEntryVersion put |> expectImportOk
        let siliconId = MaterialIds.silicon.toLibraryEntryId
        let missingVersion = incoming.content.version.next
        let missingId = LibraryEntryId (Guid.Parse "31801b05-444f-43c8-8440-000000000105")
        let assertEnvelopes () : unit =
            Assert.Equal<LibraryEntryEnvelope option>(Some silicon, context.proxy.tryHeldEnvelope siliconId silicon.content.version |> expectImportOk)
            Assert.Equal<LibraryEntryEnvelope option>(Some incoming, context.proxy.tryHeldEnvelope siliconId incoming.content.version |> expectImportOk)
            Assert.Equal<LibraryEntryEnvelope option>(None, context.proxy.tryHeldEnvelope siliconId missingVersion |> expectImportOk)
            Assert.Equal<LibraryEntryEnvelope option>(None, context.proxy.tryHeldEnvelope missingId VersionNumber.first |> expectImportOk)
        Assert.Equal(VersionNumber.first, (heldImportRow context siliconId).latest)
        assertEnvelopes ()
        context.proxy.retireEntry siliconId |> expectImportOk
        Assert.Equal(InactiveEntry, (heldImportRow context siliconId).lifecycle)
        assertEnvelopes ()

    [<Fact>]
    member this.``dropped entry files are decided against the library, each under its own file name`` () : unit =
        let stores = this.freshImport ()
        stores.context.importFrom (CatalogueProxy.createBuiltIn ()) |> expectImportOk
        |> List.map _.byDefault |> stores.context.apply |> expectImportOk |> ignore
        let entries = readImportRowEntries ()
        let incoming = userImportMaterial entries.silicon
        let folder = freshImportFolder ()
        let materialFile = writeDroppedEntry folder incoming
        let siliconFile = writeDroppedEntry folder entries.silicon
        let observed = observeStartupImport stores.context
        let reads = ResizeArray<LibraryEntryKind * FileName>()
        let context =
            {
                observed.context with
                    readEntryFile = fun kind file ->
                        reads.Add(kind, file)
                        stores.context.readEntryFile kind file
            }
        let before = importRowSnapshot context [ incoming.content.id; entries.silicon.content.id ]
        let decide : FileName list -> Result<ImportDecision list, CatalogueImportError> = context.decideDroppedFiles
        Assert.Empty(decide [] |> expectImportOk)
        Assert.Empty reads
        let rows = decide [ materialFile; siliconFile ] |> expectImportOk
        Assert.Equal<LibraryEntryId list>([ incoming.content.id; entries.silicon.content.id ], rows |> List.map _.entryId)
        Assert.Equal<ImportOrigin list>([ DroppedFileOrigin materialFile.leafName; DroppedFileOrigin siliconFile.leafName ], rows |> List.map _.origin)
        Assert.Equal<ImportStatus list>([ EntryIdNotHeld incoming; SameContentHash (entries.silicon, heldImportRow context entries.silicon.content.id) ], rows |> List.map _.status)
        Assert.Equal<(LibraryEntryKind * FileName) list>([ MaterialEntryKind, materialFile; MaterialEntryKind, siliconFile ], List.ofSeq reads)
        assertImportRowsPending context observed.puts observed.retirements before rows
        assertNoStartupWrites observed
        Assert.Equal<Result<HeldEntry option, CatalogueImportError>>(Ok None, context.proxy.tryHeldEntry incoming.content.id)

        let failure = ImportStoreUnavailableErr "dropped-entry lookup failed"
        let lookups = ResizeArray<LibraryEntryId>()
        let failingContext =
            {
                context with
                    proxy =
                        {
                            context.proxy with
                                tryHeldEntry = fun id ->
                                    lookups.Add id
                                    if id = incoming.content.id then Error failure
                                    else context.proxy.tryHeldEntry id
                        }
            }
        Assert.Equal<Result<ImportDecision list, CatalogueImportError>>(Error failure, failingContext.decideDroppedFiles [ materialFile; siliconFile ])
        Assert.Equal(incoming.content.id, lookups |> Seq.last)
        Assert.DoesNotContain(entries.silicon.content.id, lookups)
        assertNoStartupWrites observed

    [<Fact>]
    member this.``a dropped file that is not a library entry file or does not read is refused, naming every such file`` () : unit =
        let stores = this.freshImport ()
        stores.context.importFrom (CatalogueProxy.createBuiltIn ()) |> expectImportOk
        |> List.map _.byDefault |> stores.context.apply |> expectImportOk |> ignore
        let entries = readImportRowEntries ()
        let incoming = userImportMaterial entries.silicon
        let folder = freshImportFolder ()
        let materialFile = writeDroppedEntry folder incoming
        let notesName = FileName "notes.txt"
        let missingName = FileName "missing.material-json"
        let notes = notesName.combine folder
        let missing = missingName.combine folder
        File.WriteAllText(notes.value, "Not a library entry")
        let observed = observeStartupImport stores.context
        let reads = ResizeArray<FileName>()
        let lookups = ResizeArray<LibraryEntryId>()
        let context =
            {
                observed.context with
                    readEntryFile = fun kind file ->
                        reads.Add file
                        stores.context.readEntryFile kind file
                    proxy =
                        {
                            observed.context.proxy with
                                tryHeldEntry = fun id ->
                                    lookups.Add id
                                    observed.context.proxy.tryHeldEntry id
                        }
            }
        let readFailure =
            match stores.context.readEntryFile MaterialEntryKind missing with
            | Error (DroppedFileRefusedErr reason) ->
                Assert.Contains(missing.value, reason)
                reason
            | _ -> failwith "A missing entry file must become DroppedFileRefusedErr."
        match context.decideDroppedFiles [ notes; missing; materialFile ] with
        | Error (DroppedFileRefusedErr reason) ->
            Assert.Equal($"{notes.value}: not a library entry file\n{readFailure}", reason)
            Assert.DoesNotContain(materialFile.leafName.value, reason)
        | _ -> Assert.Fail("Every refused file must be reported together.")
        Assert.Equal<FileName list>([ missing; materialFile ], List.ofSeq reads)
        Assert.Empty lookups

        let refusal = DroppedFileRefusedErr $"{materialFile.value}: injected reader refusal"
        let injected = { context with readEntryFile = fun _ _ -> Error refusal }
        Assert.Equal<Result<ImportDecision list, CatalogueImportError>>(Error refusal, injected.decideDroppedFiles [ materialFile ])
        Assert.Empty lookups
        assertNoStartupWrites observed
        Assert.Equal<Result<HeldEntry option, CatalogueImportError>>(Ok None, stores.context.proxy.tryHeldEntry incoming.content.id)

    [<Fact>]
    member this.``two dropped files of one id and version are one row when equal and refused when they differ`` () : unit =
        let stores = this.freshImport ()
        stores.context.importFrom (CatalogueProxy.createBuiltIn ()) |> expectImportOk
        |> List.map _.byDefault |> stores.context.apply |> expectImportOk |> ignore
        let incoming = userImportMaterial (readImportRowEntries ()).silicon
        let folder = freshImportFolder ()
        let materialFile = writeDroppedEntry folder incoming
        let copyName = FileName "copy.material-json"
        let copyFile = copyName.combine folder
        let text = incoming.fileText |> expectImportDatabaseOk
        text.writeAtomically copyFile |> expectImportDatabaseOk
        let observed = observeStartupImport stores.context
        let context = observed.context
        let row = context.decideDroppedFiles [ materialFile; copyFile ] |> expectImportOk |> Assert.Single
        Assert.Equal(EntryIdNotHeld incoming, row.status)
        Assert.Equal(DroppedFileOrigin materialFile.leafName, row.origin)
        let reversed = context.decideDroppedFiles [ copyFile; materialFile ] |> expectImportOk |> Assert.Single
        Assert.Equal(DroppedFileOrigin copyFile.leafName, reversed.origin)

        let changedContent =
            {
                incoming.content with
                    name = EntryName "Changed lab material"
            }
        let changed = changedContent.``sealed`` |> expectImportDatabaseOk
        let changedFile = writeDroppedEntry folder changed
        match context.decideDroppedFiles [ materialFile; changedFile ] with
        | Error (DroppedFileRefusedErr reason) ->
            Assert.Contains(materialFile.value, reason)
            Assert.Contains(changedFile.value, reason)
        | _ -> Assert.Fail("Conflicting content under one id and version must be refused.")

        let next = nextImportEnvelope incoming
        let nextFile = writeDroppedEntry (freshImportFolder ()) next
        let versions = context.decideDroppedFiles [ materialFile; nextFile ] |> expectImportOk
        Assert.Equal<ImportStatus list>([ EntryIdNotHeld incoming; EntryIdNotHeld next ], versions |> List.map _.status)
        assertNoStartupWrites observed
        Assert.Equal<Result<HeldEntry option, CatalogueImportError>>(Ok None, context.proxy.tryHeldEntry incoming.content.id)

    [<Fact>]
    member this.``a dropped entry naming an entry neither held nor dropped is UnresolvedReferenceErr`` () : unit =
        let stores = this.freshImport ()
        stores.context.importFrom (CatalogueProxy.createBuiltIn ()) |> expectImportOk
        |> List.map _.byDefault |> stores.context.apply |> expectImportOk |> ignore
        let material = userImportMaterial (readImportRowEntries ()).silicon
        let materialEntry = material.content.toMaterialEntry |> decodeImportEntry
        let materialVersion : MaterialVersionId =
            {
                materialId = materialEntry.id
                version = material.content.version
            }
        let sample : Sample =
            {
                id = SampleId (Guid.Parse "31801b01-444f-43c8-8440-000000000104")
                name = EntryName "Dropped lab sample"
                structure =
                    {
                        films = []
                        substrate = None
                        lower = Some materialVersion
                    }
                substrate = ThinFilm
                description = EntryDescription "A sample referencing the dropped lab material"
                supportedEmission = EmitBoth
            }
        let sampleContent = LibraryEntryContent.ofSample VersionNumber.first UserProvenance sample
        let envelope = sampleContent.``sealed`` |> expectImportDatabaseOk
        let folder = freshImportFolder ()
        let sampleFile = writeDroppedEntry folder envelope
        let materialFile = writeDroppedEntry folder material
        let observed = observeStartupImport stores.context
        let context = observed.context
        let before = importRowSnapshot context [ material.content.id; envelope.content.id ]
        match context.decideDroppedFiles [ sampleFile ] with
        | Error (UnresolvedReferenceErr reason) ->
            Assert.Contains(material.content.id.value.ToString(), reason)
            Assert.Contains(envelope.content.id.value.ToString(), reason)
            Assert.Contains(envelope.content.name.value, reason)
            Assert.Contains(sampleFile.value, reason)
        | _ -> Assert.Fail("A missing material must be an unresolved dropped-entry reference.")
        let rows = context.decideDroppedFiles [ materialFile; sampleFile ] |> expectImportOk
        Assert.Equal<ImportStatus list>([ EntryIdNotHeld material; EntryIdNotHeld envelope ], rows |> List.map _.status)
        Assert.Equal<ImportOrigin list>([ DroppedFileOrigin materialFile.leafName; DroppedFileOrigin sampleFile.leafName ], rows |> List.map _.origin)
        let reversed = context.decideDroppedFiles [ sampleFile; materialFile ] |> expectImportOk
        Assert.Equal<ImportStatus list>([ EntryIdNotHeld envelope; EntryIdNotHeld material ], reversed |> List.map _.status)
        assertImportRowsPending context observed.puts observed.retirements before rows
        assertNoStartupWrites observed

        let failure = ImportStoreUnavailableErr "dropped reference lookup failed"
        let lookups = ResizeArray<LibraryEntryId>()
        let failingContext =
            {
                context with
                    proxy =
                        {
                            context.proxy with
                                tryHeldEntry = fun id ->
                                    lookups.Add id
                                    Error failure
                        }
            }
        Assert.Equal<Result<ImportDecision list, CatalogueImportError>>(Error failure, failingContext.decideDroppedFiles [ sampleFile ])
        Assert.Equal(material.content.id, Assert.Single lookups)
        assertNoStartupWrites observed

    [<Fact>]
    member this.``the start-up import logs the shipped catalogue and skips it, reading no entry file, while its manifest hash is unchanged`` () : unit =
        let catalogue = readImportCatalogue ()
        let folder = writtenImportFolder catalogue
        let stores = this.freshImport ()
        let observed = observeStartupImport stores.context
        let context = observed.context
        let report = context.importOnStart folder |> expectImportOk
        let folderReport = Assert.Single report.folders
        Assert.Equal(folder, folderReport.folder)
        let pending = pendingImport folderReport.outcome
        Assert.Equal<PendingCatalogueImport list>([ pending ], report.pending)
        Assert.Equal<LibraryEntryId list>(catalogue.manifest.entries |> List.map _.id, pending.decisions |> List.map _.entryId)
        Assert.All(pending.decisions, fun row -> Assert.Equal(AddIncomingEntry, row.defaultAction))
        Assert.Equal(catalogue.manifest.id, pending.entry.catalogueId)
        Assert.Equal(folder, pending.entry.folder)
        Assert.Equal(catalogue.manifest.manifestHash |> expectImportDatabaseOk, pending.entry.manifestHash)
        Assert.Equal(startupImportTime, pending.entry.importedAt)
        Assert.Equal(CatalogueEntryCount catalogue.envelopes.Length, pending.entry.entryCount)
        Assert.Equal<FolderName list>([ folder ], observed.manifestsRead |> Seq.toList)
        Assert.Equal<FolderName list>([ folder ], observed.cataloguesRead |> Seq.toList)
        assertNoStartupWrites observed
        assertEmptyStores stores
        Assert.Empty(context.proxy.loggedImports () |> expectImportOk)

        let logged = confirmImportDefaults context pending
        Assert.Equal(logged, Assert.Single observed.logs)
        Assert.Equal<CatalogueImportLogEntry list>([ logged ], context.proxy.loggedImports () |> expectImportOk)
        Assert.Equal(catalogue.envelopes.Length, observed.puts.Count)
        File.Delete((catalogue.envelopes.Head.content.fileName.combine folder).value)
        let next = observeStartupImport stores.context
        let unchanged = next.context.importOnStart folder |> expectImportOk
        Assert.Equal(Ok (CatalogueUnchanged logged), (Assert.Single unchanged.folders).outcome)
        Assert.Empty unchanged.pending
        Assert.Equal<FolderName list>([ folder ], next.manifestsRead |> Seq.toList)
        Assert.Empty next.cataloguesRead
        assertNoStartupWrites next
        Assert.Equal<CatalogueImportLogEntry list>([ logged ], context.proxy.loggedImports () |> expectImportOk)

    [<Fact>]
    member this.``a changed built-in catalogue mints the new version and moves the latest on the next start`` () : unit =
        let entries = readImportRowEntries ()
        let original = rowCatalogue CatalogueIds.builtInCatalogue entries.builtInName [ entries.category; entries.silicon ]
        let folder = writtenImportFolder original
        let stores = this.freshImport ()
        let first = stores.context.importOnStart folder |> expectImportOk
        let logged = confirmImportDefaults stores.context (Assert.Single first.pending)
        let newer = nextImportEnvelope entries.silicon
        let changed = rowCatalogue original.manifest.id original.manifest.name [ entries.category; newer ]
        writeImportFolder folder changed
        let observed = observeStartupImport stores.context
        let before = importRowSnapshot observed.context [ entries.category.content.id; entries.silicon.content.id ]
        let report = observed.context.importOnStart folder |> expectImportOk
        let pending = Assert.Single report.pending
        Assert.Equal(2, pending.decisions.Length)
        let materialRow = pending.decisions |> List.find (fun row -> row.entryId = entries.silicon.content.id)
        Assert.Equal(AddIncomingVersion MoveLatestVersion, materialRow.defaultAction)
        Assert.Equal(ReuseHeldEntry, (pending.decisions |> List.find (fun row -> row.entryId = entries.category.content.id)).defaultAction)
        assertImportRowsPending observed.context observed.puts observed.retirements before pending.decisions
        assertNoStartupWrites observed
        Assert.Equal<CatalogueImportLogEntry list>([ logged ], observed.context.proxy.loggedImports () |> expectImportOk)
        Assert.NotEqual(logged.manifestHash, pending.entry.manifestHash)

        let updatedLog = confirmImportDefaults observed.context pending
        let held = heldImportRow observed.context entries.silicon.content.id
        Assert.Equal<HeldEntryVersion list>(importVersions [ entries.silicon; newer ], held.versions)
        Assert.Equal(newer.content.version, held.latest)
        Assert.Equal(ActiveEntry, held.lifecycle)
        Assert.Equal(newer, (Assert.Single observed.puts).envelope)
        Assert.Equal(updatedLog, Assert.Single observed.logs)
        Assert.Equal<CatalogueImportLogEntry list>([ updatedLog ], observed.context.proxy.loggedImports () |> expectImportOk)

    [<Fact>]
    member this.``a changed catalogue holds every row without writes or logging until confirmation`` () : unit =
        let stores = this.freshImport ()
        let fixture = prepareLabImport stores.context
        let entries = fixture.entries
        let user = fixture.user
        let collision = fixture.collision
        let shipped = fixture.shipped
        let labFolder = fixture.labFolder
        stores.context.registerFolder labFolder |> pendingImport |> confirmImportDefaults stores.context |> ignore
        let priorLog = stores.context.proxy.loggedImports () |> expectImportOk
        let changed = changeLabImport fixture
        let observed = observeStartupImport stores.context
        let before = importRowSnapshot observed.context [ entries.category.content.id; user.content.id; collision.content.id; entries.source.content.id ]
        let startAndAssertPending () : PendingCatalogueImport =
            let report = observed.context.importOnStart shipped |> expectImportOk
            Assert.Equal<FolderName list>([ shipped; labFolder ], report.folders |> List.map _.folder)
            let pending = Assert.Single report.pending
            Assert.Equal(labFolder, pending.entry.folder)
            Assert.Equal<LibraryEntryId list>(changed.manifest.entries |> List.map _.id, pending.decisions |> List.map _.entryId)
            let rowFor (id : LibraryEntryId) : ImportDecision = pending.decisions |> List.find (fun row -> row.entryId = id)
            Assert.Equal(ReuseHeldEntry, (rowFor entries.category.content.id).defaultAction)
            Assert.Equal(NameCollision (collision, [ heldImportRow stores.context user.content.id ]), (rowFor collision.content.id).status)
            Assert.Equal(EntryIdNotHeld entries.source, (rowFor entries.source.content.id).status)
            assertImportRowsPending observed.context observed.puts observed.retirements before pending.decisions
            assertNoStartupWrites observed
            Assert.Equal<CatalogueImportLogEntry list>(priorLog, stores.context.proxy.loggedImports () |> expectImportOk)
            Assert.Equal<Result<HeldEntry option, CatalogueImportError>>(Ok None, stores.context.proxy.tryHeldEntry entries.source.content.id)
            pending
        let cancelled = startAndAssertPending ()
        let retried = startAndAssertPending ()
        Assert.Equal(cancelled, retried)
        Assert.Equal<FolderName list>([ labFolder; labFolder ], observed.cataloguesRead |> Seq.toList)

    [<Fact>]
    member this.``registering a catalogue folder imports and logs it, and lists it as the one registered folder`` () : unit =
        let stores = this.freshImport ()
        let fixture = prepareLabImport stores.context
        let observed = observeStartupImport stores.context
        let context = observed.context
        let before = importRowSnapshot context (fixture.lab.envelopes |> List.map _.content.id)
        let priorLog = context.proxy.loggedImports () |> expectImportOk
        Assert.Empty(context.proxy.registeredFolders () |> expectImportOk)

        let register : FolderName -> Result<FolderImportResult, CatalogueImportError> = context.registerFolder
        let pending = register fixture.labFolder |> pendingImport
        Assert.Equal<LibraryEntryId list>(fixture.lab.manifest.entries |> List.map _.id, pending.decisions |> List.map _.entryId)
        Assert.Contains(pending.decisions, fun row -> row.defaultAction = ReuseHeldEntry)
        Assert.Contains(pending.decisions, fun row -> row.defaultAction = AddIncomingEntry)
        assertImportRowsPending context observed.puts observed.retirements before pending.decisions
        assertNoStartupWrites observed
        Assert.Equal<CatalogueImportLogEntry list>(priorLog, context.proxy.loggedImports () |> expectImportOk)
        Assert.Empty(context.proxy.registeredFolders () |> expectImportOk)

        let logged = confirmImportDefaults context pending
        Assert.Equal(labCatalogueId, logged.catalogueId)
        Assert.Equal(fixture.labFolder, logged.folder)
        Assert.Equal(CatalogueEntryCount fixture.lab.envelopes.Length, logged.entryCount)
        Assert.Equal(fixture.lab.manifest.manifestHash |> expectImportDatabaseOk, logged.manifestHash)
        Assert.Equal(logged, Assert.Single observed.logs)
        let registered : unit -> Result<CatalogueImportLogEntry list, CatalogueImportError> = context.proxy.registeredFolders
        Assert.Equal<CatalogueImportLogEntry list>([ logged ], registered () |> expectImportOk)
        for envelope in fixture.lab.envelopes do
            Assert.Equal<HeldEntryVersion list>(importVersions [ envelope ], (heldImportRow context envelope.content.id).versions)
        Assert.Equal(fixture.user, (Assert.Single observed.puts).envelope)
        Assert.Empty observed.retirements
        Assert.Equal(Ok fixture.lab, (CatalogueProxy.create fixture.labFolder).readCatalogue ())

    [<Fact>]
    member this.``registering the Built-in catalogue or a registered catalogue again is refused`` () : unit =
        let stores = this.freshImport ()
        let fixture = prepareLabImport stores.context
        let labPending = stores.context.registerFolder fixture.labFolder |> pendingImport
        confirmImportDefaults stores.context labPending |> ignore
        let observed = observeStartupImport stores.context
        let context = observed.context
        let before = importRowSnapshot context (fixture.lab.envelopes |> List.map _.content.id)
        let priorLog = context.proxy.loggedImports () |> expectImportOk
        let builtIn = rowCatalogue CatalogueIds.builtInCatalogue fixture.entries.builtInName [ fixture.entries.category ]
        let copiedBuiltIn = writtenImportFolder builtIn
        let copiedLab = writtenImportFolder fixture.lab
        let assertRefused (folder : FolderName) (catalogue : CatalogueData) (registeredFolder : FolderName) : unit =
            match context.registerFolder folder with
            | Error (CatalogueAlreadyRegisteredErr reason) ->
                Assert.Contains(folder.value, reason)
                Assert.Contains(catalogue.manifest.name.value, reason)
                Assert.Contains(catalogue.manifest.id.value.ToString(), reason)
                Assert.Contains(registeredFolder.value, reason)
            | _ -> Assert.Fail("An already registered or Built-in catalogue must be refused by identity.")
            assertNoStartupWrites observed
            Assert.Equal<(LibraryEntryId * HeldEntry option) list>(before, importRowSnapshot context (before |> List.map fst))
            Assert.Equal<CatalogueImportLogEntry list>(priorLog, context.proxy.loggedImports () |> expectImportOk)
        assertRefused copiedBuiltIn builtIn fixture.shipped
        assertRefused fixture.labFolder fixture.lab fixture.labFolder
        assertRefused copiedLab fixture.lab fixture.labFolder
        Assert.Empty observed.cataloguesRead

        let empty = this.freshImport ()
        match empty.context.registerFolder copiedBuiltIn with
        | Error (CatalogueAlreadyRegisteredErr reason) ->
            Assert.Contains(copiedBuiltIn.value, reason)
            Assert.Contains(CatalogueIds.builtInCatalogue.value.ToString(), reason)
        | _ -> Assert.Fail("The Built-in identity is reserved even before the shipped import.")
        assertEmptyStores empty
        Assert.Empty(empty.context.proxy.loggedImports () |> expectImportOk)

        let missing = freshImportFolder ()
        match context.registerFolder missing with
        | Error (CatalogueUnreadableErr (CatalogueIoErr reason)) -> Assert.Contains(missing.value, reason)
        | _ -> Assert.Fail("Registration must retain a manifest read failure.")
        let unavailable = ImportStoreUnavailableErr "registration log unavailable"
        let failedProxy =
            {
                context.proxy with
                    loggedImports = fun () -> Error unavailable
            }
        let failed =
            {
                context with
                    proxy = failedProxy
            }
        Assert.Equal<Result<FolderImportResult, CatalogueImportError>>(Error unavailable, failed.registerFolder fixture.labFolder)
        Assert.Equal<Result<CatalogueImportLogEntry list, CatalogueImportError>>(Error unavailable, failedProxy.registeredFolders ())
        assertNoStartupWrites observed

    [<Fact>]
    member this.``unregistering a folder forgets it and keeps its entries, and the next start no longer reads it`` () : unit =
        let stores = this.freshImport ()
        let fixture = prepareLabImport stores.context
        let context = stores.context
        context.registerFolder fixture.labFolder |> pendingImport |> confirmImportDefaults context |> ignore
        let before = importRowSnapshot context (fixture.lab.envelopes |> List.map _.content.id)
        let material = fixture.user.content.toMaterialEntry |> decodeImportEntry
        let beforeLog = context.proxy.loggedImports () |> expectImportOk
        let shippedLog = beforeLog |> List.find (fun entry -> entry.catalogueId = CatalogueIds.builtInCatalogue)
        context.proxy.forgetImport labCatalogueId |> expectImportOk
        Assert.Empty(context.proxy.registeredFolders () |> expectImportOk)
        Assert.Equal<CatalogueImportLogEntry list>([ shippedLog ], context.proxy.loggedImports () |> expectImportOk)
        Assert.Equal<(LibraryEntryId * HeldEntry option) list>(before, importRowSnapshot context (before |> List.map fst))
        Assert.Equal<Result<MaterialEntry list, MaterialError>>(Ok [ material ], stores.materials.listMaterials IncludeInactive)

        let observed = observeStartupImport context
        let report = observed.context.importOnStart fixture.shipped |> expectImportOk
        let folderReport = Assert.Single report.folders
        Assert.Equal(fixture.shipped, folderReport.folder)
        Assert.Equal(Ok (CatalogueUnchanged shippedLog), folderReport.outcome)
        Assert.Equal<FolderName list>([ fixture.shipped ], observed.opened |> Seq.toList)
        Assert.Equal<FolderName list>([ fixture.shipped ], observed.manifestsRead |> Seq.toList)
        Assert.Empty observed.cataloguesRead
        Assert.Empty report.pending
        assertNoStartupWrites observed
        Assert.Equal<(LibraryEntryId * HeldEntry option) list>(before, importRowSnapshot context (before |> List.map fst))

    [<Fact>]
    member this.``settling a pending import applies the chosen rows and logs the catalogue, and an incomplete choice is refused`` () : unit =
        let stores = this.freshImport ()
        let fixture = prepareLabImport stores.context
        stores.context.registerFolder fixture.labFolder |> pendingImport |> confirmImportDefaults stores.context |> ignore
        let changed = changeLabImport fixture
        let observed = observeStartupImport stores.context
        let context = observed.context
        let report = context.importOnStart fixture.shipped |> expectImportOk
        let pending = Assert.Single report.pending
        Assert.Equal<LibraryEntryId list>(changed.manifest.entries |> List.map _.id, pending.decisions |> List.map _.entryId)
        let before = importRowSnapshot context (fixture.user.content.id :: (pending.decisions |> List.map _.entryId))
        let priorLog = context.proxy.loggedImports () |> expectImportOk
        let defaults = pending.decisions |> List.map _.byDefault
        let collision = pending.decisions |> List.find (fun row -> row.entryId = fixture.collision.content.id)
        let reused = pending.decisions |> List.find (fun row -> row.defaultAction = ReuseHeldEntry)
        let origin = CatalogueImportOrigin (labCatalogueId, labCatalogueName)
        let foreign = ImportDecision.decide origin fixture.entries.silicon None []
        let invalidSelections =
            [
                []
                [ collision.byDefault ]
                defaults |> List.filter (fun choice -> choice.decision.entryId <> reused.entryId)
                collision.byDefault :: defaults
                defaults |> List.map (fun choice -> if choice.decision.entryId = reused.entryId then collision.byDefault else choice)
                foreign.byDefault :: defaults
                defaults |> List.map (fun choice -> if choice.decision.entryId = reused.entryId then foreign.byDefault else choice)
            ]
        let settle : PendingCatalogueImport -> ImportChoice list -> Result<ImportOutcome, CatalogueImportError> = context.settle
        for choices in invalidSelections do
            match settle pending choices with
            | Error (InvalidImportChoiceErr reason) -> Assert.False(String.IsNullOrWhiteSpace reason)
            | _ -> Assert.Fail("Settlement must require exactly one choice for every pending row, including reuse.")
            assertNoStartupWrites observed
            Assert.Equal<(LibraryEntryId * HeldEntry option) list>(before, importRowSnapshot context (before |> List.map fst))
            Assert.Equal<CatalogueImportLogEntry list>(priorLog, context.proxy.loggedImports () |> expectImportOk)

        let forged =
            {
                collision with
                    defaultAction = ReuseHeldEntry
            }
        let invalidAction = defaults |> List.map (fun choice -> if choice.decision.entryId = collision.entryId then forged.byDefault else choice)
        match settle pending invalidAction with
        | Error (InvalidImportChoiceErr reason) -> Assert.Contains(collision.entryId.value.ToString(), reason)
        | _ -> Assert.Fail("A supplied decision must not change the pending row's offered actions.")
        assertNoStartupWrites observed

        let rejected = ImportRejectedErr "settlement write refused"
        let failing =
            {
                context with
                    proxy =
                        {
                            context.proxy with
                                putEntryVersion = fun _ -> Error rejected
                        }
            }
        Assert.Equal<Result<ImportOutcome, CatalogueImportError>>(Error rejected, failing.settle pending defaults)
        assertNoStartupWrites observed
        Assert.Equal<CatalogueImportLogEntry list>(priorLog, context.proxy.loggedImports () |> expectImportOk)

        let settledAt = CatalogueImportTime (DateTime(2026, 11, 6, 16, 45, 0, DateTimeKind.Utc))
        let later =
            {
                context with
                    now = fun () -> settledAt
            }
        let outcome = later.settle pending (List.rev defaults) |> expectImportOk
        Assert.Empty outcome.forks
        Assert.Empty outcome.rebindings
        let expectedLog =
            {
                pending.entry with
                    importedAt = settledAt
            }
        Assert.Equal(expectedLog, Assert.Single observed.logs)
        Assert.Equal<CatalogueImportLogEntry list>([ expectedLog ], context.proxy.registeredFolders () |> expectImportOk)
        Assert.Equal<Set<LibraryEntryId>>(
            Set.ofList [ fixture.user.content.id; fixture.collision.content.id ],
            context.proxy.heldEntriesNamed MaterialEntryKind fixture.user.content.name |> expectImportOk |> List.map _.id |> Set.ofList)
        for envelope in [ fixture.user; fixture.collision; fixture.entries.source ] do
            Assert.Equal<HeldEntryVersion list>(importVersions [ envelope ], (heldImportRow context envelope.content.id).versions)
        Assert.Equal<LibraryEntryEnvelope list>([ fixture.collision; fixture.entries.source ], observed.puts |> Seq.map _.envelope |> Seq.toList)
        Assert.Empty observed.retirements

        let next = observeStartupImport stores.context
        let unchanged = next.context.importOnStart fixture.shipped |> expectImportOk
        let labReport = unchanged.folders |> List.find (fun folder -> folder.folder = fixture.labFolder)
        Assert.Equal(Ok (CatalogueUnchanged expectedLog), labReport.outcome)
        Assert.Empty unchanged.pending
        Assert.Empty next.cataloguesRead
        assertNoStartupWrites next

    [<Fact>]
    member this.``every logged folder is re-checked on start, and a failing one is reported without stopping the others`` () : unit =
        let entries = readImportRowEntries ()
        let builtIn = rowCatalogue CatalogueIds.builtInCatalogue entries.builtInName [ entries.category ]
        let shipped = writtenImportFolder builtIn
        let stores = this.freshImport ()
        stores.context.importFolder shipped |> pendingImport |> confirmImportDefaults stores.context |> ignore
        let folders = [ freshImportFolder (); freshImportFolder (); freshImportFolder () ] |> List.sortBy _.value
        let catalogues =
            folders |> List.map (fun folder ->
                let catalogueId = CatalogueId (Guid.NewGuid())
                let catalogue = rowCatalogue catalogueId labCatalogueName [ entries.category ]
                writeImportFolder folder catalogue
                stores.context.importFolder folder |> pendingImport |> confirmImportDefaults stores.context |> ignore
                folder, catalogue)
        let firstFolder, first = catalogues[0]
        let brokenFolder, _ = catalogues[1]
        let lastFolder, last = catalogues[2]
        writeImportFolder firstFolder (rowCatalogue first.manifest.id first.manifest.name [ entries.category; entries.source ])
        File.Delete((CatalogueManifest.fileName.combine brokenFolder).value)
        writeImportFolder lastFolder (rowCatalogue last.manifest.id last.manifest.name [ entries.category; entries.source ])
        let priorLog = stores.context.proxy.loggedImports () |> expectImportOk
        let observed = observeStartupImport stores.context
        let report = observed.context.importOnStart shipped |> expectImportOk
        let expectedOrder = shipped :: (priorLog |> List.filter (fun entry -> entry.catalogueId <> CatalogueIds.builtInCatalogue) |> List.map _.folder)
        Assert.Equal<FolderName list>(expectedOrder, report.folders |> List.map _.folder)
        Assert.Equal<FolderName list>(expectedOrder, observed.opened |> Seq.toList)
        Assert.Equal<FolderName list>(expectedOrder, observed.manifestsRead |> Seq.toList)
        Assert.Equal<FolderName list>([ firstFolder; lastFolder ], observed.cataloguesRead |> Seq.toList)
        Assert.Equal<FolderName list>([ firstFolder; lastFolder ], report.pending |> List.map _.entry.folder)
        for pending in report.pending do
            Assert.Equal(2, pending.decisions.Length)
        match report.folders[2].outcome with
        | Error (CatalogueUnreadableErr (CatalogueIoErr reason)) -> Assert.Contains(brokenFolder.value, reason)
        | _ -> Assert.Fail("The unreadable folder must retain its typed IO failure in the report.")
        assertNoStartupWrites observed
        Assert.Equal<CatalogueImportLogEntry list>(priorLog, stores.context.proxy.loggedImports () |> expectImportOk)
        Assert.Equal<Result<HeldEntry option, CatalogueImportError>>(Ok None, stores.context.proxy.tryHeldEntry entries.source.content.id)

        let unavailable = ImportStoreUnavailableErr "startup log unavailable"
        let failed =
            {
                observed.context with
                    proxy =
                        {
                            observed.context.proxy with
                                loggedImports = fun () -> Error unavailable
                        }
            }
        observed.opened.Clear()
        Assert.Equal<Result<StartupImportReport, CatalogueImportError>>(Error unavailable, failed.importOnStart shipped)
        Assert.Empty observed.opened
        assertNoStartupWrites observed

    [<Fact>]
    member this.``a user entry is never touched by the start-up import`` () : unit =
        let entries = readImportRowEntries ()
        let user = userImportMaterial entries.silicon
        let builtIn = rowCatalogue CatalogueIds.builtInCatalogue entries.builtInName [ entries.category; entries.silicon ]
        let shipped = writtenImportFolder builtIn
        let stores = this.freshImport ()
        stores.context.importFolder shipped |> pendingImport |> confirmImportDefaults stores.context |> ignore
        seedImportRows stores.context [ user ]
        let originalUser = heldImportRow stores.context user.content.id
        let originalBuiltIn = heldImportRow stores.context entries.silicon.content.id
        File.Delete((entries.silicon.content.fileName.combine shipped).value)
        writeImportFolder shipped (rowCatalogue builtIn.manifest.id builtIn.manifest.name [ entries.category ])
        let observed = observeStartupImport stores.context
        let pending = observed.context.importOnStart shipped |> expectImportOk |> _.pending |> Assert.Single
        Assert.Equal(2, pending.decisions.Length)
        Assert.DoesNotContain(pending.decisions, fun row -> row.entryId = user.content.id)
        Assert.Equal(RetireHeldEntry, (pending.decisions |> List.find (fun row -> row.entryId = originalBuiltIn.id)).defaultAction)
        assertNoStartupWrites observed
        Assert.Equal(originalUser, heldImportRow stores.context user.content.id)
        Assert.Equal(originalBuiltIn, heldImportRow stores.context originalBuiltIn.id)
        confirmImportDefaults observed.context pending |> ignore
        Assert.Equal(originalUser, heldImportRow stores.context user.content.id)
        let retiredBuiltIn =
            {
                originalBuiltIn with
                    lifecycle = InactiveEntry
            }
        Assert.Equal(retiredBuiltIn, heldImportRow stores.context originalBuiltIn.id)
        Assert.Empty observed.puts
        Assert.Equal(originalBuiltIn.id, Assert.Single observed.retirements)

        let newerUser = nextImportEnvelope user
        let lab = rowCatalogue labCatalogueId labCatalogueName [ user ]
        let labFolder = writtenImportFolder lab
        stores.context.importFolder labFolder |> pendingImport |> confirmImportDefaults stores.context |> ignore
        writeImportFolder labFolder (rowCatalogue labCatalogueId labCatalogueName [ newerUser ])
        let next = observeStartupImport stores.context
        let userPending = next.context.importOnStart shipped |> expectImportOk |> _.pending |> Assert.Single
        Assert.Equal(AddIncomingVersion KeepLatestVersion, (Assert.Single userPending.decisions).defaultAction)
        assertNoStartupWrites next
        Assert.Equal(originalUser, heldImportRow stores.context user.content.id)
        confirmImportDefaults next.context userPending |> ignore
        let expectedUser =
            {
                originalUser with
                    versions = importVersions [ user; newerUser ]
            }
        Assert.Equal(expectedUser, heldImportRow stores.context user.content.id)
        Assert.Equal(KeepLatestVersion, (Assert.Single next.puts).latest)
        Assert.Empty next.retirements

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

        let itemModelRows : ItemModelVersionId list =
            catalogue.envelopes
            |> List.filter (fun envelope -> envelope.content.schema.kind = ItemModelEntryKind)
            |> List.map (fun envelope ->
                let row : ItemModelVersion = { version = envelope.content.version; entry = envelope.content.toItemModel |> decodeImportEntry }
                row.versionId)
        let listed = stores.itemModels.listItemModels () |> Result.map (List.map _.versionId)
        Assert.NotEmpty itemModelRows
        Assert.Equal<Result<int, ItemModelError>>(Ok (countOf ItemModelEntryKind), listed |> Result.map List.length)
        Assert.Equal<Result<Set<ItemModelVersionId>, ItemModelError>>(Ok (Set.ofList itemModelRows), listed |> Result.map Set.ofList)

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
            | ItemModelEntryKind ->
                let expected = content.toItemModel |> decodeImportEntry
                let actual =
                    context.proxy.tryHeldEnvelope content.id content.version
                    |> expectImportOk
                    |> Option.map (fun held -> held.content.toItemModel |> decodeImportEntry)
                Assert.True((Some expected = actual), "An item-model id must resolve to its decoded value.")
                let expectedVersion : ItemModelVersion = { version = content.version; entry = expected }
                Assert.Equal<Result<ItemModelVersion option, ItemModelError>>(
                    Ok (Some expectedVersion), stores.itemModels.resolveItemModelVersion expectedVersion.versionId)

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
            context = deterministicImportContext stores.proxy
            categories = stores.categories
            materials = stores.materials
            samples = stores.samples
            presets = stores.libraryEntries
            itemModels = stores.itemModels
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
            context = deterministicImportContext (CatalogueImportProxy.createInDatabase database)
            categories = CategoryProxy.createInDatabase database
            materials = MaterialProxy.createInDatabase database
            samples = SampleProxy.createInDatabase database
            presets = LibraryProxy.createInDatabase database
            itemModels = ItemModelProxy.createInDatabase database
        }

    [<Fact>]
    member this.``a catalogue export pins the version row of every exported kind`` () : unit =
        let database = this.openDatabase ()
        let context = deterministicImportContext (CatalogueImportProxy.createInDatabase database)
        let fixture = prepareCatalogueExport context
        let catalogue = readImportCatalogue ()
        let sourceEnvelope = catalogue.envelopes |> List.find (fun envelope -> envelope.content.schema.kind = SourceEntryKind)
        let source = sourceEnvelope.content.toSourcePreset |> decodeImportEntry
        let sourceId = newSourceId ()
        let userSource =
            {
                source with
                    id = sourceId
                    protection = UserManaged
            }
        let sourceContent = LibraryEntryContent.ofSourcePreset VersionNumber.first UserProvenance userSource |> decodeImportEntry
        BuiltinCatalogue.putSealedEntries context [ sourceContent ]
        let polarizer = catalogue.envelopes |> List.find (fun envelope -> envelope.content.schema.kind = PolarizerEntryKind)
        let detector = catalogue.envelopes |> List.find (fun envelope -> envelope.content.schema.kind = DetectorEntryKind)
        let ids = [ fixture.category.id; fixture.material.id; fixture.sample.id; sourceContent.id; polarizer.content.id; detector.content.id ]
        let exported = context.exportEntries ids (freshCatalogueExportTarget ()) |> expectCatalogueExportOk
        Assert.Equal<Set<LibraryEntryId>>(Set.ofList ids, exported.envelopes |> List.map _.content.id |> Set.ofList)
        database.run (fun stored ->
            for envelope in exported.envelopes do
                match stored.tryStoredVersion envelope.content.id envelope.content.version |> expectImportDatabaseOk with
                | Some version ->
                    Assert.Equal(VersionPinned, version.state.pin)
                    Assert.Equal(envelope, version.envelope)
                | None -> Assert.Fail($"Exported entry {envelope.content.describe} must retain its pinned version.")
            match stored.tryStoredVersion sourceEnvelope.content.id sourceEnvelope.content.version |> expectImportDatabaseOk with
            | Some version -> Assert.Equal(VersionUnpinned, version.state.pin)
            | None -> Assert.Fail("The unexported built-in source must remain held and unpinned."))
        |> expectImportDatabaseOk

    [<Fact>]
    member this.``pinEntryVersion pins the version row of every kind, keeping its lifecycle`` () : unit =
        let database = this.openDatabase ()
        let context = deterministicImportContext (CatalogueImportProxy.createInDatabase database)
        let catalogue = readImportCatalogue ()
        context.importFrom (CatalogueProxy.createBuiltIn ()) |> expectImportOk
        |> List.map _.byDefault |> context.apply |> expectImportOk |> ignore
        let selected =
            LibraryEntryKind.all |> List.map (fun kind ->
                catalogue.envelopes |> List.find (fun envelope -> envelope.content.schema.kind = kind))
        let source = selected |> List.find (fun envelope -> envelope.content.schema.kind = SourceEntryKind)
        context.proxy.retireEntry source.content.id |> expectImportOk
        for envelope in selected do
            context.proxy.pinEntryVersion envelope.content.id VersionNumber.first |> expectImportOk
        let selectedIds = selected |> List.map _.content.id |> Set.ofList
        database.run (fun stored ->
            for envelope in catalogue.envelopes do
                match stored.tryStoredVersion envelope.content.id VersionNumber.first |> expectImportDatabaseOk with
                | Some version ->
                    let expectedPin = if selectedIds |> Set.contains envelope.content.id then VersionPinned else VersionUnpinned
                    let expectedLifecycle = if envelope.content.id = source.content.id then InactiveEntry else ActiveEntry
                    Assert.Equal(expectedPin, version.state.pin)
                    Assert.Equal(expectedLifecycle, version.state.lifecycle)
                    Assert.Equal(envelope, version.envelope)
                | None -> Assert.Fail($"Imported entry {envelope.content.id.value} must retain version 1."))
        |> expectImportDatabaseOk

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
        proxy.tryHeldEnvelope envelope.content.id envelope.content.version |> assertUnavailable
        proxy.heldEntriesNamed envelope.content.schema.kind envelope.content.name |> assertUnavailable
        proxy.heldEntriesOf CatalogueIds.builtInCatalogue |> assertUnavailable
        proxy.heldUserEntries () |> assertUnavailable
        proxy.pinEntryVersion envelope.content.id envelope.content.version |> assertUnavailable
        proxy.putEntryVersion put |> assertUnavailable
        proxy.retireEntry envelope.content.id |> assertUnavailable
        proxy.loggedImports () |> assertUnavailable
        proxy.logImport logEntry |> assertUnavailable
        proxy.forgetImport logEntry.catalogueId |> assertUnavailable

    interface IDisposable with
        member _.Dispose () : unit =
            for database in databases do
                (database :> IDisposable).Dispose()
