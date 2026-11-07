namespace OpticalConstructor.Tests

open Xunit
open OpticalConstructor.Domain.Placement
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.CatalogueFiles
open OpticalConstructor.Domain.CatalogueImport
open OpticalConstructor.Storage.CatalogueFolderStore
open OpticalConstructor.Storage.CatalogueImportStore

/// Exercises catalogue imports through the real empty stores (spec 0038 step 042; spec 0050 §3.4, §4).
module CatalogueImportInMemoryTests =

    open CatalogueImportTestData

    let private presetEntries (stores : InMemoryCatalogueStores) : LibraryEntry list =
        [ LightSource; Detector; LinearPolarizer; CircularPolarizer ]
        |> List.collect (fun kind ->
            match stores.libraryEntries.entriesForKind kind with
            | Ok entries -> entries
            | Error _ -> failwith "The in-memory preset store must support every preset kind.")
        |> List.distinctBy (fun entry -> entry.entryId)

    let private assertEmptyStores (stores : InMemoryCatalogueStores) : unit =
        Assert.Equal<Result<MaterialCategory list, CategoryError>>(Ok [], stores.categories.listCategories ())
        Assert.Equal<Result<MaterialEntry list, MaterialError>>(Ok [], stores.materials.listMaterials IncludeInactive)
        Assert.Equal<Result<Sample list, SampleError>>(Ok [], stores.samples.listSamples IncludeInactive)
        Assert.Empty(presetEntries stores)

    [<Fact>]
    let ``importFrom fills empty in-memory stores with the whole Built-in catalogue`` () : unit =
        let catalogue = readImportCatalogue ()
        let stores = CatalogueImportProxy.createInMemory ()
        let context = CatalogueImportContext.create stores.proxy
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
    let ``every imported entry resolves by its id to its decoded value`` () : unit =
        let catalogue = readImportCatalogue ()
        let stores = CatalogueImportProxy.createInMemory ()
        let context = CatalogueImportContext.create stores.proxy
        let rows = context.importCatalogue catalogue |> expectImportOk
        Assert.Equal(catalogue.envelopes.Length, rows.Length)
        assertEmptyStores stores
        rows |> List.map _.byDefault |> context.apply |> expectImportOk |> ignore

        let assertPreset (entry : LibraryEntry) : unit =
            Assert.True((Ok (Some entry) = stores.libraryEntries.tryGetEntry entry.entryId), "A preset id must resolve to its decoded value.")

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
    let ``importing a catalogue the stores already hold reuses every entry and writes nothing`` () : unit =
        let catalogue = readImportCatalogue ()
        let stores = CatalogueImportProxy.createInMemory ()
        let writes = ResizeArray<EntryVersionPut>()
        let proxy =
            {
                stores.proxy with
                    putEntryVersion = fun put ->
                        writes.Add put
                        stores.proxy.putEntryVersion put
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
    let ``an entry that does not decode stops the import with UndecodableEntryErr`` () : unit =
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
        let stores = CatalogueImportProxy.createInMemory ()
        let context = CatalogueImportContext.create stores.proxy
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
        Assert.Equal<Result<HeldEntry option, CatalogueImportError>>(Ok None, stores.proxy.tryHeldEntry material.content.id)

    [<Fact>]
    let ``importFrom an unreadable catalogue imports nothing and answers CatalogueUnreadableErr`` () : unit =
        let stores = CatalogueImportProxy.createInMemory ()
        let unreadable = CatalogueProxy.createInMemory ()

        let context = CatalogueImportContext.create stores.proxy
        match context.importFrom unreadable with
        | Error (CatalogueUnreadableErr (CatalogueIoErr reason)) -> Assert.False(System.String.IsNullOrWhiteSpace reason)
        | Error error -> Assert.Fail($"Expected a catalogue IO error, got {error.describe}")
        | Ok _ -> Assert.Fail("An unwritten catalogue must not import successfully.")

        Assert.Equal<Result<MaterialCategory list, CategoryError>>(Ok [], stores.categories.listCategories ())
        Assert.Equal<Result<MaterialEntry list, MaterialError>>(Ok [], stores.materials.listMaterials IncludeInactive)
        Assert.Equal<Result<Sample list, SampleError>>(Ok [], stores.samples.listSamples IncludeInactive)
        Assert.Empty(presetEntries stores)
