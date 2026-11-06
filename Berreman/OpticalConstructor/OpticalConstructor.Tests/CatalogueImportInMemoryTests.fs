namespace OpticalConstructor.Tests

open Xunit
open OpticalConstructor.Domain.Placement
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.CatalogueFiles
open OpticalConstructor.Domain.CatalogueImport
open OpticalConstructor.Storage.CatalogueFolderStore

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

    [<Fact>]
    let ``importFrom fills empty in-memory stores with the whole Built-in catalogue`` () : unit =
        let catalogue = readImportCatalogue ()
        let stores = CatalogueImportProxy.createInMemory ()
        let import : CatalogueProxy -> Result<unit, CatalogueImportError> = stores.proxy.importFrom

        Assert.Equal<Result<unit, CatalogueImportError>>(Ok (), import (CatalogueProxy.createBuiltIn ()))

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
        Assert.Equal<Result<unit, CatalogueImportError>>(Ok (), stores.proxy.importCatalogue catalogue)

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
    let ``importing a catalogue the stores already hold is refused`` () : unit =
        let catalogue = readImportCatalogue ()
        let stores = CatalogueImportProxy.createInMemory ()
        Assert.Equal<Result<unit, CatalogueImportError>>(Ok (), stores.proxy.importCatalogue catalogue)

        match stores.proxy.importCatalogue catalogue with
        | Error (ImportRejectedErr reason) -> Assert.False(System.String.IsNullOrWhiteSpace reason)
        | Error error -> Assert.Fail($"Expected a store refusal, got {error.describe}")
        | Ok () -> Assert.Fail("A second import of the same catalogue must be refused.")

    [<Fact>]
    let ``importFrom an unreadable catalogue imports nothing and answers CatalogueUnreadableErr`` () : unit =
        let stores = CatalogueImportProxy.createInMemory ()
        let unreadable = CatalogueProxy.createInMemory ()

        match stores.proxy.importFrom unreadable with
        | Error (CatalogueUnreadableErr (CatalogueIoErr reason)) -> Assert.False(System.String.IsNullOrWhiteSpace reason)
        | Error error -> Assert.Fail($"Expected a catalogue IO error, got {error.describe}")
        | Ok () -> Assert.Fail("An unwritten catalogue must not import successfully.")

        Assert.Equal<Result<MaterialCategory list, CategoryError>>(Ok [], stores.categories.listCategories ())
        Assert.Equal<Result<MaterialEntry list, MaterialError>>(Ok [], stores.materials.listMaterials IncludeInactive)
        Assert.Equal<Result<Sample list, SampleError>>(Ok [], stores.samples.listSamples IncludeInactive)
        Assert.Empty(presetEntries stores)
