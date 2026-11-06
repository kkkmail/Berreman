namespace OpticalConstructor.Tests

open System
open Softellect.Sys.Primitives
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Placement
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.MaterialStore
open OpticalConstructor.Domain.SampleStore
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.CatalogueFiles
open OpticalConstructor.Domain.CatalogueImport
open OpticalConstructor.Domain.Lifecycle
open OpticalConstructor.Storage.CatalogueFolderStore
open OpticalConstructor.Storage.LibraryDatabaseStore

module BuiltinCatalogue =

    let private loadedCatalogue : Lazy<CatalogueData> =
        lazy
            ((CatalogueProxy.createBuiltIn ()).readCatalogue ()
             |> Result.defaultWith (fun error -> failwith error.describe))

    /// Returns the shipped catalogue, loading once on first use and failing with the read error's description.
    let catalogueData () : CatalogueData = loadedCatalogue.Force()

    /// Returns a disposable in-memory database holding every Built-in version and latest (spec 0050 S2.2).
    let freshLibraryDatabase () : LibraryDatabase =
        let catalogue = catalogueData ()
        let connectionString = ConnectionString "Data Source=:memory:"
        let database =
            LibraryDatabase.openOrCreate connectionString
            |> Result.defaultWith (fun error -> failwith error.describe)
        let staged =
            database.run (fun context ->
                catalogue.envelopes
                |> List.fold (fun result envelope ->
                    result |> Result.bind (fun () ->
                        let stored : StoredEntryVersion =
                            {
                                envelope = envelope
                                state =
                                    {
                                        lifecycle = ActiveEntry
                                        pin = VersionUnpinned
                                    }
                            }
                        context.stageVersion stored
                        |> Result.map (fun () -> context.stageLatest envelope.content.id envelope.content.version))) (Ok ())
                |> Result.map (fun () -> context.SaveChanges() |> ignore))
            |> Result.bind id
        match staged with
        | Ok () -> database
        | Error error ->
            (database :> IDisposable).Dispose()
            failwith error.describe

    /// Returns independent stores filled with the shipped catalogue, failing with the import error's description.
    let freshStores () : InMemoryCatalogueStores =
        let stores = CatalogueImportProxy.createInMemory ()
        match stores.proxy.importCatalogue (catalogueData ()) with
        | Ok () -> stores
        | Error error -> failwith error.describe

    let private contentsOfKind (kind : LibraryEntryKind) : LibraryEntryContent list =
        (catalogueData ()).envelopes
        |> List.map (fun envelope -> envelope.content)
        |> List.filter (fun content -> content.schema.kind = kind)

    let private decode (result : Result<'Entry, LibraryEntryError>) : 'Entry =
        result |> Result.defaultWith (fun error -> failwith error.describe)

    /// Returns decoded materials in catalogue row order, failing with any decode error's description.
    let materials () : MaterialEntry list =
        contentsOfKind MaterialEntryKind
        |> List.map (fun content -> content.toMaterialEntry |> decode)

    /// Returns decoded categories in catalogue row order, failing with any decode error's description.
    let categories () : MaterialCategory list =
        contentsOfKind CategoryEntryKind
        |> List.map (fun content -> content.toMaterialCategory |> decode)

    /// Returns decoded samples in catalogue row order, failing with any decode error's description.
    let samples () : Sample list =
        contentsOfKind SampleEntryKind
        |> List.map (fun content -> content.toSample |> decode)

    /// Returns decoded source, polarizer and detector presets in row order, failing with any decode error's description.
    let presets () : LibraryEntry list =
        (catalogueData ()).envelopes
        |> List.choose (fun envelope ->
            let content = envelope.content
            match content.schema.kind with
            | SourceEntryKind -> content.toSourcePreset |> decode |> SourceItem |> Some
            | PolarizerEntryKind -> content.toPolarizerPreset |> decode |> PolarizerItem |> Some
            | DetectorEntryKind -> content.toDetectorPreset |> decode |> DetectorItem |> Some
            | MaterialEntryKind | CategoryEntryKind | SampleEntryKind -> None)

    /// Returns a fresh material store filled in catalogue order, failing with the refused entry and reason (spec 0050 S1.7).
    let materialStore (samplesReferencing : MaterialId -> Sample list) : MaterialProxy =
        let store = MaterialProxy.createInMemory samplesReferencing
        for entry in materials () do
            match store.saveMaterial entry with
            | Ok () -> ()
            | Error (UnknownMaterialIdErr reason)
            | Error (DuplicateMaterialIdErr reason)
            | Error (MaterialStillReferencedErr reason)
            | Error (InvalidMaterialErr reason)
            | Error (MaterialStoreUnavailableErr reason)
            | Error (MaterialVersionPinnedErr reason) ->
                failwith $"The Built-in material '{entry.name.value}' ('{entry.id.value}') was refused: {reason}"
        store

    /// Returns a fresh sample store filled in catalogue order, failing with the refused entry and reason (spec 0050 S1.7).
    let sampleStore () : SampleProxy =
        let store = SampleProxy.createInMemory ()
        for entry in samples () do
            match store.saveSample entry with
            | Ok () -> ()
            | Error (UnknownSampleIdErr reason)
            | Error (DuplicateSampleIdErr reason)
            | Error (InvalidSampleErr reason)
            | Error (SampleVersionPinnedErr reason) ->
                failwith $"The Built-in sample '{entry.name.value}' ('{entry.id.value}') was refused: {reason}"
        store

    /// Returns a fresh category store filled in catalogue order, failing with the refused entry and reason (spec 0050 S1.7).
    let categoryStore (materialsReferencingCategory : CategoryId -> MaterialEntry list) : CategoryProxy =
        let store = CategoryProxy.createInMemory materialsReferencingCategory
        for entry in categories () do
            match store.addCategory entry with
            | Ok () -> ()
            | Error (UnknownCategoryIdErr reason)
            | Error (DuplicateCategoryIdErr reason)
            | Error (CategoryStillReferencedErr reason)
            | Error (BuiltInNotRemovableErr reason)
            | Error (CategoryStoreUnavailableErr reason)
            | Error (InvalidCategoryErr reason) ->
                failwith $"The Built-in category '{entry.name.value}' ('{entry.id.value}') was refused: {reason}"
        store

    /// Returns a fresh library holding samples then presets in row order, failing with any refused entry and reason (spec 0050 S1.7).
    let library () : LibraryProxy =
        let store = Library.createInMemory ()
        let entries = (samples () |> List.map SampleItem) @ presets ()
        for entry in entries do
            match store.addEntry entry with
            | Ok () -> ()
            | Error (UnknownEntryIdErr reason | NoEntriesForKindErr reason | LibraryUnavailableErr reason) ->
                failwith $"The Built-in library entry '{entry.entryName.value}' ('{entry.entryId.value}') was refused: {reason}"
        store.proxy

    /// Returns only source presets in catalogue row order (spec 0050 S1.7).
    let sources () : SourcePreset list =
        presets ()
        |> List.choose (fun entry ->
            match entry with
            | SourceItem source -> Some source
            | SampleItem _ | DetectorItem _ | PolarizerItem _ -> None)

    /// Returns the source with the given id, failing with that id when it is absent (spec 0050 S1.7).
    let source (id : ElementValueId) : SourcePreset =
        match sources () |> List.tryFind (fun entry -> entry.id = id) with
        | Some entry -> entry
        | None -> failwith $"The Built-in catalogue has no source with id '{id.value}'."

    /// Returns the material with the given id, failing with the id when it is absent.
    let material (id : MaterialId) : MaterialEntry =
        match materials () |> List.tryFind (fun entry -> entry.id = id) with
        | Some entry -> entry
        | None -> failwith $"The Built-in catalogue has no material with id '{id.value}'."

    /// Returns the category with the given id, failing with the id when it is absent.
    let category (id : CategoryId) : MaterialCategory =
        match categories () |> List.tryFind (fun entry -> entry.id = id) with
        | Some entry -> entry
        | None -> failwith $"The Built-in catalogue has no category with id '{id.value}'."

    /// Returns the sample with the given id, failing with the id when it is absent.
    let sample (id : SampleId) : Sample =
        match samples () |> List.tryFind (fun entry -> entry.id = id) with
        | Some entry -> entry
        | None -> failwith $"The Built-in catalogue has no sample with id '{id.value}'."

    /// Returns the preset with the given id, failing with the id when it is absent.
    let preset (id : ElementValueId) : LibraryEntry =
        match presets () |> List.tryFind (fun entry -> entry.entryId = id) with
        | Some entry -> entry
        | None -> failwith $"The Built-in catalogue has no preset with id '{id.value}'."
