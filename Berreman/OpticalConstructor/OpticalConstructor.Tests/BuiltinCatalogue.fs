namespace OpticalConstructor.Tests

open System
open OpticalConstructor.Domain.Placement
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.CatalogueFiles
open OpticalConstructor.Domain.CatalogueImport
open OpticalConstructor.Storage.CatalogueFolderStore

module BuiltinCatalogue =

    let private loadedCatalogue : Lazy<CatalogueData> =
        lazy
            ((CatalogueProxy.createBuiltIn ()).readCatalogue ()
             |> Result.defaultWith (fun error -> failwith error.describe))

    /// Returns the shipped catalogue, loading once on first use and failing with the read error's description.
    let catalogueData () : CatalogueData = loadedCatalogue.Force()

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
