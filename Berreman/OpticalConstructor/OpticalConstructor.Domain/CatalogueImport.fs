namespace OpticalConstructor.Domain

open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.MaterialStore
open OpticalConstructor.Domain.SampleStore
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.CatalogueFiles

/// The library-import seam and its in-memory stores (spec 0050 §3.4).
/// Decodes parsed catalogue entries through Result-returning save functions.
module CatalogueImport =

    /// A store refusal, undecodable entry or unreadable catalogue with its diagnostic payload (spec 0050 §3.4).
    type CatalogueImportError =
        | ImportRejectedErr of reason : string
        | UndecodableEntryErr of LibraryEntryError
        | CatalogueUnreadableErr of CatalogueError

        member this.describe : string =
            match this with
            | ImportRejectedErr reason -> $"a catalogue entry was rejected: {reason}"
            | UndecodableEntryErr error -> $"a catalogue entry could not be decoded: {error.describe}"
            | CatalogueUnreadableErr error -> $"the catalogue could not be read: {error.describe}"

    /// Saves each catalogue kind, returning CatalogueImportError on refusal (spec 0050 §3.4).
    /// Function-valued fields make proxy equality reference-based.
    [<ReferenceEquality>]
    type CatalogueImportProxy =
        {
            saveCategory : MaterialCategory -> Result<unit, CatalogueImportError>
            saveMaterial : MaterialEntry -> Result<unit, CatalogueImportError>
            saveSample : Sample -> Result<unit, CatalogueImportError>
            saveLibraryEntry : LibraryEntry -> Result<unit, CatalogueImportError>
        }

        /// Imports categories, materials, samples, then source, polarizer and detector presets (spec 0050 §3.4).
        /// Preserves row order within each kind; the first decode or save failure stops further writes, retaining prior saves.
        member this.importCatalogue (catalogue : CatalogueData) : Result<unit, CatalogueImportError> =
            let pushDecoded (save : 'Entry -> Result<unit, CatalogueImportError>) (decoded : Result<'Entry, LibraryEntryError>) : Result<unit, CatalogueImportError> =
                decoded |> Result.mapError UndecodableEntryErr |> Result.bind save

            let pushEnvelope (envelope : LibraryEntryEnvelope) : Result<unit, CatalogueImportError> =
                let content = envelope.content
                match content.schema.kind with
                | CategoryEntryKind -> content.toMaterialCategory |> pushDecoded this.saveCategory
                | MaterialEntryKind -> content.toMaterialEntry |> pushDecoded this.saveMaterial
                | SampleEntryKind -> content.toSample |> pushDecoded this.saveSample
                | SourceEntryKind -> content.toSourcePreset |> Result.map SourceItem |> pushDecoded this.saveLibraryEntry
                | PolarizerEntryKind -> content.toPolarizerPreset |> Result.map PolarizerItem |> pushDecoded this.saveLibraryEntry
                | DetectorEntryKind -> content.toDetectorPreset |> Result.map DetectorItem |> pushDecoded this.saveLibraryEntry

            [ CategoryEntryKind; MaterialEntryKind; SampleEntryKind; SourceEntryKind; PolarizerEntryKind; DetectorEntryKind ]
            |> List.collect (fun kind -> catalogue.envelopes |> List.filter (fun envelope -> envelope.content.schema.kind = kind))
            |> List.fold (fun imported envelope -> imported |> Result.bind (fun () -> pushEnvelope envelope)) (Ok ())

        /// Reads and imports a catalogue, preserving the read failure's payload (spec 0050 §3.4).
        member this.importFrom (catalogue : CatalogueProxy) : Result<unit, CatalogueImportError> =
            catalogue.readCatalogue ()
            |> Result.mapError CatalogueUnreadableErr
            |> Result.bind this.importCatalogue

    /// Empty stores bundled with the CatalogueImportProxy that fills them (spec 0050 §3.4).
    /// Read the imported catalogue through the four store proxies; equality is by reference.
    [<ReferenceEquality>]
    type InMemoryCatalogueStores =
        {
            proxy : CatalogueImportProxy
            categories : CategoryProxy
            materials : MaterialProxy
            samples : SampleProxy
            libraryEntries : LibraryProxy
        }

    type CatalogueImportProxy with

        /// Builds a CatalogueImportProxy over empty in-memory stores, mapping refusals to ImportRejectedErr (spec 0050 §3.4).
        /// Returns InMemoryCatalogueStores with live reference lookups and no database or file IO.
        static member createInMemory () : InMemoryCatalogueStores =
            let samples = SampleProxy.createInMemory ()
            let materials = MaterialProxy.createInMemory (samplesReferencing samples)
            let categories = CategoryProxy.createInMemory (materialsReferencingCategory materials)
            let libraryEntries = Library.createInMemory ()

            let categoryReason (e : CategoryError) : string =
                match e with
                | UnknownCategoryIdErr r | DuplicateCategoryIdErr r | CategoryStillReferencedErr r | BuiltInNotRemovableErr r | InvalidCategoryErr r -> r
            let materialReason (e : MaterialError) : string =
                match e with
                | UnknownMaterialIdErr r | DuplicateMaterialIdErr r | MaterialStillReferencedErr r | InvalidMaterialErr r | MaterialVersionPinnedErr r | MaterialStoreUnavailableErr r -> r
            let sampleReason (e : SampleError) : string =
                match e with
                | UnknownSampleIdErr r | DuplicateSampleIdErr r | InvalidSampleErr r | SampleVersionPinnedErr r -> r
            let libraryReason (e : LibraryError) : string =
                match e with
                | UnknownEntryIdErr r | NoEntriesForKindErr r | LibraryUnavailableErr r -> r

            let proxy =
                {
                    saveCategory = fun c -> categories.addCategory c |> Result.mapError (categoryReason >> ImportRejectedErr)
                    saveMaterial = fun m -> materials.saveMaterial m |> Result.mapError (materialReason >> ImportRejectedErr)
                    saveSample = fun s -> samples.saveSample s |> Result.mapError (sampleReason >> ImportRejectedErr)
                    saveLibraryEntry = fun e -> libraryEntries.addEntry e |> Result.mapError (libraryReason >> ImportRejectedErr)
                }
            {
                proxy = proxy
                categories = categories
                materials = materials
                samples = samples
                libraryEntries = libraryEntries.proxy
            }
