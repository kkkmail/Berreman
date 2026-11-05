namespace OpticalConstructor.Domain

open OpticalConstructor.Domain.MaterialLibrary   // MaterialCategory / MaterialEntry + standardCategories / builtInEntries / CategoryProxy / MaterialProxy / materialsReferencingCategory
open OpticalConstructor.Domain.Library           // Sample / LibraryEntry + SeedSamples / seedEntries / SampleProxy / samplesReferencing / LibraryEntryStore
open OpticalConstructor.Domain.Lifecycle         // VersionsInUse.empty (the injected in-use seam)
open OpticalConstructor.Domain.MaterialStore     // MaterialProxy.createInMemoryEmpty (the versioned store augmentation)
open OpticalConstructor.Domain.SampleStore       // SampleProxy.createInMemoryEmpty (the versioned store augmentation)

/// The library-import seam and its in-memory stores (spec 0050 §3.4).
/// Pushes the existing Domain catalogue through Result-returning save functions.
module CatalogueImport =

    /// A catalogue import refusal carrying the store's diagnostic reason.
    type CatalogueImportError =
        | ImportRejectedErr of reason : string

    /// Saves each catalogue kind, returning CatalogueImportError on refusal.
    /// Function-valued fields make proxy equality reference-based.
    [<ReferenceEquality>]
    type CatalogueImportProxy =
        {
            saveCategory : MaterialCategory -> Result<unit, CatalogueImportError>
            saveMaterial : MaterialEntry -> Result<unit, CatalogueImportError>
            saveSample : Sample -> Result<unit, CatalogueImportError>
            saveLibraryEntry : LibraryEntry -> Result<unit, CatalogueImportError>
        }

        /// Push every Domain seed in category, material, sample, then entry order so foreign keys resolve.
        /// Preserves each list's order and stops on the first ImportRejectedErr; success returns Ok ().
        member this.seedAll () : Result<unit, CatalogueImportError> =
            let pushEach (save : 'Item -> Result<unit, CatalogueImportError>) (items : 'Item list) : Result<unit, CatalogueImportError> =
                items
                |> List.fold (fun acc item -> acc |> Result.bind (fun () -> save item)) (Ok ())

            pushEach this.saveCategory standardCategories
            |> Result.bind (fun () -> pushEach this.saveMaterial builtInEntries)
            |> Result.bind (fun () -> pushEach this.saveSample SeedSamples.allSeedSamples)
            |> Result.bind (fun () -> pushEach this.saveLibraryEntry seedEntries)

    /// Empty stores bundled with the CatalogueImportProxy that fills them.
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

        /// Build a CatalogueImportProxy over empty in-memory stores, mapping refusals to ImportRejectedErr.
        /// Returns InMemoryCatalogueStores with live reference lookups and no database or file IO.
        static member createInMemory () : InMemoryCatalogueStores =
            let samples = SampleProxy.createInMemoryEmpty VersionsInUse.empty
            let materials = MaterialProxy.createInMemoryEmpty (samplesReferencing samples) VersionsInUse.empty
            let categories = CategoryProxy.createInMemoryEmpty (materialsReferencingCategory materials)
            let libraryEntries = Library.createInMemoryEmpty ()

            // Each store carries its own error DU; every case carries a diagnostic `reason` — extract
            // it (pattern-match, never reach into a case) and re-wrap as the seam's `ImportRejectedErr`.
            let categoryReason (e : CategoryError) : string =
                match e with
                | UnknownCategoryIdErr r | DuplicateCategoryIdErr r | CategoryStillReferencedErr r | BuiltInNotRemovableErr r | InvalidCategoryErr r -> r
            let materialReason (e : MaterialError) : string =
                match e with
                | UnknownMaterialIdErr r | DuplicateMaterialIdErr r | MaterialStillReferencedErr r | InvalidMaterialErr r | MaterialVersionInUseErr r -> r
            let sampleReason (e : SampleError) : string =
                match e with
                | UnknownSampleIdErr r | DuplicateSampleIdErr r | InvalidSampleErr r | SampleVersionInUseErr r -> r
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
