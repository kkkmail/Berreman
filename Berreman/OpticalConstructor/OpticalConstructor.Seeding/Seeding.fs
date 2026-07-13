namespace OpticalConstructor.Seeding

open OpticalConstructor.Domain                   // Library.createInMemoryEmpty (the writable library-entry store)
open OpticalConstructor.Domain.MaterialLibrary   // MaterialCategory / MaterialEntry + standardCategories / builtInEntries / CategoryProxy / MaterialProxy / materialsReferencingCategory
open OpticalConstructor.Domain.Library           // Sample / LibraryEntry + SeedSamples / seedEntries / SampleProxy / samplesReferencing / LibraryEntryStore
open OpticalConstructor.Domain.Lifecycle         // VersionsInUse.empty (the injected in-use seam)
open OpticalConstructor.Domain.MaterialStore     // MaterialProxy.createInMemoryEmpty (the versioned store augmentation)
open OpticalConstructor.Domain.SampleStore       // SampleProxy.createInMemoryEmpty (the versioned store augmentation)

/// Spec 0038 (041, ADD_CONTRACT STORE_XDUO_0008) — the seed-push seam. Declares, in a new
/// `OpticalConstructor.Seeding` project, the DECLARED-lifecycle `SeedingProxy`: the write boundary
/// that pushes the app's built-in seed catalogue (categories, materials, samples, library entries)
/// into a store, plus the pure `seedAll` orchestrator that drives it.
///
/// The seeds STAY where they already live next to their types — `MaterialLibrary.standardCategories`
/// / `builtInEntries` and `Library.SeedSamples.all` / `seedEntries`. `seedAll` only READS those
/// module-level values and hands each through the proxy in a fixed order; it never re-defines a seed.
/// The seeded Guids are frozen forever (they are future foreign keys), so the identity that flows
/// through the seam is exactly the identity the Domain already froze.
///
/// The seam is kept as pure DATA (the `Library.LibraryProxy` / `ExperimentData.ExperimentDataProxy`
/// convention): a record of camelCase `Result`-returning functions, so logic that holds the proxy
/// stays referentially transparent and a test substitutes a recording stub of the SAME shape.
/// Function-valued fields have no structural equality, so the record is `[<ReferenceEquality>]` — a
/// host that holds one keeps a usable equality, comparing the proxy by identity.
///
/// DECLARED lifecycle: this is the seam + its pure orchestrator ONLY — no real store, no persistence,
/// no consumer wiring. A later `IMPLEMENT_CONTRACT STORE_XDUO_0008` supplies the real store behind
/// this surface (e.g. the EFC-backed database), leaving `seedAll` unchanged.
module Seeding =

    /// The seed-push error channel (errors as values; the case carries a diagnostic `reason` — a bare
    /// error case is useless in a log). A store rejects a seed — a duplicate id, a failed write, a
    /// constraint violation — as `SeedRejected`, never a throw across this boundary.
    type SeedingError =
        | SeedRejected of reason : string

    /// The seed-push seam (the functional-proxy convention): a record of camelCase `Result`-returning
    /// functions, one per seeded kind, each taking the elevated Domain value and returning `unit` or a
    /// typed `SeedingError`. A test substitutes a recording stub of the SAME shape; a later real store
    /// (`IMPLEMENT_CONTRACT`) supplies functions that persist.
    ///
    /// `[<ReferenceEquality>]` because the function-valued fields have no structural equality; a host
    /// that holds the proxy compares it by identity.
    [<ReferenceEquality>]
    type SeedingProxy =
        {
            saveCategory : MaterialCategory -> Result<unit, SeedingError>
            saveMaterial : MaterialEntry -> Result<unit, SeedingError>
            saveSample : Sample -> Result<unit, SeedingError>
            saveLibraryEntry : LibraryEntry -> Result<unit, SeedingError>
        }

    /// Push every EXISTING Domain seed value through `proxy`, in category → material → sample → entry
    /// order (foreign keys resolve in that order: an entry may reference a sample, a sample references
    /// a material, a material references a category). Within each kind the seeds go in the order the
    /// Domain lists them. The first `SeedRejected` short-circuits — the remaining seeds are not
    /// pushed — and its error is returned; an all-`Ok` run returns `Ok ()`.
    let seedAll (proxy : SeedingProxy) : Result<unit, SeedingError> =
        let pushEach (save : 'Item -> Result<unit, SeedingError>) (items : 'Item list) : Result<unit, SeedingError> =
            items
            |> List.fold (fun acc item -> acc |> Result.bind (fun () -> save item)) (Ok ())

        pushEach proxy.saveCategory standardCategories
        |> Result.bind (fun () -> pushEach proxy.saveMaterial builtInEntries)
        |> Result.bind (fun () -> pushEach proxy.saveSample SeedSamples.all)
        |> Result.bind (fun () -> pushEach proxy.saveLibraryEntry seedEntries)

    /// The four EMPTY in-memory stores the seed pipeline fills, bundled with the `SeedingProxy` that
    /// writes into them (spec 0038 step 042 — IMPLEMENT_CONTRACT STORE_XDUO_0008). A caller runs
    /// `seedAll ctx.proxy` and then reads the reproduced catalogue back through `categories` /
    /// `materials` / `samples` / `libraryEntries` — proving the pipeline fills a blank store with the
    /// seeded catalogue using NO database and NO file IO. `[<ReferenceEquality>]` because it carries
    /// the function-valued proxies.
    [<ReferenceEquality>]
    type InMemorySeedStores =
        {
            proxy : SeedingProxy
            categories : CategoryProxy
            materials : MaterialProxy
            samples : SampleProxy
            libraryEntries : LibraryProxy
        }

    type SeedingProxy with

        /// Build a `SeedingProxy` over EMPTY in-memory stores (the IMPLEMENT_CONTRACT for
        /// STORE_XDUO_0008): each save adapts the underlying store's add verb —
        /// `CategoryProxy.addCategory`, `MaterialProxy.saveMaterial`, `SampleProxy.saveSample`, and the
        /// library-entry store's `addEntry` — mapping the store's typed error to `SeedRejected reason`.
        /// NO database, NO file IO: every store is an EMPTY `createInMemoryEmpty` the pipeline fills.
        /// Store build order mirrors `AppContext.create` (samples → materials → categories) so the
        /// referencing lookups stay wired to the live stores; a future DB cycle swaps only these four
        /// field bindings, leaving `seedAll` and the seam unchanged. Returns the proxy bundled with the
        /// four stores so a caller can seed then read the reproduced catalogue back.
        static member createInMemory () : InMemorySeedStores =
            let samples = SampleProxy.createInMemoryEmpty VersionsInUse.empty
            let materials = MaterialProxy.createInMemoryEmpty (samplesReferencing samples) VersionsInUse.empty
            let categories = CategoryProxy.createInMemoryEmpty (materialsReferencingCategory materials)
            let libraryEntries = Library.createInMemoryEmpty ()

            // Each store carries its own error DU; every case carries a diagnostic `reason` — extract
            // it (pattern-match, never reach into a case) and re-wrap as the seam's `SeedRejected`.
            let categoryReason (e : CategoryError) : string =
                match e with
                | UnknownCategoryId r | DuplicateCategoryId r | CategoryStillReferenced r | BuiltInNotRemovable r | InvalidCategory r -> r
            let materialReason (e : MaterialError) : string =
                match e with
                | UnknownMaterialId r | DuplicateMaterialId r | MaterialStillReferenced r | InvalidMaterial r | MaterialVersionInUse r -> r
            let sampleReason (e : SampleError) : string =
                match e with
                | UnknownSampleId r | DuplicateSampleId r | InvalidSample r | SampleVersionInUse r -> r
            let libraryReason (e : LibraryError) : string =
                match e with
                | UnknownEntryId r | NoEntriesForKind r | LibraryUnavailable r -> r

            let proxy =
                {
                    saveCategory = fun c -> categories.addCategory c |> Result.mapError (categoryReason >> SeedRejected)
                    saveMaterial = fun m -> materials.saveMaterial m |> Result.mapError (materialReason >> SeedRejected)
                    saveSample = fun s -> samples.saveSample s |> Result.mapError (sampleReason >> SeedRejected)
                    saveLibraryEntry = fun e -> libraryEntries.addEntry e |> Result.mapError (libraryReason >> SeedRejected)
                }
            {
                proxy = proxy
                categories = categories
                materials = materials
                samples = samples
                libraryEntries = libraryEntries.proxy
            }
