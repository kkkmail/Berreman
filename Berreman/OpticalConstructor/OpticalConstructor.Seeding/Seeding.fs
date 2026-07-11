namespace OpticalConstructor.Seeding

open OpticalConstructor.Domain.MaterialLibrary   // MaterialCategory / MaterialEntry + standardCategories / builtInEntries
open OpticalConstructor.Domain.Library           // Sample / LibraryEntry + SeedSamples / seedEntries

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
