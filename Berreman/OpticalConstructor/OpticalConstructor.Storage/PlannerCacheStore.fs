namespace OpticalConstructor.Storage

open System.Text.Json
open Softellect.Sys.Primitives
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.KitFolders
open OpticalConstructor.Domain.MeasurementPlanner
open OpticalConstructor.Storage.Errors
open OpticalConstructor.Storage.CanonicalJson

/// The Planner's candidate cache under a kit's `.oc/planner/`: derived, safe to delete and never sent, so no schema
/// covers its files (spec 0050 S14.2).
module PlannerCacheStore =

    type PlannerCacheKey with

        /// The lower-case hexadecimal SHA-256 of the key's canonical JSON, then `.candidate-json` (spec 0050 S14.2).
        member this.cacheFileName : Result<FileName, StorageError> =
            contentHashOf this |> Result.map (fun (hash : ContentHash) -> FileName $"{hash.value}.candidate-json")

    type PlannerCacheEntry with

        /// Canonical entry JSON with LF line ends and one final newline (spec 0050 S14.2).
        member this.fileText : Result<KitFileText, StorageError> =
            nodeOf this |> Result.map (fun node -> node.canonicalFileText |> KitFileText)

    type KitFileText with

        /// Binds cache text through the shared JSON options; text that does not bind is MalformedJsonErr (spec 0050
        /// S14.2).
        member this.parsePlannerCacheEntry : Result<PlannerCacheEntry, StorageError> =
            try
                let entry = JsonSerializer.Deserialize<PlannerCacheEntry>(this.withoutByteOrderMark.value, options)
                if Unchecked.equals entry Unchecked.defaultof<PlannerCacheEntry> then
                    Error (MalformedJsonErr "a planner cache entry must contain a key and its rows")
                else
                    Ok entry
            with error -> Error (MalformedJsonErr error.Message)

    type KitContext with

        /// The rows of the entry of an equal key in `.oc/planner/`, without computing; otherwise `compute ()`, its
        /// rows written as the key's entry under read-write access only; a store refusal is PlannerCacheErr (spec 0050
        /// S14.2; .spec-md §3.12).
        member this.cachedPlannerRows
            (key : PlannerCacheKey)
            (compute : unit -> Result<CandidateRows, PlannerError>)
            : Result<CandidateRows, PlannerError> =
            key.cacheFileName
            |> Result.mapError (fun (error : StorageError) -> PlannerCacheErr error.describe)
            |> Result.bind (fun name ->
                this.proxy.readPlannerCacheFile name
                |> Result.mapError (fun (error : KitError) -> PlannerCacheErr error.describe)
                |> Result.bind (fun (textOpt : KitFileText option) ->
                    let hitOpt : CandidateRows option =
                        textOpt
                        |> Option.bind (fun (text : KitFileText) ->
                            match text.parsePlannerCacheEntry with
                            | Ok entry when entry.key = key -> Some entry.rows
                            | Ok _
                            | Error _ -> None)

                    match hitOpt with
                    | Some rows -> Ok rows
                    | None ->
                        compute ()
                        |> Result.bind (fun (rows : CandidateRows) ->
                            match this.access with
                            | ReadOnlyKitAccess -> Ok rows
                            | ReadWriteKitAccess ->
                                let entry : PlannerCacheEntry = { key = key; rows = rows }

                                entry.fileText
                                |> Result.mapError (fun (error : StorageError) -> PlannerCacheErr error.describe)
                                |> Result.bind (fun (text : KitFileText) ->
                                    this.proxy.writePlannerCacheFile name text
                                    |> Result.mapError (fun (error : KitError) -> PlannerCacheErr error.describe))
                                |> Result.map (fun () -> rows))))
