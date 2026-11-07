namespace OpticalConstructor.Domain

open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.KitFolders
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.CatalogueImport

/// Snapshots a kit's referenced library versions and their dependency closure (spec 0050 S4.2).
module KitLibrary =

    /// A kit snapshot failure, preserving the storage/import error or missing reference (spec 0050 S4.2).
    type KitLibraryError =
        | KitLibraryKitErr of KitError
        | KitLibraryImportErr of CatalogueImportError
        | UnheldEntryErr of reason : string

        /// Describes the failed condition and its underlying diagnostic payload (spec 0050 S4.2).
        member this.describe : string =
            match this with
            | KitLibraryKitErr error -> $"the kit refused the library snapshot: {error.describe}"
            | KitLibraryImportErr error -> $"the library refused the kit snapshot: {error.describe}"
            | UnheldEntryErr reason -> $"neither the library nor the kit holds the reference: {reason}"

    /// One opened kit and the library seam supplying and pinning its versions (spec 0050 S4.2).
    [<ReferenceEquality>]
    type KitLibraryContext =
        {
            kit : KitContext
            imports : CatalogueImportContext
        }

        /// Pins and writes missing versions depth first, retaining held snapshots and returning writes in visit order (spec 0050 S4.2).
        /// Read-only access performs no IO; the first failure stops traversal and keeps earlier writes for the next call.
        member this.snapshotReferences (references : EntryReference list) : Result<LibraryVersionRef list, KitLibraryError> =
            match this.kit.access with
            | ReadOnlyKitAccess -> Error (KitLibraryKitErr (KitReadOnlyErr "snapshotReferences"))
            | ReadWriteKitAccess ->
                this.kit.proxy.listSnapshots ()
                |> Result.mapError KitLibraryKitErr
                |> Result.bind (fun envelopes ->
                    let snapshots = envelopes |> List.map (fun envelope -> envelope.libraryVersionRef, envelope) |> Map.ofList

                    let resolveReference (reference : EntryReference) : Result<LibraryEntryEnvelope, KitLibraryError> =
                        let resolved =
                            match reference with
                            | EntryVersionReference version ->
                                match Map.tryFind version snapshots with
                                | Some envelope -> Ok (Some envelope)
                                | None -> this.imports.proxy.tryHeldEnvelope version.entryId version.version
                            | EntryLatestReference id ->
                                this.imports.proxy.tryLatestEnvelope id
                                |> Result.map (function
                                    | Some envelope -> Some envelope
                                    | None ->
                                        envelopes
                                        |> List.filter (fun envelope -> envelope.content.id = id)
                                        |> List.sortByDescending (fun envelope -> envelope.content.version)
                                        |> List.tryHead)
                        resolved
                        |> Result.mapError KitLibraryImportErr
                        |> Result.bind (function
                            | None -> Error (UnheldEntryErr reference.describe)
                            | Some envelope ->
                                Map.tryFind envelope.libraryVersionRef snapshots
                                |> Option.defaultValue envelope
                                |> Ok)

                    let rec visitReferences
                        (visited : Set<LibraryVersionRef>)
                        (written : LibraryVersionRef list)
                        (pending : EntryReference list)
                        : Result<LibraryVersionRef list, KitLibraryError> =
                        match pending with
                        | [] -> Ok (List.rev written)
                        | reference :: rest ->
                            resolveReference reference
                            |> Result.bind (fun envelope ->
                                let version = envelope.libraryVersionRef
                                if Set.contains version visited then
                                    visitReferences visited written rest
                                else
                                    let writeResult =
                                        if Map.containsKey version snapshots then
                                            Ok written
                                        else
                                            this.imports.proxy.pinEntryVersion version.entryId version.version
                                            |> Result.mapError KitLibraryImportErr
                                            |> Result.bind (fun () -> this.kit.proxy.writeSnapshot envelope |> Result.mapError KitLibraryKitErr)
                                            |> Result.map (fun () -> version :: written)
                                    writeResult
                                    |> Result.bind (fun nextWritten ->
                                        visitReferences (Set.add version visited) nextWritten (envelope.content.references @ rest)))

                    visitReferences Set.empty [] references)
