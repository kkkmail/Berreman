namespace OpticalConstructor.Domain

open System
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.LibraryEntryFiles

/// The catalogue-folder seam: one catalogue's data, its errors, its proxy with the in-memory double, and the
/// shipped catalogue's identity (spec 0050 001 §A, §B; .spec-md §3.4, §3.5, §4, §5).
module CatalogueFiles =

    /// The well-known catalogue ids (spec 0050 .spec-md §3.3).
    module CatalogueIds =

        /// The frozen identity of the shipped Built-in catalogue, and the one place it is written (spec 0050 .spec-md §3.3).
        let builtInCatalogue : CatalogueId = Guid.Parse "499ae2ca-4f4c-4d4f-b53e-62ef2627130a" |> CatalogueId

    /// Why a catalogue cannot be read or written; each reason names the catalogue or the file (spec 0050 .spec-md §4). In case order:
    /// IO failed or the in-memory store holds no catalogue; a file failed its parse; a manifest row names a file the folder lacks; the
    /// folder holds an entry file no row names; the manifest is not the one its entries make; the entries cannot form a manifest.
    type CatalogueError =
        | CatalogueIoErr of reason : string
        | CatalogueFileRejectedErr of reason : string
        | MissingEntryFileErr of reason : string
        | UnlistedEntryFileErr of reason : string
        | ManifestMismatchErr of reason : string
        | InvalidCatalogueEntriesErr of LibraryEntryError

        member this.describe : string =
            match this with
            | CatalogueIoErr reason -> $"catalogue IO failed: {reason}"
            | CatalogueFileRejectedErr reason -> $"a catalogue file was rejected: {reason}"
            | MissingEntryFileErr reason -> $"an entry file is missing: {reason}"
            | UnlistedEntryFileErr reason -> $"an entry file is not listed: {reason}"
            | ManifestMismatchErr reason -> $"the catalogue manifest does not match its entries: {reason}"
            | InvalidCatalogueEntriesErr error -> $"the catalogue entries are invalid: {error.describe}"

    /// One catalogue: its manifest and every entry, in the manifest's row order (spec 0050 001 §A).
    type CatalogueData =
        {
            manifest : CatalogueManifest
            envelopes : LibraryEntryEnvelope list
        }

        /// The catalogue holding `envelopes`. `CatalogueManifest.tryCreate` builds its manifest and its rows order the
        /// envelopes, so the sort rule stays that member's alone (spec 0050 001 §A).
        static member tryCreate (id : CatalogueId) (name : CatalogueName) (version : VersionNumber) (envelopes : LibraryEntryEnvelope list) : Result<CatalogueData, CatalogueError> =
            match CatalogueManifest.tryCreate id name version envelopes with
            | Error error -> Error (InvalidCatalogueEntriesErr error)
            | Ok manifest ->
                let catalogue : CatalogueData =
                    {
                        manifest = manifest
                        envelopes = manifest.entries |> List.choose (fun row -> envelopes |> List.tryFind (fun e -> e.manifestRow = row))
                    }
                Ok catalogue

        /// The one consistency rule both stores call (spec 0050 .spec-md §5): the manifest is exactly the one its entries
        /// make, and the envelopes' rows are its entries in order. The rebuilt manifest is sorted, so it alone cannot see
        /// envelopes out of order.
        member this.validate : Result<unit, CatalogueError> =
            let manifest = this.manifest
            let rowsInOrder = this.envelopes |> List.map (fun e -> e.manifestRow)

            match CatalogueManifest.tryCreate manifest.id manifest.name manifest.version this.envelopes with
            | Error error -> Error (InvalidCatalogueEntriesErr error)
            | Ok rebuilt when rebuilt = manifest && rowsInOrder = manifest.entries -> Ok ()
            | Ok _ -> Error (ManifestMismatchErr $"catalogue '{manifest.name.value}' ({manifest.id.value}): its manifest is not the one its entries make")

    /// One catalogue folder's seam (spec 0050 001 §A; .spec-md §5). Read answers the catalogue with every file validated and
    /// hash-checked, or a typed error. Write refuses a catalogue that fails `validate` and then writes nothing; otherwise it
    /// writes every file atomically, so read answers an equal catalogue. Storage's `create` is the folder implementation.
    [<ReferenceEquality>]
    type CatalogueProxy =
        {
            /// Reads and checks the manifest without opening an entry file (spec 0050 S3.1).
            readManifest : unit -> Result<CatalogueManifest, CatalogueError>
            readCatalogue : unit -> Result<CatalogueData, CatalogueError>
            writeCatalogue : CatalogueData -> Result<unit, CatalogueError>
        }

        /// The in-memory double, holding at most one catalogue (spec 0050 .spec-md §4: every proxy has one). Write validates,
        /// then replaces what it holds; read answers what it holds, or `CatalogueIoErr` while nothing has been written.
        static member createInMemory () : CatalogueProxy =
            let store = ref (None : CatalogueData option)

            let readCatalogue () : Result<CatalogueData, CatalogueError> =
                match store.Value with
                | Some catalogue -> Ok catalogue
                | None -> Error (CatalogueIoErr "the in-memory store holds no catalogue: nothing has been written")

            {
                readManifest = fun () -> readCatalogue () |> Result.map (fun (catalogue : CatalogueData) -> catalogue.manifest)
                readCatalogue = readCatalogue
                writeCatalogue =
                    fun catalogue ->
                        catalogue.validate
                        |> Result.map (fun () -> store.Value <- Some catalogue)
            }
