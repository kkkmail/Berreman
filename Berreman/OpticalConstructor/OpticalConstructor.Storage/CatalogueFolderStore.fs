namespace OpticalConstructor.Storage

open System
open System.IO
open System.Text
open Softellect.Sys.Primitives
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.CatalogueFiles
open OpticalConstructor.Domain.WorkbenchSettings
open OpticalConstructor.Storage.Errors
open OpticalConstructor.Storage.EntryFileJson

/// The folder implementation of `CatalogueProxy`: one `.catalogue-json` beside one file per entry (spec 0050 001 §A;
/// .spec-md §3.5, §5). Every .NET IO call is caught here and mapped to a typed error (.spec-md §4).
module CatalogueFolderStore =

    type LibraryFileText with

        /// The text at `file` as UTF-8 without a BOM, written to a `.tmp` beside it and then moved over it, so a reader
        /// never meets a file that is not whole (spec 0050 .spec-md §4). A failure deletes the `.tmp` and is `FileIoExn`.
        member this.writeAtomically (file : FileName) : Result<unit, StorageError> =
            let tmp = FileName $"{file.value}.tmp"

            try
                File.WriteAllText(tmp.value, this.value, UTF8Encoding false)
                File.Move(tmp.value, file.value, true)
                Ok ()
            with e ->
                // The write's failure is the one to report, so a delete that also fails stays silent.
                try
                    File.Delete tmp.value
                with _ -> ()

                Error (FileIoExn e)

    /// An IO call as a value: its failure is `CatalogueIoErr` naming the folder.
    let private tryFolderIo (folder : FolderName) (io : unit -> 'Value) : Result<'Value, CatalogueError> =
        try Ok (io ())
        with e -> Error (CatalogueIoErr $"catalogue folder '{folder.value}': {e.Message}")

    let private fileRejectedErr (folder : FolderName) (file : FileName) (error : StorageError) : CatalogueError =
        CatalogueFileRejectedErr $"catalogue folder '{folder.value}': the file '{file.value}' is rejected: {error.describe}"

    let private unlistedFileErr (folder : FolderName) (file : FileName) : CatalogueError =
        UnlistedEntryFileErr $"catalogue folder '{folder.value}': the catalogue does not name the entry file '{file.value}'"

    /// The first of `candidates` that `names` does not hold. Names compare by the Windows rule step 26 uses
    /// (spec 0050 001 §A).
    let private tryFindUnnamed (names : FileName list) (candidates : FileName list) : FileName option =
        candidates
        |> List.tryFind (fun (candidate : FileName) ->
            names |> List.forall (fun (name : FileName) -> not (String.Equals(name.value, candidate.value, StringComparison.OrdinalIgnoreCase))))

    /// `step` over `items` in order; the first error ends it, so nothing after a failed item runs.
    let rec private tryMapInOrder (step : 'Item -> Result<'Value, CatalogueError>) (items : 'Item list) : Result<'Value list, CatalogueError> =
        match items with
        | [] -> Ok []
        | item :: rest ->
            step item
            |> Result.bind (fun value -> tryMapInOrder step rest |> Result.map (fun values -> value :: values))

    type FolderName with

        /// The top-level files whose extension is a kind's (spec 0050 .spec-md §3.5): the manifest, a `.tmp` and a README
        /// are not entry files. This throws on IO, so every caller runs it inside `tryFolderIo`.
        member private this.listEntryFiles () : FileName list =
            let entryExtensions = LibraryEntryKind.all |> List.map (fun (kind : LibraryEntryKind) -> kind.extension)

            Directory.GetFiles(this.value)
            |> Array.toList
            |> List.map (fun (path : string) -> FileName (Path.GetFileName(path)))
            |> List.filter (fun (file : FileName) ->
                let extension = Path.GetExtension(file.value)
                entryExtensions |> List.exists (fun (entryExtension : FileExtension) -> String.Equals(extension, entryExtension.value, StringComparison.OrdinalIgnoreCase)))

        member private this.readText (file : FileName) : Result<LibraryFileText, CatalogueError> =
            let path = file.combine this
            tryFolderIo this (fun () -> LibraryFileText (File.ReadAllText path.value))

        /// The catalogue this folder holds, checked in order (spec 0050 001 §A): the manifest reads and parses; no row
        /// lacks its file; no entry file lacks its row; every file parses with its schema, version and hash; and the
        /// result passes `validate`. A row's name must match the listing, so a row cannot reach outside the folder.
        member private this.readCatalogueFolder () : Result<CatalogueData, CatalogueError> =
            let parseRow (row : CatalogueManifestRow) : Result<LibraryEntryEnvelope, CatalogueError> =
                this.readText row.fileName
                |> Result.bind (fun (text : LibraryFileText) -> text.parseEntry row.kind |> Result.mapError (fileRejectedErr this row.fileName))

            let readEntries (manifest : CatalogueManifest) (entryFiles : FileName list) : Result<CatalogueData, CatalogueError> =
                let rowFiles = manifest.entries |> List.map (fun (row : CatalogueManifestRow) -> row.fileName)

                match tryFindUnnamed entryFiles rowFiles, tryFindUnnamed rowFiles entryFiles with
                | Some missing, _ -> Error (MissingEntryFileErr $"catalogue folder '{this.value}': its manifest names '{missing.value}', an entry file the folder lacks")
                | None, Some unlisted -> Error (unlistedFileErr this unlisted)
                | None, None ->
                    manifest.entries
                    |> tryMapInOrder parseRow
                    |> Result.bind (fun (envelopes : LibraryEntryEnvelope list) ->
                        let catalogue : CatalogueData = { manifest = manifest; envelopes = envelopes }
                        catalogue.validate |> Result.map (fun () -> catalogue))

            this.readText CatalogueManifest.fileName
            |> Result.bind (fun (text : LibraryFileText) -> text.parseCatalogueManifest |> Result.mapError (fileRejectedErr this CatalogueManifest.fileName))
            |> Result.bind (fun (manifest : CatalogueManifest) ->
                tryFolderIo this (fun () -> this.listEntryFiles ())
                |> Result.bind (readEntries manifest))

        /// Writes `catalogue` here (spec 0050 001 §A; .spec-md §4): it must pass `validate`; a folder holding an entry
        /// file it does not name is refused, so a write never deletes a file and a written folder reads back equal. The
        /// manifest goes last, so a reader never meets one naming a file that is not yet whole. No schema is read, so this
        /// also runs where no `schema/` folder sits beside the assembly (step 35's script).
        member private this.writeCatalogueFolder (catalogue : CatalogueData) : Result<unit, CatalogueError> =
            let refuseUnlistedFiles () : Result<unit, CatalogueError> =
                let named = catalogue.envelopes |> List.map (fun (envelope : LibraryEntryEnvelope) -> envelope.content.fileName)

                tryFolderIo this (fun () -> if Directory.Exists this.value then this.listEntryFiles () else [])
                |> Result.bind (fun (held : FileName list) ->
                    match tryFindUnnamed named held with
                    | Some unlisted -> Error (unlistedFileErr this unlisted)
                    | None -> Ok ())

            let render ((file, text) : FileName * Result<LibraryFileText, StorageError>) : Result<FileName * LibraryFileText, CatalogueError> =
                text
                |> Result.mapError (fileRejectedErr this file)
                |> Result.map (fun (ready : LibraryFileText) -> file, ready)

            // Every text exists before the first write, so a serialization failure leaves the folder untouched.
            let renderManifestLast () : Result<(FileName * LibraryFileText) list, CatalogueError> =
                (catalogue.envelopes |> List.map (fun (envelope : LibraryEntryEnvelope) -> envelope.content.fileName, envelope.fileText))
                @ [ CatalogueManifest.fileName, catalogue.manifest.fileText ]
                |> tryMapInOrder render

            let write ((file, text) : FileName * LibraryFileText) : Result<unit, CatalogueError> =
                text.writeAtomically (file.combine this)
                |> Result.mapError (fun (error : StorageError) -> CatalogueIoErr $"catalogue folder '{this.value}': the file '{file.value}' is not written: {error.describe}")

            catalogue.validate
            |> Result.bind refuseUnlistedFiles
            |> Result.bind renderManifestLast
            |> Result.bind (fun (rendered : (FileName * LibraryFileText) list) ->
                tryFolderIo this (fun () -> Directory.CreateDirectory this.value |> ignore)
                |> Result.bind (fun () -> tryMapInOrder write rendered)
                |> Result.map ignore)

    type CatalogueProxy with

        /// The proxy over one catalogue folder (spec 0050 .spec-md §5): the Domain record, with its real `create` here.
        static member create (folder : FolderName) : CatalogueProxy =
            {
                readCatalogue = fun () -> folder.readCatalogueFolder ()
                writeCatalogue = fun (catalogue : CatalogueData) -> folder.writeCatalogueFolder catalogue
            }

        /// The proxy over the given shipped-catalogue folder's Builtin subfolder (spec 0050 S2.4).
        static member createBuiltInUnder (shipped : FolderName) : CatalogueProxy =
            let builtIn = FolderName "Builtin"
            CatalogueProxy.create (shipped.combine builtIn)

        /// The shipped catalogue proxy at the default location beside the assembly (spec 0050 §3.5; S2.4).
        static member createBuiltIn () : CatalogueProxy =
            let installFolder = FolderName System.AppContext.BaseDirectory
            CatalogueProxy.createBuiltInUnder (installFolder.combine DataLocations.shippedCatalogueDefault)
