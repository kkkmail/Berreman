namespace OpticalConstructor.Domain

open System
open System.Text
open Softellect.Sys.Primitives
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.LibraryEntryFiles

/// The kit-folder seam and its shared in-memory double (spec 0050 S4.1).
module KitFolders =

    /// One folder's kit files via Storage's KitProxy.create or createInMemory; only initialisation makes it a kit (spec 0050 S4.1).
    [<ReferenceEquality>]
    type KitProxy =
        {
            /// Preserves existing contents and adds metadata, data and library, or refuses an existing kit (spec 0050 S4.1).
            initialiseKit : KitFile -> Result<unit, KitError>
            /// Reads the metadata, or returns NotAKitFolderErr without a kit file (spec 0050 S4.1).
            readKitFile : unit -> Result<KitFile, KitError>
            /// Replaces the metadata whole, or returns NotAKitFolderErr without writing (spec 0050 S4.1).
            writeKitFile : KitFile -> Result<unit, KitError>
            /// Lists root-file stems in ordinal order by the kind's extension, ignoring its case (spec 0050 S4.1).
            listKindFiles : KitFileKind -> Result<FileStem list, KitError>
            /// Reads a named kind file, or returns MissingKitFileErr naming it (spec 0050 S4.1).
            readKindFile : KitFileKind -> FileStem -> Result<KitFileText, KitError>
            /// Creates or replaces a named kind file whole (spec 0050 S4.1).
            writeKindFile : KitFileKind -> FileStem -> KitFileText -> Result<unit, KitError>
            /// Deletes a named kind file, or returns MissingKitFileErr without changes (spec 0050 S4.1).
            deleteKindFile : KitFileKind -> FileStem -> Result<unit, KitError>
            /// Reads the kit-relative file's text and ContentHash.ofBytes of its stored bytes, or MissingKitFileErr naming the path (spec 0050 S4.1).
            readDataFile : KitRelativePath -> Result<KitDataFile, KitError>
        }

    type FileName with

        /// A root filename used as a path within its kit (spec 0050 S4.1).
        member private this.toKitRelativePath : KitRelativePath = KitRelativePath this.value

    type KitRelativePath with

        /// The parent folders an outside writer needs for this path (spec 0050 S4.1).
        member private this.parentFolders : KitRelativePath list =
            let separator = this.value.LastIndexOf('/')
            if separator < 0 then
                []
            else
                let parent = KitRelativePath (this.value.Substring(0, separator))
                parent :: parent.parentFolders

    type KitFileKind with

        /// Selects only root files of this kind and orders their stems ordinally (spec 0050 S4.1).
        member this.stemsAtKitRoot (paths : KitRelativePath list) : FileStem list =
            let extension = this.extension
            paths
            |> List.choose (fun path ->
                if not (path.value.Contains('/') || path.value.Contains('\\'))
                   && path.value.EndsWith(extension.value, StringComparison.OrdinalIgnoreCase) then
                    path.value.Substring(0, path.value.Length - extension.value.Length) |> FileStem.ofText |> Some
                else
                    None)
            |> List.sortWith (fun first second -> String.CompareOrdinal(first.value, second.value))

    type KitFileText with

        /// Removes only the leading UTF-8 byte-order mark from decoded text (spec 0050 S4.1).
        member this.withoutByteOrderMark : KitFileText =
            if this.value.StartsWith('\uFEFF') then KitFileText (this.value.Substring 1)
            else this

    /// One folder's private contents, shared by every proxy opened over it (spec 0050 S4.1).
    type InMemoryKitFolder private () =
        let kitFilePath = KitFile.fileName.toKitRelativePath
        let mutable kitFileOpt : KitFile option = None
        let mutable texts : Map<KitRelativePath, KitFileText> = Map.empty
        let mutable folders : Set<KitRelativePath> = Set.empty

        /// Creates an existing, empty folder (spec 0050 S4.1).
        static member create () : InMemoryKitFolder = InMemoryKitFolder ()

        member private _.tryFindTextPath (path : KitRelativePath) : KitRelativePath option =
            texts
            |> Map.keys
            |> Seq.tryFind (fun held -> String.Equals(held.value, path.value, StringComparison.OrdinalIgnoreCase))

        /// Places an outside writer's text and its parents, refusing the reserved kit filename (spec 0050 S4.1).
        member this.placeLabFile (path : KitRelativePath) (text : KitFileText) : Result<unit, KitError> =
            if String.Equals(path.value, kitFilePath.value, StringComparison.OrdinalIgnoreCase) then
                Error (InvalidKitPathErr $"in-memory kit folder: '{path.value}' is reserved for kit metadata")
            else
                let heldPath = this.tryFindTextPath path |> Option.defaultValue path
                texts <- Map.add heldPath text texts
                folders <- Set.union folders (Set.ofList path.parentFolders)
                Ok ()

        /// Lists every file and folder by its kit-relative path in ordinal order (spec 0050 S4.1).
        member _.paths () : KitRelativePath list =
            let filePaths = texts |> Map.keys |> Set.ofSeq
            Set.union folders filePaths
            |> Set.toList
            |> List.sortWith (fun first second -> String.CompareOrdinal(first.value, second.value))

        /// Opens shared folder state with the supplied metadata serializer, retaining its text for any-file reads (spec 0050 S4.1).
        member this.createKitProxy (serializeMetadata : KitFile -> Result<KitFileText, KitError>) : KitProxy =
            let writeMetadata (kit : KitFile) : Result<unit, KitError> =
                serializeMetadata kit
                |> Result.map (fun text ->
                    texts <- Map.add kitFilePath text texts
                    kitFileOpt <- Some kit)

            {
                initialiseKit =
                    fun kit ->
                        match kitFileOpt with
                        | Some _ -> Error (KitAlreadyExistsErr "in-memory kit folder")
                        | None ->
                            writeMetadata kit
                            |> Result.map (fun () ->
                                let contentFolders = Set.ofList [ KitRelativePath "data"; KitRelativePath "library" ]
                                folders <- Set.union folders contentFolders)
                readKitFile =
                    fun () ->
                        match kitFileOpt with
                        | Some kit -> Ok kit
                        | None -> Error (NotAKitFolderErr "in-memory kit folder")
                writeKitFile =
                    fun kit ->
                        match kitFileOpt with
                        | Some _ -> writeMetadata kit
                        | None -> Error (NotAKitFolderErr "in-memory kit folder")
                listKindFiles =
                    fun kind -> texts |> Map.keys |> Seq.toList |> kind.stemsAtKitRoot |> Ok
                readKindFile =
                    fun kind stem ->
                        let path = (kind.fileNameOf stem).toKitRelativePath
                        match this.tryFindTextPath path |> Option.bind (fun held -> Map.tryFind held texts) with
                        | Some text -> Ok text.withoutByteOrderMark
                        | None -> Error (MissingKitFileErr $"in-memory kit folder: '{path.value}'")
                writeKindFile =
                    fun kind stem text ->
                        let path = (kind.fileNameOf stem).toKitRelativePath
                        let heldPath = this.tryFindTextPath path |> Option.defaultValue path
                        texts <- Map.add heldPath text texts
                        Ok ()
                deleteKindFile =
                    fun kind stem ->
                        let path = (kind.fileNameOf stem).toKitRelativePath
                        match this.tryFindTextPath path with
                        | Some held ->
                            texts <- Map.remove held texts
                            Ok ()
                        | None -> Error (MissingKitFileErr $"in-memory kit folder: '{path.value}'")
                readDataFile =
                    fun path ->
                        match this.tryFindTextPath path |> Option.bind (fun held -> Map.tryFind held texts) with
                        | Some text ->
                            let bytes = Encoding.UTF8.GetBytes text.value |> FileByteContent
                            Ok
                                {
                                    path = path
                                    text = text.withoutByteOrderMark
                                    contentHash = ContentHash.ofBytes bytes
                                }
                        | None -> Error (MissingKitFileErr $"in-memory kit folder: '{path.value}'")
            }

