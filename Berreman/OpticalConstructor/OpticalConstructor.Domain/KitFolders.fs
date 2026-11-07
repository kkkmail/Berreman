namespace OpticalConstructor.Domain

open System
open Softellect.Sys.Primitives
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.LibraryEntryFiles

/// The kit-folder seam and its shared in-memory double (spec 0050 S4.1).
module KitFolders =

    /// One folder's kit files via Storage's KitProxy.create or the createInMemory double; only initialisation makes it a kit (spec 0050 S4.1).
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
            let paths = Set.union folders filePaths
            let pathsWithKit =
                match kitFileOpt with
                | Some _ -> Set.add kitFilePath paths
                | None -> paths
            pathsWithKit
            |> Set.toList
            |> List.sortWith (fun first second -> String.CompareOrdinal(first.value, second.value))

        /// Captures this folder's state in a fresh proxy without exposing its mutable contents (spec 0050 S4.1).
        member internal this.createKitProxy () : KitProxy =
            {
                initialiseKit =
                    fun kit ->
                        match kitFileOpt with
                        | Some _ -> Error (KitAlreadyExistsErr "in-memory kit folder")
                        | None ->
                            let contentFolders = Set.ofList [ KitRelativePath "data"; KitRelativePath "library" ]
                            folders <- Set.union folders contentFolders
                            kitFileOpt <- Some kit
                            Ok ()
                readKitFile =
                    fun () ->
                        match kitFileOpt with
                        | Some kit -> Ok kit
                        | None -> Error (NotAKitFolderErr "in-memory kit folder")
                writeKitFile =
                    fun kit ->
                        match kitFileOpt with
                        | Some _ ->
                            kitFileOpt <- Some kit
                            Ok ()
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
            }

    type KitProxy with

        /// Opens a new proxy over the same folder state as every other proxy naming it (spec 0050 S4.1).
        static member createInMemory (folder : InMemoryKitFolder) : KitProxy = folder.createKitProxy ()
