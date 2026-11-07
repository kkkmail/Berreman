namespace OpticalConstructor.Domain

open System
open Softellect.Sys.Primitives
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.LibraryEntryFiles

/// The kit-folder seam and its shared in-memory double (spec 0050 S4.1).
module KitFolders =

    /// One opened folder's kit metadata; only initialisation makes it a kit (spec 0050 S4.1).
    [<ReferenceEquality>]
    type KitProxy =
        {
            /// Preserves existing contents and adds metadata, data and library, or refuses an existing kit (spec 0050 S4.1).
            initialiseKit : KitFile -> Result<unit, KitError>
            /// Reads the metadata, or returns NotAKitFolderErr without a kit file (spec 0050 S4.1).
            readKitFile : unit -> Result<KitFile, KitError>
            /// Replaces the metadata whole, or returns NotAKitFolderErr without writing (spec 0050 S4.1).
            writeKitFile : KitFile -> Result<unit, KitError>
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

    /// One folder's private contents, shared by every proxy opened over it (spec 0050 S4.1).
    type InMemoryKitFolder private () =
        let kitFilePath = KitFile.fileName.toKitRelativePath
        let mutable kitFileOpt : KitFile option = None
        let mutable texts : Map<KitRelativePath, KitFileText> = Map.empty
        let mutable folders : Set<KitRelativePath> = Set.empty

        /// Creates an existing, empty folder (spec 0050 S4.1).
        static member create () : InMemoryKitFolder = InMemoryKitFolder ()

        /// Places an outside writer's text and its parents, refusing the reserved kit filename (spec 0050 S4.1).
        member _.placeLabFile (path : KitRelativePath) (text : KitFileText) : Result<unit, KitError> =
            if String.Equals(path.value, kitFilePath.value, StringComparison.OrdinalIgnoreCase) then
                Error (InvalidKitPathErr $"in-memory kit folder: '{path.value}' is reserved for kit metadata")
            else
                texts <- Map.add path text texts
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
        member internal _.createKitProxy () : KitProxy =
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
            }

    type KitProxy with

        /// Opens a new proxy over the same folder state as every other proxy naming it (spec 0050 S4.1).
        static member createInMemory (folder : InMemoryKitFolder) : KitProxy = folder.createKitProxy ()
