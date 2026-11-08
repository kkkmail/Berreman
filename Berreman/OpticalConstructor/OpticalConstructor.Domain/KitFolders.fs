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
            /// Locates an absolute name inside the kit without requiring the file to exist (spec 0050 S4.2).
            tryKitPathOf : FileName -> KitRelativePath option
            /// Copies bytes into data under a free name, creating parents and never replacing a lab file (spec 0050 S4.2).
            copyInFile : KitDataFolder -> FileName -> FileByteContent -> Result<KitDataFile, KitError>
            /// Lists snapshots in ordinal filename order through their schema, version and hash checks (spec 0050 001 §A).
            listSnapshots : unit -> Result<LibraryEntryEnvelope list, KitError>
            /// Writes a version once under library, refusing an existing snapshot without changes (spec 0050 001 §A).
            writeSnapshot : LibraryEntryEnvelope -> Result<unit, KitError>
            /// Replaces this proxy's in-memory record of all current kit-file stamps without writing (spec 0050 S4.1).
            recordStamps : unit -> Result<unit, KitError>
            /// Lists added, changed or removed kit files in ordinal order, excluding this proxy's own writes (spec 0050 S4.1).
            changedKitFiles : unit -> Result<FileName list, KitError>
            /// Takes this proxy's lock at the supplied instant, or identifies the foreign holder (spec 0050 S4.1).
            acquireLock : ClockTime -> Result<KitLockHolder, KitError>
            /// Releases only this proxy's unchanged lock, retaining the cache folder (spec 0050 S4.1).
            releaseLock : unit -> Result<unit, KitError>
        }

    type FileName with

        /// A root filename used as a path within its kit (spec 0050 S4.1).
        member private this.toKitRelativePath : KitRelativePath = KitRelativePath this.value

        member private this.kitStampIdentity : FileName = FileName (this.value.ToUpperInvariant())

    let updateKitFileStamp<'Stamp>
        (file : FileName)
        (stampOpt : 'Stamp option)
        (stamps : Map<FileName, 'Stamp>)
        : Map<FileName, 'Stamp> =
        let others = stamps |> Map.filter (fun name _ -> name.kitStampIdentity <> file.kitStampIdentity)
        match stampOpt with
        | Some stamp -> Map.add file stamp others
        | None -> others

    let changedKitFileNames<'Stamp when 'Stamp : equality>
        (recorded : Map<FileName, 'Stamp>)
        (current : Map<FileName, 'Stamp>)
        : FileName list =
        let previous = recorded |> Map.toList |> List.map (fun (name, stamp) -> name.kitStampIdentity, stamp) |> Map.ofList
        let present = current |> Map.keys |> Seq.map (fun name -> name.kitStampIdentity) |> Set.ofSeq
        let changed =
            current
            |> Map.toList
            |> List.choose (fun (name, stamp) ->
                if Map.tryFind name.kitStampIdentity previous = Some stamp then None else Some name)
        let removed = recorded |> Map.keys |> Seq.filter (fun name -> not (Set.contains name.kitStampIdentity present)) |> Seq.toList
        changed @ removed |> List.sortWith (fun first second -> String.CompareOrdinal(first.value, second.value))

    type KitRelativePath with

        /// Selects metadata and known kind files at the root, preserving their spelling (spec 0050 S4.1).
        member this.tryKitFileName : FileName option =
            if this.value.Contains('/') || this.value.Contains('\\') then
                None
            elif String.Equals(this.value, KitFile.fileName.value, StringComparison.OrdinalIgnoreCase)
                 || (KitFileKind.all |> List.exists (fun kind -> this.value.EndsWith(kind.extension.value, StringComparison.OrdinalIgnoreCase))) then
                Some (FileName this.value)
            else
                None

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
                path.tryKitFileName
                |> Option.bind (fun name ->
                    if name.value.EndsWith(extension.value, StringComparison.OrdinalIgnoreCase) then
                        name.value.Substring(0, name.value.Length - extension.value.Length) |> FileStem.ofText |> Some
                    else
                        None))
            |> List.sortWith (fun first second -> String.CompareOrdinal(first.value, second.value))

    type KitFileText with

        /// Removes only the leading UTF-8 byte-order mark from decoded text (spec 0050 S4.1).
        member this.withoutByteOrderMark : KitFileText =
            if this.value.StartsWith('\uFEFF') then KitFileText (this.value.Substring 1)
            else this

    type private InMemoryKitFileStamp =
        | InMemoryKitMetadataStamp of KitFile
        | InMemoryKitTextStamp of KitFileText

    type FileByteContent with

        /// Decodes UTF-8 text and removes only a leading byte-order mark (spec 0050 S4.1; S4.2).
        member this.kitFileText : KitFileText =
            let text = KitFileText (Encoding.UTF8.GetString this.value)
            text.withoutByteOrderMark

        /// Describes a kit file with the hash of its stored bytes (spec 0050 S4.1; S4.2).
        member this.kitDataFile (path : KitRelativePath) : KitDataFile =
            {
                path = path
                text = this.kitFileText
                contentHash = ContentHash.ofBytes this
            }

    /// One folder's private contents, shared by every proxy opened over it (spec 0050 S4.1).
    type InMemoryKitFolder private () =
        let kitFilePath = KitFile.fileName.toKitRelativePath
        let folderSync = obj ()
        let mutable kitFileOpt : KitFile option = None
        let mutable texts : Map<KitRelativePath, KitFileText> = Map.empty
        let mutable snapshots : Map<FileName, LibraryEntryEnvelope> = Map.empty
        let mutable folders : Set<KitRelativePath> = Set.empty
        let mutable nextProxyProcessId = ProcessId 1

        /// Creates an existing, empty folder (spec 0050 S4.1).
        static member create () : InMemoryKitFolder = InMemoryKitFolder ()

        /// The absolute folder represented by this in-memory store (spec 0050 S4.2).
        member _.kitFolder : KitFolder =
            let folder = FolderName "C:/in-memory-kit"
            KitFolder folder

        member private _.tryFindTextPath (path : KitRelativePath) : KitRelativePath option =
            texts
            |> Map.keys
            |> Seq.tryFind (fun held -> String.Equals(held.value, path.value, StringComparison.OrdinalIgnoreCase))

        member private this.readInMemoryKitDataFile (path : KitRelativePath) : Result<KitDataFile, KitError> =
            match this.tryFindTextPath path |> Option.bind (fun held -> Map.tryFind held texts) with
            | Some text ->
                let bytes = Encoding.UTF8.GetBytes text.value |> FileByteContent
                Ok (bytes.kitDataFile path)
            | None -> Error (MissingKitFileErr $"in-memory kit folder: '{path.value}'")

        /// Places an outside writer's text and its parents, refusing the reserved kit filename (spec 0050 S4.1).
        member this.placeLabFile (path : KitRelativePath) (text : KitFileText) : Result<unit, KitError> =
            if String.Equals(path.value, kitFilePath.value, StringComparison.OrdinalIgnoreCase) then
                Error (InvalidKitPathErr $"in-memory kit folder: '{path.value}' is reserved for kit metadata")
            else
                let heldPath = this.tryFindTextPath path |> Option.defaultValue path
                texts <- Map.add heldPath text texts
                folders <- Set.union folders (Set.ofList path.parentFolders)
                Ok ()

        /// Removes an outside writer's file, refusing metadata and reporting missing paths (spec 0050 S4.2).
        member this.removeLabFile (path : KitRelativePath) : Result<unit, KitError> =
            if String.Equals(path.value, kitFilePath.value, StringComparison.OrdinalIgnoreCase) then
                Error (InvalidKitPathErr $"in-memory kit folder: '{path.value}' is reserved for kit metadata")
            else
                match this.tryFindTextPath path with
                | Some held ->
                    texts <- Map.remove held texts
                    Ok ()
                | None -> Error (MissingKitFileErr $"in-memory kit folder: '{path.value}'")

        /// Lists every file and folder by its kit-relative path in ordinal order (spec 0050 S4.1).
        member _.paths () : KitRelativePath list =
            let filePaths = texts |> Map.keys |> Set.ofSeq
            let snapshotPaths = snapshots |> Map.values |> Seq.map (fun envelope -> envelope.snapshotPath) |> Set.ofSeq
            Set.unionMany [ folders; filePaths; snapshotPaths ]
            |> Set.toList
            |> List.sortWith (fun first second -> String.CompareOrdinal(first.value, second.value))

        /// Opens shared folder state with supplied serializers, retaining stored text for any-file reads (spec 0050 S4.1).
        member this.createKitProxy
            (serializeMetadata : KitFile -> Result<KitFileText, KitError>)
            (serializeLock : KitLockHolder -> Result<KitFileText, KitError>)
            (parseLock : KitFileText -> Result<KitLockHolder, KitError>)
            (readSnapshot : LibraryEntryEnvelope -> Result<LibraryEntryEnvelope, KitError>)
            : KitProxy =
            let owner : KitLockOwner =
                lock folderSync (fun () ->
                    let processId = nextProxyProcessId
                    nextProxyProcessId <- ProcessId (processId.value + 1)
                    {
                        machine = MachineName "in-memory"
                        user = UserName "in-memory"
                        processId = processId
                    })
            let mutable heldLockOpt : KitLockHolder option = None
            let mutable recordedStamps : Map<FileName, InMemoryKitFileStamp> = Map.empty
            let lockPath = KitRelativePath.kitLockPath

            let currentStamps () : Map<FileName, InMemoryKitFileStamp> =
                texts
                |> Map.toList
                |> List.choose (fun (path, text) ->
                    path.tryKitFileName
                    |> Option.bind (fun name ->
                        if path = kitFilePath then
                            kitFileOpt |> Option.map (fun kit -> name, InMemoryKitMetadataStamp kit)
                        else
                            Some (name, InMemoryKitTextStamp text)))
                |> Map.ofList

            let tryReadLock () : KitLockHolder option =
                this.tryFindTextPath lockPath
                |> Option.bind (fun path -> Map.tryFind path texts)
                |> Option.bind (fun text -> parseLock text |> Result.toOption)

            let writeMetadata (kit : KitFile) : Result<unit, KitError> =
                serializeMetadata kit
                |> Result.map (fun text ->
                    texts <- Map.add kitFilePath text texts
                    kitFileOpt <- Some kit
                    let stamp = InMemoryKitMetadataStamp kit
                    recordedStamps <- updateKitFileStamp KitFile.fileName (Some stamp) recordedStamps)

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
                        let stamp = InMemoryKitTextStamp text
                        match heldPath.tryKitFileName with
                        | Some name -> recordedStamps <- updateKitFileStamp name (Some stamp) recordedStamps
                        | None -> ()
                        Ok ()
                deleteKindFile =
                    fun kind stem ->
                        let path = (kind.fileNameOf stem).toKitRelativePath
                        match this.tryFindTextPath path with
                        | Some held ->
                            texts <- Map.remove held texts
                            recordedStamps <- updateKitFileStamp (kind.fileNameOf stem) None recordedStamps
                            Ok ()
                        | None -> Error (MissingKitFileErr $"in-memory kit folder: '{path.value}'")
                readDataFile = this.readInMemoryKitDataFile
                tryKitPathOf = this.kitFolder.tryKitPathOf
                copyInFile =
                    fun destination file bytes ->
                        lock folderSync (fun () ->
                            destination.copyTargetFor file.leafName (this.paths ())
                            |> Result.bind (fun path ->
                                this.placeLabFile path bytes.kitFileText
                                |> Result.bind (fun () -> this.readInMemoryKitDataFile path)))
                listSnapshots =
                    fun () ->
                        lock folderSync (fun () ->
                            snapshots
                            |> Map.toList
                            |> List.sortWith (fun (first, _) (second, _) -> String.CompareOrdinal(first.value, second.value))
                            |> List.fold (fun result (_, envelope) ->
                                result |> Result.bind (fun entries ->
                                    readSnapshot envelope |> Result.map (fun entry -> entry :: entries))) (Ok [])
                            |> Result.map List.rev)
                writeSnapshot =
                    fun envelope ->
                        lock folderSync (fun () ->
                            let name = envelope.snapshotFileName
                            if Map.containsKey name snapshots then
                                Error (SnapshotExistsErr $"in-memory kit folder: '{envelope.snapshotPath.value}'")
                            else
                                snapshots <- Map.add name envelope snapshots
                                let library = KitRelativePath "library"
                                folders <- Set.add library folders
                                Ok ())
                recordStamps =
                    fun () ->
                        recordedStamps <- currentStamps ()
                        Ok ()
                changedKitFiles =
                    fun () -> currentStamps () |> changedKitFileNames recordedStamps |> Ok
                acquireLock =
                    fun time ->
                        lock folderSync (fun () ->
                            match tryReadLock () with
                            | Some stored ->
                                match heldLockOpt with
                                | Some held when stored = held -> Ok held
                                | _ -> Error (KitLockedErr stored)
                            | None ->
                                let holder =
                                    {
                                        owner = owner
                                        acquiredAt = time
                                    }
                                serializeLock holder
                                |> Result.map (fun text ->
                                    let heldPath = this.tryFindTextPath lockPath |> Option.defaultValue lockPath
                                    texts <- Map.add heldPath text texts
                                    folders <- Set.add KitRelativePath.kitCachePath folders
                                    heldLockOpt <- Some holder
                                    holder))
                releaseLock =
                    fun () ->
                        lock folderSync (fun () ->
                            match heldLockOpt with
                            | Some held ->
                                match tryReadLock () with
                                | Some stored when stored = held ->
                                    let heldPath = this.tryFindTextPath lockPath |> Option.defaultValue lockPath
                                    texts <- Map.remove heldPath texts
                                | _ -> ()
                                heldLockOpt <- None
                            | None -> ()
                            Ok ())
            }

    type KitProxy with

        /// Refuses every write, passes reads through and leaves lock ownership untouched (spec 0050 S4.1).
        member this.readOnlyView : KitProxy =
            {
                initialiseKit = fun _ -> Error (KitReadOnlyErr "initialiseKit")
                readKitFile = this.readKitFile
                writeKitFile = fun _ -> Error (KitReadOnlyErr "writeKitFile")
                listKindFiles = this.listKindFiles
                readKindFile = this.readKindFile
                writeKindFile = fun _ _ _ -> Error (KitReadOnlyErr "writeKindFile")
                deleteKindFile = fun _ _ -> Error (KitReadOnlyErr "deleteKindFile")
                readDataFile = this.readDataFile
                tryKitPathOf = this.tryKitPathOf
                copyInFile = fun _ _ _ -> Error (KitReadOnlyErr "copyInFile")
                listSnapshots = this.listSnapshots
                writeSnapshot = fun _ -> Error (KitReadOnlyErr "writeSnapshot")
                recordStamps = this.recordStamps
                changedKitFiles = this.changedKitFiles
                acquireLock = fun _ -> Error (KitReadOnlyErr "acquireLock")
                releaseLock = fun () -> Ok ()
            }

        /// Records an in-kit file in place or copies an outside file into data, retaining its relative path and stored hash (spec 0050 S4.2).
        member this.attachLabFile (target : KitDataFolder) (source : LabFileSource) : Result<DataAttachment, KitError> =
            let data =
                match this.tryKitPathOf source.file with
                | Some path -> this.readDataFile path
                | None -> this.copyInFile target source.file.leafName source.content
            data |> Result.map (fun file -> file.attachment)

        /// Reads the current attachment without writing; only a missing file becomes absence, and other errors pass through (spec 0050 S4.2).
        member this.attachmentStatus (attachment : DataAttachment) : Result<DataAttachmentStatus, KitError> =
            match this.readDataFile attachment.path with
            | Ok file -> Ok (attachment.statusAgainst (Some file))
            | Error (MissingKitFileErr _) -> Ok (attachment.statusAgainst None)
            | Error error -> Error error

    /// An opened kit's metadata, access-controlled proxy and injected clock (spec 0050 S4.1).
    type KitContext =
        {
            proxy : KitProxy
            kitFile : KitFile
            access : KitAccess
            clock : ClockProvider
        }

        /// Reads metadata, locks read-write access and records stamps; stamp failure releases the acquired lock (spec 0050 S4.1).
        static member openKit (proxy : KitProxy) (access : KitAccess) (clock : ClockProvider) : Result<KitContext, KitError> =
            let kitProxy =
                match access with
                | ReadOnlyKitAccess -> proxy.readOnlyView
                | ReadWriteKitAccess -> proxy
            kitProxy.readKitFile ()
            |> Result.bind (fun kitFile ->
                let lockResult =
                    match access with
                    | ReadWriteKitAccess -> kitProxy.acquireLock (clock.now ()) |> Result.map ignore
                    | ReadOnlyKitAccess -> Ok ()
                lockResult
                |> Result.bind (fun () ->
                    match kitProxy.recordStamps () with
                    | Ok () ->
                        Ok
                            {
                                proxy = kitProxy
                                kitFile = kitFile
                                access = access
                                clock = clock
                            }
                    | Error error ->
                        match access with
                        | ReadWriteKitAccess -> kitProxy.releaseLock () |> ignore
                        | ReadOnlyKitAccess -> ()
                        Error error))

        /// Releases this context's lock; read-only contexts leave locks untouched (spec 0050 S4.1).
        member this.close () : Result<unit, KitError> = this.proxy.releaseLock ()

        /// Stamps modified from the clock and returns the saved context, or the proxy's write error (spec 0050 S4.1).
        member this.saveKitFile (kitFile : KitFile) : Result<KitContext, KitError> =
            let savedKitFile =
                {
                    kitFile with modified = this.clock.now ()
                }
            this.proxy.writeKitFile savedKitFile
            |> Result.map (fun () ->
                {
                    this with kitFile = savedKitFile
                })

    /// Asks for a kit folder asynchronously, returning None when the user cancels (spec 0050 S4.3).
    [<ReferenceEquality>]
    type KitFolderPicker =
        {
            pickKitFolder : unit -> Async<KitFolder option>
        }

    type KitContext with

        /// Opens read-write, initializing only a folder without a kit; all other errors pass through (spec 0050 S4.3).
        static member openOrInitialise
            (proxy : KitProxy)
            (clock : ClockProvider)
            (newKitFile : ClockTime -> Result<KitFile, KitError>)
            : Result<KitContext, KitError> =
            match KitContext.openKit proxy ReadWriteKitAccess clock with
            | Error (NotAKitFolderErr _) ->
                newKitFile (clock.now ())
                |> Result.bind proxy.initialiseKit
                |> Result.bind (fun () -> KitContext.openKit proxy ReadWriteKitAccess clock)
            | result -> result

