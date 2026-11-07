namespace OpticalConstructor.Storage

open System
open System.Diagnostics
open System.IO
open System.Text
open System.Text.Json
open System.Threading
open Softellect.Sys.Primitives
open Softellect.Sys.Retry
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.KitFolders
open OpticalConstructor.Storage.Errors
open OpticalConstructor.Storage.CanonicalJson
open OpticalConstructor.Storage.CatalogueFolderStore

module KitFolderStore =

    type KitLockGuardError =
        | KitGuardIoExn of file : FileName * cause : exn

        member this.toKitError : KitError =
            match this with
            | KitGuardIoExn (file, cause) ->
                KitIoErr $"kit guard '{file.value}': {cause.GetType().Name} (HRESULT 0x{uint32 cause.HResult:X8}): {cause.Message}"

    /// The guard's IO seam; tests substitute failures without changing ownership logic (0050 .manual/009).
    type KitLockGuardProxy =
        {
            tryOpenGuard : unit -> Result<FileStream, KitLockGuardError>
            waitForGuardRetry : unit -> Result<unit, KitLockGuardError>
        }

        /// Opens exclusively with at most 100 retries; permanent failures retain their diagnostics (0050 .manual/009).
        member this.acquireGuard () : Result<FileStream, KitError> =
            let rec tryAcquire (remaining : RetryLimit) : Result<FileStream, KitError> =
                match this.tryOpenGuard () with
                | Ok stream -> Ok stream
                | Error (KitGuardIoExn (_, cause) as failure) ->
                    let retryable =
                        match cause with
                        | :? UnauthorizedAccessException -> (cause.HResult &&& 0xFFFF) = 5
                        | :? IOException -> (cause.HResult &&& 0xFFFF) = 32 || (cause.HResult &&& 0xFFFF) = 33
                        | _ -> false
                    if retryable && remaining.value > 0 then
                        this.waitForGuardRetry ()
                        |> Result.mapError (fun error -> error.toKitError)
                        |> Result.bind (fun () ->
                            let next = RetryLimit (remaining.value - 1)
                            tryAcquire next)
                    else
                        Error failure.toKitError
            let retries = RetryLimit 100
            tryAcquire retries

        /// Binds a delete-on-close file guard and its ten-millisecond retry delay (0050 .manual/009).
        static member create (file : FileName) : KitLockGuardProxy =
            let tryGuardIo (operation : unit -> 'Value) : Result<'Value, KitLockGuardError> =
                try Ok (operation ())
                with error -> Error (KitGuardIoExn (file, error))
            let delay = RetryWait 10<millisecond>
            {
                tryOpenGuard =
                    fun () ->
                        tryGuardIo (fun () ->
                            new FileStream(file.value, FileMode.OpenOrCreate, FileAccess.ReadWrite, FileShare.None, 1, FileOptions.DeleteOnClose))
                waitForGuardRetry = fun () -> tryGuardIo (fun () -> Thread.Sleep(int delay.value))
            }

    type KitLockHolder with

        /// Canonical cache text for the holder, without a versioned schema (spec 0050 S4.1).
        member this.lockFileText : Result<KitFileText, StorageError> =
            nodeOf this |> Result.map (fun node -> node.canonicalFileText |> KitFileText)

        member private this.tryFindLiveKitLockOn (machine : MachineName) : KitLockHolder option =
            if not (String.Equals(this.owner.machine.value, machine.value, StringComparison.OrdinalIgnoreCase)) then
                Some this
            else
                try
                    use ownerProcess = Process.GetProcessById this.owner.processId.value
                    Some this
                with :? ArgumentException -> None

    type KitFile with

        /// Canonical kit metadata with LF line ends and one final newline (spec 0050 S4.1).
        member this.fileText : Result<KitFileText, StorageError> =
            nodeOf this |> Result.map (fun node -> node.canonicalFileText |> KitFileText)

    type KitFileText with

        member private this.toJsonText : JsonText = JsonText this.value

        /// Binds cache text through the shared JSON options, reporting invalid text as MalformedJsonErr (spec 0050 S4.1).
        member this.parseLockHolder : Result<KitLockHolder, StorageError> =
            try
                let holder = JsonSerializer.Deserialize<KitLockHolder>(this.withoutByteOrderMark.value, options)
                if Unchecked.equals holder Unchecked.defaultof<KitLockHolder>
                   || Unchecked.equals holder.owner Unchecked.defaultof<KitLockOwner> then
                    Error (MalformedJsonErr "a kit lock must contain a holder and its owner")
                else
                    Ok holder
            with error -> Error (MalformedJsonErr error.Message)

        /// Validates the kit schema and rejects unsupported versions before binding (spec 0050 S4.1).
        member this.parseKitFile : Result<KitFile, StorageError> =
            parseVersionedText SchemaValidation.validateKitFile KitFile.extension KitFile.currentSchemaVersion this.toJsonText

    let private tryKitFolderIo (folder : KitFolder) (io : unit -> Result<'Value, KitError>) : Result<'Value, KitError> =
        try io ()
        with error -> Error (KitIoErr $"kit folder '{folder.value.value}': {error.Message}")

    let private kitFileRejectedErr (folder : FolderName) (error : StorageError) : KitError =
        KitFileRejectedErr $"kit folder '{folder.value}': the file '{KitFile.fileName.value}' is rejected: {error.describe}"

    let private serializeKitMetadata (folder : FolderName) (kitFile : KitFile) : Result<KitFileText, KitError> =
        kitFile.fileText |> Result.mapError (kitFileRejectedErr folder)

    let private kitLockIoErr (folder : FolderName) (error : StorageError) : KitError =
        KitIoErr $"kit folder '{folder.value}': '{KitRelativePath.kitLockPath.value}': {error.describe}"

    let private serializeKitLock (folder : FolderName) (holder : KitLockHolder) : Result<KitFileText, KitError> =
        holder.lockFileText |> Result.mapError (kitLockIoErr folder)

    type FileByteContent with

        member private this.kitFileText : KitFileText =
            let text = KitFileText (Encoding.UTF8.GetString this.value)
            text.withoutByteOrderMark

    type FileName with

        member private this.readKitText () : KitFileText =
            let bytes = File.ReadAllBytes this.value |> FileByteContent
            bytes.kitFileText

        member private this.tryReadKitLock () : Result<KitLockHolder, StorageError> option =
            try
                // A reader must not block another opener's atomic move or release (spec 0050 step 0129).
                use stream = new FileStream(this.value, FileMode.Open, FileAccess.Read, FileShare.Read ||| FileShare.Delete)
                use reader = new StreamReader(stream, Encoding.UTF8, true)
                let text = KitFileText (reader.ReadToEnd ())
                Some text.parseLockHolder
            with
            | :? FileNotFoundException
            | :? DirectoryNotFoundException -> None

    type KitProxy with

        /// Opens shared in-memory folder state with the same metadata serialization as the filesystem proxy (spec 0050 S4.1).
        static member createInMemory (folder : InMemoryKitFolder) : KitProxy =
            let folderName = FolderName "in-memory kit folder"
            folder.createKitProxy
                (serializeKitMetadata folderName)
                (serializeKitLock folderName)
                (fun text -> text.parseLockHolder |> Result.mapError (kitLockIoErr folderName))

        /// Opens a folder without writing; initialisation adds the kit and the first lock adds its cache (spec 0050 S4.1).
        static member create (folder : KitFolder) : KitProxy =
            let owner : KitLockOwner =
                {
                    machine = MachineName Environment.MachineName
                    user = UserName Environment.UserName
                    processId = ProcessId.getCurrentProcessId ()
                }
            let mutable heldLockOpt : KitLockHolder option = None
            let mutable recordedStamps : Map<FileName, ContentHash> = Map.empty
            let proxySync = obj ()
            let cacheFolder = FolderName (Path.Combine(folder.value.value, KitRelativePath.kitCachePath.value))
            let lockFile = FileName (Path.Combine(folder.value.value, KitRelativePath.kitLockPath.value.Replace('/', Path.DirectorySeparatorChar)))
            let guardName = FileName "lock-guard.tmp"
            let guardFile = guardName.combine cacheFolder
            let guardProxy = KitLockGuardProxy.create guardFile

            let rootFilePaths () : KitRelativePath list =
                Directory.GetFiles(folder.value.value, "*", SearchOption.TopDirectoryOnly)
                |> Array.map (fun path -> Path.GetFileName path |> KitRelativePath)
                |> Array.toList

            let currentStamps () : Map<FileName, ContentHash> =
                rootFilePaths ()
                |> List.choose (fun path -> path.tryKitFileName)
                |> List.map (fun name ->
                    let file = name.combine folder.value
                    let bytes = File.ReadAllBytes file.value |> FileByteContent
                    name, ContentHash.ofBytes bytes)
                |> Map.ofList

            let tryExistingLock () : Result<KitLockHolder, KitError> option =
                match lockFile.tryReadKitLock (), heldLockOpt with
                | Some (Ok stored), Some held when stored = held -> Some (Ok held)
                | Some (Ok stored), _ ->
                    stored.tryFindLiveKitLockOn owner.machine
                    |> Option.map (fun holder -> Error (KitLockedErr holder))
                | _ -> None

            let writeText (file : FileName) (text : KitFileText) : Result<unit, KitError> =
                let bytes = Encoding.UTF8.GetBytes text.value |> FileByteContent
                file.writeBytesAtomically bytes
                |> Result.mapError (fun (error : StorageError) ->
                    KitIoErr $"kit folder '{folder.value.value}': the file '{file.value}' is not written: {error.describe}")
                |> Result.map (fun () ->
                    // Stamp the bytes we wrote; a later outside replacement must still be reported (spec 0050 step 0130).
                    let stamp = ContentHash.ofBytes bytes
                    recordedStamps <- updateKitFileStamp file.leafName (Some stamp) recordedStamps)

            let writeMetadata (file : FileName) (kitFile : KitFile) : Result<unit, KitError> =
                serializeKitMetadata folder.value kitFile
                |> Result.bind (writeText file)

            {
                initialiseKit =
                    fun (kitFile : KitFile) ->
                        tryKitFolderIo folder (fun () ->
                            let file = KitFile.fileName.combine folder.value
                            if File.Exists file.value then
                                Error (KitAlreadyExistsErr $"kit folder '{folder.value.value}'")
                            else
                                Directory.CreateDirectory folder.value.value |> ignore
                                for name in [ FolderName "data"; FolderName "library" ] do
                                    let contentFolder = folder.value.combine name
                                    Directory.CreateDirectory contentFolder.value |> ignore
                                // The metadata makes this a kit only after its content folders exist (spec 0050 001 §E; S4.1).
                                writeMetadata file kitFile)
                readKitFile =
                    fun () ->
                        tryKitFolderIo folder (fun () ->
                            let file = KitFile.fileName.combine folder.value
                            if File.Exists file.value then
                                (file.readKitText ()).parseKitFile |> Result.mapError (kitFileRejectedErr folder.value)
                            else
                                Error (NotAKitFolderErr $"kit folder '{folder.value.value}'"))
                writeKitFile =
                    fun (kitFile : KitFile) ->
                        tryKitFolderIo folder (fun () ->
                            let file = KitFile.fileName.combine folder.value
                            if File.Exists file.value then writeMetadata file kitFile
                            else Error (NotAKitFolderErr $"kit folder '{folder.value.value}'"))
                listKindFiles =
                    fun (kind : KitFileKind) ->
                        tryKitFolderIo folder (fun () ->
                            rootFilePaths ()
                            |> kind.stemsAtKitRoot
                            |> Ok)
                readKindFile =
                    fun (kind : KitFileKind) (stem : FileStem) ->
                        tryKitFolderIo folder (fun () ->
                            let file = (kind.fileNameOf stem).combine folder.value
                            if File.Exists file.value then
                                Ok (file.readKitText ())
                            else
                                Error (MissingKitFileErr $"kit folder '{folder.value.value}': '{file.value}'"))
                writeKindFile =
                    fun (kind : KitFileKind) (stem : FileStem) (text : KitFileText) ->
                        tryKitFolderIo folder (fun () ->
                            let file = (kind.fileNameOf stem).combine folder.value
                            writeText file text)
                deleteKindFile =
                    fun (kind : KitFileKind) (stem : FileStem) ->
                        tryKitFolderIo folder (fun () ->
                            let file = (kind.fileNameOf stem).combine folder.value
                            if File.Exists file.value then
                                File.Delete file.value
                                recordedStamps <- updateKitFileStamp file.leafName None recordedStamps
                                Ok ()
                            else
                                Error (MissingKitFileErr $"kit folder '{folder.value.value}': '{file.value}'"))
                readDataFile =
                    fun (path : KitRelativePath) ->
                        tryKitFolderIo folder (fun () ->
                            let file = FileName (Path.Combine(folder.value.value, path.value.Replace('/', Path.DirectorySeparatorChar)))
                            try
                                let bytes = File.ReadAllBytes file.value |> FileByteContent
                                Ok
                                    {
                                        path = path
                                        text = bytes.kitFileText
                                        contentHash = ContentHash.ofBytes bytes
                                    }
                            with
                            | :? FileNotFoundException
                            | :? DirectoryNotFoundException ->
                                Error (MissingKitFileErr $"kit folder '{folder.value.value}': '{path.value}'"))
                recordStamps =
                    fun () ->
                        tryKitFolderIo folder (fun () ->
                            recordedStamps <- currentStamps ()
                            Ok ())
                changedKitFiles =
                    fun () ->
                        tryKitFolderIo folder (fun () ->
                            currentStamps () |> changedKitFileNames recordedStamps |> Ok)
                acquireLock =
                    fun time ->
                        lock proxySync (fun () ->
                            tryKitFolderIo folder (fun () ->
                                match tryExistingLock () with
                                | Some result -> result
                                | None ->
                                    let holder : KitLockHolder =
                                        {
                                            owner = owner
                                            acquiredAt = time
                                        }
                                    serializeKitLock folder.value holder
                                    |> Result.bind (fun text ->
                                        Directory.CreateDirectory cacheFolder.value |> ignore
                                        // Serialize stale removal with acquisition and release across openers (spec 0050 step 0129).
                                        guardProxy.acquireGuard ()
                                        |> Result.bind (fun guard ->
                                            use heldGuard = guard
                                            match tryExistingLock () with
                                            | Some result -> result
                                            | None ->
                                                let temporaryName = FileName $"lock-{Guid.NewGuid():N}.tmp"
                                                let temporaryFile = temporaryName.combine cacheFolder
                                                try
                                                    let bytes = Encoding.UTF8.GetBytes text.value |> FileByteContent
                                                    File.WriteAllBytes(temporaryFile.value, bytes.value)
                                                    File.Delete lockFile.value
                                                    try
                                                        File.Move(temporaryFile.value, lockFile.value, false)
                                                        heldLockOpt <- Some holder
                                                        Ok holder
                                                    with :? IOException as error ->
                                                        File.Delete temporaryFile.value
                                                        match lockFile.tryReadKitLock () with
                                                        | Some (Ok winner) -> Error (KitLockedErr winner)
                                                        | Some (Error invalid) -> Error (kitLockIoErr folder.value invalid)
                                                        | None -> Error (KitIoErr $"kit folder '{folder.value.value}': '{lockFile.value}': {error.Message}")
                                                finally
                                                    File.Delete temporaryFile.value))))
                releaseLock =
                    fun () ->
                        lock proxySync (fun () ->
                            tryKitFolderIo folder (fun () ->
                                let released =
                                    match heldLockOpt with
                                    | Some held ->
                                        match lockFile.tryReadKitLock () with
                                        | Some (Ok stored) when stored = held ->
                                            guardProxy.acquireGuard ()
                                            |> Result.map (fun guard ->
                                                use heldGuard = guard
                                                match lockFile.tryReadKitLock () with
                                                | Some (Ok current) when current = held -> File.Delete lockFile.value
                                                | _ -> ())
                                        | _ -> Ok ()
                                    | None -> Ok ()
                                released |> Result.map (fun () -> heldLockOpt <- None)))
            }
