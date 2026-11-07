namespace OpticalConstructor.Domain

open System
open System.Globalization
open Softellect.Sys.Primitives
open OpticalConstructor.Domain.LibraryEntryFiles

/// A kit's identity, file metadata and access mode (spec 0050 S4.1).
module Kit =

    /// An instant supplied in UTC by the injected clock (spec 0050 S4.1).
    type ClockTime =
        | ClockTime of DateTime

        member this.value : DateTime = let (ClockTime instant) = this in instant

    /// Supplies the current instant; the composition root chooses the clock (spec 0050 S4.1).
    [<ReferenceEquality>]
    type ClockProvider =
        {
            now : unit -> ClockTime
        }

    /// The machine on which a kit was opened (spec 0050 S4.1).
    type MachineName =
        | MachineName of string

        member this.value : string = let (MachineName name) = this in name

    /// The user who opened a kit (spec 0050 S4.1).
    type UserName =
        | UserName of string

        member this.value : string = let (UserName name) = this in name

    /// Identifies the user, machine and process that opened a kit (spec 0050 S4.1).
    type KitLockOwner =
        {
            machine : MachineName
            user : UserName
            processId : ProcessId
        }

    /// Identifies a kit lock's owner and its acquisition instant (spec 0050 S4.1).
    type KitLockHolder =
        {
            owner : KitLockOwner
            acquiredAt : ClockTime
        }

        member this.describe : string =
            let instant = this.acquiredAt.value.ToString("yyyy-MM-dd HH:mm:ss", CultureInfo.InvariantCulture)
            let processText = this.owner.processId.value.ToString(CultureInfo.InvariantCulture)
            $"{this.owner.user.value} on {this.owner.machine.value} (process {processText}) since {instant} UTC"

    /// Why a kit operation failed, with the affected folder, file or name in the reason (spec 0050 S4.1).
    type KitError =
        | KitIoErr of reason : string
        | NotAKitFolderErr of reason : string
        | KitAlreadyExistsErr of reason : string
        | KitFileRejectedErr of reason : string
        | InvalidKitNameErr of reason : string
        | MissingKitFileErr of reason : string
        | InvalidKitPathErr of reason : string
        | KitLockedErr of holder : KitLockHolder
        | KitReadOnlyErr of reason : string

        member this.describe : string =
            match this with
            | KitIoErr reason -> $"a kit file could not be read or written: {reason}"
            | NotAKitFolderErr reason -> $"the folder is not a kit: {reason}"
            | KitAlreadyExistsErr reason -> $"the folder is already a kit: {reason}"
            | KitFileRejectedErr reason -> $"a kit file was refused: {reason}"
            | InvalidKitNameErr reason -> $"the kit name is not valid: {reason}"
            | MissingKitFileErr reason -> $"the kit holds no such file: {reason}"
            | InvalidKitPathErr reason -> $"the path is not inside the kit: {reason}"
            | KitLockedErr holder -> $"the kit is open elsewhere: {holder.describe}"
            | KitReadOnlyErr reason -> $"the kit is open read-only: {reason}"

    /// The display name, preserved as given; its file stem follows the shared name rule (spec 0050 S4.1).
    type KitName =
        | KitName of string

        member this.value : string = let (KitName text) = this in text

        static member tryCreate (text : string) : Result<KitName, KitError> =
            if String.IsNullOrWhiteSpace text then
                Error (InvalidKitNameErr $"'{text}': a kit name must not be blank")
            else
                Ok (KitName text)

        member this.fileStem : FileStem = FileStem.ofText this.value

    /// The author's free text; an empty author is allowed (spec 0050 S4.1).
    type KitAuthor =
        | KitAuthor of string

        member this.value : string = let (KitAuthor text) = this in text

    /// Free-text kit notes, empty when none have been supplied (spec 0050 S4.1).
    type KitNotes =
        | KitNotes of string

        member this.value : string = let (KitNotes text) = this in text

    /// A glob identifying files excluded when sending the kit (spec 0050 S4.1).
    type SendExcludeGlob =
        | SendExcludeGlob of string

        member this.value : string = let (SendExcludeGlob text) = this in text

    /// The kit file's metadata, using the caller's identity and clock instant (spec 0050 S4.1).
    type KitFile =
        {
            schemaVersion : FileSchemaVersion
            id : KitId
            name : KitName
            author : KitAuthor
            created : ClockTime
            modified : ClockTime
            notes : KitNotes
            sendExclude : SendExcludeGlob list
        }

        static member currentSchemaVersion : FileSchemaVersion = FileSchemaVersion 1

        static member extension : FileExtension = FileExtension ".kit-json"

        static member fileName : FileName = (FileStem.ofText "").withExtension KitFile.extension

        static member create (id : KitId) (name : KitName) (author : KitAuthor) (now : ClockTime) : KitFile =
            {
                schemaVersion = KitFile.currentSchemaVersion
                id = id
                name = name
                author = author
                created = now
                modified = now
                notes = KitNotes ""
                sendExclude = []
            }

    /// A folder that is or becomes a kit (spec 0050 S4.1).
    type KitFolder =
        | KitFolder of FolderName

        member this.value : FolderName = let (KitFolder folder) = this in folder

    /// The access mode chosen when opening a kit (spec 0050 S4.1).
    type KitAccess =
        | ReadWriteKitAccess
        | ReadOnlyKitAccess

    /// A path inside a kit, with forward slashes and no rooted or traversal segments (spec 0050 001 §E).
    type KitRelativePath =
        | KitRelativePath of string

        member this.value : string = let (KitRelativePath path) = this in path

        static member kitCachePath : KitRelativePath = KitRelativePath ".oc"

        static member kitLockPath : KitRelativePath = KitRelativePath $"{KitRelativePath.kitCachePath.value}/lock"

        static member tryCreate (text : string) : Result<KitRelativePath, KitError> =
            if String.IsNullOrWhiteSpace text then
                Error (InvalidKitPathErr $"'{text}': a kit-relative path must not be blank")
            else
                let path = text.Replace('\\', '/')
                if path.StartsWith('/') || path.Contains(':') then
                    Error (InvalidKitPathErr $"'{text}': a kit-relative path must not be rooted or contain a colon")
                elif path.Split('/') |> Array.exists (fun segment -> segment = "" || segment = "." || segment = "..") then
                    Error (InvalidKitPathErr $"'{text}': a kit-relative path must not contain empty, '.' or '..' segments")
                else
                    Ok (KitRelativePath path)

    /// The files that occur once per item, setup, experiment, study or fit run, named by extension (spec 0050 001 §A).
    type KitFileKind =
        | ItemKitFile
        | SetupKitFile
        | ExperimentKitFile
        | StudyKitFile
        | FitRunKitFile

        member this.extension : FileExtension =
            match this with
            | ItemKitFile -> FileExtension ".item-json"
            | SetupKitFile -> FileExtension ".setup-json"
            | ExperimentKitFile -> FileExtension ".experiment-json"
            | StudyKitFile -> FileExtension ".study-json"
            | FitRunKitFile -> FileExtension ".fitrun-json"

        member this.fileNameOf (stem : FileStem) : FileName = stem.withExtension this.extension

        static member all : KitFileKind list =
            [ ItemKitFile; SetupKitFile; ExperimentKitFile; StudyKitFile; FitRunKitFile ]

    /// UTF-8 text without a byte-order mark, shared by app-owned kit files and read-only lab files (spec 0050 001 §E).
    type KitFileText =
        | KitFileText of string

        member this.value : string = let (KitFileText text) = this in text

    /// A lab file as read, with the SHA-256 of its bytes as stored (spec 0050 001 §E).
    type KitDataFile =
        {
            path : KitRelativePath
            text : KitFileText
            contentHash : ContentHash
        }

    /// The data folder or one of its subfolders, where lab files are copied (spec 0050 S4.2).
    type KitDataFolder =
        | KitDataFolder of KitRelativePath

        member this.value : KitRelativePath = let (KitDataFolder path) = this in path

        /// The default home for attached lab files (spec 0050 S4.2).
        static member root : KitDataFolder =
            let path = KitRelativePath "data"
            KitDataFolder path

        /// Accepts data and its descendants using ordinal, case-sensitive spelling (spec 0050 S4.2).
        static member tryCreate (path : KitRelativePath) : Result<KitDataFolder, KitError> =
            if path = KitDataFolder.root.value || path.value.StartsWith("data/", StringComparison.Ordinal) then
                Ok (KitDataFolder path)
            else
                Error (InvalidKitPathErr $"'{path.value}': a data folder must be data or a folder inside it")

        /// Keeps the leaf name, then tries numbered suffixes before its extension, ignoring occupied-path case (spec 0050 S4.2).
        member this.copyTargetFor (leaf : FileName) (taken : KitRelativePath list) : Result<KitRelativePath, KitError> =
            let extensionStart = leaf.value.LastIndexOf('.')
            let stem =
                if extensionStart > 0 then leaf.value.Substring(0, extensionStart) else leaf.value
                |> FileStem
            let extension =
                if extensionStart > 0 then leaf.value.Substring(extensionStart) else ""
                |> FileExtension

            let rec tryCandidate (suffix : int) : Result<KitRelativePath, KitError> =
                let candidate =
                    if suffix = 1 then
                        $"{this.value.value}/{leaf.value}"
                    else
                        let number = suffix.ToString(CultureInfo.InvariantCulture)
                        $"{this.value.value}/{stem.value} ({number}){extension.value}"

                match KitRelativePath.tryCreate candidate with
                | Error error -> Error error
                | Ok path ->
                    if taken |> List.exists (fun held -> String.Equals(held.value, path.value, StringComparison.OrdinalIgnoreCase)) then
                        tryCandidate (suffix + 1)
                    else
                        Ok path

            tryCandidate 1

    type KitFolder with

        /// Resolves a file inside this folder to a validated relative path without reading disk (spec 0050 S4.2).
        member this.tryKitPathOf (file : FileName) : KitRelativePath option =
            let folder = this.value.value.Replace('\\', '/').TrimEnd('/')
            let absoluteFile = file.value.Replace('\\', '/')
            let prefix = $"{folder}/"

            if absoluteFile.StartsWith(prefix, StringComparison.OrdinalIgnoreCase) then
                absoluteFile.Substring(prefix.Length) |> KitRelativePath.tryCreate |> Result.toOption
            else
                None

    /// A picked lab file's absolute name and original bytes (spec 0050 S4.2).
    type LabFileSource =
        {
            file : FileName
            content : FileByteContent
        }

    /// Whether attached bytes are unchanged, changed since attach, or missing (spec 0050 S4.2).
    type DataAttachmentStatus =
        | AttachedDataUnchanged
        | AttachedDataChanged of current : ContentHash
        | AttachedDataMissing

    /// A data file's kit-relative path and SHA-256 recorded at attach time (spec 0050 S4.2).
    type DataAttachment =
        {
            path : KitRelativePath
            contentHash : ContentHash
        }

        /// Compares the current file's hash with the recorded hash, or reports absence (spec 0050 S4.2).
        member this.statusAgainst (currentOpt : KitDataFile option) : DataAttachmentStatus =
            match currentOpt with
            | None -> AttachedDataMissing
            | Some file when file.contentHash = this.contentHash -> AttachedDataUnchanged
            | Some file -> AttachedDataChanged file.contentHash

    type KitDataFile with

        /// Records this file's kit-relative path and current hash for an experiment (spec 0050 S4.2).
        member this.attachment : DataAttachment =
            {
                path = this.path
                contentHash = this.contentHash
            }
