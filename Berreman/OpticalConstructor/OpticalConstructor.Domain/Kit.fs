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
            let process = this.owner.processId.value.ToString(CultureInfo.InvariantCulture)
            $"{this.owner.user.value} on {this.owner.machine.value} (process {process}) since {instant} UTC"

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
