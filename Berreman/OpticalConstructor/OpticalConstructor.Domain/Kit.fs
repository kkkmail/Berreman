namespace OpticalConstructor.Domain

open System
open System.Globalization
open Softellect.Sys.Primitives
open OpticalConstructor.Domain.Placement
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.ItemModels
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
        | SnapshotExistsErr of reason : string

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
            | SnapshotExistsErr reason -> $"the kit already holds this library snapshot: {reason}"

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

    /// Whether a kit path is the app's cache folder or lies under it, which is never sent (spec 0050 S6; 001 §A).
    type KitPathRole =
        | KitAppCachePath
        | KitContentPath

    /// A path inside a kit, with forward slashes and no rooted or traversal segments (spec 0050 001 §E).
    type KitRelativePath =
        | KitRelativePath of string

        member this.value : string = let (KitRelativePath path) = this in path

        /// The app-owned cache and lock folder, safe to delete and never sent (spec 0050 S6; 001 §A).
        static member appCacheFolder : KitRelativePath = KitRelativePath ".oc"

        static member kitLockPath : KitRelativePath = KitRelativePath $"{KitRelativePath.appCacheFolder.value}/lock"

        /// The app's cache when the first segment names its folder, ignoring case (spec 0050 S6).
        member this.pathRole : KitPathRole =
            let firstSegment = this.value.Split('/') |> Array.head
            if String.Equals(firstSegment, KitRelativePath.appCacheFolder.value, StringComparison.OrdinalIgnoreCase) then
                KitAppCachePath
            else
                KitContentPath

        /// The folder holding this path, or None at the kit root (spec 0050 S6).
        member this.parentOpt : KitRelativePath option =
            match this.value.LastIndexOf('/') with
            | separator when separator < 0 -> None
            | separator -> Some (KitRelativePath (this.value.Substring(0, separator)))

        /// The last segment, the whole path at the kit root (spec 0050 S6).
        member this.leafName : FileName = FileName (this.value.Substring(this.value.LastIndexOf('/') + 1))

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

    type LibraryEntryEnvelope with

        /// Names a snapshot by its entry id, version and kind extension (spec 0050 001 §A).
        member this.snapshotFileName : FileName =
            let version = this.content.version.value.ToString(CultureInfo.InvariantCulture)
            FileName $"{this.content.id.value:D}@v{version}{this.content.schema.kind.extension.value}"

        /// Places the immutable entry snapshot under the kit's library folder (spec 0050 001 §A).
        member this.snapshotPath : KitRelativePath = KitRelativePath $"library/{this.snapshotFileName.value}"

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

        /// Creates metadata named for this folder's leaf, refusing a blank leaf without accessing disk (spec 0050 S4.3).
        member this.namedKitFile (id : KitId) (author : KitAuthor) (now : ClockTime) : Result<KitFile, KitError> =
            let folder = this.value.value.Replace('\\', '/').TrimEnd('/')
            let leaf =
                if folder.Length = 2 && Char.IsLetter folder[0] && folder[1] = ':' then ""
                else folder.Substring(folder.LastIndexOf('/') + 1)
            KitName.tryCreate leaf |> Result.map (fun name -> KitFile.create id name author now)

        /// Resolves a file inside this folder to a validated relative path without reading disk (spec 0050 S4.2).
        member this.tryKitPathOf (file : FileName) : KitRelativePath option =
            let folder = this.value.value.Replace('\\', '/').TrimEnd('/')
            let absoluteFile = file.value.Replace('\\', '/')
            let prefix = $"{folder}/"

            if absoluteFile.StartsWith(prefix, StringComparison.OrdinalIgnoreCase) then
                absoluteFile.Substring(prefix.Length) |> KitRelativePath.tryCreate |> Result.toOption
            else
                None

        /// The folder's one spelling, `/`-separated, without a trailing `/` and upper-cased, touching no disk: a key that
        /// names a kit, never a path to show or open (spec 0050 S5.3).
        member this.canonical : KitFolder =
            let folder = this.value.value.Replace('\\', '/').TrimEnd('/').ToUpperInvariant() |> FolderName
            KitFolder folder

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

    /// Why an item, its label, its model binding or one of its parameters was refused, named in the reason
    /// (spec 0050 S7.1).
    type ItemError =
        | InvalidItemLabelErr of reason : string
        | DuplicateItemLabelErr of reason : string
        | DuplicateItemIdErr of reason : string
        | UnknownItemParameterErr of reason : string
        | InvalidParameterStateErr of reason : string
        | InvalidItemModelBindingErr of reason : string
        | UnknownKitItemErr of reason : string

        member this.describe : string =
            match this with
            | InvalidItemLabelErr reason -> $"the item label is not valid: {reason}"
            | DuplicateItemLabelErr reason -> $"the kit already has an item of that label: {reason}"
            | DuplicateItemIdErr reason -> $"the kit already has an item of that id: {reason}"
            | UnknownItemParameterErr reason -> $"the item has no such parameter: {reason}"
            | InvalidParameterStateErr reason -> $"the parameter state is not valid: {reason}"
            | InvalidItemModelBindingErr reason -> $"the item model binding is not valid: {reason}"
            | UnknownKitItemErr reason -> $"the kit has no item of that id: {reason}"

    /// One physical piece's frozen identity, minted by the caller as a kit's is (spec 0050 S7.1; 001 §I).
    type ItemId =
        | ItemId of Guid

        member this.value : Guid = let (ItemId g) = this in g

    /// The lab's name for a piece (`LP1`, `CPL2`, `QZ`), kept as given; its `<label>.item-json` stem follows the
    /// shared name rule (spec 0050 S7.1; 001 §A).
    type ItemLabel =
        | ItemLabel of string

        member this.value : string = let (ItemLabel text) = this in text

        static member tryCreate (text : string) : Result<ItemLabel, ItemError> =
            if String.IsNullOrWhiteSpace text then
                Error (InvalidItemLabelErr $"'{text}': an item label must not be blank")
            else
                Ok (ItemLabel text)

        member this.fileStem : FileStem = FileStem.ofText this.value

    /// Free-text item notes, empty when there are none (spec 0050 S7.1).
    type ItemNotes =
        | ItemNotes of string

        member this.value : string = let (ItemNotes text) = this in text

    /// The revision an item file carries, counting up from one (spec 0050 S7.1; 001 §I).
    type ItemRevision =
        | ItemRevision of int

        member this.value : int = let (ItemRevision revision) = this in revision

        static member first : ItemRevision = ItemRevision 1

        member this.next : ItemRevision = ItemRevision (this.value + 1)

    /// The exact item-model version an item instantiates, with the preset or sample its binding form takes its physics
    /// from; a standalone model binds nothing (spec 0050 S7.1; 001 §I).
    type ItemModelInstance =
        | PresetInstance of model : ItemModelVersionId * preset : ElementValueId
        | SampleInstance of model : ItemModelVersionId * sample : SampleVersionId
        | ItemModelVersionInstance of ItemModelVersionId

        /// The model version the kit snapshots and pins, in every case (spec 0050 S7.1).
        member this.entryReferenceOpt : EntryReference option =
            match this with
            | PresetInstance (model, _)
            | SampleInstance (model, _)
            | ItemModelVersionInstance model -> Some model.entryReference

        /// The bound physics by step 137's rules: a preset's GUID at its latest, a sample's exact version; `None` for a
        /// standalone model or a preset id that is no GUID, which AddItem refuses (spec 0050 S4.2, S7.1).
        member this.bindingReferenceOpt : EntryReference option =
            match this with
            | PresetInstance (_, preset) -> preset.tryToLibraryEntryId |> Option.map EntryLatestReference
            | SampleInstance (_, sample) ->
                let reference : LibraryVersionRef =
                    {
                        entryId = sample.sampleId.toLibraryEntryId
                        version = sample.version
                    }
                Some (EntryVersionReference reference)
            | ItemModelVersionInstance _ -> None

    /// The bounds a fit searches a parameter within, in its kind's unit (spec 0050 S7.1; 001 §I).
    type ParameterInterval =
        {
            lower : ParameterValue
            upper : ParameterValue
        }

    /// One standard deviation in the parameter's unit (spec 0050 S7.1; 001 §I).
    type ParameterSigma =
        | ParameterSigma of double

        member this.value : double = let (ParameterSigma sigma) = this in sigma

    /// A Gaussian prior on an unknown parameter (spec 0050 S7.1; 001 §I).
    type ParameterPrior =
        {
            centre : ParameterValue
            sigma : ParameterSigma
        }

    /// Where a fit starts an unknown parameter, the bounds it searches and its optional prior (spec 0050 S7.1).
    type UnknownParameterData =
        {
            start : ParameterValue
            bounds : ParameterInterval
            priorOpt : ParameterPrior option
        }

    /// The fit run a calibration came from, named by its file stem `<study>@<stamp>` (spec 0050 S7.1; 001 §A).
    type FitRunName =
        | FitRunName of string

        member this.value : string = let (FitRunName name) = this in name

    /// A value a fit run determined, with its standard deviation (spec 0050 S7.1; 001 §I).
    type CalibratedParameterData =
        {
            value : ParameterValue
            sigma : ParameterSigma
            fitRun : FitRunName
        }

    /// Whether a parameter is known, left for a fit to find, or calibrated by one (spec 0050 S7.1; .spec-md §3.17).
    type ParameterState =
        | KnownParameter of ParameterValue
        | UnknownParameter of UnknownParameterData
        | CalibratedParameter of CalibratedParameterData

    /// One of an item's parameters: its model axis's name and its state (spec 0050 S7.1; 001 §I).
    type Parameter =
        {
            name : ParameterName
            state : ParameterState
        }

        /// The one way an item's parameters are made from a model's axes (spec 0050 S7.1).
        static member knownAtDefault (axis : ItemParameterAxis) : Parameter =
            {
                name = axis.name
                state = KnownParameter axis.defaultValue
            }

        /// The parameter when its numbers are finite, an unknown's bounds are ordered and hold its start, and every
        /// sigma is above zero; a kind's range is its model's, not checked here (spec 0050 S7.1, S8.1).
        member this.validate : Result<Parameter, ItemError> =
            let render (number : double) : string = number.ToString("R", CultureInfo.InvariantCulture)

            let finite (role : string) (number : double) : string option =
                if Double.IsFinite number then None else Some $"{role} {render number} is not finite"

            let positive (role : string) (sigma : ParameterSigma) : string option =
                if sigma.value > 0.0 then None else Some $"{role} {render sigma.value} is not above zero"

            // Finiteness leads each list, so a NaN is reported as itself rather than as a misplaced bound.
            let brokenRules : string option list =
                match this.state with
                | KnownParameter known -> [ finite "the known value" known.value ]
                | UnknownParameter unknown ->
                    let start = unknown.start.value
                    let lower = unknown.bounds.lower.value
                    let upper = unknown.bounds.upper.value

                    let priorRules : string option list =
                        match unknown.priorOpt with
                        | None -> []
                        | Some prior ->
                            [
                                finite "the prior centre" prior.centre.value
                                finite "the prior sigma" prior.sigma.value
                                positive "the prior sigma" prior.sigma
                            ]

                    [
                        finite "the start" start
                        finite "the lower bound" lower
                        finite "the upper bound" upper
                        if lower < upper then None
                        else Some $"the lower bound {render lower} is not below the upper bound {render upper}"
                        if lower <= start && start <= upper then None
                        else Some $"the start {render start} lies outside [{render lower}, {render upper}]"
                    ]
                    @ priorRules
                | CalibratedParameter calibrated ->
                    [
                        finite "the calibrated value" calibrated.value.value
                        finite "the calibration sigma" calibrated.sigma.value
                        positive "the calibration sigma" calibrated.sigma
                    ]

            match brokenRules |> List.choose id with
            | [] -> Ok this
            | rule :: _ -> Error (InvalidParameterStateErr $"{this.name.value}: {rule}")
