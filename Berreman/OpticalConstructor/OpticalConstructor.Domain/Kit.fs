namespace OpticalConstructor.Domain

open System
open System.Globalization
open Softellect.Sys.Primitives
open Berreman.Geometry
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

    /// The name a kit's import profile is stored and picked under (spec 0050 S9.1; S9.2 keeps the profiles in `.kit-json`).
    type ImportProfileName =
        | ImportProfileName of string

        member this.value : string = let (ImportProfileName text) = this in text

        /// The text as given; an empty or white-space text names no profile (spec 0050 S9.1).
        static member tryCreate (text : string) : ImportProfileName option =
            if String.IsNullOrWhiteSpace text then None else Some (ImportProfileName text)

        /// The built-in profile over `DataFileText.parseIntensitySeries`, an X,Y intensity file (spec 0050 S9.1).
        static member intensitySeries : ImportProfileName = ImportProfileName "Intensity series"

        /// The built-in profile over `DataFileText.parseEllipsometricSeries`, a λ, Ψ, Δ file (spec 0050 S9.1).
        static member ellipsometricSeries : ImportProfileName = ImportProfileName "Ellipsometric series"

        /// The built-in profile over `DataFileText.parseMuellerCsv` (`MuellerDataStore.fs`), a phone's ROI capture CSV
        /// (spec 0050 S9.2).
        static member phoneRoiCsv : ImportProfileName = ImportProfileName "Phone ROI CSV"

        /// The built-in profile over the Mueller-matrix observation file (spec 0050 S9.2).
        static member muellerObservations : ImportProfileName = ImportProfileName "Mueller-matrix observations"

        /// The four built-in profiles (spec 0050 S9.2).
        static member builtIns : ImportProfileName list =
            [
                ImportProfileName.intensitySeries
                ImportProfileName.ellipsometricSeries
                ImportProfileName.phoneRoiCsv
                ImportProfileName.muellerObservations
            ]

    /// The character between a row's cells in a lab file; its disk tag is an int, 1 to 3 in case order
    /// (spec 0050 S9.2).
    type DataDelimiter =
        | CommaDelimiter
        | SemicolonDelimiter
        | TabDelimiter

        member this.value : int =
            match this with
            | CommaDelimiter -> 1
            | SemicolonDelimiter -> 2
            | TabDelimiter -> 3

        /// The delimiter a disk tag names; any other tag is `None`, the caller's typed error (spec 0050 S9.2).
        static member tryOfWire (tag : int) : DataDelimiter option =
            match tag with
            | 1 -> Some CommaDelimiter
            | 2 -> Some SemicolonDelimiter
            | 3 -> Some TabDelimiter
            | _ -> None

        member this.text : string =
            match this with
            | CommaDelimiter -> "comma"
            | SemicolonDelimiter -> "semicolon"
            | TabDelimiter -> "tab"

        /// Every delimiter in tag order (spec 0050 S9.2).
        static member all : DataDelimiter list = [ CommaDelimiter; SemicolonDelimiter; TabDelimiter ]

        member this.character : char =
            match this with
            | CommaDelimiter -> ','
            | SemicolonDelimiter -> ';'
            | TabDelimiter -> '\t'

    /// Whether a lab file's first row names its columns; its disk tag is an int, 1 to 2 in case order (spec 0050 S9.2).
    type HeaderRowChoice =
        | HeaderOnFirstRow
        | NoHeaderRow

        member this.value : int =
            match this with
            | HeaderOnFirstRow -> 1
            | NoHeaderRow -> 2

        /// The choice a disk tag names; any other tag is `None`, the caller's typed error (spec 0050 S9.2).
        static member tryOfWire (tag : int) : HeaderRowChoice option =
            match tag with
            | 1 -> Some HeaderOnFirstRow
            | 2 -> Some NoHeaderRow
            | _ -> None

        member this.text : string =
            match this with
            | HeaderOnFirstRow -> "first row is a header"
            | NoHeaderRow -> "no header row"

        /// Both choices in tag order (spec 0050 S9.2).
        static member all : HeaderRowChoice list = [ HeaderOnFirstRow; NoHeaderRow ]

    /// Whether the lab's files start with the UTF-8 byte-order mark; the kit drops it before any read (step 128), so
    /// the profile keeps it for the wizard and the files the app writes. Its disk tag is an int, 1 to 2 (spec 0050 S9.2).
    type ByteOrderMark =
        | ByteOrderMarkPresent
        | ByteOrderMarkAbsent

        member this.value : int =
            match this with
            | ByteOrderMarkPresent -> 1
            | ByteOrderMarkAbsent -> 2

        /// The mark a disk tag names; any other tag is `None`, the caller's typed error (spec 0050 S9.2).
        static member tryOfWire (tag : int) : ByteOrderMark option =
            match tag with
            | 1 -> Some ByteOrderMarkPresent
            | 2 -> Some ByteOrderMarkAbsent
            | _ -> None

        member this.text : string =
            match this with
            | ByteOrderMarkPresent -> "byte-order mark"
            | ByteOrderMarkAbsent -> "no byte-order mark"

        /// Both cases in tag order (spec 0050 S9.2).
        static member all : ByteOrderMark list = [ ByteOrderMarkPresent; ByteOrderMarkAbsent ]

    /// The unit a scan column's numbers are in; its disk tag is an int, 1 to 6 in case order (spec 0050 S9.2).
    type ScanColumnUnit =
        | DegreeColumnUnit
        | RadianColumnUnit
        | NanometerColumnUnit
        | MicrometerColumnUnit
        | ElectronVoltColumnUnit
        | WavenumberColumnUnit

        member this.value : int =
            match this with
            | DegreeColumnUnit -> 1
            | RadianColumnUnit -> 2
            | NanometerColumnUnit -> 3
            | MicrometerColumnUnit -> 4
            | ElectronVoltColumnUnit -> 5
            | WavenumberColumnUnit -> 6

        /// The unit a disk tag names; any other tag is `None`, the caller's typed error (spec 0050 S9.2).
        static member tryOfWire (tag : int) : ScanColumnUnit option =
            match tag with
            | 1 -> Some DegreeColumnUnit
            | 2 -> Some RadianColumnUnit
            | 3 -> Some NanometerColumnUnit
            | 4 -> Some MicrometerColumnUnit
            | 5 -> Some ElectronVoltColumnUnit
            | 6 -> Some WavenumberColumnUnit
            | _ -> None

        /// A spectral unit's symbol is its `Units.UnitOfMeasure.abbreviation`, the one place it is written
        /// (spec 0050 S9.2).
        member this.text : string =
            match this with
            | DegreeColumnUnit -> "°"
            | RadianColumnUnit -> "rad"
            | NanometerColumnUnit -> Units.Nanometer.abbreviation
            | MicrometerColumnUnit -> Units.Micrometer.abbreviation
            | ElectronVoltColumnUnit -> Units.ElectronVolt.abbreviation
            | WavenumberColumnUnit -> Units.Wavenumber.abbreviation

        /// Every unit in tag order (spec 0050 S9.2).
        static member all : ScanColumnUnit list =
            [ DegreeColumnUnit; RadianColumnUnit; NanometerColumnUnit; MicrometerColumnUnit; ElectronVoltColumnUnit; WavenumberColumnUnit ]

        /// The spectral unit of a wavelength, energy or wavenumber column; an angle column has none (spec 0050 S9.2).
        member this.spectralUnitOpt : Units.UnitOfMeasure option =
            match this with
            | DegreeColumnUnit
            | RadianColumnUnit -> None
            | NanometerColumnUnit -> Some Units.Nanometer
            | MicrometerColumnUnit -> Some Units.Micrometer
            | ElectronVoltColumnUnit -> Some Units.ElectronVolt
            | WavenumberColumnUnit -> Some Units.Wavenumber

    /// Whether a scan column holds the reading on an item's dial or the physical value (001 §I: experiments record raw
    /// dial readings); its disk tag is an int, 1 to 2 in case order (spec 0050 S9.2).
    type ScanReading =
        | RawDialReading
        | PhysicalScanValue

        member this.value : int =
            match this with
            | RawDialReading -> 1
            | PhysicalScanValue -> 2

        /// The reading a disk tag names; any other tag is `None`, the caller's typed error (spec 0050 S9.2).
        static member tryOfWire (tag : int) : ScanReading option =
            match tag with
            | 1 -> Some RawDialReading
            | 2 -> Some PhysicalScanValue
            | _ -> None

        member this.text : string =
            match this with
            | RawDialReading -> "raw dial reading"
            | PhysicalScanValue -> "physical value"

        /// Both readings in tag order (spec 0050 S9.2).
        static member all : ScanReading list = [ RawDialReading; PhysicalScanValue ]

    /// A column of a lab file, counted from 1 as a lab counts it (spec 0050 S9.2).
    type DataColumnNumber =
        | DataColumnNumber of int

        member this.value : int = let (DataColumnNumber number) = this in number

        /// The number as given; below 1 names no column (spec 0050 S9.2).
        static member tryCreate (number : int) : DataColumnNumber option =
            if number < 1 then None else Some (DataColumnNumber number)

        /// The column's position among a row's cells, counted from 0 (spec 0050 S9.2).
        member this.index : int = let (DataColumnNumber number) = this in number - 1

    /// A column of a scan variable's values; a profile's scan columns, in column order, give the experiment's scan
    /// quantities in order (spec 0050 S9.2).
    type ScanColumn =
        {
            column : DataColumnNumber
            valueUnit : ScanColumnUnit
            reading : ScanReading
        }

    /// What each row of a lab file measured: a detector signal and its σ, or the sixteen elements m00…m33 in row order
    /// and the absolute throughput when the elements are normalized (spec 0050 S9.2; 001 §K).
    type ReadingColumns =
        | SignalColumns of signal : DataColumnNumber * sigmaOpt : DataColumnNumber option
        | MuellerElementColumns of elements : DataColumnNumber list * throughputOpt : DataColumnNumber option

    /// Every role a lab file's column holds: the scan variables, the reading, the capture's timestamp and index, the
    /// observable (T or R) and the experiment key that splits one file into experiments (spec 0050 S9.2, S9.3; 001 §K).
    type ColumnRoles =
        {
            scan : ScanColumn list
            reading : ReadingColumns
            timestampOpt : DataColumnNumber option
            captureIndexOpt : DataColumnNumber option
            observableOpt : DataColumnNumber option
            keyOpt : DataColumnNumber option
        }

    /// Why an import profile or its name was refused, the name or the columns in the reason (spec 0050 S9.2).
    type ImportProfileError =
        | BlankProfileNameErr of reason : string
        | BuiltInProfileNameErr of reason : string
        | DuplicateProfileNameErr of reason : string
        | SignalColumnCountErr of reason : string
        | MixedReadingColumnsErr of reason : string
        | RepeatedRoleErr of reason : string
        | MuellerElementCountErr of reason : string

        member this.describe : string =
            match this with
            | BlankProfileNameErr reason -> $"the import profile name is blank: {reason}"
            | BuiltInProfileNameErr reason -> $"the name belongs to a built-in import profile: {reason}"
            | DuplicateProfileNameErr reason -> $"the kit already holds an import profile of that name: {reason}"
            | SignalColumnCountErr reason -> $"a signal mapping needs exactly one signal column: {reason}"
            | MixedReadingColumnsErr reason -> $"a mapping reads a signal or Mueller elements, not both: {reason}"
            | RepeatedRoleErr reason -> $"a role is given to two columns: {reason}"
            | MuellerElementCountErr reason -> $"a Mueller mapping needs each of m00…m33 once: {reason}"

    /// One named mapping of a lab file's layout and columns, kept in the kit file; the four built-in profiles are code
    /// and never stored (spec 0050 S9.2; 001 §K).
    type ImportProfile =
        {
            name : ImportProfileName
            delimiter : DataDelimiter
            header : HeaderRowChoice
            bom : ByteOrderMark
            roles : ColumnRoles
        }

    type ImportProfileName with

        /// The one naming rule of a new profile: a built-in's name, or one a held profile has (compared ordinally), is
        /// refused (spec 0050 S9.2).
        member this.availableAmong (profiles : ImportProfile list) : Result<ImportProfileName, ImportProfileError> =
            if ImportProfileName.builtIns |> List.contains this then
                Error (BuiltInProfileNameErr $"'{this.value}'")
            elif profiles |> List.exists (fun profile -> profile.name = this) then
                Error (DuplicateProfileNameErr $"'{this.value}'")
            else
                Ok this

    /// The kit's naming template as its text; its grammar is NamingTemplates.fs's (spec 0050 S8.3).
    type NamingTemplate =
        | NamingTemplate of string

        member this.value : string = let (NamingTemplate text) = this in text

        /// 001 §J's template: every new kit's and the reference pipeline's labels' (spec 0050 S8.3).
        static member standard : NamingTemplate = NamingTemplate "{source}-({obj1}#{phi1}-{obj2}#{phi2})-{analyzer}-{note}"

    /// The kit file's metadata, using the caller's identity and clock instant (spec 0050 S4.1, S8.3, S9.2).
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
            namingTemplate : NamingTemplate
            importProfiles : ImportProfile list
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
                namingTemplate = NamingTemplate.standard
                importProfiles = []
            }

        /// Appends a profile under a name no built-in or held profile has; a saved profile is never replaced, because an
        /// experiment names it (spec 0050 S9.2; 001 §K).
        member this.withImportProfile (profile : ImportProfile) : Result<KitFile, ImportProfileError> =
            profile.name.availableAmong this.importProfiles
            |> Result.map (fun _ -> { this with importProfiles = this.importProfiles @ [ profile ] })

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

    /// One value of an import profile's experiment-key column (spec 0050 S9.1; 001 §K).
    type DataRowKey =
        | DataRowKey of string

        member this.value : string = let (DataRowKey text) = this in text

    /// Which rows of a data file are an experiment's: every row, or those whose key column holds the key, so one file
    /// serves many experiments (spec 0050 S9.1; 001 §K).
    type DataRowSelection =
        | AllDataRows
        | KeyedDataRows of key : DataRowKey

    /// An experiment's data: the kit file, the import profile it is read through and the rows that are the experiment's
    /// (spec 0050 S9.1; 001 §K).
    type MappedAttachment =
        {
            attachment : DataAttachment
            profile : ImportProfileName
            rows : DataRowSelection
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

    /// Mints a new item's identity; the composition root chooses how, as it chooses the clock (spec 0050 S7.2).
    [<ReferenceEquality>]
    type ItemIdProvider =
        {
            mintItemId : unit -> ItemId
        }

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

        /// The number the parameter holds now: a known value, an unknown's start, a calibrated value (spec 0050 S8.2).
        member this.currentValue : ParameterValue =
            match this with
            | KnownParameter value -> value
            | UnknownParameter unknown -> unknown.start
            | CalibratedParameter calibrated -> calibrated.value

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

    /// How an item is mounted: on a dial, whose raw reading maps to the physical angle as `sign · (reading − dialZero)`
    /// (`DialSign.physicalAngleOf`), or fixed, read off no dial (spec 0050 S7.1, S8.2; 001 §I).
    type ItemDial =
        | DialMounted of DialSign
        | FixedMounting

        /// The dial zero as an ordinary parameter of a dial-mounted item; a fixed one has none (spec 0050 S7.1).
        member this.parameterAxes : ItemParameterAxis list =
            match this with
            | DialMounted _ -> [ { name = ParameterName.dialZero; kind = AngleParameter; defaultValue = ParameterValue 0.0 } ]
            | FixedMounting -> []

    /// The parameter values an earlier revision of an item held: its short value history (spec 0050 S7.1; 001 §I).
    type ItemRevisionRecord =
        {
            revision : ItemRevision
            parameters : Parameter list
        }

    /// One physical piece in a kit, holding its own parameters, which no other item shares, even of the same model;
    /// the photo is a kit file and `history` lists earlier revisions oldest first (spec 0050 S7.1; 001 §I).
    type Item =
        {
            id : ItemId
            label : ItemLabel
            model : ItemModelInstance
            parameters : Parameter list
            dial : ItemDial
            photoOpt : KitRelativePath option
            notes : ItemNotes
            revision : ItemRevision
            history : ItemRevisionRecord list
        }

        /// What the kit snapshots and pins when it records the item: the model's exact version, then its bound physics,
        /// each named once (spec 0050 S7.1).
        member this.libraryReferences : EntryReference list =
            [ this.model.entryReferenceOpt; this.model.bindingReferenceOpt ] |> List.choose id |> List.distinct

        member this.tryFindParameter (name : ParameterName) : Parameter option =
            this.parameters |> List.tryFind (fun parameter -> parameter.name = name)

        /// The item with the named parameter's state replaced in place, once `Parameter.validate` keeps it. It edits the
        /// working revision; only a published revision moves `revision` (spec 0050 S7.1; 001 §P). It writes nothing.
        member this.withParameterState (name : ParameterName) (state : ParameterState) : Result<Item, ItemError> =
            match this.tryFindParameter name with
            | None -> Error (UnknownItemParameterErr $"'{name.value}' on '{this.label.value}'")
            | Some _ ->
                let parameter : Parameter = { name = name; state = state }

                parameter.validate
                |> Result.map (fun validated ->
                    {
                        this with
                            parameters = this.parameters |> List.map (fun held -> if held.name = name then validated else held)
                    })

        /// The item with those notes, keeping its parameters, revision and history: an edit of the working revision,
        /// which only `publishRevision` moves (spec 0050 S7.2).
        member this.withNotes (notes : ItemNotes) : Item = { this with notes = notes }

        /// The item with that photo; `None` drops the reference and never a file, as lab files are read-only for the
        /// app (spec 0050 S7.2; 001 §E).
        member this.withPhoto (photoOpt : KitRelativePath option) : Item = { this with photoOpt = photoOpt }

        /// The item under the new label, refused when another item of `items` holds its file name ignoring case,
        /// because every `<label>.item-json` shares the kit root; its own entry is ignored (spec 0050 S7.1, S7.2).
        member this.relabelledAmong (label : ItemLabel) (items : Item list) : Result<Item, ItemError> =
            let sameFile (held : Item) : string option =
                let clashes = String.Equals(held.label.fileStem.value, label.fileStem.value, StringComparison.OrdinalIgnoreCase)

                if held.id <> this.id && clashes then
                    let file = ItemKitFile.fileNameOf held.label.fileStem
                    Some $"'{label.value}' beside '{held.label.value}': both are stored as {file.value}, ignoring case"
                else
                    None

            match items |> List.tryPick sameFile with
            | Some reason -> Error (DuplicateItemLabelErr reason)
            | None -> Ok { this with label = label }

        /// The next revision, once each published parameter is one of the item's, named once, keeping its rules: the
        /// current values join `history`, `revision` counts up and each state replaces its own (spec 0050 S7.1; 001 §I).
        member this.publishRevision (published : Parameter list) : Result<Item, ItemError> =
            let rec publishFrom (item : Item) (taken : ParameterName list) (remaining : Parameter list) : Result<Item, ItemError> =
                match remaining with
                | [] -> Ok item
                | parameter :: _ when taken |> List.contains parameter.name ->
                    Error (InvalidParameterStateErr $"{parameter.name.value}: published twice in one revision of '{this.label.value}'")
                | parameter :: rest ->
                    item.withParameterState parameter.name parameter.state
                    |> Result.bind (fun next -> publishFrom next (parameter.name :: taken) rest)

            let replaced : ItemRevisionRecord =
                {
                    revision = this.revision
                    parameters = this.parameters
                }

            publishFrom this [] published
            |> Result.map (fun revised ->
                {
                    revised with
                        revision = this.revision.next
                        history = this.history @ [ replaced ]
                })

    type Item with

        /// The zero its dial reads from, its `dialZero` parameter's current number as an angle; a fixed item reads no
        /// dial, so `Angle.zero` (spec 0050 S8.2; 001 §I).
        member this.dialZeroAngle : Angle =
            match this.tryFindParameter ParameterName.dialZero with
            | Some parameter -> parameter.state.currentValue.angle
            | None -> Angle.zero

    /// An item model as the library resolved it: a binding form with the preset or sample version it binds, or a model
    /// that stands alone (spec 0050 S7.1; 001 §I).
    type ItemModelChoice =
        | SourcePresetChoice of model : ItemModelVersion * source : SourcePreset
        | PolarizerPresetChoice of model : ItemModelVersion * polarizer : PolarizerPreset
        | StackSampleChoice of model : ItemModelVersion * version : SampleVersionId * sample : Sample
        | LibraryModelChoice of ItemModelVersion

        /// The chosen model's exact version, with the preset or sample version it binds (spec 0050 S7.1).
        member this.instance : ItemModelInstance =
            match this with
            | SourcePresetChoice (model, source) -> PresetInstance (model.versionId, source.id)
            | PolarizerPresetChoice (model, polarizer) -> PresetInstance (model.versionId, polarizer.id)
            | StackSampleChoice (model, version, _) -> SampleInstance (model.versionId, version)
            | LibraryModelChoice model -> ItemModelVersionInstance model.versionId

        /// The model's axes over its typed binding, or a standalone form's own; empty when the form takes no such
        /// binding, which `ItemDraft.addTo` refuses (spec 0050 S7.1).
        member this.parameterAxes : ItemParameterAxis list =
            let boundAxes (model : ItemModelVersion) (binding : ItemModelBinding) : ItemParameterAxis list =
                model.entry.model.tryBoundParameterAxes binding |> Option.defaultValue []

            match this with
            | SourcePresetChoice (model, source) -> boundAxes model (SourcePresetBound source)
            | PolarizerPresetChoice (model, polarizer) -> boundAxes model (PolarizerPresetBound polarizer)
            | StackSampleChoice (model, _, sample) -> boundAxes model (StackSampleBound sample)
            | LibraryModelChoice model -> model.entry.model.parameterAxes

    /// What `AddItem` gives: a new item's id, label, chosen model and mounting (spec 0050 S7.1; 001 §P).
    type ItemDraft =
        {
            id : ItemId
            label : ItemLabel
            model : ItemModelChoice
            dial : ItemDial
        }

        /// The new item at revision 1, with every parameter its model and then its dial publish known at its default
        /// (spec 0050 S7.1; 001 §I).
        member this.toItem : Item =
            let item : Item =
                {
                    id = this.id
                    label = this.label
                    model = this.model.instance
                    parameters = this.model.parameterAxes @ this.dial.parameterAxes |> List.map Parameter.knownAtDefault
                    dial = this.dial
                    photoOpt = None
                    notes = ItemNotes ""
                    revision = ItemRevision.first
                    history = []
                }

            item

        /// The new item, refused when the kit holds its id or its label's file name ignoring case, because every
        /// `<label>.item-json` shares the kit root on Windows, or when its model does not take its binding. It writes
        /// nothing; the caller records the item (spec 0050 S7.1).
        member this.addTo (items : Item list) : Result<Item, ItemError> =
            let modelText (model : ItemModelVersion) : string =
                $"the item model '{model.entry.name.value}' ({model.versionId.entryReference.describe})"

            let takesText (model : ItemModelVersion) : string =
                match model.entry.model.bindingKindOpt with
                | Some SourcePresetBinding -> $"{modelText model} binds a source preset"
                | Some PolarizerPresetBinding -> $"{modelText model} binds a polarizer preset"
                | Some StackSampleBinding -> $"{modelText model} binds a stack sample"
                | None -> $"{modelText model} binds nothing"

            // The binding must be the kind its model's form takes (step 208), and a preset binds by its GUID (step 137).
            let unlessTaken (model : ItemModelVersion) (binding : ItemModelBinding) (bound : string) : string option =
                match model.entry.model.tryBoundParameterAxes binding with
                | Some _ -> None
                | None -> Some $"{takesText model}, not {bound}"

            let unlessGuid (model : ItemModelVersion) (preset : ElementValueId) (bound : string) : string option =
                match preset.tryToLibraryEntryId with
                | Some _ -> None
                | None -> Some $"{modelText model} cannot bind {bound}: its id is not a canonical GUID"

            let bindingRules : string option list =
                match this.model with
                | SourcePresetChoice (model, source) ->
                    let bound = $"the source preset '{source.name.value}' ({source.id.value})"
                    [ unlessTaken model (SourcePresetBound source) bound; unlessGuid model source.id bound ]
                | PolarizerPresetChoice (model, polarizer) ->
                    let bound = $"the polarizer preset '{polarizer.name.value}' ({polarizer.id.value})"
                    [ unlessTaken model (PolarizerPresetBound polarizer) bound; unlessGuid model polarizer.id bound ]
                | StackSampleChoice (model, version, sample) ->
                    let bound = $"the sample '{sample.name.value}' ({sample.id.value})"
                    [
                        unlessTaken model (StackSampleBound sample) bound
                        if version.sampleId = sample.id then
                            None
                        else
                            Some $"{modelText model} cannot bind {bound} at a version of the sample {version.sampleId.value}"
                    ]
                | LibraryModelChoice model ->
                    match model.entry.model.bindingKindOpt with
                    | Some _ -> [ Some $"{takesText model}, and the choice binds none" ]
                    | None -> []

            match items |> List.tryFind (fun held -> held.id = this.id) with
            | Some held ->
                Error (DuplicateItemIdErr $"'{this.label.value}' reuses the id {this.id.value} of '{held.label.value}'")
            | None ->
                // No held item has the draft's id here, so relabelling's own-entry exemption skips none (spec 0050 S7.2).
                this.toItem.relabelledAmong this.label items
                |> Result.bind (fun item ->
                    match bindingRules |> List.choose id with
                    | reason :: _ -> Error (InvalidItemModelBindingErr reason)
                    | [] -> Ok item)

    /// One item's versioned `<label>.item-json` content; a file is read back as its `item` (spec 0050 S7.1; 001 §A).
    type ItemFile =
        {
            schemaVersion : FileSchemaVersion
            item : Item
        }

        /// The only supported item file schema version (spec 0050 S7.1).
        static member currentSchemaVersion : FileSchemaVersion = FileSchemaVersion 1

    type Item with

        /// The item in the current item file schema (spec 0050 S7.1).
        member this.itemFile : ItemFile =
            {
                schemaVersion = ItemFile.currentSchemaVersion
                item = this
            }

    type FileStem with

        /// The first of `<text>`, `<text> (2)`, `<text> (3)`, … whose stem no taken text's stem equals, ignoring case: the
        /// one suffix sequence items and setups share. The taken are texts, not stems, as ELEV010 refuses a `string`
        /// beside a `FileStem` (spec 0050 S7.2, S8.2; `KitDataFolder.copyTargetFor`).
        static member firstFreeText (text : string) (taken : string list) : string =
            let takenStems = taken |> List.map FileStem.ofText

            let isTaken (candidate : string) : bool =
                let stem = FileStem.ofText candidate
                takenStems |> List.exists (fun held -> String.Equals(held.value, stem.value, StringComparison.OrdinalIgnoreCase))

            let rec tryCandidate (suffix : int) : string =
                let candidate =
                    if suffix = 1 then text
                    else $"{text} ({suffix.ToString(CultureInfo.InvariantCulture)})"

                if isTaken candidate then tryCandidate (suffix + 1) else candidate

            tryCandidate 1

    type ItemLabel with

        /// The first free label by `FileStem.firstFreeText`, because an item's file is named by its label (spec 0050
        /// S7.2, S8.2).
        member this.nextFreeAmong (taken : ItemLabel list) : ItemLabel =
            ItemLabel (FileStem.firstFreeText this.value (taken |> List.map (fun held -> held.value)))

    /// How a copied experiment's item comes into the target kit: as the kit's item of that id, bringing nothing, or
    /// added. Never stored, so it carries no wire tag (spec 0050 S7.2; 001 §H).
    type ItemCopyChoice =
        | MapOntoKitItem of ItemId
        | AddCopiedItem

    type Item with

        /// The held item of this id, because one id is one physical piece; else the first held item of this model and
        /// label; else added (spec 0050 S7.2; 001 §I).
        member this.defaultCopyChoice (held : Item list) : ItemCopyChoice =
            match held |> List.tryFind (fun item -> item.id = this.id) with
            | Some item -> MapOntoKitItem item.id
            | None ->
                match held |> List.tryFind (fun item -> item.model = this.model && item.label = this.label) with
                | Some item -> MapOntoKitItem item.id
                | None -> AddCopiedItem

    /// One row of a copy's item-mapping table: an item the copied experiments use and how it comes in (spec 0050 S7.2).
    type ItemCopyRow =
        {
            sourceItem : Item
            choice : ItemCopyChoice
        }

        /// One row per used item, in order, keeping the choice of a chosen row of the same source id and otherwise
        /// taking the default, so the panel keeps what the user chose while the picked experiments change (spec 0050 S7.2).
        static member copyRowsFor (used : Item list) (held : Item list) (chosen : ItemCopyRow list) : ItemCopyRow list =
            used
            |> List.map (fun item ->
                let choice =
                    match chosen |> List.tryFind (fun row -> row.sourceItem.id = item.id) with
                    | Some row -> row.choice
                    | None -> item.defaultCopyChoice held

                {
                    sourceItem = item
                    choice = choice
                })

        /// The added rows' items in order, each relabelled clear of the held labels and of those given before it, after
        /// every row is checked: a mapping must name a held id, and a held piece is mapped, never added (spec 0050 S7.2).
        static member itemsToAdd (rows : ItemCopyRow list) (held : Item list) : Result<Item list, ItemError> =
            let refusalOf (row : ItemCopyRow) : ItemError option =
                let source = row.sourceItem
                match row.choice with
                | MapOntoKitItem id ->
                    if held |> List.exists (fun item -> item.id = id) then None
                    else Some (UnknownKitItemErr $"'{source.label.value}' maps onto {id.value}")
                | AddCopiedItem ->
                    match held |> List.tryFind (fun item -> item.id = source.id) with
                    | Some item ->
                        Some (DuplicateItemIdErr $"'{source.label.value}' is the kit's '{item.label.value}' ({item.id.value}), which is mapped, not added")
                    | None -> None

            match rows |> List.tryPick refusalOf with
            | Some error -> Error error
            | None ->
                let heldLabels = held |> List.map (fun item -> item.label)

                rows
                |> List.choose (fun row ->
                    match row.choice with
                    | AddCopiedItem -> Some row.sourceItem
                    | MapOntoKitItem _ -> None)
                |> List.fold (fun (added : Item list) source ->
                    let taken = heldLabels @ (added |> List.map (fun item -> item.label))
                    let relabelled =
                        {
                            source with label = source.label.nextFreeAmong taken
                        }

                    added @ [ relabelled ]) []
                |> Ok

    /// Why a setup, one of its revisions or slots, or the item a slot holds was refused, named in the reason (spec
    /// 0050 S8.2; 001 §J).
    type SetupError =
        | InvalidSetupNameErr of reason : string
        | DuplicateSetupNameErr of reason : string
        | UnknownSetupErr of reason : string
        | UnknownSetupRevisionErr of reason : string
        | UnknownSetupSlotErr of reason : string
        | UnknownSlotItemErr of reason : string
        | IncompatibleSlotItemErr of reason : string
        /// One item in two slots, as one physical piece is in one place (spec 0050 S8.3; 001 §I).
        | SlotItemTwiceErr of reason : string
        /// A slot names an item the kit's items lack (spec 0050 S8.2, S7.2).
        | SetupItemReferenceErr of ItemError

        member this.describe : string =
            match this with
            | InvalidSetupNameErr reason -> $"the setup name is not valid: {reason}"
            | DuplicateSetupNameErr reason -> $"the kit already has a setup of that name: {reason}"
            | UnknownSetupErr reason -> $"the kit has no such setup: {reason}"
            | UnknownSetupRevisionErr reason -> $"the setup has no such revision: {reason}"
            | UnknownSetupSlotErr reason -> $"the table has no such slot: {reason}"
            | UnknownSlotItemErr reason -> $"the item is not available: {reason}"
            | IncompatibleSlotItemErr reason -> $"the item does not fit the slot: {reason}"
            | SlotItemTwiceErr reason -> $"the item is in two slots: {reason}"
            | SetupItemReferenceErr error -> $"a slot's item could not be resolved: {error.describe}"

    /// A setup's name, kept as given; its `<name>.setup-json` stem follows the shared name rule (spec 0050 S8.2;
    /// 001 §A; .spec-md §3.11).
    type SetupName =
        | SetupName of string

        member this.value : string = let (SetupName text) = this in text

        static member tryCreate (text : string) : Result<SetupName, SetupError> =
            if String.IsNullOrWhiteSpace text then
                Error (InvalidSetupNameErr $"'{text}': a setup name must not be blank")
            else
                Ok (SetupName text)

        member this.fileStem : FileStem = FileStem.ofText this.value

        /// The first free name by `FileStem.firstFreeText`, because a setup's file is named by its name (spec 0050 S8.2).
        member this.nextFreeAmong (taken : SetupName list) : SetupName =
            SetupName (FileStem.firstFreeText this.value (taken |> List.map (fun held -> held.value)))

        /// The first of `Setup 1`, `Setup 2`, … whose file stem no taken name has, ignoring case: the name a table
        /// takes when it becomes a setup without one (spec 0050 S8.2).
        static member firstFree (taken : SetupName list) : SetupName =
            let takenTexts = taken |> List.map (fun held -> held.value)

            // A text is free exactly when the shared sequence keeps it as given, so the stem test stays in one place.
            let rec tryNumber (number : int) : SetupName =
                let candidate = $"Setup {number.ToString(CultureInfo.InvariantCulture)}"
                if FileStem.firstFreeText candidate takenTexts = candidate then SetupName candidate else tryNumber (number + 1)

            tryNumber 1

    /// The revision a setup file carries, counting up from one (spec 0050 S8.2; 001 §J).
    type SetupRevision =
        | SetupRevision of int

        member this.value : int = let (SetupRevision revision) = this in revision

        static member first : SetupRevision = SetupRevision 1

        member this.next : SetupRevision = SetupRevision (this.value + 1)

    /// One revision of one setup, as an experiment references it (spec 0050 S8.2; 001 §J).
    type SetupReference =
        {
            name : SetupName
            revision : SetupRevision
        }

        member this.describe : string =
            $"'{this.name.value}' revision {this.revision.value.ToString(CultureInfo.InvariantCulture)}"
