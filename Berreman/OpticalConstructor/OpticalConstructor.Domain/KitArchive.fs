namespace OpticalConstructor.Domain

open System
open System.Threading
open Softellect.Sys.Primitives
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.Kit

/// What travels when a kit is sent: one zip, without the app's cache or what `sendExclude` names (spec 0050 S6; 001 §H).
module KitArchive =

    /// Whether a send-exclusion glob matches a kit path (spec 0050 S6).
    type GlobPathMatch =
        | GlobPathMatched
        | GlobPathUnmatched

    type SendExcludeGlob with

        /// Matches the whole path from the kit root. `**` spans whole segments, and a final one needs at least one, so
        /// `a/**` is what lies inside `a`. `*` and `?` stay inside one segment; other characters ignore case (spec 0050 S6).
        member this.matchOf (path : KitRelativePath) : GlobPathMatch =
            let rec matchCharacters (pattern : char list) (text : char list) : GlobPathMatch =
                match pattern, text with
                | [], [] -> GlobPathMatched
                | '*' :: restPattern, _ ->
                    match matchCharacters restPattern text, text with
                    | GlobPathMatched, _ -> GlobPathMatched
                    | GlobPathUnmatched, _ :: restText -> matchCharacters pattern restText
                    | GlobPathUnmatched, [] -> GlobPathUnmatched
                | '?' :: restPattern, _ :: restText -> matchCharacters restPattern restText
                | expected :: restPattern, actual :: restText
                    when String.Equals(string expected, string actual, StringComparison.OrdinalIgnoreCase) ->
                    matchCharacters restPattern restText
                | _ -> GlobPathUnmatched

            let rec matchSegments (pattern : string list) (segments : string list) : GlobPathMatch =
                match pattern, segments with
                | [], [] -> GlobPathMatched
                | [ "**" ], [] -> GlobPathUnmatched
                | [ "**" ], _ -> GlobPathMatched
                | "**" :: restPattern, _ ->
                    match matchSegments restPattern segments, segments with
                    | GlobPathMatched, _ -> GlobPathMatched
                    | GlobPathUnmatched, _ :: restSegments -> matchSegments pattern restSegments
                    | GlobPathUnmatched, [] -> GlobPathUnmatched
                | globSegment :: restPattern, segment :: restSegments ->
                    match matchCharacters (List.ofSeq globSegment) (List.ofSeq segment) with
                    | GlobPathMatched -> matchSegments restPattern restSegments
                    | GlobPathUnmatched -> GlobPathUnmatched
                | _ -> GlobPathUnmatched

            let globSegments = this.value.Replace('\\', '/').Split('/') |> List.ofArray
            let pathSegments = path.value.Split('/') |> List.ofArray
            matchSegments globSegments pathSegments

    /// Whether a kit path travels when the kit is sent (spec 0050 S6).
    type KitSendChoice =
        | SentWithKit
        | LeftOutOfSend

    /// One file or folder under a kit, as a send walks its tree (spec 0050 S6).
    type KitTreeEntry =
        | KitTreeFile of KitRelativePath
        | KitTreeFolder of KitRelativePath

        member this.path : KitRelativePath =
            match this with
            | KitTreeFile path
            | KitTreeFolder path -> path

    type KitFile with

        /// Leaves out the app's cache and every path a `sendExclude` glob matches, itself or through a folder holding
        /// it (spec 0050 S6; 001 §A, §E).
        member this.sendChoiceOf (path : KitRelativePath) : KitSendChoice =
            let rec matchAny (globs : SendExcludeGlob list) (candidate : KitRelativePath) : GlobPathMatch =
                match globs with
                | [] -> GlobPathUnmatched
                | glob :: rest ->
                    match glob.matchOf candidate with
                    | GlobPathMatched -> GlobPathMatched
                    | GlobPathUnmatched -> matchAny rest candidate

            let rec choiceOf (candidate : KitRelativePath) : KitSendChoice =
                match matchAny this.sendExclude candidate, candidate.parentOpt with
                | GlobPathMatched, _ -> LeftOutOfSend
                | GlobPathUnmatched, Some folder -> choiceOf folder
                | GlobPathUnmatched, None -> SentWithKit

            match path.pathRole with
            | KitAppCachePath -> LeftOutOfSend
            | KitContentPath -> choiceOf path

        /// The entries that travel, in the tree's order (spec 0050 S6).
        member this.sentEntries (tree : KitTreeEntry list) : KitTreeEntry list =
            tree
            |> List.choose (fun entry ->
                match this.sendChoiceOf entry.path with
                | SentWithKit -> Some entry
                | LeftOutOfSend -> None)

    type KitName with

        /// The one place `.zip` is written: a kit is sent as its file stem's zip (spec 0050 S6; 001 §H).
        member this.zipFileName : FileName = this.fileStem.withExtension (FileExtension ".zip")

    /// Why a kit could not be zipped or unzipped; the reason names the folder, file, zip or entry (spec 0050 S6).
    type KitArchiveError =
        /// A kit folder, a file in it, a zip or a folder to extract into could not be listed, read, written or removed (spec 0050 S6).
        | KitArchiveIoErr of reason : string
        /// A zip entry's name is not a path inside the folder it would be extracted into (spec 0050 S6).
        | KitZipEntryEscapesErr of reason : string
        /// The folder to extract into already holds a file or folder (spec 0050 S6).
        | KitZipTargetNotEmptyErr of reason : string
        /// The zip would be written inside the kit it holds (spec 0050 S6).
        | KitZipInsideKitErr of reason : string
        /// The zip holds no `.kit-json` at its root (spec 0050 S6).
        | KitZipHoldsNoKitErr of reason : string

        member this.describe : string =
            match this with
            | KitArchiveIoErr reason -> $"a kit archive could not be read or written: {reason}"
            | KitZipEntryEscapesErr reason -> $"a zip entry is not a path inside the folder: {reason}"
            | KitZipTargetNotEmptyErr reason -> $"the folder to extract into is not empty: {reason}"
            | KitZipInsideKitErr reason -> $"the zip would be written inside the kit it holds: {reason}"
            | KitZipHoldsNoKitErr reason -> $"the zip holds no kit: {reason}"

    /// A zip entry's name as the zip stores it, a folder's ending in `/` (spec 0050 S6).
    type ZipEntryName =
        | ZipEntryName of string

        member this.value : string = let (ZipEntryName name) = this in name

    type KitTreeEntry with

        /// The one reading of a zip entry's name: a trailing `/` makes a folder. A name that is not a kit-relative path
        /// is KitZipEntryEscapesErr naming the entry as written, so no entry can land outside its folder (spec 0050 S6).
        static member tryOfZipEntryName (name : ZipEntryName) : Result<KitTreeEntry, KitArchiveError> =
            let text = name.value
            let entry =
                if text.EndsWith('/') then
                    KitRelativePath.tryCreate (text.Substring(0, text.Length - 1)) |> Result.map KitTreeFolder
                else
                    KitRelativePath.tryCreate text |> Result.map KitTreeFile
            entry |> Result.mapError (fun error -> KitZipEntryEscapesErr $"'{text}': {error.describe}")

        /// The entry's name in a zip: the path's text for a file, followed by `/` for a folder (spec 0050 S6).
        member this.zipEntryName : ZipEntryName =
            match this with
            | KitTreeFile path -> path.value
            | KitTreeFolder path -> $"{path.value}/"
            |> ZipEntryName

    /// How many files an archive job handles (spec 0050 S6).
    type KitArchiveFileCount =
        | KitArchiveFileCount of int

        member this.value : int = let (KitArchiveFileCount count) = this in count

    /// Told after each file: the files handled so far and the job's file entries (spec 0050 S6).
    type KitArchiveProgress =
        {
            filesDone : KitArchiveFileCount
            filesTotal : KitArchiveFileCount
        }

    /// Told after each file and asked before each whether the job is cancelled, the token checked between steps as in
    /// `JobRunner.runLoop` (spec 0050 S6).
    [<ReferenceEquality>]
    type KitArchiveObserver =
        {
            reportArchiveProgress : KitArchiveProgress -> unit
            cancellation : CancellationToken
        }

        /// Tells nobody and is never cancelled (spec 0050 S6).
        static member unobserved : KitArchiveObserver =
            {
                reportArchiveProgress = ignore
                cancellation = CancellationToken.None
            }

    /// How an archive job ended (spec 0050 S6).
    type KitArchiveOutcome =
        /// Every file of the job was handled (spec 0050 S6).
        | KitArchiveFinished of files : KitArchiveFileCount
        /// The job stopped before its next file and left nothing behind (spec 0050 S6).
        | KitArchiveCancelled

    /// Zips these entries of the kit folder into one zip (spec 0050 S6).
    type KitZipJob =
        {
            kit : KitFolder
            entries : KitTreeEntry list
            zip : FileName
        }

    /// Extracts these entries of the zip into the target folder (spec 0050 S6).
    type KitUnzipJob =
        {
            zip : FileName
            entries : KitTreeEntry list
            target : KitFolder
        }

    /// The zip / unzip seam. It does IO only: what is sent and what is received is the kit's rule (spec 0050 S6; 001 §P).
    [<ReferenceEquality>]
    type KitArchiveProxy =
        {
            /// Every file and folder under the kit folder, kit-relative, in ordinal order; a missing folder is KitArchiveIoErr (spec 0050 S6).
            listKitTree : KitFolder -> Result<KitTreeEntry list, KitArchiveError>
            /// Zips each entry under its zip name; the zip appears whole or not at all, cancelled or failed (spec 0050 S6).
            writeKitZip : KitZipJob -> KitArchiveObserver -> Result<KitArchiveOutcome, KitArchiveError>
            /// Reads every entry name through `KitTreeEntry.tryOfZipEntryName`, in the zip's order (spec 0050 S6).
            listZipEntries : FileName -> Result<KitTreeEntry list, KitArchiveError>
            /// Extracts into an empty or missing folder; cancelled or failed, it removes everything it created (spec 0050 S6).
            extractKitZip : KitUnzipJob -> KitArchiveObserver -> Result<KitArchiveOutcome, KitArchiveError>
        }

    /// What the in-memory disk holds under one name (spec 0050 S6).
    type internal ArchiveDiskItem =
        | ArchiveDiskFile of content : FileByteContent
        | ArchiveDiskFolder
        | ArchiveDiskZip of entries : KitTreeEntry list * contents : Map<KitRelativePath, FileByteContent>

    type FileName with

        member private this.archiveDiskSpelling : FileName = FileName (this.value.Replace('\\', '/').TrimEnd('/'))

        /// The disk's one comparison: `\` read as `/`, no trailing `/`, case ignored (spec 0050 S6).
        member private this.archiveDiskKey : FileName = FileName (this.archiveDiskSpelling.value.ToUpperInvariant())

        /// Every folder holding this name, outermost first, below its drive or root (spec 0050 S6).
        member private this.archiveDiskFolders : FileName list =
            let segments = this.archiveDiskSpelling.value.Split('/')
            let firstFolder = if segments[0] = "" || segments[0].EndsWith(':') then 2 else 1
            [ firstFolder .. segments.Length - 1 ] |> List.map (fun count -> String.Join("/", segments, 0, count) |> FileName)

    type FolderName with

        member private this.archiveDiskName : FileName = FileName this.value

    type KitFolder with

        member private this.archiveDiskFileOf (path : KitRelativePath) : FileName =
            FileName $"{this.value.value.Replace('\\', '/').TrimEnd('/')}/{path.value}"

    /// The double's disk, shared by every proxy built over it as two windows share one disk. Its state is mutated only
    /// inside its members, the boundary of `InMemoryKitFolder` (spec 0050 S6).
    type InMemoryArchiveDisk private () =
        let diskSync = obj ()
        let mutable items : Map<FileName, FileName * ArchiveDiskItem> = Map.empty

        /// An empty disk (spec 0050 S6).
        static member create () : InMemoryArchiveDisk = InMemoryArchiveDisk ()

        member internal _.tryDiskItem (name : FileName) : (FileName * ArchiveDiskItem) option =
            lock diskSync (fun () -> Map.tryFind name.archiveDiskKey items)

        /// Places or replaces an item under its first spelling, every folder holding it existing from then on (spec 0050 S6).
        member internal _.setDiskItem (name : FileName) (item : ArchiveDiskItem) : unit =
            lock diskSync (fun () ->
                let withFolders =
                    name.archiveDiskFolders
                    |> List.fold (fun (held : Map<FileName, FileName * ArchiveDiskItem>) folder ->
                        match Map.tryFind folder.archiveDiskKey held with
                        | Some (_, ArchiveDiskFolder) -> held
                        | Some (_, ArchiveDiskFile _)
                        | Some (_, ArchiveDiskZip _)
                        | None -> Map.add folder.archiveDiskKey (folder, ArchiveDiskFolder) held) items
                let spelling =
                    match Map.tryFind name.archiveDiskKey withFolders with
                    | Some (placed, _) -> placed
                    | None -> name.archiveDiskSpelling
                items <- Map.add name.archiveDiskKey (spelling, item) withFolders)

        member internal _.removeDiskItem (name : FileName) : unit =
            lock diskSync (fun () -> items <- Map.remove name.archiveDiskKey items)

        /// Every item inside a folder at any depth, in ordinal order of name (spec 0050 S6).
        member internal _.diskItemsUnder (folder : FileName) : (FileName * ArchiveDiskItem) list =
            let prefix = $"{folder.archiveDiskKey.value}/"
            lock diskSync (fun () ->
                items
                |> Map.toList
                |> List.choose (fun (key, held) -> if key.value.StartsWith(prefix, StringComparison.Ordinal) then Some held else None))
            |> List.sortWith (fun (first, _) (second, _) -> String.CompareOrdinal(first.value, second.value))

        /// An outside writer placing or replacing a file, every folder holding it existing from then on (spec 0050 S6).
        member this.placeFile (file : FileName) (content : FileByteContent) : unit = this.setDiskItem file (ArchiveDiskFile content)

        /// An outside writer placing a folder and every folder holding it (spec 0050 S6).
        member this.placeFolder (folder : FolderName) : unit = this.setDiskItem folder.archiveDiskName ArchiveDiskFolder

        /// A file's bytes; a folder, a zip or a name the disk does not hold is None (spec 0050 S6).
        member this.tryDiskFile (file : FileName) : FileByteContent option =
            match this.tryDiskItem file with
            | Some (_, ArchiveDiskFile content) -> Some content
            | Some (_, ArchiveDiskFolder)
            | Some (_, ArchiveDiskZip _)
            | None -> None

        /// Every file, folder and zip the disk holds, forward-slashed, in ordinal order (spec 0050 S6).
        member _.paths () : FileName list =
            lock diskSync (fun () -> items |> Map.values |> Seq.map fst |> Seq.toList)
            |> List.sortWith (fun first second -> String.CompareOrdinal(first.value, second.value))

    type KitArchiveProxy with

        /// The double: the four fields over `disk`, so every proxy over one disk sees the others' writes (spec 0050 S6).
        static member createInMemory (disk : InMemoryArchiveDisk) : KitArchiveProxy =
            let filePaths (entries : KitTreeEntry list) : KitRelativePath list =
                entries
                |> List.choose (fun entry ->
                    match entry with
                    | KitTreeFile path -> Some path
                    | KitTreeFolder _ -> None)

            let progressOf (handled : KitRelativePath list) (filesTotal : KitArchiveFileCount) : KitArchiveProgress =
                {
                    filesDone = handled |> List.length |> KitArchiveFileCount
                    filesTotal = filesTotal
                }

            let listKitTree (kit : KitFolder) : Result<KitTreeEntry list, KitArchiveError> =
                let folder = kit.value.archiveDiskName
                match disk.tryDiskItem folder with
                | Some (_, ArchiveDiskFolder) ->
                    disk.diskItemsUnder folder
                    |> List.choose (fun (name, item) ->
                        kit.tryKitPathOf name
                        |> Option.map (fun path ->
                            match item with
                            | ArchiveDiskFolder -> KitTreeFolder path
                            | ArchiveDiskFile _
                            | ArchiveDiskZip _ -> KitTreeFile path))
                    |> List.sortWith (fun first second -> String.CompareOrdinal(first.path.value, second.path.value))
                    |> Ok
                | Some (_, ArchiveDiskFile _)
                | Some (_, ArchiveDiskZip _)
                | None -> Error (KitArchiveIoErr $"'{folder.value}': the in-memory disk holds no such folder")

            let writeKitZip (job : KitZipJob) (observer : KitArchiveObserver) : Result<KitArchiveOutcome, KitArchiveError> =
                let files = filePaths job.entries
                let filesTotal = files |> List.length |> KitArchiveFileCount

                let rec zipFrom
                    (pending : KitRelativePath list)
                    (zipped : KitRelativePath list)
                    (contents : Map<KitRelativePath, FileByteContent>)
                    : Result<KitArchiveOutcome, KitArchiveError> =
                    match pending with
                    | [] ->
                        disk.setDiskItem job.zip (ArchiveDiskZip (job.entries, contents))
                        Ok (KitArchiveFinished filesTotal)
                    | path :: rest ->
                        if observer.cancellation.IsCancellationRequested then
                            Ok KitArchiveCancelled
                        else
                            let file = job.kit.archiveDiskFileOf path
                            match disk.tryDiskFile file with
                            | Some content ->
                                let zippedNow = path :: zipped
                                observer.reportArchiveProgress (progressOf zippedNow filesTotal)
                                zipFrom rest zippedNow (Map.add path content contents)
                            | None -> Error (KitArchiveIoErr $"'{file.value}': the in-memory disk holds no such file")

                match disk.tryDiskItem job.zip with
                | Some (_, ArchiveDiskFolder) -> Error (KitArchiveIoErr $"'{job.zip.value}': a folder holds the zip's name")
                | Some (_, ArchiveDiskFile _)
                | Some (_, ArchiveDiskZip _)
                | None -> zipFrom files [] Map.empty

            let listZipEntries (zip : FileName) : Result<KitTreeEntry list, KitArchiveError> =
                let rec readFrom (pending : KitTreeEntry list) (read : KitTreeEntry list) : Result<KitTreeEntry list, KitArchiveError> =
                    match pending with
                    | [] -> Ok (List.rev read)
                    | entry :: rest ->
                        KitTreeEntry.tryOfZipEntryName entry.zipEntryName
                        |> Result.bind (fun readEntry -> readFrom rest (readEntry :: read))

                match disk.tryDiskItem zip with
                | Some (_, ArchiveDiskZip (entries, _)) -> readFrom entries []
                | Some (_, ArchiveDiskFile _)
                | Some (_, ArchiveDiskFolder)
                | None -> Error (KitArchiveIoErr $"'{zip.value}': the in-memory disk holds no such zip")

            let extractKitZip (job : KitUnzipJob) (observer : KitArchiveObserver) : Result<KitArchiveOutcome, KitArchiveError> =
                let target = job.target.value.archiveDiskName
                let targetKey = target.archiveDiskKey.value

                // What the extraction added: the target, what lies in it, and the folders it made to hold the target.
                let removeCreated (held : Set<FileName>) : unit =
                    disk.paths ()
                    |> List.filter (fun name ->
                        let key = name.archiveDiskKey
                        not (Set.contains key held)
                        && (key.value = targetKey
                            || key.value.StartsWith($"{targetKey}/", StringComparison.Ordinal)
                            || targetKey.StartsWith($"{key.value}/", StringComparison.Ordinal)))
                    |> List.iter disk.removeDiskItem

                let extractFrom (contents : Map<KitRelativePath, FileByteContent>) : Result<KitArchiveOutcome, KitArchiveError> =
                    let held = disk.paths () |> List.map (fun name -> name.archiveDiskKey) |> Set.ofList
                    let filesTotal = filePaths job.entries |> List.length |> KitArchiveFileCount

                    let rec extractEntries (pending : KitTreeEntry list) (extracted : KitRelativePath list) : Result<KitArchiveOutcome, KitArchiveError> =
                        match pending with
                        | [] -> Ok (KitArchiveFinished filesTotal)
                        | KitTreeFolder path :: rest ->
                            disk.setDiskItem (job.target.archiveDiskFileOf path) ArchiveDiskFolder
                            extractEntries rest extracted
                        | KitTreeFile path :: rest ->
                            if observer.cancellation.IsCancellationRequested then
                                removeCreated held
                                Ok KitArchiveCancelled
                            else
                                match Map.tryFind path contents with
                                | Some content ->
                                    disk.setDiskItem (job.target.archiveDiskFileOf path) (ArchiveDiskFile content)
                                    let extractedNow = path :: extracted
                                    observer.reportArchiveProgress (progressOf extractedNow filesTotal)
                                    extractEntries rest extractedNow
                                | None ->
                                    removeCreated held
                                    Error (KitArchiveIoErr $"'{job.zip.value}': the zip holds no file '{path.value}'")

                    disk.setDiskItem target ArchiveDiskFolder
                    extractEntries job.entries []

                match disk.diskItemsUnder target, disk.tryDiskItem target, disk.tryDiskItem job.zip with
                | _ :: _, _, _ -> Error (KitZipTargetNotEmptyErr $"'{target.value}'")
                | [], Some (_, ArchiveDiskFile _), _
                | [], Some (_, ArchiveDiskZip _), _ -> Error (KitArchiveIoErr $"'{target.value}': a file holds the folder's name")
                | [], _, Some (_, ArchiveDiskZip (_, contents)) -> extractFrom contents
                | [], _, Some (_, ArchiveDiskFile _)
                | [], _, Some (_, ArchiveDiskFolder)
                | [], _, None -> Error (KitArchiveIoErr $"'{job.zip.value}': the in-memory disk holds no such zip")

            {
                listKitTree = listKitTree
                writeKitZip = writeKitZip
                listZipEntries = listZipEntries
                extractKitZip = extractKitZip
            }

        /// Zips what the kit file sends, never the app's cache or what its globs leave out; a zip inside the kit it
        /// holds is KitZipInsideKitErr, and nothing is listed or written (spec 0050 S6; 001 §H).
        member this.sendKit
            (kitFile : KitFile)
            (kit : KitFolder)
            (zip : FileName)
            (observer : KitArchiveObserver)
            : Result<KitArchiveOutcome, KitArchiveError> =
            match kit.tryKitPathOf zip with
            | Some _ -> Error (KitZipInsideKitErr $"'{zip.value}'")
            | None ->
                this.listKitTree kit
                |> Result.bind (fun tree ->
                    let job : KitZipJob = { kit = kit; entries = kitFile.sentEntries tree; zip = zip }
                    this.writeKitZip job observer)

        /// Extracts a zip holding `.kit-json` at its root, never its app cache: a foreign `.oc/lock` would make the
        /// received kit look open elsewhere. A zip without one is KitZipHoldsNoKitErr, and nothing is written (spec 0050 S6).
        member this.receiveKit
            (zip : FileName)
            (target : KitFolder)
            (observer : KitArchiveObserver)
            : Result<KitArchiveOutcome, KitArchiveError> =
            this.listZipEntries zip
            |> Result.bind (fun entries ->
                let kitFileOpt =
                    entries
                    |> List.tryPick (fun entry ->
                        match entry with
                        | KitTreeFile path
                            when String.Equals(path.value, KitFile.fileName.value, StringComparison.OrdinalIgnoreCase) ->
                            Some path
                        | KitTreeFile _
                        | KitTreeFolder _ -> None)

                match kitFileOpt with
                | None -> Error (KitZipHoldsNoKitErr $"'{zip.value}'")
                | Some _ ->
                    let contentEntries =
                        entries
                        |> List.choose (fun entry ->
                            match entry.path.pathRole with
                            | KitContentPath -> Some entry
                            | KitAppCachePath -> None)
                    let job : KitUnzipJob = { zip = zip; entries = contentEntries; target = target }
                    this.extractKitZip job observer)

    /// The save dialog of Send kit… and the open dialog of Open kit from zip…, as `KitFolderPicker` (spec 0050 S6; 001 §H).
    [<ReferenceEquality>]
    type KitZipPicker =
        {
            /// Asks where to write the zip, suggesting the given name (`KitName.zipFileName`); None when cancelled (spec 0050 S6).
            pickZipToWrite : FileName -> Async<FileName option>
            /// Asks for a zip to open; None when cancelled (spec 0050 S6).
            pickZipToOpen : unit -> Async<FileName option>
        }

        /// Answers None to both at once, the picker of a scene that picks nothing (spec 0050 S6).
        static member none : KitZipPicker =
            {
                pickZipToWrite = fun _ -> async { return None }
                pickZipToOpen = fun () -> async { return None }
            }
