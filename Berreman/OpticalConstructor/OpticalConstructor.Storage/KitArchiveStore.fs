namespace OpticalConstructor.Storage

open System
open System.IO
open System.IO.Compression
open Softellect.Sys.Primitives
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.KitArchive

/// The real kit archive over System.IO.Compression. Every .NET IO and compression exception is caught here and mapped to
/// KitArchiveError, and nothing above the proxy handles one (spec 0050 S6; .spec-md §4).
module KitArchiveStore =

    type KitFolder with

        /// The folder at a kit-relative path: this folder joined with the path's `/`-separated segments (spec 0050 S6).
        member private this.archiveFolderAt (path : KitRelativePath) : FolderName =
            path.value.Split('/')
            |> Array.map FolderName
            |> Array.fold (fun (folder : FolderName) (segment : FolderName) -> folder.combine segment) this.value

        /// The file at a kit-relative path, its folders joined as `archiveFolderAt` joins them (spec 0050 S6).
        member private this.archiveFileAt (path : KitRelativePath) : FileName =
            match path.parentOpt with
            | Some parent -> path.leafName.combine (this.archiveFolderAt parent)
            | None -> path.leafName.combine this.value

    type KitArchiveProxy with

        /// The real archive: a zip streams into `<zip>.tmp` and replaces the zip only once whole, the write-then-replace
        /// rule of `FileName.writeBytesAtomically` without holding the zip in memory. Building it touches nothing (spec 0050 S6).
        static member create () : KitArchiveProxy =
            let collected (results : Result<KitTreeEntry, KitArchiveError> list) : Result<KitTreeEntry list, KitArchiveError> =
                List.foldBack
                    (fun result rest ->
                        match result, rest with
                        | Ok entry, Ok entries -> Ok (entry :: entries)
                        | Error error, _
                        | Ok _, Error error -> Error error)
                    results
                    (Ok [])

            let listKitTree (kit : KitFolder) : Result<KitTreeEntry list, KitArchiveError> =
                let folder = kit.value
                try
                    Directory.EnumerateFileSystemEntries(folder.value, "*", SearchOption.AllDirectories)
                    |> Seq.map (fun name ->
                        let relative = Path.GetRelativePath(folder.value, name).Replace('\\', '/')
                        KitRelativePath.tryCreate relative
                        |> Result.map (fun path -> if Directory.Exists name then KitTreeFolder path else KitTreeFile path)
                        |> Result.mapError (fun error -> KitArchiveIoErr $"the kit folder '{folder.value}': {error.describe}"))
                    |> Seq.toList
                    |> collected
                    |> Result.map (List.sortWith (fun first second -> String.CompareOrdinal(first.path.value, second.path.value)))
                with error -> Error (KitArchiveIoErr $"the kit folder '{folder.value}': {error.Message}")

            let writeKitZip (job : KitZipJob) (observer : KitArchiveObserver) : Result<KitArchiveOutcome, KitArchiveError> =
                let temporary = FileName $"{job.zip.value}.tmp"
                let filesTotal = KitTreeEntry.filePathsOf job.entries |> List.length |> KitArchiveFileCount

                let zipFile (archive : ZipArchive) (entry : KitTreeEntry) : Result<unit, KitArchiveError> =
                    let file = job.kit.archiveFileAt entry.path
                    try
                        use source = File.OpenRead file.value
                        let zipEntry = archive.CreateEntry(entry.zipEntryName.value, CompressionLevel.Optimal)
                        use target = zipEntry.Open ()
                        source.CopyTo target
                        Ok ()
                    with error -> Error (KitArchiveIoErr $"the file '{file.value}': {error.Message}")

                let rec zipFrom
                    (archive : ZipArchive)
                    (pending : KitTreeEntry list)
                    (zipped : KitRelativePath list)
                    : Result<KitArchiveOutcome, KitArchiveError> =
                    match pending with
                    | [] -> Ok (KitArchiveFinished filesTotal)
                    | (KitTreeFolder _ as entry) :: rest ->
                        archive.CreateEntry entry.zipEntryName.value |> ignore
                        zipFrom archive rest zipped
                    | (KitTreeFile path as entry) :: rest ->
                        if observer.cancellation.IsCancellationRequested then
                            Ok KitArchiveCancelled
                        else
                            match zipFile archive entry with
                            | Error error -> Error error
                            | Ok () ->
                                let zippedNow = path :: zipped
                                observer.reportArchiveProgress (KitArchiveProgress.afterFiles zippedNow filesTotal)
                                zipFrom archive rest zippedNow

                let outcome =
                    try
                        let zipped =
                            use stream = new FileStream(temporary.value, FileMode.Create, FileAccess.Write, FileShare.None)
                            use archive = new ZipArchive(stream, ZipArchiveMode.Create)
                            zipFrom archive job.entries []

                        match zipped with
                        | Ok (KitArchiveFinished _) -> File.Move(temporary.value, job.zip.value, true)
                        | Ok KitArchiveCancelled
                        | Error _ -> ()

                        zipped
                    with error -> Error (KitArchiveIoErr $"the zip '{job.zip.value}': {error.Message}")

                match outcome with
                | Ok (KitArchiveFinished _) -> ()
                | Ok KitArchiveCancelled
                | Error _ ->
                    // Keep the job's own answer when the cleanup also fails (spec 0050 S6).
                    try
                        File.Delete temporary.value
                    with _ -> ()

                outcome

            let listZipEntries (zip : FileName) : Result<KitTreeEntry list, KitArchiveError> =
                try
                    use archive = ZipFile.OpenRead zip.value
                    archive.Entries
                    |> Seq.map (fun zipEntry ->
                        let name = ZipEntryName zipEntry.FullName
                        KitTreeEntry.tryOfZipEntryName name)
                    |> Seq.toList
                    |> collected
                with error -> Error (KitArchiveIoErr $"the zip '{zip.value}': {error.Message}")

            let extractKitZip (job : KitUnzipJob) (observer : KitArchiveObserver) : Result<KitArchiveOutcome, KitArchiveError> =
                let target = job.target.value
                let filesTotal = KitTreeEntry.filePathsOf job.entries |> List.length |> KitArchiveFileCount

                let rec outermostMissing (folder : DirectoryInfo) (missingOpt : FolderName option) : FolderName option =
                    if folder.Exists then
                        missingOpt
                    else
                        let missing = FolderName folder.FullName
                        match folder.Parent with
                        | null -> Some missing
                        | parent -> outermostMissing parent (Some missing)

                // Every entry is a KitRelativePath, so its place lies inside the target; no second check is written (spec 0050 S6).
                let extractFile (archive : ZipArchive) (entry : KitTreeEntry) : Result<unit, KitArchiveError> =
                    let file = job.target.archiveFileAt entry.path
                    try
                        match archive.GetEntry entry.zipEntryName.value with
                        | null -> Error (KitArchiveIoErr $"the zip '{job.zip.value}' holds no entry '{entry.zipEntryName.value}'")
                        | zipEntry ->
                            match entry.path.parentOpt with
                            | Some parent -> Directory.CreateDirectory (job.target.archiveFolderAt parent).value |> ignore
                            | None -> ()

                            use source = zipEntry.Open ()
                            use stream = new FileStream(file.value, FileMode.CreateNew, FileAccess.Write, FileShare.None)
                            source.CopyTo stream
                            Ok ()
                    with error -> Error (KitArchiveIoErr $"the file '{file.value}' from the zip '{job.zip.value}': {error.Message}")

                let rec extractFrom
                    (archive : ZipArchive)
                    (pending : KitTreeEntry list)
                    (extracted : KitRelativePath list)
                    : Result<KitArchiveOutcome, KitArchiveError> =
                    match pending with
                    | [] -> Ok (KitArchiveFinished filesTotal)
                    | KitTreeFolder path :: rest ->
                        Directory.CreateDirectory (job.target.archiveFolderAt path).value |> ignore
                        extractFrom archive rest extracted
                    | (KitTreeFile path as entry) :: rest ->
                        if observer.cancellation.IsCancellationRequested then
                            Ok KitArchiveCancelled
                        else
                            match extractFile archive entry with
                            | Error error -> Error error
                            | Ok () ->
                                let extractedNow = path :: extracted
                                observer.reportArchiveProgress (KitArchiveProgress.afterFiles extractedNow filesTotal)
                                extractFrom archive rest extractedNow

                let targetCheck =
                    try
                        if Directory.Exists target.value && not (Seq.isEmpty (Directory.EnumerateFileSystemEntries target.value)) then
                            Error (KitZipTargetNotEmptyErr $"'{target.value}'")
                        else
                            Ok (outermostMissing (DirectoryInfo target.value) None)
                    with error -> Error (KitArchiveIoErr $"the folder '{target.value}': {error.Message}")

                targetCheck
                |> Result.bind (fun createdOpt ->
                    // The target held nothing, so all it holds is this extraction's; a folder made to hold it goes whole (spec 0050 S6).
                    let removeCreated () : unit =
                        try
                            match createdOpt with
                            | Some created -> Directory.Delete(created.value, true)
                            | None ->
                                for place in Directory.GetFileSystemEntries target.value do
                                    try
                                        if Directory.Exists place then Directory.Delete(place, true) else File.Delete place
                                    with _ -> ()
                        with _ -> ()

                    let outcome =
                        try
                            Directory.CreateDirectory target.value |> ignore
                            use archive = ZipFile.OpenRead job.zip.value
                            extractFrom archive job.entries []
                        with error -> Error (KitArchiveIoErr $"the zip '{job.zip.value}' into '{target.value}': {error.Message}")

                    match outcome with
                    | Ok (KitArchiveFinished _) -> ()
                    | Ok KitArchiveCancelled
                    | Error _ -> removeCreated ()

                    outcome)

            {
                listKitTree = listKitTree
                writeKitZip = writeKitZip
                listZipEntries = listZipEntries
                extractKitZip = extractKitZip
            }
