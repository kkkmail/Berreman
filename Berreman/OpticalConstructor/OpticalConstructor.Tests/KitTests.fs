namespace OpticalConstructor.Tests

open System
open System.Globalization
open Xunit
open Softellect.Sys.Primitives
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.Kit

module KitTests =

    let private exampleLockHolder : KitLockHolder =
        {
            owner =
                {
                    machine = MachineName "BENCH-1"
                    user = UserName "lab"
                    processId = ProcessId 4242
                }
            acquiredAt = ClockTime (DateTime(2026, 11, 3, 12, 0, 0, DateTimeKind.Utc))
        }

    [<Fact>]
    let ``a kit name keeps its text, refuses a blank one and takes its file stem from the shared name rule`` () : unit =
        let expectedName = KitName "Optics: QZ/LR"
        let expectedStem = FileStem "Optics_ QZ_LR"

        match KitName.tryCreate "Optics: QZ/LR" with
        | Ok name ->
            Assert.Equal<KitName>(expectedName, name)
            Assert.Equal<string>("Optics: QZ/LR", name.value)
            Assert.Equal<FileStem>(expectedStem, name.fileStem)
            Assert.Equal<FileStem>(FileStem.ofText "Optics: QZ/LR", name.fileStem)
        | Error error -> Assert.Fail(error.describe)

        match KitName.tryCreate "  Optics: QZ/LR  " with
        | Ok name -> Assert.Equal<string>("  Optics: QZ/LR  ", name.value)
        | Error error -> Assert.Fail(error.describe)

        for text in [ ""; "   "; "\t\r\n" ] do
            match KitName.tryCreate text with
            | Error (InvalidKitNameErr reason) -> Assert.False(String.IsNullOrWhiteSpace reason)
            | Error error -> Assert.Fail($"expected an invalid kit name: {error.describe}")
            | Ok name -> Assert.Fail($"accepted a blank kit name: '{name.value}'")

    [<Fact>]
    let ``a new kit file is created and modified at one instant, with no notes and no send exclusions`` () : unit =
        let id = KitId (Guid.Parse "6f1c2a9e-3b7d-4e25-9a41-0c8d5e7f2b16")
        let name = KitName "Optics: QZ/LR"
        let author = KitAuthor "Test author"
        let instant = ClockTime (DateTime(2026, 11, 3, 12, 0, 0, DateTimeKind.Utc))
        let clock : ClockProvider =
            {
                now = fun () -> instant
            }
        let expectedVersion = FileSchemaVersion 1
        let noNotes = KitNotes ""
        let kit = KitFile.create id name author (clock.now ())

        Assert.Equal<FileSchemaVersion>(expectedVersion, KitFile.currentSchemaVersion)
        Assert.Equal<FileSchemaVersion>(expectedVersion, kit.schemaVersion)
        Assert.Equal<KitId>(id, kit.id)
        Assert.Equal<KitName>(name, kit.name)
        Assert.Equal<KitAuthor>(author, kit.author)
        Assert.Equal<ClockTime>(instant, kit.created)
        Assert.Equal<ClockTime>(instant, kit.modified)
        Assert.Equal<KitNotes>(noNotes, kit.notes)
        Assert.Empty(kit.sendExclude)

    [<Fact>]
    let ``a kit's naming template keeps its text, and a new kit takes the standard one`` () : unit =
        let id = KitId (Guid.Parse "6f1c2a9e-3b7d-4e25-9a41-0c8d5e7f2b16")
        let name = KitName "Optics: QZ/LR"
        let author = KitAuthor "Test author"
        let instant = ClockTime (DateTime(2026, 11, 3, 12, 0, 0, DateTimeKind.Utc))
        let kit = KitFile.create id name author instant

        Assert.Equal<string>("{obj1}", (NamingTemplate "{obj1}").value)
        Assert.Equal<string>("{source}-({obj1}#{phi1}-{obj2}#{phi2})-{analyzer}-{note}", NamingTemplate.standard.value)
        Assert.Equal<NamingTemplate>(NamingTemplate.standard, kit.namingTemplate)

    [<Fact>]
    let ``a new kit is named for its folder`` () : unit =
        let id = KitId (Guid.Parse "6f1c2a9e-3b7d-4e25-9a41-0c8d5e7f2b16")
        let author = KitAuthor "Test author"
        let instant = ClockTime (DateTime(2026, 11, 3, 12, 0, 0, DateTimeKind.Utc))
        let name = KitName "Optics QZ"
        let expected = KitFile.create id name author instant
        let folderNames =
            [
                FolderName "C:/kits/Optics QZ"
                FolderName @"C:\kits\Optics QZ\"
                FolderName "C:/kits/Optics QZ/"
            ]

        for folderName in folderNames do
            let folder = KitFolder folderName
            Assert.Equal<Result<KitFile, KitError>>(Ok expected, folder.namedKitFile id author instant)

        for folderName in [ FolderName "C:/"; FolderName @"C:\"; FolderName "/"; FolderName "C:/kits/   /" ] do
            let folder = KitFolder folderName
            match folder.namedKitFile id author instant with
            | Error (InvalidKitNameErr reason) -> Assert.False(String.IsNullOrWhiteSpace reason)
            | Error error -> Assert.Fail($"Expected InvalidKitNameErr, got {error.describe}")
            | Ok _ -> Assert.Fail("A folder without a nonblank leaf must not name a kit.")

    [<Fact>]
    let ``the kit file is named by its extension alone`` () : unit =
        let expectedExtension = FileExtension ".kit-json"
        let expectedFileName = FileName ".kit-json"
        Assert.Equal<FileExtension>(expectedExtension, KitFile.extension)
        Assert.Equal<FileName>(expectedFileName, KitFile.fileName)

    [<Fact>]
    let ``a snapshot is named by its entry id and version and its kind's extension, under library`` () : unit =
        let silicon =
            (BuiltinCatalogue.catalogueData ()).envelopes
            |> List.find (fun envelope -> envelope.content.id = MaterialIds.silicon.toLibraryEntryId)
        let expectedName = FileName $"{MaterialIds.silicon.value:D}@v1.material-json"
        let expectedPath = KitRelativePath $"library/{expectedName.value}"
        Assert.Equal<FileName>(expectedName, silicon.snapshotFileName)
        Assert.Equal<KitRelativePath>(expectedPath, silicon.snapshotPath)

    [<Fact>]
    let ``a kit-relative path keeps forward slashes and refuses an absolute or escaping path`` () : unit =
        let validPaths : (string * KitRelativePath) list =
            [
                @"data\raw\run 1.csv", KitRelativePath "data/raw/run 1.csv"
                "data/raw/run 1.csv", KitRelativePath "data/raw/run 1.csv"
                ".kit-json", KitRelativePath ".kit-json"
                @"data\raw/run 1.csv", KitRelativePath "data/raw/run 1.csv"
                " data/Run 1.csv ", KitRelativePath " data/Run 1.csv "
                "data/.hidden.csv", KitRelativePath "data/.hidden.csv"
            ]

        for text, expected in validPaths do
            match KitRelativePath.tryCreate text with
            | Ok path -> Assert.Equal<KitRelativePath>(expected, path)
            | Error error -> Assert.Fail(error.describe)

        let invalidPaths =
            [
                ""
                "  "
                "\t\r\n"
                "/data/a.csv"
                "C:/a.csv"
                "C:a.csv"
                "../a.csv"
                "data/../../a.csv"
                "data/./a.csv"
                "data//a.csv"
                @"\data\a.csv"
                @"\\server\share\a.csv"
                @"C:\a.csv"
                @"data\..\a.csv"
                "data/"
                "."
                ".."
                "data/a.csv:stream"
            ]

        for text in invalidPaths do
            match KitRelativePath.tryCreate text with
            | Error (InvalidKitPathErr reason) ->
                Assert.False(String.IsNullOrWhiteSpace reason)
                Assert.Contains(text, reason)
            | Error error -> Assert.Fail($"expected an invalid kit path: {error.describe}")
            | Ok path -> Assert.Fail($"accepted an invalid kit path: '{path.value}'")

    [<Fact>]
    let ``a kit path knows its folder, its leaf and whether it is the app's cache`` () : unit =
        let run = KitRelativePath "data/raw/run 1.csv"
        let raw = KitRelativePath "data/raw"
        Assert.Equal<KitRelativePath option>(Some raw, run.parentOpt)
        Assert.Equal<FileName>(FileName "run 1.csv", run.leafName)
        Assert.Equal<KitPathRole>(KitContentPath, run.pathRole)

        let kitJson = KitRelativePath ".kit-json"
        Assert.Equal<KitRelativePath option>(None, kitJson.parentOpt)
        Assert.Equal<FileName>(FileName ".kit-json", kitJson.leafName)

        Assert.Equal<KitRelativePath>(KitRelativePath ".oc", KitRelativePath.appCacheFolder)
        Assert.Equal<KitPathRole>(KitAppCachePath, KitRelativePath.appCacheFolder.pathRole)
        Assert.Equal<KitPathRole>(KitAppCachePath, (KitRelativePath ".oc/lock").pathRole)
        Assert.Equal<KitPathRole>(KitContentPath, (KitRelativePath ".ocx").pathRole)

    [<Fact>]
    let ``every kit file kind renders its 001 extension and names its files`` () : unit =
        let kinds = [ ItemKitFile; SetupKitFile; ExperimentKitFile; StudyKitFile; FitRunKitFile ]
        let extensions =
            [
                FileExtension ".item-json"
                FileExtension ".setup-json"
                FileExtension ".experiment-json"
                FileExtension ".study-json"
                FileExtension ".fitrun-json"
            ]
        let stem = FileStem "CPL-(QZ#45-LR#45)-CPL"
        let experimentFileName = FileName "CPL-(QZ#45-LR#45)-CPL.experiment-json"

        Assert.Equal<KitFileKind list>(kinds, KitFileKind.all)
        Assert.Equal<FileExtension list>(extensions, kinds |> List.map (fun kind -> kind.extension))

        for kind, extension in List.zip kinds extensions do
            Assert.Equal<FileName>(stem.withExtension extension, kind.fileNameOf stem)

        Assert.Equal<FileName>(experimentFileName, ExperimentKitFile.fileNameOf stem)

    [<Fact>]
    let ``a lock holder renders who holds the lock, on which machine and since when`` () : unit =
        let originalCulture = CultureInfo.CurrentCulture
        try
            CultureInfo.CurrentCulture <- CultureInfo.GetCultureInfo "ar-SA"
            Assert.Equal<string>(
                "lab on BENCH-1 (process 4242) since 2026-11-03 12:00:00 UTC", exampleLockHolder.describe)
        finally
            CultureInfo.CurrentCulture <- originalCulture

    [<Fact>]
    let ``every kit error renders its condition and its payload`` () : unit =
        let cases : (KitError * string) list =
            [
                KitIoErr "unreadable folder", "a kit file could not be read or written: unreadable folder"
                NotAKitFolderErr "missing folder", "the folder is not a kit: missing folder"
                KitAlreadyExistsErr "existing folder", "the folder is already a kit: existing folder"
                KitFileRejectedErr "invalid file", "a kit file was refused: invalid file"
                InvalidKitNameErr "blank name", "the kit name is not valid: blank name"
                MissingKitFileErr "data/missing.csv", "the kit holds no such file: data/missing.csv"
                InvalidKitPathErr "../outside.csv", "the path is not inside the kit: ../outside.csv"
                KitLockedErr exampleLockHolder, "the kit is open elsewhere: lab on BENCH-1 (process 4242) since 2026-11-03 12:00:00 UTC"
                KitReadOnlyErr "writeKitFile", "the kit is open read-only: writeKitFile"
                SnapshotExistsErr "library/entry@v1.material-json", "the kit already holds this library snapshot: library/entry@v1.material-json"
            ]

        for error, expected in cases do
            Assert.Equal<string>(expected, error.describe)

        let descriptions = cases |> List.map (fun (error, _) -> error.describe)
        Assert.Equal(10, descriptions |> List.distinct |> List.length)

    [<Fact>]
    let ``a data folder is the kit's data folder or a folder inside it`` () : unit =
        let rootPath = KitRelativePath "data"
        Assert.Equal<KitRelativePath>(rootPath, KitDataFolder.root.value)

        for path in [ rootPath; KitRelativePath "data/raw/run 1" ] do
            match KitDataFolder.tryCreate path with
            | Ok folder -> Assert.Equal<KitRelativePath>(path, folder.value)
            | Error error -> Assert.Fail(error.describe)

        let invalidPaths =
            [
                KitRelativePath "raw"
                KitRelativePath "library"
                KitRelativePath "datax"
                KitRelativePath ".oc/data"
                KitRelativePath "Data"
                KitRelativePath "DATA/raw"
            ]

        for path in invalidPaths do
            match KitDataFolder.tryCreate path with
            | Error (InvalidKitPathErr reason) -> Assert.Contains(path.value, reason)
            | Error error -> Assert.Fail($"expected an invalid data folder: {error.describe}")
            | Ok folder -> Assert.Fail($"accepted an invalid data folder: '{folder.value.value}'")

    [<Fact>]
    let ``a copy keeps its file name and takes the first free numbered suffix, ignoring case`` () : unit =
        let run = FileName "run 1.csv"
        let runPath = KitRelativePath "data/run 1.csv"
        let secondRunPath = KitRelativePath "data/run 1 (2).csv"
        let upperSecondRunPath = KitRelativePath "data/RUN 1 (2).CSV"
        let thirdRunPath = KitRelativePath "data/run 1 (3).csv"
        let copyCases : (FileName * KitRelativePath list * KitRelativePath) list =
            [
                run, [], runPath
                run, [ runPath ], secondRunPath
                run, [ runPath; upperSecondRunPath ], thirdRunPath
                run, [ runPath; thirdRunPath ], secondRunPath
                run, [ secondRunPath; thirdRunPath ], runPath
                run, [ KitRelativePath "DATA/RUN 1.CSV" ], secondRunPath
                FileName "archive.tar.gz", [ KitRelativePath "data/archive.tar.gz" ], KitRelativePath "data/archive.tar (2).gz"
                FileName "README", [ KitRelativePath "data/README" ], KitRelativePath "data/README (2)"
                FileName ".hidden", [ KitRelativePath "data/.hidden" ], KitRelativePath "data/.hidden (2)"
            ]

        for leaf, taken, expected in copyCases do
            match KitDataFolder.root.copyTargetFor leaf taken with
            | Ok path -> Assert.Equal<KitRelativePath>(expected, path)
            | Error error -> Assert.Fail(error.describe)

        let rawPath = KitRelativePath "data/raw"
        let leaf = FileName "a.csv"
        let expectedRawPath = KitRelativePath "data/raw/a.csv"
        let occupiedElsewhere = [ KitRelativePath "data/a.csv" ]
        match KitDataFolder.tryCreate rawPath with
        | Ok folder ->
            match folder.copyTargetFor leaf occupiedElsewhere with
            | Ok path -> Assert.Equal<KitRelativePath>(expectedRawPath, path)
            | Error error -> Assert.Fail(error.describe)
        | Error error -> Assert.Fail(error.describe)

        let invalidLeaf = FileName "C:a.csv"
        match KitDataFolder.root.copyTargetFor invalidLeaf [] with
        | Error (InvalidKitPathErr reason) -> Assert.Contains(invalidLeaf.value, reason)
        | Error error -> Assert.Fail($"expected an invalid copy target: {error.describe}")
        | Ok path -> Assert.Fail($"accepted an invalid copy target: '{path.value}'")

    [<Fact>]
    let ``a file is inside the kit when its absolute name lies under the kit folder`` () : unit =
        let folderName = FolderName "C:/kits/Optics"
        let folder = KitFolder folderName
        let validFiles : (FileName * KitRelativePath) list =
            [
                FileName "C:/kits/Optics/data/run 1.csv", KitRelativePath "data/run 1.csv"
                FileName @"c:\KITS\optics\data\run 1.csv", KitRelativePath "data/run 1.csv"
                FileName "C:/kits/Optics/notes.txt", KitRelativePath "notes.txt"
            ]
        let invalidFiles =
            [
                FileName "C:/kits/Optics"
                FileName "C:/kits/Optics/"
                FileName "C:/kits/OpticsB/data/a.csv"
                FileName "D:/lab/a.csv"
                FileName "C:/kits/Optics/../a.csv"
                FileName "C:/kits/Optics/data/../../a.csv"
                FileName "C:/kits/Optics/data/./a.csv"
                FileName "C:/kits/Optics//a.csv"
                FileName "data/a.csv"
            ]
        let trailingSlashName = FolderName "C:/kits/Optics/"
        let trailingSlashFolder = KitFolder trailingSlashName
        let backslashName = FolderName @"C:\kits\Optics\"
        let backslashFolder = KitFolder backslashName

        for kitFolder in [ folder; trailingSlashFolder; backslashFolder ] do
            for file, expected in validFiles do
                Assert.Equal<KitRelativePath option>(Some expected, kitFolder.tryKitPathOf file)

            for file in invalidFiles do
                Assert.Equal<KitRelativePath option>(None, kitFolder.tryKitPathOf file)

    [<Fact>]
    let ``two spellings of one kit folder share one canonical form`` () : unit =
        let expected = KitFolder (FolderName "C:/KITS/OPTICS QZ")
        let spellings =
            [
                KitFolder (FolderName @"C:\kits\Optics QZ\")
                KitFolder (FolderName "C:/kits/Optics QZ")
                KitFolder (FolderName "c:/KITS/optics qz/")
            ]

        for folder in spellings do
            Assert.Equal<KitFolder>(expected, folder.canonical)

        let canonicals = spellings |> List.map _.canonical |> List.distinct
        Assert.Equal<KitFolder list>([ expected ], canonicals)

        let other = KitFolder (FolderName "C:/kits/Optics QY")
        Assert.NotEqual<KitFolder>(expected, other.canonical)
        Assert.Equal<KitFolder>(KitFolder (FolderName "C:/KITS/OPTICS QY"), other.canonical)

    [<Fact>]
    let ``an attachment records its data file's path and hash, and its status compares that hash with the file now`` () : unit =
        let path = KitRelativePath "data/a.csv"
        let sourceName = FileName "D:/lab/a.csv"
        let originalBytes = FileByteContent (System.Text.Encoding.UTF8.GetBytes "abc")
        let source : LabFileSource =
            {
                file = sourceName
                content = originalBytes
            }
        let originalHash = ContentHash.ofBytes source.content
        let file : KitDataFile =
            {
                path = path
                text = KitFileText "abc"
                contentHash = originalHash
            }
        let attachment = file.attachment

        Assert.Equal<FileName>(sourceName, source.file)
        Assert.Equal<FileByteContent>(originalBytes, source.content)
        Assert.Equal<KitRelativePath>(path, attachment.path)
        Assert.Equal<ContentHash>(originalHash, attachment.contentHash)
        Assert.Equal<DataAttachmentStatus>(AttachedDataUnchanged, attachment.statusAgainst (Some file))

        let changedBytes = FileByteContent (System.Text.Encoding.UTF8.GetBytes "abd")
        let changedHash = ContentHash.ofBytes changedBytes
        let changedFile =
            {
                file with
                    text = KitFileText "abd"
                    contentHash = changedHash
            }
        Assert.Equal<DataAttachmentStatus>(AttachedDataChanged changedHash, attachment.statusAgainst (Some changedFile))
        Assert.Equal<DataAttachmentStatus>(AttachedDataMissing, attachment.statusAgainst None)
        Assert.Equal<ContentHash>(originalHash, attachment.contentHash)

    [<Fact>]
    let ``a setup name keeps its text, refuses a blank one and takes its file stem from the shared name rule`` () : unit =
        match SetupName.tryCreate "CPL-(QZ#45-LR#45)-CPL" with
        | Ok name ->
            Assert.Equal<SetupName>(SetupName "CPL-(QZ#45-LR#45)-CPL", name)
            Assert.Equal<string>("CPL-(QZ#45-LR#45)-CPL", name.value)
            Assert.Equal<FileStem>(FileStem "CPL-(QZ#45-LR#45)-CPL", name.fileStem)
        | Error error -> Assert.Fail(error.describe)

        match SetupName.tryCreate "a: b/c" with
        | Ok name ->
            Assert.Equal<string>("a: b/c", name.value)
            Assert.Equal<FileStem>(FileStem "a_ b_c", name.fileStem)
            Assert.Equal<FileStem>(FileStem.ofText "a: b/c", name.fileStem)
        | Error error -> Assert.Fail(error.describe)

        for text in [ ""; "   " ] do
            match SetupName.tryCreate text with
            | Error (InvalidSetupNameErr reason) -> Assert.False(String.IsNullOrWhiteSpace reason)
            | Error error -> Assert.Fail($"expected an invalid setup name: {error.describe}")
            | Ok name -> Assert.Fail($"accepted a blank setup name: '{name.value}'")

    [<Fact>]
    let ``a setup takes the next free suffix of its name, and a table that is no setup yet takes the first free Setup n`` () : unit =
        let bench = SetupName "Bench"
        Assert.Equal<SetupName>(SetupName "Bench (3)", bench.nextFreeAmong [ SetupName "bench"; SetupName "Bench (2)" ])
        Assert.Equal<SetupName>(SetupName "Bench", bench.nextFreeAmong [])
        Assert.Equal<SetupName>(SetupName "a:b (2)", (SetupName "a:b").nextFreeAmong [ SetupName "a_b" ])

        Assert.Equal<SetupName>(SetupName "Setup 3", SetupName.firstFree [ SetupName "Setup 1"; SetupName "setup 2" ])
        Assert.Equal<SetupName>(SetupName "Setup 1", SetupName.firstFree [])

    [<Fact>]
    let ``a setup revision starts at one and counts up, and a reference names its setup and revision`` () : unit =
        Assert.Equal<SetupRevision>(SetupRevision 1, SetupRevision.first)
        Assert.Equal<SetupRevision>(SetupRevision 3, SetupRevision.first.next.next)

        let reference : SetupReference =
            {
                name = SetupName "Bench"
                revision = SetupRevision.first.next
            }

        Assert.Equal<string>("'Bench' revision 2", reference.describe)

    [<Fact>]
    let ``every setup error renders its condition and its payload`` () : unit =
        let cases : (SetupError * string) list =
            [
                InvalidSetupNameErr "'   ': blank", "the setup name is not valid: '   ': blank"
                DuplicateSetupNameErr "'bench' beside 'Bench'", "the kit already has a setup of that name: 'bench' beside 'Bench'"
                UnknownSetupErr "'Missing bench'", "the kit has no such setup: 'Missing bench'"
                UnknownSetupRevisionErr "'Bench' revision 7", "the setup has no such revision: 'Bench' revision 7"
                UnknownSetupSlotErr "element 12", "the table has no such slot: element 12"
                UnknownSlotItemErr "item QZ9", "the item is not available: item QZ9"
                IncompatibleSlotItemErr "LP1 in a sample slot", "the item does not fit the slot: LP1 in a sample slot"
            ]

        for error, expected in cases do
            Assert.Equal<string>(expected, error.describe)

        for error, _ in cases do
            let reason =
                match error with
                | InvalidSetupNameErr reason
                | DuplicateSetupNameErr reason
                | UnknownSetupErr reason
                | UnknownSetupRevisionErr reason
                | UnknownSetupSlotErr reason
                | UnknownSlotItemErr reason
                | IncompatibleSlotItemErr reason -> reason
                | SetupItemReferenceErr error -> error.describe

            Assert.Contains(reason, error.describe)

        let descriptions = cases |> List.map (fun (error, _) -> error.describe)
        Assert.Equal(7, descriptions |> List.distinct |> List.length)
