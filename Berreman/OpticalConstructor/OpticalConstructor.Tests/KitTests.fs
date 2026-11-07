namespace OpticalConstructor.Tests

open System
open Xunit
open Softellect.Sys.Primitives
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.Kit

module KitTests =

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
    let ``the kit file is named by its extension alone`` () : unit =
        let expectedExtension = FileExtension ".kit-json"
        let expectedFileName = FileName ".kit-json"
        Assert.Equal<FileExtension>(expectedExtension, KitFile.extension)
        Assert.Equal<FileName>(expectedFileName, KitFile.fileName)

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
            ]

        for error, expected in cases do
            Assert.Equal<string>(expected, error.describe)

        let descriptions = cases |> List.map (fun (error, _) -> error.describe)
        Assert.Equal(7, descriptions |> List.distinct |> List.length)
