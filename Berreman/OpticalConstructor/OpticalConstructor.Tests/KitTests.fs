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
    let ``every kit error renders its condition and its payload`` () : unit =
        let cases : (KitError * string) list =
            [
                KitIoErr "unreadable folder", "a kit file could not be read or written: unreadable folder"
                NotAKitFolderErr "missing folder", "the folder is not a kit: missing folder"
                KitAlreadyExistsErr "existing folder", "the folder is already a kit: existing folder"
                KitFileRejectedErr "invalid file", "a kit file was refused: invalid file"
                InvalidKitNameErr "blank name", "the kit name is not valid: blank name"
            ]

        for error, expected in cases do
            Assert.Equal<string>(expected, error.describe)

        let descriptions = cases |> List.map (fun (error, _) -> error.describe)
        Assert.Equal(5, descriptions |> List.distinct |> List.length)
