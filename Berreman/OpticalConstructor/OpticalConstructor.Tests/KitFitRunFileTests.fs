namespace OpticalConstructor.Tests

open System
open System.IO
open Xunit
open Softellect.Sys.Primitives
open OpticalConstructor.Domain.ItemModels
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.KitFolders
open OpticalConstructor.Domain.Studies
open OpticalConstructor.Domain.FitRuns
open OpticalConstructor.Storage.KitContentFiles
open OpticalConstructor.Storage.KitFolderStore
open KitContentRoundTripFixture
open FitRunFileFixtures

/// A kit's append-only `<study>@<stamp>.fitrun-json` files written and read through `KitProxy` in initialized kit
/// folders under the test output directory (spec 0050 S11.3).
module KitFitRunFileTests =

    let private classFolder = FolderName "KitFitRunFileTests"

    /// QZ-LR's second fit run, an hour after its first.
    let private qzLrSecondRun : FitRun = fitRunOf (StudyName "QZ-LR") (ClockTime (DateTime(2026, 11, 1, 13, 0, 0, DateTimeKind.Utc)))

    /// LR's one fit run, stamped between QZ-LR's two.
    let private lrRun : FitRun = fitRunOf (StudyName "LR") (ClockTime (DateTime(2026, 11, 1, 12, 30, 0, DateTimeKind.Utc)))

    let private fitRunFileTextOf (run : FitRun) : KitFileText =
        run.fileText |> Result.defaultWith (fun error -> failwith error.describe)

    let private storedFitRunFiles (folder : KitFolder) : FileName list =
        Directory.GetFiles(folder.value.value, $"*{FitRunKitFile.extension.value}", SearchOption.TopDirectoryOnly)
        |> Array.map (fun path -> FileName (Path.GetFileName path))
        |> Array.sortWith (fun first second -> String.CompareOrdinal(first.value, second.value))
        |> Array.toList

    let private folderBytes (folder : KitFolder) : Map<FileName, FileByteContent> =
        Directory.GetFiles(folder.value.value, "*", SearchOption.AllDirectories)
        |> Array.map (fun path ->
            let name = FileName (Path.GetRelativePath(folder.value.value, path))
            let content = FileByteContent (File.ReadAllBytes path)
            name, content)
        |> Map.ofArray

    [<Fact>]
    let ``a kit's fit runs round-trip, each under its name, read per study in stamp order`` () : unit =
        let folder, proxy = createContentTestKit classFolder
        for run in [ qzLrSecondRun; lrRun; qzLrFitRun ] do
            proxy.writeFitRun run |> kitContentValue

        let expectedFiles =
            [
                FileName "LR@20261101T123000Z.fitrun-json"
                FileName "QZ-LR@20261101T120000Z.fitrun-json"
                FileName "QZ-LR@20261101T130000Z.fitrun-json"
            ]
        Assert.Equal<FileName list>(expectedFiles, storedFitRunFiles folder)

        let reader = KitProxy.create folder
        Assert.Equal<FitRun list>([ qzLrFitRun; qzLrSecondRun ], reader.readFitRuns (StudyName "QZ-LR") |> kitContentValue)
        Assert.Equal<FitRun list>([ lrRun ], reader.readFitRuns (StudyName "LR") |> kitContentValue)
        Assert.Equal<FitRun list>([], reader.readFitRuns (StudyName "QZ") |> kitContentValue)
        for run in [ qzLrFitRun; qzLrSecondRun; lrRun ] do
            Assert.Equal<KitFileText>(fitRunFileTextOf run, reader.readKindFile FitRunKitFile run.name.fileStem |> kitContentValue)

        // A file that does not read, or holds a run of another name, refuses every study's read, naming the file.
        let invalidFiles : (FileName * KitFileText) list =
            [
                FileName "Broken@20261101T140000Z.fitrun-json", KitFileText "{"
                FileName "Other@20261101T140000Z.fitrun-json", fitRunFileTextOf qzLrFitRun
            ]

        for file, text in invalidFiles do
            let path = file.combine folder.value
            File.WriteAllText(path.value, text.value)
            match reader.readFitRuns (StudyName "LR") with
            | Error (KitFileRejectedErr reason) -> Assert.Contains(file.value, reason)
            | Error error -> Assert.Fail($"Expected KitFileRejectedErr naming '{file.value}', got {error.describe}")
            | Ok _ -> Assert.Fail($"A kit holding '{file.value}' must refuse the whole read.")

            File.Delete path.value
            Assert.Equal<FitRun list>([ lrRun ], reader.readFitRuns (StudyName "LR") |> kitContentValue)

    [<Fact>]
    let ``a fit run is never rewritten: writing its name again is FitRunExistsErr and leaves the file's bytes`` () : unit =
        let folder, proxy = createContentTestKit classFolder
        proxy.writeFitRun qzLrFitRun |> kitContentValue
        let file = FitRunKitFile.fileNameOf qzLrFitRun.name.fileStem
        let path = file.combine folder.value
        let before = FileByteContent (File.ReadAllBytes path.value)

        let otherEntries : FitRun =
            {
                qzLrFitRun with
                    entries = qzLrFitRun.entries |> List.map (fun entry -> { entry with fitted = ParameterValue 0.5 })
            }

        // A name differing in case alone is the same file's name.
        let otherCase : FitRun =
            {
                qzLrFitRun with
                    name = FitRunName "qz-lr@20261101T120000Z"
            }

        for run in [ qzLrFitRun; otherEntries; otherCase ] do
            match proxy.writeFitRun run with
            | Error (FitRunExistsErr reason) -> Assert.Contains(file.value, reason)
            | Error error -> Assert.Fail($"Expected FitRunExistsErr for '{run.name.value}', got {error.describe}")
            | Ok () -> Assert.Fail($"A kit already holding '{file.value}' must refuse writing '{run.name.value}'.")

        Assert.Equal<FileByteContent>(before, FileByteContent (File.ReadAllBytes path.value))
        Assert.Equal<FileName list>([ file ], storedFitRunFiles folder)
        Assert.Equal<FitRun list>([ qzLrFitRun ], proxy.readFitRuns (StudyName "QZ-LR") |> kitContentValue)

    [<Fact>]
    let ``a read-only kit refuses a fit run write and writes nothing`` () : unit =
        let folder, proxy = createContentTestKit classFolder
        proxy.writeFitRun qzLrFitRun |> kitContentValue
        let before = folderBytes folder
        let reader = proxy.readOnlyView

        for run in [ qzLrSecondRun; lrRun ] do
            match reader.writeFitRun run with
            | Error (KitReadOnlyErr _) -> ()
            | Error error -> Assert.Fail($"Expected KitReadOnlyErr for '{run.name.value}', got {error.describe}")
            | Ok () -> Assert.Fail($"A read-only kit must refuse writing '{run.name.value}'.")

        Assert.Equal<Map<FileName, FileByteContent>>(before, folderBytes folder)
        Assert.Equal<FitRun list>([ qzLrFitRun ], reader.readFitRuns (StudyName "QZ-LR") |> kitContentValue)
        Assert.Equal<FitRun list>([], reader.readFitRuns (StudyName "LR") |> kitContentValue)
