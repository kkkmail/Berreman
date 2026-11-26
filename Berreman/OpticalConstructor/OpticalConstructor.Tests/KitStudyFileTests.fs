namespace OpticalConstructor.Tests

open System
open System.IO
open Xunit
open Softellect.Sys.Primitives
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.KitFolders
open OpticalConstructor.Domain.Studies
open OpticalConstructor.Storage.KitContentFiles
open OpticalConstructor.Storage.KitFolderStore
open KitContentRoundTripFixture
open StudyFileFixtures

/// A kit's `<name>.study-json` files written and read through `KitProxy` in initialized kit folders under the test
/// output directory (spec 0050 S11.1).
module KitStudyFileTests =

    let private classFolder = FolderName "KitStudyFileTests"

    let private qzLr : Study = qzLrStudyOver studyProbeMaterial

    /// A second study of the kit, whose name sorts before QZ-LR's.
    let private lr : Study =
        {
            qzLr with
                name = StudyName "LR"
                notes = StudyNotes "LR alone, before QZ is inserted."
        }

    let private studyFileTextOf (study : Study) : KitFileText =
        study.studyFile.fileText |> Result.defaultWith (fun error -> failwith error.describe)

    let private storedStudyFiles (folder : KitFolder) : FileName list =
        Directory.GetFiles(folder.value.value, $"*{StudyKitFile.extension.value}", SearchOption.TopDirectoryOnly)
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
    let ``a kit's study files round-trip, each stored under its name`` () : unit =
        let folder, proxy = createContentTestKit classFolder
        for study in [ qzLr; lr ] do
            proxy.writeStudy study |> kitContentValue

        Assert.Equal<FileName list>([ FileName "LR.study-json"; FileName "QZ-LR.study-json" ], storedStudyFiles folder)

        let reader = KitProxy.create folder
        Assert.Equal<Study list>([ lr; qzLr ], reader.readStudies () |> kitContentValue)
        for study in [ lr; qzLr ] do
            Assert.Equal<KitFileText>(studyFileTextOf study, reader.readKindFile StudyKitFile study.name.fileStem |> kitContentValue)

    [<Fact>]
    let ``a study file that does not read or holds another name refuses the read, naming the file`` () : unit =
        let invalidFiles : (FileName * KitFileText) list =
            [
                FileName "Broken.study-json", KitFileText "{"
                FileName "Other.study-json", studyFileTextOf qzLr
            ]

        for file, text in invalidFiles do
            let folder, proxy = createContentTestKit classFolder
            proxy.writeStudy lr |> kitContentValue
            let path = file.combine folder.value
            File.WriteAllText(path.value, text.value)
            match proxy.readStudies () with
            | Error (KitFileRejectedErr reason) ->
                Assert.Contains(file.value, reason)
                if file.value = "Other.study-json" then
                    Assert.Contains("'QZ-LR'", reason)
                    Assert.Contains("'Other'", reason)
            | Error error -> Assert.Fail($"Expected KitFileRejectedErr naming '{file.value}', got {error.describe}")
            | Ok _ -> Assert.Fail($"A kit holding '{file.value}' must refuse the whole read.")

            // The refusal is that file's alone: without it the kit reads its study again.
            File.Delete path.value
            Assert.Equal<Study list>([ lr ], proxy.readStudies () |> kitContentValue)

    [<Fact>]
    let ``a read-only kit refuses a study write and writes nothing`` () : unit =
        let folder, proxy = createContentTestKit classFolder
        proxy.writeStudy lr |> kitContentValue
        let before = folderBytes folder
        let reader = proxy.readOnlyView
        let renoted =
            {
                lr with notes = StudyNotes "LR, re-noted through a read-only kit."
            }

        for study in [ qzLr; renoted ] do
            match reader.writeStudy study with
            | Error (KitReadOnlyErr _) -> ()
            | Error error -> Assert.Fail($"Expected KitReadOnlyErr for '{study.name.value}', got {error.describe}")
            | Ok () -> Assert.Fail($"A read-only kit must refuse writing '{study.name.value}'.")

        Assert.Equal<Map<FileName, FileByteContent>>(before, folderBytes folder)
        Assert.Equal<Study list>([ lr ], reader.readStudies () |> kitContentValue)
