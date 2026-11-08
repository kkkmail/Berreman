namespace OpticalConstructor.Tests

open System
open System.IO
open Xunit
open Softellect.Sys.Primitives
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Placement
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.Lifecycle
open OpticalConstructor.Domain.Experiments
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.KitFolders
open OpticalConstructor.Domain.Scene
open OpticalConstructor.Storage.KitContentFiles
open OpticalConstructor.Storage.KitFolderStore
open OpticalConstructor.Ui.TableAndElementRotationView
open KitContentRoundTripFixture

/// Experiment files preserve full, sample-less and dark setups with data attachments in initialized
/// kit folders under the test output directory (spec 0038 step 029; spec 0050 S4.2 and S4.3).
module ExperimentCollectionRoundTripTests =

    let private classFolder = FolderName "ExperimentCollectionRoundTripTests"

    let private attachmentHash : ContentHash = ContentHash "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"

    let private e1Attachment : DataAttachment =
        {
            path = KitRelativePath "data/e1-transmitted.csv"
            contentHash = attachmentHash
        }

    let private e3Attachment : DataAttachment =
        {
            path = KitRelativePath "data/e3-dark.csv"
            contentHash = attachmentHash
        }

    let private el (s : string) : ElementId = Library.ElementId s

    let private placementOf (kind : CatalogueKind) : PlacementSummary =
        PlacementSummary.ofPlacement (ElementPlacement.create kind TablePoint.origin)

    let private desc (idStr : string) (label : string) (kind : CatalogueKind) (binding : ElementBinding) : ElementDescriptor =
        {
            elementId = el idStr
            label = ElementLabel label
            kind = kind
            placement = placementOf kind
            binding = binding
        }

    let private glassPlateBinding : ElementBinding =
        BoundByVersion (SampleVersionRef (SampleVersionId.firstOf SampleIds.glassPlate1mmId))

    let private e1 : Experiment =
        {
            id = ExperimentId 1
            setup =
                [
                    desc "src" "Light source #1" LightSource (BoundByEntryId PresetIds.source600)
                    desc "lp1" "Linear polarizer #2" LinearPolarizer (BoundByEntryId PresetIds.polarizerLp)
                    desc "s" "Sample #3" Sample glassPlateBinding
                    desc "lp2" "Linear polarizer #4" LinearPolarizer (BoundByEntryId PresetIds.polarizerLp)
                    desc "det" "Detector #5" Detector (BoundByEntryId PresetIds.detectorIntensity)
                ]
            varied =
                Some
                    {
                        elementId = el "lp2"
                        variable = VaryR1
                    }
            measurement = CaptureTransmitted
            range = VariableRange.forVariable VaryR1
            dataFileOpt = Some e1Attachment
            sourceWaveLengthOpt = None
        }

    let private e2 : Experiment =
        {
            e1 with
                id = ExperimentId 2
                setup =
                    [
                        desc "src" "Light source #1" LightSource (BoundByEntryId PresetIds.source600)
                        desc "lp1" "Linear polarizer #2" LinearPolarizer (BoundByEntryId PresetIds.polarizerLp)
                        desc "lp2" "Linear polarizer #3" LinearPolarizer (BoundByEntryId PresetIds.polarizerLp)
                        desc "det" "Detector #4" Detector (BoundByEntryId PresetIds.detectorIntensity)
                    ]
                dataFileOpt = None
                sourceWaveLengthOpt = None
        }

    let private e3 : Experiment =
        {
            id = ExperimentId 3
            setup = []
            varied = None
            measurement = CaptureTransmitted
            range = VariableRange.forVariable VaryR1
            dataFileOpt = Some e3Attachment
            sourceWaveLengthOpt = None
        }

    [<Fact>]
    let ``a kit's experiment files round-trip E1 (full) / E2 (sample-less) / E3 (dark), preserving ordered setups, optional shapes and data attachments`` () : unit =
        let folder, proxy = createContentTestKit classFolder
        for experiment in [ e3; e1; e2 ] do
            proxy.writeExperiment experiment |> kitContentValue

        let reader = KitProxy.create folder
        let loaded = reader.readExperiments () |> kitContentValue
        Assert.Equal<Experiment list>([ e1; e2; e3 ], loaded)
        for file in [ FileName "E1.experiment-json"; FileName "E2.experiment-json"; FileName "E3.experiment-json" ] do
            Assert.True(File.Exists((file.combine folder.value).value))
        Assert.Equal<CatalogueKind list>(
            [ LightSource; LinearPolarizer; Sample; LinearPolarizer; Detector ],
            (List.item 0 loaded).setup |> List.map (fun descriptor -> descriptor.kind))
        Assert.Equal<CatalogueKind list>(
            [ LightSource; LinearPolarizer; LinearPolarizer; Detector ],
            (List.item 1 loaded).setup |> List.map (fun descriptor -> descriptor.kind))
        Assert.Empty((List.item 2 loaded).setup)
        Assert.Contains(
            SampleVersionRef (SampleVersionId.firstOf SampleIds.glassPlate1mmId),
            (List.item 0 loaded).versionRefs)
        Assert.Empty((List.item 1 loaded).versionRefs)
        Assert.Equal(None, (List.item 2 loaded).varied)
        Assert.Equal<DataAttachment option list>(
            [ Some e1Attachment; None; Some e3Attachment ],
            loaded |> List.map (fun experiment -> experiment.dataFileOpt))

        // Spec 0050 S4.3: identity order differs from filename order, and stem matching ignores case.
        let tenth =
            {
                e3 with id = ExperimentId 10
            }
        let lowerFile = FileName "e10.experiment-json"
        let text = tenth.experimentFile.fileText |> Result.defaultWith (fun error -> failwith error.describe)
        File.WriteAllText((lowerFile.combine folder.value).value, text.value)
        Assert.Equal<Experiment list>([ e1; e2; e3; tenth ], reader.readExperiments () |> kitContentValue)

    [<Fact>]
    let ``writing an experiment again replaces its kit file`` () : unit =
        let folder, proxy = createContentTestKit classFolder
        let second =
            {
                e1 with measurement = CaptureReflected
            }
        proxy.writeExperiment e1 |> kitContentValue
        proxy.writeExperiment second |> kitContentValue

        let reader = KitProxy.create folder
        Assert.Equal<Experiment list>([ second ], reader.readExperiments () |> kitContentValue)
        Assert.Equal<FileStem list>([ e1.id.fileStem ], reader.listKindFiles ExperimentKitFile |> kitContentValue)

        let invalidRange =
            {
                second.range with min = Propagation.SweptValue Double.NaN
            }
        let invalidExperiment =
            {
                second with range = invalidRange
            }
        let storedText = reader.readKindFile ExperimentKitFile e1.id.fileStem |> kitContentValue
        match proxy.writeExperiment invalidExperiment with
        | Error (KitFileRejectedErr reason) -> Assert.Contains("E1.experiment-json", reason)
        | Error error -> Assert.Fail($"Expected KitFileRejectedErr, got {error.describe}")
        | Ok () -> Assert.Fail("An experiment that cannot be rendered must refuse the write.")
        Assert.Equal<KitFileText>(storedText, reader.readKindFile ExperimentKitFile e1.id.fileStem |> kitContentValue)

    [<Fact>]
    let ``a kit with no experiment files reads no experiments`` () : unit =
        let _, proxy = createContentTestKit classFolder
        Assert.Equal<Result<Experiment list, KitError>>(Ok [], proxy.readExperiments ())

    [<Fact>]
    let ``deleting an experiment removes its kit file and keeps the others`` () : unit =
        let folder, proxy = createContentTestKit classFolder
        for experiment in [ e1; e2; e3 ] do
            proxy.writeExperiment experiment |> kitContentValue
        let reader = KitProxy.create folder
        Assert.Equal<Experiment list>([ e1; e2; e3 ], reader.readExperiments () |> kitContentValue)

        proxy.deleteExperiment e2.id |> kitContentValue
        Assert.Equal<Experiment list>([ e1; e3 ], reader.readExperiments () |> kitContentValue)
        let deletedFile = FileName "E2.experiment-json"
        Assert.False(File.Exists((deletedFile.combine folder.value).value))
        match proxy.deleteExperiment e2.id with
        | Error (MissingKitFileErr reason) -> Assert.Contains(deletedFile.value, reason)
        | Error error -> Assert.Fail($"Expected MissingKitFileErr, got {error.describe}")
        | Ok () -> Assert.Fail("Deleting an absent experiment must report the missing file.")
        Assert.Equal<Experiment list>([ e1; e3 ], reader.readExperiments () |> kitContentValue)

    [<Fact>]
    let ``an experiment file that does not read, or holds another id, refuses the read, naming the file`` () : unit =
        let e1Text = e1.experimentFile.fileText |> Result.defaultWith (fun error -> failwith error.describe)
        let invalidFiles =
            [ FileName "E5.experiment-json", KitFileText "{"
              FileName "E6.experiment-json", e1Text ]
        for file, text in invalidFiles do
            let folder, proxy = createContentTestKit classFolder
            proxy.writeExperiment e1 |> kitContentValue
            File.WriteAllText((file.combine folder.value).value, text.value)
            match proxy.readExperiments () with
            | Error (KitFileRejectedErr reason) ->
                Assert.Contains(file.value, reason)
                Assert.False(String.IsNullOrWhiteSpace reason)
                if file.value = "E6.experiment-json" then
                    Assert.Contains("E1", reason)
            | Error error -> Assert.Fail($"Expected KitFileRejectedErr, got {error.describe}")
            | Ok _ -> Assert.Fail("An unreadable or misnamed experiment must refuse the whole read.")

        let folder, proxy = createContentTestKit classFolder
        for experiment in [ e1; e2 ] do
            proxy.writeExperiment experiment |> kitContentValue
        let lockedFile = FileName "E2.experiment-json"
        do
            use _held = File.Open((lockedFile.combine folder.value).value, FileMode.Open, FileAccess.ReadWrite, FileShare.None)
            match proxy.readExperiments () with
            | Error (KitFileRejectedErr reason) -> Assert.Contains(lockedFile.value, reason)
            | Error error -> Assert.Fail($"Expected KitFileRejectedErr, got {error.describe}")
            | Ok _ -> Assert.Fail("An experiment that cannot be opened must refuse the whole read.")
        Assert.Equal<Experiment list>([ e1; e2 ], proxy.readExperiments () |> kitContentValue)

    [<Fact>]
    let ``a read-only kit refuses experiment and scene writes and writes nothing`` () : unit =
        let folder, proxy = createContentTestKit classFolder
        for experiment in [ e1; e2 ] do
            proxy.writeExperiment experiment |> kitContentValue
        let name = SceneName "Read-only bench"
        let newName = SceneName "New bench"
        let scene = (initTableAndElementRotation ()).captureScene
        proxy.writeScene name scene |> kitContentValue
        let changedScene =
            {
                scene with snap = SnapToBeam
            }
        let changedExperiment =
            {
                e1 with measurement = CaptureReflected
            }
        let storedFiles () : Map<FileName, FileByteContent> =
            Directory.GetFiles(folder.value.value, "*", SearchOption.AllDirectories)
            |> Array.map (fun path ->
                let file = FileName (Path.GetRelativePath(folder.value.value, path))
                let bytes = File.ReadAllBytes path |> FileByteContent
                file, bytes)
            |> Map.ofArray
        let before = storedFiles ()
        let reader = proxy.readOnlyView
        let writes =
            [ (fun () -> reader.writeExperiment changedExperiment)
              (fun () -> reader.writeExperiment e3)
              (fun () -> reader.deleteExperiment e2.id)
              (fun () -> reader.writeScene name changedScene)
              (fun () -> reader.writeScene newName changedScene) ]
        for write in writes do
            match write () with
            | Error (KitReadOnlyErr reason) -> Assert.False(String.IsNullOrWhiteSpace reason)
            | Error error -> Assert.Fail($"Expected KitReadOnlyErr, got {error.describe}")
            | Ok () -> Assert.Fail("A read-only kit must refuse every content write.")
            let after = storedFiles ()
            Assert.Equal<FileName list>(before |> Map.keys |> Seq.toList, after |> Map.keys |> Seq.toList)
            for KeyValue(file, bytes) in before do
                Assert.Equal<byte>(bytes.value, (Map.find file after).value)
        Assert.Equal<Experiment list>([ e1; e2 ], reader.readExperiments () |> kitContentValue)
        Assert.Equal<SceneName list>([ name ], reader.readSceneNames () |> kitContentValue)
        Assert.Equal<SceneSnapshot>(scene, reader.readScene name |> kitContentValue)
