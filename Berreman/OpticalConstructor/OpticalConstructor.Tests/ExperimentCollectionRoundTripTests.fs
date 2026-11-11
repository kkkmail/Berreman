namespace OpticalConstructor.Tests

open System
open System.IO
open Xunit
open Softellect.Sys.Primitives
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Placement
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.Experiments
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.KitFolders
open OpticalConstructor.Storage.KitContentFiles
open OpticalConstructor.Storage.KitFolderStore
open OpticalConstructor.Ui.TableAndElementRotationView
open OpticalConstructor.Domain.Setups
open KitContentRoundTripFixture

/// Experiment files preserve full, sample-less and dark runs, their setup references and data attachments in
/// initialized kit folders under the test output directory (spec 0038 step 029; spec 0050 S4.2, S4.3, S8.2).
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

    /// A rest-pose slot of that kind, bound to `valueIdOpt`, holding no physical item.
    let private emptySlot (idStr : string) (kind : CatalogueKind) (valueIdOpt : ElementValueId option) : SetupSlot =
        {
            id = el idStr
            placement =
                {
                    ElementPlacement.create kind TablePoint.origin with valueId = valueIdOpt
                }
            zoom = BeamLayout.ElementZoom 1.0
            beam = BeamLayout.OnMainBeam
            content = EmptySlotContent
        }

    /// E1's two linear polarizers share one preset and are two physical pieces (spec 0050 S7.2).
    let private lp1ItemId : ItemId = ItemId (Guid.Parse "41b7c2e9-3a5d-4f08-9c61-2e8d7b0a5f13")

    let private lp2ItemId : ItemId = ItemId (Guid.Parse "92d4e8a1-6c3f-4b27-8e05-7f1a9c3b2d64")

    let private pieceAt (itemId : ItemId) (slot : SetupSlot) : SetupSlot =
        {
            slot with content = ItemInSlot (itemId, PlacementTurn)
        }

    let private glassPlateValueId : ElementValueId = SampleIds.glassPlate1mmId.toElementValueId

    /// `Bench` revision 1 is E1's full chain with its two pieces, revision 2 E2's sample-less chain (spec 0050 S8.2).
    let private bench : Setup =
        let fullChain : SetupRevisionRecord =
            {
                revision = SetupRevision 1
                slots =
                    [
                        emptySlot "src" LightSource (Some PresetIds.source600)
                        pieceAt lp1ItemId (emptySlot "lp1" LinearPolarizer (Some PresetIds.polarizerLp))
                        emptySlot "s" Sample (Some glassPlateValueId)
                        pieceAt lp2ItemId (emptySlot "lp2" LinearPolarizer (Some PresetIds.polarizerLp))
                        emptySlot "det" Detector (Some PresetIds.detectorIntensity)
                    ]
            }
        {
            name = SetupName "Bench"
            revision = SetupRevision 2
            slots =
                [
                    emptySlot "src" LightSource (Some PresetIds.source600)
                    emptySlot "lp1" LinearPolarizer (Some PresetIds.polarizerLp)
                    emptySlot "lp2" LinearPolarizer (Some PresetIds.polarizerLp)
                    emptySlot "det" Detector (Some PresetIds.detectorIntensity)
                ]
            history = [ fullChain ]
        }

    /// The dark line's setup holds no slot (spec 0050 S8.2).
    let private dark : Setup =
        {
            name = SetupName "Dark"
            revision = SetupRevision.first
            slots = []
            history = []
        }

    /// The rotating polarizer over R1's default range, under the label the run captured.
    let private analyzerTurn (label : ElementLabel) : ScanAxis =
        {
            elementId = el "lp2"
            label = label
            variable = VaryR1
            range = VariableRange.forVariable VaryR1
        }

    let private e1 : Experiment =
        {
            id = ExperimentId 1
            name = (ExperimentId 1).defaultName
            setup = { bench.reference with revision = SetupRevision 1 }
            scan = ExperimentScan.oneAxis (analyzerTurn (ElementLabel "Linear polarizer #4")) CaptureTransmitted None
            role = ScienceRun
            sessionOpt = None
            notes = ExperimentNotes.none
            dataOpt =
                Some
                    {
                        attachment = e1Attachment
                        profile = ImportProfileName.intensitySeries
                        rows = AllDataRows
                    }
        }

    let private e2 : Experiment =
        {
            e1 with
                id = ExperimentId 2
                name = (ExperimentId 2).defaultName
                setup = bench.reference
                scan = ExperimentScan.oneAxis (analyzerTurn (ElementLabel "Linear polarizer #3")) CaptureTransmitted None
                dataOpt = None
        }

    let private e3 : Experiment =
        {
            id = ExperimentId 3
            name = (ExperimentId 3).defaultName
            setup = dark.reference
            scan = ExperimentScan.darkLine CaptureTransmitted
            role = ScienceRun
            sessionOpt = None
            notes = ExperimentNotes.none
            dataOpt =
                Some
                    {
                        attachment = e3Attachment
                        profile = ImportProfileName.intensitySeries
                        rows = AllDataRows
                    }
        }

    [<Fact>]
    let ``a kit's experiment files round-trip E1 (full) / E2 (sample-less) / E3 (dark), preserving their setup references, optional shapes and data attachments`` () : unit =
        let folder, proxy = createContentTestKit classFolder
        for setup in [ bench; dark ] do
            proxy.writeSetup setup |> kitContentValue
        for experiment in [ e3; e1; e2 ] do
            proxy.writeExperiment experiment |> kitContentValue

        let reader = KitProxy.create folder
        let loaded = reader.readExperiments () |> kitContentValue
        let setups = reader.readSetups () |> kitContentValue
        let slotsOf (experiment : Experiment) : SetupSlot list =
            experiment.slotsIn setups |> Result.defaultWith (fun error -> failwith error.describe)
        Assert.Equal<Experiment list>([ e1; e2; e3 ], loaded)
        for file in [ FileName "E1.experiment-json"; FileName "E2.experiment-json"; FileName "E3.experiment-json" ] do
            Assert.True(File.Exists((file.combine folder.value).value))
        Assert.Equal<SetupReference list>(
            [ { name = SetupName "Bench"; revision = SetupRevision 1 }; { name = SetupName "Bench"; revision = SetupRevision 2 }; { name = SetupName "Dark"; revision = SetupRevision 1 } ],
            loaded |> List.map (fun experiment -> experiment.setup))
        Assert.Equal<CatalogueKind list>(
            [ LightSource; LinearPolarizer; Sample; LinearPolarizer; Detector ],
            slotsOf (List.item 0 loaded) |> List.map (fun slot -> slot.placement.catalogueKind))
        Assert.Equal<CatalogueKind list>(
            [ LightSource; LinearPolarizer; LinearPolarizer; Detector ],
            slotsOf (List.item 1 loaded) |> List.map (fun slot -> slot.placement.catalogueKind))
        Assert.Empty(slotsOf (List.item 2 loaded))
        Assert.Contains(Some glassPlateValueId, slotsOf (List.item 0 loaded) |> List.map (fun slot -> slot.placement.valueId))
        Assert.DoesNotContain(Some glassPlateValueId, slotsOf (List.item 1 loaded) |> List.map (fun slot -> slot.placement.valueId))
        Assert.Empty((List.item 2 loaded).scan.axes)
        Assert.Equal<ItemId option list>(
            [ None; Some lp1ItemId; None; Some lp2ItemId; None ],
            slotsOf (List.item 0 loaded) |> List.map (fun slot -> slot.content.itemOpt))
        Assert.Equal<ItemId option list>(
            [ None; None; None; None ],
            slotsOf (List.item 1 loaded) |> List.map (fun slot -> slot.content.itemOpt))
        Assert.Equal<DataAttachment option list>(
            [ Some e1Attachment; None; Some e3Attachment ],
            loaded |> List.map (fun experiment -> experiment.dataOpt |> Option.map (fun data -> data.attachment)))

        // Spec 0050 S4.3: identity order differs from filename order, and stem matching ignores case.
        let tenth =
            {
                e3 with
                    id = ExperimentId 10
                    name = (ExperimentId 10).defaultName
            }
        let lowerFile = FileName "e10.experiment-json"
        let text = tenth.experimentFile.fileText |> Result.defaultWith (fun error -> failwith error.describe)
        File.WriteAllText((lowerFile.combine folder.value).value, text.value)
        Assert.Equal<Experiment list>([ e1; e2; e3; tenth ], reader.readExperiments () |> kitContentValue)

    [<Fact>]
    let ``writing an experiment again replaces its kit file`` () : unit =
        let folder, proxy = createContentTestKit classFolder
        let e1Turn = analyzerTurn (ElementLabel "Linear polarizer #4")
        let second =
            {
                e1 with scan = ExperimentScan.oneAxis e1Turn CaptureReflected None
            }
        proxy.writeExperiment e1 |> kitContentValue
        proxy.writeExperiment second |> kitContentValue

        let reader = KitProxy.create folder
        Assert.Equal<Experiment list>([ second ], reader.readExperiments () |> kitContentValue)
        Assert.Equal<FileStem list>([ e1.name.fileStem ], reader.listKindFiles ExperimentKitFile |> kitContentValue)

        let invalidRange =
            {
                e1Turn.range with min = Propagation.SweptValue Double.NaN
            }
        let invalidExperiment =
            {
                second with scan = ExperimentScan.oneAxis { e1Turn with range = invalidRange } CaptureReflected None
            }
        let storedText = reader.readKindFile ExperimentKitFile e1.name.fileStem |> kitContentValue
        match proxy.writeExperiment invalidExperiment with
        | Error (KitFileRejectedErr reason) -> Assert.Contains("E1.experiment-json", reason)
        | Error error -> Assert.Fail($"Expected KitFileRejectedErr, got {error.describe}")
        | Ok () -> Assert.Fail("An experiment that cannot be rendered must refuse the write.")
        Assert.Equal<KitFileText>(storedText, reader.readKindFile ExperimentKitFile e1.name.fileStem |> kitContentValue)

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

        proxy.deleteExperiment e2.name |> kitContentValue
        Assert.Equal<Experiment list>([ e1; e3 ], reader.readExperiments () |> kitContentValue)
        let deletedFile = FileName "E2.experiment-json"
        Assert.False(File.Exists((deletedFile.combine folder.value).value))
        match proxy.deleteExperiment e2.name with
        | Error (MissingKitFileErr reason) -> Assert.Contains(deletedFile.value, reason)
        | Error error -> Assert.Fail($"Expected MissingKitFileErr, got {error.describe}")
        | Ok () -> Assert.Fail("Deleting an absent experiment must report the missing file.")
        Assert.Equal<Experiment list>([ e1; e3 ], reader.readExperiments () |> kitContentValue)

    [<Fact>]
    let ``an experiment file that does not read, or holds another name, refuses the read, naming the file`` () : unit =
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
    let ``a read-only kit refuses experiment and setup writes and writes nothing`` () : unit =
        let folder, proxy = createContentTestKit classFolder
        for experiment in [ e1; e2 ] do
            proxy.writeExperiment experiment |> kitContentValue
        let setup =
            Setups.Setup.create (SetupName "Read-only bench") (SceneTestFixture.staticScene ()).elements []
            |> Result.defaultWith (fun error -> failwith error.describe)
        proxy.writeSetup setup |> kitContentValue
        let changedSetup =
            {
                setup with slots = List.tail setup.slots
            }
        let newSetup =
            {
                changedSetup with name = SetupName "New bench"
            }
        let changedExperiment =
            {
                e1 with scan = ExperimentScan.oneAxis (analyzerTurn (ElementLabel "Linear polarizer #4")) CaptureReflected None
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
              (fun () -> reader.deleteExperiment e2.name)
              (fun () -> reader.writeSetup changedSetup)
              (fun () -> reader.writeSetup newSetup) ]
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
        Assert.Equal<Setups.Setup list>([ setup ], reader.readSetups () |> kitContentValue)
