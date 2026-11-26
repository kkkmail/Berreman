namespace OpticalConstructor.Tests

open System
open System.IO
open Xunit
open Berreman.Constants
open Berreman.Geometry
open Softellect.Sys.Primitives
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.KitFolders
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.Placement
open OpticalConstructor.Domain.Scene
open OpticalConstructor.Domain.Setups
open OpticalConstructor.Storage.KitContentFiles
open OpticalConstructor.Storage.KitFolderStore
open OpticalConstructor.Ui.TableAndElementRotationView

module internal KitContentRoundTripFixture =

    let kitContentValue<'Value> (result : Result<'Value, KitError>) : 'Value =
        result |> Result.defaultWith (fun error -> failwith error.describe)

    let createContentTestKit (classFolder : FolderName) : KitFolder * KitProxy =
        let folderName = FolderName (Path.Combine(AppContext.BaseDirectory, classFolder.value, Guid.NewGuid().ToString("N")))
        let folder = KitFolder folderName
        let proxy = KitProxy.create folder
        let id = KitId (Guid.NewGuid ())
        let name = KitName "Content round-trip kit"
        let author = KitAuthor "Test author"
        let now = ClockTime (DateTime(2026, 11, 7, 12, 0, 0, DateTimeKind.Utc))
        let kit = KitFile.create id name author now
        proxy.initialiseKit kit |> kitContentValue
        folder, proxy

open KitContentRoundTripFixture

/// Write and read setups through initialized kit folders under the test output directory, preserving their slots,
/// the items the slots hold and their revisions without a window (spec 0038 step 027; spec 0050 S4.3, S8.2).
module SetupRoundTripTests =

    let private classFolder = FolderName "SetupRoundTripTests"

    let private setupName (text : string) : SetupName =
        match SetupName.tryCreate text with
        | Ok name -> name
        | Error error -> failwith $"Test setup name unexpectedly rejected: {error.describe}"

    let private sourceEl : SetupSlot =
        let position : TablePoint =
            {
                x = -0.75<meter>
                y = 0.10<meter>
            }
        {
            id = Library.ElementId "src"
            placement =
                {
                    ElementPlacement.create LightSource position with
                        valueId = Some Library.PresetIds.source600
                }
            zoom = BeamLayout.ElementZoom 1.5
            beam = BeamLayout.OnMainBeam
            content = EmptySlotContent
        }

    let private polarizerEl : SetupSlot =
        {
            id = Library.ElementId "pol"
            placement =
                {
                    ElementPlacement.create LinearPolarizer TablePoint.origin with
                        valueId = Some Library.PresetIds.polarizerLp
                }
            zoom = BeamLayout.ElementZoom 3.25
            beam = BeamLayout.OnMainBeam
            content = EmptySlotContent
        }

    let private detectorEl : SetupSlot =
        let position : TablePoint =
            {
                x = 0.80<meter>
                y = -0.20<meter>
            }
        let reflectedHost = Library.ElementId "pol"
        {
            id = Library.ElementId "det"
            placement = ElementPlacement.create Detector position
            zoom = BeamLayout.ElementZoom 2.0
            beam = BeamLayout.OnReflectedBeamOf reflectedHost
            content = EmptySlotContent
        }

    /// The item the polarizer slot holds at revision 2, on a dial read at −20°.
    let private heldPolarizerContent : SlotContent =
        ItemInSlot (ItemId (Guid.Parse "26000000-0000-4000-8000-000000000001"), DialTurn (Angle.degree (-20.0<deg>)))

    /// The fixture slots as a setup at revision 2 whose polarizer holds an item, its revision 1 holding nothing there.
    let private fixtureSetup (name : SetupName) : Setup =
        let firstRevision : SetupRevisionRecord =
            {
                revision = SetupRevision.first
                slots = [ sourceEl; polarizerEl; detectorEl ]
            }
        {
            name = name
            revision = SetupRevision 2
            slots = [ sourceEl; { polarizerEl with content = heldPolarizerContent }; detectorEl ]
            history = [ firstRevision ]
        }

    [<Fact>]
    let ``SnapMode round-trips its wire form: true snaps to the beam, false is free placement`` () =
        Assert.Equal(SnapToBeam, SnapMode.create true)
        Assert.Equal(FreePlacement, SnapMode.create false)
        Assert.True(SnapToBeam.value)
        Assert.False(FreePlacement.value)

    [<Fact>]
    let ``a setup round-trips through its kit file, reproducing its slots, their items and its revisions`` () : unit =
        let folder, proxy = createContentTestKit classFolder
        let setup = fixtureSetup (setupName "Experiment A")
        proxy.writeSetup setup |> kitContentValue

        let reader = KitProxy.create folder
        let readBack = reader.readSetups () |> kitContentValue
        Assert.Equal<Setup list>([ setup ], readBack)
        let file = FileName "Experiment A.setup-json"
        Assert.True(File.Exists((file.combine folder.value).value))
        let loaded = List.exactlyOne readBack
        Assert.Equal<SlotContent list>(
            [ EmptySlotContent; heldPolarizerContent; EmptySlotContent ],
            loaded.slots |> List.map (fun slot -> slot.content))
        Assert.Equal<SetupSlot list>([ sourceEl; polarizerEl; detectorEl ], loaded.slotsAt SetupRevision.first |> Result.defaultWith (fun error -> failwith error.describe))

        // Spec 0038 step 027: a different base model exposes an activation that leaves old state behind.
        let baseModel = SceneTestFixture.staticScene ()
        let activated =
            {
                baseModel with kitSetups = [ loaded ]
            }
            |> updateTableAndElementRotation (ActivateSetup loaded.name)
        Assert.Equal<SetupSlot list>(setup.slots, activated.elements)
        Assert.Equal<Setup option>(Some setup, activated.activeSetupOpt)
        Assert.Equal<string list>(
            [ "src"; "pol"; "det" ],
            activated.elements |> List.map (fun element -> element.id.value))
        Assert.Equal<CatalogueKind list>(
            [ LightSource; LinearPolarizer; Detector ],
            activated.elements |> List.map (fun element -> element.placement.catalogueKind))
        Assert.Equal<ElementValueId option list>(
            [ Some Library.PresetIds.source600; Some Library.PresetIds.polarizerLp; None ],
            activated.elements |> List.map (fun element -> element.placement.valueId))
        let reflectedHost = Library.ElementId "pol"
        Assert.Equal<BeamLayout.BeamHost list>(
            [ BeamLayout.OnMainBeam; BeamLayout.OnMainBeam; BeamLayout.OnReflectedBeamOf reflectedHost ],
            activated.elements |> List.map (fun element -> element.beam))
        // Spec 0050 S8.2: a setup is the optical path; the view, snap mode and pivot stay the window's.
        Assert.Equal<Table.TableViewState>(baseModel.view, activated.view)
        Assert.Equal(baseModel.snapChain, activated.snapChain)
        Assert.Equal(baseModel.rotationPivot, activated.rotationPivot)

    [<Fact>]
    let ``writing a setup again under its name replaces its kit file`` () : unit =
        let folder, proxy = createContentTestKit classFolder
        let name = setupName "Bench"
        let first = fixtureSetup name
        let replaced : SetupRevisionRecord =
            {
                revision = first.revision
                slots = first.slots
            }
        let second =
            {
                first with
                    revision = first.revision.next
                    slots = [ sourceEl; detectorEl ]
                    history = first.history @ [ replaced ]
            }
        proxy.writeSetup first |> kitContentValue
        proxy.writeSetup second |> kitContentValue

        let reader = KitProxy.create folder
        Assert.Equal<Setup list>([ second ], reader.readSetups () |> kitContentValue)
        Assert.Single(reader.listKindFiles SetupKitFile |> kitContentValue) |> ignore

        let invalidSlot =
            {
                detectorEl with zoom = BeamLayout.ElementZoom Double.NaN
            }
        let invalidSetup =
            {
                second with slots = [ sourceEl; invalidSlot ]
            }
        let storedText = reader.readKindFile SetupKitFile name.fileStem |> kitContentValue
        match proxy.writeSetup invalidSetup with
        | Error (KitFileRejectedErr reason) -> Assert.Contains("Bench.setup-json", reason)
        | Error error -> Assert.Fail($"Expected KitFileRejectedErr, got {error.describe}")
        | Ok () -> Assert.Fail("A setup that cannot be rendered must refuse the write.")
        Assert.Equal<KitFileText>(storedText, reader.readKindFile SetupKitFile name.fileStem |> kitContentValue)

        let firstAlias = setupName "Bench:aux"
        let secondAlias = setupName "Bench/aux"
        Assert.Equal<FileStem>(firstAlias.fileStem, secondAlias.fileStem)
        let aliased =
            {
                second with name = secondAlias
            }
        proxy.writeSetup (fixtureSetup firstAlias) |> kitContentValue
        proxy.writeSetup aliased |> kitContentValue
        Assert.Equal<Setup list>([ second; aliased ], reader.readSetups () |> kitContentValue)
        Assert.Equal(2, (reader.listKindFiles SetupKitFile |> kitContentValue).Length)

        // Spec 0050 S4.3: sort the original names ordinally, independently of sanitized filename order.
        let afterAlias = setupName "BenchZ"
        let upperName = setupName "Zulu"
        let lowerName = setupName "alpha"
        for extraName in [ lowerName; upperName; afterAlias ] do
            proxy.writeSetup (fixtureSetup extraName) |> kitContentValue
        Assert.Equal<SetupName list>(
            [ name; secondAlias; afterAlias; upperName; lowerName ],
            reader.readSetups () |> kitContentValue |> List.map (fun setup -> setup.name))

    [<Fact>]
    let ``a kit with no setup files reads no setups`` () : unit =
        let _, proxy = createContentTestKit classFolder
        Assert.Empty(proxy.listKindFiles SetupKitFile |> kitContentValue)
        Assert.Equal<Result<Setup list, KitError>>(Ok [], proxy.readSetups ())

    [<Fact>]
    let ``a setup file that does not read, or holds another name, refuses the setup list, naming the file`` () : unit =
        let folder, proxy = createContentTestKit classFolder
        let valid = fixtureSetup (setupName "Experiment A")
        proxy.writeSetup valid |> kitContentValue
        let validText = proxy.readKindFile SetupKitFile valid.name.fileStem |> kitContentValue
        let pathOf (file : FileName) : string = (file.combine folder.value).value
        let assertRefusedNaming (file : FileName) : unit =
            match proxy.readSetups () with
            | Error (KitFileRejectedErr reason) -> Assert.Contains(file.value, reason)
            | Error error -> Assert.Fail($"Expected KitFileRejectedErr, got {error.describe}")
            | Ok _ -> Assert.Fail($"'{file.value}' must refuse the whole setup list.")

        let brokenFile = FileName "Broken.setup-json"
        File.WriteAllText(pathOf brokenFile, "{")
        assertRefusedNaming brokenFile
        File.Delete(pathOf brokenFile)
        Assert.Equal<Setup list>([ valid ], proxy.readSetups () |> kitContentValue)

        let misnamedFile = FileName "B.setup-json"
        File.WriteAllText(pathOf misnamedFile, validText.value)
        assertRefusedNaming misnamedFile
        File.Delete(pathOf misnamedFile)
        Assert.Equal<Setup list>([ valid ], proxy.readSetups () |> kitContentValue)

        let locked = fixtureSetup (setupName "Broken")
        proxy.writeSetup locked |> kitContentValue
        do
            use _held = File.Open(pathOf brokenFile, FileMode.Open, FileAccess.ReadWrite, FileShare.None)
            assertRefusedNaming brokenFile
        Assert.Equal<Setup list>([ locked; valid ], proxy.readSetups () |> kitContentValue)
