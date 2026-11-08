namespace OpticalConstructor.Tests

open System
open System.IO
open Xunit
open Berreman.Constants
open Softellect.Sys.Primitives
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.KitFolders
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.Placement
open OpticalConstructor.Domain.Scene
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

/// Capture and restore scenes through initialized kit folders under the test output directory
/// (spec 0038 step 027; spec 0050 S4.3), preserving all placement and view state without a window.
module SceneRoundTripTests =

    let private classFolder = FolderName "SceneRoundTripTests"

    let private sceneName (text : string) : SceneName =
        match SceneName.tryCreate text with
        | Ok name -> name
        | Error (InvalidSceneErr reason) -> failwith $"Test scene name unexpectedly rejected: {reason}"

    let private sourceEl : TestElement =
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
        }

    let private polarizerEl : TestElement =
        {
            id = Library.ElementId "pol"
            placement =
                {
                    ElementPlacement.create LinearPolarizer TablePoint.origin with
                        valueId = Some Library.PresetIds.polarizerLp
                }
            zoom = BeamLayout.ElementZoom 3.25
            beam = BeamLayout.OnMainBeam
        }

    let private detectorEl : TestElement =
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
        }

    let private dialledView : Table.TableViewState =
        {
            Table.defaultView with
                panX = Table.TablePan 12.5
                panY = Table.TablePan -7.25
                zoom = Table.TableZoom 1.875
        }

    let private sourceModel () : TableAndElementRotationModel =
        {
            SceneTestFixture.mainScene () with
                elements = [ sourceEl; polarizerEl; detectorEl ]
                view = dialledView
                snapChain = Scene.SnapToBeam
                rotationPivot = BeamLayout.PivotAtIncidentFace
        }

    [<Theory>]
    [<InlineData("")>]
    [<InlineData("   ")>]
    let ``SceneName.tryCreate rejects a blank name as InvalidScene carrying a reason`` (raw : string) =
        match SceneName.tryCreate raw with
        | Error (InvalidSceneErr reason) -> Assert.False(String.IsNullOrWhiteSpace reason)
        | other -> Assert.Fail($"expected Error (InvalidSceneErr _), got %A{other}")

    [<Fact>]
    let ``SceneName.tryCreate rejects a null name as InvalidScene`` () =
        match SceneName.tryCreate (null : string) with
        | Error (InvalidSceneErr reason) -> Assert.False(String.IsNullOrWhiteSpace reason)
        | other -> Assert.Fail($"expected Error (InvalidSceneErr _), got %A{other}")

    [<Fact>]
    let ``SceneName.tryCreate accepts a non-blank name and round-trips its value`` () =
        match SceneName.tryCreate "Experiment A" with
        | Ok n -> Assert.Equal("Experiment A", n.value)
        | Error e -> Assert.Fail($"expected Ok, got %A{e}")

    [<Fact>]
    let ``SnapMode round-trips its wire form: true snaps to the beam, false is free placement`` () =
        Assert.Equal(SnapToBeam, SnapMode.create true)
        Assert.Equal(FreePlacement, SnapMode.create false)
        Assert.True(SnapToBeam.value)
        Assert.False(FreePlacement.value)

    [<Fact>]
    let ``a scene round-trips through its kit file, reproducing ids, kinds, placements, bindings and view state`` () : unit =
        let folder, proxy = createContentTestKit classFolder
        let model = sourceModel ()
        let name = sceneName "Experiment A"
        let captured = model.captureScene
        proxy.writeScene name captured |> kitContentValue

        let reader = KitProxy.create folder
        Assert.Equal<SceneName list>([ name ], reader.readSceneNames () |> kitContentValue)
        let loaded = reader.readScene name |> kitContentValue
        Assert.Equal<SceneSnapshot>(captured, loaded)
        let file = FileName "Experiment A.setup-json"
        Assert.True(File.Exists((file.combine folder.value).value))

        // Spec 0038 step 027: a different base model exposes a restore that leaves old state behind.
        let restored = restoreScene loaded (SceneTestFixture.staticScene ())
        Assert.Equal<string list>(
            [ "src"; "pol"; "det" ],
            restored.elements |> List.map (fun element -> element.id.value))
        Assert.Equal<CatalogueKind list>(
            [ LightSource; LinearPolarizer; Detector ],
            restored.elements |> List.map (fun element -> element.placement.catalogueKind))
        Assert.Equal<ElementValueId option list>(
            [ Some Library.PresetIds.source600; Some Library.PresetIds.polarizerLp; None ],
            restored.elements |> List.map (fun element -> element.placement.valueId))
        Assert.Equal<TestElement list>([ sourceEl; polarizerEl; detectorEl ], restored.elements)
        let reflectedHost = Library.ElementId "pol"
        Assert.Equal<BeamLayout.BeamHost list>(
            [ BeamLayout.OnMainBeam; BeamLayout.OnMainBeam; BeamLayout.OnReflectedBeamOf reflectedHost ],
            restored.elements |> List.map (fun element -> element.beam))
        Assert.Equal<Table.TableViewState>(dialledView, restored.view)
        Assert.Equal(Scene.SnapToBeam, restored.snapChain)
        Assert.Equal(BeamLayout.PivotAtIncidentFace, restored.rotationPivot)

    [<Fact>]
    let ``writing a scene again under its name replaces its kit file`` () : unit =
        let folder, proxy = createContentTestKit classFolder
        let name = sceneName "Bench"
        let first = (sourceModel ()).captureScene
        let changedModel =
            {
                sourceModel () with
                    snapChain = Scene.FreePlacement
                    view = Table.defaultView
            }
        let second = changedModel.captureScene
        proxy.writeScene name first |> kitContentValue
        proxy.writeScene name second |> kitContentValue

        let reader = KitProxy.create folder
        Assert.Equal<SceneName list>([ name ], reader.readSceneNames () |> kitContentValue)
        Assert.Equal<SceneSnapshot>(second, reader.readScene name |> kitContentValue)
        Assert.Single(reader.listKindFiles SetupKitFile |> kitContentValue) |> ignore

        let invalidView =
            {
                second.view with zoom = Table.TableZoom Double.NaN
            }
        let invalidScene =
            {
                second with view = invalidView
            }
        let storedText = reader.readKindFile SetupKitFile name.fileStem |> kitContentValue
        match proxy.writeScene name invalidScene with
        | Error (KitFileRejectedErr reason) -> Assert.Contains("Bench.setup-json", reason)
        | Error error -> Assert.Fail($"Expected KitFileRejectedErr, got {error.describe}")
        | Ok () -> Assert.Fail("A scene that cannot be rendered must refuse the write.")
        Assert.Equal<KitFileText>(storedText, reader.readKindFile SetupKitFile name.fileStem |> kitContentValue)

        let firstAlias = sceneName "Bench:aux"
        let secondAlias = sceneName "Bench/aux"
        Assert.Equal<FileStem>(firstAlias.fileStem, secondAlias.fileStem)
        proxy.writeScene firstAlias first |> kitContentValue
        proxy.writeScene secondAlias second |> kitContentValue
        for alias in [ firstAlias; secondAlias ] do
            Assert.Equal<SceneSnapshot>(second, reader.readScene alias |> kitContentValue)
        Assert.Equal<SceneName list>([ name; secondAlias ], reader.readSceneNames () |> kitContentValue)
        Assert.Equal(2, (reader.listKindFiles SetupKitFile |> kitContentValue).Length)

        // Spec 0050 S4.3: sort the original names ordinally, independently of sanitized filename order.
        let afterAlias = sceneName "BenchZ"
        let upperName = sceneName "Zulu"
        let lowerName = sceneName "alpha"
        for extraName in [ lowerName; upperName; afterAlias ] do
            proxy.writeScene extraName first |> kitContentValue
        Assert.Equal<SceneName list>(
            [ name; secondAlias; afterAlias; upperName; lowerName ],
            reader.readSceneNames () |> kitContentValue)

    [<Fact>]
    let ``reading a scene the kit never held is MissingKitFileErr`` () : unit =
        let _, proxy = createContentTestKit classFolder
        let name = sceneName "missing"
        match proxy.readScene name with
        | Error (MissingKitFileErr reason) -> Assert.Contains("missing.setup-json", reason)
        | Error error -> Assert.Fail($"Expected MissingKitFileErr, got {error.describe}")
        | Ok _ -> Assert.Fail("An unsaved scene must be reported as missing.")
        Assert.Empty(proxy.readSceneNames () |> kitContentValue)

    [<Fact>]
    let ``a setup file that does not read refuses the scene list, naming the file`` () : unit =
        let folder, proxy = createContentTestKit classFolder
        let validName = sceneName "A valid scene"
        let scene = (sourceModel ()).captureScene
        proxy.writeScene validName scene |> kitContentValue
        let brokenName = sceneName "Broken"
        let brokenFile = FileName "Broken.setup-json"
        File.WriteAllText((brokenFile.combine folder.value).value, "{")

        match proxy.readSceneNames () with
        | Error (KitFileRejectedErr reason) -> Assert.Contains(brokenFile.value, reason)
        | Error error -> Assert.Fail($"Expected KitFileRejectedErr, got {error.describe}")
        | Ok _ -> Assert.Fail("A malformed setup must refuse the whole scene list.")
        match proxy.readScene brokenName with
        | Error (KitFileRejectedErr reason) -> Assert.Contains(brokenFile.value, reason)
        | Error error -> Assert.Fail($"Expected KitFileRejectedErr, got {error.describe}")
        | Ok _ -> Assert.Fail("A malformed setup must refuse an individual scene read.")
        Assert.Equal<SceneSnapshot>(scene, proxy.readScene validName |> kitContentValue)

        proxy.writeScene brokenName scene |> kitContentValue
        do
            use _held = File.Open((brokenFile.combine folder.value).value, FileMode.Open, FileAccess.ReadWrite, FileShare.None)
            match proxy.readSceneNames () with
            | Error (KitFileRejectedErr reason) -> Assert.Contains(brokenFile.value, reason)
            | Error error -> Assert.Fail($"Expected KitFileRejectedErr, got {error.describe}")
            | Ok _ -> Assert.Fail("A setup that cannot be opened must refuse the whole scene list.")
            match proxy.readScene brokenName with
            | Error (KitFileRejectedErr reason) -> Assert.Contains(brokenFile.value, reason)
            | Error error -> Assert.Fail($"Expected KitFileRejectedErr, got {error.describe}")
            | Ok _ -> Assert.Fail("A setup that cannot be opened must refuse an individual read.")
        Assert.Equal<SceneName list>([ validName; brokenName ], proxy.readSceneNames () |> kitContentValue)
