namespace OpticalConstructor.Tests

open System
open System.Text.Json.Nodes
open Xunit
open Berreman.Constants
open Berreman.Fields
open Softellect.Sys.Primitives
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Experiments
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.Lifecycle
open OpticalConstructor.Domain.Placement
open OpticalConstructor.Domain.Scene
open OpticalConstructor.Storage.Errors
open OpticalConstructor.Storage.CanonicalJson
open OpticalConstructor.Storage.KitContentFiles

module KitContentFileJsonTests =

    let private kitContentJsonValue (result : Result<'Value, StorageError>) : 'Value =
        match result with
        | Ok value -> value
        | Error error -> failwith error.describe

    let private polarizerId : ElementId = ElementId "lp"

    let private e1 : Experiment =
        let sampleVersion = SampleVersionId.firstOf SampleIds.glassPlate1mmId
        let sampleReference = SampleVersionRef sampleVersion
        let attachment : DataAttachment =
            {
                path = KitRelativePath "data/run 1.csv"
                contentHash = ContentHash "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"
            }
        {
            id = ExperimentId 1
            setup =
                [
                    {
                        elementId = ElementId "src"
                        label = ElementLabel "Light source #1"
                        kind = LightSource
                        placement = PlacementSummary.ofPlacement (ElementPlacement.create LightSource TablePoint.origin)
                        binding = BoundByEntryId PresetIds.source600
                    }
                    {
                        elementId = ElementId "sample"
                        label = ElementLabel "Sample #2"
                        kind = Sample
                        placement = PlacementSummary.ofPlacement (ElementPlacement.create Sample TablePoint.origin)
                        binding = BoundByVersion sampleReference
                    }
                    {
                        elementId = polarizerId
                        label = ElementLabel "Linear polarizer #3"
                        kind = LinearPolarizer
                        placement = PlacementSummary.ofPlacement (ElementPlacement.create LinearPolarizer TablePoint.origin)
                        binding = Unbound
                    }
                ]
            varied =
                Some
                    {
                        elementId = polarizerId
                        variable = VaryR1
                    }
            measurement = CaptureTransmitted
            range = VariableRange.forVariable VaryR1
            dataFileOpt = Some attachment
            sourceWaveLengthOpt = Some (WaveLength.nm 632.8<nm>)
        }

    let private e3 : Experiment =
        {
            id = ExperimentId 3
            setup = []
            varied = None
            measurement = CaptureTransmitted
            range = VariableRange.forVariable VaryR1
            dataFileOpt = None
            sourceWaveLengthOpt = None
        }

    let private editedExperimentFile (edit : JsonObject -> unit) : KitFileText =
        let text = e1.experimentFile.fileText |> kitContentJsonValue
        let node = (JsonNode.Parse text.value).AsObject()
        edit node
        node.canonicalFileText |> KitFileText

    [<Fact>]
    let ``an experiment is stored under its id`` () : unit =
        let id = ExperimentId 3
        let expectedStem = FileStem "E3"
        let expectedName = FileName "E3.experiment-json"

        Assert.Equal<FileStem>(expectedStem, id.fileStem)
        Assert.Equal<FileName>(expectedName, ExperimentKitFile.fileNameOf id.fileStem)

    [<Fact>]
    let ``an experiment file round-trips through its canonical text`` () : unit =
        let expectedVersion = FileSchemaVersion 1

        for experiment in [ e1; e3 ] do
            let file = experiment.experimentFile
            let text = file.fileText |> kitContentJsonValue
            let readBack = text.parseExperimentFile |> kitContentJsonValue

            Assert.Equal<FileSchemaVersion>(expectedVersion, file.schemaVersion)
            Assert.Equal<Experiment>(experiment, file.experiment)
            Assert.Equal<ExperimentFile>(file, readBack)
            Assert.DoesNotContain("\r", text.value)
            Assert.StartsWith("{\n  \"experiment\":", text.value)
            Assert.EndsWith("}\n", text.value)
            Assert.False(text.value.EndsWith "\n\n", "the file ends with exactly one line end")

            let node = (JsonNode.Parse text.value).AsObject()
            let keys = [ for KeyValue (key, _) in node -> key ]
            Assert.Equal<string list>([ "experiment"; "schemaVersion" ], keys)
            Assert.Equal<KitFileText>(text, readBack.fileText |> kitContentJsonValue)

    [<Fact>]
    let ``an experiment file of another schema version is UnsupportedSchemaVersionErr`` () : unit =
        let intact = editedExperimentFile (fun node -> node["schemaVersion"] <- JsonValue.Create 2)
        let versionOnly = KitFileText "{ \"schemaVersion\": 2 }"
        let invalidBody =
            editedExperimentFile (fun node ->
                node["schemaVersion"] <- JsonValue.Create 2
                node["experiment"] <- JsonValue.Create "not-an-experiment")
        let expectedFound = FileSchemaVersion 2
        let expectedVersion = FileSchemaVersion 1

        for text in [ intact; versionOnly; invalidBody ] do
            match text.parseExperimentFile with
            | Error (UnsupportedSchemaVersionErr info) ->
                Assert.Equal(ExperimentKitFile.extension, info.fileExtension)
                Assert.Equal<FileSchemaVersion>(expectedFound, info.found)
                Assert.Equal<FileSchemaVersion>(expectedVersion, info.expected)
            | Error error -> Assert.Fail($"expected UnsupportedSchemaVersionErr: {error.describe}")
            | Ok _ -> Assert.Fail "accepted an experiment file of another schema version"

    [<Fact>]
    let ``a schema-invalid or malformed experiment file is refused before it binds`` () : unit =
        let assertSchemaInvalid (text : KitFileText) : unit =
            match text.parseExperimentFile with
            | Error (SchemaValidationErr messages) ->
                Assert.NotEmpty messages
                for message in messages do
                    Assert.False(String.IsNullOrWhiteSpace message)
            | Error error -> Assert.Fail($"expected SchemaValidationErr: {error.describe}")
            | Ok _ -> Assert.Fail($"accepted a schema-invalid experiment file: {text.value}")

        for key in [ "schemaVersion"; "experiment" ] do
            editedExperimentFile (fun node -> node.Remove key |> ignore) |> assertSchemaInvalid

        for key in [ "id"; "setup"; "varied"; "measurement"; "range"; "dataFileOpt"; "sourceWaveLengthOpt" ] do
            editedExperimentFile (fun node -> node["experiment"].AsObject().Remove key |> ignore)
            |> assertSchemaInvalid

        let invalidEdits : (JsonObject -> unit) list =
            [
                (fun node -> node["notes"] <- JsonValue.Create "unexpected")
                (fun node -> node["schemaVersion"] <- JsonValue.Create 0)
                (fun node -> node["schemaVersion"] <- JsonValue.Create (-1))
                (fun node -> node["schemaVersion"] <- JsonValue.Create 1.5)
                (fun node -> node["schemaVersion"] <- JsonValue.Create "1")
                (fun node -> node["schemaVersion"] <- null)
                (fun node -> node["experiment"] <- null)
                (fun node -> node["experiment"]["notes"] <- JsonValue.Create "unexpected")
                (fun node -> node["experiment"]["id"] <- JsonValue.Create 0)
                (fun node -> node["experiment"]["setup"] <- JsonValue.Create "not-an-array")
                (fun node -> node["experiment"]["range"] <- JsonValue.Create "not-an-object")
                (fun node -> node.["experiment"].["dataFileOpt"].["contentHash"] <- JsonValue.Create "abc")
                (fun node -> node.["experiment"].["dataFileOpt"].["path"] <- JsonValue.Create "")
                (fun node -> node.["experiment"].["dataFileOpt"].["notes"] <- JsonValue.Create "unexpected")
                (fun node -> (node["experiment"]["dataFileOpt"]).AsObject().Remove "path" |> ignore)
                (fun node -> (node["experiment"]["dataFileOpt"]).AsObject().Remove "contentHash" |> ignore)
            ]

        for edit in invalidEdits do
            editedExperimentFile edit |> assertSchemaInvalid

        for text in [ KitFileText "null"; KitFileText "[]"; KitFileText "1" ] do
            assertSchemaInvalid text

        let malformedEdits : (JsonObject -> unit) list =
            [
                (fun node -> node.["experiment"].["setup"].[0] <- JsonValue.Create "not-a-descriptor")
                (fun node -> node.["experiment"].["setup"].[0].["binding"] <- JsonValue.Create "not-a-binding")
                (fun node -> node["experiment"]["varied"] <- JsonValue.Create "not-a-varied-element")
                (fun node -> node["experiment"]["measurement"] <- JsonValue.Create "not-a-measurement")
                (fun node -> node.["experiment"].["range"].["points"] <- JsonValue.Create "not-a-count")
                (fun node -> node["experiment"]["sourceWaveLengthOpt"] <- JsonValue.Create "not-a-wavelength")
            ]
        let malformed = KitFileText "{"

        for text in malformed :: (malformedEdits |> List.map editedExperimentFile) do
            match text.parseExperimentFile with
            | Error (MalformedJsonErr reason) -> Assert.False(String.IsNullOrWhiteSpace reason)
            | Error error -> Assert.Fail($"expected MalformedJsonErr: {error.describe}")
            | Ok _ -> Assert.Fail($"accepted malformed experiment file text: {text.value}")

    let private sceneSource : SceneElement =
        let position : TablePoint =
            {
                x = -0.75<meter>
                y = 0.10<meter>
            }
        {
            id = ElementId "src"
            placement =
                {
                    ElementPlacement.create LightSource position with
                        valueId = Some PresetIds.source600
                }
            zoom = BeamLayout.ElementZoom 1.5
            beam = BeamLayout.OnMainBeam
        }

    let private scenePolarizer : SceneElement =
        {
            id = polarizerId
            placement =
                {
                    ElementPlacement.create LinearPolarizer TablePoint.origin with
                        valueId = Some PresetIds.polarizerLp
                }
            zoom = BeamLayout.ElementZoom 3.25
            beam = BeamLayout.OnMainBeam
        }

    let private sceneDetector : SceneElement =
        let position : TablePoint =
            {
                x = 0.80<meter>
                y = -0.20<meter>
            }
        {
            id = ElementId "det"
            placement = ElementPlacement.create Detector position
            zoom = BeamLayout.ElementZoom 2.0
            beam = BeamLayout.OnReflectedBeamOf polarizerId
        }

    let private sceneFixture : SceneSnapshot =
        {
            elements = [ sceneSource; scenePolarizer; sceneDetector ]
            view =
                {
                    Table.defaultView with
                        panX = Table.TablePan 12.5
                        panY = Table.TablePan -7.25
                        zoom = Table.TableZoom 1.875
                }
            snap = SnapToBeam
            pivot = BeamLayout.PivotAtIncidentFace
        }

    let private sceneFile : SceneFile =
        {
            schemaVersion = SceneFile.currentSchemaVersion
            name = SceneName "Bench: A"
            scene = sceneFixture
        }

    let private editedSceneFile (edit : JsonObject -> unit) : KitFileText =
        let text = sceneFile.fileText |> kitContentJsonValue
        let node = (JsonNode.Parse text.value).AsObject()
        edit node
        node.canonicalFileText |> KitFileText

    [<Fact>]
    let ``a scene is stored under its name made filesystem-safe`` () : unit =
        let name = SceneName "CPL-(QZ#45-LR#45)-CPL"
        let expectedStem = FileStem "CPL-(QZ#45-LR#45)-CPL"
        let expectedFileName = FileName "CPL-(QZ#45-LR#45)-CPL.setup-json"
        let unsafeName = SceneName "a: b/c"
        let safeStem = FileStem "a_ b_c"

        Assert.Equal<FileStem>(expectedStem, name.fileStem)
        Assert.Equal<FileStem>(safeStem, unsafeName.fileStem)
        Assert.Equal<FileName>(expectedFileName, SetupKitFile.fileNameOf name.fileStem)

    [<Fact>]
    let ``a setup file round-trips through its canonical text`` () : unit =
        let expectedVersion = FileSchemaVersion 1
        let text = sceneFile.fileText |> kitContentJsonValue
        let readBack = text.parseSceneFile |> kitContentJsonValue

        Assert.Equal<FileSchemaVersion>(expectedVersion, sceneFile.schemaVersion)
        Assert.Equal<SceneFile>(sceneFile, readBack)
        Assert.DoesNotContain("\r", text.value)
        Assert.StartsWith("{\n  \"name\": \"Bench: A\",", text.value)
        Assert.EndsWith("}\n", text.value)
        Assert.False(text.value.EndsWith "\n\n", "the file ends with exactly one line end")

        let node = (JsonNode.Parse text.value).AsObject()
        let keys = [ for KeyValue (key, _) in node -> key ]
        Assert.Equal<string list>([ "name"; "scene"; "schemaVersion" ], keys)
        Assert.Equal<KitFileText>(text, readBack.fileText |> kitContentJsonValue)

    [<Fact>]
    let ``a setup file of another schema version is UnsupportedSchemaVersionErr`` () : unit =
        let intact = editedSceneFile (fun node -> node["schemaVersion"] <- JsonValue.Create 2)
        let versionOnly = KitFileText "{ \"schemaVersion\": 2 }"
        let invalidBody =
            editedSceneFile (fun node ->
                node["schemaVersion"] <- JsonValue.Create 2
                node["scene"] <- JsonValue.Create "not-a-scene")
        let expectedFound = FileSchemaVersion 2
        let expectedVersion = FileSchemaVersion 1

        for text in [ intact; versionOnly; invalidBody ] do
            match text.parseSceneFile with
            | Error (UnsupportedSchemaVersionErr info) ->
                Assert.Equal(SetupKitFile.extension, info.fileExtension)
                Assert.Equal<FileSchemaVersion>(expectedFound, info.found)
                Assert.Equal<FileSchemaVersion>(expectedVersion, info.expected)
            | Error error -> Assert.Fail($"expected UnsupportedSchemaVersionErr: {error.describe}")
            | Ok _ -> Assert.Fail "accepted a setup file of another schema version"

    [<Fact>]
    let ``a schema-invalid or malformed setup file is refused before it binds`` () : unit =
        let assertSchemaInvalid (text : KitFileText) : unit =
            match text.parseSceneFile with
            | Error (SchemaValidationErr messages) ->
                Assert.NotEmpty messages
                for message in messages do
                    Assert.False(String.IsNullOrWhiteSpace message)
            | Error error -> Assert.Fail($"expected SchemaValidationErr: {error.describe}")
            | Ok _ -> Assert.Fail($"accepted a schema-invalid setup file: {text.value}")

        for key in [ "schemaVersion"; "name"; "scene" ] do
            editedSceneFile (fun node -> node.Remove key |> ignore) |> assertSchemaInvalid

        for key in [ "elements"; "view"; "snap"; "pivot" ] do
            editedSceneFile (fun node -> (node["scene"]).AsObject().Remove key |> ignore)
            |> assertSchemaInvalid

        let invalidEdits : (JsonObject -> unit) list =
            [
                (fun node -> node["name"] <- JsonValue.Create "   ")
                (fun node -> node["name"] <- JsonValue.Create "")
                (fun node -> node["name"] <- JsonValue.Create "\t\n")
                (fun node -> node["name"] <- JsonValue.Create 1)
                (fun node -> node["name"] <- null)
                (fun node -> node["notes"] <- JsonValue.Create "unexpected")
                (fun node -> node["schemaVersion"] <- JsonValue.Create 0)
                (fun node -> node["schemaVersion"] <- JsonValue.Create (-1))
                (fun node -> node["schemaVersion"] <- JsonValue.Create 1.5)
                (fun node -> node["schemaVersion"] <- JsonValue.Create "1")
                (fun node -> node["schemaVersion"] <- null)
                (fun node -> node["scene"] <- null)
                (fun node -> node["scene"]["notes"] <- JsonValue.Create "unexpected")
                (fun node -> node["scene"]["elements"] <- JsonValue.Create "not-an-array")
            ]

        for edit in invalidEdits do
            editedSceneFile edit |> assertSchemaInvalid

        for text in [ KitFileText "null"; KitFileText "[]"; KitFileText "1" ] do
            assertSchemaInvalid text

        let malformedEdits : (JsonObject -> unit) list =
            [
                (fun node -> node.["scene"].["elements"].[0] <- JsonValue.Create "not-an-element")
                (fun node -> node.["scene"].["elements"].[0].["placement"] <- JsonValue.Create "not-a-placement")
                (fun node -> node["scene"]["view"] <- JsonValue.Create "not-a-view")
                (fun node -> node["scene"]["snap"] <- JsonValue.Create "not-a-snap-mode")
                (fun node -> node["scene"]["pivot"] <- JsonValue.Create "not-a-pivot")
            ]
        let malformed = KitFileText "{"

        for text in malformed :: (malformedEdits |> List.map editedSceneFile) do
            match text.parseSceneFile with
            | Error (MalformedJsonErr reason) -> Assert.False(String.IsNullOrWhiteSpace reason)
            | Error error -> Assert.Fail($"expected MalformedJsonErr: {error.describe}")
            | Ok _ -> Assert.Fail($"accepted malformed setup file text: {text.value}")

    [<Fact>]
    let ``a scene references the library entries its elements bind, each once, at their latest`` () : unit =
        let presetEntryId (presetId : ElementValueId) : LibraryEntryId =
            match presetId.tryToLibraryEntryId with
            | Some entryId -> entryId
            | None -> failwith "a preset must name a library entry"

        let sourceReference = EntryLatestReference (presetEntryId PresetIds.source600)
        let polarizerReference = EntryLatestReference (presetEntryId PresetIds.polarizerLp)
        let expectedReferences = [ sourceReference; polarizerReference ]
        Assert.Equal<EntryReference list>(expectedReferences, sceneFixture.libraryReferences)

        let secondSource =
            {
                sceneSource with id = ElementId "src-2"
            }
        let repeatedSource =
            {
                sceneFixture with elements = sceneFixture.elements @ [ secondSource ]
            }
        Assert.Equal<EntryReference list>(expectedReferences, repeatedSource.libraryReferences)

        let reversed =
            {
                sceneFixture with elements = List.rev sceneFixture.elements
            }
        Assert.Equal<EntryReference list>([ polarizerReference; sourceReference ], reversed.libraryReferences)

        let unbound =
            {
                sceneFixture with
                    elements =
                        sceneFixture.elements
                        |> List.map (fun element ->
                            {
                                element with
                                    placement =
                                        {
                                            element.placement with valueId = None
                                        }
                            })
            }
        Assert.Empty unbound.libraryReferences

        let nonEntryId = ElementValueId "not-a-library-entry"
        let nonEntryElement =
            {
                sceneDetector with
                    placement =
                        {
                            sceneDetector.placement with valueId = Some nonEntryId
                        }
            }
        let nonEntryScene =
            {
                sceneFixture with elements = [ nonEntryElement ]
            }
        Assert.Empty nonEntryScene.libraryReferences
