namespace OpticalConstructor.Tests

open System
open System.Text.Json.Nodes
open Xunit
open Berreman.Constants
open Berreman.Fields
open Softellect.Sys.Primitives
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Experiments
open OpticalConstructor.Domain.ItemModels
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

    /// The draft's item at revision 1, failing with the refusal's text when an empty kit refuses it.
    let private draftedItem (draft : ItemDraft) : Item =
        match draft.addTo [] with
        | Ok item -> item
        | Error error -> failwith $"'{draft.label.value}' was refused: {error.describe}"

    /// The Built-in polarizer preset of that id, failing when the catalogue holds another kind under it.
    let private polarizerPresetOf (id : ElementValueId) : PolarizerPreset =
        match BuiltinCatalogue.preset id with
        | PolarizerItem polarizer -> polarizer
        | SampleItem _ | SourceItem _ | DetectorItem _ -> failwith $"The Built-in preset '{id.value}' is not a polarizer."

    let private cpl1Draft : ItemDraft =
        {
            id = ItemId (Guid.Parse "861bcd5e-8c9b-4e16-b315-76f3c2d799ca")
            label = ItemLabel "CPL1"
            model = LibraryModelChoice (BuiltinCatalogue.itemModel ItemModelIds.circularPolarizerModelId)
            dial = DialMounted DialPositive
        }

    /// CPL1 at revision 2: its retardance left to a fit under a prior, its fast-axis offset calibrated, and the
    /// revision-1 parameters in its history.
    let private cpl1 : Item =
        let drafted = draftedItem cpl1Draft

        let retardance : UnknownParameterData =
            let bounds : ParameterInterval =
                {
                    lower = ParameterValue 1.2
                    upper = ParameterValue 1.9
                }

            let prior : ParameterPrior =
                {
                    centre = ParameterValue (Math.PI / 2.0)
                    sigma = ParameterSigma 0.05
                }

            {
                start = ParameterValue 1.55
                bounds = bounds
                priorOpt = Some prior
            }

        let fastAxisOffset : CalibratedParameterData =
            {
                value = ParameterValue 0.0125
                sigma = ParameterSigma 0.0004
                fitRun = FitRunName "CPL1-offset@20261108T093000Z"
            }

        let revised (parameter : Parameter) : Parameter =
            if parameter.name = ParameterName "c2.retardance" then
                {
                    parameter with state = UnknownParameter retardance
                }
            elif parameter.name = ParameterName "c2.fastAxisOffset" then
                {
                    parameter with state = CalibratedParameter fastAxisOffset
                }
            else
                parameter

        let revisionOne : ItemRevisionRecord =
            {
                revision = ItemRevision.first
                parameters = drafted.parameters
            }

        {
            drafted with
                parameters = drafted.parameters |> List.map revised
                photoOpt = Some (KitRelativePath "data/photos/CPL1.jpg")
                notes = ItemNotes "the left-handed circular polarizer, re-measured after cleaning"
                revision = ItemRevision.first.next
                history = [ revisionOne ]
        }

    let private laserDraft : ItemDraft =
        {
            id = ItemId (Guid.Parse "0c902cce-0bf6-48e7-badc-60f08d99c98c")
            label = ItemLabel "Laser"
            model = SourcePresetChoice (BuiltinCatalogue.itemModel ItemModelIds.sourcePresetItemModelId, BuiltinCatalogue.source PresetIds.heNe633)
            dial = FixedMounting
        }

    let private lp1Draft : ItemDraft =
        {
            id = ItemId (Guid.Parse "b78e50eb-900c-4c95-906a-43a50ccc4f57")
            label = ItemLabel "LP1"
            model = PolarizerPresetChoice (BuiltinCatalogue.itemModel ItemModelIds.idealPolarizerItemModelId, polarizerPresetOf PresetIds.polarizerLp)
            dial = DialMounted DialNegative
        }

    let private plateDraft : ItemDraft =
        {
            id = ItemId (Guid.Parse "b4c85d10-d97e-4c6c-ba7f-787c9f0333b1")
            label = ItemLabel "Plate1"
            model =
                StackSampleChoice (
                    BuiltinCatalogue.itemModel ItemModelIds.stackSampleItemModelId,
                    SampleVersionId.firstOf SampleIds.glassPlate1mmId,
                    BuiltinCatalogue.sample SampleIds.glassPlate1mmId
                )
            dial = FixedMounting
        }

    /// A standalone model's item, then a source, an ideal-polarizer and a stack item, each with the draft it came from.
    let private itemsWithDrafts : (Item * ItemDraft) list =
        [
            cpl1, cpl1Draft
            draftedItem laserDraft, laserDraft
            draftedItem lp1Draft, lp1Draft
            draftedItem plateDraft, plateDraft
        ]

    let private editedItemFileOf (item : Item) (edit : JsonObject -> unit) : KitFileText =
        let text = item.itemFile.fileText |> kitContentJsonValue
        let node = (JsonNode.Parse text.value).AsObject()
        edit node
        node.canonicalFileText |> KitFileText

    let private editedItemFile (edit : JsonObject -> unit) : KitFileText = editedItemFileOf cpl1 edit

    [<Fact>]
    let ``an item is stored under its label`` () : unit =
        let label = ItemLabel "CPL1"
        let expectedStem = FileStem "CPL1"
        let expectedName = FileName "CPL1.item-json"

        Assert.Equal<FileStem>(expectedStem, label.fileStem)
        Assert.Equal<FileName>(expectedName, ItemKitFile.fileNameOf label.fileStem)
        Assert.Equal<FileName>(expectedName, ItemKitFile.fileNameOf cpl1.label.fileStem)

    [<Fact>]
    let ``an item file round-trips through its canonical text`` () : unit =
        let expectedVersion = FileSchemaVersion 1

        for item, draft in itemsWithDrafts do
            let file = item.itemFile
            let text = file.fileText |> kitContentJsonValue
            let readBack = text.parseItemFile |> kitContentJsonValue

            Assert.Equal<FileSchemaVersion>(expectedVersion, file.schemaVersion)
            Assert.Equal<Item>(item, file.item)
            Assert.Equal<ItemFile>(file, readBack)
            Assert.DoesNotContain("\r", text.value)
            Assert.StartsWith("{\n  \"item\": {", text.value)
            Assert.EndsWith("}\n", text.value)
            Assert.False(text.value.EndsWith "\n\n", "the file ends with exactly one line end")

            let node = (JsonNode.Parse text.value).AsObject()
            let keys = [ for KeyValue (key, _) in node -> key ]
            Assert.Equal<string list>([ "item"; "schemaVersion" ], keys)
            Assert.Equal<KitFileText>(text, readBack.fileText |> kitContentJsonValue)

            // The item read back names its model's exact version and, for a binding form, its typed physics.
            let referenceCount =
                match draft.model with
                | LibraryModelChoice _ -> 1
                | SourcePresetChoice _ | PolarizerPresetChoice _ | StackSampleChoice _ -> 2

            Assert.Equal<ItemModelInstance>(draft.model.instance, readBack.item.model)
            Assert.Equal<EntryReference list>(item.libraryReferences, readBack.item.libraryReferences)
            Assert.Equal(referenceCount, readBack.item.libraryReferences.Length)

        // CPL1 keeps its revision, its unknown and calibrated parameters, its history and its photo.
        let cpl1Back = (cpl1.itemFile.fileText |> kitContentJsonValue).parseItemFile |> kitContentJsonValue
        Assert.Equal<ItemRevision>(ItemRevision 2, cpl1Back.item.revision)
        Assert.Equal<ItemRevisionRecord list>(cpl1.history, cpl1Back.item.history)
        Assert.Equal<KitRelativePath option>(Some (KitRelativePath "data/photos/CPL1.jpg"), cpl1Back.item.photoOpt)

        match cpl1Back.item.tryFindParameter (ParameterName "c2.retardance") with
        | Some { state = UnknownParameter { priorOpt = Some _ } } -> ()
        | Some _ | None -> Assert.Fail "CPL1 did not read its retardance back as unknown under a prior"

        match cpl1Back.item.tryFindParameter (ParameterName "c2.fastAxisOffset") with
        | Some { state = CalibratedParameter _ } -> ()
        | Some _ | None -> Assert.Fail "CPL1 did not read its fast-axis offset back as calibrated"

    [<Fact>]
    let ``an item file of another schema version is UnsupportedSchemaVersionErr`` () : unit =
        let intact =
            [
                for item, _ in itemsWithDrafts ->
                    editedItemFileOf item (fun node -> node["schemaVersion"] <- JsonValue.Create 2)
            ]
        let versionOnly = KitFileText "{ \"schemaVersion\": 2 }"
        let invalidBody =
            editedItemFile (fun node ->
                node["schemaVersion"] <- JsonValue.Create 2
                node["item"] <- JsonValue.Create "not-an-item")
        let expectedFound = FileSchemaVersion 2
        let expectedVersion = FileSchemaVersion 1

        for text in intact @ [ versionOnly; invalidBody ] do
            match text.parseItemFile with
            | Error (UnsupportedSchemaVersionErr info) ->
                Assert.Equal(ItemKitFile.extension, info.fileExtension)
                Assert.Equal<FileSchemaVersion>(expectedFound, info.found)
                Assert.Equal<FileSchemaVersion>(expectedVersion, info.expected)
            | Error error -> Assert.Fail($"expected UnsupportedSchemaVersionErr: {error.describe}")
            | Ok _ -> Assert.Fail "accepted an item file of another schema version"

    [<Fact>]
    let ``a schema-invalid or malformed item file is refused before it binds`` () : unit =
        let assertSchemaInvalid (text : KitFileText) : unit =
            match text.parseItemFile with
            | Error (SchemaValidationErr messages) ->
                Assert.NotEmpty messages
                for message in messages do
                    Assert.False(String.IsNullOrWhiteSpace message)
            | Error error -> Assert.Fail($"expected SchemaValidationErr: {error.describe}")
            | Ok _ -> Assert.Fail($"accepted a schema-invalid item file: {text.value}")

        for key in [ "schemaVersion"; "item" ] do
            editedItemFile (fun node -> node.Remove key |> ignore) |> assertSchemaInvalid

        for key in [ "id"; "label"; "model"; "parameters"; "dial"; "photoOpt"; "notes"; "revision"; "history" ] do
            editedItemFile (fun node -> node["item"].AsObject().Remove key |> ignore)
            |> assertSchemaInvalid

        let invalidEdits : (JsonObject -> unit) list =
            [
                (fun node -> node["notes"] <- JsonValue.Create "unexpected")
                (fun node -> node["schemaVersion"] <- JsonValue.Create 0)
                (fun node -> node["schemaVersion"] <- JsonValue.Create (-1))
                (fun node -> node["schemaVersion"] <- JsonValue.Create 1.5)
                (fun node -> node["schemaVersion"] <- JsonValue.Create "1")
                (fun node -> node["schemaVersion"] <- null)
                (fun node -> node["item"] <- null)
                (fun node -> node["item"]["unexpected"] <- JsonValue.Create "unexpected")
                (fun node -> node["item"]["id"] <- JsonValue.Create "not-a-guid")
                (fun node -> node["item"]["id"] <- JsonValue.Create "861BCD5E-8C9B-4E16-B315-76F3C2D799CA")
                (fun node -> node["item"]["label"] <- JsonValue.Create "")
                (fun node -> node["item"]["label"] <- JsonValue.Create 1)
                (fun node -> node["item"]["label"] <- null)
                (fun node -> node["item"]["parameters"] <- JsonValue.Create "not-an-array")
                (fun node -> node["item"]["photoOpt"] <- JsonValue.Create "")
                (fun node -> node["item"]["photoOpt"] <- JsonValue.Create 1)
                (fun node -> node["item"]["notes"] <- null)
                (fun node -> node["item"]["notes"] <- JsonValue.Create 1)
                (fun node -> node["item"]["revision"] <- JsonValue.Create 0)
                (fun node -> node["item"]["revision"] <- JsonValue.Create (-1))
                (fun node -> node["item"]["revision"] <- JsonValue.Create 1.5)
                (fun node -> node["item"]["revision"] <- JsonValue.Create "2")
                (fun node -> node["item"]["history"] <- JsonValue.Create "not-an-array")
            ]

        for edit in invalidEdits do
            editedItemFile edit |> assertSchemaInvalid

        for text in [ KitFileText "null"; KitFileText "[]"; KitFileText "1" ] do
            assertSchemaInvalid text

        let malformedEdits : (JsonObject -> unit) list =
            [
                (fun node -> node["item"]["model"] <- JsonValue.Create "not-a-model")
                (fun node -> node.["item"].["parameters"].[0] <- JsonValue.Create "not-a-parameter")
                (fun node -> node.["item"].["parameters"].[0].["state"] <- JsonValue.Create "not-a-state")
                (fun node -> node["item"]["dial"] <- JsonValue.Create "not-a-dial")
                (fun node -> node.["item"].["history"].[0] <- JsonValue.Create "not-a-revision")
                (fun node -> node.["item"].["history"].[0].["parameters"] <- JsonValue.Create "not-a-parameter-list")
            ]
        let malformed = KitFileText "{"

        for text in malformed :: (malformedEdits |> List.map editedItemFile) do
            match text.parseItemFile with
            | Error (MalformedJsonErr reason) -> Assert.False(String.IsNullOrWhiteSpace reason)
            | Error error -> Assert.Fail($"expected MalformedJsonErr: {error.describe}")
            | Ok _ -> Assert.Fail($"accepted malformed item file text: {text.value}")
