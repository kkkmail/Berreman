namespace OpticalConstructor.Tests

open System
open System.Text.Json.Nodes
open Xunit
open Berreman.Constants
open Berreman.Fields
open Berreman.Geometry
open Softellect.Sys.Primitives
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Experiments
open OpticalConstructor.Domain.ItemModels
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.Placement
open OpticalConstructor.Storage.Errors
open OpticalConstructor.Storage.CanonicalJson
open OpticalConstructor.Storage.KitContentFiles

module KitContentFileJsonTests =

    let private kitContentJsonValue (result : Result<'Value, StorageError>) : 'Value =
        match result with
        | Ok value -> value
        | Error error -> failwith error.describe

    let private polarizerId : ElementId = ElementId "lp"

    let private polarizerTurn : ScanAxis =
        {
            elementId = polarizerId
            label = ElementLabel "Linear polarizer #3"
            variable = VaryR1
            range = VariableRange.forVariable VaryR1
        }

    let private e1 : Experiment =
        let attachment : DataAttachment =
            {
                path = KitRelativePath "data/run 1.csv"
                contentHash = ContentHash "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"
            }
        {
            id = ExperimentId 1
            name = (ExperimentId 1).defaultName
            setup = { name = SetupName "Bench"; revision = SetupRevision 2 }
            scan = ExperimentScan.oneAxis polarizerTurn CaptureTransmitted (Some (WaveLength.nm 632.8<nm>))
            role = ScienceRun
            sessionOpt = None
            notes = ExperimentNotes.none
            dataOpt =
                Some
                    {
                        attachment = attachment
                        profile = ImportProfileName.intensitySeries
                        rows = AllDataRows
                        exclusions = []
                    }
            corrections = []
        }

    let private e3 : Experiment =
        {
            id = ExperimentId 3
            name = (ExperimentId 3).defaultName
            setup = { name = SetupName "Dark"; revision = SetupRevision 1 }
            scan = ExperimentScan.darkLine CaptureTransmitted
            role = ScienceRun
            sessionOpt = None
            notes = ExperimentNotes.none
            dataOpt = None
            corrections = []
        }

    /// E1 as a calibration of day 1 with notes, over two blocks: the sample's incidence × azimuth read in T and R, then
    /// the polarizer's four azimuths read in T (spec 0050 S9.1).
    let private e1TwoBlocks : Experiment =
        let sampleId = ElementId "sample"
        let range (lo : float) (hi : float) (points : int) : VariableRange =
            {
                min = Propagation.SweptValue lo
                max = Propagation.SweptValue hi
                points = Propagation.CurvePointCount points
            }
        let incidence : ScanAxis =
            {
                elementId = sampleId
                label = ElementLabel "Sample #1"
                variable = VaryR2
                range = range 20.0 60.0 3
            }
        let azimuth : ScanAxis =
            {
                incidence with
                    variable = VaryR1
                    range = range 0.0 90.0 2
            }
        let scan : ExperimentScan =
            {
                blocks =
                    [
                        { axes = [ incidence; azimuth ]; capture = CaptureBoth }
                        { axes = [ { polarizerTurn with range = range 0.0 67.5 4 } ]; capture = CaptureTransmitted }
                    ]
                sourceWaveLengthOpt = None
            }
        {
            e1 with
                scan = scan
                role = CalibrationRun
                sessionOpt = SessionName.tryCreate "Day 1"
                notes = ExperimentNotes "LR was re-seated"
        }

    /// E1 whose data is mapped by a kit's own profile to the rows of one experiment key (spec 0050 S9.1).
    let private e1KeyedRows : Experiment =
        {
            e1 with
                dataOpt =
                    e1.dataOpt
                    |> Option.map (fun data ->
                        {
                            data with
                                profile = ImportProfileName "Phone ROI CSV"
                                rows = KeyedDataRows (DataRowKey "CPL-(QZ#45)-CPL")
                        })
        }

    /// E1 whose data excludes its point 3 and whose setup was re-pointed once, with its audit note (spec 0050 S9.3).
    let private e1Corrected : Experiment =
        let cosmicRay : PointExclusion =
            {
                point = MappedPointNumber 3
                reason = ExclusionReason "cosmic ray"
            }
        let correction : SetupCorrection =
            {
                previous = { name = SetupName "CPL-(LR#45)-CPL"; revision = SetupRevision 1 }
                corrected = { name = SetupName "CPL-(LR#-45)-CPL"; revision = SetupRevision 1 }
                note = CorrectionNote "LR was at -45"
            }
        {
            e1 with
                setup = correction.corrected
                dataOpt = e1.dataOpt |> Option.map (fun data -> data.withExclusion cosmicRay)
                corrections = [ correction ]
        }

    let private editedFileOf (experiment : Experiment) (edit : JsonObject -> unit) : KitFileText =
        let text = experiment.experimentFile.fileText |> kitContentJsonValue
        let node = (JsonNode.Parse text.value).AsObject()
        edit node
        node.canonicalFileText |> KitFileText

    let private editedExperimentFile : (JsonObject -> unit) -> KitFileText = editedFileOf e1

    [<Fact>]
    let ``an experiment is stored under its name`` () : unit =
        let name = ExperimentName "CPL-(QZ#45)-CPL"
        let expectedName = FileName "CPL-(QZ#45)-CPL.experiment-json"
        let unnamed = ExperimentId 3
        let expectedStem = FileStem "E3"

        Assert.Equal<FileName>(expectedName, ExperimentKitFile.fileNameOf name.fileStem)
        Assert.Equal<FileStem>(expectedStem, unnamed.defaultName.fileStem)

    [<Fact>]
    let ``an experiment file round-trips through its canonical text`` () : unit =
        let expectedVersion = FileSchemaVersion 1

        for experiment in [ e1; e3; e1TwoBlocks; e1KeyedRows; e1Corrected ] do
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

        for key in [ "id"; "name"; "setup"; "scan"; "role"; "sessionOpt"; "notes"; "dataOpt"; "corrections" ] do
            editedExperimentFile (fun node -> node["experiment"].AsObject().Remove key |> ignore)
            |> assertSchemaInvalid

        for key in [ "blocks"; "sourceWaveLengthOpt" ] do
            editedExperimentFile (fun node -> node.["experiment"].["scan"].AsObject().Remove key |> ignore)
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
                (fun node -> node["experiment"]["unexpected"] <- JsonValue.Create "unexpected")
                (fun node -> node["experiment"]["id"] <- JsonValue.Create 0)
                (fun node -> node["experiment"]["name"] <- JsonValue.Create "  ")
                (fun node -> node["experiment"]["setup"] <- JsonValue.Create "not-an-array")
                (fun node -> node["experiment"]["setup"] <- JsonArray())
                (fun node -> node.["experiment"].["setup"].["revision"] <- JsonValue.Create 0)
                (fun node -> node["experiment"]["scan"] <- JsonValue.Create "not-an-object")
                (fun node -> node.["experiment"].["scan"].["blocks"] <- JsonValue.Create "not-an-array")
                (fun node -> node.["experiment"].["scan"].["unexpected"] <- JsonValue.Create "unexpected")
                (fun node -> node["experiment"]["role"] <- JsonValue.Create 0)
                (fun node -> node["experiment"]["role"] <- JsonValue.Create 8)
                (fun node -> node["experiment"]["role"] <- JsonValue.Create "ScienceRun")
                (fun node -> node["experiment"]["sessionOpt"] <- JsonValue.Create "")
                (fun node -> node["experiment"]["sessionOpt"] <- JsonValue.Create "   ")
                (fun node -> node["experiment"]["notes"] <- null)
                (fun node -> node.["experiment"].["dataOpt"].["attachment"].["contentHash"] <- JsonValue.Create "abc")
                (fun node -> node.["experiment"].["dataOpt"].["attachment"].["path"] <- JsonValue.Create "")
                (fun node -> node.["experiment"].["dataOpt"].["attachment"].["notes"] <- JsonValue.Create "unexpected")
                (fun node -> node.["experiment"].["dataOpt"].["attachment"].AsObject().Remove "path" |> ignore)
                (fun node -> node.["experiment"].["dataOpt"].["attachment"].AsObject().Remove "contentHash" |> ignore)
                (fun node -> node.["experiment"].["dataOpt"].["profile"] <- JsonValue.Create "")
                (fun node -> (node["experiment"]["dataOpt"]).AsObject().Remove "rows" |> ignore)
                (fun node -> (node["experiment"]["dataOpt"]).AsObject().Remove "exclusions" |> ignore)
            ]

        for edit in invalidEdits do
            editedExperimentFile edit |> assertSchemaInvalid

        // Spec 0050 S9.3: an exclusion numbers a point from 1 and gives a reason; a correction carries its audit note.
        let invalidCorrectedEdits : (JsonObject -> unit) list =
            [
                (fun node -> node.["experiment"].["dataOpt"].["exclusions"].[0].["point"] <- JsonValue.Create 0)
                (fun node -> node.["experiment"].["dataOpt"].["exclusions"].[0].["reason"] <- JsonValue.Create "  ")
                (fun node -> node.["experiment"].["corrections"].[0].AsObject().Remove "note" |> ignore)
            ]

        for edit in invalidCorrectedEdits do
            editedFileOf e1Corrected edit |> assertSchemaInvalid

        for text in [ KitFileText "null"; KitFileText "[]"; KitFileText "1" ] do
            assertSchemaInvalid text

        let malformedEdits : (JsonObject -> unit) list =
            [
                (fun node -> node.["experiment"].["scan"].["blocks"].[0].["axes"].[0].["elementId"] <- JsonValue.Create 7)
                (fun node -> node.["experiment"].["scan"].["blocks"].[0].["axes"].[0].["label"] <- JsonValue.Create 7)
                (fun node -> node.["experiment"].["scan"].["blocks"].[0] <- JsonValue.Create "not-a-block")
                (fun node -> node.["experiment"].["scan"].["blocks"].[0].["capture"] <- JsonValue.Create "not-a-measurement")
                (fun node -> node.["experiment"].["scan"].["blocks"].[0].["axes"].[0].["range"].["points"] <- JsonValue.Create "not-a-count")
                (fun node -> node.["experiment"].["scan"].["sourceWaveLengthOpt"] <- JsonValue.Create "not-a-wavelength")
            ]
        let malformed = KitFileText "{"

        for text in malformed :: (malformedEdits |> List.map editedExperimentFile) do
            match text.parseExperimentFile with
            | Error (MalformedJsonErr reason) -> Assert.False(String.IsNullOrWhiteSpace reason)
            | Error error -> Assert.Fail($"expected MalformedJsonErr: {error.describe}")
            | Ok _ -> Assert.Fail($"accepted malformed experiment file text: {text.value}")

    let private setupSource : Setups.SetupSlot =
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
            content = Setups.EmptySlotContent
        }

    let private setupPolarizer : Setups.SetupSlot =
        {
            id = polarizerId
            placement =
                {
                    ElementPlacement.create LinearPolarizer TablePoint.origin with
                        valueId = Some PresetIds.polarizerLp
                }
            zoom = BeamLayout.ElementZoom 3.25
            beam = BeamLayout.OnMainBeam
            content = Setups.EmptySlotContent
        }

    let private setupDetector : Setups.SetupSlot =
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
            content = Setups.EmptySlotContent
        }

    /// `Bench: A` at revision 2, its polarizer holding a test-local item on a dial read at −20°; at revision 1 the
    /// polarizer held nothing.
    let private setupFixture : Setups.Setup =
        let heldItem = ItemId (Guid.Parse "26000000-0000-4000-8000-000000000002")
        let dialled : Setups.SetupSlot =
            {
                setupPolarizer with content = Setups.ItemInSlot (heldItem, Setups.DialTurn (Angle.degree (-20.0<deg>)))
            }
        let firstRevision : Setups.SetupRevisionRecord =
            {
                revision = SetupRevision.first
                slots = [ setupSource; setupPolarizer; setupDetector ]
            }
        {
            name = SetupName "Bench: A"
            revision = SetupRevision 2
            slots = [ setupSource; dialled; setupDetector ]
            history = [ firstRevision ]
        }

    let private setupFile : Setups.SetupFile = setupFixture.setupFile

    let private editedSetupFile (edit : JsonObject -> unit) : KitFileText =
        let text = setupFile.fileText |> kitContentJsonValue
        let node = (JsonNode.Parse text.value).AsObject()
        edit node
        node.canonicalFileText |> KitFileText

    [<Fact>]
    let ``a setup is stored under its name made filesystem-safe`` () : unit =
        let name = SetupName "CPL-(QZ#45-LR#45)-CPL"
        let expectedStem = FileStem "CPL-(QZ#45-LR#45)-CPL"
        let expectedFileName = FileName "CPL-(QZ#45-LR#45)-CPL.setup-json"
        let unsafeName = SetupName "a: b/c"
        let safeStem = FileStem "a_ b_c"

        Assert.Equal<FileStem>(expectedStem, name.fileStem)
        Assert.Equal<FileStem>(safeStem, unsafeName.fileStem)
        Assert.Equal<FileName>(expectedFileName, SetupKitFile.fileNameOf name.fileStem)

    [<Fact>]
    let ``a setup file round-trips through its canonical text`` () : unit =
        let expectedVersion = FileSchemaVersion 1
        let text = setupFile.fileText |> kitContentJsonValue
        let readBack = text.parseSetupFile |> kitContentJsonValue

        Assert.Equal<FileSchemaVersion>(expectedVersion, setupFile.schemaVersion)
        Assert.Equal<Setups.SetupFile>(setupFile, readBack)
        Assert.Equal<Setups.Setup>(setupFixture, readBack.setup)
        Assert.DoesNotContain("\r", text.value)
        Assert.StartsWith("{\n  \"schemaVersion\": 1,\n  \"setup\": {", text.value)
        Assert.EndsWith("}\n", text.value)
        Assert.False(text.value.EndsWith "\n\n", "the file ends with exactly one line end")

        let node = (JsonNode.Parse text.value).AsObject()
        let keys = [ for KeyValue (key, _) in node -> key ]
        Assert.Equal<string list>([ "schemaVersion"; "setup" ], keys)
        Assert.Equal<KitFileText>(text, readBack.fileText |> kitContentJsonValue)

    [<Fact>]
    let ``a setup file of another schema version is UnsupportedSchemaVersionErr`` () : unit =
        let intact = editedSetupFile (fun node -> node["schemaVersion"] <- JsonValue.Create 2)
        let versionOnly = KitFileText "{ \"schemaVersion\": 2 }"
        let invalidBody =
            editedSetupFile (fun node ->
                node["schemaVersion"] <- JsonValue.Create 2
                node["setup"] <- JsonValue.Create "not-a-setup")
        let expectedFound = FileSchemaVersion 2
        let expectedVersion = FileSchemaVersion 1

        for text in [ intact; versionOnly; invalidBody ] do
            match text.parseSetupFile with
            | Error (UnsupportedSchemaVersionErr info) ->
                Assert.Equal(SetupKitFile.extension, info.fileExtension)
                Assert.Equal<FileSchemaVersion>(expectedFound, info.found)
                Assert.Equal<FileSchemaVersion>(expectedVersion, info.expected)
            | Error error -> Assert.Fail($"expected UnsupportedSchemaVersionErr: {error.describe}")
            | Ok _ -> Assert.Fail "accepted a setup file of another schema version"

    [<Fact>]
    let ``a schema-invalid or malformed setup file is refused before it binds`` () : unit =
        let assertSchemaInvalid (text : KitFileText) : unit =
            match text.parseSetupFile with
            | Error (SchemaValidationErr messages) ->
                Assert.NotEmpty messages
                for message in messages do
                    Assert.False(String.IsNullOrWhiteSpace message)
            | Error error -> Assert.Fail($"expected SchemaValidationErr: {error.describe}")
            | Ok _ -> Assert.Fail($"accepted a schema-invalid setup file: {text.value}")

        for key in [ "schemaVersion"; "setup" ] do
            editedSetupFile (fun node -> node.Remove key |> ignore) |> assertSchemaInvalid

        for key in [ "name"; "revision"; "slots"; "history" ] do
            editedSetupFile (fun node -> (node["setup"]).AsObject().Remove key |> ignore)
            |> assertSchemaInvalid

        let invalidEdits : (JsonObject -> unit) list =
            [
                (fun node -> node["setup"]["name"] <- JsonValue.Create "   ")
                (fun node -> node["setup"]["name"] <- JsonValue.Create "")
                (fun node -> node["setup"]["name"] <- JsonValue.Create "\t\n")
                (fun node -> node["setup"]["name"] <- JsonValue.Create 1)
                (fun node -> node["setup"]["name"] <- null)
                (fun node -> node["notes"] <- JsonValue.Create "unexpected")
                (fun node -> node["schemaVersion"] <- JsonValue.Create 0)
                (fun node -> node["schemaVersion"] <- JsonValue.Create (-1))
                (fun node -> node["schemaVersion"] <- JsonValue.Create 1.5)
                (fun node -> node["schemaVersion"] <- JsonValue.Create "1")
                (fun node -> node["schemaVersion"] <- null)
                (fun node -> node["setup"] <- null)
                (fun node -> node["setup"]["notes"] <- JsonValue.Create "unexpected")
                (fun node -> node["setup"]["revision"] <- JsonValue.Create 0)
                (fun node -> node["setup"]["revision"] <- JsonValue.Create "2")
                (fun node -> node["setup"]["slots"] <- JsonValue.Create "not-an-array")
                (fun node -> node["setup"]["history"] <- JsonValue.Create "not-an-array")
                (fun node -> node.["setup"].["history"].[0].AsObject().Remove "slots" |> ignore)
                (fun node -> node.["setup"].["history"].[0].["revision"] <- JsonValue.Create 0)
                (fun node -> node.["setup"].["history"].[0].["notes"] <- JsonValue.Create "unexpected")
            ]

        for edit in invalidEdits do
            editedSetupFile edit |> assertSchemaInvalid

        for text in [ KitFileText "null"; KitFileText "[]"; KitFileText "1" ] do
            assertSchemaInvalid text

        let malformedEdits : (JsonObject -> unit) list =
            [
                (fun node -> node.["setup"].["slots"].[0] <- JsonValue.Create "not-a-slot")
                (fun node -> node.["setup"].["slots"].[0].["placement"] <- JsonValue.Create "not-a-placement")
                (fun node -> node.["setup"].["slots"].[1].["content"] <- JsonValue.Create "not-a-content")
                (fun node -> node.["setup"].["history"].[0].["slots"].[0] <- JsonValue.Create "not-a-slot")
            ]
        let malformed = KitFileText "{"

        for text in malformed :: (malformedEdits |> List.map editedSetupFile) do
            match text.parseSetupFile with
            | Error (MalformedJsonErr reason) -> Assert.False(String.IsNullOrWhiteSpace reason)
            | Error error -> Assert.Fail($"expected MalformedJsonErr: {error.describe}")
            | Ok _ -> Assert.Fail($"accepted malformed setup file text: {text.value}")

    [<Fact>]
    let ``setup files preserve distinct same-preset physical ids and reject malformed slot identity`` () : unit =
        let lpAItemId = ItemId (Guid.Parse "6a2f0c8e-1d47-4b93-a5e2-8c3b7d9f0e14")
        let lpBItemId = ItemId (Guid.Parse "7b3e1d9f-2e58-4ca4-b6f3-9d4c8e0a1f25")

        let polarizerAt (id : ElementId) (x : float<meter>) (itemId : ItemId) : Setups.SetupSlot =
            {
                setupPolarizer with
                    id = id
                    placement = { setupPolarizer.placement with placementPoint = { x = x; y = 0.0<meter> } }
                    content = Setups.ItemInSlot (itemId, Setups.PlacementTurn)
            }

        let setup : Setups.Setup =
            {
                name = SetupName "Two pieces"
                revision = SetupRevision.first
                slots = [ setupSource; polarizerAt (ElementId "lp-a") -0.25<meter> lpAItemId; polarizerAt (ElementId "lp-b") 0.25<meter> lpBItemId ]
                history = []
            }

        let text = setup.setupFile.fileText |> kitContentJsonValue
        let readBack = text.parseSetupFile |> kitContentJsonValue
        Assert.Equal<Setups.Setup>(setup, readBack.setup)
        Assert.Equal<ItemId option list>(
            [ None; Some lpAItemId; Some lpBItemId ], readBack.setup.slots |> List.map (fun slot -> slot.content.itemOpt))
        // Both pieces bind the one preset, so only their ids tell them apart.
        Assert.Equal<ElementValueId option list>(
            [ Some PresetIds.polarizerLp; Some PresetIds.polarizerLp ],
            readBack.setup.slots |> List.tail |> List.map (fun slot -> slot.placement.valueId))

        // The slot's item is the shared canonical form: the GUID as the serializer writes it; an empty slot holds none.
        let slotsOf (stored : KitFileText) : JsonArray = (JsonNode.Parse stored.value).["setup"].["slots"].AsArray()
        Assert.Equal<string list>(
            [ lpAItemId.value.ToString(); lpBItemId.value.ToString() ],
            [ for slot in slotsOf text |> Seq.tail -> slot.["content"].["Fields"].[0].GetValue<string>() ])
        Assert.Equal<string>("EmptySlotContent", (slotsOf text).[0].["content"].GetValue<string>())

        let edited (edit : JsonObject -> unit) : KitFileText =
            let node = (JsonNode.Parse text.value).AsObject()
            edit node
            node.canonicalFileText |> KitFileText

        let refusals : (JsonObject -> unit) list =
            [
                (fun node -> node.["setup"].["slots"].[2].["content"].["Fields"].[0] <- JsonValue.Create "not-a-guid")
                (fun node -> node.["setup"].["slots"].[2].["content"].["Fields"].[0] <- JsonValue.Create 7)
                (fun node -> node.["setup"].["slots"].[2].["content"].["Fields"].[0] <- null)
                (fun node -> node.["setup"].["slots"].[1].["content"].["Fields"].AsArray().RemoveAt 0)
                (fun node -> node.["setup"].["slots"].[1].["content"].AsObject().Remove "Fields" |> ignore)
            ]

        // A slot whose item is no GUID, or names none, is refused before it binds: nothing is inferred.
        for refused in refusals |> List.map edited do
            match refused.parseSetupFile with
            | Error (MalformedJsonErr reason) -> Assert.False(String.IsNullOrWhiteSpace reason)
            | Error error -> Assert.Fail($"expected MalformedJsonErr: {error.describe}")
            | Ok parsed -> Assert.Fail($"accepted a slot without a valid item id: {parsed.setup.slots.Length} slot(s)")

    [<Fact>]
    let ``a setup references the library entries its slots bind in every revision, each once, at their latest`` () : unit =
        let presetEntryId (presetId : ElementValueId) : LibraryEntryId =
            match presetId.tryToLibraryEntryId with
            | Some entryId -> entryId
            | None -> failwith "a preset must name a library entry"

        let sourceReference = EntryLatestReference (presetEntryId PresetIds.source600)
        let polarizerReference = EntryLatestReference (presetEntryId PresetIds.polarizerLp)
        let expectedReferences = [ sourceReference; polarizerReference ]
        Assert.Equal<EntryReference list>(expectedReferences, setupFixture.libraryReferences)

        let sourceAgain : Setups.SetupRevisionRecord =
            {
                revision = SetupRevision 3
                slots =
                    [
                        {
                            setupSource with id = ElementId "src-2"
                        }
                    ]
            }
        let repeatedSource =
            {
                setupFixture with history = setupFixture.history @ [ sourceAgain ]
            }
        Assert.Equal<EntryReference list>(expectedReferences, repeatedSource.libraryReferences)

        let reversed =
            {
                setupFixture with
                    slots = List.rev setupFixture.slots
                    history = []
            }
        Assert.Equal<EntryReference list>([ polarizerReference; sourceReference ], reversed.libraryReferences)

        // The current slots come first, then each earlier revision's.
        let sourceInHistory : Setups.SetupRevisionRecord =
            {
                revision = SetupRevision.first
                slots = [ setupSource ]
            }
        let polarizerNow =
            {
                setupFixture with
                    slots = [ setupPolarizer; setupDetector ]
                    history = [ sourceInHistory ]
            }
        Assert.Equal<EntryReference list>([ polarizerReference; sourceReference ], polarizerNow.libraryReferences)

        let unboundSlots (slots : Setups.SetupSlot list) : Setups.SetupSlot list =
            slots
            |> List.map (fun slot ->
                {
                    slot with
                        placement =
                            {
                                slot.placement with valueId = None
                            }
                })
        let unbound =
            {
                setupFixture with
                    slots = unboundSlots setupFixture.slots
                    history =
                        setupFixture.history
                        |> List.map (fun record ->
                            {
                                record with slots = unboundSlots record.slots
                            })
            }
        Assert.Empty unbound.libraryReferences

        let nonEntryId = ElementValueId "not-a-library-entry"
        let nonEntrySlot =
            {
                setupDetector with
                    placement =
                        {
                            setupDetector.placement with valueId = Some nonEntryId
                        }
            }
        let nonEntrySetup =
            {
                setupFixture with
                    slots = [ nonEntrySlot ]
                    history = []
            }
        Assert.Empty nonEntrySetup.libraryReferences

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
