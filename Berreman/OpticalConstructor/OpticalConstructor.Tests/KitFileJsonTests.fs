namespace OpticalConstructor.Tests

open System
open System.Text.Json.Nodes
open Xunit
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Storage.Errors
open OpticalConstructor.Storage.CanonicalJson
open OpticalConstructor.Storage.KitFolderStore

module KitFileJsonTests =

    let private kitJsonValue (result : Result<'Value, StorageError>) : 'Value =
        match result with
        | Ok value -> value
        | Error error -> failwith error.describe

    /// A polarimeter's Mueller profile: tab, no header row, no byte-order mark, scan columns 1 in degrees and 2 in
    /// radians as physical values, m00…m33 in 3 to 18 with the throughput in 19, the observable in 20 and the key in 21
    /// (spec 0050 S9.2).
    let private polarimeterProfile : ImportProfile =
        let elementColumns = [ 3 .. 18 ] |> List.map DataColumnNumber
        {
            name = ImportProfileName "Polarimeter"
            delimiter = TabDelimiter
            header = NoHeaderRow
            bom = ByteOrderMarkAbsent
            roles =
                {
                    scan =
                        [
                            {
                                column = DataColumnNumber 1
                                valueUnit = DegreeColumnUnit
                                reading = PhysicalScanValue
                            }
                            {
                                column = DataColumnNumber 2
                                valueUnit = RadianColumnUnit
                                reading = PhysicalScanValue
                            }
                        ]
                    reading = MuellerElementColumns (elementColumns, Some (DataColumnNumber 19))
                    timestampOpt = None
                    captureIndexOpt = None
                    observableOpt = Some (DataColumnNumber 20)
                    keyOpt = Some (DataColumnNumber 21)
                }
        }

    let private kitFile : KitFile =
        let id = KitId (Guid.Parse "6f1c2a9e-3b7d-4e25-9a41-0c8d5e7f2b16")
        let name = KitName "Optics: QZ/LR — µ"
        let author = KitAuthor "Test author"
        let created = ClockTime (DateTime(2026, 11, 3, 12, 0, 0, DateTimeKind.Utc).AddTicks 1234567L)
        let modified = ClockTime (created.value.AddDays 1.0)
        {
            KitFile.create id name author created with
                modified = modified
                notes = KitNotes "Two lines\nof notes"
                sendExclude = [ SendExcludeGlob "**/*.dng"; SendExcludeGlob "data/raw/big/**" ]
                namingTemplate = NamingTemplate "{source:CPL1=CPL,LP1=LP}-({obj1}#{phi1})-{analyzer:CPL2=CPL}"
                importProfiles = [ KitTests.labScanProfile; polarimeterProfile ]
        }

    let private editedKitFile (edit : JsonObject -> unit) : KitFileText =
        let text = kitFile.fileText |> kitJsonValue
        let node = (JsonNode.Parse text.value).AsObject()
        edit node
        node.canonicalFileText |> KitFileText

    [<Fact>]
    let ``a kit file round-trips through its canonical text`` () : unit =
        let text = kitFile.fileText |> kitJsonValue
        let readBack = text.parseKitFile |> kitJsonValue

        Assert.Equal<KitFile>(kitFile, readBack)
        Assert.DoesNotContain("\r", text.value)
        Assert.StartsWith("{\n  \"author\":", text.value)
        Assert.EndsWith("}\n", text.value)
        Assert.False(text.value.EndsWith "\n\n", "the file ends with exactly one line end")
        Assert.Contains("Optics: QZ/LR — µ", text.value)
        Assert.Contains("\"created\": \"2026-11-03T12:00:00.1234567Z\"", text.value)
        Assert.Contains("\"modified\": \"2026-11-04T12:00:00.1234567Z\"", text.value)
        Assert.Equal<DateTimeKind>(DateTimeKind.Utc, readBack.created.value.Kind)
        Assert.Equal<DateTimeKind>(DateTimeKind.Utc, readBack.modified.value.Kind)

        let node = (JsonNode.Parse text.value).AsObject()
        let keys = [ for KeyValue (key, _) in node -> key ]
        Assert.Equal<string list>([ "author"; "created"; "id"; "importProfiles"; "modified"; "name"; "namingTemplate"; "notes"; "schemaVersion"; "sendExclude" ], keys)
        Assert.Contains("\"namingTemplate\": \"{source:CPL1=CPL,LP1=LP}-({obj1}#{phi1})-{analyzer:CPL2=CPL}\",", text.value)
        Assert.Equal<KitFileText>(text, readBack.fileText |> kitJsonValue)

        let profiles = node["importProfiles"].AsArray()
        let profileNames = [ for profile in profiles -> profile["name"].GetValue<string>() ]
        let polarimeter = profiles[1]
        let polarimeterDelimiter = polarimeter["delimiter"].GetValue<int>()
        Assert.Equal<string list>([ "Lab scan"; "Polarimeter" ], profileNames)
        Assert.Equal(3, polarimeterDelimiter)

        let emptyMetadata =
            {
                kitFile with
                    author = KitAuthor ""
                    notes = KitNotes ""
                    sendExclude = []
                    importProfiles = []
            }
        let emptyText = emptyMetadata.fileText |> kitJsonValue
        Assert.Equal<KitFile>(emptyMetadata, emptyText.parseKitFile |> kitJsonValue)

    [<Fact>]
    let ``a kit file of another schema version is UnsupportedSchemaVersionErr`` () : unit =
        let intact = editedKitFile (fun node -> node["schemaVersion"] <- JsonValue.Create 2)
        let versionOnly = KitFileText "{ \"schemaVersion\": 2 }"
        let invalidBody =
            editedKitFile (fun node ->
                node["schemaVersion"] <- JsonValue.Create 2
                node["created"] <- JsonValue.Create "not-an-instant"
                node["id"] <- JsonValue.Create "not-a-guid")
        let expectedFound = FileSchemaVersion 2
        let expectedVersion = FileSchemaVersion 1

        for text in [ intact; versionOnly; invalidBody ] do
            match text.parseKitFile with
            | Error (UnsupportedSchemaVersionErr info) ->
                Assert.Equal(KitFile.extension, info.fileExtension)
                Assert.Equal<FileSchemaVersion>(expectedFound, info.found)
                Assert.Equal<FileSchemaVersion>(expectedVersion, info.expected)
            | Error error -> Assert.Fail($"expected UnsupportedSchemaVersionErr: {error.describe}")
            | Ok _ -> Assert.Fail "accepted a kit file of another schema version"

    [<Fact>]
    let ``a schema-invalid or malformed kit file is refused before it binds`` () : unit =
        let assertSchemaInvalid (text : KitFileText) : unit =
            match text.parseKitFile with
            | Error (SchemaValidationErr messages) ->
                Assert.NotEmpty messages
                for message in messages do
                    Assert.False(String.IsNullOrWhiteSpace message)
            | Error error -> Assert.Fail($"expected SchemaValidationErr: {error.describe}")
            | Ok _ -> Assert.Fail($"accepted a schema-invalid kit file: {text.value}")

        for key in [ "schemaVersion"; "id"; "name"; "author"; "created"; "modified"; "notes"; "sendExclude"; "namingTemplate"; "importProfiles" ] do
            editedKitFile (fun node -> node.Remove key |> ignore) |> assertSchemaInvalid

        let firstProfileOf (node : JsonObject) : JsonObject =
            let profiles = node["importProfiles"].AsArray()
            profiles[0].AsObject()

        let invalidEdits : (JsonObject -> unit) list =
            [
                (fun node -> node["colour"] <- JsonValue.Create "blue")
                (fun node -> node["name"] <- JsonValue.Create "   ")
                (fun node -> node["name"] <- JsonValue.Create "")
                (fun node -> node["name"] <- JsonValue.Create "\t\r\n")
                (fun node -> node["id"] <- JsonValue.Create "not-a-guid")
                (fun node -> node["schemaVersion"] <- JsonValue.Create 0)
                (fun node -> node["schemaVersion"] <- JsonValue.Create (-1))
                (fun node -> node["schemaVersion"] <- JsonValue.Create 1.5)
                (fun node -> node["schemaVersion"] <- JsonValue.Create "1")
                (fun node -> node["schemaVersion"] <- null)
                (fun node -> node["author"] <- JsonValue.Create 1)
                (fun node -> node["notes"] <- null)
                (fun node -> node["created"] <- JsonValue.Create 1)
                (fun node -> node["modified"] <- JsonValue.Create 1)
                (fun node -> node["sendExclude"] <- JsonValue.Create "**/*.dng")
                (fun node -> node["sendExclude"] <- JsonNode.Parse "[\"\"]")
                (fun node -> node["sendExclude"] <- JsonNode.Parse "[1]")
                (fun node -> node["namingTemplate"] <- JsonValue.Create 7)
                (fun node -> (firstProfileOf node)["delimiter"] <- JsonValue.Create 0)
                (fun node -> (firstProfileOf node).Remove "roles" |> ignore)
            ]

        for edit in invalidEdits do
            editedKitFile edit |> assertSchemaInvalid

        for text in [ KitFileText "null"; KitFileText "[]"; KitFileText "1" ] do
            assertSchemaInvalid text

        let malformed = KitFileText "{"
        let invalidInstant = editedKitFile (fun node -> node["created"] <- JsonValue.Create "not-an-instant")
        let oversizedVersion = KitFileText "{ \"schemaVersion\": 3000000000 }"
        for text in [ malformed; invalidInstant; oversizedVersion ] do
            match text.parseKitFile with
            | Error (MalformedJsonErr reason) -> Assert.False(String.IsNullOrWhiteSpace reason)
            | Error error -> Assert.Fail($"expected MalformedJsonErr: {error.describe}")
            | Ok _ -> Assert.Fail($"accepted malformed kit file text: {text.value}")
