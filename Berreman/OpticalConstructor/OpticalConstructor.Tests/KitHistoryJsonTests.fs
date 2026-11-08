namespace OpticalConstructor.Tests

open System
open System.Text.Json.Nodes
open Xunit
open Softellect.Sys.Primitives
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.KitHistory
open OpticalConstructor.Storage.Errors
open OpticalConstructor.Storage.CanonicalJson
open OpticalConstructor.Storage.KitHistoryStore

module KitHistoryJsonTests =

    let private historyJsonValue (result : Result<'Value, StorageError>) : 'Value =
        match result with
        | Ok value -> value
        | Error error -> failwith error.describe

    let private firstOpened : ClockTime = ClockTime (DateTime(2026, 11, 3, 12, 0, 0, DateTimeKind.Utc).AddTicks 1234567L)

    let private starredEntry : KitHistoryEntry =
        {
            folder = KitFolder (FolderName "C:/kits/Optics QZ — µ")
            kitId = KitId (Guid.Parse "4c9e2b17-8a3f-4d60-b1e5-7f02a9c63d48")
            name = KitName "Optics QZ — µ"
            lastOpened = firstOpened
            star = StarredKit
        }

    let private unstarredEntry : KitHistoryEntry =
        {
            folder = KitFolder (FolderName "D:/lab/Bench 2")
            kitId = KitId (Guid.Parse "a37d05e2-61bc-4f98-9d24-c5e8b10f7a63")
            name = KitName "Bench 2"
            lastOpened = ClockTime (firstOpened.value.AddHours(-1.0))
            star = UnstarredKit
        }

    let private history : KitHistory = { KitHistory.empty with entries = [ starredEntry; unstarredEntry ] }

    let private editedHistory (edit : JsonObject -> unit) : KitHistoryText =
        let text = history.fileText |> historyJsonValue
        let node = (JsonNode.Parse text.value).AsObject()
        edit node
        node.canonicalFileText |> KitHistoryText

    let private editedFirstEntry (edit : JsonObject -> unit) : KitHistoryText =
        editedHistory (fun node -> node.["entries"].[0].AsObject() |> edit)

    [<Fact>]
    let ``a kit history round-trips through its canonical text`` () : unit =
        let text = history.fileText |> historyJsonValue
        let readBack = text.parseKitHistory |> historyJsonValue

        Assert.Equal<KitHistory>(history, readBack)
        Assert.DoesNotContain("\r", text.value)
        Assert.StartsWith("{\n  \"entries\": [", text.value)
        Assert.EndsWith("}\n", text.value)
        Assert.False(text.value.EndsWith "\n\n", "the file ends with exactly one line end")
        Assert.Contains("\"folder\": \"C:/kits/Optics QZ — µ\"", text.value)
        Assert.Contains("\"name\": \"Optics QZ — µ\"", text.value)
        Assert.DoesNotContain("\\u", text.value)
        Assert.Contains("\"lastOpened\": \"2026-11-03T12:00:00.1234567Z\"", text.value)

        let node = (JsonNode.Parse text.value).AsObject()
        let keys = [ for KeyValue (key, _) in node -> key ]
        Assert.Equal<string list>([ "entries"; "schemaVersion" ], keys)

        let entries = node.["entries"].AsArray()
        Assert.Equal<int>(2, entries.Count)

        for entry in entries do
            let entryKeys = [ for KeyValue (key, _) in entry.AsObject() -> key ]
            Assert.Equal<string list>([ "folder"; "kitId"; "lastOpened"; "name"; "star" ], entryKeys)

        let first = entries.[0].AsObject()
        Assert.Equal<string>("1", first.["star"].ToJsonString())
        Assert.Equal<string>("\"2026-11-03T12:00:00.1234567Z\"", first.["lastOpened"].ToJsonString())
        Assert.Equal<DateTimeKind>(DateTimeKind.Utc, readBack.entries.Head.lastOpened.value.Kind)

        Assert.Equal<KitHistoryText>(text, readBack.fileText |> historyJsonValue)

        let emptyText = KitHistory.empty.fileText |> historyJsonValue
        Assert.Equal<KitHistory>(KitHistory.empty, emptyText.parseKitHistory |> historyJsonValue)
        Assert.Equal<KitHistoryText>(emptyText, (emptyText.parseKitHistory |> historyJsonValue).fileText |> historyJsonValue)

    [<Fact>]
    let ``a kit history of another schema version is UnsupportedSchemaVersionErr`` () : unit =
        let intact = editedHistory (fun node -> node["schemaVersion"] <- JsonValue.Create 2)
        let versionOnly = KitHistoryText "{ \"schemaVersion\": 2 }"
        let expectedExtension = FileExtension ".json"
        let expectedFound = FileSchemaVersion 2
        let expectedVersion = FileSchemaVersion 1

        for text in [ intact; versionOnly ] do
            match text.parseKitHistory with
            | Error (UnsupportedSchemaVersionErr info) ->
                Assert.Equal<FileExtension>(expectedExtension, info.fileExtension)
                Assert.Equal<FileSchemaVersion>(expectedFound, info.found)
                Assert.Equal<FileSchemaVersion>(expectedVersion, info.expected)
            | Error error -> Assert.Fail($"expected UnsupportedSchemaVersionErr: {error.describe}")
            | Ok _ -> Assert.Fail "accepted a kit history of another schema version"

    [<Fact>]
    let ``a schema-invalid or malformed kit history is refused before it binds`` () : unit =
        let assertSchemaInvalid (text : KitHistoryText) : unit =
            match text.parseKitHistory with
            | Error (SchemaValidationErr messages) ->
                Assert.NotEmpty messages
                for message in messages do
                    Assert.False(String.IsNullOrWhiteSpace message)
            | Error error -> Assert.Fail($"expected SchemaValidationErr: {error.describe}")
            | Ok _ -> Assert.Fail($"accepted a schema-invalid kit history: {text.value}")

        let invalidEntryEdits : (JsonObject -> unit) list =
            [
                (fun entry -> entry.Remove "folder" |> ignore)
                (fun entry -> entry["colour"] <- JsonValue.Create "blue")
                (fun entry -> entry["star"] <- JsonValue.Create 3)
                (fun entry -> entry["kitId"] <- JsonValue.Create "not-a-guid")
                (fun entry -> entry["name"] <- JsonValue.Create "   ")
            ]

        for edit in invalidEntryEdits do
            editedFirstEntry edit |> assertSchemaInvalid

        editedHistory (fun node -> node["recent"] <- JsonValue.Create "D:/lab/Bench 2") |> assertSchemaInvalid

        let malformed = KitHistoryText "{"
        match malformed.parseKitHistory with
        | Error (MalformedJsonErr reason) -> Assert.False(String.IsNullOrWhiteSpace reason)
        | Error error -> Assert.Fail($"expected MalformedJsonErr: {error.describe}")
        | Ok _ -> Assert.Fail "accepted malformed kit history text"
