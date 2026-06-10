namespace OpticalConstructor.Storage

open System
open System.IO
open System.Text.Json
open System.Text.Json.Nodes
open Json.Schema
open OpticalConstructor.Domain
open OpticalConstructor.Storage.Errors

/// The separate per-user groups + experiment-collections store (Spec 0026 Part G/H, slice
/// 007 / R-3, R-4). Element groups (G.1) and experiment collections (H.1) are persisted in
/// their OWN JSON file, kept apart from the per-project `.ocproj`, so reusable groups and
/// sample-centred collections are available across projects (G.3.1). This module owns that
/// file's load/save and validate-on-load. It REUSES the slice-003 shared `ProjectJson.options`
/// `JsonSerializerOptions` (§I.1 — never a second JSON stack) and the schema-validate-on-load
/// SEAM the spec names by line (R-4 / G.3.1, `SchemaValidation.fs:61`): `validate` shares
/// `SchemaValidation.collectMessages` (exposed for this) for the `results.Details`-recursive
/// message flattening, so nested groups-file errors keep their detail. Only the lazy `JsonSchema`
/// itself is local — keyed by a distinct `$id`, because the project loader is hardwired to the
/// `.ocproj` schema. No new serializer (G.4); no schema migration (a `schemaVersion` mismatch is a
/// `SchemaValidationError`, never a migration). The file lives at a fixed per-user app-data path
/// outside the repository working tree (G.3.2), so it raises no `.gitignore` collision.
module GroupsLibrary =

    /// The single shipped groups-schema version, pinned with `const` in the schema. A differing
    /// value fails validation and so falls back to the empty library (no migration).
    [<Literal>]
    let schemaVersion = "1.0"

    /// The build-copied groups schema content item, loaded beside the assembly at runtime (the
    /// fsproj copies `schema/...groups.schema.json` preserving this relative path). Mirrors the
    /// slice-003 project-schema load (`SchemaValidation.fs:17`).
    let schemaRelativePath =
        Path.Combine("schema", "optical-constructor-groups.schema.json")

    let private schemaFullPath =
        Path.Combine(AppContext.BaseDirectory, schemaRelativePath)

    /// Load the published groups schema ONCE behind `lazy` — `JsonSchema.FromText` registers the
    /// document by its (distinct) `$id` in the process-global registry, so a single load keeps
    /// validate-on-load idempotent across calls. Returns `SchemaMissing` when the content item is
    /// absent at runtime, `FileIoError` on a read failure.
    let private schema : Result<JsonSchema, StorageError> Lazy =
        lazy
            if File.Exists schemaFullPath then
                try File.ReadAllText schemaFullPath |> JsonSchema.FromText |> Ok
                with e -> Error (FileIoError e)
            else
                Error SchemaMissing

    /// Validate a parsed document against the groups schema (G.3.1) via `JsonSchema.Net`. On any
    /// evaluation failure returns `SchemaValidationError` with the library's messages; the caller
    /// MUST NOT bind on `Error`. The message flattening REUSES `SchemaValidation.collectMessages`
    /// (R-4 / the validate-on-load seam the spec names by line) so a NESTED groups-file violation —
    /// e.g. a malformed member placement deep in the document — keeps its `results.Details` instance
    /// location instead of collapsing to a generic top-level message.
    let validate (element : JsonElement) : Result<unit, StorageError> =
        match schema.Value with
        | Error e -> Error e
        | Ok schema ->
            let options = EvaluationOptions(OutputFormat = OutputFormat.List)
            let results = schema.Evaluate(element, options)
            if results.IsValid then Ok ()
            else
                let messages = SchemaValidation.collectMessages results
                let messages = if List.isEmpty messages then [ "groups schema validation failed" ] else messages
                Error (SchemaValidationError messages)

    /// Serialize the groups/collections library to canonical JSON (G.4), injecting the envelope
    /// `schemaVersion`, REUSING the slice-003 shared `ProjectJson.options` (§I.1) — never a second
    /// JSON stack.
    let serialize (library : Groups.GroupsLibrary) : Result<string, StorageError> =
        try
            let node = JsonSerializer.SerializeToNode(library, ProjectJson.options)
            let envelope = node.AsObject()
            envelope.["schemaVersion"] <- JsonValue.Create(schemaVersion)
            Ok(envelope.ToJsonString(ProjectJson.options))
        with e -> Error(JsonParseError e.Message)

    /// Parse, validate-on-load, then bind (G.3.1 / G.4). Schema validation ALWAYS runs before the
    /// value is admitted; a `schemaVersion` mismatch fails the schema's `const` and surfaces as
    /// `SchemaValidationError`, never a migration.
    let deserialize (json : string) : Result<Groups.GroupsLibrary, StorageError> =
        let parsed =
            try Ok(JsonDocument.Parse json)
            with e -> Error(JsonParseError e.Message)

        match parsed with
        | Error e -> Error e
        | Ok doc ->
            use doc = doc
            match validate doc.RootElement with
            | Error e -> Error e
            | Ok () ->
                try Ok(doc.RootElement.Deserialize<Groups.GroupsLibrary>(ProjectJson.options))
                with e -> Error(JsonParseError e.Message)

    /// The single fixed per-user groups path under the OS application-data folder (G.3.2 — outside
    /// the repository working tree, so a committable-vs-gitignore non-issue). Mirrors the
    /// environment store's `Softellect/Berreman/OpticalConstructor` location convention
    /// (`UserEnvironment.fs:333`); the convention is replicated here because the Storage layer does
    /// not reference the Ui layer where the environment store lives.
    let libraryPath () : string =
        let dir =
            Path.Combine(
                Environment.GetFolderPath Environment.SpecialFolder.ApplicationData,
                "Softellect", "Berreman", "OpticalConstructor")
        Path.Combine(dir, "groups.json")

    /// Load the library from `path`, falling back to the empty library on ANY failure — missing
    /// file, malformed JSON, or schema-validation failure (no migration). Total
    /// `path -> GroupsLibrary`, so the UI never has to handle an error case just to render a usable
    /// (empty) library.
    let load (path : string) : Groups.GroupsLibrary =
        try
            if File.Exists path then
                match deserialize (File.ReadAllText path) with
                | Ok l -> l
                | Error _ -> Groups.GroupsLibrary.empty
            else Groups.GroupsLibrary.empty
        with _ -> Groups.GroupsLibrary.empty

    /// Persist the library to `path` as schema-validated canonical JSON (G.4). A serialization or
    /// write failure returns `StorageError` for the UI to surface; it never throws across this
    /// boundary.
    let save (path : string) (library : Groups.GroupsLibrary) : Result<unit, StorageError> =
        match serialize library with
        | Error e -> Error e
        | Ok json ->
            try
                Directory.CreateDirectory(Path.GetDirectoryName path) |> ignore
                File.WriteAllText(path, json)
                Ok()
            with e -> Error(FileIoError e)
