namespace OpticalConstructor.Storage

open System
open System.IO
open System.Text.Json
open System.Text.Json.Nodes
open Json.Schema
open Softellect.Sys.Primitives
open OpticalConstructor.Domain
open OpticalConstructor.Storage.Errors
open OpticalConstructor.Storage.SchemaValidation

/// The separate per-user groups + experiment-collections store (Spec 0026 Part G/H, slice
/// 007 / R-3, R-4). Element groups (G.1) and experiment collections (H.1) are persisted in
/// their OWN JSON file, kept apart from the per-project `.ocproj`, so reusable groups and
/// sample-centred collections are available across projects (G.3.1). This module owns that
/// file's load/save and validate-on-load. It REUSES the slice-003 shared `CanonicalJson.options`
/// `JsonSerializerOptions` (§I.1 — never a second JSON stack) and the one schema evaluation body,
/// `SchemaValidation.validateWith` (R-4 / G.3.1; spec 0050 step 29). Only the lazy `JsonSchema`
/// itself is local, keyed by a distinct `$id`. No new serializer (G.4); no schema migration (a
/// `schemaVersion` mismatch is a `SchemaValidationErr`, never a migration). The file lives at a fixed per-user app-data path
/// outside the repository working tree (G.3.2), so it raises no `.gitignore` collision.
module GroupsLibrary =

    /// The single shipped groups-schema version, pinned with `const` in the schema. A differing
    /// value fails validation and so falls back to the empty library (no migration).
    [<Literal>]
    let groupsLibrarySchemaVersion = "1.0"

    /// The build-copied groups schema content item (the fsproj copies `schema/...groups.schema.json`
    /// preserving this relative path), loaded as the project schema is (`FileName.tryLoadSchema`).
    let private groupsSchemaFile : FileName =
        FileName (Path.Combine("schema", "optical-constructor-groups.schema.json"))

    /// The groups schema, loaded ONCE behind `lazy`: `JsonSchema.FromText` registers the document by
    /// its (distinct) `$id` in the process-global registry, so a single load keeps validate-on-load
    /// idempotent across calls.
    let private schema : Result<JsonSchema, StorageError> Lazy = lazy (groupsSchemaFile.tryLoadSchema ())

    /// Validate a parsed document against the groups schema (G.3.1), REUSING
    /// `SchemaValidation.validateWith`, so a NESTED groups-file violation keeps its instance location.
    /// The caller MUST NOT bind on `Error`.
    let validateGroupsLibrary (element : JsonElement) : Result<unit, StorageError> = validateWith schema element

    /// The groups/collections library's canonical JSON text (G.4), as its file holds it.
    type GroupsLibraryJsonText =
        | GroupsLibraryJsonText of string

        member this.value = let (GroupsLibraryJsonText t) = this in t

        /// Parse, validate-on-load, then bind (G.3.1 / G.4). Schema validation ALWAYS runs before the
        /// value is admitted; a `schemaVersion` mismatch fails the schema's `const` and surfaces as
        /// `SchemaValidationErr`, never a migration.
        member this.parseGroupsLibrary : Result<Groups.GroupsLibrary, StorageError> =
            let parsed =
                try Ok(JsonDocument.Parse this.value)
                with e -> Error(MalformedJsonErr e.Message)

            match parsed with
            | Error e -> Error e
            | Ok doc ->
                use doc = doc
                match validateGroupsLibrary doc.RootElement with
                | Error e -> Error e
                | Ok () ->
                    try Ok(doc.RootElement.Deserialize<Groups.GroupsLibrary>(CanonicalJson.options))
                    with e -> Error(MalformedJsonErr e.Message)

    /// Serialize the groups/collections library to canonical JSON (G.4), injecting the envelope
    /// `schemaVersion`, REUSING the slice-003 shared `CanonicalJson.options` (§I.1) — never a second
    /// JSON stack.
    let serialize (library : Groups.GroupsLibrary) : Result<GroupsLibraryJsonText, StorageError> =
        try
            let node = JsonSerializer.SerializeToNode(library, CanonicalJson.options)
            let envelope = node.AsObject()
            envelope.["schemaVersion"] <- JsonValue.Create(groupsLibrarySchemaVersion)
            Ok(GroupsLibraryJsonText (envelope.ToJsonString(CanonicalJson.options)))
        with e -> Error(MalformedJsonErr e.Message)

    /// The single fixed per-user groups path under the OS application-data folder (G.3.2 — outside
    /// the repository working tree, so a committable-vs-gitignore non-issue). Mirrors the
    /// environment store's `Softellect/Berreman/OpticalConstructor` location convention
    /// (`UserEnvironment.fs:333`); the convention is replicated here because the Storage layer does
    /// not reference the Ui layer where the environment store lives.
    let libraryPath () : FileName =
        let dir =
            Path.Combine(
                Environment.GetFolderPath Environment.SpecialFolder.ApplicationData,
                "Softellect", "Berreman", "OpticalConstructor")
        FileName (Path.Combine(dir, "groups.json"))

    type FileName with

        /// Load the library from this file, falling back to the empty library on ANY failure — missing
        /// file, malformed JSON, or schema-validation failure (no migration). Total, so the UI never has
        /// to handle an error case just to render a usable (empty) library.
        member this.loadGroupsLibrary () : Groups.GroupsLibrary =
            try
                if File.Exists this.value then
                    match (GroupsLibraryJsonText (File.ReadAllText this.value)).parseGroupsLibrary with
                    | Ok l -> l
                    | Error _ -> Groups.GroupsLibrary.empty
                else Groups.GroupsLibrary.empty
            with _ -> Groups.GroupsLibrary.empty

        /// Persist the library to this file as schema-validated canonical JSON (G.4). A serialization or
        /// write failure returns `StorageError` for the UI to surface; it never throws across this
        /// boundary.
        member this.saveGroupsLibrary (library : Groups.GroupsLibrary) : Result<unit, StorageError> =
            match serialize library with
            | Error e -> Error e
            | Ok json ->
                try
                    Directory.CreateDirectory(Path.GetDirectoryName this.value) |> ignore
                    File.WriteAllText(this.value, json.value)
                    Ok()
                with e -> Error(FileIoExn e)
