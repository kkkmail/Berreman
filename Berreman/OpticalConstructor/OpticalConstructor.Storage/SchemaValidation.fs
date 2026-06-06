namespace OpticalConstructor.Storage

open System.IO
open System.Text.Json
open Json.Schema
open OpticalConstructor.Storage.Errors

/// JSON-Schema validate-on-load (§I.2 / R-2) via JsonSchema.Net. The published
/// schema is a build-copied content item shipped beside the assembly; it is
/// loaded and `Evaluate`d against the parsed document BEFORE the domain model is
/// bound. Compiled before `ProjectJson` so `deserializeProject` can call it.
module SchemaValidation =

    /// Location of the build-copied schema content item, relative to the assembly
    /// base directory (the fsproj copies `schema/...schema.json` preserving this
    /// relative path).
    let schemaRelativePath =
        Path.Combine("schema", "optical-constructor-project.schema.json")

    let private schemaFullPath =
        Path.Combine(System.AppContext.BaseDirectory, schemaRelativePath)

    /// Load the published schema ONCE (§I.2 — "load the schema once into a
    /// `JsonSchema` value"). `JsonSchema.FromText` registers the document by its
    /// `$id` in a process-global registry, so re-loading the same `$id` throws;
    /// caching behind `lazy` makes validate-on-load idempotent across calls.
    /// Returns `SchemaMissing` when the content item is absent at runtime,
    /// `FileIoError` on a read failure.
    let private schema : Result<JsonSchema, StorageError> Lazy =
        lazy
            if File.Exists schemaFullPath then
                try
                    File.ReadAllText schemaFullPath |> JsonSchema.FromText |> Ok
                with e -> Error (FileIoError e)
            else
                Error SchemaMissing

    /// Flatten a JsonSchema.Net evaluation tree into a list of human-readable
    /// messages (`<instance-location>: <keyword> -> <message>`), preserving the
    /// library's own wording verbatim (§I.2 — the messages travel into
    /// `SchemaValidationError`). Walks `results.Details` recursively so a NESTED
    /// violation keeps its precise instance location. Public so the groups store
    /// reuses this exact seam (slice 007 / R-4) instead of re-implementing a
    /// shallower, top-level-only collector that drops nested detail.
    let collectMessages (results : EvaluationResults) : string list =
        let rec walk (r : EvaluationResults) =
            seq {
                match r.Errors with
                | null -> ()
                | errs ->
                    for kv in errs do
                        yield $"{r.InstanceLocation}: {kv.Key} -> {kv.Value}"
                match r.Details with
                | null -> ()
                | details ->
                    for child in details do
                        yield! walk child
            }
        walk results |> Seq.toList

    /// Validate `element` against the published schema. On any evaluation failure
    /// returns `SchemaValidationError` carrying the JsonSchema.Net messages; the
    /// caller MUST NOT bind a project on `Error`.
    let validate (element : JsonElement) : Result<unit, StorageError> =
        match schema.Value with
        | Error e -> Error e
        | Ok schema ->
            let options = EvaluationOptions(OutputFormat = OutputFormat.List)
            let results = schema.Evaluate(element, options)
            if results.IsValid then Ok ()
            else
                let messages = collectMessages results
                let messages =
                    if List.isEmpty messages then [ "schema validation failed" ]
                    else messages
                Error (SchemaValidationError messages)
