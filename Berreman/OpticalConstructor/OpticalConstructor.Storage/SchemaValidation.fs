namespace OpticalConstructor.Storage

open System.IO
open System.Text.Json
open Json.Schema
open Softellect.Sys.Primitives
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Storage.Errors

/// JSON-Schema validate-on-load (§I.2 / R-2; spec 0050 S4.4) via JsonSchema.Net.
/// Each format loads its build-copied schema once and validates before binding.
module SchemaValidation =

    type FileName with

        /// Read and parse the build-copied schema content item at this path, relative to the assembly base
        /// directory (the fsproj copies `schema/*.schema.json` preserving it): `SchemaMissingErr` when the item
        /// is absent at runtime, `FileIoExn` on a read failure. `JsonSchema.FromText` registers the document by
        /// its `$id` in a process-global registry and throws on a second load of the same `$id`, so each owner
        /// loads its schema once, behind a `lazy`.
        member this.tryLoadSchema () : Result<JsonSchema, StorageError> =
            let fullPath = this.combine (FolderName System.AppContext.BaseDirectory)
            if File.Exists fullPath.value then
                try
                    File.ReadAllText fullPath.value |> JsonSchema.FromText |> Ok
                with e -> Error (FileIoExn e)
            else
                Error SchemaMissingErr

    /// Flatten a JsonSchema.Net evaluation tree into `<instance-location>: <keyword> -> <message>` lines, in
    /// the library's own wording (§I.2). Walks `results.Details` so a NESTED violation keeps its location.
    let private collectMessages (results : EvaluationResults) : string list =
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

    /// Validate `element` against a build-copied schema: the schema's own load error, or on any evaluation
    /// failure `SchemaValidationErr` carrying the JsonSchema.Net messages; the caller MUST NOT bind on
    /// `Error`. The one evaluation body every JSON format shares (spec 0050 `.manual/001` §B "JSON Schema
    /// per kind"); each owner passes its own schema, loaded once behind its `lazy`.
    let validateWith (schema : Lazy<Result<JsonSchema, StorageError>>) (element : JsonElement) : Result<unit, StorageError> =
        match schema.Value with
        | Error e -> Error e
        | Ok loaded ->
            let options = EvaluationOptions(OutputFormat = OutputFormat.List)
            let results = loaded.Evaluate(element, options)
            if results.IsValid then Ok ()
            else
                let messages = collectMessages results
                let messages =
                    if List.isEmpty messages then [ "schema validation failed" ]
                    else messages
                Error (SchemaValidationErr messages)

    let private libraryCommonSchemaFile : FileName = FileName (Path.Combine("schema", "library-common.schema.json"))

    /// The definitions every entry schema references by its `$id`, loaded ONCE (spec 0050 001 §B).
    let private libraryCommonSchema : Result<JsonSchema, StorageError> Lazy = lazy (libraryCommonSchemaFile.tryLoadSchema ())

    type FileName with

        /// A library schema, loaded after the common one, so its `$ref`s resolve when it is built.
        member private this.tryLoadLibrarySchema () : Result<JsonSchema, StorageError> =
            libraryCommonSchema.Value |> Result.bind (fun _ -> this.tryLoadSchema ())

    type LibraryEntryKind with

        /// The kind's build-copied entry schema (spec 0050 001 §B "JSON Schema per kind").
        member this.schemaFile : FileName =
            let kindFile =
                match this with
                | MaterialEntryKind -> "library-material.schema.json"
                | SampleEntryKind -> "library-sample.schema.json"
                | SourceEntryKind -> "library-source.schema.json"
                | PolarizerEntryKind -> "library-polarizer.schema.json"
                | DetectorEntryKind -> "library-detector.schema.json"
                | CategoryEntryKind -> "library-category.schema.json"
                | ItemModelEntryKind -> "library-item-model.schema.json"
            FileName (Path.Combine("schema", kindFile))

    let private materialEntrySchema : Result<JsonSchema, StorageError> Lazy = lazy (MaterialEntryKind.schemaFile.tryLoadLibrarySchema ())
    let private sampleEntrySchema : Result<JsonSchema, StorageError> Lazy = lazy (SampleEntryKind.schemaFile.tryLoadLibrarySchema ())
    let private sourceEntrySchema : Result<JsonSchema, StorageError> Lazy = lazy (SourceEntryKind.schemaFile.tryLoadLibrarySchema ())
    let private polarizerEntrySchema : Result<JsonSchema, StorageError> Lazy = lazy (PolarizerEntryKind.schemaFile.tryLoadLibrarySchema ())
    let private detectorEntrySchema : Result<JsonSchema, StorageError> Lazy = lazy (DetectorEntryKind.schemaFile.tryLoadLibrarySchema ())
    let private categoryEntrySchema : Result<JsonSchema, StorageError> Lazy = lazy (CategoryEntryKind.schemaFile.tryLoadLibrarySchema ())
    let private itemModelEntrySchema : Result<JsonSchema, StorageError> Lazy = lazy (ItemModelEntryKind.schemaFile.tryLoadLibrarySchema ())

    /// Validate `element` against the entry schema of `kind` through `validateWith` (spec 0050 001 §B): a file
    /// of another kind fails here, and the caller MUST NOT bind an entry on `Error`.
    let validateEntry (kind : LibraryEntryKind) (element : JsonElement) : Result<unit, StorageError> =
        let schema =
            match kind with
            | MaterialEntryKind -> materialEntrySchema
            | SampleEntryKind -> sampleEntrySchema
            | SourceEntryKind -> sourceEntrySchema
            | PolarizerEntryKind -> polarizerEntrySchema
            | DetectorEntryKind -> detectorEntrySchema
            | CategoryEntryKind -> categoryEntrySchema
            | ItemModelEntryKind -> itemModelEntrySchema
        validateWith schema element

    let private catalogueManifestSchemaFile : FileName = FileName (Path.Combine("schema", "library-catalogue.schema.json"))

    /// The `.catalogue-json` schema, loaded ONCE (spec 0050 001 §A, §B).
    let private catalogueManifestSchema : Result<JsonSchema, StorageError> Lazy = lazy (catalogueManifestSchemaFile.tryLoadLibrarySchema ())

    /// Validate `element` against the catalogue manifest's schema through `validateWith` (spec 0050 001 §B);
    /// the caller MUST NOT bind a manifest on `Error`.
    let validateCatalogueManifest (element : JsonElement) : Result<unit, StorageError> = validateWith catalogueManifestSchema element

    let private kitFileSchemaFile : FileName = FileName (Path.Combine("schema", "kit.schema.json"))

    let private kitFileSchema : Result<JsonSchema, StorageError> Lazy = lazy (kitFileSchemaFile.tryLoadLibrarySchema ())

    /// Validates a kit file before its version is compared and its metadata binds (spec 0050 S4.1).
    let validateKitFile (element : JsonElement) : Result<unit, StorageError> = validateWith kitFileSchema element

    let private experimentFileSchemaFile : FileName = FileName (Path.Combine("schema", "experiment.schema.json"))

    let private experimentFileSchema : Result<JsonSchema, StorageError> Lazy = lazy (experimentFileSchemaFile.tryLoadLibrarySchema ())

    /// Validates an experiment file before its version is compared and its experiment binds (spec 0050 S4.3).
    let validateExperimentFile (element : JsonElement) : Result<unit, StorageError> = validateWith experimentFileSchema element

    let private sceneFileSchemaFile : FileName = FileName (Path.Combine("schema", "setup.schema.json"))

    let private sceneFileSchema : Result<JsonSchema, StorageError> Lazy = lazy (sceneFileSchemaFile.tryLoadLibrarySchema ())

    /// Validates a setup file before its version is compared and its scene binds (spec 0050 S4.3).
    let validateSceneFile (element : JsonElement) : Result<unit, StorageError> = validateWith sceneFileSchema element

    let private kitHistorySchemaFile : FileName = FileName (Path.Combine("schema", "kit-history.schema.json"))

    let private kitHistorySchema : Result<JsonSchema, StorageError> Lazy = lazy (kitHistorySchemaFile.tryLoadLibrarySchema ())

    /// Validates the kit history file before its version is compared and its history binds (spec 0050 S5.1).
    let validateKitHistory (element : JsonElement) : Result<unit, StorageError> = validateWith kitHistorySchema element
