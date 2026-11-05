namespace OpticalConstructor.Storage

open System.Text.Json
open System.Text.Json.Nodes
open OpticalConstructor.Domain.Project
open OpticalConstructor.Storage.Errors

/// Canonical project (de)serialization (§I.1 / R-1) over the §A.7
/// `OpticalConstructorProject` aggregate, using System.Text.Json +
/// FSharp.SystemTextJson. No Newtonsoft, no FsPickler on this path (binding
/// rule 4). The serializer writes the domain model's canonical SI fields
/// verbatim — no unit conversion, rounding, or canonicalization of magnitudes.
module ProjectJson =

    /// The single shipped schema version (§I.2). A document whose `schemaVersion`
    /// differs surfaces as `SchemaValidationErr` (no migration; AC-I3). Pinned
    /// with `const` in the schema and injected into the serialized envelope.
    [<Literal>]
    let schemaVersion = "1.0"

    /// A project's canonical JSON text (§I.1): what an `.ocproj` file and a history snapshot hold.
    type ProjectJsonText =
        | ProjectJsonText of string

        member this.value = let (ProjectJsonText t) = this in t

        /// Parse and validate-on-load, then bind (§I.1 / §I.2 / R-1, R-2). Schema
        /// validation ALWAYS runs before the domain model is admitted; on any
        /// validation failure the result is `SchemaValidationErr` and nothing is
        /// bound. A `schemaVersion` mismatch fails the schema's `const` and so
        /// surfaces as `SchemaValidationErr`, never a migration (AC-I3).
        member this.parseProject : Result<OpticalConstructorProject, StorageError> =
            let parsed =
                try Ok(JsonDocument.Parse this.value)
                with e -> Error(MalformedJsonErr e.Message)

            match parsed with
            | Error e -> Error e
            | Ok doc ->
                use doc = doc
                match SchemaValidation.validate doc.RootElement with
                | Error e -> Error e
                | Ok () ->
                    try Ok(doc.RootElement.Deserialize<OpticalConstructorProject>(CanonicalJson.options))
                    with e -> Error(MalformedJsonErr e.Message)

    type OpticalConstructorProject with

        /// Serialize this project to canonical JSON text (§I.1 / R-1), injecting the
        /// envelope's `schemaVersion`. The aggregate carries no `schemaVersion` field
        /// of its own; it is an envelope concern, added here and validated on load.
        member this.serializeProject : Result<ProjectJsonText, StorageError> =
            try
                let node = JsonSerializer.SerializeToNode(this, CanonicalJson.options)
                let envelope = node.AsObject()
                envelope.["schemaVersion"] <- JsonValue.Create(schemaVersion)
                Ok(ProjectJsonText (envelope.ToJsonString(CanonicalJson.options)))
            with e -> Error(MalformedJsonErr e.Message)
