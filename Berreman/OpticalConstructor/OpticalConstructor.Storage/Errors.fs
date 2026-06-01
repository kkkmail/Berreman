namespace OpticalConstructor.Storage

/// The sole error channel for the storage layer (§I.1 / R-1). Every storage IO
/// function returns `Result<_, StorageError>` and NEVER throws across its own
/// boundary. This DU is net-new and storage-owned; it does NOT wrap or shadow
/// `Softellect.Sys.Errors`.
module Errors =

    /// Storage failure cases (§I.1 / R-1, R-2, R-4).
    ///   * `JsonParseError`        — malformed JSON text or a bind failure.
    ///   * `SchemaValidationError` — the JsonSchema.Net evaluation messages for a
    ///                               document that violated the published schema
    ///                               (including a `schemaVersion` mismatch — no
    ///                               migration path, §I.2).
    ///   * `SchemaMissing`         — the build-copied schema content item is absent.
    ///   * `FileIoError`           — any filesystem exception, captured not thrown.
    ///   * `BinzError`             — a `.binz` (de)serialization failure.
    ///   * `SidecarMissing`        — a referenced `.binz` sidecar is not on disk.
    type StorageError =
        | JsonParseError of string
        | SchemaValidationError of string list
        | SchemaMissing
        | FileIoError of exn
        | BinzError of exn
        | SidecarMissing of string
