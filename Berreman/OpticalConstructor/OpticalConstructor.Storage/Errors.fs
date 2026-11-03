namespace OpticalConstructor.Storage

open Softellect.Sys.Primitives

/// The sole error channel for the storage layer (§I.1 / R-1). Every storage IO
/// function returns `Result<_, StorageError>` and NEVER throws across its own
/// boundary. This DU is net-new and storage-owned; it does NOT wrap or shadow
/// `Softellect.Sys.Errors`.
module Errors =

    /// Storage failure cases (§I.1 / R-1, R-2, R-4).
    ///   * `MalformedJsonErr`    — malformed JSON text or a bind failure.
    ///   * `SchemaValidationErr` — the JsonSchema.Net evaluation messages for a
    ///                             document that violated the published schema
    ///                             (including a `schemaVersion` mismatch — no
    ///                             migration path, §I.2).
    ///   * `SchemaMissingErr`    — the build-copied schema content item is absent.
    ///   * `FileIoExn`           — any filesystem exception, captured not thrown.
    ///   * `BinzExn`             — a `.binz` (de)serialization failure.
    ///   * `SidecarMissingErr`   — a referenced `.binz` sidecar is not on disk.
    type StorageError =
        | MalformedJsonErr of string
        | SchemaValidationErr of string list
        | SchemaMissingErr
        | FileIoExn of exn
        | BinzExn of exn
        | SidecarMissingErr of FileName
