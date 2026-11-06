namespace OpticalConstructor.Storage

open Softellect.Sys.Primitives
open OpticalConstructor.Domain.LibraryEntryFiles

/// The sole error channel for the storage layer (§I.1 / R-1). Every storage IO
/// function returns `Result<_, StorageError>` and NEVER throws across its own
/// boundary. This DU is net-new and storage-owned; it does NOT wrap or shadow
/// `Softellect.Sys.Errors`.
module Errors =

    /// Names LibraryDbContext's model (spec 0050 S2.1, step 60). Raise this version together with
    /// the table listing in `the library model creates exactly its seven tables and their columns`
    /// whenever that model changes.
    type LibrarySchemaVersion =
        | LibrarySchemaVersion of int

        member this.value : int = let (LibrarySchemaVersion version) = this in version
        static member current : LibrarySchemaVersion = LibrarySchemaVersion 1

    type LibrarySchemaMismatchInfo =
        {
            foundOpt : LibrarySchemaVersion option
            expected : LibrarySchemaVersion
        }

    /// A stored file whose schema version this app does not read (spec 0050 001 §B "No up-converters"). The
    /// extension names the file's kind, so S4's kit files reuse the case.
    type SchemaVersionMismatchInfo =
        {
            fileExtension : FileExtension
            found : FileSchemaVersion
            expected : FileSchemaVersion
        }

    /// An entry file whose recorded hash is not the hash of the content it holds (spec 0050 001 §B).
    type ContentHashMismatchInfo =
        {
            entryId : LibraryEntryId
            recorded : ContentHash
            computed : ContentHash
        }

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
    ///   * `UnsupportedSchemaVersionErr` — a stored file declares a schema version
    ///                             this app does not read (spec 0050 001 §B).
    ///   * `ContentHashMismatchErr` — an entry file's recorded hash is not its
    ///                             content's (spec 0050 001 §B).
    ///   * `LibrarySchemaMismatchErr` — the library model's schema stamp is missing or incompatible.
    ///   * `LibraryDatabaseExn` — an exception caught at the SQLite / EF Core boundary.
    type StorageError =
        | MalformedJsonErr of string
        | SchemaValidationErr of string list
        | SchemaMissingErr
        | FileIoExn of exn
        | BinzExn of exn
        | SidecarMissingErr of FileName
        | UnsupportedSchemaVersionErr of SchemaVersionMismatchInfo
        | ContentHashMismatchErr of ContentHashMismatchInfo
        | LibrarySchemaMismatchErr of LibrarySchemaMismatchInfo
        | LibraryDatabaseExn of exn

        /// The one-line render a Domain error's reason carries, so no caller formats this union with `%A` (ELEV002;
        /// spec 0050 .spec-md §4).
        member this.describe : string =
            match this with
            | MalformedJsonErr text -> text
            | SchemaValidationErr messages -> String.concat "; " messages
            | SchemaMissingErr -> "a build-copied schema file is missing beside the assembly"
            | FileIoExn e
            | BinzExn e
            | LibraryDatabaseExn e -> e.Message
            | SidecarMissingErr file -> $"the sidecar file '{file.value}' is missing"
            | UnsupportedSchemaVersionErr info -> $"a {info.fileExtension.value} file declares schema version {info.found.value}, and this app reads version {info.expected.value}"
            | ContentHashMismatchErr info -> $"entry {info.entryId.value}: recorded hash {info.recorded.value}, computed hash {info.computed.value}"
            | LibrarySchemaMismatchErr info ->
                let found =
                    match info.foundOpt with
                    | Some version -> $"schema version {version.value}"
                    | None -> "no stamp"

                $"the library database has {found}; expected schema version {info.expected.value}"
