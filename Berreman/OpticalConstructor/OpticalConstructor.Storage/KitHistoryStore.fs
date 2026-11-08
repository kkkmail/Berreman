namespace OpticalConstructor.Storage

open Softellect.Sys.Primitives
open OpticalConstructor.Domain.KitHistory
open OpticalConstructor.Storage.Errors
open OpticalConstructor.Storage.CanonicalJson

/// The kit history file: canonical JSON validated by its own schema on read, another version a typed error
/// (spec 0050 S5.1).
module KitHistoryStore =

    /// The text of the kit history file (spec 0050 S5.1).
    type KitHistoryText =
        | KitHistoryText of string

        member this.value : string = let (KitHistoryText text) = this in text

    type KitHistory with

        /// The history file's name, the one place it is written (spec 0050 S5.1).
        static member fileName : FileName = FileName "kit-history.json"

        /// Canonical history JSON: sorted keys, LF line ends and one final newline (spec 0050 S5.1).
        member this.fileText : Result<KitHistoryText, StorageError> =
            nodeOf this |> Result.map (fun node -> node.canonicalFileText |> KitHistoryText)

    type KitHistoryText with

        member private this.toJsonText : JsonText = JsonText this.value

        /// Validates the history schema and refuses another schema version before binding; there are no
        /// up-converters and no fallback readers (spec 0050 S5.1).
        member this.parseKitHistory : Result<KitHistory, StorageError> =
            parseVersionedText SchemaValidation.validateKitHistory (FileExtension ".json") KitHistory.currentSchemaVersion this.toJsonText
