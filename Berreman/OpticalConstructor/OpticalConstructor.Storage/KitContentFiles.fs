namespace OpticalConstructor.Storage

open Softellect.Sys.Primitives
open OpticalConstructor.Domain.Experiments
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.Scene
open OpticalConstructor.Storage.Errors
open OpticalConstructor.Storage.CanonicalJson

/// The kit's content files, their canonical texts and their typed reads and writes (spec 0050 S4.3).
module KitContentFiles =

    type ExperimentFile with

        /// Canonical experiment JSON with LF line ends and one final newline (spec 0050 S4.3).
        member this.fileText : Result<KitFileText, StorageError> =
            nodeOf this |> Result.map (fun node -> node.canonicalFileText |> KitFileText)

    type SceneFile with

        /// Canonical setup JSON preserves the original scene name and snapshot (spec 0050 S4.3).
        member this.fileText : Result<KitFileText, StorageError> =
            nodeOf this |> Result.map (fun node -> node.canonicalFileText |> KitFileText)

    type KitFileText with

        member private this.toContentJsonText : JsonText = JsonText this.value

        /// Validates the experiment schema and rejects unsupported versions before binding (spec 0050 S4.3).
        member this.parseExperimentFile : Result<ExperimentFile, StorageError> =
            parseVersionedText SchemaValidation.validateExperimentFile ExperimentKitFile.extension ExperimentFile.currentSchemaVersion this.toContentJsonText

        /// Validates the setup schema and rejects unsupported versions before binding (spec 0050 S4.3).
        member this.parseSceneFile : Result<SceneFile, StorageError> =
            parseVersionedText SchemaValidation.validateSceneFile SetupKitFile.extension SceneFile.currentSchemaVersion this.toContentJsonText
