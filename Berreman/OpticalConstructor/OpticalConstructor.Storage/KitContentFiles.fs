namespace OpticalConstructor.Storage

open System
open Softellect.Sys.Primitives
open OpticalConstructor.Domain.Experiments
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.KitFolders
open OpticalConstructor.Domain.KitLibrary
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

    type ItemFile with

        /// Canonical item JSON with LF line ends and one final newline (spec 0050 S7.1).
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

        /// Validates the item schema and rejects unsupported versions before binding; no older version is read (spec 0050 S7.1).
        member this.parseItemFile : Result<ItemFile, StorageError> =
            parseVersionedText SchemaValidation.validateItemFile ItemKitFile.extension ItemFile.currentSchemaVersion this.toContentJsonText

    let private contentStorageRejectedErr (file : FileName) (error : StorageError) : KitError =
        KitFileRejectedErr $"the file '{file.value}': {error.describe}"

    let private contentReadRejectedErr (file : FileName) (error : KitError) : KitError =
        KitFileRejectedErr $"reading the file '{file.value}': {error.describe}"

    type KitProxy with

        /// Replaces the experiment's kit file whole, naming any rendering refusal (spec 0050 S4.3).
        member this.writeExperiment (experiment : Experiment) : Result<unit, KitError> =
            let stem = experiment.id.fileStem
            let file = ExperimentKitFile.fileNameOf stem
            experiment.experimentFile.fileText
            |> Result.mapError (contentStorageRejectedErr file)
            |> Result.bind (this.writeKindFile ExperimentKitFile stem)

        /// Deletes the experiment's kit file, or reports it missing (spec 0050 S4.3).
        member this.deleteExperiment (id : ExperimentId) : Result<unit, KitError> =
            this.deleteKindFile ExperimentKitFile id.fileStem

        /// Reads every experiment in id order; an unreadable or misnamed file refuses the whole read (spec 0050 S4.3).
        member this.readExperiments () : Result<Experiment list, KitError> =
            this.listKindFiles ExperimentKitFile
            |> Result.bind (fun stems ->
                stems
                |> List.fold (fun result stem ->
                    result |> Result.bind (fun experiments ->
                        let file = ExperimentKitFile.fileNameOf stem
                        this.readKindFile ExperimentKitFile stem
                        |> Result.mapError (contentReadRejectedErr file)
                        |> Result.bind (fun text -> text.parseExperimentFile |> Result.mapError (contentStorageRejectedErr file))
                        |> Result.bind (fun parsed ->
                            let experiment = parsed.experiment
                            if String.Equals(experiment.id.fileStem.value, stem.value, StringComparison.OrdinalIgnoreCase) then
                                Ok (experiment :: experiments)
                            else
                                Error (KitFileRejectedErr $"the file '{file.value}' holds experiment '{experiment.id.fileStem.value}' instead of '{stem.value}'")))) (Ok []))
            |> Result.map (List.sortBy (fun experiment -> experiment.id))

        /// Replaces the setup file at the name's safe stem, preserving its original name (spec 0050 S4.3).
        member this.writeScene (name : SceneName) (scene : SceneSnapshot) : Result<unit, KitError> =
            let stem = name.fileStem
            let file = SetupKitFile.fileNameOf stem
            let sceneFile : SceneFile =
                {
                    schemaVersion = SceneFile.currentSchemaVersion
                    name = name
                    scene = scene
                }
            sceneFile.fileText
            |> Result.mapError (contentStorageRejectedErr file)
            |> Result.bind (this.writeKindFile SetupKitFile stem)

        /// Reads the named setup's scene, reporting a missing or refused file by name (spec 0050 S4.3).
        member this.readScene (name : SceneName) : Result<SceneSnapshot, KitError> =
            let stem = name.fileStem
            let file = SetupKitFile.fileNameOf stem
            this.readKindFile SetupKitFile stem
            |> Result.mapError (fun error ->
                match error with
                | MissingKitFileErr _ -> error
                | _ -> contentReadRejectedErr file error)
            |> Result.bind (fun text -> text.parseSceneFile |> Result.mapError (contentStorageRejectedErr file))
            |> Result.map (fun parsed -> parsed.scene)

        /// Reads original setup names in ordinal order; any refused file refuses the list (spec 0050 S4.3).
        member this.readSceneNames () : Result<SceneName list, KitError> =
            this.listKindFiles SetupKitFile
            |> Result.bind (fun stems ->
                stems
                |> List.fold (fun result stem ->
                    result |> Result.bind (fun names ->
                        let file = SetupKitFile.fileNameOf stem
                        this.readKindFile SetupKitFile stem
                        |> Result.mapError (contentReadRejectedErr file)
                        |> Result.bind (fun text -> text.parseSceneFile |> Result.mapError (contentStorageRejectedErr file))
                        |> Result.map (fun parsed -> parsed.name :: names))) (Ok []))
            |> Result.map (List.sortWith (fun first second -> String.CompareOrdinal(first.value, second.value)))

    type KitLibraryContext with

        /// Snapshots and pins the experiment's library closure before writing its file (spec 0050 S4.3).
        /// A failure stops recording and retains snapshots already written.
        member this.recordExperiment (experiment : Experiment) : Result<unit, KitLibraryError> =
            this.snapshotReferences experiment.libraryReferences
            |> Result.bind (fun _ -> this.kit.proxy.writeExperiment experiment |> Result.mapError KitLibraryKitErr)

        /// Snapshots and pins the scene's bound entries before writing its named file (spec 0050 S4.3).
        /// A failure stops recording and retains snapshots already written.
        member this.recordScene (name : SceneName) (scene : SceneSnapshot) : Result<unit, KitLibraryError> =
            this.snapshotReferences scene.libraryReferences
            |> Result.bind (fun _ -> this.kit.proxy.writeScene name scene |> Result.mapError KitLibraryKitErr)
