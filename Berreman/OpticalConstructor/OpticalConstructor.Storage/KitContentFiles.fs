namespace OpticalConstructor.Storage

open System
open Softellect.Sys.Primitives
open OpticalConstructor.Domain.Experiments
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.KitFolders
open OpticalConstructor.Domain.KitLibrary
open OpticalConstructor.Domain.LibraryEntryFiles
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

    /// Writes a rendered content file whole under its kind and stem; a rendering refusal names the file
    /// (spec 0050 S4.3, S7.1).
    let private writeRenderedKindFile
        (proxy : KitProxy)
        (kind : KitFileKind)
        (stem : FileStem)
        (rendered : Result<KitFileText, StorageError>)
        : Result<unit, KitError> =
        rendered
        |> Result.mapError (contentStorageRejectedErr (kind.fileNameOf stem))
        |> Result.bind (proxy.writeKindFile kind stem)

    /// Reads and parses every file of the kind in listing order, accepting each against those read before it; the
    /// first refusal, naming its file, answers the whole read (spec 0050 S4.3, S7.1).
    let private readEveryKindFile<'Parsed, 'Content>
        (proxy : KitProxy)
        (kind : KitFileKind)
        (parse : KitFileText -> Result<'Parsed, StorageError>)
        (accept : FileName -> FileStem -> 'Content list -> 'Parsed -> Result<'Content, KitError>)
        : Result<'Content list, KitError> =
        proxy.listKindFiles kind
        |> Result.bind (fun stems ->
            stems
            |> List.fold (fun result stem ->
                result |> Result.bind (fun contents ->
                    let file = kind.fileNameOf stem
                    proxy.readKindFile kind stem
                    |> Result.mapError (contentReadRejectedErr file)
                    |> Result.bind (fun text -> parse text |> Result.mapError (contentStorageRejectedErr file))
                    |> Result.bind (accept file stem contents)
                    |> Result.map (fun content -> content :: contents))) (Ok []))

    type KitProxy with

        /// Replaces the experiment's kit file whole, naming any rendering refusal (spec 0050 S4.3).
        member this.writeExperiment (experiment : Experiment) : Result<unit, KitError> =
            writeRenderedKindFile this ExperimentKitFile experiment.id.fileStem experiment.experimentFile.fileText

        /// Deletes the experiment's kit file, or reports it missing (spec 0050 S4.3).
        member this.deleteExperiment (id : ExperimentId) : Result<unit, KitError> =
            this.deleteKindFile ExperimentKitFile id.fileStem

        /// Reads every experiment in id order; an unreadable or misnamed file refuses the whole read (spec 0050 S4.3).
        member this.readExperiments () : Result<Experiment list, KitError> =
            let accept (file : FileName) (stem : FileStem) (_ : Experiment list) (parsed : ExperimentFile) : Result<Experiment, KitError> =
                let experiment = parsed.experiment
                if String.Equals(experiment.id.fileStem.value, stem.value, StringComparison.OrdinalIgnoreCase) then
                    Ok experiment
                else
                    Error (KitFileRejectedErr $"the file '{file.value}' holds experiment '{experiment.id.fileStem.value}' instead of '{stem.value}'")

            readEveryKindFile this ExperimentKitFile (fun text -> text.parseExperimentFile) accept
            |> Result.map (List.sortBy (fun experiment -> experiment.id))

        /// Replaces the setup file at the name's safe stem, preserving its original name (spec 0050 S4.3).
        member this.writeScene (name : SceneName) (scene : SceneSnapshot) : Result<unit, KitError> =
            let sceneFile : SceneFile =
                {
                    schemaVersion = SceneFile.currentSchemaVersion
                    name = name
                    scene = scene
                }
            writeRenderedKindFile this SetupKitFile name.fileStem sceneFile.fileText

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
            let accept (_ : FileName) (_ : FileStem) (_ : SceneName list) (parsed : SceneFile) : Result<SceneName, KitError> =
                Ok parsed.name

            readEveryKindFile this SetupKitFile (fun text -> text.parseSceneFile) accept
            |> Result.map (List.sortWith (fun first second -> String.CompareOrdinal(first.value, second.value)))

        /// Replaces the item's `<label>.item-json` whole, naming any rendering refusal; a file under an earlier label
        /// stays (spec 0050 S7.1).
        member this.writeItem (item : Item) : Result<unit, KitError> =
            writeRenderedKindFile this ItemKitFile item.label.fileStem item.itemFile.fileText

        /// Reads every item in its label's ordinal order. A file that does not read, holds another label or repeats an id
        /// refuses the whole read, so a kit never opens with an item left out (spec 0050 S7.1).
        member this.readItems () : Result<Item list, KitError> =
            // Files list in ordinal order, so a repeated id names both of its files, whichever was read first.
            let accept (file : FileName) (stem : FileStem) (read : (FileName * Item) list) (parsed : ItemFile) : Result<FileName * Item, KitError> =
                let item = parsed.item
                if String.Equals(item.label.fileStem.value, stem.value, StringComparison.OrdinalIgnoreCase) then
                    match read |> List.tryFind (fun (_, held) -> held.id = item.id) with
                    | Some (heldFile, held) ->
                        Error (KitFileRejectedErr $"the file '{file.value}' repeats the id {item.id.value} of item '{held.label.value}' in '{heldFile.value}'")
                    | None -> Ok (file, item)
                else
                    Error (KitFileRejectedErr $"the file '{file.value}' holds item '{item.label.value}' instead of '{stem.value}'")

            readEveryKindFile this ItemKitFile (fun text -> text.parseItemFile) accept
            |> Result.map (List.map snd >> List.sortWith (fun first second -> String.CompareOrdinal(first.label.value, second.label.value)))

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

        /// Snapshots and pins the item's model version and its bound physics, each with its closure, before writing
        /// its `<label>.item-json`, so an item never names a version the kit lacks (spec 0050 S7.1).
        /// A failure stops recording and retains snapshots already written.
        member this.recordItem (item : Item) : Result<unit, KitLibraryError> =
            this.snapshotReferences item.libraryReferences
            |> Result.bind (fun _ -> this.kit.proxy.writeItem item |> Result.mapError KitLibraryKitErr)
