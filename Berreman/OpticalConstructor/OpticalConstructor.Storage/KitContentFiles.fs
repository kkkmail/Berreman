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

        /// Validates the experiment schema and rejects unsupported versions before binding (spec 0050 S4.3). A descriptor
        /// without its `itemIdOpt` is refused, so an older body never binds with an inferred item (spec 0050 S7.2).
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

    /// What a copy brought into the kit, in write order: each experiment as written, each data file copied in and each
    /// snapshot added (spec 0050 S6).
    type ExperimentCopyReport =
        {
            experiments : Experiment list
            dataFiles : DataAttachment list
            snapshots : LibraryVersionRef list
        }

    type KitContext with

        /// Copies the chosen experiments of the source, with the data files they reference and the source's snapshots of
        /// their closure this kit lacks, renumbered on a clash. Everything is read before anything is written, and the
        /// source is only read; the first failure stops the copy, keeping what it wrote (spec 0050 S6).
        member this.copyExperimentsFrom (source : KitContext) (ids : ExperimentId list) : Result<ExperimentCopyReport, KitLibraryError> =
            let keep (offered : Experiment list) : Result<Experiment list, KitLibraryError> =
                match ids |> List.tryFind (fun id -> not (offered |> List.exists (fun experiment -> experiment.id = id))) with
                | Some missing -> Error (KitLibraryKitErr (MissingKitFileErr (ExperimentKitFile.fileNameOf missing.fileStem).value))
                | None -> Ok (offered |> List.filter (fun experiment -> List.contains experiment.id ids))

            // The source's own snapshots answer the closure: the exact version, or the entry's highest one (spec 0050 S6).
            let lackedSnapshots (kept : Experiment list) : Result<LibraryEntryEnvelope list, KitLibraryError> =
                source.proxy.listSnapshots ()
                |> Result.mapError KitLibraryKitErr
                |> Result.bind (fun offered ->
                    let resolve (reference : EntryReference) : Result<LibraryEntryEnvelope, KitLibraryError> =
                        let candidates =
                            match reference with
                            | EntryVersionReference version -> offered |> List.filter (fun envelope -> envelope.libraryVersionRef = version)
                            | EntryLatestReference id -> offered |> List.filter (fun envelope -> envelope.content.id = id)
                        match candidates |> List.sortByDescending (fun envelope -> envelope.content.version) |> List.tryHead with
                        | Some envelope -> Ok envelope
                        | None -> Error (UnheldEntryErr reference.describe)

                    referenceClosure resolve (kept |> List.collect (fun experiment -> experiment.libraryReferences)))
                |> Result.bind (fun closure ->
                    this.proxy.listSnapshots ()
                    |> Result.mapError KitLibraryKitErr
                    |> Result.map (fun held ->
                        let heldVersions = held |> List.map (fun envelope -> envelope.libraryVersionRef)
                        closure |> List.filter (fun envelope -> not (List.contains envelope.libraryVersionRef heldVersions))))

            let readDataFiles (kept : Experiment list) : Result<(KitRelativePath * LabFileSource) list, KitLibraryError> =
                kept
                |> List.choose (fun experiment -> experiment.dataFileOpt |> Option.map (fun attachment -> attachment.path))
                |> List.distinct
                |> List.fold (fun result path ->
                    result |> Result.bind (fun read ->
                        source.proxy.readLabFile path
                        |> Result.mapError KitLibraryKitErr
                        |> Result.map (fun lab -> (path, lab) :: read))) (Ok [])
                |> Result.map List.rev

            let renumber (kept : Experiment list) : Result<Experiment list, KitLibraryError> =
                this.proxy.readExperiments ()
                |> Result.mapError KitLibraryKitErr
                |> Result.map (fun held -> Experiment.renumberedFor (held |> List.map (fun experiment -> experiment.id)) kept)

            let writeSnapshots (snapshots : LibraryEntryEnvelope list) : Result<LibraryVersionRef list, KitLibraryError> =
                snapshots
                |> List.fold (fun result envelope ->
                    result |> Result.bind (fun written ->
                        this.proxy.writeSnapshot envelope
                        |> Result.mapError KitLibraryKitErr
                        |> Result.map (fun () -> envelope.libraryVersionRef :: written))) (Ok [])
                |> Result.map List.rev

            // A file keeps its data folder and takes the next free suffix on a clash (spec 0050 S4.2, S6).
            let copyDataFiles (files : (KitRelativePath * LabFileSource) list) : Result<(KitRelativePath * DataAttachment) list, KitLibraryError> =
                files
                |> List.fold (fun result (path, lab) ->
                    result |> Result.bind (fun copied ->
                        let folder =
                            match path.parentOpt |> Option.map KitDataFolder.tryCreate with
                            | Some (Ok dataFolder) -> dataFolder
                            | _ -> KitDataFolder.root
                        this.proxy.copyInFile folder lab.file.leafName lab.content
                        |> Result.mapError KitLibraryKitErr
                        |> Result.map (fun data -> (path, data.attachment) :: copied))) (Ok [])
                |> Result.map List.rev

            let writeExperiments (copied : (KitRelativePath * DataAttachment) list) (renumbered : Experiment list) : Result<Experiment list, KitLibraryError> =
                let moved = Map.ofList copied
                renumbered
                |> List.fold (fun result experiment ->
                    result |> Result.bind (fun written ->
                        let copy =
                            {
                                experiment with dataFileOpt = experiment.dataFileOpt |> Option.bind (fun attachment -> Map.tryFind attachment.path moved)
                            }
                        this.proxy.writeExperiment copy
                        |> Result.mapError KitLibraryKitErr
                        |> Result.map (fun () -> copy :: written))) (Ok [])
                |> Result.map List.rev

            // Snapshots and data come first, so a written experiment never references a file the kit lacks (spec 0050 S6).
            let writeAll
                (snapshots : LibraryEntryEnvelope list)
                (files : (KitRelativePath * LabFileSource) list)
                (renumbered : Experiment list)
                : Result<ExperimentCopyReport, KitLibraryError> =
                writeSnapshots snapshots
                |> Result.bind (fun added ->
                    copyDataFiles files
                    |> Result.bind (fun copied ->
                        writeExperiments copied renumbered
                        |> Result.map (fun experiments ->
                            let report : ExperimentCopyReport =
                                {
                                    experiments = experiments
                                    dataFiles = copied |> List.map snd
                                    snapshots = added
                                }
                            report)))

            match this.access with
            | ReadOnlyKitAccess -> Error (KitLibraryKitErr (KitReadOnlyErr "copyExperimentsFrom"))
            | ReadWriteKitAccess ->
                source.proxy.readExperiments ()
                |> Result.mapError KitLibraryKitErr
                |> Result.bind keep
                |> Result.bind (fun kept ->
                    lackedSnapshots kept
                    |> Result.bind (fun snapshots ->
                        readDataFiles kept
                        |> Result.bind (fun files -> renumber kept |> Result.bind (writeAll snapshots files))))
