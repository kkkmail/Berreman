namespace OpticalConstructor.Storage

open System
open Softellect.Sys.Logging
open Softellect.Sys.Primitives
open OpticalConstructor.Domain.Experiments
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.KitFolders
open OpticalConstructor.Domain.KitLibrary
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.Lifecycle
open OpticalConstructor.Domain.Setups
open OpticalConstructor.Storage.Errors
open OpticalConstructor.Storage.CanonicalJson

/// The kit's content files, their canonical texts and their typed reads and writes (spec 0050 S4.3).
module KitContentFiles =

    type ExperimentFile with

        /// Canonical experiment JSON with LF line ends and one final newline (spec 0050 S4.3).
        member this.fileText : Result<KitFileText, StorageError> =
            nodeOf this |> Result.map (fun node -> node.canonicalFileText |> KitFileText)

    type SetupFile with

        /// Canonical setup JSON with LF line ends and one final newline (spec 0050 S8.2).
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

        /// Validates the setup schema and rejects unsupported versions before binding; no older version is read (spec
        /// 0050 S8.2).
        member this.parseSetupFile : Result<SetupFile, StorageError> =
            parseVersionedText SchemaValidation.validateSetupFile SetupKitFile.extension SetupFile.currentSchemaVersion this.toContentJsonText

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

        /// Replaces the setup's `<name>.setup-json` whole at its name's safe stem, naming any rendering refusal (spec
        /// 0050 S8.2).
        member this.writeSetup (setup : Setup) : Result<unit, KitError> =
            writeRenderedKindFile this SetupKitFile setup.name.fileStem setup.setupFile.fileText

        /// Reads every setup in its name's ordinal order. A file that does not read, or holds a setup of another name,
        /// refuses the whole read, so a window never opens a kit with a setup left out (spec 0050 S8.2).
        member this.readSetups () : Result<Setup list, KitError> =
            let accept (file : FileName) (stem : FileStem) (_ : Setup list) (parsed : SetupFile) : Result<Setup, KitError> =
                let setup = parsed.setup
                if String.Equals(setup.name.fileStem.value, stem.value, StringComparison.OrdinalIgnoreCase) then
                    Ok setup
                else
                    Error (KitFileRejectedErr $"the file '{file.value}' holds setup '{setup.name.value}' instead of '{stem.value}'")

            readEveryKindFile this SetupKitFile (fun text -> text.parseSetupFile) accept
            |> Result.map (List.sortWith (fun first second -> String.CompareOrdinal(first.name.value, second.name.value)))

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

    type ItemModelInstance with

        /// The binding a descriptor takes from the item assigned to it: the preset's id, the sample's version, or none
        /// for a standalone model, never a binding of another piece (spec 0050 S7.2).
        member private this.assignedBinding : ElementBinding =
            match this with
            | PresetInstance (_, preset) -> BoundByEntryId preset
            | SampleInstance (_, version) -> BoundByVersion (SampleVersionRef version)
            | ItemModelVersionInstance _ -> Unbound

    type Experiment with

        /// The library references of the descriptors no item is assigned to, once each in setup order (spec 0050 S7.2).
        member private this.unassignedReferences : EntryReference list =
            this.setup
            |> List.choose (fun descriptor ->
                match descriptor.itemIdOpt with
                | Some _ -> None
                | None -> descriptor.binding.entryReferenceOpt)
            |> List.distinct

    /// The snapshot among the offered ones that answers the reference: the exact version, or the entry's highest one
    /// (spec 0050 S6).
    let private resolveAmong (offered : LibraryEntryEnvelope list) (reference : EntryReference) : Result<LibraryEntryEnvelope, KitLibraryError> =
        let candidates =
            match reference with
            | EntryVersionReference version -> offered |> List.filter (fun envelope -> envelope.libraryVersionRef = version)
            | EntryLatestReference id -> offered |> List.filter (fun envelope -> envelope.content.id = id)
        match candidates |> List.sortByDescending (fun envelope -> envelope.content.version) |> List.tryHead with
        | Some envelope -> Ok envelope
        | None -> Error (UnheldEntryErr reference.describe)

    type KitLibraryContext with

        /// Resolves the experiment's assigned items through the kit's items, a missing one refused before anything is
        /// written (spec 0050 S7.2); then snapshots and pins the unassigned bindings, the items' closures and the other
        /// bindings before writing its file (spec 0050 S4.3). A failure stops recording and retains snapshots already written.
        member this.recordExperiment (experiment : Experiment) : Result<unit, KitLibraryError> =
            this.kit.proxy.readItems ()
            |> Result.mapError KitLibraryKitErr
            |> Result.bind (fun items -> Experiment.itemsUsedBy [ experiment ] items |> Result.mapError KitLibraryItemErr)
            |> Result.bind (fun used ->
                let itemReferences = used |> List.collect (fun item -> item.libraryReferences)
                this.snapshotReferences (experiment.unassignedReferences @ itemReferences @ experiment.libraryReferences |> List.distinct))
            |> Result.bind (fun _ -> this.kit.proxy.writeExperiment experiment |> Result.mapError KitLibraryKitErr)

        /// Snapshots and pins the entries the setup's slots bind in every revision before writing its file, so a
        /// revision an experiment references never names an entry the kit lacks (spec 0050 S8.2).
        /// A failure stops recording and retains snapshots already written.
        member this.recordSetup (setup : Setup) : Result<unit, KitLibraryError> =
            this.snapshotReferences setup.libraryReferences
            |> Result.bind (fun _ -> this.kit.proxy.writeSetup setup |> Result.mapError KitLibraryKitErr)

        /// Snapshots and pins the item's model version and its bound physics, each with its closure, before writing
        /// its `<label>.item-json`, so an item never names a version the kit lacks (spec 0050 S7.1).
        /// A failure stops recording and retains snapshots already written.
        member this.recordItem (item : Item) : Result<unit, KitLibraryError> =
            this.snapshotReferences item.libraryReferences
            |> Result.bind (fun _ -> this.kit.proxy.writeItem item |> Result.mapError KitLibraryKitErr)

        /// Records `relabelled`, `previous` under a new label, and deletes the old label's file; a failure undoes what it
        /// wrote, so the item stays under its old label and never in two files of one id (spec 0050 S7.2).
        member this.recordRelabelledItem (previous : Item) (relabelled : Item) : Result<unit, KitLibraryError> =
            let proxy = this.kit.proxy
            let oldStem = previous.label.fileStem
            let newStem = relabelled.label.fileStem

            // The kit root is one Windows folder: a change of case alone is one file, so it is deleted before the rewrite.
            if String.Equals(oldStem.value, newStem.value, StringComparison.OrdinalIgnoreCase) then
                proxy.deleteKindFile ItemKitFile oldStem
                |> Result.mapError KitLibraryKitErr
                |> Result.bind (fun () ->
                    match this.recordItem relabelled with
                    | Ok () -> Ok ()
                    | Error error ->
                        match proxy.writeItem previous with
                        | Ok () -> ()
                        | Error restoreError ->
                            Logger.logError $"KitLibraryContext.recordRelabelledItem: could not restore '{previous.label.value}': {restoreError.describe}"
                        Error error)
            else
                this.recordItem relabelled
                |> Result.bind (fun () ->
                    match proxy.deleteKindFile ItemKitFile oldStem with
                    | Ok () -> Ok ()
                    | Error error ->
                        match proxy.deleteKindFile ItemKitFile newStem with
                        | Ok () -> ()
                        | Error undoError ->
                            Logger.logError $"KitLibraryContext.recordRelabelledItem: could not delete '{relabelled.label.value}': {undoError.describe}"
                        Error (KitLibraryKitErr error))

    /// What a copy brought into the kit, in write order: each experiment and each added item as written, each lab file
    /// copied in (the experiments' data, then the added items' photos) and each snapshot added (spec 0050 S6, S7.2).
    type ExperimentCopyReport =
        {
            experiments : Experiment list
            items : Item list
            dataFiles : DataAttachment list
            snapshots : LibraryVersionRef list
        }

    /// A copy's item mapping: the items it adds, the held items its rows map onto, and the kept experiments with every
    /// assigned descriptor rewritten onto its target item (spec 0050 S7.2).
    type private ExperimentCopyPlan =
        {
            added : Item list
            mappedOnto : Item list
            mapped : Experiment list
        }

    type KitContext with

        /// Copies the chosen source experiments with the items they use, each mapped by its row or default or added, the
        /// data and photos they reference and the source snapshots this kit lacks, renumbered on a clash. Everything is
        /// read before anything is written, the source only read; the first failure stops the copy (spec 0050 S6, S7.2).
        member this.copyExperimentsFrom
            (source : KitContext)
            (ids : ExperimentId list)
            (itemRows : ItemCopyRow list)
            : Result<ExperimentCopyReport, KitLibraryError> =
            let keep (offered : Experiment list) : Result<Experiment list, KitLibraryError> =
                match ids |> List.tryFind (fun id -> not (offered |> List.exists (fun experiment -> experiment.id = id))) with
                | Some missing -> Error (KitLibraryKitErr (MissingKitFileErr (ExperimentKitFile.fileNameOf missing.fileStem).value))
                | None -> Ok (offered |> List.filter (fun experiment -> List.contains experiment.id ids))

            // An assigned descriptor takes its target item's id and binding; elementId, placement and the varied element
            // stay, and an unassigned descriptor is kept as it is (spec 0050 S7.2).
            let rewrite (targets : Map<ItemId, Item>) (experiment : Experiment) : Experiment =
                {
                    experiment with
                        setup =
                            experiment.setup
                            |> List.map (fun descriptor ->
                                match descriptor.itemIdOpt |> Option.bind (fun id -> Map.tryFind id targets) with
                                | Some target ->
                                    {
                                        descriptor with
                                            itemIdOpt = Some target.id
                                            binding = target.model.assignedBinding
                                    }
                                | None -> descriptor)
                }

            // The rows' refusals and the rewritten experiments' own resolution come before anything is written (spec 0050 S7.2).
            let planItems (kept : Experiment list) : Result<ExperimentCopyPlan, KitLibraryError> =
                source.proxy.readItems ()
                |> Result.mapError KitLibraryKitErr
                |> Result.bind (fun offered ->
                    this.proxy.readItems ()
                    |> Result.mapError KitLibraryKitErr
                    |> Result.bind (fun held ->
                        Experiment.itemsUsedBy kept offered
                        |> Result.bind (fun used ->
                            let rows = ItemCopyRow.copyRowsFor used held itemRows
                            ItemCopyRow.itemsToAdd rows held
                            |> Result.bind (fun added ->
                                let targetOf (row : ItemCopyRow) : Item option =
                                    match row.choice with
                                    | MapOntoKitItem id -> held |> List.tryFind (fun item -> item.id = id)
                                    | AddCopiedItem -> added |> List.tryFind (fun item -> item.id = row.sourceItem.id)
                                let targets =
                                    rows
                                    |> List.choose (fun row -> targetOf row |> Option.map (fun target -> row.sourceItem.id, target))
                                    |> Map.ofList
                                let mappedOnto =
                                    rows
                                    |> List.choose (fun row ->
                                        match row.choice with
                                        | MapOntoKitItem _ -> targetOf row
                                        | AddCopiedItem -> None)
                                    |> List.distinctBy (fun item -> item.id)
                                let mapped = kept |> List.map (rewrite targets)
                                Experiment.itemsUsedBy mapped (held @ added)
                                |> Result.map (fun _ ->
                                    {
                                        added = added
                                        mappedOnto = mappedOnto
                                        mapped = mapped
                                    })))
                        |> Result.mapError KitLibraryItemErr))

            // A held item a row maps onto resolves its whole closure from this kit's snapshots, never the source's
            // (spec 0050 S7.2).
            let resolveMappedOnto (mappedOnto : Item list) (heldSnapshots : LibraryEntryEnvelope list) : Result<unit, KitLibraryError> =
                mappedOnto
                |> List.fold (fun result item ->
                    result |> Result.bind (fun () ->
                        referenceClosure (resolveAmong heldSnapshots) item.libraryReferences
                        |> Result.mapError (fun error ->
                            match error with
                            | UnheldEntryErr reason -> UnheldEntryErr $"{reason}, which the kit's item '{item.label.value}' needs"
                            | _ -> error)
                        |> Result.map ignore)) (Ok ())

            // The source's snapshots answer the unassigned bindings, then the added items' references (spec 0050 S6, S7.2).
            let lackedSnapshots (plan : ExperimentCopyPlan) (heldSnapshots : LibraryEntryEnvelope list) : Result<LibraryEntryEnvelope list, KitLibraryError> =
                source.proxy.listSnapshots ()
                |> Result.mapError KitLibraryKitErr
                |> Result.bind (fun offered ->
                    let unassigned = plan.mapped |> List.collect (fun experiment -> experiment.unassignedReferences)
                    let addedReferences = plan.added |> List.collect (fun item -> item.libraryReferences)
                    referenceClosure (resolveAmong offered) (unassigned @ addedReferences))
                |> Result.map (fun closure ->
                    let heldVersions = heldSnapshots |> List.map (fun envelope -> envelope.libraryVersionRef)
                    closure |> List.filter (fun envelope -> not (List.contains envelope.libraryVersionRef heldVersions)))

            let readLabFiles (paths : KitRelativePath list) : Result<(KitRelativePath * LabFileSource) list, KitLibraryError> =
                paths
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

            // An added item's photo is the copy of its source photo (spec 0050 S7.2).
            let writeItems (moved : Map<KitRelativePath, DataAttachment>) (added : Item list) : Result<Item list, KitLibraryError> =
                added
                |> List.fold (fun result item ->
                    result |> Result.bind (fun written ->
                        let copy =
                            {
                                item with photoOpt = item.photoOpt |> Option.bind (fun path -> Map.tryFind path moved |> Option.map (fun photo -> photo.path))
                            }
                        this.proxy.writeItem copy
                        |> Result.mapError KitLibraryKitErr
                        |> Result.map (fun () -> copy :: written))) (Ok [])
                |> Result.map List.rev

            let writeExperiments (moved : Map<KitRelativePath, DataAttachment>) (renumbered : Experiment list) : Result<Experiment list, KitLibraryError> =
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

            // Snapshots, lab files and items come first, so a written item or experiment never references a file the kit
            // lacks (spec 0050 S6, S7.2).
            let writeAll
                (snapshots : LibraryEntryEnvelope list)
                (files : (KitRelativePath * LabFileSource) list)
                (plan : ExperimentCopyPlan)
                : Result<ExperimentCopyReport, KitLibraryError> =
                writeSnapshots snapshots
                |> Result.bind (fun written ->
                    copyDataFiles files
                    |> Result.bind (fun copied ->
                        let moved = Map.ofList copied
                        writeItems moved plan.added
                        |> Result.bind (fun items ->
                            writeExperiments moved plan.mapped
                            |> Result.map (fun experiments ->
                                let report : ExperimentCopyReport =
                                    {
                                        experiments = experiments
                                        items = items
                                        dataFiles = copied |> List.map snd
                                        snapshots = written
                                    }
                                report))))

            match this.access with
            | ReadOnlyKitAccess -> Error (KitLibraryKitErr (KitReadOnlyErr "copyExperimentsFrom"))
            | ReadWriteKitAccess ->
                source.proxy.readExperiments ()
                |> Result.mapError KitLibraryKitErr
                |> Result.bind keep
                |> Result.bind planItems
                |> Result.bind (fun plan ->
                    this.proxy.listSnapshots ()
                    |> Result.mapError KitLibraryKitErr
                    |> Result.bind (fun heldSnapshots ->
                        resolveMappedOnto plan.mappedOnto heldSnapshots
                        |> Result.bind (fun () -> lackedSnapshots plan heldSnapshots))
                    |> Result.bind (fun snapshots ->
                        let dataPaths = plan.mapped |> List.choose (fun experiment -> experiment.dataFileOpt |> Option.map (fun attachment -> attachment.path))
                        let photoPaths = plan.added |> List.choose (fun item -> item.photoOpt)
                        readLabFiles (dataPaths @ photoPaths)
                        |> Result.bind (fun files ->
                            renumber plan.mapped
                            |> Result.bind (fun renumbered ->
                                writeAll snapshots files
                                    {
                                        plan with mapped = renumbered
                                    }))))
