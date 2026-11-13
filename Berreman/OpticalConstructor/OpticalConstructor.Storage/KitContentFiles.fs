namespace OpticalConstructor.Storage

open System
open Softellect.Sys.Logging
open Softellect.Sys.Primitives
open OpticalConstructor.Domain.Experiments
open OpticalConstructor.Domain.ImportProfiles
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.KitFolders
open OpticalConstructor.Domain.KitLibrary
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.Setups
open OpticalConstructor.Domain.Studies
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

    type StudyFile with

        /// Canonical study JSON with LF line ends and one final newline (spec 0050 S11.1).
        member this.fileText : Result<KitFileText, StorageError> =
            nodeOf this |> Result.map (fun node -> node.canonicalFileText |> KitFileText)

    type KitFileText with

        member private this.toContentJsonText : JsonText = JsonText this.value

        /// Validates the experiment schema and rejects unsupported versions before binding; a `setup` that is not a setup
        /// reference is refused (spec 0050 S4.3, S8.2).
        member this.parseExperimentFile : Result<ExperimentFile, StorageError> =
            parseVersionedText SchemaValidation.validateExperimentFile ExperimentKitFile.extension ExperimentFile.currentSchemaVersion this.toContentJsonText

        /// Validates the setup schema and rejects unsupported versions before binding; no older version is read (spec
        /// 0050 S8.2).
        member this.parseSetupFile : Result<SetupFile, StorageError> =
            parseVersionedText SchemaValidation.validateSetupFile SetupKitFile.extension SetupFile.currentSchemaVersion this.toContentJsonText

        /// Validates the item schema and rejects unsupported versions before binding; no older version is read (spec 0050 S7.1).
        member this.parseItemFile : Result<ItemFile, StorageError> =
            parseVersionedText SchemaValidation.validateItemFile ItemKitFile.extension ItemFile.currentSchemaVersion this.toContentJsonText

        /// Validates the study schema and rejects unsupported versions before binding; no older version is read (spec
        /// 0050 S11.1).
        member this.parseStudyFile : Result<StudyFile, StorageError> =
            parseVersionedText SchemaValidation.validateStudyFile StudyKitFile.extension StudyFile.currentSchemaVersion this.toContentJsonText

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

        /// Replaces the experiment's `<name>.experiment-json` whole, naming any rendering refusal (spec 0050 S4.3, S9.1).
        member this.writeExperiment (experiment : Experiment) : Result<unit, KitError> =
            writeRenderedKindFile this ExperimentKitFile experiment.name.fileStem experiment.experimentFile.fileText

        /// Deletes the kit file of the experiment of that name, or reports it missing (spec 0050 S4.3, S9.1).
        member this.deleteExperiment (name : ExperimentName) : Result<unit, KitError> =
            this.deleteKindFile ExperimentKitFile name.fileStem

        /// Reads every experiment in id order; a file that does not read, or holds an experiment of another name, refuses
        /// the whole read (spec 0050 S4.3, S9.1).
        member this.readExperiments () : Result<Experiment list, KitError> =
            let accept (file : FileName) (stem : FileStem) (_ : Experiment list) (parsed : ExperimentFile) : Result<Experiment, KitError> =
                let experiment = parsed.experiment
                if String.Equals(experiment.name.fileStem.value, stem.value, StringComparison.OrdinalIgnoreCase) then
                    Ok experiment
                else
                    Error (KitFileRejectedErr $"the file '{file.value}' holds experiment '{experiment.name.value}' instead of '{stem.value}'")

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

        /// Replaces the study's `<name>.study-json` whole at its name's safe stem, naming any rendering refusal (spec
        /// 0050 S11.1).
        member this.writeStudy (study : Study) : Result<unit, KitError> =
            writeRenderedKindFile this StudyKitFile study.name.fileStem study.studyFile.fileText

        /// Reads every study in its name's ordinal order. A file that does not read, or holds a study of another name,
        /// refuses the whole read, so a kit never opens with a study left out (spec 0050 S11.1).
        member this.readStudies () : Result<Study list, KitError> =
            let accept (file : FileName) (stem : FileStem) (_ : Study list) (parsed : StudyFile) : Result<Study, KitError> =
                let study = parsed.study
                if String.Equals(study.name.fileStem.value, stem.value, StringComparison.OrdinalIgnoreCase) then
                    Ok study
                else
                    Error (KitFileRejectedErr $"the file '{file.value}' holds study '{study.name.value}' instead of '{stem.value}'")

            readEveryKindFile this StudyKitFile (fun text -> text.parseStudyFile) accept
            |> Result.map (List.sortWith (fun first second -> String.CompareOrdinal(first.name.value, second.name.value)))

    type ItemModelInstance with

        /// The entry a slot holding the item is bound to: the preset, the sample, or none for a standalone model, never
        /// an entry of another piece (spec 0050 S7.2, S8.2).
        member private this.slotBinding : OpticalConstructor.Domain.Placement.ElementValueId option =
            match this with
            | PresetInstance (_, preset) -> Some preset
            | SampleInstance (_, sample) -> Some sample.sampleId.toElementValueId
            | ItemModelVersionInstance _ -> None

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

        /// Writes the experiment once the kit holds the setup revision it references and every item its slots hold,
        /// refusing it and writing nothing otherwise. It snapshots nothing: the setup's entries were snapshotted when it
        /// was recorded (spec 0050 S4.3, S7.2, S8.2).
        member this.recordExperiment (experiment : Experiment) : Result<unit, KitLibraryError> =
            this.kit.proxy.readSetups ()
            |> Result.bind (fun setups -> this.kit.proxy.readItems () |> Result.map (fun items -> setups, items))
            |> Result.mapError KitLibraryKitErr
            |> Result.bind (fun (setups, items) -> Experiment.itemsUsedBy [ experiment ] setups items |> Result.mapError KitLibrarySetupErr)
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

        /// Snapshots and pins the material versions the study opens, each with its closure, before writing its
        /// `<name>.study-json`, so a study never names a version the kit lacks (spec 0050 S11.1; 001 §E).
        /// A failure stops recording and retains snapshots already written.
        member this.recordStudy (study : Study) : Result<unit, KitLibraryError> =
            this.snapshotReferences study.libraryReferences
            |> Result.bind (fun _ -> this.kit.proxy.writeStudy study |> Result.mapError KitLibraryKitErr)

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

    /// What a copy brought into the kit, in write order: each experiment, each added item and each setup as written,
    /// each lab file copied in (the experiments' data, then the added items' photos) and each snapshot added (spec 0050
    /// S6, S7.2, S8.2).
    type ExperimentCopyReport =
        {
            experiments : Experiment list
            items : Item list
            setups : Setup list
            dataFiles : DataAttachment list
            snapshots : LibraryVersionRef list
            /// Each import profile the copy added to the kit file, as added (spec 0050 S9.2).
            profiles : ImportProfile list
            /// The kit file as the copy left it (spec 0050 S9.2).
            kitFile : KitFile
        }

    /// A copy's item mapping: the items it adds, the held items its rows map onto, the kept experiments, the setups
    /// they reference with every slot's item mapped onto its target, and the import profiles it adds (spec 0050 S7.2,
    /// S8.2, S9.2).
    type private ExperimentCopyPlan =
        {
            added : Item list
            mappedOnto : Item list
            experiments : Experiment list
            setups : Setup list
            profiles : ImportProfile list
        }

    type KitContext with

        /// Copies the chosen source experiments with the setups they reference and the items those use, each mapped by its
        /// row or default or added, the data and photos they reference, the kit profiles their data is mapped by and the
        /// source snapshots this kit lacks, renumbered and renamed on a clash; each experiment's exclusions and corrections
        /// are kept as written. Everything is read before anything is written, the source only read; the first failure
        /// stops the copy (spec 0050 S6, S7.2, S8.2, S9.1, S9.2, S9.3).
        member this.copyExperimentsFrom
            (source : KitContext)
            (ids : ExperimentId list)
            (itemRows : ItemCopyRow list)
            : Result<ExperimentCopyReport, KitLibraryError> =
            let keep (offered : Experiment list) : Result<Experiment list, KitLibraryError> =
                match ids |> List.tryFind (fun id -> not (offered |> List.exists (fun experiment -> experiment.id = id))) with
                | Some missing -> Error (KitLibraryKitErr (MissingKitFileErr (ExperimentKitFile.fileNameOf missing.defaultName.fileStem).value))
                | None -> Ok (offered |> List.filter (fun experiment -> List.contains experiment.id ids))

            // The setups the kept experiments reference, once each in first-reference order; a revision the source lacks
            // is refused before anything is written (spec 0050 S8.2).
            let keepSetups (kept : Experiment list) : Result<Setup list, KitLibraryError> =
                source.proxy.readSetups ()
                |> Result.mapError KitLibraryKitErr
                |> Result.bind (fun offered ->
                    kept
                    |> List.fold (fun result experiment ->
                        result |> Result.bind (fun (found : Setup list) ->
                            experiment.slotsIn offered
                            |> Result.mapError KitLibrarySetupErr
                            |> Result.map (fun _ ->
                                if found |> List.exists (fun setup -> setup.name = experiment.setup.name) then found
                                else found @ (offered |> List.filter (fun setup -> setup.name = experiment.setup.name))))) (Ok []))

            // A slot holding an item mapped onto another kit item holds that item, bound as it binds a slot, so the slot's
            // identity and drawn binding agree; an added item keeps its id, so its slot is kept (spec 0050 S7.2, S8.2).
            let mapSlot (targets : Map<ItemId, Item>) (slot : SetupSlot) : SetupSlot =
                match slot.content with
                | ItemInSlot (id, turn) ->
                    match Map.tryFind id targets with
                    | Some target when target.id <> id ->
                        {
                            slot with
                                placement = { slot.placement with valueId = target.model.slotBinding }
                                content = ItemInSlot (target.id, turn)
                        }
                    | Some _
                    | None -> slot
                | EmptySlotContent -> slot

            let mapSetup (targets : Map<ItemId, Item>) (setup : Setup) : Setup =
                {
                    setup with
                        slots = setup.slots |> List.map (mapSlot targets)
                        history =
                            setup.history
                            |> List.map (fun record ->
                                {
                                    record with slots = record.slots |> List.map (mapSlot targets)
                                })
                }

            // The rows' refusals and the mapped setups' own resolution come before anything is written (spec 0050 S7.2,
            // S8.2).
            let planItems (kept : Experiment list) (keptSetups : Setup list) : Result<ExperimentCopyPlan, KitLibraryError> =
                source.proxy.readItems ()
                |> Result.mapError KitLibraryKitErr
                |> Result.bind (fun offered ->
                    this.proxy.readItems ()
                    |> Result.mapError KitLibraryKitErr
                    |> Result.bind (fun held ->
                        Experiment.itemsUsedBy kept keptSetups offered
                        |> Result.mapError KitLibrarySetupErr
                        |> Result.bind (fun used ->
                            let rows = ItemCopyRow.copyRowsFor used held itemRows
                            ItemCopyRow.itemsToAdd rows held
                            |> Result.mapError KitLibraryItemErr
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
                                let mappedSetups = keptSetups |> List.map (mapSetup targets)
                                Experiment.itemsUsedBy kept mappedSetups (held @ added)
                                |> Result.mapError KitLibrarySetupErr
                                |> Result.map (fun _ ->
                                    {
                                        added = added
                                        mappedOnto = mappedOnto
                                        experiments = kept
                                        setups = mappedSetups
                                        profiles = []
                                    })))))

            // A mapped setup the kit holds as it is is not written again; one whose name's file the kit or an earlier copied
            // setup holds takes the next free name, its experiments following it at their revisions (spec 0050 S8.2;
            // .spec-md §3.15).
            let placeSetups (plan : ExperimentCopyPlan) : Result<ExperimentCopyPlan, KitLibraryError> =
                this.proxy.readSetups ()
                |> Result.mapError KitLibraryKitErr
                |> Result.map (fun heldSetups ->
                    let heldNames = heldSetups |> List.map (fun setup -> setup.name)
                    let sameFile (setup : Setup) (held : Setup) : bool =
                        String.Equals(held.name.fileStem.value, setup.name.fileStem.value, StringComparison.OrdinalIgnoreCase)
                    let written, renames =
                        plan.setups
                        |> List.fold (fun (given : Setup list, renames : Map<SetupName, SetupName>) setup ->
                            match heldSetups |> List.tryFind (sameFile setup) with
                            | Some held when held = setup -> given, renames
                            | Some _
                            | None ->
                                let free = setup.name.nextFreeAmong (heldNames @ (given |> List.map (fun placed -> placed.name)))
                                if free = setup.name then
                                    given @ [ setup ], renames
                                else
                                    let renamed =
                                        {
                                            setup with name = free
                                        }
                                    given @ [ renamed ], Map.add setup.name free renames) ([], Map.empty)
                    let following (experiment : Experiment) : Experiment =
                        match Map.tryFind experiment.setup.name renames with
                        | Some name ->
                            {
                                experiment with setup = { experiment.setup with name = name }
                            }
                        | None -> experiment
                    {
                        plan with
                            experiments = plan.experiments |> List.map following
                            setups = written
                    })

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

            // The source's snapshots answer the mapped setups' references, then the added items'; a slot bound by a mapped
            // item's entry the source lacks resolves among this kit's own (spec 0050 S6, S7.2, S8.2).
            let lackedSnapshots
                (mappedSetups : Setup list)
                (added : Item list)
                (heldSnapshots : LibraryEntryEnvelope list)
                : Result<LibraryEntryEnvelope list, KitLibraryError> =
                source.proxy.listSnapshots ()
                |> Result.mapError KitLibraryKitErr
                |> Result.bind (fun offered ->
                    let resolve (reference : EntryReference) : Result<LibraryEntryEnvelope, KitLibraryError> =
                        match resolveAmong offered reference with
                        | Ok envelope -> Ok envelope
                        | Error _ -> resolveAmong heldSnapshots reference
                    let setupReferences = mappedSetups |> List.collect (fun setup -> setup.libraryReferences)
                    let addedReferences = added |> List.collect (fun item -> item.libraryReferences)
                    referenceClosure resolve (setupReferences @ addedReferences))
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

            // A clash of ids takes the next id, and then a clash of names the next free name (spec 0050 S6, S9.1).
            let renumber (kept : Experiment list) : Result<Experiment list, KitLibraryError> =
                this.proxy.readExperiments ()
                |> Result.mapError KitLibraryKitErr
                |> Result.map (fun held ->
                    Experiment.renumberedFor (held |> List.map (fun experiment -> experiment.id)) kept
                    |> Experiment.renamedFor (held |> List.map (fun experiment -> experiment.name)))

            // The source kit's profiles the experiments' data is mapped by, in first-use order: one held under its name with
            // equal content is reused, one with other content is added under the next free name and its experiments follow
            // it, any other is added as it is. One added before counts as held, so no two share a name (spec 0050 S9.2).
            let placeProfiles (plan : ExperimentCopyPlan) : ExperimentCopyPlan =
                let used =
                    plan.experiments
                    |> List.choose (fun experiment -> experiment.dataOpt |> Option.map (fun data -> data.profile))
                    |> List.distinct
                    |> List.choose (fun name ->
                        match ChosenImportProfile.tryOfName source.kitFile.importProfiles name with
                        | Some (KitProfileChoice profile) -> Some profile
                        | Some (BuiltInProfileChoice _)
                        | None -> None)
                let addedProfiles, renames =
                    used
                    |> List.fold (fun (given : ImportProfile list, renames : Map<ImportProfileName, ImportProfileName>) profile ->
                        let held = this.kitFile.importProfiles @ given
                        match held |> List.tryFind (fun heldProfile -> heldProfile.name = profile.name) with
                        | Some heldProfile when heldProfile = profile -> given, renames
                        | Some _ ->
                            let free = profile.name.nextFreeAmong (held |> List.map (fun heldProfile -> heldProfile.name))
                            let renamed =
                                {
                                    profile with name = free
                                }
                            given @ [ renamed ], Map.add profile.name free renames
                        | None -> given @ [ profile ], renames) ([], Map.empty)
                let following (experiment : Experiment) : Experiment =
                    {
                        experiment with
                            dataOpt =
                                experiment.dataOpt
                                |> Option.map (fun data ->
                                    match Map.tryFind data.profile renames with
                                    | Some name ->
                                        {
                                            data with profile = name
                                        }
                                    | None -> data)
                    }
                {
                    plan with
                        experiments = plan.experiments |> List.map following
                        profiles = addedProfiles
                }

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

            let writeSetups (setups : Setup list) : Result<Setup list, KitLibraryError> =
                setups
                |> List.fold (fun result setup ->
                    result |> Result.bind (fun written ->
                        this.proxy.writeSetup setup
                        |> Result.mapError KitLibraryKitErr
                        |> Result.map (fun () -> setup :: written))) (Ok [])
                |> Result.map List.rev

            let writeExperiments (moved : Map<KitRelativePath, DataAttachment>) (renumbered : Experiment list) : Result<Experiment list, KitLibraryError> =
                renumbered
                |> List.fold (fun result experiment ->
                    result |> Result.bind (fun written ->
                        let copy =
                            {
                                experiment with
                                    dataOpt =
                                        experiment.dataOpt
                                        |> Option.bind (fun data ->
                                            Map.tryFind data.attachment.path moved
                                            |> Option.map (fun attachment ->
                                                {
                                                    data with attachment = attachment
                                                }))
                            }
                        this.proxy.writeExperiment copy
                        |> Result.mapError KitLibraryKitErr
                        |> Result.map (fun () -> copy :: written))) (Ok [])
                |> Result.map List.rev

            // The kit file as saved with the added profiles, else as it is, unwritten (spec 0050 S9.2).
            let saveProfiles (profiles : ImportProfile list) : Result<KitFile, KitLibraryError> =
                match profiles with
                | [] -> Ok this.kitFile
                | _ ->
                    this.saveKitFile
                        {
                            this.kitFile with importProfiles = this.kitFile.importProfiles @ profiles
                        }
                    |> Result.mapError KitLibraryKitErr
                    |> Result.map (fun saved -> saved.kitFile)

            // Snapshots, lab files, the added profiles, items and setups come first, so a written setup or experiment never
            // references a file or profile the kit lacks (spec 0050 S6, S7.2, S8.2, S9.2).
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
                        saveProfiles plan.profiles
                        |> Result.bind (fun kitFile ->
                            writeItems moved plan.added
                            |> Result.bind (fun items ->
                                writeSetups plan.setups
                                |> Result.bind (fun setups ->
                                    writeExperiments moved plan.experiments
                                    |> Result.map (fun experiments ->
                                        let report : ExperimentCopyReport =
                                            {
                                                experiments = experiments
                                                items = items
                                                setups = setups
                                                dataFiles = copied |> List.map snd
                                                snapshots = written
                                                profiles = plan.profiles
                                                kitFile = kitFile
                                            }
                                        report))))))

            match this.access with
            | ReadOnlyKitAccess -> Error (KitLibraryKitErr (KitReadOnlyErr "copyExperimentsFrom"))
            | ReadWriteKitAccess ->
                source.proxy.readExperiments ()
                |> Result.mapError KitLibraryKitErr
                |> Result.bind keep
                |> Result.bind (fun kept -> keepSetups kept |> Result.bind (planItems kept))
                |> Result.bind (fun plan ->
                    this.proxy.listSnapshots ()
                    |> Result.mapError KitLibraryKitErr
                    |> Result.bind (fun heldSnapshots ->
                        resolveMappedOnto plan.mappedOnto heldSnapshots
                        |> Result.bind (fun () -> lackedSnapshots plan.setups plan.added heldSnapshots))
                    |> Result.bind (fun snapshots ->
                        let dataPaths = plan.experiments |> List.choose (fun experiment -> experiment.dataOpt |> Option.map (fun data -> data.attachment.path))
                        let photoPaths = plan.added |> List.choose (fun item -> item.photoOpt)
                        readLabFiles (dataPaths @ photoPaths)
                        |> Result.bind (fun files ->
                            placeSetups plan
                            |> Result.bind (fun placed ->
                                renumber placed.experiments
                                |> Result.bind (fun renumbered ->
                                    let numbered =
                                        {
                                            placed with experiments = renumbered
                                        }
                                    writeAll snapshots files (placeProfiles numbered))))))
