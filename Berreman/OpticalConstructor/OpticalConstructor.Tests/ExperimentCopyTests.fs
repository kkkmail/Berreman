namespace OpticalConstructor.Tests

open System
open System.Text
open Xunit
open Softellect.Sys.Primitives
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Placement
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.Experiments
open OpticalConstructor.Domain.ItemModels
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.CatalogueImport
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.KitFolders
open OpticalConstructor.Domain.KitLibrary
open OpticalConstructor.Storage.CatalogueImportStore
open OpticalConstructor.Storage.KitContentFiles
open OpticalConstructor.Storage.KitFolderStore
open OpticalConstructor.Domain.Setups

/// Experiments copied from another kit with the setups they reference and the items those use, the data files and
/// photos they reference and the snapshots they need, between in-memory kits: the source opened read-only, the target
/// read-write unless a fact says otherwise (spec 0050 S6, S7.2, S8.2).
module ExperimentCopyTests =

    /// One in-memory kit: its folder, as an outside writer sees it, and the context it is opened through.
    type private CopyKit =
        {
            folder : InMemoryKitFolder
            context : KitContext
        }

    /// Everything a kit holds as read: its paths, each path read as a data file (a folder or a snapshot answers its
    /// refusal), its snapshots, its experiments and its items.
    type private HeldContent =
        {
            paths : KitRelativePath list
            files : Result<KitDataFile, KitError> list
            snapshots : Result<LibraryEntryEnvelope list, KitError>
            experiments : Result<Experiment list, KitError>
            items : Result<Item list, KitError>
        }

    /// What a source kit records: its items first, then its setups, then the experiments over them.
    type private SourceRecords =
        {
            items : Item list
            setups : Setup list
            experiments : Experiment list
        }

    let private expectKitOk (result : Result<'Value, KitError>) : 'Value =
        result |> Result.defaultWith (fun error -> failwith error.describe)

    let private expectCopyOk (result : Result<'Value, KitLibraryError>) : 'Value =
        result |> Result.defaultWith (fun error -> failwith error.describe)

    /// Starts at a fixed UTC instant and advances one second per call.
    let private steppingClock () : ClockProvider =
        let mutable next = ClockTime (DateTime(2026, 11, 9, 12, 0, 0, DateTimeKind.Utc))
        {
            now =
                fun () ->
                    let current = next
                    next <- ClockTime (current.value.AddSeconds 1.0)
                    current
        }

    let private runPath = KitRelativePath "data/raw/run 1.csv"

    let private otherPath = KitRelativePath "data/b.csv"

    let private runText = KitFileText "wavelength,intensity\n600,0.5\n"

    let private otherText = KitFileText "phi,I\n0,0.25\n"

    let private hashOf (text : KitFileText) : ContentHash =
        Encoding.UTF8.GetBytes text.value |> FileByteContent |> ContentHash.ofBytes

    let private sourceKitId = KitId (Guid.Parse "22400000-0000-4000-8000-000000000001")

    let private targetKitId = KitId (Guid.Parse "22400000-0000-4000-8000-000000000002")

    /// A fresh in-memory folder initialised as a kit of this name.
    let private newKitFolder (id : KitId) (name : KitName) (clock : ClockProvider) : InMemoryKitFolder =
        let folder = InMemoryKitFolder.create ()
        let author = KitAuthor "Experiment copy tests"
        let kitFile = KitFile.create id name author (clock.now ())
        (KitProxy.createInMemory folder).initialiseKit kitFile |> expectKitOk
        folder

    let private openCopyKit (folder : InMemoryKitFolder) (access : KitAccess) (clock : ClockProvider) : CopyKit =
        {
            folder = folder
            context = KitContext.openKit (KitProxy.createInMemory folder) access clock |> expectKitOk
        }

    let private emptyTarget (access : KitAccess) (clock : ClockProvider) : CopyKit =
        let folder = newKitFolder targetKitId (KitName "Copy target") clock
        openCopyKit folder access clock

    /// The kit closed and opened again over its folder, as a later session reads it.
    let private reopened (kit : CopyKit) (clock : ClockProvider) : CopyKit =
        kit.context.close () |> expectKitOk
        openCopyKit kit.folder ReadOnlyKitAccess clock

    let private heldBy (kit : CopyKit) : HeldContent =
        let paths = kit.folder.paths ()
        {
            paths = paths
            files = paths |> List.map kit.context.proxy.readDataFile
            snapshots = kit.context.proxy.listSnapshots ()
            experiments = kit.context.proxy.readExperiments ()
            items = kit.context.proxy.readItems ()
        }

    /// Every `*.item-json` the kit holds, read as stored.
    let private itemFilesOf (kit : CopyKit) : Result<KitDataFile, KitError> list =
        kit.folder.paths ()
        |> List.filter (fun path -> path.value.EndsWith(ItemKitFile.extension.value, StringComparison.Ordinal))
        |> List.map kit.context.proxy.readDataFile

    let private catalogueEnvelope (id : LibraryEntryId) : LibraryEntryEnvelope =
        (BuiltinCatalogue.catalogueData ()).envelopes
        |> List.find (fun envelope -> envelope.content.id = id)

    let private itemModelEnvelope (id : ItemModelId) : LibraryEntryEnvelope =
        catalogueEnvelope id.toLibraryEntryId

    let private sourcePresetId : LibraryEntryId =
        PresetIds.source600.tryToLibraryEntryId
        |> Option.defaultWith (fun () -> failwith "The 600 nm source must have an entry id.")

    /// The Built-in catalogue's envelope answering the reference: the exact version, or the entry's highest one.
    let private catalogueResolve (reference : EntryReference) : Result<LibraryEntryEnvelope, KitLibraryError> =
        (BuiltinCatalogue.catalogueData ()).envelopes
        |> List.filter (fun envelope ->
            match reference with
            | EntryVersionReference version -> envelope.libraryVersionRef = version
            | EntryLatestReference id -> envelope.content.id = id)
        |> List.sortByDescending (fun envelope -> envelope.content.version)
        |> List.tryHead
        |> function
            | Some envelope -> Ok envelope
            | None -> Error (UnheldEntryErr reference.describe)

    /// The Built-in catalogue's closure of an item's model and bound physics, in visit order.
    let private closureOf (item : Item) : LibraryEntryEnvelope list =
        referenceClosure catalogueResolve item.libraryReferences |> expectCopyOk

    /// Writes each item through `writeItem` after the catalogue's snapshots of its closure the kit lacks, so the kit
    /// holds it with fully valid model snapshots.
    let private holdItems (kit : CopyKit) (items : Item list) : unit =
        for item in items do
            let held = kit.context.proxy.listSnapshots () |> expectKitOk |> List.map (fun envelope -> envelope.libraryVersionRef)
            for envelope in closureOf item do
                if not (List.contains envelope.libraryVersionRef held) then
                    kit.context.proxy.writeSnapshot envelope |> expectKitOk
            kit.context.proxy.writeItem item |> expectKitOk

    /// An item made through AddItem's draft, every parameter known at its model's default.
    let private itemOf (id : string) (label : string) (model : ItemModelChoice) (dial : ItemDial) : Item =
        let draft : ItemDraft =
            {
                id = ItemId (Guid.Parse id)
                label = ItemLabel label
                model = model
                dial = dial
            }
        draft.toItem

    /// The item with its parameter of this name known at the value; the item must have that parameter.
    let private withParameter (name : string) (value : double) (item : Item) : Item =
        let parameterName = ParameterName name
        match item.tryFindParameter parameterName with
        | Some _ -> ()
        | None -> Assert.Fail($"'{item.label.value}' must have the parameter '{name}'")
        {
            item with
                parameters =
                    item.parameters
                    |> List.map (fun parameter ->
                        if parameter.name = parameterName then
                            {
                                parameter with state = KnownParameter (ParameterValue value)
                            }
                        else
                            parameter)
        }

    /// The 600 nm source preset over the shipped source-preset item model (spec 0050 S7.1, step 210).
    let private laserChoice : ItemModelChoice =
        SourcePresetChoice (BuiltinCatalogue.itemModel ItemModelIds.sourcePresetItemModelId, BuiltinCatalogue.source PresetIds.source600)

    /// The sample's first version over the shipped stack-sample item model (spec 0050 S7.1, step 210).
    let private sampleChoice (sample : SampleId) : ItemModelChoice =
        StackSampleChoice (
            BuiltinCatalogue.itemModel ItemModelIds.stackSampleItemModelId,
            SampleVersionId.firstOf sample,
            BuiltinCatalogue.sample sample
        )

    /// The shipped circular-polarizer item model, which stands alone and binds no preset or sample.
    let private circularChoice : ItemModelChoice =
        LibraryModelChoice (BuiltinCatalogue.itemModel ItemModelIds.circularPolarizerModelId)

    let private laser600 : Item = itemOf "22700000-0000-4000-8000-000000000001" "Laser600" laserChoice FixedMounting

    let private ml : Item = itemOf "22700000-0000-4000-8000-000000000002" "ML" (sampleChoice SampleIds.multilayerQwId) (DialMounted DialPositive)

    let private qz : Item = itemOf "22700000-0000-4000-8000-000000000003" "QZ" (sampleChoice SampleIds.glassPlate1mmId) (DialMounted DialPositive)

    let private cpl1 : Item = itemOf "22700000-0000-4000-8000-000000000004" "CPL1" circularChoice (DialMounted DialPositive)

    /// Laser600 and ML (used by E1), QZ (used by E2) and CPL1 (used by none).
    let private standardItems : Item list = [ laser600; ml; qz; cpl1 ]

    /// A rest-pose slot of that kind, bound to `valueIdOpt`, holding nothing.
    let private slot (id : ElementId) (kind : CatalogueKind) (valueIdOpt : ElementValueId option) : SetupSlot =
        {
            id = id
            placement =
                {
                    ElementPlacement.create kind TablePoint.origin with valueId = valueIdOpt
                }
            zoom = BeamLayout.ElementZoom 1.0
            beam = BeamLayout.OnMainBeam
            content = EmptySlotContent
        }

    /// The slot holding the item (spec 0050 S8.2).
    let private holding (item : Item) (empty : SetupSlot) : SetupSlot =
        {
            empty with content = ItemInSlot (item.id, PlacementTurn)
        }

    let private detector : SetupSlot = slot (ElementId "copied-detector") Detector None

    let private setupOf (name : string) (slots : SetupSlot list) : Setup =
        {
            name = SetupName name
            revision = SetupRevision.first
            slots = slots
            history = []
        }

    let private glassPlateValueId : ElementValueId = SampleIds.glassPlate1mmId.toElementValueId

    let private sampleSlot : SetupSlot = slot (ElementId "copied-sample") CatalogueKind.Sample (Some SampleIds.multilayerQwId.toElementValueId)

    /// E1's `Bench` binds the 600 nm source on Laser600 and the multilayer sample on ML.
    let private benchSetup : Setup =
        setupOf "Bench" [ slot (ElementId "copied-source") LightSource (Some PresetIds.source600) |> holding laser600; sampleSlot |> holding ml; detector ]

    /// E2's `Plate` binds the 1 mm glass plate on QZ.
    let private plateSetup : Setup = setupOf "Plate" [ slot (ElementId "copied-plate") CatalogueKind.Sample (Some glassPlateValueId) |> holding qz; detector ]

    /// E3's `Dark` binds nothing.
    let private darkSetup : Setup = setupOf "Dark" [ detector ]

    let private standardSetups : Setup list = [ benchSetup; plateSetup; darkSetup ]

    let private darkLine (id : ExperimentId) (dataFileOpt : DataAttachment option) : Experiment =
        {
            id = id
            name = id.defaultName
            setup = darkSetup.reference
            scan = ExperimentScan.darkLine CaptureTransmitted
            role = ScienceRun
            sessionOpt = None
            notes = ExperimentNotes.none
            dataFileOpt = dataFileOpt
        }

    /// E1 runs over `Bench` and attaches the run.
    let private experimentE1 (run : DataAttachment) : Experiment =
        let attached = darkLine (ExperimentId 1) (Some run)
        {
            attached with setup = benchSetup.reference
        }

    /// E2 runs over `Plate` and attaches nothing.
    let private experimentE2 : Experiment =
        let unattached = darkLine (ExperimentId 2) None
        {
            unattached with setup = plateSetup.reference
        }

    /// E1, E2, and E3, which binds nothing and attaches the other file.
    let private standardExperiments (run : DataAttachment) (other : DataAttachment) : Experiment list =
        [ experimentE1 run; experimentE2; darkLine (ExperimentId 3) (Some other) ]

    /// The library a kit records through: the Built-in catalogue, so its snapshots are the library's.
    let private libraryOver (kit : KitContext) : KitLibraryContext =
        {
            kit = kit
            imports = CatalogueImportContext.create (BuiltinCatalogue.freshStores ()).proxy
        }

    /// The source: its lab files placed by an outside writer and attached in place, its items and then its experiments
    /// recorded through the library, then closed and opened read-only. Answers it with the experiments it recorded.
    let private sourceRecording
        (recordsOf : (KitRelativePath -> KitFileText -> DataAttachment) -> SourceRecords)
        (clock : ClockProvider)
        : CopyKit * Experiment list =
        let folder = newKitFolder sourceKitId (KitName "Copy source") clock
        let writer = openCopyKit folder ReadWriteKitAccess clock
        let attach (path : KitRelativePath) (text : KitFileText) : DataAttachment =
            folder.placeLabFile path text |> expectKitOk
            writer.context.proxy.readLabFile path
            |> Result.bind (writer.context.proxy.attachLabFile KitDataFolder.root)
            |> expectKitOk
        let records = recordsOf attach
        let library = libraryOver writer.context
        for item in records.items do
            library.recordItem item |> expectCopyOk
        for setup in records.setups do
            library.recordSetup setup |> expectCopyOk
        for experiment in records.experiments do
            library.recordExperiment experiment |> expectCopyOk
        writer.context.close () |> expectKitOk
        openCopyKit folder ReadOnlyKitAccess clock, records.experiments

    /// The source holding the standard items and setups and the experiments made over its run and other files.
    let private sourceHolding
        (experimentsOf : DataAttachment -> DataAttachment -> Experiment list)
        (clock : ClockProvider)
        : CopyKit * Experiment list =
        sourceRecording
            (fun attach ->
                {
                    items = standardItems
                    setups = standardSetups
                    experiments = experimentsOf (attach runPath runText) (attach otherPath otherText)
                })
            clock

    let private recordedAs (id : ExperimentId) (recorded : Experiment list) : Experiment =
        recorded |> List.find (fun experiment -> experiment.id = id)

    /// E1's closure in visit order: Laser600's model and its source preset, then ML's model and its multilayer sample
    /// with the sample's materials and their categories.
    let private closureOfE1 () : LibraryEntryEnvelope list =
        [
            itemModelEnvelope ItemModelIds.sourcePresetItemModelId
            catalogueEnvelope sourcePresetId
            itemModelEnvelope ItemModelIds.stackSampleItemModelId
            catalogueEnvelope SampleIds.multilayerQwId.toLibraryEntryId
            catalogueEnvelope MaterialIds.vacuumMaterial.toLibraryEntryId
            catalogueEnvelope CategoryIds.vacuumCategory.toLibraryEntryId
            catalogueEnvelope MaterialIds.glass152.toLibraryEntryId
            catalogueEnvelope CategoryIds.glass.toLibraryEntryId
        ]

    /// E1's closure as a copy walks it: `Bench`'s bindings, then the added Laser600's and ML's model and bound physics,
    /// each version once (spec 0050 S8.2).
    let private copiedClosureOfE1 () : LibraryEntryEnvelope list =
        referenceClosure catalogueResolve (benchSetup.libraryReferences @ laser600.libraryReferences @ ml.libraryReferences)
        |> expectCopyOk

    /// Each copied experiment's setup revision resolves among the kit's setups (spec 0050 S8.2).
    let private assertSetupsHeld (kit : CopyKit) (experiments : Experiment list) : unit =
        let setups = kit.context.proxy.readSetups () |> expectKitOk
        for experiment in experiments do
            match experiment.slotsIn setups with
            | Ok _ -> ()
            | Error error -> Assert.Fail($"{experiment.name.value}'s setup is not in the kit: {error.describe}")

    let private ordinal (paths : KitRelativePath list) : KitRelativePath list =
        paths |> List.sortWith (fun first second -> String.CompareOrdinal(first.value, second.value))

    let private bySnapshotName (envelopes : LibraryEntryEnvelope list) : LibraryEntryEnvelope list =
        envelopes |> List.sortWith (fun first second -> String.CompareOrdinal(first.snapshotFileName.value, second.snapshotFileName.value))

    /// Two source pieces of one model, a target piece of that model, the slot the source's setup holds piece B in, and
    /// the binding that slot must take from piece T.
    type private SameModelPieces =
        {
            pieceA : Item
            pieceB : Item
            pieceT : Item
            element : SetupSlot
            bindingOfT : ElementValueId option
        }

    /// The setup whose slots held the source item, in every revision, hold the target item, their bindings unchanged.
    let private reassigned (from : Item) (onto : Item) (setup : Setup) : Setup =
        let onTarget (held : SetupSlot) : SetupSlot = if held.content.itemOpt = Some from.id then holding onto held else held
        {
            setup with
                slots = setup.slots |> List.map onTarget
                history =
                    setup.history
                    |> List.map (fun record ->
                        {
                            record with slots = record.slots |> List.map onTarget
                        })
        }

    [<Fact>]
    let ``a copy brings exactly the chosen experiments, the data files they reference and the snapshots they need`` () : unit =
        let clock = steppingClock ()
        let source, recorded = sourceHolding standardExperiments clock
        let target = emptyTarget ReadWriteKitAccess clock
        let sourceBefore = heldBy source
        let run : DataAttachment =
            {
                path = runPath
                contentHash = hashOf runText
            }
        let e1 = recordedAs (ExperimentId 1) recorded
        Assert.Equal<DataAttachment option>(Some run, e1.dataFileOpt)
        let closure = closureOfE1 ()
        Assert.Equal<LibraryEntryEnvelope list>(bySnapshotName closure, bySnapshotName (copiedClosureOfE1 ()))

        let report = target.context.copyExperimentsFrom source.context [ ExperimentId 1 ] [] |> expectCopyOk

        let expected : ExperimentCopyReport =
            {
                experiments = [ e1 ]
                items = [ laser600; ml ]
                setups = [ benchSetup ]
                dataFiles = [ run ]
                snapshots = copiedClosureOfE1 () |> List.map (fun envelope -> envelope.libraryVersionRef)
            }
        Assert.Equal<ExperimentCopyReport>(expected, report)
        let kitPaths =
            [ ".kit-json"; ".oc"; ".oc/lock"; "Bench.setup-json"; "E1.experiment-json"; "Laser600.item-json"; "ML.item-json"; "data"; "data/raw"; "data/raw/run 1.csv"; "library" ]
            |> List.map KitRelativePath
        let snapshotPaths = closure |> List.map (fun envelope -> envelope.snapshotPath)
        Assert.Equal<KitRelativePath list>(ordinal (kitPaths @ snapshotPaths), target.folder.paths ())
        Assert.Equal<Result<Experiment list, KitError>>(Ok [ e1 ], target.context.proxy.readExperiments ())
        let copiedFile : KitDataFile =
            {
                path = runPath
                text = runText
                contentHash = hashOf runText
            }
        Assert.Equal<Result<KitDataFile, KitError>>(Ok copiedFile, target.context.proxy.readDataFile runPath)
        Assert.Equal<Result<LibraryEntryEnvelope list, KitError>>(Ok (bySnapshotName closure), target.context.proxy.listSnapshots ())
        Assert.Equal<HeldContent>(sourceBefore, heldBy source)
        assertSetupsHeld target [ e1 ]

    [<Fact>]
    let ``a copied experiment whose id the kit holds takes the next free id`` () : unit =
        let clock = steppingClock ()
        let source, recorded = sourceHolding standardExperiments clock
        let target = emptyTarget ReadWriteKitAccess clock
        let reflected = darkLine (ExperimentId 1) None
        let heldE1 =
            {
                reflected with scan = ExperimentScan.darkLine CaptureReflected
            }
        let both = darkLine (ExperimentId 2) None
        let heldE2 =
            {
                both with scan = ExperimentScan.darkLine CaptureBoth
            }
        for experiment in [ heldE1; heldE2 ] do
            target.context.proxy.writeExperiment experiment |> expectKitOk
        let copiedE3 = recordedAs (ExperimentId 3) recorded
        let sourceE2 = recordedAs (ExperimentId 2) recorded
        // Spec 0050 S9.1: the kit holds `E2` too, so the copy also takes the next free name.
        let copiedE4 =
            {
                sourceE2 with
                    id = ExperimentId 4
                    name = ExperimentName "E2 (2)"
            }

        let report = target.context.copyExperimentsFrom source.context [ ExperimentId 2; ExperimentId 3 ] [] |> expectCopyOk

        Assert.Equal<Experiment list>([ copiedE4; copiedE3 ], report.experiments)
        Assert.Equal<Result<Experiment list, KitError>>(Ok [ heldE1; heldE2; copiedE3; copiedE4 ], target.context.proxy.readExperiments ())
        assertSetupsHeld target report.experiments

    [<Fact>]
    let ``a copied experiment whose name the target holds takes the next free name`` () : unit =
        let clock = steppingClock ()
        let source, recorded = sourceHolding standardExperiments clock
        let target = emptyTarget ReadWriteKitAccess clock
        let unnamed = darkLine (ExperimentId 7) None
        let heldNamedE1 =
            {
                unnamed with name = (ExperimentId 1).defaultName
            }
        target.context.proxy.writeExperiment heldNamedE1 |> expectKitOk
        let e1 = recordedAs (ExperimentId 1) recorded
        let copiedE1 =
            {
                e1 with name = ExperimentName "E1 (2)"
            }

        let report = target.context.copyExperimentsFrom source.context [ ExperimentId 1 ] [] |> expectCopyOk

        Assert.Equal<Experiment list>([ copiedE1 ], report.experiments)
        let paths = target.folder.paths ()
        Assert.Contains(KitRelativePath "E1 (2).experiment-json", paths)
        Assert.Contains(KitRelativePath "E1.experiment-json", paths)
        Assert.Equal<Result<Experiment list, KitError>>(Ok [ copiedE1; heldNamedE1 ], target.context.proxy.readExperiments ())
        assertSetupsHeld target report.experiments

    [<Fact>]
    let ``a copied data file whose name the kit holds takes the next free suffix, and its hash is re-recorded`` () : unit =
        let clock = steppingClock ()
        let source, recorded = sourceHolding standardExperiments clock
        let target = emptyTarget ReadWriteKitAccess clock
        let heldText = KitFileText "wavelength,intensity\n600,0.9\n"
        target.folder.placeLabFile runPath heldText |> expectKitOk
        let suffixedPath = KitRelativePath "data/raw/run 1 (2).csv"
        let suffixed : DataAttachment =
            {
                path = suffixedPath
                contentHash = hashOf runText
            }
        let e1 = recordedAs (ExperimentId 1) recorded

        let report = target.context.copyExperimentsFrom source.context [ ExperimentId 1 ] [] |> expectCopyOk

        Assert.Equal<DataAttachment list>([ suffixed ], report.dataFiles)
        let copiedE1 =
            {
                e1 with dataFileOpt = Some suffixed
            }
        Assert.Equal<Result<Experiment list, KitError>>(Ok [ copiedE1 ], target.context.proxy.readExperiments ())
        let heldFile : KitDataFile =
            {
                path = runPath
                text = heldText
                contentHash = hashOf heldText
            }
        let copiedFile : KitDataFile =
            {
                path = suffixedPath
                text = runText
                contentHash = hashOf runText
            }
        Assert.Equal<Result<KitDataFile, KitError>>(Ok heldFile, target.context.proxy.readDataFile runPath)
        Assert.Equal<Result<KitDataFile, KitError>>(Ok copiedFile, target.context.proxy.readDataFile suffixedPath)
        assertSetupsHeld target [ copiedE1 ]

        // An outside writer rewrites the source's file after its attach: the copy records the bytes it copied.
        let rewrittenText = KitFileText "wavelength,intensity\n600,0.75\n"
        source.folder.placeLabFile runPath rewrittenText |> expectKitOk
        let fresh = emptyTarget ReadWriteKitAccess clock
        let rewritten : DataAttachment =
            {
                path = runPath
                contentHash = hashOf rewrittenText
            }
        Assert.NotEqual<DataAttachment option>(Some rewritten, e1.dataFileOpt)

        let rewrittenReport = fresh.context.copyExperimentsFrom source.context [ ExperimentId 1 ] [] |> expectCopyOk

        Assert.Equal<DataAttachment list>([ rewritten ], rewrittenReport.dataFiles)
        let rewrittenE1 =
            {
                e1 with dataFileOpt = Some rewritten
            }
        Assert.Equal<Result<Experiment list, KitError>>(Ok [ rewrittenE1 ], fresh.context.proxy.readExperiments ())
        assertSetupsHeld fresh [ rewrittenE1 ]

    [<Fact>]
    let ``a data file two copied experiments share is copied once`` () : unit =
        let clock = steppingClock ()
        let withShared (run : DataAttachment) (other : DataAttachment) : Experiment list =
            standardExperiments run other @ [ darkLine (ExperimentId 5) (Some run) ]
        let source, recorded = sourceHolding withShared clock
        let target = emptyTarget ReadWriteKitAccess clock
        let run : DataAttachment =
            {
                path = runPath
                contentHash = hashOf runText
            }
        let e1 = recordedAs (ExperimentId 1) recorded
        let e5 = recordedAs (ExperimentId 5) recorded
        Assert.Equal<DataAttachment option list>([ Some run; Some run ], [ e1.dataFileOpt; e5.dataFileOpt ])

        let report = target.context.copyExperimentsFrom source.context [ ExperimentId 1; ExperimentId 5 ] [] |> expectCopyOk

        Assert.Equal<DataAttachment list>([ run ], report.dataFiles)
        Assert.Equal<Result<Experiment list, KitError>>(Ok [ e1; e5 ], target.context.proxy.readExperiments ())
        let dataPaths =
            target.folder.paths ()
            |> List.filter (fun path -> path.value.StartsWith("data/", StringComparison.Ordinal))
        Assert.Equal<KitRelativePath list>([ KitRelativePath "data/raw"; runPath ], dataPaths)
        assertSetupsHeld target [ e1; e5 ]

    [<Fact>]
    let ``a snapshot the kit already holds is kept and not written again`` () : unit =
        let clock = steppingClock ()
        let source, _ = sourceHolding standardExperiments clock
        let target = emptyTarget ReadWriteKitAccess clock
        let held = catalogueEnvelope MaterialIds.glass152.toLibraryEntryId
        let closure = closureOfE1 ()
        Assert.Contains(held, closure)
        target.context.proxy.writeSnapshot held |> expectKitOk

        let report = target.context.copyExperimentsFrom source.context [ ExperimentId 1 ] [] |> expectCopyOk

        let added =
            copiedClosureOfE1 () |> List.filter (fun envelope -> envelope <> held) |> List.map (fun envelope -> envelope.libraryVersionRef)
        Assert.Equal<LibraryVersionRef list>(added, report.snapshots)
        let listed = target.context.proxy.listSnapshots () |> expectKitOk
        Assert.Equal<LibraryEntryEnvelope list>(bySnapshotName closure, listed)
        Assert.Single(listed |> List.filter (fun envelope -> envelope.libraryVersionRef = held.libraryVersionRef)) |> ignore
        assertSetupsHeld target report.experiments

    [<Fact>]
    let ``a copy whose data file the source lacks writes nothing`` () : unit =
        let clock = steppingClock ()
        let source, _ = sourceHolding standardExperiments clock
        let target = emptyTarget ReadWriteKitAccess clock
        source.folder.removeLabFile runPath |> expectKitOk
        let before = heldBy target

        match target.context.copyExperimentsFrom source.context [ ExperimentId 1 ] [] with
        | Error (KitLibraryKitErr (MissingKitFileErr reason)) -> Assert.Contains(runPath.value, reason)
        | Error error -> Assert.Fail($"Expected MissingKitFileErr, got {error.describe}")
        | Ok _ -> Assert.Fail("A copy whose data file the source lacks must be refused.")

        Assert.Equal<HeldContent>(before, heldBy target)

    [<Fact>]
    let ``a kit opened read-only copies nothing`` () : unit =
        let clock = steppingClock ()
        let source, _ = sourceHolding standardExperiments clock
        let target = emptyTarget ReadOnlyKitAccess clock
        let sourceReads = ResizeArray<string>()
        let proxy = source.context.proxy
        let watchedProxy =
            {
                proxy with
                    listKindFiles =
                        fun kind ->
                            sourceReads.Add "listKindFiles"
                            proxy.listKindFiles kind
                    readKindFile =
                        fun kind stem ->
                            sourceReads.Add "readKindFile"
                            proxy.readKindFile kind stem
                    readLabFile =
                        fun path ->
                            sourceReads.Add "readLabFile"
                            proxy.readLabFile path
                    listSnapshots =
                        fun () ->
                            sourceReads.Add "listSnapshots"
                            proxy.listSnapshots ()
            }
        let watched =
            {
                source.context with proxy = watchedProxy
            }
        let before = heldBy target

        let copied = target.context.copyExperimentsFrom watched [ ExperimentId 1 ] []

        Assert.Equal<Result<ExperimentCopyReport, KitLibraryError>>(Error (KitLibraryKitErr (KitReadOnlyErr "copyExperimentsFrom")), copied)
        Assert.Empty(sourceReads)
        Assert.Equal<HeldContent>(before, heldBy target)

    [<Fact>]
    let ``an experiment the source does not hold is refused and nothing is written`` () : unit =
        let clock = steppingClock ()
        let source, _ = sourceHolding standardExperiments clock
        let target = emptyTarget ReadWriteKitAccess clock
        let before = heldBy target

        for ids in [ [ ExperimentId 9 ]; [ ExperimentId 1; ExperimentId 9 ] ] do
            match target.context.copyExperimentsFrom source.context ids [] with
            | Error (KitLibraryKitErr (MissingKitFileErr reason)) -> Assert.Equal("E9.experiment-json", reason)
            | Error error -> Assert.Fail($"Expected MissingKitFileErr, got {error.describe}")
            | Ok _ -> Assert.Fail("An experiment the source does not hold must be refused.")

            Assert.Equal<HeldContent>(before, heldBy target)

    [<Fact>]
    let ``a copy adds the items its experiments use, with their model snapshots`` () : unit =
        let clock = steppingClock ()
        let source, recorded = sourceHolding standardExperiments clock
        let target = emptyTarget ReadWriteKitAccess clock
        let e1 = recordedAs (ExperimentId 1) recorded
        Assert.Equal<Result<Item list, KitError>>(Ok [ cpl1; laser600; ml; qz ], source.context.proxy.readItems ())

        let report = target.context.copyExperimentsFrom source.context [ ExperimentId 1 ] [] |> expectCopyOk

        Assert.Equal<Item list>([ laser600; ml ], report.items)
        let items = target.context.proxy.readItems () |> expectKitOk
        Assert.Equal<Item list>([ laser600; ml ], items)
        let paths = target.folder.paths ()
        Assert.Contains(KitRelativePath "Laser600.item-json", paths)
        Assert.Contains(KitRelativePath "ML.item-json", paths)
        let needed = closureOf laser600 @ closureOf ml
        Assert.Equal<LibraryEntryEnvelope list>(closureOfE1 (), needed |> List.distinct)
        for envelope in needed do
            Assert.Contains(envelope.snapshotPath, paths)
        Assert.DoesNotContain(KitRelativePath "QZ.item-json", paths)
        Assert.DoesNotContain(KitRelativePath "CPL1.item-json", paths)
        Assert.DoesNotContain((itemModelEnvelope ItemModelIds.circularPolarizerModelId).snapshotPath, paths)
        Assert.DoesNotContain((catalogueEnvelope SampleIds.glassPlate1mmId.toLibraryEntryId).snapshotPath, paths)

        // The copied E1's setup names the added pieces themselves, and resolves to them.
        Assert.Equal<Result<Experiment list, KitError>>(Ok [ e1 ], target.context.proxy.readExperiments ())
        let setups = target.context.proxy.readSetups () |> expectKitOk
        Assert.Equal<Result<Item list, SetupError>>(Ok [ laser600; ml ], Experiment.itemsUsedBy [ e1 ] setups items)
        assertSetupsHeld target [ e1 ]

    [<Fact>]
    let ``an item the kit holds by id, or by model and label, is mapped and not added`` () : unit =
        let clock = steppingClock ()
        let source, recorded = sourceHolding standardExperiments clock
        let target = emptyTarget ReadWriteKitAccess clock
        let heldLaser = itemOf "22700000-0000-4000-8000-000000000011" "Laser600" laserChoice FixedMounting
        holdItems target [ ml; heldLaser ]
        let itemFilesBefore = itemFilesOf target

        let report = target.context.copyExperimentsFrom source.context [ ExperimentId 1 ] [] |> expectCopyOk

        Assert.Empty(report.items)
        Assert.Empty(report.snapshots)
        Assert.Equal<Result<KitDataFile, KitError> list>(itemFilesBefore, itemFilesOf target)
        let items = target.context.proxy.readItems () |> expectKitOk
        Assert.Equal<Item list>([ heldLaser; ml ], items)
        let copiedE1 = recordedAs (ExperimentId 1) recorded
        let copiedBench = benchSetup |> reassigned laser600 heldLaser
        Assert.Equal<Setup list>([ copiedBench ], report.setups)
        Assert.Equal<Result<Experiment list, KitError>>(Ok [ copiedE1 ], target.context.proxy.readExperiments ())
        Assert.Equal<Result<Setup list, KitError>>(Ok [ copiedBench ], target.context.proxy.readSetups ())
        Assert.Equal<Result<Item list, SetupError>>(Ok [ heldLaser; ml ], Experiment.itemsUsedBy [ copiedE1 ] [ copiedBench ] items)
        assertSetupsHeld target [ copiedE1 ]

    [<Fact>]
    let ``an added item whose label the kit holds takes the next free label`` () : unit =
        let clock = steppingClock ()
        let source, recorded = sourceHolding standardExperiments clock
        let target = emptyTarget ReadWriteKitAccess clock
        let heldMl = itemOf "22700000-0000-4000-8000-000000000012" "ML" (sampleChoice SampleIds.glassPlate1mmId) (DialMounted DialPositive)
        holdItems target [ heldMl ]
        let e1 = recordedAs (ExperimentId 1) recorded

        let report = target.context.copyExperimentsFrom source.context [ ExperimentId 1 ] [] |> expectCopyOk

        let addedMl =
            {
                ml with label = ItemLabel "ML (2)"
            }
        Assert.Equal<Item list>([ laser600; addedMl ], report.items)
        let items = target.context.proxy.readItems () |> expectKitOk
        Assert.Equal<Item list>([ laser600; heldMl; addedMl ], items)
        Assert.Contains(KitRelativePath "ML (2).item-json", target.folder.paths ())
        Assert.Equal<Result<Experiment list, KitError>>(Ok [ e1 ], target.context.proxy.readExperiments ())
        let setups = target.context.proxy.readSetups () |> expectKitOk
        Assert.Equal<Result<Item list, SetupError>>(Ok [ laser600; addedMl ], Experiment.itemsUsedBy [ e1 ] setups items)
        assertSetupsHeld target [ e1 ]

    [<Fact>]
    let ``the user's choice overrides the default, and a choice the kit cannot take writes nothing`` () : unit =
        let clock = steppingClock ()
        let source, recorded = sourceHolding standardExperiments clock
        let e1 = recordedAs (ExperimentId 1) recorded
        let heldLaser = itemOf "22700000-0000-4000-8000-000000000011" "Laser600" laserChoice FixedMounting
        let heldTarget () : CopyKit =
            let target = emptyTarget ReadWriteKitAccess clock
            holdItems target [ ml; heldLaser ]
            target
        let addLaser : ItemCopyRow =
            {
                sourceItem = laser600
                choice = AddCopiedItem
            }

        let chosen = heldTarget ()
        let report = chosen.context.copyExperimentsFrom source.context [ ExperimentId 1 ] [ addLaser ] |> expectCopyOk

        let addedLaser =
            {
                laser600 with label = ItemLabel "Laser600 (2)"
            }
        Assert.Equal<Item list>([ addedLaser ], report.items)
        let items = chosen.context.proxy.readItems () |> expectKitOk
        Assert.Equal<Item list>([ heldLaser; addedLaser; ml ], items)
        Assert.Equal<Result<Experiment list, KitError>>(Ok [ e1 ], chosen.context.proxy.readExperiments ())
        let chosenSetups = chosen.context.proxy.readSetups () |> expectKitOk
        Assert.Equal<Result<Item list, SetupError>>(Ok [ addedLaser; ml ], Experiment.itemsUsedBy [ e1 ] chosenSetups items)
        assertSetupsHeld chosen [ e1 ]

        // Mapped onto the kit's ML of the glass plate, the slot takes that piece's sample binding, and the source's
        // multilayer is not brought in its place.
        let heldPlate = itemOf "22700000-0000-4000-8000-000000000012" "ML" (sampleChoice SampleIds.glassPlate1mmId) (DialMounted DialPositive)
        let plateTarget = emptyTarget ReadWriteKitAccess clock
        holdItems plateTarget [ heldPlate ]
        let ontoPlate : ItemCopyRow =
            {
                sourceItem = ml
                choice = MapOntoKitItem heldPlate.id
            }

        let plateReport = plateTarget.context.copyExperimentsFrom source.context [ ExperimentId 1 ] [ ontoPlate ] |> expectCopyOk

        Assert.Equal<Item list>([ laser600 ], plateReport.items)
        let onPlate =
            {
                benchSetup with
                    slots =
                        benchSetup.slots
                        |> List.map (fun held ->
                            if held.content.itemOpt = Some ml.id then
                                {
                                    held with placement = { held.placement with valueId = Some glassPlateValueId }
                                }
                                |> holding heldPlate
                            else
                                held)
            }
        Assert.Equal<Result<Experiment list, KitError>>(Ok [ e1 ], plateTarget.context.proxy.readExperiments ())
        Assert.Equal<Result<Setup list, KitError>>(Ok [ onPlate ], plateTarget.context.proxy.readSetups ())
        let plateItems = plateTarget.context.proxy.readItems () |> expectKitOk
        Assert.Equal<Result<Item list, SetupError>>(Ok [ laser600; heldPlate ], Experiment.itemsUsedBy [ e1 ] [ onPlate ] plateItems)
        let plateSnapshots = plateTarget.context.proxy.listSnapshots () |> expectKitOk
        Assert.Equal<LibraryEntryEnvelope list>(bySnapshotName (closureOf heldPlate @ closureOf laser600), plateSnapshots)
        Assert.DoesNotContain(catalogueEnvelope SampleIds.multilayerQwId.toLibraryEntryId, plateSnapshots)

        // From a valid kit and source, only the choice is altered: the kit refuses it before writing, then copies by default.
        let unknownId = ItemId (Guid.Parse "22700000-0000-4000-8000-0000000000ff")
        let mapOntoUnknown : ItemCopyRow =
            {
                sourceItem = ml
                choice = MapOntoKitItem unknownId
            }
        let addHeld : ItemCopyRow =
            {
                sourceItem = ml
                choice = AddCopiedItem
            }
        let isUnknown (error : KitLibraryError) : bool =
            match error with
            | KitLibraryItemErr (UnknownKitItemErr reason) -> reason.Contains(unknownId.value.ToString())
            | _ -> false
        let isDuplicate (error : KitLibraryError) : bool =
            match error with
            | KitLibraryItemErr (DuplicateItemIdErr reason) -> reason.Contains(ml.id.value.ToString())
            | _ -> false
        let refusals : (ItemCopyRow * (KitLibraryError -> bool)) list = [ mapOntoUnknown, isUnknown; addHeld, isDuplicate ]
        for row, isExpected in refusals do
            let target = heldTarget ()
            let before = heldBy target
            match target.context.copyExperimentsFrom source.context [ ExperimentId 1 ] [ row ] with
            | Error error -> Assert.True(isExpected error, $"Unexpected refusal: {error.describe}")
            | Ok _ -> Assert.Fail($"The choice for '{row.sourceItem.label.value}' must be refused.")
            Assert.Equal<HeldContent>(before, heldBy target)
            target.context.copyExperimentsFrom source.context [ ExperimentId 1 ] [] |> expectCopyOk |> ignore

    [<Fact>]
    let ``an added item's photo is copied with it`` () : unit =
        let clock = steppingClock ()
        let photoPath = KitRelativePath "data/photos/ml.jpg"
        let photoText = KitFileText "ML photo\n"
        let source, _ =
            sourceRecording
                (fun attach ->
                    let photo = attach photoPath photoText
                    let pictured =
                        {
                            ml with photoOpt = Some photo.path
                        }
                    {
                        items = [ laser600; pictured; qz; cpl1 ]
                        setups = standardSetups
                        experiments = standardExperiments (attach runPath runText) (attach otherPath otherText)
                    })
                clock
        let pictured =
            {
                ml with photoOpt = Some photoPath
            }
        let photo : DataAttachment =
            {
                path = photoPath
                contentHash = hashOf photoText
            }
        let run : DataAttachment =
            {
                path = runPath
                contentHash = hashOf runText
            }

        let target = emptyTarget ReadWriteKitAccess clock
        let report = target.context.copyExperimentsFrom source.context [ ExperimentId 1 ] [] |> expectCopyOk

        Assert.Equal<Item list>([ laser600; pictured ], report.items)
        Assert.Equal<DataAttachment list>([ run; photo ], report.dataFiles)
        Assert.Equal<Result<Item list, KitError>>(Ok [ laser600; pictured ], target.context.proxy.readItems ())
        let copiedPhoto : KitDataFile =
            {
                path = photoPath
                text = photoText
                contentHash = hashOf photoText
            }
        Assert.Equal<Result<KitDataFile, KitError>>(Ok copiedPhoto, target.context.proxy.readDataFile photoPath)
        assertSetupsHeld target report.experiments

        // A kit already holding another photo of that name takes the copy under the next free suffix.
        let crowded = emptyTarget ReadWriteKitAccess clock
        let heldText = KitFileText "another photo\n"
        crowded.folder.placeLabFile photoPath heldText |> expectKitOk
        let suffixedPath = KitRelativePath "data/photos/ml (2).jpg"

        let crowdedReport = crowded.context.copyExperimentsFrom source.context [ ExperimentId 1 ] [] |> expectCopyOk

        let suffixed =
            {
                ml with photoOpt = Some suffixedPath
            }
        Assert.Equal<Item list>([ laser600; suffixed ], crowdedReport.items)
        Assert.Equal<Result<Item list, KitError>>(Ok [ laser600; suffixed ], crowded.context.proxy.readItems ())
        let suffixedPhoto : KitDataFile =
            {
                path = suffixedPath
                text = photoText
                contentHash = hashOf photoText
            }
        let heldPhoto : KitDataFile =
            {
                path = photoPath
                text = heldText
                contentHash = hashOf heldText
            }
        Assert.Equal<Result<KitDataFile, KitError>>(Ok suffixedPhoto, crowded.context.proxy.readDataFile suffixedPath)
        Assert.Equal<Result<KitDataFile, KitError>>(Ok heldPhoto, crowded.context.proxy.readDataFile photoPath)

    [<Fact>]
    let ``a copied experiment reopens using the explicitly selected target physical piece`` () : unit =
        let clock = steppingClock ()
        let piecesOf (choice : ItemModelChoice) (parameter : string) (element : SetupSlot) (bindingOfT : ElementValueId option) : SameModelPieces =
            {
                pieceA = itemOf "22700000-0000-4000-8000-000000000021" "Piece A" choice FixedMounting |> withParameter parameter 0.5
                pieceB = itemOf "22700000-0000-4000-8000-000000000022" "Piece B" choice FixedMounting |> withParameter parameter 0.75
                pieceT = itemOf "22700000-0000-4000-8000-000000000023" "Piece T" choice FixedMounting |> withParameter parameter 0.25
                element = element
                bindingOfT = bindingOfT
            }
        // A preset-bound model, then a standalone one: S6 does not wait for S8.2's models. The standalone piece binds no
        // preset, so the slot's preset binding gives way to none.
        let branches : SameModelPieces list =
            [
                piecesOf
                    laserChoice
                    "intensityScale"
                    (slot (ElementId "copied-source") LightSource (Some PresetIds.source600))
                    (Some PresetIds.source600)
                piecesOf
                    circularChoice
                    "c1.extinctionRatio"
                    (slot (ElementId "copied-circular") CircularPolarizer (Some PresetIds.polarizerCpLeft))
                    None
            ]
        for branch in branches do
            let pieceA = branch.pieceA
            let pieceB = branch.pieceB
            let pieceT = branch.pieceT
            let unassigned = branch.element
            Assert.Equal<ItemModelInstance>(pieceA.model, pieceB.model)
            Assert.Equal<ItemModelInstance>(pieceB.model, pieceT.model)
            Assert.NotEqual<Parameter list>(pieceA.parameters, pieceB.parameters)
            Assert.NotEqual<Parameter list>(pieceB.parameters, pieceT.parameters)
            let onB = setupOf "Pieces" [ unassigned |> holding pieceB; detector ]
            let unattached = darkLine (ExperimentId 1) None
            let usesB =
                {
                    unattached with setup = onB.reference
                }
            let source, _ =
                sourceRecording
                    (fun _ ->
                        {
                            items = [ pieceA; pieceB ]
                            setups = [ onB ]
                            experiments = [ usesB ]
                        })
                    clock
            let toT : ItemCopyRow =
                {
                    sourceItem = pieceB
                    choice = MapOntoKitItem pieceT.id
                }

            // A target piece whose model snapshot the kit lacks is refused; the source's model is never taken instead.
            let unmodelled = emptyTarget ReadWriteKitAccess clock
            unmodelled.context.proxy.writeItem pieceT |> expectKitOk
            let unmodelledBefore = heldBy unmodelled
            match unmodelled.context.copyExperimentsFrom source.context [ ExperimentId 1 ] [ toT ] with
            | Error (UnheldEntryErr reason) ->
                let modelVersion =
                    match pieceT.model with
                    | PresetInstance (model, _)
                    | SampleInstance (model, _)
                    | ItemModelVersionInstance model -> model
                Assert.Contains(modelVersion.itemModelId.value.ToString(), reason)
                Assert.Contains("Piece T", reason)
            | Error error -> Assert.Fail($"Expected UnheldEntryErr, got {error.describe}")
            | Ok _ -> Assert.Fail("A target piece without its model snapshot must be refused.")
            Assert.Equal<HeldContent>(unmodelledBefore, heldBy unmodelled)

            let target = emptyTarget ReadWriteKitAccess clock
            holdItems target [ pieceT ]
            let pieceTFile = KitRelativePath "Piece T.item-json"
            let pieceTBefore = target.context.proxy.readDataFile pieceTFile |> expectKitOk

            target.context.copyExperimentsFrom source.context [ ExperimentId 1 ] [ toT ] |> expectCopyOk |> ignore

            let later = reopened target clock
            let items = later.context.proxy.readItems () |> expectKitOk
            let setups = later.context.proxy.readSetups () |> expectKitOk
            let copied = later.context.proxy.readExperiments () |> expectKitOk |> List.exactlyOne
            match Experiment.itemsUsedBy [ copied ] setups items with
            | Ok [ resolved ] ->
                Assert.Equal<ItemId>(pieceT.id, resolved.id)
                Assert.Equal<Parameter list>(pieceT.parameters, resolved.parameters)
            | Ok used -> Assert.Fail($"The copied experiment must resolve exactly the chosen piece, not {List.length used} item(s).")
            | Error error -> Assert.Fail(error.describe)
            let onT =
                {
                    unassigned with placement = { unassigned.placement with valueId = branch.bindingOfT }
                }
            Assert.Equal<Result<SetupSlot list, SetupError>>(Ok [ onT |> holding pieceT; detector ], copied.slotsIn setups)
            Assert.Equal<Item list>([ pieceT ], items)
            Assert.Equal<KitDataFile>(pieceTBefore, later.context.proxy.readDataFile pieceTFile |> expectKitOk)
            let paths = later.folder.paths ()
            Assert.DoesNotContain(KitRelativePath "Piece A.item-json", paths)
            Assert.DoesNotContain(KitRelativePath "Piece B.item-json", paths)

    [<Fact>]
    let ``copying two same-model pieces preserves separate choices and shared use`` () : unit =
        let clock = steppingClock ()
        let thickness = "substrate.thickness"
        let plateA = itemOf "22700000-0000-4000-8000-000000000031" "QZ-A" (sampleChoice SampleIds.glassPlate1mmId) (DialMounted DialPositive) |> withParameter thickness 1.0e-3
        let unpictured = itemOf "22700000-0000-4000-8000-000000000032" "QZ-B" (sampleChoice SampleIds.glassPlate1mmId) (DialMounted DialPositive) |> withParameter thickness 1.1e-3
        let plateT = itemOf "22700000-0000-4000-8000-000000000033" "QZ-T" (sampleChoice SampleIds.glassPlate1mmId) (DialMounted DialPositive) |> withParameter thickness 0.9e-3
        let photoPath = KitRelativePath "data/photos/b.jpg"
        let photoText = KitFileText "QZ-B photo\n"
        let plateB =
            {
                unpictured with photoOpt = Some photoPath
            }
        let plate (id : string) (piece : Item) : SetupSlot =
            slot (ElementId id) CatalogueKind.Sample (Some glassPlateValueId) |> holding piece
        let bothPlates = setupOf "Both plates" [ plate "copied-plate-a" plateA; plate "copied-plate-b" plateB; detector ]
        let plateBOnly = setupOf "Plate B" [ plate "copied-plate-b" plateB; detector ]
        let firstLine = darkLine (ExperimentId 1) None
        let both =
            {
                firstLine with setup = bothPlates.reference
            }
        let secondLine = darkLine (ExperimentId 2) None
        let onlyB =
            {
                secondLine with setup = plateBOnly.reference
            }
        let source, _ =
            sourceRecording
                (fun attach ->
                    let photo = attach photoPath photoText
                    Assert.Equal<KitRelativePath>(photoPath, photo.path)
                    {
                        items = [ plateA; plateB ]
                        setups = [ bothPlates; plateBOnly ]
                        experiments = [ both; onlyB ]
                    })
                clock
        let target = emptyTarget ReadWriteKitAccess clock
        holdItems target [ plateT ]
        let rows : ItemCopyRow list =
            [
                {
                    sourceItem = plateA
                    choice = MapOntoKitItem plateT.id
                }
                {
                    sourceItem = plateB
                    choice = AddCopiedItem
                }
            ]

        let report = target.context.copyExperimentsFrom source.context [ ExperimentId 1; ExperimentId 2 ] rows |> expectCopyOk

        Assert.Equal<Item list>([ plateB ], report.items)
        let later = reopened target clock
        let items = later.context.proxy.readItems () |> expectKitOk
        Assert.Equal<Item list>([ plateB; plateT ], items)
        let setups = later.context.proxy.readSetups () |> expectKitOk
        match later.context.proxy.readExperiments () |> expectKitOk with
        | [ first; second ] ->
            Assert.Equal<Result<Item list, SetupError>>(Ok [ plateB; plateT ], Experiment.itemsUsedBy [ first ] setups items)
            Assert.Equal<Result<Item list, SetupError>>(Ok [ plateB ], Experiment.itemsUsedBy [ second ] setups items)
            Assert.Equal<Result<SetupSlot list, SetupError>>(
                Ok [ plate "copied-plate-a" plateT; plate "copied-plate-b" plateB; detector ], first.slotsIn setups)
            Assert.Equal<Result<SetupSlot list, SetupError>>(Ok [ plate "copied-plate-b" plateB; detector ], second.slotsIn setups)
        | experiments -> Assert.Fail($"Both experiments must be copied, not {List.length experiments}.")
        Assert.Equal<Result<LibraryEntryEnvelope list, KitError>>(Ok (bySnapshotName (closureOf plateB)), later.context.proxy.listSnapshots ())
        let copiedPhoto : KitDataFile =
            {
                path = photoPath
                text = photoText
                contentHash = hashOf photoText
            }
        Assert.Equal<Result<KitDataFile, KitError>>(Ok copiedPhoto, later.context.proxy.readDataFile photoPath)
        Assert.DoesNotContain(KitRelativePath "QZ-A.item-json", later.folder.paths ())

    [<Fact>]
    let ``a copy brings the setups its experiments reference, their items mapped onto the kit's`` () : unit =
        let clock = steppingClock ()
        let source, recorded = sourceHolding standardExperiments clock
        let target = emptyTarget ReadWriteKitAccess clock

        let report = target.context.copyExperimentsFrom source.context [ ExperimentId 1 ] [] |> expectCopyOk

        Assert.Equal<Setup list>([ benchSetup ], report.setups)
        let paths = target.folder.paths ()
        Assert.Contains(KitRelativePath "Bench.setup-json", paths)
        Assert.DoesNotContain(KitRelativePath "Plate.setup-json", paths)
        Assert.DoesNotContain(KitRelativePath "Dark.setup-json", paths)
        let copied = target.context.proxy.readSetups () |> expectKitOk
        Assert.Equal<Setup list>([ benchSetup ], copied)
        Assert.Equal<ItemId option list>(
            [ Some laser600.id; Some ml.id; None ], (List.exactlyOne copied).slots |> List.map (fun held -> held.content.itemOpt))
        assertSetupsHeld target [ recordedAs (ExperimentId 1) recorded ]

        // `Bench` at revision 2 held ML at revision 1 too; the kit's ML of the same model and label, under another id,
        // takes its place in every revision.
        let firstRevision : SetupRevisionRecord =
            {
                revision = SetupRevision.first
                slots = [ sampleSlot |> holding ml; detector ]
            }
        let revisedBench =
            {
                benchSetup with
                    revision = SetupRevision 2
                    history = [ firstRevision ]
            }
        let revisedSource, revisedRecorded =
            sourceRecording
                (fun attach ->
                    let run = attach runPath runText
                    let e1 = experimentE1 run
                    {
                        items = standardItems
                        setups = [ revisedBench; plateSetup; darkSetup ]
                        experiments = [ { e1 with setup = revisedBench.reference }; experimentE2 ]
                    })
                clock
        let heldMl = itemOf "22700000-0000-4000-8000-000000000013" "ML" (sampleChoice SampleIds.multilayerQwId) (DialMounted DialPositive)
        let mapped = emptyTarget ReadWriteKitAccess clock
        holdItems mapped [ heldMl ]

        let mappedReport = mapped.context.copyExperimentsFrom revisedSource.context [ ExperimentId 1 ] [] |> expectCopyOk

        Assert.Equal<Item list>([ laser600 ], mappedReport.items)
        let mappedBench = revisedBench |> reassigned ml heldMl
        Assert.Equal<Setup list>([ mappedBench ], mappedReport.setups)
        Assert.Equal<Result<Setup list, KitError>>(Ok [ mappedBench ], mapped.context.proxy.readSetups ())
        let revisions = mappedBench.slots :: (mappedBench.history |> List.map (fun record -> record.slots))
        Assert.Equal(2, List.length revisions)
        for slots in revisions do
            let heldIds = slots |> List.choose (fun held -> held.content.itemOpt)
            Assert.Contains(heldMl.id, heldIds)
            Assert.DoesNotContain(ml.id, heldIds)
        let mappedE1 = recordedAs (ExperimentId 1) revisedRecorded
        Assert.Equal<Result<Experiment list, KitError>>(Ok [ mappedE1 ], mapped.context.proxy.readExperiments ())
        let mappedItems = mapped.context.proxy.readItems () |> expectKitOk
        Assert.Equal<Result<Item list, SetupError>>(Ok [ laser600; heldMl ], Experiment.itemsUsedBy [ mappedE1 ] [ mappedBench ] mappedItems)

    [<Fact>]
    let ``a copied setup the kit holds with the same content is not written again, and one with other content takes the next free name`` () : unit =
        let clock = steppingClock ()
        let source, recorded = sourceHolding standardExperiments clock
        let e1 = recordedAs (ExperimentId 1) recorded

        // The kit's `Bench` is the source's: nothing is written for it.
        let same = emptyTarget ReadWriteKitAccess clock
        same.context.proxy.writeSetup benchSetup |> expectKitOk
        let benchFile = same.context.proxy.readKindFile SetupKitFile benchSetup.name.fileStem |> expectKitOk
        let writes = ResizeArray<KitFileKind>()
        let watching =
            {
                same.context with
                    proxy =
                        {
                            same.context.proxy with
                                writeKindFile =
                                    fun kind stem text ->
                                        writes.Add kind
                                        same.context.proxy.writeKindFile kind stem text
                        }
            }

        let sameReport = watching.copyExperimentsFrom source.context [ ExperimentId 1 ] [] |> expectCopyOk

        Assert.Empty(sameReport.setups)
        Assert.DoesNotContain(SetupKitFile, writes)
        Assert.Equal<KitFileText>(benchFile, same.context.proxy.readKindFile SetupKitFile benchSetup.name.fileStem |> expectKitOk)
        Assert.Equal<Result<Setup list, KitError>>(Ok [ benchSetup ], same.context.proxy.readSetups ())
        Assert.Equal<Result<Experiment list, KitError>>(Ok [ e1 ], same.context.proxy.readExperiments ())

        // The kit's `Bench` holds other slots: the copy takes `Bench (2)`, and E1 follows it at its revision.
        let other = emptyTarget ReadWriteKitAccess clock
        let otherBench =
            {
                benchSetup with slots = [ detector ]
            }
        other.context.proxy.writeSetup otherBench |> expectKitOk

        let otherReport = other.context.copyExperimentsFrom source.context [ ExperimentId 1 ] [] |> expectCopyOk

        let renamed =
            {
                benchSetup with name = SetupName "Bench (2)"
            }
        Assert.Equal<Setup list>([ renamed ], otherReport.setups)
        Assert.Contains(KitRelativePath "Bench (2).setup-json", other.folder.paths ())
        Assert.Equal<Result<Setup list, KitError>>(Ok [ otherBench; renamed ], other.context.proxy.readSetups ())
        let followed =
            {
                e1 with setup = { e1.setup with name = SetupName "Bench (2)" }
            }
        Assert.Equal<SetupReference>({ name = SetupName "Bench (2)"; revision = e1.setup.revision }, followed.setup)
        Assert.Equal<Experiment list>([ followed ], otherReport.experiments)
        Assert.Equal<Result<Experiment list, KitError>>(Ok [ followed ], other.context.proxy.readExperiments ())
        assertSetupsHeld other [ followed ]
