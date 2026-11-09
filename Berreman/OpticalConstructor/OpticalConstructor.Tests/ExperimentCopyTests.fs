namespace OpticalConstructor.Tests

open System
open System.Text
open Xunit
open Softellect.Sys.Primitives
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Placement
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.Lifecycle
open OpticalConstructor.Domain.Experiments
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.CatalogueImport
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.KitFolders
open OpticalConstructor.Domain.KitLibrary
open OpticalConstructor.Storage.CatalogueImportStore
open OpticalConstructor.Storage.KitContentFiles
open OpticalConstructor.Storage.KitFolderStore

/// Experiments copied from another kit with the data files they reference and the snapshots they need, between
/// in-memory kits: the source opened read-only, the target read-write unless a fact says otherwise (spec 0050 S6).
module ExperimentCopyTests =

    /// One in-memory kit: its folder, as an outside writer sees it, and the context it is opened through.
    type private CopyKit =
        {
            folder : InMemoryKitFolder
            context : KitContext
        }

    /// Everything a kit holds as read: its paths, each path read as a data file (a folder or a snapshot answers its
    /// refusal), its snapshots and its experiments.
    type private HeldContent =
        {
            paths : KitRelativePath list
            files : Result<KitDataFile, KitError> list
            snapshots : Result<LibraryEntryEnvelope list, KitError>
            experiments : Result<Experiment list, KitError>
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

    let private heldBy (kit : CopyKit) : HeldContent =
        let paths = kit.folder.paths ()
        {
            paths = paths
            files = paths |> List.map kit.context.proxy.readDataFile
            snapshots = kit.context.proxy.listSnapshots ()
            experiments = kit.context.proxy.readExperiments ()
        }

    let private descriptor (id : ElementId) (kind : CatalogueKind) (binding : ElementBinding) : ElementDescriptor =
        let placement = ElementPlacement.create kind TablePoint.origin
        {
            elementId = id
            label = ElementLabel $"Copied {id.value}"
            kind = kind
            placement = PlacementSummary.ofPlacement placement
            binding = binding
            itemIdOpt = None
        }

    let private detector : ElementDescriptor = descriptor (ElementId "copied-detector") Detector Unbound

    let private darkLine (id : ExperimentId) (dataFileOpt : DataAttachment option) : Experiment =
        {
            id = id
            setup = [ detector ]
            varied = None
            measurement = CaptureTransmitted
            range = VariableRange.forVariable VaryR2
            dataFileOpt = dataFileOpt
            sourceWaveLengthOpt = None
        }

    /// E1 binds the multilayer sample at version 1 and the 600 nm source by its entry id, and attaches the run.
    let private experimentE1 (run : DataAttachment) : Experiment =
        let sample = SampleVersionRef (SampleVersionId.firstOf SampleIds.multilayerQwId)
        let attached = darkLine (ExperimentId 1) (Some run)
        {
            attached with
                setup =
                    [
                        descriptor (ElementId "copied-source") LightSource (BoundByEntryId PresetIds.source600)
                        descriptor (ElementId "copied-sample") CatalogueKind.Sample (BoundByVersion sample)
                        detector
                    ]
        }

    /// E2 binds the 1 mm glass plate at version 1 and attaches nothing.
    let private experimentE2 : Experiment =
        let plate = SampleVersionRef (SampleVersionId.firstOf SampleIds.glassPlate1mmId)
        let unattached = darkLine (ExperimentId 2) None
        {
            unattached with
                setup = [ descriptor (ElementId "copied-plate") CatalogueKind.Sample (BoundByVersion plate); detector ]
        }

    /// E1, E2, and E3, which binds nothing and attaches the other file.
    let private standardExperiments (run : DataAttachment) (other : DataAttachment) : Experiment list =
        [ experimentE1 run; experimentE2; darkLine (ExperimentId 3) (Some other) ]

    /// The source: its lab files placed by an outside writer and attached in place, its experiments recorded through the
    /// library (so its snapshots are the library's), then closed and opened read-only. Answers it with what it recorded.
    let private sourceHolding
        (experimentsOf : DataAttachment -> DataAttachment -> Experiment list)
        (clock : ClockProvider)
        : CopyKit * Experiment list =
        let folder = newKitFolder sourceKitId (KitName "Copy source") clock
        let writer = openCopyKit folder ReadWriteKitAccess clock
        let attach (path : KitRelativePath) (text : KitFileText) : DataAttachment =
            folder.placeLabFile path text |> expectKitOk
            writer.context.proxy.readLabFile path
            |> Result.bind (writer.context.proxy.attachLabFile KitDataFolder.root)
            |> expectKitOk
        let experiments = experimentsOf (attach runPath runText) (attach otherPath otherText)
        let library : KitLibraryContext =
            {
                kit = writer.context
                imports = CatalogueImportContext.create (BuiltinCatalogue.freshStores ()).proxy
            }
        for experiment in experiments do
            library.recordExperiment experiment |> expectCopyOk
        writer.context.close () |> expectKitOk
        openCopyKit folder ReadOnlyKitAccess clock, experiments

    let private recordedAs (id : ExperimentId) (recorded : Experiment list) : Experiment =
        recorded |> List.find (fun experiment -> experiment.id = id)

    let private catalogueEnvelope (id : LibraryEntryId) : LibraryEntryEnvelope =
        (BuiltinCatalogue.catalogueData ()).envelopes
        |> List.find (fun envelope -> envelope.content.id = id)

    /// E1's closure in visit order: the source preset, then the multilayer sample, its materials and their categories.
    let private closureOfE1 () : LibraryEntryEnvelope list =
        let sourceId =
            PresetIds.source600.tryToLibraryEntryId
            |> Option.defaultWith (fun () -> failwith "The 600 nm source must have an entry id.")
        [
            catalogueEnvelope sourceId
            catalogueEnvelope SampleIds.multilayerQwId.toLibraryEntryId
            catalogueEnvelope MaterialIds.vacuumMaterial.toLibraryEntryId
            catalogueEnvelope CategoryIds.vacuumCategory.toLibraryEntryId
            catalogueEnvelope MaterialIds.glass152.toLibraryEntryId
            catalogueEnvelope CategoryIds.glass.toLibraryEntryId
        ]

    let private ordinal (paths : KitRelativePath list) : KitRelativePath list =
        paths |> List.sortWith (fun first second -> String.CompareOrdinal(first.value, second.value))

    let private bySnapshotName (envelopes : LibraryEntryEnvelope list) : LibraryEntryEnvelope list =
        envelopes |> List.sortWith (fun first second -> String.CompareOrdinal(first.snapshotFileName.value, second.snapshotFileName.value))

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

        let report = target.context.copyExperimentsFrom source.context [ ExperimentId 1 ] |> expectCopyOk

        let expected : ExperimentCopyReport =
            {
                experiments = [ e1 ]
                dataFiles = [ run ]
                snapshots = closure |> List.map (fun envelope -> envelope.libraryVersionRef)
            }
        Assert.Equal<ExperimentCopyReport>(expected, report)
        let kitPaths =
            [ ".kit-json"; ".oc"; ".oc/lock"; "E1.experiment-json"; "data"; "data/raw"; "data/raw/run 1.csv"; "library" ]
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

    [<Fact>]
    let ``a copied experiment whose id the kit holds takes the next free id`` () : unit =
        let clock = steppingClock ()
        let source, recorded = sourceHolding standardExperiments clock
        let target = emptyTarget ReadWriteKitAccess clock
        let reflected = darkLine (ExperimentId 1) None
        let heldE1 =
            {
                reflected with measurement = CaptureReflected
            }
        let both = darkLine (ExperimentId 2) None
        let heldE2 =
            {
                both with measurement = CaptureBoth
            }
        for experiment in [ heldE1; heldE2 ] do
            target.context.proxy.writeExperiment experiment |> expectKitOk
        let copiedE3 = recordedAs (ExperimentId 3) recorded
        let sourceE2 = recordedAs (ExperimentId 2) recorded
        let copiedE4 =
            {
                sourceE2 with id = ExperimentId 4
            }

        let report = target.context.copyExperimentsFrom source.context [ ExperimentId 2; ExperimentId 3 ] |> expectCopyOk

        Assert.Equal<Experiment list>([ copiedE4; copiedE3 ], report.experiments)
        Assert.Equal<Result<Experiment list, KitError>>(Ok [ heldE1; heldE2; copiedE3; copiedE4 ], target.context.proxy.readExperiments ())

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

        let report = target.context.copyExperimentsFrom source.context [ ExperimentId 1 ] |> expectCopyOk

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

        let rewrittenReport = fresh.context.copyExperimentsFrom source.context [ ExperimentId 1 ] |> expectCopyOk

        Assert.Equal<DataAttachment list>([ rewritten ], rewrittenReport.dataFiles)
        let rewrittenE1 =
            {
                e1 with dataFileOpt = Some rewritten
            }
        Assert.Equal<Result<Experiment list, KitError>>(Ok [ rewrittenE1 ], fresh.context.proxy.readExperiments ())

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

        let report = target.context.copyExperimentsFrom source.context [ ExperimentId 1; ExperimentId 5 ] |> expectCopyOk

        Assert.Equal<DataAttachment list>([ run ], report.dataFiles)
        Assert.Equal<Result<Experiment list, KitError>>(Ok [ e1; e5 ], target.context.proxy.readExperiments ())
        let dataPaths =
            target.folder.paths ()
            |> List.filter (fun path -> path.value.StartsWith("data/", StringComparison.Ordinal))
        Assert.Equal<KitRelativePath list>([ KitRelativePath "data/raw"; runPath ], dataPaths)

    [<Fact>]
    let ``a snapshot the kit already holds is kept and not written again`` () : unit =
        let clock = steppingClock ()
        let source, _ = sourceHolding standardExperiments clock
        let target = emptyTarget ReadWriteKitAccess clock
        let held = catalogueEnvelope MaterialIds.glass152.toLibraryEntryId
        let closure = closureOfE1 ()
        Assert.Contains(held, closure)
        target.context.proxy.writeSnapshot held |> expectKitOk

        let report = target.context.copyExperimentsFrom source.context [ ExperimentId 1 ] |> expectCopyOk

        let added = closure |> List.filter (fun envelope -> envelope <> held) |> List.map (fun envelope -> envelope.libraryVersionRef)
        Assert.Equal<LibraryVersionRef list>(added, report.snapshots)
        let listed = target.context.proxy.listSnapshots () |> expectKitOk
        Assert.Equal<LibraryEntryEnvelope list>(bySnapshotName closure, listed)
        Assert.Single(listed |> List.filter (fun envelope -> envelope.libraryVersionRef = held.libraryVersionRef)) |> ignore

    [<Fact>]
    let ``a copy whose data file the source lacks writes nothing`` () : unit =
        let clock = steppingClock ()
        let source, _ = sourceHolding standardExperiments clock
        let target = emptyTarget ReadWriteKitAccess clock
        source.folder.removeLabFile runPath |> expectKitOk
        let before = heldBy target

        match target.context.copyExperimentsFrom source.context [ ExperimentId 1 ] with
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

        let copied = target.context.copyExperimentsFrom watched [ ExperimentId 1 ]

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
            match target.context.copyExperimentsFrom source.context ids with
            | Error (KitLibraryKitErr (MissingKitFileErr reason)) -> Assert.Equal("E9.experiment-json", reason)
            | Error error -> Assert.Fail($"Expected MissingKitFileErr, got {error.describe}")
            | Ok _ -> Assert.Fail("An experiment the source does not hold must be refused.")

            Assert.Equal<HeldContent>(before, heldBy target)
