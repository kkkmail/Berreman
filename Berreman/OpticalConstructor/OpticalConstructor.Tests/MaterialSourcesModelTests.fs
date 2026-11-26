namespace OpticalConstructor.Tests

open System
open System.Text
open System.Threading
open Xunit
open Softellect.Sys.Primitives
open OpticalConstructor.Controls
open OpticalConstructor.Domain
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.CatalogueFiles
open OpticalConstructor.Domain.CatalogueImport
open OpticalConstructor.Domain.MaterialSources
open OpticalConstructor.Domain.MaterialSourcePulls
open OpticalConstructor.Storage.CatalogueFolderStore
open OpticalConstructor.Storage.CatalogueImportStore
open OpticalConstructor.Ui
open OpticalConstructor.Ui.JobRunner
open OpticalConstructor.Ui.MaterialSourcesView

/// The Online sources surface's update: search, pull, and Update snapshot with per-file progress and Cancel, one
/// update at a time, telling the opener every caption (spec 0050 S13.1).
module MaterialSourcesModelTests =

    let private testSourceName : MaterialSourceName = MaterialSourceName "Test source"

    let private testLicence : EntryLicence = EntryLicence "CC0-1.0"

    let private firstVersion : MaterialSourceVersion = MaterialSourceVersion "2026-11-01"

    let private laterVersion : MaterialSourceVersion = MaterialSourceVersion "2026-11-04"

    let private siliconHit : MaterialSourceHit =
        {
            record = MaterialSourceRecordId "main/Si/nk/Aspnes.yml"
            name = EntryName "Si (Silicon), Aspnes"
        }

    let private hashOf (text : string) : ContentHash = text |> Encoding.UTF8.GetBytes |> FileByteContent |> ContentHash.ofBytes

    let private pulledMaterialId : MaterialId = Guid.Parse "45000001-0450-4450-8450-000000000450" |> MaterialId

    let private pulledEntryId : LibraryEntryId = pulledMaterialId.toLibraryEntryId

    let private pulledCaption : ControlCaption = ControlCaption "Pulled Si (Silicon), Aspnes as version 1."

    let private cancelledCaption : ControlCaption = ControlCaption "The snapshot update was cancelled; the held snapshot is unchanged."

    let private siliconRecord (hash : ContentHash) : InMemorySourceRecord =
        {
            hit = siliconHit
            recordHash = hash
            citation = EntryCitation "D. E. Aspnes and A. A. Studna, Phys. Rev. B 27, 985 (1983)"
            material = BuiltinCatalogue.material MaterialIds.silicon
        }

    /// A source holding Silicon's record at 2026-11-01, installed unobserved.
    let private heldSource () : InMemoryMaterialSource =
        let source = InMemoryMaterialSource.create testSourceName testLicence
        source.publish firstVersion [ siliconRecord (hashOf "abc") ]
        let installed : SourceSnapshotInfo =
            {
                version = firstVersion
                files = SourceSnapshotFileCount 1
            }
        Assert.Equal<Result<SourceSnapshotOutcome, MaterialSourceError>>(
            Ok (SourceSnapshotUpdated installed),
            (MaterialSourceProxy.createInMemory source).updateSnapshot SourceSnapshotObserver.unobserved)
        source

    /// The surface's import seam, as step 444's facts build theirs, offering `source`, and what it told its opener.
    type private SurfaceFixture =
        {
            imports : CatalogueImportContext
            source : InMemoryMaterialSource
            applied : ResizeArray<ControlCaption>
        }

        member this.contextOn (jobs : SourceSnapshotJobHost) : MaterialSourcesContext =
            {
                imports = this.imports
                jobs = jobs
                pullsApplied = fun caption -> this.applied.Add caption
            }

    /// The Built-in library in memory, minting the fixed GUID, offering the double over `source`.
    let private fixtureOver (source : InMemoryMaterialSource) : SurfaceFixture =
        let context = CatalogueImportContext.create (CatalogueImportProxy.createInMemory ()).proxy
        context.importFrom (CatalogueProxy.createBuiltIn ()) |> CatalogueImportTestData.expectImportOk
        |> List.map _.byDefault |> context.apply |> CatalogueImportTestData.expectImportOk |> ignore
        {
            imports =
                {
                    context with
                        newEntryId = fun () -> pulledEntryId
                        materialSources = [ MaterialSourceProxy.createInMemory source ]
                }
            source = source
            applied = ResizeArray<ControlCaption>()
        }

    /// A host that runs nothing: it records each update it is asked to start beside the cancellation source its handle
    /// cancels, and each cancel by the update's 1-based number.
    type private RecordingSnapshotHost =
        {
            started : ResizeArray<(SourceSnapshotObserver -> Result<SourceRefresh, MaterialSourcePullError>) * CancellationTokenSource>
            cancels : ResizeArray<int>
        }

        static member create () : RecordingSnapshotHost =
            {
                started = ResizeArray()
                cancels = ResizeArray()
            }

        member this.host : SourceSnapshotJobHost =
            {
                startSourceSnapshot =
                    fun update ->
                        let cancelSource = new CancellationTokenSource()
                        this.started.Add((update, cancelSource))
                        let number = this.started.Count
                        {
                            cancelSourceSnapshot =
                                fun () ->
                                    this.cancels.Add number
                                    cancelSource.Cancel()
                        }
            }

    /// `SourceSnapshotJobHost.immediate`, recording what it tells as the surface's messages, in order.
    let private immediateHost (messages : ResizeArray<MaterialSourcesMsg>) : SourceSnapshotJobHost =
        SourceSnapshotJobHost.immediate (fun progress -> messages.Add(SourceSnapshotProgressed progress)) (fun updateEnd ->
            messages.Add(SourceSnapshotEnded updateEnd))

    let private fedThrough (messages : MaterialSourcesMsg list) (model : MaterialSourcesModel) : MaterialSourcesModel =
        messages |> List.fold (fun current msg -> updateMaterialSources msg current) model

    [<Fact>]
    let ``the surface opens on the first source and its held snapshot`` () : unit =
        let fixture = fixtureOver (heldSource ())

        let opened = (fixture.contextOn SourceSnapshotJobHost.inert).initMaterialSources

        Assert.Equal<MaterialSourceName option>(Some testSourceName, opened.shownSourceOpt)
        Assert.Equal<MaterialSourceName option>(Some testSourceName, opened.shownSource |> Option.map _.sourceName)
        Assert.Equal<MaterialSourceVersion option>(Some firstVersion, opened.snapshotOpt)
        Assert.Equal<MaterialSourceHit list>([], opened.hits)
        Assert.Equal<MaterialSourceQuery>(MaterialSourceQuery "", opened.queryDraft)
        Assert.Equal<JobStatus>(Idle, opened.job.status)
        Assert.True(Option.isNone opened.handleOpt)
        Assert.Equal<ControlCaption option>(None, opened.status)
        Assert.Equal<MaterialSourcePullError option>(None, opened.error)

        let noSource : MaterialSourcesContext =
            {
                imports = CatalogueImportContext.create (CatalogueImportProxy.createInMemory ()).proxy
                jobs = SourceSnapshotJobHost.inert
                pullsApplied = ignore
            }
        let bare = noSource.initMaterialSources
        Assert.Equal<MaterialSourceName option>(None, bare.shownSourceOpt)
        Assert.True(Option.isNone bare.shownSource)
        Assert.Equal<MaterialSourceVersion option>(None, bare.snapshotOpt)
        Assert.Equal<MaterialSourceHit list>([], bare.hits)

    [<Fact>]
    let ``SearchMaterialSource lists the hits the shown source finds`` () : unit =
        let fixture = fixtureOver (heldSource ())
        let opened = (fixture.contextOn SourceSnapshotJobHost.inert).initMaterialSources

        let typed = updateMaterialSources (SetSourceQueryDraft (MaterialSourceQuery "silicon")) opened
        let searched = updateMaterialSources (SearchMaterialSource typed.queryDraft) typed

        Assert.Equal<MaterialSourceQuery>(MaterialSourceQuery "silicon", typed.queryDraft)
        Assert.Equal<MaterialSourceHit list>([ siliconHit ], searched.hits)
        Assert.Equal<MaterialSourcePullError option>(None, searched.error)

        let unheld = fixtureOver (InMemoryMaterialSource.create testSourceName testLicence)
        let openedUnheld = (unheld.contextOn SourceSnapshotJobHost.inert).initMaterialSources
        Assert.Equal<MaterialSourceVersion option>(None, openedUnheld.snapshotOpt)

        let refused = updateMaterialSources (SearchMaterialSource (MaterialSourceQuery "silicon")) openedUnheld

        match refused.error with
        | Some (PullSourceErr (SourceSnapshotMissingErr reason)) -> Assert.Contains(testSourceName.value, reason)
        | other -> Assert.Fail $"expected PullSourceErr (SourceSnapshotMissingErr _) over a source holding no snapshot; got %A{other}"
        Assert.Equal<MaterialSourceHit list>([], refused.hits)

    [<Fact>]
    let ``PullSourceRecord pulls the record and tells the opener what the library gained`` () : unit =
        let fixture = fixtureOver (heldSource ())
        let opened = (fixture.contextOn SourceSnapshotJobHost.inert).initMaterialSources

        let pulled = updateMaterialSources (PullSourceRecord siliconHit.record) opened

        Assert.Equal<ControlCaption option>(Some pulledCaption, pulled.status)
        Assert.Equal<MaterialSourcePullError option>(None, pulled.error)
        Assert.Equal<ControlCaption list>([ pulledCaption ], List.ofSeq fixture.applied)
        match fixture.imports.proxy.tryLatestEnvelope pulledEntryId |> CatalogueImportTestData.expectImportOk with
        | Some envelope ->
            Assert.Equal<VersionNumber>(VersionNumber.first, envelope.content.version)
            match envelope.content.toMaterialEntry with
            | Ok material -> Assert.Equal<EntryName>(siliconHit.name, material.name)
            | Error error -> Assert.Fail $"the pulled version is not a material: {error.describe}"
        | None -> Assert.Fail "the import seam holds no pulled material"

    [<Fact>]
    let ``UpdateSourceSnapshot runs the update with per-file progress, then refreshes the pulls and tells the opener`` () : unit =
        let fixture = fixtureOver (heldSource ())
        let messages = ResizeArray<MaterialSourcesMsg>()
        let opened = (fixture.contextOn (immediateHost messages)).initMaterialSources
        let pulled = updateMaterialSources (PullSourceRecord siliconHit.record) opened
        fixture.source.publish laterVersion [ siliconRecord (hashOf "abd") ]

        let started = updateMaterialSources UpdateSourceSnapshot pulled

        Assert.Equal<JobStatus>(Running, started.job.status)
        Assert.True(Option.isSome started.handleOpt)
        Assert.Equal<ControlCaption option>(None, started.status)
        let told = List.ofSeq messages
        match told with
        | [ SourceSnapshotProgressed progress; SourceSnapshotEnded _ ] ->
            Assert.Equal<JobStepCount>(JobStepCount 1, progress.completed)
            Assert.Equal<JobStepCount option>(Some (JobStepCount 1), progress.total)
            let progressed = updateMaterialSources (SourceSnapshotProgressed progress) started
            Assert.Equal<JobStatus>(Running, progressed.job.status)
            Assert.Equal<JobProgress option>(Some progress, progressed.job.progress)
        | other -> Assert.Fail $"expected one file's progress and then the end; got %A{other}"

        let ended = fedThrough told started

        let refreshed = ControlCaption "Snapshot 2026-11-04 holds 1 files. Si (Silicon), Aspnes: the record changed; pulled as version 2."
        Assert.Equal<JobStatus>(Completed, ended.job.status)
        Assert.Equal<SourceSnapshotFileCount option>(Some (SourceSnapshotFileCount 1), ended.job.committedResults)
        Assert.True(Option.isNone ended.handleOpt)
        Assert.Equal<MaterialSourceVersion option>(Some laterVersion, ended.snapshotOpt)
        Assert.Equal<ControlCaption option>(Some refreshed, ended.status)
        Assert.Contains("version 2", refreshed.value)
        Assert.Equal<ControlCaption list>([ pulledCaption; refreshed ], List.ofSeq fixture.applied)
        match fixture.imports.proxy.tryLatestEnvelope pulledEntryId |> CatalogueImportTestData.expectImportOk with
        | Some envelope -> Assert.Equal<VersionNumber>(VersionNumber.first.next, envelope.content.version)
        | None -> Assert.Fail "the refresh left no latest version"

        let recording = RecordingSnapshotHost.create ()
        let first = updateMaterialSources UpdateSourceSnapshot (fixture.contextOn recording.host).initMaterialSources
        let second = updateMaterialSources UpdateSourceSnapshot first
        Assert.Equal(1, recording.started.Count)
        Assert.Equal<MaterialSourcesModel>(first, second)

    [<Fact>]
    let ``a cancelled update keeps the held snapshot and says so`` () : unit =
        let fixture = fixtureOver (heldSource ())
        fixture.source.publish laterVersion [ siliconRecord (hashOf "abd") ]
        let recording = RecordingSnapshotHost.create ()
        let opened = (fixture.contextOn recording.host).initMaterialSources
        updateMaterialSources CancelSourceSnapshot opened |> ignore
        Assert.Empty(recording.cancels)

        let started = updateMaterialSources UpdateSourceSnapshot opened
        let cancelling = updateMaterialSources CancelSourceSnapshot started

        Assert.Equal<int list>([ 1 ], List.ofSeq recording.cancels)
        let update, cancelSource = recording.started[0]
        let observer : SourceSnapshotObserver =
            {
                reportSnapshotProgress = ignore
                cancellation = cancelSource.Token
            }
        let updateEnd = update observer
        Assert.Equal<Result<SourceRefresh, MaterialSourcePullError>>(Ok SourceRefreshCancelled, updateEnd)

        let cancelled = updateMaterialSources (SourceSnapshotEnded updateEnd) cancelling

        Assert.Equal<MaterialSourceVersion option>(Some firstVersion, cancelled.snapshotOpt)
        Assert.Equal<ControlCaption option>(Some cancelledCaption, cancelled.status)
        Assert.Equal<JobStatus>(Cancelled, cancelled.job.status)
        Assert.True(Option.isNone cancelled.handleOpt)
        Assert.Equal<Result<MaterialSourceVersion option, MaterialSourceError>>(
            Ok (Some firstVersion),
            (MaterialSourceProxy.createInMemory fixture.source).snapshotVersion ())
        Assert.Equal<ControlCaption list>([], List.ofSeq fixture.applied)

    [<Fact>]
    let ``a failed update tells why`` () : unit =
        let fixture = fixtureOver (heldSource ())
        let recording = RecordingSnapshotHost.create ()
        let started = updateMaterialSources UpdateSourceSnapshot (fixture.contextOn recording.host).initMaterialSources
        let offline = PullSourceErr (SourceDownloadErr "offline")

        let failed = updateMaterialSources (SourceSnapshotEnded (Error offline)) started

        Assert.Equal<MaterialSourcePullError option>(Some offline, failed.error)
        Assert.Equal("the source refused: the source's archive could not be downloaded: offline", offline.caption.value)
        Assert.Equal<JobStatus>(Idle, failed.job.status)
        Assert.True(Option.isNone failed.handleOpt)
        Assert.Equal<MaterialSourceVersion option>(Some firstVersion, failed.snapshotOpt)
        Assert.Equal<ControlCaption list>([], List.ofSeq fixture.applied)

        let restarted = updateMaterialSources UpdateSourceSnapshot failed
        Assert.Equal<JobStatus>(Running, restarted.job.status)
        Assert.Equal<MaterialSourcePullError option>(None, restarted.error)
        Assert.Equal(2, recording.started.Count)
