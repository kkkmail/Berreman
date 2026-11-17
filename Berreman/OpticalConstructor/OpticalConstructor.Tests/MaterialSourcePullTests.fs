namespace OpticalConstructor.Tests

open System
open System.Text
open System.Threading
open Xunit
open Softellect.Sys.Primitives
open OpticalConstructor.Domain
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.Lifecycle
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.CatalogueFiles
open OpticalConstructor.Domain.CatalogueImport
open OpticalConstructor.Domain.MaterialSources
open OpticalConstructor.Domain.MaterialSourcePulls
open OpticalConstructor.Storage.CatalogueFolderStore
open OpticalConstructor.Storage.CatalogueImportStore

/// A pulled record becomes a user material version, and an update mints one only where a record changed (spec 0050 S13.1).
module MaterialSourcePullTests =

    let private testSourceName : MaterialSourceName = MaterialSourceName "Test source"

    let private testLicence : EntryLicence = EntryLicence "CC0-1.0"

    let private firstVersion : MaterialSourceVersion = MaterialSourceVersion "2026-11-01"

    let private laterVersion : MaterialSourceVersion = MaterialSourceVersion "2026-11-04"

    let private siliconHit : MaterialSourceHit =
        {
            record = MaterialSourceRecordId "main/Si/nk/Aspnes.yml"
            name = EntryName "Si (Silicon), Aspnes"
        }

    let private langasiteHit : MaterialSourceHit =
        {
            record = MaterialSourceRecordId "other/La3Ga5SiO14/nk/Test.yml"
            name = EntryName "La3Ga5SiO14 (Langasite), Test"
        }

    let private hashOf (text : string) : ContentHash = text |> Encoding.UTF8.GetBytes |> FileByteContent |> ContentHash.ofBytes

    let private siliconCitation : EntryCitation = EntryCitation "D. E. Aspnes and A. A. Studna, Phys. Rev. B 27, 985 (1983)"

    let private pulledMaterialId : MaterialId = Guid.Parse "44400001-0444-4444-8444-000000000444" |> MaterialId

    let private pulledEntryId : LibraryEntryId = pulledMaterialId.toLibraryEntryId

    let private silicon () : MaterialEntry = BuiltinCatalogue.material MaterialIds.silicon

    let private langasite () : MaterialEntry = BuiltinCatalogue.material MaterialIds.langasite

    let private siliconRecord (hash : ContentHash) (material : MaterialEntry) : InMemorySourceRecord =
        {
            hit = siliconHit
            recordHash = hash
            citation = siliconCitation
            material = material
        }

    let private langasiteRecord () : InMemorySourceRecord =
        {
            hit = langasiteHit
            recordHash = hashOf "langasite"
            citation = EntryCitation "Test langasite record, 2026"
            material = langasite ()
        }

    /// The pull Silicon's record answers under the first snapshot.
    let private firstPull : SourcePullInfo =
        {
            source = testSourceName
            record = siliconHit.record
            recordHash = hashOf "abc"
            sourceVersion = firstVersion
            licence = testLicence
            citation = siliconCitation
        }

    type private PullFixture =
        {
            context : CatalogueImportContext
            source : InMemoryMaterialSource
            proxy : MaterialSourceProxy
        }

    /// The Built-in library in memory, minting the fixed GUID, beside a source holding Silicon's record at 2026-11-01.
    let private freshFixture () : PullFixture =
        let context = CatalogueImportContext.create (CatalogueImportProxy.createInMemory ()).proxy
        context.importFrom (CatalogueProxy.createBuiltIn ()) |> CatalogueImportTestData.expectImportOk
        |> List.map _.byDefault |> context.apply |> CatalogueImportTestData.expectImportOk |> ignore
        let source = InMemoryMaterialSource.create testSourceName testLicence
        source.publish firstVersion [ siliconRecord (hashOf "abc") (silicon ()) ]
        let proxy = MaterialSourceProxy.createInMemory source
        let installed : SourceSnapshotInfo =
            {
                version = firstVersion
                files = SourceSnapshotFileCount 1
            }
        Assert.Equal<Result<SourceSnapshotOutcome, MaterialSourceError>>(
            Ok (SourceSnapshotUpdated installed),
            proxy.updateSnapshot SourceSnapshotObserver.unobserved)
        {
            context =
                {
                    context with
                        newEntryId = fun () -> pulledEntryId
                }
            source = source
            proxy = proxy
        }

    let private resultAt (version : VersionNumber) (change : SourcePullChange) : SourcePullResult =
        {
            record = siliconHit.record
            name = siliconHit.name
            version =
                {
                    entryId = pulledEntryId
                    version = version
                }
            change = change
        }

    /// The fresh fixture after its one pull added Silicon's record at version 1.
    let private pulledFixture () : PullFixture =
        let fixture = freshFixture ()
        Assert.Equal<Result<SourcePullResult list, MaterialSourcePullError>>(
            Ok [ resultAt VersionNumber.first PullAddedEntry ],
            fixture.context.pullRecord fixture.proxy siliconHit.record)
        fixture

    let private heldPulled (fixture : PullFixture) : HeldEntry =
        match fixture.context.proxy.tryHeldEntry pulledEntryId |> CatalogueImportTestData.expectImportOk with
        | Some held -> held
        | None -> failwith $"the pulled entry {pulledEntryId.value} is not held"

    let private heldVersions (fixture : PullFixture) : VersionNumber list = (heldPulled fixture).versions |> List.map _.version

    let private snapshotOf (version : MaterialSourceVersion) (files : int) : SourceSnapshotInfo =
        {
            version = version
            files = SourceSnapshotFileCount files
        }

    let private refreshedWith (snapshot : SourceSnapshotInfo) (pulls : SourcePullResult list) : Result<SourceRefresh, MaterialSourcePullError> =
        let info : SourceRefreshInfo =
            {
                snapshot = snapshot
                pulls = pulls
            }
        Ok (SourceRefreshed info)

    [<Fact>]
    let ``a pulled record becomes a user material at version 1 under its pull provenance`` () : unit =
        let fixture = freshFixture ()

        Assert.Equal<Result<SourcePullResult list, MaterialSourcePullError>>(
            Ok [ resultAt VersionNumber.first PullAddedEntry ],
            fixture.context.pullRecord fixture.proxy siliconHit.record)

        match fixture.context.proxy.tryLatestEnvelope pulledEntryId |> CatalogueImportTestData.expectImportOk with
        | Some envelope ->
            Assert.Equal<VersionNumber>(VersionNumber.first, envelope.content.version)
            Assert.Equal<EntryProvenance>(SourcePullProvenance firstPull, envelope.content.provenance)
            Assert.Equal<EntryProtection>(UserManaged, envelope.content.provenance.protection)
            let expected =
                {
                    silicon () with
                        id = pulledMaterialId
                        name = siliconHit.name
                }
            Assert.Equal<Result<MaterialEntry, LibraryEntryError>>(Ok expected, envelope.content.toMaterialEntry)
        | None -> Assert.Fail "the pulled entry holds no latest version"
        Assert.Equal<VersionNumber list>([ VersionNumber.first ], heldVersions fixture)
        let pulled : PulledRecord =
            {
                version =
                    {
                        entryId = pulledEntryId
                        version = VersionNumber.first
                    }
                pull = firstPull
            }
        Assert.Equal<Result<PulledRecord list, CatalogueImportError>>(Ok [ pulled ], fixture.context.proxy.pulledRecordsOf testSourceName)

    [<Fact>]
    let ``pulling a held record again writes nothing while it is unchanged`` () : unit =
        let fixture = pulledFixture ()
        let latestBefore = fixture.context.proxy.tryLatestEnvelope pulledEntryId

        Assert.Equal<Result<SourcePullResult list, MaterialSourcePullError>>(
            Ok [ resultAt VersionNumber.first PullFoundUnchanged ],
            fixture.context.pullRecord fixture.proxy siliconHit.record)

        Assert.Equal<VersionNumber list>([ VersionNumber.first ], heldVersions fixture)
        Assert.Equal<Result<LibraryEntryEnvelope option, CatalogueImportError>>(latestBefore, fixture.context.proxy.tryLatestEnvelope pulledEntryId)

    [<Fact>]
    let ``an update that leaves a record unchanged mints no version`` () : unit =
        let fixture = pulledFixture ()
        fixture.source.publish laterVersion [ siliconRecord (hashOf "abc") (silicon ()) ]

        Assert.Equal<Result<SourceRefresh, MaterialSourcePullError>>(
            refreshedWith (snapshotOf laterVersion 1) [ resultAt VersionNumber.first PullFoundUnchanged ],
            fixture.context.updateSourceSnapshot fixture.proxy SourceSnapshotObserver.unobserved)

        Assert.Equal<VersionNumber list>([ VersionNumber.first ], heldVersions fixture)

    [<Fact>]
    let ``an update whose record changed mints version 2 and keeps version 1`` () : unit =
        let fixture = pulledFixture ()
        let versionOneBefore = fixture.context.proxy.tryHeldEnvelope pulledEntryId VersionNumber.first
        let changedHash = hashOf "abd"
        let versionTwo = VersionNumber.first.next
        Assert.NotEqual<MaterialPhysics>((silicon ()).physics, (langasite ()).physics)
        fixture.source.publish laterVersion [ siliconRecord changedHash (langasite ()) ]

        Assert.Equal<Result<SourceRefresh, MaterialSourcePullError>>(
            refreshedWith (snapshotOf laterVersion 1) [ resultAt versionTwo PullMintedVersion ],
            fixture.context.updateSourceSnapshot fixture.proxy SourceSnapshotObserver.unobserved)

        Assert.Equal<VersionNumber list>([ VersionNumber.first; versionTwo ], heldVersions fixture)
        Assert.Equal<VersionNumber>(versionTwo, (heldPulled fixture).latest)
        match fixture.context.proxy.tryHeldEnvelope pulledEntryId versionTwo |> CatalogueImportTestData.expectImportOk with
        | Some envelope ->
            let changedPull =
                {
                    firstPull with
                        recordHash = changedHash
                        sourceVersion = laterVersion
                }
            Assert.Equal<EntryProvenance>(SourcePullProvenance changedPull, envelope.content.provenance)
            let expected =
                {
                    silicon () with
                        id = pulledMaterialId
                        name = siliconHit.name
                        physics = (langasite ()).physics
                }
            Assert.Equal<Result<MaterialEntry, LibraryEntryError>>(Ok expected, envelope.content.toMaterialEntry)
        | None -> Assert.Fail "the refresh minted no version 2"
        match versionOneBefore with
        | Ok (Some _) -> ()
        | other -> Assert.Fail $"version 1 was not held before the update; got %A{other}"
        Assert.Equal<Result<LibraryEntryEnvelope option, CatalogueImportError>>(
            versionOneBefore,
            fixture.context.proxy.tryHeldEnvelope pulledEntryId VersionNumber.first)

    [<Fact>]
    let ``a record the snapshot no longer lists leaves its entry as it is`` () : unit =
        let fixture = pulledFixture ()
        let latestBefore = fixture.context.proxy.tryLatestEnvelope pulledEntryId
        fixture.source.publish laterVersion [ langasiteRecord () ]

        Assert.Equal<Result<SourceRefresh, MaterialSourcePullError>>(
            refreshedWith (snapshotOf laterVersion 1) [ resultAt VersionNumber.first PullRecordAbsent ],
            fixture.context.updateSourceSnapshot fixture.proxy SourceSnapshotObserver.unobserved)

        Assert.Equal<VersionNumber list>([ VersionNumber.first ], heldVersions fixture)
        Assert.Equal<Result<LibraryEntryEnvelope option, CatalogueImportError>>(latestBefore, fixture.context.proxy.tryLatestEnvelope pulledEntryId)

    [<Fact>]
    let ``a cancelled update refreshes nothing`` () : unit =
        let fixture = pulledFixture ()
        fixture.source.publish laterVersion [ siliconRecord (hashOf "abd") (langasite ()); langasiteRecord () ]
        use cancelSource = new CancellationTokenSource()
        let observer : SourceSnapshotObserver =
            {
                reportSnapshotProgress = fun _ -> cancelSource.Cancel()
                cancellation = cancelSource.Token
            }

        Assert.Equal<Result<SourceRefresh, MaterialSourcePullError>>(
            Ok SourceRefreshCancelled,
            fixture.context.updateSourceSnapshot fixture.proxy observer)

        Assert.Equal<VersionNumber list>([ VersionNumber.first ], heldVersions fixture)
        Assert.Equal<Result<MaterialSourceVersion option, MaterialSourceError>>(Ok (Some firstVersion), fixture.proxy.snapshotVersion ())

    [<Fact>]
    let ``a retired pulled material is never refreshed, and pulling its record again is refused`` () : unit =
        let fixture = pulledFixture ()
        fixture.context.proxy.retireEntry pulledEntryId |> CatalogueImportTestData.expectImportOk
        fixture.source.publish laterVersion [ siliconRecord (hashOf "abd") (langasite ()) ]

        Assert.Equal<Result<SourceRefresh, MaterialSourcePullError>>(
            refreshedWith (snapshotOf laterVersion 1) [],
            fixture.context.updateSourceSnapshot fixture.proxy SourceSnapshotObserver.unobserved)

        let held = heldPulled fixture
        Assert.Equal<VersionNumber list>([ VersionNumber.first ], held.versions |> List.map _.version)
        Assert.Equal<EntryLifecycle>(InactiveEntry, held.lifecycle)

        let userEntriesBefore = fixture.context.proxy.heldUserEntries ()
        let recordsBefore = fixture.context.proxy.pulledRecordsOf testSourceName
        match fixture.context.pullRecord fixture.proxy siliconHit.record with
        | Error (PullImportErr (ImportRejectedErr reason)) ->
            Assert.Contains(siliconHit.record.value, reason)
            Assert.Contains($"{pulledEntryId.value}", reason)
        | other -> Assert.Fail $"expected PullImportErr (ImportRejectedErr _) for a retired record; got %A{other}"
        Assert.Equal<Result<HeldEntry list, CatalogueImportError>>(userEntriesBefore, fixture.context.proxy.heldUserEntries ())
        Assert.Equal<Result<PulledRecord list, CatalogueImportError>>(recordsBefore, fixture.context.proxy.pulledRecordsOf testSourceName)
        Assert.Equal<EntryLifecycle>(InactiveEntry, (heldPulled fixture).lifecycle)

    [<Fact>]
    let ``every pull error renders its condition and its payload`` () : unit =
        let sourceError = SourceRecordMissingErr "main/Ge/nk/Unlisted.yml"
        let importError = ImportRejectedErr "entry 44400001 is held as a sample"
        let entryError = MissingEntryFieldErr "material 'Si (Silicon), Aspnes' has no category"
        let cases =
            [
                PullSourceErr sourceError, sourceError.describe, "the source refused: the snapshot does not list the record: main/Ge/nk/Unlisted.yml"
                PullImportErr importError, importError.describe, "the library refused the pulled version: a catalogue entry was rejected: entry 44400001 is held as a sample"
                PullEntryErr entryError, entryError.describe, "the pulled material could not be built: an entry field is missing: material 'Si (Silicon), Aspnes' has no category"
            ]

        for error, wrapped, expected in cases do
            Assert.Equal(expected, error.describe)
            Assert.Contains(wrapped, error.describe)
        Assert.Equal(3, cases |> List.map (fun (error, _, _) -> error.describe) |> List.distinct |> List.length)
