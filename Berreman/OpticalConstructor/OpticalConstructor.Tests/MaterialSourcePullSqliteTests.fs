namespace OpticalConstructor.Tests

open System
open System.IO
open System.Text
open Xunit
open Berreman.Dispersion
open Softellect.Sys.Primitives
open OpticalConstructor.Domain
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.CatalogueImport
open OpticalConstructor.Domain.MaterialSources
open OpticalConstructor.Domain.MaterialSourcePulls
open OpticalConstructor.Storage.CatalogueImportStore
open OpticalConstructor.Storage.LibraryDatabaseStore
open OpticalConstructor.Storage.MaterialDatabaseStore
open OpticalConstructor.Storage.MaterialImport
open OpticalConstructor.Storage.MaterialSourceStore
open OpticalConstructor.Tests.MaterialSourceFixture

/// A refractiveindex.info pull and update end to end: the real adapter over a fixture snapshot under the test output,
/// and the real library database in SQLite `:memory:` (spec 0050 S13.1).
module MaterialSourcePullSqliteTests =

    let private owner : string = "MaterialSourcePullSqliteTests"

    let private siRecord : MaterialSourceRecordId = MaterialSourceRecordId "main/Si/nk/Test.yml"

    let private siName : EntryName = EntryName "Si (Silicon), Test 2026: n,k 0.25–0.70 µm"

    let private updateClockTime : DateTime = DateTime(2026, 11, 4, 12, 0, 0, DateTimeKind.Utc)

    let private pulledMaterialId : MaterialId = Guid.Parse "44800001-0448-4448-8448-000000000448" |> MaterialId

    let private pulledEntryId : LibraryEntryId = pulledMaterialId.toLibraryEntryId

    let private versionTwo : VersionNumber = VersionNumber.first.next

    let private pageHash (page : string) : ContentHash = page |> Encoding.UTF8.GetBytes |> FileByteContent |> ContentHash.ofBytes

    /// The pull the Si page answers under the snapshot `writeSnapshot` stamps 2026-11-01.
    let private firstPull () : SourcePullInfo =
        {
            source = MaterialSourceName "refractiveindex.info"
            record = siRecord
            recordHash = pageHash (siPage ())
            sourceVersion = MaterialSourceVersion "2026-11-01"
            licence = EntryLicence "CC0-1.0"
            citation =
                EntryCitation
                    "refractiveindex.info, main/Si/nk/Test.yml: Test fixture — minimal refractiveindex.info-style tabulated page (NOT real Si data)."
        }

    /// The pull the changed Si page answers under the snapshot the update installs at 2026-11-04.
    let private changedPull () : SourcePullInfo =
        {
            firstPull () with
                recordHash = pageHash (changedSiPage ())
                sourceVersion = MaterialSourceVersion "2026-11-04"
        }

    /// One `PulledSources` row, its columns read back as the values they hold.
    type private PulledSourceRowData =
        {
            version : LibraryVersionRef
            source : MaterialSourceName
            record : MaterialSourceRecordId
            sourceVersion : MaterialSourceVersion
            licence : EntryLicence
            citation : EntryCitation
        }

    type private PullSqliteFixture =
        {
            context : CatalogueImportContext
            materials : MaterialProxy
            adapter : MaterialSourceProxy
        }

    /// The import context and the material store over `database`, beside the adapter over a fresh snapshot of
    /// `siPage ()` whose update downloads an archive of `archivePage` at 2026-11-04.
    let private fixtureOver (database : LibraryDatabase) (archivePage : string) : PullSqliteFixture =
        let sources = freshSources owner
        let zip = $"{sources.value}.zip"
        writeArchive zip archivePage
        let adapter = MaterialSourceProxy.createRefractiveIndexInfo sources (copyingDownloader zip (ResizeArray ())) (fixedClock updateClockTime)
        writeSnapshot (Path.Combine(sources.value, adapter.sourceName.value)) (siPage ())
        let context = CatalogueImportContext.create (CatalogueImportProxy.createInDatabase database)
        {
            context =
                {
                    context with
                        newEntryId = fun () -> pulledEntryId
                }
            materials = MaterialProxy.createInDatabase database
            adapter = adapter
        }

    let private resultAt (version : VersionNumber) (change : SourcePullChange) : SourcePullResult =
        {
            record = siRecord
            name = siName
            version =
                {
                    entryId = pulledEntryId
                    version = version
                }
            change = change
        }

    /// The fixture over `database` after its one pull added the Si page at version 1.
    let private pulledFixture (database : LibraryDatabase) (archivePage : string) : PullSqliteFixture =
        let fixture = fixtureOver database archivePage
        Assert.Equal<Result<SourcePullResult list, MaterialSourcePullError>>(
            Ok [ resultAt VersionNumber.first PullAddedEntry ],
            fixture.context.pullRecord fixture.adapter siRecord)
        fixture

    let private refreshedWith (pulls : SourcePullResult list) : Result<SourceRefresh, MaterialSourcePullError> =
        let info : SourceRefreshInfo =
            {
                snapshot =
                    {
                        version = MaterialSourceVersion "2026-11-04"
                        files = SourceSnapshotFileCount 3
                    }
                pulls = pulls
            }
        Ok (SourceRefreshed info)

    let private materialVersion (version : VersionNumber) : MaterialVersionId =
        {
            materialId = pulledMaterialId
            version = version
        }

    let private heldPulled (fixture : PullSqliteFixture) : HeldEntry =
        match fixture.context.proxy.tryHeldEntry pulledEntryId with
        | Ok (Some held) -> held
        | Ok None -> failwith $"the pulled entry {pulledEntryId.value} is not held"
        | Error error -> failwith $"the pulled entry could not be read: {error.describe}"

    let private rowOf (version : VersionNumber) (pull : SourcePullInfo) : PulledSourceRowData =
        {
            version =
                {
                    entryId = pulledEntryId
                    version = version
                }
            source = pull.source
            record = pull.record
            sourceVersion = pull.sourceVersion
            licence = pull.licence
            citation = pull.citation
        }

    /// Every `PulledSources` row, read through `run`, in version order.
    let private pulledSourceRows (database : LibraryDatabase) : PulledSourceRowData list =
        let read =
            database.run (fun context ->
                context.PulledSources
                |> Seq.map (fun row ->
                    let data : PulledSourceRowData =
                        {
                            version =
                                {
                                    entryId = LibraryEntryId row.EntryId
                                    version = VersionNumber row.Version
                                }
                            source = MaterialSourceName row.Source
                            record = MaterialSourceRecordId row.RecordPath
                            sourceVersion = MaterialSourceVersion row.SourceVersion
                            licence = EntryLicence row.Licence
                            citation = EntryCitation row.Citation
                        }
                    data)
                |> Seq.toList)
        match read with
        | Ok rows -> rows |> List.sortBy (fun row -> row.version.version)
        | Error error -> failwith $"the PulledSources rows could not be read: {error.describe}"

    /// The reason a material store refusal carries.
    let private materialRefusal (error : MaterialError) : string =
        match error with
        | UnknownMaterialIdErr reason
        | DuplicateMaterialIdErr reason
        | MaterialStillReferencedErr reason
        | InvalidMaterialErr reason
        | MaterialVersionPinnedErr reason
        | MaterialStoreUnavailableErr reason -> reason

    /// The physics `MaterialImport` reads from a page.
    let private importedPhysics (page : string) : MaterialPhysics =
        match (RefractiveIndexInfoText page).importMaterial with
        | Ok imported -> imported.physics
        | Error error -> failwith $"the page does not import: {error.describe}"

    /// The n a one-table import holds at its longest wavelength, 0.70 µm on the Si page.
    let private nAtLongestWaveLength (physics : MaterialPhysics) : float =
        match physics with
        | ComplexityPhysics { eps = EpsWithDispValue (IsotropicDispersive [ { dispersion = TabulatedNK table } ]) } -> Array.last table.n
        | _ -> failwith "the physics is not one tabulated n,k segment"

    [<Fact>]
    let ``a pulled refractiveindex.info page is a user material at version 1 with its provenance and its pulled-source row`` () : unit =
        use database = BuiltinCatalogue.freshLibraryDatabase ()
        let fixture = fixtureOver database (siPage ())
        let siHit : MaterialSourceHit =
            {
                record = siRecord
                name = siName
            }
        Assert.Equal<Result<MaterialSourceHit list, MaterialSourceError>>(
            Ok [ siHit ],
            fixture.adapter.searchRecords (MaterialSourceQuery "silicon"))

        Assert.Equal<Result<SourcePullResult list, MaterialSourcePullError>>(
            Ok [ resultAt VersionNumber.first PullAddedEntry ],
            fixture.context.pullRecord fixture.adapter siRecord)

        match fixture.materials.tryGetMaterial pulledMaterialId with
        | Ok (Some material) ->
            Assert.Equal(pulledMaterialId, material.id)
            Assert.Equal(siName, material.name)
            Assert.Equal<MaterialPhysics>(importedPhysics (siPage ()), material.physics)
        | Ok None -> Assert.Fail "the material store does not resolve the pulled material"
        | Error error -> Assert.Fail $"the pulled material could not be read: {materialRefusal error}"

        match fixture.context.proxy.tryLatestEnvelope pulledEntryId with
        | Ok (Some envelope) ->
            Assert.Equal<VersionNumber>(VersionNumber.first, envelope.content.version)
            Assert.Equal<EntryProvenance>(SourcePullProvenance (firstPull ()), envelope.content.provenance)
            Assert.Equal<EntryProtection>(UserManaged, envelope.content.provenance.protection)
        | Ok None -> Assert.Fail "the pulled entry holds no latest version"
        | Error error -> Assert.Fail $"the pulled entry's latest version could not be read: {error.describe}"
        Assert.Equal<VersionNumber list>([ VersionNumber.first ], (heldPulled fixture).versions |> List.map _.version)

        Assert.Equal<PulledSourceRowData list>([ rowOf VersionNumber.first (firstPull ()) ], pulledSourceRows database)

        match fixture.context.proxy.attributionLinesOf [ pulledEntryId ] with
        | Ok lines -> Assert.Equal<string list>([ "Si (Silicon), Test 2026: n,k 0.25–0.70 µm: Licence CC0-1.0" ], lines |> List.map _.text)
        | Error error -> Assert.Fail $"the attribution lines could not be read: {error.describe}"

    [<Fact>]
    let ``an update whose page is unchanged mints no version`` () : unit =
        use database = BuiltinCatalogue.freshLibraryDatabase ()
        let fixture = pulledFixture database (siPage ())

        Assert.Equal<Result<SourceRefresh, MaterialSourcePullError>>(
            refreshedWith [ resultAt VersionNumber.first PullFoundUnchanged ],
            fixture.context.updateSourceSnapshot fixture.adapter SourceSnapshotObserver.unobserved)

        Assert.Equal<Result<MaterialEntry option, MaterialError>>(Ok None, fixture.materials.resolveVersion (materialVersion versionTwo))
        Assert.Equal<VersionNumber list>([ VersionNumber.first ], (heldPulled fixture).versions |> List.map _.version)
        Assert.Equal<PulledSourceRowData list>([ rowOf VersionNumber.first (firstPull ()) ], pulledSourceRows database)

    [<Fact>]
    let ``an update whose page changed mints version 2 and keeps version 1`` () : unit =
        use database = BuiltinCatalogue.freshLibraryDatabase ()
        let fixture = pulledFixture database (changedSiPage ())
        let versionOneBefore = fixture.materials.resolveVersion (materialVersion VersionNumber.first)
        let changedPhysics = importedPhysics (changedSiPage ())

        Assert.Equal<Result<SourceRefresh, MaterialSourcePullError>>(
            refreshedWith [ resultAt versionTwo PullMintedVersion ],
            fixture.context.updateSourceSnapshot fixture.adapter SourceSnapshotObserver.unobserved)

        let versionTwoNow = fixture.materials.resolveVersion (materialVersion versionTwo)
        match versionTwoNow with
        | Ok (Some material) ->
            Assert.Equal(siName, material.name)
            Assert.Equal<MaterialPhysics>(changedPhysics, material.physics)
            Assert.Equal(3.790, nAtLongestWaveLength material.physics)
        | Ok None -> Assert.Fail "version 2 does not resolve"
        | Error error -> Assert.Fail $"version 2 could not be read: {materialRefusal error}"

        match fixture.context.proxy.tryHeldEnvelope pulledEntryId versionTwo with
        | Ok (Some envelope) -> Assert.Equal<EntryProvenance>(SourcePullProvenance (changedPull ()), envelope.content.provenance)
        | Ok None -> Assert.Fail "the update minted no version 2"
        | Error error -> Assert.Fail $"version 2's envelope could not be read: {error.describe}"

        match versionOneBefore with
        | Ok (Some material) ->
            Assert.Equal(siName, material.name)
            Assert.Equal<MaterialPhysics>(importedPhysics (siPage ()), material.physics)
            Assert.Equal(3.785, nAtLongestWaveLength material.physics)
        | Ok None -> Assert.Fail "version 1 did not resolve before the update"
        | Error error -> Assert.Fail $"version 1 could not be read before the update: {materialRefusal error}"
        Assert.Equal<Result<MaterialEntry option, MaterialError>>(versionOneBefore, fixture.materials.resolveVersion (materialVersion VersionNumber.first))

        let held = heldPulled fixture
        Assert.Equal<VersionNumber>(versionTwo, held.latest)
        Assert.Equal<VersionNumber list>([ VersionNumber.first; versionTwo ], held.versions |> List.map _.version)
        Assert.Equal<Result<MaterialEntry option, MaterialError>>(versionTwoNow, fixture.materials.tryGetMaterial pulledMaterialId)

        Assert.Equal<PulledSourceRowData list>(
            [ rowOf VersionNumber.first (firstPull ()); rowOf versionTwo (changedPull ()) ],
            pulledSourceRows database)

    [<Fact>]
    let ``pulling the same page twice writes it once`` () : unit =
        use database = BuiltinCatalogue.freshLibraryDatabase ()
        let fixture = pulledFixture database (siPage ())
        let latestBefore = fixture.context.proxy.tryLatestEnvelope pulledEntryId

        Assert.Equal<Result<SourcePullResult list, MaterialSourcePullError>>(
            Ok [ resultAt VersionNumber.first PullFoundUnchanged ],
            fixture.context.pullRecord fixture.adapter siRecord)

        Assert.Equal<VersionNumber list>([ VersionNumber.first ], (heldPulled fixture).versions |> List.map _.version)
        Assert.Equal<Result<LibraryEntryEnvelope option, CatalogueImportError>>(latestBefore, fixture.context.proxy.tryLatestEnvelope pulledEntryId)
        Assert.Equal<PulledSourceRowData list>([ rowOf VersionNumber.first (firstPull ()) ], pulledSourceRows database)
