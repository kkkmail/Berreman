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
open OpticalConstructor.Domain.HenkeCompounds
open OpticalConstructor.Storage.CatalogueImportStore
open OpticalConstructor.Storage.LibraryDatabaseStore
open OpticalConstructor.Storage.MaterialDatabaseStore
open OpticalConstructor.Storage.MaterialImport
open OpticalConstructor.Storage.MaterialSourceStore
open OpticalConstructor.Tests.MaterialSourceFixture

/// refractiveindex.info, PTB OCDB and CXRO Henke pulls and updates end to end: the real adapters over fixture snapshots
/// under the test output, and the real library database in SQLite `:memory:` (spec 0050 S13.1, S13.2).
module MaterialSourcePullSqliteTests =

    let private owner : string = "MaterialSourcePullSqliteTests"

    let private siRecord : MaterialSourceRecordId = MaterialSourceRecordId "main/Si/nk/Test.yml"

    let private siName : EntryName = EntryName "Si (Silicon), Test 2026: n,k 0.25–0.70 µm"

    let private siHit : MaterialSourceHit =
        {
            record = siRecord
            name = siName
        }

    let private coHit : MaterialSourceHit =
        {
            record = MaterialSourceRecordId "Co.txt"
            name = EntryName "Co (PTB OCDB)"
        }

    let private silicaHit : MaterialSourceHit =
        {
            record = MaterialSourceRecordId "SiO2@2.2"
            name = EntryName "SiO2, ρ = 2.2 g/cm³ (CXRO Henke)"
        }

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

    /// The pull PTB's Co record answers under the snapshot `writePtbSnapshot` stamps 2026-11-01.
    let private ptbFirstPull () : SourcePullInfo =
        {
            source = MaterialSourceName "PTB OCDB"
            record = coHit.record
            recordHash = pageHash (ptbRecord ())
            sourceVersion = MaterialSourceVersion "2026-11-01"
            licence = EntryLicence "CC-BY-4.0"
            citation = EntryCitation "PTB Optical Constants Database (OCDB), ocdb/db/data/Co.txt: Optical constants for Co created by PTB"
        }

    /// The pull the changed Co record answers under the snapshot the update installs at 2026-11-04.
    let private ptbChangedPull () : SourcePullInfo =
        {
            ptbFirstPull () with
                recordHash = pageHash (changedPtbRecord ())
                sourceVersion = MaterialSourceVersion "2026-11-04"
        }

    /// The pull `SiO2@2.2` answers under the snapshot `writeHenkeSnapshot` stamps 2026-11-01: hashed over the record's
    /// text, then each element's file in formula order (spec 0050 S13.2).
    let private silicaPull () : SourcePullInfo =
        let hashed = Array.concat [ Encoding.UTF8.GetBytes silicaHit.record.value; henkeFile "si"; henkeFile "o" ]
        {
            source = MaterialSourceName "CXRO Henke"
            record = silicaHit.record
            recordHash = ContentHash.ofBytes (FileByteContent hashed)
            sourceVersion = MaterialSourceVersion "2026-11-01"
            licence = EntryLicence "LicenseRef-CXRO"
            citation =
                EntryCitation
                    "B.L. Henke, E.M. Gullikson and J.C. Davis, Atomic Data and Nuclear Data Tables 54, 181-342 (1993); CXRO atomic scattering factors (henke.lbl.gov/optical_constants): SiO2 at 2.2 g/cm³"
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

    /// The import context, minting `pulledEntryId`, and the material store over `database`, beside `adapter`.
    let private fixtureWith (database : LibraryDatabase) (adapter : MaterialSourceProxy) : PullSqliteFixture =
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

    /// The refractiveindex.info adapter over a fresh snapshot of `siPage ()` whose update downloads an archive of
    /// `archivePage` at 2026-11-04.
    let private fixtureOver (database : LibraryDatabase) (archivePage : string) : PullSqliteFixture =
        let sources = freshSources owner
        let zip = $"{sources.value}.zip"
        writeArchive zip archivePage
        let adapter = MaterialSourceProxy.createRefractiveIndexInfo sources (copyingDownloader zip (ResizeArray ())) (fixedClock updateClockTime)
        writeSnapshot (Path.Combine(sources.value, adapter.sourceName.value)) (siPage ())
        fixtureWith database adapter

    /// The PTB OCDB adapter over a fresh snapshot of `ptbRecord ()` whose update downloads an archive of `archiveRecord`
    /// at 2026-11-04.
    let private ptbFixtureOver (database : LibraryDatabase) (archiveRecord : string) : PullSqliteFixture =
        let sources = freshSources owner
        let zip = $"{sources.value}.zip"
        writePtbArchive zip archiveRecord
        writePtbSnapshot (Path.Combine(sources.value, "ptb-ocdb")) (ptbRecord ())
        fixtureWith database (MaterialSourceProxy.createPtbOcdb sources (copyingDownloader zip (ResizeArray ())) (fixedClock updateClockTime))

    let private resultAt (hit : MaterialSourceHit) (version : VersionNumber) (change : SourcePullChange) : SourcePullResult =
        {
            record = hit.record
            name = hit.name
            version =
                {
                    entryId = pulledEntryId
                    version = version
                }
            change = change
        }

    /// The fixture after its one pull added `hit`'s record at version 1.
    let private pulledIn (hit : MaterialSourceHit) (fixture : PullSqliteFixture) : PullSqliteFixture =
        Assert.Equal<Result<SourcePullResult list, MaterialSourcePullError>>(
            Ok [ resultAt hit VersionNumber.first PullAddedEntry ],
            fixture.context.pullRecord fixture.adapter hit.record)
        fixture

    /// The update's outcome at 2026-11-04: the refractiveindex.info and the PTB archives each keep three files.
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

    /// The table of a physics that is one isotropic tabulated n,k segment.
    let private tabulatedTableOf (physics : MaterialPhysics) : TabulatedNkTable =
        match physics with
        | ComplexityPhysics { eps = EpsWithDispValue (IsotropicDispersive [ { dispersion = TabulatedNK table } ]) } -> table
        | _ -> failwith "the physics is not one tabulated n,k segment"

    /// The n a one-table import holds at its longest wavelength, 0.70 µm on the Si page.
    let private nAtLongestWaveLength (physics : MaterialPhysics) : float = Array.last (tabulatedTableOf physics).n

    /// The table `MaterialImport` reads from a PTB record, its one-sigma uncertainties included.
    let private ptbTable (record : string) : TabulatedNkTable =
        match (PtbOcdbText record).importMaterial with
        | Ok imported -> tabulatedTableOf imported.physics
        | Error error -> failwith $"the PTB record does not import: {error.describe}"

    /// σn of a table's first sample, None for a table without uncertainties.
    let private firstSigmaN (table : TabulatedNkTable) : float option =
        table.uncertaintyOpt |> Option.map (fun uncertainty -> Array.head uncertainty.sigmaN)

    /// The table `CompoundSpec.indexTableWith` computes for `SiO2@2.2` from the fixtures' `si.nff` and `o.nff`.
    let private silicaTable () : TabulatedNkTable =
        let factorsOf (symbol : string) : AtomicScatteringFactors =
            match (HenkeScatteringText (Encoding.UTF8.GetString(henkeFile symbol))).factors with
            | Ok factors -> factors
            | Error error -> failwith $"{symbol}.nff does not read: {error.describe}"
        match CompoundSpec.tryOfRecord silicaHit.record with
        | Ok spec ->
            match spec.indexTableWith (Map.ofList [ ElementSymbol "Si", factorsOf "si"; ElementSymbol "O", factorsOf "o" ]) with
            | Ok table -> table
            | Error error -> failwith $"SiO2@2.2 does not compute: {error.describe}"
        | Error error -> failwith $"SiO2@2.2 does not read: {error.describe}"

    [<Fact>]
    let ``a pulled refractiveindex.info page is a user material at version 1 with its provenance and its pulled-source row`` () : unit =
        use database = BuiltinCatalogue.freshLibraryDatabase ()
        let fixture = fixtureOver database (siPage ())
        Assert.Equal<Result<MaterialSourceHit list, MaterialSourceError>>(
            Ok [ siHit ],
            fixture.adapter.searchRecords (MaterialSourceQuery "silicon"))

        Assert.Equal<Result<SourcePullResult list, MaterialSourcePullError>>(
            Ok [ resultAt siHit VersionNumber.first PullAddedEntry ],
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
        let fixture = fixtureOver database (siPage ()) |> pulledIn siHit

        Assert.Equal<Result<SourceRefresh, MaterialSourcePullError>>(
            refreshedWith [ resultAt siHit VersionNumber.first PullFoundUnchanged ],
            fixture.context.updateSourceSnapshot fixture.adapter SourceSnapshotObserver.unobserved)

        Assert.Equal<Result<MaterialEntry option, MaterialError>>(Ok None, fixture.materials.resolveVersion (materialVersion versionTwo))
        Assert.Equal<VersionNumber list>([ VersionNumber.first ], (heldPulled fixture).versions |> List.map _.version)
        Assert.Equal<PulledSourceRowData list>([ rowOf VersionNumber.first (firstPull ()) ], pulledSourceRows database)

    [<Fact>]
    let ``an update whose page changed mints version 2 and keeps version 1`` () : unit =
        use database = BuiltinCatalogue.freshLibraryDatabase ()
        let fixture = fixtureOver database (changedSiPage ()) |> pulledIn siHit
        let versionOneBefore = fixture.materials.resolveVersion (materialVersion VersionNumber.first)
        let changedPhysics = importedPhysics (changedSiPage ())

        Assert.Equal<Result<SourceRefresh, MaterialSourcePullError>>(
            refreshedWith [ resultAt siHit versionTwo PullMintedVersion ],
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
        let fixture = fixtureOver database (siPage ()) |> pulledIn siHit
        let latestBefore = fixture.context.proxy.tryLatestEnvelope pulledEntryId

        Assert.Equal<Result<SourcePullResult list, MaterialSourcePullError>>(
            Ok [ resultAt siHit VersionNumber.first PullFoundUnchanged ],
            fixture.context.pullRecord fixture.adapter siRecord)

        Assert.Equal<VersionNumber list>([ VersionNumber.first ], (heldPulled fixture).versions |> List.map _.version)
        Assert.Equal<Result<LibraryEntryEnvelope option, CatalogueImportError>>(latestBefore, fixture.context.proxy.tryLatestEnvelope pulledEntryId)
        Assert.Equal<PulledSourceRowData list>([ rowOf VersionNumber.first (firstPull ()) ], pulledSourceRows database)

    [<Fact>]
    let ``a pulled PTB OCDB record keeps its uncertainties in the library and credits PTB`` () : unit =
        use database = BuiltinCatalogue.freshLibraryDatabase ()
        let fixture = ptbFixtureOver database (ptbRecord ())

        Assert.Equal<Result<SourcePullResult list, MaterialSourcePullError>>(
            Ok [ resultAt coHit VersionNumber.first PullAddedEntry ],
            fixture.context.pullRecord fixture.adapter coHit.record)

        match fixture.materials.resolveVersion (materialVersion VersionNumber.first) with
        | Ok (Some material) ->
            Assert.Equal(coHit.name, material.name)
            let table = tabulatedTableOf material.physics
            Assert.Equal<TabulatedNkTable>(ptbTable (ptbRecord ()), table)
            Assert.Equal<float option>(Some ((0.96804 - 0.96772) / 6.0), firstSigmaN table)
        | Ok None -> Assert.Fail "version 1 does not resolve"
        | Error error -> Assert.Fail $"version 1 could not be read: {materialRefusal error}"

        match fixture.context.proxy.tryLatestEnvelope pulledEntryId with
        | Ok (Some envelope) ->
            Assert.Equal<EntryProvenance>(SourcePullProvenance (ptbFirstPull ()), envelope.content.provenance)
            Assert.Equal<EntryProtection>(UserManaged, envelope.content.provenance.protection)
        | Ok None -> Assert.Fail "the pulled entry holds no latest version"
        | Error error -> Assert.Fail $"the pulled entry's latest version could not be read: {error.describe}"

        Assert.Equal<PulledSourceRowData list>([ rowOf VersionNumber.first (ptbFirstPull ()) ], pulledSourceRows database)

        match fixture.context.proxy.attributionLinesOf [ pulledEntryId ] with
        | Ok lines ->
            Assert.Equal<string list>(
                [
                    "Co (PTB OCDB): Licence CC-BY-4.0 — cite: PTB Optical Constants Database (OCDB), ocdb/db/data/Co.txt: Optical constants for Co created by PTB"
                ],
                lines |> List.map _.text)
        | Error error -> Assert.Fail $"the attribution lines could not be read: {error.describe}"

    [<Fact>]
    let ``an update whose PTB OCDB bounds changed mints version 2 and keeps version 1`` () : unit =
        use database = BuiltinCatalogue.freshLibraryDatabase ()
        let fixture = ptbFixtureOver database (changedPtbRecord ()) |> pulledIn coHit
        let versionOneBefore = fixture.materials.resolveVersion (materialVersion VersionNumber.first)

        Assert.Equal<Result<SourceRefresh, MaterialSourcePullError>>(
            refreshedWith [ resultAt coHit versionTwo PullMintedVersion ],
            fixture.context.updateSourceSnapshot fixture.adapter SourceSnapshotObserver.unobserved)

        match fixture.materials.resolveVersion (materialVersion versionTwo) with
        | Ok (Some material) ->
            Assert.Equal(coHit.name, material.name)
            let table = tabulatedTableOf material.physics
            Assert.Equal<TabulatedNkTable>(ptbTable (changedPtbRecord ()), table)
            Assert.Equal<float option>(Some ((0.96810 - 0.96772) / 6.0), firstSigmaN table)
        | Ok None -> Assert.Fail "version 2 does not resolve"
        | Error error -> Assert.Fail $"version 2 could not be read: {materialRefusal error}"

        match fixture.context.proxy.tryHeldEnvelope pulledEntryId versionTwo with
        | Ok (Some envelope) -> Assert.Equal<EntryProvenance>(SourcePullProvenance (ptbChangedPull ()), envelope.content.provenance)
        | Ok None -> Assert.Fail "the update minted no version 2"
        | Error error -> Assert.Fail $"version 2's envelope could not be read: {error.describe}"

        match versionOneBefore with
        | Ok (Some material) ->
            let table = tabulatedTableOf material.physics
            Assert.Equal<TabulatedNkTable>(ptbTable (ptbRecord ()), table)
            Assert.Equal<float option>(Some ((0.96804 - 0.96772) / 6.0), firstSigmaN table)
        | Ok None -> Assert.Fail "version 1 did not resolve before the update"
        | Error error -> Assert.Fail $"version 1 could not be read before the update: {materialRefusal error}"
        Assert.Equal<Result<MaterialEntry option, MaterialError>>(versionOneBefore, fixture.materials.resolveVersion (materialVersion VersionNumber.first))

        Assert.Equal<VersionNumber list>([ VersionNumber.first; versionTwo ], (heldPulled fixture).versions |> List.map _.version)
        Assert.Equal<PulledSourceRowData list>(
            [ rowOf VersionNumber.first (ptbFirstPull ()); rowOf versionTwo (ptbChangedPull ()) ],
            pulledSourceRows database)

    [<Fact>]
    let ``a pulled Henke compound is a user material at version 1 under CXRO's provenance, its licence named`` () : unit =
        use database = BuiltinCatalogue.freshLibraryDatabase ()
        let sources = freshSources owner
        writeHenkeSnapshot (Path.Combine(sources.value, "cxro-henke"))
        let asked = ResizeArray<SourceArchiveUrl>()
        let fixture =
            fixtureWith database (MaterialSourceProxy.createHenkeCompounds sources (servingDownloader (fun _ -> None) asked) (fixedClock updateClockTime))

        Assert.Equal<Result<MaterialSourceHit list, MaterialSourceError>>(
            Ok [ silicaHit ],
            fixture.adapter.searchRecords (MaterialSourceQuery "SiO2 2.2"))

        Assert.Equal<Result<SourcePullResult list, MaterialSourcePullError>>(
            Ok [ resultAt silicaHit VersionNumber.first PullAddedEntry ],
            fixture.context.pullRecord fixture.adapter silicaHit.record)

        match fixture.context.proxy.tryLatestEnvelope pulledEntryId with
        | Ok (Some envelope) ->
            Assert.Equal<VersionNumber>(VersionNumber.first, envelope.content.version)
            Assert.Equal<EntryProvenance>(SourcePullProvenance (silicaPull ()), envelope.content.provenance)
            Assert.Equal<EntryProtection>(UserManaged, envelope.content.provenance.protection)
        | Ok None -> Assert.Fail "the pulled entry holds no latest version"
        | Error error -> Assert.Fail $"the pulled entry's latest version could not be read: {error.describe}"

        match fixture.materials.resolveVersion (materialVersion VersionNumber.first) with
        | Ok (Some material) ->
            Assert.Equal(silicaHit.name, material.name)
            Assert.Equal<TabulatedNkTable>(silicaTable (), tabulatedTableOf material.physics)
        | Ok None -> Assert.Fail "version 1 does not resolve"
        | Error error -> Assert.Fail $"version 1 could not be read: {materialRefusal error}"

        Assert.Equal<VersionNumber list>([ VersionNumber.first ], (heldPulled fixture).versions |> List.map _.version)
        Assert.Equal<PulledSourceRowData list>([ rowOf VersionNumber.first (silicaPull ()) ], pulledSourceRows database)

        // LicenseRef-CXRO is not CC-BY, so the line names the licence alone (spec 0050 S3.3).
        match fixture.context.proxy.attributionLinesOf [ pulledEntryId ] with
        | Ok lines -> Assert.Equal<string list>([ "SiO2, ρ = 2.2 g/cm³ (CXRO Henke): Licence LicenseRef-CXRO" ], lines |> List.map _.text)
        | Error error -> Assert.Fail $"the attribution lines could not be read: {error.describe}"

        Assert.Empty(asked)
