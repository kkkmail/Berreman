namespace OpticalConstructor.Tests

open System.Text
open System.Threading
open Xunit
open Softellect.Sys.Primitives
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.MaterialSources

/// The online-source seam's double: snapshot, search, fetch, update and its errors (spec 0050 S13.1).
module MaterialSourceProxyTests =

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

    let private siliconHash : ContentHash =
        "Si Aspnes record text" |> Encoding.UTF8.GetBytes |> FileByteContent |> ContentHash.ofBytes

    let private langasiteHash : ContentHash =
        "La3Ga5SiO14 Test record text" |> Encoding.UTF8.GetBytes |> FileByteContent |> ContentHash.ofBytes

    let private siliconCitation : EntryCitation = EntryCitation "D. E. Aspnes and A. A. Studna, Phys. Rev. B 27, 985 (1983)"

    let private langasiteCitation : EntryCitation = EntryCitation "Test langasite record, 2026"

    let private siliconRecord () : InMemorySourceRecord =
        {
            hit = siliconHit
            recordHash = siliconHash
            citation = siliconCitation
            material = BuiltinCatalogue.material MaterialIds.silicon
        }

    let private langasiteRecord () : InMemorySourceRecord =
        {
            hit = langasiteHit
            recordHash = langasiteHash
            citation = langasiteCitation
            material = BuiltinCatalogue.material MaterialIds.langasite
        }

    let private publishedRecords () : InMemorySourceRecord list = [ siliconRecord (); langasiteRecord () ]

    let private freshSource () : InMemoryMaterialSource = InMemoryMaterialSource.create testSourceName testLicence

    let private progressOf (filesDone : int) (filesTotal : int) : SourceSnapshotProgress =
        {
            filesDone = SourceSnapshotFileCount filesDone
            filesTotal = SourceSnapshotFileCount filesTotal
        }

    /// A proxy over a source holding `version` of both records, installed unobserved.
    let private updatedProxy (source : InMemoryMaterialSource) (version : MaterialSourceVersion) : MaterialSourceProxy =
        let proxy = MaterialSourceProxy.createInMemory source
        source.publish version (publishedRecords ())
        let updated : SourceSnapshotInfo = { version = version; files = SourceSnapshotFileCount 2 }
        Assert.Equal<Result<SourceSnapshotOutcome, MaterialSourceError>>(
            Ok (SourceSnapshotUpdated updated),
            proxy.updateSnapshot SourceSnapshotObserver.unobserved)
        proxy

    [<Fact>]
    let ``a source holds no snapshot until its first update`` () : unit =
        let source = freshSource ()
        let proxy = MaterialSourceProxy.createInMemory source
        source.publish firstVersion (publishedRecords ())

        Assert.Equal<MaterialSourceName>(testSourceName, proxy.sourceName)
        Assert.Equal<Result<MaterialSourceVersion option, MaterialSourceError>>(Ok None, proxy.snapshotVersion ())
        match proxy.searchRecords (MaterialSourceQuery "silicon") with
        | Error (SourceSnapshotMissingErr reason) -> Assert.Contains(testSourceName.value, reason)
        | other -> Assert.Fail $"expected SourceSnapshotMissingErr from the search; got %A{other}"
        match proxy.fetchRecord siliconHit.record with
        | Error (SourceSnapshotMissingErr reason) -> Assert.Contains(testSourceName.value, reason)
        | other -> Assert.Fail $"expected SourceSnapshotMissingErr from the fetch; got %A{other}"

    [<Fact>]
    let ``an update installs the published snapshot, reporting each record once`` () : unit =
        let source = freshSource ()
        let proxy = MaterialSourceProxy.createInMemory source
        source.publish firstVersion (publishedRecords ())
        let progresses = ResizeArray<SourceSnapshotProgress>()
        let observer : SourceSnapshotObserver =
            {
                reportSnapshotProgress = progresses.Add
                cancellation = CancellationToken.None
            }

        let outcome = proxy.updateSnapshot observer

        let expected : SourceSnapshotInfo =
            {
                version = MaterialSourceVersion "2026-11-01"
                files = SourceSnapshotFileCount 2
            }
        Assert.Equal<Result<SourceSnapshotOutcome, MaterialSourceError>>(Ok (SourceSnapshotUpdated expected), outcome)
        Assert.Equal<SourceSnapshotProgress list>([ progressOf 1 2; progressOf 2 2 ], List.ofSeq progresses)
        Assert.Equal<Result<MaterialSourceVersion option, MaterialSourceError>>(Ok (Some firstVersion), proxy.snapshotVersion ())

    [<Fact>]
    let ``a search keeps the hits whose name or record holds every word, ignoring case`` () : unit =
        let proxy = updatedProxy (freshSource ()) firstVersion
        let found (text : string) : Result<MaterialSourceHit list, MaterialSourceError> = proxy.searchRecords (MaterialSourceQuery text)

        Assert.Equal<Result<MaterialSourceHit list, MaterialSourceError>>(Ok [ siliconHit ], found "silicon")
        Assert.Equal<Result<MaterialSourceHit list, MaterialSourceError>>(Ok [ siliconHit ], found "SI ASPNES")
        Assert.Equal<Result<MaterialSourceHit list, MaterialSourceError>>(Ok [ siliconHit; langasiteHit ], found "nk")
        Assert.Equal<Result<MaterialSourceHit list, MaterialSourceError>>(Ok [], found "aspnes langasite")
        Assert.Equal<Result<MaterialSourceHit list, MaterialSourceError>>(Ok [ siliconHit; langasiteHit ], found " \t ")
        Assert.Equal<Result<MaterialSourceHit list, MaterialSourceError>>(Ok [ siliconHit; langasiteHit ], found "")

    [<Fact>]
    let ``a fetched record carries its pull provenance and its material under its hit's name`` () : unit =
        let proxy = updatedProxy (freshSource ()) firstVersion
        let silicon = BuiltinCatalogue.material MaterialIds.silicon
        let pull : SourcePullInfo =
            {
                source = testSourceName
                record = siliconHit.record
                recordHash = siliconHash
                sourceVersion = firstVersion
                licence = testLicence
                citation = siliconCitation
            }
        let expected : SourceMaterialRecord =
            {
                pull = pull
                material = { silicon with name = EntryName "Si (Silicon), Aspnes" }
            }

        Assert.NotEqual<EntryName>(siliconHit.name, silicon.name)
        Assert.Equal<Result<SourceMaterialRecord, MaterialSourceError>>(Ok expected, proxy.fetchRecord siliconHit.record)
        let unlisted = MaterialSourceRecordId "main/Ge/nk/Unlisted.yml"
        match proxy.fetchRecord unlisted with
        | Error (SourceRecordMissingErr reason) -> Assert.Contains(unlisted.value, reason)
        | other -> Assert.Fail $"expected SourceRecordMissingErr for an unlisted record; got %A{other}"

    [<Fact>]
    let ``an update cancelled before its next record keeps the held snapshot`` () : unit =
        let source = freshSource ()
        let proxy = updatedProxy source firstVersion
        source.publish laterVersion (publishedRecords ())
        use cancelSource = new CancellationTokenSource()
        let progresses = ResizeArray<SourceSnapshotProgress>()
        let observer : SourceSnapshotObserver =
            {
                reportSnapshotProgress =
                    fun progress ->
                        progresses.Add progress
                        cancelSource.Cancel()
                cancellation = cancelSource.Token
            }

        Assert.Equal<Result<SourceSnapshotOutcome, MaterialSourceError>>(Ok SourceSnapshotCancelled, proxy.updateSnapshot observer)
        Assert.Equal<SourceSnapshotProgress list>([ progressOf 1 2 ], List.ofSeq progresses)
        Assert.Equal<Result<MaterialSourceVersion option, MaterialSourceError>>(Ok (Some firstVersion), proxy.snapshotVersion ())

    [<Fact>]
    let ``an update with nothing published is SourceDownloadErr`` () : unit =
        let proxy = MaterialSourceProxy.createInMemory (freshSource ())

        match proxy.updateSnapshot SourceSnapshotObserver.unobserved with
        | Error (SourceDownloadErr reason) -> Assert.Contains(testSourceName.value, reason)
        | other -> Assert.Fail $"expected SourceDownloadErr; got %A{other}"
        Assert.Equal<Result<MaterialSourceVersion option, MaterialSourceError>>(Ok None, proxy.snapshotVersion ())

    [<Fact>]
    let ``every material-source error renders its condition and its payload`` () : unit =
        let cases =
            [
                SourceSnapshotMissingErr "Test source was never updated", "the source holds no snapshot: Test source was never updated"
                SourceRecordMissingErr "main/Ge/nk/Unlisted.yml", "the snapshot does not list the record: main/Ge/nk/Unlisted.yml"
                SourceRecordUnreadableErr "main/Si/nk/Aspnes.yml has no data", "the record could not be read: main/Si/nk/Aspnes.yml has no data"
                SourceSnapshotIoErr "D:/Sources/Test was denied", "the snapshot could not be read or written: D:/Sources/Test was denied"
                SourceDownloadErr "the host did not answer", "the source's archive could not be downloaded: the host did not answer"
                SourceArchiveInvalidErr "the zip holds no catalog", "the archive is not the source's database: the zip holds no catalog"
                UnsupportedSnapshotSchemaErr "stamp schema 9", "the snapshot stamp has another schema version: stamp schema 9"
            ]

        for error, expected in cases do
            Assert.Equal(expected, error.describe)
        Assert.Equal(7, cases |> List.map (fun (error, _) -> error.describe) |> List.distinct |> List.length)
