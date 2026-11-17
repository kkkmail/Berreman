namespace OpticalConstructor.Tests

open System
open System.IO
open System.IO.Compression
open System.Text
open Xunit
open Softellect.Sys.Primitives
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Storage.MaterialSourceStore

/// refractiveindex.info snapshots and GitHub-style archives written under the test output, and the downloader and
/// clock an update runs through (spec 0050 S13.1).
module MaterialSourceFixture =

    let private fixtureText (name : string) : string = File.ReadAllText(Path.Combine(AppContext.BaseDirectory, "fixtures", name))

    /// The text of `fixtures/Si.yml`, a tabulated n,k page.
    let siPage () : string = fixtureText "Si.yml"

    /// The text of `fixtures/formula2.yml`, a formula 2 page.
    let glassPage () : string = fixtureText "formula2.yml"

    /// `siPage ()` with the n at 0.700 µm changed from 3.785 to 3.790.
    let changedSiPage () : string = (siPage ()).Replace("0.700 3.785 0.013", "0.700 3.790 0.013")

    /// A new, empty GUID-named folder under `<test output>/<owner>`, never under `%TEMP%`.
    let freshSources (owner : string) : FolderName =
        let folder = Path.Combine(AppContext.BaseDirectory, owner, Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory folder |> ignore
        FolderName folder

    /// The database's three files, catalog first, each a path under the snapshot and its text.
    let private databaseFiles (si : string) : (string * string) list =
        [
            "database/catalog-nk.yml", fixtureText "catalog-nk.yml"
            "database/data/main/Si/nk/Test.yml", si
            "database/data/glass/test/BK7/nk/Test.yml", glassPage ()
        ]

    /// Writes under `snapshot` the catalog, `si` as the Si page, `glassPage ()` as the BK7 page and the stamp of 2026-11-01.
    let writeSnapshot (snapshot : string) (si : string) : unit =
        for path, text in databaseFiles si do
            let file = Path.Combine(snapshot, path)
            Directory.CreateDirectory(Path.GetDirectoryName file) |> ignore
            File.WriteAllBytes(file, Encoding.UTF8.GetBytes text)

        let stamp : SourceSnapshotStamp =
            {
                schemaVersion = SourceSnapshotStamp.currentSchemaVersion
                source = MaterialSourceName "refractiveindex.info"
                sourceVersion = MaterialSourceVersion "2026-11-01"
            }
        match stamp.fileText with
        | Ok text -> File.WriteAllBytes(Path.Combine(snapshot, SourceSnapshotStamp.fileName.value), Encoding.UTF8.GetBytes text.value)
        | Error error -> Assert.Fail $"the fixture stamp does not render: {error.describe}"

    /// Writes a GitHub-style zip: README.md, tools/x.py, the data folders and `writeSnapshot`'s files but the stamp.
    let writeArchive (zip : string) (si : string) : unit =
        let top = "refractiveindex.info-database-main"
        let files = dict (("README.md", "# refractiveindex.info database\n") :: ("database/tools/x.py", "print('x')\n") :: databaseFiles si)
        let entries =
            [
                ""
                "README.md"
                "database/"
                "database/catalog-nk.yml"
                "database/data/"
                "database/data/glass/"
                "database/data/glass/test/"
                "database/data/glass/test/BK7/"
                "database/data/glass/test/BK7/nk/"
                "database/data/glass/test/BK7/nk/Test.yml"
                "database/data/main/"
                "database/data/main/Si/"
                "database/data/main/Si/nk/"
                "database/data/main/Si/nk/Test.yml"
                "database/tools/"
                "database/tools/x.py"
            ]
        use stream = new FileStream(zip, FileMode.Create, FileAccess.Write, FileShare.None)
        use archive = new ZipArchive(stream, ZipArchiveMode.Create)
        for entry in entries do
            let zipEntry = archive.CreateEntry $"{top}/{entry}"
            match files.TryGetValue entry with
            | true, text ->
                use content = zipEntry.Open ()
                let bytes = Encoding.UTF8.GetBytes text
                content.Write(bytes, 0, bytes.Length)
            | false, _ -> ()

    /// Records each URL asked and copies `zip` into the file the update downloads into.
    let copyingDownloader (zip : string) (asked : ResizeArray<SourceArchiveUrl>) : SourceArchiveDownloader =
        {
            downloadArchive =
                fun url file _ ->
                    asked.Add url
                    File.Copy(zip, file.value, true)
                    Ok ()
        }

    /// Answers that UTC instant every time.
    let fixedClock (instant : DateTime) : ClockProvider = { now = fun () -> ClockTime instant }
