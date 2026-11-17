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

/// refractiveindex.info and PTB OCDB snapshots and GitHub-style archives written under the test output, and the
/// downloader and clock an update runs through (spec 0050 S13.1, S13.2).
module MaterialSourceFixture =

    let private fixtureText (name : string) : string = File.ReadAllText(Path.Combine(AppContext.BaseDirectory, "fixtures", name))

    /// The text of `fixtures/Si.yml`, a tabulated n,k page.
    let siPage () : string = fixtureText "Si.yml"

    /// The text of `fixtures/formula2.yml`, a formula 2 page.
    let glassPage () : string = fixtureText "formula2.yml"

    /// `siPage ()` with the n at 0.700 µm changed from 3.785 to 3.790.
    let changedSiPage () : string = (siPage ()).Replace("0.700 3.785 0.013", "0.700 3.790 0.013")

    /// The text of `fixtures/ptb-Co.txt`, PTB's Co record with its 3-sigma bounds.
    let ptbRecord () : string = fixtureText "ptb-Co.txt"

    /// `ptbRecord ()` with the first row's upper 1 − δ bound changed from 0.96804 to 0.96810.
    let changedPtbRecord () : string = (ptbRecord ()).Replace("0.96804", "0.96810")

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

    /// A three-column Ru record: step 454's header lines without the bound columns or the 3-sigma line, and two rows.
    let private ptbUnboundedRecord : string =
        "# Optical constants for Ru created by PTB\n# n = (1-delta) - (i*beta)\n#lambda/nm\t1-delta\tbeta\n"
        + "# ------------------------\n13.5\t0.88\t0.017\n12.5\t0.91\t0.015\n"

    /// PTB's licence file (a test-local line), `record` as Co and the unbounded Ru_2019 record, each a path and its text.
    let private ptbFiles (record : string) : (string * string) list =
        [
            "ocdb/db/COPYING", "Test fixture: the data files in data are licensed under CC BY 4.0.\n"
            "ocdb/db/data/Co.txt", record
            "ocdb/db/data/Ru_2019.txt", ptbUnboundedRecord
        ]

    /// Writes each file under `snapshot`, then `source`'s stamp of 2026-11-01.
    let private writeStampedSnapshot (snapshot : string) (source : MaterialSourceName) (files : (string * string) list) : unit =
        for path, text in files do
            let file = Path.Combine(snapshot, path)
            Directory.CreateDirectory(Path.GetDirectoryName file) |> ignore
            File.WriteAllBytes(file, Encoding.UTF8.GetBytes text)

        let stamp : SourceSnapshotStamp =
            {
                schemaVersion = SourceSnapshotStamp.currentSchemaVersion
                source = source
                sourceVersion = MaterialSourceVersion "2026-11-01"
            }
        match stamp.fileText with
        | Ok text -> File.WriteAllBytes(Path.Combine(snapshot, SourceSnapshotStamp.fileName.value), Encoding.UTF8.GetBytes text.value)
        | Error error -> Assert.Fail $"the fixture stamp does not render: {error.describe}"

    /// Writes a zip of `entries` under the one folder `top`, a folder's ending in `/` and a file's holding its text in `files`.
    let private writeTopFolderZip (zip : string) (top : string) (entries : string list) (files : (string * string) list) : unit =
        let texts = dict files
        use stream = new FileStream(zip, FileMode.Create, FileAccess.Write, FileShare.None)
        use archive = new ZipArchive(stream, ZipArchiveMode.Create)
        for entry in entries do
            let zipEntry = archive.CreateEntry $"{top}/{entry}"
            match texts.TryGetValue entry with
            | true, text ->
                use content = zipEntry.Open ()
                let bytes = Encoding.UTF8.GetBytes text
                content.Write(bytes, 0, bytes.Length)
            | false, _ -> ()

    /// Writes under `snapshot` the catalog, `si` as the Si page, `glassPage ()` as the BK7 page and the stamp of 2026-11-01.
    let writeSnapshot (snapshot : string) (si : string) : unit =
        writeStampedSnapshot snapshot (MaterialSourceName "refractiveindex.info") (databaseFiles si)

    /// Writes a GitHub-style zip: README.md, tools/x.py, the data folders and `writeSnapshot`'s files but the stamp.
    let writeArchive (zip : string) (si : string) : unit =
        writeTopFolderZip
            zip
            "refractiveindex.info-database-main"
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
            (("README.md", "# refractiveindex.info database\n") :: ("database/tools/x.py", "print('x')\n") :: databaseFiles si)

    /// Writes under `snapshot` PTB's `ocdb/db/COPYING`, `record` as `ocdb/db/data/Co.txt`, the unbounded Ru_2019 record and
    /// the stamp of 2026-11-01.
    let writePtbSnapshot (snapshot : string) (record : string) : unit =
        writeStampedSnapshot snapshot (MaterialSourceName "PTB OCDB") (ptbFiles record)

    /// Writes a GitHub-style zip whose one top folder `ocdb-master/` holds README.rst, literature.bib, Co's metadata and
    /// `writePtbSnapshot`'s files but the stamp.
    let writePtbArchive (zip : string) (record : string) : unit =
        let repositoryFiles =
            [
                "README.rst", "OCDB\n====\n"
                "ocdb/db/literature.bib", "@misc{ocdb, title = {Test fixture}}\n"
                "ocdb/db/metadata/elements/Co.yaml", "symbol: Co\n"
            ]
        writeTopFolderZip
            zip
            "ocdb-master"
            [
                ""
                "README.rst"
                "ocdb/"
                "ocdb/db/"
                "ocdb/db/COPYING"
                "ocdb/db/data/"
                "ocdb/db/data/Co.txt"
                "ocdb/db/data/Ru_2019.txt"
                "ocdb/db/literature.bib"
                "ocdb/db/metadata/"
                "ocdb/db/metadata/elements/"
                "ocdb/db/metadata/elements/Co.yaml"
            ]
            (repositoryFiles @ ptbFiles record)

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
