namespace OpticalConstructor.Tests

open System
open System.IO
open Xunit
open Softellect.Sys.Primitives
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.KitFolders
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Storage.KitFolderStore
open OpticalConstructor.Domain.MeasuredData            // series / ExperimentDataError + the pure parsers (step 034)
open OpticalConstructor.Domain.ImportProfiles
open OpticalConstructor.Storage                         // ExperimentDataStore.createFileBacked (step 036 real store)

/// Kit-backed reads through an attachment's import profile and row selection, and picked-file reads and attachment, at
/// the real IO boundary (spec 0038 step 036; spec 0050 S4.2, S9.2).
module ExperimentDataStoreTests =

    let private testDir : FolderName =
        FolderName (Path.Combine(AppContext.BaseDirectory, "ExperimentDataStoreTests"))

    let private newFolder () : KitFolder =
        let folder = FolderName (Path.Combine(testDir.value, Guid.NewGuid().ToString("N")))
        Directory.CreateDirectory folder.value |> ignore
        KitFolder folder

    /// Step 302's `Lab scan`: a scan angle, a signal with its σ and the experiment key in column 4.
    let private labScanProfile : ImportProfile =
        {
            name = ImportProfileName "Lab scan"
            delimiter = CommaDelimiter
            header = HeaderOnFirstRow
            bom = ByteOrderMarkAbsent
            roles =
                {
                    scan =
                        [
                            {
                                column = DataColumnNumber 1
                                valueUnit = DegreeColumnUnit
                                reading = PhysicalScanValue
                            }
                        ]
                    reading = SignalColumns (DataColumnNumber 2, Some (DataColumnNumber 3))
                    timestampOpt = None
                    captureIndexOpt = None
                    observableOpt = None
                    keyOpt = Some (DataColumnNumber 4)
                }
        }

    /// A new folder under the class's test directory, initialised as a kit whose kit file holds `Lab scan` (step 125),
    /// because `readMapped` reads the kit file.
    let private newKit () : KitFolder * KitProxy =
        let folder = newFolder ()
        let kit = KitProxy.create folder
        let created = ClockTime (DateTime(2026, 11, 11, 12, 0, 0, DateTimeKind.Utc))
        let kitFile =
            {
                KitFile.create (KitId (Guid.Parse "30500000-0000-4000-8000-000000000001")) (KitName "Data store") (KitAuthor "test") created with
                    importProfiles = [ labScanProfile ]
            }
        kit.initialiseKit kitFile |> Result.defaultWith (fun error -> failwith error.describe)
        folder, kit

    let private writeData (folder : KitFolder) (kit : KitProxy) (name : FileName) (text : DataFileText) : DataAttachment =
        let dataFolder = FolderName (Path.Combine(folder.value.value, "data"))
        Directory.CreateDirectory dataFolder.value |> ignore
        let file = FileName (Path.Combine(dataFolder.value, name.value))
        File.WriteAllText(file.value, text.value)
        let path = KitRelativePath $"data/{name.value}"
        let stored = kit.readDataFile path |> Result.defaultWith (fun error -> failwith error.describe)
        stored.attachment

    /// The attachment read through `profile` over every row (spec 0050 S9.1).
    let private mappedBy (profile : ImportProfileName) (attachment : DataAttachment) : MappedAttachment =
        {
            attachment = attachment
            profile = profile
            rows = AllDataRows
            exclusions = []
        }

    let private kitPaths (folder : KitFolder) : string[] =
        Directory.GetFileSystemEntries(folder.value.value, "*", SearchOption.AllDirectories) |> Array.sort

    [<Fact>]
    let ``createFileBacked round-trips a known intensity file through the step-34 parser`` () =
        let folder, kit = newKit ()
        let proxy = ExperimentDataStore.createFileBacked kit
        let text = DataFileText "wavelength_nm,intensity\n400.0,0.10\n500.0,0.42\n600.0,0.31"
        let name = FileName "intensity-roundtrip.csv"
        let attachment = writeData folder kit name text

        // The store must reproduce EXACTLY what the pure step-34 parser yields from the same text — no
        // parsing logic of its own.
        let expected =
            match text.parseIntensitySeries with
            | Ok s -> s
            | Error e -> failwith $"the intensity fixture unexpectedly failed to parse: %A{e}"

        match proxy.readMapped (mappedBy ImportProfileName.intensitySeries attachment) with
        | Ok (SeriesPoints (IntensityData series)) -> Assert.Equal<IntensitySeries>(expected, series)
        | other -> Assert.Fail($"expected the parsed intensity series from the real store, got %A{other}")

    [<Fact>]
    let ``createFileBacked round-trips a known ellipsometric file through the step-34 parser`` () =
        let folder, kit = newKit ()
        let proxy = ExperimentDataStore.createFileBacked kit
        let text = DataFileText "wavelength_nm,psi_deg,delta_deg\n400.0,12.0,30.0\n500.0,15.0,45.0"
        let name = FileName "ellipsometric-roundtrip.csv"
        let attachment = writeData folder kit name text

        let expected =
            match text.parseEllipsometricSeries with
            | Ok s -> s
            | Error e -> failwith $"the ellipsometric fixture unexpectedly failed to parse: %A{e}"

        match proxy.readMapped (mappedBy ImportProfileName.ellipsometricSeries attachment) with
        | Ok (SeriesPoints (EllipsometricData series)) -> Assert.Equal<EllipsometricSeries>(expected, series)
        | other -> Assert.Fail($"expected the parsed ellipsometric series from the real store, got %A{other}")

    [<Fact>]
    let ``createFileBacked maps a missing file to a typed error through either X,Y profile, never a throw`` () =
        let _, kit = newKit ()
        let proxy = ExperimentDataStore.createFileBacked kit
        let missing : DataAttachment =
            {
                path = KitRelativePath "data/does-not-exist.csv"
                contentHash = ContentHash "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"
            }

        for profile in [ ImportProfileName.intensitySeries; ImportProfileName.ellipsometricSeries ] do
            match proxy.readMapped (mappedBy profile missing) with
            | Error (MalformedDataFileErr reason) ->
                Assert.False(String.IsNullOrWhiteSpace reason)
                Assert.Contains($"could not read the data file '{missing.path.value}' for the import profile '{profile.value}':", reason)
                Assert.Contains("the kit holds no such file", reason)
            | other -> Assert.Fail($"expected Error (MalformedDataFileErr _) for a missing path through '{profile.value}', got %A{other}")

    [<Fact>]
    let ``createFileBacked delegates parse semantics: an existing but empty file is the parser's EmptyDataFileErr`` () =
        // Spec 0038 step 036; spec 0050 S4.2: an empty kit file must retain the parser's error, not become an IO error.
        let folder, kit = newKit ()
        let proxy = ExperimentDataStore.createFileBacked kit
        let name = FileName "empty-intensity.csv"
        let text = DataFileText ""
        let attachment = writeData folder kit name text

        match proxy.readMapped (mappedBy ImportProfileName.intensitySeries attachment) with
        | Error (EmptyDataFileErr reason) -> Assert.False(String.IsNullOrWhiteSpace reason)
        | other -> Assert.Fail($"expected the parser's Error (EmptyDataFileErr _) for an empty file, got %A{other}")

    [<Fact>]
    let ``createFileBacked reads a data file through the kit's own profile and keeps the rows of its key`` () =
        let folder, kit = newKit ()
        let proxy = ExperimentDataStore.createFileBacked kit
        let text = DataFileText "phi,I,sigma,exp\n0,0.5,0.01,K1\n20,0.25,0.01,K2\n40,0.125,0.01,K1\n"
        let attachment = writeData folder kit (FileName "lab-scan.csv") text
        let k1 = DataRowKey "K1"

        let k1Row (degrees : Units.UnitMagnitude) (signal : MeasuredIntensity) : MappedRow =
            {
                keyOpt = Some k1
                scan = [ { column = List.exactlyOne labScanProfile.roles.scan; magnitude = degrees } ]
                reading = SignalReading (signal, Some (MeasuredSigma 0.01))
                timestampOpt = None
                captureIndexOpt = None
                observableOpt = None
            }

        let keyed : MappedAttachment = { attachment = attachment; profile = labScanProfile.name; rows = KeyedDataRows k1; exclusions = [] }
        let expected =
            ColumnPoints
                [
                    k1Row (Units.UnitMagnitude 0.0) (MeasuredIntensity 0.5)
                    k1Row (Units.UnitMagnitude 40.0) (MeasuredIntensity 0.125)
                ]

        Assert.Equal<Result<MappedPoints, ExperimentDataError>>(Ok expected, proxy.readMapped keyed)

        match proxy.readMapped (mappedBy labScanProfile.name attachment) with
        | Ok points -> Assert.Equal(MeasuredPointCount 3, points.pointCount)
        | Error e -> Assert.Fail($"expected every row of the lab scan, got %A{e}")

    [<Fact>]
    let ``createFileBacked refuses a profile the kit lacks and a key an X,Y file cannot select`` () =
        let folder, kit = newKit ()
        let proxy = ExperimentDataStore.createFileBacked kit
        let text = DataFileText "wavelength_nm,intensity\n400.0,0.10\n500.0,0.42"
        let attachment = writeData folder kit (FileName "intensity.csv") text

        match proxy.readMapped (mappedBy (ImportProfileName "Nope") attachment) with
        | Error (MalformedDataFileErr reason) -> Assert.Contains("the import profile 'Nope' is not available", reason)
        | other -> Assert.Fail($"expected MalformedDataFileErr for a profile the kit lacks, got %A{other}")

        let keyed = { mappedBy ImportProfileName.intensitySeries attachment with rows = KeyedDataRows (DataRowKey "K1") }

        match proxy.readMapped keyed with
        | Error (MalformedDataFileErr reason) -> Assert.Contains("no key column", reason)
        | other -> Assert.Fail($"expected MalformedDataFileErr for a key an X,Y file cannot select, got %A{other}")

    [<Fact>]
    let ``createFileBacked reads a picked file whole without attaching it`` () =
        let folder, kit = newKit ()
        let proxy = ExperimentDataStore.createFileBacked kit
        let outsideFolder = newFolder ()
        let picked = FileName (Path.Combine(outsideFolder.value.value, "picked.csv"))
        // A byte-order mark and a non-UTF-8 byte: the read keeps the bytes exactly as stored.
        let bytes = FileByteContent (Array.append [| 0xEFuy; 0xBBuy; 0xBFuy |] (Array.append (System.Text.Encoding.UTF8.GetBytes "X,Y\n0,1\n") [| 0xFFuy |]))
        File.WriteAllBytes(picked.value, bytes.value)
        let pathsBefore = kitPaths folder

        match proxy.readPickedFile picked with
        | Ok source ->
            Assert.Equal(picked, source.file)
            Assert.Equal<byte>(bytes.value, source.content.value)
        | Error error -> Assert.Fail($"expected the picked file's name and bytes, got {error.describe}")

        Assert.Equal<string>(pathsBefore, kitPaths folder)
        Assert.Equal<byte>(bytes.value, File.ReadAllBytes picked.value)

        match proxy.readPickedFile (FileName (Path.Combine(outsideFolder.value.value, "missing.csv"))) with
        | Error (KitIoErr reason) -> Assert.Contains("missing.csv", reason)
        | other -> Assert.Fail($"expected KitIoErr for a picked file that is not there, got %A{other}")

        Assert.Equal<string>(pathsBefore, kitPaths folder)

    [<Fact>]
    let ``createFileBacked copies a picked file from outside the kit into data and records one inside the kit in place`` () =
        let folder, kit = newKit ()
        let proxy = ExperimentDataStore.createFileBacked kit
        let outsideFolder = newFolder ()
        let picked = FileName (Path.Combine(outsideFolder.value.value, "outside.csv"))
        let bytes = FileByteContent (System.Text.Encoding.UTF8.GetBytes "X,Y\n0,1\n1,0.5")
        File.WriteAllBytes(picked.value, bytes.value)
        let copied = proxy.attachDataFile picked |> Result.defaultWith (fun error -> failwith error.describe)
        let expectedPath = KitRelativePath "data/outside.csv"
        Assert.Equal(expectedPath, copied.path)
        Assert.Equal(ContentHash.ofBytes bytes, copied.contentHash)
        let copiedFile = FileName (Path.Combine(folder.value.value, copied.path.value))
        Assert.Equal<byte>(bytes.value, File.ReadAllBytes copiedFile.value)
        Assert.Equal<byte>(bytes.value, File.ReadAllBytes picked.value)
        Assert.Equal<Result<DataAttachmentStatus, KitError>>(Ok AttachedDataUnchanged, proxy.attachmentStatus copied)

        let name = FileName "inside.csv"
        let text = DataFileText "X,Y\n0,0.75\n1,0.25"
        let existing = writeData folder kit name text
        let inside = FileName (Path.Combine(folder.value.value, existing.path.value))
        let filesBefore = Directory.GetFiles(folder.value.value, "*", SearchOption.AllDirectories) |> Array.sort
        let bytesBefore = FileByteContent (File.ReadAllBytes inside.value)
        let writtenBefore = File.GetLastWriteTimeUtc inside.value
        let readOnlyProxy = ExperimentDataStore.createFileBacked kit.readOnlyView
        for reader in [ proxy; readOnlyProxy ] do
            let attached = reader.attachDataFile inside |> Result.defaultWith (fun error -> failwith error.describe)
            Assert.Equal(existing, attached)
            Assert.Equal<byte>(bytesBefore.value, File.ReadAllBytes inside.value)
            Assert.Equal(writtenBefore, File.GetLastWriteTimeUtc inside.value)
            Assert.Equal<string>(filesBefore, Directory.GetFiles(folder.value.value, "*", SearchOption.AllDirectories) |> Array.sort)
            Assert.Equal<Result<DataAttachmentStatus, KitError>>(Ok AttachedDataUnchanged, reader.attachmentStatus attached)

    [<Fact>]
    let ``createFileBacked answers KitIoErr for a picked file it cannot read and attaches nothing`` () =
        let folder, kit = newKit ()
        let proxy = ExperimentDataStore.createFileBacked kit
        let outsideFolder = newFolder ()
        let missing = FileName (Path.Combine(outsideFolder.value.value, "missing.csv"))
        let filesBefore = Directory.GetFiles(folder.value.value, "*", SearchOption.AllDirectories) |> Array.sort
        let foldersBefore = Directory.GetDirectories(folder.value.value, "*", SearchOption.AllDirectories) |> Array.sort
        match proxy.attachDataFile missing with
        | Error (KitIoErr reason) ->
            Assert.Contains(missing.value, reason)
            Assert.False(String.IsNullOrWhiteSpace reason)
        | other -> Assert.Fail($"expected KitIoErr for an unreadable pick, got %A{other}")
        Assert.Equal<string>(filesBefore, Directory.GetFiles(folder.value.value, "*", SearchOption.AllDirectories) |> Array.sort)
        Assert.Equal<string>(foldersBefore, Directory.GetDirectories(folder.value.value, "*", SearchOption.AllDirectories) |> Array.sort)
