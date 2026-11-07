namespace OpticalConstructor.Tests

open System
open System.IO
open Xunit
open Softellect.Sys.Primitives
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.KitFolders
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Storage.KitFolderStore
open OpticalConstructor.Domain.MeasuredData            // series / ExperimentDataError + the pure parsers (step 034)
open OpticalConstructor.Storage                         // ExperimentDataStore.createFileBacked (step 036 real store)

/// Kit-backed parser delegation and picked-file attachment at the real IO boundary (spec 0038 step 036; spec 0050 S4.2).
module ExperimentDataStoreTests =

    let private testDir : FolderName =
        FolderName (Path.Combine(AppContext.BaseDirectory, "ExperimentDataStoreTests"))

    let private newKitFolder () : KitFolder =
        let folder = FolderName (Path.Combine(testDir.value, Guid.NewGuid().ToString("N")))
        Directory.CreateDirectory folder.value |> ignore
        KitFolder folder

    let private writeData (folder : KitFolder) (kit : KitProxy) (name : FileName) (text : DataFileText) : DataAttachment =
        let dataFolder = FolderName (Path.Combine(folder.value.value, "data"))
        Directory.CreateDirectory dataFolder.value |> ignore
        let file = FileName (Path.Combine(dataFolder.value, name.value))
        File.WriteAllText(file.value, text.value)
        let path = KitRelativePath $"data/{name.value}"
        let stored = kit.readDataFile path |> Result.defaultWith (fun error -> failwith error.describe)
        stored.attachment

    [<Fact>]
    let ``createFileBacked round-trips a known intensity file through the step-34 parser`` () =
        let folder = newKitFolder ()
        let kit = KitProxy.create folder
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

        match proxy.tryLoadIntensity attachment with
        | Ok series -> Assert.Equal<IntensitySeries>(expected, series)
        | Error e -> Assert.Fail($"expected the parsed intensity series from the real store, got %A{e}")

    [<Fact>]
    let ``createFileBacked round-trips a known ellipsometric file through the step-34 parser`` () =
        let folder = newKitFolder ()
        let kit = KitProxy.create folder
        let proxy = ExperimentDataStore.createFileBacked kit
        let text = DataFileText "wavelength_nm,psi_deg,delta_deg\n400.0,12.0,30.0\n500.0,15.0,45.0"
        let name = FileName "ellipsometric-roundtrip.csv"
        let attachment = writeData folder kit name text

        let expected =
            match text.parseEllipsometricSeries with
            | Ok s -> s
            | Error e -> failwith $"the ellipsometric fixture unexpectedly failed to parse: %A{e}"

        match proxy.tryLoadEllipsometric attachment with
        | Ok series -> Assert.Equal<EllipsometricSeries>(expected, series)
        | Error e -> Assert.Fail($"expected the parsed ellipsometric series from the real store, got %A{e}")

    [<Fact>]
    let ``createFileBacked maps a missing file to a typed error from both fields, never a throw`` () =
        let folder = newKitFolder ()
        let kit = KitProxy.create folder
        let proxy = ExperimentDataStore.createFileBacked kit
        let missing : DataAttachment =
            {
                path = KitRelativePath "data/does-not-exist.csv"
                contentHash = ContentHash "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"
            }

        match proxy.tryLoadIntensity missing with
        | Error (MalformedDataFileErr reason) ->
            Assert.False(String.IsNullOrWhiteSpace reason)
            Assert.Contains($"could not read the intensity data file '{missing.path.value}':", reason)
            Assert.Contains("the kit holds no such file", reason)
        | other -> Assert.Fail($"expected Error (MalformedDataFileErr _) for a missing intensity path, got %A{other}")

        match proxy.tryLoadEllipsometric missing with
        | Error (MalformedDataFileErr reason) ->
            Assert.False(String.IsNullOrWhiteSpace reason)
            Assert.Contains($"could not read the ellipsometric data file '{missing.path.value}':", reason)
            Assert.Contains("the kit holds no such file", reason)
        | other -> Assert.Fail($"expected Error (MalformedDataFileErr _) for a missing ellipsometric path, got %A{other}")

    [<Fact>]
    let ``createFileBacked delegates parse semantics: an existing but empty file is the parser's EmptyDataFileErr`` () =
        // Spec 0038 step 036; spec 0050 S4.2: an empty kit file must retain the parser's error, not become an IO error.
        let folder = newKitFolder ()
        let kit = KitProxy.create folder
        let proxy = ExperimentDataStore.createFileBacked kit
        let name = FileName "empty-intensity.csv"
        let text = DataFileText ""
        let attachment = writeData folder kit name text

        match proxy.tryLoadIntensity attachment with
        | Error (EmptyDataFileErr reason) -> Assert.False(String.IsNullOrWhiteSpace reason)
        | other -> Assert.Fail($"expected the parser's Error (EmptyDataFileErr _) for an empty file, got %A{other}")

    [<Fact>]
    let ``createFileBacked copies a picked file from outside the kit into data and records one inside the kit in place`` () =
        let folder = newKitFolder ()
        let kit = KitProxy.create folder
        let proxy = ExperimentDataStore.createFileBacked kit
        let outsideFolder = newKitFolder ()
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
        let folder = newKitFolder ()
        let kit = KitProxy.create folder
        let proxy = ExperimentDataStore.createFileBacked kit
        let outsideFolder = newKitFolder ()
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
