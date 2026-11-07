namespace OpticalConstructor.Tests

open System
open System.IO
open System.Text
open System.Text.Json.Nodes
open Xunit
open Softellect.Sys.Primitives
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.KitFolders
open OpticalConstructor.Storage.KitFolderStore

/// One folder viewed through fresh proxies and an outside writer (spec 0050 S4.1).
type KitTestFolder =
    {
        openProxy : unit -> KitProxy
        placeLabFile : string -> string -> unit
        paths : unit -> string list
    }

module private KitProxyTestData =

    /// Starts at the fixed UTC instant and advances one second per call (spec 0050 S4.1).
    let steppingClock () : ClockProvider =
        let mutable next = ClockTime (DateTime(2026, 11, 3, 12, 0, 0, DateTimeKind.Utc))
        {
            now =
                fun () ->
                    let current = next
                    next <- ClockTime (current.value.AddSeconds 1.0)
                    current
        }

    /// Creates the contract's kit metadata at the supplied clock's next instant (spec 0050 S4.1).
    let newKitFile (clock : ClockProvider) : KitFile =
        let id = KitId (Guid.Parse "6f1c2a9e-3b7d-4e25-9a41-0c8d5e7f2b16")
        let name = KitName "Optics: QZ/LR"
        let author = KitAuthor "Test author"
        KitFile.create id name author (clock.now ())

    let freshKitFolderPath () : FolderName =
        FolderName (Path.Combine(AppContext.BaseDirectory, "KitProxyFolderTests", Guid.NewGuid().ToString("N")))

    let kitFolderAdapter (folder : FolderName) : KitTestFolder =
        let kitFolder = KitFolder folder
        {
            openProxy = fun () -> KitProxy.create kitFolder
            placeLabFile =
                fun path text ->
                    let file = FileName (Path.Combine(folder.value, path))
                    Directory.CreateDirectory(Path.GetDirectoryName file.value) |> ignore
                    File.WriteAllText(file.value, text, UTF8Encoding false)
            paths =
                fun () ->
                    Directory.GetFileSystemEntries(folder.value, "*", SearchOption.AllDirectories)
                    |> Array.map (fun path -> Path.GetRelativePath(folder.value, path).Replace('\\', '/'))
                    |> Array.sortWith (fun first second -> String.CompareOrdinal(first, second))
                    |> Array.toList
        }

open KitProxyTestData

/// The kit-file facts shared by in-memory and filesystem folders (spec 0050 S4.1).
[<AbstractClass>]
type KitProxyContract () =

    /// Returns an existing folder that holds no files or subfolders (spec 0050 S4.1).
    abstract freshFolder : unit -> KitTestFolder

    /// Reading or replacing metadata cannot turn an empty folder into a kit (spec 0050 S4.1).
    [<Fact>]
    member this.``a folder holds no kit until it is initialised`` () : unit =
        let folder = this.freshFolder ()
        let proxy = folder.openProxy ()
        let clock = steppingClock ()
        let kit = newKitFile clock

        match proxy.readKitFile () with
        | Error (NotAKitFolderErr reason) -> Assert.False(String.IsNullOrWhiteSpace reason)
        | Error error -> Assert.Fail($"Expected NotAKitFolderErr, got {error.describe}")
        | Ok _ -> Assert.Fail("An empty folder must not hold kit metadata.")

        match proxy.writeKitFile kit with
        | Error (NotAKitFolderErr reason) -> Assert.False(String.IsNullOrWhiteSpace reason)
        | Error error -> Assert.Fail($"Expected NotAKitFolderErr, got {error.describe}")
        | Ok () -> Assert.Fail("Replacing metadata must not initialise a kit.")

        match (folder.openProxy ()).readKitFile () with
        | Error (NotAKitFolderErr reason) -> Assert.False(String.IsNullOrWhiteSpace reason)
        | Error error -> Assert.Fail($"Expected NotAKitFolderErr after the refused write, got {error.describe}")
        | Ok _ -> Assert.Fail("The refused write must leave the folder without a kit.")

        Assert.Empty(folder.paths ())

    /// Initialisation adds only the kit metadata and its two content folders (spec 0050 S4.1).
    [<Fact>]
    member this.``initialising an empty folder writes the kit file, data and library, and nothing else`` () : unit =
        let folder = this.freshFolder ()
        let proxy = folder.openProxy ()
        let otherProxy = folder.openProxy ()
        let clock = steppingClock ()
        let kit = newKitFile clock

        Assert.NotSame(proxy, otherProxy)
        Assert.Equal<Result<unit, KitError>>(Ok (), proxy.initialiseKit kit)
        Assert.Equal<string list>([ ".kit-json"; "data"; "library" ], folder.paths ())
        Assert.Equal<Result<KitFile, KitError>>(Ok kit, proxy.readKitFile ())
        Assert.Equal<Result<KitFile, KitError>>(Ok kit, otherProxy.readKitFile ())

    /// A lab folder retains its existing files and parent folders when made a kit (spec 0050 S4.1).
    [<Fact>]
    member this.``initialising a folder that holds files keeps every one of them`` () : unit =
        let folder = this.freshFolder ()
        folder.placeLabFile "notes.txt" "Lab notes"
        folder.placeLabFile "raw/run 1.csv" "wavelength,intensity\n600,1\n"
        Assert.Equal<string list>([ "notes.txt"; "raw"; "raw/run 1.csv" ], folder.paths ())
        let proxy = folder.openProxy ()
        let clock = steppingClock ()
        let kit = newKitFile clock

        Assert.Equal<Result<unit, KitError>>(Ok (), proxy.initialiseKit kit)
        Assert.Equal<string list>([ ".kit-json"; "data"; "library"; "notes.txt"; "raw"; "raw/run 1.csv" ], folder.paths ())
        Assert.Equal<Result<KitFile, KitError>>(Ok kit, (folder.openProxy ()).readKitFile ())

    /// Another proxy cannot initialise an existing kit or change its contents (spec 0050 S4.1).
    [<Fact>]
    member this.``a folder that already holds a kit is not initialised again`` () : unit =
        let folder = this.freshFolder ()
        let proxy = folder.openProxy ()
        let otherProxy = folder.openProxy ()
        let clock = steppingClock ()
        let first = newKitFile clock
        let second = newKitFile clock
        Assert.NotEqual<KitFile>(first, second)
        Assert.Equal<Result<unit, KitError>>(Ok (), proxy.initialiseKit first)
        folder.placeLabFile "notes.txt" "Keep these notes"
        let before = folder.paths ()

        match otherProxy.initialiseKit second with
        | Error (KitAlreadyExistsErr reason) -> Assert.False(String.IsNullOrWhiteSpace reason)
        | Error error -> Assert.Fail($"Expected KitAlreadyExistsErr, got {error.describe}")
        | Ok () -> Assert.Fail("An existing kit must not be initialised again.")

        Assert.Equal<Result<KitFile, KitError>>(Ok first, proxy.readKitFile ())
        Assert.Equal<Result<KitFile, KitError>>(Ok first, otherProxy.readKitFile ())
        Assert.Equal<string list>(before, folder.paths ())

    /// Whole-file replacement is visible to both existing and newly opened proxies (spec 0050 S4.1).
    [<Fact>]
    member this.``the kit file is replaced in place and read back equal`` () : unit =
        let folder = this.freshFolder ()
        let proxy = folder.openProxy ()
        let otherProxy = folder.openProxy ()
        let clock = steppingClock ()
        let kit = newKitFile clock
        Assert.Equal<Result<unit, KitError>>(Ok (), proxy.initialiseKit kit)
        let before = folder.paths ()
        let reviewed =
            {
                kit with
                    notes = KitNotes "Reviewed"
                    sendExclude = [ SendExcludeGlob "**/*.dng" ]
            }

        Assert.Equal<Result<unit, KitError>>(Ok (), proxy.writeKitFile reviewed)
        Assert.Equal<Result<KitFile, KitError>>(Ok reviewed, otherProxy.readKitFile ())
        Assert.Equal<Result<KitFile, KitError>>(Ok reviewed, (folder.openProxy ()).readKitFile ())
        Assert.Equal<string list>(before, folder.paths ())

        Assert.Equal<Result<unit, KitError>>(Ok (), otherProxy.writeKitFile kit)
        Assert.Equal<Result<KitFile, KitError>>(Ok kit, proxy.readKitFile ())
        Assert.Equal<string list>(before, folder.paths ())

    [<Fact>]
    member this.``kit files of every kind are listed, read, replaced and deleted by their names`` () : unit =
        let folder = this.freshFolder ()
        let proxy = folder.openProxy ()
        let otherProxy = folder.openProxy ()
        let kit = newKitFile (steppingClock ())
        let firstStem = FileStem.ofText "QZ-LR"
        let secondStem = FileStem.ofText "CPL-(QZ#45-LR#45)-CPL"
        let firstTexts =
            KitFileKind.all
            |> List.map (fun kind -> kind, KitFileText $"First {kind.extension.value}: μ, λ\r\n")
            |> Map.ofList
        let secondTexts =
            KitFileKind.all
            |> List.map (fun kind -> kind, KitFileText $"Second {kind.extension.value}: 測定\n")
            |> Map.ofList
        Assert.Equal<Result<unit, KitError>>(Ok (), proxy.initialiseKit kit)

        for kind in KitFileKind.all do
            Assert.Equal<Result<FileStem list, KitError>>(Ok [], proxy.listKindFiles kind)
            Assert.Equal<Result<unit, KitError>>(Ok (), proxy.writeKindFile kind firstStem (firstTexts[kind]))
            Assert.Equal<Result<unit, KitError>>(Ok (), proxy.writeKindFile kind secondStem (secondTexts[kind]))

        let expectedPaths =
            [ ".kit-json"; "data"; "library" ]
            @ [ for kind in KitFileKind.all do
                    (kind.fileNameOf firstStem).value
                    (kind.fileNameOf secondStem).value ]
            |> List.sortWith (fun first second -> String.CompareOrdinal(first, second))
        Assert.Equal<string list>(expectedPaths, folder.paths ())

        for kind in KitFileKind.all do
            let replacement = KitFileText $"Replaced {kind.extension.value}\n"
            Assert.Equal<Result<FileStem list, KitError>>(Ok [ secondStem; firstStem ], otherProxy.listKindFiles kind)
            Assert.Equal<Result<KitFileText, KitError>>(Ok (firstTexts[kind]), otherProxy.readKindFile kind firstStem)
            Assert.Equal<Result<KitFileText, KitError>>(Ok (secondTexts[kind]), otherProxy.readKindFile kind secondStem)
            Assert.Equal<Result<unit, KitError>>(Ok (), proxy.writeKindFile kind firstStem replacement)
            Assert.Equal<Result<KitFileText, KitError>>(Ok replacement, otherProxy.readKindFile kind firstStem)
            Assert.Equal<Result<unit, KitError>>(Ok (), otherProxy.deleteKindFile kind secondStem)
            Assert.Equal<Result<FileStem list, KitError>>(Ok [ firstStem ], (folder.openProxy ()).listKindFiles kind)
            Assert.Contains((kind.fileNameOf firstStem).value, folder.paths ())
            Assert.DoesNotContain((kind.fileNameOf secondStem).value, folder.paths ())

            folder.placeLabFile $"ignored{kind.extension.value}.tmp" "Temporary text"
            folder.placeLabFile $"data/hidden{kind.extension.value}" "Nested text"
            let upperStem = FileStem.ofText "Z-lab"
            let lowerStem = FileStem.ofText "a-lab"
            let unicodeStem = FileStem.ofText "Å-lab"
            let labText = KitFileText $"Lab {kind.extension.value}: μ\n"
            for stem in [ lowerStem; unicodeStem; upperStem ] do
                folder.placeLabFile $"{stem.value}{kind.extension.value.ToUpperInvariant()}" $"\uFEFF{labText.value}"

            Assert.Equal<Result<FileStem list, KitError>>(
                Ok [ firstStem; upperStem; lowerStem; unicodeStem ], proxy.listKindFiles kind)
            for stem in [ upperStem; lowerStem; unicodeStem ] do
                Assert.Equal<Result<KitFileText, KitError>>(Ok labText, proxy.readKindFile kind stem)
                Assert.Equal<Result<unit, KitError>>(Ok (), otherProxy.writeKindFile kind stem replacement)
                Assert.Equal<Result<KitFileText, KitError>>(Ok replacement, proxy.readKindFile kind stem)
                folder.placeLabFile (kind.fileNameOf stem).value labText.value
                Assert.Equal<Result<KitFileText, KitError>>(Ok labText, otherProxy.readKindFile kind stem)
                Assert.Equal<Result<unit, KitError>>(Ok (), proxy.deleteKindFile kind stem)

            Assert.Equal<Result<FileStem list, KitError>>(Ok [ firstStem ], otherProxy.listKindFiles kind)
            Assert.Contains($"ignored{kind.extension.value}.tmp", folder.paths ())
            Assert.Contains($"data/hidden{kind.extension.value}", folder.paths ())

        Assert.Equal<Result<KitFile, KitError>>(Ok kit, proxy.readKitFile ())

    [<Fact>]
    member this.``reading or deleting a missing kit file is MissingKitFileErr and changes nothing`` () : unit =
        let folder = this.freshFolder ()
        let proxy = folder.openProxy ()
        let kit = newKitFile (steppingClock ())
        let stem = FileStem.ofText "Never written"
        let keptStem = FileStem.ofText "QZ-LR"
        let keptText = KitFileText "Keep this text"
        Assert.Equal<Result<unit, KitError>>(Ok (), proxy.initialiseKit kit)
        for kind in KitFileKind.all do
            Assert.Equal<Result<unit, KitError>>(Ok (), proxy.writeKindFile kind keptStem keptText)
        let before = folder.paths ()

        for kind in KitFileKind.all do
            let file = kind.fileNameOf stem
            match proxy.readKindFile kind stem with
            | Error (MissingKitFileErr reason) -> Assert.Contains(file.value, reason)
            | Error error -> Assert.Fail($"Expected MissingKitFileErr, got {error.describe}")
            | Ok _ -> Assert.Fail("Reading a missing kit file must fail.")
            Assert.Equal<string list>(before, folder.paths ())

            match proxy.deleteKindFile kind stem with
            | Error (MissingKitFileErr reason) -> Assert.Contains(file.value, reason)
            | Error error -> Assert.Fail($"Expected MissingKitFileErr, got {error.describe}")
            | Ok () -> Assert.Fail("Deleting a missing kit file must fail.")
            Assert.Equal<string list>(before, folder.paths ())
            Assert.Equal<Result<KitFileText, KitError>>(Ok keptText, proxy.readKindFile kind keptStem)

        Assert.Equal<Result<KitFile, KitError>>(Ok kit, proxy.readKitFile ())

    [<Fact>]
    member this.``replacing a kit file leaves only its new text and no temporary file`` () : unit =
        let folder = this.freshFolder ()
        let proxy = folder.openProxy ()
        let kit = newKitFile (steppingClock ())
        let stem = FileStem.ofText "QZ-LR"
        let original = KitFileText "Long original setup text that must be completely replaced\n"
        let replacement = KitFileText "μ\n"
        Assert.Equal<Result<unit, KitError>>(Ok (), proxy.initialiseKit kit)
        Assert.Equal<Result<unit, KitError>>(Ok (), proxy.writeKindFile SetupKitFile stem original)
        Assert.Equal<Result<unit, KitError>>(Ok (), proxy.writeKindFile SetupKitFile stem replacement)
        Assert.Equal<string list>([ ".kit-json"; "QZ-LR.setup-json"; "data"; "library" ], folder.paths ())
        Assert.False(folder.paths () |> List.exists (fun path -> path.EndsWith(".tmp", StringComparison.OrdinalIgnoreCase)))
        Assert.Equal<Result<KitFileText, KitError>>(Ok replacement, (folder.openProxy ()).readKindFile SetupKitFile stem)

/// Runs the shared kit-file contract over one in-memory folder per fact (spec 0050 S4.1).
type KitProxyTests () =
    inherit KitProxyContract ()

    override _.freshFolder () : KitTestFolder =
        let folder = InMemoryKitFolder.create ()
        {
            openProxy = fun () -> KitProxy.createInMemory folder
            placeLabFile =
                fun path text ->
                    let fileText = KitFileText text
                    KitRelativePath.tryCreate path
                    |> Result.bind (fun relativePath -> folder.placeLabFile relativePath fileText)
                    |> Result.defaultWith (fun error -> failwith error.describe)
            paths = fun () -> folder.paths () |> List.map (fun path -> path.value)
        }

/// Runs the shared kit-file contract over real folders under the test output (spec 0050 S4.1).
type KitProxyFolderTests () =
    inherit KitProxyContract ()

    override _.freshFolder () : KitTestFolder =
        let folder = freshKitFolderPath ()
        Directory.CreateDirectory folder.value |> ignore
        kitFolderAdapter folder

    [<Fact>]
    member _.``a folder that does not exist is not created by its proxy, and initialising it creates it`` () : unit =
        let path = freshKitFolderPath ()
        let folder = kitFolderAdapter path
        Assert.False(Directory.Exists path.value)
        let proxy = folder.openProxy ()
        Assert.False(Directory.Exists path.value)
        let kit = newKitFile (steppingClock ())

        match proxy.readKitFile () with
        | Error (NotAKitFolderErr reason) -> Assert.Contains(path.value, reason)
        | Error error -> Assert.Fail($"Expected NotAKitFolderErr, got {error.describe}")
        | Ok _ -> Assert.Fail("Reading a missing folder must not create a kit.")

        Assert.False(Directory.Exists path.value)

        match proxy.writeKitFile kit with
        | Error (NotAKitFolderErr reason) -> Assert.Contains(path.value, reason)
        | Error error -> Assert.Fail($"Expected NotAKitFolderErr, got {error.describe}")
        | Ok () -> Assert.Fail("Replacing metadata must not create the missing folder.")

        Assert.False(Directory.Exists path.value)
        Assert.Equal<Result<unit, KitError>>(Ok (), proxy.initialiseKit kit)
        Assert.True(Directory.Exists path.value)
        Assert.Equal<string list>([ ".kit-json"; "data"; "library" ], folder.paths ())
        Assert.Equal<Result<KitFile, KitError>>(Ok kit, (folder.openProxy ()).readKitFile ())

    [<Fact>]
    member _.``a kit file of another schema version or of broken text is refused, naming the kit file`` () : unit =
        let path = freshKitFolderPath ()
        let folder = kitFolderAdapter path
        let proxy = folder.openProxy ()
        let kit = newKitFile (steppingClock ())
        Assert.Equal<Result<unit, KitError>>(Ok (), proxy.initialiseKit kit)
        let file = KitFile.fileName.combine path
        let originalBytes = File.ReadAllBytes file.value
        let originalText = Encoding.UTF8.GetString originalBytes
        Assert.False(originalText.StartsWith('\uFEFF'))

        File.WriteAllText(file.value, originalText, UTF8Encoding true)
        let withBom = File.ReadAllBytes file.value
        Assert.True((Encoding.UTF8.GetString withBom).StartsWith('\uFEFF'))
        Assert.Equal<Result<KitFile, KitError>>(Ok kit, proxy.readKitFile ())
        Assert.Equal<byte>(withBom, File.ReadAllBytes file.value)

        let unsupported = JsonNode.Parse originalText
        unsupported["schemaVersion"] <- JsonValue.Create 2
        let unsupportedText = unsupported.ToJsonString()
        folder.placeLabFile KitFile.fileName.value unsupportedText

        match proxy.readKitFile () with
        | Error (KitFileRejectedErr reason) ->
            Assert.Contains(KitFile.fileName.value, reason)
            Assert.Contains(path.value, reason)
            Assert.Contains("schema version 2", reason)
            Assert.Contains("reads version 1", reason)
        | Error error -> Assert.Fail($"Expected KitFileRejectedErr, got {error.describe}")
        | Ok _ -> Assert.Fail("An unsupported kit schema version must be refused.")

        Assert.Equal<byte>(Encoding.UTF8.GetBytes unsupportedText, File.ReadAllBytes file.value)
        folder.placeLabFile KitFile.fileName.value "{"

        match proxy.readKitFile () with
        | Error (KitFileRejectedErr reason) ->
            Assert.Contains(KitFile.fileName.value, reason)
            Assert.Contains(path.value, reason)
        | Error error -> Assert.Fail($"Expected KitFileRejectedErr, got {error.describe}")
        | Ok _ -> Assert.Fail("Malformed kit JSON must be refused.")

        Assert.Equal<byte>(Encoding.UTF8.GetBytes "{", File.ReadAllBytes file.value)
        Assert.Equal<string list>([ ".kit-json"; "data"; "library" ], folder.paths ())

    [<Fact>]
    member _.``a replace that fails keeps the old file whole, removes its temporary file and is KitIoErr`` () : unit =
        let path = freshKitFolderPath ()
        let folder = kitFolderAdapter path
        let proxy = folder.openProxy ()
        let kit = newKitFile (steppingClock ())
        let stem = FileStem.ofText "QZ-LR"
        let original = KitFileText "Original setup: μ, λ\r\n"
        let replacement = KitFileText "New setup: 測定\n"
        Assert.Equal<Result<unit, KitError>>(Ok (), proxy.initialiseKit kit)
        Assert.Equal<Result<unit, KitError>>(Ok (), proxy.writeKindFile SetupKitFile stem original)
        let file = (SetupKitFile.fileNameOf stem).combine path
        let originalBytes = File.ReadAllBytes file.value
        Assert.Equal<byte>(Encoding.UTF8.GetBytes original.value, originalBytes)
        let before = folder.paths ()

        do
            use lockedFile = new FileStream(file.value, FileMode.Open, FileAccess.Read, FileShare.None)
            match proxy.writeKindFile SetupKitFile stem replacement with
            | Error (KitIoErr reason) ->
                Assert.Contains(file.value, reason)
                Assert.Contains(path.value, reason)
            | Error error -> Assert.Fail($"Expected KitIoErr, got {error.describe}")
            | Ok () -> Assert.Fail("Replacing an exclusively held file must fail.")
            Assert.Equal<string list>(before, folder.paths ())

        Assert.Equal<byte>(originalBytes, File.ReadAllBytes file.value)
        Assert.Equal<Result<KitFileText, KitError>>(Ok original, proxy.readKindFile SetupKitFile stem)
        Assert.Equal<string list>(before, folder.paths ())
        Assert.False(folder.paths () |> List.exists (fun name -> name.EndsWith(".tmp", StringComparison.OrdinalIgnoreCase)))

        Assert.Equal<Result<unit, KitError>>(Ok (), proxy.writeKindFile SetupKitFile stem replacement)
        Assert.Equal<byte>(Encoding.UTF8.GetBytes replacement.value, File.ReadAllBytes file.value)
        Assert.Equal<Result<KitFileText, KitError>>(Ok replacement, proxy.readKindFile SetupKitFile stem)
        Assert.Equal<string list>(before, folder.paths ())
