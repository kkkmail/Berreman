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
