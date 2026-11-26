namespace OpticalConstructor.Tests

open System
open System.IO
open Xunit
open Softellect.Sys.Primitives
open OpticalConstructor.Domain.ItemModels
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.KitFolders
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Storage.KitContentFiles
open OpticalConstructor.Storage.KitFolderStore
open KitContentRoundTripFixture

/// A kit's `<label>.item-json` files written and read through `KitProxy` in initialized kit folders under the test
/// output directory (spec 0050 S7.1).
module KitItemFileTests =

    let private classFolder = FolderName "KitItemFileTests"

    let private cpl1 : Item =
        let draft : ItemDraft =
            {
                id = ItemId (Guid.Parse "5d1f8b0e-3c47-4a92-8e6b-0f2a9c71d4e3")
                label = ItemLabel "CPL1"
                model = LibraryModelChoice (BuiltinCatalogue.itemModel ItemModelIds.circularPolarizerModelId)
                dial = DialMounted DialPositive
            }
        draft.toItem

    let private lp1 : Item =
        let draft : ItemDraft =
            {
                id = ItemId (Guid.Parse "a8e2c6f4-91b3-4d05-b7c8-3e6f1a2d5b90")
                label = ItemLabel "LP1"
                model = LibraryModelChoice (BuiltinCatalogue.itemModel ItemModelIds.realLinearPolarizerModelId)
                dial = DialMounted DialNegative
            }
        draft.toItem

    let private phone : Item =
        let draft : ItemDraft =
            {
                id = ItemId (Guid.Parse "c4b7e913-6a2d-4f8e-9c15-d0a3b8f26e47")
                label = ItemLabel "Phone"
                model = LibraryModelChoice (BuiltinCatalogue.itemModel ItemModelIds.intensityDetectorModelId)
                dial = FixedMounting
            }
        draft.toItem

    /// CPL1 at revision 2, built directly: its dial zero re-measured and its revision-1 parameters in its history.
    let private cpl1Revision2 : Item =
        let dialZero = ParameterName "dialZero"
        let remeasured (parameter : Parameter) : Parameter =
            if parameter.name = dialZero then
                {
                    parameter with state = KnownParameter (ParameterValue 0.25)
                }
            else
                parameter
        let revisionOne : ItemRevisionRecord =
            {
                revision = ItemRevision 1
                parameters = cpl1.parameters
            }
        {
            cpl1 with
                parameters = cpl1.parameters |> List.map remeasured
                revision = ItemRevision 2
                history = [ revisionOne ]
        }

    let private itemFileTextOf (item : Item) : KitFileText =
        item.itemFile.fileText |> Result.defaultWith (fun error -> failwith error.describe)

    let private storedItemFiles (folder : KitFolder) : FileName list =
        Directory.GetFiles(folder.value.value, $"*{ItemKitFile.extension.value}", SearchOption.TopDirectoryOnly)
        |> Array.map (fun path -> FileName (Path.GetFileName path))
        |> Array.sortWith (fun first second -> String.CompareOrdinal(first.value, second.value))
        |> Array.toList

    let private folderBytes (folder : KitFolder) : Map<FileName, FileByteContent> =
        Directory.GetFiles(folder.value.value, "*", SearchOption.AllDirectories)
        |> Array.map (fun path ->
            let name = FileName (Path.GetRelativePath(folder.value.value, path))
            let content = FileByteContent (File.ReadAllBytes path)
            name, content)
        |> Map.ofArray

    /// A new kit holding Phone, CPL1 and LP1, written out of label order.
    let private kitWithItems () : KitFolder * KitProxy =
        let folder, proxy = createContentTestKit classFolder
        for item in [ phone; cpl1; lp1 ] do
            proxy.writeItem item |> kitContentValue
        folder, proxy

    [<Fact>]
    let ``a kit's item files round-trip, each stored under its label`` () : unit =
        let folder, _ = kitWithItems ()
        let expectedFiles = [ FileName "CPL1.item-json"; FileName "LP1.item-json"; FileName "Phone.item-json" ]
        Assert.Equal<FileName list>(expectedFiles, storedItemFiles folder)

        let reader = KitProxy.create folder
        Assert.Equal<Item list>([ cpl1; lp1; phone ], reader.readItems () |> kitContentValue)
        for item in [ cpl1; lp1; phone ] do
            Assert.Equal<KitFileText>(itemFileTextOf item, reader.readKindFile ItemKitFile item.label.fileStem |> kitContentValue)

        // A file's stem matches its item's label ignoring case.
        let lp2 : Item =
            {
                lp1 with
                    id = ItemId (Guid.Parse "1b7d4e92-c0a5-4f36-8e21-9d3c6b5a0f78")
                    label = ItemLabel "LP2"
            }
        let lowerFile = FileName "lp2.item-json"
        File.WriteAllText((lowerFile.combine folder.value).value, (itemFileTextOf lp2).value)
        Assert.Equal<Item list>([ cpl1; lp1; lp2; phone ], reader.readItems () |> kitContentValue)

    [<Fact>]
    let ``writing an item again under its label replaces its file`` () : unit =
        let folder, proxy = createContentTestKit classFolder
        proxy.writeItem cpl1 |> kitContentValue
        proxy.writeItem cpl1Revision2 |> kitContentValue

        let reader = KitProxy.create folder
        Assert.Equal<Item list>([ cpl1Revision2 ], reader.readItems () |> kitContentValue)
        Assert.Equal<FileName list>([ FileName "CPL1.item-json" ], storedItemFiles folder)
        Assert.Equal<KitFileText>(itemFileTextOf cpl1Revision2, reader.readKindFile ItemKitFile cpl1.label.fileStem |> kitContentValue)

        // An item that does not render refuses the write by its file and leaves the stored one whole.
        let unrenderable =
            {
                cpl1Revision2 with
                    parameters = cpl1Revision2.parameters |> List.map (fun parameter -> { parameter with state = KnownParameter (ParameterValue Double.NaN) })
            }
        let before = folderBytes folder
        match proxy.writeItem unrenderable with
        | Error (KitFileRejectedErr reason) -> Assert.Contains("CPL1.item-json", reason)
        | Error error -> Assert.Fail($"Expected KitFileRejectedErr, got {error.describe}")
        | Ok () -> Assert.Fail("An item that cannot be rendered must refuse the write.")
        Assert.Equal<Map<FileName, FileByteContent>>(before, folderBytes folder)
        Assert.Equal<Item list>([ cpl1Revision2 ], reader.readItems () |> kitContentValue)

    [<Fact>]
    let ``an item file that does not read, holds another label or repeats an id refuses the read, naming the file`` () : unit =
        let cpl9 : Item =
            {
                lp1 with label = ItemLabel "CPL9"
            }
        let invalidFiles : (FileName * KitFileText) list =
            [
                FileName "Broken.item-json", KitFileText "{"
                FileName "Other.item-json", itemFileTextOf cpl1
                FileName "CPL9.item-json", itemFileTextOf cpl9
            ]

        for file, text in invalidFiles do
            let folder, proxy = kitWithItems ()
            let path = file.combine folder.value
            File.WriteAllText(path.value, text.value)
            match proxy.readItems () with
            | Error (KitFileRejectedErr reason) ->
                Assert.Contains(file.value, reason)
                // Other.item-json also repeats CPL1's id; the label rule answers first, naming the stem it expected.
                if file.value = "Other.item-json" then
                    Assert.Contains("'CPL1'", reason)
                    Assert.Contains("'Other'", reason)
                if file.value = "CPL9.item-json" then
                    Assert.Contains(lp1.id.value.ToString(), reason)
            | Error error -> Assert.Fail($"Expected KitFileRejectedErr naming '{file.value}', got {error.describe}")
            | Ok _ -> Assert.Fail($"A kit holding '{file.value}' must refuse the whole read.")

            // The refusal is that file's alone: without it the kit reads every item again.
            File.Delete path.value
            Assert.Equal<Item list>([ cpl1; lp1; phone ], proxy.readItems () |> kitContentValue)

    [<Fact>]
    let ``a read-only kit refuses an item write and writes nothing`` () : unit =
        let folder, proxy = kitWithItems ()
        let lp2 : Item =
            {
                lp1 with
                    id = ItemId (Guid.Parse "e6f09a3c-2b58-47d1-8a4e-71c5d9b0f382")
                    label = ItemLabel "LP2"
            }
        let before = folderBytes folder
        let reader = proxy.readOnlyView

        for item in [ cpl1Revision2; lp2 ] do
            match reader.writeItem item with
            | Error (KitReadOnlyErr _) -> ()
            | Error error -> Assert.Fail($"Expected KitReadOnlyErr for '{item.label.value}', got {error.describe}")
            | Ok () -> Assert.Fail($"A read-only kit must refuse writing '{item.label.value}'.")

        Assert.Equal<Map<FileName, FileByteContent>>(before, folderBytes folder)
        Assert.Equal<Item list>([ cpl1; lp1; phone ], reader.readItems () |> kitContentValue)
