namespace OpticalConstructor.Tests

open System
open Xunit
open Softellect.Sys.Primitives
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.WorkbenchSettings
open OpticalConstructor.Domain.KitHistory

module KitHistoryTests =

    let private firstOpen : DateTime = DateTime(2026, 11, 3, 12, 0, 0, DateTimeKind.Utc)

    let private folderOf (path : string) : KitFolder = KitFolder (FolderName path)

    let private entryOf (kit : string) (id : string) (minutes : float) (star : KitStar) : KitHistoryEntry =
        {
            folder = folderOf $"C:/kits/{kit}"
            kitId = KitId (Guid.Parse id)
            name = KitName $"Kit {kit}"
            lastOpened = ClockTime (firstOpen.AddMinutes minutes)
            star = star
        }

    let private historyOf (entries : KitHistoryEntry list) : KitHistory = { KitHistory.empty with entries = entries }

    let private kitA : KitHistoryEntry = entryOf "a" "3d0f6a51-8c2e-4b7a-9e14-52a7c0d9e3f1" 5.0 UnstarredKit
    let private kitB : KitHistoryEntry = entryOf "b" "7a21c4e9-0f3b-4d68-b5a2-1e9c8d47f620" 1.0 StarredKit
    let private kitC : KitHistoryEntry = entryOf "c" "b84e19d2-6a07-4f35-8c91-d3f2a5e07b48" 9.0 UnstarredKit
    let private kitD : KitHistoryEntry = entryOf "d" "e5c37b80-2d94-4a1f-a6e8-9b0d14c6f273" 7.0 StarredKit
    let private kitE : KitHistoryEntry = entryOf "e" "19f8d2a6-c53e-47b0-8d7f-a4e61b9c05d2" 5.0 UnstarredKit

    let private mixed : KitHistory = historyOf [ kitA; kitB; kitC; kitD; kitE ]

    /// Starts at `start` and advances one second per call (spec 0050 S5.1).
    let private steppingClock (start : DateTime) : ClockProvider =
        let next = ref start

        {
            now =
                fun () ->
                    let current = next.Value
                    next.Value <- current.AddSeconds 1.0
                    ClockTime current
        }

    let private kitIdOf (index : int) : KitId = KitId (Guid.Parse $"00000000-0000-4000-8000-%012d{index}")

    /// The kit file a kit is opened from; its own times play no part in the history (spec 0050 S5.1).
    let private kitFileOf (id : KitId) (name : KitName) : KitFile =
        KitFile.create id name (KitAuthor "Kit history tests") (ClockTime firstOpen)

    [<Fact>]
    let ``a kit star round-trips its wire tag`` () : unit =
        Assert.Equal<int>(1, StarredKit.value)
        Assert.Equal<int>(2, UnstarredKit.value)

        for star in [ StarredKit; UnstarredKit ] do
            Assert.Equal<KitStar option>(Some star, KitStar.tryOfWire star.value)

        for tag in [ 0; 3 ] do
            Assert.Equal<KitStar option>(None, KitStar.tryOfWire tag)

    [<Fact>]
    let ``starred kits are listed first, each group newest open first`` () : unit =
        Assert.Equal<FileSchemaVersion>(FileSchemaVersion 1, KitHistory.currentSchemaVersion)
        Assert.Equal<FileSchemaVersion>(KitHistory.currentSchemaVersion, KitHistory.empty.schemaVersion)
        Assert.Empty(KitHistory.empty.entries)
        Assert.Empty(KitHistory.empty.displayOrder)

        Assert.Equal<KitHistoryEntry list>([ kitD; kitB; kitC; kitA; kitE ], mixed.displayOrder)
        Assert.Equal<KitHistoryEntry list>([ kitA; kitB; kitC; kitD; kitE ], mixed.entries)

    [<Fact>]
    let ``star and unstar change only the named kit, matching its folder in any case`` () : unit =
        let starred = mixed.star (folderOf "C:/KITS/A")

        Assert.Equal<KitHistory>(historyOf [ { kitA with star = StarredKit }; kitB; kitC; kitD; kitE ], starred)
        Assert.Equal<KitHistory>(mixed, starred.unstar (folderOf "C:/kits/a"))
        Assert.Equal<KitHistory>(mixed, mixed.star (folderOf "C:/kits/zz"))

    [<Fact>]
    let ``forget removes only the named kit, starred or not`` () : unit =
        let withoutB = mixed.forget (folderOf "C:/Kits/B")

        Assert.Equal<KitHistory>(historyOf [ kitA; kitC; kitD; kitE ], withoutB)
        Assert.Equal<KitHistory>(historyOf [ kitA; kitD; kitE ], withoutB.forget (folderOf "C:/kits/c"))
        Assert.Equal<KitHistory>(mixed, mixed.forget (folderOf "C:/kits/zz"))

    [<Fact>]
    let ``evict drops the oldest unstarred kits down to the limit`` () : unit =
        Assert.Equal<KitHistory>(historyOf [ kitA; kitB; kitC; kitD ], mixed.evict (KitHistoryLimit 4))
        Assert.Equal<KitHistory>(historyOf [ kitB; kitC; kitD ], mixed.evict (KitHistoryLimit 3))
        Assert.Equal<KitHistory>(mixed, mixed.evict (KitHistoryLimit 5))

    [<Fact>]
    let ``evict never drops a starred kit, even over the limit`` () : unit =
        let evicted = (historyOf [ kitB; kitC; kitD ]).evict (KitHistoryLimit 1)
        let starsOnly = historyOf [ kitB; kitD ]

        Assert.Equal<KitHistory>(starsOnly, evicted)
        Assert.Equal<int>(2, evicted.entries.Length)
        Assert.Equal<KitHistory>(starsOnly, starsOnly.evict (KitHistoryLimit 1))

    [<Fact>]
    let ``bump puts the opened kit first, matches its folder in any case and evicts beyond the limit`` () : unit =
        let clock = steppingClock firstOpen
        let limit = KitHistoryLimit 16
        let foldersOf (history : KitHistory) : KitFolder list = history.entries |> List.map _.folder

        let holdingA = KitHistory.empty.bump limit clock (folderOf @"C:\kits\a") (kitFileOf (kitIdOf 101) (KitName "Kit a"))
        let afterB = holdingA.bump limit clock (folderOf @"C:\kits\b") (kitFileOf (kitIdOf 102) (KitName "Kit b"))
        Assert.Equal<KitFolder list>([ folderOf @"C:\kits\b"; folderOf @"C:\kits\a" ], foldersOf afterB)

        let reBumped = afterB.bump limit clock (folderOf @"C:\KITS\A") (kitFileOf (kitIdOf 101) (KitName "Kit a"))
        Assert.Equal<KitFolder list>([ folderOf @"C:\KITS\A"; folderOf @"C:\kits\b" ], foldersOf reBumped)

        let full =
            [ 1 .. 20 ]
            |> List.fold
                (fun (history : KitHistory) index ->
                    history.bump limit clock (folderOf $@"C:\kits\f{index}") (kitFileOf (kitIdOf index) (KitName $"Kit f{index}")))
                KitHistory.empty

        Assert.Equal<int>(16, full.entries.Length)
        Assert.Equal<KitFolder>(folderOf @"C:\kits\f20", (List.head full.entries).folder)
        Assert.Equal<KitFolder>(folderOf @"C:\kits\f5", (List.last full.entries).folder)

    [<Fact>]
    let ``bump keeps a reopened kit's star and refreshes its folder text, id, name and last open`` () : unit =
        let reopenedAt = firstOpen.AddHours 1.0
        let clock = steppingClock reopenedAt
        let old = { kitA with name = KitName "Old"; star = StarredKit }
        let newId = KitId (Guid.Parse "c2d7e4a1-58b3-4f06-9a2c-71e0b5d83f94")
        let newName = KitName "New"

        let bumped = (historyOf [ kitB; old; kitC ]).bump (KitHistoryLimit 16) clock (folderOf "C:/KITS/A") (kitFileOf newId newName)

        let reopened : KitHistoryEntry =
            {
                folder = folderOf "C:/KITS/A"
                kitId = newId
                name = newName
                lastOpened = ClockTime reopenedAt
                star = StarredKit
            }

        Assert.Equal<KitHistory>(historyOf [ reopened; kitB; kitC ], bumped)
        Assert.Equal<ClockTime>(ClockTime (reopenedAt.AddSeconds 1.0), clock.now ())

    [<Fact>]
    let ``a bump over the limit drops the oldest unstarred kit and keeps every star`` () : unit =
        let unstarred2 = entryOf "u2" "4b9e0c37-a1d5-4e82-b6f3-0d7a29c5e148" 3.0 UnstarredKit
        let unstarred1 = entryOf "u1" "8f3a6d12-7c4b-4a09-9e5d-b21c60f8a7e3" 2.0 UnstarredKit
        let starred = entryOf "s" "d05b8e93-2f61-47ca-8b14-e9a3c7d26f50" 1.0 StarredKit
        let opened = entryOf "n" "5e2c9a70-b84d-4f1e-a3c6-17d0f9b4e825" 4.0 UnstarredKit
        let clock = steppingClock (firstOpen.AddMinutes 4.0)

        let bumped = (historyOf [ unstarred2; unstarred1; starred ]).bump (KitHistoryLimit 3) clock opened.folder (kitFileOf opened.kitId opened.name)

        Assert.Equal<KitHistory>(historyOf [ opened; unstarred2; starred ], bumped)

    [<Fact>]
    let ``a bump into a full starred history never drops a star`` () : unit =
        let starred3 = entryOf "s3" "a7c41e05-9d28-4b63-8f0a-3e6d52b9c1f7" 3.0 StarredKit
        let starred2 = entryOf "s2" "16e8b3d9-0a74-4c5f-b2e1-c9f0a64d7385" 2.0 StarredKit
        let starred1 = entryOf "s1" "f4d92a68-3b15-4e07-a9c8-5b7e01d6c243" 1.0 StarredKit
        let opened = entryOf "n" "5e2c9a70-b84d-4f1e-a3c6-17d0f9b4e825" 4.0 UnstarredKit
        let full = historyOf [ starred3; starred2; starred1 ]
        let limit = KitHistoryLimit 3

        let withNew = full.bump limit (steppingClock (firstOpen.AddMinutes 4.0)) opened.folder (kitFileOf opened.kitId opened.name)
        Assert.Equal<KitHistory>(full, withNew)

        let reopenedAt = firstOpen.AddMinutes 5.0
        let withLast = full.bump limit (steppingClock reopenedAt) starred1.folder (kitFileOf starred1.kitId starred1.name)
        Assert.Equal<KitHistory>(historyOf [ { starred1 with lastOpened = ClockTime reopenedAt }; starred3; starred2 ], withLast)

    [<Fact>]
    let ``a kit history error renders its condition and its reason`` () : unit =
        let reason = "C:/Users/someone/AppData/Roaming/kit-history.json is read-only"
        let description = (KitHistoryWriteErr reason).describe

        Assert.Equal<string>($"the kit history could not be written: {reason}", description)
        Assert.Contains(reason, description)
