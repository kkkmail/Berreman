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
    let ``a kit history error renders its condition and its reason`` () : unit =
        let reason = "C:/Users/someone/AppData/Roaming/kit-history.json is read-only"
        let description = (KitHistoryWriteErr reason).describe

        Assert.Equal<string>($"the kit history could not be written: {reason}", description)
        Assert.Contains(reason, description)
