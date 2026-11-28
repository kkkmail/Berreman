namespace OpticalConstructor.Ui.Tests

open Xunit
open Softellect.Sys.Primitives
open OpticalConstructor.Ui.UserEnvironment
open OpticalConstructor.Ui.TableAndElementRotationView

/// Spec 0038 Part L (step 038) — the measured-data picker's persistent last-folder plumbing. The
/// picker's IO edge (the real dialog + `StorageProvider`) is off the tested path; these pin the PURE
/// pieces it composes: which folder the next open starts from (`lastFolder`), how a confirmed selection
/// records it (`rememberFolder` / `applyPick`), that a cancel changes nothing, and the option-building
/// shape (`buildDataFilePickerOptions`) — all without raising a real picker.
module DataFilePickerFolderTests =

    /// An environment with no persisted picker folders (the built-in default shape).
    let private noFolders : EnvironmentSettings = { defaults with lastFolders = Map.empty }

    [<Fact>]
    let ``a fresh environment has no persisted measured-data folder`` () =
        Assert.Equal<FolderName option>(None, lastFolder measuredDataFolderKey noFolders)

    [<Fact>]
    let ``a confirmed selection records the file's folder under the purpose key`` () =
        let updated = applyPick measuredDataFolderKey (FilePicked (FileName @"C:\work\optics\run-01\measured.csv")) noFolders
        Assert.Equal<FolderName option>(Some (FolderName @"C:\work\optics\run-01"), lastFolder measuredDataFolderKey updated)

    [<Fact>]
    let ``a cancel leaves the persisted folder untouched`` () =
        let seeded = rememberFolder measuredDataFolderKey (FileName @"C:\prev\a.csv") noFolders
        let afterCancel = applyPick measuredDataFolderKey PickCancelled seeded
        // Cancel changes nothing: the whole environment (and the persisted folder) is unchanged.
        Assert.Equal<EnvironmentSettings>(seeded, afterCancel)
        Assert.Equal<FolderName option>(Some (FolderName @"C:\prev"), lastFolder measuredDataFolderKey afterCancel)

    [<Fact>]
    let ``the next open passes the persisted folder as the picker start folder`` () =
        // A confirmed selection persists its folder; the NEXT open reads exactly that folder to seed the
        // picker's SuggestedStartLocation (resolved through the provider at the IO edge).
        let afterPick = applyPick measuredDataFolderKey (FilePicked (FileName @"D:\data\ellipso\e.csv")) noFolders
        Assert.Equal<FolderName option>(Some (FolderName @"D:\data\ellipso"), lastFolder measuredDataFolderKey afterPick)

    [<Fact>]
    let ``a re-pick in a new folder replaces the persisted folder`` () =
        let first = applyPick measuredDataFolderKey (FilePicked (FileName @"C:\a\one.csv")) noFolders
        let second = applyPick measuredDataFolderKey (FilePicked (FileName @"C:\b\two.csv")) first
        Assert.Equal<FolderName option>(Some (FolderName @"C:\b"), lastFolder measuredDataFolderKey second)

    [<Fact>]
    let ``a bare filename with no directory part leaves the folder map untouched`` () =
        // Path.GetDirectoryName of a bare name is empty — no folder to remember, so the map is unchanged.
        let updated = applyPick measuredDataFolderKey (FilePicked (FileName "measured.csv")) noFolders
        Assert.Equal<FolderName option>(None, lastFolder measuredDataFolderKey updated)

    [<Fact>]
    let ``the picker options request a single measured-data file with no start folder when none is persisted`` () =
        // The pure option-building (no picker raised, no folder resolved): title + single-select, and a
        // null SuggestedStartLocation when the resolved folder is None.
        let options = buildDataFilePickerOptions None
        Assert.False(options.AllowMultiple)
        Assert.Equal("Attach measured-data file", options.Title)
        Assert.Null(options.SuggestedStartLocation)

    [<Fact>]
    let ``a kit records the folder above it, and every kit dialog starts there until another kit is opened`` () =
        // Spec 0051 `.manual/007` 3.4: a kit is its folder, so the last open folder is the one above the kit.
        let first = OpticalConstructor.Domain.Kit.KitFolder (FolderName (System.IO.Path.Combine("C:\\", "Kits", "optics-mueller")))
        let second = OpticalConstructor.Domain.Kit.KitFolder (FolderName (System.IO.Path.Combine("D:\\", "Lab", "2026", "run-7")))
        let once = rememberKitParent first noFolders
        Assert.Equal<FolderName option>(Some (FolderName (System.IO.Path.Combine("C:\\", "Kits"))), lastFolder kitParentFolderKey once)
        let twice = rememberKitParent second once
        Assert.Equal<FolderName option>(Some (FolderName (System.IO.Path.Combine("D:\\", "Lab", "2026"))), lastFolder kitParentFolderKey twice)
        // The data pickers keep their own folders.
        Assert.Equal<FolderName option>(None, lastFolder measuredDataFolderKey twice)

    [<Fact>]
    let ``the environment-file memory starts in the Kits folder, then in the folder above the last kit, and survives a restart`` () =
        let root = System.IO.Path.Combine(System.AppContext.BaseDirectory, "KitFolderMemoryTests", System.Guid.NewGuid().ToString("N"))
        System.IO.Directory.CreateDirectory root |> ignore
        try
            let file = FileName (System.IO.Path.Combine(root, "environment.json"))
            let kits = FolderName (System.IO.Path.Combine(root, "Kits"))
            let memory = OpticalConstructor.Ui.KitOpening.KitFolderMemory.ofEnvironment file kits
            Assert.Equal<FolderName>(kits, memory.startFolder ())
            let lab = System.IO.Path.Combine(root, "Lab")
            memory.rememberKit (OpticalConstructor.Domain.Kit.KitFolder (FolderName (System.IO.Path.Combine(lab, "bench"))))
            let restarted = OpticalConstructor.Ui.KitOpening.KitFolderMemory.ofEnvironment file kits
            Assert.Equal<FolderName>(FolderName lab, restarted.startFolder ())
        finally
            System.IO.Directory.Delete(root, true)
