namespace OpticalConstructor.Ui.Tests

open Xunit
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
        Assert.Equal<string option>(None, lastFolder measuredDataFolderKey noFolders)

    [<Fact>]
    let ``a confirmed selection records the file's folder under the purpose key`` () =
        let updated = applyPick measuredDataFolderKey (FilePicked @"C:\work\optics\run-01\measured.csv") noFolders
        Assert.Equal<string option>(Some @"C:\work\optics\run-01", lastFolder measuredDataFolderKey updated)

    [<Fact>]
    let ``a cancel leaves the persisted folder untouched`` () =
        let seeded = rememberFolder measuredDataFolderKey @"C:\prev\a.csv" noFolders
        let afterCancel = applyPick measuredDataFolderKey PickCancelled seeded
        // Cancel changes nothing: the whole environment (and the persisted folder) is unchanged.
        Assert.Equal<EnvironmentSettings>(seeded, afterCancel)
        Assert.Equal<string option>(Some @"C:\prev", lastFolder measuredDataFolderKey afterCancel)

    [<Fact>]
    let ``the next open passes the persisted folder as the picker start folder`` () =
        // A confirmed selection persists its folder; the NEXT open reads exactly that folder to seed the
        // picker's SuggestedStartLocation (resolved through the provider at the IO edge).
        let afterPick = applyPick measuredDataFolderKey (FilePicked @"D:\data\ellipso\e.csv") noFolders
        Assert.Equal<string option>(Some @"D:\data\ellipso", lastFolder measuredDataFolderKey afterPick)

    [<Fact>]
    let ``a re-pick in a new folder replaces the persisted folder`` () =
        let first = applyPick measuredDataFolderKey (FilePicked @"C:\a\one.csv") noFolders
        let second = applyPick measuredDataFolderKey (FilePicked @"C:\b\two.csv") first
        Assert.Equal<string option>(Some @"C:\b", lastFolder measuredDataFolderKey second)

    [<Fact>]
    let ``a bare filename with no directory part leaves the folder map untouched`` () =
        // Path.GetDirectoryName of a bare name is empty — no folder to remember, so the map is unchanged.
        let updated = applyPick measuredDataFolderKey (FilePicked "measured.csv") noFolders
        Assert.Equal<string option>(None, lastFolder measuredDataFolderKey updated)

    [<Fact>]
    let ``the picker options request a single measured-data file with no start folder when none is persisted`` () =
        // The pure option-building (no picker raised, no folder resolved): title + single-select, and a
        // null SuggestedStartLocation when the resolved folder is None.
        let options = buildDataFilePickerOptions None
        Assert.False(options.AllowMultiple)
        Assert.Equal("Attach measured-data file", options.Title)
        Assert.Null(options.SuggestedStartLocation)
