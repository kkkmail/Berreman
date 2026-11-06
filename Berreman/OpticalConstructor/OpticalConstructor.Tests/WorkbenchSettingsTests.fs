namespace OpticalConstructor.Tests

open Xunit
open Softellect.Sys.Primitives
open OpticalConstructor.Domain.WorkbenchSettings

/// Spec 0038 Part C (step 005) — the pure `WorkbenchSettings` Domain record. The
/// tests construct the settings record and its elevated field types DIRECTLY —
/// defaults and `tryCreate` rejection — with no file IO anywhere: the
/// `AppSettingsProvider` read seam lives only in the App composition root, and the
/// defaults path is proven here purely by construction.
module WorkbenchSettingsTests =

    [<Fact>]
    let ``DataLocations.defaultsUnder holds the 001 §G locations as configured text`` () : unit =
        let documents = FolderName "D:/Docs"
        let libraryConnection = ConnectionString "Data Source=%LOCALAPPDATA%/Softellect/Berreman/OpticalConstructor/library.db"
        let backupsFolder = FolderName "%LOCALAPPDATA%/Softellect/Berreman/OpticalConstructor/Backups"
        let preferencesFolder = FolderName "%APPDATA%/Softellect/Berreman/OpticalConstructor"
        let kitsFolder = FolderName "Optical Constructor/Kits"
        let exportsFolder = FolderName "Optical Constructor/Exports"
        let cataloguesFolder = FolderName "Optical Constructor/Catalogues"
        let shippedCatalogueFolder = FolderName "Catalogue"
        let locations = DataLocations.defaultsUnder documents

        Assert.Equal<ConnectionString>(libraryConnection, locations.libraryConnection)
        Assert.Equal<FolderName>(backupsFolder, locations.backupsFolder)
        Assert.Equal<FolderName>(preferencesFolder, locations.preferencesFolder)
        Assert.Equal<FolderName>(documents.combine kitsFolder, locations.kitsFolder)
        Assert.Equal<FolderName>(documents.combine exportsFolder, locations.exportsFolder)
        Assert.Equal<FolderName>(documents.combine cataloguesFolder, locations.cataloguesFolder)
        Assert.Equal<FolderName>(shippedCatalogueFolder, DataLocations.shippedCatalogueDefault)
        Assert.Equal<FolderName>(DataLocations.shippedCatalogueDefault, locations.shippedCatalogueFolder)

    [<Fact>]
    let ``defaults carry modeless select windows and the 5 / 100 / 8 thresholds`` () =
        let s = WorkbenchSettings.defaults
        Assert.Equal(ModelessSelectWindows, s.selectWindowModality)
        Assert.Equal(5, s.quickPickThreshold.value)
        Assert.Equal(100, s.treeAutoBuildThreshold.value)
        Assert.Equal(8, s.thicknessBucketCap.value)

    [<Fact>]
    let ``defaults equals the record constructed directly from the per-type defaults`` () =
        let constructed =
            {
                selectWindowModality = SelectWindowModality.defaultValue
                quickPickThreshold = QuickPickThreshold.defaultValue
                treeAutoBuildThreshold = TreeAutoBuildThreshold.defaultValue
                thicknessBucketCap = ThicknessBucketCap.defaultValue
            }
        Assert.Equal(constructed, WorkbenchSettings.defaults)

    [<Fact>]
    let ``select-window modality round-trips its wire form: false is modeless, true is modal`` () =
        Assert.Equal(ModelessSelectWindows, SelectWindowModality.create false)
        Assert.Equal(ModalSelectWindows, SelectWindowModality.create true)
        Assert.False(ModelessSelectWindows.value)
        Assert.True(ModalSelectWindows.value)

    [<Theory>]
    [<InlineData(1)>]
    [<InlineData(5)>]
    [<InlineData(1000)>]
    let ``QuickPickThreshold accepts a positive count and round-trips its value`` (raw : int) =
        match QuickPickThreshold.tryCreate raw with
        | Ok v -> Assert.Equal(raw, v.value)
        | Error e -> Assert.Fail($"expected Ok, got %A{e}")

    [<Theory>]
    [<InlineData(0)>]
    [<InlineData(-1)>]
    [<InlineData(-100)>]
    let ``QuickPickThreshold rejects a non-positive count with the typed error carrying the value`` (raw : int) =
        match QuickPickThreshold.tryCreate raw with
        | Error (NonPositiveQuickPickThresholdErr v) -> Assert.Equal(raw, v)
        | Error e -> Assert.Fail($"expected NonPositiveQuickPickThresholdErr, got %A{e}")
        | Ok v -> Assert.Fail($"expected rejection, got Ok %A{v}")

    [<Theory>]
    [<InlineData(1)>]
    [<InlineData(100)>]
    [<InlineData(5000)>]
    let ``TreeAutoBuildThreshold accepts a positive count and round-trips its value`` (raw : int) =
        match TreeAutoBuildThreshold.tryCreate raw with
        | Ok v -> Assert.Equal(raw, v.value)
        | Error e -> Assert.Fail($"expected Ok, got %A{e}")

    [<Theory>]
    [<InlineData(0)>]
    [<InlineData(-1)>]
    [<InlineData(-100)>]
    let ``TreeAutoBuildThreshold rejects a non-positive count with the typed error carrying the value`` (raw : int) =
        match TreeAutoBuildThreshold.tryCreate raw with
        | Error (NonPositiveTreeAutoBuildThresholdErr v) -> Assert.Equal(raw, v)
        | Error e -> Assert.Fail($"expected NonPositiveTreeAutoBuildThresholdErr, got %A{e}")
        | Ok v -> Assert.Fail($"expected rejection, got Ok %A{v}")

    [<Theory>]
    [<InlineData(1)>]
    [<InlineData(8)>]
    [<InlineData(64)>]
    let ``ThicknessBucketCap accepts a positive cap and round-trips its value`` (raw : int) =
        match ThicknessBucketCap.tryCreate raw with
        | Ok v -> Assert.Equal(raw, v.value)
        | Error e -> Assert.Fail($"expected Ok, got %A{e}")

    [<Theory>]
    [<InlineData(0)>]
    [<InlineData(-1)>]
    [<InlineData(-100)>]
    let ``ThicknessBucketCap rejects a non-positive cap with the typed error carrying the value`` (raw : int) =
        match ThicknessBucketCap.tryCreate raw with
        | Error (NonPositiveThicknessBucketCapErr v) -> Assert.Equal(raw, v)
        | Error e -> Assert.Fail($"expected NonPositiveThicknessBucketCapErr, got %A{e}")
        | Ok v -> Assert.Fail($"expected rejection, got Ok %A{v}")

    [<Fact>]
    let ``a settings record assembled from validated values carries the validated thresholds`` () =
        // The inward-flow shape the composition root produces: every field elevated,
        // thresholds passing through tryCreate — constructed here with no provider
        // and no disk.
        let okOr (fallback : 'Setting) (result : Result<'Setting, WorkbenchSettingsError>) : 'Setting =
            match result with
            | Ok v -> v
            | Error _ -> fallback

        let assembled =
            {
                selectWindowModality = SelectWindowModality.create true
                quickPickThreshold = QuickPickThreshold.tryCreate 7 |> okOr QuickPickThreshold.defaultValue
                treeAutoBuildThreshold = TreeAutoBuildThreshold.tryCreate 250 |> okOr TreeAutoBuildThreshold.defaultValue
                thicknessBucketCap = ThicknessBucketCap.tryCreate 12 |> okOr ThicknessBucketCap.defaultValue
            }
        Assert.Equal(ModalSelectWindows, assembled.selectWindowModality)
        Assert.Equal(7, assembled.quickPickThreshold.value)
        Assert.Equal(250, assembled.treeAutoBuildThreshold.value)
        Assert.Equal(12, assembled.thicknessBucketCap.value)
