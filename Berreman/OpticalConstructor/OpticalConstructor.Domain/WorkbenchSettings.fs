namespace OpticalConstructor.Domain

open Softellect.Sys.Primitives

/// Spec 0038 Part C (step 005): the pure, elevated workbench-settings record — the
/// Domain shape of the app-level `appsettings.json` values (§6). The composition root
/// (`OpticalConstructor.App/AppConfig.fs`) is the ONLY reader of
/// `Softellect.Sys.AppSettings.AppSettingsProvider`; typed values flow inward as this
/// record, so no view or Domain module ever reads the provider ambiently. User
/// preferences live in `environment.json` (`Ui/UserEnvironment.fs`) — the two stores
/// never merge; this record carries app configuration only.
module WorkbenchSettings =

    /// The typed rejections of `tryCreate` (errors as values, each case carrying the
    /// offending raw value — never a throw). Each setting is a count or a cap, so
    /// zero and negative values are meaningless and rejected at the parse boundary.
    type WorkbenchSettingsError =
        | NonPositiveQuickPickThresholdErr of value : int
        | NonPositiveTreeAutoBuildThresholdErr of value : int
        | NonPositiveThicknessBucketCapErr of value : int
        | NonPositiveKitHistoryLimitErr of value : int

    /// Select-state window policy (§5.3): Select-state windows open modal
    /// (`ShowDialog`, owner = requesting window) under `ModalSelectWindows`,
    /// non-modal under `ModelessSelectWindows` (the default — the wire switch
    /// `SelectWindowsModal` defaults to false). A named two-case DU, never a naked
    /// bool, so a match site reads as prose; `.value` is the wire form (true =
    /// modal), reached only at the IO boundary. Both wire values are meaningful, so
    /// `create` is total (no `tryCreate`).
    type SelectWindowModality =
        | ModalSelectWindows
        | ModelessSelectWindows

        /// The appsettings.json wire form: true = modal (IO boundary only).
        member this.value =
            match this with
            | ModalSelectWindows -> true
            | ModelessSelectWindows -> false

        static member create (modal : bool) : SelectWindowModality =
            match modal with
            | true -> ModalSelectWindows
            | false -> ModelessSelectWindows

        static member defaultValue : SelectWindowModality = ModelessSelectWindows

    /// The bind-directly cutoff of the quick-pick flow (§9.5): a result set of at
    /// most this many entries offers a direct pick instead of the full window.
    /// Default 5.
    type QuickPickThreshold =
        | QuickPickThreshold of int

        member this.value = let (QuickPickThreshold v) = this in v
        static member defaultValue : QuickPickThreshold = QuickPickThreshold 5

        static member tryCreate (v : int) : Result<QuickPickThreshold, WorkbenchSettingsError> =
            if v > 0 then QuickPickThreshold v |> Ok
            else NonPositiveQuickPickThresholdErr v |> Error

    /// The faceted-tree materialization gate (§7.6): the tree auto-builds only when
    /// the result count is at most this value; above it a Show/Search prompt gates
    /// the build. Default 100.
    type TreeAutoBuildThreshold =
        | TreeAutoBuildThreshold of int

        member this.value = let (TreeAutoBuildThreshold v) = this in v
        static member defaultValue : TreeAutoBuildThreshold = TreeAutoBuildThreshold 100

        static member tryCreate (v : int) : Result<TreeAutoBuildThreshold, WorkbenchSettingsError> =
            if v > 0 then TreeAutoBuildThreshold v |> Ok
            else NonPositiveTreeAutoBuildThresholdErr v |> Error

    /// The maximum bucket count of the 1–2–5 log-ladder thickness bucketing (§7.5):
    /// empty buckets drop and the fewest-first merge runs until at most this many
    /// remain. Default 8.
    type ThicknessBucketCap =
        | ThicknessBucketCap of int

        member this.value = let (ThicknessBucketCap v) = this in v
        static member defaultValue : ThicknessBucketCap = ThicknessBucketCap 8

        static member tryCreate (v : int) : Result<ThicknessBucketCap, WorkbenchSettingsError> =
            if v > 0 then ThicknessBucketCap v |> Ok
            else NonPositiveThicknessBucketCapErr v |> Error

    /// The most kits the kit history keeps; over it the oldest unstarred kit is
    /// dropped and a starred one never is (001 §F, spec 0050 S5.1). Default 100.
    type KitHistoryLimit =
        | KitHistoryLimit of int

        member this.value = let (KitHistoryLimit v) = this in v
        static member defaultValue : KitHistoryLimit = KitHistoryLimit 100

        static member tryCreate (v : int) : Result<KitHistoryLimit, WorkbenchSettingsError> =
            if v > 0 then KitHistoryLimit v |> Ok
            else NonPositiveKitHistoryLimitErr v |> Error

    /// The workbench settings record: every field an elevated type — no naked bool,
    /// no raw int. `defaults` is the single source of the built-in values (the
    /// composition root reads each key with its field of `defaults` as the default, so
    /// 5 / 100 / 8 / modeless and the kit history limit's 100 appear exactly once, here).
    type WorkbenchSettings =
        {
            selectWindowModality : SelectWindowModality
            quickPickThreshold : QuickPickThreshold
            treeAutoBuildThreshold : TreeAutoBuildThreshold
            thicknessBucketCap : ThicknessBucketCap
            kitHistoryLimit : KitHistoryLimit
        }

        static member defaults : WorkbenchSettings =
            {
                selectWindowModality = SelectWindowModality.defaultValue
                quickPickThreshold = QuickPickThreshold.defaultValue
                treeAutoBuildThreshold = TreeAutoBuildThreshold.defaultValue
                thicknessBucketCap = ThicknessBucketCap.defaultValue
                kitHistoryLimit = KitHistoryLimit.defaultValue
            }

    /// Every location of 001 §G, each an appsettings.json key read once at the composition root (spec 0050 S2.4).
    type DataLocations =
        {
            /// The library database's SQLite connection string.
            libraryConnection : ConnectionString
            /// The library database's Backups folder.
            backupsFolder : FolderName
            /// The per-user preferences folder.
            preferencesFolder : FolderName
            /// The suggested parent of kit folders.
            kitsFolder : FolderName
            /// The exports folder.
            exportsFolder : FolderName
            /// The catalogue-export folder.
            cataloguesFolder : FolderName
            /// The shipped catalogue folder.
            shippedCatalogueFolder : FolderName
        }

        static member shippedCatalogueDefault : FolderName = FolderName "Catalogue"

        static member defaultsUnder (documents : FolderName) : DataLocations =
            let kits = FolderName "Optical Constructor/Kits"
            let exports = FolderName "Optical Constructor/Exports"
            let catalogues = FolderName "Optical Constructor/Catalogues"

            {
                libraryConnection = ConnectionString "Data Source=%LOCALAPPDATA%/Softellect/Berreman/OpticalConstructor/library.db"
                backupsFolder = FolderName "%LOCALAPPDATA%/Softellect/Berreman/OpticalConstructor/Backups"
                preferencesFolder = FolderName "%APPDATA%/Softellect/Berreman/OpticalConstructor"
                kitsFolder = documents.combine kits
                exportsFolder = documents.combine exports
                cataloguesFolder = documents.combine catalogues
                shippedCatalogueFolder = DataLocations.shippedCatalogueDefault
            }

    /// One location of `DataLocations`, as the Data locations window lists it (spec 0050 S5.3).
    /// It is never stored, so it has no tag.
    type DataLocationKind =
        /// The location in `libraryConnection`.
        | LibraryDatabaseLocation
        /// The location in `backupsFolder`.
        | LibraryBackupsLocation
        /// The location in `preferencesFolder`.
        | PreferencesLocation
        /// The location in `kitsFolder`.
        | KitsLocation
        /// The location in `exportsFolder`.
        | ExportsLocation
        /// The location in `cataloguesFolder`.
        | CataloguesLocation
        /// The location in `shippedCatalogueFolder`.
        | ShippedCatalogueLocation

        /// Every kind once, in the order of the fields of `DataLocations` (spec 0050 S5.3).
        static member all : DataLocationKind list =
            [
                LibraryDatabaseLocation
                LibraryBackupsLocation
                PreferencesLocation
                KitsLocation
                ExportsLocation
                CataloguesLocation
                ShippedCatalogueLocation
            ]
