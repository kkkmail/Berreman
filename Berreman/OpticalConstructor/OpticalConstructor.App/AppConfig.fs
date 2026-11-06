/// App configuration flows inward as elevated settings and data locations through this seam
/// (spec 0038 Part C; spec 0050 S2.4). User preferences remain in environment.json.
namespace OpticalConstructor.App

open System
open System.IO
open Softellect.Sys
open Softellect.Sys.AppSettings
open Softellect.Sys.Logging
open Softellect.Sys.Primitives

open OpticalConstructor.Domain.WorkbenchSettings

/// The appsettings.json key inventory for appSettings and connectionStrings.
/// Provider calls wrap these wire names in AppSettings.ConfigKey.
[<RequireQualifiedAccess>]
module ConfigKey =

    /// appSettings switch (§5.3): Select-state windows open modal when true.
    /// Default false — `SelectWindowModality.defaultValue`.
    [<Literal>]
    let selectWindowsModal = "SelectWindowsModal"

    /// appSettings int (§9.5): the quick-pick bind-directly cutoff. Default 5 —
    /// `QuickPickThreshold.defaultValue`.
    [<Literal>]
    let quickPickThreshold = "QuickPickThreshold"

    /// appSettings int (§7.6): the faceted-tree materialization gate. Default 100 —
    /// `TreeAutoBuildThreshold.defaultValue`.
    [<Literal>]
    let treeAutoBuildThreshold = "TreeAutoBuildThreshold"

    /// appSettings int (§7.5): the 1–2–5 log-ladder bucket cap. Default 8 —
    /// `ThicknessBucketCap.defaultValue`.
    [<Literal>]
    let thicknessBucketCap = "ThicknessBucketCap"

    /// The library database's SQLite connection string, read through `AppSettingsProvider.tryGetConnectionString` (spec 0050 §3.18).
    [<Literal>]
    let sqliteConnectionString = "OpticalConstructorSqlite"

    /// appSettings folder for DataLocations.backupsFolder.
    [<Literal>]
    let libraryBackupsFolder = "LibraryBackupsFolder"

    /// appSettings folder for DataLocations.preferencesFolder.
    [<Literal>]
    let preferencesFolder = "PreferencesFolder"

    /// appSettings folder for DataLocations.kitsFolder.
    [<Literal>]
    let kitsFolder = "KitsFolder"

    /// appSettings folder for DataLocations.exportsFolder.
    [<Literal>]
    let exportsFolder = "ExportsFolder"

    /// appSettings folder for DataLocations.cataloguesFolder.
    [<Literal>]
    let cataloguesFolder = "CataloguesFolder"

    /// appSettings folder for DataLocations.shippedCatalogueFolder.
    [<Literal>]
    let shippedCatalogueFolder = "ShippedCatalogueFolder"

/// The settings doors of the workbench settings (spec 0048 E042, the shape of STL's
/// `BmbDesktopAppSettingsDoors`): a parse from the file's `ConfigValue` that hands back the
/// elevated value, and the render a default is written back through.
[<AutoOpen>]
module WorkbenchSettingsDoors =

    type SelectWindowModality with
        /// The `true` / `false` the `SelectWindowsModal` key has always held (true = modal).
        static member tryCreate (v : AppSettings.ConfigValue) : Result<SelectWindowModality, string> =
            match System.Boolean.TryParse v.value with
            | true, modal -> SelectWindowModality.create modal |> Ok
            | false, _ -> Error $"not true or false: '{v.value}'"

        member this.configValue : AppSettings.ConfigValue =
            match this with
            | ModalSelectWindows -> AppSettings.ConfigValue "true"
            | ModelessSelectWindows -> AppSettings.ConfigValue "false"

    type QuickPickThreshold with
        static member tryCreate (v : AppSettings.ConfigValue) : Result<QuickPickThreshold, string> =
            match System.Int32.TryParse(v.value, System.Globalization.NumberStyles.Integer, System.Globalization.CultureInfo.InvariantCulture) with
            | true, count ->
                match QuickPickThreshold.tryCreate count with
                | Ok threshold -> Ok threshold
                | Error _ -> Error $"not a positive count: '{v.value}'"
            | false, _ -> Error $"not a whole number: '{v.value}'"

        member this.configValue : AppSettings.ConfigValue = AppSettings.ConfigValue $"{this.value}"

    type TreeAutoBuildThreshold with
        static member tryCreate (v : AppSettings.ConfigValue) : Result<TreeAutoBuildThreshold, string> =
            match System.Int32.TryParse(v.value, System.Globalization.NumberStyles.Integer, System.Globalization.CultureInfo.InvariantCulture) with
            | true, count ->
                match TreeAutoBuildThreshold.tryCreate count with
                | Ok threshold -> Ok threshold
                | Error _ -> Error $"not a positive count: '{v.value}'"
            | false, _ -> Error $"not a whole number: '{v.value}'"

        member this.configValue : AppSettings.ConfigValue = AppSettings.ConfigValue $"{this.value}"

    type ThicknessBucketCap with
        static member tryCreate (v : AppSettings.ConfigValue) : Result<ThicknessBucketCap, string> =
            match System.Int32.TryParse(v.value, System.Globalization.NumberStyles.Integer, System.Globalization.CultureInfo.InvariantCulture) with
            | true, count ->
                match ThicknessBucketCap.tryCreate count with
                | Ok cap -> Ok cap
                | Error _ -> Error $"not a positive count: '{v.value}'"
            | false, _ -> Error $"not a whole number: '{v.value}'"

        member this.configValue : AppSettings.ConfigValue = AppSettings.ConfigValue $"{this.value}"

/// Read workbench settings and data locations once for Startup; downstream consumers receive elevated values.
module AppConfig =

    /// One key through its type's door: a value the door refuses is named in the log before the
    /// provider writes the default back over it, as a missing key's is (spec 0046, 0048 E042).
    let private readSetting<'Setting, 'Rejection>
        (provider : AppSettings.AppSettingsProvider)
        (key : AppSettings.ConfigKey)
        (defaultValue : 'Setting)
        (tryParse : AppSettings.ConfigValue -> Result<'Setting, 'Rejection>)
        (render : 'Setting -> AppSettings.ConfigValue)
        : 'Setting =
        match provider.tryGet key tryParse with
        | Error e -> Logger.logWarn $"loadWorkbenchSettings: '{key.value}' is not usable ({e.Message}); using the built-in default."
        | Ok _ -> ()

        provider.getOrDefault key defaultValue tryParse render

    [<ElevationBoundary(nameof IoBoundary, "Resolve configured data locations through the process environment at the composition root.")>]
    let private resolveLocations (install : FolderName) (configured : DataLocations) : DataLocations =
        let resolveFolder (folder : FolderName) : FolderName =
            let expanded = FolderName (Environment.ExpandEnvironmentVariables folder.value)
            if Path.IsPathRooted expanded.value then expanded
            else install.combine expanded

        {
            libraryConnection = ConnectionString (Environment.ExpandEnvironmentVariables configured.libraryConnection.value)
            backupsFolder = resolveFolder configured.backupsFolder
            preferencesFolder = resolveFolder configured.preferencesFolder
            kitsFolder = resolveFolder configured.kitsFolder
            exportsFolder = resolveFolder configured.exportsFolder
            cataloguesFolder = resolveFolder configured.cataloguesFolder
            shippedCatalogueFolder = resolveFolder configured.shippedCatalogueFolder
        }

    /// Read each location, writing missing or refused values back as configured defaults (spec 0050 S2.4).
    /// Returned locations expand environment variables and root relative folders at install.
    /// The caller saves the provider after all settings have been read.
    let readDataLocations (provider : AppSettings.AppSettingsProvider) (install : FolderName) (documents : FolderName) : DataLocations =
        let defaults = DataLocations.defaultsUnder documents
        let connectionKey = AppSettings.ConfigKey ConfigKey.sqliteConnectionString
        let libraryConnection =
            match provider.tryGetConnectionString connectionKey with
            | Ok (Some connection) -> connection
            | missingOrRefused ->
                match missingOrRefused with
                | Error error -> Logger.logWarn $"readDataLocations: '{connectionKey.value}' is not usable ({error.Message}); using the built-in default."
                | Ok _ -> ()

                match provider.trySetConnectionString connectionKey defaults.libraryConnection with
                | Error error -> Logger.logWarn $"readDataLocations: could not write the default for '{connectionKey.value}' back ({error.Message})."
                | Ok () -> ()

                defaults.libraryConnection

        let configured =
            {
                libraryConnection = libraryConnection
                backupsFolder = readSetting provider (AppSettings.ConfigKey ConfigKey.libraryBackupsFolder) defaults.backupsFolder FolderName.tryCreate _.configValue
                preferencesFolder = readSetting provider (AppSettings.ConfigKey ConfigKey.preferencesFolder) defaults.preferencesFolder FolderName.tryCreate _.configValue
                kitsFolder = readSetting provider (AppSettings.ConfigKey ConfigKey.kitsFolder) defaults.kitsFolder FolderName.tryCreate _.configValue
                exportsFolder = readSetting provider (AppSettings.ConfigKey ConfigKey.exportsFolder) defaults.exportsFolder FolderName.tryCreate _.configValue
                cataloguesFolder = readSetting provider (AppSettings.ConfigKey ConfigKey.cataloguesFolder) defaults.cataloguesFolder FolderName.tryCreate _.configValue
                shippedCatalogueFolder = readSetting provider (AppSettings.ConfigKey ConfigKey.shippedCatalogueFolder) defaults.shippedCatalogueFolder FolderName.tryCreate _.configValue
            }

        resolveLocations install configured

    /// Read workbench settings and resolved data locations through one provider, then save defaults once.
    /// An unreadable file logs its error and returns built-in settings and resolved default locations.
    let loadWorkbenchSettings () : WorkbenchSettings * DataLocations =
        let defaults = WorkbenchSettings.defaults
        let install = FolderName System.AppContext.BaseDirectory
        let documents = FolderName (Environment.GetFolderPath Environment.SpecialFolder.MyDocuments)

        match AppSettings.AppSettingsProvider.tryCreate () with
        | Ok provider ->
            let settings =
                {
                    selectWindowModality = readSetting provider (AppSettings.ConfigKey ConfigKey.selectWindowsModal) defaults.selectWindowModality SelectWindowModality.tryCreate _.configValue
                    quickPickThreshold = readSetting provider (AppSettings.ConfigKey ConfigKey.quickPickThreshold) defaults.quickPickThreshold QuickPickThreshold.tryCreate _.configValue
                    treeAutoBuildThreshold = readSetting provider (AppSettings.ConfigKey ConfigKey.treeAutoBuildThreshold) defaults.treeAutoBuildThreshold TreeAutoBuildThreshold.tryCreate _.configValue
                    thicknessBucketCap = readSetting provider (AppSettings.ConfigKey ConfigKey.thicknessBucketCap) defaults.thicknessBucketCap ThicknessBucketCap.tryCreate _.configValue
                }
            let locations = readDataLocations provider install documents

            match provider.trySave () with
            | Ok () -> ()
            | Error e -> Logger.logWarn $"loadWorkbenchSettings: could not write appsettings.json defaults back - %A{e}."

            settings, locations
        | Error e ->
            Logger.logError $"loadWorkbenchSettings: cannot open appsettings.json - %A{e}; using built-in defaults."
            defaults, resolveLocations install (DataLocations.defaultsUnder documents)
