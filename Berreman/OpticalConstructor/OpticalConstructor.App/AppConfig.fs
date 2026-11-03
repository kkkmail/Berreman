/// Spec 0038 Part C (step 005): the appsettings.json seam of the product app.
/// `ConfigKey` is the single key inventory; `AppConfig.loadWorkbenchSettings` is the
/// ONE place the solution touches `Softellect.Sys.AppSettings.AppSettingsProvider`
/// for the app configuration — created once at the composition root, values flowing
/// inward only as the elevated `WorkbenchSettings` Domain record. User preferences
/// stay in `environment.json` (`Ui/UserEnvironment.fs`); the two stores never merge.
namespace OpticalConstructor.App

open Softellect.Sys
open Softellect.Sys.Logging

open OpticalConstructor.Domain.WorkbenchSettings

/// The single appsettings.json key inventory (§6): every key the app — and the
/// Part M storage proof (step 040) — reads is a `[<Literal>]` HERE, never a string
/// scattered at a call site. The literal values are the wire key names inside the
/// file's `appSettings` / `connectionStrings` sections. (Softellect's `ConfigKey`
/// single-case DU wraps these at the provider call; `Softellect.Sys.AppSettings` is
/// deliberately NOT opened in this file so the unqualified `ConfigKey` name can only
/// mean this module.)
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

    /// appSettings string (Part M, consumed by step 040's EFC proof): the database
    /// provider choice, "MSSQL" | "SQLite" — parsed to a DU on the F# side of the
    /// step-040 test, never a naked string beyond that boundary.
    [<Literal>]
    let databaseProvider = "DatabaseProvider"

    /// connectionStrings entry (Part M, step 040): the MSSQL connection string,
    /// read via `AppSettingsProvider.tryGetConnectionString`.
    [<Literal>]
    let mssqlConnectionString = "OpticalConstructorMssql"

    /// connectionStrings entry (Part M, step 040): the SQLite connection string,
    /// read via `AppSettingsProvider.tryGetConnectionString`.
    [<Literal>]
    let sqliteConnectionString = "OpticalConstructorSqlite"

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

/// The one `AppSettingsProvider` read seam. `loadWorkbenchSettings` is called once,
/// from `Program.Startup`, and everything downstream sees only the returned record.
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

    /// Create the provider ONCE via `AppSettingsProvider.tryCreate ()` (it opens the
    /// build-copied appsettings.json beside the assembly), read the four workbench
    /// keys — the provider's `WriteDefaultOnMissing` writes each missing or refused key's default back,
    /// so the first run self-documents — persist the write-backs, and return the
    /// elevated Domain record. Total: a missing/unreadable file logs a typed error and
    /// falls back to `WorkbenchSettings.defaults`.
    let loadWorkbenchSettings () : WorkbenchSettings =
        let defaults = WorkbenchSettings.defaults

        match AppSettings.AppSettingsProvider.tryCreate () with
        | Ok provider ->
            let settings =
                {
                    selectWindowModality = readSetting provider (AppSettings.ConfigKey ConfigKey.selectWindowsModal) defaults.selectWindowModality SelectWindowModality.tryCreate _.configValue
                    quickPickThreshold = readSetting provider (AppSettings.ConfigKey ConfigKey.quickPickThreshold) defaults.quickPickThreshold QuickPickThreshold.tryCreate _.configValue
                    treeAutoBuildThreshold = readSetting provider (AppSettings.ConfigKey ConfigKey.treeAutoBuildThreshold) defaults.treeAutoBuildThreshold TreeAutoBuildThreshold.tryCreate _.configValue
                    thicknessBucketCap = readSetting provider (AppSettings.ConfigKey ConfigKey.thicknessBucketCap) defaults.thicknessBucketCap ThicknessBucketCap.tryCreate _.configValue
                }

            match provider.trySave () with
            | Ok () -> ()
            | Error e -> Logger.logWarn $"loadWorkbenchSettings: could not write appsettings.json defaults back - %A{e}."

            settings
        | Error e ->
            Logger.logError $"loadWorkbenchSettings: cannot open appsettings.json - %A{e}; using built-in defaults."
            defaults
