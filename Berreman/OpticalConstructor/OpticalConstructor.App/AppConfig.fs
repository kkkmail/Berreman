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

/// The one `AppSettingsProvider` read seam. `loadWorkbenchSettings` is called once,
/// from `Program.Startup`, and everything downstream sees only the returned record.
module AppConfig =

    /// Elevate one raw threshold, falling back to the Domain default (and logging
    /// the typed rejection) when a hand-edited file carries a non-positive value —
    /// a bad settings file must never stop the app from starting.
    let private elevateOrDefault<'Setting>
        (name : string)
        (tryCreate : int -> Result<'Setting, WorkbenchSettingsError>)
        (defaultValue : 'Setting)
        (raw : int) : 'Setting =
        match tryCreate raw with
        | Ok v -> v
        | Error e ->
            Logger.logWarn $"loadWorkbenchSettings: '{name}' = {raw} rejected (%A{e}); using the built-in default."
            defaultValue

    /// Create the provider ONCE via `AppSettingsProvider.tryCreate ()` (it opens the
    /// build-copied appsettings.json beside the assembly), read the four workbench
    /// keys — `SetOnMissing = true` writes each missing key's default back, so the
    /// first run self-documents — persist the write-backs, and return the elevated
    /// Domain record. Total: a missing/unreadable file logs a typed error and falls
    /// back to `WorkbenchSettings.defaults`.
    let loadWorkbenchSettings () : WorkbenchSettings =
        match AppSettings.AppSettingsProvider.tryCreate () with
        | Ok provider ->
            let modal = provider.getBoolOrDefault (AppSettings.ConfigKey ConfigKey.selectWindowsModal) SelectWindowModality.defaultValue.value
            let quickPick = provider.getIntOrDefault (AppSettings.ConfigKey ConfigKey.quickPickThreshold) QuickPickThreshold.defaultValue.value
            let treeAutoBuild = provider.getIntOrDefault (AppSettings.ConfigKey ConfigKey.treeAutoBuildThreshold) TreeAutoBuildThreshold.defaultValue.value
            let bucketCap = provider.getIntOrDefault (AppSettings.ConfigKey ConfigKey.thicknessBucketCap) ThicknessBucketCap.defaultValue.value

            match provider.trySave () with
            | Ok () -> ()
            | Error e -> Logger.logWarn $"loadWorkbenchSettings: could not write appsettings.json defaults back - %A{e}."

            {
                selectWindowModality = SelectWindowModality.create modal
                quickPickThreshold = elevateOrDefault ConfigKey.quickPickThreshold QuickPickThreshold.tryCreate QuickPickThreshold.defaultValue quickPick
                treeAutoBuildThreshold = elevateOrDefault ConfigKey.treeAutoBuildThreshold TreeAutoBuildThreshold.tryCreate TreeAutoBuildThreshold.defaultValue treeAutoBuild
                thicknessBucketCap = elevateOrDefault ConfigKey.thicknessBucketCap ThicknessBucketCap.tryCreate ThicknessBucketCap.defaultValue bucketCap
            }
        | Error e ->
            Logger.logError $"loadWorkbenchSettings: cannot open appsettings.json - %A{e}; using built-in defaults."
            WorkbenchSettings.defaults
