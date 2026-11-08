namespace OpticalConstructor.Ui.Tests

open System
open System.IO
open System.Text.Json
open Xunit
open Softellect.Sys
open Softellect.Sys.Primitives
open OpticalConstructor.Domain.WorkbenchSettings
open OpticalConstructor.App

module AppConfigTests =

    type private AppConfigTestContext =
        {
            settingsFile : FileName
            install : FolderName
            documents : FolderName
            provider : AppSettings.AppSettingsProvider
        }

    let private expectOk (result : Result<'Value, 'Error>) : 'Value =
        match result with
        | Ok value -> value
        | Error error -> failwith $"Expected settings operation to succeed: {error}"

    let private createSettingsContext (json : JsonText) : AppConfigTestContext =
        let folder = FolderName (Path.Combine(System.AppContext.BaseDirectory, "AppConfigTests", $"{Guid.NewGuid():N}"))
        let settingsFile = FileName (Path.Combine(folder.value, "appsettings.json"))
        let install = FolderName (Path.Combine(folder.value, "Install"))
        let documents = FolderName (Path.Combine(folder.value, "Documents"))
        Directory.CreateDirectory(folder.value) |> ignore
        File.WriteAllText(settingsFile.value, json.value)

        {
            settingsFile = settingsFile
            install = install
            documents = documents
            provider = AppSettings.AppSettingsProvider.tryCreate settingsFile |> expectOk
        }

    let private assertSavedLocations (context : AppConfigTestContext) (expected : DataLocations) : unit =
        context.provider.trySave () |> expectOk
        use json = JsonDocument.Parse(File.ReadAllText context.settingsFile.value)
        let settings = json.RootElement.GetProperty("appSettings")
        let connections = json.RootElement.GetProperty("connectionStrings")
        Assert.Equal(expected.libraryConnection.value, connections.GetProperty(ConfigKey.sqliteConnectionString).GetString())
        Assert.Equal(expected.backupsFolder.value, settings.GetProperty(ConfigKey.libraryBackupsFolder).GetString())
        Assert.Equal(expected.preferencesFolder.value, settings.GetProperty(ConfigKey.preferencesFolder).GetString())
        Assert.Equal(expected.kitsFolder.value, settings.GetProperty(ConfigKey.kitsFolder).GetString())
        Assert.Equal(expected.exportsFolder.value, settings.GetProperty(ConfigKey.exportsFolder).GetString())
        Assert.Equal(expected.cataloguesFolder.value, settings.GetProperty(ConfigKey.cataloguesFolder).GetString())
        Assert.Equal(expected.shippedCatalogueFolder.value, settings.GetProperty(ConfigKey.shippedCatalogueFolder).GetString())

    [<Fact>]
    let ``readDataLocations writes every missing location back as its default and answers it expanded`` () : unit =
        let json = JsonText """{ "appSettings": {}, "connectionStrings": {} }"""
        let context = createSettingsContext json
        let defaults = DataLocations.defaultsUnder context.documents
        let locations = AppConfig.readDataLocations context.provider context.install context.documents
        let expected : DataLocations =
            {
                libraryConnection = ConnectionString (Environment.ExpandEnvironmentVariables defaults.libraryConnection.value)
                backupsFolder = FolderName (Environment.ExpandEnvironmentVariables defaults.backupsFolder.value)
                preferencesFolder = FolderName (Environment.ExpandEnvironmentVariables defaults.preferencesFolder.value)
                kitsFolder = FolderName (Environment.ExpandEnvironmentVariables defaults.kitsFolder.value)
                exportsFolder = FolderName (Environment.ExpandEnvironmentVariables defaults.exportsFolder.value)
                cataloguesFolder = FolderName (Environment.ExpandEnvironmentVariables defaults.cataloguesFolder.value)
                shippedCatalogueFolder = context.install.combine DataLocations.shippedCatalogueDefault
            }

        Assert.Equal<DataLocations>(expected, locations)
        use beforeSave = JsonDocument.Parse(File.ReadAllText context.settingsFile.value)
        Assert.Empty(beforeSave.RootElement.GetProperty("appSettings").EnumerateObject())
        Assert.Empty(beforeSave.RootElement.GetProperty("connectionStrings").EnumerateObject())
        assertSavedLocations context defaults
        Assert.Contains("%LOCALAPPDATA%", defaults.libraryConnection.value)
        Assert.Contains("%LOCALAPPDATA%", defaults.backupsFolder.value)
        Assert.Contains("%APPDATA%", defaults.preferencesFolder.value)

    [<Fact>]
    let ``readDataLocations keeps a configured location, expands its variables and roots a relative folder at the install folder`` () : unit =
        let json = JsonText """{ "appSettings": {}, "connectionStrings": {} }"""
        let context = createSettingsContext json
        let variable = $"BERREMAN_APP_CONFIG_TEST_{Guid.NewGuid():N}"
        let variableFolder = FolderName (Path.Combine(context.install.value, "Configured"))
        let connection = ConnectionString $"Data Source=%%{variable}%%/lib.db"
        let backups = FolderName $"%%{variable}%%/Backups"
        let kits = FolderName "Kits"
        let configuredJson =
            dict
                [ "connectionStrings", dict [ ConfigKey.sqliteConnectionString, connection.value ]
                  "appSettings", dict [ ConfigKey.libraryBackupsFolder, backups.value; ConfigKey.kitsFolder, kits.value ] ]
            |> JsonSerializer.Serialize
            |> JsonText
        File.WriteAllText(context.settingsFile.value, configuredJson.value)
        let configuredContext =
            {
                context with
                    provider = AppSettings.AppSettingsProvider.tryCreate context.settingsFile |> expectOk
            }

        Environment.SetEnvironmentVariable(variable, variableFolder.value)
        try
            let locations = AppConfig.readDataLocations configuredContext.provider context.install context.documents
            let expectedConnection = ConnectionString $"Data Source={variableFolder.value}/lib.db"
            let expectedBackups = FolderName $"{variableFolder.value}/Backups"
            Assert.Equal<ConnectionString>(expectedConnection, locations.libraryConnection)
            Assert.Equal<FolderName>(expectedBackups, locations.backupsFolder)
            Assert.Equal<FolderName>(context.install.combine kits, locations.kitsFolder)
            let configured =
                {
                    DataLocations.defaultsUnder context.documents with
                        libraryConnection = connection
                        backupsFolder = backups
                        kitsFolder = kits
                }
            assertSavedLocations configuredContext configured
        finally
            Environment.SetEnvironmentVariable(variable, null)

    [<Fact>]
    let ``readDataLocations replaces a refused folder with its default and writes the default back`` () : unit =
        let json = JsonText """{ "appSettings": { "KitsFolder": "refused|folder" }, "connectionStrings": {} }"""
        let context = createSettingsContext json
        let refused = AppSettings.ConfigValue "refused|folder"
        match FolderName.tryCreate refused.value with
        | Error _ -> ()
        | Ok _ -> Assert.Fail("The fixture must exercise a refused folder.")
        let defaults = DataLocations.defaultsUnder context.documents
        let locations = AppConfig.readDataLocations context.provider context.install context.documents
        Assert.Equal<FolderName>(defaults.kitsFolder, locations.kitsFolder)
        assertSavedLocations context defaults

    [<Fact>]
    let ``the test run's appsettings.json keeps the library in memory and every folder under the test output`` () : unit =
        HeadlessSession.run (fun () ->
            let install = FolderName System.AppContext.BaseDirectory
            let documents = FolderName (Path.Combine(install.value, "UnusedDocuments"))
            let settingsFile = FileName (Path.Combine(install.value, "appsettings.json"))
            let provider = AppSettings.AppSettingsProvider.tryCreate settingsFile |> expectOk
            let locations = AppConfig.readDataLocations provider install documents
            let expected : DataLocations =
                {
                    libraryConnection = ConnectionString "Data Source=:memory:"
                    backupsFolder = FolderName (Path.Combine(install.value, "TestRun/Backups"))
                    preferencesFolder = FolderName (Path.Combine(install.value, "TestRun/Preferences"))
                    kitsFolder = FolderName (Path.Combine(install.value, "TestRun/Kits"))
                    exportsFolder = FolderName (Path.Combine(install.value, "TestRun/Exports"))
                    cataloguesFolder = FolderName (Path.Combine(install.value, "TestRun/Catalogues"))
                    shippedCatalogueFolder = FolderName (Path.Combine(install.value, "Catalogue"))
                }
            Assert.Equal<DataLocations>(expected, locations))

    [<Fact>]
    let ``the count doors read a positive whole number and refuse anything else, the kit history limit's included`` () : unit =
        let configured = AppSettings.ConfigValue "250"
        let zero = AppSettings.ConfigValue "0"
        let ten = AppSettings.ConfigValue "ten"
        let notPositive = "not a positive count: '0'"
        let notWhole = "not a whole number: 'ten'"

        match KitHistoryLimit.tryCreate configured with
        | Ok limit ->
            Assert.Equal(250, limit.value)
            Assert.Equal<AppSettings.ConfigValue>(configured, limit.configValue)
        | Error refusal -> Assert.Fail($"expected a kit history limit of 250, got '{refusal}'")

        Assert.Equal<Result<KitHistoryLimit, string>>(Error notPositive, KitHistoryLimit.tryCreate zero)
        Assert.Equal<Result<KitHistoryLimit, string>>(Error notWhole, KitHistoryLimit.tryCreate ten)
        Assert.Equal<Result<QuickPickThreshold, string>>(Error notPositive, QuickPickThreshold.tryCreate zero)
        Assert.Equal<Result<QuickPickThreshold, string>>(Error notWhole, QuickPickThreshold.tryCreate ten)
        Assert.Equal<Result<TreeAutoBuildThreshold, string>>(Error notPositive, TreeAutoBuildThreshold.tryCreate zero)
        Assert.Equal<Result<TreeAutoBuildThreshold, string>>(Error notWhole, TreeAutoBuildThreshold.tryCreate ten)
        Assert.Equal<Result<ThicknessBucketCap, string>>(Error notPositive, ThicknessBucketCap.tryCreate zero)
        Assert.Equal<Result<ThicknessBucketCap, string>>(Error notWhole, ThicknessBucketCap.tryCreate ten)
