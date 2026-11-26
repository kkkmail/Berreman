# Step 0082 implementation plan

Add `AppConfig.readDataLocations` using the existing `readSetting` helper and STL's folder settings door. Keep the configured defaults from `DataLocations.defaultsUnder`, handle connection-string read/write failures with warnings, and expand environment variables and root relative folders at the composition boundary. Leave `loadWorkbenchSettings` and its callers unchanged.

Add the four exactly named `AppConfigTests` facts before production changes. Use real settings providers over isolated files below the test output, verify persisted configured text independently from returned locations, and serialize the output-settings read through `HeadlessSession.run`. Add the test project's own `appsettings.json` as copied content and compile the new module immediately after `LauncherTests.fs`.

Authorized product files are `OpticalConstructor.App/AppConfig.fs` and `OpticalConstructor.Ui.Tests/{AppConfigTests.fs,appsettings.json,OpticalConstructor.Ui.Tests.fsproj}`. Required worker outputs use the supplied paths. Risks are settings-extension resolution, the test copy overriding transitive app content, and process environment isolation; use a unique variable restored in `finally` and never create any resolved data folder.

The supervisor owns the build, elevation, ui-smoke, selected ui-tests, and eleven s11 acceptance invariants. Higher-priority development instructions require appropriate local verification; record those observations separately from supervisor acceptance, without authoring baselines or claiming gate results.
