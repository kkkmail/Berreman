# Step 0195 — impl plan

## Approach

Wire the Launcher's footer to step 194's `DataLocationsWindow` (001 §O, §G; .spec-md §10.1).

1. `OpticalConstructor.Controls/UiIds.fs`: in the launcher group, add `launcherDataLocationsLink`
   (`"LauncherDataLocationsLink"`) with a one-line doc.
2. `OpticalConstructor.App/Program.fs`, `LauncherWindow`:
   - build one `WindowLauncher.create` over a factory that builds `DataLocationsWindow(locations, ExplorerProxy.system)`,
     `settings.selectWindowModality` and `BrowseOpen`, as `KitConstructorWindows.showConstructorIn` does;
   - inside `launcherContent` (so it is in every outcome's content), build the link-styled `Button`: transparent
     background, no border, an underlined `TextBlock` content, font size 11, `Name` and `AutomationId` the new id. Its
     click opens `DataLocationsWindowKey`. A failure is logged with its reason, as the kit window's open is;
   - the footer becomes a bottom-docked `DockPanel` row: the link docked left, the unchanged version line filling the
     rest (right-aligned, as before);
   - the `LauncherWindow` doc names the link.
3. `OpticalConstructor.Ui.Tests/LauncherTests.fs`: the two declared `ui-smoke` facts.

## Risk: existing facts that enumerate every launcher `Button`

The link is a `Button`, so three existing un-traited `LauncherTests` facts (run by `ui-tests`) would count it:

- `the launcher hosts Main / Inverse / Materials / Library in that order by stable name` and
  `a launcher with no pending import shows no import notice` assert that the launcher's buttons are exactly the four;
- `the launcher footer shows the app version from BuildInfo, below the buttons` asserts that the version sits below
  every button, and the link shares its row.

The acceptance requires `ui-tests` over `LauncherTests` to pass, and `LauncherTests.fs` is an authorized edit. These
facts get a `surfaceButtons` helper: every launcher button except the footer link, which is excluded by its id alone.
Every assertion stays, and no identity changes. The "no other button" property still holds, because only the link's
id is set aside.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Controls/UiIds.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.App/Program.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/LauncherTests.fs`
- `WireUiFinalCompositionTests.fs` is authorized but needs no change. Its launcher render fact already builds the
  launcher over `TestAppScope.testLocations owner`.
