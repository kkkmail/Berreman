# Step 0195 — impl log

## Progress

- Read the protocol, the WIRE_UI delta, the project prompt, the slice and the gate snapshot. Manual 013 applies (elevation
  from the arc origin). No other manual ruling names step 0195, and no operator note is in flight.
- Read step 0194's impl-log and state of the world. `DataLocationsWindow(locations, explorer)` and
  `WindowKey.DataLocationsWindowKey` exist. `ExplorerProxy.system` is Ui's extension in `KitOpening.fs`, which
  `Program.fs` already opens.
- Found three existing un-traited `LauncherTests` facts (run by `ui-tests`) that enumerate every launcher `Button`. A
  link-styled `Button` would break them (Gotchas). The plan's `surfaceButtons` helper was chosen for them.
- Wrote the id, the footer link and its single-instance open, and the two declared facts. The facts were red by
  construction, because they name `launcherDataLocationsLink`, which this step creates.
- First build: clean, 0 errors, and no warning from our code. Every test run passed on the first tree (Testing state).
- Elevation from the arc origin: `ELEV-GATE: OK`, 0 new findings. No repair was needed.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Controls/UiIds.fs`: the launcher group gains
  `launcherDataLocationsLink` (`"LauncherDataLocationsLink"`) after `launcherReviewImportsButton`, with a one-line doc
  citing 001 §O, §G and S5.3.
- `Berreman/OpticalConstructor/OpticalConstructor.App/Program.fs`, `LauncherWindow`:
  - The doc names the link in one clause: a library-open failure disables every surface action, "never the footer's
    Data locations link". It now cites S5.3.
  - Once per launcher, `dataLocationsLauncher` is `WindowLauncher.create` over a factory that builds
    `DataLocationsWindow(locations, ExplorerProxy.system)`, `settings.selectWindowModality` and `BrowseOpen`. The
    factory ignores the key, as `KitConstructorWindows.showConstructorIn`'s does.
  - `openDataLocations ()` opens or activates `DataLocationsWindowKey`. A factory failure is logged with its reason,
    and the unreachable `WindowNotRegisteredErr` is logged too, as the kit window's open does.
  - Inside `launcherContent`, so it is in every outcome's content (the Recreate rebuild included), the link is a
    `Button`:
    - `Name` and `AutomationId` are `launcherDataLocationsLink`, and the automation name is `Data locations`;
    - its content is a `TextBlock` reading `Data locations` with `TextDecorations.Underline`;
    - transparent background, border thickness 0, padding 0, font size 11, centred vertically;
    - margin (20, 0, 0, 10), matching the version line's left and bottom margins.
    It is never disabled, and its click is `openDataLocations ()`.
  - The footer is now a bottom-docked `DockPanel` row: the link docked left, then the unchanged version `TextBlock`
    filling the rest (still right-aligned, same margin). The version's own `DockPanel.SetDock` is gone, because it is
    the row's filling last child now. The form adds `footer` where it added `version`.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/LauncherTests.fs`:
  - new private helper `surfaceButtons`: every launcher button but the one named `launcherDataLocationsLink`;
  - `the launcher hosts Main / Inverse / Materials / Library in that order by stable name`,
    `a launcher with no pending import shows no import notice` and
    `the launcher footer shows the app version from BuildInfo, below the buttons` iterate `surfaceButtons` instead of
    `buttons`. Every assertion is kept, and only the link's id is set aside (Gotchas);
  - new `ui-smoke` fact `acceptance: the Data locations link opens one Data locations window listing the configured
    locations` (after the Library button fact):
    - the link, found by AutomationId, is a `Button` named by the id, with automation name `Data locations`;
    - raising `Click` on it (`clickAndObserve`, as the file's launcher facts click) opens exactly one
      `DataLocationsWindow`, which is shown;
    - its `dataLocationPathText KitsLocation` reads `(TestAppScope.testLocations owner).kitsFolder.value`;
    - a second click opens no window, and the first window stays shown;
    - every window the link opened is closed in a `finally`, and the close is pumped;
  - new `ui-smoke` fact `the Data locations link stays enabled when the library fails to open`: a launcher over
    `Error (LibrarySchemaMismatchErr …)`, built as step 88's fact builds it. The link is `IsEnabled` and
    `IsEffectivelyEnabled`, and the four surface buttons are disabled. No fact presses Open folder through the
    launcher.
- `WireUiFinalCompositionTests.fs` (authorized) is unchanged. Its launcher render fact builds the launcher over
  `TestAppScope.testLocations owner` and renders the new footer as it is.

## Testing state

These are development runs only. Under Invariant 6, the gate verdict belongs to the arc-runner's gate engine. Logs
are under `specs/0050/.artifacts/`, and the three TRX reports under `specs/0050/.artifacts/0195-testresults/`.

- **Build:** `dotnet build Berreman.slnx -c Release -nologo -v:m` exits 0, with 0 errors (`0195-build.log`). Its only
  warnings are the two `SYSLIB0051` in the vendored `MathNetNumerics/Numerics/Exceptions.cs`, as in `0194-build.log`.
  Nothing this step edits warns.
- **constructor-unit-tests:** 1215 passed, 0 failed (`0195-constructor-unit-tests.log`). That is checkpoint 193's 1215.
- **ui-smoke:** 236 passed, 0 failed (`0195-ui-smoke.log`). That is checkpoint 193's 234 + 2, the two declared facts.
  The TRX lists both identities as Passed.
- **ui-tests over `OpticalConstructor.Ui.Tests.LauncherTests`:** 8 passed, 0 failed (`0195-ui-tests.log`). The three
  facts moved to `surfaceButtons` are among them.
- **Elevation**, run with `ASG_ARC_BASE_COMMIT=f395597…` (`.arc-base-json`): `ELEV-GATE: OK`, 0 new findings in 12
  of 12 projects, ELEV004 excluded, 0 known findings (`0195-elevation.log`).
- **The fourteen s21 invariant greps:** none finds anything.
- **Line endings:** no CR and no BOM in any touched file. `git diff --numstat` matches `--ignore-cr-at-eol`.

## Artifacts

- `specs/0050/.artifacts/0195-build.log`
- `specs/0050/.artifacts/0195-constructor-unit-tests.log`, `0195-ui-smoke.log`, `0195-ui-tests.log`
- `specs/0050/.artifacts/0195-testresults/` (the three TRX reports)
- `specs/0050/.artifacts/0195-elevation.log`

## Gotchas

- **Interpretation: three existing structure facts set the link aside.** The slice requires a link-styled `Button`.
  `LauncherTests.buttons` collects every `Button` in the launcher's visual tree, so the link would have appeared in
  two facts that pin the button list to exactly the four surface buttons. It would also have sat beside the version
  line, which a third fact requires to be below every button. The acceptance requires `ui-tests` over `LauncherTests`
  to pass, and `LauncherTests.fs` is an authorized edit. So these three facts use `surfaceButtons`:
  - only the id `launcherDataLocationsLink` is filtered out, so any other new button still fails them;
  - no assertion is removed, and no identity is renamed or added beyond the two declared.
  This changes those facts' expectations, but no test census entry records it. It is recorded here as the deviation.
- **The link lives in `launcherContent`**, so the Recreate rebuild and both outcomes carry it. The launcher record and
  `openDataLocations` are built once, before `launcherContent`.
- **The registry is app-global.** The acceptance fact closes every window the link opened and pumps the close.
  Otherwise `WireUiFinalCompositionTests`' root-factory sweep would get `ActivatedWindow` for `DataLocationsWindowKey`.
- **The YAML carries the latest recorded checkpoint floors** (checkpoint 193), as step 0194's did. The supervisor
  measures this step's own: here, 1215 and 236.
- `specs/0050/.manifest.state.json` was already modified by the supervisor before this worker started, and it was not
  touched.
