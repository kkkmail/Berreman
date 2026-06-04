# 001 impl-log — Root MVU host + shell skeleton + ui-smoke gate

## Progress

- [x] R-1 Elmish host packages (Ui + App fsproj) — `Avalonia.FuncUI.Elmish` 1.6.0 + `Elmish` 4.3.0
- [x] R-2 `Shell.fs` root `Page`/`RootModel`/`RootMsg`/`init`/`initFrom`/`update`
- [x] R-3 Elmish mount from `MainWindow` (seeded from `Startup.settings`)
- [x] R-4 `Shell.view` panel-id dispatch over `AppShell` reducers + `toDock`
- [x] R-5 stack panel read-only (`ConstructionView.fs`, `StackEditor.layerRowLabels`)
- [x] R-6 `ui-smoke` gate (descriptor already present; command verified)
- [x] R-7 headless UI test project + smoke test + per-panel view test; registered in slnx
- [x] R-8 4 non-stack panels render titled placeholders headlessly
- [x] Gates green (build, unit-tests, constructor-unit-tests, ui-smoke, ui-tests)

## Files modified

New:
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/Shell.fs` — root MVU surface + view.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/ConstructionView.fs` — read-only stack panel.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/OpticalConstructor.Ui.Tests.fsproj`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/TestApp.fs` — headless session driver;
  `BuildAvaloniaApp` points at the real `OpticalConstructor.App.App` (attempt 03).
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/SmokeTests.fs` — `ui-smoke` test; drives
  the real `App` lifetime (Initialize theme seam + OnFrameworkInitializationCompleted) (attempt 03).
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/PanelViewTests.fs` — `ui-tests` view test.

Edited:
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/AppShell.fs` — removed superseded
  `shellView`/`panelView`; made `toDock` public.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/OpticalConstructor.Ui.fsproj` — Elmish
  packages + registered `ConstructionView.fs`, `Shell.fs`.
- `Berreman/OpticalConstructor/OpticalConstructor.App/Program.fs` — Elmish mount in `MainWindow`.
- `Berreman/OpticalConstructor/OpticalConstructor.App/OpticalConstructor.App.fsproj` — Elmish packages.
- `Berreman/Berreman.slnx` — registered `OpticalConstructor.Ui.Tests`.

Not touched (already correct): `.gates/OpticalConstructor/ui-smoke.gates`,
`.gates/OpticalConstructor/ui-tests.gates` — present and matching the slice `.gates` snapshot.

## Decisions

- **Attempt 03 (retry) — drive the real App lifetime in `ui-smoke`.** Replaced the direct
  `MainWindow()` construction in `SmokeTests.fs` with a path that exercises the REAL `App`
  lifetime headlessly (AC-U1.3 / §U1.7). Two edits, both in the test project only:
  1. `TestApp.fs`: `TestAppBuilder.BuildAvaloniaApp` now `AppBuilder.Configure<App>()` against
     `OpticalConstructor.App.App` (was a stand-in `TestApp`). The shared `HeadlessUnitTestSession`
     therefore builds and `Initialize()`s the real App, so `App.Initialize`'s theme seam
     (`AppShell.themeVariant`, Program.fs:52-54) runs on `Application.Current`. The standalone
     `TestApp` type + its `open Avalonia.Themes.Fluent` were dropped (real `App.Initialize`
     already loads the Fluent theme), so nothing else is bypassed.
  2. `SmokeTests.fs`: assert the seam ran via `(Application.Current :?> App).RequestedThemeVariant
     = AppShell.themeVariant settings.theme`, then attach a `ClassicDesktopStyleApplicationLifetime`
     to a fresh `App` and call `App.OnFrameworkInitializationCompleted()` — running its real
     desktop branch, which constructs + opens `MainWindow` (mounting the Elmish `Shell`). Assert
     `desktop.MainWindow` is set, shown, and visible. The per-page render-one-frame assertions
     (Construction + SynthesisFit via `Shell.view`) and the `PanelViewTests` per-panel test are
     unchanged. The R-2 dual construction/workspace seed and the inherited `ui-tests.gates`
     description were NOT touched.
  - **Why a fresh App instead of the session app:** the session app's `ApplicationLifetime` is
    `null` and Avalonia refuses to set it after AppBuilder initialization ("not possible to change
    ApplicationLifetime after Application was initialized"). A fresh, not-yet-initialized `App`
    accepts a lifetime, so framework-init can target the classic-desktop branch. A second full
    `AppBuilder.Setup` inside the session was avoided — it would conflict with the already
    initialized global headless platform. Recorded in the SoW `Gotchas`.
- **Attempt 02 (retry):** Seed the model through the existing `Templates.bandpassFilter ()`
  factory rather than an inline private `initialSystem`/`initialNode`/`initialProject` literal
  triple (was `Shell.fs:93-120`). This reuses the canonical `Templates` scaffold — its films
  AND its single `Templates.defaultLight` (550 nm) source — instead of re-deriving them inline,
  resolving the cycle-1 review's reuse finding and the 600→550 nm incident-light drift in the
  same edit (the inline `initialNode` carried `WaveLength.nm 600.0<nm>`). `bandpassFilter` is a
  5-layer stack, so the read path still renders real "Layer N — ..." rows (R-5 / AC-U1.2) and
  the `PanelViewTests` `layerRows.Length >= 2` assertion holds. R-2's dual project seed is
  preserved: both `construction` (`ConstructionPage.init`) and `workspace` (`Workspace.init`)
  are seeded from the one factory result. `ConstructionPage.init` solves the whole tree once,
  so the stack must be engine-solvable; the bandpass H/L stack over a glass plate is.
  The six `open`s that fed only the removed inline literal (`Berreman.Constants`/`Fields`/
  `MaterialProperties`/`Media`, `OpticalConstructor.Domain.Units`/`.BeamTree`) were dropped
  with it; the remaining opens still resolve every symbol (build re-verified green).
- `RootMsg` adds exactly `Construction`, `Workspace`, `Shell` (R-2 minimum). `Fit`/`Source`/
  `Chart`/`Io` are deferred to the parts that wire them.
- Default working folder = the user's Documents `OpticalConstructor` (never the repo root,
  §0.6); U1 wires no save path, so nothing writes there.

## Testing state

`commit_ready: true`. Every R-1..R-8 requirement is addressed in this round (no deferral
to a "round 2"). Local gate run (captured in `.artifacts/001-gate-results.txt`):

- build: succeeded, 0 errors.
- unit-tests (BerremanTests): 84 passed / 5 skipped. Baseline `berreman_unit_tests = 84`.
- constructor-unit-tests: 204 passed; stays Avalonia-free.
- ui-smoke: 1 passed (real `App.Initialize` theme seam + `App.OnFrameworkInitializationCompleted`
  run; the desktop lifetime's `MainWindow` is set + visible; every panel/page renders one frame).
- ui-tests: 1 passed (stack panel renders the layer rows).

## Artifacts

- `C:\GitHub\Berreman\specs\0024\.artifacts\001-gate-results.txt` — gate-run capture.

## Gotchas

- **`Avalonia.Headless.XUnit` 11.3.4 binds to xUnit v2** (`xunit.core` 2.4.0); its
  `[<AvaloniaFact>]` is invisible to the xunit.v3 runner. §U1.7 wanting *both* the
  `.XUnit` helper *and* xunit.v3 is unsatisfiable at these versions. Chose xunit.v3 (the
  stated framework match) + `Avalonia.Headless`, marshalling UI work onto the headless
  thread via the public `HeadlessUnitTestSession.GetOrStartForAssembly`/`Dispatch` API
  directly — what `[<AvaloniaFact>]` does internally. Recorded here per the "don't ask
  the user" rule; an operator may revisit if they want the helper attribute specifically.
- `Shell.init` is a value (R-2) but `Program.mkProgram` needs `unit -> _` and R-3 mandates
  seeding env from `Startup.settings`; reconciled with `Shell.initFrom` + a `(fun () -> …)`
  wrapper at the mount. The parameterless `Shell.init` value is what the tests mount.
- DU case/module name clashes (`Page.Construction` vs `RootMsg.Construction`; the
  `Workspace` module vs `RootMsg.Workspace` case) are resolved by type-qualifying case
  usages and aliasing the module as `WS`.
- `ConstructionView.fs` needed `open Avalonia.Controls` for the FuncUI DSL property setters.
- **(attempt 03) A headless-session app's `ApplicationLifetime` is fixed.** The
  `HeadlessUnitTestSession` leaves it `null` and Avalonia's setter throws once the AppBuilder
  has initialized the app, so the `ui-smoke` test cannot swap a classic-desktop lifetime onto
  `Application.Current`. It drives framework-init on a fresh, not-yet-initialized `App` (lifetime
  settable) on the session UI thread; the session app still proves the theme seam ran via
  `Application.Current.RequestedThemeVariant`. Verified empirically (probed the session lifetime
  = NULL and the setter exception) before settling on this shape.
