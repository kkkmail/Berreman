# 001 state-of-the-world — Root MVU host + shell skeleton + ui-smoke gate

```yaml
gates:
  berreman_unit_tests: 84
  constructor_unit_tests: 204
  ui_smoke: 1
  ui_tests: 1
```

# Where we are

Slice 001 is the CRITICAL, gating foundation of the spec-0024 UI-wiring arc (Part
U1). It stands up the live Elmish MVU loop end-to-end — the Elmish host packages, the
root `Shell` model/message/dispatcher, the `MainWindow` mount, the panel-id-dispatching
root view, one real read-only panel, and the headless UI test project plus the
`ui-smoke`/`ui-tests` gates — so every later part (U2–U8) extends a proven pattern
rather than inventing one. Nothing downstream can land until this is in place.

# What's working

- Add `Avalonia.FuncUI.Elmish` 1.6.0 + `Elmish` 4.3.0 to the Ui and App projects.
- Author the root MVU surface in new `Shell.fs`: `Page`, `RootModel`, `RootMsg`
  (Construction/Workspace/Shell), and a pure `init`/`update` dispatcher with no effects.
- Mount the Elmish program from `MainWindow`, seeding env from `Startup.settings`.
- Rewrite the shell view to switch on panel id; render the `stack` panel read-only and
  the other four panels (and the Synthesis/Fit page) as titled placeholders.
- Stand up the headless `OpticalConstructor.Ui.Tests` project; add the `ui-smoke` smoke
  test and the `stack`-panel `ui-tests` view test; register it in `Berreman.slnx`.

# Tests

All gates in the slice roster pass locally:

- `build` — solution builds, the two new package refs resolve, `OpticalConstructor.Ui.Tests`
  is in the solution (AC-U1.4).
- `unit-tests` (BerremanTests) — 84 passed / 5 skipped; baseline `berreman_unit_tests = 84`.
- `constructor-unit-tests` — 204 passed; stays Avalonia-free (headless dep is isolated
  to the new UI test project).
- `ui-smoke` — 1 passed: the real `App` lifetime runs headlessly — `App.Initialize`'s
  theme seam executes (asserted via `Application.Current`'s `RequestedThemeVariant`) and
  `App.OnFrameworkInitializationCompleted` runs its desktop branch, which constructs +
  opens `MainWindow` (mounting the MVU program); every panel/page view renders one frame
  without throwing (AC-U1.3).
- `ui-tests` — 1 passed: the `stack`-panel view renders the selected node's layer rows
  (AC-U1.2).

# Architecture

- The proven surface U2–U8 extend: `RootModel`/`RootMsg`/`Shell.init`/`Shell.update`/
  `Shell.view` plus the panel-id dispatch in `Shell.view`. Later parts add only the
  `RootMsg` cases and effectful `Cmd`s they wire.
- `RootModel` is `[<ReferenceEquality>]`: the aggregated sub-models embed engine values
  (`EmFieldSystem`'s `Matrix<Complex>`, etc.) with no structural equality. The Elmish
  host only needs *an* equality to satisfy its generic constraint; reference equality
  suffices because FuncUI re-renders and diffs the view tree on every dispatch.
- Panel layout/visibility/dock route through the existing pure `AppShell` reducers
  (`visiblePanels`/`setPanelVisible`/`dockPanel`) and the now-public `AppShell.toDock`
  (public-Avalonia `DockPanel` only — §0.3). The superseded placeholder
  `AppShell.shellView`/`panelView` were removed.
- The headless test project drives the Avalonia UI thread through the public
  `HeadlessUnitTestSession` API directly (not `[<AvaloniaFact>]`) so it stays on
  xunit.v3 — see Gotchas.
- The `ui-smoke` test drives the **real** `App` lifetime rather than constructing
  `MainWindow` directly: `TestApp.fs`'s `TestAppBuilder.BuildAvaloniaApp` points the
  shared headless session at `OpticalConstructor.App.App`, so the session's
  `Application.Current` *is* the real App and `App.Initialize` (the theme seam) ran
  on it. To exercise `App.OnFrameworkInitializationCompleted`'s classic-desktop branch,
  the test attaches a `ClassicDesktopStyleApplicationLifetime` to a fresh `App` instance
  and invokes framework-init there — see Gotchas for why the session app's lifetime
  cannot be reused.
- The root model's seed project is built through the existing `Templates.bandpassFilter ()`
  factory (one reused scaffold + one `Templates.defaultLight` 550 nm source), not an inline
  literal — so there is a single seed-wavelength source of truth, and both the construction
  and workspace sub-models seed from the one factory result (R-2 dual seed preserved).

# Deferred

- The `stack` panel edit path (`EditStack`) — U2 / slice 003.
- Materials / sources / chart / results panel content — U3–U6 (titled placeholders now).
- The renderer-host adapters (`ChartHosts.fs`: AvaPlot / WebView2 / Canvas) — slice 002
  / Part U5.
- Effectful `Cmd`s (node solve, fit) and the `Fit`/`Source`/`Chart`/`Io` `RootMsg`
  cases — U2 / U7 / U8.
- The clone-swap decision (§0.2 / spec 0022 §A.9 / AC-A8) — untouched.

# Gotchas

- **`Avalonia.Headless.XUnit` 11.3.4 binds to xUnit v2** (`xunit.core` 2.4.0), so its
  `[<AvaloniaFact>]` is invisible to the xunit.v3 runner the sibling test project uses.
  §U1.7 asks for *both* `Avalonia.Headless.XUnit` *and* xunit.v3 — impossible at these
  versions. Resolution (recorded per the "don't ask" rule): keep xunit.v3 (the stated
  framework match) + `Avalonia.Headless`, and drive the UI thread through the public
  `HeadlessUnitTestSession` API directly — exactly what `[<AvaloniaFact>]` does
  internally. The `.XUnit` convenience package is the dropped detail, not the framework.
- **`Shell.init` is a value, but `Program.mkProgram` needs `unit -> _`.** R-2 fixes the
  type as `RootModel * Cmd<RootMsg>` (a value) and R-3 also mandates seeding env from
  `Startup.settings`. Reconciled with a sibling `Shell.initFrom : EnvironmentSettings ->
  RootModel * Cmd<RootMsg>`; the mount is `Program.mkProgram (fun () -> Shell.initFrom
  Startup.settings) …`, and the parameterless `Shell.init` value (defaults) is what the
  headless tests mount.
- `Page.Construction`/`RootMsg.Construction` and the `Workspace` module/`RootMsg.Workspace`
  case share names; usages are type-qualified (`Page.Construction`, `RootMsg.Construction`)
  and the workspace module is reached via a `WS = OpticalConstructor.Ui.Workspace` alias.
- `ConstructionView.fs` needed `open Avalonia.Controls` (as `AppShell` has) for the
  FuncUI DSL property setters (`TextBlock.text`, etc.) to resolve.
- The `.gates/OpticalConstructor/ui-smoke.gates` and `ui-tests.gates` descriptors
  already existed and match the slice `.gates` snapshot — not recreated.
- **A headless-session app's `ApplicationLifetime` is permanently fixed.** The
  `HeadlessUnitTestSession` sets it to `null` and Avalonia's setter throws
  ("not possible to change ApplicationLifetime after Application was initialized")
  once the AppBuilder has initialized the app — even to overwrite a null. So the
  `ui-smoke` test cannot swap a `ClassicDesktopStyleApplicationLifetime` onto the
  session's `Application.Current`; it instead drives framework-init on a fresh,
  not-yet-initialized `App` (whose lifetime *is* settable) on the session UI thread.
  The session app still proves the theme seam (`App.Initialize`) ran via
  `Application.Current.RequestedThemeVariant`. A second full `AppBuilder.Setup` inside
  the session was avoided for the same reason it would conflict with the already
  initialized global headless platform.

# Changelog

- 2026-06-01 — Slice 001: stood up the Elmish MVU loop end-to-end (Shell.fs root
  model/msg/dispatcher, MainWindow Elmish mount, panel-id-dispatch view, read-only stack
  panel via ConstructionView.fs), added the Elmish host packages to Ui + App, created
  the headless `OpticalConstructor.Ui.Tests` project (smoke + stack-panel view test) and
  registered it in the solution. All slice gates green.
- 2026-06-01 — Slice 001 attempt 02: replaced the inline private seed-project triple in
  `Shell.fs` with a single `Templates.bandpassFilter ()` call (reuse cleanup + 600→550 nm
  incident-light fix via `Templates.defaultLight`); R-2 dual project seed preserved. All
  slice gates re-run green (build 0 errors; berreman 84, constructor 204, ui-smoke 1, ui-tests 1).
- 2026-06-01 — Slice 001 attempt 03: reworked the `ui-smoke` test (`SmokeTests.fs`) to drive
  the **real** `App` lifetime headlessly instead of constructing `MainWindow()` directly —
  pointed `TestApp.fs`'s `TestAppBuilder.BuildAvaloniaApp` at `OpticalConstructor.App.App`
  so `App.Initialize`'s theme seam runs in the session, asserted that seam via
  `Application.Current.RequestedThemeVariant`, and invoked `App.OnFrameworkInitializationCompleted`
  on a fresh App carrying a `ClassicDesktopStyleApplicationLifetime`, asserting its
  `MainWindow` is set + visible (AC-U1.3 / §U1.7). Edited only `SmokeTests.fs` + `TestApp.fs`;
  the dual construction/workspace seed (R-2) and the `ui-tests` gate description are untouched.
  All slice gates re-run green (build 0 errors; berreman 84, constructor 204, ui-smoke 1, ui-tests 1).
