# Step 004 — state of the world

## Where we are

Step 004 (ADD_COMPONENT UICOMP_XDUO_0007) completes the test-code eviction that steps 002–003
prepared: after the workbench moved into `OpticalConstructor.Ui`, the ONLY reason the product
`OpticalConstructor.App` still referenced `OpticalConstructor.TestWindows` was the seven diagnostic
buttons on its launcher. Those buttons now live on `TestLauncherWindow` inside the NEW standalone
executable `OpticalConstructor.TestWindows.App` (WinExe, net10.0, x64, its own Avalonia
`Application` + entry point, added to `Berreman.slnx`), each button carrying a stable `Name` AND
`AutomationProperties.AutomationId`/`Name`. The product launcher keeps Main only
(Inverse/Materials/Library land in step 45), and `OpticalConstructor.App` no longer references
`OpticalConstructor.TestWindows` — test code has left the product dependency graph.
`Ui.Tests` references the new project and pins the window's full contract headlessly.

## What's working

- Add the standalone OpticalConstructor.TestWindows.App executable (WinExe,
  x64, App-mirrored Avalonia package set, in Berreman.slnx) hosting
  TestLauncherWindow: one stable-named button per diagnostic scene with
  matching AutomationId, ids centralized in its UiIds module.
- Trim the product launcher to Main only (380×180) and DROP
  OpticalConstructor.App's project reference to OpticalConstructor.TestWindows.
- Pin the new window in Ui.Tests (TestLauncherTests): 16 structure cases
  (labels, automation ids, exact order) + 9 headless smoke proofs (render
  frame, seven click-opens, app-host startup window).
- Re-pin the product launcher as Main-only (LauncherTests: only-button pin;
  Main click-opens proof kept); suites 457 / 113 / 348 / 119 — ui-smoke +2 and
  ui-tests +9 over the step-003 checkpoint.

## Tests

- Gates are executed by the arc-runner's deterministic gate engine after this worker exits
  (Invariant 6 — the worker acts, it runs no checks). The roster for this step is `build`,
  `unit-tests`, `constructor-unit-tests`, `ui-smoke`, `ui-tests`.
- Diagnostic verification (not gate authority): `dotnet build Berreman.slnx -c Release` succeeded
  with 0 errors, **no MSB3277**, and zero warnings from any touched/new project — the only
  warnings are the step-001-catalogued pre-existing set in untouched files (FS1125
  SeriesDataTests, FS3873 Dispersion, FS0044 ChartWindow, SYSLIB0051 vendored MathNet, NU1701
  Wolfram.NETLink). Suites: OpticalConstructor.Tests **457/457**, ui-smoke **113/113** (checkpoint
  111), ui-tests **348/348** (checkpoint 339), BerremanTests **119 passed / 5 pre-existing
  skips**.
- Nothing deferred.

## Architecture

- **Test code is out of the product graph**: `OpticalConstructor.App` → { Ui } only;
  `OpticalConstructor.TestWindows.App` → { TestWindows } → { Ui, Controls, Domain }. The two
  executables share the Ui workbench through the same one-directional edges; no cycle exists and
  the diagnostic scenes can no longer ship with the product binary.
- **The component's registered surface is its `UiIds` module** (the repo's per-view precedent for
  the contract registry): `[<Literal>]` ids reusing the product launcher's former button names, so
  headless tests (`InlineData` cites the literals) and any external automation keyed on the old
  names survive the move unchanged.
- **The new composition root mirrors the product bootstrap** (FluentTheme, classic desktop
  lifetime, `ShutdownMode.OnLastWindowClose`) but deliberately omits the persisted-theme seam —
  diagnostics always start under the default Fluent variant; nothing reads `environment.json`.
- **The package set is mirrored verbatim from OpticalConstructor.App** so both composition roots
  pin an identical Avalonia/FuncUI dependency graph (no version skew, no MSB3277).

## Deferred

- The product launcher's Inverse / Materials / Library buttons are step 45's scope; the launcher
  intentionally hosts Main alone until then (window height 180 can grow back with them).
- The possibly under-used Ui package references (`ScottPlot.Avalonia`, `Plotly.NET`, `OpenTK`) and
  the pre-existing warnings in untouched files (FS1125, FS3873, FS0044, SYSLIB0051) remain for
  spec 0038 Part N's sweep (carried from steps 002–003).
- TestWindows.App carries FuncUI/Elmish package pins it does not directly consume (the mirror-the-
  App-package-set instruction); if Part N prunes package sets, this one can shrink to
  Avalonia/Desktop/Fluent.

## Gotchas

- **"Contract registry" interpretation recorded**: this repo has no central registry file; every
  prior UICOMP step registered its component as a per-view `UiIds` constants module, and
  UICOMP_XDUO_0007 does the same in `TestLauncherWindow.fs`.
- **Name ambiguity guard**: `OpticalConstructor.TestWindows.App` contains a type `App` just like
  `OpticalConstructor.App`. No file opens both namespaces and uses a bare `App` — keep it that way
  when adding tests that touch both composition roots (qualify one of them).
- **LauncherTests' size pin changed** (520 → 180): the product launcher shrank with the buttons
  gone. Step 45 will bump both the window and the pin again.
- **The app-host smoke proof pattern** (fresh `App` + attached
  `ClassicDesktopStyleApplicationLifetime` under the shared headless session) now exists twice —
  SmokeTests for the product, TestLauncherTests for the diagnostics executable. The session's
  `Application.Current` remains the PRODUCT App and supplies the styles every constructed window
  resolves.
- Step 002/003 carried-over gotchas remain valid (baselines come from `.checkpoints-json`, not the
  SoW YAML; several Ui modules still have only OpticalConstructor.Tests consumers).

## Changelog

- 2026-07-10 — Step 004 (IMPLEMENT, attempt 1): added the standalone
  OpticalConstructor.TestWindows.App executable (WinExe, x64, in Berreman.slnx) hosting
  TestLauncherWindow — seven stable-named + automation-id'd diagnostic scene buttons with its
  UiIds contract module and its own App/entry point; trimmed the product launcher to Main only
  and dropped OpticalConstructor.App's TestWindows reference; Ui.Tests references the new
  project, TestLauncherTests pins the window (16 structure + 9 smoke), LauncherTests re-pins the
  Main-only launcher. Build clean (no MSB3277); suites 457 / 113 / 348 / 119.

```yaml
gates:
  berreman_unit_tests: 119
  constructor_unit_tests: 457
  ui_smoke_tests: 113
  ui_tests: 348
```
