# 017 — `Softellect.Berreman.OpticalConstructor.App.exe` exits immediately (no Avalonia bootstrap)

## The issue

Launching the built desktop app
`…/OpticalConstructor.App/bin/x64/Release/net10.0/Softellect.Berreman.OpticalConstructor.App.exe`
returned instantly — no window, no error, exit code 0.

## Root cause

`OpticalConstructor.App/Program.fs` was never advanced past the **slice-001
scaffold stub**:

```fsharp
module OpticalConstructor.App.Program

[<EntryPoint>]
let main _argv = 0
```

The entry point returns `0` without ever starting Avalonia, so the process
does nothing and exits. Its own header comment even said *"Later UI slices
wire this to the Avalonia application bootstrap"* — but that wiring never
happened. Across slices 002–016 the UI was authored entirely inside
`OpticalConstructor.Ui` as a **library of pure FuncUI view functions**
(`AppShell.shellView : EnvironmentSettings -> IView`, the Part H chart views,
etc.). There was **no Avalonia `Application` subclass, no window, and no
`AppBuilder` bootstrap** anywhere in the solution — the composition root was
missing.

Why the gates didn't catch it: the three gates are `build`, `unit-tests`,
`constructor-unit-tests`. The stub *builds* fine and the test suites cover the
pure Domain/Storage/Ui projection functions headlessly (the §A.6/P3
testability design) — none of them launch the executable, so an empty `main`
sails through every gate. The spec also defined no smoke-launch / installer
step (see [016](./016-arc-complete-optical-constructor-summary.md), Q3).

## The fix

Supply the missing composition root in the **App** project (the one project
allowed to depend on Avalonia — the §P3 "no Avalonia type in model/update"
rule applies to the Ui MVU layer, not here). Rewrote
`OpticalConstructor.App/Program.fs` to host the existing shell view:

- `type MainWindow()` — a FuncUI `Avalonia.FuncUI.Hosts.HostWindow` whose
  `Content` is `Component(fun _ctx -> AppShell.shellView Startup.settings)`.
- `type App()` — an `Avalonia.Application` that adds the `FluentTheme` and
  sets `RequestedThemeVariant` from the persisted theme via the existing
  `AppShell.themeVariant` seam, and on
  `OnFrameworkInitializationCompleted` assigns `desktop.MainWindow`.
- `main` — `AppBuilder.Configure<App>().UsePlatformDetect()
  .StartWithClassicDesktopLifetime argv`, marked `[<EntryPoint; STAThread>]`
  (STA for Windows file dialogs).
- Settings are loaded once at startup via `UserEnvironment.load
  (UserEnvironment.settingsPath ())`, which is total and falls back to the
  built-in `defaults` (AC-J6), so the shell always has a layout/theme to
  render.

The bootstrap pattern mirrors the public Avalonia.FuncUI 1.6.0 reference
examples (`HostWindow` + `Application` + `AppBuilder`).

Also updated `OpticalConstructor.App.fsproj` to declare the Avalonia/FuncUI
NuGet packages it now uses directly (versions mirroring `OpticalConstructor.Ui`):
`Avalonia`, `Avalonia.Desktop`, `Avalonia.Themes.Fluent`, `Avalonia.FuncUI`
— all 11.3.4 / 1.6.0.

### Avalonia.FuncUI.Clone is still untouched
Per constraint 5 / §A.9 / AC-A8, the bootstrap uses **only the public MIT
`Avalonia.FuncUI` NuGet**. Nothing references `C:\GitHub\Avalonia.FuncUI.Clone\`;
the audit remains NOT-YET-RUN and the linking-mechanism decision unresolved.

## What the user sees now

The window shows the §J.8 dockable shell: the default panels (`stack`,
`materials`, `sources` docked left, `chart` filling the center, `results`
docked right) as titled bordered regions, in the persisted light/dark theme.
That is the current extent of `AppShell.shellView` — the spec's shell renders
panel *frames*; the richer per-panel chrome/docking was explicitly left to the
(unreferenced) FuncUI clone. The individual editors/charts exist as tested
view functions but are not yet composed into the shell's panels — wiring each
panel body into `shellView` is the natural follow-up, but it's beyond
"make the app launch".

## Verification

- `dotnet build OpticalConstructor/OpticalConstructor.App/OpticalConstructor.App.fsproj -c Release`
  → **0 errors** (the 12 warnings are all pre-existing: log4net advisory,
  WindowsBase/WebView2 version conflict, MathNet `SYSLIB0051` — none from this
  change). This transitively builds Ui/Domain/Storage/Optimization/Analytics,
  i.e. the same chain the `build` gate compiles.
- Ran the built exe: the process **stayed alive with its window open** (had to
  be force-killed after 7 s), versus the previous instant exit-0. Confirmed
  via PowerShell `Start-Process -PassThru` + `Get-Process` liveness check.
- No test impact: the change is confined to the (test-free) App composition
  root and its `.fsproj`; no Domain/Storage/Ui code under test was modified.

## Files changed

- `Berreman/OpticalConstructor/OpticalConstructor.App/Program.fs` — scaffold
  stub replaced with the Avalonia + FuncUI host bootstrap.
- `Berreman/OpticalConstructor/OpticalConstructor.App/OpticalConstructor.App.fsproj`
  — added the four public Avalonia/FuncUI `PackageReference`s the root uses.

## How to run

From repo root `C:\GitHub\Berreman`:

```powershell
dotnet build Berreman/Berreman.slnx -c Release
dotnet run --project Berreman/OpticalConstructor/OpticalConstructor.App/OpticalConstructor.App.fsproj -c Release
# or launch the exe directly:
# Berreman/OpticalConstructor/OpticalConstructor.App/bin/x64/Release/net10.0/Softellect.Berreman.OpticalConstructor.App.exe
```

Prerequisite: the **.NET 10** runtime (+ WebView2, standard on Win11).

## Prevention

If a runnable window is a real deliverable, the spec/gate set should include a
**launch smoke gate** — start the exe headless/offscreen, assert it stays up
past a short timeout (and/or that `OnFrameworkInitializationCompleted` ran),
then shut it down. A pure-function test suite alone cannot catch an empty
`main`.
