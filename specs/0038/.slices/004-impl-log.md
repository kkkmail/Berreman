# Step 004 — impl-log (attempt 1)

## Progress

- [x] Read task file, worker system prompt (`add_component_worker.system-md` +
      shared base), project prompt, slice spec; surveyed the launcher, TestWindows,
      App/Ui.Tests fsprojs, LauncherTests/SmokeTests, slnx, step-003 SoW baselines.
- [x] Create `OpticalConstructor.TestWindows.App` (fsproj + TestLauncherWindow.fs +
      Program.fs); add to Berreman.slnx.
- [x] Trim the product launcher to Main only (380×180); drop App → TestWindows
      reference; refresh doc comments.
- [x] Ui.Tests: reference the new project; add TestLauncherTests.fs (16 structure +
      9 smoke proofs); trim LauncherTests.fs to the Main-only contract (3 structure +
      1 smoke; new only-button pin).
- [x] Diagnostic build + all four suites (non-gate verification) — all green,
      ui-smoke 113 (+2) and ui-tests 348 (+9) over the step-003 checkpoint; LF check
      clean.
- [x] State-of-the-world + exit summary.

## Files modified

New (`OpticalConstructor.TestWindows.App/`, one fsproj per folder):

- `OpticalConstructor.TestWindows.App.fsproj` — WinExe / net10.0 / x64 /
  `--warnaserror+:25`; Avalonia package set mirrored verbatim from
  `OpticalConstructor.App.fsproj`; single `ProjectReference` to
  `OpticalConstructor.TestWindows` (Ui/Controls/Domain flow transitively).
- `TestLauncherWindow.fs` — `namespace OpticalConstructor.TestWindows.App`; a
  top-level `module UiIds` of `[<Literal>]` button ids (the component's automation
  contract, per the repo's per-view `UiIds` precedent — ids reuse the product
  launcher's former strings so external automation keyed on them survives), then
  `TestLauncherWindow` (plain fixed-size `Window`, 380×480): title block + seven
  scene buttons built by one helper that sets `Name`,
  `AutomationProperties.AutomationId` (same string) and `AutomationProperties.Name`
  (the label) and wires `Click` → `<SceneWindow>().Show()`.
- `Program.fs` — `App` (FluentTheme; desktop lifetime; `ShutdownMode.OnLastWindowClose`;
  `MainWindow <- TestLauncherWindow()`) + `[<EntryPoint; STAThread>]` main mirroring
  the product bootstrap. No persisted-theme seam (diagnostics start under the
  default Fluent variant).

New (`OpticalConstructor.Ui.Tests/`):

- `TestLauncherTests.fs` — the UICOMP_XDUO_0007 contract. Structure (`ui-tests`,
  16 cases): title/fixed-size fact; 7-row theory "hosts every scene button by
  stable name with its label"; 7-row theory "every scene button carries its
  automation id and accessible name"; exact-set/order fact. Behaviour (`ui-smoke`,
  9 cases): headless render proof that all seven buttons are visible in a shown
  frame; seven click-opens proofs (each button opens its diagnostic scene window,
  observed via `Window.WindowOpenedEvent`); an app-host proof (SmokeTests pattern)
  that the new `App`'s framework-init sets a `TestLauncherWindow` as the desktop
  `MainWindow` with `OnLastWindowClose`.

Edited:

- `Berreman/Berreman.slnx` — the new project added (x64 platform mapping) after the
  TestWindows entry.
- `OpticalConstructor.App/Program.fs` — the seven diagnostic buttons and the
  `open OpticalConstructor.TestWindows` removed; the launcher keeps title + Main
  only and shrinks to 380×180 (was 520 tall for nine children); launcher doc
  comment rewritten to record the step-004 split (Inverse/Materials/Library land
  in step 45).
- `OpticalConstructor.App/OpticalConstructor.App.fsproj` — the
  `OpticalConstructor.TestWindows` `ProjectReference` DROPPED (test code leaves the
  product dependency graph); comment records the move.
- `OpticalConstructor.Ui.Tests/OpticalConstructor.Ui.Tests.fsproj` —
  `<Compile Include="TestLauncherTests.fs" />` after LauncherTests.fs;
  `ProjectReference` to the new `OpticalConstructor.TestWindows.App`.
- `OpticalConstructor.Ui.Tests/LauncherTests.fs` — trimmed to the Main-only
  product-launcher contract: title/size pin (updated to 180), Main name/label pin,
  a NEW "the diagnostic scene buttons are gone — Main is the launcher's only
  button" pin, and the Main click-opens `ui-smoke` proof; the seven diagnostic
  theory rows + seven click-opens proofs moved to TestLauncherTests; the
  `open OpticalConstructor.TestWindows` removed.

## Testing state

Gate execution belongs to the arc-runner's gate engine after this worker exits
(Invariant 6 — the worker acts, it runs no checks). Diagnostic verification only,
not gate authority:

- `dotnet build Berreman.slnx -c Release` — **Build succeeded, 0 errors**;
  **no MSB3277 anywhere in the log**; zero warnings from any touched/new project
  (TestWindows.App / App / Ui.Tests). The 10 warning lines are exactly the
  step-001-catalogued pre-existing set in untouched files (FS1125 SeriesDataTests
  ×4, FS3873 Dispersion.fs, FS0044 ChartWindow.fs, SYSLIB0051 vendored MathNet ×2,
  NU1701 Wolfram.NETLink ×2). The new executable's output
  (`Softellect.Berreman.OpticalConstructor.TestWindows.App`) built.
- `OpticalConstructor.Ui.Tests --filter Category=ui-smoke`: **113/113 passed**
  (checkpoint 111; −7 launcher click-opens moved out, +9 TestLauncher proofs).
- `OpticalConstructor.Ui.Tests --filter Category!=ui-smoke`: **348/348 passed**
  (checkpoint 339; −7 launcher theory rows, +16 TestLauncher structure cases).
- `OpticalConstructor.Tests`: **457/457 passed** (checkpoint 457; untouched).
- `BerremanTests`: **119 passed, 5 skipped** (the pre-existing skips;
  checkpoint 119; untouched).
- Line endings: `git diff --numstat` equals `--ignore-cr-at-eol --numstat`; all
  four new files scanned — 0 CR bytes, no BOM.

## Artifacts

- `specs/0038/.artifacts/004-diag-build.log` — diagnostic Release build log (the
  MSB3277 / touched-project warning sweep ran against this capture).
- `specs/0038/.artifacts/004-diag-ui-smoke.log` — ui-smoke suite run (113 passed).
- `specs/0038/.artifacts/004-diag-ui-tests.log` — ui-tests suite run (348 passed).
- `specs/0038/.artifacts/004-diag-constructor-tests.log` — constructor suite run
  (457 passed).
- `specs/0038/.artifacts/004-diag-unit-tests.log` — BerremanTests run (119 passed,
  5 pre-existing skips).

## Gotchas

- No operator note in flight (the project prompt's Operator note section is empty).
- **"Register the component in the contract registry"** (ADD_COMPONENT obligation):
  this repo has no central registry file — the established per-component registry
  is the per-view `UiIds` constants module (`[<Literal>]` ids) that every prior
  UICOMP step (0001–0006) shipped. UICOMP_XDUO_0007 follows that precedent:
  `OpticalConstructor.TestWindows.App.UiIds` is the registered surface, and the
  ids deliberately REUSE the product launcher's former button names
  (`OpenTableRotationTestButton`, …) so automation keyed on them survives the move.
- **The button ids live in a namespace that shadows nothing**: the new namespace is
  `OpticalConstructor.TestWindows.App`, which contains a type `App` like
  `OpticalConstructor.App.App`. No file opens both namespaces and uses a bare
  `App`, so no ambiguity arises (TestLauncherTests opens only the TestWindows
  pair; SmokeTests/TestApp open only the product pair).
- **Launcher height pin updated**: the product launcher shrank 520 → 180 with the
  seven buttons gone (the slice doesn't mandate a size; a Main-only 520-tall form
  would be visibly broken). LauncherTests' size pin updated to match; step 45 can
  grow it again with Inverse/Materials/Library.
- **The app-host smoke proof for the new executable reuses the SmokeTests
  pattern** (fresh `App` + attached `ClassicDesktopStyleApplicationLifetime` inside
  the shared headless session) — the session's `Application.Current` stays the
  product `App`, which supplies the Fluent styles all constructed windows resolve;
  the fresh instance only exercises `Initialize`/`OnFrameworkInitializationCompleted`.
- **`dotnet build` re-ran the two `dotnet test` invocations' builds** — the test
  logs' counts are from the same Release tree the build gate produces
  (`bin\x64\Release\`, per Directory.Build.props' x64 default).
