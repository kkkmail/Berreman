# Step 004 — impl-plan (attempt 1)

## Slice

ADD_COMPONENT `UICOMP_XDUO_0007` `TestLauncherWindow`: a NEW executable project
`OpticalConstructor.TestWindows.App` (net10.0, x64, WinExe, mirroring
`OpticalConstructor.App`'s Avalonia package set, added to `Berreman.slnx`) hosting its
own Avalonia `Application` + entry point and the `TestLauncherWindow` with one
stable-named button per diagnostic scene (Table Rotations, Element Rotations,
Table + Element Rotations, Element Movement, Renderers, Snap to Beam,
Snap to Reflected), each carrying a stable `Name` AND an
`AutomationProperties.AutomationId`. The product `LauncherWindow` loses those seven
buttons and `OpticalConstructor.App` DROPS its project reference to
`OpticalConstructor.TestWindows` — test code leaves the product dependency graph;
the product launcher keeps Main only (Inverse/Materials/Library land in step 45).
`Ui.Tests` references the new project and renders the window headless.

## Approach

1. **New project** `Berreman/OpticalConstructor/OpticalConstructor.TestWindows.App/`
   (one fsproj per folder):
   - `TestLauncherWindow.fs` — `namespace OpticalConstructor.TestWindows.App`; a
     top-level `module UiIds` of `[<Literal>]` button ids (the component's
     registered automation contract, matching the repo's per-view `UiIds`
     precedent), then `type TestLauncherWindow` (plain `Window`, mirroring the
     product launcher's form): a title block plus seven buttons built from one
     helper that sets `Name`, `AutomationProperties.AutomationId` and
     `AutomationProperties.Name` and wires `Click` → `<SceneWindow>().Show()`.
     Button ids/labels reuse the product launcher's existing stable strings
     (`OpenTableRotationTestButton` / "Test Optical Table Rotations", …) so any
     existing automation keyed on them keeps working.
   - `Program.fs` — `type App` (FluentTheme, classic desktop lifetime,
     `ShutdownMode.OnLastWindowClose`, `MainWindow <- TestLauncherWindow()`) and
     the `[<EntryPoint; STAThread>]` main mirroring the product bootstrap.
   - `OpticalConstructor.TestWindows.App.fsproj` — WinExe / net10.0 / x64 /
     `--warnaserror+:25`, the App project's Avalonia package set mirrored
     verbatim, single `ProjectReference` to `OpticalConstructor.TestWindows`
     (Ui/Controls/Domain flow transitively).
2. **`Berreman.slnx`** — add the new project (x64 platform mapping) after the
   TestWindows entry.
3. **Product trim** — `OpticalConstructor.App/Program.fs`: remove the seven
   diagnostic buttons and the `open OpticalConstructor.TestWindows`; shrink the
   now Main-only launcher (380×180) and refresh the header/launcher doc comments.
   `OpticalConstructor.App.fsproj`: drop the TestWindows `ProjectReference`.
4. **Ui.Tests** — fsproj gains a `ProjectReference` to the new project and a
   `<Compile>` for the new `TestLauncherTests.fs` (placed after
   `LauncherTests.fs`).
   - `LauncherTests.fs` trims to the Main-only contract: title/size pin (updated
     size), a Main-button name/label pin, a NEW "Main is the launcher's only
     button" pin (this pins the seven buttons' removal), and the Main click-opens
     `ui-smoke` proof. The seven diagnostic theory rows + seven click-opens move
     out.
   - `TestLauncherTests.fs` (new) pins the component: structure (`ui-tests`) —
     title/fixed-size fact, a 7-row theory "hosts every scene button by stable
     name with its label", a 7-row theory "every scene button carries its
     automation id + accessible name", and an exact-order/exact-set fact;
     behaviour (`ui-smoke`) — a headless render proof that all seven buttons are
     visible under a shown frame, seven click-opens proofs (each button opens its
     scene window, observed via `Window.WindowOpenedEvent`), and an app-host
     proof that the new `App`'s framework-init makes `TestLauncherWindow` the
     desktop `MainWindow` (the SmokeTests pattern against the new composition
     root).

## Count budget (`count_at_least` gates)

- `ui_smoke_tests` (baseline 111): −7 (LauncherTests click-opens removed)
  +9 (render + 7 click-opens + app-host) → expected 113.
- `ui_tests` (baseline 339): −7 (theory 8 rows → 1 Main fact, first-button fact
  → only-button fact) +16 (1 + 7 + 7 + 1) → expected 348.
- `constructor_unit_tests` 457 and `berreman_unit_tests` 119 untouched.

## Files to modify

- New: `OpticalConstructor.TestWindows.App/{OpticalConstructor.TestWindows.App.fsproj,TestLauncherWindow.fs,Program.fs}`,
  `OpticalConstructor.Ui.Tests/TestLauncherTests.fs`.
- Edit: `Berreman/Berreman.slnx`, `OpticalConstructor.App/Program.fs`,
  `OpticalConstructor.App/OpticalConstructor.App.fsproj`,
  `OpticalConstructor.Ui.Tests/OpticalConstructor.Ui.Tests.fsproj`,
  `OpticalConstructor.Ui.Tests/LauncherTests.fs`.

## Risks

- **Name ambiguity**: the new namespace `OpticalConstructor.TestWindows.App`
  contains a type `App` while `OpticalConstructor.App.App` exists — test files
  reference the new one fully qualified; no bare `App` use appears in a file that
  opens both.
- **Second `Application` instance in the shared headless session** — mitigated by
  reusing the exact SmokeTests pattern (fresh app + attached
  `ClassicDesktopStyleApplicationLifetime`), which is proven under this session.
- **`count_at_least` regression** if any migrated proof is lost — the count
  budget above nets +2 ui-smoke / +9 ui-tests over the step-003 checkpoint.
- **MSB3277 / warnings**: the new project mirrors App's package set exactly and
  references only TestWindows; no version skew introduced. Watched in the
  diagnostic build log.
- **LF endings** on all new/edited files — verified via `git diff --numstat` vs
  `--ignore-cr-at-eol` and a CR-byte scan of new files.
