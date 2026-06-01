# 001 impl-plan — Root MVU host + shell skeleton + ui-smoke gate

## Approach

Stand up the live Elmish MVU loop end-to-end so U2–U8 extend a proven pattern.

1. **Packages (R-1).** Add `Avalonia.FuncUI.Elmish` 1.6.0 + `Elmish` to both
   `OpticalConstructor.Ui.fsproj` and `OpticalConstructor.App.fsproj`, aligned to the
   pinned FuncUI 1.6.0 / Avalonia 11.3.4.

2. **`Shell.fs` (new, R-2).** New module `OpticalConstructor.Ui.Shell` holding the
   root MVU surface: `Page` DU (`Construction` | `SynthesisFit`); `RootModel`
   aggregating env + page + `ConstructionPage.Model` + `Workspace.Model` + optional
   `SynthesisFitPage.Model` + `ChartSettings` + `Readout.Markers` + `MaterialLibrary`;
   `RootMsg` wrapping `Construction`/`Workspace` sub-msgs plus a shell-level `Shell`
   case (nav/theme/panel-show/dock). `init : RootModel * Cmd<RootMsg>` (seeded from
   `UserEnvironment.defaults`) plus `initFrom : EnvironmentSettings -> _` so the App
   can seed env from the persisted `Startup.settings`. `update` is a dispatcher that
   calls each sub-`update` and attaches `Cmd.none` (no effects in U1).

3. **`ConstructionView.fs` (new, R-5).** Read-only `stackPanel : ConstructionPage.Model
   -> IView` rendering the selected node's layer rows via `StackEditor.layerRowLabels`
   / `displayThickness`. No edit dispatch in U1.

4. **`Shell.view` (R-4).** A top-level nav control (Construction / Synthesis-Fit,
   active page marked) over a `DockPanel` of the visible panels from
   `AppShell.visiblePanels`, each docked via the now-public `AppShell.toDock`. The
   `stack` id renders real content (ConstructionView); the other four ids render a
   titled placeholder (R-8 — placeholders satisfy "render one frame headlessly").

5. **`AppShell.fs` (edit).** Remove the superseded private `shellView`/`panelView`
   placeholders; expose `toDock` (drop `private`). Keep the reducers
   (`visiblePanels`/`setPanelVisible`/`dockPanel`) and the theme seams.

6. **`Program.fs` (edit, R-3).** Replace the static `Component` mount with
   `Program.mkProgram (fun () -> Shell.initFrom Startup.settings) Shell.update
   Shell.view |> Program.withHost this |> Program.run` from `MainWindow`. Keep the
   `Initialize` theme seam (`AppShell.themeVariant`).

7. **`OpticalConstructor.Ui.Tests` (new, R-7).** Headless xUnit project
   (`Avalonia.Headless` + `Avalonia.Headless.XUnit`, xunit.v3), referencing
   `OpticalConstructor.Ui` and `OpticalConstructor.App`. `TestApp` + `AvaloniaTestApplication`
   wiring. Smoke test (`Category=ui-smoke`): construct + show the real `MainWindow`
   headlessly (proves the host opens and the MVU mount renders every visible panel),
   and render the `SynthesisFit` page body. Per-panel view test (ui-tests): mount
   `ConstructionView.stackPanel` and assert the layer-row TextBlocks render.

8. **Solution + gates.** Register `OpticalConstructor.Ui.Tests` in `Berreman.slnx`.
   The `.gates/OpticalConstructor/ui-smoke.gates` and `ui-tests.gates` descriptors
   already exist and match the slice `.gates` snapshot — no recreation needed.

## Files to modify

- New: `OpticalConstructor.Ui/Shell.fs`, `OpticalConstructor.Ui/ConstructionView.fs`,
  `OpticalConstructor.Ui.Tests/OpticalConstructor.Ui.Tests.fsproj`,
  `OpticalConstructor.Ui.Tests/{TestApp,SmokeTests,PanelViewTests}.fs`.
- Edit: `OpticalConstructor.Ui/AppShell.fs`, `OpticalConstructor.Ui/OpticalConstructor.Ui.fsproj`,
  `OpticalConstructor.App/Program.fs`, `OpticalConstructor.App/OpticalConstructor.App.fsproj`,
  `Berreman.slnx`.

## Risks

- **Package restore** of `Avalonia.FuncUI.Elmish` / `Elmish` / `Avalonia.Headless[.XUnit]`
  (not in local cache) — version alignment against FuncUI 1.6.0 / Avalonia 11.3.4.
- **`Avalonia.Headless.XUnit` ↔ xunit.v3** compatibility (spec assumes it; verify at build).
- Keeping the headless Avalonia dependency OUT of the pure `OpticalConstructor.Tests`
  so `constructor-unit-tests` stays Avalonia-free (new project is separate).
- `init` value vs `mkProgram`'s `unit -> _`: reconciled via `initFrom` + a `(fun () -> ...)`
  wrapper at the mount (documented in impl-log Gotchas).
