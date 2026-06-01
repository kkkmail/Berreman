# 018 — UI-wiring plan / spec: composing the tested MVU core into a live FuncUI app

**Status:** draft plan for review. No code written. Supersedes nothing; it is
the follow-up arc proposed at the end of
[017](./017-app-immediate-exit-avalonia-bootstrap-fix.md).

## 1. Goal & scope

Spec 0022 implemented the Optical Constructor as an **Avalonia-free MVU/logic
core** (models, `update` reducers, pure projections, persistence) with **204
passing tests**, but the **view tier was never written**: the only FuncUI view
in the whole `OpticalConstructor.Ui` project is `AppShell.shellView`, which
renders placeholder panel-name frames. This plan wires the existing tested core
into a real, interactive window — panel by panel — using the **public MIT
`Avalonia.FuncUI` 1.6.0** surface already linked.

In scope: a root MVU host loop, view functions for each panel/page, chart host
controls, background-job + cancellation wiring, project open/save/new lifecycle,
theme/layout persistence round-trip, and a **launch/render smoke gate**.

Out of scope (unchanged from the original spec): installer/packaging; the
FuncUI-clone audit (deliberately postponed — see §10); schema migration; any
change to the tested model/`update`/projection modules (they stay frozen).

## 2. Decision recorded by the operator (2026-06-01)

The clone decision (AC-A8 / §A.9) is **postponed until after the UI**. Rationale:
the public-NuGet `Avalonia.FuncUI` is already linked and stable, and the
dependency is a drop-in swap later (NuGet → ProjectReference/local-NuGet into the
audited clone). All UI work below targets the public NuGet surface; nothing
references `C:\GitHub\Avalonia.FuncUI.Clone\`. Swap path in §10.

## 3. Design principles

1. **Freeze the tested core.** The model/`update`/projection modules
   (`ConstructionPage`, `StackEditor`, `Schematic`, `SynthesisFitPage`,
   `Workspace`, `JobRunner`, charts data builders, `Validation`, …) are
   **Avalonia-free by mandate (P3)** and fully tested. We do **not** edit them.
   New view code lives in **new sibling `*View.fs` modules** (and the App
   composition root), so the 204 tests stay green and P3 holds.
2. **Elmish, because the core already is.** Every page exposes
   `update : Msg -> Model -> Model` and an `init` — literally Elmish-shaped.
   We add a thin root Elmish program (`Avalonia.FuncUI.Elmish`) that routes a
   root `Msg` to the existing sub-`update`s and renders the existing helpers.
3. **Effects live in commands, not the model.** The pure `update`s model side
   effects as *inbound* messages (`NodeSolved`, `ReportProgress`,
   `FitCompleted`). The view tier supplies the matching `Cmd` that performs the
   effect (solve, background fit, file IO) and dispatches the result back. The
   model stays pure and serializable; non-serializable handles
   (`CancellationTokenSource`, dialogs) live outside it.
4. **Renderer-host adapters are isolated.** ScottPlot/Plotly/OpenTK hosting is
   confined to small adapter views so the panels stay declarative.

## 4. Target architecture

### 4.1 Root model & message (new module `Shell.fs` in the App project or Ui)

Aggregate the existing sub-models; route to existing sub-updates:

```fsharp
type Page = Construction | SynthesisFit          // from the two navEntry modules

type RootModel = {
    env          : UserEnvironment.EnvironmentSettings   // theme + panel layout (J.6/J.8)
    page         : Page
    construction : ConstructionPage.Model
    workspace    : Workspace.Model
    fit          : SynthesisFitPage.Model option         // opened on demand
    chart        : ChartSettings.ChartSettings
    job          : JobRunner.JobModel<FitReport>          // or per-sweep result
    markers      : Readout.Markers
    materials    : MaterialLibrary                         // for the materials panel
}

type RootMsg =
    | Construction of ConstructionPage.Msg
    | Workspace    of Workspace.Msg
    | Fit          of SynthesisFitPage.Msg
    | Job          of JobRunner.JobMsg<FitReport>
    | Source       of Sources.SourceEditorView.SourceMsg
    | Chart        of ChartMsg                             // new: settings/cursor edits
    | Shell        of ShellMsg                             // new: nav, theme, panel show/dock
    | Io           of IoMsg                                // new: open/save/new/template/gallery
```

`update` is a dispatcher: each case calls the existing sub-`update` and wraps
the result, attaching a `Cmd` only for the effectful transitions in §6.

### 4.2 MVU host

`Avalonia.FuncUI.Elmish` `Program.mkProgram init update view |> Program.withHost
hostControl |> Program.run`, mounted from `MainWindow` (replacing the static
`Component(fun _ -> AppShell.shellView …)` added in slice 017). Adds two
package refs to the App project: `Avalonia.FuncUI.Elmish` and `Elmish`.

### 4.3 Threading

Background callbacks (`JobRunner.startBackground` `onProgress`/`onDone`) run off
the UI thread; every `dispatch` from them must marshal via
`Avalonia.Threading.Dispatcher.UIThread.Post`. This is the single most common
FuncUI wiring bug — call it out in every effectful Cmd.

## 5. Panel / page → view mapping

`AppShell.shellView`/`panelView` is rewritten to switch on the panel id and
render the real view, fed from `RootModel`, dispatching `RootMsg`. The default
panel ids are fixed by `UserEnvironment.defaultPanels`:
`stack`, `materials`, `sources`, `chart`, `results`. Layout/visibility/dock keep
using the existing pure reducers (`setPanelVisible`, `dockPanel`,
`visiblePanels`) and `themeVariant`; resizing uses public-Avalonia
`Grid`+`GridSplitter` (the richer drag-to-redock chrome is the clone's later).

| Panel / page | New view module | Renders from (existing surface) | Dispatches |
|---|---|---|---|
| `stack` | `ConstructionView.fs` | `ConstructionPage.Model`; `StackEditor.layerRowLabels`, `displayThickness`, `confirmationPrompt`, `canUndo`, `isNodeBusy`, `descendantCount` | `Construction (SelectNode/EditStack/RequestDeleteNode/…)` |
| `materials` | `MaterialsView.fs` | `MaterialPreview.show*` → `ScottPlot.Plot`; `MaterialLibrary`; `StackEditor.layerMaterialDrop` | `Source`/`Construction(EditStack SetLayerMaterial)` |
| `sources` | `SourceView.fs` | `SourceEditorView` (`SourceMsg`, `defaultSource`, axes), `PolarizationPicker` (`applyPreset`, `liveStokes`, `poincareMarker`, `ellipseParameters`) | `Source …` |
| `chart` | `ChartView.fs` | `Plot1DView.render*` → `AvaPlot`; `Plot3DView.render` → WebView2; `ChartSettings`; `Readout`; `PolarizationPlots` | `Chart …` |
| `results` | `ResultsView.fs` | `ConstructionPage.results` (`EmFieldSystem`), `Schematic.layout`/`rayGeometry`, `SystemView3D` | (read-mostly) |
| Construction page nav | `ConstructionPage.navEntry` (default landing) | — | `Shell (Navigate Construction)` |
| Synthesis/Fit page | `FitView.fs` | `SynthesisFitPage.Model`, `canCancel/canRevert/canAccept`, `progressText`, `comparisonOverlay` | `Fit …` / `Job …` |

## 6. Effectful transitions (the only places that need a `Cmd`)

The pure `update`s leave a "result" message to be produced by an effect. Each
row is a `Cmd` the view tier supplies:

| Trigger (pure update sets up) | Effect to run | Result msg dispatched back |
|---|---|---|
| `Construction (EditStack …)` / `AttachChild` / `ConfirmDeleteNode` → marks `busy` | `async { ConstructionPage.solveSubtree path root }` (off-thread) | `Construction (NodeSolved map)` |
| `Fit StartFit` (sets `running`) | create `CancellationTokenSource`; `JobRunner.startBackground` running a `runPoints`/`runIterations` fit; `onProgress`→UI.Post | `Fit (ReportProgress …)`, then `Fit (FitCompleted …)` / `Fit (FitFailed …)` |
| `Fit CancelFit` / `Job CancelRequested` | `cts.Cancel()` (handle held outside model) | cooperative → `Fit (FitFailed "cancelled")` / `Job RunCancelled` |
| `Io Open` / `Save` / `New` / `LoadTemplate` / `OpenGalleryEntry` | Avalonia `IStorageProvider` dialog + `Project*`/`Templates.loadTemplate`/`Help.openEntry`/`ConstructionPage.saveProject` | `Io (Loaded project)` / `Io (Saved path)` / error toast |
| `Shell (ToggleTheme/ShowPanel/DockPanel)` | none (pure reducers) + persist | `UserEnvironment.save` (fire-and-forget Cmd) |

**Non-model state:** the `CancellationTokenSource`, the live `AvaPlot`/WebView2
control instances, and `IStorageProvider` are held in the view/host layer (a
component `ctx.useState` or a host field), never in `RootModel`.

## 7. Chart & 3D hosting adapters

Charts return plain renderer objects (confirmed) — wrap each in a host control:

| Source function | Returns | Host control | Adapter note |
|---|---|---|---|
| `Plot1DView.renderSweep/renderComparison/renderFieldDepth/renderDispersion`, `PolarizationPlots.renderEllipse`, `CieView.render` | `ScottPlot.Plot` | `ScottPlot.Avalonia.AvaPlot` | one `AvaPlot` per chart; set `.Plot`, call `Refresh()` on model change |
| `Plot3DView.render`, `PolarizationPlots.poincareSphere/reflected/transmitted` | `Plotly.NET.GenericChart` | `Microsoft.Web.WebView2` | `GenericChart.toEmbeddedHTML` → `NavigateToString` (Windows-only; OK, app is win-x64) |
| `SystemView3D.placeElements/beamSegments` (`Vec3`) | geometry records | OpenTK GL control **or** 2D Canvas projection | highest effort — see §9, slice U8; recommend a 2D orthographic Canvas first, real GL viewport later |

FuncUI hosts a native control via `ContentControl.create [ ContentControl.content
controlInstance ]` (or `View.createWithOutlet` to keep a handle for `Refresh()`).

## 8. Project / IO & persistence seam

- **Open/Save/New**: Avalonia `TopLevel.StorageProvider` file pickers (STAThread
  already set in slice 017). Save uses `ConstructionPage.saveProject : Model ->
  Result<path*json, StorageError>`; open parses via the existing Storage layer.
- **Templates / gallery**: `Templates.all : TemplateEntry list` (six factories)
  and `Help.gallery : GalleryEntry list`, each via the schema-validated
  `loadTemplate`/`openEntry`.
- **Environment**: load once at startup (already done in 017 via
  `UserEnvironment.load`); persist theme/layout/recent-files on change through
  `UserEnvironment.save`. This exercises the AC-J6/AC-J8 round-trip live.
- **Sidecars / fit reports**: `JobRunner.derivedArtefactPath` /
  `SynthesisFitPage.saveFitReport` write `.binz` under the project sidecar dir.

## 9. Proposed slice breakdown (incremental, each independently green)

Each slice ends build-green + tests-green + **ui-smoke-green** (§11).

- **U1 — Root MVU + shell skeleton.** `Shell.fs` root model/msg/update
  dispatcher; Elmish host in `MainWindow`; rewrite `panelView` to switch on panel
  id; render the **stack panel read-only** (layer rows from `layerRowLabels`) to
  prove the loop end-to-end. Add the smoke gate. *Lowest risk; establishes the
  pattern.*
- **U2 — Stack editing.** Wire all `StackEditor.StackMsg` edits through
  `ConstructionPage.EditStack`; add the async node-solve `Cmd`
  (busy→`NodeSolved`); delete-confirmation gate UI; undo.
- **U3 — Materials panel.** Material library browse; `MaterialPreview` dispersion
  plots in `AvaPlot`; drag-drop material onto a layer via `layerMaterialDrop`.
- **U4 — Sources panel.** `SourceEditorView` fields + `PolarizationPicker`
  presets; live Stokes/Poincaré/ellipse via `AvaPlot`/WebView2.
- **U5 — Chart panel.** `Plot1DView` in `AvaPlot`; `ChartSettings` controls;
  `Readout` cursors; `Plot3DView` in WebView2.
- **U6 — Results + schematic + workspace.** `Schematic.layout` on a Canvas;
  `Workspace` multi-system visibility/active + overlay (`renderOverlay`).
- **U7 — Synthesis/Fit + background jobs.** `FitView`; `JobRunner` background run
  + cancel + progress (the threading-sensitive slice).
- **U8 — Lifecycle + 3D.** Open/save/new/templates/gallery dialogs;
  environment persistence round-trip; `SystemView3D` (2D Canvas projection first;
  GL viewport optional follow-up).

Sequencing: U1 → U2 are prerequisites for everything; U3–U6 are parallelizable;
U7 depends on U5 (charts) for the overlay; U8 last.

## 10. FuncUI-clone swap path (deferred, for the record)

When the §A.9 audit eventually passes, swap is localized to the two `.fsproj`
package refs (`OpticalConstructor.Ui` + `.App`): replace
`<PackageReference Include="Avalonia.FuncUI" Version="1.6.0" />` with either a
local-NuGet package built from the cleared clone, or a
`<ProjectReference Include="…\Avalonia.FuncUI.Clone\…\Avalonia.FuncUI.fsproj" />`.
Because all views are authored against the FuncUI **DSL surface** (not
clone-specific APIs), no view code should change; the clone then *adds* the
richer docking chrome behind `AppShell`. Until then, panels use public Avalonia
`DockPanel`/`Grid`/`GridSplitter`.

## 11. Testing & the new gate (the real prevention)

The current gates can't catch a missing/broken view tier (an Avalonia-free core
builds and unit-tests with no window). Add:

- **`ui-smoke` gate** — launch the app on the **Avalonia headless platform**
  (`Avalonia.Headless`), assert the window opens, `OnFrameworkInitialization
  Completed` ran, and each panel/page view **instantiates and renders one frame
  without throwing**, then shut down. This is the gate that would have caught
  both the empty-`main` (017) and the missing views.
- **Headless view tests** (`Avalonia.Headless.XUnit`) — per panel: mount the
  view over a known model, assert key controls exist and that a simulated
  interaction dispatches the expected `Msg` (the model assertions reuse the
  existing 204-test helpers). Keep them in a new `OpticalConstructor.Ui.Tests`
  (UI) project so the headless Avalonia dependency stays out of the pure
  `constructor-unit-tests` suite.

Per-slice gate roster becomes `build`, `unit-tests`, `constructor-unit-tests`,
`ui-smoke` (+ the UI test project under its own gate).

## 12. Risks & watch-items

- **Threading**: off-thread `dispatch` without `Dispatcher.UIThread.Post` →
  intermittent crashes. Enforced in §6 Cmds; a headless test should cover the
  fit-progress path.
- **WebView2**: Windows-only + needs the Evergreen runtime (standard on Win11);
  Plotly panels degrade to "unavailable" if absent — handle gracefully.
- **OpenTK/GL** (`SystemView3D`): real GL viewport is the riskiest piece; the
  plan defers it behind a 2D Canvas projection so U8 isn't blocked.
- **Pre-existing build warning**: WindowsBase 4.0.0.0↔5.0.0.0 conflict from
  WebView2 (MSB3277) is already present and benign; don't let it mask new
  warnings (the build gate uses `--warnaserror+:25` only, so it won't fail).
- **Scope**: this is a multi-slice arc comparable to Part J. Recommend running it
  as its own splitter manifest, U1 first as a vertical proof, then fan out.

## 13. Recommended next action

Approve this plan, then either (a) I author a proper slice spec/manifest for the
U-arc and hand it to the splitter, or (b) I hand-build **U1** as a vertical
proof-of-concept (root MVU + stack panel + smoke gate) so the pattern is
demonstrated before committing to the full arc. No code until you choose.
