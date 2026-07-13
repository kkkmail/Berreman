# Impl-plan — 047 WIRE_UI (AC-W1): finalize the composition root + drive the whole surface headless

## Anchor / family
`WIRE_UI` — structural composition against the target repo. Connect already-built
components at the composition boundary and ship a **headless wiring assertion** that
drives the real, root-wired surface. Add no new product behaviour; assemble existing
surfaces.

## What the slice asks
1. Composition root (`OpticalConstructor.App/Program.fs`): AppSettingsProvider once +
   typed settings (step 5) — already done; app scope (step 6) with the five stores plus
   `SceneProxy.createInMemory` (27), `ExperimentCollectionProxy.createInMemory` (29), the
   file-backed `ExperimentDataProxy` (36); the WindowRegistry + WindowLauncher with every
   window factory (Materials, Library, the three editors, SolverHandoff) + the modality
   switch; hand the context to the launcher window and both constructor states.
2. Ui.Tests ui-smoke: render one frame each for launcher, Main, Inverse, Materials +
   Library in Browse and Select, the three editors, and the SolverHandoff — without throwing.

## Current state (surveyed)
- `AppConfig.loadWorkbenchSettings` already creates the `AppSettingsProvider` once; `Startup`
  materializes the typed `WorkbenchSettings` and builds `AppContext.create` once; `Program.fs`
  already hands the context to `LauncherWindow`, `MainConstructorWindow`,
  `InverseConstructorWindow`.
- `AppContext.create` builds the five stores + file-backed `ExperimentDataProxy` (36) +
  `ExperimentCollectionProxy.createInMemory` (29). **Gap:** it does NOT build the
  `SceneProxy.createInMemory` (27) — the only proxy missing from "every proxy built once at
  the root."
- The launcher seam (`EditorLaunchers.defaults` + `WindowLauncher.create` over the ONE
  host-layer `WindowRegistry`) is already built and threaded. Every window type has a
  factory: Materials/Library via `EditorLaunchers.defaults`; the three editors via the
  Materials/Library windows' own contexts; the SolverHandoff key exists on `WindowKey`.
- The whole surface is ALREADY rendered by 177 existing ui-smoke tests (per-window suites).
  The consolidated composition-root wiring assertion is the missing piece.

## Approach
1. **`OpticalConstructor.Ui/AppContext.fs`** — add a `scenes : Scene.SceneProxy` field to the
   app-scope record and build it once via `Scene.SceneProxy.createInMemory ()` in
   `AppContext.create`. This closes the "every proxy built once at the root" gap (STORE_XDUO_0004).
   *(Scope note: the how_to explicitly puts SceneProxy in "the app scope"; the app scope is
   `AppContext` in `OpticalConstructor.Ui`, one project outside the declared `touches`
   [App, Ui.Tests] — the how_to governs. Recorded in the impl-log Gotchas.)*
2. **`OpticalConstructor.Ui.Tests/WireUiFinalCompositionTests.fs`** (new) — the consolidated
   headless wiring assertion over ONE app scope built exactly like `Startup.context`:
   - a `ui-tests` pin that the app scope carries the scene proxy (functional) beside the other
     proxies — "every proxy built once at the root";
   - `ui-smoke`: render the launcher, the Main constructor, the Inverse constructor;
   - `ui-smoke`: build the ONE root window-launcher factory covering every `WindowKey`
     (Materials, Library, the three editors, SolverHandoff) and open + render each through the
     real `WindowLauncher.create … BrowseOpen` over the shared registry, closing each (the
     `WindowLauncherTests` cleanup discipline so no key leaks across the app-global registry);
   - `ui-smoke`: render both faceted windows in Select state over the app scope.
   Register the file last in the Ui.Tests `.fsproj`.
3. `Program.fs` needs no structural change: `AppContext.create`'s signature is unchanged, so
   the new `scenes` field flows in transparently; the root already hands the context to the
   launcher + both constructor states.

## Risks
- **Registry leakage**: the real launcher registers keys in the app-global `WindowRegistry`.
  Every opened window is closed + `RunJobs`'d so its `Closed` hook unregisters — otherwise a
  later suite's open of the same key would activate a stale window. Follows the existing
  `WindowLauncherTests` discipline exactly.
- **Modality**: the Select opens use `ctx.settings.selectWindowModality` (default
  `ModelessSelectWindows` → unowned `Show`), so no modal `ShowDialog` blocks the headless run.
- **count_at_least gates**: all edits are additive (a new record field with no literal
  construction sites; new tests). Baselines: berreman 119 / constructor 674 / ui-smoke 177 /
  ui-tests 461 — carried forward, raised by the new tests.

## Files
- `OpticalConstructor.Ui/AppContext.fs` (edit: add `scenes`).
- `OpticalConstructor.Ui.Tests/WireUiFinalCompositionTests.fs` (new).
- `OpticalConstructor.Ui.Tests/OpticalConstructor.Ui.Tests.fsproj` (register the new file).
