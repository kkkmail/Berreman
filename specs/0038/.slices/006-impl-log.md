# Step 006 — impl log

## Progress

- [x] Read the task file, worker system prompt (`implement_worker.system-md` + base
      `arc-runner.system-md`), project prompt, slice spec, spec 0038 `.spec-md`, and the
      006 gate roster (`006.gates`: build / unit-tests / constructor-unit-tests /
      ui-smoke / ui-tests).
- [x] Surveyed the current composition: `MainConstructorWindow` built the five proxies in
      its constructor (`App/Program.fs`), `DefaultStores` is the test-scene default
      (`Ui/TableAndElementRotationView.fs:270-280` after step 003's move), step 005
      delivered `WorkbenchSettings` + `AppConfig.loadWorkbenchSettings` (bound in
      `Program.Startup`).
- [x] Wrote the impl-plan (`006-impl-plan.md`).
- [x] `Ui/AppContext.fs` (new) — the `[<ReferenceEquality>]` app-scope composition record
      (five proxies + `settings : WorkbenchSettings`) with `static member create` building
      the stores in the canonical coupling order (samples → materials over
      `samplesReferencing` → categories over `materialsReferencingCategory`, plus the
      library/experiments read seams). Added to the Ui compile list last.
- [x] `App/Program.fs` — `Startup.context = AppContext.create workbenchSettings` (module
      init runs when `App.Initialize` reads `Startup.settings`, i.e. before any window
      opens); `MainConstructorWindow(context : AppContext)` consumes the injected proxies
      via `initMainWith`; `LauncherWindow(context : AppContext)` passes the SAME scope to
      every Main window its button opens; `desktop.MainWindow <-
      LauncherWindow(Startup.context)`. The `open OpticalConstructor.Domain.Library`
      (needed only for in-window proxy construction) removed.
- [x] Ui.Tests re-points for the constructor signature change:
      `WireUiCompositionTests.mountRoot`, `LauncherTests` (private `freshContext` helper,
      two sites), `TableAndElementRotationTests` (one fully-qualified site) — each builds
      a FRESH `AppContext.create WorkbenchSettings.defaults` per mount (test isolation:
      the stores are mutable).
- [x] `Ui.Tests/AppContextTests.fs` (new) — 4 pure pins (`ui-tests` gate): settings
      carried verbatim; materials→LIVE-samples remove-block coupling
      (`MaterialStillReferenced`); two `initMainWith` surfaces from ONE scope share the
      store (a `removeSample` through surface 1 vanishes from surface 2's listing); two
      separate scopes do NOT share. Plus the slice acceptance (`ui-smoke` gate): TWO real
      `MainConstructorWindow`s over ONE context — Materials bay → Add → the REAL Material
      editor → name + Save through window 1; the minted id recovered through the shared
      store; window 2's Materials bay lists the new row, and window 1 does on its next
      render.
- [x] Diagnostic build + all four suite runs green (see Testing state).
- [x] LF check: no CRLF churn in modified files; both new files are LF-only.
- [x] State-of-the-world written (`006-state-of-the-world.md`).

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Ui/AppContext.fs` — NEW (the app-scope
  `*Context` record + `create`).
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/OpticalConstructor.Ui.fsproj` —
  compile entry for `AppContext.fs` (last in the list).
- `Berreman/OpticalConstructor/OpticalConstructor.App/Program.fs` — proxy construction
  hoisted out of `MainConstructorWindow` into `Startup.context`; both window types take
  `AppContext`; launcher wiring passes the scope through.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/WireUiCompositionTests.fs` —
  `mountRoot` injects a fresh context; header doc re-worded for the hoist.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/LauncherTests.fs` — fresh
  context per launcher (new opens + `freshContext` helper).
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/TableAndElementRotationTests.fs`
  — the one real-window mount site passes a fully-qualified fresh context.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/AppContextTests.fs` — NEW
  (4 pure pins + the two-surface headless acceptance).
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/OpticalConstructor.Ui.Tests.fsproj`
  — compile entry for `AppContextTests.fs`.

## Testing state

Gates are executed by the arc-runner's deterministic gate engine after this worker exits
(IMPLEMENT Invariant 6 — the worker acts, it runs no checks). Diagnostic verification
only (not gate authority), logs in `.artifacts/`:

- `dotnet build Berreman.slnx -c Release -nologo -v:m` — **Build succeeded, 0 errors, no
  MSB3277**; the touched projects (Ui / App / Ui.Tests) compiled with ZERO warnings; the
  only remaining warnings are the pre-existing catalogued set in untouched files (NU1701
  Wolfram.NETLink — exempt; FS1125 SeriesDataTests / FS0044 ChartWindow / FS3873
  Dispersion / SYSLIB0051 vendored MathNet — carried for the Part N sweep).
  (`006-diag-build.log`)
- ui-smoke (`--filter Category=ui-smoke`): **114/114** (checkpoint 113, +1 — the new
  two-window shared-store acceptance). (`006-diag-ui-smoke.log`)
- ui-tests (`--filter Category!=ui-smoke`): **352/352** (checkpoint 348, +4 pure
  `AppContext` pins). (`006-diag-ui-tests.log`)
- constructor-unit-tests: **479/479** (== checkpoint; untouched project).
  (`006-diag-constructor-tests.log`)
- unit-tests (BerremanTests): **119 passed / 5 pre-existing skips** (== checkpoint).
  (`006-diag-unit-tests.log`)

First-run failure and fix: the new test file initially failed to compile (FS0039
`Window.MouseDown` unknown) — the headless input helpers are `Avalonia.Headless`
extension methods; `open Avalonia.Headless` added, clean rebuild after.

## Artifacts

- `specs/0038/.artifacts/006-diag-build.log` — diagnostic solution build.
- `specs/0038/.artifacts/006-diag-ui-smoke.log` — diagnostic ui-smoke suite run.
- `specs/0038/.artifacts/006-diag-ui-tests.log` — diagnostic ui-tests suite run.
- `specs/0038/.artifacts/006-diag-constructor-tests.log` — diagnostic constructor suite run.
- `specs/0038/.artifacts/006-diag-unit-tests.log` — diagnostic solver suite run.

## Gotchas

- **`AppContext` lives in `OpticalConstructor.Ui`, not App or Domain** (recorded
  interpretation): the slice's `touches` names App / Ui / Ui.Tests only, and later Part
  C/E/F windows (Materials / Library, declared in Ui per the contract table) must take
  the context as a parameter — App would be a circular reference, Domain is not in
  `touches`. Compiled LAST in the Ui list so future window files can consume both the
  workbench and the context.
- **The context record is `[<ReferenceEquality>]`** — ONE context IS the app scope; two
  structurally-similar contexts are different scopes with independent stores (and the
  five proxy fields are themselves reference-equality function records).
- **"Every window opened later" needs no editor-window change**: the editor windows
  already receive their proxies from the workbench model (`EditorLaunchers.defaults`
  passes `model.materials` / `model.samples` / `model.categories`), and those now ARE the
  app-scope stores — constructor-opened editors write to the shared scope by
  construction.
- **Test isolation preserved by fresh contexts**: every Ui.Tests mount builds its own
  `AppContext.create WorkbenchSettings.defaults` — sharing one context across tests
  would leak store mutations between tests (the remove-refusal proofs mutate).
- **The Material editor's default state is savable with just a name** — the two-surface
  acceptance sets only `nameBox` before Save (the default constant-isotropic complexity
  derives a valid entry); no index edits needed, proven by the green 114th ui-smoke test.
- **`TableAndElementRotationTests` does not open `OpticalConstructor.Ui`** (only the view
  module), so its one mount site uses the fully-qualified
  `OpticalConstructor.Ui.AppContext.create` instead of new opens (avoids any
  TestWindows/Ui name-shadowing risk in a mixed suite).
- Step 002–005 carried-over gotchas remain valid (baselines come from
  `.checkpoints-json`, not the SoW YAML; the `App` type-name ambiguity guard; the
  appsettings.json write-back into test output copies is expected).
