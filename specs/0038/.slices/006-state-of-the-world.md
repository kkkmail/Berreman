# Step 006 — state of the world

## Where we are

Step 006 completes the composition half of spec 0038 Part C: the five domain proxies
(library / experiments / materials / samples / categories) no longer live inside
`MainConstructorWindow`'s constructor — they are built ONCE at startup, before any window
opens, as the new app-scope `AppContext` record (`OpticalConstructor.Ui/AppContext.fs`,
the `*Context` convention) that also carries the step-005 `WorkbenchSettings`. The
launcher receives `Startup.context` and injects it into every Main window it opens, and
the workbench's editor windows already write through the model's proxies — so
launcher-opened and constructor-opened windows now share the same in-memory stores. The
parameterless `DefaultStores` composition stays the test-scene default. The window-policy
seam (`<SVC:WindowLauncher>` over the WindowRegistry) is the remaining Part C piece for a
later step; the Materials/Library windows of Parts E/F will take this same context.

## What's working

- Add the app-scope AppContext record in OpticalConstructor.Ui: five proxies +
  the step-005 WorkbenchSettings, with create building the stores in the
  canonical coupling order (samples, then materials over samplesReferencing,
  then categories over materialsReferencingCategory).
- Hoist the five-proxy composition out of MainConstructorWindow into
  Startup.context, built once before any window opens; MainConstructorWindow
  and LauncherWindow take the injected AppContext, so every Main click opens
  a view over the SAME stores.
- Keep the parameterless DefaultStores composition as the test-scene default;
  re-point the three Ui.Tests real-window mount sites to fresh per-test contexts.
- Prove the sharing headless: two REAL Main windows over ONE app scope — a
  material added through window 1's real Material editor is listed by window
  2's Materials bay (plus 4 pure AppContext contract pins).
- Suites 479 / 119 / 114 / 352 — ui-smoke +1, ui-tests +4 over the checkpoints.

## Tests

- Gates are executed by the arc-runner's deterministic gate engine after this worker
  exits (IMPLEMENT Invariant 6 — the worker acts, it runs no checks). The roster for
  this step is `build`, `unit-tests`, `constructor-unit-tests`, `ui-smoke`, `ui-tests`.
- Diagnostic verification (not gate authority): `dotnet build Berreman.slnx -c Release`
  succeeded with 0 errors, **no MSB3277**, and zero warnings from any touched project
  (Ui / App / Ui.Tests) — the only warnings are the step-001-catalogued pre-existing set
  in untouched files (FS1125 SeriesDataTests, FS3873 Dispersion, FS0044 ChartWindow,
  SYSLIB0051 vendored MathNet, NU1701 Wolfram.NETLink). Suites: ui-smoke **114/114**
  (checkpoint 113, +1: the two-window shared-store acceptance), ui-tests **352/352**
  (checkpoint 348, +4: the pure AppContext pins), OpticalConstructor.Tests **479/479**
  (== checkpoint), BerremanTests **119 passed / 5 pre-existing skips** (== checkpoint).
- Nothing deferred.

## Architecture

- **One scope, one construction site**: `AppContext.create` is the only place the five
  in-memory stores are composed for the product app (`Startup.context`, module-init'd
  when `App.Initialize` runs — before any window). Real, disk-backed proxies would later
  be built inside `create` (or a Storage-side sibling), leaving every consumer unchanged.
- **The context lives in `OpticalConstructor.Ui`**: later Part C/E/F windows (Materials /
  Library, single-instance over the WindowRegistry) are declared in Ui and must take the
  context as a constructor argument; App would be circular, Domain is outside the slice's
  `touches`. It compiles LAST in the Ui list so future window files can consume both the
  workbench module and the context.
- **`[<ReferenceEquality>]` on the context**: the proxies are reference-equality function
  records and ONE context IS the app scope — identity, not structure, is the meaning.
- **Settings ride the scope**: `AppContext.settings` is now the injection path for the
  step-005 `WorkbenchSettings` — the Part C window-policy seam reads
  `settings.selectWindowModality` from the context a window already holds, no new plumbing.
- **Editor windows share by construction**: `EditorLaunchers.defaults` passes the model's
  proxies into the Material / Sample / Category editors, and the model's proxies ARE the
  app-scope stores — no editor-side change was needed for "every window opened later".

## Deferred

- The `<SVC:WindowLauncher>` window-policy seam + host-layer WindowRegistry
  (single-instance activate / Select re-target / modal-vs-modeless from
  `settings.selectWindowModality`) — the other half of Part C, a later step; today the
  launcher's Main button still opens unconditionally via `.Show()`.
- Consumers of the remaining `WorkbenchSettings` fields (quick-pick §9.5, tree gate §7.6,
  bucket cap §7.5) arrive with Parts D–G.
- The pre-existing warnings in untouched files remain for spec 0038 Part N's sweep
  (carried from steps 002–005).

## Gotchas

- **Fresh context per test, never a shared one**: the stores are mutable; every Ui.Tests
  mount site builds its own `AppContext.create WorkbenchSettings.defaults`. Reusing one
  context across tests would leak mutations (the remove-refusal proofs mutate the store).
- **The Material editor's default state is savable with only a name** — the acceptance
  test's Save-with-name-only path is deliberate and green; if a later step adds required
  fields to the editor, that test tells you first.
- **`Avalonia.Headless` must be opened for `window.MouseDown/MouseUp`** — they are
  extension methods; without the open the compile fails with FS0039 (this round's one
  build fix).
- **`TableAndElementRotationTests` opens the view module, not `OpticalConstructor.Ui`** —
  its mount site fully qualifies `OpticalConstructor.Ui.AppContext.create` instead of
  adding opens (mixed TestWindows/Ui suite; avoids shadowing risk).
- **`Startup.context` initializes with the module** (first touch is `App.Initialize`
  reading `Startup.settings`) — do not move the binding into a window constructor or a
  lazy; "built before any window opens" is the acceptance's load-bearing property.
- Step 002–005 carried-over gotchas remain valid (baselines come from
  `.checkpoints-json`, not the SoW YAML; the `App` type-name ambiguity guard between the
  composition roots; the appsettings.json write-back into test output copies is expected
  and harmless).

## Changelog

- 2026-07-10 — Step 006 (IMPLEMENT, attempt 1): hoisted the five-proxy composition out of
  MainConstructorWindow into the ONE app-scope AppContext record
  (OpticalConstructor.Ui/AppContext.fs) built at startup with the step-005
  WorkbenchSettings and injected into the launcher, every Main window, and (via the
  model's proxies) every editor window — launcher-opened and constructor-opened windows
  share the same in-memory stores; DefaultStores stays the test-scene default; 4 pure
  contract pins + the two-window headless shared-store acceptance added. Build clean (no
  MSB3277, zero touched-project warnings); suites 479 / 119 / 114 / 352.

```yaml
gates:
  berreman_unit_tests: 119
  constructor_unit_tests: 479
  ui_smoke_tests: 114
  ui_tests: 352
```
