# State of the world — spec 0035, slice 019

# Where we are

Slice 019 is arc 0035's `WIRE_UI` step — the **composition acceptance** over the
Main-scene root for the category catalogue. Its dependencies had landed the
pieces: slice 009 host-wired the `CategoryProxy` into the
`TableAndElementRotationView` host model, threaded it through
`initWith` / `initMainWith` / `DefaultStores` and the App `Program.fs`
composition, and gave the Materials bay its **Categories…** verb that opens the
step-6 `CategoryEditorWindow` through the `EditorLaunchers` seam; slice 008
reordered the Materials / Library bays to full-surface and last; slice 016 shipped
the workbench surfaces. Slice 009 explicitly deferred "the App composition
ACCEPTANCE (root-wiring the CategoryProxy and the full ui-smoke suite over the real
window)" to this step. This round delivers that acceptance: the five-proxy root in
`OpticalConstructor.App/Program.fs` is confirmed final (comment-only touch), and
the ui-smoke composition suite (`WireUiCompositionTests`) now drives the REAL
`MainConstructorWindow` headless to open the third editor window — the Materials
bay's **Categories…** verb opens the real Category editor over the root-wired
`CategoryProxy`, beside the already-covered Material / Sample editors and the
reordered full-surface bay sweep.

# What's working

- Confirm the composition root final: all FIVE in-memory proxies (library /
  experiments / materials / samples / categories) inject through initMainWith;
  launcher path unchanged (Program.fs comment-only touch).
- Extend WireUiCompositionTests to open the third editor: Categories… on the REAL
  Main window opens the real Category editor over the root-wired CategoryProxy
  (ui-smoke 105 → 106).
- Prove the CategoryProxy is wired at the root: the Categories… launcher receives
  model.categories (composed over materialsReferencingCategory) and mounts the real
  CategoryEditorWindow, observed through the public Window.WindowOpenedEvent.
- Keep the reordered full-surface Materials / Library bays and Material / Sample
  editor acceptance intact — the new fact only adds the Category editor path.

# Tests

Per the WIRE_UI worker role (Invariant 6 — the worker ACTS and runs no checks),
the `build` / `unit-tests` / `constructor-unit-tests` / `ui-smoke` / `ui-tests`
gates are executed by the arc-runner's deterministic gate engine AFTER this worker
exits; they were NOT run as gates here. The counts below are the EXPECTED
post-round baselines.

- `build` — unchanged surface: one new ui-smoke fact reusing the file's existing
  seams (`mountRoot` / `Window.WindowOpenedEvent` / `clickOn`) plus comment-only
  edits; no new reference, no new project.
- `unit-tests` (BerremanTests) — no core file touched; baseline carries forward
  (119).
- `constructor-unit-tests` — no UI-less project touched; baseline carries forward
  (447).
- `ui-smoke` — 105 baseline + 1 new composition-acceptance fact = **106**.
- `ui-tests` — no non-ui-smoke view test changed; baseline carries forward (329).

Nothing deferred from this slice's scope.

```yaml
gates:
  berreman_unit_tests: 119
  constructor_unit_tests: 447
  ui_smoke_tests: 106
  ui_tests: 329
```

# Architecture

- **The acceptance drives the real root, not a stand-in — now for the third
  editor too.** `WireUiCompositionTests` constructs the real
  `MainConstructorWindow`, whose constructor builds the five proxies and runs the
  Elmish loop, and asserts only through the semantic tree (UiIds) and the opened
  windows. The new fact proves the `CategoryProxy` root-wiring behaviourally: the
  `EditorLaunchers.defaults.openCategoryEditor` launcher receives `model.categories`
  — the proxy the root composed over `materialsReferencingCategory` — so opening the
  real `CategoryEditorWindow` from the Materials bay's Categories… verb is
  end-to-end proof of the root composition. This stays distinct from
  `MainWorkbenchTests` (bay logic over INJECTED stores) and
  `CategoryEditorWindowTests` (the editor over a STUB proxy).
- **Unowned editor windows are observed through the platform's own seam.** The new
  fact reuses the established pattern: the headless session app has a null
  `ApplicationLifetime`, so the suite subscribes to the public global routed event
  `Window.WindowOpenedEvent` (`.Raised` observable) — the same mechanism the desktop
  lifetime uses for window tracking — scoped after the Main window opens.

# Deferred

- Real, disk-backed proxies at the composition root (the 024/026 note stands: they
  would be built in `OpticalConstructor.Storage` and swapped in at `Program.fs`,
  leaving scene/bay logic unchanged) — no slice has asked yet.
- Driving the Category editor's own verbs (Add / Rename / Remove) from inside the
  composition-mounted editor is already covered headless over the same window in
  `CategoryEditorWindowTests`; the 019 acceptance covers only that the editor
  OPENS from the root, per the slice text.
- External UIA / FlaUI screen integration remains out of scope (the WIRE_UI family
  reserves it for screen-integration steps).

# Gotchas

- The composition root was **already wired by slice 009**, not new work this round;
  step 019 owns the ACCEPTANCE, so `Program.fs` is a comment-only touch (the
  0033/026 WIRE_UI precedent). The slice text reads as if the root wiring is new;
  the code and slice-009 SoW already record it landed, so this round verified
  rather than re-wired (base §7 skepticism rule).
- The task-file system-prompt path
  `C:\GitHub\AI-Strategy-Generator\wire_ui_worker.system-md` does not exist; the
  real file is under `src\ai_strategy_generator\multistep\` (the drift noted since
  slice 015 continues).
- The Categories… verb is a host-added clickable `Border` (`workbenchButton`,
  AutomationId `ManageCategoriesButton`), the same shape `clickOn` already drives
  for the Add / Remove-confirm buttons — no new interaction pattern.
- `.manifest.state.json` (modified, CRLF-warned) and the untracked `.claude/`
  folder are the arc-runner's / harness's own files — left alone, as in prior
  slices.

# Changelog

- 2026-07-08 — slice 019: WIRE_UI composition acceptance for the category
  catalogue. Confirmed the five-proxy Main-scene root final (Program.fs builds
  SampleProxy → MaterialProxy (over samplesReferencing) → CategoryProxy (over
  materialsReferencingCategory) beside library/experiments and injects all five
  through initMainWith; launcher path unchanged; comment-only touch). NEW
  WireUiCompositionTests ui-smoke fact drives the REAL MainConstructorWindow
  headless — the Materials bay's Categories… verb opens the real Category editor
  over the root-wired CategoryProxy (observed via the public Window.WindowOpenedEvent),
  proving the third editor window opens without throwing and the CategoryProxy is
  wired at the root (ui-smoke 105 → 106); other suites unchanged.
- 2026-07-08 — slice 018: warning-clean sweep — dropped the WebView2 package
  reference from OpticalConstructor.Ui to clear the MSB3277 WindowsBase conflict
  across App/Ui/Tests/Ui.Tests (no compiled behaviour changed).
- (earlier rounds: see slice 018's state-of-the-world changelog.)
