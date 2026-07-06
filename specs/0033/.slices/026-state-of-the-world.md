# State of the world — spec 0033, slice 026

# Where we are

Slice 026 is the arc's WIRE_UI step — the composition acceptance over the
Main-scene root. Slices 022/023 shipped the two editor windows
(SampleEditorWindow / MaterialEditorWindow), slice 024 wired the Materials +
Library workbench bays into the ribbon and threaded the four in-memory proxies
(library, experiments, materials, samples) through `initMainWith` at the
composition root, deferring "the composition acceptance" to this step. This
round delivers that acceptance: the root in `OpticalConstructor.App/Program.fs`
is confirmed final (comment-only touch), and a new ui-smoke composition suite
(`WireUiCompositionTests`) drives the REAL `MainConstructorWindow` headless —
every ribbon bay renders one frame, the Selector / Materials / Library bays
mount over the root-wired seeded stores, Add/Edit open both REAL editor windows
through `EditorLaunchers.defaults`, and a referenced-material remove surfaces
the typed refusal proving the root coupled the materials store to the LIVE
samples store.

# What's working

- WIRE_UI composition acceptance shipped: WireUiCompositionTests drives the REAL MainConstructorWindow headless (ui-smoke 81 → 83)
- Every ribbon bay renders one frame on the real window; Selector / Materials / Library bays mount over the root-wired stores (seeded rows listed)
- Add opens the real Material editor and Edit the real Sample editor via EditorLaunchers.defaults, observed through the public Window.WindowOpenedEvent
- Removing the referenced glass152 through the real window surfaces "still referenced … Glass plate" — the root's samplesReferencing coupling proven end-to-end
- Composition root confirmed final: all four in-memory proxies inject through initMainWith; launcher path unchanged (Program.fs comment-only touch)

# Tests

All gates in the slice roster pass in the worker's local ADVISORY runs
(Invariant 6: the arc-runner gate engine re-runs them authoritatively after
exit). Full logs in `specs/0033/.artifacts/026-*.log`.

- `build` — solution builds Release/x64, exit 0, 0 errors; the 94 warnings are
  the pre-existing MSB3277/NU190x/SYSLIB0051/FS0044/FS3873/FS1125 noise, none
  from this round's files.
- `unit-tests` (BerremanTests) — 119 passed, 5 skipped (pre-existing), 0 failed
  (= 119 baseline; no core file touched).
- `constructor-unit-tests` — 416 passed, 0 failed (= baseline; no UI-less
  project touched).
- `ui-smoke` — 83 passed, 0 failed (81 baseline + 2 new composition-acceptance
  facts).
- `ui-tests` — 306 passed, 0 failed (= baseline; the new file carries only
  ui-smoke facts).

Nothing deferred.

```yaml
gates:
  berreman_unit_tests:    119
  constructor_unit_tests: 416
  ui_smoke_tests:         83
  ui_tests:               306
```

# Architecture

- **The acceptance drives the real root, not a stand-in.** `MainWorkbenchTests`
  (024) proves the bay logic over INJECTED stores and recording launchers;
  `WireUiCompositionTests` (026) proves the COMPOSITION — it constructs the
  real `MainConstructorWindow`, whose constructor builds the four proxies and
  runs the Elmish loop, and asserts only through the semantic tree (UiIds) and
  the opened windows. The two layers stay distinct on purpose.
- **Unowned editor windows are observed through the platform's own seam.**
  `EditorLaunchers.defaults` opens unowned windows, and the headless session
  app has a null `ApplicationLifetime` (no `Windows` list), so the suite
  subscribes to the public global routed event `Window.WindowOpenedEvent`
  (`.Raised` observable) — the same mechanism the desktop lifetime uses for
  window tracking. The subscription is disposable and scoped after the Main
  window opens; the test body runs as one dispatched action on the shared
  headless UI thread, so no cross-test bleed is possible.
- **Wiring proof by data, not by construction.** The four-proxy wiring is
  asserted behaviourally: seeded material/sample rows listed (live stores), the
  Selector bay's projection mounted (library seam), and the
  `MaterialStillReferenced` refusal naming "Glass plate" (the materials store's
  remove-block consulting the LIVE samples store composed at the root).

# Deferred

- Real, disk-backed proxies at the composition root (the 024 note stands:
  they would be built in `OpticalConstructor.Storage` and swapped in at
  Program.fs, leaving scene/bay logic unchanged) — no slice has asked yet.
- Selector-bay BINDING through the real window (select a canvas element, pick
  an entry, confirm) needs canvas-coordinate pointer work; the binding logic is
  already covered headless over the same model in `LibraryControlsTests`. The
  026 acceptance covers the Selector bay's mount/render on the real root, per
  the slice text.
- External UIA / FlaUI screen integration remains out of scope (the WIRE_UI
  family reserves it for screen-integration steps).

# Gotchas

- The task file's system-prompt path
  `C:\GitHub\AI-Strategy-Generator\wire_ui_worker.system-md` does not exist
  (the 015–025 drift continues); the real file is under
  `src\ai_strategy_generator\multistep\`.
- The slice text reads as if the root wiring is new work; slice 024 had already
  landed it verbatim and recorded that "the final WIRE_UI step owns the
  composition acceptance". 026 therefore verifies rather than re-wires —
  Program.fs is a comment-only touch.
- `Window.WindowOpenedEvent.Raised` fires for unowned `Show()` under the
  headless platform — validated by the scratch probe
  `.artifacts/026-scratch-windowopened.fsx` before the suite was written.
- The workbench row clicks need the row inside the scroll viewport — narrow the
  search first (the `MainWorkbenchTests` precedent, reused here).
- `.manifest.state.json` (modified, CRLF-warned) and the untracked `.claude/`
  folder are the arc-runner's / harness's own files — left alone, as in slices
  001–025.

# Changelog

- 2026-07-06 — slice 026: WIRE_UI composition acceptance (spec 0033 step 026):
  the Main-scene composition root confirmed final (Program.fs builds
  SampleProxy.createInMemory + MaterialProxy.createInMemory over
  samplesReferencing beside the library/experiments proxies and injects all
  four through initMainWith; launcher path unchanged; comment-only touch); NEW
  WireUiCompositionTests (ui-smoke) drives the REAL MainConstructorWindow
  headless — every ribbon bay renders one frame, Selector / Materials / Library
  bays mount over the root-wired seeded stores, Add/Edit open both REAL editor
  windows through EditorLaunchers.defaults (observed via the public
  Window.WindowOpenedEvent), and the referenced-material remove surfaces the
  typed "still referenced" refusal end-to-end (ui-smoke 81 → 83); all suites
  green.
- 2026-07-06 — slice 025: RII formula import lowered to editable term data
  (formulas 2–7 parsed and lowered to EpsAxisDispersion; typed
  UnsupportedFormula for 8/9; constructor tests 407 → 416).
- (earlier rounds: see slice 025's state-of-the-world changelog.)
