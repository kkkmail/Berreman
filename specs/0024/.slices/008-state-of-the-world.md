# 008 state-of-the-world — Lifecycle + environment persistence + 3-D view (Part U8)

```yaml
gates:
  berreman_unit_tests:    84
  constructor_unit_tests: 204
  ui_smoke:                1
  ui_tests:               25
```

# Where we are

Slice 008 carries Part U8 — the FINAL slice of the spec-0024 UI-wiring arc — and lands the
full bundle gate set. U1 (001) stood up the root MVU loop; U5 (002) added the renderer-host
seam (`ChartHosts.fs`); U2–U7 (003–007) wired the stack, materials, sources, results, and
synthesis/fit surfaces. This slice makes the app a usable tool: project Open/Save/New through
the Avalonia `TopLevel.StorageProvider` pickers (held in the host layer, §0.5), the template
wizard and onboarding gallery through the existing schema-validated factories, live environment
persistence (theme/panel/dock) through the pure `AppShell` reducers plus a fire-and-forget
`UserEnvironment.save`, and a first 3-D system view rendered as a 2-D orthographic projection on
the slice-002 Canvas host (no GL). All IO/persistence/geometry seams already existed and are
frozen (§0.1); this slice renders and wires them in new sibling `*View.fs` modules and the
composition root only.

# What's working

- Add `LifecycleView.fs`: owns the `IoMsg` the shell wraps as `RootMsg.Io` and the lifecycle
  toolbar (New / Open / Save + the template and gallery palettes + an IO status line).
- Wire Open/Save/New in `Shell.fs` (R-1 / AC-U8.1): Save serializes via the frozen
  `ConstructionPage.saveProject` and writes `<name>.ocproj.json`; Open reads through the
  schema-validated `ProjectFile.openProject`; the `IStorageProvider` lives in a host-layer field
  (§0.5) and every IO continuation marshals back via `Dispatcher.UIThread.Post` (§0.4).
- Load templates via `Templates.loadTemplate` and gallery samples via `Help.openEntry` (R-2 /
  AC-U8.2) — the existing schema-validated factories, no private deserialize.
- Persist the environment live (R-3 / AC-U8.3): theme/panel/dock changes flip `model.env`
  through the pure `AppShell` reducers and attach a fire-and-forget `UserEnvironment.save` `Cmd`.
- Add `SystemView3DView.fs` (R-4 / AC-U8.4): a 2-D orthographic top-view of
  `SystemView3D.placeElements` + `beamSegments` (read from the already-solved per-node fields)
  on the slice-002 `ChartHosts.canvasHost` — no GL, so `ui-smoke` stays headless-green.
- Add 6 `ui-tests` (Save↔Open round-trip, template/gallery load, env persist, 3-D render) and
  keep every bundle gate green; `Program.fs` registers the window storage provider on open.

# Tests

All bundle gates pass locally (run in order; first failure short-circuits):

- `build` — solution builds, 0 errors (only the pre-existing MSB3277 WindowsBase/WebView2 and
  FS1125 SeriesDataTests warnings — inherited, not introduced).
- `unit-tests` (BerremanTests) — 84 passed / 5 skipped; baseline `berreman_unit_tests = 84`.
- `constructor-unit-tests` (OpticalConstructor.Tests) — 204 passed; U8 keeps the pure suite
  Avalonia-free.
- `ui-smoke` — 1 passed: the real App boots headlessly and every panel/page renders one frame,
  including the lifecycle toolbar and the 3-D view hosted in the results panel.
- `ui-tests` — 25 passed (prior 19 + 6 new U8 tests): Save writes `<name>.ocproj.json` and Open
  round-trips the schema-validated path (AC-U8.1); the Save button dispatches `Io SaveRequested`
  (R-1); a template loads via `Templates.loadTemplate` and a gallery entry via `Help.openEntry`
  (AC-U8.2); theme/panel/dock changes flip `env`, attach a persist `Cmd`, and the persisted env
  survives a reload (AC-U8.3); the 3-D view renders element boxes + beam lines on a Canvas
  without GL (AC-U8.4).

# Architecture

- `LifecycleView` follows the slice-002..007 view precedent: it takes its sub-message dispatch
  (`IoMsg -> unit`), NOT `RootModel`, so it composes under the root without a module cycle;
  `Shell` passes `RootMsg.Io >> dispatch`. The toolbar dispatches only the *trigger* cases; the
  IO *result* cases (`OpenPath`/`Loaded`/`Saved`/`IoError`) are posted by the `Shell.fs` `Cmd`s.
- The `IStorageProvider` (R-1) and the persist target path (R-3) are host-layer module fields in
  `Shell.fs` — the same "host field" pattern slice-007's `fitCts` established (§0.5). The picker
  is only a *path source*: the read/write logic lives in `Io (OpenPath …)` / `Io SaveRequested`,
  so the IO is fully exercised headlessly with the picker absent. Test seams
  (`clearStorageProviderForTests`, `setEnvironmentPathForTests`) keep the round-trips deterministic
  and off the real per-user state.
- The 3-D view is a pure render-boundary projection (§A.3): it maps the frozen `SystemView3D`
  viewport `Vec3` onto the Canvas (horizontal = beam-path `z`, vertical = transverse `x`) and
  consumes the construction page's already-solved `results` map for `beamSegments` — no re-solve,
  no geometry recomputed, no non-SI value written back. `preorderPaths` mirrors `placeElements`'
  DFS order exactly so each placed element pairs with the correct solved node.
- Persistence reuses the existing seams end-to-end: `ConstructionPage.saveProject` /
  `ProjectFile.openProject` for projects, `UserEnvironment.save`/`load` for the environment —
  no new committable file type (§0.6); Save writes the existing `<name>.ocproj.json`.

# Deferred

- **Real OpenTK GL viewport** for the 3-D view (OPTIONAL follow-up; MUST NOT block U8, §U8.4):
  a GL surface cannot render headlessly, so the 2-D-orthographic Canvas projection is delivered
  instead and the `ui-smoke` gate stays green.
- **Live file-picker exercise headlessly**: the `IStorageProvider` pickers cannot run on the
  headless platform, so the IO `Cmd`s are tested through their path-driven seams; the picker is
  wired live only on the desktop host (registered by `Program.fs`).
- **Degenerate-branch beam rays**: a near-fully-transmitting normal-incidence stack (e.g. the
  default seed) has a Poynting norm below `almostZero`, so `beamSegments` drops its branches by
  design (`SystemView3D.beamDirection -> None`) — the 3-D view then shows boxes without rays.
- The clone-swap decision (§0.2 / spec 0022 §A.9 / AC-A8) — untouched.

# Gotchas

- **Host `IStorageProvider` is a process-shared static.** The `ui-smoke` test boots the real App
  and registers a provider on window-open, so the Save round-trip test calls
  `Shell.clearStorageProviderForTests ()` to force the deterministic working-folder fallback;
  without it the Save `Cmd` would take the (headless-unrenderable) picker branch.
- **The 3-D view's beams come from `construction.results`, not a re-solve.** It looks each placed
  node's solved `EmFieldSystem` up by path; a node missing from `results` (e.g. just-attached,
  not yet `NodeSolved`) draws its box but no rays until the solve lands.
- **`environmentPath` test seam is a shared mutable.** Only shell theme/panel/dock messages
  trigger the persist `Cmd`, and only the AC-U8.3 test redirects the path, so there is no
  cross-test contamination; the production default remains `UserEnvironment.settingsPath ()`.
- **`ui_tests` baseline rose 19 → 25** (6 new U8 tests). `berreman_unit_tests` (84),
  `constructor_unit_tests` (204), and `ui_smoke` (1) are unchanged. This is the final slice of
  the arc; the baseline is emitted for completeness.

# Changelog

- 2026-06-01 — Slice 008: wired Part U8 (the final slice) — Open/Save/New via the
  `TopLevel.StorageProvider` pickers (host-held §0.5, §0.4-marshaled `Cmd`s; Save through
  `ConstructionPage.saveProject` writing `<name>.ocproj.json`, Open through the schema-validated
  `ProjectFile.openProject`); the template wizard / onboarding gallery via `Templates.loadTemplate`
  / `Help.openEntry`; live environment persistence (theme/panel/dock → pure `AppShell` reducers +
  a fire-and-forget `UserEnvironment.save`); and a first 3-D system view as a 2-D orthographic
  projection of `placeElements`/`beamSegments` on the slice-002 Canvas host. New
  `LifecycleView.fs` + `SystemView3DView.fs`; composition-root edits in `Shell.fs` / `Program.fs`;
  no frozen module touched (§0.1). Added 6 `ui-tests`; extended nothing frozen. All bundle gates
  green (build 0 errors; berreman 84, constructor 204, ui-smoke 1, ui-tests 25).
