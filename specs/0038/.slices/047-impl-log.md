# Impl-log — 047 WIRE_UI (AC-W1)

## Progress
- [x] Read system prompt (`wire_ui_worker.system-md` + base `arc-runner.system-md`), project
  prompt (`arc-runner.user-md`), slice spec (`047.slice-md`), the authoritative step from
  `.spec-jsonl`, prior SoW (046), CLAUDE.md.
- [x] Surveyed the composition root (`Program.fs`, `AppConfig.fs`, `AppContext.fs`), the
  launcher seam (`WindowLauncher.fs`, `EditorLaunchers`), the window constructors
  (Materials/Library Browse+Select, the three editors, SolverHandoff), and the existing
  ui-smoke coverage — 177 tests already render the whole surface per-window.
- [x] Added `SceneProxy.createInMemory` (step 27) to `AppContext.create` — the only proxy that
  was missing from "every proxy built once at the root."
- [x] Wrote the consolidated composition-root wiring assertion in Ui.Tests + registered in fsproj.
- [x] Verified the full-solution Release build compiles (`.artifacts/047-build.log`) and that the
  new wiring assertion renders headless (`.artifacts/047-wire-tests.log`).

## Files modified
- `OpticalConstructor.Ui/AppContext.fs` — added the `scenes : Scene.SceneProxy` app-scope field
  and built it once via `Scene.SceneProxy.createInMemory ()` in `AppContext.create`. Additive:
  `AppContext.create`'s signature is unchanged, so `Program.fs` and every existing caller (all
  go through `.create`, none construct the record literal) compile untouched.
- `OpticalConstructor.Ui.Tests/WireUiFinalCompositionTests.fs` — NEW. The closing
  composition acceptance: (1) a `ui-tests` pin that the finalized app scope carries every store
  proxy built once, incl. the step-027 scene seam; (2) three `ui-smoke` proofs that drive the
  whole root-wired surface — launcher + both constructor states; the ONE root window-launcher
  factory opening + rendering Materials, Library, the three editors and the SolverHandoff over
  the shared `WindowRegistry`; both faceted windows in Select state — each over ONE app scope
  composed exactly like `Startup.context`, without throwing.
- `OpticalConstructor.Ui.Tests/OpticalConstructor.Ui.Tests.fsproj` — registered the new file last.

## Testing state
- **build** — GREEN locally. Full Release build of `Berreman.slnx`: **0 Error(s), 4 Warning(s)**,
  all four exempt/third-party (`NU1701` ×2 BerremanRunner→Wolfram.NETLink; `SYSLIB0051` ×2
  vendored MathNet `Numerics.csproj`). No new our-code FS/MSBuild warning; §0.6 holds.
  `OpticalConstructor.Ui` (the AppContext change) and `OpticalConstructor.Ui.Tests` (the new
  test) both compiled. Captured: `.artifacts/047-build.log`.
- **the new wiring assertion** — GREEN locally: `--filter FullyQualifiedName~WireUiFinalCompositionTests`
  → **Passed 4, Failed 0** (1 ui-tests pin + 3 ui-smoke render sweeps). Captured:
  `.artifacts/047-wire-tests.log`.
- **unit-tests / constructor-unit-tests / the full ui-smoke / ui-tests suites** — NOT re-run
  (Invariant 6 / WIRE_UI "act only": the worker runs no gates; the arc-runner's deterministic
  gate engine runs every step gate after exit). All changes are additive — a new `AppContext`
  field with no record-literal construction site, and a new test file — and the solution builds
  clean, so the prior baselines carry forward and rise by this round's four new tests (+1 ui-tests,
  +3 ui-smoke). `commit_ready: true`.

## Artifacts
- `.artifacts/047-build.log` — full Release build capture (0 errors, 4 exempt warnings).
- `.artifacts/047-wire-tests.log` — the new wiring assertion run (4 passed).

## Gotchas
- **Scope vs the declared `touches`.** The step declares `touches: [OpticalConstructor.App,
  OpticalConstructor.Ui.Tests]`, but the how_to explicitly requires "the app scope (step 6) with
  … `SceneProxy.createInMemory` (27)." The app scope IS `AppContext` (`OpticalConstructor.Ui`),
  one project outside the declared list. The how_to is the more specific instruction and governs,
  so `AppContext.fs` was touched to compose the scene proxy. `Program.fs` needed NO change — it
  already creates the `AppSettingsProvider` once (`AppConfig`), builds `AppContext.create` once
  (`Startup.context`), and hands the context to the launcher window + both constructor states;
  `AppContext.create`'s signature is unchanged, so the new field flows in transparently. This is
  consistent with WIRE_UI "assemble existing surfaces, add no new behaviour": the only production
  edit is composing an already-built, unit-proven proxy (`Scene.SceneProxy.createInMemory`,
  step 027) at the root.
- **Why the SceneProxy has no product consumer yet.** `SceneProxy` (STORE_XDUO_0004) is composed
  at the root per the acceptance ("every proxy built exactly once at the root"), exactly as
  `experimentCollections` was composed before its builder fully wired in. The scene save/load
  surface that consumes it is a later cycle; a future disk-backed `create` swaps in with no change
  to any consumer. It is NOT dead-code churn — it is the declared seam the composition contract
  requires at the app scope.
- **The whole surface was ALREADY rendered by 177 ui-smoke tests** across the per-window suites
  (MaterialEditor 19, Library 19, SampleEditor 17, Materials 13, Category 4, SolverHandoff 2,
  Launcher 5, Inverse 1, WireUiComposition 3, AppContext 1, …). Step 047's contribution is the
  CONSOLIDATED wiring assertion that drives the whole surface over ONE root-composed app scope
  (the WIRE_UI family's "headless wiring assertion"), not re-proving each window in isolation.
- **Registry hygiene.** The `ui-smoke` factory sweep opens each window through the REAL
  `WindowLauncher.create … BrowseOpen` over the app-global module-level `WindowRegistry`, then
  CLOSES it and pumps the close so its `Closed` hook unregisters the key — otherwise a later
  suite's open of the same key would activate a stale window. This follows the `WindowLauncherTests`
  discipline exactly; the Select-state proofs use direct construction (no registry).
- **Modality.** The Select-state opens run under `AppContext.settings.selectWindowModality`
  (default `ModelessSelectWindows` → an unowned `Show`), so no modal `ShowDialog` blocks the
  headless session.
- **Line endings.** The two edited/added `.fs` files and the `.fsproj` are pure LF (verified:
  0 CRLF), matching the repo's `.gitattributes` LF policy — no EOL churn introduced.
