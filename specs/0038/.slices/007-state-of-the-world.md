# Step 007 — state of the world

## Where we are

Step 007 continues spec 0038 Part C by declaring the window-policy seam
`SVC_XDUO_0001 WindowLauncher` in `OpticalConstructor.Ui` (ADD_CONTRACT —
lifecycle `declared`; step 008 implements it for real over the host-layer
`WindowRegistry`, step 016 adds the Select-state re-target). The seam
generalizes the workbench's `EditorLaunchers` function-record precedent into
one contract: `WindowKey` names every managed window (four single-instance
keys plus id-keyed editor keys, so the registry keys uniformly by the edited
entity's elevated id), `EntryFreshness = NewUnsaved | Persisted` moves the
id-mint to Add-window open, and the `[<ReferenceEquality>]` `WindowLauncher`
record carries the three Result-returning functions (open-or-activate through
an injected per-key factory, forget-on-close, decide Select-state modality
from the step-005 `SelectWindowModality`). A mock over recording stub
factories plus an 8-test mock-driven Ui.Tests suite pin the surface, including
both slice-acceptance observations.

## What's working

- Declare SVC_XDUO_0001 in Ui/WindowLauncher.fs: WindowKey (Materials /
  Library / SolverHandoff / CategoryEditor + MaterialEditorKey of MaterialId /
  SampleEditorKey of SampleId), EntryFreshness = NewUnsaved | Persisted, the
  typed WindowLauncherError, and the WindowLaunchOutcome DU with the step-016
  RetargetedWindow case declared upfront.
- Declare the [<ReferenceEquality>] WindowLauncher record — openOrActivate /
  forgetWindow / decideSelectModality, all camelCase and Result-returning.
- Provide WindowLauncher.createMock: registry bookkeeping over a private ref
  Map with window construction delegated to a caller-injected recording stub
  factory and the step-005 modality switch handed back verbatim.
- Pin the contract mock-driven in Ui.Tests/WindowLauncherTests.fs: a second
  open of the same WindowKey activates instead of creating (all six key
  shapes), two different editor keys create two windows, forget-then-reopen
  creates afresh, plus the pure modality / typed-error / id-keying pins —
  every record field exercised through its exact signature.
- Suites 479 / 119 / 117 (+3) / 357 (+5); build clean, no MSB3277, zero
  warnings from touched projects.

## Tests

- Gates are executed by the arc-runner's deterministic gate engine after this
  worker exits (ADD_CONTRACT Invariant 6 — the worker acts, it runs no checks).
  The roster for this step is `build`, `unit-tests`, `constructor-unit-tests`,
  `ui-smoke`, `ui-tests`.
- Diagnostic verification (not gate authority): `dotnet build Berreman.slnx -c
  Release` succeeded with 0 errors, **no MSB3277**, and zero warnings from the
  touched projects (Ui / Ui.Tests) — the only warnings are the
  step-001-catalogued pre-existing set in untouched files (FS1125
  SeriesDataTests, FS3873 Dispersion, FS0044 ChartWindow, SYSLIB0051 vendored
  MathNet, NU1701 Wolfram.NETLink). Suites: ui-smoke **117/117** (checkpoint
  114, +3), ui-tests **357/357** (checkpoint 352, +5), OpticalConstructor.Tests
  **479/479** (== checkpoint), BerremanTests **119 passed / 5 pre-existing
  skips** (== checkpoint). Logs in `specs/0038/.artifacts/007-diag-*.log`.
- Nothing deferred.

## Architecture

- **The contract is a Ui-layer proxy record, module-wrapped** (`module
  WindowLauncher` under `namespace OpticalConstructor.Ui`, the
  `WorkbenchSettings` precedent): windows are host-layer objects, so the seam
  lives in Ui, next to `AppContext` (compiled after it, so step 008's real
  implementation can bake the app scope's settings in).
- **The registry keys by identity from the first moment**: `WindowKey` has
  structural equality/comparison (Guid-backed elevated ids), and an Add-opened
  editor mints its id AT WINDOW OPEN carrying `NewUnsaved` — so single-instance
  and per-entity windows share one open-or-activate semantics, and Save routing
  (step 008) becomes a match on `EntryFreshness`, never a bool.
- **The outcome DU is the observation channel**: `CreatedWindow` /
  `ActivatedWindow` / `RetargetedWindow` carry the live window, so callers (and
  tests) see WHAT the launcher did without reaching into any registry state;
  `RetargetedWindow` is declared now (compiler-complete matches under
  `--warnaserror` FS0025) and first produced by step 016.
- **The mock is the contract's executable spec**: full open-or-activate/forget
  bookkeeping over a `ref Map` (the `createInMemory` precedent), zero real
  windowing behaviour (no Activate/Show/ShowDialog/Closed) — that boundary is
  exactly what step 008's IMPLEMENT_CONTRACT adds.

## Deferred

- The real launcher over the host-layer `WindowRegistry` (module-level mutable
  `Map<WindowKey, Window>`, `Activate`, unregister-on-`Closed`, `Show` vs
  `ShowDialog` owner wiring, the workbench verb rewiring subsuming
  `EditorLaunchers.defaults`) — step 008, as the anchor prescribes.
- Select-state re-target semantics (`RetargetedWindow` production) and the
  staleness rules — step 016.
- The `SolverHandoffWindowKey` target window itself — Part L (step 39).
- Nothing in this round consumes the seam yet: `declared` lifecycle means the
  surface + mock + pinning tests exist and compile; product behaviour is
  unchanged by design.
- The pre-existing warnings in untouched files remain for spec 0038 Part N's
  sweep (carried from steps 002–006).

## Gotchas

- **The task file's system-prompt path was stale** — the ADD_CONTRACT worker
  prompt actually lives under
  `src/ai_strategy_generator/multistep/add_contract_worker.system-md` in the
  tool repo; located and read in full.
- **`.contracts-json` is supervisor-maintained and already carried
  SVC_XDUO_0001 as `declared` at step 7** — the worker records the contract by
  shipping the declared surface, not by editing the registry file.
- **`forgetWindow` of an absent key is the typed `WindowNotRegistered` error,
  not a no-op** (recorded interpretation) — a double-unregister or a
  forget-vs-close race is visible in a log; step 016's staleness logic only
  forgets keys it observed live.
- **`decideSelectModality` is Result-returning although it cannot fail today**
  — the slice mandates a record of Result-returning functions; uniformity lets
  steps 008/016 add failure modes without a signature break.
- **Pure pins vs headless proofs split by `Window` construction**: a bare
  `Window()` needs the headless platform, so every test whose stub factory
  builds windows runs inside `HeadlessSession.run` under the `ui-smoke` trait;
  the pure signature pins (whose factories fail or must not be reached) run
  session-free under `ui-tests`.
- Step 002–006 carried-over gotchas remain valid (baselines come from
  `.checkpoints-json`, not the SoW YAML; the `App` type-name ambiguity guard;
  the appsettings.json write-back into test output copies is expected).

## Changelog

- 2026-07-10 — Step 007 (ADD_CONTRACT SVC_XDUO_0001, attempt 1): declared the
  WindowLauncher window-policy seam in OpticalConstructor.Ui — WindowKey
  (single-instance + id-keyed editor keys), EntryFreshness (NewUnsaved |
  Persisted, id-mint at Add-open), the typed error/outcome DUs (incl. the
  step-016 RetargetedWindow case), the [<ReferenceEquality>] record of three
  Result-returning functions, and the createMock registry double over
  recording stub factories; 8 mock-driven Ui.Tests pins (5 pure + 3 headless)
  incl. both acceptance observations. Build clean (no MSB3277); suites
  479 / 119 / 117 / 357.

```yaml
gates:
  berreman_unit_tests: 119
  constructor_unit_tests: 479
  ui_smoke_tests: 117
  ui_tests: 357
```
