# Step 008 — state of the world

## Where we are

Step 008 completes the window-policy half of spec 0038 Part C: the step-007
`SVC_XDUO_0001 WindowLauncher` contract is now IMPLEMENTED (lifecycle
`implemented`). The real launcher runs over the host-layer `WindowRegistry` —
ONE module-level mutable `Map<WindowKey, Window>` outside any Elmish model — so
a second open of any managed key ACTIVATES the live window (`Window.Activate`),
a create invokes the injected per-key factory, registers, unregisters on
`Closed`, and shows per policy: Browse opens always `Show`; Select-state opens
follow the step-005 `SelectWindowModality` switch (`ShowDialog` owned by the
requesting window / `Show`). The id-mint moved OFF the save path: an Add-opened
editor mints its `MaterialId`/`SampleId` AT WINDOW OPEN, carries
`EntryFreshness = NewUnsaved`, and Save routes `addMaterial`/`addSample` for
`NewUnsaved` vs `updateMaterial`/`updateSample` for `Persisted`. The workbench
Add/Edit/Categories verbs all open through the launcher — subsuming
`EditorLaunchers.defaults` — so editing the same entity twice meets one window.
Step 016 adds the Select-state re-target; Parts E/F add the single-instance
Materials/Library windows over the same registry.

## What's working

- Implement WindowLauncher.create (SVC_XDUO_0001 → implemented) over the ONE
  host-layer WindowRegistry: activate the live window on a registered key,
  create-register-show through the injected factory, reference-guarded
  unregister on Closed; platform throws mapped to the typed error at the
  boundary.
- Show policy per WindowOpenMode: Browse opens always Show; Select-state opens
  honor ModalSelectWindows via ShowDialog with the requesting window as owner
  and ModelessSelectWindows via unowned Show — pinned headless both ways.
- Move the id-mint to window open: MatAdd/SmpAdd/SmpMakeMultilayer mint
  MaterialId/SampleId at the verb dispatch into the new editor intents; both
  editors' Save routes add vs update on EntryFreshness under the upfront id.
- Rewire EditorLaunchers.defaults through the launcher: Edit of the same
  material twice activates ONE editor window; two Adds create two NewUnsaved
  windows whose distinct upfront Guids persist through addMaterial (and a
  sample Add through addSample) — all proven end-to-end headless.
- Suites 479 / 119 / 124 (+7) / 361 (+4); build clean, no MSB3277, zero
  warnings from touched projects.

## Tests

- Gates are executed by the arc-runner's deterministic gate engine after this
  worker exits (IMPLEMENT_CONTRACT Invariant 6 — the worker acts, it runs no
  checks). The roster for this step is `build`, `unit-tests`,
  `constructor-unit-tests`, `ui-smoke`, `ui-tests`.
- Diagnostic verification (not gate authority): `dotnet build Berreman.slnx -c
  Release --no-incremental` succeeded with 0 errors, **no MSB3277**, and zero
  warnings from the touched projects (Ui / App / Ui.Tests) — the only warnings
  are the step-001-catalogued pre-existing set in untouched files (FS1125
  SeriesDataTests ×4, FS3873 Dispersion, FS0044 ChartWindow, SYSLIB0051 vendored
  MathNet ×2, NU1701 Wolfram.NETLink ×2). Suites: ui-smoke **124/124**
  (checkpoint 117, +7: three real-registry proofs — activate / close-unregisters
  / stale-close guard — the Select-modality show pin, and the three end-to-end
  acceptance proofs through the rewired defaults), ui-tests **361/361**
  (checkpoint 357, +4: real-launcher modality/forget/factory-failure pins and
  the two-Adds-mint-distinct-ids pin), OpticalConstructor.Tests **479/479**
  (== checkpoint), BerremanTests **119 passed / 5 pre-existing skips**
  (== checkpoint). Logs in `specs/0038/.artifacts/008-diag-*.log`.
- Nothing deferred.

## Architecture

- **One registry, many cheap launcher views**: the declared `openOrActivate`
  carries only the `WindowKey`, so per-open context (Browse vs Select, the
  requesting owner) is baked into each `create`d record via the new
  `WindowOpenMode` — while single-instance semantics across ALL composition
  sites comes from the ONE module-level `Map`. That is why the slice mandates a
  module-level registry rather than a per-instance ref (the mock keeps its
  private `ref Map` as the isolated double).
- **The id IS the key, from open to save**: the verbs mint at the window-open
  dispatch into `MaterialEditorIntent` / `SampleEditorIntent`; the launcher keys
  the registry by that id; the editor's `EditorTarget` record carries
  `{ id; freshness }`; Save matches the freshness. One Guid names the entity
  through its entire window lifecycle — never re-minted downstream.
- **`EditorLaunchers` stays the Elmish verb seam**; only its `defaults` were
  subsumed — they now build a per-dispatch launcher over the shared registry.
  Tests keep their recording-substitution point; the model shape is unchanged.
- **Errors stay values across the seam**: factory throws, `Activate`, and
  `Show`/`ShowDialog` failures are caught at the launcher boundary and mapped
  onto `WindowFactoryFailed(key, reason)` (doc widened; no new DU case, so no
  FS0025 ripple under `--warnaserror+:25`), unwinding the registration so the
  next open retries.
- **`WindowLauncher.fs` compiles before the editor views** (moved up the Ui
  compile list): the views route Save on `EntryFreshness` and the workbench
  defaults call `create` — nothing in the file needs `AppContext`, so the
  step-007 "after AppContext" placement was retired with the reason recorded.

## Deferred

- Select-state windows themselves (Materials/Library Browse+Select modes, the
  `RetargetedWindow` production, staleness rules) — steps 012/013/015/016; the
  `SelectOpen` mechanism and the modality switch behaviour are implemented and
  pinned now, composed by step 016 over `AppContext.settings`.
- The `SolverHandoffWindowKey` target window — Part L (step 39).
- The launcher's Browse-mode `defaults` bake `SelectWindowModality.defaultValue`
  (never consulted on the Browse path); the app-scope switch rides
  `AppContext.settings` for the step-016 composition.
- The pre-existing warnings in untouched files remain for spec 0038 Part N's
  sweep (carried from steps 002–007).

## Gotchas

- **The task file's system-prompt path was stale again** — the
  IMPLEMENT_CONTRACT worker prompt lives under
  `src/ai_strategy_generator/multistep/implement_contract_worker.system-md` in
  the tool repo; located and read in full.
- **The registry is app-global test state**: every real-registry test closes the
  windows it opens (close → `Closed` → unregister). Deterministic seed ids
  (glass152, glassFilm600) are registry keys in Edit flows — a test that leaks
  one couples later tests, so close editors in tests that open them through
  `EditorLaunchers.defaults`.
- **The `Closed` unregister hook is reference-guarded** — it removes the key only
  while the registry still points at the closing window; a stale window's late
  close cannot drop its successor's registration (pinned headless).
- **The typed open error is dropped at the unit-returning verb seam**
  (`openBrowse … |> ignore` in `defaults`): a failed open leaves no window — the
  same user-visible outcome as the pre-move `.Show()` crash path, minus the
  crash. If a later slice wants the refusal surfaced, the workbench needs an
  error channel for launcher failures.
- **`.contracts-json` was updated by this worker** (SVC_XDUO_0001 →
  `implemented`, implementStep 8) per the IMPLEMENT_CONTRACT registry
  obligation; the file's working-copy CRLF style (supervisor-written) was
  preserved — the LF-vs-CRLF warnings on the two spec JSON files pre-date this
  round and are not source-file churn.
- **`MaterialEditorWindow`'s second ctor argument changed shape**
  (`MaterialEntry option` → `MaterialEditorIntent`) and both NEW
  `SampleEditorIntent` cases now carry `mintedId` — any out-of-tree caller must
  mint at open (`newMaterialId ()` / `newSampleId ()`), which is the point.
- Step 002–007 carried-over gotchas remain valid (baselines come from
  `.checkpoints-json`, not the SoW YAML; the `App` type-name ambiguity guard;
  the appsettings.json write-back into test output copies is expected).

## Changelog

- 2026-07-10 — Step 008 (IMPLEMENT_CONTRACT SVC_XDUO_0001, attempt 1):
  implemented the real WindowLauncher over the host-layer WindowRegistry
  (module-level mutable Map outside any Elmish model) — activate-on-live-key,
  factory-create + register + reference-guarded unregister-on-Closed, Browse
  always Show, Select-state ShowDialog-owned/Show per the step-005 switch —
  moved the id-mint to window open (editor intents carry the upfront
  MaterialId/SampleId; Save routes add/update on EntryFreshness), and rewired
  the workbench Add/Edit/Categories verbs through the launcher (subsuming
  EditorLaunchers.defaults). 3 pure + 7 headless new pins incl. all three
  slice-acceptance observations. Build clean (no MSB3277); suites
  479 / 119 / 124 / 361.

```yaml
gates:
  berreman_unit_tests: 119
  constructor_unit_tests: 479
  ui_smoke_tests: 124
  ui_tests: 361
```
