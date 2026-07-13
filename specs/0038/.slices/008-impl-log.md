# Step 008 — impl log (attempt 1)

## Progress

- [x] Read the task file, worker system prompt (`implement_contract_worker.system-md`
      + shared base `arc-runner.system-md`), project prompt (Operator note: empty),
      slice spec, the 008 gate roster (`008.gates`), the 006/007 SoWs, spec 0038
      `.spec-md` Part C, steps 009/016 (the growth path), and the surrounding code
      (WindowLauncher.fs, AppContext.fs, the editor view/window pairs, the
      workbench verbs, the Ui.Tests conventions and every ripple site).
- [x] Wrote the impl-plan (`008-impl-plan.md`).
- [x] `Ui/WindowLauncher.fs` — the REAL launcher: `WindowOpenMode`
      (`BrowseOpen | SelectOpen of requestingWindow`), the private host-layer
      `WindowRegistry` (ONE module-level mutable `Map<WindowKey, Window>`, outside
      any Elmish model), and `WindowLauncher.create factory modality openMode` —
      activate = `Window.Activate` on the live registered window; create =
      injected factory → register → reference-guarded unregister-on-`Closed` →
      show (Browse always `Show`; Select per the step-005 switch: `ShowDialog`
      owned by the requester / `Show`); platform throws on the open path caught
      at the boundary and mapped onto `WindowFactoryFailed` (doc widened — no new
      DU case, so no FS0025 ripple). Header/docs moved to `implemented`.
- [x] `Ui.fsproj` — `WindowLauncher.fs` moved UP the compile order (before
      `SampleEditorView.fs`): the editor views route Save on `EntryFreshness` and
      the workbench defaults route through the launcher. The step-007 "after
      AppContext" placement is obsolete — the real `create` takes its
      factory/modality/mode directly and never needs the context.
- [x] `Ui/MaterialEditorView.fs` — new `MaterialEditorIntent`
      (`NewMaterial of mintedId | EditMaterial of entry`, replacing the
      `MaterialEntry option` open shape); `EditorTarget` reshaped to
      `{ materialId; freshness }` (id ALWAYS present); `init` takes the intent;
      `SaveClicked` routes `addMaterial`/`updateMaterial` on the freshness — the
      `newMaterialId ()` mint LEFT the save path (pre-move line 416).
- [x] `Ui/MaterialEditorWindow.fs` — ctor takes the intent; title matches it.
- [x] `Ui/SampleEditorView.fs` — both NEW `SampleEditorIntent` cases gain a
      `mintedId` payload; `EditorTarget` reshaped to `{ sampleId; freshness }`;
      `init`/`SaveClicked` route on the freshness (the `newSampleId ()` mint left
      the save path). `Ui/SampleEditorWindow.fs` — title patterns take payloads.
- [x] `Ui/TableAndElementRotationView.fs` — `EditorLaunchers.openMaterialEditor`
      now takes the intent; `EditorLaunchers.defaults` REWIRED through
      `WindowLauncher.create` over the shared host registry (`openBrowse` helper:
      per-dispatch launcher record, key derived from the intent, `BrowseOpen`);
      the verbs mint at the window-open dispatch — `MatAdd` →
      `NewMaterial (newMaterialId ())`, `MatEdit` → `EditMaterial entry`,
      `SmpAdd` → `NewBlankSample (newSampleId ())`, `SmpMakeMultilayer` →
      `NewSeededMultilayer (newSampleId ())`.
- [x] `App/Program.fs` — comment accuracy (the workbench verbs now open through
      the SVC_XDUO_0001 launcher; a second Edit of the same entity activates).
- [x] `Ui.Tests/WindowLauncherTests.fs` — the step-007 mock suite kept verbatim;
      added the REAL-launcher suite: 3 pure pins (baked modality on `create`,
      typed forget-of-unregistered, factory failure/throw mapped without
      registering) + 7 headless proofs (second open ACTIVATES the shown window
      with one factory call; window close unregisters so the next open creates
      afresh; a STALE window's close never drops its successor; Select-state
      modality — `ShowDialog` owned by the requester under modal, unowned `Show`
      under modeless; and the slice acceptance end-to-end through the rewired
      `EditorLaunchers.defaults`: Edit of the same material twice meets ONE
      window, Add twice creates two windows whose distinct upfront Guids persist
      through `addMaterial`, a sample Add persists through `addSample` under its
      minted id).
- [x] Ui.Tests ripples — `MainWorkbenchTests` (recording launchers record the
      minted ids; new pure pin: two Adds mint two DISTINCT upfront ids for
      materials and samples; ctor/init sites), `MaterialEditorWindowTests`
      (intents + `{ id; freshness }` target pins + the add-under-minted-id pin),
      `SampleEditorWindowTests` (same shape), `EmbeddedChartTests` (ctor site).
- [x] `specs/0038/.contracts-json` — SVC_XDUO_0001 lifecycle → `implemented`,
      implementStep → 8 (the IMPLEMENT_CONTRACT registry obligation).
- [x] Diagnostic build + all four suite runs green (see Testing state); LF check
      clean.
- [x] State-of-the-world written (`008-state-of-the-world.md`).

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Ui/WindowLauncher.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/OpticalConstructor.Ui.fsproj`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/MaterialEditorView.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/MaterialEditorWindow.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/SampleEditorView.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/SampleEditorWindow.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/TableAndElementRotationView.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.App/Program.fs` (comments)
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/WindowLauncherTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/MainWorkbenchTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/MaterialEditorWindowTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/SampleEditorWindowTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/EmbeddedChartTests.fs`
- `specs/0038/.contracts-json`

## Testing state

Gates are executed by the arc-runner's deterministic gate engine after this worker
exits (IMPLEMENT_CONTRACT Invariant 6 — the worker acts, it runs no checks).
Diagnostic verification only (not gate authority), logs in `.artifacts/`:

- `dotnet build Berreman.slnx -c Release -nologo -v:m --no-incremental` — **Build
  succeeded, 0 errors, no MSB3277**; the touched projects (Ui / App / Ui.Tests)
  compiled with ZERO warnings; the 10 warning lines are exactly the
  step-001-catalogued pre-existing set in untouched files (FS1125
  SeriesDataTests ×4, FS3873 Dispersion, FS0044 ChartWindow, SYSLIB0051 vendored
  MathNet ×2, NU1701 Wolfram.NETLink ×2). (`008-diag-build.log`)
- ui-smoke (`--filter Category=ui-smoke`): **124/124** (checkpoint 117, +7).
  (`008-diag-ui-smoke.log`)
- ui-tests (`--filter Category!=ui-smoke`): **361/361** (checkpoint 357, +4).
  (`008-diag-ui-tests.log`)
- constructor-unit-tests: **479/479** (== checkpoint; untouched project).
  (`008-diag-constructor-tests.log`)
- unit-tests (BerremanTests): **119 passed / 5 pre-existing skips** (== checkpoint).
  (`008-diag-unit-tests.log`)
- Line endings: `git diff --numstat` equals `--ignore-cr-at-eol --numstat` — no
  CRLF churn from this round (the CRLF warnings on `.contracts-json` /
  `.manifest.state.json` are the supervisor's own working-copy state, pre-dating
  this round; the 2-line `.contracts-json` edit preserved the file's style).

One compile fix was needed after the first build: a bare `NewBlankSample` at
SampleEditorWindowTests.fs:302 (the pure Save-routing test) had been missed by
the payload sweep — FS0001, fixed by minting the id.

## Artifacts

- `specs/0038/.artifacts/008-diag-build.log` — full non-incremental Release build.
- `specs/0038/.artifacts/008-diag-ui-smoke.log` — ui-smoke run (124 passed).
- `specs/0038/.artifacts/008-diag-ui-tests.log` — ui-tests run (361 passed).
- `specs/0038/.artifacts/008-diag-constructor-tests.log` — constructor suite (479 passed).
- `specs/0038/.artifacts/008-diag-unit-tests.log` — BerremanTests (119 passed, 5 skips).

## Gotchas

- No operator note in flight (the project prompt's Operator note section is empty).
- **The task file's system-prompt path does not exist**
  (`C:\GitHub\AI-Strategy-Generator\implement_contract_worker.system-md`); the real
  file is `…\src\ai_strategy_generator\multistep\implement_contract_worker.system-md`
  — located by glob and read in full (the step-007 gotcha recurred).
- **The declared `openOrActivate` carries only the key**, so per-open context
  (Browse vs Select, the requesting window) is baked into each `create`d record
  via the new `WindowOpenMode` — and single-instance semantics across all of them
  comes from the ONE module-level registry. That is WHY the slice mandates a
  module-level `Map` rather than a per-instance ref: launcher records are cheap
  per-site views; the registry is the app-global truth.
- **`WindowLauncher.fs` moved UP the compile order** (before the editor views),
  contradicting step 007's "after AppContext" note — that placement anticipated a
  `create` taking the context, but the real `create` takes factory/modality/mode
  directly. Recorded as the interpretation most consistent with the slice's
  "rewire the workbench verbs through the launcher".
- **`EditorLaunchers.defaults` stays the workbench seam** ("subsuming …defaults"
  read as: the DEFAULTS now route through the launcher; the `EditorLaunchers`
  record remains the Elmish model's typed verb seam so recording-launcher tests
  keep their substitution point). Its Browse-mode launcher bakes
  `SelectWindowModality.defaultValue` — never consulted on the Browse path; the
  step-016 Select windows compose `SelectOpen` launchers over
  `AppContext.settings` instead.
- **The typed open error is deliberately dropped at the unit-returning verb seam**
  (`openBrowse … |> ignore`): a failed open leaves no window — the same
  user-visible outcome the verb had before, minus the crash (platform throws are
  now caught at the launcher boundary).
- **The `Closed` unregister hook is reference-guarded**: it removes the key only
  while the registry still points at the closing window, so a
  forget-then-recreate followed by the stale window's close cannot drop the
  successor (pinned headless).
- **The registry is app-global test state**: every real-registry test closes the
  windows it opens (close → `Closed` → unregister). A test that fails mid-body
  can leak a key; deterministic seed ids (glass152 etc.) are registry keys in
  Edit flows, so a leaked seed-key would couple later tests — acceptable (the
  suite is already failing then), noted for future debugging.
- **`.contracts-json` updated by this worker** (lifecycle `implemented`,
  implementStep 8) per the IMPLEMENT_CONTRACT prompt's explicit registry
  obligation — step 007 found the declare-side entry pre-populated and left it
  alone; the implement-side transition is this worker's to record. If the
  supervisor also writes it, the values match (idempotent).
- Step 002–007 carried-over gotchas remain valid (baselines come from
  `.checkpoints-json`, not the SoW YAML; the `App` type-name ambiguity guard; the
  appsettings.json write-back into test output copies is expected).
