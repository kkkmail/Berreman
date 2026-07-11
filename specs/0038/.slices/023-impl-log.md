# Step 023 — impl-log

## Progress

- [x] Read system prompt, project prompt, slice spec, dependency SoWs (013/015/021/022).
- [x] Surveyed both window views, the versioned stores, lifecycle foundation, FacetedTreeControls,
      and both test suites.
- [x] Wrote impl-plan.
- [ ] Implement lifecycle in `MaterialsWindowView.fs`.
- [ ] Mirror into `LibraryWindowView.fs`.
- [ ] Add pure + headless tests to both test suites.
- [ ] Build + run gates locally; write SoW.

## Files modified

- `OpticalConstructor.Ui/MaterialsWindowView.fs` — `LifecycleAction` / `MaterialLifecycleGate`
  types; model fields `showInactive` / `lifecycleGate` / `viewedVersion`; `effectiveScope`;
  lifecycle+version helpers (`activeMaterialIds`, `selectedLifecycle`, `offeredLifecycleActions`,
  `inactiveCount`, `selectedVersionsOf`, `selectedVersions`, `inactiveBadge`); new UiIds; new Msgs
  and update arms; badge in `facetedState`; view rows (toggle, lifecycle verbs, lifecycle confirm,
  version list + view-only note). Ui project builds clean.
- `OpticalConstructor.Ui/LibraryWindowView.fs` — the mirror over samples/presets; `liveEntries`
  now takes a scope; `SampleLifecycleRefused` error case; lifecycle verbs gated on `UserManaged`
  samples only (protected presets get none). Ui project builds clean.
- `OpticalConstructor.Ui.Tests/MaterialsWindowTests.fs` — +8 tests (6 pure ui-tests, 2 ui-smoke):
  lifecycle UiIds; toggle scope+badge+reference-resolves; verbs follow live/retired state + confirm
  gate + vanished-id typed refusal; supersede retires; Select-mode exclusion; version enumeration +
  view-only older; headless toggle-reveals-badged; headless older-version-view-only.
- `OpticalConstructor.Ui.Tests/LibraryWindowTests.fs` — +8 tests (5 pure ui-tests, 3 ui-smoke):
  lifecycle UiIds; protected-no-verbs/sample-has-verbs; toggle scope+badge+reference-resolves;
  confirm gate + vanished-sample typed refusal; version enumeration + view-only older; headless
  protected-vs-sample verbs; headless toggle-reveals-badged; headless older-version-view-only.

## Decisions

- **No Domain change.** The versioned proxies already expose list-by-scope, the three lifecycle
  verbs, and by-version `resolveVersion`. The slice `touches` is Ui only; all work stays in the two
  window views + tests.
- **Inline older-version view.** Older versions render read-only inside the window's view panel
  (not a launched view-only editor). The Sample editor has no view-only mode, and the mirror must
  be identical in both windows; inline keeps the whole feature in the two view files. The Edit verb
  always targets the latest ("the library always edits the latest").
- **Version enumeration by probing `resolveVersion`** from v1 upward (the proxy exposes no
  list-versions field); terminates at the first `None`. Live store = one version per entry until
  step 25; a stub proxy proves the multi-version path.
- **Materials have no protection** (`MaterialEntry` carries no `EntryProtection`, the store has no
  built-in guard), so every material is lifecycle-eligible in the Materials window. Only the
  Library window suppresses lifecycle verbs on `ProtectedBuiltIn` presets.

## Testing state

Gate execution is the arc-runner's (IMPLEMENT Invariant 6 — the worker acts; the deterministic gate
engine is the sole authority). Local runs are diagnostic only, done to avoid wasting the single
supervisor retry:

- `build` (whole `Berreman.slnx`): 0 errors; no new warnings from our code — only the pre-existing
  exempt set (FS1125 ×4 in untouched `SeriesDataTests.fs`, SYSLIB0051 vendored MathNet, FS3873
  Dispersion, FS0044 ChartWindow, NU1701).
- `unit-tests` (BerremanTests): 119 passed / 5 pre-existing skips (== baseline 119).
- `constructor-unit-tests`: 589 passed (== baseline 589).
- `ui-smoke`: 158 passed (baseline 153, +5 new headless: 2 Materials + 3 Library).
- `ui-tests`: 425 passed (baseline 414, +11 new pure: 6 Materials + 5 Library).

No regressions; every change is additive. `commit_ready: true`.

## Artifacts

- Local gate logs captured under `specs/0038/.artifacts/023-diag-{build,unit-tests,
  constructor-unit-tests,ui-smoke,ui-tests}.log`.
