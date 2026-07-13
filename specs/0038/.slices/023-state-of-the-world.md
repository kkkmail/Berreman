# State of the world — Step 023 (Spec 0038 Part H, IMPLEMENT)

## Where we are

Step 023 surfaces entry **lifecycle** in BOTH catalogue windows — the Materials window
(UICOMP_XDUO_0009) and the Library window (UICOMP_XDUO_0010) — on top of the versioned stores landed
by steps 021/022 and the lifecycle foundation of step 020. The versioned `MaterialProxy` /
`SampleProxy` already exposed everything needed (list-by-scope, the three lifecycle verbs,
by-version `resolveVersion`), so this step is pure UI: the two window views gain a show-inactive
toggle with a per-entry badge, the Mark inactive / Mark active / Supersede… verbs (confirm-gated
inline, typed refusals), and a view-panel version list whose older versions open view-only. No
Domain change (the slice `touches` is Ui only). Step 25 wires the real `VersionsInUse` over live
experiment descriptors, at which point a version can actually be "in use" and a mint fires — so the
multi-version path is proven here through stub proxies.

## What's working

- Add a show-inactive/superseded TOGGLE (default hidden) to both windows: it flips the corpus scope
  between `ActiveOnly` and `IncludeInactive`, carries a visible count badge (`Show inactive (N)` /
  `Hide inactive (N)`), and reveals retired entries in the tree with a ` — inactive` leaf badge.
- Add the Mark inactive / Mark active / Supersede… verbs on the selected entry, confirm-gated inline
  through a `lifecycleGate` parallel to the untouched remove gate; a store refusal surfaces as the
  typed inline message. An active selection offers Mark inactive + Supersede…, a retired one offers
  Mark active.
- Suppress lifecycle verbs on `ProtectedBuiltIn` Library presets (removed, not greyed); only
  `UserManaged` samples version and retire. Materials carry no protection, so every material is
  eligible there.
- Keep pickers, Select mode and default facet counts EXCLUDING retired entries (Select always lists
  `ActiveOnly`) while reference resolution IGNORES lifecycle — a bound retired entry still resolves
  through `resolveVersion` (the table keeps drawing it).
- List an entry's versions in the view panel; the latest is edited through the ordinary Edit verb
  ("the library always edits the latest"), an older version opens VIEW-ONLY inline with a view-only
  note and no Save path.
- Add 16 tests (11 pure ui-tests + 5 ui-smoke) across the two suites proving the four acceptance
  points: inactive exclusion from offers with references still resolving, no lifecycle verbs on
  protected entries, the badged show-inactive toggle, and view-only older versions.

## Tests

Gate execution remains the arc-runner's (IMPLEMENT Invariant 6 — the worker acts; the deterministic
gate engine is the sole authority). Locally confirmed to avoid wasting the single supervisor retry,
not self-reported as authoritative:

- `build`: 0 errors; no new warnings from our code (only the pre-existing exempt FS1125 ×4 in the
  untouched `SeriesDataTests.fs`, plus SYSLIB0051 / FS3873 / FS0044 / NU1701).
- `unit-tests` (BerremanTests): 119 passed / 5 pre-existing skips (== baseline, core solver
  untouched).
- `constructor-unit-tests`: 589 passed (== baseline; Domain untouched).
- `ui-smoke`: 158 passed — a strict addition over the 153 baseline (+5: 2 Materials + 3 Library).
- `ui-tests`: 425 passed — a strict addition over the 414 baseline (+11: 6 Materials + 5 Library).

```yaml
gates:
  berreman_unit_tests: 119
  constructor_unit_tests: 589
  ui_smoke_tests: 158
  ui_tests: 425
```

## Architecture

- **Everything stayed in the two window views.** The versioned stores already expose the whole
  surface (`listMaterials/listSamples scope`, `markMaterialInactive`/`markMaterialActive`/
  `supersedeMaterial` + sample analogues, by-version `resolveVersion`). No Domain change was needed
  or in scope; the composition roots (`MaterialsWindow.fs`/`LibraryWindow.fs`) are untouched because
  the context/init/update/view signatures did not change.
- **Effective scope, not a hardcoded `ActiveOnly`.** `projectionInputs` now lists the corpus at
  `effectiveScope m` — `ActiveOnly` in Select mode (a retired entry is never a valid pick target),
  else the `showInactive` toggle. Default `ActiveOnly`, so every step-021/022 projection (offers,
  facet counts, Select) is byte-for-byte unchanged.
- **Two parallel confirm gates.** The existing `removeGate` is untouched; a new `lifecycleGate`
  carries the pending lifecycle action. Arming either disarms the other and clears the message, and
  `disarmed` clears both — one confirm is ever pending. `offeredLifecycleActions` is the ONE pure
  source of truth the verbs row renders from and the tests assert against.
- **Badge through existing primitives.** `FacetedTreeControls` is domain-free and out of scope
  (Controls project), so the badge rides the tree-leaf `label` (`… — inactive`) and the toggle's own
  label carries the count — no control change.
- **Version enumeration by probing `resolveVersion`.** The proxies expose no list-versions field, so
  the view panel probes `resolveVersion` from version 1 upward and stops at the first `None`. In the
  live in-memory store this is a single version per entry until step 25; a stub proxy proves the
  multi-version, view-only-older-version path.

## Architecture decisions

- **Older versions open VIEW-ONLY INLINE, not through a launched editor.** The Material editor has a
  `ViewOnlyMaterial` mode but only for coded engine presets; the Sample editor has no view-only mode
  at all, and adding one would be a large, asymmetric change. Rendering the older version read-only
  inside the window's view panel (with a `viewOnlyNote` and no Save path) keeps the whole feature in
  the two view files and makes the mirror identical in both windows. The Edit verb always targets the
  latest ("the library always edits the latest"). Recorded interpretation.
- **Materials carry no protection, so every material is lifecycle-eligible in the Materials window.**
  `MaterialEntry` has no `EntryProtection` and the material store has no built-in guard; the slice's
  "ProtectedBuiltIn library entries expose NO lifecycle verbs" clause is explicitly about the LIBRARY
  window's presets. Recorded interpretation.
- **Supersede and Mark inactive share `InactiveEntry`** (the steps 021/022 store shape), so the badge
  reads "inactive" for both. They stay DISTINCT `LifecycleAction` cases so the confirm prompt names
  the verb the user pressed and a future step can diverge them without a signature change.

## Deferred

- The real `VersionsInUse` over the live experiment store (step 25): only then does a version become
  "in use", a mint fire through the UI, and an entry actually grow a multi-version history in the
  live store (today the version list shows one row per entry outside tests).
- A distinct badge / representation for superseded vs mark-inactive (they share `InactiveEntry` at
  this in-memory step, as in steps 021/022).
- Threading the app-configured tree-gating threshold / bucket cap into the windows (step 47; the ctor
  overrides already exist).

## Gotchas

- **The task file's system-prompt path was stale.** It points at
  `C:\GitHub\AI-Strategy-Generator\implement_worker.system-md`; the file actually lives under
  `src/ai_strategy_generator/multistep/implement_worker.system-md` (a Development-family delta over
  the shared `arc-runner.system-md`). Both were located by search and read in full (the step-013/015
  stale-path gotcha recurred).
- **`textOf` returns the FIRST TextBlock.** The view panel keeps the entry-name TextBlock first
  (version list and view-only note follow the metadata), so the step-013/015 "panel contains the
  name" assertions stay green.
- **Materials `selectedEntry` resolves via `tryGetMaterial` (lifecycle-ignoring); Library
  `selectedEntry` via the filtered corpus.** So after Mark inactive with the toggle OFF, the
  Materials panel keeps the entry (offering Mark active) while the Library panel drops it. Both
  satisfy acceptance; the intended revive flow in both is: toggle ON → the badged inactive entry
  appears → select → Mark active. Not otherwise tested.
- **Version enumeration probes the proxy per render for the SELECTED entry only** (2 calls for a
  one-version entry: v1 → Some, v2 → None). Negligible; never enumerates the whole corpus.
- **New model fields have deterministic defaults**, so existing `Assert.Equal<Model>` comparisons are
  unaffected; every test change is additive (no baseline regression).

## Changelog

- 2026-07-11 — Step 023 (Part H, IMPLEMENT): surface entry lifecycle in both catalogue windows —
  a badged show-inactive/superseded toggle, the Mark inactive / Mark active / Supersede… verbs
  (confirm-gated inline, typed refusals; protected Library presets get none), and a view-panel
  version list whose older versions open view-only inline while the library edits the latest;
  Select/pickers/default counts keep excluding retired entries and references keep resolving through
  by-version resolve. All in the two window views (no Domain change); +16 tests. Build 0 errors;
  unit 119, constructor 589, ui-smoke 158, ui-tests 425 — all green locally, no regressions.
