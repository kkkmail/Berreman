# Step 012 — state of the world

## Where we are

Step 012 opens spec 0038 Part E: the domain-free faceted-tree rendering
control `OpticalConstructor.Controls/FacetedTreeControls.fs`
(UICOMP_XDUO_0008) now exists — the ONE surface both the Materials window
(step 013+) and the Library window (Part F) will instantiate over the Part D
engine (steps 009–011: `Facets.fs`, `FacetBuckets.fs`, `LibraryFacets.fs`).
It is a pure State+Handlers component in the `MaterialsControls` shape: the
host runs the engine and projects breadcrumbs (labels + after-counts),
count-previewed offered values, named representations, the live result count,
the built tree, and a materialization mode in; the control renders exactly
that, dispatches intent through a stub-replaceable Handlers record, and never
touches a proxy or the facet engine. Next: step 013 instantiates it as the
single-instance Materials window.

## What's working

- Add Controls/FacetedTreeControls.fs (UICOMP_XDUO_0008): domain-free State
  (built tree, breadcrumb chips with after-counts, count-previewed offer
  groups incl. numeric manual-range boxes, named representations + active,
  filter draft, live result count, TreeMaterialization mode) + Handlers
  (applyConstraint, removeConstraint, commitTextFilter, chooseRepresentation,
  requestBuild, selectNode, applyManualRange).
- Render everything in ONE ScrollViewer: removable breadcrumb chips reading
  'label (afterCount) ×', live result count, offer groups with '(preview)'
  labels, representation picker, and the tree as depth-indented 'label
  (count)' rows — or the explicit Show/Search button INSTEAD with zero rows
  generated when State says materialization is gated.
- Filter and manual-range boxes commit on Enter/LostFocus ONLY (no
  text-change subscription anywhere); Enter is marked handled to drop
  FuncUI's duplicate Tunnel|Bubble dispatch.
- Every generated element carries an intent-named AutomationId ([<Literal>]
  UiIds + derived per-code ids) and a View.withKey key; nothing sets
  StyledElement.Name.
- 6 new headless Ui.Tests (2 pure contract + 4 mounted proofs): every
  declared automation id located over a known State, chip click →
  removeConstraint, offered-value click → applyConstraint, gated mode shows
  the button and ZERO tree rows, real typed input dispatches nothing per
  keystroke with Enter/blur committing.
- Suites 559 / 119 / 128 (+4) / 363 (+2); build clean, no MSB3277, zero
  warnings from touched projects.

## Tests

- Gates are executed by the arc-runner's deterministic gate engine after this
  worker exits (ADD_COMPONENT Invariant 6 — the worker acts, it runs no
  checks). The roster for this step is `build`, `unit-tests`,
  `constructor-unit-tests`, `ui-smoke`, `ui-tests`.
- Diagnostic verification (not gate authority): `dotnet build Berreman.slnx
  -c Release` succeeded with 0 errors, **no MSB3277**, and zero warnings from
  the touched projects (Controls / Ui.Tests) — the only warnings are the
  step-001-catalogued pre-existing set in untouched files (FS1125
  SeriesDataTests ×4, FS3873 Dispersion, FS0044 ChartWindow, SYSLIB0051
  vendored MathNet ×2, NU1701 Wolfram.NETLink ×2). Suites: ui-smoke
  **128/128** (checkpoint 124, +4: mount-over-known-State locating every
  declared id with chip/offer/representation/node click dispatch, gated mode
  with requestBuild and zero rows, filter-box and manual-range-box
  Enter/LostFocus-only commits over real typed input), ui-tests **363/363**
  (checkpoint 361, +2: empty-state and UiIds contract facts), BerremanTests
  **119 passed / 5 pre-existing skips** (== checkpoint),
  OpticalConstructor.Tests **559/559** (== checkpoint). Logs in
  `specs/0038/.artifacts/012-diag-*.log`.
- Nothing deferred.

## Architecture

- **The control is a projection surface, not an engine client**: State holds
  host-flattened string codes/labels and int counts (the established
  Controls-project seam — MaterialsControls/CategoryControls precedent);
  modes are named two-case DUs (`TreeMaterialization`, `NodeExpansion`,
  `ManualRange`), never bools. The host maps codes back to its domain values
  when a handler dispatches; the control interprets nothing — so
  `TreeAutoBuildThreshold` stays in Domain settings and gating arrives as
  data.
- **Handler tokens carry the identity the host needs back**:
  `applyConstraint` takes the offer GROUP code plus the value code (the same
  discrete key can appear under two facets), mirrored in the derived
  offered-value automation id; `applyManualRange` hands the group code plus
  the box's RAW text — the host parses and applies an ordinary constraint
  chip (spec §D.0), keeping the control unit-free and culture-free.
- **The tree is a generic recursive node list** with `countOpt`: branch rows
  read 'label (count)' expanded OR collapsed (unexpanded branches carry their
  counts); heading/leaf rows the host projects without a count read the bare
  label. `selectNode` is the only node event — expansion is host-projected
  data, so the host decides what a click means.
- **Gated materialization generates nothing**: the tree container and the
  Show/Search button are keyed ALTERNATIVES (different View.withKey keys), so
  the swap recreates cleanly and gated mode provably renders zero node rows
  whatever `tree` holds.
- **Commit-on-Enter marks the event handled** (`commitOnEnter`): FuncUI
  subscribes key events on both the Tunnel and Bubble passes, so an unhandled
  Enter commit dispatches twice — the same discipline `clickBox` already
  applies to pointer events, now extended to the text boxes.

## Deferred

- The Materials window instantiating this control over the step-011 material
  facets (single-instance via SVC_XDUO_0001, view panel + verbs, ribbon
  Materials… button, bay removal) — step 013+ (Part E).
- The Library window over the whole entry corpus and `EntryProtection` —
  Part F.
- Host-side selection semantics (highlighting a selected node) — the State
  enumeration pinned by the slice carries no selected-node field; hosts
  re-project selection when they need it.
- The pre-existing warnings in untouched files remain for spec 0038 Part N's
  sweep (carried from steps 002–011).

## Gotchas

- **The task file's system-prompt path was stale again** — the ADD_COMPONENT
  worker prompt lives under
  `src/ai_strategy_generator/multistep/add_component_worker.system-md` in the
  tool repo; located and read in full together with the shared base
  `arc-runner.system-md` (the step-007..011 gotcha recurred).
- **FuncUI `onKeyDown` fires on BOTH the Tunnel and Bubble passes**: an Enter
  commit without `e.Handled <- true` dispatches TWICE. The older
  `RotationControls` / `ExperimentControls` Enter-commit sites never noticed
  (their commits set values idempotently); any dispatch-observing host or
  test sees the double. Use the control's `commitOnEnter` shape for future
  commit-on-Enter boxes.
- **The offered-value id and handler carry the group code**
  (`FacetOfferedValue_<group>_<value>`, `applyConstraint group value`) — a
  bare value code is NOT unique across facets (e.g. a material name under
  both "substrate" and "film materials"). Recorded interpretation, pinned in
  tests.
- **Tree node codes must be unique across the whole tree** — the flattened
  rows are siblings in one keyed stack (path-shaped codes work; documented on
  `TreeNode.code`).
- **The manual range box is uncontrolled**: no State field holds its draft
  (the slice's State enumeration has only the text-FILTER draft), so the box
  starts empty and hands raw text at commit; an empty/unparseable commit is
  the host's no-op.
- **No registry edit was needed**: `.contract-ids/XDUO-json` and
  `specs/0038/.contracts-json` already carry UICOMP_XDUO_0008 (declared,
  step 12) — the supervisor maintains both (the step-007 precedent).
- Step 002–011 carried-over gotchas remain valid (baselines come from
  `.checkpoints-json`, not the SoW YAML; the window registry is app-global
  test state in Ui.Tests; the appsettings.json write-back into test output
  copies is expected).

## Changelog

- 2026-07-11 — Step 012 (ADD_COMPONENT, attempt 1): added the domain-free
  faceted-tree rendering control `Controls/FacetedTreeControls.fs`
  (UICOMP_XDUO_0008 — State with built tree / breadcrumb after-counts /
  count-previewed offers / named representations / filter draft / live count /
  materialization mode; Handlers applyConstraint, removeConstraint,
  commitTextFilter, chooseRepresentation, requestBuild, selectNode,
  applyManualRange; keyed AutomationId-only rows; Enter/LostFocus-only text
  commits; Show/Search gating rendering zero rows) with 6 headless Ui.Tests
  proofs. Build clean (no MSB3277); suites 559 / 119 / 128 / 363.

```yaml
gates:
  berreman_unit_tests: 119
  constructor_unit_tests: 559
  ui_smoke_tests: 128
  ui_tests: 363
```
