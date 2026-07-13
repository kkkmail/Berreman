# Step 002 — state of the world

## Where we are

Step 002 makes the faceted-navigation tree **collapsible** with **persisted, collapsed-by-default**
expansion (operator). The domain-free `FacetedTreeControls` now renders a disclosure chevron on
every PARENT node (a leaf shows none); the chevron press dispatches a new `toggleNode` handler while
the row label keeps dispatching `selectNode`. Each host window (`MaterialsWindowView`,
`LibraryWindowView`) holds the expanded-code set in its Model as an elevated `ExpandedNodes` type,
adds a `ToggleNode` message and update arm that flips one code, and projects each `TreeNode.expansion`
as `ExpandedNode` only when its code is in the set — so every top-level node (the entries group and
every facet group) opens `CollapsedNode`, and a chevron click reveals its children in the same render
pass and survives later re-renders. This builds on step 001's alphabetical tree (the leaves/branches
stay label-sorted; only their visibility is now gated).

## What's working

- Add `toggleNode` to the `FacetedTreeControls.Handlers` record and render a per-parent disclosure
  chevron in `nodeRows` (a leaf shows none), keyed and carrying a stable `treeNodeChevron` AutomationId.
- Hold expansion in each window Model as an elevated `ExpandedNodes` DU (never a naked `Set<string>`),
  with `ToggleNode` + an update arm that flips one code and PERSISTS the choice.
- Project every tree node's `expansion` from the set, so all top-level nodes open collapsed and a
  chevron toggle expands/collapses them — headless-verified in both windows.
- Add a chevron structure/dispatch proof to the control test and collapse-toggle proofs (pure +
  headless) to both window suites.
- Update the leaf-driving headless proofs across seven test files to expand the tree first.

## Tests

- Gate execution belongs to the arc-runner's deterministic gate engine after this worker exits
  (IMPLEMENT Invariant 6 — the worker acts, it runs no checks). This session ran no gate commands as
  gates; the roster for this step is `build`, `unit-tests`, `constructor-unit-tests`, `ui-smoke`,
  `ui-tests`.
- Diagnostic verification (NOT gate authority): `dotnet build Berreman.slnx -c Release` — 0 errors,
  no new warning from any touched file (the 4 warnings are pre-existing exempt NU1701/SYSLIB0051);
  `OpticalConstructor.Ui.Tests` — 651/651 across both categories; `OpticalConstructor.Tests` —
  675/675 (Domain untouched).
- Net Ui-test delta: +5 new tests (1 control chevron proof, and per window a pure collapse-toggle
  test plus a headless collapse acceptance), none removed — so no `count_at_least` gate can regress.

## Architecture

- **The control owns rendering; the host owns the state.** The chevron and its `toggleNode` seam
  live in the domain-free control, but the expanded-code SET lives in each host Model (the same
  functional-proxy split as `selectNode`/`applyConstraint`). The control never remembers expansion —
  it renders exactly the `expansion` flag the host projects, so behaviour stays testable without a
  window.
- **Elevated `ExpandedNodes`, never a naked `Set<string>`.** Per CLAUDE.md's elevate-every-primitive
  rule, the expanded-code set is wrapped in a single-case DU with `.value` / `.isExpanded` / `.toggle`
  / `.empty` members. Declared per window (the slice mandates it in BOTH), so a later shared home is a
  non-breaking move.
- **Expansion is display-only.** `ToggleNode` disarms no gate and moves no selection; the projection
  always builds the full children list and only the `expansion` FLAG changes, so a collapsed node's
  children remain projected (pure tests read them) and merely stop RENDERING.

## Deferred

- Nothing from this slice's scope. Later slices of spec 0040 are separate rounds and out of scope here.

## Gotchas

- **Collapsed-by-default hides the browsed entry leaves**, which broke ~25 leaf/branch-driving
  headless proofs across SEVEN test files — and, because a failed leaf click left a single-instance
  window open/registered, cascaded into the strip/launcher/wire single-instance tests. Fixed by
  expanding the tree before any leaf interaction: auto-expanded in the two window suites' shared mount
  helpers (expansion persists, so once suffices), and by explicit `treeNodeChevron "entries"` clicks
  in the launcher-opened windows of `EmbeddedChartTests`, `SampleEditorWindowTests`,
  `WireUiCompositionTests`, `WindowLauncherTests`, and `AppContextTests`.
- **A facet-group chevron can scroll out of the viewport** once `entries` is expanded (17 leaves push
  it below the fold); `clickOn` finds it but the pointer lands off-screen and the toggle misses. The
  by-kind branch proof collapses `entries` first (facets rise to the top) before expanding the kind
  facet. Entry-leaf clicks avoid this because the `entries` chevron is the top row and the leaf tests
  narrow the corpus with a filter first.
- **`textOf(treeNode code)` still reads the label**: the chevron is a SIBLING of the label box in the
  row `StackPanel`, never its descendant. The chevron id uses a distinct `FacetTreeChevron_` prefix so
  the `FacetTreeNode_`-counting `treeRowCount` helpers are unaffected.
- **Adding `toggleNode` to `Handlers`** touches all three `Handlers` constructions (both windows'
  `facetedHandlers` and the control test's `recorder`).

## Changelog

- 2026-07-12 — Step 002 (IMPLEMENT): made the faceted tree collapsible with persisted,
  collapsed-by-default expansion — a per-parent disclosure chevron + `toggleNode` in
  `FacetedTreeControls`, a `treeNodeChevron` UiId, an elevated `ExpandedNodes` set + `ToggleNode`
  Msg/update in both windows projecting each node's `expansion`, and headless collapse/chevron proofs;
  updated ~25 leaf-driving headless tests across seven files (auto-expand in mount helpers + explicit
  chevron clicks) to reach the now-collapsed leaves.

```yaml
gates:
  berreman_unit_tests: 0
  constructor_unit_tests: 0
  ui_smoke_tests: 0
  ui_tests: 0
```
