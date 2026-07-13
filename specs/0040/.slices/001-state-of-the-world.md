# Step 001 — state of the world

## Where we are

Step 001 opens spec 0040 by making the faceted-navigation tree read alphabetically by
DISPLAY LABEL (case-insensitive, ordinal) at all three levels — facet groups, value
branches, and entry leaves — overriding the engine's structural order, the window
representation order, and the corpus order (operator 010/Q1). The generic engine's
`Facets.buildTree` now sorts each facet's branches by label instead of by the
structural `AttributeValue`; both host windows (`MaterialsWindowView`,
`LibraryWindowView`) sort their top-level facet-group nodes and their entry-leaf
children by label in the projection. Nothing else changes: filtering, counts,
breadcrumbs, offers (still in representation order), gating, and the numeric
film-thickness buckets (which keep their ascending range order) are all untouched.

## What's working

- Sort `Facets.buildTree` branches case-insensitively by display label instead of by the structural value.
- Sort the Materials and Library window projections' facet-group nodes and entry-leaf children alphabetically by label.
- Keep numeric film-thickness bucket branches in ascending range order (only facet groups and entry leaves are re-sorted).
- Add a domain branch-label-sort test with a mixed-case probe that a value sort could never pass.
- Rewrite the Materials representation test (tree now alphabetical and representation-invariant; offers still follow representation) and add alphabetical-projection tests for both windows.

## Tests

- Gate execution belongs to the arc-runner's deterministic gate engine after this
  worker exits (IMPLEMENT Invariant 6 — the worker acts, it runs no checks). This
  session ran no gate commands as gates; the roster for this step is `build`,
  `unit-tests`, `constructor-unit-tests`, `ui-smoke`, `ui-tests`.
- Diagnostic verification (NOT gate authority): `dotnet build Berreman.slnx -c Release`
  succeeded with 0 errors and no new warning from any touched file (the 4 warnings are
  pre-existing `NU1701` / `SYSLIB0051` third-party/vendored, exempt per CLAUDE.md);
  `OpticalConstructor.Tests` passed 675/675 (incl. the new `FacetsTests` branch-label
  probe); `OpticalConstructor.Ui.Tests` passed 646/646 across both categories (incl.
  the rewritten representation test and the two new alphabetical-projection tests).
- Net test delta this slice: +3 new tests, 1 test rewritten (none removed), so no
  `count_at_least` gate can regress from the baselines below.

## Architecture

- **One ordering rule, three surfaces.** The engine (`buildTree`) owns the BRANCH
  order (now case-insensitive by label); each window projection owns the FACET-GROUP
  and ENTRY-LEAF order (also case-insensitive by label, via a private
  `sortNodesByLabel` helper per view). The engine still returns facet NODES in
  representation order — the windows override that to alphabetical only in the
  projection, so search/representation order and display order stay decoupled
  (spec 0038 §D.0's "search order ≠ representation order").
- **Representation choice moved from the tree to the offers.** Sorting the tree
  alphabetically makes it invariant to the chosen representation; the picker's
  observable effect is now the OFFERS panel (still built in representation order).
  The rewritten Materials representation test pins this new split.
- **Buckets are exempt.** Numeric branches are re-projected as step-010 buckets in
  ascending range order; the window views sort only the facet-group and entry-leaf
  node lists, never the branch lists, so buckets are untouched and discrete branches
  inherit `buildTree`'s alphabetical order.

## Deferred

- Nothing from this slice's scope. Later slices of spec 0040 (002–013) are separate
  rounds and out of scope here.

## Gotchas

- **The Materials `choosing a representation reshapes the tree facet order` test was
  rewritten, not extended** — it asserted the OLD representation-ordered tree, which
  this slice intentionally overrides. It now asserts the tree is alphabetical and
  representation-invariant while the offers still follow representation. No other test
  navigates the faceted tree by position (all use `List.find` / stable ids), so the
  reorder breaks nothing else.
- **The branch sort is a no-op for the seeded corpus** (every discrete facet key is
  capitalised, so case-insensitive label order equals the old ordinal value order);
  the change only bites on mixed-case labels and on the facet-group / entry-leaf
  ordering the projections now impose. This is why existing `LibraryFacetsTests`
  branch-summary and by-kind assertions stayed green.
- **Do not sort numeric bucket branches.** A string sort of "10-20 nm" / "500-1000 nm"
  labels would be wrong; buckets must stay in range order. Only the two node lists the
  slice names (facet groups, entry leaves) are sorted.

## Changelog

- 2026-07-12 — Step 001 (IMPLEMENT): sorted the faceted tree alphabetically by display
  label (case-insensitive, ordinal) at all three levels — `Facets.buildTree` branch
  sort changed from the structural value to the label; `MaterialsWindowView` and
  `LibraryWindowView` projections sort facet-group nodes and entry-leaf children via a
  private `sortNodesByLabel` helper (numeric buckets keep range order). Added a domain
  branch-label-sort probe, rewrote the Materials representation test, and added
  alphabetical-projection tests for both windows.

```yaml
gates:
  berreman_unit_tests: 0
  constructor_unit_tests: 0
  ui_smoke_tests: 0
  ui_tests: 0
```
