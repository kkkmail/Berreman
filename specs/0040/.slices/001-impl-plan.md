# Step 001 — impl-plan

## Goal

Sort the faceted-navigation tree alphabetically by DISPLAY LABEL (case-insensitive,
ordinal) at all three levels — facet groups, value branches, and entry leaves —
overriding the engine's structural/representation/corpus order (operator 010/Q1).
No other behaviour changes.

## Approach

Three touch points, one behaviour:

1. **`OpticalConstructor.Domain/Facets.fs` — `buildTree` branch sort.**
   Replace the structural branch sort `List.sortBy (fun b -> b.value)` with a
   case-insensitive ordinal sort by the branch DISPLAY LABEL
   (`List.sortWith` over `String.Compare(a.label, b.label, OrdinalIgnoreCase)`).
   This re-orders the branches every facet node hands back — the second level.
   Update the `buildTree` doc comment (it described the old structural order).

2. **`OpticalConstructor.Ui/MaterialsWindowView.fs` — the window projection.**
   Add a private `sortNodesByLabel` helper (case-insensitive ordinal by
   `TreeNode.label`). Apply it to the `entriesNode` children (entry leaves,
   currently corpus order) and to `facetNodes` (top-level facet groups, currently
   representation order). Branches already arrive sorted from `buildTree`.

3. **`OpticalConstructor.Ui/LibraryWindowView.fs` — same shape.**
   Same private `sortNodesByLabel` helper; sort the `entriesNode` children and the
   `facetNodes`. The numeric film-thickness BUCKET branches keep their range order
   (they are not re-sorted — only the two node lists the slice names).

## Tests

- **`OpticalConstructor.Tests/FacetsTests.fs`** — a branch-label sort test over a
  mixed-case corpus whose case-SENSITIVE ordinal (old value sort) disagrees with the
  case-INSENSITIVE label order (the probe a value sort could never pass).
- **`OpticalConstructor.Ui.Tests/MaterialsWindowTests.fs`** — (a) rewrite the
  `choosing a representation reshapes the tree facet order` test: the tree is now
  alphabetical and INVARIANT to representation, while the OFFERS still follow it;
  (b) a new test asserting entry leaves and every facet's branches read alphabetically.
- **`OpticalConstructor.Ui.Tests/LibraryWindowTests.fs`** — a new test asserting the
  entry leaves and the top-level facet groups read alphabetically (buckets excluded).

## Risks

- The existing Materials `choosing a representation reshapes the tree facet order`
  test asserts the OLD (representation-ordered) tree — it must be updated because
  this slice intentionally overrides that order in the tree. Verified no other test
  navigates the faceted tree by position (all use `List.find`/ids).
- Numeric bucket branches must NOT be alphabetically sorted; only facet groups and
  entry leaves are sorted in the window views.
