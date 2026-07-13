# Step 001 — impl-log

## Progress

- [x] `Facets.fs` — `buildTree` branch sort → case-insensitive ordinal by DISPLAY LABEL; doc comment updated.
- [x] `MaterialsWindowView.fs` — private `sortNodesByLabel`; applied to entry leaves + facet groups.
- [x] `LibraryWindowView.fs` — private `sortNodesByLabel`; applied to entry leaves + facet groups (buckets untouched).
- [x] `FacetsTests.fs` — new branch-label-sort test (mixed-case probe).
- [x] `MaterialsWindowTests.fs` — rewrote the representation test; new alphabetical-projection test.
- [x] `LibraryWindowTests.fs` — new alphabetical-projection test.
- [x] Diagnostic local build + affected-test runs (see Testing state).

## Files modified

`OpticalConstructor.Domain` (1 file):

- `Facets.fs` — `buildTree`'s branch builder: the trailing
  `List.sortBy (fun b -> b.value)` (structural sort of the `AttributeValue` DU —
  discrete by ordinal key string, numeric by magnitude) becomes
  `List.sortWith (fun a b -> String.Compare(a.label, b.label, StringComparison.OrdinalIgnoreCase))`
  — a case-insensitive ordinal sort of the branch DISPLAY LABEL. The `buildTree`
  doc comment that described the old "structural sort of the value" was updated to
  describe the case-insensitive label order (operator 010/Q1). `open System` is
  already at the top of the file.

`OpticalConstructor.Ui` (2 files):

- `MaterialsWindowView.fs` — new private `sortNodesByLabel : TreeNode list -> TreeNode list`
  (case-insensitive ordinal `System.String.Compare` over `TreeNode.label`), placed
  just above `facetedState`. Piped onto the `entriesNode` children (the entry
  leaves, previously `filtered`/corpus order) and onto `facetNodes` (the top-level
  facet groups, previously `m.representation.order`). The per-facet branches already
  arrive sorted from `buildTree`, so they are not re-sorted here. `System` is not
  `open`ed in this file, so the helper fully-qualifies `System.String.Compare` /
  `System.StringComparison.OrdinalIgnoreCase`.
- `LibraryWindowView.fs` — the same private `sortNodesByLabel` helper (short form —
  this file `open`s `System`), piped onto the `entriesNode` children and onto
  `facetNodes`. The NUMERIC film-thickness branches are re-projected as step-010
  buckets and keep their ascending range order — they are NOT re-sorted (the slice
  sorts only the two node lists it names: facet groups and entry leaves).

`OpticalConstructor.Tests` (1 file):

- `FacetsTests.fs` — new fact
  ``buildTree orders branches case-insensitively by display label, overriding the ordinal value order``:
  a one-facet corpus with mixed-case keys `Zinc` / `apple` / `Banana`. The old value
  sort (ordinal, case-sensitive: `'B'`=66 < `'Z'`=90 < `'a'`=97) would yield
  `[Banana; Zinc; apple]`; the required case-insensitive label order is
  `[apple; Banana; Zinc]`. The test asserts the latter — red under the old sort,
  green under the new.

`OpticalConstructor.Ui.Tests` (2 files):

- `MaterialsWindowTests.fs` — (1) the existing
  ``choosing a representation reshapes the tree facet order but never the constraints or the corpus``
  test asserted the OLD representation-ordered tree (category-first for by-category,
  anisotropy-first for by-physics). Since this slice overrides tree order to be
  alphabetical, that behaviour is gone; the test was rewritten as
  ``the tree reads alphabetically regardless of representation, while the picker still reshapes the offers``
  — the tree facet groups are alphabetical and IDENTICAL across the two
  representations, the OFFERS still lead with the representation's head facet
  (Category vs Anisotropy), and constraints/corpus stay untouched (the unknown-code
  inertness assertion is kept). (2) new fact
  ``the projected tree lists entry leaves and facet branches in case-insensitive alphabetical label order``
  — the entry leaves (12, not in alphabetical corpus order) and every discrete
  facet's branches read alphabetically.
- `LibraryWindowTests.fs` — new fact
  ``the projected tree lists entry leaves and facet groups in case-insensitive alphabetical label order``
  — the 17 entry leaves and the top-level facet groups read alphabetically (the
  numeric film-thickness BUCKETS keep range order and are deliberately not asserted).

## Testing state

Gate execution belongs to the arc-runner's deterministic gate engine after this
worker exits (IMPLEMENT Invariant 6 — the worker acts, it runs no checks).
Diagnostic verification only, NOT gate authority:

- `dotnet build Berreman.slnx -c Release -nologo -v:m` — **Build succeeded, 0 errors.**
  The 4 warnings are all pre-existing and outside our code / exempt: `NU1701`
  (Wolfram.NETLink fallback restore, ×2) and `SYSLIB0051` (vendored MathNet
  `Exceptions.cs`, ×2). No warning originates in any file this slice touched.
- `OpticalConstructor.Tests` (constructor-unit-tests) — **675/675 passed**
  (`FacetsTests` alone 59/59, including the new branch-label test; `LibraryFacetsTests`
  green — its uniformly-capitalised branch summaries are order-invariant under the
  case-insensitive label sort).
- `OpticalConstructor.Ui.Tests` (ui-smoke + ui-tests) — **646/646 passed**; the
  Materials + Library window modules alone 94/94, including the rewritten
  representation test and the two new alphabetical-projection tests.
- `git diff --numstat` equals `git diff --numstat --ignore-cr-at-eol` for every
  touched file — no CRLF churn (LF preserved).

## Artifacts

None required (no captured logs / screenshots this round; diagnostic runs summarised above).

## Gotchas

- **The Materials representation test was rewritten, not merely extended.** The old
  ``choosing a representation reshapes the tree facet order`` asserted the TREE
  followed representation order (category-first, then anisotropy-first). This slice
  intentionally overrides tree order to alphabetical, so that assertion is no longer
  true. The representation picker's effect now lives in the OFFERS (the apply
  surface, still in representation order); the rewrite asserts exactly that split.
  Verified no other test (headless or pure) navigates the faceted tree by POSITION —
  every other facet/branch/leaf lookup is by `List.find`/stable id, so it is immune
  to the reorder.
- **Numeric bucket branches are deliberately NOT sorted.** In `LibraryWindowView`
  the film-thickness facet's branches are step-010 buckets in ascending range order;
  a string sort of their labels ("10-20 nm", "500-1000 nm", …) would be wrong. The
  slice names only the facet-group nodes and the entry-leaf children, so only those
  two lists are sorted; buckets keep range order. Discrete branches (which DO sort)
  come pre-sorted from `buildTree`, so the window views never re-sort branches.
- **The branch sort is a NO-OP for uniformly-cased vocabularies.** Every seeded
  discrete facet key is capitalised ("Crystal", "Detector", "Complex ε terms", …),
  so the case-insensitive label order equals the old ordinal value order for the
  seeded corpus — which is why the existing `LibraryFacetsTests` / by-kind branch
  summaries stayed green. The change only bites on mixed-case labels (the new
  `FacetsTests` probe) and on the facet-group / entry-leaf ordering the window
  projections now impose.
- **`buildTree` still returns FACETS in representation order.** The slice re-sorts
  only the BRANCHES inside `buildTree`; the facet-node order is untouched at the
  engine level and re-sorted alphabetically only in the two window projections.
  `LibraryFacetsTests`' `facets |> List.map key` assertions (which check
  representation order at the engine level) therefore stay green.
- **No operator note was in flight** (the project prompt's Operator note section is empty).
