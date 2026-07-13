# 0040 — Faceted-tree fix: branch drill-down to entry leaves + facet-group totals

Response to `.manual/001-task.txt`. The operator reported that in the Materials and
Library catalogue windows the faceted tree is "broken": only the top-level
**Materials** / **Library** node has selectable leaves, while the facet groups
(e.g. **Anisotropy**) **show no total count**, and their branches (e.g.
**Biaxial (3)**) **do not expand** to reveal the entries they contain. Both MUST
work. This log records the gap source, the fix, and the UI-less tests that would
have caught it.

---

## 1. Gap source

**Tag: `gap-pre-spec` (root) → `gap-spec` (inherited). NOT `gap-impl`.**

The implementation faithfully built the tree the spec described — the gap is that
neither the preliminary nor the full spec ever made the two reported behaviours a
requirement.

- **What shipped (grounded).** The host tree projection `facetedState`
  (`MaterialsWindowView.fs`, `LibraryWindowView.fs`) built the tree as two
  *parallel two-level* structures: a flat `entries` group ("Materials" / "Library",
  `countOpt = Some resultCount`) whose children are the corpus leaves, and separate
  facet-group nodes whose children are branch nodes. Every facet-group node was
  authored `countOpt = None` (a bare heading) and every branch node `children = []`
  (a childless count-badge). So a facet group showed no total, and a branch had
  nothing to expand to.

- **Why that matched the spec.** Spec 0040 §A.1 asks only to *sort* "at all three
  levels: the top-level facet groups, every branch, and every entry leaf". Read as
  a sorting contract over three *node kinds* — not a nested facet → branch → leaf
  path — the childless-branch + flat-entries shape satisfies it. The existing
  acceptance test (`MaterialsWindowTests.fs`, *"…lists entry leaves and facet
  branches in… alphabetical label order"*) encodes exactly that reading: "Level 1 —
  the entry leaves under the entries group" and "Level 2/3 — every facet group's
  branches." The preliminary (`specs/0038/.manual/011-tree-chart-geometry-spec.md`
  §A.1/§A.2.5) framed the tree the same way — entry leaves as a flat list, branches
  as ordinal count-badges — and grounded its edits against the pre-0040 code that
  already had this shape.

- **What was never specified.** Neither document states that **a facet branch
  drills down to its member entries**, nor that **a facet group carries a total
  count**. Those are the operator's (entirely reasonable, standard) expectation of
  a faceted browser — facet → value → items, with the group showing its total —
  that the specification under-specified. Because no acceptance test pinned either
  behaviour, the gap passed every gate. The missing test is the one
  implementation-side contributing factor, and §4 adds it.

The fix therefore *realises the intended faceted drill-down* rather than correcting
a mis-built requirement.

---

## 2. The fix (implementation)

Two host projections only; the domain-free `FacetedTreeControls` already renders
whatever tree it is handed (a node with children shows a chevron and expands; a
branch row already reads `label (count)`), so **no control change was needed**.

**`OpticalConstructor.Ui/MaterialsWindowView.fs`** and
**`OpticalConstructor.Ui/LibraryWindowView.fs`** (`facetedState`):

1. **Branches expand to their member entries.** Each facet branch now carries, as
   children, the filtered entries that fall under it — projected with the same
   leaf shape as the corpus group (label + inactive badge, terminal, alphabetical),
   under a **path-shaped, tree-unique code**
   `branch:<facet-key>:<value>:entry:<id>` so the same material can leaf both the
   corpus group and every branch it belongs to without colliding the tree's keys.
   Members are computed via the **same engine path as the branch count**
   (`Facets.filter` with the branch's own `DiscreteSelection` / `NumericRangeSelection`
   on top of everything already applied), so a branch's leaves and its `(count)`
   badge always agree.

2. **Facet groups show a total.** Each facet-group node's `countOpt` is now
   `Some <distinct member entries under the group>` (an entry a multi-valued facet
   lists under several branches counts once). For a single-valued facet — after
   Part B every material carries exactly one class — this equals the population and
   the branch counts visibly sum to it.

3. **Selection through a branch works.** `entryIdOfNodeCode` now resolves both the
   top-level `entry:<id>` and a branch-nested `…:entry:<id>` (splitting on the last
   `:entry:` marker), so clicking a leaf under a branch selects its material/entry.
   A bare `facet:` / `branch:` / `entries` code still resolves to `None` and stays
   inert (unchanged contract).

4. **Numeric buckets (Library film-thickness).** A numeric bucket's badge is the
   **distinct member count**, not `bucket.count` — `FacetBuckets.bucketsFor` counts
   value-*occurrences*, so an entry with two in-range layer thicknesses would
   otherwise over-count the leaves it actually expands to.

A shared local `entryLeafNode` helper projects the leaf shape once and is reused by
the corpus group and every branch (removing the prior duplication).

No other files changed; no domain / engine / control change; scope held to the two
window projections and their tests.

---

## 3. What the user now sees

- **Anisotropy (12)**, **Category (12)**, … — every facet group shows its total.
- **Biaxial (3)** expands to the 3 materials classified Biaxial, each selectable;
  Isotropic / Uniaxial likewise, and `3 + … = 12` (the group total).
- The flat **Materials** / **Library** node is unchanged (still lists the whole
  corpus) — the fix is additive.

**Known cosmetic limitation (documented, not fixed — out of scope):** selecting a
material *through* a branch leaf highlights that material's row in the flat corpus
group (the selection is tracked by id, and the corpus leaf carries the canonical
`entry:<id>` code), not the branch-nested copy that was clicked. Selection itself
works from either place; only the highlight lands on the flat copy. Highlighting
every copy would require the control to match selection by id-suffix (it matches by
exact code today) — a `FacetedTreeControls` change beyond this task's scope.

---

## 4. UI-less tests (start here — they would have caught the gap)

Per the task, the regression net is pure projection tests over `facetedState` (no
window, no headless render), added to both windows' pure-projection sections:

`OpticalConstructor.Ui.Tests/MaterialsWindowTests.fs` and
`…/LibraryWindowTests.fs`:

- **`every facet group shows its total count and each branch expands to its member
  entry leaves`** — for every facet group: `countOpt` is `Some (>0)`; every branch
  has non-empty children; each branch's leaf tally equals its `(count)` badge; each
  branch leaf resolves through `entryIdOfNodeCode` to a real id; and the group total
  equals the *distinct* entries reachable by expanding it. This single assertion
  fails on the old code twice over (group `countOpt = None`; branch `children = []`).
- **`the single-valued … facet totals the whole population and its branch counts
  sum to it`** — Category (Materials) / Kind (Library): group count = `resultCount`
  and the branch counts partition it ("the math adds up").
- **`a branch-nested entry leaf code selects its …, while bare branch and heading
  codes stay inert`** — the path-shaped leaf code resolves and dispatches
  `SelectEntry`; bare `facet:` / `branch:` codes remain `None`.

---

## 5. Verification

- **Build:** `dotnet build Berreman.slnx -c Release` → **0 errors**. The only
  warnings are the exempt `NU1701` (Wolfram.NETLink) and `SYSLIB0051` (vendored
  MathNet C#); **zero from our code**.
- **Tests:** full `OpticalConstructor.Ui.Tests` → **683 passed, 0 failed, 0
  skipped** (the two window modules alone: 108 passed). `ui-smoke` count unchanged
  at 194; the default set rose 483 → 489 with the 6 new tests — counts only
  increased, no regression.
- **Line endings:** all four edited files are LF with no CRLF churn (`git diff
  --numstat` identical with and without `--ignore-cr-at-eol`).

Commits are the operator's / arc's to make; the working-tree changes are left
staged-free for review.
