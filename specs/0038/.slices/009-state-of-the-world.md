# Step 009 — state of the world

## Where we are

Step 009 opens spec 0038 Part D (the faceted engine): the generic
faceted-navigation core that will power both the Materials and Library windows
now exists as `OpticalConstructor.Domain/Facets.fs` — pure, Avalonia-free,
generic over the item type, with constraints as values and the engine as naive
folds (corpus is tens to hundreds; no indices). Items + attribute definitions
go in; a filterable tree with live counts comes out: `filter` (AND across
facets, OR within one), `countFor` (count-preview of a candidate constraint),
`breadcrumbCounts` (cumulative after-counts in application order), and
`buildTree` (facets in representation order, branches with label + count,
zero-count branches absent, inapplicable attributes omitted entirely). The
plain-text filter participates as an ordinary constraint over a caller-supplied
text extractor. Step 010 adds the 1–2–5 log-ladder numeric bucket builder on
top of `NumericRangeSelection`; step 011 adds the concrete material/library
facet catalogues; Part E renders it all through `FacetedTreeControls`.

## What's working

- Add Domain/Facets.fs: the generic faceted engine — elevated keys/values
  (AttributeKey, DiscreteKey, AttributeValue, ItemCount), AttributeDef with
  applicability and LIST-valued extract (multi-valued facets first-class),
  constraints as values (discrete key-set OR within a facet, half-open
  numeric range with exact-value degenerate).
- Engine functions as naive folds with explicit signatures: filter (AND
  across facets, OR within one, fail-closed on unknown keys), countFor,
  breadcrumbCounts (cumulative, application order), buildTree (representation
  order, per-branch item counts, zero-count branches and inapplicable
  attributes absent).
- Plain-text filter as an ordinary constraint: textFilterDef over a
  caller-supplied extractor (ordinal case-insensitive containment) +
  textFilterConstraint — it ANDs, previews, and takes a breadcrumb chip with
  no engine special-casing.
- 26 new pure facts in FacetsTests over a fixed six-item corpus, covering
  every acceptance observation including multi-valued branch counts that
  exceed the total.
- Suites 505 (+26) / 119 / 124 / 361; build clean, no MSB3277, zero warnings
  from touched projects.

## Tests

- Gates are executed by the arc-runner's deterministic gate engine after this
  worker exits (IMPLEMENT Invariant 6 — the worker acts, it runs no checks).
  The roster for this step is `build`, `unit-tests`, `constructor-unit-tests`,
  `ui-smoke`, `ui-tests`.
- Diagnostic verification (not gate authority): `dotnet build Berreman.slnx -c
  Release` succeeded with 0 errors, **no MSB3277**, and zero warnings from the
  touched projects (Domain / Tests) — the only warnings are the
  step-001-catalogued pre-existing set in untouched files (FS1125
  SeriesDataTests ×4, FS3873 Dispersion, FS0044 ChartWindow, SYSLIB0051
  vendored MathNet ×2, NU1701 Wolfram.NETLink ×2). Suites:
  OpticalConstructor.Tests **505/505** (checkpoint 479, +26: the whole
  FacetsTests suite — OR within / AND across, applicability exclusion,
  half-open/degenerate/inverted ranges, fail-closed unknown key, countFor
  previews, cumulative + order-sensitive breadcrumbs, the full expected tree,
  multi-valued counts exceeding the total, at-most-once-per-branch,
  zero-count absence, inapplicable omission, empty-extract omission,
  representation order, and the text filter end-to-end), BerremanTests
  **119 passed / 5 pre-existing skips** (== checkpoint), ui-smoke **124/124**
  (== checkpoint), ui-tests **361/361** (== checkpoint). Logs in
  `specs/0038/.artifacts/009-diag-*.log`.
- Nothing deferred.

## Architecture

- **Constraints are values, the engine is folds**: `AppliedConstraint list` is
  the whole search state — application order IS the breadcrumb order, and
  every engine function is a pure fold over (defs, constraints, items). No
  indices, no caching, per the Part D corpus-size mandate.
- **Applicability gates everything**: `extract` is never consulted on an item
  the attribute is inapplicable to; a constraint on an inapplicable attribute
  excludes the item, and an attribute inapplicable to every filtered item
  vanishes from the tree entirely — the "dependent facets vanish" rule falls
  out of one gate applied uniformly.
- **Zero-count branches are absent by construction**: branches derive from the
  currently filtered population (value → count of carrying items), not from a
  static vocabulary — so the drop rule needs no pruning pass, and corpus
  values eliminated by constraints simply never appear.
- **Half-open `[lower, upper)` numeric ranges with an exact-value degenerate**
  (`lower = upper`): the same interval shape step 010's buckets emit, so a
  bucket (or a single-distinct-value collapse) applied as a chip reproduces
  exactly its displayed count through the ordinary constraint path.
- **The text filter is a def + constraint pair, not an engine feature**: the
  caller bakes the query into a per-commit `textFilterDef` whose extract
  yields a match sentinel; the engine stays uniform, and the filter box gets
  ANDing, count-previews, and a removable breadcrumb chip for free.
- **Counts are elevated** (`ItemCount`), keys/queries are single-case DUs;
  `AttributeDef`'s `name : string` stays exactly the slice-pinned shape.

## Deferred

- The 1–2–5 log-ladder numeric bucket builder (empty buckets dropped,
  fewest-first merging under `ThicknessBucketCap`, nm/µm labels, recompute on
  every constraint change) — step 010, on top of `NumericRangeSelection`.
- The concrete material/library facet catalogues (extractors over
  `MaterialEntry` / `LibraryEntry`, `availableGyrationClasses` reuse,
  sample facets as the distinct union over constituents) — step 011.
- Tree rendering, Show/Search gating above `TreeAutoBuildThreshold`, and the
  representation picker — Part E (`FacetedTreeControls`).
- The pre-existing warnings in untouched files remain for spec 0038 Part N's
  sweep (carried from steps 002–008).

## Gotchas

- **The task file's system-prompt path was stale again** — the IMPLEMENT
  worker prompt lives under
  `src/ai_strategy_generator/multistep/implement_worker.system-md` in the tool
  repo; located and read in full (the step-007/008 gotcha recurred).
- **Fail-closed on unknown constraint keys**: a constraint whose key has no
  def matches NOTHING — a broken chip surfaces as zero results, never as a
  silently ignored filter. Recorded interpretation, pinned in tests.
- **Branch counts count items, not extractions**: extract output is deduped
  per item before grouping — an item extracting the same value twice counts
  once in that branch; multi-valued items count once in EACH branch, so branch
  counts need not sum to the total (both pinned).
- **Two omission rules recorded beyond the slice's explicit ones**: an
  applicable attribute extracting no values for any filtered item yields no
  facet node, and a representation key with no def is skipped (both pinned).
- **`countFor` appends the candidate to the applied set**; an OR-extension
  preview within an already-constrained facet is expressed by the CALLER
  passing the extended selection as the candidate (replacing that facet's
  chip) — the engine does not merge selections implicitly.
- **Branch order is the structural sort of `AttributeValue`** (discrete by
  ordinal key string, then numeric by magnitude) — deterministic under corpus
  reordering; step 011's name-sorted facets ride on it.
- Step 002–008 carried-over gotchas remain valid (baselines come from
  `.checkpoints-json`, not the SoW YAML; the window registry is app-global
  test state in Ui.Tests; the appsettings.json write-back into test output
  copies is expected).

## Changelog

- 2026-07-10 — Step 009 (IMPLEMENT, attempt 1): added the generic faceted
  engine `Domain/Facets.fs` (elevated keys/values, AttributeDef with
  applicability + LIST-valued extract, discrete/numeric constraints as values,
  filter / countFor / breadcrumbCounts / buildTree as naive folds, plain-text
  filter as an ordinary constraint) with 26 pure facts in FacetsTests over a
  fixed six-item corpus proving every acceptance observation. Build clean
  (no MSB3277); suites 505 / 119 / 124 / 361.

```yaml
gates:
  berreman_unit_tests: 119
  constructor_unit_tests: 505
  ui_smoke_tests: 124
  ui_tests: 361
```
