# Step 009 — impl log (attempt 1)

## Progress

- [x] Read the task file, worker system prompt (`implement_worker.system-md` +
      shared base `arc-runner.system-md`), project prompt (Operator note: empty),
      slice spec, the 009 gate roster (`009.gates`), spec 0038 `.spec-md` Part D,
      steps 010/011 (the growth path), and the surrounding code
      (Domain/Tests fsprojs, WorkbenchSettings.fs + tests for the elevation and
      test idioms; grep confirmed no `Facets`/`AttributeDef` name collisions).
- [x] Wrote the impl-plan (`009-impl-plan.md`).
- [x] `Domain/Facets.fs` — the pure, Avalonia-free faceted engine, generic over
      `'Item`: `AttributeKey`/`DiscreteKey`/`TextQuery` (single-case string DUs
      with `.value`), `AttributeValue = DiscreteValue | NumericValue of double`
      (with a `.label` member — discrete key string / `%g` numeric),
      `Applicability`, `AttributeKind`, `AttributeDef<'Item>` exactly as the
      slice pins it (LIST-valued `extract` — multi-valued facets first-class),
      `NumericRange` (half-open `[lower, upper)`, degenerate `lower = upper` =
      exact value, inverted pair selects nothing), `FacetSelection`
      (`DiscreteSelection of Set<DiscreteKey>` OR-within / `NumericRangeSelection`),
      `AppliedConstraint`, `Representation` (ordered key list), `ItemCount`
      (elevated count), `FacetBranch`/`FacetNode`/`FacetTree`, `BreadcrumbCount`.
      Engine as naive folds with explicit concrete signatures: `filter` (AND
      across facets, OR within one, applicability-gated, fail-closed on an
      unknown key), `countFor` (candidate appended to the applied set),
      `breadcrumbCounts` (one incremental fold, cumulative after-counts in
      application order), `buildTree` (facets in representation order; branches
      derived from the filtered population so zero-count branches are absent by
      construction; items count once per branch, once in EACH branch they carry;
      attributes inapplicable to every filtered item omitted entirely; empty
      extraction and unknown representation keys also omitted). Plain-text
      filter as an ORDINARY constraint: `textFilterDef` (caller-supplied text
      extractor; ordinal case-insensitive containment onto a `textMatchKey`
      sentinel) + `textFilterConstraint`.
- [x] `Domain/OpticalConstructor.Domain.fsproj` — `Facets.fs` registered at the
      end of the compile list (depends on nothing; steps 010/011 append after it).
- [x] `Tests/FacetsTests.fs` — 26 facts over a FIXED six-item corpus (multi-valued
      `kinds`, single-valued `family`, dependent numeric `thicknessNm`, an
      upper-case name and a duplicated kind as probes): OR within a facet, AND
      across facets, inapplicable-item exclusion, half-open/degenerate/inverted
      numeric ranges, fail-closed unknown key, `countFor` previews (with and
      without an applied set), empty/cumulative/order-sensitive breadcrumb
      after-counts, the full expected `FacetTree` over the unconstrained corpus,
      multi-valued branch counts exceeding the total, at-most-once-per-branch,
      zero-count-branch absence, inapplicable-attribute omission, empty-extract
      omission, representation-order independence, unknown representation key,
      and the text filter (case-insensitive match, empty query matches all,
      ANDing, breadcrumb chip, countFor preview).
- [x] `Tests/OpticalConstructor.Tests.fsproj` — `FacetsTests.fs` registered at
      the end of the compile list.
- [x] Diagnostic build + all four suite runs green (see Testing state); LF check
      clean.
- [x] State-of-the-world written (`009-state-of-the-world.md`).

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/Facets.fs` (NEW)
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/OpticalConstructor.Domain.fsproj`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/FacetsTests.fs` (NEW)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj`

## Testing state

Gates are executed by the arc-runner's deterministic gate engine after this
worker exits (IMPLEMENT Invariant 6 — the worker acts, it runs no checks).
Diagnostic verification only (not gate authority), logs in `.artifacts/`:

- `dotnet build Berreman.slnx -c Release -nologo -v:m` — **Build succeeded,
  0 errors, no MSB3277**; the touched projects (Domain / Tests) compiled with
  ZERO warnings; the 10 warning lines are exactly the step-001-catalogued
  pre-existing set in untouched files (FS1125 SeriesDataTests ×4, FS3873
  Dispersion, FS0044 ChartWindow, SYSLIB0051 vendored MathNet ×2, NU1701
  Wolfram.NETLink ×2). (`009-diag-build.log`)
- constructor-unit-tests: **505/505** (checkpoint 479, +26 — the whole
  FacetsTests suite). (`009-diag-constructor-tests.log`)
- unit-tests (BerremanTests): **119 passed / 5 pre-existing skips**
  (== checkpoint). (`009-diag-unit-tests.log`)
- ui-smoke: **124/124** (== checkpoint). (`009-diag-ui-smoke.log`)
- ui-tests: **361/361** (== checkpoint). (`009-diag-ui-tests.log`)
- Line endings: `git diff --numstat` equals `--ignore-cr-at-eol --numstat` on
  the tracked edits, and a byte-level probe shows 0 CRLF sequences in all four
  new files (the CRLF warning on `.manifest.state.json` is the supervisor's own
  working-copy state, pre-dating this round).

No compile fixes were needed — the first build was green.

## Artifacts

- `specs/0038/.artifacts/009-diag-build.log` — full Release build.
- `specs/0038/.artifacts/009-diag-constructor-tests.log` — constructor suite (505 passed).
- `specs/0038/.artifacts/009-diag-unit-tests.log` — BerremanTests (119 passed, 5 skips).
- `specs/0038/.artifacts/009-diag-ui-smoke.log` — ui-smoke run (124 passed).
- `specs/0038/.artifacts/009-diag-ui-tests.log` — ui-tests run (361 passed).

## Gotchas

- No operator note in flight (the project prompt's Operator note section is empty).
- **The task file's system-prompt path does not exist**
  (`C:\GitHub\AI-Strategy-Generator\implement_worker.system-md`); the real file is
  `…\src\ai_strategy_generator\multistep\implement_worker.system-md` — located by
  glob and read in full (the step-007/008 gotcha recurred again).
- **Numeric range semantics chosen for the step-010 handshake**: membership is
  half-open `[lower, upper)` — the bucket-interval shape — with the degenerate
  `lower = upper` pair meaning the exact value, so step 010's
  single-distinct-value bucket collapse is representable as an ordinary
  `NumericRangeSelection` chip. An inverted pair selects nothing (total, no
  `tryCreate`). All three behaviours pinned in tests.
- **Fail-closed on an unknown constraint key**: a constraint no def explains
  matches NOTHING (zero results), never a silently ignored chip — the
  interpretation that surfaces a wiring bug immediately. Pinned.
- **A constraint requires applicability**: items for which the constrained
  attribute is `InapplicableAttribute` fail that constraint (constraining on
  "per-film thickness" excludes items without films). Pinned.
- **Branch counts count items, not extractions**: `extract` output is deduped
  per item (`List.distinct`) before branch grouping, so an item extracting the
  same value twice counts once in that branch while multi-valued items count
  once in EACH branch — branch counts need not sum to the total. Pinned both ways.
- **Two omission rules beyond the slice's explicit ones, recorded**: an
  applicable attribute that extracts no values for any filtered item yields no
  facet node (an empty facet header is noise), and a representation key with no
  def is skipped. Both pinned.
- **The text filter's engine shape**: the query lives in the DEF (built per
  query commit by the caller), not in the constraint — `extract` yields the
  `textMatchKey` sentinel exactly when the item's text contains the query
  (ordinal case-insensitive; empty query matches everything), and the
  constraint selects that sentinel. That is what makes it "an ordinary
  constraint": it ANDs, previews through `countFor`, and takes a breadcrumb
  after-count with zero special-casing in the engine.
- **Branch order is the structural sort of `AttributeValue`** (discrete by key
  string ordinal, numeric by magnitude) — deterministic and independent of
  corpus order; step 011's name-sorted requirements ride on it.
- **Counts are elevated** (`ItemCount` with `.value`) per the repo discipline;
  `name : string` / `label : string` stay bare strings exactly as the slice
  pins the `AttributeDef` shape. Type parameter written `'Item` (repo style)
  where the slice writes `'item`.
- Step 002–008 carried-over gotchas remain valid (baselines come from
  `.checkpoints-json`, not the SoW YAML; the registry is app-global test state
  in Ui.Tests; the appsettings.json write-back into test output copies is
  expected).
