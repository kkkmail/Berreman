# Step 010 — state of the world

## Where we are

Step 010 is the middle of spec 0038 Part D (the faceted engine): step 009's
generic engine (`Domain/Facets.fs`) now has its numeric bucket builder —
`Domain/FacetBuckets.fs`, the module every `NumericAttribute` branch uses.
Bucket boundaries come from the 1–2–5 log ladder spanning the CURRENTLY
CONSTRAINED population's min/max; empty buckets drop; adjacent buckets merge
fewest-items-first while the count exceeds `ThicknessBucketCap` (step 005,
default 8); intervals are half-open `[lo, hi)` with the single-distinct-value
population collapsing to one exact-value bucket; labels read `'10-20 nm (7)'`
(nm below 1 µm, µm at or above); and a bucket applies as an ORDINARY
`NumericRangeSelection` chip — the same path as a manual min–max entry.
Step 011 builds the concrete material/library facet catalogues on top of both
modules; Part E renders everything through `FacetedTreeControls`.

## What's working

- Add Domain/FacetBuckets.fs: the 1–2–5 log-ladder numeric bucket builder —
  ladderBoundaries (largest rung ≤ min to smallest rung > max, total on
  degenerate spans), buildBuckets (empty buckets dropped, fewest-first
  pair-merging under ThicknessBucketCap, half-open intervals, exact-value
  collapse), NumericBucket with contains + nm/µm labels via Units.unitAbbrev.
- Wire the bucket set to the engine: bucketsFor derives buckets from the
  CURRENTLY CONSTRAINED population (filter + applicability-gated per-item
  deduped extraction), so every constraint change recomputes the ladder from
  the narrowed min/max; constraintFor applies a bucket as an ordinary
  NumericRangeSelection chip reproducing exactly its count.
- 22 new pure facts in FacetBucketsTests covering every acceptance
  observation, plus the FsCheck property (FsCheck 3.3.3, 500 runs) that every
  magnitude lands in exactly one bucket and the cap is respected.
- Suites 527 (+22) / 119 / 124 / 361; build clean, no MSB3277, zero warnings
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
  vendored MathNet ×2, NU1701 Wolfram.NETLink ×2); FsCheck 3.3.3 restored
  cleanly. Suites: OpticalConstructor.Tests **527/527** (checkpoint 505, +22:
  the whole FacetBucketsTests suite — ladder boundary generation, empty-bucket
  dropping, half-open membership, fewest-first merge order + leftmost
  tie-break + gap-bridging, exact-value collapse, all four label shapes,
  recomputation under a narrowed population, bucket/manual chips through the
  ordinary constraint path, per-item dedup, and the FsCheck exactly-one-bucket
  property), BerremanTests **119 passed / 5 pre-existing skips**
  (== checkpoint), ui-smoke **124/124** (== checkpoint), ui-tests **361/361**
  (== checkpoint). Logs in `specs/0038/.artifacts/010-diag-*.log`.
- Nothing deferred.

## Architecture

- **The bucket builder is a sibling module, not an engine change**:
  `FacetBuckets.fs` layers on step 009's public surface (`NumericRange`,
  `ItemCount`, `AttributeDef`, `filter`) exactly as that step's comments
  anticipated — `Facets.fs` is untouched, and its numeric branch-per-value
  `buildTree` behaviour stays pinned. Step 011's catalogues (and Part E's
  control) consume buckets through `bucketsFor`/`constraintFor`.
- **Buckets ARE ordinary constraints**: a bucket carries the same half-open
  `NumericRange` the engine's `NumericRangeSelection` matches (exact-value
  degenerate included), so a bucket chip reproduces exactly its displayed
  count through the ordinary `filter` path, and a manual min–max entry is the
  same shape — no special path exists anywhere.
- **Recomputation is purity, not machinery**: `bucketsFor` derives the ladder
  from the constrained population's min/max on every call; "recomputed on
  every constraint change" needs no cache, no invalidation, no state.
- **Fewest-items-first merging is pair-minimal and deterministic**: the
  adjacent pair with the fewest combined items merges first, leftmost on ties;
  merges bridge dropped-empty gaps with a union interval (the gap holds no
  population magnitude by construction), preserving the exactly-one-bucket
  partition invariant that the FsCheck property pins.
- **Unit names stay in Units.fs**: bucket labels reuse `Units.unitAbbrev`
  (Nanometer/Micrometer) — the module that declares itself the only home of
  non-SI unit names — with the §7.5 magnitude switch (nm below 1 µm) local to
  the builder.

## Deferred

- The concrete material/library facet catalogues (extractors over
  `MaterialEntry` / `LibraryEntry`, `availableGyrationClasses` reuse, sample
  facets as the distinct union over constituents, per-film thickness buckets)
  — step 011.
- Per-item-per-bucket dedup for multi-valued numeric facets (an item whose two
  DIFFERENT film thicknesses land in the same bucket currently counts twice;
  whether the per-film facet counts items or films is step 011's catalogue
  decision).
- Tree rendering, Show/Search gating above `TreeAutoBuildThreshold`, and the
  representation picker — Part E (`FacetedTreeControls`).
- The pre-existing warnings in untouched files remain for spec 0038 Part N's
  sweep (carried from steps 002–009).

## Gotchas

- **The task file's system-prompt path was stale again** — the IMPLEMENT
  worker prompt lives under
  `src/ai_strategy_generator/multistep/implement_worker.system-md` in the tool
  repo; located and read in full (the step-007/008/009 gotcha recurred).
- **"Fewest-items-first" interpreted as pair-minimal**: the adjacent PAIR with
  the fewest COMBINED items merges first (leftmost on ties), repeated to the
  cap — the alternative "fewest bucket into a neighbour" reading needs a
  second, unspecified neighbour-choice rule. Recorded interpretation, pinned
  in tests.
- **A max exactly on a rung extends the ladder one rung above** (half-open top
  bound), and the first boundary is the largest rung AT OR BELOW min —
  `Math.Log10` is only a guess, corrected by step loops, so floating rounding
  cannot mis-anchor the ladder. Pinned.
- **The ladder's domain is positive finite magnitudes**: non-positive/
  non-finite values are dropped defensively (no throw; upstream elevated
  length types already forbid them), and `ladderBoundaries` is total (`[]` on
  degenerate spans).
- **Straddling labels name each endpoint's own unit** (`'500 nm-1 µm (2)'`) —
  recorded choice for intervals crossing the 1 µm switch, pinned.
- **Ui's `bandThicknessLabel` was NOT reused** — wrong layer (Ui) and wrong
  contract (`%.3g`, mm tier, no counts); unit symbols come from
  `Units.unitAbbrev` instead.
- **FsCheck runs via `Check.One` + `Config.QuickThrowOnFailure` in a
  `[<Fact>]`** — the solution's established pattern (`[<Property>]` is used
  nowhere), and unlike `Config.Quick` a falsified property actually fails the
  fact.
- Step 002–009 carried-over gotchas remain valid (baselines come from
  `.checkpoints-json`, not the SoW YAML; the window registry is app-global
  test state in Ui.Tests; the appsettings.json write-back into test output
  copies is expected).

## Changelog

- 2026-07-10 — Step 010 (IMPLEMENT, attempt 1): added the 1–2–5 log-ladder
  numeric bucket builder `Domain/FacetBuckets.fs` (ladder boundaries over the
  constrained population's min/max, empty buckets dropped, fewest-first
  pair-merging under ThicknessBucketCap, half-open intervals, exact-value
  collapse, nm/µm labels via Units.unitAbbrev, bucketsFor/constraintFor onto
  the step-009 engine) with 22 pure facts in FacetBucketsTests including the
  FsCheck exactly-one-bucket property (FsCheck 3.3.3 added to the Tests
  project). Build clean (no MSB3277); suites 527 / 119 / 124 / 361.

```yaml
gates:
  berreman_unit_tests: 119
  constructor_unit_tests: 527
  ui_smoke_tests: 124
  ui_tests: 361
```
