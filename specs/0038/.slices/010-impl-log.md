# Step 010 — impl log (attempt 1)

## Progress

- [x] Read the task file, worker system prompt (`implement_worker.system-md` +
      shared base `arc-runner.system-md`), project prompt (Operator note: empty),
      slice spec, the 010 gate roster (`010.gates`), spec 0038 `.spec-md` Part D,
      and the surrounding code (`Facets.fs` + tests for the step-009 handshake,
      `WorkbenchSettings.fs` `ThicknessBucketCap`, `Units.unitAbbrev`, both
      fsprojs, and BerremanTests' FsCheck 3.3.3 usage pattern).
- [x] Wrote the impl-plan (`010-impl-plan.md`).
- [x] `Domain/FacetBuckets.fs` (NEW) — the 1–2–5 log-ladder numeric bucket
      builder, sibling of step 009's engine (the slice offered "Facets.fs or a
      sibling FacetBuckets module"; the sibling keeps the proven engine file
      untouched): `NumericBucket = { range : NumericRange; count : ItemCount }`
      (step-009 elevated types reused) with `contains` (half-open
      `[lower, upper)`, exact-value degenerate) and `label` (`'10-20 nm (7)'`;
      nm below 1 µm, µm at or above, straddling intervals name each endpoint's
      own unit; unit symbols reused from `Units.unitAbbrev`, the sole home of
      unit names); `ladderBoundaries` (strictly increasing rungs `m × 10^k`,
      `m ∈ {1, 2, 5}`, from the largest rung ≤ min to the smallest rung
      STRICTLY above max — log10 only as the initial guess, corrected by
      step-down/step-up loops; non-positive/non-finite/inverted spans yield
      `[]`, total, no throw); `buildBuckets` (single-distinct-value collapse to
      one exact bucket; else pairwise intervals → count → drop empties → merge
      fewest-first to the cap; a merge's union interval absorbs dropped-empty
      gaps); `constraintFor` (a bucket click is an ORDINARY
      `NumericRangeSelection` chip — the same path as a manual min–max entry);
      `bucketsFor` (the constrained-population entry point: `Facets.filter`
      under the applied set, applicability-gated per-item-deduped numeric
      extraction, then `buildBuckets` — pure, so recomputation on every
      constraint change is just re-calling).
- [x] `Domain/OpticalConstructor.Domain.fsproj` — `FacetBuckets.fs` registered
      after `Facets.fs` (uses Facets + WorkbenchSettings + Units, all earlier).
- [x] `Tests/FacetBucketsTests.fs` (NEW) — 22 facts: ladder boundary generation
      (start rung at/below min, mid-rung min, rung-at-max extending one rung
      above, inverted/non-positive spans), empty-bucket dropping, half-open
      edge membership (lower in / upper in the next bucket), `contains`
      (half-open + exact degenerate), fewest-first merging (single fewest-pair
      merge 5/1/2/7@cap3, cascade + leftmost tie 1/1/1/1@cap2, gap-bridging
      merge, no merge within the cap), single-distinct-value collapse, empty
      population, all four label shapes (nm range, µm range, 1 µm straddle,
      exact nm + exact µm), recomputation from the NARROWED population's
      min/max under a constraint chip, bucket-as-ordinary-chip (chip reproduces
      exactly the bucket count through `filter`), manual min–max as the same
      ordinary chip shape, per-item extraction dedup, and the FsCheck property
      (caps 1–12, magnitudes 0.01–50000, 500 runs): every magnitude lands in
      EXACTLY one bucket and the bucket count respects the cap.
- [x] `Tests/OpticalConstructor.Tests.fsproj` — `FacetBucketsTests.fs`
      registered after `FacetsTests.fs`; `FsCheck 3.3.3` added (the version
      BerremanTests pins; property run via `Check.One` +
      `Config.QuickThrowOnFailure` inside a `[<Fact>]` — the solution's
      established FsCheck-under-xunit pattern; `[<Property>]` is used nowhere
      in the solution).
- [x] Diagnostic build + all four suite runs green (see Testing state); LF
      check clean.
- [x] State-of-the-world written (`010-state-of-the-world.md`).

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/FacetBuckets.fs` (NEW)
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/OpticalConstructor.Domain.fsproj`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/FacetBucketsTests.fs` (NEW)
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
  Wolfram.NETLink ×2). FsCheck 3.3.3 restored cleanly (no NU1701).
  (`010-diag-build.log`)
- constructor-unit-tests: **527/527** (checkpoint 505, +22 — the whole
  FacetBucketsTests suite including the FsCheck property).
  (`010-diag-constructor-tests.log`)
- unit-tests (BerremanTests): **119 passed / 5 pre-existing skips**
  (== checkpoint). (`010-diag-unit-tests.log`)
- ui-smoke: **124/124** (== checkpoint). (`010-diag-ui-smoke.log`)
- ui-tests: **361/361** (== checkpoint). (`010-diag-ui-tests.log`)
- Line endings: `git diff --numstat` equals `--ignore-cr-at-eol --numstat` on
  the tracked edits, and a byte-level probe shows 0 CRLF sequences in all four
  new files (the CRLF warning on `.manifest.state.json` is the supervisor's own
  working-copy state, pre-dating this round).

One compile fix during the round: a bare negative literal in
`ladderBoundaries -5.0 10.0` parses as subtraction in F#; parenthesised to
`(-5.0)` before the first build. The first build was green.

## Artifacts

- `specs/0038/.artifacts/010-diag-build.log` — full Release build.
- `specs/0038/.artifacts/010-diag-constructor-tests.log` — constructor suite (527 passed).
- `specs/0038/.artifacts/010-diag-unit-tests.log` — BerremanTests (119 passed, 5 skips).
- `specs/0038/.artifacts/010-diag-ui-smoke.log` — ui-smoke run (124 passed).
- `specs/0038/.artifacts/010-diag-ui-tests.log` — ui-tests run (361 passed).

## Gotchas

- No operator note in flight (the project prompt's Operator note section is empty).
- **The task file's system-prompt path does not exist**
  (`C:\GitHub\AI-Strategy-Generator\implement_worker.system-md`); the real file
  is `…\src\ai_strategy_generator\multistep\implement_worker.system-md` —
  located by glob and read in full (the step-007/008/009 gotcha recurred again).
- **"Fewest-items-first" merge interpreted as pair-minimal**: while the bucket
  count exceeds the cap, the adjacent PAIR with the fewest COMBINED items
  merges (the leftmost pair on ties — `List.minBy` keeps the first minimum),
  then the scan repeats. Deterministic; both the order and the tie-break are
  pinned in tests. (The alternative reading — "merge the single
  fewest-populated bucket into a neighbour" — needs a second, unspecified
  neighbour-choice rule, so the pair reading was chosen.)
- **A max sitting exactly on a rung extends the ladder one rung above** (the
  top interval's upper bound is exclusive, so a max on a rung would otherwise
  fall outside every bucket). Symmetrically the first boundary is the largest
  rung AT OR BELOW min. Pinned.
- **Merges bridge dropped-empty gaps**: after empty buckets drop, "adjacent"
  means adjacent in the surviving list; the merged interval is the union
  `[lower_left, upper_right)` and absorbs the gap — which holds no population
  magnitude by construction. Pinned.
- **The ladder's domain is positive finite magnitudes** (log ladder): the
  builder drops non-positive/non-finite values defensively rather than
  throwing, and `ladderBoundaries` yields `[]` on a non-positive or inverted
  span (total, no `tryCreate` — the elevated upstream length types already
  forbid such magnitudes). The FsCheck property generates the documented
  domain (0.01–50000 nm).
- **`Math.Log10` is only an initial guess** for the ladder start; step-down/
  step-up correction loops make "largest rung ≤ min" exact under floating
  rounding (an under-guess costs at most a leading empty bucket, which drops;
  an over-guess is corrected by the loop). Test expectations use decade rungs
  ≥ 10, which are exact doubles; the property test uses membership, never
  float equality.
- **Label units reuse `Units.unitAbbrev`** (Units.fs declares itself the ONLY
  home of unit names). The Ui `bandThicknessLabel`
  (`TableAndElementRotationView.fs:1868`) was considered for reuse and
  rejected: it lives in a layer Domain cannot reference and has a different
  contract (`%.1f`/`%.3g` formatting, an mm tier, no counts). An interval
  straddling 1 µm names each endpoint's own unit (`'500 nm-1 µm (2)'`) —
  recorded choice, pinned.
- **Bucket counts count magnitudes after per-item dedup** (`List.distinct` per
  item in `bucketsFor`, step 009's at-most-once-per-branch discipline): an
  item extracting the same magnitude twice counts once (pinned). An item
  extracting TWO DIFFERENT magnitudes that land in the SAME bucket counts
  twice at this layer — per-item-per-bucket dedup (if the multi-valued
  per-film facet wants item counts instead of film counts) is step 011's
  catalogue decision, deferred.
- **FsCheck runs through `Check.One` + `Config.QuickThrowOnFailure` inside a
  `[<Fact>]`** — FsCheck.Xunit's `[<Property>]` attribute is not used anywhere
  in the solution (BerremanTests references but never uses it), and
  `QuickThrowOnFailure` (unlike `Config.Quick`) makes a falsified property
  actually fail the fact.
- Step 002–009 carried-over gotchas remain valid (baselines come from
  `.checkpoints-json`, not the SoW YAML; the registry is app-global test state
  in Ui.Tests; the appsettings.json write-back into test output copies is
  expected).
