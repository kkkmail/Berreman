# Step 010 — impl plan (attempt 1)

## Slice

Add the numeric bucket builder every `NumericAttribute` branch uses (spec 0038
Part D §7.5): 1–2–5 log-ladder boundaries spanning the CURRENTLY CONSTRAINED
population's min/max, empty buckets dropped, adjacent buckets merged
fewest-items-first while the count exceeds `ThicknessBucketCap` (step 005,
default 8), half-open `[lo, hi)` intervals, single-distinct-value collapse to
an exact-value bucket, labels like `10-20 nm (7)` (nm below 1 µm, µm at or
above), recompute on every constraint change, manual min–max as an ordinary
`NumericRangeSelection` chip. Pure tests including an FsCheck
exactly-one-bucket property.

## Approach

New sibling module `OpticalConstructor.Domain/FacetBuckets.fs` (the slice
offers "Facets.fs or a sibling FacetBuckets module"; the sibling keeps step
009's proven engine file untouched), compiled after `Facets.fs`:

- `NumericBucket = { range : NumericRange; count : ItemCount }` — reuses step
  009's elevated types. Members: `contains` (half-open + exact-value
  degenerate, mirroring `NumericRange`'s documented contract) and `label`
  (`'10-20 nm (7)'`; unit symbols reused from `Units.unitAbbrev` — the sole
  home of unit names; the Ui `bandThicknessLabel` helper cannot be reused from
  Domain and has a different contract).
- `ladderBoundaries : double -> double -> double list` — strictly increasing
  rungs `m × 10^k`, `m ∈ {1, 2, 5}`, from the largest rung ≤ min to the
  smallest rung > max (so a max sitting exactly on a rung extends one rung
  above — the top interval's upper bound is exclusive). Log10 is only the
  initial guess; step-down/step-up loops make the start exact under floating
  rounding. Non-positive / non-finite / inverted spans yield `[]` (total, no
  throw).
- `buildBuckets : ThicknessBucketCap -> double list -> NumericBucket list` —
  distinct-1 population collapses to one exact bucket; otherwise pairwise
  ladder intervals → count → drop empties → merge to cap. Merge rule: the
  adjacent PAIR with the fewest combined items merges first (leftmost pair on
  ties), repeated until the count is at most the cap; a merge's union interval
  absorbs any dropped-empty gap between the pair.
- `constraintFor : AttributeKey -> NumericBucket -> AppliedConstraint` — a
  bucket click is an ORDINARY `NumericRangeSelection` chip (same path as a
  manual min–max entry; no special path exists).
- `bucketsFor : cap -> AttributeDef<'Item> -> defs -> applied -> items ->
  NumericBucket list` — the constrained-population entry point step 011's
  catalogues call: `Facets.filter` under the applied set, applicability-gated
  per-item-deduped numeric extraction, then `buildBuckets`. Pure, so
  "recomputed on every constraint change" is just re-calling it.

Tests in new `OpticalConstructor.Tests/FacetBucketsTests.fs`: ladder boundary
generation (including rung-at-max extension), empties dropped, half-open edge
membership, fewest-first merge order (single merge, cascade + leftmost tie,
gap-bridging merge), no merge within cap, single-value collapse, empty
population, all four label shapes (nm, µm, straddle, exact), recomputation
under a narrowed population, bucket-as-ordinary-chip (count reproduced),
manual-range-as-ordinary-chip, per-item dedup, and the FsCheck property that
every magnitude lands in exactly one bucket (cap respected), over caps 1–12
and magnitudes 0.01–50000. FsCheck 3.3.3 (the version BerremanTests pins)
added to the Tests fsproj; the property runs via `Check.One` +
`Config.QuickThrowOnFailure` inside a `[<Fact>]` (the repo's established
FsCheck-under-xunit pattern; FsCheck.Xunit's `[<Property>]` is not used
anywhere in the solution).

## Files to modify

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/FacetBuckets.fs` (NEW)
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/OpticalConstructor.Domain.fsproj`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/FacetBucketsTests.fs` (NEW)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj`

## Risks

- **FsCheck 3.3.3 API drift** (`Config.QuickThrowOnFailure`, `Prop.forAll`,
  `Arb.fromGen`): mitigated by following `BerremanTests/FourierTransformTests.fs`
  (same package version, same solution) and a diagnostic build.
- **Floating-point rung equality in tests**: expectations use decade values
  ≥ 10 whose rungs are exact doubles; the property test uses membership
  comparisons, never equality.
- **"Fewest-items-first" ambiguity**: interpreted as "the adjacent pair with
  the fewest combined items merges first, leftmost on ties" — deterministic,
  pinned in tests, recorded in the impl-log Gotchas.
