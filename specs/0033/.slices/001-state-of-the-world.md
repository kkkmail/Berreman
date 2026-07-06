# State of the world — spec 0033, slice 001

# Where we are

Slice 001 is the first IMPLEMENT step of arc 0033: it converts the Optical
Constructor's sample presets from id-keyed, hand-built engine systems into DATA.
`Library.Sample` now carries a `SampleStructure` (layers, repeated period groups,
optional substrate plate, lower half-space), every referenced material resolves
through the `MaterialLibrary` with a typed error on an unknown id, and
`Propagation.sampleToSystem` is a total function over the resolved structure with
no per-sample string branching. Step 2 (elevating `materialId`) and step 20
(structure rotation) build directly on these types.

# What's working

- Add SampleLayer / PeriodGroup / StackItem / SampleStructure with a pure expansion member; Sample now carries structure instead of materialId + thickness
- Re-seed all 11 Library samples structurally (QW = 20×[glass; vacuum] + trailing glass, EUV = 100×[Mo; Si], langasite film over silicon lower)
- Add MaterialLibrary built-ins vacuum, euv-molybdenum, euv-silicon, active-crystal and a resolveMaterialWithDisp seam
- Replace sampleToSystem's id branching and glass-fallback map with resolveSampleMaterials (typed UnknownMaterialId error) + a total ResolvedSample -> WaveLength -> OpticalSystem
- Re-point sampleMueller and the R2/λ sweep builders at ResolvedSample; the TestWindows host resolves once per run and surfaces the error as the chart message
- Prove structurally-built systems equal the previously hand-built ones (films, substrate, tensors) for every seeded sample; unknown ids return typed Errors

# Tests

All gates in the slice roster pass in the worker's local (advisory) run; the
arc-runner gate engine re-runs them authoritatively after exit.

- `build` — solution builds Release/x64, 0 errors (`--warnaserror+:25` clean).
- `unit-tests` (BerremanTests) — 84 passed, 5 skipped (pre-existing skips), 0 failed.
- `constructor-unit-tests` — 306 passed, 0 failed (5 tests added this round:
  2 unknown-id typed-error tests, seeded-samples-resolve, legacy-system-equality,
  expandedFilms-mirrors-RepeatBuilder.expand; none removed).
- `ui-smoke` — 54 passed, 0 failed.
- `ui-tests` — 249 passed, 0 failed.

Nothing deferred.

```yaml
gates:
  berreman_unit_tests:    84
  constructor_unit_tests: 306
  ui_smoke_tests:         54
  ui_tests:               249
```

# Architecture

- **Stack-as-data:** `SampleStructure` lives in the Library domain
  (`ElementId.fs`) with `materialId` as a plain string until step 2 elevates it.
  Expansion (`expandedFilms`) is a pure member mirroring `RepeatBuilder.expand`
  (`List.replicate count cell |> List.concat`) — the UI-side builder and the
  domain expansion cannot drift.
- **Resolve-once, evaluate-per-λ:** `ResolvedSample` holds engine
  `LayerWithDisp` values (dispersive, unevaluated). `resolveSampleMaterials`
  is the single Result-returning resolution seam (built on the new
  `MaterialLibrary.resolveMaterialWithDisp`); `sampleToSystem` then evaluates at
  the run wavelength and is total. Hosts resolve once per run; a typed
  `UnknownMaterialId` error surfaces as the chart's message, never a fallback.
- **New `MaterialCategory.Vacuum` case** (with the schema enum extended) so the
  QW spacer layers reference a real `vacuum` library entry instead of a
  special case.
- The `substrate : SubstrateKind` field stays on `Sample` as the pure geometry
  facet; the material facet is entirely in `structure`.

# Deferred

- Elevating `SampleLayer.materialId` (and `SampleStructure.lower`) to a typed
  material id — step 2 of this arc, per the slice text.
- Structure rotation in `sampleToSystem` — step 20, per the slice text.
- A "Vacuum" filter button in the Materials panel UI (the new category is
  reachable through the All filter; adding the button touches the Ui project,
  outside this slice).
- `Sample` JSON persistence of the new structure — no current serializer writes
  `Library.Sample` (verified); the storage schema only carries materialEntry.

# Gotchas

- The slice's built-in list omitted the `vacuum` entry the QW re-seed needs;
  added it (with the new category + one-line schema enum change in Storage's
  `optical-constructor-project.schema.json` — one file outside the declared
  `touches`, required to keep the AC-I10 schema-validation test green).
- The langasite system's `description` is now the sample name (was a hard-coded
  string); the acceptance's film/substrate/tensor equality is unaffected and the
  legacy-equality test compares with descriptions dropped.
- The schema JSON's worktree copy was CRLF before this round; normalized to LF.
  No CRLF churn anywhere (`git diff --numstat` identical with/without
  `--ignore-cr-at-eol`).
- MathNet matrix equality is value-based (`Matrix.Equals` → `Storage.Equals`),
  which is what makes whole-`OpticalSystem` record equality in the acceptance
  test sound (same precedent as StackEditTests.fs:286).

# Changelog

- 2026-07-05 — slice 001: sample stacks made DATA (SampleStructure + typed
  material resolution); id-branching sampleToSystem replaced by total
  ResolvedSample mapping; 4 new material built-ins; 11 samples re-seeded
  structurally; host resolves once per run; +5 tests, all suites green.
