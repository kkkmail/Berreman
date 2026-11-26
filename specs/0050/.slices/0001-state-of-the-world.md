# Step 0001 — ADD_RED — state of the world

## Where we are

Step 1 of spec 0050, section S1.1 ("Every dispersion axis is data: tabulated and transcendental
cases"). It declares RED_0001: a tabulated refractiveindex.info / n,k CSV import must become axis
data (001 §B "Physics is pure data", .spec-md §3.1) instead of the closure-backed `complexity =
None` entry it is today. Step 2 (CLOSE_RED) lands `TabulatedNK` in the core and builds it in
`entryFromTabulated`, closing the red.

## What's working

- Added the failing-first fact that a tabulated refractiveindex.info page imports as one view-only dispersive segment over its sample range
- Added the same fact for an n,k CSV import (400 nm to 700 nm)
- Added a green fact pinning every tabulated sample and every neighbouring midpoint, so the closure-to-data move in step 2 cannot move a number
- Wrote both fixtures' rows out by hand in the test; no fixture is parsed a second way

## Tests

- Observed locally: `MaterialImportTests` runs 16 facts, 14 pass and 2 fail, exactly the two
  shape facts, each on "got a None complexity" (logs in `.artifacts/0001-red-materialimporttests.log`).
- `build`: the test project and everything it references build with no warning from our code
  (`.artifacts/0001-build-tests-project.log`).
- `elevation` and the three s01 invariants: nothing in their scope changed (test projects are
  outside the analyzers; no project, props, targets or known-findings file touched).
- Gate execution and recording belong to the arc-runner's gate engine.

```yaml
gates:
  constructor_unit_tests: 14
```

## Architecture

- No production change. The facts assert the target shape against today's types only
  (`EpsWithDispValue`, `IsotropicDispersive`, `MaterialComplexity.isEditable`,
  `toProperties`), so they compile on the start tree and need no edit when `TabulatedNK` lands.
- The segment range is checked by `WaveLength.value` in metres, so the `WaveLength.nm` endpoints
  step 2 builds pass regardless of the fixture's unit.

## Deferred

- Nothing. The implementation that turns this red green is step 2's (CLOSE_RED, `closes:
  RED_0001`), as the arc declares.

## Gotchas

- Do not compare the segment's `WaveLength` endpoints structurally: a µm fixture and an
  `nm`-built range are equal in metres but different cases.
- The seed-invariant comparison is exact `=` on `Eps` (MathNet matrix equality is element-wise);
  keep `properties = complexity.toProperties` literally in step 2 and it holds to the bit.

## Changelog

- 2026-11-05 — step 0001: three `MaterialImportTests` facts added; two red on the `None`
  complexity of tabulated imports, one green interpolation pin.
