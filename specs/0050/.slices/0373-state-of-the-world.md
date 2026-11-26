# Step 0373 — state of the world

## Where we are

Spec 0050 section S11.2 (the forward model of an experiment and the study residual). Step 371 placed each measured
point on its scan; step 372 laid out the study's parameter vector, one value per owner of each unknown. This step
turns one vector into the numbers one experiment is evaluated with: each item's parameter values and each opened
material's crystal, with links applied, plus a material store that resolves an opened version to the study's working
copy (001 §I, §L). Later S11.2 steps evaluate an experiment's predictions and the study residual from this assignment.

## What's working

- Add `ExperimentAssignment` (item values and opened-material crystals) with `valuesOf`, `crystalOf` and `materialProxyOver`
- Add `StudyParameterLayout.assignmentFor`: each entry's value goes to the owner holding the experiment, every other parameter keeps its own, and link followers take their leader's value
- Resolve an opened material version through `materialProxyOver` to the study's working copy, so stack samples made of it use it
- Add `StudyForwardError.VectorLengthErr`, refusing a vector of another length and naming both lengths
- Add four `StudyForwardTests` facts and widen the error-render fact (constructor suite 1625 → 1629)

## Tests

```yaml
gates:
  constructor_unit_tests: 1629
```

- build: exit 0, no warning from our code (only NU1902 advisories, exempt); 24 s (deadline 600 s).
- constructor-unit-tests: Passed!, 1629 succeeded (1625 at checkpoint 372, +4), 0 failed, 0 skipped, 71 s (deadline
  180 s). The four new facts and `every forward error renders its condition and its payload` are `Passed` in the TRX.
- elevation from arc base `f395597c`: OK, 0 new findings in 13 projects, 227 s (deadline 600 s).
- The twenty s37 invariants: each exits 0.
- Nothing deferred. All durations are within their deadlines.

## Architecture

- The assignment is pure data: `Map<ItemId, ParameterValues>` and `Map<MaterialVersionId, CrystalParameters>`. No cache
  or option was added. Callers read it through `valuesOf` / `crystalOf`, which fall back to an item's own values or a
  copy's `currentCrystal`.
- `assignmentFor` works in the how-to's order. The vector length is checked first. Then the entries whose owner holds
  the experiment are applied: `ItemOwner` and `MaterialOwner` always; a `SessionOwner` whose session of `sessions`
  lists the experiment; the experiment's own `ExperimentOwner`. Values go to items (`withValue`) and to crystals
  (through the generated axis, `writeValue`). Last, each link's follower is set from its leader's value. Links don't
  chain (`withLink`), so one pass is enough.
- `materialProxyOver` wraps only `resolveVersion`, the read `Propagation.resolveSampleMaterials` uses for every layer.
  A bound stack sample therefore resolves an opened version to the assigned crystal, and every other version and
  store function passes through unchanged.

## Deferred

Nothing.

## Gotchas

- A link whose leader isn't among the `items` passed (or names no component) changes nothing, because its leader has
  no value in the assignment. Pass every item the study uses, not just one experiment's.
- An experiment in no session (a Diagnostic run) keeps an item's own value for any session-scoped parameter.
- In the test fixture, `freeing` is now `stating name state draft` and `forwardFor` delegates to
  `forwardOver items`.

## Changelog

- 2026-11-13 — Step 0373: the experiment assignment in `StudyForward.fs` (`ExperimentAssignment` with `valuesOf`,
  `crystalOf`, `materialProxyOver`; `StudyParameterLayout.assignmentFor`; `StudyForwardError.VectorLengthErr`), plus four
  `StudyForwardTests` facts and the widened error-render fact (constructor suite 1625 → 1629).
