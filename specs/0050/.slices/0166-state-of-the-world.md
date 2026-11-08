# Step 0166 — IMPLEMENT — state of the world

## Where we are

Spec 0050, S5.1 kit history. Step 164 added `KitStar` (`StarredKit` 1, `UnstarredKit` 2, with
`tryOfWire`) and the history model, and step 165 declared the `KitHistoryProxy` contract. This step
makes the canonical JSON stack store a star as its int disk tag, ahead of the history file's store.

## What's working

- Register `KitStar` in `CanonicalJson.options` as an int-tag closed set, through step 28's
  `ClosedSetTagConverter`.
- Write a star as its tag (`StarredKit` as 1, `UnstarredKit` as 2) and refuse an unknown tag with
  `JsonException`, which callers report as `MalformedJsonErr`.
- Extend the two CanonicalJsonTests closed-set facts over `KitStar`, with no change to the test
  count.

## Tests

As Invariant 6 requires, this worker ran no gates: `gates_run: []`, `gate_results: {}`. The gate
engine runs build, elevation, constructor-unit-tests and the thirteen s19 invariants.

Expected effects:

- constructor-unit-tests is unchanged at checkpoint 164's 1,149. Two existing facts change their
  expectations (S51-STAR-TAG), and none is added or removed.
- ui-smoke is not in this step's roster.

```yaml
gates:
  constructor_unit_tests: 1149
```

## Architecture

- The canonical options now hold five int-tag closed sets, all instances of the one generic
  `ClosedSetTagConverter`. A future closed set joins with one registration line before the
  `JsonFSharpConverter`, never with a new converter class.

## Deferred

Nothing.

## Gotchas

- An int-tag converter must be registered before `JsonFSharpConverter`. Otherwise FSharp.SystemTextJson
  claims the union first and writes its case name as a string.
- `CanonicalJson.fs` and `CanonicalJsonTests.fs` now open `OpticalConstructor.Domain.KitHistory`. That
  brings a type named `KitHistory` into scope, so code added to either file must not use that name
  for anything else.

## Changelog

- 2026-11-08 — Step 0166: `KitStar` is written as its int tag through `CanonicalJson.options`, and an
  unknown tag is refused. Two existing CanonicalJsonTests facts were extended over it.
