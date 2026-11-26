# Step 0485 — state of the world

## Where we are

Spec 0050, section S14.2: the Planner's numerical layer, the plan, the cache and the Plan view. Step 0484 gave the kit the
`.oc/planner/` folder and its two proxy verbs. This step builds the Planner's candidate cache on them:
- a key per candidate (`PlannerInputs.cacheKeyWith`);
- a file per key, named by the key's SHA-256 (`<hash>.candidate-json`), whose entry keeps its key;
- `KitContext.cachedPlannerRows`, which answers a hit without computing and computes a miss, writing it under
  read-write access only.

Step 0486's `planWith` takes a `rowsOf`, which this member wraps where the kit is open (step 0487).

## What's working

- Added `PlannerCacheKey` (source, plate and polarimeter items, branch, opened material, targets) and `PlannerCacheEntry`
- Added `PlannerInputs.cacheKeyWith`; an item no candidate reads is in no key, so adding one leaves every key unchanged
- Added `PlannerCacheStore`: the hashed file name, the canonical entry text and its MalformedJsonErr-safe parse
- Added `KitContext.cachedPlannerRows`: hit without computing, miss computed and written under read-write access only
- `PlannerError` gains `PlannerCacheErr`; five `PlannerCacheTests` facts cover miss/hit, keys, read-only, stray files, folder kit

## Tests

- `build`: green, 0 errors. The only warnings are the exempt `NU1902` advisories and, on a MathNet recompile, the
  pre-existing vendored `SYSLIB0051`.
- `constructor-unit-tests`: the gate's command over the whole suite (`all_discovered`) on the final tree. Passed!,
  1889 succeeded, 0 failed, 0 skipped, 116 s of a 180 s deadline. That is checkpoint 484's 1884 plus exactly the five
  census identities, whose TRX names match the census ids.
- The touched existing classes `PlannerInputsTests` and `PlannerReportTests`: 26 of 26.
- `elevation`: `ELEV-GATE: OK`, 0 new findings in 13 projects (Domain and Storage 0), within its 600 s deadline, run on
  the first green tree and again on the final tree.
- The 21 `invariant-s46-*` greps: each finds nothing, also with `--untracked` to cover the new files.
- No run exceeded its effective deadline. These are local development runs; the supervisor's gate engine remains the
  authority.

```yaml
gates:
  constructor_unit_tests: 1889
```

## Architecture

- **Key.** A candidate's key holds:
  - its own items, with every parameter state: the source, the plate and the polarimeter;
  - its branch (`Observable`);
  - the study's opened material, whose physics and parameter states are the nominal point and the targets' bounds;
  - the targets.

  Optics are in no key, because the candidates' Jacobians do not read them. So adding an item no candidate reads
  leaves every key, and its file, unchanged: adding one plate computes only its own candidates.
- **File.** The file name is the lower-case hex SHA-256 of the key's compact canonical JSON (`contentHashOf`, step 123)
  plus `.candidate-json`. The entry keeps its key, and a hit requires `entry.key = key`, so a stale, colliding or
  foreign file is a miss that is written over.
- **Store reuse.** The store reuses step 124's stack: `nodeOf`, `canonicalFileText` and `CanonicalJson.options`. The
  parse is the lock's pair (step 129): any failure, and a bare `null`, is `MalformedJsonErr`, never a throw. No schema
  covers the file, because `.oc/` is cache.
- **Errors.** Errors of the key's name, the read, the rendering and the write are `PlannerCacheErr` carrying their
  `describe`. A `compute` refusal passes through unwritten. Read-only access computes and writes nothing (.spec-md
  §3.12). There is no eviction, size limit, retry or option.

## Deferred

Nothing. Every requirement of the step landed in this round.

## Gotchas

- **Outside `effects.edits`, `PlannerInputsTests.fs` and `PlannerReportTests.fs`.**
  - Their exhaustive `PlannerError` matches gain `PlannerCacheErr _`; the `--warnaserror+:25` build requires it, as at
    steps 470, 471, 477 and 483.
  - `PlannerInputsTests` drops `private` from the twelve kit bindings the cache facts reuse. The step builds its keys
    "from `PlannerInputsTests`' kit", and AGENTS.md forbids a copy. Step 0486's `PlanRunTests` reads the same values.
- `UnresolvedPlannerItemErr.describe` says "the item's model is not in the library"; for a cache key's missing item
  the reason carries the true cause ("QZ-Z: the kit holds no such item").
- `git grep` skips untracked files: a must-find-nothing invariant checked before the commit needs `--untracked` to see
  a new file.

## Changelog

- 2026-11-18 — step 0485: Planner candidate cache: `PlannerCacheKey` / `PlannerCacheEntry` / `cacheKeyWith` and
  `PlannerCacheErr` in Domain; `PlannerCacheStore` (hashed `.oc/planner/<hash>.candidate-json`, canonical text, safe
  parse, `KitContext.cachedPlannerRows`) in Storage; five `PlannerCacheTests` facts (1884 → 1889).
