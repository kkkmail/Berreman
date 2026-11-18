# Step 0484 — state of the world

## Where we are

Spec 0050 section S14.2 (the Planner's numerical layer, the plan, the cache and the Plan view). Step 0483 gave the
Planner its inputs and report; this step gives the kit's app cache the Planner's folder, `.oc/planner/`, and the two
`KitProxy` verbs that read and write files directly under it, in both stores. The next steps build the Planner's
candidate cache (`PlannerCacheStore`, keyed entries) on these two verbs.

## What's working

- Added `KitRelativePath.plannerCacheFolder` (`.oc/planner`, built on `appCacheFolder`) and the shared name check `plannerCacheFileOf`
- Added `KitProxy.readPlannerCacheFile` and `writePlannerCacheFile`, refusing any name that reaches outside `.oc/planner/`
- The read-only view refuses the cache write with `KitReadOnlyErr` and passes the read through
- The in-memory folder keeps cache files in its one map and lists `.oc` and `.oc/planner`; the folder store writes atomically as UTF-8 without a BOM and reads with a BOM dropped
- Added the two planner-cache facts to `KitProxyContract`, run by `KitProxyTests` and `KitProxyFolderTests` (+4)

## Tests

- `build`: green, 85 s, 0 errors; only the exempt NU190x advisories and the pre-existing vendored MathNet
  `SYSLIB0051` warnings.
- `constructor-unit-tests` (whole suite, `all_discovered`): Passed!, 1884 succeeded, 0 failed, 0 skipped, 117 s of a
  180 s deadline; checkpoint 483's 1880 plus exactly the 4 census identities, which a TRX of the new facts confirms
  under both classes.
- `elevation`: `ELEV-GATE: OK - 0 new finding(s) in 13 project(s), 233s`, within its 600 s deadline.
- The 21 `invariant-s46-*` greps: each finds nothing.
- No run exceeded its effective deadline. The supervisor's gate engine remains the authority; these are local
  development runs.

```yaml
gates:
  constructor_unit_tests: 1884
```

## Architecture

- The one rule for a planner cache name is `KitRelativePath.plannerCacheFileOf` (Domain, beside `tryCreate`), called
  by both stores: a blank name, `.`, `..`, `/` or `\` is `InvalidKitPathErr` and nothing is touched; the result is
  always a direct child of `.oc/planner`.
- Cache writes are not stamped: `changedKitFiles` covers root kit files only, so stamping a cache file would report it
  as removed. The folder store's encode + atomic write + `KitIoErr` mapping is now `writeTextBytes`, shared by
  `writeText` (which stamps) and the cache write (which does not); the kit-relative-to-absolute name is one local,
  `fileAt`.
- The in-memory placing body is one private `placeText`, shared by `placeLabFile` and the cache write.
- The shared lock guard (`.manual/009`) is unchanged; `lockFile` reads `fileAt` and names the same file.
- No delete, listing or eviction verb: the cache is safe to delete by hand.

## Deferred

Nothing. Every requirement of the step landed in this round.

## Gotchas

- The step's effects name `KitProxyFolderTests.fs`, which does not exist: the class `KitProxyFolderTests` is declared
  in `KitProxyTests.fs` and inherits its shared facts from the abstract `KitProxyContract`, as every earlier fact of
  that class did. The two facts are written once there; their identities are exactly the census's four.
- A planner cache file is not a kit file: `listKindFiles` and `changedKitFiles` never see it, and sending already
  leaves `.oc/` out (`KitFile.sendChoiceOf`, step 196).
- `KitRelativePath.tryCreate` also refuses a colon, so `a:b` is refused as a planner cache name too.

## Changelog

- 2026-11-18 — step 0484: `.oc/planner/` folder and the `readPlannerCacheFile` / `writePlannerCacheFile` proxy verbs
  in both stores, read-only view refusing the write; four facts across `KitProxyTests` and `KitProxyFolderTests`.
