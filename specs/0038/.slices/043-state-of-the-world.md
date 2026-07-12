# Step 043 — State of the world

## Where we are

Spec 0038 step 043 (`IMPLEMENT`, `touches: OpticalConstructor.Ui / .Storage /
.Tests`, `depends_on: 2`). This is the recent-files consolidation step in the UI-
standardization arc: two parallel recent-file stores existed —
`EnvironmentSettings.recentFiles` (a plain `string list` field persisted in
`environment.json`) and the bounded-MRU `Storage/RecentFiles.fs` module (its own
`recent.json`, the pure `bump` invariant, case-insensitive dedup, 16-cap). This step
collapses them to ONE by removing the `EnvironmentSettings.recentFiles` field so
`Storage.RecentFiles` is the single recent-files mechanism, while the environment
loader keeps loading old `environment.json` files that still carry the dropped field
(ignored, no migration).

## What's working

- Remove the `recentFiles : string list` field from `EnvironmentSettings` and its
  `recentFiles = []` entry in the built-in `defaults` (`Ui/UserEnvironment.fs`).
- Drop `recentFiles` from the env JSON schema (both `required` and `properties`);
  the root object stays permissive so old files still validate.
- Keep old `environment.json` files loading — the extra `recentFiles` property is
  admitted by the permissive schema and ignored on bind, with no migration code.
- Leave `Storage/RecentFiles.fs` as the sole recent-files store; no production
  consumer read the removed field, so nothing needed re-pointing.
- Update the round-trip tests and add two regression tests proving an old file
  carrying a legacy `recentFiles` array still loads to the real settings.

## Tests

Per Invariant 6 (the IMPLEMENT worker acts and runs NO checks), the `build`,
`unit-tests`, `constructor-unit-tests`, `ui-smoke`, and `ui-tests` gates are executed
by the arc-runner's deterministic gate engine AFTER this worker exits — they are not
run or self-reported here.

Expected effect on the `count_at_least` baselines:

- **constructor-unit-tests** — `+2` facts (both new legacy-load regressions land in
  `EnvironmentRoundTripTests.fs`, which the `constructor-unit-tests` gate covers). No
  fact was removed: the recent-files round-trip test was RENAMED to a last-folders
  test (still one fact), and the dropped `Assert.Empty defaults.recentFiles` line was
  an assertion inside an existing fact, not its own. `672 → 674`.
- **unit-tests / ui-smoke / ui-tests** — unchanged. The change lives entirely in
  `OpticalConstructor.Ui` (record + schema) and `OpticalConstructor.Tests`; no UI or
  solver code referenced the removed field, so those suites are behaviour-preserving.

```yaml
gates:
  berreman_unit_tests:    119
  constructor_unit_tests: 674
  ui_smoke_tests:         173
  ui_tests:               461
```

## Architecture

- **The env schema root stays PERMISSIVE — deliberately.** The root object carries no
  `additionalProperties: false`, so an OLD `environment.json` that still holds a
  `recentFiles` array continues to VALIDATE (the extra property is simply allowed).
  `System.Text.Json` under the shared `ProjectJson.options` uses the default `Skip`
  unmapped-member handling, so the field is ignored on bind. Together these preserve
  the loader's total `path -> EnvironmentSettings` contract with ZERO migration code
  (§J.6 item 3 — versioning/migration out of scope).
- **`Storage.RecentFiles` is now the single recent-files authority.** It already owned
  the real MRU semantics (pure `bump`, case-insensitive dedup, 16-cap, its own
  `recent.json`); the `EnvironmentSettings.recentFiles` field was a duplicate store
  with no MRU discipline. Removing the field eliminates the divergence risk of two
  parallel lists.

## Deferred

- **Wiring a File ▸ Recent menu onto `Storage.RecentFiles`** — the step-2 Elmish shell
  that would have consumed a recent-files list is retired; surfacing `loadRecent` /
  `pushRecent` in the surviving shell is a later UI step, out of this step's scope.

## Gotchas

- **Do NOT tighten the env schema root to `additionalProperties: false`.** That would
  make old files carrying `recentFiles` fail validation and force the loader into its
  defaults fall-back — silently discarding a returning user's whole environment. The
  no-migration/total-load contract depends on the root staying permissive.
- **No consumer re-point was needed.** The slice anticipated "any consumer surviving
  the step-2 shell retirement re-points to `Storage.RecentFiles`," but a grep shows the
  only references to `EnvironmentSettings.recentFiles` were the field declaration, its
  default, the schema, and the tests — the shell that read it is already gone. Nothing
  in production read the field, so there was nothing to re-point.
- **Stale schema copies under `bin/…/net10.0/` still show `recentFiles`.** Those are
  build outputs (the schema is copied to the output root); the `build` gate regenerates
  them from the updated source schema. They are not source and were left untouched.
- **System-prompt path drift.** The task file points at
  `C:\GitHub\AI-Strategy-Generator\implement_worker.system-md`, which does not exist;
  the real prompt lives under `.../src/ai_strategy_generator/multistep/`. Read from
  there. No scope impact.

## Changelog

- 2026-07-11 — Step 043 (IMPLEMENT): collapsed the two recent-files stores into one.
  Removed the `EnvironmentSettings.recentFiles` field and its default
  (`Ui/UserEnvironment.fs`) and dropped `recentFiles` from the env JSON schema
  (`required` + `properties`), leaving the schema root permissive so old
  `environment.json` files still load with the field ignored (no migration).
  `Storage/RecentFiles.fs` is now the sole recent-files store. Updated
  `EnvironmentRoundTripTests.fs`: removed the field's uses, renamed the recent-files
  round-trip test to a last-folders test, and added two regressions proving a legacy
  file carrying a `recentFiles` array still loads to the real settings.
