# Step 0485 — impl log

## Progress

- Read the arc-runner system prompt, the IMPLEMENT delta, the project prompt and rulings R0001–R0005. None of the
  rulings reaches the planner cache; each is preserved untouched.
- `MeasurementPlanner.fs` (Domain):
  - `PlannerError` gains `PlannerCacheErr of reason : string`, described as
    `the planner's cache cannot be read or written: <reason>`.
  - After step 483's declarations, at the end of the module:
    - `PlannerCacheKey = { source; plate; detector : Item; observable : Observable; material : OpenedMaterial;
      targets : ParameterName list }`;
    - `PlannerCacheEntry = { key; rows : CandidateRows }`;
    - `PlannerInputs.cacheKeyWith items candidate`, which takes the items whose ids are the candidate's source, its
      plate and `polarimeterOpt`'s detector. An id `items` lacks is `UnresolvedPlannerItemErr "<label>: the kit holds
      no such item"` (step 483's wording for a missing optic). No polarimeter is `UnresolvedPlannerItemErr "no
      detector of the kit records a full Mueller matrix"`.
- New `PlannerCacheStore.fs` (Storage), `module PlannerCacheStore`, compiled directly after `KitContentFiles.fs` (one
  `<Compile>` line, no comment):
  - `PlannerCacheKey.cacheFileName` is `contentHashOf this` (the lower-case hex SHA-256 of the compact canonical JSON,
    `nodeOf` inside) followed by `.candidate-json`.
  - `PlannerCacheEntry.fileText` is `nodeOf` rendered by `canonicalFileText`.
  - `KitFileText.parsePlannerCacheEntry` binds through `CanonicalJson.options`. It drops the BOM and catches every
    failure as `MalformedJsonErr`, refusing a bare `null` as the lock's `parseLockHolder` refuses one.
  - `KitContext.cachedPlannerRows key compute` works in this order:
    1. the key's file name, then `proxy.readPlannerCacheFile`;
    2. a text binding to an entry whose `key = key` is a hit, answered without calling `compute`;
    3. no file, an unbound text or another key's entry is a miss, so it calls `compute ()`;
    4. under `ReadWriteKitAccess` the `Ok rows` are written as `{ key; rows }` through `writePlannerCacheFile`; under
       `ReadOnlyKitAccess` nothing is written.

    A naming, read, render or write refusal is `PlannerCacheErr` carrying that error's `describe`. A `compute` error
    passes through and nothing is written. There is no eviction, size limit, retry or option.
- New `PlannerCacheTests.fs`, `module PlannerCacheTests`, appended last in the test compile list: the five census facts,
  over keys built by `cacheKeyWith` from `PlannerInputsTests`' kit (Laser, Polarimeter, QZ-Z, the study's opened
  probe quartz, targets n_e and g33), literal rows and a `compute` that counts its calls.
- After the first green round, I rewrapped doc comments and one member signature to the tree's 120-column style
  (no semantic change), then rebuilt and re-ran everything below on the final tree.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/MeasurementPlanner.fs` (+58)
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/PlannerCacheStore.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/OpticalConstructor.Storage.fsproj` (+1)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/PlannerCacheTests.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj` (+1)
- Outside `effects.edits` (see Decisions):
  - `Berreman/OpticalConstructor/OpticalConstructor.Tests/PlannerInputsTests.fs` (13 ±);
  - `Berreman/OpticalConstructor/OpticalConstructor.Tests/PlannerReportTests.fs` (1 ±).

## Decisions

- **Consequential edit: `PlannerInputsTests.fs` and `PlannerReportTests.fs`.** Each matches `PlannerError`
  exhaustively (`unresolvedReasonOf`; `a target the crystal does not have refuses the report, naming it`). The test
  project compiles with `--warnaserror+:25`, so the new case would break the build. Each or-pattern gains
  `PlannerCacheErr _` on its existing line. No assertion or identity changed. This follows the precedent of steps 470,
  471, 477 and 483.
- **`PlannerInputsTests`' kit is reused, not copied.** The step builds its keys "from `PlannerInputsTests`' kit", but
  that kit was `private` to its module. AGENTS.md's "Reuse, never copy" rules out a second transcription of its
  drafts, sample, crystal and study, about 120 lines. So `private` is dropped from exactly the twelve bindings
  `PlannerCacheTests` reads: `itemValue`, `crystalVersion`, `choiceOf`, `laser`, `lp1`, `polarimeter`, `qzZ`,
  `inputsValue`, `nE`, `g33`, `crystalStudy` and `plannerInputsOf`. Nothing else in that file changed.
  `TabulatedStudyFixtures` and `CrystalMaterialFixtures` are the tree's precedent for test fixtures one test file
  shares with others. Step 486 (`PlanRunTests`, "over `PlannerInputsTests`' kit") reads the same values.
- **The declarations sit at the end of `MeasurementPlanner`**, after `Study.plannerInputsWith`. "After step 483's
  declarations" holds there too, and no earlier record construction in the module sees the new field labels
  (`source`, `plate`, `observable`, `material`, `targets`).
- **The file name is `contentHashOf this`.** `contentHashOf` is `nodeOf` then the SHA-256 of the compact canonical
  text, so "the text of `contentHashOf` of `nodeOf this`" is exactly this, with no second serialisation. It is built as
  `FileName $"{hash.value}.candidate-json"`, as `Kit.fs` names a snapshot.
- **A hit compares the bound key with `=`.** `Item` and `OpenedMaterial` already round-trip through
  `CanonicalJson.options` (item and study files), and doubles render in their shortest round-trip form. The bound key
  therefore equals the computing one, which the first and fifth facts check end to end (in memory and on disk).
- **The fact bodies include extra assertions within the five identities.** None is a new identity:
  - fact 1: the written text is the entry's canonical text; a `compute` refusal passes through unwritten; a refused
    write is `PlannerCacheErr` with the store's reason and the `describe` prefix;
  - fact 2: a missing item and a missing polarimeter refuse with `UnresolvedPlannerItemErr`;
  - fact 4: `{`, `null` and `{}` are `MalformedJsonErr`, and the entry's own text binds back to it.

## Testing state

- Build: `dotnet build Berreman.slnx -c Release -nologo -v:m` from `Berreman/`, the gate's command. Exit 0, 0 errors.
  The only warnings are the exempt `NU1902` advisories and, on a MathNet recompile, the four pre-existing vendored
  `SYSLIB0051` (0483 and 0484 recorded the same). The first build failed on a missing `open` in the new test file
  (`KitId` lives in `LibraryEntryFiles`), fixed. Logs: `.artifacts/0485-build-01.log` (red), `0485-build-02.log`,
  and `0485-build-03.log` (the final tree).
- `PlannerCacheTests` alone: 5 of 5, both before and after the rewrap. The TRX names are exactly the five census ids.
  Logs: `.artifacts/0485-planner-cache-tests-01.log`, `-02.log` (final), `.artifacts/0485-trx/0485-planner-cache.trx`.
- The edited classes `PlannerInputsTests` and `PlannerReportTests`: 26 of 26. Log:
  `.artifacts/0485-planner-inputs-report-tests.log`.
- `constructor-unit-tests`, the gate's command over the whole suite (selection `all_discovered`): Passed!, succeeded
  1889, failed 0, skipped 0: 115 s of 180 s on the first green tree (concurrent with elevation), 116 s on the final
  tree. Checkpoint 484 was 1884, so the count rises by exactly 5. Log (final): `.artifacts/0485-constructor-unit-tests.log`.
- The 21 `invariant-s46-*` must-find-nothing greps, decoded from `.slices/0485.gates`, find nothing, also with
  `--untracked` so the two new files are covered. Commands: `.artifacts/0485-invariant-commands.txt`.
- Elevation: `Elevation/Run-ElevationGate.ps1 -ExcludeRules ELEV004`, the gate's command, from the repo root; with no
  base it analyses all 13 projects.
  - First green tree: `ELEV-GATE: OK - 0 new finding(s) in 13 project(s), 249s`. Log: `.artifacts/0485-elevation-01.log`.
  - Final tree: `ELEV-GATE: OK - 0 new finding(s) in 13 project(s), 240s`, known findings 0, within its 600 s
    deadline. Domain 0 (207 s), Storage 0, Ui 0. Log: `.artifacts/0485-elevation.log`.
- No run exceeded its effective gate deadline.
- Line endings: every touched file is `w/lf`; `git diff --numstat` equals `--ignore-cr-at-eol`; the new files hold no
  CR byte.

## Artifacts

- `.artifacts/0485-build-01.log`, `0485-build-02.log`, `0485-build-03.log`
- `.artifacts/0485-planner-cache-tests-01.log`, `0485-planner-cache-tests-02.log`, `.artifacts/0485-trx/0485-planner-cache.trx`
- `.artifacts/0485-planner-inputs-report-tests.log`
- `.artifacts/0485-constructor-unit-tests.log`
- `.artifacts/0485-invariant-commands.txt`
- `.artifacts/0485-elevation-01.log`, `0485-elevation.log`

## Gotchas

- `UnresolvedPlannerItemErr.describe` reads "the item's model is not in the library: …". For a cache key's missing
  item that prefix is imprecise, but the step names this case for it, so the reason carries the accurate cause
  ("QZ-Z: the kit holds no such item").
- A read-write open already lists `.oc` and `.oc/lock`, so the first fact expects exactly `.oc/planner` and the one
  `<64 hex>.candidate-json` to be added to `paths`.
- `git grep` skips untracked files, so a must-find-nothing invariant run before the commit does not see a new file
  unless `--untracked` is added.
