# Step 0196 — state of the world

# Where we are

Step 0196 (`IMPLEMENT`) opens section S6 (send and receive kits) with the send rule, as pure Domain data. It covers
three parts of 001:
- §H: Send kit… writes one `<kit name>.zip`, excluding `.oc/` and `sendExclude`;
- §E: large lab files may stay out through globs such as `**/*.dng`;
- §A: `.oc/` is the app's cache, never sent.

A kit path now knows its role, folder and leaf. A glob matcher decides what `sendExclude` leaves out, and a kit file
decides which tree entries travel. Later S6 steps write the zip and receive it over this rule.

# What's working

- Add `KitPathRole` and `KitRelativePath.appCacheFolder` / `pathRole` / `parentOpt` / `leafName`. `.oc` is written once,
  and the lock names it through `appCacheFolder`.
- Add `KitArchive`: the `SendExcludeGlob.matchOf` matcher (`**`, `*`, `?`, ignoring case) with no regex or package.
- Add `KitFile.sendChoiceOf` / `sentEntries`. The app's cache, every glob match and everything inside a matched folder
  are left out, in the tree's order.
- Add `KitName.zipFileName` (`Optics: QZ/LR` → `Optics_ QZ_LR.zip`).
- Add the `KitArchiveTests` class (four facts) and one `KitTests` fact.

# Tests

No gate verdict is claimed here (Invariant 6). After exit, the arc-runner runs the frozen roster:

- build and elevation;
- constructor-unit-tests (all discovered);
- the fourteen s22 invariants.

The worker's development runs over the final tree are in the impl-log's `Testing state`, with logs under
`specs/0050/.artifacts/0196-*`:

- red first: the test project failed to build on the undefined symbols;
- build: 0 warnings and 0 errors;
- constructor-unit-tests: 1220 passed, 0 failed, 0 skipped. That is 1215 + 5 against checkpoint 194, and the TRX
  lists all five declared identities as Passed;
- elevation from the arc origin: `ELEV-GATE: OK`, 0 new findings in 12 of 12 projects;
- the fourteen s22 invariant greps: none finds anything.

No test is removed or renamed, and no assertion is changed. The YAML carries the latest recorded checkpoint floors
(checkpoint 194), as earlier steps' did. The supervisor measures this step's own: here, 1220.

```yaml
gates:
  constructor_unit_tests: 1215
  ui_smoke_tests: 236
```

# Architecture

- **The send rule is pure Domain.** `KitArchive` (after `KitHistory.fs`) holds only types and members over `Kit`'s
  types. It touches no disk, and the zip writer will consume `sentEntries`.
- **One cache constant.** `KitRelativePath.appCacheFolder` replaces the former `kitCachePath`. `kitLockPath`, the
  in-memory folder and the filesystem store all name `.oc` through it.
- **One parent rule.** `KitRelativePath.parentOpt` is the single last-`/` split. `KitFolders`' private
  `parentFolders` and `sendChoiceOf`'s folder walk both recurse over it.
- **No bool.** The matcher's recursion answers `GlobPathMatch`, the send rule answers `KitSendChoice`, and
  `sentEntries` uses `List.choose`.

# Deferred

Nothing. Every requirement of the slice is in this round.

# Gotchas

- A FINAL `**` needs at least one segment (`data/raw/big/**` does not match `data/raw/big`). A leading or inner `**`
  takes zero or more (`**/*.dng` matches `IMG_0001.DNG`).
- A glob that names a folder leaves out the folder and everything inside it, through `parentOpt`. A glob ending in
  `/**` leaves out only what is inside.
- `pathRole` compares the first segment ignoring case, so `.OC/...` is the cache and `.ocx` is content.
- A trailing `/` in a glob leaves an empty segment that matches nothing. Globs are taken literally.
- `kitLockPath` keeps its name because tests read it. Only `kitCachePath` became `appCacheFolder`.

# Changelog

- 2026-11-09, attempt 01: the kit send rule. Add `KitPathRole` and the path members, `KitArchive` (glob matcher, send
  choice, tree entries, zip name), `appCacheFolder` replacing `kitCachePath`, and five facts.
