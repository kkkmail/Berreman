# Where we are

Step 0444 is S13.1's second step, after step 443's `MaterialSourceProxy` seam. It implements what a pull
writes (obligation S131-PULLS):

- a pulled record becomes a user material at version 1, under `SourcePullProvenance`;
- updating a source re-fetches every active pulled entry's record and mints the next version only where the
  record's hash changed.

The pulls write through the import seam's existing write path (`sealEntry` and `putEntryVersion`) and use
step 442's `pulledRecordsOf`. They live on `CatalogueImportContext`, which now carries `materialSources`.
Later steps build on it:

- step 447: the real refractiveindex.info adapter;
- step 449: composes the app's sources by record update;
- step 450: runs `updateSourceSnapshot` as the Update snapshot job.

# What's working

- Added `CatalogueImportContext.materialSources`. Storage's `create` sets it to `[]`, and its signature
  is unchanged.
- Added `MaterialSourcePulls.fs`, holding:
  - `MaterialSourcePullError`, `SourcePullChange` and `SourcePullResult`;
  - `SourceRefreshInfo` and `SourceRefresh`.
- `pullRecord` adds an unheld record as a user material at version 1 under its pull provenance. It
  refreshes a record already held, and refuses one held only by retired entries without writing.
- `refreshPulls` and `updateSourceSnapshot` re-fetch each active entry's newest pulled record:
  - a changed hash mints the next version, with the record's physics;
  - an unchanged or unlisted record writes nothing;
  - a cancelled update refreshes nothing;
  - a retired entry is never refreshed.
- Added `MaterialSourcePullTests`, with the census's eight facts.

# Tests

Development runs with each gate's own command and selection. The supervisor's gate engine re-runs the
roster and is the authority.

- **build:** exit 0, 25 s (incremental) against 600 s. The only warnings are 12 × NU1902, which is exempt.
- **elevation** (ELEV004 excluded, all 13 projects, no comparison base):
  `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)`, 263 s against 600 s. Domain, Storage and every
  downstream project have zero findings.
- **constructor-unit-tests** (`all_discovered`, the whole suite): `Passed!`, 1780 of 1780, 113 s against
  180 s.
  - The count rises by 8 from step 443's 1772, as the acceptance requires.
  - All eight `OpticalConstructor.Tests.MaterialSourcePullTests` identities passed. Run alone, the class
    also passes 8 of 8.
  - This run overlapped the elevation run.
- **kit-acceptance** over `OpticalConstructor.Ui.Tests.KitAcceptanceTests`: `Passed!`, 1410 s against
  3600 s.
  - 14 succeeded and 1 skipped: AT-2's five-seed ensemble, which xUnit v3 marks Explicit (the gate's skip
    policy is `allow`).
  - `the built optics-mueller kit equals the committed kit file by file` and
    `the built absorbing-biaxial-noisy kit equals the committed kit file by file` passed.
  - AT-1's and AT-2's expectations and tolerances are unchanged, and `data/` is untouched.
- **The twenty-one s43 invariants:** each decoded `git grep` finds nothing. A direct scan of the two
  new, still untracked files with the same patterns also finds nothing.
- No development run exceeded its gate's effective deadline.

```yaml
gates:
  constructor_unit_tests: 1780
  kit_acceptance_tests: 14
```

# Architecture

- **One write path.** Every version a pull or an update adds goes through the import's existing write:
  `sealEntry`, then `proxy.putEntryVersion` with `MoveLatestVersion`. Fork and Promote to library use
  the same path, so no library verb is added.
  - The pulled-source row is derived from the stored version's provenance (step 442). A pull therefore
    needs no extra write to be found again by `pulledRecordsOf`.
- **One refresh rule.** `pullRecord` (for a held record) and `refreshPulls` share private helpers:
  - `activePulls` takes each entry's newest pulled row, with its held entry, in
    `PulledRecord.inRecordOrder`, leaving out retired entries;
  - `refreshEach` and `refreshPull` decide absent, unchanged or minted.
- **Versions are never mutated (001 §C).** A minted version is the entry's latest decoded material
  with only its `physics` replaced: its id, name, category and description are kept. It is filed at
  the latest version's `.next`. Every held version stays as it was, a user's later edit included.
- **The sources ride on the import context.** That is the library-exchange seam every library window
  already carries, so no new context is threaded. The composition root sets the app's sources by
  record update (step 449), as it sets `exportFolderOpt`.

# Deferred

Nothing.

# Gotchas

- **A pulled row whose entry the library does not hold is refused** as `PullImportErr (ImportRejectedErr …)`.
  Neither store can produce one. The brief names this refusal for a missing latest envelope, and it is
  applied one read earlier so that a result always has a name.
- **`pullRecord` keeps only its record's rows before choosing each entry's newest row,** as the brief
  orders it. If some entries pulled from the record are active and others are retired, the active ones
  are refreshed and the retired ones left out. Only when every entry is retired is the pull refused.
- **`Lifecycle` is not opened.** The brief names four opens, so the code matches on
  `Lifecycle.ActiveEntry` and `Lifecycle.InactiveEntry`.
- **ELEV012 forbids mapping a two-case union to `bool`,** so the retired check is a match inside the
  fold, not a predicate.
- **The cancelled fact publishes two records,** because the double reads the cancellation before each
  record (step 443's gotcha).
- **In the in-memory store, `putEntryVersion` with `MoveLatestVersion` makes an entry active again.**
  The retired check therefore always runs before any write.
- **`CatalogueProxy` is declared in `Domain.CatalogueFiles`.** The test opens it beside
  `Storage.CatalogueFolderStore`, which adds `createBuiltIn`.

# Changelog

- 2026-11-16, step 0444:
  - `CatalogueImportContext.materialSources`;
  - `MaterialSourcePulls.fs`, holding `pullRecord`, `refreshPulls` and `updateSourceSnapshot` and their
    result and error types;
  - `MaterialSourcePullTests`, eight facts; constructor-unit-tests went from 1772 to 1780.
